      program imsad
      implicit none  
c
c= IMSAD - image search and destroy
c& nebk
c: image analysis
c+
c       IMSAD fits a gaussian to the image histogram to determine the true
c       image rms noise, then searches for islands of pixels above some
c       cutoff, and attempts to fit gaussian components to the islands. The
c       fitting is borrowed from the miriad imfit task and the island
c       detection from the AIPS SAD task. 
c
c       Pixels above the clip level are marked in yellow.  Then a square
c	region centred on each island is coloured in. 
c
c       When fitting Gaussians, a cross is plotted at the midpoint of
c       the island.  Each island is displayed separately in the top
c	left of the display as it is fit.  The centroid of the fit is
c	plotted.
c
c	Information about the fit for each island is given.
c
c@ in
c
c	The name of the input image data set.
c@ region
c
c	The region of interest within the image (rectangular only).
c@ range
c	Image intensity range to display. Default is min to max for the
c	current plane being displayed.
c@ clip
c	Clip  level. For input images of intensity, any pixels below the
c	clip level are excluded from the fitting process. For other sorts
c	of images (e.g. Stokes Q, U or V) pixels whose absolute values are
c	below the clip level are excluded from the fit.
c
c       The clip level can be specified as a multiple of the true image 
c       rms (using the hist option) or as an absolute pixel value. No
c       default.
c@ box
c       The minimum extents for island boundaries. This would usually be
c       some multiple of the beam extents in x and y (2,2 is the default).
c       For images with no beam characteristics there is no default, the
c       units in this case are pixels.
c@ max
c	Sets the maximum number of boxes to return, if more then max
c	are detected then the max boxes with largest peak flux / pixel
c       are returned. 
c@ rad  
c       Will only report those islands detected within some angular radius
c       of the specified coordinate pair.  The default units are absolute
c       pixels, eg. 16,18,10.  If units other than the default are used
c       you must specify these after the radius, eg. 10:34:45.3,-45:54:02,
c       0.005,hms,dms,degrees. 
c@ options
c	Extra processing options. Possible values are:
c
c       hist ....... compute the image pixel histogram and compute the
c                    true image rms
c       gauss ...... fit a gaussian model to each component
c       fixed ...... the FWHM is fixed (circular cross-section)
c       point ...... a gaussian with the characteristics of the
c                    point-spread-function is fit
c       box ........ return the island box extents
c       arcsec ..... output box BLC/TRC units are arcsec offset from reference,
c                    default is absolute pixels
c       fiddle ..... interactively adjust the display lookup table (LUT),
c                    and alter the transfer function
c       nofit ...... do not perform any fitting
c       noplt ...... disable the plotting features
c	nodet ....... skip island detection and fit the box given by region 
c		     - not implemented
c@ device
c	PGPLOT plot device
c@ out
c	Dataset to write fitted source parameters.  Each line of this file
c	summarises the result of the Gaussian fit for one island.
c	Each line contains:
c
c	Island name, island number, ra and dec of island centroid, 
c	peak flux density, integrated flux density, deconvolved (from
c	beam) major axis FWHM (arcsec), deconvolved minor axis (arcsec),
c	deconvolved position angle (degrees), FLAG, DFLAG, FFLAG
c
c	FLAG:   IFAIL value (an integer) means Gaussian fit did not converge
c	        F means failed to find covariance matrix for Gaussian fit
c	        N means it was not possible to attempt a Gaussian fit
c	        C converged OK
c
c       DFLAG:  D  deconvolution of size from beam OK
c		P  deconvolution gave result close to point source
c		?  No synthesised beam so could not deconvolve
c	        F  deconvolution failed
c
c       FFLAG:  F  Integrated flux < peak flux (caused by beam > source size)
c               If blank then ok.
c@ log
c       If specified, output is written to the file given by log= instead
c       of to the terminal.
c@ label
c       Special purpose label 
c--
c
c   History:
c
c    13dec94   njt  created
c    07apr95   njt  fixed fit divergence problem by using relative coords
c    14apr95   njt  fixed stokes QUV sign problem in island pixel maxima check
c    14apr95   njt  fixed bug with histogram formation
c    01Oct95   njt  fixed ra + dra bug in report if dra < 0 and ra ~ 0
c    02Oct95   njt  added some more detail to the flagging in report and
c                   modified limits in gaupar.for to be more flexible wrt
c                   source/beam parmater errors, added label to fit records
c                   based on flux weighted centroid of component coords.
c    26jan96   nebk Lots of work to bring it in line with Miriad, F77 and nebk
c		    (who will be its keeper) coding standards.  Added keyword 
c		    "range". Added more documentation about output.  Redid 
c		    histogram stuff as there were a variety of bugs associated 
c		    with it. Elimiate use of non-standard angle conversion 
c		    subroutines.
c    17apr96   nebk/njt
c		    Fiddle with formatting in ISBOX and fix typo dimax->djmax 
c		    in write with format 300.
c    28jun96   nebk Fix some porblems with output flags (courtesy mark wieringa)
c		    fix a problem with fit locations being plotted in wrong place when
c		    a region specified.
c    16dec97   rjs  fix a problem with fit locations being plotted in wrong place when
c		    a region specified. What did NEBK do?

c-----------------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      include 'imsad.h'
c
      integer  MAXSRC, MAXBOX
      character*40   version
      parameter     (version='version 28-Jun-96')
      parameter (MAXSRC=2048, MAXBOX=1024)
c
      integer lui, luo, ip, ni, nj, iptr, mptr, ng, iwin(2,MAXSRC), 
     + jwin(2,MAXSRC), nsize(MAXNAX), boxes(MAXBOX), naxis, nbox,
     + blc(MAXNAX), trc(MAXNAX)
      double precision Diwin(2,MAXSRC), Djwin(2,MAXSRC)
      real clip,  box(2),  bmaj, bmin, bpa, bvol, bmajp, bminp, bpap, 
     + bvolp, ra0, de0, range(2), range2(2), hvp(4), ivp(4), ivp2(4)
      character*80 line
      character*64 logfile, outfile
      logical dohist, dolog, beam, dobox, doplot, inten,
     + arcsec, nofit, dofid, hisok
c
c Viewports for histogram, image, and subimage
c
      data hvp /0.1, 0.3, 0.2, 0.5/
      data ivp /0.4, 0.95, 0.1, 0.9/
      data ivp2 /0.1, 0.3, 0.6, 0.9/
c---------------------------------------------------------------------------
c
c Initialise program enviroment
c
      call output ('IMSAD : '//version)
      call init(lui,logfile,boxes,nsize,naxis,blc,trc,clip,dolog,dohist,
     + box,dobox,doplot,inten,arcsec,nbox,nofit,outfile,luo,dofid,range)
c
c Allocate memory for image and mask in blank common via mem.h
c
      ni = trc(1) - blc(1) + 1
      nj = trc(2) - blc(2) + 1
c
      call memalloc(iptr,ni*nj,'r') 
      call memalloc(mptr,ni*nj,'l') 
c
c Loop over image file planes
c
      do ip = blc(3), trc(3)
        write (line, 10) ip
10      format ('Begin plane', i4)
        call output (line)
        call output (' ')
c
c Compute beam parameters
c
        call imparm(lui,ip,bmajp,bminp,bpap,bvolp,bmaj,bmin,bpa,bvol,
     +              beam,ra0,de0,dolog)
        if(dopoint.and..not.beam) 
     +   call bug('f','No beam parameters for point fitting')
c
c Load image plane data
c
        call imload(lui,ip,blc,trc,memr(iptr),meml(mptr),range2,ng)
        if (ng.eq.0) then
          write (line, 20) ip, ' because no good pixels'
20        format ('Skipping plane ', i4, a)
          call output (line)
          goto 600
        end if
c
c Compute fitted image rms if required
c
        if (dohist) then
          call hist(hvp,blc,trc,memr(iptr),meml(mptr),clip,doplot,
     +              dolog,range2,hisok)
          if (.not.hisok) then
            write (line, 20) ip, ' because could not make histogram'
            call output (line)
            goto 600
          end if            
        end if
c
c Display image
c
        if(doplot) call imdisp(lui,memr(iptr),ni,nj,range,range2,
     +                         dofid,ivp)
c   
c Search for islands of pixels above clip with resolution checking 
c
        call island(memr(iptr),meml(mptr),blc,trc,clip,iwin,jwin,
     +              ns,doplot,inten)
c
        call ischeck(ns,iwin,jwin,blc,trc,bmajp,bminp,bpap,bvolp,
     +               beam,box,nbox,memr(iptr),meml(mptr),inten)
c
c Report island box boundaries (if required)
c
        if(dobox) call isbox (lui, ip, MAXSRC, iwin, jwin, Diwin, Djwin, 
     +                        arcsec, dolog, ns)
c
c Fit Gaussian components to islands
c
        if(.not.nofit) call isfit(lui,ip,memr(iptr),meml(mptr),blc,
     +    trc,iwin,jwin,bmajp,bminp,bpap,bvolp,bmaj,bmin,bpa,
     +    bvol,beam,ra0,de0,doplot,outfile,luo,ivp,ivp2)
600     continue
      end do
c
c Tidy up and close
c
      call cofin (lui)
      call pgend
      call xyclose(lui)
      call logclose
      if(outfile.ne.' ') call txtclose (luo)
c
      call memfree(iptr,ni*nj,'r')
      call memfree(mptr,ni*nj,'l')
c
      end
c
c
      subroutine coordfid(lu,ip)
c-----------------------------------------------------------------------
c     Convert coordinates between world and pixel coordinates, need to
c     fix error estimates, the imfit ones are suspect
c
c  Input:
c     lu ...... handle of the coordinate system.
c     ip ...... image plane index
c-----------------------------------------------------------------------
      implicit none
      include  'imsad.h'
      include  'mirconst.h'
c
      integer lu, ip
      double precision in(3), out(3)
      real ma, mi, pa
      integer   i
c-----------------------------------------------------------------------
c
c Loop over components to fit
c
      do i = 1, nc
c
c Convert the position FR: absolute pixel TO: offset world radians
c
        in(1) = dble(pf(2,i))
        in(2) = dble(pf(3,i)) 
        in(3) = dble(ip) 
        call coCvt(lu,'ap/ap/ap',in,'ow/ow/ap',out)
c       
        pf(2,i) = real(out(1))            
        pf(3,i) = real(out(2))            
c
c Convert gaussian parameters FR: absolute pixel TO: world radians
c
        call coGauCvt(lu,'ap/ap/ap',in,'p', pf(4,i),pf(5,i),pf(6,i),
     +   'w',ma,mi,pa)
c
c Convert units - the errors are screwed
c
        pf(4,i) = ma                     
        pf(5,i) = mi                     
        pf(6,i) = pa                     
      end do
c
      end
c
c
      subroutine estimate(bmaj,bmin,bpa)
c-----------------------------------------------------------------------
c     Generate an estimate of the model parameters, units are pixels,
c     radians and Jansky's
c-----------------------------------------------------------------------
      implicit none
      include 'imsad.h'
      include 'mirconst.h'
c
      integer i, ic 
      double precision P, XP, YP, XYP, XXP, YYP, SP, WS, dd, dx, dy, tmp
      real bmaj, bmin, bpa
c-----------------------------------------------------------------------
      nc = 1
      ic = nc 
      SP = 0.0d0
      P = 0.0d0
      XP = 0.0d0
      YP = 0.0d0
      XYP = 0.0d0
      XXP = 0.0d0
      YYP = 0.0d0
c
c Need to cope with multi-component sources
c
      do i = 1, nd
        dd = dble(data(i))
        dx = dble(xd(i))
        dy = dble(yd(i))
c
        SP  = SP + dd            
        tmp = abs(dd)         
        P   = P   + tmp                   
        XP  = XP  + tmp * dx          
        YP  = YP  + tmp * dy         
        XYP = XYP + tmp * dx * dy
        XXP = XXP + tmp * dx * dx
        YYP = YYP + tmp * dy * dy
      end do
      if(P.eq.0.0d0) call bug ('f', 'ESTIMATE: Error - zero flux')
      WS = 4.0d0 * log(2.0d0)  
      XP  = XP / P                      
      YP  = YP / P                   
      XYP = XYP / P - XP*YP  
      XXP = XXP / P - XP*XP
      YYP = YYP / P - YP*YP
c
      pf(2,ic) = real(XP)
      pf(3,ic) = real(YP)
c
      if(dogauss) then
        tmp = sqrt((XXP-YYP)**2 + 4.0d0*XYP**2)
        pf(4,ic) = real(sqrt(WS*(XXP + YYP + tmp)))
        pf(5,ic) = real(sqrt(WS*(XXP + YYP - tmp)))
c
        if(dofixed) then
          pf(4,ic) = sqrt(pf(4,ic)*pf(5,ic))
          pf(5,ic) = pf(4,ic)
          pf(6,ic) = 0.0
        else
          pf(6,ic) = 0.5 * atan2(2.0*XYP,YYP-XXP)
        end if
      else if(dopoint) then
        pf(4,ic) = bmaj
        pf(5,ic) = bmin
        pf(6,ic) = bpa
      end if
      if(pf(4,ic)*pf(5,ic).eq.0.0) call bug ('f', 'Error - zero width')
      pf(1,ic) = sign(WS*P/(PI*pf(4,ic)*pf(5,ic)),SP)

c      write(*,*) pf(1,ic), pf(2,ic), pf(3,ic)
c      write(*,*) pf(4,ic), pf(5,ic), pf(6,ic)

      end
c
c
      subroutine eval1(pf,nc,x,n,model)
c-----------------------------------------------------------------------
c     Evaluate the 1-D model Gaussian function
c
c  Input:
c     n ....... number of points
c     x ....... pixel coordinates at which to evaluate the model
c
c  Output:
c     model ... the evaluated model
c
c-----------------------------------------------------------------------
      implicit none
      integer MAXCOMP
      parameter (MAXCOMP=2)
      integer nc, n
      real pf(6,MAXCOMP), x(*), model(*)
      integer i, j
      real tmp, xx, fac
c-----------------------------------------------------------------------
c
c Set the model to zero initiallpy
c
      do i = 1, n
        model(i) = 0.0
      end do
c
c Compute model Gaussian component(s); can fit more than
c one Gaussian
c
      do j = 1, nc
        fac = 4.0 * log(2.0) / pf(3,j)**2
        do i = 1, n
          xx = (x(i) - pf(2,j))**2
          tmp = fac * xx
          if(tmp.lt.70.0) model(i) = model(i) + pf(1,j) * exp(-tmp)
        end do
      end do 
c
      end
c
c
      subroutine eval2(pf,nc,x,y,n,model)
c-----------------------------------------------------------------------
c     Evaluate the 2-D model Gaussian function
c
c  Input:
c     n ....... number of points
c     x, y .... pixel coordinates at which to evaluate the model
c
c  Output:
c     model ... the evaluated model
c
c-----------------------------------------------------------------------
      implicit none
      integer MAXCOMP
      parameter (MAXCOMP=2)
      integer nc, n
      real pf(6,MAXCOMP), x(*), y(*), model(*)
      integer i, j
      real cospa, sinpa, tmp, xx, yy, xp, yp, xscal, yscal
c-----------------------------------------------------------------------
c
c Set the model to zero initiallpy
c
      do i = 1, n
        model(i) = 0.0
      end do
c
c Compute model [2] Gaussian component(s)
c
      do j = 1, nc
        cospa = cos(pf(6,j))
        sinpa = sin(pf(6,j))
        xscal = 4.0 * log(2.0) / pf(5,j)**2
        yscal = 4.0 * log(2.0) / pf(4,j)**2
        do i = 1, n
          xx = x(i) - pf(2,j)
          yy = y(i) - pf(3,j)
c
c          if(i.eq.1) write(*,*) xx, yy
c
          yp =  yy*cospa + xx*sinpa
          xp = -yy*sinpa + xx*cospa
          tmp = xscal*(xp**2) + yscal*(yp**2)
          if(tmp.lt.70.0) model(i) = model(i) + pf(1,j) * exp(-tmp)
        end do
      end do 
c
      end
c
c
      subroutine FUNCTION(m,nvar,var,fvec,iflag)
c-----------------------------------------------------------------------
c     Used by least-squares fitting LMDIFF
c
c  Input:
c     m ..... number of functions      (number of data points)
c     n ..... number of variables (n <= m)
c     x ..... array of length n, on input contains an intial estimate
c             of the solution vector, on output contains the final
c             estimate of the solution vector
c
c  Output:
c     fvec .. output array of length m which contains the function(s)
c             evaluated at the output x
c     iflag . the value of iflag should not be changed by function unless 
c             the user wants to terminate execution of lmdiff, in this
c             case set iflag to a negative integer
c-----------------------------------------------------------------------
      implicit none
      include 'imsad.h'
      integer m, nvar, iflag
      real var(nvar), fvec(m)
      integer i
c-----------------------------------------------------------------------
      if(m.ne.nd) call bug('f','Inconsistency in FUNCTION')
c
c Unpack the things that we are solving for
c
      call upackvar(var,nvar)
c
c Evaluate the model.  EIther 1 or 2 dimensional
c Gaussians being fit.
c
      if (gdim.eq.1) then
        call eval1(pf,nc,xd,m,fvec)
      else if (gdim.eq.2) then
        call eval2(pf,nc,xd,yd,m,fvec)
      else
        call bug ('f', 'Unrecognized Gaussian dimensionality')
      end if
c
      do i = 1, m
        model(i) = fvec(i)          
        fvec(i) = data(i) - fvec(i) 
      end do
c
      end	
c
c 
      subroutine gaussfid(a1,b1,p1,a2,b2,p2)
c-----------------------------------------------------------------------
c     Fiddle gaussian parameters by converting from radians to arcsec
c     and degrees and fixing orientation
c
c Input:
c     a1	axis 1 (radians)
c     b1	axis 2 (radians)
c     p1	position angle (radians)
c
c Output:
c     a2	major axis (arcsec)
c     b2	minor axis (arcsec)
c     p2	position angle (degrees)
c
c------------------------------------------------------------------------
      implicit none
      include 'mirconst.h'
c
      real a1, b1, p1, a2, b2, p2, ma, mi, pa, tmp
c-----------------------------------------------------------------------
      ma = a1 * 3600*180/pi
      mi = b1 * 3600*180/pi
      pa = p1 * 180/pi
c
      if(ma.lt.mi) then
        tmp = ma
        ma = mi
        mi = tmp
        pa = pa + 90.0
      end if
c
      pa = mod(pa,180.0)
      if(pa.lt.-90.0) pa = pa + 180.0
      if(pa.gt.+90.0) pa = pa - 180.0 
c
      a2 = ma
      b2 = mi
      p2 = pa
c
      end
c
c
      subroutine hist(hvp,blc,trc,image,mask,clip,doplot,dolog,range,ok)
c-----------------------------------------------------------------------
c     Compute image plane histogram and moments
c
c  INput
c    range is the min and max of the current plane
c
c-----------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
      include 'maxnax.h'
      include 'imsad.h'
c
      integer MAXBOX, MAXVAR, MAXBIN
      parameter (MAXBOX=1024, MAXVAR=20, MAXBIN=50)
c
      integer i, j, n, ifail1, ifail2, nvar, ni, nj,
     +  blc(MAXNAX), trc(MAXNAX), id
      real clip, trms, mom(3), wmin, wmax, wid, hvp(4),
     +  rms, sum, squ, covar(MAXVAR*MAXVAR), x(MAXVAR), image(*), dx,
     +  bmax, norm, range(2), drange, xtemp(MAXBIN), dtemp(MAXBIN)
c
      character itoaf*2, line*80
      logical mask(*), doplot, dolog, ok
c
      external function
c-----------------------------------------------------------------------
      ns = 1
      nd = MAXBIN
      ok = .true.
c
c Initialise some things
c
      sum = 0.0
      squ = 0.0
      do i = 1, nd
        data(i) = 0
        xd(i) = 0.0
      end do
c
c Read image data and compute moments
c
      ni = trc(1) - blc(1) + 1 
      nj = trc(2) - blc(2) + 1 
c
      n = 0
      do i = 1, ni*nj
        if(mask(i)) then     
          n = n + 1
          sum = sum + image(i)
          squ = squ + image(i)**2
        end if
      end do
c 
      if(n.eq.0) then
        call bug('w', 'Image plane all blank')
        return
      end if
c
c Compute mean and standard deviation
c
      mom(1) = sum/n
      mom(2) = sqrt(squ/n - mom(1)**2)
c 
c Write to logfile (if required)
c
      write(line,100) mom(1)
  100 format('Image mean = ', 1pe10.3)
      call output (' ')
      if(dolog) then
        call logwrit(line)
      else
        call output (line)
      end if
c
      write(line,200) mom(2)
  200 format('Image rms = ',1pe10.3)
      if(dolog) then
        call logwrit(line)
      else
        call output (line)
      end if
c
      write(line,300) range(1), range(2)
  300 format('Image min = ', 1pe10.3, 1x, 'max = ',1pe10.3)
      if(dolog) then
        call logwrit(line)
      else
        call output (line)
      end if
c
c Set bin width and limits; try to exclude distant outliers
c
      drange = 2*min(abs(range(1)),abs(range(2)))
      if (drange.gt.0.0) then
        wmin = mom(1) - drange / 2.0
        wmax = mom(1) + drange / 2.0
      else
        wmin = range(1)
        wmax = range(2)
      end if
      wid = (wmax - wmin) / nd  
c
c Form histogram
c
      do i = 1, ni*nj
        if (mask(i) .and. image(i).ge.wmin.and.image(i).le.wmax) then 
          id = int((image(i) - wmin) / wid) + 1
          id = max(1,min(id,nd))
c
          data(id) = data(id) + 1.0
          xd(id) = xd(id) + image(i)
        end if
      end do
c
c Work out some scaling factors and set the abcissa bin values by the 
c average contributing to that bin
c
      norm = 0.0
      do i = 1, nd
        if(data(i).gt.0.0) then
          xd(i) = xd(i) / data(i) 
c
          norm = norm + data(i)
        end if
c
        xtemp(i) = xd(i)
        dtemp(i) = data(i)
      end do 
c 
c Remove empty bins from fit
c
      j = 1
      do i = 1, nd
        if (dtemp(i).gt.0.0) then
          data(j) = dtemp(i)
          xd(j) = xtemp(i)
          j = j + 1
        end if
      end do
      nd = j - 1
c
      if(nd.eq.0) then
        call bug('w','No data in histogram')  
        ok = .false.
        return
      end if
c      
c Normalise histogram volume to 1.0
c
      bmax = -1.0e30
      do i = 1, nd
        data(i) = data(i) / norm
        bmax = max(bmax,data(i))
      end do
c
c Plot histogram
c
      if(doplot) then
        dx = 0.05*(wmax-wmin)
        call pgsvp(hvp(1), hvp(2), hvp(3), hvp(4))
        call pgswin(wmin-dx,wmax+dx,0.0,1.05*bmax)
        call pgsch(0.8)
        call pgbox('BCNTS',0.0,0,'BCNTS',0.0,0)
        call pglabel('Image Intensity', 'Normalized counts',' ') 
        call pgpoint(nd,xd,data,21)
      end if 
c
c Set Gaussian fits parameter estimates.   
c 1: peak, 2: centre, 3: FWHM
c
      pf(1,1) = bmax
      pf(2,1) = mom(1)
      pf(3,1) = mom(2) * sqrt(8.0 * log(2.0))
c
      pf(4,1) = 0.0
      pf(5,1) = 0.0
      pf(6,1) = 0.0 
c 
      vf(1) = .true. 
      vf(2) = .true.  
      vf(3) = .true. 
      vf(4) = .false.
      vf(5) = .false. 
      vf(6) = .false.
c
c Fit histogram with a Gaussian
c
      nc = 1 
      xoff = 0.0
      yoff = 0.0
      gdim = 1
c
      call packvar(x,nvar,MAXVAR)     
      if(nvar.eq.0) call bug ('f', 'HIST: Internal logic error')
c
      call lsqfit(FUNCTION,nd,nvar,x,covar,rms,ifail1,ifail2)
c
      call upackvar(x,nvar)
      if(ifail2.eq.0) call upackcov(covar,nvar)
      if(ifail1.ne.0)
     +  call bug('w','HIST: Failed to converge: ifail='//itoaf(ifail1))
      if(ifail2.ne.ifail1)
     +  call bug('w','HIST: Failed to determine covariance matrix')
      if (ifail1.ne.0 .or. ifail2.ne.0) then
        ok = .false.
        return
      end if
c
c Compute fitted image rms
c
      trms = abs(pf(3,1)) / sqrt(8.0 * log(2.0)) 
      clip = clip * trms
c
c Write to logfile (if required)
c
      write(line,400) trms
  400 format('Fitted image rms = ',1pe9.3)
      if(dolog) then
        call logwrit(line)
      else
        call output (line)
      end if
c
      write(line,500) clip
  500 format('Computed clipping level = ',1pe9.3)
      if(dolog) then
        call logwrit(line)
      else
        call output (line)
      end if
      call output (' ')
c
c Plot fit
c
      if(doplot) then
        call pgline(nd,xd,model)
        write (line, 600) trms
600     format ('Fitted rms = ', 1pe10.4)
        call pglab (' ', ' ', line)
      end if
c
      end
c
c
      subroutine iadd(newisl,ns,inew,iwin,jwin,i,j)
c-----------------------------------------------------------------------
c     Add an island, ripped off from AIPS SAD
c-----------------------------------------------------------------------
      implicit none
      integer MAXSRC
      parameter (MAXSRC=2048)
c
      integer inew, i, j, iwin(2,MAXSRC), jwin(2,MAXSRC), ns
      logical newisl
c-----------------------------------------------------------------------
      if(newisl) then
        if(ns.le.MAXSRC) then
          ns = ns + 1
          inew = ns
          iwin(1,inew) = i
          iwin(2,inew) = i
          jwin(1,inew) = j
          jwin(2,inew) = j
        else
          call output ('Maximum number of islands reached')
          inew = 0 
        end if
      else
        if(inew.ne.0) then
          iwin(1,inew) = min(iwin(1,inew),i)
          iwin(2,inew) = max(iwin(2,inew),i)        
          jwin(2,inew) = j
        end if
      end if  
c
      end 
c
c
      subroutine imdisp(lui,image,ni,nj,range,range2,dofid,ivp)
c-----------------------------------------------------------------------
c     Display image on pgplot device
c
c  Input
c    range    User given display range
c    range2   Min and max for current plane
c-----------------------------------------------------------------------
      implicit none
      real tr(6), image(*), range(2), tfvp(4), range2(2), ivp(4)
      integer ni, nj, lui,i1,i2,len1
      logical dofid
      character ctype1*9, ctype2*9
c-----------------------------------------------------------------------
c 
c Initialise OFM routines
c
      call ofmini
c
c Setup viewport and window
c
      call pgsvp(ivp(1), ivp(2), ivp(3), ivp(4))
      call pgwnad(1.0,real(ni),1.0,real(nj))
      call pgsch(1.0)
c
c     setup world-coordinate transformation matrix
c
c     x = tr(1) + tr(2)*i + tr(3)*j
c     y = tr(4) + tr(5)*i + tr(6)*j 
c
      tr(1) = 0.0
      tr(2) = 1.0
      tr(3) = 0.0
      tr(4) = 0.0
      tr(5) = 0.0
      tr(6) = 1.0       
c
c Display image
c
      if (range(1).ne.range(2)) then
        range2(1) = range(1)
        range2(2) = range(2)
      end if
      call pgimag(image,ni,nj,1,ni,1,nj,range2(1),range2(2),tr)
c
c Simple label
c
      call pgbox ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
      call rdhda (lui, 'CTYPE1', ctype1, ' ')
      call rdhda (lui, 'CTYPE2', ctype2, ' ')
      i1 = index(ctype1,'--') 
      if (i1.eq.0) then 
        i1 = len1(ctype1)
      else
        i1 = i1 - 1
      end if
      i2 = index(ctype2,'--')
      if (i2.eq.0) then
        i2 = len1(ctype2)
      else
        i2 = i2  -1
      end if
      call pglabel (ctype1(1:i1)//' (pixels)', 
     +              ctype2(1:i2)//' (pixels)', ' ')
c
c Modify lookup table
c
      tfvp(1) = 0.1
      tfvp(2) = 0.6
      tfvp(3) = 0.3
      tfvp(4) = 0.9
c
      if(dofid) call ofmmod(tfvp,1,0.0,0,0.0,0.0)
c
      end
c
c
      subroutine imerge(inew,iold,LHS,RHS,new,old,iwin,jwin)
c-----------------------------------------------------------------------
c     Island merge, ripped off from AIPS SAD and modified
c-----------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
      integer MAXSRC
      parameter (MAXSRC=2048) 
      integer new(MAXDIM), old(MAXDIM), inew, iold, LHS, RHS,
     +  i, il, ih, iwin(2,MAXSRC), jwin(2,MAXSRC)
c-----------------------------------------------------------------------
      il = min(inew,iold) 
      ih = max(inew,iold) 
c
      iwin(1,il) = min(iwin(1,il),iwin(1,ih))
      iwin(2,il) = max(iwin(2,il),iwin(2,ih))
      jwin(1,il) = min(jwin(1,il),jwin(1,ih))
      jwin(2,il) = max(jwin(2,il),jwin(2,ih))
c
      iwin(1,ih) = 0
      iwin(2,ih) = 0
      jwin(1,ih) = 0
      jwin(2,ih) = 0
c
      do i = LHS, RHS
        if(old(i).eq.ih) old(i) = il
        if(new(i).eq.ih) new(i) = il
      end do
c
      end
c
c
      subroutine imload(lui,ip,blc,trc,image,mask,range2,ng)
c-----------------------------------------------------------------------
c      Load image region and mask (if any) into memory
c
c-----------------------------------------------------------------------
      implicit none
      include 'maxdim.h' 
c
      logical hdprsnt
      integer blc(3), trc(3), ip, lui, i, j, ptr, ng, nb
      real image(*), irow(MAXDIM), range2(2)
      logical mask(*), mrow(MAXDIM)
      character line*80
c-----------------------------------------------------------------------
c
c Set image plane
c
      call xysetpl(lui,1,ip)
c
c Initialise 
c
      if(hdprsnt(lui,'mask')) 
     +  call output ('Pixel blanking will be applied')
      nb = 0
      ng = 0 
      ptr = 1
      range2(1) =  1.0e30
      range2(2) = -1.0e30
c
c Read image and mask (if required)
c
      do j = blc(2), trc(2)
        call xyflgrd(lui,j,mrow)
        call xyread(lui,j,irow)            
c
        do i = blc(1), trc(1)
           image(ptr) = irow(i)
           mask(ptr) = mrow(i)
           if(mask(ptr)) then
             ng = ng + 1
             range2(1) = min(range2(1),irow(i))
             range2(2) = max(range2(2),irow(i))
           else
             nb = nb + 1
           end if
c
          ptr = ptr + 1
        end do
      end do
c
      write (line, 100) nb
100   format ('Number of blanked pixels = ', i7)
      call output (line)
      write (line, 200) ng
200   format ('Number of good pixels = ', i7)
      call output (line)
c
      end
c
c
      subroutine imparm(lu,ip,bmajp,bminp,bpap,bvolp,bmaj,bmin,bpa,bvol,
     + beam,ra0,de0,dolog)
c-----------------------------------------------------------------------
c     Get things dealing with units
c
c  Input:
c     lu ............ handle of the input dataset
c     ip............. plane of interest
c
c  Output:
c     bvol .......... beam volume, in radians**2. Set to zero if this
c                     cannot be determined.
c     bvolp ......... beam volume in pixels.
c     bmaj,bmin,bpa . beam major, minor axes and position angle.
c
c     bpa is measured from North through East in radians, where North is
c     the direct of increasing value along 2nd axis and East the direct-
c     in of increasing value along the first axis
c
c-------------------------------------------------------------------------
      implicit  none
      include  'mirconst.h'
c
      integer   lu, ip
      real bvol, bmaj, ma, bmin, mi, bpa, pa, ra0, de0
      double precision tmp
      character bunit*16, ctype(2)*16, line*80
      real bmajp, bminp, bpap, bvolp
      double precision crpix(2), crval(2), cdelt(2), x(3)
c
      logical beam, dolog
c-----------------------------------------------------------------------
      beam = .true.
c
c Get spatial reference value
c 
      call rdhdd(lu,'crval1',tmp,0.0d0)
      ra0 = tmp
      call rdhdd(lu,'crval2',tmp,0.0d0)
      de0 = tmp
c
c Get beam parameters from image header
c
      call rdhdr(lu,'bmaj',bmaj,0.0)
      call rdhdr(lu,'bmin',bmin,0.0)
      call rdhdr(lu,'bpa',bpa,0.0)
      call rdhda(lu,'bunit',bunit,' ')
      call ucase(bunit)
c
      bpa = PI / 180.0 * bpa 
      if(bmaj*bmin.eq.0.0) beam = .false.
c
c Convert the beam to radians for the current plane
c
      if(beam) then   
c
c Convert from world to pixel 
c
        x(1) = 0.0d0
        x(2) = 0.0d0
        x(3) = 0.0d0
        call coGauCvt(lu,'op/op/op',x,'w',bmaj,bmin,bpa,
     +	 'p',bmajp,bminp,bpap)
c
c Convert from pixel to world
c
        x(3) = dble(ip)
        call coGauCvt(lu,'op/op/ap',x,'p',bmajp,bminp,bpap,
     +	 'w',bmaj,bmin,bpa)
c
c Determine the beam FWHM area in radians**2
c
        if(index(bunit,'/PIXEL').ne.0)then
          x(1) = 0.0d0
          x(2) = 0.0d0
          x(3) = dble(ip)
          call coLin(lu,'op/op/ap',x,2,ctype,crpix,crval,cdelt)
          bvol = real(abs(cdelt(1)*cdelt(2)))
          bvolp = 1.0
        else if(index(bunit,'/BEAM').ne.0)then
          bvol = pi/4/log(2.0)*bmaj*bmin
          bvolp = pi/4/log(2.0)*bmajp*bminp
        else
          bvol = 0.0
          bvolp = 0.0
        end if
      else
        call bug ('w', 'Image synthesised beam parameters are zero')
        bvol = 0.0
        bvolp = 0.0
      end if 
c 
      call gaussfid(bmaj,bmin,bpa,ma,mi,pa)
c
c Write out beam parameters
c
      write (line, 100) ma, mi, pa
100   format ('BEAM major/minor/pa = ', 2(1x,f7.3), ' arcsec',
     +        1x, f6.1, ' degrees')
      if(dolog) then
        call logwrit(line)
      else
        call output (line)
      end if
c
      write (line, 400) bvol, bvolp
400   format ('BEAM area = ', 1pe9.3, ' str', 1x, 1pe9.3, ' pixels**2')
      if(dolog) then
        call logwrit(line)
      else
        call output (line)      
      end if
c
      write (line, 500) bunit
500   format ('Image units = ', a)   
      if(dolog) then
        call logwrit(line)
      else
        call output (line)
      end if
c
      end
c
c
      SUBROUTINE indexx(n,arr,indx)
      INTEGER n,indx(n),M,NSTACK
      REAL arr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      REAL a
      do 11 j=1,n
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do 12 i=j-1,1,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=0
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK)
     +    call bug ('f', 'NSTACK too small in indexx')
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
c
c
      subroutine init(lui,logfile,boxes,nsize,naxis,blc,trc,clip,dolog,
     + dohist,box,dobox,doplot,inten,arcsec,nbox,nofit,outfile,luo,
     + dofid,range)
c-----------------------------------------------------------------------
c      Get user inputs
c-----------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'maxnax.h'
      include 'imsad.h' 
c
      integer pgbeg
c
      integer MAXBOX, MAXSRC, NOPTS
      parameter (MAXBOX=1024, MAXSRC=2048, NOPTS=10)
c
      integer lui, nsize(MAXNAX), boxes(MAXBOX), naxis, blc(MAXNAX),
     + trc(MAXNAX), iax, nbox, luo, iostat
      double precision dpol
      real clip, box(2), range(2)
      character*64 logfile, file, outfile
      character*80 device
c
      logical dolog, dohist, polspara, inten, doplot, dobox,
     +  nofit, arcsec, dofid, dodet
      character*6 opts(NOPTS)
      logical present(NOPTS)
c
      data opts / 'hist  ', 'gauss ', 'fixed ', 'point ', 'box   ',
     +            'noplt ', 'arcsec', 'nofit ', 'fiddle', 'region' /
c-----------------------------------------------------------------------
      call keyini
      call keya('in',file,' ')
      if(file.eq.' ') call bug('f','Input file must be given')
      call boxinput('region',file,boxes,MAXBOX)
c
      call keya('log',logfile,' ')
      dolog = logfile.ne.' '
      if(dolog) call logopen(logfile,' ')
c
      call keya('out',outfile,' ')
      if(outfile.ne.' ') then
        call txtopen (luo, outfile, 'new', iostat)
        if (iostat.ne.0) 
     +    call bug ('f', 'Could not opend output text file')
      end if
c
      call keyr ('range', range(1), 0.0)
      call keyr ('range', range(2), 0.0)
      call keyr('clip',clip,0.0)
c
      call keyi('max',nbox,MAXSRC)
      call keyr('box',box(1),2.0) 
      call keyr('box',box(2),2.0) 
c
      call options('options',opts,present,nopts)
      dohist = present(1)
      dogauss = present(2) .or. .not.present(4)
      dofixed = present(3)
      dopoint = present(4)
      dobox = present(5)
      doplot = .not.present(6)
      arcsec = present(7)
      nofit = present(8)
      dofid = present(9)
      dodet = .not.present(10)
c
      if (clip.le.0.0 .and. .not.dohist) call bug ('f',
     +  'You must specify the clip level or use the histogram option')
c
c Initialize plotting
c
      call keya('device',device,' ')
      doplot = device.ne.' '.and.doplot
      if(doplot) then
        if(pgbeg(0,device,0,0).ne.1) then
          call pgldev
          call bug('f','Error opening plot device')
        end if
        call pgask(.false.)
      end if
      call keyfin
c
c Open image file
c
      call xyopen(lui,file,'old',MAXNAX,nsize)
      call rdhdi(lui,'naxis',naxis,0)
      if(naxis.eq.0) call bug('f','Zero dimensions in image')
      naxis = min(naxis,MAXNAX)
      if(nsize(1).gt.MAXDIM) call bug('f','Input file to big')
c
c Setup the region of interest
c
      call boxmask(lui,boxes,MAXBOX)
      call boxset(boxes,MAXNAX,nsize,' ')
      call boxinfo(boxes,MAXNAX,blc,trc)
c
c IMFIT: determine image type: total intensity I or Q, U or V
c
      call coInit(lui)
      call coFindAx(lui,'stokes',iax)
      if(iax.ne.0) then
        call coCvt1(lui,iax,'ap',1.d0,'aw',dpol)
        inten = PolsPara(nint(dpol))
      else
        inten = .true.
      end if
c
      end
c
c
      subroutine initvary
c-----------------------------------------------------------------------
c     Determine parameter varriability
c
c     vf(1) flux
c     vf(2) l-coordinate
c     vf(3) m-coordinate
c     vf(4) semi-major axis
c     vf(5) semi-minor axis
c     vf(6) position angle
c-----------------------------------------------------------------------
      implicit none
      include 'imsad.h'
c-----------------------------------------------------------------------
      vf(1) = .true.       
      vf(2) = .true.       
      vf(3) = .true.       
c
      if(dopoint) then
        vf(4) = .false.  
        vf(5) = .false.
        vf(6) = .false.
      else if(dogauss) then
        vf(4) = .true.
        if(dofixed) then
          vf(5) = .false.
          vf(6) = .false.
        else
          vf(5) = .true.   
          vf(6) = .true.
        end if
      end if
c
      end 
c
c
      subroutine isbox (lui, ip, MAXSRC, iwin, jwin, diwin, djwin, 
     +                  arcsec, dolog, ns)
c-----------------------------------------------------------------------
      implicit none
      integer lui, MAXSRC, ns, ip, iwin(2,MAXSRC), jwin(2,MAXSRC)
      logical arcsec, dolog
cc
      include 'mirconst.h'
      double precision RADSEC
      parameter (RADSEC=180.0d0/DPI*3600.0d0)
c
      double precision in(3), out(3), Diwin(2,MAXSRC), Djwin(2,MAXSRC),
     + Dimax, Djmax, imin, imax, jmin, jmax
      integer i, j
      character line*132
c-----------------------------------------------------------------------
      Dimax = -9.0e9
      Djmax = -9.0e9 
c
      imin = 9.0e9
      imax = -imin
      jmin = 9.0e9
      jmax = -jmin
c
      do i = 1, ns
        if(arcsec) then
          do j = 1, 2
            in(1) = dble(iwin(j,i)) 
            in(2) = dble(jwin(j,i))
            in(3) = dble(ip) 
            call coCvt(lui,'ap/ap/ap',in,'ow/ow/ap',out)
            Diwin(j,i) = out(1) * RADSEC
            Djwin(j,i) = out(2) * RADSEC
c
            if(abs(Diwin(j,i)).gt.Dimax) Dimax = abs(Diwin(j,i))
            if(abs(Djwin(j,i)).gt.Djmax) Djmax = abs(Djwin(j,i))
c
            if(Diwin(j,i).lt.imin) imin = Diwin(j,i)
            if(Djwin(j,i).lt.jmin) jmin = Djwin(j,i)
c
            if(Diwin(j,i).gt.imax) imax = Diwin(j,i)
            if(Djwin(j,i).gt.jmax) jmax = Djwin(j,i)
          end do
          write(line,100) Diwin(1,i), Djwin(1,i),
     +                    Diwin(2,i), Djwin(2,i)
100       format('arcsec,box(',sp,1pe10.3,',',sp,1pe10.3,',',
     +           sp,1pe10.3,',',sp,1pe10.3,')')
        else
          write(line,200) iwin(1,i), jwin(1,i),
     +                    iwin(2,i), jwin(2,i)
200       format('abspix,box(',i4,',',i4,',',i4,',',i4,')')
        end if
c
        if(dolog) then
          call logwrit(line)
        else
          call output (line)
        end if
      end do
c
      if(arcsec.and.ns.ge.1) then
        write(line,300) Dimax+4.0, Djmax+4.0
300     format ('maxima',2(1x,f8.3))  
        if(dolog) then
          call logwrit(line)
        else
         call output (line)
        end if
        write(line,400) imin-4.0, imax+4.0, jmin-4.0, jmax+4.0
400     format('extent',4(1x,f8.3))
        if(dolog) then
          call logwrit(line)
        else
          call output (line)
        end if	    
      end if 
c
      end
c
c
      subroutine ischeck(ns,iwin,jwin,blc,trc,bmaj,bmin,bpa,bvol,beam,
     +                   box,nbox,image,mask,inten)
c-----------------------------------------------------------------------
c     Check islands for valid extents and minimum pixels, could add an
c     island merger if necessary
c
c     bvol ... future use
c
c-----------------------------------------------------------------------
      implicit none
      include 'maxnax.h'
      include 'mirconst.h'
c
      integer MAXSRC
      parameter (MAXSRC=2048)
c 
      integer iwin(2,MAXSRC), jwin(2,MAXSRC), itmp(2,MAXSRC), 
     +  jtmp(2,MAXSRC), blc(MAXNAX), trc(MAXNAX), ns, imin, jmin,
     +  ni, nj, i, j, di, dj, i0, j0, ptr, mi(MAXSRC), nbox, is
      real bmaj, bmin, bpa, pa, bvol, tmpx, tmpy, dxy, dx, dy,
     +  box(2), maxpix(MAXSRC), image(*)
      character line*80
      logical beam, mask(*), inten
c-----------------------------------------------------------------------
c
c Compute beam extents as box(1) * dx or box(1) if zero beam parms 
c
      if(beam) then
        pa = PI - bpa 
c
c Code fragment taken from AIPS SAD task - units are pixels/radians
c
        tmpx = (sin(pa)/bmaj)**2 + (cos(pa)/bmin)**2
        tmpy = (cos(pa)/bmaj)**2 + (sin(pa)/bmin)**2
        dxy = ((1.0/bmaj)**2 - (1.0/bmin)**2)*(sin(pa)*cos(pa))**2
        dx = sqrt(0.25/(tmpx - dxy**2/tmpy))
        dy = sqrt(0.25/(tmpy - dxy**2/tmpx))
c
        imin = int(box(1) * dx)
        jmin = int(box(2) * dy)
      else
        imin = int(box(1))
        jmin = int(box(2))
      end if              
c
c Check minimum island extents based on beam parameters
c
      do i = 1, ns
        ni = iwin(2,i) - iwin(1,i) + 1 
        nj = jwin(2,i) - jwin(1,i) + 1
c
        di = int((imin - ni) / 2.0 + 0.5)
        dj = int((jmin - nj) / 2.0 + 0.5)
c
        if(ni.lt.imin) then
          iwin(1,i) = max(blc(1),iwin(1,i) - di)
          iwin(2,i) = min(trc(1),iwin(2,i) + di)          
        end if
        if(nj.lt.jmin) then
          jwin(1,i) = max(blc(2),jwin(1,i) - dj)
          jwin(2,i) = min(trc(2),jwin(2,i) + dj)
        end if
      end do
c
c Check box number limit
c
      if(ns.gt.nbox) then
        ni = trc(1) - blc(1) + 1
c
c Determine box pixel maxima - fixed sign problem for stokes QUV
c 
        do is = 1, ns
          maxpix(i) = -9.0e9
          do j = jwin(1,is), jwin(2,is)
            j0 = j - blc(2) + 1   
            do i = iwin(1,is), iwin(2,is)
              i0 = i - blc(1) + 1
              ptr = (j0-1)*ni + i0
              if(mask(ptr)) then
                if(inten) then
		  if(image(ptr).gt.maxpix(is)) 
     +               maxpix(is) = image(ptr)
                else
	          if(abs(image(ptr)).gt.maxpix(is)) 
     +               maxpix(is) = abs(image(ptr)) 
		end if
              end if
            end do
          end do
        end do
c
c Index maxima in ascending order
c
        call indexx(ns,maxpix,mi)
c
c Re-shuffle box arrays
c
        do is = 1, nbox
          i0 = ns - is + 1
          write (line,100) maxpix(mi(i0))
100       format ('Keeping box ', 1x, 1pe10.3)
          call output (line)
c
          itmp(1,is) = iwin(1,mi(i0))
          itmp(2,is) = iwin(2,mi(i0)) 
          jtmp(1,is) = jwin(1,mi(i0))
          jtmp(2,is) = jwin(2,mi(i0))
        end do
        ns = nbox
        do is = 1, ns
          iwin(1,is) = itmp(1,is)
          iwin(2,is) = itmp(2,is)
          jwin(1,is) = jtmp(1,is)
          jwin(2,is) = jtmp(2,is)  
        end do
      end if
c
c Check minimum island pixel numbers based on beam area - necessary ???
c
c
      end
c
c
      subroutine isfit (lui,ip,image,mask,blc,trc,iwin,jwin,
     + bmajp,bminp,bpap,bvolp,bmaj,bmin,bpa,bvol,beam,ra0,de0,
     + doplot,outfile,luo,ivp,ivp2)
c-----------------------------------------------------------------------
c     Fit Gaussian components to detected pixel islands
c-----------------------------------------------------------------------
      implicit none
      include 'maxnax.h'
      include 'imsad.h'
      external function
c
      integer MAXSRC, MAXVAR
      parameter (MAXSRC=2048, MAXVAR=20)
c
      character*2 itoaf
      integer ifail1, ifail2, nvar, iwin(2,MAXSRC), jwin(2,MAXSRC),
     +  ptr, i0, j0, ni, nj, i, j, ip, ic, lui, blc(MAXNAX), 
     +  trc(MAXNAX), is, in, jn, ii, jj, luo
c
      real bmaj, bmin, bpa, bvol, bmajp, bminp, bpap, bvolp,
     +  rms, covar(MAXVAR*MAXVAR), x(MAXVAR), image(*), tr(6),
     +  island(MAXPIX,MAXPIX), dmin, dmax, px, py, ra0, de0,
     +  xt, yt, ivp(4), ivp2(4)
      logical dofit, done, mask(*), beam, doplot
      character*2 flag(MAXCOMP), ctmp
      character*64 outfile
c
      data tr / 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /
c-----------------------------------------------------------------------
      ni = trc(1) - blc(1) + 1
      nj = trc(2) - blc(2) + 1
c
c Setup fitting paramters and units
c
      call initvary
c
c Loop through islands
c
      do is = 1, ns
        dmin = +9.0e9
        dmax = -9.0e9
c
        jn = jwin(2,is) - jwin(1,is) + 1
        in = iwin(2,is) - iwin(1,is) + 1
c
c Plot mid-point of box we are fitting
c
	if(doplot) then
          call pgsvp(ivp(1), ivp(2), ivp(3), ivp(4))
          call pgwnad(1.0,real(ni),1.0,real(nj))
          call pgpoint(1,(iwin(1,is)+iwin(2,is))/2.0-blc(1)+1.0,
     +     (jwin(1,is)+jwin(2,is))/2.0-blc(2)+1.0,2)
        end if
c
c Setup data arrays
c 
        nd = 0
        xt = 0.0
        yt = 0.0
        do j = jwin(1,is), jwin(2,is)
          j0 = j - blc(2) + 1  
          jj = j - jwin(1,is) + 1 
          do i = iwin(1,is), iwin(2,is)
            i0 = i - blc(1) + 1
            ptr = (j0-1)*ni + i0
            ii = i - iwin(1,is) + 1
c
            if(mask(ptr)) then
              if(image(ptr).lt.dmin) dmin = image(ptr)
              if(image(ptr).gt.dmax) dmax = image(ptr) 
              island(ii,jj) = image(ptr)
c
              nd = nd + 1
              data(nd) = image(ptr)
              xd(nd) = real(i)
              yd(nd) = real(j)
c
              xt = xt + xd(nd)
              yt = yt + yd(nd)
c
c	      write(*,'(i2,2(x,f4.0),x,e10.3)') nd, xd(nd),
c     -         yd(nd), data(nd)
c
            else
              island(ii,jj) = 0.0   
            end if
          end do
        end do
c
        xoff = xt / real(nd)
        yoff = yt / real(nd)
c
c Check data for multiple peaks (if required)
c
c
c     plot island
c
        if(doplot) then
          call pgsvp(ivp2(1), ivp2(2), ivp2(3), ivp2(4))
          call pgswin(0.5,real(in)+0.5,0.5,real(jn)+0.5)
          call pgimag(island,MAXPIX,MAXPIX,1,in,1,jn,dmin,dmax,tr)
        end if
c
c Begin fitting data             
c
        nc = 0
        done = .false. 
        do while(.not.done)
          ctmp = '  '
c
c Determine estimates and prepare for fitting
c
          call estimate(bminp,bmajp,bpap) 
          call packvar(x,nvar,MAXVAR)
          dofit = nvar.ne.0 .and. .not.(nvar.ge.nd) .and. nd.gt.0
c
c Begin fitting
c
          if(dofit) then
            gdim = 2
            call lsqfit(FUNCTION,nd,nvar,x,covar,rms,ifail1,ifail2)
            call upackvar(x,nvar)
c
            if(ifail1.ne.0) then
              ctmp(1:1) = itoaf(ifail1) 
              call bug('w','Gaussian fit failed to converge, ifail='//
     +                 ctmp(1:1))
            else
              ctmp(1:1) = 'C'
            end if
c
            if(ifail2.ne.ifail1) then
              call bug('w','Failed to determine covariance matrix'//
     +                 ' during Gaussian fit')
              ctmp(1:1) = 'F'              
            else if(ifail2.eq.0) then
              call upackcov(covar,nvar)
            end if
          else
            call bug('w','Gaussian fit not possible')
            ctmp(1:1) = 'N'
          end if
c
c Inspect residual for multi-component islands and refit (if required)
c Refitting currently not implemented.
c
          done = .true.
          flag(nc) = ctmp(1:2)
        end do
c        
c Plot position of fitted centroid
c 
        if(doplot) then
          call pgsci(10)
          do ic = 1, nc
            px = pf(2,ic) - real(iwin(1,is)) + 1
            py = pf(3,ic) - real(jwin(1,is)) + 1
            call pgpoint(1,px,py,2)
          end do
        end if
c
c Convert fitted parameters to astronomical units and report fit
c
        call report(flag,ra0,de0,bmaj,bmin,bpa,bvol,beam,outfile,
     +              luo,lui,ip)
      end do 
c
      end
c
c
      subroutine island(image,mask,blc,trc,clip,iwin,jwin,ns,
     +                  doplot,inten)
c-----------------------------------------------------------------------
c     Detect islands of pixels above the clipping level 
c-----------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
      include 'maxnax.h'
c
      integer MAXSRC
      parameter (MAXSRC=2048)
      real image(*), clip, px, py
      logical mask(*), doplot
      integer iwin(2,MAXSRC), jwin(2,MAXSRC),
     +  blc(MAXNAX), trc(MAXNAX), LHS, RHS, new(MAXDIM), old(MAXDIM),
     +  ptr, i, j, ns, i0, j0, ni, pi, pj, ci
      character line*80
      logical merge, null, inten
c-----------------------------------------------------------------------
c
c Set yellow colour to mark pixels above clip level
c
      if(doplot) call pgsci(7)
c
c Initialse parameters and arrays
c
      ns = 0
      do i = 1, MAXDIM
        old(i) = 0
        new(i) = 0
      end do
c
      LHS = blc(1)
      RHS = trc(1)
      ni = RHS - LHS + 1
c
c Begin search for islands
c
      do j = blc(2), trc(2)
        j0 = j - blc(2) + 1 
        do i = LHS, RHS
          i0 = i - blc(1) + 1
          ptr = (j0-1)*ni + i0  
c
c Includes all stokes parameters I > clip "OR" |QUV| > clip
c
          if((inten .and. image(ptr).lt.clip) .or.
     +       (.not.inten .and. abs(image(ptr)).lt.clip) .or.
     +       .not.mask(ptr) ) then
            new(i) = 0
          else 
c
c Put a yellow point on pixel above clip level
c
            px = real(i - blc(1) + 1)
            py = real(j - blc(2) + 1)
            if(doplot) call pgpoint(1,px,py,-1)
c
            if(i.gt.LHS .and. new(i-1).ne.0) then
              new(i) = new(i-1)
              call iadd(.false.,ns,new(i),iwin,jwin,i,j)
              merge = i.lt.RHS .and. old(i+1).ne.0 .and.
     +                old(i+1).ne.new(i)
              if(merge) 
     +         call imerge(new(i),old(i+1),LHS,RHS,new,old,iwin,jwin)
            else if(i.gt.LHS .and. old(i-1).ne.0) then
              new(i) = old(i-1)
              call iadd(.false.,ns,new(i),iwin,jwin,i,j)
c
              merge = i.lt.RHS .and. old(i+1).ne.0 .and.
     +                old(i+1).ne.new(i)
              if(merge) 
     +         call imerge(new(i),old(i+1),LHS,RHS,new,old,iwin,jwin)
            else if(old(i).ne.0) then
              new(i) = old(i)
              call iadd(.false.,ns,new(i),iwin,jwin,i,j) 
            else if(i.lt.RHS .and. old(i+1).ne.0) then
              new(i) = old(i+1)
              call iadd(.false.,ns,new(i),iwin,jwin,i,j)
            else
              call iadd(.true.,ns,new(i),iwin,jwin,i,j)
            end if
          end if 
        end do
c           
        do i = LHS, RHS
          old(i) = new(i)
        end do 
      end do
c
c Remove null islands from collection
c
      i = 1
      do while(i.le.ns)
        null = iwin(1,i).eq.0 .and. iwin(2,i).eq.0 .and. jwin(1,i).eq.0
     +         .and. jwin(2,i).eq.0
        if(null) then
          do j = i, ns - 1
            iwin(1,j) = iwin(1,j+1)
            iwin(2,j) = iwin(2,j+1)
            jwin(1,j) = jwin(1,j+1)
            jwin(2,j) = jwin(2,j+1)
          end do
          ns = ns - 1
        else
          i = i + 1
        end if         
      end do
c       
      write (line,100) ns
100   format ('Number of islands found = ', i4)
      call output (line)
c
c Plot bounded islands in some colour other than yellow
c
      if(doplot) then 
        ci = 5
        do i = 1, ns
          ci = ci + 1
          if(ci.eq.7) ci = 5
          call pgsci(ci)
          do pi = iwin(1,i), iwin(2,i)
            px = real(pi - blc(1) + 1)
            do pj = jwin(1,i), jwin(2,i)
              py = real(pj - blc(2) + 1)   
              call pgpoint(1,px,py,1)
            end do
          end do
        end do
      end if
c
      end
c
c
      subroutine packvar(var,nvar,MAXVAR)
c-----------------------------------------------------------------------
c     Store all the things that we need to vary
c-----------------------------------------------------------------------
      implicit none
      include 'imsad.h'
c
      integer nvar,MAXVAR
      real var(MAXVAR), tmp(6)
      integer i, j, ncurr
c-----------------------------------------------------------------------
      nvar = 0
c
c Loop over number of components
c
      do i = 1, nc
        ncurr = 0
        if(vf(1)) then
          ncurr = ncurr + 1
          tmp(ncurr) = pf(1,i)
        end if
        if(vf(2)) then
          ncurr = ncurr + 1
          tmp(ncurr) = pf(2,i) - xoff
        end if
        if(vf(3)) then
          ncurr = ncurr + 1
          tmp(ncurr) = pf(3,i) - yoff
        end if
        if(vf(4)) then
          ncurr = ncurr + 1
          tmp(ncurr) = pf(4,i)
        end if
        if(vf(5)) then
          ncurr = ncurr + 1
          tmp(ncurr) = pf(5,i)
        end if
        if(vf(6)) then
          ncurr = ncurr + 1
          tmp(ncurr) = pf(6,i)
        end if
c
c Copy the estimates to the variables
c
        if(nvar+ncurr.gt.MAXVAR) 
     +    call bug('f','Too many free parameters')
c
        do j = 1, ncurr
          nvar = nvar + 1
          var(nvar) = tmp(j)
        end do
      end do
c
      end
c
c
      subroutine report(flag,ra0,de0,bmaj,bmin,bpa,bvol,beam,
     +                  outfile,luo,lui,ip)
c-----------------------------------------------------------------------
c     Generate fit report for current island.
c-----------------------------------------------------------------------
      implicit none
      include 'imsad.h'
      include 'mirconst.h' 
c
      double precision RADDEG, DEGRAD
      parameter (RADDEG = 180.0d0 / DPI, DEGRAD = DPI / 180.0d0)
c
      integer i, rc, fac, luo, lui, ip, len1, iostat
      real ra0, de0, ra, de, dra, dde, bmaj, bmin, bpa, smaj, smin,
     + spa, dmaj, dmin, dpa, bvol, iflux, wgt, sumwgt, wra,
     + sumwra, wde, sumwde
c
      character*1 dflag, fflag
      character*2 flag(MAXCOMP)
      character*11 ras, des
      character*64 outfile
      character line(MAXCOMP)*90, line2*130
      character*9 label
      character*40 rangle, hangle, ctmp
      logical      beam
c-----------------------------------------------------------------------
      sumwgt = 0.0
      sumwra = 0.0
      sumwde = 0.0
c
c Convert the fitted parameters to meaningful units
c     
      call coordfid(lui,ip)
c
c Write beam characteristics
c
      call gaussfid(bmaj,bmin,bpa,smaj,smin,spa)
c
      call output (' ')
      write(line2,1000) smaj, smin, spa
 1000 format('BEAM major/minor/pa ', 2(1x,f7.3), ' arcsec', 
     +       1x, f6.1, ' degrees')
      call output (line2)
c
c Generate record label based on flux weighted mean component coordinates
c Here we loop over the number of times the fits was attempted.
c Currently, nc is always 1 (see subroutine isfit)
c
      do i = 1, nc
c
c Compute integrated flux (if possible)
c
	write(line2,4000) pf(1,i)
4000    format('Peak flux (Jy)', 1x, 1pe10.3)
        call output (line2)
c
        if(bvol.gt.0.0) then
          iflux = pf(1,i) * pi/4 * pf(4,i) * pf(5,i) 
          iflux = iflux / log(2.0)
          iflux = iflux / bvol
        else 
          iflux = 0.0
        end if 
c
c Flag integrated flux < peak flux (caused by beam > source size)
c
        fflag = ' '
        if(iflux.lt.pf(1,i)) fflag = 'F'
        write(line2,5000) iflux
 5000   format('Integrated flux (Jy)', 1x, 1pe10.3)
        call output (line2)
c
c Convert major, minor and position angle 
c
        call gaussfid(pf(4,i),pf(5,i),pf(6,i),smaj,smin,spa)
        write(line2,6000) smaj, smin, spa
 6000   format('Fitted major/minor/pa ',2(1x,f6.2), ' arcsec',
     +         2x, f6.1, ' degrees')
        call output (line2)
c
c Deconvolve the beam (if possible) and setup source min/maj/pa
c TODO - add as an option
c
        if(beam) then 
          call gaudfac(pf(4,i),pf(5,i),pf(6,i)*180/pi,bmaj,bmin,
     +                 bpa*180/pi,fac,dmaj,dmin,dpa,rc)
          if(rc.eq.0) then
c
c Deconvolution ok
c
            dpa = pi/180 * dpa
            call gaussfid(dmaj,dmin,dpa,smaj,smin,spa)
            dflag = 'D'
          else if (rc.eq.1) then
c
c Result is close to a point source
c
            dflag = 'P'
	  else if (rc.eq.2) then
c
c Deconvolution failed
c
	    dflag = 'F'
          end if
        else
          dflag = '?'
        end if 
c
c Add centroid offsets to reference RA and Dec
c In radians
c
        dra = pf(2,i) / cos(de0)
        dde = pf(3,i)
c
        write(line2,3000) pf(2,i)*3600*RADDEG, pf(3,i)*3600*RADDEG
 3000   format('Fitted offsets', 2(1x,1pe10.3), ' arcsec')
        call output (line2)
c
        ra = ra0 + dra
        de = de0 + dde
c
c Sum weighted coordinates
c
        wgt = pf(1,i)
        sumwgt = sumwgt + wgt 
        sumwra = sumwra + wgt * ra
        sumwde = sumwde + wgt * de
c
c Convert RA and Dec to formatted strings
c
	if(ra.lt.0.0) ra = 2.0d0*DPI + ra
        ras = hangle(dble(ra))
        if(ras(3:3).ne.':') then
          ctmp = ras
          ras = '0'//ctmp
        end if
        des = rangle(dble(de))
        if (des(1:1).ne.'-') then
          ctmp = des
          des = '+'//ctmp
        end if
        if (des(4:4).ne.':') then
          ctmp = des
          des(2:) = '0'//ctmp(2:)
        end if
c
c Write to display/logfile
c
        write(line(i),100) ras, des, pf(1,i),
     +    iflux, smaj, smin, spa, flag(i), dflag, fflag
  100   format(2(1x,a11),5(1x,1pe10.3),1x,a1,1x,a1,1x,a1)
      end do  
c
c Determine source label
c
      wra = sumwra / sumwgt
      wde = sumwde / sumwgt
c
      if(wra.lt.0.0) wra = 2.0d0*DPI + wra
      ras = hangle(dble(wra))
      if(ras(3:3).ne.':') then
        ctmp = ras
        ras = '0'//ctmp
      end if
      des = rangle(dble(wde))
      if (des(1:1).ne.'-') then
        ctmp = des
        des = '+'//ctmp
      end if
      if (des(4:4).ne.':') then
        ctmp = des
        des(2:) = '0'//ctmp(2:)
      end if
c
      label = ras(1:2)//ras(4:5)//des(1:3)//des(5:6)
c
c Write report records
c 
      do i = 1, nc
        if(outfile.ne.' ') then
          line2 = label//' : '//line(i)
          call txtwrite (luo, line2, len1(line2), iostat)
          if (iostat.ne.0) 
     +      call bug ('f','Error writing to output file')
        else
          call output (label//' : '//line(i))
        end if
      end do
c
      end 
c
c
      subroutine upackcov(covar,nvar)
c-----------------------------------------------------------------------
c     Unpack the covariance matrix
c-----------------------------------------------------------------------
      implicit none
      include 'imsad.h'
      integer nvar
      real covar(nvar,nvar)
      integer i, n
c-----------------------------------------------------------------------
      n = 0
      do i = 1, nc
        if(vf(1)) then
          n = n + 1
          sf(1,i) = covar(n,n)
        end if
        if(vf(2)) then
          n = n + 1
          sf(2,i) = covar(n,n)
        end if
        if(vf(3)) then
          n = n + 1
          sf(3,i) = covar(n,n)
        end if
        if(vf(4)) then
          n = n + 1
          sf(4,i) = covar(n,n)
          if(dofixed) sf(5,i) = sf(4,i)
        end if
        if(vf(5)) then
          n = n + 1
          sf(5,i) = covar(n,n)
        end if
        if(vf(6)) then
          n = n + 1
          sf(6,i) = covar(n,n)
        end if
      end do
      if(n.ne.nvar) call bug('f','Inconsistency in UPackCov')
c
      end
c
c
      subroutine upackvar(var,nvar)
c-----------------------------------------------------------------------
c     Unpack all the things that we need to vary
c-----------------------------------------------------------------------
      implicit none
      include 'imsad.h'
      integer nvar
      real var(nvar)
      integer i, n
c-----------------------------------------------------------------------
      n = 0
      do i = 1, nc
        if(vf(1)) then
          n = n + 1
          pf(1,i) = var(n)
        end if
        if(vf(2)) then
          n = n + 1
          pf(2,i) = var(n) + xoff
        end if
        if(vf(3)) then
          n = n + 1
          pf(3,i) = var(n) + yoff
        end if
        if(vf(4)) then
          n = n + 1
          pf(4,i) = var(n)
          if(dofixed) pf(5,i) = pf(4,i)
        end if
        if(vf(5)) then
          n = n + 1
          pf(5,i) = var(n)
        end if
        if(vf(6)) then
          n = n + 1
          pf(6,i) = var(n)
        end if
      end do
      if(n.ne.nvar) call bug('f','Inconsistency in UPackVar')
c
      end
