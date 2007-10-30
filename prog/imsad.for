      program imsad
      implicit none  
c
c= IMSAD - image search and destroy
c& njt
c: image analysis
c+
c       IMSAD fits a gaussian to the image histogram to determiine the true
c       image rms noise, then searches for islands of pixels above some
c       cutoff, and attempts to fit gaussian components to the islands. The
c       fitting is borrowed from the miriad imfit task and the island
c       detection from the AIPS SAD task. 
c
c	IMFIT is a Miriad task which fits model components to a
c	image data-set. If several image planes are given, each plane
c	is fitted separately. Optionally the model or the residuals can be
c	written out.
c
c@ in
c
c	The name of the input image data set.
c
c@ region
c
c	The region of interest within the image (rectangular only).
c
c@ clip
c
c	Clip level. For input images of intensity, any pixels below the
c	clip level are excluded from the fitting process. For other sorts
c	of images (e.g. Stokes Q, U or V) pixels whose absolute values are
c	below the clip level are excluded from the fit.
c
c       The clip level can be specified as a multiple of the true image 
c       rms (using the hist option) or as an absolute pixel value. No
c       default.
c
c@ box
c
c       The minimum extents for island boundaries. This would usually be
c       some multiple of the beam extents in x and y (2,2 is the default).
c       For images with no beam characteristics there is no default, the
c       units in this case are pixels.
c
c@ max
c
c	Sets the maximum number of boxes to return, if more then max
c	are detected then the max boxes with largest peak flux / pixel
c       are returned. 
c
c@ rad  
c
c       Will only report those islands detected within some angular radius
c       of the specified coordinate pair.  The default units are absolute
c       pixels, eg. 16,18,10.  If units other than the default are used
c       you must specify these after the radius, eg. 10:34:45.3,-45:54:02,
c       0.005,hms,dms,degrees. 
c
c@ options
c
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
c 
c@ device
c
c	Plot device (usually /xs or /xw).
c
c@ out
c
c	Dataset to write fitted source parameters.
c
c@ log
c
c       If specified, output is written to the file given by log= instead
c       of to the terminal.
c
c@ label
c
c       Special purpose label 
c
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
c    26jan96   nebk Hacked about to be more in line with MIriad code standards
c
 
      include       'mirconst.h'
      include       'maxdim.h'
      include       'maxnax.h'
      include       'mem.h'
      include       'imsad.h'

      integer        MAXSRC, MAXBOX
      parameter     (MAXSRC=2048, MAXBOX=1024)

      integer        lui, i, j,
     -               ip,
     -               iwin(2,MAXSRC), jwin(2,MAXSRC),
     -               ni, nj,
     -               iptr, mptr

      integer        nsize(MAXNAX),
     -               boxes(MAXBOX),
     -               naxis,
     -               blc(MAXNAX),
     -               trc(MAXNAX),
     -               nbox,
     -               luo
     
      real           clip,
     -               box(2),
     -               bmaj, bmin, bpa, bvol,
     -               bmajp, bminp, bpap, bvolp,
     -               ra0, de0

      double precision in(3), out(3),
     -               Diwin(2,MAXSRC), Djwin(2,MAXSRC),
     -               RADSEC,
     -               Dimax, Djmax,
     -		     imin, imax,
     -		     jmin, jmax

      character*80   line
      character*64   logfile,
     -               outfile

      logical        dohist,
     -               dolog,
     -               domask,
     -               beam,
     -               dobox,
     -               doplot,
     -               inten,
     -               arcsec,
     -               nofit,
     -               dofid

      RADSEC = (180.0 / PI) * 3600.0

c     initialise program enviroment

      call init(lui,logfile,boxes,nsize,naxis,blc,trc,clip,dolog,dohist,
     - box,dobox,doplot,inten,arcsec,nbox,nofit,outfile,luo,dofid)

c      write(*,'(''> NAXIS ='',x,i1)') naxis
c      write(*,'(''> NSIZE ='',7(x,i4.4))') (nsize(i),i=1,naxis)
c      write(*,'(''> BLC ='',3(x,i4.4))') blc(1), blc(2), blc(3)
c      write(*,'(''> TRC ='',3(x,i4.4))') trc(1), trc(2), trc(3)

c     allocate memory for image and mask in blank common via mem.h

      ni = trc(1) - blc(1) + 1
      nj = trc(2) - blc(2) + 1

      call memalloc(iptr,ni*nj,'r') 
      call memalloc(mptr,ni*nj,'l') 

c     loop over image file planes

      do ip = blc(3), trc(3)

c     compute beam parameters

        call imparm(lui,ip,bmajp,bminp,bpap,bvolp,bmaj,bmin,bpa,bvol,
     -   beam,ra0,de0,dolog)
        if(dopoint.and..not.beam) 
     -   call bug('f','no beam parameters for point fitting')

c     load image plane data

        call imload(lui,ip,blc,trc,memr(iptr),meml(mptr),domask)

c     compute true image rms (if required)

        if(dohist) call hist(lui,blc,trc,memr(iptr),meml(mptr),
     -   domask,clip,doplot,dolog)

c     display image

        if(doplot) 
     -   call imdisp(memr(iptr),ni,nj,blc,trc,-clip,9*clip,dofid)
   
c     search for islands of pixels above clip with resolution checking 

        call island(memr(iptr),meml(mptr),domask,blc,trc,clip,
     -   iwin,jwin,ns,doplot,inten)

        call ischeck(ns,iwin,jwin,blc,trc,bmajp,bminp,bpap,bvolp,
     -   beam,box,nbox,memr(iptr),meml(mptr),domask,inten)

c     report island box boundaries (if required)

        if(dobox) then

          Dimax = -9.0e9
          Djmax = -9.0e9 

	  imin = 9.0e9
	  imax = -imin
	  jmin = 9.0e9
	  jmax = -jmin

          do i = 1, ns
            
            if(arcsec) then

              do j = 1, 2

                in(1) = dble(iwin(j,i)) 
                in(2) = dble(jwin(j,i))
                in(3) = dble(ip) 
                call coCvt(lui,'ap/ap/ap',in,'ow/ow/ap',out)
                Diwin(j,i) = out(1) * RADSEC
                Djwin(j,i) = out(2) * RADSEC

                if(abs(Diwin(j,i)).gt.Dimax) Dimax = abs(Diwin(j,i))
                if(abs(Djwin(j,i)).gt.Djmax) Djmax = abs(Djwin(j,i))

                if(Diwin(j,i).lt.imin) imin = Diwin(j,i)
                if(Djwin(j,i).lt.jmin) jmin = Djwin(j,i)

                if(Diwin(j,i).gt.imax) imax = Diwin(j,i)
                if(Djwin(j,i).gt.jmax) jmax = Djwin(j,i)

              end do
              write(line,100) Diwin(1,i), Djwin(1,i),
     -         Diwin(2,i), Djwin(2,i)
            else
              write(line,200) iwin(1,i), jwin(1,i),
     -         iwin(2,i), jwin(2,i)
            end if

            if(dolog) then
              call logwrit(line)
            else
              write(*,'(a)') line
            end if

          end do

          if(arcsec.and.ns.ge.1) then
            write(line,300) Dimax, Djmax
            if(dolog) then
              call logwrit(line)
            else
              write(*,'(a)') line
            end if
            write(line,400) imin-4.0, imax+4.0, jmin-4.0, jmax+4.0
            if(dolog) then
              call logwrit(line)
            else
              write(*,'(a)') line
            end if	    
          end if 

        end if

c     fit Gaussian components to islands

        if(.not.nofit) then

          call isfit(lui,ip,memr(iptr),meml(mptr),domask,blc,trc,iwin,
     -     jwin,bmajp,bminp,bpap,bvolp,bmaj,bmin,bpa,bvol,beam,ra0,de0,
     -     doplot,outfile,luo)

        end if

      end do

c     tidy up and close

      call pgend
      call xyclose(lui)
      call logclose
      if(outfile.ne.' ') close(luo)

      call memfree(iptr,ni*nj,'r')
      call memfree(mptr,ni*nj,'l')


  100 format('arcsec,box(',sp,e10.3,',',sp,e10.3,',',sp,e10.3,
     - ',',sp,e10.3,')')
  200 format('box(',i4.4,',',i4.4,',',i4.4,',',i4.4,')')

  300 format('maxima',2(x,f8.3))  

  400 format('extent',4(x,f8.3))

      end

c -----------------------------------------------------------------------
c +++ INIT
c
c     program initialisation 
c
c ---

      subroutine init(lui,logfile,boxes,nsize,naxis,blc,trc,clip,dolog,
     - dohist,box,dobox,doplot,inten,arcsec,nbox,nofit,outfile,luo,
     - dofid)
      implicit none

      include       'maxdim.h'
      include       'maxnax.h'
      include       'imsad.h' 

      integer        pgbeg

      integer        MAXBOX, MAXSRC
      parameter     (MAXBOX=1024, MAXSRC=2048)

      character*40   version
      parameter     (version='version 1.0 13-Feb-95')

      integer        lui,
     -               nsize(MAXNAX),
     -               boxes(MAXBOX),
     -               naxis,
     -               blc(MAXNAX),
     -               trc(MAXNAX),
     -               iax,
     -               nbox,
     -               luo

      double precision dpol

      real           clip,
     -               box(2)

      character*64   logfile,
     -               file,
     -               outfile
      character*80   device

      integer        NOPTS
      parameter     (NOPTS=10)

      logical        dolog,
     -               dohist,
     -               polspara,
     -               inten,
     -               doplot,
     -               dobox,
     -               nofit,
     -               arcsec,
     -               dofid,
     -               dodet

      character*6    opts(NOPTS)
      character*2    pol,
     -               PolsC2P

      logical        present(NOPTS)

      data opts / 'hist  ', 'gauss ', 'fixed ', 'point ', 'box   ',
     - 'noplt ', 'arcsec', 'nofit ', 'fiddle', 'region' /

      call output('IMSAD: '//version)

c     setup user inputs

      call keyini

      call keya('in',file,' ')
      if(file.eq.' ') call bug('f','Input file must be given')

      call boxinput('region',file,boxes,MAXBOX)

c     open log file and out file

      call keya('log',logfile,' ')
      dolog = logfile.ne.' '
      if(dolog) call logopen(logfile,' ')

      call keya('out',outfile,' ')
      if(outfile.ne.' ') then
        luo = 10
        open(luo,access='sequential',file=outfile,status='new') 
      end if

c     parameters borrowed from IMFIT

      call keyr('clip',clip,0.0)

c     island boxing parameters
   
      call keyi('max',nbox,MAXSRC)

      call keyr('box',box(1),2.0) 
      call keyr('box',box(2),2.0) 

c     determine processing options - add a check

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

c     initailise plotting

      call keya('device',device,' ')
      doplot = device.ne.' '.and.doplot
      if(doplot) then
        call pgask(.false.)
        if(pgbeg(0,device,0,0).ne.1) then
          call pgldev
          call bug('f','Error opening plot device')
        end if
      end if

      call keyfin

c     open image file

      call xyopen(lui,file,'old',MAXNAX,nsize)
      call rdhdi(lui,'naxis',naxis,0)
      if(naxis.eq.0) call bug('f','Zero dimensions in image')
      naxis = min(naxis,MAXNAX)
      if(nsize(1).gt.MAXDIM) call bug('f','Input file to big')

c     setup the region of interest

      call boxmask(lui,boxes,MAXBOX)
      call boxset(boxes,MAXNAX,nsize,' ')
      call boxinfo(boxes,MAXNAX,blc,trc)

c     IMFIT: determine image type: total intensity I or Q, U or V

      call coInit(lui)
      call coFindAx(lui,'stokes',iax)
      if(iax.ne.0) then
        call coCvt1(lui,iax,'ap',1.d0,'aw',dpol)
        inten = PolsPara(nint(dpol))
        pol = PolsC2P(nint(dpol))
      else
        inten = .true.
      end if

      return
      end

c -----------------------------------------------------------------------
c +++ INITVARY
c
c     determine parameter varriability
c
c     vf(1) flux
c     vf(2) l-coordinate
c     vf(3) m-coordinate
c     vf(4) semi-major axis
c     vf(5) semi-minor axis
c     vf(6) position angle
c
c ---

      subroutine initvary
      implicit none

      include 'imsad.h'

      vf(1) = .true.       
      vf(2) = .true.       
      vf(3) = .true.       

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

      return
      end 

c -----------------------------------------------------------------------
c +++ HIST
c
c     compute image plane histogram and moments
c
c ---

      subroutine hist(lui,blc,trc,image,mask,domask,clip,doplot,dolog)
      implicit none

      include       'maxdim.h'
      include       'maxnax.h'
      include       'imsad.h'

      external       function

      integer        MAXBOX, MAXVAR, MAXBIN
      parameter     (MAXBOX=1024, MAXVAR=20, MAXBIN=50)

      integer        lui, i, j, n,
     -               ifail1, ifail2,
     -               nvar,
     -               blc(MAXNAX),
     -               trc(MAXNAX),   
     -               id, imax, p, q,
     -               ptr,
     -               i0, j0,
     -               ni

      real           clip, trms,
     -               mom(3),
     -               pmin, pmax,
     -               wmin, wmax,
     -               range, wid,
     -               rms, sum, squ,
     -               covar(MAXVAR*MAXVAR),
     -               x(MAXVAR),
     -               image(*),
     -               bmax, norm

      character*2    itoaf 
      character*80   line

      logical        dofit,
     -               domask,
     -               mask(*),
     -               found,
     -               doplot,
     -               dolog

      ns = 1
      nd = MAXBIN
      nc = 1 

c     initialise some things

      pmin = +9.9e9
      pmax = -9.9e9

      sum = 0.0
      squ = 0.0
      do i = 1, nd
        data(i) = 0
        xd(i) = 0.0
        yd(i) = 1.0
      end do
 
c     set offsets to zero (relative positions)

      xoff = 0.0
      yoff = 0.0

c     read image data, compute moments and determine pixel max and min values

      ni = trc(1) - blc(1) + 1 
      n = 0
      do j = blc(2), trc(2)
        j0 = j - blc(2) + 1
        do i = blc(1), trc(1)
          i0 = i - blc(1) + 1
          ptr = (j0-1)*ni + i0 
          if(.not.(domask .and. .not.mask(ptr))) then     
            n = n + 1
            sum = sum + image(ptr)
            squ = squ + image(ptr)**2
            if(image(ptr).lt.pmin) pmin = image(ptr)
            if(image(ptr).gt.pmax) pmax = image(ptr)     
          end if
        end do 
      end do
 
      if(n.eq.0) then
        call bug('w','no data to bin')
        return
      end if

      mom(1) = sum/n
      mom(2) = sqrt(squ/n - mom(1)**2)

c     compute minimum range of data to bin and bin width

      range = 2.0 * min(abs(pmax),abs(pmin))

      if(range.eq.0.0) then
        call bug('w','distribution not valid')
        return
      end if

      wid = range / nd
 
c     write to logfile (if required)

      write(line,100) mom(1)
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if

      write(line,200) mom(2)
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if

      write(line,300) pmin, pmax
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if

c     form histogram of image pixel values

      wmin = mom(1) - range / 2.0
      wmax = mom(1) + range / 2.0

      do j = blc(2), trc(2)
        j0 = j - blc(2) + 1
        do i = blc(1), trc(1)
          i0 = i - blc(1) + 1
          ptr = (j0-1)*ni + i0        
          if(.not.(domask .and. .not.mask(ptr))) then

            if(wmin.le.image(ptr) .and. image(ptr).le.wmax) then 

              id = int((image(ptr) - wmin) / wid) + 1
              data(id) = data(id) + 1.0
              xd(id) = xd(id) + image(ptr)

            end if

          end if
        end do 
      end do

c     compute average of values contributing to each bin

      do i = 1, nd
        if(data(i).gt.0.0) xd(i) = xd(i) / data(i) 
      end do 
 
c     remove empty bins from fit

      i = 1
      do while(i.le.nd)
        if(data(i).le.0.0) then
          nd = nd - 1 
          do j = i, nd
            data(j) = data(j+1)
            xd(j) = xd(j+1)
          end do  
        end if 
        i = i + 1
      end do 
      if(nd.eq.0) then
        call bug('w','no data in histogram')
        return
      end if

c     form Gaussian parameter estimates

      norm = 0.0 
      bmax = 0.0
      do i = 1, nd
        norm = norm + data(i)
        if(data(i).gt.bmax) then
          bmax = data(i)
          imax = i
        end if
      end do
      pf(1,1) = data(imax)          
      pf(2,1) = xd(imax)            
      
      found = .false.
      do i = 1, nd
        if(data(i).gt.bmax/2.0) then
          if(.not.found) then
            p = i                   
            found = .true.
          else
            q = i                   
          end if
        end if
      end do
      pf(5,1) = abs(xd(q) - xd(p))  

c     normalise and plot histogram data

      do i = 1, nd
        data(i) = data(i) / norm
      end do

      if(doplot) then
        call pgsvp(0.1,0.3,0.2,0.5)
        call pgswin(xd(1),xd(nd),0.0,data(imax))
        call pgsch(0.8)
        call pgbox('bcnts',0.0,0,'bcnts',0.0,0)
        call pglabel('Flux (mJy pixel\u-1\d)','Counts',' ') 
        call pgpoint(nd,xd,data,21)
      end if 

c     setup fitting parameters

      pf(3,1) = 1.0
      pf(4,1) = 1.0
      pf(6,1) = 0.0 

      vf(1) = .true. 
      vf(2) = .true.  
      vf(3) = .false. 
      vf(4) = .false.  
      vf(5) = .true. 
      vf(6) = .false.

c     fit histogram with a Gaussian

      call packvar(x,nvar,MAXVAR)     
      if(nvar.ne.0) dofit = .true.

      if(dofit) then 
        call lsqfit(FUNCTION,nd,nvar,x,covar,rms,ifail1,ifail2)
        call upackvar(x,nvar)
        if(ifail2.eq.0) call upackcov(covar,nvar)
        if(ifail1.ne.0)
     -    call bug('w','Failed to converge: ifail='//itoaf(ifail1))
        if(ifail2.ne.ifail1)
     -    call bug('w','Failed to determine covariance matrix')
      else
        call bug('w','Nothing to fit!')
        rms = 0
      end if

c     compute true image rms

      trms = pf(5,1) / sqrt(8.0 * log(2.0)) 
      clip = clip * trms

c     write to logfile (if required)

      write(line,400) trms
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if

      write(line,500) clip
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if

      if(doplot) call pgpoint(nd,xd,model,17)

      return

  100 format('mean = ',e9.3)
  200 format('rms = ',e9.3)
  300 format('min = ',e9.3,x,'max = ',e9.3)
  400 format('fitted image rms = ',e9.3)
  500 format('clipping level = ',e9.3)

      end

c +++ IMPARM
c
c     get things dealing with units
c
c     input:
c
c     lu ............ handle of the input dataset
c     k ............. plane of interest
c
c     output:
c
c     bvol .......... beam volume, in radians**2. Set to zero if this
c                     cannot be determined.
c     bvolp ......... beam volume in pixels.
c     bmaj,bmin,bpa . beam major, minor axes and position angle.
c
c     bpa is measured from North through East in radians, where North is
c     the direct of increasing value along 2nd axis and East the direct-
c     ion of increasing value along the first axis
c
c ---

      subroutine imparm(lu,ip,bmajp,bminp,bpap,bvolp,bmaj,bmin,bpa,bvol,
     - beam,ra0,de0,dolog)
      implicit  none

      include  'mirconst.h'

      integer   lu, ip

      real      bvol,
     -          bmaj, ma,
     -          bmin, mi,
     -          bpa, pa,
     -          ra0, de0

      double precision    tmp

      character bunit*16, ctype(2)*16,
     -          line*80

      real      bmajp, bminp, bpap, bvolp

      double precision crpix(2), crval(2), cdelt(2), x(3)

      logical   beam,
     -          dolog

      beam = .true.

c     get pointing parameters from image header
 
      call rdhdd(lu,'crval1',tmp,0.0)
      ra0 = real(tmp)
      call rdhdd(lu,'crval2',tmp,0.0)
      de0 = real(tmp)

c     get beam parameters from image header

      call rdhdr(lu,'bmaj',bmaj,0.)
      call rdhdr(lu,'bmin',bmin,0.)
      call rdhdr(lu,'bpa',bpa,0.)
      call rdhda(lu,'bunit',bunit,' ')
      call ucase(bunit)

      bpa = PI / 180.0 * bpa 

      if(bmaj*bmin.eq.0.0) beam = .false.

c     convert the beam to radians for the current plane

      if(beam) then   

c     convert from world to pixel 

        x(1) = 0.0d0
        x(2) = 0.0d0
        x(3) = 0.0d0
        call coGauCvt(lu,'op/op/op',x,'w',bmaj,bmin,bpa,
     -	 'p',bmajp,bminp,bpap)

c     convert from pixel to world

        x(3) = dble(ip)
        call coGauCvt(lu,'op/op/ap',x,'p',bmajp,bminp,bpap,
     -	 'w',bmaj,bmin,bpa)

c     determine the beam FWHM area in radians**2

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

        write(*,'(''! zero image header beam parameters'')')
        bvol = 0.0
        bvolp = 0.0

      end if 

      call gaussfid(bmaj,bmin,bpa,ma,mi,pa)

c     write out beam parameters

      write(line,'(''> BEAM major axis = '',e9.3)') ma
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if
      write(line,'(''> BEAM minor axis = '',e9.3)') mi
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if
      write(line,'(''> BEAM position angle = '',e9.3)') pa
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if
      write(line,'(''> BEAM units = '',a16)') bunit   
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if
      write(line,'(''> BEAM area = '',e9.3,x,e9.3)') bvol, bvolp
      if(dolog) then
        call logwrit(line)
      else
        write(*,'(a)') line
      end if

      return
      end

c +++ COORDFID
c
c     convert coordinates between world and pixel coordinates, need to
c     fix error estimates, the imfit ones are suspect
c
c     input:
c
c     lu ...... handle of the coordinate system.
c     ip ...... image plane index
c
c ---

      subroutine coordfid(lu,ip)
      implicit none

      include  'imsad.h'
      include  'mirconst.h'

      integer   lu, ip

      double precision in(3), out(3)

      real      ma,
     -          mi,
     -          pa

      integer   i

c     loop over components to fit

      do i = 1, nc

c     convert the position FR: absolute pixel TO: offset world radians

        in(1) = dble(pf(2,i))
        in(2) = dble(pf(3,i)) 
        in(3) = dble(ip) 

        call coCvt(lu,'ap/ap/ap',in,'ow/ow/ap',out)
       
        pf(2,i) = real(out(1))            
        pf(3,i) = real(out(2))            

c     convert gaussian parameters FR: absolute pixel TO: world radians

        call coGauCvt(lu,'ap/ap/ap',in,'p', pf(4,i),pf(5,i),pf(6,i),
     -   'w',ma,mi,pa)

c     convert units - the errors are screwed

        pf(4,i) = ma                     
        pf(5,i) = mi                     
        pf(6,i) = pa                     
     
      end do

      return
      end

c -----------------------------------------------------------------------
c +++ ESTIMATE
c
c     generate an estimate of the model parameters, units are pixels,
c     radians and Jansky's
c
c ---

      subroutine estimate(bmaj,bmin,bpa)
      implicit none

      include 'imsad.h'
      include 'mirconst.h'

      integer i, ic 

      double precision  P,
     -        XP, YP,
     -        XYP, XXP, YYP,
     -        SP,
     -        WS,
     -        dd, dx, dy,
     -        tmp

      real    bmaj, bmin, bpa

      nc = 1
      ic = nc 

      SP = 0.0d0
      P = 0.0d0
      XP = 0.0d0
      YP = 0.0d0
      XYP = 0.0d0
      XXP = 0.0d0
      YYP = 0.0d0

c     need to cope with multi-component sources

      do i = 1, nd

        dd = dble(data(i))
        dx = dble(xd(i))
        dy = dble(yd(i))

        SP  = SP + dd            
        tmp = abs(dd)         
        P   = P   + tmp                   
        XP  = XP  + tmp * dx          
        YP  = YP  + tmp * dy         
        XYP = XYP + tmp * dx * dy
        XXP = XXP + tmp * dx * dx
        YYP = YYP + tmp * dy * dy

      end do

      if(P.eq.0.0d0) call bug ('f', 'error - zero flux')
     
      WS = 4.0d0 * log(2.0d0)  

      XP  = XP / P                      
      YP  = YP / P                   
      XYP = XYP / P - XP*YP  
      XXP = XXP / P - XP*XP
      YYP = YYP / P - YP*YP

      pf(2,ic) = real(XP)
      pf(3,ic) = real(YP)

      if(dogauss) then
 
        tmp = sqrt((XXP-YYP)**2 + 4.0d0*XYP**2)
        pf(4,ic) = real(sqrt(WS*(XXP + YYP + tmp)))
        pf(5,ic) = real(sqrt(WS*(XXP + YYP - tmp)))

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

      if(pf(4,ic)*pf(5,ic).eq.0.0) call bug ('f', 'error - zero width')

      pf(1,ic) = sign(WS*P/(PI*pf(4,ic)*pf(5,ic)),SP)

c      write(*,*) pf(1,ic), pf(2,ic), pf(3,ic)
c      write(*,*) pf(4,ic), pf(5,ic), pf(6,ic)

      return
      end

c -----------------------------------------------------------------------
c +++ PACKVAR
c
c     store all the things that we need to vary
c
c ---

      subroutine packvar(var,nvar,MAXVAR)
      implicit none

      include 'imsad.h'

      integer nvar,MAXVAR
      real    var(MAXVAR)

      integer i, j,
     -        ncurr

      real    tmp(6)

      nvar = 0

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

c     copy the estimates to the variables

        if(nvar+ncurr.gt.MAXVAR) 
     -   call bug('f','Too many free parameters')

        do j = 1, ncurr
          nvar = nvar + 1
          var(nvar) = tmp(j)
        end do

      end do

      return
      end

c -----------------------------------------------------------------------
c +++ UPACKVAR
c
c     unpack all the things that we need to vary
c
c ---

      subroutine upackvar(var,nvar)
      implicit none

      include 'imsad.h'

      integer nvar

      real    var(nvar)

      integer i, n

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

      return
      end

c -----------------------------------------------------------------------
c +++ UPACKCOV
c
c     unpack the covariance matrix
c
c ---

      subroutine upackcov(covar,nvar)
      implicit none

      include 'imsad.h'

      integer nvar

      real    covar(nvar,nvar)

      integer i, n

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

      return
      end

c -----------------------------------------------------------------------
c +++ FUNCTION
c
c     used by least-squares fitting LMDIFF
c
c     input:
c
c     m ..... number of functions     
c     n ..... number of variables (n <= m)
c     x ..... array of length n, on input contains an intial estimate
c             of the solution vector, on output contains the final
c             estimate of the solution vector
c
c     ouput:
c
c     fvec .. output array of length m which contains the function(s)
c             evaluated at the output x
c     iflag . the value of ifalg should not be changed by function unless 
c             the user wants to terminate execution of lmdiff, in this
c             case set iflag to a negative integer
c
c ---

      subroutine FUNCTION(m,nvar,var,fvec,iflag)
      implicit none

      include 'imsad.h'

      integer m,
     -        nvar,
     -        iflag

      real    var(nvar),
     -        fvec(m)

      integer i

      if(m.ne.nd) call bug('f','Inconsistency in FUNCTION')

c     unpack the things that we are solving for

      call upackvar(var,nvar)

c     evaluate the model

      call eval(pf,nc,xd,yd,m,fvec)

      do i = 1, m
        model(i) = fvec(i)          
        fvec(i) = data(i) - fvec(i) 
      end do

      return
      end	

c -----------------------------------------------------------------------
c +++ EVAL
c
c     evaluate the model Gaussian function
c
c     input:
c
c     n ....... number of points
c     x, y .... pixel coordinates at which to evaluate the model
c
c     output:
c
c     model ... the evaluated model
c
c ---

      subroutine eval(pf,nc,x,y,n,model)
      implicit none

      integer    MAXCOMP
      parameter (MAXCOMP=2)

      integer    nc, n

      real       pf(6,MAXCOMP),
     -           x(*),
     -           y(*),
     -           model(*)

      integer    i, j
     
      real       cospa, sinpa,
     -           tmp,
     -           xx, yy,
     -           xp, yp,
     -           xscal, yscal

c     set the model to zero initially

      do i = 1, n
        model(i) = 0.0
      end do

c     compute model [2] Gaussian component(s)

      do j = 1, nc

        cospa = cos(pf(6,j))
        sinpa = sin(pf(6,j))
        xscal = 4.0 * log(2.0) / pf(5,j)**2
        yscal = 4.0 * log(2.0) / pf(4,j)**2

        do i = 1, n

          xx = x(i) - pf(2,j)
          yy = y(i) - pf(3,j)

c          if(i.eq.1) write(*,*) xx, yy

          yp =  yy*cospa + xx*sinpa
          xp = -yy*sinpa + xx*cospa
          tmp = xscal*(xp**2) + yscal*(yp**2)
          if(tmp.lt.70.0) model(i) = model(i) + pf(1,j) * exp(-tmp)
           
        end do

      end do 

      return
      end

c +++ IMDISP
c
c     display image on pgplot device
c
c ---

      subroutine imdisp(image,ni,nj,blc,trc,pmin,pmax,dofid)
      implicit none
 
      include 'maxnax.h'

      real     tr(6),
     -         image(*),
     -         pmin, pmax,
     -         tfvp(4)

      integer  ni, nj,
     -         blc(MAXNAX), trc(MAXNAX)

      logical  dofid
 
c     initialise OFM routines

      call ofmini

c     setup viewport and window

      call pgsvp(0.35,0.9,0.1,0.9)
      call pgwnad(1.0,real(ni),1.0,real(nj))
      call pgsch(1.0)

c     setup world-coordinate transformation matrix
c
c     x = tr(1) + tr(2)*i + tr(3)*j
c     y = tr(4) + tr(5)*i + tr(6)*j 

      tr(1) = 0.0
      tr(2) = 1.0
      tr(3) = 0.0
      tr(4) = 0.0
      tr(5) = 0.0
      tr(6) = 1.0       

c     display image

      call pgimag(image,ni,nj,1,ni,1,nj,pmin,pmax,tr)

c     modify lookup table

      tfvp(1) = 0.1
      tfvp(2) = 0.6
      tfvp(3) = 0.3
      tfvp(4) = 0.9

      if(dofid) call ofmmod(tfvp,1,0.0,0,0.0,0.0)

      call pgpoint(1,real(ni)/2.0,real(nj)/2.0,2)

      return
      end

c +++ IMLOAD
c
c     load image region and mask (if any) into memory
c
c ---

      subroutine imload(lui,ip,blc,trc,image,mask,domask)
      implicit none

      include 'maxdim.h' 

      logical        hdprsnt

      integer blc(3), trc(3),
     -        ip,
     -        lui,
     -        i, j,
     -        ptr,
     -        ng, nb

      real    image(*),
     -        irow(MAXDIM)
 
      logical mask(*),
     -        domask,
     -        mrow(MAXDIM)

c     set image plane

      call xysetpl(lui,1,ip)

c     initialise and setup image mask and data arrays 

      domask = hdprsnt(lui,'mask')
      if(domask) write(*,'(''> pixel blanking will be applied'')')

      nb = 0
      ng = 0 
      ptr = 1

c     read image and mask (if required)

      do j = blc(2), trc(2)

        if(domask) call xyflgrd(lui,j,mrow)
        call xyread(lui,j,irow)            

        do i = blc(1), trc(1)

          if(domask) then
            mask(ptr) = mrow(i)
            if(mask(ptr)) then
              ng = ng + 1
            else
              nb = nb + 1
            end if
          else
            ng = ng + 1
          end if

          image(ptr) = irow(i)
          ptr = ptr + 1

        end do

      end do

      write(*,'(''> number of blanked pixels = '',i7)') nb
      write(*,'(''> number of good pixels = '',i7)') ng

      return
      end

c +++ ISLAND
c
c     detect islands of pixels above the cliping level 
c
c ---

      subroutine island(image,mask,domask,blc,trc,clip,iwin,jwin,ns,
     - doplot,inten)
      implicit none

      include 'maxdim.h'
      include 'maxnax.h'

      integer      MAXSRC
      parameter   (MAXSRC=2048)

      real         image(*),
     -             clip,
     -             px, py

      logical      mask(*),
     -             doplot

      integer      iwin(2,MAXSRC), jwin(2,MAXSRC),
     -             blc(MAXNAX),
     -             trc(MAXNAX),
     -             LHS, RHS,
     -             new(MAXDIM),
     -             old(MAXDIM),
     -             ptr,
     -             i, j,
     -             ns,
     -             i0, j0, ni,
     -             pi, pj,
     -             ci

      logical      merge,
     -             domask,
     -             null,
     -             inten

      if(doplot) call pgsci(7)

c     initailse parameters and arrays

      ns = 0
      do i = 1, MAXDIM
        old(i) = 0
        new(i) = 0
      end do

      LHS = blc(1)
      RHS = trc(1)
      ni = RHS - LHS + 1

c     begin search for islands

      do j = blc(2), trc(2)

        j0 = j - blc(2) + 1 

        do i = LHS, RHS

          i0 = i - blc(1) + 1
          ptr = (j0-1)*ni + i0  

c     includes all stokes parameters I > clip "OR" |QUV| > clip

          if((inten .and. image(ptr).lt.clip) .or.
     -       (.not.inten .and. abs(image(ptr)).lt.clip) .or.
     -       (domask .and. .not.mask(ptr)) ) then

            new(i) = 0

          else 

            px = real(i - blc(1) + 1)
            py = real(j - blc(2) + 1)
            if(doplot) call pgpoint(1,px,py,-1)

            if(i.gt.LHS .and. new(i-1).ne.0) then

              new(i) = new(i-1)
              call iadd(.false.,ns,new(i),iwin,jwin,i,j)

              merge = i.lt.RHS .and. old(i+1).ne.0 .and.
     -         old(i+1).ne.new(i)
              if(merge) 
     -         call imerge(new(i),old(i+1),ns,LHS,RHS,new,old,iwin,jwin)

            else if(i.gt.LHS .and. old(i-1).ne.0) then

              new(i) = old(i-1)
              call iadd(.false.,ns,new(i),iwin,jwin,i,j)

              merge = i.lt.RHS .and. old(i+1).ne.0 .and.
     -         old(i+1).ne.new(i)
              if(merge) 
     -         call imerge(new(i),old(i+1),ns,LHS,RHS,new,old,iwin,jwin)
 
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
           
        do i = LHS, RHS
          old(i) = new(i)
        end do 

      end do

c     remove null islands from collection

      i = 1
      do while(i.le.ns)
        null = iwin(1,i).eq.0 .and. iwin(2,i).eq.0 .and. jwin(1,i).eq.0
     -   .and. jwin(2,i).eq.0
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
       
      write(*,'(''> number of islands found = '',i4.4)') ns

c     plot bounded islands

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
              call pgpoint(1,px,py,-1)
            end do
          end do
        end do
      end if

      return
      end

c +++ ISCHECK
c
c     check islands for valid extents and minimum pixels, could add an
c     island merger if necessary
c
c     bvol ... future use
c
c ---

      subroutine ischeck(ns,iwin,jwin,blc,trc,bmaj,bmin,bpa,bvol,beam,
     - box,nbox,image,mask,domask,inten)
      implicit none

      include 'maxnax.h'
      include 'mirconst.h'

      integer      MAXSRC
      parameter   (MAXSRC=2048)

      integer      iwin(2,MAXSRC), jwin(2,MAXSRC),
     -             itmp(2,MAXSRC), jtmp(2,MAXSRC),
     -             blc(MAXNAX),
     -             trc(MAXNAX),
     -             ns,
     -             imin, jmin,
     -             ni, nj,
     -             i, j,
     -             di, dj,
     -             i0, j0, ptr,
     -             mi(MAXSRC),
     -             nbox,
     -             is

      real         bmaj, bmin, bpa, pa,
     -             bvol,
     -             tmpx, tmpy,
     -             dxy, dx, dy,
     -             box(2),
     -             maxpix(MAXSRC),
     -             image(*)

      logical      beam,
     -             mask(*),
     -             domask,
     -		   inten

c     compute beam extents as box(1) * dx or box(1) if zero beam parms 

      if(beam) then

        pa = PI - bpa 

c     code fragment taken from AIPS SAD task - units are pixels/radians

        tmpx = (sin(pa)/bmaj)**2 + (cos(pa)/bmin)**2
        tmpy = (cos(pa)/bmaj)**2 + (sin(pa)/bmin)**2
        dxy = ((1.0/bmaj)**2 - (1.0/bmin)**2)*(sin(pa)*cos(pa))**2
        dx = sqrt(0.25/(tmpx - dxy**2/tmpy))
        dy = sqrt(0.25/(tmpy - dxy**2/tmpx))

        imin = int(box(1) * dx)
        jmin = int(box(2) * dy)

      else

        imin = int(box(1))
        jmin = int(box(2))

      end if              

c     check minimum island extents based on beam parameters

      do i = 1, ns
  
        ni = iwin(2,i) - iwin(1,i) + 1 
        nj = jwin(2,i) - jwin(1,i) + 1

        di = int((imin - ni) / 2.0 + 0.5)
        dj = int((jmin - nj) / 2.0 + 0.5)

        if(ni.lt.imin) then
          iwin(1,i) = max(blc(1),iwin(1,i) - di)
          iwin(2,i) = min(trc(1),iwin(2,i) + di)          
        end if
        if(nj.lt.jmin) then
          jwin(1,i) = max(blc(2),jwin(1,i) - dj)
          jwin(2,i) = min(trc(2),jwin(2,i) + dj)
        end if
    
      end do

c     check box number limit

      if(ns.gt.nbox) then

        ni = trc(1) - blc(1) + 1

c     determine box pixel maxima - fixed sign problem for stokes QUV
 
        do is = 1, ns

          maxpix(i) = -9.0e9

          do j = jwin(1,is), jwin(2,is)
            j0 = j - blc(2) + 1   
            do i = iwin(1,is), iwin(2,is)
              i0 = i - blc(1) + 1
              ptr = (j0-1)*ni + i0
              if(.not.(mask(ptr) .and. .not.domask)) then

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

c     index maxima in ascending order

        call indexx(ns,maxpix,mi)

c     re-shuffle box arrays

        do is = 1, nbox
          i0 = ns - is + 1
          write(*,'(''> keeping box '',x,e10.3)') maxpix(mi(i0))
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

c     check minimum island pixel numbers based on beam area - necessary ???

      return
      end

c +++ IADD
c
c     add an island, ripped off from AIPS SAD
c
c ---

      subroutine iadd(newisl,ns,inew,iwin,jwin,i,j)
      implicit none

      integer      MAXSRC
      parameter   (MAXSRC=2048)

      integer inew,
     -        i, j,
     -        iwin(2,MAXSRC), jwin(2,MAXSRC),
     -        ns

      logical newisl

      if(newisl) then
 
        if(ns.le.MAXSRC) then
          ns = ns + 1
          inew = ns
          iwin(1,inew) = i
          iwin(2,inew) = i
          jwin(1,inew) = j
          jwin(2,inew) = j
        else
          write(*,'(''maximum islands detected'')')
          inew = 0 
        end if
 
      else

        if(inew.ne.0) then
          iwin(1,inew) = min(iwin(1,inew),i)
          iwin(2,inew) = max(iwin(2,inew),i)        
          jwin(2,inew) = j
        end if

      end if  
 
      return
      end 

c +++ IMERGE
c
c     island merge, ripped off from AIPS SAD and modified
c
c ---

      subroutine imerge(inew,iold,ns,LHS,RHS,new,old,iwin,jwin)
      implicit none

      include 'maxdim.h'

      integer    MAXSRC
      parameter (MAXSRC=2048) 

      integer    new(MAXDIM), old(MAXDIM),
     -           inew, iold, ns,
     -           LHS, RHS,
     -           i, il, ih,
     -           iwin(2,MAXSRC), jwin(2,MAXSRC)

      il = min(inew,iold) 
      ih = max(inew,iold) 

      iwin(1,il) = min(iwin(1,il),iwin(1,ih))
      iwin(2,il) = max(iwin(2,il),iwin(2,ih))
      jwin(1,il) = min(jwin(1,il),jwin(1,ih))
      jwin(2,il) = max(jwin(2,il),jwin(2,ih))

      iwin(1,ih) = 0
      iwin(2,ih) = 0
      jwin(1,ih) = 0
      jwin(2,ih) = 0

      do i = LHS, RHS
        if(old(i).eq.ih) old(i) = il
        if(new(i).eq.ih) new(i) = il
      end do

      return
      end

c +++ ISFIT
c
c     fit Gaussian components to detected pixel islands
c
c ---

      subroutine isfit(lui,ip,image,mask,domask,blc,trc,iwin,jwin,
     - bmajp,bminp,bpap,bvolp,bmaj,bmin,bpa,bvol,beam,ra0,de0,
     - doplot,outfile,luo)
      implicit none

      include 'maxnax.h'
      include 'imsad.h'

      external     function

      integer      MAXSRC, MAXVAR
      parameter   (MAXSRC=2048, MAXVAR=20)

      character*2  itoaf

      integer      ifail1, ifail2,
     -             nvar,
     -             iwin(2,MAXSRC), jwin(2,MAXSRC),
     -             ptr,
     -             i0, j0,
     -             ni, nj,
     -             i, j,
     -             ip, ic,
     -             lui,
     -             blc(MAXNAX),
     -             trc(MAXNAX),
     -             is,
     -             in, jn, ii, jj,
     -             luo

      real         bmaj, bmin, bpa, bvol,
     -             bmajp, bminp, bpap, bvolp,
     -             rms,
     -             covar(MAXVAR*MAXVAR),
     -             x(MAXVAR),
     -             image(*),
     -             tr(6),
     -             island(MAXPIX,MAXPIX),
     -             dmin, dmax,
     -             px, py,
     -             ra0, de0,
     -             xt, yt

      logical      dofit,
     -             domask,
     -             done,
     -             mask(*),
     -             beam,
     -             doplot

      character*2  flag(MAXCOMP),
     -             ctmp
      character*64 outfile

      data tr / 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 /

      ni = trc(1) - blc(1) + 1
      nj = trc(2) - blc(2) + 1

c     setup fitting paramters and units

      call initvary

c     loop through islands

      do is = 1, ns

        dmin = +9.0e9
        dmax = -9.0e9

        jn = jwin(2,is) - jwin(1,is) + 1
        in = iwin(2,is) - iwin(1,is) + 1

c     plot mid-point of box we are fitting

	if(doplot) then
          call pgsvp(0.35,0.9,0.1,0.9)
          call pgwnad(1.0,real(ni),1.0,real(nj))
          call pgpoint(1,(iwin(1,is)+iwin(2,is))/2.0,
     -     (jwin(1,is)+jwin(2,is))/2.0,2)
        end if

c     setup data arrays
 
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

            if(.not.(mask(ptr) .and. .not.domask)) then

              if(image(ptr).lt.dmin) dmin = image(ptr)
              if(image(ptr).gt.dmax) dmax = image(ptr) 

              island(ii,jj) = image(ptr)

              nd = nd + 1
              data(nd) = image(ptr)
              xd(nd) = real(i)
              yd(nd) = real(j)

              xt = xt + xd(nd)
              yt = yt + yd(nd)

c	      write(*,'(i2,2(x,f4.0),x,e10.3)') nd, xd(nd),
c     -         yd(nd), data(nd)

            else
              island(ii,jj) = 0.0   
            end if

          end do

        end do

        xoff = xt / real(nd)
        yoff = yt / real(nd)

c     check data for multiple peaks (if required)


c     plot island

        if(doplot) then
          call pgsvp(0.1,0.3,0.6,0.9)                  
          call pgswin(0.5,real(in)+0.5,0.5,real(jn)+0.5)
          call pgimag(island,MAXPIX,MAXPIX,1,in,1,jn,dmin,dmax,tr)
        end if

c     begin fitting data             

        nc = 0
        done = .false. 
        do while(.not.done)

          ctmp = '  '

c     determine estimates and prepare for fitting

          call estimate(bminp,bmajp,bpap) 
          call packvar(x,nvar,MAXVAR)

          dofit = nvar.ne.0 .and. .not.(nvar.ge.nd) .and. nd.gt.0

c     begin fitting

          if(dofit) then

            call lsqfit(FUNCTION,nd,nvar,x,covar,rms,ifail1,ifail2)
            call upackvar(x,nvar)

            if(ifail1.ne.0) then
              ctmp(1:1) = itoaf(ifail1) 
              call bug('w','failed to converge, ifail='//ctmp(1:1))
            end if

            if(ifail2.ne.ifail1) then
              call bug('w','failed to determine covariance matrix')
              ctmp(2:2) = 'F'              
            else if(ifail2.eq.0) then
              call upackcov(covar,nvar)
            end if

          else
            call bug('w','fit not possible')
            ctmp(1:1) = 'N'
          end if

c     inspect residue for multi-component islands and refit (if required)

          done = .true.

          flag(nc) = ctmp(1:2)

        end do
        
c     plot position of fitted centroid
 
        if(doplot) then
          call pgsci(10)
          do ic = 1, nc
            px = pf(2,ic) - real(iwin(1,is)) + 1.0        
            py = pf(3,ic) - real(jwin(1,is)) + 1.0
            call pgpoint(1,px,py,2)
          end do
        end if

c     convert fitted parameters to astronomical units and report fit

        call report(flag,ra0,de0,bmaj,bmin,bpa,bvol,beam,outfile,
     -   luo,lui,ip)
 
      end do 

      return
      end

c +++ REPORT
c
c     generate fit report
c
c ---

      subroutine report(flag,ra0,de0,bmaj,bmin,bpa,bvol,beam,
     - outfile,luo,lui,ip)
      implicit none

      include 'imsad.h'
      include 'mirconst.h' 

      integer      i,
     -             rc,
     -             fac,
     -             luo,
     -             lui, ip,
     -		   rlen, dlen

      real         ra0, de0,
     -             ra ,de,
     -             dra, dde,
     -             RADDEG,
     -             bmaj, bmin, bpa,
     -             smaj, smin, spa,
     -             dmaj, dmin, dpa,
     -             bvol,
     -             iflux, 
     -             wgt, sumwgt,
     -             wra, sumwra,
     -             wde, sumwde

      double precision	   ratmp,
     -		   detmp

      character*1  dflag, fflag
      character*2  flag(MAXCOMP)
      character*11 ras,
     -             des,
     -             dangle
      character*64 outfile
      character*86 line(MAXCOMP)
      character*9  label

      logical      beam

      RADDEG = 180.0 / PI

      sumwgt = 0.0
      sumwra = 0.0
      sumwde = 0.0

c     convert the fitted parameters to meaningful units
     
      call coordfid(lui,ip)

c     write beam characteristics

      call gaussfid(bmaj,bmin,bpa,smaj,smin,spa)

      write(*,1000) smaj, smin
      write(*,2000) spa

c     generate record label based on flux weighted mean component coordinates

      do i = 1, nc

c     compute integrated flux (if possible)

	write(*,4000) pf(1,i)

        if(bvol.gt.0.0) then
          iflux = pf(1,i) * pi/4 * pf(4,i) * pf(5,i) 
          iflux = iflux / log(2.0)
          iflux = iflux / bvol
        else 
          iflux = 0.0
        end if 

c     flag integrated flux < peak flux (caused by beam > source size)

        fflag = ' '
        if(iflux.ne.-999999.9 .and. iflux.lt.pf(1,i)) fflag = 'F'

        write(*,5000) iflux

c     convert major, minor and position angle 

        call gaussfid(pf(4,i),pf(5,i),pf(6,i),smaj,smin,spa)

        write(*,6000) smaj, smin
        write(*,7000) spa

c     deconvolve the beam (if possible) and setup source min/maj/pa
c     TODO - add as an option

        if(beam) then 
          call gaudfac(pf(4,i),pf(5,i),pf(6,i)*180/pi,bmaj,bmin,
     -     bpa*180/pi,fac,dmaj,dmin,dpa,rc)
          if(rc.eq.0) then
            dpa = pi/180 * dpa
            call gaussfid(dmaj,dmin,dpa,smaj,smin,spa)
            dflag = 'D'
          else if (rc.eq.1) then
            dflag = 'P'
	  else if (rc.eq.2) then
	    dflag = 'F'
          end if
        else
          dflag = '?'
        end if 

c     add centroid offsets to reference RA and Dec

        dra = (pf(2,i) * RADDEG) / cos(de0)
        dde = (pf(3,i) * RADDEG)

        write(*,3000) pf(2,i)*3600*RADDEG, pf(3,i)*3600*RADDEG

        ra = (RADDEG * ra0) + dra
        de = (RADDEG * de0) + dde

c     sum weighted coordinates

        wgt = pf(1,i)
        sumwgt = sumwgt + wgt 
        sumwra = sumwra + wgt * ra
        sumwde = sumwde + wgt * de

c     convert RA and Dec to sexigesimal notation

	if(ra.lt.0.0) ra = 360.0 + ra
c	ra = ra / 15.0

c        ras = dangle(dble(ra))
c        if(ras(3:3).ne.':') ras = '0'//ras(1:10)
c        des = dangle(dble(de))

	ratmp = dble(ra)
	detmp = dble(de)

	call ang2str('RD',1,ratmp,ras,rlen)
	call ang2str('DD',1,detmp,des,dlen)

c     write to display/logfile

        write(line(i),100) i, ras, des, pf(1,i),
     -   iflux, smaj, smin, spa, flag(i), dflag, fflag

      end do  

c     determine source label

      wra = sumwra / sumwgt
      wde = sumwde / sumwgt

      if(wra.lt.0.0) wra = 360.0 + wra
c      wra = wra / 15.0

c      ras = dangle(dble(wra))
c      if(ras(3:3).ne.':') ras = '0'//ras(1:10)
c      des = dangle(dble(wde))

      ratmp = dble(wra)
      detmp = dble(wde)

      call ang2str('RD',1,ratmp,ras,rlen)
      call ang2str('DD',1,detmp,des,dlen)

      label = ras(1:2)//ras(4:5)//des(1:3)//des(5:6)

c     write report records

      do i = 1, nc

        if(outfile.ne.' ') then
          write(luo,'(a9,'':'',a86)') label,line(i)
        else
          write(*,'(a9,'':'',a86)') label,line(i)
        end if

      end do

      return

  100 format(i2.2,2(x,a11),5(x,e10.3),x,a2,a1,a1)

 1000 format('> beam major/minor axes',2(x,f6.2))
 2000 format('> beam position angle',x,f6.1)
 3000 format('> offsets',2(x,e10.3))
 4000 format('> peak flux (Jy)',x,e10.3)
 5000 format('> integrated flux (Jy)',x,e10.3)
 6000 format('> major/minor axes',2(x,f6.2))
 7000 format('> position angle',x,f6.1)

      end 
 
c +++ GAUSSFID
c
c     fiddle gaussian parameters by converting from radians to arcsec
c     and degrees and fixing orientation
c
c     input:
c
c     a1	axis 1 (radians)
c     b1	axis 2 (radians)
c     p1	position angle (radians)
c
c     output:
c
c     a2	major axis (arcsec)
c     b2	minor axis (arcsec)
c     p2	position angle (degrees)
c
c ---

      subroutine gaussfid(a1,b1,p1,a2,b2,p2)
      implicit none

      include 'mirconst.h'

      real a1, b1, p1,
     -     a2, b2, p2,
     -     ma, mi, pa,
     -     tmp

      ma = a1 * 3600*180/pi
      mi = b1 * 3600*180/pi
      pa = p1 * 180/pi

      if(ma.lt.mi) then
        tmp = ma
        ma = mi
        mi = tmp
        pa = pa + 90.0
      end if

      pa = mod(pa,180.0)
      if(pa.lt.-90.0) pa = pa + 180.0
      if(pa.gt.+90.0) pa = pa - 180.0 

      a2 = ma
      b2 = mi
      p2 = pa

      return
      end

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

c +++ ANG2STR
c
c     convert degrees/hours value into a formatted string of required
c     precision
c
c     Command:
c
c     RR	input is RA expressed as radians
c      D	                         degrees
c      H                                 hours
c
c     DR	input is DEC expressed as radians
c      D                                  degrees
c
c     Precision:	
c
c     RA:				DEC:
c
c     0 HH:MM:SS.S			[+/-]DD:MM:SS
c     1 HH:MM:SS.SS			[+/-]DD:MM:SS.S
c     2 HH:MM.SS.SSS			[+/-]DD:MM:SS.SS
c     3 HH:MM:SS.SSSS			[+/-]DD:MM:SS.SSS
c
c     input:
c
c     cmd	conversion command(s)
c     p		string precision 
c     theta	angle in decimal degrees or hours.
c
c     output:
c
c     dangle	angle formated into a string with format [+/-]DD:MM:SS.SS
c
c ---

      subroutine ang2str(cmd,p,theta,tmpstr,length)
      implicit none
      include 'mirconst.h'
      double precision degrad, raddeg
      parameter (raddeg=180.0d0/dpi, degrad=dpi/180.0d0)    


      double precision ROUND,
     -          theta,
     -          tmp,
     -          prec,
     -          frac

      parameter (ROUND=0.5/360000000.0d0)

      integer   p,
     -          lra(4), lde(4), 
     -          dra(4), dde(4), 
     -          dd, mm, ss,
     -          N,
     -          length

      character line*13,
     -          tmpstr*(*),
     -          cmd*2

      logical   doRA, 
     -          doDEC
   
      data lra / 10, 11, 12, 13 /
      data lde /  9, 11, 12, 13 / 
      data dra /  1,  2,  3,  4 /
      data dde /  0,  1,  2,  3 /

      doRA = .false.
      doDEC = .false.

c     interpret command sequence and setup precision

      if(cmd(1:1).eq.'R') then
        doRA = .true.
        if(cmd(2:2).eq.'R') tmp = theta * RADDEG / 15.0d0
        if(cmd(2:2).eq.'D') tmp = theta / 15.0d0 
        if(cmd(2:2).eq.'H') tmp = theta
      else if(cmd(1:1).eq.'D') then
        doDEC = .true.
        if(cmd(2:2).eq.'R') tmp = theta * RADDEG
        if(cmd(2:2).eq.'D') tmp = theta  
      end if

c     fix rounding errors

      tmp = abs(tmp) + ROUND
 
c     setup line   

      if(doRA) then
        length = lra(p+1) 
        N = dra(p+1)
        prec = 10.0 ** (p+1)
      else if(doDEC) then
        length = lde(p+1)
        N = dde(p+1)
        prec = 10.0 ** p
      end if

      dd = int(tmp)                              
      mm = mod(int(60.0*tmp),60)                 
      ss = mod(int(3600.0*tmp),60)               
      frac = mod(int(3600.0*tmp*prec),prec)      

      if(N.gt.0) then 
        write (line,100) dd, mm, ss, int(frac)
      else
        write (line,200) dd, mm, ss
      end if

c     setup result

      if(doRA) then
        tmpstr(1:length) = line(1:length)
      else if(doDEC) then
        if(theta.lt.0.0) then
          tmpstr(1:length) = '-'//line(1:length-1)
        else
          tmpstr(1:length) = '+'//line(1:length-1)
        end if
      end if

      return
  
  100 format(i2.2,':',i2.2,':',i2.2,'.',i<N>.<N>)
  200 format(i2.2,':',i2.2,':',i2.2)

      end

c +++ STR2ANG
c
c     convert formatted string into degrees/hours value of required
c     precision
c
c     Command:
c
c     RR	output is RA expressed as radians
c      D	                         degrees
c      H                                 hours
c
c     DR	output is DEC expressed as radians
c      D                                  degrees
c
c     Precision:	
c
c     RA:				DEC:
c
c     0 HH:MM:SS.S			[+/-]DD:MM:SS
c     1 HH:MM:SS.SS			[+/-]DD:MM:SS.S
c     2 HH:MM.SS.SSS			[+/-]DD:MM:SS.SS
c     3 HH:MM:SS.SSSS			[+/-]DD:MM:SS.SSS
c
c     input:
c
c     cmd	conversion command(s)
c     p		string precision 
c     theta	angle in decimal degrees or hours.
c
c     output:
c
c     dangle	angle formated into a string with format [+/-]DD:MM:SS.SS
c
c ---

      subroutine str2ang(cmd,p,theta,tmpstr)
      implicit none
      include 'mirconst.h'
      double precision degrad, raddeg
      parameter (raddeg=180.0d0/dpi, degrad=dpi/180.0d0)
    
      integer      dd, mm, ss, frac,
     -             lra(4), lde(4), 
     -             dra(4), dde(4), 
     -             N,
     -             length,
     -             p

      character*2  cmd
      character*(*) tmpstr
      character*13  line

      double precision theta,
     -             tmp,
     -             prec,
     -             s

      logical      doRA,
     -             doDEC

      data lra / 10, 11, 12, 13 /
      data lde /  9, 11, 12, 13 / 
      data dra /  1,  2,  3,  4 /
      data dde /  0,  1,  2,  3 /

      doRA = .false.
      doDEC = .false.

c     setup line   

      if(cmd(1:1).eq.'R') then
        doRA = .true.
        length = lra(p+1) 
        N = dra(p+1)
        prec = 10.0d0 ** (p+1)
        line(1:length) = tmpstr(1:length)
        s = +1.0d0
      else if(cmd(1:1).eq.'D') then
        doDEC = .true.
        length = lde(p+1)
        N = dde(p+1)
        prec = 10.0d0 ** p
        if(tmpstr(1:1).eq.'+' .or. tmpstr(1:1).eq.'-') then
          line(1:length-1) = tmpstr(2:length)
          if(tmpstr(1:1).eq.'-') s = -1.0d0
          if(tmpstr(1:1).eq.'+') s = +1.0d0 
          length = length - 1
        else 
          line(1:length) = tmpstr(1:length)
          s = +1.0d0 
        end if
      end if

      if(N.gt.0) then 
        read(line(1:length),100) dd, mm, ss, frac
      else
        read(line(1:length),200) dd, mm, ss
        frac = 0.0d0
      end if

c     determine angle and perform conversions

      tmp = s * (dble(dd) + dble(mm)/60.0d0 + dble(ss)/3600.0d0 +
     - dble(frac) / (3600.0 * prec))  

      if(doRA) then
        if(cmd(2:2).eq.'R') theta = tmp * 15.0d0 * DEGRAD
        if(cmd(2:2).eq.'D') theta = tmp * 15.0d0 
        if(cmd(2:2).eq.'H') theta = tmp
      else if(doDEC) then
        if(cmd(2:2).eq.'R') theta = tmp * DEGRAD
        if(cmd(2:2).eq.'D') theta = tmp  
      end if

      return

  100 format(i2.2,':',i2.2,':',i2.2,'.',i<N>.<N>)
  200 format(i2.2,':',i2.2,':',i2.2)

      end
 

