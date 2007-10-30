c***********************************************************************
        program ellint
        implicit none
c
c= ELLINT - Integrate a Miriad image in elliptical annuli.
c& mchw
c: image analysis
c+
c	ELLINT integrates a Miriad image in elliptical annuli in the first
c	two dimensions. E.g. to find the radial brightness distribution,
c	or flux density as a function of distance in a galaxy. The
c	integration is done separately for each image plane in the region
c	included.
c
c	The output consists of 6 columns. They are the
c
c	outer radius (arcsec) of the annulus
c	number of pixels in the annulus
c	the average (or median or mode) in the annulus
c	the rms of the annulus
c	the sum in the annulus (normalized by the volume of the primary beam
c	   if there is one)
c	the cumulative sum for all annuli so far
c
c@ in
c	Input image name. xyz images only. No default.
c@ region
c	Region of image to be integrated. E.g.
c	  % ellint region=relpix,box(-4,-4,5,5)(1,2)
c	integrates the center 10 x 10 pixels for image planes 1 and 2.
c	Unmasked pixels within the bounding box are selected.
c	The default region is the entire image.
c@ center
c	The offset of the center of the annuli in arcsec from the
c	reference pixel, measured in the directions of RA and DEC.
c@ pa
c	Position angle of ellipse major axis in degrees. Default is 0 (north).
c@ incline
c	Inclination angle in degrees. Default=0. (face on)
c@ radius
c	Inner and outer radii and step size along major axis in arcsecs.
c	The default is the whole image in steps equal to the pixel size.
c@ wedge
c       Opening angle (degrees) of wedge in which to integrate. 
c       Integration is within (pa +/- wedge). Default=180.
c@ pbtype
c	If you request that the fluxes be corrected for the primary beam
c	(see OPTIONS), ELLINT will normally construct a primary beam type
c	using information from the dataset. However you can override this
c	with a primary beam type of your own choosing. The primary beam
c	type can either be a telescope name whose primary beam is known
c	(e.g. hatcreek, vla, atca, etc) or you can select a Gaussian form
c	with "gaus(xxx)". Here xxx is the primary beam FWHM in arcseconds.
c	For example gaus(120) is a telescope with a 120 arcsec primary beam.
c@ options
c	Task enrichment options.  Minimum match is active.
c	  pbcorr    means correct the image for the primary beam
c	            attenutation before integrating.
c	  median    Find the median of each annulus instead of the average
c         olay      Write an overlay file showing the region that was sampled.
c                   The output filename will be <logfile>.olay. If no log file
c                   specified, the o/p goes to the terminal.
c@ log
c	The output log file. The default is the terminal.
c--
c  History:
c    mchw  aug 1982	Original version.
c    mchw  26jun90	Miriad version.
c    mchw  15nov90	Use pbfwhm from image header if present.
c			Omit checks on 3rd axis parameters.
c    mjs   16feb91      Delete internal subroutines which are now in
c                       the subroutine library (with permission).
c    mjs   25feb91      Changed references of itoa to itoaf.
c    mchw  02apr91	Allow non RA/DEC axes. Change to LogWrit.
c    mchw  03apr91	Write effective beam size in title lines.
c    mchw  07may91	Fix bug in center for reversed axes.
c    pjt   15may91      Fix bug in radius keyword (rstep->radius)
c    mchw  17may91	Add more info' to output.
c    mchw  05dec91	Default center pixel, improve doc.
c    rjs   10mar92	Re-added 's' flag to BoxSet.
c    pjt    4may92      read flags
c    mchw  29may92	Change g-format to f because Sun messes up.
c    mchw  22sep92	Change keyword to inclination angle.
c    nebk  28jan93      Adapt to new primary beam routines. Put pixels
c                       exactly on outer ring edge into that ring.
c    nebk  22aug94      Adapt to GETFREQ error status change
c    rjs   24oct94	Use new pb routines.
c    nebk  27feb96      Add options=median, more doc.
c    vjm   04jun96      Various changes including median and modes.
c----------------------------------------------------------------------c
	include 'mirconst.h'
	include 'maxdim.h'
	include 'mem.h'
c
        character*(*) label,version
        parameter(version='version 1.0 18-May-96')
        double precision rts,value
        parameter(rts=3600.d0*180.d0/dpi)
        parameter(label='Integrate a Miriad image in elliptical annuli')
        integer maxnax,maxboxes,maxruns,naxis,axis,plane,maxring
        parameter(maxnax=3,maxboxes=2048)
        parameter(maxruns=3*maxdim,maxring=200)
c
        integer boxes(maxboxes)
        integer i,j,ir,lin,nsize(maxnax),blc(maxnax),trc(maxnax)
        integer irmin,irmax,pbobj,ipm(maxring)
        real crpix(maxnax),cdelt(maxnax),var,med, xmode
        real center(2),pa,incline,rmin,rmax,rstep
        real wedge,rwedge,theta,rpa,a,b,cp,sp
        real buf(maxdim),cospa,sinpa,cosi,x,y,r,ave,rms,fsum,cbof
        real pixe(maxdim),flux(maxdim),flsq(maxdim),pbfac
        logical mask(maxdim),keep,dopb,domedian,domode,doolay
        character in*64,logf*64,line*132,cin*1,ctype*9,caxis*13,units*13
        character btype*25,pbtype*16,olayf*132,statist*32
c
c  Externals.
c
        integer len1
        character*1 itoaf
        real pbget
c
c Get inputs.
c
        call output( 'ELLINT: '//version )
        call keyini
        call keya('in',in,' ')
        call boxinput('region',in,boxes,maxboxes)
        call keyr('center',center(1),0.)
        call keyr('center',center(2),0.)
        call keyr('pa',pa,0.)
        call keyr('incline',incline,0.)
        call keyr('radius',rmin,0.)
        call keyr('radius',rmax,0.)
        call keyr('radius',rstep,0.)
        call keyr('wedge',wedge,180.)
        call keya('pbtype',pbtype,' ')
        call getopt(dopb,domedian,domode,doolay)
        call keya('log',logf,' ')
        call keyfin
c
c  Check inputs.
c
        if(in .eq. ' ') call bug('f','No input specified.')
        call xyopen(lin,in,'old',maxnax,nsize)
        call rdhdi(lin,'naxis',naxis,0)
        naxis = min(naxis,maxnax)
        if(nsize(1).gt.maxdim)call bug('f','Input file too big for me')
c
c  Set up the region of interest.
c
        call boxmask(lin,boxes,maxboxes)
        call boxset(boxes,maxnax,nsize,'s')
        call boxinfo(boxes,maxnax,blc,trc)
c
c  Get center and pixel size from image.
c
        do i=1,naxis
          cin = itoaf(i)
          call rdhda(lin,'ctype'//cin,ctype,' ')
          call rdhdr(lin,'crpix'//cin,crpix(i),0.)
          call rdhdr(lin,'cdelt'//cin,cdelt(i),0.)

          if(i.le.2)then
            if(ctype(1:2).ne.'RA'.and.ctype(1:3).ne.'DEC')
     *      call bug('w','Axes 1 and 2 are not RA or DEC')
            if(crpix(i).eq.0)then
              crpix(i) = nsize(i)/2+1
              call bug('w','Center pixel missing - assume naxis/2+1')
            endif
            if(cdelt(i).eq.0)call bug('f','Pixel size missing')
          endif
        enddo
c
c Set up for primary beam correction
c
        if (dopb) then
          call rdbtype(lin,btype,' ')
          if (btype.ne.' ' .and. btype.ne.'intensity') then
            call bug ('w',
     *        'This is a '//btype(1:len1(btype))//
     *        ' image; are you sure')
            call bug ('w',
     *        'you want to apply the primary beam correction')
          endif
c
          if(pbtype.eq.' ')call pbread(lin,pbtype)
          call coinit(lin)
          call pbinit(pbobj,pbtype,lin)
        end if
c
c  Open the output text file and write title.
c
        call logopen(logf,'q')
        call logwrit(' ***** '//label//' *****')
        call logwrit('  Image = '//in(1:len1(in)))
        call title(lin,naxis,blc,trc,cbof)
        write(line,'(a,2f7.1,a,f5.0,a,f4.0,a,f4.0)')
     *    '  Center: ',center,'  Ellipse pa: ',pa,
     *          '  Incline: ',incline,'  Wedge: ',wedge
        call logwrit(line)
        if (dopb) then
          write(line,'(a)') '  P.B. Corrected'
        else
          write(line,'(a)') '  Not P.B. Corrected'
        end if
        call logwrit(line)
        if(cbof.ne.1)then
          write(line,'(a,f11.4,a,a)')
     *    '  Effective beam area: ',cbof, ' pixels',
     *    '  (Used to normalize total flux)'
          call logwrit(line(1:len1(line)))
        endif
c
c  Convert the inputs to more useful numbers, and defaults.
c
        do i=1,2
          if(cdelt(i).lt.0.) center(i) = -center(i)
          cdelt(i) = abs(cdelt(i)*rts)
        enddo
        if(rmax.eq.0.) rmax = nsize(1)*cdelt(1)
        if(rstep.eq.0.) rstep = cdelt(1)
        cospa = cos(pa*pi/180.)
        sinpa = sin(pa*pi/180.)
        cosi = cos(incline*pi/180.)
        rwedge = wedge*pi/180.
        rpa    = pa*pi/180.
c
c  Initialize integrals for each axis.
c
        axis = 3
        plane = blc(axis)
        do while(plane.le.trc(axis))
          call axistype(lin,axis,plane,ctype,caxis,value,units)
          call logwrit(' ')
          write(line,'(a,i4,4x,a,a,i4,2x,a,a)')
     *    'Axis: ',axis,ctype,'  Plane: ',plane,caxis,units
          call logwrit(line(1:len1(line)))
          do ir = 1,maxdim
            pixe(ir) = 0.
            flux(ir) = 0.
            flsq(ir) = 0.
          enddo
          fsum = 0.
          irmin = maxdim
          irmax = 0
c
c  Integrate in elliptical annuli.
c
          call xysetpl(lin,1,plane)
          do j = blc(2),trc(2)
            call xyread(lin,j,buf)
            call xyflgrd(lin,j,mask)
            y = (j-crpix(2))*cdelt(2) - center(2)
            do i = blc(1),trc(1)
              x = (i-crpix(1))*cdelt(1) - center(1)
              keep  = mask(i)
              if (keep.and.dopb) then
                pbfac = pbget(pbobj,real(i),real(j))
                keep = pbfac.gt.0
                if(keep) buf(i)=buf(i)/pbfac
              endif
c
              if (keep) then
              r = sqrt((y*cospa-x*sinpa)**2+((y*sinpa+x*cospa)/cosi)**2)
c                opening angle is angle on the sky, not in disk plane.
c                quantisation effect: loses pixels more than half-out of wedge
                 theta = abs(atan2(y,x)-(rpa+0.5*pi))
                 if (theta.gt.twopi) theta=theta-twopi
                 if(r.ge.rmin.and.r.le.rmax.and.theta.le.rwedge)then
c
c                   Make sure pixels on outer edge of ring go into current,
c                   not next ring
                    ir = r/rstep + 1
                    if (mod(r,rstep).eq.0.0 .and. r.ne.0.0) ir = ir - 1

                    pixe(ir) = pixe(ir) + 1.
                    flux(ir) = flux(ir) + buf(i)
                    flsq(ir) = flsq(ir) + buf(i)*buf(i)
                    irmin = min(ir,irmin)
                    irmax = max(ir,irmax)
                 end if
              endif
            enddo
          enddo
c
c If we want the median, make a second pass through the plane.
c We now know how big the arrays need to be for each annulus
c so allocate memory first for median arrays
c
          if (domode.or.domedian) then
            do ir = irmin, irmax
               if (nint(pixe(ir)).gt.0) then
                  call memalloc (ipm(ir), nint(pixe(ir)), 'r')
                  pixe(ir) = 0.0
               endif
            end do
c
            do j = blc(2),trc(2)
              call xyread(lin,j,buf)
              call xyflgrd(lin,j,mask)
              y = (j-crpix(2))*cdelt(2) - center(2)
              do i = blc(1),trc(1)
                x = (i-crpix(1))*cdelt(1) - center(1)
                keep  = mask(i)
                if (keep.and.dopb) then
                  pbfac = pbget(pbobj,real(i),real(j))
                  keep = pbfac.gt.0
                  if(keep) buf(i)=buf(i)/pbfac
                endif
c
                if (keep) then
                  r =
     *            sqrt((y*cospa-x*sinpa)**2+((y*sinpa+x*cospa)/cosi)**2)
c                 opening angle is angle on the sky, not in disk plane.
                  theta = abs(atan2(y,x)-(pa+0.5*pi))
                  if (theta.gt.twopi) theta=theta-twopi
                  if(r.ge.rmin.and.r.le.rmax.and.theta.le.rwedge)then
                     ir = r/rstep + 1
                     if (mod(r,rstep).eq.0.0 .and. r.ne.0.0) ir = ir-1
c
                     pixe(ir) = pixe(ir) + 1.
                     memr(ipm(ir)+nint(pixe(ir))-1) = buf(i)
                   end if
                endif
              enddo
            enddo
          end if
c
c  Write out the results
c
          call logwrit(' ')
          if (domode) then
             statist = '  Mode    '
          else
             if (domedian) then
                statist = '  Median  '
             else
                statist = ' Average  '
             end if
          endif
          write(line,'(a,a,a,a,a,a)') '   Radius(") ',
     *            '   Pixels   ', statist(1:len1(statist)),
     *             '       rms  ','   Ann. Sum  ','  Cum. Sum '
          call logwrit(line(1:72))
c
          do ir = irmin,irmax
            r = ir*rstep
            if(pixe(ir).ne.0.) then
              ave = flux(ir)/pixe(ir)
              var = flsq(ir)/pixe(ir)-ave*ave
              rms = 0.0
              if (var.gt.0.0) rms = sqrt(var)
            else
              ave = 0.0
              rms = 0.0
            endif
            fsum = fsum + flux(ir)
c
            if (domode) then
               call mode (memr(ipm(ir)), nint(pixe(ir)), xmode)
               write(line,'(6f11.3,1x)') r,pixe(ir),xmode,rms,
     *              flux(ir)/cbof,fsum/cbof
            else
               if (domedian) then
                  call median (memr(ipm(ir)), nint(pixe(ir)), med)
                  write(line,'(6f11.3,1x)') r,pixe(ir),med,rms,
     *                 flux(ir)/cbof,fsum/cbof
               else
                  write(line,'(6f11.3,1x)') r,pixe(ir),ave,rms,
     *                 flux(ir)/cbof,fsum/cbof
               end if
            endif
c
            call logwrit(line(1:72))
          enddo
          if (domode.or.domedian) then
             do ir = irmin, irmax
                if (nint(pixe(ir)).gt.0) then
                   call memfree (ipm(ir), nint(pixe(ir)), 'r')
                endif
             end do
          endif

c
c Increment plane
c
c
          plane = plane + 1
        enddo

c
c  All done.
c
        call xyclose(lin)
        call logclose

c  except for the overlay file...
        if (doolay) then
           ir = Len1(logf)
           olayf = logf(1:ir) // '.olay'
           call logopen(olayf,'q')
           do ir = irmin,irmax
              r = ir*rstep
              a = r
              b = a*cosi
              write(line,'(a,f10.2,a,4(e11.5,1x),f8.3,a)') 
     *        'oellipse arcsec arcsec ',r,' no ',center(1),center(2),
     *             a,b,pa,'0 0'

              call logwrit(line)
           enddo
c
c          ellipse equations are x=a*cos(th), y=b*sin(th)
c          then rotate by pa to get sky coords.
c          cgdisp puts adds pi/2 to any pa you give, such as ellipse pa.
c          but lines are drawn between endpoints, not as r,theta, so no pa
c          is added by cgdisp, must do it by hand.
           cp = cos(rpa+pi/2.)
           sp = sin(rpa+pi/2.)
c
c          need the signs of cdelt()s for writing the overlay.
c          They are all converted to abs values above, so must get them again
           call xyopen(lin,in,'old',maxnax,nsize)
           call rdhdi(lin,'naxis',naxis,0)
           naxis = min(naxis,maxnax)
           do i=1,naxis
             cin = itoaf(i)
             call rdhdr(lin,'cdelt'//cin,cdelt(i),0.)
           enddo

           a = r        * cos(twopi-rwedge)*cdelt(1)/abs(cdelt(1))
           b = r * cosi * sin(twopi-rwedge)*cdelt(2)/abs(cdelt(2))
           x =      a*cp + b*sp
           y = -1.0*a*sp + b*cp
           write(line,'(a,4(e11.5,1x),a)') 
     *  'line arcsec arcsec pa-w no ',center(1),center(2),x,y,' 0 0'
           call logwrit(line)
           a = r *        cos(0.0+rwedge)*cdelt(1)/abs(cdelt(1))
           b = r * cosi * sin(0.0+rwedge)*cdelt(2)/abs(cdelt(2))
           x =      a*cp + b*sp
           y = -1.0*a*sp + b*cp
           write(line,'(a,4(e11.5,1x),a)') 
     *  'line arcsec arcsec pa+w no ',center(1),center(2),x,y,' 0 0'
           call logwrit(line)
           call logclose
        endif

        end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine getopt (dopb,domedian,domode,doolay)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     dopb       DO primary beam corection
c     median     Use medians not means
c-----------------------------------------------------------------------
      implicit none
c
      logical dopb, domedian, domode, doolay
cc
      integer maxopt
      parameter (maxopt = 4)
c
      character opshuns(maxopt)*6
      logical present(maxopt)
      data opshuns /'pbcorr', 'median', 'mode', 'olay'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      dopb     = present(1)
      domedian = present(2)
      domode   = present(3)
      doolay   = present(4)
c
      end
c************************************************************************
c  Mode -- Find the mode of an array of data.
c  vjm
c  miscellaneous
c 
	subroutine mode(x,n,xmode)
c
	implicit none
	integer n
        real    x(n)
	integer MAXBINS
	parameter(MAXBINS=100)

c
c  Determine the mode (distribution peak) of an array of real numbers.
c  It merely chooses the most populous bin, doesn't fit a parabola etc.
c  On output, the input data array is sorted into ascending order.
c
c  Input:
c    n		Number of points.
c  Input/Output:
c    x		Data to find the mode of. On output, it is sorted in
c		ascending order.
c  Output:
c    xmode	The mode of the data.
c
c------------------------------------------------------------------------
	integer i,ilo,ihi,ibin,i1,i2,iter
	real sum,sum2,av,rms,xi,xmed,xmode
	real xlo,xhi,dx,xnext,xcount(MAXBINS)
c
        if (n.gt.1) then
           call sortr(x,n)
           i = n/2
           if (2*i.eq.n) then
              xmed = 0.5*(x(i) + x(i+1))
           else
              xmed = x(i+1)
           endif
           
c        write(6,*)'MODE:x(1),xmed,x(n)',x(1),xmed,x(n)
c       compute stats; drop the upper & lower 10% to reduce outlier effects
	   iter = 0
           sum=0.0
           sum2=0.0
           i1=n/10+1
           i2=9*n/10+1
           do i=i1,i2
              xi=x(i)
              sum=sum+xi
              sum2=sum2+xi*xi
              iter=iter+1
           enddo
           av = sum/(real(iter))
           rms = sqrt(abs(sum2/real(iter)-av*av))
c     write(6,*)'MODE:av,rms',av,rms
           xlo=xmed-2.0*rms
           xhi=xmed+2.0*rms
           
           if (xlo.lt.x(1).or.xhi.gt.x(n)) then
c     huge rms wil give xlo,xhi out of bounds; use deciles
              xlo=x(n*1/10)
              xhi=x(n*9/10)
           else
c     use recursive search to find where in the array xlow & xhi are
              i1=1
              i2=n/2
              iter=0
 200          i=(i1+i2)/2
              if (x(i).gt.xlo) then
                 i2=i
              else
                 i1=i
              endif
              iter=iter+1
c     write(6,*)i1,i,i2,x(i1),x(i),x(i2)
              if (i2-i1.gt.1) goto 200
              ilo=i
              i1=n/2
              i2=n
              iter=0
 201          i=(i1+i2)/2
              if (x(i).gt.xhi) then
                 i2=i
              else
                 i1=i
              endif
              iter=iter+1
              if (i2-i1.gt.1) goto 201
              ihi=i
           endif
           
c     bin it
           dx=4*rms/MAXBINS
           xnext=xlo+dx
           xcount(1)=1.0
           ibin=1
           
c           write(6,*)'MODE:',ilo,ihi,xlo,xhi,xnext,dx
           do i=ilo,ihi
              if (x(i).lt.xnext) then
                 xcount(ibin)=xcount(ibin)+1.0
              else
                 xnext=xnext+dx
                 ibin=ibin+1
                 xcount(ibin)=1.0
              endif
           enddo
           
c     find the mode
           call sortr(xcount,MAXBINS)
           i=xcount(MAXBINS)
           xmode=x(i)
        else
           xmode=x(1)   
        endif
c
	end
 



