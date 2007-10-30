c**********************************************************************c
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
c	the average (median) in the annulus
c	the rms of the annulus
c	the sum in the annulus normalized by the volume of the primary beam
c	   if there is one
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
c----------------------------------------------------------------------c
	include 'mirconst.h'
	include 'maxdim.h'
	include 'mem.h'
        character*(*) label,version
        parameter(version='version 1.0 27-Feb-96')
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
        real buf(maxdim),cospa,sinpa,cosi,x,y,r,ave,rms,fsum,cbof
        real pixe(maxdim),flux(maxdim),flsq(maxdim),pbfac
        logical mask(maxdim),dopb,keep,domedian, domode
        character in*64,logf*64,line*132,cin*1,ctype*9,caxis*13,units*13
        character btype*25,pbtype*16
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
        call keya('pbtype',pbtype,' ')
        call getopt(dopb,domedian,domode)
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
        if (dopb) then
          write(line,'(a,2f7.1,a,f5.0,a,f4.0,a)')
     *    '  Center: ',center,'  Ellipse pa: ',pa,
     *          '  Incline: ',incline,'  P.B. Corrected'
        else
          write(line,'(a,2f7.1,a,f5.0,a,f4.0,a)')
     *    '  Center: ',center,'  Ellipse pa: ',pa,
     *          '  Incline: ',incline,'  Not P.B. Corrected'
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
              r = sqrt((y*cospa-x*sinpa)**2+((y*sinpa+x*cospa)/cosi)**2)
              if(r.ge.rmin .and. r.le.rmax .and. keep)then
c
c  Make sure pixels on outer edge of ring go into current
c  not next ring
c
                ir = r/rstep + 1
                if (mod(r,rstep).eq.0.0 .and. r.ne.0.0) ir = ir - 1
c
                pixe(ir) = pixe(ir) + 1.
                flux(ir) = flux(ir) + buf(i)
                flsq(ir) = flsq(ir) + buf(i)*buf(i)
                irmin = min(ir,irmin)
                irmax = max(ir,irmax)
              end if
            enddo
          enddo
c
c If we want the median, make a second pass through the plane.
c We now know how big the arrays need to be for each annulus
c so allocate memory first for median arrays
c
          if (domedian.or.domode) then
            do ir = irmin, irmax
              call memalloc (ipm(ir), nint(pixe(ir)), 'r')
              pixe(ir) = 0.0
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
                r =
     *            sqrt((y*cospa-x*sinpa)**2+((y*sinpa+x*cospa)/cosi)**2)
                if(r.ge.rmin .and. r.le.rmax .and. keep)then
                  ir = r/rstep + 1
                  if (mod(r,rstep).eq.0.0 .and. r.ne.0.0) ir = ir - 1
c
                  pixe(ir) = pixe(ir) + 1.
                  memr(ipm(ir)+nint(pixe(ir))-1) = buf(i)
                end if
              enddo
            enddo
          end if
c
c  Write out the results (for mean and median only)
c
          call logwrit(' ')
          if (domedian) then
            write(line,'(a,a,a,a,a,a)') '   Radius(") ',
     *       '   Pixels   ', '  Median  ', '      rms  ',
     *       ' Ann. Sum  ',' Cum. Sum '
          else
            write(line,'(a,a,a,a,a,a)') '   Radius(") ',
     *       '   Pixels   ', ' Average  ', '      rms  ',
     *       ' Ann. Sum  ',' Cum. Sum '
          end if
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
            if (domedian) then
              call median (memr(ipm(ir)), nint(pixe(ir)), med)
              write(line,'(6f11.3,1x)') r,pixe(ir),med,rms,
     *                             flux(ir)/cbof,fsum/cbof
            else
              write(line,'(6f11.3,1x)') r,pixe(ir),ave,rms,
     *                             flux(ir)/cbof,fsum/cbof
            end if
c
            call logwrit(line(1:72))
          enddo
          if (domedian) then
            do ir = irmin, irmax
              call memfree (ipm(ir), nint(pixe(ir)), 'r')
            end do
          end if
c
c
c   write out the results for mode
c
c
          call logwrit(' ')
          if (domode) then
            write(line,'(a,a,a,a,a,a)') '   Radius(") ',
     *       '   Pixels   ', '  Mode    ', '      rms  ',
     *       ' Ann. Sum  ',' Cum. Sum '
          else
            write(line,'(a,a,a,a,a,a)') '   Radius(") ',
     *       '   Pixels   ', ' Average  ', '      rms  ',
     *       ' Ann. Sum  ',' Cum. Sum '
          end if
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
     *                             flux(ir)/cbof,fsum/cbof
            else
              write(line,'(6f11.3,1x)') r,pixe(ir),ave,rms,
     *                             flux(ir)/cbof,fsum/cbof
            end if
c
            call logwrit(line(1:72))
          enddo
          if (domode) then
            do ir = irmin, irmax
              call memfree (ipm(ir), nint(pixe(ir)), 'r')
            end do
          end if
c
c
c     end our mode addition
c





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
        end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine getopt (dopb,median,mode)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     dopb       DO primary beam corection
c     median     Use medians not means
c-----------------------------------------------------------------------
      implicit none
c
      logical dopb, median, mode
cc
      integer maxopt
      parameter (maxopt = 3)
c
      character opshuns(maxopt)*6
      logical present(maxopt)
      data opshuns /'pbcorr', 'median', 'mode'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      dopb = present(1)
      median = present(2)
      mode = present(3)
c
      end

c our mode subroutine
c************************************************************************
c* Mode -- Find the mode of an array of data.
c& vjm, wmw
c: miscellaneous
c+
	subroutine mode(x,n,xmode)
c
	implicit none
	integer n
        real    x(n)
	integer MAXBINS
	parameter(MAXBINS=100)

c
c  Determine the mode (distribution peak) of an array of real numbers.
c  On output, the input data array is sorted.
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
	integer i,j,ilo,ihi,iter,maxcount
	real sum,sum2,av,rms,xi,xmed,xmode
	real xlo,xhi,dx,xnext,xcount(MAXBINS)

c     sort data and evaluate the median

        call sortr(x,n)
	i = n/2
	if (2*i.eq.n) then
	  xmed = 0.5*(x(i) + x(i+1))
	else
	  xmed = x(i+1)
	endif

c       compute stats; another version of this program might
c       drop the upper & lower ?rms to reduce outlier effects
c       note that this works out the rms of the array of pixels
c       which has already had the outer deciles deleted

        iter=0
        sum=0.0
        sum2=0.0
        ilo=n/10
        ihi=9*n/10
	do i=ilo,ihi
	   xi=x(i)
	   sum=sum+xi
	   sum2=sum2+xi*xi
           iter=iter+1
	enddo
        av = sum/(float(iter))
        rms=sqrt(sum2/iter - sum*sum/iter**2)

c       divide inner 8 deciles of data into MAXBINS bins 

        dx=(x(n*9/10)-x(n/10))/MAXBINS 
        xlo=x(n/10)
        xhi=x(n*9/10)
c        write(6,*)'dx,xlo,xhi,ilo,ihi',dx,xlo,xhi,ilo,ihi
	xnext=xlo+dx
        maxcount=0
        xcount(1)=0
        j=1

c       find which bin contains the most data points: call the
c       middle of this bin the mode

	do i=ilo,ihi
	   if (x(i).lt.xnext) then
              xcount(j)=xcount(j)+1
              if (xcount(j).gt.maxcount) then
                 maxcount=xcount(j)
                 xmode=xnext-dx/2
              endif
	   else
              j=j+1
	      xnext=xnext+dx
	      xcount(j)=0
	   endif
	enddo

	end
