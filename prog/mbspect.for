c**********************************************************************c
	program MbSpect
	implicit none
c
c  	MBSPECT makes an average spectrum of a specified region of a
c	Miriad image. The average spectrum can be plotted and/or written
c	out as an ascii file for further analysis.
c
c= MBSPECT - Make spectrum and measure velocities from a Miriad image.
c& lss
c: image analysis and display.
c+
c	MBSPECT makes an spectrum of the velocity or frequency axis of a
c	Miriad image. The spectrum is integrated, averaged or beam-weighted,
c       over the width specified, for the other image axes. Robust spectral
c       baselines can be fitted, and the profile parameterised (velocities,
c       widths, moments). The output spectrum can be plotted and/or written
c       out as a miriad and/or ascii file for further analysis.
c@ in
c	The input image. vxy and xyv images are acceptable inputs. 
c	No default.
c@ out
c	The output spectrum, if required. Spectral units are always in the
c       same units as the input file (even if a conversion is performed for
c       the plot and/or log file). This spectrum can be read back into
c       MbSpect. 
c@ coord
c       The position, in world coordinates, for which the spectrum is
c       required, e.g. coord=12:00:13,-42:00:43. The cube must have
c       an RA and a DEC axis. The pixel with the nearest position is
c       chosen. No default.
c@ width
c       Two numbers, being the spatial width of the box in pixels (in RA
c       and DEC) within which the spectrum is averaged (or integrated).
c       Must be odd numbers. Default is 1,1.
c@ xaxis
c	The x-axis can be plotted as 'channel', 'frequency' ('FREQ'), 
c       'optical' velocity ('FELO'),  'radio' velocity ('VELO'), or the units
c       in the image. The default which is whatever units are in the header.
c@ yaxis
c	If 'average' then the pixels enclosed in the x-y area specified
c	are averaged. If 'sum' they are summed and normalized if the units
c	are known. If 'point' they are optimally weighted according to 
c       the beam parameters, assuming that the source is unresolved.
c       Default is 'average'
c@ xrange
c	X-axis range for plot. The default is to self-scale to the region
c       requested. The units are km/s for velocity and MHz for frequency
c       (i.e. not the normal miriad convention).
c@ yrange
c	Y-axis range for plot. The default is to self-scale.
c@ hann
c	Hanning smoothing length (an odd integer < 15). The default is
c	no smoothing (hann = 1).
c@ order
c       Order of optional robust (clipped polynomial) fit (0-10) to be applied
c       to the spectral axis. If the order is positive (0 to 10), the fit is
c       plotted on top of the data; if negative (-0 to -10), the fit is
c       subtracted before plotting. The fit is always subtracted from any 
c       output data written. (For bulk removal of baselines in a cube, use 
c       contsub). Default is no fit.
c@ options
c	List of minimum match task enrichment options.
c	1deriv  Take 1-sided derivative of spectrum before plotting
c	        and after Hanning smoothing. Useful for Zeeman enthusiasts.
c	2deriv  Take 2-sided derivative of spectrum before plotting.
c	histo   Plot the spectrum as a histogram instead of joining points.
c	pstyle1 Alternative plot style, where the object and position
c               information is omitted and the comment field is centered
c               at the top of the plot. Typically, this is used for
c               publication-quality plots (the source name, if required.
c               should be inserted into the comment field).
c	pstyle2 Alternative plot style, where the object and position
c               information is omitted, the comment field is centered
c               at the top of the plot (as with options=pstyle1), and
c               the x and y axis labels are omitted. Typically, this is 
c               used to generate publication-quality n x m matrix plots
c               (the source name, if required should be inserted into 
c               the comment field).
c       measure Measure various spectral parameters on plotted spectrum.
c               If a profile window is set, the line is only measured 
c               within this window. If the order keyword is used, the fit
c               is always subtracted before spectral fitting. If a plot
c               device is selected, the width-maximised 50% and 20% points
c               are highlighted with a circle and the width-minimised
c               points are highlighted with a cross. Zeroth moment (profile
c               area), first moment (mean velocity/frequency/channel) and
c               second moment (dispersion) are calculated in the usual way.
c               These parameters are not robust unless careful use of the
c               profile and clip parameters is made. However, more
c               robust moment-like parameters are also calculated by using an
c               algorithm which minimises the mean absolute deviation of
c               the flux-weighted velocities
c@ clip
c       Two values. Exclude pixels with values in the range clip(1) to clip(2).
c       If only one value is given, then exclude -abs(clip) to abs(clip).
c@ mask
c       This specifies the x-axis ranges to be excluded from any
c       continuum fit, e.g. those containing line emission. It consists 
c       of a number of pairs, each pair giving a start and end x-value. 
c       The default is that all channels are line-free, which is quite a 
c       good approximation if the line is weak compared to the continuum. 
c       The units of the x-axis values are the same as given by the xaxis
c       keyword.
c@ profile
c       Two values. This specifies the x-axis range to be included for
c       profile measurement (options=measure). It consists of a start
c       and end x-value. The default is that all channels are used for
c       profile measurement. For weak lines, you will normally need to 
c       set a profile window, a mask window a clip level, or any 
c       combination of the above. The profile and mask windows may be 
c       the same, although the profile window is limited to a single pair 
c       of values. The units of the x-axis values are the same as given 
c       by the xaxis keyword.
c@ device
c	Standard PGPLOT device. See the help on "device" for more information.
c@ csize
c	Up to 2 values. Character sizes in units of the PGPLOT default
c       (which is ~ 1/40 of the view surface height) for the plot axis
c       labels and the title.
c       Defaults try to choose something sensible.  Use 0.0 to default
c       any particular value.
c@ log
c       Write spectrum to this ascii file. Spectral axis units are as
c       specified by the xaxis keyword. Default is no output file.
c@ comment
c	A one-line comment which is written into the logfile and any plot.
c--
c
c  History:
c    lss  26jul99  Copied "imspect" to "mbspect" for HIPASS-specific chores
c    rjs  29aug99  Some FORTRAN standardization.
c    rjs   3sep99  Change pgpt1 to pgpt calls.
c    lss   3sep99  pgpt bug when order negative; added rms output line
c    pjt  20sep99  fixed obvious syntax error - does anybody use flint anymore
c    lss  18apr02  Added axis label scaling (csize)
c    lss   8may02  Added alternative axis labelling, lengthened in,out,
c                  log strings
c----------------------------------------------------------------------c
	include 'maxdim.h'
	integer maxco,maxnax,naxis,maxch
 	character version*(*)

	parameter(version='version 08-May-2002')

	parameter(maxco=15,maxnax=3)
	parameter(maxch=32)
c
	integer blc(maxnax),trc(maxnax),nsize(maxnax)
        real wpix(maxdim),fit(maxdim),mask(2,maxch),profile(2)
	real lab1,lab2
	real spec(maxdim),chan(maxdim),value(maxdim),work(maxdim)
        real work1(maxdim),weight(maxdim),work2(4*maxdim)
	real coeffs(maxco),hwork(maxco),yrange(2),xrange(2),csize(2)
	double precision restfreq, coord(2), scoord(3), dtrc(3),dtemp
	double precision rcoord(3), etrc(3)
	real xdmin, xdmax, ydmin, ydmax, fac, temp, clip(2)
	real xv(2), yv(2), xw(2), yw(2), epoch
	real bmaj,bmin,bpa,cdelt1,cdelt2,cdelt3,cdeltr,cdeltd
	integer lIn,lOut,raxis,daxis,vaxis,i,nsmth,nchan,iostat
	integer width(2),poly,nmask
        logical none, deriv1, deriv2, histo, measure
	logical pstyle1, pstyle2, subpoly
	character*132 in,out,logf
        character*64 xlabel,ylabel,device,xaxis,yaxis, cpoly
	character title*130, line*72,comment*80, str*3, word*80
	character*9 object,date,rctype*9,dctype*9,vctype*9
	character*16 unit0
	character ra1*13, ra2*13, dec1*13, dec2*13
c
c  Externals.
c
	integer len1, pgbeg
        character itoaf*3, hangle*32, rangle*32
        logical   keyprsnt
c
c  Remove program ID, so this can pipe gif and ps files straight to web
c
c        call output( 'MbSpect: '//version )
c
c  Get inputs
c
	call keyini
	call keyf('in',in,' ')
        call keya('out',out,' ')
        call keyt('coord',coord(1),'hms',0.0d0)
        call keyt('coord',coord(2),'dms',0.0d0)
        if (coord(1).eq.0.0d0.and.coord(2).eq.0.0d0)
     *    call bug ('f', 'No default permitted for coord')
        call keyi('width',width(1),1)
        call keyi('width',width(2),width(1))
        if(mod(width(1),2).ne.1.or.mod(width(2),2).ne.1)
     *    call bug ('f', 'width must be an odd number')
	call keya('xaxis',xaxis,' ')
	call keya('yaxis',yaxis,'average')
        call keyr('xrange',xrange(1),0.0)
        call keyr('xrange',xrange(2),xrange(1))
        if (xrange(1).eq.xrange(2).and.xrange(1).ne.0.0)
     *    call bug ('f', 'Invalid x-axis plot range')
        call keyr('yrange',yrange(1),0.0)
        call keyr('yrange',yrange(2),yrange(1))
        if (yrange(1).eq.yrange(2).and.yrange(1).ne.0.0)
     *    call bug ('f', 'Invalid y-axis plot range')
        call keyr('clip',clip(1),0.)
        if(keyprsnt('clip'))then
          call keyr('clip',clip(2),0.)
        else
          clip(2) = abs(clip(1))
          clip(1) = -clip(2)
        endif
        call mkeyr('mask',mask,2*maxch,nmask)
	if(mod(nmask,2).ne.0)
     *	  call bug('f','Incomplete mask range given')
	nmask = nmask / 2
        call keyr('profile',profile(1),0.0)
        call keyr('profile',profile(2),profile(1))
	call keyi('hann',nsmth,1)
        if (nsmth.gt.maxco) then
          str = itoaf(maxco)
          call bug ('f', 'Hanning smoothing length must be <= '//str)
        end if
	call keya('order',cpoly,' ')
	if(cpoly.eq.' ') then
	   poly=-99
	else
	   read(cpoly, *, err=100) poly
	   if(cpoly(1:1).eq.'-') then
	     subpoly=.true.
	   else
	     subpoly=.false.
	   end if
	   goto 120
 100	   call bug('f', 'Order must be integer between -10 and 10')
 120	   poly=abs(poly)
	   if(poly.gt.10) goto 100
        endif
        call getopt (deriv1, deriv2, histo, measure, pstyle1, pstyle2)
	call keya('device',device,' ')
        call keyr('csize',csize(1),0.0)
        call keyr('csize',csize(2),csize(1))
	call keya('log',logf,' ')
	comment = ' '
	do while (keyprsnt('comment'))
            call keya('comment',word,' ')
            comment = comment(1:len1(comment))//' '//word(1:len1(word))
	enddo
	call keyfin
c
c  Check inputs.
c
	if(in .eq. ' ') call bug('f','No input specified.')
	if(device.eq.' '.and.logf.eq.' '.and.out.eq.' '.and.
     *    .not.measure) call bug('f',
     *    'None of out, device, log or options=measure'//
     *    ' specified')
	call xyopen(lIn,in,'old',maxnax,nsize)
	call rdhdi(lin,'naxis',naxis,0)
	naxis = min(naxis,maxnax)
	if(nsize(1).gt.maxdim)call bug('f','Input file too big for me')
c
c  Find v-axis.
c
	call GetVaxis(lIn,vaxis,vctype)
	if(vaxis.eq.0) call bug('f','Velocity axis was not found')
c
c  Find RA-axis.
c
	call GetRaxis(lIn,raxis,rctype)
	if(raxis.eq.0) call bug('f','RA axis was not found')
c
c  Find DEC-axis.
c
	call GetDaxis(lIn,daxis,dctype)
	if(raxis.eq.0) call bug('f','DEC axis was not found')
c
c  Find beam size, if needed
c
	if(yaxis.eq.'sum'.or.yaxis.eq.'point') then
          call rdhdr(lIn,'bmaj',bmaj,0.)
          call rdhdr(lIn,'bmin',bmin,0.)
          call rdhdr(lIn,'bpa',bpa,0.)
	  if(bmaj.eq.0.0.or.bmin.eq.0.0) 
     *     call bug('f','beam parameters not found')
	  call rdhdr(lIn,'cdelt'//itoaf(raxis),cdeltr,0.)
	  call rdhdr(lIn,'cdelt'//itoaf(daxis),cdeltd,0.)
	  call rdhdr(lIn,'cdelt1',cdelt1,0.)
	  call rdhdr(lIn,'cdelt2',cdelt2,0.)
	  call rdhdr(lIn,'cdelt3',cdelt3,0.)
	  if(yaxis.eq.'point') then
c
c  Only deal with circular beams just now
c
	    if(bmaj.ne.bmin) 
     *       call bug('f','can only deal with circular beam')
	  end if
	end if
c
c  Find equinox
c
	call rdhdr(lin,'epoch',epoch,0.)
c
c  x-axis
c
	if(xaxis.eq.' ') xaxis=vctype(1:4)

c
c  Find region of interest
c
	call coInit(lIn)
	scoord(raxis)=coord(1)
	scoord(daxis)=coord(2)
	scoord(vaxis)=0.0
	call coCvt(lIn,'aw/aw/aw',scoord,'ap/ap/ap',dtrc)
	do i=1,3
	   trc(i)=nint(dtrc(i))
	   blc(i)=trc(i)
	end do
c
c  Formatted value for requested coordinate
c
	ra1=hangle(coord(1))
	dec1=rangle(coord(2))
c
c  Formatted value for actual coordinate, if yaxis is sum or average
c
	if(yaxis.eq.'point'.and.(width(1).gt.1.0.or.
     *                              width(2).gt.1.0)) then
	   ra2=ra1
	   dec2=dec1
	else
	  do i=1,3
	     rcoord(i)=0.0
	     etrc(i)=dble(trc(i))
	  end do
	  call coCvt(lIn,'ap/ap/ap',etrc,'aw/aw/aw',rcoord)
	  ra2=hangle(rcoord(raxis))
	  dec2=rangle(rcoord(daxis))
	end if
c
c  Give box a spatial width
c
	blc(raxis)=blc(raxis)-width(1)/2
	trc(raxis)=trc(raxis)+width(1)/2
	blc(daxis)=blc(daxis)-width(2)/2
	trc(daxis)=trc(daxis)+width(2)/2
c
c  Find pixel range for spectral axis
c
	blc(vaxis)=1
	trc(vaxis)=nsize(vaxis)
	if(xaxis.ne.'channel') call coVelSet(lIn, xaxis)
        if(xrange(1).ne.0.0 .or. xrange(2).ne.0.0) then
	   if(xaxis.eq.'frequency'.or.xaxis.eq.'FREQ') then
	      fac=1000.0
	   else
	      fac=1.0
	   end if
	   if(xaxis.eq.'channel') then
	      blc(vaxis)=int(xrange(1))
	      trc(vaxis)=int(xrange(2))
	   else
	      call coCvt1(lIn,vaxis,'aw',dble(xrange(1)/fac),'ap',dtemp)
	      blc(vaxis)=int(dtemp)
	      call coCvt1(lIn,vaxis,'aw',dble(xrange(2)/fac),'ap',dtemp)
	      trc(vaxis)=int(dtemp)
	      if(trc(vaxis).gt.blc(vaxis)) then
		 blc(vaxis)=blc(vaxis)+1
              else
		 trc(vaxis)=trc(vaxis)+1
	      endif
	   end if
	endif
	call coFin(lIn)
c
c  Check if coordinate inside cubes
c
	if(nint(dtrc(raxis)).lt.1.or.
     *    nint(dtrc(raxis)).gt.nsize(raxis).or.
     *    nint(dtrc(daxis)).lt.1.or.
     *    nint(dtrc(daxis)).gt.nsize(daxis))
     *    call bug('f','Requested region not inside cube')
c
c  Reset spatial boundaries if necessary
c
        if(blc(raxis).lt.1.or.blc(daxis).lt.1.or.
     *     trc(raxis).gt.nsize(raxis).or.
     *     trc(daxis).gt.nsize(daxis))
     *   call bug('w', 'Region partially outside image - resetting'//
     *            ' spatial boundary')
	if(blc(raxis).lt.1) blc(raxis)=1
	if(blc(daxis).lt.1) blc(daxis)=1
	if(trc(raxis).gt.nsize(raxis)) trc(raxis)=nsize(raxis)
	if(trc(daxis).gt.nsize(daxis)) trc(daxis)=nsize(daxis)
c
c  Reset spectral boundaries if invalid
c
	if(blc(vaxis).gt.trc(vaxis)) then
	  temp=blc(vaxis)
	  blc(vaxis)=trc(vaxis)
	  trc(vaxis)=temp
	endif
	if(blc(vaxis).lt.1.or.trc(vaxis).gt.nsize(vaxis))
     *     call bug('w', 'Region partially outside image - resetting'//
     *            ' spectral boundary')
	if(blc(vaxis).lt.1) blc(vaxis)=1
	if(trc(vaxis).gt.nsize(vaxis)) trc(vaxis)=nsize(vaxis)

c
c  Set up the region of interest.
c
	nchan = trc(vaxis)-blc(vaxis)+1
c
c  Integrate the spectrum over the specified region.
c
	if(vaxis.eq.1) then
	  call vaxis1(lIn,naxis,dtrc,blc,trc,cdelt2,cdelt3,bmaj,
     *             bmin,yaxis,nchan,chan,spec,wpix,none)
	else if(vaxis.eq.3) then
	  call vaxis3(lIn,naxis,dtrc,blc,trc,cdelt1,cdelt2,bmaj,
     *             bmin,yaxis,nchan,chan,spec,wpix,none)
	else
	  call bug('f','this image orientation is not implemented')
	endif
	if(none) call bug('f','No good pixels in selected region')
c
c  Optionally Hanning smooth spectrum.
c
	if(nsmth.ge.3) then
	  call hcoeffs (nsmth, coeffs)
	  call hannsm (nsmth, coeffs, nchan, spec, hwork)
	endif
c
c  mbspect title
c
	call imheader(lIn,object,restfreq,date)
	title=' '
c
c  Get plot axes, convert units, and write labels.
c
	call axes(lIn,vaxis,xaxis,yaxis,nchan,naxis,wpix,
     *		  xlabel,ylabel,chan,value,spec,unit0)
c
c  Optionally take derivatives.
c
        if (deriv1 .or. deriv2) call der (deriv1, nchan, spec, work)
c
c  Polynomal fit
c
	if(poly.ge.0) then
	   call polyfit(poly,nchan,value,work2,weight,mask,nmask,unit0,
     *      spec,fit)
	end if
c
c  Subtract fit now if required
c

	if(poly.ge.0.and.subpoly) then
	   do i=1,nchan
	      spec(i)=spec(i)-fit(i)
	   end do
	else
	   do i=1,nchan
	      work(i)=clip(1)+fit(i)
	      work1(i)=clip(2)+fit(i)
	   end do
	end if
c
c  Open plot device if requested.
c
	if(device.ne.' ') then
	  iostat = pgbeg(0,device,1,1)
	  if(iostat.eq.0) call bug ('f', 'Error opening plot device') 
c 
c  Work out limits
c
	  if(xrange(1).ne.0.0 .or. xrange(2).ne.0.0) then
            xdmin = xrange(1)
            xdmax = xrange(2)
	  else
	    xdmin = value(1) - 0.05 * (value(nchan) - value(1))
	    xdmax = value(nchan) + 0.05 * (value(nchan) - value(1))
	  endif
c
          if (yrange(1).ne.0.0 .or. yrange(2).ne.0.0) then
            ydmin = yrange(1)
            ydmax = yrange(2)
          else
  	    ydmin = spec(1)
  	    ydmax = ydmin
	    do i = 1,nchan
	      ydmax = max(ydmax, spec(i))
	      ydmin = min(ydmin, spec(i))
	    enddo
c
  	    ydmax = ydmax + 0.05 * (ydmax - ydmin)
	    ydmin = ydmin - 0.05 * (ydmax - ydmin)
          end if
c
c  Make plots if requested.
c       
	  call pgscf(2)
	  lab1=1.1
	  lab2=1.1
	  if(csize(1).ne.0.0) lab1=lab1*csize(1)
	  if(csize(2).ne.0.0) lab2=lab2*csize(2)
	  call pgsch (lab1)
	  call pgsls (1)

	  if(pstyle2) then
	    call pgsvp(0.01+lab1/30.0,0.99,0.01+lab1/30.0,0.99-
     1                 lab2/30.0)
	  end if

	  call pgswin (xdmin, xdmax, ydmin, ydmax)
	  call pgbox('BCNTS1',0.0,0.0,'BCNTS',0.0,0.0)
c	  call pgenv (xdmin, xdmax, ydmin, ydmax, 0, 0)
          if (histo) then
  	    call pgHline (nchan,value,spec,2.)
          else
            call pgline (nchan,value,spec)
          endif
	  if (poly.ge.0) then
	     if(subpoly) then
	       call pgmove(xdmin,0.0)
	       call pgdraw(xdmax,0.0)
	       if (measure) then
		  call pgsls(2)
		  call pgmove(xdmin,clip(1))
		  call pgdraw(xdmax,clip(1))
		  call pgmove(xdmin,clip(2))
		  call pgdraw(xdmax,clip(2))
		  call pgsls(1)
	       end if
	     else
	       call pgsci(2+poly)
	       call pgline (nchan,value,fit)
	       call pgsci(1)
	       if (measure.and.(clip(1).ne.0.0.or.clip(2).ne.0.0)) then
	         call pgsls(2)
		 call pgline (nchan,value,work)
		 call pgline (nchan,value,work1)
	         call pgsls(1)
	       end if
             end if
	  endif
	  if (nmask.gt.0) then
	    call pgsls(2)
	    do i=1,nmask
	       call pgmove(mask(1,i),ydmin)
	       call pgdraw(mask(1,i),ydmax)
	       call pgmove(mask(2,i),ydmin)
	       call pgdraw(mask(2,i),ydmax)
	    end do
	    call pgsls(1)
          end if
	  if(measure.and.(profile(1).ne.0.0.or.profile(2).ne.0.0))then
	     call pgsci(2)
	     call pgsls(4)
             call pgmove(profile(1),ydmin)
             call pgdraw(profile(1),ydmax)
             call pgmove(profile(2),ydmin)
             call pgdraw(profile(2),ydmax)
	     call pgsls(1)
	     call pgsci(1)
	  end if
	  if(pstyle1) then
	    call pglab (xlabel,ylabel,comment)
	  else
	    if (.not.pstyle2) call pglab (xlabel,ylabel,' ')
c
c  Extra information
c
	  
	    call pgqvp(0,xv(1),xv(2),yv(1),yv(2))
	    call pgqwin(xw(1),xw(2),yw(1),yw(2))
	    call pgsvp(xv(1),xv(2),yv(2),1.0)
	    call pgswin(0.0,1.0,0.0,1.0)
	    call pgsch(lab2)
	    if(pstyle2) then
	       call pgptxt(0.5, 0.5, 0.0, 0.5,comment)
	    else
	       call pgscf(1)
	       call pgtext(0.0,0.8,'Object: '//object)
	       call pgtext(0.0,0.6,'Requested: '//ra1//dec1)
	       call pgtext(0.0,0.4,'Actual    : '//ra2//dec2)
	       title='Equinox  :'
	       if(epoch.ne.0.0) then
	          if(epoch.eq.1950.0) then
	            title='Equinox  : B1950'
	          else if(epoch.eq.2000.0) then
	            title='Equinox  : J2000'
	          else
	            write(title(13:16), '(i4)') int(epoch)
	          end if
	       end if
	       call pgtext(0.0,0.2,title)
	       call pgptext(1.0,0.8,0.0,1.0,comment)
	       call pgsvp(xv(1),xv(2),yv(1),yv(2))
	       call pgswin(xw(1),xw(2),yw(1),yw(2))
	       call pgsch(1.0)
	       call pgscf(2)
             endif
	   endif
	end if

c
c  Subtract fit now if required
c

	if(poly.ge.0.and..not.subpoly) then
	   do i=1,nchan
	      spec(i)=spec(i)-fit(i)
	   end do
	end if
c
c  Measure spectral parameters
c
	if(measure)then
	   call vmom(nchan,value,spec,fit,xaxis,unit0,clip,xrange,
     *               profile,work,work1,poly,subpoly,device)
	endif
c
c  Close graphics
c
	if(device.ne.' ')  call pgend

c
c  Write output miriad spectrum if desired.
c
	if(out.ne.' ') then
	  naxis=3
	  nsize(1) = nchan
	  nsize(2) = 1
	  nsize(3) = 1
	  call xyopen(lOut,out,'new',naxis,nsize)
	  call mbheader(lIn,lOut,coord,raxis,daxis,vaxis,blc,unit0)
	  call xywrite(lOut,1,spec)
c
c  Update history and close files.
c
	  call Hisopen(lOut,'append')
          call HisWrite(lOut,'MbSpect: '//version)
	  call HisInput(lOut,'MbSpect')
	  call HisClose(lOut)
	  call xyclose(lOut)
	end if
c
c  Write ascii spectrum if desired.
c
	if(logf.ne.' ') then
	  call txtopen(lOut, logf, 'new', iostat)
	  if(iostat.ne.0) call bug ('f', 'Error opening output file')
	  call txtwrite(lOut,'#'//title,1+len1(title),iostat)
	  call txtwrite(lOut,'#File: '//In,7+len1(In),iostat)
          line='#Robust polynomial order: none'
	  if(poly.ge.0) write(line(27:30),*) poly
	  call txtwrite(lOut,line(1:30),30,iostat)
          call txtwrite(lOut,'#Spectral  axis = '//xaxis(1:9),
     *                  32,iostat)
          call txtwrite(lOut,'#Intensity axis = '//unit0,
     *                  34,iostat)
	  call txtwrite(lOut,'#'//comment,1+len1(comment),iostat)
	  call txtwrite(lOut,'#  Channel     Spectral axis '//
     *       '   Intensity',41,iostat)
	  do i = 1, nchan
	    write (line,'(1pe12.5, 3x, 1pe12.5, 3x, 1pe12.5)')
     *						chan(i),value(i),spec(i)
	    call txtwrite (lOut, line, 45, iostat)
	    if(iostat.ne.0) call bug ('f', 'Error writing output file')
	  enddo
	  call txtclose(lOut)
	endif
c
c  All done.
c
	call xyclose(lIn)
	end
c
	subroutine GetVaxis(lIn,vaxis,vctype)
c
	implicit none
	integer lIn,vaxis
        character*(*) vctype
c
c  Find the v-axis.
c
c  Inputs:
c    lIn	The handle of the image.
c  Output:
c    vaxis	The velocity or frequency axis.
c
c		  03jul90 mchw.
c----------------------------------------------------------------------c
	integer axis,naxis
	character*9 ctype
c
c  Externals.
c
	character*1 itoaf
c
c  Look for v-axis
c
	vaxis = 0
	call rdhdi(lIn,'naxis',naxis,1)
	do axis=1,naxis
	  call rdhda(lIn,'ctype'//itoaf(axis),ctype,' ')
	  if(ctype(1:4).eq.'VELO'.or.ctype(1:4).eq.'FELO'
     *					.or.ctype(1:4).eq.'FREQ') then
	    vaxis = axis
            vctype = ctype
	  endif
	enddo
	end
c
	subroutine GetRaxis(lIn,raxis,rctype)
c
	implicit none
	integer lIn,raxis
        character*(*) rctype
c
c  Find the RA-axis.
c
c  Inputs:
c    lIn	The handle of the image.
c  Output:
c    raxis	The RA axis.
c
c----------------------------------------------------------------------c
	integer axis,naxis
	character*9 ctype
c
c  Externals.
c
	character*1 itoaf
c
c  Look for RA-axis
c
	raxis = 0
	call rdhdi(lIn,'naxis',naxis,1)
	do axis=1,naxis
	  call rdhda(lIn,'ctype'//itoaf(axis),ctype,' ')
	  if(ctype(1:2).eq.'RA') then
	    raxis = axis
            rctype = ctype
	  endif
	enddo
	end
c
	subroutine GetDaxis(lIn,daxis,dctype)
c
	implicit none
	integer lIn,daxis
        character*(*) dctype
c
c  Find the DEC-axis.
c
c  Inputs:
c    lIn	The handle of the image.
c  Output:
c    daxis	The DEC axis.
c
c----------------------------------------------------------------------c
	integer axis,naxis
	character*9 ctype
c
c  Externals.
c
	character*1 itoaf
c
c  Look for DEC-axis
c
	daxis = 0
	call rdhdi(lIn,'naxis',naxis,1)
	do axis=1,naxis
	  call rdhda(lIn,'ctype'//itoaf(axis),ctype,' ')
	  if(ctype(1:3).eq.'DEC') then
	    daxis = axis
            dctype = ctype
	  endif
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine vaxis1(lIn,naxis,dtrc,blc,trc,cdelt2,cdelt3,bmaj,
     *               bmin,yaxis,nchan,chan,spec,wpix,none)
c
	implicit none
	integer lIn,naxis,blc(naxis),trc(naxis),nchan
	real chan(nchan),spec(nchan),wpix(nchan)
	real dr,dd,fac,bmaj,bmin,cdelt2,cdelt3
	double precision dtrc(*)
	character*(*) yaxis
        logical none
c
c  get integrated spectrum for vaxis=1
c
c  Inputs:
c    lIn	The handle of the image.
c    naxis	Number of image axes.
c    yaxis      Can be 'average', 'sum' or 'point'
c    dtrc       Pixel position of object of interest (yaxis='point')
c    blc,trc	Corners of region of interest.
c    cdelt2     Axis 2 spatial increment in radians
c    cdelt3     Axis 3 increment in radians
c    bmaj       Major axis in radians
c    bmin       Minor axis in radians
c    nchan	Number of channels.
c  Output:
c    chan	Array of channel numbers.
c    spec	Integrated spectrum.
c    wpix	Number or weight of spatial pixels used for each channel
c    none       True if no good pixels selected
c----------------------------------------------------------------------c
	include 'maxdim.h'
	real buf(maxdim)
	logical flags(maxdim)
	integer i,j,k,indx
c
	do i = 1, nchan
	  spec(i)  = 0.0
	  chan(i)  = i + blc(1) -1
          wpix(i) = 0.0
	enddo
        none = .true.
c
	do k = blc(3),trc(3)
	  call xysetpl(lIn,1,k)
	  do j = blc(2),trc(2)
	    call xyread(lIn,j,buf)
	    call xyflgrd (lIn,j,flags)
	    do i = blc(1),trc(1)
	      if(flags(i)) then
		indx = i-blc(1)+1
		if(yaxis.eq.'point') then
		  dr=(real(j)-real(dtrc(2)))*cdelt2
		  dd=(real(k)-real(dtrc(3)))*cdelt3
		  fac=exp(-2.0*log(4.0)*(dr**2+dd**2)/(bmaj*bmin))
		else
		  fac=1.0
		end if
		spec(indx) = spec(indx) + fac*buf(i)
		wpix(indx) = wpix(indx) + fac**2
                none = .false.
	      endif
	    enddo
	  enddo
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine vaxis3(lIn,naxis,dtrc,blc,trc,cdelt1,cdelt2,bmaj,
     *               bmin,yaxis,nchan,chan,spec,wpix,none)
c
	implicit none
	integer lIn,naxis,blc(naxis),trc(naxis),nchan
	real chan(nchan),spec(nchan),wpix(nchan)
	real dr,dd,fac,bmaj,bmin,cdelt1,cdelt2
	double precision dtrc(*)
	character*(*) yaxis
        logical none
c
c  get integrated spectrum for vaxis=3
c
c  Inputs:
c    lIn	The handle of the image.
c    naxis	Number of image axes.
c    yaxis      Can be 'average', 'sum' or 'point'
c    dtrc       Pixel position of object of interest (yaxis='point')
c    blc,trc	Corners of region of interest.
c    cdelt1     Axis 1 spatial increment in radians
c    cdelt2     Axis 2 increment in radians
c    bmaj       Major axis in radians
c    bmin       Minor axis in radians
c    nchan	Number of channels.
c  Output:
c    chan	Array of channel numbers.
c    spec	Integrated spectrum.
c    wpix	Number or weight of spatial pixels used for each channel
c    none       True if no good pixels selected
c----------------------------------------------------------------------c
	include 'maxdim.h'
	real buf(maxdim)
	logical flags(maxdim)
	integer i,j,k,indx
c
	do i = 1, nchan
	  spec(i)  = 0.0
   	  wpix(i) = 0.0
	enddo
        none = .true. 
c
	do k = blc(3),trc(3)
	  call xysetpl(lIn,1,k)
	  indx = k-blc(3)+1
	  chan(indx) = k
	  do j = blc(2),trc(2)
	    call xyread(lIn,j,buf)
	    call xyflgrd (lIn,j,flags)
	    do i = blc(1),trc(1)
	      if(flags(i)) then
		if(yaxis.eq.'point') then
		  dr=(real(i)-real(dtrc(1)))*cdelt1
		  dd=(real(j)-real(dtrc(2)))*cdelt2
		  fac=exp(-2.0*log(4.0)*(dr**2+dd**2)/(bmaj*bmin))
		else
		  fac=1.0
		end if
		spec(indx) = spec(indx) + fac*buf(i)
		wpix(indx) = wpix(indx) + fac**2
                none = .false.
	      endif
	    enddo
	  enddo
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine axes(lIn,vaxis,xaxis,yaxis,nchan,naxis,wpix,
     *			xlabel,ylabel,chan,value,spec,unit0)
	implicit none
	integer lIn,vaxis,naxis,nchan
	character*(*) xaxis,yaxis,xlabel,ylabel,unit0
	real chan(nchan),spec(nchan),value(nchan)
	real wpix(nchan)
c
c  Get plot axes and write labels.
c  Inputs:
c    lIn	The handle of the image.
c    vaxis	The velocity or frequency axis.
c    naxis	Number of image axes.
c    xaxis	Units for xaxis. Can be 'channel','frequency','optical',
c               'radio' or (default) units in image.
c    yaxis	Units for yaxis. Can be 'average' (default), 'sum' or 'point'.
c    wpix	Number or weight of good pixels in integrated spectrum for 
c               each channel
c    nchan	Number of channels.
c    chan	Array of channel numbers
c  Output:
c    value	Array of xaxis values.
c    spec	Spectrum (with converted units).
c    xlabel	Label for xaxis.
c    ylabel	Label for yaxis.
c    unit0      yaxis units
c
c    - Could be much fancier by converting internal units 'JY' 'JY/BEAM'
c    etc. to requested units 'JY' 'JY/BEAM' 'K' etc. calling GetBeam
c    to get beam oversampling factor.
c
c----------------------------------------------------------------------c
	integer i
	character ctype*16,bunit*16
	real bmaj,bmin,omega,cbof
	double precision dtemp
c
c  Externals.
c
	character*1 itoaf
	integer len1
c
c  Get xlabel.
c
	call rdhda(lIn,'ctype'//itoaf(vaxis),ctype,' ')
	if(xaxis.eq.'channel') then
	  xlabel = 'Channel'
	else if(xaxis.eq.'FREQ'.or.xaxis.eq.'frequency') then
	  xlabel = 'Frequency (MHz)'
	else if (xaxis.eq.'VELO'.or.xaxis.eq.'radio') then
	  xlabel = 'Radio Velocity, \ficz(1+z)\u-1\d\fr'
     *	           //'(km s\u-1\d)'
	else if (xaxis.eq.'FELO'.or.xaxis.eq.'optical') then
	  xlabel = 'Velocity, \ficz\fr (km s\u-1\d)'
	else
	  xlabel = ctype(1:len1(ctype))
	end if
c
c  Convert xaxis units.
c
	call coInit(lIn)
	if(xaxis.ne.'channel') call coVelSet(lIn, xaxis)
	do i=1,nchan
	  call coCvt1(lIn,vaxis,'ap',dble(chan(i)),'aw',dtemp)
	  if(xaxis.eq.'FREQ'.or.xaxis.eq.'frequency') then
	     value(i) = 1000.0*dtemp
	  elseif(xaxis.eq.'channel') then
	     value(i)=chan(i)
	  else
	     value(i) = dtemp
	  endif
	enddo
	call coFin(lIn)
c
c  Get units and beam oversampling factor from image header.
c
	call GetBeam(lIn,naxis,bunit,bmaj,bmin,omega,cbof)
c
c  Normalize the spectra and get the yaxis.
c
	if(yaxis.eq.'average'.or.yaxis.eq.'point')then
	  do i=1,nchan
            if (wpix(i).gt.0.0) then
               spec(i) = spec(i)/wpix(i)
            else
               call bug ('f', 'Error normalizing integrated spectrum')
            end if
	  enddo
	  if(bunit(1:7).eq.'JY/BEAM') then
	     if(yaxis.eq.'point')then
	       unit0 = 'Jy'
	       ylabel = 'Flux Density (Jy)'
	     else
	       unit0 = 'Jy/b'
	       ylabel = 'Flux Density (Jy beam\u-1\d)'
	     end if
	  else   
             unit0=bunit(1:len1(bunit))
	     ylabel = 'Average Intensity ('//unit0(1:len1(unit0))//')'
	  end if
	else if(bunit.eq.'JY/PIXEL')then
          unit0='Jy'
	  ylabel = 'Total Intensity (Jy)'
	else if(bunit(1:7).eq.'JY/BEAM'.and.bmaj*bmin*omega.ne.0)then
	  do i=1,nchan
	    spec(i) = spec(i)/cbof
	  enddo
	  unit0='Jy'
	  ylabel = 'Total Flux Density (Jy)'
	else

c  Shouldn't get here now - disallowed

	  unit0=bunit(1:len1(bunit))//'*pix'
	  ylabel = 
     *      'Total Intensity ('//unit0//' x pixels)'
	endif
c
	end

	Subroutine ImHeader(lIn,object,restfreq,date)
c
	implicit none
	integer lIn
	character*9 object,date
	double precision restfreq
c
c  Get image header items 'object' 'restfreq' and 'date-obs' from image.
c
c  Input:
c    lIn	The handle of the Image.
c  Output:
c    object	The object or source name.
c    restfreq	The restfreq of the observations, in GHz.
c    date	The date of the observations.
c
c			mchw june 1990
c----------------------------------------------------------------------c
	call rdhda(lIn,'object',object,' ')
	call rdhda(lIn,'date-obs',date,' ')
	call rdhdd(lIn,'restfreq',restfreq,0.d0)
	end
c
c
      subroutine getopt (deriv1, deriv2, histo, measure, pstyle1,
     1                   pstyle2)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     deriv1     True means take 1-sided derivative of spectrum
c     deriv2     True means take 2-sided derivative of spectrum
c     histo      True means plot a histogram rather than a curve
c     measure    True means measure spectral parameters
c     pstyle1    True means use alternative plot style
c     pstyle2    True means use alternative plot style 2
c
c-----------------------------------------------------------------------
      implicit none
c
      logical deriv1, deriv2, histo, measure, pstyle1, pstyle2
cc
      integer maxopt
      parameter (maxopt = 6)
c
      character opshuns(maxopt)*7
      logical present(maxopt)
      data opshuns /'1deriv', '2deriv', 'histo', 'measure','pstyle1',
     1              'pstyle2'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      deriv1 = present(1)
      deriv2 = present(2)
      if (deriv1 .and. deriv2) deriv2 = .false.
      histo = present(3)
      measure = present(4)
      pstyle1= present(5)
      pstyle2= present(6)
c
      end
c
c
      subroutine der (deriv1, nchan, spec, work)
c-----------------------------------------------------------------------
c     Take derivative of spectrum
c
c   Inputs:
c     deriv1       True for one sided, false for two sided derivative
c     nchan        Number of channels
c     spec         SPectrum. On output contains derivative
c     work         Work array.
c-----------------------------------------------------------------------
      implicit none
c
      logical deriv1
      integer nchan
      real spec(nchan), work(nchan)
cc
      integer i
c-----------------------------------------------------------------------
      if (deriv1) then
        do i = 2, nchan
          work(i) = spec(i) - spec(i-1)
        end do
      else 
        do i = 2, nchan-1
          work(i) = 0.5 * (spec(i+1) - spec(i-1))
        end do
c
c  Fudge end
c
        work(nchan) = work(nchan-1)      
      end if
c
c  Fudge beginning
c
      work(1) = work(2)
c
c  Copy
c
      do i = 1, nchan
        spec(i) = work(i)
      end do
c
      end
c
c
      subroutine polyfit(poly,nchan,value,work2,weight,mask,nmask,unit0,
     *                   spec,fit)
c-----------------------------------------------------------------------
c     Polynomial fit of spectrum
c
c   Inputs:
c     poly         order of fit
c     nchan        Number of channels
c     value        Array of x-values.
c     work2        Work array (4*maxdim)
c     weight       Weight array (maxdim)
c     spec         Spectrum.
c     mask         Mask array
c     nmask        Number of mask pairs
c     unit0        yaxis units
c   Outputs:
c     weight       Weight array (maxdim)
c     fit         Polynomial fit
c-----------------------------------------------------------------------
      implicit none
c
      integer nchan,poly,nmask
      real spec(*),value(*),fit(*),work2(*),weight(*)
      real mask(2,nmask),xlim1,xlim2
      character*(*) unit0
cc
      real clip
      integer i,j,ifail,npts,niter
      double precision dfit
      real coef(11),serr,test2,work3(24)
      character*80 line
c-----------------------------------------------------------------------

c  Number of clipping iterations

      niter=5

c  Clip level (sigma)

      clip=2.0

c  Apply mask

      do i=1,nchan
	 weight(i)=1.0
	 if(nmask.gt.0) then
           do j=1,nmask
	      xlim1=min(mask(1,j),mask(2,j))
	      xlim2=max(mask(1,j),mask(2,j))
	      if(value(i).gt.xlim1.and.value(i).lt.xlim2)
     *          weight(i)=0.0
	   end do
         endif
      end do

c  Iterate

 100  if(niter.eq.0) goto 1000

c  Initialize

      serr=0.0
      do i=1,nchan
	 fit(i)=0.0
      end do

      do i=1,11
       coef(i)=0.0
      end do

c  Count unclipped values

      npts=0
      do i=1,nchan
	 if(weight(i).gt.0.0)  npts=npts+1
      end do

c  Polynomial fit

      ifail=1
      if(npts.gt.poly+1) then
        if(poly.gt.0) then
           call wpfit(poly,nchan,value,spec,weight,coef,test2,work3,
     *                work2,ifail)
        else
	   coef(1)=0.0
	   test2=0.0
           do i=1,nchan
	      coef(1)=coef(1)+weight(i)*spec(i)/real(npts)
           end do
	   do i=1,nchan
	      test2=test2+weight(i)*(spec(i)-coef(1))**2
	   end do
	   test2=sqrt(test2)
	   ifail=0
        end if
      end if
      if(ifail.ne.0) call bug('f', 'Clipped polynomial fit error')
c
c  RMS error corrected for dof (trap zero divides)
c
      if(npts.eq.11 .and. poly.eq.10) then
        serr=0.0
      else
        serr=test2/sqrt(real(npts-poly-1))
      end if

c  Evaluate polynomial

      do i=1,nchan
	 dfit=dble(coef(1))
	 if(poly.gt.0) then
	   do j=2,poly+1
	     if(value(i).ne.0.0) then
	       dfit=dfit+dble(coef(j))*dble(value(i))**(j-1)
             end if
	   end do
         end if
	 fit(i)=real(dfit)
      end do

c  sigma clip

      do i=1,nchan
	 if(weight(i).gt.0.0) then
	   if(abs(spec(i)-fit(i)).gt.clip*serr) 
     *        weight(i)=0.0
	 end if
      end do

c  Iteration count

      niter=niter-1
      goto 100

 1000 write(line(1:80), 1010) serr, unit0
 1010	format('#FR   Clipped rms: ', f9.4,1x, a)
      call output(line)
      write(line(1:80), 1020) npts, nchan
 1020	format('#FN   (', i4, ' out of ', i4,  ' channels)')
      call output(line)
      end	

c
      subroutine vmom (nchan,value,spec,fit,xaxis,unit0,clip,xrange,
     *                 profile,work,work1,poly,subpoly,device)
c-----------------------------------------------------------------------
c     Take moments of spectrum
c
c   Inputs:
c     nchan     Number of channels
c     value     Array of x-values
c     spec      Spectrum
c     fits      Fit
c     xaxis	Units for xaxis. Can be 'channel','frequency','optical',
c               'radio' or (default) units in image.
c     unit0     Brightness units
c     clip      clip levels
c     xrange    range of x values (default is full range)
c     profile   Measure within this range of x-values
c     work      Work array (maxdim)
c     work1     Work array (maxdim)
c     poly      Polynomial order
c     subpoly   Subtract polynomial
c     device    pgplot device
c-----------------------------------------------------------------------
      implicit none
c
      integer nchan,npts,poly
      real profile(*),fit(*)
      real spec(nchan), value(nchan), clip(*), work(*), work1(*)
      real xrange(2)
      character*(*) xaxis, unit0, device
      logical subpoly
cc
      integer i, len1, i50a(2), i50b(2), i20a(2), i20b(2), j
      integer imax, imin, imaxa, imaxb, fmax, jprof
      real spmax,spmin,spmaxa,spmaxb,v50a,v50b,v20a,v20b
      real v50,v20,w50,w20,prof(2),d(5),sg
      double precision mom0,mom1,mom2,delta,rmom0,rmom1,rmom2,dt
      character*16 ulabel, unit1, unit2
      character*80 line
      logical skip20, skip50
c-----------------------------------------------------------------------

      skip20=.false.
      skip50=.false.

c
c  Initialize
c
      do i=1,5
	 d(i)=0.0
      end do
c
c  Copy profile window
c
      prof(1)=min(profile(1),profile(2))
      prof(2)=max(profile(1),profile(2))
c
c Copy spectrum
c
      do i=1,nchan
	 work(i)=value(i)
	 work1(i)=spec(i)
      end do
      npts=nchan
c
c  Mask the spectrum outside the profile window
c
      j=0
      if(prof(1).ne.0.0.or.prof(2).ne.0.0) then
        if(xrange(1).ne.0.0.or.xrange(2).ne.0.0) then
	   if(xrange(2).gt.xrange(1)) then
	     if(prof(1).lt.xrange(1).or.prof(2).gt.xrange(2))
     *        call bug('f','Profile window extends beyond xrange')
	   else
             if(prof(1).gt.xrange(1).or.prof(2).lt.xrange(2))
     *        call bug('f','Profile window extends beyond xrange')
	   end if
	end if
	do i=1,nchan
	  if(work(i).gt.prof(1).and.work(i).lt.prof(2)) then
             j=j+1
	     work(j)=work(i)
	     work1(j)=work1(i)
	     if(j.eq.1) jprof=i-1
	  end if
	end do
	npts=j
      endif
      if(npts.le.0) then
	 call output('No spectral points selected')
	 return
      end if
c
c  Zeroth, first moment, maximum and minimum
c
      mom0=0.0
      mom1=0.0
      spmax=work1(1)
      spmin=spmax
      imax=1
      imin=imax
      delta=abs(work(2)-work(1))
      if(work1(1).lt.clip(1).or.work1(1).gt.clip(2)) then
        mom0=work1(1)*delta
        mom1=work(1)*work1(1)*delta
      endif
      do i=2,npts
	 delta=abs(work(i)-work(i-1))
	 if(work1(i).lt.clip(1).or.work1(i).gt.clip(2)) then
	   mom0=mom0+work1(i)*delta
	   mom1=mom1+work(i)*work1(i)*delta
	 end if
	 if(work1(i).gt.spmax) then
	    spmax=work1(i)
	    imax=i
         end if
	 if(work1(i).lt.spmin) then
	    spmin=work1(i)
	    imin=i
         end if
      end do
c
c  Normalise
c
      if(mom0.ne.0.0) then
	 mom1=mom1/mom0
      else
	 mom1=0.0
      end if
c
c  Second moment
c
      mom2=0.0
      delta=abs(work(2)-work(1))
      if(work1(1).lt.clip(1).or.work1(1).gt.clip(2)) then
        mom2=(work(1)-mom1)**2*work1(1)*delta
      end if
      do i=2,npts
	 if(work1(i).lt.clip(1).or.work1(i).gt.clip(2)) then
	   delta=abs(work(i)-work(i-1))
	   mom2=mom2+(work(i)-mom1)**2*work1(i)*delta
	 end if
      end do
c
c  Normalise
c
      if(mom0.ne.0.0) then
	 dt=mom2/mom0
         mom2=sqrt(abs(dt))
	 if(dt.lt.0.0d0) mom2=-mom2
      else
	 mom2=0.0
      end if

      if(xaxis.eq.'frequency'.or.xaxis.eq.'FREQ') then
         ulabel=unit0(1:len1(unit0))//' MHz'
         unit1='MHz'
      else if(xaxis.eq.'channel') then
         ulabel=unit0(1:len1(unit0))//'*chan'
         unit1='chan'
      else
	 ulabel=unit0(1:len1(unit0))//' km/s'
         unit1='km/s'
      end if
      unit2=unit1
c
c  Robust moments
c
      call rbmom(npts,work,work1,clip,rmom0,rmom1,rmom2)
c
c  Log results
c
      line=' '
      write(line,60) xaxis
 60	format('#MC   xaxis: ',a)
      call output(line)
      line=' '
      write(line,70) spmax, unit0(1:len1(unit0)), work(imax), unit1
 70	format('#MX   Maximum: ', f10.3, 1x, a, '   at ', f10.3, 1x, a)
      call output(line)
      write(line,80) spmin, unit0(1:len1(unit0)), work(imin), unit1
 80	format('#MN   Minimum: ', f10.3, 1x, a, '   at ', f10.3, 1x, a)
      call output(line)
      write(line,85) npts
 85	format('#NP   Number of spectral points: ', i5)
      call output(line)
      write(line,90) clip(1), clip(2), unit0
 90	format('#CL   Clipping inside range (',f9.3,',',f9.3, ')', 1x,a)
      call output(line)
      call output('      Moment:         0               1'//
     *            '               2')
      write(line,100) real(mom0),real(mom1),real(mom2)
 100	format('#MM       ',3(6x,f10.3))
      call output(line)
      call output('      Robust moments: 0               1'//
     *            '               2')
      write(line,105) real(rmom0),real(rmom1),real(rmom2)
 105	format('#MR       ',3(6x,f10.3))
      call output(line)
      write(line, 110) ulabel, unit1, unit2
 110	format('      units:        ',3a16)
      call output(line)
c
c  Width maximizing algorithm (intended for +ve profile galaxies, but
c  will work for absorption lines)
c
      if(abs(2.0*spmax).gt.abs(spmin)) then
	 spmaxa=spmax
	 fmax=imax
	 sg=1.0
      else
	 spmaxa=abs(spmin)
	 fmax=imin
	 sg=-1.0
      end if
      spmaxb=spmaxa
      do i=1,2
	 i50a(i)=0
	 i50b(i)=0
	 i20a(i)=0
	 i20b(i)=0
      end do
      do i=2,npts
	 if(sg*work1(i).gt.spmaxa/2.0) then
	    i50a(1)=i-1
	    i50a(2)=i
	    goto 600
	 end if
      end do
 600  do i=2,npts
	if(sg*work1(i).gt.spmaxa/5.0) then
	    i20a(1)=i-1
	    i20a(2)=i
	    goto 620
	 end if
       end do
 620   do i=npts-1,1,-1
	 if(sg*work1(i).gt.spmaxb/2.0) then
	    i50b(1)=i+1
	    i50b(2)=i
            goto 640
	 end if
       end do
 640   do i=npts-1,1,-1	
	 if(sg*work1(i).gt.spmaxb/5.0) then
	    i20b(1)=i+1
	    i20b(2)=i
	    goto 660
	 end if
       end do
 660	continue
c
c  Error check
c
       if(sg*work1(i50a(1)).gt.spmaxa/2.0.or.sg*work1(i50a(2)).lt.
     *   spmaxa/2.0.or.sg*work1(i50b(1)).gt.spmaxb/2.0.or.
     *   sg*work1(i50b(2)).lt.spmaxb/2.0) then
          call output('Unable to determine 50% max point')
          skip50=.true.
       end if
       if(sg*work1(i20a(1)).gt.spmaxa/5.0.or.sg*work1(i20a(2)).lt.
     *   spmaxa/5.0.or.sg*work1(i20b(1)).gt.spmaxb/5.0.or.
     *   sg*work1(i20b(2)).lt.spmaxb/5.0) then
          call output('Unable to determine 20% max width')
	  skip20=.true.
       end if
       v50a=0.0
       v50b=0.0
       v20a=0.0
       v20b=0.0
c
c  Interpolate velocities
c
       if(.not.skip50) then
         v50a=(spmaxa/2.0-sg*work1(i50a(2)))*(work(i50a(2))-
     *    work(i50a(1)))/(sg*work1(i50a(2))-sg*work1(i50a(1)))+
     *    work(i50a(2))
         v50b=(spmaxb/2.0-sg*work1(i50b(2)))*(work(i50b(2))-
     *    work(i50b(1)))/(sg*work1(i50b(2))-sg*work1(i50b(1)))+
     *    work(i50b(2))
       endif
       if(.not.skip20) then
         v20a=(spmaxa/5.0-sg*work1(i20a(2)))*(work(i20a(2))-
     *    work(i20a(1)))/(sg*work1(i20a(2))-sg*work1(i20a(1)))+
     *    work(i20a(2))
         v20b=(spmaxb/5.0-sg*work1(i20b(2)))*(work(i20b(2))-
     *    work(i20b(1)))/(sg*work1(i20b(2))-sg*work1(i20b(1)))+
     *    work(i20b(2))
       end if
c
c  Velocities and widths
c
       v50=(v50a+v50b)/2.0
       w50=abs(v50a-v50b)
       v20=(v20a+v20b)/2.0
       w20=abs(v20a-v20b)
c
c  Plot results
c
       if(device.ne.' ') then
c
c  Approximate offsets above fitted polynomial
c
	 if(poly.ge.0.and..not.subpoly) then
           d(1)=fit(fmax+jprof)
           d(2)=fit(i50a(1)+jprof)
           d(3)=fit(i50b(1)+jprof)
           d(4)=fit(i20a(1)+jprof)
           d(5)=fit(i20b(1)+jprof)
	 end if
	 call pgsci(2)
         call pgpt(1,work(fmax),sg*spmaxa+d(1),-10)
	 if(.not.skip50) then
           call pgpt(1,v50a,sg*spmaxa/2.0+d(2),4)
           call pgpt(1,v50b,sg*spmaxb/2.0+d(3),4)
	 end if
	 if(.not.skip20) then
           call pgpt(1,v20a,sg*spmaxa/5.0+d(4),4)
           call pgpt(1,v20b,sg*spmaxb/5.0+d(5),4)
	 end if
	 call pgsci(1)
       end if
c
c  Log results
c
	if(.not.skip50.and..not.skip20) then
         call output(' ')
         call output('      Width maximiser:')
         call output('                             50%            20%')
         write(line,800) '#MXC  ',v50,v20
 800	 format(a,'Line centre:', 2(6x,f10.3))
         call output(line)
         write(line,820) '#MXW  ',w50,w20
 820	 format(a,'Line width: ', 2(6x,f10.3))
         call output(line)
         write(line, 840) unit1, unit1
 840	 format('      units:                ',2a16)
         call output(line)
	end if
c
c  Width minimizing algorithm
c
      imaxa=fmax
      imaxb=imaxa
      do i=1,2
	 i50a(i)=0
	 i50b(i)=0
	 i20a(i)=0
	 i20b(i)=0
      end do
      do i=imaxa-1,1,-1
	 if(sg*work1(i).lt.spmaxa/2.0) then
	    i50a(1)=i
	    i50a(2)=i+1
	    goto 900
	 end if
      end do
 900  do i=imaxa-1,1,-1
	if(sg*work1(i).lt.spmaxa/5.0) then
	    i20a(1)=i
	    i20a(2)=i+1
	    goto 920
	 end if
       end do
 920   do i=imaxb+1,npts
	 if(sg*work1(i).lt.spmaxb/2.0) then
	    i50b(1)=i
	    i50b(2)=i-1
            goto 940
	 end if
       end do
 940   do i=imaxb+1,npts
	 if(sg*work1(i).lt.spmaxb/5.0) then
	    i20b(1)=i
	    i20b(2)=i-1
	    goto 960
	 end if
       end do
 960	continue
c
c  Error check
c
       skip20=.false.
       skip50=.false.
       if(sg*work1(i50a(1)).gt.spmaxa/2.0.or.sg*work1(i50a(2)).lt.
     *   spmaxa/2.0.or.sg*work1(i50b(1)).gt.spmaxb/2.0.or.
     *   sg*work1(i50b(2)).lt.spmaxb/2.0) then
          call output('Unable to determine 50% min point')
          skip50=.true.
       end if
       if(sg*work1(i20a(1)).gt.spmaxa/5.0.or.sg*work1(i20a(2)).lt.
     *   spmaxa/5.0.or.sg*work1(i20b(1)).gt.spmaxb/5.0.or.
     *   sg*work1(i20b(2)).lt.spmaxb/5.0) then
          call output('Unable to determine 20% min width')
	  skip20=.true.
       end if
       v50a=0.0
       v50b=0.0
       v20a=0.0
       v20b=0.0
c
c  Interpolate velocities
c
       if(.not.skip50) then
         v50a=(spmaxa/2.0-sg*work1(i50a(2)))*(work(i50a(2))-
     *    work(i50a(1)))/(sg*work1(i50a(2))-sg*work1(i50a(1)))+
     *    work(i50a(2))
         v50b=(spmaxb/2.0-sg*work1(i50b(2)))*(work(i50b(2))-
     *    work(i50b(1)))/(sg*work1(i50b(2))-sg*work1(i50b(1)))+
     *    work(i50b(2))
       endif
       if(.not.skip20) then
         v20a=(spmaxa/5.0-sg*work1(i20a(2)))*(work(i20a(2))-
     *    work(i20a(1)))/(sg*work1(i20a(2))-sg*work1(i20a(1)))+
     *    work(i20a(2))
         v20b=(spmaxb/5.0-sg*work1(i20b(2)))*(work(i20b(2))-
     *    work(i20b(1)))/(sg*work1(i20b(2))-sg*work1(i20b(1)))+
     *    work(i20b(2))
       endif
c
c  Velocities and widths
c
       v50=(v50a+v50b)/2.0
       w50=abs(v50a-v50b)
       v20=(v20a+v20b)/2.0
       w20=abs(v20a-v20b)
c
c  Plot results
c
       if(device.ne.' ') then
	 if(poly.ge.0.and..not.subpoly) then
           d(1)=fit(fmax+jprof)
           d(2)=fit(i50a(1)+jprof)
           d(3)=fit(i50b(1)+jprof)
           d(4)=fit(i20a(1)+jprof)
           d(5)=fit(i20b(1)+jprof)
	 end if
	 call pgsci(2)
	 if(.not.skip50) then
           call pgpt(1,v50a,sg*spmaxa/2.0+d(2),5)
           call pgpt(1,v50b,sg*spmaxb/2.0+d(3),5)
	 end if
	 if(.not.skip20) then
           call pgpt(1,v20a,sg*spmaxa/5.0+d(4),5)
           call pgpt(1,v20b,sg*spmaxb/5.0+d(5),5)
	 end if
	 call pgsci(1)
       end if
c
c  Log results
c
      if(.not.skip50.and..not.skip20) then
         call output(' ')
         call output('      Width minimiser:')
         call output('                             50%            20%')
         write(line,800) '#MNC  ',v50,v20
         call output(line)
         write(line,820) '#MNW  ',w50,w20
         call output(line)
         write(line, 840) unit1, unit1
         call output(line)
      end if
c
      end
c************************************************************************
      subroutine rbmom(ntot,vel,flux,clip,vmom0,vmom1,vmom2)
c-----------------------------------------------------------------------
c     Take robust moments of spectrum. Zeroth-second moments are
c     similar to normal moments. Skewness and kurtosis are somewhat
c     different.
c
c   Inputs:
c     npts      Number of points
c     vel       Array of x-values
c     flux      Spectrum
c     clip      Flux clip values
c   Output:
c     vmom0-3   Moments
c-----------------------------------------------------------------------
      implicit none
      integer ntot
      double precision vmom0, vmom1, vmom2
      real vel(*), flux(*), vwork(4096), fwork(4096), clip(*)
c
      integer niter, iter, npts, ivmin, i, j,k
      real clipr, x1,x2,delta, f(3), a,b,c, xvmin, tmp
c
c  Clip parameters
c
      niter=5
      clipr=2.5
c
c  fill work arrays
c
      do i=1,ntot
         vwork(i)=vel(i)
         fwork(i)=flux(i)
      end do
      npts=ntot
      iter=niter
 1200 if(npts.lt.3) return
      vmom0=0.0d0
      vmom1=0.0d0
      vmom2=0.0d0
      delta=abs(vwork(2)-vwork(1))
      if(fwork(1).lt.clip(1).or.fwork(1).gt.clip(2)) then
        vmom0=fwork(1)*delta
      endif
      do i=2,npts
	 delta=abs(vwork(i)-vwork(i-1))
	 if(fwork(i).lt.clip(1).or.fwork(i).gt.clip(2)) then
	   vmom0=vmom0+fwork(i)*delta
	 end if
      end do
c
c  minimum mean abs deviation
c
      vmom1=vwork(1)
      vmom2=abs(vwork(npts)-vwork(1))
      ivmin=1
      do j=1,npts
        delta=abs(vwork(2)-vwork(1))
        tmp=0.0
        if(fwork(1).lt.clip(1).or.fwork(1).gt.clip(2)) then
	   tmp=fwork(1)*delta*abs(vwork(1)-vwork(j))/vmom0
        end if
        do i=2,npts
	   delta=abs(vwork(i)-vwork(i-1))
	   if(fwork(i).lt.clip(1).or.fwork(i).gt.clip(2)) then
             tmp=tmp+fwork(i)*delta*abs(vwork(i)-vwork(j))/vmom0
	   end if
        end do
        if(tmp.lt.vmom2) then
           vmom2=tmp
           vmom1=vwork(j)
           ivmin=j
        end if
      end do
c
c  recompute mean abs deviation around mid 3 channels (ignore clipping)
c
      do k=1,3
         j=ivmin+k-2
         delta=abs(vwork(2)-vwork(1))
         f(k)=0.0
         if(fwork(1).lt.clip(1).or.fwork(1).gt.clip(2)) then
	   f(k)=fwork(1)*delta*abs(vwork(1)-vwork(j))/vmom0
         end if
         do i=2,npts
	   delta=abs(vwork(i)-vwork(i-1))
	   if(fwork(i).lt.clip(1).or.fwork(i).gt.clip(2)) then
             f(k)=f(k)+fwork(i)*delta*abs(vwork(i)-vwork(j))/vmom0
	   end if
         end do
      end do
c
c  parabolic fit to get approximate true minimum
c
      c=(f(3)-2.0*f(2)+f(1))/2.0
      b=(f(3)-f(1)-4.0*c*real(ivmin))/2.0
      a=f(2)-b*real(ivmin)-c*real(ivmin)**2
      xvmin=-0.5*b/c
      vmom1=vwork(int(xvmin))+(xvmin-int(xvmin))*
     *         (vwork(int(xvmin)+1)-vwork(int(xvmin)))
      vmom2=a+b*xvmin+c*xvmin**2
c
c  clip
c
      x1=vmom1-clipr*abs(vmom2)
      x2=vmom1+clipr*abs(vmom2)
      j=0
      do i=1,npts
         if(vwork(i).gt.x1.and.vwork(i).lt.x2) then
           j=j+1
           vwork(j)=vwork(i)
           fwork(j)=fwork(i)
         end if
      end do
      npts=j

      iter=iter-1
      if(iter.gt.0) goto 1200

      end

c************************************************************************
	subroutine mbheader(lIn,lOut,coord,raxis,daxis,vaxis,blc,unit0)
	implicit none
	integer lin,lOut,raxis,daxis,vaxis,blc(*)
	double precision coord(*)
	character*(*) unit0
c
c  Copy keywords to output file.
c
c  Inputs:
c    lIn,lOut	Handle of input and output files.
c    raxis	RA axis on input cube
c    daxis	DEC axis on input cube
c    vaxis	vel axis on input cube
c    blc        bottom left corner of sub-cube
c    unit0      Intensity axis
c------------------------------------------------------------------------
	character atemp*16,ctype*10,cin*2,cout*2,unit1*16
	double precision rtemp,cdelt,crval,crpix
	integer i,k
c
c  Externals.
c
	character itoaf*2
	logical hdprsnt
	integer len1
c
c  Be careful that nkeys and nckeys match the number of keywords.
c
	integer nkeys,nckeys
	parameter (nkeys=23, nckeys=5)
	character keyw(nkeys)*8, ckeyw(nckeys)*5
c
	data keyw/   'bmaj    ','bmin    ','bpa     ',
     *    'obstime ','epoch   ','history ',  
     *    'ltype   ','lstart  ','lstep   ','lwidth  ','pbfwhm  ',
     *    'instrume','niters  ','object  ','telescop','pbtype  ',
     *    'restfreq','vobs    ','observer','obsra   ',
     *    'obsdec  ','mostable','cellscal'/
c
c  Keyword values which must be changed as they are passed from in to out.
c
	data ckeyw/'ctype','cdelt','crval','crpix','crota'/
c
c  Copy across unchanged header keywords.
c
	do i = 1,nkeys
	  call hdcopy(lin,lout,keyw(i))
	enddo
c
c  Handle the keywords which must be moved to another axis.
c
	 cin = itoaf(vaxis)
	 cout = itoaf(1)
	 atemp = ckeyw(1)//cin
	 call rdhda(lin,ckeyw(1)//cin,atemp,' ')
	 if(atemp.ne.' ')call wrhda(lout,ckeyw(1)//cout,atemp)
	 do k = 2,nckeys
	   atemp = ckeyw(k)//cin
	   if(hdprsnt(lin,atemp)) then
	      call rdhdd(lin,ckeyw(k)//cin,rtemp,0.0d0)
c
c  Create new reference pixel if output spectrum is shorter than input
c
	      if(k.eq.4) rtemp=rtemp-real(blc(vaxis))+1.0
	      call wrhdd(lout,ckeyw(k)//cout,rtemp)
	    endif
	 enddo
c
c  Special cases: the crpixes will change if the user uses a subcube.
c
c	    if(hdprsnt(lin,'crpix'//cin)) then
c	      call rdhdd(lin,'crpix'//cin,rtemp,0.0d0)
c	      rtemp = rtemp - dble(blc(i)) + 1
c	      call wrhdd(lout,'crpix'//cout,rtemp)
c	    endif
c	  endif
c	enddo
c
c  bunit
c
	unit1=unit0
	if(unit1.eq.'Jy/b') unit1='JY/BEAM'
	if(unit1.eq.'Jy') unit1='JY'
	call wrhda(lout,'bunit',unit1(1:len1(unit1)))
c
c  Write out RA and DEC for the dummy axes.
c
        cin = itoaf(raxis)
	call rdhdd(lin,'crpix'//cin, crpix, 1.0d0)
	call rdhdd(lin,'cdelt'//cin, cdelt, 1.0d0)
	call rdhdd(lin,'crval'//cin, crval, 0.0d0)
	call rdhda(lin,'ctype'//cin, ctype, ' ')
	call wrhdd(lout,'crpix2', 1.d0)
	call wrhdd(lout,'cdelt2', cdelt)
	call wrhdd(lout,'crval2', coord(1))
	call wrhda(lout,'ctype2', ctype)
        cin = itoaf(daxis)
	call rdhdd(lin,'crpix'//cin, crpix, 1.0d0)
	call rdhdd(lin,'cdelt'//cin, cdelt, 1.0d0)
	call rdhdd(lin,'crval'//cin, crval, 0.0d0)
	call rdhda(lin,'ctype'//cin, ctype, ' ')
	call wrhdd(lout,'crpix3', 1.d0)
	call wrhdd(lout,'cdelt3', cdelt)
	call wrhdd(lout,'crval3', coord(2))
	call wrhda(lout,'ctype3', ctype)
	end
