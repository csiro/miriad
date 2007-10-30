c**********************************************************************c
	program ImSpect
	implicit none
c
c  	IMSPECT makes an average spectrum of a specified region of a
c	Miriad image. The average spectrum can be plotted and/or written
c	out as an ascii file for further analysis.
c
c= IMSPECT - Make spectrum from a Miriad image.
c& mchw
c: image analysis and display.
c+
c	IMSPECT makes an spectrum of the velocity or frequency axis of a
c	Miriad image. The spectrum is integrated, or averaged over the
c	region specified for the other image axes. The output spectrum
c	can be plotted and/or written out as an ascii file for further
c	analysis.
c@ in
c	The input image. vxy and xyv images are acceptable inputs. 
c	No default.
c@ region
c	Rectangular region of image to be averaged. E.g.
c	  % imspect  region=relpix,box(-4,-4,5,5)(10,50) for an xyv
c	image, makes a spectrum of image planes 10 to 50 integrated
c	over the center 10 x 10 pixels in the x-y plane. For a vxy
c	image where the x and y size is 128x128, the corresponding
c	region=box(10,61,50,70)(61,70).	Box refers image axes 1 and 2
c	for either vxy or xyv images. The default is the entire image.
c@ xaxis
c	The x-axis can be plotted as 'channel' or the units in the image.
c	The default which is whatever units are in the header.
c@ yaxis
c	If 'average' then the pixels enclosed in the x-y area specified
c	are averaged. If 'sum' they are summed and normalized if the units
c	are known. Default is 'average'
c@ yrange
c	Y-axis range for plot.  Default self-scales.
c@ hann
c	Hanning smoothing length (an odd integer < 15) Default is
c	no smoothing (hann = 1).
c@ options
c	List of minimum match task enrichment options.
c	1deriv  Take 1-sided derivative of spectrum before plotting
c	        and after Hanning smoothing. Useful for Zeeman enthusiasts.
c	2deriv  Take 2-sided derivative of spectrum before plotting.
c	curve   Plot the spectrum joining up the dots instead of the
c	        default step-plot.
c@ device
c	Plot device/type (e.g. /retro, filename/im for hardcopy on a Vax,
c	/sunview, filename/ps for harcopy on a Sun). The default is no plot.
c	To print the hardcopy on the Vax use:
c	  $PGPLOT filename
c	To print the postscript file on the Sun:
c	  %lpr filename
c@ log
c	Write spectrum to this ascii file. Default is no output file.
c@ comment
c	A one-line comment which is written into the logfile.
c--
c
c  History:
c    rl   25Jan89  Original version.
c    nebk  4May89  Change graphics to XYPLOT, add labels to plot
c                  and change DOAVE input to MODE
c         23May89  Added VTYPE option.
c         26May89  Added Hanning smoothing option and PDEV.
c         11Aug89  Added log file containing spectrum.
c          9Sep89  Restore PGPLOT as XYPLOT will not be distributed.
c    rjs  23oct89  Changed 'pdev' to 'device'.
c    mchw 03jul90  Merge xyvspec and vxyspec into ImSpect.
c		   - remove xaxis and averaging out of loop.
c		   - allow plot or ascii output, or both.
c		   - add title to ascii output.
c		   - use standard region of interest.
c		   - add pixel blanking.
c    nebk 11dec90  Fix bug in normalization of averaged spectra
c    mjs  25feb91  Changed references of mitoa to mitoaf, of itoa to
c                  itoaf.
c    mchw 17apr91  Better yaxis units.
c    nebk 18apr91  Change call sequence for new HANNSM, whcih also
c		   fixes the bug that was precluding the smoothed
c		   spectrum from being plotted.
c                  Change input parameter SMLEN to HANN for consistency
c                  with other tasks.
c                  Change variables with fortran intrinsic names
c    pjt  29apr91  Get comments which are more than a word
c    rjs  05aug91  Added 's' flag to boxset.
c    nebk 02sep91  Add YRANGE and OPTIONS=1,2DERIV keywords.
c    nebk 05sep91  Remove call to PGPAGE (PGENV does it).
c    nebk 05sep91  Fix error in working out transformaiton from
c                  channel to x-axis value. Added OPTIONS=CURVE
c    nebk 23sep91  Somehow a version with a half-finished 
c                  OPTIONS=CURVE in it has come back.  Fix properly.
c    mjs  13mar93  pgplot subr names have less than 7 chars.
c    nebk 28mar94  Write frequency axis label into log
c----------------------------------------------------------------------c
	include 'maxdim.h'
	integer maxco,maxnax,maxboxes,maxruns,naxis
	parameter(maxco=15,maxnax=3,maxboxes=2048)
	parameter(maxruns=3*maxdim)
c
	integer boxes(maxboxes),blc(maxnax),trc(maxnax),nsize(maxnax)
        integer npix(maxdim)
	real spec(maxdim),chan(maxdim),value(maxdim),work(maxdim)
	real coeffs(maxco),hwork(maxco),yrange(2)
	double precision restfreq
	real xdmin, xdmax, ydmin, ydmax
	integer lIn,lOut,vaxis,i,nsmth,nchan,iostat,ilen
	integer lblc,ltrc
        logical none, deriv1, deriv2, curve
	character*64 in,logf,xlabel,ylabel,device,xaxis,yaxis
	character title*130, line*72, ans*3, comment*80, str*3, word*80
	character*9 object,date,vctype*9
	character*20 txtblc,txttrc
c
c  Externals.
c
	integer len1, pgbeg
        character itoaf*3
        logical   keyprsnt

        call output( 'Imspect: version 1.0 28-Mar-94' )
c
c  Get inputs
c
	call keyini
	call keyf('in',in,' ')
	call BoxInput('region',in,boxes,maxboxes)
	call keya('xaxis',xaxis,' ')
	call keya('yaxis',yaxis,'average')
        call keyr('yrange',yrange(1),0.0)
        call keyr('yrange',yrange(2),yrange(1))
        if (yrange(1).eq.yrange(2).and.yrange(1).ne.0.0)
     *    call bug ('f', 'Invalid y-axis plot range')
	call keyi('hann',nsmth,1)
        if (nsmth.gt.maxco) then
          str = itoaf(maxco)
          call bug ('f', 'Hanning smoothing length must be <= '//str)
        end if
        call getopt (deriv1, deriv2, curve)
	call keya('device',device,' ')
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
	if(device.eq.' '.and.logf.eq.' ') call bug('f',
     *	  'Neither Plot device nor log file specified for output')
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
c  Set up the region of interest.
c
	call BoxMask(lIn,boxes,maxboxes)
	call BoxSet(boxes,maxnax,nsize,'s')
	call BoxInfo(boxes,maxnax,blc,trc)
	nchan = trc(vaxis)-blc(vaxis)+1
c
c  Integrate the spectrum over the specified region.
c
	if(vaxis.eq.1) then
	  call vaxis1(lIn,naxis,blc,trc,nchan,chan,spec,npix,none)
	else if(vaxis.eq.3) then
	  call vaxis3(lIn,naxis,blc,trc,nchan,chan,spec,npix,none)
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
c  Make title.
c
	call ImHeader(lIn,object,restfreq,date)
	call mitoaf(blc,3,txtblc,lblc)
	call mitoaf(trc,3,txttrc,ltrc)
	write(title,'(a,f10.6,a,a,a,a,i2)')object,restfreq,' GHz ',date,
     *	  ' Blc=('//txtblc(1:lblc)//'),Trc=('//txttrc(1:ltrc)//')',
     *	  ' hann=',nsmth
        if (deriv1) title = title(1:len1(title))//', D1'
        if (deriv2) title = title(1:len1(title))//', D2'
c
c  Get plot axes, convert units, and write labels.
c
	call axes(lIn,vaxis,xaxis,yaxis,nchan,naxis,npix,
     *		  xlabel,ylabel,chan,value,spec)
c
c  Optionally take derivatives
c
        if (deriv1 .or. deriv2) call der (deriv1, nchan, spec, work)
c
c  Open plot device if requested.
c
	if(device.ne.' ') then
	  iostat = pgbeg(0,device,1,1)
	  if(iostat.eq.0) call bug ('f', 'Error opening plot device') 
c 
c  Work out limits
c
	  if(xaxis.eq.'channel') then
	    xdmin = chan(1) - 0.05 * (chan(nchan) - chan(1))
	    xdmax = chan(nchan) + 0.05 * (chan(nchan) - chan(1))
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
  	    ydmax = spec(1)
	    do i = 2, nchan
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
	  call pgqinf ('hardcopy',ans,ilen)
	  if(ans.eq.'YES') then
	    call pgscf(2)
	  else
	    call pgscf(1)
	  end if
	  call pgsch (1.1)
	  call pgsls (1)
	  call pgenv (xdmin, xdmax, ydmin, ydmax, 0, 0)
	  if(xaxis.eq.'channel') then
            if (curve) then
              call pgline (nchan,chan,spec)
            else
  	      call pgHline (nchan,chan,spec,2.)
            endif
	  else
            if (curve) then
              call pgline (nchan,value,spec)
            else
  	      call pgHline (nchan,value,spec,2.)
            endif
	  endif
	  call pglab (xlabel,ylabel,title)
	  call pgend
	endif
c
c  Write ascii spectrum if desired.
c
	if(logf.ne.' ') then
	  call txtopen(lOut, logf, 'new', iostat)
	  if(iostat.ne.0) call bug ('f', 'Error opening output file')
	  call txtwrite(lOut,title,len1(title),iostat)
	  call txtwrite(lOut,'File: '//In,6+len1(In),iostat)
          call txtwrite(lOut,'Spectral axis type = '//vctype,30,iostat)
	  call txtwrite(lOut,comment,len1(comment),iostat)
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
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine vaxis1(lIn,naxis,blc,trc,nchan,chan,spec,npix,none)
c
	implicit none
	integer lIn,naxis,blc(naxis),trc(naxis),nchan,npix(nchan)
	real chan(nchan),spec(nchan)
        logical none
c
c  get integrated spectrum for vaxis=1
c
c  Inputs:
c    lIn	The handle of the image.
c    naxis	Number of image axes.
c    blc,trc	Corners of region of interest.
c    nchan	Number of channels.
c  Output:
c    chan	Array of channel numbers.
c    spec	Integrated spectrum.
c    npix	Number of spatial pixels used for each channel
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
          npix(i) = 0
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
		spec(indx) = spec(indx) + buf(i)
		npix(indx) = npix(indx) + 1
                none = .false.
	      endif
	    enddo
	  enddo
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine vaxis3(lIn,naxis,blc,trc,nchan,chan,spec,npix,none)
c
	implicit none
	integer lIn,naxis,blc(naxis),trc(naxis),nchan,npix(nchan)
	real chan(nchan),spec(nchan)
        logical none
c
c  get integrated spectrum for vaxis=3
c
c  Inputs:
c    lIn	The handle of the image.
c    naxis	Number of image axes.
c    blc,trc	Corners of region of interest.
c    nchan	Number of channels.
c  Output:
c    chan	Array of channel numbers.
c    spec	Integrated spectrum.
c    npix	Number of spatial pixels used for each channel
c    none       True if no good pixels selected
c----------------------------------------------------------------------c
	include 'maxdim.h'
	real buf(maxdim)
	logical flags(maxdim)
	integer i,j,k,indx
c
	do i = 1, nchan
	  spec(i)  = 0.0
   	  npix(i) = 0
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
		spec(indx) = spec(indx) + buf(i)
		npix(indx) = npix(indx) + 1
                none = .false.
	      endif
	    enddo
	  enddo
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine axes(lIn,vaxis,xaxis,yaxis,nchan,naxis,npix,
     *					xlabel,ylabel,chan,value,spec)
	implicit none
	integer lIn,vaxis,naxis,nchan,npix(nchan)
	character*(*) xaxis,yaxis,xlabel,ylabel
	real chan(nchan),spec(nchan),value(nchan)
c
c  Get plot axes and write labels.
c  Inputs:
c    lIn	The handle of the image.
c    vaxis	The velocity or frequency axis.
c    naxis	Number of image axes.
c    xaxis	Units for xaxis. Can be 'channel' or (default) units in image.
c    yaxis	Units for yaxis. Can be 'average' or 'sum'. Default is average.
c    npix	Number of good pixels in integrated spectrum for each channel
c    nchan	Number of channels.
c    chan	Array of channel numbers.
c  Output:
c    value	Array of xaxis values.
c    spec	Spectrum (with converted units).
c    xlabel	Label for xaxis.
c    ylabel	Label for yaxis.
c
c    - Could be much fancier by converting internal units 'JY' 'JY/BEAM'
c    etc. to requested units 'JY' 'JY/BEAM' 'K' etc. calling GetBeam
c    to get beam oversampling factor.
c
c----------------------------------------------------------------------c
	integer i
	real crpix,crval,cdelt
	character caxis*1,ctype*9,bunit*10
	real bmaj,bmin,omega,cbof
c
c  Externals.
c
	character*1 itoaf
	integer len1
c
c  Get coordinates and units from image.
c
	caxis = itoaf(vaxis)
	call rdhda(lIn,'ctype'//caxis,ctype,' ')
	call rdhdr(lIn,'crval'//caxis,crval,0.)
	call rdhdr(lIn,'crpix'//caxis,crpix,0.)
	call rdhdr(lIn,'cdelt'//caxis,cdelt,0.)
c
c  Get xlabel.
c
	if(xaxis.eq.'channel') then
	  xlabel = 'Channel'
	else if(ctype(1:4).eq.'FREQ') then
	  xlabel = 'Frequency (GHz)'
	else if (ctype(1:4).eq.'VELO') then
	  xlabel = 'Velocity (Km/s)'
	else if (ctype(1:4).eq.'FELO') then
	  xlabel = 'Felocity (Km/s)'
	else
	  xlabel = ctype(1:len1(ctype))
	end if
c
c  Convert xaxis units.
c
	do i=1,nchan
	  value(i) = crval + (chan(i)-crpix)*cdelt
	enddo
c
c  Get units and beam oversampling factor from image header.
c
	call GetBeam(lIn,naxis,bunit,bmaj,bmin,omega,cbof)
c
c  Normalize the spectra and get the yaxis.
c
	if(yaxis.eq.'average')then
	  do i=1,nchan
            if (npix(i).gt.0) then
               spec(i) = spec(i)/npix(i)
            else
               call bug ('f', 'Error normalizing integrated spectrum')
            end if
	  enddo
	  ylabel = 'Average Intensity ('//bunit(1:len1(bunit))//')'
	else if(bunit.eq.'JY/PIXEL')then
	  ylabel = 'Total Intensity (JY)'
	else if(bunit.eq.'JY/BEAM'.and.bmaj*bmin*omega.ne.0)then
	  do i=1,nchan
	    spec(i) = spec(i)/cbof
	  enddo
	  ylabel = 'Total Intensity (JY)'
	else
	  ylabel =
     *      'Total Intensity ('//bunit(1:len1(bunit))//' x pixels)'
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
      subroutine getopt (deriv1, deriv2, curve)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     deriv1     True means take 1-sided derivative of spectrum
c     deriv2     True means take 2-sided derivative of spectrum
c     curve      True means plot a curve rather than step-plot
c
c-----------------------------------------------------------------------
      implicit none
c
      logical deriv1, deriv2, curve
cc
      integer maxopt
      parameter (maxopt = 3)
c
      character opshuns(maxopt)*6
      logical present(maxopt)
      data opshuns /'1deriv', '2deriv', 'curve'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      deriv1 = present(1)
      deriv2 = present(2)
      if (deriv1 .and. deriv2) deriv2 = .false.
      curve = present(3)
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
