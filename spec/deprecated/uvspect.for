c------------------------------------------------------------------------
	program uvspect
	implicit none
c
c= UVSPECT - Analyse uvdata spectra and plot amplitude & phase spectra
c& mchw
c: uv analysis, plotting
c+
c	UVSPECT is a Miriad task to analyse and plot visibility spectra.
c	Plot Amplitude and Phase versus channel, frequency or velocity.
c	The default is to average the selected uvdata for all baselines
c	and polarizations. UVSPECT does not apply the gains, or bandpass
c	corrections when plotting the data.
c@ vis
c	The input visibility file. No default.
c@ device
c	PGPLOT device (e.g. /retro, plot.plt/im for hardcopy on a Vax,
c	or  /sunview, plot/ps for harcopy on a Sun).  No default.
c	See documentation on the device keyword for more information.
c@ interval
c	Averaging interval in minutes. Default is no averaging.
c@ select
c	Standard uvdata selection. Default is all visibilities. See
c	documentation on the select keyword for more information.
c@ line
c	Line type of the data in the format
c	  type,nchan,start,width,step
c	Here "type" can be `channel', `wide' or `velocity'.
c	Default is all channels.
c@ fmode
c	Flagging type to plot. Set fmode=a to plot flagged and unflagged
c	data. fmode=u to plot unflagged data, fmode=f to plot flagged data.
c	Default is to plot all data.
c@ fft
c	fft is a character string describing the FFT processing options.
c	Default fft=' ' does no FFT processing. Other options take the
c	complex FFT for each record and each spectral window before
c	averaging the data.
c	   +	The sign of the exponent in the FFT.
c	   -	The sign of the exponent in the FFT.
c	   s	Add a phase gradient of pi per channel before the FFT
c		This produces a half channel shift after the FFT.
c	   q	Quad swap the input to move the center to the 1st point.
c	   h	Hanning smooth the output.
c	Use fft=-qsh to make frequency spectra from HATCREEK correlator
c	lag data with the lsb and usb of LO1 adjacent for each corf.
c	The input uvdata must be the default all spectral channels with
c	the number of channels in each window being a power of 2.
c	Other options to take the FFT of any input line type are possible
c	but not yet implemented.
c@ hann
c       Hanning smoothing length (an odd integer <15). Is applied after
c       averaging over time. Default is no smoothing (hann=1).
c@ xrange
c	Plot range in the x-direction (in channels or km/s according to
c	the linetype selected). Default	is to self-scale.
c@ amprange
c	Plot range for amplitude in the y-direction (in intrinsic units).
c	Default is to self-scale.
c@ phirange
c	Plot range for phase in the y-direction (in degrees).
c	Default is (-180,180) degrees.
c@ mode
c	Mode can be 'inter' or 'batch'. Default is batch. In either mode,
c	the user is prompted for options after each plot on a non-hardcopy
c	device. The options are applied to the current spectrum, which can
c	then be replotted. mode=inter displays the cursor. Type the first
c	character and <cr> to select option. The cursor handling is device
c	dependent, and the character and <cr> may need to given several times.
c	Options:
c	  Device - enter new plot device.
c	  Limits - change plot limits.
c	  Channel - change x-axis to channel number in the line keyword.
c	  Velocity - change x-axis to velocity.
c	  Frequency - change x-axis to frequency.
c	  Replot - replot current spectrum.
c	  Position - display cursor position. (if device supports a cursor)
c	  Write - write out current spectrum to an ascii file.
c	  Quit - stop processing and exit from task.
c	  End - end of options, get next average.
c	  <cr> - go to next time average.
c	The original inputs are restored for the next time average.
c--
c
c  History:
c    31oct89 mchw - Original version.
c     1nov89 rjs  - Fixed the number of channels requested. Fixed a few
c		    portability problems.
c     2nov89 mchw - Improved the logic and added some plotting options.
c     9mar90 rjs  - Change in call sequence to JulDay.
c     2may90 pjt  - maxdim.h now defines MAXCHAN
c     3may90 mchw - Replaced 'maxsels=512'. Corrected 'version'.
c    15may90 mchw - Implemented option to write out ascii spectra.
c    17may90 mchw - Changed default scale for phase to +/-180 degrees.
c    25may90 mchw - Added cursor position, other options, and help.
c    15jun90 mchw - Removed pgqinf test for cursor.
c			Put pgend inside replot loop in Plotit.
c    26jun90 mchw - Trap for char(0) and infinite loop from pgcurse.
c    28jun90 mchw - Changed default mode=batch. Cursor needs work.
c		    Cursor handling is device dependent.
c    27nov90 rjs  - Increased the length of the source variable.
c    25feb91 mjs  - Changed references of atod to atodf.
c    22mar91 mchw - Fixed bug in plot title.
c    20jun91 mchw - Modified plot title, longer filename, keyf input.
c    26jun91 bpw  - Added hanning smoothing
c    08sep91 nebk - Correct use of work array in call to HANNSM
c    25oct91 mchw   Initialize cursor position for cray.
c    17jul92 mchw   Call pgend for each average if hardcopy device.
c    23jul92 mchw   Added frequency and velocity to ascii file output.
c    24jul92 mchw   Frequency plot option. Line width=2 on hardcopy.
c    04aug92 mchw   Corrected line width code for hardcopy.
c    26aug92 rjs    Improved autoscaling, to account for rounding error.
c    13mar93 mjs    pgplot subr names have less than 7 chars.
c    16aug93 mchw   Change averaging to standard keyword "interval".
c    13dec93 mchw   Added FFT option and worked on documentation.
c    16dec93 mchw   Added option to switch x-axis to channel number.
c    10apr94 mchw   Put MAXWIN into maxdim.h
c-----------------------------------------------------------------------
	include 'maxdim.h'
	integer maxsels
	parameter(maxsels=512)
	character vis*80,line*128,date*18,device*30
	character fmode*1,mode*10,fft*10
	integer lvis,nchan,npts,j,nread
	complex data(MAXCHAN),average(MAXCHAN),out(MAXCHAN)
	logical flags(MAXCHAN)
	real Xaxis(MAXCHAN),Amp(MAXCHAN),Arg(MAXCHAN)
	real sels(maxsels),count(MAXCHAN)
	double precision preamble(4),startime
	real start,width,step,interval,avertime,window(6)
	integer hann
	real coeffs(15),work(15)
c
c  Get the parameters given by the user.
c
        call output('Uvspect: version 1.0 10-Apr-94')
	call keyini
	call keyf('vis',vis,' ')
	call keya('line',line,'channel')
	call keyi('line',nchan,0)
	call keyr('line',start,1.)
	call keyr('line',width,1.)
	call keyr('line',step,1.)
	call keyr('interval',interval,0.001)
        call keya('mode',mode,'batch')
        call keya('fmode',fmode,'a')
        call keya('fft',fft,' ')
        call keyi('hann',hann,1)
	call keya('device',device,' ')
	call keyr('xrange',window(1),0.)
	call keyr('xrange',window(2),0.)
	call keyr('amprange',window(3),0.)
	call keyr('amprange',window(4),0.)
	call keyr('phirange',window(5),-180.)
	call keyr('phirange',window(6),180.)
	call SelInput('select',sels,maxsels)
	call keyfin
c
c  Do some checking.
c
	if(vis.eq.' ')call bug('f','Input name must be given')
	if(device.eq.' ')call bug('f','Plot device must be given')
	if(nchan.gt.0) nchan = min(nchan,MAXCHAN)
        if(index('fua',fmode).eq.0)
     *	  call bug ('f','Unrecognized flagging mode')
        if(hann.gt.15)call bug('f','Hanning width too large')
	if(fft.ne.' ')call output('Taking FFT of each spectral window')
c
c  Open the visibility file.
c
	call uvopen(lvis,vis,'old')
	if(nchan.gt.0)
     *	  call uvset(lvis,'data',line,nchan,start,width,step)
	call uvset(lvis,'coord','wavelength',0,0.,0.,0.)
	call SelApply(lvis,sels,.true.)
c
c  Get the first visibility record in average.
c
	call uvread(lvis,preamble,data,flags,MAXCHAN,nread)
	if(nread.le.0) call bug('f','No data found in the input.')
	if(nread.lt.2) call bug('f','Too few channels')
c
c  Start new average
c
	do while(nread.gt.0)
	  startime = preamble(3)
	  avertime = 0.
	  call JulDay(preamble(3),'H',Date)
	  npts = nread
	  do j=1,npts
	    average(j) = cmplx(0.,0.)
	    count(j) = 0
	  enddo
c
c  Process the visibility data.
c
	  do while ((preamble(3)-startime)*24.*60. .lt. interval
     .			.and. nread.eq.npts)
	    if(fft.ne.' ')call specfft(lvis,fft,nread,data,out)
	    do j=1,npts
	      if( fmode.eq.'a' .or. fmode.eq.'u' .and.flags(j) .or.
     .		fmode.eq.'f' .and. .not.flags(j)) then
		average(j) = average(j) + data(j)
		count(j) = count(j) + 1.
	      endif
	    enddo
	    call uvread(lvis,preamble,data,flags,MAXCHAN,nread)
	  enddo
	  avertime = (preamble(3) - startime) * 24.0 * 60.0
c
	  if(nread.ne.npts) then
	    call bug('w','Number of channels changed. New average')
	  endif
c
c  Average the data
c
	  do j=1,npts
	   xaxis(j) = start +(j-1)*step
	   if(count(j).ne.0.) then
	    average(j) = average(j)/count(j)
	    call amphase(average(j),amp(j),arg(j))
	   endif
	  enddo
c Apply hanning smooth
          if(hann.gt.1) then
           call hcoeffs( hann, coeffs )
           call hannsm( hann,coeffs,npts,amp,work )
          endif
c Plot it
	  call plotit(lvis,vis,line,date(1:16),npts,xaxis,amp,arg,
     .			avertime,device,window,mode)
	enddo
	call uvclose(lvis)
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine specfft(lvis,fft,nread,data,out)
	implicit none
	integer lvis,nread
	character*(*) fft
c
c  FFT of each spectral window
c
c  Inputs:
c    lvis	handle of input visibility
c    fft	FFT processing options.
c    nread	Number of input points
c    data	Complex visibility data.
c    out	Workspace for FFT of visibility data.
c----------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer i, j, isn, nchan, nspect
	complex data(MAXCHAN), out(MAXCHAN)
	integer ischan(MAXWIN), nschan(MAXWIN)
	complex expi
c
c  Get the dimensioning info.
c
	call uvgetvri(lvis,'nchan',nchan,1)
	if(nchan.ne.nread)
     *	  call bug('f','Number read not equal to nchan in data')
	call uvgetvri(lvis,'nspect',nspect,1)
	if(nspect.le.0)
     *	  call bug('f','Bad value for uv variable nspect')
	call uvgetvri(lvis,'ischan',ischan,nspect)
	call uvgetvri(lvis,'nschan',nschan,nspect)
c
c  check if nspect is a power of 2.
c  not implemented.
c
	if(index(fft,'+').ne.0)then
	  isn = 1
	else
	  isn = -1
	endif
c
c  FFT of each spectral window
c
	do i=1,nspect
c
c  pre-processing options.
c
	  if(index(fft,'q').ne.0)then
	    do j=ischan(i),ischan(i)+nschan(i)/2-1
	      out(j) = data(j+nschan(i)/2)
	      out(j+nschan(i)/2) = data(j)
	   enddo
	  else
	    do j=ischan(i),ischan(i)+nschan(i)/2-1
	      out(j) = data(j)
	    enddo
	  endif
c
	  if(index(fft,'s').ne.0)then
	    do j=ischan(i),ischan(i)+nschan(i)-1
	      data(j) = out(j) * expi(pi * (j-ischan(i)))
	    enddo
	  else
	    do j=ischan(i),ischan(i)+nschan(i)/2-1
	      data(j) = out(j)
	    enddo
	  endif
c
	  call fftcc(data(ischan(i)),out(ischan(i)),isn,nschan(i))
c
c  post-processing options.
c
	  if(index(fft,'h').ne.0)then
	    do j=ischan(i)+1,ischan(i)+nschan(i)-2
	      data(j) = 0.25*out(j-1) + 0.5*out(j) + 0.25*out(j+1)
	    enddo
	    data(ischan(i)) = out(ischan(i))
	    data(ischan(i)+nschan(i)-1) = out(ischan(i)+nschan(i)-1)
	  else
	    do j=ischan(i),ischan(i)+nschan(i)-1
	      data(j) = out(j)
	    enddo
	  endif
c
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine plotit(lvis,vis,line,date,npts,xaxis, 
     *              amp,arg,avertime,device,window,mode)
c
	implicit none
	character vis*(*),line*(*),device*(*),date*16,mode*(*)
	integer lvis,npts
	real xaxis(npts),amp(npts),arg(npts),avertime,window(6)
c
c  Draw amplitude and phase plot for uvspect
c
c  Inputs:
c    lvis	handle of input visibility
c    vis	input visibility filename
c    line	linetype
c    date	starting date and time 
c    npts	number of points to plot
c    xaxis	array of channels or velocities to plot
c    amp,arg	amplitude and phase arrays
c    avertime	averaging time
c    device	PGPLOT device
c    window	user defined plotting window
c    mode	plotting mode
c----------------------------------------------------------------------
 	real xmin,xmax,ymin,ymax,xlo,xhi,ylo,yhi,plo,phi
	integer ierr,il
	character xlabel*100,ylabel*100,title*100,hard*1,pdev*30
	character source*32
	logical replot
	integer len1,pgbeg,ismin,ismax
c
c  Save initial inputs
c
      pdev = device
c
c  Work out plot window
c
	xmin = xaxis(ismin(npts,xaxis,1))
	xmax = xaxis(ismax(npts,xaxis,1))
	ymin = amp(ismin(npts,amp,1))
	ymax = amp(ismax(npts,amp,1))
	call uvplwin (window(1), window(2), xmin, xmax, xlo, xhi)
	call uvplwin (window(3), window(4), ymin, ymax, ylo, yhi)
	call uvplwin (window(5), window(6), -180., 180., plo, phi)
c
c  Write plot labels
c
	if(line(1:4).eq.'chan') then
	  xlabel = 'channel'
	else
          xlabel = 'velocity (km/s)'
	end if
	write(ylabel,10) plo,phi
10	format('Amplitude and Phase(',f5.0,',',f5.0,')')
	call uvgetvra(lvis,'source',source)
        write (title,20) source(1:len1(source)),date,avertime
20	format(a,3x,a,'   Average:',f9.2,' min')
c
c  Now draw plot
c
	ierr = pgbeg(0,pdev,1,1)
	call pgqinf('HARDCOPY',hard,il)
	if(hard.eq.'YES') then
	  call pgslw(2) 
	else
	  call pgslw(1) 
	endif
	replot = .true.
	do while(replot)
	  if(pdev.ne.device) ierr = pgbeg(0,pdev,1,1)
          if(ierr.eq.1) then
            call pgenv (xlo, xhi, ylo, yhi, 0, 0)
            call pglab (xlabel, ylabel, title)
            call pgHline (npts, xaxis, amp, 2.)
            call pgswin (xlo, xhi, plo, phi)
            call pgpt (npts, xaxis, arg, 1)
            call output (' ')
	  else
	    call bug('f','Can not open plot device')
	  endif
c
c  If interactive prompt for options
c
	  if(hard.ne.'YES') then
	    call options(lvis,title,xlabel,pdev,mode,
     *			xaxis,amp,arg,npts,xlo,ylo,xhi,yhi,replot)
	  else
	    replot = .false.
	    call pgend
	  endif
	  if(pdev.ne.device) call pgend
	end do
	call pgend
	end
c*******************************************************************
	subroutine options(lvis,title,xlabel,pdev,mode,
     *			xaxis,amp,arg,npts,xlo,ylo,xhi,yhi,replot)

	implicit none
	integer lvis,npts
	character title*(*),pdev*(*),xlabel*(*),mode*(*)
	real xaxis(npts),amp(npts),arg(npts),xlo,ylo,xhi,yhi
	logical replot
c
c  Handle plot options for plotit.
c
c  Inputs:
c    lvis	handle of input visibility
c    title	Plot title.
c    pdev	Plot device.
c    mode	Cursor mode.
c    xaxis	array of channels or velocities to plot
c    amp, arg	amplitude and phase arrays
c    npts	number of points to plot
c  In and out:
c    xlo,ylo,xhi,yhi	Plot limits
c    replot		Replot current average.
c    xlabel		xaxis label
c-------------------------------------------------------------------
        include 'maxdim.h'
	logical loop
	character ans*1,text*80,file*80
c	character curse*1
	real xx,yy,xmax,xmin
	integer i,il,lu,iostat,len1,ismin,ismax,nloop
	double precision velocity(MAXCHAN),frequency(MAXCHAN)
c
c  Initialize cursor position.
c
	xx = 0.
	yy = 0.
c
c  Options.
c
	call output ('Options: Help, Device, Limits, Freq, Velocity')
	call output ('Options: Position, Write, Quit, Replot, End')
	call output ('Options: Channel')
	replot = .false.
	loop = .true.
	nloop = 0
	do while(loop.and.nloop.lt.100)
	  nloop = nloop + 1
c  cursor is available on device=/retro but pgplot does not think so !!
c	  call pgqinf('CURSOR',curse,il)
c	  if(curse.eq.'YES'.and.mode.eq.'inter') then
	  if(mode.eq.'inter') then
	    call pgcurs(xx,yy,ans)
	    if(ans.eq.char(0)) loop = .false.
	    call output('Enter option: ')
	  else
	    call prompt (ans, il, 'Enter option: ')
	  endif
	  call ucase(ans)
	  if(ans.eq.'D') then
	    call prompt(pdev,il,'Enter plot device: ')
	  else if(ans.eq.'L') then
	    call getwin (xlo,ylo,xhi,yhi)
 	  else if(ans.eq.'P') then
c	    if(curse.eq.'YES'.and.mode.eq.'inter') then
	    if(mode.eq.'inter') then
	      write(text,'(''x= '',1pg13.6,'' y= '',1pg13.6)') xx,yy
	      call output(text(1:32))
	    else
	      call output('No cursor available on this device')
	    endif
	  else if(ans.eq.'C') then
	    call output('Change x-axis to channel. Type R to replot')
	    xlabel = 'channel'
 	    do i=1,npts
	      xaxis(i) = i
	    enddo
	    xmin = 1.
	    xmax = npts
	    call uvplwin (0.,0.,xmin,xmax,xlo,xhi)
	  else if(ans.eq.'F') then
	    call output('Change x-axis to frequency. Type R to replot')
	    call uvinfo(lvis,'frequency',frequency)
	    xlabel = 'frequency (GHz)'
 	    do i=1,npts
	      xaxis(i) = frequency(i)
	    enddo
	    xmin = xaxis(ismin(npts,xaxis,1))
	    xmax = xaxis(ismax(npts,xaxis,1))
	    call uvplwin (0.,0.,xmin,xmax,xlo,xhi)
	  else if(ans.eq.'V') then
	    call output('Change x-axis to velocity. Type R to replot')
	    call uvinfo(lvis,'velocity', velocity)
	    xlabel = 'velocity (km/s)'
 	    do i=1,npts
	      xaxis(i) = velocity(i)
	    enddo
	    xmin = xaxis(ismin(npts,xaxis,1))
	    xmax = xaxis(ismax(npts,xaxis,1))
	    call uvplwin (0.,0.,xmin,xmax,xlo,xhi)
	  else if(ans.eq.'W') then
	    call prompt(file,il,'Enter output file name: ')
	    if(il.ne.0)then
	      call TxtOpen(lu,file(1:il),'new',iostat)
	      call TxtWrite(lu,title,len1(title),iostat)
	      write(text,'(a,a,a,a,a)')'  channel   ',' frequency ',
     *			'  velocity  ','  amplitude ','   phase    '
	      call TxtWrite(lu,text,len1(text),iostat)
	      call uvinfo(lvis,'frequency',frequency)
	      call uvinfo(lvis,'velocity', velocity)
	      i=1
	      do while(i.le.npts.and.iostat.eq.0)
		write(text,'(g12.5,2x,f10.6,2x,3(g12.5,2x))')
     *		  xaxis(i),frequency(i),velocity(i),amp(i),arg(i)
		call TxtWrite(lu,text,68,iostat)
		i=i+1
	      enddo
	      call TxtClose(lu)
	    endif
	  else if(ans.eq.'H') then
	    call help
c
c  Here are the exits from the options loop.
c	Replot current spectrum, Quit, Get next average.
c
	  else if(ans.eq.'R') then
	    loop = .false.
	    replot = .true.
 	  else if(ans.eq.'Q') then
	    call output('OK - I Quit')
	    stop
 	  else
            loop = .false.
	    replot = .false.
	  end if
	enddo
	end
c****************************************************************
	subroutine help
	call output('Type 1st character to select option:')
	call output('Device - enter new plot device')
	call output('Limits - change plot limits')
	call output('Velocity - change x-axis to velocity')
	call output('Frequency - change x-axis to frequency')
	call output('Replot - replot current spectrum')
	call output('Position - display cursor position')
	call output('Write - write out current spectrum')
	call output('Quit - stop processing and exit from task')
	call output('End - end of options, get next average')
	end
c****************************************************************
      subroutine uvplwin (umin, umax, pmin, pmax, lo, hi)
      implicit none
      real umin, umax, pmin, pmax, lo, hi
c
c  Work out UV-plot range for one axis 
c
c     Input:
c       umin,max    User specified minimum and maximum
c       pmin,max    Min and max fitting data found by program
c     Output:
c       lo,hi       Actual window used
c
c--------------------------------------------------------------------
      real delta,absmax
c
c  Auto-scale, adding a bit of extra space. Also make sure we are not
c  fooling around in rounding error (which PGPLOT does not appreciate).
c
      if (umin.eq.0.0 .and. umax.eq.0.0) then
	delta = 0.05*(pmax-pmin)
	absmax = max(abs(pmax),abs(pmin))
	if(delta.le.1e-4*absmax) delta = 0.01*absmax
	if(delta.eq.0) delta = 1
        lo = pmin - delta
        hi = pmax + delta
c
c  Use user specificed limits.
c
      else
        lo = umin 
        hi = umax 
      end if
      end
c********************************************************************
      subroutine getwin (xlo, ylo, xhi, yhi)
      implicit none
      real xlo, xhi, ylo, yhi
c
c  Prompt user for new plot window
c
c  Input:
c    xlo,ylo,xhi,yhi	previous plot window.
c  Output:
c    xlo,ylo,xhi,yhi	new plot window
c
c------------------------------------------------------------------
      real win(4)
      character aline*132, str*100
      logical loop, ok
      integer il
c
      loop = .true.
      do while (loop) 
        call output (' ')
        write (aline, 10) xlo, xhi, ylo, yhi
10      format ('Current window (x1 x2 y1 y2) is: ', 1pe12.4, ' ', 
     *         1pg12.4, 1pg12.4, ',', 1pg12.4)
        call output (aline)
c
        call output (' ')
        call prompt (str, il, 'Enter new window (x1 x2 y1 y2) : ')
        call windec (str, il, win, ok)
c
        if (win(1).eq.win(2) .or. win(3).eq.win(4) .or. .not.ok) then
          call output (' ')
          call output ('Bad window, try again')
        else
          xlo = win(1)
          xhi = win(2)
          ylo = win(3)
          yhi = win(4)
          loop = .false.
       end if
      end do
      end
c*******************************************************************
      subroutine windec (aline, ilen, win, ok)
c
c     Decode string into window 
c
c     Input
c       aline    Input string
c       ilen     Length of string with trailing blanks ignored
c     Output
c       win      Window (x1, x2, y1, y2)
c                
c---------------------------------------------------------------------
      implicit none
c
      integer ilen
      real win(4)
      character*(*) aline
      logical ok
      double precision val
      integer ist, iend, j
c--------------------------------------------------------------------
      if (ilen.gt.0) then
        ist = 1
        j = 1
        ok = .true.
        do while (j.le.4 .and. ok)
          do while (aline(ist:ist).eq.' ' .and. ist.le.ilen)
            ist = ist + 1
          end do
          iend = index(aline(ist:),' ')
          if (iend.eq.0) then
            iend = ilen
          else
            iend = iend + (ist - 1) - 1
          end if
c 
          if (ist.gt.iend) then
            ok = .false.
          else
            call atodf (aline(ist:iend), val, ok) 
            win(j) = val
          end if
c
          ist = iend + 1
          j = j + 1
        end do
      else
        ok = .false.
      end if
c
      end
