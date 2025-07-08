c************************************************************************
	program gpshift
	implicit none
c
c= GpShift -- Shift the frequency of bandpass solutions.
c& rjs
c: calibration
c+
c@ vis
c	The name of the input data-set. This will normally be a visibility
c	data-set. No default.
c@ offset
c	Frequency offset to add, in GHz. Default is 0.
c--
c  History:
c  Bugs:
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='GpShift: version 1.0 07-Sep-99')
	character vis*80,line*80
	integer tIn,nschan,nspect,iostat,item,off
	double precision offset,freqs(2)
c
c  Get the user parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input data-set must be given')
	call keyd('offset',offset,0.d0)
	call keyfin
c
c  Open up all the inputs.
c
	call hopen(tIn,vis,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening input '//vis)
	  call bugno('f',iostat)
	endif
	call rdhdi(tIn,'nspect0',nspect,0)
	if(nspect.ne.1)call bug('f','Bad number of windows')
	call haccess(tIn,item,'freqs','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error accessing the bandpass frequency table')
	  call bugno('f',iostat)
	endif
c
	off = 8
	call hreadi(item,nschan,off,4,iostat)
	off = off + 8
	if(iostat.eq.0)call hreadd(item,freqs,off,2*8,iostat)
	write(line,'(a,f10.5,a)')'Initial frequency before shift: ',
     *	  freqs(1),' GHz'
	call output(line)
	freqs(1) = freqs(1) + offset
	write(line,'(a,f10.5,a)')'Initial frequency after  shift: ',
     *	  freqs(1),' GHz'
	call output(line)
	if(iostat.eq.0)call hwrited(item,freqs,off,2*8,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error reading/writing frequency table')
	  call bugno('f',iostat)
	endif
	call hdaccess(item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing frequency table')
	  call bugno('f',iostat)
	endif
c
	call hclose(tIn)
	end
