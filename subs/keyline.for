c************************************************************************
c  Get line and reference line parameters
c
c  History:
c    rjs  ??      Original version.
c    rjs  16nov94 Fix reference line retrieval.
c************************************************************************
c* KeyLine -- Get the linetype from the user interface.
c& rjs
c: user-input
c+
	subroutine KeyLine(line,nchan,lstart,lwidth,lstep)
c
	implicit none
	character line*(*)
	integer nchan
	real lstart,lwidth,lstep
c
c  Get the linetype from the user interface.
c
c  Output:
c    line	Either 'channel', 'velocity','felocity' or 'wide'.
c    nchan	Number of channels.
c    lstart,lwidth,lstep  Line parameters.
c--
c------------------------------------------------------------------------
	integer NTYPES
	parameter(NTYPES=4)
	character ltypes(NTYPES)*8
	integer nout
	data ltypes/'channel  ','velocity','felocity','wide    '/
c
	call keymatch('line',NTYPES,ltypes,1,line,nout)
	if(nout.eq.0)line = ' '
	call keyi('line',nchan,0)
	if(nchan.lt.0)call bug('f',
     *	  'Invalid number of chans in line parameter')
c
	if(line.eq.'felocity'.or.line.eq.'velocity')then
	  call keyr('line',lstart,0.)
	  call keyr('line',lwidth,0.)
	  call keyr('line',lstep,lwidth)
	else
	  call keyr('line',lstart,1.)
	  call keyr('line',lwidth,1.)
	  call keyr('line',lstep,lwidth)
	endif
c
	end
c************************************************************************
c* KeyrLine -- Get the reference linetype from the user interface.
c& rjs
c: user-input
c+
	subroutine KeyrLine(line,lstart,lwidth)
c
	implicit none
	character line*(*)
	real lstart,lwidth
c
c  Get the reference linetype from the user interface.
c
c  Output:
c    line	Either 'channel', 'velocity','felocity' or 'wide'.
c    lstart,lwidth  Line parameters.
c--
c------------------------------------------------------------------------
	integer NTYPES
	parameter(NTYPES=4)
	character ltypes(NTYPES)*8
	integer nout
	data ltypes/'channel  ','velocity','felocity','wide    '/
c
	call keymatch('ref',NTYPES,ltypes,1,line,nout)
	if(nout.eq.0)line = ' '
c
	if(line.eq.'felocity'.or.line.eq.'velocity')then
	  call keyr('ref',lstart,0.)
	  call keyr('ref',lwidth,0.)
	else
	  call keyr('ref',lstart,1.)
	  call keyr('ref',lwidth,1.)
	endif
c
	end
