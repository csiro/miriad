c************************************************************************
	program varmerge
c
	implicit none
c
c= varmerge - Merge in a new visibility variable.
c& rjs
c: uv analysis
c+
c@ vis
c       The input visibility data set
c@ out
c       The output visibility data set, with the new variable added
c@ var
c	The name of the variable to be merged in.
c@ data
c	A text file containing the variable. It is in the form
c	of a time stamp followed by the variable.
c--
c  History:
c  07jun05 rjs	Original version
c  29mar12 vjm	Support longer filenames, consistent with var*.for
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	parameter(version='Varmerge: version 1.0 07-Jun-05')
c
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	integer lVis,lOut,pol,npol,nchan
        character daz*132,out*132,vis*132,var*16
	double precision preamble(5),ptime
	real val
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keya('out',out,' ')
	call keya('var',var,' ')
	call keya('data',daz,' ')
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
	if(var.eq.' ')call bug('f','A variable name must be given')
	if(daz.eq.' ')call bug('f','An data text file must be given')
c
c  Get ready to copy the data.
c
	call metInit(daz)
	call uvopen(lVis,vis,'old')
	call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varInit(lVis,'channel')
c
	call uvopen(lOut,out,'new')
	call varOnit(lVis,lOut,'channel')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
c  Make the output history.
c
	call hdcopy(lVis,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'VARMERGE: Miriad '//version)
	call hisinput(lOut,'VARMERGE')
	call hisclose(lOut)
c
c  Copy across any calibration tables.
c
	call calcopy(lVis,lOut)
c
	call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	ptime = preamble(4) - 1
	dowhile(nchan.gt.0)
	  call uvrdvri(lVis,'pol',pol,0)
	  call uvrdvri(lVis,'npol',npol,0)
	  call varCopy(lVis,lOut)
c
	  if(abs(ptime-preamble(4)).gt.5.d0/86400.d0)then
	    ptime = preamble(4)
	    call metGet(ptime,val)
	    call uvputvrr(lOut,var,val,1)
	  endif
c
	  if(npol.gt.0)then
	    call uvputvri(lOut,'npol',npol,1)
	    call uvputvri(lOut,'pol',pol,1)
	  endif
c
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call metFin
	call uvclose(lVis)
	call uvclose(lOut)
	end
c************************************************************************
	subroutine metInit(mdata)
c
	implicit none
	character mdata*(*)
c------------------------------------------------------------------------
	real daz(2)
	double precision time(2)
	common/metcom/time,daz
c
	call tinOpen(mdata,'n')
	call metRec(time(1),daz(1))
	call metRec(time(2),daz(2))
	end
c************************************************************************
	subroutine metRec(time,daz)
c
	implicit none
	real daz
	double precision time
c------------------------------------------------------------------------
c
c  Externals.
c
	integer tinNext
c
	if(tinNext().le.0)call bug('f','Error getting met data')
	call tinGett(time,0.d0,'atime')
	call tinGetr(daz,0.0)
	end
c************************************************************************
	subroutine metGet(time0,daz0)
c
	double precision time0
	real daz0
c------------------------------------------------------------------------
	integer i
c
	real daz(2)
	double precision time(2)
	common/metcom/time,daz
c
c  Point to the earlier time.
c
	i = 1
	if(time(1).gt.time(2))i = 2
c
c  Step through until we straddle two sets of measurements.
c
	dowhile(time0.gt.time(3-i))
	  call metRec(time(i),daz(i))
	  i = 3 - i
	enddo
c
c  Return the measurements closest to the requested time.
c
	i = 1
	if(abs(time0-time(1)).gt.abs(time0-time(2)))i = 2
	daz0 = daz(i)
c
	end
c************************************************************************
	subroutine metFin
	call tinClose
	end
c************************************************************************
	subroutine CalCopy(lIn,lOut)
c
	implicit none
	integer lIn,lOut
c
c  Copy across calibration tables.
c
c------------------------------------------------------------------------
	integer j
c
	integer NTABLE
	parameter(NTABLE=13)
	character tables(NTABLE)*8
	data tables/'interval','nsols   ','ngains  ','nfeeds  ',
     *	 'ntau    ','gains   ','freq0   ','leakage ','bandpass',
     *	 'freqs   ','nspect0 ','nchan0  ','senmodel'/
c
	do j=1,NTABLE
	  call hdcopy(lIn,lOut,tables(j))
	enddo
c
	end
