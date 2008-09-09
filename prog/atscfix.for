c************************************************************************
	program atscfix
	implicit none
c
c= ATSCFIX - Correct for inadvertent on-line selfcal application
c& rjs
c: uv analysis
c+
c	ATSCFIX corrects data that has been affected by on-line selfcal
c       which was switched on by mistake
c@ vis
c	The input visibility file to correct. No default.
c@ out
c	The output corrected file. No default.
c@ scphase
c	A text file containing the selfcal phase corrections.
c       Format : time (yymmmdd:hh:mm:ss), phase (6x).
c       Each time should appear twice, first one is XX, second one YY.
c	No default.
c	  .
c@ offset
c	Offset, in seconds, between the recorded values and the data
c--
c  History:
c    03jul04 rjs  Original version.
c    25jul04 rjs  Adjust tolerance to determine glitches.
c    09sep08 mhw  Modify atrtfix to become atscfix
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='AtScFix: version 1.0 09-Sep-08')
	include 'maxdim.h'
	include 'mirconst.h'
c
	character vis*64,out*64,scphase*64
	real phase,theta
	complex w
	integer lVis,lOut,nchan,i,pol,npol
	double precision preamble(5)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	real offset
c
c  Externals.
c
	real phget
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keya('out',out,' ')
	call keya('scphase',scphase,' ')
	call keyr('offset',offset,0.0)
	call keyfin
c
c  Open the input and output, and do various housekeeping.
c
        call uvopen(lVis,vis,'old')
        call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
        call varInit(lVis,'channel')
c
        call uvopen(lOut,out,'new') 
        call varOnit(lVis,lOut,'channel')
        call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
        call hdcopy(lVis,lOut,'history')
        call hisopen(lOut,'append')  
        call hiswrite(lOut,'ATSCFIX: Miriad '//version)
        call hisinput(lOut,'ATSCFIX')   
        call hisclose(lOut)
c
c  Get first record
c
        call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
        if(nchan.eq.0)call bug('f','No data found')
c
c  Open the phase file and skip to start of data
c
	call Phopen(scphase,preamble(4)-offset/86400.d0)
c
c  Loop through the data
c               
	dowhile(nchan.gt.0)
          call uvrdvri(lVis,'pol',pol,0)
          call uvrdvri(lVis,'npol',npol,0)
	  phase = Phget(preamble(4)-offset/86400.d0,preamble(5),pol)
c
	  do i=1,nchan
	    theta = PI/180*phase
	    w = cmplx(cos(theta),sin(theta))
	    data(i) = w*data(i)
	  enddo
c
c  Copy to the output.
c
          call varCopy(lVis,lOut)
          if(npol.gt.0)then
            call uvputvri(lOut,'npol',npol,1)
            call uvputvri(lOut,'pol',pol,1)
          endif
          call uvwrite(lOut,preamble,data,flags,nchan)
          call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
        enddo
c
c  All said and done.
c
	call uvclose(lOut)
	call uvclose(lVis)
	call Phclose()
c
	end
c************************************************************************
	subroutine PhOpen(file,t)
c
	implicit none
	character file*(*)
        double precision t
c------------------------------------------------------------------------
	integer i
c
	double precision time
	real phases(6,2),cphases(6,2)
	common/phcomm/time,phases,cphases
c
	call tinOpen(file,'n')
	call phrec(time,phases(1,1))
	call phrec(time,phases(1,2))
	dowhile(t.gt.time)
	  call phrec(time,phases(1,1))
	  call phrec(time,phases(1,2))
	enddo
	do i=1,6
	  cphases(i,1) = 0
	  cphases(i,2) = 0
	enddo
c
	end
c***********************************************************************
	real function PhGet(t,bl,pol)
c
	implicit none
        integer pol
	double precision t,bl
c------------------------------------------------------------------------
	integer i1,i2
c
	double precision time
	real phases(6,2),cphases(6,2)
	common/phcomm/time,phases,cphases
c

	dowhile(t.gt.time)
	  call phcumul(phases,cphases)
	  call phrec(time,phases(1,1))
	  call phrec(time,phases(1,2))
	enddo
c
	phget=0
        call basant(bl,i1,i2)
        if (pol.eq.-5) then
c xx
	  phget = cphases(i1,1) - cphases(i2,1)
        else if (pol.eq.-6) then
c yy
	  phget = cphases(i1,2) - cphases(i2,2)
        else if (pol.eq.-7) then
c xy
	  phget = cphases(i1,1) - cphases(i2,2)
        else if (pol.eq.-8) then
c yx
	  phget = cphases(i1,2) - cphases(i2,1)
        endif
	end
c************************************************************************
	subroutine phcumul(phase,cphase)
c
	implicit none
	real phase(6,2),cphase(6,2)
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,6
	    cphase(i,1) = cphase(i,1) + phase(i,1)
	    cphase(i,2) = cphase(i,2) + phase(i,2)
	enddo
	end
c************************************************************************
	subroutine PhRec(time,phases)
c
	implicit none
	double precision time
	real phases(6)
c------------------------------------------------------------------------
	integer i
c
	integer tinNext
c
	if (tinNext().gt.0) then
	  call tinGett(time,0.d0,'atime')
	  do i=1,6
	    call tinGetr(phases(i),0.)
	  enddo
        else
c
c Set phases to zero if we run out of selfcal phases
c
          do i=1,6
            phases(i)=0
          enddo
          time=time+1
        endif
	end
c************************************************************************
	subroutine PhClose
c
	implicit none
c------------------------------------------------------------------------
	call tinClose
	end
