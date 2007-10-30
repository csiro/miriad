c************************************************************************
	program atrtfit
	implicit none
c
c= ATRTFIX - Correct for misbehaving round trip system
c& rjs
c: uv analysis
c+
c	ATRTFIX corrects data for errors in the round trip phase measurement.
c	This is only needed when the round trip measurement malfunctions.
c@ vis
c	The input visibility file to correct. No default.
c@ out
c	The output corrected file. No default.
c@ rtphase
c	A text file containing the round trip phase measurements.
c	No default.
c@ options
c	Extra processing options.
c	  glitch  Only modify the phase to account for glitches (not
c	          the gradual variation).
c@ offset
c	Offset, in seconds, between the recorded values and the data
c--
c  History:
c    03jul04 rjs  Original version.
c    25jul04 rjs  Adjust tolerance to determine glitches.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='AtRpFix: version 1.0 25-Jul-04')
	include 'maxdim.h'
	include 'mirconst.h'
c
	character vis*64,out*64,rtphase*64
	real phase,theta
	complex w
	logical glitch
	integer lVis,lOut,nchan,i,pol,npol
	double precision preamble(5)
	complex data(MAXCHAN)
	double precision freq(MAXCHAN)
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
	call keya('rtphase',rtphase,' ')
	call getopt(glitch)
	call keyr('offset',offset,0.0)
	call keyfin
c
c  Open the phase file.
c
	call Phopen(rtphase,glitch)
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
        call hiswrite(lOut,'ATRTFIX: Miriad '//version)
        call hisinput(lOut,'ATRFFIX')   
        call hisclose(lOut)
c
c  Get first record and then loop.
c
        call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
        if(nchan.eq.0)call bug('f','No data found')
	dowhile(nchan.gt.0)
	  phase = Phget(preamble(4)-offset/86400.d0,preamble(5))
	  call uvinfo(lVis,'sfreq',freq)
c
	  do i=1,nchan
	    theta = PI/180*phase*freq(i)
	    w = cmplx(cos(theta),sin(theta))
	    data(i) = w*data(i)
	  enddo
c
c  Copy to the output.
c
          call varCopy(lVis,lOut)
          call uvrdvri(lVis,'pol',pol,0)
          call uvrdvri(lVis,'npol',npol,0)
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
	subroutine PhOpen(file,glitch1)
c
	implicit none
	character file*(*)
	logical glitch1
c------------------------------------------------------------------------
	integer i
c
	double precision time(2)
	real phases(6,2),gphases(6,2)
	logical glitch
	common/phcomm/time,phases,gphases,glitch
c
	glitch = glitch1
	call tinOpen(file,'n')
	call phrec(time(1),phases(1,1))
	call phrec(time(2),phases(1,2))
	do i=1,6
	  gphases(i,1) = 0
	enddo
	call phglitch(phases(1,1),gphases(1,1),
     *		      phases(1,2),gphases(1,2))
c
	end
c***********************************************************************
	real function PhGet(t,bl)
c
	implicit none
	double precision t,bl
c------------------------------------------------------------------------
	integer i,i1,i2
c
	double precision time(2)
	real phases(6,2),gphases(6,2)
	logical glitch
	common/phcomm/time,phases,gphases,glitch
c
	i = 1
	if(time(1).gt.time(2))i=2
c
	dowhile(t.gt.time(3-i))
	  call phrec(time(i),phases(1,i))
	  call phglitch(phases(1,3-i),gphases(1,3-i),
     *		        phases(1,i),gphases(1,i))
	  i = 3 - i
	enddo
c
	i = 1
c        if(abs(t-time(1)).gt.abs(t-time(2)))i = 2
        if(time(2).lt.time(1))i = 2
	call basant(bl,i1,i2)
	if(glitch)then
	  phget = gphases(i1,i) - gphases(i2,i)
	else
	  phget = phases(i1,i) - phases(i2,i)
	endif
	end
c************************************************************************
	subroutine phglitch(phase1,gphase1,phase2,gphase2)
c
	implicit none
	real phase1(6),gphase1(6),phase2(6),gphase2(6)
c
c------------------------------------------------------------------------
	real THRESH
	parameter(THRESH=0.5)
	integer i
c
	do i=1,6
	  if(abs(phase1(i)-phase2(i)).gt.THRESH)then
	    gphase2(i) = gphase1(i) - phase1(i) + phase2(i)
	  else
	    gphase2(i) = gphase1(i)
	  endif
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
	if(tinNext().le.0)call bug('f','Error getting RTP data')
	call tinGett(time,0.d0,'atime')
	do i=1,6
	  call tinGetr(phases(i),0.)
	enddo
	end
c************************************************************************
	subroutine PhClose
c
	implicit none
c------------------------------------------------------------------------
	call tinClose
	end
c************************************************************************
	subroutine getopt(glitch)
c
	implicit none
	logical glitch
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	character opts(NOPTS)*8
	logical present(NOPTS)
c
	data opts/'glitch  '/
c
	call options('options',opts,present,NOPTS)
	glitch = present(1)
	end
