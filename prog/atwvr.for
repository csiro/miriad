c************************************************************************
	program atwvr
	implicit none
c
c= ATWVR - Correct phases using WVR data
c& rjs
c: uv analysis
c+
c	ATWVR applies phase corrections derived from WVR measurements
c@ vis
c	The input visibility file to correct. No default.
c@ out
c	The output corrected file. No default.
c@ wvrphase
c	A text file containing the phase corrections.
c       Format : time (yymmmdd:hh:mm:ss), phase (6x).
c	Prepending a '-' to the file name will negate the phases.
c@ offset
c	Offset of phase correction time stamps vs data time stamps.
c@ uvrange
c       Range of uv distance to apply the corrections over in meters(!).
c       Defaults to all data. First value defaults to zero,
c       second value defaults to infinity.
c@ log
c       Name of log file. If specified it will list the phases
c       applied to each baseline
c--
c  History:
c    03jul04 rjs  Original version.
c    25jul04 rjs  Adjust tolerance to determine glitches.
c    09sep08 mhw  Modify atrtfix to become atscfix
c    23mar11 mhw  Modify atscfix to become atwvr
c    22jun11 mhw  Add pol code back in
c    21sep12 mhw  Add log file
c    09oct12 mhw  Use closest correction slot instead of preceeding slot
c    20dec12 mhw  Add uvrange parameter
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='AtWVR: version 1.0 21-Sep-2012')
	include 'maxdim.h'
	include 'mirconst.h'
c
	character vis*128,out*64,wvrphase*128,log*128,line*80
	real phase,theta, visph, uvrange(2), uvdist
	complex w,v
	integer lVis,lOut,nchan,i,pol,pol0,npol,sign,i1,i2
	double precision preamble(5)
	complex data(MAXCHAN)
	logical flags(MAXCHAN),dolog, apply
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
	call keya('wvrphase',wvrphase,' ')
        sign=1
        if (wvrphase(1:1).eq.'-') then 
          sign=-1
          wvrphase = wvrphase(2:)
        endif
	call keyr('offset',offset,0.0)
        offset = offset/86400.d0
        call keyr('uvrange',uvrange(1),0.0)
        call keyr('uvrange',uvrange(2),1e20)
        call keya('log',log,' ')
        dolog = log.ne.' '
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
        call hiswrite(lOut,'ATWVR: Miriad '//version)
        call hisinput(lOut,'ATWVR')   
        call hisclose(lOut)
c        
        if (dolog) call LogOpen(log,' ')       
c
c  Get first record
c
        call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
        if(nchan.eq.0)call bug('f','No data found')
c
c  Open the phase file and skip to start of data
c
	call Phopen(wvrphase,preamble(4)+offset)
c
c  Loop through the data
c               
        call uvrdvri(lVis,'pol',pol0,0)
	dowhile(nchan.gt.0)
          call uvrdvri(lVis,'pol',pol,0)
          call uvrdvri(lVis,'npol',npol,0)
          
          uvdist = sqrt(preamble(1)**2+preamble(2)**2)*0.3
          apply = uvdist.gt.uvrange(1).and.uvdist.lt.uvrange(2)

	  phase = Phget(preamble(4)+offset,preamble(5))
c
	  theta = sign*PI/180*phase
	  w = cmplx(cos(theta),sin(theta))
          v = 0
	  do i=1,nchan
             if (flags(i)) v = v + data(i)
	     if (apply) data(i) = w*data(i)
	  enddo
          if (abs(v).gt.0) visph =atan2(imag(v),real(v))*180/PI
          call basant(preamble(5),i1,i2)
          if (dolog.and.pol.eq.pol0) then
            call julday(preamble(4),'H',line)
            write(line(20:),'(I3,''-'',I3,1x,3F8.1)') 
     *       i1,i2,sign*phase,visph,visph+sign*phase
            call logwrit(line)
          endif
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
        if (dolog) call LogClose
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
	double precision time, ctime
	real phases(6),cphases(6)
	common/phcomm/time,ctime,phases,cphases
c
	call tinOpen(file,'n')
        do i=1,6
          cphases(i)=0
        enddo
	call phrec(time,phases(1))
        ctime=time-10./86400
	dowhile(t.gt.time)
          do i=1,6
            cphases(i)=phases(i)
          enddo
          ctime = time
	  call phrec(time,phases(1))
	enddo
        if (abs(t-time).lt.abs(t-ctime)) then
          ctime=time
          do i=1,6
            cphases(i)=phases(i)
          enddo
        endif
        
c
	end
c***********************************************************************
	real function PhGet(t,bl)
c
	implicit none
	double precision t,bl
c------------------------------------------------------------------------
	integer i1,i2,i
c
	double precision time, ctime
	real phases(6),cphases(6)
c        character*80 line
	common/phcomm/time,ctime,phases,cphases
c

	dowhile(t.gt.time)
          do i=1,6
            cphases(i)=phases(i)
          enddo
          ctime = time
	  call phrec(time,phases(1))
	enddo
        if (abs(t-time).lt.abs(t-ctime)) then
          ctime=time
          do i=1,6
            cphases(i)=phases(i)
          enddo
        endif
c
	phget=0
        call basant(bl,i1,i2)
c        call julday(ctime,'H',line)
c        call julday(t,'H',line(20:))
c        write(line(40:),'(2I4,1x,2F8.1)') i1,i2,cphases(i1),cphases(i2)
c        call logwrit('DEBUG '//line)
	phget = cphases(i1) - cphases(i2)
        
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
c Set phases to zero if we run out of wvr phases
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
