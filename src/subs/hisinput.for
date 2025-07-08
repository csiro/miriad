c***********************************************************************
c  Copy the keywords (input parameters) to the history file.
c
c  History:
c    jm    ??jun90    Inherited.
c    jm    01feb91    Added comments to inline doc.  The history file
c                     MUST be opened via HisOpen for this to work.
c    rjs   16may91    Reads inputs from -f files.
c    jm    13sep91    Increased size of character strings to allow for
c                     very long keyword inputs (eg. select=...).
c    jm    08oct91    Added date of program execution to output.
c    rjs   18aug93    More tolerant of funny input.
c    mhw   15jun12    Adopt T format time (from pjt)
c***********************************************************************
c* HisInput -- Copy task input parameters to a history file.
c& jm
c: history,user-input
c+
	subroutine HisInput(tno,name)
c
	implicit none
	integer tno
	character name*(*)
c
c  This copies the task parameters on the command line and writes them
c  to the history file.
c
c  NOTE:  The history file must be currently open (via HisOpen) for 
c  this routine to work properly.
c
c  Inputs:
c    tno	Handle of the history file.
c    name	Program name. History comments are prepended with this
c		name.
c--
c-----------------------------------------------------------------------
	integer narg,i,l1,l2,length,lu,iostat
	logical dofile
	character line*1024,file*1024
	double precision julian
c
c  Externals.
c
	integer iargc,len1
c
	narg = iargc()
	l2 = len(line)
	l1 = min(len1(name),l2-5)
	dowhile(name(l1:l1).eq.':')
	  l1 = l1 - 1
	enddo
	if(l1.le.0) call bug('f','HisInput: Bad program name.')
	call TodayJul(julian)
	call JulDay(julian, 'T', file)
	line = name(1:l1)//': Executed on: '//file(1:len1(file))
	call ucase(line(1:l1))
	call hiswrite(tno,line)
	line(l1+1:) = ': Command line inputs follow:'
	call hiswrite(tno,line)
c
	line(l1+1:l1+4) = ':'
	l1 = l1 + 5
c
	dofile = .false.
	do i=1,narg
	  if(dofile)then
	    call getarg(i,file)
	    call txtopen(lu,file,'old',iostat)
	    if(iostat.ne.0)then
	      call bug('w','Error opening input parameter file')
	      call bugno('w',iostat)
	    else
	      call txtread(lu,line(l1:l2),length,iostat)
	      dowhile(iostat.eq.0)
		length = min(l2,length + l1 - 1)
		call hiswrite(tno,line(1:length))
		call txtread(lu,line(l1:l2),length,iostat)
	      enddo
	      call txtclose(lu)
	    endif
	    dofile = .false.
	  else
	    call getarg(i,line(l1:l2))
	    if(line(l1:l2).eq.'-f')then
	      dofile = .true.
	    else
	      call hiswrite(tno,line)
	    endif
	  endif
	enddo
	end
