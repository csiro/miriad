c************************************************************************
c  History:
c    25aug97 rjs	Original version.
c     8sep97 rjs	Better error message.
c    29oct97 rjs        tinLen was terribly flawed ... fix it.
c    02nov97 rjs	Better error messages. Flag parameter in tinopen.
c    04dec98 rjs	Better error messages again.
c************************************************************************
	subroutine tinOpen(name,flags)
c
	implicit none
	character name*(*),flags*(*)
c
c  Open a text file, and ready things for reading of a text file.
c
c  Inputs:
c    name	Name of the text file to process.
c    flags	Extra processing flags:
c		  'n'  Do not allow defaults
c		  's'  Steam-mode. Treat end-of-line like any other
c		       space.
c------------------------------------------------------------------------
	include 'tin.h'
	integer iostat
c
	stream = index(flags,'s').ne.0
	nodef  = index(flags,'n').ne.0
c
	fname = name
	recno = 0
	call txtopen(lIn,name,'old',iostat)
	if(iostat.ne.0)then
	  call tinBug('w','Error opening file')
	  call bugno('f',iostat)
	endif
c
	end
c************************************************************************
	subroutine tinClose
c
	implicit none
c------------------------------------------------------------------------
	include 'tin.h'
	call txtclose(lIn)
	end
c************************************************************************
	subroutine tinLine(line1,length)
c
	implicit none
	character line1*(*)
	integer length
c
c  Return the next input line to the user.
c
c------------------------------------------------------------------------
	include 'tin.h'
c
c  Externals.
c
	integer tinNext
c
	length = tinNext()
	if(length.gt.len(line1))
     *	  call tinBug('f','Input string too short in tinLine')
	if(length.gt.0)line1 = line(1:length)
	end
c************************************************************************
	integer function tinNext()
c
	implicit none
c
c  Move to the next line in the input.
c------------------------------------------------------------------------
	include 'tin.h'
	integer iostat,i,l
	character c*1
	logical more,within
c
	k1 = 1
	k2 = 0
	iostat = 0
c
	dowhile(k2.eq.0.and.iostat.eq.0)
	  call txtread(lIn,line,k2,iostat)
	  if(k2.gt.len(line))
     *	    call tinBug('f','Text file line too long for me!')
          if(iostat.eq.0)then
	    recno = recno + 1
            i = 0  
            l = 0
            more = .true.
            within = .false.
            dowhile(i.lt.k2.and.more)
              i = i + 1
              if(within)then
                within = line(i:i).ne.c
                l = i
              else if(line(i:i).eq.'"'.or.line(i:i).eq.'''')then
                within = .true.
                c = line(i:i)
                l = i
              else if(line(i:i).eq.'#')then
                more = .false.
              else if(line(i:i).gt.' '.and.line(i:i).ne.',')then
                l = i
	      else if(line(i:i).lt.' ')then
		line(i:i) = ' '
              endif
            enddo
            k2 = l
            if(within)call tinBug('f','Unbalanced quotes on line')
          else if(iostat.eq.-1)then
            k2 = 0
          else
            call bug('w','Error reading text file')
            call bugno('f',iostat)
          endif
	enddo
c
	tinNext = k2
c
	end
c************************************************************************
	integer function tinLen()
c
	implicit none
c
c  Return the number of non-blank characters left in the current line.
c
c------------------------------------------------------------------------
	include 'tin.h'
	logical more
c
c  Skip white at the start of the line.
c
	more = .true.
	dowhile(more.and.k1.le.k2)
	  more = line(k1:k1).le.' '
	  if(more)k1 = k1 + 1
	enddo
c
	tinLen = k2 - k1 + 1
	end
c************************************************************************
	subroutine tinGetr(value,default)
c
	implicit none
	real value,default
c------------------------------------------------------------------------
	double precision dvalue
	call tinGetd(dvalue,dble(default))	
	value = dvalue
	end
c************************************************************************
	subroutine tinGetd(value,default)
c
	implicit none
	double precision value,default
c------------------------------------------------------------------------
	character string*48
	integer length
	logical ok
	double precision dval
c
	call tinGet(string,length)
c
	if(length.eq.0)then
	  value = default
	else
	  call atodf(string(1:length),dval,ok)
	  if(ok)then
	    value = dval
	  else
	    call tinbug('f','Error reading numeric value')
	  endif
	endif
c
	end
c************************************************************************
	subroutine tinGeti(value,default)
c
	implicit none
	integer value,default
c------------------------------------------------------------------------
	character string*48
	integer length
	logical ok
	integer ival
c
	call tinGet(string,length)
c
	if(length.eq.0)then
	  value = default
	else
	  call atoif(string(1:length),ival,ok)
	  if(ok)then
	    value = ival
	  else
	    call tinbug('f','Error reading numeric value')
	  endif
	endif
c
	end
c************************************************************************
	subroutine tinSkip(n)
c
	implicit none
	integer n
c
c  Skip a number of tokens in the input stream.
c
c------------------------------------------------------------------------
	integer i,length
	character value*128
c
	do i=1,n
	  call tinGet(value,length)
	enddo
c
	end
c************************************************************************
	subroutine tinGeta(value,default)
c
	implicit none
	character value*(*),default*(*)
c------------------------------------------------------------------------
	integer length
c
	call tinGet(value,length)
c
	if(length.eq.0)value = default
	end
c************************************************************************
	subroutine tinGett(value,default,fmt)
c
	implicit none
	double precision value,default
	character fmt*(*)
c
c  Get an angle or time from the command line arguments, and decode it.
c
c  Input:
c    fmt        The format of the angle. Valid values are:
c                 'dms' -- An angle given as dd:mm:ss.s or dd.ddd. The output
c                          is in radians.
c                 'hms' -- An angle given as hh:mm:ss.s or hh.hhh. The output
c                          is in radians.
c                 'dtime' -- Day fraction, either as hh:mm:ss.s, or hh.hhh.
c                           The output is as a day fraction (i.e. number in
c                           the range [0,1]).
c                 'atime' -- A absolute time, either in the normal Miriad format
c                          (yymmmdd.ddd or yymmmdd:hh:mm:ss.s), or as an
c                          epoch (bxxxx or jxxx, where xxxx is a year. e.g.
c                          j2000). The output is in Julian days.
c                 'time' -- Either an absolute time or day fraction. The
c                          output is in either Julian days, or as a day  
c                          fraction.
c    default    The default value.
c  Output:
c    value      The value retrieved.
c--
c------------------------------------------------------------------------
	character string*48
	integer length
	logical ok
c
	call tinGet(string,length)
c
	if(length.eq.0)then
	  value = default
	  ok = .true.
	else if(fmt.eq.'dms'.or.fmt.eq.'hms'.or.fmt.eq.'dtime')then
	  call decangle(string,value,fmt,ok)
	else if(fmt.eq.'time'.or.fmt.eq.'atime')then
	  call dectime(string,value,fmt,ok)
	else
	  call tinbug('f','Unrecognised format in tinGett')
	endif
	if(.not.ok)
     *	  call tinbug('f','Error decoding angle or time')
c
	end
c************************************************************************
	subroutine tinGet(string,length)
c
	implicit none
	character string*(*)
	integer length
c------------------------------------------------------------------------
	include 'tin.h'
	integer itemp
c
c  Externals.
c
	integer tinNext
c
	call getfield(line,k1,k2,string,length)
	if(length.eq.0.and.stream)then
	  itemp = tinNext()
	  call getfield(line,k1,k2,string,length)
	endif
	if(nodef.and.length.eq.0)
     *		call tinBug('f','Values missing in this line')
	end
c************************************************************************
	subroutine tinBug(sev,string)
c
	implicit none
	character sev*(*),string*(*)
c------------------------------------------------------------------------
	include 'tin.h'
	integer l,lname,lnum	
	character num*9
c
	integer len1
	character itoaf*9
c
	lname = len1(fname)
	num = itoaf(recno)
	lnum = len1(num)
	l = min(k2,len(line))
	if(l.gt.0)l = len1(line(1:l))
	if(l.gt.0)then
	  call bug('w','Error processing text file string '//
     *	    line(1:l)//' (file '//
     *	    fname(1:lname)//', record '//num(1:lnum)//')')
	else if(recno.gt.0)then
	  call bug('w','Error processing text file '//fname(1:lname)//
     *	    ', record '//num)
	else if(lname.gt.0)then
	  call bug('w','Error processing text file '//fname(1:lname))
	endif
	call bug(sev,string)
	end
