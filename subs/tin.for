c************************************************************************
	subroutine tinOpen(name)
c
	implicit none
	character name*(*)
c------------------------------------------------------------------------
	include 'tin.h'
	integer iostat
c
	call txtopen(lIn,name,'old',iostat)
	if(iostat.ne.0)then
	  line(1:64) = 'Error opening text file '//name
	  call bug('w',line(1:64))
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
     *	  call bug('f','Input string too short in tinLine')
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
     *		call bug('f','Text file line too long for me!')
          if(iostat.eq.0)then
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
            if(within)call bug('f','Unbalanced quotes on line')
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
c  Return the number of non=blank characters in the current line.
c
c------------------------------------------------------------------------
	include 'tin.h'
c
c  Skip white at the start of the line.
c
	dowhile(k1.le.k2)
	  if(line(k1:k1).le.' ')k1 = k1 + 1
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
	include 'tin.h'
	character string*48
	integer length
	logical ok
	double precision dval
c
	call getfield(line,k1,k2,string,length)
c
	if(length.eq.0)then
	  value = default
	else
	  call atodf(string(1:length),dval,ok)
	  if(ok)then
	    value = dval
	  else
	    call bug('f','Error reading numeric value from text file')
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
	include 'tin.h'
	character string*48
	integer length
	logical ok
	integer ival
c
	call getfield(line,k1,k2,string,length)
c
	if(length.eq.0)then
	  value = default
	else
	  call atoif(string(1:length),ival,ok)
	  if(ok)then
	    value = ival
	  else
	    call bug('f','Error reading numeric value from text file')
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
	include 'tin.h'
	integer i,length
	character value*128
c
	do i=1,n
	  call getfield(line,k1,k2,value,length)
	enddo
c
	end
c************************************************************************
	subroutine tinGeta(value,default)
c
	implicit none
	character value*(*),default*(*)
c------------------------------------------------------------------------
	include 'tin.h'
	integer length
c
	call getfield(line,k1,k2,value,length)
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
	include 'tin.h'
	character string*48
	integer length
	logical ok
c
	call getfield(line,k1,k2,string,length)
c
	if(length.eq.0)then
	  value = default
	  ok = .true.
	else if(fmt.eq.'dms'.or.fmt.eq.'hms'.or.fmt.eq.'dtime')then
	  call decangle(string,value,fmt,ok)
	else if(fmt.eq.'time'.or.fmt.eq.'atime')then
	  call dectime(string,value,fmt,ok)
	else
	  call bug('f','Unrecognised format in tinGett')
	endif
	if(.not.ok)
     *	  call bug('f','Error decoding text file angle or time')
c
	end

