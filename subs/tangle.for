c***********************************************************************
c
c  History:
c    18oct93 rjs   Original version.
c    25nov93 rjs   Incorrect error check.
c    29sep94 rjs   Date type of today.
c************************************************************************
c* mkeyt - Retrieve multiple angles or times from the command line.
c& rjs
c: user-input,command-line
c+
	subroutine mkeyt(key,value,nmax,n,fmt)
c
	implicit none
	integer n,nmax
	character key*(*),fmt*(*)
	double precision value(nmax)
c
c  Retrieve multiple angles or times from the command line. If the
c  keyword is not found, then n=0 on return.
c
c  Input:
c    key	The name of the keyword.
c    nmax	The maximum number of values to return.
c    fmt	Gives the format of the keyword. See the description of
c		this in the keyt routine.
c  Output:
c    n		The number of values returned.
c    value	The returned values.
c------------------------------------------------------------------------
	logical more
c
c  Externals.
c
	logical keyprsnt
c
	n = 0
	more = keyprsnt(key)
	dowhile(more.and.n.lt.nmax)
	  n = n + 1
	  call keyt(key,value(n),fmt,0.d0)
	  more = keyprsnt(key)
	enddo
	if(more)call bug('f','Buffer overflow in mkeyt')
	end
c************************************************************************
c* keyt - Retrieve an angle or time from the command line.
c& rjs
c: user-input,command-line
c+
	subroutine keyt(key,value,fmt,def)
c
	implicit none
	character key*(*),fmt*(*)
	double precision value,def
c
c  Get an angle or time from the command line arguments, and decode it.
c
c  Input:
c    key	Keyword associated with the angle.
c    fmt	The format of the angle. Valid values are:
c		  'dms' -- An angle given as dd:mm:ss.s or dd.ddd. The output
c			   is in radians.
c		  'hms' -- An angle given as hh:mm:ss.s or hh.hhh. The output
c			   is in radians.
c		  'dtime' -- Day fraction, either as hh:mm:ss.s, or hh.hhh.
c			    The output is as a day fraction (i.e. number in
c			    the range [0,1]).
c		  'atime' -- A absolute time, either in the normal Miriad format
c			   (yymmmdd.ddd or yymmmdd:hh:mm:ss.s), or as an
c			   epoch (bxxxx or jxxx, where xxxx is a year. e.g.
c			   j2000). The output is in Julian days.
c		  'time' -- Either an absolute time or day fraction. The
c			   output is in either Julian days, or as a day
c			   fraction.
c    def	The default value.
c  Output:
c    value	The value retrieved.
c--
c------------------------------------------------------------------------
	character string*32
	logical ok
c
	call keya(key,string,' ')
	if(string.eq.' ')then
	  value = def
	  ok = .true.
	else if(fmt.eq.'dms'.or.fmt.eq.'hms'.or.fmt.eq.'dtime')then
	  call decangle(string,value,fmt,ok)
	else if(fmt.eq.'time'.or.fmt.eq.'atime')then
	  call dectime(string,value,fmt,ok)
	endif
	if(.not.ok)call bug('f','Error decoding angle or time')
	end
c************************************************************************
	subroutine dectime(string,value,fmt,ok)
c
	implicit none
	character string*(*),fmt*(*)
	double precision value
	logical ok
c
c  Decode a time.
c
c------------------------------------------------------------------------
	integer l1,l2
	double precision d
c
c  Externals.
c
	double precision epo2jul
	integer len1
c
	l1 = 1
	l2 = len1(string)
	if(string(1:1).eq.'B'.or.string(1:1).eq.'J'.or.
     *	   string(1:1).eq.'b'.or.string(1:1).eq.'j')then
	  l1 = l1 + 1
	  call atodf(string(l1:l2),d,ok)
	  if(.not.ok)return
	  value = epo2jul(d,string(1:1))
	else if(string.eq.'today')then
	  call todayjul(value)
	  ok = .true.
	else
	  call dayjul(string(l1:l2),value)
	  ok = .true.
	endif
	if(value.lt.1.and.fmt.eq.'atime')ok = .false.
c
	end
c************************************************************************
	subroutine decangle(angle,val,fmt,ok)
c
	character angle*(*),fmt*(*)
	double precision val
	logical ok
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	logical minus,point
	double precision v(3),f
	character c*1
	integer l,l1,l2,digits,nval
c
c  Externals.
c
	integer len1
c
c  Assume something is bad.
c
	ok = .false.
c
c  Get the length of the string, and handle negative numbers.
c
	l1 = 1
	l2 = len1(angle)
	minus = angle(1:1).eq.'-'
	if(minus)l1 = l1 + 1
c
c  Get the strings.
c
	nval = 1
	v(1) = 0
	v(2) = 0
	v(3) = 0
	digits = 0
	point = .false.
	do l=l1,l2
	  c = angle(l:l)
	  if(c.eq.':')then
	    if(nval.ge.3.or.digits.eq.0.or.point)return
	    nval = nval + 1
	    digits = 0
	  else if(c.eq.'.')then
	    point = .true.
	    f = 0.1
	  else if(c.ge.'0'.and.c.le.'9')then
	    digits = digits + 1
	    if(point)then
	      v(nval) = v(nval) + f * (ichar(c) - ichar('0'))
	      f = 0.1*f
	    else
	      v(nval) = 10*v(nval) + (ichar(c) - ichar('0'))
	    endif
	  else
	    return
	  endif
	enddo
	if(digits.eq.0)return
c
c  Check the validity of the string.
c
	if((v(1).ge.24.and.(fmt.eq.'dtime'.or.fmt.eq.'hms')).or.
     *	    v(1).ge.360.or.v(2).ge.60.or.v(3).ge.60)return
c
c  Return the result.
c
	val = v(1) + ( v(2) + v(3)/60.d0 )/60.d0
	if(minus) val = -val
	if(fmt.eq.'dms')then
	  val = dpi/180 * val
	else if(fmt.eq.'hms')then
	  val = dpi/12 * val
	else if(fmt.eq.'dtime')then
	  val = val/24
	else
	  call bug('f','Invalid format, in decangle')
	endif
c
c  All is OK.
c
	ok = .true.
c
	end
