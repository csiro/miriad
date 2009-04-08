c************************************************************************
c
c  A simple set of routines to handle multiple precision integer arithmetic.
c
c  The routines all take a handle which contains an encoding of the integer.
c
c  History:
c    27mar09 rjs  Original version
c
c  Routines:
c    integer function mpCvtim(int)         mp = int
c    integer function mpCvtmi(mp)          i = mp
c    double precision function mpCvtmd(mp) d = mp
c    subroutine mpAddmm(mp1,mp2)           mp1 = mp1 + mp1
c    subroutine mpSubmm(mp1,mp2)           mp1 = mp1 - mp2
c    subroutine mpMulmi(mp,int)            mp = int*mp
c    subroutine mpMulmm(mp1,mp2)           mp1 = mp1*mp2
c    subroutine mpNegm(mp)		   mp = -mp
c    subroutine mpAbsm(mp)		   mp = abs(mp)
c    subroutine mpPowm(mp,n)		   mp = mp**n
c    integer function mpCmpmm(mp1,mp2)     out = mp1 cmp mp2
c    integer function mpCmpAbs(mp1,mp2)	   out = abs(mp1) cmp abs(mp2)
c    integer function mpDup(mp)            Duplicate a handle.
c    subroutine mpDel(mp)		   Free up a handle.
c    subroutine mpFmt(string,mp)	   Convert to string.
c
c************************************************************************
	integer function mpDup(mp)
c
	implicit none
	integer mp
c
c  Generate a duplicate of the handle.
c
c  Input:
c    mp		The handle of the input integer.
c  Output:
c    mpDup	A handle of a duplicate.
c
c------------------------------------------------------------------------
	include 'mp.h'
	integer i,ret
c
	ret = first
	if(ret.eq.0)call bug('f','Not enough multiprecision integers')
	first = vars(1,ret)
c
	do i=1,abs(vars(1,mp))+1
	  vars(i,ret) = vars(i,mp)
	enddo
c
	mpDup = ret
c
	end
c************************************************************************
	subroutine mpDel(mp)
c
	implicit none
	integer mp
c
c  Free up the handle.
c
c  Input:
c    mp		The handle of the input integer.
c--
c------------------------------------------------------------------------
	include 'mp.h'
	vars(1,mp) = first
	first = mp
	end
c************************************************************************
	integer function mpCvtim(in)
c
	implicit none
	integer in
c
c  Convert a normal FORTRAN integer into the format 
c
c  Input:
c    in		Normal FORTRAN integer
c  Output:
c    mpCvtim	Handle of a multi-precision integer,
c------------------------------------------------------------------------
	include 'mp.h'
c
	integer mp,temp,nd
	logical doinit
	save doinit
	data doinit/.true./
c
c  Initialise the first time through.
c
	if(doinit)call mpInit
	doinit = .false.
c
c  Allocate a multi-precision integer slot.
c
	mp = first
	if(mp.eq.0)call bug('f','Not enough multiprecision integers')
	first = vars(1,mp)
c
	temp = abs(in)
c
	nd = 0
	dowhile(temp.gt.0)
	  nd = nd + 1
	  if(nd.gt.maxdig)call bug('f','Integer too large in mp')
	  vars(nd+1,mp) = mod(temp,base)
	  temp = temp / base
	enddo
	if(in.lt.0)nd = -nd
	vars(1,mp) = nd
c
	mpCvtim = mp
c
	end
c************************************************************************
	subroutine mpPowm(mp,n)
c
	implicit none
	integer n,mp
c
c  Raise a multi-precision integer to a power.
c
c  Input:
c    n		Integer to raise the number to. Must be non-negative.
c  Input/Output:
c    mp		Handle of the input multi-precision integer.
c
c------------------------------------------------------------------------
	include 'mp.h'
	integer MAXN
	parameter(MAXN=32)
	integer pows(MAXN),j,jd,logj,logjmax
c
c  Externals.
c
	integer mpDup
c
	if(n.eq.0)then
	  vars(1,mp) = 1
	  vars(2,mp) = 1
	else
	  j=1
	  logj = 1
	  pows(logj) = mp
	  dowhile(j+j.le.n)
	    logj = logj + 1
	    j = j + j
	    pows(logj) = pows(logj-1)
	    pows(logj-1) = mpDup(pows(logj))
	    call mpMulmm(pows(logj),pows(logj-1))
	  enddo
c
	  logjmax = logj
	  jd = j
c
c  Note at this stage, pows(logjmax) == mp
c
	  dowhile(logj.gt.1)
	    logj = logj - 1
	    j = j / 2
	    if(j+jd.le.n)then
	      jd = jd + j
	      call mpMulmm(mp,pows(logj))
	    endif
	    call mpDel(pows(logj))
	  enddo
	endif
c
	end
c************************************************************************
	double precision function mpCvtmd(mp)
c
	implicit none
	integer mp
c
c  Convert a multi-precision integer into a FORTRAN double precision.
c
c  Input:
c    mp		Handle of the input multi-precision integer.
c  Output:
c    mpCvtmd	The output double precision number.
c------------------------------------------------------------------------
	include 'mp.h'
	integer i,nd
	double precision temp
c
	temp = 0
	nd = abs(vars(1,mp))
	do i=nd,1,-1
	  temp = temp*base + vars(i+1,mp)
	enddo
	if(vars(1,mp).lt.0)temp = -temp
c
	mpCvtmd = temp
	end
c************************************************************************
	integer function mpCvtmi(mp)
c
	implicit none
	integer mp
c
c  Convert a multi-precision integer into a FORTRAN integer.
c  An error is generated if an overflow would result.
c
c  Input:
c    mp		Handle of the input multi-precision integer.
c  Output:
c    mpCvtmi	The output integer.
c------------------------------------------------------------------------
	include 'mp.h'
	integer ret,nd,i
c
	nd = abs(vars(1,mp))
	ret = 0
	do i=nd,1,-1
	  if((maxint-vars(i+1,mp))/base.lt.ret)
     *				call bug('f','Integer overflow')
	  ret = base*ret + vars(i+1,mp)
	enddo
c
	if(vars(1,mp).lt.0)ret = -ret
c
	mpCvtmi = ret
	end
c************************************************************************
	subroutine mpNegm(mp)
c
	implicit none
	integer mp
c
c  Negate a multi-precision integer. mp = -mp
c
c  Input/Output:
c    mp		Handle of the integer being negated.
c------------------------------------------------------------------------
	include 'mp.h'
	vars(1,mp) = -vars(1,mp)
	end
c************************************************************************
	subroutine mpAbsm(mp)
c
	implicit none
	integer mp
c
c  Take the absolute value of a multi-precision integer. mp = abs(mp)
c
c  Input/Output:
c    mp		The handle of the integer affected.
c------------------------------------------------------------------------
	include 'mp.h'
	vars(1,mp) = abs(vars(1,mp))
	end
c************************************************************************
	subroutine mpAddSub(mp1,mp2,dosub)
c
	implicit none
	integer mp1,mp2
	logical dosub
c
c  Internal routine to do addition/subtraction.
c
c------------------------------------------------------------------------
	include 'mp.h'
	logical diff
	integer tmp(maxdig),nd,nd1,nd2,carry,i,v1,v2,sgn,i1,i2,ndo
c
c  Externals.
c
	integer mpCmpAbs
c
	diff = vars(1,mp1)*vars(1,mp2).lt.0
	if(dosub) diff = .not.diff
	sgn = 1
	if(vars(1,mp1).lt.0)sgn = -1
c
	i1 = mp1
	i2 = mp2
	if(diff)then
	  if(mpCmpAbs(mp1,mp2).lt.0)then
	    i1 = mp2
	    i2 = mp1
	    sgn = -sgn
	  endif
	endif
c
	nd1 = abs(vars(1,i1))
	nd2 = abs(vars(1,i2))
	nd = max(nd1,nd2)
	carry = 0
	ndo = 0
	do i=1,nd
	  v1 = 0
	  if(i.le.nd1)v1 = vars(i+1,i1)
	  v2 = 0
	  if(i.le.nd2)v2 = vars(i+1,i2)
	  if(diff)v2 = -v2
	  tmp(i) = v1 + v2 + carry
	  carry = tmp(i)/base
	  tmp(i) = tmp(i) - carry*base
	  if(tmp(i).lt.0)then
	    tmp(i) = tmp(i) + base
	    carry = carry - 1
	  endif
	  if(tmp(i).ne.0)ndo = i
	enddo
	if(carry.gt.0)then
	  nd = nd + 1
	  ndo = nd
	  if(nd.gt.maxdig)call bug('f','Integer overflow in mpAddSub')
	  tmp(nd) = carry
	else if(carry.lt.0)then
	  call bug('f','Algorithmic failure in mpAddSub')
	endif
c
c  Copy to the output.
c
	do i=1,ndo
	  vars(i+1,mp1) = tmp(i)
	enddo
	vars(1,mp1) = sgn*ndo
c
	end
c************************************************************************
	subroutine mpMulmi(mp1,in)
c
	implicit none
	integer mp1,in
c
c  Multiply a multi-precision integer by a normal integer. mp1 = mp1*in
c
c  Input:
c    in		The input FORTRAN integer.
c  Input/Output:
c    mp1	The handle of the input/output mutli-precision integer.
c------------------------------------------------------------------------
	integer mp2
c
c  Externals.
c
	integer mpCvtim
c
	mp2 = mpCvtim(in)
	call mpMulmm(mp1,mp2)
	call mpDel(mp2)
	end
c************************************************************************
	subroutine mpMulmm(mp1,mp2)
c
	implicit none
	integer mp1,mp2
c
c  Multiply two multi-precision integers. mp1 = mp1*mp2
c
c  Input:
c    mp2	Handle of one of the multi-precision integers.
c  Input/Output:
c    mp1	Handle of the input/output multi-precision integer.
c------------------------------------------------------------------------
	include 'mp.h'
	integer nd,nd1,nd2,tmp(maxdig),i,j,k,carry
c
	nd1 = abs(vars(1,mp1))
	nd2 = abs(vars(1,mp2))
c
	do i=1,min(nd1+nd2-1,maxdig)
	  tmp(i) = 0
	enddo
c
	nd = 0
	do j=1,nd2
	  carry = 0
	  do i=1,nd1
	    k = i+j-1
	    nd = max(nd,k)
	    tmp(k) = tmp(k) + vars(i+1,mp1)*vars(j+1,mp2) + carry
	    carry = tmp(k)/base
	    tmp(k) = tmp(k) - carry*base
	  enddo
	  if(carry.gt.0)then
	    nd = k+1
	    if(nd.gt.maxdig)call bug('f','Integer overflow')
	    tmp(k+1) = carry
	  endif
	enddo
c
c  Copy to the output.
c
	do i=1,nd
	  vars(i+1,mp1) = tmp(i)
	enddo
c
	if(vars(1,mp1)*vars(1,mp2).lt.0)then
	  vars(1,mp1) = -nd
	else
	  vars(1,mp1) = nd
	endif
c
	end
c************************************************************************
	subroutine mpAddmm(mp1,mp2)
c
	implicit none
	integer mp1,mp2
c
c  Add two multi-precision integers. mp1 = mp1 + mp2
c
c  Input:
c    mp2	Handle of one of the multi-precision integers.
c  Input/Output:
c    mp1	Handle of the input/output multi-precision integer.
c------------------------------------------------------------------------
	call mpAddSub(mp1,mp2,.false.)
	end
c************************************************************************
	subroutine mpSubmm(mp1,mp2)
c
	implicit none
	integer mp1,mp2
c
c  Subtract two multi-precision integers. mp1 = mp1 - mp2
c
c  Input:
c    mp2	Handle of one of the multi-precision integers.
c  Input/Output:
c    mp1	Handle of the input/output multi-precision integer.
c------------------------------------------------------------------------
	call mpAddSub(mp1,mp2,.true.)
	end
c************************************************************************
	integer function mpCmpmm(mp1,mp2)
c
	implicit none
	integer mp1,mp2
c
c  Compare two multi-precision integers.
c
c  Input:
c    mp1,mp2	The handles of the multi-precision integers to compare.
c
c  Output:
c    mpCmpmm	+1 if mp1 > mp2
c		-1 if mp1 < mp2
c		 0 if mp1 == mp2
c------------------------------------------------------------------------
	include 'mp.h'
c
c  Externals.
c
	integer mpCmpAbs
c
	if(vars(1,mp1).eq.vars(1,mp2))then
	  mpCmpmm = mpCmpAbs(mp1,mp2)
	  if(vars(1,mp1).lt.0)mpCmpmm = -mpCmpmm
	else if(vars(1,mp1).gt.vars(1,mp2))then
	  mpCmpmm = 1
	else
	  mpCmpmm = -1
	endif
c
	end
c************************************************************************
	integer function mpCmpAbs(mp1,mp2)
c
	implicit none
	integer mp1,mp2
c
c  Compare the absolute value of two multi-precision integers.
c
c  Input:
c    mp1,mp2	The handles of the multi-precision integers to compare.
c
c  Output:
c    mpCmpmm	+1 if abs(mp1) > abs(mp2)
c		-1 if abs(mp1) < abs(mp2)
c		 0 if abs(mp1) == abs(mp2)
c
c------------------------------------------------------------------------
	include 'mp.h'
	integer nd1,nd2
	logical more
c
	nd1 = abs(vars(1,mp1))
	nd2 = abs(vars(1,mp2))
	if(nd1.eq.nd2)then
	  more = .true.
	  dowhile(nd1.gt.0.and.more)
	    more = vars(nd1+1,mp1).eq.vars(nd1+1,mp2)
	    if(more)nd1 = nd1 - 1
	  enddo
	  if(more)then
	    mpCmpAbs = 0
	  else if(vars(nd1+1,mp1).gt.vars(nd1+1,mp2))then
	    mpCmpAbs = 1
	  else
	    mpCmpAbs = -1
	  endif
	else if(nd1.gt.nd2)then
	  mpCmpAbs = 1
	else
	  mpCmpAbs = -1
	endif
c
	end
c************************************************************************
	subroutine mpInit
c
	implicit none
c
c  Internal routine used to initialise the mp routines.
c------------------------------------------------------------------------
	include 'mp.h'
	integer i
c
c  Externals.
c
	integer ipmpar
c
	do i=1,maxvar-1
	  vars(1,i) = i+1
	enddo
	vars(1,maxvar) = 0
	first = 1
c
	maxint = ipmpar(3)
c
	base = sqrt(real(maxint)) - 2
	if(base.lt.10)call bug('f','Something screwy in mp routines')
c
	end
c************************************************************************
	subroutine mpFmt(out,mp)
c
	implicit none
	integer mp
	character out*(*)
c
c  Format a multi-precision integer as a string.
c
c  Input:
c    mp		The handle of the input multi-precision integer.
c  Output:
c    out	The string containing a representation (base 10) of the 
c		multi-precision integer.
c------------------------------------------------------------------------
	include 'mp.h'
	integer maxtd
	parameter(maxtd=9)
	character fmt*6
	integer dout(2*maxdig),ndo,ndi,ntd,baseo,l1,l2,i
c
c  Externals.
c
	character itoaf*(maxtd)
	integer len1
c
	if(vars(1,mp).eq.0)then
	  out = '0'
	else
	  ntd = 1
	  baseo = 10
	  dowhile(10*baseo.lt.base.and.ntd.lt.maxtd)
	    ntd = ntd + 1
	    baseo = 10*baseo
	  enddo
	  write(fmt,'(a,i1,a,i1,a)') '(i',ntd,'.',ntd,')'
c
	  ndi = abs(vars(1,mp))
	  call mpNewBas(ndi,vars(2,mp),base,2*maxdig,ndo,dout,baseo)
c
	  if(len(out).lt.ntd+1)call bug('f','Format overflow in mpFmt')
	  if(vars(1,mp).lt.0)then
	    out = '-'//itoaf(dout(ndo))
	  else
	    out = itoaf(dout(ndo))
	  endif
	  l1 = len1(out(1:ntd+1)) + 1
	  l2 = len(out)
c
	  if((ndo-1)*ntd+l1-1.gt.l2)call bug('f','Format overflow')
	  do i=ndo-1,1,-1
	    write(out(l1:l1+ntd-1),fmt)dout(i)
	    l1 = l1 + ntd
	  enddo
c
	endif
c
	end
c************************************************************************
	subroutine mpNewBas(ndi,din,basei,maxdo,ndo,dout,baseo)
c
	implicit none
	integer ndi,din(ndi),basei,maxdo,ndo,dout(maxdo),baseo
c
c  Internal routine used to change the radix base of the representation
c  of a multi-precision integer.
c
c  Input:
c    nd		The number of digits in the input.
c    din	An array of the digits of the input.
c    basei	Radix base of the input.
c    maxdo	The maximum possible number of digits for the output.
c    baseo	Radix base of the output.
c
c  Output:
c    dout	The digits for the output.
c    ndo	The number of digits in the output.
c
c------------------------------------------------------------------------
	integer carry,i,j
c
	dout(1) = 0
	ndo = 0
	do i=ndi,1,-1
	  carry = din(i)
	  do j=1,ndo
	    carry = carry + basei*dout(j)
	    dout(j) = mod(carry,baseo)
	    carry = carry/baseo
	  enddo
	  dowhile(carry.ne.0)
	    ndo = ndo + 1
	    if(ndo.gt.maxdo)
     *	      call bug('f','Integer overflow in mpNewBas')
	    dout(ndo) = mod(carry,baseo)
	    carry = carry/baseo
	  enddo
	enddo
c
	end
