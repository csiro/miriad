c************************************************************************
c
c  A set of routines to do extended precision integer arithmetic
c  in standard FORTRAN. These routines approximately double the
c  range of integers.
c
c  History:
c    27mar09 rjs  Original version
c    21apr09 rjs  Complete rewrite to use a simpler, more limited,
c		  more efficient, representation.
c
c  An integer is stored as a triple
c
c    value = v(1) + v(2)*v(3)
c
c  The set of routines manipulate these triples.
c
c  subroutine mpCvtim(v,int)
c  integer function mpCvtmi(v)
c  double precision function mpCvtmd(v)
c  subroutine mpFmt(string,v)
c  subroutine mpAddmi(v,int)
c  subroutine mpAddmm(v1,v2)
c  subroutine mpSubmi(v,int)
c  subroutine mpSubmm(v1,v2)
c  subroutine mpMulmi(v,int)
c  subroutine mpMulmm(v1,v2)
c  integer function mpCmp(v1,v2)
c  subroutine mpNeg(v)
c  subroutine mpAbs(v)
c
c************************************************************************
	subroutine mpCvtim(v,k)
c
	implicit none
	integer v(3),k
c
c------------------------------------------------------------------------
	include 'mp.h'
	logical first
	save first
c
	data first/.true./
c
	if(first)call mpInit
	first = .false.
	call mpStd2(k,0,mpBase2,v(1),v(2))
	v(3) = mpBase2
	end
c************************************************************************
	subroutine mpStd2(v1,v2,v3,d1,d2)
c
	implicit none
	integer v1,v2,v3,d1,d2
c
c  Convert an arbitrary triple, v1+v2*v3, into a "standard triple",
c  d1+d2*mpBase2, where d1 and d2 have the same sign.
c------------------------------------------------------------------------
	include 'mp.h'
	integer d1a,a(3),b(3),d(6),carry,i,j,k
c
	d1 = v1
	d1a = 0
	if(v3.eq.mpBase2)then
	  d2 = v2
	else if(v2.ne.0.and.v3.ne.0)then
	  a(1) = v2
	  a(2) = a(1)/mpBase
	  a(1) = a(1) - a(2)*mpBase
	  a(3) = a(2)/mpBase
	  a(2) = a(2) - a(3)*mpBase
	  b(1) = v3
	  b(2) = b(1)/mpBase
	  b(1) = b(1) - b(2)*mpBase
	  b(3) = b(2)/mpBase
	  b(2) = b(2) - b(3)*mpBase
	  do k=1,6
	    d(k) = 0
	  enddo
	  do j=1,3
	    carry = 0
	    do i=1,3
	      k = i + j - 1
	      d(k) = d(k) + a(i)*b(j) + carry
	      carry = d(k)/mpBase
	      d(k) = d(k) - carry*mpBase
	    enddo
	    d(j+3) = d(j+3) + carry
	  enddo
	  d1a = d(2)*mpBase + d(1)
	  d2  = d(4)*mpBase + d(3)
	  if(abs(d(5))+abs(d(6)).ne.0)
     *		call bug('f','Integer overflow in mpStd2')
	else
	  d2 = 0
	endif
c
c  Add in the different components with care to avoid overflow.
c
	carry = d1/mpBase2
	d1 = d1 - carry*mpBase2
	d2 = d2 + carry
c
	d1 = d1 + d1a
	carry = d1/mpBase2
	d1 = d1 - carry*mpBase2
	d2 = d2 + carry
c
c  Make d1 and d2 have the same signs.
c
	if(d1.lt.0.and.d2.gt.0)then
	  d1 = d1 + mpBase2
	  d2 = d2 - 1
	else if(d1.gt.0.and.d2.lt.0)then
	  d1 = d1 - mpBase2
	  d2 = d2 + 1
	endif
c
	end
c************************************************************************
	double precision function mpCvtmd(v)
c
	implicit none
	integer v(3)
c------------------------------------------------------------------------
	double precision temp
c
c  Order the operations to avoid the possibility of overflow.
c
	temp = v(3)
	temp = temp*v(2) + v(1)
	mpCvtmd = temp
c
	end
c************************************************************************
	integer function mpCvtmi(v)
c
	implicit none
	integer v(3)
c------------------------------------------------------------------------
	include 'mp.h'
	integer t(3)
c
c  Externals.
c
	integer mpCmp
c
c  Check for overflow first.
c
	t(1) = v(1)
	t(2) = v(2)
	t(3) = v(3)
	call mpAbs(t)
	if(mpCmp(t,mpMAXInt).gt.0)
     *		call bug('f','Integer overflow in mpCvtmi')
	mpCvtmi = v(1) + v(2)*v(3)
	end
c************************************************************************
	subroutine mpAddmi(v,d)
c
	implicit none
	integer v(3),d
c
c  Add two numbers.
c------------------------------------------------------------------------
	include 'mp.h'
	integer t(3),carry

c
c  If the triple is in standard format and the input integer is
c  small, then do the operation quickly.
c
	if(v(3).eq.mpBase2.and.abs(d).lt.mpBase2
     *			  .and.abs(v(1)).lt.mpBase2)then
	  v(1) = v(1) + d
	  carry = v(1)/mpBase2
	  v(1) = v(1) - carry*mpBase2
	  v(2) = v(2) + carry
	else
	  call mpCvtim(t,d)
	  call mpAddmm(v,t)
	endif
c
	end
c************************************************************************
	subroutine mpAddmm(v1,v2)
c
	implicit none
	integer v1(3),v2(3)
c
c------------------------------------------------------------------------
	include 'mp.h'
	integer d11,d12,d21,d22,carry
c
c  If the triple is in the standard form, then do the operation
c  quickly. Otherwise convert to standard and do it.
c
	if(abs(v1(1)).lt.mpBase2.and.abs(v2(1)).lt.mpBase2.and.
     *	       v1(3) .eq.mpBase2.and.    v2(3) .eq.mpBase2)then
	  v1(1) = v1(1) + v2(1)
	  v1(2) = v1(2) + v2(2)
	else
	  call mpStd2(v1(1),v1(2),v1(3),d11,d12)
	  call mpStd2(v2(1),v2(2),v2(3),d21,d22)
	  v1(1) = d11 + d21
	  v1(2) = d12 + d22
	  v1(3) = mpBase2
	endif
	carry = v1(1)/mpBase2
	v1(1) = v1(1) - carry*mpBase2
	v1(2) = v1(2) + carry
	end
c************************************************************************
	subroutine mpSubmi(v,d)
c
	implicit none
	integer v(3),d
c
c  Add two numbers.
c------------------------------------------------------------------------
	include 'mp.h'
	integer t(3),carry

c
c  If the triple is in standard format and the integer is small, then do
c  the operation quickly.
c
	if(v(3).eq.mpBase2.and.abs(d).lt.mpBase2
     *			  .and.abs(v(1)).lt.mpBase2)then
	  v(1) = v(1) - d
	  carry = v(1)/mpBase2
	  v(1) = v(1) - carry*mpBase2
	  v(2) = v(2) + carry
	else
	  call mpCvtim(t,d)
	  call mpSubmm(v,t)
	endif
c
	end
c************************************************************************
	subroutine mpSubmm(v1,v2)
c
	implicit none
	integer v1(3),v2(3)
c
c  Subtract two triples.
c
c------------------------------------------------------------------------
	include 'mp.h'
	integer carry,d11,d12,d21,d22
c
	if(abs(v1(1)).lt.mpBase2.and.abs(v2(1)).lt.mpBase2.and.
     *	       v1(3) .eq.mpBase2.and.    v2(3) .eq.mpBase2)then
	  v1(1) = v1(1) - v2(1)
	  v1(2) = v1(2) - v2(2)
	else
	  call mpStd2(v1(1),v1(2),v1(3),d11,d12)
	  call mpStd2(v2(1),v2(2),v2(3),d21,d22)
	  v1(1) = d11 - d21
	  v1(2) = d12 - d22
	  v1(3) = mpBase2
	endif
	carry = v1(1)/mpBase2
	v1(1) = v1(1) - carry*mpBase2
	v1(2) = v1(2) + carry
	end
c************************************************************************
	subroutine mpMulmi(v,d)
c
	implicit none
	integer v(3),d
c
c  Multiply a triple with an integer.
c
c------------------------------------------------------------------------
	include 'mp.h'
	integer q(4),t(3),carry,i
c
	if(abs(d).lt.mpBase)then
	  call mpStd4(v,q)
	  carry = 0
	  do i=1,4
	    q(i) = q(i)*d + carry
	    carry = q(i)/mpBase
	    q(i) = q(i) - carry*mpBase
	  enddo
	  if(carry.ne.0)call bug('f','Integer overflow in mpMulmi')
	  v(1) = q(2)*mpBase + q(1)
	  v(2) = q(4)*mpBase + q(3)
	  v(3) = mpBase2
	else
	  call mpCvtim(t,d)
	  call mpMulmm(v,t)
	endif
	end
c************************************************************************
	subroutine mpMulmm(v1,v2)
c
	implicit none
	integer v1(3),v2(3)
c
c  Multiply two triples.
c
c------------------------------------------------------------------------
	include 'mp.h'
	integer q1(4),q2(4),d(8),i,j,k,carry
c
	call mpStd4(v1,q1)
	call mpStd4(v2,q2)
c
	do k=1,8
	  d(k) = 0
	enddo
c
	do j=1,4
	  carry = 0
	  do i=1,4
	    k = i + j - 1
	    d(k) = d(k) + q1(i)*q2(j) + carry
	    carry = d(k)/mpBase
	    d(k) = d(k) - carry*mpBase
	  enddo
	  d(j+4) = d(j+4) + carry
	enddo
c
	if(d(8).ne.0.or.d(7).ne.0.or.d(6).ne.0.or.d(5).ne.0)
     *	  call bug('f','Integer overflow in mpMulmm')
	v1(1) = d(2)*mpBase + d(1)
	v1(2) = d(4)*mpBase + d(3)
	v1(3) = mpBase2
c
	end
c************************************************************************
	subroutine mpStd4(v,q)
c
	implicit none
	integer v(3),q(4)
c
c  Convert a triple into a standard "quad" representation.
c
c  v(1)+v(2)*v(3) = q(1) + q(2)*mpBase + q(3)*mpBase**2 + q(4)*mpBase**3
c
c------------------------------------------------------------------------
	include 'mp.h'
	integer d1,d2
c
	call mpStd2(v(1),v(2),v(3),d1,d2)
	q(2) = d1/mpBase
	q(1) = d1 - q(2)*mpBase
	q(4) = d2/mpBase
	q(3) = d2 - q(4)*mpBase
	end
c************************************************************************
	integer function mpCmp(v1,v2)
c
	implicit none
	integer v1(3),v2(3)
c
c  Compute v1-v2 and reutnr +1, 0 or -1 depending on whether the result
c  is positive, zero or negative.
c
c------------------------------------------------------------------------
	integer t(3),v
c
	t(1) = v1(1)
	t(2) = v1(2)
	t(3) = v1(3)
	call mpSubmm(t,v2)
c
	if(t(3).le.0)call bug('f','Assertion in mpCmp failed')
	v = t(2)
	if(v.eq.0)v = t(1)
	if(v.lt.0)then
	  v = -1
	else if(v.gt.0)then
	  v = 1
	endif
c
	mpCmp = v
	end
c************************************************************************
	subroutine mpAbs(v)
c
	implicit none
	integer v(3)
c
c  Take the absolute value of a triple.
c
c------------------------------------------------------------------------
	include 'mp.h'
	integer d1,d2
c
	if(((v(1).le.0.and.v(2).le.0).or.(v(1).ge.0.and.v(2).ge.0))
     *	    .and.v(3).ge.0)then
	  v(1) = abs(v(1))
	  v(2) = abs(v(2))
	else
	  call mpStd2(v(1),v(2),v(3),d1,d2)
	  v(1) = abs(d1)
	  v(2) = abs(d2)
	  v(3) = mpBase2
	endif
c
	end
c************************************************************************
	subroutine mpNeg(v)
c
	implicit none
	integer v(3)
c
c  Negate a triple.
c
c------------------------------------------------------------------------
	v(1) = -v(1)
	v(2) = -v(2)
	end
c************************************************************************
	subroutine mpInit
c
	implicit none
c
c------------------------------------------------------------------------
	integer k
	include 'mp.h'
c
c  Externals.
c
	integer ipmpar
	k = ipmpar(3)
	mpBase = sqrt(0.99*real(k/2))
	mpBase2 = mpBase*mpBase
	call mpStd2(k,0,mpBase2,mpMaxInt(1),mpMaxInt(2))
	mpMaxInt(3) = mpBase2
c
	end
c************************************************************************
	subroutine mpFmt(out,v)
c
	implicit none
	integer v(3)
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
	integer maxtd,maxdig
	parameter(maxtd=9,maxdig=10)
	character fmt*6
	integer dout(2*maxdig),ndo,ndi,ntd,baseo,l1,l2,i,din(4)
	logical neg
c
c  Externals.
c
	character itoaf*(maxtd)
	integer len1
c
	call mpStd4(v,din)
	ndi = 0
	do i=1,4
	  if(din(i).ne.0)then
	    ndi = i
	    neg = din(i).lt.0
	  endif
	  din(i) = abs(din(i))
	enddo
c
	if(ndi.eq.0)then
	  out = '0'
	else
	  ntd = 1
	  baseo = 10
	  dowhile(10*baseo.lt.mpBase.and.ntd.lt.maxtd)
	    ntd = ntd + 1
	    baseo = 10*baseo
	  enddo
	  write(fmt,'(a,i1,a,i1,a)') '(i',ntd,'.',ntd,')'
c
	  call mpNewBas(ndi,din,mpBase,2*maxdig,ndo,dout,baseo)
c
	  if(len(out).lt.ntd+1)call bug('f','Format overflow in mpFmt')
	  if(neg)then
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
