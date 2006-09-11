C************************************************************************
C  These are a collection of routines originally based on Bergland and Dolan's
C  fft842 routine (called fft842x here). The fft842x (and the routines it calls)
c  is the real work horse. It performs a complex-to-complex FFT using a sign
c  of +1 in the exponent. The heart of fft842x is the r8txy and r8tyx routines,
c  which perform the radix-8 iterations.
c
c References:
c  Bergland G.D., Dolan M.T. (1979) "Fast Fourier transform algorithms",
c    in Programs for Digital Signal Processing, IEEE Press.
c  Press, Flannery,Teukolsky,Vetterling (1986), Numerical Recipes. Chapter 12.
C
c  History:
c    rjs Dark_ages  Adapted from the original Bergland and Dolan routines.
c    rjs   8sep89   Improved documentation.
c    rjs  25jan89   More documentation improvements.
C    rjs  20dec90   Check if N too large, in FFT842X.
c    rjs  19apr91   Redid FFTRC and FFTCR completely.
c    rjs  29apr91   Rewrote r8tyx to use "real" (rather than "complex")
c		    variables.
C************************************************************************
c* Fftrc -- Real to complex 1D FFT
c: fourier-transform,FFT
c& rjs
c+
	subroutine fftrc(in,out,isn,n)
c
	implicit none
	integer isn,n
	real in(n),out(n+2)
C
c  This performs a 1D Fourier transform of a real sequence. There is no
c  1/N scaling, and the "phase center" of the transform is the first
c  element of the input array.
c
c  Input:
c    in 	The input real array.
c    isn	The sign of the exponent in the transform, This can be
c		either 1 or -1.
c    n		The number of elements to transform. This must be a power
c		of 2.
c  Output:
c    out	The output array. Because of the conjugate symmetry of the
c		FFT of a real sequence, only half of the full complex sequence
c		is returned. Normally this array will be dimensioned of size
c		N/2+1 complex elements. Element 1 corresponds to the "DC" term,
c		element N/2+1 corresponding to the "folding frequency" term.
c		This array could alternately be dimensioned as N+2 real
c		elements.
c--
c------------------------------------------------------------------------
	real pi
	parameter(pi=3.141592653589793)
	real wr0,wi0,wr,wi,tr1,ti1,tr2,ti2,tr,ti,theta,t
	integer i,j
c
	do i=1,n
	  out(i) = in(i)
	enddo
c
c  Initially assume ISN is positive (this is all that fft842x can handle).
c
	call fft842x(out,n/2)
c
	theta = 2*pi/n
	wr0 = sin(theta/2)
	wr0 = -2*wr0*wr0
	wi0 = sin(theta)
c
	wr = 1 + wr0
	wi = wi0
c
	j = n-1
c#ivdep
	do i=3,n/2,2
	  tr1 = 0.5*(out(i)   + out(j))
	  ti1 = 0.5*(out(i+1) - out(j+1))
	  tr2 = 0.5*(out(i+1) + out(j+1))
	  ti2 =-0.5*(out(i)   - out(j))
	  tr = wr*tr2 - wi*ti2
	  ti = wi*tr2 + wr*ti2
	  out(i)   = tr1 + tr
	  out(i+1) = ti1 + ti
	  out(j)   = tr1 - tr
	  out(j+1) =-ti1 + ti
	  t = wr
	  wr = t*wr0  - wi*wi0 + t
	  wi = wi*wr0 + t*wi0  + wi
	  j = j - 2
	enddo
c
c  If ISN was really negative, conjugate the output.
c
	if(isn.lt.0)then
	  do i=4,n,2
	    out(i) = -out(i)
	  enddo
	endif
c
c  Fix the end elements.
c
	t = out(1)
	out(1)	 = t+out(2)
	out(n+1) = t-out(2)
	out(2)	 = 0
	out(n+2) = 0
	end
C************************************************************************
c* Fftcr -- Complex to real 1D FFT routine.
c: fourier-transform,FFT
c& rjs
c+
	subroutine fftcr(in,out,isn,n)
c
	implicit none
	integer n,isn
	real in(n+2),out(n)
C
c  This performs a 1D Fourier transform of a complex sequence (with
c  conjugate symmetry), to produce a real output. There is no
c  1/N scaling, and the "phase center" of the transform is the first
c  element of the input array.
c
c  Input:
c    in 	The input complex array. This will normally be dimensioned
c		as N/2+1 complex elements. Because the sequence is assumed
c		to have conjugate symmetry, only half the input array is
c		needed. The first element corresponds to the "DC" term.
c		This array could also be declared to be a real array of
c		size N+2 elements.
c    isn	The sign of the exponent in the transform, This can be
c		either 1 or -1.
c    n		The number of elements to transform. This must be a power
c		of 2.
c  Output:
c    out	The output real array.
c--
C------------------------------------------------------------------------
	real pi
	parameter(pi=3.141592653589793)
	real wr0,wi0,wr,wi,tr1,ti1,tr2,ti2,tr,ti,theta,t
	integer i,j
c
c  Copy the vector across. If ISN is negative, conjugate the data on the
c  way (as fft842x can only handle positive ISN).
c
	if(isn.gt.0)then
	  do i=1,n
	    out(i) = in(i)
	  enddo
	else
	  do i=1,n,2
	    out(i) = in(i)
	    out(i+1) = -in(i+1)
	  enddo
	endif
	out(2) = in(n+1)
c
	theta = 2*pi/n
	wr0 = sin(theta/2)
	wr0 = -2*wr0*wr0
	wi0 = sin(theta)
c
	wr = 1 + wr0
	wi = wi0
c
	j = n-1
c#ivdep
	do i=3,n/2,2
	  tr1 = (out(i)   + out(j))
	  ti1 = (out(i+1) - out(j+1))
	  tr2 = (out(i+1) + out(j+1))
	  ti2 =-(out(i)   - out(j))
	  tr = wr*tr2 - wi*ti2
	  ti = wi*tr2 + wr*ti2
	  out(i)   = tr1 + tr
	  out(i+1) = ti1 + ti
	  out(j)   = tr1 - tr
	  out(j+1) =-ti1 + ti
	  t = wr
	  wr = t*wr0  - wi*wi0 + t
	  wi = wi*wr0 + t*wi0  + wi
	  j = j - 2
	enddo
c
c  Fiddle the end elements, and do the FFT.
c
	t = out(1)
	out(1) =  (t + out(2))
	out(2) = -(t - out(2))
	out(n/2+1) = 2*out(n/2+1)
	out(n/2+2) = 2*out(n/2+2)
c
	call fft842x(out,n/2)
c
c  Reconjugate the data (always!).
c
	do i=2,n,2
	  out(i) = -out(i)
	enddo
c
	end
C************************************************************************
c* Fftcc -- Complex to complex 1D FFT routine.
c: fourier-transform,FFT
c& rjs
c+
	subroutine fftcc(in,out,isn,n)
c
	implicit none
	integer isn,n
	complex in(n),out(n)
c
c  This performs a 1D Fourier transform of a complex sequence. There is no
c  1/N scaling, and the "phase center" of the transform is the first
c  element of the input array.
c
c  Input:
c    in 	The input complex array.
c    isn	The sign of the exponent in the transform, This can be
c		either 1 or -1.
c    n		The number of elements to transform. This must be a power
c		of 2.
c  Output:
c    out	The output complex array.
c--
c------------------------------------------------------------------------
	integer i
c
C  Take the conjugate on a forward transform. Otherwise copy input
C  to output.
c
	if (isn.lt.0)then
	  do i=1,n
	    out(i) = conjg(in(i))
	  enddo
	else
	  do i=1,n
	    out(i) = in(i)
	  enddo
	endif
c
c  Do the fft.
c
	call fft842x(out,n)
c
c  Conjugate (forward) the result.
c
	if(isn.lt.0)then
	  do i=1,n
	    out(i) = conjg(out(i))
	  enddo
	endif
c
	end
c***********************************************************************
	subroutine fft842x(data,n)
c
	implicit none
	integer n
	complex data(n)
c
c Fast fourier transform for n=2**m, for complex input sequence.
c
c This routine replaces the vector data by its	finite
c discrete, complex fourier transform. It performs as many base
c 8 iterations as possible and then finishes with a base 4 iteration
c or a base 2 iteration if needed.
c
c Tables are used to store twiddle factors and bit-reverse permutation.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer lengt, nxtlt, i
	complex temp
c
	integer nsave,m,ni,i1(maxdim/2),i2(maxdim/2)
	complex twiddle(maxdim)
	save nsave,m,ni,i1,i2,twiddle
	data nsave/0/
c
c  Check if we have to initialise the arrays containing info for the
c  transformation.
c
	if(n.gt.maxdim)call bug('f','N too large in FFT routine')
	if(n.ne.nsave)call fftini(n,m,ni,i1,i2,twiddle)
	nsave = n
C
C Radix 8 passes,if any.
C
	nxtlt = n/8
	lengt = n
	do i=1,m/3
c#ifdef vector
	  if(n .ge. lengt*nxtlt)then
c#endif
	    call r8tyx(nxtlt, n, lengt, twiddle, data(1), data(nxtlt+1),
     *	      data(2*nxtlt+1), data(3*nxtlt+1), data(4*nxtlt+1),
     *	      data(5*nxtlt+1), data(6*nxtlt+1), data(7*nxtlt+1) )
c#ifdef vector
	  else
	    call r8txy(nxtlt, n, lengt, twiddle, data(1), data(nxtlt+1),
     *	      data(2*nxtlt+1), data(3*nxtlt+1), data(4*nxtlt+1),
     *	      data(5*nxtlt+1), data(6*nxtlt+1), data(7*nxtlt+1) )
	  endif
c#endif
	  lengt = nxtlt
	  nxtlt = nxtlt / 8
	enddo
C
C Do the remaining radix-4 or radix-2 stage, if required.
C
	if(mod(m,3).eq.2)then
	  call r4txx(n, data(1), data(2), data(3), data(4) )
	else if(mod(m,3).eq.1)then
	  call r2txx(n, data(1), data(2) )
	endif
C
C  Perform bit reversal.
C
c# ivdep
	do i=1,ni
	  temp = data(i1(i))
	  data(i1(i)) = data(i2(i))
	  data(i2(i)) = temp
	enddo
	end
c************************************************************************
	subroutine fftini(n,m,ni,i1,i2,twiddle)
c
	implicit none
	integer n,ni,m
	integer i1(n/2),i2(n/2)
	complex twiddle(n)
c
c  Generate twiddle factors and a bit-reverse lookup table.
c  This technique for generating bit-reverse permutation is (poorly) described
c  in Bracewell's Hartley transform book, and is apparently due to David
c  Evans. Though it is cryptic and long, it is significantly more efficient
c  than any other bit-reverse permutation I know of. It works by using a
c  bit-reverse lookup table for ndash = 2**(log2(n)/2).
c
c  Input:
c    n		Value of n to generate tables for.
c
c  Output:
c    m		log2(n)
c    i1,i2	Permutation array. To perform bit reverse ordering,
c		swap data(i1(i)) with data(i2(i)) for i=1,ni.
c    ni 	Size of permutation array.
c    twiddle	Twiddle factors.
c		  twiddle(i) = exp( j*2*pi/n * (i-1) )
c
c------------------------------------------------------------------------
	real pi
	parameter(pi=3.14159265358979323846)
	integer i,irev,k
	real theta,t
	complex w0,w
c
c  Determine m = log2(n)
c
	k = 1
	m = 0
	dowhile(k.lt.n)
	  k = k + k
	  m = m + 1
	enddo
	if(k.ne.n)call bug('f','Not power of 2 in FFTINI')
c
c  Generate a bit-reverse permutation array.
c
	ni = 0
	irev = 0
	do i=0,n-2
	  if(i.lt.irev)then
	    ni = ni + 1
	    i1(ni) = i+1
	    i2(ni) = irev+1
	  endif
	  k = n/2
	  dowhile(irev.ge.k)
	    irev = irev - k
	    k = k/2
	  enddo
	  irev = irev + k
	enddo
c
c  Generate the twiddle factors.
c
	twiddle(1) = (1.,0.)
	theta = 2*pi/n
	t = sin(theta/2)
	w0 = cmplx(-2*t*t,sin(theta))
	w = 1 + w0
	do i=2,n
	  twiddle(i) = w
	  w = w0*w + w
	enddo
	end
c**********************************************************************
      subroutine r2txx(nthpo, c0, c1)
c
      implicit none
      integer nthpo
      complex c0(*), c1(*)
c
c Radix 2 iteration subroutine.
c
c-----------------------------------------------------------------------
      complex temp
      integer k
c
      do k=1,nthpo,2
	temp  = c0(k) + c1(k)
	c1(k) = c0(k) - c1(k)
	c0(k) = temp
      enddo
c
      end
c************************************************************
      subroutine r4txx(nthpo, c0, c1, c2, c3 )
c
      implicit none
      integer nthpo
      complex c0(*), c1(*), c2(*), c3(*)
c
c Radix 4 iteration subroutine.
c
c-----------------------------------------------------------------------
      complex t1,t2,t3,t4
      integer k
c
      do k=1,nthpo,4
	t1 = c0(k) + c2(k)
	t2 = c0(k) - c2(k)
	t3 = c1(k) + c3(k)
	t4 = c1(k) - c3(k)
	t4 = cmplx(-aimag(t4),real(t4))
	c0(k) = t1 + t3
	c1(k) = t1 - t3
	c2(k) = t2 + t4
	c3(k) = t2 - t4
      enddo
c
      end
c#ifdef vector
C************************************************************************
      subroutine r8txy(nxtlt, nthpo, lengt, cs,
     *			c0, c1, c2, c3, c4, c5, c6, c7)
C
      implicit none
      integer nxtlt, nthpo, lengt
      complex c0(*),c1(*),c2(*),c3(*),c4(*),c5(*),c6(*),c7(*)
      complex cs(*)
c
c  Radix - 8 iterations.
c
c  NOTE: This code is only used on a vector machine. This routine performs
c  the same function as the r8tyx routine, except that the two inner loops
c  have been interchanged, to increase vector lengths.
c
c------------------------------------------------------------------------
      real p7
      parameter(p7=0.7071067811865475)
      complex a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7
      integer j,k
      integer i1,i2,i3,i4,i5,i6,i7
      integer inc1,inc2,inc3,inc4,inc5,inc6,inc7
c
      inc1 = nthpo/lengt
      inc2 = inc1 + inc1
      inc3 = inc1 + inc2
      inc4 = inc1 + inc3
      inc5 = inc1 + inc4
      inc6 = inc1 + inc5
      inc7 = inc1 + inc6
c
c#nooptimize
      do j=1,nthpo,lengt
	i1 = 1
	i2 = 1
	i3 = 1
	i4 = 1
	i5 = 1
	i6 = 1
	i7 = 1
	do k=j,j+nxtlt-1
	  a0 = c0(k) + c4(k)
	  a4 = c0(k) - c4(k)
	  a2 = c2(k) + c6(k)
	  a6 = c2(k) - c6(k)
	  a6 = cmplx(-aimag(a6),real(a6))
c
	  b0 = a0 + a2
	  b2 = a0 - a2
	  b4 = a4 + a6
	  b6 = a4 - a6
c
	  a1 = c1(k) + c5(k)
	  a5 = c1(k) - c5(k)
	  a3 = c3(k) + c7(k)
	  a7 = c3(k) - c7(k)
	  a7 = cmplx(-aimag(a7),real(a7))
c
	  b1 = a1 + a3
	  b3 = a1 - a3
	  b3 = cmplx(-aimag(b3),real(b3))
	  b5 = a5 + a7
	  b5 = cmplx(p7*(real(b5)-aimag(b5)),p7*(real(b5)+aimag(b5)))
	  b7 = a5 - a7
	  b7 = cmplx(-p7*(real(b7)+aimag(b7)),p7*(real(b7)-aimag(b7)))
c
 	  c0(k) =	    b0 + b1
	  c1(k) = cs(i4) * (b0 - b1)
	  c2(k) = cs(i2) * (b2 + b3)
	  c3(k) = cs(i6) * (b2 - b3)
	  c4(k) = cs(i1) * (b4 + b5)
	  c5(k) = cs(i5) * (b4 - b5)
	  c6(k) = cs(i3) * (b6 + b7)
	  c7(k) = cs(i7) * (b6 - b7)
c
	  i1 = i1 + inc1
	  i2 = i2 + inc2
	  i3 = i3 + inc3
	  i4 = i4 + inc4
	  i5 = i5 + inc5
	  i6 = i6 + inc6
	  i7 = i7 + inc7
	enddo
      enddo
      end
c#endif
c#ifndef vms
c************************************************************************
	subroutine r8tyx(nxtlt,nthpo,lengt,cs,c0,c1,c2,c3,c4,c5,c6,c7)
c
	implicit none
	integer nxtlt,nthpo,lengt
	real c0(*),c1(*),c2(*),c3(*),c4(*),c5(*),c6(*),c7(*)
	real cs(*)
c
c  Radix 8 iterations. This uses "REAL" rather than "COMPLEX" as many
c  compilers are inefficient at dealing with "COMPLEX".
c
c  NOTE: For VMS, the code in this routine has been transcribed into machine
c  for extra efficiency.
c------------------------------------------------------------------------
	real p7
	parameter(p7=0.7071067811865475)
	real a0r,a0i,a1r,a1i,a2r,a2i,a3r,a3i,tr,ti
	real b0r,b0i,b1r,b1i,b2r,b2i,b3r,b3i
	real b4r,b4i,b5r,b5i,b6r,b6i,b7r,b7i
	integer j,k
	integer i1,i2,i3,i4,i5,i6,i7
	integer inc1,inc2,inc3,inc4,inc5,inc6,inc7
c
c  Initialise.
c
	i1 = 1
	i2 = 1
	i3 = 1
	i4 = 1
	i5 = 1
	i6 = 1
	i7 = 1
c
	inc1 = 2*nthpo/lengt
	inc2 = inc1 + inc1
	inc3 = inc2 + inc1
	inc4 = inc3 + inc1
	inc5 = inc4 + inc1
	inc6 = inc5 + inc1
	inc7 = inc6 + inc1
c
c  Do the real work.
c
c#nooptimize
	do j=1,2*nxtlt,2
	  do k=j,2*nthpo,2*lengt
	    a0r = c0(k)   + c4(k)
	    a0i = c0(k+1) + c4(k+1)
	    a2r = c0(k)   - c4(k)
	    a2i = c0(k+1) - c4(k+1)
	    a1r = c2(k)   + c6(k)
	    a1i = c2(k+1) + c6(k+1)
	    a3r = c6(k+1) - c2(k+1)
	    a3i = c2(k)   - c6(k)
	    b0r = a0r + a1r
	    b0i = a0i + a1i
	    b2r = a0r - a1r
	    b2i = a0i - a1i
	    b4r = a2r + a3r
	    b4i = a2i + a3i
	    b6r = a2r - a3r
	    b6i = a2i - a3i
c
	    a0r = c1(k)   + c5(k)
	    a0i = c1(k+1) + c5(k+1)
	    a2r = c1(k)   - c5(k)
	    a2i = c1(k+1) - c5(k+1)
	    a1r = c3(k)   + c7(k)
	    a1i = c3(k+1) + c7(k+1)
	    a3r = c7(k+1) - c3(k+1)
	    a3i = c3(k)   - c7(k)
	    b1r = a0r + a1r
	    b1i = a0i + a1i
	    b3r = a1i - a0i
	    b3i = a0r - a1r
	    tr = a2r + a3r
	    ti = a2i + a3i
	    b5r = P7*(tr-ti)
	    b5i = P7*(tr+ti)
	    tr = a2r - a3r
	    ti = a2i - a3i
	    b7r = -P7*(tr+ti)
	    b7i =  P7*(tr-ti)
c
c#ifndef vector
	    if(j.gt.1)then
c#endif
	      c0(k)   = b0r + b1r
	      c0(k+1) = b0i + b1i
	      tr      = b0r - b1r
	      ti      = b0i - b1i
	      c1(k)   = cs(i4)   * tr - cs(i4+1) * ti
	      c1(k+1) = cs(i4+1) * tr + cs(i4)   * ti
	      tr      = b2r + b3r
	      ti      = b2i + b3i
	      c2(k)   = cs(i2)   * tr - cs(i2+1) * ti
	      c2(k+1) = cs(i2+1) * tr + cs(i2)   * ti
	      tr      = b2r - b3r
	      ti      = b2i - b3i
	      c3(k)   = cs(i6)   * tr - cs(i6+1) * ti
	      c3(k+1) = cs(i6+1) * tr + cs(i6)   * ti
	      tr      = b4r + b5r
	      ti      = b4i + b5i
	      c4(k)   = cs(i1)   * tr - cs(i1+1) * ti
	      c4(k+1) = cs(i1+1) * tr + cs(i1)   * ti
	      tr      = b4r - b5r
	      ti      = b4i - b5i
	      c5(k)   = cs(i5)   * tr - cs(i5+1) * ti
	      c5(k+1) = cs(i5+1) * tr + cs(i5)   * ti
	      tr      = b6r + b7r
	      ti      = b6i + b7i
	      c6(k)   = cs(i3)   * tr - cs(i3+1) * ti
	      c6(k+1) = cs(i3+1) * tr + cs(i3)   * ti
	      tr      = b6r - b7r
	      ti      = b6i - b7i
	      c7(k)   = cs(i7)   * tr - cs(i7+1) * ti
	      c7(k+1) = cs(i7+1) * tr + cs(i7)   * ti
c#ifndef vector
	    else
	      c0(k)    = b0r + b1r
	      c0(k+1) = b0i + b1i
	      c1(k)   = b0r - b1r
	      c1(k+1) = b0i - b1i
	      c2(k)   = b2r + b3r
	      c2(k+1) = b2i + b3i
	      c3(k)   = b2r - b3r
	      c3(k+1) = b2i - b3i
	      c4(k)   = b4r + b5r
	      c4(k+1) = b4i + b5i
	      c5(k)   = b4r - b5r
	      c5(k+1) = b4i - b5i
	      c6(k)   = b6r + b7r
	      c6(k+1) = b6i + b7i
	      c7(k)   = b6r - b7r
	      c7(k+1) = b6i - b7i
	    endif
c#endif
	  enddo
	  i1 = i1 + inc1
	  i2 = i2 + inc2
	  i3 = i3 + inc3
	  i4 = i4 + inc4
	  i5 = i5 + inc5
	  i6 = i6 + inc6
	  i7 = i7 + inc7
	enddo
	end
c#endif
