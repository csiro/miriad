c************************************************************************
	subroutine fftcc(in,out,sign,n)
c
	implicit none
	integer n,sign
	complex in(n),out(n)
c
c  Take a Fourier transform. On the Cray, just use the local routines,
c  while taking care of a bit of overhead.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex work(5*maxdim/2)
	integer nw
	save work,nw
	data nw/0/
c
c  Make sure we have enough storage.
c
	if(n.gt.maxdim)
     *		call bug('f','FfT size exceeds internal storage')
c
c  Check if we have the right number.
c
	if(nw.ne.n)then
	  call cfft2(1,sign,n,in,work,out)
	  nw = n
	endif
	call cfft2(0,sign,n,in,work,out)
c
	end
c************************************************************************
	subroutine fftrc(in,out,sign,n)
c
	implicit none
	integer n,sign
	real  in(n)
	complex out(n/2+1)
c
c  Take a Fourier transform. On the Cray, just use the local routines,
c  while taking care of a bit of overhead.
c
c------------------------------------------------------------------------
	integer i
	include 'maxdim.h'
	complex work(3*maxdim/2+2)
	integer nw
	save work,nw
	data nw/0/
c
c  Check we have enough space.
c
	if(n.gt.maxdim)
     *		call bug('f','FFT size exceeds internal storage')
c
c  Check if we have the right number.
c
	if(nw.ne.n)then
	  call rcfft2(1,sign,n,in,work,out)
	  nw = n
	endif
c
c  Do the FFT.
c
	call rcfft2(0,sign,n,in,work,out)
c
c  The Cray routine makes the output a factor of 2 more than you could
c  hope for. So divide by two.
c
	do i=1,n/2+1
	  out(i) = 0.5 * out(i)
	enddo
c
	end
c************************************************************************
	subroutine fftcr(in,out,sign,n)
c
	implicit none
	integer n,sign
	real  in(n)
	complex out(n)
c
c  Take a Fourier transform. On the Cray, just use the local routines,
c  while taking care of a bit of overhead.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex work(3*maxdim/2+2)
	integer nw
	save work,nw
	data nw/0/
c
c  Check that we have enough space.
c
	if(n.gt.maxdim)
     *		call bug('f','FFT size exceeds internal storage')
c
c  Check if we have the right number.
c
	if(nw.ne.n)then
	  call crfft2(1,sign,n,in,work,out)
	  nw = n
	endif
	call crfft2(0,sign,n,in,work,out)
c
	end
