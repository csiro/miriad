c************************************************************************
c
c  These routines perform FFTs
c
c  History:
c    rjs  Dark-ages Original version.
c    rjs  27mar91   Adapted for Convex with the VECLIB library routines.
c    rjs  11apr91   Changed itoa to itoaf.
c    rjs  28may93   Better error messages when N is not a power of 2.
c************************************************************************
	subroutine fftcc(in,out,sgn,n)
c
	implicit none
	integer n,sgn
	complex in(n),out(n)
c
c  Take a complex-to-complex Fourier transform.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex work(5*maxdim/2)
	integer nw,ier,i
c
c  Externals.
c
	character itoaf*4
c
	save work,nw
	data nw/0/
c
c  Make sure we have enough storage.
c
	if(n.gt.maxdim)
     *		call bug('f','FFT size exceeds internal storage')
c
c  Check if we have the right number.
c
	if(nw.ne.n)then
	  call c1dfft(in,n,work,-3,ier)
	  if(ier.eq.-2)then
	    call bug('f','N not a power of 2 in FFT routine')
	  else if(ier.ne.0)then
	    call bug('f','Error in FFTCC, ier = '//itoaf(ier))
	  endif
	  nw = n
	endif
c
c  Copy the input to the output.
c
	do i=1,n
	  out(i) = in(i)
	enddo
c
c  Do FFT.
c
	if(sgn.lt.0)then
	  call c1dfft(out,n,work,+1,ier)
	else
	  call c1dfft(out,n,work,-2,ier)
	endif
	if(ier.ne.0)call bug('f','Error in FFTCC, ier = '//itoaf(ier))
c
	end
c************************************************************************
	subroutine fftrc(in,out,sgn,n)
c
	implicit none
	integer n,sgn
	real  in(n),out(n+2)
c
c  Take a real-to-complex Fourier transform.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex work(3*maxdim/2+2)
	integer nw,ier,i
c
c  Externals.
c
	character itoaf*3
c
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
	  call src1ft(out,n,work,-3,ier)
	  if(ier.eq.-2)then
	    call bug('f','N not a power of 2 in FFT routine')
	  else if(ier.ne.0)then
	    call bug('f','Error in FFTRC, ier = '//itoaf(ier))
	  endif
	  nw = n
	endif
c
c  Copy the input to the output.
c
	do i=1,n
	  out(i) = in(i)
	enddo
c
c  Do the FFT. The Convex routine only allows a exp(-j2pi*k*l/N)
c  real-to-complex FFT. The exp(+j2pi*k*l/N) transform is just the
c  conjugate of the exp(-j2pi*k*l/N) transform. So conjugate the
c  data afterwards if necessary.
c
	call src1ft(out,n,work,+1,ier)
	if(ier.ne.0) call bug('f','Error in FFTRC, ier = '//itoaf(ier))
	if(sgn.gt.0)then
	  do i=4,n,2
	    out(i) = -out(i)
	  enddo
	endif
c
	end
c************************************************************************
	subroutine fftcr(in,out,sgn,n)
c
	implicit none
	integer n,sgn
	real in(n+2),out(n)
c
c  Take a complex-to-real Fourier transform.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex work(3*maxdim/2+2)
	real temp(maxdim+2)
	integer nw,ier,i
c
c  Externals.
c
	character itoaf*3
c
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
	  call src1ft(out,n,work,-3,ier)
	  if(ier.eq.-2)then
	    call bug('f','N not a power of 2 in FFT routine')
	  else if(ier.ne.0)then
	    call bug('f','Error in FFTCR, ier = '//itoaf(ier))
	  endif
	  nw = n
	endif
c
c  Copy the input to the temporary. We need a temporary, because the input
c  to the Convex routine need to be of size n+2. Also the Convex routine only
c  allows a exp(+j2pi*k*l/N) complex-to-real FFT. To get the exp(-j2pi*k*l/N)
c  transform, we merely need to conjugate the input before the
c  exp(+j2pi*k*l/N) transform.
c
	if(sgn.lt.0)then
	  do i=1,n+2,2
	    temp(i) = in(i)
	    temp(i+1) = -in(i+1)
	  enddo
	else
	  do i=1,n+2
	    temp(i) = in(i)
	  enddo
	endif
c
c  Do the FFT. 
c
	call src1ft(temp,n,work,-2,ier)
	if(ier.ne.0)call bug('f','Error in FFTCR, ier='//itoaf(ier))
c
c  Copy the result to the output.
c
	do i=1,n
	  out(i) = temp(i)
	enddo
c
	end
