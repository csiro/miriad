c************************************************************************
c  Routines to perform a convolution of a map by a beam.
c  History:
c    rjs Dark-ages Original version.
c    rjs  11sep89  Improved documentation.
c    rjs  25jan90  Minor documentation improvement.
c    pjt   3may90  Bug call verbosity
c    rjs   3apr91  Calls the MemAlloc and MemFree routines.
c    rjs  10sep96  Eliminate common.
c************************************************************************
c* ConvlIni -- Initialize the convolution routines.
c& rjs
c: convolution,FFT
c+
	subroutine ConvlIni(lu,Out,n1,n2,phat,ic,jc)
c
	implicit none
	integer lu,n1,n2,ic,jc
	real Out(n2,n1/2+1),phat
c
c  The Convolution routines perform an FFT-based convolution of two
c  images (usually a map and a beam). This routine initializes the 
c  convolution routines, and performs an FFT of the beam.
c
c  Input:
c    n1,n2	Size of the input beam. Must be a power of 2.
c    ic,jc	Reference pixel of the input beam.
c    lu		Handle of the MIRIAD file containing the beam. The beam
c		is assumed to be even, i.e.
c		  Beam(ic+i,jc+j) .eq. Beam(ic-i,jc-j)
c    phat	Prussian hat parameter.
c
c  Output:
c    Out	The output, transformed, transposed version of the input.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer pnt
	real Trans(maxbuf)
	real Data(maxdim)
	complex CData1(maxdim),CData2(maxdim),Rotv(maxdim)
	common Trans
c
	call MemAlloc(pnt,(n1+2)*n2,'r')
	if(max(n1,n2).gt.maxdim)
     *	  call bug('f','convlini: Insufficient space to transform beam')
c
	call  ConvlIn1(Data,Trans(pnt),CData1,CData2,Rotv,lu,Out,n1,n2,
     *				phat,ic,jc)
	call MemFree(pnt,(n1+2)*n2,'r')
c
	end
c************************************************************************
	subroutine ConvlIn1(Data,Trans,Cdata1,Cdata2,Rotv,
     *					lu,Out,n1,n2,phat,ic,jc)
c
	implicit none
	integer n1,n2,ic,jc,lu
	real Out(n2,n1/2+1),phat
c
	real Data(n1)
	complex Trans(n1/2+1,n2),CData1(n2),CData2(n2),Rotv(n2)
c
c------------------------------------------------------------------------
	real pi
	parameter(pi=3.141592653589793)
	integer i,j
	real theta,temp
	complex Rotu
c
	do j=1,n2
	  call xyread(lu,j,Data)
	  if(j.eq.jc) Data(ic) = Data(ic) + phat
	  call fftrc(Data,Trans(1,j),-1,n1)
	enddo
c
c  Prepare for the second pass. When copying the input to the output,
c  a phase term is applied, which is equivalent to originally shifting
c  the image to that the reference pixel of the input is at pixel (1,1).
c
	theta = 2*pi*real(jc-1)/real(n2)
	temp = 1./real(n1*n2)
	do j=1,n2
	  Rotv(j) = temp * cmplx(cos((j-1)*theta),sin((j-1)*theta))
	enddo
c
	do i=1,n1/2+1
	  theta = 2*pi*real(ic-1)*real(i-1)/real(n1)
	  Rotu = cmplx(cos(theta),sin(theta))
	  do j=1,n2
	    CData1(j) = Trans(i,j)
	  enddo
	  call fftcc(CData1,CData2,-1,n2)
	  do j=1,n2
	    Out(j,i) = real(Rotu * Rotv(j) * CData2(j))
	  enddo
	enddo
c
	end
c************************************************************************
c* Convl -- Performs the convolution of the image with the beam.
c& rjs
c: convolution,FFT
c+
	subroutine Convl(in,out,n,nx,ny,Runs,nRuns,Beam,n1,n2)
c
	implicit none
	integer n,nx,ny,n1,n2,nRuns,Runs(3,nRuns)
	real in(n),out(n),Beam(n2,n1/2+1)
c
c  Convolve the input by the beam, returning it in the output. Both the
c  input and output are only those portions of the image that are of
c  interest. These are described by "runs" arrays, as produced by
c  BoxRuns.
c
c  Input:
c    in		The input image. Only the pixels within the
c		region-of-interest are included.
c    n		Total number of pixels in the region-of-interest.
c    nx,ny	The bounding size of the input image.
c    Runs	The runs describing the region-of-interest in the input
c		image.
c    nRuns	The total number of runs.
c    Beam	The FFT of the beam, as produced by ConvlIni.
c    n1,n2	The size of the beam, in x and y.
c  Output:
c    out	The resultant convolved image.
c
c--
c  Internal:
c    Data	A temporary array to hold the data during the expansion
c		run is packed form into a linear array.
c    Trans	This is  a large temporary array, used in the FFT/transpose
c		process.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer pnt
	real Data(maxdim)
	complex CData1(maxdim),CData2(maxdim)
	real Trans(maxbuf)
	common Trans
c
	call MemAlloc(pnt,(n1+2)*ny,'r')
	if(max(n1,n2).gt.maxdim)
     *	call bug('f','convl: Insufficient space to perform convolution')
c
	call Convl1(Data,Trans(pnt),CData1,CData2,
     *		    in,out,n,nx,ny,Runs,nRuns,Beam,n1,n2)
	call MemFree(pnt,(n1+2)*ny,'r')
c
	end
c************************************************************************
	subroutine Convl1(Data,Trans,CData1,Cdata2,
     *			  in,out,n,nx,ny,Runs,nRuns,Beam,n1,n2)
c
	implicit none
	integer n,nx,ny,n1,n2,nRuns,Runs(3,nRuns+1)
	real in(n),out(n),Beam(n2,n1/2+1)
c
	real Data(n1)
	complex Trans(n1/2+1,ny),CData1(n2),CData2(n2)
c
c  This is called by CONVL to do the real work. This indirection is used
c  to simplify array subscripts.
c
c------------------------------------------------------------------------
	logical init
	integer i,j,k,id,izero
c
c  Zero pad the end of the array used for FFT's.
c
	do i=nx+1,n1
	  Data(i) = 0
	enddo
c
c  Pass 1: Expand the runs data into a linear array, remembering to zero
c  everything that should be zero. FFT the resulting array if necessary.
c
	id = 1
	k = 1
	do j=1,ny
	  izero = 0
	  dowhile(k.le.nruns.and.runs(1,k).eq.j)
	    do i=izero+1,runs(2,k)-1
	      Data(i) = 0
	    enddo
	    do i=runs(2,k),runs(3,k)
	      Data(i) = In(id)
	      id = id + 1
	    enddo
	    izero = runs(3,k)
	    k = k + 1
	  enddo
c
c  Check if this row is zero. In this case do not bother doing an FFT of
c  zeros. Otherwise do the FFT, placing the output into the scratch array.
c
	  if(izero.eq.0)then
	    do i=1,n1/2+1
	      Trans(i,j) = (0.,0.)
	    enddo
	  else
	    do i=izero+1,nx
	      Data(i) = 0
	    enddo
	    call fftrc(Data,Trans(1,j),-1,n1)
	  endif
	enddo
c
c  Pass 2: Perform the column FFT, multiply by the beam, and perform the
c  inverse FFT.
c
	do i=1,n1/2+1
	  do j=1,ny
	    CData1(j) = Trans(i,j)
	  enddo
	  do j=ny+1,n2
	    CData1(j) = (0.,0.)
	  enddo
c
	  call fftcc(CData1,CData2,-1,n2)
	  do j=1,n2
	    CData2(j) = CData2(j) * Beam(j,i)
	  enddo
	  call fftcc(CData2,CData1,+1,n2)
	  do j=1,ny
	    Trans(i,j) = CData1(j)
	  enddo
	enddo
c
c  Pass 3: Perform the final pass of the FFT, and copy the portions of
c  interest to the output.
c
	id = 1
	k = 1
	do j=1,ny
	  init = .false.
	  dowhile(k.le.nruns.and.runs(1,k).eq.j)
c
c  Transform if neccessary, and copy the data to the output.
c
	    if(.not.init)then
	      call fftcr(Trans(1,j),Data,+1,n1)
	      init = .true.
	    endif
	    do i=runs(2,k),runs(3,k)
	      Out(id) = Data(i)
	      id = id + 1
	    enddo
	    k = k + 1
	  enddo
	enddo
c
	end
