c************************************************************************
c  The IntpIO routines implement a "on the fly" interpolation scheme,
c  to regrid an image. The interpolation scheme used is from
c  Robert Keys, "Cubic Convolution", IEEE Trans ASSP, Dec, 1981.
c  These routines may look rather cumbersome, but they vectorise!
c
c  History:
c    rjs  3nov89 Adapted from the Werong IMDIFF task.
c    rjs 25jan90 Minor documentation improvement.
c************************************************************************
c*IntpIni - Initialize the interpolation i/o routines.
c:interpolation,regridding
c+
	subroutine IntpIni(n1,n2,BlcTrc)
c
	implicit none
	integer n1,n2
	real BlcTrc(4)
c
c  Initialize the interpolation routines.
c
c  Input:
c    n1,n2	Size of the output image.
c    BlcTrc	A real array of 4 elements, giving xlo,ylo,xhi,xhi,
c		in the coord. system of the input, of the pixels which
c		correspond to (1,1) and (n1,n2) in the output.
c--
c------------------------------------------------------------------------
	integer i,jx
	real x,f
	include 'intpio.h'
c
c  Copy across things that we need to remember.
c
	nx = n1
	ny = n2
	do i=1,4
	  Coord(i) = BlcTrc(i)
	enddo
c
c  Calculate the weights and indices of the interpolation in the x dimension.
c
	do i=1,nx
	  x = (Coord(3)-Coord(1))/(nx-1)*(i-1) + Coord(1) - 1
	  jx = x
	  f = x - jx
	  wx(i,1) = ((-0.5*f+1.0)*f-0.5)*f
	  wx(i,2) = (( 1.5*f-2.5)*f    )*f + 1.0
	  wx(i,3) = ((-1.5*f+2.0)*f+0.5)*f
	  wx(i,4) = (( 0.5*f-0.5)*f    )*f
	  indx(i) = jx
	enddo
c
c  Do other initializations.
c
	call IntpRIni
	end
c************************************************************************
c*IntpRIni - Reinitialize the interpolation i/o routines.
c:interpolation,regridding
c+
	subroutine IntpRIni
	implicit none
c
c  Reinitialize the interpolation routines. This effectively empties the
c  internal buffers of the interpolation routine, but does not change
c  the interpolation mapping.
c--
c------------------------------------------------------------------------
	include 'intpio.h'
	integer i
	do i=1,4
	  indices(i) = 0
	enddo
	end
c************************************************************************
c*IntpRd - Read a row of interpolated data
c:interpolation,regridding
c+
	subroutine IntpRd(lu,jj,Out,IntpGet)
c
	implicit none
	integer lu,jj
	real Out(*)
	external IntpGet
c
c  The user-called routine to return a row of interpolated data.
c
c  Input:
c    lu		Some user-defined value for its action routine.
c    jj		The row of the output that we want.
c    IntpGet	The action routine to return a row of the input.
c  Output:
c    Out	The interpolated output.
c--
c------------------------------------------------------------------------
	include 'intpio.h'
	integer Pnt(4),jy,j,Pnt0,k
	real y,f,wy1,wy2,wy3,wy4
c
c  Calculate the weights, etc, for the y direction.
c
	y = (Coord(4)-Coord(2))/(ny-1)*(jj-1) + Coord(2) - 1
	jy = y
	f = y - jy
	wy1 = ((-0.5*f+1.0)*f-0.5)*f
	wy2 = (( 1.5*f-2.5)*f    )*f + 1.0
	wy3 = ((-1.5*f+2.0)*f+0.5)*f
	wy4 = (( 0.5*f-0.5)*f    )*f
c
c  Get the rows that we need.
c
	do j=jy,jy+3
	  Pnt0 = 0
	  do k=1,4
	    if(indices(k).eq.j.or.(Pnt0.eq.0.and.
     *	      (indices(k).lt.jy.or.indices(k).gt.jy+3))) Pnt0 = k
	  enddo
	  if(indices(Pnt0).ne.j)then
	    indices(Pnt0) = j
	    call IntpGet(lu,indices(Pnt0),Dat(1,Pnt0))
	  endif
	  Pnt(j-jy+1) = Pnt0
	enddo
c
c  Now do the actual interpolation.
c
	call Intpolat(Out,nx,Indx,Dat(1,Pnt(1)),Dat(1,Pnt(2)),
     *	  Dat(1,Pnt(3)),Dat(1,Pnt(4)),Wx(1,1),Wx(1,2),Wx(1,3),Wx(1,4),
     *	  Wy1,Wy2,Wy3,Wy4)
	end
c************************************************************************
	subroutine Intpolat(Out,n,indx,z0,z1,z2,z3,
     *		wx0,wx1,wx2,wx3,wy0,wy1,wy2,wy3)
c
	implicit none
	integer n
	integer indx(n)
	real Out(n),z0(*),z1(*),z2(*),z3(*)
	real wx0(n),wx1(n),wx2(n),wx3(n),wy0,wy1,wy2,wy3
c
c  Perform the actual interpolation.
c
c  Inputs:
c    n		Number of elements in the output.
c    indx	Element i in the output is formed by elements
c		indx(i) to indx(i)+3 in the input.
c    wx0,wx1,wx2,wx3 The interpolation weights in x.
c    wy0,wy1,wy2,wy3 The interpolation weights in y.
c    z0,z1,z2,z3     The values of the input.
c  Output:
c    Out	The interpolated values.
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Out(i) = ( z0(indx(i)  )*wy0 + z1(indx(i)  )*wy1
     *		   + z2(indx(i)  )*wy2 + z3(indx(i)  )*wy3 ) * wx0(i) +
     *		   ( z0(indx(i)+1)*wy0 + z1(indx(i)+1)*wy1
     *		   + z2(indx(i)+1)*wy2 + z3(indx(i)+1)*wy3 ) * wx1(i) +
     *		   ( z0(indx(i)+2)*wy0 + z1(indx(i)+2)*wy1
     *		   + z2(indx(i)+2)*wy2 + z3(indx(i)+2)*wy3 ) * wx2(i) +
     *		   ( z0(indx(i)+3)*wy0 + z1(indx(i)+3)*wy1
     *		   + z2(indx(i)+3)*wy2 + z3(indx(i)+3)*wy3 ) * wx3(i)
	enddo
	end
