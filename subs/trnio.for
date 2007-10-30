c************************************************************************
c
c  These routines are used to reorder the axes of a 3D image. The
c  reordering is such that axes 1,2,3 on input, become axes 2,3,1 on
c  output, respectively. These routines process a plane at a time.
c
c  History:
c    rjs Dark-ages Adapted from Werong's tranio.for.
c    rjs  11sep89  Improved documentation.
c    rjs  30mar92  Uses memalloc/memfree routines.
c
c************************************************************************
c*TrnIni -- Initialise the transpose routines.
c:transpose,reorder
c+
	subroutine trnIni(nd1,nd2,nd3)
c
	implicit none
	integer nd1,nd2,nd3
c
c  The transpose routines perform a reordering on the axes of a 3D cube.
c  In particular the reordering is such that axes 1,2,3 on input, become
c   axes 2,3,1 on output, respectively.
c  Initialise things for the transpose routine.
c
c  Inputs:
c    nd1,nd2,nd3	Dimensions of the input image.
c--
c------------------------------------------------------------------------
	include 'trnio.h'
c
	n1 = nd1
	n2 = nd2
	n3 = nd3
	if(n1*n2*n3.gt.maxbuf)then
	  blk = max(1, maxbuf / (n1*n2))
	  size = blk*n1*n2
	  call ScrOpen(lScr)
	else
	  size = n1*n2*n3
	  blk = 0
	  lScr = 0
	endif
	call memalloc(buf,size,'r')
c
	end
c************************************************************************
c*TrnRead -- Read back a plane of the reordered cube.
c:transpose,reorder
c+
	subroutine trnread(p,Data)
c
	implicit none
	integer p
	real Data(*)
c
c  TrnRead reads back a plane of the reordered cube.
c
c  Input:
c    p		Plane number. Plane numbers run from 1 to nd1.
c    Data	The pixel data of the plane. This should be a real array
c		of size nd2 by nd3.
c--
c------------------------------------------------------------------------
	include 'trnio.h'
	integer i,j,k,ktot,ltot,pnt
c
	if(blk.eq.0)then
	  pnt = (p-1)*n1 + buf
	  k = 1
	  do j=1,n3
	    do i=1,n1
	      Data(k) = ref(pnt)
	      pnt = pnt + 1
	      k = k + 1
	    enddo
	    pnt = pnt - n1 + n1*n2
	  enddo
	else
	  pnt = n1*blk*(p-1)
	  k = 0
	  ktot = n1*n3
	  dowhile(k.lt.ktot)
	    ltot = min(ktot-k,n1*blk)
	    call scrread(lScr,Data(k+1),pnt,ltot)
	    pnt = pnt + n2*ltot
	    k = k + ltot
	  enddo
	endif
c
	end
c************************************************************************
c*TrnWrite -- Write a plane of the cube in its initial order.
c:transpose,reorder
c+
	subroutine trnwrite(p,Data)
c
	implicit none
	integer p
	real Data(*)
c
c  The caller passes TrnWrite a plane of the input (initial ordered) cube.
c  The caller can then later read back a reordered cube with TrnRead.
c
c  Input:
c    p		Plane number. Plane numbers vary from 1 to nd3.
c    Data	The pixel data of the plane. This should be a real array
c		of size nd1 by nd2.
c------------------------------------------------------------------------
	include 'trnio.h'
	integer i,j,k,pnt
c
	if(blk.eq.0)then
	  pnt = (p-1)*n1*n2 + buf
	  do k=1,n1*n2
	    ref(pnt) = Data(k)
	    pnt = pnt + 1
	  enddo
	else
c
c  Copy the data to the buffer.
c
	  pnt = n1 * mod(p-1,blk) + buf
	  k = 1
	  do j=1,n2
	    do i=1,n1
	      ref(pnt) = Data(k)
	      pnt = pnt + 1
	      k = k + 1
	    enddo
	    pnt = pnt + n1*(blk-1)
	  enddo
c
c  Write it out if necessary.
c
	  if(p.eq.n3.or.mod(p,blk).eq.0)then
	    pnt = ((p-1)/blk) * n1*n2*blk
	    call scrwrite(lScr,ref(buf),pnt,n1*n2*blk)
	  endif
	endif
c
	end
c************************************************************************
c*TrnFin -- Close up the transpose routines.
c:transpose,reorder
c+
	subroutine trnFin
c
	implicit none
c
c  TrnFin releases any resources allocated by the transpose routines.
c--
c------------------------------------------------------------------------
	include 'trnio.h'
c
	call memfree(buf,size,'r')
	if(lScr.ne.0)call ScrClose(lScr)
	lScr = 0
	end
