c************************************************************************
c
c  These routines are used to reorder the axes of a 3D image.
c
c  History:
c    rjs  Dark-ages Adapted from Werong's tranio.for
c    rjs  11sep89 Improved documentation.
c    rjs  30mar92 Use memalloc/memfree routines.
c    rjs  19dec92 Near total rewrite, to appease nebk!
c    rjs  22feb93 Include maxnax.h
c    rjs  18sep05 Corrected declaration of the work array.
c
c************************************************************************
c*TrnIni -- Initialise the transpose routines.
c:transpose,reorder
c& rjs
c+
	subroutine trnIni(lu,naxis,nsize,reorder)
c
	implicit none
	integer lu,naxis,nsize(naxis)
	character reorder*(*)
c
c  Initialise the transpose routines.
c
c  Inputs:
c    naxis	The dimensionality of the image. Currently this must
c		be 2 or 3.
c    nsize	The size of each dimension in the image.
c    reorder	String giving the reordering to be performed.
c  Output:
c    lu		Handle used internally.
c--
c------------------------------------------------------------------------
	include 'trnio.h'
	integer i,totsize,blk1
	logical first
	save first
	data first/.true./
c
c  If this is the first time, initialise all the slots.
c
	if(first)then
	  do i=1,MAXSLOTS
	    inuse(i) = .false.
	  enddo
	  first = .false.
	endif
c
c  Allocate a slot.
c
	lu = 0
	do i=1,MAXSLOTS
	  if(.not.inuse(i))lu = i
	enddo
	if(lu.eq.0)call bug('f','All slots allocated, in TRNINI')
c
c  Copy the dimensios to the output.
c
	inuse(lu) = .true.
	if(naxis.gt.MAXNAX.or.naxis.lt.2)
     *	  call bug('f','Bad number of dimensions, in TRNINI')	
c
c  Decode the reorder argument, set array size parameters.
c
	call trnop(lu,naxis,nsize,reorder)
c
c  Save array info.
c
	totsize = 1
	do i=1,ndim(lu)
	  totsize = totsize * size(i,lu)
	enddo
c
c  If the buffer needed is less than MAXBUF is size, do it all in
c  memory.
c
	blk1 = min(max(1,MAXBUF/(size(1,lu)*size(2,lu))),size(3,lu))
	inmem(lu) = blk1*size(1,lu)*size(2,lu).ge.totsize
	if(inmem(lu))then
	  blk(lu) = 0
	  memsize(lu) = totsize
	  lScr(lu) = 0
	else
	  if(major(lu))then
	    blk(lu) = blk1
	    memsize(lu) = blk(lu) * size(1,lu) * size(2,lu)
	  else
	    blk(lu) = 0
	    memsize(lu) = 0
	  endif
	  call ScrOpen(lScr(lu))
	endif
c
	if(memsize(lu).gt.0)call memalloc(buf(lu),memsize(lu),'r')
	p(lu) = 0
	end
c************************************************************************
c*TrnWrite -- Write a plane pof the cube in its initial order.
c:transpose,reorder
c& rjs
c+
	subroutine trnwrite(lu,Data)
c
	implicit none
	integer lu
	real Data(*)
c
c  The caller passes TrnWrite a plane of the input (initial ordered) cube.
c
c  Input:
c    lu		Trnio handle.
c  Input/Output:
c    Data	The pixel data of the plane. This should be a real array
c		of size size(1,lu) by size(2,lu). This may be destroyed
c		on output.
c------------------------------------------------------------------------
	include 'trnio.h'
	integer work(MAXDIM)
	integer n1,n2,n3,pnt,k,j,i,offset,length,ifail
c
	n1 = size(1,lu)
	n2 = size(2,lu)
	n3 = size(3,lu)
c
	p(lu) = p(lu) + 1
	if(p(lu).lt.1.or.p(lu).gt.n3)call bug('f',
     *	  'Written too many planes, in TRNWRITE')
	if(pre(lu))then
	  call transr(Data,n1,n2,work,MAXDIM,ifail)
	  call trnswap(n1,n2)
	  if(ifail.ne.0)call bug('f','Initial transpose failed')
	endif
c
c  Case of an in memory transpose.
c
	if(inmem(lu))then
	  pnt = (p(lu)-1)*n1*n2 + buf(lu)
	  do k=1,n1*n2
	    ref(pnt) = Data(k)
	    pnt = pnt + 1
	  enddo
c
c  Case of disk-based transpose, with a "major" step.
c
	else if(major(lu))then
	  pnt = n1 * mod(p(lu)-1,blk(lu)) + buf(lu)
	  k = 1
	  do j=1,n2
	    do i=1,n1
	      ref(pnt) = Data(k)
	      pnt = pnt + 1
	      k = k + 1
	    enddo
	    pnt = pnt + n1*(blk(lu)-1)
	  enddo
c
	  if(p(lu).eq.n3.or.mod(p(lu),blk(lu)).eq.0)then
	    offset = ((p(lu)-1)/blk(lu)) * n1*n2*blk(lu)
	    length = n1*n2*blk(lu)
	    call ScrWrite(lScr(lu),ref(buf(lu)),offset,length)
	  endif
c
c  Case of a disk-based transpose, without a "major" step.
c
	else
	  length = n1*n2
	  offset = (p(lu)-1)*n1*n2
	  call ScrWrite(lScr(lu),Data,offset,length)
	endif
c
c  Zero the plane counter when we have finished.
c
	if(p(lu).eq.n3) p(lu) = 0
c
 	end
c************************************************************************
c*TrnRead -- Read back a plane of the reordered cube.
c:transpose,reorder
c& rjs
c+
	subroutine trnread(lu,Data)
c
	implicit none
	integer lu
	real Data(*)
c
c  TrnRead reads back a plane of the reordered cube.
c
c  Input:
c    lu
c  Output:
c    Data
c--
c------------------------------------------------------------------------
	include 'trnio.h'
	integer i,j,k,ktot,ltot,pnt,n1,n2,n3,ifail,offset,length,pd,n3d
	integer work(MAXDIM)
c
	n1 = size(1,lu)
	n2 = size(2,lu)
	n3 = size(3,lu)
c
	p(lu) = p(lu) + 1
c
	n3d = n3
	if(major(lu))then
	  n3d = n2
	  if(pre(lu))n3d = n1
	endif
	if(p(lu).lt.1.or.p(lu).gt.n3d)call bug('f',
     *	  'Written too many planes, in TRNREAD')
c
	if(pre(lu))call trnswap(n1,n2)
	if(flip(3,lu))then
	  if(major(lu))then
	    pd = n2 - p(lu) + 1
	  else
	    pd = n3 - p(lu) + 1
	  endif
	else
	  pd = p(lu)
	endif
c
c  The case of an in memory transpose.
c
	if(inmem(lu))then
	  if(major(lu))then
	    pnt = (pd-1)*n1 + buf(lu)
	    k = 1
	    do j=1,n3
	      do i=1,n1
		Data(k) = ref(pnt)
		pnt = pnt + 1
		k = k + 1
	      enddo
	      pnt = pnt - n1 + n1*n2
	    enddo
c
c  No major step.
c
	  else
	    pnt = (pd-1)*n1*n2 + buf(lu)
	    do i=1,n1*n2
	      Data(i) = ref(pnt)
	      pnt = pnt + 1
	    enddo
	  endif
c
c  Case of a disk-based transpose.
c
	else
	  if(major(lu))then
	    pnt = (pd-1)* n1 * blk(lu)
	    k = 0
	    ktot = n1*n3
	    dowhile(k.lt.ktot)
	      ltot = min(ktot-k,n1*blk(lu))
	      call ScrRead(lScr(lu),Data(k+1),pnt,ltot)
	      pnt = pnt + n2*ltot
	      k = k + ltot
	    enddo
c
c  No major step.
c
	  else
	    length = n1*n2
	    offset = (pd-1)*n1*n2
	    call ScrRead(lScr(lu),Data,offset,length)
	  endif
	endif
c
	if(major(lu))call trnswap(n2,n3)
c
c  Perform the post-transpose, if needed.
c
	if(post(lu))then
	  call transr(Data,n1,n2,work,MAXDIM,ifail)
	  call trnswap(n1,n2)
	  if(ifail.ne.0)call bug('f','Final transpose failed')
	endif
c
c  Perform X and Y swaps if needed.
c
	if(flip(1,lu))call trnflpx(Data,n1,n2)
	if(flip(2,lu))call trnflpy(Data,n1,n2)
c
c  Zero the plane counter when we have finished.
c
	if(p(lu).eq.n3) p(lu) = 0
c
	end
c************************************************************************
c*TrnFin -- Release the resources used by the trn routines.
c:transpose,reorder
c& rjs
c+
	subroutine trnfin(lu)
c
	implicit none
	integer lu
c
c  Release the resources allocated by the trnio routines.
c
c  Input:
c    lu		Handle used by the trnio routines.
c--
c------------------------------------------------------------------------
	include 'trnio.h'
	inuse(lu) = .false.
	if(lScr(lu).ne.0)call ScrClose(lScr(lu))
	if(memsize(lu).ne.0)call MemFree(buf(lu),memsize(lu),'r')
	end
c************************************************************************
	subroutine trnflpx(Data,n1,n2)
c
	implicit none
	integer n1,n2
	real Data(n1,n2)
c
c  Flip pixels in the X direction.
c
c  Input:
c    n1,n2	Image dimensions.
c  Input/Output:
c    Data	Image pixels.
c------------------------------------------------------------------------
	integer i,j
	real tmp
c
	do j=1,n2
	  do i=1,n1/2
	    tmp = Data(i,j)
	    Data(i,j) = Data(n1-i+1,j)
	    Data(n1-i+1,j) = tmp
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine trnflpy(Data,n1,n2)
c
	implicit none
	integer n1,n2
	real Data(n1,n2)
c
c  Flip pixels in the Y direction.
c
c  Input:
c    n1,n2	Image dimensions.
c  Input/Output:
c    Data	Image pixels.
c------------------------------------------------------------------------
	integer i,j
	real tmp
c
	do j=1,n2/2
	  do i=1,n1
	    tmp = Data(i,j)
	    Data(i,j) = Data(i,n2-j+1)
	    Data(i,n2-j+1) = tmp
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine trnswap(n1,n2)
c
	implicit none
	integer n1,n2
c
c  Swap two integers.
c
c  Input/Output:
c    n1,n2	The integers to swap.
c
c------------------------------------------------------------------------
	integer tmp
c
	tmp = n1
	n1 = n2
	n2 = tmp
	end
c************************************************************************
	subroutine trnop(lu,naxis,nsize,reorder)
c
	implicit none
	integer lu,naxis,nsize(naxis)
	character reorder*(*)
c
c  Determine the reordering to be performed.
c
c  Input:
c    lu		Handle used by trnio routines.
c    naxis	Number of axes.
c    reorder	The reordering, such as '123', '231', etc
c------------------------------------------------------------------------
	include 'trnio.h'
	integer i,k,length,idx(MAXNAX),reverse(MAXNAX),xlate(MAXNAX)
	logical flipper(MAXNAX),doflip
c
c  Externals.
c
	integer len1
c
c  Initialise things to do nothing!
c
	do i=1,naxis
	  idx(i) = i
	  flipper(i) = .false.
	  reverse(i) = 0
	enddo
c
c  Decode the reorder parameter.
c
	doflip = .false.
	length = len1(reorder)
	k = 0
	do i=1,length
	  if(reorder(i:i).eq.'-')then
	    doflip = .true.
	  else
	    k = k + 1
	    if(k.gt.naxis)
     *	      call bug('f','Bad reordering parameter')
	    idx(k) = ichar(reorder(i:i)) - ichar('0')
	    if(idx(k).lt.0.or.idx(k).gt.naxis)
     *	      call bug('f','Bad reordering parameter')
	    flipper(k) = doflip.and.nsize(idx(k)).gt.1
	    doflip = .false.
	  endif
	enddo
c
c  Check its a 1-to-1 mapping.
c
	do i=1,naxis
	  reverse(idx(i)) = i
	enddo
	do i=1,naxis
	  if(reverse(i).eq.0)call bug('f',
     *		'Not 1-to-1 mapping, in TRNINI')
	enddo
c
c  Determine which dummy dimensions can be discarded. They must not
c  be either the first or second dimensions (in either input or output).
c  Otherwise all dummy axes can be discarded.
c
	ndim(lu) = 2
	xlate(1) = 1
	xlate(2) = 2
	do i=3,naxis
	  if(idx(i).ge.3.and.nsize(i).eq.1)then
	   xlate(i) = 0
	  else
	    ndim(lu) = ndim(lu) + 1
	    xlate(i) = ndim(lu)
	  endif
	enddo
c
c  Save the size of the thing being transposed.
c
	do i=1,naxis
	  if(xlate(i).gt.0) size(xlate(i),lu) = nsize(i)
	enddo
	do i=ndim(lu)+1,MAXNAX
	  size(i,lu) = 1
	enddo
c
c  Collapse the idx and flipper arrays to be of size ndim.
c
	k = 0
	do i=1,naxis
	  if(xlate(idx(i)).ne.0)then
	    k = k + 1
	    idx(k) = xlate(idx(i))
	    flipper(k) = flipper(i)
	  endif
	enddo
c
c  Determine the operations that we have to perform.
c
	if(ndim(lu).gt.3)call bug('f',
     *	  'Transposition too complex for me, in TRNINI')
c
	pre(lu) = idx(1).eq.2
	flip(1,lu) = flipper(1)
	flip(2,lu) = flipper(2)
	if(ndim(lu).eq.2)then
	  major(lu) = .false.
	  post(lu) = .false.
	  flip(3,lu) = .false.
	else
	  pre(lu) = pre(lu).or.idx(3).eq.1
	  major(lu) = idx(3).ne.3
	  post(lu) = idx(1).eq.3
	  flip(3,lu) = flipper(3)
	endif
c
	end
