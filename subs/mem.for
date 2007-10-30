c************************************************************************
c
c  A set of routines to allocate and free memory. The memory is currently
c  statically declared in blank common, and through dynamic memory allocation
c  routines.
c
c  History:
c    rjs   3apr91 Original version.
c    rjs  18mar92 Sun version. Allocates memory dynamically.
c    rjs  26mar92 Made it generic. Added support for UNICOS and VMS.
c    rjs   3apr92 Added membuf routine.
c    rjs  27jul92 Significant mods to support 'c' and 'd' data types.
c    rjs  14aug92 Doc changes.
c    rjs  10sep92 Serious bug in memalloc, when calling mmAlloc.
c    rjs  24dec92 Doc changes only, for jm.
c    rjs  28jun93 Improvement to membuf routine, to bring it better in
c		  line with the 27jul92 changes.
c************************************************************************
c* MemBuf -- Return suggested memory buffer size.
c& rjs
c: utilities
c+
	integer function MemBuf()
c
	implicit none
c
c  The MemBuf routine returns some suggestion of what is a reasonable
c  memory buffer to allocate. The number returned is in terms of
c  integers. If you want to allocate complex or double precision
c  values, you will probably halve the value returned by MemBuf.
c
c  Output:
c    MemBuf	Size of a "reasonable" integer buffer.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer q
	integer Data(MAXBUF)
	common Data
c
	integer align
	common/MemCom/align
c
c  Initialise the first time.
c
	call MemIni
c
c  Find the biggest hole.
c
	q = 1
	MemBuf = 0
	dowhile(q.gt.0)
	  MemBuf = max(MemBuf,Data(q+1))
	  q = Data(q)
	enddo
c
c  If all the holes are small, return a reasonably sized hole.
c
	MemBuf = (MemBuf/align-1)*align
	MemBuf = max(MemBuf,MAXBUF/3)
	end
c************************************************************************
	subroutine MemIni
c
	implicit none
c
c  An internal routine (not intended to be called by "the general public")
c  which initialises the memory allocation routines.
c------------------------------------------------------------------------
	include 'maxdim.h'
	logical First
	integer Data(MAXBUF)
	common Data
	save First
	integer align
	common/MemCom/align
c
c  Externals.
c
	integer mmSize
c
	data First/.true./
c
	if(.not.First)return
	align = max(2, mmSize(ichar('i')),
     *		       mmSize(ichar('l')),
     *		       mmSize(ichar('r')),
     *		       mmSize(ichar('d')),
     *		       mmSize(ichar('c')))
	Data(1) = 0
	Data(2) = MAXBUF
	First = .false.
	end
c************************************************************************
c* MemAlloc -- Allocate memory
c& rjs
c: utilities
c+
	subroutine MemAlloc(pnt,size,type)
c
	implicit none
	integer pnt,size
	character type*1
c
c  The MemAlloc routine reserves a portion of blank common for its
c  caller. It returns a pointer to the index in blank common of the
c  reserved portion. The caller should declare an array in blank common
c  of size MAXBUF integer elements.
c
c  NOTE: If memalloc is used by any routine, then ALL routines that
c  use blank common MUST go through memalloc/memfree. Otherwise
c  multiple routines may overwrite blank common.
c
c  Typical usage would be something like:
c
c	integer ipnt,rpnt,dpnt
c	include 'maxdim.h'
c	integer iData(MAXBUF)
c	real rData(MAXBUF)
c	double precision dData(MAXBUF/2)
c	common iData
c	equivalence(iData,rData,dData)
c
c	call MemAlloc(ipnt,1000,'i')
c	call MemAlloc(rpnt,1000,'r')
c	call MemAlloc(dpnt,1000,'d')
c
c  After this sequence, 1000 integers are available from iData(ipnt),
c			1000 reals    are available from rData(rpnt), and
c			1000 doubles  are available from dData(dpnt).
c
c  Input:
c    type	The data type. Possible values are:
c		  'i'	Integer.
c		  'r'	Real.
c		  'l'	Logical.
c		  'd'	Double precision.
c		  'c'	Complex.
c    size	The number of elements to allocate.
c  Output:
c    pnt	Pointer to location in blank common, where the memory is.
c		This is a FORTRAN index of an array of the appropriate
c		type. That is, for double precision values, it will
c		be an index into a double precision array.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer p,q,nc,sized,pntd,elsize
	integer Data(MAXBUF)
	common Data
c
	integer align
	common/MemCom/align
c
c  Externals.
c
	integer mmAlloc,mmSize
c
c  Initalise, if needed.
c
	call MemIni
c
c  Find a block of memory that is big enough.
c
	if(size.le.0)
     *	  call bug('f','Bad value for size, in MemAlloc')
	q = 1
	p = 0
	nc = align
	elsize = mmSize(ichar(type))
	sized = ( (size*elsize-1)/align + 1 ) * align
c
	dowhile(q.gt.0.and.Data(q+1).lt.sized+nc)
	  nc = 0
	  p = q
	  q = Data(q)
	enddo
c
c  We do not have a chunk of memory big enough. Allocate it using mmalloc.
c
	if(q.eq.0)then
	  pntd = mmAlloc(Data,sized)
	  if(pntd.eq.0)call bug('f','Unable to allocate memory')
c
c  We have a big enough bit of memory.
c
	else if(Data(q+1).ge.sized+2)then
	  Data(q+1) = Data(q+1) - sized
	  pntd = q + Data(q+1)
	else
	  pntd = q
	  Data(p) = Data(q)
	endif
c
	pntd = pntd - 1
	if(mod(pntd,elsize).ne.0)
     *	  call bug('f','Alignment error in memAlloc')
	pnt = pntd / elsize + 1
c
	end
c************************************************************************
c* MemFree -- Free allocated memory.
c& rjs
c: utilities
c+
	subroutine MemFree(pnt,size,type)
c
	implicit none
	integer pnt,size
	character type*1
c
c  This frees up a portion of blank common, previously allocated by
c  MemAlloc.
c
c  Inputs:
c    pnt	Pointer to the memory in blank common.
c    size	The number of elements to free.
c    type	The data type. Possible values are:
c		  'i'	Integer.
c		  'r'	Real.
c		  'l'	Logical.
c		  'd'	Double precision.
c		  'c'	Complex.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer p,q,sized,pntd,elsize
	integer Data(MAXBUF)
	common Data
c
	integer align
	common/MemCom/align
c
c  Externals.
c
	integer mmSize
c
c  Check.
c
	if(size.le.0)
     *	  call bug('f','Bad value for size, in MemFree')
	elsize = mmSize(ichar(type))
	pntd = (pnt-1)*elsize + 1
	sized = ( (size*elsize-1)/align + 1 ) * align
c
c  Free memory which was obtained with mmalloc.
c
	if(pntd.le.0.or.pntd.gt.MAXBUF)then
	  call mmfree(Data(pntd))
c
c  Frr memory which was obtained from the common block.
c  Search the free list for the place to add this bit.
c
	else
	  if(pntd+sized-1.gt.MAXBUF)
     *	    call bug('f','Bad value for pnt or size, in MemFree')
	  q = 1
	  p = 0
	  dowhile(q.lt.pntd.and.q.gt.0)
	    p = q
	    q = Data(q)
	  enddo
c
c  Various checks.
c
	  if(p.eq.0)call bug('f','Internal bug in MemFree')
	  if(q.gt.0.and.pntd+sized.gt.q) call bug('f',
     *	    'Deallocating a deallocated part of memory, in MemFree')
	  if(p+Data(p+1).gt.pntd) call bug('f',
     *	    'Deallocating a deallocated part of memory, in MemFree')
c
c  Insert this bit into the free list.
c
	  Data(pntd) = q
	  Data(pntd+1) = sized
	  Data(p) = pntd
c
c  Can we concatentate this bit with the following bit?
c
	  if(pntd+sized.eq.q)then
	    Data(pntd) = Data(q)
	    Data(pntd+1) = Data(pntd+1) + Data(q+1)
	  endif
c
c  Can we concatenate this bit with the previous bit?
c
	  if(p+Data(p+1).eq.pntd)then
	    Data(p) = Data(pntd)
	    Data(p+1) = Data(p+1) + Data(pntd+1)
	  endif
c
	endif
c	    
	end
