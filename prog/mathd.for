c************************************************************************
	program mathd
	implicit none
c
c= MATHD -- Perform mathematical operations on items.
c& rjs
c: utility
c+
c	MATHD is a MIRIAD task which performs mathematical operations on
c	items in a Miriad data-set. The items can be either a single value,
c	or a ``vector'' of values.
c@ in
c	Name of the input data-set. No default.
c@ item
c	Name of the output item, in the data set. No default.
c@ exp
c	The expression to compute. This is an expression in FORTRAN-like
c	syntax (like MATHS), 
c--
c  History:
c    pjt          Original version.
c    rjs  15apr91 Total rewrite.
c    rjs  23apr91 Fixed char*(*) in subroutine arg string concatenation.
c		  Corrected hclose call sequence.
c------------------------------------------------------------------------
	include 'mathd.h'
	character version*(*)
	integer BufLen,RBufLen
	parameter(version='Mathd: version 1.0 23-Apr-91')
	parameter(BufLen=64,RBufLen=5*MAXLEN)
c
	integer iostat
	character in*64,item*12,exp*80,descr*16,dtype*16
	real ExprBuf(RBufLen)
	integer ExpBuf(BufLen),pnt,i,n,type
c
c  Externals.
c
	external PACTION,VACTION
c
c  Get the inputs.
c
	call output(version)
	call keyini
	call keya('in',in,' ')
	if(in.eq.' ')call bug('f','Input data-set name must be given')
	call keya('item',item,' ')
	if(item.eq.' ')call bug('f','Output item name must be given')
	call keya('exp',exp,' ')
	if(exp.eq.' ')call bug('f','An expression must be given')
	call keyfin
c
c  Open the data-set.
c
	call hopen(tIn,in,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening '//in)
	  call bugno('f',iostat)
	endif
c
c  Determine the data-type of the output item.
c
	typo = ' '
	call hdprobe(tIn,item,descr,dtype,n)
	if(n.ne.0)then
	  if(dtype.eq.'integer'.or.dtype.eq.'real'.or.
     *	    dtype.eq.'double')then
	    call bug('w','Overwriting item '//item)
	    typo = dtype
	  else
	    call bug('f','Cannot overwrite item '//item)
	  endif
	endif
c
c  Parse and evaluate the input expression.
c
	nItems = 0
	Size = 0
	call ariComp(exp,PACTION,type,ExpBuf,Buflen,ExpRBuf,RBuflen)
	if(type.eq.ERROR)call bug('f','Error parsing the expression')
	if(nItems.eq.0)Size = 1
	if(Size.gt.MAXLEN)call bug('f','Items are too big to handle')
	call ariExec(VACTION,Size,ExpBuf,BufLen,ExpRBuf,RBufLen,pnt)
	if(typo.eq.' ')typo = 'r'
c
c  Close up all the input items.
c
	do i=1,nItems
	  call hdaccess(items(i),iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error closing one of the input items')
	    call bugno('f',iostat)
	  endif
	enddo
c
c  Finally write out the item.
c
	call ItWrite(tIn,item,ExpRBuf(pnt),Size,typo)
c
c  Add some history.
c
	call hisopen(tIn,'append')
	call hiswrite(tIn,'MATHD: Miriad '//version)
	call hisinput(tIn,'MATHD')
	call hisclose(tIn)
c
c  All said and done.
c
	call hclose(tIn)
	end
c************************************************************************
	subroutine ItWrite(tno,item,Data,n,type)
c
	implicit none
	integer tno,n
	character type*1,item*(*)
	real Data(n)
c
c  Write out an item.
c
c  Input:
c    tno	Handle of the data set.
c    item	Name of the item to write out.
c    Data	Data to write out.
c    n		Number of elements.
c    type	Either 'i','r' or 'd', to write out the data as
c		integer,real or double.
c------------------------------------------------------------------------
	include 'mathd.h'
	integer it,iostat
	character line*64
c
	call haccess(tno,it,item,'write',iostat)
	if(iostat.ne.0)then
	  line = 'Error writing item '//item
	  call bug('w',line)
	  call bugno('f',iostat)
	endif
	if(typo.eq.'r')then
	  call hwritei(it,VALR,0,4,iostat)
	  if(iostat.eq.0)call hwriter(it,data,OFFR,SIZER*n,iostat)
	else if(typo.eq.'d')then
	  call cvtrd(data,bufd,n)
	  call hwritei(it,VALD,0,4,iostat)
	  if(iostat.eq.0)call hwrited(it,bufd,OFFD,SIZED*n,iostat)
	else if(type.eq.'i')then
	  call cvtri(data,bufi,n)
	  call hwritei(it,VALI,0,4,iostat)
	  if(iostat.eq.0)call hwritei(it,bufi,OFFI,SIZEI*n,iostat)
	else
	  call bug('f','Bad type, in ItWrite')
	endif
c
	if(iostat.eq.0)call hdaccess(it,iostat)
	if(iostat.ne.0)then
	  call bug('w','I/O error writing out the item')
	  call bugno('f',iostat)
	endif
	end
c************************************************************************
	subroutine cvtrd(in,out,n)
c
	implicit none
	integer n
	real in(n)
	double precision out(n)
c
c  Convert reals to doubles.
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  out(i) = in(i)
	enddo
	end
c************************************************************************
	subroutine cvtri(in,out,n)
c
	implicit none
	integer n
	real in(n)
	integer out(n)
c
c  Convert reals to integers.
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  out(i) = in(i)
	enddo
	end
c************************************************************************
	subroutine cvtdr(in,out,n)
c
	implicit none
	integer n
	double precision in(n)
	real out(n)
c
c  Convert doubles to reals.
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  out(i) = in(i)
	enddo
	end
c************************************************************************
	subroutine cvtir(in,out,n)
c
	implicit none
	integer n
	integer in(n)
	real out(n)
c
c  Convert integers to reals
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  out(i) = in(i)
	enddo
	end
c************************************************************************
	subroutine PACTION(item,type,indx,value)
c
	implicit none
	character item*(*)
	integer type,indx
	real value
c
c  This returns to the parser the characteristics of those items that
c  appear in the expression.
c
c  Input:
c    symbol	Name of the item of interest.
c  Output:
c    type	Either ERROR, CONSTANT or VECTOR
c    indx	For VECTOR, this is an index into the table holding the
c		item handles.
c    value	For CONSTANT, this is the value of the constant.
c------------------------------------------------------------------------
	include 'mathd.h'
	integer n,iostat
	character dtype*16,descr*32,line*64
c
c  Determine the characteristics of the item.
c
	call hdprobe(tIn,item,descr,dtype,n)
	type = ERROR
c
	if(dtype.ne.'integer'.and.dtype.ne.'real'.and.
     *	  dtype.ne.'double')then
	  line = 'Non-existent or non-numeric item '//item
	  call bug('w',line)
	else if(n.eq.1)then
	  if(typo.eq.' ')typo = dtype
	  type = CONSTANT
	  call rdhdr(tIn,item,value,0.)
c
c  An item of many elements.
c
	else if(nItems.ge.MaxItems)then
	  call bug('w','Too many items')
	else if(nItems.gt.0.and.n.ne.size)then
	  call bug('w','Inconsistent number of elements in the items')
	else
	  if(typo.eq.' ')typo = dtype
	  type = VECTOR
	  Size = n
	  nItems = nItems + 1
	  call haccess(tIn,Items(nItems),item,'read',iostat)
	  if(iostat.ne.0) call bug('f','This cannot happen, in PACTION')
	  types(nItems) = dtype
	  Indx = nItems
	endif
	end
c************************************************************************
	subroutine VACTION(Indx,type,Data,n)
c
	implicit none
	integer Indx,type,n
	real Data(n)
c
c  This reads the values of items for the evaluation routine.
c
c  Inputs:
c    Indx	Index into the "Items" table, to get the items handle.
c    type	Will be VECTOR.
c    n		Number of elements. Will be the same as Size.
c  Output:
c    Data	The value of the item.
c------------------------------------------------------------------------
	include 'mathd.h'
	integer iostat
c
	if(types(Indx).eq.'r')then
	  call hreadr(Items(Indx),Data,OFFR,SIZER*n,iostat)
	else if(types(Indx).eq.'d')then
	  call hreadd(Items(Indx),Bufd,OFFD,SIZED*n,iostat)
	  call cvtdr(Bufd,Data,n)
	else if(types(Indx).eq.'i')then
	  call hreadi(Items(Indx),BufI,OFFI,SIZEI*n,iostat)
	  call cvtir(Bufi,Data,n)
	else
	  call bug('f','Unrecognised type, in VACTION')
	endif
	if(iostat.ne.0)then
	  call bug('w','I/O error reading an item')
	  call bugno('f',iostat)
	endif
	end
