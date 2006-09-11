c************************************************************************
	program gethd
	implicit none
c= GetHd -- Print the value of a header item.
c& rjs
c: miscellaneous
c	GetHd simply prints the value of a header item. Its main use will
c	be in scripts, where it can be used to extract information from
c	the image header.
c@ in
c	The name of an item within a dataset. This is given in the form
c	  in=dataset/item
c	There is no default. The dataset may be either a uv or image data-set. 
c@ log
c	Output log file. Default is the terminal.
c--
c  History:
c    rjs  07dec95 Original version (supercedes some imhead functionality).
c------------------------------------------------------------------------
	character in*128,item*32,logf*64,descr*32,type*16
	logical more
	integer n,lIn,iostat,ltype,ldescr
c
c  Externals.
c
	character itoaf*8
	integer len1
c
c  NOTE: we do not give a version line, as this would clutter the output
c  that is presumably being piped or captured.
c
	call keyini
	call keya('in',in,' ')
	call keya('log',logf,' ')
	call keyfin
	if(in.eq.' ')call bug('f','Input name is missing')
c
c  Get the item and open the file.
c
	call getitem(in,item)
	call hopen(lin,in,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening the data-set '//in)
	  call bugno('f',iostat)
	endif
c
	call logOpen(Logf,' ')
	call hdprobe(lin,item,descr,type,n)
	if(n.gt.1)then
	  descr = itoaf(n)
	  ldescr = len1(descr)
	  ltype  = len1(type)
	  descr(ldescr+1:) = ' '//type(1:ltype)//' values'
	endif
	if(n.gt.0)call logWrite(descr,more)
	call logClose
	call hclose(lin)
	end
c************************************************************************
	subroutine GetItem(in,item)
c
	implicit none
	character in*(*),item*(*)
c
c  Extract the trailing item specification from a data-set name.
c  Remove this from `in', and return it in `item'.
c
c  Input/Output:
c    in		Name of the dataset/item. The return is just the name of
c		the dataset.
c  Output:
c    item	Name of the item.
c
c------------------------------------------------------------------------
	integer k1,k2
	logical more
c
c  Externals.
c
	integer len1
c
	k2 = len1(in)
	k1 = k2
	more = .true.
	dowhile(k1.gt.0.and.more)
	  more = in(k1:k1).ne.'/'
	  if(more) k1 = k1 - 1
	enddo
c
	if(k1.eq.k2) call bug('f','Bad name/item specification')
	item = in(k1+1:k2)
	in(k1:k2) = ' '
c
	end
