c************************************************************************
	PROGRAM delhd
	implicit none
c= delhd - Delete a dataset item.
c& rjs
c: utility
c+
c   DELHD is a MIRIAD task to delete an item from a MIRIAD dataset.
c
c   For image datasets items are not only header variables, such as
c   ``crpix1'', ``datamin'' etc., but also the ``mask'' and ``image''
c   items.
c   For visibility datasets examples of items are ``wflags'', ``gains''
c   and ``pdata'', but NOT the uv variables. They cannot be deleted.
c   See also copyhd, itemize, mathd and puthd.
c@ in
c   The name of an item within a dataset. This is given in the form:
c	  % delhd in=dataset/item
c--
c  History:
c    rjs  27apr90 Original version.
c    pjt  28jul91 added history comments to the dataset, improved output
c    rjs  25apr94 Changed call sequence to hclose.
c------------------------------------------------------------------------
        character PVERSION*(*)
        parameter (PVERSION='Version 1.0 25-Apr-94')
	integer lin,iostat
	character in*80,item*16
c
	call output( 'DELHD: '//PVERSION)
c
	call keyini
	call keya('in',in,' ')
	call keyfin
	if(in.eq.' ')call bug('f','Input name is missing (in=)')
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
	call hdelete(lin,item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error deleting item '//item)
	  call bugno('f',iostat)
	endif
c
        call hisopen(lin,'append')
        call hiswrite(lin,'DELHD: Miriad DelHd: '//PVERSION)
        call hisinput(lin,'DELHD')
        call hisclose(lin)
c
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
