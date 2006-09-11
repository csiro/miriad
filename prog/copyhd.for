c************************************************************************
	program copyhd
	implicit none
c
c= COPYHD - Copy items from one data-set to another.
c& rjs
c: utility
c+
c	COPYHD is a Miriad task which copies items from one Miriad data-set
c	to another. There is no interpretation of the items at all.
c@ in
c	Name of the input data set. No default.
c@ out
c	Name of the output data set. This must already exist. No default.
c@ items
c	A list of items to be copied across. At least one value must be
c	given.
c--
c  History:
c    rjs  29jan91  Original version.
c    rjs  25apr94  Corrected call sequence to hclose.
c------------------------------------------------------------------------
	character version*(*)
	integer maxitems
	parameter(version='version 1.0 25-Apr-94')
	parameter(maxitems=32)
c
	character items(maxitems)*16,in*64,out*64
	integer tIn,tOut,nitems,iostat,i
c
c  Externals.
c
	logical hdprsnt
c
c  Get the inputs.
c
	call output('Copyhd: '//version)
	call keyini
	call keya('in',in,' ')
	call keya('out',out,' ')
	call mkeya('items',items,maxitems,nitems)
	call keyfin
c
c  Check the inputs.
c
	if(in.eq.' ')call bug('f','Input must be given')
	if(out.eq.' ') call bug('f','Output must be given')
	if(nitems.eq.0)
     *	  call bug('f','A list of items to copy must be given')
c
c  Open the inputs and the output.
c
	call hopen(tIn,in,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening input: '//in)
	  call bugno('f',iostat)
	endif
	call hopen(tOut,out,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output: '//out)
	  call bugno('f',iostat)
	endif
c
c  Loop through the list of items to copy. Check there existence in both
c  the input and the output, and give warning if they fail to exist, or are
c  being overwritten.
c
	do i=1,nitems
	  if(.not.hdprsnt(tIn,items(i)))then
	    call bug('w','Item missing from input: '//items(i))
	  else
	    if(hdprsnt(tOut,items(i)))
     *	      call bug('w','Overwriting item in output: '//items(i))
	    call hdcopy(tIn,tOut,items(i))
	  endif
	enddo
c
c  Now add some history to the output.
c
	call hisopen(tOut,'append')
	call hiswrite(tOut,'COPYHD: Miriad Copyhd, '//version)
	call hisinput(tOut,'COPYHD')
	call hisclose(tOut)
c
c  All said and done. Close up.
c
	call hclose(tIn)
	call hclose(tOut)
	end
