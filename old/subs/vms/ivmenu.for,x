c************************************************************************
	subroutine Ivmenu_fin
c
c  Remove the menu from the screen and forget all about it.
c
c------------------------------------------------------------------------
	include 'ivas.h'
	include 'Ivmenu.h'
	nfields = 0
	call fivasGMdefGraphic(1,GMdisable,0,0)
	call fivasGMdefGraphic(2,GMdisable,0,0)
	call fivasGPHblinkCursor(0,1)
	end
c************************************************************************
	subroutine Ivmenu_init
c
c  Get ready to define a menu.
c------------------------------------------------------------------------
	include  'Ivmenu.h'
	nfields = 0
	end

c************************************************************************
	subroutine Ivmenu_select(Op)
c
	implicit none
	character Op*(*)
c
c  Allow the user to select one of the labels with the cursor.
c
c  Output:
c    Op		The tags of the panel selected.
c------------------------------------------------------------------------
	include 'Ivmenu.h'
	integer button,xPos,yPos,i,i0
	logical found
c
c  Enable the cursor, and wait for a button to be hit.
c
	call fivasMOUSEcursor(1)
c
c  Loop until the cursor selects one of the menu items.
c
	found = .false.
	do while(.not.found)
	  call fivasMousestatus(button,xPos,yPos,7,2)
	  i0 = 0
	  do i=1,nfields
	    if(xPos.ge.Coord(1,i).and.yPos.ge.Coord(2,i).and.
     *	     xPos.le.Coord(3,i).and.yPos.le.Coord(4,i))i0 = i
	  enddo
	  if(i0.ne.0)then
	    found = select(i0)
	  else
	    found = .false.
	  endif
	enddo
c
c  The user has selected something. Return it.
c
	op = tags(i0)
	end
c************************************************************************
	subroutine Ivmenu_item(x,y,width,height,op,flags)
c
	implicit none
	character flags*(*),Op*(*)
	integer x,y,width,height
c
c  Add a new panel item.
c
c  Input:
c    x,y	Pixel location of the centre if the item.
c    width,height Width and height of the item.
c    op		Name of the item.
c    flags	A combination of 'b', 'l' and 's'.
c		's' means "selectable by the user"
c		'l' means label it.
c		'b' means "draw a border".
c
c------------------------------------------------------------------------
	include 'Ivmenu.h'
	integer x1,y1,x2,y2,radius
c
c  Calculate the box.
c
	x1 = x
	y1 = y
	x2 = x + width - 1
	y2 = y + height - 1
c
c  Check validity of the thing.
c
	if(nfields.ge.maxfield)
     *	  call bug('f','Too many menu items')
	if(height.le.0.or.width.le.0)
     *	  call bug('f','Negative size for menu item')
	if(x1.lt.0.or.x2.ge.1024.or.y1.lt.0.or.y2.ge.1024)
     *	  call bug('f','Bad coordinate or size information')
c
c  Save the information.
c
	nfields = nfields + 1
	coord(1,nfields) = x1
	coord(2,nfields) = y1
	coord(3,nfields) = x2
	coord(4,nfields) = y2
	tags(nfields) = Op
	select(nfields) = op.ne.' '.and.index(flags,'s').ne.0
c
c  Add a border if need be.
c
	if(index(flags,'b').ne.0)then
	  call fivasGPHpattern(0,0,1,1,0,0,0,0,1,1,0)
	  radius = min(width,height)/4
	  call fivasGPHmove(x1+radius,y1,0)
	  call fivasGPHarc(x1+radius,y1+radius,x1,y1+radius,1,0)
	  call fivasGPHline(x1,y2-radius,0)
	  call fivasGPHarc(x1+radius,y2-radius,x1+radius,y2,1,0)
	  call fivasGPHline(x2-radius,y2,0)
	  call fivasGPHarc(x2-radius,y2-radius,x2,y2-radius,1,0)
	  call fivasGPHline(x2,y1+radius,0)
	  call fivasGPHarc(x2-radius,y1+radius,x2-radius,y1,1,0)
	  call fivasGPHline(x1+radius,y1,0)
	endif
c
c  Label the bugger, if needed.
c
	if(op.ne.' '.and.index(flags,'l').ne.0)
     *	  call Ivmenu_label(op,Op)
	end
c************************************************************************
	subroutine Ivmenu_label(op,text)
c
	implicit none
	character text*(*),op*(*)
c
c  Draw some text for a menu item.
c
c------------------------------------------------------------------------
	include 'Ivmenu.h'
	integer xo,yo,i,i0
c
c  Find the coordinates for this item.
c
	i0 = 0
	do i=1,nfields
	  if(tags(i).eq.Op)i0 = i
	enddo
	if(i0.eq.0)return
c
c  Put the label of the plot.
c
	xo = (coord(1,i0)+coord(3,i0))/2
	yo = (coord(2,i0)+coord(4,i0))/2
	call Ivtext(xo,yo,1,text)
	end
