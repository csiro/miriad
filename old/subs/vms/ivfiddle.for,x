c************************************************************************
	subroutine ivfiddle
	implicit none
c
c  Perform a number of "local" functions on the IVAS.
c
c------------------------------------------------------------------------
	include 'ivas.h'
	integer gap,ppc,height
	parameter(gap=12,ppc=18,height=40)
	integer x0,y0,y1,tvchan,Zoom,xCenter,yCenter,button
	integer x,y,newchan
	character op*8,LUT*8
c
c  Reset the ivas as much as we dare.
c
	call fivasGPHset(0,0,1024,1024,0)
	call IvReset
 	Zoom = 1
	xCenter = 512
	yCenter = 512
	LUT = ' '
	tvchan = 0
c
c  Make the menu.
c
	call Ivmenu_init
	x0 = 1004 - 6*ppc
	y0 = 1020 - height
	y1 = y0 - height - gap
	call Ivmenu_item(x0,y0,6*ppc,height,'Cursor','bls')
	call Ivmenu_item(x0,y1,6*ppc,height,'Blink','bls')
	x0 = x0 - 8*ppc - gap
	call Ivmenu_item(x0,y0,8*ppc,height,'Fiddle','bls')
	call Ivmenu_item(x0,y1,8*ppc,height,'Zoom/Pan','bls')
	x0 = x0 - 6*ppc - gap
	call Ivmenu_item(x0,y0,6*ppc,height,'Table','bs')
	call Ivmenu_label('Table','B&W')
	call Ivmenu_item(x0,y1,6*ppc,height,'Hide','bls')
	x0 = x0 - 3*ppc - gap
	call Ivmenu_item(x0,y0,3*ppc,height,'2','bls')
	call Ivmenu_item(x0,y1,3*ppc,height,'*','bls')
	x0 = x0 - 3*ppc - gap
	call Ivmenu_item(x0,y0,3*ppc,height,'1','bls')
	call Ivmenu_item(x0,y1,3*ppc,height,'3','bls')
	x0 = x0 - 5*ppc - gap
	call Ivmenu_item(x0,y0,5*ppc,height,'Exit','bls')
	call Ivmenu_item(x0,y1,5*ppc,height,'Reset','bls')
c
	x0 = x0 - gap
	call Ivmenu_item(20,y0,x0-19,height,'coords','b')
	call Ivmenu_item(20,y1,x0-19,height,'TVALL','bl')
c
c  Until we get a selection of exit, loop.
c
	op = ' '
	do while(op.ne.'Exit')
	  call Ivmenu_select(op)
c
c  Flick the menu off then on, so someone can take a photo.
c
	  if(op.eq.'*'.or.op.eq.'Hide')then
	    call fivasGMdefGraphic(1,GMdisable,0,0)
	    call fivasGMdefGraphic(2,GMdisable,0,0)
	    call fivasGPHblinkCursor(0,1)
	    call fivasMOUSEstatus(button,x,y,0,2)
c
c The number 76 = '4C'x is orange, 3840 = 'F00'x is blue.
c
	    call fivasGMdefGraphic(1,GMcolor,76,0)
	    call fivasGMdefGraphic(2,GMcolor,3840,0)
	    call fivasGPHblinkCursor(1,0)
c
c  Reset as much as possible.
c
	  else if(op.eq.'Reset')then
	    call IvReset
	    LUT = 'B&W'
	    call Ivset_lut(lut)
	    Zoom = 1
	    xCenter = 512
	    yCenter = 512
c
c  Set the colour lookup table.
c
	  else if(op.eq.'Table')then
	    if(Lut.eq.' '.or.Lut.eq.'IRAF')then
	      Lut = 'B&W'
	    else if(Lut.eq.'B&W')then
	      Lut = 'Colour'
	    else if(Lut.eq.'Colour')then
	      Lut = 'Tonry'
	    else
	      Lut = 'IRAF'
	    endif
	    call Ivset_lut(lut)
	  else if(op.eq.'Fiddle')then
	    call IvModOfm(LUT)
	  else if(op.eq.'Blink')then
	    call Ivblink(tvchan)
	  else if(op.eq.'Zoom/Pan')then
	    call IvZoomPan(Zoom,xCenter,yCenter)
	  else if(op.eq.'Cursor')then
	    call IvCursor
c
c  Switch to a new channel.
c
	  else if(op.eq.'1'.or.op.eq.'2'.or.op.eq.'3')then
	    newchan = ichar(op)-ichar('0')
	    if(tvchan.ne.newchan)call Ivsetchan(tvchan,newchan)
	  endif
	enddo
c
c  Get rid of the menu.
c
	call Ivmenu_fin
	end
c************************************************************************
	subroutine Ivset_lut(lut)
c
	implicit none
	character lut*(*)
c
c  Set the lut table to be used.  First change the label on the menu,
c  then modify the lookup table.
c
c------------------------------------------------------------------------
	character lutd*6
	integer i,j
c
	integer len1
c
	lutd = ' '
	j = len1(Lut)
	i = (len(Lutd) - j)/2
	j = i + j
	i = i + 1
	lutd(i:j) = lut
	call Ivmenu_label('Table',lutd)
	call Ivofm(lut,0,255)
	end
c************************************************************************
	subroutine IvCursor
c
	implicit none
c
c  Allow the user to roam a cursor across the IVAS screen, and return
c  the IVAS pixel location (not the image pixel location) that is being
c  pointed to.
c
c------------------------------------------------------------------------
	character text*11
	integer button,x,y,xStart,yStart,xZoom,yZoom
c
	call fivasGPHvalue(0,1)
	call fivasVPrdZoomScroll(xStart,yStart,xZoom,yZoom,1)
	call fivasMOUSEstatus(button,x,y,1,0)
	dowhile(button.eq.0)
	  x = x / xZoom + xStart
	  y = 1023 - (y / yZoom + yStart)
	  write(text,'(a,i4,a,i4,a)')'(',x,',',y,')'
	  call Ivmenu_label('coords',text)
	  call fivasMOUSEstatus(button,x,y,1,3)
	enddo
	end
c************************************************************************
	subroutine IvReset
c
c  Reset as many things as I dare, but do not clear image planes and
c  graphics planes.
c
c------------------------------------------------------------------------
	include 'ivas.h'
	call fivasMOUSEreset
	call fivasMOUSEput(512,512,3)
	call fivasVPzoomScroll(0,0,1,1,1)
	call fivasVPzoomScroll(0,0,1,1,2)
	call fivasGMdefGraphic(1,GMcolor,76,0)
	call fivasGMdefGraphic(2,GMcolor,3840,0)
	call fivasGPHorigin(0,0)
	call fivasGPHvalue(0,1)
	call fivasVPdepth(8)
	call fivasVPenable(1)
	call fivasCSshape(7,24,6,0)
	call fivasCScolor(76)
	call fivasGPHblinkCursor(1,0)
	end
c************************************************************************
	subroutine IvZoomPan(Zoom,xCenter,yCenter)
c
	implicit none
	integer Zoom,xCenter,yCenter
c
c  Zoom and Pan the TV.
c
c  Input:
c    Zoom	Zoom factor.
c    xCenter,yCenter Central pixel being displayed.
c
c  Mouse Usage:
c    Right button:   Finish up
c    Middle button:  Decrement zoom.
c    Left button:    Increment zoom.
c    Mouse Position. Determines the place to be (under the sea in an octopuses
c		     garden).
c
c------------------------------------------------------------------------
	integer xStart,yStart,button
c
	call fivasMOUSEput(xCenter,yCenter,3)
	call fivasMOUSEstatus(button,xCenter,yCenter,3,0)
	button = 0
c
c  Wait for a button to be hit, or the mouse to be move.
c
	do while(mod(button,2).eq.0)
	  call fivasMOUSEstatus(button,xCenter,yCenter,3,3)
	  if(mod(button/2,2).ne.0)Zoom = Zoom - 1
	  if(mod(button/4,2).ne.0)Zoom = Zoom + 1
	  Zoom = max(1,min(16,Zoom))
	  xStart = xCenter - 512/Zoom
	  yStart = 1024 - yCenter - 512/Zoom
	  call fivasVPzoomScroll(xStart,yStart,Zoom,Zoom,1)
	enddo
	end
c************************************************************************
	subroutine Ivblink(channel)
c
	implicit none
	integer channel
c
c  Blink between two planes at a rate determined by the y position of the
c  mouse. Exit if any button is hit.
c
c  Input:
c    channel	The current channel being displayed.
c------------------------------------------------------------------------
	integer button,x,y,i,old,new
c
	button = 0
	old = channel
	new = channel
c
c  Wait for something to happen.
c
	do while(button.eq.0)
	  if(new.eq.channel)then
	    new = mod(channel,3)+1
	  else
	    new = channel
	  endif
c
	  call Ivsetchan(old,new)
	  call fivasMOUSEstatus(button,x,y,3,0)
	  y = (1023-y)**2/1024
	  i = 1024
	  do while(i.gt.y.and.button.eq.0)
	    call lib$wait(0.01)
	    call fivasMOUSEstatus(button,x,y,3,0)
	    i = i - 4
	  enddo
	enddo
	call Ivsetchan(old,channel)
	end
c************************************************************************
	subroutine IvsetChan(old,new)
c
	implicit none
	integer old,new
c
c  Set the TV channel. Do the highlighting right too.
c
c  Input:
c    new	Channel to be displayed.
c  Input/Output:
c    old	Previous channel displayed. This is updated to new.
c------------------------------------------------------------------------
	character c*1
	if(old.eq.new.or.new.eq.0)return
c
c  Remove the high-light off the old channel.
c
	if(old.gt.0)then
	  call fivasGPHvalue(0,1)
	  c = char(old+ichar('0'))
	  call Ivmenu_label(c,c)
	endif
c
c  High-light the new channel.
c
	call fivasGPHvalue(0,2)
	c = char(new+ichar('0'))
	call Ivmenu_label(c,c)
c
c  Finally switch channels.
c
	call fivasVPsetup(new-1,new-1,new-1,31)
	old = new
	end
c************************************************************************
	subroutine IvModOfm(LUT)
c
	implicit none
	character LUT*(*)
c
c  This sets up the colour lookup tables of the IVAS display.
c  This exits if any button is hit.
c
c  Input:
c    LUT	Lookup-table to be loaded.
c
c------------------------------------------------------------------------
	integer button,beg,fin,x,y,oldx,oldy
c
	button = 0
	x = 512
	y = 512
	oldx = 0
	oldy = 0
	call fivasMOUSEput(x,y,3)
c
c  Wait for something to happen.
c
	do while(button.eq.0)
c
c  Load the colour if necessary.
c
	  if(x.ne.oldx.or.y.ne.oldy)then
	    beg = min(max((x-y/2)/4,0),255)
	    fin = max(min((x+y/2)/4,255),0)
	    call IvOfm(LUT,beg,fin)
	  else
	    call lib$wait(0.1)
	  endif
c
c  Check if we are at an end.
c
	  oldx = x
	  oldy = y
	  call fivasMOUSEstatus(button,x,y,3,3)
	enddo
	end
