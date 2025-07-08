c************************************************************************
       program tvall
       implicit none

c= tvall - Interact with a previously displayed image
c& jm
c: visual display
c+
c	TVALL is a WERONG/MIRIAD task which allows interactive
c	modification of the TV lookup tables, etc. Usually this would be
c	used to manipulate an image previously loaded with TVDISP.
c	A menu for operations
c	to be performed will appear on the TV screen. The following are
c	possible: 
c
c	Choose the channel being displayed (Select the channel number
c	from the menu). 
c
c	Blink between two channels (The mouse y-coordinate determines
c	the blink rate. It blinks between the current and succeeding
c	image, mod 3. Hit any button to stop blinking). 
c
c	Fiddle the lookup tables (The transfer function is governed by
c	the mouse position. Hit any button to finish the command). 
c
c	Change the lookup tables between black and white, or colour. 
c
c	Zoom and pan (the left button zooms in, the middle button zooms
c	out, and the mouse position controls pan. The cursor drawn on
c	the IVAS gives the position, on the unzoomed screen, of the
c	central pixel currently being displayed. To finish, hit the
c	right button.). 
c
c	Switch the menu off (by `selecting' the asterisk). This is
c	useful if you want to take a photograph of the screen. 
c
c	Return cursor position. The coordinates given are coordinates
c	(not image coordinates). 
c@ server
c	The TV device. No default. See the Users Manual for information
c	on how to specify this.
c--
c------------------------------------------------------------------------
      character server*32
      call output( 'Tvall: version 1.0' )
      call keyini
      call keya('server',server,' ')
      call keyfin
      call tvopen(server)
      call tvlocal
      call tvclose
c
      end
