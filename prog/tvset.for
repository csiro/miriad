c************************************************************************
	program tvset
	implicit none
c
c= tvset - Set up and initialize the TV device.
c& rjs
c+
c	TVSET either sets the lookup table, or clears the TV device.
c< server
c	No default.
c@ table
c	This gives the name of the operation to perform, or look-up
c	table to load. Possible values are:
c	  'clear'    Reset the TV device.
c	  'grey'     Black and white look-up table.
c	  'colour'   Colour look-up table.
c	  'rainbow'  Another colour look-up table.
c--
c  History:
c    rjs  10sep90 Original version.
c    rjs   1jul91 Added in-line doc.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='TvSet: version 1.0  1-Jul-91')
	character server*32,op*16
c
c  Get the inputs.
c
	call output(version)
	call keyini
	call keya('server',server,' ')
	call keya('table',op,' ')
	call keyfin
c
c  Check the inputs.
c
	if(server.eq.' ')call bug('f','The server must be givien')
c
c  Open up the TV device, and do the appropriate operation.
c
	call tvopen(server)
	if(op.eq.'clear')then
	  call tvreset
	else if(op.eq.'grey')then
	  call tvlut('B&W')
	else if(op.eq.'colour'.or.op.eq.'rainbow')then
	  call tvlut(op)
	else
	  call bug('i','Unrecognised operation')
	endif
	call tvclose
	end

