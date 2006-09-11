c************************************************************************
c  Some routines to interact with the Ultra frame buffer via the control
c  panel routines.
c
c  History:
c    rjs  8jun89 - Created
c    rjs 26jun89 - Added support for ofms.
c    rjs 16jul89 - Changed default ofm. Ulocal now flushed buffers as the
c		   first thing. Changed Ulocal labels.
c    rjs 27jul89 - Fudges for Ultra demo.
c    rjs 20sep89 - Eliminated ULUT (replaced it with TVLUT). Minor other mods.
c
c************************************************************************
	subroutine uinit(host)
c
	implicit none
	character host*(*)
c
c  This indicates to the Ultra software where the host for the control panel
c  is.
c
c  Input:
c    host	Host running the control panel software. If this is -, then
c		there is not control panel host.
c
c------------------------------------------------------------------------
	include 'ultra2.h'
c
	if(host.eq.'-')then
	  hostname = ' '
	else
	  hostname = host
	endif
c
	panel = .false.
	Zoom = 1
	Xc = 640
	Yc = 512
c
	call Uopen
	call TVLut('colour')
c
	end
c************************************************************************
	subroutine ulocal()
	implicit none
c
c  Allow the user to fiddle the ultra frame buffer.
c
c------------------------------------------------------------------------
	include 'ultra2.h'
c
	character object*8
	integer changes,blcx,blcy,trcx,trcy,val1,val2
c
	if(hostname.eq.' ')return
	call uflush
	if(.not.panel) call UDef
c
c  Loop.
c
	call CtrlClr
	call CtrlWait(object,changes,val1,val2)
	dowhile(object.ne.'exit')
	  if(object.eq.'zoom')then
	    zoom = min(8,Zoom + Zoom)
	  else if(object.eq.'unzoom')then
	    zoom = max(1,Zoom/2)
	  else if(object.eq.'reset')then
	    zoom = 1
	    xc = 640
	    yc = 512
	  else if(object.eq.'cursor')then
	    xc = 1280*val1/100
	    yc = 1024*val2/100
	  endif
	  blcx = max(0,min(xc - 640/Zoom,1280-1280/Zoom))
	  blcy = max(0,min(yc - 512/Zoom,1024-1024/Zoom))
	  trcx = blcx + 1280/Zoom - 1
	  trcy = blcy + 1024/Zoom - 1
	  call TvView(blcx,blcy,trcx,trcy)
	  call CtrlWait(object,changes,val1,val2)
	enddo
c
	end
c************************************************************************
	subroutine UCursor(x,y,button)
c
	implicit none
	integer x,y,button
c
c  Check the cursor position and the state of the 'exit' button.
c
c  Outputs:
c    x,y	Coordinates of the cursor. These are rescaled to the
c		range 0 to 2047 in x, and 0 to 1023 in y.
c    button	This is set to 1 if the Exit button has been pressed.
c
c------------------------------------------------------------------------
	include 'ultra2.h'
	integer changes,val1,val2
c
	if(hostname.eq.' ')return
	if(.not.panel) call UDef
	call CtrlChck('cursor',changes,val1,val2)
	x = 2048*val1/100
	y = 1024*val2/100
	call CtrlChck('exit',changes,val1,val2)
	if(changes.gt.0)then
	  button = 1
	else
	  button = 0
	endif
	end
c************************************************************************
	subroutine UDef
	implicit none
c
c  Set up the control panel, and display it.
c
c------------------------------------------------------------------------
	include 'ultra2.h'
	integer status
	call CtrlInit(hostname,status)
	if(status.ne.0) call bugno('f',status)
	call CtrlDef('zoom','button','ZOOM+',1)
	call CtrlDef('unzoom','button','ZOOM-',1)
	call CtrlDef('reset','button','RESET',1)
	call CtrlDef('exit','button','EXIT ',1)
	call CtrlDef('cursor','cursor','ROAM',1)
	call CtrlView
	panel = .true.
	end
c************************************************************************
	subroutine ufin
	implicit none
c
c  Finish up with the control panel and the ultra frame buffer.
c
c------------------------------------------------------------------------
	include 'ultra2.h'
c
	if(panel) call CtrlFin
	call uclose
	end
