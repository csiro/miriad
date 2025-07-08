c************************************************************************
c
c  History:
c    rjs   Jun89 Original version.
c    rjs 17jul89 Fudges for siggraph demo. Added status arg to CtrlInit,
c		 changed len1 to len in CtrlDef
c     jm 07may90 Added CtrlOpen.
c     jm 27aug90 Added CtrlSet.
c     jm 26feb91 Changes Itoa to Itoaf.
c    rjs 16jul91 Added CtrlSeta and support for status lines.
c     jm 02nov92 Modified CtrlOpen to not print a warning if it could
c		 not open the server.  Also added a string length check
c		 to the routines which search for an item number for the
c		 case when an item is added with Len(name)>Len(Items).
c		 In other words, only the first Len(Items) characters
c		 are now significant.
c
c*Ctrl -- Manipulating a control panel.
c& jm
c+
c  The control panel routines (all starting with the prefix Ctrl) are
c  used to open up a connection to a control panel server (perhaps on
c  a remote machine); allow the caller to define a number of buttons,
c  cursors, and sliders that the caller wants the user to control; and
c  then lets the caller and the user interact with each other.
c
c  Associated with each button, etc, is its current value (or state).
c  A button can have several states -- the control panel will show the
c  current state as the label on the button. For example "ON" and "OFF".
c  The value of a button is the number of the state.  These vary from
c  0 to n-1 (for a button with n states), as defined by CtrlDef.  A
c  slider takes on a value from 0 to 100 (which the user can change).
c  A cursor takes on two values, both ranging from 0 to 100.  Each
c  button, cursor, or slider has a caller-defined name, which the
c  caller uses for identification.  The caller may also inquire if a
c  particular button, slider, or cursor has change its value.
c--
c***********************************************************************
c*CtrlOpen -- Open the panel panel.
c& jm
c:control-panel,user-interaction
c+
      subroutine ctrlopen(name, ctrl)
c
      implicit none
      character name*(*)
      logical ctrl
c
c  This opens up a connection to the "control panel" used by a device.
c  Currently this is a TCP/IP socket to a "panel" server accessed
c  through the routines called `panel' and `xpanel' in MIRIAD.
c
c  Input:
c    name       Name of the server device used in TVOPEN.  This
c               contains the name of the host running the panel server.
c               This can have any of the following forms:
c                 type@name1
c                 type@name1/name2
c                 type@name1/name2:port
c               where name1 and name2 represent physical device names
c               or the name of the server (for network display servers).
c               Type is the device or server type (ignored by this
c               routine) and is described in detail in the User's manual.
c               If name2 exists, this is used as the panel device name;
c               otherwise, name1 is used.  If the optional port argument
c               is provided, it overrides the default value.
c  Output:
c    ctrl       A logical flag that is true if the input device can
c               drive a ctrl panel; otherwise, ctrl is false.
c--
c-----------------------------------------------------------------------
c
      integer length, status, i1, i2, msglen
      integer port
      character errmsg*80, panel*25
      logical okay
c
c  Externals.
c
      integer Len1
c
      Ctrl = .FALSE.
      length = Len1(name)
      if (length .le. 0) return
      i1 = index(name, '@')
      if ((i1 .le. 1) .or. (i1 .ge. length)) then
        errmsg =  'CTRLOPEN:  TV name must be of the form type@name.'
        msglen = Len1(errmsg)
        call bug('f', errmsg(1:msglen))
      endif
      i2 = index(name(i1+1:length), '/') + i1
      status = min((length - i2), (i2 - (i1 + 1)))
      if ((i2 .ne. i1) .and. (status .le. 0)) then
        errmsg =  'CTRLOPEN:  Bad name for TV/Panel server.'
        msglen = Len1(errmsg)
        call bug('f', errmsg(1:msglen))
      endif
      if (name(i2:i2) .eq. '/') then
        panel = name(i2+1:length)
      else
        panel = name(i1+1:length)
      endif
c
c  Set up to use the default port number.
c
      port = 0
      i1 = index(panel,':')
      if ((i1 .gt. 1) .and. (i1 .lt. len1(panel))) then
        call atoif(panel(i1+1:),port,okay)
        if (.not. okay)
     *    call bug('f', 'Panel port number incorrectly formatted.')
        panel = panel(1:i1-1)
      endif
      call CtrlPort(panel, port, status)
      Ctrl = (status.eq.0)
      return
      end
c************************************************************************
c*CtrlInit -- Initialise the panel panel.
c& jm
c:control-panel,user-interaction
c+
	subroutine CtrlInit(name,status)
c
	implicit none
	character name*(*)
	integer status
c
c  This opens up a connection to the "control panel".  Currently this is
c  a TCP/IP socket to a "panel" server accessed through the routines
c  called `panel' and `xpanel' in MIRIAD.
c
c  NOTE:  This is really internal code and should NOT be called.
c         Use ctrlopen() instead.
c         This documentation is kept for backwards compatibility.
c
c  Input:
c    name	Name of the host running the panel server.
c  Output:
c    status	Completion status.  Zero indicates success. Other values
c		indicate some failure in attempted to connect to the
c		server.
c--
c------------------------------------------------------------------------
c  Depreciated code that uses the default port number.
c
	call CtrlPort(name,0,status)
	end
c************************************************************************
c*CtrlPort -- Initialise the panel panel.
c& jm
c:control-panel,user-interaction
c+
	subroutine CtrlPort(name,port,status)
c
	implicit none
	character name*(*)
	integer port
	integer status
c
c  This opens up a connection to the "control panel".  Currently this is
c  a TCP/IP socket to a "panel" server accessed through the routines
c  called `panel' and `xpanel' in MIRIAD.
c
c  NOTE:  This is really internal code and should NOT be called.
c         Use ctrlopen() instead.
c         This documentation is kept for backwards compatibility.
c
c  Input:
c    name	Name of the host running the panel server.
c    port	Port number to connect to.  If this is negative or 0,
c        	the default value of DefPort is used (see ctrl.h).
c  Output:
c    status	Completion status.  Zero indicates success. Other values
c		indicate some failure in attempted to connect to the
c		server.
c--
c------------------------------------------------------------------------
	include 'ctrl.h'
	integer cport, inet
c
c  Externals.
c
	integer TcpSock,TcpConn
c
	NItems = 0
	status = -2
	call TcpNode(name,inet)
	if(inet.eq.0)return
	Status = TcpSock(handle)
	if(Status.ne.0)call bugno('f',Status)
	cport = port
	if (cport .le. 0) cport = DefPort
	Status = TcpConn(handle,inet,cport)
	end
c************************************************************************
c*CtrlView -- Pop up the control panel on the workstation screen.
c& jm
c:control-panel,user-interaction
c+
	subroutine CtrlView
c
	implicit none
c
c  Make the control panel become visible.
c--
c------------------------------------------------------------------------
	include 'ctrl.h'
	call CtrlFlsh(2)
	Buffer(BufLen+1) = DISPLAY
	Buffer(BufLen+2) = 0
	BufLen = BufLen + 2
	call CtrlFlsh(BufSize)
	end
c************************************************************************
c*CtrlClr -- Clear any memory in the control panel of buttons being pressed.
c& jm
c:control-panel,user-interaction
c+
	subroutine CtrlClr
c
	implicit none
c
c  Make the control panel forget about any buttons, sliders, or cursors
c  that have changed their value.
c--
c------------------------------------------------------------------------
	include 'ctrl.h'
	call CtrlFlsh(2)
	Buffer(BufLen+1) = CLEAR
	Buffer(BufLen+2) = 0
	BufLen = BufLen + 2
	call CtrlFlsh(BufSize)
	end
c************************************************************************
c*CtrlChck -- Check if a particular buttons has been pushed.
c& jm
c:user-interaction,control-panel
c+
	subroutine CtrlChck(name,changes,val1,val2)
c
	implicit none
	character name*(*)
	integer changes,val1,val2
c
c  This checks if a particular panel item has been modified and returns
c  its current value.  If the item has not been previously accessed,
c  `changes' will return with a zero value and `val1' and `val2' will
c  be returned undefined.
c
c  Input:
c    name	The name of the item.
c  Output:
c    changes	The number of times the item has been changed.
c    val1,val2	Two values associated with the item.
c--
c------------------------------------------------------------------------
	include 'ctrl.h'
	integer itno,i,length
c
c  Determine the number of the item.
c
	length = min(Len(Items(1)), Len(name))
	itno = 0
	do i=1,NItems
	  if(name(1:length).eq.Items(i)(1:length)) itno = i
	enddo
	if(itno.eq.0) call bug('f','Item was not found, in CtrlChck')
c
	call CtrlFlsh(2)
	Buffer(BufLen+1) = CHECK
	Buffer(BufLen+2) = itno
	BufLen = BufLen + 2
	call CtrlRead(4)
	changes = Buffer(2)
	val1 = Buffer(3)
	val2 = Buffer(4)
	end
c************************************************************************
c*CtrlWait -- Wait for a button to be pressed.
c& jm
c:user-interaction,control-panel
c+
	subroutine CtrlWait(name,changes,val1,val2)
c
	implicit none
	character name*(*)
	integer changes,val1,val2
c
c  Waits for any panel item to be modified.  If one or several items
c  have previously changed, this routine returns immediately and passes
c  back the first item that it happens to find that has changed.
c
c  NOTE:  This routine will block until something has been modified.
c
c  Output:
c    name	The name of the item that changed.
c    changes	The number of times the item has been changed.
c    val1,val2	Two values associated with the item.
c--
c------------------------------------------------------------------------
	include 'ctrl.h'
c
	call CtrlFlsh(2)
	Buffer(BufLen+1) = WAIT
	Buffer(BufLen+2) = 0
	BufLen = BufLen + 2
	call CtrlRead(4)
	name = Items(Buffer(1))
	changes = Buffer(2)
	val1 = Buffer(3)
	val2 = Buffer(4)
	end
c************************************************************************
c*CtrlDef -- Define a control panel button, etc.
c& jm
c:user-interaction,control-panel
c+
	subroutine CtrlDef(name,type,values,nvalues)
c
	implicit none
	integer nvalues
	character name*(*),type*(*),values(nvalues)*(*)
c
c  This defines a particular button, etc, that is to appear on the
c  control panel.
c
c  Inputs:
c    Name	The name of the item.
c    type	The type.  This can be 'button','slider','cursor'.
c    values	The values that the think can take on.
c    nvalues	Number of values.
c--
c------------------------------------------------------------------------
	include 'ctrl.h'
	integer i,j,l,Size
c
c  Externals.
c
c	integer len1
c
	if(NItems.eq.MaxItems) call bug('f','Too many items')
	NItems = NItems + 1
	Items(NItems) = name
c
c  Determine the number of characters we have to send to the server.
c
	Size = 0
	do i=1,nvalues
	  Size = Size + len(values(i)) + 1
	enddo
	call CtrlFlsh(Size+4)
	Buffer(BufLen+1) = DEFINE
	Buffer(BufLen+2) = Size
	Buffer(BufLen+3) = NItems
	if(type.eq.'button')then
	  Buffer(BufLen+4) = BUTTONS
	else if(type.eq.'slider')then
	  Buffer(BufLen+4) = SLIDERS
	else if(type.eq.'cursor')then
	  Buffer(BufLen+4) = CURSORS
	else if(type.eq.'status')then
	  Buffer(BufLen+4) = STATUSES
	endif
	BufLen = BufLen + 4
	do i=1,nvalues
	  l = len(Values(i))
	  do j=1,l
	    Buffer(BufLen+1) = ichar(values(i)(j:j))
	    BufLen = BufLen + 1
	  enddo
	  Buffer(BufLen+1) = 0
	  BufLen = BufLen + 1
	enddo
	end
c***********************************************************************
c*CtrlSet -- Set the value of a control panel button, etc.
c& jm
c:user-interaction,control-panel
c+
      subroutine ctrlset(name, values, nvalues)
c
      implicit none
      integer nvalues
      integer values(nvalues)
      character name*(*)
c
c  This changes the value for a particular button, etc, that appears
c  on the control panel.  If the item type associated with name is a
c  BUTTON, then only one value is accepted and represents the offset
c  from the first BUTTON entry in the list for that name (i.e.
c  value(1) = 0 is the first entry for that BUTTON, value(1) = 1
c  is the second entry, etc.).  The list entry pointed to by the offset
c  will be moved to the current BUTTON.  If the item type is a CURSOR,
c  then up to two values will be accepted and represent the X and Y
c  positions of the CURSOR.  If only one value is present, then both
c  X and Y will be set to that value.  Finally, if the item type is a
c  SLIDER and only one value is present, then the current value of the
c  SLIDER will be set to that value.  If two values are present, then
c  these values represent the new lower and upper limits of the SLIDER.
c
c  NOTE: This routine ONLY is useful AFTER a call to CtrlView!!
c  If CtrlView has not yet been called, no action will be performed.
c
c  Inputs:
c    Name     The name of the previously defined control panel item.
c             This is the same name (including case) of the panel item
c             defined in CtrlDef.
c    values   An integer array of values used for setting the item.
c    nvalues  The size of the values array.
c--
c-----------------------------------------------------------------------
      include 'ctrl.h'
      integer i, j, Size, Itno
      character String*15
c
c  Externals.
c
      integer Len1
      character*15 Itoaf
c
      Size = min(Len(Items(1)), Len(name))
      Itno = 0
      do i = 1, NItems
        if (name(1:Size).eq.Items(i)(1:Size)) Itno = i
      enddo
      if (Itno .eq. 0) call bug('f', 'CtrlSet: Item not found.')
c
c  Determine the number of characters we have to send to the server.
c
      Size = 0
      do i = 1, nvalues
        String = Itoaf(values(i))
        Size = Size + Len1(String) + 1
      enddo
      if (Size .eq. nvalues) call bug('f','CtrlSet: No values defined.')
c
      call CtrlFlsh(Size+3)
      Buffer(BufLen+1) = SET
      Buffer(BufLen+2) = Size
      Buffer(BufLen+3) = Itno
      BufLen = BufLen + 3
      do i = 1, nvalues
        String = Itoaf(values(i))
        Size = Len1(String)
        do j = 1, Size
          Buffer(BufLen+1) = ichar(String(j:j))
          BufLen = BufLen + 1
        enddo
        Buffer(BufLen+1) = 0
        BufLen = BufLen + 1
      enddo
      call CtrlFlsh(BufSize)
      end
c***********************************************************************
c*CtrlSeta -- Set the value of a control panel message.
c& jm
c:user-interaction,control-panel
c+
      subroutine ctrlseta(name, string)
c
      implicit none
      character name*(*),string*(*)
c
c  This changes the value of a text message associated with a
c  button, etc.  It is primarily useful for "status" items.
c
c  NOTE: This routine ONLY is useful AFTER a call to CtrlView!!
c  If CtrlView has not yet been called, no action will be performed.
c
c  Inputs:
c    Name     The name of the previously defined control panel item.
c             This is the same name (including case) of the panel item
c             defined in CtrlDef.
c    String   A character string used as the message text.
c--
c-----------------------------------------------------------------------
      include 'ctrl.h'
      integer i, j, Size, Itno
c
      Size = min(Len(Items(1)), Len(name))
      Itno = 0
      do i = 1, NItems
        if (name(1:Size).eq.Items(i)(1:Size)) Itno = i
      enddo
      if (Itno .eq. 0) call bug('f', 'CtrlSeta: Item not found.')
c
c  Determine the number of characters we have to send to the server.
c
      Size = len(string)
c
      call CtrlFlsh(Size+4)
      Buffer(BufLen+1) = SET
      Buffer(BufLen+2) = Size + 1
      Buffer(BufLen+3) = Itno
      BufLen = BufLen + 3
      do j = 1, Size
        Buffer(BufLen+1) = ichar(String(j:j))
        BufLen = BufLen + 1
      enddo
      Buffer(BufLen+1) = 0
      BufLen = BufLen + 1
      call CtrlFlsh(BufSize)
      end
c************************************************************************
c*CtrlFin -- Close down the control panel.
c& jm
c:user-interaction,control-panel
c+
	subroutine CtrlFin
	implicit none
c
c  This closes down the control panel releasing any resources allocated
c  by the control panel routines.
c
c------------------------------------------------------------------------
	include 'ctrl.h'
c
	call CtrlFlsh(2)
	Buffer(BufLen+1) = DONE
	Buffer(BufLen+2) = 0
	BufLen = BufLen + 2
	call CtrlFlsh(BufSize)
	call TcpClose(handle)
	end
c************************************************************************
	subroutine CtrlFlsh(n)
c
	implicit none
	integer n
c
c  This makes sure there is enough space in the buffer to hold the next
c  request.  If there is not, the buffer is flushed to the socket.
c
c  Input:
c    n		Remaining size requested in the buffer.  If this value
c               is greater than the buffer size minus the current filled
c               buffer size, the buffer is flushed.  If it is greater
c               than the buffer size, the program aborts!
c
c------------------------------------------------------------------------
	include 'ctrl.h'
	integer Status
c
c  Externals.
c
	integer TcpWrite
c
c  Check for space, and flush the buffer if needed.
c
	if(n.gt.BufSize)call bug('f','Request overflows buffer')
	if(BufLen+n.gt.BufSize)then
	  call packi2(buffer,iobuf,BufLen)
	  Status = TcpWrite(handle,iobuf,2*BufLen)
	  if(Status.ne.0)call bugno('f',Status)
	  BufLen = 0
	endif
	end
c************************************************************************
	subroutine CtrlRead(n)
c
	implicit none
	integer n
c
c  Read from the socket.
c
c  Inputs:
c    n		The number of short integers to read.
c------------------------------------------------------------------------
	include 'ctrl.h'
	integer iostat,nread
c
c  Externals.
c
	integer TcpRead
c
	if(BufLen.ne.0) call CtrlFlsh(BufSize)
	iostat = TcpRead(handle,iobuf,2*n,nread)
	if(iostat.ne.0) call bugno('f',iostat)
	if(nread.ne.2*n) call bug('f','Unexpected End-of-Data')
	call unpacki2(iobuf,buffer,n)
	end
