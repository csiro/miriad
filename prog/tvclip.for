c***********************************************************************
c  Allow interactive editing of UV data loaded onto a TV device.
c
c  History:
c    jm    14feb90    Initial version.
c    jm    26mar90    Changed flow to make use of SelProbe routine.
c    jm    30apr90    Changed how the array is loaded (data and flag).
c    jm    07may90    Changed Dispedit to include declared size of iflag.
c    jm    16jun90    Added LdDisp and cleaned up code a bit.
c    jm    14jul90    Increased the WIDTH of the strips.
c    jm    22aug90    Added TV labeling and improved flag re-writing.
c    jm    21sep90    Substantial change to include rubber banding.
c    jm    24oct90    Corrected a few items.
c    jm    20nov90    Major program control flow re-write.
c    jm    22jan91    Expanded documentation and added source keyword
c                     and folded subroutines into main program file.
c    jm    22feb91    Added a check to avoid array size overflow in
c                     UVread loop.  Also added a few more user comments.
c    jm    22feb91    Added MAXDIM2 to allow a larger time range.
c    jm    26feb91    Changed all occurrances of Itoa to Itoaf.
c    jm    22mar91    Added a note to the user about min/max range
c                     but only on the first pass through the data.
c    rjs   19jun91    Fixed bugs in time messages and LUT selection.
c    jm    21jun91    Added mode and started taver section.
c    jm    12aug91    Added routine Report to use CtrlSeta function.
c                     Also finished taver section.
c    jm    21sep91    Corrected xypos in edflag routine and now do an
c                     inquire as to whether a display can call tvselpt.
c    rjs    1oct91    Fixed rounding problem with final time.
c    jm     1oct91    Added additional counters to properly compute
c                     the running averages and sum wedges.  Also, added
c                     mod 360 for phase differences and summary wedges.
c    jm     8oct91    Moved some data statements into code in edflag.
c                     Also removed argument chan from call to getbox.
c    jm    21apr92    Increased the declared size of the variable sEDIT
c                     from 7 to 10.  This array is defined in tvclip.h.
c    jm    06jul92    Added a check to see if data is present at all
c                     so that a "Last time" message is not generated.
c    jm    15jul92    Modified code to reflect changes to call structure
c                     of tv.for subroutine TvSelpt.
c    jm    18jul92    Modified EdFlag routine to query state from server
c                     rather than rely on internal variables.  Also
c                     added an initial (dumb) call to EdFlag from EdDisp
c                     to initialize some (EdFlag) internal variables.
c    jm    20oct92    Corrected command line syntax to allow single
c                     character input (also command words).  The
c                     documentation said it did, but it wasn't.
c    mjs   17feb93    minor doc mod only; rm ref to MIRTOOL.
c    mjs   27feb93    include tmpdim.h instead of maxdim.h
c    rjs    8mar93    Standardise history comments. Eliminate some
c		      non-standard FORTRAN.
c    jm    14jun93    Modified getcmd() to handle single key commands
c                     and command names correctly.  Also think I fixed
c                     an empty string bug in report() which was causing
c                     xpanel to bug out because of an insufficient
c                     amount of data available to read.
c    jm    17jun93    Changed amp-diff mode to use ABS(amp-diff).
c    mjs   30sep93    call delay -> call delayf
c    nebk  29oct93    Add mode=real,imag
c    jm    29oct93    Made sure that amplitude was the default mode.
c    rjs    7dec93    Major rework to do averaging and reduce
c		      the number of passes through the
c		      data.
c    rjs   27jan94    Don't know how I ever believed it was working!!!
c    rjs    4mar93    Fudges to get it to work on data out of time order.
c    nebk  09mar94    Declare APRI long enough to avoid string truncation
c                     warning on convex (does not affect run time performance)
c    rjs   27jul94    Get rid of some debugging write statements.
c    rjs   11nov94    Eliminate bounds violation for large numbers of times.
c    mhw&ip 22nov94    Add clip option
c***********************************************************************
c= Tvclip - Interactive editing of a UV data set on a TV device.
c& jm
c: calibration, uv-data, tv, plotting, display
c+
      program tvclip
      implicit none
c
c     TVCLIP is a MIRIAD task which allows interactive baseline
c     editing of a UV data set.  The user should make sure,
c     if the display device is capable of creating menus that
c     the menu program has been loaded ``before'' running this
c     routine (eg. xpanel for the XMTV) - see the User's Guide
c     or Cookbook under TV Devices for details.
c
c     When this program is run, each baseline is displayed,
c     one at a time, with the x-axis representing channel
c     number (increasing to the right), the y-axis representing
c     the change in time (increasing upward), and the color
c     intensity representing the amplitude or phase (depending
c     on the value of the keyword ``mode'') of the visibility.
c     The current antenna that constitute the baseline are
c     labeled at the bottom of the plot (if room permits).
c     In addition, if room permits, a color wedge is displayed
c     to the right side of the visibility data and is the sum
c     of the data over all displayed channels.  Also, a wedge
c     of the sum of the data over all displayed times will
c     appear above (or below) the data as room permits.
c
c     The user may edit the data in one of two ways: 1) By
c     entering commands at the keyboard; or 2) by selecting
c     commands from the listed menu items.  Which method is
c     determined by the presence of a menu program (such as
c     ``XPANEL'' for the Sun systems).
c
c     If no menu program is currently active, then a message
c     is issued to the user and all commands are prompted from
c     the keyboard.  Commands are entered followed by a carriage
c     return (<CR>).  Single letter abbreviations are used for
c     all commands.  The current command list, each corresponding
c     abbreviation, and a brief description of each command is
c     available by entering a question mark (``?'') as a command.
c     Commands requiring further interaction (eg. ``select'' and
c     ``value'') will prompt the user when they are invoked.
c
c     If a menu program is active, then TVCLIP will attempt
c     to construct an assortment of menu buttons and other
c     items that will perform the equivalent of entering commands
c     at the keyboard.  Certain commands (eg. ``select'' and
c     ``value'') require further cursor input and will prompt
c     the user for a particular action (for example: ``select''
c     will request the user to select the region to box for
c     further flagging commands).
c
c     The current technique of editing is to load the baseline
c     to be edited, zoom and pan to the desired location, and
c     then, select the region to be edited followed by an
c     editing command.  To identify a region to edit, chose
c     the ``select'' command (or ``channel'' or ``time'' select
c     options) and then use the cursor to box the desired points.
c     To select a desired region, move the cursor (mouse) to a
c     corner of the region to select; identify, to the program,
c     the corner (usually by pushing and holding the left mouse
c     button); move the pointer to the other corner (by dragging
c     the mouse), and then identify, again to the program, the
c     other corner (usually by releasing the left mouse button).
c     After the region to edit has been identified, the selected
c     region may be flagged as good or bad using the appropriate
c     editting command (or button).  If data is flagged improperly,
c     the ``undo'' command will reverse the last editing operation.
c
c     NOTE:  If the device does not support this type of ``rubber-
c     band'' selection (Eg. XMTV does; MXAS does not), then the
c     user will be asked to use the cursor to identify two opposite
c     corners to delimit the selected region.
c
c     To save the editing done to a particular baseline, use
c     the ``exit'' command (or select the ``exit'' button).
c     The flagging changes will be immediately applied to the
c     data.  To NOT save the changes made to a particular
c     baseline, use the ``quit'' command (or button).  After
c     a ``quit'' or ``exit'' operation, the data for the next
c     baseline is loaded onto the display and the user may continue
c     editing.  Selecting the ``abort'' command (button) at any
c     time will perform the same operation as ``quit'', but
c     will also terminate the program.
c
c@ vis
c     The name of the input UV data set.  A visibility file name
c     must be supplied.  Only one file may be edited at a time.
c
c< server
c
c@ tvchan
c     An integer specifying which TV channel the data is
c     displayed.  The default is channel 1.
c
c@ range
c     The minimum and maximum range used in scaling the data
c     on the TV.  Default is to autoscale to the first image.
c     If this keyword is used, TWO parameters must be input.
c
c@ tvcorn
c     The integer device coordinate of the lower left corner
c     of the image.  The default is to center the image on the
c     display.  If this keyword is used and only the x coordinate
c     value is input, the y coordinate value is set to the x value.
c
c< line
c
c     NOTE: Here ``type'' must be `channel' and the maximum of
c     both ``width'' and ``step'' must be 1.  The default is
c     to display all channels.
c
c@ mode
c     Display ``amplitude'', ``phase'', ``real'', ``imaginary''.
c     By default, ``amplitude'' is selected. For mode=phase, the
c     phase is in degrees.
c
c@ taver
c     The length of time (in minutes) used to average the data when
c     determining the running mean. This is used with the "DIFF"
c     command, where the running mean of the data are subtraced off.
c     Two values can be given for taver, being TTOT and TGAP
c     respectively. If the time interval between any
c     two successive data points is greater than TGAP, or if the
c     total time between the first data point in an average
c     and any succeeding data point exceeds TTOT, then a new
c     average is started.  The default for TTOT is 5 minutes, whereas
c     the default TGAP is the value of TTOT.
c
c@ clip
c     The clip level in sigma's of the current display for use with
c     the CLIP command
c
c< select
c
c     NOTE: The default is to use all visibilities.
c
c--
c-----------------------------------------------------------------------
c
c  Internal parameters.
c
      include 'maxdim.h'
      character PROG*(*)
      parameter (PROG = 'TVCLIP: ')
      character VERSION*(*)
      parameter (VERSION = PROG // 'version 2.3 9-Mar-94')
      integer NOPT,MAXSELS,MAXEDIT
      parameter (NOPT=4,MAXSELS=256,MAXEDIT=10000)
c
c  Internal variables.
c
      character Vis*132, Server*132
      character Line*30, errmsg*80
      character apri*9
      character Opts(NOPT)*10
      integer Lin, nchan, MostChan, channel
      integer maxxpix, maxypix, levels, msglen
      integer jx0, jy0, nout
      real start, width, step
      real pmin, pmax
      real amp
      real taver(2)
      real clip
      real Sels(MAXSELS)
      logical center, Ctrl
      integer nedit,edits(2,MAXBASE)
      real times(2,MAXEDIT)
      integer chans(2,MAXEDIT)
      logical flagval(MAXEDIT)
      double precision day0
c
c  Externals.
c
      integer Len1
      logical KeyPrsnt
c
      data Opts / 'amplitude', 'phase','real', 'imaginary'/
c
c  End declarations.
c-----------------------------------------------------------------------
c  Announce program.
c
      call Output(VERSION)
c-----------------------------------------------------------------------
c  Use the key routines to get the user input parameters.
c
      call KeyIni
      call Keyf('vis', Vis, ' ')
      call Keya('server', Server, ' ')
      call Keyi('tvchan', channel, 1)
      call Keyr('range', pmin, 0.0)
      call Keyr('range', pmax, pmin)
      center = .not. KeyPrsnt('tvcorn')
      call Keyi('tvcorn', jx0, 0)
      call Keyi('tvcorn', jy0, jx0)
c
      call keymatch('line',1,'channel',1,Line,nout)
      if(nout.eq.0) Line = 'channel'
      call Keyi('line', nchan, 0)
      call Keyr('line', start, 1.0)
      call Keyr('line', width, 1.0)
      call Keyr('line', step, 1.0)
      call SelInput('select', Sels, MAXSELS)
      call keymatch('mode', NOPT, Opts, 1, apri, nout)
      if(nout.eq.0) apri = 'amplitude'
      call Keyr('taver', taver(1), 5.0)
      call Keyr('taver', taver(2), taver(1) )
      call Keyr('clip', clip, 5.0)
c
      call KeyFin
c
c  Check the input parameters.
c
      if (Vis .eq. ' ') then
        errmsg = PROG // 'A Visibility file must be given.'
        msglen = Len1(errmsg)
        call Bug('f', errmsg(1:msglen))
      endif
      if (Server .eq. ' ') then
        errmsg = PROG // 'A TV device/server must be given.'
        msglen = Len1(errmsg)
        call Bug('f', errmsg(1:msglen))
      endif
c
      if (nchan .lt. 0) then
        errmsg = PROG // 'Bad number of channels.'
        msglen = Len1(errmsg)
        call Bug('f', errmsg(1:msglen))
      endif
c
      if (Line .ne. 'channel') then
        errmsg = PROG // 'Line type must be channel.'
        msglen = Len1(errmsg)
        call Bug('f', errmsg(1:msglen))
      endif
      if (width .le. 0.0) then
        errmsg = PROG // 'Negative width is useless.'
        msglen = Len1(errmsg)
        call Bug('f', errmsg(1:msglen))
      endif
      if (step .eq. 0.0) then
        errmsg = PROG // 'Step = 0.0 is useless.'
        msglen = Len1(errmsg)
        call Bug('f', errmsg(1:msglen))
      endif
      amp = max(width, step)
      if (amp .ne. 1.0) then
        errmsg = PROG // 'Maximum width and step must be 1.'
        msglen = Len1(errmsg)
        call Bug('f', errmsg(1:msglen))
      endif
c
      if(taver(1).le.0.or.taver(2).le.0)
     *	  call bug('f','Invalid averaging parameter')
      taver(1) = taver(1) / (24.0 * 60.0)
      taver(2) = taver(2) / (24.0 * 60.0)
c
      if(clip.le.0)
     *	  call bug('f','Invalid clip parameter')
c
c  End of user inputs.
c-----------------------------------------------------------------------
c
c  Open the connection to the display server, get default device
c  characteristics, and determine the active channel and if device
c  can support a menu window.
c
      call Output('Opening server and panel...')
      call TvOpen(Server)
      call TvChar(maxxpix, maxypix, MostChan, levels)
      channel = min(max(channel, 1), MostChan)
      call TvChan(channel)
      call TvView(0, 0, (maxxpix-1), (maxypix-1))
      call CtrlOpen(Server, Ctrl)
c
c  Open the visibility file and determine how many antennae are present.
c
      call Output('Opening visibility file: ' // Vis)
      call UvOpen(Lin, Vis, 'old')
      nchan = min(nchan,MAXCHAN,maxxpix)
      if (nchan .ge. 0) call UvSet(Lin, 'data', Line, nchan, start,
     *    width, step)
      call SelApply(Lin, Sels, .TRUE.)
c
c  Determine the flagging to be done.
c
      call doEdit(Lin,apri,taver,center,jx0,jy0,channel,
     *	  Ctrl,pmin,pmax,
     *    edits,MAXBASE,day0,times,chans,flagval,MAXEDIT,nedit, clip)
c
c  Do the flagging.
c
      if(nedit.gt.0)then
	call Output('Applying changes to the dataset')
	call HisOpen(Lin,'append')
	call HisWrite(Lin,PROG // 'Miriad '// VERSION)
	call HisInput(Lin,PROG)
	call UvRewind(Lin)
	call doFlag(Lin,edits,MAXBASE,day0,times,chans,flagval,nedit)
	call HisClose(Lin)
      endif
c
c  Close up the TV unit and the Vis file and then quit.
c
      if (Ctrl) then
        call CtrlClr
        call CtrlFin
      endif
      call TvClose
      call UvClose(Lin)
      end
c************************************************************************
	subroutine doEdit(Lin,apri,taver,center,jx0,jy0,channel,
     *	  Ctrl,pmin,pmax,
     *    edits,nbase,day0,times,chans,flagval,MAXEDIT,nedit, clip)
c
	implicit none
	integer Lin,channel,jx0,jy0,nbase,maxedit,nedit
	character apri*1
	logical center,Ctrl
	real taver(2),pmin,pmax
	double precision day0
	real times(2,maxedit), clip
	integer edits(2,nbase),chans(2,maxedit)
	logical flagval(maxedit)
c
c  Determine things to flag or unflag.
c
c  Input:
c    Lin	Handle of the input dataset.
c    apri	Quantity to display.
c    taver	Integration times used for running difference.
c    center	True if jx0,jy0 are to default.
c    jx0,jy0	BLC of the region to display.
c    channel	Channel to use for the display.
c    Ctrl	Whether to use the panel server or not.
c    pmin,pmax	Scaling parameters.
c  Output:
c    nedit	Number of editting instructions.
c    flagval
c    times
c    edits
c    chans
c    day0	Base time.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer MAXTIME,MAXTIME2,MAXSAVE,MAXTREV
	real ttol
	logical doScr
	parameter(MAXTIME=10000,MAXTIME2=MAXDIM,MAXTREV=128,
     *			MAXSAVE=1024,ttol=1./86400.,doScr=.true.)
	integer i,j,k,i1,i2,iblok
	integer lScr
	real t1(MAXTIME),t2(MAXTIME)
	integer ntime,nbased,nchan,iret,nbl,nbld,nvis,nout,bl
	integer nstep,npass
	integer iDat,iFlg,iDato,iFlgo
	logical blpres(MAXBASE)
	integer bl2idx(MAXBASE),idx2bl(MAXBASE),trevidx(2,MAXTREV)
	integer idx1(MAXTIME2),idx2(MAXTIME2)
	integer isave(5,MAXSAVE)
	integer maxxpix,maxypix,mostchan,levels,chanoff
c
c  Externals.
c
	integer membuf
	character itoaf*8
c
c  Initialise the index of the flagging instructions.
c
	nbased = min(nbase,MAXBASE)
	do i=1,nbase
	  edits(1,i) = 0
	  edits(2,i) = -1
	enddo
c  
c  Write all the data out to the scratch file, and make a list of
c  all the times that we have encountered. Also remember any baselines
c  that we encounter.
c
	lScr = 0
	if(doScr)call scropen(lScr)
c
c  Copy all the data to a scratch file, and determine which
c  baselines and times are present.
c
	call Output('Loading the data ...')
	call CopyDat(lIn,lScr,apri,nchan,t1,MAXTIME,ntime,day0,ttol,
     *		blpres,nbased,nvis,chanoff)
c
c  Determine the time ranges to average together.
c
	call TvChar(maxxpix,maxypix,mostchan,levels)
	if(maxypix.gt.300)maxypix = maxypix - 100
	maxypix = min(maxypix,MAXTIME2)
	call TimeMap(t1,t2,ntime,ttol,nout,maxypix)
	ntime = nout
c
c  Determine the index giving time reversals.
c
	call Trev(t1,idx1,idx2,ntime,trevidx,MAXTREV)
c
c  Determine the number of baselines.
c
	nbl = 0
	do i=1,nbased
	  if(blpres(i))nbl = nbl + 1
	enddo
c
c  Determine the number of things to grid together. Make it as small
c  as can be tolerated to keep the number of passes down to a minimum.
c
	nstep = min(nbl,max(1, (membuf()-10) / (2*nchan*ntime)))
	npass = (nbl+nstep-1)/nstep
	nstep = (nbl+npass-1)/npass
	call output('Number of channels in the grid: '//itoaf(nchan))
	call output('Number of time slots in the grid: '//itoaf(ntime))
	call output('Number of baselines gridded per pass: '//
     *							  itoaf(nstep))
c
c  Allocate memory.
c
	call memalloc(iFlg,nstep*nchan*ntime,'i')
	call memalloc(iDat,nstep*nchan*ntime,'r')
c
c  Load the menu.
c
        call LdMenu(Ctrl)
c
c  Enter the gridding loop.
c  Determine the baselines to process on this pass.
c
	do k=1,nbl
	  nstep = 1
	  nbld = 0
	  do i=1,nbased
	    if(blpres(i).and.nbld.lt.nstep)then
	      blpres(i) = .false.
	      nbld = nbld + 1
	      idx2bl(nbld) = i
	      bl2idx(i) = nbld
	    else
	      bl2idx(i) = 0
	    endif
	  enddo
c
c  Grid all the baselines that we want to select this pass through.
c
	  call Report(Ctrl,'Performing a gridding pass ...')
	  call Gridit(memI(iFlg),memR(iDat),nchan,ntime,nbld,
     *	    bl2idx,nbased,day0,lScr,nvis,t1,t2)
c
c  Edit all the baselines that we currently have,
c
	  do j=1,nbld
	    bl = idx2bl(j)
	    iFlgo = iFlg + (j-1)*nchan*ntime
	    iDato = iDat + (j-1)*nchan*ntime
	    call EdDisp(memI(iFlgo),memR(iDato),nchan,ntime,
     *	      t1,t2,taver,chanoff,
     *	      center,jx0,jy0,channel,pmin,pmax,Ctrl,bl,
     *	      isave,iret, clip)
c
c  Jump out if the user has hit the ABORT button.
c
	    if(iret.lt.0)then
	      nedit = 0
	      goto 999
	    endif
c
c  Copy the editting instructions if the user has not aborted.
c
	    edits(1,bl) = nedit + 1
	    do i=1,iret
	      i1 = isave(1,i)
	      i2 = isave(3,i)
	      if(t1(i1).lt.-1)i1 = i1 + 1
	      if(t2(i2).lt.-1)i2 = i2 - 1
	      if(i1.le.i2)then
	        iblok = 1
		dowhile(i1.gt.trevidx(2,iblok))
		  iblok = iblok + 1
		enddo
		dowhile(i2.gt.trevidx(2,iblok))
		  nedit = nedit + 1
		  if(nedit.eq.MAXEDIT)
     *		    call bug('f','Too many flagging operations')
		  edits(2,bl) = nedit
		  times(1,nedit) = t1(i1)
		  times(2,nedit) = t2(trevidx(2,iblok))
		  chans(1,nedit) = isave(2,i)
		  chans(2,nedit) = isave(4,i)
		  flagval(nedit) = isave(5,i).eq.1
		  iblok = iblok + 1
		  i1 = trevidx(1,iblok)
		enddo
		nedit = nedit + 1
		if(nedit.eq.MAXEDIT)
     *		  call bug('f','Too many flagging operations')
		edits(2,bl) = nedit
		times(1,nedit) = t1(i1)
		times(2,nedit) = t2(i2)
		chans(1,nedit) = isave(2,i)
		chans(2,nedit) = isave(4,i)
		flagval(nedit) = isave(5,i).eq.1
	      endif
	    enddo
	  enddo
	enddo
c
c  We have finished the flagging. Release the scratch file and
c  memory.
c
  999	continue
	if(doScr)call scrclose(lScr)
	call memfree(iFlg,nstep*nchan*ntime,'i')
	call memfree(iDat,nstep*nchan*ntime,'r')
c
	end
c************************************************************************
	subroutine trev(t,idx1,idx2,n,tblk,MAXBLK)
c
	implicit none
	integer n,maxblk,tblk(2,maxblk),idx1(n),idx2(n)
	real t(n)
c
c  Break a sequence of times (which are more or less monotonically
c  increasing) into non-overlapping blocks of monotonically increasing
c  times.
c
c  Input:
c    t		The sequence of times. Times of -2 are treated as dummies.
c    n		The number of time intervals.
c    maxblk	The size of the output array.
c  Output:
c    tblk	The indices of the time blocks.
c  Scratch:
c    idx1,idx2	Used for sorting the times.
c------------------------------------------------------------------------
	integer nblk,ipnt,i
c
c  Get a sorting of the times.
c
	call hsortr(n,t,idx1)
c
c  Generate a reverse map.
c
	do i=1,n
	  idx2(idx1(i)) = i
	enddo
c
c  Generate the blocks of non-overlapping times.
c
	ipnt = idx2(1)
	nblk = 1
	tblk(1,1) = 1
	tblk(2,1) = 1
	do i=1,n
	  if(t(i).lt.-1)then
	    continue
	  else if(idx2(i).eq.ipnt)then
	    tblk(2,nblk) = i
	    ipnt = ipnt + 1
	  else
	    nblk = nblk + 1
	    if(nblk.gt.MAXBLK)call bug('f','Too many time reversals')
	    tblk(1,nblk) = i
	    tblk(2,nblk) = i
	    ipnt = idx2(i) + 1
	  endif
	enddo
	end
c************************************************************************
	subroutine Gridit(iflag,array,nchan,ntime,nbl,
     *	    bl2idx,nbase,day0,lScr,nvis,t1,t2)
c
	implicit none
	integer nchan,ntime,nbl,nbase,lScr,nvis
	integer bl2idx(nbase),iflag(nchan,ntime,nbl)
	real t1(ntime),t2(ntime),array(nchan,ntime,nbl)
	double precision day0
c
c  Read through the scratch file, and extract and average the data that
c  we want.
c
c  Input:
c    day0	Base time
c    nchan	Number of channels.
c    ntime	Number of time slots.
c    nbl	Number of baselines to grid.
c    bl2idx	Convert baseline number to index into array,iflag where
c		the gridded data are to be stored.
c    t1,t2	Time range for each time slot.
c    lScr	Handle of the scratch file containing the data.
c    nvis	Number of visibilities.
c  Output:
c    array	The gridded data.
c    iflag	Flag for the gridded data.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k,offset,length,pnt,bl,i0
	real buf(2*MAXCHAN+3),t
c
	if(nchan.gt.MAXCHAN)call bug('f','Too many channels')
c
	do k=1,nbl
	  do j=1,ntime
	    do i=1,nchan
	      iflag(i,j,k) = 0
	      array(i,j,k) = 0
	    enddo
	  enddo
	enddo
c
c  Start reading the data.
c
	offset = 0
	length = 2*nchan + 3
	pnt = 1
	do k=1,nvis
	  call scrread(lScr,buf,offset,length)
	  offset = offset + length
	  bl = nint(buf(1))
	  t = buf(2) + (dble(buf(3)) - day0)
	  bl = bl2idx(bl)
	  if(bl.gt.0)then
	    dowhile(t1(pnt).lt.-1.or.t.lt.t1(pnt).or.t.gt.t2(pnt))
	      pnt = pnt + 1
	    enddo
	    if(t1(pnt).gt.t.or.t.gt.t2(pnt))call bug('f','Inconsistent')
	    i0 = 3
	    do i=1,nchan
	      if(nint(buf(i0+2)).gt.0)then
		if(iflag(i,pnt,bl).gt.0)then
		  array(i,pnt,bl) = array(i,pnt,bl) + buf(i0+1)
		  iflag(i,pnt,bl) = iflag(i,pnt,bl) + 1
		else
		  array(i,pnt,bl) = buf(i0+1)
		  iflag(i,pnt,bl) = 1
		endif
	      else
		if(iflag(i,pnt,bl).le.0)then
		  array(i,pnt,bl) = array(i,pnt,bl) + buf(i0+1)
		  iflag(i,pnt,bl) = iflag(i,pnt,bl) - 1
		endif
	      endif
	      i0 = i0 + 2
	    enddo
	  endif
	enddo
c
c  We have accumulated all the data. Now form the average.
c
	do k=1,nbl
	  do j=1,ntime
	    do i=1,nchan
	      if(iflag(i,j,k).gt.0)then
		array(i,j,k) = array(i,j,k) / iflag(i,j,k)
		iflag(i,j,k) = 1
	      else if(iflag(i,j,k).lt.0)then
		array(i,j,k) = - array(i,j,k) / iflag(i,j,k)
		iflag(i,j,k) = 0
	      endif
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine EdDisp(iflag,array,Nx,Ny,t1,t2,taver,chanoff,
     *	      center,jx,jy,chan,pmin,pmax,Ctrl,bl,isave,iret, clip)
c
	implicit none
	integer Nx,Ny,jx,jy,chan,bl,iret,chanoff
	logical center,Ctrl
	integer iflag(Nx,Ny),isave(5,*)
	real array(Nx,Ny),pmin,pmax,t1(Ny),t2(Ny),taver(2),clip
c
c  Do all the operations involved in editting a baseline.
c
c  Input:
c    iflag(Nx,Ny) Input integer array of flag values.
c    array(Nx,Ny) Input real array of data.
c    sumoverx(Ny) The sum of array over the x extent at each y position.
c    sumovery(Nx) The sum of array over the y extent at each x position.
c    Mx           Actual (declared) X dimension of the flag array.
c    Nx, Ny       X/Y working dimensions of the flag array.
c    chan         The TV channel on which the image is loaded.
c    x0, y0       The lower left corner that is the start of the image.
c    pmin, pmax   The real minimum/maximum (pmin/pmax) of the array.
c    Ctrl         Logical flag that is true if the device can define
c                 its own panel buttons; false otherwise.
c    t1,t2	  Start and end time of a time slot.
c    chanoff	  Offset to add to channel number to convert to a true
c		  channel number.
c    clip         Clip level in sigmas
c
c  Output:
c    pmin, pmax   The real minimum/maximum (pmin/pmax) of the array.
c    iflag(Nx,Ny) Output integer array of adjusted flag values.
c    sumoverx(Ny) The sum of array over the x extent if IFLAG changed.
c    sumovery(Nx) The sum of array over the y extent if IFLAG changed.
c    isave(5,*)   Integer array of changes made (B, L, T, R, Val).
c    iret         Integer that specifies whether or not to save
c                 changes to the flag data.  Iret = 0 means to QUIT
c                 without saving changes; iret = N (> 0) means save
c                 the N changes; and iret = -1 means to QUIT and abort
c                 the rest of the baselines.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'tvclip.h'
	character antmsg*32
	integer x0,y0,maxxpix,maxypix,mostchan,levels,ant1,ant2,l,k
	real bmin,bmax
	real SumoverY(MAXDIM),SumoverX(MAXDIM)
	integer NoverY(MAXDIM),NoverX(MAXDIM)
	integer istate, comm
	integer param(5)
	logical First
	save First
c
c  Externals.
c
	integer len1
	character itoaf*8
	data First /.TRUE./
c
	if(max(Nx,Ny).gt.MAXDIM)
     *	  call bug('f','Too many times or channels')
c
c  Determine the scales.
c
	if(pmin.lt.pmax)then
	  bmin = pmin
	  bmax = pmax
	else
	  call GetScale(array,iflag,Nx,Nx,Ny,bmin,bmax)
	endif
	if(bmin.ge.bmax)return
c
c  Determine the sums with time and channel.
c
	call GetWedge(array,iflag,SumoverY,NoverY,SumoverX,NoverX,
     *							Nx,Nx,Ny)
c
c  Determine the center.
c
	if(center)then
	  call TvChar(maxxpix,maxypix,mostchan,levels)
	  x0 = (maxxpix - Nx) / 2
	  y0 = (maxypix - Ny) / 2
	else
	  x0 = jx
	  y0 = jy
	endif
c
c  Determine the antennae involved, so that we can give a message.
c
	ant1 = 1
	ant2 = 1
	do k=2,bl
	  ant1 = ant1 + 1
	  if(ant1.gt.ant2)then
	    ant1 = 1
	    ant2 = ant2 + 1
	  endif
	enddo
	antmsg = 'Antennas: '//itoaf(ant1)
	l = len1(antmsg)
	antmsg(l+1:) = '-'//itoaf(ant2)
	l = len1(antmsg)
c
	call output('Displaying data for '//antmsg(1:l))
c
c  Present option menu and initialize some variables.
c  This includes a dummy call to EdFlag to initialize some
c  internal parameters for that routine.
c
	param(1) = 0
c
	if (First) call edflag(iflag, array, sumoverx, sumovery,
     *	  NoverX, NoverY, Nx, Nx, Ny, t1, t2, taver,chanoff,antmsg(1:l),
     *    iINITALL,param, x0, y0, chan, bmin, bmax, isave, Ctrl, istate, 
     *    clip)
c
	First = .false.
c
	call edflag(iflag, array, sumoverx, sumovery,
     *	  NoverX, NoverY, Nx, Nx, Ny, t1, t2, taver,chanoff,antmsg(1:l),
     *    iINITBL, param, x0, y0, chan, bmin, bmax, isave, Ctrl, istate,
     *    clip)
c
	if (Ctrl) call CtrlClr
c
c  Loop until done editing (EdFlag will return istate = 0 until
c  either a QUIT (-1), EXIT (>0), or ABORT (-99) command is found).
c
	istate = 0
	dowhile (istate .eq. 0)
	  if (Ctrl) then
	    call GetBtn(comm, param)
	  else
	    call GetCmd(comm, param)
	  endif
	  call edflag(iflag, array, sumoverx, sumovery,
     *	    NoverX, NoverY, Nx, Nx, Ny, t1,t2,taver,chanoff,antmsg(1:l),
     *      comm, param, x0, y0, chan, bmin, bmax, isave, Ctrl, istate, 
     *      clip)
	enddo
c
c  Done editing.  Set iret to save the flag file if editing ended with
c  an EXIT;  otherwise, set iret according to whether a QUIT or ABORT
c  command was given.
c
	iret = 0
	if (istate .lt. -10) iret = -1
	if (istate .gt. 0) iret = istate
	end
c************************************************************************
	subroutine GetScale(array,iflag,Mx,Nx,Ny,bmin,bmax)
c
	implicit none
	integer Mx,Nx,Ny
	integer iflag(Mx,Ny)
	real array(Mx,Ny),bmin,bmax
c
c  Determine the maximum and minimum of the good data.
c
c------------------------------------------------------------------------
	integer i,j
	logical first
c
	bmin = 0
	bmax = 0
c
	first = .true.
	do j=1,Ny
	  do i=1,Nx
	    if(iflag(i,j).gt.0)then
	      if(first)then
		bmin = array(i,j)
		bmax = bmin
		first = .false.
	      else
		bmin = min(bmin,array(i,j))
		bmax = max(bmax,array(i,j))
	      endif
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GetWedge(array,iflag,SumoverY,NoverY,SumoverX,NoverX,
     *	  Mx,Nx,Ny)
c
	implicit none
	integer Mx,Nx,Ny
	integer iflag(Mx,Ny),NoverY(Nx),NoverX(Ny)
	real array(Mx,Ny),SumoverY(Nx),SumoverX(Ny)
c
c  Determine the wedges.
c------------------------------------------------------------------------
	integer i,j
c
	do i=1,Nx
	  SumoverY(i) = 0
	  NoverY(i) = 0
	enddo
	do j=1,Ny
	  SumoverX(j) = 0
	  NoverX(j) = 0
	enddo
c
	do j=1,Ny
	  do i=1,Nx
	    if(iflag(i,j).gt.0)then
	      SumoverX(j) = SumoverX(j) + array(i,j)
	      NoverX(j) = NoverX(j) + 1
	      SumoverY(i) = SumoverY(i) + array(i,j)
	      NoverY(i) = NoverY(i) + 1
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine CopyDat(lIn,lScr,apri,nchan,time,MAXTIME,ntime,
     *		day0,ttol,blpres,nbase,nvis,chanoff)
c
	implicit none
	integer lIn,lScr,nchan,maxtime,ntime,nbase,nvis,chanoff
	character apri*1
	real time(maxtime),ttol
	double precision day0
	logical blpres(nbase)
c
c  Copy the data to a scratch file.
c
c  Input:
c    lIn	Handle of the input visibility dataset.
c    lScr	Handle of the output scratch file.
c    apri	Sort of quantity to display.
c    maxtime	Dimension of time array.
c    ttol	Time tolerance.
c    nbase	Maximum number of baselines.
c  Output:
c    blpres	Indicates whether a particular baseline is present.
c    day0	Base time.
c    time	The time of each integration.
c    nvis	Number of visibilities.
c    ntime	Number of times.
c    nchan	Number of channels.
c    chanoff	Offset to add to channel numbers to get true channel numbers.
c------------------------------------------------------------------------
	include 'maxdim.h'
	logical flags(MAXCHAN),newsrc
	complex data(MAXCHAN)
	double precision preamble(4),line(6),day1
	real buf(2*MAXCHAN+3),t,tprev
	logical torder
	integer vsrc,nread,length,offset,ant1,ant2,i,bl,i0
c
c  Externals.
c
	logical uvvarupd
	real ctoapri
c
c  Initialise the array to keep track of what baselines are present.
c
	do i=1,nbase
	  blpres(i) = .false.
	enddo
c
c  Whenever the source changes, cause a break point.
c
	call uvVarini(lIn,vsrc)
	call uvVarSet(vsrc,'source')
c
	call uvread(lIn,preamble,data,flags,MAXCHAN,nchan)
	call uvinfo(lIn,'line',line)
	chanoff = nint(line(3)) - 1
	day0 = nint(preamble(3)-1) + 0.5d0
	tprev = -1
	length = 2*nchan + 3
	offset = 0
	ntime = 0
	nread = nchan
	torder = .true.
	dowhile(nread.eq.nchan)
	  call basant(preamble(4),ant1,ant2)
	  bl = ((ant2-1)*ant2)/2 + ant1
	  t = preamble(3) - day0
	  if(t.lt.0)then
	    day1 = nint(preamble(3)-1) + 0.5d0
	    do i=1,ntime
	      if(time(i).gt.-1)time(i) = time(i) + day0 - day1
	    enddo
	    tprev = tprev + day0 - day1
	    t = t + day0 - day1
	    day0 = day1
	  endif
	  if(bl.gt.0.and.bl.lt.nbase)then
	    if(abs(t-tprev).gt.ttol)then
	      if(t.lt.tprev)torder = .false.
	      newsrc = uvVarUpd(vsrc)
	      if(ntime.gt.0.and.
     *		(newsrc.or.t-tprev.gt.120*ttol.or.t.lt.tprev))then
		if(ntime.ge.MAXTIME)
     *		  call bug('f','Too many times for me')
		ntime = ntime + 1
		time(ntime) = -2
	      endif
	      if(ntime.ge.MAXTIME)call bug('f','Too many times for me')
	      ntime = ntime + 1
	      time(ntime) = t
	      tprev = t
	    endif
	    blpres(bl) = .true.
c
c  Write the data to a scratch file (if one exists).
c
	    if(lScr.ne.0)then
	      buf(1) = bl
	      buf(2) = t
	      buf(3) = day0
	      i0 = 3
	      do i=1,nchan
		buf(i0+1) = ctoapri(data(i), apri)
		buf(i0+2) = 0
		if(flags(i))buf(i0+2) = 1
		i0 = i0 + 2
	      enddo
	      call scrwrite(lScr,buf,offset,length)
	    endif
c
	    offset = offset + length
	  endif
	  call uvread(lIn,preamble,data,flags,MAXCHAN,nread)
	enddo
c
	if(.not.torder)
     *    call bug('w','Display will not be in strict time order')
	if(nread.ne.0)call bug('f',
     *	  'Number of chanels changed while reading the data')
	if(ntime.eq.0)call bug('f','No data found')
	if(time(ntime).lt.-1) ntime = ntime - 1
	if(ntime.eq.0)call bug('f','No data found')
c
	nvis = offset / length
c
	end
c************************************************************************
	subroutine TimeMap(t1,t2,ntime,ttol,nout,maxtime)
c
	implicit none
	integer ntime,nout,maxtime
	real t1(ntime),t2(ntime),ttol
c
c  Determine the time ranges for each time slot.
c
c  Input:
c    ntime	The number of time slots before averaging.
c    maxtime	Maximum number of time slots.
c    ttol	Time tolerance.
c  Input/Output:
c    t1		On input, it contains the time for each integration.
c		On output, it contains the first time for a time
c		slot.
c  Output:
c    nout	The number of time slots after averaging.
c    t2		The last time in a time slot.
c    
c------------------------------------------------------------------------
	integer i
	real tprev,inttime
c
c  Time to apply to the data to make it fit on the screen.
c
	if(ntime.gt.maxtime)then
	  call GetInt(t1,t2,ntime,inttime,maxtime,ttol)
	else
	  inttime = ttol
	endif
c
c  Determine the time ranges to average together in the display.
c
	tprev = t1(1) - 0.5 * ttol
	nout = 0
	do i=1,ntime
	  if(t1(i).lt.-1.or.t1(i).gt.tprev+inttime)then
	    nout = nout + 1
	    t2(nout) = t1(i-1) + 0.5 * ttol
	    t1(nout) = tprev
	    if(t1(i).lt.-1)then
	      tprev = t1(i+1) - 0.5 * ttol
	      nout = nout + 1
	      t1(nout) = -2
	      t2(nout) = -2
	    else
	      tprev = t1(i) - 0.5 * ttol
	    endif
	  endif
	enddo
c
c  Flush out the final one.
c
	nout = nout + 1
	t2(nout) = t1(ntime) + 0.5 * ttol
	t1(nout) = tprev
c
	end
c************************************************************************
	subroutine GetInt(time,temp,ntime,inttime,maxtime,ttol)
c
	implicit none
	integer ntime,maxtime
	real time(ntime),temp(ntime),inttime,ttol
c
c  Determine a integration period to use with the data which will make
c  the data fit on the screen.
c
c------------------------------------------------------------------------
	integer i,n,nout
	real delta,tprev
	character line*80
c
c  Determine all the delta times.
c
	n = 0
	do i=2,ntime
	  if(time(i-1).gt.-1.and.time(i).gt.-1)then
	    n = n + 1
	    temp(n) = time(i) - time(i-1)
	  endif
	enddo
c
c  Sort all the delta times.
c
	if(ntime-n.gt.maxtime)call bug('f',
     *	  'Cannot determine appropriate averaging interval')
	call sortr(temp,n)
c
	delta = temp(1)
	n = 0
	nout = maxtime + 1
	dowhile(nout.gt.maxtime)
	  n = n + 1
	  inttime = n * delta + ttol
	  tprev = time(1) - 0.5*ttol
	  nout = 0
	  do i=2,ntime
	    if(time(i).lt.-1)then
	      tprev = time(i+1) - 0.5 * ttol
	      nout = nout + 2
	    else if(time(i).gt.tprev+inttime)then
	      tprev = time(i) - 0.5 * ttol
	      nout = nout + 1
	    endif
	  enddo
	enddo
c
	write(line,'(a,i4,a)')'Averaging the data over',
     *		nint(86400.*(n+1)*delta),' seconds'
	call output(line)
c
	end
c************************************************************************
	subroutine doFlag(Lin,edits,nbase,day0,times,chans,flagval,
     *								  nedit)
c
	implicit none
	integer Lin,nbase,nedit
	integer edits(2,nbase)
	double precision day0
	real times(2,nedit)
	integer chans(2,nedit)
	logical flagval(nedit)
c
c  Apply the flagging commands to the data.
c
c  Input:
c    Lin	Handle of the input dataset.
c    nbase	Number of baseline slots.
c    edits	Index to the editting to be performed on each baseline.
c    day0	Base time.
c    times	Time range to flag/unflag.
c    chans	Channel range to flag/unflag.
c    flagval	The flagging value.
c    nedit	Number of edit commands.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer nchan,ant1,ant2,bl,i,j
	complex data(MAXCHAN)
	logical flags(MAXCHAN),flagged
	real t
	double precision preamble(4)
c
	call uvread(Lin,preamble,data,flags,MAXCHAN,nchan)
	dowhile(nchan.gt.0)
	  t = preamble(3) - day0
	  flagged = .false.
	  call basant(preamble(4),ant1,ant2)
	  bl = ((ant2-1)*ant2)/2 + ant1
	  if(bl.gt.0.and.bl.le.nbase)then
	    do j=edits(1,bl),edits(2,bl)
	      if(t.ge.times(1,j).and.t.le.times(2,j))then
	        flagged = .true.
	        do i=max(chans(1,j),1),min(chans(2,j),nchan)
	          flags(i) = flagval(j)
	        enddo
	      endif
	    enddo
	  endif
	  if(flagged)call uvflgwr(Lin,flags)
	  call uvread(Lin,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	end
c***********************************************************************
      real function ctoapri(data, apri)
      implicit none
      complex data
      character apri*(*)
c
c     Input:
c        data     The complex value.
c        apri     'a', 'p', 'r', 'i'
c     Output:
c        ctoapri  Returns either amp, phase, real, imaginary based on 
c                 value of ``apri''.  The phase is returned in units of
c                 degrees and in the inclusive range of [0, 360].
c
c-
c-----------------------------------------------------------------------
c
      real amp, phase
c
      call AmPhase(data, amp, phase)
      if (apri.eq.'p') then
c       Initial range is -180<=phase<=180; push it up to 0<=phase<=360.
        if (phase .lt. 0) phase = phase + 360.0
        ctoapri = phase
      else if (apri.eq.'a') then
        ctoapri = amp
      else if (apri.eq.'r') then
        ctoapri = real(data)
      else if (apri.eq.'i') then
        ctoapri = aimag(data)
      else
       call Bug('f', 'Unrecognized option in CTOAPRI.')
      end if
c
      end
c***********************************************************************
c  LdMenu -- Internal routine to load tvclip.h common block.
c
      subroutine ldmenu(ctrl)
c
      implicit none
      logical ctrl
c
c  Input:
c    ctrl         Input logical flag to determine if the Ctrl routines
c                 can be used to interact with the display device.
c
c  Output:
c    none
c
c-----------------------------------------------------------------------
c
      include 'tvclip.h'
      character string*10
      character panfid(2)*7
      character ctable(3)*7
      integer i, j, k
c
      integer Len1
c
      data panfid /'  PAN', 'STRETCH'/
      data ctable /'  B&W', 'COLOUR', 'RAINBOW'/
c
      iEDIT(iNULL)    = iNULL
      iEDIT(iZOOMIN)  = iZOOMIN
      iEDIT(iZOOMOUT) = iZOOMOUT
      iEDIT(iSELECT)  = iSELECT
      iEDIT(iCHANNEL) = iCHANNEL
      iEDIT(iTIME)    = iTIME
      iEDIT(iFLAGOOD) = iFLAGOOD
      iEDIT(iFLAGBAD) = iFLAGBAD
      iEDIT(iRESCALE) = iRESCALE
      iEDIT(iDIFF)    = iDIFF
      iEDIT(iCLIP)    = iCLIP
      iEDIT(iLIST)    = iLIST
      iEDIT(iPIXVAL)  = iPIXVAL
      iEDIT(iLUT)     = iLUT
      iEDIT(iRESET)   = iRESET
      iEDIT(iQUIT)    = iQUIT
      iEDIT(iEXIT)    = iEXIT
      iEDIT(iUNDO)    = iUNDO
      iEDIT(iABORT)   = iABORT
      iEDIT(iCURSOR)  = iCURSOR
      iEDIT(iFIDPAN)  = iFIDPAN
      iEDIT(iHELP)    = iHELP
c
      cEDIT(iEDIT(iNULL))    = ' '
      cEDIT(iEDIT(iZOOMIN))  = '+'
      cEDIT(iEDIT(iZOOMOUT)) = '-'
      cEDIT(iEDIT(iSELECT))  = 's'
      cEDIT(iEDIT(iCHANNEL)) = 'x'
      cEDIT(iEDIT(iTIME))    = 'y'
      cEDIT(iEDIT(iFLAGOOD)) = 'g'
      cEDIT(iEDIT(iFLAGBAD)) = 'b'
      cEDIT(iEDIT(iRESCALE)) = 'r'
      cEDIT(iEDIT(iDIFF))    = 'd'
      cEDIT(iEDIT(iCLIP))    = 'i'
      cEDIT(iEDIT(iLIST))    = 'l'
      cEDIT(iEDIT(iPIXVAL))  = 'v'
      cEDIT(iEDIT(iLUT))     = 't'
      cEDIT(iEDIT(iRESET))   = '0'
      cEDIT(iEDIT(iQUIT))    = 'q'
      cEDIT(iEDIT(iEXIT))    = 'e'
      cEDIT(iEDIT(iUNDO))    = 'u'
      cEDIT(iEDIT(iABORT))   = 'a'
      cEDIT(iEDIT(iCURSOR))  = 'c'
      cEDIT(iEDIT(iFIDPAN))  = 'f'
      cEDIT(iEDIT(iHELP))    = 'h'
c
      sEDIT(iEDIT(iNULL))    = 'NULL'
      sEDIT(iEDIT(iZOOMIN))  = 'ZOOMIN'
      sEDIT(iEDIT(iZOOMOUT)) = 'ZOOMOUT'
      sEDIT(iEDIT(iSELECT))  = 'SELECT'
      sEDIT(iEDIT(iCHANNEL)) = 'SLCT CHAN'
      sEDIT(iEDIT(iTIME))    = 'SLCT TIME'
      sEDIT(iEDIT(iFLAGOOD)) = 'FLAGOOD'
      sEDIT(iEDIT(iFLAGBAD)) = 'FLAGBAD'
      sEDIT(iEDIT(iRESCALE)) = 'RESCALE'
      sEDIT(iEDIT(iDIFF))    = 'DIFF'
      sEDIT(iEDIT(iCLIP))    = 'CLIP'
      sEDIT(iEDIT(iLIST))    = 'LIST'
      sEDIT(iEDIT(iPIXVAL))  = 'VALUE'
      sEDIT(iEDIT(iLUT))     = 'LUT'
      sEDIT(iEDIT(iRESET))   = 'RESET'
      sEDIT(iEDIT(iQUIT))    = 'QUIT'
      sEDIT(iEDIT(iEXIT))    = 'EXIT'
      sEDIT(iEDIT(iUNDO))    = 'UNDO'
      sEDIT(iEDIT(iABORT))   = 'ABORT'
      sEDIT(iEDIT(iCURSOR))  = 'CURSOR'
      sEDIT(iEDIT(iFIDPAN))  = 'STRETCH'
      sEDIT(iEDIT(iHELP))    = 'HELP'
c
      tEDIT(iEDIT(iZOOMIN))  = 'ZOOM IN DISPLAY'
      tEDIT(iEDIT(iZOOMOUT)) = 'ZOOM OUT DISPLAY'
      tEDIT(iEDIT(iSELECT))  = 'SELECT REGION FOR FLAGGING OPERATION'
      tEDIT(iEDIT(iCHANNEL)) = 'SELECT A CHANNEL FOR FLAGGING OPERATION'
      tEDIT(iEDIT(iTIME))    = 'SELECT A TIME FOR FLAGGING OPERATION'
      tEDIT(iEDIT(iFLAGOOD)) = 'FLAG SELECTION AS GOOD'
      tEDIT(iEDIT(iFLAGBAD)) = 'FLAG SELECTION AS BAD'
      tEDIT(iEDIT(iRESCALE)) = 'RESCALE THE DISPLAY'
      tEDIT(iEDIT(iDIFF))    = 'SUBTRACT OFF RUNNING MEAN'
      tEDIT(iEDIT(iCLIP))    = 'CLIP ALL PIXELS ABOVE CLIP*SIGMA'
      tEDIT(iEDIT(iLIST))    = 'LIST CHANGES MADE FOR THIS BASELINE'
      tEDIT(iEDIT(iPIXVAL))  = 'LIST PIXEL VALUE'
      tEDIT(iEDIT(iLUT))     = 'SELECT B&W, COLOR, OR PSEUDO LUT'
      tEDIT(iEDIT(iRESET))   = 'RESET LUT AND ZERO ZOOM AND PAN'
      tEDIT(iEDIT(iQUIT))    = 'STOP EDITING AND DO NOT SAVE CHANGES'
      tEDIT(iEDIT(iEXIT))    = 'STOP EDITING AND SAVE CHANGES'
      tEDIT(iEDIT(iUNDO))    = 'UNDO LAST FLAG CHANGES'
      tEDIT(iEDIT(iABORT))   = 'QUIT AND TERMINATE THE PROGRAM'
      tEDIT(iEDIT(iCURSOR))  = 'PAN AROUND THE SCREEN OR STRETCH LUT'
      tEDIT(iEDIT(iFIDPAN))  = 'TOGGLE BETWEEN PANNING AND STRETCHING'
      tEDIT(iEDIT(iHELP))    = 'DISPLAY THIS MESSAGE'
c
      if (Ctrl) then
        do i = 1, NCOMS
          j = iEDIT(i)
          string = sEDIT(j)
          k = Len1(string)
          if (j .eq. iFIDPAN) then
            call CtrlDef('STRETCH', 'button', panfid, 2)
          else if (j .eq. iLUT) then
            call CtrlDef(string(1:k), 'button', ctable, 3)
          else if (j .eq. iCURSOR) then
            call CtrlDef(string(1:k), 'cursor', string(1:k), 1)
          else if (j .eq. iHELP) then
            continue
          else
            call CtrlDef(string(1:k), 'button', string(1:k), 1)
          endif
        enddo
c  Initialize the widgets with the longest string.
        call CtrlDef('Line1', 'status',
     *      'Select a position and then push button [A].', 1)
        call CtrlDef('Line2', 'status',
     *      '1234567890123456789 1234567890123456789 123', 1)
        call CtrlDef('Line3', 'status',
     *      '1234567890123456789 1234567890123456789 123', 1)
        call CtrlDef('Line4', 'status',
     *      '1234567890123456789 1234567890123456789 123', 1)
        call CtrlView
c  Initialize the saved Report strings.
        call Report(.TRUE., ' ')
        call Report(.TRUE., ' ')
        call Report(.TRUE., ' ')
      endif
c
      end
c***********************************************************************
c EdMenu -- Present a menu from which to select edit functions.
c 
      subroutine edmenu(Ctrl)
c
      implicit none
      logical Ctrl
c
c  This routine is called only when an incorrect option or the HELP
c  option has been selected.  It is an internal routine only used
c  by TVCLIP and provides a listing of the accepted commands allowed
c  by the program.
c
c  Input:
c    Ctrl         Logical flag that determines if the TV device can
c                 support a panel menu.
c
c  Output:
c    none
c
c--
c-----------------------------------------------------------------------
c
      include 'tvclip.h'
      integer j
      character string*80
c
      if (Ctrl) then
        call Report(Ctrl, 'Select a command from the menu.')
        return
      endif
c
      call Output(' ')
      call Output(' ')
      call Output('*********TVCLIP EDIT MENU*********')
      call Output(' ')
      call Output(' ')
      call Output('KEY==> VALUE  : WHAT KEY MEANS....')
      call Output(' ')
      do j = 1, NCOMS
        write (string, 10) cEDIT(j), sEDIT(j), tEDIT(j)
        call Output(string)
      enddo
   10 format ( 1x, a, ' ==> ', a, ': ', a )
c
      call Output(' ')
      call Output('   ==> Select an image postion with the cursor')
      call Output('   ==> and then enter one of the above commands.')
      call Output(' ')
      end
c***********************************************************************
      subroutine getbtn(cmd, param)
c
      implicit none
      integer cmd
      integer param(5)
c
c  This routine is used by TVCLIP to return a command and its
c  parameters relevent to a button selected on the control panel.
c
c  Input:
c    NONE
c
c  Output:
c    cmd      Integer containing the Edit command selected.
c    param        An integer array that contains (in this order):
c                   1.  The number of times a button has been pushed.
c                   2.  The first value of the button pushed.
c                   3.  The second value of the button pushed.
c                   4.  Unused.
c                   5.  Unused.
c
c--
c-----------------------------------------------------------------------
c
      include 'tvclip.h'
      integer j, length
      integer change, val1, val2
      integer lstcmd
      character string*10
c
      integer Len1
c
      save lstcmd
      data lstcmd /0/
c
c  Loop until a proper command is returned.  If the command is not
c  correct, then print some information and then return to the loop.
c
      if (lstcmd .ne. iEDIT(iCURSOR))
     *  call Report(.TRUE., 'Waiting for a Command...')
c
      cmd = iEDIT(iNULL)
      do while (cmd .eq. iEDIT(iNULL))
        j = iNULL
        call CtrlWait(string, change, val1, val2)
        length = Len1(string)
        if (length .gt. 0) then
          do 10 j = 1, NCOMS
            if (string(1:length) .eq. sEDIT(iEDIT(j))(1:length)) goto 20
   10     continue
   20     continue
        endif
        if (j .gt. NCOMS) j = iNULL
        if (length .gt. 0) cmd = iEDIT(j)
      enddo
      lstcmd = cmd
      param(1) = change
      param(2) = val1
      param(3) = val2
      param(4) = 0
      param(5) = 0
      end
c***********************************************************************
      subroutine getcmd(cmd, param)
c
      implicit none
      integer cmd
      integer param(5)
c
c
c  This routine is used by TVCLIP to return a selected command and
c  cursor position for editing.
c
c  Input:
c    NONE
c
c  Output:
c    cmd      Integer containing the Edit command selected.
c    param        An integer array that contains (in this order):
c                   1.  Unused.
c                   2.  Unused.
c                   3.  Unused.
c                   4.  The x position of the cursor.
c                   5.  The y position of the cursor.
c
c-----------------------------------------------------------------------
c
      include 'tvclip.h'
      integer j, k, length
      character key*1, string*10
      logical single
c
c  Loop until a proper command is returned.  If the command is
c  incorrect or '?', then print some help information and return
c  the user to this loop.
c
      cmd = iEDIT(iNULL)
      do while (cmd .eq. iEDIT(iNULL))
        j = iNULL
        k = 1
        call Prompt(string, length, 'TVCLIP> ')
        do while ((string(k:k) .le. ' ') .and. (k .le. length))
          k = k + 1
        enddo
        if (k .le. length) then
          string = string(k:length)
          length = length - k + 1
          key = string(1:1)
          single = .TRUE.
          if (length .gt. 1) single = .FALSE.
          call Lcase(key)
          call Ucase(string(1:length))
          do 10 j = 0, NCOMS
            if ((single) .and. (key .eq. cEDIT(iEDIT(j)))) goto 20
            if ((.not. single) .and.
     *        (string(1:length) .eq. sEDIT(iEDIT(j))(1:length))) goto 20
   10     continue
          if ((key .eq. '?') .or. (string(1:1) .eq. '?')) j = iHELP
   20     continue
        endif
        if (j .gt. NCOMS) j = iNULL
        if (length .gt. 0) cmd = iEDIT(j)
      enddo
      param(1) = 1
      param(2) = 0
      param(3) = 0
      param(4) = 0
      param(5) = 0
      end
c***********************************************************************
      subroutine edflag(iflag, array, sumoverx, sumovery,
     *	NoverX, NoverY, Mx, Nx, Ny, t1, t2, taver, chanoff, bltext,
     *  opt, param, x0, y0, chan, pmin, pmax, isave, Ctrl, status, 
     *  cliplev)
c
      implicit none
      integer Mx, Nx, Ny, opt
      integer x0, y0, chan, status
      integer iflag(Mx, Ny), param(5), isave(5, *), chanoff
      integer NoverX(Ny), NoverY(Nx)
      real pmin, pmax, taver(2),cliplev
      real array(Mx, Ny), sumoverx(Ny), sumovery(Nx), t1(Ny), t2(Ny)
      logical Ctrl
      character bltext*(*)
c
c EdFlag -- Change the flag array based on edit selection.
c
c  This routine is used by TVCLIP to alter the input flag array "iflag"
c  based on which edit function was chosen and where the cursor was
c  located when the function was selected.
c  The current allowed functions are:
c
c    INITALL	  INITIALISE EVERYTHING.
c    INITBL       INITIALISE BASELINE SPECIFIC THINGS.
c    ABORT        STOP EDIT WITHOUT SAVING CHANGES AND TERMINATE PROGRAM
c    QUIT         STOP EDIT AND DO NOT SAVE CHANGES
c    EXIT         STOP EDIT AND SAVE CHANGES
c    UNDO         UNDO LAST FLAG CHANGES
c    ZOOMIN       ZOOM IN DISPLAY
c    ZOOMOUT      ZOOM OUT DISPLAY
c    SELECT       SELECT REGION FOR FLAGGING OPERATION
c    CHANNEL      SELECT A CHANNEL FOR FLAGGING OPERATION
c    TIME         SELECT A TIME FOR FLAGGING OPERATION
c    FLAGOOD      FLAG SELECTION AS GOOD
c    FLAGBAD      FLAG SELECTION AS BAD
c    RESCALE      DETERMINE THE BEST SCALE FACTORS
c    DIFF         SUBTRACT OFF THE RUNNING MEAN.
c    CLIP         CLIP PIXELS ABOVE CLIP*SIGMA.
c    LIST         LIST CHANGES MADE TO THIS BASELINE
c    PIXVAL       LIST PIXEL VALUE
c    LUT          SELECT BETWEEN COLOR, B&W, AND PSEUDO COLOR MAP
c    RESET        RESET LUT AND ZERO ZOOM AND PAN
c    FIDPAN       TOGGLE BETWEEN PANNING AND STRETCHING
c    CURSOR       PAN AROUND SCREEN OR STRETCH LUT
c
c  Input:
c    Mx           Input array x dimension.
c    Nx, Ny       Input array working dimensions.
c    opt          Edit command selected.
c    param(1)     The number of times the command was selected.
c    param(2)     The lower value associated with the command.
c    param(3)     The upper value associated with the command.
c    param(4)     Unused.
c    param(5)     Unused.
c    x0, y0       Pixel location of the lower left corner of the array.
c    chan         The TV channel on which the image is loaded.
c    Ctrl         Set to True if panel exists; false otherwise.
c    array(Nx,Ny) Real array of data values.
c    taver(2)     Parameters for the running mean.
c    bltext	  Descriptive text for this baseline.
c
c  Input/Output:
c    iflag(Nx,Ny) Integer array of flag values with good flagged
c                 data = 1 and bad flagged data = 0.
c    sumoverx(Ny) The sum of array over the x extent.
c    sumovery(Nx) The sum of array over the y extent.
c    NoverX(Ny)   The number of points in the SumoverX.
c    NoverY(Nx)   The number of points in the SumoverY.
c    isave(5,*)   Integer array of flag values changes.
c                 The order of each element of the array is
c                 (Bside, Lside, Tside, Rside, Value of flag).
c
c  Output:
c    Status       Output status value.  Status is 0 until the user is
c                 done editing.  If Status = -1, the user wishes to
c                 quit (apply no changes to the array iflag).  If
c                 Status > 0, then the user wishes to exit (apply
c                 changes, if any, to the array iflag).  If Status is
c                 set to -99, then the user wishes to Abort the rest
c                 of the program.
c
c--
c-----------------------------------------------------------------------
      integer PAN, STRETCH
      parameter (PAN = 1, STRETCH = 2)
c
      include 'tvclip.h'
      logical Undone
      integer j, FLAGVAL
      integer val1, val2, x, y, button
      integer Lside, Rside, Tside, Bside
      integer zoom, xpos1, ypos1, xpos2, ypos2, xc, yc, xc0, yc0
      integer maxxpix, maxypix, mxchan, levels
      integer changes, Nquit, Lut, NStretch
      integer panfid, xypos(2, 2)
      real bmin, bmax
      character line*80
      character table(3)*8
      logical doselpt
c
      save Lside, Rside, Tside, Bside, changes, Nquit
      save maxxpix, maxypix, Lut
      save zoom, xc, yc, panfid, xypos
      save NStretch
      save bmin, bmax
      save doselpt
c
      data table /'B&W', 'colour', 'rainbow'/
c
c  Initialize everything not already set.
c
      if (opt.eq.iINITALL) then
        call TvChar(maxxpix, maxypix, mxchan, levels)
        call TvSelect(j)
        doselpt = (j .eq. 1)
        xc0 = 0
        yc0 = 0
        xc = 0
        yc = 0
        call TvWind(xc0, yc0, xc, yc)
        xc = (xc0 + xc) / 2
        yc = (yc0 + yc) / 2
        zoom = 1
        call TvScrl(0, 0)
        call TvZoom(zoom, xc, yc)
        Lut = 1
        panfid = PAN
        xypos(1, 1) = 49
        xypos(2, 1) = xypos(1, 1)
        xypos(1, 2) = xypos(1, 1)
        xypos(2, 2) = xypos(2, 1)
      else if(opt.eq.iINITBL)then
	bmin = pmin
	bmax = pmax
        changes = 0
        Nquit = 0
        NStretch = 0
        Undone = .false.
        Lside = 0
        Rside = 0
        Tside = 0
        Bside = 0
	call TvEras(chan)
        call LdDisp('wit', array, iflag, sumoverx, sumovery, Mx, Nx, Ny,
     *	  NoverX, NoverY, chan, x0, y0, bmin, bmax, bltext)
	write(line,'(a,1pe10.3,a,1pe10.3)')'Setting min and max to ',
     *		bmin,',',bmax
	call output(line)
      endif
c
c  See what has to be done.
c
      val1 = param(2)
      val2 = param(3)
      status = 0
      if ((Nquit .eq. 1) .and. (opt .ne. iQUIT)) Nquit = 0
c
c  If we were stretching, but have now finished, redisplay the wedges.
c
      if ((NStretch .eq. 1) .and. (opt .ne. iCURSOR)) then
        call LdDisp('w',array, iflag, sumoverx, sumovery, Mx, Nx, Ny,
     *	  NoverX, NoverY, chan, x0, y0, bmin, bmax, bltext)
        NStretch = 0
      endif
c
c  Handle a command.
c------------------------------------------------------------------------
c
c  INIT -- we have already done it.
c
      IF (opt .eq. iINITALL .or. opt .eq. iINITBL ) then
	continue
c
c  ABORT without any changes and terminate program.
c
      else if (opt .eq. iABORT) then
        call Report(Ctrl, 'Aborting routine...')
        call Report(Ctrl, 'Please wait...')
        call Report(Ctrl, ' ')
        call Report(Ctrl, ' ')
        status = -99
        changes = 0
c
c  EXIT without any changes.
c
      else if (opt .eq. iQUIT) then
        if ((Nquit .eq. 0) .and. (changes .gt. 0)) then
          call Report(Ctrl, 'Are you sure you want to Quit?  Changes')
          call Report(Ctrl, 'made to this baseline will NOT be saved!')
          call Report(Ctrl, 'Select Quit again NOW to confirm QUIT...')
          Nquit = 1
        else
          call Report(Ctrl, 'Quiting this baseline...')
          call Report(Ctrl, 'Please wait...')
          call Report(Ctrl, ' ')
          call Report(Ctrl, ' ')
          Nquit = 0
          status = -1
          changes = 0
        endif
c
c  EXIT and apply changes.
c  If there are no changes yet, then set Status to -1 (same as QUIT).
c
      else if (opt .eq. iEXIT) then
        status = changes
        if (changes .le. 0) status = -1
        changes = 0
        call Report(Ctrl, 'Exiting this baseline...')
        call Report(Ctrl, 'Please wait...')
        call Report(Ctrl, ' ')
        call Report(Ctrl, ' ')
c
c  Zoom up.
c
      else if (opt .eq. iZOOMIN) then
        call TvRZScr(j, zoom, xc0, yc0)
        if ((j .gt. 0) .and. (j .ne. chan)) then
          call TvChan(chan)
          call TvRZScr(j, zoom, xc0, yc0)
        endif
        if (zoom .lt. 15) then
          zoom = min(15, (zoom + 1))
          call Report(Ctrl, 'Zooming up...')
          call TvZoom(zoom, xc, yc)
        endif
c
c  Zoom down.
c
      else if (opt .eq. iZOOMOUT) then
        call TvRZScr(j, zoom, xc0, yc0)
        if ((j .gt. 0) .and. (j .ne. chan)) then
          call TvChan(chan)
          call TvRZScr(j, zoom, xc0, yc0)
        endif
        if (zoom .gt. 0) then
          zoom = max(1, (zoom - 1))
          call Report(Ctrl, 'Zooming down...')
          call TvZoom(zoom, xc, yc)
        endif
c
c  Select the region to flag.
c
      else if (opt .eq. iSELECT) then
        call Report(Ctrl, 'To select a region with the mouse:')
        call Report(Ctrl, 'point it at the lower left position, push')
        call Report(Ctrl, 'the left button, drag it to the upper right')
        call Report(Ctrl, 'most position and then release the button.')
        if (doselpt) then
          call TvSelpt(chan, 3, xpos1, ypos1, xpos2, ypos2, j)
        else
          call Delayf(0.1)
          call GetBox(Ctrl, xpos1, ypos1, xpos2, ypos2)
        endif
        call Tvtopx(xpos1, ypos1, x, y)
        xpos1 = x - x0 + 1
        ypos1 = y - y0 + 1
        call Tvtopx(xpos2, ypos2, x, y)
        xpos2 = x - x0 + 1
        ypos2 = y - y0 + 1
        Lside = 0
        Bside = 0
        Rside = 0
        Tside = 0
        if (((xpos1 .ge. 1) .or. (xpos2 .ge. 1)) .and.
     *      ((xpos1 .le. Nx) .or. (xpos2 .le. Nx))) then
          Lside = max(1, min(xpos1, xpos2))
          Rside = min(Nx, max(xpos1, xpos2))
        endif
        if (((ypos1 .ge. 1) .or. (ypos2 .ge. 1)) .and.
     *      ((ypos1 .le. Ny) .or. (ypos2 .le. Ny))) then
          Bside = max(1, min(ypos1, ypos2))
          Tside = min(Ny, max(ypos1, ypos2))
        endif
c
c  Select a block of channels to flag.
c
      else if (opt .eq. iCHANNEL) then
        call Report(Ctrl, 'To select a block of channels with the')
        call Report(Ctrl, 'mouse: point it at the left channel, push')
        call Report(Ctrl, 'the left button, drag it to the right most')
        call Report(Ctrl, 'channel and then release the button.')
        if (doselpt) then
          call TvSelpt(chan, 3, xpos1, ypos1, xpos2, ypos2, j)
        else
          call Delayf(0.1)
          call GetBox(Ctrl, xpos1, ypos1, xpos2, ypos2)
        endif
        call Tvtopx(xpos1, ypos1, x, y)
        xpos1 = x - x0 + 1
        ypos1 = y - y0 + 1
        call Tvtopx(xpos2, ypos2, x, y)
        xpos2 = x - x0 + 1
        ypos2 = y - y0 + 1
        Bside = 1
        Tside = Ny
        Lside = 0
        Rside = 0
        if (((xpos1 .ge. 1) .or. (xpos2 .ge. 1)) .and.
     *      ((xpos1 .le. Nx) .or. (xpos2 .le. Nx))) then
          Lside = max(1, min(xpos1, xpos2))
          Rside = min(Nx, max(xpos1, xpos2))
        endif
c
c  Select a block of times to flag.
c
      else if (opt .eq. iTIME) then
        call Report(Ctrl, 'To select a block of time with the mouse:')
        call Report(Ctrl, 'point it at the lowest time value, push')
        call Report(Ctrl, 'the left button, drag it to the highest')
        call Report(Ctrl, 'and then release the button.')
        if (doselpt) then
          call TvSelpt(chan, 3, xpos1, ypos1, xpos2, ypos2, j)
        else
          call Delayf(0.1)
          call GetBox(Ctrl, xpos1, ypos1, xpos2, ypos2)
        endif
        call Tvtopx(xpos1, ypos1, x, y)
        xpos1 = x - x0 + 1
        ypos1 = y - y0 + 1
        call Tvtopx(xpos2, ypos2, x, y)
        xpos2 = x - x0 + 1
        ypos2 = y - y0 + 1
        Lside = 1
        Rside = Nx
        Bside = 0
        Tside = 0
        if (((ypos1 .ge. 1) .or. (ypos2 .ge. 1)) .and.
     *      ((ypos1 .le. Ny) .or. (ypos2 .le. Ny))) then
          Bside = max(1, min(ypos1, ypos2))
          Tside = min(Ny, max(ypos1, ypos2))
        endif
c
c  Flag the data within limits set previously or the current cursor
c  position (one point only).
c
      else if ((opt .eq. iFLAGOOD) .or. (opt .eq. iFLAGBAD)) then
        if (opt .eq. iFLAGOOD) FLAGVAL = 1
        if (opt .eq. iFLAGBAD) FLAGVAL = 0
        Lside = max(1, Lside)
        Rside = min(Nx, Rside)
        Bside = max(1, Bside)
        Tside = min(Ny, Tside)
c
c  If the editting instruction looks good. do it, redisplaying
c  what is necessary, and tell the user what is being done.
c
        if ((Rside .ge. Lside) .and. (Tside .ge. Bside)) then
          changes = changes + 1
          isave(1, changes) = Bside
          isave(2, changes) = Lside
          isave(3, changes) = Tside
          isave(4, changes) = Rside
          isave(5, changes) = FLAGVAL
c
c  Do the editting and redisplay.
c
	  call Edit(array,iflag,Mx,Nx,Ny,SumoverX,SumoverY,
     *		NoverX,NoverY,isave(1,changes),x0,y0,bmin,bmax,chan,
     *		t1,t2,chanoff)
          Undone = .false.
        endif
c
c  If changes have been made, allow the user to undo last command.
c
      else if (opt .eq. iUNDO) then
        if ((changes .gt. 0) .or. Undone) then
          call Report(Ctrl, 'Undoing last flagging command...')
c
	  if(Undone)then
	    changes = changes + 1
	    Undone = .false.
	    call Edit(array,iflag,Mx,Nx,Ny,SumoverX,SumoverY,
     *		NoverX,NoverY,isave(1,changes),x0,y0,bmin,bmax,chan,
     *		t1,t2,chanoff)
	  else
	    isave(5,changes) = 1 - isave(5,changes)
	    call Edit(array,iflag,Mx,Nx,Ny,SumoverX,SumoverY,
     *		NoverX,NoverY,isave(1,changes),x0,y0,bmin,bmax,chan,
     *		t1,t2,chanoff)
	    isave(5,changes) = 1 - isave(5,changes)
	    Undone = .true.
	    changes = changes - 1
	  endif
        endif
c
c  Set the scales to the min and max of the good data.
c
      else if(opt .eq. iRESCALE .or. opt .eq. iDIFF) then
	if( opt .eq. iDIFF ) then
	  call Diff(iflag,array,t1,t2,taver(1),taver(2),Mx,Nx,Ny)
	  call GetWedge(array,iflag,SumoverY,NoverY,SumoverX,NoverX,
     *							Nx,Nx,Ny)
	endif
	call GetScale(array,iflag,Mx,Nx,Ny,bmin,bmax)
	write(line,'(a,1pe10.3,a,1pe10.3)')'Setting min and max to ',
     *		bmin,',',bmax
	call output(line)
	call LdDisp('wi',array, iflag, sumoverx, sumovery, Mx, Nx, Ny,
     *	    NoverX, NoverY, chan, x0, y0, bmin, bmax, bltext)
c
c  Calculate the rms for the current data and clip all points above
c  clip*rms
c
      else if(opt .eq. iCLIP) then
        call Clip(array,iflag,Mx,Nx,Ny,SumoverX,SumoverY,
     *		NoverX,NoverY,isave,changes,x0,y0,
     *          bmin,bmax,chan,
     *		t1,t2,chanoff,cliplev)
c
c  List all changes made to this baseline.
c
      else if (opt .eq. iLIST) then
	call Output('-------------------------------------------------')
	if(changes.eq.0)then
	  call output('There are no current editting commands')
	else
	  call Output('Listing of Current Flagging Commands')
          do j = 1, changes
	    call FmtCmd(line,isave(1,j),t1,t2,Ny,chanoff)
	    call Output(line)
          enddo
	endif
c
c  Display the position and value of the identified pixel.
c
      else if (opt .eq. iPIXVAL) then
c  Flush out any previous button pushes first...
        call TvCursor(xpos1, ypos1, button)
c  Print a message to the users about how to select pts and end...
        call Report(Ctrl, 'Select a position and then push button [A].')
        call Report(Ctrl, 'Push button [D] to finish.')
        call Report(Ctrl, 'Buttons [A-D] are sometimes keys F3-F6.')
        button = 0
        call TvCursor(xpos1, ypos1, button)
c  Loop until some button is pressed...
        do while (button .le. 1)
          if (button .le. 0) then
            call Delayf(0.1)
          else
            call Tvtopx(xpos1, ypos1, x, y)
            x = x - x0 + 1
            y = y - y0 + 1
            if (x.ge.1.and.x.le.Nx.and.y.ge.1.and.y.le.Ny)then
	      call FmtVal(line,x,y,array(x,y),iflag(x,y),
     *		t1,t2,Ny,chanoff)
	      call Output(line)
	    endif
          endif
          button = 0
          call TvCursor(xpos1, ypos1, button)
        enddo
c
c  Reset LUT and ZOOM and PAN, and redisplay everything.
c
      else if (opt .eq. iRESET) then
        call Report(Ctrl, 'Resetting parameters...')
        xc0 = 0
        yc0 = 0
        xc = 0
        yc = 0
        call TvWind(xc0, yc0, xc, yc)
        xc = (xc0 + xc) / 2
        yc = (yc0 + yc) / 2
        zoom = 1
        call TvScrl(0, 0)
        call TvZoom(zoom, xc, yc)
        panfid = PAN
        if (Ctrl) then
          xypos(1, panfid) = 49
          xypos(2, panfid) = xypos(1, panfid)
          call CtrlSet(sEDIT(iCURSOR), xypos(1, panfid), 2)
          call CtrlSet(sEDIT(iFIDPAN), 0, 1)
        endif
c
        NStretch = 0
c
        bmin = pmin
        bmax = pmax
	write(line,'(a,1pe10.3,a,1pe10.3)')'Setting min and max to ',
     *		bmin,',',bmax
	call output(line)
        call LdDisp('wit', array, iflag, sumoverx, sumovery, Mx, Nx, Ny,
     *	  NoverX, NoverY, chan, x0, y0, bmin, bmax, bltext)
c
c  Change the image LUT.
c
      else if (opt .eq. iLUT) then
        if (Ctrl) then
          Lut = mod(Val1, 3) + 1
        else
          Lut = mod(Lut, 3) + 1
        endif
        line = 'Setting LUT to: ' // table(Lut)
        call Report(Ctrl, line)
        call TvLut(table(Lut))
c
c  Select whether cursor moves mean LUT changes or Panning.
c
      else if (opt .eq. iFIDPAN) then
        if (Ctrl) then
          panfid = mod(val1, 2) + 1
        else
          panfid = mod(panfid, 2) + 1
        endif
        if (Ctrl) call CtrlSet(sEDIT(iCURSOR), xypos(1, panfid), 2)
        if (panfid .eq. PAN) then
          call Report(Ctrl, 'Pan selected.')
        else if (panfid .eq. STRETCH) then
          call Report(Ctrl, 'Stretch selected.')
        endif
c
c  Set the low and high value of the LUT or Pan around the screen.
c
      else if (opt .eq. iCURSOR) then
        if (panfid .eq. STRETCH) then
          bmin = (pmax * (val1 - val2)) + (pmin * (val1 + val2))
          bmin = 0.02 * bmin
          bmax = bmin + (0.04 * (pmax - pmin) * (val2 + 1))
	  write(line,'(a,1pe10.3,a,1pe10.3)')'Setting min and max to ',
     *		bmin,',',bmax
	  call output(line)
	  call LdDisp('i', array, iflag, sumoverx, sumovery, Mx, Nx, Ny,
     *	    NoverX, NoverY, chan, x0, y0, bmin, bmax, bltext)
          NStretch = 1
        else if (panfid .eq. PAN) then
          if (zoom .gt. 1) then
            xc0 = ((maxxpix * (50 - val1)) / (50 * zoom))
            yc0 = ((maxypix * (50 - val2)) / (50 * zoom))
          else
            xc0 = 0
            yc0 = 0
          endif
          call TvScrl(xc0, yc0)
        endif
        xypos(1, panfid) = val1
        xypos(2, panfid) = val2
c
c  No choice or help selected.
c
      else if ((opt .eq. iHELP) .or. (opt .eq. iNULL)) then
        call edmenu(Ctrl)
c
c  Bad option.
c
      else
        call Bug('w', 'Incorrect Command Selected.')
      endif
c
c  Flush any buffered TV commands.
c
      call TvFlush
c
      end
c************************************************************************
	subroutine Diff(iflag,array,t1,t2,tint,tgap,Mx,Nx,Ny)
c
	implicit none
	integer Mx,Nx,Ny
	integer iflag(Mx,Ny)
	real array(Mx,Ny),tint,tgap,t1(Ny),t2(Ny)
c
c  Subtract off the running mean.
c
c  Input:
c    Nx		Number of channels.
c    Ny		Number of time slots.
c    iflag	Flags for the data.
c  Input/Output:
c    array	On input, it contains the original values. On output,
c		it contains the absolute value of the data with the
c		running mean subtracted off.
c  
c------------------------------------------------------------------------
	include 'maxdim.h'
	real sum(MAXDIM),acc
	integer cnt(MAXDIM),cstart(MAXDIM),cend(MAXDIM)
	integer pstart,pend,i,j,n,i0
c
	if(Ny.gt.MAXDIM)call bug('f','Too many times')
c
c  Determine the ranges to average over for each time slot.
c
	call AvRange(Ny,t1,t2,tint,tgap,cstart,cend)
c
c  Now generate the running mean.
c
	do j=1,Nx
	  pstart = 1
	  pend = 0
	  do i=1,Ny
	    if(cstart(i).gt.cend(i))then
	      acc = 0
	      n = 0
c
c  Can we reuse the previous summation? Subtract off the samples
c  we are no longer interested in. Only do this if it is worthwhile.
c
	    else
	      if(pend-cstart(i)+1.gt.cstart(i)-pstart)then
	        do i0=pstart,cstart(i)-1
		  if(iflag(j,i0).gt.0)then
		    acc = acc - array(j,i0)
		    n = n - 1
		  endif
	        enddo
c
c  Do not bother to try to reuse the previous summation. Start
c  from scratch.
c
	      else
		pend = cstart(i) - 1
		n = 0
		acc = 0
	      endif
c
c  Accumulate from the end of the last one.
c
	      do i0=pend+1,cend(i)
		if(iflag(j,i0).gt.0)then
		  acc = acc + array(j,i0)
		  n = n + 1
		endif
	      enddo
	    endif
c
c  Save the summation and the counters.
c
	    cnt(i) = n
	    sum(i) = acc
	    pstart = cstart(i)
	    pend = cend(i)
	  enddo
c
c  Subtract off the mean, and take the absolute value of the
c  difference.
c
	  do i=1,Ny
	    if(cnt(i).gt.0)
     *	      array(j,i) = abs( array(j,i) - sum(i)/cnt(i) )
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine AvRange(Ny,t1,t2,tint,tgap,cstart,cend)
c
	implicit none
	integer Ny
	real t1(Ny),t2(Ny),tint,tgap
	integer cstart(Ny),cend(Ny)
c
c  Determine the range of time slots to use in determining the running
c  mean.
c
c  Input:
c    t1,t2	Time ranges of each time slot.
c    Ny	Number of time slots.
c    tint,tgap	Largest integration and gap time.
c  Output:
c    cstart	First time slot to use for the running mean.
c    cend	Last time slot to use for the running mean.
c------------------------------------------------------------------------
	integer i
	integer pstart,pend
	logical more
c
c  Determine the channels to use in the running mean.
c
	pstart = 1
	pend   = 0
	do i=1,Ny	
c
c  If this is a gap, reset the accumulators, and set the cstart and
c  cend variables to minimise the work next time around.
c
	  if(t1(i).lt.-1)then
	    pstart = i+1
	    pend = i
	  else
c
c  Determine the limits of this running mean. We average times
c  "cstart" to "cend". "pstart" and "pend" are the limits of the
c  running mean for the previous time slot.
c
	    dowhile(t1(i)+t2(i)-t1(pstart)-t2(pstart).gt.tint)
	      pstart = pstart + 1
	    enddo
	    pend = max(pstart,pend)
	    more = .true.
	    dowhile(more)
	      more = pend+1.le.Ny
	      if(more) more = t1(pend+1).gt.-1
	      if(more) more = t1(pend+1)-t2(pend).lt.tgap
	      if(more) more = t2(pend+1)-t1(pstart).lt.tint
	      if(more) pend = pend + 1
	    enddo
	  endif
	  cstart(i) = pstart
	  cend(i) = pend
	enddo
c
	end
c************************************************************************
	subroutine Edit(array,iflag,Mx,Nx,Ny,SumoverX,SumoverY,
     *		NoverX,NoverY,isave,x0,y0,bmin,bmax,chan,
     *		t1,t2,chanoff)
c
	implicit none
	integer Mx, Nx, Ny
	real array(Mx,Ny),SumoverX(Ny),SumoverY(Nx)
	real bmin,bmax,t1(Ny), t2(Ny)
	integer iflag(Mx,Ny),NoverX(Ny),NoverY(Nx),isave(5)
	integer x0,y0,chan,chanoff
c
c  Apply an editting operation to the data, recompute the wedges,
c  and redisplay the necessary parts that need redisplaying.
c
c------------------------------------------------------------------------
	integer WIDTH
	parameter(WIDTH=20)
c
	integer i,j,FLAGVAL,Left,Bott,Rite,Topp
	character string*80
c
c  Give the user a message about what is going on.
c
	call FmtCmd(string,isave,t1,t2,Ny,chanoff)
	call output(string)
c
c  Apply the flagging operation to the iflag array, and recompute the
c  wedge values. 
c
	Bott = isave(1)
	Left = isave(2)
	Topp = isave(3)
	Rite = isave(4)
	FLAGVAL = isave(5)
	do j=Bott,Topp
	  if(t1(j).gt.-1)then
	    do i=Left,Rite
	      if(iflag(i,j).eq.FLAGVAL)then
	        continue
	      else if(FLAGVAL.eq.0)then
	        SumoverX(j) = SumoverX(j) - array(i,j)
	        NoverX(j) = NoverX(j) - 1
	        SumoverY(i) = SumoverY(i) - array(i,j)
	        NoverY(i) = NoverY(i) - 1
	      else if(FLAGVAL.eq.1)then
	        SumoverX(j) = SumoverX(j) + array(i,j)
	        NoverX(j) = NoverX(j) + 1
	        SumoverY(i) = SumoverY(i) + array(i,j)
	        NoverY(i) = NoverY(i) + 1
	      endif
	      iflag(i,j) = FLAGVAL
	    enddo
	  endif
	enddo
c
c  Redisplay the image.
c
	call DispIm(array(Left,Bott),iflag(Left,Bott),
     *	  Mx, Rite-Left+1, Topp-Bott+1, chan, x0+Left-1, y0+Bott-1,
     *	  bmin, bmax)
c
c  Redisplay the wedges.
c
	call DispWg(sumoverx(Bott), NoverX(Bott), Topp-Bott+1, 1, 
     *		chan, x0, y0+Bott-1, Nx, bmin, bmax, WIDTH)
	call DispWg(sumovery(Left), NoverY(Left), Rite-Left+1, 0,
     *		chan, x0+Left-1, y0, Ny, bmin, bmax, WIDTH)
c
	end
c************************************************************************
	subroutine Clip(array,iflag,Mx,Nx,Ny,SumoverX,SumoverY,
     *		NoverX,NoverY,isave,changes,x0,y0,bmin,bmax,chan,
     *		t1,t2,chanoff,cliplev)
c
	implicit none
	integer Mx, Nx, Ny
	real array(Mx,Ny),SumoverX(Ny),SumoverY(Nx)
	real bmin,bmax,t1(Ny), t2(Ny), sigma, cliplev
	integer iflag(Mx,Ny),NoverX(Ny),NoverY(Nx),isave(5,*)
	integer x0,y0,chan,chanoff, count, changes
c
c  Apply a clip operation to the data, recompute the wedges,
c  and redisplay the necessary parts that need redisplaying.
c
c------------------------------------------------------------------------
	integer WIDTH, MAXEDIT, MAXX, MAXY
	parameter(WIDTH=20, MAXEDIT=10000, MAXX=8192, MAXY=8640)
        real omedian, chnmed(MAXX), timemed(MAXY), medchn,
     -   medtime, chnsigma, timerms, buf(MAXY)
        integer chncount(MAXX), timecnt(MAXY)
c
	integer i,j,FLAGVAL,Left,Bott,Rite,Topp
	character string*80
c
c  Give the user a message about what is going on.
c
c	call FmtCmd(string,isave,t1,t2,Ny,chanoff)
        FLAGVAL = isave(5,changes)

c
c  Calculate the channel medians
c
        do i=1, Nx
          chncount(i) = 0
          chnmed(i) = 0
          do j=1, Ny
            if (iflag(i,j).ne.FLAGVAL)then
              chncount(i) = chncount(i) +1
              buf(chncount(i)) = array(i,j)
            endif
          enddo
          if (chncount(i).gt.0) 
     -      call median(buf, chncount(i), chnmed(i))
        enddo

c
c  Calculate the time medians
c
        do j=1, Ny
          timecnt(j) = 0
          timemed(j) = 0
          do i=1, Nx
            if (iflag(i,j).ne.FLAGVAL)then
              timecnt(j) = timecnt(j) +1
              buf(timecnt(j)) = array(i,j)
            endif
          enddo
          if (timecnt(j).gt.0) 
     -      call median(buf, timecnt(j), timemed(j))
        enddo

c
c  Calculate the overall channel median
c
        count = 0
        do i = 1, Nx
          if (chncount(i).gt.0) then
            count = count +1
            buf(count)=chnmed(i)
          endif
        enddo
        call median(buf,count,medchn)

c
c  Calculate the overall time median
c
        count = 0
        do j = 1, Ny
          if (timecnt(j).gt.0) then
            count = count +1
            buf(count)=timemed(j)
          endif
        enddo
        call median(buf,count,medtime)
c
c  Guess the overall median
c
        omedian = min (medchn,medtime)

c
c  Calculate the channel average deviation
c
        chnsigma = 0
        count = 0
        do i=1, Nx
          if (chncount(i).gt.0)then
            chnsigma = chnsigma + abs(chnmed(i)-medchn)
            count = count + 1
          endif
        enddo
        if (count.gt.0) chnsigma=chnsigma/count
        write(*,*) ' Nx=',Nx,' chncount=',count
c
c  Calculate the timeseries average deviation
c
        timerms = 0
        count = 0
        do j=1, Ny
          if (timecnt(j).gt.0)then
            timerms = timerms + abs(timemed(j)-medtime)
            count = count + 1
          endif
        enddo
        if (count.gt.0) timerms=timerms/count
        write(*,*) ' Ny=',Ny,' timecount=',count

c
c  Calculate the overall average deviation
c
        sigma = 0.
        count = 0 
        do i=1, Nx
          do j=1, Ny
            if (iflag(i,j).ne.FLAGVAL)then
              count = count + 1
              sigma = sigma + abs(array(i,j)-omedian)
            endif
          enddo
        enddo
        if (count.gt.0) sigma=sigma/count
       
        write(string,'(A,F9.5,A,F5.1,A,F9.5)')
     *   'Flagging channels with '//
     *   'abs(median - ',medchn,')>',
     *   cliplev,' * ',chnsigma
	call output(string)

        write(string,'(A,F9.5,A,F5.1,A,F9.5)') 
     *   'Flagging times    with '//
     *   'abs(median - ',medtime,')>',
     *   cliplev,' * ',timerms
	call output(string)

        write(string,'(A,F9.5,A,F5.1,A,F9.5)')
     *   'Clipping data     with abs(value-',omedian,')>',
     *   cliplev,' * ',sigma
        call output(string)
        
c
c  Apply the flagging operation to the iflag array, and recompute the
c  wedge values. 
c


c  First flag the bad channels

        do i= 1, Nx
          if (chncount(i).gt.0) then
            if (abs(chnmed(i)-medchn).gt.cliplev*chnsigma) then
              write(*,*) 'Flagging channel: ',i
              do j= 1, Ny
                iflag(i,j)=FLAGVAL
	        SumoverX(j) = SumoverX(j) - array(i,j)
	        NoverX(j) = NoverX(j) - 1
	        SumoverY(i) = SumoverY(i) - array(i,j)
	        NoverY(i) = NoverY(i) - 1
              enddo
              changes=changes+1
              if (changes.le.MAXEDIT)then
                isave(1,changes)=1
                isave(2,changes)=i
                isave(3,changes)=Ny
                isave(4,changes)=i
                isave(5,changes)=FLAGVAL
              endif
            endif
          endif
        enddo

C Now flag the bad times

        do j= 1, Ny
          if (timecnt(j).gt.0) then
            if (abs(timemed(j)-medtime).gt.cliplev*timerms) then
              write(*,*) 'Flagging time: ',j
              do i= 1, Nx
                iflag(i,j)=FLAGVAL
	        SumoverX(j) = SumoverX(j) - array(i,j)
	        NoverX(j) = NoverX(j) - 1
	        SumoverY(i) = SumoverY(i) - array(i,j)
	        NoverY(i) = NoverY(i) - 1
              enddo
              changes=changes+1
              if (changes.le.MAXEDIT)then
                isave(1,changes)=j
                isave(2,changes)=1
                isave(3,changes)=j
                isave(4,changes)=Nx
                isave(5,changes)=FLAGVAL
              endif
            endif
          endif
        enddo

C Finally clip the bad points

	Bott = 1
	Left = 1
	Topp = Ny
	Rite = Nx
	do j= 1, Ny
	  if(t1(j).gt.-1)then
	    do i=1, Nx
	      if(iflag(i,j).eq.FLAGVAL)then
	        continue
	      else if (abs(array(i,j)-omedian).gt.cliplev*sigma) then
	        SumoverX(j) = SumoverX(j) - array(i,j)
	        NoverX(j) = NoverX(j) - 1
	        SumoverY(i) = SumoverY(i) - array(i,j)
	        NoverY(i) = NoverY(i) - 1
  	        iflag(i,j) = FLAGVAL
                changes=changes+1
                if (changes.le.MAXEDIT)then
                   isave(1,changes)=j
                   isave(2,changes)=i
                   isave(3,changes)=j
                   isave(4,changes)=i
                   isave(5,changes)=FLAGVAL
                endif
	      endif
	    enddo
	  endif
	enddo
c
c  Redisplay the image.
c
	call DispIm(array(Left,Bott),iflag(Left,Bott),
     *	  Mx, Rite-Left+1, Topp-Bott+1, chan, x0+Left-1, y0+Bott-1,
     *	  bmin, bmax)
c
c  Redisplay the wedges.
c
	call DispWg(sumoverx(Bott), NoverX(Bott), Topp-Bott+1, 1, 
     *		chan, x0, y0+Bott-1, Nx, bmin, bmax, WIDTH)
	call DispWg(sumovery(Left), NoverY(Left), Rite-Left+1, 0,
     *		chan, x0+Left-1, y0, Ny, bmin, bmax, WIDTH)
c
	end
c***********************************************************************
      subroutine LdDisp(things, array, iflag, sumoverx, sumovery,
     *	Mx, Nx, Ny, NoverX, NoverY, chan, jx0, jy0, pmin, pmax, string)
c
      implicit none
      integer Mx, Nx, Ny, chan, jx0, jy0
      real pmin, pmax
      real array(Mx, Ny), sumoverx(Ny), sumovery(Nx)
      integer iflag(Mx, Ny), NoverX(Ny), NoverY(Nx)
      character string*(*), things*(*)
c
c  Loads an array on a Server device and attempts to find room for
c  two wedge summary strips on the sides of the array.  If necessary,
c  this routine autoscales the data to the maximum/minimum of the
c  array.  This routine is used just by TVCLIP.
c
c  Input:
c    things	  The things to display. Its composed of
c		    'w' -- wedges
c		    'i' -- the image.
c		    't' -- the text.
c    array(Nx,Ny) Input real array of data.
c    iflag(Nx,Ny) Data flags.
c    sumoverx(Ny) The sum of array over the x extent at each y position.
c    sumovery(Nx) The sum of array over the y extent at each x position.
c    Nx, Ny       X/Y working integer dimensions of the array.
c    NoverX,NoverY Counts in the SumoverX and SumoverY arrays.
c    chan         The TV channel to load the image.
c    jx0, jy0     The lower left corner pixel of the image.
c    pmin, pmax	  The minimum/maximum to be displayed.
c    string       The character string used to label the plot.
c                   Where the string is loaded on the plot is determined
c                   by the amount of space that remains after the image
c                   and wedges are loaded and tries to go in this order:
c                     1: centered in x below the plot;
c                     2: centered in x above the plot.
c                     3: centered in y left of the plot;
c                     4: centered in y right of the plot;
c
c  Output:
c    none
c
c-----------------------------------------------------------------------
      integer WIDTH, COLOR
      parameter (WIDTH = 20)
      parameter (COLOR = 40)
c
      integer maxxpix, maxypix, mxchan, nlev
      integer strlen, dir, jxw, jyw
c
c  Externals.
c
      integer Len1
c
c  Display the image.
c
      if(index(things,'i').ne.0)
     *	call DispIm(array,iflag,Mx,Nx,Ny,chan,jx0,jy0,pmin,pmax)
c
c  Load Wedges if there is sufficient room.
c
      if(index(things,'w').ne.0)then
	call DispWg(sumoverx, NoverX, Ny, 1, 
     *		chan, jx0, jy0, Nx, pmin, pmax, WIDTH)
	call DispWg(sumovery, NoverY, Nx, 0,
     *		chan, jx0, jy0, Ny, pmin, pmax, WIDTH)
      endif
c
c  Try to place the label in the proper quadrant.
c
      if(index(things,'t').ne.0)then
	strlen = 10 * Len1(string)
	call TvChar(maxxpix, maxypix, mxchan, nlev)
c
	jxw = jx0 + Nx + WIDTH
	if(jxw+WIDTH.gt.maxxpix) jxw = jx0 - 2*WIDTH
	if(jxw.lt.1) jxw = -1
c
	jyw = jy0 + Ny + WIDTH
	if(jyw+WIDTH.gt.maxypix) jyw = jy0 - 2*WIDTH
	if(jyw.lt.1) jyw = -1
c
	if (jyw .gt. 0) then
	  if (jyw .gt. jy0) then
           jyw = jy0 - WIDTH
          else
            jyw = jy0 + Ny + WIDTH
          endif
          if (jyw .gt. maxypix) jyw = -1
          if (jyw .lt. 1) jyw = -1
          jxw = jx0 + ((Nx - strlen) / 2)
          dir = 0
        else if (jxw .gt. 0) then
          if (jxw .gt. jx0) then
            jxw = jx0 - WIDTH
          else
            jxw = jx0 + Nx + WIDTH
          endif
          if (jxw .gt. maxxpix) jxw = -1
          if (jxw .lt. 1) jxw = -1
          jyw = jy0 + ((Ny + strlen) / 2)
          dir = 1
        endif
        strlen = Len1(string)
        if ((jxw .gt. 0) .and. (jyw .gt. 0) )
     *	  call TvText(jxw, jyw, chan, string, strlen, COLOR, dir)
      endif
c
      end
c***********************************************************************
      subroutine DispIm(array,iflag,Mx,Nx,Ny,chan,x0,y0,pmin,pmax)
c
      implicit none
      integer Mx, Nx, Ny, chan, x0, y0
      real array(Mx, Ny), pmin, pmax
      integer iflag(Mx,Ny)
c
c  Displays an array of real numbers on the TV device scaled to the
c  limits of the device.  An internal integer array is used to hold
c  the scaled data and quietly accounts for very large array
c  dimensions.
c
c  Input:
c    array(Nx,Ny) Input real array of values.
c    iflag(Nx,Ny) Flags
c    Nx, Ny       X/Y working dimensions of the array.
c    chan         The TV channel to load the image.
c    x0, y0       The lower left corner to start loading the image.
c    pmin         The minimum value of the array.
c    pmax         The maximum value of the array.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer data(MAXDIM)
c
      integer jx, jy, ichan
      integer m, x, y, Nitem, ival
      integer Nfirst, Nlast, Nsection
      integer xmax, ymax, mostchan, nlev
      real bzero, bscale
c
      if (pmin .eq. pmax) return
      call TvChar(xmax, ymax, mostchan, nlev)
      if ((chan .lt. 1) .or. (chan .gt. mostchan)) return
c
      ichan = chan
      nlev = nlev - 1
      bzero = pmin
      bscale = nlev / (pmax - pmin)
      Nsection = (Nx - 1) / MAXDIM
      jy = y0
c
      call TvScale(bzero, bscale)
      do y = 1, Ny
        jx = x0
        Nfirst = 1
        Nlast = MAXDIM
        do m = 0, Nsection
          if (m .eq. Nsection) Nlast = Nx
          Nitem = 0
          do x = Nfirst, Nlast
	    if(iflag(x,y).eq.0)then
	      ival = 0
	    else
	      ival = max(0,min(nint((array(x,y)-bzero)*bscale),nlev))
	    endif
            Nitem = Nitem + 1
	    data(Nitem) = ival
	  enddo
          call TvLine(jx, jy, ichan, data, Nitem)
          jx = jx + MAXDIM
          Nfirst = Nlast + 1
          Nlast = Nlast + MAXDIM
        enddo
        jy = jy + 1
      enddo
c
      end
c************************************************************************
	subroutine DispWg(Sum, NSum, n, dir, chan, x0, y0, Dn,
     *						pmin, pmax, width)
c
	implicit none
	integer n,dir,chan,x0,y0,Dn,width
	integer NSum(n)
	real Sum(n),pmin,pmax
c
c  Display the wedges.
c
c  Input:
c    x0,y0	Blc of the display.
c    dir	Display direction. 0 -- horizontal wedge (sum over Y).
c				   1 -- veritcal   wedge (sum over X).
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer data(MAXDIM)
	integer maxxpix,maxypix,maxchn,nlev,jxw,jyw, i, j, ival
	real bzero, bscale
c
	if(max(WIDTH,n).gt.MAXDIM)
     *	  call bug('f','Buffer overflow in DispWg')
c
c  Determine where the strips are located.
c
	call TvChar(maxxpix, maxypix, maxchn, nlev)
	jxw = x0
	jyw = y0
	nlev = nlev - 1
	bzero = pmin
	bscale = nlev / (pmax - pmin)
c
c  Do a horizontal wedge.
c
	if(dir.eq.0)then
	  jyw = jyw + Dn + WIDTH
	  if(jyw+WIDTH.ge.maxypix) jyw = y0 - 2*WIDTH
	  if(jyw.lt.1) return
	  do i=1,n
	    if(Nsum(i).gt.0)then
	      data(i) = min(nlev,max(0,
     *			  nint(bscale*(Sum(i)/NSum(i)-bzero))))
	    else
	      data(i) = 0
	    endif
	  enddo
	  do j=1,WIDTH
	    call TvLine(jxw, jyw, chan, data, n)
	    jyw = jyw + 1
	  enddo
c
c  Do a vertical wedge.
c
	else
	  jxw = jxw + Dn + WIDTH
	  if(jxw+WIDTH.gt.maxxpix) jxw = x0 - 2*WIDTH
	  if(jxw.lt.1) return
	  do j=1,n
	    if(Nsum(j).gt.0)then
	      ival = min(nlev,max(0,
     *			  nint(bscale*(Sum(j)/NSum(j)-bzero))))
	    else
	      ival = 0
	    endif
	    do i=1,WIDTH
	      data(i) = ival
	    enddo
	    call tvline(jxw,jyw, chan, data, WIDTH)
	    jyw = jyw + 1
	  enddo
	endif
c
	end
c************************************************************************
	subroutine FmtVal(string,x,y,val,iflag,t1,t2,ntime,chanoff)
c
	implicit none
	character string*(*)
	integer x,y,iflag,chanoff,ntime
	real val,t1(ntime),t2(ntime)
c
c  Format the value of a pixel.
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision time
	character pixval*10,flagval*5
c
c  Externals.
c
	character hangle*8,itoaf*4
c
	if(t1(y).lt.-1)then
	  if(t1(y+1).lt.t2(y-1).or.t1(y+1).gt.t2(y-1)+0.5)then
	    string = 'Location is a step in time'
	  else
	    time = 2*pi*mod(0.5*(t1(y+1) + t2(y-1)),1.0)
	    if(time.lt.0)time = time + 2*pi
	    string = 'Location is a break in the data near time '//
     *	      hangle(time)
	  endif
	else
	  write(pixval,'(1pe10.3)')val
	  flagval = 'BAD, '
	  if(iflag.gt.0)flagval = 'GOOD,'
	  time = 2*pi*mod(0.5*(t1(y)+t2(y)),1.0)
	  string = 'Pixel is '//flagval//' value '//pixval//
     *		', time '//hangle(time)//
     *		', channel '//itoaf(x+chanoff)
	endif
	end
c************************************************************************
	subroutine FmtCmd(string,isave,t1,t2,ntime,chanoff)
c
	implicit none
	character string*(*)
	integer isave(5),ntime,chanoff
	real t1(ntime),t2(ntime)
c
c  Nicely format an editting instruction.
c
c  Input:
c  Output:
c    string	The formatted command.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i1,i2,chan1,chan2,l
	double precision time1,time2
	character flagval*4
c
c  Externals.
c
	character itoaf*8,hangle*8
	integer len1
c
	i1 = isave(1)
	i2 = isave(3)
	if(t1(i1).lt.-1)i1 = i1 + 1
	if(t2(i2).lt.-1)i2 = i2 - 1
	if(i1.le.i2)then
	  time1 = 2*pi*mod(t1(i1),1.0)
	  if(time1.lt.0)time1 = time1 + 2*pi
	  time2 = 2*pi*mod(t2(i2),1.0)
	  if(time2.lt.0)time2 = time2 + 2*pi
	  chan1  = isave(2) + chanoff
	  chan2  = isave(4) + chanoff
	  flagval = 'BAD'
	  if(isave(5).gt.0) flagval = 'GOOD'
	  string = 'Changing times '//hangle(time1)//' to '//
     *	    hangle(time2)//', channels '//itoaf(chan1)
	  l = len1(string)
	  string(l+1:) = ' to '//itoaf(chan2)
	  l = len1(string)
	  string(l+1:) = ' to '//flagval
	endif
c
	end
c***********************************************************************
c  GetBox - Get a boxed region from the TV display.
c  image-data
c 
      subroutine getbox(Ctrl, x1, y1, x2, y2)
c
      implicit none
      integer x1, y1, x2, y2
      logical Ctrl
c
c  Retrieves a boxed region on a TV display.
c
c  This routine is currently used just by TVCLIP.
c
c  Input:
c    Ctrl     TRUE if control panel in use.
c
c  Output:
c    x1, y1   The lower left corner selected.
c    x2, y2   The upper right corner selected.
c
c--
c-----------------------------------------------------------------------
      integer k, x, y, button
c-----------------------------------------------------------------------
c
    1 continue
      call Report(Ctrl, 'Use the mouse to select the boxed region.')
      call Report(Ctrl, 'Use button [A] to set the lower left corner.')
      call Report(Ctrl, 'Use button [B] to set the upper right corner.')
      call Report(Ctrl, '(Buttons [A-D] are sometimes keys F3-F6)')
c
      k = 0
      x1 = 0
      y1 = 0
      x2 = 0
      y2 = 0
      do while (k .lt. 2)
        x = 0
        y = 0
        button = 0
        do while (button .eq. 0)
          call TvCursor(x, y, button)
        enddo
        if (button .le. 1) then
          x1 = x
          y1 = y
          k = k + 1
        else if (button .gt. 1) then
          x2 = x
          y2 = y
          k = k + 1
        endif
      enddo
      if ((x1 .le. 0) .or. (y1 .le. 0) .or.
     *    (x2 .le. 0) .or. (y2 .le. 0)) then
        call Report(Ctrl, ' ')
        call Report(Ctrl, 'Incorrect selection; try again.')
        goto 1
      endif
      if (x2 .lt. x1) then
        x = x1
        x1 = x2
        x2 = x
      endif
      if (y2 .lt. y1) then
        y = y1
        y1 = y2
        y2 = y
      endif
      end
c***********************************************************************
c  Report - Send a message to the user.
c  text-i/o
c 
      subroutine report(Ctrl, inline)
c
      implicit none
      logical Ctrl
      character inline*(*)
c
c  Displays a message to the user of the TV display.
c
c  This routine is currently used just by TVCLIP.
c
c  Input:
c    Ctrl     TRUE if control panel in use.
c    inline   Line of message.
c
c  Output:
c    none
c
c--
c-----------------------------------------------------------------------
      character last2*80, last3*80, last4*80
      integer len2, len3, len4, lenline
c
      integer Len1
c
      save last2, last3, last4
      data last2 /' '/
      data last3 /' '/
      data last4 /' '/
c-----------------------------------------------------------------------
c
      if (Ctrl) then
        len2 = Len1(last2)
        if (len2 .le. 0) then
          last2 = ' '
          len2 = 1
        endif
        call CtrlSeta('Line1', last2(1:len2))
        len3 = Len1(last3)
        if (len3 .le. 0) then
          last3 = ' '
          len3 = 1
        endif
        call CtrlSeta('Line2', last3(1:len3))
        len4 = Len1(last4)
        if (len4 .le. 0) then
          last4 = ' '
          len4 = 1
        endif
        call CtrlSeta('Line3', last4(1:len4))
c
        lenline = Len1(inline)
        if (lenline .gt. 0) then
          call CtrlSeta('Line4', inline(1:lenline))
        else
          call CtrlSeta('Line4', ' ')
        endif
        last2 = last3(1:len3)
        last3 = last4(1:len4)
        last4 = inline(1:lenline)
      else
        call Output(inline)
      endif
      end
