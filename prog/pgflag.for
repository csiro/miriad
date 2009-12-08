      program pgflag
c-----------------------------------------------------------------------
c
c= PGFLAG - displays waterfall plots of uv data and allows flagging
c& jbs
c: flagging
c+
c     PGFLAG is a MIRIAD task that allows interactive baseline
c     editing of a UV data set. It is much like the MIRIAD task
c     TVFLAG, but it uses the PGPLOT display devices instead of
c     of the TV devices, which should make it more accessible.
c
c     When this program is run, the first selectable baseline is
c     displayed as a greyscale waterfall plot. The x-axis is
c     labelled as channel number (increasing to the right), while
c     the y-axis represents time (later is toward the top of the
c     plot). The colour intensity represents the amplitude or phase
c     (depending on the value of the keyword ``mode'') of the
c     visibility. At the very right and bottom edges of the plot
c     are averages of the data over all the channels and times
c     respectively. Between the right-hand average and the main
c     plot is a colour wedge that shows the amplitude/phase scaling.
c     At the top of the plot is a box with information about the
c     baseline that is being viewed, the last taken measurement,
c     and a guide to the commonly required action keys.
c
c     The user may edit the data by selecting a range of channels
c     and times with the mouse, and then flagging or unflagging
c     them. Selecting a range of channels and times is done by
c     selecting two points - one with the left mouse button and
c     one with the right mouse button - that bound the desired
c     range. When a selection is made with either the left mouse
c     button or the right mouse button, a green point will be
c     placed on the screen at the selected location. If both a
c     left mouse selection and a right mouse selection have been
c     made, a green box will be displayed that will have the two
c     selected points as opposite corners. Selecting another point
c     with either the left mouse button or the right mouse button
c     will move the appropriate corner of the box to form a new
c     selection.
c
c     To act upon this selection, or the plot itself, the user
c     should press one of the following keys while the cursor is
c     inside the main plot area:
c
c     Exiting PGFLAG:
c       a                Abort the editing procedure and quit
c                        PGFLAG immediately without applying
c                        any flags.
c       q                Apply any flagging that was requested
c                        and then exit PGFLAG.
c
c     Editing data:
c       f                Flag the selected range of data on the
c                        currently displayed baseline only.
c       F                Flag the selected range of data on all
c                        the selectable baselines.
c       g                Unflag the selected range of data on the
c                        currently displayed baseline only.
c       G                Unflag the selected range of data on all
c                        the selectable baselines.
c       u                Undo the last flagging action - if the
c                        last flagging action was an undo of a
c                        previous flagging action, then the effect
c                        will be to redo the flagging.
c       1                Flag the selected range of data on all
c                        baselines A-n formed with the currently
c                        displayed antenna A.
c       !                Unflag the selected range of data on all
c                        baselines A-n formed with the currently
c                        displayed antenna A.
c       2                Flag the selected range of data on all
c                        baselines n-B formed with the currently
c                        displayed antenna B.
c       @                Unflag the selected range of data on all
c                        baselines n-B formed with the currently
c                        displayed antenna B.
c       rr               Remove all current user-specified flags
c                        on this baseline; r must be pressed twice
c                        in a row for safety as this procedure
c                        cannot be undone.
c       RR               Remove all current user-specified flags
c                        on all baselines; R must be pressed twice
c                        in a row for safety as this procedure
c                        cannot be undone.
c       ?                Print some information about how many flagging
c                        operations have been made so far in total
c                        and on this baseline.
c
c     Manipulating the selection:
c       c                Extend the selection box to include all
c                        the displayed channels; press it twice in a row
c                        to extend to all channels.
c       t                Extend the selection box to include all
c                        the displayed times; press it twice in a row to
c                        extend to all times.
c       C                Clear the selected points.
c
c     Manipulating the plot:
c       z                Zoom into the selected region, ie. make
c                        the selected region cover the entire plot,
c                        and display green arrows on the sides of
c                        the plot to indicate available scrolling
c                        directions.
c       Z                Unzoom to show all the available data.
c       s                Unzoom to show all the available channels.
c       S                Unzoom to show all the available times.
c       h                Move the plot left by half the range shown
c                        on the plot.
c       H                Move the plot left by the range shown on the
c                        plot.
c       j                Move the plot down by half the range shown
c                        on the plot.
c       J                Move the plot down by the range shown on the
c                        plot.
c       k                Move the plot up by half the range shown
c                        on the plot.
c       K                Move the plot up by the range shown on the
c                        plot.
c       l                Move the plot right by half the range shown
c                        on the plot.
c       L                Move the plot right by the range shown on the
c                        plot.
c       n                Display the next selectable baseline in the
c                        main plot, leaving the selection and zoom
c                        as is.
c       p                Display the previous selectable baseline in
c                        the main plot, leaving the selection and
c                        zoom as is.
c       x                Subtract the average channel value from each
c                        channel value in the main plot. Pressing this
c                        key again adds back the average channel value
c                        to each channel value. When the channel average
c                        is being subtracted, the channel average box
c                        at the bottom of the plot will be outlined in
c                        red, otherwise it will be outlined in white.
c       d                Subtract the average time value from each
c                        time value in the main plot. Pressing this
c                        key again adds back the average time value to
c                        each time value. When the time average is being
c                        subtracted, the time average box at the right
c                        of the plot will be outlined in red, otherwise
c                        it will be outlined in white.
c       ,                Fiddle the amplitude scale; press this key
c                        then click the left mouse button in the colour
c                        wedge to set the maximum value, or the right
c                        mouse button to set the minimum value.
c       .                Reset the user-set colour amplitude scale.
c       [                Enable switch to use only the currently
c                        displayed points when deciding the colour
c                        amplitude scale.
c       ]                Set the colour amplitude scale from the current
c                        selection region.
c       ;                Lock the colour amplitude scale until it is reset
c                        or fiddled.
c
c     Displaying information:
c       m                Display information about the sample
c                        underneath the cursor, including the channel
c                        number (and associated frequency), the
c                        time, and the amplitude (or phase depending
c                        on the ``mode'' setting); all this information
c                        is shown at the top left of the plot in the red
c                        box.
c       spacebar         Does the same thing as 'm', except it will show
c                        the currently displayed value, which will be
c                        different from the value displayed by 'm' if the
c                        averages have been subtracted from the display.
c       M                Locate the sample with the maximum value on
c                        the currently displayed baseline, and print
c                        information about it into the controlling
c                        terminal. Pressing it twice in succession will
c                        make PGFLAG create a selection region of 20chans
c                        and 20times centred on this sample.
c
c@ vis
c     Input visibility dataset to be flagged. No default.
c@ line
c     This is the normal linetype specification. See the help on
c     "line" for more information. The default is all channels.
c@ select
c     This selects which visibilities to be used. Default is all
c     visibilities. See the Users Guide for information about how
c     to specify uv data selection.
c     Default is all data
c@ stokes
c     Select Stokes parameter(s) or polarization(s) from:
c       xx, yy, xy, yx,  i, q, u, v,
c       rr, ll, rl, lr
c     Default is all polarizations or Stokes parameters present
c@ device
c     PGPLOT plot device/type, which must be interactive. No default.
c@ mode
c     Display ``amplitude'' or ``phase''. By default, ``amplitude''
c     is selected. For mode=``phase'', the phase is in degrees.
c@ options
c     Task enrichment parameters. Several can be given, separated by
c     commas. Minimum match is used. Possible values are:
c       selgen  Generate a file appropriate for selecting the bad
c               data (via a ``select'' keyword). The output is two text
c               files called ``blflag_flag.select'' (for flagging
c               operations), and ``blflag_unflag.select'' (for unflagging
c               operations). Unfortunately, since ``select'' does not
c               support the use of a ``channel'' selection, this option
c               is of limited use, but is supplied in case time-based
c               selection is all that is required.
c       nosrc   Do not cause a break in the display when the source
c               changes. Normally PGFLAG puts a gap in the display
c               whenever the source changes.
c       noapply Do not apply the flagging.
c     The following options can be used to disable calibration.
c       nocal   Do not apply antenna gain calibration.
c       nopass  Do not apply bandpass correction.
c       nopol   Do not apply polarisation leakage correction.
c--
c
c  History:
c    jbs 21Sep09  Original version. Copying blflag/tvflag.
c    jbs 30Sep09  First released version. Compiles without any warnings
c                 when the -Wall option is passed to gfortran. Comments
c                 for some subroutines are complete, but most still lack
c                 MIRIAD-compliant commenting. The doc-type header
c                 comments are present however. All desired features are
c                 implemented, but it seems that the "nosrc" option does
c                 not work.
c    jbs 02Oct09  Changed I0 format codes to I3 (or I6), as g77 does not
c                 like automatic width settings. Add the 'x' and 'd'
c                 average subtraction functionality.
c    jbs 06Oct09  Added colour amplitude scaling fiddle commands:
c                 , . [ ] ;
c                 Made PGFLAG ask whether quitting/aborting is really
c                 what the user wants to do.
c                 Fixed bug with flagging time sample before desired
c                 time sample.
c                 Made a few consistency checks so PGPLOT doesn't
c                 complain about bad ranges.
c    jbs 07Oct09  Fixed bug that caused too many flags to be applied to
c                 the data. Correctly accounts for and displays the
c                 number of good/bad flags before and after flagging,
c                 and the number of flags changed. Changed internal
c                 baseline flagging representation to allow for antenna
c                 based flagging. Implemented antenna based flagging.
c                 Implement throw-away flags option for this baseline
c                 and all baselines. (version 1.3)
c    jbs 08Oct09  Fixed bug that stopped PGFLAG from reversing through
c                 baselines that were present but completely flagged.
c                 Fixed date calculation issues as previous version
c                 was reporting data as day before. Fixed bug where
c                 flagging was not applied as the last time sample to
c                 be flagged was outside the range of the known time
c                 samples.
c    jbs 09Nov09  Added 'spacebar' function to make a measurement of
c                 the currently displayed value, leaving 'm' to display
c                 the original value. Changed some documentation based
c                 on suggestions from Vince. PGFLAG now plots a red
c                 box below the area of the top region that the
c                 measurements are printed in. Can now use HJKL to move
c                 zoom window faster. Changed max value finder key to
c                 'M', and made it print out actual time instead of
c                 time sample number. Average boxes are now recomputed
c                 and display the average of what is shown instead of
c                 only the average of the original array, so they
c                 update when the averages are subtracted. The box
c                 around each average is drawn in red whenever the
c                 average in that direction is subtracted. Fixed a
c                 bug that would cause the measured values (with
c                 spacebar) to revert to the original values after
c                 a few keypresses. The colour wedge now gets outlined
c                 in light-blue after the ',' key is pressed, and in
c                 purple if the scale has been fiddled, or white if it
c                 has not been. Fix problem with incorrect flag numbers
c                 being printed if we quit with no flagging to apply.
c                 Added action on '?' to print out info about the number
c                 of flagging actions made in total and on the current
c                 baseline.
c                 (version 1.4)
      implicit none
      include 'maxdim.h'
      include 'mirconst.h'
      include 'maxnax.h'
      include 'mem.h'
c
      integer MAXDAT,MAXTIME,MAXEDIT
      parameter(MAXDAT=5000000,MAXTIME=10000,MAXEDIT=5000)
c
      character version*(*)
      parameter(version='PGFlag: version 1.4 09-Nov-09')
      character device*80,xaxis*12,yaxis*12,uvflags*12,val*16
      logical selgen,noapply
      integer npol,tno,i,j,length
c
c Data storage.
c
      double precision day0,cfreq(MAXCHAN)
      integer lScr,nchan,ntime,nvis,chanoff,chanw,nbl,cbl
      integer newbl,tbl
      real t1(MAXTIME),ttol,curs_x,curs_y
      logical blpres(MAXBASE),nosrc,needplot,needread
      logical firstread(MAXBASE)
      parameter(ttol=1.0/86400.0)
      integer iFlg,iDat,curr_zooms(2,2),points(2,2)
      integer min_x_zoom,max_x_zoom,min_y_zoom,max_y_zoom
      character pressed*2,previous_pressed*2
      integer meas_channel,shift_x,shift_y,f_a1,f_a2,f_mode
      real meas_freq,meas_amp
      character meas_time*20,yn*20,status*60
      logical plot_top,plot_average,plot_main,plot_points
      logical do_flag,do_unflag,do_undoflag,subavgc,subavgt
      integer times(2,MAXEDIT)
      integer chans(2,MAXEDIT),nflags,aflags,nthis
      integer region_width,meastype
      logical bases(MAXBASE,MAXEDIT)
      logical flagval(MAXEDIT),some_unflagged,scale_locked
      logical going_forward,going_backward,fiddle_active
      logical use_fiddle,keep_looping,colour_from_window
      logical colour_from_region,max_makeregion
      real fiddle_min,fiddle_max,datamin,datamax,fiddlefraction
c
c Externals
c
      integer pgopen
      logical uvDatOpn
c
c Get user inputs
c
      call keyini
      call keya('device',device,' ')
      if (device.eq.' ') call bug('f','A PGPLOT device must be given')
      call GetAxis(xaxis,yaxis)
      call GetOpt(selgen,noapply,nosrc,uvflags)
      call uvDatInp('vis',uvflags)
      call keyfin
c
c  Set the default polarisation type if needed.
c
      call uvDatGti('npol',npol)
      if(npol.eq.0)call uvDatSet('stokes',0)
c     
c     Open the input data.
c     
      if(.not.uvDatOpn(tno))call bug('f','Error opening input')
c     
c     Open the plot device.
c     
      if(pgopen(device).ne.1)then
         call pgldev
         call bug('f','Unable to open PGPLOT device')
      endif
      call pgqinf('CURSOR',val,length)
      if(val.eq.'NO')call bug('f','PGPLOT device is not interactive')
      call pgask(.false.)
c
c     Use a scratch file for the data.
c
      call scropen(lScr)
      call CopyDat(tno,lScr,yaxis,nchan,t1,MAXTIME,ntime,day0,ttol,
     *             blpres,MAXBASE,nvis,chanoff,chanw,nosrc)
      call uvinfo(tno,'sfreq',cfreq)
c
c Determine the number of baselines.
c
      nflags=0
      nbl=0
      do i=1,MAXBASE
         if (blpres(i)) nbl=nbl+1
         firstread(i)=.true.
      enddo
c
c Allocate memory.
c
      call memalloc(iFlg,2*nchan*ntime,'i')
      call memalloc(iDat,2*nchan*ntime,'r')
c
c Loop through the baselines.
c
      curr_zooms(1,1)=1
      curr_zooms(2,1)=1
      curr_zooms(1,2)=nchan
      curr_zooms(2,2)=ntime
      call reset_points(points)
      do i=1,MAXBASE
         if (blpres(i)) then
            cbl=i
            exit
         endif
      enddo
      needread=.true.
      needplot=.true.
      meas_channel=9999
      meas_freq=999999.9999
      meas_time='99DEC9999-99:99:99.9'
      meas_amp=9999.999
      plot_top=.true.
      plot_average=.true.
      plot_main=.true.
      plot_points=.false.
      going_forward=.true.
      going_backward=.false.
      subavgc=.false.
      subavgt=.false.
      fiddle_active=.false.
      use_fiddle=.false.
      keep_looping=.true.
      colour_from_window=.false.
      scale_locked=.false.
      colour_from_region=.false.
      previous_pressed=' '
      do while (keep_looping)
         if (needread .eqv. .true.) then
            some_unflagged=.false.
            if (.not.scale_locked) then
               use_fiddle=.false.
            endif
            do while (some_unflagged.eqv..false.)
               call Gridit(memI(iFlg),memR(iDat),nchan,ntime,cbl,day0,
     *              lScr,nvis,t1,firstread(cbl),some_unflagged)
               call ApplyFlags(memI(iFlg),nchan,ntime,chans,times,
     *              bases,flagval,MAXEDIT,nflags,cbl,some_unflagged)
               if (some_unflagged.eqv..false.) then
                  if (going_forward) then
                     do i=cbl+1,MAXBASE
                        if (blpres(i)) then
                           newbl=i
                           exit
                        endif
                     enddo
                     if (newbl.eq.cbl) then
                        going_forward=.false.
                        going_backward=.true.
                     else
                        cbl=newbl
                     endif
                  elseif (going_backward) then
                     do i=1,cbl-1
                        if (blpres(i)) then
                           newbl=i
                        endif
                     enddo
                     if (newbl.eq.cbl) then
                        going_forward=.true.
                        going_backward=.false.
                     else
                        cbl=newbl
                     endif
                  endif
               endif
            enddo
            needread=.false.
            firstread(cbl)=.false.
         endif
c     Now plot the data.
         if (needplot .eqv. .true.) then
            call MakePlot(memI(iFlg),memR(iDat),nchan,ntime,cbl,
     *           curr_zooms,meas_channel,meas_freq,meas_time,meas_amp,
     *           plot_top,plot_average,plot_main,points,plot_points,
     *           yaxis,chanoff,chanw,subavgc,subavgt,datamin,datamax,
     *           fiddle_min,fiddle_max,use_fiddle,colour_from_window,
     *           colour_from_region,fiddle_active)
            needplot=.false.
            plot_top=.false.
            plot_average=.false.
            plot_main=.false.
            plot_points=.false.
            if (colour_from_region) then
               colour_from_region=.false.
               fiddle_min=datamin
               fiddle_max=datamax
               use_fiddle=.true.
            endif
         endif
         pressed=' '
         call pgband(7,0,0.0,0.0,curs_x,curs_y,pressed)
c     Do what the user wants and nobody gets hurt.
         if ((pressed(1:1).eq.'n').or.(pressed(1:1).eq.'p')) then
c     change the baseline
            if (pressed(1:1).eq.'n') then
c     next baseline requested
               going_forward=.true.
               going_backward=.false.
               newbl=cbl
               do i=cbl+1,MAXBASE
                  if (blpres(i)) then
                     newbl=i
                     exit
                  endif
               enddo
               if (newbl.ne.cbl) then
                  cbl=newbl
                  needread=.true.
                  needplot=.true.
                  plot_top=.true.
                  plot_average=.true.
                  plot_main=.true.
                  plot_points=.true.
               endif
            elseif (pressed(1:1).eq.'p') then
c     previous baseline requested
               newbl=cbl
               going_backward=.true.
               going_forward=.false.
               do i=1,cbl-1
                  if (blpres(i)) then
                     newbl=i
                  endif
               enddo
               if (newbl.ne.cbl) then
                  cbl=newbl
                  needread=.true.
                  needplot=.true.
                  plot_top=.true.
                  plot_average=.true.
                  plot_main=.true.
                  plot_points=.true.
               endif
            endif
         elseif ((pressed(1:1).eq.'m').or.
     *           (pressed(1:1).eq.' ')) then
c     make a measurement
c     check that we are within the range of the plot
            if (pressed(1:1).eq.'m') then
               meastype=0
            else
               meastype=1
            endif
            if (curs_x.gt.curr_zooms(1,1)) then
               if (curs_x.lt.curr_zooms(1,2)) then
                  if (curs_y.gt.curr_zooms(2,1)) then
                     if (curs_y.lt.curr_zooms(2,2)) then
                        call MakeMeasurement(memI(iFlg),memR(iDat),
     *                       nchan,ntime,curs_x,curs_y,day0,t1,
     *                       meas_channel,meas_freq,meas_time,
     *                       meas_amp,chanoff,chanw,cfreq,meastype)
                        needplot=.true.
                        plot_top=.true.
                     endif
                  endif
               endif
            endif
         elseif ((pressed(1:1).eq.'A').or.(pressed(1:1).eq.'X')) then
c     a mouse button has been clicked
            if (fiddle_active) then
c     fiddle the colour amplitude scale
               if ((curs_y.lt.curr_zooms(2,2)).and.
     *              (curs_y.gt.curr_zooms(2,1))) then
                  fiddlefraction=(curs_y-curr_zooms(2,1))/
     *                 (curr_zooms(2,2)-curr_zooms(2,1))
                  if (pressed(1:1).eq.'A') then
c     set the maximum value
                     fiddle_max=fiddlefraction*(datamax-datamin)+
     *                    datamin
                  else
c     set the minimum value
                     fiddle_min=fiddlefraction*(datamax-datamin)+
     *                    datamin
                  endif
                  fiddle_active=.false.
                  needplot=.true.
                  plot_top=.true.
                  plot_main=.true.
                  plot_average=.true.
                  plot_points=.true.
               endif
            else
c     add a point on the graph
               if ((curs_x.gt.curr_zooms(1,1)).and.
     *              (curs_x.lt.curr_zooms(1,2)).and.
     *              (curs_y.gt.curr_zooms(2,1)).and.
     *              (curs_y.lt.curr_zooms(2,2))) then
                  if (pressed(1:1).eq.'A') then
c     left mouse button
                     points(1,1)=int(curs_x)
                     points(1,2)=int(curs_y)
                  else
c     right mouse button
                     points(2,1)=int(curs_x)
                     points(2,2)=int(curs_y)
                  endif
                  plot_points=.true.
                  needplot=.true.
               endif
            endif
         elseif (pressed(1:1).eq.'z') then
c     zoom in
            if ((points(1,1).ne.-1).and.(points(1,2).ne.-1).and.
     *           (points(2,1).ne.-1).and.(points(2,2).ne.-1)) then
               min_x_zoom=min(points(1,1),points(2,1))
               max_x_zoom=max(points(1,1),points(2,1))
               min_y_zoom=min(points(1,2),points(2,2))
               max_y_zoom=max(points(1,2),points(2,2))
c     check that we start at 1, and end before the top
               min_x_zoom=max(min_x_zoom,1)
               min_y_zoom=max(min_y_zoom,1)
               max_x_zoom=min(max_x_zoom,nchan)
               max_y_zoom=min(max_y_zoom,ntime)
c     and check we don't have the same value for the range
               if (min_x_zoom.eq.max_x_zoom) then
                  min_x_zoom=max(1,min_x_zoom-1)
                  max_x_zoom=min(nchan,max_x_zoom+1)
               endif
               if (min_y_zoom.eq.max_y_zoom) then
                  min_y_zoom=max(1,min_y_zoom-1)
                  max_y_zoom=min(ntime,max_y_zoom+1)
               endif
               curr_zooms(1,1)=min_x_zoom
               curr_zooms(1,2)=max_x_zoom
               curr_zooms(2,1)=min_y_zoom
               curr_zooms(2,2)=max_y_zoom
               call reset_points(points)
               needplot=.true.
               plot_main=.true.
               plot_average=.true.
               plot_top=.true.
            endif
         elseif ((pressed(1:1).eq.'Z').or.(pressed(1:1).eq.'s').or.
     *           (pressed(1:1).eq.'S')) then
c     unzoom
            if ((pressed(1:1).eq.'Z').or.(pressed(1:1).eq.'s')) then
c     show all the channels
               curr_zooms(1,1)=1
               curr_zooms(1,2)=nchan
            endif
            if ((pressed(1:1).eq.'Z').or.(pressed(1:1).eq.'S')) then
c     show all the times
               curr_zooms(2,1)=1
               curr_zooms(2,2)=ntime
            endif
            needplot=.true.
            plot_main=.true.
            plot_average=.true.
            plot_top=.true.
            plot_points=.true.
         elseif ((pressed(1:1).eq.'h').or.(pressed(1:1).eq.'j').or.
     *           (pressed(1:1).eq.'k').or.(pressed(1:1).eq.'l').or.
     *           (pressed(1:1).eq.'H').or.(pressed(1:1).eq.'J').or.
     *           (pressed(1:1).eq.'K').or.(pressed(1:1).eq.'L')) then
c     move the window - each press moves the window half/all the
c     current size of the window in the appropriate direction
            if ((pressed(1:1).eq.'h').or.(pressed(1:1).eq.'l').or.
     *          (pressed(1:1).eq.'H').or.(pressed(1:1).eq.'L')) then
c     move right or left
               if ((pressed(1:1).eq.'h').or.(pressed(1:1).eq.'l')) then
                  shift_x=(curr_zooms(1,2)-curr_zooms(1,1))/2
               else
                  shift_x=curr_zooms(1,2)-curr_zooms(1,1)
               endif
               if ((pressed(1:1).eq.'h').or.(pressed(1:1).eq.'H')) then
c     move left
                  if ((curr_zooms(1,1)-shift_x).lt.1) then
                     shift_x=curr_zooms(1,1)-1
                  endif
                  curr_zooms(1,1)=curr_zooms(1,1)-shift_x
                  curr_zooms(1,2)=curr_zooms(1,2)-shift_x
               elseif ((pressed(1:1).eq.'l').or.
     *                 (pressed(1:1).eq.'L')) then
c     move right
                  if ((curr_zooms(1,2)+shift_x).gt.nchan) then
                     shift_x=nchan-curr_zooms(1,2)
                  endif
                  curr_zooms(1,1)=curr_zooms(1,1)+shift_x
                  curr_zooms(1,2)=curr_zooms(1,2)+shift_x
               endif
            else
c     move up or down
               if ((pressed(1:1).eq.'j').or.(pressed(1:1).eq.'k')) then
                  shift_y=(curr_zooms(2,2)-curr_zooms(2,1))/2
               else
                  shift_y=curr_zooms(2,2)-curr_zooms(2,1)
               endif
               if ((pressed(1:1).eq.'j').or.(pressed(1:1).eq.'J')) then
c     move down
                  if ((curr_zooms(2,1)-shift_y).lt.1) then
                     shift_y=curr_zooms(2,1)-1
                  endif
                  curr_zooms(2,1)=curr_zooms(2,1)-shift_y
                  curr_zooms(2,2)=curr_zooms(2,2)-shift_y
               elseif ((pressed(1:1).eq.'k').or.
     *                 (pressed(1:1).eq.'K')) then
c     move up
                  if ((curr_zooms(2,2)+shift_y).gt.ntime) then
                     shift_y=ntime-curr_zooms(2,2)
                  endif
                  curr_zooms(2,1)=curr_zooms(2,1)+shift_y
                  curr_zooms(2,2)=curr_zooms(2,2)+shift_y
               endif
            endif
            needplot=.true.
            plot_main=.true.
            plot_top=.true.
            call reset_points(points)
            plot_average=.true.
         elseif ((pressed(1:1).eq.'c').or.(pressed(1:1).eq.'t')) then
c     extend the range of the selection to all viewed channels
c     or all viewed time
            if (pressed(1:1).eq.'c') then
c     extend to all channels
               if (previous_pressed.ne.'c') then
                  points(1,1)=curr_zooms(1,1)
                  points(2,1)=curr_zooms(1,2)
               else
                  points(1,1)=1
                  points(2,1)=nchan
               endif
            elseif (pressed(1:1).eq.'t') then
c     extend to all times
               if (previous_pressed.ne.'t') then
                  points(1,2)=curr_zooms(2,1)
                  points(2,2)=curr_zooms(2,2)
               else
                  points(1,2)=1
                  points(2,2)=ntime
               endif
            endif
            needplot=.true.
            plot_points=.true.
         elseif ((pressed(1:1).eq.'f').or.(pressed(1:1).eq.'F').or.
     *           (pressed(1:1).eq.'u').or.(pressed(1:1).eq.'g').or.
     *           (pressed(1:1).eq.'G').or.(pressed(1:1).eq.'1').or.
     *           (pressed(1:1).eq.'!').or.(pressed(1:1).eq.'2').or.
     *           (pressed(1:1).eq.'@')) then
c     flag the data in the selection box
            do_flag=.false.
            do_unflag=.false.
            do_undoflag=.false.
            do i=1,MAXANT
               do j=i,MAXANT
                  tbl=((j-1)*j)/2+i
                  if (cbl.eq.tbl) then
                     f_a1=i
                     f_a2=j
                     exit
                  endif
               enddo
            enddo
            if ((pressed(1:1).eq.'f').or.(pressed(1:1).eq.'F')) then
               do_flag=.true.
               if (pressed(1:1).eq.'f') then
                  f_mode=1
               else
                  f_mode=4
               endif
            elseif ((pressed(1:1).eq.'g').or.(pressed(1:1).eq.'G'))
     *           then
               do_unflag=.true.
               if (pressed(1:1).eq.'g') then
                  f_mode=1
               else
                  f_mode=4
               endif
            elseif (pressed(1:1).eq.'u') then
               do_undoflag=.true.
               f_mode=4
            elseif ((pressed(1:1).eq.'1').or.(pressed(1:1).eq.'2')) then
               do_flag=.true.
               if (pressed(1:1).eq.'1') then
                  f_mode=2
               else
                  f_mode=3
               endif
            elseif ((pressed(1:1).eq.'!').or.(pressed(1:1).eq.'@')) then
               do_unflag=.true.
               if (pressed(1:1).eq.'!') then
                  f_mode=2
               else
                  f_mode=3
               endif
            endif
            call FlagData(points,f_a1,f_a2,f_mode,
     *        do_flag,do_unflag,do_undoflag,chans,times,
     *        bases,flagval,MAXEDIT,nflags,day0,t1,ntime,chanoff,
     *        chanw)
            call ApplyFlags(memI(iFlg),nchan,ntime,chans,times,
     *        bases,flagval,MAXEDIT,nflags,cbl,some_unflagged)
            needplot=.true.
            plot_top=.true.
            plot_average=.true.
            plot_main=.true.
            plot_points=.true.
         elseif (pressed(1:1).eq.'C') then
c     clears the points - an expert option
            call reset_points(points)
            needplot=.true.
            plot_main=.true.
         elseif (pressed(1:1).eq.'M') then
c     locate the maximum value - an expert option
            region_width=10
            if (previous_pressed.ne.'M') then
               max_makeregion=.false.
            else
               max_makeregion=.true.
            endif
            call LocateMax(memi(iFlg),nchan,ntime,memr(iDat),chanoff,
     *           chanw,region_width,max_makeregion,points,day0,t1)
            if (max_makeregion) then
               needplot=.true.
               plot_points=.true.
            endif
         elseif (pressed(1:1).eq.'x') then
            subavgc=.not.subavgc
            subavgt=.false.
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_average=.true.
            plot_top=.true.
         elseif (pressed(1:1).eq.'d') then
            subavgt=.not.subavgt
            subavgc=.false.
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_average=.true.
            plot_top=.true.
         elseif (pressed(1:1).eq.',') then
c     fiddle the colour amplitude scale
            call output('Select new min/max amplitude.')
            fiddle_active=.true.
            if (use_fiddle) then
               datamin=fiddle_min
               datamax=fiddle_max
            endif
            use_fiddle=.true.
            fiddle_min=datamin
            fiddle_max=datamax
            colour_from_window=.false.
            scale_locked=.false.
            needplot=.true.
            plot_main=.true.
         elseif (pressed(1:1).eq.'.') then
            call output('Resetting colour amplitude scaling.')
            use_fiddle=.false.
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_top=.true.
            plot_average=.true.
            colour_from_window=.false.
            scale_locked=.false.
         elseif (pressed(1:1).eq.'q') then
c     quit, applying the flagging
            yn=' '
            write(*,*) ''
            write(*,*) 'Really quit and apply flagging? y[n]'
            read(*,*) yn
            if (yn(1:1).eq.'y') then
               keep_looping=.false.
            endif
         elseif (pressed(1:1).eq.'a') then
c     abort, discarding all flags
            yn=' '
            write(*,*) ''
            write(*,*) 'Really abort, discarding all flagging? y[n]'
            read(*,*) yn
            if (yn(1:1).eq.'y') then
               keep_looping=.false.
            endif
         elseif (pressed(1:1).eq.'[') then
            if (.not.colour_from_window) then
               call output(
     *         'Setting colour amplitude scale from window.')
               colour_from_window=.true.
            else
               call output(
     *         'Setting colour amplitude scale from baseline.')
               colour_from_window=.false.
            endif
            colour_from_region=.false.
            use_fiddle=.false.
            scale_locked=.false.
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_top=.true.
            plot_average=.true.
         elseif (pressed(1:1).eq.']') then
            call output('Setting colour amplitude scale from region.')
            colour_from_window=.false.
            colour_from_region=.true.
            use_fiddle=.false.
            scale_locked=.false.
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_top=.true.
            plot_average=.true.
         elseif (pressed(1:1).eq.';') then
            if (.not.use_fiddle) then
               fiddle_min=datamin
               fiddle_max=datamax
               use_fiddle=.true.
            endif
            scale_locked=.true.
         elseif ((pressed(1:1).eq.'r').and.
     *           (previous_pressed(1:1).eq.'r')) then
c     remove flags from this baseline
            if (nflags.lt.0) then
               aflags=abs(nflags)-1
            else
               aflags=nflags
            endif
            do i=1,aflags
               bases(cbl,i)=.false.
            enddo
            call ApplyFlags(memI(iFlg),nchan,ntime,chans,times,
     *           bases,flagval,MAXEDIT,nflags,cbl,some_unflagged)
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_top=.true.
            plot_average=.true.
         elseif ((pressed(1:1).eq.'R').and.
     *           (previous_pressed(1:1).eq.'R')) then
c     remove flags from all baselines
            if (nflags.lt.0) then
               aflags=abs(nflags)-1
            else
               aflags=nflags
            endif
            do i=1,aflags
               do j=1,MAXBASE
                  bases(j,i)=.false.
               enddo
            enddo
            call ApplyFlags(memI(iFlg),nchan,ntime,chans,times,
     *           bases,flagval,MAXEDIT,nflags,cbl,some_unflagged)
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_top=.true.
            plot_average=.true.
         elseif (pressed(1:1).eq.'?') then
c     print some info on the flagging done so far
            do i=1,60
               status(i:i)=' '
            enddo
            call output(' ')
            if (nflags.lt.0) then
               aflags=abs(nflags)-1
            else
               aflags=nflags
            endif
            call output('Flagging info:')
            if (aflags.eq.0) then
               call output(' No flagging on any baseline.')
            else
               write(status,'(A,I6,A)') '                     Total: ',
     *               aflags,' flags'
               call output(status)
            endif
            nthis=0
            do i=1,aflags
               if (bases(cbl,i)) then
                  nthis=nthis+1
               endif
            enddo
            if (aflags.gt.0) then
               write(status,'(A,I6,A)') ' Applying to this baseline: ',
     *               nthis,' flags'
               call output(status)
            endif
         endif
         previous_pressed=pressed
      enddo
c
c     Apply the flagging
c
      if (pressed(1:1).eq.'q') then
         if (.not.noapply) then
            call WriteFlags(bases,chans,times,flagval,nflags,tno,t1,
     *        day0,ntime,chanoff,chanw,selgen)
         endif
      else
         call output('PGFLAG aborted at user request.')
      endif
c     
c Release the scratch file and memory.
c
      call scrclose(lScr)
      call memfree(iFlg,2*nchan*ntime,'i')
      call memfree(iDat,2*nchan*ntime,'r')
c
c Close the PGPLOT device.
c
      call pgclos()
c
c     Close the file
c
      call uvDatCls

      end
c************************************************************************
      subroutine WriteFlags(bases,chans,times,flagval,nflags,tno,t1,
     *  day0,ntime,chanoff,chanw,selgen)
c
      implicit none
      include 'maxdim.h'
      integer nflags,ntime,tno,chanoff,chanw
      integer chans(2,nflags),times(2,nflags)
      logical bases(MAXBASE,nflags)
      logical flagval(nflags),selgen
      real t1(ntime)
      double precision day0
c
c  Write out the user generated flags to the UV file.
c
c  Input:
c    bases    the first and last baselines to flag
c    chans    the first and last channels to flag
c    times    the first and last times to flag
c    flagval  whether to flag or unflag
c    nflags   the number of flagging edits to apply
c    tno      the UV filehandle
c    t1       array of times
c    day0     the Julian date at midnight UT on the day of the first
c             time sample
c    ntime    the number of time samples in t1
c    chanoff  the channel offset as specified by the line parameter
c    chanw    the channel width as specified by the line parameter
c    selgen   switch to determine whether to output selgen files
c  Output:
c    none
c-----------------------------------------------------------------------
      integer isave(5),nchan,ant1,ant2,bl,i,j,lu,lf,iostat,l,k
      character flagstring*120,selectline*120
      double precision preamble(4)
      complex data(MAXCHAN)
      logical flags(MAXCHAN),flagged,more
      real t
      integer oldflags_bad,oldflags_good,newflags_bad,newflags_good
      integer flags_goodtobad,flags_badtogood,nflagged,fbl
      integer t_a1,t_a2,f_a1,time1,time2
      character outline*256
c
c     Externals
c
      integer len1
c
      oldflags_bad=0
      oldflags_good=0
      newflags_bad=0
      newflags_good=0
      flags_goodtobad=0
      flags_badtogood=0
      if (nflags.lt.0) then
         nflags=abs(nflags)-1
      endif
      if (nflags.ge.0) then
         call output('Applying changes to dataset')
         call hisopen(tno,'append')
         call hiswrite(tno,'PGFLAG')
         call hisinput(tno,'PGFLAG')
         call uvrewind(tno)
c     write out the history of the flagging
         if (selgen) then
            call txtopen(lf,'pgflag_flag.select','new',iostat)
            if (iostat.ne.0) then
               call bug('w',
     *           'Error opening output text file pgflag_flag.select')
               call bugno('w',iostat)
            endif
            call txtopen(lu,'pgflag_unflag.select','new',iostat)
            if (iostat.ne.0) then
               call bug('w',
     *           'Error opening output text file pgflag_unflag.select')
               call bugno('w',iostat)
            endif
         endif
         do i=1,nflags
c     determine what type of flagging
            nflagged=0
            do j=1,MAXBASE
               if (bases(j,i).eqv..true.) then
                  nflagged=nflagged+1
                  fbl=j
               endif
            enddo
            if (nflagged.eq.1) then
               isave(1)=1
               isave(2)=fbl
            elseif (nflagged.eq.MAXBASE) then
               isave(1)=4
               isave(2)=fbl
            else
c     must be flagging based on one antenna; figure out which one
               t_a1=-1
               t_a2=-1
               f_a1=-1
               do l=1,MAXBASE
                  if (bases(l,i).eqv..true.) then
                     do j=1,MAXANT
                        do k=j,MAXANT
                           fbl=((k-1)*k)/2+j
                           if (fbl.eq.l) then
                              if (t_a1.eq.-1) then
                                 t_a1=j
                                 t_a2=k
                              elseif (f_a1.eq.-1) then
                                 if ((j.eq.t_a1).or.(j.eq.t_a2)) then
                                    f_a1=j
                                 elseif ((k.eq.t_a1).or.
     *                                   (k.eq.t_a2)) then
                                    f_a1=k
                                 endif
                              endif
                           endif
                        enddo
                     enddo
                  endif
               enddo
               isave(1)=2
               isave(2)=f_a1
            endif
            isave(3)=chans(1,i)
            isave(4)=chans(2,i)
            if (flagval(i).eqv..true.) then
               isave(5)=0
            else
               isave(5)=1
            endif
            if (nflagged.gt.0) then
               time1=times(1,i)+1
               time2=times(2,i)+1
               if (time1.gt.ntime) then
                  time1=ntime
               endif
               if (time2.gt.ntime) then
                  time2=ntime
               endif
               call FmtCmd(flagstring,isave,t1(time1),
     *              t1(time2),chanoff,chanw,day0,selectline)
               call hiswrite(tno,'PGFLAG: '//flagstring)
               if (selgen) then
                  l=len1(selectline)
                  more=.false.
                  do j=i+1,nflags
                     if (flagval(j).eqv.flagval(i)) then
                        more=.true.
                     endif
                  enddo
                  if (more) then
                     selectline(l+1:)=',or'
                     l=len1(selectline)
                  endif
                  if (flagval(i)) then
                     call txtwrite(lf,selectline,l,iostat)
                     if (iostat.ne.0) then
                        call bug('w',
     *                       'Error writing to file blflag_flag.select')
                        call bugno('w',iostat)
                     endif
                  else
                     call txtwrite(lu,selectline,l,iostat)
                     if (iostat.ne.0) then
                        call bug('w',
     *               'Error writing to file blflag_unflag.select')
                        call bugno('w',iostat)
                     endif
                  endif
               endif
            endif
         enddo
c     do the flagging
         call uvread(tno,preamble,data,flags,MAXCHAN,nchan)
         do while (nchan.gt.0)
            do i=1,nchan
               if (flags(i).eqv..true.) then
                  oldflags_good=oldflags_good+1
               else
                  oldflags_bad=oldflags_bad+1
               endif
            enddo
            t=preamble(3)-day0
            flagged=.false.
            call basant(preamble(4),ant1,ant2)
            bl=((ant2-1)*ant2)/2+ant1
            do i=1,nflags
               if (bases(bl,i).eqv..true.) then
                  time1=times(1,i)+1
                  time2=times(2,i)+1
                  if (time1.gt.ntime) then
                     time1=ntime
                  endif
                  if (time2.gt.ntime) then
                     time2=ntime
                  endif
                  if ((t1(time1).le.t).and.
     *                (t1(time2).ge.t)) then
                     flagged=.true.
                     do j=chans(1,i),chans(2,i)
                        flags(j)=.not.flagval(i)
                        if (flagval(i).eqv..false.) then
                           flags_badtogood=flags_badtogood+1
                        else
                           flags_goodtobad=flags_goodtobad+1
                        endif
                     enddo
                  endif
               endif
            enddo
            if (flagged) call uvflgwr(tno,flags)
            do i=1,nchan
               if (flags(i).eqv..true.) then
                  newflags_good=newflags_good+1
               else
                  newflags_bad=newflags_bad+1
               endif
            enddo
            call uvread(tno,preamble,data,flags,MAXCHAN,nchan)
         enddo
         call hisclose(tno)
         if (selgen) then
            call txtclose(lf)
            call txtclose(lu)
         endif
      endif
c     summary of flagging
      call output('Counts of correlations within selected channels')
      write(outline,'( A8,''  Originally  Currently'')') 'channel'
      call output(outline)
      write(outline,'( ''Good:  '', 3X, I10, 1X, I10 )')
     *     oldflags_good,newflags_good
      write(outline(len1(outline)+1:),
     * '(4x, ''Changed to bad: '', I10 )') flags_goodtobad
      call output(outline)
      write(outline,'( ''Bad:   '', 3X, I10, 1X, I10 )')
     *     oldflags_bad,newflags_bad
      write(outline(len1(outline)+1:),
     * '(4x, ''Changed to good:'', I10 )') flags_badtogood
      call output(outline)
      
c
      end
c************************************************************************
      subroutine LocateMax(iflag,nchan,ntime,valarray,chanoff,chanw,
     *     regwidth,makesel,points,day0,t1)
c
      implicit none
      integer nchan,ntime,chanoff,chanw,regwidth
      integer iflag(nchan,ntime,2),points(2,2)
      real valarray(nchan,ntime,2),t1(ntime)
      logical makesel
      double precision day0
c
c  Locate the maximum value on this baseline, and output some info
c  about its value and location.
c
c  Input:
c    iflag     Array of flags for the currently displayed baseline.
c    nchan     The number of channels present in the data.
c    ntime     The number of time samples present in the data.
c    valarray  Array of amp/phase values for the currently displayed
c              baseline.
c    chanoff   The channel offset as specified by the line parameter.
c    chanw     The channel width as specified by the line parameter.
c    regwidth  The width of the region to use for selection purposes.
c    makesel   A switch to make this routine generate a selection box.
c    t1        Array of times.
c    day0      The Julian date at midnight UT on the day of the first
c              time sample.
c  Output:
c    points    A selection centred on the maximum value, with as many
c              as regwidth channels/time samples on each side.
c-----------------------------------------------------------------------
      integer i,j,xloc,yloc
      real maxval
      logical setval
      character maxstatus*256,maxtime*20,mon*3
      integer day,month,year,ierr,hour,minute,startday
      real second,thist
c
c     first find the maximum value
c
      setval=.true.
      do i=1,nchan
         do j=1,ntime
            if (iflag(i,j,2).eq.1) then
               if (setval) then
                  maxval=valarray(i,j,1)
                  xloc=i
                  yloc=j
                  setval=.false.
               else
                  if (valarray(i,j,1).gt.maxval) then
                     maxval=max(maxval,valarray(i,j,1))
                     xloc=i
                     yloc=j
                  endif
               endif
            endif
         enddo
      enddo
c
      if (.not.makesel) then
         startday=int(day0+real(int(t1(int(yloc)))))+1
         call julian_to_date(startday,day,month,year,ierr)
         thist=t1(int(yloc))*24.0
         hour=int(thist)
         thist=(thist-int(hour))*60.0
         minute=int(thist)
         thist=(thist-int(minute))*60.0
         second=thist
         call monthstring(month,mon)
         write(maxtime,'(I2.2,A3,I4.4,A1,I2.2,A1,I2.2,A1,F4.1)')
     *     day,mon,year,'-',hour,':',minute,':',second
         if (maxtime(17:17).eq.' ') maxtime(17:17)='0'
         write(maxstatus,'(A,F10.4,A,I6,A,A)') 'maximum value ',
     *        valarray(xloc,yloc,1),' at chan = ',chanoff+xloc*chanw,
     *        ' time = ',maxtime
         call output(maxstatus)
      else
         points(1,1)=max(1,xloc-regwidth)
         points(1,2)=max(1,yloc-regwidth)
         points(2,1)=min(nchan,xloc+regwidth)
         points(2,2)=min(ntime,yloc+regwidth)
      endif
c      write(*,*) 'maximum value at',valarray(xloc,yloc,1),xloc,yloc
c
      end
c************************************************************************
      subroutine ApplyFlags(iflag,xdim,ydim,chans,times,bases,
     *  flagval,MAXEDIT,nflags,bl,some_unflagged)
c
      include 'maxdim.h'
      integer MAXEDIT,xdim,ydim,nflags,bl
      integer iflag(xdim,ydim,2),chans(2,MAXEDIT)
      integer times(2,MAXEDIT)
      logical bases(MAXBASE,MAXEDIT)
      logical flagval(MAXEDIT),some_unflagged
c
c  Apply the user generated flags to the currently displayed
c  baseline.
c
c  Input:
c    iflag           Array of flags for the currently displayed
c                    baseline.
c    xdim            The number of channels present in the data.
c    ydim            The number of time samples present in the data.
c    chans           The first and last channels to flag.
c    times           The first and last times to flag.
c    bases           The first and last baselines to flag.
c    flagval         Whether to flag or unflag.
c    MAXEDIT         The maximum number of edits that can be held
c                    in the chans, times, bases and flagval arrays.
c    nflags          The number of flagging edits to apply.
c    bl              The currently displayed baseline ID.
c  Output:
c    some_unflagged  Status switch indicating whether there are
c                    unflagged visibilites on this baseline.
c-----------------------------------------------------------------------
      implicit none
      integer i,j,k,aflags
c
c     Initialise the flags
c
      do i=1,xdim
         do j=1,ydim
            iflag(i,j,2)=iflag(i,j,1)
         enddo
      enddo
c
c     Apply the flags we have now
c
      if (nflags.lt.0) then
         aflags=abs(nflags)-1
      else
         aflags=nflags
      endif
      do i=1,aflags
         if (bases(bl,i).eqv..true.) then
            do j=chans(1,i),chans(2,i)
               do k=times(1,i),times(2,i)
                  if (flagval(i)) then
                     iflag(j,k,2)=0
                  else
                     some_unflagged=.true.
                     iflag(j,k,2)=1
                  endif
               enddo
            enddo
         endif
      enddo
c
      end
c************************************************************************
      subroutine FlagData(points,a1,a2,mode,flag,
     *  unflag,undo,chans,times,bases,flagval,MAXEDIT,
     *  nflags,day0,t1,ntime,chanoff,chanw)
c
      include 'maxdim.h'
      integer points(2,2),a1,a2,mode,MAXEDIT,ntime
      integer chans(2,MAXEDIT),chanoff,chanw
      integer times(2,MAXEDIT),nflags
      logical bases(MAXBASE,MAXEDIT)
      real t1(ntime)
      logical flag,unflag,undo,flagval(MAXEDIT)
      double precision day0
c
c  Flag the data in the user-selected region.
c
c  Input:
c    points   The bounding points of the selection region.
c    a1       The first antenna for flagging purposes.
c    a2       The second antenna for flagging purposes.
c    mode     How to use a1 & a2: 1 = flag baseline a1-a2
c                                 2 = flag all baselines containing a1
c                                 3 = flag all baselines containing a2
c                                 4 = flag all baselines
c    flag     A switch to specify that the data should be flagged.
c    unflag   A switch to specify that the data should be unflagged.
c    undo     A switch to specify that the last flagging action
c             should be reversed.
c    chans    The first and last channels to flag.
c    times    The first and last channels to flag.
c    bases    The first and last baselines to flag.
c    flagval  Whether to flag or unflag.
c    MAXEDIT  The maximum number of edits that can be held in the
c             chans, times, bases and flagval arrays.
c    nflags   The number of flagging edits currently held in the
c             chans, times, bases and flagval arrays.
c    day0     The Julian date at midnight UT on the day of the
c             first time sample.
c    t1       Array of times.
c    ntime    The number of samples in t1.
c    chanoff  The channel offset as specified by the line parameter.
c    chanw    The channel width as specified by the line parameter.
c  Output:
c    chans    The first and last channels to flag, updated to contain
c             the new flagging action.
c    times    The first and last times to flag, updated to contain
c             the new flagging action.
c    bases    The first and last baselines to flag, updated to contain
c             the new flagging action.
c    flagval  Whether to flag or unflag, updated to contain the new
c             flagging action.
c    nflags   The number of flagging edits now held in the chans,
c             times, bases and flagval arrays.
c-----------------------------------------------------------------------
      implicit none
      integer minx,maxx,miny,maxy,i,j,tbl
      integer isave(5),time1,time2
      character flagstring*120,selectline*120
c
c     check that we have a full selection box
      if ((points(1,1).ne.-1).and.(points(1,2).ne.-1).and.
     *    (points(2,1).ne.-1).and.(points(2,2).ne.-1)) then
         minx=min(points(1,1),points(2,1))
         maxx=max(points(1,1),points(2,1))
         miny=min(points(1,2),points(2,2))
         maxy=max(points(1,2),points(2,2))
         if (flag .or. unflag) then
            if (nflags.lt.0) then
               nflags=abs(nflags)
            else
               nflags=nflags+1
            endif
            chans(1,nflags)=minx
            chans(2,nflags)=maxx
            isave(3)=minx
            isave(4)=maxx
            do while ((t1(miny+1).lt.0.0).or.(t1(miny+1).gt.1.0))
               miny=miny+1
            enddo
            times(1,nflags)=miny
            do while ((t1(maxy+1).lt.0.0).or.(t1(maxy+1).gt.1.0))
               maxy=maxy-1
            enddo
            times(2,nflags)=maxy
            do i=1,MAXBASE
               bases(i,nflags)=.false.
               if (mode.eq.4) then
                  bases(i,nflags)=.true.
               endif
            enddo
            do i=1,MAXANT
               do j=i,MAXANT
                  tbl=((j-1)*j)/2+i
                  if (mode.eq.1) then
                     if (((i.eq.a1).and.(j.eq.a2)).or.
     *                   ((i.eq.a2).and.(j.eq.a1))) then
                        if (tbl.le.MAXBASE) then
                           bases(tbl,nflags)=.true.
                        endif
                     endif
                  elseif (mode.eq.2) then
                     if ((i.eq.a1).or.(j.eq.a1)) then
                        if (tbl.le.MAXBASE) then
                           bases(tbl,nflags)=.true.
                        endif
                     endif
                  elseif (mode.eq.3) then
                     if ((i.eq.a2).or.(j.eq.a2)) then
                        if (tbl.le.MAXBASE) then
                           bases(tbl,nflags)=.true.
                        endif
                     endif
                  elseif (mode.eq.4) then
                     if (tbl.le.MAXBASE) then
                        bases(tbl,nflags)=.true.
                     endif
                  endif
               enddo
            enddo
            isave(1)=mode
            if (mode.eq.1) then
               isave(2)=((a2-1)*a2)/2+a1
            elseif (mode.eq.2) then
               isave(2)=a1
            elseif (mode.eq.3) then
               isave(2)=a2
            elseif (mode.eq.4) then
               isave(2)=-1
            endif
            if (flag) then
               flagval(nflags)=.true.
               isave(5)=0
            elseif (unflag) then
               flagval(nflags)=.false.
               isave(5)=1
            endif
            time1=miny+1
            time2=maxy+1
            if (time1.gt.ntime) then
               time1=ntime
            endif
            if (time2.gt.ntime) then
               time2=ntime
            endif
            call FmtCmd(flagstring,isave,t1(time1),t1(time2),chanoff,
     *       chanw,day0,selectline)
            call output(flagstring)
         elseif (undo) then
            if (nflags.gt.0) then
               nflags=-1*nflags
            else if (nflags.lt.0) then
               nflags=-1*nflags
            endif
         endif
      endif
c
      end
c************************************************************************
      subroutine reset_points(points)
c
      integer points(2,2)
c
c  Clear the user-selected points defining the selection region.
c
c  Input:
c    points  The bounding points of the selection region.
c  Output:
c    points  The bounding points of the selection region, reset to
c            default values so that no region is selected.
c-----------------------------------------------------------------------
      points(1,1)=-1
      points(1,2)=-1
      points(2,1)=-1
      points(2,2)=-1
c
      end
c************************************************************************
      subroutine MakeMeasurement(iflag,array,nchan,ntime,curs_x,curs_y,
     *  day0,t1,meas_channel,meas_frequency,meas_time,meas_amplitude,
     *  chanoff,chanw,cfreq,meastype)
c
      implicit none
      integer nchan,ntime,chanoff,chanw,meastype
      real curs_x,curs_y
      real t1(ntime),array(nchan,ntime,2),thist
      integer iflag(nchan,ntime,2)
      double precision day0,cfreq(nchan)
      integer meas_channel
      real meas_frequency,meas_amplitude
      character meas_time*(*),mon*3
      integer day,month,year,ierr,hour,minute,startday
      real second
c
c  Make a measurement of the sample under the cursor.
c
c  Input:
c    iflag           Array of flags for the currently displayed
c                    baseline.
c    array           The real values of the data (either amplitude
c                    or phase) for the currently displayed baseline.
c    nchan           The number of channels present in the data.
c    ntime           The number of time samples present in the data.
c    curs_x          The channel number that is under the cursor.
c    curs_y          The time sample that is under the cursor.
c    day0            The Julian date at midnight UT on the day of
c                    the first time sample.
c    t1              Array of times.
c    chanoff         The channel offset as specified by the line
c                    parameter.
c    chanw           The channel width as specified by the line
c                    parameter.
c    cfreq           Array that specifies each channel's centre
c                    frequency.
c    meastype        Whether to display the original value (set to
c                    0) or the currently displayed value (set to 1)
c  Output:
c    meas_channel    The sample's channel number.
c    meas_frequency  The frequency (in MHz) of the sample.
c    meas_time       The real UT date and time of the sample.
c    meas_amplitude  The amplitude (or phase) of the sample.
c-----------------------------------------------------------------------
      include 'maxdim.h'
c     
c     check we have a valid time
      if ((t1(int(curs_y)).ge.0.0).and.(t1(int(curs_y)).le.1.0)) then
         meas_channel=chanoff+int(curs_x)*chanw
         meas_frequency=cfreq(int(curs_x))*1000.0
         if (iflag(int(curs_x),int(curs_y),2).ge.1) then
            if (meastype.eq.0) then
               meas_amplitude=array(int(curs_x),int(curs_y),1)
            else
               meas_amplitude=array(int(curs_x),int(curs_y),2)
            endif
         else
            meas_amplitude=0.0
         endif
         startday=int(day0+real(int(t1(int(curs_y)))))+1
         call julian_to_date(startday,day,month,year,ierr)
         thist=t1(int(curs_y))*24.0
         hour=int(thist)
         thist=(thist-int(hour))*60.0
         minute=int(thist)
         thist=(thist-int(minute))*60.0
         second=thist
         call monthstring(month,mon)
         write(meas_time,'(I2.2,A3,I4.4,A1,I2.2,A1,I2.2,A1,F4.1)')
     *     day,mon,year,'-',hour,':',minute,':',second
         if (meas_time(17:17).eq.' ') meas_time(17:17)='0'
      endif
c
      end
c************************************************************************
      subroutine MakePlot(iflag,array,nchan,ntime,bl,curr_zooms,
     *  meas_channel,meas_frequency,meas_time,meas_amplitude,plot_top,
     *  plot_averages,plot_main,points,plot_points,yaxis,chanoff,chanw,
     *  subavgc,subavgt,minval,maxval,fiddle_min,fiddle_max,
     *  use_fiddle,colour_window,colour_region,fiddle_active)
c
      implicit none
      integer nchan,ntime,bl,chanoff,chanw
      integer iflag(nchan,ntime,2)
      real array(nchan,ntime,2)
      integer curr_zooms(2,2),points(2,2)
      integer meas_channel
      real meas_frequency,meas_amplitude,minval,maxval
      real fiddle_min,fiddle_max
      character meas_time*(*),yaxis*(*)
      logical plot_top,plot_averages,plot_main,plot_points
      logical subavgc,subavgt,use_fiddle,colour_window
      logical colour_region,fiddle_active
c
c  Top-level routine for generating the PGFLAG window.
c
c  Input:
c    iflag           Array of flags for the currently displayed
c                    baseline.
c    array           The real values of the data (either amplitude or
c                    phase) for the currently displayed baseline.
c    nchan           The number of channels present in the data.
c    ntime           The number of time samples present in the data.
c    bl              The ID of the baseline to display.
c    curr_zooms      The first and last channel and time sample
c                    to display in the main plot.
c    meas_channel    The channel number of the last sample that
c                    was measured.
c    meas_frequency  The frequency (in MHz) of the last sample
c                    that was measured.
c    meas_time       The real UT date and time of the last sample
c                    that was measured.
c    meas_amplitude  The amplitude (or phase) of the last sample
c                    that was measured.
c    plot_top        Switch to indicate whether to generate the top
c                    info box.
c    plot_averages   Switch to indicate whether to generate the
c                    average plots.
c    plot_main       Switch to indicate whether to generate the main
c                    plot.
c    points          The bounding points of the selection region.
c    plot_points     Switch to indicate whether to display the
c                    bounding points and the selection region.
c    yaxis           Indicates whether amplitude or phase is being
c                    displayed.
c    chanoff         The channel offset as specified by the line
c                    parameter.
c    chanw           The channel width as specified by the line
c                    parameter.
c    subavgc         Switch to indicate whether the average channel
c                    level should be subtracted from the data.
c    subavgt         Switch to indicate whether the average time
c                    level should be subtracted from the data.
c    fiddle_min      The user-specified minimum value for the colour
c                    amplitude scale.
c    fiddle_max      The user-specified maximum value for the colour
c                    amplitude scale.
c    use_fiddle      Switch to use the user-specified colour amplitude
c                    scale.
c    colour_window   Switch to use only the currently displayed range
c                    when determining the colour amplitude scale.
c    colour_region   Switch to use only the currently selected region
c                    when determining the colour amplitude scale.
c    fiddle_active   Switch to show when the user is being asked to
c                    fiddle the colour amplitude scale.
c  Output:
c    minval          The minimum unflagged value in the data.
c    maxval          The maximum unflagged value in the data.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer n_columns,n_toplines
      parameter(n_columns=6,n_toplines=6)
      integer LEFT_MARGIN_PIXELS,RIGHT_MARGIN_PIXELS
      parameter(LEFT_MARGIN_PIXELS=40,RIGHT_MARGIN_PIXELS=80)
      integer BOTTOM_MARGIN_PIXELS,TOP_MARGIN_PIXELS
      parameter(BOTTOM_MARGIN_PIXELS=70,TOP_MARGIN_PIXELS=90)
      integer X_BUFFER_PIXELS,Y_BUFFER_PIXELS
      parameter(X_BUFFER_PIXELS=50,Y_BUFFER_PIXELS=10)
      integer ARROWBOX_WIDTH_PIXELS
      parameter(ARROWBOX_WIDTH_PIXELS=40)
      real TOPBOX_WORLD_XRANGE,TOPBOX_WORLD_YRANGE
      parameter(TOPBOX_WORLD_XRANGE=100.0,TOPBOX_WORLD_YRANGE=100.0)
      character titles(n_columns,n_toplines)*256
      real colpos(n_columns,n_toplines,5)
      real vp_xmin,vp_xmax,vp_ymin,vp_ymax
      real xleft,xright,ybot,ytop,xbuffer,ybuffer
      real arrowbox_frac_x,arrowbox_frac_y
      real xchrhgt,ychrhgt,chrhgt,req_ychrhgt,chrhgt_fraction
      real ylinespacing,xcolumnbuffer
      character meas_baseline*256
      integer i,j
      logical acc,erasetop,setlims
      real curr_xpos,biggest_xworld,column_width,check_width,tmp
      real curr_ypos,tr(6)
      integer len1,stringlength,tbl
c
c     Setup the PGPLOT view area
c
      call pgqvsz(3,vp_xmin,vp_xmax,vp_ymin,vp_ymax)
      xleft=real(LEFT_MARGIN_PIXELS)/vp_xmax
      xright=1.0-real(RIGHT_MARGIN_PIXELS)/vp_xmax
      ybot=real(BOTTOM_MARGIN_PIXELS)/vp_ymax
      ytop=1.0-real(TOP_MARGIN_PIXELS+Y_BUFFER_PIXELS)/vp_ymax
      xbuffer=real(X_BUFFER_PIXELS)/vp_xmax
      ybuffer=real(Y_BUFFER_PIXELS)/vp_ymax
      arrowbox_frac_x=real(ARROWBOX_WIDTH_PIXELS)/vp_xmax
      arrowbox_frac_y=real(ARROWBOX_WIDTH_PIXELS)/vp_ymax
c
c     Print the top info
c
      erasetop=.false.
      call set_plot_top_info(ytop,ybuffer,TOPBOX_WORLD_XRANGE,
     *           TOPBOX_WORLD_YRANGE,erasetop)
c
c     Determine character height that will give six lines
c     in the top section
c
      call pgqcs(4,xchrhgt,ychrhgt)
      call pgqch(chrhgt)
      req_ychrhgt=real(TOPBOX_WORLD_YRANGE)/real(n_toplines+1)
      chrhgt_fraction=req_ychrhgt/ychrhgt
      chrhgt=chrhgt*chrhgt_fraction
      call pgsch(chrhgt)
      ylinespacing=real(TOPBOX_WORLD_YRANGE)/real(n_toplines)
      xcolumnbuffer=real(TOPBOX_WORLD_XRANGE)/50.0
c
c     Assign the titles
c
      do i=1,6
         do j=1,6
            titles(i,j)=' '
         enddo
      enddo
      titles(1,1)='baseline:'
      titles(1,2)='chan:'
      titles(1,3)='frequency (MHz):'
      titles(1,4)='time:'
      if (yaxis(1:1).eq.'a') then
         titles(1,5)='amplitude:'
      elseif (yaxis(1:1).eq.'p') then
         titles(1,5)='phase:'
      endif
c     figure out what baseline it is
      do i=1,MAXANT
         do j=i,MAXANT
            tbl=((j-1)*j)/2+i
            if (tbl.eq.bl) then
               WRITE(meas_baseline,'(I2.2,A,I2.2)') i,'-',j
               exit
            endif
         enddo
      enddo
      titles(2,1)=meas_baseline
      WRITE(titles(2,2),'(I4.4)') meas_channel
      WRITE(titles(2,3),'(F12.4)') meas_frequency
      titles(2,4)=meas_time
      WRITE(titles(2,5),'(F8.3)') meas_amplitude
      titles(3,1)='[m]'
      titles(4,1)='measure'
      titles(3,2)='[z/Z]'
      titles(4,2)='zoom/unzoom'
      titles(3,3)='[q/a]'
      titles(4,3)='quit/abort'
      titles(3,4)='[u]'
      titles(4,4)='undo/redo last flagging'
      titles(3,5)='[f/F]'
      titles(4,5)='flag this/all baselines'
      titles(5,1)='[g/G]'
      titles(6,1)='unflag this/all baselines'
      titles(5,2)='[h/j/k/l]'
      titles(6,2)='move window l/d/u/r'
      titles(5,3)='[n/p]'
      titles(6,3)='next/previous baseline'
      titles(5,4)='[s/S]'
      titles(6,4)='show all channels/times'
      titles(5,5)='[c/t]'
      titles(6,5)='extend all channels/time'
c
c     Now check the column widths fit
c
      acc=.false.
      do while (acc .eqv. .false.)
         acc=.true.
         curr_xpos=xcolumnbuffer
         biggest_xworld=0.0
         do i=1,n_columns
c     get the column width
            column_width=0.0
            do j=1,n_toplines
               stringlength=len1(titles(i,j))
               call pglen(4,titles(i,j)(1:stringlength),check_width,tmp)
               column_width=max(column_width,check_width)
            enddo
            do j=1,n_toplines
               curr_ypos=real(TOPBOX_WORLD_YRANGE)-
     *           real(j)*ylinespacing
               colpos(i,j,1)=curr_xpos
               colpos(i,j,2)=curr_xpos+column_width
               biggest_xworld=max(colpos(i,j,2),biggest_xworld)
               if (biggest_xworld .gt. (real(TOPBOX_WORLD_XRANGE)
     *           -xcolumnbuffer)) acc = .false.
               colpos(i,j,3)=curr_ypos
               if (i .eq. 1) then
c     string is right-justified
                  colpos(i,j,4)=1.0
                  colpos(i,j,5)=colpos(i,j,2)
               elseif ((i .eq. 3).or.(i .eq. 5)) then
c     string is centre-justified
                  colpos(i,j,4)=0.5
                  colpos(i,j,5)=((colpos(i,j,2)-colpos(i,j,1))/2.0+
     *              colpos(i,j,1))
               else
c     string is left-justified
                  colpos(i,j,4)=0.0
                  colpos(i,j,5)=colpos(i,j,1)
               endif
            enddo
            curr_xpos=curr_xpos+column_width+xcolumnbuffer
         enddo
         if (acc .eqv. .false.) then
c     shrink the text a bit
            chrhgt_fraction=(real(TOPBOX_WORLD_XRANGE)-xcolumnbuffer)/
     *        biggest_xworld
            chrhgt=chrhgt*chrhgt_fraction
            call pgsch(chrhgt)
         endif
      enddo
c
c     Find the max and min values in the array
c
      setlims=.true.
      do i=1,nchan
         do j=1,ntime
            if (iflag(i,j,2) .ge. 1) then
               if (setlims) then
                  minval=array(i,j,1)
                  maxval=array(i,j,1)
                  setlims=.false.
               else
                  minval=min(array(i,j,1),minval)
                  maxval=max(array(i,j,1),maxval)
               endif
            endif
         enddo
      enddo
c
c     Now make a waterfall plot
c
      tr(1)=0.0
      tr(2)=1.0
      tr(3)=0.0
      tr(4)=0.0
      tr(5)=0.0
      tr(6)=1.0
      call paint_plot(xleft,xright,ybot,ytop,ybuffer,xbuffer,nchan,
     *  ntime,minval,maxval,arrowbox_frac_x,arrowbox_frac_y,colpos,
     *  tr,titles,curr_zooms,array,n_columns,n_toplines,
     *  TOPBOX_WORLD_XRANGE,TOPBOX_WORLD_YRANGE,plot_top,plot_averages,
     *  plot_main,plot_points,points,iflag,chanoff,chanw,subavgc,
     *  subavgt,fiddle_min,fiddle_max,use_fiddle,colour_window,
     *  colour_region,xcolumnbuffer,ylinespacing,fiddle_active)
c
      end
c************************************************************************
      subroutine paint_plot(xleft,xright,ybot,ytop,ybuffer,xbuffer,
     *  xdim,ydim,minval,maxval,arrowbox_frac_x,arrowbox_frac_y,
     *  colpos,tr,titles,curr_zooms,valarray,n_columns,n_toplines,
     *  TOPBOX_WORLD_XRANGE,TOPBOX_WORLD_YRANGE,plot_top,plot_averages,
     *  plot_main,plot_points,points,iflag,chanoff,chanw,subavgc,
     *  subavgt,fiddle_min,fiddle_max,use_fiddle,colour_window,
     *  colour_region,xcolumnbuffer,ylinespacing,fiddle_active)
c
      implicit none
      real xleft,xright,ybot,ytop,ybuffer,xbuffer
      integer xdim,ydim,n_columns,n_toplines,chanoff,chanw
      integer iflag(xdim,ydim,2)
      real minval,maxval,arrowbox_frac_x,arrowbox_frac_y
      real colpos(n_columns,n_toplines,5),tr(6)
      character titles(n_columns,n_toplines)*256
      integer curr_zooms(2,2)
      real TOPBOX_WORLD_XRANGE,TOPBOX_WORLD_YRANGE
      real valarray(xdim,ydim,2),fiddle_min,fiddle_max
      real xcolumnbuffer,ylinespacing
      logical plot_top,plot_averages,plot_main,plot_points
      logical subavgc,subavgt,use_fiddle,colour_window
      logical colour_region,fiddle_active
      integer points(2,2)
c
c  Routine that does all the grunt work in making the PGFLAG
c  window.
c
c  Input:
c    xleft                The leftmost device coordinate of the plot.
c    xright               The rightmost device coordinate of the
c                         plot.
c    ybot                 The bottommost device coordinate of the
c                         plot.
c    ytop                 The topmost device coordinate of the plot.
c    ybuffer              The fraction of the plot's y dimension
c                         to use as a buffer between objects.
c    xbuffer              The fraction of the plot's x dimension
c                         to use as a buffer between objects.
c    xdim                 The number of channels in valarray.
c    ydim                 The number of time samples in valarray.
c    minval               The minimum unflagged value present in
c                         valarray.
c    maxval               The maximum unflagged value present in
c                         valarray.
c    arrowbox_frac_x      The fraction of the plot's x dimension
c                         taken up by the arrow box on the side.
c    arrowbox_frac_y      The fraction of the plot's y dimension
c                         taken up by the arrow box on the side.
c    colpos               The world-coordinate positions of the info
c                         at the top of the plot, along with info
c                         on how to align the text.
c    tr                   The transform array describing how to
c                         map valarray to the plot.
c    titles               A string array containing the text to put
c                         in the key info box at the top of the plot.
c    curr_zooms           The first and last channel and time sample
c                         to display in the main plot.
c    valarray             The real values of the data (either amp
c                         or phase) for the currently displayed
c                         baseline.
c    n_columns            The number of columns of info in the key
c                         info box at the top of the plot.
c    n_toplines           The number of lines of info in the key info
c                         box at the top of the plot.
c    TOPBOX_WORLD_XRANGE  The world coordinate range assigned to the
c                         x-axis of the boxes that contain the key
c                         information at the top, and the arrows on
c                         the sides.
c    TOPBOX_WORLD_YRANGE  The world coordinate range assigned to the
c                         y-axis of the boxes that contain the key
c                         information at the top, and the arrows on
c                         the sides.
c    plot_top             Switch to make this routine plot the info
c                         box at the top of the plot.
c    plot_averages        Switch to make this routine calculate and
c                         plot the averages at the bottom and right of
c                         the plot.
c    plot_main            Switch to make this routine plot the main
c                         waterfall plot.
c    plot_points          Switch to make this routine plot the user
c                         selected points and the selection rectangle.
c    points               The bounding points of the selection region.
c    iflag                Array of flags for the currently displayed
c                         baseline.
c    chanoff              The channel offset as specified by the line
c                         parameter.
c    chanw                The channel width as specified by the line
c                         parameter.
c    subavgc              Switch to indicate whether the average channel
c                         level should be subtracted from the data.
c    subavgt              Switch to indicate whether the average time
c                         level should be subtracted from the data.
c    fiddle_min      The user-specified minimum value for the colour
c                    amplitude scale.
c    fiddle_max      The user-specified maximum value for the colour
c                    amplitude scale.
c    use_fiddle      Switch to use the user-specified colour amplitude
c                    scale.
c    colour_window   Switch to use only the currently displayed range
c                    when determining the colour amplitude scale.
c    colour_region   Switch to use only the currently selected region
c                    when determining the colour amplitude scale.
c    xcolumnbuffer   Spacing between columns in the top region.
c    ylinespacing    Spacing between rows in the top region.
c    fiddle_active   Switch to show when the user is being asked to
c                    fiddle the colour amplitude scale.
c  Output:
c    none
c-----------------------------------------------------------------------
      integer i,j,average_box_width,normal_linewidth
      integer point_linewidth,measbox_colour,tcolour,twidth
      parameter(average_box_width=5)
      real xaverage(xdim,average_box_width)
      real yaverage(average_box_width,ydim)
      real dxaverage(xdim,average_box_width)
      real dyaverage(average_box_width,ydim)
      real VERTICAL_ARROW_LENGTH,HORIZONTAL_ARROW_LENGTH
      logical erasetop
c
      VERTICAL_ARROW_LENGTH=TOPBOX_WORLD_YRANGE/10.0
      HORIZONTAL_ARROW_LENGTH=TOPBOX_WORLD_XRANGE/10.0
c
      measbox_colour=2
c
      if ((plot_top).and.(plot_averages).and.(plot_main)) then
         call pgeras()
      endif
      if (plot_top) then
         if ((plot_averages.eqv..false.).or.(plot_main.eqv..false.))
     *      then
            erasetop=.true.
         else
            erasetop=.false.
         endif
         call set_plot_top_info(ytop,ybuffer,TOPBOX_WORLD_XRANGE,
     *     TOPBOX_WORLD_YRANGE,erasetop)
         call pgqci(tcolour)
         call pgsci(measbox_colour)
         call pgsfs(1)
         call pgrect(colpos(1,1,1)-xcolumnbuffer/2.,
     *               colpos(2,1,2)+xcolumnbuffer/2.,
     *               ylinespacing/4.0,
     *               TOPBOX_WORLD_YRANGE-ylinespacing/4.)
         call pgsci(tcolour)
         do i=1,n_columns
            do j=1,n_toplines
               if (i.le.2) then
                  call pgqlw(twidth)
                  call pgslw(min(twidth*5,201))
               endif
               call pgptext(colpos(i,j,5),colpos(i,j,3),0.0,
     *           colpos(i,j,4),titles(i,j))
               if (i.le.2) then
                  call pgslw(twidth)
               endif
            enddo
         enddo
      endif
c
c     Draw any required arrows
c
      call pgsvp(xleft-arrowbox_frac_x,xleft,ybot,ytop)
      call pgswin(0.0,TOPBOX_WORLD_XRANGE,0.0,TOPBOX_WORLD_YRANGE)
      call pgsci(3)
c     can we move down?
      if (curr_zooms(2,1).gt.1) then
         call pgarro(TOPBOX_WORLD_XRANGE/2.0,VERTICAL_ARROW_LENGTH,
     *     TOPBOX_WORLD_XRANGE/2,0.0)
      endif
c     can we move up?
      if (curr_zooms(2,2).lt.ydim) then
         call pgarro(TOPBOX_WORLD_XRANGE/2.0,
     *     TOPBOX_WORLD_YRANGE-VERTICAL_ARROW_LENGTH,
     *     TOPBOX_WORLD_XRANGE/2.0,TOPBOX_WORLD_YRANGE)
      endif
      call pgsvp(xleft,xright,ybot-arrowbox_frac_y,ybot)
      call pgswin(0.0,TOPBOX_WORLD_XRANGE,0.0,TOPBOX_WORLD_YRANGE)
c     can we move left?
      if (curr_zooms(1,1).gt.1) then
         call pgarro(HORIZONTAL_ARROW_LENGTH,TOPBOX_WORLD_YRANGE/2.0,
     *     0.0,TOPBOX_WORLD_YRANGE/2.0)
      endif
c     can we move right?
      if (curr_zooms(1,2).lt.xdim) then
         call pgarro(TOPBOX_WORLD_XRANGE-HORIZONTAL_ARROW_LENGTH,
     *    TOPBOX_WORLD_YRANGE/2.0,TOPBOX_WORLD_XRANGE,
     *    TOPBOX_WORLD_YRANGE/2.0)
      endif
      call compute_average_spectrum(valarray,xdim,ydim,xaverage,
     *     yaverage,average_box_width,iflag,dxaverage,dyaverage)
      call mask_and_range(iflag,valarray,xdim,ydim,subavgc,subavgt,
     *     minval,maxval,fiddle_min,fiddle_max,use_fiddle,
     *     curr_zooms,colour_window,xaverage,yaverage,
     *     average_box_width,points,colour_region)
      call compute_average_spectrum(valarray,xdim,ydim,xaverage,
     *     yaverage,average_box_width,iflag,dxaverage,dyaverage)
      if (plot_averages) then
c     
c     Draw the average spectra - on the bottom first
c     
         call pgsci(1)
         call pgsvp(xleft,xright,0.0,ybot-arrowbox_frac_y)
         call pgswin(real(curr_zooms(1,1)),real(curr_zooms(1,2)),
     *     1.0,real(average_box_width))
         if (subavgc) then
            call pgsci(2)
         endif
         call pgbox('BC',0.0,0,'BC',0.0,0)
         if (subavgc) then
            call pgsci(1)
         endif
         if ((subavgc).or.(subavgt)) then
            call pggray(dxaverage,xdim,average_box_width,
     *             curr_zooms(1,1),curr_zooms(1,2),1,average_box_width,
     *             maxval,minval,tr)
         else
            call pggray(xaverage,xdim,average_box_width,curr_zooms(1,1),
     *             curr_zooms(1,2),1,average_box_width,maxval,minval,tr)
         endif
c     and then on the right
         call pgsvp(xright+xbuffer,1.0,ybot,ytop)
         call pgswin(1.0,real(average_box_width),
     *        real(curr_zooms(2,1)),real(curr_zooms(2,2)))
         if (subavgt) then
            call pgsci(2)
         endif
         call pgbox('BC',0.0,0,'BC',0.0,0)
         if (subavgt) then
            call pgsci(1)
         endif
         if ((subavgc).or.(subavgt)) then
            call pggray(dyaverage,average_box_width,ydim,1,
     *             average_box_width,curr_zooms(2,1),curr_zooms(2,2),
     *             maxval,minval,tr)
         else
            call pggray(yaverage,average_box_width,ydim,1,
     *             average_box_width,curr_zooms(2,1),curr_zooms(2,2),
     *             maxval,minval,tr)
         endif
      endif
      call set_plot_main(xleft,xright,ybot,ytop,curr_zooms)
      if (plot_main) then
c
c     Now the main plot
c     
         call pgsci(1)
         call pgswin(real(chanoff+curr_zooms(1,1)*chanw),
     *     real(chanoff+curr_zooms(1,2)*chanw),
     *     real(curr_zooms(2,1)),real(curr_zooms(2,2)))
         call pgbox('BCN',0.0,0,'BC',0.0,0)
         call set_plot_main(xleft,xright,ybot,ytop,curr_zooms)
         call draw_waterfall(valarray,xdim,ydim,curr_zooms,maxval,
     *     minval,tr)
         if (fiddle_active) then
            call pgsci(5)
         elseif (use_fiddle) then
            call pgsci(6)
         endif
         call pgwedg('R',1.0,3.0,maxval,minval,' ')
         if ((fiddle_active).or.(use_fiddle)) then
            call pgsci(1)
         endif
      endif
      if (plot_points) then
         if (plot_main.eqv..false.) then
            call draw_waterfall(valarray,xdim,ydim,curr_zooms,maxval,
     *        minval,tr)
         endif
c     draw user-defined points
         do i=1,2
            call pgsci(3)
            call pgqlw(normal_linewidth)
            point_linewidth=5*normal_linewidth
            call pgslw(point_linewidth)
            if ((points(i,1).ne.-1).and.(points(i,2).ne.-1)) then
               call pgpt1(real(points(i,1)),real(points(1,2)),-1)
            endif
            call pgsci(1)
            call pgslw(normal_linewidth)
         enddo
c     connect with a box if we have both points
         if ((points(1,1).ne.-1).and.(points(1,2).ne.-1).and.
     *       (points(2,1).ne.-1).and.(points(2,2).ne.-1)) then
            call pgsci(3)
            call pgsfs(2)
            call pgslw(point_linewidth)
            call pgrect(real(points(1,1)),real(points(2,1)),
     *        real(points(1,2)),real(points(2,2)))
            call pgsci(1)
            call pgslw(normal_linewidth)
         endif
      endif
c
      end
c************************************************************************
      subroutine draw_waterfall(valarray,xdim,ydim,curr_zooms,maxval,
     *  minval,tr)
c
      implicit none
      integer xdim,ydim,curr_zooms(2,2)
      real maxval,minval,tr(6)
      real valarray(xdim,ydim,2)
c
c  Draw the main waterfall plot, after masking the data with the
c  current user-specified flags.
c
c  Input:
c    valarray    The real values of the data (either amplitude or
c                phase) for the currently displayed baseline.
c    xdim        The number of channels in valarray.
c    ydim        The number of time steps in valarray.
c    curr_zooms  The first and last channel and time sample to
c                display in the main plot.
c    maxval      The maximum unflagged value present in valarray.
c    minval      The minimum unflagged value present in valarray.
c    tr          The transform array describing how to map valarray
c                to the plot.
c  Output:
c    none
c-----------------------------------------------------------------------
      call pggray(valarray(1,1,2),xdim,ydim,curr_zooms(1,1),
     *     curr_zooms(1,2),curr_zooms(2,1),curr_zooms(2,2),maxval,
     *     minval,tr)
c
      end
c************************************************************************
      subroutine mask_and_range(iflag,valarray,xdim,ydim,subavgc,
     *     subavgt,minval,maxval,fiddle_min,fiddle_max,use_fiddle,
     *     curr_zooms,colour_window,xaverage,yaverage,abw,points,
     *     colour_region)
c
      implicit none
      integer xdim,ydim,curr_zooms(2,2),abw
      real minval,maxval,fiddle_min,fiddle_max
      logical subavgc,subavgt,use_fiddle,colour_window,colour_region
      real valarray(xdim,ydim,2)
      real xaverage(xdim,abw),yaverage(abw,ydim)
      integer iflag(xdim,ydim,2),points(2,2)
c
c  Create the second set of the valarray, by masking values that are
c  flagged. From this and the user-selected options, determine the
c  colour-amplitude scaling range.
c
c  Input:
c    iflag          Array of flags for the currently displayed
c                   baseline.
c    valarray       The real values of the data (either amp or phase)
c                   for the currently displayed baseline.
c    xdim           The number of channels in valarray.
c    ydim           The number of time samples in valarray.
c    subavgc        Switch to indicate whether the average channel
c                   level should be subtracted from the data.
c    subavgt        Switch to indicate whether the average time level
c                   should be subtracted from the data.
c    fiddle_min     The user-specified minimum value for the colour
c                   amplitude scale.
c    fiddle_max     The user-specified maximum value for the colour
c                   amplitude scale.
c    use_fiddle     Switch to use the user-specified colour amplitude
c                   scale.
c    curr_zooms     The first and last channel and time sample to
c                   display in the main plot.
c    colour_window  Switch to use only the currently displayed range
c                   when determining the colour amplitude scale.
c    xaverage       The average over time of each channel in valarray.
c    yaverage       The average over channel of each time in valarray.
c    abw            The number of copies of the average channel data in
c                   xaverage.
c    points         The bounding points of the current selection region.
c    colour_region  Switch to use only the currently selection region
c                   when determining the colour amplitude scale.
c  Output:
c    minval         The minimum unflagged value present in the
c                   appropriate region of valarray.
c    maxval         The maximum unflagegd value present in the
c                   appropriate region of valarray.
c-----------------------------------------------------------------------
      integer i,j,minx,maxx,miny,maxy
      logical firstset
      real range
c
      minval=0.0
      maxval=0.0
c     first make a masked value array
      do i=1,xdim
         do j=1,ydim
            if (iflag(i,j,2).ge.1) then
               valarray(i,j,2)=valarray(i,j,1)
               if (subavgc) then
                  valarray(i,j,2)=valarray(i,j,2)-xaverage(i,1)
               elseif (subavgt) then
                  valarray(i,j,2)=valarray(i,j,2)-yaverage(1,j)
               endif
            else
               valarray(i,j,2)=0.0
            endif
         enddo
      enddo
c     now look for the appropriate min/max values
c     check that if we are supposed to be using a region, that we
c     have a properly specified region
      if (colour_region) then
         if ((points(1,1).eq.-1).or.(points(1,2).eq.-1).or.
     *       (points(2,1).eq.-1).or.(points(2,2).eq.-1)) then
            colour_region=.false.
         else
            minx=min(points(1,1),points(2,1))
            maxx=max(points(1,1),points(2,1))
            miny=min(points(1,2),points(2,2))
            maxy=max(points(1,2),points(2,2))
         endif
      endif
      if (use_fiddle) then
c     easy - just use the user-specified values
         minval=fiddle_min
         maxval=fiddle_max
      else
         firstset=.true.
         do i=1,xdim
            do j=1,ydim
               if ((colour_region).and.
     *              ((i.ge.minx).and.(i.le.maxx)).and.
     *              ((j.ge.miny).and.(j.le.maxy))) then
                  if (firstset) then
                     minval=valarray(i,j,2)
                     maxval=valarray(i,j,2)
                     firstset=.false.
                  else
                     minval=min(minval,valarray(i,j,2))
                     maxval=max(maxval,valarray(i,j,2))
                  endif
               elseif ((colour_window).and.
     *              ((i.ge.curr_zooms(1,1)).and.
     *               (i.le.curr_zooms(1,2))).and.
     *              ((j.ge.curr_zooms(2,1)).and.
     *               (j.le.curr_zooms(2,2)))) then
                  if (firstset) then
                     minval=valarray(i,j,2)
                     maxval=valarray(i,j,2)
                     firstset=.false.
                  else
                     minval=min(minval,valarray(i,j,2))
                     maxval=max(maxval,valarray(i,j,2))
                  endif
               elseif ((.not.colour_window).and.
     *                 (.not.colour_region)) then
                  if (firstset) then
                     minval=valarray(i,j,2)
                     maxval=valarray(i,j,2)
                     firstset=.false.
                  else
                     minval=min(minval,valarray(i,j,2))
                     maxval=max(maxval,valarray(i,j,2))
                  endif
               endif
            enddo
         enddo
      endif
c     check that our min/max values are reasonable
      if (minval.eq.maxval) then
         if (minval.eq.0.0) then
            minval=0.0
            maxval=1.0
         else
            range=minval/10.0
            minval=minval-range
            maxval=maxval+range
         endif
      endif
c
      end
c************************************************************************
      subroutine set_plot_main(xleft,xright,ybot,ytop,curr_zooms)
c
      implicit none
      real xleft,xright,ybot,ytop
      integer curr_zooms(2,2)
c
      call pgsvp(xleft,xright,ybot,ytop)
      call pgswin(real(curr_zooms(1,1)),real(curr_zooms(1,2)),
     *  real(curr_zooms(2,1)),real(curr_zooms(2,2)))
c
      end
c************************************************************************
      subroutine compute_average_spectrum(valarray,xdim,ydim,
     *  xaverage,yaverage,average_box_width,iflag,dxaverage,
     *  dyaverage)
c
      implicit none
      integer xdim,ydim,average_box_width
      real xaverage(xdim,average_box_width)
      real yaverage(average_box_width,ydim)
      real dxaverage(xdim,average_box_width)
      real dyaverage(average_box_width,ydim)
      real valarray(xdim,ydim,2)
      integer iflag(xdim,ydim,2)
c
c  Routine to compute the average channel and time values for both
c  the original data and the data to be displayed.
c
c  Input:
c    valarray           The real values of the data (either amp or
c                       phase) for the currently displayed baseline.
c    xdim               The number of channels in valarray.
c    ydim               The number of time samples in valarray.
c    average_box_width  The number of times to copy the average value
c                       for display purposes.
c    iflag              Array of flags for the currently displayed
c                       baseline.
c  Output:
c    xaverage           The average channel values of the original
c                       values in a format suitable for displaying as 
c                       a waterfall plot.
c    yaverage           The average time values of the original values
c                       in a format suitable for displaying as a
c                       waterfall plot.
c    dxaverage          The average channel values of the values to be
c                       displayed in a format suitable for displaying
c                       as a waterfall plot.
c    dyaverage          The average time values of the values to be
c                       displayed in a format suitable for displaying
c                       as a waterfall plot.
c-----------------------------------------------------------------------
      integer i,j,n
c
c     Initialise the arrays
c
      do i=1,xdim
         do j=1,average_box_width
            xaverage(i,j)=0.0
            dxaverage(i,j)=0.0
         enddo
      enddo
      do i=1,average_box_width
         do j=1,ydim
            yaverage(i,j)=0.0
            dyaverage(i,j)=0.0
         enddo
      enddo
c
c     Compute the x average
c
      do i=1,xdim
         n=0
         do j=1,ydim
            if (iflag(i,j,2).ge.1) then
               xaverage(i,1)=xaverage(i,1)+valarray(i,j,1)
               dxaverage(i,1)=dxaverage(i,1)+valarray(i,j,2)
               n=n+1
            endif
         enddo
         xaverage(i,1)=xaverage(i,1)/n
         dxaverage(i,1)=dxaverage(i,1)/n
c     copy this value a few times
         do j=2,average_box_width
            xaverage(i,j)=xaverage(i,1)
            dxaverage(i,j)=dxaverage(i,1)
         enddo
      enddo
c
c     Compute the y average
c
      do i=1,ydim
         n=0
         do j=1,xdim
            if (iflag(j,i,2).ge.1) then
               yaverage(1,i)=yaverage(1,i)+valarray(j,i,1)
               dyaverage(1,i)=dyaverage(1,i)+valarray(j,i,2)
               n=n+1
            endif
         enddo
         yaverage(1,i)=yaverage(1,i)/n
         dyaverage(1,i)=dyaverage(1,i)/n
c     copy this value a few times
         do j=2,average_box_width
            yaverage(j,i)=yaverage(1,i)
            dyaverage(j,i)=dyaverage(1,i)
         enddo
      enddo
c
      end
c************************************************************************
      subroutine set_plot_top_info(ytop,ybuffer,xrange,yrange,erase)
c
      implicit none
      real ytop,ybuffer
      real xrange,yrange
      logical erase
c
      integer cfs
c
      call pgsvp(0.0,1.0,ytop+ybuffer,1.0)
      call pgswin(0.,xrange,0.,yrange)
      if (erase) then
         call pgsci(0)
         call pgqfs(cfs)
         call pgsfs(1)
         call pgrect(0.,xrange,0.,yrange)
         call pgsci(1)
         call pgsfs(cfs)
      endif
      call pgsci(1)
      call pgbox('BC',0.0,0,'BC',0.0,0)
c
      end
c************************************************************************
      subroutine Gridit(iflag,array,nchan,ntime,rqbl,day0,
     *                  lScr,nvis,t1,firstread,some_unflagged)
c
      implicit none
      integer nchan,ntime,lScr,nvis,rqbl
      logical firstread,some_unflagged
      integer iflag(nchan,ntime,2)
      real t1(ntime),array(nchan,ntime,2)
      double precision day0
      include 'maxdim.h'
      integer i,j,k,offset,length,pnt,bl,i0
      real buf(2*MAXCHAN+3),t

      some_unflagged=.false.
      if (firstread) then
         do j=1,ntime
            do i=1,nchan
               iflag(i,j,1)=0
               array(i,j,1)=0
               array(i,j,2)=0
            enddo
         enddo
      endif
c
c     Start reading the data.
c
      offset=0
      length=2*nchan+3
      do k=1,nvis
         call scrread(lScr,buf,offset,length)
         offset=offset+length
         bl=nint(buf(1))
         if (bl.eq.rqbl) then
            t=buf(2)+(dble(buf(3))-day0)
            do pnt=1,ntime
               if (t1(pnt).gt.-1.0) then
                  if (t1(pnt).le.t .and. t.le.t1(pnt+1)) goto 10
               endif
            enddo
            call bug ('f','Time slot miscalculation')

 10         i0=3
            do j=1,nchan
               if (nint(buf(i0+2)).gt.0) then
                  some_unflagged=.true.
                  array(j,pnt,1)=buf(i0+1)
                  iflag(j,pnt,1)=1
                  iflag(j,pnt,2)=1
               else
                  array(j,pnt,1)=buf(i0+1)
                  iflag(j,pnt,1)=0
                  iflag(j,pnt,2)=0
               endif
               i0=i0+2
            enddo
         endif
      enddo
c
      end
c************************************************************************
	subroutine GetAxis(xaxis,yaxis)
c
	implicit none
	character xaxis*(*),yaxis*(*)
c------------------------------------------------------------------------
	integer NAX
	parameter(NAX=2)
	integer n
	character axes(3)*12
	data axes/'amplitude   ','phase       ',
     *		  'time        '/
c	call keymatch('axis',NAX,axes,1,xaxis,n)
	xaxis = axes(3)
	call keymatch('mode',NAX,axes,1,yaxis,n)
	if(n.eq.0)yaxis = axes(1)
	end
c************************************************************************
	subroutine GetOpt(selgen,noapply,nosrc,uvflags)
c
	implicit none
	logical selgen,noapply,nosrc
	character uvflags*(*)
c
c  Get extra processing options.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=6)
	logical present(NOPTS)
	character opts(NOPTS)*8
	data opts/'nocal   ','nopass  ','nopol   ',
     *		  'selgen  ','noapply ','nosrc   '/
c
	call options('options',opts,present,NOPTS)
c
	selgen = present(4)
	noapply= present(5)
        nosrc  = present(6)
	uvflags = 'sdlwb'
	if(.not.present(1))uvflags(6:6) = 'c'
	if(.not.present(2))uvflags(7:7) = 'f'
	if(.not.present(3))uvflags(8:8) = 'e'
	end
c************************************************************************
      subroutine GetDat(tno,xaxis,yaxis,present,maxbase1,xdat,ydat,
     *                  bldat,timedat,ndat,MAXDAT,ant1,ant2)
c
      implicit none
      integer tno,maxbase1,MAXDAT,ndat,ant1,ant2
      integer bldat(MAXDAT)
      logical present(maxbase1)
      double precision timedat(MAXDAT)
      real xdat(MAXDAT),ydat(MAXDAT)
      character xaxis*(*),yaxis*(*)
c
      include 'maxdim.h'
      double precision TTOL
      parameter(TTOL=1d0/86400d0)
c     
      logical flags(MAXCHAN),ok
      complex data(MAXCHAN)
      complex corr(MAXBASE)
      double precision preamble(4),time,time0,tprev,lst,ra
      real uvdist2(MAXBASE),var(MAXBASE),temp
      integer i,n,bl,i1,i2,nants,npnt(MAXBASE),mbase,nchan
c     
c     Miscellaneous initialisation.
c     
      mbase = min(MAXBASE,maxbase1)
      do i=1,MAXBASE
         present(i) = .false.
      enddo
c     
      do i=1,mbase
         npnt(i)    = 0
         uvdist2(i) = 0
         corr(i)    = 0
         var(i) = 0
      enddo
      ndat = 0
c     
c     Lets get going.
c     
      call output('Reading the data ...')
      call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
      if(nchan.eq.0)call bug('f','No visibility data found')
      call flagchk(tno)
      nants = 0
      tprev = preamble(3)
      time0 = int(tprev - 0.5d0) + 0.5d0
      call uvrdvrd(tno,'lst',lst,0.d0)
      call uvrdvrd(tno,'ra',ra,0.d0)
      do while(nchan.gt.0)
         call BasAnt(preamble(4),i1,i2)
         bl = (i2*(i2-1))/2 + i1
         if ((ant1.eq.i1).and.(ant2.eq.i2)) then
            ok=.true.
         else
            ok=.false.
         endif
c     ok = bl.lt.mbase
         if(ok)then
            time = preamble(3)
            if(abs(time-tprev).gt.TTOL)then
c               call output(xaxis)
c               call output(yaxis)
               if(nants.gt.0) call IntFlush(nants,tprev,
     *              uvdist2,var,corr,xaxis,yaxis,npnt,
     *              time0,present,mbase,xdat,ydat,timedat,bldat,ndat,
     *              MAXDAT)
               nants = 0
               tprev = time
               call uvrdvrd(tno,'lst',lst,0.d0)
               call uvrdvrd(tno,'ra',ra,0.d0)
            endif
            n = 0
            do i=1,nchan
               if(flags(i))then
                  n = n + 1
                  npnt(bl) = npnt(bl) + 1
                  corr(bl) = corr(bl) + data(i)
               endif
            enddo
            if(n.gt.0)then
               call uvDatGtr('variance',temp)
               var(bl) = var(bl) + n*temp
               uvdist2(bl) = uvdist2(bl) +
     *              n * (preamble(1)*preamble(1)+preamble(2)*
     *              preamble(2))
               nants = max(nants,i1,i2)
            endif
         endif
         call uvDatRd(preamble,data,flags,MAXCHAN,nchan)	  
      enddo
c     
      if(nants.gt.0)
     *     call IntFlush(nants,time,uvdist2,var,
     *     corr,xaxis,yaxis,npnt,
     *     time0,present,mbase,xdat,ydat,timedat,bldat,ndat,MAXDAT)
c     
      end
c************************************************************************
	subroutine IntFlush(nants,time,uvdist2,var,
     *	  corr,xaxis,yaxis,npnt,
     *	  time0,present,MAXBASE,xdat,ydat,timedat,bldat,ndat,MAXDAT)
c
	implicit none
	integer MAXBASE,MAXDAT,nants,npnt(MAXBASE),bldat(MAXDAT),ndat
	double precision time,time0,timedat(MAXDAT)
	real uvdist2(MAXBASE),var(MAXBASE),xdat(MAXDAT),ydat(MAXDAT)
	complex corr(MAXBASE)
	logical present(MAXBASE)
	character xaxis*(*),yaxis*(*)
c
c------------------------------------------------------------------------
	integer i,j,k
c
c  Externals.
c
	real GetVal
c
c        call output(xaxis)
c        call output(yaxis)
	k = 0
	do j=1,nants
	  do i=1,j
	    k = k + 1
	    if(npnt(k).gt.0)then
	      ndat = ndat + 1
	      if(ndat.gt.MAXDAT)call bug('f','Too many points')
c              call output(xaxis)
	      xdat(ndat) = GetVal(xaxis,corr(k),
     *		npnt(k),time,time0)
c              call output(yaxis)
	      ydat(ndat) = GetVal(yaxis,corr(k),
     *		npnt(k),time,time0)
	      bldat(ndat) = k
	      timedat(ndat) = time
	      present(k) = .true.
	      npnt(k) = 0
	      uvdist2(k) = 0
	      var(k) = 0
	      corr(k) = 0
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	real function GetVal(axis,corr,npnt,
     *		time,time0)
c
	implicit none
	character axis*(*)
	complex corr
	double precision time,time0
	integer npnt
c------------------------------------------------------------------------
	include 'mirconst.h'
	complex data
c
        data = corr/npnt
c
c        call output(axis)
	if(axis.eq.'amplitude')then
	  GetVal = abs(data)
	else if(axis.eq.'phase')then
	  GetVal = 180/pi * atan2(aimag(data),real(data))
	else if(axis.eq.'time')then
	  GetVal = 86400*(time - time0)
	else
	  call bug('f','I should never get here')
	endif
	end
c************************************************************************
	subroutine flagchk(tno)
c
	implicit none
	integer tno
c
c  Check that the user's linetype is not going to cause the flagging
c  routine to vomit when the flagging is applied.
c
c------------------------------------------------------------------------
	integer CHANNEL,WIDE
	parameter(CHANNEL=1,WIDE=2)
	double precision line(6)
c
	call uvinfo(tno,'line',line)
	if(nint(line(1)).ne.CHANNEL.and.nint(line(1)).ne.WIDE)
     *	  call bug('f','Can only flag "channel" or "wide" linetypes')
	if(nint(line(4)).ne.1)
     *	  call bug('f','Cannot flag when the linetype width is not 1')
c
	end
c***********************************************************************
	subroutine CopyDat(lIn,lScr,apri,nchan,time,MAXTIME,ntime,
     *		day0,ttol,blpres,nbase,nvis,chanoff,chanw,nosrc)
c
        implicit none
	integer lIn,lScr,nchan,maxtime,ntime,nbase,nvis,chanoff,chanw
	character apri*1
	real time(maxtime),ttol
	double precision day0
	logical blpres(nbase),nosrc
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
c    nosrc      Do not cause a gap when the source changes.
c  Output:
c    blpres	Indicates whether a particular baseline is present.
c    day0	Base time.
c    time	The time of each integration.
c    nvis	Number of visibilities.
c    ntime	Number of times.
c    nchan	Number of channels.
c    chanoff	Offset to add to channel numbers to get true channel
c               numbers.
c    chanw	Width of channel specified in linetype
c-----------------------------------------------------------------------
	include 'maxdim.h'
	logical flags(MAXCHAN),newsrc
	complex data(MAXCHAN)
	double precision preamble(4),line(6),day1
	real buf(2*MAXCHAN+3),t,tprev,maxgap
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
	if (nchan .eq. 0) call bug('f', 'No valid data found.')
	call flagchk(lIn)
	call uvinfo(lIn,'line',line)
	call uvrdvrr(lIn,'inttime',maxgap,35.0)
	maxgap = max(3.5*maxgap,50.0)*ttol
	chanoff = nint(line(3)) - 1
	chanw   = nint(line(4))
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
	    if(abs(t-tprev).gt.ttol/2.0)then
	      if(t.lt.tprev)torder = .false.
	      if(nosrc)then
	        newsrc = .false.
	      else
	        newsrc = uvVarUpd(vsrc)
	      endif
	      if(ntime.gt.0.and.
     *	        (newsrc.or.t-tprev.gt.maxgap.or.t.lt.tprev))then
	        if(ntime.ge.MAXTIME)
     *	          call bug('f','Too many times for me')
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
c
	    offset = offset + length
	  endif
	  call uvread(lIn,preamble,data,flags,MAXCHAN,nread)
	enddo
c
	if(.not.torder)
     *    call bug('w','Display will not be in strict time order')
	if(nread.ne.0)call bug('f',
     *	  'Number of channels changed while reading the data')
	if(ntime.eq.0)call bug('f','No data found')
	if(time(ntime).lt.-1) ntime = ntime - 1
	if(ntime.eq.0)call bug('f','No data found')
c
	nvis = offset / length
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
      subroutine julian_to_date(julian,day,month,year,ierr)
c
      implicit none
      integer igreg
      parameter(igreg=2299161)
      integer julian
      integer day, month, year
      integer ierr
      integer ia, ja, jb, jc, jd, je, iday, imonth, iyear
      real xc
c
      if(julian .lt. 0) then
        ierr = -1
        return
      else
        ierr = 0
      endif

      if (julian.ge.igreg) then
        ia = (real(julian-1867216)-0.25)/36524.25
        ja = julian + 1+ia-int(0.25*ia)
      else
        ja = julian
      end if

      jb = ja + 1524
      xc = (real(jb-2439870)-122.1)/365.25
      jc = 6680.0 + xc
      jd = 365*jc + int(0.25*real(jc))
      je = int(real(jb-jd)/30.6001)

      iday = jb - jd - int(30.6001*real(je))

      imonth = je - 1
      if (imonth.gt.12) imonth = imonth - 12

      iyear = jc - 4715
      if (imonth.gt.2) iyear = iyear - 1
      if (iyear.le.0) iyear = iyear - 1
c
c     Assign output values
c     
      year=iyear
      month=imonth
      day=iday

      end
c***********************************************************************
	subroutine FmtCmd(string,isave,t1,t2,chanoff,chanw,day0,
     *    selectline)
c
	character string*(*),selectline*(*)
	integer isave(5),chanoff,chanw
	real t1,t2
        double precision day0
c
c  Nicely format an editting instruction.
c
c  Input:
c     isave(5)  The details of the flag
c               1: the mode of baseline flagging
c               2: the indicator of baseline flagging
c               3: the first channel to be flagged
c               4: the last channel to be flagged
c               5: the value of the flag (0=flag,1=unflag)
c     t1        the first time to be flagged
c     t2        the last time to be flagged
c     chanoff   the channel offset from the line command
c     chanw     the channel width from the line command
c     day0      the start of the day
c  Output:
c    string	The formatted command.
c    selectline The formatted command in select keyword format
c-----------------------------------------------------------------------
	include 'mirconst.h'
        include 'maxdim.h'
	integer chan1,chan2,l,hour,minute,i,j,tbl,ls
        integer day,month,year,ierr,startday
        real second,tet
	character time1*18,time2*18,mon*3
	character flagval*4,baseflag*30,selectant*13
c
c  Externals.
c
	character itoaf*8
	integer len1
c
        if (isave(1).eq.1) then
           do i=1,MAXANT
              do j=i,MAXANT
                 tbl=((j-1)*j)/2+i
                 if (tbl.eq.isave(2)) then
                    write(baseflag,'(A,I3,A,I3)') 'baseline',
     *                   i,'-',j
                    write(selectant,'(A,I3,A,I3,A)') 'ant(',i,')(',
     *                   j,')'
                    exit
                 endif
              enddo
           enddo
        elseif ((isave(1).eq.2).or.(isave(1).eq.3)) then
           write(baseflag,'(A,I3)') 'all baselines with antenna',
     *          isave(2)
           write(selectant,'(A,I3,A)') 'ant(',isave(2),')'
        elseif (isave(1).eq.4) then
           baseflag='all baselines'
           selectant=' '
        endif
        if (isave(5).eq.1) then
           flagval='GOOD'
        else
           flagval='BAD'
        endif
        startday=int(day0+real(int(t1)))+1
        call julian_to_date(startday,day,month,year,ierr)
        year=year-1900
        if (year.gt.100) then
           year=year-100
        endif
        call monthstring(month,mon)
        tet=t1*24.0
        hour=int(tet)
        tet=(tet-real(hour))*60.0
        minute=int(tet)
        tet=(tet-real(minute))*60.0
        second=tet
        write(time1,'(I2.2,A3,I2.2,A1,I2.2,A1,I2.2,A1,F4.1)')
     *    year,mon,day,':',hour,':',minute,':',second
        startday=int(day0+real(int(t1)))+1
        call julian_to_date(startday,day,month,year,ierr)
        year=year-1900
        if (year.gt.100) then
           year=year-100
        endif
        call monthstring(month,mon)
        tet=t2*24.0
        hour=int(tet)
        tet=(tet-real(hour))*60.0
        minute=int(tet)
        tet=(tet-real(minute))*60.0
        second=tet
        write(time2,'(I2.2,A3,I2.2,A1,I2.2,A1,I2.2,A1,F4.1)') 
     *    year,mon,day,':',hour,':',minute,':',second
        if (time1(15:15).eq.' ') time1(15:15)='0'
        if (time2(15:15).eq.' ') time2(15:15)='0'
        selectline='time('//time1
        ls=len1(selectline)
        selectline(ls+1:)=','//time2
        ls=len1(selectline)
        selectline(ls+1:)=')'
        ls=len1(selectline)
        chan1=chanoff+isave(3)*chanw
        chan2=chanoff+isave(4)*chanw
        selectline(ls+1:)=',chan('//itoaf(chan1)
        ls=len1(selectline)
        selectline(ls+1:)=','//itoaf(chan2)
        ls=len1(selectline)
        selectline(ls+1:)=')'
        ls=len1(selectline)
        if (selectant.ne.' ') then
           selectline(ls+1:)=','//selectant
        endif
        string='Changing times '//time1//' to '//time2//
     *    ', channels '//itoaf(chan1)
        l=len1(string)
        string(l+1:)=' to '//itoaf(chan2)
        l=len1(string)
        string(l+1:)=' on '//baseflag
        l=len1(string)
        string(l+1:)=' to '//flagval
c
	end
c***********************************************************************
      subroutine monthstring(month,string)
c
      integer month
      character string*(*)
c
      if (month.eq.1) then
         string='JAN'
      elseif (month.eq.2) then
         string='FEB'
      elseif (month.eq.3) then
         string='MAR'
      elseif (month.eq.4) then
         string='APR'
      elseif (month.eq.5) then
         string='MAY'
      elseif (month.eq.6) then
         string='JUN'
      elseif (month.eq.7) then
         string='JUL'
      elseif (month.eq.8) then
         string='AUG'
      elseif (month.eq.9) then
         string='SEP'
      elseif (month.eq.10) then
         string='OCT'
      elseif (month.eq.11) then
         string='NOV'
      elseif (month.eq.12) then
         string='DEC'
      endif
c
      end
