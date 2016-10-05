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
c     Selecting data:
c     The user may edit the data by selecting a range of channels
c     and times with the mouse, and then flagging or unflagging them.
c     This is done by selecting two points - one with the left mouse button
c     and one with the right mouse button - that bound the desired range.
c     When a selection is made with either the left mouse button or
c     the right mouse button, a green point will be placed on the screen
c     at the selected location. If both a left mouse selection and
c     a right mouse selection have been made, a green box will be displayed
c     that will have the two selected points as opposite corners.
c     Selecting another point with either the left mouse button or
c     the right mouse button will move the appropriate corner of the box
c     to form a new selection.
c
c     To act upon this selection, or the plot itself, the user
c     should press one of the following keys while the cursor is
c     inside the main plot area:
c     aAbcCdDfFgGhHjJkKlLmMnpPqrRsStTuvVwWxXzZ ,<.?;[]12=!@*
c
c     Exiting PGFLAG:
c       a                Abort the editing procedure and quit
c                        PGFLAG immediately without applying
c                        any flags.
c       q                Apply any flagging that was requested
c                        and then exit PGFLAG.
c
c     Editing data:
c       w                Apply the flagging specified by the patch
c                        file.
c       W                Apply the flagging specified by the patch
c                        file, in reverse (ie. good <-> bad)
c       b                Blow away the dust: flag all visibilities
c                        with less than 3 good neighbours.
c       e                Extend the flagging to all sequences in
c                        time or channel with less than 20% good data.
c                        Only apply this once per baseline.
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
c       v                Flag all visibilities that have values
c                        greater than the current maximum on the
c                        scale on the displayed baseline, for this
c                        baseline only.
c       V                Flag all visibilities that have values
c                        greater than the current maximum on the
c                        scale on the displayed baseline, for all
c                        baselines.
c       <                Flag using SumThreshold method, using
c                        the parameters in flagpar
c       T                Change the Threshold and other parameters 
c                        of the SumThreshold algorithm
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
c       *                Subtract off a convolved version of the plot,
c                        the convolution parameters are specified by
c                        the flagpar parameters 2 and 3
c       ,                Fiddle the amplitude scale; press this key
c                        then click the left mouse button in the colour
c                        wedge to set the maximum value, or the right
c                        mouse button to set the minimum value.
c       .                Reset the user-set colour amplitude scale.
c       =                Autoscale plot from -10 to +10 times the
c                        median absolute deviation
c       [                Enable switch to use only the currently
c                        displayed points when deciding the colour
c                        amplitude scale.
c       ]                Set the colour amplitude scale from the current
c                        selection region.
c       ;                Lock the colour amplitude scale until it is
c                        reset or fiddled.
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
c                        different from the value displayed by 'm' if
c                        the averages have been subtracted from the
c                        display.
c       M                Locate the sample with the maximum value on
c                        the currently displayed baseline, and print
c                        information about it into the controlling
c                        terminal. Pressing it twice in succession will
c                        make PGFLAG create a selection region of 20
c                        chans and 20 times centred on this sample.
c       P                Display the current selection on the secondary
c                        plot device as a spectrum. This command will
c                        work only if device2 is specified.
c       D                Dump the current page on to the tertiary plot
c                        device. This command will only work if
c                        device3 is specified.
c
c     Non interactive flagging:
c     Using the command parameter and the flagpar parameters you can use
c     pgflag in non-interactive mode. The recommended strategy is to run
c     pgflag interactively, work out what flagpar parameters work best 
c     using the * command to see the effect of background subtraction and
c     the < command to see the effects of SumThreshold flagging. 
c     If too much or too little was flagged, change the parameters with
c     the 'T' command and undo the flagging with the 'rr' command and try
c     again. Once the right parameters are found you can abort with 'a' 
c     and run pgflag with command set to "<" or "<be" to flag the entire 
c     dataset. You can use options=nodisp if you don't want to watch the
c     flagging. If you do want to see what is happening, you'll want to 
c     specify command='.=<' or the like to scale the data for the display.
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
c     Default is Stokes i.
c     If more than one polarization is specified only the LAST one is 
c     displayed.
c     If one 'raw' polarization is specified, only that one is flagged.
c     If multiple 'raw' polarizations or one or more 'converted' 
c     polarizations are specified, all polarizations in the data are 
c     flagged.
c@ device
c     PGPLOT plot device/type, which must be interactive. No default.
c@ device2
c     PGPLOT plot device/type, which must be interactive. This optional
c     plot device will be used to display spectra from the selected
c     region, if requested.
c@ device3
c     PGPLOT plot device/type, which does not need to be interactive.
c     This optional plot device is the destination for the D command.
c@ mode
c     Display ``amplitude'' or ``phase''. By default, ``amplitude''
c     is selected. For mode=``phase'', the phase is in degrees.
c@ flagpar
c     Parameters for SumThreshold flagging, dusting and extending
c     (see Offringa et al,2010, MNRAS 405,155)
c     1 : Threshold in estimated sigma's (estimated using the 
c         median absolute deviation), default 7
c     2 : Convolution size for channel direction (in channels), used
c         to generate a smooth background, default 1. Zero disables
c         convolution in the channel direction.
c     3 : Convolution size for time direction (in integrations), used
c         to generate a smooth background, default 1. Zero disables
c         convolution in the time direction
c     4 : Number of iterations of the convolve/subtract/threshold
c         operation.The threshold level decreases by a factor of two
c         each iteration. Default 3
c     5 : Power of two of the maximum number of points used in the
c         SumThreshold operation (e.g., 5 -> 32 points). Default 5.
c     6 : Dust the plot - flag points with less than flagpar(6) 
c         unflagged neighbours. Useful range 1-4, default 3.
c     7 : Extend flags to all points which are in a time or
c         channel sequence with less than flagpar(7) % good data,
c         default 20. This process 'eats' away at the data from the
c         flagged areas. (See Offringa et al 2012, A&A 539,A95)
c@ patch
c     The name of the flagging patch file, either to write (if the
c     'patch' option is specified), or to use for the 'w' or 'W'
c     commands.
c@ log
c     The name of a log file to which will be output the entire set
c     of flagging actions, in text format.
c@ command
c     Specify a series of commands for non-interactive flagging.
c     E.g., '<be' will apply SumThreshold flagging followed
c      by blowing away the dust and extending the flags for each baseline;
c     '.=vx=v' will autoscale the data, do a clip operation, then
c      subtract the channel average, autoscale and clip again before
c     moving on to the next baseline. Note that '=' cannot be the first
c     character of the command parameter or it will be skipped.
c     There is no need to specify a 'q' in the command sequence,
c     unless you want to quit before all baselines are processed.
c     Default is no command.
c@ options
c     Task enrichment parameters. Several can be given, separated by
c     commas. Minimum match is used. Possible values are:
c       patch   Generate a flagging patch file that contains all the
c               flagging operations that are done by this run of
c               pgflag.
c       selgen  Generate a file appropriate for selecting the bad data
c               (via a 'select' keyword). The output is two text files
c               called 'pgflag_flag.select' (for flagging operations),
c               and 'pgflag_unflag.select' (for unflagging operations).
c               Unfortunately, since 'select' does not support the use
c               of a 'channel' selection, this option is of limited use,
c               but is supplied in case time-based selection is all that
c               is required.
c       nosrc   Do not cause a break in the display when the source
c               changes. Normally PGFLAG puts a gap in the display
c               whenever the source changes.
c       noapply Do not apply the flagging.
c       nodisp  Do not use the display, just use the specified command 
c               to flag all baselines in the dataset.
c     The following options can be used to disable calibration.
c       nocal   Do not apply antenna gain calibration.
c       nopass  Do not apply bandpass correction.
c       nopol   Do not apply polarisation leakage correction.
c
c$Id: pgflag.for,v 1.30 2016/10/05 23:06:50 wie017 Exp $
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
c    vjm 09Dec09  Merge to mainline. Remove the eol characters. Add RCS
c                 tags.
c    jbs 07Oct10  Fix bug that stopped single-scan mosaics from being
c                 displayed, since each source change creates a scan
c                 break (represented internally by -2), making it
c                 impossible to find an intermediate time sample; now
c                 it works, but looks stupid - can make it look better
c                 by using 'options=nosrc'. Made a number of changes
c                 to allow selection of the first and last channels and
c                 times.
c    jbs 21Feb11  Fix bug that wouldn't allow user to make measurements
c                 of, or flag, visibilities that were from the day
c                 after the starting date of the plot. Added the 'V' and
c                 'v' commands to flag bright pixels.
c    jbs 22Feb11  Fix bug in the way flagging regions were extended near
c                 time break boundaries. Made a common task to handle
c                 conversion to a string representation of the time.
c                 Now possible to plot an average spectrum of the
c                 selected region on another specified PGPLOT device.
c    mhw 30Mar12  Add selected features from the AOFlagger and
c                 non interactive mode.
c    jbs 27Nov12  Remove all the crap from the history,
c                 and make a flagging 'patch' that
c                 contains all the flagging that we have done, and that
c                 can be used later to repeat the exact procedure.
c                 The same stuff can be made into a human-readable
c                 text log file.
c    mhw 23Aug13  Add 'e' command for extension of flags.
c    jbs 16Dec13  Add hardcopy output option D.
c    mhw 30Apr14  Make less verbose in nodisp mode
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'
      include 'maxnax.h'
      include 'mem.h'
c
      integer MAXTIME,MAXEDIT
      parameter(MAXTIME=10000,MAXEDIT=10000000)
c
      character versan*72, version*72
      character device*80,xaxis*12,yaxis*12,uvflags*12,val*16
      character device2*80,device3*80
      integer devicenum,device2num,device3num
      logical selgen,noapply
      integer npol,tno,i,j,length
c
c Data storage.
c
      double precision day0,cfreq(MAXCHAN)
      integer lScr,nchan,ntime,nvis,chanoff,chanw,nbl,cbl
      integer newbl,tbl,mbl,mant,k1,k2,toklen
      integer pant1,pant2,pchan1,pchan2,pflag
      real t1(MAXTIME),ttol,curs_x,curs_y
      logical blpres(MAXBASE),nosrc,nodisp,logstats,needplot,needread
      logical genpatch,keepreading,pantok,genlog,autopatch
      parameter(ttol=1.0/86400.0)
      integer iFlg,iDat,curr_zooms(2,2),points(2,2)
      integer min_x_zoom,max_x_zoom,min_y_zoom,max_y_zoom,patchlen
      character pressed*2,previous_pressed*2,patchline*80
      integer meas_channel,shift_x,shift_y,f_a1,f_a2,f_mode
      real meas_freq,meas_amp,meas_maxval
      double precision patcht1,patcht2,ptdiff
      character meas_time*20,yn*20,status*60,promp*30,string*20
      character patchfile*80,patchtok*80,logfile*80
      character*80 command,prefts,patchts
      integer icmd,ncmd,previous_bl
      logical plot_top,plot_average,plot_main,plot_points,tmfnd
      logical do_flag,do_unflag,do_undoflag,subavgc,subavgt,subbkgnd
      integer times(2,MAXEDIT)
      integer chans(2,MAXEDIT),nflags,aflags,nthis,last_nflags
      integer region_width,meastype,nIter,l,iresult,lfp,iostat
      integer bases(2,MAXEDIT)
      logical flagval(MAXEDIT),some_unflagged,scale_locked
      logical going_forward,going_backward,fiddle_active
      logical use_fiddle,keep_looping,colour_from_window
      logical colour_from_region,max_makeregion,ok,verbose
      real fiddle_min,fiddle_max,datamin,datamax,fiddlefraction
      real flagpar(7),level,result
c
c Externals
c
      integer pgopen,len1
      logical uvDatOpn

      version = versan ('pgflag',
     :                  '$Revision: 1.30 $',
     :                  '$Date: 2016/10/05 23:06:50 $')

c
c Get user inputs
c
      call keyini
      call keya('device',device,' ')
      call keya('device2',device2,' ')
      call keya('device3',device3,' ')
      call GetAxis(xaxis,yaxis)
      call keyr('flagpar',flagpar(1),7.0)
      call keyr('flagpar',flagpar(2),1.0)
      call keyr('flagpar',flagpar(3),1.0)
      call keyr('flagpar',flagpar(4),3.0)
      call keyr('flagpar',flagpar(5),5.0)
      call keyr('flagpar',flagpar(6),3.0)
      call keyr('flagpar',flagpar(7),20.0)
      flagpar(7)=flagpar(7)/100
      call keya('command',command,' ')
      call keya('patch',patchfile,' ')
      call keya('log',logfile,' ')
      if (command.eq.' ') command=''
      if (patchfile.eq.' ') patchfile=''
      genlog=.true.
      if (logfile.eq.' ') then
         genlog=.false.
         logfile=''
      endif
      logstats=.true.
      call GetOpt(selgen,noapply,nosrc,nodisp,genpatch,uvflags)
      verbose = .not.nodisp
      if (command.eq.''.and.nodisp) call bug('f','Nothing to do')
      if (device.eq.' '.and..not.nodisp) 
     *   call bug('f','A PGPLOT device must be given')
      call uvDatInp('vis',uvflags)
      call keyfin
c
c  Set the default polarisation type if needed.
c
      call uvDatGti('npol',npol)
      if(npol.eq.0)call uvDatSet('stokes',1)
c
c     Open the input data.
c
      if(.not.uvDatOpn(tno))call bug('f','Error opening input')
c
c     Open the plot device(s).
c
      if (.not.nodisp) then
        devicenum=pgopen(device)
        if(devicenum.le.0)then
           call pgldev
           call bug('f','Unable to open PGPLOT device')
        endif
        device2num=0
        if (device2.ne.' ') then
           device2num=pgopen(device2)
           if (device2num.le.0) then
              call pgldev
              call bug('f','Unable to open PGPLOT device')
           endif
        endif
        device3num=0
        if (device3.ne.' ') then
           device3num=pgopen(device3)
           if (device3num.le.0) then
              call pgldev
              call bug('f', 'Unable to open PGPLOT device')
           endif
        endif
        call pgslct(devicenum)
        call pgqinf('CURSOR',val,length)
        if(val.eq.'NO')call bug('f','PGPLOT device is not interactive')
        call pgask(.false.)
        if (device2num.gt.0) then
           call pgslct(device2num)
           call pgask(.false.)
           call pgslct(devicenum)
        endif
        if (device3num.gt.0) then
           call pgslct(device3num)
           call pgask(.false.)
           call pgslct(devicenum)
        endif
      endif
c
c     Use a scratch file for the data.
c
      call scropen(lScr)
      call CopyDat(tno,lScr,yaxis,nchan,t1,MAXTIME,ntime,day0,ttol,
     *             blpres,MAXBASE,nvis,chanoff,chanw,nosrc,mbl,mant)
      call uvinfo(tno,'sfreq',cfreq)
c
c Determine the number of baselines.
c
      nflags=0
      nbl=0
      do i=1,mbl
         if (blpres(i)) then
	   nbl=nbl+1
	 endif
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
      do i=1,mbl
         if (blpres(i)) then
            cbl=i
            goto 10
         endif
      enddo
 10   needread=.true.
      needplot=.true.
      autopatch=.false.
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
      subbkgnd=.false.
      fiddle_active=.false.
      use_fiddle=.false.
      keep_looping=.true.
      colour_from_window=.false.
      scale_locked=.false.
      colour_from_region=.false.
      previous_pressed=' '
      icmd=1
      ncmd=len1(command)
      if (ncmd.gt.0) then
        if (command(ncmd:ncmd).ne.'n') then
          ncmd=ncmd+1
          command(ncmd:ncmd)='n'
        endif
      endif
      do while (keep_looping)
         if (needread .eqv. .true.) then
            some_unflagged=.false.
            if (.not.scale_locked) then
               use_fiddle=.false.
            endif
            do while (some_unflagged.eqv..false.)
               call Gridit(memI(iFlg),memR(iDat),nchan,ntime,cbl,
     *              day0,lScr,nvis,t1,some_unflagged)
               call ApplyFlags(memI(iFlg),nchan,ntime,chans,times,
     *            bases,flagval,MAXEDIT,nflags,cbl,mant,some_unflagged)
               if (some_unflagged.eqv..false.) then
                  if (going_forward) then
                     do i=cbl+1,mbl
                        if (blpres(i)) then
                           newbl=i
                           goto 20
                        endif
                     enddo
 20                  if (newbl.eq.cbl) then
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
            do i=1,mant
               do j=i,mant
                  tbl=((j-1)*j)/2+i
                  if (cbl.eq.tbl) then
                     f_a1=i
                     f_a2=j
                     goto 25
                  endif
               enddo
            enddo
         endif
c     Now plot the data.
 25      if (needplot.and..not.nodisp) then
            call MakePlot(memI(iFlg),memR(iDat),t1,nchan,ntime,cbl,
     *           curr_zooms,meas_channel,meas_freq,meas_time,meas_amp,
     *           plot_top,plot_average,plot_main,points,plot_points,
     *           yaxis,chanoff,chanw,subavgc,subavgt,subbkgnd,datamin,
     *           datamax,fiddle_min,fiddle_max,use_fiddle,
     *           colour_from_window,colour_from_region,fiddle_active,
     *           flagpar)
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
         if (ncmd.eq.0) then
           call pgband(7,0,0.0,0.0,curs_x,curs_y,pressed)
         else
           if (previous_pressed.eq.'n'.and.previous_bl.eq.cbl) then
             pressed='q'
             keep_looping=.false.
           else
              pressed=''
              if ((command(icmd:icmd).eq.'w'.or.
     *             command(icmd:icmd).eq.'W').and.
     *             autopatch.eqv..true.) then
                 icmd = icmd + 1
              else
                 pressed=command(icmd:icmd)
                 icmd = icmd + 1
              endif
             if (icmd.gt.ncmd) icmd=1
           endif
           previous_bl = cbl
         endif
c         write(status,'(A,I6,I6)') 'Command: '//pressed(1:1),
c     *        int(anint(curs_x)),int(anint(curs_y))
c         call output(status)
c     Do what the user wants and nobody gets hurt.
         if ((pressed(1:1).eq.'n').or.(pressed(1:1).eq.'p')) then
c     change the baseline
            if (pressed(1:1).eq.'n') then
c     next baseline requested
               going_forward=.true.
               going_backward=.false.
               newbl=cbl
               do i=cbl+1,mbl
                  if (blpres(i)) then
                     newbl=i
                     goto 30
                  endif
               enddo
 30            if (newbl.ne.cbl) then
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
               do i=cbl-1,1,-1
                  if (blpres(i)) then
                     newbl=i
                     goto 35
                  endif
               enddo
 35            if (newbl.ne.cbl) then
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
            if (int(anint(curs_x)).ge.curr_zooms(1,1)) then
               if (int(anint(curs_x)).le.curr_zooms(1,2)) then
                  if (int(anint(curs_y)).ge.curr_zooms(2,1)) then
                     if (int(anint(curs_y)).le.curr_zooms(2,2)) then
                        call MakeMeasurement(memI(iFlg),memR(iDat),
     *                       nchan,ntime,anint(curs_x),anint(curs_y),
     *                       day0,t1,meas_channel,meas_freq,meas_time,
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
               if ((int(anint(curs_x)).ge.curr_zooms(1,1)).and.
     *              (int(anint(curs_x)).le.curr_zooms(1,2)).and.
     *              (int(anint(curs_y)).ge.curr_zooms(2,1)).and.
     *              (int(anint(curs_y)).le.curr_zooms(2,2))) then
                  if (pressed(1:1).eq.'A') then
c     left mouse button
                     points(1,1)=int(anint(curs_x))
                     points(1,2)=int(anint(curs_y))
                  else
c     right mouse button
                     points(2,1)=int(anint(curs_x))
                     points(2,2)=int(anint(curs_y))
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
            do i=1,mant
               do j=i,mant
                  tbl=((j-1)*j)/2+i
                  if (cbl.eq.tbl) then
                     f_a1=i
                     f_a2=j
                     goto 40
                  endif
               enddo
            enddo
 40         if ((pressed(1:1).eq.'f').or.(pressed(1:1).eq.'F')) then
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
     *        chanw,.true.)
            call ApplyFlags(memI(iFlg),nchan,ntime,chans,times,
     *        bases,flagval,MAXEDIT,nflags,cbl,mant,some_unflagged)
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
     *           chanw,region_width,max_makeregion,points,day0,t1,
     *           meas_maxval)
            if (max_makeregion) then
               needplot=.true.
               plot_points=.true.
            endif
         elseif ((pressed(1:1).eq.'v').or.(pressed(1:1).eq.'V')) then
c     flag out all regions brighter than the currently set maximum
c     plot value, on this baseline, or on all baselines
            max_makeregion=.true.
            region_width=0
            last_nflags=nflags
	    call Thres(memR(iDat),memI(iFlg),nchan,ntime,chans,times,
     *          bases,flagval,MAXEDIT,nflags,cbl,t1,-datamax,0,verbose)
            do_flag=.true.
            if (pressed(1:1).eq.'V') then
              do i=last_nflags+1,nflags
                bases(2,i)=4
              enddo
            endif
            needplot=.true.
            plot_top=.true.
            plot_average=.true.
            plot_main=.true.
c     clear the selection
            call reset_points(points)
            plot_points=.true.
         elseif (pressed(1:1).eq.'x') then
            subavgc=.not.subavgc
            subavgt=.false.
            subbkgnd=.false.
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_average=.true.
            plot_top=.true.
         elseif (pressed(1:1).eq.'d') then
            subavgt=.not.subavgt
            subavgc=.false.
            subbkgnd=.false.
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
            if (verbose) 
     *         call output('Resetting colour amplitude scaling.')
            use_fiddle=.false.
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_top=.true.
            plot_average=.true.
            colour_from_window=.false.
            scale_locked=.false.
         elseif (pressed(1:1).eq.'=') then
            if (verbose) call output('Autoscaling colour amplitude.')
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_top=.true.
            plot_average=.true.
            colour_from_window=.false.
            call GetScale(memR(iDat),memI(iFlg),nchan,ntime,
     *         fiddle_min,fiddle_max)
            use_fiddle=.true.
            scale_locked=.false.
            datamin=fiddle_min
            datamax=fiddle_max
         elseif (pressed(1:1).eq.'q') then
c     quit, applying the flagging
            if (keep_looping) then
              yn=' '
              write(*,*) ''
              write(*,*) 'Really quit and apply flagging? y[n]'
              read(*,*) yn
              l = len1(yn)
              if (yn(l:l).eq.'y') then
                 keep_looping=.false.
              endif
            endif
         elseif (pressed(1:1).eq.'a') then
c     abort, discarding all flags
            yn=' '
            write(*,*) ''
            write(*,*) 'Really abort, discarding all flagging? y[n]'
            read(*,*) yn
            l = len1(yn)
            if (yn(l:l).eq.'y') then
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
               if (bases(1,i).eq.cbl.and.bases(2,i).eq.1) then
                  bases(2,i)=0
               endif
            enddo
            call ApplyFlags(memI(iFlg),nchan,ntime,chans,times,
     *           bases,flagval,MAXEDIT,nflags,cbl,mant,some_unflagged)
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
               bases(2,i)=0
            enddo
            call ApplyFlags(memI(iFlg),nchan,ntime,chans,times,
     *           bases,flagval,MAXEDIT,nflags,cbl,mant,some_unflagged)
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
               if (bases(1,i).eq.cbl.and.bases(2,j).eq.1) then
                  nthis=nthis+1
               endif
            enddo
            if (aflags.gt.0) then
               write(status,'(A,I6,A)') ' Applying to this baseline: ',
     *               nthis,' flags'
               call output(status)
            endif
         elseif (pressed(1:1).eq.'P') then
c     plot the selection as a spectrum in the secondary device, if
c     available
            meastype=0
            if (device2num.gt.0) then
               call pgslct(device2num)
               call PlotSpectrum(memI(iFlg),memR(iDat),nchan,ntime,
     *              points,day0,t1,chanoff,chanw,cfreq,meastype,yaxis)
               call pgslct(devicenum)
            endif
         elseif (pressed(1:1).eq.'*') then
            subbkgnd = .not.subbkgnd
            if (subbkgnd) call output('Subtract smooth background')
            subavgt=.false.
            subavgc=.false.
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_average=.true.
            plot_top=.true.
         elseif (pressed(1:1).eq.'<') then
            write(status,'(A,I3,1x,I3)') 
     *        'Do SumThreshold operation on baseline ',f_a1,f_a2
            if (verbose) call output(status)
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_average=.true.
            plot_top=.true.
            nIter=nint(flagpar(4))
            do i=1,nIter
             level=flagpar(1)*2**(nIter-i)
              call subConvl(memR(iDat),t1,nchan,ntime,memI(iFlg),
     *          flagpar(2),flagpar(3))
	      call Thres(memR(iDat),memI(iFlg),nchan,ntime,chans,times,
     *          bases,flagval,MAXEDIT,nflags,cbl,t1,level,
     *          nint(flagpar(5)),verbose)
            enddo 
         elseif (pressed(1:1).eq.'b') then
            if (verbose) call output('Blow away the dust...')
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_average=.true.
            plot_top=.true.
            call dust(memI(iFlg),t1,nchan,ntime,
     *                nint(flagpar(6)),chans,times,bases,flagval,
     *                MAXEDIT,nflags,cbl,verbose)
         elseif (pressed(1:1).eq.'e') then
            if (verbose) call output('Extend flagged areas...')
            needplot=.true.
            plot_main=.true.
            plot_points=.true.
            plot_average=.true.
            plot_top=.true.
            call expand(memI(iFlg),t1,nchan,ntime,
     *                flagpar(7),chans,times,bases,flagval,
     *                MAXEDIT,nflags,cbl,verbose)
         elseif (pressed(1:1).eq.'D') then
            if (device3num.gt.0) then
               write(*,*) 'Dumping current screen to file.'
               call pgslct(device3num)
               call pgpap(11.69,0.7074)
               call pgpage()
               plot_top=.true.
               plot_average=.true.
               plot_main=.true.
               plot_points=.true.
               call MakePlot(memI(iFlg),memR(iDat),t1,nchan,ntime,
     *              cbl,curr_zooms,meas_channel,meas_freq,meas_time,
     *              meas_amp,plot_top,plot_average,plot_main,points,
     *              plot_points,yaxis,chanoff,chanw,subavgc,subavgt,
     *              subbkgnd,datamin,datamax,fiddle_min,fiddle_max,
     *              use_fiddle,colour_from_window,colour_from_region,
     *              fiddle_active,flagpar)
               call pgslct(devicenum)
               plot_top=.false.
               plot_average=.false.
               plot_main=.false.
               plot_points=.false.
            endif
         elseif (pressed(1:1).eq.'T') then
            call output('Change SumThreshold parameters')
            write(promp,'(A,F4.1,A)') 
     *        'Threshold             (',flagpar(1),'): '
            call prompt(string,length,promp)
            if (length.gt.0) then
              call atorf(string,result,ok)
              if (ok) flagpar(1)=max(1.,result)
            endif 
            write(promp,'(A,F4.1,A)') 
     *        'Conv size - channels  (',flagpar(2),'): '
            call prompt(string,length,promp)
            if (length.gt.0) then
              call atorf(string,result,ok)
              if (ok) flagpar(2)=max(0.,result) 
            endif
            write(promp,'(A,F4.1,A)') 
     *        'Conv size - timeslots (',flagpar(3),'): '
            call prompt(string,length,promp)
            if (length.gt.0) then
              call atorf(string,result,ok)
              if (ok) flagpar(3)=max(0.,result)
            endif 
            write(promp,'(A,I2,A)') 
     *        'Iterations              (',nint(flagpar(4)),'): '
            call prompt(string,length,promp)
            if (length.gt.0) then
              call atoif(string,iresult,ok)
              if (ok) flagpar(4)=max(1,iresult)
            endif 
            write(promp,'(A,I2,A)') 
     *        'Max #points as 2^n    (n=',nint(flagpar(5)),'): '
            call prompt(string,length,promp)
            if (length.gt.0) then
              call atoif(string,iresult,ok)
              if (ok) flagpar(5)=max(1,iresult)
            endif
            write(promp,'(A,I2,A)') 
     *        'Minumum # neighbours    (',nint(flagpar(6)),'): '
            call prompt(string,length,promp)
            if (length.gt.0) then
              call atoif(string,iresult,ok)
              if (ok) flagpar(6)=max(1,iresult)
            endif
            write(promp,'(A,F4.1,A)') 
     *        'Percentage            (',flagpar(7)*100,'): '
            call prompt(string,length,promp)
            if (length.gt.0) then
              call atorf(string,result,ok)
              if (ok) flagpar(7)=max(0.,result)/100
            endif
         elseif ((pressed(1:1).eq.'w').or.(pressed(1:1).eq.'W')) then
c     Take the specified patch file and do the flagging.
c     Open the patch file.
            call txtopen(lfp,patchfile,'old',iostat)
            if (iostat.ne.0) then
               call bug('w','Error opening patch file '//patchfile)
               call bugno('w',iostat)
            else
               autopatch=.true.
c     Read in each flag and add it to the list.
               keepreading=.true.
               if (pressed(1:1).eq.'w') then
                  call output('Flagging using patch file.')
               else
                  call output('Reversing patch file flagging.')
               endif
               do while (keepreading)
                  call txtread(lfp,patchline,patchlen,iostat)
                  if (iostat.eq.-1) then
c     This is the end of the file.
                     keepreading=.false.
                  elseif (iostat.eq.0) then
c     Interpret the line.
                     k1=1
                     k2=patchlen
                     f_mode=-1
c     The first element is the first antenna in the baseline.
                     call gettok(patchline,k1,k2,patchtok,toklen)
c     Turn it into an integer.
                     call atoif(patchtok,pant1,pantok)
                     if (.not.pantok) then
                        call bug('w','Bad read of patch ant1')
                        keepreading=.false.
                     else
                        f_a1=pant1
                     endif
c     The second element is the second antenna in the baseline.
                     call gettok(patchline,k1,k2,patchtok,toklen)
c     Turn it into an integer.
                     call atoif(patchtok,pant2,pantok)
                     if (.not.pantok) then
                        call bug('w','Bad read of patch ant2')
                        keepreading=.false.
                     else
                        f_a2=pant2
                     endif
c     Check the baseline.
                     if (f_a1.eq.0) then
                        f_mode=4
                     elseif (f_a2.eq.0) then
c     This is all baselines with f_a1.
                        f_mode=2
                     else
c     This is a single baseline.
                        f_mode=1
                     endif
c     The next element is the first time.
                     if (keepreading) then
                        call gettok(patchline,k1,k2,patchtok,toklen)
c     Find this time in terms of the samples.
                        call atodf(patchtok,patcht1,pantok)
                        if (.not.pantok) then
                           call bug('w','Bad read of patch t1')
                           keepreading=.false.
                        else
                           patcht1=patcht1-day0
                           write(patchts,'(F18.5)') patcht1
                           tmfnd=.false.
                           do i=1,ntime
                              write(prefts,'(F18.5)') t1(i)
                              ptdiff=t1(i)-patcht1
                              if (prefts.eq.patchts) then
                                 points(1,2)=i
                                 tmfnd=.true.
                              endif
                           enddo
                           if (.not.tmfnd) then
c     We couldn't find this time.
                              call bug('w','Patch file not applicable')
                              keepreading=.false.
                           endif
                        endif
                     endif
c     The next element is the first time.
                     if (keepreading) then
                        call gettok(patchline,k1,k2,patchtok,toklen)
c     Find this time in terms of the samples.
                        call atodf(patchtok,patcht2,pantok)
                        if (.not.pantok) then
                           call bug('w','Bad read of patch t2')
                           keepreading=.false.
                        else
                           patcht2=patcht2-day0
                           write(patchts,'(F18.5)') patcht2
                           tmfnd=.false.
                           do i=1,ntime
                              write(prefts,'(F18.5)') t1(i)
                              if (prefts.eq.patchts) then
                                 points(2,2)=i
                                 tmfnd=.true.
                              endif
                           enddo
                           if (.not.tmfnd) then
c     We couldn't find this time.
                              call bug('w','Patch file not applicable')
                              keepreading=.false.
                           endif
                        endif
                     endif
c     The next element is the first channel.
                     if (keepreading) then
                        call gettok(patchline,k1,k2,patchtok,toklen)
                        call atoif(patchtok,pchan1,pantok)
                        if (.not.pantok) then
                           call bug('w','Bad read of patch chan1')
                           keepreading=.false.
                        else
                           points(1,1)=(pchan1-chanoff)/chanw
                        endif
                     endif
c     The next element is the second channel.
                     if (keepreading) then
                        call gettok(patchline,k1,k2,patchtok,toklen)
                        call atoif(patchtok,pchan2,pantok)
                        if (.not.pantok) then
                           call bug('w','Bad read of patch chan2')
                           keepreading=.false.
                        else
                           points(2,1)=(pchan2-chanoff)/chanw
                        endif
                     endif
c     The next element is the flag action.
                     if (keepreading) then
                        call gettok(patchline,k1,k2,patchtok,toklen)
                        call atoif(patchtok,pflag,pantok)
                        if (.not.pantok) then
                           call bug('w','Bad read of patch flag')
                           keepreading=.false.
                        else
                           do_undoflag=.false.
                           if (pflag.eq.0) then
                              do_flag=.true.
                              do_unflag=.false.
                           else
                              do_flag=.false.
                              do_unflag=.true.
                           endif
                        endif
                     endif
c     Now do the flagging.
                     if (keepreading) then
                        if (pressed(1:1).eq.'W') then
c     Reverse the flagging action.
                           do_flag=.not.do_flag
                           do_unflag=.not.do_unflag
                        endif
                        call FlagData(points,f_a1,f_a2,f_mode,
     *                       do_flag,do_unflag,do_undoflag,chans,times,
     *                       bases,flagval,MAXEDIT,nflags,day0,t1,ntime,
     *                       chanoff,chanw,.false.)
                     endif
                  else
c     This is an error.
                     call output('Error reading patch file.')
                     call bugno('w',iostat)
                     keepreading=.false.
                  endif
               enddo
c     Close the patch file.
               call txtclose(lfp)
c     Apply the flags to the screen.
               call ApplyFlags(memI(iFlg),nchan,ntime,chans,times,
     *              bases,flagval,MAXEDIT,nflags,cbl,mant,
     *              some_unflagged)
               needplot=.true.
               plot_top=.true.
               plot_average=.true.
               plot_main=.true.
               plot_points=.true.
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
     *        day0,ntime,chanoff,chanw,selgen,logstats,genpatch,
     *        patchfile,genlog,logfile)
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
c Close the PGPLOT device(s).
c
      if (.not.nodisp) then
        call pgclos()
        if (device2num.gt.0) then
           call pgslct(device2num)
           call pgclos()
        endif
        if (device3num.gt.0) then
           call pgslct(device3num)
           call pgclos()
        endif
      endif
c
c     Close the file
c
      call uvDatCls

      end
c***********************************************************************
      subroutine WriteFlags(bases,chans,times,flagval,nflags,tno,t1,
     *  day0,ntime,chanoff,chanw,selgen,logstats,genpatch,patchfile,
     *  genlog,logfile)
c
      include 'maxdim.h'
      integer nflags,ntime,tno,chanoff,chanw
      integer chans(2,nflags),times(2,nflags)
      integer bases(2,nflags)
      logical flagval(nflags),selgen,logstats,genpatch,genlog
      real t1(ntime)
      double precision day0
      character patchfile*80,logfile*80
c
c  Write out the user generated flags to the UV file.
c
c  Input:
c    bases    the baselines to flag
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
c    logstats whether to log only the flagging statistics to the
c             history instead of the hideous detail
c    genpatch whether to make a flagging patch file
c    patchfile the name of the flagging patch file to make
c  Output:
c    none
c-----------------------------------------------------------------------
      integer MAXEDIT2
      parameter (MAXEDIT2=1000000)
      integer isave(5),nchan,ant1,ant2,bl,i,j,lu,lf,iostat,l,k,lfp,lfl
      character flagstring*120,selectline*120
      double precision preamble(4)
      complex data(MAXCHAN)
      logical flags(MAXCHAN),flagged,more,blselect
      real t,tprev
      integer oldflags_bad,oldflags_good,newflags_bad,newflags_good
      integer flags_goodtobad,flags_badtogood
      integer time1,time2,ntflag,tflag(MAXEDIT2)
      character outline*256
      character flagfile*32
      character unflagfile*32
c
c     Externals
c
      integer len1
c
c     Files to create in local directory, if selgen is true
      flagfile='pgflag_flag.select'
      unflagfile='pgflag_unflag.select'
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
            call txtopen(lf,flagfile,'new',iostat)
            if (iostat.ne.0) then
               call bug('w',
     *           'Error opening output text file '//flagfile)
               call bugno('w',iostat)
            endif
            call txtopen(lu,unflagfile,'new',iostat)
            if (iostat.ne.0) then
               call bug('w',
     *           'Error opening output text file '//unflagfile)
               call bugno('w',iostat)
            endif
         endif
c     write out a flagging patch file
         if (genpatch) then
            call txtopen(lfp,patchfile,'new',iostat)
            if (iostat.ne.0) then
               call bug('w',
     *           'Error opening patch file '//patchfile)
               call bugno('w',iostat)
            endif
         endif
c     write out a flagging log file
         if (genlog) then
            call txtopen(lfl,logfile,'new',iostat)
            if (iostat.ne.0) then
               call bug('w',
     *           'Error opening patch file '//logfile)
               call bugno('w',iostat)
            endif
         endif
c        we have nflags new flags to apply
c        to apply a flag value we need these coordinates:
c          a baseline, a set of channels and a time range
         do i=1,nflags
c     determine what type of flagging
            isave(1)=bases(2,i)
            isave(2)=bases(1,i)
            isave(3)=chans(1,i)
            isave(4)=chans(2,i)
            if (flagval(i)) then
               isave(5)=0
            else
               isave(5)=1
            endif
            if (bases(2,i).gt.0) then
               time1=min(times(1,i),ntime)
               time2=min(times(2,i),ntime)
c
c      Only log large flagged areas, not single points
c
               if ((bases(2,i).ne.1.or.(times(2,i)-times(1,i)).gt.10.or.
     *           (chans(2,i)-chans(1,i)).gt.50).and..not.logstats) then
                 call FmtCmd(flagstring,isave,t1(time1),
     *              t1(time2),chanoff,chanw,day0,selectline)
                 if (.not.logstats) call hiswrite(tno,
     *                'PGFLAG: '//flagstring)
               endif
               if (genpatch) then
                  call FmtPatch(flagstring,isave,t1(time1),
     *               t1(time2),chanoff,chanw,day0,.false.)
                  l=len1(flagstring)
                  call txtwrite(lfp,flagstring,l,iostat)
                  if (iostat.ne.0) then
                     call bug('w',
     *                    'Error writing to file '//patchfile)
                     call bugno('w',iostat)
                  endif
               endif
               if (genlog) then
                  call FmtPatch(flagstring,isave,t1(time1),
     *               t1(time2),chanoff,chanw,day0,.true.)
                  l=len1(flagstring)
                  call txtwrite(lfl,flagstring,l,iostat)
                  if (iostat.ne.0) then
                     call bug('w',
     *                    'Error writing to file '//logfile)
                     call bugno('w',iostat)
                  endif
               endif
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
     *                       'Error writing to file '//flagfile)
                        call bugno('w',iostat)
                     endif
                  else
                     call txtwrite(lu,selectline,l,iostat)
                     if (iostat.ne.0) then
                        call bug('w',
     *               'Error writing to file '//unflagfile)
                        call bugno('w',iostat)
                     endif
                  endif
               endif
            endif
         enddo
c     do the flagging
         tprev=0
         call uvread(tno,preamble,data,flags,MAXCHAN,nchan)
         do while (nchan.gt.0)
            do i=1,nchan
               if (flags(i)) then
                  oldflags_good=oldflags_good+1
               else
                  oldflags_bad=oldflags_bad+1
               endif
            enddo
            t=preamble(3)-day0
            flagged=.false.
            call basant(preamble(4),ant1,ant2)
            bl=((ant2-1)*ant2)/2+ant1
            if (t.ne.tprev) then
               ntflag=0
               do i=1,nflags
                  time1=min(times(1,i),ntime)
                  time2=min(times(2,i),ntime)
                  if ((t1(time1).le.t).and.
     *                (t1(time2).ge.t)) then
                     ntflag=ntflag+1
                     if (ntflag.gt.MAXEDIT2) 
     *                 call bug('f','MAXEDIT2 exceeded')
                     tflag(ntflag)=i
                  endif
               enddo
            endif
            do k=1,ntflag
               i=tflag(k)
               blselect = bases(2,i).eq.4.or.
     *         (bases(2,i).eq.1.and.bases(1,i).eq.bl).or.
     *         (bases(2,i).eq.2.and.bases(1,i).eq.ant1).or.
     *         (bases(2,i).eq.3.and.bases(1,i).eq.ant2)     
               if (blselect) then
                  flagged=.true.
                  do j=chans(1,i),chans(2,i)
                     if (flags(j).neqv.(.not.flagval(i))) then
                        flags(j)=.not.flagval(i)
                        if (flags(j)) then
                           flags_badtogood=flags_badtogood+1
                        else
                           flags_goodtobad=flags_goodtobad+1
                        endif
                     endif
                  enddo
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
            tprev=t
         enddo
         if (selgen) then
            call txtclose(lf)
            call txtclose(lu)
         endif
         if (genpatch) then
            call txtclose(lfp)
         endif
         if (genlog) then
            call txtclose(lfl)
         endif
c     summary of flagging
        outline='Counts of correlations within selected channels:'
        call output(outline)
        if (logstats) call hiswrite(tno,'PGFLAG: '//outline)
        write(outline,'( A8,''  Originally  Currently'')') 'channel'
        call output(outline)
        if (logstats) call hiswrite(tno,'PGFLAG: '//outline)
        write(outline,'( '' Good: '', 3X, I10, 1X, I10 )')
     *       oldflags_good,newflags_good
        write(outline(len1(outline)+1:),
     *   '(4x, ''Changed to bad: '', I10 )') flags_goodtobad
        call output(outline)
        if (logstats) call hiswrite(tno,'PGFLAG: '//outline)
        write(outline,'( '' Bad:  '', 3X, I10, 1X, I10 )')
     *     oldflags_bad,newflags_bad
        write(outline(len1(outline)+1:),
     *   '(4x, ''Changed to good:'', I10 )') flags_badtogood
        call output(outline)
        if (logstats) call hiswrite(tno,'PGFLAG: '//outline)
        write(outline,'(F5.1,''% of the data is now flagged'')')
     *    newflags_bad*100.0/(newflags_bad+newflags_good)
        call output(outline)
        if (logstats) call hiswrite(tno,'PGFLAG: '//outline)
        call hisclose(tno)
      endif

c
      end
c***********************************************************************
      subroutine LocateMax(iflag,nchan,ntime,valarray,chanoff,chanw,
     *     regwidth,makesel,points,day0,t1,maxval)
c
      integer nchan,ntime,chanoff,chanw,regwidth
      integer iflag(nchan,ntime,2),points(2,2)
      real valarray(nchan,ntime,2),t1(ntime),maxval
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
c    maxval    The maximum value found.
c-----------------------------------------------------------------------
      integer i,j,xloc,yloc
c      real maxval
      logical setval
      character maxstatus*256,maxtime*20
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
         call TimeToString(day0,t1(int(yloc)),1,maxtime)
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
c***********************************************************************
      subroutine ApplyFlags(iflag,xdim,ydim,chans,times,bases,
     *  flagval,MAXEDIT,nflags,bl,mant,some_unflagged)
c
      include 'maxdim.h'
      integer MAXEDIT,xdim,ydim,nflags,bl,mant
      integer iflag(xdim,ydim,2),chans(2,MAXEDIT)
      integer times(2,MAXEDIT)
      integer bases(2,MAXEDIT)
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
c    bases           The baselines to flag.
c    flagval         Whether to flag or unflag.
c    MAXEDIT         The maximum number of edits that can be held
c                    in the chans, times, bases and flagval arrays.
c    nflags          The number of flagging edits to apply.
c    bl              The currently displayed baseline ID.
c  Output:
c    some_unflagged  Status switch indicating whether there are
c                    unflagged visibilites on this baseline.
c-----------------------------------------------------------------------
      integer i,j,k,aflags,a(2,MAXBASE),bsln
      logical blselect
c
c     Initialise the flags
c
      do i=1,xdim
         do j=1,ydim
            iflag(i,j,2)=iflag(i,j,1)
         enddo
      enddo
      
      bsln = 0
      do j=1,mant
        do i=1,j
          bsln = bsln + 1
          a(1,bsln)=i
          a(2,bsln)=j
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
         blselect = (bases(2,i).eq.4).or.
     *      (bases(2,i).eq.1.and.bases(1,i).eq.bl).or.
     *      (bases(2,i).eq.2.and.((bases(1,i).eq.a(1,bl)).or.
     *        (bases(1,i).eq.a(2,bl)))).or.
     *      (bases(2,i).eq.3.and.((bases(1,i).eq.a(2,bl)).or.
     *        (bases(1,i).eq.a(1,bl))))
         if (blselect) then
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
c***********************************************************************
      subroutine FlagData(points,a1,a2,mode,flag,
     *  unflag,undo,chans,times,bases,flagval,MAXEDIT,
     *  nflags,day0,t1,ntime,chanoff,chanw,dprint)
c
      include 'maxdim.h'
      integer points(2,2),a1,a2,mode,MAXEDIT,ntime
      integer chans(2,MAXEDIT),chanoff,chanw
      integer times(2,MAXEDIT),nflags
      integer bases(2,MAXEDIT)
      real t1(ntime)
      logical flag,unflag,undo,flagval(MAXEDIT),dprint
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
c    bases    The baselines to flag.
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
c    dprint   If true, print out the flagging action to the terminal.
c  Output:
c    chans    The first and last channels to flag, updated to contain
c             the new flagging action.
c    times    The first and last times to flag, updated to contain
c             the new flagging action.
c    bases    The baselines to flag, updated to contain
c             the new flagging action.
c    flagval  Whether to flag or unflag, updated to contain the new
c             flagging action.
c    nflags   The number of flagging edits now held in the chans,
c             times, bases and flagval arrays.
c-----------------------------------------------------------------------
      integer minx,maxx,miny,maxy
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
            do while (t1(miny).lt.0.0)
               miny=miny+1
            enddo
            times(1,nflags)=miny
            do while (t1(maxy).lt.0.0)
               maxy=maxy-1
            enddo
            times(2,nflags)=maxy
            bases(2,nflags)=mode
            if (mode.eq.1) bases(1,nflags)=(a2*(a2-1))/2+a1
            if (mode.eq.2) bases(1,nflags)=a1
            if (mode.eq.3) bases(1,nflags)=a2
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
            time1=miny
            time2=maxy
            if (time1.gt.ntime) then
               time1=ntime
            endif
            if (time2.gt.ntime) then
               time2=ntime
            endif
            if (dprint) then
               call FmtCmd(flagstring,isave,t1(time1),t1(time2),
     *              chanoff,
     *              chanw,day0,selectline)
               call output(flagstring)
            endif
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
c***********************************************************************
      subroutine Thres(array,iflag,Nx,Ny,chans,times,bases,flagval,
     *                 MAXEDIT,nflags,cbl,t1,cliplev,nThresholds,
     *                 verbose)
      include 'maxdim.h'
c
      integer Nx, Ny, cbl, nFlags, MAXEDIT
      real array(Nx,Ny,2)
      real t1(Ny),cliplev
      integer iflag(Nx,Ny,2)
      integer nThresholds
      integer chans(2,MAXEDIT),times(2,MAXEDIT),bases(2,MAXEDIT)
      logical flagval(MAXEDIT),verbose
c
c  Apply a SumThreshold operation to the data.
c  If cliplev<0, just do a clip operation.
c
c-----------------------------------------------------------------------
      integer WIDTH, MAXX, MAXY
      parameter(WIDTH=20, MAXX=8192, MAXY=8640)
      real buf(MAXY*4),level,sum,mad
      integer N,step,wid, count
c
      integer i,j,k,l
c
c compile stats
      integer nclippix
c
c external
c
      integer prime

      nclippix = 0
c
c  Calculate median absolute deviation from zero
c
      mad = 0
      count = 0
      N = min(MAXY*3,Nx*Ny)
      step = (Nx*Ny)/N
      if (step.gt.3) step = prime(step)
      do k=0,Nx*Ny-1,step
        i=mod(k,Nx)+1
        j=k/Nx+1
        if (t1(j).gt.-1.and.iflag(i,j,2).gt.0) then
          count = count + 1
          buf(count) = abs(array(i,j,2))
        endif
      enddo
      if (count.gt.0) call median(buf,count,mad)
c
c  Apply the flagging operation to the iflag array, and recompute the
c  wedge values.
c  For normal distr.: mad*1.4826 = sigma
c  First apply SumThreshold in X direction
c
      do k=0,nThresholds
        wid = 2**k
        if (cliplev.gt.0) then
          level = cliplev*mad*1.4826/1.5**k
        else
          level=abs(cliplev)
        endif
        do j= 1, Ny
          if(t1(j).gt.-1)then
            do i=1, Nx-wid+1
              sum = 0
              count = 0
              do l=0,wid-1
                if(iflag(i+l,j,2).gt.0)then
                  sum = sum + array(i+l,j,2)
                  count = count + 1
                endif
              enddo
              if (count.gt.0) then
                if (sum/count.gt.level) then
                  if (nflags.lt.MAXEDIT)then
                    nclippix = nclippix + count
                    nflags=nflags+1
                    times(1,nflags)=j
                    times(2,nflags)=j
                    chans(1,nflags)=i
                    chans(2,nflags)=i+wid-1
                    bases(1,nflags)=cbl
                    bases(2,nflags)=1
                    flagval(nflags)=.true.
                    do l=0,wid-1
                      iflag(i+l,j,2) = 0
                    enddo
                  endif
                endif
              endif
            enddo
          endif
        enddo
      enddo
c
c Now do Y direction (skipping single point case)
c
      do k=1,nThresholds
        wid = 2**k
        level = cliplev*mad/1.5**k
        do j= 1, Ny-wid+1
          if(t1(j).gt.-1)then
            do i=1, Nx
              sum = 0
              count = 0
              do l=0,wid-1
                if(t1(j+l).gt.-1.and.iflag(i,j+l,2).gt.0)then
                  sum = sum + array(i,j+l,2)
                  count = count + 1
                endif
              enddo
              if (count.gt.0) then
                if (sum/count.gt.level) then
                  if (nflags.lt.MAXEDIT)then
                    nclippix = nclippix + count
                    nflags=nflags+1
                    times(1,nflags)=j
                    times(2,nflags)=j+wid-1
                    chans(1,nflags)=i
                    chans(2,nflags)=i
                    bases(1,nflags)=cbl
                    bases(2,nflags)=1
                    flagval(nflags)=.true.
                    do l=0,wid-1
                      if (t1(j+l).gt.-1) then
                        iflag(i,j+l,2) = 0
                      endif
                    enddo
                  endif
                endif
              endif
            enddo
          endif
        enddo
      enddo
      
      call stats(iflag,Nx,Ny,t1,nflags, MAXEDIT,verbose)

      end
      
c***********************************************************************
      subroutine Stats(iflag,Nx,Ny,t1,nflags,MAXEDIT,verbose)
      include 'maxdim.h'
c
      integer Nx, Ny, nflags, MAXEDIT
      real t1(Ny)
      integer iflag(Nx,Ny,2)
      logical verbose
c
c  Display baseline flagging stats
c
c-----------------------------------------------------------------------
      integer count,tot
c
      integer i,j
      character string*80
c
c  Show the flag statistics
c
      count = 0
      tot =0
      do j=1, Ny
        if (t1(j).gt.-1) then
          do i=1, Nx
            if(iflag(i,j,2).eq.0) count = count + 1
            tot = tot + 1
          enddo
        endif
      enddo
      if (tot.gt.0) then
        write(string,'(F5.1,A)')  (100.0*count)/tot,
     *         '% of the data on this baseline is now flagged'
        if (verbose) call output(string)
      endif
      if (nflags.eq.MAXEDIT) then
        call output('Flagging exceeded max #flags')
      endif
c
      end
c***********************************************************************
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
c***********************************************************************
      subroutine PlotSpectrum(iflag,array,nchan,ntime,points,day0,t1,
     *     chanoff,chanw,cfreq,meastype,yaxis)
c
      integer nchan,ntime,chanoff,chanw,meastype,points(2,2)
      real t1(ntime),array(nchan,ntime,2)
      integer iflag(nchan,ntime,2)
      double precision day0,cfreq(nchan)
      character yaxis*(*)
c
c  Make a spectral plot using the data in the currently selected region.
c
c  Input:
c    iflag           Array of flags for the currently displayed
c                    baseline.
c    array           The real values of the data (either amplitude
c                    or phase) for the currently displayed baseline.
c    nchan           The number of channels present in the data.
c    ntime           The number of time samples present in the data.
c    points          The bounding points of the selection region.
c    day0            The Julian date at midnight UT on the day of the
c                    first time sample.
c    t1              Array of times.
c    chanoff         The channel offset as specified by the line
c                    parameter.
c    chanw           The channel width as specified by the line
c                    parameter.
c    cfreq           Array that specifies each channel's centre
c                    frequency.
c    meastype        Whether to display the original value (set to 0)
c                    or the currently displayed value (set to 1)
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real spectrum(nchan),minval,maxval,channels(nchan)
      integer i,j,nspectrum(nchan),chanmin,chanmax,mintime,maxtime
      real minfreq,maxfreq
      character xtitle*60,ytitle*60,ptitle*60
      character time1*18,time2*18
c
c     check that we have a full selection box
      if ((points(1,1).eq.-1).or.(points(1,2).eq.-1).or.
     *    (points(2,1).eq.-1).or.(points(2,2).eq.-1)) then
         return
      endif
c     zero our spectrum
      do i=1,nchan
         spectrum(i)=0.0
         nspectrum(i)=0
         channels(i)=real(chanoff+i*chanw)
      enddo
c     build up the average spectrum
      chanmin=min(points(1,1),points(2,1))
      chanmax=max(points(1,1),points(2,1))
      mintime=min(points(1,2),points(2,2))
      maxtime=max(points(1,2),points(2,2))
      do i=chanmin,chanmax
         do j=mintime,maxtime
            if (iflag(i,j,2).ge.1) then
               if (meastype.eq.0) then
                  spectrum(i)=spectrum(i)+array(i,j,1)
                  nspectrum(i)=nspectrum(i)+1
               else
                  spectrum(i)=spectrum(i)+array(i,j,2)
                  nspectrum(i)=nspectrum(i)+1
               endif
            endif
         enddo
      enddo
      
      do i=chanmin,chanmax
         if (nspectrum(i).gt.0) then
            spectrum(i)=spectrum(i)/real(nspectrum(i))
         endif
         if (i.eq.chanmin) then
            minval=spectrum(i)
            maxval=spectrum(i)
         else
            minval=min(spectrum(i),minval)
            maxval=max(spectrum(i),maxval)
         endif
      enddo
c     make the plot now
      call pgeras()
c      write(status,'(A,I6,I6,F8.3,F8.3)') 'range:',chanmin,
c     *     chanmax,minval,maxval
c      call output(status)
      minfreq=real(cfreq(chanmin)*1000.0)
      maxfreq=real(cfreq(chanmax)*1000.0)
      call pgswin(minfreq,maxfreq,minval,maxval)
      call pgbox('CMTS',0.0,0,'',0.0,0)
      call pgswin(channels(chanmin),channels(chanmax),minval,maxval)
      call pgbox('BNTS',0.0,0,'BCNTS',0.0,0)
      xtitle='channel'
      if (yaxis(1:1).eq.'a') then
         ytitle='amplitude'
      elseif (yaxis(1:1).eq.'p') then
         ytitle='phase'
      endif
      call TimeToString(day0,t1(mintime),0,time1)
      call TimeToString(day0,t1(maxtime),0,time2)
      ptitle='Average '//time1//' to '//time2
      call pglab(xtitle,ytitle,ptitle)
      call pgline(nchan,channels,spectrum)
c
      end
c***********************************************************************
      subroutine TimeToString(day0,dtime,type,stime)
c
      double precision day0
      real dtime
      character stime*(*)
      integer type
c
c  Convert a Miriad time into a string representation.
c
c  Input:
c    day0            The Julian date at midnight UT on the day of the
c                    first time sample.
c    dtime           The real representation of the time.
c    type            The format of the string to return
c                    0 = yymmmdd:HH:MM:SS.S
c                    1 = ddmmmyyyy-HH:MM:SS.S
c  Output:
c    stime           A string representation of the time given in dtime.
c-----------------------------------------------------------------------
      integer startday,day,month,year,ierr,hour,minute
      real onet,second
      character mon*3
c
      startday=int(day0+real(int(dtime)))+1
      call julian_to_date(startday,day,month,year,ierr)
      if (type.eq.0) then
         year=year-1900
         if (year.gt.100) then
            year=year-100
         endif
      endif
      call monthstring(month,mon)
      onet=dtime
      do while (onet.ge.1.0)
         onet=onet-1.0
      enddo
      onet=onet*24.0
      hour=int(onet)
      onet=(onet-real(hour))*60.0
      minute=int(onet)
      onet=(onet-real(minute))*60.0
      second=onet
      if (type.eq.0) then
         write(stime,'(I2.2,A3,I2.2,A1,I2.2,A1,I2.2,A1,F4.1)')
     *        year,mon,day,':',hour,':',minute,':',second
         if (stime(15:15).eq.' ') stime(15:15)='0'
      elseif (type.eq.1) then
         write(stime,'(I2.2,A3,I4.4,A1,I2.2,A1,I2.2,A1,F4.1)')
     *        day,mon,year,'-',hour,':',minute,':',second
         if (stime(17:17).eq.' ') stime(17:17)='0'
      endif
c
      end
c***********************************************************************
      subroutine MakeMeasurement(iflag,array,nchan,ntime,curs_x,curs_y,
     *  day0,t1,meas_channel,meas_frequency,meas_time,meas_amplitude,
     *  chanoff,chanw,cfreq,meastype)
c
      integer nchan,ntime,chanoff,chanw,meastype
      real curs_x,curs_y
      real t1(ntime),array(nchan,ntime,2),zerot
      integer iflag(nchan,ntime,2)
      double precision day0,cfreq(nchan),zeroday
      integer meas_channel
      real meas_frequency,meas_amplitude
      character meas_time*(*)
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
      zeroday=day0
      zerot=t1(int(curs_y))
      do while (zerot.ge.1.0)
         zeroday=zeroday+1
         zerot=zerot-1
      enddo
      if ((zerot.ge.0.0).and.(zerot.le.1.0)) then
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
         call TimeToString(day0,t1(int(curs_y)),1,meas_time)
      endif
c
      end
c***********************************************************************
      subroutine MakePlot(iflag,array,t1,nchan,ntime,bl,curr_zooms,
     *  meas_channel,meas_frequency,meas_time,meas_amplitude,plot_top,
     *  plot_averages,plot_main,points,plot_points,yaxis,chanoff,chanw,
     *  subavgc,subavgt,subbkgnd,minval,maxval,fiddle_min,fiddle_max,
     *  use_fiddle,colour_window,colour_region,fiddle_active,flagpar)
c
      integer nchan,ntime,bl,chanoff,chanw
      integer iflag(nchan,ntime,2)
      real array(nchan,ntime,2),t1(ntime)
      integer curr_zooms(2,2),points(2,2)
      integer meas_channel
      real meas_frequency,meas_amplitude,minval,maxval
      real fiddle_min,fiddle_max,flagpar(5)
      character meas_time*(*),yaxis*(*)
      logical plot_top,plot_averages,plot_main,plot_points
      logical subavgc,subavgt,subbkgnd,use_fiddle,colour_window
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
c    subbkgnd        Switch to indicate whether the convolved
c                    background should be subtracted from the data.
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
      integer i,j,outscale
      logical acc,erasetop,setlims
      real curr_xpos,biggest_xworld,column_width,check_width,tmp
      real curr_ypos,tr(6)
      integer len1,stringlength,tbl
c
c     Setup the PGPLOT view area
c
      call pgqvsz(3,vp_xmin,vp_xmax,vp_ymin,vp_ymax)
      outscale=1
      if (vp_xmax.gt.5000) then
         outscale=10
      endif
      xleft=real(LEFT_MARGIN_PIXELS*outscale)/vp_xmax
      xright=1.0-real(RIGHT_MARGIN_PIXELS*outscale)/vp_xmax
      ybot=real(BOTTOM_MARGIN_PIXELS*outscale)/vp_ymax
      ytop=1.0-real((TOP_MARGIN_PIXELS+Y_BUFFER_PIXELS)*outscale)/
     *  vp_ymax
      xbuffer=real(X_BUFFER_PIXELS*outscale)/vp_xmax
      ybuffer=real(Y_BUFFER_PIXELS*outscale)/vp_ymax
      arrowbox_frac_x=real(ARROWBOX_WIDTH_PIXELS*outscale)/vp_xmax
      arrowbox_frac_y=real(ARROWBOX_WIDTH_PIXELS*outscale)/vp_ymax
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
      if (ychrhgt.gt.100) then
         ychrhgt = ychrhgt / 16.
      endif
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
               goto 10
            endif
         enddo
      enddo
 10   titles(2,1)=meas_baseline
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
               if (biggest_xworld .gt.
     *             (real(TOPBOX_WORLD_XRANGE)
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
            chrhgt_fraction=(real(TOPBOX_WORLD_XRANGE)-
     *        xcolumnbuffer)/
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
     *  tr,titles,curr_zooms,array,t1,n_columns,n_toplines,
     *  TOPBOX_WORLD_XRANGE,TOPBOX_WORLD_YRANGE,plot_top,plot_averages,
     *  plot_main,plot_points,points,iflag,chanoff,chanw,subavgc,
     *  subavgt,subbkgnd,fiddle_min,fiddle_max,use_fiddle,colour_window,
     *  colour_region,xcolumnbuffer,ylinespacing,fiddle_active,flagpar)
c
      end
c***********************************************************************
      subroutine paint_plot(xleft,xright,ybot,ytop,ybuffer,xbuffer,
     *  xdim,ydim,minval,maxval,arrowbox_frac_x,arrowbox_frac_y,
     *  colpos,tr,titles,curr_zooms,valarray,t1,n_columns,n_toplines,
     *  TOPBOX_WORLD_XRANGE,TOPBOX_WORLD_YRANGE,plot_top,plot_averages,
     *  plot_main,plot_points,points,iflag,chanoff,chanw,subavgc,
     *  subavgt,subbkgnd,fiddle_min,fiddle_max,use_fiddle,colour_window,
     *  colour_region,xcolumnbuffer,ylinespacing,fiddle_active,
     *  flagpar)
c
      real xleft,xright,ybot,ytop,ybuffer,xbuffer
      integer xdim,ydim,n_columns,n_toplines,chanoff,chanw
      integer iflag(xdim,ydim,2)
      real minval,maxval,arrowbox_frac_x,arrowbox_frac_y
      real colpos(n_columns,n_toplines,5),tr(6)
      character titles(n_columns,n_toplines)*256
      integer curr_zooms(2,2)
      real TOPBOX_WORLD_XRANGE,TOPBOX_WORLD_YRANGE
      real valarray(xdim,ydim,2),t1(ydim),fiddle_min,fiddle_max
      real xcolumnbuffer,ylinespacing,flagpar(5)
      logical plot_top,plot_averages,plot_main,plot_points
      logical subavgc,subavgt,subbkgnd,use_fiddle,colour_window
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
c    subbkgnd             Switch to indicate whether the convolved
c                         background should be subtracted from the data.
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
      call mask_and_range(iflag,valarray,t1,xdim,ydim,subavgc,subavgt,
     *     subbkgnd,minval,maxval,fiddle_min,fiddle_max,use_fiddle,
     *     curr_zooms,colour_window,xaverage,yaverage,
     *     average_box_width,points,colour_region,flagpar)
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
         if ((subavgc).or.(subavgt).or.subbkgnd) then
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
         if ((subavgc).or.(subavgt).or.subbkgnd) then
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
c***********************************************************************
      subroutine draw_waterfall(valarray,xdim,ydim,curr_zooms,maxval,
     *  minval,tr)
c
      integer xdim,ydim,curr_zooms(2,2)
      real maxval,minval,tr(6)
      real valarray(xdim,ydim,2)
c      character status*60
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
c      write(status,'(A,I6,I6,I6,I6)') 'zooms',curr_zooms(1,1),
c     *     curr_zooms(1,2),curr_zooms(2,1),curr_zooms(2,2)
c      call output(status)
      call pggray(valarray(1,1,2),xdim,ydim,curr_zooms(1,1),
     *     curr_zooms(1,2),curr_zooms(2,1),curr_zooms(2,2),maxval,
     *     minval,tr)
c
      end
c***********************************************************************
      subroutine mask_and_range(iflag,valarray,t1,xdim,ydim,subavgc,
     *     subavgt,subbkgnd,minval,maxval,fiddle_min,fiddle_max,
     *     use_fiddle,curr_zooms,colour_window,xaverage,yaverage,abw,
     *     points,colour_region,flagpar)
c
      integer xdim,ydim,curr_zooms(2,2),abw
      real t1(ydim),minval,maxval,fiddle_min,fiddle_max
      logical subavgc,subavgt,subbkgnd
      logical use_fiddle,colour_window,colour_region
      real valarray(xdim,ydim,2)
      real xaverage(xdim,abw),yaverage(abw,ydim),flagpar(5)
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
c    flagpar        The flag parameters
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
      if (subbkgnd) then
        call subconvl(valarray,t1,xdim,ydim,iflag,flagpar(2),flagpar(3))
      else
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
      endif
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
c***********************************************************************
      subroutine subconvl(valarray,t1,xdim,ydim,iflag,sigx,sigy)
c
      integer xdim,ydim,iflag(xdim,ydim,2)
      real valarray(xdim,ydim,2),t1(ydim),sigx,sigy
c
c  Convolve array in channel and time directions and subtract
c  convolved array from original array
c
c  Input:
c    valarray	On input, it contains the original values.
c    xdim	Number of channels.
c    ydim	Number of time slots.
c    iflag	Flags for the data.
c    sigx,sigy  The width of the gaussian convolution in pixels 
c               for channels and time slots respectively
c  Output:
c    valarray	The input minus the input convolved in time and 
c		channel. Flagged data is skipped over and time gaps
c               are not convolved across.
c
c-----------------------------------------------------------------------
c 
      include 'maxdim.h'
      integer maxk
      parameter (maxk=99)
      real acc,wtsum, wt, c(MAXDIM),kernel(0:maxk),maxwt
      integer cstart(MAXDIM),cend(MAXDIM)
      integer i,j,i0,j0,jw,iw
c
      if(ydim.gt.MAXDIM)call bug('f','Too many times')
c
c  Determine the ranges to average over for each time slot.
c
      call AvRange(ydim,t1,cstart,cend)
        
c
c  Generate the convolution over time
c
      if (sigy.gt.0) then
        iw = nint(3*sigy)
        if (iw.gt.maxk) call bug('f','Time conv. kernel too large')
        maxwt=0
        do i=0,iw
          kernel(i)=exp((-0.5*i*i)/sigy/sigy)
          maxwt=maxwt+kernel(i)
        enddo
        maxwt=maxwt*2-1
	do j=1,xdim
	  do i=1,ydim
            if (iflag(j,i,2).gt.0) then
              acc = 0
              wtsum = 0
	      if(cstart(i).le.cend(i))then
	        do i0=max(cstart(i),i-iw),min(cend(i),i+iw)
		  if(iflag(j,i0,2).gt.0)then
                    wt = kernel(abs(i-i0))
		    acc = acc + valarray(j,i0,1) * wt
                    wtsum = wtsum + wt
		  endif
	        enddo
	      endif
c
c  Save the result. Note we require a reasonable weight for the
c  convolved value (a few isolated points will be zeroed instead)
c
              if (wtsum-1.gt.maxwt/4) then
                valarray(j,i,2) = acc/wtsum
              else 
                valarray(j,i,2) = 0
              endif
            else
              valarray(j,i,2) = 0
            endif
	  enddo
	enddo
      else
        do j=1,xdim
          do i=1,ydim
            if (iflag(j,i,2).gt.0) then
              valarray(j,i,2) = valarray(j,i,1)
            else
              valarray(j,i,2) = 0
            endif
          enddo
        enddo
      endif
c
c  Generate the convolution over channel
c
      if (sigx.gt.0) then
        jw = nint(3*sigx)
        if (jw.gt.maxk) call bug('f','Channel conv. kernel too large')
        maxwt=0
        do j=0,jw
          kernel(j)=exp((-0.5*j*j)/sigx/sigx)
          maxwt = maxwt + kernel(j)
        enddo
        maxwt = 2*maxwt-1
	do i=1,ydim
	  do j=1,xdim
            if (iflag(j,i,2).gt.0) then
              acc = 0
              wtsum = 0
	      do j0=max(1,j-jw),min(xdim,j+jw)
                if(iflag(j0,i,2).gt.0)then
                  wt = kernel(abs(j-j0))
	          acc = acc + valarray(j0,i,2) * wt
                  wtsum = wtsum + wt
                endif
	      enddo
              if (wtsum-1.gt.maxwt/4) then
                c(j) = acc/wtsum
              else 
                c(j) = 0
              endif
            endif
	  enddo
          do j=1,xdim
            if (iflag(j,i,2).gt.0) then
              valarray(j,i,2)=valarray(j,i,1)-c(j)
            else
              valarray(j,i,2)=0
            endif
          enddo
	enddo
      else
        do i=1,ydim
          do j=1,xdim
            if (iflag(j,i,2).gt.0) then
              valarray(j,i,2) = valarray(j,i,1) - valarray(j,i,2)
            else
              valarray(j,i,2) = 0
            endif  
          enddo
        enddo
      endif
c
            
c      
      end
c***********************************************************************
      subroutine dust(iflag,t1,xdim,ydim,minN,
     *  chans,times,bases,flagval,MAXEDIT,nflags,cbl,verbose)
c
      integer xdim,ydim,iflag(xdim,ydim,2),minN
      integer cbl, nFlags, MAXEDIT
      real t1(ydim)
      integer chans(2,MAXEDIT),times(2,MAXEDIT),bases(2,MAXEDIT)
      logical flagval(MAXEDIT),verbose
c
c  Dust the array - flag all pixels with less than minN neighbours 
c
c  Input:
c    valarray	On input, it contains the original values.
c    xdim	Number of channels.
c    ydim	Number of time slots.
c    iflag	Flags for the data.
c    minN       Minimum number of unflagged neighbours
c    chans,times,bases Channels, Times and Baselines to flag
c    flagval    Flag value for each edit operation
c    MAXEDIT,nflags Max and current number of flags
c    cbl        Current baseline number
c  Output:
c    valarray	The input with 'dust' flagged out
c
c-----------------------------------------------------------------------
c 
      include 'maxdim.h'
      integer cstart(MAXDIM),cend(MAXDIM)
      integer i,j,i0,j0,n,lastnflags,niter
c
      if(ydim.gt.MAXDIM)call bug('f','Too many times')
c
c  Determine the valid ranges for each time slot.
c
      call AvRange(ydim,t1,cstart,cend)
        
c
c  Blow away the dust
c
      lastnflags=-1
      niter=0
      do while (nflags.gt.lastnflags.and.niter.lt.5)
        niter=niter+1
        lastnflags=nflags
        do j=1,xdim
	  do i=1,ydim
	    if(cstart(i).le.cend(i))then
              if (iflag(j,i,2).gt.0) then
                n = 9
	        do j0=max(1,j-1),min(xdim,j+1)
	          do i0=max(cstart(i),i-1),min(cend(i),i+1)
		    if(iflag(j0,i0,2).le.0) n=n-1
                  enddo
                enddo
                if (n.le.minN.and.nflags.lt.MAXEDIT) then
                  iflag(j,i,2)=0
                  nflags = nflags + 1
                  times(1,nflags)=i
                  times(2,nflags)=i
                  chans(1,nflags)=j
                  chans(2,nflags)=j
                  bases(1,nflags)=cbl
                  bases(2,nflags)=1
                  flagval(nflags)=.true.
                endif
              endif
            endif
	  enddo
        enddo
      enddo
      call stats(iflag,xdim,ydim,t1,nflags,MAXEDIT,verbose)
      end
c***********************************************************************
      subroutine expand(iflag,t1,xdim,ydim,frac,
     *  chans,times,bases,flagval,MAXEDIT,nflags,cbl,verbose)
c
      integer xdim,ydim,iflag(xdim,ydim,2)
      integer cbl, nFlags, MAXEDIT
      real t1(ydim),frac
      integer chans(2,MAXEDIT),times(2,MAXEDIT),bases(2,MAXEDIT)
      logical flagval(MAXEDIT),verbose
c
c  Expand the flagged areas by applying the Scale Invariant Rank operator
c  to the array - flag pixels in all ranges where the 
c  fraction of good pixels is less than frac, Applied first in time,
c  then in channel. Exclude fully flagged times and channels from the
c  algorithm to avoid 'eating away' too much good data. 
c
c  Input:
c    iflag	Flags for the data.
c    xdim	Number of channels.
c    ydim	Number of time slots.
c    frac       Minimum fracion of unflagged pixels in range
c    chans,times,bases Channels, Times and Baselines to flag
c    flagval    Flag value for each edit operation
c    MAXEDIT,nflags Max and current number of flags
c    cbl        Current baseline number
c  Output:
c    iflag	The input with bad areas flagged out
c
c-----------------------------------------------------------------------
c 
      include 'maxdim.h'
      integer p(MAXDIM),q(MAXDIM)
      integer inx(MAXDIM),iny(MAXDIM)
      real psi(MAXDIM),m(MAXDIM)
      integer ix,iy,nx,ny
      logical good
c
      if(ydim.gt.MAXDIM)call bug('f','Too many times')
c
c  Exclude time gaps and completely flagged times
c
      ny=0
      do iy=1,ydim
        if (t1(iy).gt.0) then
          good = .false.
          ix=1
          do while (.not.good.and.ix.le.xdim)
            good=good.or.(iflag(ix,iy,2).gt.0)
            ix=ix+1
          enddo
          if (good) then
            ny=ny+1
            iny(ny)=iy
          endif
        endif
      enddo
        
c
c  Determine what to flag, use Offringa's linear time SIR algorithm
c
c  Do the time direction
c     
      do ix=1,xdim
        do iy=1,ny
          psi(iy) = frac - iflag(ix,iny(iy),2)
        enddo
        m(1)=0
        do iy=2,ny+1
          m(iy) = m(iy-1) + psi(iy-1)
        enddo
        p(1)=1
        do iy=2,ny
          p(iy)=p(iy-1)
          if (m(p(iy)).gt.m(iy)) p(iy)=iy
        enddo
        q(ny)=ny+1
        do iy=ny-1,1,-1
          q(iy)=q(iy+1)
          if (m(q(iy)).lt.m(iy+1)) q(iy)=iy+1
        enddo
        do iy=1,ny
          if (iflag(ix,iny(iy),2).gt.0.and.m(q(iy)).ge.m(p(iy))
     *        .and.nflags.lt.MAXEDIT) then
            iflag(ix,iny(iy),2)=0
            nflags = nflags + 1
            times(1,nflags)=iny(iy)
            times(2,nflags)=iny(iy)
            chans(1,nflags)=ix
            chans(2,nflags)=ix
            bases(1,nflags)=cbl
            bases(2,nflags)=1
            flagval(nflags)=.true.
          endif
        enddo
      enddo
      
c
c  Exclude completely flagged channels
c
      nx=0
      do ix=1,xdim
          good = .false.
          iy=1
          do while (.not.good.and.iy.le.ny)
            good=good.or.(iflag(ix,iny(iy),2).gt.0)
            iy=iy+1
          enddo
          if (good) then
            nx=nx+1
            inx(nx)=ix
          endif
      enddo
c
c  Do the channel direction
c     
      do iy=1,ny
        do ix=1,nx
          psi(ix) = frac - iflag(inx(ix),iny(iy),2)
        enddo
        m(1)=0
        do ix=2,nx+1
          m(ix) = m(ix-1) + psi(ix-1)
        enddo
        p(1)=1
        do ix=2,nx
          p(ix)=p(ix-1)
          if (m(p(ix)).gt.m(ix)) p(ix)=ix
        enddo
        q(nx)=nx+1
        do ix=nx-1,1,-1
          q(ix)=q(ix+1)
          if (m(q(ix)).lt.m(ix+1)) q(ix)=ix+1
        enddo
        do ix=1,nx
          if (iflag(inx(ix),iny(iy),2).gt.0.and.
     *        m(q(ix)).ge.m(p(ix)).and.nflags.lt.MAXEDIT) then
            iflag(inx(ix),iny(iy),2)=0
            nflags = nflags + 1
            times(1,nflags)=iny(iy)
            times(2,nflags)=iny(iy)
            chans(1,nflags)=inx(ix)
            chans(2,nflags)=inx(ix)
            bases(1,nflags)=cbl
            bases(2,nflags)=1
            flagval(nflags)=.true.
          endif
        enddo
      enddo
      call stats(iflag,xdim,ydim,t1,nflags,MAXEDIT,verbose)
      
      end
c***********************************************************************
	subroutine AvRange(Ny,t1,cstart,cend)
c
	integer Ny
	real t1(Ny)
	integer cstart(Ny),cend(Ny)
c
c  Determine the range of time slots for each source
c
c  Input:
c    t1	Time of each time slot.
c    Ny	Number of time slots.
c  Output:
c    cstart	First time slot to use
c    cend	Last time slot to use
c-----------------------------------------------------------------------
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
	    pend = max(pstart,pend)
	    more = .true.
	    do while(more)
	      more = pend+1.le.Ny
	      if(more) more = t1(pend+1).gt.-1
	      if(more) pend = pend + 1
	    enddo
	  endif
	  cstart(i) = pstart
	  cend(i) = pend
	enddo
c
	end
c***********************************************************************
      subroutine set_plot_main(xleft,xright,ybot,ytop,curr_zooms)
c
      real xleft,xright,ybot,ytop
      integer curr_zooms(2,2)
c
      call pgsvp(xleft,xright,ybot,ytop)
      call pgswin(real(curr_zooms(1,1)),real(curr_zooms(1,2)),
     *  real(curr_zooms(2,1)),real(curr_zooms(2,2)))
c
      end
c***********************************************************************
      subroutine compute_average_spectrum(valarray,xdim,ydim,
     *  xaverage,yaverage,average_box_width,iflag,dxaverage,
     *  dyaverage)
c
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
c***********************************************************************
      subroutine set_plot_top_info(ytop,ybuffer,xrange,yrange,erase)
c
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
c***********************************************************************
      subroutine Gridit(iflag,array,nchan,ntime,rqbl,day0,
     *                  lScr,nvis,t1,some_unflagged)
c
      integer nchan,ntime,lScr,nvis,rqbl
      logical some_unflagged
      integer iflag(nchan,ntime,2)
      real t1(ntime),array(nchan,ntime,2)
      double precision day0
      include 'maxdim.h'
      integer i,j,k,length,pnt,bl,i0
      ptrdiff offset
      real buf(2*MAXCHAN+3),t
c      character status*80

      some_unflagged=.false.
      do j=1,ntime
         do i=1,nchan
            iflag(i,j,1)=0
            iflag(i,j,2)=0
            array(i,j,1)=0
            array(i,j,2)=0
         enddo
      enddo
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
                  if (pnt.le.ntime-1) then
                     if ((t1(pnt).le.t).and.(t.lt.t1(pnt+1))) goto 10
                     if (pnt.le.ntime-2) then
                        if ((t1(pnt+1).eq.-2).and.
     *                      (t.lt.t1(pnt+2))) goto 10
                     endif
                  else
                     if(t1(pnt).le.t) goto 10
                  endif
               endif
            enddo
c            write(status,'(A,F20.10)') 'Time slot miscalculation',t
            call output('Time slot miscalculation')
c            call output(status)
            goto 11

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
 11         i0=3
         endif
      enddo
c
      end
c***********************************************************************
      subroutine GetAxis(xaxis,yaxis)
c
      character xaxis*(*),yaxis*(*)
c-----------------------------------------------------------------------
      integer NAX
      parameter(NAX=2)
      integer n
      character axes(3)*12
      data axes/'amplitude   ','phase       ',
     *          'time        '/
c      call keymatch('axis',NAX,axes,1,xaxis,n)
      xaxis = axes(3)
      call keymatch('mode',NAX,axes,1,yaxis,n)
      if(n.eq.0)yaxis = axes(1)
      end
c***********************************************************************
      subroutine GetOpt(selgen,noapply,nosrc,nodisp,genpatch,uvflags)
c
      logical selgen,noapply,nosrc,nodisp,genpatch
      character uvflags*(*)
c
c  Get extra processing options.
c-----------------------------------------------------------------------
      integer NOPTS
      parameter(NOPTS=8)
      logical present(NOPTS)
      character opts(NOPTS)*8
      data opts/'nocal   ','nopass  ','nopol   ',
     *          'selgen  ','noapply ','nosrc   ',
     *          'nodisp  ','patch   '/
c
      call options('options',opts,present,NOPTS)
c
      selgen = present(4)
      noapply= present(5)
      nosrc  = present(6)
      nodisp = present(7)
      genpatch = present(8)
c      logstats = present(8).or.nodisp
      uvflags = 'sdlwb'
      if(.not.present(1))uvflags(6:6) = 'c'
      if(.not.present(2))uvflags(7:7) = 'f'
      if(.not.present(3))uvflags(8:8) = 'e'
      end
c***********************************************************************
      subroutine flagchk(tno)
c
      integer tno
c
c  Check that the user's linetype is not going to cause the flagging
c  routine to vomit when the flagging is applied.
c
c-----------------------------------------------------------------------
      integer CHANNEL,WIDE
      parameter(CHANNEL=1,WIDE=2)
      double precision line(6)
c
      call uvinfo(tno,'line',line)
      if(nint(line(1)).ne.CHANNEL.and.nint(line(1)).ne.WIDE)
     *  call bug('f','Can only flag "channel" or "wide" linetypes')
      if(nint(line(4)).ne.1)
     *  call bug('f','Cannot flag when the linetype width is not 1')
c
      end
c***********************************************************************
      subroutine CopyDat(lIn,lScr,apri,nchan,time,MAXTIME,ntime,
     *  day0,ttol,blpres,nbase,nvis,chanoff,chanw,nosrc,mbl,mant)
c
      integer lIn,lScr,nchan,maxtime,ntime,nbase,nvis,chanoff,chanw
      integer mbl,mant
      character apri*1
      real time(maxtime),ttol
      double precision day0
      logical blpres(nbase),nosrc
c
c  Copy the data to a scratch file.
c
c  Input:
c    lIn        Handle of the input visibility dataset.
c    lScr       Handle of the output scratch file.
c    apri       Sort of quantity to display.
c    maxtime    Dimension of time array.
c    ttol       Time tolerance.
c    nbase      Maximum number of baselines.
c    nosrc      Do not cause a gap when the source changes.
c  Output:
c    blpres     Indicates whether a particular baseline is present.
c    day0       Base time.
c    time       The time of each integration.
c    nvis       Number of visibilities.
c    ntime      Number of times.
c    nchan      Number of channels.
c    chanoff    Offset to add to channel numbers to get true channel
c               numbers.
c    chanw      Width of channel specified in linetype
c    mbl        Max baseline number
c    mant       Max antenna number
c-----------------------------------------------------------------------
      include 'maxdim.h'
      logical flags(MAXCHAN),newsrc
      complex data(MAXCHAN)
      double precision preamble(4),line(6),day1
      real buf(2*MAXCHAN+3),t,tprev,maxgap
      logical torder
      integer vsrc,nread,length,ant1,ant2,i,bl,i0
      ptrdiff offset
c
c  Externals.
c
      logical uvvarupd
      real ctoapri
c      character status*60
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
c      call uvread(lIn,preamble,data,flags,MAXCHAN,nchan)
      call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
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
      mant=0
      mbl=0
      dowhile(nread.eq.nchan)
        call basant(preamble(4),ant1,ant2)
        mant=max(max(mant,ant1),ant2)
        bl = ((ant2-1)*ant2)/2 + ant1
        mbl=max(mbl,bl)
        t = preamble(3) - day0
        if(t.lt.0)then
          day1 = nint(preamble(3)-1) + 0.5d0
          do i=1,ntime
c             write(status,'(A,F20.10)') 'timebad',time(i)
c             call output(status)
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
     *        (newsrc.or.t-tprev.gt.maxgap.or.t.lt.tprev))then
              if(ntime.ge.MAXTIME)
     *            call bug('f','Too many times for me')
              ntime = ntime + 1
              time(ntime) = -2
            endif
            if(ntime.ge.MAXTIME)call bug('f','Too many times for me')
            ntime = ntime + 1
            time(ntime) = t
c            write(status,'(A,I10,F20.10)') 'time',ntime,time(ntime)
c            call output(status)
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
c       call uvread(lIn,preamble,data,flags,MAXCHAN,nread)
        call uvDatRd(preamble,data,flags,MAXCHAN,nread)
      enddo
c
      if(.not.torder)
     *    call bug('w','Display will not be in strict time order')
      if(nread.ne.0)call bug('f',
     *  'Number of channels changed while reading the data')
      if(ntime.eq.0)call bug('f','No data found')
      if(time(ntime).lt.-1) ntime = ntime - 1
      if(ntime.eq.0)call bug('f','No data found')
c
      nvis = offset / length
c
      end
c***********************************************************************
      real function ctoapri(data, apri)
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
c    string     The formatted command.
c    selectline The formatted command in select keyword format
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxdim.h'
      integer chan1,chan2,l,i,j,tbl,ls
      character time1*18,time2*18
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
     *                 i,'-',j
                  write(selectant,'(A,I3,A,I3,A)') 'ant(',i,')(',
     *                 j,')'
                  goto 10
               endif
            enddo
         enddo
      elseif ((isave(1).eq.2).or.(isave(1).eq.3)) then
         write(baseflag,'(A,I3)') 'all baselines with antenna',
     *        isave(2)
         write(selectant,'(A,I3,A)') 'ant(',isave(2),')'
      elseif (isave(1).eq.4) then
         baseflag='all baselines'
         selectant=' '
      endif
 10   if (isave(5).eq.1) then
         flagval='GOOD'
      else
         flagval='BAD'
      endif
      call TimeToString(day0,t1,0,time1)
      call TimeToString(day0,t2,0,time2)
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
     *  ', channels '//itoaf(chan1)
      l=len1(string)
      string(l+1:)=' to '//itoaf(chan2)
      l=len1(string)
      string(l+1:)=' on '//baseflag
      l=len1(string)
      string(l+1:)=' to '//flagval
c
      end
c***********************************************************************
      subroutine FmtPatch(string,isave,t1,t2,chanoff,chanw,day0,logform)
c
      character string*(*)
      integer isave(5),chanoff,chanw
      real t1,t2
      double precision day0
      logical logform
c
c  Format an editting instruction as a line in a patch file.
c
c  A patch file line will look like:
c  bsln time1 time2 chan1 chan2 flag
c
c  Here, bsln is the baseline selection specifier:
c   1-5 would be baseline formed by antenna 1 and 5
c   1*  would be all baselines formed with antenna 1 as the
c       first antenna
c   *   would be all baselines present
c  time1 is the first time to flag, time2 is the last
c  chan1 is the first channel to flag, chan2 is the last
c  flag is the way to flag, BAD or GOOD
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
c     logform   format for text log output
c  Output:
c    string     The formatted command.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxdim.h'
      integer chan1,chan2,l,i,j,tbl,base1,base2
      character time1*18,time2*18
c      double precision time1,time2
      character flagval*4,baseflag*7
      double precision dt1,dt2
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
                  write(baseflag,'(I3,A,I3)') i,'-',j
                  base1=i
                  base2=j
                  goto 10
               endif
            enddo
         enddo
      elseif ((isave(1).eq.2).or.(isave(1).eq.3)) then
         write(baseflag,'(I3,A)') isave(2),'*'
         base1=isave(2)
         base2=0
      elseif (isave(1).eq.4) then
         baseflag='*'
         base1=0
         base2=0
      endif
 10   if (isave(5).eq.1) then
         flagval='GOOD'
      else
         flagval='BAD'
      endif
      if (logform) then
         call TimeToString(day0,t1,0,time1)
         call TimeToString(day0,t2,0,time2)
c      write(time1,'(F18.5)') real(day0+dbl(t1))
c      write(time2,'(F18.5)') real(day0+dbl(t2))
         chan1=chanoff+isave(3)*chanw
         chan2=chanoff+isave(4)*chanw
         string=''//baseflag//' '//time1//' '//time2//
     *        ' '//itoaf(chan1)
         l=len1(string)
         string(l+1:)=' '//itoaf(chan2)
         l=len1(string)
         string(l+1:)=' '//flagval
      else
         chan1=chanoff+isave(3)*chanw
         chan2=chanoff+isave(4)*chanw
         dt1=day0+t1
         dt2=day0+t2
         write(string,'(I4,I4,F18.5,F18.5,I7,I7,I2)') base1,base2,
     *        dt1,dt2,
     *        chan1,chan2,isave(5)
      endif
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
c***********************************************************************
	subroutine GetScale(array,iflag,Nx,Ny,bmin,bmax)
c
	integer Nx,Ny
	integer iflag(Nx,Ny,2)
	real array(Nx,Ny,2),bmin,bmax
c
c  Determine a useful display range for the good data.
c
c-----------------------------------------------------------------------
	integer i,j,k,step,N,count
	logical first
        real mad,buf(15000)
c
        integer prime
c
	bmin = 0
	bmax = 0
c
c
c  Calculate median absolute deviation from zero
c
        mad = 0
        count = 0
        N = min(10000,Nx*Ny)
        step = (Nx*Ny)/N
        if (step.gt.3) step = prime(step)
        do k=0,Nx*Ny-1,step
          i=mod(k,Nx)+1
          j=k/Nx+1
          if (iflag(i,j,2).gt.0) then
            count = count + 1
            buf(count) = abs(array(i,j,2))
          endif
        enddo
        if (count.gt.0) call median(buf,count,mad)

	first = .true.
	do j=1,Ny
	  do i=1,Nx
	    if(iflag(i,j,2).gt.0)then
	      if(first)then
		bmin = array(i,j,2)
		bmax = bmin
		first = .false.
	      else
		bmin = min(bmin,array(i,j,2))
		bmax = max(bmax,array(i,j,2))
	      endif
	    endif
	  enddo
	enddo
c
        bmin = max(bmin,-10*mad)
        bmax = min(bmax, 10*mad)
c
	end
c***********************************************************************
