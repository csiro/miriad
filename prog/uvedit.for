c***********************************************************************
c  Allow editing of UV data baseline parameters.
c
c  History:
c    jm    05dec90    Initial version.
c    jm    19dec90    Made each option into its own keyword to allow
c                     multiple corrections.  Also removed ``select''
c                     keyword as this option would not allow data that
c                     is not edited (selected) to be copied.
c    jm    11jan91    Added source keyword.
c   lgm    26jan91    Modified to handle wide band only data.
c    jm    22mar91    Increased error/internal message string lengths.
c   bpw    02apr91    Repaired the 22mar bug in writing of internal i/o
c    jm    30aug91    Fixed a bug with reading the data antpos;  the
c                     number of input Vis data sets was increased from
c                     10 to 50; and now reads the number of Antennas
c                     present in the data set and is used to limit the
c                     range of the antenna arrays.
c    jm    09oct91    Added delay option.  This also meant having to do
c                     a recalculation of the wide band data.
c   rjs    07sep92    Better check for nants being updated.
c  nebk    22sep92    Added option not to recompute u and v.
c    jm    25sep92    Confirmed lgm's change from wt**2 to abs(wt) and
c                     added some documentation to the last two changes.
c  nebk    19oct92    Better documentation and use of options=nouv
c    jm    21oct92    Modified documentation to get it up to date.
c    jm    12feb93    Modified ra/dec variables to double precision;
c                     reflects change made to uv structure.
c    jm    15apr93    Modified calls to get individual variables from
c                     uvgetvr* to uvrdvr* so that old data that have
c                     variables with different types (real/dble) will
c                     work properly.  This "fixes" the change (i.e.
c                     allows old data to be read) made on 12feb93.
c    jm    23apr93    Corrected an argument mismatch in the call
c                     to hreada() in the subroutine trackit().
c    jm    03jun93    Added apfile keyword.
c    rjs   11aug93    Rotation of uv coordinates to correct epoch.
c                     Also fixed continuation bug in getcoor.
c    rjs   19nov93    Output correlations are in the same format as the
c                     input.
c    jm    03dec93    Only call uvset for corr format if corr exists!
c   bpw/jm 17dec93    Added options=dra to correct for MINT problems.
c    rjs   16dec95    Make phase shift calculation more accurate.
c    jm    19feb96    Added warnings if no obvious changes are made or
c                     if the requested source is not found.
c    jm    06jun96    Correctly add dra/ddec to RA/Dec in Getrdphz.
c    rjs   11oct96    Added delra,deldec,pntra,pntdec to the output.
c    rjs   24mar97    Copy all calibration tables.
c    rjs   12may97    Doc change only.
c    rjs   14jul97    nspect etc was not getting read when the source
c		      selected was not the first source in the file.
c    rjs   08may00    Change incorrect call of keyf to keya.
c***********************************************************************
c= Uvedit - Editing of the baseline of a UV data set.
c& jm
c: calibration, uv-data
c+
      program uvedit
      implicit none
c
c     UVEDIT is a MIRIAD task which allows baseline editing of a UV
c     data set.  As a result of the editing, certain header variables
c     of the data set are changed.  The headers `corr' and `wcorr'
c     are always changed since they are the data themselves.  The
c     headers `coord(2)' are the baseline coordinates and, as a
c     result, are also always changed (except with the option nouv).
c     The UV variable headers `lst', `ut', and `time' are updated
c     whenever a time offset is entered.  The headers `ra', `dec',
c     `obsra', and `obsdec' are changed whenever a positional
c     correction is entered with the ra or dec input.  Finally,
c     antenna coordinate corrections will cause the header `antpos'
c     to be corrected.
c
c     NOTE: There can be NO select keyword for this routine!  If
c     one includes the select option, then data that is not selected
c     will not be copied!
c
c@ vis
c     The name of the input UV data set.  At least one file name must
c     be supplied.  Up to 50 visibility files are currently allowed.
c
c@ source
c     The name of the source to apply corrections to if more than
c     one source is present in a UV data set.  All UV data that does
c     not correspond to the input source name is copied without being
c     edited.  If this keyword is not set (the default), then all
c     sources are edited.  Only one source name may be input.
c     Note: it probably doesn't make sense to use this keyword with
c     any other options except the ra and dec keywords.  If any other
c     editing (except ra/dec) is requested along with this keyword,
c     a warning message will be issued but the editing will proceed.
c
c@ apfile
c     The name of a file that contains the absolute antenna positions
c     entered in increasing antenna order.  Only one antenna position
c     file is permitted.  The first line of the file is ALWAYS skipped
c     and the remaining lines MUST contain three entries corresponding
c     to the X, Y, and Z equatorial coordinates, respectively, in units
c     of nanoseconds.  The function of this keyword is identical to the
c     ``antpos'' keyword except that there is no way to specify a
c     subset of antennae; every antenna up to and including the largest
c     antenna number must be present in the file.
c     NOTE: You may only specify at most one of the ``apfile'',
c           ``antpos'', or ``dantpos'' keywords.
c
c@ antpos
c     Inputs are the absolute equatorial coordinates entered in the
c     following order (NO checking is done for consistency):
c          antpos = A1,X1,Y1,Z1,A2,X2,Y2,Z2,A3,X3,Y3,Z3,....
c     The input values are the antenna number and the three equatorial
c     coordinates (entered in units of nanoseconds).  Note that A1 does
c     not necesarily have to correspond to Antenna 1; it is used to 
c     represent the variable containing the antenna number.  Antenna
c     (and the corresponding coordinates) not included in the input
c     listing do not have their coordinates changed.
c     NOTE: An antenna position value of zero is not possible.  If
c     an antenna value is set to zero, the current value from the
c     data is used.  To force an antenna position value to be zero,
c     use the keyword ``dantpos''.
c     NOTE: You may only specify at most one of the ``apfile'',
c           ``antpos'', or ``dantpos'' keywords.
c
c@ dantpos
c     Inputs are the equatorial coordinate offsets entered in the
c     following order (NO checking is done for consistency):
c          dantpos = A1,X1,Y1,Z1,A2,X2,Y2,Z2,A3,X3,Y3,Z3,....
c     The input values are the antenna number and the three equatorial
c     coordinate offsets (entered in units of nanoseconds).  These input
c     values are added to the absolute coordinates read from the data.
c     Note that A1 does not necesarily have to correspond to Antenna 1;
c     it is used to represent the variable containing the antenna
c     number.  Antenna present in the data but not included in the
c     input value list are treated as having a zero coordinate offset.
c     NOTE: You may only specify at most one of the ``apfile'',
c           ``antpos'', or ``dantpos'' keywords.
c     NOTE: The dantpos keyword is the usually used when correcting antenna
c     position errors in a VLA observation. The  coordinate system used by
c     Miriad and the VLA are the same, and the baseline changes provided
c     by the VLAIS system need only be changed from units of meters to nanosec
c     when using uvedit (1 nanosec = 0.2997 meters).
c@ ra
c     Input is either an absolute or delta right ascension of the
c     phase tracking center.  If one value is present, it is considered
c     as a offset position and is to be entered as time seconds.
c     Otherwise, three values are expected and are to be entered in
c     the following order:
c          ra = HH,MM,SS.S
c     The right ascension (offset) is relative to the epoch coordinates.
c     The default value is 0 seconds offset (no change).
c
c@ dec
c     Input is either an absolute or delta declination of the phase
c     tracking center.  If only one value is present, it is considered
c     as a offset position and is to be entered in arcseconds.
c     Otherwise, three values are expected and are to be entered in
c     the following order:
c          dec = DD,MM,SS.S
c     The declination (offset) is relative to the epoch coordinates.
c     The default value is 0 arcseconds offset (no change).  If the
c     absolute declination is negative but the DD value is 0, then make
c     the MM value negative.  If MM is also 0, then make SS.S negative.
c
c@ time
c     Input is a time offset (in seconds) to be added to the clock time.
c     The default value is 0 seconds offset (no change).
c
c@ delay
c     Inputs are the delay error corrections for each antenna.
c     The inputs are the delay values for each antenna entered in the
c     following order (in units of nanoseconds):
c          delay = D1,D2,D3,....
c     If no value for delay is specified, the array of antenna values
c     are set to 0 nanoseconds offset.  Also, a resulting difference
c     (D[i]-D[j]) of less than 0.05 nanoseconds is ignored (no change).
c     If a difference exists, the digital wide band data will be
c     reconstructed.  All previously flagged narrow band data will
c     be ignored in the reconstruction.  Also, the two (2) end
c     channels of each window will be ignored in the reconstruction.
c
c@ out
c     The name of the output visibility file.  This parameter is
c     ignored when more than one visibility file is given.  If no value
c     for ``out'' is given or more than one visibility file is input,
c     then the output file name(s) will be the same as the input file
c     name(s) but with an "_c" appended to the file name
c     (ie. "Vis = saturn,jupiter" will result in output files "saturn_c"
c     and "jupiter_c").
c
c@ options
c     Task enrichment options.  Minimum match is active.
c       nouv     Do not recompute the u and v variables (coord(1) and
c                coord(2), respectively).  This option should, in
c                principle, only be used with the delay correction;
c                all other corrections should recompute u and v.
c       dra
c                Multiply the dra values by a cos(obsdec) correction.
c                This is used to correct the dra value in the uv
c                dataset for MINT data taken at Hat Creek before
c                11dec93.  Before that date, the 1/cos(obsdec)
c                correction was not applied to the dra in the grid
c                file, so that the pointing was incorrect (instead
c                of dra arcseconds offsets, the offsets were
c                dra*cos(obsdec) arcseconds).
c                NOTE:  The obsdec used is the "old" obsdec.  If there
c                is a correction in declination, this is NOT applied
c                in computing the cos(obsdec).
c
c--
c-----------------------------------------------------------------------
c
c  Internal parameters.
c
      include 'maxdim.h'
      include 'mirconst.h'
      character PROG*(*)
      parameter (PROG = 'UVEDIT: ')
      character VERSION*(*)
      parameter (VERSION = 'version 1.8 19-Feb-96')
c
      real SECRAD, ASECRAD
c  -------------(SECRAD = PI / (12.0 * 3600.0))
      parameter (SECRAD = PI / 43200.0)
c  -------------(ASECRAD = PI / (180.0 * 3600.0))
      parameter (ASECRAD = PI / 648000.0)
c
      character ENDING*(*)
      parameter (ENDING = '_c')
      integer MAXFILES
      parameter (MAXFILES = 50)
      integer ENDCHN
      parameter (ENDCHN = 2)
      real BANDWID
      parameter (BANDWID = 0.04)
c
c  Internal variables.
c
      character Vis(MAXFILES)*132, Outsave*132, Outfile*132
      character Selsrc*132, apFile*132, source*132
      character except(15)*11, type*1, corrtype*1
      character errmsg*175, mesg*80, antkey*7
      integer j, k, m
      integer Lin, Lout
      integer Nfiles, Nflags, Nantpos, Nexcept
      integer Nant
      integer coropt
      integer Nread, Nwread, LastChn
      integer nspect, nwide
      integer ant1, ant2
      integer nschan(MAXCHAN), ischan(MAXCHAN)
      real timeoff
      real dra
      real wt, wtup, wtdn
      real phase, timphz, radphz, antphz, tmphaz, tmpdelay
      real delay(MAXANT)
      real wfreq(MAXCHAN)
      double precision val1, val2
      double precision RA, dec, raoff, decoff, delra, deldec
      double precision freq, Lst, UT, obsra, obsdec, HA
      double precision lo1, lo2
      double precision cosHA, sinHA, cosdec, sindec
      double precision delX, delY, delZ
      double precision bxnew, bynew, bznew
      double precision BX, BY, BZ
      double precision uu, vv
      double precision preamble(4)
      double precision dummy(3 * MAXANT)
      double precision antpos(MAXANT, 3)
      double precision XYZ(MAXANT, 3), newxyz(MAXANT, 3)
      double precision sdf(MAXCHAN), sfreq(MAXCHAN)
      double precision epoch, jepoch, theta, costh, sinth
      complex delta
      complex data(MAXCHAN), wdata(MAXCHAN)
      logical suffix, allsrc, srcfound, changed
      logical raabs, decabs, antabs
      logical dotime, dorad, doants, dodelay, douv, dodra
      logical dopntra, dopntdec, dodelra, dodeldec
      logical updUT, updLst, updodra, updora, updodec, updra, upddec
      logical dowide, docorr
      logical more, updated
      logical flags(MAXCHAN), wflags(MAXCHAN)
c
c  Externals.
c
      integer Len1
      logical KeyPrsnt
      double precision epo2jul
c
c  End declarations.
c-----------------------------------------------------------------------
c  Announce program.
c
      call Output(PROG // VERSION)
c-----------------------------------------------------------------------
c  Use the key routines to get the user input parameters and check the
c  input parameters for incorrect entries.
c
      call KeyIni
c
      call MKeyf('vis', Vis, MAXFILES, Nfiles)
      if (Vis(1) .eq. ' ') then
        errmsg = PROG // 'A Visibility file must be given.'
        call Bug('f', errmsg)
      endif
c
      suffix = .TRUE.
      if (Nfiles .eq. 1) then
        call Keya('out', Outsave, ' ')
        if (Outsave .ne. ' ') suffix = .FALSE.
      endif
c
      allsrc = .TRUE.
      if (KeyPrsnt('source')) then
        call Keya('source', Selsrc, ' ')
        if (Selsrc .eq. ' ') then
          errmsg = PROG // 'Incorrect source name entered.'
          call Bug('f', errmsg)
        endif
        allsrc = .FALSE.
      endif
c
      Nflags = 0
c
c Time.
c
      timeoff = 0.0
      dotime = .FALSE.
      if (KeyPrsnt('time')) then
        call Keyr('time', timeoff, -100001.0)
        if (timeoff .le. -100000.0) then
          errmsg = PROG // 'Incorrect time offset entered.'
          call Bug('f', errmsg)
        endif
        timeoff = timeoff * SECRAD
        dotime = .TRUE.
        Nflags = Nflags + 1
      endif
c
c Position.  Default these logicals here in case only an RA or Dec
c correction (but not both) is specified.
c
      raoff = 0
      decoff = 0
      dorad = .FALSE.
      raabs = .FALSE.
      decabs = .FALSE.
c RA.
      if (KeyPrsnt('ra')) then
        call Keyd('ra', raoff, -100001.0)
        if (KeyPrsnt('ra')) then
          call Keyd('ra', val1, -1.0)
          call Keyd('ra', val2, -1.0)
          if ((val1 .lt. 0) .or. (val2 .lt. 0)) then
            errmsg = PROG // 'Incorrect RA entered.'
            call Bug('f', errmsg)
          endif
          raoff = (raoff * 3600.0) + (val1 * 60.0) + val2
          raabs = .TRUE.
        else
          if (raoff .le. -100000.0) then
            errmsg = PROG // 'Incorrect RA offset entered.'
            call Bug('f', errmsg)
          endif
        endif
        dorad = .TRUE.
        Nflags = Nflags + 1
      endif
c Dec.
      if (KeyPrsnt('dec')) then
        call Keyd('dec', decoff, -100001.0)
        if (KeyPrsnt('dec')) then
          call Keyd('dec', val1, -1.0)
          call Keyd('dec', val2, -1.0)
          if ((val1 .lt. 0) .or. (val2 .lt. 0)) then
            errmsg = PROG // 'Incorrect DEC entered.'
            call Bug('f', errmsg)
          endif
          if (decoff .eq. 0.0) then
            if (val1 .ge. 0.0) then
              decoff = (val1 * 60.0) + val2
            else
              decoff = (val1 * 60.0) - val2
            endif
          else if (decoff .gt. 0.0) then
            decoff = (decoff * 3600.0) + (val1 * 60.0) + val2
          else
            decoff = (decoff * 3600.0) - (val1 * 60.0) - val2
          endif
          decabs = .TRUE.
        else
          if (decoff .le. -100000.0) then
            errmsg = PROG // 'Incorrect DEC offset entered.'
            call Bug('f', errmsg)
          endif
        endif
        dorad = .TRUE.
        Nflags = Nflags + 1
      endif
      raoff = raoff * SECRAD
      decoff = decoff * ASECRAD
c
c Antenna.
c
      j = 0
      if (KeyPrsnt('antpos')) j = j + 1
      if (KeyPrsnt('dantpos')) j = j + 1
      if (KeyPrsnt('apfile')) j = j + 1
      if (j .gt. 1) then
        errmsg = PROG //
     *    'ANTPOS, DANTPOS, and APFILE are mutually exclusive options.'
        call Bug('f', errmsg)
      endif
      doants = .FALSE.
      if (KeyPrsnt('apfile')) then
        call Keyf('apfile', apFile, ' ')
        if ((apFile .eq. ' ') .or. (Len1(apFile) .le. 0)) then
          errmsg = PROG // 'keyword APFILE incorrectly specified.'
          call Bug('f', errmsg)
        endif
        call readapf(PROG, apFile, XYZ, MAXANT)
        antabs = .TRUE.
        doants = .TRUE.
        Nflags = Nflags + 1
      else if (KeyPrsnt('antpos') .or. KeyPrsnt('dantpos')) then
c  Initialize the antenna arrays.
        do j = 1, MAXANT
          XYZ(j, 1) = 0
          XYZ(j, 2) = 0
          XYZ(j, 3) = 0
        enddo
        if (KeyPrsnt('antpos')) then
          antkey = 'antpos'
          antabs = .TRUE.
        else if (KeyPrsnt('dantpos')) then
          antkey = 'dantpos'
          antabs = .FALSE.
        endif
        Nantpos = 0
        call Keyi(antkey, j, 0)
        do while (j .gt. 0)
          if (j .gt. MAXANT) then
            errmsg = PROG // 'Antenna number larger than expected.'
            call Bug('f', errmsg)
          endif
          Nantpos = Nantpos + 1
          call Keyd(antkey, XYZ(j, 1), 0.0)
          call Keyd(antkey, XYZ(j, 2), 0.0)
          call Keyd(antkey, XYZ(j, 3), 0.0)
          call Keyi(antkey, j, 0)
        enddo
        if (j .lt. 0) then
          errmsg = PROG // 'Incorrect antenna number entered.'
          call Bug('f', errmsg)
        endif
        if (Nantpos .eq. 0) then
          errmsg = PROG // 'No antenna offsets entered.'
          call Bug('f', errmsg)
        endif
        if (Nantpos .gt. MAXANT) then
          errmsg = PROG // 'More antennas entered than expected.'
          call Bug('f', errmsg)
        endif
        doants = .TRUE.
        Nflags = Nflags + 1
      endif
c
c Delay.
c
      dodelay = .FALSE.
      if (KeyPrsnt('delay')) then
        do j = 1, MAXANT
          delay(j) = 0.0
        enddo
c  Initialize the delay array.
        j = 0
        do while (KeyPrsnt('delay'))
          j = j + 1
          if (j .gt. MAXANT) then
            errmsg = PROG //
     *        'Number of delays larger than program can handle.'
            call Bug('f', errmsg)
          endif
          call Keyr('delay', delay(j), 0.0)
        enddo
        if (j .lt. 1) then
          errmsg = PROG // 'No delay values entered.'
          call Bug('f', errmsg)
        endif
        dodelay = .TRUE.
        Nflags = Nflags + 1
      endif
c
c Options.
c
      call GetOpt(douv, dodra)
      if ((.not. douv) .and. (dorad .or. dotime .or. doants))
     *  call Bug('w',
     *  'OPTIONS=NOUV possibly unsafe for these editing options.')
      if (dodra) Nflags = Nflags + 1
c
      call KeyFin
c
c Should never get in this if-conditional!
c
      if (Nflags .eq. 0) then
        errmsg = PROG // 'No editing option selected.'
        call Bug('f', errmsg)
      endif
c
c  Check if the user has specified a source name but not selected
c  RA/Dec editing.  If this is the case, send a warning message
c  but continue on with the program.
c
      if (.not. allsrc) then
        if ((.not. dorad) .or. dotime .or. doants .or. dodelay) then
          errmsg = PROG //
     *      'Options selected for a single source might cause problems?'
          call Bug('w', errmsg)
        endif
      endif
c
c  End of user inputs.
c-----------------------------------------------------------------------
c  Set up tracking so that unedited items are directly copied.
c  If an item is listed in the except list, it is not copied directly
c  and, hence, needs to be written explicitly.  The first five items
c  are required because they are always written with every UVWRITE (in
c  the preamble and the correlator data).  The sixth item is also
c  required as it written with every call to UVWWRITE.
c
      except(1) = 'coord'
      except(2) = 'baseline'
      except(3) = 'time'
      except(4) = 'tscale'
      except(5) = 'corr'
      except(6) = 'wcorr'
      except(7) = 'antpos'
      Nexcept = 7
      if (dotime) then
        except(Nexcept + 1) = 'ut'
        except(Nexcept + 2) = 'lst'
        Nexcept = Nexcept + 2
      endif
      if (dorad) then
        except(Nexcept + 1) = 'ra'
        except(Nexcept + 2) = 'dec'
        except(Nexcept + 3) = 'obsra'
        except(Nexcept + 4) = 'obsdec'
        Nexcept = Nexcept + 4
      endif
      if (dodra) then
        except(Nexcept + 1) = 'dra'
        Nexcept = Nexcept + 1
      endif
      Nant = -1
c
c  Open the input visibility file.
c
      do j = 1, Nfiles
        srcfound = allsrc
        changed = .FALSE.
        call UvOpen(Lin, Vis(j), 'old')
        call UvTrack(Lin, 'antpos', 'u')
        call TrackIt(Lin, except, Nexcept)
        call UvNext(Lin)
c
c  Check that this data set is in cross correlation mode.
c
        k = Len1(Vis(j))
        call uvrdvri(Lin, 'coropt', coropt, 0)
        if (coropt .ne. 0) then
          errmsg = PROG //
     *             'Correlator is not set in cross correlation mode.'
          call Bug('w', errmsg)
          errmsg = PROG // 'Ignoring visibility file: ' // Vis(j)(1:k)
          call Bug('w', errmsg)
        else
          mesg = PROG // 'Editing visibility file: ' // Vis(j)(1:k)
          call Output(mesg)
c
c  Determine if this data set has narrow and/or wide band data.
c
          call UvProbvr(Lin, 'corr', type, k, updated)
          call Lcase(type)
          docorr = ((type .eq. 'r') .or. (type .eq. 'j'))
          corrtype =  type
          if (.not. docorr) then
            if (dodelay) then
              k = Len1(Vis(j))
              errmsg = PROG //
     *             'No narrow band data present in ' // Vis(j)(1:k)
              call Bug('w', errmsg)
              errmsg = PROG //
     *             'No delay correction will be applied to ' //
     *             Vis(j)(1:k)
              call Bug('w', errmsg)
            endif
          endif
          call UvProbvr(Lin, 'wcorr', type, k, updated)
          call Lcase(type)
          dowide = (type .eq. 'c')
          if ((.not. docorr) .and. (.not. dowide)) then
            k = Len1(Vis(j))
            errmsg = PROG // 'No wide or narrow band data to edit.'
            call Bug('w', errmsg)
            errmsg = PROG // 'Ignoring visibility file: ' // Vis(j)(1:k)
            call Bug('w', errmsg)
          else
c
c  Open the output visibility file.
c
            Outfile = ' '
            if (suffix) then
              k = Len1(Vis(j))
              Outfile = Vis(j)(1:k) // ENDING
            else
              k = Len1(Outsave)
              Outfile = Outsave(1:k)
            endif
            call UvOpen(Lout, Outfile, 'new')
            mesg = PROG // 'Writing visibilities to: '// Outfile
            call Output(mesg)
            if (docorr)
     *        call uvset(Lout, 'corr', corrtype, 0, 0.0, 0.0, 0.0)
c
c  Copy all calibration tables.
c
	    call CalCopy(Lin,Lout,PROG)
c
c  Copy the old history entries to the new file and then add a few
c  additional history entries to the new file.
c
            call hdcopy(Lin, Lout, 'history')
            call HisOpen(Lout, 'append')
            call HisWrite(Lout, PROG // ' Miriad '// PROG // VERSION)
            call HisInput(Lout, PROG)
c
c  Initialize the new coordinate array, but only if necessary.
c
            if (doants) then
              do k = 1, MAXANT
                newxyz(k, 1) = 0.0
                newxyz(k, 2) = 0.0
                newxyz(k, 3) = 0.0
              enddo
            endif
c
c  If there are wide band channels but no narrowband channels, set up
c  uvread and uvwrite to speak wide band channels
c
            if(dowide .and. (.not. docorr)) then
              call uvset(Lin, 'data','wide',0, 1.0, 1.0, 1.0)
              call uvset(Lout,'data','wide',0, 1.0, 1.0, 1.0)
            endif
c
c  See if the delay tracking or pointing center RA and DEC are present.
c
	    call UvProbvr(Lin, 'delra', type, k, updated)
	    dodelra = type.eq.' '
	    call UvProbvr(Lin, 'deldec',type, k, updated)
	    dodeldec = type.eq.' '
	    call UvProbvr(Lin, 'pntra', type, k, updated)
	    dopntra = type.eq.' '
	    call UvProbvr(Lin, 'pntdec',type, k, updated)
	    dopntdec = type.eq.' '
c
c  Begin editing the input file.
c
            call UvRewind(Lin)
            more = .TRUE.
            do while (more)
              call UvRead(Lin, preamble, data, flags, MAXCHAN, Nread)
              if (Nread .le. 0) then
                more = .FALSE.
              else
c
c  Copy unchanged variables to the output data set.
c
                call UvCopyvr(Lin, Lout)
c
c  If first time through the data, get the number of antennas present;
c  otherwise, warn the user that the number of antennas has changed.
c
c  Change of method to make sure 'nants' is read properly when the
c  file is rewound.
c
c               call UvProbvr(Lin, 'nants', type, k, updated)
c               if (updated) then
c--             call UvGetvri(Lin, 'nants', k, 1)
                call uvrdvri(Lin, 'nants', k, 0)
                if (k .ne. Nant) then
                  if (Nant .eq. -1) then
                    Nant = k
                  else if (Nant .ne. k) then
                    write(errmsg, '(A, A, I4, A, I4)') PROG,
     *                'The number of antennas has changed from ',
     *                Nant, ' to ', k
                    call Bug('w', errmsg)
                  endif
                  Nant = k
                endif
c
c  Get lo1 and lo2 for use with delay corrections.
c
                call UvProbvr(Lin, 'lo1', type, k, updated)
c--             if (updated) call UvGetvrd(Lin, 'lo1', lo1, 1)
                if (updated) call uvrdvrd(Lin, 'lo1', lo1, 0.0D0)
                call UvProbvr(Lin, 'lo2', type, k, updated)
c--             if (updated) call UvGetvrd(Lin, 'lo2', lo2, 1)
                if (updated) call uvrdvrd(Lin, 'lo2', lo2, 0.0D0)
c
c  Check if we are looking at the proper source.
c
                call UvProbvr(Lin, 'source', type, k, updated)
                if (updated) call UvGetvra(Lin, 'source', source)
                if ((allsrc) .or. (Selsrc .eq. source)) then
                  srcfound = .TRUE.
c
c  Get the antenna positions for this particular baseline and then
c  compute the baseline positions (in wavelength/GHz units).
c  Whenever the antenna positions change, then recompute the
c  new values and write them out.  This should ALWAYS happen on
c  the first UvRead of every file.  Since NewAnt writes to the
c  history file whenever an antenna moves, make sure that the
c  history file is opened prior to any call of NewAnt.
c
                  call UvProbvr(Lin, 'antpos', type, k, updated)
                  if (updated) then
                    call UvGetvrd(Lin, 'antpos', dummy, Nant * 3)
                    do k = 1, Nant
                      antpos(k, 1) = dummy(k)
                      antpos(k, 2) = dummy(k + Nant)
                      antpos(k, 3) = dummy(k + Nant + Nant)
                    enddo
                    call NewAnt(PROG, Lout, antpos, XYZ, MAXANT, Nant,
     *                doants, antabs, newxyz)
                    do k = 1, Nant
                      dummy(k) = newxyz(k, 1)
                      dummy(k + Nant) = newxyz(k, 2)
                      dummy(k + Nant + Nant) = newxyz(k, 3)
                    enddo
                    call UvPutvrd(Lout, 'antpos', dummy, Nant * 3)
                  endif
                  call BasAnt(preamble(4), ant1, ant2)
                  BX = antpos(ant2, 1) - antpos(ant1, 1)
                  BY = antpos(ant2, 2) - antpos(ant1, 2)
                  BZ = antpos(ant2, 3) - antpos(ant1, 3)
                  if (doants) then
                    bxnew = newxyz(ant2, 1) - newxyz(ant1, 1)
                    bynew = newxyz(ant2, 2) - newxyz(ant1, 2)
                    bznew = newxyz(ant2, 3) - newxyz(ant1, 3)
                    delX = bxnew - BX
                    delY = bynew - BY
                    delZ = bznew - BZ
                  else
                    bxnew = BX
                    bynew = BY
                    bznew = BZ
                    delX = 0.0
                    delY = 0.0
                    delZ = 0.0
                  endif
c
c  Get the declination and Hour Angle and position offsets.  If any
c  of the values listed below have changed and are not being edited,
c  then they are already automatically copied to the output file.
c  Edited values are not automatically copied and so we need to keep
c  track of their value and when they get updated.  The returned
c  logical ``updXXXX'' has been "and"-ed with the operational logical.
c  The reason for this is that operations on updated variables only
c  happen when that variable is also being edited (hence, it saves
c  having to do an if(updXXX .and. doedit) everywhere.  If the variable
c  has not changed, then it will retain the previous obtained value.
c
                  call GetUpd(Lin, 'ut', dotime, UT, updUT)
                  call GetUpd(Lin, 'lst', dotime, Lst, updLst)
                  call GetUpd(Lin, 'obsra', dorad, obsra, updora)
                  call GetUpd(Lin, 'obsdec', dorad, obsdec, updodec)
                  call GetUpd(Lin, 'ra', dorad, RA, updra)
                  call GetUpd(Lin, 'dec', dorad, dec, upddec)
                  call GetUpr(Lin, 'dra', dodra, dra, updodra) 
   10             format(A, A, ' changed from ', G17.9, ' to ', G17.9)
                  if (updUT) then
                    val1 = UT + timeoff
                    call UvPutvrd(Lout, 'ut', val1, 1)
                    write (mesg, 10) PROG, 'UT', UT, val1
                    call HisWrite(Lout, mesg)
                  endif
                  if (updLst) then
                    val1 = Lst + timeoff
                    call UvPutvrd(Lout, 'lst', val1, 1)
                    write (mesg, 10) PROG, 'LST', Lst, val1
                    call HisWrite(Lout, mesg)
                  endif
                  if (updora) then
                    val1 = obsra + raoff
                    if (raabs) val1 = raoff - RA + obsra
                    call UvPutvrd(Lout, 'obsra', val1, 1)
                    write (mesg, 10) PROG, 'ObsRA', obsra, val1
                    call HisWrite(Lout, mesg)
                  endif
                  if (updra) then
                    val1 = RA + raoff
                    if (raabs) val1 = raoff
                    call UvPutvrd(Lout, 'ra', val1, 1)
                    if(dopntra)call UvPutvrd(lOut, 'pntra', RA, 1)
                    if(dodelra)call UvPutvrd(lOut, 'delra', RA, 1)
                    write (mesg, 10) PROG, 'RA', RA, val1
                    call HisWrite(Lout, mesg)
                  endif
                  if (updodec) then
                    val1 = obsdec + decoff
                    if (decabs) val1 = decoff - dec + obsdec
                    call UvPutvrd(Lout, 'obsdec', val1, 1)
                    write (mesg, 10) PROG, 'ObsDec', obsdec, val1
                    call HisWrite(Lout, mesg)
                  endif
                  if (upddec) then
                    val1 = dec + decoff
                    if (decabs) val1 = decoff
                    call UvPutvrd(Lout, 'dec', val1, 1)
                    if(dopntdec)call UvPutvrd(lOut, 'pntdec', dec, 1)
                    if(dodeldec)call UvPutvrd(lOut, 'deldec', dec, 1)
                    write (mesg, 10) PROG, 'Dec', dec, val1
                    call HisWrite(Lout, mesg)
                  endif
                  if (updodra) then
                    write (mesg, 10) PROG, 'DRA', dra, dra * cos(obsdec)
                    dra = dra * cos(obsdec)
                    call UvPutvrr(Lout, 'dra', dra, 1)
                    call HisWrite(Lout, mesg)
                  endif
c
c  Determine the phase increments for the various types of editing.
c
                  HA = Lst - obsra
                  cosHA = cos(HA)
                  sinHA = sin(HA)
                  cosdec = cos(obsdec)
                  sindec = sin(obsdec)
                  delra = raoff
                  if (raabs) delra = raoff - RA
                  deldec = decoff
                  if (decabs) deldec = decoff - dec
c Get original uv coordinates.
                  uu = preamble(1)
                  vv = preamble(2)
                  phase = 0.0
c Time.
                  timphz = -uu * cosdec * timeoff
                  if (dotime) phase = phase - timphz
c Ra and Dec.
                  if (dorad) then
                    call Getrdphz(Lin,RA+delra,dec+deldec,uu,vv,radphz)
                    phase = phase - radphz
                  endif
c Antenna.
                  antphz = (((delX * cosHA) - (delY * sinHA)) * cosdec)
     *                     + (delZ * sindec)
                  if (doants) phase = phase - antphz
c Delay (treated differently because of lo dependence).
                  tmpdelay = 0.0
                  if (dodelay .and. docorr) then
                    tmpdelay = delay(ant2) - delay(ant1)
                    if (abs(tmpdelay) .lt. 0.05) tmpdelay = 0.0
                    tmpdelay = 2 * PI * tmpdelay
                  endif
c
                  phase = 2 * PI * phase
c
c  Set headers variables to new values.
c
c  Get new uv coordinates...
                  if (dotime) then
                    preamble(3) = preamble(3) + (timeoff / (2 * PI))
                    HA = HA + timeoff
                  endif
                  if (dorad) HA = HA - delra
                  if (dotime .or. dorad) then
                    cosHA = cos(HA)
                    sinHA = sin(HA)
                    cosdec = cos(obsdec + deldec)
                    sindec = sin(obsdec + deldec)
                  endif
                  if (douv) then
                    uu = (bxnew * sinHA) + (bynew * cosHA)
                    vv = (((-bxnew * cosHA) + (bynew * sinHA)) * sindec)
     *                 + (bznew * cosdec)
c
                    call uvrdvrd(Lin, 'epoch', epoch, 2000.0d0)
                    jepoch = epo2jul(epoch, ' ')
                    call prerotat(jepoch, RA, dec, preamble(3), theta)
                    costh = cos(theta)
                    sinth = sin(theta)
c
                    preamble(1) = (uu * costh) + (vv * sinth)
                    preamble(2) = (vv * costh) - (uu * sinth)
                  endif
c
c  Get particular headers necessary to do editing (these items have
c  already been copied, so there is no need to write them again).
c
                  if (dowide) call GetWide(Lin, MAXCHAN, nwide, wfreq)
                  if (docorr) call GetCoor(Lin, MAXCHAN, nspect, nschan,
     *              ischan, sdf, sfreq)
c
c  Apply corrections...
c
                  if(dowide .and. (.not. docorr)) then
                    if (phase .ne. 0.0) then
                      changed = .TRUE.
                      do k = 1, nwide
                        tmphaz = phase * wfreq(k)
                        delta = cmplx(cos(tmphaz), sin(tmphaz))
                        data(k) = data(k) * delta
                      enddo
                    endif
                  endif
c
                  tmphaz = phase
                  if (dodelay) tmphaz = tmphaz - tmpdelay
                  if ((docorr) .and. (tmphaz .ne. 0.0)) then
                    changed = .TRUE.
                    do k = 1, nspect
                      LastChn = ischan(k) + nschan(k) - 1
                      do m = ischan(k), LastChn
                        freq = sfreq(k) + (sdf(k) * (m - ischan(k)))
                        tmphaz = phase * freq
                        if (dodelay .and. (tmpdelay .ne. 0.0)) then
                          freq = freq - lo1 - lo2
                          tmphaz = tmphaz - (tmpdelay * freq)
                        endif
                        delta = cmplx(cos(tmphaz), sin(tmphaz))
                        data(m) = data(m) * delta
                      enddo
                    enddo
                  endif
c
                  if (dowide .and. docorr) then
                    call UvWread(Lin, wdata, wflags, MAXCHAN, Nwread)
                    if (Nwread .le. 0) then
                      errmsg = PROG // 'No wide band data?'
                      call Bug('f', errmsg)
                    else
                      if (dodelay .and. (tmpdelay .ne. 0.0)) then
c
c  Reconstruct the digital wide band data if delay error.  No need to
c  apply other corrections since the narrow band data has it already.
c  Weight the sums by the square of the bandwidth and keep different
c  sums for the upper and lower sidebands.  Only include data that is
c  previously flagged as "good" in the narrow bands.  Also omit the
c  first and last ENDCHN channels in each window.
c
                        wtup = 0.0
                        wtdn = 0.0
                        wflags(1) = .TRUE.
                        wflags(2) = .TRUE.
                        wdata(1) = cmplx(0.0, 0.0)
                        wdata(2) = cmplx(0.0, 0.0)
                        do k = 1, nspect
CC                        if (wflags(k)) then
                            wt = abs(sdf(k) * nschan(k) / BANDWID)
                            LastChn = ischan(k) + nschan(k) - 1 - ENDCHN
                            do m = ischan(k) + ENDCHN, LastChn
                              if (flags(m)) then
                                if (sfreq(k) .gt. lo1) then
                                  wdata(2) = wdata(2) + (data(m) * wt)
                                  wtup = wtup + wt
                                else
                                  wdata(1) = wdata(1) + (data(m) * wt)
                                  wtdn = wtdn + wt
                                endif
                              endif
                            enddo
CC                        endif
                        enddo
                        if (wtdn .gt. 0.0) wdata(1) = wdata(1) / wtdn
                        if (wtup .gt. 0.0) wdata(2) = wdata(2) / wtup
                      else if (phase .ne. 0.0) then
c  Simply apply same corrections to wide band data.
                        do k = 1, nwide
                          tmphaz = phase * wfreq(k)
                          delta = cmplx(cos(tmphaz), sin(tmphaz))
                          wdata(k) = wdata(k) * delta
                        enddo
                      endif
                    endif
                    call UvWwrite(Lout, wdata, wflags, Nwread)
                  endif
c
c  Not the correct source... (ie. allsrc=FALSE and source!=Selsrc).
c  Just make sure to copy any wide band data.
c
                else
                  if (dowide .and. docorr) then
                    call UvWread(Lin, wdata, wflags, MAXCHAN, Nwread)
                    if (Nwread .le. 0) then
                      errmsg = PROG // 'No wide band data?'
                      call Bug('f', errmsg)
                    endif
                    call UvWwrite(Lout, wdata, wflags, Nwread)
                  endif
                endif
c
c  Write out preamble and any narrow band data present.
c
                call UvWrite(Lout, preamble, data, flags, Nread)
c
c  End of editing step (data was present: more = TRUE).
c
              endif
c
c  End of reading loop (more = FALSE).
c
            enddo
c
c  Close the new history file and UV data set.
c
            call HisClose(Lout)
            call UvClose(Lout)
c
c  End of wide/narrow check if.
c
          endif
c
c  End of cross/auto check if.
c
        endif
c
c  Close the old UV data set.
c
        call UvClose(Lin)
c
c  Warn if source not found in a particular dataset.
c
        if (.not. srcfound) then
          k = Len1(Vis(j))
          m = Len1(Selsrc)
          errmsg = PROG // 'Source [' // Selsrc(1:m) //
     *      '] not found in ' // Vis(j)(1:k)
          call bug('w', errmsg)
        endif
c
c  Warn if no apparent changes have been made.
c
        if (.not. changed) then
          k = Len1(Vis(j))
          errmsg = PROG // 'No apparent changes made to ' // Vis(j)(1:k)
          call bug('w', errmsg)
        endif
c
c  End of Nfiles do loop.
c
      enddo
c
c  End of main routine.
c
      end
c***********************************************************************
	subroutine CalCopy(Lin,Lout,prog)
c
	implicit none
	integer Lin,Lout
	character prog*(*)
c
c  Copy the calibration tables from the input to the output.
c
c  Input:
c    Lin,Lout	Handles of the input and output datasets.
c    prog	Program name (for messages).
c------------------------------------------------------------------------
	character umsg*64
c
c  Externals.
c
	logical hdprsnt
c
	if(hdprsnt(Lin,'gains'))then
	  umsg = prog//'Copying antanne gain calibration'
	  call output(umsg)
          call hdcopy(Lin,Lout,'interval')
          call hdcopy(Lin,Lout,'nsols')
          call hdcopy(Lin,Lout,'ngains')
          call hdcopy(Lin,Lout,'nfeeds')
          call hdcopy(Lin,Lout,'ntau')
          call hdcopy(Lin,Lout,'gains')
          call hdcopy(Lin,Lout,'freq0')
	endif
c
	if(hdprsnt(Lin,'bandpass'))then
	  umsg = prog//'Copying bandpass calibration'
	  call output(umsg)
	  call hdcopy(Lin,Lout,'ngains')
          call hdcopy(Lin,Lout,'nfeeds')
          call hdcopy(Lin,Lout,'ntau')
          call hdcopy(Lin,Lout,'bandpass')
          call hdcopy(Lin,Lout,'freqs')
          call hdcopy(Lin,Lout,'nspect0')
          call hdcopy(Lin,Lout,'nchan0')
	endif
c
	if(hdprsnt(Lin,'cgains'))then
	  umsg = prog//'Copying cgains table'
	  call output(umsg)
          call hdcopy(Lin,Lout,'cgains')
          call hdcopy(Lin,Lout,'ncbase')
          call hdcopy(Lin,Lout,'ncgains')
	endif
	if(hdprsnt(Lin,'wgains'))then
	  umsg = prog//'Copying wgains table'
	  call output(umsg)
          call hdcopy(Lin,Lout,'wgains')
          call hdcopy(Lin,Lout,'nwbase')
          call hdcopy(Lin,Lout,'nwgains')
	endif
c
	if(hdprsnt(Lin,'leakage'))then
	  umsg = prog//'Copying polarisation calibration'
	  call output(umsg)
	  call hdcopy(Lin,Lout,'leakage')
	endif
	end
c***********************************************************************
cc= GetOpt - Internal routine to get command line options.
cc& jm
cc: internal utility
cc+
      subroutine GetOpt(douv, dodra)
      implicit none
      logical douv, dodra
c
c  Get user options from the command line.
c
c  Input:
c    none
c
c  Output:
c    douv     If true, then recompute u and v.
c    dodra    If true, then multiply dra by 1/cos(obsdec).
c
c--
c-----------------------------------------------------------------------
c
c  Internal variables.
c
      integer NOPTS
      parameter (NOPTS = 2)
c
      character opts(NOPTS)*9
      logical present(NOPTS)
      data opts/'nouv', 'dra'/
c
      call Options('options', opts, present, NOPTS)
      douv = .not. present(1)
      dodra = present(2)
c
      end
c************************************************************************
      subroutine Getrdphz(Lin,RA,dec,uu,vv,radphz)
c
      implicit none
      integer Lin
      double precision RA,dec,uu,vv
      real radphz
c
c  Determine the phase shift to apply to center the dataset at a particular
c  position.
c
c  Input:
c    Lin    Handle of the input dataset.
c    RA,dec Coordinates (radians) of the point of interest.
c    uu,vv  u-v coordinates.
c  Output:
c    radphz
c------------------------------------------------------------------------
      real dra, ddec
      double precision x1(2),x2(2)
c
      call uvrdvrr(Lin, 'dra', dra, 0.0)
      call uvrdvrr(Lin, 'ddec', ddec, 0.0)

      x1(1) = RA
      if (dra .ne. 0.0) x1(1) = x1(1) + (dra / cos(dec))
      x1(2) = dec + ddec
c
      call coInit(Lin)
      call coCvt(Lin,'aw/aw',x1,'op/op',x2)
      call coFin(Lin)
      radphz = (uu * x2(1)) + (vv * x2(2))
c
      end
c***********************************************************************
cc= TrackIt - Internal routine to track almost all UV variables.
cc& jm
cc: calibration, uv-data
cc+
      subroutine trackit(lin, except, nexcept)
      implicit none
      integer lin, nexcept
      character except(nexcept)*(*)
c
c  TrackIt marks every item in the vartable to ``copy'' mode with
c  the exception of the items listed in the ``except'' array.
c
c  Input:
c    Lin     Input UV data set handle.
c    except  Character array of exception item names.
c    nexcept Number of items in the exception array.
c
c  Output:
c    none
c
c--
c-----------------------------------------------------------------------
c
c  Internal variables.
c
      character varname*11 
c     character text*80
      integer j, item, iostat
      logical track
c
      call UvNext(Lin)
      call haccess(Lin, item, 'vartable', 'read', iostat)
      if (iostat .ne. 0) then
        call Bug('w', 'TRACKIT:  Could not access the vartable.')
        call Bug('w', 'TRACKIT:  Value returned from haccess call:')
        call Bugno('f', iostat)
      endif
c
      call hreada(item, varname, iostat)
      do while (iostat .eq. 0)
        track = .TRUE.
        do j = 1, nexcept
          if (varname(3:10) .eq. except(j)) track = .FALSE.
        enddo
        if (track) then
          call UvTrack(Lin, varname(3:10), 'c')
C        else
C          write(text, '('' DEBUG: Variable not directly copied: '', a)')
C     *          varname(3:10)
C          call Output(text)
        endif
        call hreada(item, varname, iostat)
      enddo
c
      call hdaccess(item, iostat)
      if (iostat .ne. 0) then
        call Bug('w', 'TRACKIT:  Could not de-access the vartable.')
        call Bug('w', 'TRACKIT:  Value returned from hdaccess call:')
        call Bugno('f', iostat)
      endif
      call UvRewind(Lin)
      return
      end
c
c***********************************************************************
cc= NewAnt - Internal routine to determine new antenna spacings.
cc& jm
cc: calibration, uv-i/o, uv-data, utilities
cc+
      subroutine newant(prog, Lout, antpos, xyz, maxant, nant, doants,
     *  antabs, newxyz)
      implicit none
      integer maxant, nant, Lout
      character prog*(*)
      double precision antpos(maxant, 3), xyz(maxant, 3)
      double precision newxyz(maxant, 3)
      logical doants, antabs
c
c  NewAnt returns the antenna positions that are corrected by the
c  values input by the user.  if ``doants'' is FALSE, then no
c  editing is being performed, so ``newxyz'' will just be filled
c  with ``antpos''.  If ``doants'' is TRUE, then apply the changes
c  as follows:  If ``antabs'' is TRUE, then the positions given in
c  the array ``xyz'' are used to fill the entries of ``newxyz''.
c  If no value is present in the ``xyz'' array for a particular
c  antenna, then the value of ``antpos'' is used.  If ``antabs''
c  is FALSE, then the values of ``xyz'' are added to the those in
c  the ``antpos'' array and are stored in ``newxyz''.
c
c  Input:
c    prog     The calling program's name (used for history writing).
c    Lout     The logical unit associated with the output file and
c             used for writing to an already opened history file.
c    antpos   The equatorial coordinates returned from a call to
c             UvRead (X, Y, Z; in nanosecs).
c    xyz      The equatorial coordinates input by the user
c             (X, Y, Z; in nanosecs).
c    maxant   The maximum first dimension of the antpos, xyz, and
c             newxyz arrays.
c    nant     The number of items in the antpos, xyz, and newxyz arrays.
c    doants   A logical that is TRUE if antenna position editing is
c             being performed.  It is FALSE if not, and this means
c             the input array ``antpos'' is just copied into ``newxyz''.
c    antabs   A logical that is TRUE if the positions in the xyz array
c             represents absolute positions or FALSE to indicate that
c             the xyz entries are offset positions to be added to the
c             corresponding entries in the antpos array.
c
c  Output:
c    newxyz   The new equatorial coordinates values (in nanosecs).
c
c--
c-----------------------------------------------------------------------
c
c  Internal variables.
c
      integer j, k, n
      character mesg*132, dir(3)*1
      double precision del, del2
      logical change
c
      integer Len1
c
      data dir /'X', 'Y', 'Z'/
c
      do 20 j = 1, nant
        del2 = 0
        do 10 k = 1, 3
          change = .FALSE.
          if (.not. doants) then
            newxyz(j, k) = antpos(j, k)
          else if (.not. antabs) then
            newxyz(j, k) = antpos(j, k) + xyz(j, k)
            if (xyz(j, k) .ne. 0) change = .TRUE.
            del = newxyz(j, k) - antpos(j, k)
            del2 = del2 + (del * del)
          else if (xyz(j, k) .eq. 0) then
            newxyz(j, k) = antpos(j, k)
          else
            newxyz(j, k) = xyz(j, k)
            change = .TRUE.
            del = newxyz(j, k) - antpos(j, k)
            del2 = del2 + (del * del)
          endif
          if (change) then
            write (mesg, 30) prog, j, dir(k), antpos(j, k), newxyz(j, k)
            n = Len1(mesg)
            call HisWrite(Lout, mesg(1:n))
          endif
   10   continue
        if (del2 .gt. 0.0001) then
          del = sqrt(del2)
          write (mesg, 40) prog, j
          n = Len1(mesg)
          call Bug('w', mesg(1:n))
          write (mesg, 50) del
          n = Len1(mesg)
          call Bug('w', mesg(1:n))
        endif
   20 continue
   30 format (A, 'Antenna ', I3, ' moved in ', A, ' from ',
     *       G15.8, ' to ', G15.8, ' nanosecs.')
   40 format (A, 'Antenna ', I3, ' moved by more than 0.01 nanosecs. ')
   50 format ( 10X, '(Total distance = ', G15.8, ' nanosecs.)' )
      return
      end
c
c***********************************************************************
cc= GetUpd - Internal routine to get changed header variables.
cc& jm
cc: calibration, uv-i/o, uv-data, utilities
cc+
      subroutine getupd(lin, variable, doedit, value, updated)
      implicit none
      integer lin
      character variable*(*)
      logical doedit, updated
      double precision value
c
c  Input:
c    lin      The input file descriptor.
c    variable The name of header to check to see if it is updated.
c    doedit   Set to TRUE if this variable is being edited.
c    value    The variable from the last call to GetUpd.
c
c  Output:
c    value    The variable.
c    updated  A logical that is TRUE if the variable was updated in
c             the last call to UVREAD and if ``doedit'' is set to
c             TRUE.  Otherwise, ``updated'' is set to FALSE.
c
c--
c-----------------------------------------------------------------------
c
c  Internal variables.
c
      character type*1
      integer length
c
      call UvProbvr(Lin, variable, type, length, updated)
      if ((type .eq. ' ') .or. (length .le. 0)) then
        value = 0.0
      else if (updated) then
c--     call UvGetvrd(Lin, variable, value, 1)
        call uvrdvrd(Lin, variable, value, 0.0D0)
      endif
      updated = updated .and. doedit
      return
      end
c
c***********************************************************************
cc= GetUpr - Internal routine to get changed header variables.
cc& jm
cc: calibration, uv-i/o, uv-data, utilities
cc+
      subroutine getupr(lin, variable, doedit, value, updated)
      implicit none
      integer lin
      character variable*(*)
      logical doedit, updated
      real value
c
c  Input:
c    lin      The input file descriptor.
c    variable The name of header to check to see if it is updated.
c    doedit   Set to TRUE if this variable is being edited.
c    value    The variable from the last call to GetUpr.
c
c  Output:
c    value    The variable.
c    updated  A logical that is TRUE if the variable was updated in
c             the last call to UVREAD and if ``doedit'' is set to
c             TRUE.  Otherwise, ``updated'' is set to FALSE.
c
c--
c-----------------------------------------------------------------------
c
c  Internal variables.
c
      character type*1
      integer length
c
      call UvProbvr(Lin, variable, type, length, updated)
      if ((type .eq. ' ') .or. (length .le. 0)) then
        value = 0.0
      else if (updated) then
c--     call UvGetvrr(Lin, variable, value, 1)
        call uvrdvrr(Lin, variable, value, 0.0)
      endif
      updated = updated .and. doedit
      return
      end
c
c***********************************************************************
cc= GetWide - Internal routine to get the wide band frequency array.
cc& jm
cc: calibration, uv-i/o, uv-data, utilities
cc+
      subroutine getwide(lin, maxwide, nwide, wfreq)
      implicit none
      integer lin, maxwide, nwide
      real wfreq(maxwide)
c
c     Values returned from this routine do not change from the
c     previous call unless they get updated during the intervening
c     call to UVREAD.
c
c  Input:
c    lin      The input file descriptor.
c    maxwide  Maximum size of the ``wfreq'' array.
c
c  Output:
c    nwide    The number of wide band channels.
c    wfreq    The array of wide band coorelation average frequencies.
c             This value is updated if ``nwide'' or ``wfreq'' changes.
c
c--
c-----------------------------------------------------------------------
      call uvrdvri(Lin, 'nwide', nwide, 0)
      if(nwide.gt.maxwide)call bug('f','Too many wideband chans for me')
      if(nwide.gt.0)call UvGetvrr(Lin, 'wfreq', wfreq, nwide)
c
      end
c***********************************************************************
cc= GetCoor - Internal routine to get the narrow band frequency arrays.
cc& jm
cc: calibration, uv-i/o, uv-data, utilities
cc+
      subroutine getcoor(lin, maxn, nspect, nschan, ischan, sdf, sfreq)
      implicit none
      integer lin, maxn, nspect
      integer nschan(maxn), ischan(maxn)
      double precision sdf(maxn), sfreq(maxn)
c
c     Values returned from this routine do not change from the
c     previous call unless they get updated during the intervening
c     call to UVREAD.  If ``nspect'' is updated, then all arrays
c     are updated as well.
c
c  Input:
c    lin      The input file descriptor.
c    maxn     Maximum size of the arrays.
c
c  Output:
c    nspect   The number of filled elements in the arrays.
c    nschan   The number of channels in a spectral window.
c    ischan   The starting channel of the spectral window.
c    sdf      The change in frequency per channel.
c    sfreq    Doppler tracked frequency of the first channel
c             in each window.
c
c--
c-----------------------------------------------------------------------
      call uvrdvri(Lin, 'nspect', nspect, 0)
c
      if(nspect.gt.maxn)call bug('f','Too many windows')
      if (nspect .gt. 0) then
        call UvGetvri(Lin, 'nschan', nschan, nspect)
	call UvGetvri(Lin, 'ischan', ischan, nspect)
	call UvGetvrd(Lin, 'sdf', sdf, nspect)
	call UvGetvrd(Lin, 'sfreq', sfreq, nspect)
      endif
c
      end
c***********************************************************************
cc= readAPF - Internal routine to read an antenna position file keyword.
cc& jm
cc: calibration, utilities
cc+
      subroutine readapf(progname, apFile, ants, maxants)
      implicit none
      character progname*(*), apFile*(*)
      integer maxants
      double precision ants(maxants, 3)
c
c  This routine reads from the specified antenna position file a
c  header line followed by the absolute positions for each antenna
c  (up to a specified maximum).  The order of the positions is the
c  same for each line of the file (x, y, z) and must appear in the
c  file in increasing antenna number order.  Lines that begin with
c  the '#' character are ignored and do not affect the antenna number
c  count.
c
c  The routine does not return if there is an error.
c
c  Input:
c    progname The name of the calling program (used by bug calls).
c    apFile   The name of the antenna position file.
c    maxants  The maximum number of antennas.
c
c  Input/Output:
c    ants     The array of absolute antenna positions.
c
c--
c-----------------------------------------------------------------------
c
c  Internal variables.
c
      character token*40
      character line*132, errmsg*132
      integer Lu
      integer j, nants
      integer L, k1, k2
      integer tlen, length, iostat
      double precision dpval
      logical okay
c
      character fullname*132
      integer Len1
c
      L = Len1(apFile)
      line = fullname(apFile(1:L))
      if (Len1(line) .le. 0) then
        errmsg = progname // 'Error finding file [' //
     *    apFile(1:L) // '].'
        call Bug('f', errmsg)
      endif
c
      call TxtOpen(Lu, line, 'old', iostat)
      if (iostat .ne. 0) then
        errmsg = progname // 'Error opening file [' //
     *    apFile(1:L) // '].'
        call Bug('w', errmsg)
        call Bugno('f', iostat)
      endif
c
c  Read the first (comment) line and then read until either an
c  EOF or error is identified (iostat=-1).  Skip any line that
c  begins with the '#' character.
c
      call TxtRead(Lu, line, length, iostat)
      if (iostat .ne. 0) then
        errmsg = progname // 'Error reading file [' //
     *    apFile(1:L) // '].'
        call Bug('w', errmsg)
        call Bugno('f', iostat)
      endif
c
      do j = 1, maxants
        ants(j, 1) = 0.0D0
        ants(j, 2) = 0.0D0
        ants(j, 3) = 0.0D0
      enddo
c
      nants = 0
      call TxtRead(Lu, line, length, iostat)
      do while (iostat .eq. 0)
        if (line(1:1) .ne. '#') then
          k1 = 1
          k2 = length
          nants = nants + 1
          if (nants .gt. maxants) then
            errmsg = progname // 'More antennas entered than expected.'
            call Bug('f', errmsg)
          endif
          do j = 1, 3
            call getfield(line, k1, k2, token, tlen)
            if (tlen .gt. 0) then
              call atodf(token(1:tlen), dpval, okay)
              if (okay) ants(nants, j) = dpval
            endif
          enddo
        endif
        call TxtRead(Lu, line, length, iostat)
      enddo
c
      if (iostat .ne. -1) then
        errmsg = progname // 'Error reading file [' //
     *    apFile(1:L) // '].'
        call Bug('w', errmsg)
        call Bugno('f', iostat)
      endif
      call TxtClose(Lu)
c
      if (nants .eq. 0) then
        errmsg = progname //
     *    'No antenna positions present in [' // apFile(1:L) // ']'
        call Bug('f', errmsg)
      endif
c
      return
      end
