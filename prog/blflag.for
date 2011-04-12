      program blflag

c= blflag -- Interactive flagging task.
c& rjs
c: uv analysis
c+
c       BLFLAG is a Miriad task for flagging visibilities interactively.
c       It plots visibilities (e.g. amplitude vs time), either one
c       baseline at a time or all together, and allows discrepant points
c       to be flagged with a cursor.
c
c       Commands are entered as a single character at the keyboard:
c         Left-Button  Left mouse button flags the nearest visibility.
c         Right-Button Right mouse button causes BLFLAG to precede to
c                      the next baseline.
c         <CR>         Carriage-return gives help.
c         ?            Help.
c         a            Flag nearest visibility (same as left mouse
c                      button).
c         c            Clear the flagging for this baseline and redraw
c                      plot.
c         h            Give help (same as carriage return).
c         p            Define a polygonal region, and flag visibilities
c                      within this region.  Define the vertices of the
c                      polygon by moving the cursor and then hitting the
c                      left mouse button (or a).  Finish defining the
c                      polygon by hitting the right mouse button (or x).
c                      You can delete vertices with the middle mouse
c                      button (or d).
c         q            Abort completely.  This does not apply flagging.
c         r            Redraw plot.
c         u            Unzoom.
c         x            Move to next baseline (same as right mouse
c                      button).
c         z            Zoom in.  You follow this by clicking the mouse
c                      on the left and right limits to zoom.
c
c@ vis
c       Input visibility dataset to be flagged.  No default.
c
c@ line
c       The normal Miriad linetype specification.  BLFLAG averages all
c       channels together before displaying them, flags being applied
c       to all channels.  The default is all channels.
c
c@ device
c       Normal PGPLOT plot device.  An interactive device, e.g. /xserve,
c       must be selected.  No default.
c
c@ stokes
c       Normal Stokes/polarisation parameter selection.  The default is
c       'ii' (i.e. Stokes-I assuming the source is unpolarised).  NOTE
c       BLFLAG plots the average of all the selected Stokes/polarisation
c       quantities.  Also it flags ALL quantities, regardless of whether
c       they were selected or not.
c
c@ select
c       Normal visibility data selection.  Only selected data can be
c       flagged.  The default is to select all data.
c
c@ axis
c       Two character strings, giving the X and Y axes of the plot.
c       Possible axis values are:
c         time         (the default for the X axis)
c         lst          Local apparent sidereal time.
c         uvdistance   sqrt(u**2+v**2)
c         hangle       (hour angle)
c         channel      (implies nofqav)
c         amplitude    (the default for the Y axis)
c         phase
c         real
c         imaginary
c         rms          Theoretical rms noise.
c
c@ xrange
c       Plot range in the x-direction
c         If axis = uvdistance            [kilo-lambda;   2 values]
c         If axis = time                  [dd,hh,mm,ss.s; 8 values]
c         If axis = amplitude, real, imag [natural units; 2 values]
c         If axis = phase                 [degrees;       2 values]
c         If axis = hangle                [hh,mm,ss.s;    6 values]
c         If axis = rms                   [flux units;    2 values]
c         If axis = lst                   [decimal hours; 2 values]
c         If axis = freq                  [GHz;           2 values]
c
c       For axis types other than 'time' or 'hangle', one or other of
c       the limits may be set with the other self-scaled by specifying
c       the lower limit as 'min' and the upper as 'max' (or simply
c       omitting it).  For example,
c
c         xrange=min,0
c
c       self-scales the lower limit while pinning the upper limit to
c       zero, whereas either of the following
c
c         xrange=0,max
c         xrange=0
c
c       set the lower limit to zero while self-scaling the upper limit.
c
c       Default is to self-scale both limits.
c
c@ yrange
c       Plot range for the y-axis as for the x-axis.  The default is to
c       self-scale.  For amplitude type plots you can greatly reduce the
c       number of points to plot by using something like yrange=0.3 to
c       cut out noise.
c
c@ options
c       Task enrichment parameters.  Several can be given, separated by
c       commas.  Minimum match is used.  Possible values are:
c         nobase  Normally BLFLAG plots a single baseline at a time.
c                 This option causes all baselines to be plotted on
c                 a single plot.
c         selgen  Generate a file appropriate for selecting the bad
c                 data (via a "select" keyword).  The output is a text
c                 file called "blflag.select".
c         noapply Do not apply the flagging.
c         rms     When processing spectra, blflag normally plots the
c                 mean value of the spectra.  Using options=rms causes
c                 it to plot the rms value instead.
c         scalar  When processing spectra, blflag normally forms an
c                 average value by vector averaging.  This option
c                 causes it to generate the scalar average.  It should
c                 be used with significant caution.
c         nofqaver Do not average spectra - the resulting number of
c                 points may be too large to handle.  Use select to
c                 break up the data in time ranges or use yrange to
c                 exclude noise.
c       The following options can be used to disable calibration.
c         nocal   Do not apply antenna gain calibration.
c         nopass  Do not apply bandpass correction.
c         nopol   Do not apply polarisation leakage correction.
c
c$Id: blflag.for,v 1.16 2011/04/12 05:34:40 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'

c     Before increasing MAXDAT, check that it does not cause linking to
c     fail with truncated relocations.
      integer    MAXDAT
      parameter (MAXDAT = 32*1024*1024)

      logical   havebl(MAXBASE), noapply, nobase, nofqaver, rms,
     *          scalar, selgen
      integer   ant1, ant2, bl, blIdxp, i, length, lIn, nBl, nBlIdx,
     *          nDat, nEdit, nPol, pCorr, pCorr1, pCorr2, pFlags, pNpnt,
     *          pVis
      real      xmax, xmin, ymax, ymin
      double precision time0
      character device*64, title*32, uvflags*12, val*16, version*72,
     *          xaxis*12, yaxis*12

c     Data store 768MiB (24*MAXDAT bytes).
      integer   blDat(MAXDAT), blIdx(MAXDAT), chDat(MAXDAT)
      real      tDat(MAXDAT), xDat(MAXDAT), yDat(MAXDAT)

      external  itoaf, len1, pgbeg, uvDatOpn, versan
      logical   uvDatOpn
      integer   len1, pgbeg
      character itoaf*10, versan*72
c-----------------------------------------------------------------------
      version = versan('blflag',
     *                 '$Revision: 1.16 $',
     *                 '$Date: 2011/04/12 05:34:40 $')

c     Get the input parameters.
      call keyini
      call keya('device',device,' ')
      if (device.eq.' ') call bug('f','A PGPLOT device must be given')
      call getAxis(xaxis,yaxis)
      call getOpt(nobase,selgen,noapply,rms,scalar,nofqaver,
     *  uvflags)
      if (xaxis.eq.'channel' .or. yaxis.eq.'channel') nofqaver=.true.

c     Get axis ranges
      call getRng('xrange', xaxis, xmin, xmax)
      call getRng('yrange', yaxis, ymin, ymax)
      call uvDatInp('vis',uvflags)
      call keyfin

c     Set the default polarisation type if needed.
      call uvDatGti('npol',nPol)
      if (nPol.eq.0) call uvDatSet('stokes',0)

c     Open the input data.
      if (.not.uvDatOpn(lIn)) call bug('f','Error opening input')

c     Open the plot device.
      if (pgbeg(0,device,1,1).ne.1) then
        call pgldev
        call bug('f','Unable to open PGPLOT device')
      endif
      call pgqinf('CURSOR',val,length)
      if (val.eq.'NO') call bug('f','PGPLOT device is not interactive')
      call pgask(.false.)

c     Allocate memory for getDat.
      call memAlloc(pFlags, MAXCHAN, 'l')
      call memAlloc(pNpnt,  MAXCHAN, 'i')
      call memAlloc(pCorr,  MAXCHAN, 'c')
      call memAlloc(pCorr1, MAXCHAN, 'c')
      call memAlloc(pCorr2, MAXCHAN, 'c')
      call memAlloc(pVis,   MAXCHAN, 'c')

c     Get the data.
      call getDat(lIn,rms,scalar,nofqaver,xaxis,yaxis,xmin,xmax,
     *  ymin,ymax,MAXCHAN,memL(pFlags),memI(pNpnt),memC(pCorr),
     *  memC(pCorr1),memC(pCorr2),memC(pVis),MAXBASE,havebl,MAXDAT,nDat,
     *  blDat,chDat,time0,tDat,xDat,yDat)
      call uvDatCls
      call output('Number of points read: '//itoaf(nDat))
      if (nDat.eq.0) call bug('f','No points to flag')

c     Free memory (in reverse order).
      call memFree(pVis,   MAXCHAN, 'c')
      call memFree(pCorr2, MAXCHAN, 'c')
      call memFree(pCorr1, MAXCHAN, 'c')
      call memFree(pCorr,  MAXCHAN, 'c')
      call memFree(pNpnt,  MAXCHAN, 'i')
      call memFree(pFlags, MAXCHAN, 'l')

c     Loop over the baselines.
      call output('Entering interactive mode ...')
      nEdit  = 0
      blIdxp = 1
      if (nobase) then
        do i = 1, nDat
          blIdx(i) = i
        enddo

        call edit(xaxis,yaxis,'All baselines',nDat,blDat,chDat,tDat,
     *    xDat,yDat,nDat,blIdx,nEdit)
        nBlIdx = nDat

      else
        bl = 0
        do ant2 = 1, MAXANT
          do ant1 = 1, ant2
            bl = bl + 1
            if (havebl(bl)) then
              title = 'Baseline ' // itoaf(ant1)
              length = len1(title)
              title(length+1:) = '-' // itoaf(ant2)
              call getIdx(bl,nDat,blDat,nBl,blIdx(blIdxp))
              if (nBl.gt.0) then
                call edit(xaxis,yaxis,title,nDat,blDat,chDat,tDat,xDat,
     *            yDat,nBl,blIdx(blIdxp),nEdit)
                blIdxp = blIdxp + nBl
              endif
            endif
          enddo
        enddo

        nBlIdx = blIdxp - 1
        if (nBlIdx.ne.nDat) call bug('w',
     *    'Baseline index count check failed.')
      endif

      call pgend

c     Generate the "blflag.select" file, if needed.
      if (selgen) then
        if (nEdit.eq.0) then
          call bug('w','No edit commands to write out!')
        else
          call doSelGen(nDat,blDat,chDat,time0,tDat,nBlIdx,blIdx)
        endif
      endif

c     Apply the changes.
      if (nEdit.gt.0 .and. .not.noapply) then
        call output('Applying the flagging ...')
        call uvDatRew
        call uvDatSet('disable',0)
        if (.not.uvDatOpn(lIn)) call bug('f','Error reopening input')
        call flagApp(lIn,nDat,blDat,chDat,time0,tDat,nBlIdx,blIdx,
     *    version)
        call uvDatCls
      endif

      end

c***********************************************************************

      subroutine getAxis(xaxis,yaxis)

      character xaxis*(*), yaxis*(*)
c-----------------------------------------------------------------------
      integer NAX
      parameter (NAX=10)
      integer n
      character axes(NAX)*12
      data axes/'amplitude   ','phase       ',
     *          'real        ','imaginary   ',
     *          'time        ','uvdistance  ',
     *          'lst         ','hangle      ',
     *          'rms         ','channel     '/
c-----------------------------------------------------------------------
      call keymatch('axis',NAX,axes,1,xaxis,n)
      if (n.eq.0) xaxis = 'time'
      call keymatch('axis',NAX,axes,1,yaxis,n)
      if (n.eq.0) yaxis = 'amplitude'

      end

c***********************************************************************

      subroutine getOpt(nobase,selgen,noapply,rms,scalar,nofqaver,
     *  uvflags)

      logical   nobase, selgen, noapply, rms, scalar, nofqaver
      character uvflags*(*)
c-----------------------------------------------------------------------
c  Get extra processing options.
c-----------------------------------------------------------------------
      integer    NOPTS
      parameter (NOPTS=9)

      logical   present(NOPTS)
      character opts(NOPTS)*8

      data opts/'nobase  ','nocal   ','nopass  ','nopol   ',
     *          'selgen  ','noapply ','rms     ','scalar  ',
     *          'nofqaver'/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)

      nobase = present(1)
      selgen = present(5)
      noapply= present(6)
      rms    = present(7)
      scalar = present(8)
      nofqaver=present(9)
      if (scalar .and. rms)
     *  call bug('f','Options scalar and rms cannot be used together')
      uvflags = 'sdlwb'
      if (.not.present(2)) uvflags(6:6) = 'c'
      if (.not.present(3)) uvflags(7:7) = 'f'
      if (.not.present(4)) uvflags(8:8) = 'e'

      end

c***********************************************************************

      subroutine getRng(keyw, axis, rmin, rmax)

      character keyw*(*), axis*(*)
      real      rmin, rmax
c-----------------------------------------------------------------------
c  Get the axis ranges given by the user
c
c  Input
c    keyw     Keyword to get from user
c    axis     Axis type
c  Output
c    rmin,max Range in appropriate units
c-----------------------------------------------------------------------
      logical ok
      integer il, len1, nt, s
      real trange(8)
      character cval*64
c-----------------------------------------------------------------------
      il = len1(keyw)
      if (axis.eq.'time') then
        call mkeyr(keyw(1:il), trange, 8, nt)
        if (nt.gt.0) then
          if (nt.ne.8) then
            call bug('f',
     *        'You must specify 8 numbers for the time range')
          else
c           Convert to seconds.
            rmin = 24.0*3600.0*trange(1) + 3600.0*trange(2) +
     *                    60.0*trange(3) + trange(4)
            rmax = 24.0*3600.0*trange(5) + 3600.0*trange(6) +
     *                    60.0*trange(7) + trange(8)
          endif
        else
          rmin = -1e32
          rmax =  1e32
        endif

      else if (axis.eq.'hangle') then
        call mkeyr(keyw(1:il), trange, 6, nt)
        if (nt.gt.0) then
          if (nt.ne.6) then
            call bug('f',
     *        'You must specify 6 numbers for the hangle range')
          else
c           Convert to seconds.
            s = 1
            if (trange(1).lt.0.0) s = -1
            rmin = 3600.0*abs(trange(1)) + 60.0*trange(2) + trange(3)
            rmin = s * rmin

            s = 1
            if (trange(4).lt.0.0) s = -1
            rmax = 3600.0*abs(trange(4)) + 60.0*trange(5) + trange(6)
            rmax = s * rmax
          endif
        else
          rmin = -1e32
          rmax =  1e32
        endif

      else
        call keya(keyw(:il), cval, 'min')
        if (cval.eq.'min') then
          rmin = -1e32
        else
          call atorf(cval, rmin, ok)
          if (.not.ok) then
            cval = 'Conversion error decoding parameter ' // keyw(:il)
            call bug('f', cval)
          endif
        endif

        call keya(keyw(:il), cval, 'max')
        if (cval.eq.'max') then
          rmax = 1e32
        else
          call atorf(cval, rmax, ok)
          if (.not.ok) then
            cval = 'Conversion error decoding parameter ' // keyw(:il)
            call bug('f',cval)
          endif
        endif

c       Because atorf actually uses atodf and conversion between
c       double and real may introduce rounding errors.
        if (-1.000001e32.lt.rmin .and. rmin.lt.-0.999999e32) then
          rmin = -1e32
        endif

        if (0.999999e32.lt.rmax .and. rmax.lt.1.000001e32) then
          rmax =  1e32
        endif
      endif

      end

c***********************************************************************

      subroutine getDat(lIn,rms,scalar,nofqaver,xaxis,yaxis,xmin,xmax,
     *  ymin,ymax,MAXCHAN,flags,npnt,corr,corr1,corr2,visDat,
     *  MAXBASE,havebl,MAXDAT,nDat,blDat,chDat,time0,tDat,xDat,yDat)

      integer   lIn
      logical   rms, scalar, nofqaver
      character xaxis*(*), yaxis*(*)
      real      xmin, xmax, ymin, ymax

      integer   MAXCHAN
      logical   flags(MAXCHAN)
      integer   npnt(MAXCHAN)
      complex   corr(MAXCHAN), corr1(MAXCHAN), corr2(MAXCHAN),
     *          visDat(MAXCHAN)

      integer   MAXBASE
      logical   havebl(MAXBASE)

      integer   MAXDAT, nDat, blDat(MAXDAT), chDat(MAXDAT)
      double precision time0
      real      tDat(MAXDAT), xDat(MAXDAT), yDat(MAXDAT)
c-----------------------------------------------------------------------
      double precision TTOL
      parameter (TTOL=1d0/86400d0)

      logical   flush
      integer   ant1, ant2, bl, bnext, chInc, ic, jc, mchan, n, nchan
      real      dTime, temp, uvsq, var, x, y
      double precision lst, preamble(4), ra, time

      external  getVal
      real      getVal
c-----------------------------------------------------------------------
c     Initialise accumulators.
      uvsq = 0.0
      var  = 0.0
      do ic = 1, MAXCHAN
        npnt(ic)  = 0
        corr(ic)  = (0.0,0.0)
        corr1(ic) = (0.0,0.0)
        corr2(ic) = (0.0,0.0)
      enddo

      do bl = 1, MAXBASE
        havebl(bl) = .false.
      enddo

      nDat = 0

      if (nofqaver) then
        chInc = 1
      else
        chInc = 0
      endif

c     Let's get going.
      call output('Reading the data ...')
      call uvDatRd(preamble,visDat,flags,MAXCHAN,nchan)
      if (nchan.eq.0) call bug('f','No visibility data found')
      if (nchan.eq.MAXCHAN) call bug('f','Too many channels for me')

      call flagChk(lIn)
      mchan = 1
      time  = preamble(3)
      time0 = int(time - 0.5d0) + 0.5d0
      call BasAnt(preamble(4),ant1,ant2)
      bl    = (ant2*(ant2-1))/2 + ant1
      bnext = bl
      call uvrdvrd(lIn,'lst',lst,0d0)
      call uvrdvrd(lIn,'ra', ra, 0d0)

      flush = .false.
      do while (.true.)
c       Is the baseline within the limit that we can handle?
        if (bl.lt.MAXBASE) then
c         Was a new baseline read?
          if (flush) then
c           Yes, flush the old one.
            do ic = 1, mchan
              if (npnt(ic).gt.0) then
                havebl(bl)  = .true.

                if (nofqaver) then
                  jc = ic
                else
c                 Use 0 to indicate the whole spectrum.
                  jc = 0
                endif

c               Compute the requested x, and y values.
                dTime = real(time - time0)
                x = getVal(rms,scalar,xaxis,dTime,lst,ra,jc,uvsq,var,
     *                npnt(ic),corr(ic),corr1(ic),corr2(ic))
                y = getVal(rms,scalar,yaxis,dTime,lst,ra,jc,uvsq,var,
     *                npnt(ic),corr(ic),corr1(ic),corr2(ic))

c               Store it if within range.
                if (x.ge.xmin .and. x.le.xmax .and.
     *              y.ge.ymin .and. y.le.ymax) then
                  nDat = nDat + 1
                  if (nDat.gt.MAXDAT) call bug('f','Too many points!')

                  blDat(nDat) = bl
                  chDat(nDat) = jc
                  tDat(nDat)  = dTime
                  xDat(nDat)  = x
                  yDat(nDat)  = y
                endif
              endif
            enddo

c           Finished if no visibility read last time.
            if (nchan.le.0) return

c           Reset the accumulators.
            uvsq = 0.0
            var  = 0.0
            do ic = 1, mchan
              npnt(ic)  = 0
              corr(ic)  = (0.0,0.0)
              corr1(ic) = (0.0,0.0)
              corr2(ic) = (0.0,0.0)
            enddo

            call uvrdvrd(lIn,'lst',lst,0d0)
            call uvrdvrd(lIn,'ra', ra, 0d0)
          endif

c         Accumulate Stokes for this baseline.
          jc = 1
          n  = 0
          do ic = 1, nchan
            if (flags(ic)) then
              n = n + 1
              npnt(jc)  = npnt(jc)  + 1
              corr(jc)  = corr(jc)  + visDat(ic)
              corr1(jc) = corr1(jc) + abs(visDat(ic))
              corr2(jc) = corr2(jc) + cmplx(real(visDat(ic))**2,
     *                                     aimag(visDat(ic))**2)
            endif

c           chInc is set to 1 for nofqaver, otherwise 0.
            jc = jc + chInc
          enddo

          if (n.gt.0) then
            call uvDatGtr('variance',temp)
            var  = var  + n * temp
            uvsq = uvsq + n * (preamble(1)*preamble(1) +
     *                         preamble(2)*preamble(2))
          endif
        endif

c       Finished if no visibility read last time.
        if (nchan.le.0) return

c       mchan is the actual number of channels for nofqaver, else 1.
        if (nofqaver) then
          mchan = nchan
        endif

        time = preamble(3)
        bl   = bnext
        call uvDatRd(preamble,visDat,flags,MAXCHAN,nchan)
        if (nchan.gt.0) then
c         Looks like a valid visibility.
          call BasAnt(preamble(4),ant1,ant2)
          bnext = (ant2*(ant2-1))/2 + ant1

          flush = abs(preamble(3)-time).gt.TTOL .or. bnext.ne.bl
        else
c         Last visibility has been read, flush the accumulators.
          flush = .true.
        endif
      enddo

      end

c***********************************************************************

      subroutine flagChk(lIn)

      integer lIn
c-----------------------------------------------------------------------
c  Check that the user's linetype is not going to cause the flagging
c  routine to vomit when the flagging is applied.
c-----------------------------------------------------------------------
      integer CHANNEL,WIDE
      parameter (CHANNEL=1,WIDE=2)
      double precision line(6)
c-----------------------------------------------------------------------
      call uvinfo(lIn,'line',line)
      if (nint(line(1)).ne.CHANNEL .and. nint(line(1)).ne.WIDE)
     *  call bug('f','Can only flag "channel" or "wide" linetypes')
      if (nint(line(4)).ne.1)
     *  call bug('f','Cannot flag when the linetype width is not 1')

      end

c***********************************************************************

      real function getVal(rms,scalar,axis,dTime,lst,ra,chan,uvsq,var,
     *  npnt,corr,corr1,corr2)

      logical   rms, scalar
      character axis*(*)
      real      dTime
      double precision lst, ra
      integer   chan
      real      uvsq, var
      integer   npnt
      complex   corr, corr1, corr2
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision dtemp
      complex   visDat
c-----------------------------------------------------------------------
      if (rms) then
        visDat = cmplx(sqrt( real(corr2)/npnt -  real(corr/npnt)**2),
     *                 sqrt(aimag(corr2)/npnt - aimag(corr/npnt)**2))
      else if (scalar) then
        visDat = corr1/npnt
      else
        visDat = corr/npnt
      endif

      if (axis.eq.'real') then
        getVal = real(visDat)
      else if (axis.eq.'imaginary') then
        getVal = aimag(visDat)
      else if (axis.eq.'amplitude') then
        getVal = abs(visDat)
      else if (axis.eq.'phase') then
        getVal = atan2(aimag(visDat),real(visDat))*R2D
      else if (axis.eq.'uvdistance') then
        getVal = 0.001 * sqrt(uvsq/npnt)
      else if (axis.eq.'rms') then
        getVal = sqrt(var/npnt)
      else if (axis.eq.'time') then
        getVal = 86400.0*dTime
      else if (axis.eq.'lst') then
        getVal = lst*86400d0/DTWOPI
      else if (axis.eq.'hangle') then
        dtemp = lst - ra
        if (dtemp.gt.DPI) then
          dtemp = dtemp - DTWOPI
        else if (dtemp.lt.-DPI) then
          dtemp = dtemp + DTWOPI
        endif
        getVal = dtemp*86400d0/DTWOPI
      else if (axis.eq.'channel') then
        getVal = chan
      else
        getVal = 0.0
        call bug('f','I should never get here')
      endif

      end

c***********************************************************************

      subroutine getIdx(bl,NDAT,blDat,nBl,blIdx)

c     Given.
      integer   bl, NDAT, blDat(NDAT)

c     Returned.
      integer   nBl, blIdx(*)
c-----------------------------------------------------------------------
c  Construct an index into the data for the specified baseline.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      nBl = 0
      do i = 1, NDAT
        if (blDat(i).eq.bl) then
          nBl = nBl + 1
          blIdx(nBl) = i
        endif
      enddo

      end

c***********************************************************************

      subroutine edit(xaxis,yaxis,title,NDAT,blDat,chDat,tDat,xDat,yDat,
     *  NBL,blIdx,nEdit)

c     Given.
      character xaxis*(*), yaxis*(*), title*(*)
      integer   NDAT, blDat(NDAT), chDat(NDAT)
      real      tDat(NDAT), xDat(NDAT), yDat(NDAT)
      integer   NBL

c     Given and returned.
      integer   blIdx(NBL), nEdit
c-----------------------------------------------------------------------
      logical   more
      integer   i, nEdit0
      real      xmax, xmin, xs, xv, ys, yv
      character mode*1
c-----------------------------------------------------------------------
      nEdit0 = nEdit
      mode = 'c'
      more = .true.
      do while (more)
        call lcase(mode)
        if (mode.eq.'q') then
          call pgend
          call bug('f','Aborting as requested')
        else if (mode.eq.'c') then
          nEdit = nEdit0
          xmin = 0.0
          xmax = 0.0
          do i = 1, NBL
            blIdx(i) = abs(blIdx(i))
          enddo

          call draw(xaxis,yaxis,title,xmin,xmax,NDAT,xDat,yDat,NBL,
     *      blIdx,xs,ys)

        else if (mode.eq.'r') then
          call draw(xaxis,yaxis,title,xmin,xmax,NDAT,xDat,yDat,NBL,
     *      blIdx,xs,ys)

        else if (mode.eq.'h' .or. mode.le.' ' .or. mode.eq.'?') then
          call output('-------------------------------------')
          call output('Single key commands are')
          call output(' Left-button  Delete nearest point')
          call output(' Right-button Next baseline')
          call output(' <CR>  Help')
          call output(' ?     Help')
          call output(' a     Delete nearest point')
          call output(' c     Clear flagging of this baseline')
          call output(' h     Help -- these messages')
          call output(' p     Define and delete polygonal region')
          call output(' q     Quit -- discarding edits')
          call output(' r     Redraw')
          call output(' u     Unzoom')
          call output(' x     Next baseline')
          call output(' z     Zoom in')
          call output('-------------------------------------')

        else if (mode.eq.'u') then
          xmin = 0
          xmax = 0
          call draw(xaxis,yaxis,title,xmin,xmax,NDAT,xDat,yDat,NBL,
     *      blIdx,xs,ys)

        else if (mode.eq.'x') then
          more = .false.

        else if (mode.eq.'z') then
          call output('Click on left-hand edge of the zoomed region')
          call pgcurs(xmin,yv,mode)
          call output('Click on right-hand edge of the zoomed region')
          call pgcurs(xmax,yv,mode)
          call draw(xaxis,yaxis,title,xmin,xmax,NDAT,xDat,yDat,NBL,
     *      blIdx,xs,ys)

        else if (mode.eq.'a') then
          call nearest(xv,yv,xs,ys,NDAT,blDat,chDat,tDat,xDat,yDat,
     *      NBL,blIdx,nEdit)

        else if (mode.eq.'p') then
          call region(NDAT,blDat,chDat,tDat,xDat,yDat,NBL,blIdx,nEdit)

        else
          call bug('w','Unrecognised keystroke - use h for help')
        endif

        if (more) call pgcurs(xv,yv,mode)
      enddo

      end

c***********************************************************************

      subroutine draw(xaxis,yaxis,title,xmin,xmax,NDAT,xDat,yDat,NBL,
     *  blIdx,xs,ys)

      character xaxis*(*), yaxis*(*), title*(*)
      real      xmin, xmax
      integer   NDAT
      real      xDat(NDAT), yDat(NDAT)
      integer   NBL, blIdx(NBL)
      real      xs, ys
c-----------------------------------------------------------------------
      integer   i, k
      real      xhi, xlo, yhi, ylo
      character xflags*12, xtitle*32, yflags*12, ytitle*32
c-----------------------------------------------------------------------
c     Determine the min and max values.
      call pgbbuf
      call setUp(xaxis,NDAT,xDat,NBL,blIdx,xlo,xhi,xtitle,xflags)
      if (xmin.lt.xmax) then
        xlo = xmin
        xhi = xmax
      endif
      call setUp(yaxis,NDAT,yDat,NBL,blIdx,ylo,yhi,ytitle,yflags)

      xs = 1/(xhi-xlo)**2
      ys = 1/(yhi-ylo)**2

c     Draw the plot.
      call pgsci(1)
      call pgpage
      if ((xaxis.eq.'real' .or. xaxis.eq.'imaginary') .and.
     *    (yaxis.eq.'real' .or. yaxis.eq.'imaginary')) then
        call pgvstd
        call pgwnad(xlo,xhi,ylo,yhi)
      else
        call pgvstd
        call pgswin(xlo,xhi,ylo,yhi)
      endif
      call pgtbox(xflags,0,0.0,yflags,0,0.0)
      call pglab(xtitle,ytitle,title)

c     Plot all the good data.
      do i = 1, NBL
        if (blIdx(i).gt.0) then
          k = blIdx(i)
          call pgpt(1,xDat(k),yDat(k),1)
        endif
      enddo

c     Change the colour to red.
      call pgsci(2)
      call pgebuf

      end

c***********************************************************************

      subroutine setUp(axis,NDAT,xyDat,NBL,blIdx,lo,hi,title,flags)

      character axis*(*)
      integer   NDAT
      real      xyDat(NDAT)
      integer   NBL, blIdx(NBL)

      real      lo, hi
      character title*(*), flags*(*)
c-----------------------------------------------------------------------
      integer   i, k
      real      absmax, delta, xymin, xymax
c-----------------------------------------------------------------------
      xymin =  1e30
      xymax = -1e30
      do i = 1, NBL
        if (blIdx(i).gt.0) then
          k = blIdx(i)
          xymin = min(xymin,xyDat(k))
          xymax = max(xymax,xyDat(k))
        endif
      enddo

      delta = 0.05*(xymax-xymin)
      absmax = max(abs(xymax),abs(xymin))
      if (delta.le.1e-4*absmax) delta = 0.01*absmax
      if (delta.eq.0) delta = 1
      lo = xymin - delta
      hi = xymax + delta

      if (axis.eq.'time' .or. axis.eq.'lst' .or. axis.eq.'hangle') then
        flags = 'BCNSTHZ0'
      else
        flags = 'BCNST'
      endif

      if (axis.eq.'uvdistance') then
        title = '(u\u2\d+v\u2\d)\u1/2\d (k\gl)'
      else if (axis.eq.'phase') then
        title = 'Phase (degrees)'
      else if (axis.eq.'rms') then
        title = 'Theoretical rms noise'
      else
        title = axis
        call ucase(title(1:1))
      endif

      end

c***********************************************************************

      subroutine nearest(xv,yv,xs,ys,NDAT,blDat,chDat,tDat,xDat,yDat,
     *  NBL,blIdx,nEdit)

c     Given.
      real      xv, yv, xs, ys
      integer   NDAT, blDat(NDAT), chDat(NDAT)
      real      tDat(NDAT), xDat(NDAT), yDat(NDAT)
      integer   NBL

c     Given and returned.
      integer   blIdx(NBL), nEdit
c-----------------------------------------------------------------------
      integer   i, j, k
      real      dsq, dsqmin
c-----------------------------------------------------------------------
      j = 0
      dsqmin = 0.0
      do i = 1, NBL
        if (blIdx(i).gt.0) then
          k = blIdx(i)
          dsq = xs*(xDat(k)-xv)**2 + ys*(yDat(k)-yv)**2
          if (j.eq.0 .or. dsq.lt.dsqmin) then
            j = i
            dsqmin = dsq
          endif
        endif
      enddo

      if (j.eq.0) then
        call bug('w','No points left to edit')
      else
        nEdit = nEdit + 1
        k = blIdx(j)
        blIdx(j) = -blIdx(j)
        call pgpt(1,xDat(k),yDat(k),1)
      endif

      end

c***********************************************************************

      subroutine region(NDAT,blDat,chDat,tDat,xDat,yDat,NBL,blIdx,nEdit)

      integer   NDAT, blDat(NDAT),chDat(NDAT)
      real      tDat(nDAT), xDat(NDAT), yDat(NDAT)
      integer   NBL, blIdx(NBL), nEdit
c-----------------------------------------------------------------------
      integer    MAXV
      parameter (MAXV=100)

      logical   within
      integer   i, j, k, nv
      real      xv(MAXV+1), yv(MAXV+1)
c-----------------------------------------------------------------------
      call output('Define a region - exit with x')
      call pgsci(3)
      nv = 0
      call pgolin(MAXV,nv,xv,yv,17)
      if (nv.lt.3) then
        call bug('w','Too few vertices')
      else if (nv.gt.MAXV) then
        call bug('w','Too many vertices for me!')
      else
        call pgsfs(2)
        call pgslw(2)
        call pgpoly(nv,xv,yv)
        call pgslw(1)
        call pgsci(2)

        xv(nv+1) = xv(1)
        yv(nv+1) = yv(1)

c       Find all points that are within the poly.
        call pgbbuf
        do i = 1, NBL
          k = blIdx(i)
          if (k.gt.0) then
            within = .false.
            do j = 1, nv
              if ((xDat(k)-xv(j))*(xDat(k)-xv(j+1)).le.0 .and.
     *         abs(xDat(k)-xv(j))*(yv(j+1)-yv(j)).lt.
     *         abs(xv(j+1)-xv(j))*(yDat(k)-yv(j))) then
                within = .not.within
              endif
            enddo

            if (within) then
              nEdit = nEdit + 1
              blIdx(i) = -blIdx(i)
              call pgpt(1,xDat(k),yDat(k),1)
            endif
          endif
        enddo
        call pgebuf
      endif

      end

c***********************************************************************

      subroutine doSelGen(NDAT,blDat,chDat,time0,tDat,NBLIDX,blIdx)

      integer   NDAT, blDat(NDAT), chDat(NDAT)
      double precision time0
      real      tDat(NDAT)
      integer   NBLIDX, blIdx(NBLIDX)
c-----------------------------------------------------------------------
c  Generate a file of select commands.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      double precision TTOL
      parameter (TTOL=1d0/86400d0)

      logical   warn
      integer   i, i1(MAXBASE),i2(MAXBASE), iostat, j, k, length, lu
      double precision time
      character line*80, time1*24, time2*24

      external  itoaf, len1
      integer   len1
      character itoaf*4
c-----------------------------------------------------------------------
c     Generate a table to allow translation from baseline number to
c     antenna pairs.
      k = 0
      do j = 1, MAXANT
        do i = 1, j
          k = k + 1
          i1(k) = i
          i2(k) = j
        enddo
      enddo

      call txtopen(lu,'blflag.select','new',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening output text file blflag.select')
        call bugno('f',iostat)
      endif

      warn = .true.
      do i = 1, NBLIDX
        if (blIdx(i).lt.0) then
c         This data was flagged.
          k = abs(blIdx(i))
          if (chDat(k).eq.0) then
            time = time0 + tDat(k)
            call julday(time-TTOL,'H',time1)
            call julday(time+TTOL,'H',time2)
            line = 'ant('//itoaf(i1(blDat(k)))
            length = len1(line)
            line(length+1:) = ')('//itoaf(i2(blDat(k)))
            length = len1(line)
            line(length+1:) = '),time('//time1
            length = len1(line)
            line(length+1:) = ','//time2
            length = len1(line)
            line(length+1:) = ')'
            length = length + 1
            if (i.ne.NBLIDX) then
              line(length+1:length+3) = ',or'
              length = length + 3
            endif

            call txtwrite(lu,line,length,iostat)
            if (iostat.ne.0) then
              call bug('w','Error writing to text file blflag.select')
              call bugno('f',iostat)
            endif

          else
            if (warn) then
              call bug('w',
     *          'Omitting channel specific flags in text file')
              warn=.false.
            endif
          endif
        endif
      enddo

      call txtclose(lu)

      end

c***********************************************************************

      subroutine flagApp(lIn,NDAT,blDat,chDat,time0,tDat,NBLIDX,blIdx,
     *  version)

      integer   lIn, NDAT, blDat(NDAT),chDat(NDAT)
      double precision time0
      real      tDat(NDAT)
      integer   NBLIDX, blIdx(NBLIDX)
      character version*(*)
c-----------------------------------------------------------------------
c  Apply flagging to the dataset.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      double precision TTOL
      parameter (TTOL=1d0/86400d0)

      logical   chflag, flags(MAXCHAN), match
      integer   ant1, ant2, bl, i, j, k, nchan, ncorr, nflag
      real      dTime
      double precision preamble(4)
      complex   visDat(MAXCHAN)
      character line*64

      external  itoaf
      character itoaf*10
c-----------------------------------------------------------------------
      nflag = 0
      ncorr = 0

      call uvDatRd(preamble,visDat,flags,MAXCHAN,nchan)

      do while (nchan.gt.0)
        ncorr = ncorr + nchan
        call basant(preamble(4),ant1,ant2)
        bl = (ant2*(ant2-1))/2 + ant1
        dTime = real(preamble(3) - time0)

c       Search for this integration.
        chflag = .false.
        do i = 1, NBLIDX
          if (blIdx(i).lt.0) then
c           This data was flagged.
            k = abs(blIdx(i))
            match = blDat(k).eq.bl .and. abs(tDat(k)-dTime).le.TTOL

            if (match) then
c             Flag bad channels.
              if (chDat(k).eq.0) then
c               Flag the whole spectrum.
                do j = 1, nchan
                  if (flags(j)) then
                    flags(j) = .false.
                    nflag  =  nflag + 1
                    chflag = .true.
                  endif
                enddo
              else
c               Flag the particular channel.
                if (flags(chDat(k))) then
                  flags(chDat(k)) = .false.
                  nflag  =  nflag + 1
                  chflag = .true.
                endif
              endif
            endif
          endif
        enddo

        if (chflag) call uvflgwr(lIn,flags)

c       Go back for more.
        call uvDatRd(preamble,visDat,flags,MAXCHAN,nchan)
      enddo

c     Write history.
      call hisOpen(lIn,'append')
      line = 'BLFLAG: Miriad '//version
      call hisWrite(lIn,line)
      call hisInput(lIn,'BLFLAG')
      call hisWrite(lIn,'BLFLAG: Number of correlations flagged: '//
     *  itoaf(nflag))
      call hisClose(lIn)

c     Report how much we have done.
      call output('Total number of correlations:   '//itoaf(ncorr))
      call output('Number of correlations flagged: '//itoaf(nflag))

      end
