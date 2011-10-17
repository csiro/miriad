      program gpcal

c= Gpcal -- Gain/phase/polarization calibration of dual feed data.
c& rjs nebk
c: calibration
c+
c       Gpcal is a MIRIAD task that determines calibration corrections
c       (both antenna gains and instrumental polarisation
c       characteristics) for an array with dual feeds, from an
c       observation of a point source.  The source can be polarised,
c       with unknown polarisation characteristics.  Though the source
c       may be strongly polarized, the instrumental polarisation errors
c       are assumed to be small (of order at most a few percent).
c
c       Normally GPCAL writes the solutions as a gains table (item
c       'gains') and a polarization leakage table (item 'leakage').
c
c       With nfbin>1, GPCAL writes additional tables 'gainsf' and
c       'leakagef' with solutions for frequency binned data.
c       Not many other tasks currently read these tables, but
c       they will be applied in preference to the single frequency
c       versions if present. 
c
c       GPCAL can handle either dual linear or dual circular feeds.
c       However by default it expects dual linears -- you must use
c       options=circular so switch it to circular mode.  Also the
c       terminology in this document is aimed at linears (e.g. we talk
c       of "XY phase" -- for dual circulars this is really "RL phase").
c
c       Note that the user specifies which parameters are to be solved
c       for.  In the case of leakages and xyphases, GPCAL will check for
c       the existence of items "leakage" and "gains" in the input data-
c       set.  If present, these will be used as the initial estimates of
c       these parameters.  If you are not solving for these parameters,
c       they will be held at their initial value through the solution
c       process.  After converging on a solution, and if the xyphase
c       offsets or leakage parameters have been modified, GPCAL will
c       write out their current values to the appropriate items.
c
c       Conventions: Unfortunately there has been a number of changes in
c       the "sign conventions" used within Miriad.  For a discussion of
c       the conventions, past sign errors and how they affect you, see
c       the memo "The Sign of Stokes-V, etc." by Bob Sault.
c
c@ vis
c       Input visibility data file. The data should be either raw linear
c       or raw circular polarisations. No default. The visibility data
c       must be in time order.
c@ select
c       Standard uv selection. Default is all data.
c@ line
c       Standard line-type specification.  Multiple channels can be
c       given.  Generally it is better to give multiple channels, rather
c       than averaging them into a "channel-0".  The default is all the
c       channel data (or all the wide data, if there is no channel
c       data).
c@ flux
c       The values of the I,Q,U,V Stokes parameters. If no values are
c       given, and it is a source known to GPCAL, GPCAL uses its known
c       flux as the default.  If GPCAL does not know the source, the
c       flux is determined by assuming that the rms gain amplitude is 1.
c       If the option "qusolve" is used, the given fluxes for Q and U
c       are used as the initial estimates.  Also see the "oldflux"
c       option.
c@ refant
c       The reference antenna.  Default is 3.  The reference antenna
c       must be present throughout the observation.  Any solution
c       intervals where the reference antenna is missing are discarded.
c@ minants
c       The minimum number of antenna that must be present before a
c       solution is attempted. Default is 2.
c@ interval
c       This gives one or two numbers, both given in minutes, both being
c       used to determine the extents of an amplitude calibration
c       solution interval.  The first gives the max length of a solution
c       interval.  The second gives the max gap size in a solution
c       interval.  A new solution interval is started when either the
c       max time length is exceeded, or a gap larger than the max gap is
c       encountered.  The default is max length is 5 minutes, and the
c       max gap size is the same as the max length.  The polarisation
c       characteristics are assumed to be constant over the observation.
c@ nfbin
c       The number of frequency bins. The default is 1. Use nfbin>1 to
c       solve for variation across the band in the gain and leakage 
c       parameters.
c       Works best for uv files with a single spectral window, i.e., 
c       after uvsplit.      
c@ tol
c       Error tolerance. The default is 0.001 which should be adequate.
c@ xyphase
c       Generally the use of this parameter has been superceded.
c
c       Initial estimate of the XY phase of each antenna. The default is
c       0 for all antennas. If the XY phase has not been applied to the
c       data, then it is important that this parameter is set correctly,
c       particularly for the reference antenna.
c@ options
c       These options determine what GPCAL solves for.  There are many
c       permutations, the more obscure or useless of which are not
c       supported.  The option values are used to turn on or off some of
c       the solvers.  Several options can be given, separated by commas.
c       Minimum match is used.  Some combinations of these options are
c       not supported.
c         xyvary     The XY phase varies with time.  By default the XY
c                    phase is assumed to remain constant.
c         qusolve    Solve for Q and U fluxes. Good parallactic
c                    angle coverage is required for this.
c         oldflux    This causes GPCAL to use a pre-August 1994 ATCA
c                    flux density scale.  See the help on "oldflux" for
c                    more information.
c         nopol      Do not solve for the instrumental polarisation
c                    leakage characteristics. The default is to solve
c                    for the polarisation leakages on all feeds except
c                    the X feed of the reference antenna.
c         noxy       Do not solve for any XY phase offset.  The default
c                    is to solve for the XY phase offset on all antennas
c                    except for the reference antenna.
c         nopass     Do not apply bandpass correction. The default is
c                    to apply bandpass correction if possible. This is
c                    rarely useful. Use with caution.
c         noamphase  Do not solve for the amplitude and phase. The
c                    default is to solve for amplitude and phase. This
c                    option is rarely useful.
c         linear     Expect/handle data from feeds that are linearly
c                    polarised. This is the default.
c         circular   Expect/handle data from feeds that are circularly
c                    polarised.
c
c       The following are options for advanced users, and should be used
c       with forethought and caution.
c         reset      If calibration tables (leakage and gains) are
c                    present within the input dataset, GPCAL usually
c                    uses these in determining default XY phases,
c                    leakages and flux scale.  This is usually the
c                    desired behaviour.  The "reset" option suppresses
c                    this behaviour, and starts GPCAL with a clean
c                    slate.
c         xyref      Solve for the XY phase of the reference antenna.
c                    To do this, the source must be linearly polarized
c                    and you must have reasonable parallactic angle
c                    coverage.  This option can be used with "noxy", in
c                    which case GPCAL solves for the offset of the
c                    reference antenna.
c         polref     Solve for the instrumental polarization leakage of
c                    the X feed on the reference antenna.  This can be
c                    combined with "nopol", in which case GPCAL solves
c                    for X feed of the reference antenna only.
c         vsolve     Solve for the Stokes-V of the source.  This is only
c                    possible for linear feeds and a preliminary leakage
c                    solution for the observation already exists.  This
c                    preliminary solution must be formed from a
c                    calibrator with known Stokes-V.
c
c$Id: gpcal.for,v 1.12 2011/10/17 03:17:15 wie017 Exp $
c--
c  History:
c    rjs,nebk 1may91 Original version.
c    rjs     17may91 A working version.
c    nebk    25may91 Add some plots while rjs is away
c    nebk    02jun91 Fix convergence when gain only solution, and change
c                    to output all polarizations, even if no leakage
c                    solved for.  Add log file, remove history from
c                    input.
c            03jun91 Change default output file to corrected data.
c                    Add option "NOLEAKCORR"
c    rjs     17jun91 Changed sign conventions.
c    nebk    17jun91 Add Q and U plots with iteration
c    rjs     18jun91 Fudged the equations for Vxx and Vyy when solving
c                    for unknown q and u.
c    rjs     20jun91 Replaced the "strong" option with xyref and polref
c                    options. Minor other changes.
c    rjs     21jun91 Name change and more hacks.
c    rjs     28jun91 Writes gain table correctly.
c    rjs     17jul91 Send Neil home in tears after I deleted his
c                    plotting software from this otherwise excellent
c                    program.  One should note this capability now in
c                    GPPLT, sob.
c    rjs     24jul91 Added RefSolve routines, read xyphase and leakage
c                    tables, use these as the initial guess.
c    rjs     29jul91 Can use options=qusolve,polref together.
c    nebk    05aug91 Implement routines with knowledge about IQUV for
c                    some selected calibrator sources.
c    nebk    13aug91 Put info. back into input history when no output
c                    file.
c    nebk    14aug91 Removed Option=NOLEAKCORR since the use of gain
c                    and leakage tables now makes it redundant
c    nebk    26aug91 Changed polynomials in CALSTOKE for 3C286 & 3C138
c    rjs      1sep91 Elimiated "out" options. Fiddled determining the
c                    end of scans and the interval parameter.
c    rjs      2sep91 Fixed formatting bugs in CalStoke.
c    nebk    03sep91 Use Bob Duncan's polynomial fit for 1934 rather
c                    than mine for consistency with SetJy in AIPS
c                    (although mine fits better I think).  Other fiddles
c                    in CALSTOKE.
c    nebk    17jan92 Add output from GETIQUV to history
c    rjs     28apr92 Fixed bug in initial estimate of amplitude
c                    solutions.  Bug will not effect any results.
c    rjs      4may92 Minor improvement to amplitude solution routine.
c    rjs     23jun92 XYvary option. Estimate XY phase.
c    rjs     22jul92 Get source name earlier.
c    rjs      4aug92 Eliminate xyphases item. Remove calstoke to a
c                    separate file, and change its call sequence.
c    rjs     28aug92 Protected sqrt() from rounding error which made it
c                    go negative.
c    rjs     30oct92 Change minant to minants.
c    rjs     22nov92 Added ntau keyword to output gains file.
c    rjs     16feb93 More decimal places in a message.
c    rjs      9mar93 Qusolve when not solving for leakages.
c    nebk    07jun93 Minor formatting change
c    rjs     24jun93 Hasn't gone anywhere.
c    rjs     28jul93 Fiddle gain scaling if no flux is given.
c    rjs     10aug93 Applies bandpass corrections. Handles multi-channel
c                    data.
c    rjs     18oct93 Doc correction only.
c    rjs     13dec93 Sign convention of V change.
c    rjs     23mar94 Check for existence of closures, and do the
c                    appropriate things if there aren't any.
c    rjs     25mar94 Minor correction to the above change.
c    rjs      3aug94 options=oldflux.
c    rjs     26sep95 Check for ill-determined solution.
c    mchw    08aug96 fix minibug for more antennas.
c    rjs     12may97 Added options=circular.
c    rjs     23jun97 Get gpcal options=nopol,qusolve to work when a
c                    pol'n solution is already present.
c    rjs     15aug97 Change to normalisation, options=vsolve and fiddles
c                    to the doc comments.
c    rjs     22may98 Turn off xyref on early iterations of weakly
c                    polarised source.
c    rjs     19aug98 Changes in ampsol2 and ampsol1 to avoid an SGI
c                    compiler bug.
c    rjs     12oct99 Attempts to perform absolute flux calibration.
c    rjs      7oct04 Set senmodel parameter.
c    rjs     27nov06 Doc correction only.
c    rjs     24apr09 xyphase array was being ignored in some instances.
c    rjs     30apr09 Fix bug in handling of strongly circularly
c                    polarised calibrator when using circularly
c                    polarised feeds.  Implement refsolve capability for
c                    circularly polarised feeds.  Better handling of
c                    effect of large XY phase errors on leakages in the
c                    iteration process.  Add (and correct!) misc
c                    comments to the code.
c    mhw     03sep10 Use mean freq of all data used for flux cal
c    mhw     15feb11 Solve for leakage in frequency bins
c
c  Miscellaneous notes:
c ---------------------
c  The model used within gpcal is that
c
c  Measurement = (Antenna Gains) * (Leakage Process) * Rotation *
c                  (Model Sky)
c
c  The "Gains" and "D" arrays are arrays of corrupting effects (as
c  distinct from the corrections that need to be applied).
c
c  Note the "leakage" item stored with a Miriad visibility dataset is
c  the table of the corrupting influence.  The uvDat routine does a
c  matrix inversion to determine the corrections that it needs to apply.
c  However the "gains" item is a table of corrections to apply, and
c  gpcal (and similar other programs) need to do the inversion from
c  corrupting effect to correction term when creating this table.
c
c  Bugs:
c    * Polarisation solutions when using noamp are wrong!  The equations
c      it solves for have a fudge to account for a bias introduced by
c      the amplitude solver.
c    * Need a constant X/Y gain option.
c-----------------------------------------------------------------------
      include 'gpcal.h'

      integer MAXITER
      parameter (MAXITER=30)

      logical   amphsol, circular, defflux, dopass, notrip, oldflx,
     *          polcal, polref, polsol, qusolve, reset, usepol, useref,
     *          vsolve, xyref, xysol, xyvary
      integer   i, j, k, jmax, maxsoln, minant, nants, nbl, niter, 
     *          nsoln, nxyphase, off, refant, tIn, nfbin, n, fbin
      real      epsi, epsi1, fac, flux(4,0:MAXFBIN), 
     *          oldFlux(4,0:MAXFBIN), pcent,
     *          tol, ttol, xyphase(MAXANT)
      double precision freq(0:MAXFBIN), interval(2)
      character line*80, source*32, uvflags*8, version*72

      logical present(MAXANT)
      integer Count(MAXDBL)
      real    SumS(MAXDBL),SumC(MAXDBL)
      real    SumS2(MAXDBL),SumCS(MAXDBL)
      complex D(2,MAXANT*MAXFBIN),xyp(MAXANT*MAXFBIN)
      complex Gains(2,MAXDANTS)
      complex OldD(2,MAXANT*MAXFBIN),OldGains(2,MAXDANTS)
      complex Vis(4,MAXDBL),xypcorr,xypref
      complex VisCos(4,MAXDBL),VisSin(4,MAXDBL)
      double precision time(MAXDSOLN)

      logical   keyprsnt, uvDatOpn
      character itoaf*3, versan*80
      external  itoaf, keyprsnt, uvDatOpn
c-----------------------------------------------------------------------
      version = versan('gpcal',
     *                 '$Revision: 1.12 $',
     *                 '$Date: 2011/10/17 03:17:15 $')
c
c  Get inputs.
c
      call keyini
      call GetOpt(dopass,amphsol,polsol,xysol,xyref,xyvary,polref,
     *  qusolve,vsolve,oldflx,circular,reset)
      uvflags = 'dlb'
      if (dopass) uvflags = 'dlbf'
      call uvDatInp('vis',uvflags)
      defflux = .not.keyprsnt('flux')
      call keyr('flux',flux(1,0),1.0)
      call keyr('flux',flux(2,0),0.0)
      call keyr('flux',flux(3,0),0.0)
      call keyr('flux',flux(4,0),0.0)
      call keyi('refant',refant,3)
      call keyi('minants',minant,2)
      call mkeyr('xyphase',xyphase,MAXANT,nxyphase)
      call keyd('interval',interval(1),5d0)
      call keyd('interval',interval(2),interval(1))
      call keyi('nfbin',nfbin,1)
      call keyr('tol',tol,0.001)
      call keyfin
c
c  Check the input parameters.
c
      if (refant.le.0) call bug('f','Invalid reference antenna number')
      if (minant.le.1) call bug('f','Bad number of mininum antennae')
      if (amphsol) then
        if (min(interval(1),interval(2)).le.0)
     *    call bug('f','Negative value for interval')
        interval(1) = interval(1) / (24*60)
        interval(2) = interval(2) / (24*60)
      else
        interval(1) = 1
        interval(2) = 1
      endif
      if (nfbin.le.1) nfbin=1
      if (nfbin.gt.MAXFBIN) then
        nfbin=MAXFBIN
        call bug('w','Max number of frequency bins exceeded')
      endif
      n = 0
      if (nfbin.gt.1) n = nfbin
      do i=1,n
        do j=1,4
          flux(j,i) = flux(j,0)
        enddo
      enddo
c
c  Determine the polarisation solver to use.
c
      polcal = abs(flux(2,0))+abs(flux(3,0))+abs(flux(4,0)).gt.0.0
      usepol = (xysol .and. .not.xyvary) .or.
     *          polsol .or. qusolve .or. vsolve
      useref = (polref .and. .not.polsol) .or. (xyref .and. .not.usepol)
      if (usepol .and. useref)
     *  call bug('f','Unsupported combination of options')
      if (circular .and. vsolve)
     *  call bug('f','Cann use options=vsolve and circular together')
      if (vsolve .and. .not.polsol) call bug('f',
     *  'options=vsolve and nopol cannot be used together')
c
c  Indicate that we only want to get back raw linear polarisations.
c
      off = 0
      if (.not.circular) off = 4
      call uvDatSet('stokes',-1-off)
      call uvDatSet('stokes',-2-off)
      if (usepol .or. useref .or. xyref .or. polcal) then
        call uvDatSet('stokes',-3-off)
        call uvDatSet('stokes',-4-off)
      endif
c
c  Open the uv files.
c
      if (.not.uvDatOpn(tIn)) call bug('f','Error opening input file')
      call HisOpen(tIn,'append')
      call HisWrite(tIn,'GPCAL: '//version)
      call HisInput(tIn,'GPCAL')
c
c  Determine the number of antennae.
c
      call uvnext(tIn)
      call uvrdvri(tIn,'nants',nants,0)
      if (nants.le.0) call bug('f',
     *  'Unable to determine the number of antennae')
      call output('Number of antennae: '//itoaf(nants))
      if (nants.gt.MAXANT)
     *  call bug('f','Too many antennae for me to handle')
      if (refant.gt.nants)
     *  call bug('f','Bad reference antenna number')
      if (minant.gt.nants)
     *  call bug('f','Number of antennae is less than minimum')
      nbl = (nants*(nants-1))/2
      maxsoln = min(MAXDANTS/nants/nfbin,
     * MAXDBL/nbl/nfbin,MAXDSOLN) - 1
c
c  Set the initial estimates of the polarisation characteristics.
c
      if (nxyphase.gt.nants) call bug('w',
     *  'More XY phases were given than there are antennae')
      nxyphase = min(nxyphase,nants)
      call PolIni(reset,tIn,xyphase,nxyphase,D,xyp,nants,vsolve,fac)
c
c  Read the data, forming the sums that we need.
c
      call output('Reading the data ...')
      call DatRead(tIn,useref .or. usepol .or. xyref .or. polcal,
     *  nants,nbl,maxsoln,
     *  interval,refant,minant,n,time,nsoln,present,notrip,
     *  Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,
     *  source, freq)
c
c  If an antenna is not present, set its leakages to zero.
c  Initialize frequency bin values to continuum value.
c
      do fbin = 0, n
        do i = 1, nants
          k = i + fbin*nants
          if (.not.present(i)) then
            d(1,k) = (0.0,0.0)
            d(2,k) = (0.0,0.0)
          else if (fbin.gt.0) then
            d(1,k) = d(1,i)
            d(2,k) = d(2,i)
            xyp(k) = xyp(i)
          endif
        enddo
      enddo
c
c  Initialize the flux array.
c
      if (defflux) then
        do fbin = 0, n
          call getiquv(source,freq(fbin),oldflx,flux(1,fbin),defflux)
        enddo
      endif

      if (defflux .and. .not.qusolve) call bug('w',
     *  'It is unwise to omit OPTION=QUSOLVE when flux is unknown')
      if (flux(1,0).le.0.0) call bug('f','Invalid total flux value')
      pcent = sqrt(flux(2,0)**2 + flux(3,0)**2 + flux(4,0)**2) /
     *  flux(1,0)
      if (polref .and. pcent.eq.0.0 .and. .not.qusolve) call bug('f',
     *  'You must give values for Q,U,V to use option POLREF')
      if (polref .and. pcent.lt.0.05) call bug('w',
     *  'Source appears only weakly polarised')
c
c  Initialise the gains.
c
      call GainIni(nants,nsoln,n,Gains,1.0,xyp)
c
c  Iterate until we have converged.
c
      do fbin = 0, n
        if (fbin.gt.0) then
          write(line,'(a,i2)') 'Solution for freq bin ',fbin
          call output(line)
        endif
        niter = 0
        epsi = tol + 1.0
        if (usepol .or. useref) then
          ttol = 0.01
        else
          ttol = tol
        endif

        do while (epsi.gt.tol .and. niter.lt.MAXITER)
          niter = niter + 1
          epsi = 0.0
          call CopyG(nants,nsoln,n,fbin,
     *               D,Gains,Flux,OldD,OldGains,OldFlux)
          ttol = max(0.3*ttol,0.5*tol)
c
c  Solve for the antenna gains.
c
          xypref = xyp(refant+fbin*(n+1))
          if (amphsol) then
            if (niter.eq.1 .and. xysol .and. .not.xyvary) then
              call output('Estimating the XY phases ...')
              call AmpSolve(maxsoln,nsoln,nants,nbl,n,fbin,flux,refant,
     *          xyref .and. .not.qusolve,.true.,
     *          D,xyp,Gains,niter.le.1,ttol,epsi1,
     *          Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,
     *          circular)
              call xyfudge(nsoln,nants,n,fbin,xyp,
     *                     Gains,refant,.false.)
            else
              call AmpSolve(maxsoln,nsoln,nants,nbl,n,fbin,flux,refant,
     *          xyref .and. xyvary .and. .not.qusolve,xyvary,
     *          D,xyp,Gains,niter.le.1,ttol,epsi1,
     *          Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,
     *          circular)
              if (xyvary) call xyfudge(nsoln,nants,n,fbin,xyp,
     *                                          Gains,refant,.true.)
            endif
c
c  If the XY phase for the reference antenna has changed, then adjust
c  the leakages accordingly.
c
            xypcorr = xyp(refant+fbin*(n+1))*conjg(xypref)
            if ((polref .or. polsol .or. xyref) .and.
     *          abs(real(xypcorr)-1)+abs(aimag(xypcorr)).gt.ttol)
     *             call rotleaks(D(1,1+nants*fbin),nants,xypcorr)

            write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *          ', Amplit/Phase Solution Error: ',epsi1
            call output(line)
            epsi = max(epsi,epsi1)
          endif
c
c  Solve for all the polarisation characteristics.
c
          if (usepol) then
            call PolSolve(maxsoln,nsoln,nants,nbl,n,fbin,flux,refant,
     *        polsol,polref,qusolve,vsolve,xysol,xyref,notrip,
     *        D,Gains,xyp,epsi1,present,Vis,VisCos,VisSin,
     *        SumS,SumC,SumS2,SumCS,Count,circular)
            write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *          ', Polarisation Solution Error: ',epsi1
            call output(line)
            epsi = max(epsi,epsi1)
          else if (useref) then
            call RefSolve(maxsoln,nsoln,nants,nbl,n,fbin,flux,xyref,
     *        polref,D,Gains,xyp,epsi1,Vis,VisCos,VisSin,
     *        SumS,SumC,SumS2,SumCS,Count,circular)
            write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *          ', Reference Solution Error:    ',epsi1
            call output(line)
            epsi = max(epsi,epsi1)
          endif

          if ((usepol .or. useref) .and. amphsol) then
            call CompareG(nants,nsoln,n,fbin,D,Gains,Flux,
     *          OldD,OldGains,OldFlux,epsi)
            write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *          ', Overall Solution Error:      ',epsi
            call output(line)
          else if (amphsol) then
            epsi = 0.0
          endif
        enddo
        if (niter.ge.MAXITER)
     *    call bug('w','Failed to converge ... saving solution anyway')
c
c  If no flux was given, scale the gains so that they have an rms of
c  of any original gains. Determine what flux this implies.
c

        if (defflux) call GainScal(flux(1,fbin),
     *     Gains(1,fbin*nants*nsoln+1),2*nants*nsoln,fac)
c
c  Tell about the source polarisation.
c
        write(line,'(a,f8.4)')'I flux density: ', flux(1,fbin)
        call writeo(tIn,line)
        if (qusolve) then
          write(line,'(a,f8.3)')'Percent Q:',
     *         100.0*flux(2,fbin)/flux(1,fbin)
          call writeo(tIn,line)
          write(line,'(a,f8.3)')'Percent U:',
     *         100.0*flux(3,fbin)/flux(1,fbin)
          call writeo(tIn,line)
        endif
        if (vsolve) then
          write(line,'(a,f8.3)')'Percent V:',
     *         100.0*flux(4,fbin)/flux(1,fbin)
          call writeo(tIn,line)
        endif
c
c  Tell about the XY phases.
c
        if ((xysol .or. xyref .or. nxyphase.gt.0).and. .not.xyvary) then
          call writeo(tIn,'XY phases (degrees)')
          do j = 1,nants,7
            jmax = min(j+6,nants)
            do i = j, jmax
              xyphase(i) = atan2(aimag(xyp(i)),real(xyp(i)))
            enddo
            write(line,'(1x,a,i2,a,i2,a,7f7.1)')'Xyphase(',j,'-',jmax,
       *      ') = ',(180/pi*xyphase(i),i=j,jmax)
            call writeo(tIn,line)
          enddo
        endif
c
c  Tell about the leakage parameters.
c
        if (polsol .or. polref .or. xyref) then
          call writeo(tIn,'Leakage terms:')
          do j = 1, nants
            if (present(j)) then
              k = j + fbin*nants
              write(line,'(1x,a,i2,a,f8.5,a,f8.5,a,f8.5,a,f8.5,a)')
     *       'Ant',j,':Dx,Dy = (',real(D(1,k)),',',aimag(D(1,k)),
     *                         '),(',real(D(2,k)),',',aimag(D(2,k)),')'
              call writeo(tIn,line)
            endif
          enddo
        endif
      enddo
c
c  Write out the gain table.
c
      if (amphsol)
     *  call GainTab(tIn,time,Gains,nants,nsoln,n,freq)
c
c  Write out the leakage parameters.
c
      if (polsol .or. polref .or. xyref) then
        call LeakTab(tIn,D,nants,n,freq)
      endif
c
c  All said and done.
c
      call HisClose(tIn)
      call uvDatCls()

      end

**************************************************************** xyfudge

      subroutine xyfudge(nsoln,nants,nfbin,fbin,xyp,Gains,refant,xyvary)

      integer nsoln,nants,refant,nfbin,fbin
      logical xyvary
      complex Gains(2,nants,nsoln,0:nfbin),xyp(nants,0:nfbin)
c-----------------------------------------------------------------------
c  Given X and Y antenna gains, remove a mean XY phase from
c  each antenna. If the XY phase is not supposed to vary, replace
c  the actual values with the means.
c
c  Input:
c    nsoln
c    nants
c    refant     Reference antenna.
c    xyvary     True if the XY phase is allowed to vary with time.
c  Input/Output:
c    Gains
c  Output:
c    xyp
c-----------------------------------------------------------------------
      include 'gpcal.h'
      real phase(MAXANT),phase2(MAXANT),theta,t
      complex w
      integer i,j,count(MAXANT),sd
      character line*80
c-----------------------------------------------------------------------
c
c  Initialise the counters.
c
      do i = 1, nants
        count(i) = 0
      enddo
c
c  Accumulate all the info.
c
      
      do j = 1, nsoln
        do i = 1, nants
          if (   abs(real(Gains(X,i,j,fbin)))+
     *           abs(aimag(Gains(X,i,j,fbin))).gt.0.0
     *     .and. abs(real(Gains(Y,i,j,fbin)))+
     *           abs(aimag(Gains(Y,i,j,fbin))).gt.0.0
     *      ) then
            w = Gains(Y,i,j,fbin) / Gains(X,i,j,fbin)
            theta = atan2(aimag(w),real(w))
            if (count(i).eq.0) then
              phase(i) = theta
              phase2(i) = theta*theta
            else
              theta = theta +
     *          2*pi*nint((phase(i)/count(i) - theta)/(2*pi))
              phase(i) = phase(i) + theta
              phase2(i) = phase2(i) + theta*theta
            endif
            count(i) = count(i) + 1
          endif
        enddo
      enddo
c
c  Determine the mean values.
c
      do i = 1, nants
        if (count(i).gt.0) then
          theta = phase(i)  / count(i)
          xyp(i,fbin) = cmplx(cos(theta),sin(theta))
          sd = nint(180/pi*sqrt(abs(phase2(i)/count(i)-theta*theta)))
          if (sd.gt.10) then
            write(line,'(a,i2,a,i4,a)')
     *        'Scatter in estimating XY phase for antenna',i,
     *        ' was',sd,' degrees'
            call bug('w',line)
          endif
        else
          xyp(i,fbin) = 1
        endif
      enddo
c
c  Now if the XY phase is not supposed to vary, replace any variation
c  with the mean.
c
      if (.not.xyvary) then
        do j = 1, nsoln
          do i = 1, nants
            t = abs(Gains(X,i,j,fbin))
            if (t.gt.0) Gains(Y,i,j,fbin) =
     *        abs(Gains(Y,i,j,fbin)) * Gains(X,i,j,fbin) / t *
     *         xyp(i,fbin)
          enddo
        enddo
      else
        do j = 1, nsoln
          t = abs(Gains(Y,refant,j,fbin))
          if (t.gt.0) then
            w = conjg(Gains(Y,refant,j,fbin))/t * xyp(refant,fbin)
            do i = 1, nants
              Gains(Y,i,j,fbin) = w*Gains(Y,i,j,fbin)
            enddo
          endif
        enddo
      endif

      end

*************************************************************** rotleaks

      subroutine rotleaks(D,nants,xypcorr)

      integer nants
      complex D(2,nants),xypcorr
c-----------------------------------------------------------------------
c  This applies a phase term to the leakages to account for a change
c  in the reference antenna XY phase.
c
c  Because a phase has been added to the reference antenna, to keep the
c  modelled leakage the same, the same phase has to be removed from
c  the leakages.
c
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nants
        D(1,i) =       xypcorr  * D(1,i)
        D(2,i) = conjg(xypcorr) * D(2,i)
      enddo

      end

*************************************************************** CompareG

      subroutine CompareG(nants,nsoln,nfbin,fbin,D,Gains,Flux,
     *                        OldD,OldGains,OldFlux,epsi)

      integer nants,nsoln,nfbin,fbin
      complex D(2,nants,0:nfbin),Gains(2,nants,nsoln,0:nfbin)
      complex OldD(2,nants,0:nfbin),OldGains(2,nants,nsoln,0:nfbin)
      real Flux(4,0:nfbin),OldFlux(4,0:nfbin),epsi
c-----------------------------------------------------------------------
c  Determine the change in the solutions.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,j
      complex Temp
      real t
c-----------------------------------------------------------------------
      epsi = 0
      do i = 1, nants
        Temp = D(X,i,fbin) - OldD(X,i,fbin)
        epsi = max(epsi,real(Temp)**2+aimag(Temp)**2)
        Temp = D(Y,i,fbin) - OldD(Y,i,fbin)
        epsi = max(epsi,real(Temp)**2+aimag(Temp)**2)
      enddo

      do j = 1, nsoln
        do i = 1, nants
          Temp = Gains(X,i,j,fbin) - OldGains(X,i,j,fbin)
          t = real(Gains(X,i,j,fbin))**2 + aimag(Gains(X,i,j,fbin))**2
          if (t.gt.0) epsi = max(epsi,(real(Temp)**2+aimag(Temp)**2)/t)
          Temp = Gains(Y,i,j,fbin) - OldGains(Y,i,j,fbin)
          t = real(Gains(Y,i,j,fbin))**2 + aimag(Gains(Y,i,j,fbin))**2
          if (t.gt.0) epsi = max(epsi,(real(Temp)**2+aimag(Temp)**2)/t)
        enddo
      enddo

      t = Flux(1,fbin)*Flux(1,fbin)
      do i = 1, 4
        epsi = max(epsi,(Flux(i,fbin)-OldFlux(i,fbin))**2/t)
      enddo

      epsi = sqrt(epsi)
      end

****************************************************************** CopyG

      subroutine CopyG(nants,nsoln,nfbin,k,
     *                 D,Gains,Flux,OldD,OldGains,OldFlux)

      integer nants,nsoln,nfbin,k
      complex D(2,nants,0:nfbin),Gains(2,nants,nsoln,0:nfbin)
      complex OldD(2,nants,0:nfbin),OldGains(2,nants,nsoln,0:nfbin)
      real Flux(4,0:nfbin),OldFlux(4,0:nfbin)

c-----------------------------------------------------------------------
c  Copy the gains to a safe place.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,j
c-----------------------------------------------------------------------
      do i = 1, nants
        OldD(X,i,k) = D(X,i,k)
        OldD(Y,i,k) = D(Y,i,k)
      enddo

      do j = 1, nsoln
        do i = 1, nants
          OldGains(X,i,j,k) = Gains(X,i,j,k)
          OldGains(Y,i,j,k) = Gains(Y,i,j,k)
        enddo
      enddo

      do i = 1, 4
        OldFlux(i,k) = Flux(i,k)
      enddo

      end

*************************************************************** AmpSolve

      subroutine AmpSolve(maxsoln,nsoln,nants,nbl,nfbin,fbin,flux,
     *    refant,xyref,xyvary,D,xyp,Gains,first,tol,epsi,
     *    Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,circular)

      integer maxsoln,nsoln,nants,nfbin,nbl,refant,fbin
      real flux(4,0:nfbin),epsi,tol
      logical first,xyref,xyvary
      complex D(2,nants,0:nfbin),Gains(2,nants,nsoln,0:nfbin)
      complex xyp(nants,0:nfbin)
      integer Count(nbl,0:maxsoln,0:nfbin)
      complex Vis(4,nbl,maxsoln,0:nfbin),VisSin(4,nbl,maxsoln,0:nfbin)
      complex VisCos(4,nbl,maxsoln,0:nfbin)
      real SumS(nbl,0:maxsoln,0:nfbin),SumC(nbl,0:maxsoln,0:nfbin)
      real SumS2(nbl,0:maxsoln,0:nfbin)
      real SumCS(nbl,0:maxsoln,0:nfbin)
      logical circular
c-----------------------------------------------------------------------
c  Solve for antenna amplitudes and phases.
c
c  Input:
c    nsoln      Number of solution intervals.
c    nants,nbl  Number of antennae and number of baselines.
c    nfbin,fbin Number of frequency bins, bin number
c    flux       The flux of the calibrator.
c    refant     The reference antenna number.
c    first      True if this is the first call.
c    tol        The tolerance in determining the gains.
c    D          The current leakage parameter estimates.
c    xyp        The current XY phase estimates.
c    Vis,VisSin,VisCos,Count,SumS,SumC,SumS2,SumCS
c  Input/Output:
c    Gains      The current estimates of the antenna gains.  It is
c               assumed that the Y phases are equal to the X phases plus
c               the XY phases.
c  Output:
c    epsi       Fractional change in the solutions.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      complex Gx(MAXANT),Gy(MAXANT),SVM(4,MAXBASE),TempX,TempY
      complex a(4,MAXBASE),b(4,MAXBASE),c(4,MAXBASE),xyphase
      real amag(4,MAXBASE),bmag(4,MAXBASE),cmag(4,MAXBASE)
      real ab(4,MAXBASE),ac(4,MAXBASE),bc(4,MAXBASE)
      real    Axy(MAXANT),SMM(4,MAXBASE)
      integer b1(MAXBASE),b2(MAXBASE),Indx(MAXANT)
      integer nantsd,nbld,i,j,k,l,soln
      logical convrg
c-----------------------------------------------------------------------
c
c  Determine the model seen by each baseline.
c
      call GetVis(nants,nbl,flux(1,fbin),D(1,1,fbin),a,b,c,circular)
      k = 0
      do j = 2, nants
        do i = 1, j-1
          k = k + 1
          do l = 1, 4
            xyphase = (1.0,0.0)
            if (l.eq.YX .or. l.eq.YY) xyphase = xyphase * xyp(i,fbin)
            if (l.eq.XY .or. l.eq.YY) xyphase = xyphase *
     *        conjg(xyp(j,fbin))
            a(l,k) = xyphase * a(l,k)
            b(l,k) = xyphase * b(l,k)
            c(l,k) = xyphase * c(l,k)
          enddo
        enddo
      enddo

      do l = 1, 4
        do k = 1, nbl
          a(l,k) = conjg(a(l,k))
          b(l,k) = conjg(b(l,k))
          c(l,k) = conjg(c(l,k))
          amag(l,k) =   real(a(l,k))*real(a(l,k))
     *                + aimag(a(l,k))*aimag(a(l,k))
          bmag(l,k) =   real(b(l,k))*real(b(l,k))
     *                + aimag(b(l,k))*aimag(b(l,k))
          cmag(l,k) =   real(c(l,k))*real(c(l,k))
     *                + aimag(c(l,k))*aimag(c(l,k))
          ab(l,k) = 2*a(l,k)*conjg(b(l,k))
          ac(l,k) = 2*a(l,k)*conjg(c(l,k))
          bc(l,k) = 2*b(l,k)*conjg(c(l,k))
        enddo
      enddo
c
c  Loop over all solutions intervals.
c
      epsi = 0
      do soln = 1, nsoln
c
c  Initialise for the squeezing step.
c
        nbld = 0
        nantsd = 0
        do k = 1, nants
          Indx(k) = 0
        enddo
c
c  Form the needed sums, and squeeze out unnecessary baselines.
c
        k = 0
        do j = 2, nants
          do i = 1, j-1
            k = k + 1
            if (Count(k,soln,fbin).gt.0) then
              nbld = nbld + 1
              do l = 1, 4
                SVM(l,nbld) = a(l,k)*Vis(l,k,soln,fbin)
     *                      + b(l,k)*VisCos(l,k,soln,fbin)
     *                      + c(l,k)*VisSin(l,k,soln,fbin)
                SMM(l,nbld) = Count(k,soln,fbin)*(amag(l,k)+bmag(l,k))
     *                      +(cmag(l,k)-bmag(l,k))*SumS2(k,soln,fbin)
     *                      + ab(l,k)*SumC(k,soln,fbin)
     *                      + ac(l,k)*SumS(k,soln,fbin)
     *                      + bc(l,k)*SumCS(k,soln,fbin)
              enddo
              if (Indx(i).eq.0) then
                nantsd = nantsd + 1
                Indx(i) = nantsd
              endif
              if (Indx(j).eq.0) then
                nantsd = nantsd + 1
                Indx(j) = nantsd
              endif
              b1(nbld) = Indx(i)
              b2(nbld) = Indx(j)
            endif
          enddo
        enddo
c
c  Fill in the current estimates of everything, and go get the solution.
c
        if (nbld.gt.0) then
          do k = 1, nants
            if (Indx(k).gt.0) then
              Gx(Indx(k)) = Gains(X,k,soln,fbin)
              Gy(Indx(k)) = Gains(Y,k,soln,fbin) * conjg(xyp(k,fbin))
              Axy(Indx(k)) = abs(Gains(Y,k,soln,fbin) /
     *                           Gains(X,k,soln,fbin))
            endif
          enddo

          convrg = .true.
          if (xyvary) then
            if (first) then
              if (nbld.gt.0)call AmpSol0(nbld,nantsd,SVM,SMM,b1,b2,Gx,
     *                                   axy,epsi)
              do k = 1, nantsd
                Gy(k) = axy(k) * Gx(k)
              enddo
            endif
            call AmpSol2(nbld,nantsd,SVM,SMM,b1,b2,Gx,Gy,tol*tol,epsi)
          else
            if (first) call AmpSol0(nbld,nantsd,SVM,SMM,b1,b2,
     *                              Gx,axy,epsi)
            call AmpSol1(nbld,nantsd,SVM,SMM,b1,b2,Gx,axy,tol*tol,epsi)
          endif
c
c  Unpack the solutions and determine the Y gains.
c
          if (convrg) then
            do k = 1, nants
              if (Indx(k).eq.0) then
                Gains(X,k,soln,fbin) = (0.0,0.0)
                Gains(Y,k,soln,fbin) = (0.0,0.0)
              else
                Gains(X,k,soln,fbin) = Gx(Indx(k))
                if (xyvary) then
                  Gains(Y,k,soln,fbin) = xyp(k,fbin) * Gy(Indx(k))
                else
                  Gains(Y,k,soln,fbin) = axy(Indx(k))
     *                                 * xyp(k,fbin) * Gx(Indx(k))
                endif
              endif
            enddo
c
c  Refer the solution to the reference antenna.
c
            TempX = conjg(Gains(X,refant,soln,fbin)) /
     *              abs(Gains(X,refant,soln,fbin))
            TempY = TempX
            if (.not.xyref) TempY = conjg(Gains(Y,refant,soln,fbin)) /
     *                              abs(Gains(Y,refant,soln,fbin)) *
     *                              xyp(refant,fbin)
            do k = 1, nants
              Gains(X,k,soln,fbin) = TempX * Gains(X,k,soln,fbin)
              Gains(Y,k,soln,fbin) = TempY * Gains(Y,k,soln,fbin)
            enddo
          endif
        else
          do k=1,nants
            Gains(X,k,soln,fbin) = 0
            Gains(Y,k,soln,fbin) = 0
           enddo   
        endif
      enddo
c
c  Return with the error.
c
      epsi = sqrt(epsi)
      end

***************************************************************** SumVis

      subroutine SumVis(nsoln,nants,nbl,Gains,Count,Vis,VisCos,VisSin,
     *  V,VS,VC)

      integer nsoln,nants,nbl
      integer Count(nbl,0:nsoln)
      complex Vis(4*nbl,nsoln),VisCos(4*nbl,nsoln),VisSin(4*nbl,nsoln)
      complex Gains(2*nants,nsoln),V(4*nbl),VS(4*nbl),VC(4*nbl)
c-----------------------------------------------------------------------
c  This forms the sums of the visibilities for a given baseline and
c  polarisation. For efficiency's sake, the baseline and polarisation
c  dimensions have been collapsed into one.
c
c  Input:
c    nsoln      Number of solution intervals.
c    nants      Number of antennae.
c    nbl        Number of baselines.
c    Gains      The antenna gains.
c    Count
c    Vis,VisCos,VisSin
c  Output:
c    V,VC,VS
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,j,k,l,m
      integer b1(4*MAXBASE),b2(4*MAXBASE),p1(4),p2(4)
      complex g

      data p1/X,Y,X,Y/
      data p2/X,Y,Y,X/
c-----------------------------------------------------------------------
c
c  Work out an index table.
c
      k = 0
      do j = 2, nants
        do i = 1, j-1
          do m = 1, 4
            b1(k+m) = 2*(i-1) + p1(m)
            b2(k+m) = 2*(j-1) + p2(m)
          enddo
          k = k + 4
        enddo
      enddo
c
c  Initialise the summing arrays.
c
      do k = 1, 4*nbl
        V(k) = 0
        VS(k) = 0
        VC(k) = 0
      enddo
c
c  Now do the summation.
c
      do l = 1, nsoln
        do k = 1, 4*nbl
          if (Count((k+3)/4,l).gt.0) then
            if (abs(gains(b1(k),l)).gt.0.and.
     *          abs(gains(b2(k),l)).gt.0) then 
              g = 1/(Gains(b1(k),l)*conjg(Gains(b2(k),l)))
              V(k)  = V(k)  + g*Vis(k,l)
              VS(k) = VS(k) + g*VisSin(k,l)
              VC(k) = VC(k) + g*VisCos(k,l)
            endif
          endif
        enddo
      enddo

      end

*************************************************************** RefSolve

      subroutine RefSolve(maxsoln,nsoln,nants,nbl,nfbin,fbin,flux,
     *    xyref,polref,D,Gains,xyp,epsi,
     *    Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,circular)

      integer maxsoln,nsoln,nants,nbl,nfbin,fbin
      logical xyref,polref
      real flux(4,0:nfbin),epsi
      logical circular
      complex D(2,nants,0:nfbin),Gains(2,nants,nsoln,0:nfbin)
      complex xyp(nants,0:nfbin)
      integer Count(nbl,0:maxsoln,0:nfbin)
      complex Vis(4,nbl,maxsoln,0:nfbin),VisSin(4,nbl,maxsoln,0:nfbin)
      complex VisCos(4,nbl,maxsoln,0:nfbin)
      real SumS(nbl,0:maxsoln,0:nfbin),SumC(nbl,0:maxsoln,0:nfbin)
      real SumS2(nbl,0:maxsoln,0:nfbin),SumCS(nbl,0:maxsoln,0:nfbin)
c-----------------------------------------------------------------------
c  Solve for an offset in the value of the leakage parameters and the XY
c  phases.  It uses a linearised approach for solving for the XY phase
c  offset.
c
c  Input:
c    nsoln      Number of solution intervals.
c    nants,nbl  Number of antennae and baselines.
c    xyref      Solve for reference antenna XY phase.
c    Vis,VisSin,VisCos,Count,SumS,SumC,SumS2,SumCS Various accumulated
c               rubbish.
c    flux       The source flux.
c    circular
c  Input/Output:
c    D      )   On input, these contain the current estimates of the
c    Gains  )   gains, leakages, etc. On output, they contain the
c    xyp    )   updated estimates.
c  Output:
c    epsi       The relative change in the gains, etc.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      real b(3),A(9),t1,t2,rcond,z(3)
      integer pivot(3),var(3),nvar
      integer i,j,idxD,idxXYP
      complex Gain,leakoff
c-----------------------------------------------------------------------
c
c  Determine the number of things we have to solve for. Its pretty easy.
c
      nvar = 0
      idxD = 0
      if (polref) then
        idxD = nvar + 1
        var(idxD)   = 1
        var(idxD+1) = 2
        nvar = nvar + 2
      endif

      idxXYP = 0
      if (xyref) then
        idxXYP = nvar + 1
        var(idxXYP) = 3
        nvar = nvar + 1
      endif
c
c  Generate the matrix that we are to solve for.
c
      call RefAcc(nsoln,nants,var,nvar,nbl,Gains(1,1,1,fbin),
     *  D(1,1,fbin),flux(1,fbin),A,b,
     *  Vis(1,1,1,fbin),VisCos(1,1,1,fbin),VisSin(1,1,1,fbin),
     *  SumS(1,0,fbin),SumC(1,0,fbin),SumS2(1,0,fbin),SumCS(1,0,fbin),
     *  Count(1,0,fbin),circular)
c
c  Solve the system of equations.
c
      call sgeco(A,nvar,nvar,pivot,rcond,z)
      if (rcond.le.1e-6) call bug('f',
     *  'Solution for requested parameters is degenerate')
      if (rcond.lt.1e-4) call bug('w',
     *  'Solution for requested parameters is unstable')
      call sgesl(A,nvar,nvar,pivot,b,1)
c
c  Save the results, and compute epsi.
c
      epsi = 0
c
c  The XY phase offset.
c
      if (idxXYP.ne.0) then
        t1 = b(idxXYP)
        t2 = 1.0/sqrt(1+t1*t1)
        Gain = cmplx(t2,t1*t2)
        epsi = abs(t1)

        call rotleaks(D(1,1,fbin),nants,Gain)

        do i = 1, nants
          xyp(i,fbin) = xyp(i,fbin) * Gain
        enddo

        do j = 1, nsoln
          do i = 1, nants
            Gains(Y,i,j,fbin) = Gains(Y,i,j,fbin) * Gain
          enddo
        enddo
      endif
c
c  The leakage offset.
c
      if (idxD.ne.0) then
        leakoff = 0.5*cmplx(b(idxD),b(idxD+1))
        do i = 1, nants
          D(X,i,fbin) = D(X,i,fbin) + leakoff
          D(Y,i,fbin) = D(Y,i,fbin) - conjg(leakoff)
        enddo
        epsi = max(abs(b(idxD)),abs(b(idxD+1)),epsi)
      endif

      end

***************************************************************** RefAcc

      subroutine RefAcc(nsoln,nants,var,nvar,nbl,Gains,D,flux,
     *  A,b,Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,circular)

      integer nsoln,nants,nbl,nvar,var(nvar)
      complex Gains(2,nants,nsoln),D(2,nants)
      real flux(4),A(nvar,nvar),b(nvar)

      integer Count(nbl,0:nsoln)
      complex Vis(4,nbl,nsoln),VisSin(4,nbl,nsoln),VisCos(4,nbl,nsoln)
      real SumS(nbl),SumC(nbl),SumS2(nbl)
      real SumCS(nbl)
      logical circular
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,id,j,jd,k,m
      real ar(0:3,4,MAXBASE),ai(0:3,4,MAXBASE)
      real br(0:3,4,MAXBASE),bi(0:3,4,MAXBASE)
      real cr(0:3,4,MAXBASE),ci(0:3,4,MAXBASE)
      double precision temp
      complex V(4,MAXBASE),VS(4,MAXBASE),VC(4,MAXBASE)
c-----------------------------------------------------------------------
      call SumVis(nsoln,nants,nbl,Gains,Count,
     *  Vis,VisCos,VisSin,V,VS,VC)
      call RefCoeff(flux,ar,ai,br,bi,cr,ci,D,nants,nbl,circular)

      do jd = 1, nvar
        j = var(jd)
        temp = 0
        do m = 1, 4
          do k = 1, nbl
            temp = temp
     *           + real(V(m,k))*ar(j,m,k) + aimag(V(m,k))*ai(j,m,k)
     *           + real(VC(m,k))*br(j,m,k) + aimag(VC(m,k))*bi(j,m,k)
     *           + real(VS(m,k))*cr(j,m,k) + aimag(VS(m,k))*ci(j,m,k)
            temp = temp -   (ar(0,m,k)*ar(j,m,k)
     *                    +  br(0,m,k)*br(j,m,k)
     *                    +  ai(0,m,k)*ai(j,m,k)
     *                    +  bi(0,m,k)*bi(j,m,k)) * Count(k,0)
     *                  -   (cr(0,m,k)*cr(j,m,k)
     *                    -  br(0,m,k)*br(j,m,k)
     *                    +  ci(0,m,k)*ci(j,m,k)
     *                    -  bi(0,m,k)*bi(j,m,k)) * SumS2(k)
     *                  -   (ar(0,m,k)*br(j,m,k)
     *                    +  br(0,m,k)*ar(j,m,k)
     *                    +  ai(0,m,k)*bi(j,m,k)
     *                    +  bi(0,m,k)*ai(j,m,k)) * SumC(k)
            temp = temp -   (ar(0,m,k)*cr(j,m,k)
     *                    +  cr(0,m,k)*ar(j,m,k)
     *                    +  ai(0,m,k)*ci(j,m,k)
     *                    +  ci(0,m,k)*ai(j,m,k)) * SumS(k)
     *                  -   (br(0,m,k)*cr(j,m,k)
     *                    +  cr(0,m,k)*br(j,m,k)
     *                    +  bi(0,m,k)*ci(j,m,k)
     *                    +  ci(0,m,k)*bi(j,m,k)) * SumCS(k)
          enddo
        enddo
        b(jd) = temp
      enddo

      do jd = 1, nvar
        j = var(jd)
        do id = 1, jd
          i = var(id)
          temp = 0
          do m = 1, 4
            do k = 1, nbl
              temp = temp + (ar(i,m,k)*ar(j,m,k)
     *                    +  br(i,m,k)*br(j,m,k)
     *                    +  ai(i,m,k)*ai(j,m,k)
     *                    +  bi(i,m,k)*bi(j,m,k)) * Count(k,0)
     *                    + (cr(i,m,k)*cr(j,m,k)
     *                    -  br(i,m,k)*br(j,m,k)
     *                    +  ci(i,m,k)*ci(j,m,k)
     *                    -  bi(i,m,k)*bi(j,m,k)) * SumS2(k)
     *                    + (ar(i,m,k)*br(j,m,k)
     *                    +  br(i,m,k)*ar(j,m,k)
     *                    +  ai(i,m,k)*bi(j,m,k)
     *                    +  bi(i,m,k)*ai(j,m,k)) * SumC(k)
              temp = temp + (ar(i,m,k)*cr(j,m,k)
     *                    +  cr(i,m,k)*ar(j,m,k)
     *                    +  ai(i,m,k)*ci(j,m,k)
     *                    +  ci(i,m,k)*ai(j,m,k)) * SumS(k)
     *                    + (br(i,m,k)*cr(j,m,k)
     *                    +  cr(i,m,k)*br(j,m,k)
     *                    +  bi(i,m,k)*ci(j,m,k)
     *                    +  ci(i,m,k)*bi(j,m,k)) * SumCS(k)
            enddo
          enddo
          A(id,jd) = temp
          A(jd,id) = temp
        enddo
      enddo
      end

*************************************************************** RefCoeff

      subroutine RefCoeff(flux,ar,ai,br,bi,cr,ci,D,nants,nbl,circular)

      integer nants,nbl
      real flux(4),ar(0:3,4,nbl),ai(0:3,4,nbl)
      real br(0:3,4,nbl),bi(0:3,4,nbl),cr(0:3,4,nbl),ci(0:3,4,nbl)
      complex D(2,nants)
      logical circular
c-----------------------------------------------------------------------
c  Generate the various coefficient tables, that are used to generate
c  the needed matrix.
c
c  Input:
c    flux       I,Q,U,V vector.
c    D          Leakages.
c    nants      Number of antennas.
c    nbl        Number of baselines.
c    circular   True if the feeds are circularly polarised.
c  Output:
c    ar,ai,br,bi,cr,ci
c               Coefficient arrays.  These are such that
c                 Re[V(p,bl)] \approx  ar(0,p,bl) +
c                                      cos(2*chi)*br(0,p,bl) +
c                                      sin(2*chi)*c(0,p,bl) +
c                                      Sum_i x(i)*[ ar(i,p,bl) +
c                                        cos(2*chi)*br(i,p,bl) +
c                                        sin(2*chi)*c(i,p,bl) ]
c   and x(1) = 2*Re[ LeakOff ]
c   and x(2) = 2*Im[ LeakOff ]
c   and x(3) = XyPhaseOff (radians).
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer k,m
      integer arl(0:3,4),ail(0:3,4),brl(0:3,4),bil(0:3,4)
      integer crl(0:3,4),cil(0:3,4)
      integer arc(0:3,4),aic(0:3,4),brc(0:3,4),bic(0:3,4)
      integer crc(0:3,4),cic(0:3,4)
      complex a(4,MAXBASE),b(4,MAXBASE),c(4,MAXBASE)
c
c                      XX           YY           XY           YX
c                 0 Dr  i  T   0 Dr  i  T   0 Dr  i  T   0 Dr  i  T
      data arl/ 0, 0,-4, 0,  0, 0, 4, 0,  0, 0, 0, 0,  0, 0, 0, 0/
      data ail/ 0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0/
      data brl/ 0, 3, 0, 0,  0,-3, 0, 0,  0,-2, 0, 0,  0,-2, 0, 0/
      data bil/ 0, 0, 0, 0,  0, 0, 0, 0,  0, 0,-2, 0,  0, 0, 2, 0/
      data crl/ 0,-2, 0, 0,  0, 2, 0, 0,  0,-3, 0, 0,  0,-3, 0, 0/
      data cil/ 0, 0, 0, 0,  0, 0, 0, 0,  0, 0,-3, 0,  0, 0, 3, 0/
c
c                      RR           LL           RL           LR
c                 0 Dr  i  T   0 Dr  i  T   0 Dr  i  T   0 Dr  i  T
      data arc/ 0, 0, 0, 0,  0, 0, 0, 0,  0,-4, 0, 0,  0,-4, 0, 0/
      data aic/ 0, 0, 0, 0,  0, 0, 0, 0,  0, 0,-4, 0,  0, 0, 4, 0/
      data brc/ 0, 2,-3, 0,  0,-2, 3, 0,  0, 0, 0, 0,  0, 0, 0, 0/
      data bic/ 0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0/
      data crc/ 0, 3, 2, 0,  0,-3,-2, 0,  0, 0, 0, 0,  0, 0, 0, 0/
      data cic/ 0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0/
c-----------------------------------------------------------------------
      do k = 1, nbl
        if (circular) then
          call Fill(flux(1),3,arc,ar(0,1,k))
          call Fill(flux(1),3,aic,ai(0,1,k))
          call Fill(flux(1),3,brc,br(0,1,k))
          call Fill(flux(1),3,bic,bi(0,1,k))
          call Fill(flux(1),3,crc,cr(0,1,k))
          call Fill(flux(1),3,cic,ci(0,1,k))
        else
          call Fill(flux(1),3,arl,ar(0,1,k))
          call Fill(flux(1),3,ail,ai(0,1,k))
          call Fill(flux(1),3,brl,br(0,1,k))
          call Fill(flux(1),3,bil,bi(0,1,k))
          call Fill(flux(1),3,crl,cr(0,1,k))
          call Fill(flux(1),3,cil,ci(0,1,k))
        endif
      enddo

      call GetVis(nants,nbl,flux(1),D(1,1),a,b,c,circular)
c
c  Now fill in the unset ones.
c
      do m = 1, 4
        do k = 1, nbl
          ar(0,m,k) = real(a(m,k))
          ai(0,m,k) = aimag(a(m,k))
          br(0,m,k) = real(b(m,k))
          bi(0,m,k) = aimag(b(m,k))
          cr(0,m,k) = real(c(m,k))
          ci(0,m,k) = aimag(c(m,k))
          if (m.eq.XY) then
            ar(3,m,k) =   ai(0,m,k)
            ai(3,m,k) = - ar(0,m,k)
            br(3,m,k) =   bi(0,m,k)
            bi(3,m,k) = - br(0,m,k)
            cr(3,m,k) =   ci(0,m,k)
            ci(3,m,k) = - cr(0,m,k)
          else if (m.eq.YX) then
            ar(3,m,k) = - ai(0,m,k)
            ai(3,m,k) =   ar(0,m,k)
            br(3,m,k) = - bi(0,m,k)
            bi(3,m,k) =   br(0,m,k)
            cr(3,m,k) = - ci(0,m,k)
            ci(3,m,k) =   cr(0,m,k)
          endif
        enddo
      enddo
      end

***************************************************************** GetVis

      subroutine GetVis(nants,nbl,flux,D,a,b,c,circular)

      integer nants,nbl
      real flux(4)
      complex a(4,nbl),b(4,nbl),c(4,nbl),D(2,nants)
      logical circular
c-----------------------------------------------------------------------
c  Given the values of I,Q,U,V and polarisation leakage, calculate the
c  value of the visibility that we would expect to measure on each
c  baseline:
c
c    V(m,k) = a(m,k) + b(m,k)*cos(2*chi) + c(m,k)*sin(2*chi)
c
c  Input:
c    nants
c    nbl
c    flux
c    D
c  Output:
c    a,b,c
c-----------------------------------------------------------------------
      include 'gpcal.h'

      integer II,QQ,UU,VV
      parameter (II=1, QQ=2, UU=3, VV=4)

      integer i,j,k
      complex axx,bxx,cxx,ayy,byy,cyy,axy,bxy,cxy,ayx,byx,cyx
c-----------------------------------------------------------------------
      if (circular) then
        axx = flux(II) + flux(VV)
        bxx = 0
        cxx = 0
        ayy = flux(II) - flux(VV)
        byy = 0
        cyy = 0
        axy = 0
        bxy = flux(QQ) - cmplx(0.0,flux(UU))
        cxy = flux(UU) + cmplx(0.0,flux(QQ))
        ayx = 0
        byx = flux(QQ) + cmplx(0.0,flux(UU))
        cyx = flux(UU) - cmplx(0.0,flux(QQ))
      else
        axx =  flux(II)
        bxx =  flux(QQ)
        cxx =  flux(UU)
        ayy =  flux(II)
        byy = -flux(QQ)
        cyy = -flux(UU)
        axy = -cmplx(0.0,flux(VV))
        bxy =  flux(UU)
        cxy = -flux(QQ)
        ayx =  cmplx(0.0,flux(VV))
        byx =  flux(UU)
        cyx = -flux(QQ)
      endif
c
c  Do it now for each baseline.
c
      k = 0
      do j = 2, nants
        do i = 1, j-1
          k = k + 1
          a(XX,k) = axx + D(X,i)*ayx + conjg(D(X,j))*axy
     *                  + D(X,i)*      conjg(D(X,j))*ayy
          b(XX,k) = bxx + D(X,i)*byx + conjg(D(X,j))*bxy
     *                  + D(X,i)*      conjg(D(X,j))*byy
          c(XX,k) = cxx + D(X,i)*cyx + conjg(D(X,j))*cxy
     *                  + D(X,i)*      conjg(D(X,j))*cyy

          a(YY,k) = ayy + D(Y,i)*axy + conjg(D(Y,j))*ayx
     *                  + D(Y,i)*      conjg(D(Y,j))*axx
          b(YY,k) = byy + D(Y,i)*bxy + conjg(D(Y,j))*byx
     *                  + D(Y,i)*      conjg(D(Y,j))*bxx
          c(YY,k) = cyy + D(Y,i)*cxy + conjg(D(Y,j))*cyx
     *                  + D(Y,i)*      conjg(D(Y,j))*cxx

          a(XY,k) = axy + D(X,i)*ayy + conjg(D(Y,j))*axx
     *                  + D(X,i)*      conjg(D(Y,j))*ayx
          b(XY,k) = bxy + D(X,i)*byy + conjg(D(Y,j))*bxx
     *                  + D(X,i)*      conjg(D(Y,j))*byx
          c(XY,k) = cxy + D(X,i)*cyy + conjg(D(Y,j))*cxx
     *                  + D(X,i)*      conjg(D(Y,j))*cyx

          a(YX,k) = ayx + D(Y,i)*axx + conjg(D(X,j))*ayy
     *                  + D(Y,i)*      conjg(D(X,j))*axy
          b(YX,k) = byx + D(Y,i)*bxx + conjg(D(X,j))*byy
     *                  + D(Y,i)*      conjg(D(X,j))*bxy
          c(YX,k) = cyx + D(Y,i)*cxx + conjg(D(X,j))*cyy
     *                  + D(Y,i)*      conjg(D(X,j))*cxy
        enddo
      enddo

      end

*************************************************************** PolSolve

      subroutine PolSolve(maxsoln,nsoln,nants,nbl,nfbin,fbin,flux,
     *    refant,polsol,polref,qusolve,vsolve,xysol,xyref,notrip,D,
     *    Gains,xyp,epsi,present,Vis,VisCos,VisSin,SumS,SumC,SumS2,
     *    SumCS,Count,circular)

      integer maxsoln,nsoln,nants,nbl,refant, nfbin, fbin
      logical xysol,qusolve,vsolve,xyref,polsol,polref,notrip,circular
      real flux(4,0:nfbin),epsi 
      complex D(2,nants,0:nfbin),Gains(2,nants,nsoln,0:nfbin)
      complex xyp(nants,0:nfbin)

      integer Count(nbl,0:maxsoln,0:nfbin)
      logical present(nants)
      complex Vis(4,nbl,maxsoln,0:nfbin),VisSin(4,nbl,maxsoln,0:nfbin)
      complex VisCos(4,nbl,maxsoln,0:nfbin)
      real SumS(nbl,0:maxsoln,0:nfbin),SumC(nbl,0:maxsoln,0:nfbin)
      real SumS2(nbl,0:maxsoln,0:nfbin),SumCS(nbl,0:maxsoln,0:nfbin)
c-----------------------------------------------------------------------
c  Solve for the leakage parameters, the source Q and U, XY phases, etc.
c  This uses a linearised approach, where the produces of the error
c  terms are assumed negligible.  This means that the system of
c  equations is linear, and so can be explicitly solved.
c
c  On the reference antenna:
c    * aimag(Gains(X,refant,*)) are not varied.
c  If the "xysol" option is not used:
c    * xyp(*) are not varied.
c  If the "qusolve" option is not used:
c    * flux(2:3) are not varied.
c  If the "xyref" option is not used, then
c    * xyp(refant) is not varied.
c
c  If the "polref" option is not used, then
c    * D(X,refant) is not varied.
c  If the "qusolve" and "polref" options are used together
c    * real(D(X,refant)) is not varied.
c
c  Input:
c    nsoln      Number of solution intervals.
c    nants,nbl  Number of antennae and baselines.
c    qusolve    If true, solve for Q and U.
c    xyref      Solve for reference antenna XY phase.
c    notrip     True if there are no good closures in the data.
c    polref     Solve for the reference antenna X feed alignment,
c               differential ellipticity.
c    xysol      If true, solve for the XY phase.
c    refant     The reference antenna number.
c    present    True if the antenna is represented in the data.
c    Vis,VisSin,VisCos,Count,SumS,SumC,SumS2,SumCS Various accumulated
c               rubbish.
c    circular   True if the data are for circularly polarised (as
c               distinct from linearly polarised) feeds.
c  Input/Output:
c    flux   )   On input, these contain the current estimates of the
c    Gains  )   gains, leakages, etc.  On output, they contain the
c    D      )   updated estimates.
c  Output:
c    xyp        XY phase estimate.
c    epsi       The relative change in the gains, etc.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer Dvar(2,2,MAXANT),QUvar,Tvar(2,MAXANT),nvar
      integer Vars(8,4,MAXBASE),pivot(6*MAXANT+1)
      integer i,j,ngood
      complex expix(MAXANT),expiy(MAXANT),dD(2,MAXANT),Sum1,Sum2
      complex dDx,dDy,xypref,xypcorr
      real b(6*MAXANT+1),A((6*MAXANT+1)*(6*MAXANT+1)),z(6*MAXANT+1)
      real temp,dv,rcond
      logical polref1,xyref1
c-----------------------------------------------------------------------
c
c  Determine if we are really going to do polref.
c
      polref1 = polref
      xyref1  = xyref
      if ((polref .or. xyref) .and. qusolve) then
        temp = flux(2,fbin)*flux(2,fbin) + flux(3,fbin)*flux(3,fbin) 
     *         + flux(4,fbin)*flux(4,fbin)
        if (temp.lt.1e-4*flux(1,fbin)*flux(1,fbin)) then
          call bug('w','Turning off XYREF/POLREF for this iteration')
          polref1 = .false.
          xyref1 = .false.
        endif
      endif
c
c  We solve simultaneously for the antenna phase error, Q and U, as well
c  as the leakage terms. First determine how many unknowns there are,
c  and assign a index to each of them.  QUvar,Tvar,Dvar give the indices
c  for Q, antenna phase, and leakage terms.
c
      call GetVarNo(nants,nbl,polsol,polref1,qusolve,xysol,xyref1,
     *  notrip,refant,present,QUvar,Tvar,Dvar,nvar,Vars)
c
c  We are going to solve the system Ax = b. Generate A and b.
c
      call PolAcc(nsoln,nants,nbl,Gains(1,1,1,fbin),D(1,1,fbin),
     * flux(1,fbin),Vars,
     *  A,b,nvar,Vis(1,1,1,fbin),VisCos(1,1,1,fbin),VisSin(1,1,1,fbin),
     *  SumS(1,0,fbin),SumC(1,0,fbin),SumS2(1,0,fbin),SumCS(1,0,fbin),
     *  Count(1,0,fbin),circular)
     
c
c  Solve the system of equations using LINPACK routines.
c
      call sgeco(A,nvar,nvar,pivot,rcond,z)
      if (rcond.le.1e-6) then
        if (nfbin.eq.0) then
          call bug('f',
     *      'Solution for requested parameters is degenerate')
        else
          epsi=0
          return
        endif
      endif
      if (rcond.lt.1e-4) call bug('w',
     *  'Solution for requested parameters is unstable')
      call sgesl(A,nvar,nvar,pivot,b,1)
c
c  Save the results, and compute epsi.
c
c  Q and U.
c
      epsi = 0
      if (QUvar.gt.0) then
        temp = flux(1,fbin)
        if (circular) temp = flux(1,fbin) + flux(4,fbin)
        flux(2,fbin) = flux(2,fbin) + b(QUvar)   * temp
        flux(3,fbin) = flux(3,fbin) + b(QUvar+1) * temp
        epsi = max(epsi,b(QUvar)**2,b(QUvar+1)**2)
      endif
c
c  The phases.
c
      if (xysol) then
        xypref = xyp(refant,fbin)
        do i = 1, nants
          call UnpackT(Tvar(X,i),b,nvar,expix(i),epsi)
          call UnpackT(Tvar(Y,i),b,nvar,expiy(i),epsi)
          xyp(i,fbin) = xyp(i,fbin) * expiy(i)*conjg(expix(i))
        enddo

        do j = 1, nsoln
          do i = 1, nants
            Gains(X,i,j,fbin) = Gains(X,i,j,fbin) * expix(i)
            Gains(Y,i,j,fbin) = Gains(Y,i,j,fbin) * expiy(i)
          enddo
        enddo

        xypcorr = xyp(refant,fbin)*conjg(xypref)
        if (abs(aimag(xypcorr)).gt.1e-3) 
     *     call rotleaks(D(1,1,fbin),nants,xypcorr)
      endif
c
c  The leakage terms.  Accumulate some weird things so we can normalise
c  them.
c
      Sum1 = (0.0,0.0)
      Sum2 = (0.0,0.0)
      ngood = 0
      do i = 1, nants
        call UnpackD(Dvar(1,X,i),Dvar(2,X,i),b,nvar,dD(X,i),epsi)
        call UnpackD(Dvar(1,Y,i),Dvar(2,Y,i),b,nvar,dD(Y,i),epsi)
        if (present(i)) then
          ngood = ngood + 2
          Sum1 = Sum1 + dD(X,i) - conjg(dD(Y,i))
          Sum2 = Sum2 + dD(X,i) + conjg(dD(Y,i))
        endif
      enddo
c
c  Combine the leakages with the leakage increments, adding in the
c  appropriate adjustments.
c
      Sum1 = Sum1/ngood
      dDx = Sum1
      if (polref1 .and. .not.qusolve) dDx = cmplx(0.0,aimag(dDx))
      if (polref1)                 dDx = cmplx(real(dDx),0.0)
      dDy = -conjg(dDx)

      if (vsolve) then
        dv = -2.0*aimag(Sum2)/ngood
        flux(4,fbin) = flux(4,fbin) + dv*flux(1,fbin)
        dDx = dDx - cmplx(0.0,0.5*dv)
        dDy = dDy + cmplx(0.0,0.5*dv)
        epsi = max(epsi,dv*dv)
      endif

      do i = 1, nants
        if (present(i)) then
          D(X,i,fbin) = D(X,i,fbin) + dD(X,i) - dDx
          D(Y,i,fbin) = D(Y,i,fbin) + dD(Y,i) - dDy
        endif
      enddo
c
c  Return with the bacon.
c
      epsi = sqrt(epsi)

      end

***************************************************************** PolAcc

      subroutine PolAcc(nsoln,nants,nbl,Gains,D,flux,
     *  Vars,A,b,nvar,Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,
     *  circular)

      integer nsoln,nants,nbl,nvar
      complex Gains(2,nants,nsoln),D(2,nants)
      integer Vars(8,4,nbl)
      real A(nvar,nvar),b(nvar),flux(4)
      logical circular

      integer Count(nbl,0:nsoln)
      complex Vis(4,nbl,nsoln),VisSin(4,nbl,nsoln),VisCos(4,nbl,nsoln)
      real SumS(nbl,0:nsoln),SumC(nbl,0:nsoln),SumS2(nbl,0:nsoln)
      real SumCS(nbl,0:nsoln)
c-----------------------------------------------------------------------
c  This routine is responsible for building the matrix that we are to
c  solve.
c
c  Input:
c    D          The leakage to remove from the reference antenna.
c    nsolns     Number of solution intervals.
c    nants,nbl  Number of antennae, and number of baselines.
c    nvar       Number of variables to solve for.
c    Vars       Indices of the variables.
c    Gains      The antenna gains.
c    flux       The source fluxes (I,Q,U,V).
c    Count,Vis,VisSin,VisCos,SumS,SumC,SumS2,SumCS Various accumulated
c               crap.
c  Output:
c    A          Matrix to solve.
c    b          Left hand side. That is the matrix eqn is
c                   b = Ax
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,j,k,m,v1,v2
      complex V(4,MAXBASE),VS(4,MAXBASE),VC(4,MAXBASE)
      real ar(0:8,4),ai(0:8,4),br(0:8,4),bi(0:8,4),cr(0:8,4),ci(0:8,4)
      real Cconst(0:8,0:8,4),Csin(0:8,0:8,4),Ccos(0:8,0:8,4)
      real Ccossin(0:8,0:8,4),Csin2(0:8,0:8,4)
c-----------------------------------------------------------------------
c
c  Sum the visibilities.
c
      call SumVis(nsoln,nants,nbl,Gains,Count,
     *  Vis,VisCos,VisSin,V,VS,VC)
c
c  Correct the sums for the current leakage estimates.
c
      call DCorrect(nants,nbl,V,VC,VS,D,flux,
     *  Count(1,0),SumC(1,0),SumS(1,0),SumCS(1,0),SumS2(1,0),
     *  circular)
c
c  Generate the coefficient tables.
c
      call PolCoeff(flux,Cconst,Csin,Ccos,Csin2,Ccossin,
     *                                ar,ai,br,bi,cr,ci,circular)
c
c  We want to solve the system of linear equations, Ax = b.
c  Initialise A and b, then accumulate them.
c
      do j = 1, nvar
        b(j) = 0
        do i = 1, j
          A(i,j) = 0
        enddo
      enddo

      do k = 1, nbl
        do m = 1, 4
          do j = 1, 8
            v2 = Vars(j,m,k)
            if (v2.gt.0) then
              b(v2) = b(v2)
     *              + real(V(m,k))*ar(j,m) + aimag(V(m,k))*ai(j,m)
     *              + real(VC(m,k))*br(j,m) + aimag(VC(m,k))*bi(j,m)
     *              + real(VS(m,k))*cr(j,m) + aimag(VS(m,k))*ci(j,m)
            endif
          enddo
        enddo
      enddo

      do k = 1, nbl
        if (Count(k,0).gt.0) then
          do m = 1, 4
            do j = 1, 8
              v2 = Vars(j,m,k)
              if (v2.gt.0) then
                do i = 1, j
                  v1 = Vars(i,m,k)
                  if (v1.gt.0) then
                    A(v1,v2) = A(v1,v2)
     *                  + Cconst(i,j,m)  *Count(k,0)
     *                  + Csin2(i,j,m)   *SumS2(k,0)
     *                  + Csin(i,j,m)    *SumS(k,0)
     *                  + Ccos(i,j,m)    *SumC(k,0)
     *                  + Ccossin(i,j,m) *SumCS(k,0)
                  endif
                enddo
              endif
            enddo
          enddo
        endif
      enddo

      do j = 2, nvar
        do i = 1, j-1
          a(j,i) = a(i,j)
        enddo
      enddo
      end

*************************************************************** Dcorrect

      subroutine DCorrect(nants,nbl,V,VC,VS,D,flux,
     *  Count,SumC,SumS,SumCS,SumS2,circular)

      integer nants,nbl,Count(nbl)
      complex V(4,nbl),VC(4,nbl),VS(4,nbl),D(2,nants)
      real flux(4),SumC(nbl),SumS(nbl),SumCS(nbl),SumS2(nbl)
      logical circular
c-----------------------------------------------------------------------
c  Correct the sums of the visibilities for the leakage of the reference
c  feed.
c
c  Input:
c    nants,nbl
c    refant
c    D
c    flux
c    Count,SumC,SumS,SumCS,SumS2
c  Input/Output:
c    V,VC,VS
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,j,k,p
      complex a(4,MAXBASE),b(4,MAXBASE),c(4,MAXBASE)
c-----------------------------------------------------------------------
c
c  Determine the nominal response in each thingo.
c
      call GetVis(nants,nbl,flux,D,a,b,c,circular)
c
c  Correct for the known leakages.
c
      k = 0
      do j = 2, nants
        do i = 1, j-1
          k = k + 1
          do p = 1, 4
            V(p,k)  = V(p,k)  - (a(p,k)*Count(k) +
     *                b(p,k)*SumC(k)              + c(p,k)*SumS(k))
            VC(p,k) = VC(p,k) - (a(p,k)*SumC(k)  +
     *                b(p,k)*(Count(k)-SumS2(k))  + c(p,k)*SumCS(k))
            VS(p,k) = VS(p,k) - (a(p,k)*SumS(k)  +
     *                b(p,k)*SumCS(k)             + c(p,k)*SumS2(k))
          enddo
        enddo
      enddo

      end

*************************************************************** PolCoeff

      subroutine PolCoeff(flux,Cconst,Csin,Ccos,Csin2,Ccossin,
     *                                ar,ai,br,bi,cr,ci,circular)

      real flux(4)
      real ar(0:8,4),ai(0:8,4),br(0:8,4),bi(0:8,4),cr(0:8,4),ci(0:8,4)
      real Cconst(0:8,0:8,4),Csin(0:8,0:8,4),Ccos(0:8,0:8,4)
      real Ccossin(0:8,0:8,4),Csin2(0:8,0:8,4)
      logical circular
c-----------------------------------------------------------------------
c  This generates various coefficient tables, that are used to generate
c  the needed matrix.
c
c  We define the arrays ar,br,cr the coefficient arrays for the "real"
c  equations, as:
c
c  real(V(p)) - ar(0,p) - br(0,p)*cos(2*chi) - cr(0,p)*sin(2*chi)
c         = x1*(ar(1,p) + br(1,p)*cos(2*chi) + cr(1,p)*sin(2*chi))
c         + x2*(ar(2,p) + br(2,p)*cos(2*chi) + cr(2,p)*sin(2*chi))
c         + x3*(ar(3,p) + br(3,p)*cos(2*chi) + cr(3,p)*sin(2*chi))
c         + x4*(ar(4,p) + br(4,p)*cos(2*chi) + cr(4,p)*sin(2*chi))
c         + x5*(ar(5,p) + br(5,p)*cos(2*chi) + cr(5,p)*sin(2*chi))
c         + x6*(ar(6,p) + br(6,p)*cos(2*chi) + cr(6,p)*sin(2*chi))
c         + x7*(ar(7,p) + br(7,p)*cos(2*chi) + cr(7,p)*sin(2*chi))
c         + x8*(ar(8,p) + br(8,p)*cos(2*chi) + cr(8,p)*sin(2*chi))
c  Similarly the ai,bi,ci arrays are the coefficients for the
c  "imaginary" equations.  The unknowns x1,x2,...,x8 are
c    q,u,theta1,real(D1),aimag(D1),theta2,real(D2),aimag(D2).
c  "p" varies over XX,YY,XY and YX.
c
c  We also need tables of coefficients of xi by xj, which involve
c  products of trig functions with themselves.
c
c  The coefficient arrays Ccos is defined as:
c
c    Ccos(i,j,p)*cos(2*chi)
c      = coefficient for sum of the terms in cosine.
c      = Sum (ar(i,p)*br(j,p) + ar(j,p)*br(i,p))*cos(2*chi)
c      =(ar(i,p)*br(j,p) + ar(j,p)*br(i,p))*Sum(cos(2*chi))
c  Similarly Csin is coefficient for sum of sine terms.
c            Csin2                          sine squared terms.
c            Ccossin                        cos-sin terms.
c            Cconst                         constant terms.
c  Because this task only stores Sum(sin**2) (and not Sum(cos**2)),
c  we use cos**2 = 1 - sin**2, so the cos**2 terms get included into the
c  sin**2 and constant term.
c
c  Input:
c    flux       The source flux (I,Q,U,V).
c  Output:
c    ar,ai      Coefficients for the constant terms.
c    br,bi      Coefficients for the cosine terms.
c    cr,ci      Coefficients for the sine terms.
c    Cconst
c    Ccos
c    Csin
c    Csin2
c    Ccossin
c-----------------------------------------------------------------------
c       integer qq,uu,theta1,theta2,D1r,D1i,D2r,D2i
c       parameter(qq=1,uu=2,theta1=3,D1r=4,D1i=5,theta2=6,D2r=7,D2i=8)
      integer i,j,m

      real fluxd(4)
      integer arl(0:8,4),ail(0:8,4),brl(0:8,4),bil(0:8,4)
      integer crl(0:8,4),cil(0:8,4)
      integer arc(0:8,4),aic(0:8,4),brc(0:8,4),bic(0:8,4)
      integer crc(0:8,4),cic(0:8,4)

c                 0  Q  U T1 D1r i T2 D2r i   0  Q  U T1 D1r i T2 D2r i
      data arl/ 1, 0, 0, 0, 0,-4, 0, 0,-4,  1, 0, 0, 0, 0, 4, 0, 0, 4,
     *          0, 0, 0, 4, 1, 0,-4, 1, 0,  0, 0, 0,-4, 1, 0, 4, 1, 0/
      data ail/ 0, 0, 0, 1, 4, 0,-1,-4, 0,  0, 0, 0, 1,-4, 0,-1, 4, 0,
     *         -4, 0, 0, 0, 0, 1, 0, 0,-1,  4, 0, 0, 0, 0, 1, 0, 0,-1/
      data brl/ 2, 0, 0, 0, 3, 0, 0, 3, 0, -2, 0, 0, 0, 3, 0, 0, 3, 0,
     *          3, 0, 1, 0,-2, 0, 0, 2, 0,  3, 0, 1, 0, 2, 0, 0,-2, 0/
      data bil/ 0, 0, 0, 2, 0, 3,-2, 0,-3,  0, 0, 0,-2, 0, 3, 2, 0,-3,
     *          0, 0, 0, 3, 0,-2,-3, 0,-2,  0, 0, 0, 3, 0, 2,-3, 0, 2/
      data crl/ 3, 0, 0, 0,-2, 0, 0,-2, 0, -3, 0, 0, 0,-2, 0, 0,-2, 0,
     *         -2,-1, 0, 0,-3, 0, 0, 3, 0, -2,-1, 0, 0, 3, 0, 0,-3, 0/
      data cil/ 0, 0, 0, 3, 0,-2,-3, 0, 2,  0, 0, 0,-3, 0,-2, 3, 0, 2,
     *          0, 0, 0,-2, 0,-3, 2, 0,-3,  0, 0, 0,-2, 0, 3, 2, 0, 3/

c                 0  Q  U T1 D1r i T2 D2r i   0  Q  U T1 D1r i T2 D2r i
      data arc/ 1, 0, 0, 0, 0, 0, 0, 0, 0,  4, 0, 0, 0, 0, 0, 0, 0, 0,
     *          0, 0, 0, 0, 4, 0, 0, 1, 0,  0, 0, 0, 0, 1, 0, 0, 4, 0/
      data aic/ 0, 0, 0, 1, 0, 0,-1, 0, 0,  0, 0, 0, 4, 0, 0,-4, 0, 0,
     *          0, 0, 0, 0, 0, 4, 0, 0,-1,  0, 0, 0, 0, 0, 1, 0, 0,-4/
      data brc/ 0, 0, 0, 0, 2,-3, 0, 2,-3,  0, 0, 0, 0, 2, 3, 0, 2, 3,
     *          2, 1, 0, 3, 0, 0,-3, 0, 0,  2, 1, 0,-3, 0, 0, 3, 0, 0/
      data bic/ 0, 0, 0, 0, 3, 2, 0,-3,-2,  0, 0, 0, 0,-3, 2, 0, 3,-2,
     *         -3, 0,-1, 2, 0, 0,-2, 0, 0,  3, 0, 1, 2, 0, 0,-2, 0, 0/
      data crc/ 0, 0, 0, 0, 3, 2, 0, 3, 2,  0, 0, 0, 0, 3,-2, 0, 3,-2,
     *          3, 0, 1,-2, 0, 0, 2, 0, 0,  3, 0, 1, 2, 0, 0,-2, 0, 0/
      data cic/ 0, 0, 0, 0,-2, 3, 0, 2,-3,  0, 0, 0, 0, 2, 3, 0,-2,-3,
     *          2, 1, 0, 3, 0, 0,-3, 0, 0, -2,-1, 0, 3, 0, 0,-3, 0, 0/
c-----------------------------------------------------------------------
c
c  Get the coefficient arrays. These are determined from the ari,aii,etc
c  arrays. If ari(i,j) is non-zero
c    ar(i,j) = sign(flux(abs(ari(i,j))),ari(i,j))
c
      if (circular) then
        fluxd(1) = flux(1) + flux(4)
        fluxd(2) = flux(2)
        fluxd(3) = flux(3)
        fluxd(4) = flux(1) - flux(4)
        call Fill(fluxd,8,arc,ar)
        call Fill(fluxd,8,aic,ai)
        call Fill(fluxd,8,brc,br)
        call Fill(fluxd,8,bic,bi)
        call Fill(fluxd,8,crc,cr)
        call Fill(fluxd,8,cic,ci)
      else
        call Fill(flux,8,arl,ar)
        call Fill(flux,8,ail,ai)
        call Fill(flux,8,brl,br)
        call Fill(flux,8,bil,bi)
        call Fill(flux,8,crl,cr)
        call Fill(flux,8,cil,ci)
      endif
c
c  Work out the product coefficients.
c
      do m = 1, 4
        do j = 0, 8
          do i = 0, 8
            Cconst(i,j,m) = ar(i,m)*ar(j,m) + br(i,m)*br(j,m)
     *                    + ai(i,m)*ai(j,m) + bi(i,m)*bi(j,m)
            Csin2(i,j,m)  = cr(i,m)*cr(j,m) - br(i,m)*br(j,m)
     *                    + ci(i,m)*ci(j,m) - bi(i,m)*bi(j,m)
            Csin(i,j,m)   = ar(i,m)*cr(j,m) + ar(j,m)*cr(i,m)
     *                    + ai(i,m)*ci(j,m) + ai(j,m)*ci(i,m)
            Ccos(i,j,m)   = ar(i,m)*br(j,m) + ar(j,m)*br(i,m)
     *                    + ai(i,m)*bi(j,m) + ai(j,m)*bi(i,m)
            Ccossin(i,j,m)= br(i,m)*cr(j,m) + br(j,m)*cr(i,m)
     *                    + bi(i,m)*ci(j,m) + bi(j,m)*ci(i,m)
          enddo
        enddo
      enddo

      end

******************************************************************* Fill

      subroutine Fill(flux,n,ar0,ar)

      integer n
      real flux(4),ar(0:n,4)
      integer ar0(0:n,4)
c-----------------------------------------------------------------------
c  Fill in a coefficient array.
c-----------------------------------------------------------------------
      integer i,j,s,Indx
c-----------------------------------------------------------------------
      do j = 1, 4
        do i = 0, n
          if (ar0(i,j).ne.0) then
            s = sign(1,ar0(i,j))
            Indx = abs(ar0(i,j))
            ar(i,j) = s*flux(Indx)
          else
            ar(i,j) = 0
          endif
        enddo
      enddo
      end

*************************************************************** GetVarNo

      subroutine GetVarNo(nants,nbl,polsol,polref,qusolve,xysol,xyref,
     *  notrip,refant,present,QUvar,Tvar,Dvar,nvar,Vars)

      integer nants,nbl,refant
      logical xyref,polsol,polref,qusolve,xysol,present(nants),notrip
      integer QUvar,Tvar(2,nants),Dvar(2,2,nants),nvar,Vars(8,4,nbl)
c-----------------------------------------------------------------------
c  This assigns each of the things we have to solve for a number,
c  varying from 1 to nvar. If something is assigned a number of 0, this
c  means that we do not solve for this. Rather it is assumed to be 0.
c
c  Input:
c    nants,nbl  Number of antennae and baselines.
c    present    True if the antenna is present in the data.
c    qusolve,xysol,xyref,polref,polsol,notrip Logicals, which determine
c               what we solve for.
c    refant     The reference antenna number.
c  Output:
c    QUvar      Gives the variable number of Q
c    Tvar       Gives the variable numbers of the phase errors.
c    Dvar       Gives the variable numbers of the leakage terms. The 3
c               indices of this are for real/imag, X/Y, nants.
c    Vars       Gives the variable numbers for all the variables in a
c               given set of equations.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,j,k,l,Uvar
      integer p1(4),p2(4)
      data p1/X,Y,X,Y/
      data p2/X,Y,Y,X/
c-----------------------------------------------------------------------
c
c  Determine a "variable number" for all the unknowns.
c
      nvar = 0
      if (qusolve) then
        QUvar = 1
        nvar = nvar + 2
      else
        QUvar = 0
      endif
      do i = 1, nants
        if (xysol .and. i.ne.refant .and. present(i)) then
          Tvar(X,i) = nvar + 1
          Tvar(Y,i) = nvar + 2
          nvar = nvar + 2
        else if (i.eq.refant .and. xyref) then
          Tvar(X,i) = 0
          Tvar(Y,i) = nvar + 1
          nvar = nvar + 1
        else
          Tvar(X,i) = 0
          Tvar(Y,i) = 0
        endif
c
c  Do not solve for leakage terms of missing antennas!
c
        if (.not.present(i) .or. .not.polsol) then
          Dvar(1,X,i) = 0
          Dvar(2,X,i) = 0
          Dvar(1,Y,i) = 0
          Dvar(2,Y,i) = 0
c
c  Solve for leakage terms if its not the reference antenna, or if
c  "polref" was specified.
c
        else if (i.ne.refant .or. polref) then
          if (qusolve .and. i.eq.refant) then
            Dvar(1,X,i) = 0
          else
            Dvar(1,X,i) = nvar + 1
            nvar = nvar + 1
          endif
          Dvar(2,X,i) = nvar + 1
          Dvar(1,Y,i) = nvar + 2
          Dvar(2,Y,i) = nvar + 3
          nvar = nvar + 3
c
c  Do not solve for the leakage terms of the X feed of the reference
c  antenna if "polref" was not specified.  Do not solve for the
c  characteristics of the Y antenna if there are no closures (we
c  cannot).
c
        else
          Dvar(1,X,i) = 0
          Dvar(2,X,i) = 0
          if (notrip) then
            Dvar(1,Y,i) = 0
            Dvar(2,Y,i) = 0
          else
            Dvar(1,Y,i) = nvar + 1
            Dvar(2,Y,i) = nvar + 2
            nvar = nvar + 2
          endif
        endif
      enddo
      if (nvar.lt.1) call bug('f','No variables to solve for??')
c
c  In any given equation in the least squares solution, 8 unknowns
c  could be present. Determine a table that gives the variable number
c  of these 8 unknowns. This table varies with baseline and XX,YY,XY,YX.
c
      Uvar = 0
      if (QUvar.gt.0) Uvar = QUvar + 1
      do k = 1, nbl
        do l = 1, 4
          Vars(1,l,k) = QUvar
          Vars(2,l,k) = Uvar
        enddo
      enddo

      k = 0
      do j = 2, nants
        do i = 1, j-1
          k = k + 1
          do l = 1, 4
            Vars(3,l,k) = Tvar(p1(l),i)
            Vars(4,l,k) = Dvar(1,p1(l),i)
            Vars(5,l,k) = Dvar(2,p1(l),i)
            Vars(6,l,k) = Tvar(p2(l),j)
            Vars(7,l,k) = Dvar(1,p2(l),j)
            Vars(8,l,k) = Dvar(2,p2(l),j)
          enddo
        enddo
      enddo
      end

**************************************************************** UnpackD

      subroutine UnpackD(Indx1,Indx2,x,nvar,D,epsi)

      integer Indx1,Indx2,nvar
      complex D
      real x(nvar),epsi
c-----------------------------------------------------------------------
c  Unpack a leakage term.
c
c  Input:
c    Indx1,Indx2 The indices of the real and imaginary parts.
c    nvar        The number of variables.
c    x           The variables.
c  Input/Output:
c    epsi       Error measure.
c  Output:
c    D          The leakage increment.
c-----------------------------------------------------------------------
      real t1,t2
c-----------------------------------------------------------------------
      if (Indx1.gt.0) then
        t1 = x(Indx1)
      else
        t1 = 0
      endif
      if (Indx2.gt.0) then
        t2 = x(Indx2)
      else
        t2 = 0
      endif

      epsi = max(epsi,t1**2+t2**2)
      D = cmplx(t1,t2)
      end

**************************************************************** UnpackT

      subroutine UnpackT(Indx,x,nvar,Gain,epsi)

      integer Indx,nvar
      real x(nvar),epsi
      complex Gain
c-----------------------------------------------------------------------
c  Unpack a phase angle, theta, (which is assumed to be small), and
c  return exp(i*theta).
c
c  Input:
c    Indx
c    x
c    nvar
c  Input/Output:
c    epsi
c  Output:
c    Gain
c-----------------------------------------------------------------------
      real t1,t2
c-----------------------------------------------------------------------
      if (Indx.eq.0) then
        Gain = (1.0,0.0)
      else
        t1 = x(Indx)
        t2 = 1.0/sqrt(1+t1*t1)
        Gain = cmplx(t2,t1*t2)
        epsi = max(epsi,t1**2)
      endif

      end

**************************************************************** DatRead

      subroutine DatRead(tIn,polsol,nants,nbl,maxsoln,
     *  interval,refant,minant,nfbin,time,nsoln,present,notrip,
     *  Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,
     *  source,freq)

      integer nsoln,nants,nbl,maxsoln,tIn,refant,minant,nfbin
      logical polsol,present(nants),notrip
      double precision interval(2),freq(0:nfbin)
      character source*(*)
      complex Vis(4,nbl,maxsoln,0:nfbin)
      complex VisCos(4,nbl,maxsoln,0:nfbin)
      complex VisSin(4,nbl,maxsoln,0:nfbin)
      real SumS(nbl,0:maxsoln,0:nfbin),SumC(nbl,0:maxsoln,0:nfbin)
      real SumS2(nbl,0:maxsoln,0:nfbin),SumCS(nbl,0:maxsoln,0:nfbin)
      integer Count(nbl,0:maxsoln,0:nfbin)
      double precision time(maxsoln)
c-----------------------------------------------------------------------
c  Read in the visibility data and form sums of things that we will need
c  in the gain solution stages.
c
c  Inputs:
c    maxsoln    Max number of solutions that we can deal with.
c    nbl        Number of baselines.
c    nants      Number of antennae.
c    polsol     If true, then process XY and YX as well as just XX, YY.
c    interval   Solution time interval.
c    refant     The reference antenna number.  This must always be
c               present.
c    minant     The minimum number of antennae to attempt a solution.
c  Output:
c    time       The time for each solution interval.
c    nsoln      Number of solution intervals.
c    present    Logical indicating whether the antenna is present at
c               all.
c    notrip     Flag that is true if there are no closures present.
c    Vis,VisCos,VisSin,SumC,SumS,SumS2,SumCS,Count
c               Accumulated statistics.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,j,k,l,i1,i2,bl,totvis,ngood,nauto,nchan,nbad,n,nread
      integer ncorr(0:MAXFBIN),nr,b1,b2,b3,nfreq(0:MAXFBIN)
      double precision preamble(4),tfirst,tlast,dfreq(0:MAXFBIN)
      double precision sfreq(MAXCHAN)
      real Cos2Chi,Sin2Chi,chi
      logical accept,flag(MAXCHAN,4),okscan,trip,tied(MAXANT),ok
      logical missing
      complex data(MAXCHAN,4),d(4,0:MAXFBIN)
      character line*80
c
c  Externals.
c
      character itoaf*8,stcat*80
      logical doaccept
c-----------------------------------------------------------------------
      call uvrewind(tIn)
      call uvDatRd(preamble,Data(1,XX),flag(1,XX),MAXCHAN,nchan)
      if (nchan.eq.0) call bug('f','No data read from input file')
c
c  Get the source and frequency of the first data.
c
      call uvrdvra(tIn,'source',source,' ')
c      call uvfit1(tIn,'frequency',nchan,freq,epsi)
      call defsmodl(tIn)
c
c  Get the data. Read the remaining correlations for this record.
c
      n = 0
      nr = 2
      if (polsol) nr = 4
      nsoln = 0
      tfirst = 0
      tlast = 0
      totvis = 0
      ngood = 0
      nbad = 0
      nauto = 0
      do k=0,nfbin
        freq(k)=0
        nfreq(k)=0
      enddo
      do while (nchan.gt.0)
        totvis = totvis + 1
        call uvinfo(tIn,'sfreq',sfreq)
c
c  Read the other polarisations and determine the overall flags.
c
        do j = 2, nr
          call uvDatRd(preamble,Data(1,j),flag(1,j),MAXCHAN,nread)
          if (nread.ne.nchan)
     *      call bug('f','Inconsistent number of polarisation chans')
        enddo
c
c  Accumulate the polarisations.
c
        do k=0,nfbin
          ncorr(k) = 0
          d(XX,k) = 0
          d(YY,k) = 0
          d(XY,k) = 0
          d(YX,k) = 0
          dfreq(k) = 0
        enddo
        k = 0
        if (polsol) then
          do j = 1, nchan
            if (flag(j,XX) .and. flag(j,YY) .and.
     *          flag(j,XY) .and. flag(j,YX)) then
              if (nfbin.gt.0) then
                k = min(nfbin,(j-1)/(nchan/nfbin)+1)
                ncorr(k) = ncorr(k) + 1
                d(XX,k) = d(XX,k) + data(j,XX)
                d(YY,k) = d(YY,k) + data(j,YY)
                d(XY,k) = d(XY,k) + data(j,XY)
                d(YX,k) = d(YX,k) + data(j,YX)
                dfreq(k)=dfreq(k)+log(sfreq(j))
              endif
              ncorr(0) = ncorr(0) + 1
              d(XX,0) = d(XX,0) + data(j,XX)
              d(YY,0) = d(YY,0) + data(j,YY)
              d(XY,0) = d(XY,0) + data(j,XY)
              d(YX,0) = d(YX,0) + data(j,YX)
              dfreq(0)=dfreq(0)+log(sfreq(j))
            endif
          enddo
        else
          do j = 1, nchan
            if (flag(j,XX) .and. flag(j,YY)) then
              if (nfbin.gt.0) then
                k = min(nfbin,(j-1)/(nchan/nfbin)+1)
                ncorr(k) = ncorr(k) + 1
                d(XX,k) = d(XX,k) + data(j,XX)
                d(YY,k) = d(YY,k) + data(j,YY)
                dfreq(k)=dfreq(k)+log(sfreq(j))
              endif
              ncorr(0) = ncorr(0) + 1
              d(XX,0) = d(XX,0) + data(j,XX)
              d(YY,0) = d(YY,0) + data(j,YY)
              dfreq(0)=dfreq(0)+log(sfreq(j))
            endif
          enddo
        endif
c
c  Determine if we will accept this visibility.
c
        call basant(preamble(4),i1,i2)
        accept = ncorr(0).gt.0 .and. i1.gt.0 .and. i1.le.nants .and.
     *                            i2.gt.0 .and. i2.le.nants .and.
     *                            i1.ne.i2
c
c  If its a good record, process it.  Initialise a new scan slot if
c  needed.
c
        if (accept) then
          if (nsoln.eq.0 .or.
     *        preamble(3).gt.tfirst+interval(1) .or.
     *        preamble(3).gt.tlast+interval(2)) then
            if (nsoln.ne.0) then
              time(nsoln) = (tfirst+tlast)/2
              okscan = doaccept(time(nsoln),Count(1,nsoln,0),
     *                                n,nants,nbl,refant,minant)
              if (okscan) then
                ngood = ngood + n
              else
                nsoln = nsoln - 1
              endif
            endif

            nsoln = nsoln + 1
            n = 0

            if (nsoln.gt.maxsoln)
     *        call bug('f','Too many solution intervals')
            tfirst = preamble(3)
            tlast  = tfirst
            do k = 0, nfbin
              do j = 1, nbl
                Count(j,nsoln,k) = 0
                SumS(j,nsoln,k)  = 0
                SumC(j,nsoln,k)  = 0
                SumS2(j,nsoln,k) = 0
                SumCS(j,nsoln,k) = 0
                do i = 1, 4
                  Vis(i,j,nsoln,k) = (0.0,0.0)
                  VisCos(i,j,nsoln,k) = (0.0,0.0)
                  VisSin(i,j,nsoln,k) = (0.0,0.0)
                enddo
              enddo
            enddo
          else if (preamble(3).lt.tfirst) then
            call bug('f','Data does not appear to be in time order')
          endif

          tlast = max(tlast,preamble(3))

          bl = (i2-1)*(i2-2)/2 + i1
c
c  Accumulate the statistics.
c
          call uvrdvrr(tIn,'chi',chi,0.0)
          Cos2Chi = cos(2*chi)
          Sin2Chi = sin(2*chi)
          do k = 0, nfbin
            do i = 1, 4
              Vis(i,bl,nsoln,k) = Vis(i,bl,nsoln,k)       + d(i,k)
              VisCos(i,bl,nsoln,k) = VisCos(i,bl,nsoln,k) 
     *           + d(i,k)*Cos2Chi
              VisSin(i,bl,nsoln,k) = VisSin(i,bl,nsoln,k)
     *           + d(i,k)*Sin2Chi
            enddo
            Count(bl,nsoln,k) = Count(bl,nsoln,k) + ncorr(k)
            SumS(bl,nsoln,k)  = SumS(bl,nsoln,k)  + ncorr(k) * Sin2Chi
            SumC(bl,nsoln,k)  = SumC(bl,nsoln,k)  + ncorr(k) * Cos2Chi
            SumCS(bl,nsoln,k) = SumCS(bl,nsoln,k)
     *         + ncorr(k) * Cos2Chi*Sin2Chi
            SumS2(bl,nsoln,k) = SumS2(bl,nsoln,k)
     *         + ncorr(k) * Sin2Chi*Sin2Chi
            nfreq(k) = nfreq(k) + ncorr(k)
            freq(k) = freq(k) + dfreq(k)
          enddo
          n = n + 1
        else if (i1.eq.i2) then
          nauto = nauto + 1
        else
          nbad = nbad + 1
        endif
        call uvDatRd(preamble,Data(1,XX),flag(1,XX),MAXCHAN,nchan)
      enddo
c
c  Check if the last time interval is to be accepted.
c
      if (nsoln.ne.0) then
        time(nsoln) = (tfirst+tlast)/2
        okscan = doaccept(time(nsoln),Count(1,nsoln,0),
     *                        n,nants,nbl,refant,minant)
        if (okscan) then
          ngood = ngood + n
        else
          nsoln = nsoln - 1
        endif
      endif
c
c  Tell the user whats what.
c
      do k=0,nfbin
        if (nfreq(k).gt.0) freq(k) = exp(freq(k)/nfreq(k))
      enddo
      call output('Number of solution intervals: '//itoaf(nsoln))
      call output('Total visibilities read: '//itoaf(totvis))
      if (nauto.ne.0)
     *  call bug('w','Number of autocorrelations discarded: '//
     *  itoaf(nauto))
      if (nbad.ne.0)
     *  call bug('w','Number of flagged visibilities: '//
     *  itoaf(nbad))
      if (ngood+nauto+nbad.ne.totvis) call bug('w',
     *  'No. visibs failing refant or minant requirement: '//
     *  itoaf(totvis-ngood-nauto-nbad))
      call output('Number visibilities accepted: '//itoaf(ngood))
      if (nsoln.eq.0 .or. ngood.eq.0)
     *  call bug('f','No data to process!')
c
c  Accumulate all the sums into slot 0.
c 
      do k = 0, nfbin
        do j = 1, nbl
          Count(j,0,k) = Count(j,1,k)
          SumS(j,0,k)  = SumS(j,1,k)
          SumC(j,0,k)  = SumC(j,1,k)
          SumS2(j,0,k) = SumS2(j,1,k)
          SumCS(j,0,k) = SumCS(j,1,k)
        enddo

        do l = 2, nsoln
          do j = 1, nbl
            Count(j,0,k) = Count(j,0,k) + Count(j,l,k)
            SumS(j,0,k)  = SumS(j,0,k)  + SumS(j,l,k)
            SumC(j,0,k)  = SumC(j,0,k)  + SumC(j,l,k)
            SumS2(j,0,k) = SumS2(j,0,k) + SumS2(j,l,k)
            SumCS(j,0,k) = SumCS(j,0,k) + SumCS(j,l,k)
          enddo
        enddo
      enddo
c
c  Determine whether there are any closures present.
c
      trip = .false.
      do k = 3, nants
        do j = 2, k-1
          do i = 1, j-1
            b1 = (k-1)*(k-2)/2 + j
            b2 = (j-1)*(j-2)/2 + i
            b3 = (k-1)*(k-2)/2 + i
            trip = trip .or. (Count(b1,0,0).gt.0 .and.
     *                        Count(b2,0,0).gt.0 .and.
     *                        Count(b3,0,0).gt.0)
          enddo
        enddo
      enddo
      notrip = .not.trip
c
c  Determine whether the solution process is well-determined.
c
      do i = 1, nants
        tied(i) = .false.
        present(i) = .false.
      enddo
      tied(refant) = .true.

      do k = 1, nants-1
        do j = 2, nants
          do i = 1, j-1
            b1 = (j-1)*(j-2)/2 + i
            if (Count(b1,0,0).gt.0) then
              present(i) = .true.
              present(j) = .true.
              tied(i) = tied(i) .or. tied(j)
              tied(j) = tied(i)
            endif
          enddo
        enddo
      enddo
c
c  Give warning messages.
c
      missing = .false.
      line = 'No data present for antenna(s):'
      ok = .true.
      do i = 1, nants
        if (.not.present(i)) then
          missing = .true.
          line = stcat(line,' '//itoaf(i))
        endif
        if (present(i) .and. .not.tied(i)) then
          ok = .false.
          call bug('w',
     *      'Antenna not tied to reference antenna: '//itoaf(i))
        endif
      enddo
      if (.not.ok) call bug('f','Solution is degenerate')
      if (notrip) then
        call bug('w','No closures were present')
        call bug('w','Quality of solution will suffer')
      endif
      if (missing) call bug('w',line)
      end

*************************************************************** doaccept

      logical function doaccept(time,Count,n,nants,nbl,refant,minant)

      integer nants,nbl,refant,minant,n
      integer Count(nbl)
      double precision time
c-----------------------------------------------------------------------
c  Determine whether the reference antenna, and the minimum number of
c  antennae are present in the data.
c
c  Input:
c    time       Time corresponding to this interval.
c    n          Number of visibilities in this interval.
c    refant
c    minant
c    nants,nbl
c    Count      Number of visibility records for each baseline.
c  Output:
c    doaccept   True if we are to accept this solution interval.
c-----------------------------------------------------------------------
      include 'gpcal.h'

      integer i,j,k,npresent,lnvis
      logical present(MAXANT)
      character string*32,nvis*8,line*80

      integer   len1
      character itoaf*8
      external  itoaf, len1
c-----------------------------------------------------------------------

      do k = 1, nants
        present(k) = .false.
      enddo

      k = 0
      do j = 2, nants
        do i = 1, j-1
          k = k + 1
          if (Count(k).gt.0) then
            present(i) = .true.
            present(j) = .true.
          endif
        enddo
      enddo

      npresent = 0
      do k = 1, nants
        if (present(k)) npresent = npresent + 1
      enddo

      doaccept = npresent.ge.minant .and. present(refant)
      if (.not.doaccept) then
        nvis = itoaf(n)
        lnvis = len1(nvis)
        call JulDay(time,'H',string)
        line = 'Discarding '//nvis(1:lnvis)//
     *        ' visibilities, being the scan near '//string
        call bug('w',line)
      endif
      end

***************************************************************** PolIni

      subroutine PolIni(reset,tIn,xyphase,nxyphase,D,xyp,nants,
     *                                                vsolve,fac)

      integer tIn,nants,nxyphase
      real xyphase(nants),fac
      logical vsolve,reset
      complex D(2,nants),xyp(nants)
c-----------------------------------------------------------------------
c  Set the initial estimates of the gains and leakage terms.
c
c  Inputs:
c    reset      True if we are to start with a clean slate.
c    tIn        Handle of the input file.
c    xyphase    User-specified values of the xyphase.
c    nxyphase   Number of user-specified xyphases.
c    nants      Number of antennae.
c    vsolve     True if we are solving for Stokes-V
c  Output:
c    D          The initial leakage terms.
c    xyp        The initial xyphases.
c    fac        RMS of the gain table.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,n,item,iostat
      real theta,phase(MAXANT)
      integer count(MAXANT)
c
c  Externals.
c
      integer hsize
c-----------------------------------------------------------------------
c
c  Get the XY phases that implicitly exist in the gains table.
c  NOTE: The XY phases returned are the corrupting phases. These
c  would be phases introduced by the system. This is the negative of
c  the phases needed to correct for them.
c
      if (reset) then
        n = 0
        fac = 1
      else
        call GetXY(tIn,fac,phase,count,MAXANT,n)
      endif
      if (fac.le.0.or.n.eq.0) fac=1
      do i = n+1, nants
        count(i) = 0
      enddo
      do i = 1, nants
        if (i.le.nxyphase) then
          theta = pi/180 * xyphase(i)
          xyp(i) = cmplx(cos(theta),sin(theta))
        else if (count(i).gt.0) then
          theta = phase(i)
          xyp(i) = cmplx(cos(theta),sin(theta))
        else
          xyp(i) = (1.0,0.0)
        endif
      enddo
c
c  Initialise the leakage terms. See if there is already a leakage
c  table in the input file. If so use this as the initial estimate.
c  Otherwise just set things to zero.
c
      if (reset) then
        iostat = -1
      else
        call haccess(tIn,item,'leakage','read',iostat)
      endif
      if (iostat.eq.0) then
        call output(
     *    'Using leakage parameters from input as initial guess')
        n = min((hsize(item)-8)/16,nants)
        call hreadr(item,D,8,16*n,iostat)
        if (iostat.ne.0) then
          call bug('w','Error reading leakage table from input')
          call bugno('f',iostat)
        endif
        call hdaccess(item,iostat)
      else
        if (vsolve) call bug('f',
     *      'A leakage table must exist to use options=vsolve')
         n = 0
      endif
      do i = n+1, nants
        D(X,i) = (0.0,0.0)
        D(Y,i) = (0.0,0.0)
      enddo

      end

**************************************************************** GainIni

      subroutine GainIni(nants,nsoln,nfbin,Gains,fac,xyp)

      integer nants,nsoln,nfbin
      complex Gains(2,nants,nsoln,0:nfbin),xyp(nants,0:nfbin)
      real fac
c-----------------------------------------------------------------------
c  Initialise the gains.
c
c  Input:
c    xyp        The xyphases.
c    fac        Estimate of gain amplitude.
c  Output:
c    Gains      Initial gains.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer i,j,k
c-----------------------------------------------------------------------
      do k = 0, nfbin
        do j = 1, nsoln
          do i = 1, nants
            Gains(X,i,j,k) = fac
            Gains(Y,i,j,k) = fac*xyp(i,k)
          enddo
        enddo
      enddo
      end

**************************************************************** GainTab

      subroutine GainTab(tIn,time,Gains,nants,nsoln,nfbin,freq)

      integer tIn
      integer nsoln,nants,nfbin
      double precision time(nsoln), freq(0:nfbin)
      complex Gains(2*nants,nsoln,0:nfbin)
c-----------------------------------------------------------------------
c  Write out the amp/phase gain tables.
c
c  Input:
c    tIn        Handle of the file.
c    time       Time (midpoint) for each solution interval.
c    Gains      The gains to be written out.
c    nants      Number of antenna.
c    nsoln      Number of solution intervals.
c    nfbin      Number of frequency bins
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer iostat,off,item,i,j,fbin
      complex G(2*MAXANT)
c-----------------------------------------------------------------------
      call haccess(tIn,item,'gains','write',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening output amp/phase table.')
        call bugno('f',iostat)
      endif
      call hwritei(item,0,0,4,iostat)
      if (iostat.ne.0) then
        call bug('w','Error writing header of amp/phase table')
        call bugno('f',iostat)
      endif
c
c  Write out all the continuum  gains.
c
      off = 8
      do i = 1, nsoln
        call hwrited(item,time(i),off,8,iostat)
        off = off + 8
        if (iostat.ne.0) then
          call bug('w','Error writing time to amp/phase table')
          call bugno('f',iostat)
        endif
        do j = 1, 2*nants
          if (abs(real(Gains(j,i,0)))+abs(aimag(Gains(j,i,0)))
     *          .ne.0) then
            G(j) = 1/Gains(j,i,0)
          else
            G(j) = (0.0,0.0)
          endif
        enddo
        call hwriter(item,G,off,8*2*nants,iostat)
        off = off + 8*2*nants
        if (iostat.ne.0) then
          call bug('w','Error writing gains to amp/phase table')
          call bugno('f',iostat)
        endif
      enddo
c
c  Finished writing the gain table.
c
      call hdaccess(item,iostat)
      if (iostat.ne.0) call bugno('f',iostat)
c
c  Now write out the other parameters that need to go along with this.
c
      call wrhdi(tIn,'nfeeds',2)
      call wrhdi(tIn,'ntau',0)
      call wrhdi(tIn,'ngains',2*nants)
      call wrhdi(tIn,'nsols',nsoln)
      call wrhdd(tIn,'interval',0.5d0)
      
      if (nfbin.le.1) return
c
c  Now write the gains for the frequency binned solutions
c
      call haccess(tIn,item,'gainsf','write',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening output gainsf table.')
        call bugno('f',iostat)
      endif
      call hwritei(item,0,0,4,iostat)
      call hwritei(item,nfbin,4,4,iostat)
      if (iostat.ne.0) then
        call bug('w','Error writing header of gainsf table')
        call bugno('f',iostat)
      endif
c
c  Write out all the gains.
c
      off = 8
      do fbin = 1, nfbin
        do i = 1, nsoln
          call hwrited(item,time(i),off,8,iostat)
          off = off + 8
          if (iostat.ne.0) then
            call bug('w','Error writing time to gainsf table')
            call bugno('f',iostat)
          endif
          do j = 1, 2*nants
            if (abs(real(Gains(j,i,fbin)))+abs(aimag(Gains(j,i,fbin)))
     *          .ne.0) then
              G(j) = 1/Gains(j,i,fbin)
            else
              G(j) = (0.0,0.0)
            endif
          enddo
          call hwriter(item,G,off,8*2*nants,iostat)
          off = off + 8*2*nants
          if (iostat.ne.0) then
            call bug('w','Error writing gains to gainsf table')
            call bugno('f',iostat)
          endif
        enddo
        call hwrited(item,freq(fbin),off,8,iostat)
        off = off + 8
      enddo
c
c  Finished writing the gain table.
c
      call hdaccess(item,iostat)
      if (iostat.ne.0) call bugno('f',iostat)
      call wrhdi(tIn,'nfbin',nfbin)

      end

*************************************************************** GainScal

      subroutine GainScal(flux,Gains,ngains,fac)

      integer ngains
      complex Gains(ngains)
      real flux(4),fac
c-----------------------------------------------------------------------
c  Scale the gains and the flux so that the rms gain is 1.
c
c  Input:
c    ngains     Number of gains.
c    fac        RMS of the original gains.
c  Input/Output:
c    Gains      The gains.
c    flux       Nominal source flux.
c-----------------------------------------------------------------------
      real Sum2,t
      integer n,i
c-----------------------------------------------------------------------
      n = 0
      Sum2 = 0.0
      do i = 1, ngains
        t = real(Gains(i))**2 + aimag(Gains(i))**2
        if (t.gt.0.0) then
          Sum2 = Sum2 + t
          n = n + 1
        endif
      enddo
c
c  Return if all the gains are flagged bad.
c
      if (Sum2.eq.0.0) return
c
c  Scale the gains.
c
      t = fac*sqrt(n/Sum2)
      do i = 1, ngains
        Gains(i) = t*Gains(i)
      enddo
c
c  Scale the fluxes.
c
      t = 1.0 / (t*t)
      do i = 1, 4
        flux(i) = t*flux(i)
      enddo

      end

**************************************************************** LeakTab

      subroutine LeakTab(tIn,D,nants,nfbin,freq)

      integer tIn,nants,nfbin
      double precision freq(0:nfbin)
      complex D(2,nants,0:nfbin)
c-----------------------------------------------------------------------
c  Save the polarisation gains in an item in the calibrator file.
c  This uses some dirty tricks. Firstly it uses wrhdc to create the
c  item (because there is no way in FORTRAN to make the correct header).
c  Second it uses hwriter, because there is no hwritec.
c
c  Input:
c    tIn        Handle of the calibrator file.
c    D          Leakage parameters.
c    nants      Number of antennae.
c    nfbin      Number of freq bins
c-----------------------------------------------------------------------
      integer item,iostat, off, k
c-----------------------------------------------------------------------
      call wrhdc(tIn,'leakage',(1.0,0.0))
      call haccess(tIn,item,'leakage','append',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening the output leakage table')
        call bugno('f',iostat)
      endif
c
c  The item alignment requirement requires that we start writing at
c  byte 8. Bytes 0-3 are the header, and bytes 4-7 are unused.
c  For v2+ table: use byte 4-7 to write version number
c
      off = 8
      call hwriter(item,D,off,2*8*nants,iostat)
      if (iostat.ne.0) then
        call bug('w','Error writing to the leakage table')
        call bugno('f',iostat)
      endif
      call hdaccess(item,iostat)
      if (iostat.ne.0) call bugno('f',iostat)
      if (nfbin.le.1) return
c
c  Now write the frequency binned leakages
c
      call haccess(tIn,item,'leakagef','write',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening the output leakagef table')
        call bugno('f',iostat)
      endif
      off = 4
      call hwritei(item,nfbin,off,4,iostat)
      off = 8
      do k = 1, nfbin
        call hwriter(item,D(1,1,k),off,2*8*nants,iostat)
        off = off + 2*8*nants
        call hwrited(item,freq(k),off,8, iostat)
        off = off + 8
      enddo
      if (iostat.ne.0) then
        call bug('w','Error writing to the leakagef table')
        call bugno('f',iostat)
      endif
      call hdaccess(item,iostat)
      if (iostat.ne.0) call bugno('f',iostat)
      end

***************************************************************** writeo

      subroutine writeo(tIn,line)

      integer tIn
      character line*(*)
c-----------------------------------------------------------------------
c  Write out a line to the history file (if open) and the output.
c-----------------------------------------------------------------------
      character string*80
c-----------------------------------------------------------------------
      string = 'GPCAL: '//line
      call HisWrite(tIn,string)
      call output(line)
      end

***************************************************************** GetOpt

      subroutine GetOpt(dopass,amphsol,polsol,xysol,xyref,xyvary,
     *  polref,qusolve,vsolve,oldflux,circular,reset)

      logical dopass,amphsol,polsol,xysol,xyvary,xyref,polref
      logical qusolve,vsolve,oldflux,circular,reset
c-----------------------------------------------------------------------
c  Get processing options.
c
c  Output:
c    dopass     Apply bandpass correction if possible.
c    amphsol    Solve for antenna gains.
c    polsol     Solve for polarisation corrections.
c    xysol      Solve for the xy phases (except for the reference
c               antenna).
c    xyvary     XY phase may vary with time.
c    xyref      Solve for the xy phase of the reference.
c    polref     Solve for reference antenna misalignment and
c               differential ellipticity.
c    qusolve    Solve for Q and U as well as everything else.
c    vsolve     Solve for V as well as everything else.
c    oldflux    Use pre-Aug 1994 ATCA flux scale.
c    circular   Expect/handle circularly polarised feeds.
c    reset      Start GPCAL with a clean slate.
c-----------------------------------------------------------------------
      integer nopt
      parameter (nopt=13)
      logical present(nopt),linear
      character opts(nopt)*10
      data opts/'noamphase ','nopol     ','polref    ','noxy      ',
     *          'xyref     ','qusolve   ','xyvary    ','nopass    ',
     *          'oldflux   ','circular  ','vsolve    ','reset     ',
     *          'linear    '/
c-----------------------------------------------------------------------
      call options('options',opts,present,nopt)
      amphsol = .not.present(1)
      polsol  = .not.present(2)
      polref  = present(3)
      xysol   = .not.present(4)
      xyref   = present(5)
      qusolve = present(6)
      xyvary  = present(7)
      dopass  = .not.present(8)
      oldflux = present(9)
      circular= present(10)
      linear  = present(13) .or. .not.circular
      vsolve  = present(11)
      reset   = present(12)

      if (.not.polsol .and. polref .and.
     *  ((xysol .and. .not.xyvary) .or. qusolve)) then
        call bug('w','Unsupported combination of options')
        call bug('f','Include either the NOXY or XYVARY option')
      endif
      if (.not.xysol .and. xyref .and.
     *  (polsol .or. qusolve .or. xyvary .or. .not.polref))
     *  call bug('f','Unsupported combination of options')
      if (.not.xysol .and. xyvary) call bug('f',
     *  'Option NOXY cannot be used with XYVARY')
      if (.not.amphsol .and. .not.polsol .and. .not.polref .and.
     *   .not.qusolve .and. .not.xysol .and. .not.xyref) call bug('f',
     *  'The options inhibit solving for anything')
      if (.not.amphsol .and. (xysol .or. xyref)) call bug('f',
     *  'Using NOAMPHASE prevents solving for XY phases')
      if (polref .and. .not.xyref) call bug('w',
     *  'It is advisable to use option XYREF with POLREF')
      if (linear .and. circular) call bug('f',
     *  'Options linear and circular cannot be used together')
      if (vsolve .and. reset) call bug('f',
     *  'Options reset and vsolve cannot be used together')

      end

**************************************************************** getiquv

      subroutine getiquv(source,freq,oldflux,flux,defflux)

      character source*(*)
      double precision freq
      real      flux(4)
      logical   defflux, oldflux
c-----------------------------------------------------------------------
c     Provide a model of I,Q,U, and V for selected calibrators
c
c  Input:
c     source   Name of the source.
c     freq     Observing frequency.
c     oldflux  Use old 1934-638 flux density scale.
c  Output:
c     flux     I,Q,U, and V.  All set according to the some default
c              value.
c     defflux  True if the source was not found, and default of 1,0,0,0
c              used.
c-----------------------------------------------------------------------
      integer   ierr
      character src*16, umsg*80

      integer   len1
      external  len1
c-----------------------------------------------------------------------
      src = source
      if (src.eq.'1934-638' .or.
     *    src.eq.'1934'     .or.
     *    src.eq.'1939-637') then
        if (oldflux) then
          src = 'old1934'
          call bug('w','Using pre-Aug94 ATCA flux scale for 1934-638.')
        else
          src = '1934-638'
          call output('Using post-Aug94 ATCA flux scale for 1934-638.')
        endif
      endif

      ierr = 2
      if (src.ne.' ') call calstoke(src,'i',freq,flux(1),1,ierr)
      defflux = ierr.eq.2

      if (defflux) then
        flux(1) = 1.0
        flux(2) = 0.0
        flux(3) = 0.0
        flux(4) = 0.0

      else
        if (src.ne.'1934-638' .and. src.ne.'old1934') then
          call output(src(1:len1(source)) //
     *      ' is a recognised flux calibrator.')
        endif

        call calstoke(src,'q',freq,flux(2),1,ierr)
        call calstoke(src,'u',freq,flux(3),1,ierr)
        call calstoke(src,'v',freq,flux(4),1,ierr)
        if (ierr.eq.1) then
          call bug('w', 'Used frequency extrapolation to determine ' //
     *      'Stokes parameters.')
        endif

        write(umsg, 10) flux, freq
 10     format('Using IQUV =',f8.4,3(',',f8.4),' at',f9.4,' GHz.')
        call output(umsg)
      endif

      end

*************************************************************** PhaseSol

      subroutine PhaseSol(nbl,nants,SumVM,b1,b2,G,tol,epsi)

      integer nbl,nants
      integer b1(nbl),b2(nbl)
      real tol,epsi
      complex SumVM(4,nbl),G(nants)
c-----------------------------------------------------------------------
c  Solve for the phase corrections that minimise the error.  This uses
c  a nonlinear Jacobi iteration, as suggested by Fred Schwab in
c  "Adaptive calibration of radio interferomter data", SPIE Vol. 231,
c  1980 International Optical Computing Conference (pp 18-24).  The
c  damping heuristics are copied from AIPS ASCAL.
c
c  Input:
c    nbl        Number of baselines.
c    nants      Number of antennae.
c    b1,b2      This gives the antennae pair for each baseline.
c    SumVM      Sum of conjg(Vis)*Model
c    tol        Convergence tolerance.
c  Input/Output:
c    Gain       The antenna gain solution.
c    epsi       Chanle in solution during the iterations.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      complex SVM(MAXBASE),Sum(MAXANT)
      logical convrg
      real Factor,Change
      complex Temp
      integer Niter,i

      integer MAXITER
      parameter (MAXITER=100)
c-----------------------------------------------------------------------
c
c  Initialise. Remember the ratio of the Y to X gains, and then
c  sum the Y and X parts together.
c
      do i = 1, nants
        Sum(i) = 0
      enddo

      do i = 1, nbl
        SVM(i) = SumVM(XX,i) + SumVM(YY,i)
      enddo

      Factor = 0.8
      if (Nants.le.6) Factor = 0.5
c
c  Iterate.
c
      Convrg = .false.
      Niter = 0
      do while (.not.Convrg .and. Niter.lt.MAXITER)
        Niter = Niter + 1
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
        do i = 1, nbl
          Sum(b1(i)) = Sum(b1(i)) + G(b2(i)) *       SVM(i)
          Sum(b2(i)) = Sum(b2(i)) + G(b1(i)) * conjg(SVM(i))
        enddo
c
c  Update the gains.  The following will be flagged as a short loop on
c  the Cray, if we assume we have fewer than 32 antennae.  Hopefully it
c  will vectorise.  For "typical" cases, the absolute value function in
c  the next loop takes up about 30-40% of the run time of this routine
c  on a VAX.
c
        Change = 0

        do i = 1, nants
          Temp = (Sum(i)/abs(Sum(i)))
          Temp = G(i) + Factor * (Temp - G(i))
          Temp = Temp/abs(Temp)
          Change = Change + real(G(i)-Temp)**2
     *                    + aimag(G(i)-Temp)**2
          G(i) = Temp
          Sum(i) = (0.0,0.0)
        enddo
        epsi = max(epsi,Change/nants)
        Convrg = Change/nants.lt.tol
      enddo

      end

**************************************************************** AmpSol0

      subroutine AmpSol0(nbl,nants,SumVM,SumMM,b1,b2,G,axy,epsi)

      integer nbl,nants,b1(nbl),b2(nbl)
      complex SumVM(4,nbl),G(nants)
      real SumMM(4,nbl),axy(nants),epsi
c-----------------------------------------------------------------------
c  Get the first estimate at the amplitude gain. Do this by getting the
c  phase-only solution, and then scaling to the average amplitude.
c
c  Input:
c    nbl        Number of baselines.
c    nants      Number of antennae.
c    SumVM,SumMM Accumulated rubbish used in the solution step.
c    b1,b2      Antenna numbers for each sum.
c  Input/Output:
c    G          The gains.
c    epsi       Solution change.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      real tol
      parameter (tol=1e-5)
      integer i
      real SumRMM,SumRVM,alpha
c-----------------------------------------------------------------------
c
c  Get the phase solution.
c
      call PhaseSol(nbl,nants,SumVM,b1,b2,G,tol,epsi)
c
c  Determine a scale factor to multiply the gains by, to make them about
c  the correct amplitude.
c
      SumRVM = 0
      SumRMM = 0
      do i = 1, nbl
        SumRVM = SumRVM + conjg(G(b1(i)))*G(b2(i))*
     *     (SumVM(XX,i) + axy(b1(i))*axy(b2(i))*SumVM(YY,i))
        SumRMM = SumRMM +   G(b1(i))*G(b2(i))  *
     *                conjg(G(b1(i))*G(b2(i))) *
     *      (SumMM(XX,i) + (axy(b1(i))*axy(b2(i)))**2 * SumMM(YY,i))
      enddo
      alpha = sqrt(abs(SumRVM / SumRMM))

      do i = 1, nants
        G(i) = alpha * G(i)
      enddo

      end

**************************************************************** AmpSol2

      subroutine AmpSol2(nbl,nants,SumVM,SumMM,b1,b2,Gx,Gy,tol,epsi)

      integer nbl,nants,b1(nbl),b2(nbl)
      complex SumVM(4,nbl),Gx(nants),Gy(nants)
      real SumMM(4,nbl),epsi,tol
c-----------------------------------------------------------------------
c  Determine the amplitude/phase solution for a given time interval.
c  The complex gain of the X and Y feeds are determined.  The XY phase
c  or amplitude ratio *is not* constrained and allowed to vary as
c  dictated by the data etc.
c
c  Inputs:
c    nbl,nants  Number of baselines and number of antennae.
c    SumMM,SumVM Accumulated rubbish used in the solution process.
c    b1,b2      Antenna numbers of the given baseline.
c    tol        Tolerance in determining the solutions.
c  Input/Output:
c    Gx         Current estimate of the X gains.
c    Gy         Current estimate of the Y gains.
c  Output:
c    epsi       Fractional change in gains.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer MAXITER
      parameter (MAXITER=100)

      integer i,niter
      logical convrg
      real t,Factor,ChangeX,ChangeY,SumWtX,SumWtY,t1,t2,t3,t4
      real Sum2(2,MAXANT)
      complex Sum(2,MAXANT),Temp

      real Factors(11)
      data Factors/0.5,0.75,8*0.9,0.5/
c-----------------------------------------------------------------------
c
c  Initialise.
c
      do i = 1, nants
        Sum(X,i) = (0.0,0.0)
        Sum(Y,i) = (0.0,0.0)
        Sum2(X,i) = 0.0
        Sum2(Y,i) = 0.0
      enddo

      convrg = .false.
      niter = 0
      do while (.not.convrg .and. niter.lt.maxiter)
        niter = niter + 1
c
c  Get the same damping factor as AIPS.
c
        if (nants.le.6) then
          factor = 0.5
        else
          factor = factors(min(11,niter))
        endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop contains a dependency (it should not vectorise).
c
        do i = 1, nbl
          Sum(X,b1(i))  = Sum(X,b1(i))
     *                        + Gx(b2(i)) *       SumVM(XX,i)
     *                        + Gy(b2(i)) *       SumVM(XY,i)
          Sum(X,b2(i))  = Sum(X,b2(i))
     *                        + Gx(b1(i)) * conjg(SumVM(XX,i))
     *                        + Gy(b1(i)) * conjg(SumVM(YX,i))

          t1 = real(Gx(b2(i)))
          t2 = aimag(Gx(b2(i)))
          t3 = real(Gy(b2(i)))
          t4 = aimag(Gy(b2(i)))
          Sum2(X,b1(i)) = Sum2(X,b1(i)) + (t1*t1+t2*t2)*SumMM(XX,i) +
     *                                    (t3*t3+t4*t4)*SumMM(XY,i)
          t1 = real(Gx(b1(i)))
          t2 = aimag(Gx(b1(i)))
          t3 = real(Gy(b1(i)))
          t4 = aimag(Gy(b1(i)))
          Sum2(X,b2(i)) = Sum2(X,b2(i)) + (t1*t1+t2*t2)*SumMM(XX,i) +
     *                                    (t3*t3+t4*t4)*SumMM(YX,i)

          Sum(Y,b1(i))  = Sum(Y,b1(i))
     *                        + Gy(b2(i)) *       SumVM(YY,i)
     *                        + Gx(b2(i)) *       SumVM(YX,i)
          Sum(Y,b2(i))  = Sum(Y,b2(i))
     *                        + Gy(b1(i)) * conjg(SumVM(YY,i))
     *                        + Gx(b1(i)) * conjg(SumVM(XY,i))

          t1 = real(Gy(b2(i)))
          t2 = aimag(Gy(b2(i)))
          t3 = real(Gx(b2(i)))
          t4 = aimag(Gx(b2(i)))
          Sum2(Y,b1(i)) = Sum2(Y,b1(i)) + (t1*t1+t2*t2)*SumMM(YY,i)
     *                                  + (t3*t3+t4*t4)*SumMM(YX,i)
          t1 = real(Gy(b1(i)))
          t2 = aimag(Gy(b1(i)))
          t3 = real(Gx(b1(i)))
          t4 = aimag(Gx(b1(i)))
          Sum2(Y,b2(i)) = Sum2(Y,b2(i)) + (t1*t1+t2*t2)*SumMM(YY,i)
     *                                  + (t3*t3+t4*t4)*SumMM(XY,i)
        enddo
c
c  Update the gains.
c
        ChangeX = 0
        SumWtX = 0
        ChangeY = 0
        SumWtY = 0

        do i = 1, nants
c
c  Evaluate X and Y gains.
c
          Temp = Sum(X,i) / Sum2(X,i) - Gx(i)
          Gx(i) = Gx(i) + Factor * Temp
          ChangeX = ChangeX + real(Temp)**2 + aimag(Temp)**2
          SumWtX = SumWtX + real(Gx(i))**2  + aimag(Gx(i))**2

          Temp = Sum(Y,i) / Sum2(Y,i) - Gy(i)
          Gy(i) = Gy(i) + Factor * Temp
          ChangeY = ChangeY + real(Temp)**2 + aimag(Temp)**2
          SumWtY = SumWtY + real(Gy(i))**2  + aimag(Gy(i))**2
c
c  Zero the accumulators.
c
          Sum(X,i) = 0
          Sum(Y,i) = 0
          Sum2(X,i) = 0
          Sum2(Y,i) = 0
        enddo
        t = max(ChangeX/SumWtX,ChangeY/SumWtY)
        epsi = max(epsi,t)
        convrg = t.lt.tol
      enddo

      end

**************************************************************** AmpSol1

      subroutine AmpSol1(nbl,nants,SumVM,SumMM,b1,b2,G,axy,tol,epsi)

      integer nbl,nants,b1(nbl),b2(nbl)
      complex SumVM(4,nbl),G(nants)
      real SumMM(4,nbl),axy(nants),epsi,tol
c-----------------------------------------------------------------------
c  Determine the amplitude/phase solution for a given time interval.
c  The complex gain of the X feed, and the X/Y amplitude gain ratio is
c  determined.  It it assumed that the XY phases have already been
c  applied to the YY data and that the phases of the XX and YY data
c  track each other exactly.
c
c  Inputs:
c    nbl,nants  Number of baselines and number of antennae.
c    SumMM,SumVM Accumulated rubbish used in the solution process.
c    b1,b2      Antenna numbers of the given baseline.
c    tol        Tolerance in determining the solutions.
c  Input/Output:
c    G          Current estimate of the X gains.
c    Axy        Current estimate of the X/Y amplitude ratio.
c  Output:
c    epsi       Fractional change in gains.
c-----------------------------------------------------------------------
      include 'gpcal.h'
      integer MAXITER
      parameter (MAXITER=100)

      integer i,niter
      logical convrg
      real t,Factor,ChangeX,ChangeY,SumWtX,SumWtY
      real t1,t2
      real Sum2(2,MAXANT)
      complex Sum(2,MAXANT),Temp

      real Factors(11)
      data Factors/0.5,0.75,8*0.9,0.5/
c-----------------------------------------------------------------------
c
c  Initialise.
c
      do i = 1, nants
        Sum(X,i) = (0.0,0.0)
        Sum(Y,i) = (0.0,0.0)
        Sum2(X,i) = 0.0
        Sum2(Y,i) = 0.0
      enddo

      convrg = .false.
      niter = 0
      do while (.not.convrg .and. niter.lt.maxiter)
        niter = niter + 1
c
c  Get the same damping factor as AIPS.
c
        if (nants.le.6) then
          factor = 0.5
        else
          factor = factors(min(11,niter))
        endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop contains a dependency (it should not vectorise).
c
        do i = 1, nbl
          Sum(X,b1(i))  = Sum(X,b1(i)) +
     *                   G(b2(i)) *       SumVM(XX,i)
          Sum(X,b2(i))  = Sum(X,b2(i)) +
     *                   G(b1(i)) * conjg(SumVM(XX,i))

          t1 = real(G(b2(i)))
          t2 = aimag(G(b2(i)))
          Sum2(X,b1(i)) = Sum2(X,b1(i)) + (t1*t1 + t2*t2)*SumMM(XX,i)
          t1 = real(G(b1(i)))
          t2 = aimag(G(b1(i)))
          Sum2(X,b2(i)) = Sum2(X,b2(i)) + (t1*t1 + t2*t2)*SumMM(XX,i)

          Sum(Y,b1(i))  = Sum(Y,b1(i)) +
     *      Axy(b2(i)) * G(b2(i)) *       SumVM(YY,i)
          Sum(Y,b2(i))  = Sum(Y,b2(i)) +
     *      Axy(b1(i)) * G(b1(i)) * conjg(SumVM(YY,i))

          t1 = Axy(b2(i))*real(G(b2(i)))
          t2 = Axy(b2(i))*aimag(G(b2(i)))
          Sum2(Y,b1(i)) = Sum2(Y,b1(i)) + (t1*t1 + t2*t2)*SumMM(YY,i)
          t1 = Axy(b1(i))*real(G(b1(i)))
          t2 = Axy(b1(i))*aimag(G(b1(i)))
          Sum2(Y,b2(i)) = Sum2(Y,b2(i)) + (t1*t1 + t2*t2)*SumMM(YY,i)
        enddo
c
c  Update the gains.
c
        ChangeX = 0
        SumWtX = 0
        ChangeY = 0
        SumWtY = 0

        do i = 1, nants
c
c  Evaluate X gain
c
          t = 1.0/(Sum2(X,i) + Axy(i)*Axy(i)*Sum2(Y,i))
          Temp = t * (Sum(X,i) + Axy(i)*Sum(Y,i)) - G(i)
          G(i) = G(i) + Factor * Temp
          ChangeX = ChangeX + real(Temp)**2 + aimag(Temp)**2
          SumWtX = SumWtX + real(G(i))**2  + aimag(G(i))**2
c
c  Evaluate Y amplitude.
c
          t = real(conjg(G(i))*Sum(Y,i)) /
     *        ((real(G(i))**2+aimag(G(i))**2) * Sum2(Y,i)) - Axy(i)
          t = max(t,-0.5*Axy(i))
          Axy(i) = Axy(i) + Factor * t
          ChangeY = ChangeY + t*t
          SumWtY = SumWtY + Axy(i)*Axy(i)
c
c  Zero the accumulators.
c
          Sum(X,i) = 0
          Sum(Y,i) = 0
          Sum2(X,i) = 0
          Sum2(Y,i) = 0
        enddo
        t = max(ChangeX/SumWtX,ChangeY/SumWtY)
        epsi = max(epsi,t)
        convrg = t.lt.tol
      enddo

      end
