      program selfcal

c= selfcal - Determine self-calibration of calibration gains.
c& mchw
c: calibration, map making
c+
c       SELFCAL is a MIRIAD task to perform self-calibration of
c       visibility data.  Either phase only or amplitude and phase
c       calibration can be performed.  The input to SELCAL are a
c       visibility data file, and model images.  This program then
c       calculates the visibilities corresponding to the model,
c       accumulates the statistics needed to determine the antennae
c       solutions, and then calculates the self-cal solutions.
c@ vis
c       Name of input visibility data file. No default.
c@ select
c       Standard uv data selection criteria.  See the help on "select"
c       for more information.
c@ model
c       Name of the input models.  Several models can be given, which
c       can cover different channel ranges and different pointing and
c       sources of the input visibility data.  Generally the model
c       should be derived (by mapping and deconvolution) from the input
c       visibility file, so that the channels in the model correspond to
c       channels in the visibility file.  The units of the model MUST be
c       Jy/pixel, rather than Jy/beam, and should be weighted by the
c       primary beam.  The task DEMOS can be used to extract primary
c       beam weighted models from a mosaiced image.  If no models are
c       given, a point source model is assumed.
c
c       NOTE: When you give SELFCAL a model, it will, by default, select
c       the data associated with this model from the visibility data-
c       set.  This includes selecting the appropriate range of channels
c       and the appropriate pointing/source (if options=mosaic is used).
c       If you use a point source model, if it YOUR responsibility to
c       select the appropriate data.
c@ clip
c       Clip level.  For models of intensity, any pixels below the clip
c       level are set to zero.  For models of Stokes Q,U,V, or MFS
c       I*alpha models, any pixels whose absolute value is below the
c       clip level are set to zero.  Default is 0.
c@ interval
c       The length of time, in minutes, of a gain solution.  Default is
c       5, but use a larger value in cases of poor signal-to-noise, or
c       if the atmosphere and instrument is fairly stable.
c@ nfbin
c       The number of frequency bins. The default is 1. Use nfbin>1 to
c       solve for variation across the band in the gains.
c       Works best for uv files with a single spectral window, i.e., 
c       after uvsplit. 
c@ options
c       This gives several processing options. Possible values are:
c         amplitude  Perform amplitude and phase self-cal.
c         phase      Perform phase only self-cal.
c         mfs        This is used if there is a single plane in the
c                    input model, which is assumed to represent the
c                    image at all frequencies.  This should also be
c                    used if the model has been derived from MFCLEAN.
c         mmfs       Same as mfs, but with multiple input models,
c                    each applying to a subset of the bandwidth. 
c                    For use with nfbin>1: models will be allocated to
c                    bins based on their centre frequency.
c         relax      Relax the convergence criteria. This is useful when
c                    selfcal'ing with a very poor model.
c         noscale    Do not scale the gains.  By default the gains are
c                    scaled so that the rms gain amplitude is 1.  In
c                    this way, the total flux is not contrained to agree
c                    with the model.
c         mosaic     This causes SELFCAL to select only those
c                    visibilities whose observing center is within plus
c                    or minus three pixels of the model reference pixel.
c                    This is needed if there are multiple pointings or
c                    multiple sources in the input uv file.  By default
c                    no observing center selection is performed.
c       Note that "amplitude" and "phase" are mutually exclusive.
c       The default is options=phase.
c@ minants
c       Data at a given solution interval is deleted if there are fewer
c       than MinAnts antennae operative during the solution interval.
c       The default is 3 for options=phase and 4 for options=amplitude.
c@ refant
c       This sets the reference antenna, which is given a phase angle of
c       zero.  The default, for a given solution interval, is the
c       antennae with the greatest weight.
c@ flux
c       If MODEL is blank, then the flux density (Jy) of a point source
c       model can be specified here.  The default is 1.
c@ offset
c       This gives the offset in arcseconds of a point source model (the
c       offset is positive to the north and to the east).  This
c       parameter is used if the MODEL parameter is blank.  The default
c       is 0,0.  The amplitude of the point source is chosen so that
c       flux in the model is the same as the visibility flux.
c@ line
c       The visibility linetype to use, in the standard form, viz:
c         type,nchan,start,width,step
c       Generally if there is an input model, this parameter defaults to
c       the linetype parameters used to construct the map.  If you wish
c       to override this, or if the info is not in the header, or if you
c       are using a point source model, this parameter can be useful.
c
c$Id: selfcal.for,v 1.14 2014/07/16 05:02:08 wie017 Exp $
c--
c
c  History:
c    rjs  13mar90 Original version.
c    rjs  23mar90 Mods to gains file. Eliminated write statements. Check
c                 that the minimum number of antennae are there.
c    rjs  28mar90 Fixed apriori and noscale options.  Get it to
c                 recalculate channel bandwidth after every model.
c    rjs  29mar90 Fixed bug in the scaling of SumVM.
c    rjs  31mar90 Increased maxchan.
c    rjs  24apr90 An added error check.
c    rjs  27apr90 Corrected bug in checking for convergence in amp
c                 selfcal.  Mildly improved amp selfcal.
c    pjt   2may90 maxchan in selfacc1 now through maxdim.h
c    rjs  16oct90 Check that the vis file is cross-correlation data.
c   mchw  24nov90 Corrected comment and protected sqrt in solve1.
c    rjs  26nov90 Minor changes to the phase-solution routine, to make
c                 it more persistent.
c    mjs  25feb91 Changed references of itoa to itoaf.
c    rjs  25mar91 Added `relax' option. Minor changes to appease flint.
c    rjs   5apr91 Trivial change to get ModelIni to do some polarisation
c                 handling.
c    mjs  04aug91 Replaced local maxants/maxbl to use maxdim.h values
c    rjs  15oct91 Increased hash table size. Extra messages.
c    rjs   1nov91 Polarized and mfs options. Removed out.
c    rjs  29jan92 New call sequence to "Model".
c    rjs   3apr92 Use memalloc routines.
c    rjs  26apr92 Handle clip level better.
c    rjs   1may92 Added nfeeds keyword to output gains header.
c    rjs  17may92 More fiddles to the way clipping is handled.
c    rjs  23jun92 Changes to doc and messages (centre=>center, etc).
c    rjs  22nov92 Added ntau keyword to output gains header.
c    mchw 20apr93 Added flux keyword and option selradec.
c    rjs  29mar93 Fiddle noise calculation. BASANT changes.
c    rjs  18may93 If rms=0, assume rms=1.
c    rjs  19may93 Merge mchw and rjs versions of selfcal.
c    rjs  28jun93 Iterate a bit longer. Better memory allocation.
c    rjs  31aug93 Better amplitude calibration with low S/N data.
c    rjs  24sep93 Doc changes only.
c    rjs   9nov93 Better recording of time of a particular solution
c                 interval.
c    rjs  23dec93 Minimum match for linetypes.
c    rjs  30jan95 Change option "selradec" to "mosaic", and update doc
c                 file somewhat. Change to helper routine.
c    rjs  16apr96 Increase select routine arrays.
c    rjs  28aug96 Minor change to get around gcc-related bug.  Change
c                 care Dave Rayner.
c    rjs   1oct96 Major tidy up.
c    rjs  19feb97 Better error messages.
c    rjs  25aug97 Correct summing of weights in "merger"
c    rjs  09nov98 Make rtime variable double precision to avoid loss
c                 of timing precision.
c    rjs  01dec98 Added extra warning message.
c    rjs  30aug99 Increase maxmod to 64
c    rjs  14dec99 Ability to use model visibility datasets.
c    dpr  17apr01 Increase MaxMod to 128
c    mhw  16jan12 Use ptrdiff for scr routines to handle larger files
c    mhw  10apr13 Add nfbin parameter
c    mhw  09may13 Add mmfs option
c    mhw  12mar14 Initialize nchan in point source case
c    mhw  16jul14 Fixed binned solution interpolation
c
c  Bugs/Shortcomings:
c   * Selfcal should check that the user is not mixing different
c     polarisations and pointings.
c   * It would be desirable to apply bandpasses, and merge gain tables,
c     apply polarisation calibration, etc.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer   MAXMOD, MAXSELS, NHEAD
      parameter (MAXMOD=1024, MAXSELS=1024, NHEAD=3)

      logical   amp, doim, doline, mfs, noscale, phase, relax, selradec
      logical   mmfs
      integer   i, minants, nModel, nchan, nvis, refant, tmod, tscr,
     *          tvis, nfbin, numchan, start
      real      clip, flux(2), interval, lstart, lstep, lwidth,
     *          offset(2), sels(MAXSELS)
      character flag1*8, flag2*8, ltype*32, Models(MAXMOD)*64,
     *          obstype*32, version*72, vis*64
      double precision sfreq(MAXCHAN)

c     Externals.
      logical   hdprsnt
      character versan*72
      external  hdprsnt, header, versan
c-----------------------------------------------------------------------
      version = versan('selfcal',
     *                 '$Revision: 1.14 $',
     *                 '$Date: 2014/07/16 05:02:08 $')
c
c  Get the input parameters.
c
      call keyini
      call keyf('vis',vis,' ')
      call SelInput('select',sels,MAXSELS)
      call mkeyf('model',Models,MAXMOD,nModel)
      call keyr('clip',clip,0.0)
      call keyr('interval',interval,5.0)
      call keyi('nfbin',nfbin,0)
      nfbin=min(MAXFBIN,max(0,nfbin))
      if (nfbin.eq.1) nfbin=0
      call keyi('minants',minants,0)
      call keyi('refant',refant,0)
      call keyr('flux',flux(1),1.0)
      flux(2) = 1
      call keyr('offset',offset(1),0.0)
      call keyr('offset',offset(2),0.0)
      call keyline(ltype,nchan,lstart,lwidth,lstep)
      doline = ltype.ne.' '
      call GetOpt(phase,amp,noscale,relax,mfs,mmfs,selradec)
      mfs = mfs .or. mmfs
      call keyfin
c
c  Check that the inputs make sense.
c
      if (vis.eq.' ') call bug('f','Input visibility file is missing')
      if (interval.le.0) call bug('f','Bad calibration interval')
      interval = interval / (24.0*60.0)

      if (MinAnts.eq.0) then
        if (phase) then
          Minants = 3
        else if (amp) then
          Minants = 4
        endif
      endif
      if (MinAnts.lt.2) then
        call bug('f','Bad value for the minants parameter.')
      else if (phase .and. MinAnts.lt.3) then
        call bug('w','Phase selfcal with minants < 3 is unusual')
      else if (amp .and. MinAnts.lt.4) then
        call bug('w','Amplitude selfcal with minants < 4 is unusual')
      endif

      if (nModel.eq.0) then
        if (abs(offset(1))+abs(offset(2)).eq.0) then
          call output(
     *        'Model is a point source at the observing center')
        else
          call output('Using a point source model')
        endif
      endif
c
c  Open the visibility file, check that it is interferometer data, and
c  set the line type if necessary.
c
      call uvopen(tvis,vis,'old')
      call rdhda(tvis,'obstype',obstype,'crosscorrelation')
      if (index(obstype,'cross').eq.0)
     *  call bug('f','The vis file is not cross correlation data')
      if (hdprsnt(tvis,'leakage') .or. hdprsnt(tvis,'bandpass')) then
        call bug('w',
     *    'Selfcal does not apply pre-existing calibration tables')
        if (hdprsnt(tvis,'leakage'))
     *    call bug('w','No polarization calibration applied')
        if (hdprsnt(tvis,'bandpass'))
     *    call bug('w','No bandpass calibration applied')
      endif
      if (doline) call uvset(tvis,'data',ltype,nchan,lstart,lwidth,
     *                                                        lstep)
c
c  Determine the flags to the MODELINI routine.
c  p -- Perform pointing selection.
c  l -- Set up line type.
c
      flag1 = ' '
      if (selradec)                flag1(1:1) = 'p'
      if (.not.doline .and. .not.mfs) flag1(2:2) = 'l'
c
c  Determine the flags to the MODEL routine.
c  l - Perform clipping.
c  m - Model is a mfs one.
c
      flag2 = 'l'
      if (mfs) flag2(2:2) = 'm'
c
c  Loop over all the models.
c
      if (nModel.eq.0) then
        call output('Reading the visibility file ...')
        call SelfSet(.true.,MinAnts)
        call SelApply(tvis,sels,.true.)
        call getFreq(tvis,sfreq,numchan)
        call Model(flag2,tvis,0,offset,flux,tscr,
     *                        NHEAD,header,nchan,nvis)
        call SelfIni(nfbin)
        call output('Accumulating statistics ...')
        call SelfAcc(tscr,numchan,numchan,1,nvis,interval,sfreq)
        call scrclose(tscr)
      else
        do i = 1, nModel
          start = 1
          if (i.eq.1) then
            call getFreq(tvis,sfreq,numchan)
          endif
          call output('Calculating the model for '//Models(i))
          call SelfSet(i.eq.1,MinAnts)
          call getopen(tmod,Models(i),doim)
          if (doim) then
            if (mmfs.and.nfbin.gt.0) 
     *        call SelBin(tvis,tmod,nfbin,numchan,sfreq,start)
           call ModelIni(tmod,tvis,sels,flag1)
          else
            call uvrewind(tvis)
            call uvselect(tvis,'clear',0d0,0d0,.true.)
            call SelApply(tvis,sels,.true.)
            call SelApply(tmod,sels,.true.)
          endif
          call Model(flag2,tvis,tmod,offset,Clip,tscr,
     *                        NHEAD,header,nchan,nvis)
          if (doim) then
            call xyclose(tmod)
          else
            call uvclose(tmod)
          endif
          call output('Accumulating statistics ...')
          if (i.eq.1) then
            call SelfIni(nfbin)
          endif
          call SelfAcc(tscr,numchan,nchan,start,nvis,interval,sfreq)
          call scrclose(tscr)
        enddo
      endif
c
c  Open the output file to contain the gain solutions.
c
      call HisOpen(tvis,'append')
      call HisWrite(tvis,'SELFCAL: Miriad '//version)
      call HisInput(tvis,'SELFCAL')
c
c  Calculate the self-cal gains.
c
      call output('Finding the selfcal solutions ...')
      call Solve(tvis,phase,relax,noscale,
     *     refant,interval,numchan)
c
c  Close up.
c
      call SelfFin
      call HisClose(tvis)
      call uvclose(tvis)
      end

c***********************************************************************

      subroutine getopen(tno,name,doim)

      integer tno
      character name*(*)
      logical doim
c-----------------------------------------------------------------------
c  Open either a model visibility dataset or model image.
c-----------------------------------------------------------------------
      integer iostat,nsize(3)

c     Externals.
      logical hdprsnt
c-----------------------------------------------------------------------
      call hopen(tno,name,'old',iostat)
      if (iostat.ne.0) call bugno('f',iostat)
      doim = hdprsnt(tno,'image')
      call hclose(tno)
      if (doim) then
        call xyopen(tno,name,'old',3,nsize)
      else
        call uvopen(tno,name,'old')
      endif
      end

c***********************************************************************

      subroutine Chkpolm(tmod)

      integer tmod
c-----------------------------------------------------------------------
c  Check that the visibility is a total intensity type.
c-----------------------------------------------------------------------
      integer iax
      double precision t

c     Externals.
      logical polspara
c-----------------------------------------------------------------------
      call coInit(tmod)
      call coFindAx(tmod,'stokes',iax)
      if (iax.ne.0) then
        call coCvt1(tmod,iax,'ap',1d0,'aw',t)
        if (.not.polspara(nint(t)))
     *    call bug('f','Model is not a total intenisty one')
      endif
      call coFin(tmod)
      end

c***********************************************************************

      subroutine GetOpt(phase,amp,noscale,relax,mfs,mmfs,selradec)

      logical phase,amp,noscale,relax,mfs,mmfs,selradec
c-----------------------------------------------------------------------
c  Determine extra processing options.
c
c  Output:
c    phase      If true, do phase self-cal.
c    amp        If true, do amplitude/phase self-cal.
c    noscale    Do not scale the model to conserve flux.
c    relax      Relax convergence criteria.
c    mfs        Model is frequency independent, or has been derived
c               from MFCLEAN.
c    mmfs       Multiple mfs models each covering part of the band 
c               are present.
c    selradec   Input uv file contains multiple pointings or multiple
c               sources.
c-----------------------------------------------------------------------
      integer nopt
      parameter (nopt=7)
      character opts(nopt)*9
      logical present(nopt)
      data opts/'amplitude','phase    ','noscale  ','relax    ',
     *          'mfs      ','mmfs     ','mosaic   '/
c-----------------------------------------------------------------------
      call options('options',opts,present,nopt)
      amp = present(1)
      phase = present(2)
      noscale = present(3)
      relax = present(4)
      mfs = present(5)
      mmfs = present(6)
      selradec = present(7)
      if (amp .and. phase)
     *  call bug('f','Cannot do both amp and phase self-cal')
      if (.not.(amp .or. phase)) phase = .true.

      end

c***********************************************************************

      subroutine header(tvis,preamble,data,flags,nchan,
     *                                        accept,Out,nhead)
      integer tvis,nchan,nhead
      complex data(nchan)
      logical flags(nchan),accept
      real Out(nhead)
      double precision preamble(5)
c-----------------------------------------------------------------------
c  This is a service routine called by the model subroutines every time
c  a visibility is read from the data file.
c
c  Input:
c    tvis       Handle of the visibility file.
c    nhead      The value of nhead
c    nchan      The number of channels.
c    preamble   Preamble returned by uvread.
c    data       A complex array of nchan elements, giving the
c               correlation data.  Not used.
c    flags      The data flags. Not used.
c  Output:
c   out         The nhead values to save with the data. Three values are
c               returned:
c                 out(1) -- baseline number.
c                 out(2) -- time (days) relative to time0.
c                 out(3) -- estimated sigma**2.
c   accept      This determines whether the data is accepted or
c               discarded.  It is always accepted unless the baseline
c               number looks bad.
c-----------------------------------------------------------------------
      include 'selfcal.h'
      integer PolI
      parameter (PolI=1)

      integer i1,i2,polv
      double precision rms
      logical okpol,okbase

c     External.
      logical polspara

c-----------------------------------------------------------------------
      if (first) then
        call uvrdvri(tvis,'nants',nants,0)
        if (nants.le.0) call bug('f',
     *    'The data file does not contain the number of antennae')
        if (nants.lt.MinAnts) call bug('f',
     *    'Fewer than the minimum number of antennae are present')
        time0 = int(preamble(4)) + 0.5
        nbstok = 0
        nbad = 0
        first = .false.
      endif
c
c  Accept visibilities only if they are total intensities.
c  and their antenna numbers are OK.
c
      call uvrdvri(tvis,'pol',polv,PolI)
      call basant(preamble(5),i1,i2)
      okpol = polspara(polv)
      okbase = min(i1,i2).ge.1 .and. max(i1,i2).le.nants .and. i1.ne.i2
      accept = okpol .and. okbase
c
c  If all looks OK, then calculate the theoretical rms, and store away
c  the information that we need.
c
      if (accept) then
        out(1) = preamble(5)
        out(2) = preamble(4) - time0
        call uvinfo(tvis,'variance',rms)
        if (rms.le.0) rms=1
        out(3) = rms
      else if (.not.okpol) then
        nbstok = nbstok + 1
      else
        nbad = nbad + 1
      endif
      end

c***********************************************************************

      subroutine SelfSet(firstd,MinAntsd)

      logical firstd
      integer MinAntsd
c-----------------------------------------------------------------------
      include 'selfcal.h'
c-----------------------------------------------------------------------
      first = firstd
      MinAnts = MinAntsd
      end

c***********************************************************************

      subroutine SelfIni(nfbin1)
      
      integer nfbin1
c-----------------------------------------------------------------------
c  Initialise variables and allocate memory.
c  Input:
c     nfbin1    number of frequency bins
c-----------------------------------------------------------------------
      include 'selfcal.h'

      integer i, SolSize, n1

      integer  MemBuf, prime
      external MemBuf, prime
c-----------------------------------------------------------------------
      nfbin = nfbin1
      n1 = nfbin+1
      nHash = prime(maxHash-1)
      do i = 1, nHash+1
        Hash(i) = 0
      enddo
      
      do i=1,nfbin
        freq(i)   = 0.d0
        wfreq(i)  = 0.d0
      enddo
      nBl = (nants*(nants-1))/2
      SolSize = 3 + 4*nBl + 2*nants
      maxSol = min(nHash,max(minSol,(MemBuf()-10)/SolSize))
      nSols  = 0
      TotVis = 0

c     Allocate memory.  The indices are offsets into a single scratch
c     buffer used as follows:
c       integer count(maxSol,n1)
c       real    stpTime(maxSol), strTime(maxSol), sumMM(maxSol,n1),
c               sumVV(nBl,maxSol,n1), weight(nBl,maxSol,n1)
c       double precision rTime(maxSol)
c       complex gains(nants,maxSol,n1), sumVM(nBl,maxSol,n1)
      call MemAlloc(pStptim,maxSol, 'r')
      call MemAlloc(pStrtim,maxSol, 'r')
      call MemAlloc(pCount, maxSol*n1, 'i')
      call MemAlloc(prTime, maxSol, 'd')
      call MemAlloc(pSumMM, maxSol*n1, 'r')
      call MemAlloc(pSumVV, maxSol*nBl*n1, 'r')
      call MemAlloc(pSumVM, maxSol*nBl*n1, 'c')
      call MemAlloc(pWeight,maxSol*nBl*n1, 'r')
      call MemAlloc(pGains, maxSol*nants*n1, 'c')

      end

c***********************************************************************

      subroutine SelfFin
c-----------------------------------------------------------------------
c  Release allocated memory.
c-----------------------------------------------------------------------
      include 'selfcal.h'
      integer n1
c-----------------------------------------------------------------------
      n1 = nfbin+1
      call MemFree(pStpTim,maxSol, 'r')
      call MemFree(pStrTim,maxSol, 'r')
      call MemFree(pCount, maxSol*n1, 'i')
      call MemFree(prTime, maxSol, 'd')
      call MemFree(pSumMM, maxSol*n1, 'r')
      call MemFree(pSumVV, maxSol*nBl*n1, 'r')
      call MemFree(pSumVM, maxSol*nBl*n1, 'c')
      call MemFree(pWeight,maxSol*nBl*n1, 'r')
      call MemFree(pGains, maxSol*nants*n1, 'c')

      end

***********************************************************************

      subroutine getFreq(tvis,sfreq,nchan)
      include 'selfcal.h'
      integer tvis,nchan
      double precision sfreq(MAXCHAN)
c-----------------------------------------------------------------------
c  Get frequency details at start of file and rewind file.
c  Input:
c     tvis - handle to uv file
c  Output:
c     sfreq - channel frequencies
c-----------------------------------------------------------------------
      double precision preamble(6)
      logical flags(MAXCHAN)
      complex vis(MAXCHAN)
c-----------------------------------------------------------------------
      call uvread(tvis,preamble,vis,flags,MAXCHAN,nchan)
      call uvinfo(tVis,'sfreq',sfreq)
      call uvrewind(tvis)
      end
***********************************************************************

      subroutine SelBin(tvis,tmod,nfbin,nchan,sfreq,start)
c      include 'selfcal.h'
      integer tvis,tmod,nfbin,nchan,start
      double precision sfreq(nchan),cfreq
c-----------------------------------------------------------------------
c  Select the correct bin for this model, based on ref frequency
c  Input:
c     tvis - handle to uv file
c     tmod - handle to image model
c     nfbin - number of bins
c     nchan - number of channels in full spectrum
c     sfreq - channel frequencies
c-----------------------------------------------------------------------
      integer nchan1, chn, i, fbin
      double precision dmin, d
c-----------------------------------------------------------------------
      call rdhdd(tmod,'crval3',cfreq,0.d0)
      nchan1 = nchan/nfbin
      dmin=1d10
      do i=1,nchan
        d = abs(sfreq(i)-cfreq)
        if (d.lt.dmin) then
          chn = i
          dmin = d
        endif
      enddo
      fbin = min(nfbin,(chn-1)/(nchan/nfbin)+1)
      start = (fbin-1)*(nchan/nfbin)+1
      nchan1 = nchan/nfbin
      if (fbin.eq.nfbin) nchan1=nchan-start+1 
      call uvset(tvis,'data','channel',nchan1,start*1.0,1.0,1.0)
      end

c***********************************************************************

      subroutine SelfAcc(tscr,nchan0,nchan,start,nvis,interval,sfreq)

      integer tscr,nchan0,nchan,start,nvis
      real interval
      double precision sfreq(nchan)
c-----------------------------------------------------------------------
c  Call the routine that does the real work in accumulating the
c  statistics about a model.
c
c  Input:
c    tscr       The handle of the scratch file, which contains the
c               visibility and model information.
c    nchan0     Number of channels in original spectrum (mmfs)
c    nchan      The number of channels in the scratch file.
c    start      THe start channel number, 1 unless using options=mmfs
c    nvis       The number of visbilities in the scratch file.
c    Interval   The self-cal gain interval.
c    sfreq      The channel frequencies
c-----------------------------------------------------------------------
      include 'selfcal.h'
c-----------------------------------------------------------------------
      TotVis = TotVis + nVis
      call SelfAcc1(tscr,nchan0,nchan,start,nvis,nBl,maxSol,nSols,
     *  nfbin,nhash,Hash,Indx,interval,
     *  Memc(pSumVM),Memr(pSumVV),Memr(pSumMM),
     *  Memr(pWeight),Memi(pCount),Memd(prTime),Memr(pstpTim),
     *  Memr(pstrTim),sfreq,freq,wfreq)

      end

c***********************************************************************

      subroutine SelfAcc1(tscr,nchan0,nchan,start,nvis,nBl,maxSol,
     *  nSols,nfbin,nHash,Hash,Indx,interval,
     *  SumVM,SumVV,SumMM,Weight,Count,rTime,StpTime,StrTime,sfreq,freq,
     *  wfreq)

      integer tscr,nchan0,nchan,start,nvis,nBl,maxSol,nSols,nfbin
      integer nHash,Hash(nHash+1),Indx(nHash)
      real    interval
      complex SumVM(nBl,maxSol,0:nfbin)
      real    SumVV(nBl,maxSol,0:nfbin),SumMM(maxSol,0:nfbin)
      real    Weight(nBl,maxSol,0:nfbin)
      integer count(maxSol,0:nfbin)
      double precision rTime(maxSol),freq(0:nfbin),wfreq(0:nfbin)
      double precision sfreq(nchan)
      real    StpTime(maxSol), StrTime(maxSol)
c-----------------------------------------------------------------------
c  This reads through the scratch file which contains the visibility
c  and the model.  It finds (via a hash table) the index of the slot
c  that is being used to store info for this time interval.  It then
c  accumulates various statistics into the appropriate arrays.  These
c  statistics are eventually used to determine the self-cal solutions.
c
c  Input:
c    tscr       Handle of the scratch file.
c    nchan0     Number of channels in original spectrum (mmfs)
c    nchan      Number of channels.
c    start      THe start channel number, 1 unless using options=mmfs
c    nvis       Number of visibilities.
c    nants      Number of antennae.
c    nBl        Number of baselines = nants*(nants-1)/2.
c    maxSol     Max number of solution slots.
c    nSols      Actual number of solution slots being used.
c    nHash      Hash table size.
c    interval   Self-cal interval.
c  Input/Output:
c  In the following, t=time (within a solution interval), f=channels,
c               b=baseline number, a = antenna number.
c    Time       The integer time, nint((preamble(4)-time0)/interval).
c    Hash       Hash table, used to locate a solution interval.
c    Indx       Index from hash table to solution interval.
c    SumVM      Sum(over t,f)conjg(Model)*Vis/sigma**2. Varies with b.
c    SumVV      Sum(over t,f)|Model|**2/sigma**2. Varies with b.
c    SumMM      Sum(over t,f,b) |Vis|**2/sigma**2.
c    Weight     Sum(over t,f) 1/sigma**2. Varies with b.
c    Count      Sum(over t,f,b) 1.
c    rTime      Sum(over t,f,b) t.
c
c  The Visibility Records:
c    The scratch file consists of "nvis" records, each of size
c    nhead+5*nchan The first nhead=3 values are
c      baseline number
c      time
c      estimated sigma**2
c
c  The Hash Table:
c    The hash table is a cyclic buffer.  Each element in the buffer
c    potentially holds info pertaining to one time interval.  A value of
c    0 in Hash indicates an unused slot, whereas a non-zero number
c    indicates a used slot.  This non-zero number is 2*itime+1, which is
c    always odd, and is unique to a particular time.
c
c    The last entry in the hash table (element nHash+1) is always zero,
c    so the check for the end of the buffer can be placed outside the
c    main loop.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer   NHEAD, MAXLEN
      parameter (NHEAD = 3, MAXLEN = NHEAD + 5*MAXCHAN)

      integer   i, i1, i2, iBl, ihash, itime, j, k, length, chn, fbin
      integer   sCount(0:MAXFBIN)
      ptrdiff   off
      real      mm, out(MAXLEN), sMM(0:MAXFBIN), sTime
      real      sVV(0:MAXFBIN), vv, wgt
      complex   sVM(0:MAXFBIN), vm
      double precision sF(MAXFBIN)
c-----------------------------------------------------------------------
      if (nchan.gt.MAXCHAN) call bug('f','Too many channels')
      length = NHEAD + 5*nchan

      do j = 1, nvis
        off = j-1
        off = off * length
        call scrread(tscr,Out,off,length)
        itime = nint(Out(2)/interval)
        ihash = 2*itime + 1
        i = mod(itime,nHash)
        if (i.le.0) i = i + nHash

c       Find this entry in the hash table.
        do while (Hash(i).ne.0 .and. Hash(i).ne.ihash)
          i = i + 1
        enddo

        if (i.gt.nHash) then
          i = 1
          do while (Hash(i).ne.0 .and. Hash(i).ne.ihash)
            i = i + 1
          enddo
        endif

c       Not in the hash table, add a new entry.
        if (Hash(i).eq.0) then
          nSols = nSols + 1
          if (nSols.ge.maxSol) call bug('f','Hash table overflow')
          Hash(i) = ihash
          Indx(i) = nSols

          StpTime(nSols) = Out(2)
          StrTime(nSols) = Out(2)
          rTime(nSols) = 0d0
          do fbin=0,nfbin
            count(nSols,fbin) = 0
            sumMM(nSols,fbin) = 0.0
            do iBl = 1, nBl
              sumVV(iBl,nSols,fbin) = 0.0
              sumVM(iBl,nSols,fbin) = (0.0,0.0)
              weight(iBl,nSols,fbin) = 0.0
            enddo
          enddo
        endif

c       Accumulate info about this visibility record.
        call basant(dble(out(1)),i1,i2)
        iBl = (i2-1)*(i2-2)/2 + i1

        i = Indx(i)
        StpTime(i) = min(StpTime(i), out(2))
        StrTime(i) = max(StrTime(i), out(2))

c       N.B. using temporaries to accumulate statistics separately for
c       each spectrum helps to maintain precision when there are a very
c       large number of correlations in the solution interval.

        sTime  = 0.0
        do fbin = 0, nfbin
          sCount(fbin) = 0
          sMM(fbin)    = 0.0
          sVV(fbin)    = 0.0
          sVM(fbin)    = (0.0,0.0)
          if (fbin.gt.0) then
            sF(fbin)     = 0.d0
          endif
        enddo
        chn=start-1
        do k = NHEAD+1, NHEAD+5*nchan, 5
          chn = chn + 1
          if (out(k+4).gt.0.0) then
            sCount(0) = sCount(0) + 1
            sTime  = sTime  + Out(2)

            mm = Out(k)**2   + Out(k+1)**2
            vv = Out(k+2)**2 + Out(k+3)**2
            vm = cmplx(Out(k),Out(k+1)) * cmplx(Out(k+2),-Out(k+3))

            sMM(0) = sMM(0) + mm
            sVV(0) = sVV(0) + vv
            sVM(0) = sVM(0) + vm
            if (nfbin.gt.1) then
              fbin = min(nfbin,(chn-1)/(nchan0/nfbin)+1)
              sCount(fbin) = sCount(fbin) + 1
              sMM(fbin) = sMM(fbin) + mm
              sVV(fbin) = sVV(fbin) + vv
              sVM(fbin) = sVM(fbin) + vm
              sF(fbin) = sF(fbin) + log(sfreq(chn))
            endif
          endif
        enddo

c       Accumulate statistics for this spectrum.
        wgt = 0.5d0 / Out(3)
        rTime(i)     =  rTime(i)     + sTime
        do fbin=0,nfbin
          count(i,fbin)     =  count(i,fbin)     + sCount(fbin)
          sumMM(i,fbin)     =  sumMM(i,fbin)     + wgt*sMM(fbin)
          sumVV(iBl,i,fbin) =  sumVV(iBl,i,fbin) + wgt*sVV(fbin)
          sumVM(iBl,i,fbin) =  sumVM(iBl,i,fbin) + wgt*sVM(fbin)
          weight(iBl,i,fbin) = weight(iBl,i,fbin) + wgt*sCount(fbin)
          if (fbin.gt.0) then
            freq(fbin) = freq(fbin) + wgt*sF(fbin)
            wfreq(fbin) = wfreq(fbin) + wgt*sCount(fbin)
          endif
        enddo
      enddo

      end

c***********************************************************************

      subroutine Solve(tgains,phase,relax,noscale,refant,
     *                                     interval,nchan)

      integer tgains
      logical phase,relax,noscale
      integer refant,nchan
      real interval
c-----------------------------------------------------------------------
c  Having accumulated all the needed statistics we are now ready to
c  determine the self-cal solutions.
c-----------------------------------------------------------------------
      include 'selfcal.h'
      character line*64
      integer fbin

c     Externals.
      character itoaf*8
c-----------------------------------------------------------------------
      if (nbad.ne.0) call bug('w',
     *  'No. correlations with bad baseline numbers: '//
     *  itoaf(nbad*nchan))
      if (nbstok.ne.0) call output(
     *  'Correlations with inappropriate Stokes type discarded: '//
     *  itoaf(nbstok*nchan))
      line = 'Total number of correlations being used: '//
     *  itoaf(TotVis*nchan)
      call HisWrite(tgains,'SELFCAL: '//line)
      call output(line)
      line = 'Total number of solution intervals: '//itoaf(nSols)
      call HisWrite(tgains,'SELFCAL: '//line)
      call output(line)

c     Calculate geometric mean frequency for each bin 
      do fbin=1,nfbin
        if (wfreq(fbin).gt.0) freq(fbin)=exp(freq(fbin)/wfreq(fbin))
      enddo

c     Determine all the gain solutions.
      call Solve1(tgains,nSols,maxsol,nBl,nants,nfbin,phase,relax,
     *  noscale,minants,refant,Time0,interval,Indx,
     *  Memc(pSumVM),Memr(pSumVV),Memr(pSumMM),Memc(pGains),
     *  Memr(pWeight),Memi(pCount),memD(prTime),
     *  Memr(pStpTim),Memr(pStrTim),freq)

      end

c***********************************************************************

      subroutine Solve1(tgains,nSols,maxsol,nBl,nants,nfbin,phase,
     *  relax,noscale,minants,refant,Time0,interval,TIndx,
     *  SumVM,SumVV,SumMM,Gains,Weight,Count,rTime,
     *  StpTime,StrTime,freq)

      integer tgains, nSols, maxsol, nBl, nants, nfbin
      logical phase,relax,noscale
      integer minants, refant
      double precision Time0
      real   interval
      integer TIndx(nSols)
      complex SumVM(nBl,maxsol,0:nfbin), Gains(nants,maxsol,0:nfbin)
      real    SumVV(nBl,maxsol,0:nfbin), SumMM(maxsol,0:nfbin)
      real    Weight(nBl,maxsol,0:nfbin)
      integer Count(maxsol,0:nfbin)
      double precision rTime(nSols),freq(0:nfbin)
      real    StpTime(nSols),StrTime(nSols)
c-----------------------------------------------------------------------
c  Run through all the accumulated data and calculate the selfcal
c  solutions.
c
c  Input:
c    phase
c    relax
c    nSols
c    nBl
c    nants
c    minants
c    refant
c    time
c    Weight
c    Count
c    rTime
c    SumMM
c    SumVM
c    SumVV
c  Scratch:
c    TIndx
c    Gains
c-----------------------------------------------------------------------
      include 'maxdim.h'
      logical Convrg
      integer i,k,k0,nbad,iostat,item,offset,header(2),nmerge,fbin
      double precision dtime,dtemp
      character line*80
      complex G(MAXANT)

c     Externals.
      character itoaf*8
c-----------------------------------------------------------------------
c     Sort the self-cal solutions into order of increasing time.
      call sortidxr(nSols,StpTime,TIndx)

c     Merge together short, adjacent, solution intervals.
      call Merger(nSols,nBl,nfbin,TIndx,interval,StpTime,StrTime,
     *  SumVM,SumVV,SumMM,Weight,Count,rTime,nmerge)
      if (nmerge.gt.0) call output(
     *  'Solution intervals merged together: '//itoaf(nmerge))

c     Now calculate the solutions.
      do fbin=0,nfbin
        if (fbin.gt.0) then
          write(line,'(a,i2)') 'Solution for freq. bin ',fbin
          call output(line)
          call HisWrite(tgains,'SELFCAL: '//line)
        endif
        nbad = 0
        do k = 1, nSols
          if (Count(k,fbin).gt.0) then
            call Solve2(nbl,nants,SumVM(1,k,fbin),SumVV(1,k,fbin),
     *        Weight(1,k,fbin),phase,relax,minants,refant,
     *        Gains(1,k,fbin),Convrg)
            if (.not.Convrg) then
              nbad = nbad + 1
              Count(k,fbin) = 0
            endif
          endif
          if (Count(k,fbin).eq.0) then
            do i = 1, nants
              Gains(i,k,fbin) = 0
            enddo
          endif
        enddo

c       Write out some info to wake the user up from his/her slumber.
        if (nbad.ne.0) call bug('w','Intervals with no solution: '//
     *                                                itoaf(nbad))
        if (nbad+nmerge.eq.nsols) 
     *    call bug('f','No solutions were found')
      
c       Scale the gains if needed.
        if (.not.(noscale .or. phase)) call GainScal(Gains(1,1,fbin),
     *    nants,nSols)
c       Calculate statistics.
        call CalcStat(tgains,nsols,nbl,nants,SumVM(1,1,fbin),
     *    SumMM(1,fbin),SumVV(1,1,fbin),
     *    Weight(1,1,fbin),Count(1,fbin),Gains(1,1,fbin))
      enddo


c     Write the gains out to a gains table.
      call haccess(tgains,item,'gains','write',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening output gains item')
        call bugno('f',iostat)
      endif
      header(1) = 0
      header(2) = 0
      offset = 0
      call hwritei(item,header,offset,8,iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening output gains item')
        call bugno('f',iostat)
      endif
      offset = 8
      do k = 1, nSols
        k0 = TIndx(k)
        if (Count(k0,0).gt.0) then
          dtime = rTime(k0) / Count(k0,0) + time0
          call hwrited(item,dtime,offset,8,iostat)
          offset = offset + 8
          call GFudge(gains(1,k0,0),nants)
          if (iostat.eq.0) call hwriter(item,gains(1,k0,0),offset,
     *      8*nants,iostat)
          offset = offset + 8*nants
          if (iostat.ne.0) then
            call bug('w','I/O error while writing to gains item')
            call bugno('f',iostat)
          endif
        endif
      enddo
      call hdaccess(item,iostat)
      if (iostat.ne.0) then
        call bug('w','Error closing output gains item')
        call bugno('f',iostat)
      endif

c     Write some extra information for the gains table.
      dtemp = interval
      call wrhdd(tgains,'interval',dtemp)
      call wrhdi(tgains,'ngains',nants)
      call wrhdi(tgains,'nsols',nsols-nbad-nmerge)
      call wrhdi(tgains,'nfeeds',1)
      call wrhdi(tgains,'ntau',0)
      
      if (nfbin.le.1) then
        call hdelete(tgains,'gainsf',iostat)
        call hdelete(tgains,'leakagef',iostat)
        call hdelete(tgains,'nfbin',iostat)
        return
      endif
c
c  Now write the gains for the frequency binned solutions
c
      call haccess(tgains,item,'gainsf','write',iostat)
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
      offset = 8
      do fbin = 1, nfbin
        do k = 1, nSols
          k0 = TIndx(k)
          if (Count(k0,0).gt.0) then
            dtime = rTime(k0) / Count(k0,0) + time0
            call hwrited(item,dtime,offset,8,iostat)
            offset = offset + 8
            if (iostat.ne.0) then
              call bug('w','Error writing time to gainsf table')
              call bugno('f',iostat)
            endif
            do i = 1, nants
              if (abs(real(Gains(i,k0,fbin)))+
     *            abs(aimag(Gains(i,k0,fbin))).ne.0) then
                G(i) = 1/Gains(i,k0,fbin)
              else
                G(i) = (0.0,0.0)
              endif
            enddo
            call hwriter(item,G,offset,8*nants,iostat)
            offset = offset + 8*nants
            if (iostat.ne.0) then
              call bug('w','Error writing gains to gainsf table')
              call bugno('f',iostat)
            endif
          endif
        enddo
        call hwrited(item,freq(fbin),offset,8,iostat)
        offset = offset + 8
      enddo
c
c  Finished writing the gain table.
c
      call hdaccess(item,iostat)
      if (iostat.ne.0) call bugno('f',iostat)
      call wrhdi(tgains,'nfbin',nfbin)
      

      end

c***********************************************************************

      subroutine Merger(nSols,nBl,nfbin,TIndx,interval,StpTime,StrTime,
     *  SumVM,SumVV,SumMM,Weight,Count,rTime,nmerge)

      integer nSols, nBl, nfbin, tIndx(nSols)
      real    interval
      real    StpTime(nSols), StrTime(nSols)
      complex SumVM(nBl,nSols,0:nfbin)
      real    SumVV(nBl,nSols,0:nfbin),SumMM(nSols,0:nfbin)
      real    Weight(nBl,nSols,0:nfbin)
      integer Count(nSols,0:nfbin)
      double precision rTime(nSols)
      integer nmerge
c-----------------------------------------------------------------------
c  Merge together adjacent solution intervals if the total span of time
c  is last than "interval".
c
c  Input:
c    nSols
c    nBl
c    TIndx
c    interval
c  Input/Output:
c    SumVM
c    SumVV
c    SumMM
c    Weight
c    Count
c    rTime
c    StpTime
c    StrTime
c  Output:
c    nmerge     The number of mergers.
c-----------------------------------------------------------------------
      logical more
      integer iBl, iSol, jSol, jdx, idx, fbin
c-----------------------------------------------------------------------
      nmerge = 0

c     Find the first solution slot with some valid data.
      iSol = 0
      more = .true.
      do while (iSol.lt.nSols .and. more)
        iSol = iSol + 1
        idx = TIndx(iSol)
        if (count(idx,0).le.0) then
          nmerge = nmerge + 1
        else
          more = .false.
        endif
      enddo
      if (nmerge.eq.nSols) call bug('f','No valid data')

      do jSol = iSol+1, nSols
        jdx = TIndx(jSol)
        if (count(jdx,0).le.0) then
          nmerge = nmerge + 1
        else if (StrTime(jdx)-StpTime(idx).le.interval) then
          nmerge = nmerge + 1
          StrTime(idx) = StrTime(jdx)
          rTime(idx) = rTime(idx) + rTime(jdx)
          do fbin=0,nfbin
            count(idx,fbin) = count(idx,fbin) + count(jdx,fbin)
            count(jdx,fbin) = 0
            sumMM(idx,fbin) = sumMM(idx,fbin) + sumMM(jdx,fbin)
            do iBl = 1, nBl
               sumVM(iBl,idx,fbin) =  sumVM(iBl,idx,fbin) + 
     *           sumVM(iBl,jdx,fbin)
               sumVV(iBl,idx,fbin) =  sumVV(iBl,idx,fbin) + 
     *           sumVV(iBl,jdx,fbin)
              weight(iBl,idx,fbin) = weight(iBl,idx,fbin) +
     *          weight(iBl,jdx,fbin)
            enddo
          enddo
        else
          idx = jdx
        endif
      enddo

      end

c***********************************************************************

      subroutine GainScal(Gains,nants,nSols)

      integer nants,nSols
      complex Gains(nants,nSols)
c-----------------------------------------------------------------------
c  Scale the gains to have an rms value of 1.
c
c-----------------------------------------------------------------------
      integer i,j,N
      real Sum,fac
      complex g
c-----------------------------------------------------------------------
      Sum = 0
      N = 0
      do j = 1, nsols
        do i = 1, nants
          g = Gains(i,j)
          if (abs(real(g))+abs(aimag(g)).gt.0) then
            N = N + 1
            Sum = Sum + real(g)**2 + aimag(g)**2
          endif
        enddo
      enddo

      if (N.eq.0) return
      fac = sqrt(N / Sum)

      do j = 1, nSols
        do i = 1, nants
          Gains(i,j) = fac * Gains(i,j)
        enddo
      enddo

      end

c***********************************************************************

      subroutine GFudge(gains,nants)

      integer nants
      complex gains(nants)
c-----------------------------------------------------------------------
c  Get the reciprocal of the gains.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nants
        if (abs(real(gains(i)))+abs(aimag(gains(i))).gt.0)
     *    gains(i) = 1/gains(i)
      enddo

      end

c***********************************************************************

      subroutine Solve2(nbl,nants,SumVM,SumVV,Weight,
     *    phase,relax,MinAnts,Refant,GainOut,Convrg)

      integer nbl,nants,MinAnts,RefAnt
      real SumVV(nbl),Weight(nBl)
      complex SumVM(nbl),GainOut(nants)
      logical phase,relax,Convrg
c-----------------------------------------------------------------------
c  Routine to do various fooling around before a routine is called to
c  determine the gain solution. The fooling around makes the inner loop
c  of the gain solution pretty clean.
c
c  Inputs:
c    nants      Number of antennae.
c    nbl        Number of baselines, must equal nants*(nants-1)/2
c    MinAnts    The minimum number of antenna that must be present.
c    RefAnt     The antenna to refer solutions to.
c    SumVM      The weighted sum of the visibilities.
c    SumVV      The weighted sum of the visibilities moduli squared.
c
c  Outputs:
c    GainOut    The complex gains to apply to correct the data.
c    Convrg     Whether it converged or not.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer b1(MAXBASE),b2(MAXBASE)
      integer i,j,k,Nblines,nantenna,nref
      complex Sum(MAXANT),Gain(MAXANT),Temp,SVM(MAXBASE)
      real Sum2(MAXANT),Wts(MAXANT)
      real SVV(MAXBASE)
      integer Indx(MAXANT)

c     Externals.
      integer ismax
c-----------------------------------------------------------------------
c
c  Check.
c
      if (nbl.ne.nants*(nants-1)/2)
     *  call bug('f','Number of antennae and baselines do not agree')
      if (nants.gt.MAXANT) call bug('f','Too many antennas')
c
c  Some intialising.
c
      do k = 1, nants
        Indx(k) = 0
        Wts(k) = 0
      enddo
c
c  Now find which baselines are there, and map the antenna numbers,
c
c  Squeeze out unnecessary baselines and antenna from this time slice.
c  This also works out the map from baseline number to "notional antenna
c  number" pairs, contained in B1 and B2.  INDX is set up to contain the
c  map from notional antenna number to the FITS-file antenna number.
c  Thus baseline i, corresponds to notional antenna nos.  B1(i) and
c  B2(i).  If INDX(k).eq.B1(i), then the real antenna no. is K.
c
c  This fancy compression is needed to give good clean inner loops
c  (no IF statements) in the gain solution routines.
c
      nantenna = 0
      NBlines = 0
      k = 0
      do j = 2, nants
        do i = 1, j-1
          k = k + 1
          if (SumVV(k).gt.0) then
            Wts(i) = Wts(i) + Weight(k)
            Wts(j) = Wts(j) + Weight(k)

            NBlines = NBlines + 1
            SVM(NBlines) = SumVM(k)
            SVV(NBlines) = SumVV(k)

            if (Indx(i).eq.0) then
              nantenna = nantenna + 1
              Indx(i) = nantenna
            endif
            B1(NBlines) = Indx(i)

            if (Indx(j).eq.0) then
              nantenna = nantenna + 1
              Indx(j) = nantenna
            endif
            B2(NBlines) = Indx(j)
          endif
        enddo
      enddo
c
c  Check that we have enough antennae. More initialisation.
c  Call the routine to get the solution.
c
      if (MinAnts.gt.nantenna) then
        Convrg = .false.
      else if (phase) then
        call phasol(NBlines,nantenna,Sum,SVM,b1,b2,gain,convrg)
        convrg = convrg .or. relax
      else
        call amphasol(NBlines,nantenna,Sum,Sum2,SVM,SVV,
     *                                           b1,b2,gain,convrg)
        convrg = convrg .or. relax
      endif
c
c  If it converged, unsqueeze the gains, and refer them to the reference
c  antenna.
c
      if (Convrg) then
        do i = 1, nants
          if (Indx(i).eq.0) then
            GainOut(i) = (0.0,0.0)
          else
            GainOut(i) = Gain(Indx(i))
          endif
        enddo

        nref = Refant
        if (nref.ne.0) then
          if (Wts(nref).le.0) nref = 0
        endif
        if (nref.eq.0) nref = ismax(nants,Wts,1)
        Temp = conjg(GainOut(nRef))/abs(GainOut(nRef))
        do i = 1, nants
          GainOut(i) = Temp * GainOut(i)
        enddo
      endif

      end

c***********************************************************************

      subroutine CalcStat(tgains,nsols,nbl,nants,SumVM,SumMM,SumVV,
     *                                Weight,Count,Gains)

      integer tgains, nsols, nbl, nants
      complex SumVM(nbl,nsols)
      real    SumMM(nsols), SumVV(nbl,nsols), Weight(nbl,nsols)
      integer Count(nsols)
      complex Gains(nants,nsols)
c-----------------------------------------------------------------------
c  Accumulate the various statistics.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real Resid,SumChi2,SumPhi,SumExp,SumWts,SumAmp,Phi,Amp,Sigma
      real m1,m2,wt
      complex g1,g2
      integer i,j,k,sol
      character line*80
c-----------------------------------------------------------------------
      SumChi2 = 0
      SumPhi = 0
      SumExp = 0
      SumWts = 0
      SumAmp = 0
c
c  Calculate residual.  Note that if the residuals is so small that
c  rounding error may be a problem (this will only happen with dummy
c  data), then take its absolute value.
c
      do sol = 1, nSols
        if (Count(sol).gt.0) then
          k = 0
          Resid = SumMM(sol)
          do j = 2, nants
            do i = 1, j-1
              k = k + 1
              Wt = Weight(k,sol)
              g1 = conjg(Gains(i,sol))
              g2 = Gains(j,sol)
              m1 = real(g1)**2 + aimag(g1)**2
              m2 = real(g2)**2 + aimag(g2)**2
              if (Wt.gt.0 .and. m1.gt.0 .and. m2.gt.0) then
                Resid = Resid - 2*real(g1*g2*SumVM(k,sol)) +
     *                m1*m2*SumVV(k,sol)
                SumWts = SumWts + Wt
                call amphase(g1*g2,amp,phi)
                SumPhi = SumPhi + Wt*phi**2
                SumAmp = SumAmp + Wt*(1-amp)**2
              endif
            enddo
          enddo
          SumChi2 = SumCHi2 + abs(Resid)
          SumExp = SumExp + Count(sol)
        endif
      enddo

      if (SumWts.gt.0) then
        Phi = sqrt(abs(SumPhi/(2*SumWts)))
        Amp = sqrt(abs(SumAmp/(2*SumWts)))
      else
        Phi = 0
        Amp = 0
      endif
      if (SumExp.gt.0) then
        Sigma = sqrt(abs(SumChi2/SumExp))
      else
        Sigma = 0
      endif
      write(line,'(a,f6.1)')'Rms of the gain phases (degrees):',phi
      call output(line)
      call HisWrite(tgains,'SELFCAL: '//line)
      write(line,'(a,1pg10.3)')'Rms deviation of gain from 1:',amp
      call output(line)
      call HisWrite(tgains,'SELFCAL: '//line)
      write(line,'(a,1pg10.3)')
     *  'Ratio of Actual to Theoretical noise:',Sigma
      call output(line)
      call HisWrite(tgains,'SELFCAL: '//line)

      end

c***********************************************************************

      subroutine phasol(Nblines,NAnts,Sum,SumVM,b1,b2,Gain,Convrg)

      logical Convrg
      integer Nblines,Nants
      integer b1(NBlines),b2(NBlines)
      complex SumVM(Nblines),Gain(NAnts),Sum(NAnts)
c-----------------------------------------------------------------------
c  Solve for the phase corrections which minimise the error.  This uses
c  a nonlinear Jacobi iteration, as suggested by Fred Schwab in
c  "Adaptive calibration of radio interferomter data", SPIE Vol. 231,
c  1980 International Optical Computing Conference (pp 18-24).  The
c  damping heuristics are copied from AIPS ASCAL.
c
c  Input:
c    NBlines    Number of baselines.
c    Nants      Number of antennae.
c    b1,b2      This gives the antennae pair for each baseline.
c    SumVM      Sum of Model*conjg(Vis)
c  Scratch:
c    Sum
c  Output:
c    Convrg     If .true., then the algorithm converged.
c    Gain       The antenna gain solution.
c-----------------------------------------------------------------------
      integer MaxIter
      real Epsi,Epsi2
      parameter (MaxIter=100,Epsi=1e-8,Epsi2=1e-4)

      real Factor,Change
      complex Temp
      integer Niter,i
c-----------------------------------------------------------------------
c
c  Initialise.
c
      do i = 1, NAnts
        Gain(i) = (1.0,0.0)
        Sum(i) = (0.0,0.0)
      enddo

      Factor = 0.8
      if (Nants.le.6) Factor = 0.5
c
c  Iterate.
c
      Convrg = .false.
      Niter = 0
      do while (.not.Convrg .and. Niter.lt.MaxIter)
        Niter = Niter + 1
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
        do i = 1, nblines
          Sum(b1(i)) = Sum(b1(i)) + Gain(b2(i)) *       SumVM(i)
          Sum(b2(i)) = Sum(b2(i)) + Gain(b1(i)) * conjg(SumVM(i))
        enddo
c
c  Update the gains.  The following will be flagged as a short loop on
c  the Cray, if we assume we have fewer than 32 antennae.  Hopefully
c  this will vectorise.  For "typical" cases, the absolute value
c  function in the next loop takes up about 30-40% of the run time of
c  this routine on a VAX.
c
        Change = 0
c#maxloop 32
        do i = 1, nants
          Temp = (Sum(i)/abs(Sum(i)))
          Temp = Gain(i) + Factor * (Temp - Gain(i))
          Temp = Temp/abs(Temp)
          Change = Change + real(Gain(i)-Temp)**2
     *                    + aimag(Gain(i)-Temp)**2
          Gain(i) = Temp
          Sum(i) = (0.0,0.0)
        enddo
        Convrg = Change/nants.lt.Epsi
      enddo
      Convrg = Change/nants.lt.Epsi2
      end

c***********************************************************************

      subroutine amphasol(NBlines,NAnts,Sum,Sum2,SumVM,SumVV,
     *                                        b1,b2,gain,convrg)

      logical Convrg
      integer NBlines,NAnts
      integer B1(NBlines),B2(NBlines)
      complex SumVM(NBlines),Gain(NAnts),Sum(NAnts)
      real SumVV(NBlines),Sum2(NAnts)
c-----------------------------------------------------------------------
c  Solve for the amplitudes and phase corrections which minimise
c  error. Algorithm is again Schwab's Jacobi iteration. The damping
c  heuristics are copied from AIPS ASCAL or dreamed up by me (as was the
c  initial gain estimate).
c
c  Input:
c    NBlines    Number of baselines.
c    Nants      Number of antennae.
c    b1,b2      This gives the antennae pair for each baseline.
c    SumVM      Sum of Model*conjg(Vis), for each baseline.
c    SumVV      Sum of Vis*conjg(Vis), for each baseline.
c  Scratch:
c    Sum
c  Output:
c    Convrg     If .true., then the algorithm converged.
c    Gain       The antenna gain solution.
c
c-----------------------------------------------------------------------
      integer maxiter
      real Epsi,Epsi2
      parameter (maxiter=100,Epsi=1e-8,Epsi2=1e-4)
      integer i,Niter
      real Factor,Change,SumRVV,SumWt,SumRVM
      complex Temp
      real Factors(11)
      data Factors/0.5,0.75,8*0.9,0.5/
c-----------------------------------------------------------------------
c
c  Calculate initial phase gain estimates.
c
      call phasol(NBlines,Nants,Sum,SumVM,b1,b2,gain,convrg)
      if (.not.convrg) return
c
c  Get an initial approximation of the gain solution.  This finds a
c  single real gain which minimises the error.  This helps stablise the
c  algorithm when the gain solution is very different from 1 (e.g. when
c  we are calculating a priori calibration factors).
c
      SumRVM = 0
      SumRVV = 0
      do i = 1, NBlines
        SumRVM = SumRVM + conjg(gain(b1(i)))*gain(b2(i))*SumVM(i)
        SumRVV = SumRVV + SumVV(i)
      enddo
      Factor = sqrt(abs(SumRVM / SumRVV))
c
c  Ready for the amplitude/phase solution.
c
      do i = 1, NAnts
        Gain(i) = Factor * Gain(i)
        Sum(i) = 0
        Sum2(i) = 0.0
      enddo
c
c  Iterate.
c
      Convrg = .false.
      Niter = 0
      do while (.not.Convrg .and. Niter.lt.MaxIter)
        Niter = Niter + 1
c
c  Set the damping constant. I do not think this is really necessary.
c  AIPS does it.
c
        if (Nants.le.6) then
          Factor = 0.5
        else
          Factor = Factors(min(11,Niter))
        endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
        do i = 1, nblines
          Sum(b1(i)) = Sum(b1(i)) + Gain(b2(i)) *       SumVM(i)
          Sum(b2(i)) = Sum(b2(i)) + Gain(b1(i)) * conjg(SumVM(i))
          Sum2(b1(i)) = Sum2(b1(i)) +
     *     (real(Gain(b2(i)))**2 + aimag(Gain(b2(i)))**2) * SumVV(i)
          Sum2(b2(i)) = Sum2(b2(i)) +
     *     (real(Gain(b1(i)))**2 + aimag(Gain(b1(i)))**2) * SumVV(i)
        enddo
c
c  Update the gains. The following should be flagged as a short loop
c  on the Cray, if we assume we have fewer than 32 antennae. Hopefully
c  this will vectorise.
c
        Change = 0
        SumWt = 0
c#maxloop 32
        do i = 1, nants
          Temp = Sum(i)/Sum2(i) - Gain(i)
          Gain(i) = Gain(i) + Factor * Temp
          Change = Change + (real(Temp)**2 + aimag(Temp)**2)
          SumWt  = SumWt  + (real(Gain(i))**2 + aimag(Gain(i))**2)
          Sum(i) = (0.0,0.0)
          Sum2(i) = 0.0
        enddo
        Convrg = Change/SumWt.lt.Epsi
      enddo
      Convrg = Change/SumWt.lt.Epsi2
      end
