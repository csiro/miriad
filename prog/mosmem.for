      program mosmem

c= mosmem - Maximum Entropy deconvolution for a mosaiced image
c& rjs
c: deconvolution
c+
c       MOSMEM is a MIRIAD task that performs a maximum entropy
c       deconvolution of a mosaiced image.  Optionally it can also
c       perform a joint deconvolution of a mosaic and single dish image.
c
c       MOSMEM will also work correctly on a single-pointing observation
c       interferometric observation.  In this case, it will be less
c       efficient than MAXEN, but it could be used when combining single
c       dish data with a single pointing.
c
c       MOSMEM spits out some information as it goes:
c
c       RMSFAC is the ratio (actual rms)/(theoretical rms).  It measures
c       the residuals (i.e. the difference between the dirty image and
c       the model modified by the point spread function).  RMSFAC should
c       converge to 1.
c
c       NormGrd is normalised gradient in the maximisation process.
c       Convergence requires this to be less than 0.05
c
c       Flux is the sum of all the pixel values in the model.
c
c@ map
c       One or perhaps two input dirty images (or cubes).  These should
c       have units of Jy/beam.  The first should be produced by INVERTs
c       mosaic mode.  The optional second dirty map can be a single-dish
c       image.  It must be on exactly the same pixel grid as the first
c       image.  If necessary, use REGRID to make this so.  If two inputs
c       are given, then a joint deconvolution of the two is performed.
c@ beam
c       One or perhaps two input dirty beams.  The first, corresponding
c       to the first input dirty map, will be produced by INVERTs mosaic
c       mode.  There is no default.  The second dirty beam (which must
c       be given if there are two dirty map inputs) gives the point-
c       spread function of the single dish dirty map.  This second dirty
c       beam need not be the same image size as the input dirty maps,
c       and may be appreciably smaller.  This single-dish beam is
c       assumed to be position-independent, but it need not be
c       symmetric.
c@ model
c       An initial estimate of the deconvolved image.  For point
c       sources, giving a good initial model may help convergence.  In
c       principle, this only helps convergence, but should not affect
c       the final solution.  The model could be the output from a
c       previous run of MOSMEM or any other deconvolution task.  It must
c       have flux units of Jy/pixel.  The default is a flat estimate,
c       with the correct flux.
c@ default
c       The default image.  This is the image that the final solution
c       will tend towards.  The final result will be influenced by this
c       default if the constrains that the data put on the solution are
c       weak.  The default is a flat estimate, with the correct flux.
c@ out
c       The name of the output map.  The units of the output will be
c       Jy/pixel.  It can be input to RESTOR to produce a restored
c       image, or alternatively to MOSMEM, as a model, to continue the
c       deconvolution process.
c@ niters
c       The maximum number of iterations.  The default is 30.
c@ region
c       This specifies the region to be deconvolved.  See the User's
c       Manual for instructions on how to specify this.  The default is
c       the entire image.
c@ measure
c       The entropy measure to be used, either "gull" (-p*log(p/e)) or
c       "cornwell" (-log(cosh(p)) -- also called the maximum emptiness
c       criteria).  Using the maximum emptiness criteria is not
c       recommended.
c@ tol
c       Tolerance of solution.  There is no need to change this from the
c       default of 0.01.
c@ q
c       One or two values (corresponding to the mosaic and single dish
c       observations).  These give estimates of the number of points per
c       beam.  MOSMEM can usually come up with a good, image-dependent
c       estimate.
c@ rmsfac
c       MOSMEM must be able to the theoretical rms noise of the input
c       dirty map(s), and will, by default, attempt to reduce the
c       residuals to have the same rms as this.  If the true rms noise
c       is different from the theoretical, you may give the factor to
c       multiply by to convert from theoretical to true rms noise.
c
c       The theoretical rms will usually be an optimistic estimate of
c       the true noise level.  The true noise will be increased by
c       calibration errors, confusion, poorly understood distant
c       sidelobes, etc.  The rmsfac factor gives some "fudge factor"
c       (usually greater than 1) to scale the theoretical noise estimate
c       by.  Either one or two values can be given, with the second
c       value corresponding to the single dish input.
c
c       For a mosaic, the theoretical rms is position dependent, and is
c       determined from information save by INVERT (the mostable table).
c       For a single dish image, the rms is assumed to be constant
c       across the field, and given by the "rms" item in the image.  If
c       the single dish input does not contain this item, then this must
c       be added before using MOSMEM.  This is easily done: for image
c       xxxx, use
c         puthd in=xxxx/rms value=????
c       where "????" is the rms noise in Jy/beam.
c@ factor
c       The flux calibration factor.  This is only relevant when doing a
c       joint deconvolution of a mosaic and a single-dish image.  It
c       gives the factor which the single-dish data should be multiplied
c       by to convert it to the same flux units as the mosaic.  The
c       default is 1.  If the "dofactor" options is used (see below),
c       MOSMEM solves for this parameter.
c@ flux
c       An estimate of the integrated flux of the source.  This
c       parameter is generally not useful if there is an input single
c       dish image.  Giving MOSMEM a good value for the integrated flux
c       will help it find a good solution.  On the other hand, giving a
c       poor value may do harm.  Normally MOSMEM will NOT constrain the
c       integrated flux to be this value, but see the "doflux" option
c       below.  The default is image-dependent for measure=gull, and
c       zero for measure=cornwell.  A value can be given for each plane
c       being deconvolved.
c@ options
c       Task enrichment parameters.  Several can be given, separated by
c       commas.  Minimum match is used.  Possible values are:
c         doflux     Constrain the solution to have the correct
c                    integrated flux (normally the integrated flux is
c                    not constrained).  The integrated flux is
c                    determined from the "flux" parameter or (if no flux
c                    parameter is given) from the default image.  This
c                    option cannot be used if a single dish input map is
c                    also given.
c         dofactor   Solve for the flux calibration factor.
c         verbose    Give lots of messages during the iterations.  The
c                    default is to give a one line message at each
c                    iteration.
c
c$Id: mosmem.for,v 1.5 2011/10/31 00:37:30 wie017 Exp $
c--
c  History:
c    rjs  23nov94  Adapted from MAXEN.
c    rjs   3dec94  Doc only.
c    rjs   6feb95  Copy mosaic table to output component file.
c    rjs  10aug95  New routine to modify alpha and beta.
c    rjs  12oct95  Support "default" and "model" being different sizes
c                  from the deconvolved region.
c    rjs  27oct95  Increased max length of filenames.
c    rjs  24nov95  Default default image is now proportional to the
c                  gain.
c    rjs  29Feb96  Call xyflush after each plane.
c    mwp  27May97  Allow flux estimates for each plane.
c    rjs  21jun97  Tidies up above change.
c    rjs  24jun97  Correct call to alignini
c    rjs  02jul97  cellscal change.
c    rjs  23jul97  Add pbtype.
c    rjs  01aug97  Fiddle default to make it always reasonably valued.
c    rjs  28nov97  Increase max number of boxes.
c    rjs  23feb98  Added extra protection against underflow.
c    rjs  17mar98  Added single dish support, "factor" parameter and
c                  options=dofactor.
c    rjs  19jan99  It was failing to access the correct single dish
c                  plane!!  Also some changes in some checks and
c                  guessing TFlux to make it more robust.
c    rjs  22jan99  Fudge to get the rms noise information to propogate
c                  through correctly for single pointing work.
c    rjs  10feb99  Get measure=cornwell to work by setting initial
c                  estimate to zero.
c    nebk 07sep04  Add some words about the output information mosmem
c                  spits out as it goes
c    mhw  27oct11  Use ptrdiff type for memory allocations
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      integer MaxRun,MaxBoxes
      parameter (MaxRun=3*maxdim,MaxBoxes=2048)

      integer gull,cornwell
      parameter (gull=1,cornwell=2)

      logical   converge, dofac, doflux, dosingle, positive, verbose
      integer   blc(3), Boxes(maxBoxes), i, icentre, imax, imin,
     *          jcentre, jmax, jmin, k, kmax, kmin, lBeama, lBeamb,
     *          lDef, lMapa, lMapb, lModel, lOut, maxniter, maxPoint,
     *          measure, n1, n2, naxis, nBeam(3), nDef(3), nfret,
     *          niter, nMap(3), nMapb(3), nModel(3), nOut(MAXNAX),
     *          nPoint, nRun, Run(3,MaxRun), trc(3), xdoff,
     *          xmax, xmin, xmoff, ydoff,
     *          ymax, ymin, ymoff, zdoff, zmoff
      ptrdiff   Cnvl, pDef, pEst, pMapa, pMapb, pNewEst,
     *          pNewResa, pNewResb, pResa, pResb, pWta, pWtb
      real      Alpha, Beta, De, Df, fac, ffacDef, ffacSD, Flux,
     *          fluxlist(maxdim), Grad11, GradEE, GradEF, GradEH,
     *          GradEJ, GradFF, GradFH, GradFJ, GradHH, GradJJ, Immax,
     *          Immin, J0, J1, OStLen1, OStLen2, Qa, Qb, Qest, Rmsa,
     *          Rmsb, rmsfaca, rmsfacb, StLen1, StLen2, StLim, temp,
     *          TFlux, Tol, TRms, Trms2
      character BeamNam*64, BeamSin*64, DefNam*64, entropy*8, line*80,
     *          MapNam*64, MapSin*64, ModelNam*64, OutNam*64, version*72

      logical   hdprsnt
      integer   len1
      character itoaf*4, versan*80
      external  hdprsnt, itoaf, len1, versan
c-----------------------------------------------------------------------
      version = versan('mosmem',
     *                 '$Revision: 1.5 $',
     *                 '$Date: 2011/10/31 00:37:30 $')
c
c  Get and check the input parameters.
c
      call keyini
      call keya('map',MapNam,' ')
      call keya('map',MapSin,' ')
      call keya('beam',BeamNam,' ')
      call keya('beam',BeamSin,' ')
      call keya('out',OutNam,' ')
      if (MapNam.eq.' ' .or. BeamNam.eq.' ' .or. OutNam.eq.' ')
     *  call bug('f','A file name was missing from the parameters')
      if (MapSin.eq.' '.neqv.BeamSin.eq.' ') call bug('f',
     *  'Input single dish map and beam names should be given')
      dosingle = MapSin.ne.' '
      call keya('model',ModelNam,' ')
      call keya('default',DefNam,' ')
      call keyr('tol',Tol,0.01)
      call keyi('niters',maxniter,30)
      call keyr('q',Qa,0.0)
      qb = 0
      if (dosingle) call keyr('q',Qb,0.0)
      call keyr('rmsfac',rmsfaca,1.0)
      rmsfacb = 1
      if (dosingle) call keyr('rmsfac',rmsfacb,1.0)
      nfret = 0
      call mkeyr('flux',fluxlist,maxdim,nfret)
      if (dosingle .and. nfret.gt.0) then
        call bug('w','Setting the "flux" parameter when using '//
     *                'single-dish data may be unwise')
      endif
      call BoxInput('region',MapNam,Boxes,MaxBoxes)
      call GetOpt(verbose,doflux,dofac,entropy)
      if (dofac .and. .not.dosingle) call bug('f',
     *  'options=dofactor is valid only with joint deconvolutions')
      if (doflux .and. dosingle) call bug('f',
     *  'option=doflux cannot be used when a single dish is given')
      if (min(rmsfaca,rmsfacb).le.0.0)
     *  call bug('f','RMSFAC is not positive')
      if (min(rmsfaca,rmsfacb).lt.0.9)
     *  call bug('w','RMSFAC seems small')
      if (maxniter.lt.0) call bug('f','NITERS was given a bad value')
      if (Tol.le.0.0)
     *  call bug('f','The TOL parameter must be positive valued')
      call keyr('factor',fac,1.0)
      call keyfin
c
c  Get ready.
c
      if (entropy.eq.'gull') then
        measure = gull
        positive = .true.
      else if (entropy.eq.'cornwell') then
        measure = cornwell
        positive = .false.
      endif
c
c  Open the input maps.
c
      call xyopen(lMapa,MapNam,'old',3,nMap)
      if (max(nMap(1),nMap(2)).gt.maxdim) call bug('f','Map too big')
      call rdhdi(lMapa,'naxis',naxis,3)
      naxis = min(naxis,MAXNAX)
      call coInit(lMapa)
c
c  Open the single dish image.
c
      if (dosingle) then
        call xyopen(lMapb,MapSin,'old',3,nMapb)
        if (hdprsnt(lMapb,'mostable')) call bug('w',
     *    'Is the second input map really a single dish observation?')
        if (nMap(1).ne.nMapb(1) .or. nMap(2).ne.nMapb(2) .or.
     *     nMap(3).ne.nMapb(3)) call bug('f',
     *    'Input maps differ in size; they must have identical grids')
        call AlignIni(lMapa,lMapb,nMap(1),nMap(2),nMap(3),
     *    xmoff,ymoff,zmoff)
        if (xmoff.ne.0 .or. ymoff.ne.0 .or. zmoff.ne.0) call bug('f',
     *    'Input maps are misaligned; they must have identical grids')
        call rdhdr(lMapb,'rms',Trms,0.0)
        if (Trms.le.0) call bug('f',
     *    'The single dish map must have an rms item')
      else
        TRms = 0.0
      endif
      TRms2 = TRms*TRms

      call BoxMask(lMapa,boxes,maxboxes)
      if (dosingle) call BoxMask(lMapb,boxes,maxboxes)
      call BoxSet(Boxes,3,nMap,' ')
      call BoxInfo(Boxes,3,blc,trc)
      imin = blc(1)
      imax = trc(1)
      jmin = blc(2)
      jmax = trc(2)
      kmin = blc(3)
      kmax = trc(3)
      nOut(1) = imax - imin + 1
      nOut(2) = jmax - jmin + 1
      nOut(3) = kmax - kmin + 1

      if (nfret.lt.nOut(3)) then
        TFlux = 0
        if (nfret.ge.1) TFlux = fluxlist(nfret)
        do i = nfret+1, nOut(3)
          fluxlist(i) = TFlux
        enddo
      else if (nfret.gt.nOut(3)) then
        call bug('w','More flux estimates than planes...')
        call bug('w','ignoring extras.')
      endif
c
c  Open the mosaic beam, and get some info about it.
c
      call xyopen(lBeama,BeamNam,'old',3,nBeam)
      n1 = nBeam(1)
      n2 = nBeam(2)
      if (max(n1,n2).gt.maxdim) call bug('f','Beam too big')
      call BeamChar(lBeama,n1,n2,Qest,icentre,jcentre,ffacSD)
      write(line,'(a,f6.1)')'For '//BeamNam(1:len1(BeamNam))//
     *        ', an estimate of Q is',Qest
      call output(line)
      if (Qa.gt.0.0) then
        write(line,'(a,1pg8.1)')
     *                'Using user given pixels per beam of',Qa
        call output(line)
      else
        Qa = Qest
      endif
      call mcInitF(lBeama)
c
c  Open the single dish beam, if needed.
c
      if (dosingle) then
        call xyopen(lBeamb,BeamSin,'old',2,nBeam)
        n1 = nBeam(1)
        n2 = nBeam(2)
        if (max(n1,n2).gt.maxdim) call bug('f','Beam too big')
        call BeamChar(lBeamb,n1,n2,Qest,icentre,jcentre,ffacSD)
        write(line,'(a,f6.1)')'For '//BeamSin(1:len1(BeamSin))//
     *        ', an estimate of Q is',Qest
        call output(line)
        if (Qb.gt.0.0) then
          write(line,'(a,1pg8.1)')
     *                'Using user given pixels per beam of',Qb
          call output(line)
        else
          Qb = Qest
        endif
        call SDConIni(Cnvl,lBeamb,n1,n2,icentre,jcentre,
     *    nout(1),nout(2))
        call xyclose(lBeamb)
      else
        Qb = 0.0
      endif
c
c  Open the model if needed, and check that is is the same size as the
c  output.
c
      if (ModelNam.ne.' ') then
        call xyopen(lModel,ModelNam,'old',3,nModel)
        call AlignIni(lModel,lMapa,nMap(1),nMap(2),nMap(3),
     *                                        xmoff,ymoff,zmoff)
      endif
c
c  Initial values for alpha and beta.
c
      Alpha = 0
      Beta = 0
c
c  Open the default image if needed, and check that is is the same size
c  as the output.
c
      ffacDef = 0
      if (DefNam.ne.' ') then
        call xyopen(lDef,DefNam,'old',3,nDef)
        call AlignIni(lDef,lMapa,nMap(1),nMap(2),nMap(3),
     *                                        xdoff,ydoff,zdoff)
      endif
c
c  Open up the output.
c
      do i = 4, naxis
        nOut(i) = 1
      enddo
      call xyopen(lOut,OutNam,'new',naxis,nOut)
      call mkHead(lMapa,lOut,blc,trc,version)
      call xyflush(lOut)
c
c  Loop.
c
      maxPoint = 0
      do k = kmin, kmax
        if (kmin.ne.kmax) call output('Plane: '//itoaf(k))

        call BoxRuns(1,k,' ',boxes,Run,MaxRun,nRun,
     *                                xmin,xmax,ymin,ymax)
        call BoxCount(Run,nRun,nPoint)
        if (nPoint.gt.0) then
c
c  Allocate arrays to hold everything.
c
        if (nPoint.gt.maxPoint) then
          if (maxPoint.gt.0) then
            call memFrep(pEst,maxPoint,'r')
            call memFrep(pDef,maxPoint,'r')
            call memFrep(pNewEst,maxPoint,'r')
            call memFrep(pMapa,maxPoint,'r')
            call memFrep(pWta,maxPoint,'r')
            call memFrep(pResa,maxPoint,'r')
            call memFrep(pNewResa,maxPoint,'r')
            if (dosingle) then
              call memFrep(pMapb,maxPoint,'r')
              call memFrep(pWtb,maxPoint,'r')
              call memFrep(pResb,maxPoint,'r')
              call memFrep(pNewResb,maxPoint,'r')
            endif
          endif
          maxPoint = nPoint
          call memAllop(pEst,maxPoint,'r')
          call memAllop(pDef,maxPoint,'r')
          call memAllop(pNewEst,maxPoint,'r')
          call memAllop(pMapa,maxPoint,'r')
          call memAllop(pWta,maxPoint,'r')
          call memAllop(pResa,maxPoint,'r')
          call memAllop(pNewResa,maxPoint,'r')
          if (dosingle) then
            call memAllop(pMapb,maxPoint,'r')
            call memAllop(pWtb,maxPoint,'r')
            call memAllop(pResb,maxPoint,'r')
            call memAllop(pNewResb,maxPoint,'r')
          else
            pMapb = pMapa
            pWtb = pWta
            pResb = pResa
            pNewResb = pNewResa
          endif
        endif
c
c  Initialise the mosaic routines, get 1/sigma**2, and get the dirty
c  image.
c
        call mcPlaneR(lMapa,k,Run,nRun,nPoint)
c
c
c  FUDGE. The beam of single pointing observations does not
c  contain the appropriate information to compute the rms
c  noise -- only the dirty image does. So, if its a single
c  pointing observation, then get the rms from the dirty
c  image, and scale the Wta arrary appropriately.
c
        if (.not.hdprsnt(lBeama,'mostable')) then
          call rdhdr(lMapa,'rms',temp,1.0)
          if (temp.le.0) temp = 1
          call SPntFid(memr(pWta),nPoint,temp)
        else
          call mcSigma2(memr(pWta),nPoint,.false.)
        endif

        call mcGain(memr(pEst),nPoint)
        call xysetpl(lMapa,1,k)
        call GetPlane(lMapa,Run,nRun,0,0,nMap(1),nMap(2),
     *                        memr(pMapa),maxPoint,nPoint)
        if (dosingle) then
          call xysetpl(lMapb,1,k)
          call GetPlane(lMapb,Run,nRun,0,0,nMap(1),nMap(2),
     *                        memr(pMapb),maxPoint,nPoint)
          call SDConWt(memr(pEst),memr(pWtb),nPoint)
        endif
c
c  Get the default image.
c
        if (DefNam.eq.' ') then
          call Copy(nPoint,memr(pEst),memr(pDef))
        else
          call AlignGet(lDef,Run,nRun,k,xdoff,ydoff,zdoff,
     *        nDef(1),nDef(2),nDef(3),memr(pDef),maxPoint,nPoint)
          call DefGain(memr(pEst),memr(pDef),nPoint)
        endif
c
c  Get the Default map.
c
        Tflux = fluxlist(k-kmin+1)
        if (TFlux.le.0 .and. dosingle) then
          call GetFxSD(memr(pEst),memr(pMapb),nPoint,TFlux)
          TFlux = fac * TFlux / ffacSD
        endif
        if (Tflux.le.0 .and. DefNam.ne.' ') then
          if (ffacDef.le.0) call GetFFDef(lDef,ffacDef)
          call GetFxDef(memr(pDef),nPoint,TFlux)
          TFlux = TFlux / ffacDef
        endif
        if (Tflux.le.0) then
          call GetRms(memr(pWta),nPoint,TFlux)
          TFlux = RmsFaca*TFlux*nPoint/Qa
        endif
        call DefFudge(nPoint,memr(pDef),TFlux)
        write(line,'(a,1pe10.3)')'Using an integrated flux of',TFlux
        call output(line)
c
c  Get the Estimate and Residual.
c
        if (ModelNam.eq.' ') then
          if (positive) then
            call Copy(nPoint,memr(pDef),memr(pEst))
          else
            call Zeroit(nPoint,memr(pEst))
          endif
        else
          call AlignGet(lModel,Run,nRun,k,xmoff,ymoff,zmoff,
     *        nModel(1),nModel(2),nModel(3),memr(pEst),
     *        maxPoint,nPoint)
          if (positive) call ClipIt(memr(pDef),memr(pEst),nPoint)
        endif

        call Diff(memr(pEst),memr(pMapa),memr(pResa),nPoint,Run,nRun)
        if (dosingle) call SDConDif(cnvl,memr(pEst),memr(pMapb),fac,
     *   memr(pResb),memr(pWtb),nPoint,Run,nRun,xmax,ymax)
c
c  Get all the information.
c
        call GetInfo(nPoint,
     *      memr(pEst),memr(pResa),memr(pWta),memr(pResb),memr(pWtb),
     *      TRms2*fac*fac,measure,memr(pDef),dosingle,Alpha,Beta,
     *      Qa,Qb,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *      GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb)
c-----------------------------------------------------------------------
c  Now start to iterate at long last.
c
      OStLen1 = 0
      OStLen2 = 0
      Converge = .false.
      Niter = 0
      do while (.not.converge .and. Niter.lt.MaxNiter)
        Niter = Niter + 1
c
c  Update Alpha and Beta.
c
        De = nPoint*(Rmsa*Rmsa - RmsFaca*RmsFaca)
        if (dosingle) then
          Df = nPoint*(Rmsb*Rmsb - RmsFacb*RmsFacb)
        else
          Df = Flux - TFlux
        endif
        call NewAlpB(Alpha,Beta,De,Df,doflux .or. dosingle,GradEE,
     *    GradEF,GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)
c
c  Calculate the next step to take.
c
        call CalStep(nPoint,dosingle,
     *      memr(pResa),memr(pWta),memr(pResb),memr(pWtb),
     *      TRms2*fac*fac,memr(pEst),memr(pNewEst),memr(pDef),
     *      measure,Alpha,Beta,Qa,Qb,J0)
c
c  Determine the max step length, and the initial step length.
c
        StLim = 1
        if (GradJJ.gt.0) StLim = min(1.4,0.15*Grad11/GradJJ)
        StLen1 = min(0.5*(1+OStLen1),StLim)
        OStLen1 = StLen1
        J0 = J0 * StLen1
c
c  Take the plunge.
c
        call TakeStep(nPoint,memr(pEst),memr(pNewEst),
     *                                StLen1,positive,StLim)
c
c  Convolve the estimate with the beam and subtract the map.
c
        call Diff(memr(pNewEst),memr(pMapa),memr(pNewResa),
     *                                        nPoint,Run,nRun)
        if (dosingle) call SDConDif(cnvl,memr(pNewEst),memr(pMapb),fac,
     *    memr(pNewResb),memr(pWtb),nPoint,Run,nRun,xmax,ymax)
c
c  Work out what was really the best step length.
c
        call ChekStep(nPoint,memr(pEst),memr(pNewEst),memr(pDef),
     *        memr(pNewResa),memr(pWta),memr(pNewResb),memr(pWtb),
     *        TRms2*fac*fac,dosingle,measure,Alpha,Beta,Qa,Qb,J1)
        if (J0-J1.ne.0) then
          StLen2 = J0/(J0-J1)
        else
          StLen2 = 1
        endif
        StLen2 = 0.5*(StLen2 + OStLen2)
        StLen2 = min(StLen2,StLim/StLen1)
        OStLen2 = StLen2
c
c  Now interpolate between the actual step and the one we should
c  have taken. Only interpolate if its absolutely necessary. That
c  is if the second step length is not near 1. In practise it will
c  be near 1 on the first few iterations.
c
        if (abs(StLen2-1.0).gt.0.05) then
          call IntStep(nPoint,memr(pEst),memr(pNewEst),StLen2)
          call IntStep(nPoint,memr(pResa),memr(pNewResa),StLen2)
          if (dosingle) call IntStep(nPoint,memr(pResb),
     *                                    memr(pNewResb),StLen2)
        else
          StLen2 = 1
          call Swap(pEst,pNewEst)
          call Swap(pResa,pNewResa)
          if (dosingle) call Swap(pResb,pNewResb)
        endif
c
c  Calculate a new estimate for Q using a magic formula.
c
      if (abs(StLen1-1.0).lt.0.05) then
        temp =  sqrt((1.0/max(0.5,min(2.0,StLen1*StLen2))+3.0)/4.0)
        Qa = Qa * temp
        Qb = Qb * temp
      endif
c
c  Get all the information.
c
        if (dofac .and. abs(StLen1-1.0).lt.0.05)
     *    call NewFac(nPoint,memr(pResb),memr(pMapb),fac,TRms2)
        call GetInfo(nPoint,
     *      memr(pEst),memr(pResa),memr(pWta),memr(pResb),memr(pWtb),
     *      TRms2*fac*fac,measure,memr(pDef),dosingle,Alpha,Beta,
     *      Qa,Qb,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *      GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb)
c
c  Reawaken the user with more crap to let him/her ponder over
c  what could possibly be going wrong. Give him/her as much as
c  possible to ponder over.
c
        if (verbose) then
          call output('Iteration '//itoaf(niter))
          write(line,20) Alpha,Beta,Qa,Qb
          call output(line)
          write(line,22) Flux,GradJJ/Grad11,Rmsa,Rmsb
          call output(line)
          write(line,21) Immin,Immax,fac
          call output(line)
          write(line,23) StLim,StLen1,StLen2
          call output(line)
        else
          if (dosingle) then
            write(line,25) Niter,Rmsa,Rmsb,fac,Flux,GradJJ/Grad11
          else
            write(line,24) Niter,Rmsa,Flux,GradJJ/Grad11
          endif
          call output(line)
        endif

  20    format('  Alpha =',1pe12.3,' Beta   =',1pe12.3,
     *        ' Q       =',1p2e12.3)
  21    format('  Immin =',1pe12.3,' Immax  =',1pe12.3,
     *        ' Factor  =',1pe12.3)
  22    format('  Flux  =',1pe12.3,' NormGrd=',1pe12.3,
     *        ' Rms     =',1p2e12.3)
  23    format('  StLim =',1pe12.3,' StLen1 =',1pe12.3,
     *        ' StLen2  =',1pe12.3)
  24    format(' Iter =',i3,' RmsFac =',1pe10.3,' Flux =',1pe10.3,
     *        ' NormGrd =',0pf6.3)
  25    format(' Iter=',i3,' RmsFac=',f6.2,1x,f6.2,' Factor=',1pe9.3,
     *        ' Flux=',1pe9.3,' NormGrd=',0pf5.3)
c
c  Check for convergence.
c
        converge = (Rmsa-RmsFaca.lt.0.05*Rmsfaca)             .and.
     *             ((Flux-TFlux).lt.0.05*TFlux .or. .not.doflux) .and.
     *              (GradJJ/Grad11.lt.Tol)
        if (dosingle .and. converge)
     *        converge = Rmsb-RmsFacb.lt.0.05*RmsFacb
      enddo
c-----------------------------------------------------------------------
c
c  We have finished processing this plane. More info to the user!
c
          if (converge) then
            call output('MOSMEM seems to have converged')
          else
            call output('Failed to converge in NITERS iterations')
          endif
        endif
c
c  Write out this plane.
c
        call xysetpl(lOut,1,k-kmin+1)
        call PutPlane(lOut,Run,nRun,1-imin,1-jmin,
     *                        nOut(1),nOut(2),memr(pEst),nPoint)
        call xyflush(lOut)
      enddo
c
c  Close up the files. Ready to go home.
c
      call coFin(lMapa)
      call xyclose(lMapa)
      call xyclose(lBeama)
      if (dosingle) call xyclose(lMapb)
      if (ModelNam.ne.' ') call xyclose(lModel)
      call xyclose(lOut)
c
c  Release memory.
c
      if (maxPoint.le.0) call bug('f','No data deconvolved')
      call memFrep(pEst,maxPoint,'r')
      call memFrep(pDef,maxPoint,'r')
      call memFrep(pNewEst,maxPoint,'r')
      call memFrep(pMapa,maxPoint,'r')
      call memFrep(pWta,maxPoint,'r')
      call memFrep(pResa,maxPoint,'r')
      call memFrep(pNewResa,maxPoint,'r')
      if (dosingle) then
        call memFrep(pMapb,maxPoint,'r')
        call memFrep(pWtb,maxPoint,'r')
        call memFrep(pResb,maxPoint,'r')
        call memFrep(pNewResb,maxPoint,'r')
      endif
c
c  Thats all folks.
c
      end

c***********************************************************************

      subroutine SPntFid(Wta,nPoint,rms)

      integer nPoint
      real Wta(nPoint),rms
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nPoint
        Wta(i) = 1.0/(rms*rms)
      enddo

      end

c***********************************************************************

      subroutine GetOpt(verbose,doflux,dofac,entropy)

      logical verbose,doflux,dofac
      character entropy*(*)
c-----------------------------------------------------------------------
c  Get extra processing options and the entropy measure.
c
c  Output:
c    verbose    Give lots of messages.
c    doflux     Constrain the flux.
c    entropy    The entropy measure.
c-----------------------------------------------------------------------
      integer NOPT
      parameter (NOPT=3)
      logical present(NOPT)
      character opts(NOPT)*8

      integer NMEASURE
      parameter (NMEASURE=2)
      integer nout
      character measure(NMEASURE)*8

      data measure/'gull    ','cornwell'/
      data opts/'verbose ','doflux  ','dofactor'/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPT)
      verbose = present(1)
      doflux  = present(2)
      dofac   = present(3)

      call keymatch('measure',NMEASURE,measure,1,entropy,nout)
      if (nout.eq.0) entropy = measure(1)
      end

c***********************************************************************

      subroutine Swap(a,b)

      integer a,b
c-----------------------------------------------------------------------
      integer t
c-----------------------------------------------------------------------
      t = a
      a = b
      b = t
      end

c***********************************************************************

      subroutine Copy(n,From,To)

      integer n
      real From(n),To(n)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        To(i) = From(i)
      enddo
      end

c***********************************************************************

      subroutine GetFFDef(lDef,ffacDef)

      integer lDef
      real ffacDef
c-----------------------------------------------------------------------
c  Get the factor needed to convert the sum of the pixel values into
c  an integrated flux.
c-----------------------------------------------------------------------
      character bunit*32
      real bmaj,bmin,cdelt1,cdelt2
c-----------------------------------------------------------------------
      call rdhda(lDef,'bunit',bunit,' ')
      call lcase(bunit)

      if (bunit.eq.'jy/pixel') then
        ffacDef = 1
      else if (bunit.eq.'jy/beam') then
        call rdhdr(lDef,'bmaj',bmaj,0.0)
        call rdhdr(lDef,'bmin',bmin,0.0)
        call rdhdr(lDef,'cdelt1',cdelt1,0.0)
        call rdhdr(lDef,'cdelt2',cdelt2,0.0)
        if (abs(cdelt1*cdelt2*bmaj*bmin).le.0.0) call bug('f',
     *    'Could not determine the beam parameters of default image')
        ffacDef = 1.1331 * abs(bmaj * bmin / (cdelt1*cdelt2))
      else
        call bug('w','Unrecognised flux units for the default image')
        call bug('w','Pretending default image units are JY/PIXEL')
        ffacDef = 1
      endif

      end

c***********************************************************************

      subroutine DefGain(Gain,Def,nPoint)

      integer nPoint
      real Gain(nPoint),Def(nPoint)
c-----------------------------------------------------------------------
c  Apply the gains to the default image.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nPoint
        Def(i) = Gain(i) * Def(i)
      enddo

      end

c***********************************************************************

      subroutine DefFudge(npoint,Def,TFlux)

      integer npoint
      real Def(npoint),TFlux
c-----------------------------------------------------------------------
c  This clips the default so that its well defined, and scales it so
c  that it has the correct integrated flux.
c-----------------------------------------------------------------------
      real alpha,temp
      double precision sum
      integer i
c-----------------------------------------------------------------------
      sum = 0
      temp = 1e-4 * TFlux/nPoint
      do i = 1, npoint
        sum = sum + max(Def(i),temp)
      enddo

      if (Sum.le.0) call bug('f','Cannot scale default image')

      alpha = TFlux/Sum
      do i = 1, npoint
        Def(i) = alpha*max(Def(i),temp)
      enddo

      end

c***********************************************************************

      subroutine ClipIt(Def,Est,nPoint)

      integer nPoint
      real Def(nPoint),Est(nPoint)
c-----------------------------------------------------------------------
c  Set up the minimum of the default image.
c
c  Input:
c    clip
c    nPoint
c    Def        The default image.
c  Input/Output:
c    Est        The estimate image, whixh is being clipped.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nPoint
        Est(i) = max(Est(i),0.1*Def(i))
      enddo
      end

c***********************************************************************

      subroutine BeamChar(lBeam,n1,n2,Qest,icentre,jcentre,fluxfac)

      integer lBeam,n1,n2,icentre,jcentre
      real Qest,fluxfac
c-----------------------------------------------------------------------
c  Determine the location of the centre of the beam, and get an estimate
c  of the number of points per beam.
c
c  Inputs:
c    lBeam      Handle of the beam file.
c    n1,n2      Size of the beam.
c
c  Outputs:
c    icentre,jcentre Coordinates of the centre of the beam. This is
c               assumed to be near the image centre.
c    Qest       An estimate of the number of points per beam.
c    fluxfac    Integration of the beam.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real tol
      parameter (tol=0.1)
      integer i,j
      real bmax,Data(maxdim)
      double precision Sum,Sum2
c-----------------------------------------------------------------------
      Sum = 0
      Sum2 = 0
      bmax = 0
      icentre = 0
      jcentre = 0
      do j = 1, n2
        call xyread(lBeam,j,Data)
        do i = 1, n1
          Sum = Sum + Data(i)
          if (data(i).gt.tol) Sum2 = Sum2 + Data(i)*Data(i)
          if (Data(i).gt.bmax) then
            icentre = i
            jcentre = j
            bmax = Data(i)
          endif
        enddo
      enddo

      Qest = sqrt(2.7*Sum2)
      fluxfac = Sum
      if (abs(1-bmax).gt.0.01) call bug('f','Beam peak is not 1')
      end

c***********************************************************************

      subroutine GetFxSD(Gain,Model,nPoint,flux)

      integer nPoint
      real Gain(nPoint),Model(nPoint),flux
c-----------------------------------------------------------------------
c  Determine the flux of an image.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      flux = 0
      do i = 1, nPoint
        if (Gain(i)*Model(i).gt.0) flux = flux + Gain(i)*Model(i)
      enddo

      end

c***********************************************************************

      subroutine GetFxDef(Model,nPoint,flux)

      integer nPoint
      real Model(nPoint),flux
c-----------------------------------------------------------------------
c  Determine the flux of an image.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      flux = 0
      do i = 1, nPoint
        flux = flux + Model(i)
      enddo

      end

c***********************************************************************

      subroutine GetRms(Wt,nPoint,Rms)

      integer nPoint
      real Wt(nPoint),Rms
c-----------------------------------------------------------------------
c  Determine the RMS value.
c-----------------------------------------------------------------------
      integer i,Count
c-----------------------------------------------------------------------
      Rms = 0
      Count = 0

      do i = 1, nPoint
        if (Wt(i).gt.0) then
          Rms = Rms + 1/Wt(i)
          Count = Count + 1
        endif
      enddo

      Rms = sqrt(Rms/Count)

      end

c***********************************************************************

      subroutine IntStep(nPoint,Old,New,FracNew)

      integer nPoint
      real FracNew
      real Old(nPoint),New(nPoint)
c-----------------------------------------------------------------------
c  Update the current image by interpolating between two previous ones.
c-----------------------------------------------------------------------
      real FracOld
      integer i
c-----------------------------------------------------------------------
      FracOld = 1.0 - FracNew
      do i = 1, nPoint
        Old(i) = FracOld*Old(i) + FracNew*New(i)
      enddo

      end

c***********************************************************************

      subroutine CalStep(nPoint,dosingle,Resa,Wta,Resb,Wtb,TRms2,
     *  Est,Step,Def,measure,Alpha,Beta,Qa,Qb,J0)

      integer nPoint,measure
      real Alpha,Beta,Qa,Qb,J0,TRms2
      real Def(nPoint),Est(nPoint),Step(nPoint)
      real Resa(nPoint),Resb(nPoint),Wta(nPoint),Wtb(nPoint)
      logical dosingle
c-----------------------------------------------------------------------
c  Calculate the step to take next.
c
c  Inputs:
c    nPoint     Number of points.
c    Est        Current estimate of the MEM solution.
c    Def        The default image.
c    Resa       Current residuals of the mosaic.
c    Wta        1/Sigma**2 for the mosaic image.
c    Resb       Current residuals for the single dish.
c    Wtb        1/Gain of the mosaic.
c    TRms2      sigma2 for the single dish.
c    dosingle   True if joint deconvolution with single dish data.
c    measure    Determines the entropy measure used.
c
c  Output:
c    Step       The step to take towards a better estimate.
c    Length     A measure of the length of the step.
c-----------------------------------------------------------------------
      integer run
      parameter (run=1024)
      integer n,l,ltot
      real Diag, GradJ, Stepd
      real dH(run),d2H(run)
c-----------------------------------------------------------------------
      J0 = 0
      n = 0
      do while (n.lt.nPoint)
        ltot = min(nPoint-n,run)
        call EntFunc(measure,ltot,Est(n+1),Def(n+1),dH,d2H)
        do l = 1, ltot
          if (dosingle) then
            GradJ = dH(l) - 2*Qa*Alpha*Resa(n+l)*Wta(n+l)
     *                    - 2*Qb*Beta *Resb(n+l)*Wtb(n+l)/TRms2
            Diag = 1.0/(2.0*Alpha*Qa*Qa*Wta(n+l) +
     *                  2.0*Beta* Qb*Qb*Wtb(n+l)*Wtb(n+l)/TRms2-d2H(l))
          else
            GradJ = dH(l) - 2.0*Qa*Alpha*Resa(n+l)*Wta(n+l) - Beta
            Diag = 1 / (2*Alpha*Qa*Qa*Wta(n+l) - d2H(l))
          endif
          Stepd = Diag*GradJ
          J0 = J0 + GradJ*Stepd
          Step(n+l) = Stepd
        enddo
        n = n + ltot
      enddo

      end

c***********************************************************************

      subroutine TakeStep(nPoint,Est,NewEst,StLen,doClip,StLim)

      integer nPoint
      real Est(nPoint),NewEst(nPoint)
      real StLen,StLim
      logical doClip
c-----------------------------------------------------------------------
c  Take the final step!
c-----------------------------------------------------------------------
      integer i
      real Stepd
c-----------------------------------------------------------------------
      if (doClip) then
        do i = 1, nPoint
          Stepd = StLen*max(NewEst(i),-0.9*Est(i)/StLim)
          NewEst(i) = max(Est(i) + Stepd,1e-30)
        enddo
      else
        do i = 1, nPoint
          NewEst(i) = Est(i) + StLen*NewEst(i)
        enddo
      endif
      end

c***********************************************************************

      subroutine ChekStep(nPoint,OldEst,Est,Def,Resa,Wta,Resb,Wtb,
     *        TRms2,dosingle,measure,Alpha,Beta,Qa,Qb,J0)

      integer nPoint,Measure
      real OldEst(nPoint),Est(nPoint),Def(nPoint)
      real Resa(nPoint),Wta(nPoint),Resb(nPoint),Wtb(nPoint)
      real Alpha,Beta,Qa,Qb,J0,TRms2
      logical dosingle
c-----------------------------------------------------------------------
c  Determine some things about this place we are thinking of moving
c  to. Is it a good neighbourhood? Will my kids be safe here?
c
c  Inputs:
c    nPoint     Size of the region being deconvolved.
c    Alpha,Beta Lagrangian multipliers.
c    Def        Default image.
c    Wt
c    Q          Pixels/beam.
c    Res        The residual.
c    Est        The estimate.
c    OldEst     The old estimate.
c    measure    Determines the entropy measure.
c
c  Output:
c    J0         Some useful (??) statistic.
c-----------------------------------------------------------------------
      integer run
      parameter (run=1024)
      integer n,l,ltot
      real GradJ,Step
      real dH(run),d2H(run)
c-----------------------------------------------------------------------
      J0 = 0.0
      n = 0
      do while (n.lt.nPoint)
        ltot = min(nPoint-n,run)
        call EntFunc(measure,ltot,Est(n+1),Def(n+1),dH,d2H)
        do l = 1, ltot
          if (dosingle) then
            GradJ = dH(l) - 2*Qa*Alpha*Resa(n+l)*Wta(n+l)
     *                    - 2*Qb*Beta *Resb(n+l)*Wtb(n+l)/TRms2
          else
            GradJ = dH(l) - 2.0*Alpha*Qa*Resa(n+l)*Wta(n+l) - Beta
          endif
          Step = Est(n+l) - OldEst(n+l)
          J0 = J0 + GradJ*Step
        enddo
        n = n + ltot
      enddo

      end

c***********************************************************************

      subroutine GetInfo(nPoint,Est,Resa,Wta,Resb,Wtb,TRms2,Measure,
     *  Def,dosingle,Alpha,Beta,Qa,Qb,
     *  GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ,
     *  GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb)

      integer nPoint
      real Resa(nPoint),Wta(nPoint),Resb(nPoint),Wtb(nPoint)
      real Est(nPoint),Def(nPoint)
      integer Measure
      real Alpha,Beta,Qa,Qb,TRms2
      real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
      real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rmsa,Rmsb
      logical dosingle
c-----------------------------------------------------------------------
c  Get information on the current state of play.
c
c  Inputs:
c    nPoint     Number of points in the input.
c    Res,Est    The Residuals and Estimate respectively.
c    measure    Determines the entropy measure used.
c    Def        The default image.
c    Wt         1/Sigma**2 for the mosaic.
c    Alpha
c    Beta
c    Q
c
c  Outputs:
c    GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
c    GradHH,GradJJ,NomGrd,Immax,Immin,Flux,Rms
c-----------------------------------------------------------------------
      integer Run
      parameter (Run=1024)
      integer n,l,ltot
      real Diag,GradE,GradH,GradF,temp
      real dH(Run),d2H(Run)
c-----------------------------------------------------------------------
      GradEE = 0.0
      GradEF = 0.0
      GradEH = 0.0
      GradFF = 0.0
      GradFH = 0.0
      GradHH = 0.0
      Rmsa   = 0.0
      Rmsb   = 0.0
      Flux   = 0.0
      Immin = Est(1)
      Immax = Immin
      temp = 0

      n = 0
      do while (n.lt.nPoint)
        ltot = min(Run,nPoint-n)
        call EntFunc(measure,ltot,Est(n+1),Def(n+1),dH,d2H)
        do l = 1, ltot
          GradE = 2.0 * Qa * Resa(n+l) * Wta(n+l)
          Rmsa = Rmsa + Wta(n+l) * Resa(n+l)**2
          if (dosingle) then
            Rmsb = Rmsb + Resb(n+l)**2/TRms2
            GradF = 2.0 * Qb * Resb(n+l) * Wtb(n+l)/TRms2
            Diag = 1.0/(2.0*Alpha*Qa*Qa*Wta(n+l) +
     *                  2.0*Beta* Qb*Qb*Wtb(n+l)*Wtb(n+l)/TRms2-d2H(l))
          else
            GradF = 1
            Diag = 1.0/(2.0*Alpha*Qa*Qa*Wta(n+l) - d2H(l))
          endif
          GradH = dH(l)
          GradEE = GradEE + GradE*Diag*GradE
          GradEF = GradEF + GradE*Diag*GradF
          GradEH = GradEH + GradE*Diag*GradH
          GradFF = GradFF + GradF*Diag*GradF
          GradFH = GradFH + GradF*Diag*GradH
          GradHH = GradHH + GradH*Diag*GradH
          temp = temp + Diag
          Flux = Flux + Est(n+l)
          Immin = min(Immin,Est(n+l))
          Immax = max(Immax,Est(n+l))
        enddo
        n = n + ltot
      enddo
c
c  Finish up various variables.
c
      Rmsa = sqrt(Rmsa/real(nPoint))
      Rmsb = sqrt(Rmsb/real(nPoint))
      GradEJ = GradEH - Alpha*GradEE - Beta*GradEF
      GradFJ = GradFH - Alpha*GradEF - Beta*GradFF
      GradJJ = GradHH + Alpha*Alpha*GradEE + Beta*Beta*GradFF
     *        - 2.0*Alpha*GradEH - 2.0*Beta*GradFH
     *        + 2.0*Alpha*Beta*GradEF
      Grad11 = GradHH + alpha**2*GradEE + beta**2*GradFF
      if (Grad11.le.0) Grad11 = temp

      end

c***********************************************************************

      subroutine EntFunc(measure,n,Est,Default,dH,d2H)

      integer n,measure
      real Default(n),Est(n),dH(n),d2H(n)
c-----------------------------------------------------------------------
c  Routine to find the first and second derivatives of the desired
c  entropy measure. These are:
c    Gull, Daniel and Skilling entropy function:
c      H   = - SUM b*log(b/em)
c      dH  = -log(b/m)
c      d2H = -1/b
c
c    Cornwell's "UTESS" measure:
c      H   = - SUM log(cosh(b/m))
c      dH  = -tanh(b/m)/m
c      d2H = -(sech(b/m)/m)**2
c          = dH**2 - 1/m**2
c
c  Inputs:
c    measure    The entropy measure desired, either gull or cornwell.
c    n          Number of elements to find derivative info for.
c    Est        Brightness estimate, b.
c    Default    Default image.
c
c  Outputs:
c    dH         First derivative of the entropy function.
c    d2H        Second derivative.
c-----------------------------------------------------------------------
      integer gull,cornwell
      parameter (gull=1,cornwell=2)
      integer i
      real def
c-----------------------------------------------------------------------
c
c  The Gull, Daniel and Skilling measure.
c
      if (measure.eq.gull) then
        do i = 1, n
          dH(i) = -log(Est(i)/Default(i))
          d2H(i) = -1.0/Est(i)
        enddo
c
c  Cornwells UTESS measure.
c
      else
        do i = 1, n
          def = 1/Default(i)
          dH(i) = -def * tanh(Est(i)*def)
          d2H(i) = dH(i)*dH(i) - def*def
        enddo
      endif

      end

c***********************************************************************

      subroutine Diff(Est,Map,Res,nPoint,Run,nRun)

      integer nPoint,nRun,Run(3,nRun)
      real Est(nPoint),Map(nPoint),Res(nPoint)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      call mcCnvlR(Est,Run,nRun,Res)

      do i = 1, nPoint
        Res(i) = Res(i) - Map(i)
      enddo

      end

c***********************************************************************

      subroutine mkHead(lMap,lOut,blc,trc,version)

      integer   lMap, lOut, blc(3), trc(3)
      character version*72
c-----------------------------------------------------------------------
c  Write a header for the output file.
c
c  Input:
c    version    Program version ID.
c    lMap       The handle of the input map.
c    lOut       The handle of the output estimate.
c    blc        Blc of the bounding region.
c    trc        Trc of the bounding region.
c-----------------------------------------------------------------------
      integer   iax, lblc, ltrc
      double precision crpix
      character cin*1, line*72, txtblc*32, txttrc*32

      character itoaf*8
      external  itoaf
c-----------------------------------------------------------------------
c     Start by making a verbatim copy of the input header.
      call headcp(lMap, lOut, 0, 0, 0, 0)

c     Update parameters that may have changed.
      do iax = 1, 3
        if (blc(iax).ne.1) then
          cin = itoaf(iax)
          call rdhdd(lMap, 'crpix'//cin, crpix, 1d0)
          crpix = crpix - dble(blc(iax) + 1)
          call wrhdd(lOut, 'crpix'//cin, crpix)
        endif
      enddo

      call wrhda(lOut, 'bunit', 'JY/PIXEL')

c     Update history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'MOSMEM: Miriad ' // version)
      call hisinput(lOut, 'MOSMEM')

      call mitoaf(blc, 3, txtblc, lblc)
      call mitoaf(trc, 3, txttrc, ltrc)
      line = 'MOSMEM: Bounding region is Blc=(' // txtblc(1:lblc) //
     *                                '),Trc=(' // txttrc(1:ltrc) // ')'
      call hiswrite(lOut,line)
      call hisclose(lOut)

      end

c***********************************************************************

      subroutine NewAlpB(Alpha,Beta,De,Df,dotwo,GradEE,GradEF,
     *        GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)

      real Alpha,Beta,De,Df,GradEE,GradEF
      real GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH
      logical dotwo
c-----------------------------------------------------------------------
c  Determine new values for alpha and beta.
c-----------------------------------------------------------------------
      real tol1,tol2
      parameter (tol1=0.1,tol2=0.05)

      real Denom,Dalp,Dbet,l,Alpha1,Alpha2,Beta1,Beta2,b2m4ac
c-----------------------------------------------------------------------
c
c  Check if things are doing poorly. If so, just aim at reducing the
c  gradient.
c
      l = abs(GradJJ/Grad11)
      if (Alpha.le.0) l = 0.0

      if (dotwo) then
        Denom = 1.0/(GradEE*GradFF - GradEF*GradEF)
        Alpha1 = (GradFF*GradEH - GradEF*GradFH) * Denom
        Beta1  = (GradEE*GradFH - GradEF*GradEH) * Denom
      else
        Alpha1 = GradEH / GradEE
        Beta1  = 0.0
      endif

      if (dotwo) then
        Denom = 1.0/(GradEE*GradFF - GradEF*GradEF)
        Dalp = (GradFF*(De+GradEJ) - GradEF*(Df+GradFJ)) * Denom
        Dbet =-(GradEF*(De+GradEJ) - GradEE*(Df+GradFJ)) * Denom
      else
        Denom = 1.0/GradEE
        Dalp = (De+GradEJ) * Denom
        Dbet = 0.0
      endif

      b2m4ac = GradEJ*GradEJ - (GradJJ-tol1*Grad11)*GradEE
      if (b2m4ac.gt.0) then
        b2m4ac = sqrt(b2m4ac)
        Dalp = max((GradEJ - b2m4ac)/GradEE,
     *         min((GradEJ + b2m4ac)/GradEE,Dalp))
      else
        Dalp = 0
      endif

      b2m4ac = GradFJ*GradFJ - (GradJJ-tol1*Grad11)*GradFF
      if (b2m4ac.gt.0.0) then
        b2m4ac = sqrt(b2m4ac)
        Dbet = max((GradFJ - b2m4ac)/GradFF,
     *         min((GradFJ + b2m4ac)/GradFF,Dbet))
      else
        Dbet = 0.0
      endif

      Alpha2 = Alpha+ Dalp
      Beta2  = Beta + Dbet

      if (l.ge.tol2 .or. Alpha2.le.0.0) then
        Alpha = max(Alpha1,0.0)
      else
        Alpha = max(Alpha2,0.0)
      endif

      if (l.ge.tol2 .or. Beta2.le.0.0) then
        Beta = max(Beta1,0.0)
      else
        Beta = max(Beta2,0.0)
      endif

      end

c***********************************************************************

      subroutine SDConIni(Cnvl,lBeam,n1,n2,ic,jc,nx,ny)

      ptrdiff Cnvl
      integer lBeam,n1,n2,ic,jc,nx,ny
c-----------------------------------------------------------------------
c  Initialise the routines that convolve with the dirty beam.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      integer na,nb
      ptrdiff pData
c-----------------------------------------------------------------------
c
c  Determine the size of the beam that we need to feed to the
c  convolution routines.
c
      na = max(n1,nx+min(n1,nx)-1)
      nb = max(n2,ny+min(n2,ny)-1)

      if (na.eq.n1 .and. nb.eq.n2) then
        call CnvlIniF(Cnvl,lBeam,n1,n2,ic,jc,0.0,' ')
      else if (na.le.2*n1 .and. nb.le.2*n2) then
        call CnvlIniF(Cnvl,lBeam,n1,n2,ic,jc,0.0,'e')
      else
        call memAllop(pData,na*nb,'r')
        call SDConLod(lBeam,n1,n2,memr(pData),na,nb)
        call CnvlIniA(Cnvl,memr(pData),na,nb,ic,jc,0.0,' ')
        call memFrep(pData,na*nb,'r')
      endif

      end

c***********************************************************************

      subroutine SDConLod(lBeam,n1,n2,Data,nx,ny)

      integer lBeam,n1,n2,nx,ny
      real Data(nx,ny)
c-----------------------------------------------------------------------
c  Load a single dish beam, zero padding where necessary.
c-----------------------------------------------------------------------
      integer i,j
c-----------------------------------------------------------------------
      do j = 1, n2
        call xyread(lBeam,j,Data(1,j))
        do i = n1+1, nx
          Data(i,j) = 0.0
        enddo
      enddo

      do j = n2+1, ny
        do i = 1, nx
          Data(i,j) = 0.0
        enddo
      enddo

      end

c***********************************************************************

      subroutine SDConDif(cnvl,Est,Map,fac,Res,Wt,nPoint,Run,nRun,
     *                                        nx,ny)

      integer nPoint,nRun,Run(3,nRun),nx,ny
      ptrdiff cnvl
      real Est(nPoint),Map(nPoint),Res(nPoint),Wt(nPoint),fac
c-----------------------------------------------------------------------
c  Determine the convolution of the estimate with the single dish beam.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nPoint
        Res(i) = Est(i) * Wt(i)
      enddo

      call CnvlR(cnvl,res,nx,ny,Run,nRun,res,'c')

      do i = 1, nPoint
        Res(i) = Res(i) - fac*Map(i)
      enddo

      end

c***********************************************************************

      subroutine SDConWt(Gain,Wt,nPoint)

      integer nPoint
      real Gain(nPoint),Wt(nPoint)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nPoint
        if (Gain(i).gt.0.0) then
          Wt(i) = 1.0 / Gain(i)
        else
          Wt(i) = 0.0
        endif
      enddo

      end

c***********************************************************************

      subroutine NewFac(nPoint,Res,Map,fac,TRms2)

      integer nPoint
      real TRms2,fac,Res(nPoint),Map(nPoint)
c-----------------------------------------------------------------------
      real dirty,nFac
      double precision SumMM,SumDM
      integer i
c-----------------------------------------------------------------------
      SumMM = 0d0
      SumDM = 0d0
      do i = 1, nPoint
        dirty = Res(i) + fac*Map(i)
        SumMM = SumMM + Map(i)*Map(i)
        SumDM = SumDM + dirty*Map(i)
      enddo

      nfac = SumDM/(SumMM - nPoint*TRms2)

      do i = 1, nPoint
        Res(i) = Res(i) + (fac - nfac)*Map(i)
      enddo

      fac = nfac

      end

c***********************************************************************

      subroutine Zeroit(n,array)

      integer n
      real array(n)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        array(i) = 0.0
      enddo

      end
