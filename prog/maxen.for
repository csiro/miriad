      program maxen

c= maxen - Maximum Entropy deconvolution
c& rjs mchw
c: deconvolution
c+
c       MAXEN is a MIRIAD task that performs a maximum entropy
c       deconvolution algorithm on a cube.
c@ map
c       The input dirty map, which should have units of Jy/beam.
c       No default.
c@ beam
c       The input dirty beam. No default
c@ model
c       An initial estimate of the deconvolved image.  For point
c       sources, giving a good initial model may help convergence.  In
c       principle, this only helps convergence, but should not affect
c       the final solution.  The model could be the output from a
c       previous run of MAXEN or any other deconvolution task.  It must
c       have flux units of Jy/pixel.  The default is a flat estimate,
c       with the correct flux.
c@ default
c       The default image.  This is the image that the final solution
c       will tend towards.  The final result will be influenced by this
c       default if the constrains that the data put on the solution are
c       weak.  The default is a flat estimate, with the correct flux.
c@ out
c       The name of the output map.  The units of the output will be
c       Jy/pixel.  It can be input to RESTOR, MAXEN (as a model, to
c       continue the deconvolution process), or to SELFCAL (for self-
c       calibrating visibility data).
c@ niters
c       The maximum number of iterations. The default is 20.
c@ region
c       This specifies the region to be deconvolved.  See the User
c       Manual for instructions on how to specify this.  The default is
c       the largest centered region that it is safe to deconvolve.
c@ measure
c       The entropy measure to be used, either "gull" (-p*log(p/e)) or
c       "cornwell" (-log(cosh(p)) -- also called the maximum emptyness
c       criteria).
c@ tol
c       Tolerance of solution. There is no need to change this from the
c       default of 0.01.
c@ q
c       An estimate of the number of points per beam. MAXEN can usually
c       come up with a pretty good, image dependent estimate.
c@ rms
c       The rms noise, in Jy/beam, in the dirty map. No default. The
c       convergence and behavior of MAXEN depends strongly on the value
c       of this parameter.
c@ flux
c       The flux of the source. If FLUX is given as a positive
c       number, then the resultant image is constrained to have this
c       flux. If FLUX is negative, then abs(FLUX) is used as an estimate
c       of the source flux only and the actual flux is allowed to vary.
c       If FLUX=0, then MAXEN makes a guess at the source flux.  Default
c       is FLUX=0, though you should give MAXEN more information if at
c       all possible.  On the other hand, a bad value here can cause
c       MAXEN to blow up.
c@ options
c       Task enrichment parameters.  Several can be given, separated by
c       commas.  Minimum match is used. Possible values are:
c         quiet      Do not give any messages during iterations.  The
c                    default is to give a one line message at each
c                    iteration.
c         verbose    Give lots of messages during the iterations.  The
c                    default is to give a one line message at each
c                    iteration.
c         asym       The beam is asymmetric.  By default MAXEN assumes
c                    the beam has 180 deg rotation symmetry, which is
c                    the norm for beams in radio-astronomy.
c         pad        Double the beam size by padding it with zeros.
c                    This will give better stability if you are daring
c                    enough to deconvolve more than the inner quarter of
c                    the dirty image.
c
c$Id: maxen.for,v 1.5 2011/10/31 00:37:30 wie017 Exp $
c--
c  History:
c    rjs   nov88 - Original version.
c    rjs  9jun89 - Included Diff in source. Changed call sequences to
c                  GetPlane and PutPlane. Improved history. Removed sign
c                  change fudge.
c    rjs 13mar90 - Copied linetype header values to the output file.
c                  Added version thingo.
c    rjs 22mar90 - Cosmetic changes to the format statements.
c    rjs 12apr90 - Fixed bug which caused the old history to be
c                  overwritten.
c    rjs 30apr90 - Changed call sequence to BoxInput.
c   mchw  7may90 - Fixed call sequence to BoxInput.
c   mchw 25oct90 - Added default image and message level.
c                  Fixed bug in output map size in PutPlane.
c   mchw 14nov90 - Set minimum default level. Added pbfwhm to header.
c   mchw 03jan91 - Increased Maxboxes to handle more complex region.
c   rjs  08apr91 - Used Mem routines, to avoid the need to declare large
c                  static arrays.
c   rjs  19apr91 - Fixed bug in EntFunc in determining d2H (apparently
c                  introduced on 25oct90). Tidied up messages somewhat.
c   rjs  19apr91 - More minor optimisations.
c   mjs  08aug91 - correct BeamChar typo: jmax=max(etc) to jmax=min(etc)
c   rjs  14oct91 - Bug in dealing with non-square beams.
c   rjs  24may92 - Fiddles to steps in alpha and stlen to make
c                  convergence more robust (but sometimes slower).
c   nebk 25nov92 - Copy btype to output
c   mchw 07jan93 - Copy across crpix3.
c   mjs  17feb93 - minor doc mod only (RESTORE -> RESTOR).
c   rjs   5mar93 - History standardisation. Use cnvl routines. Add extra
c                  options.
c   rjs  27nov94 - Significant changes and bugfixes to make it more
c                  robust.
c   rjs  10aug95 - New routine to change alpha and beta.
c   rjs  120ct95 - Support model and default being different size to
c                  selected region.
c   rjs  18Oct05 - Handle higher axes somewhat better.
c   rjs  27oct95 - Increased max length of filenames.
c   rjs  18mar96 - Increase MAXBOXES.
c   rjs  29jan97 - Change default region of interest.
c   rjs  10mar97 - Default region is all channels.
c   rjs  25mar97 - Check whether data are selected for a given plane.
c   rjs  24jun97 - Correct check for alignment mismatch.
c   rjs  02jul97 - cellscal change.
c   rjs  23jul97 - Added pbtype.
c   rjs  10feb99 - Zero initial estimate for measure=cornwell to 0.
c-----------------------------------------------------------------------
      include 'maxnax.h'
      include 'maxdim.h'

      integer    MAXRUN, MAXBOXES
      parameter (MAXRUN=3*MAXDIM, MAXBOXES=2048)

      integer    QUIET, NORMAL, VERBOSE
      parameter (QUIET=0, NORMAL=1, VERBOSE=2)

      integer    GULL, CORNWELL
      parameter (GULL=1, CORNWELL=2)

      logical   asym, converge, doflux, pad, positive
      integer   blc(3), boxes(MAXBOXES), i, icentre, imax, imin,
     *          jcentre, jmax, jmin, k, kmax, kmin, lBeam, lDef, lMap,
     *          lModel, lOut, maxMap, maxniter, measure, message, n1,
     *          n2, nBeam(2), nDef(3), nMap(3), nModel(3), nOut(MAXNAX),
     *          nPoint, nRun, naxis, niter, nx, ny, run(3,MAXRUN),
     *          trc(3), xdoff, xmax, xmin, xmoff, ydoff, ymax, ymin,
     *          ymoff, zdoff, zmoff
      real      Alpha, Beta, ClipLev, De, Df, Flux, Grad11, GradEE,
     *          GradEF, GradEH, GradEJ, GradFF, GradFH, GradFJ, GradHH,
     *          GradJJ, Immax, Immin, J0, J1, OStLen1, OStLen2, Q, Qest,
     *          Rms, StLen1, StLen2, StLim, TFlux, TRms, Tol

      character BeamNam*64, DefNam*64, entropy*8, flags*8, line*72,
     *          MapNam*64, messlev*8, ModelNam*64, OutNam*64, version*72

      ptrdiff   pBem,pMap,pEst,pDef,pRes,pNewEst,pNewRes
      real Data(MaxBuf)
      common Data

      integer   ismax
      character itoaf*4, versan*80
      external  ismax, itoaf, versan
c-----------------------------------------------------------------------
      version = versan('maxen',
     *                 '$Revision: 1.5 $',
     *                 '$Date: 2011/10/31 00:37:30 $')
c
c  Get the input parameters.
c
      call keyini
      call keya('map',MapNam,' ')
      call keya('beam',BeamNam,' ')
      call keya('model',ModelNam,' ')
      call keya('default',DefNam,' ')
      call keya('out',OutNam,' ')
      call keyr('tol',Tol,0.01)
      call keyi('niters',maxniter,20)
      call keyr('q',Q,0.0)
      call keyr('rms',TRms,0.0)
      call keyr('flux',TFlux,0.0)
      call BoxInput('region',MapNam,Boxes,MAXBOXES)
      call GetMeas(entropy)
      call GetOpt(messlev,asym,pad)
      call keyfin
c
c  Check everything makes sense.
c
      doflux = TFlux.gt.0
      TFlux = abs(TFlux)
      if (Trms.le.0.0) call bug('f','RMS is not positive')
      if (maxniter.lt.0) call bug('f','NITERS has bad value')
      if (MapNam.eq.' ' .or. BeamNam.eq.' ' .or. OutNam.eq.' ')
     *  call bug('f','A file name was missing from the parameters')
      if (Tol.le.0.0)
     *  call bug('f','The TOL parameter must be positive valued')

      if (messlev.eq.'quiet') then
        message = QUIET
      else if (messlev.eq.'normal') then
        message = NORMAL
      else if (messlev.eq.'verbose') then
        message = VERBOSE
      endif

      if (entropy.eq.'gull') then
        measure = GULL
        positive = .true.
      else if (entropy.eq.'cornwell') then
        measure = CORNWELL
        positive = .false.
      endif
c
c  Open the beam, and get some info about it.
c
      call xyopen(lBeam,BeamNam,'old',2,nBeam)
      n1 = nBeam(1)
      n2 = nBeam(2)
      if (max(n1,n2).gt.MAXDIM) call bug('f','Beam too big')
      call BeamChar(lBeam,n1,n2,Qest,icentre,jcentre)
      write(line,'(a,1pg8.1)')'An estimate of Q is',Qest
      call output(line)
      if (Q.gt.0.0) then
        write(line,'(a,1pg8.1)')
     *                'Using user given pixels per beam of',Q
        call output(line)
      else
        Q = Qest
      endif

      flags = ' '
      if (.not.asym) flags(1:1) = 's'
      if (pad)       flags(2:2) = 'e'
      call CnvlIniF(pBem,lBeam,n1,n2,icentre,jcentre,0.0,flags)
c
c  Open the input map.
c
      call xyopen(lMap,MapNam,'old',3,nMap)
      if (max(nMap(1),nMap(2)).gt.MAXDIM) call bug('f','Map too big')
      call rdhdi(lMap,'naxis',naxis,3)
      naxis = min(naxis,MAXNAX)
      call defregio(boxes,nMap,nBeam,icentre,jcentre)
      call BoxMask(lMap,boxes,MAXBOXES)
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
      do i = 4, naxis
        nOut(i) = 1
      enddo
      if (nOut(1).gt.n1 .or. nOut(2).gt.n2)
     *  call bug('f','Region of map to deconvolve is too big')
      if (2*nOut(1)-1.gt.n1 .or. 2*nOut(2)-1.gt.n2)
     *  call bug('w','Size of region of map to deconvolve is unsafe')
c
c  Allocate arrays to hold everything.
c
      MaxMap = nOut(1)*nOut(2)
      call MemAllop(pMap,MaxMap,'r')
      call MemAllop(pEst,MaxMap,'r')
      call MemAllop(pDef,MaxMap,'r')
      call MemAllop(pRes,MaxMap,'r')
      call MemAllop(pNewEst,MaxMap,'r')
      call MemAllop(pNewRes,MaxMap,'r')
c
c  Open the model if needed, and check that is is the same size as the
c  output.
c
      if (ModelNam.ne.' ') then
        call xyopen(lModel,ModelNam,'old',3,nModel)
        call AlignIni(lModel,lMap,nMap(1),nMap(2),nMap(3),
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
      if (DefNam.ne.' ') then
        call xyopen(lDef,DefNam,'old',3,nDef)
        call AlignIni(lDef,lMap,nMap(1),nMap(2),nMap(3),
     *                                        xdoff,ydoff,zdoff)
      endif
c
c  Open up the output.
c
      call xyopen(lOut,OutNam,'new',naxis,nOut)
c
c  Loop.
c
      do k = kmin, kmax
        if (kmin.ne.kmax) call output('Plane: '//itoaf(k))

        call BoxRuns(1,k,'r',boxes,Run,MAXRUN,nRun,
     *                                xmin,xmax,ymin,ymax)
        nx = xmax - xmin + 1
        ny = ymax - ymin + 1
c
c  Get the Map.
c
        call xysetpl(lMap,1,k)
        call GetPlane(lMap,Run,nRun,xmin-1,ymin-1,nMap(1),nMap(2),
     *                        Data(pMap),MaxMap,nPoint)
        if (nPoint.gt.0) then
c
c  Get the Default map and Clip level.
c
        if (TFlux.eq.0) TFlux = TRms*nPoint/Q

        if (DefNam.eq.' ') then
          ClipLev = 0.01 * TFlux/nPoint
          call Assign(TFlux/nPoint,Data(pDef),nPoint)
        else
          call AlignGet(lDef,Run,nRun,k,xmin+xdoff-1,ymin+ydoff+1,
     *        zdoff,nDef(1),nDef(2),nDef(3),
     *        Data(pDef),MaxMap,nPoint)
          Imax = Ismax(npoint,Data(pDef),1)
          ClipLev = 0.01 * abs(Data(pDef+Imax-1))
          call ClipIt(0.1*ClipLev,Data(pDef),nPoint)
        endif
c
c  Get the Estimate and Residual. Also get information about the
c  current situation.
c
        if (ModelNam.eq.' ') then
          if (positive) then
            call Copy(nPoint,Data(pDef),Data(pEst))
          else
            call Zeroit(nPoint,Data(pEst))
          endif
        else
          call AlignGet(lModel,Run,nRun,k,xmin+xmoff-1,ymin+ymoff+1,
     *        zmoff,nModel(1),nModel(2),nModel(3),
     *        Data(pEst),MaxMap,nPoint)
          if (ClipLev.gt.0) call ClipIt(ClipLev,Data(pEst),nPoint)
        endif

        call Diff(pBem,Data(pEst),Data(pMap),Data(pRes),
     *    nPoint,nx,ny,Run,nRun)

        call GetInfo(nPoint,Data(pEst),Data(pRes),measure,Data(pDef),
     *    Alpha,Beta,Q,GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,
     *    GradFJ,GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms)
c
c  Put the user to sleep with lots of meaningful messages.
c
        if (message.eq.VERBOSE) then
          call output('Initialising ...')
          write(line,10) Alpha,Beta,Q
  10      format('  Alpha =',1pe12.3,' Beta  =',1pe12.3,
     *        ' Q       =',1pe12.3)
          call output(line)
          write(line,11) Immin,Immax
  11      format('  Immin =',1pe12.3,' Immax =',1pe12.3)
          call output(line)
          write(line,12) Rms,Flux,GradJJ/Grad11
  12      format('  Rms   =',1pe12.3,' Flux  =',1pe12.3,
     *        ' NormGrd =',1pe12.3)
          call output(line)
        endif
c
c-----------------------------------------------------------------------
c  Now start to iterate at long last.
c
        OStLen2 = 0
        OStLen1 = 0
        Converge = .false.
        Niter = 0
        do while (.not.converge .and. Niter.lt.MaxNiter)
          Niter = Niter + 1
c
c  Update Alpha and Beta.
c
          De = nPoint*(Rms*Rms - TRms*TRms)
          Df = Flux - TFlux
          call NewAlpB(Alpha,Beta,De,Df,doflux,GradEE,GradEF,
     *          GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)
c
c  Calculate the next step to take.
c
          call CalStep(nPoint,Data(pEst),Data(pRes),Data(pNewEst),
     *          measure,Data(pDef),Alpha,Beta,Q,J0)
c
c  Determine the max step length, and the initial step length.  Also
c  correct the value of GradJ.Step for the shorter length step.
c
          StLim = 1
          if (GradJJ.gt.0) StLim = min(1.4,0.15*Grad11/GradJJ)
          StLen1 = min(0.5*(1+OStLen1),StLim)
          OStLen1 = StLen1
          J0 = J0 * StLen1
c
c  Determine the correct Clip Level (to prevent the estimate going
c  negative, if this is not allowed).
c
          if (positive) ClipLev = min(ClipLev,max(0.1*Immin,1e-6*Immax))
c
c  Take the plunge.
c
          call TakeStep(nPoint,Data(pEst),Data(pNewEst),
     *                                  StLen1,ClipLev,StLim)
c
c  Convolve the estimate with the beam and subtract the map.
c
          call Diff(pBem,Data(pNewEst),Data(pMap),Data(pNewRes),
     *      nPoint,nx,ny,Run,nRun)
c
c  Now work out what was really the best step length.  Make sure that
c  it's no longer than the step length we previously determined was the
c  max length overall.
c
          call ChekStep(nPoint,Data(pEst),Data(pNewEst),Data(pNewRes),
     *                  measure,Data(pDef),Alpha,Beta,Q,J1)
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
c  be near 1 on the first few iterations, where StLim, rather than
c  anything else, determines the step length. If StLen2 is near 1,
c  just swap the pointers around rather than do any real work.
c
          if (abs(StLen2-1.0).gt.0.05) then
            call IntStep(nPoint,Data(pEst),Data(pNewEst),StLen2)
            call IntStep(nPoint,Data(pRes),Data(pNewRes),StLen2)
          else
            StLen2 = 1
            call Swap(pEst,pNewEst)
            call Swap(pRes,pNewRes)
          endif
c
c  Calculate a new estimate for Q using the magic formula which seems
c  to work. Only recalculate when we did not clip back the step length
c  excessively. That is recalculate if StLen1 is close to 1.
c
          if (abs(StLen1-1.0).lt.0.05)
     *      Q = Q * sqrt((1.0/max(0.5,min(2.0,StLen1*StLen2))+3.0)/4.0)
c
c  Get new info on the current state of play.
c
            call GetInfo(nPoint, Data(pEst), Data(pRes), measure,
     *        Data(pDef), Alpha, Beta, Q, GradEE, GradEF, GradEH,
     *        GradEJ, GradFF, GradFH, GradFJ, GradHH, GradJJ, Grad11,
     *        Immax, Immin, Flux, Rms)
c
c  Reawaken the user with more crap to let him/her ponder over what
c  could possibly be going wrong. Give him/her as much as possible to
c  ponder over.
c
            if (message.eq.VERBOSE) then
              call output('Iteration '//itoaf(niter))
              write(line,20) Alpha,Beta,Q
              call output(line)
              write(line,21) Immin,Immax
              call output(line)
              write(line,22) Rms,Flux,GradJJ/Grad11
              call output(line)
              write(line,23) StLim,StLen1,StLen2
              call output(line)
            else if (message.eq.NORMAL) then
              write(line,24) Niter,Rms,Flux,GradJJ/Grad11
              call output(line)
            endif

  20        format('  Alpha =',1pe12.3,' Beta  =',1pe12.3,
     *            ' Q       =',1pe12.3)
  21        format('  Immin =',1pe12.3,' Immax =',1pe12.3)
  22        format('  Rms   =',1pe12.3,' Flux  =',1pe12.3,
     *            ' NormGrd =',1pe12.3)
  23        format('  StLim =',1pe12.3,' StLen1=',1pe12.3,
     *            ' StLen2  =',1pe12.3)
  24        format(' Iter =',i3,' Rms =',1pe10.3,' Flux =',1pe10.3,
     *            ' NormGrd =',1pe10.3)
c
c  Check for convergence.
c
            converge = Rms-TRms.lt.0.05*TRms .and.
     *                 GradJJ/Grad11.lt.Tol  .and.
     *                 (abs(Flux-TFlux).lt.0.05*TFlux .or. .not.doflux)
          enddo
        endif
c-----------------------------------------------------------------------
c
c  We have finished processing this plane. More info to the user!
c
        if (nPoint.eq.0) then
          call output('No data selected for this plane')
        else if (converge) then
          call output('MAXEN has converged ... finishing up now')
        else
          call output('Failed to converge in NITERS iterations')
        endif
c
c  Write out this plane.
c
        call xysetpl(lOut,1,k-kmin+1)
        call PutPlane(lOut,Run,nRun,xmin-imin,ymin-jmin,
     *                        nOut(1),nOut(2),Data(pEst),nPoint)
      enddo
c
c  Construct a header for the output file, and give some history
c  information.
c
      call Header(lMap,lOut,blc,trc,version,niter)
c
c  Close up the files. Ready to go home.
c
      call xyclose(lMap)
      call xyclose(lBeam)
      if (ModelNam.ne.' ') call xyclose(lModel)
      call xyclose(lOut)
c
c  Thats all folks.
c
      end

c***********************************************************************

      subroutine GetOpt(messlev,asym,pad)

      character messlev*(*)
      logical asym,pad

c  Get extra processing options.
c
c  Output:
c    messlev    Possibles values: 'quiet', 'normal', 'verbose'.
c    asym       Beam is asymmetric.
c    pad        Double beam size.
c-----------------------------------------------------------------------
      integer NOPT
      parameter (NOPT=4)
      logical present(NOPT)
      character opts(NOPT)*8
      data opts/'quiet   ','verbose ','asym    ','pad     '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPT)
      messlev = 'normal'
      if (present(1)) messlev = 'quiet'
      if (present(2)) messlev = 'verbose'
      if (present(1) .and. present(2))
     *  call bug('f','Cannot mix quiet and verbose options')
      asym = present(3)
      pad = present(4)

      end

c***********************************************************************

      subroutine GetMeas(entropy)

      character entropy*(*)

c  Determine the entropy measure to be used.
c
c  Output:
c    entropy    The entropy measure.
c-----------------------------------------------------------------------
      integer NOPT
      parameter (NOPT=2)
      integer nout
      character opts(NOPT)*8
      data opts/'gull    ','cornwell'/
c-----------------------------------------------------------------------
      call keymatch('measure',NOPT,opts,1,entropy,nout)
      if (nout.eq.0) entropy = opts(1)
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
c
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        To(i) = From(i)
      enddo

      end

c***********************************************************************

      subroutine Assign(def,Default,nPoint)

      integer nPoint
      real def,Default(nPoint)

c  Set up the default image.
c
c  Input:
c    def
c    nPoint
c  Output:
c    Default    The default image.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nPoint
        Default(i) = def
      enddo

      end

c***********************************************************************

      subroutine ClipIt(clip,Default,nPoint)

      integer nPoint
      real clip,Default(nPoint)

c  Set up the minimum of the default image.
c
c  Input:
c    clip
c    nPoint
c  Input/Output:
c    Default    The default image.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nPoint
        Default(i) = max(clip,Default(i))
      enddo

      end

c***********************************************************************

      subroutine BeamChar(lBeam,n1,n2,Qest,icentre,jcentre)

      integer lBeam,n1,n2,icentre,jcentre
      real Qest

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
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer nP
      parameter (nP=8)
      integer imin,imax,jmin,jmax,i,j
      real Sum,bmax,Data(MAXDIM)

      integer ismax
c-----------------------------------------------------------------------
      imin = max(n1/2+1-nP,1)
      imax = min(n1/2+1+nP,n1)
      jmin = max(n2/2+1-nP,1)
      jmax = min(n2/2+1+nP,n2)

      sum = 0
      bmax = 0
      icentre = 0
      jcentre = 0
      do j = jmin, jmax
        call xyread(lBeam,j,Data)
        do i = imin, imax
          Sum = Sum + Data(i)*Data(i)
        enddo
        i = ismax(n1,Data,1)
        if (Data(i).gt.bmax) then
          icentre = i
          jcentre = j
          bmax = Data(i)
        endif
      enddo

      Qest = sqrt(8*Sum)
      if (abs(1-bmax).gt.0.01) call bug('f','Beam peak is not 1')
      end

c***********************************************************************

      subroutine IntStep(nPoint,Old,New,FracNew)

      integer nPoint
      real FracNew
      real Old(nPoint),New(nPoint)

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

      subroutine ChekStep(nPoint,OldEst,Est,Res,
     *                measure,Default,Alpha,Beta,Q,J0)

      integer nPoint,Measure
      real OldEst(nPoint),Est(nPoint),Res(nPoint)
      real Default(nPoint),Alpha,Beta,Q,J0

c  Determine some things about this place we are thinking of moving
c  to. Is it a good neighbourhood? Will my kids be safe here?
c
c  Inputs:
c    nPoint     Size of the region being deconvolved.
c    Alpha,Beta Lagrangian multipliers.
c    Default    Default image.
c    Q          Pixels/beam.
c    Res        The residual.
c    Est        The estimate.
c    OldEst     The old estimate.
c    measure    Determines the entropy measure.
c
c  Output:
c    J0         Some useful (??) statistic.
c
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
        call EntFunc(measure,ltot,Est(n+1),Default(n+1),dH,d2H)
        do l = 1, ltot
          GradJ = dH(l) - 2.0*Alpha*Q*Res(n+l) - Beta
          Step = Est(n+l) - OldEst(n+l)
          J0 = J0 + GradJ*Step
        enddo
        n = n + ltot
      enddo

      end

c***********************************************************************

      subroutine NewAlpB(Alpha,Beta,De,Df,doflux,GradEE,GradEF,
     *        GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH)

      real Alpha,Beta,De,Df,GradEE,GradEF
      real GradEJ,GradFF,GradFJ,GradJJ,Grad11,GradEH,GradFH
      logical doflux

c  Determine new values for alpha and beta.
c-----------------------------------------------------------------------
      real tol1,tol2
      parameter (tol1=0.1,tol2=0.05)

      real Denom,Dalp,Dbet,l,Alpha1,Alpha2,Beta1,Beta2,b2m4ac
c-----------------------------------------------------------------------
c  Check if things are doing poorly. If so, just aim at reducing the
c  gradient.
c
      l = abs(GradJJ/Grad11)
      if (Alpha.le.0) l = 0

      if (doflux) then
        Denom = 1.0/(GradEE*GradFF - GradEF*GradEF)
        Alpha1 = (GradFF*GradEH - GradEF*GradFH) * Denom
        Beta1  = (GradEE*GradFH - GradEF*GradEH) * Denom
      else
        Alpha1 = GradEH / GradEE
        Beta1  = 0
      endif

      if (doflux) then
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
      if (b2m4ac.gt.0) then
        b2m4ac = sqrt(b2m4ac)
        Dbet = max((GradFJ - b2m4ac)/GradFF,
     *         min((GradFJ + b2m4ac)/GradFF,Dbet))
      else
        Dbet = 0
      endif

      Alpha2 = Alpha+ Dalp
      Beta2  = Beta + Dbet

      if (l.ge.tol2 .or. Alpha2.le.0) then
        Alpha = max(Alpha1,0.0)
      else
        Alpha = max(Alpha2,0.0)
      endif

      if (l.ge.tol2 .or. Beta2.le.0) then
        Beta = max(Beta1,0.0)
      else
        Beta = max(Beta2,0.0)
      endif

      end

c***********************************************************************

      subroutine CalStep(nPoint,Estimate,Residual,Step,
     *        measure,Default,Alpha,Beta,Q,J0)

      integer nPoint,measure
      real Default(nPoint),Alpha,Beta,Q,J0
      real Estimate(nPoint),Residual(nPoint),Step(nPoint)

c  Calculate the step to take next.
c
c  Inputs:
c    nPoint     Number of points.
c    Estimate   Current estimate of the MEM solution.
c    Residual   Current residuals.
c    Default    The default image.
c    measure    Determines the entropy measure used.
c
c  Output:
c    Step       The step to take towards a better estimate.
c    J0         The current value for GradJJ.
c
c-----------------------------------------------------------------------
      integer run
      parameter (run=1024)
      integer n,l,ltot
      real Temp, Diag, GradJ, Stepd
      real dH(run),d2H(run)
c-----------------------------------------------------------------------
      Temp = 2.0*Alpha*Q*Q
      J0 = 0

      n = 0
      do while (n.lt.nPoint)
        ltot = min(nPoint-n,run)
        call EntFunc(measure,ltot,Estimate(n+1),Default(n+1),dH,d2H)
        do l = 1, ltot
          Diag = 1 / (Temp - d2H(l))
          GradJ = dH(l) - 2.0*Q*Alpha*Residual(n+l) - Beta
          Stepd = Diag*GradJ
          J0 = J0 + GradJ*Stepd
          Step(n+l) = Stepd
        enddo
        n = n + ltot
      enddo

      end

c***********************************************************************

      subroutine TakeStep(nPoint,Est,NewEst,StLen,Clip,StLim)

      integer nPoint
      real Est(nPoint),NewEst(nPoint)
      real StLen,Clip,StLim

c  Take the final step!
c
c-----------------------------------------------------------------------
      integer i
      real Stepd
c-----------------------------------------------------------------------
      if (Clip.gt.0) then
        do i = 1, nPoint
          Stepd = StLen*max(NewEst(i),(Clip-Est(i))/StLim)
          NewEst(i) = Est(i) + Stepd
        enddo
      else
        do i = 1, nPoint
          NewEst(i) = Est(i) + StLen*NewEst(i)
        enddo
      endif
      end

c***********************************************************************

      subroutine GetInfo(nPoint,Est,Res,Measure,Default,Alpha,Beta,Q,
     *  GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ,
     *  GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms)

      integer nPoint
      real Res(nPoint),Est(nPoint)
      integer Measure
      real Default(nPoint),Alpha,Beta,Q
      real GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
      real GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms

c  Get information on the current state of play.
c
c  Inputs:
c    nPoint     Number of points in the input.
c    Res,Est    The Residuals and Estimate respectively.
c    measure    Determines the entropy measure used.
c    Default    The default image.
c    Alpha
c    Beta
c    Q
c
c  Outputs:
c    GradEE,GradEF,GradEH,GradEJ,GradFF,GradFH,GradFJ
c    GradHH,GradJJ,Grad11,Immax,Immin,Flux,Rms
c-----------------------------------------------------------------------
      integer Run
      parameter (Run=1024)
      integer n,l,ltot,Imax,Imin
      real Diag,GradE,GradH
      real dH(Run),d2H(Run)

c     Externals to find min and max indices.
      integer Ismin,Ismax
c-----------------------------------------------------------------------
      n = 0

      GradEE = 0.0
      GradEF = 0.0
      GradEH = 0.0
      GradFF = 0.0
      GradFH = 0.0
      GradHH = 0.0
      Rms    = 0.0
      Flux   = 0.0
      do while (n.lt.nPoint)
        ltot = min(Run,nPoint-n)
        call EntFunc(measure,ltot,Est(n+1),Default(n+1),dH,d2H)
        do l = 1, ltot
          GradE = 2.0 * Q * Res(n+l)
          GradH = dH(l)
          Diag = 1.0/(2.0*Alpha*Q*Q - d2H(l))
          GradEE = GradEE + GradE*Diag*GradE
          GradEF = GradEF + GradE*Diag
          GradEH = GradEH + GradE*Diag*GradH
          GradFF = GradFF +       Diag
          GradFH = GradFH +       Diag*GradH
          GradHH = GradHH + GradH*Diag*Gradh
          Flux = Flux + Est(n+l)
          Rms  = Rms + Res(n+l)**2
        enddo
        n = n + ltot
      enddo
c
c  Find the min and max values. Call the library routines to do this,
c  so that we use efficient code!
c
      Imax = Ismax(nPoint,Est,1)
      Imin = Ismin(nPoint,Est,1)
      Immax = Est(Imax)
      Immin = Est(Imin)
c
c  Finish up various variables.
c
      Rms = sqrt(Rms/real(nPoint))
      GradEJ = GradEH - Alpha*GradEE - Beta*GradEF
      GradFJ = GradFH - Alpha*GradEF - Beta*GradFF
      GradJJ = GradHH + Alpha*Alpha*GradEE + Beta*Beta*GradFF
     *        - 2.0*Alpha*GradEH - 2.0*Beta*GradFH
     *        + 2.0*Alpha*Beta*GradEF
      Grad11 = GradHH + Alpha*Alpha*GradEE + Beta*Beta*GradFF
      if (Grad11.le.0) Grad11 = GradFF

      end

c***********************************************************************

      subroutine EntFunc(measure,n,Est,Default,dH,d2H)

      integer n,measure
      real Default(n),Est(n),dH(n),d2H(n)

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
c    measure    The entropy measure desired, either GULL or CORNWELL.
c    n          Number of elements to find derivative info for.
c    Est        Brightness estimate, b.
c    Default    Default image.
c
c  Outputs:
c    dH         First derivative of the entropy function.
c    d2H        Second derivative.
c
c-----------------------------------------------------------------------
      integer    GULL, CORNWELL
      parameter (GULL=1, CORNWELL=2)
      integer i
      real def
c-----------------------------------------------------------------------
c  The Gull, Daniel and Skilling measure.
c
      if (measure.eq.GULL) then
        do i = 1, n
          dH(i) = -log(Est(i)/Default(i))
          d2H(i) = -1.0/Est(i)
        enddo
c
c  Cornwell's UTESS measure.
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

      subroutine Diff(pBem,Estimate,Map,Residual,nPoint,nx,ny,
     *  Run,nRun)

      integer nPoint,nx,ny,nRun,Run(3,nRun)
      ptrdiff pBem
      real Estimate(nPoint),Map(nPoint),Residual(nPoint)
c
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      call CnvlR(pBem,Estimate,nx,ny,Run,nRun,Residual,'c')

      do i = 1, nPoint
        Residual(i) = Residual(i) - Map(i)
      enddo

      end

c***********************************************************************

      subroutine Header(lIn,lOut,blc,trc,version,niter)

      integer   lIn, lOut
      integer   blc(3),trc(3)
      character version*(*)
      integer   niter

c  Write a header for the output file.
c
c  Input:
c    version    Program version ID.
c    lIn        Handle of the input map.
c    lOut       Handle of the output estimate.
c    blc        BLC of the bounding region.
c    trc        TRC of the bounding region.
c    niter      Maximum number of iterations performed.
c
c-----------------------------------------------------------------------
      integer   i, lblc, ltrc
      double precision crpix
      character keyw*8, line*72, txtblc*32, txttrc*32

      character itoaf*8
      external  itoaf
c-----------------------------------------------------------------------
c     Start by copying the header verbatim.
      call headcp(lIn, lOut, 0, 0, 0, 0)

c     Update parameters that have changed.
      do i = 1, 3
        keyw = 'crpix' // itoaf(i)
        call rdhdd(lIn, keyw, crpix, 1d0)
        crpix = crpix - dble(blc(i) - 1)
        call wrhdd(lOut, keyw, crpix)
      enddo

      call wrhda(lOut, 'bunit', 'JY/PIXEL')
      call wrhdi(lOut, 'niters', niter)

c     Write history.
      call hisopen(lOut,'append')
      line = 'MAXEN: Miriad ' // version
      call hiswrite(lOut,line)
      call hisinput(lOut,'MAXEN')

      call mitoaf(blc,3,txtblc,lblc)
      call mitoaf(trc,3,txttrc,ltrc)
      line = 'MAXEN: Bounding region is Blc=('//txtblc(1:lblc)//
     *                               '),Trc=('//txttrc(1:ltrc)//')'
      call hiswrite(lOut,line)

      call hiswrite(lOut,'MAXEN: Total Iterations = '//itoaf(Niter))
      call hisclose(lOut)

      end

c***********************************************************************

      subroutine defregio(boxes,nMap,nBeam,icentre,jcentre)

      integer boxes(*),nMap(3),nBeam(2),icentre,jcentre

c  Set the region of interest to the lastest area that can be safely
c  deconvolved.
c-----------------------------------------------------------------------
      integer blc(3),trc(3),width
c-----------------------------------------------------------------------
      width = min(icentre-1,nBeam(1)-icentre) + 1
      blc(1) = max(1,(nMap(1)-width)/2)
      trc(1) = min(nMap(1),blc(1)+width-1)

      width = min(jcentre-1,nBeam(2)-jcentre) + 1
      blc(2) = max(1,(nMap(2)-width)/2)
      trc(2) = min(nMap(2),blc(2)+width-1)

      blc(3) = 1
      trc(3) = nMap(3)

      call BoxDef(boxes,3,blc,trc)

      end

c***********************************************************************

      subroutine Zeroit(n,array)

      integer n
      real array(n)
c
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        array(i) = 0
      enddo

      end
