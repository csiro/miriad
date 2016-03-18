      program mossdi

c= mossdi - Mosaic Steer CLEAN algorithm
c& rjs
c: deconvolution
c+
c       MOSSDI is a MIRIAD task that performs a Steer CLEAN on a
c       mosaiced image or cube.
c
c@ map
c       The input dirty map, which should have units of Jy/beam
c       No default.
c@ beam
c       The input dirty beam. No default
c@ model
c       An initial model of the deconvolved image. The default is 0.
c@ out
c       The name of the output map.  The units of the output will be
c       Jy/pixel.
c@ gain
c       CLEAN loop gain. The default is 0.1.
c@ niters
c       The maximum number of iterations. The default is 100.
c@ cutoff
c       Iterating stops if the absolute maximum residual falls below t
c       his level.  The default is 0.
c@ clip
c       This sets the relative clip level.  Values are typically 0.75 to
c       0.9.  The default is 0.9.
c@ region
c       The standard region of interest keyword.  See the help on
c       "region" for more information. The default is the entire image.
c@ options
c       Extra processing options:
c         positive   Constrain the deconvolved image to be positive
c                    valued.
c
c$Id: mossdi.for,v 1.11 2016/03/18 03:25:44 wie017 Exp $
c--
c  History:
c    rjs 31oct94 - Original version.
c    rjs  6feb95 - Copy mosaic table to output component table.
c    rjs 27feb97 - Fix glaring bug in the default value for "clip".
c    rjs 28feb97 - Last day of summer. Add options=positive.
c    rjs 02jul97 - cellscal change.
c    rjs 23jul97 - add pbtype.
c    rjs 28nov97 - Increase max number of boxes.
c    rjs 29jan99 - Correct user message only.
c    gmx 07mar04 - Changed optimum gain determination to handle
c                   negative components
c    mhw  27oct11  Use ptrdiff type for memory allocations
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      integer  MAXRUN, MAXBOXES
      parameter (MAXRUN=5*maxdim, MAXBOXES=(1024)*8+4)

      logical   dopos, more
      integer   blc(3), Boxes(MAXBOXES), i, imax, imin, jmax, jmin, k,
     *          kmax, kmin, lBeam, lMap, lModel, lOut, maxniter, nAlloc,
     *          naxis, nbeam(3), ncomp, niter, nMap(3), nModel(3),
     *          nout(MAXNAX), nPoint, nRun,  Run(3,MAXRUN), trc(3),
     *          xmax, xmin, ymax, ymin
      ptrdiff   pEst, pRes, pStep, pStepR, pWt
      real      clip, cutoff, dmax, dmin, drms, flux, gain
      character BeamNam*64, line*64, MapNam*64, ModelNam*64, OutNam*64,
     *          version*72

      character itoaf*8, versan*72
      external  itoaf, versan
c-----------------------------------------------------------------------
      version = versan('mossdi',
     *                 '$Revision: 1.11 $',
     *                 '$Date: 2016/03/18 03:25:44 $')
c
c  Get the input parameters.
c
      call keyini
      call keya('map',MapNam,' ')
      call keya('beam',BeamNam,' ')
      call keya('model',ModelNam,' ')
      call keya('out',OutNam,' ')
      if (MapNam.eq.' ' .or. BeamNam.eq.' ' .or. OutNam.eq.' ')
     *  call bug('f','A file name was missing from the parameters')
      call keyi('niters',maxniter,100)
      if (maxniter.lt.0) call bug('f','NITERS has bad value')
      call keyr('cutoff',cutoff,0.0)
      call keyr('gain',gain,0.1)
      if (gain.le.0 .or. gain.gt.1) call bug('f','Invalid gain value')
      call keyr('clip',clip,0.9)
      if (clip.le.0) call bug('f','Invalid clip value')
      call BoxInput('region',MapNam,Boxes,MaxBoxes)
      call GetOpt(dopos)
      call keyfin
c
c  Open the input map.
c
      call xyopen(lMap,MapNam,'old',3,nMap)
      if (max(nMap(1),nMap(2)).gt.maxdim) call bug('f','Map too big')
      call coInit(lMap)
      call rdhdi(lMap,'naxis',naxis,3)
      naxis = min(naxis,MAXNAX)
      call BoxMask(lMap,boxes,maxboxes)
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
        nout(i) = 1
      enddo
c
c  Allocate arrays to hold everything.
c
      nAlloc = 0
c
c  Open the model if needed, and check that is is the same size as the
c  output.
c
      if (ModelNam.ne.' ') then
        call xyopen(lModel,ModelNam,'old',3,nModel)
        if (nModel(1).ne.nOut(1) .or. nModel(2).ne.nOut(2)
     *    .or. nModel(3).ne.nOut(3)) call bug('f','Model size is bad')
      endif
c
c  Open up the output.
c
      call xyopen(lOut,OutNam,'new',naxis,nOut)
c
c  Initialise the convolution routines.
c
      call xyopen(lBeam,BeamNam,'old',3,nBeam)
      call mcInitF(lBeam)
c
c  Loop.
c
      do k = kmin, kmax
        if (kmin.ne.kmax) then
          call output('Beginning plane: '//itoaf(k))
        else
          call output('Beginning to CLEAN ...')
        endif

        call BoxRuns(1,k,' ',boxes,Run,MaxRun,nRun,
     *                                xmin,xmax,ymin,ymax)
        call BoxCount(Run,nRun,nPoint)
c
c  Allocate the memory, if needed.
c
        if (nPoint.gt.0) then
          if (nPoint.gt.nAlloc) then
            if (nAlloc.gt.0) then
              call memFrep(pStep, nAlloc,'r')
              call memFrep(pStepR,nAlloc,'r')
              call memFrep(pEst,  nAlloc,'r')
              call memFrep(pRes,  nAlloc,'r')
              call memFrep(pWt,   nAlloc,'r')
            endif
            nAlloc = nPoint
            call memAllop(pStep, nAlloc,'r')
            call memAllop(pStepR,nAlloc,'r')
            call memAllop(pEst,  nAlloc,'r')
            call memAllop(pRes,  nAlloc,'r')
            call memAllop(pWt,   nAlloc,'r')
          endif
c
c  Get the Map.
c
          call mcPlaneR(lMap,k,Run,nRun,nPoint)
          call xysetpl(lMap,1,k)
          call GetPlane(lMap,Run,nRun,0,0,nMap(1),nMap(2),
     *                        memr(pRes),nAlloc,nPoint)
          call mcSigma2(memr(pWt),nPoint,.false.)
c
c  Get the Estimate and Residual. Also get information about the
c  current situation.
c
          if (ModelNam.eq.' ') then
            call Zero(nPoint,memr(pEst))
          else
            call xysetpl(lModel,1,k-kmin+1)
            call GetPlane(lModel,Run,nRun,1-imin,1-jmin,
     *                nModel(1),nModel(2),memr(pEst),nAlloc,nPoint)
            call Diff(memr(pEst),memr(pRes),memr(pStep),nPoint,
     *                                                     Run,nRun)
            call swap(pRes,pStep)
          endif
c
c  Do the real work.
c
          niter = 0
          more = .true.
          do while (more)
            call Steer(memr(pEst),memr(pRes),memr(pStep),memr(pStepR),
     *        memr(pWt),nPoint,Run,nRun,
     *        gain,clip,dopos,dmin,dmax,drms,flux,ncomp)
            niter = niter + ncomp
            line = 'Steer Iterations: '//itoaf(niter)
            call output(line)
            write(line,'(a,1p3e12.3)')' Residual min,max,rms: ',
     *                                dmin,dmax,drms
            call output(line)
            write(line,'(a,1pe12.3)') ' Total CLEANed flux: ',Flux
            call output(line)
            more = niter.lt.maxniter .and.
     *           max(abs(dmax),abs(dmin)).gt.cutoff
          enddo
        endif
c
c  Write out this plane.
c
        call xysetpl(lOut,1,k-kmin+1)
        call PutPlane(lOut,Run,nRun,1-imin,1-jmin,
     *                        nOut(1),nOut(2),memr(pEst),nPoint)
      enddo
c
c  Make the header of the output.
c
      call mkHead(lMap,lOut,blc,trc,niter,version)
c
c  Free up memory.
c
      if (nAlloc.gt.0) then
        call memFrep(pStep, nAlloc,'r')
        call memFrep(pStepR,nAlloc,'r')
        call memFrep(pEst,  nAlloc,'r')
        call memFrep(pRes,  nAlloc,'r')
        call memFrep(pWt,   nAlloc,'r')
      endif
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

      subroutine Steer(Est,Res,Step,StepR,Wt,nPoint,Run,nRun,
     *  gain,clip,dopos,dmin,dmax,drms,flux,ncomp)

      integer nPoint,nRun,Run(3,nRun),ncomp
      real gain,clip,dmin,dmax,drms,flux
      real Est(nPoint),Res(nPoint),Step(nPoint),StepR(nPoint)
      real Wt(nPoint)
      logical dopos
c-----------------------------------------------------------------------
c  Perform a Steer iteration.
c
c  Input:
c    nPoint     Number of points in the input.
c    Run,nRun   This describes the region-of-interest.
c    gain       CLEAN loop gain.
c    clip       Steer clip level.
c    Wt         The array of 1/Sigma**2 -- which is used as a weight
c               when determining the maximum residual.
c  Input/Output:
c    Est        The current deconvolved image estimate.
c    Res        The current residuals.
c  Scratch:
c    Step,StepR Used to contain the proposed change and its convolution
c               with the beam pattern.
c  Output:
c    dmin,dmax,drms Min, max and rms residuals after this iteration.
c    ncomp      Number of components subtracted off this time.
c-----------------------------------------------------------------------
      real MinOptGain
      parameter (MinOptGain=0.02)
      integer i
      real g,thresh
      logical ok
      double precision SS,RS,RR
c-----------------------------------------------------------------------
c
c  Determine the threshold.
c
      thresh = 0
      if (dopos) then
        do i = 1, nPoint
          if (Est(i).gt.(-gain*Res(i)))
     *      thresh = max(thresh,Res(i)*Res(i)*Wt(i))
        enddo
      else
        do i = 1, nPoint
          thresh = max(thresh,Res(i)*Res(i)*Wt(i))
        enddo
      endif
      thresh = clip * clip * thresh
c
c  Get the step to try.
c
      ncomp = 0
      do i = 1, nPoint
        ok = Res(i)*Res(i)*Wt(i).gt.thresh
        if (dopos .and. ok) ok = Est(i).gt.(-gain*Res(i))
        if (ok) then
          Step(i) = Res(i)
          ncomp = ncomp + 1
        else
          Step(i) = 0
        endif
      enddo

      if (ncomp.eq.0) call bug('f','Could not find components')
c
c  Convolve this step.
c
      call mcCnvlR(Step,Run,nRun,StepR)
c
c  Now determine the amount of this step to subtract which would
c  minimise the residuals.
c
      SS = 0
      RS = 0
      do i = 1, nPoint
        SS = SS + Wt(i) * StepR(i) * StepR(i)
        RS = RS + Wt(i) * Res(i)   * StepR(i)
      enddo
c
c       RS (and SS?) can be negative, so it is better to take the
c       absolute value of them when determining the optimum
c       gain (gmx - 07mar04)
c
c       abs(RS/SS) may be close to zero, in which case
c       a semi-infinite loop can be the result. We apply a
c       lower limit to abs(RS/SS). A good value for it
c       is empirically determined to be 0.02 (MinOptGain),
c       which may however not be the best choice in all cases.
c       In case of problems, you can try a lower value for the
c       task option Gain before changing MinOptGain (gmx - 07mar04).
c
      g = Gain * max(MinOptGain, min(1.0, abs(real(RS/SS))))
c
c  Subtract off a fraction of this, and work out the new statistics.
c
      dmin = Res(1) - g*StepR(1)
      dmax = dmin
      RR = 0
      flux = 0
      do i = 1, nPoint
        Res(i) = Res(i) - g * StepR(i)
        dmin = min(dmin,Res(i))
        dmax = max(dmax,Res(i))
        RR = RR + Res(i)*Res(i)
        Est(i) = Est(i) + g * Step(i)
        flux = flux + Est(i)
      enddo

      drms = sqrt(RR/nPoint)

      end

c***********************************************************************

      subroutine Diff(Est,Map,Res,nPoint,Run,nRun)

      integer nPoint,nRun,Run(3,nRun)
      real Est(nPoint),Map(nPoint),Res(nPoint)
c-----------------------------------------------------------------------
c  Determine the residuals for this model.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      call mcCnvlR(Est,Run,nRun,Res)

      do i = 1, nPoint
        Res(i) = Map(i) - Res(i)
      enddo

      end

c***********************************************************************

      subroutine Swap(a,b)

      ptrdiff a, b
c-----------------------------------------------------------------------
c  Swap two long integers.
c-----------------------------------------------------------------------
      ptrdiff t
c-----------------------------------------------------------------------
      t = a
      a = b
      b = t
      end

c***********************************************************************

      subroutine Zero(n,Out)

      integer n
      real Out(n)
c-----------------------------------------------------------------------
c  Zero an array.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        Out(i) = 0
      enddo

      end

c***********************************************************************

      subroutine mkHead(lIn,lOut,blc,trc,niter,version)

      integer   lIn, lOut, blc(3), trc(3), niter
      character version*72
c-----------------------------------------------------------------------
c  Write a header for the output file.
c
c  Input:
c    version    Program version ID.
c    lIn        Handle of the input map.
c    lOut       Handle of the output estimate.
c    blc        BLC of the bounding region.
c    trc        TRC of the bounding region.
c    niter      Maximum number of iterations performed.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   axmap(MAXNAX), iax, k1, k2, lblc(MAXNAX), ltrc(MAXNAX)
      character line*72, txtblc*32, txttrc*32

      character itoaf*8
      external  itoaf
c-----------------------------------------------------------------------
c     Set up to copy input keywords with subimaging.
      do iax = 1, MAXNAX
        axmap(iax) = iax
        if (iax.le.3) then
          lblc(iax) = blc(iax)
        else
          lblc(iax) = 1
        endif
        ltrc(iax) = 0
      enddo

      call headcp(lIn, lOut, MAXNAX, axmap, lblc, ltrc)

c     Update parameters that will have changed.
      call wrhda(lOut, 'bunit', 'JY/PIXEL')
      call wrhdi(lOut, 'niters', niter)

c     Write history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'MOSSDI: Miriad ' // version)
      call hisinput(lOut, 'MOSSDI')

      call mitoaf(blc, 3, txtblc, k1)
      call mitoaf(trc, 3, txttrc, k2)
      line = 'MOSSDI: Bounding region is BLC=(' // txtblc(:k1) //
     *                                '),TRC=(' // txttrc(:k2) // ')'
      call hiswrite(lOut, line)

      call hiswrite(lOut, 'MOSSDI: Total Iterations = ' // itoaf(niter))
      call hisclose(lOut)

      end

c***********************************************************************

      subroutine GetOpt(dopos)

      logical dopos
c-----------------------------------------------------------------------
c  Output:
c    dopos      Constrain the model to be positive valued.
c-----------------------------------------------------------------------
      integer NOPTS
      parameter (NOPTS=1)
      logical present(NOPTS)
      character opts(NOPTS)*8
      data opts/'positive'/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)
      dopos = present(1)

      end
