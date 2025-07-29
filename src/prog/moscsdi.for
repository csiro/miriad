      program moscsdi

c= moscsdi - Mosaic Complex Steer CLEAN algorithm
c& rjs
c: deconvolution
c+
c       MOSCSDI is a MIRIAD task that performs a Complex Steer CLEAN on
c       mosaiced Q and U images or cubes. This follows the CSDI MIRIAD task,
c       but adapted for a mosaic, see the help for this task for more
c       information.
c
c       Full details of the algorithm are found in:
c       Pratley & Johnston-Hollitt, "An improved method for polarimetric
c	image restoration in interferometry", MNRAS, 2016. ArXiv: 1606.01482.
c	Please acknowledge this work in publications using the code.
c
c@ map
c       The input Q and U dirty map, which should have units of Jy/beam
c       No default.
c@ beam
c       The input dirty beam. No default
c@ model
c       An initial Q and U model of the deconvolved image.
c       The default is 0.
c@ out
c       The name of the Q and U output map.
c       The units of the output will be Jy/pixel.
c@ gain
c       CLEAN loop gain. The default is 0.1.
c@ niters
c       The maximum number of iterations. The default is 100.
c@ cutoff
c       Iterating stops if the absolute maximum residual falls below
c       this level.  The default is 0. It is recommended that the
c       cutoff is 3 times the rms noise of Stokes Q or U.
c@ clip
c       This sets the relative clip level.  Values are typically 0.75 to
c       0.9.  The default is 0.9.
c@ region
c       The standard region of interest keyword.  See the help on
c       "region" for more information. The default is the entire image.
c
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
c    mhw 27oct11 - Use ptrdiff type for memory allocations
c    lp  02aug15 - Modified into complex steer clean
c    lp & mjh 8Jun16 - updated commentary and added paper reference.
c    mhw 10jun16 - Some tydying up for inclusion in Miriad distribution
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      integer  MAXRUN, MAXBOXES
      parameter (MAXRUN=5*maxdim, MAXBOXES=(1024)*8+4)

      logical   more
      integer   blc(3), Boxes(MAXBOXES), i, imax, imin, jmax, jmin, k,
     *          kmax, kmin, lBeam, lMap, lModel, lOut, maxniter,
     *          naxis, nbeam(3), ncomp, niter, nMap(3), nModel(3),
     *          nout(MAXNAX), nRun,  Run(3,MAXRUN), trc(3),
     *          xmax, xmin, ymax, ymin, ulMap, ulModel, ulOut
      ptrdiff   pEst, pRes, pStep, pStepR, pWt, nPoint, nAlloc
      ptrdiff   upEst, upRes, upStep, upStepR, upWt
      real      clip, cutoff, dmax, dmin, drms, flux, gain
      real      udmax, udmin, udrms, uflux, maxP
      character BeamNam*256, line*64, MapNam*256, ModelNam*256,
     *          OutNam*256, uMapNam*256, uModelNam*256, uOutNam*256,
     *          version*72
      character itoaf*8, versan*72
      external  itoaf, versan
c-----------------------------------------------------------------------
      version = versan('moscsdi')
c
c  Get the input parameters.
c
      call keyini
      call keya('map',MapNam,' ')
      call keya('map',uMapNam,' ')
      call keya('beam',BeamNam,' ')
      call keya('model',ModelNam,' ')
      call keya('model',uModelNam,' ')
      call keya('out',OutNam,' ')
      call keya('out',uOutNam,' ')
      if (MapNam.eq.' ' .or. uMapNam.eq.' ' .or. BeamNam.eq.' ' .or.
     *  OutNam.eq.' ' .or. uOutNam.eq.' ' .or.
     *  (ModelNam.ne.' '.and.uModelNam.eq.' '))
     *  call bug('f','A file name was missing from the parameters')
      call keyi('niters',maxniter,100)
      if (maxniter.lt.0) call bug('f','NITERS has bad value')
      call keyr('cutoff',cutoff,0.0)
      call keyr('gain',gain,0.1)
      if (gain.le.0 .or. gain.gt.1) call bug('f','Invalid gain value')
      call keyr('clip',clip,0.9)
      if (clip.le.0) call bug('f','Invalid clip value')
      call BoxInput('region',MapNam,Boxes,MaxBoxes)
      call keyfin
c
c  Open the input map.
c
      call xyopen(lMap,MapNam,'old',3,nMap)
      call xyopen(ulMap,uMapNam,'old',3,nMap)
      if (max(nMap(1),nMap(2)).gt.maxdim) call bug('f','Map too big')
      call coInit(lMap)
      call coInit(ulMap)
      call rdhdi(lMap,'naxis',naxis,3)
      call rdhdi(ulMap,'naxis',naxis,3)
      naxis = min(naxis,MAXNAX)
      call BoxMask(lMap,boxes,maxboxes)
      call BoxMask(ulMap,boxes,maxboxes)
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
      if (ModelNam.ne.' ' .or. uModelNam.ne.' ' ) then
        if (ModelNam.ne.' ') then
          call xyopen(lModel,ModelNam,'old',3,nModel)
          if (nModel(1).ne.nOut(1) .or. nModel(2).ne.nOut(2)
        *    .or. nModel(3).ne.nOut(3))
        *     call bug('f','q Model size is bad')
        endif
        if (uModelNam.ne.' ') then
          call xyopen(ulModel,uModelNam,'old',3,nModel)
          if (nModel(1).ne.nOut(1) .or. nModel(2).ne.nOut(2)
        *    .or. nModel(3).ne.nOut(3))
        *     call bug('f','u Model size is bad')
        endif
      endif
c
c  Open up the output.
c
      call xyopen(lOut,OutNam,'new',naxis,nOut)
      call xyopen(ulOut,uOutNam,'new',naxis,nOut)
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
              call memFrex(pStep, nAlloc,'r')
              call memFrex(pStepR,nAlloc,'r')
              call memFrex(pEst,  nAlloc,'r')
              call memFrex(pRes,  nAlloc,'r')
              call memFrex(pWt,   nAlloc,'r')
              call memFrex(upStep, nAlloc,'r')
              call memFrex(upStepR,nAlloc,'r')
              call memFrex(upEst,  nAlloc,'r')
              call memFrex(upRes,  nAlloc,'r')
              call memFrex(upWt,   nAlloc,'r')
            endif
            nAlloc = nPoint
            call memAllox(pStep, nAlloc,'r')
            call memAllox(pStepR,nAlloc,'r')
            call memAllox(pEst,  nAlloc,'r')
            call memAllox(pRes,  nAlloc,'r')
            call memAllox(pWt,   nAlloc,'r')
            call memAllox(upStep, nAlloc,'r')
            call memAllox(upStepR,nAlloc,'r')
            call memAllox(upEst,  nAlloc,'r')
            call memAllox(upRes,  nAlloc,'r')
            call memAllox(upWt,   nAlloc,'r')
          endif
c
c  Get the Map.
c
          call mcPlaneR(lMap,k,Run,nRun,nPoint)
          call mcPlaneR(ulMap,k,Run,nRun,nPoint)
          call xysetpl(lMap,1,k)
          call xysetpl(ulMap,1,k)
          call GetPlane(lMap,Run,nRun,0,0,nMap(1),nMap(2),
     *                        memr(pRes),nAlloc,nPoint)
          call GetPlane(ulMap,Run,nRun,0,0,nMap(1),nMap(2),
     *                        memr(upRes),nAlloc,nPoint)
          call mcSigma2(memr(pWt),nPoint,.false.)
          call mcSigma2(memr(upWt),nPoint,.false.)
c
c  Get the Estimate and Residual. Also get information about the
c  current situation.
c

          if (ModelNam.eq.' ') then
            call Zero(nPoint,memr(pEst))
          endif
          if (uModelNam.eq.' ') then
            call Zero(nPoint,memr(upEst))
          endif

          if (ModelNam.ne.' ') then
              call xysetpl(lModel,1,k-kmin+1)
              call GetPlane(lModel,Run,nRun,1-imin,1-jmin,
       *                nModel(1),nModel(2),memr(pEst),nAlloc,nPoint)
              call Diff(memr(pEst),memr(pRes),memr(pStep),nPoint,
       *                                                     Run,nRun)
              call swap(pRes,pStep)
          endif
          if (uModelNam.ne.' ') then
              call xysetpl(ulModel,1,k-kmin+1)
              call GetPlane(ulModel,Run,nRun,1-imin,1-jmin,
       *                nModel(1),nModel(2),memr(upEst),nAlloc,nPoint)
              call Diff(memr(upEst),memr(upRes),memr(upStep),nPoint,
       *                                                     Run,nRun)
              call swap(upRes,upStep)
          endif
c
c  Do the real work.
c
          niter = 0
          maxP = 0
          more = .true.
          do while (more)
            call ComplexSteer(memr(pEst),memr(upEst),memr(pRes),
     *      memr(upRes),memr(pStep),memr(upStep),memr(pStepR),
     *      memr(upStepR),memr(pWt),memr(upWt),nPoint,Run,nRun,
     *      gain,clip,dmin,udmin,dmax,udmax,drms,udrms,flux,uflux,ncomp)
            niter = niter + ncomp
            call maxPol(memr(pRes),memr(upRes),nPoint,maxP)
            line = 'Steer Iterations: '//itoaf(niter)
            call output(line)
            write(line,'(a,1p3e12.3)')' qResidual min,max,rms: ',
     *                                dmin,dmax,drms
            call output(line)
            write(line,'(a,1p3e12.3)')' uResidual min,max,rms: ',
     *                                udmin,udmax,udrms
            call output(line)
            write(line,'(a,1pe12.3)') ' Total CLEANed qflux: ',flux
            call output(line)
            write(line,'(a,1pe12.3)') ' Total CLEANed uflux: ',uflux
            call output(line)
            write(line,'(a,1pe12.3)') ' Max P: ',maxP
            call output(line)
            more = niter.lt.maxniter .and.
     *           maxP.gt.cutoff
          enddo
        endif
c
c  Write out this plane.
c
        call xysetpl(lOut,1,k-kmin+1)
        call xysetpl(ulOut,1,k-kmin+1)
        call PutPlane(lOut,Run,nRun,1-imin,1-jmin,
     *                        nOut(1),nOut(2),memr(pEst),nPoint)
        call PutPlane(ulOut,Run,nRun,1-imin,1-jmin,
     *                        nOut(1),nOut(2),memr(upEst),nPoint)
      enddo
c
c  Make the header of the output.
c
      call mkHead(lMap,lOut,blc,trc,niter,version)
      call mkHead(ulMap,ulOut,blc,trc,niter,version)
c
c  Free up memory.
c
      if (nAlloc.gt.0) then
        call memFrex(pStep, nAlloc,'r')
        call memFrex(pStepR,nAlloc,'r')
        call memFrex(pEst,  nAlloc,'r')
        call memFrex(pRes,  nAlloc,'r')
        call memFrex(pWt,   nAlloc,'r')
        call memFrex(upStep, nAlloc,'r')
        call memFrex(upStepR,nAlloc,'r')
        call memFrex(upEst,  nAlloc,'r')
        call memFrex(upRes,  nAlloc,'r')
        call memFrex(upWt,   nAlloc,'r')
      endif
c
c  Close up the files. Ready to go home.
c
      call xyclose(lMap)
      call xyclose(ulMap)
      call xyclose(lBeam)
      if (ModelNam.ne.' ') call xyclose(lModel)
      if (uModelNam.ne.' ') call xyclose(ulModel)
      call xyclose(lOut)
      call xyclose(ulOut)
c
c  Thats all folks.
c
      end

c***********************************************************************

      subroutine ComplexSteer(qEst,uEst,qRes,uRes,qStep,uStep,qStepR,
     *      uStepR,qWt,uWt,nPoint,Run,nRun,gain,clip,
     *      qdmin,udmin,qdmax,udmax,qdrms,udrms,qflux,uflux,ncomp)
      ptrdiff nPoint
      integer nRun,Run(3,nRun),ncomp
      real gain,clip,qdmin,qdmax,qdrms,qflux
      real udmin,udmax,udrms,uflux
      real qEst(nPoint),qRes(nPoint),qStep(nPoint),qStepR(nPoint)
      real uEst(nPoint),uRes(nPoint),uStep(nPoint),uStepR(nPoint)
      real qWt(nPoint),uWt(nPoint)
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
      ptrdiff i
      real qg,ug,thresh,zr,zi
      double precision SS,qRS,qRR
      double precision uRS,uRR, mag
c-----------------------------------------------------------------------
c
c  Determine the threshold.
c
      thresh = 0
        do i = 1, nPoint
          thresh = max(thresh,(qRes(i)*qRes(i)+uRes(i)*uRes(i))/
          *    (1/qWt(i)+1/uWt(i)))
          if(qwt(i).ne.uwt(i)) print *,'q,u,wt:',qwt(i),uwt(i)
        enddo
      thresh = clip * clip * thresh
c
c  Get the step to try.
c
      ncomp = 0
      do i = 1, nPoint
        if (((qRes(i)*qRes(i)+uRes(i)*uRes(i))/(1/qWt(i)+1/uWt(i)))
        *   .gt.thresh)  then
          qStep(i) = qRes(i)
          uStep(i) = uRes(i)
          ncomp = ncomp + 1
        else
          qStep(i) = 0
          uStep(i) = 0
        endif
      enddo

      if (ncomp.eq.0) call bug('f','Could not find components')
c
c  Convolve this step.
c
      call mcCnvlR(qStep,Run,nRun,qStepR)
      call mcCnvlR(uStep,Run,nRun,uStepR)
c
c  Now determine the amount of this step to subtract which would
c  minimise the residuals.
c
      SS = 0
      qRS = 0
      uRS = 0
      zr = 0
      zi = 0
      do i = 1, nPoint
        SS = SS + (qStepR(i) * qStepR(i) + uStepR(i) * uStepR(i))/
        *    (1/qWt(i) + 1/uWt(i))
        Call ComplexMultiply(qRes(i),uRes(i),
        *   qStepR(i), -uStepR(i),zr,zi)
        qRS = qRS + zr/(1/qWt(i) + 1/uWt(i))
        uRS = uRS + zi/(1/qWt(i) + 1/uWt(i))
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
      mag = (qRS*qRS + uRS*uRS)/(SS*SS)
      qg = Gain * max(MinOptGain, min(1.0, mag)) * (qRS/SS)/mag
      ug = Gain * max(MinOptGain, min(1.0, mag)) * (uRS/ss)/mag
c
c  Subtract off a fraction of this, and work out the new statistics.
c
      Call ComplexMultiply(qg,ug,qStepR(1), uStepR(1),zr,zi)
      qdmin = qRes(1) - zr
      qdmax = qdmin
      qRR = 0
      qflux = 0
      udmin = uRes(1) - zi
      udmax = udmin
      uRR = 0
      uflux = 0
      do i = 1, nPoint
        Call ComplexMultiply(qg,ug,qStepR(i), uStepR(i),zr,zi)
        qRes(i) = qRes(i) - zr
        qdmin = min(qdmin,qRes(i))
        qdmax = max(qdmax,qRes(i))
        qRR = qRR + qRes(i)*qRes(i)
        qflux = qflux + qEst(i)
        uRes(i) = uRes(i) - zi
        udmin = min(udmin,uRes(i))
        udmax = max(udmax,uRes(i))
        uRR = uRR + uRes(i)*uRes(i)
        uflux = uflux + uEst(i)
        Call ComplexMultiply(qg,ug,qStep(i), uStep(i),zr,zi)
        qEst(i) = qEst(i) + zr
        uEst(i) = uEst(i) + zi
      enddo

      qdrms = sqrt(qRR/nPoint)
      udrms = sqrt(uRR/nPoint)
      end
c***********************************************************************
      subroutine maxPol(qData,uData,n,Dmax)

      ptrdiff n
      real qData(n),uData(n)
      real Dmax
c-----------------------------------------------------------------------
c  Calculate maximum amplitude.
c
c  Input:
c    n          Number of points.
c    Data       Input data array.
c
c  Output:
c    Dmax       Data maxima.
c-----------------------------------------------------------------------
      ptrdiff i
      real temp
c-----------------------------------------------------------------------
c
c  Calculate the maxima.
c

      Dmax = 0
      do i = 1, n
         temp = qData(i)*qData(i)+uData(i)*uData(i)
         if (temp.gt.Dmax) then
            Dmax = temp
         endif
      enddo
      Dmax = sqrt(Dmax)
      end
c***********************************************************************
      subroutine ComplexMultiply(pr, pi, qr, qi, zr, zi)

      real pr, pi, qr, qi, zr, zi
c-----------------------------------------------------------------------
c  Multiply two complex numbers p and q, to give an output of z
c
c  Input:
c    pr, qr     Real parts of each input
c    pi, qi     Imaginary parts of each input
c  Output:
c    zr, zi     Real and imaginary part of output
c-----------------------------------------------------------------------
      zr = pr*qr - pi*qi
      zi = pr*qi + pi*qr
      end
c***********************************************************************

      subroutine Diff(Est,Map,Res,nPoint,Run,nRun)

      ptrdiff nPoint
      integer nRun,Run(3,nRun)
      real Est(nPoint),Map(nPoint),Res(nPoint)
c-----------------------------------------------------------------------
c  Determine the residuals for this model.
c-----------------------------------------------------------------------
      ptrdiff i
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

      ptrdiff n
      real Out(n)
c-----------------------------------------------------------------------
c  Zero an array.
c-----------------------------------------------------------------------
      ptrdiff i
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
