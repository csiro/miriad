      program mossen

c= mossen -- Determine the rms and gain in a mosaiced image.
c& rjs
c: map combination
c+
c       MOSSEN determines the rms and gain of a mosaiced image.  Both
c       of these parameters are a function of position (and also mildly
c       frequency dependent).
c
c@ in
c       The input map of interest. No default.
c@ region
c       The standard region of interest parameter, giving the region
c       where the sensitivity function is to be determined. The default
c       is the entire input image.
c@ sen
c       The output image giving the rms as a function of position.
c       The default is that this image is not created.
c@ gain
c       The output image giving the gain to a unit point source as a
c       function of position. To avoid noise amplification at the
c       edge of mosaiced regions, Miriad does not normally totally
c       correct for the primary beam beyond a certain point. The default
c       is that no gain image is formed.
c
c$Id: mossen.for,v 1.5 2018/11/29 23:30:11 wie017 Exp $
c--
c  History:
c    rjs   6nov94 Original version.
c    rjs  13mar95 Add call to mosMFin
c    rjs  23jul97 Add pbtype.
c    rjs  24feb98 Write bunit keyword to output.
c    rjs  30sep99 Make output the full size of the region-of-interest.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      character version*(*)
      integer MAXBOXES,MAXRUNS
      parameter (version='MosSen: version 1.0 30-Sep-99')
      parameter (MAXBOXES=1024,MAXRUNS=3*MAXDIM)

      integer tIn,tSen,tGain,i,k
      ptrdiff pSen,pGain,nBuff,npix1,npix2
      integer xmin,xmax,ymin,ymax
      logical dosen,dogain
      character in*64,sen*64,gain*64
      integer boxes(MAXBOXES),nin(3),nout(MAXNAX)
      integer Runs1(3,MAXRUNS),Runs2(3,MAXRUNS),nRuns1,nRuns2
      integer naxis,blc(MAXNAX),trc(MAXNAX)
      integer npnt
c-----------------------------------------------------------------------
c
c  Get the input parameters.
c
      call output(version)
      call keyini
      call keya('in',in,' ')
      if (in.eq.' ') call bug('f','An input must be given')
      call keya('sen',sen,' ')
      call keya('gain',gain,' ')
      dosen = sen.ne.' '
      dogain = gain.ne.' '
      if (.not.dosen .and. .not.dogain) call bug('f',
     *  'Either an output rms or gain image must be requested')
      call BoxInput('region',in,boxes,MAXBOXES)
      call keyfin
c
c  Open the input.
c
      call xyopen(tIn,in,'old',3,nin)
      call coInit(tIn)
      call rdhdi(tIn,'naxis',naxis,3)
      naxis = min(naxis,MAXNAX)
c
c  Initialise the mosaic routines.
c
      call mosLoad(tIn,npnt)
c
c  Set up the region of interest and determine the output image size.
c
      call boxSet(boxes,3,nin,' ')
      call boxInfo(boxes,naxis,blc,trc)
      call boxMask(tIn,Boxes,MAXBOXES)
      nout(1) = trc(1) - blc(1) + 1
      nout(2) = trc(2) - blc(2) + 1
      nout(3) = trc(3) - blc(3) + 1
      do i = 4, naxis
        nout(i) = 1
      enddo

c     Create the output images.
      if (dosen) then
        call mkOut(tIn,tSen,sen,naxis,nout,blc,.true.,version)
      endif

      if (dogain) then
        call mkOut(tIn,tGain,gain,naxis,nout,blc,.false.,version)
      endif

      nBuff = 0
      do k = blc(3), trc(3)
        call boxRuns(1,k,' ',boxes,Runs1,MAXRUNS,nRuns1,
     *                                xmin,xmax,ymin,ymax)
        call Count(Runs1,nRuns1,npix1)

c       Allocate memory if needed.
        if (npix1.gt.nBuff) then
          if (nBuff.gt.0) then
            call memFrex(pSen,nBuff,'r')
            call memFrex(pGain,nBuff,'r')
          endif
          nBuff = npix1
          call memAllox(pSen,nBuff,'r')
          call memAllox(pGain,nBuff,'r')
        endif

c       Do the real work now.
        call mosMini(tIn,real(k))
        call mosWtsR(Runs1,nRuns1,memr(pGain),memr(pSen),npix1)
        call Compress(memr(pSen),memr(pGain),npix1,npix2,
     *    Runs1,nRuns1,Runs2,nRuns2,MAXRUNS)

c       Store the result.  We actually compute the reciprocal of what
c       the user wants.
        if (dosen) then
          if (naxis.gt.2) call xysetpl(tSen,1,k-blc(3)+1)
          call PutPlane(tSen,Runs2,nRuns2,1-blc(1),1-blc(2),
     *                        nOut(1),nOut(2),memr(pSen),npix2)
          call PutRuns(tSen,Runs2,nRuns2,1-blc(1),1-blc(2),
     *                        nOut(1),nOut(2))
        endif

        if (dogain) then
          if (naxis.gt.2) call xysetpl(tGain,1,k-blc(3)+1)
          call PutPlane(tGain,Runs2,nRuns2,1-blc(1),1-blc(2),
     *                        nOut(1),nOut(2),memr(pGain),npix2)
          call PutRuns(tGain,Runs2,nRuns2,1-blc(1),1-blc(2),
     *                        nOut(1),nOut(2))
        endif
        call mosMFin
      enddo
c
c  Close up shop.
c
      if (dogain) call xyclose(tGain)
      if (dosen) call xyclose(tSen)
      call memFrex(pSen,nBuff,'r')
      call memFrex(pGain,nBuff,'r')
      call xyclose(tIn)
      end

c***********************************************************************

      subroutine Count(Runs,nRuns,npix)

      integer nRuns,Runs(3,nRuns+1)
      ptrdiff npix
c-----------------------------------------------------------------------
c  Count the number of pixels of interest in this plane.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      npix = 0
      do i = 1, nRuns
        npix = npix + Runs(3,i) - Runs(2,i) + 1
      enddo

      end

c***********************************************************************

      subroutine Compress(Sen,Gain,npix1,npix2,
     *    Runs1,nRuns1,Runs2,nRuns2,MAXRUNS)

      ptrdiff npix1,npix2
      integer nRuns1,nRuns2,MAXRUNS
      integer Runs1(3,nRuns1),Runs2(3,MAXRUNS)
      real Sen(npix1),Gain(npix1)
c-----------------------------------------------------------------------
c  Eliminate pixels that have a gain of zero.
c-----------------------------------------------------------------------
      integer i,iRuns,ngood
      ptrdiff opt,ipt
c-----------------------------------------------------------------------
      ipt = 0
      opt = 0
      nRuns2 = 0
      do iRuns = 1, nRuns1
        ngood = 0
        do i = Runs1(2,iRuns),Runs1(3,iRuns)
          ipt = ipt + 1
          if (Gain(ipt).gt.0) then
            ngood = ngood + 1
            opt = opt + 1
            Gain(opt) = 1/Gain(ipt)
            Sen(opt)  = sqrt(Sen(ipt)*Gain(opt))
          else if (ngood.gt.0) then
            nRuns2 = nRuns2 + 1
            if (nRuns2.gt.MAXRUNS) call bug('f','Runs array overflow')
            Runs2(1,nRuns2) = Runs1(1,iRuns)
            Runs2(2,nRuns2) = i - ngood
            Runs2(3,nRuns2) = i - 1
            ngood = 0
          endif
        enddo
        if (ngood.gt.0) then
          nRuns2 = nRuns2 + 1
          if (nRuns2.ge.MAXRUNS) call bug('f','Runs array overflow')
          Runs2(1,nRuns2) = Runs1(1,iRuns)
          Runs2(2,nRuns2) = Runs1(3,iRuns) - ngood + 1
          Runs2(3,nRuns2) = Runs1(3,iRuns)
        endif
      enddo

      Runs2(1,nRuns2+1) = 0
      Runs2(2,nRuns2+1) = 0
      Runs2(3,nRuns2+1) = 0
      npix2 = opt

      end

c***********************************************************************

      subroutine mkOut(tIn, tOut, outnam, naxis, nout, blc, dosen,
     *                  version)

      integer   tIn, tOut, naxis, nout(naxis), blc(naxis)
      character outnam*(*), version*(*)
      logical   dosen
c-----------------------------------------------------------------------
c  Create an output dataset.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   iax
      double precision crpix
      character line*72, axn*1

      character itoaf*8
      external  itoaf
c-----------------------------------------------------------------------
c     Open the output.
      call xyopen(tOut, outnam, 'new', naxis, nout)

c     Start with a verbatim copy of the input keywords.
      call headcp(tIn, tOut, 0, 0, 0, 0)

c     Adjust the reference pixel for subimaging.
      do iax = 1, naxis
        if (blc(iax).ne.1) then
          axn = itoaf(iax)
          call rdhdd(tIn,  'crpix'//axn, crpix, 1d0)
          crpix = crpix - dble(blc(iax) - 1)
          call wrhdd(tOut, 'crpix'//axn, crpix)
        endif
      enddo

c     Update changed keywords.
      if (.not.dosen) call wrhda(tOut, 'bunit', 'GAIN')

c     Write history.
      call hisopen(tOut,'append')
      line = 'MOSSEN: Miriad '//version
      call hiswrite(tOut,line)
      call hisinput(tOut,'MOSSEN')
      call hisclose(tOut)

      end
