        program mfspin

c= mfspin - Compute spectral index from a MFCLEAN model.
c& rjs
c: deconvolution
c+
c       MFSPIN is a MIRIAD task that computes spectral index and
c       possibly rotation measure images, given models produced by
c       MFCLEAN.  It does this by convolving both the flux and "flux
c       time spectral index" planes of the models by a Gaussian, and
c       then performing a division.
c
c       It can also handle 'pre-convolved' images with flux and flux
c       times spectral index planes such as those produced by restor
c       with options=mfs or linmos with options=alpha.
c
c       It has two modes of operation.  One is to give a single input
c       model (Stokes I or V even) which consists of two planes.  MFSPIN
c       will then compute the spectral index image from that.  The
c       second mode requires both Stokes Q and U models.  MFSPIN will
c       then compute a linearly polarized intensity spectral index and
c       optionally a rotation measure image.
c@ model
c       The model produced by MFCLEAN.  No default.  Normally this will
c       be a single image such as a model for Stokes I or V.  However,
c       you can also give it two models, one for Stokes Q and one for
c       Stokes U (see the discussion above). If the input already has
c       units of 'JY/BEAM' (e.g. output from restor with options=mfs)
c       the convolution step is skipped.
c       
c@ beam
c       The input dirty beam.  This is only required to determine the
c       beam size.  If the fwhm parameter is given, or the input is
c       already convolved, the beam will not be used.
c@ clip
c       The division clip level.  One or two numbers can be given.  If
c       two values are given, then flux pixel values in this range are
c       flagged as bad before the division (but after the convolution!).
c       If a single value is given, then for Stokes I images (or images
c       for which the Stokes parameter is unknown), all pixel values
c       less than the specified clip are flagged.  If one value is given
c       and the image is of Stokes Q, U, or V, then pixels are flagged
c       when their absolute value is less than the clip level.
c       The default is 0.
c@ fwhm
c       The size, in arcsec, of the gaussian beam to convolve with.
c       This will normally be two numbers, giving the
c       full-width at half-maximum of the major and minor axes of the
c       gaussian. If only one number is given, the gaussian will have
c       equal major and minor axes. If no values are given, they are
c       computed by fitting a gaussian to the given dirty beam.
c@ pa
c       The position angle, in degrees, of the Gaussian, measured east
c       from north.  The default is determined from the dirty beam fit
c       (The value for PA is ignored, if no value is given for FWHM).
c@ options
c       Extra processing options. Several can be given, separated by
c       commas. Minimum match is used. Possible options are:
c         pbcorr   Correct the spectral index for primary beam effects.
c                  By default, this is not performed.
c@ out
c       The output image names.  No default.  Normally this will be a
c       single output, being the spectral index.  If the input consists
c       of a Q and a U model, the output can be two names.  The first is
c       the linearly polarized intensity spectral index image, and the
c       second will receive an estimate of the rotation measure (units
c       of rad/m**2).
c
c$Id: mfspin.for,v 1.12 2021/06/02 04:45:09 wie017 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c    mhw  27oct11  Use ptrdiff type for memory allocations
c    mhw  13may13  Read mosaic table for pointing centre if present
c    mhw  22oct13  Cope with restored (I,I*alpha) images
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      integer   MAXRUNS, nP, POLQ, POLU, POLV
      parameter (MAXRUNS=30*MAXDIM, NP=11)
      parameter (POLQ=2, POLU=3 ,POLV=4)

      logical   doQU, NeedFwhm,pbcorr,twoclip
      integer    lBeam,
     *          lMod1, lMod2, mBeam, mModel, nBeam, nModel, nPd,
     *          nruns, nsize(3), pol1, pol2, Runs(3,MAXRUNS), xBeam,
     *          yBeam
      ptrdiff   Alpha1, Alpha2, Flux1, Flux2, Gaus, handle
      ptrdiff   npix
      real      cdelt1, cdelt2, clip(2), crpix1, crpix2, dat(MAXBUF),
     *          fwhm1, fwhm2, pa, Patch(NP*NP)
      double precision nu0
      character beam*128, line*72, modl1*128, modl2*128, out1*128,
     *          out2*128, pbtype*16, version*72, bunit*16

      integer   nextpow2
      logical   keyprsnt, doconv
      character versan*72
      external  keyprsnt, nextpow2, versan

      common dat
c-----------------------------------------------------------------------
      version = versan('mfspin',
     *                 '$Revision: 1.12 $',
     *                 '$Date: 2021/06/02 04:45:09 $')
c
c  Get the input parameters.
c
      call keyini
      call keya('model',modl1,' ')
      call keya('model',modl2,' ')
      call keya('beam',beam,' ')
      call keya('out',out1,' ')
      call keya('out',out2,' ')
      call keyr('clip',clip(1),0.0)
      twoclip = keyprsnt('clip')
      if (twoclip) then
        call keyr('clip',clip(2),0.0)
      else
        clip(2) =  clip(1)
        clip(1) = -clip(1)
      endif
      call keyr('fwhm',fwhm1,0.0)
      call keyr('fwhm',fwhm2,fwhm1)
      call keyr('pa',pa,0.0)
      call GetOpt(pbcorr)
      call keyfin
c
c  Check the reasonableness of the inputs.
c
      if (clip(2).lt.clip(1)) call bug('f','Bad clip range')
      if (modl1.eq.' ') call bug('f','Input model name missing')
      if (Out1.eq.' ')  call bug('f','Output map name missing')
      doQU = modl2.ne.' '
c
c  Open the model, and get various info about it.
c
      call xyopen(lMod1,modl1,'old',3,nsize)
      mModel = nsize(1)
      nModel = nsize(2)
      if (nsize(3).ne.2) call bug('f',
     *        'Model does not appear to be of the correct type')
      call rdhda(lMod1,'bunit',bunit,' ')
      call ucase(bunit)
      doconv = index(bunit,'/BEAM').eq.0

      fwhm1 = fwhm1 * AS2R
      fwhm2 = fwhm2 * AS2R
      NeedFwhm = fwhm1*fwhm2.eq.0.and.doconv
      if (beam.eq.' ' .and. NeedFwhm)
     *  call bug('f','Either beam or fwhm must be given')
c
c  Determine the gaussian that we are going to convolve with. If its is
c  in the beams header, just use that. Otherwise fit it.
c
      if (NeedFwhm) then
        call xyopen(lBeam,beam,'old',3,nsize)
        mBeam = nsize(1)
        nBeam = nsize(2)
        call rdhdr(lBeam,'bmaj',fwhm1,0.0)
        call rdhdr(lBeam,'bmin',fwhm2,0.0)
        call rdhdr(lBeam,'bpa',pa,0.0)
        if (fwhm1*fwhm2.le.0) then
          if (max(mBeam,nBeam).gt.maxdim)
     *    call bug('f','Beam is too big for me to handle')
          nPd = min(NP,mBeam,nBeam)
          nPd = nPd - mod(nPd+1,2)
          call rdhdr(lBeam,'cdelt1',cdelt1,0.0)
          call rdhdr(lBeam,'cdelt2',cdelt2,0.0)
          if (cdelt1*cdelt2.eq.0)
     *      call bug('f','Pixel increments missing from the beam')
          call GetPatch(lBeam,mBeam,nBeam,Patch,nPd,xBeam,yBeam)
          call GetFwhm(Patch,nPd,xBeam-mBeam/2+nPd/2,
     *      yBeam-nBeam/2+nPd/2,cdelt1,cdelt2,Fwhm1,Fwhm2,Pa)
          call wrhdr(lBeam,'bmaj',fwhm1)
          call wrhdr(lBeam,'bmin',fwhm2)
          call wrhdr(lBeam,'bpa',pa)
        endif
        call xyclose(lBeam)
      endif
c
c  Keep the user awake with spurious information.
c
      if (doconv) then
        write(line,'(a,f6.2,a,f6.2,a)')
     *   'Using Gaussian beam fwhm of',fwhm1*R2AS,' by',fwhm2*R2AS,
     *   ' arcsec.'
        call output(line)
        write(line,'(a,f6.1,a)')'Position angle: ',pa,' degrees.'
        call output(line)
      endif
      call GetInfo(lMod1,mModel,nModel,
     *  pol1,nu0,cdelt1,cdelt2,crpix1,crpix2)
      twoclip = twoclip .or.
     *          (pol1.eq.POLQ .or. pol1.eq.POLU .or. Pol1.eq.POLV)
c
c  If there is a second model, open it, etc.
c
      if (doQU) then
        call xyopen(lMod2,modl2,'old',3,nsize)
        if (nsize(1).ne.mModel .or. nsize(2).ne.nModel)
     *    call bug('f','Model sizes do not agree')
        if (nsize(3).ne.2) call bug('f',
     *        'Model does not appear to be of the correct type')
        call GetInfo(lMod2,mModel,nModel,
     *        pol2,nu0,cdelt1,cdelt2,crpix1,crpix2)
c
c  Make the Q model the first.
c
        if (pol1.gt.pol2) call QUswap(lMod1,lMod2,pol1,pol2,modl1,modl2)
c
c  Final checks for sensibleness.
c
        if (pol1.ne.POLQ .and. pol2.ne.POLU) call bug('f',
     *    'The two models do not seem to be Q and U')
        if (out2.ne.' ' .and. nu0.le.0d0) call bug('f',
     *    'Unable to determine reference frequency')
      else if (pol1.eq.POLQ .or. pol1.eq.POLU) then
        call bug('w',
     *    'Spectral index of a linearly polarised map is meaningless')
      endif
c
c  Get the characteristics of the gaussian.
c
      if (doconv) then
        mBeam = nextpow2(mModel)
        nBeam = nextpow2(nModel)
        xBeam = mBeam/2 + 1
        yBeam = nBeam/2 + 1
c
c  Get the Fourier transform of the gaussian.
c
        call MemAllox(Gaus,1_8*mBeam*nBeam,'r')
        call GetBeam(mBeam,nBeam,xBeam,yBeam,dat(Gaus),
     *    cdelt1,cdelt2,fwhm1,fwhm2,pa)
        call CnvlIniA(handle,dat(Gaus),mBeam,nBeam,xBeam,yBeam,0.0,'s')
        call MemFrex(Gaus,1_8*mBeam*nBeam,'r')
      endif
c
c  Allocate all the memory we could possible want.
c
      call MemAllox(Flux1,1_8*mModel*nModel,'r')
      call MemAllox(Alpha1,1_8*mModel*nModel,'r')
      if (doQU) then
        call MemAllox(Flux2,1_8*mModel*nModel,'r')
        call MemAllox(Alpha2,1_8*mModel*nModel,'r')
      else
        Flux2 = Flux1
      endif
c
c  Convolve or read the flux planes and determine the runs.
c
      if (doconv) then
        call CnvlF(handle,lMod1,mModel,nModel,dat(Flux1),'c')
        if (doQU) call CnvlF(handle,lMod2,mModel,nModel,dat(Flux2),'c')
      else
        call ReadPlane(lMod1,mModel,nModel,dat(Flux1))
        if (doQU) call ReadPlane(lMod2,mModel,nModel,dat(Flux2))
      endif
      call Clipper(twoclip,doQU,clip,
     *    dat(Flux1),dat(Flux2),mModel,nModel,Runs,maxruns,nruns)
      if (nruns.eq.0) call bug('f','No good pixels selected')
c
c  Compress them.
c
      call Compact(dat(Flux1),mModel,nModel,Runs,nRuns,npix)
      if (doQU) call Compact(dat(Flux2),mModel,nModel,Runs,nRuns,npix)
c
c  Now convolve and compact the "alpha" plane.
c
      call xysetpl(lMod1,1,2)
      if (doconv) then
        call CnvlF(handle,lMod1,mModel,nModel,dat(Alpha1),'c')
      else
        call ReadPlane(lMod1,mModel,nModel,dat(Alpha1))
      endif
      call Compact(dat(Alpha1),mModel,nModel,Runs,nRuns,npix)
      if (doQU) then
        call xysetpl(lMod2,1,2)
        if (doconv) then
          call CnvlF(handle,lMod2,mModel,nModel,dat(Alpha2),'c')
        else
          call ReadPlane(lMod1,mModel,nModel,dat(Alpha2))
        endif
        call Compact(dat(Alpha2),mModel,nModel,Runs,nRuns,npix)
      endif
c
c  Do the division and primary beam correction (if needed).
c
      if (doQU) then
        call Divide2(npix,dat(Alpha1),dat(Alpha2),
     *                    dat(Flux1),dat(Flux2))
      else
        call Divide1(npix,dat(Alpha1),dat(Flux1))
      endif
      if (pbcorr) call CorrSI(lMod1,npix,dat(Alpha1),Runs,nRuns,pbtype)
c
c  Save the spectral index map.
c
      call WriteOut(out1,dat(Alpha1),npix,mModel,nModel,
     *        Runs,nruns,lMod1,version,fwhm1,fwhm2,pa,
     *        pbcorr,pbtype,.true.,doconv)
c
c  Save the rotation measure map, if required.
c
      if (out2.ne.' ') then
        if (doQU) then
          call CorrRM(npix,dat(Alpha2),nu0)
          call WriteOut(out2,dat(Alpha2),npix,mModel,nModel,
     *        Runs,nruns,lMod1,version,fwhm1,fwhm2,pa,
     *        pbcorr,pbtype,.false.,doconv)
        else
          call bug('w','Second output data-set name ignored')
        endif
      endif
c
c  All said and done. Close up the files, and leave.
c
      call MemFrex(Flux1,1_8*mModel*nModel,'r')
      call MemFrex(Alpha1,1_8*mModel*nModel,'r')
      call xyclose(lMod1)
      if (doQU) then
        call MemFrex(Flux2,1_8*mModel*nModel,'r')
        call MemFrex(Alpha2,1_8*mModel*nModel,'r')
        call xyclose(lMod2)
      endif

      end

c***********************************************************************

      subroutine QUswap(lMod1,lMod2,pol1,pol2,modl1,modl2)

      integer   lMod1, lMod2, pol1, pol2
      character modl1*(*), modl2*(*)
c-----------------------------------------------------------------------
c  Swap around things.
c-----------------------------------------------------------------------
      integer   itmp
      character ctmp*128
c-----------------------------------------------------------------------
      itmp  = lMod1
      lMod1 = lMod2
      lMod2 = itmp

      itmp = pol1
      pol1 = pol2
      pol2 = itmp

      ctmp  = modl1
      modl1 = modl2
      modl2 = ctmp

      end

c***********************************************************************

      subroutine GetOpt(pbcorr)

      logical pbcorr
c-----------------------------------------------------------------------
c  Get processing options.
c
c  Output:
c    pbcorr     If true, correct spectral index for primary beam
c               effects.
c-----------------------------------------------------------------------
      integer nopts
      parameter (nopts=1)
      character opts(nopts)*8
      logical present(nopts)
      data opts/'pbcorr  '/
c-----------------------------------------------------------------------
      call options('options',opts,present,nopts)
      pbcorr = present(1)

      end

c***********************************************************************

      subroutine GetInfo(lMod,nx,ny,pol,nu0,cdelt1,cdelt2,crpix1,crpix2)

      integer lMod, nx, ny, pol
      double precision nu0
      real    cdelt1, cdelt2, crpix1, crpix2
c-----------------------------------------------------------------------
c  Determine various info about the model.
c
c  Input:
c    lMod       The handle of the input model.
c    nx,ny      Model size.
c  Output:
c    pol        The polarisation type of the model.  If this info is
c               missing then Stokes-I is assumed.
c    nu0        Reference frequency of the model, default 0.
c    crpix1,crpix2 Reference pixel -- assumed to be the pointing centre.
c               Default is the model centre.
c    cdelt1,cdelt2 Pixel increments -- No default.
c-----------------------------------------------------------------------
      integer    STOKESI
      parameter (STOKESI=1)

      integer   iax, ifrq, n, naxis
      real      cdelt, crpix, crval
      character algo*3, ctype*16, num*2

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
c     The first two axes are assumed to be RA and DEC.
      call rdhdr(lMod, 'crpix1', crpix1, real(nx/2+1))
      call rdhdr(lMod, 'crpix2', crpix2, real(ny/2+1))
      call rdhdr(lMod, 'cdelt1', cdelt1, 0.0)
      call rdhdr(lMod, 'cdelt2', cdelt2, 0.0)
      if (cdelt1.eq.0.0 .or. cdelt2.eq.0.0)
     *  call bug('f','Model pixel increment missing')

c     Determine the frequency of channel 1.
      nu0 = 0d0
      call coInit(lMod)
      call coSpcSet(lMod, 'FREQ', ' ', ifrq, algo)
      if (ifrq.ne.0) then
        call coCvt1(lMod, ifrq, 'ap', 1d0, 'aw', nu0)
      endif
      call coFin(lMod)

c     Determine the polarisation.
      call rdhdi(lMod, 'naxis', naxis, 0)
      pol = STOKESI
      do iax = 3, naxis
        if (iax.eq.ifrq) continue

        num = itoaf(iax)
        call rdhda(lMod, 'ctype'//num, ctype, ' ')

        if (ctype.eq.'STOKES') then
          call rdhdi(lMod, 'naxis'//num, n, 0)
          if (n.ne.1) call bug('f', 'Cannot handle polarisation cubes')

          call rdhdr(lMod, 'crpix'//num, crpix, 1.0)
          call rdhdr(lMod, 'cdelt'//num, cdelt, 1.0)
          call rdhdr(lMod, 'crval'//num, crval, 1.0)
          pol = nint(crval + (1.0 - crpix)*cdelt)
        endif
      enddo

      end

c***********************************************************************

      subroutine CorrRM(npix,rdat,nu0)

      ptrdiff npix
      real    rdat(npix)
      double precision nu0
c-----------------------------------------------------------------------
c  Scale the rotation measure, to get it into units of rad/m**2
c
c  Input:
c    npix       Number of pixels.
c    nu0        Reference frequency, in GHz.
c  Input/Output:
c    rdat       Rotation measure.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      ptrdiff i
      real    factor
c-----------------------------------------------------------------------
      factor = -nu0*nu0*1d18 / (4d0*DCMKS*DCMKS)

      do i = 1, npix
        rdat(i) = factor * rdat(i)
      enddo

      end

c***********************************************************************

      subroutine CorrSI(tin,npix,Data,Runs,nRuns,type)

      integer nRuns,Runs(3,nRuns),tin
      ptrdiff npix
      real Data(npix)
      character type*(*)
c-----------------------------------------------------------------------
c  Correct the spectral index, in Data, for the effect of the primary
c  beam.
c
c  Input:
c    tin        Handle of first model image
c    npix       Total number of pixels.
c    nRuns,Runs Runs specification.
c  Output:
c    type       Type of primary beam correction; 'GAUSS', 'POLY',
c               'SINGLE'.
c  Input/Output:
c    Data       The spectral index, before and after correction.
c-----------------------------------------------------------------------
      integer i,j,k,pbObj
      ptrdiff pix
      real pbfac,pbdif
      double precision freq,pra(2),pdec(2),x(3)

c     Externals.
      real pbget,pbder
c-----------------------------------------------------------------------
c
c Find frequency of model image and set primary beam
c
      call coInit(tin)
      call coFreq(tin,'op',0d0,freq)
      call pntCent(tin,type,pra,pdec)
      x(1)=pra(1)
      x(2)=pdec(1)
      if (abs(pra(2)).gt.0.or.abs(pdec(2)).gt.0)
     *  call bug('f','mfspin does not handle OTF mosaics yet') 
      x(3)=1
      call pbInitc(pbObj,type,tin,'aw/aw/ap',x,freq,0.d0)

      pix = 0
      do k = 1, nRuns
        j = Runs(1,k)
        do i = Runs(2,k),Runs(3,k)
          pix = pix + 1
          pbfac = pbGet(pbObj,real(i),real(j))
          pbdif = pbDer(pbObj,real(i),real(j))
          if (pbfac.gt.0.0) then
            Data(pix) = Data(pix) - freq*pbdif/pbfac
          else
            Data(pix) = 0.0
          endif
        enddo
      enddo

      call pbFin(pbObj)
      call coFin(tin)

      end

c***********************************************************************

      subroutine Divide1(npix,Num,Denom)

      ptrdiff npix
      real Num(*),Denom(*)
c-----------------------------------------------------------------------
      ptrdiff i
c-----------------------------------------------------------------------
      do i = 1, npix
        Num(i) = Num(i) / Denom(i)
      enddo

      end

c***********************************************************************

      subroutine Divide2(npix,NumR,NumI,DenomR,DenomI)

      ptrdiff npix
      real NumR(npix),NumI(npix),DenomR(npix),DenomI(npix)
c-----------------------------------------------------------------------
c  This divides the complex number, (NumR,NumI) by the complex
c  number (DenomR,DenomI).
c-----------------------------------------------------------------------
      ptrdiff i
      real tempR,tempI,temp
c-----------------------------------------------------------------------
      do i = 1, npix
        temp = DenomR(i)*DenomR(i) + DenomI(i)*DenomI(i)
        tempR = (NumR(i)*DenomR(i) + NumI(i)*DenomI(i)) / temp
        tempI = (NumI(i)*DenomR(i) - NumR(i)*DenomI(i)) / temp
        NumR(i) = tempR
        NumI(i) = tempI
      enddo
      end

c***********************************************************************

      subroutine Compact(Data,nx,ny,Runs,nRuns,npix)

      integer nx,ny,nRuns,Runs(3,nRuns)
      ptrdiff npix
      real Data(1_8*nx*ny)
c-----------------------------------------------------------------------
c  Compress out the unneeded data.
c  Input:
c    nx,ny      Array size.
c    Runs,nRuns Runs specification.
c  Input/Output:
c    data       On input, it contains all the data. On output, just the
c               good data.
c  Output:
c    npix       Total number of good pixels.
c-----------------------------------------------------------------------
      integer i,k
      ptrdiff j
c-----------------------------------------------------------------------
      npix = 0
      do k = 1, nRuns
        j = nx*(Runs(1,k)-1) + Runs(2,k) - 1
        do i = Runs(2,k),Runs(3,k)
          npix = npix + 1
          j = j + 1
          Data(npix) = Data(j)
        enddo
      enddo

      end

c***********************************************************************

      subroutine Clipper(twoclip,twodat,clip,Dat1,Dat2,nx,ny,
     *                                        Runs,maxruns,nruns)

      logical twoclip,twodat
      integer nx,ny,maxruns,Runs(3,maxruns),nruns
      real clip(2),Dat1(nx,ny),Dat2(nx,ny)
c-----------------------------------------------------------------------
c  Determine the good data. Discard data that is below the clip levels.
c
c  Input:
c    twoclip
c    clip       The two clip levels.  If twoclip, data between clip(1)
c               and clip(2) is bad.  If twoclip is false, data below
c               clip(2) is bad.
c    nx,ny      Image size.
c    Dat1,Dat2  The pixel data.
c    maxruns    Max number of runs.
c  Output:
c    runs       The runs of the good data.
c    nruns      The number of runs.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      logical good(MAXDIM)
      integer i,j,ngood
      real clip1,clip2
c-----------------------------------------------------------------------
      clip1 = clip(1)
      clip2 = clip(2)

      nruns = 0
      do j = 1, ny
        if (twoclip) then
          if (twodat) then
            do i = 1, nx
              good(i) = (Dat1(i,j).lt.clip1 .or. Dat1(i,j).gt.clip2)
     *          .and.   (Dat2(i,j).lt.clip1 .or. Dat2(i,j).gt.clip2)
            enddo
          else
            do i = 1, nx
              good(i) = Dat1(i,j).lt.clip1 .or. Dat1(i,j).gt.clip2
            enddo
          endif
        else
          if (twodat) then
            do i = 1, nx
              good(i) = Dat1(i,j).gt.clip2 .and. Dat2(i,j).gt.clip2
            enddo
          else
            do i = 1, nx
              good(i) = Dat1(i,j).gt.clip2
            enddo
          endif
        endif

        ngood = 0
        do i = 1, nx
          if (good(i)) then
            ngood = ngood + 1
          else if (ngood.gt.0) then
            nruns = nruns + 1
            if (nruns.ge.MAXRUNS) call bug('f','Runs buffer overflow')
            Runs(1,nruns) = j
            Runs(2,nruns) = i-ngood
            Runs(3,nruns) = i-1
            ngood = 0
          endif
        enddo
        if (ngood.gt.0) then
          nruns = nruns + 1
          if (nruns.ge.MAXRUNS) call bug('f','Runs buffer overflow')
          Runs(1,nruns) = j
          Runs(2,nruns) = nx - ngood + 1
          Runs(3,nruns) = nx
        endif
      enddo

      Runs(1,nruns+1) = 0

      end

c***********************************************************************

      subroutine WriteOut(Out,Data,npix,nx,ny,Runs,nRuns,
     *  lMod,version,fwhm1,fwhm2,pa,pbcorr,pbtype,si,doconv)

      integer lMod,nx,ny,nRuns
      ptrdiff npix
      integer Runs(3,nRuns+1)
      real Data(npix)
      character version*(*),out*(*),pbtype*(*)
      real fwhm1,fwhm2,pa
      logical pbcorr,si,doconv
c-----------------------------------------------------------------------
c  Write out the convolved model to the output file.
c
c  Input:
c    Out        Output image name.
c    Data       The model pixel values.
c    nx,ny      The size of the model.
c    lMod       The handle of the input file -- used to construct output
c               header.
c    version    Program version.
c    fwhm1,fwhm2,pa Gaussian parameters -- for output history.
c    pbcorr     Primary beam corrected?
c    si         Spectral index map?
c    doconv     Convolution with beam performed?
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   lOut, naxis, nsize(4)
      character line*72
c-----------------------------------------------------------------------
c     Open the output.
      nsize(1) = nx
      nsize(2) = ny
      nsize(3) = 1
      nsize(4) = 1
      call rdhdi(lMod,'naxis',naxis,2)
      naxis = min(naxis,4)
      call xyopen(lOut,Out,'new',naxis,nsize)

c     Write out the data.
      call putplane(lOut,Runs,nRuns,0,0,nx,ny,Data,npix)
      call putruns(lOut,Runs,nRuns,0,0,nx,ny)

c     Make a verbatim copy of the header.
      call headcp(lMod, lOut, 0, 0, 0, 0)

c     Update items that have changed.
      if (si) then
        call wrbtype(lOut,'spectral_index')
        call wrhda(lOut,'bunit','VALUE')
      else
        call wrbtype(lOut,'rotation_measure')
        call wrhda(lOut,'bunit','RAD/M/M')
      endif

c     Write the history file.
      call hisopen(lOut,'append')
      line = 'MFSPIN: Miriad '//version
      call hiswrite(lOut,line)
      call hisinput(lOut,'MFSPIN')
      if (doconv) then
        call wrhdr(lOut,'bmaj', fwhm1)
        call wrhdr(lOut,'bmin', fwhm2)
        call wrhdr(lOut,'bpa',  pa)
        write(line, 100) fwhm1*R2AS, fwhm2*R2AS, pa
100     format('MFSPIN: Beam = ', 1pe10.3, ' x ', 1pe10.3,
     *        ' arcsec, pa = ', 1pe10.3, ' degrees')
        call hiswrite(lOut,line)
      endif

      if (pbcorr .and. si) then
        line = 'MFSPIN: Corrected with primary beam type '//pbtype
        call hiswrite(lOut,line)
      endif

      call hisclose(lOut)
      call xyclose(lOut)

      end

c***********************************************************************

      subroutine GetBeam(mBeam,nBeam,xBeam,yBeam,Beam,
     *                cdelt1,cdelt2,fwhm1,fwhm2,pa)

      integer mBeam,nBeam,xBeam,yBeam
      real Beam(mBeam,nBeam),cdelt1,cdelt2,fwhm1,fwhm2,pa
c-----------------------------------------------------------------------
c  Get the effective beam, less any gaussian.
c
c  Input:
c    mBeam,nBeam  Size of the beam.
c    cdelt1,cdelt2,fwhm1,fwhm2,pa Gaussian parameters.
c  Output:
c    Beam         The output beam.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer i,j
      real    theta,s2,c2,sxx,syy,sxy,a,b,t
c-----------------------------------------------------------------------
c
c  Determine the beam.
c
      theta = pa * D2R
      s2 = -sin(2*theta)
      c2 = -cos(2*theta)
      a = 4*log(2.0)/(fwhm1*fwhm1)
      b = 4*log(2.0)/(fwhm2*fwhm2)
      sxx = -0.5*(a*(c2+1) + b*(1-c2)) * cdelt1*cdelt1
      syy = -0.5*(b*(c2+1) + a*(1-c2)) * cdelt2*cdelt2
      sxy = -(b-a)*s2 * cdelt1*cdelt2
      do j = 1, nBeam
        do i = 1, mBeam
          t = sxx*(i-xBeam)*(i-xBeam) + sxy*(i-xBeam)*(j-yBeam) +
     *      syy*(j-yBeam)*(j-yBeam)
          if (t.gt.-20) then
            Beam(i,j) = exp(t)
          else
            Beam(i,j) = 0
          endif
        enddo
      enddo

      end

c***********************************************************************

      subroutine GetPatch(lBeam,n1,n2,Patch,nP,xBeam,yBeam)

      integer n1,n2,lBeam,nP,xBeam,yBeam
      real Patch(nP,nP)
c-----------------------------------------------------------------------
c  This gets the central portion of the beam, and determines the
c  location of the beam maxima (which is assumed to be near the centre
c  of the beam).
c
c  Inputs:
c    lBeam      Handle of the beam.
c    n1,n2      Dimensions of the beam
c    nP         Size of central patch to return.
c
c  Outputs:
c    xBeam,yBeam Location of beam peak.
c    Patch      The central portion of the beam, centered aound the
c               pixel (n1/2+1,n2/2+1)
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer i,j,imin,imax,jmin,jmax
      real Data(maxdim)

c     Externals.
      integer Ismax
c-----------------------------------------------------------------------
c
c  Open the beam file and check its size.
c
      if (max(n1,n2).gt.maxdim) call bug('f','Beam is too big')
      imin = n1/2 - nP/2 + 1
      imax = imin + nP - 1
      jmin = n2/2 - nP/2 + 1
      jmax = jmin + nP - 1
      if (imin.lt.1 .or. imax.gt.n1 .or. jmin.lt.1 .or. jmax.gt.n2)
     *  call bug('f','Beam is too small')
c
c  Read in the central patch of the beam.
c
      do j = jmin, jmax
        call xyread(lBeam,j,Data)
        do i = imin, imax
          Patch(i-imin+1,j-jmin+1) = Data(i)
        enddo
      enddo
c
c  Find the maximum, and hopefully it is 1.
c
      i = ismax(nP*nP,Patch,1)
      xBeam = mod(i-1,nP) + 1
      yBeam = (i-1)/nP + 1
      if (abs(1-Patch(xBeam,yBeam)).gt.0.01)
     *        call bug('w','Beam peak is not 1')
      xBeam = xBeam + imin - 1
      yBeam = yBeam + jmin - 1

      end

c***********************************************************************

      subroutine GetFwhm(Beam,nP,xBeam,yBeam,cdelt1,cdelt2,
     *                        Fwhm1,Fwhm2,Pa)

      integer xBeam,yBeam,nP
      real Beam(nP*nP)
      real cdelt1,cdelt2,Fwhm1,Fwhm2,Pa
c-----------------------------------------------------------------------
c  Get the full width half max parameters. This calls a routine which
c  finds the least squares fit of the beam patch to a guassian. The
c  result is then converted into more useful units.
c
c  Inputs:
c    Beam       The central portion of the beam.
c    np         Dimension of the beam patch.
c    xBeam,yBeam Location of the center of the beam.
c    cdelt1,cdelt2 Grid increments, in degrees.
c
c  Outputs:
c    Fwhm1      Fwhm, in degrees along the major axis.
c    Fwhm2      Fwhm, in degrees along the minor axis.
c    Pa         Position angle, in degrees, measured east of north.
c
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'mfspin.h'

      integer MaxIter
      parameter (MaxIter=100)
      real X(3),dx(3),aa(3*3),t1,t2
      real f(nPM*nPM),fp(nPM*nPM),dfdx(3*nPM*nPM)
      integer ifail,k,i,j
      external FUNCTION,DERIVE
c-----------------------------------------------------------------------
c
c  Initialise the arrays ready for the optimisation routine.
c
      if (nP.gt.nPM) call bug('f','Beam patch too big to handle')
      k = 0
      do j = 1, nP
        do i = 1, nP
          k = k + 1
          sxxc(k) = (i-xBeam)**2
          syyc(k) = (j-yBeam)**2
          sxyc(k) = (i-xBeam)*(j-yBeam)
          Patch(k) = Beam(k)
        enddo
      enddo
c
c  Form the initial estimate of the gaussian beam, by using the least
c  squares solution of a "linearised" version of the problem.  This
c  should be robust, though somewhat inaccurate.
c
      call LinEst(Beam,nP,xBeam,yBeam,x)
c
c  Now perform the fit using a proper least squares routine.
c
      call nllsqu(3,nP*nP,x,dx,MaxIter,0.0,0.005/3,.true.,ifail,
     *  FUNCTION,DERIVE,f,fp,dx,dfdx,aa)
      if (ifail.ne.0) call bug('f','Beam fit failed')
c
c  Convert the results to meaningful units. The fwhm are in grid units
c  and the pa is in degrees.
c
      x(1) = -x(1) / (cdelt1*cdelt1)
      x(2) = -x(2) / (cdelt2*cdelt2)
      x(3) = -x(3) / (cdelt1*cdelt2)

      t1 = x(1)+x(2)
      t2 = sqrt((x(1)-x(2))**2 + x(3)**2)
      fwhm1 = 0.5 * (t1 - t2)
      fwhm2 = 0.5 * (t1 + t2)
      fwhm1 = sqrt(4*log(2.0)/fwhm1)
      fwhm2 = sqrt(4*log(2.0)/fwhm2)
      if (x(3).ne.0.0) then
        pa = 0.5 * atan2(-x(3),x(1)-x(2)) * R2D
      else
        pa = 0.0
      endif

      end

c***********************************************************************

      subroutine LinEst(Beam,nP,xBeam,yBeam,b)

      integer nP,xBeam,yBeam
      real b(3),Beam(nP,nP)
c-----------------------------------------------------------------------
c  Estimate the parameters for the gaussian fit using an approximate
c  but linear technique. This finds values of b which
c  minimises:
c
c    SUM ( log(Beam(x,y)) - b(1)*x*x - b(2)*y*y - b(3)*x*y )**2
c
c  where the sum is taken over the "main lobe" of the beam only (the
c  "main lobe" is the central part of the beam which is greater than
c  a threshold). Because this is a linear least squares problem, it
c  should always produce a solution (i.e. no worries about convergence
c  of an iterative fitting process).
c
c  Inputs:
c    nP         Dimension of the beam patch.
c    xBeam)     Center pixel of the beam patch.
c    yBeam)
c    Beam       The beam patch.
c
c  Output:
c    b          The estimates of the parameters.
c
c-----------------------------------------------------------------------
      real thresh
      parameter (thresh=0.1)
      integer i,j,ilo,ihi,ilod,ihid,ipvt(3),ifail
      real a(3,3),x,y,z,f
      logical more
c-----------------------------------------------------------------------
c
c  Check that center pixel is within the patch.
c
      if (xBeam.lt.1 .or. xBeam.gt.nP .or. yBeam.lt.1 .or. yBeam.gt.nP)
     *  call bug('f','Centre pixel of beam is not in beam patch')
c
c  Determine the pixel range that spans across the main lobe at x=0.
c
      more = .true.
      ihi = xBeam
      do while (ihi.lt.nP .and. more)
        more = Beam(ihi+1,yBeam).gt.thresh
        if (more) ihi = ihi + 1
      enddo
      ilo = xBeam - (ihi-xBeam)
c
c  Accumulate the info we want over the pixels of the main lobe.  For
c  each row, this also keeps track of the range in x which bridges the
c  central lobe.
c
      do j = 1, 3
        b(j) = 0
        do i = 1, 3
          a(i,j) = 0
        enddo
      enddo

      j = yBeam
      do while (ilo.le.ihi .and. j.le.nP)
        ilod = nP + 1
        ihid = 0
        do i = max(ilo-1,1),min(ihi+1,nP)
          if (Beam(i,j).gt.thresh) then
            ilod = min(ilod,i)
            ihid = max(ihid,i)
            x = (i-xBeam)**2
            y = (j-yBeam)**2
            z = (i-xBeam)*(j-yBeam)
            f = log(Beam(i,j))
            a(1,1) = a(1,1) + x*x
            a(2,1) = a(2,1) + x*y
            a(3,1) = a(3,1) + x*z
            a(2,2) = a(2,2) + y*y
            a(3,2) = a(3,2) + y*z
            a(3,3) = a(3,3) + z*z
            b(1) = b(1) + f*x
            b(2) = b(2) + f*y
            b(3) = b(3) + f*z
          endif
        enddo
        ilo = ilod
        ihi = ihid
        j = j + 1
      enddo

      a(1,2) = a(2,1)
      a(1,3) = a(3,1)
      a(2,3) = a(3,2)
c
c  Solve the 3x3 system of equations, to find the numbers that we really
c  want.  If the matrix proves singular, return the estimate as two grid
c  units.
c
      call sgefa(a,3,3,ipvt,ifail)
      if (ifail.eq.0) then
        call sgesl(a,3,3,ipvt,b,0)
      else
        b(1) = -log(2.0)
        b(2) = -log(2.0)
        b(3) = 0
      endif
      end

c***********************************************************************

      subroutine DERIVE(x,dfdx,n,m)

      integer n,m
      real x(n),dfdx(n,m)
c-----------------------------------------------------------------------
      include 'mfspin.h'
      integer i
      real temp
c-----------------------------------------------------------------------
      do i = 1, m
        temp = sxxc(i)*x(1) + syyc(i)*x(2) + sxyc(i)*x(3)
        if (temp.gt.-20) then
          temp = exp(temp)
        else
          temp = 0
        endif
        dfdx(1,i) = - sxxc(i) * temp
        dfdx(2,i) = - syyc(i) * temp
        dfdx(3,i) = - sxyc(i) * temp
      enddo

      end

c***********************************************************************

      subroutine FUNCTION(x,f,n,m)

      integer n,m
      real x(n),f(m)
c-----------------------------------------------------------------------
c  Calculate the mismatch function.
c-----------------------------------------------------------------------
      include 'mfspin.h'
      integer i
      real temp
c-----------------------------------------------------------------------
      do i = 1, m
        temp = sxxc(i)*x(1) + syyc(i)*x(2) + sxyc(i)*x(3)
        if (temp.gt.-20) then
          f(i) = Patch(i) - exp(temp)
        else
          f(i) = Patch(i)
        endif
      enddo

      end
**************************************************************** pntCent

      subroutine pntCent(lIn,pbtype,pra,pdec)

      integer lIn
      double precision pra(2),pdec(2)
      character pbtype*(*)
c-----------------------------------------------------------------------
c  Determine the pointing centre and the primary beam type.
c
c  Inputs:
c    lIn        Handle of the input image dataset
c  Output:
c    pra,pdec   1:Pointing centre RA and DEC, in radians.
c               2:Pointing centre for next or prev otf mosaic position
c    pbtype     Primary beam type. This will normally just be the
c               name of a telescope (e.g. 'HATCREEK' or 'ATCA'), but it
c               can also be 'GAUS(xxx)', where xxx is a Gaussian primary
c               beam size, with its FWHM given in arcseconds.  For
c               example 'GAUS(120)' is a Gaussian primary beam with
c               FWHM 120 arcsec.
c-----------------------------------------------------------------------
      integer mit,size,iostat,ival(2)
      character string*16

      logical  hdprsnt,otf
      integer  hsize
      external hdprsnt, hsize
c-----------------------------------------------------------------------
c     Zero the otf parameters
      pra(2)=0
      pdec(2)=0
c     Is the mosaic table present?
      if (hdprsnt(lIn, 'mostable')) then
c       Yes, read it.
        call haccess(lIn, mit, 'mostable', 'read', iostat)
        if (iostat.ne.0) call bugno('f',iostat)

c       Check its size.
        size = hsize(mit)

c       Check version
        call hreadi(mit,ival,0,8,iostat)
        otf = ival(2).eq.2
        
        if ((size.ne.56.and..not.otf).or.(size.ne.72.and.otf))
     *    call bug('f','Bad size for mosaic table')

c       Read (RA,Dec).
        call hreadd(mit,pra(1),16,8,iostat)
        if (iostat.eq.0) call hreadd(mit,pdec(1),24,8,iostat)

c       Read the primary beam type.
        if (iostat.eq.0) call hreadb(mit,string,32,16,iostat)
        pbtype = string
        
c       Read the otf parameters
        if (otf) then
          if (iostat.eq.0) call hreadd(mit,pra(2),56,8,iostat)
          if (iostat.eq.0) call hreadd(mit,pdec(2),64,8,iostat)        
        endif
        
        call hdaccess(mit,iostat)
        if (iostat.ne.0) call bugno('f',iostat)

      else
c       No, treat a regular synthesis image.
        call rdhdd(lIn,'crval1',pra(1), 0.d0)
        call rdhdd(lIn,'crval2',pdec(1),0.d0)
        call pbRead(lIn, pbtype)
      endif

c
      end

c***********************************************************************
      subroutine ReadPlane(lu,n1,n2,Dat1)
c
      integer lu,n1,n2
      real Dat1(n1,n2)
c
c  Read the image into memory from a disk file.
c-----------------------------------------------------------------------
      integer i
c
      do i=1,n2
        call xyread(lu,i,Dat1(1,i))
      enddo
c
      end

