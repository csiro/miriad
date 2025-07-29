      program fft

c= fft - Fourier transform on image(s)
c& mchw
c: uv analysis, map making
c+
c       FFT is a MIRIAD task that performs a fast Fourier transform on
c       an image.  If the input is a cube, then each plane is FFT'ed
c       individually (i.e. this does not perform a 3D FFT).
c
c       Blanked pixels in the input images are treated as zeroes.
c       The input image dimensions will be padded to a power of two if
c       necessary.
c
c       The output of the FFT is normally complex-valued.  You can save
c       it in any one of several ways -- as the real or imaginary part,
c       or as the magnitude (amplitude) and phase.
c@ rin
c       Input real image.  No default.
c@ iin
c       Input imaginary image.  The default is a zero image.
c@ sign
c       Sign of the exponent in the transform.  -1 gives a a forward
c       transform, +1 an inverse transform.  The inverse transform
c       applies 1/N scaling.  The default is a forward transform.
c@ center
c       Origin of the transform.  If two values are given they are used
c       as the origin in the x and y axis respectively.  If one value is
c       given then it's used for the origin for both the x and y.  The
c       default is the header value for CRPIX1 and CRPIX2 or N/2+1 if
c       they are not present in the header.
c@ rout
c       Output real image.  Default is not to write it
c@ iout
c       Output imaginary image.  Default is not to write it.
c@ mag
c       Output amplitude image.  Default is not to write it.
c@ phase
c       Output phase image.  Default is not to write it.
c
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxnax.h'
      include 'maxdim.h'
      include 'mem.h'

      logical   doflag
      integer   Center1, Center2, Sgn, dims(MAXNAX), i,
     *          lMag, lPhase, liIn, liOut, lrIn, lrOut, n1, n2, naxis,
     *          nin(MAXNAX), nout(MAXNAX)
      real      temp
      character mag*64, phase*64, iIn*64, iOut*64, rIn*64, rOut*64,
     *          version*72

      external  hdprsnt, Inc3More, nextpow2, versan
      logical   hdprsnt, Inc3More
      integer   nextpow2
      ptrdiff   Data, cData, npix
      character versan*72
c-----------------------------------------------------------------------
      version = versan('fft')

c     Get the input parameters.
      call keyini
      call keya('rin',rIn,' ')
      call keya('iin',iIn,' ')
      call keya('rout',rOut,' ')
      call keya('iout',iOut,' ')
      call keya('mag',mag,' ')
      call keya('phase',phase,' ')
      call keyi('sign',Sgn,-1)
      if (abs(Sgn).ne.1) call bug('f','Sign must be -1 or 1')
      call keyi('center',Center1,0)
      call keyi('center',Center2,Center1)
      call keyfin

      if (rout.eq.' ' .and.  iout.eq.' ' .and.
     *    mag.eq.' ' .and. phase.eq.' ')
     *  call bug('f','There are no output names')

c     Open the real part, and work out the defaults.
      if (rIn.eq.' ') call bug('f','A real input part must be given')
      call xyopen(lrIn,rIn,'old',MAXNAX,nin)
      doflag = hdprsnt(lrIn,'mask')
      call rdhdi(lrIn,'naxis',naxis,2)
      naxis = min(naxis,MAXNAX)

      n1 = nextpow2(nin(1))
      n2 = nextpow2(nin(2))
      if (n1.ne.nin(1) .or. n2.ne.nin(2))
     *  call bug('w','Padding image up to power of two in size')
      if (max(n1,n2).gt.maxdim) call bug('f','Image too big to handle')

      if (Center1.le.0) then
        call rdhdr(lrIn,'crpix1',temp,real(nin(1)/2 + 1))
        Center1 = nint(temp)
      endif
      if (Center1.lt.1 .or. Center1.gt.n1) Center1 = mod(Center1,n1)
      if (Center1.lt.1) Center1 = Center1 + n1

      if (Center2.le.0) then
        call rdhdr(lrIn,'crpix2',temp,real(nin(2)/2 + 1))
        Center2 = nint(temp)
      endif
      if (Center2.lt.1 .or. Center2.gt.n2) Center2 = mod(Center2,n2)
      if (Center2.lt.1) Center2 = Center2 + n2

      if (iIn.ne.' ') then
c       Check that the imaginary part is the same size.
        call xyopen(liIn,iIn,'old',MAXNAX,nout)
        doflag = doflag .or. hdprsnt(lrIn,'mask')
        do i = 1, MAXNAX
          if (nin(i).ne.nout(i))
     *      call bug('f','Real and imag inputs are different sizes')
        enddo
      else
        liIn = 0
      endif

c     Determine the output size.
      do i = 1, naxis
        nout(i) = nin(i)
      enddo
      nout(1) = n1
      nout(2) = n2

c     Open the output files.
      if (rOut.ne.' ') then
        call xyopen(lrOut,rOut,'new',naxis,nout)
        call header(lrIn,lrOut,nout,'Real',version)
      else
        lrOut = 0
      endif

      if (iOut.ne.' ') then
        call xyopen(liOut,iOut,'new',naxis,nout)
        call header(lrIn,liOut,nout,'Imaginary',version)
      else
        liOut = 0
      endif

      if (mag.ne.' ') then
        call xyopen(lMag,mag,'new',naxis,nout)
        call header(lrIn,lMag,nout,'Magnitude',version)
      else
        lMag = 0
      endif

      if (phase.ne.' ') then
        call xyopen(lPhase,phase,'new',naxis,nout)
        call header(lrIn,lPhase,nout,'Phase',version)
      else
        lPhase = 0
      endif

c     Allocate memory.
      npix = 1_8 * n1 * n2
      if (liIn.eq.0) call MemAllox(Data,npix,'r')
      call MemAllox(cData,npix,'c')

c     Loop FFTing each plane and writing it out.
      call IncIni(naxis,nin,dims)
      do while (Inc3More(naxis,nin,dims))
        if (naxis.gt.2) then
          if (lrIn.ne.0)  call xysetpl(lrIn,naxis-2,dims(3))
          if (liIn.ne.0)  call xysetpl(liIn,naxis-2,dims(3))
          if (lrOut.ne.0) call xysetpl(lrOut,naxis-2,dims(3))
          if (liOut.ne.0) call xysetpl(liOut,naxis-2,dims(3))
          if (lMag.ne.0)  call xysetpl(lMag,naxis-2,dims(3))
          if (lPhase.ne.0) call xysetpl(lPhase,naxis-2,dims(3))
        endif

        if (liIn.eq.0) then
          call GetPlR(memR(Data),nin(1),nin(2),n1,n2,lrIn,doflag)
          if (Sgn.eq.1) call Scale(memR(Data),npix,1.0/real(npix))
          call FFTRC2(memR(Data),memC(cData),n1,n2,Sgn,
     *                                        Center1,Center2)
        else
          call GetPlC(memC(cData),nin(1),nin(2),
     *                                n1,n2,lrIn,liIn,doflag)
          if (Sgn.eq.1) call Scale(memC(cData),2*npix,1.0/real(npix))
          call FFTCC2(MemC(cData),n1,n2,Sgn,Center1,Center2)
        endif

        call OutData(memC(cData),n1,n2,lrOut,liOut,lMag,lPhase)
      enddo

c     Finish up.
      if (liIn.eq.0) call MemFrex(Data,npix,'r')
      call MemFrex(cData,npix,'c')

      call xyclose(lrIn)
      if (liIn.ne.0) call xyclose(liIn)
      if (lrOut.ne.0) call xyclose(lrOut)
      if (liOut.ne.0) call xyclose(liOut)
      if (lMag.ne.0) call xyclose(lMag)
      if (lPhase.ne.0) call xyclose(lPhase)

      end

c***********************************************************************

      subroutine header(lIn,lOut,n,otype,version)

      integer   lIn, lOut, n(2)
      character otype*(*), version*(*)
c-----------------------------------------------------------------------
c  Create the output header for the transformed file.
c-----------------------------------------------------------------------
      integer   iax
      double precision cdelt, crpix, crval
      character algo*3, bunit*32, cax*2, ctype*16, text*64

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
c     Start with a verbatim copy of the input header.
      call headcp(lIn, lOut, 0, 0, 0, 0)

c     Find the spectral axis.
      call coInit(lIn)
      call coFindAx(lIn, 'spectral', iax)
      if (iax.le.2) then
c       Ensure that it's expressed as frequency.
        call coSpcSet(lIn, 'FREQ', ' ', iax, algo)
      endif

c     Update changed keywords.
      call wrhdd(lOut, 'crpix1', dble(n(1)/2+1))
      call wrhdd(lOut, 'crpix2', dble(n(2)/2+1))
      call wrhdd(lOut, 'crval1', 0d0)
      call wrhdd(lOut, 'crval2', 0d0)

      do iax = 1, 2
        call coAxGet(lIn, iax, ctype, crpix, crval, cdelt)
        cdelt = 1d0 / (n(iax)*cdelt)

        if (ctype(:4).eq.'RA--') then
          ctype(:2) = 'UU'

        else if (ctype(:4).eq.'UU--') then
          ctype(:2) = 'RA'
          if (ctype(6:8).eq.'NCP') ctype(6:8) = 'SIN'

        else if (ctype(:4).eq.'DEC-') then
          ctype(:3) = 'VV-'

        else if (ctype(:4).eq.'VV--') then
          ctype(:3) = 'DEC'
          if (ctype(6:8).eq.'NCP') ctype(6:8) = 'SIN'

        else if (ctype(:4).eq.'FREQ') then
          ctype = 'TIME'
          cdelt = 1d-9 * cdelt

        else if (ctype.eq.'TIME') then
          ctype = 'FREQ'
          cdelt = 1d-9 * cdelt
        endif

        cax = itoaf(iax)
        call wrhdd(lOut, 'cdelt'//cax, cdelt)
        call wrhda(lOut, 'ctype'//cax, ctype)
      enddo

      call coFin(lIn)

c     Determine the correct bunit.
      call rdhda(lin, 'bunit', bunit, ' ')
      if (bunit.eq.'JY/BEAM' .or. bunit.eq.'JY') then
        bunit = 'JY'
      else
        bunit = ' '
      endif
      if (otype.eq.'Phase') bunit = 'DEGREES'
      if (bunit.ne.' ') call wrhda(lOut, 'bunit', bunit)

c     Update history.
      call hisopen(lOut,'append')
      text = 'FFT: Miriad ' // version
      call hiswrite(lOut, text)
      call hisinput(lOut, 'FFT')
      text = 'FFT: File is ' // otype // ' data.'
      call hiswrite(lOut, text)
      call hisclose(lOut)

      end

c***********************************************************************

      subroutine OutData(cData,n1,n2,lrOut,liOut,lMag,lPhase)

      integer n1,n2
      integer lrOut,liOut,lMag,lPhase
      complex cData(n1,n2)
c-----------------------------------------------------------------------
c  Write the output data.
c
c  Input:
c    cData      The transform data.
c    n1,n2      Size of the output.
c    lrOut,liOut,lMag,lPhase
c               The handle of the thingos.  Zero if not wanted.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxdim.h'

      integer   i, j
      real      ip, rdat(MAXDIM), rp
c-----------------------------------------------------------------------
c     Output the real file.
      if (lrOut.ne.0) then
        do j = 1, n2
          do i = 1, n1
            rdat(i) = real(cData(i,j))
          enddo
          call xywrite(lrOut,j,rdat)
        enddo
      endif

c     Output the imaginary file.
      if (liOut.ne.0) then
        do j = 1, n2
          do i = 1, n1
            rdat(i) = aimag(cData(i,j))
          enddo
          call xywrite(liOut,j,rdat)
        enddo
      endif

c     Output the magnitude file.
      if (lMag.ne.0) then
        do j = 1, n2
          do i = 1, n1
            rdat(i) = abs(cData(i,j))
          enddo
          call xywrite(lMag,j,rdat)
        enddo
      endif

c     Output the phase file.
      if (lPhase.ne.0) then
        do j = 1, n2
          do i = 1, n1
            rp = real(cData(i,j))
            ip = aimag(cData(i,j))
            if (rp.eq.0 .and. ip.eq.0) then
              rdat(i) = 0
            else
              rdat(i) = atan2(ip,rp)*R2D
            endif
          enddo
          call xywrite(lPhase,j,rdat)
        enddo
      endif

      end

c***********************************************************************

      subroutine GetPlR(rData,n1,n2,n1d,n2d,lrIn,doflag)

      integer n1,n2,n1d,n2d,lrIn
      real    rData(n1d,n2d)
      logical doflag
c-----------------------------------------------------------------------
c  Read in a plane of a real-valued Miriad image.
c
c  Input:
c    n1,n2      Image size.
c    n1d,n2d    Size after padding.
c    lrIn       Handle of the Miriad image file.
c    doflag     True if some of the data is flagged.
c
c  Output:
c    rData      The image.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical   flags(MAXDIM)
      integer   i, j
c-----------------------------------------------------------------------
      do j = 1, n2
        call xyread(lrIn,j,rData(1,j))

        if (doflag) then
          call xyflgrd(lrIn,j,flags)
          do i = 1, n1
            if (.not.flags(i)) rData(i,j) = 0
          enddo
        endif

        do i = n1+1, n1d
          rData(i,j) = 0
        enddo
      enddo

      do j = n2+1, n2d
        do i = 1, n1d
          rData(i,j) = 0
        enddo
      enddo

      end

c***********************************************************************

      subroutine GetPlC(cData,n1,n2,n1d,n2d,lrIn,liIn,doflag)

      integer n1,n2,n1d,n2d,lrIn,liIn
      logical doflag
      complex cData(n1d,n2d)
c-----------------------------------------------------------------------
c  Read in a plane of a complex-valued Miriad image.
c
c  Input:
c    n1,n2      Image size.
c    n1d,n2d    Size after padding.
c    lrIn       Handle of the Miriad real image file.
c    liIn       Handle of the Miriad imaginary image file.
c    doflag     true if some of the data are flagged.
c
c  Output:
c    cData      The output image.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical   flagi(MAXDIM), flagr(MAXDIM)
      integer   i, j
      real      dati(MAXDIM), datr(MAXDIM)
c-----------------------------------------------------------------------
      do j = 1, n2
        call xyread(lrIn,j,datr)
        call xyread(liIn,j,dati)

        do i = 1, n1
          cData(i,j) = cmplx(datr(i),dati(i))
        enddo

        do i = n1+1, n1d
          cData(i,j) = (0.0,0.0)
        enddo

        if (doflag) then
          call xyflgrd(lrIn,j,flagr)
          call xyflgrd(liIn,j,flagi)
          do i = 1, n1
            if (.not.(flagr(i) .and. flagi(i))) cData(i,j) = (0.0,0.0)
          enddo
        endif
      enddo

      do j = n2+1, n2d
        do i = 1, n1d
          cData(i,j) = (0.0,0.0)
        enddo
      enddo

      end
