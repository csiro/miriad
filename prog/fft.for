      program fft

c= fft - Fourier transform on image(s)
c& mchw
c: uv analysis, map making
c+
c       FFT is a MIRIAD task which performs a fast Fourier transform on
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
c       The input real part image.  No default.
c@ iin
c       The input imaginary part image.  The default is a zero image.
c@ sign
c       This gives the sign of the exponent in the transform. -1 gives a
c       forward transform, +1 an inverse transform. An inverse transform
c       applies 1/N scaling. The default is a forward transform.
c@ center
c       Origin of the transform.  If two values are given they are used
c       as the origin in the x and y axis respectively.  If one value is
c       given then it's used for the origin for both the x and y.  The
c       default is the header value for CRPIX1 and CRPIX2 or N/2+1 if
c       they are not present in the header. 
c@ rout
c       The output real part image.  The default is not to write this
c       image.
c@ iout
c       The output imaginary part image.  Default is not to write it.
c@ mag
c       The output magnitude (amplitude) image.  Default is not to write
c       it.
c@ phase
c       The output phase image.  Default is not to write it.
c
c$Id: fft.for,v 1.3 2011/10/05 05:59:53 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxnax.h'
      include 'maxdim.h'
      include 'mem.h'

      logical   doflag
      integer   cData, Center1, Center2, Data, Sgn, dims(MAXNAX), i,
     *          lMag, lPhase, liIn, liOut, lrIn, lrOut, n1, n2, naxis,
     *          nin(MAXNAX), nout(MAXNAX)
      real      temp
      character mag*64, phase*64, iIn*64, iOut*64, rIn*64, rOut*64,
     *          version*72

      external  hdprsnt, Inc3More, nextpow2, versan
      logical   hdprsnt, Inc3More
      integer   nextpow2
      character versan*72
c-----------------------------------------------------------------------
      version = versan('fft',
     *                 '$Revision: 1.3 $',
     *                 '$Date: 2011/10/05 05:59:53 $')
c
c  Get the input parameters.
c
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
c
c  Open the real part, and work out the defaults.
c
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
c
c  Process it according to whether there is an imaginary part to
c  worry about.
c
      if (iIn.ne.' ') then
        call xyopen(liIn,iIn,'old',MAXNAX,nout)
        doflag = doflag .or. hdprsnt(lrIn,'mask')
        do i = 1, MAXNAX
          if (nin(i).ne.nout(i))
     *      call bug('f','Real and imag inputs are different sizes')
        enddo
      else
        liIn = 0
      endif
c
c  Determine the output size.
c
      do i = 1, naxis
        nout(i) = nin(i)
      enddo
      nout(1) = n1
      nout(2) = n2
c
c  Now open the output files.
c
      if (rOut.ne.' ') then
        call xyopen(lrOut,rOut,'new',naxis,nout)
        call header(lrIn,lrOut,'Real',version,nout)
      else
        lrOut = 0
      endif

      if (iOut.ne.' ') then
        call xyopen(liOut,iOut,'new',naxis,nout)
        call header(lrIn,liOut,'Imaginary',version,nout)
      else
        liOut = 0
      endif

      if (mag.ne.' ') then
        call xyopen(lMag,mag,'new',naxis,nout)
        call header(lrIn,lMag,'Magnitude',version,nout)
      else
        lMag = 0
      endif

      if (phase.ne.' ') then
        call xyopen(lPhase,phase,'new',naxis,nout)
        call header(lrIn,lPhase,'Phase',version,nout)
      else
        lPhase = 0
      endif
c
c  Allocate memory.
c
      if (liIn.eq.0) call MemAlloc(Data,n1*n2,'r')
      call MemAlloc(cData,n1*n2,'c')
c
c  Loop around doing the FFT of a plane, then writing it out.
c
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
          if (Sgn.eq.1) call Scale(memR(Data),n1*n2,1.0/real(n1*n2))
          call FFTRC2(memR(Data),memC(cData),n1,n2,Sgn,
     *                                        Center1,Center2)
        else
          call GetPlC(memC(cData),nin(1),nin(2),
     *                                n1,n2,lrIn,liIn,doflag)
          if (Sgn.eq.1) call Scale(memC(cData),2*n1*n2,1.0/real(n1*n2))
          call FFTCC2(MemC(cData),n1,n2,Sgn,Center1,Center2)
        endif
        call OutData(memC(cData),n1,n2,lrOut,liOut,lMag,lPhase)
      enddo
c
c  Finish up.
c
      if (liIn.eq.0) call MemFree(Data,n1*n2,'r')
      call MemFree(cData,n1*n2,'c')

      call xyclose(lrIn)
      if (liIn.ne.0) call xyclose(liIn)
      if (lrOut.ne.0) call xyclose(lrOut)
      if (liOut.ne.0) call xyclose(liOut)
      if (lMag.ne.0) call xyclose(lMag)
      if (lPhase.ne.0) call xyclose(lPhase)

      end

c***********************************************************************

      subroutine Scale(rData,n,factor)

      integer n
      real    rData(n), factor
c-----------------------------------------------------------------------
c  Scale the image by a factor.
c
c  Input:
c    n          Number of pixels to scale.
c    factor     The scale factor.
c  Input/Output:
c    rData      The data to be scaled.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        rData(i) = factor * rData(i)
      enddo

      end

c***********************************************************************

      subroutine FFTRC2(In,Out,n1,n2,sgn,Ic,Jc)

      integer n1,n2,Sgn,Ic,Jc
      real    In(n1,n2)
      complex Out(n1,n2)
c-----------------------------------------------------------------------
c  Perform a Fourier transform of a real image. There is no scaling.
c  The "phase-centre" of the transform is (ic,jc), and the centre of
c  the output is (n1/2+1,n2/2+1).
c
c  Inputs:
c    n1,n2      Size of the input image.
c    ic,jc      "Phase-centre" of the input image.
c    Sgn        Sign of the transform, either +1 or -1.
c    In         The input real image.
c
c  Output:
c    Out        The output complex image.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer   i, j
      real      rdat(MAXDIM)
      complex   cdat(MAXDIM), cdat2(MAXDIM)
c-----------------------------------------------------------------------
c
c  First pass -- transform in the x direction. During this pass, shift
c  the image in x so that the centre is at pixel (1,?). Also multiply
c  by (-1)**(i-1), so that the centre of the output is at pixel n1/2.
c
      do j = 1, n2
        do i = ic, n1
          rdat(i-ic+1) =  In(i,j)
        enddo

        do i = 1, ic-1
          rdat(i-ic+n1+1) = In(i,j)
        enddo

        do i = 1,n1,2
          rdat(i+1) = -rdat(i+1)
        enddo

        call fftrc(rdat,out(1,j),sgn,n1)
      enddo
c
c  Second pass -- transform in the y direction. During this pass, shift
c  the image in y so that the centre is at pixel (1,1). Also multiply
c  by (-1)**(j-1), so that the centre of the output is at pixel n2/2.
c
      do i = 1, n1/2+1
        do j = jc, n2
          cdat(j-jc+1) =  Out(i,j)
        enddo

        do j = 1, jc-1
          cdat(j-jc+n2+1) = Out(i,j)
        enddo

        do j = 1,n2,2
          cdat(j+1) = -cdat(j+1)
        enddo

        call fftcc(cdat,cdat2,sgn,n2)

        do j = 1, n2
          Out(i,j) = cdat2(j)
        enddo
      enddo
c
c  Third pseudo-pass: Make the output full size, by using complex
c  conjugate symmetry to fill in unused spaces.
c
c#ivdep
      do i = n1/2+2, n1
        Out(i,1) = conjg(Out(n1+2-i,1))
      enddo

      do j = 2, n2
c#ivdep
        do i = n1/2+2, n1
          Out(i,j) = conjg(Out(n1+2-i,n2+2-j))
        enddo
      enddo

      end

c***********************************************************************

      subroutine FFTCC2(In,n1,n2,sgn,Ic,Jc)

      integer n1,n2,Sgn,Ic,Jc
      complex In(n1,n2)
c-----------------------------------------------------------------------
c  Perform a Fourier transform of a complex image. There is no scaling.
c  The "phase-centre" of the transform is (ic,jc), and the centre of the
c  output is (n1/2+1,n2/2+1).
c
c  Inputs:
c    n1,n2      Size of the input image.
c    ic,jc      "Phase-centre" of the input image.
c    Sgn        Sign of the transform, either +1 or -1.
c
c  Input/Output:
c    In         The image to be transformed.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer   i, j
      complex   cdat(MAXDIM), cdat2(MAXDIM)
c-----------------------------------------------------------------------
c
c  First pass -- transform in the x direction. During this pass, shift
c  the image in x so that the centre is at pixel (1,?). Also multiply
c  by (-1)**(i-1), so that the centre of the output is at pixel n1/2.
c
      do j = 1, n2
        do i = ic, n1
          cdat(i-ic+1) =  In(i,j)
        enddo

        do i = 1, ic-1
          cdat(i-ic+n1+1) = In(i,j)
        enddo

        do i = 1,n1,2
          cdat(i+1) = -cdat(i+1)
        enddo

        call fftcc(cdat,in(1,j),sgn,n1)
      enddo
c
c  Second pass -- transform in the y direction. During this pass, shift
c  the image in y so that the centre is at pixel (1,1). Also multiply
c  by (-1)**(j-1), so that the centre of the output is at pixel n2/2.
c
      do i = 1, n1
        do j = jc, n2
          cdat(j-jc+1) =  In(i,j)
        enddo

        do j = 1, jc-1
          cdat(j-jc+n2+1) = In(i,j)
        enddo

        do j = 1,n2,2
          cdat(j+1) = -cdat(j+1)
        enddo

        call fftcc(cdat,cdat2,sgn,n2)

        do j = 1, n2
          In(i,j) = Cdat2(j)
        enddo
      enddo

      end

c***********************************************************************

      subroutine header(lIn,lOut,Type,version,n)

      integer lIn,lOut,n(2)
      character Type*(*),version*(*)
c-----------------------------------------------------------------------
c  Create the output header for the transformed file.
c-----------------------------------------------------------------------
      integer NKEYS
      parameter (NKEYS=20)

      integer   i
      double precision cdelt
      character ax*1, bunit*32
      character ctype*16, keyw(NKEYS)*8, line*64

      external  itoaf
      character itoaf*1

      data keyw/   'obstime ','epoch   ','history ',
     *  'object  ','telescop','vobs    ','restfreq',
     *  'cellscal',
     *  'cdelt3  ','crval3  ','crpix3  ','ctype3  ',
     *  'cdelt4  ','crval4  ','crpix4  ','ctype4  ',
     *  'cdelt5  ','crval5  ','crpix5  ','ctype5  '/
c-----------------------------------------------------------------------
c
c  Copy all the other keywords across, which have not changed.
c
      call wrhdr(lOut,'crpix1',real(n(1)/2+1))
      call wrhdr(lOut,'crpix2',real(n(2)/2+1))
      call wrhdd(lOut,'crval1',0d0)
      call wrhdd(lOut,'crval2',0d0)

      do i = 1, 2
        ax = itoaf(i)
        call rdhdd(lIn,'cdelt'//ax,cdelt,1d0)
        call rdhda(lIn,'ctype'//ax,ctype,' ')
        if (ctype(1:4).eq.'RA--') then
          ctype(1:2) = 'UU'
          cdelt = 1.0/(n(i)*cdelt)
        else if (ctype(1:4).eq.'UU--') then
          ctype(1:2) = 'RA'
          if (ctype(6:8).eq.'NCP') ctype(6:8) = 'SIN'
          cdelt = 1.0/(n(i)*cdelt)
        else if (ctype(1:4).eq.'DEC-') then
          ctype(1:3) = 'VV-'
          cdelt = 1.0/(n(i)*cdelt)
        else if (ctype(1:4).eq.'VV--') then
          ctype(1:3) = 'DEC'
          if (ctype(6:8).eq.'NCP') ctype(6:8) = 'SIN'
          cdelt = 1.0/(n(i)*cdelt)
        else if (ctype(1:4).eq.'FREQ') then
          ctype = 'TIME'
          cdelt = 1.0/(n(i)*cdelt*1e9)
        else if (ctype.eq.'TIME') then
          ctype = 'FREQ'
          cdelt = 1e-9/(n(i)*cdelt)
        endif
        call wrhdd(lOut,'cdelt'//ax,cdelt)
        call wrhda(lOut,'ctype'//ax,ctype)
      enddo
c
c  Copy other keywords and add to the history file.
c
      do i = 1, NKEYS
        call hdcopy(lIn,lOut,keyw(i))
      enddo
c
c  Determine correct bunit.
c
      call rdhda(lin,'bunit',bunit,' ')
      if (bunit.eq.'JY/BEAM' .or. bunit.eq.'JY') then
        bunit = 'JY'
      else
        bunit = ' '
      endif
      if (Type.eq.'Phase') bunit = 'DEGREES'
      if (bunit.ne.' ') call wrhda(lOut,'bunit',bunit)
c
c  Write the history.
c
      call hisopen(lOut,'append')
      line = 'FFT: Miriad '//version
      call hiswrite(lOut,line)
      call hisinput(lOut,'FFT')
      line = 'FFT: File is '//Type//' data.'
      call hiswrite(lOut,line)
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
c
c  Output the real file.
c
      if (lrOut.ne.0) then
        do j = 1, n2
          do i = 1, n1
            rdat(i) = real(cData(i,j))
          enddo
          call xywrite(lrOut,j,rdat)
        enddo
      endif
c
c  Output the imaginary file.
c
      if (liOut.ne.0) then
        do j = 1, n2
          do i = 1, n1
            rdat(i) = aimag(cData(i,j))
          enddo
          call xywrite(liOut,j,rdat)
        enddo
      endif
c
c  Output the magnitude file.
c
      if (lMag.ne.0) then
        do j = 1, n2
          do i = 1, n1
            rdat(i) = abs(cData(i,j))
          enddo
          call xywrite(lMag,j,rdat)
        enddo
      endif
c
c  Output the phase file.
c
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
