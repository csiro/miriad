c***********************************************************************
c
c  A set of routines to perform convolutions of two images.
c
c  User callable routines are:
c    CnvlIniF
c    CnvlIniA
c    CnvlIniG
c    CnvlCopy
c    CnvlCo
c    CnvlA
c    CnvlF
c    CnvlR
c    CnvlFin
c    CnvlExt
c
c  History:
c    rjs  10sep91 Adapted from the Convl and ConvlC routines. This,
c                 hopefully, includes all the functionality of these.
c    rjs   1oct91 Changes to the CnvlCorr routine (renamed it CnvlCo
c                 for a start.
c    rjs  31oct94 Added CnvlExt, plus some comments.
c    mhw  27oct11  Use ptrdiff type for memory allocations
c
c $Id: cnvl.for,v 1.5 2012/06/13 00:44:40 wie017 Exp $

c***********************************************************************
c* CnvlIniF -- Ready a beam, from a file, for a convolution operation.
c& rjs
c: convolution,FFT
c+
      subroutine CnvlIniF(handle,lu,n1,n2,ic,jc,param,flags)
c
      ptrdiff handle
      integer lu,n1,n2,ic,jc
      character flags*(*)
      real param
c
c  This routine readys a beam pattern for a series of convolution
c  operations.  In particular, it reads a beam from a Miriad image file,
c  performs an FFT on it, and stores the result in memory.  It returns a
c  handle which points to the structure containing the needed
c  information.
c
c  Input:
c    lu         Handle of the beam file.
c    n1,n2      Size of the beam. Need not be powers of two.
c    ic,jc      Central pixel of the beam.
c    param      A real parameter value. Its use is determined by the
c               "flags" argument.
c    flags      Extra processing flags, each character indicating
c               another processing option.  Possible values are:
c                 's'   Assume beam is symmetric.
c                 'p'   Param is the value of a Prussian hat to add to
c                       the beam.
c                 'd'   Perform deconvolution (rather than convolution).
c                       Param is the nominal error in the beam.
c                 'e'   Double the size of the beam (through zero
c                       padding) to avoid various edge effects.
c  Output:
c    handle     Handle to the transformed beam.
c--
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
      integer n1d,n2d,space,xr,yr
      ptrdiff Trans,CDat1,CDat2
c
c  Initalise.
c
      call CnvlIn0(handle,n1,n2,n1d,n2d,space,
     *    Trans,CDat1,CDat2,flags,ic,jc,xr,yr)
c
c  Calculate the transform of the beam.
c
      call Cnvl1a(lu,memR(Trans),memR(CDat1),memR(CDat2),n1,n2,n1d,xr)
      call CnvlIn2(memR(Handle+6),memR(Trans),memR(CDat1),memR(CDat2),
     *  n1d,n2,n1d,n2d,yr,flags,param)
c
c  All said and done. Release allocated memory, and return.
c
      call MemFrep(Trans,space,'r')
      end

c***********************************************************************
c* CnvlIniA -- Transform the "beam", ready for a convolution operation.
c& rjs
c: convolution,FFT
c+
      subroutine CnvlIniA(handle,array,n1,n2,ic,jc,param,flags)
c
      ptrdiff handle
      integer n1,n2,ic,jc
      character flags*(*)
      real param,array(n1,n2)
c
c  This routine readys a beam pattern for a series of convolution
c  operations.  In particular, it performs an FFT on the beam, and
c  stores the result in memory.  It returns a handle which points to the
c  structure containing the needed information.
c
c  Input:
c    array      Array containing the beam.
c    n1,n2      Size of the beam. Need not be powers of two.
c    ic,jc      Central pixel of the beam.
c    param      A real parameter value. Its use is determined by the
c               "flags" argument.
c    flags      Extra processing flags, each character indicating
c               another processing option.  Possible values are:
c                 's'   Assume beam is symmetric.
c                 'p'   Param is the value of a Prussian hat to add to
c                       the beam.
c                 'd'   Perform deconvolution (rather than convolution).
c                       Param is the nominal error in the beam.
c                 'e'   Double the size of the beam (through zero
c                       padding) to avoid various edge effects.
c  Output:
c    handle     Handle to the transformed beam.
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
      integer n1d,n2d,space,xr,yr
      ptrdiff Trans,CDat1,CDat2
c
c  Initalise.
c
      call CnvlIn0(handle,n1,n2,n1d,n2d,space,
     *    Trans,CDat1,CDat2,flags,ic,jc,xr,yr)
c
c  Calculate the transform of the beam.
c
      call Cnvl1b(Array,memR(Trans),memR(CDat1),n1,n2,n1d,xr)
      call CnvlIn2(memR(Handle+6),memR(Trans),memR(CDat1),memR(CDat2),
     *  n1d,n2,n1d,n2d,yr,flags,param)
c
c  All said and done. Release allocated memory, and return.
c
      call MemFrep(Trans,space,'r')
      end

c***********************************************************************
c* CnvlIniG -- Construct a Gaussian beam for a convolution operation.
c& mrc
c: convolution,FFT
c+
      subroutine CnvlIniG (handle, n1, n2, ref1, ref2,
     *                 bmaj, bmin, bpa, cdelt1, cdelt2)
c
      ptrdiff handle
      integer n1, n2, ref1, ref2
      real    bmaj, bmin, bpa
      double precision cdelt1, cdelt2
c
c  This routine constructs the Fourier transform of a Gaussian beam
c  pattern intended for use in convolution operations.  It computes the
c  result directly, without doing an FFT, using the fact that the
c  Fourier transform of a Gaussian is another Gaussian.  When the FWHM
c  of the Gaussian convolution function is much less than the image cell
c  spacing, it is important to do it this way rather than via CnvlIniA
c  for which the beam may not be adequately sampled.  The result is
c  stored in memory for which a handle is returned.
c
c  Input:
c    n1,n2      Size of the beam.  Need not be powers of two.
c    ref1,ref2  Central pixel of the beam.
c    bmaj,bmin  Gaussian major and minor FWHM (radian).
c    bpa        Gaussian position angle (deg).
c    cdelt1     Map cell spacing (radian).
c    cdelt2     Map cell spacing (radian).
c  Output:
c    handle     Handle to the transformed Gaussian.
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      include 'mirconst.h'

      ptrdiff Trans, CDat1, CDat2, k
      integer i, i1, i2, n1d, n2d, space, xr, yr
      real    amaj, amin, c2, fdelt1, fdelt2, fmaj, fmin,
     *        r, s, s2, scl, sxx, syy, sxy, t

c-----------------------------------------------------------------------
c     Initalize.
      call CnvlIn0(handle,n1,n2,n1d,n2d,space,
     *    Trans,CDat1,CDat2,'s',ref1,ref2,xr,yr)
      call MemFrep (Trans, space, 'r')

c     Parameters of the convolving Gaussian.
      amaj = 4.0*log(2.0) / (bmaj*bmaj)
      amin = 4.0*log(2.0) / (bmin*bmin)

c     Parameters of the Fourier-transform of the convolving Gaussian.
      fmaj = pi*pi / amaj
      fmin = pi*pi / amin
      scl  = pi / sqrt(amaj*amin)

      c2  = cos(2.0*bpa*D2R)
      s2  = sin(2.0*bpa*D2R)
      sxx = 0.5 * (fmaj*(1.0 - c2) + fmin*(1.0 + c2))
      syy = 0.5 * (fmin*(1.0 - c2) + fmaj*(1.0 + c2))
      sxy = (fmaj-fmin)*s2

c     Apply pixel spacing in the Fourier domain.
      fdelt1 = 1.0 / (n1d * cdelt1)
      fdelt2 = 1.0 / (n2d * cdelt2)
      sxx = sxx * (fdelt1*fdelt1)
      syy = syy * (fdelt2*fdelt2)
      sxy = sxy * (fdelt1*fdelt2)
      scl = scl * abs(fdelt1*fdelt2)

c     Generate the Fourier transform of the convolving Gaussian noting
c     that the array is transposed.
      k = handle + 6
      do i1 = 0, n1d/2
        r = sxx*i1*i1
        s = sxy*i1
        do i2 = 0, n2d-1
          i = i2
          if (i.ge.n2d/2) i = i2 - n2d
          t = r + (s + syy*i)*i

          if (t.gt.20.0) then
            memR(k) = 0.0
          else
            memR(k) = scl * exp(-t)
          endif

          k = k + 1
        enddo
      enddo

      end

c***********************************************************************
c* CnvlCopy -- Form the copy of the FFT of a beam.
c& rjs
c: convolution,FFT
c+
      subroutine CnvlCopy(out,handle)
c
      ptrdiff out,handle
c
c  This forms a new handle, representing the FFT of a beam. The output
c  beam is a copy of the input beam.
c
c  Input:
c    handle     The handle of the input beam.
c  Output:
c    out        The handle of the output beam, which is a copy of the
c               input beam.
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
      integer n1,n2,space
      logical sym
c
c  Determine whats what.
c
      n1 = nint(memR(Handle+2))
      n2 = nint(memR(Handle+3))
      sym = nint(memR(Handle+4)).ne.0
c
c  Allocate the memory.
c
      space = (n1+2)*n2
      if(sym) space = space/2
      space = space + 6
      call MemAllop(out,space,'r')
c
c  Now copy it across.
c
      call CnvlCpy(memR(handle),memR(out),space)
      end

c***********************************************************************
c* CnvlCo -- Correlate two beam patterns.
c& rjs
c: convolution,FFT
c+
      subroutine CnvlCo(handle1,handle2,flags)
c
      ptrdiff handle1,handle2
      character flags*(*)
c
c  Convolve or Correlate two beam patterns. The result is overwrites
c  handle1.
c
c  In the transform domain, this is equivalent to
c
c    Beam1 = Beam1 * conjg(Beam2)       (correlation)
c     or
c    Beam1 = Beam1 * Beam2              (convolution).
c
c  Input:
c    handle1    The beam handle of the input/output beam pattern.
c    handle2    The beam handle of the other input beam.
c    flags      Extra processing flags, each character indicating
c               another processing option.  Possible values are:
c                 'x'   Perform correlation. The default is to perform
c                       convolution.
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
      integer n1,n2,n
      logical sym1,sym2,corr
c
      n1 = nint(memR(Handle1+2))
      n2 = nint(memR(Handle1+3))
      if(nint(memR(Handle2+2)).ne.n1.or.nint(memR(Handle2+3)).ne.n2)
     *  call bug('f','Cannot handle different sized beams')
c
      sym1 = nint(memR(Handle1+4)).ne.0
      sym2 = nint(memR(Handle2+4)).ne.0
      corr = index(flags,'x').ne.0
c
      n = (n1/2+1)*n2
      if(sym2)then
        if(sym1)then
          call CnvlMRR(memR(Handle1+6),memR(Handle2+6),n)
        else
          call CnvlMCR(memR(Handle1+6),memR(Handle2+6),n)
        endif
      else
        if(sym1)then
          call bug('f',
     *        'Correlating symmetric by asymmetric not implemented')
        else if(corr)then
          call CnvlMCCc(memR(Handle1+6),memR(Handle2+6),n)
        else
          call CnvlMCC(memR(Handle1+6),memR(Handle2+6),n)
        endif
      endif
c
c  Scale the output beam to the correct value.
c
      if(.not.sym1)n = n + n
      call CnvlScal(real(n1*n2),memR(Handle1+6),n)
c
      end

c***********************************************************************
      subroutine CnvlIn0(handle,n1,n2,n1d,n2d,space,
     *        Trans,CDat1,CDat2,flags,ic,jc,xr,yr)
c
      ptrdiff handle,Trans,CDat1,CDat2
      integer space,ic,jc,xr,yr,n1,n2,n1d,n2d
      character flags*(*)
c
c  Initialise ready for getting the transform of the beam.
c
c  Input:
c    n1,n2
c    flags
c  Output:
c    n1d,n2d    Size of the convolution array. This is the powers of
c               two greater of equal to n1,n2.
c    space      Space to free up, pointer to by Trans.
c    handle     The handle for the output beam.
c    Trans,CDat1,CDat2 Pointers to work arrays.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
      logical sym
c
c  Externals.
c
      integer nextpow2
c
      n1d = nextpow2(n1)
      n2d = nextpow2(n2)
      sym = index(flags,'s').ne.0
c
c  Double the size of the convolution area, if needed.
c
      if(index(flags,'e').ne.0)then
        n1d = n1d + n1d
        n2d = n2d + n2d
      endif
c
c  Allocate and initialise space for the FFT of the beam.
c
      space = (n1d+2)*n2d
      if(sym) space = space/2
      space = space + 6
      call MemAllop(handle,space,'r')
      memR(handle)   = n1
      memR(handle+1) = n2
      memR(handle+2) = n1d
      memR(handle+3) = n2d
      memR(handle+4) = 0
      if(sym) memR(handle+4) = 1
c
c  Allocate space to transform the beam.
c
      space = (n1d+2)*n2 + 4*max(n1d,n2d)
      call MemAllop(Trans,space,'r')
      CDat1 = Trans + (n1d+2)*n2
      CDat2 = CDat1 + 2*max(n1d,n2d)
c
c  Determine the shift to apply to the beam.
c
      if(ic.lt.1.or.ic.gt.n1.or.jc.lt.1.or.jc.gt.n2)
     *  call bug('f','Beam center is outside the beam')
      xr = mod(n1d-ic+1, n1d)
      yr = mod(n2d-jc+1, n2d)
c
      end

c***********************************************************************
      subroutine CnvlIn2(Beam,Trans,CDat1,CDat2,n1,n2,n1d,n2d,yr,
     *                                                flags,param)
c
      integer n1,n2,n1d,n2d,yr
      complex Trans(n1/2+1,n2),CDat1(n2d),CDat2(n2d)
      character flags*(*)
      real Beam(*),param
c
c  Perform the second pass of the beam FFT.
c
c-----------------------------------------------------------------------
      integer i,j,n,j1,j2,k
      real param2,temp,factor
      logical sym,phat,deconv
c
c  Various constants.
c
      temp = 1/real(n1d*n2d)
      param2 = param*param
      sym    = index(flags,'s').ne.0
      phat   = index(flags,'p').ne.0.and.param.ne.0
      deconv = index(flags,'d').ne.0
c
c  Zero out some locations that are never otherwise assigned to.
c
      n = max(yr+n2+1-n2d,1)
      do j=n,yr
        CDat1(j) = (0.,0.)
      enddo
      do j=yr+n2+1,n2d
        CDat1(j) = 0
      enddo
c
      n = min(n2d-yr,n2)
      j1 = yr
      j2 = yr - n2d
c
c  Extract a column, place it so that it is centered on pixel j=1, and
c  FFT it.
c
      k = 1
      do i=1,n1d/2+1
        do j=1,n
          Cdat1(j+j1) = Trans(i,j)
        enddo
        do j=n+1,n2
          Cdat1(j+j2) = Trans(i,j)
        enddo
        call fftcc(CDat1,CDat2,-1,n2d)
c
c  If its deconvolution, then work out the Weiner filter.
c
        if(deconv)then
          do j=1,n2d
            factor = real(CDat2(j))**2 + aimag(CDat2(j))**2 + param2
            CDat2(j) = conjg(CDat2(j)) / factor
          enddo
        endif
c
c  If there is a Prussian hat to be added, add it.
c
        if(phat)then
          do j=1,n2d
            CDat2(j) = CDat2(j) + param
          enddo
        endif
c
c  Store the FFTed beam, either in a real or a complex array.
c
        if(sym)then
          call CnvlCpyR(temp,Cdat2,Beam(k),n2d)
          k = k + n2d
        else
          call CnvlCpyC(temp,CDat2,Beam(k),n2d)
          k = k + n2d + n2d
        endif
      enddo
c
      end

c***********************************************************************
c* CnvlA -- Convolve the beam with a subimage.
c& rjs
c: convolution,FFT
c+
      subroutine CnvlA(handle,in,nx,ny,out,flags)
c
      ptrdiff handle 
      integer nx,ny
      character flags*(*)
      real in(nx*ny),out(*)
c
c  This convolves, or correlates, an image by a beam pattern.  The input
c  image is passed in as an array. The beam pattern is passed in as a
c  handle as previously returned by either the CnvlIniA or CnvlIniF
c  routines.
c
c  The output image can either be the same size as the input image (see
c  flag='c' below), or the same size as the previously input beam.  In
c  the latter case, the output image is centered in the middle of the
c  output array.  In particular, if the beam is of size n1 x n2, and the
c  input image is of size nx x ny, then the output image will be of size
c  n1 x n2, and pixel (i,j) in the input will correspond to pixel
c  (x0+i,y0+j) in the output, where
c       x0 = n1/2 - nx/2
c       y0 = n2/2 - ny/2
c
c  Input:
c    handle     Handle of the beam, previously returned by CnvlIniA or
c               CnvlIniF.
c    in         Input image to convolve.
c    nx,ny      Image size.
c    flags      Extra processing options:
c                'c'    The output is the same size as the input.  The
c                       default is the output image is the size of the
c                       beam.
c                'x'    Correlate with the beam.  The default is to
c                       convolve with the beam.
c  Output:
c    out        The convolved image.
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
      integer n1,n2,n1a,n2a,n1d,n2d,xr,yr,space
      ptrdiff Trans,CDat1,CDat2
      logical sym,compr,corr
c
c  Initialise.
c
      call Cnvl0(handle,nx,ny,n1,n2,n1a,n2a,n1d,n2d,space,
     *  Trans,CDat1,CDat2,flags,sym,compr,corr,xr,yr)
c
c  Do the first pass.
c
      call Cnvl1b(In,memR(Trans),memR(CDat1),nx,ny,n1d,xr)
c
c  Do the second pass, where you do the multiplication.c
c
      if(compr)then
        n1a = nx
        n2a = ny
      else
        n1a = n1
        n2a = n2
      endif
      call Cnvl2(memR(Handle+6),memR(Trans),memR(CDat1),memR(CDat2),
     *  n1d,ny,n2a,n2d,yr,sym,corr)
c
c  All we have to do now is the final FFT pass, and return the
c  desired array.
c
      call Cnvl3a(memR(Trans),memR(CDat1),Out,n1a,n2a,n1d)
c
c  Bring home the bacon.
c
      call MemFrep(Trans,space,'r')
      end

c***********************************************************************
c* CnvlF -- Convolve the beam with a subimage (the latter being a file).
c& rjs
c: convolution,FFT
c+
      subroutine CnvlF(handle,lu,nx,ny,out,flags)
c
      ptrdiff handle
      integer lu,nx,ny
      character flags*(*)
      real out(*)
c
c  This convolves, or correlates, an image by a beam pattern.  The input
c  image is in a Miriad image file. The beam pattern is passed in as a
c  handle as previously returned by either the CnvlIniA or CnvlIniF
c  routines.
c
c  The output image can either be the same size as the input image (see
c  flag='c' below), or the same size as the previously input beam.  In
c  the latter case, the output image is centered in the middle of the
c  output array.  In particular, if the beam is of size n1 x n2, and the
c  input image is of size nx x ny, then the output image will be of size
c  n1 x n2, and pixel (i,j) in the input will correspond to pixel
c  (x0+i,y0+j) in the output, where
c       x0 = n1/2 - nx/2
c       y0 = n2/2 - ny/2
c
c  Input:
c    handle     Handle of the beam, previously returned by CnvlIniA or
c               CnvlIniF.
c    lu         Handle of the input Miriad image file.
c    nx,ny      Image size.
c    flags      Extra processing options:
c                'c'    The output is the same size as the input.  The
c                       default is the output image is the size of the
c                       beam.
c                'x'    Correlate with the beam.  The default is to
c                       convolve with the beam.
c  Output:
c    out        The convolved image.
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
      integer n1,n2,n1a,n2a,n1d,n2d,xr,yr,space
      ptrdiff Trans,CDat1,CDat2
      logical sym,compr,corr
c
c  Initialise.
c
      call Cnvl0(handle,nx,ny,n1,n2,n1a,n2a,n1d,n2d,space,
     *  Trans,CDat1,CDat2,flags,sym,compr,corr,xr,yr)
c
c  Do the first pass.
c
      call Cnvl1a(lu,memR(Trans),memR(CDat1),memR(CDat2),nx,ny,n1d,xr)
c
c  Do the second pass, where you do the multiplication.c
c
      if(compr)then
        n1a = nx
        n2a = ny
      else
        n1a = n1
        n2a = n2
      endif
      call Cnvl2(memR(Handle+6),memR(Trans),memR(CDat1),memR(CDat2),
     *  n1d,ny,n2a,n2d,yr,sym,corr)
c
c  All we have to do now is the final FFT pass, and return the
c  desired array.
c
      call Cnvl3a(memR(Trans),memR(CDat1),Out,n1a,n2a,n1d)
c
c  Bring home the bacon.
c
      call MemFrep(Trans,space,'r')
      end

c***********************************************************************
c* CnvlR -- Convolve the beam with a subimage (in "runs" format).
c& rjs
c: convolution,FFT
c+
      subroutine CnvlR(handle,in,nx,ny,runs,nRuns,out,flags)
c
      ptrdiff handle
      integer nx,ny,nruns,runs(3,nruns)
      character flags*(*)
      real in(*),out(*)
c
c  This convolves, or correlates, an image by a beam pattern.  The input
c  image is in an array in "runs" format. The beam pattern is passed in
c  as a handle as previously returned by either the CnvlIniA or CnvlIniF
c  routines.
c
c  The output image can either be in runs format (see flag='c' below),
c  or the same size as the previously input beam. In the latter case,
c  the output image is centered in the middle of the output array.  In
c  particular, if the beam is of size n1 x n2, and the input image is of
c  size nx x ny, then the output image will be of size n1 x n2, and
c  pixel (i,j) in the input will correspond to pixel (x0+i,y0+j) in the
c  output, where
c       x0 = n1/2 - nx/2
c       y0 = n2/2 - ny/2
c
c  Input:
c    handle     Handle of the beam, previously returned by CnvlIniA or
c               CnvlIniF.
c    in         Input pixels to convolve.
c    nx,ny      Image size.
c    runs       Runs of the image.
c    nRuns      The number of runs.
c    flags      Extra processing options:
c                'c'    The output is the same size as the input. The
c                       default is the output image is the size of the
c                       beam.
c                'x'    Correlate with the beam. The default is to
c                       convolve with the beam.
c  Output:
c    out        The convolved image. This can be either in runs format,
c               or an array the size of the beam (depending on the 'x'
c               flag).
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
      integer n1,n2,n1a,n2a,n1d,n2d,xr,yr,space
      ptrdiff Trans,CDat1,CDat2
      logical sym,compr,corr
c
c  Initialise.
c
      call Cnvl0(handle,nx,ny,n1,n2,n1a,n2a,n1d,n2d,space,
     *  Trans,CDat1,CDat2,flags,sym,compr,corr,xr,yr)
c
c  Do the first pass.
c
      call Cnvl1c(In,memR(Trans),memR(CDat1),nx,ny,n1d,Runs,nruns,xr)
c
c  Do the second pass, where you do the multiplication.
c
      call Cnvl2(memR(Handle+6),memR(Trans),memR(CDat1),memR(CDat2),
     *  n1d,ny,n2a,n2d,yr,sym,corr)
c
c  All we have to do now is the final FFT pass, and return the
c  desired array.
c
      if(compr)then
        call Cnvl3b(memR(Trans),memR(CDat1),Out,n1a,n2a,n1d,Runs,nruns)
      else
        call Cnvl3a(memR(Trans),memR(CDat1),Out,n1a,n2a,n1d)
      endif
c
c  Bring home the bacon.
c
      call MemFrep(Trans,space,'r')
      end

c***********************************************************************
c* CnvlExt -- Determine the extent of the point-spread function, etc.
c& rjs
c: convolution,FFT
c+
      subroutine CnvlExt(handle,n1,n2,n1d,n2d)
c
      ptrdiff handle
      integer n1,n2,n1d,n2d
c
c  Return the size of the point-spread function, and the maximum sized
c  input image that can be handled.
c
c  Input:
c    handle     Handle of the beam, previously returned by CnvlIniA or
c               CnvlIniF.
c  Output:
c    n1,n2      Size of the point-spread function.
c    n1d,n2d    Maximum sized input image that the CNVL routines can
c               cope with for this beam
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
c  Extract beam info.
c
      n1 = nint(memR(Handle  ))
      n2 = nint(memR(Handle+1))
      n1d = nint(memR(Handle+2))
      n2d = nint(memR(Handle+3))
c
      end

c***********************************************************************
      subroutine Cnvl0(handle,nx,ny,n1,n2,n1a,n2a,n1d,n2d,space,
     *  Trans,CDat1,CDat2,flags,sym,compr,corr,xr,yr)
c
      integer nx,ny,n1,n2,n1a,n2a,n1d,n2d,xr,yr,space
      ptrdiff handle,Trans,CDat1,CDat2
      character flags*(*)
      logical sym,compr,corr
c
c  Get parameters and memory needed by the convolution routine.
c
c  Input:
c    handle     The handle of the input beam.
c    nx,ny      Size of the image.
c    flags      Flags passed to the convolution routine.
c  Output:
c    n1,n2      Size of the beam.
c    n1d,n2d    Full size of the beam.
c    sym        True if the beam is symmetric.
c    compr      True if the output image is to be in compressed form.
c    corr       True if we are to correlate the beam with the image.
c    xoff,yoff  Right shift to apply to the image in x and y.
c    Trans,CDat1,CDat2 Pointers to scratch areas.
c    space      Space to dealloca with the MemFree routine.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
c  Return the various flags.
c
      corr  = index(flags,'x').ne.0
      compr = index(flags,'c').ne.0
c
c  Extract beam info.
c
      n1 = nint(memR(Handle  ))
      n2 = nint(memR(Handle+1))
      n1d = nint(memR(Handle+2))
      n2d = nint(memR(Handle+3))
      if(nx.gt.n1d.or.ny.gt.n2d)
     *  call bug('f','Image being convolved is larger than the beam')
      sym = nint(memR(Handle+4)).gt.0
      if(compr)then
        xr = 0
        yr = 0
      else
        xr = n1/2 - nx/2
        yr = n2/2 - ny/2
      endif
c
c  Determine the size of the output.
c
      if(compr)then
        n1a = nx
        n2a = ny
      else
        n1a = n1
        n2a = n2
      endif
c
c  Allocate work space.
c
      if(compr)then
        space = (n1d+2)*ny + 4*max(n1d,n2d)
      else
        space = (n1d+2)*n2 + 4*max(n1d,n2d)
      endif
c
      call MemAllop(Trans,space,'r')
      if(compr)then
        CDat1 = Trans + (n1d+2)*ny
      else
        CDat1 = Trans + (n1d+2)*n2
      endif
      CDat2 = CDat1 + 2*max(n1d,n2d)
      end

c***********************************************************************
      subroutine Cnvl1a(lu,Trans,Dat1,Dat2,n1,n2,n1d,xr)
c
      integer lu,n1,n2,n1d,xr
      complex Trans(n1d/2+1,n2)
      real Dat1(n1),Dat2(n1d)
c
c  Do the first pass of the transform of an image, the image being in a
c  disk file.
c-----------------------------------------------------------------------
      integer i,j,i1,i2,n
c
c  Do the first pass of reading the image, shifting it and transforming
c  it.
c
      n = max(xr+n1+1-n1d,1)
      do i=n,xr
        Dat2(i) = 0
      enddo
      do i=xr+n1+1,n1d
        Dat2(i) = 0
      enddo
c
      n = min(n1d-xr,n1)
      i1 = xr
      i2 = xr - n1d
c
      do j=1,n2
        if(n.eq.n1)then
          call xyread(lu,j,Dat2(1+i1))
        else
          call xyread(lu,j,Dat1)
          do i=1,n
            Dat2(i+i1) = Dat1(i)
          enddo
          do i=n+1,n1
            Dat2(i+i2) = Dat1(i)
          enddo
        endif
        call fftrc(Dat2,Trans(1,j),-1,n1d)
      enddo
c
      end

c***********************************************************************
      subroutine Cnvl1b(Array,Trans,Dat,n1,n2,n1d,xr)
c
      integer n1,n2,n1d,xr
      complex Trans(n1d/2+1,n2)
      real Array(n1,n2),Dat(n1d)
c
c  Do the first pass of the transform of an image, the image being in
c  memory.
c-----------------------------------------------------------------------
      integer i,j,i1,i2,n
c
c  Do the first pass of reading the image, shifting it and transforming
c  it.
c
      n = max(xr+n1+1-n1d,1)
      do i=n,xr
        Dat(i) = 0
      enddo
      do i=xr+n1+1,n1d
        Dat(i) = 0
      enddo
c
      n = min(n1d-xr,n1)
      i1 = xr
      i2 = xr - n1d
c
      do j=1,n2
        if(n.eq.n1d)then
          call fftrc(Array(1,j),Trans(1,j),-1,n1d)
        else
          do i=1,n
            Dat(i+i1) = Array(i,j)
          enddo
          do i=n+1,n1
            Dat(i+i2) = Array(i,j)
          enddo
          call fftrc(Dat,Trans(1,j),-1,n1d)
        endif
      enddo
c
      end

c***********************************************************************
      subroutine Cnvl1c(Array,Trans,Dat,n1,n2,n1d,Runs,nruns,xr)
c
      integer n1,n2,n1d,xr,nruns
      integer Runs(3,nRuns+1)
      complex Trans(n1d/2+1,n2)
      real Array(*),Dat(n1d)
c
c  Do the first pass of the transform of an image, the image being in
c  memory in "runs" format.
c
c-----------------------------------------------------------------------
      integer i,j,i1,i2,k,n,nd,nzero,pnt
c
c  Do the first pass of reading the image, shifting it and transforming
c  it.
c
      n = max(xr+n1+1-n1d,1)
      do i=n,xr
        Dat(i) = 0
      enddo
      do i=xr+n1+1,n1d
        Dat(i) = 0
      enddo
c
      n = min(n1d-xr,n1)
      i1 = xr
      i2 = xr - n1d
c
      pnt = 1
      k = 1
c
      do j=1,n2
        nzero = 0
        dowhile(k.le.nruns.and.runs(1,k).eq.j)
c
c  Zero up to element runs(2,k)-1.
c
          nd = min(runs(2,k)-1,n)
          do i=nzero+1,nd
            Dat(i+i1) = 0
          enddo
          do i=nd+1,runs(2,k)-1
            Dat(i+i2) = 0
          enddo
c
c  Copy the data across for elements runs(2,k) to runs(3,k).
c
          nd = min(runs(3,k),n)
          do i=runs(2,k),nd
            Dat(i+i1) = Array(pnt)
            pnt = pnt + 1
          enddo
          do i=nd+1,runs(3,k)
            Dat(i+i2) = Array(pnt)
            pnt = pnt + 1
          enddo
c
c  Update the number of elements processed.
c
          nzero = runs(3,k)
          k = k + 1
        enddo
c
c  If this is a completely zeroed row, just copy zeros to the Trans
c  array.
c
        if(nzero.eq.0)then
          do i=1,n1d
            Trans(i,j) = (0.,0.)
          enddo
c
c  Otherwise this is a row with some data. Finish zeroing it, and
c  FFT it.
c
        else
          nd = min(n1,n)
          do i=nzero+1,nd
            Dat(i+i1) = 0
          enddo
          do i=nd+1,n1
            Dat(i+i2) = 0
          enddo
          call fftrc(Dat,Trans(1,j),-1,n1d)
        endif
      enddo
c
      end

c***********************************************************************
      subroutine Cnvl2(Beam,Trans,CDat1,CDat2,n1,n2,n2a,n2d,yr,
     *  sym,corr)
c
      integer n1,n2,n2a,n2d,yr
      complex Trans(n1/2+1,n2a),CDat1(n2d),CDat2(n2d)
      logical sym,corr
      real Beam(*)
c
c  Perform the second and third FFTs, returning the result in Trans.
c-----------------------------------------------------------------------
      integer i,j,k,j1,j2,n,n0
c
c  Zero out some locations that are never otherwise assigned to.
c
      n0 = max(yr+n2+1-n2d,1)
      n = min(n2d-yr,n2)
      j1 = yr
      j2 = yr - n2d
c
c  Copy the array across.
c
      k = 1
      do i=1,n1/2+1
c
c  Zero out the array to start with.
c
        do j=n0,yr
          CDat1(j) = (0.,0.)
        enddo
        do j=yr+n2+1,n2d
          CDat1(j) = 0
        enddo
c
        do j=1,n
          CDat1(j+j1) = Trans(i,j)
        enddo
        do j=n+1,n2
          CDat1(j+j2) = Trans(i,j)
        enddo
c
c  FFT it.
c
        call fftcc(CDat1,CDat2,-1,n2d)
c
c  Multiply by the beam.
c
        if(sym)then
          call CnvlMCR(CDat2,Beam(k),n2d)
          k = k + n2d
        else if(corr)then
          call CnvlMCCc(CDat2,Beam(k),n2d)
          k = k + n2d + n2d
        else
          call CnvlMCC(CDat2,Beam(k),n2d)
          k = k + n2d + n2d
        endif
c
c  Inverse FFT.
c
        call fftcc(CDat2,CDat1,+1,n2d)
c
c  Copy back to the Trans array now.
c
        do j=1,n2a
          Trans(i,j) = CDat1(j)
        enddo
      enddo
      end

c***********************************************************************
      subroutine Cnvl3a(Trans,Dat,Out,n1,n2,n1d)
c
      integer n1,n2,n1d
      complex Trans(n1d/2+1,n2)
      real Out(n1,n2),Dat(n1d)
c
c  The final Fourier transform of the rows to get the convolved image.
c
c-----------------------------------------------------------------------
      integer i,j
c
      do j=1,n2
        if(n1.ne.n1d)then
          call fftcr(Trans(1,j),Dat,+1,n1d)
          do i=1,n1
            Out(i,j) = Dat(i)
          enddo
        else
          call fftcr(Trans(1,j),Out(1,j),+1,n1d)
        endif
      enddo
c
      end

c***********************************************************************
      subroutine Cnvl3b(Trans,Dat,Out,n1,n2,n1d,Runs,nruns)
c
      integer n1,n2,n1d
      integer nruns,Runs(3,nruns+1)
      complex Trans(n1d/2+1,n2)
      real Out(*),Dat(n1d)
c
c  The final Fourier transform of the rows to get the convolved image,
c  storing the convolved image in "runs" form.
c
c-----------------------------------------------------------------------
      integer i,j,k,j0,pnt
c
      pnt = 1
      j0 = 0
      do k=1,nruns
        j = runs(1,k)
        if(j.ne.j0)then
          if(runs(2,k).eq.1.and.runs(3,k).eq.n1d)then
            call fftcr(Trans(1,j),Out(pnt),+1,n1d)
            pnt = pnt + n1d
          else
            call fftcr(Trans(1,j),Dat,+1,n1d)
            j0 = j
          endif
        endif
        if(j.eq.j0)then
          do i=Runs(2,k),Runs(3,k)
            Out(pnt) = Dat(i)
            pnt = pnt + 1
          enddo
        endif
      enddo
c
      end

c***********************************************************************
c* CnvlFin -- Release the storage associated with a transformed "beam".
c& rjs
c: convolution,FFT
c+
      subroutine CnvlFin(handle)
c
      ptrdiff handle
c
c  Input:
c  Output:
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
c
      integer n1d,n2d,space
      logical sym
c
      n1d = nint(memR(handle+2))
      n2d = nint(memR(handle+3))
      sym = nint(memR(handle+4)).gt.0
c
      space = (n1d+2)*n2d
      if(sym) space = space/2
      space = space + 6
      call MemFrep(handle,space,'r')
      end

c***********************************************************************
      subroutine CnvlCpy(in,out,n)
c
      integer n
      real in(n),out(n)
c
c  Copy a real valued array to a real valued array.
c-----------------------------------------------------------------------
      integer i
c
      do i=1,n
        out(i) = in(i)
      enddo
c
      end

c***********************************************************************
      subroutine CnvlScal(factor,in,n)
c
      integer n
      real factor,in(n)
c
c  Scale the input array.
c-----------------------------------------------------------------------
      integer i
c
      do i=1,n
        in(i) = factor * in(i)
      enddo
c
      end

c***********************************************************************
      subroutine CnvlCpyR(factor,in,out,n)
c
      integer n
      complex in(n)
      real out(n),factor
c
c  Move a complex valued array to a real valued array.
c-----------------------------------------------------------------------
      integer i
      do i=1,n
        out(i) = factor * in(i)
      enddo
      end

c***********************************************************************
      subroutine CnvlCpyC(factor,in,out,n)
c
      integer n
      complex in(n)
      complex out(n)
      real factor
c
c  Move a complex valued array to a complex valued array.
c-----------------------------------------------------------------------
      integer i
      do i=1,n
        out(i) = factor * in(i)
      enddo
      end

c***********************************************************************
      subroutine CnvlMCR(in,beam,n)
c
      integer n
      complex in(n)
      real beam(n)
c
c  Move a complex valued array to a real valued array.
c-----------------------------------------------------------------------
      integer i
      do i=1,n
        in(i) = in(i) * beam(i)
      enddo
      end

c***********************************************************************
      subroutine CnvlMRR(in,beam,n)
c
      integer n
      real in(n), beam(n)
c
c  Multiply a real valued array to a real valued array.
c-----------------------------------------------------------------------
      integer i
      do i=1,n
        in(i) = in(i) * beam(i)
      enddo
      end

c***********************************************************************
      subroutine CnvlMCC(in,beam,n)
c
      integer n
      complex in(n), beam(n)
c
c  Multiply a complex valued array to a complex valued array.
c-----------------------------------------------------------------------
      integer i
      do i=1,n
        in(i) = in(i) * beam(i)
      enddo
      end

c***********************************************************************
      subroutine CnvlMCCc(in,beam,n)
c
      integer n
      complex in(n)
      complex beam(n)
c
c  Multiply a complex valued array to a complex valued array.
c-----------------------------------------------------------------------
      integer i
      do i=1,n
        in(i) = in(i) * conjg(beam(i))
      enddo
      end

