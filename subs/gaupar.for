c***********************************************************************
c  History:
c    22nov93 rjs  Original version.
c    13sep94 rjs  Added gaudfac.
c    11aug97 rjs  Protect against atan2(0,0)
c    11dec97 rjs  Handle images in units of Kelvin.
c    25feb98 rjs  Correct bunit shortcoming introduced above.
c    26apr05 tw   Default is Kelvin-like scaling rather than no scaling
c                 in gaupar.
c
c***********************************************************************


c* gaupar1 - Determine effective beam of the convolution of two images.
c& rjs
c: image analysis
c+
        subroutine GauPar1(lIn,bmaj2,bmin2,bpa2,
     *                          bunit,bmaj,bmin,bpa,fac)

        integer lIn
        real bmaj2,bmin2,bpa2,bmaj,bmin,bpa,fac
        character bunit*(*)
c
c  Determine the units and effective beam of the convolution of an
c  image dataset with a gaussian.
c
c  Input:
c    lIn        Handle of the image dataset.
c    bmaj2,bmin2 Gaussian beam major and minor FWHM, in same units as
c               the input dataset.
c    bpa2       Position angle of the gaussian beam, in degrees.
c  Output:
c    bunit      Units of the result (usually JY/BEAM).
c    bmaj,bmin  Effective beam major and minor FWHM.
c    bpa        Effective beam position angle, in degrees.
c    fac        Scale factor to multiply by to correct units.
c--
c-----------------------------------------------------------------------
      double precision dx,dy
      character bunit1*32
      real bmaj1,bmin1,bpa1
c-----------------------------------------------------------------------
c
c  Determine the parameters from the input dataset.
c
      call rdhda(lIn,'bunit',bunit1,'?/PIXEL')
      call rdhdr(lIn,'bmaj',bmaj1,0.)
      call rdhdr(lIn,'bmin',bmin1,0.)
      call rdhdr(lIn,'bpa', bpa1,0.)
      call rdhdd(lIn,'cdelt1',dx,0.d0)
      call rdhdd(lIn,'cdelt2',dy,0.d0)

      call GauPar(bunit1,  dx,dy,bmaj1,bmin1,bpa1,
     *            '?/BEAM',dx,dy,bmaj2,bmin2,bpa2,
     *            bunit,         bmaj, bmin, bpa, fac)

      end


c***********************************************************************
c* gaupar2 - Determine effective beam of the convolution of two images.
c& rjs
c: image analysis
c+
      subroutine GauPar2(lIn1,lIn2,bunit,bmaj,bmin,bpa,fac)

      integer lIn1,lIn2
      real bmaj,bmin,bpa,fac
      character bunit*(*)
c
c  Determine the units and effective beam of the convolution of two
c  image datasets.
c
c  Input:
c    lIn1,lIn2  Handle of the image datasets.
c  Output:
c    bunit      Units of the result (usually JY/BEAM).
c    bmaj,bmin  Effective beam major and minor FWHM.
c    bpa        Effective beam position angle, in degrees.
c    fac        Scale factor to multiply by to correct units.
c--
c-----------------------------------------------------------------------
      double precision dx1,dy1,dx2,dy2
      character bunit1*32,bunit2*32
      real bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2
c-----------------------------------------------------------------------
c
c  Determine the parameters from the first input dataset.
c
      call rdhda(lIn1,'bunit',bunit1,'?/PIXEL')
      call rdhdr(lIn1,'bmaj',bmaj1,0.)
      call rdhdr(lIn1,'bmin',bmin1,0.)
      call rdhdr(lIn1,'bpa', bpa1,0.)
      call rdhdd(lIn1,'cdelt1',dx1,0.d0)
      call rdhdd(lIn1,'cdelt2',dy1,0.d0)
c
c  Determine the parameters from the first input dataset.
c
      call rdhda(lIn2,'bunit',bunit2,'?/PIXEL')
      call rdhdr(lIn2,'bmaj',bmaj2,0.)
      call rdhdr(lIn2,'bmin',bmin2,0.)
      call rdhdr(lIn2,'bpa', bpa2,0.)
      call rdhdd(lIn2,'cdelt1',dx2,0.d0)
      call rdhdd(lIn2,'cdelt2',dy2,0.d0)
c
c  Get the gaussian parameters.
c
      call GauPar(bunit1,dx1,dy1,bmaj1,bmin1,bpa1,
     *            bunit2,dx2,dy2,bmaj2,bmin2,bpa2,
     *            bunit,         bmaj, bmin, bpa, fac)

      end


c***********************************************************************
c* gaupar - Determine effective beam of the convolution of two images.
c& rjs
c: image analysis
c+
      subroutine GauPar(bunit1x,dx1,dy1,bmaj1,bmin1,bpa1,
     *                    bunit2x,dx2,dy2,bmaj2,bmin2,bpa2,
     *                    bunit,bmaj,bmin,bpa,fac)

      character bunit1x*(*),bunit2x*(*),bunit*(*)
      double precision dx1,dy1,dx2,dy2
      real bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2,bmaj,bmin,bpa,fac
c
c  Determine the units and effective beam of the convolution of
c  two images.
c
c  Input:
c    bunit1x    The units of the first image, e.g. JY/PIXEL or JY/BEAM.
c    dx1,dy1    Increment in x and y.
c    bmaj1,bmin1 Beam major and minor FWHM, in same units as dx1,dy1.
c               May be 0.
c    bpa1       Position angle of the beam, in degrees.
c
c    Similarly for bunit2,dx2,dy2,bmaj2,bmin2,bpa2, except that its
c    for the second image.
c
c  Output:
c    bunit      Units of the resultant image, e.g. JY/BEAM.
c    bmaj,bmin  Effective beam major and minor FWHM.
c    bpa        Effective beam position angle.
c    fac        Factor to multipl the result by to convert to correct
c               units.
c--
c-----------------------------------------------------------------------
      logical pPix1,pBem1,pPix2,pBem2,pKel1,pKel2
      integer l,ifail
      character line*64,b1a*32,b1b*32,b2a*32,b2b*32,btemp*32
c
c  Externals.
c
      integer len1
c-----------------------------------------------------------------------
c
c  Set defaults.
c
      btemp = bunit1x
      call ucase(btemp)
      l = index(btemp,'/')
      if(l.gt.1.and.l.lt.len(btemp))then
        b1a = btemp(1:l-1)
        b1b = btemp(l+1:)
      else
        b1a = btemp
        b1b = ' '
      endif
      btemp = bunit2x
      call ucase(btemp)
      l = index(btemp,'/')
      if(l.gt.1.and.l.lt.len(btemp))then
        b2a = btemp(1:l-1)
        b2b = btemp(l+1:)
      else
        b2a = btemp
        b2b = ' '
      endif
      if(b1a.eq.'?')b1a = b2a
      if(b2a.eq.'?')b2a = b1a

      if(b2a.ne.b1a)call bug('w','Inconsistent units')
      bunit = b1a

      bmaj = 0
      bmin = 0
      bpa = 0
      fac = 1
c
c  Determine what are the units of the map and beam.
c
      pPix1 = b1b.eq.'PIXEL'
      pBem1 = b1b(1:4).eq.'BEAM'
      pKel1 = (.not.pPix1).and.(.not.pBem1)
c     pKel1 = b1a.eq.'KELVIN'
c     if(.not.pPix1.and..not.pBem1.and..not.pKel1)then
c       call bug('w','Unknown units for first image ... no scaling')
c       return
c     endif
c
      pPix2 = b2b.eq.'PIXEL'
      pBem2 = b2b.eq.'BEAM'
      pKel2 = b2a.eq.'KELVIN'
      if(.not.pPix2.and..not.pBem2.and..not.pKel2)then
        call bug('w','Unknown units for second image ... no scaling')
        return
      endif
c
c  Check that the pixel increments are the same.
c
      if(abs(dx1-dx2).gt.0.01*min(abs(dx1),abs(dx2)).or.
     *   abs(dy1-dy2).gt.0.01*min(abs(dy1),abs(dy2)))then
        call bug('w','Pixel increments are different ... no scaling')
        return
      endif
c
c  Nothing to do if both the map and the beam are in units of /PIXEL.
c
      l = len1(bunit)
      if(pPix1.and.pPix2)then
        bunit(l+1:) = '/PIXEL'
c
c  One of the other is in /PIXEL.  Effective beam is just the other or
c  one.
c
      else if(pPix1)then
        if(b2b.ne.' ')bunit(l+1:) = '/'//b2b
        bmaj = bmaj2
        bmin = bmin2
        bpa  = bpa2
      else if(pPix2)then
        if(b1b.ne.' ')bunit(l+1:) = '/'//b1b
        bmaj = bmaj1
        bmin = bmin1
        bpa  = bpa1
c
c  The hard one.  Map and beam are in units of /BEAM.  Calculate the
c  effective beam and scale factor.
c
      else if(pBem1.and.pBem2)then
        bunit(l+1:) = '/'//b1b
        if(bmaj1*bmin1.ne.0.and.bmaj1*bmin1.ne.0.and.
     *        dx1*dy1.ne.0)then
          call Gaufac(bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2,
     *        fac,bmaj,bmin,bpa,ifail)
          fac = abs(dx1*dy1/fac)
          call output('Determining the appropriate scale factor ...')
        else
          call bug('w','Bmaj or bmin missing ... no scaling')
        endif

      else if(pBem1.and.pKel2)then
        bunit = 'KELVIN'
        if(bmaj1*bmin1.ne.0.and.bmaj1*bmin1.ne.0.and.
     *        dx1*dy1.ne.0)then
          call Gaufac(bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2,
     *        fac,bmaj,bmin,bpa,ifail)
          fac = abs(dx1*dy1/fac)*abs((bmaj2*bmin2)/(bmaj*bmin))
          call output('Determining the appropriate scale factor ...')
        else
          call bug('w','Bmaj or bmin missing ... no scaling')
        endif

      else if(pKel1.and.pBem2)then
        bunit = bunit1x
        if(bmaj1*bmin1.ne.0.and.bmaj1*bmin1.ne.0.and.
     *        dx1*dy1.ne.0)then
          call Gaufac(bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2,
     *        fac,bmaj,bmin,bpa,ifail)
          fac = abs(dx1*dy1/fac)*abs((bmaj1*bmin1)/(bmaj*bmin))
          call output('Assuming Kelvin-like image units ...')
          call output('Determining the appropriate scale factor ...')
        else
          call bug('w','Bmaj or bmin missing ... no scaling')
        endif
c
c  Both are in Kelvin ... this makes no sense.
c
      else if(pKel1.and.pKel2)then
        call bug('w',
     *    'Map and beam are in units of Kelvin ... no scaling performed')
      endif

      write(line,'(a,1pe10.3)')'Scaling the output by',fac
      call output(line)
      line = 'Output units are '//bunit
      call output(line)
      end


c***********************************************************************
      subroutine Gaufac(bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2,
     *  fac,bmaj,bmin,bpa,ifail)

      integer ifail
      real bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2,fac,bmaj,bmin,bpa
c
c  Determine the Gaussian parameters resulting from convolving
c  two gaussians.
c
c  Input:
c    bmaj1,bmin1        Major and minor FWHM of 1st gaussian.
c    bpa1               Position angle of 1st gaussian, in degrees.
c    bmaj2,bmin2        Major and minor FWHM of 2nd gaussian.
c    bpa2               Position angle of 2nd gaussian, in degrees.
c  Output:
c    ifail              Always 0.
c    fac                Amplitude of resultant gaussian.
c    bmaj,bmin          Major and minor axes of resultant gaussian.
c    bpa                Position angle of the result.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      real alpha,beta,gamma,theta1,theta2,s,t
c-----------------------------------------------------------------------

      theta1 = pi/180. * bpa1
      theta2 = pi/180. * bpa2
      alpha  = (bmaj1*cos(theta1))**2 + (bmin1*sin(theta1))**2 +
     *         (bmaj2*cos(theta2))**2 + (bmin2*sin(theta2))**2
      beta   = (bmaj1*sin(theta1))**2 + (bmin1*cos(theta1))**2 +
     *         (bmaj2*sin(theta2))**2 + (bmin2*cos(theta2))**2
      gamma  = 2 * ( (bmin1**2-bmaj1**2)*sin(theta1)*cos(theta1) +
     *               (bmin2**2-bmaj2**2)*sin(theta2)*cos(theta2) )

      s = alpha + beta
      t = sqrt((alpha-beta)**2 + gamma**2)
      bmaj = sqrt(0.5*(s+t))
      bmin = sqrt(0.5*(s-t))
      if(abs(gamma)+abs(alpha-beta).eq.0)then
        bpa = 0
      else
        bpa = 180 / pi * 0.5*atan2(-gamma,alpha-beta)
      endif
      fac = pi / ( 4.*log(2.)) * bmaj1 * bmin1 * bmaj2 * bmin2 /
     *   sqrt(alpha*beta - 0.25 * gamma * gamma)
      ifail = 0

      end


c***********************************************************************
      subroutine GauDPar1(lIn,bmaj1,bmin1,bpa1,
     *        bmaj,bmin,bpa,fac,ifail)

      integer lIn,ifail
      real bmaj1,bmin1,bpa1,bmaj,bmin,bpa,fac
c-----------------------------------------------------------------------
      real bmaj2,bmin2,bpa2
c-----------------------------------------------------------------------
c
c  Determine the parameters for the first one.
c
      call rdhdr(lIn,'bmaj',bmaj2,0.)
      call rdhdr(lIn,'bmin',bmin2,bmaj2)
      call rdhdr(lIn,'bpa',bpa2,0.)

      call GauDFac(bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2,
     *  fac,bmaj,bmin,bpa,ifail)

      end


c***********************************************************************
      subroutine GauDfac(bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2,
     *  fac,bmaj,bmin,bpa,ifail)

      integer ifail
      real bmaj1,bmin1,bpa1,bmaj2,bmin2,bpa2,bmaj,bmin,bpa,fac
c
c  Determine the parameters of a gaussian deconvolved with another
c  gaussian.
c
c  Input:
c    bmaj1,bmin1        Major and minor FWHM of the source..
c    bpa1               Position angle of 1st gaussian, in degrees.
c    bmaj2,bmin2        Major and minor FWHM of gaussian to deconvolve
c                       with.
c    bpa2               Position angle of 2nd gaussian, in degrees.
c  Output:
c    bmaj,bmin          Major and minor axes of resultant gaussian.
c    bpa                Position angle of the result, in radians.
c    fac                Always 1 (for future use ...).
c    ifail              Success status: 0   All OK.
c                                       1   Result is pretty close to a
c                                           point source.
c                                       2   Illegal result.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      real alpha,beta,gamma,s,t,limit,theta1,theta2
c-----------------------------------------------------------------------
      theta1 = pi/180. * bpa1
      theta2 = pi/180. * bpa2

      alpha  = (bmaj1*cos(theta1))**2 + (bmin1*sin(theta1))**2 -
     *         (bmaj2*cos(theta2))**2 - (bmin2*sin(theta2))**2
      beta   = (bmaj1*sin(theta1))**2 + (bmin1*cos(theta1))**2 -
     *         (bmaj2*sin(theta2))**2 - (bmin2*cos(theta2))**2
      gamma  = 2 * ( (bmin1**2-bmaj1**2)*sin(theta1)*cos(theta1) -
     *               (bmin2**2-bmaj2**2)*sin(theta2)*cos(theta2) )

      s = alpha + beta
      t = sqrt((alpha-beta)**2 + gamma**2)
      limit = min(bmaj1,bmin1,bmaj2,bmin2)
      limit = 0.1*limit*limit
      if(alpha.lt.0.or.beta.lt.0.or.s.lt.t)then
        bmaj = 0
        bmin = 0
        bpa = 0
        if(0.5*(s-t).lt.limit.and.alpha.gt.-limit.and.
     *                             beta.gt.-limit)then
          ifail = 1
        else
          ifail = 2
        endif
      else
        bmaj = sqrt(0.5*(s+t))
        bmin = sqrt(0.5*(s-t))
        if(abs(gamma)+abs(alpha-beta).eq.0)then
          bpa = 0
        else
          bpa  = 180. / pi * 0.5 * atan2(-gamma,alpha-beta)
        endif
        ifail = 0
      endif
      fac = 1

      end
