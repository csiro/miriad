      program smooth
c-----------------------------------------------------------------------
c= SMOOTH - Convolve an image (in the image domain) with a 2-D gaussian
c	or boxcar.
c& nebk
c: analysis
c+
c	SMOOTH is a MIRIAD task which convolves an image by an elliptical
c	gaussian or a boxcar the hard way.   The convolving Gaussian and 
c	boxcar have peaks of unity.  Additional scaling is provided by 
c	the keyword "scale"
c@ in 
c	The input image.  Wild card expansion is supported, no default.
c@ out
c	The output image.  No default.
c@ type
c	Specifies the type of function to convolve by.  Should
c	be one of
c	  "gaussian"     Gaussian at arbitrary position angle 
c	  "boxcar"       Boxcar oriented in x and y directions. Note
c	                 that the full width is rounded up to be an 
c		         odd number of pixels.
c	Default is gaussian.  Minimum match is active. 
c@ fwhm
c	The Gaussian FWHM along the major and minor axes or the boxcar
c	full widths in the x and y directions (all in arcseconds).  
c	The image pixel increments are assumed to be in radians.
c	No default.
c@ pa
c	The position angle in degrees CCW from North of the major axis
c	of the gaussian.  Not used for boxcar smoothing.
c	Default is 0.0.
c@ scale
c	If unset, then SMOOTH will attempt to make the units of the
c	smoothed image be Jy/beam for Gaussian convolution.  If 0.0,
c	then the convolution integral is scaled (multipled) by the 
c	inverse of the volume of the convolving function. Otherwise, 
c	the integral is scaled by "scale"
c@ options
c	"nocheck"   By default, blanked input pixels do not contribute to the
c	   convolution sum.  If you set NOCHECK then blanked input pixels 
c	   are not checked for (but the output image is blanked around the 
c	   unconvolved edge, and wherever the input image is blanked).
c
c--
c
c  History:
c    nebk  11sep90   Original version.
c    rjs   25oct90   Merged source and documentation.
c    nebk  14dec90   Fixed an incorrect comment.
c    nebk  15jan91   Modify the way in which blanks are dealt with
c    nebk  12mar91   Change KEYA to KEYF for input file
c    nebk  06apr92   Adapt for memalloc routines
c    nebk  14jul92   Announce task for pjt
c    nebk  25nov92   Copy btype to output
c    mjs   12mar93   Use maxnax.h file instead of setting own value.
c    nebk  10aug93   Replace manual history by hisinput and add boxcar.
c    nebk  20nov93   Attempts to make units Jy/beam automatically.
c    rjs   25nov93   Change "width" to "fwhm".
c    rjs   02jul97   cellscale change.
c    rjs   23jul97   Add pbtype.
c------------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
c
      double precision a2r
      integer maxk, maxk2
      character version*25
      parameter (maxk = 100, maxk2 = (2*maxk+1)**2, 
     +           a2r = dpi / 180.0d0 / 3600.0d0)
      parameter (version = 'version 20-Nov-93')
c
      integer ipin, ipout, ipmin, ipmout
      real data(maxbuf)
      common data
c
      character in*32, out*32, line*80, ktype*8, bunit*8
      integer nsize(maxnax), kipnt(maxk2), kjpnt(maxk2), lin, lout,
     +naxis, ksizex, ksizey, ksize2, k, nktype
      double precision cdelt1, cdelt2
      real kern(maxk2), fwhm1, fwhm2, pa, scale, major, minor, ksum,
     +bmaj, bmin, bpa
      logical lrow(maxdim), hdprsnt, nocheck
c
      integer ntype
      parameter (ntype = 2)
      character type(ntype)*8
      data type  /'gaussian', 'boxcar'/
c-----------------------------------------------------------------------
      call output ('SMOOTH '//version)
      call output (' ')
      call bug ('i', 'Keyword "norm" replaced by keyword "scale"')
      call bug ('i', 'Keyword "scale" offers automatic Jy/beam scaling')
      call output (' ')
c
c  Get the input parameters.
c
      call keyini
      call keyf ('in', in, ' ')
      call keya ('out', out, ' ')
      call keymatch ('type', ntype, type, 1, ktype, nktype)
      call keyr ('fwhm', fwhm1, 0.0)
      call keyr ('fwhm', fwhm2, fwhm1)
      call keyr ('pa', pa, 0.0)
      call keyr ('scale', scale, -1.0)
      call getopt (nocheck)
      call keyfin
c
c  Check inputs.
c
      if (in.eq.' ') call bug ('f', 'No input image given')
      if (out.eq.' ') call bug ('f', 'No output image given')
      if (fwhm1.le.0.0 .or. fwhm2.le.0.0) call bug ('f', 'Invalid FWHM')
      major = max(fwhm1,fwhm2)
      minor = min(fwhm1,fwhm2)
c
      if (ktype.eq.' ') ktype = 'gaussian'
      if (ktype.ne.'gaussian' .and. ktype.ne.'boxcar') 
     +  call bug('f', 'Unrecognized type of convolving function')
c
      if (scale.lt.0.0) then
        if (ktype.eq.'boxcar') 
     +    call bug ('f', 
     +    'Jy/beam output autoscaling not available for boxcars')
      else if (scale.eq.0.0) then
        if (ktype.eq.'gaussian') then
          call output
     +     ('Scaling convolution sums by inverse gaussian volume')
        else if (ktype.eq.'boxcar') then
          call output
     +     ('Scaling convolution sums by inverse boxcar volume')
        end if
      else
        write (line, 100) scale
100     format ('Scaling convolution sums by ', 1pe12.5)
        call output (line)
      end if
c
c  Open the input image
c
      call xyopen (lin, in, 'old', maxnax, nsize)
      if (nsize(1).gt.maxdim) 
     +  call bug ('f', 'First dimension of image too large for storage')
      if (nsize(3).le.0) nsize(3) = 1
      call rdhdi (lin, 'naxis', naxis, 3)
      if (naxis.gt.maxnax) 
     +   call bug ('f', 'Image has too many dimensions')
      if (naxis.ge.4) then
         do k = 4, naxis
           if (nsize(k).ne.1) 
     +        call bug ('f', 'Can''t deal with hyper-cube')
         end do
      end if
      call rdhdd (lin, 'cdelt1', cdelt1, 0.0d0)
      call rdhdd (lin, 'cdelt2', cdelt2, 0.0d0)
      if (cdelt1*cdelt2.eq.0.0d0) call bug ('f',
     +    'Invalid increments in image')
c
c Don't bother checking for blanks if there is no blanking mask,
c regardless of what the user wants.
c
      nocheck = .not.hdprsnt(lin, 'mask')
c
c  Try to allocate memory
c 
      call memalloc (ipin,   nsize(1)*nsize(2), 'r')
      call memalloc (ipmin,  nsize(1)*nsize(2), 'r')
      call memalloc (ipout,  nsize(1)*nsize(2), 'r')
      call memalloc (ipmout, nsize(1)*nsize(2), 'r')
c
c  Open the output image and fill its header
c
      call xyopen (lout, out, 'new', naxis, nsize)
      call header (lin, lout)
c
c  Compute convolving gaussian or boxcar and normalization factor
c
      if (ktype.eq.'gaussian') then
        call makgauss (pa, major, minor, cdelt1, cdelt2, maxk2,
     +     ksizex, ksizey, ksize2, kern, kipnt, kjpnt, ksum)
        if (scale.lt.0.0) then
          call gaupar1 (lin, real(major*a2r), real(minor*a2r), 
     +                  pa, bunit, bmaj, bmin, bpa, scale)
          call wrhda (lout, 'bunit', bunit)
          call wrhdr (lout, 'bmaj', bmaj)
          call wrhdr (lout, 'bmin', bmin)
          call wrhdr (lout, 'bpa', bpa)
        end if
      else if (ktype.eq.'boxcar') then
        call makbox (fwhm1, fwhm2, cdelt1, cdelt2, maxk2,
     +     ksizex, ksizey, ksize2, kern, kipnt, kjpnt, ksum)
      end if
c
c  Loop over the third dimension and convolve each plane
c
      call output (' ')
      do k = 1, nsize(3)
        call xysetpl (lin,  1, k)
        call xysetpl (lout, 1, k)
        write (line, '(a,i4)') 'Beginning plane ', k
        call output (line)
c
c Read image. 
c
        call readim (lin, nsize, data(ipin), data(ipmin), lrow)
c
c Convolve image
c
        if (.not.nocheck) then
          call sm1 (nsize(1), nsize(2), ksize2, ksizex, ksizey, scale,
     +              kern, kipnt, kjpnt, data(ipin), data(ipmin),
     +              data(ipout), data(ipmout))
        else 
          call sm2 (nsize(1), nsize(2), ksize2, ksizex, ksizey, ksum,
     +              scale, kern, kipnt, kjpnt, data(ipin), data(ipmin),
     +              data(ipout), data(ipmout))
        end if 
c
c Write out image
c
        call writim (lout, nsize, data(ipout), data(ipmout), lrow)
      end do
c  
c Close up
c
      call history (lout, version)
      call xyclose (lin)
      call xyclose (lout)
c
      end
c
c
      subroutine getopt (nocheck)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     nocheck   don't check for blanks
c
c-----------------------------------------------------------------------
      implicit none
c
      logical nocheck
cc
      integer maxopt
      parameter (maxopt = 1)
c
      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'nocheck'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      nocheck =      present(1)
c
      end
c
c

      subroutine makgauss (pa, major, minor, cdelt1, cdelt2, maxk2,
     +                     ksizex, ksizey, ksize2, kern, ip, jp, ksum)
c-----------------------------------------------------------------------
c     Generate an array containing the convolving Gaussian. It is put
c     in a 1-D array so that inner convolution loop is 1-D instead of 
c     2-D and it will all vectorise.  The Gaussian has a oeak of 1.0
c
c  Input:
c     pa        r   User input P.A. in degrees
c     major     r   Major axis FWHM (arcsec)
c     minor     r   Minor axis FWHM (arcsec)
c     cdelt1,2  r   Increments of image (arcsec)
c     maxk2     i   Dimension of KERN
c  Output:
c     ksizex,y  i   Maximum possible pixel offsets from gaussian centre
c                   in gaussian array (as if it were 2-D).  This defines
c                   width of the image border than can't be convolved
c                   and the half sizes of the Gaussian.
c     ksize2    i   Dimension of KERN
c     kern      r   The convolving gaussian array (kernel)
c     ip,jp     i   For each KERN(I), IP(I) and JP(I) are the pixel 
c                   offsets in X and Y from the gaussian centre 
c     ksum      r   Integral of Gaussian 
c
c-----------------------------------------------------------------------
      implicit none
c
      integer ksizex, ksizey, ksize2, maxk2, ip(maxk2), jp(maxk2)
      double precision cdelt1, cdelt2
      real pa, major, minor, kern(maxk2), ksum
cc
      include 'mirconst.h'
      double precision ra, rd
      parameter (rd = 180.0d0/dpi, ra = rd*3600.0d0)
c
      real phi, theta, cphi, sphi, ctheta, stheta, majmin, xmax,
     +ymax, x, y, xp, yp, gfac, majsq, minsq, kmin, kmax, d1a, d2a
      integer h, i, j
      character line*80
c-----------------------------------------------------------------------
c
c Internal p.a. defined ccw from positive X axis -- 0 to 180 degrees
c
      phi = mod(pa+90.0, 360.0)
      if (phi.gt.180.0) then
        phi = phi - 180.0
      else if (phi.lt.-180.0) then
        phi = phi + 180.0
      end if
      if (phi.lt.0.0) phi = phi + 180.0
c
c Theta is the angle defining the axial ratio of the gaussian
c
      cphi = cos(phi/rd)
      sphi = sin(phi/rd)
c
      theta = atan(minor/major)
      ctheta = cos(theta)
      stheta = sin(theta)
c
c xmax is the x offset from the gaussian centre when the gaussian
c has fallen to about 1E-3 along the major axis. Similarly ymax. 
c Both in arcsec. These define the size of the Gaussian array 
c used for convolution
c
      majmin = 1.56 * sqrt(major**2 + minor**2)
      xmax = majmin * sqrt((ctheta**2*cphi**2) + (stheta**2*sphi**2))
      ymax = majmin * sqrt((ctheta**2*sphi**2) + (stheta**2*cphi**2))
c
c Convert increments to pixels  (rounded up)
c
      d1a = abs(cdelt1) * ra
      d2a = abs(cdelt2) * ra
      ksizex = int(xmax / d1a) + 1
      ksizey = int(ymax / d2a) + 1
c
c Size of 1-D Gaussian array
c
      ksize2 = (2*ksizex + 1) * (2*ksizey + 1)
      if (ksize2.gt.maxk2) 
     +   call bug ('f', 'Gaussian too big for internal storage')
c
c Now compute the Gaussian
c
      gfac = -4.0 * log(2.0) 
      majsq = major * major
      minsq = minor * minor 
      kmin = 1.0e9
      kmax = -1.0e9
      ksum = 0.0
c
      h = 1
      do j = -ksizey, ksizey, 1
         do i = -ksizex, ksizex, 1
            x = i * d1a
            y = j * d2a
            xp = y*sphi + x*cphi
            yp = y*cphi - x*sphi
c
            kern(h) = exp(gfac * ((xp**2/majsq) + (yp**2/minsq)))
            ip(h) = i
            jp(h) = j
            ksum = ksum + kern(h)
c
            if (i.eq.-ksizex .or. i.eq.ksizex .or.
     +          j.eq.-ksizey .or. j.eq.ksizey) then
              kmin = min (kmin, kern(h))
              kmax = max (kmax, kern(h))
            end if
c
            h = h + 1
         end do
      end do
c
c Report Gaussian information to user
c
      write (line, 100) 2*ksizex+1, 2*ksizey+1
100   format ('Gaussian array is ', i4, ' by ', i4, ' pixels')
      call output (line)
c
c      write (line, 200) kmin, kmax
c200   format ('Minimum and maximum array border values are ',
c     +         1pe11.4, ',', 1pe11.4)
c      call output (line)
c
      write (line, 300) ksum
300   format ('Gaussian integral = ', 1pe11.4)
      call output (line)
      call output (' ')
c
      end
c
c
      subroutine makbox (fwhm1, fwhm2, cdelt1, cdelt2, maxk2, ksizex,
     +                   ksizey, ksize2, kern, ip, jp, ksum)
c-----------------------------------------------------------------------
c     Generate an array containing the convolving Boxcar. It is put
c     in a 1-D array so that inner convolution loop is 1-D instead of
c     2-D and it will all vectorise
c
c  Input:
c     fwhm1     r   X axis FWHM (arcsec)
c     fwhm2     r   Y axis FWHM (arcsec)
c     cdelt1,2  r   Increments of image (arcsec)
c     maxk2     i   Dimension of GAUSS
c  Output:
c     ksizex,y  i   Maximum possible pixel offsets from boxcar centre
c                   in gaussian array (as if it were 2-D).  This defines
c                   width of the image border than can't be convolved
c                   and the half sizes of the boxcar.
c     ksize2    i   Dimension of GAUSS
c     kern      r   The convolving boxcar array (kernel)
c     ip,jp     i   For each GAUSS(I), IP(I) and JP(I) are the pixel
c                   offsets in X and Y from the boxcar centre
c     ksum      r   Integral of Boxcar
c
c-----------------------------------------------------------------------
      implicit none
c
      integer ksizex, ksizey, ksize2, maxk2, ip(maxk2), jp(maxk2)
      double precision cdelt1, cdelt2
      real fwhm1, fwhm2, kern(maxk2), ksum
cc
      include 'mirconst.h'
      double precision  ra, rd
      parameter (rd = 180.0d0/dpi, ra = rd*3600.0d0)
c
      double precision d1a, d2a, xpix, ypix
      real kmin, kmax
      integer h, i, j
      character line*80
c-----------------------------------------------------------------------
c
c Work out kernel widths in pixels.  The full width is always rounded
c up to an odd number of pixels.
c
      d1a = abs(cdelt1) * ra
      d2a = abs(cdelt2) * ra
c
      xpix = fwhm1 / (2*d1a)
      ypix = fwhm2 / (2*d2a)
c
      ksizex = nint(xpix)
      ksizey = nint(ypix)
c
      if (xpix - int(xpix).eq.0.5) ksizex = ksizex - 1      
      if (ypix - int(ypix).eq.0.5) ksizey = ksizey - 1
c
c Size of 1-D Boxcar array
c  
      ksize2 = (2*ksizex + 1) * (2*ksizey + 1)
      if (ksize2.gt.maxk2)
     +   call bug ('f', 'Boxcar too big for internal storage')
c     
c Now compute the boxcar
c     
      kmin = 1.0e9
      kmax = -1.0e9 
      ksum = 0.0
c
      h = 1
      do j = -ksizey, ksizey, 1
         do i = -ksizex, ksizex, 1
            kern(h) = 1.0 
            ip(h) = i
            jp(h) = j
            ksum = ksum + kern(h)
            if (i.eq.-ksizex .or. i.eq.ksizex .or.
     +          j.eq.-ksizey .or. j.eq.ksizey) then
              kmin = min(kmin, kern(h))
              kmax = max(kmax, kern(h))
            end if
c
            h = h + 1
         end do
      end do
c
c Report Boxcar information to user
c
      write (line, 100) 2*ksizex+1, 2*ksizey+1
100   format ('Boxcar array is ', i4, ' by ', i4, ' pixels')
      call output (line)
c      
      write (line, 300) ksum
300   format ('Boxcar integral = ', 1pe11.4)
      call output (line)
      call output (' ')
c  
      end
c
c
      subroutine readim (lun, size, data, mask, lrow)
c-----------------------------------------------------------------------
c     Read in image and blanking mask arrays
c     Do horrid copy to real array from logical mask so can
c     use memalloc routines for array allocation
c
c     Output
c       data   image 
c       mask   blanking mask.  +1 = true,  -1 = .false.
c-----------------------------------------------------------------------
      implicit none
c
      integer size(2), lun
      real data(*), mask(*)
      logical lrow(*)
cc
      integer j, ip, i
c-----------------------------------------------------------------------
      ip = 1
      do j = 1, size(2)
         call xyread (lun, j, data(ip))
         call xyflgrd (lun, j, lrow)
         do i = 1, size(1)
           mask(ip+i-1) = -1.0
           if (lrow(i)) mask(ip+i-1) = 1.0
         end do
c    
         ip  = ip + size(1)
      end do
c
      end
c
c
      subroutine sm1 (size1, size2, ksize2, ksizex, ksizey, scale,
     +                kern, ip, jp, in, maskin, out, maskout)
c-----------------------------------------------------------------------
c     Convolve image via multiplication and summation with checks for
c     blanking.  For a given output pixel,  if an input pixel in the
c     convolution sum is blank, it just doesn't contribute to the sum.
c     If the normalization is by the volume of the  convolving kernel
c     then the weight that would normally have been assigned the blanked
c     pixel is not included in the normalization sum either. 
c
c  Input:
c     size1,2   i   Dimensions of image arrays
c     ksize2    i   Dimension of kernel array
c     ksizex,y  i   Maximum possible pixel offset from kernel centre
c                   in gaussian array (as if it were 2-D).  This defines
c                   width of the image border than can't be convolved
c     scale     r   Convolution scaling factor.  If 0.0 then 
c		    normalize by the the kernel volume.  Otherwise,
c                   scale by SCALE
c     kern      r   The convolving kernel array
c     ip,jp     i   For each KERN(I), IP(I) and JP(I) are the pixel 
c                   offsets in X and Y from the kernel centre 
c     in        r   Input image
c     maskin    r   Input blanking mask; +1 = .true., -1 = .false.
c  Output:
c     out       r   Output image
c     maskout   r   Output blanking mask; +1 = .true., -1 = .false.
c
c-----------------------------------------------------------------------
      implicit none
c
      integer size1, size2, ksizex, ksizey, ksize2, ip(ksize2), 
     +jp(ksize2)
      real in(size1,size2), out(size1,size2), kern(ksize2), scale,
     +maskin(size1,size2), maskout(size1,size2)
cc
      integer h, i, j, n
      real sum, ksum, kscale
c-----------------------------------------------------------------------
      kscale = scale
      do j = 1, size2
        do i = 1, size1
          if (j.gt.ksizey .and. i.gt.ksizex .and.
     +        j.lt.size2-ksizey .and. i.lt.size1-ksizex) then
            sum = 0.0
            ksum = 0.0
            n = 0
            do h = 1, ksize2
              if (maskin(i+ip(h),j+jp(h)).gt.0.0) then
                n = n + 1
                sum = sum + (kern(h) * in(i+ip(h),j+jp(h)))
                ksum = ksum + kern(h)
              end if
            end do
c
            if (scale.eq.0.0) kscale = 1.0 / ksum
            if (n.gt.ksize2/2) then
c
c Output convolved pixel only if more than a half of the input 
c pixels in the convolution rectangle were unblanked.  This is
c an arbitrarily assigned cutoff.
c
              out(i,j) = sum * kscale
              maskout(i,j) = +1.0
            else
              out(i,j) = 0.0
              maskout(i,j) = -1.0
            end if
          else
            out(i,j) = 0.0
            maskout(i,j) = -1.0
          end if
        end do
      end do
c
      end
c
c
      subroutine sm2 (size1, size2, ksize2, ksizex, ksizey, ksum, scale,
     +                kern, ip, jp, in, maskin, out, maskout)
c-----------------------------------------------------------------------
c     Convolve image via multiplication and summation with no checks 
c     for blanking
c
c  Input:
c     size1,2   i   Dimensions of image arrays
c     ksize2    i   Dimension of kernel array
c     ksizex,y  i   Maximum possible pixel offsets from kernel centre
c                   in gaussian array (as if it were 2-D).  This defines
c                   width of the image border than can't be convolved
c     ksum      r   Convolving kernel sum
c     scale     r   Convolution scale factor
c     kern      r   The convolving kernel array
c     ip,jp     i   For each KERN(I), IP(I) and JP(I) are the pixel 
c                   offsets in X and Y from the kernel centre 
c     in        r   Input image
c     maskin    r   Input blanking mask; +1 = .true., -1 = .false.
c  Output:
c     out       r   Output image
c     maskout   r   Output blanking mask.  Equal to input mask.
c
c-----------------------------------------------------------------------
      implicit none
c
      integer size1, size2, ksizex, ksizey, ksize2, ip(ksize2), 
     +jp(ksize2)
      real in(size1,size2), out(size1,size2), kern(ksize2), scale,
     +maskin(size1,size2), maskout(size1,size2), ksum
cc
      integer h, i, j
      real sum, kscale
c-----------------------------------------------------------------------
      if (scale.eq.0.0) then
        kscale = 1.0 / ksum
      else
        kscale = scale
      end if
c
      do j = 1, size2
        do i = 1, size1
          if (j.gt.ksizey .and. i.gt.ksizex .and.
     +        j.lt.size2-ksizey .and. i.lt.size1-ksizex) then
            sum = 0.0
            do h = 1, ksize2
              sum = sum + (kern(h) * in(i+ip(h),j+jp(h)))
            end do
            out(i,j) = sum * kscale
            maskout(i,j) = maskin(i,j)
          else
            out(i,j) = 0.0
            maskout(i,j) = -1.0
          end if
        end do
      end do
c
      end
c
c
      subroutine writim (lun, size, data, mask, lrow)
c-----------------------------------------------------------------------
c     Write image and blanking masking arrays
c-----------------------------------------------------------------------
      implicit none
c
      integer size(2), lun
      real data(*), mask(*)
      logical lrow(*)
cc
      integer j, ip, i
c-----------------------------------------------------------------------
      ip = 1
      do j = 1, size(2)
         call xywrite (lun, j, data(ip))
         do i = 1, size(1)
           lrow(i) = .false.
           if (mask(ip+i-1).gt.0.0) lrow(i) = .true.
         end do
         call xyflgwr (lun, j, lrow)
c
         ip  = ip + size(1)
      end do
c
      end
c
c
      subroutine header (lin, lout)
c------------------------------------------------------------------------
c     Write header of output image
c------------------------------------------------------------------------
      implicit none
      integer lin, lout
c
      integer nkeys, i
      parameter(nkeys = 35)
      character keyw(nkeys)*8
      data keyw/     'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ',
     +    'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     +    'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     +    'obstime ','epoch   ','instrume','niters  ','object  ',
     +    'telescop','cellscal','history ','restfreq',
     +    'vobs    ','observer','obsra   ','obsdec  ','crpix1  ',
     +    'crpix2  ','crpix3  ','crpix4  ','crpix5  ','pbfwhm',
     +    'btype   ','pbtype  '/
c-----------------------------------------------------------------------
c
c  Copy keywords across 
c
      do i = 1, nkeys
        call hdcopy (lin, lout, keyw(i))
      end do
c
      end
c
c
      subroutine history (lout, version)
c------------------------------------------------------------------------
c     Write history of output image
c------------------------------------------------------------------------
      implicit none
c
      character version*(*)
      integer lout
cc
      character line*72
c-----------------------------------------------------------------------
      call hisopen (lout, 'append')
      line = 'SMOOTH Miriad '//version
      call hiswrite (lout, line)
      call hisinput (lout, 'SMOOTH')
      call hisclose(lout)
c
      end
