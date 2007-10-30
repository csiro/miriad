      program imhol
c-----------------------------------------------------------------------
c= IMHOL - Compute amplitude and phase images from real and imaginary
c& mchw
c: image analysis
c+
c	IMHOL computes amplitude and phase images from real and
c	imaginary holographic images. The amplitude image can be debiased,
c       and the phase image is computed as 0.5 * atan2(imaginary/real).
c@ in
c	Two values; the real and imaginary images, respectively. These
c	Wild card expansion is supported. 
c@ mag
c	Up to two values; the output intensity image and
c	optionally, its associated error image (which will be constant).
c	Default is no output images.
c@ phase
c	Up to two values; the output position angle image and optionally,
c	its associated error image (which will not be constant).  These
c	will be in degress, radians or microns (see OPTIONS).
c	Default is no output images.
c@ sigma
c	The mean standard deviation of the noise in the images.
c	Required when debiasing or blanking; try to make it as accurate
c	a value as possible. The default is 0.
c@ sncut
c	This is the S/N ratio below which the output images
c	are blanked (see also options=zero below). It is STRONGLY 
c	recommended that SNCUT of at least 2 is used.
c	The default is 0.
c@ pacut
c	The output images are blanked if the error in the phase
c	image (degrees, radians or mircrons depending on OPTIONS) is greater
c	than this value.  This is active even if you don't output
c	the PA image.   Note that there is no equivalent for the output
c	error of the POLI image because the error is constant and
c	equal to SIGMA.  Keyword SNCUT essentially takes care of this.
c	The default is no position angle error based blanking.
c@ options
c	Task enrichment options.  Minimum match is active.
c	  bias    If computing polarized intensity, do NOT remove the Ricean
c	          bias in the image.  By default, the bias is removed to first
c	          order with P = sqrt(P_obs**2 - sigma**2)   You should have
c	          a very good reason for using this option.  See VLA memo
c	          no. 161 by Patrick Leahy for more details of bias removal.
c	  zero    When the output pixel is clipped, by setting CLIP(1),
c	          setting OPTIONS=ZERO will cause the output polarized
c	          intensity image (not the position angle image) pixel to
c	          be set to 0.0 rather than being masked out.   This is
c	          very important if you are interested in doing statistics
c	          on a region of low polarized intensity S/N ratio.  If 
c	          you leave the region masked rather than zeroed, you will 
c	          bias the statistics in that region -- zero is a better
c	          estimate of the pixel than just excluding it from the
c	          statistics (provided the clip level is sufficiently small).
c	          Residual bias in the statistical results from the area then
c	          depend upon how well the bias remover works and at what 
c	          level clipping was performed.  See VLA memo no. 161
c	          by Patrick Leahy.
c	  radians Output the phase image in radians instead of degrees.
c	  microns Output the phase image as equivalent surface error in
c	          microns.
c	  relax   Only warn about image axis descriptor mismatches
c	          instead of giving a fatal error
c	  bmfit   Fit focus and pointing offsets to aperture E-field maps.
c--
c  History:
c    nebk 21may92 Original version.
c    nebk 18aug92 Add options=zero and keyword device.
c    nebk 04nov92 Better blanking
c    mjs  12mar93 Use maxnax.h file instead of setting own value.
c    mjs  13mar93 pgplot subr names have less than 7 chars.
c    mchw 09jul93 Added routine to make aperture E-field maps.
c    mchw 09aug93 Renamed IMHOL to keep Neil happy.
c    mchw 09nov93 Fixed a bug in bmproc for planet holography.
c    rjs  11oct95 Rework.
c------------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
      include 'maxnax.h'
      character version*40
      parameter (version = 'ImHol : version 11-Oct-95')
c
      real qepoch, uepoch, qcrpix(maxnax),ucrpix(maxnax), sigma,
     *		snclip, paclip
      double precision qcdelt(maxnax), ucdelt(maxnax), qcrval(maxnax),
     *		ucrval(maxnax)
c
      integer lq, lu, lpout(2), lpaout(2), qsize(maxnax), usize(maxnax),
     +qnaxis, unaxis, qstkax, ustkax, npout, npaout
c
      character qin*64, uin*64, pout(2)*64, paout(2)*64, ustr*8,
     +qctype(maxnax)*9, uctype(maxnax)*9, bflag, line*80, blstr*7
c
      logical radians, microns, debias, relax, zero, bmfit
c
      integer nkeys
      parameter (nkeys = 23)
      character keyw(nkeys)*8
c
      data keyw/     'date-obs','epoch   ','history ','instrume',
     +    'niters  ','object  ','restfreq','telescop','vobs    ',
     +    'obsra   ','obsdec  ','observer','xshift  ','yshift  ',
     +    'bmaj    ','bmin    ','bpa     ','pbfwhm  ','lstart  ',
     +    'lstep   ','ltype   ','lwidth  ','vobs    '/
c-------------------------------------------------------------------------
      call output (version)
c
c Get the inputs
c
      call keyini
c
      call keyf ('in', qin, ' ')
      call keyf ('in', uin, ' ')
      call mkeya ('mag', pout, 2, npout)
      call mkeya ('phase', paout, 2, npaout)
      if (qin.eq.' ' .or. uin.eq.' ') call bug ('f', 
     +      'You must specify both real and imaginary input images')
c
      call getopt (debias, radians, microns, relax, zero, bmfit)
c
      call keyr ('sncut', snclip, 0.0)
      snclip = max(snclip,0.0)
      call keyr ('paclip', paclip, 0.0)
      blstr = 'blanked'
      if (zero) blstr = 'zeroed'
c 
      call keyr ('sigma', sigma, 0.0)
c
      bflag = 'f'
      if (relax) bflag = 'w'
      call keyfin
c
c Issue some messages if producing an output image
c
      write (line, 10) blstr, snclip
10    format ('Output ', a, ' when     P/sigma < ', f6.2)
      call output (line)
c
      if (paclip.gt.0.0) then
        ustr = ' degrees'
        if (radians) ustr = ' radians'
        write (line, 30) blstr, paclip, ustr
30      format ('Output images ', a, ' when sigma(P.A.) > ', 
     +            1pe10.4, a)
        call output (line)
c
        if ((snclip.gt.0.0 .or. paclip.gt.0.0 .or. debias) .and. 
     +       sigma.le.0.0) 
     +     call bug ('f', 'You must specify sigma')
c
        if (npout.gt.0) then
          if (debias) then
            call output ('The polarized intensity image '//
     +                   'will be debiased')
          else
            call bug ('w', 
     +         'You are NOT debiasing the intensity image')
            if (snclip.lt.2.0) call bug ('w',
     +        'Nor have you safely blanked the image with SNCUT > 2')
          endif
        endif
      endif
c
c Open the input images 
c
      call openin (bflag, maxdim, maxnax, qin, lq, qnaxis, qsize, 
     +     qepoch, qcrpix, qcdelt, qcrval, qctype, qstkax)
c
      call openin (bflag, maxdim, maxnax, uin, lu, unaxis, usize,
     +      uepoch, ucrpix, ucdelt, ucrval, uctype, ustkax)
c
c Compare images for consistency
c
      call chkdes (bflag, qin, uin, qnaxis, unaxis, qsize, usize, 
     +     qcrpix, ucrpix, qcdelt, ucdelt, qcrval, ucrval, qepoch,
     +     uepoch, qctype, uctype, qstkax, ustkax)
c
c Strip the Stokes axis from the input header
c
      call axstrip (qstkax, qnaxis, qsize, qcrval, qcrpix, qcdelt,
     +                qctype)
c
c Open polarized intensity images as required
c
      lpout(1) = 0
      if (npout.gt.0) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, pout(1),
     +     version, lpout(1))
      lpout(2) = 0
      if (npout.eq.2) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, pout(2),
     +     version, lpout(2))
c
c Open position angle images as required
c
      lpaout(2) = 0
      if (npaout.gt.0) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, paout(1),
     +     version, lpaout(1))
      lpaout(2) = 0
      if (npaout.eq.2) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, paout(2),
     +     version, lpaout(2))
c
c Now compute and write out the output image(s)
c
       call bmproc (lq, lu, lpout, lpaout, qnaxis, qsize, qcrpix, 
     *   qcrval, qcdelt, qctype, debias, radians, microns, snclip,
     *	 paclip, sigma, zero, bmfit)
c
c Close up
c
      call xyclose (lq)
      call xyclose (lu)
      if (lpout(1).ne.0) call xyclose (lpout(1))
      if (lpout(2).ne.0) call xyclose (lpout(2))
      if (lpaout(1).ne.0) call xyclose (lpaout(1))
      if (lpaout(2).ne.0) call xyclose (lpaout(2))
c
      end
c************************************************************************
      subroutine getopt (debias, radians, microns, relax, zero, bmfit)
c
      implicit none
      logical debias, radians, relax, zero, bmfit, microns
c
c     Decode options array into named variables.
c
c   Output:
c     debias    Debias the polarized intensity image
c     radians   Output phase image is in radians
c     microns   Output "phase" image is in microns.
c     relax     Warnings only for axis descriptor mismatches
c     zero      Output zeros rather than setting flagging mask
c     bmfit	Fit focus and pointing offsets to aperture E-field maps.
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 6)
c
      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'bias    ', 'radians ', 'microns ',
     *		    'relax   ', 'zero    ', 'bmfit   '/
c
      call options ('options', opshuns, present, maxopt)
c
      debias  = .not.present(1)
      radians = present(2)
      microns = present(3)
      if(microns.and.radians)call bug('f',
     *  'Cannot use options=microns and radians together')
      relax   = present(4)
      zero    = present(5)
      bmfit   = present(6)
c
      end
c************************************************************************
      subroutine openin (bflag, maxdim, maxnax, in, lun, naxis, size,
     +                   epoch, crpix, cdelt, crval, ctype, stkax)
c
      implicit none
      integer maxdim, maxnax, lun, naxis, size(maxnax), stkax
      double precision cdelt(maxnax), crval(maxnax)
      real epoch, crpix(maxnax)
      character*(*) ctype(maxnax), in, bflag*1
c
c     Open an image and return some information about it
c
c  Input
c    bflag      Bug flag
c    maxdim     Maximum size a row can be
c    maxnax     Maximum number of axes image can have
c    in         Image name
c  Output
c    lun        Handle
c    naxis      Number of axes
c    size       Size of each axis
c    epoch      EPoch of image
c    crpix      Refernce pixels
c    cdelt      Increments
c    crval      Reference values
c    ctype      Axis types
c    stkax      Stokes axis
c-----------------------------------------------------------------------
      integer len1, i
      character*80 aline
c
      call xyopen (lun, in, 'old', maxnax, size)
      call rdhdi (lun, 'naxis', naxis, 0)
      if (naxis.eq.0) then
        aline = in(1:len1(in))//' has zero dimensions !!'
        call bug ('f', aline)
      endif
c
      if (size(1).gt.maxdim) then
        aline = 'First dimension of '//in(1:len1(in))//
     +             ' too large for storage'
        call bug ('f', aline)
      endif
      call hedinf (lun, naxis, size, epoch, crpix, cdelt, crval, ctype)
c
      stkax = 0
      do i = 1, naxis
        if (ctype(i).eq.'STOKES') stkax = i
      end do
      if (stkax.eq.0) then
        aline = 'Could not find Stokes axis in '//in(1:len1(in))
        call bug (bflag, aline)
      endif
c
      end
c
c
      subroutine hedinf (lun, naxis, size, epoch, crpix, cdelt,
     +                   crval, ctype)
c------------------------------------------------------------------------
c     Get some header keywords from the image associated with LUN
c 
c     Input
c       lun      Handle of image
c       naxis    Number of dimensions in image
c       size     Size of each axis
c     Output
c       epoch    Epoch of image
c       crpix    Array of image reference pixels
c       cdelt    Array of image increments (natural inits; rad)
c       crval    Array of image reference values (natural units)
c       ctype    Array of image axis types
c
c------------------------------------------------------------------------
      implicit none
c
      integer lun, naxis, size(naxis)
      real crpix(naxis), epoch
      double precision cdelt(naxis), crval(naxis)
      character*(*) ctype(naxis)
cc
      integer i
      character str*1, itoaf*1
c---------------------------------------------------------------------
      do i = 1, naxis
        str = itoaf(i)
c
        call rdhdr (lun, 'crpix'//str, crpix(i), real(size(i))/2.0)
        call rdhdd (lun, 'cdelt'//str, cdelt(i), 1.0)
        call rdhda (lun, 'ctype'//str, ctype(i), ' ')
        call rdhdd (lun, 'crval'//str, crval(i), 0.0)
      end do
      call rdhdr (lun, 'epoch', epoch, 0.0)
c
      end 
c
c
      subroutine chkdes (bflag, im1, im2, naxis1, naxis2, size1, size2,
     +   crpix1, crpix2, cdelt1, cdelt2, crval1, crval2, epoch1, 
     +   epoch2, ctype1, ctype2, stkax1, stkax2)
c-----------------------------------------------------------------------
c     Compare axis descriptors 
c
c  Input:
c   im1,2        Images
c   naxis1,2     Number of axes
c   size1,2      Sizes of each dimension
c   crpix1,2     Reference pixels
c   cdelt1,2     Increments
c   crval1,2     Refernce values
c   ctype1,2     types of axes
c   epoch1,2     Epochs
c   stkax1,2     Stokes axis
c  Output
c   stkax        Stokes axis
c-----------------------------------------------------------------------
      implicit none
c
      integer naxis1, naxis2, size1(*), size2(*), stkax1, stkax2
      character*(*) im1, im2, ctype1(*), ctype2(*), bflag
      real crpix1(*), crpix2(*), epoch1, epoch2
      double precision crval1(*), crval2(*), cdelt1(*), cdelt2(*)
cc
      integer k, l1, l2, len1
      character line*130
c-----------------------------------------------------------------------
      l1 = len1(im1)
      l2 = len1(im2)
c
      if (epoch1.ne.epoch2) then
        line = 'Unequal epochs for images '//im1(1:l1)//' & '//im2(1:l2)
        call bug (bflag, line)
      endif
c
      if (naxis1.ne.naxis2) then
        line = 'Unequal number dimensions for images '//
     +         im1(1:l1)//' & '//im2(1:l2)
        call bug (bflag, line)
      endif
c
      do k = 1, min(naxis1,naxis2)
        if (size1(k).ne.size2(k)) then
          write (line, 10) im1(1:l1), im2(1:l2), k
10        format ('Unequal sizes for images ', a, ' & ', a, 
     +            ' on axis ', i1)
          call bug (bflag, line)
        endif
c
        if (ctype1(k).ne.ctype2(k)) then
          write (line, 20) im1(1:l1), im2(1:l2), k
20        format ('Unequal ctype for images ', a, ' & ', a, 
     +            ' on axis ', i1)
          call bug (bflag, line)
        endif
c
        call chkds2 (bflag, 'crpix', k, im1(1:l1), im2(1:l2), 
     +               crpix1(k), crpix2(k))
        call chkds2 (bflag, 'cdelt', k, im1(1:l1), im2(1:l2), 
     +               real(cdelt1(k)), real(cdelt2(k)))
        if (k.ne.stkax1 .or. k.ne.stkax2)
     +    call chkds2 (bflag, 'crval', k, im1(1:l1), im2(1:l2), 
     +                 real(crval1(k)), real(crval2(k)))
      end do
c
      end
c
c
      subroutine chkds2 (bflag, type, iaxis, im1, im2, des1, des2)
c-----------------------------------------------------------------------
c     Compare an axis descriptor from two images
c
c  Input:
c    type    Type fo descriptor
c    iaxis   Axis number
c    im1,2   Images
c    des1,2  Descriptors
c
c-----------------------------------------------------------------------
      implicit none
c
      character*(*) type, im1, im2, bflag
      integer iaxis
      real des1, des2
cc
      character line*130
c-----------------------------------------------------------------------
      if (abs(des1-des2).gt.0.01*max(abs(des1),abs(des2))) then
        write (line, 10) type, im1, im2, iaxis
10      format ('Unequal ', a, ' for images ', a, ' & ', a, 
     +          ' on axis ', i1)
        call bug (bflag, line)
      endif
c
      end
c************************************************************************
      subroutine axstrip (iax, naxis, size, crval, crpix, cdelt,
     +                    ctype)
c----------------------------------------------------------------------
c     Strip an axis from the header items
c
c  Input:
c   iax    Axis to strip
c  Input/output
c   naxis  Number of axes
c   size   Size of axes
c   crval  Ref. values
c   crpix  Ref. pixels
c   cdelt  Increments
c   ctype  Axis types
c
c----------------------------------------------------------------------
      implicit none
c
      integer iax, naxis, size(naxis)
      real crpix(naxis)
      double precision crval(naxis), cdelt(naxis)
      character*(*) ctype(naxis)
cc
      integer i
c----------------------------------------------------------------------
      if (iax.eq.0) return
c
      if (naxis.eq.1) call bug ('f', 
     +   'This image has only one dimension; cannot strip it')
c
      naxis = naxis - 1
      if (iax.eq.naxis+1) return
c
      do i = iax, naxis
        size(i) = size(i+1)
        crval(i) = crval(i+1)
        crpix(i) = crpix(i+1)
        cdelt(i) = cdelt(i+1)
        ctype(i) = ctype(i+1)
      end do
c
      end
c************************************************************************
      subroutine openout (lin, naxis, size, nkeys, keyw, crval, crpix,
     +                    cdelt, ctype, out, version, lout)
      implicit none
c
      integer lin, lout, naxis, size(naxis), nkeys
      real crpix(naxis)
      double precision crval(naxis), cdelt(naxis)
      character*(*) keyw(nkeys), out, version, ctype(naxis)
c
c     Open an output image, copy header keywords across and write
c     the history
c
c  Input
c   lin    Image to copy keuwrods from
c   naxis  Number of axes
c   size   Size of axes
c   nkeys  Number of header keywords to copy
c   keyw   Keywords
c   crval  Refernce values
c   crpix  Reference pixels
c   cdelt  Increments
c   ctype  Axis types
c   out    Name of output image
c   versionVersion of this program
c  Output
c   lout   Handle for output image
c
c-----------------------------------------------------------------------
      integer i
      character itoaf*1, istr*1, aline*80
c-----------------------------------------------------------------------
      call xyopen (lout, out, 'new', naxis, size)
      do i = 1, nkeys
        call hdcopy (lin, lout, keyw(i))
      end do
c
c Do these separately because we had to strip the Stokes
c axis from the input image
c
      do i = 1, naxis
        istr = itoaf(i)
        call wrhdd (lout, 'crval'//istr, crval(i))
        call wrhdr (lout, 'crpix'//istr, crpix(i))
        call wrhdd (lout, 'cdelt'//istr, cdelt(i))
        call wrhda (lout, 'ctype'//istr, ctype(i))
      end do
c
      call hisopen  (lout, 'append')
      aline = 'IMHOL Miriad'//version
      call hiswrite (lout, aline)
      call hisinput (lout, 'IMHOL')
      call hisclose (lout)
c
      end
      subroutine allblnk (p, pf, ep, epf, pa, paf, epa, epaf)
      implicit none
      real p, ep, pa, epa
      logical pf, epf, paf, epaf
c
      p = 0.0
      pf = .false.
      ep = 0.0
      epf = .false.
      pa = 0.0
      paf = .false.
      epa = 0.0
      epaf = .false.
c
      end
c************************************************************************
	subroutine bmproc (lq, lu, lpout, lpaout, naxis, size, crpix, 
     +   crval, cdelt, ctype, debias, radians, microns, snclip, paclip,
     +   sigma, zero,bmfit)
c
      implicit none
      integer lq, lu, lpout(2), lpaout(2), naxis, size(naxis)
      real crpix(naxis), snclip, paclip, sigma
      double precision crval(naxis), cdelt(naxis)
      logical radians, debias, zero, bmfit, microns
      character*(*) ctype(naxis)
c
c  Compute aperture E-field amplitude, position angle 
c  image and associated error images
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'
c
      logical qflags(maxdim),uflags(maxdim),pflags(maxdim),
     *	epflags(maxdim),paflags(maxdim),epaflags(maxdim)
      real qline(maxdim),uline(maxdim),pline(maxdim),
     *	epline(maxdim),paline(maxdim),epaline(maxdim)
      logical pass1, ok
      integer i,j,k, frqax
      double precision fac, antdiam, subdiam
      real psq, sigsq, snclipsq, freq, snr, p, rms, rmsw
      character ustr*8, aline*80, telescop*10
      double precision sum,sumxx,sumyy,sumr2,sumr4,sumz,sumzz,sumw,
     *	sumwz,sumwzz,sumzx,sumzy,sumzr2,det,x,y,r2,dd,a,b,c,d,fitph
c
c  Get dish and subreflector radius in meters for masking the images.
c
      call rdhda(lq,'telescop',telescop,' ')
      call obspar(telescop,'antdiam',antdiam,ok)
      if(ok)then
        antdiam = antdiam / 2.
      else
        antdiam = 10000.
        call output('Unknown antenna diameter; setting to 10000.')
      endif
      call obspar(telescop,'subdiam',subdiam,ok)
      if(ok)then
        subdiam = subdiam / 2.
      else
        subdiam = 0.
        call output('Unknown subreflector diameter; setting to 0.')
      endif
c
c  Find frequency axis.
c
      frqax = 0
      do i = 1, naxis
        if (index(ctype(i),'FREQ').ne.0) frqax = i
      enddo
      if (frqax.eq.0) call bug ('w', 
     +    'Could not find frequency axis')
      if (frqax.le.2) call bug ('f',
     +    'Frequency axis is either 1 or 2.  These should be spatial')
c
c  Make amplitude and phase images for each plane (frequency axis)
c
      do k = 1, size(3)
        call xysetpl (lq,   1, k)
        call xysetpl (lu,   1, k)
        if (lpout(1).ne.0) call xysetpl (lpout(1), 1, k)
        if (lpout(2).ne.0) call xysetpl (lpout(2), 1, k)
        if (lpaout(1).ne.0) call xysetpl (lpaout(1), 1, k)
        if (lpaout(2).ne.0) call xysetpl (lpaout(2), 1, k)
c
	fac = 180/pi
	ustr = 'degrees'
	if(radians)then
	  fac = 1
	  ustr = 'radians'
	endif
	if(microns.and.frqax.eq.3)then
          freq = (k-crpix(3))*cdelt(3) + crval(3)
          fac = 0.5 / (2*pi) * cmks/freq * 1e-3
          ustr = 'microns'
	endif
	sigsq = sigma * sigma
	snclipsq = snclip * snclip
	paclip = fac * paclip
c
c  If(bmfit) then go thro' this loop twice:
c  1st pass to accumulate the sums and 2nd pass to correct the data.
c
	pass1 = .true.
100	continue
	sum    = 0.d0
	sumz   = 0.d0
	sumzz  = 0.d0
	sumw   = 0.d0
	sumwz  = 0.d0
	sumwzz = 0.d0
	sumxx  = 0.d0
	sumyy  = 0.d0
	sumr2  = 0.d0
	sumr4  = 0.d0
	sumzx  = 0.d0
	sumzy  = 0.d0
	sumzr2 = 0.d0
c
        do j = 1, size(2)
          call xyread  (lq, j, qline)
          call xyflgrd (lq, j, qflags)
          call xyread  (lu, j, uline)
          call xyflgrd (lu, j, uflags)
c
c Work out everything, but only write out what is requested
c
          do i = 1, size(1)
            psq = qline(i)**2 + uline(i)**2
            snr = 1.0
            if (snclip.gt.0.0) snr = psq / sigsq
c
            call allblnk (pline(i), pflags(i), epline(i), 
     +         epflags(i), paline(i), paflags(i), epaline(i),
     +         epaflags(i))
            if (zero) pflags(i) = .true.
c
            if ( (uline(i).eq.0.0 .and. qline(i).eq.0.0) .or.
     +           (.not.qflags(i) .or. .not.uflags(i)) ) then
c
c Undefined, so don't allow the "zero" blanking option
c
              pflags(i)   = .false.
            else if (snr.gt.snclipsq) then
c
c Passed the S/N ration criterion; work out amplitude and phase
c 
              pline(i) = sqrt(psq)
              epline(i) = sigma
              pflags(i) = .true.
              epflags(i) = .true.
c
              paline(i) = fac * (atan2(uline(i),qline(i))/2.0)
              epaline(i) = fac * sigma / sqrt(psq)
              paflags(i) = .true.
              epaflags(i) = .true.
c
              if (paclip.gt.0.0 .and. epaline(i).gt.paclip) then
c
c Failed the phase error blanking test.   Don't allow "zero"
c blanking here. Blank both amplitude and phase.
c
                call allblnk (pline(i), pflags(i), epline(i), 
     +            epflags(i), paline(i), paflags(i), epaline(i),
     +            epaflags(i))
              else
c
c Debias intensity if required
c
                if (debias) then
                  p = psq - sigsq
                  if (p.gt.0.0) then
                    pline(i) = sqrt(p)
                  else
c
c Blank amplitude and phase if we can't debias amplitude
c
                    call allblnk (pline(i), pflags(i), epline(i), 
     +                epflags(i), paline(i), paflags(i), epaline(i),
     +                epaflags(i))
c
c Zero is a reasonable estimate for this pixel so if requested,
c leave the flag mask at good for the amplitude image
c
                    if (zero) pflags(i) = .true.
                  endif
                endif
              endif
            endif
c
c  Fit focus and pointing offsets to aperture E-field maps.
c  Fit linear and quadratic terms to phase across aperture
c  phase(x,y)=a+bx+cy+d(x*x+y*y)
c
	    x  = (i-crpix(1))*cdelt(1)
	    y  = (j-crpix(2))*cdelt(2)
	    r2 = (x*x+y*y)
c
c  Mask amplitude and phase outside of illuminated aperture surface.
c
	    if(r2.gt.antdiam**2 .or. r2.lt.subdiam**2)then
              pflags(i)   = .false.
	      paflags(i) = .false.
	    endif
c
c  Accumulate the sums.
c
	    if(pass1.and.paflags(i))then 
	      sum    = sum    + 1.
	      sumz   = sumz   + paline(i)
	      sumzz  = sumzz  + paline(i)*paline(i)
	      sumxx  = sumxx  + x*x
	      sumyy  = sumyy  + y*y
	      sumr2  = sumr2  + r2
	      sumr4  = sumr4  + r2*r2
	      sumzx  = sumzx  + paline(i)*x
	      sumzy  = sumzy  + paline(i)*y
	      sumzr2 = sumzr2 + paline(i)*r2
	    else if(bmfit.and..not.pass1)then 
              fitph  = a + b*x + c*y + d*r2
              paline(i) = paline(i) - fitph
	      if(paflags(i))then 
	        sum    = sum    + 1.
	        sumz   = sumz   + paline(i)
	        sumzz  = sumzz  + paline(i)*paline(i)
	      endif
	      if(pflags(i))then 
	        sumw    = sumw    + pline(i)
	        sumwz   = sumwz   + pline(i)*paline(i)
	        sumwzz  = sumwzz  + pline(i)*paline(i)*paline(i)
	      endif
	    endif
          enddo
c
c Write out the images.
c
	  if(bmfit.and..not.pass1 .or. pass1.and..not.bmfit)then
            if (lpout(1).ne.0) then
              call xywrite (lpout(1), j, pline)
              call xyflgwr (lpout(1), j, pflags)
            endif
            if (lpout(2).ne.0) then
              call xywrite (lpout(2), j, epline)
              call xyflgwr (lpout(2), j, epflags)
            endif
c
            if (lpaout(1).ne.0) then
              call xywrite (lpaout(1), j, paline)
              call xyflgwr (lpaout(1), j, paflags)
            endif
            if (lpaout(2).ne.0) then
              call xywrite (lpaout(2), j, epaline)
              call xyflgwr (lpaout(2), j, epaflags)
            endif
          endif
c  Get next image row
        enddo
c
c  Sumarize results of focus and pointing fits.
c
        if(pass1)then
	  write(aline,'(a,i6)')
     *	    'Number of points in phase fit =',int(sum)
	  call output(aline)
	  b   = sumzx/sumxx
	  c   = sumzy/sumyy
	  det = sumr2*sumr2 - sum*sumr4
	  if (abs(det) .gt. 1.d-10) then
	    dd = (sumz*sumr2-sumzr2*sum)/det
	    d  = dd
	    a  = (sumz-dd*sumr2)/sum
          else
	    call output('Not fitting focus')
            a  = sumz/sum
	    d  = 0.0
          endif
c
	  call output(
     *      'Fit linear and quadratic terms to phase across aperture')
	  call output('Phase(x,y) = A + Bx + Cy + D(x*x+y*y) '//ustr)
          call output('Results of Phase Fit are:')
          write(aline,'(a,4(g12.5,3x))') 'A,B,C,D = ',A,B,C,D
	  call output(aline)
        endif
c
c  Compute the surface rms.
c
	if(sum.gt.0.)then
	  rms = sqrt(sumzz/sum - (sumz/sum)**2)
	  if(pass1)then
	    write(aline,'(a,g12.5,1x,a)')
     +		 'Surface rms before fit = ',rms, ustr
	  else
	    write(aline,'(a,g12.5,1x,a)')
     +		 'Surface rms after fit = ',rms, ustr
	  endif
	  call output(aline)
	endif
c
c  Compute the amplitude weighted surface rms.
c
	if(sumw.gt.0.)then
	  rmsw = sqrt(sumwzz/sumw - (sumwz/sumw)**2)
	  if(.not.pass1)then
	    write(aline,'(a,g12.5,1x,a)')
     +        'Amplitude weighted surface rms after fit = ',
     +		rmsw, ustr
	    call output(aline)
	  endif
	endif
c
c  Convert the fits to sensible units.
c  A and B have units of 1/fac radians per meter. Convert to arcsecs.
c
        if(frqax.eq.3)then
          freq = (real(k)-crpix(3))*cdelt(3) + crval(3)
	  write(aline,'(a,2g12.5,a)') 'Pointing offset in az,el = ',
     *	    b/fac/twopi*cmks/(freq*1e9)*180/pi*3600,
     *	    c/fac/twopi*cmks/(freq*1e9)*180/pi*3600, ' arcsecs'
	  call output(aline)
	  if(pass1)then
	    write(aline,'(a,g12.5,a)') 'Surface rms before fit = ',
     +	      0.5*rms/fac/twopi/freq*cmks*1.e-3, ' microns'
	    call output(aline)
	  else
	    write(aline,'(a,g12.5,a)') 'Surface rms after fit = ',
     +	      0.5*rms/fac/twopi/freq*cmks*1.e-3, ' microns'
	    call output(aline)
	    write(aline,'(a,g12.5,a)')
     +        'Amplitude weighted surface rms after fit = ',
     +	      0.5*rmsw/fac/twopi/freq*cmks*1.e-3, ' microns'
	    call output(aline)
          endif
        endif
c
        if(pass1.and.bmfit)then
	  call output('  ')
	  call output('Applying the fit to the output images')
  	  pass1 = .false.
  	  goto 100
	endif
c
c  Get next image plane
c
      enddo
c
c  Write some header info.
c
      call lcase(ustr)
      if (lpaout(1).ne.0) call wrhda (lpaout(1), 'bunit', ustr)
      if (lpaout(2).ne.0) call wrhda (lpaout(2), 'bunit', ustr)
c
      end
