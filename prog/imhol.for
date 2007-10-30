      program impol
c-----------------------------------------------------------------------
c= IMHOL - Compute amplitude and phase images from real and imaginary
c& mchw
c: image analysis
c+
c	IMHOL computes amplitude and phase images from real and
c	imaginary images. The amplitude image can be debiased, and the
c	phase image is computed as 0.5 * atan2(imaginary/real).
c	Uses of the task include making polarized intensity and position
c	angle images from Stokes Q and U images, and making make images
c	of the telescope aperture E-field amplitude and phase from
c	holography data. See options=relax,bmap,bmfit
c@ in
c	Two values; the Q and U images, respectively. Q and U are the
c	real and imaginary inputs for the amplitude and phase outputs.
c	Wild card expansion is supported. 
c@ poli
c	Up to two values; the output polarized intensity image and
c	optionally, its associated error image (which will be constant).
c	Default is no output images.
c@ pa
c	Up to two values; the output position angle image and optionally,
c	its associated error image (which will not be constant).  These
c	will be in degress (but see OPTIONS=RADIANS),
c	Default is no output images.
c@ sigma
c	The mean standard deviation of the noise in the Q and U images.
c	Required when debiasing or blanking; try to make it as accurate
c	a value as possible.  Perhaps measure it from a V image
c	No default.
c@ sncut
c	This is the S/N ratio, P/SIGMA, below which the output images
c	are blanked (see also options=zero below). It is STRONGLY 
c	recommended that SNCUT of at least 2 is used.
c	The default is 2.0
c@ pacut
c	The output images are blanked if the error in the position
c	angle image (degrees or radians depending on OPTIONS) is greater
c	than this value.  This is active even if you don't output
c	the PA image.   Note that there is no equivalent for the output
c	error of the POLI image because the error is constant and
c	equal to SIGMA.  Keyword SNCUT essentially takes care of this.
c	The default is no position angle error based blanking.
c@ rm
c	After computing the position angle image, rotate the position
c	angles back to zero wavelength by an amount specified by 
c	RM (rad/m**2).   Better to use IMRM to generate the rotation
c	measure and zero wavelength position angle images.
c	Default is 0.0
c@ options
c	Task enrichment options.  Minimum match is active.
c
c	"bias"     If computing polarized intensity, do NOT remove the Ricean
c	           bias in the image.  By default, the bias is removed to first
c		   order with P = sqrt(P_obs**2 - sigma**2)   You should have
c		   a very good reason for using this option.  See VLA memo
c		   no. 161 by Patrick Leahy for more details of bias removal.
c
c	"zero"     When the output pixel is clipped, by setting CLIP(1),
c		   setting OPTIONS=ZERO will cause the output polarized
c		   intensity image (not the position angle image) pixel to
c		   be set to 0.0 rather than being masked out.   This is
c		   very important if you are interested in doing statistics
c		   on a region of low polarized intensity S/N ratio.  If 
c		   you leave the region masked rather than zeroed, you will 
c		   bias the statistics in that region -- zero is a better
c		   estimate of the pixel than just excluding it from the
c		   statistics (provided the clip level is sufficiently small).
c		   Residual bias in the statistical results from the area then
c		   depend upon how well the bias remover works and at what 
c		   level clipping was performed.  See VLA memo no. 161
c		   by Patrick Leahy.
c
c	"radians"  Output the position angle image in radians instead
c		   of degrees.
c
c	"relax"    Only warn about image axis descriptor mismatches
c		   instead of giving a fatal error
c	"bmap"	   Make aperture E-field maps instead of polarization.
c	"bmfit"	   Fit focus and pointing offsets to aperture E-field maps.
c@device
c	PGPLOT device on which to draw a plot showing the effect of bias
c	in polarized intensity images.  It plots true polarized intensity
c	versus the bias, which is the estimated polarized intensity minus
c	the true polarized intensity.  Three estimators are shown; 
c	observed, first order, and maximum likelhood.  It is assumed
c	that sigma_P = 1  in these plots.  Because these plots are drawn 
c	following a monte carlo simulation of some 15,000 trials of the 
c	noise, you will need to be patient.  You can just make this bias
c	plot without actually working on any data if you wish. See also
c	VLA memo no. 161 by Patrick Leahy.
c	Default is no plot.
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
c------------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
      include 'maxnax.h'
      character version*40
      parameter (version = 'ImHol : version 09-Nov-93')
cc
      real qline(maxdim), uline(maxdim), pline(maxdim), paline(maxdim),
     +epline(maxdim), epaline(maxdim), qepoch, uepoch, qcrpix(maxnax),
     +ucrpix(maxnax), sigma, rm, snclip, paclip
      double precision qcdelt(maxnax), ucdelt(maxnax), qcrval(maxnax),
     +ucrval(maxnax)
c
      integer lq, lu, lpout(2), lpaout(2), qsize(maxnax), usize(maxnax),
     +qnaxis, unaxis, qstkax, ustkax, npout, npaout
c
      character qin*64, uin*64, pout(2)*64, paout(2)*64, ustr*8,
     +qctype(maxnax)*9, uctype(maxnax)*9, bflag, line*80, blstr*7,
     +device*80
c
      logical radians, debias, qflags(maxdim), uflags(maxdim),
     +pflags(maxdim), epflags(maxdim), paflags(maxdim), 
     +epaflags(maxdim), relax, zero, bmap, bmfit, doimage
c
      integer len1
      integer nkeys
      parameter (nkeys = 23)
      character keyw(nkeys)*8
c
      data keyw/     'date-obs','epoch   ','history ','instrume',
     +    'niters  ','object  ','restfreq','telescop','vobs    ',
     +    'obsra   ','obsdec  ','observer','xshift  ','yshift  ',
     +    'bmaj    ','bmin    ','bpa     ','pbfwhm  ','lstart  ',
     +    'lstep   ','ltype   ','lwidth  ','vobs    '/
      data lpout, lpaout /2*0, 2*0/
c-------------------------------------------------------------------------
      call output (version)
c
c Get the inputs
c
      call keyini
c
      call keyf ('in', qin, ' ')
      call keyf ('in', uin, ' ')
      call mkeya ('poli', pout, 2, npout)
      call mkeya ('pa', paout, 2, npaout)
      doimage = .true.
      if (qin.eq.' ' .and. uin.eq.' ' .and. npout.eq.0 .and.
     +    npaout.eq.0) then
        doimage = .false.
      else
        if (qin.eq.' ' .or. uin.eq.' ') call bug ('f', 
     +      'You must specify both Q and U input images')
        if (npout.eq.0 .and. npaout.eq.0) 
     +     call bug ('f', 'You must specify an output image')
      end if       
c
      call getopt (debias, radians, relax, zero, bmap, bmfit)
c
      call keyr ('sncut', snclip, 2.0)
      snclip = max(snclip,0.0)
      call keyr ('paclip', paclip, 0.0)
      blstr = 'blanked'
      if (zero) blstr = 'zeroed'
c 
      call keyr ('sigma', sigma, 0.0)
c
      call keyr ('rm', rm, 0.0)
      if (npaout.eq.0) rm = 0.0
c
      bflag = 'f'
      if (relax) bflag = 'w'
c
      call keya ('device', device, ' ')
      if (.not.doimage .and. device.eq.' ') call bug ('f',
     +   'You have given me nothing to do')
c
      call keyfin
c
c Issue some messages if producing an output image
c
      if (doimage) then
        write (line, 10) blstr, snclip
10      format ('Output ', a, ' when     P/sigma < ', f6.2)
        call output (line)
c
        if (paclip.gt.0.0) then
          ustr = ' degrees'
          if (radians) ustr = ' radians'
          write (line, 30) blstr, paclip, ustr
30        format ('Output images ', a, ' when sigma(P.A.) > ', 
     +            1pe10.4, a)
          call output (line)
        end if
c
        if (snclip.lt.2.0) call bug ('w', 'Interpreting polarized '
     +    //'images below P/SIG=2 is EXTREMELY hazardous')
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
          end if
        end if
      end if
c
c Open the input images 
c
      if (doimage) then
        call openin (bflag, maxdim, maxnax, qin, lq, qnaxis, qsize, 
     +     qepoch, qcrpix, qcdelt, qcrval, qctype, qstkax)
        if (qstkax.ne.0 .and. qcrval(qstkax).ne.2 .and..not.bmap)
     +	  call bug (bflag, 
     +     qin(1:len1(qin))//' does not appear to be a Q image')
c
        call openin (bflag, maxdim, maxnax, uin, lu, unaxis, usize,
     +      uepoch, ucrpix, ucdelt, ucrval, uctype, ustkax)
        if (ustkax.ne.0 .and. ucrval(ustkax).ne.3 .and..not.bmap)
     +     call bug (bflag, 
     +     uin(1:len1(uin))//' does not appear to be a U image')
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
        if (npout.gt.0) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, pout(1), .false., 
     +     version, lpout(1))
        if (npout.eq.2) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, pout(2), .false., 
     +     version, lpout(2))
c
c Open position angle images as required
c
        if (npaout.gt.0) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, paout(1), .true., 
     +     version, lpaout(1))
        if (npaout.eq.2) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, paout(2), .true.,
     +     version, lpaout(2))
c
c Now compute and write out the output image(s)
c
	if(bmap)then
         call bmproc (lq, lu, lpout, lpaout, qnaxis, qsize, qcrpix, 
     +   qcrval, qcdelt, qctype, debias, radians, snclip, paclip,
     +   sigma, qline, uline, pline, paline, epline, epaline,
     +   qflags, uflags, pflags, paflags, epflags, epaflags, zero,
     +   bmfit)
	else
         call polout (lq, lu, lpout, lpaout, qnaxis, qsize, qcrpix, 
     +   qcrval, qcdelt, qctype, debias, radians, snclip, paclip,
     +   sigma, rm, qline, uline, pline, paline, epline, epaline,
     +   qflags, uflags, pflags, paflags, epflags, epaflags, zero)
	endif
c
c Close up
c
        call xyclose (lq)
        call xyclose (lu)
        if (lpout(1).ne.0) call xyclose (lpout(1))
        if (lpout(2).ne.0) call xyclose (lpout(2))
        if (lpaout(1).ne.0) call xyclose (lpaout(1))
        if (lpaout(2).ne.0) call xyclose (lpaout(2))
      end if
c
c Draw plot
c
      if (device.ne.' ') call pltbias (device)
c
      end
c
c
      subroutine getopt (debias, radians, relax, zero, bmap, bmfit)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     debias    Debias the polarized intensity image
c     radians   Output position nagle in radians
c     relax     Warnings only for axis descriptor mismatches
c     zero      Output zeros rather than setting flagging mask
c     bmap	Make aperture E-field maps instead of polarization.
c     bmfit	Fit focus and pointing offsets to aperture E-field maps.
c-----------------------------------------------------------------------
      implicit none
c
      logical debias, radians, relax, zero, bmap, bmfit
cc
      integer maxopt
      parameter (maxopt = 6)
c
      character opshuns(maxopt)*7
      logical present(maxopt)
      data opshuns /'bias', 'radians', 'relax', 'zero', 'bmap', 'bmfit'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      debias  = .not.present(1)
      radians = present(2)
      relax   = present(3)
      zero    = present(4)
      bmap    = present(5)
      bmfit   = present(6)
c
      end
c
c
      subroutine openin (bflag, maxdim, maxnax, in, lun, naxis, size,
     +                   epoch, crpix, cdelt, crval, ctype, stkax)
c-----------------------------------------------------------------------
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
      implicit none
c
      integer maxdim, maxnax, lun, naxis, size(maxnax), stkax
      double precision cdelt(maxnax), crval(maxnax)
      real epoch, crpix(maxnax)
      character*(*) ctype(maxnax), in, bflag*1
cc
      integer len1, i
      character*80 aline
c-----------------------------------------------------------------------     
      call xyopen (lun, in, 'old', maxnax, size)
      call rdhdi (lun, 'naxis', naxis, 0)
      if (naxis.eq.0) then
        aline = in(1:len1(in))//' has zero dimensions !!'
        call bug ('f', aline)
      end if
c
      if (size(1).gt.maxdim) then
        aline = 'First dimension of '//in(1:len1(in))//
     +             ' too large for storage'
        call bug ('f', aline)
      end if
      call hedinf (lun, naxis, size, epoch, crpix, cdelt, crval, ctype)
c
      stkax = 0
      do i = 1, naxis
        if (ctype(i).eq.'STOKES') stkax = i
      end do
      if (stkax.eq.0) then
        aline = 'Could not find Stokes axis in '//in(1:len1(in))
        call bug (bflag, aline)
      end if
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
      end if
c
      if (naxis1.ne.naxis2) then
        line = 'Unequal number dimensions for images '//
     +         im1(1:l1)//' & '//im2(1:l2)
        call bug (bflag, line)
      end if
c
      do k = 1, min(naxis1,naxis2)
        if (size1(k).ne.size2(k)) then
          write (line, 10) im1(1:l1), im2(1:l2), k
10        format ('Unequal sizes for images ', a, ' & ', a, 
     +            ' on axis ', i1)
          call bug (bflag, line)
        end if
c
        if (ctype1(k).ne.ctype2(k)) then
          write (line, 20) im1(1:l1), im2(1:l2), k
20        format ('Unequal ctype for images ', a, ' & ', a, 
     +            ' on axis ', i1)
          call bug (bflag, line)
        end if
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
      end if
c
      end
c
c
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
c
c
      subroutine openout (lin, naxis, size, nkeys, keyw, crval, crpix,
     +                    cdelt, ctype, out, angle, version, lout)
c-----------------------------------------------------------------------
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
c   angle  True if output is position angle, else polarized intensity
c   versionVersion of this program
c  Output
c   lout   Handle for output image
c
c-----------------------------------------------------------------------
      implicit none
c
      integer lin, lout, naxis, size(naxis), nkeys
      real crpix(naxis)
      double precision crval(naxis), cdelt(naxis)
      character*(*) keyw(nkeys), out, version, ctype(naxis)
      logical angle
cc
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
      if (angle) then
        call wrbtype (lout, 'position_angle')
      else
        call wrbtype (lout, 'polarized_intensity')
      end if
c
      call hisopen  (lout, 'append')
      aline = 'IMPOL Miriad'//version
      call hiswrite (lout, aline)
      call hisinput (lout, 'IMPOL')
      call hisclose (lout)
c
      end
c
c
      subroutine polout (lq, lu, lpout, lpaout, naxis, size, crpix, 
     +   crval, cdelt, ctype, debias, radians, snclip, paclip,
     +   sigma, rm, qline, uline, pline, paline, epline, epaline, 
     +   qflags, uflags, pflags, paflags, epflags, epaflags, zero)
c-----------------------------------------------------------------------
c     Compute some combinaiton of polarized intensity, position angle 
c     image and associated error images
c
c-----------------------------------------------------------------------
      implicit none
c
      integer lq, lu, lpout(2), lpaout(2), naxis, size(naxis)
      real crpix(naxis), qline(*), uline(*), pline(*), paline(*), 
     +epline(*), epaline(*), snclip, paclip, sigma, rm
      double precision crval(naxis), cdelt(naxis)

      logical qflags(*), uflags(*), pflags(*), paflags(*), epflags(*),
     +epaflags(*), radians, debias, zero
      character*(*) ctype(naxis)
cc
      include 'mirconst.h'
      double precision r2d
      parameter (r2d = 180.0d0/dpi)
c
      integer i, j, k, frqax
      double precision fac
      real psq, sigsq, snclipsq, freq, snr, parot, p
      character ustr*8, aline*80
c-----------------------------------------------------------------------
      fac = 1.0
      ustr = ' radians'
      if (.not.radians) then
        fac = r2d
        ustr = ' degrees'
      end if
c
c Find frequency axis if rotating position angles back
c
      if (rm.ne.0.0) then
        frqax = 0
        do i = 1, naxis
          if (index(ctype(i),'FREQ').ne.0) frqax = i
        end do
c
        if (frqax.eq.0) call bug ('f', 
     +    'Could not find frequency axis with which to apply RM')
        if (frqax.le.2) call bug ('f',
     +    'Frequency axis is either 1 or 2.  These should be spatial')
c
        if (frqax.gt.3 .or. size(frqax).eq.1) then
c
c Find frequency of pixel one
c
          freq = (1.0 - crpix(frqax))*cdelt(frqax) + crval(frqax)
          freq = freq * 1.0e9
          parot = rm * (dcmks / freq)**2
          write (aline, 10) fac*parot, ustr
10        format ('Rotating position angles back by ', 1pe11.4, a)
          call output (aline)
        end if
      else
        parot = 0.0
      end if
c
      sigsq = sigma * sigma
      snclipsq = snclip * snclip
      paclip = fac * paclip
c
      do k = 1, size(3)
        if (rm.ne.0.0 .and. frqax.eq.3 .and. size(frqax).gt.1) then
          freq = 1.0e9 * ((real(k)-crpix(3))*cdelt(3) + crval(3))
          parot = rm * (dcmks / freq)**2 
        end if
c
        call xysetpl (lq,   1, k)
        call xysetpl (lu,   1, k)
        if (lpout(1).ne.0) call xysetpl (lpout(1), 1, k)
        if (lpout(2).ne.0) call xysetpl (lpout(2), 1, k)
        if (lpaout(1).ne.0) call xysetpl (lpaout(1), 1, k)
        if (lpaout(2).ne.0) call xysetpl (lpaout(2), 1, k)
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
c Passed the S/N ration criterion; work out poli and p.a.
c 
              pline(i) = sqrt(psq)
              epline(i) = sigma
              pflags(i) = .true.
              epflags(i) = .true.
c
              paline(i) = fac * (atan2(uline(i),qline(i))/2.0 - parot)
              epaline(i) = fac * sigma / sqrt(psq)
              paflags(i) = .true.
              epaflags(i) = .true.
c
              if (paclip.gt.0.0 .and. epaline(i).gt.paclip) then
c
c Failed the p.a. error blanking test.   Don't allow "zero"
c blanking here. Blank both poli and p.a.
c
                call allblnk (pline(i), pflags(i), epline(i), 
     +            epflags(i), paline(i), paflags(i), epaline(i),
     +            epaflags(i))
              else
c
c Debias polarized intensity if required
c
                if (debias) then
                  p = psq - sigsq
                  if (p.gt.0.0) then
                    pline(i) = sqrt(p)
                  else
c
c Blank poli and p.a. if we can't debias poli
c
                    call allblnk (pline(i), pflags(i), epline(i), 
     +                epflags(i), paline(i), paflags(i), epaline(i),
     +                epaflags(i))
c
c Zero is a reasonable estimate for this pixel so if requested,
c leave the flag mask at good for the poli image
c
                    if (zero) pflags(i) = .true.
                  end if
                end if
              end if
            end if
          end do
c
c Write them out
c
          if (lpout(1).ne.0) then
            call xywrite (lpout(1), j, pline)
            call xyflgwr (lpout(1), j, pflags)
          end if
          if (lpout(2).ne.0) then
            call xywrite (lpout(2), j, epline)
            call xyflgwr (lpout(2), j, epflags)
          end if
c
          if (lpaout(1).ne.0) then
            call xywrite (lpaout(1), j, paline)
            call xyflgwr (lpaout(1), j, paflags)
          end if
          if (lpaout(2).ne.0) then
            call xywrite (lpaout(2), j, epaline)
            call xyflgwr (lpaout(2), j, epaflags)
          end if
        end do
      end do
c
      if (lpaout(1).ne.0) then
        if (radians) then
          call wrhda (lpaout(1), 'bunit', 'RADIANS')
          if (lpaout(2).ne.0) call wrhda (lpaout(2), 'bunit', 'RADIANS')
        else
          call wrhda (lpaout(1), 'bunit', 'DEGREES')
          if (lpaout(2).ne.0) call wrhda (lpaout(2), 'bunit', 'DEGREES')
        end if
      end if
c
      end
c
c
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
c
c
      subroutine pltbias (device)
c-----------------------------------------------------------------------
c     Make a plot of the biases  in different estimators
c
c-----------------------------------------------------------------------
      implicit none
c
      character*(*) device
cc
      integer maxrun, maxpol
      parameter (maxrun = 15000, maxpol = 1000)
c
      real qumax, quinc, qu, pmlsum, pml, pp0, pfo, pfosum, 
     +pobssum
c
      real pmldi(maxpol), pfodi(maxpol), pobsdi(maxpol), ptrue(maxpol),
     +qunoise(2*maxrun), pobs(maxrun)
      integer nruns, i, j, nrml
c
      integer pgbeg, ierr, hlen
      logical conv
      character hard*3, aline*80
      real xmin, xmax, ymin, ymax
      data ymin, ymax /1.0e30, -1.0e30/
c-------------------------------------------------------------------
      call output (' ')
      call output ('Compute bias plots')
      call output (' ')
      qumax = 4.0
      quinc = 0.15
      nruns = maxrun
c
c Loop over S/N; sigma = 1
c
      qu = qumax
      pp0 = 1.0
      j = 1
      do while (pp0.gt.0.1)
c
c True polarization
c
        pp0 = sqrt(qu**2 + qu**2) 
        write (aline, 10) pp0
10      format ('P_true / sigma = ', f5.3)
        call output (aline)
        ptrue(j) = pp0
c
c Maximum likelihood
c
        call noisy (nruns, qu, qunoise, pobs)
        pmlsum = 0.0
        nrml = 0
        do i = 1, nruns
          call ml (pobs(i), pml, conv)
          if (conv) then
            nrml = nrml + 1
            pmlsum = pmlsum + pml
          end if
        end do
        if (nrml.gt.0) then
          pmldi(j) = (pmlsum / nrml) - pp0
        else
          pmldi(j) = 0.0
        end if
c
c First order
c
        pfosum = 0.0
        call noisy (nruns, qu, qunoise, pobs)
        do i = 1, nruns
          call firstord (pobs(i), pfo)
          pfosum = pfosum + pfo
        end do
        pfodi(j) = (pfosum / nruns) - pp0
c
c Observed
c
        pobssum = 0.0
        call noisy (nruns, qu, qunoise, pobs)
        do i = 1, nruns
          pobssum = pobssum + pobs(i)
        end do
        pobsdi(j) = (pobssum / nruns) - pp0
c
c Update extrema
c
        ymin = min(ymin,pmldi(j),pfodi(j),pobsdi(j))
        ymax = max(ymax,pmldi(j),pfodi(j),pobsdi(j))
c
c Increment P/sigma
c
        qu = qu - quinc
        j = j + 1
      end do
c
c  Try to open plot device
c
      ierr = pgbeg (0, device, 1, 1)
      if (ierr.ne.1) then
        call pgldev 
        call bug ('f', 'Error opening plot device')
      else
        call pgqinf ('hardcopy', hard, hlen)
        call pgscf (1)
        if (hard.eq.'YES') call pgscf (2)
c
        xmin = 0.0
        xmax = sqrt(2.0*qumax**2) + 0.1
        call limstr (ymin, ymax)
        call pgswin (xmin, xmax, ymin, ymax)
c
c  Draw box and label
c
        call pgpage
        call pgbox ('BCNST', 0.0, 0, 'BCNST', 0.0, 0)
        call pglab ('P\dtrue\u(\gs\dP\u=1)', 
     +                '<P\dest\u> - P\dtrue\u', 'Polarization bias')
c
c  Plot points
c
        call pgline (j-1, ptrue, pobsdi)
        call pgsls (2)
        call pgline (j-1, ptrue, pfodi)
        call pgsls (3)
        call pgline (j-1, ptrue, pmldi)
c
        call pgtext (4.0, 0.2, 'Observed')
        call pgtext (1.5, 0.075, 'First order')
        call pgtext (3.0, -0.125, 'Maximum likelihood')
        call pgend
      end if
c
      end
c
c
      subroutine noisy (nruns, qu, qunoise, pobs)
c--------------------------------------------------------------------
c     Make some Gaussian noise and add it to the signal
c
c--------------------------------------------------------------------
      integer nruns
      real qu, qunoise(*), pobs(*)
cc
      integer i
c-------------------------------------------------------------------
      call gaus (qunoise,2*nruns)
      do i = 1, nruns
        pobs(i) = sqrt((qu+qunoise(i))**2 + (qu+qunoise(i+nruns))**2)
      end do
c
      end
c
c
      subroutine ml (ppobs, ppml, conv)
c---------------------------------------------------------------------
c     Maximum likelihood method
c     P = estimate of real polarization from measured polarization P'
c
c     PP'/sig**2=(P/sig)**2  * I1(PP'/sig**2) / I0(PP'/sig**2)
c----------------------------------------------------------------------
      real ppobs, ppml
      logical conv
cc
      integer itmax
      double precision tol
      parameter (itmax = 2000, tol = 1.0e-5)
c
      double precision pobs, pml, pmlold, argi, bessi0, bessi1, di
      integer i
c----------------------------------------------------------------------
      pobs = ppobs
      pmlold = pobs
      di = 1.0
c 
c  Iterate solution of Pml/sigma at value of Pobs/sigma
c
      i = 1
      do while (i.le.itmax .and. di.gt.tol)
        argi = pmlold * pobs
        pml = pobs * bessi1(argi) / bessi0(argi)
        di = abs(pml - pmlold)
        pmlold = pml
        i = i + 1
      end do
c
      conv = .true.
      if (i.ge.itmax) conv = .false.
      ppml = pml
c
      end
c
c
      subroutine firstord (pobs, pfo)
c--------------------------------------------------------------------
c     First order
c     P = sqrt(P'**2 - sigma**2) 
c     P = estimate of real polarization from measured polarization P'
c-------------------------------------------------------------------
      real pobs, pfo
cc   
      real fac
c-------------------------------------------------------------------
      fac = pobs**2 - 1.0
      if (fac.gt.0.0) then
        pfo = sqrt(fac)
      else
        pfo = 0.0
      end if
c
      end
c
c
      subroutine limstr (dmin, dmax)
c-----------------------------------------------------------------------
c     Stretch limits by 5%
c
c     Input/output:
c       dmin,max    Minimum and maximum
c       
c-----------------------------------------------------------------------
      implicit none
      real dmin, dmax
cc
      real absmax, delta
c-----------------------------------------------------------------------
      delta = 0.05 * (dmax - dmin)
      absmax = max(abs(dmax),abs(dmin))
      if (delta.le.1.0e-4*absmax) delta = 0.01 * absmax
      if (delta.eq.0.0) delta = 1
      dmin = dmin - delta
      dmax = dmax + delta
c
      end
c-----------------------------------------------------------------------
	subroutine bmproc (lq, lu, lpout, lpaout, naxis, size, crpix, 
     +   crval, cdelt, ctype, debias, radians, snclip, paclip,
     +   sigma, qline, uline, pline, paline, epline, epaline, 
     +   qflags, uflags, pflags, paflags, epflags, epaflags, zero,bmfit)
c-----------------------------------------------------------------------
c     Compute aperture E-field amplitude, position angle 
c     image and associated error images
c
c-----------------------------------------------------------------------
      implicit none
c
      integer lq, lu, lpout(2), lpaout(2), naxis, size(naxis)
      real crpix(naxis), qline(*), uline(*), pline(*), paline(*), 
     +epline(*), epaline(*), snclip, paclip, sigma
      double precision crval(naxis), cdelt(naxis)

      logical qflags(*), uflags(*), pflags(*), paflags(*), epflags(*),
     +epaflags(*), radians, debias, zero, bmfit, pass1, ok
      character*(*) ctype(naxis)
cc
      include 'mirconst.h'
      double precision r2d,rts
      parameter (r2d=180.0d0/dpi,rts=3600.d0*180.d0/dpi)
c
      integer i, j, k, frqax
      double precision fac, antdiam, subdiam
      real psq, sigsq, snclipsq, freq, snr, p, rms, rmsw
      character ustr*8, aline*80, telescop*10
	double precision sum,sumxx,sumyy,sumr2,sumr4,sumz,sumzz,sumw,
     +    sumwz,sumwzz,sumzx,sumzy,sumzr2,det,x,y,r2,dd,a,b,c,d,fitph
c-----------------------------------------------------------------------
c
      fac = 1.0
      ustr = ' radians'
      if (.not.radians) then
        fac = r2d
        ustr = ' degrees'
      end if
c
c  Get dish and subreflector radius in nanosecs for masking the images.
c
	call rdhda(lq,'telescop',telescop,' ')
	call obspar(telescop,'antdiam',antdiam,ok)
	if(ok)then
	  antdiam = 1.0e9 / cmks * antdiam / 2.
	else
	  antdiam = 10000.
	  call output('Unknown antenna diameter; setting to 10000.')
	endif
	call obspar(telescop,'subdiam',subdiam,ok)
	if(ok)then
	  subdiam = 1.0e9 / cmks * subdiam / 2.
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
c  Holography data has u-v coordinates in arcsecs and image pixel in nanosecs.
c  Convert spatial coordinates from radians to arcsecs, i.e. nanosecs.
c
	do i = 1, 2
	  crval(i) = crval(i)*180.*3600./pi
	  cdelt(i) = cdelt(i)*180.*3600./pi
        enddo
c
c  Make amplitude and phase images for each plane (frequency axis)
c
      sigsq = sigma * sigma
      snclipsq = snclip * snclip
      paclip = fac * paclip
      do k = 1, size(3)
        call xysetpl (lq,   1, k)
        call xysetpl (lu,   1, k)
        if (lpout(1).ne.0) call xysetpl (lpout(1), 1, k)
        if (lpout(2).ne.0) call xysetpl (lpout(2), 1, k)
        if (lpaout(1).ne.0) call xysetpl (lpaout(1), 1, k)
        if (lpaout(2).ne.0) call xysetpl (lpaout(2), 1, k)
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
                  end if
                end if
              end if
            end if
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
	  write(*,'(a,i6)') ' Number of points in phase fit =',INT(SUM)
	  b   = sumzx/sumxx
	  c   = sumzy/sumyy
	  det = sumr2*sumr2 - sum*sumr4
	  if (abs(det) .gt. 1.d-10) then
	    dd = (sumz*sumr2-sumzr2*sum)/det
	    d  = dd
	    a  = (sumz-dd*sumr2)/sum
          else
            write(*,'(a)') ' Not fitting focus '
            a  = sumz/sum
	    d  = 0.0
          endif
c
	  write(*,'(a)')
     +      ' Fit linear and quadratic terms to phase across aperture'
	  write(*,'(a,a)') ' Phase(x,y) = A + Bx + Cy + D(x*x+y*y)',ustr
          WRITE(*,'(a)') ' Results of Phase Fit are:'
          WRITE(*,'(a,4(g12.5,3x))') ' A,B,C,D = ',A,B,C,D
        endif
c
c  Compute the surface rms.
c
	if(sum.gt.0.)then
	  rms = sqrt(sumzz/sum - (sumz/sum)**2)
	  if(pass1)then
	    write(aline,'(a,g12.5,a)')
     +		 'surface rms before fit= ',rms, ustr
	  else
	    write(aline,'(a,g12.5,a)')
     +		 'surface rms after fit= ',rms, ustr
	  endif
	  call output(aline)
	endif
c
c  Compute the amplitude weighted surface rms.
c
	if(sumw.gt.0.)then
	  rmsw = sqrt(sumwzz/sumw - (sumwz/sumw)**2)
	  if(.not.pass1)then
	    write(aline,'(a,g12.5,a)')
     +        'amplitude weighted surface rms after fit= ',
     +		rmsw, ustr
	    call output(aline)
	  endif
	endif
c
c  Convert the fits to sensible units.
c  A and B have units of 1/fac radians per nanosec. Convert to arcsecs.
c
        if(frqax.eq.3)then
          freq = (real(k)-crpix(3))*cdelt(3) + crval(3)
	  write(aline,'(a,2g12.5,a)') 'pointing offset in az,el = ',
     +	    a/fac/twopi/freq*rts, b/fac/twopi/freq*rts, ' arcsecs'
	  call output(aline)
	  if(pass1)then
	    write(aline,'(a,g12.5,a)') 'surface rms before fit= ',
     +	      rms/fac/twopi/freq*cmks*1.e-3, ' microns'
	    call output(aline)
	  else
	    write(aline,'(a,g12.5,a)') 'surface rms after fit= ',
     +	      rms/fac/twopi/freq*cmks*1.e-3, ' microns'
	    call output(aline)
	    write(aline,'(a,g12.5,a)')
     +        'amplitude weighted surface rms after fit= ',
     +	      rmsw/fac/twopi/freq*cmks*1.e-3, ' microns'
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
      if(lpaout(1).ne.0) then
        if(radians) then
          call wrhda (lpaout(1), 'bunit', 'RADIANS')
          if (lpaout(2).ne.0) call wrhda (lpaout(2), 'bunit', 'RADIANS')
        else
          call wrhda (lpaout(1), 'bunit', 'DEGREES')
          if (lpaout(2).ne.0) call wrhda (lpaout(2), 'bunit', 'DEGREES')
        endif
      endif
      end
