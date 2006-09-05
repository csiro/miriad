      program impol
c-----------------------------------------------------------------------
c= IMPOL - Compute polarized intensity and position angle from Q and U
c& nebk
c: image analysis
c+
c	IMPOL computes the total linearly polarized intensity
c	(optionally debiasing it) and position angle images from 
c	Stokes Q and U images.  Position angle is positive N -> E.
c@ in
c	Upto three values; the Q, U and I images, respectively.
c	The I image is only needed if you want to compute the
c	fractional polarization image as well or if you want to 
c	blank the output based upon an I S/N ratio.
c	Wild card expansion is supported. 
c@ poli
c	Up to two values; the output polarized intensity image and
c	optionally, its associated error image (which will be constant).
c	Default is no output images.
c@ polm
c	Up to values; the output fractional polarization image
c	and optionally, its associated error image.  You need
c	to input an I image to keyword "in" for this.
c@ pa
c	Up to two values; the output position angle image and optionally,
c	its associated error image (which will not be constant).  These
c	will be in degrees (but see OPTIONS=RADIANS),
c	Default is no output images.
c@ sigma
c	Up to 2 values; the mean standard deviation of the noise in 
c	the Q & U images (i.e. one number for them both),  and the
c	standard deviation of the I image.  
c
c	These are required for debiasing (Q,U only), or for generating
c	output error images, or for blanking the output. Try to make the 
c	Q,U value as accurate as possible for the debiasing.
c	Perhaps measure it from a V image
c	No default for sigma_QU, sigma_I defaults to sigma_QU
c@ sncut
c	Up to 2 values.  The first is the S/N ratio, P/SIGMA_QU, below 
c	which the output images are blanked (see also options=zero below).
c	It is generally recommended that an SNCUT of at least 2 is used.
c	The second value, which is only valid when you have input an I
c	image and sigma, is the S/N ratio, I/SIGMA_I, below which output
c	images are blanked (defaults to no I based blanking)
c	The default is 2.0 and 0.0
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
c		   a very good reason for using this option (e.g. a detection
c		   experiment).  See VLA memo no. 161 by Patrick Leahy for 
c		   more details of bias removal.
c
c	"zero"     When the output pixel is clipped because the debiasing
c		   fails (P**2 may become negative), setting OPTIONS=ZERO
c		   will cause the output polarized intensity image (not the
c		   position angle image) pixel to be set to 0.0 rather than 
c		   being masked out.   This is very important if you are 
c		   interested in doing statistics on a region of low 
c		   polarized intensity S/N ratio.  If you leave the region 
c		   masked rather than zeroed, you will bias the statistics 
c		   in that region -- zero is a better estimate of the pixel 
c		   than just excluding it from the statistics (provided the
c		   clip level is sufficiently small). Residual bias in the 
c		   statistical results from the area then depend upon how 
c		   well the bias remover works and at what level clipping 
c		   was performed.  See VLA memo no. 161 by Patrick Leahy.
c
c	"radians"  Output the position angle image in radians instead
c		   of degrees.
c
c	"relax"    Only warn about image axis descriptor mismatches
c		   instead of giving a fatal error
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
c    nebk 14nov93 Keyword pacut called paclip in code and doc changes.
c    rjs  19jan94 Relax alignment requirements in chkdes2 so that values
c		  need only align to 1%.
c    nebk 28feb94 Default values to RDHDD must be double precision
c    nebk 29mar95 Add fractional polarization images to output
c    nebk 25may95 Ref. pix. of output stuffed because of type mismatch
c    nebk 09jun95 Add I/sigma_I blanking
c    nebk 22aug96 Check for I=0
c    nebk 18dec96 Error for fractional polarization was wrong.  It needed
c                 to be multipled by the fractional polarization
c    rjs  02jul97 cellscal change.
c    rjs  23jul97 added pbtype.
c    nebk 13mar98 position angle error was factor of 2 too big
c------------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'maxnax.h'
      character version*(*)
      parameter (version = 'ImPol: version 13-Mar-98')
cc
      real iline(maxdim), qline(maxdim), uline(maxdim), pline(maxdim), 
     +  mline(maxdim), paline(maxdim), epline(maxdim), emline(maxdim),
     +  epaline(maxdim), iepoch, qepoch, uepoch, sigmaqu, sigmai, 
     +  rm, snclip(2), paclip
      double precision icdelt(maxnax), qcdelt(maxnax), ucdelt(maxnax), 
     +  icrval(maxdim), qcrval(maxnax), ucrval(maxnax),
     +  icrpix(maxnax), qcrpix(maxnax), ucrpix(maxnax)
c
      integer li, lq, lu, lpout(2), lpaout(2), lmout(2), isize(maxnax),
     +  qsize(maxnax), usize(maxnax), inaxis, qnaxis, unaxis, istkax,
     +  qstkax, ustkax, npout, npaout, nmout, nin
c
      character ins(3)*64, iin*64, qin*64, uin*64, pout(2)*64, 
     +  paout(2)*64, mout(2)*64, ustr*8, ictype(maxnax)*9, 
     +  qctype(maxnax)*9, uctype(maxnax)*9, 
     +  bflag, line*80, blstr*7, device*80
c
      logical radians, debias, iflags(maxdim), qflags(maxdim), 
     +  uflags(maxdim), pflags(maxdim), mflags(maxdim), epflags(maxdim), 
     +  emflags(maxdim), paflags(maxdim), epaflags(maxdim), relax, 
     +  zero, doimage
c
      integer len1
      integer nkeys
      parameter (nkeys = 23)
      character keyw(nkeys)*8
c
      data keyw/     'obstime ','epoch   ','history ','instrume',
     +    'niters  ','object  ','restfreq','telescop','vobs    ',
     +    'obsra   ','obsdec  ','observer','cellscal',
     +    'bmaj    ','bmin    ','bpa     ','pbfwhm  ','lstart  ',
     +    'lstep   ','ltype   ','lwidth  ','vobs    ','pbtype  '/
      data li, lpout, lmout, lpaout /0, 2*0, 2*0, 2*0/
c-------------------------------------------------------------------------
      call output (version)
      call output (' ')
c
c Get the inputs
c
      call keyini
      call mkeyf ('in', ins, 3, nin)
      call mkeya ('poli', pout, 2, npout)
      call mkeya ('polm', mout, 2, nmout)
      call mkeya ('pa', paout, 2, npaout)
      call getopt (debias, radians, relax, zero)
      call keyr ('sncut', snclip(1), 2.0)
      call keyr ('sncut', snclip(2), 0.0)
      call keyr ('pacut', paclip, 0.0)
      call keyr ('sigma', sigmaqu, 0.0)
      call keyr ('sigma', sigmai, sigmaqu)
      call keyr ('rm', rm, 0.0)
      call keya ('device', device, ' ')
      call keyfin
c
c Process the inputs
c
      doimage = .true.
      if (device.eq.' ') then 
        if (nin.eq.0) call bug ('f', 'Nothing to do')
      else
        if (nin.eq.0) doimage = .false.
      end if
c
      if (doimage .and. nin.lt.2) 
     +  call bug ('f', 'Not enough input images')
      qin = ins(1)
      uin = ins(2)
      iin = ' '
      if (nin.eq.3) iin = ins(3)
c
      if (doimage .and. npout.eq.0 .and. npaout.eq.0 .and. nmout.eq.0)
     +  call bug ('f', 'You must specify an output image')
c
      snclip(1) = max(snclip(1),0.0)
      snclip(2) = max(snclip(2),0.0)
      blstr = 'blanked'
      if (zero) blstr = 'zeroed'
      if (npaout.eq.0) rm = 0.0
c
      bflag = 'f'
      if (relax) bflag = 'w'
c
      sigmaqu = abs(sigmaqu)
      sigmai = abs(sigmai)
c
c Issue some messages if producing an output image
c
      if (doimage) then
        write (line, 10) blstr, snclip(1)
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
        if (snclip(1).lt.2.0) call bug ('w', 'Interpreting polarized '
     +    //'images below P/SIG=2 can be hazardous')
        if ((snclip(1).gt.0.0 .or. paclip.gt.0.0 .or. debias) .and. 
     +       sigmaqu.le.0.0) 
     +     call bug ('f', 'You must specify sigma')
c
        if (npout.gt.0) then
          if (debias) then
            call output ('The polarized intensity image '//
     +                   'will be debiased')
          else
            call bug ('w', 
     +      'The polarized intensity image will not be debiased')
            if (snclip(1).lt.2.0) call bug ('w',
     +   'The polarized intensity image will not be blanked with SNCUT')
          end if
        end if
      end if
c
c Open the input images 
c
      if (doimage) then
c
c I
c
        if (iin.ne.' ') call openin (bflag, maxdim, maxnax, iin, li, 
     +      inaxis, isize, iepoch, icrpix, icdelt, icrval, ictype, 
     +      istkax)
        if (istkax.ne.0 .and. icrval(istkax).ne.1) call bug (bflag, 
     +     iin(1:len1(iin))//' does not appear to be an I image')
c
c Q
c
        call openin (bflag, maxdim, maxnax, qin, lq, qnaxis, qsize, 
     +     qepoch, qcrpix, qcdelt, qcrval, qctype, qstkax)
        if (qstkax.ne.0 .and. qcrval(qstkax).ne.2) call bug (bflag, 
     +     qin(1:len1(qin))//' does not appear to be a Q image')
c
c U
c
        call openin (bflag, maxdim, maxnax, uin, lu, unaxis, usize,
     +      uepoch, ucrpix, ucdelt, ucrval, uctype, ustkax)
        if (ustkax.ne.0 .and. ucrval(ustkax).ne.3) call bug (bflag, 
     +     uin(1:len1(uin))//' does not appear to be a U image')
c
c Compare images for consistency
c
        call chkdes (bflag, qin, uin, qnaxis, unaxis, qsize, usize, 
     +     qcrpix, ucrpix, qcdelt, ucdelt, qcrval, ucrval, qepoch,
     +     uepoch, qctype, uctype, qstkax, ustkax)
        if (iin.ne.' ') call chkdes (bflag, qin, iin, qnaxis, inaxis, 
     +     qsize, isize, qcrpix, icrpix, qcdelt, icdelt, qcrval, 
     +     icrval, qepoch, iepoch, qctype, ictype, qstkax, istkax)
c
c Strip the Stokes axis from the input header (use the Q header
c from now on as they are all consistent)
c
        call axstrip (qstkax, qnaxis, qsize, qcrval, qcrpix, qcdelt,
     +                qctype)
c
c Open output images. Start with polarized intensity
c
        if (npout.gt.0) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, pout(1), 
     +     'polarized_intensity', version, lpout(1))
        if (npout.eq.2) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, pout(2), 
     +     'polarized_intensity', version, lpout(2))
c
c Fractional polarization
c
        if (nmout.gt.0) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, mout(1), 
     +     'fractional_polarization', version, lmout(1))
        if (nmout.eq.2) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, mout(2),
     +     'fractional_polarization', version, lmout(2))
c
c Position angle 
c
        if (npaout.gt.0) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, paout(1), 'position_angle',
     +     version, lpaout(1))
        if (npaout.eq.2) call openout (lq, qnaxis, qsize, nkeys, keyw,
     +     qcrval, qcrpix, qcdelt, qctype, paout(2), 'position_angle',
     +     version, lpaout(2))
c
c Now compute and write out the output image(s)
c
        call polout (lq, lu, li, lpout, lmout, lpaout, qnaxis, qsize, 
     +   qcrpix, qcrval, qcdelt, qctype, debias, radians, snclip, 
     +   paclip, sigmai, sigmaqu, rm, iline, qline, uline, pline, 
     +   mline, paline, epline, emline, epaline, iflags, qflags, 
     +   uflags, pflags, mflags, paflags, epflags, emflags, 
     +   epaflags, zero)
c
c Close up
c
        if (li.ne.0) call xyclose (li)
        call xyclose (lq)
        call xyclose (lu)
        if (lpout(1).ne.0) call xyclose (lpout(1))
        if (lpout(2).ne.0) call xyclose (lpout(2))
        if (lmout(1).ne.0) call xyclose (lmout(1))
        if (lmout(2).ne.0) call xyclose (lmout(2))
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
      subroutine getopt (debias, radians, relax, zero)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     debias    Debias the polarized intensity image
c     radians   Output position nagle in radians
c     relax     Warnings only for axis descriptor mismatches
c     zero      Output zeros rather than setting flagging mask
c-----------------------------------------------------------------------
      implicit none
c
      logical debias, radians, relax, zero
cc
      integer maxopt
      parameter (maxopt = 4)
c
      character opshuns(maxopt)*7
      logical present(maxopt)
      data opshuns /'bias', 'radians', 'relax', 'zero'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      debias  = .not.present(1)
      radians = present(2)
      relax   = present(3)
      zero    = present(4)
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
      double precision cdelt(maxnax), crval(maxnax), crpix(maxnax)
      real epoch
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
      real epoch
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      character*(*) ctype(naxis)
cc
      integer i
      character str*1, itoaf*1
c---------------------------------------------------------------------
      do i = 1, naxis
        str = itoaf(i)
c
        call rdhdd (lun, 'crpix'//str, crpix(i), dble(size(i))/2.0d0)
        call rdhdd (lun, 'cdelt'//str, cdelt(i), 1.0d0)
        call rdhda (lun, 'ctype'//str, ctype(i), ' ')
        call rdhdd (lun, 'crval'//str, crval(i), 0.0d0)
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
      real epoch1, epoch2
      double precision crval1(*), crval2(*), cdelt1(*), cdelt2(*),
     +  crpix1(*), crpix2(*)
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
     +               cdelt1(k), cdelt2(k))
        if (k.ne.stkax1 .or. k.ne.stkax2)
     +    call chkds2 (bflag, 'crval', k, im1(1:l1), im2(1:l2), 
     +                 crval1(k), crval2(k))
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
      double precision des1, des2
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
      double precision crval(naxis), cdelt(naxis), crpix(naxis)
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
     +                    cdelt, ctype, out, btype, version, lout)
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
c   btype  The type of image being opened. 
c            'fractional_polarization'
c	     'polarized_intensity'
c            'position_angle'
c   versionVersion of this program
c  Output
c   lout   Handle for output image
c
c-----------------------------------------------------------------------
      implicit none
c
      integer lin, lout, naxis, size(naxis), nkeys
      double precision crval(naxis), cdelt(naxis), crpix(naxis)
      character*(*) keyw(nkeys), out, version, ctype(naxis), btype*(*)
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
        call wrhdd (lout, 'crpix'//istr, crpix(i))
        call wrhdd (lout, 'cdelt'//istr, cdelt(i))
        call wrhda (lout, 'ctype'//istr, ctype(i))
      end do
      call wrbtype (lout, btype)
c
      call hisopen  (lout, 'append')
      aline = 'IMPOL: Miriad '//version
      call hiswrite (lout, aline)
      call hisinput (lout, 'IMPOL')
      call hisclose (lout)
c
      end
c
c
      subroutine polout (lq, lu, li, lpout, lmout, lpaout, naxis, 
     +   size, crpix, crval, cdelt, ctype, debias, radians, snclip, 
     +   paclip, sigmai, sigmaqu, rm, iline, qline, uline, pline, 
     +   mline, paline, epline, emline, epaline, iflags, qflags, 
     +   uflags, pflags, mflags, paflags, epflags, emflags, 
     +   epaflags, zero)
c-----------------------------------------------------------------------
c     Compute some combinaiton of polarized intensity, position angle 
c     image and associated error images
c
c-----------------------------------------------------------------------
      implicit none
c
      integer li, lq, lu, lpout(2), lmout(2), lpaout(2), naxis, 
     +  size(naxis)
      real iline(*), qline(*), uline(*), pline(*), mline(*), paline(*),
     +  epline(*), emline(*), epaline(*), snclip(2), paclip, sigmai,
     +  sigmaqu, rm
      double precision crval(naxis), cdelt(naxis), crpix(naxis)
      logical iflags(*), qflags(*), uflags(*), pflags(*), mflags(*),
     +  paflags(*), epflags(*), emflags(*), epaflags(*), radians, 
     +  debias, zero
      character*(*) ctype(naxis)
cc
      include 'mirconst.h'
      double precision r2d
      parameter (r2d = 180.0d0/dpi)
c
      integer i, j, k, frqax
      double precision fac
      real psq, sigsq, snclipsq, freq, psnr, isnr, parot, p, paerr
      character ustr*8, aline*80
      logical ok
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
      sigsq = sigmaqu * sigmaqu
      snclipsq = snclip(1) * snclip(1)
      paclip = fac * paclip
c
c Loop over planes
c
      do k = 1, size(3)
        if (rm.ne.0.0 .and. frqax.eq.3 .and. size(frqax).gt.1) then
          freq = 1.0e9 * ((real(k)-crpix(3))*cdelt(3) + crval(3))
          parot = rm * (dcmks / freq)**2 
        end if
c
c Set planes
c
        if (li.ne.0) call xysetpl (li, 1, k)
        call xysetpl (lq, 1, k)
        call xysetpl (lu, 1, k)
        if (lpout(1).ne.0) call xysetpl (lpout(1), 1, k)
        if (lpout(2).ne.0) call xysetpl (lpout(2), 1, k)
        if (lmout(1).ne.0) call xysetpl (lmout(1), 1, k)
        if (lmout(2).ne.0) call xysetpl (lmout(2), 1, k)
        if (lpaout(1).ne.0) call xysetpl (lpaout(1), 1, k)
        if (lpaout(2).ne.0) call xysetpl (lpaout(2), 1, k)
c
c Read lines of data
c
        do j = 1, size(2)
          if (li.ne.0) then
            call xyread  (li, j, iline)
            call xyflgrd (li, j, iflags)
          end if
          call xyread  (lq, j, qline)
          call xyflgrd (lq, j, qflags)
          call xyread  (lu, j, uline)
          call xyflgrd (lu, j, uflags)
c
c Work out everything possible from this row. By default, snclip(2)=0
c so ISNR=1 means no I based blanking by default
c
          do i = 1, size(1)
            psq = 0.0
            if (uline(i).ne.0.0 .and. qline(i).ne.0.0)
     +        psq = qline(i)**2 + uline(i)**2
            psnr = 1.0
            if (snclip(1).gt.0.0) psnr = psq / sigsq
            isnr = 1.0
            if (li.ne.0 .and. sigmai.gt.0.0 .and. snclip(2).gt.0.0)
     +        isnr = iline(i) / sigmai
c
c Work out p.a. error.  If this fails because q=u=0, doesn't
c matter because nothing more will be worked out under these
c conditions and all output will be blanked
c
            paerr = -1.0
            if (paclip.gt.0.0 .and. psq.gt.0.0)
     +        paerr = 0.5 * fac * sigmaqu / sqrt(psq)
c
c Init all output arrays with zeros and bad flags
c
            call allblnk (pline(i), pflags(i), epline(i), epflags(i),
     +         mline(i), mflags(i), emline(i), emflags(i),
     +         paline(i), paflags(i), epaline(i), epaflags(i))
c
c See what we can validly work out
c
            if ( (uline(i).ne.0.0 .or. qline(i).ne.0.0)    .and.
     +            qflags(i) .and. uflags(i)                .and.
     +            psnr.gt.snclipsq .and. isnr.gt.snclip(2) .and.
     +            paerr.lt.paclip) then
c
c Passed the P/sigma cutoff, the I/sigma cutoff, and the p.a. error 
c cutoff so debias P if desired
c
              ok = .true.
              if (debias) psq = psq - sigsq
              if (psq.gt.0.0) then
                p = sqrt(psq)
              else
                ok = .false.
              end if
c
              if (ok) then
c
c Work out all the output quantities here. Use debiased
c P for errors now
c
                pline(i) = p
                epline(i) = sigmaqu
                pflags(i) = .true.
                epflags(i) = .true.
c
                if (li.ne.0 .and. iline(i).ne.0.0) then
                  mline(i) = pline(i) / iline(i)
                  emline(i) = mline(i) * 
     +              sqrt((sigmaqu/pline(i))**2 + (sigmai/iline(i))**2)
                  mflags(i) = .true.
                  emflags(i) = .true.
                end if
c
                paline(i) = 
     +            fac * (atan2(uline(i),qline(i))/2.0 - parot) 
                epaline(i) = fac * sigmaqu / pline(i)
                paflags(i) = .true.
                epaflags(i) = .true.
              else
c
c If we can't debias poli, user can ask for zero as
c estimate for P and m
c
                if (zero) then
                  pflags(i) = .true.
                  mflags(i) = .true.
                end if
              end if
            end if
          end do
c
c Write out polarized intensity and error
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
c Write out fractional polarization and error
c
          if (lmout(1).ne.0) then
            call xywrite (lmout(1), j, mline)
            call xyflgwr (lmout(1), j, mflags)
          end if
          if (lmout(2).ne.0) then
            call xywrite (lmout(2), j, emline)
            call xyflgwr (lmout(2), j, emflags)
          end if
c
c Write out position angle and error
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
      subroutine allblnk (p, pf, ep, epf, m, mf, em, emf, pa, paf, 
     +                    epa, epaf)
      implicit none
      real p, ep, m, em, pa, epa
      logical pf, epf, mf, emf, paf, epaf
c
      p = 0.0
      pf = .false.
      ep = 0.0
      epf = .false.
c
      m = 0.0
      mf = .false.
      em = 0.0
      emf = .false.
c      
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

