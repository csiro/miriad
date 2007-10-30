      program regrid
      implicit none
c
c= regrid - regrid an image dataset
c& nebk
c: map analysis
c+
c	REGRID regrids an image by spline fitting and resampling.  The
c	regridding parameters can be specified by a template image, by
c	axis descriptors, or by a start and end range.   Blanked input
c	pixels are excluded from the interpolation arrays.  Extrapolated 
c	output pixels are blanked.  Regridding of any combination of 
c	the first three axes of an image is supported.
c
c@ in
c	The input image name. No default.
c@ tin
c	Input template image.  The axis descriptors of the regridded
c	image, for those axes specified by keyword "axis", are those
c	of the template image.
c	Default is no template image.
c@ out
c	The output image name.  No default.
c@ desc
c	If "tin" is unset, then specify the reference value, reference
c	pixel, pixel increment, and number of pixels for the axes 
c	designated by keyword "axis" of the output image.  
c
c	Note that for RA/DEC axes, the increments are in radians on 
c	the sky. Thus, dRA = dX / cos(dec) and dDEC = dY where you 
c	specify dX and dY.
c
c	Defaults are no axis descriptors.
c@ range
c	If "tin" and "desc" are unset, then specify the start coordinate
c	value, end coordinate value and pixel increment for the axes
c	designated by the keyword "axis" of the output image. The reference
c	value of the output image will be the same as the input image, 
c	and the reference pixel will be recomputed.
c
c	Note that for RA/DEC axes, the increments are in radians on 
c	the sky. Thus, dRA = dX / cos(dec) and dDEC = dY where you 
c	specify dX and dY.  The start and end coordinates are directly
c	RA and DEC in radians.
c	
c	Defaults are no ranges.
c@ axis 
c	Specify axes to regrid with a command such as axis=1,2 to regrid
c	axes 1 and 2, or 2,3 to regrid axes 2 and 3.  No matter
c	what order you specify axes in "axis", you MUST always give
c	the axis descriptors in "desc" and "range" in monotonically 
c	increasing axis order (e.g., 1,2,3 or 1,3 or 3 say).
c	No default.
c@ options
c	Task enrichment options.  Minimum match is active.
c
c	nohog   If regridding the third axis, the program is quite
c	        expensive on disk and memory resources. It may not
c		be able to allocate the needed resources. This option
c               offers a slower and less memory greedy method.
c	relax   Only issue warnings rather than fatal errors if axis type
c	        mismatches between template and input files exist
c
c--
c
c  History:
c     7jul92 pjt   Written to keep Neil quiet (not an easy thing to do)
c    23nov92 nebk  CHange to include all three regridding input styles
c		   (template, axis descriptors and start/end/inc).
c                  Deal with blanking, and generally tidy up.  Attempt
c		   to add new code in yucky pjt style. 
c    01dec92 nebk  More sig figs in printout
c      jan93 nebk  Rewrite to implement regridding of upto 3 axes in one
c		   pass. All good fun. NEBK becomes owner. 
c     2feb93 pjt   extracted spline.for to MIRSUBS 
c    10feb93 rjs   redefined double precision dynamic memory array to
c		   size MAXBUF/2.
c    12mar93 mjs   Use maxnax.h file instead of its own set value.
c    22jun93 nebk  Take cos(DEC) into account for RA/DEC axes
c    15apr94 nebk  Keyword range input was failing.
c    03jan95 nebk  REGZ was failing if output longer than input
c
c To do:
c     add simple transformation option (scale,shift etc)
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      integer maxboxes, maxruns
      character version*30
      parameter (maxboxes = 2048)
      parameter (maxruns = 3*maxdim)
      parameter (version = 'Version 05-Jan-95')
c
      character filei*80, fileo*80, filet*80
      integer axes(maxnax), naxes
      integer i, j, luni, luno, lunt
c
      double precision desc(maxnax*4), grid(maxnax*3), crvali(maxnax), 
     +  cdelti(maxnax), crvalt(maxnax), cdeltt(maxnax), crvalo(maxnax),
     +  cdelto(maxnax), crpixi(maxnax), crpixt(maxnax), crpixo(maxnax),
     +  cosdi, cosdo, xx, xxmax, xxinc, xxfac, faci(maxnax), 
     +  faco(maxnax)
      integer ni(maxnax), nt(maxnax), no(maxnax), ndesc, ngrid, naxisi,
     +  naxist, naxiso, irai, ideci, irao, ideco
      character*9 ctypei(maxnax), ctypet(maxnax), ctypeo(maxnax), 
     +  itoaf*1, str*1
      logical do(maxnax), hog, relax
c-----------------------------------------------------------------------
      call output( 'REGRID: '//version)
c
c Get inputs
c
      call keyini
      call keyf ('in', filei,' ')
      call keyf ('tin', filet, ' ')
      call keya ('out', fileo,' ')
      call mkeyd ('desc', desc, maxnax*4, ndesc)
      call mkeyd ('range', grid, maxnax*3, ngrid)
      call mkeyi ('axis', axes, maxnax, naxes)
      call getopt (hog, relax)
      call keyfin
c
c Check inputs
c
      if (filei.eq.' ' .or. fileo.eq.' ') call bug ('f',
     +  'You must give an input and an output file')
      if (naxes.eq.0) call bug ('f',
     +  'You have not specified any axes to regrid')
      if (ndesc.gt.0) then
        if (filet.ne.' ') call bug ('f', 
     +    'You cannot specify "tin" and "desc"')
        if (mod(ndesc,4).ne.0) call bug ('f', 
     +    'You must specify a multiple of 4 numbers for "desc"')
      end if
      if (ngrid.gt.0) then
        if (filet.ne.' ') call bug ('f', 
     +    'You cannot specify "tin" and "range"')
        if (mod(ngrid,3).ne.0) call bug ('f', 
     +    'You must specify a multiple of 3 numbers for "range"')
      end if
      if (ngrid.gt.0 .and. ndesc.gt.0) call bug ('f',
     +  'You cannot specify "desc" and "range"')
      if (filet.eq.' ' .and. ngrid.eq.0 .and. ndesc.eq.0) call bug ('f',
     +  'You didn''t specify any of "tin", "desc" or "range"')
c
c Make sure we have enough input values
c
      if (ndesc.gt.0) then
        if (ndesc.lt.naxes*4) call bug ('f',
     +    'You have not specified enough values with keyword "desc"')
      else if (ngrid.gt.0) then
        if (ngrid.lt.naxes*3) call bug ('f',
     +    'You have not specified enough values with keyword "range"')
      end if
c
c Find chosen axes to regrid
c
      do i = 1, maxnax
        do(i) = .false.
      end do
      do i = 1, naxes
        do(axes(i)) = .true.
      end do
c       
c  Open input and template files
c
      call openim (do, luni, filei, maxnax, naxisi, ni, crvali,
     +             crpixi, cdelti, ctypei, cosdi, irai, ideci)
c
      if (filet.ne.' ') then
        call openim (do, lunt, filet, maxnax, naxist, nt, crvalt,
     +               crpixt, cdeltt, ctypet, cosdo, irao, ideco)
        call xyclose (lunt)
      end if
c
c Make sure chosen regridding axes are sensible.
c
      do i = 1, naxes
        if (axes(i).gt.3) call bug ('f',
     +    'Can only regrid the first three axes')
c 
        if (axes(i).gt.naxisi) call bug ('f',
     +    'You have given an axis number higher than exists')
c
        if (ni(axes(i)).lt.2) then
          str = itoaf(axes(i))
          call bug ('f', 
     +      'Axis '//str//' has less than 2 pixels; can''t regrid')
        end if
      end do
c
c Initialize output axis descriptors to input descriptors
c
      naxiso = naxisi
      do i = 1, maxnax
        if (i.le.naxisi) then
          crvalo(i) = crvali(i)
          crpixo(i) = crpixi(i)
          cdelto(i) = cdelti(i)
          ctypeo(i) = ctypei(i)
          no(i) = ni(i)
        else
          crvalo(i) = 1.0
          crpixo(i) = 1.0
          cdelto(i) = 1.0
          ctypeo(i) = ' '
          no(i) = 1
        end if
      end do
c 
c Now compute the axis descriptors for the axes to be regridded
c
      if (filet.ne.' ') then
c
c Specified with template
c
        do i = 1, naxisi
          if (do(i)) then
            if (i.le.naxist) then
              if (ctypet(i).ne.ctypei(i)) then
                if (relax) then
                  call bug ('w', 
     +             'Input and template files have different axis types')
                  call bug ('w', 'Will use input axis types as truth')
                else
                  call bug ('i', 'Try, with care, options=relax')
                  call bug ('f',
     +             'Input and template files have different axis types')
                end if
              end if
c             
              crvalo(i) = crvalt(i)
              crpixo(i) = crpixt(i)
              cdelto(i) = cdeltt(i)
              no(i) = nt(i)
            else
              call bug ('f',
     +         'Template does not have enough axes for your request')
            end if
          end if
        end do
      else if (ndesc.gt.0) then
c
c Specified directly
c
        j = 1
        do i = 1, naxisi
          if (do(i)) then
            crvalo(i) = desc(j)
            crpixo(i) = desc(j+1)
            cdelto(i) = desc(j+2)
            no(i) = nint(desc(j+3))
            j = j + 4
          end if
        end do
c
c Compute cos(DEC) of output image only if both RA and DEC
c axes in input image
c
        cosdo = 1.0
        if (irai*ideci.ne.0) cosdo = cos(crvalo(ideci))
      else if (ngrid.gt.0) then
c
c Compute cos(DEC) of output image only if both RA and DEC
c axes in input image
c
        cosdo = 1.0
        if (irai*ideci.ne.0) cosdo = cos(crvalo(ideci))
c
c Given via start stop and increment.  Keep reference value the same 
c and recompute the reference pixel
c
        j = 1
        do i = 1, naxisi
          if (do(i)) then
            if (grid(j+2).ne.0.0) then
              if (grid(j).eq.grid(j+1)) call bug ('f',
     +          'You have given equal start and end axis limits')
c
c Find reference pixel and increment
c
              xxfac = 1.0              
              if (i.eq.irai) xxfac = cosdo
              crpixo(i) = 1.0 - xxfac*(grid(j) - crvalo(i))/grid(j+2)
              cdelto(i) = grid(j+2)
c
c Find length of axis
c
              xx = min(grid(j),grid(j+1))
              xxmax = max(grid(j),grid(j+1))
              xxinc = abs(grid(j+2))
              no(i) = 1
              do while (xx.le.xxmax)
                xx = xx + xxinc
                no(i) = no(i) + 1
              end do
              no(i) = no(i) - 1
            end if
            j = j + 3
          end if
        end do            
      end if
c
c Some checks
c
      do i = 1, naxisi
        str = itoaf(i)
        if (no(i).le.0) call bug ('f',
     +    'You specified a non-positive length for axis '//str)
        if (no(i).gt.maxdim) call bug ('f',
     +    'You specified too long an axis for axis '//str)
        if (cdelto(i).eq.0.0d0) call bug ('f',
     +    'You specified a zero pixel increment for axis '//str)
      end do
c
c Open the output file 
c
      call xyopen (luno, fileo, 'new', naxisi, no)
      call headcopy (luni, luno, 0, naxisi, 0, 0)
      do i = 1, naxiso
        if (do(i)) then
          str = itoaf(i)
          call wrhdd (luno, 'crval'//str, crvalo(i))
          call wrhdd (luno, 'cdelt'//str, cdelto(i))
          call wrhdd (luno, 'crpix'//str, crpixo(i))
        end if
      end do
c
c Set up the scale factors that are used for RA/DEC axes to make
c sure we deal in units of polar rotation & not radians on the sky
c
      do i = 1, maxnax
        faci(i) = 1.0
        faco(i) = 1.0
        if (i.eq.irai) then
          faci(i) = cosdi
          faco(i) = cosdo
        end if
      end do
c
c Keep user happy; report on the old and newly regridded axes
c
      call report (maxnax, faci, faco, do, ni, ctypei, crvali, crpixi,
     +             cdelti, no, ctypeo, crvalo, crpixo, cdelto)
c
c Deal with each case separately.  There is some code duplication but
c it is clearer this way, rather than trying to be clever (heaven forbid).
c
      if (do(1) .and. do(2) .and. do(3)) then
        call do123 (naxisi, ni, crpixi, crvali, cdelti, naxiso, no,
     +     crpixo, crvalo, cdelto, luni, luno, hog, faci, faco)
      else if (do(1) .and. do(2)) then
        call do12 (naxisi, ni, crpixi, crvali, cdelti, naxiso, no,
     +     crpixo, crvalo, cdelto, luni, luno, faci, faco)
      else if (do(1) .and. do(3)) then
        call do13 (naxisi, ni, crpixi, crvali, cdelti, naxiso, no,
     +     crpixo, crvalo, cdelto, luni, luno, hog, faci, faco)
      else if (do(2) .and. do(3)) then
        call do23 (naxisi, ni, crpixi, crvali, cdelti, naxiso, no,
     +     crpixo, crvalo, cdelto, luni, luno, hog, faci, faco)
      else if (do(3)) then
        call do3 (naxisi, ni, crpixi, crvali, cdelti, naxiso, no,
     +     crpixo, crvalo, cdelto, luni, luno, hog, faci, faco)
      else if (do(2)) then
        call do2 (naxisi, ni, crpixi, crvali, cdelti, naxiso, no,
     +     crpixo, crvalo, cdelto, luni, luno, faci, faco)
      else if (do(1)) then
        call do1 (naxisi, ni, crpixi, crvali, cdelti, naxiso, no,
     +     crpixo, crvalo, cdelto, luni, luno, faci, faco)
      else
        call bug ('f', 'I forgot a case, silly me')
      end if
c
c Process some history
c
      call hdcopy (luni, luno, 'history')
      call hisopen (luno, 'append')
      call hiswrite (luno, 'REGRID: Miriad '//version)
      call hisinput (luno, 'REGRID')
      call hisclose (luno)
c
c Close up and go home.
c
      call xyclose (luni)
      call xyclose (luno)
c
      end
c
c
      subroutine do1 (naxi, ni, crpixi, crvali, cdelti, naxo, no,
     +                crpixo, crvalo, cdelto, li, lo, faci, faco)
c-----------------------------------------------------------------------
c     Regrid just the first axis of an image
c
c  Input
c    naxi,o    Number of axes in input and output images
c    ni,o      Number of pixels on each axis of input and output
c    cr*i,o    Axis descriptors for first axis of input and output
c    li,o      Handles of input and output images
c    faci,o    Scale factors to be applied to axis increments of RA/DEC
c	       axes to make sure we deal in angles of polar rotation,
c              not radians on the sky
c
c-----------------------------------------------------------------------
      implicit none
c
      integer naxi, ni(naxi), naxo, no(naxo), li, lo
      double precision crvali, cdelti, crvalo, cdelto,
     +  crpixi, crpixo, faci, faco
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      logical lbuf(maxbuf)
c
      integer k, j, i, iabi, ivi, iabo, ivo, n, imi, imo
      character itoaf*4, str*4
      common rbuf
      equivalence (dbuf, rbuf, lbuf)
c-----------------------------------------------------------------------
      call output ('Regridding first axis')
c
c Allocate memory
c
      call memalloc (iabi, ni(1), 'd')
      call memalloc (iabo, no(1), 'd')
      call memalloc (ivi, ni(1), 'r')
      call memalloc (ivo, no(1), 'r')
      call memalloc (imi, ni(1), 'l')
      call memalloc (imo, no(1), 'l')
c
c Generate x output abcissa array
c
      do i = 1, no(1)
        dbuf(iabo+i-1) = (dble(i) - crpixo)*cdelto/faco + crvalo
      end do
c
c Loop over image
c
      do k = 1, ni(3)
        str = itoaf(k)
        call output ('Beginning plane '//str)
c
        call xysetpl (li, 1, k)
        call xysetpl (lo, 1, k)
c
c Read rows
c
        do j = 1, ni(2)
          call xyread  (li, j, rbuf(ivi))
          call xyflgrd (li, j, lbuf(imi))
c
c Generate input arrays, dealing with blanking
c
          call genabv (ni(1), crpixi, cdelti/faci, crvali, lbuf(imi), 
     +                 dbuf(iabi), rbuf(ivi), n)
c
c Spline fit and regrid
c
          call fitline (n, dbuf(iabi), rbuf(ivi), cdelti, no(1),
     +                  dbuf(iabo), rbuf(ivo), lbuf(imo))
c
c Write output
c
          call xywrite (lo, j, rbuf(ivo))
          call xyflgwr (lo, j, lbuf(imo))
        end do
      end do
c
c Free memory
c     
      call memfree (iabi, ni(1), 'd')
      call memfree (iabo, no(1), 'd')
      call memfree (ivi, ni(1), 'r')
      call memfree (ivo, no(1), 'r')
      call memfree (imi, ni(1), 'l')
      call memfree (imo, no(1), 'l')
c
      end
c
c
      subroutine do12 (naxi, ni, crpixi, crvali, cdelti, naxo, no,
     +                 crpixo, crvalo, cdelto, li, lo, faci, faco)
c-----------------------------------------------------------------------
c     Regrid the first two axes of an image.  In this subroutine,
c     we try to keep the i,j,k indices attached to the x,y,z axes
c     of the input image.
c
c
c  Input
c    naxi,o    Number of axes in input and output images
c    ni,o      Number of pixels on each axis of input and output
c    cr*i,o    Axis descriptors for first axis of input and output
c    li,o      Handles of input and output images
c    faci,o    Scale factors to be applied to axis increments of RA/DEC
c	       axes to make sure we deal in angles of polar rotation,
c              not radians on the sky
c
c-----------------------------------------------------------------------
      implicit none
c
      integer naxi, ni(naxi), naxo, no(naxo), li, lo
      double precision crvali(naxi), cdelti(naxi), crvalo(naxo), 
     +  cdelto(naxo), crpixi(naxi), crpixo(naxo), faci(2), faco(2)
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      integer ibuf(maxbuf)
      logical lbuf(maxbuf)
      
      integer k, j, iv2d, im2d, ip, iw, ierr, iwrk, ny
      character itoaf*4, str*4
c
      common rbuf
      equivalence (dbuf, rbuf, lbuf, ibuf)
c-----------------------------------------------------------------------
      call output ('Regridding first two axes')
c
c Allocate memory for 2-d image stored in core.   y-size must
c accomodate input and spline fit y-axis, so take biggest
c
      ny = max(ni(2), no(2))
      call memalloc (iv2d, no(1)*ny, 'r')
      call memalloc (im2d, no(1)*ny, 'l')
c
c Loop over image planes
c
      do k = 1, ni(3)
        str = itoaf(k)
        call output ('Beginning plane '//str)
c
        call xysetpl (li, 1, k)
        call xysetpl (lo, 1, k)
c
c Read x-axis and regrid writing into yx image in core
c
        call regx (li, ni, no, ny, crpixi(1), crvali(1), 
     +             cdelti(1)/faci(1), crpixo(1), crvalo(1), 
     +             cdelto(1)/faco(1), iv2d, im2d)
c
c Regrid the y-axis, and put back into the yx image in core
c
        call regy (ni, no, ny, crpixi(2), crvali(2), 
     +             cdelti(2)/faci(2), crpixo(2), crvalo(2),
     +             cdelto(2)/faco(2), iv2d, im2d)
c 
c Do in situ transpose of 2-d images
c
        iwrk = (no(1) + no(2)) / 2
        call memalloc (iw, iwrk, 'i')
        call transr (rbuf(iv2d), no(2), no(1), ibuf(iw),
     +               iwrk, ierr)
        if (ierr.ne.0) call bug ('f', 
     +     'DO12: Error transposing real matrix')
        call transl (lbuf(im2d), no(2), no(1), ibuf(iw), 
     +               iwrk, ierr)
        if (ierr.ne.0) call bug ('f', 
     +     'DO12: Error transposing logical matrix')
c
c Write out the image
c
        do j = 1, no(2)
          ip = (j-1)*no(1)
          call xywrite (lo, j, rbuf(iv2d+ip))
          call xyflgwr (lo, j, lbuf(im2d+ip))
        end do
c
c Free memory
c
        call memfree (iw, iwrk, 'i')
      end do
c
c Free memory allocated to 2-D image
c     
      call memfree (iv2d, no(1)*ny, 'r')
      call memfree (im2d, no(1)*ny, 'l')
c
      end
c
c
      subroutine do13 (naxi, ni, crpixi, crvali, cdelti, naxo, no,
     +                 crpixo, crvalo, cdelto, li, lo, hog, faci, faco)
c-----------------------------------------------------------------------
c     Regrid the first and third axes of an image.   In this subroutine,
c     we try to keep the i,j,k indices attached to the x,y,z axes
c     of the input image.
c
c  Input
c    naxi,o    Number of axes in input and output images
c    ni,o      Number of pixels on each axis of input and output
c    cr*i,o    Axis descriptors for first axis of input and output
c    li,o      Handles of input and output images
c    hog       if true, do it the faster and greedier way
c    faci,o    Scale factors to be applied to axis increments of RA/DEC
c	       axes to make sure we deal in angles of polar rotation,
c              not radians on the sky
c
c-----------------------------------------------------------------------
      implicit none
c
      integer naxi, ni(naxi), naxo, no(naxo), li, lo
      double precision crvali(naxi), cdelti(naxi), crvalo(naxo), 
     +  cdelto(naxo), crpixi(naxi), crpixo(naxo), faci(3), faco(3)
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      logical lbuf(maxbuf), hog
      integer k, iv2d, im2d, lv1, lm1, ts(3)
      character itoaf*4, str*4
c
      common rbuf
      equivalence (dbuf, rbuf, lbuf)
c-----------------------------------------------------------------------
c
c Allocate memory for 2-d image
c
      call memalloc (iv2d, no(1)*no(2), 'r')
      call memalloc (im2d, no(1)*no(2), 'l')
c
c Initialize transpose routines.  This will be used to transpose
c planes of a yxz cube into planes of a zxy cube
c
      ts(1) = no(2)
      ts(2) = no(1)
      ts(3) = ni(3)      
      call trnini (lv1, 3, ts, '321')
      call trnini (lm1, 3, ts, '321')
c
c Loop over image planes
c
      call output ('Regridding first axis')
      do k = 1, ni(3)
        str = itoaf(k)
        call output ('Beginning plane '//str)
        call xysetpl (li, 1, k)
c
c Read x-axis and regrid writing into yx image in core.  We could just
c keep an xy image in core and transpose that, but as that would not
c enable me to use REGX, might as well do it this way as there is
c no cpu penalty.
c
        call regx (li, ni, no, no(2), crpixi(1), crvali(1), 
     +             cdelti(1)/faci(1), crpixo(1), crvalo(1), 
     +             cdelto(1)/faco(1), iv2d, im2d)
c
c Write this yx data plane of the yxz cube out with the transpose routines
c
        call trnwrite (lv1, rbuf(iv2d))
c 
c Now transfer the mask to the real array (reuse it) so that it too
c can be transposed
c
        call l2r (no, iv2d, im2d)
        call trnwrite (lm1, rbuf(iv2d))
      end do
c
c Free memory allocated to 2-D images. 
c     
      call memfree (iv2d, no(1)*no(2), 'r')
      call memfree (im2d, no(1)*no(2), 'l')
c
c Now regrid the third axis using the tranpose files.
c 
      call regz (hog, lv1, lm1, lo, ni, no, crpixi(3), crvali(3),
     +   cdelti(3)/faci(3), crpixo(3), crvalo(3), cdelto(3)/faco(3))
c
      end
c
c
      subroutine do2 (naxi, ni, crpixi, crvali, cdelti, naxo, no,
     +                crpixo, crvalo, cdelto, li, lo, faci, faco)
c-----------------------------------------------------------------------
c     Regrid the second axis of an image. In this subroutine,
c     we try to keep the i,j,k indices attached to the x,y,z axes
c     of the input image.
c
c  Input
c    naxi,o    Number of axes in input and output images
c    ni,o      Number of pixels on each axis of input and output
c    cr*i,o    Axis descriptors for first axis of input and output
c    li,o      Handles of input and output images
c    faci,o    Scale factors to be applied to axis increments of RA/DEC
c	       axes to make sure we deal in angles of polar rotation,
c              not radians on the sky
c
c-----------------------------------------------------------------------
      implicit none
c
      integer naxi, ni(naxi), naxo, no(naxo), li, lo
      double precision crvali(naxi), cdelti(naxi), crvalo(naxo), 
     +  cdelto(naxo), crpixi(naxi), crpixo(naxo), faci(2), faco(2)
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      integer ibuf(maxbuf)
      logical lbuf(maxbuf)
c
      integer k, j, ivi, imi, iv2d, im2d, ip, iw, ierr, iwrk, ny
      character itoaf*4, str*4
c
      common rbuf
      equivalence (dbuf, rbuf, lbuf, ibuf)
c-----------------------------------------------------------------------
      call output ('Regridding second axis')
c
c Allocate memory for 2-d image stored in core.   y-size must
c accomodate input and spline fit y-axis, so take biggest
c
      ny = max(ni(2), no(2))
      call memalloc (iv2d, no(1)*ny, 'r')
      call memalloc (im2d, no(1)*ny, 'l')
c
c Loop over image planes
c
      do k = 1, ni(3)
        str = itoaf(k)
        call output ('Beginning plane '//str)
c
        call xysetpl (li, 1, k)
        call xysetpl (lo, 1, k)
c
c Allocate memory
c
        call memalloc (ivi, ni(1), 'r')
        call memalloc (imi, ni(1), 'l')
c
c Read rows and reorder
c
        do j = 1, ni(2)
          call xyread  (li, j, rbuf(ivi))
          call xyflgrd (li, j, lbuf(imi))
c
c Write row into column of yx image in core
c
          call row2col (j, ny, no(1), rbuf(ivi), lbuf(imi), 
     +                  rbuf(iv2d), lbuf(im2d))
        end do
c
c Free up memory
c
        call memfree (ivi, ni(1), 'r')
        call memfree (imi, ni(1), 'l')
c
c Now regrid the y-axis, and put it back into the yx image in core
c
        call regy (ni, no, ny, crpixi(2), crvali(2), cdelti(2)/faci(2),
     +             crpixo(2), crvalo(2), cdelto(2)/faco(2), iv2d, im2d)
c 
c Do in situ transpose of 2-d images
c
        iwrk = (no(1) + no(2)) / 2
        call memalloc (iw, iwrk, 'i')
        call transr (rbuf(iv2d), no(2), no(1), ibuf(iw),
     +               iwrk, ierr)
        if (ierr.ne.0) call bug ('f', 
     +     'DO2: Error transposing real matrix')
        call transl (lbuf(im2d), no(2), no(1), ibuf(iw), 
     +               iwrk, ierr)
        if (ierr.ne.0) call bug ('f', 
     +     'DO2: Error transposing logical matrix')
c
c Write out the image
c
        do j = 1, no(2)
          ip = (j-1)*no(1)
          call xywrite (lo, j, rbuf(iv2d+ip))
          call xyflgwr (lo, j, lbuf(im2d+ip))
        end do
c
c Free memory
c
        call memfree (iw, iwrk, 'i')
      end do
c
c Free memory allocated to 2-D image
c     
      call memfree (iv2d, no(1)*ny, 'r')
      call memfree (im2d, no(1)*ny, 'l')
c
      end
c
c
      subroutine do123 (naxi, ni, crpixi, crvali, cdelti, naxo,
     +   no, crpixo, crvalo, cdelto, li, lo, hog, faci, faco)
c-----------------------------------------------------------------------
c     Regrid the first three axes of an image.   In this subroutine,
c     we try to keep the i,j,k indices attached to the x,y,z axes
c     of the input image.
c
c  Input
c    naxi,o    Number of axes in input and output images
c    ni,o      Number of pixels on each axis of input and output
c    cr*i,o    Axis descriptors for first axis of input and output
c    li,o      Handles of input and output images
c    hog       if true, do it the faster and greedier way
c    faci,o    Scale factors to be applied to axis increments of RA/DEC
c	       axes to make sure we deal in angles of polar rotation,
c              not radians on the sky
c
c-----------------------------------------------------------------------
      implicit none
c
      integer naxi, ni(naxi), naxo, no(naxo), li, lo
      double precision crvali(naxi), cdelti(naxi), crvalo(naxo), 
     +  cdelto(naxo), crpixi(naxi), crpixo(naxo), faci(3), faco(3)
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      logical lbuf(maxbuf), hog
      integer k, iv2d, im2d, ny, lv1, lm1, ts(3)
      character itoaf*4, str*4
c
      common rbuf
      equivalence (dbuf, rbuf, lbuf)
c-----------------------------------------------------------------------
c
c Allocate memory for 2-d image
c
      ny = max(ni(2), no(2))
      call memalloc (iv2d, no(1)*ny, 'r')
      call memalloc (im2d, no(1)*ny, 'l')
c
c Initialize transpose routines.  This will be used to transpose
c planes of a yxz cube into planes of a zxy cube
c
      ts(1) = no(2)
      ts(2) = no(1)
      ts(3) = ni(3)      
      call trnini (lv1, 3, ts, '321')
      call trnini (lm1, 3, ts, '321')
c
c Loop over image planes
c
      call output ('Regridding first two axes')
      do k = 1, ni(3)
        str = itoaf(k)
        call output ('Beginning plane '//str)
        call xysetpl (li, 1, k)
c
c Read x-axis and regrid writing into yx image in core
c
        call regx (li, ni, no, ny, crpixi(1), crvali(1), 
     +             cdelti(1)/faci(1), crpixo(1), crvalo(1),
     +             cdelto(1)/faco(1), iv2d, im2d)
c
c Regrid the y-axis, and put it back into the yx image in memory
c
        call regy (ni, no, ny, crpixi(2), crvali(2), cdelti(2)/faci(2),
     +             crpixo(2), crvalo(2), cdelto(2)/faco(2), iv2d, im2d)
c
c Write this yx data plane of the yxz cube out with the transpose routines
c
        call trnwrite (lv1, rbuf(iv2d))
c 
c Now transfer the mask to the real array (reuse it) so that it too
c can be transposed
c
        call l2r (no, iv2d, im2d)
        call trnwrite (lm1, rbuf(iv2d))
      end do
c
c Free memory allocated to 2-D images. 
c     
      call memfree (iv2d, no(1)*ny, 'r')
      call memfree (im2d, no(1)*ny, 'l')
c
c Now regrid the third axis using tranpose files.
c 
      call regz (hog, lv1, lm1, lo, ni, no, crpixi(3), crvali(3),
     +   cdelti(3)/faci(3), crpixo(3), crvalo(3), cdelto(3)/faco(3))
c
      end
c
c
      subroutine do23 (naxi, ni, crpixi, crvali, cdelti, naxo,
     +   no, crpixo, crvalo, cdelto, li, lo, hog, faci, faco)
c-----------------------------------------------------------------------
c     Regrid the second and thrid axes of an image.   In this subroutine,
c     we try to keep the i,j,k indices attached to the x,y,z axes
c     of the input image.
c
c  Input
c    naxi,o    Number of axes in input and output images
c    ni,o      Number of pixels on each axis of input and output
c    cr*i,o    Axis descriptors for first axis of input and output
c    li,o      Handles of input and output images
c    hog       if true, do it the faster and greedier way
c    faci,o    Scale factors to be applied to axis increments of RA/DEC
c	       axes to make sure we deal in angles of polar rotation,
c              not radians on the sky
c
c-----------------------------------------------------------------------
      implicit none
c
      integer naxi, ni(naxi), naxo, no(naxo), li, lo
      double precision crvali(naxi), cdelti(naxi), crvalo(naxo), 
     +  cdelto(naxo), crpixi(naxi), crpixo(naxo), faci(3), faco(3)
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      logical lbuf(maxbuf), hog
      integer k, j, ivi, imi, iv2d, im2d, ny, lv1, lm1, ts(3)
      character itoaf*4, str*4
c
      common rbuf
      equivalence (dbuf, rbuf, lbuf)
c-----------------------------------------------------------------------
c
c Allocate memory for 2-d image stored in core.  Remeber no(1)=ni(1)
c
      ny = max(ni(2), no(2))
      call memalloc (iv2d, no(1)*ny, 'r')
      call memalloc (im2d, no(1)*ny, 'l')
c
c Initialize transpose routines.  This will be used to transpose
c planes of a yxz cube into planes of a zxy cube
c
      ts(1) = no(2)
      ts(2) = no(1)
      ts(3) = ni(3)
      call trnini (lv1, 3, ts, '321')
      call trnini (lm1, 3, ts, '321')
c
c Loop over image planes
c
      call output ('Regridding second axis')
      do k = 1, ni(3)
        str = itoaf(k)
        call output ('Beginning plane '//str)
        call xysetpl (li, 1, k)
c
c Allocate memory
c
        call memalloc (ivi, no(1), 'r')
        call memalloc (imi, no(1), 'l') 
c
c Read rows 
c
        do j = 1, ni(2)
          call xyread  (li, j, rbuf(ivi))
          call xyflgrd (li, j, lbuf(imi))
c
c Write row into column of reordered image (yx)
c
          call row2col (j, ny, no(1), rbuf(ivi), lbuf(imi), 
     +                  rbuf(iv2d), lbuf(im2d))
        end do
c
c Free up memory
c
        call memfree (ivi, no(1), 'r')
        call memfree (imi, no(1), 'l')
c
c Now regrid the y-axis, and put it back into the yx image in common
c
        call regy (ni, no, ny, crpixi(2), crvali(2), cdelti(2)/faci(2),
     +             crpixo(2), crvalo(2), cdelto(2)/faco(2), iv2d, im2d)
c
c Write this yx plane of the yxz cube out to the transpose routines
c
        call trnwrite (lv1, rbuf(iv2d))
c 
c Now transfer the mask to the real array (reuse it) so that it too
c can be transposed
c
        call l2r (no, iv2d, im2d)
        call trnwrite (lm1, rbuf(iv2d))
      end do
c
c Free memory allocated to 2-D images. 
c     
      call memfree (iv2d, no(1)*ny, 'r')
      call memfree (im2d, no(1)*ny, 'l')
c
c Now regrid the third axis using transpose files.
c
      call regz (hog, lv1, lm1, lo, ni, no, crpixi(3), crvali(3),
     +   cdelti(3)/faci(3), crpixo(3), crvalo(3), cdelto(3)/faco(3))
c
      end
c
c
      subroutine do3 (naxi, ni, crpixi, crvali, cdelti, naxo,
     +   no, crpixo, crvalo, cdelto, li, lo, hog, faci, faco)
c-----------------------------------------------------------------------
c     Regrid the third axis of an image.   In this subroutine,
c     we try to keep the i,j,k indices attached to the x,y,z axes
c     of the input image.
c
c  Input
c    naxi,o    Number of axes in input and output images
c    ni,o      Number of pixels on each axis of input and output
c    cr*i,o    Axis descriptors for first axis of input and output
c    li,o      Handles of input and output images
c    hog       if true, do it the faster and greedier way
c    faci,o    Scale factors to be applied to axis increments of RA/DEC
c	       axes to make sure we deal in angles of polar rotation,
c              not radians on the sky
c
c-----------------------------------------------------------------------
      implicit none
c
      integer naxi, ni(naxi), naxo, no(naxo), li, lo
      double precision crvali(naxi), cdelti(naxi), crvalo(naxo), 
     +  cdelto(naxo), crpixi(naxi), crpixo(naxo), faci(3), faco(3)
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      logical lbuf(maxbuf), hog
      integer j, k, iv2d, im2d, lv1, lm1, ioff, ts(3)
      character itoaf*4, str*4
c
      common rbuf
      equivalence (dbuf, rbuf, lbuf)
c-----------------------------------------------------------------------
c
c Allocate memory for 2-d image
c
      call memalloc (iv2d, no(1)*no(2), 'r')
      call memalloc (im2d, no(1)*no(2), 'l')
c
c Initialize transpose routines.  This will be used to transpose
c planes of an xyz cube into planes of a zxy cube
c
      ts(1) = no(1)
      ts(2) = no(2)
      ts(3) = ni(3)
      call trnini (lv1, 3, ts, '312')
      call trnini (lm1, 3, ts, '312')
c
c Loop over image planes
c
      call output ('Transposing cube')
      do k = 1, ni(3)
        str = itoaf(k)
        call output ('Beginning plane '//str)
        call xysetpl (li, 1, k)
c
c Read in image and mask
c
        do j = 1, ni(2)
          ioff = (j-1)*no(1)
          call xyread  (li, j, rbuf(iv2d+ioff))
          call xyflgrd (li, j, lbuf(im2d+ioff))
        end do
c
c Write this plane of the xyz cube out with the transpose routines
c
        call trnwrite (lv1, rbuf(iv2d))
        call l2r (no, iv2d, im2d)
        call trnwrite (lm1, rbuf(iv2d))
      end do
c
c Free memory allocated to 2-D images. 
c     
      call memfree (iv2d, no(1)*no(2), 'r')
      call memfree (im2d, no(1)*no(2), 'l')
c
c Now regrid the third axis using tranpose files.
c 
      call regz (hog, lv1, lm1, lo, ni, no, crpixi(3), crvali(3),
     +   cdelti(3)/faci(3), crpixo(3), crvalo(3), cdelto(3)/faco(3))
c
      end
c
c
      subroutine regx (li, ni, no, ny, crpixi, crvali, cdelti, crpixo, 
     +                 crvalo, cdelto, iv2d, im2d)
c-----------------------------------------------------------------------
c     Regrid the x axis of the input image.  Data are transferred via 
c     blank common.
c
c  Input
c    li     Handle of input image
c    ni     Length of each input axis
c    no     Length of each output axis
c    ny     The bigger of ni(2) and no(2) 
c    cr*    Axis descriptors for x axis of input and output images
c    iv2d   Pointer to real yx image in common 
c    im2d   Pointer to logical yx image in common 
c
c-----------------------------------------------------------------------
      implicit none
      integer ni(*), no(*), iv2d, im2d, ny, li
      double precision crpixi, cdelti, crvali, crpixo, cdelto, crvalo
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      logical lbuf(maxbuf)
      integer i, j, iabi, iabo, ivi, ivo, imi, imo, n
c
      common rbuf
      equivalence (dbuf, rbuf, lbuf)
c-----------------------------------------------------------------------
c
c Allocate memory
c
      call memalloc (iabi, ni(1), 'd')
      call memalloc (iabo, no(1), 'd')
      call memalloc (ivi, ni(1), 'r')
      call memalloc (ivo, no(1), 'r')
      call memalloc (imi, ni(1), 'l')
      call memalloc (imo, no(1), 'l')
c
c Generate x output abcissa array
c
      do i = 1, no(1)
        dbuf(iabo+i-1) = (dble(i) - crpixo)*cdelto + crvalo
      end do
c
c Read rows and fit
c
      do j = 1, ni(2)
        call xyread  (li, j, rbuf(ivi))
        call xyflgrd (li, j, lbuf(imi))
c
c Generate x input abcissa and data arrays, dealing with blanking
c
        call genabv (ni(1), crpixi, cdelti, crvali,
     +               lbuf(imi), dbuf(iabi), rbuf(ivi), n)
c
c Spline fit and regrid
c
        call fitline (n, dbuf(iabi), rbuf(ivi), cdelti, no(1),
     +                dbuf(iabo), rbuf(ivo), lbuf(imo))
c
c Write row into column of reordered image  (yx)
c
        call row2col (j, ny, no(1), rbuf(ivo), lbuf(imo), 
     +                rbuf(iv2d), lbuf(im2d))
      end do
c
c
c Free up memory
c
      call memfree (iabi, ni(1), 'd')
      call memfree (iabo, no(1), 'd')
      call memfree (ivi, ni(1), 'r')
      call memfree (ivo, no(1), 'r')
      call memfree (imi, ni(1), 'l')
      call memfree (imo, no(1), 'l')
c
      end
c
c
      subroutine regy (ni, no, ny, crpixi, crvali, cdelti, crpixo, 
     +                 crvalo, cdelto, iv2d, im2d)
c-----------------------------------------------------------------------
c     Regrid the y axis of the input image.  Data are transferred via 
c     blank common.
c
c  Input
c    ni     Length of each input axis
c    no     Length of each output axis
c    ny     The bigger of ni(2) and no(2) 
c    cr*    Axis descriptors for second axis of input and output images
c    iv2d   Pointer to real yx image in common 
c    im2d   Pointer to logical yx image in common 
c
c-----------------------------------------------------------------------
      implicit none
      integer ni(*), no(*), iv2d, im2d, ny
      double precision crpixi, cdelti, crvali, crpixo, cdelto, crvalo
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      logical lbuf(maxbuf)
      integer i, j, iabi, iabo, ivo, imo, ip, n
c
      common rbuf
      equivalence (dbuf, rbuf, lbuf)
c-----------------------------------------------------------------------
c
c Allocate memory
c
      call memalloc (iabi, ni(2), 'd')
      call memalloc (iabo, no(2), 'd')
      call memalloc (ivo, no(2), 'r')
      call memalloc (imo, no(2), 'l')
c
c Generate y output abcissa array
c
      do j = 1, no(2)
        dbuf(iabo+j-1) = (dble(j) - crpixo)*cdelto + crvalo
      end do
c
c Loop over the yx image, fitting rows 
c
      do i = 1, no(1)
c
c Generate y input abcissa and data arrays, dealing with blanking
c
        ip = (i-1)*ny
        call genabv (ni(2), crpixi, cdelti, crvali,
     +               lbuf(im2d+ip), dbuf(iabi), rbuf(iv2d+ip), n)
c
c Spline fit and regrid
c
        call fitline (n, dbuf(iabi), rbuf(iv2d+ip), cdelti,
     +                no(2), dbuf(iabo), rbuf(ivo), lbuf(imo))
c
c Copy back into the same row of the 2-d image.  Here is why the
c second dimension of the 2-d image  must be the bigger of the 
c input and output second axis sizes; otherwise we could overwrite.
c
        ip = (i-1)*no(2)
        do j = 1, no(2)
          rbuf(iv2d+ip+j-1) = rbuf(ivo+j-1)
          lbuf(im2d+ip+j-1) = lbuf(imo+j-1)
        end do
      end do
c
c Free memory
c
      call memfree (iabi, ni(2), 'd')
      call memfree (iabo, no(2), 'd')
      call memfree (ivo, no(2), 'r')
      call memfree (imo, no(2), 'l')
c
      end
c
c
      subroutine regz (hog, lv1, lm1, lo, ni, no, crpixi, crvali, 
     +                 cdelti, crpixo, crvalo, cdelto)
c-----------------------------------------------------------------------
c     Regrid the z axis of input image.  The data currently reside
c     in scratch files written by the transpose routines.  The
c     transpose routines must have been initialized such that when
c     the scratch files are read with the transpose routines,
c     a cube of order zxy is returned.
c
c Input
c  hog     if true, try to do it the fastest and greediest in resources
c          way, involving more transpose routines.  Otherwise we do
c          it with more scratch files written in a rather slow fashion
c  lv1,m1  Handles for the open data and mask transpose files.
c          Closed on exit
c  lo      Handle for the open output image
c  ni      Length of each input axis (xyz order)
c  no      Length of each output axis (xyz order)
c  cr*     Axis descriptors for z axis of input and output images
c
c-----------------------------------------------------------------------
      implicit none
c
      integer lv1, lm1, lo, ni(*), no(*)
      double precision crpixi, cdelti, crvali, crpixo, cdelto, crvalo
      logical hog
cc
      include 'maxdim.h'
      double precision dbuf(maxbuf/2)
      real rbuf(maxbuf)
      logical lbuf(maxbuf)
      integer i, j, k, nz
      integer iabi, iabo, ivo, imo, iv2d, im2d, lv2, lm2, lscr1, lscr2,
     +  ip, n, ioff, ts(3), zs, ipi, ipo
c
      common rbuf
      equivalence (dbuf, rbuf, lbuf)
c-----------------------------------------------------------------------
c
c Now we must read in the transposed zxy image, and fit the rows.
c We then write it back out to another scratch file in xyz order.
c
      call output ('Regridding third axis')
c
c Allocate memory
c
      call memalloc (iabi, ni(3), 'd')
      call memalloc (iabo, no(3), 'd')
      call memalloc (ivo,  no(3), 'r')
      call memalloc (imo,  no(3), 'l')
c
      nz = max(no(3), ni(3))
      zs = no(3) - ni(3)
      call memalloc (iv2d, nz*no(1), 'r')
      call memalloc (im2d, nz*no(1), 'r')
c
c Generate z output abcissa array
c
      do k = 1, no(3)
        dbuf(iabo+k-1) = (dble(k) - crpixo)*cdelto + crvalo
      end do
c
c If trying to do it the fastest and greediest way, initialize yet another
c set of transpose files and allocate yet more memory.  Otherwise, open 
c scratch files into which we will write the transposed images.  Note
c that both the data and the mask will be stored as floating point numbers
c at the end of this section.
c
      if (hog) then
c
c Transpose from zxy to xyz.  
c
        ts(1) = no(3)
        ts(2) = no(1)
        ts(3) = no(2)
        call trnini (lv2, 3, ts, '231')
        call trnini (lm2, 3, ts, '231')
      else
        call scropen (lscr1)
        call scropen (lscr2)
      end if
c
c Loop over transposed cube planes (these are now zx planes)
c and spline fit the z-axis
c
      do j = 1, no(2)
        call trnread (lv1, rbuf(iv2d))
        call trnread (lm1, rbuf(im2d))
c
c Reorder this zx image if the output resampled row is longer
c than the input row.   All we do is put some space between the
c shorter input rows allowing for the longer output to be 
c overwritten later on
c 
c
        if (zs.gt.0) then
          do i = no(1), 1, -1
            ipi = (i-1)*ni(3)
            ipo = (i-1)*no(3)
            do k = ni(3), 1, -1
              rbuf(iv2d+ipo+k-1) = rbuf(iv2d+ipi+k-1)
              rbuf(im2d+ipo+k-1) = rbuf(im2d+ipi+k-1)
            end do
          end do
        end if
c
c Loop over and spline fit rows in zx plane 
c
        do i = 1, no(1)
c
c Generate z input abcissa and data arrays, dealing with blanking
c
          if (zs.gt.0) then
            ip = (i-1)*no(3)
          else
            ip = (i-1)*ni(3)
          end if
          call genabv2 (ni(3), crpixi, cdelti, crvali, 
     +                  rbuf(im2d+ip), dbuf(iabi), rbuf(iv2d+ip), n)
          if (n.ne.ni(3)) write (*,*) 'blanks'
c
c Spline fit and regrid
c
          call fitline (n, dbuf(iabi), rbuf(iv2d+ip), cdelti, no(3),
     +                  dbuf(iabo), rbuf(ivo), lbuf(imo))
c
c Copy back into the same row of the zx plane
c
          ip = (i-1)*no(3)
          do k = 1, no(3)
            rbuf(iv2d+ip+k-1) = rbuf(ivo+k-1)
            rbuf(im2d+ip+k-1) = 1.0
            if (.not.lbuf(imo+k-1)) rbuf(im2d+ip+k-1) = -1.0 
          end do
        end do
c
c Write out to disk
c
        if (hog) then
c
c Write out to the next set of transpose files which will convert
c from zxy to xyz order
c
          call trnwrite (lv2, rbuf(iv2d))
          call trnwrite (lm2, rbuf(im2d))
        else
c
c Write zxy images into scratch files in xyz order.  This will be slow.
c
          do i = 1, no(1)
            do k = 1, no(3)
              ip = k + (i-1)*no(3) - 1
              ioff = i + (j-1)*no(1) + (k-1)*no(1)*no(2) - 1
c
              call scrwrite (lscr1, rbuf(iv2d+ip), ioff, 1)
              call scrwrite (lscr2, rbuf(im2d+ip), ioff, 1)
            end do
          end do
        end if
      end do
c
c Free up memory
c
      call memfree (iabi, ni(3), 'd')
      call memfree (iabo, no(3), 'd')
      call memfree (ivo,  no(3), 'r')
      call memfree (imo,  no(3), 'l')
      call memfree (iv2d, nz*no(1), 'r')
      call memfree (im2d, nz*no(1), 'r')
c
      call trnfin (lv1)
      call trnfin (lm1)
c
c***********************************************************************
c
c Read in the transposed cube, which is now in xyz order again,
c and write it out to the output image.
c
      if (hog) then
        call memalloc (iv2d, no(1)*no(2),  'r')
      else
        call memalloc (ivo, no(1), 'r')
      end if
      call memalloc (imo, no(1),  'l')
c
      do i = 1, no(1)
        lbuf(imo+i-1) = .true.
      end do
c
      call output ('Begin final transposition and output to image')
c
      do k = 1, no(3)
        call xysetpl (lo, 1, k)
        if (hog) then
c
c Read data from transpose routines; do data first
c
          call trnread (lv2, rbuf(iv2d))
          do j = 1, no(2)
            ioff = (j-1)*no(1)
            call xywrite (lo, j, rbuf(iv2d+ioff))
          end do
c
c Now do mask
c
          call trnread (lm2, rbuf(iv2d))
          do j = 1, no(2)
            ioff = (j-1)*no(1)
            do i = 1, no(1)
              lbuf(imo+i-1) = .true.
              if (rbuf(iv2d+ioff+i-1).lt.0.0) lbuf(imo+i-1) = .false.
            end do            
            call xyflgwr (lo, j, lbuf(imo))
          end do
        else
c
c Read data from xyz order scratch files and write it out
c
          do j = 1, no(2)
            ioff = (j-1)*no(1) + (k-1)*no(1)*no(2)
            call scrread (lscr1, rbuf(ivo), ioff, no(1))
            call xywrite (lo, j, rbuf(ivo))
c
            call scrread (lscr2, rbuf(ivo), ioff, no(1))
            do i = 1, no(1)
              lbuf(imo+i-1) = .true.
              if (rbuf(ivo+i-1).lt.0.0) lbuf(imo+i-1) = .false.
            end do
            call xyflgwr (lo, j, lbuf(imo))
          end do
        end if
      end do
c
c Free up memory
c
      if (hog) then
        call trnfin (lv2)
        call trnfin (lm2)
        call memfree (iv2d, no(1)*no(2),  'r')
      else
        call scrclose (lscr1)
        call scrclose (lscr2)
        call memfree (ivo, no(1), 'r')
      end if
      call memfree (imo, no(1),  'l')
c
      end
c
c
      subroutine l2r (no, iv2d, im2d)
c-----------------------------------------------------------------------
c     Transfer the mask in the yx logical to the yx real image
c     so that it can be written out to the transpose routines
c     This is used by the code that regrids axes 1,2,3 and 2,3
c
c  Input
c   no     Length of output image axes
c   iv2d   Pointer to real array in common
c   im2d   Pointer to logical array in common
c
c-----------------------------------------------------------------------
      implicit none
      integer no(*), iv2d, im2d
cc
      include 'maxdim.h'
      real rbuf(maxbuf)
      logical lbuf(maxbuf)
      integer i
c
      common rbuf
      equivalence (rbuf, lbuf)
c-----------------------------------------------------------------------
      do i = 1, no(2)*no(1)
        rbuf(iv2d+i-1) = 1.0
        if (.not.lbuf(im2d+i-1)) rbuf(iv2d+i-1) = -1.0
      end do
c
      end
c
c
      subroutine genabv (ni, crpix, cdelt, crval, mask, x, val, no)
c-----------------------------------------------------------------------
c     Generate arrays of abcissa and data value excluding all
c     blanked values.
c
c  Input
c    ni    Number of input pixels
c    mask  Input mask
c    cr*   Axis descriptors
c  Input/output
c    val   Input data values.  Blanked pixels excluded on output.
c  Output
c    x     INput abcissa values.  Blanked pixels excluded on output.
c    no    Number of pixels after excluding blanks
c
c-----------------------------------------------------------------------
      implicit none
c
      integer ni, no
      logical mask(ni)
      real val(ni)
      double precision x(ni), crval, cdelt, crpix
cc
      integer ii, i
c-----------------------------------------------------------------------
      ii = 1
      do i = 1, ni
        if (mask(i)) then
c
c Only keep unblanked pixels
c
          x(ii) = (dble(i)-crpix)*cdelt + crval
          val(ii) = val(i)
          ii = ii + 1
        end if
      end do
      no = ii - 1
c
      end
c
c
      subroutine genabv2 (ni, crpix, cdelt, crval, mask, x, val, no)
c-----------------------------------------------------------------------
c     Generate arrays of abcissa and data value excluding all
c     blanked values.  Same as genabv except that mask stored as
c     real not logical
c
c  Input
c    ni    Number of input pixels
c    mask  Input mask
c    cr*   Axis descriptors
c  Input/output
c    val   Input data values.  Blanked pixels excluded
c  Output
c    x     INput abcissa values.  Blanked pixels excluded
c    no    Number of pixels after excluding blanks
c
c-----------------------------------------------------------------------
      implicit none
c
      integer ni, no
      real val(ni), mask(ni)
      double precision x(ni), crval, cdelt, crpix
cc
      integer ii, i
c-----------------------------------------------------------------------
      ii = 1
      do i = 1, ni
        if (mask(i).gt.0.0) then
c
c Only keep unblanked pixels
c
          x(ii) = (dble(i)-crpix)*cdelt + crval
          val(ii) = val(i)
          ii = ii + 1
        end if
      end do
      no = ii - 1
c
      end
c
c
      subroutine row2col (i, ni, nj, rinp, linp, rout, lout)
c-----------------------------------------------------------------------
c     Write a row (real and logical)  into a specified column of
c     a matrix (real and logical)
c
c  Input
c   i      Column to write into
c   ni     Size of matrix in first dimension
c   nj     Size of matrix in second dimension and length of row
c   rinp   Input real row
c   linp   Input logical row  
c  Input/output
c   rout   Real matrix
c   lout   logical matrix
c
c-----------------------------------------------------------------------
      implicit none
      integer ni, nj, i
      real rinp(nj), rout(ni,nj)
      logical linp(nj), lout(ni,nj)
cc
      integer j
c-----------------------------------------------------------------------
      do j = 1, nj
        rout(i,j) = rinp(j)
        lout(i,j) = linp(j)
      end do
c
      end 
c
c
      subroutine fitline (ni, xi, yi, cdelt, no, xo, yo, mo)
c-----------------------------------------------------------------------
c     Spline fit an input array and return the fit evaluated
c     at specified locations
c
c  Input
c    ni     Number of input points
c    xi     Input abcissa values
c    yi     Input data values
c    cdelt  Input pixel increment
c    no     Number of output points
c    xo     Array of abcissa values at which to evaluate the spline fit
c  Output
c    yo     Spline fitted data
c    mo     Output mask
c
c-----------------------------------------------------------------------
      implicit none
c
      integer ni, no
      double precision xi(ni), xo(no), cdelt
      real yi(ni), yo(no)
      logical mo(no)
cc
      integer i
c-----------------------------------------------------------------------
      if (ni.le.1) then
c
c No input points; blank output              
c  
        do i = 1, no
          yo(i) = 0.0
          mo(i) = .false.
        end do
      else
c
c Fit spline and evaluate
c 
        if (cdelt.lt.0.0d0) then
          call flipd (ni, xi)
          call flipr (ni, yi)
        end if
        call myspline (ni, xi, yi, no, xo, yo, mo)
      end if
c
      end
c
c
      subroutine myspline (n1, x1, y1, n2, x2, y2, m2)
c-----------------------------------------------------------------------
c  For n1 input values (x1,y1) the n2 interpolated values
c  y2() on x2() are returned. Uses a cubic spline fit
c  (see Forsyth et al.)
c-----------------------------------------------------------------------
      implicit none
c
      integer n1, n2
      double precision x1(n1), x2(n2)
      logical m2(n2)
      real y1(n1), y2(n2)
cc
      include 'maxdim.h'
      double precision y1d(maxdim), b(maxdim), c(maxdim), d(maxdim)
      integer i
      logical interval
c
      double precision seval
c-----------------------------------------------------------------------
c
c Local copy, in double precision
c
      do i = 1, n1
         y1d(i) = y1(i)
      end do
c
c Compute spline coefficients
c
      call spline (n1, x1, y1d, b, c, d)
c
c Interpolate on the requested grid. Return as REAL
c No extrapolation allowed - 0.0 returned
c
      do i = 1, n2
         if (interval(x1(1),x1(n1),x2(i))) then
            y2(i) = real(seval(n1, x2(i), x1, y1d, b, c, d))
            m2(i) = .true.
         else
            y2(i) = 0.0
            m2(i) = .false.
         end if
      end do
c
      end
c
c
      logical function interval (a, b, x)
c-----------------------------------------------------------------------
c  determine is x is in the interval [a,b] or [b,a]
c
c-----------------------------------------------------------------------
      implicit none
c
      double precision a, b, x
c-----------------------------------------------------------------------
      interval = .false.
      if (a.lt.b) then
        if (a.le.x .and. x.le.b) interval = .true.
      else if (b.lt.a) then
        if (b.le.x .and. x.le.a) interval = .true.
      else
        if (x.eq.a) interval = .true.
      end if
c
      end
c
c
      subroutine flipd (n, f)
c-----------------------------------------------------------------------
c     Flip a double array
c-----------------------------------------------------------------------
      implicit none
c
      integer n, n1, i
      double precision f(n), tmp
c-----------------------------------------------------------------------
      if (n.lt.1) call bug ('f', 'FLIPD: Cannot flip with n<0')
      n1 = n/2
      do i = 1, n1
        tmp = f(i)
        f(i) = f(n-i+1)
        f(n-i+1) = tmp
      end do
c
      end
c
c
      subroutine flipr (n, f)
c-----------------------------------------------------------------------
c     FLip a real array
c-----------------------------------------------------------------------
      implicit none
c
      integer n, n1, i
      real f(n), tmp
c-----------------------------------------------------------------------
      if (n.lt.1) call bug ('f', 'FLIPR: Cannot flip with n<0')
      n1 = n/2
      do i = 1, n1
        tmp = f(i)
        f(i) = f(n-i+1)
        f(n-i+1) = tmp
      end do
c
      end
c
c
      subroutine flipl (n, f)
c-----------------------------------------------------------------------
c     Flip a logical array
c-----------------------------------------------------------------------
      implicit none
c
      integer n, n1, i
      logical f(n), tmp
c-----------------------------------------------------------------------
      if (n.lt.1) call bug ('f', 'FLIPL: Cannot flip with n<0')
      n1 = n/2
      do i = 1, n1
        tmp = f(i)
        f(i) = f(n-i+1)
        f(n-i+1) = tmp
      end do
c
      end
c
c
      subroutine openim (do, lun, file, maxnax, naxis, nsize, crval, 
     +                   crpix, cdelt, ctype, cosd, ira, idec)
c-----------------------------------------------------------------------
c   Open an image and return some header information
c-----------------------------------------------------------------------
      implicit none
      include 'maxdim.h'
      integer lun, maxnax, nsize(maxnax), naxis, ira, idec
      double precision crval(maxnax), cdelt(maxnax), crpix(maxnax), cosd
      character file*(*), ctype(maxnax)*(*)
      logical do(maxnax)
cc
      integer i
      character itoaf*1, str*1
      logical dora
c-----------------------------------------------------------------------
c
c  Open file
c
      call xyopen (lun, file, 'old', maxnax, nsize)
      call rdhdi (lun, 'naxis', naxis, 0)
      if (naxis.eq.0) call bug ('f', 'No axes in this image')
      naxis = min(naxis,maxnax)
c
c  Read items
c
      do i = 1, naxis
        str = itoaf(i)
        call rdhdd (lun, 'crval'//str, crval(i), 0.0d0)
        call rdhdd (lun, 'cdelt'//str, cdelt(i), 0.0d0)
        call rdhdd (lun, 'crpix'//str, crpix(i), 0.0d0)
        call rdhda (lun, 'ctype'//str, ctype(i), ' ')
      end do
      if (nsize(1).gt.maxdim) call bug('f','Input file too big for me')
c
c  Look for RA and DEC axes 
c
      call axfndcg ('RA', naxis, ctype, ira)
      call axfndcg ('DEC', naxis, ctype, idec)
      cosd = 1.0
      if (ira*idec.ne.0) cosd = cos(crval(idec))
c
c  Are we going to regrid the RA axis ?
c
      dora = .false.
      do i = 1, naxis
        if (do(i) .and. i.eq.ira) dora = .true.
      end do
c
      if (dora .and. ira.ne.0 .and. idec.eq.0) then
        call bug ('w', 'RA axis present but no DEC axis. Cannot find')
        call bug ('w', 'cos(DEC) so regridded RA axis may be incorrect')
      end if
c
      end
c
c
      subroutine report (maxnax, faci, faco, do, nsizei, ctypei, crvali,
     +   crpixi, cdelti, nsizeo, ctypeo, crvalo, crpixo, cdelto)
c-----------------------------------------------------------------------
c     Tell user what's happening
c
c-----------------------------------------------------------------------
      implicit none
      integer maxnax, nsizei(maxnax), nsizeo(maxnax)
      double precision crvali(maxnax), crvalo(maxnax), cdelti(maxnax),
     +  cdelto(maxnax), crpixi(maxnax), crpixo(maxnax), faci(maxnax),
     +  faco(maxnax)
      character ctypei(maxnax)*(*), ctypeo(maxnax)*(*)
      logical do(maxnax)
cc
      integer i
      character itoaf*1, str*1
c-----------------------------------------------------------------------
      do i = 1, maxnax
        if (do(i)) then
          str = itoaf(i)
          call output ('Regridding information for axis '//str)        
          call report2 ('in', ctypei(i), crvali(i), cdelti(i), 
     +                   crpixi(i), nsizei(i), faci(i))
          call output (' ')
          call report2 ('out', ctypeo(i), crvalo(i), cdelto(i),
     +                   crpixo(i), nsizeo(i), faco(i))
          call output (' ')
        end if
      end do
c
      end
c
c
      subroutine report2 (inout, ctype, crval, cdelt, crpix, nsize, fac)
      implicit none
      integer nsize
      character*(*) ctype, inout
      double precision crval, cdelt, crpix, fac
cc
      character*80 mesg, str*6
c 
      if (inout.eq.'in') then
        str = 'Input'
      else
        str = 'Output'
      end if
c
      write (mesg, '(3a)')  str, ' axis type ', ctype
      call output(mesg)
c
      write (mesg, '(a,a,i5,10X,a,1pe15.8)') str, ' naxis=', nsize,
     +                                            '  crval=',crval
      call output(mesg)
c
      write (mesg, '(a,a,1pe15.8,a,1pe15.8)') str, ' crpix=', crpix,
     +                                             '  cdelt=',cdelt
      call output(mesg)
c
      write (mesg, '(a,a,1pe15.8,a,1pe15.8)') str, ' start=',
     +                                  (1.0-crpix)*cdelt/fac+crval,
     +                                     '    end=',
     +                                  (nsize-crpix)*cdelt/fac+crval
      call output(mesg)
c
      end
c
c
      subroutine getopt (hog, relax)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     hog       Be greedy
c     relax     Be nice
c
c-----------------------------------------------------------------------
      implicit none
c
      logical hog, relax
cc
      integer maxopt
      parameter (maxopt = 2)
c
      character opshuns(maxopt)*6
      logical present(maxopt)
      data opshuns /'nohog', 'relax'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      hog   = .not.present(1)
      relax =      present(2)
c
      end
