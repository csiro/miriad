c**********************************************************************
c     A collection of subroutines shared by the programs CGDISP,
c     CGSPEC, CGCURS, CGSLICE, and IMBIN.
c
c
c  angconcg :  Convert between radians and seconds of arc/time
c  apptrfcg :  Apply transfer function to image
c  chkdescg :  Compare a real axis descriptor from two images
c  chnselcg :  Make list of CHAN and REGION selected channel groups
c  conlevcg :  Compute contour levels
c  copyimcg :  Copy image
c  defchrcg :  Give a default char. height for axis & velocity labels
c  dolabcg  :  See if x or y axes need labelling (numeric and/or ascii)
c  grfixcg  :  Fix up a grey scale range (includes log taking)
c  heqcg    :  Histogram equalize an image
c  limitscg :  Work out limits and transformation matrix for both axes
c  maskorcg :  OR mask of mask image with mask of data image
c  matchcg  :  Match string with allowed types
c  nxnycg   :  Work out number of sub-plots per page
c  opimcg   :  Open an image and return axis descriptors
c  optcg    :  Version of BobS options, but without the fatalities
c  ol2pixcg :  Convert overlay location in specified coords to pixels
c  ppconcg  :  Convert between unbinned full image pixels and binned
c              subimage pixels
c  razerocg :  See if an RA axis crosses RA = 0
c  readbcg  :  Read blanking mask form mask image
c  readimcg :  Read in image dealing with averaging and blanking
c  setccscg :  Set rjs coordinate conversion strings for tick labelling
c  setcolcg :  Set a PGPLOT colour for multiple line graphics on 1 plot
c  setlabcg :  Set axis labels
c  strprpcg :  Prepare string by stripping extra white space and
c              delimitering fields by commas
c  subinccg :  Step to next sub-plot
c  wedgincg :  Work out if greys cale wedges inside ro outside subplots
c  windfidcg:  Adjust window size to fit an integral number of bins
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c $Id: cgsubs.for,v 1.8 2012/02/15 03:23:50 cal103 Exp $
c***********************************************************************

c* angconCG -- Convert radians to and from seconds of time/arc
c& nebk
c: plotting
c+
      subroutine angconcg (id, labtyp, w)

      character*(*) labtyp
      double precision w
      integer id
c  ---------------------------------------------------------------------
c  Convert RA/DEC axis world coordinates between seconds (arc/time)
c  and radians.
c
c  Input
c    id       1 -> convert from radians
c             2 -> convert to   radians
c    labtyp   axis label type
c                hms    seconds of time
c                dms    arc seconds
c  Input/Ouput
c    w        world coordinate. SHould be radians (id=1), or
c             (id=1) seconds of arc ('dms') or time ('hms')
c-----------------------------------------------------------------------
      include 'mirconst.h'
      double precision st2r
      parameter (st2r=dpi/3600d0/12d0)
c-----------------------------------------------------------------------
      if (id.eq.1) then
c
c From radians
c
        if (labtyp.eq.'hms') then
          w = w / st2r
        else if (labtyp.eq.'dms') then
          w = w / AS2R
        endif
      else if (id.eq.2) then
c
c To radians
c
        if (labtyp.eq.'hms') then
          w = w * st2r
        else if (labtyp.eq.'dms') then
          w = w * AS2R
        endif
      else
        call bug('f', 'ANGCONCG: unrecognized conversion code')
      endif

      end

c**********************************************************************

c* apptrfCG -- Apply transfer function to image
c& nebk
c: plotting
c+
      subroutine apptrfcg (pixr, trfun, groff, size, nimage, image,
     *                     nbins, his, cumhis)

      integer nimage(*), size, nbins, his(nbins)
      real groff, image(*), pixr(2), cumhis(*)
      character trfun*3
c  ---------------------------------------------------------------------
c  Apply the desired transfer function to the image
c
c  Input:
c   pixr     Intensity range with NO bias or logs/sqrt taken
c   trfun    Transfer function.  "lin", "log", "heq" or "sqr"
c   groff    Bias to make image positive if necessary
c   size     Size of image
c   nimage   Normalization image
c   nbins    Number of bins for histogram equalization
c  Input/output:
c   image    Image.  Transfer function applied on output. Pixels
c            below pixr(1) are set equal to pixr(1)
c   his      Image histogram for histogram equalization
c   cumhis   Cumulative histogram for histogram equalization
c            Values for each bin are the intensities assigned to
c            the image.  Thus if an image pixel ended up in
c            cumhis bin idx, then its new value is cumhis(idx)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      if (trfun.eq.'log') then
        do i = 1, size
          if (nimage(i).ne.0) then
            if (image(i).lt.pixr(1)) image(i) = pixr(1)
            image(i) = log10(image(i)-groff)
          endif
        enddo
      else if (trfun.eq.'sqr') then
        do i = 1, size
          if (nimage(i).ne.0) then
            if (image(i).lt.pixr(1)) image(i) = pixr(1)
            image(i) = sqrt(image(i)-groff)
          endif
        enddo
      else if (trfun.eq.'heq') then
        call heqcg(pixr, size, nimage, image, nbins, his, cumhis)
      endif

      end

c**********************************************************************

c* chkdesCG -- Compare double precision axis descriptor from two images
c& nebk
c: plotting
c+
      subroutine chkdescg (relax, type, iaxis, im1, im2, des1, des2)

      character type*(*), im1*(*), im2*(*)
      integer iaxis
      double precision des1, des2
      logical relax
c  ---------------------------------------------------------------------
c  Compare a double precision axis descriptor from two images
c
c  Input:
c    type    Type of descriptor
c    iaxis   Axis number
c    im1,2   Images
c    des1,2  Descriptors
c-----------------------------------------------------------------------
      double precision desmax
      character line*130
      integer len1
c-----------------------------------------------------------------------
      desmax = max(abs(des1),abs(des2))
      if (abs(des1-des2).gt.desmax*1d-6 .or. des1*des2.lt.0d0) then
        write(line, 10) type, im1(1:len1(im1)), im2(1:len1(im2)),
     *                   iaxis
10      format('CHKDESCG: Unequal ', a, ' for images ', a, ' & ', a,
     *          ' on axis ', i1)
        if (relax) then
          call bug('w', line)
        else
          call bug('i',
     *       'CHKDESCG: You might consider, with care, OPTIONS=RELAX')
          call bug('f', line)
        endif
      endif

      end

c**********************************************************************

c* chnselCG -- Make list of CHAN and REGION selected channel groups
c& nebk
c: plotting
c+
      subroutine chnselcg (blc, trc, kbin, maxbox, boxes, ngrps,
     *                     grpbeg, ngrp)

      integer maxbox, boxes(maxbox), grpbeg(*), ngrp(*), ngrps, kbin(2),
     *  blc(3), trc(3)
c  ---------------------------------------------------------------------
c  Find the channels designated by the CHAN and REGION specifiations
c  via the RUNS arrays.    Note that currently, none of the
c  CG programs call BOXMASK so that all planes offered by the
c  REGION keyword are selected by CHANSELCG as being good.
c  Blanked pixels are dealt with by READIMCG
c
c  Input
c    blc,trc    Cube surrounding region of interest
c    kbin       Channel increment and average to step through image.
c               If kbin(1) and kbin(2) = 0 then output groups begin
c               every time the selected channels are non-contiguous
c               and the end channel of each group is made equal to
c               the number of contiguous channels in that group
c    maxbox     Maximum number of boxes
c    boxes      Boxes following BOXINPUT,BOXSET,BOXINFO (optional) and
c               BOXMASK
c  Output
c    ngrps      Number of groups of channels
c    grpbeg     Array of start channels for each group of selected
c               channels.  Each group will be averaged together to
c               make one sub-plot
c    ngrp       Number of channels in each group
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer maxruns
      parameter (maxruns = 10*maxdim)

      integer runs(3,maxruns), nruns, i, j, k, xmin, xmax, ymin,
     *        ymax, inc, ave, last, jend, start(maxchan), end(maxchan)
c-----------------------------------------------------------------------
      inc = kbin(1)
      ave = kbin(2)
      if ((inc.eq.0 .and. ave.ne.0) .or. (inc.ne.0 .and. ave.eq.0))
     *  call bug('f', 'CHNSELCG: invalid channel inc/ave values')
c
c Find first good plane
c
      start(1) = 0
      j = blc(3)
      do while (start(1).eq.0 .and. j.le.trc(3))
        call boxruns(1, j, ' ', boxes, runs, maxruns,
     *                nruns, xmin, xmax, ymin, ymax)

        if (nruns.gt.0) start(1) = j
      enddo
      if (start(1).eq.0) call bug('f',
     *  'CHNSELCG: There were no valid pixels in the region')
c
c Loop over remaining planes, and accumulate start and end channels
c for each run of contiguous channels
c
      if (start(1).eq.trc(3)) then
c
c Special case if first good channel is last available channel
c
        end(1) = trc(3)
        ngrps = 1
      else
        k = 1
        last = start(1)
        do j = start(1)+1, trc(3)
          call boxruns(1, j, ' ', boxes, runs, maxruns,
     *                  nruns, xmin, xmax, ymin, ymax)

          if (nruns.eq.0) then
c
c The current channel is not wanted. Assign the last good
c channel as the end of the current group, and indicate
c we need to start a new group.
c
            if (last.ne.0) end(k) = last
            last = 0
          else
c
c This is a good channel, start a new group if needed
c and note this is currently the last good channel
c
            if (last.eq.0) then
               k = k + 1
               start(k) = j
            endif
            last = j
          endif
        enddo
c
c Assign the end channel to the last group if need be
c
        if (last.ne.0) end(k) = last
        ngrps = k
      endif
c
c Special case now if kbin(1) = kbin(2) = 0  Just return
c the contiguous groups
c
      if (kbin(1).eq.0 .and. kbin(2).eq.0) then
        do k = 1, ngrps
          grpbeg(k) = start(k)
          ngrp(k) = end(k) - start(k) + 1
        enddo
      else
c
c Now loop over the number of groups of contiguous channels
c and divide up into smaller collections of channels reflecting
c the users averaging and incrementing requests
c
        i = 0
        do k = 1, ngrps
c
c Loop over the channels in this group
c
          do j = start(k), end(k), inc
            i = i + 1
            if (i.gt.maxchan) call bug('f',
     *        'CHNSELCG: You have selected too many groups of channels')
            jend = min(j+ave-1,end(k))
c
c Assign start and end channels for this group. These channels
c will be averaged together to make one subplot
c
            grpbeg(i) = j
            ngrp(i) = jend - j + 1
          enddo
        enddo
        ngrps = i
      endif

      end

c**********************************************************************

c* conlevCG -- Compute contour levels
c& nebk
c: plotting
c+
      subroutine conlevcg (mirror, maxlev, lcin, levtyp, slev, nlevs,
     *                     levs, srtlev)

      integer lcin, nlevs, maxlev, srtlev(maxlev)
      real slev, levs(maxlev)
      character*1 levtyp
      logical mirror
c  ---------------------------------------------------------------------
c  Compute contour levels
c
c  Input:
c    mirror   MUltiply specified contour levsls by -1 and add to list
c    maxlev   Maximum number of levels allowed
c    lcin     Handle of contour image
c  Input/output:
c    levtyp   Type of scale factor (percentage or absolute)
c    slev     Contour scale factor
c    nlevs    Number of contour levels
c  Output:
c    levs     Contour levels
c    srtlev   Indexes of array giving levels in increasing order
c-----------------------------------------------------------------------
      include 'maxnax.h'
      integer i, ilev, mlevs, cnaxis, csize(maxnax)
      real cdmin, cdmax, off, inc
      character*1 itoaf
c-----------------------------------------------------------------------
      call rdhdi(lcin, 'naxis', cnaxis, 0)
      do i = 1, cnaxis
        call rdhdi(lcin, 'naxis'//itoaf(i), csize(i), 0)
      enddo

      mlevs = nlevs
      if (nlevs.eq.0) then
c
c Set default contours
c
        call imminmax(lcin, cnaxis, csize, cdmin, cdmax)

        if (cdmax.gt.0.0 .and. cdmin.lt.0.0) then
           slev = max(abs(cdmax), abs(cdmin)) / 8

           nlevs = abs(cdmin) / slev
           ilev = 1
           do i = -nlevs, -1, 1
             levs(ilev) = i * slev
             ilev = ilev + 1
           enddo

           nlevs = cdmax / slev
           do i = 1, nlevs, 1
             levs(ilev) = i * slev
             ilev = ilev + 1
           enddo

           nlevs = ilev - 1
           slev = 1.0
           levtyp = 'a'
        else
           off = 0.05 * (cdmax - cdmin)
           nlevs = 10
           inc = ((cdmax-off) - (cdmin+off)) / (nlevs - 1)
           do i = 1, nlevs
              levs(i) = cdmin+off + (i-1)*inc
           enddo

           slev = 1.0
           levtyp = 'a'
        endif
      else if (levtyp.eq.'p')  then
c
c Set percentage contours
c
        if (slev.eq.0.0) slev = 1.0
        call imminmax(lcin, cnaxis, csize, cdmin, cdmax)
        slev = slev * cdmax / 100.0
      else if (levtyp.eq.'a') then
c
c Absolute contours
c
        if (slev.eq.0.0) slev = 1.0
      endif
c
c Set mirrored contours only for user specified contours
c
      if (mirror .and. mlevs.ne.0) then
        mlevs = nlevs
        do i = 1, mlevs
          if (levs(i).ne.0.0) then
            if (nlevs.lt.maxlev) then
              nlevs = nlevs + 1
              levs(nlevs) = -1.0*levs(i)
            else
              call bug('w',
     *        'CONLEVCG: Max. no. of contours reached during mirroring')
              goto 100
            endif
          endif
        enddo
      endif
c
c Scale levels
c
100   do i = 1, nlevs
        levs(i) = levs(i) * slev
      enddo
c
c Sort in increasing order
c
      call sortidxr(nlevs, levs, srtlev)

      end

c**********************************************************************

c* copyimCG -- Copy image
c& nebk
c: plotting
c+
      subroutine copyimcg (n, in, copy)

      integer n
      real in(n), copy(n)
c  ---------------------------------------------------------------------
c  Copy an image for safe keeping
c
c Input
c     n       Size of image
c     image   Image
c Output
c     copy    Copy of image
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        copy(i) = in(i)
      enddo

      end

c**********************************************************************

c* defchrCG -- Give a default char. height for axis & velocity labels
c& nebk
c: plotting
c+
      subroutine defchrcg (nx, ny, cs)

      real cs(*)
      integer nx, ny
c  ---------------------------------------------------------------------
c  Work out default character size for axis labels and for velocity
c  label.  Add a linear ramp otherwise they come out too big for
c  single plots per page, and too small for multiple plots per page
c
c  Input:
c    nx,ny     Number of sub-plots in x and y directions
c  Input/output:
c    cs        PGPLOT character sizes for axis labels and velocity label
c-----------------------------------------------------------------------
c
c Axis labels
c
      if (cs(1).le.0.0) then
        cs(1) = 1.2 / max(nx,ny)
        cs(1) = (0.13*max(nx,ny) + 0.67) * cs(1)
      endif
c
c Velocity/frequency/channel labels
c
      if (cs(2).le.0.0) then
        cs(2) = 1.2 / max(nx,ny)
        cs(2) = (0.13*max(nx,ny) + 0.67) * cs(2)
      endif

      end

c**********************************************************************

c* dolabCG -- See if axis needs a character and/or numeric label
c& nebk
c: plotting
c+
      subroutine dolabcg (gaps, dotr, nx, ny, nz, nlast, iplot, labtyp,
     *                    doaxlab, doaylab, donxlab, donylab)

      integer nx, ny, nz, nlast, iplot
      logical gaps, dotr, doaxlab, doaylab, donxlab(2), donylab(2)
      character labtyp(2)*(*)
c  ---------------------------------------------------------------------
c  Label axes and prepare options strings for PGTBOX according to
c  whether the sub-plots abut each other or not.
c
c  Input
c    gaps    False means sub-plots abut, else they don't
c    dotr    Label top and right axes too.
c    nx,ny   Number of sub-plots in x and y directions on page
c    nz      Total number of sub-plots that will be drawn
c    nlast   Number of sub-plots on the last row of the last page
c    iplot   Number of current sub-plot
c    labtyp  Axis label types
c  Output
c    doaxlab True to draw character label for x axis
c    doaylab True to draw character label for y axis
c    donxlab True to draw x numeric label; (1) for bottom, (2) for top
c    donylab True to draw y numeric label; (1) for left, (2) for right
c-----------------------------------------------------------------------
      integer jplot
c-----------------------------------------------------------------------
c
c Number if subplot on current page
c
      jplot = mod(iplot,nx*ny)
      if (jplot.eq.0) jplot = nx*ny

      doaxlab = .false.
      doaylab = .false.
      donxlab(1) = .false.
      donxlab(2) = .false.
      donylab(1) = .false.
      donylab(2) = .false.

      if (.not.gaps) then
c
c Only put character and numeric labels along the bottom for the bottom
c row
c
        if (labtyp(1).ne.'none') then
          if (jplot.ge.nx*ny-nx+1 .or. iplot.ge.nz-nlast+1 .or.
     *      iplot+nx.gt.nz) then
            doaxlab = .true.
            donxlab(1) = .true.
          endif
c
c Only put top numeric labels on top row of subplots
c
          if (dotr .and. jplot.le.nx) donxlab(2) = .true.
        endif
c
c Now y axis
c
        if (labtyp(2).ne.'none') then
c
c Only put character and numeric labels along the left
c for the leftmost column
c
          if (mod(jplot,nx).eq.1 .or. nx.eq.1) then
            doaylab = .true.
            donylab(1) = .true.
          endif
c
c Only write right axis label if rightmost subplot
c
          if (dotr .and. (mod(jplot,nx).eq.0 .or. iplot.eq.nz))
     *       donylab(2) = .true.
        endif
      else
c
c Each subplot separated by gaps, always write labels
c
        if (labtyp(1).ne.'none') then
          doaxlab = .true.
          donxlab(1) = .true.
          if (dotr) donxlab(2) = .true.
        endif
        if (labtyp(2).ne.'none') then
          doaylab = .true.
          donylab(1) = .true.
          if (dotr) donylab(2) = .true.
        endif
      endif

      end

c**********************************************************************

c* grfixCG -- Fix grey scale range with optional bias for taking logs
c& nebk
c: plotting
c+
      subroutine grfixcg (pixr, lgin, gnaxis, gsize, trfun,
     *                    pixr2, groff, blankg)

      real pixr(2), pixr2(2), groff, blankg
      integer lgin, gnaxis, gsize(*)
      character trfun*(*)
c  ---------------------------------------------------------------------
c  Make sure the grey scale range is valid, and take logs if desired.
c  This may require a DC bias to avoid negative numbers in the image.
c
c  Input:
c    lgin     Handle for image
c    gnaxis   Number of dimesions in image
c    gsize    Size of dimensions
c    trfun    'log', 'lin', 'heq', or 'sqr' transfer functions
c  Input/Output:
c    pixr     User supplied grey scale range.  Defaults filled in
c             on output
c  Output:
c    pixr2    Grey scale range with bias and logs/sqrt taken if
c             necessary
c    groff    DC bias to avoid negatives in image if logs taken
c    blankg   Value to use for blanked pixels
c-----------------------------------------------------------------------
      real fac
c-----------------------------------------------------------------------
c
c Set default range to data min to max
c
      if (pixr(1).eq.0.0 .and. pixr(2).eq.0.0) then
        call imminmax(lgin, gnaxis, gsize, pixr(1), pixr(2))
      else if (pixr(1).eq.pixr(2)) then
        call bug('w',
     *  'GRFIXCG: Zero pixel map range, reset to image intensity range')
        call imminmax(lgin, gnaxis, gsize, pixr(1), pixr(2))
      endif
c
c Work out offset if log or square root transfer function requested
c
      pixr2(1) = pixr(1)
      pixr2(2) = pixr(2)
      groff = 0.0
      fac = 100.0
      if (trfun.eq.'log' .or. trfun.eq.'sqr') then
        if (pixr(1).eq.0.0 .and. pixr(2).eq.0.0) call imminmax(lgin,
     *      gnaxis, gsize, pixr(1), pixr(2))
        if (pixr(1).le.0.0) groff = pixr(1) - (pixr(2)-pixr(1))/fac

        if (trfun.eq.'log') then
          pixr2(1) = log10(pixr(1)-groff)
          pixr2(2) = log10(pixr(2)-groff)
        else
          pixr2(1) = sqrt(pixr(1)-groff)
          pixr2(2) = sqrt(pixr(2)-groff)
        endif
      endif
c
c Set blanked pixel value to background colour
c
      blankg = pixr2(1) - (0.0001*(pixr2(2)-pixr2(1)))

      end

c**********************************************************************

c* heqCG -- Histogram equalize an image
c& nebk
c: plotting
c+
      subroutine heqcg (pixr, n, nimage, image, nbins, his, cumhis)

      integer nbins, n, nimage(n), his(nbins)
      real image(n), pixr(2), cumhis(nbins)
c  ---------------------------------------------------------------------
c  Apply histogram equalization to an image directly.  128 bins
c  are used in the histogram.
c
c  Input
c   pixr   Display intensity range
c   n      Number of pixels
c   nimage Normalization image
c   nbins  Number of bins in histogram
c  Input/output
c   image  Image
c   his    Image histogram
c   cumhis Cumulative histogram.  Values for each bin are
c          the intensities assigned to the image.  Thus
c          if an image pixel ended up in cumhis bin idx, then
c          its new value is cumhis(idx)
c-----------------------------------------------------------------------
      integer idx, i
      real fac, bmin, bmax, cum
c-----------------------------------------------------------------------
c
c Initialize histogram
c
      bmin = pixr(1)
      bmax = pixr(2)
      do i = 1, nbins
        his(i) = 0
        cumhis(i) = 0.0
      enddo
c
c Generate image histogram
c
      fac = real(nbins-1) / (bmax-bmin)
      do i = 1, n
        if (nimage(i).gt.0.0) then
          idx = max(1,min(nbins,nint((image(i)-bmin)*fac)+1))
          his(idx) = his(idx) + 1
        endif
      enddo
c
c Generate cumulative histogram.
c
      cum = 0.0
      do i = 1, nbins
        cum = cum + his(i)
        cumhis(i) = cum
      enddo
c
c Now discretize the cumulative histogram values as well
c
      fac = real(nbins-1) / real(n)
      do i = 1, nbins
c
c This index converts the actual cumulative histogram
c value to the nearest discrete bin
c
        idx = max(1,min(nbins,nint(cumhis(i)*fac)+1))
c
c Convert this bin back to an intensity and reuse CUMHIS array
c
        cumhis(i) = real(idx)/real(nbins)*(bmax-bmin) + bmin
      enddo
c
c Now fix the image pixels (including masked ones)
c
      fac = real(nbins-1) / (bmax-bmin)
      do i = 1, n
c
c Find cumulative histogram index of this pixel
c
        idx = max(1,min(nbins,nint((image(i)-bmin)*fac)+1))
c
c Replace by discretized cumulative histogram intensity
c
        image(i) = cumhis(idx)
      enddo

      end

c**********************************************************************

c* limitsCG -- Work out limits and transformation matrix for both axes
c& nebk
c: plotting
c+
      subroutine limitscg (blc, ibin, jbin, tr)

      integer blc(*), ibin, jbin
      real tr(6)
c  ---------------------------------------------------------------------
c   Work out window world coordinate limits and PGPLOT
c   transformation matrix
c
c     Input
c       blc          Spatial window BLC in unbinned pixels
c       i,jbin       x and y increments to step through image in
c     Output
c       tr           Matrix transforming from array indices to world
c                    coordinates.  Note this accounts for the fact that
c                    only the desired window is read into the data
c                    arrays, so there is a blc offset included in tr.
c                    It also accounts for any spatial binning.
c-----------------------------------------------------------------------
c
c No cross terms in transformation
c
      tr(3) = 0.0
      tr(5) = 0.0
c
c x axis
      tr(1) = blc(1) - 1.0 - (ibin-1)*0.5
      tr(2) = ibin
c
c y axis
c
      tr(4) = blc(2) - 1.0 - (jbin-1)*0.5
      tr(6) = jbin

      end

c**********************************************************************

c* maskorCG -- OR mask image mask with data image mask
c& nebk
c: plotting
c+
      subroutine maskorcg (blank, win, bimage, nimage, image)

      integer nimage(*), win(2)
      real image(*), blank
      logical bimage(*)
c  ---------------------------------------------------------------------
c  OR the mask image mask and the grey/contour/vector image mask
c
c  Input:
c    blank       Value to give blanked pixel
c    win         Size of image
c    bimage      The mask image mask.True is unflagged, false is flagged
c  Input/Output
c    nimage      The normalization image.  0-> blanked
c    image       The image.  New blanks may be set
c-----------------------------------------------------------------------
      integer i, imsize
c-----------------------------------------------------------------------
      imsize = win(1) * win(2)
      do i = 1, imsize
        if (.not.bimage(i)) then
          nimage(i) = 0
          image(i)  = blank
        endif
      enddo

      end

c**********************************************************************

c*matchCG -- Match fields with allowed types and die if no good
c:plotting
c+
      subroutine matchcg (n, field, string, struct, ntype, types)

      integer ntype, n
      character*(*) types(ntype), string, field, struct
c  ---------------------------------------------------------------------
c  Look for string in list of allowed ones.  If not found die with
c  fatal error.  Expand string for minimum match.  Extra variables
c  can be used to provide error messages.  These messages expect
c  that the string is one field from several fields making up
c  one structure, and that there are several structures being
c  examined.  For example, an overlay file for CGDISP expects
c  many rows, each describing one overlay.  Each row contains
c  several fields, each of which may take on different values.
c
c  Inputs:
c    ntype      The number of possible values that STRING can have
c    type       An array of possible values for STRING
c
c    n          Number of the thing that we are matching (used
c               in  error messages if non zero)
c    field      A string describing what field STRING is (used
c               in error messages if non blank).
c    struct     A string describing the generic name of the structure
c               from which STRING is one field (used in error messages
c               if non blank).
c               Examples are "overlay", "slice"
c  Input/output:
c    string     The string that we are trying to match.  It is
c               expanded from minimum  match on output
c
c  An example of an error message would be:
c
c   STRING is ambiguous for STRUCT # N field FIELD.  Choose from: ...
c
c  Thus, if STRING was "rel", STRUCT was "overlay", N was 14 and FIELD
c  was "xotype" then the message would read
c
c   "rel" is ambiguous for overlay # 14 field "xotype". Choose from: ...
c
c  Got it ??
c-----------------------------------------------------------------------
      integer l, i, iopt, j, il, il2
      integer len1
      character*130 umsg, str*10
c-----------------------------------------------------------------------
      l = len1(string)

      iopt = 0
      do i = 1, ntype
        if (string(1:l).eq.types(i)(1:l)) then
          if (iopt.ne.0) then
            umsg = '"'//string(1:l)//'" is ambiguous'
            il = len1(umsg) + 1

            if (struct.ne.' ' .and. n.gt.0) then
              umsg(il:) = ' for '//struct(1:len1(struct))
              il = len1(umsg) + 1

              call strfi(n, '(i4)', str, il2)
              umsg(il:) = ' (# '//str(1:il2)//')'
              il = len1(umsg) + 1
            endif

            if (field.ne.' ') then
              if (struct.eq.' ' .or. n.eq.0) then
                umsg(il:) =  ' for field "'//field(1:len1(field))//
     *                       '".  Choose from'
              else
                umsg(il:) =  ' field "'//field(1:len1(field))//
     *                       '".  Choose from'
              endif
            else
              umsg(il:) =  '. Choose from'
            endif

            call output(umsg)
            do j = 1, ntype
              umsg = '   '//types(j)
              call output(umsg)
            enddo
            call bug('f', 'MATCHCG:')
          endif
          iopt = i
        endif
      enddo
c
c Set expanded string
c
      if (iopt.ne.0) string = types(iopt)
c
c Didn't find nuttin
c
      if (iopt.eq.0) then
        umsg = '"'//string(1:l)//'" is unrecognized'
        il = len1(umsg) + 1

        if (struct.ne.' ' .and. n.gt.0) then
          umsg(il:) = ' for '//struct(1:len1(struct))
          il = len1(umsg) + 1

          call strfi(n, '(i4)', str, il2)
          umsg(il:) = ' (# '//str(1:il2)//')'
          il = len1(umsg) + 1
        endif

        if (field.ne.' ') then
          if (struct.eq.' ' .or. n.eq.0) then
            umsg(il:) =  ' for field "'//field(1:len1(field))//
     *                   '".  Choose from'
          else
            umsg(il:) =  ' field "'//field(1:len1(field))//
     *                   '".  Choose from'
          endif
        else
          umsg(il:) =  '. Choose from'
        endif

        call output(umsg)
        do j = 1, ntype
          umsg = '   '//types(j)
          call output(umsg)
        enddo
        call bug('f', 'MATCHCG:')
      endif

      end

c**********************************************************************

c* nxnyCG -- Work out number of sub-plots per page
c& nebk
c: plotting
c+
      subroutine nxnycg (nxdef, nydef, nz, nx, ny, nlast)

      integer nxdef, nydef, nx, ny, nz, nlast
c  ---------------------------------------------------------------------
c  Work out number of plots in the x and y directions and the
c  total number of plots
c
c  Inputs
c    nx,ydef   Default x and y numbers of sub-plots per page
c    nz        Total number of sub-plots
c  Outputs
c    nx,ny     Number of sub-plots in x and y directions per page
c    nlast     Number of sub-plots on the last row of the last page
c-----------------------------------------------------------------------
      if (nx.le.0 .or. ny.le.0) then
        if (nz.lt.nxdef*nydef) then
          nx = 1
          ny = 1
          do while (nz.gt.nx*ny)
            if (nx.eq.ny) then
              nx = nx + 1
            else
              ny = ny + 1
            endif
          enddo
        else
          nx = nxdef
          ny = nydef
        endif
      endif

      nlast = mod(nz,nx)
      if (nlast.eq.0) nlast = nx

      end

c**********************************************************************

c*OpImCG -- Open an image and return axis descriptors
c:plotting
c+
      subroutine opimcg (maxnax, in, lin, size, naxis)

      integer maxnax, lin, size(maxnax), naxis
      character*(*) in
c  ---------------------------------------------------------------------
c     Open an image and return some header descriptors
c
c   Input:
c     maxnax     Maximum number of allowed axes
c     in         Image name
c   Output:
c     lin        Handle for image
c     size       SIze of each dimension of image
c     naxis      Number of dimensions
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer len1
      character msg*132
c-----------------------------------------------------------------------
      call xyopen(lin, in, 'old', maxnax, size)
      call rdhdi(lin, 'naxis', naxis, 0)
      if (naxis.eq.0) then
        msg = in(1:len1(in))//' has zero dimensions !!'
        call bug('f', msg)
      endif

      if (naxis.gt.maxnax) then
        msg = 'OPIMCG: '//in(1:len1(in))//
     *        ' has too many dimensions'
        call bug('f', msg)
      endif

      if (size(1).gt.maxdim) then
        msg = 'OPIMCG: '//in(1:len1(in))//
     *        ' first dimension too large'
        call bug('f', msg)
      endif

      end

c**********************************************************************

c*OptCG -- Get command line options but only warn if unrecognized.
c:plotting
c+
      subroutine optcg (key, opts, present, nopt)

      character key*(*)
      integer nopt
      character opts(nopt)*(*)
      logical present(nopt)
c  ---------------------------------------------------------------------
c  Get options from the command line, and return to the caller those
c  options that are present.
c
c  Unrecognized options generate a warning only, unlike the standard
c  Miriad subroutine that does this which issues a fatal error.
c
c  Inputs:
c    key        The task keyword to use.
c    opts       An array of possible option values.  These should be in
c               lower case.
c    nopt       The number of possible options.
c  Output:
c    present    This indicates whether the option was present.
c-----------------------------------------------------------------------
      integer l,i,iopt
      character string*16, umsg*80

      external  len1
      integer   len1
c-----------------------------------------------------------------------
c
c  Initialise the options to indicate that none are present.
c
      do i = 1, nopt
        present(i) = .false.
      enddo
c
c  Search the task parameters.
c
      call keya(key, string, ' ')
      do while (string.ne.' ')
        l = len1(string)
        call lcase(string(1:l))
        umsg = 'OPTCG: Unrecognised option "'//string(1:l)//'"'
        if (l.gt.len(opts(1))) call bug('f', umsg)
        iopt = 0
        do i = 1, nopt
          if (string(1:l).eq.opts(i)(1:l)) then
            umsg = 'OPTCG: Ambiguous option "'//string(1:l)//'"'
            if (iopt.ne.0) call bug('f', umsg)
            iopt = i
          endif
        enddo

        if (iopt.eq.0) then
          umsg = 'OPTCG: Unrecognised option "'//string(1:l)//'"'
          call bug('w', umsg)
        else
          present(iopt) = .true.
        endif
        call keya(key, string, ' ')
      enddo

      end

c**********************************************************************

c* ol2pixCG -- Convert overlay location in given coordinates to pixels
c& nebk
c: plotting
c+
      subroutine ol2pixcg (lun, pix3, otype, off, dsign, nums, opos, np)

      double precision off(*), nums(*), opos(*), pix3
      integer lun, dsign(2), np
      character*(*) otype(2)
c  ---------------------------------------------------------------------
c Convert overlay location from OTYPE units to pixels.  This is
c a conversion from true world coordinates to image pixels
c
c Input
c   lun       Handle of image
c   pix3      Third axis pixel (channel) that we are displaying
c             this overlay on
c   otype     Overlay units  for each axis ('arcsec' etc)
c   dsign     SIgn for dms axes. +/-1
c   nums      The array of numbers read from the overlay file
c             starting with the first one to use here.  These
c             are in true coordinates such as those given by IMPOS
c   off       Offset to be added to the locations decoded from
c             the text file and held in NUMS.  These are in the same
c             units as OTYPE so no conversion is done.  Is ignored
c             for RA and DEC because I am too lazy.
c Output
c   opos      Output location in pixels for x and y
c   np        This is the number of locations of the NUMS array used
c-----------------------------------------------------------------------
      include 'mirconst.h'
      double precision win(3), wout(3)
      character*6 typei(3), typeo(3)
      integer ip, i, naxis
c-----------------------------------------------------------------------
c
c Prepare coordinates for conversion
c
      ip = 1
      do i = 1, 2
        if (otype(i).eq.'hms') then
          win(i) = nums(ip) + nums(ip+1)/60d0 + nums(ip+2)/3600d0
          win(i) = win(i) * dpi / 12d0
          ip = ip + 3
        else if (otype(i).eq.'dms') then
          win(i) = abs(nums(ip)) + nums(ip+1)/60d0 +
     *             nums(ip+2)/3600d0
          win(i) = dsign(i) * abs(win(i)) * dpi / 180d0
          ip = ip + 3
        else
          win(i) = nums(ip) + off(i)
          ip = ip + 1
        endif

        typei(i) = otype(i)
        typeo(i) = 'abspix'
      enddo
      typei(3) = 'abspix'
      typeo(3) = 'abspix'
      win(3) = pix3
c
c Convert location to absolute pixels
c
      call rdhdi(lun, 'naxis', naxis, 0)
      naxis = min(3,naxis)
      call w2wco(lun, naxis, typei, ' ', win, typeo, ' ', wout)
      opos(1) = wout(1)
      opos(2) = wout(2)

      np = ip - 1

      end

c**********************************************************************

c* ppconCG -- Convert full image pixels to binned subimage pixels
c& nebk
c: plotting
c+
      subroutine ppconcg (id, blc, bin, p)

      integer id, blc, bin
      double precision p
c  ---------------------------------------------------------------------
c  Convert pixel values from a full image unbinned pixel to
c  a subimage binned pixel, and vice versa
c
c  Input
c   id        Direction of convsersion
c                1 -> p      -> pb-sub
c                2 -> pb-sub -> p
c   blc       BLC (in full image unbinned pixels) at which subimage
c             begins
c   bin       Pixel increment with which we are stepping through image
c  Input/output
c   p         Pixel with bin=1 and blc=1 OR pixel appropriate to BIN
c             and BLC
c-----------------------------------------------------------------------
      if (id.eq.1) then
c
c Convert to subimage pixels
c
        p = p - blc + 1
c
c Convert to binned subimage pixel
c
        if (bin.ne.1) p = (p-0.5d0)/dble(bin) + 0.5d0
      else if (id.eq.2) then
c
c Convert to unbinned subimage pixel
c
        if (bin.ne.1) p = dble(bin)*(p-0.5d0) + 0.5d0
c
c Convert to full image unbinned pixel
c
        p = p + blc - 1
      endif

      end

c**********************************************************************

c* razerocg - see if RA axis crosses RA=0
c& nebk
c: plotting
c+
      subroutine razerocg (lun, blc, trc, zero)

      integer lun, blc(2), trc(2)
      logical zero(2)
c  ---------------------------------------------------------------------
c     See if an RA axis crosses RA=0. Only looks at
c     first two axes
c
c     Input:
c       lun     Handle of image
c       blc,trc Window being displayed in absolute
c               unbinned full image pixels
c     Output
c       zero   True if that axis is a) RA and b) crosses 0
c-----------------------------------------------------------------------
      integer i1,i2
      double precision x(2),ya(2),yb(2),yc(2)
c-----------------------------------------------------------------------
      zero(1) = .false.
      zero(2) = .false.
      call coInit(lun)
      call coFindAx(lun,'longitude',i1)
      if (i1.eq.1 .or. i1.eq.2) then
        i2 = 3 - i1
        x(i1) = blc(i1)
        x(i2) = blc(i2)
        call coCvt(lun,'ap/ap',x,'aw/aw',ya)
        x(i1) = 0.5*(blc(i1)+trc(i1))
        call coCvt(lun,'ap/ap',x,'aw/aw',yb)
        x(i1) = trc(i1)
        call coCvt(lun,'ap/ap',x,'aw/aw',yc)
        zero(i1) = (yc(i1)-yb(i1))*(yb(i1)-ya(i1)).lt.0
      endif

      call coFin(lun)

      end

c**********************************************************************

c* readbCG -- Read in mask image mask
c& nebk
c: plotting
c+
      subroutine readbcg (init, lun, ibin, jbin, krng, blc, trc,
     *                    bimage, blanks)

      logical bimage(*)
      integer lun, blc(*), trc(*), ibin(2), jbin(2), krng(2)
      logical blanks, init
c  ---------------------------------------------------------------------
c  Read in the blanking mask from the specified window from the image
c  When reading more than one plane, the mask image pixel is considered
c  blanked if any of the planes are blanked at that pixel.  When
c  spatially binning images, a binned pixel is considered blanked if
c  any of the input pixels were blanked.
c
c  Input:
c    init        If true initialize BINMAGE to all good first
c    lun         Handle of image
c    ibin        Increment and average for i direction
c    jbin        Increment and average for j direction
c    krng        First pixel in k direction to read and number of
c                pixels to average
c    blc,trc     Window to read
c  Input/Output
c    blanks      True if there are blanked pixels in bimage
c  Output
c    bimage      Masking image.  True means a good pixel (unflagged) and
c                false means bad (flagged pixel).  Will be bad if any
c                pixel in spectral range is bad for each spatial pixel
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer i, j, k, ii, jj, pi, po, kst, kav, kend, io, jo,
     *  nii, nji, nio, njo, no
      logical good(maxdim)
c-----------------------------------------------------------------------
c
c Find size of unbinned and binned image
c
      nii = trc(1) - blc(1) + 1
      nji = trc(2) - blc(2) + 1
      if (ibin(2).ne.1) then
        nio = nii / ibin(1)
      else
        nio = (nii-1)/ibin(1) + 1
      endif
      if (jbin(2).ne.1) then
        njo = nji / jbin(1)
      else
        njo = (nji-1)/jbin(1) + 1
      endif
c
c Initialize
c
      no = nio * njo
      if (init) then
        do i = 1, no
          bimage(i) = .true.
        enddo
        blanks = .false.
      endif
      do i = 1, maxdim
        good(i) = .true.
      enddo
c
c Read in plane(s)
c
      kst = krng(1)
      kav = krng(2)
      kend = min(trc(3),kst+kav-1)

      do k = kst, kend
        call xysetpl(lun, 1, k)
c
c Step through rows
c
        jo = 1
        do j = 1, nji, jbin(1)
          call xyflgrd(lun, j, good)
c
c Accumulate desired rows
c
          do jj = j, j+jbin(2)-1
            call xyflgrd(lun, jj+blc(2)-1, good)
c
c Step through row
c
            io = 1
            do i = 1, nii, ibin(1)
c
c Accumulate desired pixels
c
              do ii = i, i+ibin(2)-1
c
c Input row and output image pointers
c
                pi = ii + blc(1) - 1
                po = (jo-1)*nio + io
c
c If any pixel in the binned region is bad, set the binned pixel to bad
c
                if (.not.good(pi)) then
                  bimage(po) = .false.
                  blanks = .true.
                endif
              enddo
              io = io + 1
            enddo
          enddo
          jo = jo + 1
        enddo
      enddo

      end

c**********************************************************************

c* readimCG -- Read in image dealing with averaging and blanking
c& nebk
c: plotting
c+
      subroutine readimcg (init, blank, lun, ibin, jbin, krng, blc,
     *                     trc, norm, nimage, image, blanks, dmm)

      real blank, image(*), dmm(3)
      integer nimage(*), lun, ibin(2), jbin(2), krng(2), blc(3),
     *  trc(3)
      logical blanks, init, norm
c  ---------------------------------------------------------------------
c  Read in the specified window from the image and apply spatial
c  and spectral binning as desired
c
c  Input:
c    init        True to initialize output array and normalization
c                image first
c    blank       Value to use for magic blanking
c    lun         Handle of image
c    ibin        Increment and binning for i direction
c    jbin        Increment and binning for j direction
c    krng        First pixel in k direction to read and number of
c                pixels to average
c    blc,trc     Input window, in unbinned pixels, to read and bin
c    norm        If true, normalize the summed image before
c                exiting.  It is up to you to renitialize at
c                the appropriate time with INIT on the next call
c  Output
c    nimage      Normalization image; it is the number of pixels
c                that were averaged together at each output pixel
c                location.  Will be zero for blanked pixels
c    image       Output image (binned, normalized)
c    blanks      True if blanks in output image
c  Input/output
c    dmm         Data min, max, abs max so far
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real row(maxdim)
      logical good(maxdim), mask, hdprsnt
      integer i, j, k, ii, jj, pi, po, kst, kav, kend, io, jo,
     *  nii, nji, nio, njo, no
c-----------------------------------------------------------------------
c
c Does image have a mask
c
      mask = hdprsnt (lun, 'mask')
c
c Find size of unbinned and binned images
c
      nii = trc(1) - blc(1) + 1
      nji = trc(2) - blc(2) + 1
      if (ibin(2).ne.1) then
        nio = nii / ibin(1)
      else
        nio = (nii-1)/ibin(1) + 1
      endif
      if (jbin(2).ne.1) then
        njo = nji / jbin(1)
      else
        njo = (nji-1)/jbin(1) + 1
      endif
c
c Initialize
c
      no = nio * njo
      if (init) then
        do i = 1, no
          image(i) = 0.0
          nimage(i) = 0
        enddo
        blanks = .false.
      endif
      do i = 1, maxdim
        good(i) = .true.
      enddo
c
c Loop over planes
c
      kst = krng(1)
      kav = krng(2)
      kend = min(trc(3),kst+kav-1)

      do k = kst, kend
        call xysetpl(lun, 1, k)
c
c Step through rows
c
        jo = 1
        do j = 1, nji, jbin(1)
c
c Accumulate desired rows
c
          do jj = j, j+jbin(2)-1
            call xyread(lun, jj+blc(2)-1, row)
            if (mask) call xyflgrd(lun, jj+blc(2)-1, good)
c
c Step through row
c
            io = 1
            if (ibin(2).eq.1) then
c
c Faster route if no binning
c
              do i = 1, nii, ibin(1)
c
c Input row and output image pointers
c
                pi = i + blc(1) - 1
                po = (jo-1)*nio + io

                if (good(pi)) then
                  nimage(po) = nimage(po) + 1
                  image(po) = image(po) + row(pi)
                endif
                io = io + 1
              enddo
            else
              do i = 1, nii, ibin(1)
c
c Accumulate desired pixels
c
                do ii = i, i+ibin(2)-1
c
c Input row and output image pointers
c
                  pi = ii + blc(1) - 1
                  po = (jo-1)*nio + io

                  if (good(pi)) then
                    nimage(po) = nimage(po) + 1
                    image(po) = image(po) + row(pi)
                  endif
                enddo
                io = io + 1
              enddo
            endif
          enddo
          jo = jo + 1
        enddo
      enddo
c
c Normalize and blank
c
      do i = 1, no
        if (nimage(i).ne.0) then
          if (norm) image(i) = image(i) / real(nimage(i))
          dmm(1) = min(dmm(1),image(i))
          dmm(2) = max(dmm(2),image(i))
          dmm(3) = max(dmm(3),abs(image(i)))
        else
          blanks = .true.
          image(i) = blank
        endif
      enddo

      end

c**********************************************************************

c* setccsCG -- Set rjs coordinate conversion strings for ticking
c& nebk
c: plotting
c+
      subroutine setccscg (labtyp, ccstr)

      character*(*) labtyp(2), ccstr
c  ---------------------------------------------------------------------
c  For the non-linear tick labelling, we need to convert from world
c  coordinates to absolute pixels.  Depending upon the axis label
c  type, we set an rjs style conversion string indicating what type
c  of input coordinate we are converting.  Note that the third
c  axis is always dealt with in absolute pixels only.
c
c  Input
c    labtyp  Axis label types
c  Output
c    ccstr   String appropriate for rjs style coordinate transformation
c-----------------------------------------------------------------------
      integer i, ip
c-----------------------------------------------------------------------
c
c Loop over first two axes
c
      ip = 1
      do i = 1, 2
        if (labtyp(i).eq.'hms' .or. labtyp(i).eq.'dms' .or.
     *      labtyp(i).eq.'absdeg' .or. labtyp(i).eq.'abskms' .or.
     *      labtyp(i).eq.'absghz' .or. labtyp(i).eq.'absnat') then
          ccstr(ip:ip+2) = 'aw/'
        else if (labtyp(i).eq.'arcsec' .or. labtyp(i).eq.'arcmin' .or.
     *           labtyp(i).eq.'arcmas' .or.
     *           labtyp(i).eq.'reldeg' .or. labtyp(i).eq.'relkms' .or.
     *           labtyp(i).eq.'relghz' .or. labtyp(i).eq.'relnat') then
          ccstr(ip:ip+2) = 'ow/'
        else if (labtyp(i).eq.'abspix' .or. labtyp(i).eq.'none') then
          ccstr(ip:ip+2) = 'ap/'
        else if (labtyp(i).eq.'relpix') then
          ccstr(ip:ip+2) = 'op/'
        endif
        ip = ip + 3
      enddo

      ccstr(6:) = '/ap'

      end

c**********************************************************************

c* setcolCG --  Set multiple line graphics PGPLOT colours
c& nebk
c: plotting
c+
      subroutine setcolcg (i, icol)

      integer i, icol
c  ---------------------------------------------------------------------
c  Return a PGPLOT colour index given a graph number, where you plan
c  to put many graphs with different colours on the one plot.  The
c  colours are chosen so that similar colours are not consecutive
c
c Input:
c   i      Graph number in the range 1 -> NGRAPH, where NGRAPH is the
c          number of graphs that will be drawn on the one plot
c Output:
c   icol   The colour index to set with PGSCI (ICOL)
c-----------------------------------------------------------------------
      integer maxcol
      parameter (maxcol = 13)
      integer lcols(maxcol), ip

      save lcols
      data lcols /2, 7, 5, 3, 1, 6, 8, 12, 4, 10, 11, 9, 13/
c-----------------------------------------------------------------------
      ip = mod(i,maxcol)
      if (ip.eq.0) ip = maxcol
      icol = lcols(ip)

      end

c**********************************************************************

c* setlabCG -- Set axis labels
c& nebk
c: plotting
c+
      subroutine setlabCG (lh, labtyp, doepoch, xlabel, ylabel)

      integer lh
      character*(*) labtyp(2), xlabel, ylabel
      logical doepoch
c  ---------------------------------------------------------------------
c  Set the axis labels
c
c  Input:
c    lh       Handle of image
c    labtyp   Axis label types
c    doepoch  DO we want epoch in string ?
c  Output
c    x,ylabel Labels
c-----------------------------------------------------------------------
      integer   iax, k
      real      epoch
      character algo*3, axtype*9, estr*5, label*100, units*6, wtype*9

      external  len1
      integer   len1
c-----------------------------------------------------------------------
c     Write epoch string for label.
      call rdhdr(lh, 'epoch', epoch, 0.0)
      if (doepoch .and. epoch.gt.0.0) then
        write(estr(2:), 100) nint(epoch)
100     format(i4)
        if (epoch.gt.1984) then
          estr(1:1) = 'J'
        else
          estr(1:1) = 'B'
        endif
      else
        estr = ' '
      endif

c     Loop over axes.
      do iax = 1, 2
        call coAxType(lh, iax, axtype, wtype, algo, units)

        k = len1(wtype)

c       Don't qualify galactic coordinates with an equinox.
        if (wtype.eq.'GLON' .or. wtype.eq.'GLAT') then
          estr = ' '
        endif

        if (labtyp(iax).eq.'abspix') then
          label = wtype(:k)//' (pixels; '//estr//')'
          if (estr.eq.' ') label = wtype(:k)//' (pixels)'
        else if (labtyp(iax).eq.'relpix') then
          label = wtype(:k)//' offset (pixels; '//estr//')'
          if (estr.eq.' ') label = wtype(:k)//' offset (pixels)'
        else if (labtyp(iax).eq.'arcsec') then
          label = wtype(:k)//' offset (arcsec; '//estr//')'
          if (estr.eq.' ') label = wtype(:k)//' offset (arcsec)'
        else if (labtyp(iax).eq.'arcmin') then
          label = wtype(:k)//' offset (arcmin; '//estr//')'
          if (estr.eq.' ') label = wtype(:k)//' offset (arcmin)'
        else if (labtyp(iax).eq.'arcmas') then
          label = wtype(:k)//' offset (mas; '//estr//')'
          if (estr.eq.' ') label = wtype(:k)//' offset (mas)'
        else if (labtyp(iax).eq.'absdeg') then
          label = wtype(:k)//' (degrees; '//estr//')'
          if (estr.eq.' ') label = wtype(:k)//' (degrees)'
        else if (labtyp(iax).eq.'reldeg') then
          label = wtype(:k)//' offset (degrees; '//estr//')'
          if (estr.eq.' ') label = wtype(:k)//' offset (degrees)'
        else if (labtyp(iax).eq.'hms') then
          label = wtype(:k)//' ('//estr//')'
          if (estr.eq.' ') label = wtype(:k)
        else if (labtyp(iax).eq.'dms') then
          label = wtype(:k)//' ('//estr//')'
          if (estr.eq.' ') label = wtype(:k)
        else if (labtyp(iax).eq.'absghz') then
          label = wtype(:k)//' (GHz)'
        else if (labtyp(iax).eq.'relghz') then
          label = wtype(:k)//' offset (GHz)'
        else if (labtyp(iax).eq.'abskms') then
          label = wtype(:k)//' (km s\u-1\d)'
        else if (labtyp(iax).eq.'relkms') then
          label = wtype(:k)//' offset (km s\u-1\d)'
        else if (labtyp(iax).eq.'relnat') then
          if (units.eq.'rad') then
            label = wtype(:k)//' offset (radians)'
          else if (units.eq.'GHz') then
            label = wtype(:k)//' offset (GHz)'
          else if (units.eq.'km/s') then
            label = wtype(:k)//' offset (km s\u-1\d)'
          else if (units.eq.'lambda') then
            label = wtype(:k)//' offset (\gl)'
          else
            label = wtype(:k)//' offset '
          endif
        else if (labtyp(iax).eq.'absnat') then
          if (units.eq.'rad') then
            label = wtype(:k)//' (radians)'
          else if (units.eq.'GHz') then
            label = wtype(:k)//' (GHz)'
          else if (units.eq.'km/s') then
            label = wtype(:k)//' (km s\u-1\d)'
          else if (units.eq.'lambda') then
            label = wtype(:k)//' (\gl)'
          else
            label = wtype(:k)
          endif
        else if (labtyp(iax).eq.'none') then
          label = ' '
        endif

        if (iax.eq.1) then
          xlabel = label
        else if (iax.eq.2) then
          ylabel = label
        endif
      enddo

      end

c**********************************************************************

c* strprpCG -- Strip extra white space from string & delimit with commas
c& nebk
c: plotting
c+
      subroutine strprpcg (maxloc, aline, comloc, nfield, lena)

      character*(*) aline
      integer nfield, maxloc, comloc(maxloc), lena
c  ---------------------------------------------------------------------
c     Take a string with a number of mixed ascii/numeric fields in it
c     and prepare it for use by stripping out extra white space and
c     replacing the space delimiters by commas (matod needs this).
c
c     Input:
c       maxloc  Maximum number of fields allowed in string
c     Input/output:
c       aline   String
c     Output
c       comloc  Locations along string of comma delimiters for
c               each field.  comloc(1) is the comma between the
c               first and second fields etc
c       nfield  Number of fields in string
c       lena    Length of output string after massaging
c-----------------------------------------------------------------------
      integer i, j, lenb, idx
      character bline*132

      integer len1
c-----------------------------------------------------------------------
c
c Strip leading white space
c
      idx = 1
      do while (aline(idx:idx).eq.' ')
        idx = idx + 1
      enddo
      bline = aline(idx:)
      aline = ' '
      aline = bline
c
c Strip additional white space out. Catch cases where commas
c already the separator too
c
      bline = ' '
      lena = len1(aline)
      bline(1:1) = aline(1:1)
      j = 2
      do i = 2, lena
        if ((aline(i:i).eq.' ' .and. aline(i-1:i-1).eq.' ') .or.
     *      (aline(i:i).eq.' ' .and. aline(i-1:i-1).eq.',')) then
          continue
        else
          bline(j:j) = aline(i:i)
          j = j + 1
        endif
      enddo
c
c Replace spaces and colons (which may come from RA or DEC formatted
c strings) by commas (for matodf) and count how many fields there are
c
      lenb = len1(bline)
      nfield = 0
      do i = 1, lenb
        if (bline(i:i).eq.' ' .or. bline(i:i).eq.':' .or.
     *      bline(i:i).eq.',') then
          bline(i:i) = ','
          nfield = nfield  + 1
          if (nfield.gt.maxloc) call bug('f',
     *      'STRPRPCG: Too many fields for internal storage')
          comloc(nfield) = i
        endif
      enddo
      nfield = nfield + 1
      if (bline(lenb:lenb).eq.',') then
        nfield = nfield - 1
        lenb = lenb - 1
      endif
      aline = bline
      lena = lenb

      end

c**********************************************************************

c* subincCG -- Step to next sub-plot
c& nebk
c: plotting
c+
      subroutine subinccg (iplot, nx, ny, vxmin, vymax, vxsize, vysize,
     *                     vxgap, vygap, vx, vy)

      real vxmin, vymax, vxsize, vysize, vxgap, vygap, vx, vy
      integer iplot, nx, ny
c  ---------------------------------------------------------------------
c  Increment view port locations ready for next sub-plot
c
c  Input
c    iplot    Current sub-plot number
c    nx,ny    Number of sub-plots in x and y directions on view-surface
c    vxmin    minimum x location of encompassing viewport (ndc)
c    vymax    maximum y location of encompassing viewport (ndc)
c    vx,ysize Size of sub-plots on view-surface (ndc)
c    vx,ygap  Gap between sub-plots on the view-surface (ndc)
c  Input/output
c    vx,vy    Location of blc of next sub-plot on view-surface
c-----------------------------------------------------------------------
      if (mod(iplot,nx*ny).eq.0) then
        vx = vxmin
        vy = vymax - vysize
      else if (mod(iplot,nx).eq.0) then
        vx = vxmin
        vy = vy - vygap - vysize
      else
        vx = vx + vxgap + vxsize
      endif

      end

c**********************************************************************

c* wedginCG -- See if grey scale wedges are inside or outside subplots
c& nebk
c: plotting
c+
      subroutine wedgincg (hard, dofid, dowedge, nx, ny, npixr,
     *                     trfun, wedcod)

      logical dowedge, dofid
      integer nx, ny, npixr, wedcod
      character trfun*3, hard*3
c  ---------------------------------------------------------------------
c  Work out whether the grey scale wedges are to be drawn inside
c  or outside the subplots, and whether there will be one or many
c
c  Input
c    hard     'YES' if writing to hardcopy PGPLOT device
c    dofid    True if user has requested OFM fiddle option
c    dowedge  True if user requests wedge
c    nx,ny    Number of subplots in x and y directions
c    npixr    NUmber of grey scale "range" groups given by user
c    trfun    Transfer function type of first "range" group
c  Output
c    wedcod   0 -> No wedges
c             1 -> one wedge to right of all subplots
c             2 -> one wedge to right per subplot
c             3 -> one wedge per subplot inside subplot
c-----------------------------------------------------------------------
      if (.not.dowedge) then
        wedcod = 0
      else
        if (hard.eq.'YES') then
          if (nx*ny.eq.1) then
            wedcod = 1
          else
            if (dofid) then
              wedcod = 3
            else
              if (npixr.eq.1 .and. trfun.ne.'heq') then
                wedcod = 1
              else if (ny.gt.1 .and. nx.eq.1 .and. ((npixr.eq.1 .and.
     *                 trfun.eq.'heq') .or. npixr.gt.1)) then
                wedcod = 2
              else
                wedcod = 3
              endif
            endif
          endif
        else
          if (nx*ny.eq.1 .or. (npixr.eq.1 .and. trfun.ne.'heq')) then
              wedcod = 1
          else if (ny.gt.1 .and. nx.eq.1 .and. ((npixr.eq.1 .and.
     *             trfun.eq.'heq') .or. npixr.gt.1)) then
            wedcod = 2
          else
            wedcod = 3
          endif
        endif
      endif

      end

c**********************************************************************

c* winfidcg - adjust window size to fit integral number of bins
c& nebk
c: plotting
c+
      subroutine winfidcg (size, axis, bin, blc, trc, win)

      integer axis, bin(2), blc, trc, size, win
c  ---------------------------------------------------------------------
c     Adjust the size of the window so that the bin width fits
c     an integer number of times
c
c     Input:
c        size    SIze of total available image
c        axis    Axis number
c        bin     Pixel increment and binning width across image
c     Input/output
c        blc,trc Window in pixels, adjusted if necessary to fit
c                an integral number of bins
c     Output
c        win     Size of binned window
c-----------------------------------------------------------------------
      integer lo, hi, rem, size2, bin2
      logical new, fail
      character aline*80
c-----------------------------------------------------------------------
c     Don't fiddle width if no binning, READIMCG and READBCG will cope.
      bin2  = bin(2)
      size2 = trc - blc + 1
      if (bin2.eq.1) then
        win = (size2 - 1) / bin(1) + 1
        return
      endif

c     If the binning width is not unity, the increment must already have
c     been set to the same number.
      rem = mod(size2,bin2)

c     Return early if no adjustement needed.
      if (rem.eq.0) then
        win = size2 / bin2
        return
      endif

c     Adjust window to fit integral number of bins.  Increment TRC by 1
c     and decrement BLC by 1 until ok.
      lo = blc
      hi = trc
      fail = .false.
      new  = .false.
      do while (rem.ne.0 .and. .not.fail)
        if (blc.eq.1 .and. trc.eq.size) fail = .true.

        if (.not.fail) then
          trc = min(trc+1,size)
          size2 = trc - blc + 1
          rem = mod(size2,bin2)

          if (rem.ne.0) then
            blc = max(blc-1,1)
            size2 = trc - blc + 1
            rem = mod(size2,bin2)
          endif

          new = .true.
        endif
      enddo

      if (fail) then
c       Failed in making the window bigger, try making it smaller.
        size2 = hi - lo + 1
        rem = mod(size2,bin2)
        new = .false.
        fail = .false.
        do while (rem.ne.0 .and. .not.fail)
          if (blc+bin2.gt.trc) fail = .true.

          if (.not.fail) then
            trc = trc - 1
            size2 = trc - blc + 1
            rem = mod(size2,bin2)

            if (rem.ne.0) then
              blc = blc + 1
              size2 = trc - blc + 1
              rem = mod(size2,bin2)
            endif

            new = .true.
          endif
        enddo
      endif

c     Report what happened.
      if (fail) then
        write(aline, 50) axis
50      format('Can''t adjust window to contain',
     *          ' integral no. of bins on axis ', i1)
        call bug('f', aline)
      else if (new) then
        write(aline, 100) axis, lo, hi, blc, trc, bin(2)
100     format('Adjusted axis ', i1, ' window from ', i4, ',', i4,
     *          ' to ', i4, ',', i4, ' to fit bin width ',i4)
        call output(aline)
      endif

c     Size of binned window.
      win = size2 / bin2

      end
