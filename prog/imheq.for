      program imheq

c= imheq - apply histogram equalization to an image
c& nebk
c: map manipulation
c+
c       IMHEQ -- applies histogram equalization to an image.  This
c       technique generates a cumulative histogram of an image.  The
c       ordinate for this histogram (number of pixels) is then also
c       discretized into the prescribed number of bins.  Each image
c       pixel is then replaced by the value of the cumulative
c       histogram bin that it contributed to.  This essentially means
c       that in terms of a non-cumulative histogram of the image,
c       equal numbers of pixels have fallen into each intensity bin
c       so that the bins are not of equal intensity width.  This
c       technique enables you to see best the intensity range that
c       has the most pixels.
c
c       Image pixels which are flagged by the image mask will not
c       contribute to the histograms.  However, they will be equalized
c       in the output image (although their mask will be unchanged).
c
c@ in
c       The input image. No default.
c@ out
c       The output image. No default.
c@ nbins
c       The number of bins for the image histogram.   Default is 128.
c@ range
c       The intensity minimum and maximum to bin in the histogram.
c       Pixels outside this range are set to the nearest limit.
c       Default is to use the full image plane range.  Over-rides
c       OPTIONS=GLOBAL below.
c@ options
c       "global" means use the global image minimum and maximum as the
c          histogram limits for all image planes.  By default, each
c          image plane is equalized with the intensity minimum and
c          maximum from that plane.
c@ device
c       PGPLOT device to show plots of the histograms & discretized
c       cumulative histogram. Will plot after each plane, so really
c       of use only for single plane images
c
c$Id: imheq.for,v 1.3 2010/08/25 06:10:09 cal103 Exp $
c--
c
c  History:
c    nebk 27jan94  Original version
c    rjs  02jul97  cellscal change.
c    rjs  23jul97  added pbtype.
c-----------------------------------------------------------------------
      include 'maxnax.h'
      include 'maxdim.h'
      include 'mem.h'

      integer MAXBIN
      parameter (MAXBIN = 1000)

      logical   global
      integer   his(MAXBIN), ierr, ipl, ipr, k, lin, lout, naxis, nbins,
     *          nin(maxnax), pgbeg
      real      bmax, bmax2, bmaxg, bmaxu, bmin, bmin2, bming, bminu,
     *          cumhis(MAXBIN), xp(MAXBIN), ymax, yp(MAXBIN,2)
      character device*80, in*80, out*80, version*72

      character versan*80
      external  versan

      data bmin2, bmax2 /1e32, -1e32/
c-----------------------------------------------------------------------
      version = versan('imheq',
     *                 '$Revision: 1.3 $',
     *                 '$Date: 2010/08/25 06:10:09 $')

c     Get the input parameters.
      call output (version)
      call keyini
      call keya ('in', in, ' ')
      call keya ('out', out, ' ')
      if (in.eq.' ' .or. out.eq.' ')
     *  call bug ('f', 'You must give an input and output file')
      call keyi ('nbins', nbins, 128)
      nbins = min(MAXBIN,nbins)
      call keyr ('range', bminu, 0.0)
      call keyr ('range', bmaxu, 0.0)
      call keya ('device', device, ' ')
      call decopt (global)
      if (bminu.ne.0.0 .or. bmaxu.ne.0.0) global = .false.
      call keyfin

c     Open the input.
      call xyopen (lin, in, 'old', maxnax, nin)
      if (nin(1).gt.maxdim)
     *  call bug ('f', 'Image too big for me to handle')
      call rdhdi (lin, 'naxis', naxis, 0)
      call imminmax (lin, naxis, nin, bming, bmaxg)

c     Make the output file, and make its header.
      call xyopen (lout, out, 'new', naxis, nin)
      call headcopy(lIn, lOut, 0, 0, 0, 0)
      call hisopen (lout, 'append')
      call hiswrite (lout, 'IMHEQ: Miriad '//version)
      call hisinput (lout, 'IMHEQ')
      call hisclose (lout)

c     Allocate memory.
      call memalloc (ipr, nin(1)*nin(2), 'r')
      call memalloc (ipl, nin(1)*nin(2), 'l')

c     Open PGPLOT device.
      if (device.ne.' ') then
        ierr = pgbeg (0, device, 1, 1)
        if (ierr.ne.1) then
          call pgldev
          call bug ('f', 'Error opening plot device')
        endif
        call pgsvp (0.2, 0.8, 0.2, 0.8)
        call pgpage
      endif

c     Loop over planes.
      do k = 1, nin(3)
        call xysetpl (lin,  1, k)
        call xysetpl (lout, 1, k)

c       Read image.
        call readim (lin, nin(1), nin(2), memr(ipr), meml(ipl),
     *               bmin, bmax)
        if (global) then
          bmin = bming
          bmax = bmaxg
        else if (bminu.ne.0.0 .or. bmaxu.ne.0.0) then
          bmin = bminu
          bmax = bmaxu
        endif

c       Apply histogram equalization.
        call equal (nin(1)*nin(2), memr(ipr), meml(ipl), bmin, bmax,
     *              nbins, his, cumhis, bmin2, bmax2, MAXBIN, xp,
     *              yp, ymax)

c       Write out image.
        call writim (lout, nin(1), nin(2), memr(ipr), meml(ipl))

c       Draw plot.
        if (device.ne.' ') then
          call pgswin (bmin, bmax, 0.0, ymax)
          call pgbox ('BCNST', 0.0, 0, 'BNST', 0.0, 0)
          call pghline (nbins, xp, yp(1,1), 2.0)
          call pglab ('Intensity', 'Number',
     *                'Histogram and Cumulative Histogram')

          call pgsci (7)
          call pgswin (bmin, bmax, 0.0,  real(nin(1)*nin(2)))
          call pgbox (' ', 0.0, 0, 'CMST', 0.0, 0)
          call pghline (nbins, xp, yp(1,2), 2.0)
          call pgmtxt ('R', 2.0, 0.5, 0.5, 'Number')
          call pgupdt
        endif
      enddo

c     Close up.
      call wrhdr (lout, 'datamin', bmin2)
      call wrhdr (lout, 'datamax', bmax2)

      call memfree (ipr, nin(1)*nin(2), 'i')
      call memfree (ipl, nin(1)*nin(2), 'i')
      call xyclose (lin)
      call xyclose (lout)
      if (device.ne.' ') call pgend

      end


      subroutine readim (lin, nx, ny, image, mask, bmin, bmax)

      integer lin, nx, ny
      real    image(nx*ny), bmin, bmax
      logical mask(nx*ny)
c-----------------------------------------------------------------------
c     Read image.
c-----------------------------------------------------------------------
      integer i, j, k
c-----------------------------------------------------------------------
      k = 1
      bmin =  1e32
      bmax = -1e32
      do j = 1, ny
        call xyread (lin, j, image(k))
        call xyflgrd (lin, j, mask(k))
        do i = 1, nx
          bmin = min(bmin,image(k+i-1))
          bmax = max(bmax,image(k+i-1))
        enddo

        k = k + nx
      enddo

      end


      subroutine writim (lin, nx, ny, image, mask)

      integer lin, nx, ny
      real    image(nx*ny)
      logical mask(nx*ny)
c-----------------------------------------------------------------------
c     Write image.
c-----------------------------------------------------------------------
      integer j, k
c-----------------------------------------------------------------------
      k = 1
      do j = 1, ny
        call xywrite (lin, j, image(k))
        call xyflgwr (lin, j, mask(k))
        k = k + nx
      enddo

      end


      subroutine equal (n, image, mask, bmin, bmax, nbins, his,
     *   cumhis, bmin2, bmax2, maxbin, xp, yp, ymax)

      integer n, maxbin, nbins, his(nbins)
      real    bmin, bmax, bmin2, bmax2, image(n), cumhis(nbins),
     *        xp(nbins), yp(maxbin,2), ymax
      logical mask(n)
c-----------------------------------------------------------------------
c     Apply histogram equalization
c
c  Input
c    n       Number of pixels in image.
c    image   Image.
c    mask    Image mask (.true. is good).
c    bmin    Image minimum.
c    bmax    Image maximum.
c    nbins   Number of bins for histogram.
c    maxbin  Max number of bins.
c  Scratch
c    his     Histogram.
c    cumhis  Cumulative histogram.
c  Output
c    bmin2   Output image minimum.
c    bmax2   Output image maximum.
c    xp      Intensity for plotting.
c    yp      Histogram and discretized cumulative histogram (same as
c            transfer function apart from normalization) for plotting.
c
c-----------------------------------------------------------------------
      integer   i, idx
      real      cum, fac
c-----------------------------------------------------------------------
c     Initialize histogram.
      do i = 1, nbins
        his(i) = 0
        cumhis(i) = 0.0

c       Plotting array.
        xp(i) = (i-1)/real(nbins-1)*(bmax-bmin) + bmin
      enddo

c     Generate image histogram.
      fac = real(nbins-1) / (bmax-bmin)
      do i = 1, n
        if (mask(i)) then
          idx = max(1,min(nbins,nint((image(i)-bmin)*fac)+1))
          his(idx) = his(idx) + 1
        endif
      enddo

c     Generate cumulative histogram.
      cum  = 0.0
      ymax = -1e32
      do i = 1, nbins
        cum = cum + his(i)
        cumhis(i) = cum
        yp(i,1) = his(i)

        ymax = max(ymax,yp(i,1))
      enddo

c     Now discretize the cumulative histogram values as well.
      fac = real(nbins-1) / real(n)
      bmin2 =  1e32
      bmax2 = -1e32
      do i = 1, nbins

c       This index converts the actual cumulative histogram
c       value to the nearest discrete bin.
        idx = max(1,min(nbins,nint(cumhis(i)*fac)+1))

c       Convert this bin back to an intensity and reuse CUMHIS array.
        yp(i,2) = cumhis(i)
        cumhis(i) = real(idx)/real(nbins)*(bmax-bmin) + bmin
        bmin2 = min(bmin2,cumhis(i))
        bmax2 = max(bmax2,cumhis(i))
      enddo

c     Now fix the image pixels (including masked ones).
      fac = real(nbins-1) / (bmax-bmin)
      do i = 1, n

c       Find cumulative histogram index of this pixel.
        idx = max(1,min(nbins,nint((image(i)-bmin)*fac)+1))

c       Replace by discretized cumulative histogram intensity.
        image(i) = cumhis(idx)
      enddo

      end


      subroutine decopt  (global)

      logical global
c-----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     global    Use global image min and max.
c-----------------------------------------------------------------------
      integer MAXOPT
      parameter (MAXOPT = 1)

      logical   present(MAXOPT)
      character opshuns(MAXOPT)*8

      data opshuns /'global'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, MAXOPT)

      global = present(1)

      end
