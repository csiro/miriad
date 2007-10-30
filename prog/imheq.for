      program imheq
      implicit none
c
c= imheq - apply histogram equalization to an image
c& nebk
c: map manipulation
c+
c	IMHEQ -- applies histogram equalization to an image.  This
c	technique generates a cumulative histogram of an image.  The
c	ordinate for this histogram (number of pixels) is then also
c	discretized into the prescribed number of bins.  Each image
c	pixel is then replaced by the value of the cumulative 
c	histogram bin that it contributed to.  This essentially means
c	that in terms of a non-cumulative histogram of the image,
c	equal numbers of pixels have fallen into each intensity bin
c	so that the bins are not of equal intensity width.  This
c	technique enables you to see best the intensity range that
c	has the most pixels.
c
c	Image pixels which are flagged by the image mask will not 
c	contribute to the histograms.  However, they will be equalized 
c	in the output image (although their mask will be unchanged).
c	
c@ in
c	The input image. No default.
c@ out
c	The output image. No default.
c@ nbins
c	The number of bins for the image histogram.   Default is 128.
c@ range
c	The intensity minimum and maximum to bin in the histogram.
c	Pixels outside this range are set to the nearest limit.
c	Default is to use the full image plane range.  Over-rides
c	OPTIONS=GLOBAL below.
c@ options
c	"global" means use the global image minimum and maximum as the
c	   histogram limits for all image planes.  By default, each
c	   image plane is equalized with the intensity minimum and 
c	   maximum from that plane.
c@ device
c	PGPLOT device to show plots of the histograms & discretized
c	cumulative histogram. Will plot after each plane, so really 
c	of use only for single plane images
c 
c--
c
c  History:
c    nebk 27jan94  Original version
c    rjs  02jul97  cellscal change.
c    rjs  23jul97  added pbtype.
c------------------------------------------------------------------------
      character version*(*)
      parameter(version='ImHeq: version 27-Jan-94' )
c
      include 'maxnax.h'
      include 'maxdim.h'
      include 'mem.h'
c
      integer maxbin
      parameter (maxbin = 1000)
c
      character in*80, out*80, device*80
      integer nin(maxnax), naxis, lin, lout, nbins, ipr, ipl, 
     +  ierr, pgbeg, k, his(maxbin)
      real cumhis(maxbin), xp(maxbin), yp(maxbin,2)
      real bmin, bmax, bming, bmaxg, bmin2, bmax2, ymax, bminu, bmaxu
      logical global
c
c  Header keywords.
c
      integer nkeys
      parameter(nkeys=49)
      character keyw(nkeys)*8
      data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *    'crota1  ','crota2  ','crota3  ','crota4  ','crota5  ',
     *        'crval1  ','crval2  ','crpix3  ','crpix4  ','crpix5  ',
     *        'crpix1  ','crpix2  ','crval3  ','crval4  ','crval5  ',
     *        'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','cdelt5  ',
     *        'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *        'obstime ','epoch   ','history ','instrume','niters  ',
     *        'object  ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *        'restfreq','telescop','vobs    ','cellscal','pbtype  ',
     *        'ltype   ','lstart  ','lwidth  ','lstep   ','btype   '/
      data bmin2, bmax2 /1.0e32, -1.0e32/
c------------------------------------------------------------------------
c
c  Get the input parameters.
c
      call output (version)
      call keyini
      call keya ('in', in, ' ')
      call keya ('out', out, ' ')
      if (in.eq.' ' .or. out.eq.' ')
     +  call bug ('f', 'You must give an input and output file')
      call keyi ('nbins', nbins, 128)
      nbins = min(maxbin,nbins)
      call keyr ('range', bminu, 0.0)
      call keyr ('range', bmaxu, 0.0)
      call keya ('device', device, ' ')
      call decopt (global)
      if (bminu.ne.0.0 .or. bmaxu.ne.0.0) global = .false.
      call keyfin
c
c  Open the input.
c
      call xyopen (lin, in, 'old', maxnax, nin)
      if (nin(1).gt.maxdim)
     +  call bug ('f', 'Image too big for me to handle')
      call rdhdi (lin, 'naxis', naxis, 0)
      call imminmax (lin, naxis, nin, bming, bmaxg)
c
c  Make the output file, and make its header.
c
      call xyopen (lout, out, 'new', naxis, nin)
      do k = 1, nkeys
        call hdcopy (lin, lout, keyw(k))
      end do
      call hisopen (lout, 'append')
      call hiswrite (lout, 'IMHEQ: Miriad '//version)
      call hisinput (lout, 'IMHEQ')
      call hisclose (lout)
c
c Allocate memory
c
      call memalloc (ipr, nin(1)*nin(2), 'r')
      call memalloc (ipl, nin(1)*nin(2), 'l')
c
c Open PGPLOT device
c
      if (device.ne.' ') then
        ierr = pgbeg (0, device, 1, 1)
        if (ierr.ne.1) then
          call pgldev
          call bug ('f', 'Error opening plot device')
        end if
        call pgsvp (0.2, 0.8, 0.2, 0.8)
        call pgpage
      end if
c
c Loop over planes
c 
      do k = 1, nin(3)
        call xysetpl (lin,  1, k)
        call xysetpl (lout, 1, k)
c
c Read image
c
        call readim (lin, nin(1), nin(2), memr(ipr), meml(ipl),
     +               bmin, bmax)
        if (global) then
          bmin = bming
          bmax = bmaxg
        else if (bminu.ne.0.0 .or. bmaxu.ne.0.0) then
          bmin = bminu
          bmax = bmaxu
        end if
c
c Apply histogram equalization
c
        call equal (nin(1)*nin(2), memr(ipr), meml(ipl), bmin, bmax,
     +              nbins, his, cumhis, bmin2, bmax2, maxbin, xp, 
     +              yp, ymax)
c
c Write out image
c
        call writim (lout, nin(1), nin(2), memr(ipr), meml(ipl))
c
c Draw plot
c
        if (device.ne.' ') then
          call pgswin (bmin, bmax, 0.0, ymax)
          call pgbox ('BCNST', 0.0, 0, 'BNST', 0.0, 0)
          call pghline (nbins, xp, yp(1,1), 2.0)
          call pglab ('Intensity', 'Number', 
     +                'Histogram and Cumulative Histogram')
c
          call pgsci (7)
          call pgswin (bmin, bmax, 0.0,  real(nin(1)*nin(2)))
          call pgbox (' ', 0.0, 0, 'CMST', 0.0, 0)
          call pghline (nbins, xp, yp(1,2), 2.0)
          call pgmtxt ('R', 2.0, 0.5, 0.5, 'Number')
          call pgupdt
        end if
      end do
c
c Close up
c
      call wrhdr (lout, 'datamin', bmin2)
      call wrhdr (lout, 'datamax', bmax2)
c
      call memfree (ipr, nin(1)*nin(2), 'i')
      call memfree (ipl, nin(1)*nin(2), 'i')
      call xyclose (lin)
      call xyclose (lout)
      if (device.ne.' ') call pgend
c
      end
c
c
      subroutine readim (lin, nx, ny, image, mask, bmin, bmax)
c-----------------------------------------------------------------------
c     Read image
c-----------------------------------------------------------------------
      implicit none
      integer lin, nx, ny
      real image(nx*ny), bmin, bmax
      logical mask(nx*ny)
c
      integer i, j, k
c-----------------------------------------------------------------------
      k = 1
      bmin =  1.0e32
      bmax = -1.0e32
      do j = 1, ny
        call xyread (lin, j, image(k))
        call xyflgrd (lin, j, mask(k))
        do i = 1, nx
          bmin = min(bmin,image(k+i-1))
          bmax = max(bmax,image(k+i-1))
        end do
c
        k = k + nx
      end do
c
      end
c
c
      subroutine writim (lin, nx, ny, image, mask)
c-----------------------------------------------------------------------
c     Write image
c-----------------------------------------------------------------------
      implicit none
      integer lin, nx, ny
      real image(nx*ny)
      logical mask(nx*ny)
c
      integer j, k
c-----------------------------------------------------------------------
      k = 1
      do j = 1, ny
        call xywrite (lin, j, image(k))
        call xyflgwr (lin, j, mask(k))
        k = k + nx
      end do
c
      end
c
c
      subroutine equal (n, image, mask, bmin, bmax, nbins, his,
     +   cumhis, bmin2, bmax2, maxbin, xp, yp, ymax)
c-----------------------------------------------------------------------
c     Apply histogram equalization
c
c  Input
c    n       Number of pixels in image
c    image   Image
c    mask    Image mask (.true. is good)
c    bmin    Image minimum
c    bmax    Image maximum
c    nbins   Number of bins for histogram
c    maxbin  Max number of bins
c  Scratch
c    his     Histogram
c    cumhis  Cumulative histogram
c  Output
c    bmin2   Output image minimum
c    bmax2   Output image maximum
c    xp      Intensity for plotting
c    yp      Histogram and discretized cumulative histogram (same as
c            transfer function apart from normalization) for plotting
c   
c-----------------------------------------------------------------------
      implicit none
      integer maxbin, n, nbins, his(nbins)
      real bmin, bmax, bmin2, bmax2, image(n), xp(nbins), yp(maxbin,2),
     +  cumhis(nbins), ymax
      logical mask(n)
cc
      real fac, cum
      integer i, idx
c-----------------------------------------------------------------------
c
c Initialize histogram
c
      do i = 1, nbins
        his(i) = 0
        cumhis(i) = 0.0
c
c Plotting array
c
        xp(i) = (i-1)/real(nbins-1)*(bmax-bmin) + bmin
      end do
c
c Generate image histogram
c
      fac = real(nbins-1) / (bmax-bmin)
      do i = 1, n
        if (mask(i)) then
          idx = max(1,min(nbins,nint((image(i)-bmin)*fac)+1))
          his(idx) = his(idx) + 1
        end if
      end do
c
c Generate cumulative histogram.  
c
      cum = 0.0
      ymax = -1.0e32
      do i = 1, nbins
        cum = cum + his(i) 
        cumhis(i) = cum
        yp(i,1) = his(i)
c
        ymax = max(ymax,yp(i,1))
      end do
c
c Now discretize the cumulative histogram values as well
c
      fac = real(nbins-1) / real(n)
      bmin2 =  1.0e32
      bmax2 = -1.0e32
      do i = 1, nbins
c
c This index converts the actual cumulative histogram
c value to the nearest discrete bin
c
        idx = max(1,min(nbins,nint(cumhis(i)*fac)+1))
c
c Convert this bin back to an intensity and reuse CUMHIS array
c
        yp(i,2) = cumhis(i)
        cumhis(i) = real(idx)/real(nbins)*(bmax-bmin) + bmin
        bmin2 = min(bmin2,cumhis(i))
        bmax2 = max(bmax2,cumhis(i))
      end do
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
      end do
c
      end
c
      subroutine decopt  (global)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     global    Use global image min and max 
c-----------------------------------------------------------------------
      implicit none
c
      logical global
cc
      integer maxopt
      parameter (maxopt = 1)
c
      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'global'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      global = present(1)
c
      end
