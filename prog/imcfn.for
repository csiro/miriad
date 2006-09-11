      program imcfn
c-----------------------------------------------------------------------
c= IMCFN - Compute confusion noise for an interferometer image
c& nebk
c: image analysis
c+
c	IMCFN computes confusion noise in an interferometer image
c	owing to the presence of unresolved background sources.
c
c	At each location (x0,y0) in the output image, the variance
c	of the confusion noise is given by the product of 2 integrals
c
c	var(x0,y0)  =  I1 * I2(x0,y0)
c
c	where I1 is the source integral defined by
c
c	         - Smax
c	   I1 = |        N(S) S**2 dS
c	       - Smin
c
c
c	and I2 is the beam integral defined by
c
c	                -- 
c	   I2(x0,y0) = ||   [SB(x-x0,y-y0) * PB(x,y)]**2  dx dy
c	              --
c
c	Thus, at the desired output location (x0,y0), the synthesized
c	beam is shifted to the location (x0,y0), and then the product of 
c	the shifted beam and the primary beam is formed, and then that 
c	product squared is summed over the size of the synthesized 
c	beam image.
c
c	The user is told what the source integral value is.  Thus,
c	you can in fact rescale the output image to other flux density
c	ranges just by knowing the square root of the flux integral.  
c
c	The program can run without an input beam or output image 
c	whereupon just the source integral is done.   This can be useful 
c	because the computation of the beam integral is very slow, as 
c	for each output pixel, a sum over an image the size of the 
c	synthesized beam must be done.
c
c	The user inputs the differential and normalized source count 
c	function N(S)/N0(S) where S is the flux density.   The units 
c	of N(S) are counts/Jy/steradian.    The user also specifies 
c	N0, which is a cosmological normalization, usually a power
c	law of S.   This function is input via a power law and/or
c	polynomial.  A break flux density is given below which the 
c	power law is used and above which the polynomial is used.
c
c	In the help file, examples are given for keyword values for 
c	the 20cm number counts.  These results come from Windhorst 
c	et al 1993, ApJ, 405, 409
c
c@ beam 
c	Synthesised dirty beam image (must be power of 2). No default.
c@ out  
c	Confusion noise (sigma) image (1/2 size of beam image) in Jy 
c@ flux
c	3 numbers (Jy).  flux(1) and flux(2) define the range of flux 
c	densities for which the confusion noise is calculated.  
c	flux(3) is the break point below which the power law is
c	used, and above which the polynomial is used.  Set flux(3)
c	above flux(2) if you wish to use only the power law. Set
c	flux(3) below flux(1) if you wish to use only the polynomial.
c	No defaults.
c
c	For the 20cm counts, the break point is flux(3) = 1E-4 Jy
c	The polynomial coefficients are good from flux(3) to 
c	flux(2) = 10 Jy.  The power law is good from 
c	flux(1) = 1E-5 Jy to flux(3) = 1E-4 Jy.   Below 1E-5 Jy
c	the function is unknown, but must turn over and converge
c	so as not to distort the CMB spectrum.
c@ poly
c	N(S)/N0(S) specified as a polynomial of up to 5th order fit to 
c	log10(N/N0 counts/Jy/sr) vs log10(S Jy)  You input up to 6
c	polynomial coefficients (low to high order).
c
c	For the 20cm number counts, the polynomial coefficients are
c	2.519192, -0.139680, -0.302235, 0.067539, 0.046945, 0.005320
c	See the flux keyword for the acceptable range.
c@ power
c	N(S)/N0(S) (counts/Jy/sr) specified as a power law  
c	a * S**p where S is in Jy and you give a and p.
c
c	For the 20cm number counts, the power law is a = 195
c	and p = 0.45   See the flux keyword for the acceptable range.
c@ n0
c	The normalization factor N0 = f * S**b  where S is in Jy
c	You give f and b.
c
c	For the 20cm number counts, f = 1.0 and b = -2.5
c@ device
c	Plot device on which to plot the differential normalized
c	and un-normalized source count functions.  You can specify 
c	a plot device but not an output or beam image if you just 
c	want to see the plots but don't want to spend ages computing
c	the beam integral.
c	
c--
c  History:
c    nebk 13sep95 Original version
c
c------------------------------------------------------------------------
      implicit none
c
      include 'mirconst.h'
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
      character version*32 
      integer maxpc, maxpts
      parameter (version = 'ImCFN: version 13-Sep-95', maxpc = 6,
     +           maxpts = 10000)
cc
      real xx(maxpts), yy(maxpts), yy2(maxpts), ymin, ymax, xmin, xmax,
     +  ymin2, ymax2
      integer ierr, pgbeg
c
      double precision crpixi(maxnax), crpixo(maxnax), cdelti(maxnax), 
     +  smin, smax, sb, a, b, c(maxpc), sum, s1, s2, s3, ds, n0(2), 
     +  sold, s, nn, n, dlogs, ss, sJy
      integer naxisi, sizei(maxnax), sizeo(maxnax), li, lo, ipi, ipo, 
     +  ip, i, j, nc
      character in*132, out*132, str*1, itoaf*1, device*80, line*80,
     +  xlabel*80, ylabel*80, title*80
      data c /maxpc*0.0d0/
      data xmin,  xmax  /1.0e30, -1.0e30/
      data ymin,  ymax  /1.0e30, -1.0e30/
      data ymin2, ymax2 /1.0e30, -1.0e30/
c-------------------------------------------------------------------------
      call output (version)
c
c Get the inputs
c
      call keyini
      call keyf ('beam', in, ' ')
      call keyf ('out', out, ' ')
      call keyd ('flux', smin, 0.0d0)
      call keyd ('flux', smax, 0.0d0)
      call keyd ('flux', sb, 0.0d0)
      call mkeyd ('poly', c, maxpc, nc)
      call keyd ('power', a, 0.0d0)
      call keyd ('power', b, 0.0d0)
      call keyd ('n0', n0(1), 0.0d0)
      call keyd ('n0', n0(2), 0.0d0)
      call keya ('device', device, ' ')
      call keyfin
c
c Check inputs
c
      if (out.ne.' '.and. in.eq.' ') call bug ('f', 
     +  'You must give an input beam')
      if (smin.le.0.0 .or. smax.le.0.0 .or. sb.le.0.0) call bug ('f',
     +  'Flux densities must be positive')
      if (n0(1).le.0.0 .and. n0(2).eq.0.0) call bug ('f',
     +  'You must give the normalization expression')
      if (nc.eq.0 .and. a.eq.0.0 .and. b.eq.0.0) call bug ('f', 
     +  'You have not given a differential source function')
      if (nc.eq.0 .and. a.le.0.0) call bug ('f', 'a must be > 0')
c
c Do the source flux integral
c
      sum = 0.0
      s1 = log10(smin)
      s2 = log10(smax)
      s3 = log10(sb)
      dlogs = (s2-s1)/real(maxpts-1)      
      sold = s1
      i = 1
c
      do s = s1+dlogs, s2, dlogs
        sjy = 10**s
c
        if (s.le.s3) then
c
c Evaluate N/N0 from power law
c
          nn = a * sJy**b
        else
c
c Evaluate N/N0 with polynomial and Horners rule 
c
          nn = c(nc)
          do j = nc-1, 1, -1
            nn = nn * s + c(j)
          end do
          nn = 10**nn
        end if
c
c Scale by N0 to make N(S) in counts/Jy/steradian
c
        n = nn * n0(1) * sJy**n0(2)
c
c Update sum
c
        ds = sjy - sold
        sum = sum + sjy**2*n*ds
c
c Fill plot arrays 
c
        xx(i) = s
        yy(i) =  log10(n)
        yy2(i) = log10(nn)
        xmin  = min (xmin,xx(i))
        xmax  = max (xmax,xx(i))
        ymin  = min (ymin,yy(i))
        ymax  = max (ymax,yy(i))
        ymin2 = min (ymin2,yy2(i))
        ymax2 = max (ymax2,yy2(i))
c
        i = i + 1
        sold = sjy
      end do
c
      write (line,10) sum
10    format ('Source integral = ', 1pe14.6, ' Jy**2 per steradian')
      call output (line)
c
c Draw plot
c
      if (device.ne.' ') then
        xlabel = 'S (Jy)'
        title = 'Differential Source counts'
        ierr = pgbeg (0, device, 1, 2)
        if (ierr.ne.1) then
          call pgldev
          call bug ('f', 'Error opening plot device')
        end if
        call pgsvp (0.2, 0.8, 0.1, 0.9)
c
        call pgpage
        call pgswin (xmin, xmax, ymin2, ymax2)
        call pgbox ('BCNSTL', 0.0, 0, 'BCNSTL', 0.0, 0)
        ylabel = 'N(S)/N\d0\u(S)'
        call pglabel (xlabel, ylabel, title)
        call pgline (i-1, xx, yy2)
c
        call pgpage
        call pgswin (xmin, xmax, ymin, ymax)
        call pgbox ('BCNSTL', 0.0, 0, 'BCNSTL', 0.0, 0)
        ylabel = 'N(S) counts Jy\u-1\d sr\u-1\d'
        call pglabel (xlabel, ylabel, title)
        call pgline (i-1, xx, yy)
c
        call pgend
      end if
c
c Open input image
c
      if (out.ne.' ') then
        call xyopen (li, in, 'old', maxnax, sizei)
        call rdhdi (li, 'naxis', naxisi, 0)
        do i = 1, naxisi
          str = itoaf(i)
          call rdhdd (li, 'crpix'//str, crpixi(i), dble(sizei(i))/2.0d0)
          call rdhdd (li, 'cdelt'//str, cdelti(i), 1.0d0)
        end do
c
c Open output image and copy header items to it   
c
        sizeo(1) = sizei(1) / 2
        sizeo(2) = sizei(2) / 2      
        call xyopen (lo, out, 'new', 2, sizeo)
        call headcopy (li, lo, 0, 2, 0, 0)
        do i = 1, 2
          crpixo(i) = sizei(i) / 2 + 1
          str = itoaf(i)   
          call wrhdd (lo, 'crpix'//str, crpixo(i))
        end do
c
c Write history
c
        call hisopen (lo,'append') 
        call hiswrite (lo, 'IMCFN: Miriad '//version)
        call hisinput (lo, 'IMCFN')
        call hisclose (lo)
c
c Allocate memory for input and output images
c
        call memalloc (ipi, sizei(1)*sizei(2), 'r')
        call memalloc (ipo, sizeo(1)*sizeo(2), 'r')
c
c Read in the synthesized beam 
c
        do j = 1, sizei(2)
          ip = ipi + (j-1)*sizei(1) 
          call xyread (li, j, memr(ip))
        end do
c
c Do the beam integral and multiply by source integral 
c
        call bemint (li, sum, sizei(1), sizei(2), memr(ipi), 
     +               cdelti, crpixi, sizeo(1), sizeo(2), memr(ipo))
c
c Write out confusion noise image
c
        do j = 1, sizeo(2)
          ip = ipo + (j-1)*sizeo(1) 
          call xywrite (lo, j, memr(ip))
        end do
c
c Close up
c
        call memfree (ipi, sizei(1)*sizei(2), 'r')
        call memfree (ipo, sizeo(1)*sizeo(2), 'r')
        call xyclose (li)
        call xyclose (lo)
      end if
c
      end
c
c
      subroutine bemint (lsb, ssum, nx, ny, sb, cdelt, 
     +                   crpix, nxo, nyo, cf)
c-----------------------------------------------------------------------
c     For each (x0,y0) do the primary-synthesized beam integral
c
c  Input
c    ssum    integral of S**2 N(S) dS in Jy**2 per steradian
c
c       I(x0,y0) = PB(x,y) * SB(x-x0,y-y0)
c
c-----------------------------------------------------------------------
      implicit none
      integer nx, ny, nxo, nyo, lsb
      real sb(nx*ny), cf(nxo,nyo)
      double precision cdelt(2), crpix(2), ssum
cc
      real pbget, dj, di
      integer pbobj, i, j, i1, j1, i2, j2, ii, jj, k
      character pbtype*10, str*80, line*80
      double precision dx, dy, sum, prod, imax
      data imax /0.0/
c-----------------------------------------------------------------------
c
c Initialize primary beam routines
c
      call pbRead (lsb, pbtype)
      call coinit (lsb)
      call pbInit (pbObj, pbtype, lsb)
c
c Loop through output image locations
c
      dx = abs(cdelt(1))
      dy = abs(cdelt(2))
c
      i1 = nx/4 + 1
      i2 = nx - i1 + 1
      j1 = ny/4 + 1
      j2 = ny - j1 + 1
      do j = j1, j2
        if (mod(j-j1+1,10).eq.1) then
          write (str, '(a,1x,i4,1x,a,1x,i4)') 'Starting output row = ', 
     +           j-j1+1, ' of ', j2-j1+1
          call output (str)
        end if
c
        dj = j - crpix(2)
        do i = i1, i2
c
c Compute sum of PB and SB images for this shift
c
          sum = 0.0
          di = i - crpix(1) 
          do k = 1, nx*ny
            jj = k/nx
            if (mod(k,nx).ne.0) jj = jj + 1
            ii = k - (jj-1)*nx
c
            prod = sb(k) * pbget(pbobj,real(ii)-di,real(jj)-dj)
     +             * dx * dy
            sum = sum + prod**2 
          end do
c
c Fill output image as sigma in Jy
c
          imax = max(imax,sum)
          cf(i-i1+1,j-j1+1) = sqrt(ssum*sum)
        end do
      end do
      write (line,10) imax
10    format ('Maximum value of beam integral = ', 
     +        1pe14.6, ' radian**2')
      call output (line)
c
c Close up
c
      call cofin (lsb)
      call pbfin (pbobj)
c
      end
