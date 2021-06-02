      program imcfn

c= IMCFN - Compute confusion noise for an interferometer image
c& nebk
c: image analysis
c+
c       IMCFN computes confusion noise in an interferometer image
c       owing to the presence of unresolved background sources.
c
c       At each location (x0,y0) in the output image, the variance
c       of the confusion noise is given by the product of 2 integrals
c
c       var(x0,y0)  =  I1 * I2(x0,y0)
c
c       where I1 is the source integral defined by
c
c                - Smax
c          I1 = |        N(S) S**2 dS
c              - Smin
c
c
c       and I2 is the beam integral defined by
c
c                       --
c          I2(x0,y0) = ||   [SB(x-x0,y-y0) * PB(x,y)]**2  dx dy
c                     --
c
c       Thus, at the desired output location (x0,y0), the synthesized
c       beam is shifted to the location (x0,y0), and then the product of
c       the shifted beam and the primary beam is formed, and then that
c       product squared is summed over the size of the synthesized
c       beam image.
c
c       The user is told what the source integral value is.  Thus,
c       you can in fact rescale the output image to other flux density
c       ranges just by knowing the square root of the flux integral.
c
c       The program can run without an input beam or output image
c       whereupon just the source integral is done.   This can be useful
c       because the computation of the beam integral is very slow, as
c       for each output pixel, a sum over an image the size of the
c       synthesized beam must be done.
c
c       The user inputs the differential and normalized source count
c       function N(S)/N0(S) where S is the flux density.   The units
c       of N(S) are counts/Jy/steradian.    The user also specifies
c       N0, which is a cosmological normalization, usually a power
c       law of S.   This function is input via a power law and/or
c       polynomial.  A break flux density is given below which the
c       power law is used and above which the polynomial is used.
c
c       In the help file, examples are given for keyword values for
c       the 20cm number counts.  These results come from Windhorst
c       et al 1993, ApJ, 405, 409
c
c@ beam
c       Synthesised dirty beam image (must be power of 2). No default.
c@ out
c       Confusion noise (sigma) image (1/2 size of beam image) in Jy
c@ flux
c       3 numbers (Jy).  flux(1) and flux(2) define the range of flux
c       densities for which the confusion noise is calculated.
c       flux(3) is the break point below which the power law is
c       used, and above which the polynomial is used.  Set flux(3)
c       above flux(2) if you wish to use only the power law. Set
c       flux(3) below flux(1) if you wish to use only the polynomial.
c       No defaults.
c
c       For the 20cm counts, the break point is flux(3) = 1E-4 Jy
c       The polynomial coefficients are good from flux(3) to
c       flux(2) = 10 Jy.  The power law is good from
c       flux(1) = 1E-5 Jy to flux(3) = 1E-4 Jy.   Below 1E-5 Jy
c       the function is unknown, but must turn over and converge
c       so as not to distort the CMB spectrum.
c@ poly
c       N(S)/N0(S) specified as a polynomial of up to 5th order fit to
c       log10(N/N0 counts/Jy/sr) vs log10(S Jy)  You input up to 6
c       polynomial coefficients (low to high order).
c
c       For the 20cm number counts, the polynomial coefficients are
c       2.519192, -0.139680, -0.302235, 0.067539, 0.046945, 0.005320
c       See the flux keyword for the acceptable range.
c@ power
c       N(S)/N0(S) (counts/Jy/sr) specified as a power law
c       a * S**p where S is in Jy and you give a and p.
c
c       For the 20cm number counts, the power law is a = 195
c       and p = 0.45   See the flux keyword for the acceptable range.
c@ n0
c       The normalization factor N0 = f * S**b  where S is in Jy
c       You give f and b.
c
c       For the 20cm number counts, f = 1.0 and b = -2.5
c@ device
c       Plot device on which to plot the differential normalized
c       and un-normalized source count functions.  You can specify
c       a plot device but not an output or beam image if you just
c       want to see the plots but don't want to spend ages computing
c       the beam integral.
c
c$Id: imcfn.for,v 1.9 2021/06/02 04:45:09 wie017 Exp $
c--
c  History:
c    nebk 13sep95 Original version
c    rjs   2jul98 Increase ize of pbtype variable.
c    rjs  08may00 Change incorrect call to keyf to keya.
c    rjs  06apr09 Make sure do loop parameters are integer valued.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      integer   MAXPC, MAXPTS
      parameter (MAXPC = 6, MAXPTS = 10000)

      integer   i, iAxLen(MAXNAX), ierr,  j, li, lo,
     *          naxisi, nc, oAxLen(MAXNAX)
      ptrdiff   ip, ipi, ipo
      real      xmax, xmin, xx(MAXPTS), ymax, ymax2, ymin, ymin2,
     *          yy(MAXPTS), yy2(MAXPTS)
      double precision a, b, c(MAXPC), cdelti(MAXNAX), crpixi(MAXNAX),
     *          crpixo(MAXNAX), dlogs, ds, n, n0(2), nn, s, s1, s2, s3,
     *          sJy, sb, smax, smin, sold, sum
      character device*80, in*132, itoaf*1, line*80, out*132, str*1,
     *          title*80, version*72, xlabel*80, ylabel*80

      integer   pgbeg
      character versan*72
      external  pgbeg, versan

      data c /MAXPC*0d0/
      data xmin,  xmax  /1e30, -1e30/
      data ymin,  ymax  /1e30, -1e30/
      data ymin2, ymax2 /1e30, -1e30/
c-----------------------------------------------------------------------
      version = versan('imcfn',
     *                 '$Revision: 1.9 $',
     *                 '$Date: 2021/06/02 04:45:09 $')

c     Get the inputs.
      call keyini
      call keyf('beam', in, ' ')
      call keya('out', out, ' ')
      call keyd('flux', smin, 0d0)
      call keyd('flux', smax, 0d0)
      call keyd('flux', sb, 0d0)
      call mkeyd('poly', c, MAXPC, nc)
      call keyd('power', a, 0d0)
      call keyd('power', b, 0d0)
      call keyd('n0', n0(1), 0d0)
      call keyd('n0', n0(2), 0d0)
      call keya('device', device, ' ')
      call keyfin

c     Check inputs.
      if (out.ne.' ' .and. in.eq.' ') call bug('f',
     *  'You must give an input beam')
      if (smin.le.0.0 .or. smax.le.0.0 .or. sb.le.0.0) call bug('f',
     *  'Flux densities must be positive')
      if (n0(1).le.0.0 .and. n0(2).eq.0.0) call bug('f',
     *  'You must give the normalization expression')
      if (nc.eq.0 .and. a.eq.0.0 .and. b.eq.0.0) call bug('f',
     *  'You have not given a differential source function')
      if (nc.eq.0 .and. a.le.0.0) call bug('f', 'a must be > 0')

c     Do the source flux integral.
      sum = 0.0
      s1 = log10(smin)
      s2 = log10(smax)
      s3 = log10(sb)
      dlogs = (s2-s1)/real(MAXPTS-1)
      sold = s1
      i = 1

      s = s1 + dlogs
      do while (s.le.s2)
        sjy = 10**s

        if (s.le.s3) then

c         Evaluate N/N0 from power law.
          nn = a * sJy**b
        else

c         Evaluate N/N0 with polynomial and Horner's rule.
          nn = c(nc)
          do j = nc-1, 1, -1
            nn = nn * s + c(j)
          enddo
          nn = 10**nn
        endif

c       Scale by N0 to make N(S) in counts/Jy/steradian.
        n = nn * n0(1) * sJy**n0(2)

c       Update sum.
        ds = sjy - sold
        sum = sum + sjy**2*n*ds

c       Fill plot arrays.
        xx(i) = real(s)
        yy(i) = real(log10(n))
        yy2(i) = real(log10(nn))
        xmin  = min (xmin,xx(i))
        xmax  = max (xmax,xx(i))
        ymin  = min (ymin,yy(i))
        ymax  = max (ymax,yy(i))
        ymin2 = min (ymin2,yy2(i))
        ymax2 = max (ymax2,yy2(i))

        i = i + 1
        sold = sjy
        s = s + dlogs
      enddo

      write(line,10) sum
10    format('Source integral = ', 1pe14.6, ' Jy**2 per steradian')
      call output(line)

c     Draw plot.
      if (device.ne.' ') then
        xlabel = 'S (Jy)'
        title = 'Differential Source counts'
        ierr = pgbeg (0, device, 1, 2)
        if (ierr.ne.1) then
          call pgldev
          call bug('f', 'Error opening plot device')
        endif
        call pgsvp(0.2, 0.8, 0.1, 0.9)

        call pgpage
        call pgswin(xmin, xmax, ymin2, ymax2)
        call pgbox('BCNSTL', 0.0, 0, 'BCNSTL', 0.0, 0)
        ylabel = 'N(S)/N\d0\u(S)'
        call pglabel(xlabel, ylabel, title)
        call pgline(i-1, xx, yy2)

        call pgpage
        call pgswin(xmin, xmax, ymin, ymax)
        call pgbox('BCNSTL', 0.0, 0, 'BCNSTL', 0.0, 0)
        ylabel = 'N(S) counts Jy\u-1\d sr\u-1\d'
        call pglabel(xlabel, ylabel, title)
        call pgline(i-1, xx, yy)

        call pgend
      endif

c     Open input image.
      if (out.ne.' ') then
        call xyopen(li, in, 'old', MAXNAX, iAxLen)
        call rdhdi(li, 'naxis', naxisi, 0)
        do i = 1, naxisi
          str = itoaf(i)
          call rdhdd(li, 'crpix'//str, crpixi(i), dble(iAxLen(i))/2d0)
          call rdhdd(li, 'cdelt'//str, cdelti(i), 1d0)
        enddo

c       Open output image and copy header items to it.
        oAxLen(1) = iAxLen(1) / 2
        oAxLen(2) = iAxLen(2) / 2
        call xyopen(lo, out, 'new', 2, oAxLen)
        call headcp(li, lo, 0, 0, 0, 0)
        do i = 1, 2
          crpixo(i) = iAxLen(i) / 2 + 1
          str = itoaf(i)
          call wrhdd(lo, 'crpix'//str, crpixo(i))
        enddo

c       Write history.
        call hisopen(lo,'append')
        call hiswrite(lo, 'IMCFN: Miriad '//version)
        call hisinput(lo, 'IMCFN')
        call hisclose(lo)

c       Allocate memory for input and output images.
        call memalloc(ipi, iAxLen(1)*iAxLen(2), 'r')
        call memalloc(ipo, oAxLen(1)*oAxLen(2), 'r')

c       Read in the synthesized beam.
        do j = 1, iAxLen(2)
          ip = ipi + (j-1)*iAxLen(1)
          call xyread(li, j, memr(ip))
        enddo

c       Do the beam integral and multiply by source integral.
        call bemint(li, sum, iAxLen(1), iAxLen(2), memr(ipi),
     *               cdelti, crpixi, oAxLen(1), oAxLen(2), memr(ipo))

c       Write out confusion noise image.
        do j = 1, oAxLen(2)
          ip = ipo + (j-1)*oAxLen(1)
          call xywrite(lo, j, memr(ip))
        enddo

c       Close up.
        call memfree(ipi, iAxLen(1)*iAxLen(2), 'r')
        call memfree(ipo, oAxLen(1)*oAxLen(2), 'r')
        call xyclose(li)
        call xyclose(lo)
      endif

      end

c***********************************************************************

      subroutine bemint (lsb, ssum, nx, ny, sb, cdelt,
     *                   crpix, nxo, nyo, cf)

      integer   nx, ny, nxo, nyo, lsb
      real      sb(nx*ny), cf(nxo,nyo)
      double precision cdelt(2), crpix(2), ssum
c-----------------------------------------------------------------------
c  For each (x0,y0) do the primary-synthesized beam integral.
c
c  Input
c    ssum    integral of S**2 N(S) dS in Jy**2 per steradian
c
c       I(x0,y0) = PB(x,y) * SB(x-x0,y-y0)
c-----------------------------------------------------------------------
      real pbget, dj, di
      integer pbobj, i, j, i1, j1, i2, j2, ii, jj, k
      character pbtype*16, str*80, line*80
      double precision dx, dy, sum, prod, imax
      data imax /0.0/
c-----------------------------------------------------------------------
c     Initialize primary beam routines.
      call pbRead(lsb, pbtype)
      call coinit(lsb)
      call pbInit(pbObj, pbtype, lsb)

c     Loop through output image locations.
      dx = abs(cdelt(1))
      dy = abs(cdelt(2))

      i1 = nx/4 + 1
      i2 = nx - i1 + 1
      j1 = ny/4 + 1
      j2 = ny - j1 + 1
      do j = j1, j2
        if (mod(j-j1+1,10).eq.1) then
          write(str, '(a,1x,i4,1x,a,1x,i4)') 'Starting output row = ',
     *           j-j1+1, ' of ', j2-j1+1
          call output(str)
        endif

        dj = int(j - crpix(2))
        do i = i1, i2

c         Compute sum of PB and SB images for this shift.
          sum = 0.0
          di = int(i - crpix(1))
          do k = 1, nx*ny
            jj = k/nx
            if (mod(k,nx).ne.0) jj = jj + 1
            ii = k - (jj-1)*nx

            prod = sb(k) * pbget(pbobj,real(ii)-di,real(jj)-dj)
     *             * dx * dy
            sum = sum + prod**2
          enddo

c         Fill output image as sigma in Jy.
          imax = max(imax,sum)
          cf(i-i1+1,j-j1+1) = real(sqrt(ssum*sum))
        enddo
      enddo
      write(line,10) imax
10    format('Maximum value of beam integral = ',
     *        1pe14.6, ' radian**2')
      call output(line)

c     Close up.
      call cofin(lsb)
      call pbfin(pbobj)

      end
