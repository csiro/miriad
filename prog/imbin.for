      program imbin

c= IMBIN - Bin up an image.
c& nebk
c: image analysis
c+
c       IMBIN bins up (averages) pixels, and/or picks out every Nth
c       pixel, in some combination of the first three dimensions of an
c       image.  If the selected binning size does not integrally fit
c       into the size of the axis, the region is adjusted so that it
c       does if possible.
c
c       An output pixel is blanked only if there were no valid
c       contributing input pixels.
c
c@ in
c       Input image.  Wild card expansion supported.  No default.
c@ region
c       Standard region of interest.  See the help on "region" for
c       more information.  The default is the entire input.
c@ bin
c       A pair of values for each axis.  These give the spatial
c       increment and binning size in pixels for each axis to be applied
c       to the selected region.  If the binning size is not unity, it
c       must equal the increment.  For example, to bin up the image by 2
c       pixels in the x direction, and to pick out every third pixel in
c       the z direction, set BIN=2,2, 1,1, 3,1
c       Defaults are 1,1 for each axis.
c@ out
c       Output image
c@ options
c       Extra processing options.  Only the minimum characters to avoid
c       ambiguity is needed.
c         sum       Produce sum rather than average of pixels in
c                   each bin
c
c$Id: imbin.for,v 1.8 2010/11/22 06:10:00 cal103 Exp $
c--
c
c  History:
c    nebk 11Jan95  Original version
c    nebk 14nov95  New call for READIMCG
c    nebk 25may96  Fix glaring error with 2-D images
c    rjs  12oct99  Correctly handle mosaic tables.  Other cosmetic
c                  improvements.
c    dpr  08nov00  make bin specs for 3rd axis redundant for 2-D
c                  images.
c    bmg  11may01  Added options=sum
c   nebk  14nov01  Track change to readimcg interface
c    rjs  19jan06  Fortran standardisation.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      integer MAXBOX, NOPTS
      parameter (MAXBOX = 1024, NOPTS = 1)

      logical   aver, blanks, flags(MAXDIM), present(NOPTS)
      integer   bin(2,MAXNAX), blc(MAXNAX), boxes(MAXBOX), i, ip, ipn,
     *          j, k, krng(2), lin, lout, naxis, nbin, npnt, nx, ny, p,
     *          pn, sizin(MAXNAX), sizout(MAXNAX), trc(MAXNAX)
      real      dmm(2), mm(3)
      double precision cdelti(MAXNAX), cdelto(MAXNAX), crpixi(MAXNAX),
     *          crpixo(MAXNAX), crvali(MAXNAX)
      character in*64, itoaf*1, line*80, opts(NOPTS)*8, out*64, str*1,
     *          version*80

c     Externals.
      logical hdprsnt
      character versan*80

      data opts /'sum     '/
c-----------------------------------------------------------------------
      version = versan ('imbin',
     *                  '$Revision: 1.8 $',
     *                  '$Date: 2010/11/22 06:10:00 $')

      do i = 1, MAXNAX
        blc(i) = 1
        trc(i) = 1
        sizout(i) = 1
        bin(1,i) = 1
        bin(2,i) = 1
      enddo

c     Get user inputs.
      call keyini
      call keyf('in', in, ' ')
      if (in.eq.' ') call bug('f', 'No input image given')
      call keya('out', out, ' ')
      if (out.eq.' ') call bug('f', 'No output image given')
      if (in.eq.out) call bug('f',
     *  'Input and output images must be different')
      call mkeyi('bin', bin, MAXNAX*2, nbin)
      if (nbin.eq.0) call bug('f', 'You must give some binning')
      if (mod(nbin,2).ne.0) call bug('f','Invalid number of bins')

      call boxinput('region', in, boxes, MAXBOX)
      call options('options', opts, present, NOPTS)
      aver = .not.present(1)
      call keyfin

c     Open input image.
      call xyopen(lin, in, 'old', MAXNAX, sizin)
      call rdhdi(lin, 'naxis', naxis, 0)
      naxis = min(naxis,MAXNAX)

      call output(' ')
      call output(' Axis   inc   bin')
      call output(' ----------------')
      do i = 1, naxis
        write(line, '(i4,i6,i6)') i, bin(1,i), bin(2,i)
        call output(line)

        if (bin(2,i).ne.1 .and. bin(2,i).ne.bin(1,i)) then
          call bug('f', 'Image increment must equal bin size')
        endif
      enddo

c     Finish key inputs for region of interest.
      call boxset(boxes, naxis, sizin, 's')
      call boxinfo(boxes, naxis, blc, trc)


c     Read input image header items and adjust window sizes to fit
c     binning factors integrally.
      do i = 1, naxis
        str = itoaf(i)
        call rdhdd(lin, 'crpix'//str, crpixi(i), dble(sizin(i))/2d0)
        call rdhdd(lin, 'cdelt'//str, cdelti(i), 1d0)
        call rdhdd(lin, 'crval'//str, crvali(i), 0d0)

        call winfidcg(sizin(i), i, bin(1,i), blc(i), trc(i), sizout(i))
      enddo


c     Open output image and write header.
      call xyopen(lout, out, 'new', naxis, sizout)
      call headcp(lin, lout, 0, 0, 0, 0)

      call hisopen(lout,'append')
      call hiswrite(lout, 'IMBIN: Miriad '//version)
      call hisinput(lout,'IMBIN')
      call hisclose(lout)

      do i = 1, naxis
c       Adjust the reference pixel and increment for sampling.
        crpixo(i) = 1d0 + (crpixi(i) - blc(i)) / bin(1,i)
        cdelto(i) = bin(1,i) * cdelti(i)

c       Adjust the reference pixel for binning - not the reference
c       value which must be left unchanged for non-linear axes.
        crpixo(i) = crpixo(i) - 0.5d0 * (bin(2,i) - 1d0) / bin(2,i)

        str = itoaf(i)
        call wrhdd(lout, 'crpix'//str, crpixo(i))
        call wrhdd(lout, 'cdelt'//str, cdelto(i))
      enddo


c     Allocate memory for binned images.
      call memalloc(ip,  sizout(1)*sizout(2), 'r')
      call memalloc(ipn, sizout(1)*sizout(2), 'i')

c     Loop over input image.
      dmm(1) =  1e32
      dmm(2) = -1e32
      krng(1) = blc(3)
      krng(2) = bin(2,3)

      do k = 1, sizout(3)
c       Bin up next subcube.
        mm(1) =  1e32
        mm(2) = -1e32
        call readimcg(.true., 0.0, lin, bin(1,1), bin(1,2), krng,
     *    blc, trc, aver, memi(ipn), memr(ip), blanks, mm)
        if (mm(1).lt.dmm(1)) dmm(1) = mm(1)
        if (mm(2).gt.dmm(2)) dmm(2) = mm(2)
        krng(1) = krng(1) + bin(1,3)

c       Write out plane of new image.
        call xysetpl(lout, 1, k)
        do j = 1, sizout(2)
          p  = (j-1)*sizout(1) + ip
          pn = (j-1)*sizout(1) + ipn

          call xywrite(lout, j, memr(p))
          if (blanks) then
            do i = 1, sizout(1)
              flags(i) = memi(pn+i-1).gt.0
            enddo
            call xyflgwr(lout, j, flags)
          endif
        enddo
      enddo
      call wrhdr(lout, 'datamax', dmm(2))
      call wrhdr(lout, 'datamin', dmm(1))

c     If there is a mosaicing table in the input and some sort of
c     decimation of the RA and DEC axes, then decimate the mostable.
      if (hdprsnt(lin,'mostable') .and.
     *   (bin(1,1).gt.1 .or. bin(1,2).gt.1)) then
        call mosLoad(lin,npnt)
        call mosGetn(nx,ny,npnt)
        call mosSetn(nx/bin(1,1), ny/bin(1,2))
        call mosSave(lout)
      endif

      call memfree(ip,  sizout(1)*sizout(2), 'r')
      call memfree(ipn, sizout(1)*sizout(2), 'i')
      call xyclose(lin)
      call xyclose(lout)

      end
