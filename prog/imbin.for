      program imbin
c-----------------------------------------------------------------------
c
c= IMBIN - Bin up an image.
c& nebk
c: image analysis
c+
c	IMBIN bins up (averages) pixels, and/or picks out every Nth pixel, 
c	in some combination of the first three dimensions of an image.
c	If the selected binning size does not integrally fit into the
c	size of the axis, the region is adjusted so that it does
c	if possible.
c
c	An output pixel is blanked only if there were no valid
c	contributing input pixels.
c
c@ in
c	Input image.  Wild card expansion supported. No default.
c@ region
c	Standard region of interest. See the help on "region" for
c	more information. The default is the entire input.
c@ bin
c	A pair of values for each axis.  These give the spatial increment 
c	and binning size in pixels for each axis to be applied to  the
c	selected region.  If the binning size is not unity, it must equal
c	the increment.  For example, to bin up the image by 2 pixels
c	in the x direction, and to pick out every third pixel in the 
c	z direction, set BIN=2,2, 1,1, 3,1   
c	Defaults are 1,1 for each axis.  
c@ out
c	Output image
c--
c
c  History:
c    nebk 11Jan95  Original version
c    nebk 14nov95  New call for READIMCG
c    nebk 25may96  Fix glaring error with 2-D images
c    rjs  12oct99  Correctly handle mosaic tables. Other cosmetic
c		   improvements.
c    dpr  08nov00  make bin specs for 3rd axis redundant for 2-D 
c                  images.
c
c-----------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
c
      character version*(*)
      integer maxbox
      parameter (maxbox = 1024)
      parameter (version = 'ImBin: version 1.0 12-Oct-99')
c
      integer sizin(maxnax), sizout(maxnax), blc(maxnax), trc(maxnax), 
     + bin(2,maxnax), nbin, boxes(maxbox), krng(2), lin, lout, ip, ipn, 
     + i, j, k,l, naxis, p, pn, nx, ny, npnt
      double precision cdelti(maxnax), crvali(maxnax), crpixi(maxnax),
     + cdelto(maxnax), crpixo(maxnax)
      real dmm(2), mm(2)
      logical flags(maxdim), blanks
      character in*64, out*64, itoaf*1, str*1, line*80
c
c  Externals.
c
      logical hdprsnt
c
c Get user inputs
c
      call output (version)
      call keyini
      call keyf ('in', in, ' ')
      if (in.eq.' ') call bug ('f', 'No input image given')
      call keya ('out', out, ' ')
      if (out.eq.' ') call bug ('f', 'No output image given')
      if (in.eq.out) call bug ('f', 
     +  'Input and output images must be different')
      call mkeyi ('bin', bin, maxnax*2, nbin)
      if (nbin.eq.0) call bug ('f', 'You must give some binning')
      if (mod(nbin,2).ne.0)call bug('f','Invalid number of bins')
      call boxinput ('region', in, boxes, maxbox)
      call keyfin
c
c Open input image
c
      call xyopen (lin, in, 'old', maxnax, sizin)
      call rdhdi (lin, 'naxis', naxis, 0)
      naxis = min(naxis,maxnax)
c
      do i=nbin/2+1,naxis
        bin(1,i) = 1
        bin(2,i) = 1
      enddo
c
      call output (' ')
      call output (' Axis   inc    bin')
      call output ('------------------')
      do i = 1, naxis
        write (line, 100) i, bin(1,i), bin(2,i)
100     format (2x, i2, 2x, i4, 3x, i4) 
        call output (line)
      end do
c
c Finish key inputs for region of interest 
c
      call boxset (boxes, naxis, sizin, 's')
      call boxinfo (boxes, naxis, blc, trc)
c
c Read input image header items and adjust window sizes to fit 
c binning factors integrally
c
      do i = 1, naxis
        str = itoaf(i)
        call rdhdd (lin, 'crpix'//str, crpixi(i), dble(sizin(i))/2.0)
        call rdhdd (lin, 'cdelt'//str, cdelti(i), 1.0d0)
        call rdhdd (lin, 'crval'//str, crvali(i), 0.0d0)
c
        if (bin(2,i).ne.1 .and. bin(2,i).ne.bin(1,i)) call bug ('f',
     +   'Image increment must equal bin size')
        call winfidcg (sizin(i), i, bin(1,i), blc(i), trc(i), sizout(i))
      end do
      if (naxis.lt.3) then
        do i = naxis+1,3
          sizout(i) = 1
          blc(i) = 1
          trc(i) = 1
c no binning in the 3rd axis ->
          if (nbin/2 .lt. 3) then
            do l=nbin/2+1,3
              bin(1,l) = 1
              bin(2,l) = 1
            enddo
          endif
c <- dpr 08-11-00
        end do
      end if        
c
c Open output image and copy header items to it
c  
      call xyopen (lout, out, 'new', naxis, sizout)
      call headcopy (lin, lout, 0, naxis, 0, 0)
      call hisopen (lout,'append')
      call hiswrite (lout, 'IMBIN: Miriad '//version)
      call hisinput (lout,'IMBIN')
      call hisclose (lout)
c
c Work out output image header items and write them out to the output 
c
      do i = 1, naxis
        cdelto(i) = bin(1,i) * cdelti(i)
        crpixo(i) = 0.5d0 - 
     +     ((dble(blc(i)) - 0.5d0 - crpixi(i)) * cdelti(i) / cdelto(i))
c
        str = itoaf(i)
        call wrhdd (lout, 'crpix'//str, crpixo(i))
        call wrhdd (lout, 'cdelt'//str, cdelto(i))
      end do
c
c Allocate memory for binned images
c
      call memalloc (ip,  sizout(1)*sizout(2), 'r')
      call memalloc (ipn, sizout(1)*sizout(2), 'i')
c
c Loop over input image
c
      dmm(1) =  1.0e32
      dmm(2) = -1.0e32
      krng(1) = blc(3)
      krng(2) = bin(2,3)
c    
      do k = 1, sizout(3)
c
c Bin up next subcube
c
        mm(1) = 1.0e32
        mm(2) = -1.0e32
        call readimcg (.true., 0.0, lin, bin(1,1), bin(1,2), krng,
     +    blc, trc, .true., memi(ipn), memr(ip), blanks, mm)
        dmm(1) = min(dmm(1), mm(1))
        dmm(2) = max(dmm(2), mm(2))
        krng(1) = krng(1) + bin(1,3)
c
c Write out plane of new image
c
        call xysetpl (lout, 1, k)
        do j = 1, sizout(2)
          p  = (j-1)*sizout(1) + ip
          pn = (j-1)*sizout(1) + ipn
c
          call xywrite (lout, j, memr(p))
          if (blanks) then
            do i = 1, sizout(1)
              flags(i) = memi(pn+i-1).gt.0
            end do
            call xyflgwr (lout, j, flags)
          end if
        end do
      end do
      call wrhdr (lout, 'datamax', dmm(2))
      call wrhdr (lout, 'datamin', dmm(1))
c
c  If there is a mosaicing table in the input, and if there is
c  some sort of decimation of the RA and DEC axes, then decimate
c  the mostable.
c
      if(hdprsnt(lin,'mostable').and.
     *	(bin(1,1).gt.1.or.bin(1,2).gt.1))then
	call mosLoad(lin,npnt)
	call mosGetn(nx,ny,npnt)
	call mosSetn(nx/bin(1,1),ny/bin(1,2))
	call mosSave(lout)
      endif
c
      call memfree (ip,  sizout(1)*sizout(2), 'r')
      call memfree (ipn, sizout(1)*sizout(2), 'i')
      call xyclose (lin)
      call xyclose (lout)
c
      end
