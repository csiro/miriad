      program hanning

c= HANNING - Smooth a cube along the spectral axis
c& bpw
c: map combination
c+
c       Hanning does a Hann or boxcar smooth on the spectral axis of
c       a Miriad dataset.  It determines the axis from the header, or
c       else assumes that it is the z-axis.  Masked pixels are zeroed
c       before smoothing.
c
c@ in
c        The input image.  No default.
c
c@ out
c        The output image.  No default.
c
c@ region
c        Region of the input image to process.
c
c@ object
c        hanning or boxcar.  Default is hanning.
c
c@ width
c        The number of channels over which to smooth.  Must be an odd
c        number.  Default is 3.
c
c$Id: hanning.for,v 1.4 2011/04/04 02:50:34 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      integer    MAXBOXES, MAXWIDTH
      parameter (MAXBOXES=1024, MAXWIDTH=7)

      logical   mask(MAXDIM)
      integer   axlen(MAXNAX), boxes(MAXBOXES), i, iblc(MAXNAX),
     *          itrc(MAXNAX), j, lIn, lOut, naxis, nchan, nprofiles,
     *          oblc(MAXNAX), otrc(MAXNAX), spcAxI, viraxlen(MAXNAX),
     *          vircsz(MAXNAX), width
      real      coeffs(MAXWIDTH*2+1), rdat(MAXDIM), work(MAXWIDTH*2+1)
      character axC*7, inp*1024, object*8, outp*1024, spcAxC, version*72

      external  versan
      character versan*72

      data axC /'xyzabcd'/
c-----------------------------------------------------------------------
      version = versan('hanning',
     *                 '$Revision: 1.4 $',
     *                 '$Date: 2011/04/04 02:50:34 $')

c     Get and check the inputs.
      call keyini

      call keyf('in',  inp, ' ')
      call assertl(inp.ne.' ', 'Input file name is missing')

      call keya('out', outp, ' ')
      call assertl(outp.ne.' ', 'Output file name is missing')

      call boxinput('region', inp, boxes, MAXBOXES)

      call keya('object',object,'hanning')
      if (object.ne.'hanning' .and. object.ne.'boxcar') then
        call bug('f', 'Invalid object')
      endif

      call keyi('width', width, 3)
      call assertl(mod(width,2).eq.1, 'Width must be odd number')

      call keyfin

c     Open the input image.
      naxis = MAXNAX
      call xyzopen(lIn, inp, 'old', naxis, axlen)
      if (naxis.lt.3) then
        call bug('f', 'Input image has fewer than three axes')
      endif

c     Set regions.
      call boxset(boxes, naxis, axlen, ' ')
      call boxinfo(boxes, naxis, iblc, itrc)

c     Find the spectral axis.
      call coInit(lIn)
      call coFindAx(lIn, 'spectral', spcAxI)
      call coFin(lIn)

      if (spcAxI.eq.0) spcAxI = 3
      spcAxC = axC(spcAxI:spcAxI)

c     Set up for reading the spectral axis.
      call xyzsetup(lIn, spcAxC, iblc, itrc, viraxlen, vircsz)
      nchan     = viraxlen(1)
      nprofiles = vircsz(naxis) / vircsz(1)

c     Open the output image and set up for writing.
      do i = 1, naxis
        axlen(i) = itrc(i) - iblc(i) + 1
        oblc(i)  = 1
        otrc(i)  = axlen(i)
      enddo

      call xyzopen(lOut, outp, 'new', naxis, axlen)
      call headcp(lIn, lOut, naxis, 0, iblc, itrc)
      call xyzsetup(lOut, spcAxC, oblc, otrc, viraxlen, vircsz)

c     Do the smoothing.
      if (object.eq.'hanning') then
        call hcoeffs(width, coeffs)
      else if (object.eq.'boxcar') then
        call bcoeffs(width, coeffs)
      endif

      do i = 1, nprofiles
        call xyzprfrd(lIn, i, rdat, mask, nchan)
        do j = 1, nchan
          if (.not.mask(j)) rdat(j) = 0.0
        enddo

        if (object.eq.'hanning') then
          call hannsm(width, coeffs, nchan, rdat, work)
        else if (object.eq.'boxcar') then
          call boxcarsm(width, coeffs, nchan, rdat, work)
        endif

        call xyzprfwr(lOut, i, rdat, mask, nchan)
      enddo

c     Write history.
      call hisopen(lOut, 'append')
      call hisinput(lOut, 'HANNING: '//version)
      call hisclose(lOut)

c     Finish up.
      call xyzclose(lIn)
      call xyzclose(lOut)

      end
