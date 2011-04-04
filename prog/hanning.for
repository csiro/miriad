      program hanning

c= HANNING - Smooth a cube along the velocity axis
c& bpw
c: map combination
c+
c       Hanning does a hanning or boxcar smooth on the spectral axis of
c       a Miriad dataset.  It determines the spectral axis from the
c       header, or else assumes that it is the z-axis.  Masked pixels
c       are set to zero before smoothing.
c
c@ in
c        The input image.  vxy and xyv images are acceptable inputs.
c        No default.
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
c$Id: hanning.for,v 1.3 2011/04/04 01:50:03 cal103 Exp $
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
     *          itrc(MAXNAX), j, naxis, nchan, nprofiles, oblc(MAXNAX),
     *          otrc(MAXNAX), tinp, tout, velaxnr, viraxlen(MAXNAX),
     *          vircsz(MAXNAX), width
      real      coeffs(MAXWIDTH*2+1), rdat(MAXDIM), work(MAXWIDTH*2+1)
      character inp*1024, object*8, out*1024, velaxis, version*72

      external  versan
      character versan*72
c-----------------------------------------------------------------------
      version = versan('hanning',
     *                 '$Revision: 1.3 $',
     *                 '$Date: 2011/04/04 01:50:03 $')

c     Get and check the inputs.
      call keyini

      call keyf('in',  inp, ' ')
      call assertl(inp.ne.' ', 'Input file name is missing')

      call keya('out', out, ' ')
      call assertl(out.ne.' ', 'Output file name is missing')

      call keya('object',object,'hanning')
      if (object.ne.'hanning' .and. object.ne.'boxcar') then
        call bug('f', 'Invalid object')
      endif

      call keyi('width', width, 3)
      call assertl((width/2)*2.ne.width, 'Width must be odd number')

      call keyfin

c     Open the input and set up for reading.
      naxis = MAXNAX
      call xyzopen(tinp, inp, 'old', naxis, axlen)

      call boxinput('region', inp, boxes, MAXBOXES)
      call boxset(boxes, naxis, axlen, ' ')
      call boxinfo(boxes, naxis, iblc, itrc)

      velaxis = 'z'
      call fndaxnum(tinp, 'freq', velaxis, velaxnr)

      call xyzsetup(tinp, velaxis, iblc, itrc, viraxlen, vircsz)
      nchan     = viraxlen(1)
      nprofiles = vircsz(naxis) / vircsz(1)

      do i = 1, naxis
        axlen(i) = itrc(i) - iblc(i) + 1
        oblc(i)  = 1
        otrc(i)  = axlen(i)
      enddo

c     Open the output image.
      call xyzopen(tout, out, 'new', naxis, axlen)
      call headcp(tinp, tout, naxis, 0, iblc, itrc)
      call xyzsetup(tout, velaxis, oblc, otrc, viraxlen, vircsz)

c     Do the smoothing.
      if (object.eq.'hanning') then
        call hcoeffs(width, coeffs)
      else if (object.eq.'boxcar') then
        call bcoeffs(width, coeffs)
      endif

      do i = 1, nprofiles
        call xyzprfrd(tinp, i, rdat, mask, nchan)
        do j = 1, nchan
          if (.not.mask(j)) rdat(j) = 0.0
        enddo

        if (object.eq.'hanning') then
          call hannsm(width, coeffs, nchan, rdat, work)
        else if (object.eq.'boxcar') then
          call boxcarsm(width, coeffs, nchan, rdat, work)
        endif

        call xyzprfwr(tout, i, rdat, mask, nchan)
      enddo

c     Write history.
      call hisopen(tout, 'append')
      call hisinput(tout, 'HANNING: '//version)
      call hisclose(tout)

c     Finish up.
      call xyzclose(tinp)
      call xyzclose(tout)

      end
