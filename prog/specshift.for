      program specshift

c= SPECSHIFT - shift velocity profiles
c& tw
c: analysis
c+
c       SPECSHIFT shifts all spectra in a cube according to a 2-D
c       template (a velocity field).  The velocity of the template is
c       made the central pixel of the spectrum.  If the template is
c       blanked or out-of-range the spectrum is unchanged.  Based on
c       Bart Wakker's HANNING.
c
c@ in
c        The input data cube.  No default.
c
c@ tin
c        The template image, should be 2-D with same RA/DEC axes as the
c        input cube.
c
c@ out
c        The output image.  No default.
c
c@ region
c        The region of the input cube to modify.  Default is the whole
c        cube.
c
c@ refpix
c        The velocity pixel which will be assigned to the template
c        velocity.  Default is the middle pixel of the v-range.
c
c@ options
c        doblank    Don't wrap profile around; blank if out of range.
c                   By default the edge channels are assumed to be
c                   noise so the profile is wrapped when shifting
c                   beyond edges.
c$Id: specshift.for,v 1.7 2012/03/13 02:21:33 wie017 Exp $
c--
c   History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      integer   MAXBOXES, NOPTS
      parameter (MAXBOXES=1024, NOPTS=1)

      logical   doblank, flags(MAXDIM), mask(MAXDIM), outrange,
     *          present(NOPTS), workmsk(MAXDIM)
      integer   axlen(MAXNAX), boxes(MAXBOXES), coords(MAXNAX), i,
     *          iblc(MAXNAX), ictrpx, itrc(MAXNAX), ivelpx, j, jold,
     *          lIn, lOut, lTem, naxis, nchan, nprofs, spcAxI,
     *          taxlen(MAXNAX), viraxlen(MAXNAX)
      ptrdiff   vircsz(MAXNAX),pix
      real      rdat(MAXDIM), fracshift, refpix, tdat(MAXDIM),
     *          work(MAXDIM)
      double precision b(MAXDIM), c(MAXDIM), d(MAXDIM), seval, u, vel,
     *          velpx, x(MAXDIM), y(MAXDIM)
      character algo*3, axC*7, inp*1024, line*80, opts(NOPTS)*8,
     *          outp*1024, spcAxC, tmpl*1024, version*72

      external  len1, versan
      integer   len1
      character versan*72

      data opts /'doblank'/
      data axC  /'xyzabcd'/
c-----------------------------------------------------------------------
      version = versan('specshift',
     *                 '$Revision: 1.7 $',
     *                 '$Date: 2012/03/13 02:21:33 $')

c     Get and check the inputs.
      call keyini

      call keyf('in',  inp, ' ')
      call assertl(inp.ne.' ', 'Input cube name is missing')

      call keyf('tin', tmpl, ' ')
      call assertl(tmpl.ne.' ', 'Template image name is missing')

      call keya('out', outp, ' ')
      call assertl(outp.ne.' ', 'Output file name is missing')

      call keyr('refpix',refpix,0.0)

      call boxinput('region', inp, boxes, MAXBOXES)

      call options('options',opts,present,NOPTS)
      doblank = present(1)

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
      call coSpcSet(lIn, 'VRAD', ' ', spcAxI, algo)
      if (spcAxI.eq.0) call bug('f', 'No spectral axis in input cube')
      spcAxC = axC(spcAxI:spcAxI)

c     Set up for reading the spectral axis.
      call xyzsetup(lIn, spcAxC, iblc, itrc, viraxlen, vircsz)
      nchan  = viraxlen(1)
      nprofs = vircsz(naxis) / vircsz(1)

c     Open the output image and set up for writing.
      call xyzopen(lOut, outp, 'new', naxis, axlen)
      call headcp(lIn, lOut, naxis, 0, iblc, itrc)
      call xyzsetup(lOut, spcAxC, iblc, itrc, viraxlen, vircsz)

c     Open the template image.
      call xyopen(lTem, tmpl, 'old', maxnax, taxlen)
      call rdhdi(lTem, 'naxis', naxis, 0)
      if (naxis.gt.2 .and. taxlen(3).gt.1) then
        call bug('w', tmpl(1:len1(tmpl))//' appears to have >2 axes')
        call bug('w',
     *    'Only the first plane of the 3rd axis will be used')
      endif
      call xysetpl(lTem, 1, 1)

c     Check image sizes for consistency.
      if (axlen(1).ne.taxlen(1)) then
        call bug('f', 'Unequal dimensions for input images on axis 1')
      endif
      if (axlen(2).ne.taxlen(2)) then
        call bug('f', 'Unequal dimensions for input images on axis 2')
      endif


c     Set velocity reference pixel.
      if (refpix.gt.0) then
        ictrpx = nint(refpix)
      else
        ictrpx = (nchan+1)/2
      endif
      write(line,'(a,i4)') 'Shifting template velocity to pixel',ictrpx
      call output(line)

      do i = 1, nprofs
c       Read the profile.
        pix = i
        call xyzs2c(lIn, pix, coords)
        call xyzread(lIn, coords, rdat, mask, nchan)

c       Read the velocity from the image.
        call xyread(lTem, coords(2), tdat)
        call xyflgrd(lTem, coords(2), flags)
        if (flags(coords(1))) then
          vel = dble(tdat(coords(1)))
          call coCvt1(lIn, spcAxI, 'aw', vel, 'ap', velpx)
          ivelpx = int(velpx)
          fracshift = velpx - ivelpx
        else
          ivelpx = 0
        endif

c       Shift if velocity is in range; otherwise do nothing.
        if (ivelpx.gt.1 .and. ivelpx.lt.nchan) then
          do j = 1, nchan + 1
            outrange=.false.
            jold = j + ivelpx - ictrpx
            do while (jold.lt.1)
              jold = jold + nchan
              outrange=.true.
            enddo

            do while (jold.gt.nchan)
              jold = jold - nchan
              outrange=.true.
            enddo

            work(j) = rdat(jold)
            if (doblank .and. outrange) then
              workmsk(j) = .false.
            else
              workmsk(j) = mask(jold)
            endif

            x(j) = dble(j)
            y(j) = dble(work(j))
          enddo

          call spline(nchan+1, x, y, b, c, d)
          do j = 1, nchan
            u = x(j) + fracshift
            work(j) = seval(nchan+1, u, x, y, b, c, d)
          enddo
        else
          do j = 1, nchan
            work(j) = rdat(j)
            workmsk(j) = mask(j)
          enddo
        endif

        call xyzprfwr(lOut, i, work, workmsk, nchan)
      enddo
      call coFin(lIn)

c     Write history.
      call hisopen( lOut, 'append')
      call hisinput(lOut, 'SPECSHIFT: '//version)
      call hisclose(lOut)

c     Finish up.
      call xyzclose(lIn)
      call xyzclose(lOut)
      call xyclose(lTem)

      end
