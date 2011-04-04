      program vblank

c= VBLANK - mask outlier pixels in velocity space
c& thw
c: image-analysis
c+
c       VBLANK takes a cube that has already been masked and masks it
c       further so that unmasked pixels that are surrounded by only
c       masked pixels along axis 1 will be masked.  The default is to
c       mask if both adjacent pixels are masked; one can increase the
c       window size with keyword 'tol'.
c
c       This can be useful for reducing noise in a datacube that has
c       already been masked with a smoothed cube and where the velocity
c       field is assumed to be smooth.  In this case axis 1 would be the
c       velocity axis and spikes far from the channels of actual
c       emission are masked.
c
c       NOTE: Because flagging is only implemented for planes, the
c       velocity axis must be axis 1.  Use REORDER first to make this
c       so:
c         reorder in=file1 out=file2 mode='312'
c         vblank  in=file2
c         reorder in=file2 out=file3 mode='231'
c
c< in
c
c@ tol
c       How far (in pixels) the nearest unmasked pixel can be for the
c       given pixel to stay unmasked.  Default is 1.
c
c$Id: vblank.for,v 1.2 2011/04/04 05:38:40 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      logical   flags(MAXDIM)
      integer   axlen(MAXNAX), blc(MAXNAX), i, i0, i1, iscan, ivaxis, j,
     *          k, lIn, naxis, nflag, nscan, tol, trc(MAXNAX)
      real      buf(MAXDIM)
      character inp*80, msg*80, vaxis*1, version*72

      external  versan
      character versan*72
c-----------------------------------------------------------------------
      version = versan('vblank',
     *                 '$Revision: 1.2 $',
     *                 '$Date: 2011/04/04 05:38:40 $')

c     Get and check the inputs.
      call keyini

      call keyf('in', inp, ' ')
      call assertl(inp.ne.' ', 'You must specify an input file')

      call keyi('tol', tol, 1)
      call keyfin

c     Open the input image.
      naxis = MAXNAX
      call xyopen(lIn, inp, 'old', naxis, axlen)
      do i = 1, naxis
        blc(i) = 1
        trc(i) = axlen(i)
      enddo

c     Find the velocity axis.
      call fndaxnum(lIn, 'freq', vaxis, ivaxis)
      call assertl(vaxis.ne.' ', 'Velocity axis not found in dataset')
      call assertl(ivaxis.eq.1,
     *          'Velocity axis must be axis 1 (use REORDER)')

c     Apply masking.
      nflag = 0
      do k = blc(3), trc(3)
        call xysetpl(lIn,1,k)

        do j = blc(2), trc(2)
          call xyread(lIn,j,buf)
          call xyflgrd(lIn,j,flags)

          do i = blc(1), trc(1)
            if (flags(i)) then
              i0 = max(i-tol,1)
              i1 = min(i+tol,trc(1))
              nscan=0
              do iscan = i0, i1
                if (flags(iscan)) nscan = nscan + 1
              enddo

              if (nscan.eq.1) then
                flags(i) = .false.
                nflag = nflag + 1
              endif
            endif
          enddo

          call xyflgwr(lIn,j,flags)
        enddo
      enddo

c     Report number of extra pixels flagged.
      write(msg,'(i7,'' extra pixels flagged'')') nflag
      call output(msg)

c     Update history.
      call hisopen(lIn,'append')
      call hiswrite(lIn,'VBLANK: '//version)
      call hisinput(lIn,'VBLANK')
      call hiswrite(lIn,'VBLANK: '//msg)
      call hisclose(lIn)

      call xyclose(lIn)

      end
