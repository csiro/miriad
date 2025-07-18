      program imblr

c= IMBLR - Replace blanked pixels in an image with the specified value
c& nebk
c: image analysis
c+
c       IMBLR replaces any pixel that is blanked by an unblanked pixel
c       with value specified by the user.
c
c@ in
c       Input image.  Wild card expansion supported.
c@ out
c       Output image
c@ value
c       Value to replace blanked pixels by. Default is zero.
c
c--
c
c  History:
c    nebk 27Nov95  Original version
c    nebk 16aug96  Tell user some numbers
c    nebk 13sep96  Tell them some more
c    rjs  12oct99  Do not write out gflags.
c    rjs  08may00  Change incorrect call to keyf to keya.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      logical   first, flags(MAXDIM)
      integer   axLen(MAXNAX), i, j, k, lin, lout, naxis
      real      data(MAXDIM), dmm(2), npix, npix2, val
      character in*64, line*132, out*64, version*72

      character versan*72
      external  versan
c-----------------------------------------------------------------------
      version = versan('imblr')

c     Get user inputs.
      call keyini
      call keyf('in', in, ' ')
      call keya('out', out, ' ')
      call keyr('value', val, 0.0)
      call keyfin

      if (in.eq.' ') call bug('f', 'No input image given')
      if (out.eq.' ') call bug('f', 'No output image given')
      if (in.eq.out) call bug('f',
     *  'Input and output images must be different')

c     Open the input image.
      call xyopen(lin, in, 'old', MAXNAX, axLen)
      call rdhdi(lin, 'naxis', naxis, 0)

c     Create the output image and copy header items to it.
      call xyopen(lout, out, 'new', naxis, axLen)
      call headcp(lin, lout, 0, 0, 0, 0)

      call hisopen(lout, 'append')
      call hiswrite(lout, 'IMBLR: Miriad '//version)
      call hisinput(lout, 'IMBLR')
      call hisclose(lout)

c     Loop over the input image.
      first = .true.
      npix2 = 0.0
      do k = 1, axLen(3)
        npix = 0.0
        call xysetpl(lin, 1, k)
        call xysetpl(lout, 1, k)

        do j = 1, axLen(2)
          call xyread(lin, j, data)
          if (first) then
            dmm(1) = data(1)
            dmm(2) = dmm(1)
            first = .false.
          endif
          call xyflgrd(lin, j, flags)

          do i = 1, axLen(1)
            if (.not.flags(i)) then
              data(i) = val
              npix = npix + 1
            endif
            dmm(1) = min(dmm(1),data(i))
            dmm(2) = max(dmm(2),data(i))
          enddo

          call xywrite(lout, j, data)
        enddo

        if (npix.gt.0.0) then
          write(line, 10) k, npix
10        format('Plane ', i3, ' : replaced ', f8.0, ' blanks')
          call output(line)
          npix2 = npix2 + npix
        endif
      enddo

      if (npix2.le.0.0) call output('There were no blanks')

      call wrhdr(lout, 'datamax', dmm(2))
      call wrhdr(lout, 'datamin', dmm(1))

      call xyclose(lin)
      call xyclose(lout)

      end
