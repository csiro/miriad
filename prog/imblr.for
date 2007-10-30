      program imblr
c-----------------------------------------------------------------------
c
c= IMBLR - Replace blanked pixels in an image with a value of user's choice
c& nebk
c: image analysis
c+
c	IMBLR replaces any pixel that is blanked by an unblanked pixel
c	with value specified by the user.
c
c@ in
c	Input image.  Wild card expansion supported.
c@ out
c	Output image
c@ value
c	Value to replace blanked pixels by. Default is zero.
c--
c
c  History:
c    nebk 27Nov95  Original version
c    nebk 16aug96  Tell user some numbers
c    nebk 13sep96  Tell them some more
c  Notes:
c    Uses cgsubs.for
c
c-----------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'maxnax.h'
c
      character version*20
      parameter (version = 'Version 13-Sep-96')
c
      integer size(maxnax), lin, lout, i, j, k, naxis
      real dmm(2), val, data(maxdim), npix, npix2
      logical gflags(maxdim), flags(maxdim)
      character in*64, out*64, line*132
      data gflags /maxdim*.true./
      data dmm /1.0e30, -1.0e30/
c-----------------------------------------------------------------------
      call output ('ImBLR: '//version)
c
c Get user inputs
c
      call keyini
      call keyf ('in', in, ' ')
      call keyf ('out', out, ' ')
      call keyr ('value', val, 0.0)
      call keyfin
c
      if (in.eq.' ') call bug ('f', 'No input image given')
      if (out.eq.' ') call bug ('f', 'No output image given')
      if (in.eq.out) call bug ('f', 
     +  'Input and output images must be different')
c
c Open input image
c
      call xyopen (lin, in, 'old', maxnax, size)
      call rdhdi (lin, 'naxis', naxis, 0)
c
c Open output image and copy header items to it
c  
      call xyopen (lout, out, 'new', naxis, size)
      call headcopy (lin, lout, 0, naxis, 0, 0)
      call hisopen (lout,'append')
      call hiswrite (lout, 'IMBLR: Miriad '//version)
      call hisinput (lout,'IMBLR')
      call hisclose (lout)
c
c Loop over input image
c
      npix2 = 0.0
      do k = 1, size(3)
        npix = 0.0
        call xysetpl (lin, 1, k)
        call xysetpl (lout, 1, k)
c
        do j = 1, size(2)
          call xyread (lin, j, data)
          call xyflgrd (lin, j, flags)
c
          do i = 1, size(1)
            if (.not.flags(i)) then
              data(i) = val
              npix = npix + 1
            end if
            dmm(1) = min(dmm(1),data(i))
            dmm(2) = max(dmm(2),data(i))
          end do
c
          call xywrite (lout, j, data)
          call xyflgwr (lout, j, gflags)
        end do
c
        if (npix.gt.0.0) then
          write (line, 10) k, npix
10        format ('Plane ', i3, ' : replaced ', f8.0, ' blanks')
          call output (line)  
          npix2 = npix2 + npix
        end if
      end do
c      
      if (npix2.le.0.0) call output ('There were no blanks')
c   
      call wrhdr (lout, 'datamax', dmm(2))
      call wrhdr (lout, 'datamin', dmm(1))
c
      call xyclose (lin)
      call xyclose (lout)
c
      end
