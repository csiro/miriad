c************************************************************************
c* headcopy - Copy image header
c& bpw
c: utilities, image-i/o
c+
      subroutine headcopy(tnoinp, tnoout, axnum, naxis, blc, trc)

      integer tnoinp, tnoout, axnum(*), naxis, blc(*), trc(*)

c Headcopy copies the small items (the header) from one image to
c another.  Its list contains all official header elements except for
c the four described below.
c
c The items listed in the KEYW array (below) are copied verbatim except
c for crpix, crval, cdelt, crota and ctype which may be deleted,
c exchanged, or reversed depending on axis permutations.  The axnum
c array defines the relation between old and new axes, e.g.
c
c                                     axnum
c                        naxis  (1)  (2)  (3)  (4)
c                        -----  ---  ---  ---  ---
c           direct copy:   4     1    2    3    4
c         delete z-axis:   2     1    2    0    0
c         delete x-axis:   2     2    3    0    0
c    output is zxy cube:   3     3    1    2    0
c       x-axis reversed:   4    -1    2    3    4
c         verbatim copy:  any    0    -    -    -
c
c   The last entry shows the shorthand used in the common case where
c   there are no axis permutations and the input and output images have
c   the same dimensions (no sub-imaging).
c
c   For reversed axes cdelt is multiplied by -1 and 180 added to crota.
c
c   For output images whose corners differ from the input image, crpix
c   may have to be corrected.  This is done using the arrays blc and trc
c   which give the corners of the output image relative to the pixel
c   numbers of the input image.  blc is used for non-reversed axes and
c   trc for reversed axes.  Often one can use the output of subroutine
c   boxinfo to get blc and trc.
c
c As xyopen and xyzopen maintain the naxis and naxis# items, they are
c not copied here.  datamin, datamax, and rms are also not copied but
c must be recalculated explicitly for the output image and updated with
c wrhdr.
c
c   Input:
c      tnoinp     handle of input image
c      tnoout     handle of output image
c      axnum      array giving relation betweem old and new axes
c                 (negative values imply axis reversal)
c      naxis      dimension of input image
c      blc        list of bottom-left-corners of input image region
c      trc        list of top-right-corners of input image region
c
c $Id: headcopy.for,v 1.3 2010/08/19 08:33:58 cal103 Exp $
c***********************************************************************
      integer   CRPIX, CDELT, CRVAL, CROTA, CTYPE
      parameter (CRPIX = 1, CDELT = 2, CRVAL = 3, CROTA = 4, CTYPE = 5)

      integer   NKEYS
      parameter (NKEYS = 32)

      logical   hdprsnt
      integer   n, k
      double precision dvalue
      character avalue*80, keyIn*8, keyOut*8, keyw(NKEYS)*8

      character itoaf*1
      external  itoaf

      data keyw /
     *    'crpix   ', 'cdelt   ', 'crval   ', 'crota   ', 'ctype   ',
     *    'bmaj    ', 'bmin    ', 'bpa     ', 'btype   ', 'bunit   ',
     *    'cellscal', 'date-obs', 'epoch   ', 'instrume', 'llrot   ',
     *    'ltype   ', 'lstart  ', 'lstep   ', 'lwidth  ', 'mostable',
     *    'niters  ', 'object  ', 'observer', 'obsra   ', 'obsdec  ',
     *    'obstime ', 'pbfwhm  ', 'pbtype  ', 'restfreq', 'telescop',
     *    'vobs    ', 'history '/
c-----------------------------------------------------------------------
c     Handle crpix, crval, cdelt, crota, and ctype.
      do k = 1, 5
        do n = 1, naxis
          keyOut = keyw(k)(1:5) // itoaf(n)

          if (axnum(1).eq.0) then
c           Verbatim copy.
            call hdcopy(tnoinp, tnoout, keyOut)
          else
c           Handle axis permutations.
            if (axnum(n).ne.0) then
              keyIn = keyw(k)(1:5) // itoaf(abs(axnum(n)))

              if (hdprsnt(tnoinp, keyIn)) then
c               Read it from the input image.
                if (k.eq.CTYPE) then
                  call rdhda(tnoinp, keyIn, avalue, ' ')
                else
                  call rdhdd(tnoinp, keyIn, dvalue, 0d0)
                endif

                if (axnum(n).gt.0) then
                  if (k.eq.CRPIX) then
c                   Sub-imaging.
                    dvalue = dvalue - blc(axnum(n)) + 1d0
                  endif

                else if (axnum(n).lt.0) then
c                 Axis reversal.
                  if (k.eq.CRPIX) then
c                   Sub-imaging.
                    dvalue = trc(-axnum(n)) - dvalue + 1d0
                  else if (k.eq.CDELT) then
                    dvalue = -dvalue
                  else if (k.eq.CROTA) then
                    dvalue = dvalue - 180d0
                  endif
                endif

c               Write it to the output image.
                if (k.eq.CTYPE) then
                  call wrhda(tnoout, keyOut, avalue)
                else
                  call wrhdd(tnoout, keyOut, dvalue)
                endif
              endif
            endif
          endif
        enddo
      enddo

c     Copy the remaining items verbatim.
      do k = 6, NKEYS
         call hdcopy(tnoinp, tnoout, keyw(k))
      enddo

      return
      end
