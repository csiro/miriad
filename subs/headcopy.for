c************************************************************************
c* headcopy - Copy image header
c& bpw
c: utilities, image-i/o
c+
      subroutine headcopy(tIn, tOut, axnum, naxnum, blc, trc)

      integer tIn, tOut, axnum(*), naxnum, blc(*), trc(*)

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
c                       naxnum  (1)  (2)  (3)  (4)
c                       ------  ---  ---  ---  ---
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
c      tIn        Handle of input image.
c      tOut       Handle of output image.
c      axnum      Array giving relation betweem old and new axes
c                 (negative values imply axis reversal).
c      naxnum     Dimension of axnum array - ignored if axnum(1) = 0.
c      blc        List of bottom-left-corners of input image region.
c      trc        List of top-right-corners   of input image region.
c
c $Id: headcopy.for,v 1.4 2010/08/20 07:32:02 cal103 Exp $
c***********************************************************************
      integer   CRPIX, CDELT, CRVAL, CROTA, CTYPE
      parameter (CRPIX = 1, CDELT = 2, CRVAL = 3, CROTA = 4, CTYPE = 5)

      integer   NKEYS
      parameter (NKEYS = 32)

      logical   verbtm
      integer   iAxIn, iAxOut, nAxOut, k
      double precision defVal(5), dvalue
      character avalue*80, keyIn*8, keyOut*8, keyw(NKEYS)*8

      logical   hdprsnt
      character itoaf*1
      external  hdprsnt, itoaf

      data defVal /0d0, 1d0, 0d0, 0d0, 0d0/
      data keyw /
     *    'crpix   ', 'cdelt   ', 'crval   ', 'crota   ', 'ctype   ',
     *    'bmaj    ', 'bmin    ', 'bpa     ', 'btype   ', 'bunit   ',
     *    'cellscal', 'date-obs', 'epoch   ', 'instrume', 'llrot   ',
     *    'ltype   ', 'lstart  ', 'lstep   ', 'lwidth  ', 'mostable',
     *    'niters  ', 'object  ', 'observer', 'obsra   ', 'obsdec  ',
     *    'obstime ', 'pbfwhm  ', 'pbtype  ', 'restfreq', 'telescop',
     *    'vobs    ', 'history '/
c-----------------------------------------------------------------------
c     All axes in the output image must have coordinate keywords.  
      call rdhdi(tOut, 'naxis', nAxOut, 0)

c     Loop for crpix, crval, cdelt, crota, and ctype.  
      verbtm = axnum(1).eq.0
      do k = 1, 5
c       Set default values.
        avalue = ' '
        dvalue = defVal(k)

        do iAxOut = 1, nAxOut
          keyOut = keyw(k)(1:5) // itoaf(iAxOut)

          if (verbtm .and. hdprsnt(tIn, keyOut)) then
c           Copy verbatim.
            call hdcopy(tIn, tOut, keyOut)

          else
c           Handle axis permutations.
            if (.not.verbtm .and. iAxOut.le.naxnum) then
              iAxIn = axnum(iAxOut)
            else
c             Use default values.
              iAxIn = 0
            endif

            if (iAxIn.ne.0) then
              keyIn = keyw(k)(1:5) // itoaf(abs(iAxIn))

              if (hdprsnt(tIn, keyIn)) then
c               Read it from the input image.
                if (k.eq.CTYPE) then
                  call rdhda(tIn, keyIn, avalue, ' ')
                else
                  call rdhdd(tIn, keyIn, dvalue, 0d0)
                endif

                if (iAxIn.gt.0) then
                  if (k.eq.CRPIX) then
c                   Sub-imaging.
                    dvalue = dvalue - blc(iAxIn) + 1d0
                  endif

                else if (iAxIn.lt.0) then
c                 Axis reversal.
                  if (k.eq.CRPIX) then
c                   Sub-imaging.
                    dvalue = trc(-iAxIn) - dvalue + 1d0
                  else if (k.eq.CDELT) then
                    dvalue = -dvalue
                  else if (k.eq.CROTA) then
                    dvalue = dvalue - 180d0
                  endif
                endif
              endif
            endif

c           Write it to the output image.
            if (k.eq.CTYPE) then
              call wrhda(tOut, keyOut, avalue)
            else
              call wrhdd(tOut, keyOut, dvalue)
            endif
          endif
        enddo
      enddo

c     Copy the remaining items verbatim.
      do k = 6, NKEYS
         call hdcopy(tIn, tOut, keyw(k))
      enddo

      return
      end
