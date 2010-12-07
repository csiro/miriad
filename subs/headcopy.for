      subroutine headcp(lIn, lOut, nAxMap, axMap, blc, trc)

      integer lIn, lOut, nAxMap, axMap(*), blc(*), trc(*)
c-----------------------------------------------------------------------
c  HEADCP copies the small items (header keywords) and the history from
c  one image to another.  It transfers all official header keywords with
c  only a few exceptions:
c    - naxis and naxis# which are maintained by XYOPEN and XYZOPEN,
c    - datamin, datamax, and rms must be recalculated for the output
c      image and updated with WRHDR.
c
c  Keywords are copied verbatim except for crpix, crval, cdelt, ctype,
c  and pc which may be deleted, exchanged, or reversed depending on axis
c  permutations.  The axMap array defines the relation between new and
c  old axes in the sense axMap(new) = old, e.g.
c
c                                      axMap
c                        nAxMap  (1)  (2)  (3)  (4)
c                        ------  ---  ---  ---  ---
c            direct copy:   4     1    2    3    4
c          delete z-axis:   2     1    2    0    0
c          delete x-axis:   2     2    3    0    0
c     output is zxy cube:   3     3    1    2    0
c        x-axis reversed:   4    -1    2    3    4
c          straight copy:  any    0    -    -    -
c
c  The last entry shows the shorthand used in the common case where
c  there are no axis permutations or reversals.
c
c  For reversed axes cdelt is multiplied by -1.
c
c  For output images whose corners differ from the input image, crpix
c  may have to be corrected.  This is done using the arrays blc and trc
c  which give the corners of the output image relative to the pixel
c  numbers of the input image.  blc is used for non-reversed axes and
c  trc for reversed axes.  Often one can use the output of subroutine
c  boxinfo to get blc and trc.
c
c  Input:
c    lIn        Handle of input image.
c    lOut       Handle of output image.
c    nAxMap     Dimension of axMap array, and the blc and trc arrays.
c               If zero, a verbatim copy is done (no axis reversals or
c               subimaging - axMap, blc, and trc are ignored).
c    axMap      Array that relates new axes with old (see above).  Can
c               be specified as 0 (scalar) meaning that there are no
c               swapped or reversed axes.
c    blc        List of bottom-left-corners of input image region.
c               Can be specified as 0 (scalar) meaning (1,1,...).
c    trc        List of top-right-corners of input image region.
c               Can be specified as 0 (scalar) meaning
c               (naxis1,naxis2,...) for the input image.
c
c $Id: headcopy.for,v 1.9 2010/12/07 04:56:16 cal103 Exp $
c-----------------------------------------------------------------------
      integer   CRPIX, CDELT, CRVAL, CTYPE
      parameter (CRPIX = 1, CDELT = 2, CRVAL = 3, CTYPE = 4)

      integer   NKEYW
      parameter (NKEYW = 26)

      logical   noPerm, verbtm
      integer   axLen, iAxIn, iAxOut, jAxIn, jAxOut, nAxOut, k, m
      double precision defVal(4), dval
      character cval*80, keyIn*8, keyOut*8, keyw(NKEYW)*8

      logical   hdprsnt
      character itoaf*1
      external  hdprsnt, itoaf

      data defVal /0d0, 1d0, 0d0, 0d0/

c     The following coordinate keywords are handled individually below:
c       crpix, cdelt, crval, ctype, llrot, pc,
c       lonpole, latpole, phi0, theta0, xyzero, pv.
c     The remainder are copied verbatim in the order defined here if
c     present in the input image.
      data keyw /
     *    'bmaj    ', 'bmin    ', 'bpa     ', 'btype   ', 'bunit   ',
     *    'cellscal', 'date-obs', 'epoch   ', 'instrume', 'ltype   ',
     *    'lstart  ', 'lstep   ', 'lwidth  ', 'mostable', 'niters  ',
     *    'object  ', 'observer', 'obsra   ', 'obsdec  ', 'obstime ',
     *    'pbfwhm  ', 'pbtype  ', 'restfreq', 'telescop', 'vobs    ',
     *    'history '/
c-----------------------------------------------------------------------
      call rdhdi(lOut, 'naxis', nAxOut, 0)

c     Copy crpix, cdelt, crval, and ctype for each axis.  All axes in
c     the output image must have these basic coordinate keywords.  
      noPerm = axMap(1).eq.0
      verbtm = nAxMap.eq.0
      do k = 1, 4
c       Set default values.
        cval = ' '
        dval = defVal(k)

        do iAxOut = 1, nAxOut
          keyOut = keyw(k)(1:5) // itoaf(iAxOut)

          if (verbtm .and. hdprsnt(lIn, keyOut)) then
c           Copy verbatim.
            call hdcopy(lIn, lOut, keyOut)

          else
c           Handle axis permutations.
            if (noPerm) then
              iAxIn = iAxOut
            else if (iAxOut.le.nAxMap) then
              iAxIn = axMap(iAxOut)
            else
c             Use default values.
              iAxIn = 0
            endif

            if (iAxIn.ne.0) then
              keyIn = keyw(k)(1:5) // itoaf(abs(iAxIn))

              if (hdprsnt(lIn, keyIn)) then
c               Read it from the input image.
                if (k.eq.CTYPE) then
                  call rdhda(lIn, keyIn, cval, ' ')
                else
                  call rdhdd(lIn, keyIn, dval, 0d0)
                endif

                if (iAxIn.gt.0) then
                  if (k.eq.CRPIX) then
c                   Sub-imaging.
                    if (.not.verbtm .and. blc(1).ne.0) then
                      dval = dval - dble(blc(iAxIn)-1)
                    endif
                  endif

                else if (iAxIn.lt.0) then
c                 Axis reversal.
                  iAxIn = -iAxIn

                  if (k.eq.CRPIX) then
c                   Sub-imaging.
                    if (.not.verbtm .and. trc(1).ne.0) then
                      axLen = trc(iAxIn)
                    else
                      keyIn = 'naxis' // itoaf(iAxIn)
                      call rdhdi(lOut, keyIn, axLen, 0)
                    endif

                    dval = dble(axLen+1) - dval

                  else if (k.eq.CDELT) then
                    dval = -dval
                  endif
                endif
              endif
            endif

c           Write it to the output image.
            if (k.eq.CTYPE) then
              call wrhda(lOut, keyOut, cval)
            else
              call wrhdd(lOut, keyOut, dval)
            endif
          endif
        enddo
      enddo

c     llrot is deprecated in favour of pci_j, only copied if present in
c     the input image and non-zero.
      call rdhdd(lIn, 'llrot', dval, 0d0)
      if (dval.ne.0d0) then
        call wrhdd(lOut, 'llrot', dval)
      endif

c     Copy whatever linear transformation matrix elements are present,
c     with transposition if necessary.
      do iAxOut = 1, nAxOut
        do jAxOut = 1, nAxOut
          keyOut = 'pc' // itoaf(iAxOut) // '_' // itoaf(jAxOut)

          if (verbtm) then
c           Copy verbatim.
            call hdcopy(lIn, lOut, keyOut)

          else
c           Handle axis permutations.
            if (noPerm) then
              iAxIn = iAxOut
              jAxIn = jAxOut
            else if (iAxOut.le.nAxMap .and. jAxOut.le.nAxMap) then
              iAxIn = abs(axMap(iAxOut))
              jAxIn = abs(axMap(jAxOut))
            else
c             Skip it.
              iAxIn = 0
            endif

            if (iAxIn.ne.0) then
              keyIn = 'pc' // itoaf(iAxIn) // '_' // itoaf(jAxIn)

              if (hdprsnt(lIn, keyIn)) then
c               Read it from the input image.
                call rdhdd(lIn, keyIn, dval, 0d0)

c               Write it to the output image.
                call wrhdd(lOut, keyOut, dval)
              endif
            endif
          endif
        enddo
      enddo

c     Parameters related to celestial coordinates.
      call hdcopy(lIn, lOut, 'lonpole')
      call hdcopy(lIn, lOut, 'latpole')
      call hdcopy(lIn, lOut, 'phi0')
      call hdcopy(lIn, lOut, 'theta0')
      call hdcopy(lIn, lOut, 'xyzero')

c     Projection parameters.
      do m = 0, 29
        keyOut = 'pv' // itoaf(m)
        call hdcopy(lIn, lOut, keyOut)
      enddo

c     Copy the remaining items verbatim, if present.
      do k = 1, NKEYW
        call hdcopy(lIn, lOut, keyw(k))
      enddo

      end

c***********************************************************************

c     HEADCOPY is defunct, use HEADCP directly instead.  This stub
c     exists solely to provide backward compatibility.

      subroutine headcopy(lIn, lOut, axMap, nAxMap, blc, trc)
      integer lIn, lOut, axMap(*), nAxMap, blc(*), trc(*)
      call headcp (lIn, lOut, nAxMap, axMap, blc, trc)
      end
