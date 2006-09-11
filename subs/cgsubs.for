c
c* conlevCG -- Compute contour levels
c& nebk
c: plotting
c+
      subroutine conlevcg (mirror, maxlev, lcin, levtyp, slev, nlevs, 
     +                     levs, srtlev)
c
      implicit none
      integer lcin, nlevs, maxlev, srtlev(maxlev)
      real slev, levs(maxlev)
      character*1 levtyp
      logical mirror
c
c  Compute contour levels
c
c  Input:
c    mirror   MUltiply specified contour levsls by -1 and add to list
c    maxlev   Maximum number of levels allowed
c    lcin     Handle of contour image
c  Input/output:
c    levtyp   Type of scale factor (percentage or absolute)
c    slev     Contour scale factor
c    nlevs    Number of contour levels
c  Output:
c    levs     Contour levels
c    srtlev   Indexes of array giving levels in increasing order
c--
c-----------------------------------------------------------------------
      include 'maxnax.h'
      integer i, ilev, mlevs, cnaxis, csize(maxnax)
      real cdmin, cdmax, off, inc
      character*1 itoaf
c-----------------------------------------------------------------------
      call rdhdi (lcin, 'naxis', cnaxis, 0)
      do i = 1, cnaxis
        call rdhdi (lcin, 'naxis'//itoaf(i), csize(i), 0)
      end do
c
      mlevs = nlevs
      if (nlevs.eq.0) then
c
c Set default contours
c
        call imminmax (lcin, cnaxis, csize, cdmin, cdmax)
c
        if (cdmax.gt.0.0 .and. cdmin.lt.0.0) then
           slev = max(abs(cdmax), abs(cdmin)) / 8
c
           nlevs = abs(cdmin) / slev
           ilev = 1
           do i = -nlevs, -1, 1
             levs(ilev) = i * slev
             ilev = ilev + 1
           end do 
c
           nlevs = cdmax / slev
           do i = 1, nlevs, 1
             levs(ilev) = i * slev
             ilev = ilev + 1
           end do          
c 
           nlevs = ilev - 1
           slev = 1.0
           levtyp = 'a'
        else
           off = 0.05 * (cdmax - cdmin)
           nlevs = 10
           inc = ((cdmax-off) - (cdmin+off)) / (nlevs - 1)
           do i = 1, nlevs
              levs(i) = cdmin+off + (i-1)*inc
           end do
c
           slev = 1.0
           levtyp = 'a'
        end if
      else if (levtyp.eq.'p')  then
c
c Set percentage contours
c
        if (slev.eq.0.0) slev = 1.0
        call imminmax (lcin, cnaxis, csize, cdmin, cdmax)
        slev = slev * cdmax / 100.0
      else if (levtyp.eq.'a') then
c
c Absolute contours
c
        if (slev.eq.0.0) slev = 1.0
      end if
c
c Set mirrored contours only for user specified contours
c
      if (mirror .and. mlevs.ne.0) then
        mlevs = nlevs
        do i = 1, mlevs
          if (levs(i).ne.0.0) then
            if (nlevs.lt.maxlev) then
              nlevs = nlevs + 1
              levs(nlevs) = -1.0*levs(i)
            else
              call bug ('w',
     +        'CONLEVCG: Max. no. of contours reached during mirroring')
              goto 100
            end if
          end if
        end do
      end if
c
c Scale levels
c
100   do i = 1, nlevs
        levs(i) = levs(i) * slev
      end do
c 
c Sort in increasing order
c
      call sortidxr (nlevs, levs, srtlev)
c      
      end
