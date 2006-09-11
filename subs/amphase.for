c************************************************************************
c*AmPhase -- Compute amplitude and phase.
c:complex-data,uv-data
c+
      subroutine amphase (data, amp, phase)
      implicit none
c
      complex data
      real amp, phase
c
c  Compute amplitude and phase from one complex correlation
c
c  Input:
c    data	The complex data.
c  Output:
c    amp	The amplitude.
c    phase	The phase (degrees).
c--
c  History:
c    wh?  xxxxxxx Original version.
c    rjs  21oct92 Use abs() to avoid floating overflow for large numbers.
c-------------------------------------------------------------------------
      real pi, temp
      parameter (pi = 3.141592653589793)
c
      temp = abs(data)
      if (temp.gt.0) then
        amp = temp
        phase = 180.0 / pi * atan2(aimag(data),real(data))
      else
        amp = 0.0
        phase = 0.0
      end if
c
      end
