c*GetFreq -- Return frequency & increment for given image spectral pixel
c& nebk
c: utilities
c+
        subroutine getfreq (tin, pix, ifax, freq, finc, ierr)

        integer   tin, ifax, ierr
        real      pix
        double precision freq, finc
c  ---------------------------------------------------------------------
c  Work out the frequency of a spectral pixel, whether the spectral
c  axis is frequency or velocity, and regardless of which axis
c  is the spectral axis.  Works for images only.
c
c  Inputs:
c    tin    i   Handle of dataset
c    pix    r   Pixel of interest
c  Input/output
c    ifax   i   The spectral axis number.  If 0 on input, GETFREQ
c               will look for it.
c  Output:
c    freq   dp  Frequency in GHz.
c    finc   dp  Frequency increment in GHz.
c    ierr   i   0 -> OK,
c               1 -> no spectral axis,
c History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c $Id: getfreq.for,v 1.2 2011/02/14 06:27:09 cal103 Exp $
c-----------------------------------------------------------------------
      double precision freq1, freq2
      character line*80
c-----------------------------------------------------------------------
c     Initialize
      call coinit(tin)
      ierr = 0

c     Find spectral axis
      if (ifax.le.0) then
        call cofindax(tin, 'spectral', ifax)
        if (ifax.eq.0) then
          call cofin(tin)
          call bug('w',
     *     'GETFREQ: No spectral axis; could not work out frequency')
          freq = 0.0
          finc = 0.0
          ierr = 1
          return
        endif
      endif

c     Set frequency axis and load conversion arrays
      call covelset(tin, 'frequency')

      call cocvt1(tin, ifax, 'ap', dble(pix-0.5), 'aw', freq1)
      call cocvt1(tin, ifax, 'ap', dble(pix+0.5), 'aw', freq2)
      call cocvt1(tin, ifax, 'ap', dble(pix),     'aw', freq)
      call cofin(tin)

      finc = freq2 - freq1

      if (freq.le.0.0) then
        write(line, 100) freq
100     format('GETFREQ: ', f9.5, ' GHz is an unusual frequency')
        call bug('w', line)
      endif

      end
