c*GetFreq -- Return frequency & increment for given image spectral pixel
c& nebk
c: utilities
c+
        subroutine getfreq (tin, pix, ifrq, freq, finc, ierr)

        integer   tin, ifrq, ierr
        real      pix
        double precision freq, finc
c  ---------------------------------------------------------------------
c  Return the frequency of a spectral pixel, regardless of spectral axis
c  type, or which axis is the spectral axis.  Works for images only.
c
c  Inputs:
c    tin    i   Handle of dataset
c    pix    r   Pixel of interest
c
c  Output:
c    ifrq   i   The spectral axis number.
c    freq   d   Frequency in GHz.
c    finc   d   Frequency increment in GHz.
c    ierr   i   0 -> OK,
c               1 -> no spectral axis,
c History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c $Id: getfreq.for,v 1.4 2011/10/06 07:10:02 cal103 Exp $
c-----------------------------------------------------------------------
      double precision freq1, freq2
      character algo*3, line*80
c-----------------------------------------------------------------------
c     Initialize the coordinate object.
      call coInit(tin)

c     Switch spectral axis to frequency.
      call coSpcSet(tin, 'FREQ', ' ', ifrq, algo)
      if (ifrq.eq.0) then
        call coFin(tin)
        call bug('w',
     *   'GETFREQ: No spectral axis; could not work out frequency')
        freq = 0d0
        finc = 0d0
        ierr = 1
        return
      endif

c     Compute the frequency.
      call coCvt1(tin, ifrq, 'ap', dble(pix), 'aw', freq)
      if (freq.le.0d0) then
        write(line, 10) freq
 10     format('GETFREQ: ',f9.5,' GHz is an unusual frequency')
        call bug('w', line)
      endif

c     Compute the frequency increment.
      call coCvt1(tin, ifrq, 'ap', dble(pix-0.5), 'aw', freq1)
      call coCvt1(tin, ifrq, 'ap', dble(pix+0.5), 'aw', freq2)
      finc = freq2 - freq1

      call coFin(tin)
      ierr = 0

      end
