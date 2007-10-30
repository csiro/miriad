c*GetFreq -- Return frequency & increment for given image spectral pixel
c& nebk
c: utilities
c+
        subroutine getfreq (tin, pix, ifax, freq, finc, ierr)
c
        implicit none
        real pix
        integer tin, ierr, ifax
        double precision freq, finc
c
c  Work out the frequency of a spectral pixel, whether the spectral
c  axis is frequency or velocity, and regardless of which axis
c  is the spectral axis.   Works for images only.
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
c--
c
c History:
c    11nov92  nebk   Original version
c    28nov92  nebk   Doc changes
c    10dec92  nebk   Make pixel real instead of integer, add ierr, ifax
c    17dec92  nebk   Adapt for new fndaxnum
c    22aug94  nebk   Adapt for new COCVT coordinate routines
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxnax.h'
c
      integer ip, i
      double precision win(maxnax), wout(maxnax), freq2
      character line*80
      character cti*50, cto*50
c-----------------------------------------------------------------------
c
c Initialize
c
      call coinit (tin)
      ierr = 0
c
c Find spectral axis
c
      if (ifax.le.0) then
        call cofindax (tin, 'spectral', ifax)        
        if (ifax.eq.0) then
          call bug ('w',  
     +     'GETFREQ: No spectral axis; could not work out frequency')
          freq = 0.0
          finc = 0.0
          ierr = 1
          return
        end if
      end if
c
c Set frequency axis and load conversion arrays
c
      call covelset (tin, 'frequency')
      ip = 1
      do i = 1, maxnax
        win(i) = 0.0
        cti(ip:ip+2) = 'ap/'
        cto(ip:ip+2) = 'aw/'
        ip = ip + 3
      end do
c
c Work out frequency increment
c
      win(ifax) = pix - 0.5
      call cocvt (tin, cti, win, cto, wout)
      freq = wout(ifax)
      win(ifax) = pix + 0.5
      call cocvt (tin, cti, win, cto, wout)
      freq2 = wout(ifax)
      finc = freq2 - freq
c
c Work out frequency
c
      win(ifax) = pix      
      call cocvt (tin, cti, win, cto, wout)
      freq = wout(ifax)
c
      if (freq.le.0.0) then
        write (line, 100) freq
100     format ('GETFREQ: ', f9.5, ' GHz is an unusual frequency')
        call bug ('w', line)
      end if
c
      end
