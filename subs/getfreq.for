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
c               1 -> spectral axis and rest freq present, but no 
c                    Vobs. Result may be inaccurate
c               2 -> no spectral axis, 
c               3 -> spectral axis present and is velocity, but no 
c                    rest frequency
c--
c
c History:
c    11nov92  nebk   Original version
c    28nov92  nebk   Doc changes
c    10dec92  nebk   Make pixel real instead of integer, add ierr, ifax
c    17dec92  nebk   Adapt for new fndaxnum
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxdim.h'
c
      character itoaf*1, ctype*8, axnam*1, line*80
      double precision cdelt, crval, vel, f0, vobs
      real crpix
c-----------------------------------------------------------------------
      ierr = 0
c
c Find spectral axis
c
      if (ifax.le.0) then
        axnam = ' '      
        call fndaxnum (tin, 'freq', axnam, ifax)
        if (ifax.eq.0) then
          call bug ('w',  
     +     'GETFREQ: No spectral axis; could not work out frequency')
          freq = 0.0
          finc = 0.0
          ierr = 2
          return
        end if
      end if
c
c Get spectral axis descriptors
c
      call rdhdd (tin, 'crval'//itoaf(ifax), crval, -1.0d0)
      call rdhdd (tin, 'cdelt'//itoaf(ifax), cdelt, 0.0d0)
      call rdhdr (tin, 'crpix'//itoaf(ifax), crpix, 0.0)
      call rdhda (tin, 'ctype'//itoaf(ifax), ctype, ' ')
c
c Find frequency of this channel
c
      call ucase (ctype)
      if (index(ctype,'FREQ').ne.0) then
        freq = (pix-crpix)*cdelt + crval
        finc = cdelt
      else if (index(ctype,'VELO').ne.0 .or.
     +         index(ctype,'FELO').ne.0) then
c
c Velocities are in the radio convention
c
        call rdhdd (tin, 'restfreq', f0, 0.0d0)
        if (f0.le.0.0d0) then
          call bug ('w', 
     +   'GETFREQ: No rest frequency, cannnot convert vel. to freq.')
         freq = 0.0
         ierr = 3
         return
        end if
c
        call rdhdd (tin, 'vobs', vobs, 0.0d0)
        if (vobs.eq.0.0d0) then
          call bug ('w',
     +     'GETFREQ: Vobs is zero; conversion to frequency from')
          call bug ('w', 
     +     'GETFREQ: velocity will be inaccurate; try PUTHD')
          ierr = 1
        end if
c
        vel = 1000 * ( ((pix-crpix)*cdelt + crval) + vobs )
        freq = f0 * (1.0 - vel/dcmks)
        finc = -f0 * 1.0d3 * cdelt / dcmks
      end if
c
      if (freq.le.0.0) then
        write (line, 100) freq
100     format ('GETFREQ: ', f9.5, ' GHz is an unusual frequency')
        call bug ('w', line)
      end if
c
      end
