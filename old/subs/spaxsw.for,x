c
c* spaxsw -- Switch (freq/velo) the spectral axis descriptors of an image
c& nebk/rjs
c: utilities
c+
c
      subroutine spaxsw (lh, switch, ctype, cdelt, crval)
      implicit none
c
      integer lh
      double precision cdelt, crval
      character*(*) switch, ctype
c
c   Switch the reference value, pixel increment and axis type for the
c   spectral axis of image to those describing the axis in one of 
c   frequency or velocity (radio or optical definition)
c  
c  Input
c    lh         Input handle
c    switch     The requested type of the spectral axis.  Choose from
c                 ' '        meaning  freq    -> radio
c                                     radio   -> frequency
c                                     optical -> frequency
c                 'radio'    meaning          -> radio
c                 'optical'  meaning          -> optical
c  Input/output
c    ctype      Axis type. Should start with one of FREQ, FELO (optical 
c               velocity and  VELO (radio velocity)
c    cdelt      Pixel increment (GHz or km/s)
c    crval      Reference value (GHs or km/s)
c--
c
c  History
c    nebk/rjs  01jul94   Created by stripping out guts of task velsw
c-----------------------------------------------------------------------
      include 'mirconst.h'
      double precision c
      parameter(c=0.001*cmks)
c
      double precision f, df, restfreq, vobs
      character insw*9, frame*3
      logical hdprsnt, obsvprs
c-----------------------------------------------------------------------
c
c Check that this is a spectral axis
c
      if (ctype(1:4).eq.'FELO')then
        insw = 'optical'
      else if(ctype(1:4).eq.'VELO')then
        insw = 'radio'
      else if(ctype(1:4).eq.'FREQ')then
        insw = 'frequency'
      else
        call bug('f','SPAXSW: this is not a spectral axis')
      endif
c
c Save the reference frame
c
      if (ctype(5:5).eq.'-')then
        frame = ctype(6:8)
      else
        frame = '???'
      end if
c
c  Determine what spectral type to switch to if it is not given
c
      if (switch.eq.' ')then
        if(insw.eq.'radio' .or. insw.eq.'optical') then
          switch = 'frequency'
        else
          switch = 'radio'
        end if
      end if
c
c Go home if nothing to do
c
      if (switch.eq.insw) return
c
c Get the rest frequency and observatory's doppler velocity
c
      call rdhdd (lh, 'restfreq', restfreq, 0.d0)
      if (restfreq.le.0.0d0) call bug ('f', 'Rest frequency is missing')
c
      obsvprs = hdprsnt (lh, 'vobs')
      if (obsvprs) then
        call rdhdd (lh, 'vobs', vobs, 0.d0)
      else
        vobs = 0.0
        call bug ('w', 
     +     'Observatory Doppler velocity item missing; assumed 0')
      end if
c
c Determine the frequency of the reference pixel
c
      if (insw.eq.'optical') then
        f = restfreq / (1+(crval+vobs)/c)
        df = -(cdelt/c) * f * (f/restfreq)
      else if (insw.eq.'radio') then
        f = restfreq * (1-(crval+vobs)/c)
        df = -(cdelt/c) * restfreq
      else
        f = crval
        df = cdelt
      end if
c
c Perform the transformation
c
      if (switch.eq.'optical') then
        crval =  c*(restfreq/f-1) - vobs
        cdelt = -c*(df/f)*(restfreq/f)
        ctype = 'FELO-'//frame
      else if (switch.eq.'radio') then
        crval =  c*(1-f/restfreq) - vobs
        cdelt = -c*(df/restfreq)
        ctype = 'VELO-'//frame
      else if (switch.eq.'frequency') then
        crval = f
        cdelt = df
        ctype = 'FREQ-'//frame
      endif
c
      end
