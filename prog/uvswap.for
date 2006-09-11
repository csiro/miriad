	program uvswap
	implicit none

c= uvswap - Relabel correlations RL as LR and LR as RL
c& nebk
c: uv analysis
c+
c	UVSWAP relabels RL correlations as LR and LR correlations as RL.
c	RR and LL are left unchanged.  The data are copied in their 
c	raw form only, gain, polarization and bandpass tables are 
c	neither applied nor copied.
c
c@ vis
c	Input visibility data file. No default
c@ out
c	Output visibility data file name. No default. 
c--
c
c  History:
c    nebk 28sep92 Original version cloned from UVMODEL
c
c------------------------------------------------------------------------
      include 'maxdim.h'
      character version*(*)
      parameter(version='version 28-Sep-92')
c
      character vis*64, out*64
      integer nread, npols, pol, tvis, tout
      double precision preamble(4)
      complex data(maxchan)
      logical flags(maxchan)
c------------------------------------------------------------------------
c
c Get the input parameters
c
      call output ('UvSwap: '//version)
      call keyini
      call keya ('vis', vis, ' ')
      call keya ('out', out, ' ')
      call keyfin
c
c Check the input parameters
c
      if (vis.eq.' ' .or. out.eq.' ') call bug ('f',
     +    'Input and output must both be given') 
c
c Open sesame
c
      call uvopen (tvis, vis, 'old')
      call uvopen (tout, out, 'new')
      call varinit (tvis, 'channel')
      call varonit (tvis, tout, 'channel')
c
c Perform the copying.
c
      call uvread (tvis, preamble, data, flags, maxchan, nread)
      do while (nread.gt.0) 
        call uvgetvri (tvis, 'npol', npols, 1)
        call uvgetvri (tvis, 'pol', pol, 1)
c 
        call varcopy (tvis, tout)
        if (pol.eq.-3) then
          pol = -4
        else if (pol.eq.-4) then
          pol = -3
        end if 
        call uvputvri (tout, 'npol', npols, 1)
        call uvputvri (tout, 'pol', pol, 1)
        call uvwrite (tout, preamble, data, flags, nread)
c
        call uvread (tvis, preamble, data, flags, maxchan, nread)
      end do
c
c  Make the history of the output and close up shop.
c
      call hdcopy (tvis, tout, 'history')
      call hisopen (tout, 'append')
      call hiswrite (tout, 'UVSWAP: Miriad UvSwap '//version)
      call hisinput (tout, 'UVSWAP')
      call hisclose (tout)
c
      call uvclose (tvis)
      call uvclose (tout)
c
      end
