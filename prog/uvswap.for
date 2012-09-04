	program uvswap
	implicit none

c= uvswap - Relabel polarizations in uvdata.
c& nebk
c: uv analysis
c+
c	UVSWAP relabels RL correlations as LR and LR correlations as RL.
c	XY are relabeled YX and YX correlations as XY.
c	RR, LL, XX and YY  are left unchanged.  The data are copied in their 
c	raw form only, gain, polarization and bandpass tables are 
c	neither applied nor copied.
c
c@ vis
c	Input visibility data file. No default
c@ out
c	Output visibility data file name. No default. 
c@ options
c      xyswap  swap X and Y pols.
c   XX to YY, YY to XX, XY to YX and YX to XY.
c--
c
c  History:
c    nebk 28sep92 Original version cloned from UVMODEL
c    mchw 10apr09 Added XY and YX swap.
c    mchw 16apr09 Added options=xywap
c
c------------------------------------------------------------------------
      include 'maxdim.h'
      character version*(*)
      parameter(version='version 16-Apr-2009')
c
      character vis*64, out*64
      integer nread, npols, pol, tvis, tout
      double precision preamble(4)
      complex data(maxchan)
      logical flags(maxchan), doxyswap
c------------------------------------------------------------------------
c
c Get the input parameters
c
      call output ('UvSwap: '//version)
      call keyini
      call keya ('vis', vis, ' ')
      call keya ('out', out, ' ')
        call GetOpt(doxyswap)
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
c
c swap XY YX
c
        if (pol.eq.-7) then
          pol = -8
        else if (pol.eq.-8) then
          pol = -7
        endif 
c
c swap RL LR
c
        if (pol.eq.-3) then
          pol = -4
        else if (pol.eq.-4) then
          pol = -3
        endif 
c
c swap XX YY
c
       if(doxyswap) then
         if(pol.eq.-6)then
           pol = -5
         else if(pol.eq.-5)then
           pol = -6
         endif
       endif
c
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
c********1*********2*********3*********4*********5*********6*********7*c 
        subroutine GetOpt(doxyswap)
        implicit none
        logical doxyswap
c
c  Get extra processing options.
c
c  Output:
c    doxyswap    xyswap  swap X and Y pols.
c----------------------------------------------------------------------- 
        integer nopt 
        parameter(nopt=1) 
        logical present(nopt) 
        character opts(nopt)*9 
c 
        data opts/'xyswap   '/
c    
        call options('options',opts,present,nopt) 
        doxyswap    = present(1) 
        end
c********1*********2*********3*********4*********5*********6*********7*c 
