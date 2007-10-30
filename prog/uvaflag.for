      program uvaflag
c--------------------------------------------------------------------------
c     UVAFLAG is a MIRIAD program to compare two UV data bases, visibility 
c     by visibility and automatically flag the second data base whenever 
c     the first is flagged.   The data bases must be in the same order, 
c     and have identical numbers of visibilities and channels.  
c     This program may be useful when forming I and V with FITS, flagging 
c     on I and then applying the flags to V.
c
c= uvaflag - Use flags in one uv database to set flags in another
c& bpw
c: calibration
c+
c	UVAFLAG is a MIRIAD task which flags correlations in one 
c	database when they are flagged in a template data base.
c	Data bases must be identical except for the values of the
c	correlations and the initial flag mask. 
c@ tvis
c	The template input visibility file. No default.
c@ vis
c	The input visibility file to be flagged. No default.
c--
c
c     nebk 25may89 Original program
c     pjt   2may90 included maxchan through maxdim.h
c     bpw  28jan91 include standard keyword vis
c     rjs   8mar93 Standardise history writing.
c---------------------------------------------------------------------------
      implicit none
      character version*(*)
      parameter(version='Uvaflag: version 1.0 8-Mar-93')
c
      include 'maxdim.h'
c
      complex data(maxchan)
      double precision pream1(4), pream2(4)
      integer lin1, lin2, i, nread1, nread2, nchan
      character in1*40, in2*40
      logical flags1(maxchan), flags2(maxchan), loop
c-----------------------------------------------------------------------
      call output(version)
c
c Get inputs
c
      call keyini
c
      call keya ('tvis', in1, ' ')
      if (in1.eq.' ') call bug ('f', 'Template file name not given')
      call keya ('vis', in2, ' ')
      if (in2.eq.' ') call bug ('f', 'Visibility file name not given')
c
      call keyfin
c 
c Open files
c
      call uvopen (lin1, in1, 'old')
      call uvopen (lin2, in2, 'old')
c
c Loop over visibilities and set flags
c
      loop = .true.
      do while (loop)
        call uvread (lin1, pream1, data, flags1, maxchan, nread1)
        call uvread (lin2, pream2, data, flags2, maxchan, nread2)
        if (nread1.ne.nread2) call bug('f',
     *      'Data bases have different numbers of channels')
        if (nread1.eq.0) then
          loop = .false.
        else
          if (pream1(1).ne.pream2(1) .or. 
     *        pream1(2).ne.pream2(2) .or.
     *        pream1(3).ne.pream2(3) .or.
     *        pream1(4).ne.pream2(4) )  
     *      call bug ('f', 'Data base preambles are different')
c
          nchan = min(nread1, maxchan)
          do i = 1, nchan
            if (.not.flags1(i)) flags2(i) = .false.
          end do
          call uvflgwr (lin2, flags2)
        end if
      end do
c
c Write history and close up
c
      call hisopen (lin2, 'append')
      call hiswrite (lin2, 'UVAFLAG: Miriad '//version)
      call hisinput (lin2,'UVAFLAG')
      call hisclose (lin2)
      call uvclose (lin1)
      call uvclose (lin2)
c
      end
