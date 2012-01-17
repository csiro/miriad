      program uvpflag
c--------------------------------------------------------------------------
c     UVPFLAG will flag all correlations of specified polarizations
c     when ALL given template polarizations are flagged.  
c
c= uvpflag - Flag correlations of certain polarizations when template 
c	     polarizations are flagged.
c& nebk
c: calibration
c+
c     UVPFLAG will flag all correlations of specified polarizations
c     when ALL given template polarizations are flagged.   This can be
c     useful, for example, if you run TVFLAG, and flag data based upon 
c     the XX correlation.  You then use UVPFLAG and flag all other 
c     correlations of the same baseline and time when the XX correlations 
c     are flagged. 
c
c@ vis
c	Input visibility file. No default.
c@ polt
c	Template polarizations.  No default. Can be upto 4 polarizations
c	separated by commas from the following list:
c		xx, yy, xy, yx, rr, ll, rl, lr, i, q, u, v
c@ pols
c	Slave polarizations to flag when ALL template polarizations
c	are bad.  No default. Choose up to 4 from the list given 
c	for keyword `polt'.
c@ options
c	Task enrichment options.  Minimum match is active.
c
c	or      By default, UVPFLAG flags the slaves when ALL
c		of the templates are flagged.  This option
c		directs it to flag the slaves when ANY one of
c		the templates are flagged.
c	full    Instructs UVPFLAG to tell you about every record
c	        it flags.  The default is a summary at the end.
c--
c
c     nebk 25oct92 Original version
c     nebk 27feb93 Add options=or.
c     rjs   8mar93 Write out history comments.
c     rjs  18mar93 Fix horrid bug in adding history comments.
c     nebk 12jan94 More information for user
c     nebk 30mar94 Give user correct information
c     nebk 03oct03 Rename function count to countFlags to avoid compiler warning
c---------------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      integer maxpol
      parameter (maxpol = 4)
      character version*(*)
      parameter (version='UvPFlag: Version 30-Mar-94')
c
      complex data(maxchan)
      double precision pream(4), datum
      real scratch(maxchan+3)
      integer lin, lins, npol, i, j, nread, polidx, nscr, nrec, irec,
     +  ntpol, nspol, nflag(-8:4), ntot(-8:4), nftot(-8:4),
     +  found(-8:4), icnt
      ptrdiff offset
      character in*40, tpol(4)*2, spol(4)*2
      logical gflags(maxchan,maxpol), agflags(maxchan), psel, some,
     +  wantpi(-8:4), wantpo(-8:4), none, ini, brief, doand
      character*2 polist(-8:4)*2, aline*100
c
      data polist /'yx', 'xy', 'yy', 'xx', 'lr', 'rl', 'll', 'rr', ' ',
     +             'i', 'q', 'u', 'v'/
      data nflag, ntot, nftot, icnt /13*0, 13*0, 13*0, 0/
      data found /13*0/
c-----------------------------------------------------------------------
      call output(version)
c
c Get inputs
c
      call keyini
c
      call keya ('vis', in, ' ')
      if (in.eq.' ') call bug ('f', 'Visibility file name not given')
      call mkeya ('polt', tpol, 4, ntpol)
      if (ntpol.eq.0) call bug ('f', 'No template polarizations given')
      call mkeya ('pols', spol, 4, nspol)
      if (nspol.eq.0) call bug ('f', 'No slave polarizations given')
      call getopt (brief, doand)
      call keyfin
c
      do i = 1, ntpol
        call lcase(tpol(i))
      end do
      do i = 1, nspol
        call lcase(spol(i))
      end do
c
c Find template polarizations
c
      none = .true.
      do i = -8, 4
        wantpi(i) = .false.
        do j = 1, ntpol
          if (i.ne.0 .and. polist(i).eq.tpol(j)) wantpi(i) = .true.
        end do
        if (wantpi(i)) none = .false.
      end do
      if (none) call bug ('f',
     +    'Could not match specified template polarizations')
c
c Find slave polarizations 
c
      none = .true.
      do i = -8, 4
        wantpo(i) = .false.
        do j = 1, nspol
          if (i.ne.0 .and. polist(i).eq.spol(j)) wantpo(i) = .true.
        end do
        if (wantpo(i)) none = .false.
      end do
      if (none) call bug ('f',
     +    'Could not match specified slave polarizations')
c
c Open files
c
      call uvopen (lin, in, 'old')
      call scropen (lins)
c
c Loop over visibilities and write scratch file containing visibility 
c number of first record in collection of polarizations, the number
c of polarizations, the number of channels and the ANDed flags
c
      call uvread (lin, pream, data, gflags(1,1), maxchan, nread)
      icnt = icnt + 1
      if (nread.eq.0) call bug ('f', 'No data in this file')
      offset = 0
      nscr = 0
c
      do while (nread.gt.0)
c
c Extract info from first visibility in the group of polarizations
c
        call uvrdvri (lin, 'npol', npol, 0)
        if (npol.eq.0) call bug ('f', 'No polarizations in this file')
c
        call uvrdvri (lin, 'pol', polidx, -10)
        if (polidx.lt.-8 .or. polidx.gt.4) call bug ('f',
     +    'Illegal polarization in file')
        found(polidx) = 1
c
        call uvinfo (lin, 'visno', datum)
        nrec = nint(datum)
c
c Get the flags if we want this polarization
c
        psel  = .false.
        ini = .false.
        if (wantpi(polidx)) then
          psel = .true.
          do j = 1, nread
            agflags(j) = gflags(j,1)
          end do
          ini = .true.
        end if
c
c Read rest of this group of polarizations
c
        do i = 2, npol
          call uvread (lin, pream, data, gflags(1,i), maxchan, nread)
          if (nread.eq.0) 
     +      call bug ('f', 'Unexpected end of file in pass 1')
          icnt = icnt + 1
c
          call uvrdvri (lin, 'pol', polidx, -10)
          if (polidx.lt.-8 .or. polidx.gt.4) call bug ('f',
     +      'Illegal polarization in file')
          found(polidx) = 1
c
c If ALL of the template polarizations are flagged (flag=.false.)
c for a channel set the flagging mask to .false.
c
          if (wantpi(polidx)) then
            psel = .true.
            if (.not.ini) then
              do j = 1, nread
                agflags(j) = gflags(j,i)
              end do
              ini = .true.
            else
              if (doand) then
c
c AND the templates
c
                do j = 1, nread
                  agflags(j) = 
     +              .not.(.not.agflags(j) .and. .not.gflags(j,i))
                end do
              else
c
c OR the templates
c
                do j = 1, nread
                  agflags(j) = 
     +              .not.(.not.agflags(j) .or. .not.gflags(j,i)) 
                end do
              end if
            end if
          end if
        end do
c
c Are any of the AND/ORed channels actually bad ; .false. = -1 = yes.
c Convert from logical to real for scratch file.
c
        if (psel) then
          some = .false.
          do j = 1, nread
            scratch(j+3) = 1.0
            if (.not.agflags(j)) then
              some = .true.
              scratch(j+3) = -1.0
            end if
          end do
        end if
c
c Write out scratch record if there were some flags found
c in this group of polarizations
c
        if (some) then
          scratch(1) = nrec
          scratch(2) = npol
          scratch(3) = nread
          call scrwrite (lins, scratch, offset, nread+3)
          offset = offset + nread + 3
          nscr = nscr + 1
        end if
c
c Get another visibility
c
        call uvread (lin, pream, data, gflags(1,1), maxchan, nread)
        icnt = icnt + 1
      end do
      if (nscr.eq.0) call bug ('f', 
     +  'There were no selected (flagged) template records')
c
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c
c Now read through data again, this time flagging the specified 
c polarizations when there are template correlations stored in
c the scratch file
c
      call uvrewind (lin)
      offset = 0
      irec = 0
c
      do i = 1, nscr
c
c Extract number of first visibility in the next wanted group of
c polarizations, the number of polarizations, channels and the flags
c
        call scrread (lins, scratch, offset, 3)
        nrec = nint(scratch(1))
        npol = nint(scratch(2))
        nread = nint(scratch(3))
        offset = offset + 3
        call scrread (lins, scratch(4), offset, nread)
        offset = offset + nread
c
c Read upto visibility before starting visbility in next group
c
        do while (irec.lt.nrec-1)
          call uvread (lin, pream, data, gflags, maxchan, nread)
          if (nread.eq.0) call bug ('f', 
     +      'Unexpected end of visibility file in pass 2')
c
          call uvinfo (lin, 'visno', datum)
          irec = nint(datum)
c
          call uvrdvri (lin, 'pol', polidx, -10)
          if (polidx.lt.-8 .or. polidx.gt.4) call bug ('f',
     +      'Illegal polarization in file')
c
          call countFlags (maxchan, nread, gflags, ntot(polidx), 
     +                     nftot(polidx))
        end do
c
c Now read/flag the NPOL polarizations
c
        do j = 1, npol
          call uvread (lin, pream, data, gflags, maxchan, nread)
          if (nread.eq.0) call bug ('f', 
     +      'Unexpected end of visibility file in pass 2')
c
          call uvinfo (lin, 'visno', datum)
          irec = nint(datum)
c
          call uvrdvri (lin, 'pol', polidx, -10)
          if (polidx.lt.-8 .or. polidx.gt.4) call bug ('f',
     +        'Illegal polarization in file')
c
          if (wantpo(polidx)) then
            if (.not.brief) then
              write (aline,100) nrec+j-1, polist(polidx)
100           format ('Flagging record ', i7, ' polarization ', a2)
              call output (aline)
            end if
            call flagit (lin, nread, scratch(4), gflags, nflag(polidx))
          end if
          call countFlags (maxchan, nread, gflags, ntot(polidx), 
     +                     nftot(polidx))
        end do
      end do
c
c Read through remainder of file to get statistics correct
c
      if (irec.lt.icnt) then
        do while (nread.ne.0)
          call uvread (lin, pream, data, gflags, maxchan, nread)
          if (nread.ne.0) then
            call uvrdvri (lin, 'pol', polidx, -10)
            if (polidx.lt.-8 .or. polidx.gt.4) call bug ('f',
     +          'Illegal polarization in file')
            call countFlags (maxchan, nread, gflags, ntot(polidx), 
     +                       nftot(polidx))
          end if
        end do
      end if
c
c Report and close
c
      call output (' ')
      call output (' ***** Summary *****')
      call output (' ')
      do i = -1, -8, -1
        if (nflag(i).gt.0) then
          write (aline, 200) polist(i), nflag(i)
200       format ('For polarization ', a2, ', ', i9,
     +            ' correlations were flagged')
          call output (aline)
        end if
      end do
      do i = 1, 4
        if (nflag(i).gt.0) then
          write (aline, 200) polist(i), nflag(i)
          call output (aline)
        end if
      end do
c
      call output (' ')
      do i = -1, -8, -1
        if (found(i).eq.1) then
          write (aline, 300) polist(i), 
     +                       real(nftot(i))/real(ntot(i))*100.0
300       format ('For polarization ', a, ', ', f8.2, 
     +            '% of your data are now flagged')
          call output (aline)
        end if
      end do
      do i = 1, 4
        if (found(i).eq.1) then
          write (aline, 300) polist(i), 
     +                       real(nftot(i))/real(ntot(i))*100.0
          call output (aline)
        end if
      end do
c
c Write the history.
c
      call hisopen (lin, 'append')
      call hiswrite (lin, 'UVPFLAG: Miriad '//version)
      call hisinput (lin, 'UVPFLAG')
      call hisclose (lin)
c
c Close up the files.
c
      call uvclose (lin)
      call scrclose (lins)
c
      end
c
c
      subroutine flagit (lin, nread, tflags, gflags, nflags)
c-----------------------------------------------------------------------
c     Flag channels when the template channels are bad.
c     
c
c   Input
c      lin    Handle of visibility file
c      nread  NUmber of channels
c      tflags The template flags
c   Input/output
c      gflags The visibility flags
c      nflags Number of slave polarization correlations flagged
c
c-----------------------------------------------------------------------
      implicit none
c     
      integer lin, nread, nflags
      real tflags(nread)
      logical gflags(nread)
cc
      integer i
c-----------------------------------------------------------------------
      do i = 1, nread
        if (tflags(i).lt.0) then
          gflags(i) = .false.
          nflags = nflags + 1
        end if
      end do
c
      call uvflgwr (lin, gflags)
c
      end
c
c
      subroutine getopt (brief, doand)
c-----------------------------------------------------------------------
c     Get user options
c
c   Output:
c      brief  SHort summary instead of all the juice
c      doand  AND the template poalrizations, else OR them
c             to see which slaves get the chop
c-----------------------------------------------------------------------
      implicit none
c
      logical brief, doand
cc
      integer nopt
      parameter (nopt = 2)
c
      character opts(nopt)*7
      logical present(nopt)
      data opts /'full', 'or'/
c-----------------------------------------------------------------------
      call options ('options', opts, present, nopt)
      brief = .not.present(1)
      doand = .not.present(2)
c
      end
c
c
      subroutine countFlags (maxn, n, flag, ntot, nftot)
      implicit none
      integer n, ntot, nftot, maxn
      logical flag(maxn)
cc
      integer i
c
      if (n.eq.0) return
c
      do i = 1, n
        ntot = ntot + 1
        if (.not.flag(i)) nftot = nftot + 1
      end do
c
      end

