      program impos
      implicit none
c
c= IMPOS - Converts image coordinates between different systems.
c& nebk
c: image analysis
c+
c	IMPOS takes an image coordinate in a specified system (such
c	as "abspix" or "arcsec") and converts it to all appropriate
c	coordinate systems (absolute world, offset world, pixels, 
c	offset pixels).   Spectral axes are converted to values in
c	frequency, radio (VELO) and optical (FELO) velocities.
c
c	If the specified coordinate represents a pixel actually on the
c	image, its value is reported as well.
c
c@ in
c	The input image.
c@ coord
c	Specify the coordinate for each axis that you are interested 
c	in; you don't necessarily need one for every axis in the image.
c	No default.
c@ type
c	Specify the coordinate system of the input coordinate for each 
c	axis.  Different axes can be in different systems. Choose from:
c
c	   "hms"         HH:MM:SS.S  (e.g. for RA)
c	   "dms"         DD:MM:SS.S  (e.g. for DEC)
c	   "arcsec"      Arcseconds relative to the reference pixel
c	   "absdeg"      Absolute degrees
c	   "reldeg"      Degrees relative to the reference pixel
c	   "abspix"      Pixels 
c	   "relpix"      Pixels relative to the reference pixel
c	   "absghz"      GHz
c	   "relghz"      GHz relative to the reference pixel
c	   "abskms"      Km/s
c	   "relkms"      Km/s relative to the reference pixel
c	   "abslin"      Linear coordinate 
c	   "rellin"      Linear coordinate relative to the reference pixel
c
c	Default is "abspix", or value of previous coordinate.
c@ stype
c	'radio', 'optical', or 'frequency'.  If you specify a spectral axis
c	coordinate, this indicates what convention it is in.  For example, 
c	you might give an optical velocity with "type=abskms", but the header 
c	indicates a frequency axis.  If unset, it is assumed the coordinate
c	is in the convention defined by the image header.   
c	
c--
c
c  History:
c    nebk 03dec92  Original version
c    rjs  04jan93  Rename "pad" to "pader"
c    mjs  12mar93  Use maxnax.h file instead of setting own value.
c    nebk 22jun93  Removed unused units.  Minor doc change.
c    nebk 26aug93  Add "absdeg" and "reldeg" types
c    nebk 07jan94  Remove pixel-> world restriction.  Now world->world.
c    nebk 17aug94  Revise to use new COCVT coord. transformation routines
c                  Remove keyword "typeo"
c    nebk 14oct94  Fix problem when restfreq=0 
c    nebk 27jan95  Remove *(*) concatenations
c    nebk 13nov95  Better non-coordinate checking
c    nebk 29nov95  Calls to CTYPECO were out of date.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
c
      integer maxtyp
      parameter (maxtyp = 13)
c
      double precision win(maxnax), pixel(maxnax), rfreq
      real data(maxdim), value
      integer ntypei, lun, naxis, nsize(maxnax), i, ipix(maxnax), 
     +  nco, il, nstypes, sax
      character file*80, typei(maxnax)*6, typeo(maxnax)*6, 
     +  typeo2(maxnax)*6, typeo3(maxnax)*6, labtyp(maxtyp)*6, bunit*9, 
     +  ctypes(maxnax)*9, stypes(3)*9, sctypes(3)*4, stypei*9
      character*80 strout1(maxnax), strout2(maxnax), strout3(maxnax)
      character text*132, str1*132, trail*6
      integer strlen1(maxnax), strlen2(maxnax), strlen3(maxnax)
      logical off, dospec
c
      data labtyp /'hms   ', 'dms   ', 'abspix', 'relpix',
     +             'arcsec', 'absghz', 'relghz', 'abskms', 
     +             'relkms', 'abslin', 'rellin', 'absdeg',
     +             'reldeg'/
      data stypes /'frequency', 'radio', 'optical'/
      data typei /maxnax*' '/
      data nco, ipix /0, maxnax*1/
c-----------------------------------------------------------------------
      call output ('IMPOS: version 13-Nov-95')
      call output (' ')
c
c  Get inputs
c
      call keyini
      call keyf ('in', file, ' ')
      if (file.eq.' ') call bug ('f', 'Input file must be given')
      call keymatch ('type', maxtyp, labtyp, maxnax, typei, ntypei)
c
      do i = 1, maxnax
c
c Set type defaults
c
        if (typei(i).eq.' ') then
          if (i.eq.1) then
            typei(i) = 'abspix'
          else 
            typei(i) = typei(i-1)
          end if
        end if
c
c Get coordinate
c
        if (typei(i).eq.'hms' .or. typei(i).eq.'dms') then
          call keyt ('coord', win(i), typei(i), -123456789.0d0)
          if (win(i).ne.-123456789.0d0) nco = nco + 1
        else 
          call keyd ('coord', win(i), -123456789.0d0)
          if (win(i).ne.-123456789.0d0) nco = nco + 1
        end if
      end do
      if (nco.eq.0) call bug ('f', 'You must give a coordinate')
c
c Get spectral-axis type
c
      call keymatch ('stype', 3, stypes, 1, stypei, nstypes)
      call keyfin
c
c  Open file
c
      call xyopen (lun, file, 'old', maxnax, nsize)
      call rdhdi (lun, 'naxis', naxis, 0)
      if (naxis.eq.0) call bug ('f', 'Zero dimensions in image')
      naxis = min(naxis, maxnax)
      if (nsize(1).gt.maxdim) call bug ('f',
     +  'First axis of image too big for me')
      call rdhda (lun, 'bunit', bunit, ' ')
      call rdhdd (lun, 'restfreq', rfreq, 0.0d0)
c
c Initialize coordinate transformation routines and fish out CTYPES
c
      call initco( lun)
      do i = 1, naxis
        call ctypeco (lun, i, ctypes(i))
      end do
c
c Check spectral-axis type, set default value if needed and
c convention order in which spectral axes will be listed
c
      call sstdef (lun, nco, typei, stypei, sax)
      dospec = sax.ne.0 .and. nco.ge.sax
      if (sax.gt.0) then
        trail = ctypes(sax)(5:)
        sctypes(1) = ctypes(sax)(1:4)
        if (rfreq.gt.0.0d0) then
          if (sctypes(1).eq.'FREQ') then
            sctypes(2) = 'VELO'
            sctypes(3) = 'FELO'
            stypes(1) = 'frequency'
            stypes(2) = 'radio'
            stypes(3) = 'optical'
          else if (sctypes(1).eq.'VELO') then
            sctypes(2) = 'FELO'
            sctypes(3) = 'FREQ'
            stypes(1) = 'radio'
            stypes(2) = 'optical'
            stypes(3) = 'frequency'
          else if (sctypes(1).eq.'FELO') then
            sctypes(2) = 'VELO'
            sctypes(3) = 'FREQ'
            stypes(1) = 'optical'
            stypes(2) = 'radio'
            stypes(3) = 'frequency'
          end if
        else
          dospec = .false.
          stypes(1) = ' '
          stypes(2) = ' '
          stypes(3) = ' '
        end if
      end if
c
c***********************************************************************
c World coordinate
c***********************************************************************
c
      call setoaco (lun, 'abs', nco, 0, typeo)
c
c Convert & format and inform
c
      call w2wfco (lun, nco, typei, stypei, win, typeo, stypes(1),
     +             .false., strout1, strlen1)
c
      if (dospec) then
        call repspc (sax, stypes, nco, typeo, typeo2, typeo3)
        call w2wfco (lun, nco, typei, stypei, win, typeo2, stypes(2),
     +               .false., strout2, strlen2)
        call w2wfco (lun, nco, typei, stypei, win, typeo3, stypes(3),
     +               .false., strout3, strlen3)
      end if
c
      call output (' ')
      call output ('World coordinates')
      do i = 1, nco
        call pader (typeo(i), strout1(i), strlen1(i))
c
        if (i.eq.sax) then
          write (text, 100) i, sctypes(1)//trail, 
     +                      strout1(i)(1:strlen1(i))
          call output (text)
c
          if (dospec) then
            write (text, 100) i, sctypes(2)//trail, 
     +                        strout2(i)(1:strlen2(i))
            call output (text)
            write (text, 100) i, sctypes(3)//trail, 
     +                        strout3(i)(1:strlen3(i))
            call output (text)
          end if
        else
          write (text, 100) i, ctypes(i), strout1(i)(1:strlen1(i))
100       format ('Axis ', i1, ': ',  a, ' = ', a)
          call output (text)
        end if
      end do
c
c***********************************************************************
c Offset world coordinate
c***********************************************************************
c
      call setoaco (lun, 'off', nco, 0, typeo)
      call w2wfco (lun, nco, typei, stypei, win, typeo, stypes(1),
     +             .false., strout1, strlen1)
c
      if (dospec) then
        call repspc (sax, stypes, nco, typeo, typeo2, typeo3)
        call w2wfco (lun, nco, typei, stypei, win, typeo2, stypes(2),
     +               .false., strout2, strlen2)
        call w2wfco (lun, nco, typei, stypei, win, typeo3, stypes(3),
     +               .false., strout3, strlen3)
      end if
c
      call output (' ')
      call output ('Offset world coordinates')
      do i = 1, nco
        if (i.eq.sax) then
          write (text, 100) i, sctypes(1)//trail,
     +                      strout1(i)(1:strlen1(i))
          call output (text)
c
          if (dospec) then
            write (text, 100) i, sctypes(2)//trail,
     +                        strout2(i)(1:strlen2(i))
            call output (text)
            write (text, 100) i, sctypes(3)//trail, 
     +                        strout3(i)(1:strlen3(i))
            call output (text)
          end if
        else
          write (text, 100) i, ctypes(i), strout1(i)(1:strlen1(i))
          call output (text)
        end if
      end do
c
c***********************************************************************
c Absolute pixels 
c***********************************************************************
c
      do i = 1, nco
        typeo(i) = 'abspix'
      end do
      call w2wco  (lun, nco, typei, stypei, win, typeo, ' ', pixel)
      call w2wfco (lun, nco, typei, stypei, win, typeo, stypes(1),
     +             .true., strout1, strlen1)
c
      call output (' ')
      call output ('Absolute pixels')
      do i = 1, nco
        write (text, 100) i, ctypes(i), strout1(i)(1:strlen1(i))
        call output (text)
      end do
c
c***********************************************************************
c Offset pixels
c***********************************************************************
c
      do i = 1, nco
        typeo(i) = 'relpix'
      end do
      call w2wfco (lun, nco, typei, stypei, win, typeo, stypes(1),
     +             .true., strout1, strlen1)
c
      call output (' ')
      call output ('Offset pixels')
      do i = 1, nco
        write (text, 100) i, ctypes(i), strout1(i)(1:strlen1(i))
        call output (text)
      end do
c***********************************************************************
c
c Find nearest pixel to coordinate location
c
      off = .false.
      do i = 1, nco
        ipix(i) = nint(pixel(i))
        if (ipix(i).lt.1 .or. ipix(i).gt.nsize(i)) off = .true.
      end do
c
c Find value if on image
c
      if (.not.off) then
        call xysetpl (lun, maxnax-2, ipix(3))
        call xyread (lun, ipix(2), data)
        value = data(ipix(1))
c
        call output (' ')
        call mitoaf (ipix, nco, str1, il)
        write (text, 200) str1(1:il), value, bunit
200     format ('Nearest pixel = ', a, '.  Value = ', 1pe13.6, ' ', a)
        call output (text)
      end if
c
c Close up
c
      call xyclose (lun)
      call finco (lun)
      end
c
c
      subroutine pader (type, str, ilen)
      implicit none
      character str*(*), str2*132, type*(*)
      integer len1, ilen, it
c
      if (type.eq.'hms' .or. type.eq.'dms') then
        str2 = str
        it = index(str2,':')
        str = ' '
        str(3-it+2:) = str2(1:len1(str2))
        ilen = len1(str)
      end if
c
      end
c
c
      subroutine sstdef (lun, n, typei, stypei, sax)
c-----------------------------------------------------------------------
c     Check consistency of spectral-axis type and set a default
c     if needed
c
c  Input
c    typei     User specified coordinate types ('hms' etc)
c  Output
c    stypei    Will be ' ' if the user has not given a coordinate for
c              the spectral axis.  Else 'radio', 'optical' 
c              or 'frequency'
c    sax       Spectral axis number of image
c-----------------------------------------------------------------------
      implicit none
      integer lun, n, sax
      character*(*) typei(n), stypei
cc
      character ltype*3, dstype*9, line*80
      integer i
c-----------------------------------------------------------------------
c
c First set a default spectral axis type based upon the header
c
      dstype = ' '
      sax = 0
      i = 1
      do while (i.le.n .and. dstype.eq.' ')
c
c See if this axis is spectral
c
        call specco (lun, i, dstype)
        if (dstype.ne.' ') sax = i
        i = i + 1
      end do
c
c Check if user has given a spectral coordinate for a non-spectral axis
c
      do i = 1, n
        if (typei(i)(4:6).eq.'ghz' .or. typei(i)(4:6).eq.'kms') then
          if (i.ne.sax) call bug ('f', 
     +      'Spectral coordinate given for non-spectral axis')
        end if
      end do
c
c Now, if the image has a spectral axis, continue on to see what
c the user is offering for that axis.
c
      if (sax.ne.0 .and. sax.le.n) then
c
c Fish out latter half of user given coordinate type and check
c given spectral-axis type, or set one if not given
c
        ltype = typei(sax)(4:6)
c
        if (ltype.eq.'kms') then
c
c User says coordinate in km/s
c
          if (stypei.eq.' ') then
            if (dstype.ne.'frequency') then
c
c Default to whatever type of velocity definition we have
c
              stypei = dstype
            else
c
c Can't work out what type of velocity user wants as axis is frequency
c
              call bug ('f', 'You must give keyword "stype" as '//
     +                  'the axis is frequency')
            end if
          else
c
c Inconsistent, must give radio or optical
c
            if (stypei.eq.'frequency') then
              line = 'Coord. type "'//typei(sax)//'" & spectral type "'
     +                //stypei//'" do not match'
              call bug ('f', line)
            end if
          end if
        else if (ltype.eq.'ghz') then
c
c User says coordinate in GHz
c
          if (stypei.eq.' ') then
c
c Give them frequency
c
            stypei = 'frequency'
          else
c
c Inconsistent, must be frequency
c
            if (stypei.ne.'frequency') then
              line = 'Coordinate type '//typei(sax)//
     +               ' & spectral type '//stypei//' do not match'
              call bug ('f', line)
            end if
          end if         
        else 
c
c Make spectral axis type that indicated by header
c
          stypei = dstype
        end if
      else
        stypei = ' '
      end if
c
      end
c
      subroutine repspc (sax, stypes, n, typeo, typeo2, typeo3)
c-----------------------------------------------------------------------
c     See if any of the axes is a spectral axis.  If it is, then
c     we want to list the spectral axis in frequency, radio and
c     optical velocities
c-----------------------------------------------------------------------
      implicit none
      integer n, sax
      character*(*) typeo(n), typeo2(n), typeo3(n), stypes(3)
cc
      integer i
      character*3 lstype(3)
c-----------------------------------------------------------------------
      do i = 1, n
        typeo2(i) = typeo(i)
        typeo3(i) = typeo(i)
      end do
c
      if (sax.gt.0) then
        do i = 1, 3
          if (stypes(i).eq.'optical' .or. stypes(i).eq.'radio') then
            lstype(i) = 'kms'
          else 
            lstype(i) = 'ghz'
          end if
        end do
c
        typeo2(sax)(4:6) = lstype(2)
        typeo3(sax)(4:6) = lstype(3)
      end if
c
      end
