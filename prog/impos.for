      program impos
      implicit none
c
c= IMPOS - Converts image coordinates between different systems.
c& nebk
c: image analysis
c+
c	IMPOS takes an image coordinate in a specified system (such
c	as "abspix" or "arcsec") and converts it to the specified
c	coordinates system (such as "hms" or "relpix").  If the
c	specified coordinate represents a pixel actually on the
c	image, its value is reported as well.
c
c@ in
c	The input image.
c@ coord
c	Specify the coordinate for each axis that you are interested 
c	in; you don't necessarily need one for every axis in the image.
c	No default.
c@ typei
c	Specify the coordinate system of the input coordinate for each 
c	axis.  Different axes can be in different systems. Choose from:
c
c	   "hms"         HH:MM:SS.S  (e.g. for RA)
c	   "dms"         DD:MM:SS.S  (e.g. for DEC)
c	   "arcsec"      Arcseconds relative to the reference pixel
c	   "absdeg"      Absolute degrees
c	   "reldeg"      Degrees relative to the reference pixel
c			 The above assume the pixel increment is in radians
c	   "abspix"      Pixels 
c	   "relpix"      Pixels relative to the reference pixel
c	   "absghz"      GHz
c	   "relghz"      GHz relative to the reference pixel
c	   "abskms"      Km/s
c	   "relkms"      Km/s relative to the reference pixel
c	   "abslin"      Linear coordinate (the "natural" axis unit)
c		         Note that RA axes require COORD in radians of
c			 polar rotation with this coordinate type.
c	   "rellin"      Linear coordinate relative to the reference pixel
c
c	Default is "abspix"
c
c@ typeo
c	Specify which coordinate system you would like the input coordinate
c	converted to for each axis.  Different axes can be in different 
c	systems. Choose from the same systems as in keyword TYPEI above
c
c	Default is "abspix"
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
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
c
      double precision r2st, r2sa
      integer maxtyp
      parameter (maxtyp = 13, r2st = 12.0d0*3600.0d0/dpi, 
     +           r2sa = 180.0d0*3600.0d0/dpi)
c
      double precision cdelt(maxnax), crval(maxnax), win(maxnax), 
     +  crpix(maxnax), pixel(maxnax)
      real epoch, data(maxdim), value
      integer ntypei, ntypeo, lun, naxis, nsize(maxnax), i, ilen,
     +  ipix(maxnax), ncoord
      character file*80, typei(maxnax)*6, typeo(maxnax)*6, 
     +  labtyp(maxtyp)*6, bunit*9, ctype(maxnax)*9, text*132, 
     +  str*132
      logical mask, ok, off
c
      data labtyp /'hms   ', 'dms   ', 'abspix', 'relpix',
     +             'arcsec', 'absghz', 'relghz', 'abskms', 
     +             'relkms', 'abslin', 'rellin', 'absdeg',
     +             'reldeg'/
      data typei, typeo /maxnax*' ', maxnax*' '/
      data ncoord, ipix /0, maxnax*1/
c-----------------------------------------------------------------------
      call output ('IMPOS: version 09-Jan-94')
      call bug ('i', 'The inputs have changed -- see help')
      call output (' ')
c
c  Get inputs
c
      call keyini
      call keyf ('in', file, ' ')
      if (file.eq.' ') call bug ('f', 'Input file must be given')
c
c  Open file
c
      call xyopen (lun, file, 'old', maxnax, nsize)
      call rdhdi (lun, 'naxis', naxis, 0)
      if (naxis.eq.0) call bug ('f', 'Zero dimensions in image')
      naxis = min(naxis, maxnax)
      if (nsize(1).gt.maxdim) call bug ('f',
     +  'First axis of image too big for me')
      call hedinfcg (lun, naxis, nsize, epoch, crpix, cdelt,
     +               crval, ctype, mask)
      call rdhda (lun, 'bunit', bunit, ' ')
c
      call keymatch ('typei', maxtyp, labtyp, maxnax, typei, ntypei)
c
      call keymatch ('typeo', maxtyp, labtyp, maxnax, typeo, ntypeo)
c
      do i = 1, maxnax
c
c Set type defaults
c
        if (typei(i).eq.' ') typei(i) = 'abspix'
        if (typeo(i).eq.' ') typeo(i) = 'abspix'
c
c Get coordinate
c
        if (typei(i).eq.'hms' .or. typei(i).eq.'dms') then
          call keyt ('coord', win(i), typei(i), -123456789.0d0)
          if (win(i).ne.-123456789.0d0) then
            ncoord = ncoord + 1
            if (typei(i).eq.'hms') then
              win(i) = win(i) * r2st
            else
              win(i) = win(i) * r2sa
            end if
          end if
        else 
          call keyd ('coord', win(i), -123456789.0d0)
          if (win(i).ne.-123456789.0d0) then
            ncoord = ncoord + 1
          end if
        end if
      end do
c
      call keyfin
      if (ncoord.eq.0) call bug ('f', 'You must give a coordinate')
c 
c Make coordinate conversion
c
      do i = 1, ncoord
        call w2wfcg (win(i), i, typei(i), typeo(i), naxis, crval, 
     +               crpix, cdelt, ctype, .false., str, ilen)
        call pader (typeo, str, ilen)
        write (text, 100) i, ctype(i), str(1:ilen)
100     format ('Axis ', i1, ': ',  a, ' = ', a)
        call output (text)
      end do
c
c Find nearest pixel to coordinate location
c
      off = .false.
      do i = 1, ncoord
        call w2pixcg (win(i), i, typei(i), naxis, crval, crpix, cdelt,
     +                ctype, pixel(i), ok)
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
        call mitoaf (ipix, ncoord, str, ilen)
        write (text, 200) str(1:ilen), value, bunit
200     format ('Nearest pixel = ', a, '.  Value = ', 1pe13.6, ' ', a)
        call output (text)
        call output (' ')
      end if
c
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


