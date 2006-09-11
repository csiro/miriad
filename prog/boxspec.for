      program boxspec
c--------------------------------------------------------------------------
c= Boxspec - Write spectra (from vxy cube) as text file
c& nebk
c: utility, plotting
c+
c	BOXSPEC is a MIRIAD task to save spectra from a vxy image in
c	text files.
c< in
c@ chan
c	The channel range to plot, default is all channels.
c@ aveop
c	If 'a' then the pixels enclosed in the x-y area specified
c	are averaged.  If 's' they are just summed.  Default is 'a'
c@ hann
c	Hanning smoothing length (an odd integer < 15) Default is
c	no smoothing.
c@ infile
c	List of locations and sizes for boxes in format
c		NPOS
c		IPOS  X  Y  XOFF  YOFF
c	where 
c		NPOS   is the number of boxes to read
c		IPOS   is a number identifying each box, used in
c		       creating each output file name
c		X,Y    is the pixel location of each box
c		X,YOFF are the half sizes of the box in pixels so that
c		       each box is of size 2XOFF+1 in x and 2YOFF+1 in y.  
c		X,YOFF are optional and default to 2
c@ log        
c	Save each spectrum in a file with root name LOG.  The
c	rest of the file name is formed from ``IPOS'' so that
c	for example, for LOG=SPEC and IPOS=32 the output file name
c	is SPEC32   
c       Each file contains  : 	channel, velocity or freq, intensity
c--
c
c   Neil Killeen   05-Sep-89
c   nebk           01-Dec-90  Allow longer file names and rearrange
c                             normalization factor to avoid C220 compiler
c                             bug.
c   nebk           11-apr-91  Change to reflect new HANNSM and rename
c                             variables that are also fortran intrinsics
c                             Also changed input adverb SMLEN to HANN
c                             for consistency with other tasks
c   nebk           20-mar-92  Move these comments to standard place in file
c   nebk           06-apr-92  Check buffer big enough for row
c--------------------------------------------------------------------------
      include 'maxdim.h'
      character*(*) version
      parameter (version='version 06-apr-92')
c
      integer maxco
      parameter (maxco = 15)
c
      real row(maxdim), spec(maxdim), work(maxco), npix, crval1,
     *cdelt1, crpix1, coeffs(maxco), chann(maxdim), veltyp(maxdim),
     *norm
      integer lunin, xmin, xmax, ymin, ymax, bchan, echan, siz(3),
     *indx, i, j, k, l, nsmth, nchan, iostat, lunout, pos(5), npos, 
     *luntxt, l2, l3, ilen
      character in*60, mode*3, logf*40, line*72, filnam*60,
     *str*3, ofile*40, pline*72, umsg*79
c
      character itoaf*3
      integer len1
c--------------------------------------------------------------------------
      call output('Boxspec: '//version)
c
c Get inputs
c
      call keyini
      call keya('in',in,' ')
      call keyi('chan',bchan,1)
      call keyi('chan',echan,maxdim)
      call keya('aveop',mode,'a')
      if (mode.ne.'a' .and. mode.ne.'s') mode = 'a'
      call keyi('hann',nsmth,1)
      if (nsmth.gt.maxco) then
        str = itoaf (maxco)
        call bug ('f', 'Hanning smoothing length must be <= '//str)
      end if
      call keya('infile', filnam, ' ')
      call keya('log',logf,' ')
      call keyfin
      if(filnam.eq.' ' .or. logf.eq.' ')
     *   call bug ('f', 'Input or output files not given')
c
c Open file and get units
c
      call xyopen (lunin, in, 'old', 3, siz)
      if (siz(1).gt.maxdim) 
     *   call bug ('f', 'First axis of image too big for storage')
      call rdhdr (lunin, 'crpix1', crpix1, 0.0)
      call rdhdr (lunin, 'crval1', crval1, 0.0)
      call rdhdr (lunin, 'cdelt1', cdelt1, 0.0)
c
c Check boundaries of spectrum.
c
      bchan = max(1, bchan)
      echan = min(siz(1), echan)
      nchan = echan - bchan + 1
c
c Open positions list file and get number of boxes
c
      call txtopen (luntxt, filnam, 'old', iostat)
      if (iostat.ne.0) call bug ('f', 'Error opening positions list')
      call txtread (luntxt, pline, ilen, iostat)
      if (iostat.ne.0) call bug('f','Error reading number of boxes')
      ilen = len1(pline)
      call posdec1 (pline, ilen, npos)
c
c Read and decode locations and set window
c
      do l = 1, npos
        pline = ' '
        call txtread (luntxt, pline, ilen, iostat)
        if (iostat.ne.0) call bug ('f', 
     *                             'Error reading from positions file')
        ilen = len1(pline)
        call posdec2 (l, pline, ilen, pos)
c
        xmin = pos(2) - pos(4)
        xmax = pos(2) + pos(4)
        ymin = pos(3) - pos(5)
        ymax = pos(3) + pos(5)
c
        if (mode.eq.'a') then
          npix = (xmax-xmin+1) * (ymax-ymin+1)
        else
          npix = 1.0
        end if
        norm = 1.0 / npix
c
c Loop over current window
c
        do i = 1, nchan
          spec(i) = 0.0
        end do
        do k = ymin, ymax
           call xysetpl(lunin,1,k)
           do j = xmin, xmax
              call xyread(lunin,j,row)
              do i = bchan, echan
                indx = i-bchan+1
                chann(indx) = i
                veltyp(indx) = crval1 + (i-crpix1)*cdelt1
                spec(indx) = spec(indx) + norm*row(i)
              enddo
           enddo
        enddo
c
c Optionally Hanning smooth spectrum
c
        if (nsmth.ge.3) then
           call hcoeffs (nsmth, coeffs)
           call hannsm (nsmth, coeffs, nchan, spec, work)
        end if
c
c Open output file
c
        str = itoaf(pos(1))
        l2 = len1(str)
        l3 = len1(logf)
        ofile = logf(1:l3)//str(1:l2)
        umsg = 'Opening file '//ofile
        call output ( umsg )
        call txtopen (lunout, ofile, 'new', iostat)
        if (iostat.ne.0) call bug ('f', 'Error opening output file')
c
        do i = 1, nchan
          write (line,30) chann(i), veltyp(i), spec(i)
30        format (1pe12.5, 3x, 1pe12.5, 3x, 1pe12.5)
          call txtwrite (lunout, line, 45, iostat)
          if (iostat.ne.0) call bug ('f', 'Error writing output file')
        end do
        call txtclose(lunout)
      end do
      call txtclose(luntxt)
      call xyclose(lunin)
c
      end
c************************************************************************
      subroutine posdec1 (aline, ilen, npos)
c
      implicit none
      integer npos, ilen
      character*(*)aline
c
c     Decode string into number of positions read from positions
c     list file 
c
c     Input
c       aline   Input string
c       ilen    Length of string with trailing blanks stripped
c     Output
c       npos    Number of boxes
c----------------------------------------------------------------------
      integer i
      logical ok
c----------------------------------------------------------------------
      if (ilen.gt.0) then
        i = 1
        do while (aline(i:i).eq.' ' .and. i.le.ilen)
          i = i + 1
        end do
        call atoif (aline(i:ilen), npos, ok)
        if (.not.ok) call bug ('f', 
     *              'Error decoding first line of position list')
      else
        call bug ('f', 'Error decoding first line of position list')
      end if
c
      end
c
c
      subroutine posdec2 (iline, aline, ilen, pos)
c---------------------------------------------------------------------
c     Decode string into positions list
c
c     Input
c       iline    Current line number
c       aline    Input string
c       ilen     Length of string with trailing blanks ignored
c     Output
c       pos      List of: overlay #, xpos, ypos, xoff, yoff
c                xoff, yoff are optional, and default to 2 and 2
c                
c---------------------------------------------------------------------
      implicit none
c
      integer ilen, pos(5), iline
      character*(*) aline
cc 
      double precision val
      integer ist, iend, j, slen
      logical ok
      character estr*50, str*3
c
      integer len1
      character itoaf*3
c--------------------------------------------------------------------
      str = itoaf(iline)
      slen = len1(str)
      estr = 'Error decoding line '//str(1:slen)//' from positions list'
c
      if (ilen.gt.0) then
c
c Decode line and convert to pixels
c
        ist = 1
        j = 1
        ok = .true.
        do while (j.le.5 .and. ok)
c
c Find start and end locations of current number in string
c
          do while (aline(ist:ist).eq.' ' .and. ist.le.ilen)
            ist = ist + 1
          end do
          iend = index(aline(ist:),' ')
          if (iend.eq.0) then
            iend = ilen
          else
            iend = iend + (ist - 1) - 1
          end if
c 
c Decode number
c
          if (ist.gt.iend) then
c
c Last number was at the end of the string
c
            ok = .false.
          else
c
c Try to extract current number from string
c
            call atodf (aline(ist:iend), val, ok) 
          end if
          if (ok) then
c
c Decode
c
            if (j.le.3) then
              pos(j) = nint(val)
            else
              pos(j) = abs(val)
            end if
          else 
c
c Catch error or set defaults
c
            if (j.le.3) then
              call bug ('f', estr)
            else if (j.eq.4) then
              pos(4) = 2
              pos(5) = 2
            else if (j.eq.5) then
              pos(5) = 2
            end if
          end if
          ist = iend + 1
          j = j + 1
        end do
      else
        call bug ('f', 'Error decoding  line from position list')
      end if
c
      end
