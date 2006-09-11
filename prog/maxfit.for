      program maxfit
      implicit none
c
c= MAXFIT - Fits a 2-D parabola to a 3x3 region of an image
c& nebk
c: image analysis
c+
c	MAXFIT finds the maximum value of a region of an image.
c	This region may be three dimensional.  It then fits a 
c	parabola to a 3x3 array extracted from the first two 
c	dimensions of the image and centred on the maximum pixel 
c	in the specified region.  MAXFIT then returns the location
c	and value of the maximum pixel and the fitted pixel.
c
c@ in
c	The input image.
c@ region
c	Region of interest in which to search for the maximum pixel.
c	The default is the whole image.
c@ options
c	"abs"  means that MAXFIT search for the maximum absolute 
c	       pixel value rather than the default, which is just
c	       the maximum pixel value.
c@ log
c	Write results to this log file as well as screen.
c--
c
c  History:
c    nebk 28Aug91  Original version nicked from Werong version.
c    nebk 02Sep91  Don't restrict REGION to a plane
c    mjs  04sep91  Call output with ' ' string (not '') for Cray.
c    nebk 20may92  Improve documentation
c    nebk 02dec92  Add RA and DEC format output
c    rjs  04jan93  Rename "pad" to "pader" to avoid conflict.
c    mjs  12mar93  Use maxnax.h file instead of setting own value.
c    nebk 22jun93  Increase the size of STR 
c    nebk 26aug93  Include new "absdeg" and "reldeg" axis types
c    nebk 16sep93  Add keyword log
c    nebk 09jan94  Convert CRPIX to double precision
c    rjs  24jan94  Small typo in doc (appease pjt and bpw's "doc").
c    nebk 22mar94  Twiddle about with output format
c    nebk 18aug94  Revise to use COCVT coord. transformation routines
c    nebk 16nov95  New calls to some "co" routines
c    nebk 29nov95  New call for CTYPECO
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
c
      integer maxboxes, maxruns
c
      parameter (maxboxes = 2048, maxruns =3*maxdim)
c
      double precision pixmax(maxnax)
      real data(maxdim), dmax, fit(9), coeffs(6), fmax, dd
      integer nsize(maxnax), blc(maxnax), trc(maxnax), boxes(maxboxes),
     +  runs(3,maxruns), ploc(maxnax), strlen(maxnax), nruns, lun, 
     +  naxis, i, j, k, l, ip, il, len1
      character ctype*9, typesi(maxnax)*9, typeso(maxnax)*9, 
     +  strout(maxnax)*50, file*40, text*132, logf*132
      logical doabs
c-----------------------------------------------------------------------
      call output ('MAXFIT: version 29-Nov-95')
c
c  Get inputs
c
      call keyini
      call keya ('in', file, ' ')
      if (file.eq.' ') call bug ('f', 'Input file must be given')
      call boxinput ('region', file, boxes, maxboxes)
      call keya ('log', logf, ' ')
      call getopt (doabs)
      call keyfin
c
c  Open file
c
      call xyopen (lun, file, 'old', maxnax, nsize)
      call rdhdi (lun, 'naxis', naxis, 0)
      if (naxis.eq.0) call bug ('f', 'Zero dimensions in image')
      naxis = min(3,naxis)
      if (nsize(1).gt.maxdim) call bug ('f','Input file too big for me')
c
c  Set up the region of interest.
c
      call boxmask (lun, boxes, maxboxes)
      call boxset (boxes, maxnax, nsize, ' ')
      call boxinfo (boxes, maxnax, blc, trc)
c
c Open log file
c
      if (logf.ne.' ') call logopen (logf, ' ')
c
c  Read in region, and  find the maximum pixel in that region
c  taking account of blanked pixels.
c
      dmax = -1.0e30
c
c  Loop over planes
c
      do k = blc(3), trc(3)
        call xysetpl (lun, 1, k)
        call boxruns (1, k, ' ', boxes, runs, maxruns,
     +                nruns, blc(1), trc(1), blc(2), trc(2))
c
        if (nruns.gt.0) then
          j = 0
          do l = 1, nruns
c
c  Read new row, if needed, from current plane
c
            if (runs(1,l).ne.j) then
              j = runs(1,l)
              call xyread (lun, j, data)
            end if
c
c  Loop over all good pixels in row
c
            do i = runs(2,l), runs(3,l)
              dd = data(i)
              if (doabs) dd = abs(data(i))
              if (dd.gt.dmax) then
                dmax = data(i)
                if (doabs) dmax = abs(data(i))
                ploc(1) = i
                ploc(2) = j
                ploc(3) = k
              end if
            end do
          end do
        end if
      end do
c
c  Make sure the peak estimate is not on the edge of the image
c
      do i = 1, min(2,naxis)
        if (ploc(i).eq.1 .or. ploc(i).eq.nsize(1)) then
          write (text, 10) i, ploc(i)
10        format ('Axis ', i1, ' max. pixel at ', i4,
     +            ' is too close to the image edge')
          call bug ('f', text)
        end if
      end do
c
c Now pick out the 3x3 region for fitting.  Blanked pixels are not 
c dealt with in this step. 
c
      call xysetpl (lun, 1, ploc(3))
      k = 0
c 
      do j = ploc(2)-1, ploc(2)+1
        call xyread (lun, j, data)
        do i = ploc(1)-1, ploc(1)+1
          k = k + 1
          fit(k) = data(i)
        end do
      end do
c
c  Do the fit
c
      call solve (fit, 3, fmax, pixmax, coeffs)
c
c  Change the x and y to image pixel coordinates and add the z location
c
      pixmax(1) = ploc(1) + pixmax(1)
      pixmax(2) = ploc(2) + pixmax(2)
      pixmax(3) = ploc(3)
c
c  Tell user of maxfit's endeavours
c
      call output (' ')
      if (logf.ne.' ') call logwrit (' ')
c
c Peak pixel location and value
c
      text = 'Peak pixel   : ('
      ip = len1(text) + 1
      do i = 1, naxis
        call strfi (ploc(i), '(i4)', text(ip:), il)
        ip = ip + il
        text(ip:ip) = ','
        ip = ip + 1
      end do
      text(ip-1:) = ') = '
      ip = len1(text) + 2
c
      call strfr (dmax, '(1pe12.4)', text(ip:), il)
      call  output(text)
      if (logf.ne.' ') call logwrit (text)
c
c Fitted location and fitted value
c
      call output (' ')
      if (logf.ne.' ') call logwrit (' ')
      text = 'Fitted pixel : ('
      ip = len1(text) + 1
      do i = 1, naxis
        if (i.le.2) then
          call strfd (pixmax(i), '(f7.2)', text(ip:), il)
        else
          call strfi (ploc(i), '(i4)', text(ip:), il)
        end if
        ip = ip + il
        text(ip:ip) = ','
        ip = ip + 1
      end do
      text(ip-1:) = ') = '
      ip = len1(text) + 2
c
      call strfr (fmax, '(1pe12.4)', text(ip:), il)
      call  output(text)
      if (logf.ne.' ') call logwrit (text)
c
c Find offsets of fitted pixel from reference pixel
c
      call initco (lun)
c
      call output (' ')
      if (logf.ne.' ') call logwrit (' ')
      call output ('Offsets from reference pixel :')
      if (logf.ne.' ') call logwrit ('Offsets from reference pixel :')
c
c Convert coordinates
c
      do i = 1, naxis
        typesi(i) = 'abspix'
      end do
      call setoaco (lun, 'off', naxis, 0, typeso)
      call w2wfco (lun, naxis, typesi, ' ', pixmax, typeso, ' ', 
     +             .false., strout, strlen)
c
c Tell user
c
       do i = 1, naxis
        if (i.lt.3) then
          write (text,70) i, strout(i)(1:strlen(i))
70        format ('  Axis ', i1, ': Fitted pixel offset = ', a)
        else
          write (text, 75) i, strout(i)(1:strlen(i))
75        format ('  Axis ', i1, ':        pixel offset = ', a)
        end if
        call output (text)
        if (logf.ne.' ') call logwrit (text)
      end do
c                
c Compute world coordinate
c
      call output (' ')
      if (logf.ne.' ') call logwrit (' ')
      call output ('Coordinate:')
      if (logf.ne.' ') call logwrit ('Coordinate:')
c
c Convert coordinates
c
      call setoaco (lun, 'abs', naxis, 0, typeso)
      call w2wfco (lun, naxis, typesi, ' ', pixmax, typeso, ' ', 
     +             .false., strout, strlen)
c
c Tell user
c
      do i = 1, naxis
        call pader (typeso(i), strout(i), strlen(i))
c
        call ctypeco (lun, i, ctype, il)
        if (i.lt.3) then
          write (text,80) i, ctype, strout(i)(1:strlen(i))
80        format ('  Axis ', i1, ': Fitted ', a, ' = ', a)
        else
          write (text, 85) i, ctype, strout(i)(1:strlen(i))
85        format ('  Axis ', i1, ':        ', a, ' = ', a)
        end if
        call output (text)
        if (logf.ne.' ') call logwrit (text)
      end do
c
      call xyclose (lun)
      if (logf.ne.' ') call logclose
      call finco (lun)
c
      end
c
c
      subroutine solve (z, width, zmax, pix, c)
c---------------------------------------------------------------------
c  Fit a parabola to the function in Z.
c
c  Input:
c    Width      Width of the support of Z.
c    Z          Function values.
c
c  Output:
c    C          Coefficients of the fit.
c    zmax       Estimated max value of the function.
c    pix        Location, relative to the central pixel, of the maximum
c
c-----------------------------------------------------------------------
      implicit none
c
      integer width
      real z(width,width), c(6), zmax
      double precision pix(2)
cc
      integer maxwidth
      parameter(maxwidth = 5)
c
      real array(6,maxwidth*maxwidth), hw, rtemp(36), denom, x, y
      integer i, j, k, itemp(6), ifail
c-----------------------------------------------------------------------
c
c  Fill in the coefficients of the array.
c
      k = 0
      hw = 0.5 * (width + 1)
      do j = 1, width
        y = j - hw
c
        do i=1,width
          k = k + 1
          x = i - hw
          array(1,k) = 1
          array(2,k) = x
          array(3,k) = y
          array(4,k) = x*y
          array(5,k) = x*x
          array(6,k) = y*y
        end do
      end do
c
c  This is a linear problem. Call a routine to solve it.
c
      call llsqu (z, array, 6, width*width, c, ifail, rtemp, itemp)
      if (ifail.ne.0) call bug ('f','Least squares solution failed')
c
c  Work out the location of the maxima, and the value of the function there.
c
      denom  = c(4)*c(4) - 4*c(5)*c(6)
      if (denom.ne.0.0) then
        pix(1) = (2*c(2)*c(6)-c(3)*c(4)) / denom
        pix(2) = (2*c(3)*c(5)-c(2)*c(4)) / denom
      else
        call bug ('f', 
     +    'Least squares solution failed -- all pixels identical ?')
      end if
      zmax = c(1) + c(2)*pix(1) + c(3)*pix(2) + 
     +       c(4)*pix(1)*pix(2) + c(5)*pix(1)*pix(1) +
     +       c(6)*pix(2)*pix(2)
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
c
c
      subroutine getopt (doabs)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c       
c   Output:
c     doabs     Search for absolute maximum pixel
c       
c-----------------------------------------------------------------------
      implicit none
c       
      logical doabs
cc
      integer maxopt
      parameter (maxopt = 1)
c       
      character opshuns(maxopt)*3
      logical present(maxopt)
      data opshuns /'abs'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c       
      doabs = present(1)
c
      end
