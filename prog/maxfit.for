      program maxfit
      implicit none
c
c= MAXFIT - Fits a 2-D parabola to a 3x3 region of an image
c& nebk
c: image analysis
c+
c	MAXFIT finds the maximum value of a region of an image
c       by fitting a parabola to a 3x3 array centred on the
c       maximum pixel in the specified region.  It returns
c	the fitted value and location.
c@ in
c	The input image.
c@ region
c	Region of interest in which to search for the maximum pixel.
c	The default is the whole image.
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
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
c
      double precision rtod
      integer maxboxes, maxruns
c
      parameter (rtod = 180.0d0/dpi, maxboxes = 2048, maxruns =3*maxdim)
c
      double precision cdelt(maxnax), crval(maxnax)
      real data(maxdim), dmax, fit(9), coeffs(6), crpix(maxnax),
     +  epoch, pixmax(maxnax), fmax
      integer nsize(maxnax), blc(maxnax), trc(maxnax), boxes(maxboxes),
     +  runs(3,maxruns), ploc(maxnax), nruns, lun, naxis, i, j, k, l,
     +  ira, idec, ilat, ilon, ilen
      character file*40, text*132, ctype(maxnax)*9, str*60, type*9,
     +  logf*132
      logical mask
c-----------------------------------------------------------------------
      call output ('MAXFIT: version 26-Aug-93')
c
c  Get inputs
c
      call keyini
      call keya ('in', file, ' ')
      if (file.eq.' ') call bug ('f', 'Input file must be given')
      call boxinput ('region', file, boxes, maxboxes)
      call keya ('log', logf, ' ')
      call keyfin
c
c  Open file
c
      call xyopen (lun, file, 'old', maxnax, nsize)
      call rdhdi (lun, 'naxis', naxis, 0)
      if (naxis.eq.0) call bug ('f', 'Zero dimensions in image')
      naxis = min(naxis, maxnax)
      if (nsize(1).gt.maxdim) call bug ('f','Input file too big for me')
      call hedinfcg (lun, naxis, nsize, epoch, crpix, cdelt,
     +               crval, ctype, mask)
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
              if (data(i).gt.dmax) then
                dmax = data(i)
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
      call xyclose (lun)
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
c  Tell user of endeavours
c
      call output (' ')
      if (logf.ne.' ') call logwrit (' ')
      call output ('Peak pixel location:')
      if (logf.ne.' ') call logwrit ('Peak pixel location:')
      do i = 1, min(3,naxis)
        write(text, 20) i, ploc(i)
20      format ('  Axis ', i1, ' pixel = ', i4)
        call output (text)
        if (logf.ne.' ') call logwrit (text)
      end do
c
      call output (' ')
      if (logf.ne.' ') call logwrit (' ')
      write(text,25) dmax
25    format ('Peak pixel value = ', 1pe12.4)
      call  output(text)
      if (logf.ne.' ') call logwrit (text)
c
      call output (' ')
      if (logf.ne.' ') call logwrit (' ')
      call output ('Fitted pixel location:')
      if (logf.ne.' ') call logwrit ('Fitted pixel location:')
      do i = 1, min(2,naxis)
        write (text,30) i, pixmax(i)
30      format ('  Axis ', i1, ': pixel = ', f7.2)
        call output(text)
        if (logf.ne.' ') call logwrit (text)
      end do
c
      call output (' ')
      if (logf.ne.' ') call logwrit (' ')
      write(text,40) fmax
40    format ('Fitted pixel value = ', 1pe12.4)
      call output (text)
      if (logf.ne.' ') call logwrit (text)
c
c Find offsets from reference pixel
c
      call axfndcg ('RA',  naxis, ctype, ira)
      call axfndcg ('DEC', naxis, ctype, idec)
      call axfndcg ('LONG', naxis, ctype, ilon)
      call axfndcg ('LATI', naxis, ctype, ilat)
c
      call output (' ')
      if (logf.ne.' ') call logwrit (' ')
      call output ('Pixel offsets from reference pixel:')
      if (logf.ne.' ') 
     +  call logwrit ('Pixel offsets from reference pixel:')
      do i = 1, min(3,naxis)
        if (i.eq.ira .or. i.eq.idec) then
          type = 'arcsec'
        else if (i.eq.ilon .or. i.eq.ilat) then
          type = 'reldeg'
        else 
          type = 'rellin'
        end if
        call pix2wfcg (type, i, pixmax(i), naxis, crval, crpix, 
     +                 cdelt, ctype, .false., str, ilen)
c
        if (i.lt.3) then
          write (text,70) i, str(1:ilen)
70        format ('  Axis ', i1, ': Fitted pixel offset = ', a)
        else
          write (text, 75) i, str(1:ilen)
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
      call output ('World coordinate:')
      if (logf.ne.' ') call logwrit ('World coordinate:')
      do i = 1, min(3,naxis)
        if (i.eq.ira) then
          type = 'hms'
        else if (i.eq.idec) then
          type = 'dms'
        else if (i.eq.ilat .or. i.eq.ilon) then
          type = 'absdeg'
        else
          type = 'abslin'
        end if
c
        call pix2wfcg (type, i, pixmax(i), naxis, crval, crpix, 
     +                 cdelt, ctype, .false., str, ilen)
        if (ira.eq.i .or. idec.eq.i) call pader (str, ilen)
c
        if (i.lt.3) then
          write (text,80) i, ctype(i), str(1:ilen)
80        format ('  Axis ', i1, ': Fitted ', a, ' = ', a)
        else
          write (text, 85) i, ctype(i), str(1:ilen)
85        format ('  Axis ', i1, ':        ', a, ' = ', a)
        end if
        call output (text)
        if (logf.ne.' ') call logwrit (text)
      end do
c
      if (logf.ne.' ') call logclose
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
      real z(width,width), c(6), zmax, pix(2)
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
      pix(1) = (2*c(2)*c(6)-c(3)*c(4)) / denom
      pix(2) = (2*c(3)*c(5)-c(2)*c(4)) / denom
      zmax = c(1) + c(2)*pix(1) + c(3)*pix(2) + 
     +       c(4)*pix(1)*pix(2) + c(5)*pix(1)*pix(1) +
     +       c(6)*pix(2)*pix(2)
c
      end
c
c
      subroutine pader (str, ilen)
      implicit none
      character str*(*), str2*132
      integer len1, ilen, it
c
      str2 = str
      it = index(str2,':')
      str = ' '
      str(3-it+2:) = str2(1:len1(str2))
      ilen = len1(str)
c
      end
