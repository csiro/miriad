      program maxfit

c= MAXFIT - Fits a 2-D parabola to a 3x3 region of an image
c& nebk
c: image analysis
c+
c       MAXFIT finds the maximum value of a region of an image.
c       This region may be three dimensional.  It then fits a
c       parabola to a 3x3 array extracted from the first two
c       dimensions of the image and centred on the maximum pixel
c       in the specified region.  MAXFIT then returns the location
c       and value of the maximum pixel and the fitted pixel.
c@ in
c       The input image.
c@ region
c       Region of interest in which to search for the maximum pixel.
c       The default is the whole image.
c@ options
c       "abs"  means that MAXFIT search for the maximum absolute
c              pixel value rather than the default, which is just
c              the maximum pixel value.
c@ log
c       Write results to this log file as well as screen.
c
c$Id: maxfit.for,v 1.6 2012/02/15 03:40:46 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'

      integer    MAXBOXES, MAXRUNS
      parameter (MAXBOXES = 2048, MAXRUNS = 3*MAXDIM)

      logical   doabs
      integer   blc(MAXNAX), boxes(MAXBOXES), i, iax, il, ip, j, k, l,
     *          lun, naxis, nruns, nsize(MAXNAX), ploc(MAXNAX),
     *          runs(3,MAXRUNS), strlen(MAXNAX), trc(MAXNAX)
      real      coeffs(6), data(MAXDIM), dd, dmax, fit(9), fmax
      double precision pixmax(MAXNAX)
      character ctype*9, infile*128, logf*132, strout(MAXNAX)*50,
     *          text*132, typesi(MAXNAX)*9, typeso(MAXNAX)*9, version*72

      external  itoaf, len1, versan
      integer   len1
      character itoaf*2, versan*72
c-----------------------------------------------------------------------
      version = versan('maxfit',
     *                 '$Revision: 1.6 $',
     *                 '$Date: 2012/02/15 03:40:46 $')

c     Get inputs.
      call keyini
      call keya('in', infile, ' ')
      if (infile.eq.' ') call bug('f', 'Input file must be given')
      call boxinput('region', infile, boxes, MAXBOXES)
      call keya('log', logf, ' ')
      call getopt(doabs)
      call keyfin

c     Open file.
      call xyopen(lun, infile, 'old', MAXNAX, nsize)
      call rdhdi(lun, 'naxis', naxis, 0)
      if (naxis.eq.0) call bug('f', 'Zero dimensions in image')
      naxis = min(3,naxis)
      if (nsize(1).gt.MAXDIM) call bug('f','Input file too big for me')

c     Set up the region of interest.
      call boxmask(lun, boxes, MAXBOXES)
      call boxset(boxes, MAXNAX, nsize, ' ')
      call boxinfo(boxes, MAXNAX, blc, trc)

c     Open log file.
      if (logf.ne.' ') call logopen(logf, ' ')

c     Read in region and find the maximum pixel therein taking account
c     of blanked pixels.
      dmax = -1e30

c     Loop over planes.
      do k = blc(3), trc(3)
        call xysetpl(lun, 1, k)
        call boxruns(1, k, ' ', boxes, runs, MAXRUNS,
     *               nruns, blc(1), trc(1), blc(2), trc(2))

        if (nruns.gt.0) then
          j = 0
          do l = 1, nruns
c           Read new row, if needed, from current plane.
            if (runs(1,l).ne.j) then
              j = runs(1,l)
              call xyread(lun, j, data)
            endif

c           Loop over all good pixels in row.
            do i = runs(2,l), runs(3,l)
              dd = data(i)
              if (doabs) dd = abs(data(i))
              if (dd.gt.dmax) then
                dmax = data(i)
                if (doabs) dmax = abs(data(i))
                ploc(1) = i
                ploc(2) = j
                ploc(3) = k
              endif
            enddo
          enddo
        endif
      enddo

c     Make sure the peak estimate is not on the edge of the image.
      do iax = 1, min(2,naxis)
        if (ploc(iax).eq.1 .or. ploc(iax).eq.nsize(1)) then
          write(text, 10) iax, ploc(iax)
 10       format('Axis ',i1,' max. pixel at ',i4,
     *           ' is too close to the image edge')
          call bug('f', text)
        endif
      enddo

c     Pick out the 3x3 region for fitting.  Blanked pixels are not
c     dealt with in this step.
      call xysetpl(lun, 1, ploc(3))
      k = 0

      do j = ploc(2)-1, ploc(2)+1
        call xyread(lun, j, data)
        do iax = ploc(1)-1, ploc(1)+1
          k = k + 1
          fit(k) = data(iax)
        enddo
      enddo
      dmax = fit(5)

c     Do the fit.
      call pkfit(fit, 3, fmax, pixmax, coeffs)

c     Change x and y to image pixel coordinates and add the z location.
      pixmax(1) = ploc(1) + pixmax(1)
      pixmax(2) = ploc(2) + pixmax(2)
      pixmax(3) = ploc(3)

c     Tell user of maxfit's endeavours.
      call output(' ')
      if (logf.ne.' ') call logwrit(' ')

c     Peak pixel location and value.
      text = 'Peak pixel   : ('
      ip = len1(text) + 1
      do iax = 1, naxis
        call strfi(ploc(iax), '(i4)', text(ip:), il)
        ip = ip + il
        text(ip:ip) = ','
        ip = ip + 1
      enddo
      text(ip-1:) = ') = '
      ip = len1(text) + 2

      call strfr(dmax, '(1pe12.4)', text(ip:), il)
      call  output(text)
      if (logf.ne.' ') call logwrit(text)

c     Fitted location and fitted value.
      call output(' ')
      if (logf.ne.' ') call logwrit(' ')
      text = 'Fitted pixel : ('
      ip = len1(text) + 1
      do iax = 1, naxis
        if (iax.le.2) then
          call strfd(pixmax(iax), '(f7.2)', text(ip:), il)
        else
          call strfi(ploc(iax), '(i4)', text(ip:), il)
        endif
        ip = ip + il
        text(ip:ip) = ','
        ip = ip + 1
      enddo
      text(ip-1:) = ') = '
      ip = len1(text) + 2

      call strfr(fmax, '(1pe12.4)', text(ip:), il)
      call  output(text)
      if (logf.ne.' ') call logwrit(text)

c     Find offsets of fitted pixel from reference pixel.
      call coInit(lun)

      call output(' ')
      if (logf.ne.' ') call logwrit(' ')
      call output('Offsets from reference pixel :')
      if (logf.ne.' ') call logwrit('Offsets from reference pixel :')

c     Convert coordinates.
      do iax = 1, naxis
        typesi(iax) = 'abspix'
      enddo
      call setoaco(lun, 'off', naxis, 0, typeso)
      call w2wfco(lun, naxis, typesi, ' ', pixmax, typeso, ' ',
     *             .false., strout, strlen)

c     Tell user.
      do iax = 1, naxis
        if (iax.lt.3) then
          write(text,20) iax, strout(iax)(1:strlen(iax))
 20       format('  Axis ',i1,': Fitted pixel offset = ',a)
        else
          write(text, 30) iax, strout(iax)(1:strlen(iax))
 30       format('  Axis ',i1,':        pixel offset = ',a)
        endif
        call output(text)
        if (logf.ne.' ') call logwrit(text)
      enddo

c     Compute world coordinate.
      call output(' ')
      if (logf.ne.' ') call logwrit(' ')
      call output('Coordinate:')
      if (logf.ne.' ') call logwrit('Coordinate:')

c     Convert coordinates.
      call setoaco(lun, 'abs', naxis, 0, typeso)
      call w2wfco(lun, naxis, typesi, ' ', pixmax, typeso, ' ',
     *            .false., strout, strlen)

c     Tell user.
      do iax = 1, naxis
        call pader(typeso(iax), strout(iax), strlen(iax))

        call coGetA(lun, 'ctype'//itoaf(iax), ctype)
        if (ctype.eq.' ') ctype = 'Axis '//itoaf(iax)

        if (iax.lt.3) then
          write(text,40) iax, ctype, strout(iax)(1:strlen(iax))
 40       format('  Axis ',i1,': Fitted ',a,' = ',a)
        else
          write(text,50) iax, ctype, strout(iax)(1:strlen(iax))
 50       format('  Axis ',i1,':        ',a,' = ',a)
        endif

        call output(text)
        if (logf.ne.' ') call logwrit(text)
      enddo

      call xyclose(lun)
      if (logf.ne.' ') call logclose
      call coFin(lun)

      end

c***********************************************************************

      subroutine pader(type, str, ilen)

      character type*(*), str*(*)
      integer   ilen
c-----------------------------------------------------------------------
      integer   it
      character str2*132

      external  len1
      integer   len1
c-----------------------------------------------------------------------
      if (type.eq.'hms' .or. type.eq.'dms') then
        str2 = str
        it = index(str2,':')
        str = ' '
        str(3-it+2:) = str2(1:len1(str2))
        ilen = len1(str)
      endif

      end

c***********************************************************************

      subroutine getopt(doabs)

      logical   doabs
c-----------------------------------------------------------------------
c  Decode options array into named variables.
c
c  Output:
c     doabs     Search for absolute maximum pixel
c-----------------------------------------------------------------------
      integer    MAXOPT
      parameter (MAXOPT = 1)

      logical   present(MAXOPT)
      character opshuns(MAXOPT)*3

      data opshuns /'abs'/
c-----------------------------------------------------------------------
      call options('options', opshuns, present, MAXOPT)

      doabs = present(1)

      end
