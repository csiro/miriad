      program mosgen

c= mosgen - Generate a mosaic file for the on-line system or for UVGEN.
c& rjs
c: uv analysis
c+
c       MOSGEN generates a file containing mosaic pointing centres.  The
c       output format may be chosen either for the ATCA on-line system
c       or UVGEN.
c@ radec
c       RA and DEC of the centre of the mosaic in hh:mm:ss,dd:mm:ss
c       format, or decimal hours and degrees.  The default is 0,0.
c@ width
c       Width of the field to mosaic, in degrees.  One or two values can
c       be given, being the width in RA and DEC.  Default is 0.1 deg.
c@ freq
c       The observing frequency, in GHz.  Default is 1.4 GHz.
c@ device
c       Standard PGPLOT plotting device to display a diagram of the
c       pointing centres relative to the mosaic centre.  Default is not
c       to produce a plot.
c@ log
c       Output file listing the pointing centres.  Default is the
c       terminal.
c@ mode
c       Format of the output file:
c         atmosaic  Mosaic file format understood by the ATCA on-line
c                   system.
c
c                   NOTE: The ATCA mosaic files produced by mosgen have
c                   the offsets referenced to the pointing in the lower
c                   left corner (earliest RA, least DEC value).  That
c                   is, it is not the centre of the mosaic given by the
c                   user.  The coordinate of the reference pointing is
c                   given as a comment in the head of the file.  The
c                   reason for the use of the lower left is to minimise
c                   drive times and to pixel the reference as the first
c                   point in the mosaic that rises. Note the ATCA on-
c                   line system has a limit of 2048 pointing centers.
c
c         uvgen     Format required for UVGEN's "center" keyword.
c@ telescop
c       Primary beam type.  Default is ATCA.
c@ name
c       For mode=atmosaic, a name used to derive the pointing name.  For
c       example, using name=lmc, MOSGEN will generate pointing names of
c       lmc_1, lmc_2, etc.  The full name must be 9 characters or less.
c@ cycles
c       For mode=atmosaic, the number of cycles spent on each pointing.
c
c$Id: mosgen.for,v 1.4 2011/10/11 03:44:31 cal103 Exp $
c--
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer    MAXPNT, NMODES
      parameter (MAXPNT = 9999, NMODES = 2)

      logical   first, more
      integer   cycles, i, j, l, l2, lu, nout, npnt, nx, ny, pbObj, s
      real      cutoff, freq, h, maxrad, pbfwhm, v, widthx, widthy,
     *          x(MAXPNT), y(MAXPNT)
      double precision dec, ra, x1(2), x2(2)
      character device*64, line*16, line1*80, logf*80, mode*8,
     *          modes(NMODES)*8, name*12, num*6, telescop*16, version*72

      external  hangleh, itoaf, len1, pgbeg, rangleh, stcat, versan
      integer   len1, pgbeg
      character hangleh*32, itoaf*4, rangleh*32, stcat*70, versan*72

      data modes /'atmosaic', 'uvgen   '/
c-----------------------------------------------------------------------
      version = versan('mosgen',
     *                 '$Revision: 1.4 $',
     *                 '$Date: 2011/10/11 03:44:31 $')

      call keyini
      call keya('device',device,' ')
      call keyt('radec',ra,'hms',0d0)
      call keyt('radec',dec,'dms',0d0)
      call keyr('width',widthx,0.1)
      call keyr('width',widthy,widthx)
      widthx = 0.5*widthx*D2R
      widthy = 0.5*widthy*D2R
      call keyr('freq',freq,1.4)
      call keya('name',name,'pnt')
      call keyi('cycles',cycles,3)
      call keymatch('mode',NMODES,modes,1,mode,nout)
      if (nout.eq.0) mode = modes(1)
      call keya('log',logf,' ')
      call keya('telescop',telescop,'atca')
      call keyfin

      l = len1(name)

      call logOpen(logf,' ')

      call coRaDec(lu,'SIN',ra,dec)
      call coAxSet(lu,3,'FREQ',1d0,dble(freq),0.1d0*dble(freq))
      call coReinit(lu)
      call pbInit(pbObj,telescop,lu)
      call pbInfo(pbObj,pbfwhm,cutoff,maxrad)

      h = 0.566*pbfwhm*cos(PI/3.0)
      v = 0.566*pbfwhm*sin(PI/3.0)
      nx = widthx/h
      ny = widthy/v

      i = -nx
      j = -ny
      s =   1
      npnt = 0
      first = .true.
      more = .true.
      do while (more)
        x1(1) = i*h
        x1(2) = j*v
        call CoCvt(lu,'op/op',x1,'aw/aw',x2)
        call CoCvt(lu,'aw/aw',x2,'ow/ow',x1)

        if (first) then
          ra = x2(1)
          dec = x2(2)
          line1 = stcat('# Reference position is '//hangleh(x2(1)),
     *                                         ','//rangleh(x2(2)))
          call logWrite(line1,more)
          call logInput('mosgen')
          first = .false.
        endif

        npnt = npnt + 1
        if (npnt.gt.MAXPNT) call bug('f','Too many pointings')
        x(npnt) = x1(1)*R2D
        y(npnt) = x1(2)*R2D

        x2(1) = x2(1) - ra
        x2(2) = x2(2) - dec
        if (mode.eq.'atmosaic') then
          if (x2(1).gt. PI) x2(1) = x2(1) - TWOPI
          if (x2(1).lt.-PI) x2(1) = x2(1) + TWOPI
          write(line,'(2f8.4)') x2(1)*R2D, x2(2)*R2D

          num = '_'//itoaf(npnt)
          l2 = len1(num)
          if (l2+l.gt.9) then
            call bug('w','Field name truncated to 9 characters')
            l=9-l2
          endif
          line1 = line//' '//itoaf(cycles)//' $'//
     *                                name(1:l)//'_'//itoaf(npnt)
        else
          write(line1,'(2f10.1)') x1(1)*R2AS, x1(2)*R2AS
        endif

        call logwrite(line1,more)
        if (j*s.eq.ny) then
          if (i.eq.nx) then
            more = .false.
          else
            i = i + 2
            s = -s
          endif
        else
          j = j + s
          if (2*(j/2).eq.j) then
            i = i - 1
          else
            i = i + 1
          endif
        endif
      enddo

      if (device.ne.' ') then
        if (pgbeg(0,device,1,1).ne.1) then
          call pgldev
          call bug('f','Error opening graphics device')
        endif

        call pgscf(2)
        call pgpage
        call pgvstd
        call setwidth(npnt,x,y)
        call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
        call pglab('RA offset (degrees)', 'DEC offset (degrees)', ' ')
        call pgpt(npnt,x,y,1)
        call pgend
      endif

      call output('Total number of pointings: '//itoaf(npnt))
      call logClose

      end

c***********************************************************************

      subroutine setwidth(NPNT,x,y)

      integer NPNT
      real    x(NPNT), y(NPNT)
c-----------------------------------------------------------------------
      integer i
      real    del, xmax, xmin, ymax, ymin
c-----------------------------------------------------------------------
      xmin = x(1)
      xmax = xmin
      ymin = y(1)
      ymax = ymin

      do i = 2, NPNT
        xmin = min(xmin,x(i))
        xmax = max(xmax,x(i))
        ymin = min(ymin,y(i))
        ymax = max(ymax,y(i))
      enddo

      del = 0.05*max(xmax-xmin,ymax-ymin)
      call pgwnad(xmax+del,xmin-del,ymin-del,ymax+del)

      end
