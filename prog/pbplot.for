      program pbplot

c= pbplot -- Plot primary beam shapes.
c& rjs
c: utility
c+
c       PBPLOT plots the primary beam function.
c@ telescop
c       Used to determine the type of the primary beam.  Several values
c       can be given.  Normally this will simply be a telescope name.
c@ freq
c       Frequency at which to determine the primary beam, in GHz.
c       Default is 1.4 GHz.
c@ bw   Bandwidth, in GHz.  The response is averaged over the bandwidth.
c       The default is 0.
c@ options
c       Extra processing options:
c         derivative  Plot the derivative with frequency of the primary
c                     beam (rather than the normal primary beam).
c@ device
c       PGPLOT device.  Default is no plot.
c@ log
c       Log file for listing.  Default is no log file.
c
c$Id: pbplot.for,v 1.3 2011/10/11 04:40:27 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer    MAXTEL, NPTS
      parameter (MAXTEL = 8, NPTS = 256)

      logical   doder, more
      integer   coObj, i, j, length, ntel, pbObj(MAXTEL)
      real      bw, cutoff, freq, maxrad, pbfwhm, x(NPTS), xhi, xlo,
     *          xmax, xmin, y(NPTS,MAXTEL), yhi, ylo, ymax, ymin
      character device*64, line*64, logf*64, telescop(MAXTEL)*16,
     *          title*32, version*72

      external  len1, pbder, pbget, pgbeg, versan
      integer   len1, pgbeg
      real      pbder, pbget
      character versan*72
c-----------------------------------------------------------------------
      version = versan('pbplot',
     *                 '$Revision: 1.3 $',
     *                 '$Date: 2011/10/11 04:40:27 $')

c     Get input parameters.
      call keyini
      call mkeya('telescop',telescop,MAXTEL,ntel)
      call keyr('freq',freq,1.4)
      call keyr('bw',bw,0.0)
      if (freq.le.0.0) call bug('f','Invalid frequency')
      call keya('device',device,' ')
      call keya('log',logf,' ')
      call GetOpt(doder)
      call keyfin

      if (ntel.eq.0) then
c       No telescopes were given, just list the possibilities.
        call pbList
        goto 999
      endif

c     Create a simple coorindate object.
      call coRaDec(coObj,'SIN',0d0,0d0)
      call coAxSet(coObj,3,'FREQ',0d0,dble(freq),0.1d0*dble(freq))
      call coReinit(coObj)

c     Create the primary beam objects.
      xmax = 0.0
      do j = 1, ntel
        call pbInitb(pbObj(j),telescop(j),coObj,bw)
        call pbInfo(pbObj(j),pbfwhm,cutoff,maxrad)
        maxrad = maxrad / sqrt((1.0 + bw/2.0/freq)*(1.0 - bw/2.0/freq))

c       Issue messages.
        call output('Primary beam: '//telescop(j))
        write(line,10) pbfwhm*R2D*60.0
  10    format('  FWHM (arcmin):',f7.2)

        call output(line)
        write(line,20) maxrad*R2D*60.0
  20    format('  Cutoff Radius:',f7.2)

        call output(line)
        write(line,30) cutoff
  30    format('  Cutoff Value: ',f6.3)
        call output(line)

c       Maximum value of X to plot.
        xmax = max(xmax, maxrad)
        if (j.eq.1) then
          title = 'Telescopes: '//telescop(1)
        else
          title(length+1:) = ','//telescop(j)
        endif
        length = len1(title)
      enddo

c     Evaluate the primary beams at NPTS points.
      do i = 1, NPTS
        x(i) = xmax/real(NPTS-1) * (i-1)
      enddo

      do j = 1, ntel
        do i = 1, NPTS
          if (doder) then
            y(i,j) = pbDer(pbObj(j),x(i),0.0)
          else
            y(i,j) = pbGet(pbObj(j),x(i),0.0)
          endif
        enddo
        call pbFin(pbObj(j))
      enddo
      call coFin(coObj)

c     Determine min and max values.
      xmin = 0.0
      xmax = xmax*R2D*60.0
      do i = 1, NPTS
        x(i) = x(i)*R2D*60.0
      enddo

      ymin = y(1,1)
      ymax = ymin
      do j = 1, ntel
        do i = 1, NPTS
          ymax = max(ymax,y(i,j))
          ymin = min(ymin,y(i,j))
        enddo
      enddo

      call pgrnge(xmin,xmax,xlo,xhi)
      call pgrnge(ymin,ymax,ylo,yhi)

c     Do the plotting.
      if (device.ne.' ') then
        if (pgbeg(0,device,1,1).ne.1) then
          call pgldev
          call bug('f','Error opening graphics device')
        endif

        call pgpage
        call pgvstd
        call pgswin(xlo,xhi,ylo,yhi)
        call pgtbox('BCNST',0.0,0,'BCNST',0.0,0)
        call pgsls(1)

        do j = 1, ntel
          call pgline(NPTS,x,y(1,j))
        enddo

        if (doder) then
          call pglab('Radial Distance (arcmin)',
     *                'Primary Beam Derivative',title(1:length))
        else
          call pglab('Radial Distance (arcmin)',
     *                'Primary Beam Response',title(1:length))
        endif

        call pgend

      endif

      if (logf.ne.' ') then
        call logopen(logf,' ')
        do j = 1, ntel
          do i = 1, NPTS
            write(line,'(1p2e15.8)') x(i),y(i,j)
            call logwrite(line,more)
          enddo
        enddo
        call logclose
      endif

 999  continue
      end

c***********************************************************************

      subroutine GetOpt(doder)

      logical doder
c-----------------------------------------------------------------------
c  Get processing options.
c-----------------------------------------------------------------------
      integer    NOPTS
      parameter (NOPTS = 1)

      logical   present(NOPTS)
      character opts(NOPTS)*10

      data opts /'derivative'/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)
      doder = present(1)

      end
