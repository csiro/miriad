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
c@ bw
c       Bandwidth, in GHz.  The response is averaged over the bandwidth.
c       The default is 0. An optional second parameter specifies how
c       many intervals in frequency to use for the average. The default
c       for this is 10. Steps may be visible at the edge of the beam.
c@ options
c       Extra processing options:
c         derivative  Plot the derivative with frequency of the primary
c                     beam (rather than the normal primary beam).
c         alpha       Plot the spectral index of the beam
c         frequency   Plot the effective frequency. This is only
c                     useful for non zero bw.
c@ device
c       PGPLOT device.  Default is no plot.
c@ log
c       Log file for listing.  Default is no log file.
c
c$Id: pbplot.for,v 1.6 2013/10/24 01:03:53 wie017 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer    MAXTEL, NPTS
      parameter (MAXTEL = 8, NPTS = 256)

      logical   doder, more, doalpha, dofreq
      integer   coObj, i, j, k, length, ntel, pbObj(MAXTEL),nbw
      real      bw, b, cutoff, freq, maxrad, pbfwhm, x(NPTS), xhi, xlo,
     *          xmax, xmin, y(NPTS,MAXTEL), yhi, ylo, ymax, ymin, xs, 
     *          pb, wt
      character device*64, line*64, logf*64, telescop(MAXTEL)*16,
     *          title*32, version*72

      external  len1, pbder, pbget, pgbeg, versan
      integer   len1, pgbeg
      real      pbder, pbget
      character versan*72
c-----------------------------------------------------------------------
      version = versan('pbplot',
     *                 '$Revision: 1.6 $',
     *                 '$Date: 2013/10/24 01:03:53 $')

c     Get input parameters.
      call keyini
      call mkeya('telescop',telescop,MAXTEL,ntel)
      call keyr('freq',freq,1.4)
      call keyr('bw',bw,0.0)
      call keyi('bw',nbw,10)
      nbw = 2 *(nbw/2)
      if (freq.le.0.0) call bug('f','Invalid frequency')
      call keya('device',device,' ')
      call keya('log',logf,' ')
      call GetOpt(doder,doalpha,dofreq)
      if (dofreq.and.bw.le.0.) call bug('f','Freq is constant for bw=0')
      call keyfin

      if (ntel.eq.0) then
c       No telescopes were given, just list the possibilities.
        call pbList
        goto 999
      endif

c     Create a simple coordinate object.
      call coCreate(3, coObj)
      call coAxSet(coObj, 1, 'RA---SIN', 0d0, 0d0, 1d0)
      call coAxSet(coObj, 2, 'DEC--SIN', 0d0, 0d0, 1d0)
      call coAxSet(coObj, 3, 'FREQ', 0d0, dble(freq), 0.1d0*dble(freq))
      call coReinit(coObj)
      
      b=bw
      if (doder.or.doalpha.or.dofreq) b=0

c     Create the primary beam objects.
      xmax = 0.0
      do j = 1, ntel
        call pbInitb(pbObj(j),telescop(j),coObj,b)
        call pbInfo(pbObj(j),pbfwhm,cutoff,maxrad)

c       Average over bandwidth.
        maxrad = maxrad * freq / sqrt((freq + bw/2.0)*(freq - bw/2.0))

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
            if (bw.gt.0) then
              y(i,j)=0
              do k=-nbw/2,nbw/2
                xs = x(i)*(freq+k*bw/nbw)/freq
                y(i,j) = y(i,j) + pbDer(pbObj(j),xs,0.0)
              enddo
              y(i,j) = y(i,j)/(nbw+1)
            else
              y(i,j) = pbDer(pbObj(j),x(i),0.0)
            endif
          else if (doalpha.or.dofreq) then
            y(i,j)=0
            if (bw.gt.0) then
              wt = 0
              do k=-nbw/2,nbw/2
                 xs = x(i)*(freq+k*bw/nbw)/freq
                 pb = pbGet(pbObj(j),xs,0.0)
                 if (pb.gt.0) then
                   if (dofreq) then
                     y(i,j) = y(i,j)+ freq+k*bw/nbw
                   else
                     y(i,j) = y(i,j)+ freq*pbDer(pbObj(j),xs,0.0)/pb
                   endif
                   wt = wt + 1   
                 endif             
              enddo
              y(i,j) = y(i,j)/wt
            else 
              pb = pbGet(pbObj(j),x(i),0.0)
              if (pb.gt.0) y(i,j) = freq*pbDer(pbObj(j),x(i),0.0)/pb                 
            endif
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
        else if (doalpha) then
          call pglab('Radial Distance (arcmin)',
     *                'Primary Beam Spectral Index',title(1:length))
        else if (dofreq) then
          call pglab('Radial Distance (arcmin)',
     *                'Effective Frequency (MHz)',title(1:length))
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

      subroutine GetOpt(doder,doalpha,dofreq)

      logical doder,doalpha,dofreq
c-----------------------------------------------------------------------
c  Get processing options.
c-----------------------------------------------------------------------
      integer    NOPTS
      parameter (NOPTS = 3)

      logical   present(NOPTS)
      character opts(NOPTS)*10

      data opts /'derivative','alpha     ','frequency '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)
      doder = present(1)
      doalpha = present(2)
      dofreq = present(3)
      if (doder.and.doalpha) call bug('f','Please specify one option')
      if (doder.and.dofreq) call bug('f','Please specify one option')
      if (dofreq.and.doalpha) call bug('f','Please specify one option')

      end
