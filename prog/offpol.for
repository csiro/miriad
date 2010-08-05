      program  offpol

c= offpol -- Generate ATCA primary beam polarimetric response.
c& rjs
c: utility
c+
c       OFFPOL generates images of the primary beam response (both total
c       intensity and polarimetric) of the ATCA antennas.  It performs a
c       simple simulation of an observation.
c
c@ out
c       Output name template. No default.
c@ freq
c       Frequency of interest, in GHz. The default is 1.384 GHz.
c@ harange
c       Hour angle range to simulate. This gives the start and
c       end hour angles, and a simulation step size. The default is
c       to simulate a snapshot at 0 hours. The default step size is
c       0.1 hours (6 minutes). This might be inadequate for sources
c       with a declination near -30 degrees.
c@ dec
c       Declination of the source. The default is -45 degrees.
c@ imsize
c       The image size. The default is 255.
c@ options
c       Task enrichment parameters. Several can be given.
c         raw      Generate images of the XX,YY,XY and YX responses,
c                  rather than the Stokes responses.
c         subtract Subtract off the circularly symmetric portion of the
c                  primary beam response from I, XX and YY responses.
c
c$Id: offpol.for,v 1.4 2010/08/05 00:07:54 cal103 Exp $
c--
c  History:
c    rjs  24apr97 Original version.
c    rjs   3may00 Stripped out code for Jones matrix computation.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      logical rotate
      real    chioff
      parameter (rotate = .true.)
      parameter (chioff = 0.25*PI)

c     Observatory latitude.
      double precision lat
      parameter (lat = -30d0*DD2R)

      logical doraw, dosub, flag(MAXDIM)
      integer coObj, i, ic, iha, ipol, j, jc, lout, nha, nx, ny, pbObj,
     *        stokes(8), toff(4)
      real    c2chi, chi, cutoff, Jones(2,2), maxrad, off(MAXDIM,4), pb,
     *        pbfwhm, psi, q, rad, s2chi, u, x, xx, xy, y, yx, yy
      double precision dec, delta, dha, freq, ha, ha0, ha1
      character stokId(8)*2, out*64, version*80

c     Externals.
      integer len1
      character versan*80

      data stokes / 1,   2,   3,   4,   -5,   -6,   -7,   -8 /
      data stokId /'i', 'q', 'u', 'v', 'xx', 'yy', 'xy', 'yx'/
c-----------------------------------------------------------------------
      version = versan('offpol',
     :                 '$Revision: 1.4 $',
     :                 '$Date: 2010/08/05 00:07:54 $')
c
c  Get the inputs.
c
      call keyini
      call keya('out',out,' ')
      lout = len1(out)
      if (lout.eq.0) call bug('f','An output must be given')
      call keyd('freq',freq,1.384d0)
      call keyt('harange',ha0,'hms',0d0)
      call keyt('harange',ha1,'hms',ha0)
      call keyt('harange',dha,'hms',0.1d0)
      nha = nint((ha1 - ha0)/dha) + 1
      call keyt('dec',dec,'dms',-0.25d0*DPI)
      call keyi('imsize',nx,255)
      call keyi('imsize',ny,nx)
      if (nx.le.0 .or. ny.le.0) call bug('f','Invalid image size')

      call GetOpt(doraw,dosub)
      call keyfin

      ic = nx/2 + 1
      jc = ny/2 + 1
c
c Determine the FWHM of the primary beam at this frequency.
c
      call coRaDec(coObj,'SIN',0d0,0d0)
      call coAxSet(coObj,3,'FREQ',0d0,freq,0.1d0*freq)
      call coReinit(coObj)
      call pbInit(pbObj,'atca',coObj)
      call pbInfo(pbObj,pbfwhm,cutoff,maxrad)

      delta = 2 * pbfwhm / real(nx)

c     Open an output map for each polarization.
      do ipol = 1, 4
        i = ipol
        if (doraw) i = i + 4

        call mkopen(toff(ipol), out(1:lout) // '.' // stokId(i),
     *    stokes(i), freq, version, nx, ny, delta, dec)
      enddo

c     Loop over the map.
      do j = 1, ny
        do i = 1, nx
          do ipol = 1, 4
            off(i,ipol) = 0.0
          enddo

c         Blank the corners.
          flag(i) = sqrt(real((i-ic)**2 + (j-jc)**2)).lt.real(nx/2)
        enddo

        do iha = 1, nha
          ha = dha*(iha-1) + ha0
          call parang(0d0,dec,ha,lat,chi)
          chi = chi + chioff

          if (rotate) then
            c2chi = cos(2.0*chi)
            s2chi = sin(2.0*chi)
          else
            c2chi = 1.0
            s2chi = 0.0
          endif

          do i = 1, nx
            if (i.eq.ic .and. j.eq.jc) then
              xx = 1.0
              yy = 1.0
              xy = 0.0
              yx = 0.0
              pb = 1.0
            else
c             Compute the Jones matrix at this point.
              x = -(i - ic)*delta
              y =  (j - jc)*delta
              rad = sqrt(x*x + y*y)
              psi = atan2(x,y)
              call atjones(rad,psi-chi,freq,Jones,pb)

c             Coherence matrix.
              xx = Jones(1,1)*Jones(1,1) + Jones(1,2)*Jones(1,2)
              yy = Jones(2,1)*Jones(2,1) + Jones(2,2)*Jones(2,2)
              xy = Jones(1,1)*Jones(2,1) + Jones(1,2)*Jones(2,2)
              yx = xy
            endif

c           Subtract the primary beam response?
            if (dosub) then
              xx = xx - pb
              yy = yy - pb
            endif

            if (doraw) then
c             Generate images of the XX, YY, XY, and YX responses.
              off(i,1) = off(i,1) + xx
              off(i,2) = off(i,2) + yy
              off(i,3) = off(i,3) + xy
              off(i,4) = off(i,4) + yx

            else
c             Stokes-I.
              off(i,1) = off(i,1) + 0.5*(xx + yy)

c             Stokes-Q, and -U.
              q = 0.5*(xx - yy)
              u = 0.5*(xy + yx)
              off(i,2) = off(i,2) + q*c2chi - u*s2chi
              off(i,3) = off(i,3) + q*s2chi + u*c2chi

c             Stokes-V (zero, because xy and yx are real).
c             off(i,4) = off(i,4) + 0.5*real((0.0,-1.0)*(xy-yx))
            endif
          enddo
        enddo

        do ipol = 1, 4
          do i = 1, nx
            off(i,ipol) = off(i,ipol) / real(nha)
          enddo

          call xywrite(toff(ipol), j, off(1,ipol))
          call xyflgwr(toff(ipol), j, flag)
        enddo
      enddo

      do ipol = 1, 4
        call xyclose(toff(ipol))
      enddo

      end
c***********************************************************************
      subroutine GetOpt(doraw,dosub)

      logical doraw,dosub
c
c-----------------------------------------------------------------------
      integer NOPTS
      parameter(NOPTS=2)
      character opts(NOPTS)*8
      logical present(NOPTS)
      data opts/'raw     ','subtract'/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)
      doraw = present(1)
      dosub = present(2)
      end
c***********************************************************************
      subroutine mkopen(tno,name,stokes,sfreq,version,nx,ny,delta,dec)

      integer tno,stokes,nx,ny
      double precision sfreq,delta,dec
      character name*(*),version*(*)
c
c-----------------------------------------------------------------------
      integer nsize(4),coObj
      character line*64
c-----------------------------------------------------------------------
      call coCreate(coObj)
      call coAxSet(coObj,1,'RA---SIN',dble(nx/2+1),0d0,-delta)
      call coAxSet(coObj,2,'DEC--SIN',dble(ny/2+1),dec, delta)
      call coAxSet(coObj,3,'FREQ',    1d0,sfreq,0.1d0)
      call coAxSet(coObj,4,'STOKES',  1d0,dble(stokes),1d0)
      call coReInit(coObj)
      nsize(1) = nx
      nsize(2) = ny
      nsize(3) = 1
      nsize(4) = 1
      call xyopen(tno,name,'new',4,nsize)
      call hisopen(tno,'write')
      line = 'OFFPOL: Miriad '//version
      call hiswrite(tno,line)
      call hisinput(tno,'OFFPOL')
      call hisclose(tno)
      call coWrite(coObj,tno)
      call coFin(coObj)
      call wrhda(tno,'telescop','ATCA')

      end
