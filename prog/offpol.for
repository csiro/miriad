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
c       Output name template.  No default.
c@ imsize
c       Two values, the image size.  Second value defaults to first.
c       Default 255.
c@ cell
c       Two values, the image cell size in RA and Dec, in arcsec.
c       Second value defaults to first.  Each defaults separately to
c       twice the primary beam FWHM divided by imsize.
c@ ra
c       Right ascension of the field centre.  (Used solely for setting
c       the output header, it has no effect on the computation.)
c       Default 0.
c@ dec
c       Declination of the field centre.  Default -45 deg.
c@ harange
c       Hour angle range to simulate - the start and end hour angles,
c       and the simulation step size.  The default is to simulate a
c       snapshot at 0 hours.  The default step size is 0.1 hours
c       (6 min) which might be inadequate for sources with a declination
c       near -30 deg.
c@ freq
c       Frequency of interest (GHz).  The default is 1.384 GHz.
c@ options
c       Task enrichment parameters. Several can be given.
c         raw      Generate images of the XX,YY,XY and YX responses,
c                  rather than the Stokes responses.
c         subtract Subtract off the circularly symmetric portion of the
c                  primary beam response from I, XX and YY responses.
c
c$Id: offpol.for,v 1.6 2011/10/11 04:04:59 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      logical    ROTATE
      parameter (ROTATE = .true.)

      real       CHIOFF
      parameter (CHIOFF = 0.25*PI)

c     Observatory latitude.
      double precision LAT
      parameter (LAT = -30d0*DD2R)

      logical   doraw, dosub, flag(MAXDIM)
      integer   coObj, i, ic, iha, ipol, j, jc, lout, nha, nx, ny,
     *          pbObj, stokes(8), toff(4)
      real      c2chi, chi, cutoff, Jones(2,2), maxrad, off(MAXDIM,4),
     *          pb, pbfwhm, psi, q, rad, s2chi, u, x, xx, xy, y, yx, yy
      double precision cdelt1, cdelt2, crpix1, crpix2, crval1, crval2,
     *          dha, freq, ha, ha0, ha1
      character stokId(8)*2, out*64, version*80

      external  len1, versan
      integer   len1
      character versan*80

      data stokes / 1,   2,   3,   4,   -5,   -6,   -7,   -8 /
      data stokId /'i', 'q', 'u', 'v', 'xx', 'yy', 'xy', 'yx'/
c-----------------------------------------------------------------------
      version = versan('offpol',
     *                 '$Revision: 1.6 $',
     *                 '$Date: 2011/10/11 04:04:59 $')

c     Get the inputs.
      call keyini

      call keya('out',out,' ')
      lout = len1(out)
      if (lout.eq.0) call bug('f','An output must be given')

      call keyi('imsize',nx,255)
      call keyi('imsize',ny,nx)
      if (nx.le.0 .or. MAXDIM.lt.nx) call bug('f','Invalid image size')
      if (ny.le.0 .or. MAXDIM.lt.ny) call bug('f','Invalid image size')

      call keyd('cell',cdelt1,0d0)
      call keyd('cell',cdelt2,cdelt1)

      call keyt('ra', crval1,'hms',0d0)
      call keyt('dec',crval2,'dms',-0.25d0*DPI)

      call keyt('harange',ha0,'hms',0d0)
      call keyt('harange',ha1,'hms',ha0)
      call keyt('harange',dha,'hms',0.1d0)
      nha = nint((ha1 - ha0)/dha) + 1

      call keyd('freq',freq,1.384d0)

      call GetOpt(doraw,dosub)
      call keyfin

      ic = nx/2 + 1
      jc = ny/2 + 1
      crpix1 = dble(ic)
      crpix2 = dble(jc)

c     Determine the FWHM of the primary beam at this frequency.
      call coCreate(3, coObj)
      call coAxSet(coObj, 1, 'RA---SIN', 0d0, 0d0, 1d0)
      call coAxSet(coObj, 2, 'DEC--SIN', 0d0, 0d0, 1d0)
      call coAxSet(coObj, 3, 'FREQ', 0d0, freq, 0.1d0*freq)
      call coReinit(coObj)
      call pbInit(pbObj,'atca',coObj)
      call pbInfo(pbObj,pbfwhm,cutoff,maxrad)

      if (cdelt1.eq.0d0) then
        cdelt1 = 2d0 * pbfwhm / real(nx)
      else
        cdelt1 = -abs(cdelt1 * DAS2R)
      endif

      if (cdelt2.eq.0d0) then
        cdelt2 = 2d0 * pbfwhm / real(ny)
      else
        cdelt2 =  abs(cdelt2 * DAS2R)
      endif

c     Open an output map for each polarization.
      do ipol = 1, 4
        i = ipol
        if (doraw) i = i + 4

        call mkopen(toff(ipol), nx, ny, crpix1, crpix2, cdelt1, cdelt2,
     *    crval1, crval2, freq, stokes(i),
     *    out(1:lout) // '.' // stokId(i), version)
      enddo

c     Loop over the map.
      do j = 1, ny
        y = (j - jc)*cdelt2

        do i = 1, nx
          do ipol = 1, 4
            off(i,ipol) = 0.0
          enddo

c         Blank the corners.
          x = (i - ic)*cdelt1
          flag(i) = sqrt(x*x + y*y).lt.pbfwhm
        enddo

        do iha = 1, nha
          ha = dha*(iha-1) + ha0
          call parang(0d0,crval2,ha,LAT,chi)
          chi = chi + CHIOFF

          if (ROTATE) then
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
              x = (i - ic)*cdelt1
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

      logical doraw, dosub
c-----------------------------------------------------------------------
      integer    NOPTS
      parameter (NOPTS=2)

      character opts(NOPTS)*8
      logical   present(NOPTS)

      data opts/'raw     ','subtract'/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)
      doraw = present(1)
      dosub = present(2)

      end

c***********************************************************************

      subroutine mkopen(tno, nx, ny, crpix1, crpix2, cdelt1, cdelt2,
     *  crval1, crval2, sfreq, stokes, name, version)

      integer   tno, nx, ny
      double precision crpix1, crpix2, cdelt1, cdelt2, crval1, crval2,
     *          sfreq
      integer   stokes
      character name*(*),version*(*)
c-----------------------------------------------------------------------
      integer nsize(4),coObj
      character line*64
c-----------------------------------------------------------------------
      call coCreate(4, coObj)
      call coAxSet(coObj,1,'RA---SIN',crpix1,crval1,cdelt1)
      call coAxSet(coObj,2,'DEC--SIN',crpix2,crval2,cdelt2)
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
