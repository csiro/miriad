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
c$Id: offpol.for,v 1.3 2010/08/03 08:26:52 cal103 Exp $
c--
c  History:
c    rjs  24apr97 Original version.
c    rjs   3may00 Stripped out code for Jones matrix computation.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      logical rotate
      real    chioff
      double precision lat
      parameter (rotate = .true.)
      parameter (chioff = 0.25*PI, lat = -30d0*DD2R)

      logical doraw, dosub, flag(MAXDIM)
      integer coObj, i, ic, iha, j, jc, lout, nha, nx, ny, pbObj, tiir,
     *        tqqr, tuur, tvvr
      real    chi, cutoff, iir(MAXDIM), jo(2,2), maxrad, pb, pbfwhm,
     *        psi, qq, qqr(MAXDIM), rad, uu, uur(MAXDIM), vvr(MAXDIM),
     *        x, xx, xy, y, yx, yy
      double precision dec, delta, dha, freq, ha, ha0, ha1
      character out*64, version*80

c     Externals.
      integer len1
      character versan*80
c-----------------------------------------------------------------------
      version = versan('offpol',
     :                 '$Revision: 1.3 $',
     :                 '$Date: 2010/08/03 08:26:52 $')
c
c  Get the inputs.
c
      call keyini
      call keya('out',out,' ')
      lout = len1(out)
      if(lout.eq.0)call bug('f','An output must be given')
      call keyd('freq',freq,1.384d0)
      call keyt('harange',ha0,'hms',0.d0)
      call keyt('harange',ha1,'hms',ha0)
      call keyt('harange',dha,'hms',0.1d0)
      nha = nint((ha1 - ha0)/dha) + 1
      call keyt('dec',dec,'dms',-0.25d0*DPI)
      call keyi('imsize',nx,255)
      call keyi('imsize',ny,nx)
      if(nx.le.0.or.ny.le.0)call bug('f','Invalid image size')

      call GetOpt(doraw,dosub)
      call keyfin

      ic = nx/2 + 1
      jc = ny/2 + 1
c
c Determine the FWHM of the primary beam at this frequency.
c
      call coRaDec(coObj,'SIN',0.d0,0.d0)
      call coAxSet(coObj,3,'FREQ',0.d0,freq,0.1d0*freq)
      call coReinit(coObj)
      call pbInit(pbObj,'atca',coObj)
      call pbInfo(pbObj,pbfwhm,cutoff,maxrad)

      delta = 2 * pbfwhm / nx

      if(doraw)then
        call mkopen(tiir,out(1:lout)//'.xx',-5,freq,version,nx,ny,
     *                                                delta,dec)
        call mkopen(tqqr,out(1:lout)//'.yy',-6,freq,version,nx,ny,
     *                                                delta,dec)
        call mkopen(tuur,out(1:lout)//'.xy',-7,freq,version,nx,ny,
     *                                                delta,dec)
        call mkopen(tvvr,out(1:lout)//'.yx',-8,freq,version,nx,ny,
     *                                                delta,dec)
      else
        call mkopen(tiir,out(1:lout)//'.i',1,freq,version,nx,ny,
     *                                                delta,dec)
        call mkopen(tqqr,out(1:lout)//'.q',2,freq,version,nx,ny,
     *                                                delta,dec)
        call mkopen(tuur,out(1:lout)//'.u',3,freq,version,nx,ny,
     *                                                delta,dec)
        call mkopen(tvvr,out(1:lout)//'.v',4,freq,version,nx,ny,
     *                                                delta,dec)
      endif

      do j=1,ny
        do i=1,nx
          iir(i) = 0.0
          qqr(i) = 0.0
          uur(i) = 0.0
          vvr(i) = 0.0
          flag(i) = sqrt(real((i-ic)**2 + (j-jc)**2)).lt.nx/2
        enddo

        do iha=1,nha
          ha = dha*(iha-1) + ha0
          call parang(0d0,dec,ha,lat,chi)
          chi = chi + chioff
          do i=1,nx
            if(i.ne.ic.or.j.ne.jc)then
              x = -(i - ic)*delta
              y = (j - jc)*delta
              rad = sqrt(x**2 + y**2)
              psi = atan2(x,y)
              call atjones(rad,psi-chi,freq,Jo,pb)
              XX = Jo(1,1)*Jo(1,1) + Jo(1,2)*Jo(1,2)
              XY = Jo(1,1)*Jo(2,1) + Jo(1,2)*Jo(2,2)
              YX = XY
              YY = Jo(2,1)*Jo(2,1) + Jo(2,2)*Jo(2,2)
            else
              XX = 1.0
              XY = 0.0
              YX = 0.0
              YY = 1.0
              pb = 1.0
            endif

            if(doraw)then
              if(dosub)then
                iir(i) = iir(i) + xx - pb
                qqr(i) = qqr(i) + yy - pb
              else
                iir(i) = iir(i) + xx
                qqr(i) = qqr(i) + yy
              endif
              uur(i) = uur(i) + xy
              vvr(i) = vvr(i) + yx
            else
              iir(i) = iir(i) + 0.5*(xx + yy)
              if(dosub)iir(i) = iir(i) - pb
              qq = 0.5*(xx - yy)
              uu = 0.5*(xy + yx)
              if(rotate)then
                qqr(i) = qqr(i) + qq*cos(2*chi) - uu*sin(2*chi)
                uur(i) = uur(i) + qq*sin(2*chi) + uu*cos(2*chi)
              else
                qqr(i) = qqr(i) + qq
                uur(i) = uur(i) + uu
              endif
              vvr(i) = vvr(i) + 0.5*real((0.0,-1.0)*(xy-yx))
            endif
          enddo
        enddo

        do i=1,nx
          iir(i) = iir(i) / nha
          qqr(i) = qqr(i) / nha
          uur(i) = uur(i) / nha
          vvr(i) = vvr(i) / nha
        enddo

        call xywrite(tiir,j,iir)
        call xyflgwr(tiir,j,flag)
        call xywrite(tqqr,j,qqr)
        call xyflgwr(tqqr,j,flag)
        call xywrite(tuur,j,uur)
        call xyflgwr(tuur,j,flag)
        call xywrite(tvvr,j,vvr)
        call xyflgwr(tvvr,j,flag)
      enddo

      call xyclose(tiir)
      call xyclose(tqqr)
      call xyclose(tuur)
      call xyclose(tvvr)

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
      call coAxSet(coObj,1,'RA---SIN',dble(nx/2+1),0.d0,-delta)
      call coAxSet(coObj,2,'DEC--SIN',dble(ny/2+1),dec, delta)
      call coAxSet(coObj,3,'FREQ',    1.d0,sfreq,0.1d0)
      call coAxSet(coObj,4,'STOKES',  1.d0,dble(stokes),1.d0)
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
