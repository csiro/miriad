c-----------------------------------------------------------------------
c  fits.h
c-----------------------------------------------------------------------
c  Include file for fits.for.
c
c  $Id: fits.h,v 1.8 2021/12/21 22:54:58 wie017 Exp $
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer    ALTAZ,   EQUATOR,   NASMYTH, FIXED
      parameter (ALTAZ=0, EQUATOR=1, NASMYTH=4, FIXED=6)

      integer    uvCRVAL,   uvCDELT,   uvCRPIX
      parameter (uvCRVAL=1, uvCDELT=2, uvCRPIX=3)

      integer    uvSTOKES,   uvFREQ,   uvRA,   uvDEC
      parameter (uvSTOKES=1, uvFREQ=2, uvRA=3, uvDEC=4)

      integer MAXCONFG, MAXFREQ, MAXIF, MAXSRC
      parameter (MAXCONFG=40,
     *           MAXSRC=10000,
     *           MAXFREQ=MAXWIN,
     *           MAXIF=MAXFREQ)

      logical   emok, inited, jok, llok, mosaic, systok, velcomp
      integer   config, findx(MAXFREQ), freqid, freqids(MAXFREQ),
     *          freqidx, mount(MAXCONFG), nants(MAXCONFG), nchan,
     *          nconfig, nfreq, nif, nsrc, sindx(MAXSRC), srcid,
     *          srcids(MAXSRC), srcidx, velsys
      real      dnu, evec, jyperk, systemp, velref
      double precision antpos(3*MAXANT,MAXCONFG), ddec(MAXSRC),
     *          decapp(MAXSRC), decepo(MAXSRC), dra(MAXSRC),
     *          epoch(MAXSRC), eq, freqoff(MAXSRC*MAXIF),
     *          freqref(MAXCONFG), lat(MAXCONFG), long(MAXCONFG),
     *          raapp(MAXSRC), raepo(MAXSRC), restfreq(MAXSRC*MAXIF),
     *          sdf(MAXIF*MAXFREQ), sfreq(MAXIF*MAXFREQ),
     *          timeoff(MAXCONFG), timeref, tprev, veldop(MAXSRC),
     *		height(MAXCONFG)
      character observer*16, source(MAXSRC)*20, telescop*16

      common /tables/ raepo, decepo, raapp, decapp, dra, ddec, sfreq,
     *          freqoff, restfreq, veldop, antpos, timeoff, freqref,
     *          epoch, lat, long, height, tprev, timeref, eq, sdf, dnu,
     *		evec, systemp, jyperk, velref, nsrc, nif, nchan, nfreq,
     *          nconfig, nants, srcids, freqids, srcid, freqid, srcidx,
     *          freqidx, sindx, findx, mount, velsys, config, mosaic,
     *          velcomp, llok, emok, systok, jok, inited
      common /tablesc/ observer, source, telescop
