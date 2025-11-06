c-----------------------------------------------------------------------
c  fits.h
c-----------------------------------------------------------------------
c  Include file for fits.for.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer    ALTAZ,   EQUATOR,   NASMYTH, FIXED
      parameter (ALTAZ=0, EQUATOR=1, NASMYTH=4, FIXED=6)

      integer    uvCRVAL,   uvCDELT,   uvCRPIX
      parameter (uvCRVAL=1, uvCDELT=2, uvCRPIX=3)

      integer    uvSTOKES,   uvFREQ,   uvRA,   uvDEC
      parameter (uvSTOKES=1, uvFREQ=2, uvRA=3, uvDEC=4)

      integer MAXCONFG, MAXFREQ, MAXIF, MAXSRC, MAXTIME
      parameter (MAXCONFG=40,
     *           MAXSRC=10000,
     *           MAXFREQ=MAXWIN,
     *           MAXIF=MAXFREQ,
     *           MAXTIME=8640)

      logical   emok, inited, jok, llok, mosaic, systok, velcomp
      integer   config, findx(MAXFREQ), freqid, freqids(MAXFREQ),
     *          freqidx, mount(MAXCONFG), nants(MAXCONFG), nchan,
     *          nconfig, nfreq, nif, nsrc, sindx(MAXSRC), srcid,
     *          srcids(MAXSRC), srcidx, velsys, nwth
      real      dnu, evec, jyperk, systemp, velref, tempc(MAXTIME),
     *          pressmb(MAXTIME),relhum(MAXTIME),wind(MAXTIME),
     *          winddir(MAXTIME),wvr(MAXTIME)
      double precision antpos(3*MAXANT,MAXCONFG), ddec(MAXSRC),
     *          decapp(MAXSRC), decepo(MAXSRC), dra(MAXSRC),
     *          epoch(MAXSRC), eq, freqoff(MAXSRC*MAXIF),
     *          freqref(MAXCONFG), lat(MAXCONFG), long(MAXCONFG),
     *          raapp(MAXSRC), raepo(MAXSRC), restfreq(MAXSRC*MAXIF),
     *          sdf(MAXIF*MAXFREQ), sfreq(MAXIF*MAXFREQ),
     *          timeoff(MAXCONFG), timeref, tprev, veldop(MAXSRC),
     *          height(MAXCONFG),timewth(MAXTIME)
      character observer*16, source(MAXSRC)*20, telescop*16

      common /tables/ raepo, decepo, raapp, decapp, dra, ddec, sfreq,
     *          freqoff, restfreq, veldop, antpos, timeoff, freqref,
     *          epoch, lat, long, height, tprev, timeref, eq, sdf,
     *          timewth, tempc, pressmb, relhum, wind, winddir, wvr,
     *          evec, systemp, jyperk, velref, nsrc, nif, nchan, nfreq,
     *          dnu, nconfig, nants, srcids, freqids, srcid, freqid,
     *          srcidx, freqidx, sindx, findx, mount, velsys, config,
     *          mosaic, velcomp, llok, emok, systok, jok, inited, nwth
      common /tablesc/ observer, source, telescop
