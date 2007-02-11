	include 'maxdim.h'
c
	integer EQUATOR,ALTAZ,NASMYTH
	parameter(EQUATOR=1,ALTAZ=0,NASMYTH=4)
c
	integer uvCrval,uvCdelt,uvCrpix
	integer uvStokes,uvFreq,uvRa,uvDec
	parameter(uvCrval=1,uvCdelt=2,uvCrpix=3)
	parameter(uvStokes=1,uvFreq=2,uvRa=3,uvDec=4)
c
	integer MAXSRC,MAXIF,MAXFREQ,MAXCONFG
	parameter(MAXSRC=1000,MAXFREQ=MAXWIN,MAXIF=MAXFREQ,MAXCONFG=40)
c
	double precision raepo(MAXSRC),decepo(MAXSRC)
	double precision raapp(MAXSRC),decapp(MAXSRC)
	double precision dra(MAXSRC),ddec(MAXSRC)
	double precision sfreq(MAXIF*MAXFREQ)
	double precision freqoff(MAXSRC*MAXIF),epoch(MAXSRC)
	double precision freqref(MAXCONFG)
	double precision restfreq(MAXSRC*MAXIF),veldop(MAXSRC)
	double precision antpos(3*MAXANT,MAXCONFG)
	double precision timeref,eq,timeoff(MAXCONFG)
	double precision lat(MAXCONFG),long(MAXCONFG),Tprev
	integer mount(MAXCONFG),nants(MAXCONFG),velsys,config
	real evec,systemp,jyperk,velref
	logical llok,emok,systok,jok,mosaic,velcomp,inited
c
	real sdf(MAXIF*MAXFREQ),dnu
	integer nsrc,nif,nchan,nfreq,nconfig
	integer srcids(MAXSRC),freqids(MAXFREQ),srcid,srcidx
	integer freqid,freqidx
	integer sindx(MAXSRC),findx(MAXFREQ)
	character source(MAXSRC)*20
c
	character observer*16,telescop*12
c
	common/Tables/raepo,decepo,raapp,decapp,dra,ddec,sfreq,freqoff,
     *	    restfreq,veldop,antpos,timeoff,freqref,epoch,lat,long,Tprev,
     *	    timeref,eq,
     *	  sdf,dnu,evec,systemp,jyperk,velref,
     *	  nsrc,nif,nchan,nfreq,nconfig,nants,srcids,freqids,
     *	    srcid,freqid,srcidx,freqidx,sindx,findx,mount,velsys,config,
     *	  mosaic,velcomp,llok,emok,systok,jok,inited
	common/TablesC/source,observer,telescop
