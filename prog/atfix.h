	include 'maxdim.h'
	integer MAXFREQ
	parameter(MAXFREQ=1000)
	integer ifreq,nfreq,vfreq
	integer nchan(MAXFREQ),nspect(MAXFREQ)
	integer nschan(MAXWIN,MAXFREQ)
	double precision sfreqs(MAXWIN,3,MAXFREQ)
	common/atfreq/sfreqs,ifreq,nfreq,vfreq,nchan,nspect,nschan
c
	
	integer MAXTCAL,TMNONE,TMCONST,TMEXTRAP,TMINTERP
	parameter(TMNONE=1,TMCONST=2,TMEXTRAP=3,TMINTERP=4)
c       allow for 5 days worth of syscal records, at one every 10sec.
	parameter(MAXTCAL=5*24*60*60/10)
	double precision ttime(MAXTCAL)
	integer tfreq(MAXTCAL),ntcal,vtcal1,vtcal2,nants,t1,t2,tmode
	real xtrec(MAXANT,MAXWIN,MAXTCAL),ytrec(MAXANT,MAXWIN,MAXTCAL)
	real xtsys(MAXANT,MAXWIN,MAXTCAL),ytsys(MAXANT,MAXWIN,MAXTCAL)
	real xtcur(MAXANT,MAXWIN),ytcur(MAXANT,MAXWIN)
	logical doatm,tvalid(MAXTCAL)
	common /attcal/ttime,tfreq,ntcal,nants,t1,t2,tmode,
     *		vtcal1,vtcal2,doatm,
     *		tvalid,xtsys,ytsys,xtrec,ytrec,xtcur,ytcur
c
