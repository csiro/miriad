c***********************************************************************
	include 'maxdim.h'
	include 'mem.h'
	integer MAXPOL,MAXBINS,MAXSLOT
	parameter(MAXPOL=4,MAXBINS=64,MAXSLOT=1000)
	integer slot(MAXPOL,MAXBASE)
	integer pFlags(MAXBINS,MAXSLOT),pData(MAXBINS,MAXSLOT)
	integer nchans,pols(MAXPOL)
	double precision uvw(3,MAXBASE),time,sfreq(MAXCHAN*MAXPOL)
	integer nants,nbins,nslots,npols
	common/psrcom/uvw,time,sfreq,slot,pFlags,pData,nchans,pols,
     *	  nants,nbins,nslots,npols
