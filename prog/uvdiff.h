	integer MAXCHAN,MAXBASE,MAXPOL
	parameter(MAXCHAN=20,MAXBASE=15,MAXPOL=4)
c
	complex data(MAXCHAN,MAXPOL,MAXBASE,2),cspare(MAXCHAN)
	logical flags(MAXCHAN,MAXPOL,MAXBASE,2),lspare(MAXCHAN)
	double precision pspare(4),ha(2)
	integer tno,nchan(2),nspare
	common/uvdcom/pspare,ha,data,cspare,tno,nchan,nspare,
     *		flags,lspare
