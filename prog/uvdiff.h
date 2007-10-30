	include 'maxdim.h'
	include 'mem.h'
	integer MAXPOL,PolMin,PolMax
	parameter(MAXPOL=4,PolMin=-8,PolMax=4)
c
	complex cspare(MAXCHAN)
	logical lspare(MAXCHAN)
	double precision pspare(4),ha(2)
	integer tno,nchan(2),nspare,mbase(2),mpol(2),npols
	integer polindx(PolMin:PolMax)
	integer cindices(MAXPOL,MAXBASE,2),findices(MAXPOL,MAXBASE,2)
	common/uvdcom/pspare,ha,cspare,tno,nchan,nspare,cindices,
     *		findices,mbase,mpol,npols,polindx,lspare
