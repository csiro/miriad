	integer MAXMOD
	parameter(MAXMOD=3)
	include 'maxdim.h'
	include 'mem.h'
c
	integer nu,nv,u0,v0,nchan,tno,pGrid(MAXMOD),nmod
	real du,dv,fac(MAXCHAN),sfreq,sdf,ll,mm
	double precision ucoeff(3),vcoeff(3)
c
	common/viscompc/ucoeff,vcoeff,du,dv,fac,sfreq,sdf,ll,mm,
     *	  nu,nv,u0,v0,nchan,tno,nmod,pGrid
