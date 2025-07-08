	integer MAXVIS
	parameter(MAXVIS=200000)
c
	complex vis(MAXVIS)
	real u(MAXVIS),v(MAXVIS),chi(MAXVIS),t(MAXVIS)
	real iflux,qflux,uflux,vflux,dfdt,offset(2)
	integer pol(MAXVIS)
	integer nvis
	logical noqu,nov,const,noshift
	common/fitvaryc/vis,u,v,chi,t,iflux,qflux,uflux,vflux,dfdt,
     *	  offset,pol,nvis,noqu,nov,const,noshift



