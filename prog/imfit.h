	integer LEVEL,GAUSSIAN,DISK
	parameter(LEVEL=1,GAUSSIAN=2,DISK=3)
	integer MAXSRC,MAXDATA
	parameter(MAXSRC=10,MAXDATA=100000)
	integer nsrc,ndata
	integer srctype(MAXSRC)
	real fwhm1(MAXSRC),fwhm2(MAXSRC),pa(MAXSRC),flux(MAXSRC)
	real l0(MAXSRC),m0(MAXSRC)
	real Data(MAXDATA)
	integer x(MAXDATA),y(MAXDATA)
	logical vflux(MAXSRC),vl0(MAXSRC),vm0(MAXSRC)
	logical vfwhm1(MAXSRC),vfwhm2(MAXSRC),vpa(MAXSRC),circ(MAXSRC)
c
	common/ImFit/vflux,vl0,vm0,vfwhm1,vfwhm2,vpa,circ,
     *	  nsrc,srctype,ndata,x,y,l0,m0,fwhm1,fwhm2,pa,flux,Data
