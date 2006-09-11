	integer LEVEL,GAUSSIAN,DISK,POINT,BEAM
	parameter(LEVEL=1,GAUSSIAN=2,DISK=3,POINT=4,BEAM=5)
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
	real    sflux(MAXSRC),sl0(MAXSRC),sm0(MAXSRC)
	real    sfwhm1(MAXSRC),sfwhm2(MAXSRC),spa(MAXSRC)
	real xoff,yoff
c
	common/JmFit/vflux,vl0,vm0,vfwhm1,vfwhm2,vpa,circ,
     *	  nsrc,srctype,ndata,x,y,l0,m0,fwhm1,fwhm2,pa,flux,Data,
     *		     sflux,sl0,sm0,sfwhm1,sfwhm2,spa,
     *		     xoff,yoff
