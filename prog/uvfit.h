	integer MAXVIS,MAXSRC
	integer POINT,DISK,GAUSSIAN,SHELL,RING
	parameter(MAXVIS=200000,MAXSRC=20)
	parameter(DISK=1,GAUSSIAN=2,POINT=3,RING=4,SHELL=5)
c
	complex vis(MAXVIS)
	real u(MAXVIS),v(MAXVIS)
	integer nvis,nsrc
	integer srctype(MAXSRC)
	real flux(MAXSRC),l0(MAXSRC),m0(MAXSRC)
	real fwhm1(MAXSRC),fwhm2(MAXSRC),pa(MAXSRC)
	real sflux(MAXSRC),sl0(MAXSRC),sm0(MAXSRC)
	real sfwhm1(MAXSRC),sfwhm2(MAXSRC),spa(MAXSRC)
	logical vflux(MAXSRC),vl0(MAXSRC),vm0(MAXSRC)
	logical vfwhm1(MAXSRC),vfwhm2(MAXSRC),vpa(MAXSRC),circ(MAXSRC)
	common/fitvaryc/vis,u,v,flux,l0,m0,fwhm1,fwhm2,
     *	  pa,sflux,sl0,sm0,sfwhm1,sfwhm2,spa,
     *	  nvis,nsrc,srctype,vflux,vl0,vm0,vfwhm1,vfwhm2,vpa,circ
