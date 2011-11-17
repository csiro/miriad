	integer MAXVIS,MAXSRC,MAXBIN,MAXVAR
	parameter(MAXVIS=20000000,MAXSRC=20,MAXBIN=16,MAXVAR=20)
	integer POINT,DISK,GAUSSIAN,SHELL,RING
	parameter(DISK=1,GAUSSIAN=2,POINT=3,RING=4,SHELL=5)
c
	complex vis(MAXVIS),vis1(MAXVIS)
	real u(MAXVIS),v(MAXVIS),u1(MAXVIS),v1(MAXVIS)
        double precision freq(MAXVIS),time(MAXVIS)
        double precision freqs(MAXBIN),times(MAXBIN),mfreq(MAXBIN),dt,df
	integer nvis,nvis1,nsrc,nfreq,ntime,nf(MAXBIN)
	integer srctype(MAXSRC)
	real flux(MAXSRC),l0(MAXSRC),m0(MAXSRC)
	real fwhm1(MAXSRC),fwhm2(MAXSRC),pa(MAXSRC)
	real sflux(MAXSRC),sl0(MAXSRC),sm0(MAXSRC)
	real sfwhm1(MAXSRC),sfwhm2(MAXSRC),spa(MAXSRC)
	logical vflux(MAXSRC),vl0(MAXSRC),vm0(MAXSRC)
	logical vfwhm1(MAXSRC),vfwhm2(MAXSRC),vpa(MAXSRC),circ(MAXSRC)
        real par(MAXBIN,MAXBIN,MAXVAR,4)
	common/fitvaryc/vis,vis1,u,v,u1,v1,freq,time,freqs,times,mfreq,
     *	  df,dt,flux,l0,m0,fwhm1,fwhm2,pa,sflux,sl0,sm0,sfwhm1,sfwhm2,
     *	  spa,nvis,nvis1,nsrc,nfreq,ntime,srctype,vflux,vl0,vm0,
     *    vfwhm1,vfwhm2,vpa,circ,par,nf
