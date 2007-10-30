c
c  The common block (yuk) used to buffer up an integration.
c
	include 'maxdim.h'
	integer ATIF,ATANT,ATPOL,ATDATA,ATBASE
	parameter(ATIF=2,ATANT=6,ATPOL=4,ATBASE=((ATANT+1)*ATANT)/2)
	parameter(ATDATA=MAXCHAN*ATBASE)
	integer nifs,nfreq(ATIF),nstoke(ATIF),polcode(ATIF,ATPOL)
	double precision sfreq(ATIF),sdf(ATIF),restfreq(ATIF)
	double precision time
	real xtsys(ATIF,ATANT),ytsys(ATIF,ATANT),chi
	real u(ATBASE),v(ATBASE),w(ATBASE)
	real xyphase(ATIF,ATANT),xyamp(ATIF,ATANT)
	real xsampler(3,ATIF,ATANT),ysampler(3,ATIF,ATANT)
	complex data(ATDATA)
	integer pnt(ATIF,ATPOL,ATBASE)
	real inttime(ATBASE)
	logical flag(ATIF,ATPOL,ATBASE),dosw(ATBASE)
	integer nused,tno,nants
	logical dosam,dohann,doif,dobary,newfreq,newsc,newpnt
	double precision obsra,obsdec,lat,long,ra,dec
c
	common/atlodc/sfreq,sdf,restfreq,time,obsra,obsdec,lat,long,
     *	  ra,dec,
     *	  data,
     *	  xtsys,ytsys,chi,xyphase,xyamp,xsampler,ysampler,u,v,w,inttime,
     *	  pnt,nused,tno,nants,nifs,nfreq,nstoke,polcode,
     *	  flag,dosw,dosam,dohann,doif,dobary,newfreq,newsc,newpnt
