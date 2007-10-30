c
c  The common block (yuk) used to buffer up an integration.
c
	include 'maxdim.h'
	integer ATIF,ATANT,ATPOL,ATDATA,ATBASE,ATBIN,ATCONT
	parameter(ATIF=2,ATANT=8,ATPOL=4,ATBASE=((ATANT+1)*ATANT)/2)
	parameter(ATBIN=1024,ATCONT=33)
	parameter(ATDATA=8*MAXCHAN*ATBASE)
	integer nifs,nfreq(ATIF),nstoke(ATIF),polcode(ATIF,ATPOL)
	double precision sfreq(ATIF),sdf(ATIF),restfreq(ATIF)
	double precision time
	integer tcorr
	real xtsys(ATIF,ATANT),ytsys(ATIF,ATANT),chi
	real u(ATBASE),v(ATBASE),w(ATBASE)
	real xyphase(ATIF,ATANT),xyamp(ATIF,ATANT)
	real xsampler(3,ATIF,ATANT),ysampler(3,ATIF,ATANT)
	complex data(ATDATA)
	integer pnt(ATIF,ATPOL,ATBASE,ATBIN),nbin(ATIF),edge(ATIF)
	integer bchan(ATIF)
	real inttime(ATBASE),inttim
	logical flag(ATIF,ATPOL,ATBASE,ATBIN),dosw(ATBASE)
	integer nused,tno,nants
	logical dosam,dohann,birdie,doif,dobary,newfreq,newsc,newpnt
	logical dowt,dopmps,doxyp,opcorr,hires
	real wts(2*ATCONT-2)
	real axisrms(ATANT),axismax(ATANT),mdata(5)
	logical mflag
	double precision obsra,obsdec,lat,long,ra,dec
	character sname*64
c
	common/atlodd/sname
	common/atlodc/sfreq,sdf,restfreq,time,obsra,obsdec,lat,long,
     *	    ra,dec,
     *	  data,
     *	  xtsys,ytsys,chi,xyphase,xyamp,xsampler,ysampler,u,v,w,inttime,
     *	    inttim,wts,mdata,axisrms,axismax,
     *	  pnt,nbin,nused,tno,nants,nifs,nfreq,nstoke,polcode,edge,
     *	    bchan,tcorr,
     *	  flag,dosw,dosam,dohann,birdie,dowt,dopmps,doxyp,opcorr,
     *	    doif,dobary,newfreq,hires,mflag,
     *	  newsc,newpnt
