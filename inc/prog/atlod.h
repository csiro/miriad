c
c  The common block (yuk) used to buffer up an integration.
c
	include 'maxdim.h'
	integer ATIF,ATANT,ATPOL,ATDATA,ATBASE,ATBIN,ATCONT
	parameter(ATIF=34,ATANT=8,ATPOL=4,ATBASE=((ATANT+1)*ATANT)/2)
	parameter(ATBIN=1024,ATCONT=33)
	parameter(ATDATA=8*MAXCHAN*ATBASE)
	integer nifs,nfreq(ATIF),nstoke(ATIF),polcode(ATIF,ATPOL)
        integer ifchain(ATIF)
	double precision sfreq(ATIF),sdf(ATIF),restfreq(ATIF)
	double precision time
	integer tcorr
	real xtsys(ATIF,ATANT),ytsys(ATIF,ATANT),chi
	real xgtp(ATIF,ATANT),ygtp(ATIF,ATANT)
	real xsdo(ATIF,ATANT),ysdo(ATIF,ATANT)
	real xcaljy(ATIF,ATANT),ycaljy(ATIF,ATANT)
	real u(ATBASE),v(ATBASE),w(ATBASE)
	real xyphase(ATIF,ATANT),xyamp(ATIF,ATANT)
	real xsampler(3,ATIF,ATANT),ysampler(3,ATIF,ATANT)
	complex data(ATDATA)
	integer pnt(ATIF,ATPOL,ATBASE,ATBIN),nbin(ATIF),edge(ATIF)
	integer bchan(ATIF)
	real inttime(ATBASE),inttim
	logical flag(ATIF,ATPOL,ATBASE,ATBIN),dosw(ATBASE)
	integer nused,tno,nants,mcount
	logical dosam,dohann,birdie,doif,dobary,newfreq,newsc,newpnt
	logical dowt,dopmps,doxyp,opcorr,hires,cabb,dopack,dotsys
	real wts(2*ATCONT-2),edgepc
	real axisrms(ATANT),axismax(ATANT),mdata(9)
	double precision obsra,obsdec,lat,long,ra,dec,xyzref(3)
	character sname*64
	integer refnant,nopcorr
	real refpnt(2,ATANT)
	real stemp,spress,shumid
c
	common/atlodd/sname
	common/atlodc/sfreq,sdf,restfreq,time,obsra,obsdec,lat,long,
     *	    xyzref,ra,dec,
     *	  data,
     *	  xtsys,ytsys,xgtp,ygtp,xsdo,ysdo,xcaljy,ycaljy,chi,
     *    xyphase,xyamp,xsampler,ysampler,u,v,w,inttime,
     *	    inttim,wts,mdata,axisrms,axismax,refpnt,stemp,spress,shumid,
     *	  pnt,nbin,nused,tno,nants,nifs,nfreq,nstoke,polcode,ifchain,
     *	    edge,bchan,tcorr,mcount,refnant,nopcorr,
     *	  flag,dosw,dosam,dohann,birdie,dowt,dopmps,doxyp,opcorr,
     *	    doif,dobary,newfreq,hires,cabb,dopack,dotsys,
     *	  newsc,newpnt,edgepc
