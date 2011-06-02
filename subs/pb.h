	integer POLY,GAUS,COS6,SINGLE,IPOLY,BLOCKED
	parameter(POLY=1,GAUS=2,COS6=3,IPOLY=4,BLOCKED=5,SINGLE=6)
	integer MAXPB,MAXVAL,MAXOBJ,NCONV
	parameter(MAXPB=32,MAXVAL=64,MAXOBJ=20000,NCONV=10)
c
	character pb(MAXPB)*16,descrip(MAXPB)*32
	integer pbtype(MAXPB),npbvals,npb,indx(MAXPB),nvals(MAXPB)
	real f1(MAXPB),f2(MAXPB),cutoff(MAXPB),pbfwhm(MAXPB)
	real pbvals(MAXVAL),maxrad(MAXPB)
c
	integer pbhead
	real xc(MAXOBJ),yc(MAXOBJ),x0(MAXOBJ),y0(MAXOBJ),fwhm(MAXOBJ)
	real freq(MAXOBJ), xn(MAXOBJ), yn(MAXOBJ), bandw(MAXOBJ)
	integer pnt(MAXOBJ)
        logical conv(MAXOBJ)
c
	common/pb1com/ f1,f2,cutoff,pbfwhm,pbvals,xc,yc,x0,y0,fwhm,freq,
     *	    maxrad,xn,yn,bandw, pbtype,npbvals,npb,indx,nvals,pbHead,
     *      pnt,conv
	common/pb1comc/pb,descrip
