c
c  Include file for tmcvt
c
	integer iUVW,iBL,iTime,iChi1,iChi2
	parameter(iUVW=1,iBl=5,iTime=4,iChi1=6,iChi2=7)	
	integer MAXPOL
	parameter(MAXPOL=4)
	include 'maxdim.h'
	double precision preamble(7,MAXBASE)
	integer blmax,pntc(MAXPOL,MAXBASE),pntr(MAXPOL,MAXBASE)
	integer cnt(MAXBASE),nchans(MAXBASE)
	real tints(MAXPOL,MAXBASE)
	common/tmcvtc/preamble,blmax,pntc,pntr,cnt,nchans,tints
