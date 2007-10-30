c--------------------------------------------------
c	uvgen.h - include file for uvgen program
c--------------------------------------------------
c
c  This include file is used to return the correlator setups
c  from the various "coram" routines to the main routine.
c
c  Outputs:
c    LO description:			lo1,lo2,freqif
c    Wide correlator description:	nwide,wfreq,wwidth
c    Spectral correlator description:	numchan,nspect,sdf,sfreq,ischan,nschan
c
	integer maxspect
	parameter(maxspect=8)
        integer maxsrc,maxpol,maxpnt,maxpolar
        parameter(maxsrc=1000,maxpol=4,maxpnt=100,maxpolar=20)
c
	double precision sfreq(maxspect),sdf(maxspect),lo1,lo2,freqif
	real wfreq(maxspect),wwidth(maxspect)
	integer numchan,nwide,nspect,ischan(maxspect),nschan(maxspect)
	common/uvgencom/sfreq,sdf,lo1,lo2,freqif,wfreq,wwidth,
     *	  numchan,nwide,nspect,ischan,nschan


