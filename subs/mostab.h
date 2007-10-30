	integer MAXPNT,MAXHASH
	parameter(MAXPNT=512,MAXHASH=4*MAXPNT+1)
	integer nxy,pX,pY,coRef
	integer pntno,npnt,vPntUpd,HashSize,Hash(MAXHASH),nx2,ny2
	logical solar,doinit
	real ucoeff(3,MAXPNT),vcoeff(3,MAXPNT)
	real Rms2(MAXPNT),SumWt(MAXPNT)
	double precision llmm(2,MAXPNT),radec(2,MAXPNT),radec0(2)
	double precision cdelt1,cdelt2
	character telescop(MAXPNT)*16
	real pbfwhm(MAXPNT)
	common/mostab1/	llmm,radec,radec0,cdelt1,cdelt2,
     *	  pbfwhm,ucoeff,vcoeff,Rms2,SumWt,
     *	  pntno,npnt,nxy,pX,pY,vPntUpd,HashSize,Hash,nx2,ny2,coRef,
     *	  solar,doinit
	common/mostab2/telescop
