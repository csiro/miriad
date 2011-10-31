	integer MAXPNT,MAXHASH
	parameter(MAXPNT=20000,MAXHASH=4*MAXPNT+1)
	integer nxy,coRef
        ptrdiff pX,pY
	integer pntno,npnt,vPntUpd,HashSize,Hash(MAXHASH),nx2,ny2
	integer pbObj(MAXPNT)
	logical solar,doinit, otf
	double precision ucoeff(3,MAXPNT),vcoeff(3,MAXPNT)
	real Rms2(MAXPNT),SumWt(MAXPNT),x0(MAXPNT),y0(MAXPNT)
	double precision llmm(2,MAXPNT),radec(2,MAXPNT),radec0(2)
	double precision cdelt1,cdelt2,radec2(2,MAXPNT)
	character telescop(MAXPNT)*16
	common/mostab1/	llmm,radec,radec0,radec2,cdelt1,cdelt2,
     *	  ucoeff,vcoeff,Rms2,SumWt,x0,y0,
     *	  pntno,npnt,nxy,vPntUpd,pX,pY,HashSize,Hash,nx2,ny2,coRef,
     *	  pbObj,
     *	  solar,doinit,otf
	common/mostab2/telescop
