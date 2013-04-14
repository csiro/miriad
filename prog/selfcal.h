c------------------------------------------
c	selfcal.h
c------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer maxHash,minSol
	parameter(maxHash=30000,minSol=200)
	integer nants,nBl,nbad,nbstok,maxSol,nsols,TotVis,minants
	integer nHash,Hash(maxHash),Indx(maxHash),nfbin,nchan0
	integer pSumVM,pSumVV,pSumMM,pWeight,pCount,pGains,prTime
        integer pstptim,pstrtim
	logical first
	double precision time0,freq(0:MAXFBIN)
	common/selfcom/time0,freq,nants,nBl,nbad,nbstok,nsols,maxSol,
     *	  TotVis,minants,nfbin,nchan0,nHash,Hash,Indx,pSumVm,pSumVV,
     *	  pSumMM,pWeight,pCount,pGains,prTime,pstptim,pstrtim,first


