c -- [maths.h] Include file for maths.for
c
c   Uses of order 3*MAXRUNS words of memory, ignored small things
c   of order MAXFILES and MAXNAX.
c
c
	INCLUDE 'maxdim.h'
	INCLUDE 'maxnax.h'
	INTEGER MAXFILES,MAXRUNS,XVAL,YVAL,ZVAL

	PARAMETER(MAXFILES=16,MAXRUNS=80000)
	PARAMETER(XVAL=MAXFILES+1,YVAL=MAXFILES+2,ZVAL=MAXFILES+3)
c
	INTEGER   nfiles,nsize(MAXNAX),naxis,Plane(MAXNAX)
	INTEGER   lIn(MAXFILES),Offset(MAXFILES+1)
	INTEGER   runs(3,MAXRUNS),nruns,blc(MAXNAX),trc(MAXNAX)
	REAL      range(2,MAXNAX)
	LOGICAL   xused,yused,zused
	CHARACTER names*256
	COMMON/mathcom/nfiles,nsize,naxis,Plane,lIn,Offset,Runs,nRuns,
     *		blc,trc,Range,Xused,Yused,Zused
	COMMON/mathcomc/names

