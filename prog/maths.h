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
	INTEGER   lIn(MAXFILES),Offset(MAXFILES+1),naxes(MAXFILES)
	INTEGER   runs(3,MAXRUNS),nruns,blc(MAXNAX),trc(MAXNAX),ref
	REAL      range(2,MAXNAX)
	LOGICAL   xused,yused,zused,grow
	CHARACTER names*256
	COMMON/mathcom/nfiles,nsize,naxes,naxis,Plane,ref,lIn,Offset,
     *		Runs,nRuns,blc,trc,Range,Xused,Yused,Zused,grow
	COMMON/mathcomc/names

