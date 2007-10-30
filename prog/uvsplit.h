	integer MAXFILES,MAXOPEN
	parameter(MAXFILES=256,MAXOPEN=6)
c
	integer npol,nopen,nfiles,ifno(MAXFILES),indx(MAXFILES)
	integer vCheck(MAXFILES),vCopy(MAXFILES),lOut(MAXFILES),lVis
	logical done(MAXFILES),doif,dowide,docomp
	character out(MAXFILES)*64,version*64
c
	common/Files/npol,nopen,nfiles,ifno,indx,vCheck,vCopy,
     *	    lOut,lVis,done,doif,dowide,docomp
	common/FilesC/out,version
