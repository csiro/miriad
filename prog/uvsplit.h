	include 'maxdim.h'
	integer MAXFILES,MAXOPEN
	parameter(MAXFILES=1000,MAXOPEN=6)
c
	integer npol,nopen,nfiles,ifno(MAXFILES),indx(MAXFILES),nwins
	integer vCheck(MAXFILES),vCopy(MAXFILES),lOut(MAXFILES),lVis
	logical done(MAXFILES),doif,dowide,docomp,wins(MAXWIN)
	character out(MAXFILES)*64,version*64
c
	common/Files/npol,nopen,nfiles,ifno,indx,vCheck,vCopy,
     *	    lOut,lVis,nwins,done,doif,dowide,docomp,wins
	common/FilesC/out,version
