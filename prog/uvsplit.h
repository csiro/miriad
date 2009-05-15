c uvsplit.h: Include file for uvsplit.for.
c
c $Id: uvsplit.h,v 1.2 2009/05/15 06:02:31 cal103 Exp $
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer MAXFILES, MAXOPEN
      parameter (MAXFILES = 8192)
      parameter (MAXOPEN  = 6)

      integer npol,nopen,nfiles,ifno(MAXFILES),indx(MAXFILES),nwins
      integer vCheck(MAXFILES),vCopy(MAXFILES),lOut(MAXFILES),lVis
      logical done(MAXFILES),doif,dowide,docomp,wins(MAXWIN)
      character out(MAXFILES)*64,version*64

      common/Files/npol,nopen,nfiles,ifno,indx,vCheck,vCopy,
     *       lOut,lVis,nwins,done,doif,dowide,docomp,wins
      common/FilesC/out,version
