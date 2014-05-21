	ptrdiff cnvl(MAXPNT),pWrk1,pWrk2,pWts1,pWts2
	integer nWrk,nWts,tno,npix,npnt,n1,n2,n1d,n2d,ic,jc
	logical mosini,dogaus
	real bmaj,bmin,bpa
	character flags*8
c
	common/mccom/cnvl,bmaj,bmin,bpa,tno,npix,npnt,
     *	    n1,n2,n1d,n2d,ic,jc,
     *	    pWrk1,pWrk2,pWts1,pWts2,nWrk,nWts,mosini,dogaus
	common/mccomc/flags
