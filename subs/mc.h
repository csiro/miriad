	ptrdiff cnvl(MAXPNT),pWrk1,pWrk2,pWts1,pWts2
        ptrdiff nWrk,npix,nWts
	integer tno,npnt,n1,n2,n1d,n2d,ic,jc
	logical mosini,dogaus
	real bmaj,bmin,bpa
	character flags*8
c
	common/mccom/cnvl,bmaj,bmin,bpa,tno,npnt,
     *	    n1,n2,n1d,n2d,ic,jc,mosini,npix,
     *	    pWrk1,pWrk2,pWts1,pWts2,nWrk,nWts,dogaus
	common/mccomc/flags
