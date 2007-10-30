c-----------------------------------------------------------------
c	invert.h - include file for invert
c-----------------------------------------------------------------
	integer maxpols
	parameter(maxpols=4)
	integer npols,pols(maxpols),PolStat
	real epoch,vobs,pbfwhm,lstart,lwidth,lstep
	double precision crval1,crval2,crval3,cdelt1,cdelt2,cdelt3
	double precision restfreq,obsra,obsdec
	logical Vchang,Rchang,RAchang,DecChang,SrcChang,Vlin,Rconst
	character observer*32,source*32,telescop*32
	character ctype1*32,ctype2*32,ctype3*32,ltype*32
	common/invertc1/crval1,crval2,crval3,cdelt1,cdelt2,cdelt3,
     *			restfreq,obsra,
     *			obsdec,epoch,vobs,pbfwhm,npols,pols,PolStat,
     *			Vchang,Rchang,RAchang,DecChang,SrcChang,
     *			Vlin,Rconst,lstart,lwidth,lstep
	common/invertc2/observer,source,telescop,
     *			ctype1,ctype2,ctype3,ltype
