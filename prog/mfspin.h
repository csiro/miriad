c-------------------------------------------------
c	mfspect.h - include file for mfspect.for
c-------------------------------------------------
	integer nPM
	parameter(nPM=11)
	real Patch(nPM*nPM)
	integer sxxc(nPM*nPM),syyc(nPM*nPM),sxyc(nPM*nPM)
	common/PatchCom/Patch,sxxc,syyc,sxyc
