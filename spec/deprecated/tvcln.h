c-------------------------------------------------
c	restor.h - include file for restor
c-------------------------------------------------
c  History
c
c    ???????  mchw  Original version.
c    13mar92  mjs   restore -> restor name mod
c-------------------------------------------------
	integer nPM
	parameter(nPM=11)
	real Patch(nPM*nPM)
	integer sxxc(nPM*nPM),syyc(nPM*nPM),sxyc(nPM*nPM)
	common/PatchCom/Patch,sxxc,syyc,sxyc
