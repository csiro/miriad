c--------------------------------------------------
c	fits.h - include file for fits
c--------------------------------------------------
	include 'maxdim.h'
	integer uvCrval,uvCdelt,uvCrpix
	integer uvStokes,uvFreq,uvRa,uvDec
	integer uvU,uvV,uvW,uvBl,uvT,uvSrcId,uvFreqId
	integer uvRandom,uvData
c
	parameter(uvCrval=1,uvCdelt=2,uvCrpix=3)
	parameter(uvStokes=1,uvFreq=2,uvRa=3,uvDec=4)
	parameter(uvU=1,uvV=2,uvW=3,uvBl=4,uvT=5,uvRandom=5)
	parameter(uvSrcId=6,uvFreqId=7)
	parameter(uvData=uvRandom+1)
c
	integer PolII,PolI,PolQ,PolU,PolV,PolRR,PolLL,PolRL,PolLR
	integer PolXX,PolYY,PolXY,PolYX,PolMin,PolMax
c
	parameter(PolII=0,PolI=1,PolQ=2,PolU=3,PolV=4,PolRR=-1)
	parameter(PolLL=-2,PolRL=-3,PolLR=-4,PolXX=-5,PolYY=-6)
	parameter(PolXY=-7,PolYX=-8,PolMin=PolYX,PolMax=PolV)
c
	integer MAXPOL,MAXSRC,MAXIF,MAXFREQ,MAXCONFG
	parameter(MAXPOL=4,MAXSRC=128,MAXIF=16,MAXFREQ=16,MAXCONFG=10)
