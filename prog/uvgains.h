c************************************************************************
c
c  Include file for uvgains.for
c
c  Buf		Buffer used to accumulate the data.
c  Count(i)	Number of good correlations added into Data(i).
c  free		Points to the first unused location in Data and Count.
c  pnt		For a baseline, points to location of data in Data and Count.
c  nchan	Number of channels for a given baseline.
c  npols	Number of polarisations.
c  pols		The polarisation codes.
c  preamble	The accumulated preambles.
c  cnt		The number of things accumulated into the preambles.
c  
	include 'maxdim.h'
	integer MAXAVER,MAXPOL
	parameter(MAXAVER=81920,MAXPOL=4)
	complex buf(MAXAVER)
	integer count(MAXAVER)
	integer pnt(MAXPOL,MAXBASE),nchan(MAXPOL,MAXBASE),free,mbase
	integer npols(MAXBASE),pols(MAXPOL,MAXBASE),cnt(MAXBASE)
	integer cntp(MAXPOL,MAXBASE)
	double precision preamble(4,MAXBASE)
	common/uvavcom/preamble,buf,count,pnt,nchan,npols,pols,cnt,
     *	  cntp,free,mbase
