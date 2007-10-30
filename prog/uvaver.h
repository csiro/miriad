c************************************************************************
c
c  Include file for uvaver.for
c
c  Buf		Buffer used to accumulate the data.
c  Bufr         Buffer used to accumulate amplitudes
c               for amp-scalar averaging
c  Count(i)	Weights of the correlations added into Data(i).
c  free		Points to the first unused location in Data and Count.
c  pnt		For a baseline, points to location of data in Data and Count.
c  nchan	Number of channels for a given baseline.
c  npols		Number of polarisations.
c  pols		The polarisation codes.
c  preamble	The accumulated preambles.
c  cnt		Weights of the things accumulated into the preambles.
c  
	include 'maxdim.h'
	integer MAXAVER,MAXPOL
	parameter(MAXAVER=163840,MAXPOL=4)
	complex buf(MAXAVER)
        real    bufr(MAXAVER)
	double precision count(MAXAVER),cnt(MAXBASE)
	integer pnt(MAXPOL,MAXBASE),nchan(MAXPOL,MAXBASE),free,mbase
	integer npols(MAXBASE),pols(MAXPOL,MAXBASE)
	double precision preamble(6,MAXBASE)
	common/uvavcom/preamble,count,cnt,buf,bufr,pnt,nchan,npols,
     *    pols,free,mbase
