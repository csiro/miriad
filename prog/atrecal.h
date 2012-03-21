c************************************************************************
c
c  Include file for atrecal.for
c
c  Buf		Buffer used to accumulate the data.
c  Bufr         Buffer used to accumulate amplitudes
c               for amp-scalar averaging
c  Count(i)	Weights of the correlations added into Data(i).
c  free		Points to the first unused location in Data and Count.
c  pnt		For a baseline, points to location of data in Data and Count.
c  nchan	Number of channels for a given baseline.
c  npols	Number of polarisations.
c  pols		The polarisation codes.
c  preamble	The accumulated preambles.
c  cnt		Weights of the things accumulated into the preambles.
c  
	include 'maxdim.h'
	integer MAXAVER,MAXPOL,ATIF,ATANT,ATBASE
	parameter(MAXAVER=16777216,MAXPOL=4,ATIF=34,ATANT=8,
     *   ATBASE=(ATANT*(ATANT+1))/2)
	complex buf(MAXAVER)
        real    bufr(MAXAVER)
	double precision count(MAXAVER),cnt(ATBASE)
	integer pnt(MAXPOL,ATBASE),nchan(MAXPOL,ATBASE),free,mbase
	integer npols(ATBASE),pols(MAXPOL,ATBASE),nauto
	double precision preamble(6,ATBASE)
        real xtsys(ATIF,ATANT),ytsys(ATIF,ATANT),xyphase(ATIF,ATANT)
        real xtsysf(MAXCHAN,ATANT),ytsysf(MAXCHAN,ATANT)
	common/uvavcom/preamble,count,cnt,buf,bufr,pnt,nchan,npols,
     *    pols,free,mbase,nauto,xtsys,ytsys,xyphase,xtsysf,ytsysf
