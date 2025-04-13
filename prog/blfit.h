	integer NANT,NBL
	parameter(NANT=6,NBL=15)
c
c  A scan is a collection of visibilities which are measured over a
c  sufficiently short period that they are of the same source, the Earth
c  has not rotated significantly (same az/el) and the atmosphere
c  has not varied much.
c  
	integer MAXSCAN
	parameter(MAXSCAN=2000)
	integer nscan
	complex scanData(NBL,MAXSCAN)
	real scanErr(NANT,MAXSCAN)
	real scanPred(NANT,MAXSCAN),scanPhs(NANT,MAXSCAN)
	real scanSM(MAXSCAN)
	double precision scanL(MAXSCAN),scanM(MAXSCAN),scanN(MAXSCAN)
	double precision scanF(MAXSCAN),scanT(MAXSCAN),scanDaz(MAXSCAN)
	double precision scanf160(MAXSCAN)
	double precision scanWt(NBL,MAXSCAN)
	integer scanSet(MAXSCAN)
c
c  A set is a collection of scans that have the same offset parameter
c  in a baseline solution.
c
	integer MAXSET
	parameter(MAXSET=64)
	integer nset
	double precision setSfreq(MAXSET),setSdf(MAXSET)
	double precision setTmin(MAXSET),setTmax(MAXSET)
	logical setIni(MAXSET,NANT)
	integer setPol(MAXSET),setAset(MAXSET),setBm(MAXSET)
c
	common/blcom/scanL,scanM,scanN,scanF,scanf160,scanT,scanDaz,
     *	  setSfreq,setSdf,
     *	  setTmin,setTmax,scanWt,scanData,scanErr,scanPred,
     *	  scanPhs,scanSM,nscan,scanSet,nset,setPol,setAset,setIni,
     *    setBm
