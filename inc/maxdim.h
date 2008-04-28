c=======================================================================
c - MAXBUF tells us how many words of memory we can use for data
	INTEGER   MAXBUF
        PARAMETER(MAXBUF=1000000)
c-----------------------------------------------------------------------
c - MAXDIM is an often used parameter, to indicate maximum size of maps
	INTEGER   MAXDIM
	PARAMETER(MAXDIM=8192)
c-----------------------------------------------------------------------
c		maximum number of antennae (HC=3/6/9/..., WSRT=14, VLA=27)
	INTEGER   MAXANT
	PARAMETER(MAXANT=64)

c		maximum number of baselines
	INTEGER   MAXBASE
	PARAMETER(MAXBASE=((MAXANT*(MAXANT+1))/2))

c		maximum number of channels in spectral data
	INTEGER   MAXCHAN
	PARAMETER(MAXCHAN=4097)

c		maximum number of windows in visibility data
	INTEGER   MAXWIN
	PARAMETER(MAXWIN=16)
c
c		maximum number of wideband channels.
	INTEGER MAXWIDE
	PARAMETER(MAXWIDE=18)
c=======================================================================
