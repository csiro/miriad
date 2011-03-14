        real THRESH
        parameter(THRESH=1.0/(6.*24.0))
c
        integer MAXGAPS,NBANDS,ATANT
        parameter(MAXGAPS=6000,NBANDS=7,ATANT=6)
        double precision times(ATANT),tstart,tend,timesys(ATANT)
	double precision tgaps(MAXGAPS),tgape(MAXGAPS)
        integer ngaps,agap(MAXGAPS),band(NBANDS)
	logical doant(ATANT)
	character dgap(MAXGAPS)*16
        common/timescom/times,timesys,tstart,tend,tgaps,tgape,ngaps,
     *						agap,band,doant
	common/timescoc/dgap
