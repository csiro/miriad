	integer ATANT,MAXTIME,MAXSOL
	parameter(ATANT=6,MAXTIME=10000,MAXSOL=2*MAXTIME)
	integer idx(MAXTIME),nidx(MAXTIME),nsol,ntime
	double precision time(MAXTIME),freq(MAXSOL)
	complex xyphase(ATANT,MAXSOL)
	common/atxycom/time,freq,xyphase,idx,nidx,nsol,ntime
