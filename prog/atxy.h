	integer ATANT,MAXTIME,MAXSOL
	parameter(ATANT=6,MAXTIME=10000,MAXSOL=2*MAXTIME)
	integer idx(MAXTIME),nidx(MAXTIME),nsol,ntime
	double precision time(MAXTIME),freq(MAXSOL)
	complex xyphase(ATANT,MAXSOL),axyphase
	common/atxycom/time,freq,xyphase,axyphase,idx,nidx,nsol,ntime
