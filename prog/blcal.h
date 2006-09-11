	include 'maxdim.h'
	include 'mem.h'
	integer MAXPOL,MAXSOL
	parameter(MAXPOL=13,MAXSOL=100000)
	integer head(MAXPOL,MAXBASE),nchan,nsols
	integer gidx(MAXSOL),fidx(MAXSOL),next(MAXSOL)
	double precision time(MAXSOL)
	common/blcalc/time,gidx,fidx,next,head,nchan,nsols
