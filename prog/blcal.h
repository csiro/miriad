	include 'maxdim.h'
	include 'mem.h'
	integer MAXPOL,MAXSOL
	parameter(MAXPOL=13,MAXSOL=100000)
	integer head(MAXPOL,MAXBASE),nchan,nsols,next(MAXSOL)
	ptrdiff gidx(MAXSOL),fidx(MAXSOL)
	double precision time(MAXSOL)
	common/blcalc/time,gidx,fidx,next,head,nchan,nsols
