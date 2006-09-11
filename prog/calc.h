	integer MAXCONST
	parameter(MAXCONST=200)
	integer nconst
	double precision values(MAXCONST)
	character names(MAXCONST)*8,units(MAXCONST)*8,defs(MAXCONST)*32
	common/calci/values,nconst
	common/calcc/names,units,defs
