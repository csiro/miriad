c------------------------------------------------
c	uvlist.h - include file for uvlist
c
c  itlen	The maximum number of variables.
c  varname	A table of variable names.
c  vnum		The number of names in varname.
c------------------------------------------------
	integer itlen
	parameter (itlen=200)
	integer vnum
	character varname(itlen)*8
	common /vtablec/ varname
	common /vtablen/ vnum
