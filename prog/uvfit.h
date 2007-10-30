	integer maxvis,maxso
	parameter(maxvis=200000,maxso=10)
	complex vis(maxvis)
	real u(maxvis),v(maxvis),scale
	integer nvis,nso
	common/fitvaryc/vis,u,v,scale,nvis,nso
