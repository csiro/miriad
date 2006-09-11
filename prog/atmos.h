	integer MAXCAL,MAXSCAL,NRAN
	parameter(MAXCAL=500,MAXSCAL=500,NRAN=5)
	integer ncal,nscal(MAXCAL),calidx(MAXCAL)
	integer scalidx(MAXSCAL,MAXCAL)
	real uni(NRAN)
	common/moscom/uni,ncal,nscal,calidx,scalidx
