	integer MAXCAL,MAXSCAL,NRAN
	parameter(MAXCAL=500,MAXSCAL=500,NRAN=5)
	integer ncal,nscal(MAXCAL),calidx(MAXCAL)
	integer scalidx(MAXSCAL,MAXCAL)
	real uni(NRAN)
	common/moscomA/uni
	common/moscomB/ncal,nscal,calidx,scalidx
