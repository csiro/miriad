c
        include 'maxdim.h'
c
c
c MAXVAL = Maximum values we can store  (product of dimension(s) and time )
c MAXVAR = Maximum uv variables a visibility dataset could have
c
        INTEGER MAXVAL, MAXVAR
        PARAMETER (MAXVAL=10000, MAXVAR=500)
        CHARACTER hdvars(MAXVAR)*9,type(MAXVAR)*1,avarnew*20
        REAL rvarnew(MAXVAL)
        DOUBLE PRECISION dvarnew(MAXVAL),atime(MAXVAL)
        INTEGER ivarnew(MAXVAL),nmods
        INTEGER nhdvars,length(MAXVAR),yourvar,tidx,nttable
c
        COMMON /uvputhd1/ hdvars,type,avarnew   
        COMMON /uvputhd2/ nhdvars,length,yourvar,ivarnew,rvarnew,
     *                    tidx,nttable,nmods
        COMMON /uvputhd3/ dvarnew, atime

