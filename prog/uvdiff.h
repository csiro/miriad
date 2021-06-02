      include 'maxdim.h'
      include 'mem.h'

      integer MAXPOL, POLMIN, POLMAX
      parameter (MAXPOL=4, POLMIN=-8, POLMAX=4)

      ptrdiff cindices(MAXPOL,MAXBASE,2)
      ptrdiff findices(MAXPOL,MAXBASE,2)
      integer iha, jha, nchan(2), npols, polindx(POLMIN:POLMAX), tno
      double precision ha2(2)

      common /uvdcom/ ha2, cindices, findices, iha, jha, nchan, npols,
     *                polindx, tno
