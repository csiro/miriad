**************************************************************************

	subroutine minmax (N, dat, zmin, zmax)


	implicit none

	integer N, i
	real    dat(N), zmin, zmax, range

*----------------------------------------------------


	zmin = dat(1)
	zmax = dat(1)

	do i = 1, N
	  zmin = min (zmin, dat(i))
	  zmax = max (zmax, dat(i))
	end do

	range = zmax - zmin
	if (range .eq. 0) range = 1.

	zmax = zmax + .05*range
	zmin = zmin - .05*range

	end
