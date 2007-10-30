
*  include file for amiba_uv_gen.f


*  the geometry, map
*  the geometry

	integer MXP		! max number of platforms
	parameter (MXP=1)

	integer MXA		! max number of antennas/platform
	parameter (MXA=25)

	structure /coord_struct/
	  real x		! coords in m
	  real y
	  real z
	end structure

	structure /platform_struct/
	  record /coord_struct/ xp
	  integer nant		! number of antennas on the platform
	  record /coord_struct/ ant(MXA)
	end structure
	record /platform_struct/ plat(MXP)

	integer np		! number of platforms




* the uv data

	integer MXB		! max number of baselines
	parameter (MXB = 250)

	integer nb		! number of baselines

	integer MXV		! max number of visibilities
	parameter (MXV = 100)

	structure /uvw_struct/
	  real u
	  real v
	  real w
	end structure

	structure /vis_structure/
	  record /uvw_struct/ u(MXB)
	end structure
	record /vis_structure/ vis(MXV)
	integer nv		! number of visibilities

	structure /map_structure/
	  integer pl1
	  integer ant1
	  integer pl2
	  integer ant2
	end structure
	record /map_structure/ bmap(MXB)

	real  rad(MXB*MXV)
	integer ir

	common /sz_config/ np, plat, nb, nv, vis, bmap, ir, rad


*--------------------------------------------------

* scan details

	integer DRIFT
	integer POINT
	parameter (DRIFT = 1)
	parameter (POINT = 2)


*  The next 3 are in radians

	real    dec
	real    lat
	real    elmin

*  The next 3 items are all in decimal hours

	real    Dextent
	real    overhead
	real    offset

*  GHz
	real    freq

*  mm
	real    lambda

	integer sctype
	logical parallactify


	common /sz_run/ dec, lat, elmin, 
     :                  freq, lambda, 
     :                  Dextent, overhead, offset,
     :                  sctype,
     :                  parallactify



