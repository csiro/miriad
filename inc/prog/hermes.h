c
	real scale,freq
	common /main/ scale,freq
c
	integer imax,jmax,nlon,nlat
	real dx(50),dt
	common /grid/ imax,jmax,dx,dt,nlon,nlat
c
	real e,rsun,pi,lonse,latse,pa,rmerc,time
	common /geom/ e,rsun,pi,lonse,latse,pa,rmerc,time
c
	real inertia,delta,albedo,refract,leak
	common /phys/ inertia,delta,albedo,refract,leak
c
	real cho,chi,s(7)
	common /nonlin/ cho,chi,s
c
	real temp(0:50,0:1000),tb(100,20)
	common /therm/ temp,tb
c
	real longr(64,64),latgr(64,64),tbgr(64,64),cell
	common /map/ longr,latgr,tbgr,cell
c
	integer kmin,kmax,msg
	real tol,tnot
	common /iter/ kmin,kmax,msg,tol,tnot
c
	real tnu,xmax
	common /obs/ tnu,xmax
c
	integer nmax
	real pi2,sre,oerr,lonss
	common /orb/ nmax,pi2,sre,oerr,lonss
c
