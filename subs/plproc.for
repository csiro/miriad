c************************************************************************
	subroutine PlInit(dist1,plant,erad1,prad1)
c
	implicit none
	integer plant
	real dist1,erad1,prad1
c
c  Set the planet of interest and the mean distance.
c
c  Input:
c    dist1	The standard distance to the planet in AU. e.g. for
c		Jupiter use 4.04.
c    plant	Planet number. Mercury=1, Venus=2, etc
c  Output:
c    erad	Planet major axis in radians.
c    prad	Planet minor axis in radians.
c------------------------------------------------------------------------
	include 'plproc.h'
	double precision alpha,delta,w,r,f
c
	double precision AUKM,J2000
	parameter(AUKM=149.597870D6)
        parameter(J2000=2451545.0d0)
c
	planet = plant
	meandist = dist1 * AUKM
c
	call plphyeph(J2000,planet,alpha,delta,w,r,f)
	erad = r/meandist
	prad = erad*(1.d0 - f)
	erad1 = erad
	prad1 = prad
	nmat = 0
	tprev = 0
	fake = .false.
	end
c************************************************************************
	subroutine PlFake(dist1,lambda,De)
c
	implicit none
	real dist1,lambda,De
c
c  Set up the ephemeris routines to fake an observation.
c
c  Inputs:
c    dist1	The Earth/planet distance in AU.
c    lambda     Planetocentric CML of the fake observation.
c    De         The planetocentric declination of the Earth.
c------------------------------------------------------------------------
	double precision AUKM
	parameter(AUKM=149.597870D6)
	include 'plproc.h'
	double precision f
c
	call sph2lmn(-dble(lambda),dble(De),n0)
	dist = dist1 * AUKM
c
	bmaj = 2*erad*meandist/dist
	f = 1 - prad/erad
	bmin = bmaj*(1 - f + f*n0(3)*n0(3))
	bpa = 0
c
	call plComm
	fake = .true.
c
	end
c************************************************************************
	subroutine plComm
c
	implicit none
c
c  Some things common to Pluvw and Plfake.
c------------------------------------------------------------------------
	include 'plproc.h'
	double precision l1(3),m1(3),n1(3),scale
	integer i
c
	cospa = cos(bpa)
	sinpa = sin(bpa)
c
	do i=1,3
	  n1(i) = -n0(i)
	enddo
c
c  Compute the planetocentric vectors corresponding to the Earth-equatorial
c  l-, m- and n-axes.
c
	scale = 1/sqrt( n1(1)**2 + n1(2)**2)
	l1(1) = -scale * n1(2)
	l1(2) =  scale * n1(1)
	l1(3) = 0
	call veccross(n1,l1,m1)
	do i=1,3
	  ldash(i) =  l1(i)*cospa + m1(i)*sinpa
	  mdash(i) = -l1(i)*sinpa + m1(i)*cospa
	enddo
c
c  Distance normalisation factor.
c
	fac = dist / meandist
c
c  Shadow matrix
c
	nmat = nmat + 1
	if(nmat.gt.MAXMAT)
     *	  call bug('f','Ran out of shadow matrix slots')
	do i=1,3
	  smat(i,1,nmat) = l1(i) * 2/(bmaj*fac)
	  smat(i,2,nmat) = m1(i) * 2/(bmin*fac)
	  smat(i,3,nmat) = n1(i)
	enddo
	end
c************************************************************************
	subroutine Pluvw(uv,time,uvw,a,b,fac1,smatidx,bpa1,sub)
c
	implicit none
	double precision uv(2),time,uvw(3),sub(3)
	real a,b,fac1,bpa1
	integer smatidx
c
c  Determine various planet-specific parameters, including
c    * The effective (u,v,w) coordinates.
c    * Flux normalisation factor.
c    * Disk parameters.
c    * Shadowing matrix index.
c
c  Input:
c    uv
c    time	Julian date, in UTC
c  Output:
c    uvw	UVW coordinates.
c    a,b	The disk response a*j1xbyx(b*f/f0)
c    fac1	Flux normalisation parameter.
c    smatidx	Index to the shadowing matrix.
c    bpa1	Angle of the rotation axis.
c    sub	Sub-earth point in a planetocentric direction cosines.
c------------------------------------------------------------------------
	include 'plproc.h'
	include 'mirconst.h'
c
	integer i
	double precision tdb
c
c  Externals.
c
	double precision deltime
c
c  Convert UTC into TDB, get the planet parameters, and call the routine
c  to store infor for this time slice.
c
	if(.not.fake.and.(abs(time-tprev).gt.1.d0/86400d0.or.nmat.eq.0))
     *								then
	  tprev = time
	  tdb = time + deltime(time,'tdb')
	  call plpar(tdb,planet,n0,dist,bmaj,bmin,bpa)
	  call plComm
	endif
c
c  Return the uvw coordinates.
c
	do i=1,3
	  uvw(i) = ( uv(1)*ldash(i) + uv(2)*mdash(i) ) / fac
	  sub(i) = n0(i)
	enddo
c
c  Return the disk parameters.
c
	b = PI * sqrt((bmaj*(uv(1)*cospa-uv(2)*sinpa))**2
     *		    + (bmin*(uv(1)*sinpa+uv(2)*cospa))**2)
	a = 2 * PI/4 * bmaj*bmin * fac * fac
c
c  Return the flux normalisation parameter and the shadowing matrix index.
c
	bpa1 = bpa
	fac1 = fac * fac
	smatidx = nmat
c
	end
c************************************************************************
	logical function PlShadow(idx,x,y,z)
c
	implicit none
	integer idx
	real x,y,z
c
c  Determine whether a point is shadowed by the planet for a given
c  shadow matrix.
c
c  Input:
c    idx	Shadow matrix index.
c    x,y,z	Planetocentric coordinate of interest, in radians.
c------------------------------------------------------------------------
	include 'plproc.h'
	real xd,yd,zd,r2
c
c  Check whether the point is within the planet.
c
c	PlShadow = .true.
c	r2 = (x/erad)*(x/erad) + (y/erad)*(y/erad) + (z/prad)*(z/prad)
c	if(r2.lt.1)return
c
c  Check whether the point is in front of the planet centre.
c
	PlShadow = .false.
	zd = smat(1,3,idx)*x + smat(2,3,idx)*y + smat(3,3,idx)*z
	if(zd.lt.0)return
c
c  If its behind, check whether its shadowed by the disk.
c
	xd = smat(1,1,idx)*x + smat(2,1,idx)*y + smat(3,1,idx)*z
	yd = smat(1,2,idx)*x + smat(2,2,idx)*y + smat(3,2,idx)*z
	if(xd*xd+yd*yd.gt.1)return
	PlShadow = .true.
	end
c************************************************************************
	logical function PlInt(x,y,z)
c
	implicit none
	real x,y,z
c
c  Determine whether a point is within the planet.
c
c  Input:
c    x,y,z	Planetocentric coordinate of interest, in radians.
c------------------------------------------------------------------------
	include 'plproc.h'
	real r2
c
c  Check whether the point is within the planet.
c
	r2 = (x/erad)*(x/erad) + (y/erad)*(y/erad) + (z/prad)*(z/prad)
	PlInt = r2.lt.1
	end
