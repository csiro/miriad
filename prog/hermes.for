c************************************************************************
	program hermes
	implicit none
c
c= HERMES  Make model images for Mercury.
c& mchw
c: image analysis, planets.
c+
c	First calculates a Hermographic brightness temperature map,
c	then uses interpolation and coordinate rotations to find
c	the corresponding map in geocentric RA and DEC offsets from
c	the sub-earth point.  Next, the Fresnel reflection coef-
c	ficients are applied to obtain the model map.  Finally, a
c	set of CLEAN components are generated for use in the RAL
c	software package.  This is the non-linear diffusion version.
c
c	$MIR/cat/hermes.in is a sample input file, and can be copied
c	to the user's own directory.  A more complete discussion of
c	program HERMES can be found in file $MIR/cat/hermes.txt.
c@ in
c	Parameter file. Default is "hermes.in".
c@ imsize
c	Image size. Default is 64.
c@ log
c	Output log file. Default is "hermes.log".
c@ out
c	Output image. Units JY/PIXEL. No default.
c--
c  History:
c    D.Mitchell 01Jun89	 Initial version.
c	        23sep90  Revised revision.
c		10dec90	 Miriad version.
c    nebk       26nov92  Add btype
c------------------------------------------------------------------------
	include 'hermes.h'
	include 'maxdim.h'
	character in*64,out*64,log*64
	integer lout,nsize(3),imsize
	integer i,j,icrpix,jcrpix,iimin,iimax,jjmin,jjmax,ioff,joff
	real row(maxdim),pixel
c
	data row/maxdim*0./
c
c  Get the input parameters.
c
	call keyini
	call keya('in',in,'hermes.in')
	call keyi('imsize',imsize,64)
	call keya('log',log,'hermes.log')
	call keya('out',Out,' ')
	call keyfin
c
c  Check the reasonableness of the inputs.
c
	if(in.eq.' ') call bug('f','Input parameter file missing')
	if(out.eq.' ') call bug('f','Output image missing')
c
c  Open the output log file.
c
	call LogOpen(log,'q')
c
c  Get the parameters and make the model.
c
	call mercury(in)
c
c  Open the output array
c
	nsize(1) = imsize
	nsize(2) = imsize
	nsize(3) = 1
	call xyopen(lout,out,'new',3,nsize)
c
c  write output map header
c
	call wrhdi(lOut,'naxis',3)
	call wrhdi(lOut,'naxis1',nsize(1))
	call wrhdi(lOut,'naxis2',nsize(2))
	call wrhdi(lOut,'naxis3',nsize(3))
	call wrhda(lOut,'ctype1','RA---SIN')
	call wrhda(lOut,'ctype2','DEC--SIN')
	call wrhda(lOut,'ctype3','VELO-LSR')
	call wrhdr(lOut,'crpix1',float(nsize(1)/2+1))
	call wrhdr(lOut,'crpix2',float(nsize(2)/2+1))
	call wrhdr(lOut,'crpix3',1.)
	pixel = cell*pi/(180.*3600.)
	call wrhdr(lOut,'cdelt1',-pixel)
	call wrhdr(lOut,'cdelt2',pixel)
	call wrhda(lOut,'bunit','JY/PIXEL')
	call wrhdr(lOut,'crval1',0.)
	call wrhdr(lOut,'crval2',0.)
	call wrhdr(lOut,'crval3',0.)
	call wrhdr(lOut,'epoch',2000.0)
	call wrhda(lOut,'object','mercury')
	call wrhdd(lOut,'restfreq',dble(freq))
        call wrbtype(lOut,'intensity')
c
c  write out image.
c
	scale = scale*tnot
	icrpix = float(nsize(1)/2+1)
	iimin = icrpix-32
	iimax = icrpix+31
	ioff = icrpix-33
	jcrpix = float(nsize(2)/2+1)
	jjmin = jcrpix-32
	jjmax = jcrpix+31
	joff = jcrpix-33
c
	call xysetpl(lOut,1,1)
	do j=1,nsize(2)
	  if((j .ge. jjmin) .and. (j .le. jjmax)) then
	    do i=iimin,iimax
	      row(i)=tbgr(i-ioff,j-joff)*scale
	    enddo
	  endif
	  call xywrite(lOut,j,row)
	enddo
c
c  Write the history file.
c
	call hisopen(lOut,'write')
        call hiswrite(lOut,'HERMES: Create mercury model')
	call hisinput(lOut,'HERMES')
	call hisclose(lOut)
c
c  Close the files after writing history
c
	call xyclose(lOut)
	call logclose
c
	end
c------------------------------------------------------------------
	subroutine mercury(name)
c
	implicit none
	character name*(*)
c  Input:
c    name	Parameter file name.
c  Output:
c    fills out common 'hermes.h'
c  Description:
c	First calculates a Hermographic brightness temperature map,
c	then uses interpolation and coordinate rotations to find
c	the corresponding map in geocentric RA and DEC offsets from
c	the sub-earth point.  Next, the Fresnel reflection coef-
c	ficients are applied to obtain the model map.  Finally, a
c	set of CLEAN components are generated for use in the RAL
c	software package.  This is the non-linear diffusion version.
c
c	D. Mitchell    1 June      1989   UCB
c	              23 September 1990   (latest revision)
c
c	Internal checks: radiative and thermal equilibrium,
c	                 solution periodicity
c	External checks: agreement with linear theory when chi = 0
c	                 agreement between Schmidt and C-N methods
c	                 agreement with other published results
c------------------------------------------------------------------
c
	integer i,lu,iostat
	character line*80
	real x,y,t,d2x,rd,sma,eir
	logical ok
c
c.......Externals
c
	real bound,rtbis
c
c.......Common Blocks:
c
	include 'hermes.h'
c
c.......Read parameters
c
	call txtopen(lu,name,'old',iostat)
	if(iostat .eq. 0) then
		read(lu,10) lonse,latse,pa,rmerc,lonss,time
10		format(//f14.3/f14.3/f14.3/f14.3/f14.3/f14.3)
		read(lu,20) albedo,eir,inertia,chi,leak,refract,delta
20		format(////f14.4/f14.4/f14.5/f14.3/f14.3/f14.3/f14.4)
		read(lu,30) freq,xmax,d2x,imax,jmax,nlon,nlat,cell
30		format(f14.7/////f14.4/f14.4/i14/i14/i14/i14/f14.4)
		read(lu,40) kmin,kmax,tol
40		format(i14/i14/f14.8)
		call txtclose(lu)
	else
		call bug('f','Failed to open parameter file')
	endif
c
c.......Initialize physical grid and convert units
c
	pi = 4.*atan(1.)
	pi2 = 2.*pi
	dt = pi2/float(jmax)
	dx(1) = xmax*(d2x - 1.)/(d2x**float(imax) - 1.)
	do i=2,imax
		dx(i) = dx(i-1)*d2x
	enddo
c
	rd = pi/180.
	lonse = lonse*rd
	latse = latse*rd
	lonss = lonss*rd
	pa = pa*rd
	time = pi*time/12.
c
	e = 0.20561421
	sma = 0.387099
	sre = sqrt((1 + e)/(1 - e))
	nmax = 20
	oerr = 1.e-12
	rsun = (4.65240642e-3)/sma
	tnot = (2.79643432e2)/(sma*sma*eir)**0.25
	scale = (7.22126015e-7)*(freq*cell)**2.
	tnu = (4.79942832e-2)*freq/tnot
	tol = tol/tnot
	inertia = inertia*(3.35504315e8)/(eir*tnot**3.)
	leak = -leak*sma*sma/(3.46689169e5)
	refract = sqrt(refract)
c
	cho = 1./(1. + chi)
	chi = chi*cho*(tnot/350.)**3.
	s(1) = -2.3175e5
	s(2) = tnot*(2.1271e4)
	s(3) = tnot*tnot*(1.5008e2)
	s(4) = (tnot**3.)*(-7.3680e-1)
	s(5) = (tnot**4.)*(9.6567e-4)
	s(6) = 8.4987e6
	s(7) = 1.5909e6
c
	x = s(6)
	do i=1,7
		s(i) = s(i)/x
	enddo
c
c.......Refine time estimate to match sub-solar point
c
	if(time .ge. 0.) then
		x = time
		y = time + 0.003
		t = 1.e-4
		call zbrac(x,y,ok)
		if(ok) then
			time = rtbis(x,y,t)
			time = bound(time)
		else
			write(line,50)
50			format(' HERMESN: cant get Mercury time ')
			call LogWrit(line(1:32))
			pause
		endif
	else
		time = -time
	endif
	write(line,60) time*12./pi
60	format(' HERMESN: Mercury time = ',f6.3)
	call LogWrit(line(1:31))
c
c.......Calculate Hermographic brightness temperature map
c
	call regoln
c
c.......Initialize the geocentric RA-Dec grid
c
	call rotate
c
c.......Interpolate to find the geocentric brightness temperature map
c
	call interp
c
	end
c--------------------------------------------------------------
	subroutine rotate
c
c	From RA and DEC offsets from the sub-earth point in 
c	geocentric coordinates, determine via coordinate
c	rotations the corresponding longitude and latitude on
c	Mercury.  WARNING: Left-handed coordinates in use.
c
c	D. Mitchell   24 May       1989   UCB
c	              27 September 1989   (latest revision)
c
c	tested OK on 27 September 1989
c--------------------------------------------------------------
c
	integer i,j,l,m
	real lrot(3,3),xe(3),xh(3),xx
c
c.......Common blocks: geom, map
c
	common /geom/ e,rsun,pi,lonse,latse,pa,rmerc,time
	real e,rsun,pi,lonse,latse,pa,rmerc,time
c
	common /map/ longr(64,64),latgr(64,64),tbgr(64,64),cell
	real longr,latgr,tbgr,cell
c
c.......Calculate the left-handed coordinate rotation matrix
c
	lrot(1,1) = cos(lonse)*cos(latse)
	lrot(1,2) = -sin(pa)*sin(latse)*cos(lonse) - sin(lonse)*cos(pa)
	lrot(1,3) = -cos(pa)*sin(latse)*cos(lonse) + sin(lonse)*sin(pa)
	lrot(2,1) = sin(lonse)*cos(latse)
	lrot(2,2) = -sin(pa)*sin(latse)*sin(lonse) + cos(lonse)*cos(pa)
	lrot(2,3) = -cos(pa)*sin(latse)*sin(lonse) - cos(lonse)*sin(pa)
	lrot(3,1) = sin(latse)
	lrot(3,2) = sin(pa)*cos(latse)
	lrot(3,3) = cos(pa)*cos(latse)
c
c.......For each geocentric grid point, xe(1:3)
c
	do i=1,64
		xe(2) = float(33-i)*cell
		do j=1,64
			xe(3) = float(j-33)*cell
			xx = rmerc*rmerc - xe(2)*xe(2) - xe(3)*xe(3)
			if(xx .lt. 0.) then
				longr(i,j) = -1.
				latgr(i,j) = 0.
				go to 10
			endif
			xe(1) = sqrt(xx)
c
c.......Determine the Hermographic cartesian coordinates
c
			do l=1,3
				xh(l) = 0.
				do m=1,3
					xh(l) = xh(l) + lrot(l,m)*xe(m)
				enddo
			enddo
c
c.......Find longitude and latitude on Mercury corresponding to xh(1:3)
c
			latgr(i,j) = asin(xh(3)/rmerc)
			xx = atan(xh(2)/xh(1))
			if(xh(1) .lt. 0.) then
				longr(i,j) = xx + pi
			else if(xh(2) .lt. 0.) then
				longr(i,j) = xx + 2.*pi
			else
				longr(i,j) = xx
			endif
c
10			continue
		enddo
	enddo
c
	return
	end
c------------------------------------------------------------------
	subroutine interp
c
c	Interpolates from the Hermographic brightness temperature
c	map (generated by REGOL) using the Numerical Recipes bi-
c	cubic spline routines to find the geocentric brightness
c	temperature map.  Finally, a Fresnel limb darkening cor-
c	rection is applied to obtain the observed geocentric map.
c
c	D. Mitchell    1 June      1989   UCB
c	              28 January   1990   (latest revision)
c
c	tested OK on 28 January 1990
c------------------------------------------------------------------
c
	integer i,j,m,n,midb
	real dlon,dlat,midlat,reflect
	real x1a(100),x2a(20),tb2(100,20),x1,x2,y
c
c.......Common Blocks: grid, geom, therm, map
c
	common /grid/ imax,jmax,dx(50),dt,nlon,nlat
	integer imax,jmax,nlon,nlat
	real dx,dt
c
	common /geom/ e,rsun,pi,lonse,latse,pa,rmerc,time
	real e,rsun,pi,lonse,latse,pa,rmerc,time
c
	common /therm/ temp(0:50,0:1000),tb(100,20)
	real temp,tb
c
	common /map/ longr(64,64),latgr(64,64),tbgr(64,64),cell
	real longr,latgr,tbgr,cell
c
c.......Initialize Hermographic longitude (x1a) and latitude (x2a) vectors
c
	m = 2*nlon
	n = nlat
	midb = nlat/2 + 1
	dlon = pi/float(nlon)
	dlat = pi/float(nlat)
	midlat = float(nlat+1)/2.

	do i=1,m
		x1a(i) = float(i-1)*dlon
	enddo
	do j=1,n
		x2a(j) = (float(j) - midlat)*dlat
	enddo
c
c.......Compute auxiliary 2nd derivative table
c
	call splie2(x1a,x2a,tb,m,n,tb2)
c
c.......Compute the geocentric brightness temperature map
c
	do i=1,64
		do j=1,64
			x1 = longr(i,j)
			x2 = latgr(i,j)
			if(x1 .lt. 0.) then
			  tbgr(i,j) = 0.
			else
			  call splin2(x1a,x2a,tb,tb2,m,n,x1,x2,y)
			  tbgr(i,j) = y*(1. - reflect(x1,x2))
			endif
		enddo
	enddo
c
	return
	end
c----------------------------------------------------------------
      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
      INTEGER N
      REAL XA,YA,Y2A,X,Y
c
c     Numerical Recipes 1-D cubic spline routine which inter-
c     polates from the function (YA) at equally-spaced locations
c     (XA) given the second derivatives (Y2A) to estimate the
c     function value (Y) at location (X).
c
c     Copied from BKYAST$DUA1:[USERLIB.RECIPES]
c----------------------------------------------------------------
c
      INTEGER KLO,KHI,K
      REAL H,A,B
c
      DIMENSION XA(N),YA(N),Y2A(N)
      KLO=1
      KHI=N
1     IF (KHI-KLO.GT.1) THEN
        K=(KHI+KLO)/2
        IF(XA(K).GT.X)THEN
          KHI=K
        ELSE
          KLO=K
        ENDIF
      GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      IF (H.EQ.0.) PAUSE 'Bad XA input.'
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     *      ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      RETURN
      END
c------------------------------------------------------------------
      SUBROUTINE SPLINE(X,Y,N,YP1,YPN,Y2)
      INTEGER N
      REAL X,Y,YP1,YPN,Y2
c
c     Numerical Recipes 1-D cubic spline routine which computes
c     the second derivative (Y2) of a function (Y) at monotonically
c     increasing points (X) given the first derivative at the end
c     points (YP1, YPN).
c
c     Copied from BKYAST$DUA1:[USERLIB.RECIPES]
c------------------------------------------------------------------
c
      INTEGER NMAX,I,K
      REAL U,SIG,P,QN,UN
c
      PARAMETER (NMAX=100)
      DIMENSION X(N),Y(N),Y2(N),U(NMAX)
      IF (YP1.GT..99E30) THEN
        Y2(1)=0.
        U(1)=0.
      ELSE
        Y2(1)=-0.5
        U(1)=(3./(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
      ENDIF
      DO 11 I=2,N-1
        SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
        P=SIG*Y2(I-1)+2.
        Y2(I)=(SIG-1.)/P
        U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     *      /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
11    CONTINUE
      IF (YPN.GT..99E30) THEN
        QN=0.
        UN=0.
      ELSE
        QN=0.5
        UN=(3./(X(N)-X(N-1)))*(YPN-(Y(N)-Y(N-1))/(X(N)-X(N-1)))
      ENDIF
      Y2(N)=(UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
      DO 12 K=N-1,1,-1
        Y2(K)=Y2(K)*Y2(K+1)+U(K)
12    CONTINUE
      RETURN
      END
c-------------------------------------------------------------------
      SUBROUTINE SPLIE2(X1A,X2A,YA,M,N,Y2A)
      INTEGER M,N
      REAL X1A,X2A,YA,Y2A
c
c     Computes the second derivative table (Y2A) for the Numerical
c     Recipes bicubic spline routine given a 2-D grid of equally-
c     spaced independent variables (X1A, X2A) and the corresponding
c     function values (YA).
c
c     Modified on 15 October 1989: natural splines replaced by zero
c     derivatives at the end points.
c
c     Modified on 26 January 1990: variable arrays up MMAX,NMAX
c
c     Copied from BKYAST$DUA1:[USERLIB.RECIPES]
c-------------------------------------------------------------------
c
      INTEGER NN,MMAX,NMAX,J,K
      REAL YTMP,Y2TMP
c
      PARAMETER (NN=100, MMAX=100, NMAX=20)
      DIMENSION X1A(M),X2A(N),YA(MMAX,NMAX),Y2A(MMAX,NMAX),
     &          YTMP(NN),Y2TMP(NN)
      DO 13 J=1,M
        DO 11 K=1,N
          YTMP(K)=YA(J,K)
11      CONTINUE
        CALL SPLINE(X2A,YTMP,N,0.0,0.0,Y2TMP)
        DO 12 K=1,N
          Y2A(J,K)=Y2TMP(K)
12      CONTINUE
13    CONTINUE
      RETURN
      END
c------------------------------------------------------------------------
      SUBROUTINE SPLIN2(X1A,X2A,YA,Y2A,M,N,X1,X2,Y)
      INTEGER M,N
      REAL X1A,X2A,YA,Y2A,X1,X2,Y

c     Numerical Recipes bicubic splines routine which interpolates
c     between function values (YA) at equally-spaced grid points (X1A,
c     X2A) given the second derivative table (Y2A) to estimate the
c     function value (Y) at location (X1, X2).
c
c     Modified on 15 October 1989: natural splines replaced by zero
c     derivatives at the end points.
c
c     Modified on 26 January 1990: variable arrays up to MMAX,NMAX
c
c     Copied from BKYAST$DUA1:[USERLIB.RECIPES]
c------------------------------------------------------------------------
c
      INTEGER NN,MMAX,NMAX,J,K
      REAL YTMP,Y2TMP,YYTMP
c
      PARAMETER (NN=100, MMAX=100, NMAX=20)
      DIMENSION X1A(M),X2A(N),YA(MMAX,NMAX),Y2A(MMAX,NMAX),
     &          YTMP(NN),Y2TMP(NN),YYTMP(NN)
      DO 12 J=1,M
        DO 11 K=1,N
          YTMP(K)=YA(J,K)
          Y2TMP(K)=Y2A(J,K)
11      CONTINUE
        CALL SPLINT(X2A,YTMP,Y2TMP,N,X2,YYTMP(J))
12    CONTINUE
      CALL SPLINE(X1A,YYTMP,M,0.0,0.0,Y2TMP)
      CALL SPLINT(X1A,YYTMP,Y2TMP,M,X1,Y)
      RETURN
      END
c------------------------------------------------------------------
	subroutine regoln
c
c	Calculates a brightness temperature map of the surface of
c	Mercury in a grid of longitude and latitude elements.
c	The map is determined for a given time (Mercury Hours) and 
c	for a given set of thermal parameters (thermal inertia,
c	thermal skin depth) and electrical parameters (electrical
c	skin depth and refractive index).  Finally, the viewing
c	geometry is applied to find the brightness temperature
c	distribution as seen on Earth.
c
c	D. Mitchell   12 May     1989   UCB
c	              28 January 1990   (latest revision)
c
c	tested OK on 28 January 1990
c------------------------------------------------------------------
c
	integer l,b,midb
	real x,t,lon,lat,dlon,dlat,midlat,bound,radiate
c
c.......Common Blocks: grid, geom, therm
c
	common /grid/ imax,jmax,dx(50),dt,nlon,nlat
	integer imax,jmax,nlon,nlat
	real dx,dt
c
	common /geom/ e,rsun,pi,lonse,latse,pa,rmerc,time
	real e,rsun,pi,lonse,latse,pa,rmerc,time
c
	common /therm/ temp(0:50,0:1000),tb(100,20)
	real temp,tb
c
c.......Initialize Hermographic grid
c
	dlon = pi/float(nlon)
	dlat = pi/float(nlat)
	midlat = float(nlat+1)/2.
	midb = nlat/2 + 1
c
c.......Calculate the brightness temperature for each surface element
c
	do b=midb,nlat
		lat = (float(b) - midlat)*dlat
		do l=1,nlon
			lon = float(l-1)*dlon
			call crankn(lon,lat)
			call smooth
c
			t = bound(time - lon)
			tb(l,b) = radiate(lon,lat,t)
c
c.......Use symmetry for three more surface elements
c
			tb(l,nlat-b+1) = radiate(lon,-lat,t)
			t = bound(t + pi)
			x = bound(lon + pi)
			tb(l+nlon,b) = radiate(x,lat,t)
			tb(l+nlon,nlat-b+1) = radiate(x,-lat,t)
c
		enddo
	enddo
c
	return
	end
c-----------------------------------------------------------------
	subroutine crankn(lon,lat)
	real lon,lat
c
c	CRANKN
c
c	Iterates the thermal diffusion equation with temperature
c	dependent thermal parameters using the Crank-Nicholson
c	method with a predictor-corrector "inner loop" to solve
c	the non-linear difference equations.  The output is the
c	steady-state temperature structure as a function of depth
c	and time (for one solar day) at a given longitude and
c	latitude on the surface.
c
c	D.L. Mitchell    6 October   1989   UCB
c	                23 September 1990   (latest revision)
c
c	tested OK on 23 September 1990
c	algebra checked OK on 8 June 1990
c-----------------------------------------------------------------
c
	integer i,j,k,l,lmax,zapflg
	character line*80
	real x,u,v,w,z,zip,flux
	real a(0:50),b(0:50),c(0:50),r(0:50),tem(0:50)
	real t,tav,teq,trad,cerr,ferr,rerr,flx(0:1000),tol2
	real f,g,h(0:50),uu(0:50),rp(0:50),p,q,ld,xf(2)
c
c.......Common blocks: grid, phys, nonlin, therm, iter, obs
c
	common /grid/ imax,jmax,dx(50),dt,nlon,nlat
	integer imax,jmax,nlon,nlat
	real dx,dt
c
	common /phys/ inertia,delta,albedo,refract,leak
	real inertia,delta,albedo,refract,leak
c
	common /nonlin/ cho,chi,s(7)
	real cho,chi,s
c
	common /therm/ temp(0:50,0:1000),tb(100,20)
	real temp,tb
c
	common /iter/ kmin,kmax,msg,tol,tnot
	integer kmin,kmax,msg
	real tol,tnot
c
	common /obs/ tnu,xmax
	real tnu,xmax
c
c.......Define non-linear heat conduction
c
	z(x) = x*x*x
	u(x) = cho + chi*z(x)
	w(x) = cho + chi*z(x)/4.
c
c.......Initialize the temperature structure at time = lon
c	(fast rotator radiative equilibrium)
c
	f = 0.
	t = lon - dt/2.
	do j=0,(jmax-1)
		t = t + dt
		flx(j) = flux(lon,lat,t)
		f = f + flx(j)
	enddo
	tav = (f/float(jmax) + leak)**0.25
	trad = tav
	ld = leak/inertia
	teq = tav*w(tav) + xmax*ld
c
	temp(0,0) = tav
	do i=1,imax
	  temp(i,0) = temp(i-1,0) + ld*dx(i)/u(temp(i-1,0))
	enddo
c
c.......Initialize thermal diffusion constants
c
	k = 0
	lmax = 50
	tol2 = tol*tol
	zapflg = 0
	p = 4.*dx(1)/inertia
	h(0) = 2.*dx(1)*dx(1)/dt
	do i=1,(imax-1)
		h(i) = 2.*(dx(i+1) + dx(i))/dt
	enddo
	h(imax) = 2.*dx(imax)*dx(imax)/dt
	q = 2.*cho*dx(imax)
c
c.......Iterate the thermal diffusion equation for one solar day
c
10	continue
	do j=0,(jmax-1)
c
c...............Calculate the tri-diagonal predictor coefficients
c
		g = 2.*h(0)*v(temp(0,j))
		uu(0) = u(temp(0,j))
		uu(1) = u(temp(1,j))
		c(0) = -(uu(1) + uu(0))
		a(0) = p*z(temp(0,j))
		b(0) = 2.*a(0) + g - c(0)
		r(0) = (a(0) + g + c(0))*temp(0,j) - c(0)*temp(1,j)
     &		       + p*(flx(j) + leak)
		rp(0) = r(0)
c
		do i=1,(imax-1)
			uu(i+1) = u(temp(i+1,j))
			a(i) = -(uu(i) + uu(i-1))/dx(i)
			c(i) = -(uu(i+1) + uu(i))/dx(i+1)
			b(i) = 2.*h(i)*v(temp(i,j)) - a(i) - c(i)
			r(i) = (b(i) + 2.*(a(i) + c(i)))*temp(i,j)
     &                       - c(i)*temp(i+1,j) - a(i)*temp(i-1,j)
			rp(i) = r(i)
		enddo
c
		g = 2.*h(imax)*v(temp(imax,j))
		c(imax) = chi*dx(imax)*z(temp(imax,j))
		a(imax) = -(uu(imax) + uu(imax-1))
		b(imax) = g - a(imax) + q + 2.*c(imax)
		r(imax) = 4.*dx(imax)*(teq + ld)
     *			  - a(imax)*temp(imax-1,j)
     &                    + (g + a(imax) - q + c(imax))*temp(imax,j)
		rp(imax) = r(imax)
c
c...............Estimate temp(0...imax,j+1) using predictor
c
		call tridagn(a,b,c,r,tem,imax)
		do i=0,imax
			temp(i,j+1) = tem(i)
		enddo
c
		l = 0
20		continue
c
c...............Calculate the tri-diagonal corrector coefficients
c
		f = h(0)*v(temp(0,j+1))
		g = h(0)*v(temp(0,j))
		uu(0) = u(temp(0,j+1))
		uu(1) = u(temp(1,j+1))
		c(0) = -(uu(1) + uu(0))
		b(0) = 2.*a(0) + f + g - c(0)
		r(0) = rp(0) + (f - g)*temp(0,j)
c
		do i=1,(imax-1)
			f = h(i)*v(temp(i,j+1))
			g = h(i)*v(temp(i,j))
			uu(i+1) = u(temp(i+1,j+1))
			a(i) = -(uu(i) + uu(i-1))/dx(i)
			c(i) = -(uu(i+1) + uu(i))/dx(i+1)
			b(i) = f + g - a(i) - c(i)
			r(i) = rp(i) + (f - g)*temp(i,j)
		enddo
c
		f = h(imax)*v(temp(imax,j+1))
		g = h(imax)*v(temp(imax,j))
		a(imax) = -(uu(imax) + uu(imax-1))
		b(imax) = f + g - a(imax) + q + 2.*c(imax)
		r(imax) = rp(imax) + (f - g)*temp(imax,j)
c
c...............Improve the estimate of temp(0...imax,j+1) using corrector
c
		call tridagn(a,b,c,r,tem,imax)
c
c...............Test for P-C loop convergence
c
		cerr = 0.
		do i=0,imax
			f = tem(i) - temp(i,j+1)
			if(f*f .gt. cerr*cerr) then
				cerr = f
			endif
			temp(i,j+1) = tem(i)
		enddo
		l = l + 1
c
		if(l .ge. lmax) then
			write(line,30)
30			format(' CRANKN: P-C loop not converging ')
			call LogWrit(line(1:33))
			pause
		endif
c
		if(cerr*cerr .gt. tol2) then
			go to 20
		endif
c
	enddo
	k = k + 1
c
c.......Compare current and previous temperature vs depth at time = lon
c	(In steady state: temp(i,0) = temp(i,jmax) for all i.)
c
	x = 0.
	xf(1) = 0.
	cerr = temp(0,jmax) - temp(0,0)
	do i=1,imax
		x = x + dx(i)
		g = temp(i,jmax) - temp(i,0)
		if(g*g .gt. cerr*cerr) then
			cerr = g
			xf(1) = x
		endif
	enddo
c
c.......Test for radiative equilibrium at the surface
c
	f = 0.
	do j=1,jmax
		g = (temp(0,j) + temp(0,j-1))/2.
		f = f + g*z(g)
	enddo
	rerr = (f/float(jmax))**0.25 - trad
c
c.......Test for net heat flux equilibrium throughout subsurface
c
	f = 0.
	do j=1,jmax
		g = (temp(0,j) + temp(0,j-1))/2.
		f = f + g*w(g)
	enddo
	f = f/float(jmax)
	tav = zip(f)
c
	if(zapflg .eq. 1) then
		zapflg = 0
	else
		teq = tav*w(tav) + xmax*ld
	endif
c
	x = 0.
	xf(2) = 0.
	ferr = 0.
	do i=1,imax
		x = x + dx(i)
		f = 0.
		do j=1,jmax
			g = (temp(i,j) + temp(i,j-1))/2.
			f = f + g*w(g)
		enddo
		f = f/float(jmax) - x*ld
		g = zip(f) - tav
		if(g*g .gt. ferr*ferr) then
			ferr = g
			xf(2) = x
		endif
	enddo
c
c.......Use the improved equilibrium temperature estimate if k = 2
c
	if(k .eq. 2) then
		temp(0,0) = tav
		do i=1,imax
			temp(i,0) = temp(i-1,0)
     &			            + ld*dx(i)/u(temp(i-1,0))
		enddo
		zapflg = 1
		go to 10
	endif
c
c.......Otherwise check convergence and iterate for another solar day
c
	if(((cerr*cerr .gt. tol2) .or. (ferr*ferr .gt. tol2))
     &	   .and. (k .lt. kmax)) then
c
c...............Heat pertubation to speed flux equilibration
c
		if((k .gt. 3) .and. (k .le. kmin)) then
			t = ferr/2.
		else
			t = 0.
		endif
c
		x = 0.
		temp(0,0) = temp(0,jmax)
		do i=1,imax
		  x = x + dx(i)
		  g = (x - xf(2))/2.
		  temp(i,0) = temp(i,jmax) - t*exp(-g*g)
		enddo
		go to 10
	endif
c
c.......Output convergence diagnostics and return
c
	write(line,50) xf(1),cerr*tnot,xf(2),ferr*tnot,rerr*tnot
50	format(' CRANKN: cerr[',f4.1,'] = ',f7.3,3x,
     &         ' ferr[',f4.1,'] = ',f7.3,3x,' rerr = ',f7.3)
	call LogWrit(line(1:73))
c
	return
	end
c------------------------------------------------------------------
	real function zip(teq)
	real teq
c
c	D. Mitchell    6 April 1990   UCB
c
c	tested OK on 6 April 1990
c------------------------------------------------------------------
c
	real x,w,tav,f,g
c
c.......Common blocks: nonlin, iter
c
	common /nonlin/ cho,chi,s(7)
	real cho,chi,s
c
	common /iter/ kmin,kmax,msg,tol,tnot
	integer kmin,kmax,msg
	real tol,tnot
c
c.......Define non-linear heat conduction
c
	w(x) = cho + chi*x*x*x/4.
c
c.......Solve for average temperature
c
	f = 0.
	g = teq/cho
50	continue
	tav = (f + g)/2.
	if(tav*w(tav) .lt. teq) then
		f = tav
	else
		g = tav
	endif
	if((4.*(g - f)) .ge. tol) then
		go to 50
	endif
c
	zip = (f + g)/2.
c
	return
	end
c----------------------------------------------------------------
      SUBROUTINE TRIDAGN(A,B,C,R,U,N)
      INTEGER N
      REAL A,B,C,R,U
c
c	Numerical Recipes routine which solves for the vector
c	U(0 --> N) given the tridiagonal matrix defined by A,
c	B, C, and R.
c
c	copied from BKYAST$DUA1:[USERLIB.RECIPES]
c
c	modified by D. Mitchell on 3 August 1989
c----------------------------------------------------------------
c
      INTEGER NMAX,J
      REAL GAM,BET
c
      PARAMETER (NMAX=50)
      DIMENSION GAM(0:NMAX),A(0:N),B(0:N),C(0:N),R(0:N),U(0:N)
      IF(B(0).EQ.0.)PAUSE
      BET=B(0)
      U(0)=R(0)/BET
      DO 11 J=1,N
        GAM(J)=C(J-1)/BET
        BET=B(J)-A(J)*GAM(J)
        IF(BET.EQ.0.)PAUSE
        U(J)=(R(J)-A(J)*U(J-1))/BET
11    CONTINUE
      DO 12 J=N-1,0,-1
        U(J)=U(J)-GAM(J+1)*U(J+1)
12    CONTINUE
      RETURN
      END
c-----------------------------------------------------------
	subroutine smooth
c
c	Smooths the temperature structure to get rid of
c	numerical oscillations resulting from the coarse
c	time grid.  As long as the time grid is not too
c	coarse, smoothing gives the same result in less 
c	time as a finer grid.
c
c	D. Mitchell   12 May 1989   UCB
c	              29 May 1990   (latest revision)
c
c	tested OK on 29 May 1990
c-----------------------------------------------------------
c
	integer i,j
c
c.......Common blocks: grid, therm
c
	common /grid/ imax,jmax,dx(50),dt,nlon,nlat
	integer imax,jmax,nlon,nlat
	real dx,dt
c
	common /therm/ temp(0:50,0:1000),tb(100,20)
	real temp,tb
c
c.......Smooth in time at each depth
c
	do i=0,imax
	  temp(i,0) = (temp(i,0) + temp(i,1))/2.
	enddo
	do j=2,(jmax-2),2
	  do i=0,imax
	    temp(i,j) = (temp(i,j) + temp(i,j+1))/2.
	    temp(i,j-1) = (temp(i,j-2) + temp(i,j))/2.
	  enddo
	enddo
	do i=0,imax
	  temp(i,jmax) = temp(i,0)
	  temp(i,jmax-1) = (temp(i,jmax-2) + temp(i,jmax))/2.
	enddo
c
	return
	end
c-------------------------------------------------------------------
	real function flux(lon,lat,t)
	real lon,lat,t
c
c	Calculates the normalized insolation as a function
c	of longitude, latitude, and time.
c
c	D. Mitchell   12 April 1989   UCB
c	              31 May   1989   (latest revision)
c
c	tested OK on 1 June 1990
c-------------------------------------------------------------------
c
	real x,y,f,dist,thet,elev,eta
c
c.......Common blocks: geom, phys
c
	common /geom/ e,rsun,pi,lonse,latse,pa,rmerc,time
	real e,rsun,pi,lonse,latse,pa,rmerc,time
c
	common /phys/ inertia,delta,albedo,refract,leak
	real inertia,delta,albedo,refract,leak
c
c.......Determine elevation and distance of the sun
c
	call orbit(t,dist,thet)
	elev = asin(cos(thet - lon)*cos(lat))
c
	eta = rsun/dist
	x = elev/eta
c
c.......If sun has set then flux is zero
c
	if(x .le. -1.) then
	  flux = 0.
c
c.......If sun is partially below the horizon
c
	else if(x .lt. 1.) then
	  y = sqrt(1. - x*x)
	  f = 0.5 + (x*y + asin(x))/pi
	  elev = elev + 2.*eta*(y**3.)/(3.*pi*f)
	  flux = 4.*(1. - albedo)*f*sin(elev)/(dist*dist)
c
c.......Otherwise sun is completely above the horizon
c
	else
	  flux = 4.*(1. - albedo)*sin(elev)/(dist*dist)
	endif
c
	return
	end
c-------------------------------------------------------------
	subroutine orbit(t,dist,thet)
	real t,dist,thet
c
c	Calculates the sub-solar longitude and sun-planet 
c	distance for Mercury given the time.  This routine
c	achieves total orbital energy conservation to a few
c	parts in 1000 and positional accuracy to a few parts
c	in a million.
c	The method is described in Moulton (pp. 158-165).
c
c	D.L. Mitchell    30 May 1990    UCB
c
c	tested OK on 31 May 1990
c-------------------------------------------------------------
c
	integer n
	character line*80
	real m,x,eps,eps1,bound
c
c.......Common block: geom, orb
c
	common /geom/ e,rsun,pi,lonse,latse,pa,rmerc,time
	real e,rsun,pi,lonse,latse,pa,rmerc,time
c
	common /orb/ nmax,pi2,sre,oerr,lonss
	integer nmax
	real pi2,sre,oerr,lonss
c
	n = 0
	m = bound(2.*t)
	eps1 = m + e*(sin(m) + e*sin(2.*m)/2.)
c
10	continue
	eps = m + e*sin(eps1)
	x = eps - eps1
	if(x*x .ge. oerr) then
		eps1 = eps
		n = n + 1
		if(n .gt. nmax) then
			write(line,20)
20			format(' ORBIT: convergence error ')
			call LogWrit(line(1:26))
			pause
		endif
		go to 10
	endif
c
	dist = 1. - e*cos(eps)
	thet = bound(3.*t - 2.*atan(sre*tan(eps/2.)))
c
	return
	end
c---------------------------------------------------------------
      SUBROUTINE ZBRAC(X1,X2,SUCCES)
      LOGICAL SUCCES
      REAL X1,X2
c
c	Numerical Recipes routine to bracket a zero-crossing
c	interval of the function defined by FUNC3
c
c	copied from bkyast$dua1:[userlib.recipes]
c---------------------------------------------------------------
c
      INTEGER J,NTRY
      REAL FACTOR,F1,F2,FUNC3,ABS
c
      PARAMETER (FACTOR=1.2,NTRY=6)
      IF(X1.EQ.X2)PAUSE 'You have to guess an initial range'
      F1=FUNC3(X1)
      F2=FUNC3(X2)
      SUCCES=.TRUE.
      DO 11 J=1,NTRY
        IF(F1*F2.LT.0.)RETURN
        IF(ABS(F1).LT.ABS(F2))THEN
          X1=X1+FACTOR*(X1-X2)
          F1=FUNC3(X1)
        ELSE
          X2=X2+FACTOR*(X2-X1)
          F2=FUNC3(X2)
        ENDIF
11    CONTINUE
      SUCCES=.FALSE.
      RETURN
      END
c----------------------------------------------------------------------
      REAL FUNCTION RTBIS(X1,X2,XACC)
      REAL X1,X2,XACC
c
c	Numerical Recipes routine which finds the root of the function
c	defined by FUNC3 between the limits X1 and X2 by the method of
c	bisection.
c
c	copied from bkyast$dua1:[userlib.recipes]
c----------------------------------------------------------------------
c
      INTEGER JMAX,J
      REAL FMID,FUNC3,F,DX,ABS,XMID
c
      PARAMETER (JMAX=40)
      FMID=FUNC3(X2)
      F=FUNC3(X1)
      IF(F*FMID.GE.0.) PAUSE 'Root must be bracketed for bisection.'
      IF(F.LT.0.)THEN
        RTBIS=X1
        DX=X2-X1
      ELSE
        RTBIS=X2
        DX=X1-X2
      ENDIF
      DO 11 J=1,JMAX
        DX=DX*.5
        XMID=RTBIS+DX
        FMID=FUNC3(XMID)
        IF(FMID.LE.0.)RTBIS=XMID
        IF(ABS(DX).LT.XACC .OR. FMID.EQ.0.) RETURN
11    CONTINUE
      PAUSE 'too many bisections'
      END
c---------------------------------------------------------------
	real function func3(t)
	real t
c
c	Mercury time refinement by sub-solar longitude.
c
c	D.L. Mitchell  18 June 1990   UCB
c
c---------------------------------------------------------------
c
	real x,y,lon
c
c.......Common Block: orb
c
	integer nmax
	real pi2,sre,oerr,lonss
	common /orb/ nmax,pi2,sre,oerr,lonss
c
c.......Refine time estimate to match sub-solar point
c
	call orbit(t,x,lon)
	x = cos(lonss)
	y = sin(lonss)
	if(x*x .lt. y*y) then
		func3 = cos(lon) - x
	else
		func3 = sin(lon) - y
	endif
c
	return
	end
c--------------------------------------------------------------
	real function radiate(lon,lat,t)
	real lon,lat,t
c
c	Integrates the radiative transport equation with the
c	Romberg method (Numerical Recipes) given the temper-
c	ature structure at a given longitude, latitude and
c	Mercury time.  It then applies the Earth-Mercury
c	geometry to determine the observed brightness temp.
c	of the surface element.
c
c	D. Mitchell   16 April     1989   UCB
c	              23 September 1990   (latest revision)
c
c	tested OK on 23 September 1990
c--------------------------------------------------------------
c
	integer i,j
	character line*80
	real x,xmax,thi,thf,cthf
c
c.......Externals
c
	real bound2
c
c.......Common blocks: grid, geom, phys, nonlin, therm, iter, rad
c
	common /grid/ imax,jmax,dx(50),dt,nlon,nlat
	integer imax,jmax,nlon,nlat
	real dx,dt
c
	common /geom/ e,rsun,pi,lonse,latse,pa,rmerc,time
	real e,rsun,pi,lonse,latse,pa,rmerc,time
c
	common /phys/ inertia,delta,albedo,refract,leak
	real inertia,delta,albedo,refract,leak
c
	common /nonlin/ cho,chi,s(7)
	real cho,chi,s
c
	common /therm/ temp(0:50,0:1000),tb(100,20)
	real temp,tb
c
	common /iter/ kmin,kmax,msg,tol,tnot
	integer kmin,kmax,msg
	real tol,tnot
c
	integer n
	real a,xa(51),ya(51),y2a(51),bwb0
	common /rad/ n,a,xa,ya,y2a,bwb0
c
c.......Determine the viewing geometry
c
	cthf = cos(lon - lonse)*cos(lat)*cos(latse)
     *					 + sin(lat)*sin(latse)
	cthf = bound2(cthf)
	thf = acos(cthf)
c
c.......If the surface element is not visible from Earth
c
	if(thf .gt. pi/2.) then
		radiate = 0.
		go to 10
	endif
c
c.......Otherwise initialize the radiative transport integrand ...
c
	thi = asin(sin(thf)/refract)
	a = delta*cos(thi)
	j = int(t/dt + 0.5)
	n = imax + 1
c
	xa(1) = 0.
	ya(1) = temp(0,j)
	do i=2,n
		xa(i) = xa(i-1) + dx(i-1)
		ya(i) = temp(i-1,j)
	enddo
	bwb0 = ya(n)*(cho + chi*ya(n)*ya(n)*ya(n)/4.)
c
	call spline(xa,ya,n,1.e30,1.e30,y2a)
c
c.......then integrate the radiative transport equation
c
	xmax = 12.*delta
	call qromb(xa(1),xmax,x)
	radiate = x/a
c
10	continue
c
	x = 180./pi
	write(line,20) lon*x,lat*x,radiate*tnot
20	format(' RADIATE: lon = ',f6.2,3x,' lat = ',f6.2,3x,
     *						' Tb = ',f6.2)
	call LogWrit(line(1:53))
c
	return
	end
c-------------------------------------------------------------
      SUBROUTINE QROMB(A,B,SS)
      REAL A,B,SS
c
c	Numerical Recipes routine which performs Romberg
c	integration of the function defined in FUNC.FOR
c
c	copied from BKYAST$DUA1:[USERLIB.RECIPES]
c
c-------------------------------------------------------------
      INTEGER JMAX,JMAXP,J,K,L,KM
      REAL EPS,S,H,DSS,ABS
c
      PARAMETER(EPS=1.E-6,JMAX=20,JMAXP=JMAX+1,K=5,KM=4)
      DIMENSION S(JMAXP),H(JMAXP)
      H(1)=1.
      DO 11 J=1,JMAX
        CALL TRAPZD(A,B,S(J),J)
        IF (J.GE.K) THEN
          L=J-KM
          CALL POLINT(H(L),S(L),K,0.,SS,DSS)
          IF (ABS(DSS).LT.EPS*ABS(SS)) RETURN
        ENDIF
        S(J+1)=S(J)
        H(J+1)=0.25*H(J)
11    CONTINUE
      PAUSE 'Too many steps.'
      END
c---------------------------------------------------
      SUBROUTINE TRAPZD(A,B,S,N)
      INTEGER N
      REAL A,B,S
c
c	Numerical Recipes routine which performs
c	the trapezoidal rule to obtain successive 
c	approximations to an integral
c
c	copied from BKYAST$DUA1:[USERLIB.RECIPES]
c---------------------------------------------------
c
      INTEGER J,IT
      REAL X,TNM,SUM,DEL,FUNC
c
      IF (N.EQ.1) THEN
        S=0.5*(B-A)*(FUNC(A)+FUNC(B))
        IT=1
      ELSE
        TNM=IT
        DEL=(B-A)/TNM
        X=A+0.5*DEL
        SUM=0.
        DO 11 J=1,IT
          SUM=SUM+FUNC(X)
          X=X+DEL
11      CONTINUE
        S=0.5*(S+(B-A)*SUM/TNM)
        IT=2*IT
      ENDIF
      RETURN
      END
c----------------------------------------------------
      SUBROUTINE POLINT(XA,YA,N,X,Y,DY)
      INTEGER N
      REAL XA,YA,X,Y,DY
c
c	Numerical Recipes routine which extrapolates
c	the successive approximations obtained by
c	QROMB to zero step size.
c
c	copied from BKYAST$DUA1:[USERLIB.RECIPES]
c
c----------------------------------------------------
c
      INTEGER I,M,NMAX,NS
      REAL C,D,DIF,DIFT,HO,HP,W,DEN
c
      PARAMETER (NMAX=10) 
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N 
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END
c--------------------------------------------------------------
	real function func(x)
	real x
c
c	The radiative transport equation given the temperature
c	structure at a given longitude, latitude and Mercury
c	time.
c
c	D. Mitchell   30 July      1989   UCB
c	               7 June      1990   (latest revision)
c
c	tested OK on 7 June 1990
c--------------------------------------------------------------
c
	real y,zip
c
c.......Common blocks: phys, nonlin, obs, rad
c
	common /phys/ inertia,delta,albedo,refract,leak
	real inertia,delta,albedo,refract,leak
c
	common /nonlin/ cho,chi,s(7)
	real cho,chi,s
c
	common /obs/ tnu,xmax
	real tnu,xmax
c
	common /rad/ n,a,xa(51),ya(51),y2a(51),bwb0
	integer n
	real a,xa,ya,y2a,bwb0
c
	if(x .gt. xmax) then
		y = zip(bwb0 + (x - xmax)*leak/inertia)
	else
		call splint(xa,ya,y2a,n,x,y)
	endif
c
	func = tnu*exp(-x/a)/(exp(tnu/y) - 1.)
c
	return
	end
c-----------------------------------------------------------
	real function reflect(lon,lat)
	real lon,lat
c
c	Calculates the Fresnel reflection coefficient at the
c	surface-vacuum interface for radiation propagating
c	through the sub-surface at angle thi to the surface
c	normal and emerging at angle thf toward the observer.
c	Assumes a perfectly smooth, spherical surface.
c
c	D. Mitchell   17 April 1989   UCB
c	               1 June  1989   (latest revision)
c
c	tested OK on 1 June 1989
c-----------------------------------------------------------
c
	real perp,par,thi,thf,cthf
c
c.......Externals
c
	real bound2
c
c.......Common blocks: geom, phys
c
	common /geom/ e,rsun,pi,lonse,latse,pa,rmerc,time
	real e,rsun,pi,lonse,latse,pa,rmerc,time
c
	common /phys/ inertia,delta,albedo,refract,leak
	real inertia,delta,albedo,refract,leak
c
c.......Determine the viewing geometry
c
	cthf = cos(lon-lonse)*cos(lat)*cos(latse)
     *					 + sin(lat)*sin(latse)
	cthf = bound2(cthf)
	thf = acos(cthf)
	thi = asin(sin(thf)/refract)
c
c.......Compute the Fresnel reflection coefficient
c
	if(thi .lt. 0.001) then
		perp = (1. - refract)/(1. + refract)
		par = perp
	else
		perp = sin(thi - thf)/sin(thi + thf)
		par = tan(thi - thf)/tan(thi + thf)
	endif
c
	reflect = (perp*perp + par*par)/2.
c
	return
	end
c------------------------------------------------------------------
	real function bound(x)
	real x
c
c	Given an angle in radians (x), returns the same
c	angle between the limits of 0 and 2*pi
c
c	D.L. Mitchell   1 June 1989   UCB
c
c	tested OK on 1 June 1989
c------------------------------------------------------------------
c
	integer n,int
c
	common /orb/ nmax,pi2,sre,oerr,lonss
	integer nmax
	real pi2,sre,oerr,lonss
c
	n = int(x/pi2)
	if(x .lt. 0.) n = n - 1
	bound = x - pi2*float(n)
c
	return
	end
c------------------------------------------------------------------
	real function bound2(x)
	real x
c
c	Given an angle in radians (x), returns the same
c	angle between the limits of 0 and 2*pi
c
c	D.L. Mitchell   11 May 1991   UCB
c
c	tested OK on 11 May 1991
c------------------------------------------------------------------
c
	if(x .lt. -1.) then
		bound2 = -1.
	else if(x .gt. 1.) then
		bound2 = 1.
	else
		bound2 = x
	endif
c
	return
	end
c--------------------------------------------------------------
	real function v(x)
	real x
c
c	The heat capacity as a function of temperature.
c
c	D. Mitchell   9 October 1989   UCB
c
c	tested OK on 13 October 1989
c--------------------------------------------------------------
c
	real y
c
	common /nonlin/ cho,chi,s(7)
	real cho,chi,s
c
	common /iter/ kmin,kmax,msg,tol,tnot
	integer kmin,kmax,msg
	real tol,tnot
c
	y = x*tnot
	if(y .lt. 350.) then
	  v = s(1) + x*(s(2) + x*(s(3) + x*(s(4) + x*s(5))))
	else
	  v = s(6) + s(7)*(1. - exp((350. - y)/100.))
	endif
c
	return
	end
