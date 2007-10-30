c************************************************************************
	program  offpol
	implicit none
c
c************************************************************************
	program paraplot
	implicit none
c
c= offpol -- Generate ATCA primary beam polarimetric response.
c& rjs
c: utility
c+
c	OFFPOL generates images of the polarimetric response of the
c	ATCA as a function of position in the primary beam. This
c	performs a simple simulation of an observation.
c@ out
c	Output name template. No default.
c@ harange
c	Hour angle range to simulate. This gives the start and
c	end hour angles, and a simulation step size. The default is
c	to simulate a snapshot at 0 hours. The default step size is
c	0.1 hours (6 minutes). This might be inadequate for sources
c	with a declination near -30 degrees.
c@ dec
c	Declination of the source. The default is -45 degrees.
c--
c  History:
c    rjs  24apr97 Original version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	double precision lat
	real chioff
	logical rotate
	character version*(*)
	parameter(lat=-30.d0*DPI/180.d0,chioff=0.25*PI)
	parameter(rotate=.true.)
	parameter(version='Offpol: version 1.0 24-Apr-97')
	integer iha,nha
	double precision dha,ha0,ha1,ha
c
	real rad,psi,chi,x,y,pb
	double precision sfreq,delta,dec
	integer ic,jc,nx,ny,i,j,ifreq,tiir,tqqr,tuur,tvvr,lout
	character c*1,out*64
	complex xx,yy,xy,yx,jo(2,2),t,qq,uu
	real qqr(MAXDIM),iir(MAXDIM),uur(MAXDIM),vvr(MAXDIM)
	logical flag(MAXDIM)
c
	call output(version)
	call keyini
	call keya('out',out,' ')
	lout = len1(out)
	if(lout.eq.0)call bug('f','An output must be given')
	call keyt('harange',ha0,'hms',0.d0)
	call keyt('harange',ha1,'hms',ha0)
	call keyt('harange',dha,'hms',0.1d0)
	nha = nint((ha1 - ha0)/dha) + 1
	call keyt('dec',dec,'dms',-0.25d0*DPI)
	call keyfin
c
	do ifreq=1,2
	  if(ifreq.eq.1)then
	    sfreq =1.384
	    c = 'l'
	    delta = 34.61
	  else
	    sfreq = 2.368
	    c = 's'
	    delta = 20.9882
	  endif
	  nx = 255
	  ny = 255
	  ic = nx/2 + 1
	  jc = ny/2 + 1
	  delta = 2 * delta * PI/180/60 / nx
c
	  call mkopen(tiir,out(1:lout)//'.i'//c,1,sfreq,version,nx,ny,
     *							delta,dec)
	  call mkopen(tqqr,out(1:lout)//'.q'//c,2,sfreq,version,nx,ny,
     *							delta,dec)
	  call mkopen(tuur,out(1:lout)//'.u'//c,3,sfreq,version,nx,ny,
     *							delta,dec)
	  call mkopen(tvvr,out(1:lout)//'.v'//c,4,sfreq,version,nx,ny,
     *							delta,dec)
c
	  do j=1,ny
	    do i=1,nx
	      iir(i) = 0
	      qqr(i) = 0
	      uur(i) = 0
	      vvr(i) = 0
	      flag(i) = sqrt(real((i-ic)**2+(j-jc)**2)).lt.nx/2
	    enddo
	    do iha=1,nha
	      ha = dha*(iha-1) + ha0
	      call parang(0.d0,dec,ha,lat,chi)
	      chi = chi + chioff
	      do i=1,nx
	        if(i.ne.ic.or.j.ne.jc)then
	          x = -(i - ic)*delta
	          y = (j - jc)*delta
	          rad = sqrt(x**2+y**2)
	          psi = atan2(x,y)
	          call GetJones(rad,psi-chi,sfreq,Jo,pb)
	          XX = real(Jo(1,1))**2 + aimag(Jo(1,1))**2 +
     *		     real(Jo(1,2))**2 + aimag(Jo(1,2))**2
	          YY = real(Jo(2,2))**2 + aimag(Jo(2,2))**2 +
     *		     real(Jo(2,1))**2 + aimag(Jo(2,1))**2
	          t =  Jo(1,1)*conjg(Jo(2,1)) + conjg(Jo(2,2))*Jo(1,2)
	          XY = t
	          YX = conjg(t)
	        else
		  xx = 0
		  yy = 0
		  xy = 0
		  yx = 0
		  pb = 0
	        endif
c
	        iir(i) = iir(i) + 0.5*real(xx+yy) - pb
	        qq = 0.5*real(xx-yy)
	        uu = 0.5*real(xy+yx)
	        if(rotate)then
		  qqr(i) = qqr(i) + qq*cos(2*chi) - uu*sin(2*chi)
		  uur(i) = uur(i) + qq*sin(2*chi) + uu*cos(2*chi)
		else
		  qqr(i) = qqr(i) + qq
		  uur(i) = uur(i) + uu
	        endif
	        vvr(i) = vvr(i) + 0.5*real( (0.0,-1.0)*(xy-yx))
	      enddo
	    enddo
	    do i=1,nx
	      iir(i) = iir(i) / nha
	      qqr(i) = qqr(i) / nha
	      uur(i) = uur(i) / nha
	      vvr(i) = vvr(i) / nha
	    enddo
	    call xywrite(tiir,j,iir)
	    call xyflgwr(tiir,j,flag)
	    call xywrite(tqqr,j,qqr)
	    call xyflgwr(tqqr,j,flag)
	    call xywrite(tuur,j,uur)
	    call xyflgwr(tuur,j,flag)
	    call xywrite(tvvr,j,vvr)
	    call xyflgwr(tvvr,j,flag)
	  enddo
c
	  call xyclose(tiir)
	  call xyclose(tqqr)
	  call xyclose(tuur)
	  call xyclose(tvvr)
	enddo
c
	end
c************************************************************************
	subroutine GetJones(rad,psi,freq,Jo,pb)
c
	implicit none
	real rad,psi,pb
	double precision freq
	complex Jo(2,2)
c
c  Compute the corresponding Jones matrix at a particular position in the
c  primary beam.
c------------------------------------------------------------------------
	include 'mirconst.h'
c
c  Alpha2 = 4*log(2).
c
	real alpha2
	parameter(alpha2=2.772589)
c
	integer i
	real rdist,x(7),px,py
	real coeffs(2,7),coeffl(2,5)
	save coeffs,coeffl
	data coeffs/
     *  1.3992E+00,   0.0000E+00,
     *  6.6962E-02,   9.2800E-01,
     * -8.1047E-02,   4.6582E-02,
     *  5.5058E-02,  -4.5728E-02,
     *  4.2927E-02,  -1.5807E-02,
     *  5.2665E-02,  -3.8708E-02,
     * -1.8535E-02,   1.3006E-02/
c
	data coeffl/
     *  1.3523E+00,   0.0000E+00,
     * -8.9658E-02,   4.1000E+00,
     * -1.2942E-02,   6.4604E-03,
     *  1.5156E-02,  -1.0285E-02,
     * -1.5113E-03,   5.0859E-03/

c
c  Compute the coefficients of the trig functions in the Jones
c  matrix. Also compute the primary beam response.
c
c  13-cm response.
c
	if(freq.gt.2)then
	  rdist =  rad / ( 2.368/freq * 20.9882*PI/180/60 )
	  x(1) = exp(-alpha2*(rdist/coeffs(1,1))**2)
	  x(2) = coeffs(1,2)*sin(0.5*PI*rdist/coeffs(2,2))**2
	  pb = x(1)*x(1) + 0.5*x(2)*x(2)
	  do i=3,7
	    x(i) = (coeffs(2,i)*rdist + coeffs(1,i))*rdist
	    pb = pb + 0.5*x(i)*x(i)
	  enddo
c
	  px = psi
	  py = -psi - 0.5*PI
  	  Jo(1,1) = x(1) + x(2)*cos(2*px) + x(3)*cos(px) + x(4)*sin(px)
	  Jo(2,2) = x(1) + x(2)*cos(2*py) + x(3)*cos(py) + x(4)*sin(py)
	  py = -py
	  Jo(1,2) =   cmplx(x(5),x(6))*sin(px) + cmplx(x(7),0.)*cos(px)
	  Jo(2,1) = -(cmplx(x(5),x(6))*sin(py) + cmplx(x(7),0.)*cos(py))
c
c  20-cm response.
c
	else
	  rdist =  rad / ( 1.384/freq * 34.61*PI/180/60 )
	  x(1) = exp(-alpha2*(rdist/coeffl(1,1))**2)
	  x(2) = coeffl(1,2)*sin(0.5*PI*rdist/coeffl(2,2))**2
	  pb = x(1)*x(1) + 0.5*x(2)*x(2)
	  do i=3,5
	    x(i) = (coeffl(2,i)*rdist + coeffl(1,i))*rdist
	    pb = pb + 0.5*x(i)*x(i)
	  enddo
c
	  px = psi
	  py = -psi - 0.5*PI
  	  Jo(1,1) = x(1) + x(2)*cos(2*px) + x(3)*cos(px) + x(4)*sin(px)
	  Jo(2,2) = x(1) + x(2)*cos(2*py) + x(3)*cos(py) + x(4)*sin(py)
	  py = -py
	  Jo(1,2) =  x(5)*sin(2*px)
	  Jo(2,1) = -x(5)*sin(2*py)
	endif	
c
	end
c************************************************************************
	subroutine mkopen(tno,name,stokes,sfreq,version,nx,ny,delta,dec)
c
	implicit none
	integer tno,stokes,nx,ny
	double precision sfreq,delta,dec
	character name*(*),version*(*)
c
c------------------------------------------------------------------------
	integer nsize(4),coObj
	character line*64
c
	call coCreate(coObj)
	call coAxSet(coObj,1,'RA---SIN',dble(nx/2+1),0.d0,-delta)
	call coAxSet(coObj,2,'DEC--SIN',dble(ny/2+1),dec, delta)
	call coAxSet(coObj,3,'FREQ',    1.d0,sfreq,0.1d0)
	call coAxSet(coObj,4,'STOKES',  1.d0,dble(stokes),1.d0)
	call coReInit(coObj)
	nsize(1) = nx
	nsize(2) = ny
	nsize(3) = 1
	nsize(4) = 1
	call xyopen(tno,name,'new',4,nsize)
	call hisopen(tno,'write')
	line = 'OFFPOL: Miriad '//version
	call hiswrite(tno,line)
	call hisinput(tno,'OFFPOL')
	call hisclose(tno)
	call coWrite(coObj,tno)
	call coFin(coObj)
	end
	

