c************************************************************************
	program equator
c
c= equator -- Find the "equator" of the emission.
c& rjs
c: map analysis
c+
c@ in
c	The input file name. No default.
c@ log
c	The output file. The default is not to produce it.
c@ device
c	PGPLOT device to plot the latitude of the equator.
c@ nbins
c	Default is 30.
c@ angle
c@ options
c	magnetic
c--
c
c  History:
c    05feb96 rjs   Original version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mirconst.h'
	character version*(*)
	parameter(version = 'Equator: version 1.0 05-Feb-96' )
c
	integer MAXBINS
	parameter(MAXBINS=100)
	integer nsize(3),i,j,k,ilong,tno,nbins,ilat
	integer npnts
	character in*64,out*64,device*64,line*64,ctype*16
	real x(MAXBINS,3),y(MAXBINS,3),z(MAXBINS,3)
	real xc(3)
	real val(MAXBINS,3),lat(MAXBINS),long(MAXBINS),rad(MAXBINS)
	real dat(MAXBINS)
	real data(MAXDIM),lat1,long1
	real xlo,xhi,ylo1,yhi1,ylo2,yhi2,ylo3,yhi3,tant
	real xmin,xmax,ymin1,ymax1,ymin2,ymax2,ymin3,ymax3
	real xd,yd,zd,xyd,t1,t2
	real cdelt1,cdelt2,cdelt3,crpix1,crpix2,crpix3
	logical flags(MAXDIM),mag,more
	real a(3,3),b(3),Rj
c
c  Externals
c
	integer pgbeg
	character itoaf*4
c
	call output(version)
	call keyini
	call keya('in',in,' ')
	call keya('device',device,'/xs')
	call keyi('nbins',nbins,30)
	call keyr('angle',tant,100.0)
	call keya('log',out,' ')
	call options('options','magnetic',mag,1)
	call keyfin
c
	call JupCvt(a,b,Rj)
c
	tant = tant * PI/180
	if(nbins.le.1)call bug('f','Illegal value for nbins')
	nbins = min(nbins,MAXBINS)
	do j=1,3
	do i=1,nbins
	  val(i,j) = 0
	  x(i,j) = 0
	  y(i,j) = 0
	  z(i,j) = 0
	enddo
	enddo
c
	if(in.eq.' ')call bug('f','An input must be given')
	call xyopen(tno,in,'old',3,nsize)
	call rdhdr(tno,'cdelt1',cdelt1,1.0)
	call rdhdr(tno,'cdelt2',cdelt2,1.0)
	call rdhdr(tno,'cdelt3',cdelt3,1.0)
	call rdhdr(tno,'crpix1',crpix1,1.0)
	call rdhdr(tno,'crpix2',crpix2,1.0)
	call rdhdr(tno,'crpix3',crpix3,1.0)
	call rdhda(tno,'ctype1',ctype,' ')
	call ucase(ctype)
	if(ctype.eq.'ANGLE')then
	  continue
	else if(ctype(1:6).eq.'RADIUS')then
	  Rj = 1
	else
	  call bug('f','Unrecognised axis type')
	endif
c
c  Find the emission "equator".
c
	do k=1,nsize(3)
	  call xysetpl(tno,1,k)
	  do j=1,nsize(2)
	    call xyread(tno,j,data)
	    call xyflgrd(tno,j,flags)
	    do i=1,nsize(1)
	      xd = (i-crpix1)*cdelt1
	      yd = (j-crpix2)*cdelt2
	      xyd = sqrt(xd*xd + yd*yd)
	      zd = (k-crpix3)*cdelt3
	      if(abs(xd)+abs(yd).gt.0.and.flags(i))then
		long1 = atan2(-yd,xd)
		lat1  = atan2(zd,xyd) +
     *			   10./180.*PI*cos(long1-200./180.*PI)
		ilong = nint(nbins*(long1/(2*PI) + 0.5) + 1)
		if(ilong.gt.nbins)ilong = 1
		if(lat1.gt.tant)then
		  ilat = 2
		else if(lat1.lt.-tant)then
		  ilat = 3
		else
		  ilat = 1
		endif
		if(data(i).gt.val(ilong,ilat))then
		  x(ilong,ilat) = i
		  y(ilong,ilat) = j
		  z(ilong,ilat) = k
		  val(ilong,ilat) = data(i)
		endif
	      endif
	    enddo
	  enddo
	enddo
c
c  Refine the positions.
c
	call output('Doing position refinement ...')
	do j=1,3
	  do i=1,nbins
	    if(x(i,j).ne.0.or.y(i,j).ne.0.or.z(i,j).ne.0)then
	      xc(1) = x(i,j)
	      xc(2) = y(i,j)
	      xc(3) = z(i,j)
	      call solve3(tno,xc,val(i,j))
	      x(i,j) = xc(1)
	      y(i,j) = xc(2)
	      z(i,j) = xc(3)
	    endif
	  enddo
	enddo
c
c  Create the plots.
c
	call logopen(out,' ')
	if(device.ne.' ')then
	  call output('Creating the plots')
	  if(pgbeg(0,device,1,3).ne.1)then
	    call pgldev
	    call bug('f','Error opening graphics device')
	  endif
	  call pgsch(1.6)
	  call pgscf(2)
	  do j=1,3
	  npnts = 0
	  do i=1,nbins
	    if(x(i,j).ne.0.or.y(i,j).ne.0.or.z(i,j).ne.0)then
	      npnts = npnts + 1
	      xd = cdelt1*(x(i,j)-crpix1) / Rj
	      yd = cdelt2*(y(i,j)-crpix2) / Rj
	      zd = cdelt3*(z(i,j)-crpix3) / Rj
c
	      if(mag)then
		xd = xd - b(1)
		yd = yd - b(2)
		zd = zd - b(3)
		t1 = a(1,1)*xd + a(1,2)*yd + a(1,3)*zd
		t2 = a(2,1)*xd + a(2,2)*yd + a(2,3)*zd
		zd = a(3,1)*xd + a(3,2)*yd + a(3,3)*zd
		xd = t1
		yd = t2
	      endif
	      long(i) = 180/PI*atan2(-yd,xd)
	      if(long(i).lt.0)long(i) = long(i) + 360
	      xd = sqrt(xd*xd + yd*yd)
	      lat(i)  = 180/PI*atan2(zd,xd)
	      rad(i)  = sqrt(xd*xd + zd*zd)
	      dat(i)  = val(i,j)
	      if(npnts.eq.1)then
	        xmin = long(i)
	        xmax = xmin
	        ymin1 = lat(i)
	        ymax1 = ymin1
	        ymin2 = rad(i)
	        ymax2 = ymin2
	        ymin3 = dat(i)
	        ymax3 = ymin3
	      else
	        xmin = min(xmin,long(i))
	        xmax = max(xmax,long(i))
	        ymin1 = min(ymin1,lat(i))
	        ymax1 = max(ymax1,lat(i))
	        ymin2 = min(ymin2,rad(i))
	        ymax2 = max(ymax2,rad(i))
	        ymin3 = min(ymin3,dat(i))
	        ymax3 = max(ymax3,dat(i))
	      endif
	    endif
	  enddo
c
	  if(npnts.gt.0)then
	    call pgrnge(xmin,xmax,xlo,xhi)
	    call pgrnge(ymin1,ymax1,ylo1,yhi1)
	    call pgrnge(ymin2,ymax2,ylo2,yhi2)
	    call pgrnge(ymin3,ymax3,ylo3,yhi3)
c
	    call pgpage
	    call pgvstd
	    call pgswin(xlo,xhi,ylo1,yhi1)
	    call pgbox('BCNST',0.,0,'BCNST',0.,0)
	    call pgpt(npnts,long,lat,17)
	    call pglab('\gl\dIII\u (degrees)','Latitude (deg)',' ')
c
	    call pgpage
	    call pgvstd
	    call pgswin(xlo,xhi,ylo2,yhi2)
	    call pgbox('BCNST',0.,0,'BCNST',0.,0)
	    call pgpt(npnts,long,rad,17)
	    call pglab('\gl\dIII\u (degrees)',
     *				'Radius (R\dJ\u)',' ')
c
	    call pgpage
	    call pgvstd
	    call pgswin(xlo,xhi,ylo3,yhi3)
	    call pgbox('BCNST',0.,0,'BCNST',0.,0)
	    call pgpt(npnts,long,dat,17)
	    call pglab('\gl\dIII\u (degrees)','Intensity (Jy/beam)',
     *								' ')
c
c  Now write out the output file, if needed.
c
	    if(out.ne.' ')then
	      line = '# Latitude range '//itoaf(j)
	      call logwrite(line,more)
	      do i=1,npnts
		write(line,'(1p4e15.7)')long(i),lat(i),rad(i),dat(i)
		call logwrite(line,more)
	      enddo
	    endif
	      
	  endif
	  enddo
	  call pgend	  
	endif
c
	call logclose
	call xyclose(tno)
c
	end
c************************************************************************
	subroutine JupCvt(a,b,Rj)
c
	implicit none
	real a(3,3),b(3),Rj
c
c  Conversion matrices to convert to magnetic lat and long.
c------------------------------------------------------------------------
	include 'mirconst.h'
	real Off,Offl3,Tilt,Tiltl3
	parameter(Off=0.07,Offl3=130.0,Tilt=-9.6,Tiltl3=198.0)
	real Mat(3,3)
c
	Rj = 71492/149.597870e6/4.04
	b(1) = Off*cos(-Offl3*PI/180.)
	b(2) = Off*sin(-Offl3*PI/180.)
	b(3) = 0
c
	call Rot(2,1,-Tiltl3*PI/180.,a)
	call Rot(1,3,Tilt*PI/180.,Mat)
	call MMult(a,Mat)
	call Rot(1,2,-Tiltl3*PI/180.,Mat)
	call MMult(a,Mat)
	end
c************************************************************************
	subroutine Rot(i1,i2,angle,a)
c
	implicit none
	integer i1,i2
	real angle,a(3,3)
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer j
c
	do j=1,3
	  a(1,j) = 0
	  a(2,j) = 0
	  a(3,j) = 0
	  a(j,j) = 1
	enddo
c
	a(i1,i1) = cos(angle)
	a(i1,i2) = sin(angle)
	a(i2,i1) = -a(i1,i2)
	a(i2,i2) =  a(i1,i1)
c
	end
c************************************************************************
	subroutine MMult(a,b)
c
	implicit none
	real a(3,3),b(3,3)
c
c  a = a * b
c------------------------------------------------------------------------
	integer i,j,k
	real v(3)
c
	do i=1,3
	  do j=1,3
	    v(j) = a(i,j)
	  enddo
	  do k=1,3
	    a(i,k) = v(1)*b(1,k) + v(2)*b(2,k) + v(3)*b(3,k)
	  enddo
	enddo
c
	end

c************************************************************************
	subroutine solve3(tno,xc,f)
c
	implicit none
	integer tno
	real xc(3),f
c
c  Do parabolic interpolation in three variables to estimate the
c  location of a peak in 3D.
c
c  Input:
c    tno
c  Output:
c    xc
c    f
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer NCOEFF,NDATA,NW
	parameter(NW=3,NCOEFF=10,NDATA=NW*NW*NW)
	real fxyz(NDATA),coeff(NCOEFF,NDATA),rtemp(NCOEFF*NCOEFF)
	real c(NCOEFF),x,y,z,b(3,3),tx
	integer i,j,k,l,itemp(NCOEFF),ifail
	real data(MAXDIM)
	integer ic(3)
c
	ic(1) = nint(xc(1))
	ic(2) = nint(xc(2))
	ic(3) = nint(xc(3))
	l = 0
	do k=ic(3)-NW/2,ic(3)+NW/2
	  call xysetpl(tno,1,k)
	  do j=ic(2)-NW/2,ic(2)+NW/2
	    call xyread(tno,j,data)
	    do i=ic(1)-NW/2,ic(1)+NW/2
	      l = l + 1
	      x = i - ic(1)
	      y = j - ic(2)
	      z = k - ic(3)
	      coeff( 1,l) = 1
	      coeff( 2,l) = x
	      coeff( 3,l) = y
	      coeff( 4,l) = z
	      coeff( 5,l) = x*x
	      coeff( 6,l) = y*y
	      coeff( 7,l) = z*z
	      coeff( 8,l) = x*y
	      coeff( 9,l) = x*z
	      coeff(10,l) = y*z
	      fxyz(l) = data(i)
	    enddo
	  enddo
	enddo
c
	call llsqu(fxyz,coeff,NCOEFF,NW*NW*NW,c,ifail,rtemp,itemp)
	if(ifail.ne.0)then
	  call bug('w','Least squares solution failed')
	  xc(1) = ic(1)
	  xc(2) = ic(2)
	  xc(3) = ic(3)
	  f = fxyz((NW*NW*NW)/2+1)
	else
	  xc(1) = -c(2)
	  xc(2) = -c(3)
	  xc(3) = -c(4)
	  b(1,1) = c(5)
	  b(2,2) = c(6)
	  b(3,3) = c(7)
	  b(1,2) = c(8)
	  b(2,1) = b(1,2)
	  b(1,3) = c(9)
	  b(3,1) = b(1,3)
	  b(2,3) = c(10)
	  b(3,2) = b(2,3)
	  call sgefa(b,3,3,itemp,ifail)
	  if(ifail.ne.0)call bug('f','Matrix inversion failed')
	  call sgesl(b,3,3,itemp,xc,1)
	  tx = max(abs(xc(1)),abs(xc(2)),abs(xc(3)))
	  if(tx.gt.1)then
	    xc(1) = xc(1)/tx
	    xc(2) = xc(2)/tx
	    xc(3) = xc(3)/tx
	  endif
	  if(abs(xc(1)).gt.1.or.abs(xc(2)).gt.1.or.abs(xc(3)).gt.1)then
	    xc(1) = ic(1)
	    xc(2) = ic(2)
	    xc(3) = ic(3)
	    f = fxyz((NW*NW*NW)/2+1)
	  else

	    f = c(1) + c(2)*xc(1) + c(3)*xc(2) + c(4)*xc(3) +
     *	      c(5)*xc(1)*xc(1) + c(6)*xc(2)*xc(2) + c(7)*xc(3)*xc(3) +
     *	      c(8)*xc(1)*xc(2) + c(9)*xc(1)*xc(3) + c(10)*xc(2)*xc(3)
	    xc(1) = xc(1) + ic(1)
	    xc(2) = xc(2) + ic(2)
	    xc(3) = xc(3) + ic(3)
	  endif
	endif
c
	end

