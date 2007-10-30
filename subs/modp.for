c************************************************************************
	subroutine modpcomp(uvw,nchan,sfreq,vis)
c
	implicit none
	integer nchan
	double precision uvw(3),sfreq(nchan)
	complex vis(nchan)
c------------------------------------------------------------------------
	include 'modp.h'
	include 'mirconst.h'
c
	integer i,j
	real theta,tflux,epsi
	complex ctemp
c
c  Determine the decorrelation factor (bandwidth and time smearing).
c
	call modpDeco(uvw,sfreq,ucoeff,vcoeff,wcoeff,lmn,factor,nsrc,
     *	  inttime,wts,nwts,cosd,sind)
c
	do j=1,nchan
	  ctemp = 0
	  if(freq0.gt.0)then
	    epsi = log(real(sfreq(j)/freq0))
	  else
	    epsi = 0
	  endif
	  do i=1,nsrc
	    tflux = ( flux(1,i) + epsi*flux(2,i) ) * factor(i)
	    theta = 2*DPI*sfreq(j)*(uvw(1)* lmn(1,i) +
     *				    uvw(2)* lmn(2,i) +
     *				    uvw(3)*(lmn(3,i) - 1))/sfreq(1)
	    ctemp = ctemp + tflux*cmplx(cos(theta),sin(theta))
	  enddo
	  vis(j) = vis(j) + ctemp
	enddo
c
	end
c************************************************************************
	subroutine modpDeco(uvw,freq0,ucoeff,vcoeff,wcoeff,lmn,
     *	  factor,nsrc,inttime,wts,nwts,cosd,sind)
c
	implicit none
	integer nsrc,nwts
	real inttime,factor(nsrc),wts(nwts)
	double precision uvw(3),lmn(3,nsrc),freq0
	double precision ucoeff(3),vcoeff(3),wcoeff(3),cosd,sind
c
c------------------------------------------------------------------------
c
c  The rotation rate of the Earth, in radians/sec.
c
	include 'mirconst.h'
	real omegae
	parameter(omegae=2*PI/86400*366.25/365.25)
c
	double precision ll,mm,nn,uu,vv,ww
	real theta,bsmear,tsmear
	integer i,j
c
c  Externals.
c
	real sinc
c
c  Convert to coordinates at the delay centre.
c
	uu = ucoeff(1)*uvw(1) + ucoeff(2)*uvw(2) + ucoeff(3)*uvw(3)
	vv = vcoeff(1)*uvw(1) + vcoeff(2)*uvw(2) + vcoeff(3)*uvw(3)
	ww = wcoeff(1)*uvw(1) + wcoeff(2)*uvw(2) + wcoeff(3)*uvw(3)
c
	do j=1,nsrc
	  ll = ucoeff(1)*lmn(1,j)+ucoeff(2)*lmn(2,j)+ucoeff(3)*lmn(3,j)
	  mm = vcoeff(1)*lmn(1,j)+vcoeff(2)*lmn(2,j)+vcoeff(3)*lmn(3,j)
	  nn = wcoeff(1)*lmn(1,j)+wcoeff(2)*lmn(2,j)+wcoeff(3)*lmn(3,j)
c
c  Bandwidth smearing factor.
c
	  theta = 0.256/freq0*(ll*uu+mm*vv+(nn-1)*ww)
	  bsmear = 0
	  do i=1,nwts
	    bsmear = bsmear + wts(i)*( sinc(0.5*(theta+i-1)) +
     *				       sinc(0.5*(theta-i+1)) )
	  enddo
c
c  Time smearing factor. This assumes an east-west array.
c
	  theta = (-ll*vv/sind+mm*uu*sind-(nn-1)*uu*cosd)*omegae*inttime
	  tsmear = sinc(theta)
c
c  Overall factor.
c
	  factor(j) = bsmear*tsmear
	enddo
c
	end
c************************************************************************
	subroutine modpini(tvis,tmod)
c
	implicit none
	integer tmod,tvis
c
c  Load the point source file form a image dataset.
c------------------------------------------------------------------------
	include 'modp.h'
	integer tcmp,iostat,k1,k2,i,length,iax,coObj
	double precision dtemp,radel,decdel,x1(2),radec(2)
	logical ok
	character fluxst*32,line*132
c
c  Externals.
c
	integer len1
	real sinc
c
c  Determine the reference frequency.
c
	call coInit(tmod)
	call coFindAx(tmod,'frequency',iax)
	if(iax.gt.0)then
	  call coVelSet(tmod,'frequency')
	  call coCvt1(tmod,iax,'op',0.d0,'aw',freq0)
	else
	  freq0 = 0
	endif
	call coFin(tmod)
c
c  Get a handle coordinate handle for the visibility dataset.
c
	call coInit(tvis)
	x1(1) = 0
	x1(2) = 0
	call coCvt(tvis,'op/op',x1,'aw/aw',radec)
c
c  Determine characteristics about the delay tracking centre.
c
	call uvrdvrd(tvis,'radel',radel,radec(1))
	call uvrdvrd(tvis,'decdel',decdel,radec(2))
	call uvrdvrr(tvis,'inttime',inttime,10.0)
c
	cosd = cos(decdel)
	sind = sin(decdel)
c
c  Compute rotation matrices to convert (u,v,w) and (l,m,n) to 
c  be relative to the delay tracking centre.
c
	call coRaDec(coObj,'NCP',radel,decdel)
	call coGeom(coObj,'aw/aw',radec,ucoeff,vcoeff,wcoeff)
	call coFin(coObj)
c
c  Get the lag weights. This assumes the 33 channel/128 MHz system,
c  with options=reweight used during ATLOD.
c
	call lagwt(wts,64,0.04)
	nwts = 32
	do i=2,32
	  wts(i) = 2*wts(i)*(1.0-real(i-1)/32.0)
	enddo
c
c  Normalise the lag weights.
c
	dtemp = 0
	do i=1,nwts
	  dtemp = dtemp + wts(i)*2*sinc(0.5*(i-1))
	enddo
	do i=1,nwts
	  wts(i) = wts(i) / dtemp
	enddo
c
c  Get a handle for the input dataset.
c
	call haccess(tmod,tcmp,'sources','read',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Loop over all the sources.
c
	nsrc = 0
	call hreada(tcmp,line,iostat)
	dowhile(iostat.eq.0)
	  k1 = 1
	  k2 = len1(line)
	  if(k2.gt.0.and.line(1:1).ne.'#')then
	    nsrc = nsrc + 1
	    call modprd(tvis,line,k1,k2,lmn(1,nsrc))
	    do i=1,2
	      call getfield(line,k1,k2,fluxst,length)
	      if(length.gt.0)then
		call atodf(fluxst(1:length),dtemp,ok)
		if(.not.ok)call bug('f',
     *			'Error decoding number in sources table')
		flux(i,nsrc) = dtemp
	      else
		flux(i,nsrc) = 0
	      endif
	    enddo
	  endif
c
	  call hreada(tcmp,line,iostat)
	enddo
c
	if(iostat.ne.-1)then
	  call bug('w','Error while reading source table')
	  call bugno('f',iostat)
	endif
	call hdaccess(tcmp,iostat)
c
	call cofin(tvis)
	end
c************************************************************************
	subroutine modprd(tvis,line,k1,k2,lmn)
c
	implicit none
	integer k1,k2,tvis
	character line*(*)
	double precision lmn(3)
c------------------------------------------------------------------------
	character rast*32,decst*32
	integer ralen,declen
	logical ok
	double precision x(2)
c
	call getfield(line,k1,k2,rast,ralen)
	call getfield(line,k1,k2,decst,declen)
	if(ralen.le.0.or.declen.le.0)call bug('f',
     *		'Error in the format of RA/DEC in source table')
	call decangle(rast,x(1),'hms',ok)
	if(ok)call decangle(decst,x(2),'dms',ok)
	if(.not.ok)call bug('f',
     *		'Error in decoding RA/DEC in source table')
	call colmn(tvis,'aw/aw',x,lmn)
	end
