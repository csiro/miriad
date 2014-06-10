c************************************************************************
	program arrayplt
	implicit none
c
c= Arrayplt -- Plot antenna layout of the array.
c& rjs
c: plotting
c+
c	Arrayplt is a MIRIAD task which plots the array layout of an
c	interferometer array. Only the first antenna table in the
c	file is plotted.
c@ vis
c	The name of the input data-set. No default.
c@ select
c	Normal visibility selection, although only the antenna
c	selection is honoured (no other selection is honoured!).
c@ options
c	Extra processing options. Several can be given, abbreviated to
c	uniqueness. Possible options are:
c	  name   Label each antenna with its name rather than its number.
c	
c@ device
c	The PGPLOT plotting device to use. The default is no plot.
c--
c  History:
c    rjs  09aug10 Original version.
c    rjs  06mar14 Added selection.
c    rjs  07may14 Name antennas.
c    rjs  27may14 Option of array names or numbers.
c
c  Bugs:
c    ?? Perfect?
c------------------------------------------------------------------------
	integer MAXSELS
	parameter(MAXSELS=1024)
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	parameter(version='Arrayplt: version 1.0 09-Aug-10')
c
	double precision antpos(3*MAXANT)
	double precision lat,cosl,sinl,fac,antno
	real x(MAXANT),y(MAXANT),z(MAXANT)
	real xmin,xmax,ymin,ymax,delta,xch,ych
	real sels(MAXSELS)
	logical updated,skip,doname
	character vis*64,device*64,type*1,c*16,hard*6,telescop*16
	character names(MAXANT)*16
	integer tIn,iostat,nant,x0,y0,z0,i,n,k,lc,ilen
c
c  Externals.
c
	integer pgbeg,uvscan,len1
	character itoaf*4
	logical selProbe
	double precision antbas
c
c  Get the user parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input data-set must be given')
	call keya('device',device,' ')
	if(device.eq.' ')call bug('f','A PGPLOT device must be given')
	call selInput('select',sels,MAXSELS)
	call getopt(doname)
	call keyfin
c
c  Open up all the inputs.
c
	call uvopen(tIn,vis,'old')
c
c  Find and check the correctness of the antenna table. Then get it.
c
	iostat = uvscan(tIn,'antpos')
	if(iostat.ne.0)call bug('f','Failed to find antenna table')
	call uvprobvr(tIn,'antpos',type,nant,updated)
	if(type.ne.'d'.or..not.updated.or.nant.le.0.or.mod(nant,3).ne.0)
     *	  call bug('f','Invalid antenna table')
	nant = nant / 3
	if(nant.gt.MAXANT)call bug('f','Too many antennas for me')
	call uvgetvrd(tIn,'antpos',antpos,3*nant)
	if(doname)then
	  call uvrdvra(tIn,'telescop',telescop,' ')
	  call getnames(tIn,names,nant,telescop)
	else
	  do i=1,nant
	    names(i) = itoaf(i)
	  enddo
	endif
c
c  Get the latitude of the array.
c
	call uvprobvr(tIn,'latitud',type,n,updated)
	if(type.ne.'d'.or..not.updated.or.n.ne.1)
     *	 call bug('f','Problem finding the latitude of the observatory')
	call uvgetvrd(tIn,'latitud',lat,1)
	call uvclose(tIn)
c
c  Convert the antenna table to local north, east and height coordinates.
c
	cosl = cos(lat)
	sinl = sin(lat)
	x0 = 0
	y0 = nant
	z0 = 2*nant
	fac = DCMKS * 1d-9
c
c  Cmpute local east, north and height (x,y,z respectively).
c
	k = 0
	do i=1,nant
	  antno = antbas(i,i)
	  skip = .not.SelProbe(sels,'antennae',antno)
	  if(skip)then
	    continue
	  else if(antpos(i+x0).eq.999999.and.
     *	          antpos(i+y0).eq.999999.and.
     *	          antpos(i+z0).eq.999999)then
	    continue
	  else
	    k = k + 1
	    names(k) = names(i)
	    x(k) = fac * antpos(i+y0)
	    y(k) = fac * (-sinl*antpos(i+x0) + cosl*antpos(i+z0) )
	    z(k) = fac * ( cosl*antpos(i+x0) + sinl*antpos(i+z0) )
	  endif
	enddo
	nant = k
c
c  Open the PGPLOT device.
c
	if(pgbeg(0,device,1,1).ne.1)then
	  call pgldev
	  call bug('f','Error opening graphics device')
	endif
	call pgqinf ('hardcopy', hard, ilen)
	if (hard.eq.'YES')call pgscf(2)
c
	xmin = x(1)
	xmax = xmin
	ymin = y(1)
	ymax = ymin
	do i=2,nant
	  xmin = min(xmin,x(i))
	  xmax = max(xmax,x(i))
	  ymin = min(ymin,y(i))
	  ymax = max(ymax,y(i))
	enddo
c
	delta = 0.1*(xmax - xmin)
	xmin = xmin - delta
	xmax = xmax + delta
c
	delta = 0.1*(ymax - ymin)
	ymin = ymin - delta
	ymax = ymax + delta
c
	call pgpage
	call pgvstd
	call pgwnad(xmin,xmax,ymin,ymax)
	call pgbox('BCNST',0,0.,'BCNST',0,0.)
	call pglab('Easting (metres)','Northing (metres)',
     *						'Array layout')
	call pgpt(nant,x,y,17)
c
	call pgqcs(4,xch,ych)
	call pgsci(2)
	do i=1,nant
	  c = names(i)
	  lc = len1(c)
	  call pgptxt(x(i),y(i)+0.25*ych,0.0,0.5,c(1:lc))
	enddo
c
	call pgend
c
c  Bye bye.
c
	end
c************************************************************************
	subroutine getnames(lIn,names,nants,telescop)
c
	implicit none
	integer lIn,nants
	character names(nants)*(*),telescop*(*)
c------------------------------------------------------------------------
	integer i,ant,l1,l2,length
	logical updated
	character buf*1024,dtype*1
c
	call uvprobvr(lIn,'antname',dtype,length,updated)
	if(dtype.ne.'a'.or.length.ge.len(buf))length = 0
	if(length.gt.0)call uvgetvra(lIn,'antname',buf(1:length+1))
	ant = 0
	l1 = 1
	do l2=1,length
	  if(buf(l2:l2).eq.';')then
	    ant = ant + 1
	    if(l2.gt.l1)then
	      names(ant) = buf(l1:l2-1)
	    else
	      call defant(names(ant),ant,telescop)
	    endif
	    l1 = l2 + 1
	  endif
	enddo
	if(l1.le.length)then
	  ant = ant + 1
	  names(ant) = buf(l1:length)
	endif
c
	do i=ant+1,nants
	  call defant(names(i),i,telescop)
	enddo
c
	end
c************************************************************************
	subroutine defant(name,ant,telescop)
c
	implicit none
	character name*(*),telescop*(*)
	integer ant
c
c  Externals.
c
	character itoaf*8
c
	if(telescop.eq.'atca')then
	  name = 'ca0'//itoaf(ant)
	else
	  name = itoaf(ant)
	endif
c
	end
c************************************************************************
	subroutine getopt(doname)
c
	implicit none
	logical doname
c
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	character opts(NOPTS)*8
	logical present(NOPTS)
c
	data opts/'name    '/
c
	call options('options',opts,present,NOPTS)
	doname = present(1)
	end
