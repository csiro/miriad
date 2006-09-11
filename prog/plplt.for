c************************************************************************
	program plplt
	implicit none
c
c= plplt - Plot the visibility function for a planet
c& rjs
c: uv analysis
c+
c	PLPLOT plots the visibility function of a planet at a particular
c	epoch. This models the planet as a spherical black body with
c	some brightness temperature.
c@ source
c	This gives the name of a planet. No default.
c@ epoch
c	This gives the time of the observation, in Miriad's standard
c	date format. No default.
c@ device
c	PGPLOT plotting device. No default.
c@ freq
c	Observing frequency, in GHz. The default is 22 GHz.
c@ pltb
c	The brightness temperature of the planet, in Kelvins.
c	The default depends on the planet and the observing frequency.
c@ uvrange
c	The range of baseline lengths to plot over. The baseline
c	length is given in metres. The default is 0 to 3000 metres.
c	
c--
c  History
c    rjs  15jun99 Original version.
c    rjs  04feb01 General tidy up to bring it up to standard.
c    rjs  12feb01 More minor tidying.
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	integer NPLOT
	parameter(NPLOT=500)
	real bmaj,bmin,bpa,freq,ymin,ymax,xlo,xhi,ylo,yhi,uvrange(2)
	real a,b,x(NPLOT),y(NPLOT),pltb
	character device*64,afreq*16,line*80
	double precision dist,sub(3),epoch
	integer ip,i,length
c
c  Externals.
c
        integer binsrcha,pgbeg,len1
	double precision deltime
	real j1xbyx,pltbs
	character itoaf*8
c
	integer NSOLAR
        parameter(NSOLAR=8)
	integer np(NSOLAR)
        character solar(NSOLAR)*8
	character source*32
	integer nout
        data solar/'jupiter ','mars    ','mercury ',
     *  'neptune ','pluto   ','saturn  ','uranus  ','venus   '/
	data np   / 5,	       4,         1,
     *	 8,	    9,	       6,	  7,	     2/
c
c  Look for the source name in the list of solar system objects.
c
	call output('PlPlot: version 1.0 15-Jun-99')
	call keyini
	call keymatch('source',NSOLAR,solar,1,source,nout)
	if(nout.eq.0)call bug('f','A planet must be given')
	call keya('device',device,' ')
	if(device.eq.' ')call bug('f','A plotting device must be given')
	call keyr('freq',freq,22.0)
	if(freq.le.0)call bug('f','Invalid frequency')
	call keyr('uvrange',uvrange(1),0.)
	call keyr('uvrange',uvrange(2),3000.)
	if(uvrange(1).ge.uvrange(2).or.uvrange(1).lt.0)
     *				call bug('f','Invalid uvrange')
	call keyt('epoch',epoch,'atime',0.d0)
	if(epoch.le.0)call bug('f','An epoch must be given')
	call keyr('pltb',pltb,0.)
	if(pltb.lt.0)call bug('f','Invalid value for pltb')
	call keyfin
c
c  Get the planet parameters.
c
	ip = binsrcha(source,solar,NSOLAR)
	ip = np(ip)
	epoch = epoch + deltime(epoch,'tdb')/86400.0d0
	call plpar(epoch,ip,sub,dist,bmaj,bmin,bpa)
	bmaj = sqrt(bmaj*bmin)
c
c  Get the value for the planet brightness temperature, if needed.

	if(pltb.le.0)then
	  pltb = pltbs(ip,freq)
	  call output(
     *	  'Using a planet brightness temperature of '//
     *	   itoaf(nint(pltb)))
	endif
c
c  Compute the flux as a function of length.
c
	ymin = 0
	ymax = 0
	do i=1,NPLOT
	  x(i) = (i-1)*(uvrange(2)-uvrange(1))/real(NPLOT-1)+uvrange(1)
          b = PI * bmaj*x(i)*freq*1e9/CMKS
          a = PI * pltb * (KMKS*1e18)/(CMKS*CMKS*1e-26) * bmaj*bmaj
	  y(i) = a*freq*freq*j1xbyx(b)
	  ymin = min(ymin,y(i))
	  ymax = max(ymax,y(i))
	enddo
c
	if(pgbeg(0,device,1,1).ne.1)then
	  call pgldev
	  call bug('f','Error opeing graphics device')
	endif
	call pgscf(2)
	call pgrnge(uvrange(1),uvrange(2),xlo,xhi)
	call pgrnge(ymin,ymax,ylo,yhi)
	call pgpage
	call pgvstd
	call pgswin(xlo,xhi,ylo,yhi)
	call pgbox('BCNST',0.,0,'BCNST',0.,0)
	call pgline(NPLOT,x,y)
	line = 'Visibility function for '//source
	length = len1(line)
	line(length+1:length+4) = ' at '
	length = length + 4
	write(afreq,'(f8.3)')freq
	i = 1
	dowhile(afreq(i:i).eq.' ')
	  i = i + 1
	enddo
	line(length+1:) = afreq(i:)
	length = len1(line)
	line(length+1:) = ' GHz'
	call pglab('Baseline Length (metres)','Flux Density (Jy)',line)
	call pgend
c
	end
