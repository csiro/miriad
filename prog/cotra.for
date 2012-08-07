      PROGRAM cotra
c-----------------------------------------------------------------------
c History:
c     30-may-91   Quick and dirty				     PJT
c     20-jun-91   Correction due to dsfetr/dsfetra doc error         MJS
c     24-sep-93   Implemented two-step using intermediate EQ         PJT
c     23-jul-97   General tidy up.				     RJS
c     30-jul-97   More precision for RA and add "epoch" keyword.     RJS
c     07-aug-97   Correct use of epo2jul routine.		     RJS
c      4-apr-08   Add some velocity reference frames                 PJT
c     10-aug-09   Add some doppler calculations                      PJT
c     25-jan-11   Add more doppler options, more obvious output      PJT
c-----------------------------------------------------------------------
c= cotra - coordinate transformations
c& pjt
c: utility
c+
c	COTRA is a MIRIAD task to transform between astronomical coordinate 
c	systems.  The coordinate systems must be one of:
c	equatorial, galactic, ecliptic, super-galactic
c
c       For given galactic longitude,latitude, it also shows three conversions
c       for common velocity frames of reference:
c       dVlsr  = Vlsr  - Vbsr     (BSR = Heliocentric)
c       dVgsr  = Vgsr  - Vlsr
c       dVlgsr = Vlgsr - Vgsr
c
c       In addition, several conversions between doppler velocities
c       can be calculated (optical, radio, relativistic)
c@ radec
c	Input RA/DEC or longitude/latitude. RA is given in hours
c	(or hh:mm:ss), whereas all the others are given in degrees
c	(or dd:mm:ss). There is no default.
c@ type
c	Input coordinate system. Possible values are
c	"b1950" (the default), "j2000", "galactic", "ecliptic"
c	and "super-galactic". b1950 and j2000 are equatorial coordinates
c	in the B1950 and J2000 frames. All other coordinates are in the
c	B1950 frames.
c@ epoch
c	Epoch (in standard Miriad time format) of the coordinate. Note
c	that this is distinct from the equinox of the coordinate as given
c	above. Varying epoch changes the coordinate values by less than an
c	arcsecond. The default is b1950.
c@ uvw
c       Standard Solar motion in right handed UVW coordinate system.
c       The classic value of 20 km/s toward RA,DEC=18h,30d corresponds to
c       uvw=10.3,15.3,7.7 but is using too high a value
c       for V. A bettter, Hipparchos based, value would be
c       uvw=10.0,5.2,7.2
c       Default:   9,12,7
c@ theta0
c       LSR speed in km/s. Default: 220
c       The GSR motion (-62,40,-35) is currently hardcoded.
c@ z
c       Redshift (unitless) or velocity (km/s).
c       If velocity is given,  the optical or radio definition
c       in a 2nd argument needs to be specified. Valid are
c       'o' (optical), 'r' (radio) and 'x' (relativistic).
c       Default : 0
c@ restfreq
c       Rest frequency of a spectral line in GHz. 
c       Default: 115.271204 (the CO 0-1 transition)
c       
c----------------------------------------------------------------------
c todo:
c vLSR = vBSR + 9 cos(l) cos(b) + 12 sin(l) cos(b) + 7 sin(b) 
c vGSR = vLSR + 220 sin(l) cos(b) 
c vLGSR = vGSR − 62 cos(l) cos(b) + 40 sin(l) cos(b) − 35 sin(b) 
c
	INCLUDE 'mirconst.h'
	CHARACTER  VERSION*(*)
	PARAMETER (VERSION='Version 25-jan-2011')
c
	double precision lon,lat,blon,blat,dra,ddec,epoch
        double precision dvlsr, dvgsr, dvlgsr,uvw(3),theta0
        double precision restfreq,z,vopt,vrad,vrel,freq,c,beta
	character line*200, vtype*10
c
	integer NTYPES
	parameter(NTYPES=5)
	character type*16,types(NTYPES)*16
	integer ntype
c
c  Externals.
c
	double precision epo2jul
	character rangle*13,hangleh*15
c
	data types/'b1950           ','j2000           ',
     *		   'galactic        ',
     *		   'ecliptic        ','super-galactic  '/
c      
	CALL output('COTRA: '//VERSION)
	CALL keyini
	call keymatch('type',NTYPES,types,1,type,ntype)
	if(ntype.eq.0)type = types(1)
	if(type.eq.'b1950'.or.type.eq.'j2000')then
	  CALL keyt('radec',blon,'hms',0.0d0)
	else
	  CALL keyt('radec',blon,'dms',0.0d0)
	endif
	call keyt('radec',blat,'dms',0.d0)
	call keyt('epoch',epoch,'atime',epo2jul(1950.0d0,'B'))
        call keyd('uvw',uvw(1),9d0)
        call keyd('uvw',uvw(2),12d0)
        call keyd('uvw',uvw(3),7d0)
        call keyd('theta0',theta0,220d0)
        call keyd('restfreq',restfreq,115.271204d0)
        call keyd('z',z,0.0d0)
        call keya('z',vtype,'z')
	CALL keyfin
c
c  Convert to b1950 coordinates.
c
	if(type.eq.'super-galactic')then
	  call dsfetra(blon,blat,.true.,3)
	else if(type.eq.'ecliptic')then
	  call dsfetra(blon,blat,.true.,2)
	else if(type.eq.'galactic')then
	  call dsfetra(blon,blat,.true.,1)
	else if(type.eq.'j2000')then
	  lon = blon
	  lat = blat
	  call fk54z(lon,lat,epoch,blon,blat,dra,ddec)
	endif
c
c  Convert from B1950 to all the other coordinate types, and write them out.
c
	call fk45z(blon,blat,epoch,lon,lat)
	line = 'J2000:            '//hangleh(lon)//rangle(lat)
	call output(line)
c
	line = 'B1950:            '//hangleh(blon)//rangle(blat)
	call output(line)
c
	lon = blon
	lat = blat
	call dsfetra(lon,lat,.false.,1)
        dvlsr = (uvw(1)*cos(lon) + uvw(2)*sin(lon))*cos(lat) 
     *                                     + uvw(3)*sin(lat)

        dvgsr = theta0*sin(lon)*cos(lat) 
        dvlgsr= -62*cos(lon)*cos(lat)+40*sin(lon)*cos(lat)-35*sin(lat)
	write(line,'(a,2f14.6)')'Galactic:      ',
     *					180/DPI*lon,180/DPI*lat
	call output(line)
c
	lon = blon
	lat = blat
	call dsfetra(lon,lat,.false.,2)
	write(line,'(a,2f14.6)')'Ecliptic:      ',
     *					180/DPI*lon,180/DPI*lat
	call output(line)
c
	lon = blon
	lat = blat
	call dsfetra(lon,lat,.false.,1)
	write(line,'(a,2f14.6)')'Super-Galactic:',
     *					180/DPI*lon,180/DPI*lat
	call output(line)

        write(line,'(a,f14.6)')'dVlsr:         ', dvlsr
	call output(line)
        write(line,'(a,f14.6)')'dVgsr:         ', dvgsr
	call output(line) 
        write(line,'(a,f14.6)')'dVlgsr:        ', dvlgsr
	call output(line)

c       speed of light in km/s
        c = DCMKS/1000.0
        IF (z.NE.0.0d0) THEN
           if (vtype(1:1).eq.'z') then
              freq = restfreq/(1.0d0+z)
              vrel = ((z+1)*(z+1)-1)/((z+1)*(z+1)+1)*c
              vopt = z*c
              vrad = vopt/(1+z)
              write(line,'(a,2f11.6,a,f8.6,a,f12.4,a,f12.4,a,f12.4)')
     *              'Doppler:          ', restfreq,freq,
     *              ' z=',z, ' v=',vrel,
     *              ' vopt=',vopt,' vrad=',vrad
           else if (vtype(1:1).eq.'r') then
              vrad = z
              vopt = vrad/(1-vrad/c)
              z = vopt/c
              freq = restfreq*(1.0d0-vrad/c)
              vrel = ((z+1)*(z+1)-1)/((z+1)*(z+1)+1)*c
              write(line,'(a,2f11.6,a,f8.6,a,f12.4,a,f12.4)') 
     *              'Doppler:          ',
     *              restfreq,freq,
     *              ' z=',z, ' v=',vrel,' vopt=',vopt
           else if (vtype(1:1).eq.'o') then
              vopt = z
              z = vopt/c
              freq = restfreq/(1.0d0+z)
              vrad = vopt/(1+vopt/c)
              vrel = ((z+1)*(z+1)-1)/((z+1)*(z+1)+1)*c
              write(line,'(a,2f11.6,a,f8.6,a,f12.4,a,f12.4))')
     *              'Doppler:          ',
     *              restfreq,freq,
     *              ' z=',z,' v=',vrel,' vrad=',vrad
           else if (vtype(1:1).eq.'x') then
              vrel = z
              beta = vrel/c
              z = sqrt((1+beta)/(1-beta)) - 1
              vopt = c*z
              vrad = vopt/(1+z)
              freq = restfreq/(1.0d0+z)
              write(line,'(a,2f11.6,a,f8.6,a,f12.4,a,f12.4))')
     *              'Doppler:          ',
     *              restfreq,freq,
     *              ' z=',z,' vopt=',vopt,' vrad=',vrad
           else
              write(line,'(a,a)') 'vel type unknown [z,r,o,x]:',vtype
              call bug('f',line)
           endif
           call output(line)
        ENDIF
	END
