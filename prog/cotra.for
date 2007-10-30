      PROGRAM cotra
c-----------------------------------------------------------------------
c History:
c     30-may-91   Quick and dirty				     PJT
c     20-jun-91   Correction due to dsfetr/dsfetra doc error         MJS
c     24-sep-93   Implemented two-step using intermediate EQ         PJT
c     23-jul-97   General tidy up.				     RJS
c     30-jul-97   More precision for RA and add "epoch" keyword.     RJS
c-----------------------------------------------------------------------
c= cotra - coordinate transformations
c& pjt
c: utility
c+
c	COTRA is a MIRIAD task to transform between astronomical coordinate 
c	systems.  The coordinate systems must be one of:
c	equatorial, galactic, ecliptic, super-galactic
c@radec
c	Input RA/DEC or longitude/latitude. RA is given in hours
c	(or hh:mm:ss), whereas all the others are given in degrees
c	(or dd:mm:ss). There is no default.
c@type
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
c----------------------------------------------------------------------
	INCLUDE 'mirconst.h'
	CHARACTER  VERSION*(*)
	PARAMETER (VERSION='Version 1.0 30-Jul-97')
c
	double precision lon,lat,blon,blat,dra,ddec,epoch
	character line*64
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
	call keyt('epoch',epoch,'atime',epo2jul(1950.0,'B'))
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
	END
