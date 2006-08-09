c************************************************************************
c
c  A set of routines giving information about observatories.
c
c  History:
c    rjs  20jun91 Original version.
c    rjs   2jul91 Corrected Westerbork parameters, as suggested by pjt.
c    rjs   2jun93 Near complete rewrite, to give it greater flexibility.
c	          Add also more parameters.
c    mchw 15jul93 Add some more parameters.
c    rjs  16sep93 Rename bsrch to binsrch.
c    rjs   7mar94 Assume WSRT evector is -90 degrees (assume fixed dipoles).
c    rjs  29jul94 Tolerate double-barrelled names (e.g. 'OVRO MMA').
c    rjs   9aug94 Add "ew" parameter for ATCA and WSRT.
c    rjs   5jul95 Added some penticon parameters.
c    rjs  27sep95 Added Parkes and nants.
c    rjs  11oct95 Added subreflector diameter for atca.
c    rjs  11mar96 Changed "nobeyama" to "nro10m", and added more info
c		  on it.
c    rjs  14mar96 Fixed bug I introduced on 11mar96.
c    rjs  11aug96 Added mopra, plus miscellaneous other parameters.
c    mchw 18sep96 Added iram15m array on PdB.
c    rjs  17dec96 More accurate positions for ATNF telescopes.
c    mchw 20may97 Added CSO and JCMT. Updated HATCREEK.
c    mchw 09jun97 Added evector to hatcreek
c    rjs  24jun97 Correct height of ATCA.
c    rjs/jm 11jul97 Correct longitude sign for IRAM15M, JCMT and CSO.
c    mchw 05aug98 Added mount and nants to ovro.
c    mchw 05feb99 Added CARMA for combined hatcreek and ovro arrays.
c    rjs  31mar00 Added SEST.
c    dpr  22may01 Added HOBART26M, CEDUNA30M, XYEW
c    mchw 24may01 Added RPA
c    mchw 03jan02 Added SZA
c    mchw 09jul02 Added ALMA
c    mchw 26aug03 Added SMA
c    gxm  27jan04 Added generic systemp for WSRT (to please wsrtfits).
c    mchw 07jul04 Added SMA10 and SZA6 cross correlations for CARMA.
c    pjt  24sep04 Final location of CARMA at Cedar Flats
c    sdw  06jul06 Change to read observatory data from a paramater file,
c                 allow greater flexability. Rather than having data hard
c                 coded into the program.
c************************************************************************
c* ObsPrint -- Print list of known observatories.
c: utility
c& rjs
c+
	subroutine obsPrint
c
	implicit none
c
c  This prints a list of the known observatories.
c--
c------------------------------------------------------------------------
	include 'obspar.h'
	character tel*32
	integer l,i
c
	call obsInit
c
	tel = ' '
	call output('Known observatories are:')
	do i=1,nparms
	  l = index(parname(i),'/')
	  if(parname(i)(1:l-1).ne.tel)then
	    tel = parname(i)(1:l-1)
	    call output('   '//tel)
	  endif
	enddo
c
	end
c************************************************************************
c* ObsPar - Get characteristics of a particular observatory.
c: utility
c& rjs
c+
	subroutine obsPar(observ,object,value,ok)
c
	implicit none
	character observ*(*),object*(*)
	double precision value
	logical ok
c
c  This returns known characteristics of various observervatories.
c  from the observatories.dat file.
c
c  Input:
c    observ	Name of the observatory. 
c                 
c    object	The parameter of the observatory of interest. Possible
c		values are:
c		 'latitude '	Observatory latitude, in radians.
c		 'longitude'	Observatory longitude, in radians.
c		 'jyperk'	Typical system gain, in Jy/K.
c		 'systemp'	Typical system temperature, in K.
c		 'evector'	Offset angle of the feed to the local
c				vertical, fraction of 360 degrees.
c		 'mount'	Telescope mount: 0 = alt-az
c						 1   equitorial
c                                                3   xy-ew
c                                                4   nasmyth
c		 'antdiam'	Antenna diameter, in meters.
c		 'subdiam'	Subreflector diameter.
c		 'height'	Height above sea level, in meters
c		 'ew'		Positive if the telescope is an E-W array.
c	         'nants'        Number of antennas normally in the array.
c		 'ellimit'	Elevation limit, in degrees.
c  Output:
c    value	The value of the parameter.
c    ok		True if the value was successfully found.
c--
c------------------------------------------------------------------------
	include 'obspar.h'
	character name*24
	integer l
c
c  Externals.
c
	integer len1,binsrcha
c
c
c  Initialise the list of known parameters, if necessary.
c
	call obsInit
c
c  Determine the name of the parameter that we want, and locate it.
c
	l = index(observ,' ') - 1
	if(l.le.0)l = len1(observ)
	name = observ(1:l)//'/'//object
	call lcase(name)
	l = binsrcha(name,parname,nparms)
	ok = l.gt.0
	if(ok) value = parvalue(l)
	end
c************************************************************************
	subroutine obsinit
c
	implicit none
c
c  Initialise the list of known parameters.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'obspar.h'
c
	double precision ALTAZ,EQUATOR,NASMYTH,XYEW
	parameter(ALTAZ=0.d0,EQUATOR=1.d0,NASMYTH=4.d0,XYEW=3.d0)
c
        double precision value
        character*24 input,param,observatory,cvalue
c  Define File handling param's
        integer iostat,lu,length,istart,ifin
        character*80 obsfile,line,mir_root
        character*80 file
c  Lat / Long variables
        integer sign,deg,min
        real    sec
c
c  Externals.
c
	double precision obsdms
c
	logical first
	save first
	data first/.true./
c
	if(.not.first)return
	first = .false.
	nparms = 0
c
c       Has a local MIRPARAM been set pointing to a different location
c       for a observatories.dat, ie a local version
c       If it isn't set then assume a 
c       default of $MIR/param/observatories.dat
        call getenv('MIRPARAM',mir_root)
        if (mir_root(1:1).eq.' ') then
           call getenv('MIR',mir_root)
c          build the filename to open
           obsfile='/param/observatories.dat'        
           file=mir_root(1:index(mir_root,' ')-1)//
     +          obsfile(1:index(obsfile,' '))
        else
c          build the filename to open
           obsfile='/observatories.dat'        
           file=mir_root(1:index(mir_root,' ')-1)//
     +          obsfile(1:index(obsfile,' '))
        end if 

        call output('Reading '//file)
c
c       Open and read the observatories.dat

        call txtopen(lu,file,'old',iostat)
        if(iostat.ne.0) then
           call bug('w','Error opening observatories.dat')
           call bugno('w',iostat)
        else
           dowhile(iostat.eq.0)
             call txtread(lu,line,length,iostat)
             if (iostat.ne.0) goto 10
             if (line(1:3).eq.'EOF') goto 10

c            Ignore comment lines in file, ie line(1:1)='#'
             if (line(1:1).ne.'#') then
               read(line,*) observatory,param
               input=observatory(1:index(observatory,' ')-1)//'/'//
     +               param(1:index(param,' ')-1)

c              Allow for param = mount, latitude or longitude

               if (param(1:5).eq.'mount') then
                 read(line,*)observatory,param,cvalue
                 if(cvalue(1:5).eq.'ALTAZ') 
     +              call obsad(input,ALTAZ)
                 if(cvalue(1:7).eq.'EQUATOR') 
     +              call obsad(input,EQUATOR)
                 if(cvalue(1:7).eq.'NASMYTH') 
     +              call obsad(input,NASMYTH)
                 if(cvalue(1:5).eq.'XYEW')
     +              call obsad(input,XYEW)
               else if (param(1:8).eq.'latitude') then
                  read(line,*)observatory,param,sign,deg,min,sec
                  call obsad(input,obsdms(sign,deg,min,sec))
               else if (param(1:9).eq.'longitude') then  
                  read(line,*)observatory,param,sign,deg,min,sec
                  call obsad(input,obsdms(sign,deg,min,sec))

c              For ellimit & evector convert

               else if (param(1:7).eq.'ellimit') then
                  read(line,*)observatory,param,value   
                  call obsad(input,value*dpi/180.d0) 
               else if (param(1:7).eq.'evector') then
                  read(line,*)observatory,param,value   
                  call obsad(input,value*dpi) 
               else
                 read(line,*)observatory,param,value   
                 call obsad(input,value)
               endif
              endif
           enddo
        endif

10      call txtclose(lu)
c
c
	end
c************************************************************************
	subroutine obsad(name,value)
c
	implicit none
	character name*(*)
	double precision value
c
c  Add a new parameter to the list of parameters.
c------------------------------------------------------------------------
	include 'obspar.h'
	character line*24
c
c  Check for sensible values.
c
	line = name
	if(nparms.eq.MAXPARMS)
     *		call bug('f','Buffer overflow in ObsAdd:'//line)
	if(nparms.gt.0)then
	  if(name.le.parname(nparms))
     *		call bug('f','ObsInit list not ordered:'//line)
	endif
	nparms = nparms + 1
	if(len(name).gt.len(parname(nparms)))
     *		call bug('f','Name too long in ObsInit list:'//line)
c
	parname(nparms) = name
	parvalue(nparms)  = value
	end
c************************************************************************
	double precision function obsdms(s,deg,m,sec)
c
	implicit none
	integer deg,m,s
	real sec
c
c  Convert degrees, minutes, seconds to radians. The angle is
c  s*(deg + m/60 + sec/3600)
c
c  Inputs:
c    s		The sign of the angle.
c    deg	Degrees -- must be positive!
c    m		Minutes.
c    sec	Seconds.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	if(min(deg,m).lt.0.or.sec.lt.0)
     *	  call bug('f','Negative value in obsdms')
	if(abs(s).ne.1)call bug('f','Bad sign in obsdms')
	obsdms = dble(deg) + m/60.d0 + sec/3600.d0
	obsdms = s*dpi/180.d0 * obsdms
	end
