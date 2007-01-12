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
c    rjs  08jan07 Eliminate spurious message. Tidy up to use tin routines.
c
c $Id: obspar.for,v 1.7 2007/01/12 15:25:42 rsault Exp $
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
c  Return the specified telescope parameter obtained from
c  observatories.dat.
c
c  Input:
c    observ	Name of the observatory. 
c                 
c    object     Name of the parameter, refer to observatories.dat for a
c               list of recognized names.
c
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
	parameter(ALTAZ=0.d0, EQUATOR=1.d0, XYEW=3.d0, NASMYTH=4.d0)
c
        double precision value
        character input*24,param*24,observ*24,cvalue*24
        character obsfile*80
        integer sgn,deg,mins
        real    sec
c
c  Externals.
c
	double precision obsdms
	character stcat*80
	integer tinNext
c
	logical first
	save first
	data first/.true./
c
	if(.not.first)return
	first = .false.
	nparms = 0
c
c  Locate and open observatories.dat.
c  Use $MIRCAT/observatories.dat if neded.
c
        call getenv('MIROBS',obsfile)
        if (obsfile.eq.' ') then
          call getenv('MIRCAT',obsfile)
          obsfile = stcat(obsfile,'/observatories.dat')
        end if 
c
c  Open and read observatories.dat.
c
        call tinOpen(obsfile,'n')
        dowhile(tinNext().gt.0)
	  call tinGeta(observ,' ')
	  call tinGeta(param,' ')
	  input = stcat(observ,'/'//param)
	  if(param.eq.'mount')then
	    call tinGeta(cvalue,' ')
	    call ucase(cvalue)
            if (cvalue.eq.'ALTAZ') then
              call obsad(input,ALTAZ)
            else if (cvalue.eq.'EQUATOR') then
              call obsad(input,EQUATOR)
            else if (cvalue.eq.'XYEW')then
              call obsad(input,XYEW)
            else if (cvalue.eq.'NASMYTH') then
              call obsad(input,NASMYTH)
            end if
c
c  Latitude and longitude.
c
          else if (param.eq.'latitude'.or.param.eq.'longitude') then
	    call tinGeti(sgn,0)
	    call tinGeti(deg,0)
	    call tinGeti(mins,0)
	    call tinGetr(sec,0.)
            call obsad(input,obsdms(sgn,deg,mins,sec))
c
c  For ellimit & evector convert
c
          else if (param.eq.'ellimit'.or.param.eq.'evector') then
            call tinGetd(value,0.d0)
            call obsad(input,value*dpi/180.0) 
c
c  General parameter.
c
          else
	    call tinGetd(value,0.d0)
            call obsad(input,value)
          endif
        enddo
c
	call tinClose()
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
