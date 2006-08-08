c************************************************************************
	program telepar
	implicit none
c
c= telepar -- Tell about telescope characteristics.
c& rjs
c: utility
c+
c	TELEPAR gives the characteristics of various observatories.
c	Its main use is to check that the characteristics are correct.
c@ telescop
c	Name of the observatory. Several can be given. If none are
c	given, TELEPAR simply lists the known observatories.
c--
c  History:
c    rjs  20jun91 Original version.
c    rjs   2jun93 Better formating.
c    rjs  15dec95 List observatories.
c    rjs  06dec96 Print altitude.
c    rjs  09jun97 Standardize keyword.
c    dpr  22may01 Add XY-EW
c    mchw 26aug03 Add Nasmyth
c    sdw  8jul06  Doesnt print all characteristics !
c                 added subdiam,ew,nants,ellimt
c                 also tried to improve formatting.
c------------------------------------------------------------------------
	character version*(*)
	integer MAXOBS
	parameter(version='Telepar: version 3.1 8-JUL-06')
	parameter(MAXOBS=16)
	include 'mirconst.h'
	character string*13,line*64,observs(MAXOBS)*12,observ*12
	logical ok
	integer nobs,i,n
	double precision value
c
	character rangle*20
c
	call output(version)
	call keyini
	call mkeya('telescop',observs,MAXOBS,nobs)
	call keyfin
c
	if(nobs.eq.0)then
	  call obsPrint
	endif
c
	do i=1,nobs
	  call output('********************************')
	  observ = observs(i)
	  call ucase(observ)
c
	  n = 0
c
	  call obspar(observ,'latitude',value,ok)
c
c         Put output here to get better output order
	  call output('Observatory:           '//observ)
	  if(ok)then
	    n = n + 1
	    string = rangle(value)
	    call output('Latitude:              '//string
     *                   //' deg:mm:ss.ss')
	  endif
c
	  call obspar(observ,'longitude',value,ok)
	  if(ok)then
	    n = n + 1
	    string = rangle(value)
	    call output('Longitude:             '//string
     *                  //' deg:mm:ss.ss')
	  endif
c
	  call obspar(observ,'height',value,ok)
	  if(ok)then
	    n = n + 1
            write(string,*) value
	    call output('Height:              '//string
     *                  //'   meters')	
	  endif
c
	  call obspar(observ,'evector',value,ok)
	  if(ok)then
	    n = n + 1
            value=180/pi*value
            write(string,*) value
	    call output('Feed Offset angle:   '//string
     *                  //'   degrees')
	  endif
c
	  call obspar(observ,'mount',value,ok)
	  if(ok)then
	    n = n + 1
	    if(value.eq.0)string = 'Alt-az'
	    if(value.eq.1)string = 'Equatorial'
	    if(value.eq.3)string = 'XY-EW'
	    if(value.eq.4)string = 'Nasmyth'
	    call output('Mount:                 '//string)	
	  endif
c
	  call obspar(observ,'antdiam',value,ok)
	  if(ok)then
	    n = n + 1
            write(string,*) value
	    call output('Antenna diameter:    '//string
     *                  //'   meters')
	  endif
c
	  call obspar(observ,'jyperk',value,ok)
	  if(ok)then
	    n = n + 1
            write(string,*) value
	    call output('System Gain:         '//string
     *                  //'   Jy/K')
	  endif
c
	  call obspar(observ,'subdiam',value,ok)
	  if(ok)then
	    n = n + 1
            write(string,*) value
	    call output('Subreflector diam:   '//string
     *                  //'   meters')
	  endif
c
	  call obspar(observ,'systemp',value,ok)
	  if(ok)then
	    n = n + 1
            write(string,*) value
	    call output('System Temp :        '//string
     *                  //'   K')
	  endif
c
	  call obspar(observ,'ellimit',value,ok)
	  if(ok)then
	    n = n + 1
            value=180/pi*value
            write(string,*) value
	    call output('Elevation Limit:     '//string
     *                  //'   degrees')
	  endif
c
	  call obspar(observ,'nants',value,ok)
	  if(ok)then
	    n = n + 1
            write(string,*) value
	    call output('Number of Antennas:  '//string)
	  endif
c
	  call obspar(observ,'ew',value,ok)
	  if(ok)then
	    n = n + 1
            write(string,*) value
	    call output('E-W Array:           '//string)
	  endif
c
	  if(n.eq.0)call bug('w','No information was found')
	end do

        end
