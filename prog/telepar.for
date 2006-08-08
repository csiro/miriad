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
c@ observ
c	Name of the observatory. Several can be given. Possible values
c	are atca,hatcreek,vla,wsrt,nobeyama,nobeyama45,ovro,onsala,
c	quabbin.
c--
c  History:
c    rjs  20jun91 Original version.
c    rjs   2jun93 Better formating.
c------------------------------------------------------------------------
	character version*(*)
	integer MAXOBS
	parameter(version='Telepar: version 1.0 2-Jun-93')
	parameter(MAXOBS=16)
	include 'mirconst.h'
	character string*20,line*64,observs(MAXOBS)*12,observ*12
	logical ok
	integer nobs,i,n
	double precision value
c
	character rangle*20
c
	call output(version)
	call keyini
	call mkeya('observ',observs,MAXOBS,nobs)
	call keyfin
	if(nobs.eq.0)call bug('f','No observatory given')
c
	do i=1,nobs
	  call output('********************************')
	  observ = observs(i)
	  call ucase(observ)
c
	  n = 0
	  call output('Observatory:         '//observ)
c
	  call obspar(observ,'latitude',value,ok)
	  if(ok)then
	    n = n + 1
	    string = rangle(value)
	    call output('Latitude:            '//string)
	  endif
c
	  call obspar(observ,'longitude',value,ok)
	  if(ok)then
	    n = n + 1
	    string = rangle(value)
	    call output('Longitude:           '//string)	
	  endif
c
	  call obspar(observ,'evector',value,ok)
	  if(ok)then
	    n = n + 1
	    write(line,'(a,f7.1)')'Feed Offset angle:',180/pi*value
	    call output(line)
	  endif
c
	  call obspar(observ,'mount',value,ok)
	  if(ok)then
	    n = n + 1
	    if(value.eq.0)string = 'Alt-az'
	    if(value.eq.1)string = 'Equatorial'
	    call output('Mount:               '//string)	
	  endif
c
	  call obspar(observ,'antdiam',value,ok)
	  if(ok)then
	    n = n + 1
	    write(line,'(a,f8.1)')'Antenna diameter:',value
	    call output(line)
	  endif
c
	  call obspar(observ,'jyperk',value,ok)
	  if(ok)then
	    n = n + 1
	    write(line,'(a,f6.1)')'System Gain (Jy/K):',value
	    call output(line)
	  endif
c
	  call obspar(observ,'systemp',value,ok)
	  if(ok)then
	    n = n + 1
	    write(line,'(a,f9.1)')'System Temp (K):',value
	    call output(line)
	  endif
	  if(n.eq.0)call bug('w','No information was found')
	enddo
	end
