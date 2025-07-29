c***********************************************************************
      program telepar
c
c= telepar -- Report telescope characteristics.
c& rjs
c: utility
c+
c       TELEPAR reports the characteristics of various observatories.
c       Its main use is to check that the characteristics are correct.
c
c@ telescop
c       Name of the observatory.  Several can be given.  If none are
c       given, TELEPAR simply lists the known observatories.
c
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
c    pjt   1aug08 keyword reminder
c    rjs  23apr10 Reworked formatting was failing on some machines.
c                 Rework the formatting again.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer MAXOBS
      parameter(MAXOBS=16)

      logical ok
      integer i, n, nobs
      double precision value
      character observ*12, observs(MAXOBS)*12, string*20, version*72

c     Externals.
      character rangle*20, versan*72
c-----------------------------------------------------------------------
      version = versan('telepar')

      call keyini
      call mkeya('telescop',observs,MAXOBS,nobs)
      call keyfin

      if(nobs.eq.0)then
        call obsPrint
        call output('Use telescop= to select observatories')
      endif

      do i=1,nobs
        call output('********************************')
        observ = observs(i)
        call ucase(observ)

        n = 0

        call obspar(observ,'latitude',value,ok)

c       Put output here to get better output order
        call output('Observatory:           '//observ)
        if(ok)then
          n = n + 1
          string = rangle(value)
          call output('Latitude:              '//string
     *                //' deg:mm:ss.ss')
        endif

        call obspar(observ,'longitude',value,ok)
        if(ok)then
          n = n + 1
          string = rangle(value)
          call output('Longitude:             '//string
     *                //' deg:mm:ss.ss')
        endif

        call obspar(observ,'height',value,ok)
        if(ok)then
          n = n + 1
          write(string,'(f8.2)') value
          call output('Height:              '//string
     *                //'   metres')
        endif

        call obspar(observ,'evector',value,ok)
        if(ok)then
          n = n + 1
          value=180/pi*value
          write(string,'(f8.2)') value
          call output('Feed Offset angle:   '//string//'   degrees')
        endif

        call obspar(observ,'mount',value,ok)
        if(ok)then
          n = n + 1
          if(value.eq.0)string = 'Alt-az'
          if(value.eq.1)string = 'Equatorial'
          if(value.eq.3)string = 'XY-EW'
          if(value.eq.4)string = 'Nasmyth'
          if(value.eq.6)string = 'Fixed'
          call output('Mount:                 '//string)
        endif

        call obspar(observ,'antdiam',value,ok)
        if(ok)then
          n = n + 1
          write(string,'(f8.2)') value
          call output('Antenna diameter:    '//string
     *                //'   metres')
        endif

        call obspar(observ,'jyperk',value,ok)
        if(ok)then
          n = n + 1
            write(string,'(f8.2)') value
          call output('System Gain:         '//string
     *                //'   Jy/K')
        endif

        call obspar(observ,'subdiam',value,ok)
        if(ok)then
          n = n + 1
            write(string,'(f8.2)') value
          call output('Subreflector diam:   '//string
     *                //'   metres')
        endif

        call obspar(observ,'systemp',value,ok)
        if(ok)then
          n = n + 1
          write(string,'(f8.2)') value
          call output('System Temp :        '//string
     *                //'   K')
        endif

        call obspar(observ,'ellimit',value,ok)
        if(ok)then
          n = n + 1
          value=180/pi*value
          write(string,'(f8.2)') value
          call output('Elevation Limit:     '//string
     *                //'   degrees')
        endif

        call obspar(observ,'nants',value,ok)
        if(ok)then
          n = n + 1
          write(string,'(i4)') nint(value)
          call output('Number of Antennas:  '//string)
        endif

        call obspar(observ,'ew',value,ok)
        if(ok)then
          n = n + 1
          write(string,'(f6.2)') value
          call output('E-W Array:           '//string)
        endif

        if(n.eq.0)call bug('w','No information was found')
      end do

      end
