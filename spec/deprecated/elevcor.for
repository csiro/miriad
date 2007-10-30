c************************************************************************
	program elevcor
c
	implicit none
c
c= elevcor - Correct ATCA data for elevation-dependent gain. 
c& dpr
c: uv analysis
c+
c       ElevCor corrects ATCA data for elevation-dependent gain, using a
c       pre-defined template. This may be useful when the target and
c       secondary calibrator are sufficiently distant on the sky that a
c       gain elevation dependence cannot be subsumed into the usual time
c       dependent gains.
c
c       ElevCor scales the visibilities themselves, it doesn't mess with
c       the gains. Actually, elevcor doesn't copy or apply existing
c       gain/pol/bandpass tables at all. If you want to retain
c       pre-existing calibration (eg. bandpass), copy them after with
c       gpcopy. Of course, existing amplitude gains won't be very
c       useful.
c
c       Currenly, the only template available is for 3cm data, which was
c       defined from observations at 8640GHz. For details of the gain
c       elevation-dependence template, blame Hayley Bignall.
c
c@ vis
c	The names of the input uv data set. No default.
c@ out
c	The name of the output uv data set. No default.
c@ options
c       Extra processing options, minimum match allowed:
c          simulate  Add a gain error to the data, rather
c                    than applying the correction. This can be
c                    used on data generated with uvgen to simulate
c                    elevation-dependence in fake uvdata.
c
c--
c  History:
c    20jun01 dpr  Original version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	parameter(version='elevcor: version 1.0 20-Jun-01')
	integer PolXX,PolYY,PolXY,PolYX
	parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
c
	integer lVis,lOut,vupd,pol,npol,i,j,k,nants
	logical updated
        logical dosim ! run in simulation mode
	character vis*256,out*256,type*1
	integer nschan(MAXWIN),nif,nchan,length
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	double precision preamble(5),ptime
	double precision sfreq(MAXWIN),sdf(MAXWIN)
	double precision lst,lat,az,el,ra,dec,dtemp
	real freq0(MAXWIN)
c
c
c  Externals.
c
	logical uvvarUpd
	real EleScale
c
	call output(version)
	call keyini
        call GetOpt(dosim)
	call keya('vis',vis,' ')
	call keya('out',out,' ')
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
c
	call EleInit()
c
c  Get ready to copy the data.
c
	call uvopen(lVis,vis,'old')
	call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varInit(lVis,'channel')
c
	call uvvarIni(lVis,vupd)
	call uvvarSet(vupd,'nschan')
	call uvvarSet(vupd,'sfreq')
	call uvvarSet(vupd,'sdf')
	call uvvarSet(vupd,'ra')
	call uvvarSet(vupd,'obsra')
	call uvvarSet(vupd,'dec')
	call uvvarSet(vupd,'obsdec')
	call uvvarSet(vupd,'telescop')
	call uvvarSet(vupd,'latitud')
	call uvvarSet(vupd,'systemp')
	call uvvarSet(vupd,'ytsys')
	call uvvarSet(vupd,'xtsys')
c
	call uvopen(lOut,out,'new')
	call varOnit(lVis,lOut,'channel')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
c  Make the output history.
c
	call hdcopy(lVis,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'ELEVCOR: Miriad '//version)
	call hisinput(lOut,'ELEVCOR')
	call hisclose(lOut)
c
	call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	ptime = preamble(4) - 1
	dowhile(nchan.gt.0)
	  call uvrdvri(lVis,'pol',pol,0)
	  call uvrdvri(lVis,'npol',npol,0)
c
	  if(uvvarUpd(vupd))then
	    call uvrdvri(lVis,'nants',nants,0)
	    call uvprobvr(lVis,'nschan',type,length,updated)
	    nif = length
	    if(type.ne.'i'.or.length.le.0.or.length.gt.MAXWIN)
     *	      call bug('f','Invalid nschan parameter')
	    call uvgetvri(lVis,'nschan',nschan,nif)
c
	    call uvgetvrd(lVis,'sfreq',sfreq,nif)
	    call uvgetvrd(lVis,'sdf',sdf,nif)
	    do i=1,nif
	      freq0(i) = sfreq(i) + 0.5*(nschan(i)-1)*sdf(i)
	      freq0(i) = freq0(i) * 1e9
	    enddo
	    call uvrdvrd(lVis,'ra',dtemp,0.d0)
	    call uvrdvrd(lVis,'obsra',ra,dtemp)
	    call uvrdvrd(lVis,'dec',dtemp,0.d0)
	    call uvrdvrd(lVis,'obsdec',dec,dtemp)
	    call getlat(lVis,lat)
	  endif
c
	  call varCopy(lVis,lOut)
c

	  ptime = preamble(4)
	  call getlst(lVis,lst)
	  call azel(ra,dec,lst,lat,az,el)
c
c
	  if(npol.gt.0)then
	    call uvputvri(lOut,'npol',npol,1)
	    call uvputvri(lOut,'pol',pol,1)
	  endif
c
	  k = 0
	  if (.not. dosim) then
	    do i=1,nif
	      do j=1,nschan(i)
		k = k + 1
		data(k) = data(k) * EleScale(el,freq0(i),preamble(5))
	      enddo
	    enddo
	  else
	    do i=1,nif
	      do j=1,nschan(i)
		k = k + 1
		data(k) = data(k) / EleScale(el,freq0(i),preamble(5))
	      enddo
	    enddo
	  end if
c
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call uvclose(lVis)
	call uvclose(lOut)
	end
c************************************************************************
      subroutine getlst (lin, lst)
c
      implicit none
      integer lin
      double precision lst
c
c  Get lst of the current data point.
c
c  Input:
c    lin         Handle of file
c  Output:
c    lst         LAST in radians
c-----------------------------------------------------------------------
      double precision time,ra,long,dtemp
      character type*1
      integer length
      logical ok
c
c  Externals.
c
      double precision eqeq
c
      lst = 0.0d0
      call uvprobvr (lin, 'lst', type, length, ok)
      if (type(1:1).eq.' ') then
	call uvrdvrd (lin, 'ra', dtemp, 0.d0)
	call uvrdvrd (lin, 'obsra', ra, dtemp)
	call getlong(lin,long)
        call jullst (time, long, lst)
	lst = lst + eqeq(time)
      else
         call uvrdvrd (lin, 'lst', lst, 0.0d0)
      end if
c
      end
c************************************************************************
      subroutine getlong (lin, long)
c
c     Get longitude from variable or obspar subroutine
c
c  Input:
c    lin         Handle of file
c  Output:
c    longitude   Longitude in radians
c-----------------------------------------------------------------------
      integer lin
      double precision long
c
      character type*1, telescop*10
      integer length
      logical ok, printed
      save printed
      data printed/.false./
c------------------------------------------------------------------------ 
      long = 0.0d0
      call uvprobvr (lin, 'longitu', type, length, ok)
      if (type(1:1).eq.' ') then
         if(.not.printed)call bug ('w', 
     *		'No longitude variable; trying telescope')
	 printed = .true.
         call uvprobvr (lin, 'telescop', type, length, ok)
         if (type(1:1).eq.' ') then
            call bug ('f', 
     +      'No telescope variable either, can''t work out longitude')
         else
            call uvrdvra (lin, 'telescop', telescop, ' ')
            call obspar (telescop, 'longitude', long, ok)
            if (.not.ok) call bug('f', 
     +          'No valid longitude found for '//telescop)
         end if
      else
         call uvrdvrd (lin, 'longitu', long, 0.0d0)
      end if
c
      end
c************************************************************************
      subroutine getlat (lin, lat)
c
c     Get latitude from variable or obspar subroutine
c
c  Input:
c    lin         Handle of file
c  Output:
c    lat        Latitude in radians
c-----------------------------------------------------------------------
      integer lin
      double precision lat
c
      character type*1, telescop*10
      integer length
      logical ok, printed
      save printed
      data printed/.false./
c------------------------------------------------------------------------ 
      lat = 0.0d0
      call uvprobvr (lin, 'latitud', type, length, ok)
      if (type(1:1).eq.' ') then
         if(.not.printed)call bug ('w', 
     *		'No latitude variable; trying telescope')
	 printed = .true.
         call uvprobvr (lin, 'telescop', type, length, ok)
         if (type(1:1).eq.' ') then
            call bug ('f', 
     +      'No telescope variable either, can''t work out latitude')
         else
            call uvrdvra (lin, 'telescop', telescop, ' ')
            call obspar (telescop, 'latitude', lat, ok)
            if (.not.ok) call bug('f', 
     +          'No valid latitude found for '//telescop)
         end if
      else
         call uvrdvrd (lin, 'latitud', lat, 0.0d0)
      end if
c
      end
c************************************************************************
	subroutine EleInit ()
c
c     Init polynomial coefficients, or whatever else you
c     decide to parameterize your funtion as.
c
c-----------------------------------------------------------------------
	implicit none
c-----------------------------------------------------------------------
	include 'maxdim.h'
	include 'elevcor.h'
c	
	integer i,j
c
	data ((xpc(i,j),i=1,MAXANT),j=1,NPARAMS)/InIPARAMS*0./
        data (xpc(1,j),j=1,3)/1.00000,  -2.81563e-04,   6.41390e-06/
	data (xpc(2,j),j=1,3)/1.00000,  -2.89640e-04,   6.36535e-06/
	data (xpc(3,j),j=1,3)/1.00000,  -5.85414e-04,   1.09251e-05/
	data (xpc(4,j),j=1,3)/1.00000,  -1.61529e-04,   7.44309e-06/
	data (xpc(5,j),j=1,3)/1.00000,  -1.81528e-05,   5.27955e-06/
	data (xpc(6,j),j=1,3)/1.00000,  -1.02589e-03,   8.71686e-06/
c	
	end
c************************************************************************
      real function EleScale (el,freq0,baseline)
c
c     Get latitude from variable or obspar subroutine
c
c  Input:
c     el        elevation   (rad)
c     freq0     freq of obs (Hz)
c     baseline  baseline number
c  Output:
c     EleScale  factor to multiply vis by
c-----------------------------------------------------------------------
	implicit none
	double precision el
	real freq0
	double precision baseline
c-----------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	include 'elevcor.h'
	integer ant1, ant2   ! antennas used in this baseline
	real             za  ! zenith angle
	real           g1,g2 ! gain factors for the two antennas
        character*60   line
c
	call basant(baseline, ant1, ant2)
	za=90.0 - real(el*180.0/PI)
c
	if ((8.0e+9 .le. freq0) .and. (freq0 .le. 9.2e+9)) then
	  g1=xpc(ant1,1)*1.0 + xpc(ant1,2)*za + xpc(ant1,3)*za**2
     -          + xpc(ant1,4)*za**3 + xpc(ant1,4)*za**4
	  g2=xpc(ant2,1)*1.0 + xpc(ant2,2)*za + xpc(ant2,3)*za**2
     -          + xpc(ant2,4)*za**3 + xpc(ant2,4)*za**4
	else
	  write(line,'(a,f6.3,a)')
     -     'Elevation corrections are unknown for freq ',
     -     freq0/1e+9,'GHz'
	  call bug('f',line)
	end if
c
	EleScale=g1*g2
c
	end
c
c************************************************************************
        subroutine GetOpt(dosim)
c
        implicit none
        logical dosim
c
c  Determine the flags to pass to the uvdat routines.
c
c  Output:
c    dosim      Run in simulation mode
c------------------------------------------------------------------------
        integer nopts
        parameter(nopts=1)
        character opts(nopts)*9
        logical present(nopts)
        data opts/'simulate '/
c
        call options('options',opts,present,nopts)
        dosim = present(1)
	end 
