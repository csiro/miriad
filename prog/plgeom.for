c************************************************************************
        program plgeom
        implicit none
c= plgeom -- Fiddle planetary and solar system data.
c& rjs
c: uv analysis
c+
c	plgeom is a Miriad task used to manipulate and rephase
c	a visibility dataset so that an image formed from that
c	dataset will result in a facet of that point. It achieves
c	this by both applying a shift to the data, and manipulating
c	the u-v coordinates. The parameters used to compute noise
c	variance of a visibility are also manipulated to account for
c	poorer sensitivity when observing the facet at oblique angles.
c
c	See Sault, Engel and de Pater (Icarus 2004) for more details
c	of its faceting algorithm.
c@ vis
c	The name of the input uv datasets. Several can be given
c	Wildcards are supported. No default.
c@ out
c	The name of the output uv data-set. There is no default name.
c@ select
c	The normal uv selection commands. The default is to select everything.
c@ latlon
c	The latitude and longitude of the facet centre. The latitude
c	is planetographic. For Jupiter, the longitude is System III.
c@ tol
c	When the fractional sensitivity of a modified visibility drops
c	below "tol" of the original measurement, then the visibility
c	is flagged. The default value is 0.1.
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreviated to the minimum
c	needed to avoid ambiguity. Possible options are:
c	   nocal       Do not apply the gains table.
c	   nopass      Do not apply bandpass corrections.
c	   nopol       Do not apply polarization corrections.
c--
c  History:
c    10dec97 rjs  Created from uvjup, which this generalised and supercedes.
c    21jun99 rjs  Added options=replace.
c    09may06 rjs  Reborn as plgeom. Much work on the code.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
c
        character version*(*)
        parameter(version='plGeom: version 1.0 09-May-06')
c
        character out*64,ltype*16,uvflags*16
        real tol
        integer lin,lout
        logical first
        double precision ll(3)
c
c  Externals.
c
        logical uvdatopn
c
c  Get the input parameters.
c
        call output(version)
        call keyini
        call getopt(uvflags)
        call uvdatinp('vis',uvflags)
        call keyd('latlon',ll(1),0.0d0)
        call keyd('latlon',ll(2),0.0d0)
	call keyr('tol',tol,0.1)
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','An output must be given')
        call keyfin
c
c  Open the inputs and the outputs.
c
        first = .true.
        dowhile(uvdatopn(lin))
          call uvdatgta('ltype',ltype)
          call varinit(lin,ltype)
c
          if(first)then
            call uvopen(lout,out,'new')
            call uvset(lout,'preamble','uvw/time/baseline',0,0.,0.,0.)
            call hdcopy(lin,lout,'history')
            call hisopen(lout,'append')
            call hiswrite(lout,'PLGEOM: Miriad '//version)
            call hisinput(lout,'PLGEOM')
            call hisclose(lout)
            first = .false.
          endif
          call varonit(lin,lout,ltype)
c
c  Do the work.
c
          call process(lin,lout,ll,tol)
c
c  All said and done. Close up shop.
c
          call uvdatcls
        enddo
c
        if(first)call bug('f','No input datasets found')
        call uvclose(lout)
        end
c************************************************************************
        subroutine getopt(uvflags)
c
        implicit none
        character uvflags*(*)
c
c  Determine extra processing options.
c
c  Output:
c    uvflags
c------------------------------------------------------------------------
        integer nopts
        parameter(nopts=3)
        logical present(nopts)
        character opts(nopts)*8
c
        data opts/'nocal   ','nopol   ','nopass  '/
c
        call options('options',opts,present,nopts)
c
c  Determine the flags to pass to the uvDat routines.
c    d - Data selection.
c    3 - Return w (as well as u and v).
c    c - Apply gain table.
c    e - Apply leakage correction.
c    f - Apply bandpass correction.
c
        uvflags = 'd3'
        if(.not.present(1))uvflags(5:5) = 'c'
        if(.not.present(2))uvflags(6:6) = 'e'
        if(.not.present(3))uvflags(7:7) = 'f'
c
        end
c************************************************************************
        subroutine process(lin,lout,ll,tol)
c
        implicit none
        integer lin,lout
        double precision ll(2)
        real tol
c
c  Do all the real work.
c
c  Input:
c    lIn
c    lOut
c    ll
c    tol
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
c
        integer nchan,i,npol,pol,iplanet,vupd
        double precision preamble(5),sub(3),xyz(3)
        double precision utc,tdb,uu,vv
        real bmaj,bmin,bpa,cospa,sinpa
	real sin1,sin2,sin3,sin4,cos1,cos2,cos3,cos4
        logical flags(MAXCHAN)
        double precision freq(MAXCHAN),dist
        complex data(MAXCHAN)
        double precision long,lat,longe,late,det
        real theta,jyperk
        double precision yy,zz,yi,zi,theta1,theta2,lambda3,theta4,rad
	double precision alpha,delta,wprime,radius,flat
        complex fac
c
c  Externals.
c
        logical uvvarupd
        double precision deltime
c
        call uvvarini(lin,vupd)
        call uvvarset(vupd,'source')
c
c  Read the first record.
c
        call uvdatrd(preamble,data,flags,maxchan,nchan)
	iplanet = 0
        dowhile(nchan.gt.0)
c
c  Get planet name.
c
          if(uvvarupd(vupd))call getplan(lin,iplanet)
	  if(iplanet.eq.0)call bug('f','Data not recognised as planet')
          call uvinfo(lin,'sfreq',freq)
c
c  Copy all the variables from the input to the output.
c
          call uvdatgti('npol',npol)
          if(npol.eq.0)
     *      call bug('f','Could not determine number of polarisations')
          call uvdatgti('pol',pol)
          call uvputvri(lout,'npol',npol,1)
          call uvputvri(lout,'pol',pol,1)
          call varcopy(lin,lout)
c
          uu = preamble(1)
          vv = preamble(2)
          utc = preamble(4)
          tdb = utc + deltime(utc,'tdb')
          call plpar(tdb,iplanet,sub,dist,bmaj,bmin,bpa)
	  call plphyeph(tdb,iplanet,alpha,delta,wprime,radius,flat)
c
c  Here is where the shift and transformation is done
c
	  sinpa=sin(bpa)
	  cospa=cos(bpa)
c
	  lat=ll(1)*PI/180
	  long=ll(2)*PI/180
	  call ll2xyz(lat,long,xyz(1),xyz(2),xyz(3),flat)
	  rad = sqrt(xyz(1)*xyz(1)+xyz(2)*xyz(2)+xyz(3)*xyz(3))
	  call lmn2sph(sub,longe,late)
	  call lmn2sph(xyz,long,lat)
c
	  theta1  = late - atan(tan(late*(bmin/bmaj)*(bmin/bmaj)))
	  theta2  = lat
	  lambda3 = long - longe
	  theta4  = -late
c
	  sin1 = sin(theta1)
	  cos1 = cos(theta1)
	  sin2 = sin(theta2)
	  cos2 = cos(theta2)
	  sin3 = sin(lambda3)
	  cos3 = cos(lambda3)
	  sin4 = sin(theta4)
	  cos4 = cos(theta4)
c
	  yy = (bmaj/2.0)*rad*cos2*sin3
	  zz = (bmaj/2.0)*rad*(cos2*cos3*sin4 + sin2*cos4)
c
	  yi =  -(yy*cospa-zz*sinpa)
	  zi =   (yy*sinpa+zz*cospa)
c
	  theta=-2.0*PI*(uu*yi+vv*zi)
c
          do i=1,nchan
            fac=cmplx(cos(theta*freq(i)),sin(theta*freq(i)))
	    data(i)=data(i)*fac
          enddo
c
	  preamble(1) = -uu*cos3-vv*sin3*sin4
	  preamble(2) = uu*(sin1*cos2*sin3+cos1*sin2*sin3) +
     *			vv*(cos1*(-sin2*cos3*sin4+cos2*cos4)-
     *			    sin1*(cos2*cos3*sin4+sin2*cos4))
	  uu = -preamble(1)
	  vv =  preamble(2)
	  preamble(1) = uu*cospa - vv*sinpa
	  preamble(2) = uu*sinpa + vv*cospa
	  bpa = 0
c
	  det = cos3*(cos1*(-sin2*cos3*sin4+cos2*cos4)
     *		     -sin1*( cos2*cos3*sin4+sin2*cos4))
     *		-sin3*sin4*(sin1*cos2*sin3+cos1*sin2*sin3)
c
	  if(det.lt.tol) then
	    do i=1,nchan
	      flags(i) = .false.
	    enddo
	  else
	    do i=1,nchan
	      data(i) = data(i) / det
	    enddo
	  endif
	  call uvrdvrr(lin,'jyperk',jyperk,8.0)
	  jyperk = jyperk / abs(det)
	  call uvputvrr(lout,'jyperk',jyperk,1)
c
          call uvputvrr(lout,'plangle',180/PI*bpa,1)
          call uvputvrr(lout,'plmaj',3600*180/PI*bmaj,1)
          call uvputvrr(lout,'plmin',3600*180/PI*bmin,1)
c
c  All done. Loop the loop.
c
          call uvwrite(lout,preamble,data,flags,nchan)
          call uvdatrd(preamble,data,flags,maxchan,nchan)
        enddo
c
        end
c************************************************************************
	subroutine ll2xyz(lat,long,x,y,z,f)
c
	implicit none
	double precision x,y,z,lat,long,f
c
c  Convert betweena location defined in terms
c  geodetic latitude, longitude, and height above the reference geoid to
c  CIO coordinates.
c
c  Input:
c   lat,long    Geodetic latitude and longitude, in radians.
c   f           Flattening number.
c  Output:
c   x,y,z	Cartesian coordinates.
c-----------------------------------------------------------------------
	double precision fm12,Nphi
c
	fm12=(1-f*(2-f))
	Nphi = 1.0d0/sqrt(1-f*(2-f)*sin(lat)**2)
	x = (Nphi)*cos(-long)*cos(lat)
	y = (Nphi)*sin(-long)*cos(lat)
	z = (fm12*Nphi)*sin(lat)
c
	end
c************************************************************************
        subroutine getplan(lin,iplanet)
c
        implicit none
        integer lin,iplanet
c
c  Determine the planet number of the source.
c------------------------------------------------------------------------
        character source*32
	integer l
c
c  Externals.
c
        integer plLook,len1
c
c  Look for the source name in the list of solar system objects.
c
        call uvrdvra(lin,'source',source,' ')
	iplanet = plLook(source)
        if(iplanet.ne.0)then
          call output('Found data for planet '//source)
        else
          l = max(len1(source),1)
          call output('Found data for '//source(1:l)//
     *          '; this is not recognised as a planet.')
        endif
c
        end
