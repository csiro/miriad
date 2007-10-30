c************************************************************************
	program uvplanet
	implicit none
c= uvplanet -- Fiddle planetary and solar system data.
c& rjs
c: uv analysis
c+
c	uvplanet is a Miriad task used to fiddle visibility datasets
c	from planetary (and other solar system) observations. It can:
c	  - Add planet orientation parameters (plmaj,plmin,plangle) to
c	    the dataset. uvplanet recognises planets by their source name,
c	    and knows their orientations as a function of time.
c	  - Apply a time-varying phase shift to track the object.
c	  - Subtract a thermal disk.
c	  - Subtract background sources (which will move relative to the phase
c	    centre).
c@ vis
c	The name of the input uv datasets. Several can be given
c	Wildcards are supported. No default.
c@ out
c	The name of the output uv data-set. There is no default name.
c@ select
c	The normal uv selection commands. The default is to select everything.
c@ pltb
c	For planets only: Blackbody temperature of the disk to subtract off.
c	Default is 0 (i.e. do not subtract off a disk).
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreviated to the minimum
c	needed to avoid ambiguity. Possible options are:
c	   replace     Replace the data with the blackbody disk and point
c	               sources (the normal behaviour is to subtract the
c	               blackbody disk and point sources).
c	   magnetic    For Jupiter only: The magnetic axis is used to 
c	               set planet orientation parameters. This overrides the
c	               normal behaviour of using the spin axis.
c	   arcane      The format for the pmotion text files is the
c	               "arcane" format (see below).
c	The following options can be used to turn off calibration corrections.
c	The default is to apply any calibration present.
c	   nocal       Do not apply the gains table.
c	   nopass      Do not apply bandpass corrections.
c	   nopol       Do not apply polarization corrections.
c@ sources
c	This gives the position and flux of point sources to be subtracted
c	from the visibility dataset. Many sources can be given. Each
c	source is specified by three numbers:
c	  ra dec flux
c	The RA and DEC are in normal Miriad format, and the flux is in Jy.
c@ pmotion
c	This parameter allows phase shifts to be applied to the data to
c	make the phase centre track the object of interest. Assuming that
c	the object has significant proper motion, this can
c	correct for phase tracking errors (e.g. wrong ephemeris) or perhaps 
c	no tracking at all. One or two tracking files can be given. The first
c	file gives the centre of the object (as a function of time). The second
c	gives the phase centre of the observation (as a function of time).
c	If both files are given, the data are phase shifted to the centre
c	of the object (i.e. corrects for tracking errors). If only one file
c	is given, then the RA and DEC in the dataset are taken as the
c	phase centre of the telescope.
c
c	The "pmotion" parameter can also be used to correct the RA and DEC
c	stored in the dataset. If these are incorrect (e.g. many telescopes
c	and the FITS format, do not store the time-varying RA/DEC, even though
c	they track time varying RA/DEC), then giving the one text file
c	twice will cause the RA and DEC to be corrected and no shifts being
c	applied.
c
c	The text files contain potentially many lines, with each line
c	giving a sky position at a given time.
c	Two formats for the text files are possible (if there are
c	two input files, both must be in the same format). The normal format
c	consists of 3 values per line, being
c
c	  time ra dec
c
c	with "time" (UTC), "ra" and "dec" being the position of the object at
c	a given time. The time, ra and dec are in the normal Miriad formats.
c	The times should be in increasing order. Linear interpolation
c	is used to estimate positions between the given times.
c
c	The other possible format -- the arcane format, which will be expected 
c	if "options=arcane" is given -- consists of 6 values per line, being
c
c	  start_time time ra dec dra ddec
c
c	The times are in modifed Julian days, ra and dec are in turns,
c	and the rate parameters, "dra" and "ddec", are in turns/day. The
c	record is considered valid after "start_time", and the rates
c	are used to estimate positions between given times.
c--
c  History:
c    10dec97 rjs  Created from uvjup, which this generalised and supercedes.
c    21jun99 rjs  Added options=replace.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer MAXSCAN,MAXSRC
	parameter(MAXSCAN=1024,MAXSRC=200)
	character version*(*)
	parameter(version='uvPlanet: version 1.0 21-Jun-99')
c
	character out*64,ltype*16,uvflags*16
	real pltb
	integer lIn,lOut
	logical dojaxis,arcane,first,dorep
	integer jscan,tscan
	double precision jdata(6,MAXSCAN),tdata(6,MAXSCAN)
	integer nsrc
	double precision sra(MAXSRC),sdec(MAXSRC)
	real flux(MAXSRC)
	character jpath*64,tpath*64
c
c  Externals.
c
	logical uvDatOpn,keyprsnt
	character itoaf*8
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(dorep,dojaxis,arcane,uvflags)
	call uvDatInp('vis',uvflags)
	call keyr('pltb',pltb,0.)
	call keya('pmotion',jpath,' ')
	jscan = 0
	if(jpath.ne.' ')call pathload(jpath,jdata,jscan,MAXSCAN,arcane)
	call keya('pmotion',tpath,' ')
	tscan = 0
	if(tpath.ne.' ')call pathload(tpath,tdata,tscan,MAXSCAN,arcane)
c
	nsrc = 0
	dowhile(keyprsnt('sources'))
	  nsrc = nsrc + 1
	  if(nsrc.gt.MAXSRC)call bug('f','Too many sources')
	  call keyt('sources',sra(nsrc),'hms',0.d0)
	  call keyt('sources',sdec(nsrc),'dms',0.d0)
	  call keyr('sources',flux(nsrc),0.0)
	enddo
	if(nsrc.gt.0)
     *	  call output('Number of sources read: '//itoaf(nsrc))
c
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','An output must be given')
	call keyfin
c
c  Open the inputs and the outputs.
c
	first = .true.
	dowhile(uvDatOpn(lIn))
	  call uvDatGta('ltype',ltype)
	  call VarInit(lIn,ltype)
c
	  if(first)then
	    call uvopen(lOut,out,'new')
	    call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	    call hdcopy(lIn,lOut,'history')
	    call hisopen(lOut,'append')
	    call hiswrite(lOut,'UVPLANET: Miriad '//version)
	    call hisinput(lOut,'UVPLANET')
	    call hisclose(lOut)
	    first = .false.
	  endif
	  call VarOnit(lIn,lOut,ltype)
c
c  Do the work.
c
	  call Process(lIn,lOut,pltb,dorep,dojaxis,
     *	    tscan,tdata,jscan,jdata,sra,sdec,flux,nsrc)
c
c  All said and done. Close up shop.
c
	  call uvDatCls
	enddo
c
	if(first)call bug('f','No input datasets found')
	call uvclose(lOut)
	end
c************************************************************************
	subroutine GetOpt(dorep,dojaxis,arcane,uvflags)
c
	implicit none
	logical dojaxis,arcane,dorep
	character uvflags*(*)
c
c  Determine extra processing options.
c
c  Output:
c    uvflags
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=6)
	logical present(NOPTS)
	character opts(NOPTS)*8
c
	data opts/'magnetic','nocal   ','nopol   ','nopass  ',
     *		  'arcane  ','replace '/
c
	call options('options',opts,present,NOPTS)
	dojaxis = present(1)
	arcane  = present(5)
	dorep   = present(6)
c
c  Determine the flags to pass to the uvDat routines.
c    d - Data selection.
c    3 - Return w (as well as u and v).
c    c - Apply gain table.
c    e - Apply leakage correction.
c    f - Apply bandpass correction.
c
	uvflags = 'd3'
	if(.not.present(2))uvflags(5:5) = 'c'
	if(.not.present(3))uvflags(6:6) = 'e'
	if(.not.present(4))uvflags(7:7) = 'f'
c
	end
c************************************************************************
	subroutine Process(lIn,lOut,pltb,dorep,dojaxis,
     *			tscan,tdata,jscan,jdata,sra,sdec,flux,nsrc)
c
	implicit none
	integer lIn,lOut
	logical dojaxis,dorep
	real pltb
	integer tscan,jscan,nsrc
	double precision tdata(6,*),jdata(6,*),sra(*),sdec(*)
	real flux(*)
c
c  Do all the real work.
c
c  Input:
c    lIn
c    lOut
c    dojaxis	Write out the position angle related to the magnetic
c		axis (not spin axis).
c    pltb	Planet brightness temperature to subtract off.
c
c------------------------------------------------------------------------
	integer JUPITER
	parameter(JUPITER=5)
	include 'maxdim.h'
	include 'mirconst.h'
	integer nchan,i,j,npol,pol,itscan,ijscan,iplanet,vupd
	double precision preamble(5),sub(3)
	double precision lamIII,latitude,utc,tdb,uu,vv
	double precision tra,tdec,jra,jdec,dra,ddec
	real a,b,disk,bmaj,bmin,bpa,cospa,sinpa
	logical flags(MAXCHAN)
	double precision freq(MAXCHAN),dist
	complex data(MAXCHAN)
	double precision x(2),lmn(3),theta0
	real theta
	complex w
	integer coObj
c
c  Externals.
c
	real j1xbyx
	logical polspara,uvVarUpd
	double precision deltime
c
c  Initialise for the various recomputations.
c
	itscan = 1
	ijscan = 1
c
	call uvVarIni(lIn,vupd)
	call uvVarSet(vupd,'source')
c
c  Read the first record.
c
	
	call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
c
c  the default rest frame, if we are doing velocity recomputation
c  and a rest frame has not been given.
c
	dowhile(nchan.gt.0)
c
	  if(dorep)then
	    do i=1,nchan
	      data(i) = 0
	    enddo
	  endif
c
c  Get planet name.
c
	  if(uvVarUpd(vupd))call getplan(lIn,iplanet)
	  call uvinfo(lIn,'sfreq',freq)
c
c  Copy all the variables from the input to the output.
c
	  call uvDatGti('npol',npol)
	  if(npol.eq.0)
     *	    call bug('f','Could not determine number of polarisations')
	  call uvDatGti('pol',pol)
	  call uvputvri(lOut,'npol',npol,1)
	  call uvputvri(lOut,'pol',pol,1)
	  call VarCopy(lIn,lOut)
c
	  uu = preamble(1)
	  vv = preamble(2)
	  utc = preamble(4)
	  if(iplanet.ne.0)then
	    tdb = utc + deltime(utc,'tdb')
	    call plpar(tdb,iplanet,sub,dist,bmaj,bmin,bpa)
	  endif
c
c  Fix the phase error in the observations.
c
	  if(jscan.gt.0)then
	    call GetRaDec(jscan,ijscan,jdata,utc,jra,jdec)
	    if(tscan.gt.0)then
	      call GetRaDec(tscan,itscan,tdata,utc,tra,tdec)
	    else
	      call uvrdvrd(lIn,'ra',tra,0.d0)
	      call uvrdvrd(lIn,'dec',tdec,0.d0)
	    endif
	    dra  = (jra - tra ) * cos(jdec)
	    ddec = jdec - tdec
	    if(abs(dra)+abs(ddec).gt.0)
     *		call Jupfix(preamble,freq,data,nchan,dra,ddec)
	    call uvputvrd(lOut,'ra',jra,1)
	    call uvputvrd(lOut,'dec',jdec,1)
	  else
	    call uvrdvrd(lIn,'ra',jra,0.d0)
	    call uvrdvrd(lIn,'dec',jdec,0.d0)
	  endif
c
c  Subtract off point sources, if needed.
c
	  if(nsrc.gt.0.and.polspara(pol))then
	    call coRaDec(coObj,'SIN',jra,jdec)
	    do j=1,nsrc
	      x(1) = sra(j)
	      x(2) = sdec(j)
	      call coLMN(coObj,'aw/aw',x,lmn)
	      theta0 = 2*DPI*(preamble(1)*lmn(1) +
     *			      preamble(2)*lmn(2) +
     *			      preamble(3)*(lmn(3)-1))
	      do i=1,nchan
		theta = theta0 * freq(i)
		w = flux(j)*cmplx(cos(theta),sin(theta))
		data(i) = data(i) - w
	      enddo
	    enddo
	    call coFin(coObj)
	  endif
	    
c
c  Subract the disk response if appropriate.
c
	  if(pltb.gt.0.and.polspara(pol).and.iplanet.ne.0)then
	    cospa = cos(bpa)
	    sinpa = sin(bpa)
	    b = PI * sqrt((bmaj*(uu*cospa-vv*sinpa))**2
     *                  + (bmin*(uu*sinpa+vv*cospa))**2)
	    a = 2 * pltb * (KMKS*1e18)/(CMKS*CMKS*1e-26)
     *	      * 2 * PI/4 * bmaj*bmin
	    do i=1,nchan
	      disk = a*freq(i)*freq(i)*j1xbyx(real(b*freq(i)))
	      data(i) = data(i) - disk
	    enddo
	  endif
	  if(pltb.ne.0.and.iplanet.ne.0)call uvputvrr(lOut,'pltb',0.,1)
c
	  if(dorep)then
	    do i=1,nchan
	      data(i) = -data(i)
	    enddo
	  endif
c
c  Compute the position angle of the magnetic axis.
c
	  if(dojaxis.and.iplanet.eq.JUPITER)then
	    sub(2) = -sub(2)
	    call lmn2sph(sub,lamIII,latitude)
	    bpa = bpa - 10.*PI/180.*sin(lamIII-200.*PI/180.)
	  endif
c
	  if(iplanet.ne.0)then
	    call uvputvrr(lOut,'plangle',180/PI*bpa,1)
	    call uvputvrr(lOut,'plmaj',3600*180/PI*bmaj,1)
	    call uvputvrr(lOut,'plmin',3600*180/PI*bmin,1)
	  endif
c
c  All done. Loop the loop.
c
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	end
c************************************************************************
	subroutine getplan(lIn,iplanet)
c
	implicit none
	integer lIn,iplanet
c
c  Determine the planet number of the source.
c------------------------------------------------------------------------
        integer NSOLAR
        parameter(NSOLAR=9)
	integer np(NSOLAR)
        character solar(NSOLAR)*8
	character source*32
	integer l
c
c  Externals.
c
        integer binsrcha,len1
c
        data solar/'earth   ','jupiter ','mars    ','mercury ',
     *  'neptune ','pluto   ','saturn  ','uranus  ','venus   '/
	data np   / 3,         5,	  4,         1,
     *	 8,	    9,	       6,	  7,	     2/
c
c  Look for the source name in the list of solar system objects.
c  
        call uvrdvra(lIn,'source',source,' ')
        call lcase(source)
        iplanet = binsrcha(source,solar,NSOLAR)
	if(iplanet.ne.0)then
	  call output('Found data for planet '//solar(iplanet))
	  iplanet = np(iplanet)
	else
	  l = len1(source)
	  call output('Found data for '//source(1:l)//
     *		'; this is not recognised as a planet.')
	endif
c
	end
c************************************************************************
	subroutine pathload(path,data,nscan,MAXSCAN,arcane)
c
	implicit none
	character path*(*)
	integer nscan,MAXSCAN
	double precision data(6,MAXSCAN)
	logical arcane
c
c  The input text file gives the path of the telescope phase centre or planet
c  as a sequence of records. Each record consists of
c    stime,rtime,ra,dec,prm,pdec
c  Where
c    stime -- Start time of the validity of this record, as MJD.
c    rtime -- Reference time for this record, as MJD.
c    ra    -- R.A. at reference time, in turns.
c    dec   -- DEC at reference time, in turns.
c    prm   -- Proper motion of RA in turns/day.
c    pdec  -- Proper motion of DEC in turns/day.
c
c  These are converted to times in Julian days, and angular units in
c  radians or radians/day.
c------------------------------------------------------------------------
	integer length,i,i1,i2
	character line*128
	double precision val
c
	include 'mirconst.h'
	double precision scale(6),offset(6)
c
c  Externals.
c
	integer len1,tinNext
	character itoaf*8
c
	data scale/ 1.d0,1.d0,DTWOPI,DTWOPI,DTWOPI,DTWOPI/
	data offset/2 400 000.5d0,2 400 000.5d0,0.d0, 0.d0, 0.d0, 0.d0/
c
c  Open the input text file.
c
	call tinOpen(path,'n')
c
	nscan = 0
	dowhile(tinNext().gt.0)
	  nscan = nscan + 1
	  if(nscan.gt.MAXSCAN)call bug('f','Too many scans')
	  if(arcane)then
	    do i=1,6
	      call tinGetd(val,0.d0)
	      data(i,nscan) = scale(i)*val + offset(i)
	    enddo
	  else
	    call tinGett(data(2,nscan),0.d0,'atime')
	    call tinGett(data(3,nscan),0.d0,'hms')
	    call tinGett(data(4,nscan),0.d0,'dms')
	  endif
	enddo
	call tinClose
c
c  For the normal format, make it look like the arcane format.
c
	if(.not.arcane)then
	  if(nscan.eq.1)call bug('f','Too few proper motion records')
	  do i=1,nscan
	    i1 = max(i-1,1)
	    i2 = min(i+1,nscan)
	    data(1,i) = 0.5*(data(2,i)+data(2,i1))
	    data(5,i) = (data(3,i2)-data(3,i1))/(data(2,i2)-data(2,i1))
	    data(6,i) = (data(4,i2)-data(4,i1))/(data(2,i2)-data(2,i1))
	  enddo
	endif
c
	line = 'Loaded '//itoaf(nscan)
	length = len1(line)
	line(length+1:) = ' records from file '//path
	length = len1(line)
	call output(line(1:length))
c
	end
c************************************************************************
	subroutine GetRaDec(nscan,iscan,data,time,ra,dec)
c
	implicit none
	integer nscan,iscan
	double precision data(6,nscan),time,ra,dec
c------------------------------------------------------------------------
	if(iscan.le.nscan-1.and.time.ge.data(1,iscan).and.
     *			      time.lt.data(1,iscan+1))then
	  continue
	else if(iscan.le.nscan-2.and.time.ge.data(1,iscan+1).and.
     *			             time.lt.data(1,iscan+2))then
	  iscan = iscan + 1
	else if(time.ge.data(1,nscan))then
	  iscan = nscan
	else
	  if(time.lt.data(1,iscan))iscan = 1
	  dowhile(time.gt.data(1,iscan+1))
	    iscan = iscan + 1
	  enddo
	endif
c
	ra  = data(3,iscan) + data(5,iscan)*(time-data(2,iscan))
	dec = data(4,iscan) + data(6,iscan)*(time-data(2,iscan))
c
	end
c************************************************************************
	subroutine Jupfix(uv,freq,data,nchan,dra,ddec)
c
	implicit none
	integer nchan
	double precision uv(2),dra,ddec,freq(nchan)
	complex data(nchan)
c
c  Correct for a error in the phasing of the data. Use the small
c  angle approximation.
c
c  Input:
c    lIn	Handle of the input data-set.
c    nchan	Number of channels.
c    uv		UV coordinates, in nanosec.
c    freq	Sky frequency, in GHz.
c    dra,ddec	Shift to apply in radians.
c		True Position = Incorrect_Position + dra/ddec
c  Input/Output:
c    data	Visibility data.
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	integer i
	real theta,theta0
	complex w
c
	theta0 = -2*dpi*( uv(1)*dra + uv(2)*ddec )
c
	do i=1,nchan
	  theta = theta0 * freq(i)
	  w = cmplx(cos(theta),sin(theta))
	  data(i) = w * data(i)
	enddo
c
	end
c************************************************************************
	logical function varprsnt(tno,var)
c
	implicit none
	integer tno
	character var*(*)
c
c  Determine whether a variable is present.
c------------------------------------------------------------------------
	character type*1
	logical upd
	integer n
c
	call uvprobvr(tno,var,type,n,upd)
	varprsnt = type.ne.' '
	end

