c************************************************************************
	program blfit
c
c= blfit - Determine antenna locations from visibility data.
c& rjs
c: uv analysis
c+
c	BLFIT is a Miriad task which solves for antenna locations (baseline
c	lengths) visibility data. It works by breaking the data into
c	a collection of sets, each set of which can contain several
c	scans. A set is a collection of data for which the antenna-based
c	instrumental phases are assumed constant. Different polarisation
c	measurements and measurements at different frequencies are taken
c	to be different sets. When observations are taken over a long period,
c	a time interval can be specified to limit the maximum time span
c	of a set. Scans are collections of data for a given source and
c	over some restricted time period (typically 5 minutes).
c@ vis
c	Name of the input visibility file(s). No default.
c@ stokes
c	Normal Stokes/polarisation parameter (e.g. i,q,u,v,ii etc).
c	Only a single polarisation can be requested. The default is
c	to select all parallel hand polarizations.
c@ line
c	Normal line-type processing with normal defaults. 
c@ select
c	Normal data selection. Default is all cross-correlation data.
c@ interval
c	This can give two numbers, in units of minutes. The first is the
c	interval over which to average the data when finding a solution.
c	The second gives the maximum time interval spanned by a set.
c@ refant
c	Reference antenna. No default.
c@ device
c	BLFIT optionally plots the data involved in the fit in
c	various ways. This parameter gives the
c	PGPLOT device. The default is to not create a plot.
c@ axis
c	The two axes to plot. The default is time vs residual.
c	Possible axes are time,residual,l,m,n,frequency,azimith,
c       dazimuth, elevation, ha, dec, 
c	and residual, nresidual (residuals, with those above 10 GHz
c	negated), predicted and raw phase.
c@ yrange
c	Plot range. The default is to autoscale.
c@ log
c	Output log file. The default is to not create a log file.
c@ tol
c	Tolerance, in mm, of the solution. The default is 1000 mm.
c@ factor
c	Frequency tolerance factor. Default is 1.3
c@ options
c	Extra processing options. Minimum match is used. Possible values are
c	  wdp    ATCA only: Solve for the "wrap dependent phase" term.
c	  model  ATCA only: Apply best known model.
c	  noz    Do not solve in the polar direction.
c--
c  History:
c    20nov04 rjs	Original version ?
c    17mar08 rjs	Compute az/el directly.
c    11dec08 rjs	Correct dimensioning of b1() and b2() in Solve (thanks
c			to Randall Wayth).
c------------------------------------------------------------------------
	integer PolXX,PolYY
	parameter(PolXX=-5,PolYY=-6)
	include 'mirconst.h'
	include 'maxdim.h'
	character version*(*)
        parameter(version='version 1.0 11-Dec-2008')
	include 'blfit.h'
c
	integer NP
	parameter(NP=4)
	real model(NP,NANT),moderr(NP,NANT),omodel(NP,NANT),tol,factor
	character device*64,xaxis*10,yaxis*10,logf*80,telescop*16
	integer lIn,vUpd,npol,i,j,nvis,nread,refant,ref2,nselect
	integer nbadsc,nbadset,npd
	double precision t0,lat,long,cosl,sinl
	real interval,gap,flo,fhi,f1,f2,fac,xd,yd,zd,yrange(2)
	double precision xyz(NANT,3)
	character names(NANT)*5
	logical first,newscan,more,thefirst,dowdp,domodel,noz
	logical doal,dohd
c
	character typeaz*1,typeel*1
	integer laz,lel
	logical uaz,uel,doazel
	double precision dtemp,ra,dec,lst
c
	double precision t,bl,az,daz,el,preamble(4)
	real sigma2,smon,temp
	double precision freq(MAXCHAN),Tbase,f160,f160d
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	integer set(MAXCHAN)
	character line*80
c
	integer Scan(MAXSET)
	logical sets(MAXSET)
c
c  Externals.
c
        character itoaf*8
	logical uvDatOpn,uvVarUpd
	integer scanMake,pgbeg
c
c  Get the inputs.
c
	call output('Blfit: '//version)
	call keyini
	call uvDatInp('vis','sdlefx')
	call keya('device',device,' ')
	call keyr('interval',interval,5.0)
	call keyr('interval',gap,120.0)
	interval = interval/(24.0*60.0)
	gap = gap/(24.0*60.0)
	call keyi('refant',refant,0)
	call keyi('refant',ref2,refant)
	if(min(refant,ref2).le.0.or.
     *	   max(refant,ref2).gt.NANT)
     *	  call bug('f','Invalid reference antenna')
	call keyr('yrange',yrange(1),0.)
	call keyr('yrange',yrange(2),yrange(1)-1.0)
	call keyr('tol',tol,1000.)
	call keyr('factor',factor,1.3)
	call getaxis(xaxis,yaxis)
	call keya('log',logf,' ') 
	call getopt(dowdp,domodel,noz)
        call keyfin
	doal = dowdp.or.domodel.or.xaxis.eq.'azimuth'.or.
     *		xaxis.eq.'dazimuth'.or.xaxis.eq.'elevation'
	dohd = noz.or.xaxis.eq.'ha'.or.xaxis.eq.'dec'.or..not.doal
	if(doal.and.dohd)
     *	  call bug('f','Unsupported combination of options')
c
c  Set the polarisations to ii if nothing was selected.
c
	call uvDatGti('npol',npol)
	if(npol.eq.0)call uvDatSet('stokes',0)
c
c  Open up the PGPLOT device for later plotting.
c
	if(device.ne.' ')then
	  if(pgbeg(0,device,3,2).ne.1)then
	    call pgldev
	    call bug('f','Failed to open PGPLOT device')
	  endif
	endif
c
	do j=1,NANT
	  do i=1,NP
	    omodel(i,j) = 0.0
	  enddo
	enddo
c
c  The big outer loop.
c
	thefirst = .true.
	more = .true.
	dowhile(more)
c
c  Set the sets to 0.
c
	call scanInit
	call setInit
	do i=1,MAXSET
	  Scan(i) = 0
	enddo
c
c  Open the visibility file, and read all the data.
c
	if(thefirst)then
	  call output('Reading the data ...')
	else
	  call output('Re-reading the data ...')
	  call uvDatRew()
	endif
        nvis = 0
	first = .true.
	doazel = .false.
	dowhile(uvDatOpn(lIn))
	  call uvvarIni(lIn,vUpd)
	  call uvvarSet(vUpd,'source')
	  call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	  call uvrdvra(lIn,'telescop',telescop,' ')
	  call getnames(lIn,names,NANT,telescop)
          dowhile(nread.gt.0)
	    t = preamble(3)
	    bl = preamble(4)
	    if(first)then
	      t0 = t
	      tbase = t
	      call uvgetvrd(lIn,'latitud',lat,1)
	      call uvgetvrd(lIn,'longitu',long,1)
	      call AntLd(lIn,lat,xyz,NANT,refant)
	      call uvprobvr(lIn,'antaz',typeaz,laz,uaz)
	      call uvprobvr(lIn,'antel',typeel,lel,uel)
	      doazel = .not.(typeaz.eq.'d'.and.typeel.eq.'d'.and.
     *			     laz.eq.1.and.lel.eq.1.and.uaz.and.uel)
	      doazel = doazel.or.dohd
	      first = .false.
	    endif
	    if(doazel)then
	      call uvrdvrd(lIn,'ra',dtemp,0.d0)
	      call uvrdvrd(lIn,'obsra',ra,dtemp)
	      call uvrdvrd(lIn,'dec',dtemp,0.d0)
	      call uvrdvrd(lIn,'obsdec',dec,dtemp)
	      call getlst(lIn,t,long,lst)
	      if(dohd)then
		az = lst - ra
		el = dec
	      else
		call azel(ra,dec,lst,lat,az,el)
	      endif
	    else
	      call uvgetvrd(lIn,'antaz',az,1)
	      call uvgetvrd(lIn,'antel',el,1)
	      az = DPI/180.d0 * az
	      el = DPI/180.d0 * el
	    endif
	    call uvrdvrd(lIn,'antdaz',daz,az)
	    call uvrdvrd(lIn,'f160',f160d,0.0d0)
	    daz = DPI/180.d0 * daz
	    call uvrdvrr(lIn,'smonrms',smon,150.0)
	    smon = 1.5*smon
c
	    t = t - tbase
	    newscan = uvVarUpd(vUpd)
	    if(.not.newscan)newscan = abs(t-t0).gt.interval
c
c  If its a new scan, flush out the existing scans.
c
	    if(newscan)then
	      do i=1,nset
		Scan(i) = 0
	      enddo
	      t0 = t
	    endif
c
	    call uvinfo(lIn,'sfreq',freq)
	    call setGet(lIn,nread,set)
	    call uvDatGtr('variance',sigma2)
	    if(sigma2.le.0)sigma2 = 1
c
	    do i=1,nread
	      if(flags(i))then
		if(Scan(set(i)).eq.0)then
		  Scan(set(i)) = scanMake(set(i))
		endif
		if(f160d.ne.0)then
		  f160 = f160d
		else if(freq(i).lt.12)then
		  f160 = freq(i)
		else if(freq(i).lt.30.0)then
		  f160 = freq(i) - 28.358
		else
		  f160 = freq(i) - 100.178
		endif
		call scanAdd(Scan(set(i)),
     *			t,bl,freq(i),f160,sigma2,az,el,daz,smon,
     *			data(i),omodel,NP,NANT,domodel)
		nvis = nvis + 1
	      endif
	    enddo
	    call uvDatRd(preamble,data,flags,MAXCHAN,nread)
	  enddo
	  call uvDatCls
	enddo
c
        if(nvis.le.0)call bug('f','No valid data found')
	if(nscan.le.0)call bug('f','No scans found')
	call output('Total number of correlations: '//itoaf(nvis))
	call output('Total number of scans: '//itoaf(nscan))
c
c  Make sure sets are Duplicate sets so that
c
	call scanDup(gap,tbase,flo,fhi)
	write(line,'(a,2f9.4,a)')'Frequency range:',flo,fhi,' GHz'
	call output(line)
	call output('Total number of sets: '//itoaf(nset))
	call output('Solving for scan phases ...')
	call scanAnt(xyz,NANT,refant,nbadsc,nbadset)
	if(nbadsc.gt.0)call bug('w',
     *	  'Scans lacking ref ant: '//itoaf(nbadsc))
	if(nbadset.gt.0)call bug('w',
     *	  'Sets discarded: '//itoaf(nbadset))
c
c  Now loop.
c
	do j=1,NANT
	  do i=1,NP
	    model(i,j) = 0.0
	  enddo
	enddo
c
	do i=1,nset
	  sets(i) = .false.
	enddo
c
	call output('Doing baseline solution ...')
	f1 = flo/1.15
	f2 = f1
	first = .true.
	nselect = 0
	dowhile(nselect+nbadsc.lt.nscan)
	  call getset(f1,f2,sets,nset,nselect,factor)
	  call output(' ')
	  call output(' ')
	  call output('****************************************'//
     *		      '*************************')
	  write(line,'(a,f7.2,a,f7.2,a)')'Frequency range:',
     *					 f1,' -',f2,' GHz'
	  call output(line)
	  call output('Scans selected: '//itoaf(nselect))
	  call output(' ')
	  if(dowdp)then
	    npd = 4
	  else
	    npd = 3
	  endif
	  if(noz)npd = npd - 1
	  call scanSolv(sets,nset,model,moderr,npd,NP,NANT,first)
	  call output(' ')
	  call output(
     *	   ' Ant       X (mm)            Y (mm)            Z (mm)')
	  call output(
     *	   ' ---   --------------    ---------------   ---------------')
c
	  do i=1,NANT
	    if(npd.eq.4)then
	      write(line,'(i4,4(f8.3,a,f6.3))')
     *			  i,model(1,i)+omodel(1,i),' +/-',moderr(1,i),
     *			    model(2,i)+omodel(2,i),' +/-',moderr(2,i),
     *			    model(3,i)+omodel(3,i),' +/-',moderr(3,i),
     *			    model(4,i)+omodel(4,i),' +/-',moderr(4,i)
	    else
	      write(line,'(i4,3(f8.3,a,f6.3))')
     *			  i,model(1,i)+omodel(1,i),' +/-',moderr(1,i),
     *			    model(2,i)+omodel(2,i),' +/-',moderr(2,i),
     *			    model(3,i)+omodel(3,i),' +/-',moderr(3,i)
	    endif
	    call output(line)
	  enddo
	  if(device.ne.' ')call plotit(sets,nset,yrange,
     *					xaxis,yaxis,names)
	  first = .false.
	enddo
c
	  thefirst = .false.
	  more = .false.
	  do j=1,NANT
	    do i=1,NP
	      omodel(i,j) = model(i,j)+omodel(i,j)
	      more = more.or.abs(model(i,j)).gt.tol
	    enddo
	  enddo
	enddo
	if(device.ne.' ')call pgend
c
c  Reference to a new antenna if needed.
c
	if(ref2.ne.refant)then
	  do i=1,NP
	    temp = omodel(i,ref2)
	    do j=1,NANT
	      omodel(i,j) = omodel(i,j) - temp
	    enddo
	  enddo
	endif
c
c  Print out the solution.
c
	if(logf.ne.' ')then
	  call logopen(logf,' ')
	  call loginput('Blfit: '//version)
	endif
	call output(' ')
	call output(' ')
	call output('****************************************'//
     *		      '*************************')
	call output('Miriad baseline convention:')
	call output('Ant X (nsec) Y (nsec) Z (nsec)')
	call output('--- -------- -------- --------')
	if(logf.ne.' ')then
	  call logWrit('# Miriad baseline convention:')
	  call logWrit('# Ant X (nsec) Y (nsec) Z (nsec)')
	  call logWrit('# --- -------- -------- --------')
	endif
	fac = 1e-3/(CMKS*1e-9)
	if(dohd)then
	  cosl = 0.d0
	  sinl = 1.d0
	else
	  cosl = cos(lat)
	  sinl = sin(lat)
	endif
	do i=1,NANT
	  xd = (-omodel(2,i)*sinl + omodel(3,i)*cosl)
	  yd = omodel(1,i)
	  zd = ( omodel(2,i)*cosl + omodel(3,i)*sinl)
	  omodel(1,i) = xd
	  omodel(2,i) = yd
	  omodel(3,i) = zd
	  write(line,'(i5,3f12.5)')i,xd*fac,yd*fac,zd*fac
	  call output(line)
	  if(logf.ne.' ')call logWrit('# '//line)
	enddo
	call output(' ')
	call output('CABSLN convention:')
	call output('Ant  X (mm)   Y (mm)   Z (mm)')
	call output('--- -------- -------- --------')
	if(logf.ne.' ')then
	  call logwrit('#')
	  call logWrit('# CABSLN convention:')
	  call logWrit('# Ant  X (mm)   Y (mm)   Z (mm)')
	  call logWrit('# --- -------- -------- --------')
	endif
c
	cosl = cos(long)
	sinl = sin(long)
	do i=1,NANT
	  xd = -(omodel(1,i)*cosl - omodel(2,i)*sinl)
	  yd = -(omodel(1,i)*sinl + omodel(2,i)*cosl)
	  zd = -omodel(3,i)
	  write(line,'(a,3f8.3)')names(i),xd,yd,zd
	  call output(line)
	  if(logf.ne.' ')call logWrit(line)
	enddo
c
	if(logf.ne.' ')call logClose
c
	end
c************************************************************************
	subroutine getopt(dowdp,domodel,noz)
c
	implicit none
	logical dowdp,domodel,noz
c
c  Extra processing options.
c  Output:
c    dowdp
c    noz
c------------------------------------------------------------------------
        integer nopts
        parameter(nopts=3) 
        character opts(nopts)*8
        logical present(nopts)
c
        data opts/'wdp     ','model   ','noz     '/
        call options('options',opts,present,nopts)
c
        dowdp = present(1)
	domodel = present(2)
	noz = present(3)
        end
c************************************************************************
	subroutine getaxis(xaxis,yaxis)
c
	implicit none
	character xaxis*(*),yaxis*(*)
c
c------------------------------------------------------------------------
	integer NAXIS
	parameter(NAXIS=14)
	character axes(NAXIS)*9,axis(2)*10
	logical invalid
	integer nax
	data axes/'time     ','residual ','ll       ','mm       ',
     *		  'nn       ','frequency','azimuth  ','elevation',
     *		  'dazimuth ','predicted','raw      ','nresidual',
     *		  'ha       ','dec      '/
c
	call keymatch('axis',NAXIS,axes,2,axis,nax)
	if(nax.lt.1)axis(1) = axes(1)
	if(nax.lt.2)axis(2) = axes(2)
	xaxis = axis(1)
	yaxis = axis(2)
c
	invalid = .false.
	if(yaxis.eq.'residual'.or.
     *     yaxis.eq.'predicted'.or.
     *     yaxis.eq.'raw'.or.
     *	   yaxis.eq.'nresidual')then
	  if(xaxis.ne.'ll'.and.xaxis.ne.'mm'.and.xaxis.ne.'nn'.and.
     *	     xaxis.ne.'time'.and.xaxis.ne.'frequency'.and.
     *	     xaxis.ne.'azimuth'.and.xaxis.ne.'elevation'.and.
     *	     xaxis.ne.'dazimuth'.and.xaxis.ne.'ha'.and.xaxis.ne.'dec')
     *	     invalid = .true.
	else if(yaxis.eq.'mm')then
	  if(xaxis.ne.'ll')invalid = .true.
	else
	  invalid = .true.
	endif
	if(invalid)call bug('f','Invalid combination of axes')
	end
c************************************************************************
	subroutine getset(f1,f2,sets,ns,nselect,factor)
c
	implicit none
	integer ns,nselect
	logical sets(ns)
	real f1,f2,factor
c
c------------------------------------------------------------------------
	include 'blfit.h'
	logical new
	integer i
c
	if(ns.ne.nset)call bug('f','Another inconsistency in getset')
c
	new = .false.
	dowhile(.not.new)
	  f2 = factor*f2
	  nselect = 0
	  do i=1,nscan
	    if(scanSet(i).ne.0)then
	      if(f1.le.scanF(i).and.scanF(i).le.f2)then
		if(.not.sets(scanSet(i)))then
		  sets(scanSet(i)) = .true.
		  new = .true.
		endif
		nselect = nselect + 1
	      endif
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine plotit(sets,ns,yrange,xaxis,yaxis,names)
c
	implicit none
	integer ns
	logical sets(ns)
	real yrange(2)
	character xaxis*(*),yaxis*(*),names(*)*(*)
c
c  Plot the residuals for an antenna.
c
c  Input:
c    ns    Total number of sets.
c    sets  Mask showing the sets to process.
c    yrange Plot range.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer SYM
	parameter(SYM=17)
	include 'blfit.h'
	integer i,j,iset,npnt,lx,ly,lline
	logical ok,first
	real delta
	double precision xmin,xmax,T0,phs,phmin,phmax,xv
	real xval(MAXSCAN),yval(MAXSCAN),ypsig(MAXSCAN),ymsig(MAXSCAN)
	real xlo,xhi,ylo,yhi,x,y,z
	character xlab*16,ylab*32,line*64
c
c  Externals.
c
	character itoaf*2
	integer len1
c
	if(ns.ne.nset)call bug('f','Another inconsistency in plotit')
c
c  Initialise.
c
	phmin = 0.0d0
	phmax = phmin
c
c  Determine the range of the data.
c
	xlab = xaxis
	call ucase(xlab(1:1))
	lx = len1(xlab)
	ylab = yaxis
	if(ylab.eq.'mm')ylab = 'residual'
	call ucase(ylab(1:1))
	ly = len1(ylab)
	ylab(ly+1:) = ' Phase (mm)'
	ly = len1(ylab)
c
	first = .true.
	do j=1,nscan
          iset = scanSet(j)
          ok = iset.ne.0
          if(ok) ok = sets(iset)
          if(ok)then
	    call getxval(xaxis,scanL(j),scanM(j),scanN(j),
     *			  scanDaz(j),scanT(j),scanF(j),xv)
	    if(first)then
	      xmin = xv
	      xmax = xmin
	      first = .false.
	    else
	      xmin = min(xmin,xv)
	      xmax = max(xmax,xv)
	    endif
c
	    do i=1,NANT
	      if(ScanErr(i,j).gt.0)then
		call getyval(yaxis,scanPhs(i,j),scanPred(i,j),
     *							scanF(j),phs)
		phmin = min(phmin,phs)
		phmax = max(phmax,phs)
	      endif
	    enddo
	  endif
	enddo
c
c  Fiddle the limits of the plot.
c
	delta = 0.05*(xmax-xmin)
	if(xaxis.eq.'time')then
	  T0 = int(xmin - delta - 0.5d0) + 0.5d0
	  xlo = 86400.0*(xmin-delta-T0)
	  xhi = 86400.0*(xmax+delta-T0)
	elseif(xaxis.eq.'azimuth')then
	  xlo = -190.
	  xhi =  190.
	elseif(xaxis.eq.'elevation')then
	  xlo =   0.
	  xhi = 100.
	else
	  if(delta.gt.0.01*max(xmax,-xmin))
     *			delta = 0.01*max(xmax,-xmin)
	  xlo = xmin - delta
	  xhi = xmax + delta
	endif
	if(yrange(2).gt.yrange(1))then
	  ylo = yrange(1)
	  yhi = yrange(2)
	else
	  phmax = max(-phmin,phmax)
	  delta = 0.05*phmax
	  ylo = -phmax - delta
	  yhi =  phmax + delta
	endif
c
c  Loop over the antennas and the scans to generate the plots.
c
	do i=1,NANT
	  call pgpage
	  if(xaxis.ne.'ll'.or.yaxis.ne.'mm')then
	    call pgvstd
	    call pgswin(xlo,xhi,ylo,yhi)
	    if(xaxis.eq.'time')then
	      call pgtbox('BCNSTHZO',0.,0,'BCNST',0.,0)
	    else
	      call pgbox('BCNST',0.,0,'BCNST',0.,0)
	    endif
	    line = 'Antenna '//names(i)
	    lline = len1(line)
	    call pglab(xlab(1:lx),ylab(1:ly),line(1:lline))
	    npnt = 0
	    do j=1,nscan
	      iset = scanSet(j)
	      ok = iset.ne.0.and.scanErr(i,j).gt.0
	      if(ok) ok = sets(iset)
	      if(ok)then
	        npnt = npnt + 1
	        call getxval(xaxis,scanL(j),scanM(j),scanN(j),
     *				scanDaz(j),scanT(j),scanF(j),xv)
		if(xaxis.eq.'time')then
		  xval(npnt) = 86400*(xv - T0)
		else
		  xval(npnt) = xv
		endif
		call getyval(yaxis,scanPhs(i,j),scanPred(i,j),
     *							scanF(j),phs)
	        yval(npnt) = phs
	        ypsig(npnt) = phs + scanErr(i,j)
	        ymsig(npnt) = phs - scanErr(i,j)
	      endif
	    enddo
	    if(npnt.gt.0)then
	      call pgpt(npnt,xval,yval,SYM)
	      call pgerry(npnt,xval,ypsig,ymsig,1.0)
	    endif
c
c  Do a polar plot.
c
	  else
	    call polset
	    call pglab(' ',' ',ylab(1:ly)//' on CA0'//itoaf(i))
	    do j=1,nscan
	      iset = scanSet(j)
	      ok = iset.ne.0.and.scanErr(i,j).gt.0
	      if(ok) ok = sets(iset)
	      if(ok)then
		x = scanL(j)
		y = scanM(j)
		call getyval(yaxis,scanPhs(i,j),scanPred(i,j),
     *							scanF(j),phs)
		z = 8*phs/yhi
c
c Negative is green, positive is red.
c
		if(z.lt.0)then
		  call pgsci(3)
		else
		  call pgsci(2)
		endif
		call pgsch(abs(z))
		call pgpt1(x,y,ichar('+'))
	      endif
	    enddo
c
	    call pgsci(1)
	    call pgsch(1.0)
	  endif	    
	enddo
c
	end
c************************************************************************
	subroutine getxval(xaxis,scanL,scanM,scanN,scanDaz,
     *		scanT,scanF,xv)
c
	implicit none
	character xaxis*(*)
	double precision scanL,scanM,scanN,scanT,scanF,scanDaz,xv
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	if(xaxis.eq.'time')then
	  xv = scanT
	else if(xaxis.eq.'ll')then
	  xv = scanL
	else if(xaxis.eq.'mm')then
	  xv = scanM
	else if(xaxis.eq.'nn')then
	  xv = scanN
	else if(xaxis.eq.'elevation'.or.xaxis.eq.'dec')then
	  xv = 180.0/pi * asin(scanN)
	else if(xaxis.eq.'azimuth'.or.xaxis.eq.'ha')then
	  xv = 180.0/pi * atan2(scanL,scanM)
	else if(xaxis.eq.'dazimuth')then
	  xv = 180.0/pi * scanDaz
	else if(xaxis.eq.'frequency')then
	  xv = scanF
	endif
c
	end
c************************************************************************
	subroutine getyval(yaxis,scanPhs,scanPred,scanF,yv)
c
	implicit none
	real scanPhs,scanPred
	double precision scanF,yv
	character yaxis*(*)
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision lambda
c
	lambda = 1d3*(DCMKS*1d-9)/scanF
	if(yaxis.eq.'predicted')then
	  yv = scanPred
	else
	  yv = scanPhs - scanPred
	  yv = mod(yv,lambda)
	  if(yv.gt.0.5*lambda)then
	    yv = yv - lambda
	  else if(yv.lt.-0.5*lambda)then
	    yv = yv + lambda
	  endif
	  if(yaxis.eq.'raw')yv = yv + scanPred
	  if(yaxis.eq.'nresidual'.and.scanF.gt.10)yv = -yv
	endif
c
	end
c************************************************************************
	subroutine polset
c
	implicit none
c
c------------------------------------------------------------------------
	include 'mirconst.h'
        real theta
        integer i
c
	call pgsfs(2)
	call pgwnad(-1.0,1.0,-1.0,1.0)
	call pgcirc(0.,0.,1.)
	call pgcirc(0.,0.,0.9781476)
c
        call pgsls(4)
        do i=0,360,30
          theta = i*pi/180.0
          call pgmove(cos(theta),sin(theta))
          call pgdraw(-cos(theta),-sin(theta))
        enddo
c
        do i=0,90,30 
          theta = i*pi/180.0
          call pgcirc(0.,0.,sin(theta))
        enddo   
        call pgsls(1)
	end
c************************************************************************
c************************************************************************
	subroutine scanTry(iant,iset,nosol,model,np,ok)
c
	implicit none
	integer iset,iant,np
	logical nosol,ok
	real model(np)
c
c  Guess the initial predicted phase for each scan for each antenna.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'blfit.h'
	integer MAXVAR
	parameter(MAXVAR=60)
c
	integer j
	double precision a,lambda
	real wt,theta,predict,t,ti,tr,pred
	double precision coeff(MAXVAR)
	complex offset
c
	ok = .false.
	predict = 0.
	offset = (0.,0.)
c
c  In the case where there is no solution estimate, generate the first 
c  guess just by assuming the phase does not vary too much from one
c  scan to the next.
c
	if(nosol)then
	  do j=1,nscan
	    if(iset.eq.scanSet(j).and.scanErr(iant,j).gt.0)then
	      ok = .true.
	      lambda = 1d3*(DCMKS*1d-9)/scanF(j)
	      a = scanPhs(iant,j)
	      predict = a + lambda*nint((predict-a)/lambda)
	      scanPred(iant,j) = predict
	    endif
	  enddo
c
c  Otherwise use the current solution to estimate the phase, and generate
c  an offset from this.
c
	else
	  do j=1,nscan
	    if(iset.eq.scanSet(j).and.scanErr(iant,j).gt.0)then
	      ok = .true.
	      lambda = 1d3*(DCMKS*1d-9)/scanF(j)
	      wt = scanErr(iant,j)
              call mmeqn(np,model,scanL(j),scanM(j),scanN(j),
     *                 	  scanF(j),scanf160(j),scanT(j),coeff,pred)
	      scanPred(iant,j) = pred
	      t = scanPhs(iant,j) - pred
	      theta = 2*PI*t/lambda
	      offset = offset + cmplx(cos(theta),sin(theta))/(wt*wt)
	    endif
	  enddo
c
	  ti = aimag(offset)
	  tr = real(offset)
	  if(abs(ti)+abs(tr).gt.0)then
	    predict = atan2(ti,tr)/(2.0*PI)
	  else
	    predict = 0.0
	  endif
c
	  do j=1,nscan
	    if(iset.eq.scanSet(j).and.scanErr(iant,j).gt.0)then
	      lambda = 1d3*(DCMKS*1d-9)/scanF(j)
 	      scanPred(iant,j) = scanPred(iant,j) + lambda*predict
	    endif
	  enddo
	endif
c
	end
c************************************************************************
	subroutine scanAnt(xyz,n,refant,nbadsc,nbadset)
c
	implicit none
	integer refant,nbadsc,nbadset,n
	double precision xyz(n,3)
c
c  This determines antenna-based phases for all the scans.
c
c  Input:
c    refant  Reference antenna.
c  Output:
c    nbad    Number of rejected scans.
c
c------------------------------------------------------------------------
	include 'blfit.h'
	logical setpres(MAXSET)
	real perr(NANT)
	logical ok
	integer i
c
c
	if(nant.ne.n)call bug('f','Inconsistency in scanAnt')
	do i=1,nset
	  setpres(i) = .false.
	enddo
	nbadsc = 0
	do i=1,nscan
	  call smpee(xyz,perr,nant,scanSM(i),
     *			scanL(i),scanM(i),scanN(i))
	  call Solve(NBL,NANT,scanData(1,i),scanWt(1,i),perr,
     *			refant,scanPhs(1,i),scanErr(1,i),scanF(i),ok)
	  if(.not.ok)then
	    scanSet(i) = 0
	    nbadsc = nbadsc + 1
	  else
	    setpres(scanSet(i)) = .true.
	  endif
	enddo
c
	nbadset = 0
	do i=1,nset
	  if(.not.setpres(i))nbadset = nbadset + 1
	enddo
c
	end
c************************************************************************
	subroutine mmEqn(np,model,L,M,N,F,f160,T,coeff,predict)
c
	implicit none
	integer np
	real model(np),predict
        double precision L,M,N,F,f160,T,coeff(np)
c------------------------------------------------------------------------
	if(np.ne.4)call bug('f','Inconsistency in mmEqn')
	coeff(1) = L
	coeff(2) = M
	coeff(3) = N
	coeff(4) = f160
	predict = model(1)*L + model(2)*M + model(3)*N + model(4)*f160
	end
c************************************************************************
	subroutine scanSolv(sets,ns,model,moderr,npd,np,na,nosol)
c
	implicit none
	integer na,np,ns,npd
	logical sets(ns)
	real model(np,na),moderr(np,na)
	logical nosol
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'blfit.h'
	integer MAXVAR
	parameter(MAXVAR=60)
	double precision A(MAXVAR,MAXVAR),b(MAXVAR)
	real phs(MAXVAR),predict,wt,rms,t
	double precision lambda,coeff(MAXVAR)
	integer itemp(MAXVAR)
	integer i,j,k,id,jd
	integer iset,nvar,neqn,nophs,setidx(MAXSET),iter,ierr
	logical more,ok
c
c  Externals.
c
	character itoaf*2,streal*15
c
	if(na.ne.NANT)call bug('f','Inconsistency in scanSolv')
	if(ns.ne.nset)call bug('f','Another inconsistency in scanSolv')
c
	do k=1,nant
c
c  Determine the number of offset phases that need to be solved for.
c
	  nophs = 0
	  do i=1,nset
	    if(sets(i))then
	      ok = .true.
	      if(.not.setIni(i,k))call scanTry(k,i,nosol,
     *						model(1,k),np,ok)
	      setIni(i,k) = ok
	      if(ok)then
	        nophs = nophs + 1
	        setidx(i) = nophs
	      else
		setidx(i) = 0
	      endif
	    else
	      setidx(i) = 0
	    endif
	  enddo
	  nvar = nophs + npd
	  if(nvar.gt.MAXVAR)
     *	    call bug('f','Too many variables in scanSolv')
c
c  Now iterate the basic solution.
c
	  more = .true.
	  iter = 0
	  dowhile(more)
	    iter = iter + 1
c
c  Zero the accumulation matrices.
c
	    do j=1,nvar
	      do i=1,nvar
		a(i,j) = 0.0d0
	      enddo
	      b(j) = 0.0d0
	    enddo
c
c  Accumulate the scans.
c
	    neqn = 0
	    do j=1,nscan
	      iset = scanSet(j)
	      ok = iset.ne.0
	      if(ok) ok = setidx(iset).ne.0
	      if(ok.and.scanErr(k,j).gt.0)then
		neqn = neqn + 1
	        call mmEqn(np,model(1,k),scanL(j),scanM(j),scanN(j),
     *			scanF(j),scanf160(j),scanT(j),coeff,predict)
	        lambda = 1d3*(DCMKS*1d-9)/scanF(j)
		t = scanPhs(k,j) 
		t = t + lambda*nint((scanPred(k,j)-t)/lambda)
		wt = scanErr(k,j)
		wt = 1.0/(wt*wt)
	        do i=npd+1,nvar
		  coeff(i) = 0.0d0
	        enddo
	        coeff(npd+setidx(iset)) = 1
		do jd=1,nvar
		  do id = 1,jd
		    a(id,jd) = a(id,jd) + wt*coeff(id)*coeff(jd)
		  enddo
		  b(jd) = b(jd) + wt*t*coeff(jd)
		enddo
	      endif
	    enddo
c
c  Call the solver.
c
	    do jd=1,nvar
	      do id=1,jd-1
		a(jd,id) = a(id,jd)
	      enddo
	    enddo
c
c  Check whether there are enough equations to solve.
c
	    if(neqn.gt.0)then
	      if(neqn.le.nvar)
     *		call bug('f','Underdetermined system in ScanSolv')
c
	      call dgefa(A,MAXVAR,nvar,itemp,ierr)
	      if(ierr.ne.0)
     *		call bug('f',
     *		  'Least squares fit failed in ScanSolv: ierr='//
     *		  itoaf(ierr))
              call dgesl(A,MAXVAR,nvar,itemp,b,0)
c
	      do i=1,npd
	        model(i,k) = b(i)
	      enddo
	      do i=1,nophs
	        phs(i) = b(npd+i)
	      enddo
c
c  Determine the error of the estimate.
c
	      do j=1,npd
	        do i=1,nvar
		  b(i) = 0.0d0
	        enddo
	        b(j) = 1
	        call dgesl(A,MAXVAR,nvar,itemp,b,0)
	        if(b(j).le.0)call bug('f','Ill conditioned problem?')
	        moderr(j,k) = sqrt(b(j))
	      enddo
c
	      more = .false.
	      neqn = 0
	      rms = 0.0
	      do j=1,nscan
	        iset = scanSet(j)
	        ok = iset.ne.0
	        if(ok) ok = setidx(iset).ne.0
	        if(ok.and.ScanErr(k,j).gt.0)then
	          call mmEqn(np,model(1,k),scanL(j),scanM(j),scanN(j),
     *			   scanF(j),scanf160(j),scanT(j),coeff,predict)
		  predict = predict + phs(setidx(iset))
	          lambda = 1d3*(DCMKS*1d-9)/scanF(j)
		  t = scanPhs(k,j) 
		  more = more.or.
     *		    (nint((scanPred(k,j)-t)/lambda).ne.
     *		     nint((predict-t)/lambda))
		  scanPred(k,j) = predict
		  t = t + lambda*nint((predict-t)/lambda)
		  rms = rms + ((predict-t)/ScanErr(k,j))**2
		  neqn = neqn + 1
	        endif
	      enddo
c
	      rms = sqrt(rms/(neqn-nvar))
	      call output(' Ant='//itoaf(k)//'Iter='//itoaf(iter)
     *		//'Normalise rms='//streal(rms,'(f6.2)'))
	    else
	      call output(' Ant='//itoaf(k)//'       '
     *		//'No data, no solution')
	      do i=1,npd
	        model(i,k) = 0.0
		moderr(i,k) = 0.0
	      enddo
	      more = .false.
	    endif
c
	  enddo
	enddo
	end
c************************************************************************
	subroutine scanInit
c
	implicit none
c
c------------------------------------------------------------------------
	include 'blfit.h'
	nscan = 0
	end
c************************************************************************
	integer function scanMake(set)
c
	implicit none
	integer set
c
c------------------------------------------------------------------------
	include 'blfit.h'
	integer i
c
	nscan = nscan + 1
	if(nscan.gt.MAXSCAN)call bug('f','Too many scans')
c
	scanSet(nscan) = set
	do i=1,NBL
	  scanData(i,nscan) = (0.,0.)
	  scanWt(i,nscan) = 0.
	enddo
	scanL(nscan) = 0.0d0
	scanM(nscan) = 0.0d0
	scanN(nscan) = 0.0d0
	scanT(nscan) = 0.0d0
	scanF(nscan) = 0.0d0
	scanf160(nscan) = 0.0d0
	scanSM(nscan) = 0.0d0
	scanDaz(nscan) = 0.0d0
c
	scanMake = nscan
	end
c************************************************************************
	subroutine scanAdd(iscan,t,bl,freq,f160,sigma2,az,el,daz,smon,
     *		data,omodel,np,na,domodel)

c
	implicit none
	integer iscan,np,na
	double precision t,bl,freq,f160,az,el,daz
	real sigma2,smon
	complex data
	real omodel(np,na)
	logical domodel
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'blfit.h'
	integer MNP
	parameter(MNP=10)
c
	double precision ll,mm,nn,theta,lambda,coeff(MNP),f0
	complex g
	integer i1,i2,i
	real wt,p1,p2
c
	real factor(6),edep
	data factor/1.0,0.0,0.0,0.0,0.25,0.0/
c
	if(MNP.lt.np)call bug('f','MNP too small')
	wt = 1.0/sigma2
	call basant(bl,i1,i2)
c
	i = (i2-1)*(i2-2)/2 + i1
	scanWt(i,iscan) = scanWt(i,iscan) + wt
	ll = cos(el)*sin(az)
	mm = cos(el)*cos(az)
	nn = sin(el)
	scanL(iscan) = scanL(iscan) + ll*wt
	scanM(iscan) = scanM(iscan) + mm*wt
	scanN(iscan) = scanN(iscan) + nn*wt
	scanT(iscan) = scanT(iscan) + wt*T
	scanF(iscan) = scanF(iscan) + wt*freq
	f0 = (daz/(2*pi) - 1.0/6.0)* f160/freq
	scanf160(iscan) = scanf160(iscan) + wt*f0
	scanSM(iscan) = scanSM(iscan) + wt*smon
	scanDaz(iscan) = scanDaz(iscan) + wt*daz
	call mmEqn(np,omodel(1,i1),ll,mm,nn,freq,f0,t,coeff,p1)
	call mmEqn(np,omodel(1,i2),ll,mm,nn,freq,f0,t,coeff,p2)
	if(domodel)then
	  edep = 7.4563 - el*(15.2243 - el*7.2267)
	  p1 = p1 +factor(i1)*edep
	  p2 = p2 + factor(i2)*edep
	endif
	lambda = 1d3*(DCMKS*1d-9)/freq
	theta = 2*DPI*(p1 - p2)/lambda
	g = cmplx(cos(theta),-sin(theta))
	scanData(i,iscan) = scanData(i,iscan) + wt*g*data
c	
	end
c************************************************************************
	subroutine scanDup(gap,tbase,flo,fhi)
c
	implicit none
	real gap,flo,fhi
	double precision tbase
c------------------------------------------------------------------------
	include 'blfit.h'
c
	integer i,iscan
	real wt
c
c  Externals.
c
	integer setTime
c
	do iscan=1,nscan
	  wt = 0.0
	  do i=1,NBL
	    wt = wt + scanWt(i,iscan)
	  enddo
	  scanL(iscan) = scanL(iscan) / Wt
	  scanM(iscan) = scanM(iscan) / Wt
	  scanN(iscan) = scanN(iscan) / Wt
	  scanT(iscan) = scanT(iscan) / Wt + Tbase
	  scanF(iscan) = scanF(iscan) / Wt
	  scanf160(iscan) = scanf160(iscan) / Wt
	  scanSM(iscan) = scanSM(iscan) / Wt
	  scanDaz(iscan) = scanDaz(iscan) / Wt
	  i = setTime(scanSet(iscan),scanT(iscan),gap)
	  scanSet(iscan) = i
	  if(iscan.eq.1)then
	    flo = scanF(iscan)
	    fhi = flo
	  else
	    flo = min(flo,real(scanF(iscan)))
	    fhi = max(fhi,real(scanF(iscan)))
	  endif
	enddo
c
	end
c************************************************************************
c************************************************************************
	subroutine setGet(tno,nchan,set)
c
	implicit none
	integer tno,nchan,set(nchan)
c
c  Determine the chan/spect parameters for a particular
c  set of correlations that have just been read.
c
c  Input:
c    tno
c    nchan
c    maxspect
c  Input/Output:
c    nspect
c    sfreq
c    sdf
c    poln
c  Output:
c    set
c------------------------------------------------------------------------
	include 'blfit.h'
	include 'maxdim.h'
	integer CHANNEL
	parameter(CHANNEL=1)
	integer i,j,n,ispect,ltype,start,nschan0(MAXWIN),nspect0
	integer chans,nwidth,nstep,pol
	double precision line(6),sfreq0(MAXWIN),sdf0(MAXWIN),f,df
	integer iset,bm
c
c  Determine what the current frequency setup is.
c
	call uvdatgti('number',bm)
	call uvdatgti('pol',pol)
	call uvinfo(tno,'line',line)
	if(nint(line(2)).ne.nchan)
     *	    call bug('f','Number of channels disagree')
	nstep  = nint(line(5))
	nwidth = nint(line(4))
	if(nstep.ne.1.and.nwidth.eq.1)
     *	  call bug('f','step and width parameters not supported')
	ltype = nint(line(1))
	if(ltype.ne.CHANNEL)
     *	    call bug('f','Only channel linetype supported')
	start = nint(line(3))
c
	call uvrdvri(tno,'nspect',nspect0,0)
	if(nspect0.le.0.or.nspect0.gt.MAXWIN)
     *	      call bug('f','Bad value for nspect, in DESPECT')
	call uvgetvrd(tno,'sfreq',sfreq0,nspect0)
	call uvgetvrd(tno,'sdf',sdf0,nspect0)
	call uvgetvri(tno,'nschan',nschan0,nspect0)
c
c  Go through the windows that we have. Match this with any
c  previous windows of the same sort.
c
	j = 0
	ispect = 1
	n = nchan
	dowhile(n.gt.0)
	  dowhile(start.gt.nschan0(ispect))
	    start = start - nschan0(ispect)
	    ispect = ispect + 1
	  enddo
	  chans = min(n,nschan0(ispect) - start + 1)
	  f = sfreq0(ispect) + sdf0(ispect) *(start - 1)
	  df = sdf0(ispect)
c
	  call getfam(f,df,pol,bm,MAXSET,nset,setSfreq,setSdf,
     *						setPol,setBm,iset)
c
c  Now remember which solution family this belongs to.
c
	  do i=1,chans
	    j = j + 1
	    set(j) = iset
	  enddo
	  n = n - chans
	  start = start + chans
	enddo
c
	end
c************************************************************************
	subroutine setInit
c
	implicit none
c
c------------------------------------------------------------------------
	include 'blfit.h'
	integer i,k
	do k=1,NANT
	  do i=1,MAXSET
	    setIni(i,k) = .false.
	  enddo
	enddo
c
	do i=1,MAXSET
	  setTmin(i) = 0.0d0
	  setTmax(i) = 0.0d0
	  setAset(i) = i
	enddo
	nset = 0
	end
c************************************************************************
	integer function setTime(iset,T,gap)
c
	implicit none
	integer iset
	real gap
	double precision T
c------------------------------------------------------------------------	
	include 'blfit.h'
	integer i
	if(setTmin(iset).le.0)then
	  setTmin(iset) = T
	  setTmax(iset) = T
	  setAset(iset) = iset
	endif
c
	i = iset
	dowhile(setAset(i).ne.i.and.
     *	        (T.gt.setTmin(i)+gap.or.T.lt.setTmax(i)-gap))
	  i = setAset(i)
	enddo
	if(T.gt.setTmin(i)+gap.or.T.lt.setTmax(i)-gap)then
	  nset = nset + 1
	  if(nset.gt.MAXSET)
     *	    call bug('f','Too many sets')
	  setAset(i) = nset
	  i = nset
	  setAset(i) = i
	  setTmin(i) = T
	  setTmax(i) = T
	  setSfreq(i) = setSfreq(iset)
	  setSdf(i) = setSdf(iset)
	  setPol(i) = setPol(iset)
	endif
	setTime = i
	end
c************************************************************************
	subroutine getfam(f,df,pol,bm,maxspect,nspect,
     *					sfreq,sdf,poln,bms,k)
c
	implicit none
	integer maxspect,nspect,k,pol,bm
	double precision f,df,sfreq(maxspect),sdf(maxspect)
	integer poln(maxspect),bms(maxspect)
c
c  Match up a spectral window parameters.
c
c------------------------------------------------------------------------
	integer ispect
	logical more
c
	ispect=1
	more = nspect.gt.0
	dowhile(more)
	  if(abs(f-sfreq(ispect)).lt.0.5*abs(sdf(ispect)).and.
     *       abs(df-sdf(ispect)).lt.0.1*abs(sdf(ispect)).and.
     *	     pol.eq.poln(ispect).and.bm.eq.bms(ispect))then
            more = .false.
          else
            ispect = ispect + 1
            more = ispect.le.nspect
          endif
        enddo
c
	if(ispect.gt.nspect)then
	  nspect = nspect + 1
	  if(nspect.gt.maxspect)
     *	    call bug('f','Too many spectral windows')
	  sdf(nspect) = df
	  sfreq(nspect) = f
	  poln(nspect) = pol
	  bms(nspect) = bm
	  ispect = nspect
	endif
c
	k = ispect
	end
c************************************************************************
	subroutine Solve(nbl,nant,data,wts,perr,refant,phs,err,freq,ok)
c
	implicit none
	integer nbl,nant,refant
	complex data(nbl)
	real phs(nant),err(nant),perr(nant)
	double precision freq,wts(nbl)
	logical ok
c
c  Given visibility data, determine antenna-based gains solutions.
c
c  Input:
c    nbl,nant	Number of baselines and antennas.
c    data	Weighted data.
c    wts	Weights.
c    refant	Reference antenna.
c    perr	Estimated atmospheric phase error, in mm.
c  Output:
c    gains	Complex antenna gains
c    err	Estimate of error of the gains.
c    ok		Success or failure of this routine.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
c
	integer b1((MAXANT*(MAXANT-1))/2),b2((MAXANT*(MAXANT-1))/2)
	integer i,j,k,nbld,nantd
	integer indx(MAXANT)
	complex d((MAXANT*(MAXANT-1))/2),g(MAXANT),t,temp
	real e((MAXANT*(MAXANT-1))/2),errref,x
	double precision lambda
c
	if(nant.gt.MAXANT.or.nbl.ne.(nant*(nant-1))/2)
     *	  call bug('f','Inconsistency in Solve')	
c
	do i=1,nant
	  indx(i) = 0
	enddo
c
	nantd = 0
	nbld = 0
	k = 0
	do j=2,nant
	  do i=1,j-1
	    k = k + 1
	    if(wts(k).gt.0)then
	      nbld = nbld + 1
	      d(nbld) = data(k)
	      if(indx(i).eq.0)then
		nantd = nantd + 1
		indx(i) = nantd
	      endif
	      if(indx(j).eq.0)then
		nantd = nantd + 1
		indx(j) = nantd
	      endif
	      b1(nbld) = indx(i)
	      b2(nbld) = indx(j)
	    endif
	  enddo
	enddo
c
c  Now solve.
c
	ok = indx(refant).gt.0.and.nbld.ge.3
	if(ok)call solve2(nbld,nantd,d,b1,b2,g,e)
c
c Unpack and convert.
c
	if(ok)then
	  lambda = 1d3*(DCMKS*1d-9)/freq
	  temp = 1.0/g(indx(refant))
	  errref = e(indx(refant))/(2*DPI)*lambda
	  do k=1,nant
	    if(indx(k).eq.0)then
	      phs(k) = 0.
	      err(k) = 0.
	    else
	      t = temp*g(indx(k))
	      phs(k) = atan2(aimag(t),real(t))/(2*DPI)*lambda
	      x = (e(indx(k))/(2*DPI)*lambda)**2

	      if(k.ne.refant)x = x + errref*errref
	      x = x + perr(k)*perr(k)
	      err(k) = sqrt(x)
	    endif
	  enddo
	endif
	    
	end
c************************************************************************
	subroutine solve2(nbl,nant,d,b1,b2,G,err)
c
	implicit none
	integer nbl,nant,b1(nbl),b2(nbl)
	complex d(nbl),G(nant)
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXITER,MAXTRY
	real TOL
	parameter(MAXITER=100,MAXTRY=1000,TOL=1e-10)
c
	integer i,j,niter
	real t,Change
	complex Sum(MAXANT),Temp
	integer Acc(MAXANT),Count(MAXANT),n(MAXANT)
	real err(MAXANT)
	logical ok
c
c  Externals.
c
	integer ranno
c
c  Initialise.
c
	do i=1,nant
	  G(i) = (1.,0.)
	  Sum(i) = (0.,0.)
	enddo
c
	ok = .false.
	niter = 0
	dowhile(.not.ok.and.niter.lt.maxiter)
	  niter = niter + 1
c
c  Sum the contributions over the baselines. Note that the following
c  loop contains a dependency (it should not vectorise).
c
	  do i=1,nbl
	    Sum(b1(i))  = Sum(b1(i)) + G(b2(i)) * d(i)
	    Sum(b2(i))  = Sum(b2(i)) + G(b1(i)) * conjg(d(i))
	  enddo
c
c  Update the gains.
c
	  Change = 0.0
c
	  do i=1,nant
	    Temp = Sum(i)/abs(Sum(i))
	    Temp = 0.5*(G(i) + Temp)
	    Temp = Temp / abs(Temp)
	    Change = Change +  real(G(i)-Temp)**2 +
     *			      aimag(G(i)-Temp)**2
	    G(i) = Temp
	    Sum(i) = 0.0
	  enddo
	  ok = Change.lt.tol*nant
	enddo
c
c  Work out an error estimate.
c
	do i=1,nant
	  Count(i) = 0
	  Acc(i) = 0
	  N(i) = 0
	  err(i) = 0.0
	enddo
	do i=1,nbl
	  Count(b1(i)) = Count(b1(i)) + 1
	  Count(b2(i)) = Count(b2(i)) + 1
	enddo
c
	do i=1,MAXTRY
	  j = ranno(nbl)
	  Temp = conjg(G(b1(j)))*G(b2(j))*d(j)
	  Sum(b1(j)) = Sum(b1(j)) + Temp
	  Acc(b1(j)) = Acc(b1(j)) + 1
	  Sum(b2(j)) = Sum(b2(j)) + conjg(Temp)
	  Acc(b2(j)) = Acc(b2(j)) + 1
c
	  if(Acc(b1(j)).eq.Count(b1(j)))then
	    Temp = Sum(b1(j))
	    t = aimag(Temp)/real(Temp)
	    err(b1(j)) = err(b1(j)) + t*t
	    n(b1(j)) = n(b1(j)) + 1
	    Sum(b1(j)) = (0.,0.)
	    Acc(b1(j)) = 0
	  endif
c
	  if(Acc(b2(j)).eq.Count(b2(j)))then
	    Temp = Sum(b2(j))
	    t = aimag(Temp)/real(Temp)
	    err(b2(j)) = err(b2(j)) + t*t
	    n(b2(j)) = n(b2(j)) + 1
	    Sum(b2(j)) = (0.,0.)
	    Acc(b2(j)) = 0
	  endif
	enddo
c 
	do i=1,nant
c	  if(n(i).eq.0)call bug('f','No acc in solve')
	  if(n(i).le.0)then
	    err(i) = 0.0
	  else
	    err(i) = 1.5*sqrt(err(i)/n(i))
	  endif
	enddo
c
	end
c************************************************************************
	integer function ranno(n)
c
	implicit none
	integer n
c
c  Return a "random" number in the range 1 to n.
c
c------------------------------------------------------------------------
	integer NUNI
	parameter(NUNI=1000)
	real uni(NUNI)
	integer iuni
	save uni,iuni
	data iuni/0/
c
	if(iuni.eq.0)then
	  call uniform(uni,NUNI)
	  iuni = NUNI
	endif
	ranno = int(n*uni(iuni)) + 1
	if(ranno.gt.n)ranno = n
	iuni = iuni - 1
	end
c************************************************************************
	subroutine AntLd(tno,lat,xyz,nant,refant)
c
	implicit none
	integer tno,nant,refant
	double precision lat
	double precision xyz(nant,3)
c------------------------------------------------------------------------
	include 'mirconst.h'
	real xd,yd,zd,cosl,sinl,fac,x,y,z,x0,y0,z0
	integer i
c
c  Get the antenna coordinates.
c
	call uvgetvrd(tno,'antpos',xyz,3*nant)
c
c  Convert the antenna coordinates to local x,y,z
c
	fac = CMKS*1e-9
	cosl = cos(lat)
	sinl = sin(lat)
	x0 = xyz(refant,1)
	y0 = xyz(refant,2)
	z0 = xyz(refant,3)
	do i=1,NANT
	  x = xyz(i,1) - x0
	  y = xyz(i,2) - y0
	  z = xyz(i,3) - z0
	  xd = fac*y
	  yd = fac*(-x*sinl + z*cosl)
	  zd = fac*( x*cosl + z*sinl)
	  xyz(i,1) = xd
	  xyz(i,2) = yd
	  xyz(i,3) = zd
	enddo
c
	end
c************************************************************************
	subroutine smpee(xyz,perr,nant,smon,ll,mm,nn)
c
	implicit none
	integer nant
	double precision xyz(nant,3),ll,mm,nn
	real perr(nant),smon
c
c  Estimate the RMS phase error for each antenna phase solution.
c
c------------------------------------------------------------------------
	integer i
	real d
c
	do i=1,nant
	  d = xyz(i,1)*xyz(i,1) + xyz(i,2)*xyz(i,2) + xyz(i,3)*xyz(i,3)
     *      - ( xyz(i,1)*ll + xyz(i,2)*mm + xyz(i,3)*nn )**2
	  d = sqrt(d)
	  perr(i) = sqrt(3.0)/nn * (d/230)**0.6 * 1e-3 * smon
	enddo
c
	end
c************************************************************************
      subroutine getlst (lin, time, long, lst)
c
      implicit none
      integer lin
      double precision time, lst, long
c
c  Get lst of the current data point.
c
c  Input:
c    lin         Handle of file
c  Output:
c    lst         LAST in radians
c-----------------------------------------------------------------------
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
        call jullst (time, long, lst)
	lst = lst + eqeq(time)
      else
         call uvrdvrd (lin, 'lst', lst, 0.0d0)
      end if
c
      end
c************************************************************************
	subroutine getnames(lIn,names,nants,telescop)
c
	implicit none
	integer lIn,nants
	character names(nants)*(*),telescop*(*)
c------------------------------------------------------------------------
	integer i,ant,l1,l2,length
	logical updated
	character buf*1024,dtype*1
c
	call uvprobvr(lIn,'antname',dtype,length,updated)
	if(dtype.ne.'a'.or.length.ge.len(buf))length = 0
	if(length.gt.0)call uvgetvra(lIn,'antname',buf(1:length+1))
	ant = 0
	l1 = 1
	do l2=1,length
	  if(buf(l2:l2).eq.';')then
	    ant = ant + 1
	    if(l2.gt.l1)then
	      names(ant) = buf(l1:l2-1)
	    else
	      call defant(names(ant),ant,telescop)
	    endif
	    l1 = l2 + 1
	  endif
	enddo
	if(l1.le.length)then
	  ant = ant + 1
	  names(ant) = buf(l1:length)
	endif
c
	do i=ant+1,nants
	  call defant(names(i),i,telescop)
	enddo
c
	end
c************************************************************************
	subroutine defant(name,ant,telescop)
c
	implicit none
	character name*(*),telescop*(*)
	integer ant
c
c  Externals.
c
	character itoaf*8
c
	if(telescop.eq.'atca')then
	  name = 'ca0'//itoaf(ant)
	else
	  name = 'Ant'//itoaf(ant)
	endif
c
	end
