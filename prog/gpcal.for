c************************************************************************
	program gpcal
	implicit none
c
c= Gpcal -- Gain/phase/polarization calibration of of linear feed data.
c& rjs nebk
c: calibration
c+
c	Gpcal is a MIRIAD task which determines calibration corrections
c	(both antenna gains and instrumental polarisation characteristics)
c	for an array with dual linear feeds, from an observation of a point
c	source. The source can be polarised, with unknown polarisation
c	characteristics. Though the source may be strongly polarized, the
c	instrumental polarisation errors are assumed to be small (of order
c	at most a few percent).
c
c	Normally GPCAL writes the solutions as a gains table (item `gains')
c	and a polarization leakage table (item `leakage').
c
c	Note that the user specifies which parameters are to be solved
c	for. In the case of leakages and xyphases, GPCAL will check for
c	the existence of items ``leakage'' and ``gains'' in the input
c	data-set. If present, these will be used as the initial estimates
c	of these parameters. If you are not solving for these parameters,
c	they will be held at there initial value through the solution
c	process. After converging on a solution, and if the xyphase offsets
c	or leakage parameters have been modified, GPCAL will write out
c	their current values to the appropriate items.
c
c	Conventions: Unfortunately there has been a number of changes in
c	the `sign conventions' used within Miriad. For a discussion of the
c	conventions, past sign errors and how they affect you, see the
c	memo ``The Sign of Stokes-V, etc'' by Bob Sault.
c
c@ vis
c	Input visibility data file. The data must contain raw linear
c	polarisations. No default. The visibility data must be in time
c	order.
c@ select
c	Standard uv selection. Default is all data.
c@ line
c	Standard line-type specification. Multiple channels can be given.
c	Generally it is better to give multiple channels, rather than
c	averaging them into a `channel-0'. The default is all the channel
c	data (or all the wide data, if there is no channel data).
c@ flux
c	The values of the I,Q,U,V Stokes parameters. If no values are
c	given, and it is a source known to GPCAL, GPCAL uses its known
c	flux as the default. If GPCAL does not know the source, the
c	lux is determined by assuming that the rms gain amplitude is 1.
c	If the option `qusolve' is used, the given fluxes for Q and U are
c	used as the initial estimates. Also see the `oldflux' option.
c@ refant
c	The reference antenna. Default is 3. The reference antenna needs to be
c	present throughout the observation. Any solution intervals where the
c	reference antenna is missing are discarded.
c@ minants
c	The minimum number of antenna that must be present before a
c	solution is attempted. Default is 2.
c@ interval
c	This gives one or two numbers, both given in minutes, both being used
c	to determine the extents of an amplitude calibration solution interval.
c	The first gives	the max length of a solution interval. The second
c	gives the max gap size in a solution interval. A new solution interval
c	is started when	either the max time length is exceeded, or a gap
c	larger than the	max gap is encountered. The default is max length
c	is 5 minutes, and the max gap size is the same as the max length.
c	The polarisation characteristics are assumed to be
c	constant over the observation.
c@ tol
c	Error tolerance. Default 0.001.
c@ xyphase
c	Initial estimate of the XY phase of each antenna. The default is
c	0 for all antennas. If the XY phase has not been applied to the
c	data, then it is important that this parameter is set correctly,
c	particularly for the reference antenna.
c@ options
c	These options determine what GPCAL solves for. There are many
c	permutations, the more obscure or useless of which are not
c	supported. The option values are used to turn on or off some
c	of the solvers. Several options	can be given, separated by
c	commas. Minimum match is used.
c	  "nopass"     Do not apply bandpass correction. The default is
c	               to apply bandpass correction if possible.
c	  "noamphase"  Do not solve for the amplitude and phase. The 
c	               default is to solve for amplitude and phase.
c	  "nopol"      Do not solve for the instrumental polarisation
c	               leakage characteristics. The default is to solve
c	               for the polarisation leakages on all feeds except
c	               the X feed of the reference antenna.
c	  "noxy"       Do not solve for any XY phase offset. The default is to
c	               solve for the XY phase offset on all antennas
c	               except for the reference antenna.
c	  "xyvary"     The XY phase varies with time. By default the XY phase
c	               is assumed to remain constant.
c	  "qusolve"    Solve for q and u fluxes. You require good parallactic
c	               angle coverage for this.
c	  "xyref"      Solve for the XY phase of the reference antenna. To
c	               do this, the source must be strongly polarized. This
c	               option can be used with ``noxy'', in which case GPCAL
c	               solves for the offset of the reference antenna. To
c		       use this option, the source should be at least 5%
c	               polarised.
c	  "polref"     Solve for the instrumental polarization leakage
c	               of the X feed on the reference antenna. The source
c	               must be at least 5% polarised. This can
c	               be combined with ``nopol'', in which case GPCAL
c	               solves for X feed of the reference antenna only.
c	   "oldflux"   This causes GPCAL to use a pre-August 1994 ATCA flux
c	               density scale. See the help on `oldflux' for more
c	               information.
c	Some combinations of these options are not supported.
c--
c  History:
c    rjs,nebk 1may91 Original version.
c    rjs     17may91 A working version.
c    nebk    25may91 Add some plots while rjs is away
c    nebk    02jun91 Fix convergence when gain only solution, and change
c                    to output all polarizations, even if no leakage
c                    solved for.  Add log file, remove history from input.
c            03jun91 Change default output file to corrected data.
c                    Add option "NOLEAKCORR"
c    rjs     17jun91 Changed sign conventions.
c    nebk    17jun91 Add Q and U plots with iteration
c    rjs     18jun91 Fudged the equations for Vxx and Vyy when solving
c		     for unknown q and u.
c    rjs     20jun91 Replaced the "strong" option with xyref and polref
c		     options. Minor other changes.
c    rjs     21jun91 Name change and more hacks.
c    rjs     28jun91 Writes gain table correctly.
c    rjs     17jul91 Send Neil home in tears after I deleted his plotting
c		     software from this otherwise excellent program.
c                    One should note this capability now in GPPLT, sob.
c    rjs     24jul91 Added RefSolve routines, read xyphase and leakage 
c                    tables, use these as the initial guess.
c    rjs     29jul91 Can use options=qusolve,polref together.
c    nebk    05aug91 Implement routines with knowledge about IQUV for
c                    some selected calibrator sources.
c    nebk    13aug91 Put info. back into input history when no output file
c    nebk    14aug91 Removed Option=NOLEAKCORR since the use of gain
c                    and leakage tables now makes it redundant
c    nebk    26aug91 Changed polynomials in CALSTOKE for 3C286 & 3C138
c    rjs      1sep91 Elimiated "out" options. Fiddled determining the end
c		     of scans and the interval parameter.
c    rjs      2sep91 Fixed formatting bugs in CalStoke.
c    nebk    03sep91 Use Bob Duncan's polynomial fit for 1934 rather than
c                    mine for consistency with SetJy in AIPS (although
c                    mine fits better I think).  Other fiddles in CALSTOKE
c    nebk    17jan92 Add output from GETIQUV to history
c    rjs     28apr92 Fixed bug in initial estimate of amplitude solutions.
c		     Bug will not effect any results.
c    rjs      4may92 Minor improvement to amplitude solution routine.
c    rjs     23jun92 XYvary option. Estimate XY phase.
c    rjs     22jul92 Get source name earlier.
c    rjs      4aug92 Eliminate xyphases item. Remove calstoke to a separate
c		     file, and change its call sequence.
c    rjs     28aug92 Protected sqrt() from rounding error which made it
c		     go negative.
c    rjs     30oct92 Change minant to minants.
c    rjs     22nov92 Added ntau keyword to output gains file.
c    rjs     16feb93 More decimal places in a message.
c    rjs      9mar93 Qusolve when not solving for leakages.
c    nebk    07jun93 Minor formatting change
c    rjs     24jun93 Hasn't gone anywhere.
c    rjs     28jul93 Fiddle gain scaling if no flux is given.
c    rjs     10aug93 Applies bandpass corrections. Handles multi-channel
c		     data.
c    rjs     18oct93 Doc correction only.
c    rjs     13dec93 Sign convention of V change.
c    rjs     23mar94 Check for existence of closures, and do the appropriate
c		     things if there aren't any.
c    rjs     25mar94 Minor correction to the above change.
c    rjs      3aug94 options=oldflux.
c
c  Bugs:
c    * Polarisation solutions when using noamp are wrong! The equations it
c      solves for have a fudge to account for a bias introduced by the
c      amplitude solver.
c    * Need a constant X/Y gain option.
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer MAXITER
	character version*(*)
	parameter(MAXITER=30)
	parameter(version='Gpcal: version 1.0 3-Aug-94')
c
	integer tIn
	double precision interval(2), freq
	real flux(4),OldFlux(4),xyphase(MAXANT)
	real pcent,epsi,epsi1,tol,ttol
	integer refant,minant,nants,nbl,nxyphase,nsoln,niter,i,j,jmax
	integer maxsoln
	logical amphsol,polsol,xysol,xyvary,xyref,polref,qusolve,defflux
	logical usepol,useref,dopass,polcal,notrip,oldflx
	character line*80,source*32,uvflags*8
c
	complex D(2,MAXANT),xyp(MAXANT),Gains(2,MAXDANTS)
 	complex OldD(2,MAXANT),OldGains(2,MAXDANTS)
	complex Vis(4,MAXDBL)
	complex VisCos(4,MAXDBL),VisSin(4,MAXDBL)
	real SumS(MAXDBL),SumC(MAXDBL)
	real SumS2(MAXDBL),SumCS(MAXDBL)
	integer Count(MAXDBL)
	double precision time(MAXDSOLN)
	logical present(MAXANT)
c
c  Externals.
c
	logical uvDatOpn,keyprsnt
	character itoaf*3
c
c  Get inputs.
c
	call output(version)
	call keyini
	call GetOpt(dopass,amphsol,
     *		polsol,xysol,xyref,xyvary,polref,qusolve,oldflx)
	uvflags = 'dlb'
	if(dopass) uvflags = 'dlbf'
	call uvDatInp('vis',uvflags)
	defflux = .not.keyprsnt('flux')
	call keyr('flux',flux(1),1.)
	call keyr('flux',flux(2),0.)
	call keyr('flux',flux(3),0.)
	call keyr('flux',flux(4),0.)
	call keyi('refant',refant,3)
	call keyi('minants',minant,2)
	call mkeyr('xyphase',xyphase,MAXANT,nxyphase)
	call keyd('interval',interval(1),5.d0)
	call keyd('interval',interval(2),interval(1))
	call keyr('tol',tol,0.001)
	call keyfin
c
c  Check the input parameters.
c
	if(refant.le.0) call bug('f','Invalid reference antenna number')
	if(minant.le.1) call bug('f','Bad number of mininum antennae')
	if(amphsol)then
	  if(min(interval(1),interval(2)).le.0)
     *	    call bug('f','Negative value for interval')
	  interval(1) = interval(1) / (24*60)
	  interval(2) = interval(2) / (24*60)
	else
	  interval(1) = 1
	  interval(2) = 1
	endif
c
c  Determine the polarisation solver to use.
c
	polcal = abs(flux(2))+abs(flux(3))+abs(flux(4)).gt.0
	usepol = (xysol.and..not.xyvary).or.polsol.or.qusolve
	useref = polref.and..not.polsol
	if(usepol.and.useref)
     *	  call bug('f','Unsupported combination of options')
c
c  Indicate that we only want to get back raw linear polarisations.
c
	call uvDatSet('stokes',-5)
	call uvDatSet('stokes',-6)
	if(usepol.or.useref.or.xyref.or.polcal)then
	  call uvDatSet('stokes',-7)
	  call uvDatSet('stokes',-8)
	endif
c
c  Open the uv files.
c
	if(.not.uvDatOpn(tIn)) call bug('f','Error opening input file')
        call HisOpen(tIn,'append')
        call HisWrite(tIn,'GPCAL: '//version)
	call HisInput(tIn,'GPCAL')
c 
c  Determine the number of antennae.
c
	call uvnext(tIn)
	call uvrdvri(tIn,'nants',nants,0)
	if(nants.le.0)call bug('f',
     *	  'Unable to determine the number of antennae')
	call output('Number of antennae: '//itoaf(nants))
	if(nants.gt.MAXANT)
     *	  call bug('f','Too many antennae for me to handle')
	if(refant.gt.nants)
     *	  call bug('f','Bad reference antenna number')
	if(minant.gt.nants)
     *	  call bug('f','Number of antennae is less than minimum')
	nbl = (nants*(nants-1))/2
	maxsoln = min(MAXDANTS/nants,MAXDBL/nbl,MAXDSOLN) - 1
c
c  Set the initial estimates of the polarisation characteristics.
c
	if(nxyphase.gt.nants)call bug('w',
     *	  'More XY phases were given than there are antennae')
	nxyphase = min(nxyphase,nants)
	call PolIni(tIn,xyphase,nxyphase,D,xyp,nants)
c
c  Read the data, forming the sums that we need.
c
	call output('Reading the data ...')
	call DatRead(tIn,useref.or.usepol.or.xyref.or.polcal,
     *	  nants,nbl,maxsoln,
     *	  interval,refant,minant,time,nsoln,present,notrip,
     *	  Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,
     *	  source, freq)
c
c  Initialize the flux array
c
      if(defflux)call getiquv(source,freq,oldflx,flux,defflux)
c
      if(defflux.and..not.qusolve)call bug('w',
     *	  'It is unwise to omit OPTION=QUSOLVE when flux is unknown')
      if(flux(1).le.0)call bug('f','Invalid total flux value')
      pcent = sqrt( flux(2)**2 + flux(3)**2 + flux(4)**2)/flux(1)
      if(polref.and.pcent.eq.0.and..not.qusolve) call bug('f',
     *	  'You must give values for Q,U,V to use option POLREF')
      if(polref.and.pcent.lt.0.05) call bug('w',
     *	  'Source appears only weakly polarised')
c
c  Initialise the gains.
c
	call GainIni(nants,nsoln,Gains,xyp)
c
c  Determine which polarisation solver to use.
c
c
c  Iterate until we have converged.
c
	niter = 0
	epsi = tol + 1
	if(usepol.or.useref)then
	  ttol = 0.01
	else
	  ttol = tol
	endif
c
	dowhile(epsi.gt.tol.and.niter.lt.MAXITER)
	  niter = niter + 1
	  epsi = 0
	  call CopyG(nants,nsoln,D,Gains,Flux,OldD,OldGains,OldFlux)
	  ttol = max(0.3*ttol,0.5*tol)
c
c  Solve for the antenna gains.
c
	  if(amphsol)then
	    if(niter.eq.1.and.xysol.and..not.xyvary)then
	      call output('Estimating the XY phases ...')
	      call AmpSolve(nsoln,nants,nbl,flux,refant,
     *	        xyref,.true.,
     *	        D,xyp,Gains,niter.le.1,ttol,epsi1,
     *	        Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count)
	      call XYFudge(nsoln,nants,xyp,Gains)
	    else
	      call AmpSolve(nsoln,nants,nbl,flux,refant,
     *		xyref.and.xyvary,xyvary,
     *		D,xyp,Gains,niter.le.1,ttol,epsi1,
     *		Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count)
	    endif
	    write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *		', Amplit/Phase Solution Error: ',epsi1
	    call output(line)
	    epsi = max(epsi,epsi1)
	  endif
c
c  Solve for all the polarisation characteristics.
c
	  if(usepol)then
	    call PolSolve(nsoln,nants,nbl,flux,refant,polsol,polref,
     *	      qusolve,xysol.and..not.xyvary,xyref.and..not.xyvary,
     *	      notrip,D,Gains,xyp,epsi1,present,Vis,VisCos,VisSin,
     *	      SumS,SumC,SumS2,SumCS,Count)
	    write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *		', Polarisation Solution Error: ',epsi1
	    call output(line)
	    epsi = max(epsi,epsi1)
	  else if(useref)then
	    call RefSolve(nsoln,nants,nbl,flux,xyref.and..not.xyvary,D,
     *	      Gains,xyp,epsi1,Vis,VisCos,VisSin,
     *	      SumS,SumC,SumS2,SumCS,Count)
	    write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *		', Reference Solution Error:    ',epsi1
	    call output(line)
	    epsi = max(epsi,epsi1)
	  endif
c
	  if((usepol.or.useref).and.amphsol)then
	    call CompareG(nants,nsoln,D,Gains,Flux,
     *		OldD,OldGains,OldFlux,epsi)
	    write(line,'(a,i2,a,f7.3)')'Iter=',niter,
     *		', Overall Solution Error:      ',epsi
	    call output(line)
	  else if(amphsol)then
	    epsi = 0
	  endif
	enddo
	if(niter.ge.MAXITER)
     *	  call bug('w','Failed to converge ... saving solution anyway')
c
c  If no flux was given, scale the gains so that they have an rms of
c  1. Determine what flux this implies.
c
	if(defflux)call GainScal(flux,Gains,2*nants*nsoln)
c
c  Tell about the source polarisation.
c
	if(qusolve.or.defflux)then
          write(line,'(a,f8.3)')'I flux density: ', flux(1)
          call writeo(tIn,line)
c
	  write(line,'(a,f8.3)')'Percent Q: ',100*flux(2)/flux(1)
	  call writeo(tIn,line)
c
	  write(line,'(a,f8.3)')'Percent U: ',100*flux(3)/flux(1)
	  call writeo(tIn,line)
	endif
c
c  Tell about the XY phases.
c
	if((xysol.or.xyref.or.nxyphase.gt.0).and..not.xyvary)then
	  call writeo(tIn,'XY phases (degrees)')
	  do j=1,nants,7
	    jmax = min(j+7,nants)
	    do i=j,jmax
	      xyphase(i) = atan2(aimag(xyp(i)),real(xyp(i)))
	    enddo
	    write(line,'(1x,a,i2,a,i2,a,7f7.1)')'Xyphase(',j,'-',jmax,
     *	      ') = ',(180/pi*xyphase(i),i=j,jmax)
	    call writeo(tIn,line)
          enddo
	endif
c
c  Write out the gain table.
c
	if(amphsol)
     *	  call GainTab(tIn,time,Gains,nants,nsoln)
c
c  Tell about the leakage parameters, and save them.
c
	if(polsol.or.polref)then
	  call writeo(tIn,'Leakage terms:')
	  do j=1,nants
	    write(line,'(1x,a,i2,a,f6.3,a,f6.3,a,f6.3,a,f6.3,a)')
     *	      'Ant',j,':Dx,Dy = (',real(D(1,j)),',',aimag(D(1,j)),'),(',
     *				   real(D(2,j)),',',aimag(D(2,j)),')'
	    call writeo(tIn,line)
	  enddo
	  call LeakTab(tIn,D,nants)
	endif
c
c  All said and done.
c
        call HisClose(tIn)
	call uvDatCls()
c
	end
c************************************************************************
	subroutine XYFudge(nsoln,nants,xyp,Gains)
c
	implicit none
	integer nsoln,nants
	complex Gains(2,nants,nsoln),xyp(nants)
c
c  Determine the initial estimates for the XY phase.
c
c  Input:
c    nsoln
c    nants
c  Input/Output:
c    Gains
c  Output:
c    xyp
c------------------------------------------------------------------------
	include 'gpcal.h'
	real phase(MAXANT),phase2(MAXANT),theta,t
	complex w
	integer i,j,count(MAXANT),sd
	character line*80
c
c  Initialise the counters.
c
	do i=1,nants
	  count(i) = 0
	enddo
c
c  Accumulate all the info.
c
	do j=1,nsoln
	  do i=1,nants
	    if(   abs(real(Gains(X,i,j)))+abs(aimag(Gains(X,i,j))).gt.0
     *	     .and.abs(real(Gains(Y,i,j)))+abs(aimag(Gains(Y,i,j))).gt.0
     *	       )then
	      w = Gains(Y,i,j) / Gains(X,i,j)
	      theta = atan2(aimag(w),real(w))
	      if(count(i).eq.0)then
		phase(i) = theta
		phase2(i) = theta*theta
	      else
		theta = theta + 
     *		  2*pi*nint((phase(i)/count(i) - theta)/(2*pi))
		phase(i) = phase(i) + theta
		phase2(i) = phase2(i) + theta*theta
	      endif
	      count(i) = count(i) + 1
	    endif
	  enddo
	enddo
c
c  Set the initial estimates.
c
	do i=1,nants
	  if(count(i).gt.0)then
	    theta = phase(i)  / count(i)
	    xyp(i) = cmplx(cos(theta),sin(theta))
	    sd = nint(180/pi*sqrt(abs(phase2(i)/count(i)-theta*theta)))
	    if(sd.gt.10)then
	      write(line,'(a,i2,a,i4,a)')
     *		'Scatter in estimating XY phase for antenna',i,
     *		' was',sd,' degrees'
	      call bug('w',line)
	    endif
	  else
	    xyp(i) = 1
	  endif
	enddo
c
c  Now correct the gains.
c
	do j=1,nsoln
	  do i=1,nants
	    t = abs(Gains(X,i,j))
	    if(t.gt.0) Gains(Y,i,j) =
     *		abs(Gains(Y,i,j)) * Gains(X,i,j) / t * xyp(i)
	  enddo
	enddo
	end
c************************************************************************
	subroutine CompareG(nants,nsoln,D,Gains,Flux,
     *				OldD,OldGains,OldFlux,epsi)
c
	implicit none
	integer nants,nsoln
	complex D(2,nants),Gains(2,nants,nsoln)
	complex OldD(2,nants),OldGains(2,nants,nsoln)
	real Flux(4),OldFlux(4),epsi
c
c  Determine the change in the solutions.
c
c------------------------------------------------------------------------
	include 'gpcal.h' 
	integer i,j
	complex Temp
	real t
c
	epsi = 0
	do i=1,nants
	  Temp = D(X,i) - OldD(X,i)
	  epsi = max(epsi,real(Temp)**2+aimag(Temp)**2)
	  Temp = D(Y,i) - OldD(Y,i)
	  epsi = max(epsi,real(Temp)**2+aimag(Temp)**2)
	enddo
c
	do j=1,nsoln
	  do i=1,nants
	    Temp = Gains(X,i,j) - OldGains(X,i,j)
	    t = real(Gains(X,i,j))**2 + aimag(Gains(X,i,j))**2
	    if(t.gt.0)epsi = max(epsi,(real(Temp)**2+aimag(Temp)**2)/t)
	    Temp = Gains(Y,i,j) - OldGains(Y,i,j)
	    t = real(Gains(Y,i,j))**2 + aimag(Gains(Y,i,j))**2
	    if(t.gt.0)epsi = max(epsi,(real(Temp)**2+aimag(Temp)**2)/t)
	  enddo
	enddo
c
	t = Flux(1)*Flux(1)
	do i=1,4
	  epsi = max(epsi,(Flux(i)-OldFlux(i))**2/t)
	enddo
c
	epsi = sqrt(epsi)
	end
c************************************************************************
	subroutine CopyG(nants,nsoln,D,Gains,Flux,OldD,OldGains,OldFlux)
c
	implicit none
	integer nants,nsoln
	complex D(2,nants),Gains(2,nants,nsoln)
	complex OldD(2,nants),OldGains(2,nants,nsoln)
	real Flux(4),OldFlux(4)
c
c  Copy the gains to a safe place.
c
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,j
c
	do i=1,nants
	  OldD(X,i) = D(X,i)
	  OldD(Y,i) = D(Y,i)
	enddo
c
	do j=1,nsoln
	  do i=1,nants
	    OldGains(X,i,j) = Gains(X,i,j)
	    OldGains(Y,i,j) = Gains(Y,i,j)
	  enddo
	enddo
c
	do i=1,4
	  OldFlux(i) = Flux(i)
	enddo
c
	end
c************************************************************************
	subroutine AmpSolve(nsoln,nants,nbl,flux,refant,xyref,xyvary,
     *	    D,xyp,Gains,first,tol,epsi,
     *	    Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count)
c
	implicit none
	integer nsoln,nants,nbl,refant
	real flux(4),epsi,tol
	logical first,xyref,xyvary
	complex D(2,nants),Gains(2,nants,nsoln),xyp(nants)
	integer Count(nbl,0:nsoln)
	complex Vis(4,nbl,nsoln),VisSin(4,nbl,nsoln),VisCos(4,nbl,nsoln)
	real SumS(nbl,0:nsoln),SumC(nbl,0:nsoln),SumS2(nbl,0:nsoln)
	real SumCS(nbl,0:nsoln)
c
c  Solve for antenna amplitudes and phases.
c
c  Input:
c    nsoln	Number of solution intervals.
c    nants,nbl	Number of antennae and number of baselines.
c    flux	The flux of the calibrator.
c    refant	The reference antenna number.
c    first	True if this is the first call.
c    tol	The tolerance in determining the gains.
c    D		The current leakage parameter estimates.
c    xyp	The current XY phase estimates.
c    Vis,VisSin,VisCos,Count,SumS,SumC,SumS2,SumCS
c  Input/Output:
c    Gains	The current estimates of the antenna gains. It is assumed
c		that the Y phases are equal to the X phases plus the XY phases.
c  Output:
c    epsi	Fractional change in the solutions.
c------------------------------------------------------------------------
	include 'gpcal.h'
	complex Gx(MAXANT),Gy(MAXANT),SVM(4,MAXBASE),TempX,TempY
	complex a(4,MAXBASE),b(4,MAXBASE),c(4,MAXBASE),xyphase
	real amag(4,MAXBASE),bmag(4,MAXBASE),cmag(4,MAXBASE)
	real ab(4,MAXBASE),ac(4,MAXBASE),bc(4,MAXBASE)
	real    Axy(MAXANT),SMM(4,MAXBASE)
	integer b1(MAXBASE),b2(MAXBASE),Indx(MAXANT)
	integer nantsd,nbld,i,j,k,l,soln
	logical convrg
c
c  Determine the model seen by each baseline.
c
	call GetVis(nants,nbl,flux,D,a,b,c)
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    do l=1,4
	      xyphase = (1.,0.)
	      if(l.eq.YX.or.l.eq.YY) xyphase = xyphase * xyp(i)
	      if(l.eq.XY.or.l.eq.YY) xyphase = xyphase * conjg(xyp(j))
	      a(l,k) = xyphase * a(l,k)
	      b(l,k) = xyphase * b(l,k)
	      c(l,k) = xyphase * c(l,k)
	    enddo
	  enddo
	enddo
c
	do l=1,4
	  do k=1,nbl
	    a(l,k) = conjg(a(l,k))
	    b(l,k) = conjg(b(l,k))
	    c(l,k) = conjg(c(l,k))
	    amag(l,k) =   real( a(l,k))*real( a(l,k))
     *			+ aimag(a(l,k))*aimag(a(l,k))
	    bmag(l,k) =   real( b(l,k))*real( b(l,k))
     *			+ aimag(b(l,k))*aimag(b(l,k))
	    cmag(l,k) =   real( c(l,k))*real( c(l,k))
     *			+ aimag(c(l,k))*aimag(c(l,k))
	    ab(l,k) = 2*a(l,k)*conjg(b(l,k))
	    ac(l,k) = 2*a(l,k)*conjg(c(l,k))
	    bc(l,k) = 2*b(l,k)*conjg(c(l,k))
	  enddo
	enddo
c
c  Loop over all solutions intervals.
c
	epsi = 0
	do soln=1,nsoln
c
c  Initialise for the squeezing step.
c
	  nbld = 0
	  nantsd = 0
	  do k=1,nants
	    Indx(k) = 0
	  enddo
c
c  Form the needed sums, and squeeze out unnecessary  baselines
c
	  k = 0
	  do j=2,nants
	    do i=1,j-1
	      k = k + 1
	      if(Count(k,soln).gt.0)then
		nbld = nbld + 1
		do l=1,4
		  SVM(l,nbld) = a(l,k)*Vis(l,k,soln)
     *				  + b(l,k)*VisCos(l,k,soln)
     *				  + c(l,k)*VisSin(l,k,soln)
		  SMM(l,nbld) = Count(k,soln)*(amag(l,k)+bmag(l,k))
     *				  + (cmag(l,k)-bmag(l,k))*SumS2(k,soln)
     *				  + ab(l,k)*SumC(k,soln)
     *				  + ac(l,k)*SumS(k,soln)
     *				  + bc(l,k)*SumCS(k,soln)
		enddo
		if(Indx(i).eq.0)then
		  nantsd = nantsd + 1
		  Indx(i) = nantsd
		endif
		if(Indx(j).eq.0)then
		  nantsd = nantsd + 1
		  Indx(j) = nantsd
		endif
		b1(nbld) = Indx(i)
		b2(nbld) = Indx(j)
	      endif
	    enddo
	  enddo
c
c  Fill in the current estimates of everything, and go get the solution.
c
	  do k=1,nants
	    if(Indx(k).gt.0)then
	      Gx(Indx(k)) = Gains(X,k,soln)
	      Gy(Indx(k)) = Gains(Y,k,soln) * conjg(xyp(k))
	      Axy(Indx(k)) = abs( Gains(Y,k,soln) / Gains(X,k,soln) )
	    endif
	  enddo
c
	  convrg = .true.
	  if(xyvary)then
	    if(first)then
	      call AmpInSol(nbld,nantsd,SVM,SMM,b1,b2,Gx,axy,epsi)
	      do k=1,nantsd
		Gy(k) = axy(k) * Gx(k)
	      enddo
	    endif
	    call AmpSolXY(nbld,nantsd,SVM,SMM,b1,b2,Gx,Gy,tol*tol,epsi)
	  else
	    if(first)call AmpInSol(nbld,nantsd,SVM,SMM,b1,b2,
     *							Gx,axy,epsi)
	    call AmpSol(nbld,nantsd,SVM,SMM,b1,b2,Gx,axy,tol*tol,epsi)
	  endif
c
c  Unpack the solutions and determine the Y gains.
c
	  if(convrg)then
	    do k=1,nants
	      if(Indx(k).eq.0)then
	        Gains(X,k,soln) = (0.,0.)
	        Gains(Y,k,soln) = (0.,0.)
	      else
	        Gains(X,k,soln) = Gx(Indx(k))
		if(xyvary)then
		  Gains(Y,k,soln) = xyp(k) * Gy(Indx(k))
		else
		  Gains(Y,k,soln) = axy(Indx(k))*xyp(k)*Gx(Indx(k))
		endif
	      endif
	    enddo
c
c  Refer the solution to the reference antenna.
c
	    TempX = conjg(Gains(X,refant,soln)) /
     *		      abs(Gains(X,refant,soln))
	    TempY = TempX
	    if(.not.xyref) TempY = conjg(Gains(Y,refant,soln)) / 
     *				     abs(Gains(Y,refant,soln)) *
     *				     xyp(refant)
	    do k=1,nants
	      Gains(X,k,soln) = TempX * Gains(X,k,soln)
	      Gains(Y,k,soln) = TempY * Gains(Y,k,soln)
	    enddo
	  endif
	enddo
c
c  Return with the error.
c
	epsi = sqrt(epsi)
	end
c************************************************************************
	subroutine SumVis(nsoln,nants,nbl,Gains,Count,Vis,VisCos,VisSin,
     *	  V,VS,VC)
c
	implicit none
	integer nsoln,nants,nbl
	integer Count(nbl,nsoln)
	complex Vis(4*nbl,nsoln),VisCos(4*nbl,nsoln),VisSin(4*nbl,nsoln)
	complex Gains(2*nants,nsoln),V(4*nbl),VS(4*nbl),VC(4*nbl)
c
c  This forms the sums of the visibilities for a given baseline and
c  polarisation. For efficiency's sake, the baseline and polarisation
c  dimensions have been collapsed into one.
c
c  Input:
c    nsoln	Number of solution intervals.
c    nants	Number of antennae.
c    nbl	Number of baselines.
c    Gains	The antenna gains.
c    Count
c    Vis,VisCos,VisSin
c  Output:
c    V,VC,VS
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,j,k,l,m
	integer b1(4*MAXBASE),b2(4*MAXBASE),p1(4),p2(4)
	complex g
c	
	data p1/X,Y,X,Y/
	data p2/X,Y,Y,X/
c
c  Work out an index table.
c
	k = 0
	do j=2,nants
	  do i=1,j-1
	    do m=1,4
	      b1(k+m) = 2*(i-1) + p1(m)
	      b2(k+m) = 2*(j-1) + p2(m)
	    enddo
	    k = k + 4
	  enddo
	enddo
c
c  Initialise the summing arrays.
c
	do k=1,4*nbl
	  V(k) = 0
	  VS(k) = 0
	  VC(k) = 0
	enddo
c
c  Now do the summation.
c
	do l=1,nsoln
	  do k=1,4*nbl
	    if(Count((k+3)/4,l).gt.0)then
	      g = 1/(Gains(b1(k),l)*conjg(Gains(b2(k),l)))
	      V(k)  = V(k)  + g*Vis(k,l)
	      VS(k) = VS(k) + g*VisSin(k,l)
	      VC(k) = VC(k) + g*VisCos(k,l)
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine RefSolve(nsoln,nants,nbl,flux,xyref,
     *	    D,Gains,xyp,epsi,
     *	    Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count)
c
	implicit none
	integer nsoln,nants,nbl
	logical xyref
	real flux(4),epsi
	complex D(2,nants),Gains(2,nants,nsoln),xyp(nants)
c
	integer Count(nbl,0:nsoln)
	complex Vis(4,nbl,nsoln),VisSin(4,nbl,nsoln),VisCos(4,nbl,nsoln)
	real SumS(nbl,0:nsoln),SumC(nbl,0:nsoln),SumS2(nbl,0:nsoln)
	real SumCS(nbl,0:nsoln)
c
c  This solves for an offset in the value of the leakage parameters and
c  the XY phases. It uses a linearised approach for solving for the XY phase
c  offset.
c
c  Input:
c    nsoln	Number of solution intervals.
c    nants,nbl	Number of antennae and baselines.
c    xyref	Solve for reference antenna XY phase.
c    Vis,VisSin,VisCos,Count,SumS,SumC,SumS2,SumCS Various accumulated
c		rubbish.
c    flux	The source flux.
c  Input/Output:
c    D	    )	On input, these contain the current estimates of the gains,
c    Gains  )	leakages, etc. On output, they contain the updated
c    xyp    )	estimates.
c  Output:
c    epsi	The relative change in the gains, etc.
c------------------------------------------------------------------------
	include 'gpcal.h'
	real b(3),A(3*3),t1,t2
	integer nvar,ifail
	integer pivot(3)
	integer i,j
	complex Gain,leakoff
c
c  Determine the number of things we have to solve for. Its pretty easy.
c
	nvar = 2
	if(xyref)nvar = 3
c
c  Generate the matrix that we are to solve for.
c
	call RefAcc(nsoln,nants,nvar,nbl,Gains,D,flux,A,b,
     *	  Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count)
c
c  Solve the system of equations.
c
	call sgefa(A,nvar,nvar,pivot,ifail)
	if(ifail.ne.0)call bug('f',
     *	  'Matrix inversion failed in RefSolve -- singular matrix?')
	call sgesl(A,nvar,nvar,pivot,b,1)
c
c  Save the results, and compute epsi.
c
c  The leakage offset.
c
	leakoff = 0.5*cmplx(b(1),b(2))
	do i=1,nants
	  D(X,i) = D(X,i) + leakoff
	  D(Y,i) = D(Y,i) - conjg(leakoff)
	enddo
	epsi = max(abs(b(1)),abs(b(2)))
c
c  The XY phase offset.
c
	if(xyref)then
	  t1 = b(3)
	  t2 = 1.0/sqrt(1+t1*t1)
	  Gain = cmplx(t2,t1*t2)
	  epsi = max(epsi,abs(t1))
	  do i=1,nants
	    xyp(i) = xyp(i) * Gain
	  enddo
	  do j=1,nsoln
	    do i=1,nants
	      Gains(Y,i,j) = Gains(Y,i,j) * Gain
	    enddo
	  enddo
	endif
	end
c************************************************************************
	subroutine RefAcc(nsoln,nants,nvar,nbl,Gains,D,flux,A,b,
     *	  Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count)
c
	implicit none
	integer nsoln,nants,nbl,nvar
	complex Gains(2,nants,nsoln),D(2,nants)
	real flux(4),A(nvar,nvar),b(nvar)
c
	integer Count(nbl,0:nsoln)
	complex Vis(4,nbl,nsoln),VisSin(4,nbl,nsoln),VisCos(4,nbl,nsoln)
	real SumS(nbl,0:nsoln),SumC(nbl,0:nsoln),SumS2(nbl,0:nsoln)
	real SumCS(nbl,0:nsoln)
c
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,j,k,m
	real ar(0:3,4,MAXBASE),ai(0:3,4,MAXBASE)
	real br(0:3,4,MAXBASE),bi(0:3,4,MAXBASE)
	real cr(0:3,4,MAXBASE),ci(0:3,4,MAXBASE)
	double precision temp
	complex V(4,MAXBASE),VS(4,MAXBASE),VC(4,MAXBASE)
c
	call SumVis(nsoln,nants,nbl,Gains,Count(1,1),
     *	  Vis,VisCos,VisSin,V,VS,VC)
	call RefCoeff(flux,ar,ai,br,bi,cr,ci,D,nants,nbl)
c
	do j=1,nvar
	  temp = 0
	  do m=1,4
	    do k=1,nbl
	      temp = temp
     *		   + real(V(m,k) )*ar(j,m,k) + aimag(V(m,k) )*ai(j,m,k)
     *		   + real(VC(m,k))*br(j,m,k) + aimag(VC(m,k))*bi(j,m,k)
     *		   + real(VS(m,k))*cr(j,m,k) + aimag(VS(m,k))*ci(j,m,k)
	      temp = temp -   ( ar(0,m,k)*ar(j,m,k)
     *			    +   br(0,m,k)*br(j,m,k)
     *			    +   ai(0,m,k)*ai(j,m,k)
     *			    +   bi(0,m,k)*bi(j,m,k) ) * Count(k,0)
     *			  -   ( cr(0,m,k)*cr(j,m,k)
     *			    -	br(0,m,k)*br(j,m,k)
     *			    +	ci(0,m,k)*ci(j,m,k)
     *			    -	bi(0,m,k)*bi(j,m,k) ) * SumS2(k,0)
     *			  -   ( ar(0,m,k)*br(j,m,k)
     *			    +   br(0,m,k)*ar(j,m,k)
     *			    +   ai(0,m,k)*bi(j,m,k)
     *			    +	bi(0,m,k)*ai(j,m,k) ) * SumC(k,0)
	      temp = temp -   ( ar(0,m,k)*cr(j,m,k)
     *			    +	cr(0,m,k)*ar(j,m,k)
     *			    +   ai(0,m,k)*ci(j,m,k)
     *			    +	ci(0,m,k)*ai(j,m,k) ) * SumS(k,0)
     *			  -   (	br(0,m,k)*cr(j,m,k)
     *			    +	cr(0,m,k)*br(j,m,k)
     *			    +	bi(0,m,k)*ci(j,m,k)
     *			    +	ci(0,m,k)*bi(j,m,k) ) * SumCS(k,0)

	    enddo
	  enddo
	  b(j) = temp
	enddo
c
	do j=1,nvar
	  do i=1,j
	    temp = 0
	    do m=1,4
	      do k=1,nbl
		temp = temp + ( ar(i,m,k)*ar(j,m,k)
     *			    +   br(i,m,k)*br(j,m,k)
     *			    +   ai(i,m,k)*ai(j,m,k)
     *			    +   bi(i,m,k)*bi(j,m,k) ) * Count(k,0)
     *			    + ( cr(i,m,k)*cr(j,m,k)
     *			    -	br(i,m,k)*br(j,m,k)
     *			    +	ci(i,m,k)*ci(j,m,k)
     *			    -	bi(i,m,k)*bi(j,m,k) ) * SumS2(k,0)
     *			    + ( ar(i,m,k)*br(j,m,k)
     *			    +   br(i,m,k)*ar(j,m,k)
     *			    +   ai(i,m,k)*bi(j,m,k)
     *			    +	bi(i,m,k)*ai(j,m,k) ) * SumC(k,0)
		temp = temp + ( ar(i,m,k)*cr(j,m,k)
     *			    +	cr(i,m,k)*ar(j,m,k)
     *			    +   ai(i,m,k)*ci(j,m,k)
     *			    +	ci(i,m,k)*ai(j,m,k) ) * SumS(k,0)
     *			    + (	br(i,m,k)*cr(j,m,k)
     *			    +	cr(i,m,k)*br(j,m,k)
     *			    +	bi(i,m,k)*ci(j,m,k)
     *			    +	ci(i,m,k)*bi(j,m,k) ) * SumCS(k,0)
	      enddo
	    enddo
	    A(i,j) = temp
	    A(j,i) = temp
	  enddo
	enddo
	end
c************************************************************************
	subroutine RefCoeff(flux,ar,ai,br,bi,cr,ci,D,nants,nbl)
c
	implicit none
	integer nants,nbl
	real flux(4),ar(0:3,4,nbl),ai(0:3,4,nbl)
	real br(0:3,4,nbl),bi(0:3,4,nbl),cr(0:3,4,nbl),ci(0:3,4,nbl)
	complex D(2,nants)
c
c  This geenrates the various coefficient tables, that are use to generate
c  the needed matrix.
c
c  Output:
c    ar,ai,br,bi,cr,ci
c
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer k,m
	integer ar0(0:3,4),ai0(0:3,4),br0(0:3,4),bi0(0:3,4)
	integer cr0(0:3,4),ci0(0:3,4)
	complex a(4,MAXBASE),b(4,MAXBASE),c(4,MAXBASE)
c
	data ar0/ 0, 0,-4,0,  0, 0,4,0,  0, 0, 0,0,  0, 0, 0,0/
	data ai0/ 0, 0,0,0,  0, 0, 0,0,  0, 0, 0,0,  0, 0, 0,0/
	data br0/ 0, 3,0,0,  0,-3, 0,0,  0,-2, 0,0,  0,-2, 0,0/
	data bi0/ 0, 0,0,0,  0, 0, 0,0,  0, 0,-2,0,  0, 0, 2,0/
	data cr0/ 0,-2,0,0,  0, 2, 0,0,  0,-3, 0,0,  0,-3, 0,0/
	data ci0/ 0, 0,0,0,  0, 0, 0,0,  0, 0,-3,0,  0, 0, 3,0/
c
	do k=1,nbl
	  call Fill(flux,3,ar0,ar(0,1,k))
	  call Fill(flux,3,ai0,ai(0,1,k))
	  call Fill(flux,3,br0,br(0,1,k))
	  call Fill(flux,3,bi0,bi(0,1,k))
	  call Fill(flux,3,cr0,cr(0,1,k))
	  call Fill(flux,3,ci0,ci(0,1,k))
	enddo
c
	call GetVis(nants,nbl,flux,D,a,b,c)
c
c  Now fill in the unset ones.
c
	do m=1,4
	  do k=1,nbl
	    ar(0,m,k) = real(a(m,k))
	    ai(0,m,k) = aimag(a(m,k))
	    br(0,m,k) = real(b(m,k))
	    bi(0,m,k) = aimag(b(m,k))
	    cr(0,m,k) = real(c(m,k))
	    ci(0,m,k) = aimag(c(m,k))
	    if(m.eq.XY)then
	      ar(3,m,k) =   ai(0,m,k)
	      ai(3,m,k) = - ar(0,m,k)
	      br(3,m,k) =   bi(0,m,k)
	      bi(3,m,k) = - br(0,m,k)
	      cr(3,m,k) =   ci(0,m,k)
	      ci(3,m,k) = - cr(0,m,k)
	    else if(m.eq.YX)then
	      ar(3,m,k) = - ai(0,m,k)
	      ai(3,m,k) =   ar(0,m,k)
	      br(3,m,k) = - bi(0,m,k)
	      bi(3,m,k) =   br(0,m,k)
	      cr(3,m,k) = - ci(0,m,k)
	      ci(3,m,k) =   cr(0,m,k)
	    endif
	  enddo
	enddo
	end
c************************************************************************
	subroutine GetVis(nants,nbl,flux,D,a,b,c)
c
	implicit none
	integer nants,nbl
	real flux(4)
	complex a(4,nbl),b(4,nbl),c(4,nbl),D(2,nants)
c
c  This calculates the value of the visibility that we would
c  expect to measure on each baseline, given the values of I,Q,U,V
c  and polarisation leakage.
c    V(m,k) = a(m,k) + b(m,k)*cos(2*chi) + c(m,k)*sin(2*chi)
c
c  Input:
c    nants
c    nbl
c    flux
c    D
c  Output:
c    a,b,c
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,j,k
c
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    a(XX,k) = flux(1) + cmplx(0.,flux(4))*(D(X,i)-conjg(D(X,j)))
	    b(XX,k) = flux(2) + flux(3)*(D(X,i) + conjg(D(X,j)))
	    c(XX,k) = flux(3) - flux(2)*(D(X,i) + conjg(D(X,j)))
c
	    a(YY,k) = flux(1) - cmplx(0.,flux(4))*(D(Y,i)-conjg(D(Y,j)))
	    b(YY,k) =-flux(2) + flux(3)*(D(Y,i)+conjg(D(Y,j)))
	    c(YY,k) =-flux(3) - flux(2)*(D(Y,i)+conjg(D(Y,j)))
c
	    a(XY,k) = flux(1)*(D(X,i)+conjg(D(Y,j))) - cmplx(0.,flux(4))
	    b(XY,k) = flux(3) - flux(2)*(D(X,i) - conjg(D(Y,j)))
	    c(XY,k) =-flux(2) - flux(3)*(D(X,i) - conjg(D(Y,j)))
c
	    a(YX,k) = flux(1)*(D(Y,i)+conjg(D(X,j))) + cmplx(0.,flux(4))
	    b(YX,k) = flux(3) + flux(2)*(D(Y,i) - conjg(D(X,j)))
	    c(YX,k) =-flux(2) + flux(3)*(D(Y,i) - conjg(D(X,j)))
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine PolSolve(nsoln,nants,nbl,flux,refant,polsol,polref,
     *	    qusolve,xysol,xyref,notrip,D,Gains,xyp,epsi,
     *	    present,Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count)
c
	implicit none
	integer nsoln,nants,nbl,refant
	logical xysol,qusolve,xyref,polsol,polref,notrip
	real flux(4),epsi
	complex D(2,nants),Gains(2,nants,nsoln),xyp(nants)
c
	integer Count(nbl,0:nsoln)
	logical present(nants)
	complex Vis(4,nbl,nsoln),VisSin(4,nbl,nsoln),VisCos(4,nbl,nsoln)
	real SumS(nbl,0:nsoln),SumC(nbl,0:nsoln),SumS2(nbl,0:nsoln)
	real SumCS(nbl,0:nsoln)
c
c  This solves for the leakage parameters, the source Q and U, XY phases,
c  etc. This uses a linearised approach, where the produces of the error
c  terms are assumed negligible. This means that the system of equations is
c  linear, and so can be explicitly solved.
c
c  On the reference antenna:
c    * aimag(Gains(X,refant,*)) are not varied.
c  If the "xysol" option is not used:
c    * xyp(*) are not varied.
c  If the "qusolve" option is not used:
c    * flux(2:3) are not varied.
c  If the "xyref" option is not used, then
c    * xyp(refant) is not varied.
c  If the "polref" option is not used, then
c    * D(X,refant) is not varied.
c  If the "qusolve" and "polref" options are used together
c    * real(D(X,refant)) is not varied.
c
c  Input:
c    nsoln	Number of solution intervals.
c    nants,nbl	Number of antennae and baselines.
c    qusolve	If true, solve for Q and U.
c    xyref	Solve for reference antenna XY phase.
c    notrip	True if there are no good closures in the data.
c    polref	Solve for the reference antenna X feed alignment,
c		differential ellipticity.
c    xysol	If true, solve for the XY phase.
c    refant	The reference antenna number.
c    present	True if the antenna is represented in the data.
c    Vis,VisSin,VisCos,Count,SumS,SumC,SumS2,SumCS Various accumulated
c		rubbish.
c  Input/Output:
c    flux   )	On input, these contain the current estimates of the gains,
c    Gains  )	leakages, etc. On output, they contain the updated estimates.
c    D      )
c  Output:
c    xyp	XY phase sstimate.
c    epsi	The relative change in the gains, etc.
c------------------------------------------------------------------------
	include 'gpcal.h'	
	integer Dvar(2,2,MAXANT),QUvar,Tvar(2,MAXANT),nvar,ifail
	integer Vars(8,4,MAXBASE),pivot(6*MAXANT+1)
	integer i,j
	complex expix(MAXANT),expiy(MAXANT),D1
	real b(6*MAXANT+1),A((6*MAXANT+1)*(6*MAXANT+1)),temp
	logical polref1
c
c  Determine if we are really going to do polref.
c
	D1 = D(X,refant)
	polref1 = polref
	if(polref)then
	  if(qusolve)then
	    temp = flux(2)*flux(2) + flux(3)*flux(3) + flux(4)*flux(4)
	    if(temp.lt.1e-4*flux(1)*flux(1))then
	      call bug('w','Turning off POLREF for this iteration')
	      polref1 = .false.
	    else
	      D1 = cmplx(real(D(X,refant)),0.)
	    endif
	  else
	    D1 = (0.,0.)
	  endif
	endif
c
c  D(Y,refant) will be solved for, unless "notrip" is true. In the later case
c  make sure it is zero.
c
	D(Y,refant) = 0
c
c  We solve simultaneously for the antenna phase error, Q and U, as well
c  as the leakage terms. First determine how many unknowns there are, and
c  assign a index to each of them.
c  QUvar,Tvar,Dvar give the indices for Q, antenna phase, and leakage terms.
c
	call GetVarNo(nants,nbl,polsol,polref1,qusolve,xysol,xyref,
     *	  notrip,refant,present,QUvar,Tvar,Dvar,nvar,Vars)
c
c  We are going to solve the system Ax = b. Generate A and b.
c
	call PolAcc(nsoln,nants,nbl,Gains,D1,polref1,refant,flux,Vars,
     *	  A,b,nvar,Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count)
c
c  Solve the system of equations using LINPACK routines.
c
	call sgefa(A,nvar,nvar,pivot,ifail)
	if(ifail.ne.0)call bug('f',
     *	  'Matrix inversion failed -- singular matrix?')
	call sgesl(A,nvar,nvar,pivot,b,1)
c
c  Save the results, and compute epsi.
c
c  Q and U.
c
	epsi = 0
	if(QUvar.gt.0)then
	  flux(2) = flux(2) + b(QUvar)   * flux(1)
	  flux(3) = flux(3) + b(QUvar+1) * flux(1)
	  epsi = max(epsi,b(QUvar)**2,b(QUvar+1)**2)
	endif
c
c  The phases.
c
	if(xysol)then
	  do i=1,nants
	    call UnpackT(Tvar(X,i),b,nvar,expix(i),epsi)
	    call UnpackT(Tvar(Y,i),b,nvar,expiy(i),epsi)
	    xyp(i) = xyp(i) * expiy(i)*conjg(expix(i))
	  enddo
c
	  do j=1,nsoln
	    do i=1,nants
	      Gains(X,i,j) = Gains(X,i,j) * expix(i)
	      Gains(Y,i,j) = Gains(Y,i,j) * expiy(i)
	    enddo
	  enddo
	endif
c
c  The leakage terms.
c
	do i=1,nants
	  call UnpackD(Dvar(1,X,i),Dvar(2,X,i),b,nvar,D(X,i),epsi)
	  call UnpackD(Dvar(1,Y,i),Dvar(2,Y,i),b,nvar,D(Y,i),epsi)
	enddo
c
c  Return with the bacon.
c
	epsi = sqrt(epsi)
c
	end
c************************************************************************
	subroutine PolAcc(nsoln,nants,nbl,Gains,D,polref,refant,flux,
     *	  Vars,A,b,nvar,Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count)
c
	implicit none
	logical polref
	integer nsoln,nants,nbl,nvar,refant
	complex Gains(2,nants,nsoln),D
	integer Vars(8,4,nbl)
	real A(nvar,nvar),b(nvar),flux(4)
c
	integer Count(nbl,0:nsoln)
	complex Vis(4,nbl,nsoln),VisSin(4,nbl,nsoln),VisCos(4,nbl,nsoln)
	real SumS(nbl,0:nsoln),SumC(nbl,0:nsoln),SumS2(nbl,0:nsoln)
	real SumCS(nbl,0:nsoln)
c
c  This routine is responsible for building the matrix that we are to
c  solve.
c
c  Input:
c    D		The leakage to remove from the reference antenna.
c    nsolns	Number of solution intervals.
c    nants,nbl	Number of antennae, and number of baselines.
c    nvar	Number of variables to solve for.
c    Vars	Indices of the variables.
c    Gains	The antenna gains.
c    flux	The source fluxes (I,Q,U,V).
c    Count,Vis,VisSin,VisCos,SumS,SumC,SumS2,SumCS Various accumulated crap.
c  Output:
c    A		Matrix to solve.
c    b		Left hand side. That is the matrix eqn is
c		    b = Ax
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,j,k,m,v1,v2
	complex V(4,MAXBASE),VS(4,MAXBASE),VC(4,MAXBASE)
	real ar(0:8,4),ai(0:8,4),br(0:8,4),bi(0:8,4),cr(0:8,4),ci(0:8,4)
	real Cconst(0:8,0:8,4),Csin(0:8,0:8,4),Ccos(0:8,0:8,4)
	real Ccossin(0:8,0:8,4),Csin2(0:8,0:8,4)
	
c
c  Sum the visibilities.
c
	call SumVis(nsoln,nants,nbl,Gains,Count(1,1),
     *	  Vis,VisCos,VisSin,V,VS,VC)
c
c  Correct the sums for the leakage of the reference antenna.
c
	if(.not.polref)call DCorrect(nants,nbl,V,VC,VS,D,refant,flux,
     *	  Count(1,0),SumC(1,0),SumS(1,0),SumCS(1,0),SumS2(1,0))
c
c  Generate the coefficient tables.
c
	call PolCoeff(flux,Cconst,Csin,Ccos,Csin2,Ccossin,
     *						ar,ai,br,bi,cr,ci)
c
c  We want to solve the system of linear equations, Ax = b.
c  Initialise A and b, then accumulate them.
c
	do j=1,nvar
	  b(j) = 0
	  do i=1,j
	    A(i,j) = 0
	  enddo
	enddo
c
	do k=1,nbl
	  do m=1,4
	    do j=1,8
	      v2 = Vars(j,m,k)
	      if(v2.gt.0)then
		b(v2) = b(v2)
     *		      + real(V(m,k) )*ar(j,m) + aimag(V(m,k) )*ai(j,m)
     *		      + real(VC(m,k))*br(j,m) + aimag(VC(m,k))*bi(j,m)
     *		      + real(VS(m,k))*cr(j,m) + aimag(VS(m,k))*ci(j,m)
     *		      - Cconst(0,j,m)  *Count(k,0)
     *		      - Csin2(0,j,m)   *SumS2(k,0)
     *		      - Csin(0,j,m)    *SumS(k,0)
     *		      - Ccos(0,j,m)    *SumC(k,0)
     *		      - Ccossin(0,j,m) *SumCS(k,0)
	      endif
	    enddo
	  enddo
	enddo
c
	do k=1,nbl
	  if(Count(k,0).gt.0)then
	    do m=1,4
	      do j=1,8
		v2 = Vars(j,m,k)
		if(v2.gt.0)then
		  do i=1,j
		    v1 = Vars(i,m,k)
		    if(v1.gt.0)then
		      A(v1,v2) = A(v1,v2)
     *		          + Cconst(i,j,m)  *Count(k,0) 
     *			  + Csin2(i,j,m)   *SumS2(k,0)
     *		          + Csin(i,j,m)    *SumS(k,0)
     *		          + Ccos(i,j,m)    *SumC(k,0)
     *		          + Ccossin(i,j,m) *SumCS(k,0)
		    endif
		  enddo
		endif
	      enddo
	    enddo
	  endif
	enddo
c
	do j=2,nvar
	  do i=1,j-1
	    a(j,i) = a(i,j)
	  enddo
	enddo
	end
c************************************************************************
	subroutine DCorrect(nants,nbl,V,VC,VS,D0,refant,flux,
     *	  Count,SumC,SumS,SumCS,SumS2)
c
	implicit none
	integer nants,nbl,refant,Count(nbl)
	complex V(4,nbl),VC(4,nbl),VS(4,nbl),D0
	real flux(4),SumC(nbl),SumS(nbl),SumCS(nbl),SumS2(nbl)
c
c  Correct the sums of the visibilities for the leakage of the reference
c  feed.
c
c  Input:
c    nants,nbl
c    refant
c    D
c    flux
c    Count,SumC,SumS,SumCS,SumS2
c  Input/Output:
c    V,VC,VS
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer j,k
	complex D
c
c  Return straight away if there is nothing to do.
c
	if(real(D0).eq.0.and.aimag(D0).eq.0)return
c
c  Correct the antennas before the reference antenna.
c
	D = conjg(D0)
	k = (refant-1)*(refant-2) / 2
	do j=1,refant-1
	  k = k + 1
	  V(XX,k) = V(XX,k) - D *
     *	    (-flux(2)*SumS(k) + flux(3)*SumC(k)
     *	        - cmplx(0.,flux(4)*Count(k)))
	  VC(XX,k) = VC(XX,k) - D *
     *	    ( -flux(2)*SumCS(k) + flux(3)*(Count(k)-SumS2(k))
     *		- cmplx(0.,flux(4)*SumC(k)))
	  VS(XX,k) = VS(XX,k) - D *
     *	    (-flux(2)*SumS2(k) + flux(3)*SumCS(k)
     *		- cmplx(0.,flux(4)*SumS(k)))
	  V(YX,k) = V(YX,k)   - D *
     *	    (flux(1)*Count(k) - flux(2)*SumC(k) - flux(3)*SumS(k))
	  VC(YX,k) = VC(YX,k) - D *
     *	    ( flux(1)*SumC(k) - flux(2)*(Count(k)-SumS2(k))
     *	    - flux(3)*SumCS(k))
	  VS(YX,k) = VS(YX,k)   - D *
     *	    (flux(1)*SumS(k) - flux(2)*SumCS(k) - flux(3)*SumS2(k))
	enddo
	k = k + 1
c
c  Correct the antennas after the reference antenna.
c
	D = D0
	do j=refant+1,nants
	  k = k + j - 2
	  V(XX,k) = V(XX,k) - D *
     *	    (-flux(2)*SumS(k) + flux(3)*SumC(k)
     *		+ cmplx(0.,flux(4)*Count(k)))
	  VC(XX,k) = VC(XX,k) - D *
     *	    ( -flux(2)*SumCS(k) + flux(3)*(Count(k)-SumS2(k))
     *		+ cmplx(0.,flux(4)*SumC(k)))
	  VS(XX,k) = VS(XX,k) - D *
     *	    (-flux(2)*SumS2(k) + flux(3)*SumCS(k)
     *		+ cmplx(0.,flux(4)*SumS(k)))
	  V(XY,k) = V(XY,k)   - D *
     *	    (flux(1)*Count(k) - flux(2)*SumC(k) - flux(3)*SumS(k))
	  VC(XY,k) = VC(XY,k) - D *
     *	    ( flux(1)*SumC(k) - flux(2)*(Count(k)-SumS2(k))
     *	    - flux(3)*SumCS(k))
	  VS(XY,k) = VS(XY,k)   - D *
     *	    (flux(1)*SumS(k) - flux(2)*SumCS(k) - flux(3)*SumS2(k))
	enddo
	end
c************************************************************************
	subroutine PolCoeff(flux,Cconst,Csin,Ccos,Csin2,Ccossin,
     *						ar,ai,br,bi,cr,ci)
	implicit none
	real flux(4)
	real ar(0:8,4),ai(0:8,4),br(0:8,4),bi(0:8,4),cr(0:8,4),ci(0:8,4)
	real Cconst(0:8,0:8,4),Csin(0:8,0:8,4),Ccos(0:8,0:8,4)
	real Ccossin(0:8,0:8,4),Csin2(0:8,0:8,4)
c
c  This generates various coefficient tables, that are used to generate
c  the needed matrix.
c
c  We define the arrays ar,br,cr the coefficient arrays for the "real"
c  equations, as:
c
c  real(V(p)) - ar(0,p) - br(0,p)*cos(2*chi) - cr(0,p)*sin(2*chi)
c         = x1*(ar(1,p) + br(1,p)*cos(2*chi) + cr(1,p)*sin(2*chi))
c         + x2*(ar(2,p) + br(2,p)*cos(2*chi) + cr(2,p)*sin(2*chi))
c         + x3*(ar(3,p) + br(3,p)*cos(2*chi) + cr(3,p)*sin(2*chi))
c         + x4*(ar(4,p) + br(4,p)*cos(2*chi) + cr(4,p)*sin(2*chi))
c         + x5*(ar(5,p) + br(5,p)*cos(2*chi) + cr(5,p)*sin(2*chi))
c         + x6*(ar(6,p) + br(6,p)*cos(2*chi) + cr(6,p)*sin(2*chi))
c         + x7*(ar(7,p) + br(7,p)*cos(2*chi) + cr(7,p)*sin(2*chi))
c         + x8*(ar(8,p) + br(8,p)*cos(2*chi) + cr(8,p)*sin(2*chi))
c  Similarly the ai,bi,ci arrays are the coefficients for the "imaginary"
c  equations. The unknowns x1,x2,...,x8 are
c    q,u,theta1,real(D1),aimag(D1),theta2,real(D2),aimag(D2).
c  "p" varies over XX,YY,XY and YX.
c
c  We also need tables of coefficients of xi by xj, which involve products
c  of trig functions with themselves.
c
c  The coefficient arrays Ccos is defined as:
c
c    Ccos(i,j,p)*cos(2*chi) = coefficient for sum of the terms in sine.
c		            = Sum (ar(i,p)*br(j,p) + ar(j,p)*br(i,p))*cos(2*chi)
c			    =(ar(i,p)*br(j,p) + ar(j,p)*br(i,p))*Sum(cos(2*chi))
c  Similarly Csin is coefficient for sum of cosine terms.
c	     Csin2			    sine squared terms.
c	     Ccossin			    cos-sin terms.
c	     Cconst			    constant terms.
c  Because this task only stores Sum(sin**2) (and not Sum(cos**2)),
c  we use cos**2 = 1 - sin**2, so the cos**2 terms get included into the
c  sin**2 and constant term.
c
c  Input:
c    flux	The source flux (I,Q,U,V).
c  Output:
c    ar,ai	Coefficients for the constant terms.
c    br,bi	Coefficients for the cosine terms.
c    cr,ci	Coefficients for the sine terms.
c    Cconst
c    Ccos
c    Csin
c    Csin2
c    Ccossin
c------------------------------------------------------------------------
c	integer qq,uu,theta1,theta2,D1r,D1i,D2r,D2i
c	parameter(qq=1,uu=2,theta1=3,D1r=4,D1i=5,theta2=6,D2r=7,D2i=8)
	integer i,j,m
c
	integer ar0(0:8,4),ai0(0:8,4),br0(0:8,4),bi0(0:8,4)
	integer cr0(0:8,4),ci0(0:8,4)
c                 0  Q  U T1 D1r i T2 D2r i   0  Q  U T1 D1r i T2 D2r i
	data ar0/ 1, 0, 0, 0, 0,-4, 0, 0,-4,  1, 0, 0, 0, 0, 4, 0, 0, 4,
     *		  0, 0, 0, 4, 1, 0,-4, 1, 0,  0, 0, 0,-4, 1, 0, 4, 1, 0/
 	data ai0/ 0, 0, 0, 1, 4, 0,-1,-4, 0,  0, 0, 0, 1,-4, 0,-1, 4, 0,
     *		 -4, 0, 0, 0, 0, 1, 0, 0,-1,  4, 0, 0, 0, 0, 1, 0, 0,-1/
	data br0/ 2, 0, 0, 0, 3, 0, 0, 3, 0, -2, 0, 0, 0, 3, 0, 0, 3, 0,
     *		  3, 0, 1, 0,-2, 0, 0, 2, 0,  3, 0, 1, 0, 2, 0, 0,-2, 0/
	data bi0/ 0, 0, 0, 2, 0, 3,-2, 0,-3,  0, 0, 0,-2, 0, 3, 2, 0,-3,
     *		  0, 0, 0, 3, 0,-2,-3, 0,-2,  0, 0, 0, 3, 0, 2,-3, 0, 2/
	data cr0/ 3, 0, 0, 0,-2, 0, 0,-2, 0, -3, 0, 0, 0,-2, 0, 0,-2, 0,
     *		 -2,-1, 0, 0,-3, 0, 0, 3, 0, -2,-1, 0, 0, 3, 0, 0,-3, 0/
	data ci0/ 0, 0, 0, 3, 0,-2,-3, 0, 2,  0, 0, 0,-3, 0,-2, 3, 0, 2,
     *		  0, 0, 0,-2, 0,-3, 2, 0,-3,  0, 0, 0,-2, 0, 3, 2, 0, 3/
c
c  Get the coefficient arrays. These are determined from the ari,aii,etc
c  arrays. If ari(i,j) is non-zero
c    ar(i,j) = sign(flux(abs(ari(i,j))),ari(i,j))
c
	call Fill(flux,8,ar0,ar)
	call Fill(flux,8,ai0,ai)
	call Fill(flux,8,br0,br)
	call Fill(flux,8,bi0,bi)
	call Fill(flux,8,cr0,cr)
	call Fill(flux,8,ci0,ci)
c
c  Work out the product coefficients.
c
	do m=1,4
	  do j=0,8
	    do i=0,8
	      Cconst(i,j,m) = ar(i,m)*ar(j,m) + br(i,m)*br(j,m)
     *			    + ai(i,m)*ai(j,m) + bi(i,m)*bi(j,m)
	      Csin2(i,j,m)  = cr(i,m)*cr(j,m) - br(i,m)*br(j,m)
     *			    + ci(i,m)*ci(j,m) - bi(i,m)*bi(j,m)
	      Csin(i,j,m)   = ar(i,m)*cr(j,m) + ar(j,m)*cr(i,m)
     *		            + ai(i,m)*ci(j,m) + ai(j,m)*ci(i,m)
	      Ccos(i,j,m)   = ar(i,m)*br(j,m) + ar(j,m)*br(i,m)
     *			    + ai(i,m)*bi(j,m) + ai(j,m)*bi(i,m)
	      Ccossin(i,j,m)= br(i,m)*cr(j,m) + br(j,m)*cr(i,m)
     *			    + bi(i,m)*ci(j,m) + bi(j,m)*ci(i,m)
	    enddo
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine Fill(flux,n,ar0,ar)
c
	implicit none
	integer n
	real flux(4),ar(0:n,4)
	integer ar0(0:n,4)
c
c  Fill in a coefficient array.
c------------------------------------------------------------------------
	integer i,j,s,Indx
c
	do j=1,4
	  do i=0,n
	    if(ar0(i,j).ne.0)then
	      s = sign(1,ar0(i,j))
	      Indx = abs(ar0(i,j))
	      ar(i,j) = s*flux(Indx)
	    else
	      ar(i,j) = 0
	    endif
	  enddo
	enddo
	end
c************************************************************************
	subroutine GetVarNo(nants,nbl,polsol,polref,qusolve,xysol,xyref,
     *	  notrip,refant,present,QUvar,Tvar,Dvar,nvar,Vars)
c
	implicit none
	integer nants,nbl,refant
	logical xyref,polsol,polref,qusolve,xysol,present(nants),notrip
	integer QUvar,Tvar(2,nants),Dvar(2,2,nants),nvar,Vars(8,4,nbl)
c
c  This assigns each of the things we have to solve for a number,
c  varying from 1 to nvar. If something is assigned a number of 0, this
c  means that we do not solve for this. Rather it is assumed to be 0.
c
c  Input:
c    nants,nbl	Number of antennae and baselines.
c    present	True if the antenna is present in the data.
c    qusolve,xysol,xyref,polref,polsol,notrip Logicals, which determine
c		what we solve for.
c    refant	The reference antenna number.
c  Output:
c    QUvar	Gives the variable number of Q
c    Tvar	Gives the variable numbers of the phase errors.
c    Dvar	Gives the variable numbers of the leakage terms. The 3
c		indices of this are for real/imag, X/Y, nants.
c    Vars	Gives the variable numbers for all the variables in a given
c		set of equations.
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,j,k,l,Uvar
	integer p1(4),p2(4)
	data p1/X,Y,X,Y/
	data p2/X,Y,Y,X/
c		
c  Determine a "variable number" for all the unknowns.
c
	nvar = 0
	if(qusolve)then
	  QUvar = 1
	  nvar = nvar + 2
	else
	  QUvar = 0
	endif
	do i=1,nants
	  if(xysol.and.i.ne.refant.and.present(i))then
	    Tvar(X,i) = nvar + 1
	    Tvar(Y,i) = nvar + 2
	    nvar = nvar + 2
	  else if(i.eq.refant.and.xyref)then
	    Tvar(X,i) = 0
	    Tvar(Y,i) = nvar + 1
	    nvar = nvar + 1
	  else
	    Tvar(X,i) = 0
	    Tvar(Y,i) = 0
	  endif
c
c  Do not solve for leakage terms of missing antennas!
c
	  if(.not.present(i).or..not.polsol)then
	    Dvar(1,X,i) = 0
	    Dvar(2,X,i) = 0
	    Dvar(1,Y,i) = 0
	    Dvar(2,Y,i) = 0
c
c  Solve for leakage terms if its not the reference antenna, or if "polref"
c  was specified.
c
	  else if(i.ne.refant.or.polref)then
	    if(qusolve.and.i.eq.refant)then
	      Dvar(1,X,i) = 0
	    else
	      Dvar(1,X,i) = nvar + 1
	      nvar = nvar + 1
	    endif
	    Dvar(2,X,i) = nvar + 1
	    Dvar(1,Y,i) = nvar + 2
	    Dvar(2,Y,i) = nvar + 3 
	    nvar = nvar + 3
c
c  Do not solve for the leakage terms of the X feed of the reference antenna
c  if "polref" was not specified. Do not solve for the characteristics of
c  the Y antenna if there are no closures (we cannot).
c
	  else
	    Dvar(1,X,i) = 0
	    Dvar(2,X,i) = 0
	    if(notrip)then
	      Dvar(1,Y,i) = 0
	      Dvar(2,Y,i) = 0
	    else
	      Dvar(1,Y,i) = nvar + 1
	      Dvar(2,Y,i) = nvar + 2
	      nvar = nvar + 2
	    endif
	  endif
	enddo
	if(nvar.lt.1)call bug('f','No variables to solve for??')
c
c  In any given equation in the least squares solution, 8 unknowns
c  could be present. Determine a table which gives the variable number
c  of these 8 unknowns. This table varies with baseline and XX,YY,XY,YX.
c
	Uvar = 0
	if(QUvar.gt.0) Uvar = QUvar + 1
	do k=1,nbl
	  do l=1,4
	    Vars(1,l,k) = QUvar
	    Vars(2,l,k) = Uvar
	  enddo
	enddo
c
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    do l=1,4
	      Vars(3,l,k) = Tvar(p1(l),i)
	      Vars(4,l,k) = Dvar(1,p1(l),i)
	      Vars(5,l,k) = Dvar(2,p1(l),i)
	      Vars(6,l,k) = Tvar(p2(l),j)
 	      Vars(7,l,k) = Dvar(1,p2(l),j)
	      Vars(8,l,k) = Dvar(2,p2(l),j)
	    enddo
	  enddo
	enddo
	end
c************************************************************************
	subroutine UnpackD(Indx1,Indx2,x,nvar,D,epsi)
c
	implicit none
	integer Indx1,Indx2,nvar
	complex D
	real x(nvar),epsi
c
c  Unpack a leakage term.
c
c  Input:
c    Indx1,Indx2 The indices of the real and imaginary parts.
c    nvar	 The number of variables.
c    x		 The variables.
c  Input/Output:
c    D		The leakage parameter.
c    epsi	Error measure.
c------------------------------------------------------------------------
	real t1,t2
	complex t,delta
c
	if(Indx1.gt.0)then
	  t1 = x(Indx1)
	else
	  t1 = real(D)
	endif
	if(Indx2.gt.0)then
	  t2 = x(Indx2)
	else
	  t2 = aimag(D)
	endif
c
	t = cmplx(t1,t2)
	delta = t - D
	epsi = max(epsi,real(delta)**2+aimag(delta)**2)
	D = t
	end
c************************************************************************
	subroutine UnpackT(Indx,x,nvar,Gain,epsi)
c
	implicit none
	integer Indx,nvar
	real x(nvar),epsi
	complex Gain
c
c  Unpack a phase angle, theta,(which is assumed to be small), and return
c  exp(i*theta).
c
c  Input:
c    Indx
c    x
c    nvar
c  Input/Output:
c    epsi
c  Output:
c    Gain
c------------------------------------------------------------------------
	real t1,t2
c
	if(Indx.eq.0)then
	  Gain = (1.,0.)
	else
	  t1 = x(Indx)
	  t2 = 1.0/sqrt(1+t1*t1)
	  Gain = cmplx(t2,t1*t2)
	  epsi = max(epsi,t1**2)
	endif
c
	end
c************************************************************************
	subroutine DatRead(tIn,polsol,nants,nbl,maxsoln,
     *	  interval,refant,minant,time,nsoln,present,notrip,
     *	  Vis,VisCos,VisSin,SumS,SumC,SumS2,SumCS,Count,
     *	  source,freq)
c
	implicit none
	integer nsoln,nants,nbl,maxsoln,tIn,refant,minant
	logical polsol,present(nants),notrip
	double precision interval(2),freq
	character source*(*)
	complex Vis(4,nbl,maxsoln)
	complex VisCos(4,nbl,maxsoln),VisSin(4,nbl,maxsoln)
	real SumS(nbl,0:maxsoln),SumC(nbl,0:maxsoln)
	real SumS2(nbl,0:maxsoln),SumCS(nbl,0:maxsoln)
	integer Count(nbl,0:maxsoln)
	double precision time(maxsoln)
c
c  Read in the visibility data, and form sums of things that we will need
c  in the gain solution stages.
c
c  Inputs:
c    maxsoln	Max number of solutions that we can deal with.
c    nbl	Number of baselines.
c    nants	Number of antennae.
c    polsol	If true, then process XY and YX as well as just XX, YY.
c    interval	Solution time interval.
c    refant	The reference antenna number. This must always be present.
c    minant	The minimum number of antennae to attempt a solution.
c  Output:
c    time	The time for each solution interval.
c    nsoln	Number of solution intervals.
c    present	Logical indicating whether the antenna is present at all.
c    notrip	Flag which is true if there are no closures present.
c    Vis,VisCos,VisSin,SumC,SumS,SumS2,SumCS,Count
c		Accumulated statistics.
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,j,k,l,i1,i2,bl,totvis,ngood,nauto,nchan,nbad,n,nread
	integer ncorr,nr,b1,b2,b3
	double precision preamble(4),tfirst,tlast,epsi
	real Cos2Chi,Sin2Chi,chi
	logical accept,flag(MAXCHAN,4),okscan,trip
	complex data(MAXCHAN,4),d(4)
c
c  Externals.
c
	character itoaf*8
	logical doaccept
c
	call uvrewind(tIn)
	call uvDatRd(preamble,Data(1,XX),flag(1,XX),MAXCHAN,nchan)
	if(nchan.eq.0)call bug('f','No data read from input file')
c
c  Get the source and frequency of the first data.
c
	call uvrdvra(tIn,'source',source,' ')
	call uvfit1(tIn,'frequency',nchan,freq,epsi)
c
c  Get the data. Read the remaining correlations for this record.
c
	n = 0
	nr = 2
	if(polsol)nr = 4
	nsoln = 0
	tfirst = 0
	tlast = 0
	totvis = 0
	ngood = 0
	nbad = 0
	nauto = 0
	dowhile(nchan.gt.0)
	  totvis = totvis + 1
c
c  Read the other polarisations and determine the overall flags.
c
	  do j=2,nr
	    call uvDatRd(preamble,Data(1,j),flag(1,j),MAXCHAN,nread)
	    if(nread.ne.nchan)
     *	      call bug('f','Inconsistent number of polarisation chans')
	  enddo
c
c  Accumulate the polarisations.
c
	  ncorr = 0
	  d(XX) = 0
	  d(YY) = 0
	  d(XY) = 0
	  d(YX) = 0
	  if(polsol)then
	    do j=1,nchan
	      if(flag(j,XX).and.flag(j,YY).and.
     *	         flag(j,XY).and.flag(j,YX))then
		ncorr = ncorr + 1
		d(XX) = d(XX) + data(j,XX)
		d(YY) = d(YY) + data(j,YY)
		d(XY) = d(XY) + data(j,XY)
		d(YX) = d(YX) + data(j,YX)
	      endif
	    enddo
	  else
	    do j=1,nchan
	      if(flag(j,XX).and.flag(j,YY))then
		ncorr = ncorr + 1
		d(XX) = d(XX) + data(j,XX)
		d(YY) = d(YY) + data(j,YY)
	      endif
	    enddo
	  endif
c
c  Determine if we will accept this visibility.
c
	  call basant(preamble(4),i1,i2)
	  accept = ncorr.gt.0.and.i1.gt.0.and.i1.le.nants.and.
     *			          i2.gt.0.and.i2.le.nants.and.
     *				  i1.ne.i2
c
c  If its a good record, process it. Initialise a new scan slot if needed.
c
	  if(accept)then
	    if(	nsoln.eq.0.or.
     *		preamble(3).gt.tfirst+interval(1).or.
     *		preamble(3).gt.tlast+interval(2))then
	      if(nsoln.ne.0)then
		time(nsoln) = (tfirst+tlast)/2
		okscan = doaccept(time(nsoln),Count(1,nsoln),
     *					n,nants,nbl,refant,minant)
		if(okscan)then
		  ngood = ngood + n
		else
		  nsoln = nsoln - 1
		endif
	      endif
c
	      nsoln = nsoln + 1
	      n = 0
c
	      if(nsoln.gt.maxsoln)
     *		call bug('f','Too many solution intervals')
	      tfirst = preamble(3)
	      tlast  = tfirst
	      do j=1,nbl
		Count(j,nsoln) = 0
		SumS(j,nsoln)  = 0
		SumC(j,nsoln)  = 0
		SumS2(j,nsoln) = 0
		SumCS(j,nsoln) = 0
		do i=1,4
	          Vis(i,j,nsoln) = (0.,0.)
		  VisCos(i,j,nsoln) = (0.,0.)
		  VisSin(i,j,nsoln) = (0.,0.)
		enddo
	      enddo
	    else if(preamble(3).lt.tfirst)then
	      call bug('f','Data does not appear to be in time order')
 	    endif
c
	    tlast = max(tlast,preamble(3))
c
	    bl = (i2-1)*(i2-2)/2 + i1
c
c  Accumulate the statistics.
c
	    call uvrdvrr(tIn,'chi',chi,0.)
	    Cos2Chi = cos(2*chi)
	    Sin2Chi = sin(2*chi)
	    do i=1,4
	      Vis(i,bl,nsoln) = Vis(i,bl,nsoln)       + d(i)
	      VisCos(i,bl,nsoln) = VisCos(i,bl,nsoln) + d(i)*Cos2Chi
	      VisSin(i,bl,nsoln) = VisSin(i,bl,nsoln) + d(i)*Sin2Chi
	    enddo
	    Count(bl,nsoln) = Count(bl,nsoln) + ncorr
	    SumS(bl,nsoln)  = SumS(bl,nsoln)  + ncorr * Sin2Chi
	    SumC(bl,nsoln)  = SumC(bl,nsoln)  + ncorr * Cos2Chi
	    SumCS(bl,nsoln) = SumCS(bl,nsoln) + ncorr * Cos2Chi*Sin2Chi
	    SumS2(bl,nsoln) = SumS2(bl,nsoln) + ncorr * Sin2Chi*Sin2Chi
	    n = n + 1
	  else if(i1.eq.i2)then
	    nauto = nauto + 1
	  else
	    nbad = nbad + 1
	  endif
	  call uvDatRd(preamble,Data(1,XX),flag(1,XX),MAXCHAN,nchan)
	enddo
c
c  Check if the last time interval is to be accepted.
c
	if(nsoln.ne.0)then
	  time(nsoln) = (tfirst+tlast)/2
	  okscan = doaccept(time(nsoln),Count(1,nsoln),
     *				n,nants,nbl,refant,minant)
	  if(okscan)then
	    ngood = ngood + n
	  else
	    nsoln = nsoln - 1
	  endif
	endif
c
c  Tell the user whats what.
c
	call output('Number of solution intervals: '//itoaf(nsoln))
	call output('Total visibilities read: '//itoaf(totvis))
	if(nauto.ne.0)
     *	  call bug('w','Number of autocorrelations discarded: '//
     *	  itoaf(nauto))
	if(nbad.ne.0)
     *	  call bug('w','Number of flagged visibilities: '//
     *	  itoaf(nbad))
	if(ngood+nauto+nbad.ne.totvis) call bug('w',
     *	  'No. visibs failing refant or minant requirement: '//
     *	  itoaf(totvis-ngood-nauto-nbad))
	call output('Number visibilities accepted: '//itoaf(ngood))
	if(nsoln.eq.0.or.ngood.eq.0)
     *	  call bug('f','No data to process!')
c
c  Accumulate all the sums into slot 0.
c
	do j=1,nbl
	  Count(j,0) = Count(j,1)
	  SumS(j,0) = SumS(j,1)
	  SumC(j,0) = SumC(j,1)
	  SumS2(j,0) = SumS2(j,1)
	  SumCS(j,0) = SumCS(j,1)
	enddo
c
	do l=2,nsoln
	  do j=1,nbl
	    Count(j,0) = Count(j,0) + Count(j,l)
	    SumS(j,0)  = SumS(j,0)  + SumS(j,l)
	    SumC(j,0)  = SumC(j,0)  + SumC(j,l)
	    SumS2(j,0) = SumS2(j,0) + SumS2(j,l)
	    SumCS(j,0) = SumCS(j,0) + SumCS(j,l)
	  enddo
	enddo
c
c  Initialise the array used to check if an antenna is present.
c
	do i=1,nants
	  present(i) = .false.
	enddo
c
c  Determine which antennas are present.
c
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    present(i) = present(i).or.Count(k,0).gt.0
	    present(j) = present(j).or.Count(k,0).gt.0
	  enddo
	enddo
c
c  Determine whether there are any closures present.
c
	trip = .false.
	do k=3,nants
	  do j=2,k-1
	    do i=1,j-1
	      b1 = (k-1)*(k-2)/2 + j
	      b2 = (j-1)*(j-2)/2 + i
	      b3 = (k-1)*(k-2)/2 + i
	      trip = trip.or.(Count(b1,0).gt.0.and.
     *			      Count(b2,0).gt.0.and.
     *			      Count(b3,0).gt.0)
	    enddo
	  enddo
	enddo
	notrip = .not.trip
c
c  Give warning messages.
c
	k = 0
	do i=1,nants
	  if(.not.present(i)) call bug('w',
     *	    'No data present for antenna '//itoaf(i))
	enddo
	if(notrip)then
	  call bug('w','No closures were present')
	  call bug('w','Quality of solution will suffer')
	endif
	end
c************************************************************************
	logical function doaccept(time,Count,n,nants,nbl,refant,minant)
c
	implicit none
	integer nants,nbl,refant,minant,n
	integer Count(nbl)
	double precision time
c
c  Determine whether the reference antenna, and the minimum number of
c  antennae are present in the data.
c
c  Input:
c    time	Time corresponding to this interval.
c    n		Number of visibilities in this interval.
c    refant
c    minant
c    nants,nbl
c    Count	Number of visibility records for each baseline.
c  Output:
c    doaccept	True if we are to accept this solution interval.
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,j,k,npresent,lnvis
	logical present(MAXANT)
	character string*32,nvis*8,line*80
c
c  Externals.
c
	integer len1
	character itoaf*8
c
	do k=1,nants
	  present(k) = .false.
	enddo
c
	k = 0
	do j=2,nants
	  do i=1,j-1
	    k = k + 1
	    if(Count(k).gt.0)then
	      present(i) = .true.
	      present(j) = .true.
	    endif
	  enddo
	enddo
c
	npresent = 0
	do k=1,nants
	  if(present(k))npresent = npresent + 1
	enddo
c
	doaccept = npresent.ge.minant.and.present(refant)
	if(.not.doaccept)then
	  nvis = itoaf(n)
	  lnvis = len1(nvis)
	  call JulDay(time,'H',string)
	  line = 'Discarding '//nvis(1:lnvis)//
     *		' visibilities, being the scan near '//string
	  call bug('w',line)
	endif
	end
c************************************************************************
	subroutine PolIni(tIn,xyphase,nxyphase,D,xyp,nants)
c
	implicit none
	integer tIn,nants,nxyphase
	real xyphase(nants)
	complex D(2,nants),xyp(nants)
c
c  Set the initial estimates of the gains and leakage terms.
c
c  Inputs:
c    tIn	Handle of the input file.
c    xyphase	User-specified values of the xyphase.
c    nxyphase	Number of user-specified xyphases.
c    nants	Number of antennae.
c  Output:
c    D		The initial leakage terms.
c    xyp	The initial xyphases.
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,n,n0,item,iostat
	real theta,phase(MAXANT),sd(MAXANT)
	integer count(MAXANT)
c
c  Externals.
c
	integer hsize
c
c  Get the user-specified XY phases.
c
	do i=1,nxyphase
	  theta = pi/180 * xyphase(i)
	  xyp(i) = cmplx(cos(theta),sin(theta))
	enddo
c
c  Determine the XY phase from an old gain table, if there is one.
c
	n = nxyphase
	if(nxyphase.lt.nants)then
	  call GetXY(tIn,phase,sd,count,MAXANT,n0)
	  n0 = min(n0,nants)
	  do i=n+1,n0
	    if(count(i).gt.0)then
	      theta = phase(i)
	      xyp(i) = cmplx(cos(theta),sin(theta))
	    else
	      xyp(i) = (1.,0.)
	    endif
	  enddo
	  n = max(n0,n)
	endif
c
c  Fill in any as yet uninitialised xyphase values.
c
	do i=n+1,nants
	  xyp(i) = (1.,0.)
	enddo
c
c  Initialise the leakage terms. See if there is already a leakage
c  table in the input file. If so use this as the initial estimate.
c  Otherwise just set things to zero.
c
	call haccess(tIn,item,'leakage','read',iostat)
	if(iostat.eq.0)then
	  call output(
     *	    'Using leakage parameters from input as initial guess')
	  n = min((hsize(item)-8)/16,nants)
	  call hreadr(item,D,8,16*n,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error reading leakage table from input')
	    call bugno('f',iostat)
	  endif
	  call hdaccess(item,iostat)
	else
	  n = 0
	endif
	do i=n+1,nants
	  D(X,i) = (0.,0.)
	  D(Y,i) = (0.,0.)
	enddo
c
	end
c************************************************************************
	subroutine GainIni(nants,nsoln,Gains,xyp)
c
	implicit none
	integer nants,nsoln
	complex Gains(2,nants,nsoln),xyp(nants)
c
c  Initialise the gains.
c
c  Input:
c    xyp
c  Output:
c    Gains
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer i,j
	do j=1,nsoln
	  do i=1,nants
	    Gains(X,i,j) = (1.,0.)
	    Gains(Y,i,j) = xyp(i)
	  enddo
	enddo
	end
c************************************************************************
	subroutine GainTab(tIn,time,Gains,nants,nsoln)
c
	implicit none
	integer tIn
	integer nsoln,nants
	double precision time(nsoln)
	complex Gains(2*nants,nsoln)
c
c  Write out the amp/phase gain table.
c
c  Input:
c    tIn	Handle of the file.
c    time	Time (midpoint) for each solution interval.
c    Gains	The gains to be written out.
c    nants	Number of antenna.
c    nsoln	Number of solution intervals.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer iostat,off,item,i,j
	complex G(2*MAXANT)
c
	call haccess(tIn,item,'gains','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output amp/phase table.')
	  call bugno('f',iostat)
	endif
	call hwritei(item,0,0,4,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error writing header of amp/phase table')
	  call bugno('f',iostat)
	endif
c
c  Write out all the gains.
c
	off = 8
	do i=1,nsoln
	  call hwrited(item,time(i),off,8,iostat)
	  off = off + 8
	  if(iostat.ne.0)then
	    call bug('w','Error writing time to amp/phase table')
	    call bugno('f',iostat)
	  endif
	  do j=1,2*nants
	    if(abs(real(Gains(j,i)))+abs(aimag(Gains(j,i))).ne.0)then
	      G(j) = 1/Gains(j,i)
	    else
	      G(j) = (0.,0.)
	    endif
	  enddo
	  call hwriter(item,G,off,8*2*nants,iostat)
	  off = off + 8*2*nants
	  if(iostat.ne.0)then
	    call bug('w','Error writing gains to amp/phase table')
	    call bugno('f',iostat)
	  endif
	enddo
c
c  Finished writing the gain table.
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
c  Now write out the other parameters that need to go along with this.
c
	call wrhdi(tIn,'nfeeds',2)
	call wrhdi(tIn,'ntau',0)
	call wrhdi(tIn,'ngains',2*nants)
	call wrhdi(tIn,'nsols',nsoln)
	call wrhdd(tIn,'interval',0.5d0)
	  
	end
c************************************************************************
	subroutine GainScal(flux,Gains,ngains)
c
	implicit none
	integer ngains
	complex Gains(ngains)
	real flux(4)
c
c  Scale the gains and the flux so that the rms gain is 1.
c
c  Input:
c    ngains	Number of gains.
c  Input/Output:
c    Gains	The gains.
c    flux	Nominal source flux.
c------------------------------------------------------------------------
	real Sum2,t
	integer n,i
c
	n = 0
	Sum2 = 0
	do i=1,ngains
	  t = real(Gains(i))**2 + aimag(Gains(i))**2
	  if(t.gt.0)then
	    Sum2 = Sum2 + t
	    n = n + 1
	  endif
	enddo
c
c  Return if all the gains are flagged bad.
c
	if(Sum2.eq.0)return
c
c  Scale the gains.
c
	t = sqrt(n/Sum2)
	do i=1,ngains
	  Gains(i) = t*Gains(i)
	enddo
c
c  Scale the fluxes.
c
	t = Sum2/n
	do i=1,4
	  flux(i) = t*flux(i)
	enddo
c
	end
c************************************************************************
	subroutine LeakTab(tIn,D,nants)
c
	implicit none
	integer tIn,nants
	complex D(2,nants)
c
c  Save the polarisation gains in an item in the calibrator file.
c  This uses some dirty tricks. Firstly it uses wrhdc to create the
c  item (because there is no way in FORTRAN to make the correct header).
c  Second it uses hwriter, because there is no hwritec.
c
c  Input:
c    tIn	Handle of the calibrator file.
c    D		Leakage parameters.
c    nants	Number of antennae.
c------------------------------------------------------------------------
	integer item,iostat
c
	call wrhdc(tIn,'leakage',(1.,0.))
	call haccess(tIn,item,'leakage','append',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening the output leakage table')
	  call bugno('f',iostat)
	endif
c
c  The item alignment requirement requires that we start writing at
c  byte 8. Bytes 0-3 are the header, and bytes 4-7 are unused.
c
	call hwriter(item,D,8,2*8*nants,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error writing to the leakage table')
	  call bugno('f',iostat)
	endif
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	end
c************************************************************************
	subroutine writeo(tIn,line)
c
	implicit none
	integer tIn
	character line*(*)
c
c  Write out a line to the history file (if open) and the output.
c
c------------------------------------------------------------------------
	character string*80
c
	string = 'GPCAL: '//line
	call HisWrite(tIn,string)
	call output(line)
	end
c************************************************************************
	subroutine GetOpt(dopass,amphsol,polsol,xysol,xyref,
     *				xyvary,polref,qusolve,oldflux)
c
	implicit none
	logical dopass,amphsol,polsol,xysol,xyvary,xyref,polref,qusolve
	logical oldflux
c
c  Get processing options.
c
c  Output:
c    dopass	Apply bandpass correction if possible.
c    amphsol	Solve for antenna gains.
c    polsol	Solve for polarisation corrections.
c    xysol	Solve for the xy phases (except for the reference antenna).
c    xyvary	XY phase may vary with time.
c    xyref	Solve for the xy phase of the reference.
c    polref	Solve for reference antenna misalignment and differential
c               ellipticity.
c    qusolve	Solve for Q and U as well as everything else.
c    oldflux	Use pre-Aug 1994 ATCA flux scale.
c------------------------------------------------------------------------
	integer nopt
	parameter(nopt=9)
	logical present(nopt)
	character opts(nopt)*10
	data opts/'noamphase ','nopol     ','polref    ','noxy      ',
     *    	  'xyref     ','qusolve   ','xyvary    ','nopass    ',
     *		  'oldflux   '/
c
	call options('options',opts,present,nopt)
	amphsol = .not.present(1)
	polsol  = .not.present(2)
	polref  = present(3)
	xysol   = .not.present(4)
	xyref   = present(5)
	qusolve = present(6)
	xyvary  = present(7)
	dopass  = .not.present(8)
	oldflux = present(9)
c
	if(.not.polsol.and.polref.and.
     *	  ((xysol.and..not.xyvary).or.qusolve))then
     	  call bug('w','Unsupported combination of options')
	  call bug('f','Include either the NOXY or XYVARY option')
	endif
	if(.not.xysol.and.xyref.and.
     *	  (polsol.or.qusolve.or.xyvary.or..not.polref))
     *	  call bug('f','Unsupported combination of options')
	if(.not.xysol.and.xyvary)call bug('f',
     *	  'Option NOXY cannot be used with XYVARY')
	if(.not.amphsol.and..not.polsol.and..not.polref.and.
     *	   .not.qusolve.and..not.xysol.and..not.xyref)call bug('f',
     *	  'The options inhibit solving for anything')
	if(.not.amphsol.and.(xysol.or.xyref))call bug('f',
     *	  'Using NOAMPHASE prevents solving for XY phases')
	if(polref.and..not.xyref) call bug('w',
     *	  'It is advisable to use option XYREF with POLREF')
c
	end
c************************************************************************
      subroutine getiquv(source,freq,oldflux,flux,defflux)
      implicit none
c
      character source*(*)
      double precision freq
      real flux(4)
      logical defflux,oldflux
c
c     Provide a model of I,Q,U, and V for selected calibrators
c
c  Input:
c     source   Name of the source.
c     freq     Observing frequency.
c     oldflux  Use old 1934-638 flux density scale.
c  Output:
c     flux     I,Q,U, and V.  All set according to the some default value.
c     defflux  True if the source was not found, and default of 1,0,0,0 used.
c----------------------------------------------------------------------
      integer ierr
      character umsg*80,src*16
c
      src = source
      if(src.eq.'1934-638'.or.src.eq.'1934'.or.
     *		      src.eq.'1939-637')then
	if(oldflux)then
	  src = 'old1934'
	  call output('Using pre-Aug94 ATCA flux scale for 1934-638')
	else
	  call bug('w','Using post-Aug94 ATCA flux scale for 1934-638')
	endif
      endif
      ierr = 2
      if(src.ne.' ')call calstoke(src,'i',freq,flux(1),1,ierr)
      if(ierr.ne.2)then
	defflux = .false.
	call calstoke(src,'q',freq,flux(2),1,ierr)
	call calstoke(src,'u',freq,flux(3),1,ierr)
	call calstoke(src,'v',freq,flux(4),1,ierr)
	umsg = 'Frequency extrapolation was needed to '//
     *	  'determine Stokes parameters of '//source
	if(ierr.eq.1)call bug('w',umsg)
        write(umsg,'(''Using IQUV = '',4(f8.4,'',''))') flux
	call output(umsg)
      else
	defflux = .true.
	flux(1) = 1
	flux(2) = 0
	flux(3) = 0
	flux(4) = 0
      endif
      end
c************************************************************************
	subroutine PhaseSol(nbl,nants,SumVM,b1,b2,G,tol,epsi)
c
	implicit none
	integer nbl,nants
	integer b1(nbl),b2(nbl)
	real tol,epsi
	complex SumVM(4,nbl),G(nants)
c
c  Solve for the phase corrections which minimise the error. This uses
c  a nonlinear Jacobi iteration, as suggested by Fred Schwab in "Adaptive
c  calibration of radio interferomter data", SPIE Vol. 231, 1980
c  International Optical Computing Conference (pp 18-24). The damping
c  heuristics are copied from AIPS ASCAL.
c
c  Input:
c    nbl	Number of baselines.
c    nants	Number of antennae.
c    b1,b2	This gives the antennae pair for each baseline.
c    SumVM	Sum of conjg(Vis)*Model
c    tol	Convergence tolerance.
c  Input/Output:
c    Gain	The antenna gain solution.
c    epsi	Chanle in solution during the iterations.
c------------------------------------------------------------------------
	include 'gpcal.h'
	complex SVM(MAXBASE),Sum(MAXANT)
	logical convrg
	real Factor,Change
	complex Temp
	integer Niter,i
c
	integer MAXITER
	parameter(MAXITER=100)
c
c  Initialise. Remember the ratio of the Y to X gains, and then
c  sum the Y and X parts together.
c
	do i=1,nants
	  Sum(i) = 0
	enddo
c
	do i=1,nbl
	  SVM(i) = SumVM(XX,i) + SumVM(YY,i)
	enddo
c
	Factor = 0.8
	if(Nants.le.6)Factor = 0.5
c
c  Iterate.
c
	Convrg = .false.
	Niter = 0
	do while(.not.Convrg.and.Niter.lt.MAXITER)
	  Niter = Niter + 1
c
c  Sum the contributions over the baselines. Note that the following
c  loop has a dependency.
c
	  do i=1,nbl
	    Sum(b1(i)) = Sum(b1(i)) + G(b2(i)) *       SVM(i)
	    Sum(b2(i)) = Sum(b2(i)) + G(b1(i)) * conjg(SVM(i))
	  enddo
c
c  Update the gains. The following will be flagged as a short loop
c  on the Cray, if we assume we have fewer than 32 antennae. Hopefully
c  this will vectorise. For "typical" cases, the absolute value function
c  in the next loop takes up about 30-40% of the run time of this routine
c  on a VAX.
c
	  Change = 0
c#maxloop 32
	  do i=1,nants
	    Temp = ( Sum(i)/abs(Sum(i)) )
	    Temp = G(i) + Factor * ( Temp - G(i) )
	    Temp = Temp/abs(Temp)
	    Change = Change + real(G(i)-Temp)**2
     *			    + aimag(G(i)-Temp)**2
	    G(i) = Temp
	    Sum(i) = (0.,0.)
	  enddo
	  epsi = max(epsi,Change/nants)
	  Convrg = Change/nants .lt. tol
	enddo
c
	end
c************************************************************************
	subroutine AmpInSol(nbl,nants,SumVM,SumMM,b1,b2,G,axy,epsi)
c
	implicit none
	integer nbl,nants,b1(nbl),b2(nbl)
	complex SumVM(4,nbl),G(nants)
	real SumMM(4,nbl),axy(nants),epsi
c
c  Get the first estimate at the amplitude gain. Do this by getting the
c  phase-only solution, and then scaling to the average amplitude.
c
c  Input:
c    nbl	Number of baselines.
c    nants	Number of antennae.
c    SumVM,SumMM Accumulated rubbish used in the solution step.
c    b1,b2	Antenna numbers for each sum.
c  Input/Output:
c    G		The gains.
c    epsi	Solution change.
c------------------------------------------------------------------------
	include 'gpcal.h'
	real tol
	parameter(tol=1e-5)
	integer i
	real SumRMM,SumRVM,alpha
c
c  Get the phase solution.
c
	call PhaseSol(nbl,nants,SumVM,b1,b2,G,tol,epsi)
c
c  Determine a scale factor to multiply the gains by, to make them about
c  the correct amplitude.
c
	SumRVM = 0
	SumRMM = 0
	do i=1,nbl
	  SumRVM = SumRVM + conjg(G(b1(i)))*G(b2(i))*
     *	     ( SumVM(XX,i) + axy(b1(i))*axy(b2(i))*SumVM(YY,i) )
	  SumRMM = SumRMM +   G(b1(i))*G(b2(i))  *
     *			conjg(G(b1(i))*G(b2(i))) *
     *	      (SumMM(XX,i) + (axy(b1(i))*axy(b2(i)))**2 * SumMM(YY,i))
	enddo
	alpha = sqrt( abs(SumRVM / SumRMM) )
c
	do i=1,nants
	  G(i) = alpha * G(i)
	enddo
c
	end
c************************************************************************
	subroutine AmpSolXY(nbl,nants,SumVM,SumMM,b1,b2,Gx,Gy,tol,epsi)
c
	implicit none
	integer nbl,nants,b1(nbl),b2(nbl)
	complex SumVM(4,nbl),Gx(nants),Gy(nants)
	real SumMM(4,nbl),epsi,tol
c
c  This determines the amplitude/phase solution for a given time interval.
c  The complex gain of the X and Y feeds are determined. The XY phase or
c  amplitude ratio is not constrained.
c
c  Inputs:
c    nbl,nants	Number of baselines and number of antennae.
c    SumMM,SumVM Accumulated rubbish used in the solution process.
c    b1,b2	Antenna numbers of the given baseline.
c    tol	Tolerance in determining the solutions.
c  Input/Output:
c    Gx		Current estimate of the X gains.
c    Gy		Current estimate of the Y gains.
c  Output:
c    epsi	Fractional change in gains.
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer MAXITER
	parameter(MAXITER=100)
c
	integer i,niter
	logical convrg
	real t,Factor,ChangeX,ChangeY,SumWtX,SumWtY
	real Sum2(2,MAXANT)
	complex Sum(2,MAXANT),Temp
c
	real Factors(11)
	data Factors/0.5,0.75,8*0.9,0.5/
c
c  Initialise.
c
	do i=1,nants
	  Sum(X,i) = (0.,0.)
	  Sum(Y,i) = (0.,0.)
	  Sum2(X,i) = 0.
	  Sum2(Y,i) = 0.
	enddo
c
	convrg = .false.
	niter = 0
	dowhile(.not.convrg.and.niter.lt.maxiter)
	  niter = niter + 1
c
c  Get the same damping factor as AIPS.
c
	  if(nants.le.6)then
	    factor = 0.5
	  else
	    factor = factors(min(11,niter))
	  endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop contains a dependency (it should not vectorise).
c
	  do i=1,nbl
	    Sum(X,b1(i))  = Sum(X,b1(i))
     *				+ Gx(b2(i)) *	    SumVM(XX,i)
     *				+ Gy(b2(i)) *       SumVM(XY,i)
	    Sum(X,b2(i))  = Sum(X,b2(i))
     *				+ Gx(b1(i)) * conjg(SumVM(XX,i))
     *				+ Gy(b1(i)) * conjg(SumVM(YX,i))
c
	    Sum2(X,b1(i)) = Sum2(X,b1(i))
     *	      + (real(Gx(b2(i)))**2 + aimag(Gx(b2(i)))**2)*SumMM(XX,i)
     *	      + (real(Gy(b2(i)))**2 + aimag(Gy(b2(i)))**2)*SumMM(XY,i)
	    Sum2(X,b2(i)) = Sum2(X,b2(i)) +
     *	      + (real(Gx(b1(i)))**2 + aimag(Gx(b1(i)))**2)*SumMM(XX,i)
     *	      + (real(Gy(b1(i)))**2 + aimag(Gy(b1(i)))**2)*SumMM(YX,i)
c
	    Sum(Y,b1(i))  = Sum(Y,b1(i))
     *				+ Gy(b2(i)) *       SumVM(YY,i)
     *				+ Gx(b2(i)) *       SumVM(YX,i)
	    Sum(Y,b2(i))  = Sum(Y,b2(i))
     *				+ Gy(b1(i)) * conjg(SumVM(YY,i))
     *				+ Gx(b1(i)) * conjg(SumVM(XY,i))
c
	    Sum2(Y,b1(i)) = Sum2(Y,b1(i))
     *	      + (real(Gy(b2(i)))**2 + aimag(Gy(b2(i)))**2)*SumMM(YY,i)
     *	      + (real(Gx(b2(i)))**2 + aimag(Gx(b2(i)))**2)*SumMM(YX,i)
	    Sum2(Y,b2(i)) = Sum2(Y,b2(i)) +
     *	      + (real(Gy(b1(i)))**2 + aimag(Gy(b1(i)))**2)*SumMM(YY,i)
     *	      + (real(Gx(b1(i)))**2 + aimag(Gx(b1(i)))**2)*SumMM(XY,i)
	  enddo
c
c  Update the gains.
c
	  ChangeX = 0
	  SumWtX = 0
	  ChangeY = 0
	  SumWtY = 0
c#maxloop 32
	  do i=1,nants
c
c  Evaluate X and Y gains.
c
	    Temp = Sum(X,i) / Sum2(X,i) - Gx(i)
	    Gx(i) = Gx(i) + Factor * Temp
	    ChangeX = ChangeX + real(Temp)**2 + aimag(Temp)**2
	    SumWtX = SumWtX + real(Gx(i))**2  + aimag(Gx(i))**2
c
	    Temp = Sum(Y,i) / Sum2(Y,i) - Gy(i)
	    Gy(i) = Gy(i) + Factor * Temp
	    ChangeY = ChangeY + real(Temp)**2 + aimag(Temp)**2
	    SumWtY = SumWtY + real(Gy(i))**2  + aimag(Gy(i))**2
c
c  Zero the accumulators.
c
	    Sum(X,i) = 0
	    Sum(Y,i) = 0
	    Sum2(X,i) = 0
	    Sum2(Y,i) = 0
	  enddo
	  t = max(ChangeX/SumWtX,ChangeY/SumWtY)
	  epsi = max(epsi,t)
	  convrg = t.lt.tol
	enddo
c
	end
c************************************************************************
	subroutine AmpSol  (nbl,nants,SumVM,SumMM,b1,b2,G,axy,tol,epsi)
c
	implicit none
	integer nbl,nants,b1(nbl),b2(nbl)
	complex SumVM(4,nbl),G(nants)
	real SumMM(4,nbl),axy(nants),epsi,tol
c
c  This determines the amplitude/phase solution for a given time interval.
c  The complex gain of the X feed, and the X/Y amplitude gain ratio is
c  determined. It it assumed that the XY phases have already been applied
c  to the YY data.
c
c  Inputs:
c    nbl,nants	Number of baselines and number of antennae.
c    SumMM,SumVM Accumulated rubbish used in the solution process.
c    b1,b2	Antenna numbers of the given baseline.
c    tol	Tolerance in determining the solutions.
c  Input/Output:
c    G		Current estimate of the X gains.
c    Axy	Current estimate of the X/Y amplitude ratio.
c  Output:
c    epsi	Fractional change in gains.
c------------------------------------------------------------------------
	include 'gpcal.h'
	integer MAXITER
	parameter(MAXITER=100)
c
	integer i,niter
	logical convrg
	real t,Factor,ChangeX,ChangeY,SumWtX,SumWtY
	real Sum2(2,MAXANT)
	complex Sum(2,MAXANT),Temp
c
	real Factors(11)
	data Factors/0.5,0.75,8*0.9,0.5/
c
c  Initialise.
c
	do i=1,nants
	  Sum(X,i) = (0.,0.)
	  Sum(Y,i) = (0.,0.)
	  Sum2(X,i) = 0.
	  Sum2(Y,i) = 0.
	enddo
c
	convrg = .false.
	niter = 0
	dowhile(.not.convrg.and.niter.lt.maxiter)
	  niter = niter + 1
c
c  Get the same damping factor as AIPS.
c
	  if(nants.le.6)then
	    factor = 0.5
	  else
	    factor = factors(min(11,niter))
	  endif
c
c  Sum the contributions over the baselines. Note that the following
c  loop contains a dependency (it should not vectorise).
c
	  do i=1,nbl
	    Sum(X,b1(i))  = Sum(X,b1(i)) +
     *			   G(b2(i)) *       SumVM(XX,i)
	    Sum(X,b2(i))  = Sum(X,b2(i)) +
     *			   G(b1(i)) * conjg(SumVM(XX,i))
c
	    Sum2(X,b1(i)) = Sum2(X,b1(i)) +
     *	      (real(G(b2(i)))**2 + aimag(G(b2(i)))**2)*SumMM(XX,i)
	    Sum2(X,b2(i)) = Sum2(X,b2(i)) +
     *	      (real(G(b1(i)))**2 + aimag(G(b1(i)))**2)*SumMM(XX,i)
c
	    Sum(Y,b1(i))  = Sum(Y,b1(i)) +
     *	      Axy(b2(i)) * G(b2(i)) *       SumVM(YY,i)
	    Sum(Y,b2(i))  = Sum(Y,b2(i)) +
     *	      Axy(b1(i)) * G(b1(i)) * conjg(SumVM(YY,i))
c
	    Sum2(Y,b1(i)) = Sum2(Y,b1(i)) + Axy(b2(i))**2 *
     *	      (real(G(b2(i)))**2 + aimag(G(b2(i)))**2)*SumMM(YY,i)
	    Sum2(Y,b2(i)) = Sum2(Y,b2(i)) + Axy(b1(i))**2 *
     *	      (real(G(b1(i)))**2 + aimag(G(b1(i)))**2)*SumMM(YY,i)
	  enddo
c
c  Update the gains.
c
	  ChangeX = 0
	  SumWtX = 0
	  ChangeY = 0
	  SumWtY = 0
c#maxloop 32
	  do i=1,nants
c
c  Evaluate X gain
c
	    t = 1./(Sum2(X,i) + Axy(i)*Axy(i)*Sum2(Y,i))
	    Temp = t * ( Sum(X,i) + Axy(i)*Sum(Y,i) ) - G(i)
	    G(i) = G(i) + Factor * Temp
	    ChangeX = ChangeX + real(Temp)**2 + aimag(Temp)**2
	    SumWtX = SumWtX + real(G(i))**2  + aimag(G(i))**2
c
c  Evaluate Y amplitude.
c
	    t = real(conjg(G(i))*Sum(Y,i)) /
     *	        ((real(G(i))**2+aimag(G(i))**2) * Sum2(Y,i)) - Axy(i)
	    t = max(t,-0.5*Axy(i))
	    Axy(i) = Axy(i) + Factor * t
	    ChangeY = ChangeY + t*t
	    SumWtY = SumWtY + Axy(i)*Axy(i)
c
c  Zero the accumulators.
c
	    Sum(X,i) = 0
	    Sum(Y,i) = 0
	    Sum2(X,i) = 0
	    Sum2(Y,i) = 0
	  enddo
	  t = max(ChangeX/SumWtX,ChangeY/SumWtY)
	  epsi = max(epsi,t)
	  convrg = t.lt.tol
	enddo
c
	end
