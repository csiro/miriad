c************************************************************************
	program gpnorm
	implicit none
c
c= GpNorm -- Correct the polarisation leakage terms for an offset.
c& rjs
c: calibration
c+
c	GpNorm is a MIRIAD task which corrects gains and polarisation
c	leakage tables for XYphase and leakage offsets. It is based on the
c	supposition that the leakage and gain tables are offset from the
c	true values. This is often the case when they have been determined
c	from observations of a weakly polarised calibrator. GpNorm
c	compares the leakage terms with nominally correct terms, and determines
c	an leakage and XY phase offset to add which will minimise the
c	difference between the two sets of leakages. The determined leakage
c	and XY phase offset can then be used to correct the leakages and
c	gains.
c@ vis
c	The visibility data-set to correct. This must have a leakage table.
c	Additionally, it should have a gains table if the XY phase correction
c	is to be applied. No default.
c@ cal
c	The data-set containing the nominally correct leakage table. No
c	default.
c@ select
c	Normal uv selection. Only antenna-based selection is supported.
c@ options
c	Task enrichment parameters. Several can be given, separated by commas.
c	Minimum match is used.
c	  noflag   Treat leakages that are identically 0 as good.
c	  noxy     Do not solve for, or apply, the XY phase offset.
c	  apply    Apply the solved for corrections to the vis data-set. The
c	           default is to solve for them only.
c	  xyleak   GPNORM can determine the xy phase differences from both the
c	           gains table and polarisation leakages. By default, it assumes
c	           the gains-derived value should take precedence. The xyleak
c	           option causes GPNORM to take the leakage-derived value as the
c	           preferred on. This only has an effect if options=apply is used
c	           and options=noxy is not used.
c--
c  History:
c    rjs    28nov91 Original version.
c    rjs    17dec91 Allow number of leakages to differ between the two
c		    files.
c    rjs     4aug92 Handle new gains file possibility, and lack of xyphases
c		    item.
c    rjs    12oct93 noapply is now the default.
c    rjs    13jul96 Added select keyword.
c    rjs    28nov97 Added more decimal places to printout.
c    rjs    27apr09 Added option zeroflag.
c    rjs    05may09 Deduced xyphase was being applied with the wrong sign!!!!
c    rjs    07may09 Compare gain tables. xyleak option.
c
c  Bugs and Shortcomings:
c   * If the number of leakages in the "vis" file is greater than the 
c     cal file, and the leakages are being corrected, then the last ones
c     are not corrected!
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	character version*(*)
	parameter(version='GpNorm: version 1.0 07-May-09')
	logical doxy,doapply,antflag(MAXANT),zerofl,xyleak
	integer tCal,tVis,iostat,nLeaks,itVis,itCal,itemp,i,ntot
	character vis*64,cal*64,line*64
	complex CalLeak(2,MAXANT),VisLeak(2,MAXANT)
	complex xyphase,offset,xycorr
	real error,arg,theta0,thetad,facvis,faccal
	real xypvis(MAXANT),xypcal(MAXANT)
	integer CntVis(MAXANT),CntCal(MAXANT),ncal,nvis,nants,n,n0
c
	integer MAXSELS
	parameter(MAXSELS=100)
	real sels(MAXSELS)
c
c  Externals.
c
	double precision antbas
	integer hsize
	character itoaf*8
	logical selProbe,hdprsnt
c
c  Get the inputs and check them.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keya('cal',cal,' ')
	call GetOpt(doxy,doapply,zerofl,xyleak)
	call selInput('select',sels,MAXSELS)
	call keyfin
c
	if(vis.eq.' ')call bug('f','Input vis file must be given')
	if(cal.eq.' ')call bug('f','Input cal file must be given')
c
c  Open the two files. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,vis,'old',iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error opening '//vis)
	call hopen(tCal,cal,'old',iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error opening '//cal)
c
c  Start history processing.
c
	if(doapply)then
	  call hisopen(tVis,'append')
	  call hiswrite(tVis,'GPNORM: Miriad '//version)
	  call hisinput(tVis,'GPNORM')
	endif
c
c  Compare gains tables.
c
	ntot = 0
	n0 = 0
	if(hdprsnt(tVis,'gains').and.hdprsnt(tCal,'gains').and.doxy)then
	  call getxy(tVis,facVis,xypVis,CntVis,MAXANT,nvis)
	  call getxy(tCal,facCal,xypCal,CntCal,MAXANT,ncal)
	  nants = min(nvis,ncal)
c
	  arg = 0
	  do i=1,nants
	    n = min(CntVis(i),CntCal(i))
	    if(n.gt.0.and.selProbe(sels,'antennae',antbas(i,i)))then
	      n0 = n0 + 1
	      theta0 = 0
	      if(ntot.gt.0)theta0 = arg/ntot
	      thetad = xypCal(i)-xypVis(i)
	      thetad = thetad + 2*PI*nint((theta0-thetad)/(2*PI))
	      arg = arg + n*thetad
	      ntot = ntot + n
	    endif
	  enddo
	  if(ntot.gt.0)then
	    if(doapply)
     *		call hiswrite(tVis,'GPNORM: Gain table comparison:')
	    call output('Gain table comparison:')
	    call output('----------------------')
	    call output('  Number of antennas selected: '//itoaf(n0))
	    arg = arg/ntot
	    write(line,'(a,f7.2)')'  Xyphase offset (degrees): ',
     *							180/PI*arg
	    call output(line)
	    if(doapply)call hiswrite(tVis,'GPNORM: '//line)
	    call output(' ')
	    xycorr = cmplx(cos(arg),sin(arg))
	  endif
	endif
c
c  Compare leakage tables.
c
	call haccess(tVis,itVis,'leakage','append',iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error opening vis leakages')
	nLeaks = (hsize(itVis)-8)/16
	if(nLeaks.lt.2.or.nLeaks.gt.MAXANT)
     *	  call bug('f','Bad number of leakages in vis file')
c
	call haccess(tCal,itCal,'leakage','read',iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error opening cal leakages')
	itemp = (hsize(itCal)-8)/16
	if(itemp.lt.2)
     *	  call bug('f','Bad number of leakages in cal file')
	if(itemp.ne.nLeaks)
     *	  call bug('w','Different number of leakage terms in the files')
	nLeaks = min(nLeaks,itemp)
c
	do i=1,nLeaks
	  antflag(i) = selProbe(sels,'antennae',antbas(i,i))
	enddo
c
c  Read in the leakages.
c
	call hreadr(itVis,VisLeak,8,16*nLeaks,iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error reading vis leakages')
	call hreadr(itCal,CalLeak,8,16*nLeaks,iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error reading cal leakages')
c
c  Do the fitting between the two.
c
	call output('Leakage table comparison:')
	if(doapply)call hiswrite(tVis,
     *			'GPNORM: Leakage table comparison')
	call output('-------------------------')
	call PolComp(CalLeak,VisLeak,nLeaks,xyphase,offset,error,
     *	  antflag,doxy,zerofl)
c
c  Report on what we have found out, write it to the history.
c
	arg = atan2(aimag(xyphase),real(xyphase))
	write(line,'(a,f7.2)')'  Xyphase offset (degrees): ',
     *						180/PI*arg
	call output(line)
	if(doapply)call hiswrite(tVis,'GPNORM: '//line)
	write(line,'(a,f7.4,a,f7.4,a)')'  Offset: (',real (offset),',',
     *						   aimag(offset),')'
	call output(line)
	if(doapply)call hiswrite(tVis,'GPNORM: '//line)
	write(line,'(a,f8.5)')'  Rms Error: ',error
	call output(line)
	if(doapply)call hiswrite(tVis,'GPNORM: '//line)
c
c  Write out the leakages if needed.
c
	if(doapply)then
	  if(ntot.eq.0.or.xyleak)xycorr = xyphase
	  call LeakCorr(VisLeak,nLeaks,xycorr,offset,zerofl)
	  call hwriter(itVis,VisLeak,8,16*nLeaks,iostat)
	  if(iostat.ne.0)call NormBug(iostat,
     *					'Error writing vis leakages')
	endif
c
c  Close up the leakages.
c
	call hdaccess(itVis,iostat)
	if(iostat.ne.0)call NormBug(iostat,'Errror closing vis leaks')
	call hdaccess(itCal,iostat)
	if(iostat.ne.0)call NormBug(iostat,'Errror closing cal leaks')
c
c  Bloody hell! Possibly we have to apply the XY phase to the gain solutions.
c
	if(doxy.and.doapply)call GainCorr(tVis,xycorr)
c
c  Close up shop.
c
	if(doapply)call hisclose(tVis)
	call hclose(tVis)
	call hclose(tCal)
	end
c************************************************************************
	subroutine GainCorr(tVis,xyphase)
c
	implicit none
	integer tVis
	complex xyphase
c
c  Apply a gain term to the Y gains.
c
c  Input:
c    tVis	Handle of the data-set to apply the gain to.
c    xyphase	Gain term to be applied.
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex gains(3*MAXANT)
	integer itVis,ntau,nfeeds,ngains,nsols,offset,i,j,iostat
c
c  Externals.
c
	logical hdprsnt
c
	call rdhdi(tVis,'nsols',nsols,0)
	call rdhdi(tVis,'ngains',ngains,0)
	call rdhdi(tVis,'nfeeds',nfeeds,0)
	call rdhdi(tVis,'ntau',ntau,0)
	if(ngains.gt.3*MAXANT)call bug('f','Too many antennas')
c
c  If the required gains are not present, just give a warning.
c
	if(.not.hdprsnt(tVis,'gains').or.nfeeds.ne.2.or.ngains.le.0.or.
     *	   nsols.le.0)then
	  call bug('w','Cannot apply XY phase corrections')
	  call output(' ... Required gains are not pressent')
c
c  If the gains are present, then correct them.
c
	else
	  call haccess(tVis,itVis,'gains','append',iostat)
	  if(iostat.ne.0)call NormBug(iostat,'Error accessing gains')
	  offset = 16
	  do j=1,nsols
	    call hreadr(itVis,Gains,offset,8*ngains,iostat)
	    if(iostat.ne.0)
     *	      call NormBug(iostat,'Error reading gains item')
	    do i=1,ngains,nfeeds+ntau
	      Gains(i+1) = conjg(xyphase) * Gains(i+1)
	    enddo
	    call hwriter(itVis,Gains,offset,8*ngains,iostat)
	    if(iostat.ne.0)
     *	      call NormBug(iostat,'Error writing gains item')
	    offset = offset + 8*ngains + 8
	  enddo
	  call hdaccess(itVis,iostat)
	  if(iostat.ne.0)call NormBug(iostat,'Error closing gains item')
	endif
c
	end
c************************************************************************
	subroutine NormBug(iostat,message)
c
	implicit none
	integer iostat
	character message*(*)
c
c  Give an error message, and bugger off.
c------------------------------------------------------------------------
	call bug('w',message)
	call bugno('f',iostat)
	end
c************************************************************************
	subroutine PolComp(l1,l2,nants,xyphase,offset,error,
     *	  antflag,doxy,zerofl)
c
	implicit none
	integer nants
	complex xyphase,offset,l1(2,nants),l2(2,nants)
	real error
	logical doxy,antflag(nants),zerofl
c
c  Determine the best fit between two sets of polarisation leakage parameters,
c  and their rms difference.
c
c  Input:
c    nants	Number of antennas.
c    doxy	Do solve for xyphase error.
c    l1		Reference set of polarisation leakage parameters.
c    l2		Set of leakage parameters to be adjusted.
c    antflag	True if the antenna is to be used.
c    zerofl     If true, then discard an antenna if the leakages in at
c		least one of the tables is identically zero.
c  Output:
c    xyphase	XYphase offset between the two solutions.
c    offset	Offset term between the two solutions.
c    error	Rms difference between l1 and the corrected l2.
c------------------------------------------------------------------------
	integer i,nantd
	complex a,b,fg,f,g,temp
	real gg,t1,t2
	logical ok,ok2
c
c  Externals.
c
	character itoaf*9
c
	fg = 0
	f = 0
	g = 0
	gg = 0
	nantd = 0
c
	do i=1,nants
	  ok = antflag(i)
	  if(ok.and.zerofl)then
	    t1 = real(l1(1,i))**2 + aimag(l1(1,i))**2 + 
     *		 real(l1(2,i))**2 + aimag(l1(2,i))**2
	    t2 = real(l2(1,i))**2 + aimag(l2(1,i))**2 + 
     *		 real(l2(2,i))**2 + aimag(l2(2,i))**2
	    ok = t1.ne.0.and.t2.ne.0
	  endif
	  if(ok)then
	    f = f + l1(1,i) - conjg(l1(2,i))
	    g = g + l2(1,i) - conjg(l2(2,i))
	    fg = fg   + l1(1,i)*conjg(l2(1,i))
     *		      + conjg(l1(2,i))*l2(2,i)
	    gg = gg   + l2(1,i)*conjg(l2(1,i))
     *		      + l2(2,i)*conjg(l2(2,i))
	    nantd = nantd + 1
	  endif
	enddo
c
	call output('  Number of antennas selected: '//itoaf(nantd))
	if(nantd.le.1)call bug('f','Too few antennas selected')
c
	if(doxy)then
	  a = (2*nantd*fg - f*conjg(g)) /
     *		 (2*nantd*gg - real(g)**2 - aimag(g)**2)
	  a = a / abs(a)
	  g = a * g
	else
	  a = 1
	endif
	b = (f - g)/2/nantd
c
	error = 0
	do i=1,nants
	  t1 = real(l1(1,i))**2 + aimag(l1(1,i))**2 + 
     *	       real(l1(2,i))**2 + aimag(l1(2,i))**2
	  t2 = real(l2(1,i))**2 + aimag(l2(1,i))**2 + 
     *	       real(l2(2,i))**2 + aimag(l2(2,i))**2
	  ok = (t2.ne.0).or..not.zerofl
	  ok2 = (t1.ne.0.and.t2.ne.0).or..not.zerofl
	  temp = a*l2(1,i) + b
	  temp = l1(1,i) - temp
	  if(antflag(i).and.ok2)error = error + real(temp)**2
     *					      + aimag(temp)**2
	  temp = conjg(a)*l2(2,i) - conjg(b)
	  temp = l1(2,i) - temp
	  if(antflag(i).and.ok2)error = error + real(temp)**2
     *					      + aimag(temp)**2
	enddo
c
	xyphase = a
	offset = b
	error = sqrt(error/2/nantd)
	end
c************************************************************************
	subroutine LeakCorr(l2,nants,xyphase,offset,zerofl)
c
	implicit none
	integer nants
	logical zerofl
	complex l2(2,nants),xyphase,offset
c------------------------------------------------------------------------
	integer i
	real t2
	complex a,b
	logical ok
c
	a = xyphase
	b = offset
c
	do i=1,nants
	  t2 = real(l2(1,i))**2 + aimag(l2(1,i))**2 + 
     *	       real(l2(2,i))**2 + aimag(l2(2,i))**2
	  ok = (t2.ne.0).or..not.zerofl
	  if(ok)then
	    l2(1,i) = a*l2(1,i) + b
	    l2(2,i) = conjg(a)*l2(2,i) - conjg(b)
	  endif
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(doxy,doapply,zerofl,xyleak)
c
	implicit none
	logical doxy,doapply,zerofl,xyleak
c
c  Get extra processing options.
c
c  Output:
c    doxy	Calculate XY phase terms.
c    doapply	Apply all corrections.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=4)
	logical present(nopts)
	character opts(nopts)*8
	data opts/'noxy    ','apply   ','noflag  ','xyleak  '/
c
	call options('options',opts,present,nopts)
	doxy = .not.present(1)
	doapply = present(2)
	zerofl  = .not.present(3)
	xyleak  = present(4)
	if(xyleak.and..not.doxy)call bug('w',
     *	  'Using options=xyleak,noxy together makes no sense')
	if(xyleak.and..not.doapply)call bug('w',
     *	  'Using options=xyleak without options=apply makes no sense')
c
	end
