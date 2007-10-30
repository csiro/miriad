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
c@ options
c	Task enrichment parameters. Several can be given, separated by commas.
c	Minimum match is used.
c	  "noxy"     Do not solve for, or apply, the XY phase offset.
c	  "apply"    Apply the solved for corrections to the vis data-set.
c--
c  History:
c    rjs    28nov91 Original version.
c    rjs    17dec91 Allow number of leakages to differ between the two
c		    files.
c    rjs     4aug92 Handle new gains file possibility, and lack of xyphases
c		    item.
c    rjs    12oct93 noapply is now the default.
c
c  Bugs and Shortcomings:
c   * If the number of leakages in the "vis" file is greater than the 
c     cal file, and the leakages are being corrected, then the last ones
c     are not corrected!
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	character version*(*)
	parameter(version='GpNorm: version 1.0 12-Oct-93')
	logical doxy,doapply
	integer tCal,tVis,iostat,nLeaks,itVis,itCal,itemp
	character vis*64,cal*64,line*64
	complex CalLeak(2,MAXANT),VisLeak(2,MAXANT),xyphase,offset
	real error,arg
c
c  Externals.
c
	integer hsize
c
c  Get the inputs and check them.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keya('cal',cal,' ')
	call GetOpt(doxy,doapply)
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
c  Start history processing.
c
	if(doapply)then
	  call hisopen(tVis,'append')
	  call hiswrite(tVis,'GPNORM: Miriad '//version)
	  call hisinput(tVis,'GPNORM')
	endif
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
	call PolComp(CalLeak,VisLeak,nLeaks,xyphase,offset,error,
     *	  doxy,doapply)
c
c  Report on what we have found out, write it to the history.
c
	arg = atan2(aimag(xyphase),real(xyphase))
	write(line,'(a,f6.1)')'Xyphase (degrees): ',180/pi*arg
	call output(line)
	if(doapply)call hiswrite(tVis,'GPNORM: '//line)
	write(line,'(a,f6.3,a,f6.3,a)')'Offset: (',real (offset),',',
     *						   aimag(offset),')'
	call output(line)
	if(doapply)call hiswrite(tVis,'GPNORM: '//line)
	write(line,'(a,f7.4)')'Rms Error: ',error
	call output(line)
	if(doapply)call hiswrite(tVis,'GPNORM: '//line)
c
c  Write out the leakages if needed.
c
	if(doapply)call hwriter(itVis,VisLeak,8,16*nLeaks,iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error writing vis leakages')
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
	if(doxy.and.doapply)call GainCorr(tVis,xyphase)
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
	if(ngains.gt.3*MAXANT)call bug('f','Too many antennae')
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
	      Gains(i+1) = xyphase * Gains(i+1)
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
     *	  doxy,doapply)
c
	implicit none
	integer nants
	complex xyphase,offset,l1(2,nants),l2(2,nants)
	real error
	logical doxy,doapply
c
c  Determine the best fit between two sets of polarisation leakage parameters,
c  and their rms difference.
c
c  Input:
c    nants	Number of antennae.
c    doxy	Do solve for xyphase error.
c    doapply	Apply the corrections.
c    l1		Reference set of polarisation leakage parameters.
c  Input/Possibly Output:
c    l2		Set of leakage parameters to be adjusted.
c  Output:
c    xyphase	XYphase offset between the two solutions.
c    offset	Offset term between the two solutions.
c    error	Rms difference between l1 and the corrected l2.
c------------------------------------------------------------------------
	real pi
	parameter(pi=3.141592653589793)
c
	integer i
	complex a,b,fg,f,g,temp
	real gg
c
	fg = 0
	f = 0
	g = 0
	gg = 0
c
	do i=1,nants
	  f = f + l1(1,i) - conjg(l1(2,i))
	  g = g + l2(1,i) - conjg(l2(2,i))
	  fg = fg   + l1(1,i)*conjg(l2(1,i))
     *		    + conjg(l1(2,i))*l2(2,i)
	  gg = gg   + l2(1,i)*conjg(l2(1,i))
     *		    + l2(2,i)*conjg(l2(2,i))
	enddo
c
	if(doxy)then
	  a = (2*nants*fg - f*conjg(g)) /
     *		 (2*nants*gg - real(g)**2 - aimag(g)**2)
	  a = a / abs(a)
	  g = a * g
	else
	  a = 1
	endif
	b = (f - g)/2/nants
c
	error = 0
	do i=1,nants
	  temp = a*l2(1,i) + b
	  if(doapply) l2(1,i) = temp
	  temp = l1(1,i) - temp
	  error = error + real(temp)**2 + aimag(temp)**2
	  temp = conjg(a)*l2(2,i) - conjg(b)
	  if(doapply) l2(2,i) = temp
	  temp = l1(2,i) - temp
	  error = error + real(temp)**2 + aimag(temp)**2
	enddo
c
	xyphase = a
	offset = b
	error = sqrt(error/2/nants)
	end
c************************************************************************
	subroutine GetOpt(doxy,doapply)
c
	implicit none
	logical doxy,doapply
c
c  Get extra processing options.
c
c  Output:
c    doxy	Calculate XY phase terms.
c    doapply	Apply all corrections.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=2)
	logical present(nopts)
	character opts(nopts)*8
	data opts/'noxy    ','apply   '/
c
	call bug('w',
     *	  'The noapply option has changed -- see the help file')
	call options('options',opts,present,nopts)
	doxy = .not.present(1)
	doapply = present(2)
c
	end
