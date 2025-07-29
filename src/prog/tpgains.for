c********1*********2*********3*********4*********5*********6*********7**
	program TPGAINS
	implicit none
c
c= TPGAINS - Estimate antenna gains from total power measurements.
c& mchw
c: tpower, antenna gains
c	TPGAINS is a Miriad task to estimate antenna gains to correct
c	amplitude or phase which are correlated with total power. e.g. 
c	atmospheric phase fluctuations correlated with total power 
c	fluctuations. Antenna gain corrections are estimated from the
c	total power measurments and written into the gains item.
c@ vis
c	Name of input visibility data file. No default.
c@ again
c	`again' gives an antenna amplitude factor as a function
c		 again * sqrt(total_power/systemp)
c	Default value 0. gives no amplitude  change.
c@ rgain
c	`rgain' gives an antenna amplitude factor as a function
c		 rgain * sqrt(systemp/total_power)
c	Default value 0. gives no amplitude  change.
c@ pgain
c	`pgain' gives the antenna phase change with total power in 
c	degrees/Kelvin. Default value is 0. i.e. No phase change.
c	Typical value at 100 GHz is pgain = -280 degrees/K.
c@ scale
c	Scale total power to tsys [K]. Default no scaling applied.
c	Three values for each antenna: tpower = a + b*tpower + c*tpower**2
c	These values can be obtained by editing the log file from VARFIT
c	E.g. varfit vis=? xaxis=tpower refant=1 options=xscale log=varlog
c	The quadratic term is currently ignored.
c@ refant
c	Compute the antenna gains relative to refant.
c	The gain of refant is set to cmplx(1.,0.). Default=0. no refant.
c@ out
c       Name of output calibration file. The default is to write the gain
c       corrections into the input visibility file.
c--
c  History:
c    11dec90 mchw
c    25feb91 mjs  Changed references of itoa to itoaf.
c    04aug91 mjs  Replaced local maxants to use maxdim.h value MAXANT
c    20jul94 mchw Scale total power to tsys. Change pgain to degrees/K
c    15feb96 mchw Changed refpwr.
c    18mar96 mchw Scale total power to tsys = a + b*tpower + c*tpower**2
c    20dec96 mchw Scale total power to tpower(n) = (tpower(n)-a)/b
c    06aug97 mchw Added amplitude correction.
c    12aug97 mchw divide amplitude correction by nwide. Add 'rgain'.
c    13aug97 mchw change rgain to  sqrt(systemp/total_power)
c    14aug97 mchw change again to  sqrt(total_power/systemp)
c------------------------------------------------------------------------
	character version*(*),vis*80,out*80
	parameter(version='(version 1.0 14-AUG-97)')
	include	'maxdim.h'
	integer length,item,nscale,i,j,nwide
	complex gains(MAXANT)
	real tpower(MAXANT),tpower0(MAXANT),scale(3*MAXANT),refpwr
	real tsys(MAXANT),wsystemp(MAXANT*MAXWIDE),again,rgain,pgain
	double precision time,time0,interval
	integer refant,tvis,tgains,nants,nsols,iostat,offset,header(2)
	character obstype*32,type*1
	logical updated
c
c  Externals.
c
	character itoaf*4
	integer uvscan
	complex expi
c
c  Get input parameters.
c
	call output('TPGAINS '//version)
	call keyini
	call keyf('vis',vis,' ')
	call keyi('refant',refant,0)
	call keyr('again',again,0.)
	call keyr('rgain',rgain,0.)
	call keyr('pgain',pgain,0.)
	call mkeyr('scale',scale,3*MAXANT,nscale)
        if(mod(nscale,3).ne.0) call bug('f',
     *    'There must be three scale values for each antenna')
	call keya('out',out,' ')
	call keyfin
c
c  Open the uvdata file.
c
	call uvopen(tvis,vis,'old')
	if(vis.eq.' ') call bug('f','Input visibility file is missing')
	call rdhda(tvis,'obstype',obstype,'crosscorrelation')
	if(obstype(1:5).ne.'cross')
     *	  call bug('f','The vis file is not cross correlation data')
c
c  Check that nants and tpower are present.
c
	call uvprobvr(tvis,'tpower',type,length,updated)
	if(type.ne.'r') call bug('f','tpower is not in uvdata')
	call uvprobvr(tvis,'nants',type,length,updated)
	if(type.ne.'i') call bug('f','nants is not in uvdata')
	if(again.ne.0. .or. rgain.ne.0.)then
          call uvprobvr(tvis,'wsystemp',type,length,updated)
          if(type.ne.'r') call bug('f','wsystemp is not in uvdata')
	endif
c
c  Open the output file to contain the gain solutions. Start History.
c
	if(out.eq.' ')then
	  tgains = tvis
	  call HisOpen(tgains,'append')
	else
	  call hopen(tgains,out,'new',iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error opening output gains file '//out)
	    call bugno('f',iostat)
	  endif
	  call HisOpen(tgains,'write')
	endif
	call HisWrite(tgains,'TPGAINS: Miriad TPGains '//version)
	call HisInput(tgains,'TPGAINS')
c
c  Change units.
c
	pgain = pgain * 3.1415926 / 180.
c
c  Start the gains file.
c
	call haccess(tgains,item,'gains','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
	header(1) = 0
	header(2) = 0
	offset = 0
	call hwritei(item,header,offset,8,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
	offset = 8
c
c  Scan the uvdata looking for tpower and write the gains.
c
	nsols = 0
	do while(uvscan(tvis,'tpower').eq.0)
	  call uvrdvrd(tvis,'time',time,0.d0)
	  if(nsols.eq.0) then
	    time0 = time
  	    call uvrdvri(tvis,'nants',nants,0)
	    if(nants.eq.0)call bug('f','nants is zero')
	    if(nscale.ne.0.and.nscale.ne.3*nants)call bug('f',
     *    'There must be three scale values for each antenna')
	  endif
	  call uvgetvrr(tvis,'tpower',tpower,nants)
c
c  Get tsys for amplitude scaling.
c
	  if(again.ne.0. .or. rgain.ne.0.)then
	    call uvrdvri(tvis,'nwide',nwide,1)
            if(nwide.ne.0)then
              call uvgetvrr(tvis,'wsystemp',wsystemp,nants*nwide)
              do i=1,nants
                tsys(i) = 0.
                do j=1,nwide
                  tsys(i) = tsys(i) + wsystemp(i + (j-1)*nants)
                enddo
              enddo
	    else
	      call bug('f','nwide not in uvdata')
            endif
          endif
c
c  Get starting values.
c
	  if(nsols.eq.0) then
	    do i=1,nants
	      tpower0(i) = tpower(i)
	    enddo
	  endif
c
c  Scale tpower to scale.
c
	  if(nscale.ne.0)then
	    do i=1,nants
	      tpower(i) = ( tpower(i) - scale(3*i-2) ) / scale(3*i-1) 
	    enddo
	  endif
c
c  Compute gains relative to refant.
c
	  if(refant.ne.0.)then
	    refpwr = tpower(refant)
	    do i=1,nants
	      tpower(i) = tpower(i) - refpwr
	    enddo
	  endif
c
c  Write the gains file.
c
	  do i=1,nants
	    gains(i) = cmplx(1.,0.)
	  enddo
	  if(again.ne.0.)then
	    do i=1,nants
	      if(tsys(i).ne.0.)
     *	        gains(i) = again*sqrt(tpower(i)/tsys(i)*nwide)*gains(i)
	    enddo
	  endif
	  if(rgain.ne.0.)then
	    do i=1,nants
	      if(tpower(i).ne.0.)
     *	        gains(i) = rgain*sqrt(tsys(i)/nwide/tpower(i))*gains(i)
c********1*********2*********3*********4*********5*********6*********7**
	    enddo
	  endif
	  if(pgain.ne.0.)then
	    do i=1,nants
	      gains(i) = expi(pgain*(tpower(i)))*gains(i)
	    enddo
	  endif
	  call hwrited(item,time,offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0)call hwriter(item,gains,offset,8*nants,iostat)
	  if(iostat.ne.0)then
	    call bug('w','I/O error while writing to gains item')
	    call bugno('f',iostat)
	  endif
	  offset = offset + 8*nants
	  nsols = nsols + 1
	enddo
c
c  Write some header information for the gains file.
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing output gains item')
	  call bugno('f',iostat)
	endif
	if(nsols.gt.1) then
	  interval=(time-time0)/float(nsols-1)
	  call wrhdd(tgains,'interval',interval)
	  call wrhdi(tgains,'ngains',nants)
	  call wrhdi(tgains,'nsols',nsols)
	  call output('Number of solution intervals: '//itoaf(nSols))
	else
	  call bug('f','too few gains')
	endif
c
c  Close up.
c
	call HisClose(tgains)
	call uvclose(tvis)
	if(out.ne.' ') call hclose(tgains,iostat)
	end
c********1*********2*********3*********4*********5*********6*********7**
 
