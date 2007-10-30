c************************************************************************
	program gpboot
	implicit none
c
c= GpBoot -- Correct the gains table of a visibility data-set.
c& rjs nebk
c: calibration
c+
c	GpBoot is a MIRIAD program which corrects a visibility data-set's
c	gain table.
c
c	GpBoot works by comparing the amplitudes in the gain table of one
c	data-set with those of another. The gains are assumed to differ
c	by a constant factor. It is assumed that the
c	instrumental gain and atmospheric attenuation of the two data-sets
c	were the same, and so the difference in gain is due to an
c	arbitrary or incorrect flux being specified when initially
c	determining the gains.
c
c	GpBoot will also correct for differences in the XY phase offsets
c	between the two data-sets, and copy across the polarization
c	leakage parameters.
c@ vis
c	The input visibility file, containing the gain file to correct.
c	The gains and XY phases are assumed to be out by a constant
c	factor.
c@ cal
c	This is a visibility data-set, which is assumed to contain a
c	gain table which scales the data to absolute flux units, and
c	contains correct XY phases.
c@ select
c	Normal uv-selection parameter. This selects the gains in the
c	``vis'' that are compared against the ``cal'' gains (note that
c	all the ``cal'' gains are involved in the comparison). Currently
c	ONLY time selection is permitted. You will use this parameter
c	to select which time(s) the instrumental/atmospheric amplitude
c	gains for ``vis'' are comparable to the observation in ``cal''.
c@ options
c	This gives task enrichment parameters. Several can be given,
c	separated by commas. Minimum match is used.
c	  "noxy"      Do not correct for XY phase differences. The
c	              default is to correct for this if the ``gains''
c	              item is present in both data-sets.
c	  "nocal"     Do not correct the flux scale. The default is to
c	              correct the flux scale if the ``gains'' item is
c	              present in both data-sets.
c--
c  History:
c    rjs     24jul91 Original version.
c    rjs     25jul91 Copied leakage table across, and some checks on the
c		     selection criteria.
c    nebk    23aug91 Inform user and add scale factor to history
c    rjs      4aug92 The xyphases item is now history. Do without it!
c    rjs     24sep93 Handle case of different number of feeds between
c		     primary and secondary.
c    rjs      5nov93 Do not copy polarisation solutions.
c
c  Bugs and Shortcomings:
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	integer MAXSELS
	parameter(version='GpBoot: version 25-Mar-94')
	parameter(MAXSELS=256)
c
	logical docal,doxy
	character cal*64,vis*64,line*72
	real sels(MAXSELS)
	real VAmp(2*MAXANT),CAmp(2*MAXANT),factor
	integer VCNT(2*MAXANT),CCnt(2*MAXANT)
	complex Gains(3*MAXANT),xyp(MAXANT)
	integer iostat,tVis,tCal,ngains,nants,nfeedc,nfeedv,ntau
	integer temp,i,j
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('cal',cal,' ')
	call keya('vis',vis,' ')
	call SelInput('select',sels,MAXSELS)
	call GetOpt(docal,doxy)
	call keyfin
	if(vis.eq.' '.or.cal.eq.' ')call bug('f',
     *	  'No calibrator or input vis file given')
c
c  Open the two files. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,vis,'old',iostat)
	if(iostat.ne.0)call BootBug(iostat,'Error opening '//vis)
	call hopen(tCal,cal,'old',iostat)
	if(iostat.ne.0)call BootBug(iostat,'Error opening '//cal)
c
c  Determine the number of feeds in the gain table.
c
	call rdhdi(tCal,'ngains',ngains,0)
	call rdhdi(tCal,'nfeeds',nfeedc,1)
	call rdhdi(tCal,'ntau',  ntau,  0)
	if(nfeedc.le.0.or.nfeedc.gt.2.or.mod(ngains,nfeedc+ntau).ne.0
     *	  .or.ntau.gt.1.or.ntau.lt.0)
     *	  call bug('f','Bad number of gains or feeds in '//cal)
	nants = ngains / (nfeedc + ntau)
	if(docal)call SumGains(tCal,nants,nfeedc,ntau,CAmp,CCnt,
     *							sels,.false.)
c
	call rdhdi(tVis,'nfeeds',nfeedv,1)
	if(nfeedv.ne.nfeedc)
     *	  call bug('w','Number of feeds differ for the two inputs')
	call rdhdi(tVis,'ngains',ngains,0)
	call rdhdi(tVis,'ntau',  ntau,  0)
	if(mod(ngains,nfeedv+ntau).ne.0)
     *	  call bug('f','Bad number of gains or feeds in '//vis)
	temp = ngains / (nfeedv + ntau)
	if(temp.ne.nants)
     *	  call bug('f','Number of antennae differ for the two inputs')
	if(docal)call SumGains(tVis,nants,nfeedv,ntau,VAmp,VCnt,
     *							sels,.true.)
c
c  Determine the scale factor to apply.
c
	if(docal)then
	  call GetFac(nants,nfeedv,VAmp,VCnt,nfeedc,CAmp,CCnt,factor)
	else
	  factor = 1
	endif
c
c  Get the xyphase offsets to apply, if appropriate.
c
	doxy = doxy.and.nfeedv.eq.2
	if(doxy)call ProcXY(tVis,tCal,xyp,nants,doxy)
c
c  Determine the gain to apply.
c
	j = 1
	do i=1,ngains,nfeedv+ntau
	  Gains(i) = cmplx(factor,0.)
	  if(doxy)then
	    Gains(i+1) = factor*xyp(j)
	  else if(nfeedv.eq.2)then
	    Gains(i+1) = cmplx(factor,0.)
	  endif
	  if(ntau.eq.1)Gains(i+nfeedv) = (1.,0.)
	  j = j + 1
	enddo
c
c  Now apply the correction.
c
	if(docal.or.doxy)call Correct(tVis,ngains,Gains)
c
c  Inform user, not appeasing Bob, who would rather the user
c  be kept bare foot and ignorant.
c
        write(line,'(a,f6.3)') 'Secondary flux density scaled by:',
     *                         factor**2
        call output(line)
c
c  Write out some history now. Do not appease Neil -- just write it to
c  the 'vis' file. Neil would want it written to the 'cal' file, as
c  well as any other file we happened to find lying around.
c
	call hisopen(tVis,'append')
	call hiswrite(tVis,'GPBOOT: Miriad '//version)
	call hisinput(tVis,'GPBOOT')
        call hiswrite(tVis,'GPBOOT: '//line)
	call hisclose(tVis)
c
c  Close up everything.
c
	call hclose(tVis)	
	call hclose(tCal)
	end
c************************************************************************
	subroutine SumGains(tVis,nants,nfeeds,ntau,Amp,Cnt,
     *						sels,doselect)
c
	implicit none
	integer tVis,nants,nfeeds,ntau
	integer Cnt(nants*nfeeds)
	real Amp(nants*nfeeds),sels(*)
	logical doselect
c
c  Sum up the gain amplitude, possibly over a selected time.
c
c  Input:
c    tVis
c    nants
c    nfeeds
c    ntau
c    sels
c    doselect
c  Output:
c    Amp	The summed amplitudes. There are "nants*nfeeds" of them.
c    Cnt	The number of things that went into summing the amplitudes.
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex Gains(3*MAXANT)
	integer item,nsols,i,j,k,iostat,offset,ngains
	logical ok,dosel
	double precision time
c
c  Externals.
c
	logical SelProbe
c
	ngains = nants*(nfeeds+ntau)
	if(ngains.gt.3*MAXANT)
     *	  call bug('f','Not enough space, in SumGains')
c
c  Check out the selection criteria.
c
	dosel = .false.
	if(doselect)then
	  dosel = SelProbe(sels,'time?',0.d0)
	  if(SelProbe(sels,'antennae?',0.d0))then
	    call bug('w','Antenna selection processing is unsupported')
	    call bug('w','Antenna selection ignored')
	  endif
	endif
c
	do i=1,nants*nfeeds
	  Cnt(i) = 0
	  Amp(i) = 0
	enddo
	call haccess(tVis,item,'gains','read',iostat)
	if(iostat.ne.0)call BootBug(iostat,'Error opening gains file')
	call rdhdi(tVis,'nsols',nsols,0)
	if(nsols.le.0)
     *	  call bug('f','Bad number of gain solutions for gains file')
c
c  Now read through and accumulate.
c
	offset = 8
	do i=1,nsols
	  call hreadd(item,time,offset,8,iostat)
	  offset = offset + 8
	  if(iostat.ne.0)call BootBug(iostat,'Error reading gains file')
	  ok = .not.dosel
	  if(.not.ok)ok = SelProbe(sels,'time',time)
c
	  if(ok)then
	    call hreadr(item,Gains,offset,8*ngains,iostat)
	    if(iostat.ne.0)
     *	      call BootBug(iostat,'Error reading gains file')
	    k = 0
	    do j=1,ngains,nfeeds+ntau
	      k = k + 1
	      if(abs(real(Gains(j)))+abs(aimag(Gains(j))).gt.0)then
		Amp(k) = Amp(k) + abs(Gains(j))
		Cnt(k) = Cnt(k) + 1
	      endif
	      if(nfeeds.eq.2)then
	        k = k + 1
	        if(abs(real(Gains(j+1)))+abs(aimag(Gains(j+1))).gt.0)
     *								   then
		  Amp(k) = Amp(k) + abs(Gains(j+1))
		  Cnt(k) = Cnt(k) + 1
		endif
	      endif
	    enddo
	  endif
	  offset = offset + 8*ngains
	enddo
c
	call hdaccess(item,iostat)
	end
c************************************************************************
	subroutine GetFac(nants,nfeedv,VAmp,VCnt,nfeedc,CAmp,CCnt,
     *								factor)
c
	implicit none
	integer nants,nfeedc,nfeedv
	integer CCnt(nfeedc,nants),VCnt(nfeedv,nants)
	real    CAmp(nfeedc,nants),VAmp(nfeedv,nants),factor
c
c  Determine the scale factor, given the summed amplitudes. That it,
c  finds a scale factor such that
c
c    C = factor * V
c
c  Input:
c    ngains	The number of gains.
c    VAmp	The summed amplitudes for V.
c    VCnt	Number of amplitudes that went into forming each sum.
c    CAmp)	Similar to the above.
c    CCnt)
c  Output:
c    factor	The result.
c------------------------------------------------------------------------
	double precision SumCC,SumVC
	real x
	integer i,j
c
	SumCC = 0
	SumVC = 0
c
c  Handle the case where the number of feeds in the primary and secondary
c  are the same.
c
	if(nfeedv.eq.nfeedc)then
	  do j=1,nants
	    do i=1,nfeedv
	      if(VCnt(i,j).gt.0.and.CCnt(i,j).gt.0)then
	        x = CAmp(i,j) / CCnt(i,j)
	        SumCC = SumCC + VCnt(i,j)*x*x
	        SumVC = SumVC + VAmp(i,j)*x
	      endif
	    enddo
	  enddo
c
c  Handle the case where there is only one feed in the primary calibrator.
c
	else if(nfeedc.eq.1)then
	  do j=1,nants
	    do i=1,nfeedv
	      if(Vcnt(i,j).gt.0.and.CCnt(1,j).gt.0)then
		x = CAmp(1,j) / CCnt(1,j)
		SumCC = SumCC + VCnt(i,j)*x*x
		SumVC = SumVC + VAmp(i,j)*x
	      endif
	    enddo
	  enddo
c
c  Handle the case where there is only one feed in the secondary calibrator.
c
	else if(nfeedv.eq.1)then
	  do j=1,nants
	    do i=1,nfeedc
	      if(Vcnt(1,j).gt.0.and.CCnt(i,j).gt.0)then
		x = CAmp(i,j) / CCnt(i,j)
		SumCC = SumCC + VCnt(1,j)*x*x
		SumVC = SumVC + VAmp(1,j)*x
	      endif
	    enddo
	  enddo
c
c  I cannot imagine that we should ever get here!
c
	else
	  call bug('f','I am confused about the number of feeds')
	endif
c
	if(SumVC.le.0) call bug('f',
     *	  'There was no data to determine the scale factor')
	factor = SumCC / SumVC
	end
c************************************************************************
	subroutine Correct(tVis,ngains,Factor)
c
	implicit none
	integer tVis,ngains
	complex Factor(ngains)
c
c  Apply the gain corrections to the gain file.
c
c  Input:
c    tVis	Handle of the input data-set.
c    ngains	Number of gains.
c    Factor	The gain factor to be applied.
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex Gains(3*MAXANT)
	integer item,nsols,i,j,iostat,offset
c
	if(ngains.gt.3*MAXANT)
     *	  call bug('f','Not enough space, in SumGains')
	call haccess(tVis,item,'gains','append',iostat)
	if(iostat.ne.0)call BootBug(iostat,'Error opening gains file')
	call rdhdi(tVis,'nsols',nsols,0)
	if(nsols.le.0)
     *	  call bug('f','Bad number of gain solutions for gains file')
c
c  Now correct the data.
c
	offset = 16
	do i=1,nsols
	  call hreadr(item,Gains,offset,8*ngains,iostat)
	  if(iostat.ne.0)
     *	    call BootBug(iostat,'Error reading gains file')
	  do j=1,ngains
	    Gains(j) = Gains(j) * Factor(j)
	  enddo
	  call hwriter(item,Gains,offset,8*ngains,iostat)
	  if(iostat.ne.0)
     *	    call BootBug(iostat,'Error writing gains file')
	  offset = offset + 8*ngains + 8
	enddo
c
	call hdaccess(item,iostat)
	end
c************************************************************************
	subroutine BootBug(iostat,message)
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
	subroutine ProcXY(tVis,tCal,xyp,nants,ok)
c
	implicit none
	integer tVis,tCal,nants
	complex xyp(nants)
	logical ok
c
c  Determine the XYphase offsets to apply to each antenna in the
c  output.
c
c  Input:
c    tVis
c    tCal
c    nants
c  Output:
c    ok
c    xyp
c------------------------------------------------------------------------
	include 'maxdim.h'
	real tol
	parameter(tol = 0.075)
	real Vphase(MAXANT),Vsd(MAXANT),Cphase(MAXANT),Csd(MAXANT)
	integer Vcnt(MAXANT),Ccnt(MAXANT),Vnants,Cnants
	real theta
	integer Count,n,i
c
c  Get the xyphases for the two files.
c
	call GetXY(tVis,Vphase,Vsd,Vcnt,MAXANT,Vnants)
	call GetXY(tCal,Cphase,Csd,Ccnt,MAXANT,Cnants)
c
c
c  If alls OK, average those XYphase errors where the XY phase appears
c  to be constant (has a standard deviation of less than "tol" radians.
c
	ok = Vnants.eq.nants.and.Cnants.eq.nants
	if(ok)then
	  theta = 0
	  Count = 0
	  do i=1,nants
	    n = min(Vcnt(i),Ccnt(i))
	    if(Vsd(i).lt.tol.and.Csd(i).lt.tol.and.n.gt.0)then
	      theta = theta + n*(Cphase(i)-Vphase(i))
	      Count = Count + n
	    endif
	  enddo
	  ok = Count.gt.0
	  if(ok)then
	    theta = theta / Count
	    xyp(1) = cmplx(cos(theta),-sin(theta))
	    do i=2,nants
	      xyp(i) = xyp(1)
	    enddo
	  endif	
	endif
	if(.not.ok) call bug('w',
     *	    'Unable to determine XY phases for vis and/or cal')
c
	end
c************************************************************************
	subroutine GetOpt(docal,doxy)
c
	implicit none
	logical docal,doxy
c
c  Get "Task Enrichment Parameters".
c
c  Output:
c    doxy	Do not correct XY phase.
c    docal	Do not correct absolute flux levels.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=2)
	logical present(nopts)
	character opts(nopts)*8
	data opts/'noxy    ','nocal   '/
c
	call options('options',opts,present,nopts)
	doxy = .not.present(1)
	docal = .not.present(2)
c
	if(.not.doxy.and..not.docal)call bug('f',
     *	  'Options prevent anything being done')
	end

