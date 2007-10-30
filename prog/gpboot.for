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
c@ vis
c	The input visibility file, containing the gain file to correct.
c	The gains are assumed to be out by a constant factor.
c@ cal
c	This is a visibility data-set, which is assumed to contain a
c	gain table which scales the data to absolute flux units.
c@ select
c	Normal uv-selection parameter. This selects the gains in the
c	``vis'' that are compared against the ``cal'' gains (note that
c	all the ``cal'' gains are involved in the comparison). Currently
c	ONLY time and antenna selection are permitted. You will use
c	this parameter to select which data the instrumental/atmospheric
c	amplitude gains for ``vis'' are comparable to the observation in
c	``cal''.
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
c    rjs     17aug95 Antenna selection.
c    rjs     20may97 Print out the xy phasr that is being applied.
c		     Get the wraps right.
c    rjs      6feb98 Doc change oonly.
c    rjs     12oct99 Get rid of options=noxy.
c  Bugs and Shortcomings:
c    * The xy phase is not applied to the polarisation solution.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	integer MAXSELS
	parameter(version='GpBoot: version 12-Oct-99')
	parameter(MAXSELS=256)
c
	character cal*64,vis*64,line*72
	real sels(MAXSELS)
	real VAmp(2*MAXANT),CAmp(2*MAXANT),factor
	integer VCNT(2*MAXANT),CCnt(2*MAXANT)
	complex Gains(3*MAXANT)
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
	call SumGains(tCal,nants,nfeedc,ntau,CAmp,CCnt,sels,.false.)
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
	call SumGains(tVis,nants,nfeedv,ntau,VAmp,VCnt,
     *							sels,.true.)
c
c  Determine the scale factor to apply.
c
	call GetFac(nants,nfeedv,VAmp,VCnt,nfeedc,CAmp,CCnt,factor)
c
c  Determine the gain to apply.
c
	j = 1
	do i=1,ngains,nfeedv+ntau
	  Gains(i) = factor
	  if(nfeedv.gt.1)Gains(i+1) = factor
	  if(ntau.eq.1)Gains(i+nfeedv) = (1.,0.)
	  j = j + 1
	enddo
c
c  Now apply the correction.
c
	call Correct(tVis,ngains,Gains)
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
	integer item,nsols,i,j,k,iostat,offset,ngains,ant
	logical ok,dosel,doant(MAXANT)
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
	do i=1,nants
	  doant(i) = .true.
	enddo
c
	dosel = .false.
	if(doselect)then
	  dosel = SelProbe(sels,'time?',0.d0)
	  if(SelProbe(sels,'antennae?',0.d0))then
	    do i=1,nants
	      doant(i) = SelProbe(sels,'antennae',257.d0*i)
	    enddo
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
	    ant = 0
	    do j=1,ngains,nfeeds+ntau
	      ant = ant + 1
	      k = k + 1
	      if(doant(ant).and.
     *		 abs(real(Gains(j)))+abs(aimag(Gains(j))).gt.0)then
		Amp(k) = Amp(k) + abs(Gains(j))
		Cnt(k) = Cnt(k) + 1
	      endif
	      if(nfeeds.eq.2)then
	        k = k + 1
	        if(doant(ant).and.
     *		  abs(real(Gains(j+1)))+abs(aimag(Gains(j+1))).gt.0)then
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
