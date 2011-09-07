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
c	instrumental gain and atmospheric attenuation of the two datasets
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
c    rjs     21jan01 Change print format.
c  Bugs and Shortcomings:
c    * The xy phase is not applied to the polarisation solution.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	integer MAXSELS,MAXFBIN
	parameter(version='GpBoot: version 21-Jan-01')
	parameter(MAXSELS=256,MAXFBIN=16)
c
	character cal*64,vis*64,line*72
	real sels(MAXSELS)
	real VAmp(2,MAXANT,0:MAXFBIN),CAmp(2,MAXANT,0:MAXFBIN)
        real factor(0:MAXFBIN)
	integer VCNT(2,MAXANT,0:MAXFBIN),CCnt(2,MAXANT,0:MAXFBIN)
	integer iostat,tVis,tCal,ngains,nants,nfeedc,nfeedv,ntau
	integer temp,j,nfbin,nfbin1
        double precision freqc(MAXFBIN),freqv(MAXFBIN)
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
	call SumGains(tCal,nants,nfeedc,ntau,CAmp,CCnt,MAXANT,MAXFBIN,
     *                sels,.false.,freqc,nfbin)
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
	call SumGains(tVis,nants,nfeedv,ntau,VAmp,VCnt,MAXANT,MAXFBIN,
     *	              sels,.true.,freqv,nfbin1)
        if (nfbin.ne.nfbin1) then
          nfbin=0
          call bug('w','Number of freq bins in gain solutions differs,')
          call bug('w','using the continuum solution for scaling.')
        endif
c
c  Determine the scale factor to apply.
c
	call GetFac(nants,nfeedv,VAmp,VCnt,nfeedc,CAmp,CCnt,factor,
     *              MAXANT,MAXFBIN,nfbin)
c
c  Determine the gain to apply.
c
	do j = 1, nfbin1
          if (nfbin.eq.0) then
            factor(j)=factor(0)
          endif
	enddo
c
c  Now apply the correction.
c
	call Correct(tVis,nants,nfeedv,ntau,factor,freqc,freqv,
     *   nfbin,nfbin1,MAXFBIN)
c
c  Inform user, not appeasing Bob, who would rather the user
c  be kept bare foot and ignorant.
c
c  Write out some history now. Do not appease Neil -- just write it to
c  the 'vis' file. Neil would want it written to the 'cal' file, as
c  well as any other file we happened to find lying around.
c
	call hisopen(tVis,'append')
	call hiswrite(tVis,'GPBOOT: Miriad '//version)
	call hisinput(tVis,'GPBOOT')
        do j=0,nfbin
          if (j.eq.0) then
            write(line,'(a,f8.3)') 'Secondary flux density scaled by:',
     *                         factor(j)**2
          else
            write(line,'(a,i2,a,f8.3)') 'Frequency bin ',j,
     *                        ' scaled by      :', factor(j)**2
          endif
          call output(line)
          call hiswrite(tVis,'GPBOOT: '//line)
        enddo
	call hisclose(tVis)
c
c  Close up everything.
c
	call hclose(tVis)	
	call hclose(tCal)
	end
c************************************************************************
	subroutine SumGains(tVis,nants,nfeeds,ntau,Amp,Cnt,maxant,
     *	                    maxfbin,sels,doselect,freq,nfbin)
c
	implicit none
	integer tVis,nants,nfeeds,ntau,MAXANT,MAXFBIN,nfbin
	integer Cnt(2,MAXANT,0:MAXFBIN)
	real Amp(2,MAXANT,0:MAXFBIN),sels(*)
	logical doselect
        double precision freq(MAXFBIN)
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
c    Amp    The summed amplitudes. There are "nants*nfeeds" of them.
c    Cnt    The number of things that went into summing the amplitudes.
c------------------------------------------------------------------------
	complex Gains(3*MAXANT)
	integer item,nsols,i,j,iostat,offset,ngains,ant,n
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
	do i=1,nfeeds
          do j=1,nants
	    Cnt(i,j,0) = 0
	    Amp(i,j,0) = 0
	  enddo
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
	    ant = 0
	    do j=1,ngains,nfeeds+ntau
	      ant = ant + 1
	      if(doant(ant).and.
     *		 abs(real(Gains(j)))+abs(aimag(Gains(j))).gt.0)then
		Amp(1,ant,0) = Amp(1,ant,0) + abs(Gains(j))
		Cnt(1,ant,0) = Cnt(1,ant,0) + 1
	      endif
	      if(nfeeds.eq.2)then
	        if(doant(ant).and.
     *		  abs(real(Gains(j+1)))+abs(aimag(Gains(j+1))).gt.0)then
		  Amp(2,ant,0) = Amp(2,ant,0) + abs(Gains(j+1))
		  Cnt(2,ant,0) = Cnt(2,ant,0) + 1
		endif
	      endif
	    enddo
	  endif
	  offset = offset + 8*ngains
	enddo
c
	call hdaccess(item,iostat)
c
c  Now read the freq binned solutions, if any
c  
        call haccess(tVis,item,'gainsf','read',iostat)
        nfbin=0
        if (iostat.eq.0) then
          call hreadi(item,nfbin,4,4,iostat)
          if (iostat.ne.0) 
     *      call BootBug(iostat,'Error reading gainsf header')
          offset = 8
          do n=1,nfbin
            do j=1,nants
              do i=1,nfeeds
                Cnt(i,j,n)=0
                Amp(i,j,n)=0
              enddo
            enddo
            do i=1,nsols
              offset = offset + 8
              call hreadr(item,Gains,offset,8*ngains,iostat)
	       if(iostat.ne.0)
     *	      call BootBug(iostat,'Error reading gainsf file')
              ant = 0
              do j = 1, ngains, nfeeds+ntau
                ant = ant + 1
                if (doant(ant).and.abs(real(Gains(j)))+
     *                      abs(aimag(Gains(j))).ne.0) then
                  Amp(1,ant,n)=Amp(1,ant,n)+abs(Gains(j))
                  Cnt(1,ant,n)=Cnt(1,ant,n)+1
                endif
                if (nfeeds.eq.2) then
                  if (doant(ant).and.abs(real(Gains(j+1)))+
     *                      abs(aimag(Gains(j+1))).ne.0) then
                    Amp(2,ant,n)=Amp(2,ant,n)+abs(Gains(j+1))
                    Cnt(2,ant,n)=Cnt(2,ant,n)+1
                  endif
                endif  
              enddo
              offset = offset + ngains * 8
            enddo
            call hreadd(item,freq(n),offset,8,iostat)
            offset = offset + 8
          enddo       
          call hdaccess(item,iostat)
        endif 
	end
c************************************************************************
	subroutine GetFac(nants,nfeedv,VAmp,VCnt,nfeedc,CAmp,CCnt,
     *					factor,MAXANT,MAXFBIN,nfbin)
c
	implicit none
	integer nants,nfeedc,nfeedv,nfbin,maxfbin,maxant
	integer CCnt(2,MAXANT,0:MAXFBIN),VCnt(2,MAXANT,0:MAXFBIN)
	real    CAmp(2,MAXANT,0:MAXFBIN),VAmp(2,MAXANT,0:MAXFBIN)
        real factor(0:nfbin)
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
	double precision SumCC(0:MAXFBIN),SumVC(0:MAXFBIN)
	real x
	integer i,j,n
        character line*72
c
        do n=0,nfbin
	  SumCC(n) = 0
	  SumVC(n) = 0
c
c  Handle the case where the number of feeds in the primary and secondary
c  are the same.
c
	  if(nfeedv.eq.nfeedc)then
	    do j=1,nants
	      do i=1,nfeedv
	        if(VCnt(i,j,n).gt.0.and.CCnt(i,j,n).gt.0)then
	          x = CAmp(i,j,n) / CCnt(i,j,n)
	          SumCC(n) = SumCC(n) + VCnt(i,j,n)*x*x
	          SumVC(n) = SumVC(n) + VAmp(i,j,n)*x
	        endif
	      enddo
	    enddo
c
c  Handle the case where there is only one feed in the primary 
c   calibrator.
c
          else if(nfeedc.eq.1)then
	    do j=1,nants
	      do i=1,nfeedv
	        if(Vcnt(i,j,n).gt.0.and.CCnt(1,j,n).gt.0)then
		  x = CAmp(1,j,n) / CCnt(1,j,n)
		  SumCC(n) = SumCC(n) + VCnt(i,j,n)*x*x
		  SumVC(n) = SumVC(n) + VAmp(i,j,n)*x
	        endif
	      enddo
	    enddo
c
c  Handle the case where there is only one feed in the secondary 
c   calibrator.
c
	  else if(nfeedv.eq.1)then
	    do j=1,nants
	      do i=1,nfeedc
	        if(Vcnt(1,j,n).gt.0.and.CCnt(i,j,n).gt.0)then
		  x = CAmp(i,j,n) / CCnt(i,j,n)
		  SumCC(n) = SumCC(n) + VCnt(1,j,n)*x*x
		  SumVC(n) = SumVC(n) + VAmp(1,j,n)*x
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
	  if(SumVC(n).le.0) then
            if (n.eq.0) then
              call bug('w',
     *	          'There was no data to determine the scale factor')
            else
              write(line,'(A,I2)') 'No data to determine scale factor'//
     *          ' for bin ',n
              call bug('w',line)
            endif
            factor(n) = 1
          else
  	    factor(n) = SumCC(n) / SumVC(n)
          endif
        enddo
	end
c************************************************************************
	subroutine Correct(tVis,nants,nfeeds,ntau,Factor,freqc,freqv,
     *      nfbinc,nfbinv,MAXFBIN)
c
	implicit none
	integer tVis,nants,nfeeds,ntau,nfbinc,nfbinv,MAXFBIN
	real Factor(0:nfbinv)
        double precision freqc(nfbinc),freqv(nfbinv)
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
	integer item,nsols,i,j,iostat,offset,ngains,k,n
        real f(MAXFBIN)
        logical found
c
        ngains=nants*(nfeeds+ntau)
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
          k=0
	  do j=1,nants
            k=k+1
	    Gains(k) = Gains(k) * Factor(0)
            if (nfeeds.eq.2) then
              k=k+1
	      Gains(k) = Gains(k) * Factor(0)
            endif
            if (ntau.gt.0) k=k+1  
	  enddo
	  call hwriter(item,Gains,offset,8*ngains,iostat)
	  if(iostat.ne.0)
     *	    call BootBug(iostat,'Error writing gains file')
	  offset = offset + 8*ngains + 8
	enddo
c
	call hdaccess(item,iostat)
c
c  Now correct the freq binned data
c  Try to compensate for different bin centers by interpolating
c  between 'good' factors      
c
        if (nfbinc.gt.0) then
          do i=1,nfbinv
            found=.false.
            j=0
            do while (.not.found.and.(i-j.gt.1.or.i+j.lt.nfbinc))
              j = j + 1
              if (i+j.le.nfbinc) 
     *          found = freqc(i+j).gt.0.and.freqv(i+j).gt.0
              if (.not.found) then
                if (i-j.ge.1) then
                  found = freqc(i-j).gt.0.and.freqv(i-j).gt.0
                  if (found) j=-j
                endif
              endif
            enddo
            f(i) = factor(i)
            if (found) then
              j=i+j
              if (freqc(i).gt.0.and.freqv(i).gt.0) then
                f(i)=factor(i)+(freqv(i)-freqc(i))/(freqc(j)-freqc(i))*
     *            (factor(j)-factor(i))
              endif
            endif
          enddo
        endif
 
        if (nfbinv.le.1) return
        call haccess(tVis,item,'gainsf','append',iostat)
        if (iostat.eq.0) then
          offset = 8
          do n=1,nfbinv
            do i=1,nsols
              offset = offset + 8
              call hreadr(item,Gains,offset,8*ngains,iostat)
	      if(iostat.ne.0)
     *	        call BootBug(iostat,'Error reading gainsf file')
              k = 0
              do j = 1, nants
                k = k + 1
                if (nfbinc.eq.0) then
                  Gains(k) = Gains(k) * Factor(n)
                else
                  Gains(k) = Gains(k) * f(n)
                endif
                if (nfeeds.eq.2) then
                  k=k+1
                  if (nfbinc.eq.0) then
                    Gains(k) = Gains(k) * Factor(n) 
                  else
                    Gains(k) = Gains(k) *f(n)
                  endif
                endif
                if (ntau.gt.0) k=k+1 
              enddo
	      call hwriter(item,Gains,offset,8*ngains,iostat)
              offset = offset + ngains * 8
            enddo
            offset = offset + 8
          enddo       
          call hdaccess(item,iostat)
        endif      
        
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
