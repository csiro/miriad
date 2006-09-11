c************************************************************************
	program gpcomb
	implicit none
c
c= GpComb -- Combine multiple polarization calibration solutions.
c& dpr
c: calibration
c+
c
c       GPCOMB combines the leakage tables and xyphase solutions from
c       multiple calibrators. The routine is intended to be used when
c       attempting high-precision circular polarization observations at
c       the ATCA. Because leakages and xyphases are treated as invariant
c       during circular polarization calibration, a more accurate
c       calibration may be obtained by averaging several solutions from
c       several "leakage calibrators".
c	
c       Notes:
c
c	GPCOMB will probably be less useful for linear polarization
c	observations, because linear polarization observations are 
c	usually limited by the xygain amplitudes, not the leakages
c	or xyphases.
c       
c       If the calibration solutions you are comparing had the circular
c       polarization zero-point determined from different observations
c       of 1934-638 (eg. on different days), then there may be an
c       xy-phase offset hidden in the bandpass, and so averaging the
c       leakages and xyphases would probably be a bad idea. Maybe
c       try using the same bandpass for both...
c
c       And finally, if you have consistent systematic errors in your
c       solutions (eg. you haven't used xyref,polref in gpcal), then no
c       amount of averaging is going to help.
c
c       References:
c
c       For an overview of high-precision circular polarization
c       calibration with the ATCA, see 
c
c       Rayner, "Circular Polarization User's Guide", ATNF Technical
c       Document Series, 2000, 39.3/102,
c       http://www.atnf.csiro.au/people/drayner/Publications/
c       index.html#Circular_Polarization_Users_Guide
c
c       For thorough treatment of the issues, see 
c
c       Sault, Hamaker and Bregman, "Understanding radio polarimetry
c       II. Instrumental calibration of an interferometer array", A&AS,
c       1996, v117, pp149-159.
c       ftp://ftp.atnf.csiro.au/pub/people/rsault/papers/polar2.ps.gz
c	
c@ vis
c	The visibility data-set to which the average leakages and
c	xyphases will be written. If omitted, the averaged
c       leakage values are still reported to screen.
c
c       Existing calibration tables are not applied to vis ie. the
c       resultant calibration and leakage tables in vis are REPLACED by
c       those from the cals. This is what you would usually want to
c       do. If not, apply the appropriate calibration first with uvaver.
c
c       The corrected dataset does retain the original gain 
c       amplitude and the antenna-based component of the gain phase.
c@ cal
c	The data-sets containing the nominally correct polarization 
c       calibration. No default. May include vis.
c@ select
c	Normal uv selection. Only antenna-based selection is supported.
c       If an existing leakage/xyphase solution exists for a de-selected
c       antenna in vis, the value will be preserved. Otherwise,
c       it will be set to zero.
c@ options
c	Task enrichment parameters. Several can be given, 
c       separated by commas. Minimum match is used.
c	  "noxy"     Do not solve for, or apply, the average XY phase.
c         "nopol"    Do not solve for, or apply, average leakages.
c--
c  History:
c    dpr    27apr01 original version started - was it that long ago?!?
c
c  Bugs and Shortcomings:
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	include 'gpcomb.h'
	character version*(*)
	parameter(version='GpComb: version 1.0 27-Apr-01')
c
c  options flags
	logical doxy,dopol,dopass
c  Internal function flags:
c  Apply the averaged calibration to vis
	logical doapply
	character vis*64
c  input cal file names, and index counter, index variable
	character cals(MAXCALS)*64
	integer   calindex
	character cal*64
	integer ncals
c  selection, and antenna selection array
	integer MAXSELS
	parameter(MAXSELS=100)
	real sels(MAXSELS)
	logical antflag(MAXANT)
c  the accumulated leakages, which are averaged after accumulation,
c  and the number of leakages accumulated. Note nleaksolns .gt. 0
c  implies dopol
	complex leakages(2,MAXANT)
	integer nleaksolns
c  the number of antennas with leakages in the first file opened,
c  used for a consistency check
        integer nleaks, prevnleaks
c
c  vis filehandle
        integer tVis
c
c  accumulated xyphases, number of valid gain solution tables
	complex    xyphases(MAXANT)
	integer    nxysolns
c  and num antennas with xyphase, and consistency check
	integer nxy,prevnxy
c
	integer i,iostat
c
c  Externals.
c
	logical selProbe
c
c  Get the inputs and check them.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call mkeya('cal',cals,MAXCALS,ncals)
	call GetOpt(doxy,dopass,dopol)
	call selInput('select',sels,MAXSELS)
	call keyfin
c
c  Check minimum requirements to start
c
	if(.not. (doxy.or.dopol)) 
     -     call bug('f','Nothing to solve for')
	if(ncals.eq.0)call bug('f',
     -     'You must specify at least one cal file')
c
c  Check minimum requrements to apply averages
c
	doapply=.false.
	if(vis.ne.' ') doapply=.true.
c
c  find which antennas the user has selected
c
	do i=1,MAXANT
	  antflag(i) = selProbe(sels,'antennae',dble(257*i))
	enddo
c
c  Initialize counters, set accumulators to zer0
c
	nleaks=0
	nxy=0
c
	nleaksolns=0
	nxysolns=0
c
	prevnleaks=0	
	prevnxy=0
c
	call InitComplex(leakages,2*MAXANT,cmplx(0.0))
	call InitComplex(xyphases,MAXANT,cmplx(0.0))
c
c  PROCESS:
c
c  Now loop through all the cals, accumulating the leakages,
c  xyphases and bandpass (as required).
c  We'll average/normalize them at the end.
c
	do calindex=1,ncals
	  cal=cals(calindex)
c
c  Leakages
c         
	  if (dopol) then
	    call AccLeakages(cal,leakages,nleaksolns,nleaks,antflag)
c           check we have compatible leakage tables
c           
	    if ((calindex .ne. 1) .and. (nleaks .ne. prevnleaks))
     - call bug('f','Inconsistent number of leakages in '//cal)
	    prevnleaks=nleaks
	  end if
c
c  XY phases
c
	  if (doxy) then
           call AccXY(cal,xyphases,nxysolns,nxy,antflag)
c           check we have compatible num gains (ie antennas)
c
	    if ((calindex .ne. 1) .and. (nxy .ne. prevnxy))
     - call bug('f','Inconsistent number of antennas in '//cal)
	    prevnxy=nxy

	  end if
	end do
c
c  Take the averages/normalize
c
	if (nleaksolns.gt.0) then
	  call TimesLeakages(leakages,nleaks,1.0/cmplx(nleaksolns))
	end if
c
	do calindex=1,nxy
	  if (abs(xyphases(calindex)).gt.0.0) 
     *  xyphases(calindex)=xyphases(calindex)/abs(xyphases(calindex))
	end do

c  
c  Ok!
c  If required, replace the xyphase, leakages and bandpass of vis.
c
	if (doapply) then
c
c  Open the file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	  call hopen(tVis,vis,'old',iostat)
	  if(iostat.ne.0)call NormBug(iostat,'Error opening '//vis)
	  
c         Start history processing
c         
	  call hisopen(tVis,'append')
	  call hiswrite(tVis,'GPCOMB: Miriad '//version)
	  call hisinput(tVis,'GPCOMB')

c         Apply leakages, say what we did
	  if (dopol.and.(nleaksolns.gt.0)) then
	    call WriteLeakages(vis,leakages,nleaks,
     *          antflag,tVis)
	    call ReportLeakages(leakages,nleaks,tVis)
	  end if

c         Apply xyphase, say what we did
	  if (doxy.and.(nxysolns.gt.0)) then
	    call WriteXY(vis,xyphases,nxy,antflag,tVis)
	    call ReportXY(xyphases,nxy,tVis)
	  end if
c
c         Close up.
c
	  call hisclose(tVis)
	  call hclose(tVis)
	else
c         we're not applying; just write to screen
	  call ReportLeakages(leakages,nleaks,-1)
	  call ReportXY(xyphases,nxy,-1)

	end if
	end
c************************************************************************
	subroutine NormBug(iostat,message)
c
	implicit none
	integer iostat
	character message*(*)
c
c  Give an error message, and bugger off.
c  Input:
c    iostat - last error message.
c    message - say what the context was.
c------------------------------------------------------------------------
	call bug('w',message)
	call bugno('f',iostat)
	end
c************************************************************************
	subroutine AccLeakages(cal,leakages,nsolns,nleaks,antflag)
c
	implicit none
	include 'maxdim.h'
	character cal*64
	complex   leakages(2,MAXANT)
	integer   nsolns
	integer   nleaks
	logical   antflag(MAXANT)
c
c  Add the leakages from file cal to those in leakages
c  Input:
c    cal - cal file name
c    antflag - antenna flagging info, true === use it.
c  Input/Output:
c    leakages
c    nsolns - number of cals which had good leakages. 
c             Incremented if successful.
c  Output:
c    nleaks - number of antennas with leakage solns
c------------------------------------------------------------------------
c       For reading in the cal leakage table
	complex calleaks(2,MAXANT)
	integer tCal,iostat,itCal,i
c
c  Externals.
c
	integer hsize

c
c  Open the file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tCal,cal,'old',iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error opening '//cal)
c
	call haccess(tCal,itCal,'leakage','read',iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error opening cal leakages')
	nleaks = (hsize(itCal)-8)/16
	if(nleaks.lt.2)
     *	  call bug('f','Bad number of leakages in cal file')
c
c
c  Read in the leakages.
c
	call hreadr(itCal,calleaks,8,16*nLeaks,iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error reading cal leakages')

	do i=1,nLeaks
	  if (antflag(i)) then
	    leakages(1,i)=leakages(1,i)+calleaks(1,i)
	    leakages(2,i)=leakages(2,i)+calleaks(2,i)
	  end if
	end do

	nsolns=nsolns+1

c  Close up
	call hdaccess(itCal,iostat)
	if(iostat.ne.0)call NormBug(iostat,
     *    'Errror closing '//cal//' leakages')
	call hclose(tCal)

	end
c************************************************************************
	subroutine TimesLeakages(leakages,nants,factor)
c
	implicit none
	include 'maxdim.h'
	complex   leakages(2,MAXANT)
	integer   nants
	complex   factor
c
c  multiply all leakages by factor
c  Input:
c    nants - number of antennas
c    factor - to multiply by
c  Input/Output:
c    leakages
c------------------------------------------------------------------------
	integer i

	do i=1,nants
	  leakages(1,i)=leakages(1,i)*factor
	  leakages(2,i)=leakages(2,i)*factor
	end do

	end
c************************************************************************
	subroutine InitComplex(leakages,n,factor)
c
	implicit none
	integer   n
	complex   leakages(n)
	complex   factor
c
c  Init an array of complex to be factor
c  Input:
c    n - number of elements
c    factor - to multiply by
c  Output:
c    leakages - output array
c------------------------------------------------------------------------
	integer i

	do i=1,n
	  leakages(i)=factor
	end do
	end
c************************************************************************
	subroutine ReportLeakages(D,nants,tVis)
c
	implicit none
	include 'maxdim.h'
	complex   D(2,MAXANT)
	integer   nants
	integer   tVis
c
c  print the leakages, and write to history file if required
c  Input:
c       D - leakages
c       nants - number of antennas
c       tVis   - history file handle -1 for screen only
c------------------------------------------------------------------------
	integer j
	character  line*80

	call writeo(tVis,'Average Leakage terms:')
          do j=1,nants
            write(line,'(1x,a,i2,a,f7.4,a,f7.4,a,f7.4,a,f7.4,a)')
     *        'Ant',j,':Dx,Dy = (',real(D(1,j)),',',aimag(D(1,j)),'),(',
     *                             real(D(2,j)),',',aimag(D(2,j)),')'
            call writeo(tVis,line)
	end do

	end
c************************************************************************
	subroutine WriteLeakages(vis,leakages,nants,antflag,tVis)
c
	implicit none
	include 'maxdim.h'
	character vis*64
	integer   nants
	complex   leakages(2,nants)
	logical   antflag(nants)
	integer   tVis
c  Write out the leakage table. Existing leakages, if they exist,
c  are retained for antennas which user deselected.
c  Input:
c    vis - file to write leakages to.
c    leakages 
c    nants
c    antflag - good antennas == .true.
c    tVis - vis file handle.
c------------------------------------------------------------------------
c       vis leakages file handle
	integer   itVis
	integer   iostat
	integer   i
	integer   nOldLeaks
	complex   oldleaks(2,MAXANT)
c
c  Externals
c
	integer   hsize
c
c  First, we read in any existing leakage table
c  This is because some antennas might not be
c  selected. 
c
c  open leakages
	call haccess(tVis,itVis,'leakage','read',iostat)
	if(iostat.eq.0) then
c
c  there is a pre-existing leakage table
	  noldleaks = (hsize(itVis)-8)/16
c
	  call hreadr(itVis,oldleaks,8,16*nOldLeaks,iostat)
	  if(iostat.ne.0) then
	    call Bug('w',
     - 'Error reading existing leakage table - ignoring')
	  else 
c
c           Read in the leakages for the ones which weren't selected
	    do i=1,nants
	      if (.not.antflag(i)) then
		leakages(1,i)=oldleaks(1,i)
		leakages(2,i)=oldleaks(2,i)
	      end if
	    end do
	  endif   ! iostat.ne.0
c
c  Close up the leakages, so we can open again in append mode
	  call hdaccess(itVis,iostat)
	  if(iostat.ne.0)call NormBug(iostat,
     *     'Errror closing old leakage table')
	end if   ! iostat.ne.0

c
c  Write out the leakages. This is verbatim copy from GPCAL,
c  which comes with the following disclaimer:
c
c  Save the polarisation gains in an item in the calibrator file.
c  This uses some dirty tricks. Firstly it uses wrhdc to create the
c  item (because there is no way in FORTRAN to make the correct header).
c  Second it uses hwriter, because there is no hwritec.
c
c
c
        call wrhdc(tVis,'leakage',cmplx(1.0,0.0))
        call haccess(tVis,itVis,'leakage','append',iostat)
        if(iostat.ne.0)then
          call bug('w','Error opening the output leakage table')
          call bugno('f',iostat)
        endif
c
c  The item alignment requirement requires that we start writing at
c  byte 8. Bytes 0-3 are the header, and bytes 4-7 are unused.
c
        call hwriter(itVis,leakages,8,8*2*nants,iostat)
        if(iostat.ne.0)then
          call bug('w','Error writing to the leakage table')
          call bugno('f',iostat)
        endif
        call hdaccess(itVis,iostat)
        if(iostat.ne.0)call bugno('f',iostat)

	end 
c************************************************************************
	subroutine AccXY(cal,xyphases,nxysolns,ngains,antflag)
c
	implicit none
	include 'maxdim.h'
	include 'mirconst.h'
	character cal*64
	complex    xyphases(MAXANT)
	integer    nxysolns
	integer    ngains
	logical    antflag(MAXANT)
c
c       Accumulate xyphase solutions from cal
c       Input:
c         cal     - vis file to open and process
c         antflag - antenna selection use == .true.
c       Input/Output:
c         xyphases - normalized xyphases from cal added to these.
c         nxysolns - number of cals successfully added to xyphases.
c                    Incremented on success
c       Output:
c         ngains   - number of antennas which have xyphases
c------------------------------------------------------------------------
c       temp arr for reading in data
	complex   gains(3*MAXANT)
c       file handles etc.
	integer   itVis,tVis,iostat,i,ntau,nfeeds
c       number of gain solutions for this cal
	integer   nsols
c       function return array for converting gains -> xygains
	complex   calXYGains(MAXANT)
c       accumulated xyphase for this cal
	complex   accCalXYGains(MAXANT)
c       index counter
        integer   ant
c       byte offset for stepping through gains
        integer   offset
c       
        integer   j
c
c  Externals.
c
	logical hdprsnt
c	real    GetPhasW
c
c  Open the file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,cal,'old',iostat)
	if(iostat.ne.0)call NormBug(iostat,'Error opening '//cal)
c
c  Open the gains
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
	  call bug('w','Cannot determine XY phases for '//cal)
	  call output(' ... Required gains are not pressent')
	else
c         If the gains are present, then open them and accumulate.
c 
c         Set ngains equivalent nants
	  ngains=nint(real(ngains)/real(nfeeds+ntau))
	  call InitComplex(accCalXYGains,MAXANT,cmplx(0.0))

	  call haccess(tVis,itVis,'gains','read',iostat)
	  if(iostat.ne.0)call NormBug(iostat, 
     *      'Error accessing gains for '//cal)
	  offset = 16
	  do j=1,nsols
	    call hreadr(itVis,Gains,offset,8*(ngains*nfeeds),iostat)
	    if(iostat.ne.0)
     *	      call NormBug(iostat,'Error reading gains item')
	    ant=1
	    do i=1,ngains
	      call XYCvt(Gains,calXYGains,nfeeds,ntau,ngains,.true.)
	      accCalXYGains(ant) = accCalXYGains(ant) + calXYGains(ant)
	      ant=ant+1
	    enddo
	    offset = offset + 8*(ngains*nfeeds) + 8
	  enddo
	  call hdaccess(itVis,iostat)
	  if(iostat.ne.0)call NormBug(iostat,'Error closing gains item')
c
c  accumulate with the other xyphase solutions
c
	  do ant=1,ngains
	    if(antflag(ant)) xyphases(ant) = xyphases(ant) + 
     *        accCalXYGains(ant)/abs(accCalXYGains(ant))
c	      write(*,*) ant, GetPhasW(accCalXYGains(ant))
	  end do
c
	nxysolns=nxysolns+1
c
	end if
c  Close up
	call hclose(tVis)
	end
c***********************************************************************
	subroutine ReportXY(xyphases,nxy,tVis)
c
	implicit none
	complex xyphases(*)
	integer nxy
	integer tVis
c
c       Write the derived xyphases to screen and file.
c       Input:
c         xyphases
c         nxy - number of antennas for which there is valid xyphase
c         tVis- file-handle, presumably of history file
c              -1 for screen only
c-----------------------------------------------------------------------
	integer i
	character  line*80
c
c Externals
c
	real  GetPhasW

	call writeo(tVis,'Average XY-phases:')

	write(line,'(1x,a,i2,a,i2,a,7f7.2)')'Xyphase(',1,'-',nxy,
     *        ') = ',(GetPhasW(xyphases(i)),i=1,nxy)

	call writeo(tVis,line)

	end
c************************************************************************
	subroutine WriteXY(vis,xyphases,nxy,antflag,tVis)
c
	implicit none
	include 'maxdim.h'
	character vis*64
	complex   xyphases(MAXANT)
	integer   nxy
	logical   antflag(MAXANT)
	integer   tVis
c  Change the gains of vis to have xyphases.
c  This does a simple-minded replace - the xyphases will
c  be time independent after this!!
c
c  Would be good to provide an option to compute offsets
c  to the average, and apply them instead.
c
c  If an antenna is not selected, but there is an existing
c  gains table, then use the old xyphase. But still makes
c  it time-invariant! ie sets it to the average of the old.
c
c  Input:
c    vis
c    xyphases
c    nxy - number of antennas
c    antflag - true if xyphases to be used for that antenna 
c    tVis - vis file handle
c------------------------------------------------------------------------
c       vis gains file handle
	integer   itVis
	integer   iostat
	integer   i
c       If there is are old xyphases, accumulate them into this:
	complex   accCalXYGains(MAXANT)
c       temp arr for reading in old gains
	complex   gains(3*MAXANT)
c       function return array for converting gains -> xygains
	complex   calXYGains(MAXANT)
c       gain header info
	integer   nsols,ngains,nfeeds,ntau,nant
	integer   offset,j,ant
c
c  Externals
c
	logical   hdprsnt
c	real      GetPhasW
c
c  First, we read in the existing xyphases.
c  This is because some antennas might not be
c  selected. 
c

c  Get gain info
	call rdhdi(tVis,'nsols',nsols,0)
	call rdhdi(tVis,'ngains',ngains,0)
	call rdhdi(tVis,'nfeeds',nfeeds,0)
	call rdhdi(tVis,'ntau',ntau,0)
	if(ngains.gt.3*MAXANT)call bug('f','Too many antennae')

	if(hdprsnt(tVis,'gains').and.nfeeds.ge.2.and.ngains.gt.0.and.
     *	   nsols.gt.0)then
c
c  there are pre-existing gains. Read them in.
c
c         Set ngains equivalent nants
	  nant=nint(real(ngains)/real(nfeeds+ntau))

	  call InitComplex(accCalXYGains,MAXANT,cmplx(0.0))

	  call haccess(tVis,itVis,'gains','read',iostat)
	  if(iostat.ne.0)call NormBug(iostat, 
     *      'Error accessing gains for '//vis)
c
	  offset = 16
	  do j=1,nsols
	    call hreadr(itVis,Gains,offset,8*ngains,iostat)
	    if(iostat.ne.0)
     *	      call NormBug(iostat,'Error reading gains item')
	    ant=1
	    do i=1,nant
	      call XYCvt(Gains,calXYGains,nfeeds,ntau,nant,.true.)
	      accCalXYGains(ant) = accCalXYGains(ant) + calXYGains(ant)
	      ant=ant+1
	    enddo
	    offset = offset + 8*ngains + 8
	  enddo
c         close gains
	  call hdaccess(itVis,iostat)
	  if(iostat.ne.0)call NormBug(iostat,'Error closing gains item')
c
c         Now take the ones which weren't selected
c
	  do i=1,nant
	    if (.not.antflag(i)) then
	      xyphases(i)=accCalXYGains(i)/abs(accCalXYGains(i))
	    end if
	  end do
	  
c
c  open gains again and write them out with the new xyphases
c
          call haccess(tVis,itVis,'gains','append',iostat)
          if(iostat.ne.0)call NormBug(iostat,'Error accessing gains')
          offset = 16
          do j=1,nsols
            call hreadr(itVis,Gains,offset,8*ngains,iostat)
            if(iostat.ne.0)
     *        call NormBug(iostat,'Error reading gains item')
	    ant=1
            do i=1,ngains,nfeeds+ntau
c             Exp is: gain(y)=amp(y)*phase(x)*xyphase
              Gains(i+1) = cmplx(abs(Gains(i+1))) * 
     *           Gains(i)/abs(Gains(i)) * conjg(xyphases(ant))
	      ant=ant+1
            enddo
            call hwriter(itVis,Gains,offset,8*ngains,iostat)
            if(iostat.ne.0)
     *        call NormBug(iostat,'Error writing gains item')
            offset = offset + 8*ngains + 8
          enddo
          call hdaccess(itVis,iostat)
          if(iostat.ne.0)
     *      call NormBug(iostat,'Error closing gains item')
 	else
	  call bug('w','Unable to open exisiting gains for '//vis)
	  call bug('f',
     *     'I cant put in an xyphase if there are no gains')
	end if
c
	
	end 
c************************************************************************
        real function GetPhasW(G)
c
        implicit none
        complex G
c
c  Get the wrapped phase of a complex value.
c  From, um, gpcal? I forget - dpr
c------------------------------------------------------------------------
        include 'mirconst.h'
c
        if(abs(real(G))+abs(aimag(G)).le.0)then
          GetPhasW = 0
        else
          GetPhasW = 180/pi * atan2(aimag(G),real(G))
        endif
        end
c************************************************************************
        subroutine XYCvt(In,Out,nfeeds,ntau,ngains,doxy)
c
        implicit none
        integer nfeeds,ntau,ngains
        logical doxy
        complex In((nfeeds+ntau)*ngains),Out(ngains)
c
c  This divides or multiplies the X gains by the Y gains, 
c  to get the XY gains.
c  Pirated from somewhere - dpr
c------------------------------------------------------------------------
        integer i,j
        real temp
c
        i = 1
        do j=1,ngains
          temp = abs(real(In(i+1)))+abs(aimag(In(i+1)))
          if(temp.le.0)then
            Out(j) = cmplx(0.0,0.0)
          else if(doxy)then
            Out(j) = In(i)/In(i+1)
          else
            Out(j) = In(i)*conjg(In(i+1))
          endif
          i = i + nfeeds + ntau
        enddo
        end
c************************************************************************
        subroutine writeo(tIn,line)
c
        implicit none
        integer tIn
        character line*(*)
c
c  Write out a line to the history file and the output.
c  Input
c    tIn - input file handle.  tIn=-1 for screen only
c    line - message
c------------------------------------------------------------------------
        character string*80
c
        string = 'GPCOMB: '//line
	if (tIn.ne.-1) call HisWrite(tIn,string)
        call output(line)
        end
c************************************************************************
	subroutine GetOpt(doxy,dopass,dopol)
c
	implicit none
	logical doxy,dopass,dopol
c
c  Get extra processing options.
c
c  Output:
c    doxy	Solve for and apply averaged XY phases.
c    dopass	Solve for and apply averaged bandpass.
c    dopol      Solve for and apply averaged leakages.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=3)
	logical present(nopts)
	character opts(nopts)*8
	data opts/'noxy    ','nopass  ','nopol   '/
c
	call options('options',opts,present,nopts)
	doxy = .not.present(1)
	dopass = .not.present(2)
	dopol = .not.present(3)
c
	end
c************************************************************************








