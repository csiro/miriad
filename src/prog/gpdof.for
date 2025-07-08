c************************************************************************
	program gpdof
	implicit none
c
c= GpDoF -- Compute Degrees-of-freedom for calibration solutions.
c& dpr
c: calibration
c+
c 
c       GPDOF estimates the likely error in a calibration solution, by
c       calculating the errors in the "Degrees-of-Freedom" (dofs) of an
c       interferometer. This is useful for calibration of high-precision
c       circular polarization observations.
c
c       What GPDOF does is to determine the average calibration solution
c       of several input solutions. Then, assuming that this average
c       solution is "ideal", GPDoF determines the error in each of the
c       input calibration solutions, using the degrees-of-freedom
c       notation. Finally, GPDoF calculates the std and standard error
c       of the derived dofs, which you can use to estimate the errors in
c       the Stokes V of a target observation.
c
c       The input calibration solutions should have been processed using
c       the method outlined in the "Circular Polarization User's Guide"
c       (see References below). Obviously, the more calibrators you
c       have, the more accurate the dof estimates will be.
c
c       Explanation:
c       
c       The degrees-of-freedom (dofs) are linear combinations of leakage
c       (delta) or gain (gamma) errors. The advantages in the dofs over
c       other measures of calibration consistency (eg. rms scatter in
c       the leakages) is that the dofs actually describe how the
c       calibration error would affect a target observation.  Thus, for
c       the dofs delta-+, delta--, and gamma--, and equatorially-mounted
c       antennas:
c
c       Stokes V error = delta-+ * Stokes I + 
c                          delta-- * Stokes Q + 
c                            gamma-- * Stokes U 
c
c       For the ATCA, the parallactic angle \chi comes, in so it 
c       isn't quite so elegant:
c
c       Stokes V error = delta-+ * I +
c                         delta-- * ( Q.sin(2\chi) + U.cos(2\chi)) +
c                           gamma-- * ( U.sin(2\chi) + Q.cos(2\chi))
c
c       Note that these are the instantaneous calibration errors in 
c       a target source; for a synthesis observation, where \chi
c       varies, the resultant error in the Stokes V image will be
c       more complicated.
c
c       GPDoF estimates the std and standard error in the dofs. These
c       parameters be combined with the above equation for Stokes V
c       Error to estimate the likely level of leakage errors in a target
c       observation. If you are just using the solution from a single
c       calibrator, calculate using the std. If you are going to use the
c       average calibration solution (using GPComb), then calculate
c       using the standard error.
c
c       For each dof, GPDoF has an "error" estimate, which measures the
c       variations in the calibration solutions which are NOT
c       attributable to a dof. The physical significance of these
c       quantities isn't even clear to the author. In general, however,
c       if the "error" term is dominant, then un-dof-like errors are
c       dominating the calibration eg. pure random errors, or bizzarro
c       systematic errors. In this situation, the relationship between
c       the dofs and the error in the target observation may break down.
c
c       Um, I did mention that there are actually seven dofs? GPDoF only
c       computes the three which affect calibration of circular
c       polarization.  There are some good reasons for this (some of the
c       others can't easily be computed, or are time-variable), besides
c       the obvious one that I'm too lazy.
c
c       Note that GPDoF is entirely ATCAcentric.
c
c       Note that the leakage errors derived from GPDoF are not the only
c       errors in a circular polarization observation! See Equation 1 of
c       Rayner et al, 2000. A task to estimate the effect of the dofs on
c       a synthesis observation will hopefully become available
c       soon. Actually, forget the "soon"...
c
c       And finally, if you have consistent systematic errors in your
c       solutions (eg. you haven't used xyref,polref in gpcal), then
c       GPDoF will severely underestimate the leakage errors. 
c
c       References:
c
c       For an explanation of degrees of freedom, see:
c
c       Sault, R J, "The Hamaker-Bregman-Sault Measurement Equation", 
c       pp 657--699, in Syhthesis Imaging in Radio Astronomy II, 1999,
c       eds. Taylor, Carilli,and Perley.
c
c       or
c
c       Sault, Hamaker and Bregman, "Understanding radio polarimetry II.
c       Instrumental calibration of an interferometer array", 1996
c       A&AS, v117, pp149-159.
c       ftp://ftp.atnf.csiro.au/pub/people/rsault/papers/polar2.ps.gz
c
c       For an overview of high-precision circular polarization
c       calibration with the ATCA, see 
c
c       Rayner, "Circular Polarization User's Guide", ATNF Technical
c       Document Series, 2000, 39.3/102,
c       http://www.atnf.csiro.au/people/drayner/Publications/
c       index.html#Circular_Polarization_Users_Guide
c
c       For a summary of the ATCA circular polarization error budget,
c       see Equation 1 of
c
c       Rayner, Norris, Sault, "Radio circular polarization of active
c       galaxies", 2000, v319, pp484-496
c	http://www.atnf.csiro.au/people/drayner/Publications/mnr3854.pdf
c
c@ cal
c	The data-sets containing the nominally correct polarization 
c       calibration. No default. 
c@ select
c	Normal uv selection. Only antenna-based selection is supported.
c--
c  History:
c    dpr    25may01 start original version, copy from gpcomb
c
c  Bugs and Shortcomings:
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	include 'gpdof.h'
	character version*(*)
	parameter(version='GpDoF: version 1.0 25-May-01')
c
c       input cal file names, and index counter, index variable
	character cals(MAXCALS)*64
	integer   calindex
	character cal*64
	integer ncals
c       selection, and antenna selection array
	integer MAXSELS
	parameter(MAXSELS=100)
	real sels(MAXSELS)
	logical antflag(MAXANT)
c       the accumulated leakages, which are averaged after accumulation,
c       and the number of leakages accumulated. Note nleaksolns .gt. 0
c       implies dopol
	complex leakages(2,MAXANT)
	integer nleaksolns
c       the number of antennas with leakages in the first file opened,
c       used for a consistency check
        integer nleaks, prevnleaks
c
c       accumulated xyphases, number of valid gain solution tables
	complex    xyphases(MAXANT)
	integer    nxysolns
c       and num antennas with xyphase, and consistency check
	integer nxy,prevnxy
c       for reading in leakages for each cal
        complex    calleakages(2,MAXANT)
c       for reading in xyphases for each cal
	complex    calxyphases(MAXANT)
c       Degrees of freedom, for cals.
c       See include file for index meanings.
        real       dofs(NDOFS,MAXCALS)
c       std of the dof parameters. Am I seriously going to try and
c       explain this???
	real       stddofs(NDOFS,MAXCALS)
c        
	integer i,junk1
c
c  Externals.
c
	logical selProbe
c
c  Get the inputs and check them.
c
	call output(version)
	call keyini
	call mkeya('cal',cals,MAXCALS,ncals)
	call selInput('select',sels,MAXSELS)
	call keyfin
c
c  Check minimum requirements to start
c
	if(ncals.eq.0)call bug('f',
     -     'You must specify at least one cal file')
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
c  Averages: we want to determine the dofs wrt the 
c  mean calibration solutions. So first we get the mean
c  cal solutions, using the gpcomb code:
c
c  Now loop through all the cals, accumulating the leakages,
c  xyphases and bandpass (as required).
c  We'll calculate the dofs at the end.
c
	do calindex=1,ncals
	  cal=cals(calindex)
c
c  Leakages
c         
          call AccLeakages(cal,leakages,nleaksolns,nleaks,antflag)
c         check we have compatible leakage tables
c           
          if ((calindex .ne. 1) .and. (nleaks .ne. prevnleaks))
     - call bug('f','Inconsistent number of leakages in '//cal)
          prevnleaks=nleaks
c
c  XY phases
c
          call AccXY(cal,xyphases,nxysolns,nxy,antflag)
c         check we have compatible num gains (ie antennas)
c
          if ((calindex .ne. 1) .and. (nxy .ne. prevnxy))
     - call bug('f','Inconsistent number of antennas in '//cal)
          prevnxy=nxy
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
c  Tell the user what we're using as the reference calibration
c
        call ReportLeakages(leakages,nleaks,-1)
        call ReportXY(xyphases,nxy,-1)
c
c  Print out the banner
	call BannerDofs(cals,ncals)
c
c  Now we go through them all again, but this time forming the
c  dof products. Report as we go.
c
 	do calindex=1,ncals
	  cal=cals(calindex)
c
c  Leakages
c         
c         We use AccLeakages, as a convenient way to read
c         in a leakage table
c
c         init
          call InitComplex(calleakages,2*MAXANT,cmplx(0.0))
          do i=1,NDOFS
            dofs(i,calindex)=0.0
	    stddofs(i,calindex)=0.0
          end do
c
          call AccLeakages(cal,calleakages,junk1,nleaks,antflag)
c
c  Xyphases
c  Again, use AccXY as a convenient way to read in xyphases
c
c         init
	  call InitComplex(calxyphases,MAXANT,cmplx(0.0))
	  call AccXY(cal,calxyphases,junk1,nxy,antflag)
c
c  dofs
c
          call CalcDofs(leakages,calleakages,xyphases,
     *      calxyphases,nxy,nleaks,antflag,dofs(1,calindex),
     *      stddofs(1,calindex))
          call ReportDofs(calindex,
     *      dofs(1,calindex),stddofs(1,calindex))
        end do
c  
c
c  Work out the std and standard error, and
c  report it
	if ((nxysolns.ne.ncals).or.(nleaksolns.ne.ncals)) then
          call bug('w',
     *      'Dof computation failed for some cals')
	  call bug('f',
     *      'Unable to compute dof statistics')
	else
	  call StatsDofs(ncals,dofs,stddofs)
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
      subroutine BannerDofs(cals,ncals)
c
      implicit none
      	character cals(*)*64
        integer   ncals
c  Print out the banner for the dofs
c  We do the printing in the loop, so we don't have to save
c  any variables.
c------------------------------------------------------------------------
	character  string*80
	character  cal*64
	integer    i
c
	write (string,'(75a1)')
     *   ('-',i=1,75)
	call output(string)

	call output(' Degree-of-Freedom Estimates:')
	call output(' File # :')
	do i=1,ncals
	  cal=cals(i)
	  write (string,'(1x,i5,a4,a)') i,' : ',cal
	  call output(string)
	end do

	write (string,'(75a1)')
     *   ('-',i=1,75)
	call output(string)

	write (string,
     *  '(1x,a6,a3,a18,3x,a18,3x,a18)') 
     *  'File #',' : ',
     *  'gamma-- (Q,U -> V)',
     *  'delta-- (Q,U -> V)',
     *  'delta+- (I -> V)  '
	call output(string)
	end
c************************************************************************
        subroutine CalcDofs(leakages,calleakages,xyphases,
     *     calxyphases,nxy,nleaks,antflag,dofs,stddofs)
c
        implicit none
	include 'gpdof.h'
 	include 'maxdim.h'
	complex   leakages(2,MAXANT)
	complex   calleakages(2,MAXANT)
	complex   xyphases(MAXANT)
	complex   calxyphases(MAXANT)
	integer   nxy
	integer   nleaks
	logical   antflag(MAXANT)
        real      dofs(NDOFS)
	real      stddofs(NDOFS)
c------------------------------------------------------------------------
c       for accumulating antenna solutions
        real      dmmarr(MAXANT),dmparr(MAXANT),gmmarr(MAXANT)
        integer   i
	integer   nant
c  External
	real      rMean,rStd

	nant=0
c       
c  Leakages
c
        do i=1,nleaks
	  if (antflag(i)) then
	    nant=nant+1
            dmmarr(nant)=imag(leakages(1,i))+imag(leakages(2,i)) - 
     *        (imag(calleakages(1,i))+imag(calleakages(2,i)))
            dmparr(nant)=imag(leakages(1,i))-imag(leakages(2,i)) - 
     *        (imag(calleakages(1,i))-imag(calleakages(2,i)))
	  end if
        end do
	if (nant.eq.0) call bug('f','No leakages selected')
        dofs(DMM)=rMean(dmmarr,nant)
        dofs(DMP)=rMean(dmparr,nant)
        stddofs(DMM)=rStd(dmmarr,nant)/sqrt(real(nant))
        stddofs(DMP)=rStd(dmparr,nant)/sqrt(real(nant))
c
c  Xyphases
c
	nant=0
	do i=1,nxy
	  if (antflag(i)) then
	    nant=nant+1
	    gmmarr(nant)=imag(calxyphases(i)) - imag(xyphases(i))
	  end if
	enddo
	if (nant.eq.0) call bug('f','No xygains selected')
	dofs(GMM)=rMean(gmmarr,nant)
	stddofs(GMM)=rStd(gmmarr,nant)/sqrt(real(nant))
        end
c************************************************************************
      subroutine ReportDofs(calindex,dofs,stddofs)
c
      implicit none
      include 'gpdof.h'
      integer         calindex
      real            dofs(NDOFS)
      real            stddofs(NDOFS)
c     Tell the poor unsuspecting user how crap the calibration is
c------------------------------------------------------------------------
	character  string*80

	write (string,
     *  '(1x,i5,a4,f8.5,a3,f7.5,a3,f8.5,a3,f7.5,a3,f8.5,a3,f7.5)') 
     *  calindex, ' : ',
     *  dofs(GMM),'+/-',stddofs(GMM),' ',
     *  dofs(DMM),'+/-',stddofs(DMM),' ',
     *  dofs(DMP),'+/-',stddofs(DMP)
	call output(string)

      end

c************************************************************************
      subroutine StatsDofs(ncals,dofs,doferr)
c
      implicit none
      include 'gpdof.h'
      integer         ncals
      real            dofs(NDOFS,MAXCALS)
      real            doferr(NDOFS,MAXCALS)
c     Compute std and standard error in the computed dofs, and report
c     to screen
c------------------------------------------------------------------------
c       actual values to compute and report
c       odd is the value for dofs, even for doferr
	real    dofstd(2*NDOFS)
	integer i,j,thisdof
c       for accumulating dofs and doferr, for a given dof
	real    tmpdof1(MAXCALS),tmpdof2(MAXCALS)
	integer doforder(NDOFS)
	character   string*80
c  External
	real    rStd,rMean
c
c  Write a banner
	write (string,'(75a1)')
     *   ('-',i=1,75)
	call output(string)
c
c  The order in which the dofs are printed in ReportDofs
c	
	doforder(1)=GMM
	doforder(2)=DMM
	doforder(3)=DMP
c
c  Calculate the std of dofs and doferr
c
	do j=1,NDOFS
	  thisdof=doforder(j)
	  do i=1,ncals
	    tmpdof1(i)=dofs(thisdof,i)
	    tmpdof2(i)=doferr(thisdof,i)
	  end do
	  dofstd(2*j-1)=rStd(tmpdof1,ncals)
	  dofstd(2*j)=rMean(tmpdof2,ncals)
	end do

c	write (string,
c     *  '(1x,a5,a4,f8.5,a3,f7.5,a3,f8.5,a3,f7.5,a3,f8.5,a3,f7.5)') 
c     *  'std  ', ' : ',
c     *  dofstd(1),'+/-',dofstd(2),' ',
c     *  dofstd(3),'+/-',dofstd(4),' ',
c     *  dofstd(5),'+/-',dofstd(6)
	write (string,
     *  '(1x,a5,a4,f9.6,a2,a7,a3,f9.6,a2,a7,a3,f9.6,a3,a7)') 
     *  'std  ', ' : ',
     *  dofstd(1),' ',' ',' ',
     *  dofstd(3),' ',' ',' ',
     *  dofstd(5),' ',' '
	call output(string)
	
c
c  Repeat for the standard error
c
	do j=1,NDOFS
	  thisdof=doforder(j)
	  do i=1,ncals
	    tmpdof1(i)=dofs(thisdof,i)
	  end do
	  dofstd(j)=rStd(tmpdof1,ncals)/sqrt(real(ncals))
	end do

	write (string,
     *  '(1x,a9,f9.6,a2,a7,a3,f9.6,a2,a7,a3,f9.6,a2,a7)') 
     *  'std err: ',
     *  dofstd(1),' ',' ',' ',
     *  dofstd(2),' ',' ',' ',
     *  dofstd(3),' ',' '
	call output(string)
	

      end

c************************************************************************
        real function rMean (rarray,nvals)
c
        implicit none
        real rarray(*)
        integer nvals
c       calc the mean
c------------------------------------------------------------------------
        integer i
        
        rMean=0.0
c
        if (nvals.eq.0) return
c          
        do i=1,nvals
          rMean=rMean+rarray(i)
        end do
        rMean=rMean/real(nvals)
        return
c
        end
c************************************************************************
        real function rStd (rarray,nvals)
c
        implicit none
        real rarray(*)
        integer nvals
c       calc the std
c------------------------------------------------------------------------
        integer i
        real    mn
c       External:
        real    rMean

        rStd=0.0
        if (nvals.eq.0) return

        mn=rMean(rarray,nvals)

        do i=1,nvals
          rStd=rStd+(rarray(i)-mn)**2
        end do

        rStd=sqrt(rStd/real(nvals)) *
     *    sqrt(real(nvals)/(real(nvals)-1))
        return
        end


          




