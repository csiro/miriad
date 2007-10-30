************************************************************************
	PROGRAM odnh3 
	implicit none
c
c  Program to calculate optical depths and temperatures from ammonia data
c  Calculates optical depth of NH3(1,1) line from hyperfine ratios
c    (uses lookup table to invert the equation: 
c       T_m/T_hf = (1 - exp(-tau_m))/(1 - exp(-tau_hf)); (tau_hf = f*tau_m)
c                     f = 0.28, 0.22 for inner hyperfine, outer hyperfine
c
c  Calculates rotation temperature from NH3(1,1) and NH3(2,2) lines
c     using the formula (Ho and Townes, ARAA, p. 249 (1983)):
c		T_R(2,2;1,1) = -41.5 / 
c ln[(-0.282/tau_m(1,1)) * ln{1 - T_b(2,2)/T_b(1,1) * [1 - exp(tau_m(1,1))]}]
c
c  History:
c    alr  jun90 - Adapted from MATHS (Alex Rudolph)
c    pjt   4mar91 - itoa->itoaf
c    pjt  10mar91 got rid of testing MIN() in PARAMETER: is not ANSI
c    mjs  26may92 declared and initialized "zrow" variable that is used.
c    nebk 26nov92 Add btype to output header
c    mjs  17feb93 Elim unused vars to eliminate compiler warning messsges.
c    rjs  02jul97 cellscal change.
c------------------------------------------------------------------------
c This program allocates:
c   maths.h     3*MAXRUNS
c   main        3*MAXRUNS + 4*BUFLEN + MAXBOX + RBUFLEN
c
c The biggest memory hog is the array Rbuf(RBUFLEN), which likes to
c fill it's array with the size of one image plane (taking into account
c the boxes= keyword). This can be up to MAXDIM*MAXDIM, so very large.
c------------------------------------------------------------------------
c= odnh3 - Ammonia analysis program
c& pjt
c: map combination, map manipulation
c+
c ODNH3 is a MIRIAD task which creates optical depth and rotational
c temperatures maps from ammonia maps.  Ammonia J,K=(1,1) emission
c has five hyperfine components, one main hyperfine component, two
c inner hyperfine components, and two outer hyperfine components.
c
c Odnh3 takes a main hyperfine component map and either an inner
c or outer hyperfine component map and combines the two maps to
c make a single main hyperfine component optical depth map. 
c
c Odnh3 can also combine a J,K=(1,1) main hyperfine component
c map, a J,K=(2,2) main hyperfine component map, and a J,K=(1,1)
c main hyperfine component optical depth map to create a rotational
c temperature map.
c
c Any of the input maps can be masked to allow a signal-to-noise cutoff.
c@ map1
c Main hyperfine component map of the J,K=(1,1) level.  No default.
c@ map2
c For op=taui, map2 is the inner hyperfine component map of 
c the J,K=(1,1) level. 
c For op=tauo, map2 is the outer hyperfine component map of 
c the J,K=(1,1) level.
c For op=temp, map2 is the main  hyperfine component map of 
c the J,K=(2,2) level.
c No default.
c@ mapt
c For op=temp, mapt is the J,K=(1,1) main hyperfine component 
c optical depth map. 
c No default.
c@ mask
c A mask expression using FORTRAN syntax.  The optical depth or 
c rotational temperature is only calculated at pixels where the 
c mask is TRUE.  Thus, this keyword can be used to enforce a 
c signal-to-noise cutoff on the input maps.  No default.
c< region
c@ op
c Which Odnh3 option is being used.  See map2 for details.  
c No default.
c@ out
c The name of the output image.  No default.
c------------------------------------------------------------------------
	INCLUDE 'maths.h'
	INTEGER ERROR,VECTOR,SCALAR,CONSTANT
	PARAMETER(ERROR=0,VECTOR=3,SCALAR=2,CONSTANT=1)
	INTEGER BUFLEN,RBUFLEN,MAXBOX
        PARAMETER(BufLen=64,RBufLen=MaxBuf,MaxBox=2048)
	CHARACTER PVERSION*(*)
	PARAMETER (PVERSION='Version 26-Nov-92')
c
	CHARACTER map1*64,map2*64,mapt*64,out*64,mask*64,op*16
	INTEGER	  LOWI,HIGHI
	PARAMETER(LOWI=1,HIGHI=180)
	INTEGER   type,lout,index,i,j,n,low,high,counter
	INTEGER	  run,runmin,runmax
	INTEGER   maskbuf(BUFLEN),boxes(MAXBOX)
	INTEGER   size1(3),size2(3),size3(3),naxis1,naxis2,naxis3 
	REAL      rbuf(RBUFLEN),maskrbuf(BUFLEN)
	REAL	  buf1(512),buf2(512),buf3(512)
	REAL	  R(500),tau(500),Robs,tauobs,tempobs,tout
	REAL	  frac,exptau,logterm1,logterm2,f
	INTEGER   nMRB,nBuf,xblc,xtrc,yblc,ytrc,npixels,ZRow
	LOGICAL   doMask,doRuns
	INTEGER   scratch(3,MAXRUNS),nout(MAXNAX)
c
c  Externals.
c
	logical BoxRect
	integer Fill
	external paction,vaction
c
	call output('ODNH3: '//PVERSION)
c
c  Get the input parameters.
c
	call keyini
	call keya('map1',Map1,' ')
	call keya('map2',Map2,' ')
	call keya('mapt',Mapt,' ')
	call keya('mask',Mask,' ')
	call keya('out',Out,' ')
	call keya('op',op,' ')
	call BoxInput('region',' ',boxes,maxBox)
	call keyfin
	call lcase(op)
c
	if(Map1.eq.' '.or.Map2.eq.' ')
     *    call bug('f','Two input maps must be given')
	if(Out.eq.' ')
     *	  call bug('f','Output file must be given')
	if(op.ne.'taui'.and.op.ne.'tauo'.and.op.ne.'temp')
     *	  call bug('f','Invalid option given')
	if(op.eq.'temp'.and.Mapt.eq.' ')
     *	  call bug('f','Optical depth map must be given to calculate '//
     *						'temperature')
c
c  Open the input files
c
	call xyopen(lIn(1),Map1,'old',maxnax,size1)
	call xyopen(lIn(2),Map2,'old',maxnax,size2)
	if((op.eq.'temp').and.(Mapt.ne.' ')) then
	  call xyopen(lIn(3),Mapt,'old',maxnax,size3)
	else
 	  do i=1,3
	    size3(i) = size2(i)
	  enddo
	endif
	do i=1,3
	  if(size1(i).ne.size2(i).or.size2(i).ne.size3(i)
     *					.or.size3(i).ne.size1(i))
     *      call bug('f','Input images must all be the same size')
	enddo
	call rdhdi(lIn(1),'naxis',naxis1,0)
	call rdhdi(lIn(2),'naxis',naxis2,0)	
	if((op.eq.'temp').and.(Mapt.ne.' ')) then
	  call rdhdi(lIn(3),'naxis',naxis3,0)
	else
	  naxis3 = naxis2
	endif
  	if(naxis1.ne.naxis2.or.naxis2.ne.naxis3.or.naxis3.ne.naxis1)
     *    call bug('f','Input images must have the same dimensionality')
	if(naxis1.gt.2.or.naxis2.gt.2.or.naxis3.gt.2)
     *    call bug('f','Odnh3 presently only supports '//
     *					 'two-dimensional images')
	naxis = naxis1
c
c  Parse the mask.
c
	doMask = Mask.ne.' '
	if(doMask)then
	  call ariComp(Mask,paction,type,MaskBuf,Buflen,MaskRBuf,BufLen)
	  if(type.eq.error)
     *		call bug('f','Error parsing the mask expression')
	  if(type.ne.vector)
     *		call bug('f','No images in input mask expression')
	  call ariInq(MaskBuf,MaskRBuf,nBuf,nMRB)
	endif
c
c  Bloody box information.
c
	call BoxSet(boxes,naxis,size1,' ')
	call BoxInfo(boxes,naxis,blc,trc)
c
c  "And" the flagging masks of the three input files over the
c  regions where the computation is to take place
c
	call BoxMask(lIn(1),boxes,maxBox)
	call BoxMask(lIn(2),boxes,maxBox)
	call BoxMask(lIn(3),boxes,maxBox)
c
	doRuns = .not.BoxRect(boxes)
c
	do i=1,naxis
	  nOut(i) = trc(i) - blc(i) + 1
	enddo
	do i=naxis+1,maxnax
	  nOut(i) = 1
	  blc(i) = 1
	  trc(i) = 1
	enddo
c
c  If calculating optical depths, generate the look-up table
c  from tau from 0.10 to 1.00 by 0.01 and from 1.0 to 10.0 by 0.1
c
c  Note:  R(180) < R(1), whereas tau(180) > tau(1)
c
	if(op.ne.'temp') then
	  if(op.eq.'taui') then
	    f = 0.28
	  else 
	    f = 0.22
 	  endif
	  do i = 1,90,1
	    tau(i) = (9 + i) * 0.01
	      R(i) = (1 - exp(-tau(i)))/(1 - exp(-f*tau(i)))
	  enddo
	  do i = 91,180,1
	    tau(i) = (i - 81) * 0.10
	      R(i) = (1 - exp(-tau(i)))/(1 - exp(-f*tau(i)))
	  enddo
	end if
c
c  Open the output, create the header, append the history
c
	call xyopen(lOut,Out,'new',naxis,nOut)
	call CpHdr(lIn(1),lOut,naxis,nsize,blc,trc,op,Mask)
c
c  Let us recapitulate what we have.
c  The mask expression (if doMask) is in MaskBuf,RBuf.
c  The boxes specification (if doRuns) is in boxes.
c
c  Keep this around for future expansion to three dimensions
c
c	do k=1,nOut(3)
c	  ZRow = k + blc(3) - 1
c
	ZRow = 1
	  call xysetpl(lIn(1),1,ZRow)
	  call xysetpl(lIn(2),1,ZRow)
	  call xysetpl(lOut,1,ZRow)
c
	  call BoxRuns(max(naxis-2,1),ZRow,' ',boxes,
     *		Runs,MaxRuns,nRuns,xblc,xtrc,yblc,ytrc)
c
	  if(doMask) then
	    npixels = Fill(Runs,nRuns)
	    if(npixels.gt.0)then
	      if(nMRB.gt.0)call MoveData(MaskRBuf,nMRB,RBuf)
	      call ariExec(vaction,npixels,MaskBuf,BufLen,RBuf,RBufLen,
     *							Index)
	      call CompRuns(RBuf(Index),
     *				Runs,maxRuns,nRuns,Scratch,MaxRuns)
	    endif
	  endif
	  if(doMask.or.doRuns) call PutRuns(lOut,Runs,nRuns,
     *	       1-blc(1),1-blc(2),nOut(1),nOut(2))
c
c  Here is the work; calculations will be done in run format
c  Each run is contained in the array Runs(3,*), where
c  Runs(1,r) = row number j contained in run r
c  Runs(2,r) = minimum x in row j contained in run r
c  Runs(3,r) = maximum x in row j contianed in run r
c  nRuns = number of runs
c
c  Read each run; save the starting point in RBuf, called Index
c
	  counter = Index
	  do run = 1,nRuns
	    j = Runs(1,run)
	    call xyread(lIn(1),j,buf1)
	    call xyread(lIn(2),j,buf2)
	    if (op.eq.'temp') call xyread(lIn(3),j,buf3)
c
c  For each element of the run, calculate Robs = T(1)/T(2)
c
	    runmin = runs(2,run)
	    runmax = runs(3,run)	    
	    do i = runmin,runmax
	      Robs = buf1(i)/buf2(i)
	      tauobs = buf3(i)
c
c  For optical depths, find the tau for this R by searching the lookup table 
c  and interpolating
c
	      if (op.eq.'taui'.or.op.eq.'tauo') then
		if((Robs.lt.R(highi)).or.(Robs.gt.R(lowi))) then
		  tauobs = -99999
		else
		  low = lowi
		  high = highi
		  do while((high-low).gt.1)
		    n = (low + high)/2
		    if(Robs.gt.R(n)) then
		      high = n
		    else
		      low = n
		    endif
		  enddo
		  frac = (R(low) - Robs)/(R(low) - R(high))
		  tauobs = tau(low) + frac * (tau(high) - tau(low))
		endif
		tout = tauobs
c
c  For Temperature calculate T_R using the formula from Ho and Townes (1983)
c  		Test logarithms before calculating
c
	      else 
		  tempobs = 0
		  exptau = 1 - exp(-tauobs)
c	
		  if (Robs.gt.exptau) then 
		    logterm1 = log(1 - exptau/Robs)
		  else
		    tempobs = -99999
		  endif
c
		  if (logterm1.lt.0)  then
		    logterm2 = log(tauobs/(-0.282 * logterm1))
		  else
		    tempobs = -99999
		  endif
c
		  if(tempobs.ne.-99999)  tempobs = 41.5 / logterm2
		tout = tempobs
	      endif	
c
c  Write the value into RBuf
c
c		if(tout.eq.-99999) then
c		  call xymkwr(lOut,j,i,1)
c		else
		  RBuf(counter) = tout
		  counter = counter + 1
c		endif
	    enddo
	  enddo
c
c  Write the data into the relevant plane
c
	npixels = counter - Index
c	If(npixels.ne.(counter-Index))
c    *    call bug('f','The number of pixels from Fill does not '//
c    *			          'agree with the number counted here')
c
	call PutPlane(lOut,Runs,nRuns,
     *	       1-blc(1),1-blc(2),nOut(1),nOut(2),RBuf(Index),npixels)
c
c	enddo
c  End of do loop to allow three dimensions
c
c  Mask off the values that are -99999
c
c	expr = 'lOut.ne.-99999'
c 	call ariComp(expr,paction,type,ExpBuf,Buflen,ExpRBuf,BufLen)
c	call ariInq(ExpBuf,ExpRBuf,nBuf,nERB)
c	npixels = Fill(Runs,nRuns)
c	if(npixels.gt.0) then
c	  call MoveData(ExpRBuf,nERB,RBuf)
c	  call ariExec(vaction,npixels,ExpBuf,BufLen,RBuf,RBufLen,Index)
c	  call CompRuns(RBuf(Index),Runs,maxRuns,nRuns,Scratch,MaxRuns)
c	  call PutRuns(lOut,Runs,nRuns,1-blc(1),1-blc(2),
c    *						nOut(1),nOut(2))
c	endif
c
c  Close all the files.
c
	call xyclose(lOut)
	call xyclose(lIn(1))
	call xyclose(lIn(2))
	if(op.eq.'temp') call xyclose(lIn(3))
c
	end
c************************************************************************
	subroutine MoveData(In,n,Out)
c
	implicit none
	integer n
	real In(n),Out(n)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Out(i) = In(i)
	enddo
	end
c************************************************************************
	subroutine CompRuns(Mask,Runs,maxRuns,nRuns,Scratch,nScratch)
c
	implicit none
	integer nRuns,maxRuns,nScratch
	integer Runs(3,maxRuns),Scratch(3,nScratch)
	real Mask(*)
c
c  This takes an input runs specification and a mask, and combines them
c  into an output runs specification. The mask is real valued. A positive
c  value indicates the corresponding pixel is to be selected. A negative
c  or zero value indicates the pixel is to be ignored. The values in the
c  mask correspond only for pixels selected in the input runs specification.
c
c  Inputs:
c    Mask	The mask.
c    maxRuns	The size of the runs array.
c    nScratch	Size of the scratch buffer.
c
c  Input/Output:
c    Runs	Input runs.
c    Nruns	Number of runs in the input.
c
c  Scratch:
c    Scratch	An intermediate working array.
c------------------------------------------------------------------------
	integer i,k,l,length,nOut
c
c  Initialise.
c
	nOut = 0
	k = 1
c
c  Determine the output runs, and place them in the scratch buffer.
c
	do i=1,NRuns
	  length = Runs(3,i) - Runs(2,i) + 1
	  l = 0
	  dowhile(l.lt.length)
	    dowhile(l.lt.length.and.Mask(k).le.0)
	      k = k + 1
	      l = l + 1
	    enddo
	    if(l.lt.length)then
	      if(nOut.eq.nScratch)
     *		call bug('f','Scratch buffer overflow')
	      nOut = nOut + 1
	      Scratch(1,nOut) = Runs(1,i)
	      Scratch(2,nOut) = Runs(2,i) + l
	      dowhile(l.lt.length.and.Mask(k).gt.0)
		k = k + 1
		l = l + 1
	      enddo
	      Scratch(3,nOut) = Runs(2,i) + l - 1
	    endif
	  enddo
	enddo
c
c  Copy the output runs (in the scratch buffer) to the runs array.
c
	if(nOut.gt.maxRuns) call bug('f','Runs buffer overflow')
	do i=1,nOut
	  Runs(1,i) = Scratch(1,i)
	  Runs(2,i) = Scratch(2,i)
	  Runs(3,i) = Scratch(3,i)
	enddo
	nRuns = nOut
c
	end
c************************************************************************
	integer function Fill(Runs,nRuns)
c
	implicit none
	integer nRuns,Runs(3,*)
c
c  This determines the number of pixels of interest in the current plane.
c
c  Output:
c    Fill	The number of pixels of interest.
c
c------------------------------------------------------------------------
	integer npixels,i
c
	npixels = 0
	do i=1,nRuns
	  npixels = npixels + Runs(3,i) - Runs(2,i) + 1
	enddo
c
	Fill = npixels
	end
c************************************************************************
	subroutine CPHdr(lIn,lOut,naxis,n,blc,trc,op,Mask)
c
	implicit none
	integer lIn,lOut
	integer naxis,blc(naxis),trc(naxis),n(naxis)
	character Mask*(*),op*(*)
c
c  Make the header of the output file. This is a carbon copy of the
c  input, except that a history record is added.
c
c------------------------------------------------------------------------
	character card*128,txtblc*32,txttrc*32,key*8
	integer i,lblc,ltrc
	real def,crpix
	integer nkeys
	parameter(nkeys=26)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*2
c
	data keyw/   'bunit   ','cdelt1  ','cdelt2  ','cdelt3  ',
     *	  'crota1  ','crota2  ','crota3  ','crval1  ','crval2  ',
     *    'crval3  ','ctype1  ',
     *	  'ctype2  ','ctype3  ','obstime ','epoch   ','history ',
     *	  'instrume','niters  ','object  ','telescop','cellscal',
     *	  'restfreq','vobs    ','observer','obsra   ',
     *	  'obsdec  '/
C
	do i=1,nkeys
	  call hdcopy(lIn,lOut,keyw(i))
	enddo
c
c  Write the reference pixel location.
c
	do i=1,naxis
	  key = 'crpix'//itoaf(i)
	  if(i.le.2)then
	    def = n(i)/2 + 1
	  else
	    def = 1
	  endif
	  call rdhdr(lIn,key,crpix,def)
	  call wrhdr(lOut,key,crpix-blc(i)+1)
	enddo
        if (op.eq.'temp') then
          call wrbtype (lOut,'rotational_temperature')
        else
          call wrbtype (lOut,'optical_depth')
        end if
c
	call hisopen(lOut,'append')
        call hiswrite(lOut,'ODNH3 (MIRIAD)')
	call mitoaf(blc,naxis,txtblc,lblc)
	call mitoaf(trc,naxis,txttrc,ltrc)
	card = 'ODNH3: Bounding region is Blc=('//txtblc(1:lblc)//
     *				       '),Trc=('//txttrc(1:ltrc)//')'

	call hiswrite(lOut,card)
	if(op.eq.'taui') then
	  card = 'ODNH3: Calculating tau_m(1,1) for NH3, using the '//
     *				'main and inner hyperfine component'
	else if(op.eq.'tauo') then
	  card = 'ODNH3: Calculating tau_m(1,1) for NH3, using the '//
     * 				'main and outer hyperfine component'
	else
	  card = 'ODNH3: Calculating T_R(2,2;1,1) for NH3, using '//
     *				'the (1,1) and (2,2) transitions'
	  call hiswrite(lOut,card)
	endif
	if(Mask.ne.' ')then
	  Card = 'ODNH3: Mask = '//Mask
	  call hiswrite(lOut,card)
	endif
	call hisclose(lOut)
c
	end
c************************************************************************
	subroutine PACTION(symbol,type,index,value)
c
	implicit none
	character symbol*(*)
	integer type,index
	real value
c
c The parsers action routine. Open the file and make sure its the
c  right size.
c
c  Inputs:
c    Symbol	The file name.
c  Outputs:
c    type	Vector.
c    index	Index into lIn array.
c    value	Unused.
c
c------------------------------------------------------------------------
	include 'maths.h'
	integer error,constant,scalar,vector
	parameter(error=0,constant=1,scalar=2,vector=3)
	integer i
	integer nin(maxnax)
c
c  Externals.
c
	integer len1
c
	if(symbol.eq.'x')then
	  Xused = .true.
	  Index = xval
	else if(symbol.eq.'y')then
	  Yused = .true.
	  Index = yval
	else if(symbol.eq.'z')then
	  Zused = .true.
	  Index = zval
c
c  Check if we already have this one open.
c
	else
	  Index = 0
	  do i=1,nfiles
	    if(symbol.eq.Names(Offset(i)+1:Offset(i+1)))Index = i
	  enddo
c
c  If it was not found, open the file.
c
	  if(Index.eq.0)then
	    if(nfiles.ge.maxfiles)call bug('f','Too many open files')
	    nfiles = nfiles + 1
	    call xyopen(lIn(nfiles),Symbol,'old',maxnax,nin)
	    if(nfiles.eq.1)then
	      do i=1,maxnax
	        nsize(i) = nin(i)
	      enddo
	      call rdhdi(lIn(1),'naxis',naxis,0)
	      naxis = min(naxis, maxnax)
	      Offset(1) = 0
	    else
	      do i=1,naxis
	        if(nin(i).ne.nsize(i))
     *		  call bug('f','Input images are different sizes')
	      enddo
	    endif
	    Offset(nfiles+1) = Offset(nfiles) + len1(symbol)
	    if(Offset(nfiles+1).gt.len(Names))
     *		call bug('f','Name buffer overflow, in PACTION')
	    Names(Offset(nfiles)+1:Offset(nfiles+1)) = symbol
	    Index = nfiles
	  endif
	endif
c
	type = vector
c
	end
c************************************************************************
	subroutine VACTION(Index,Type,Data,N)
c
	implicit none
	integer Index,Type,N
	real Data(*)
c
c  This routine is called by ariExec each time it wants a row of a
c  file. Its not exactly the most complex routine in the world.
c
c  Inputs:
c    Index	Index into lIn array of file descriptors.
c    Type	Will be vector.
c    N		Will equal n1.
c  Output:
c    Data	The row of data.
c
c------------------------------------------------------------------------
	include 'maths.h'
	integer i,j,k,npixel,ZRow
	real cdelt,crval,temp
c
c  Fill in Data if it corresponds to a value of x, y or z.
c
        ZRow = 0
	if(Index.eq.xval)then
	  cdelt = (Range(2,1)-Range(1,1))/real(nsize(1)-1)
	  crval = Range(1,1) - cdelt
	  k = 1
	  do j=1,nRuns
	    do i=Runs(2,j),Runs(3,j)
	      Data(k) = cdelt * i + crval
	      k = k + 1
	    enddo
	  enddo
	else if(Index.eq.yval)then
	  cdelt = (Range(2,2)-Range(1,2))/real(nsize(2)-1)
	  crval = Range(1,2) - cdelt
	  k = 1
	  do j=1,nRuns
	    temp = cdelt * Runs(1,j) + crval
	    do i=Runs(2,j),Runs(3,j)
	      Data(k) = temp
	      k = k + 1
	    enddo
	  enddo
	else if(Index.eq.zval)then
	  cdelt = (Range(2,3)-Range(1,3))/real(nsize(3)-1)
	  crval = Range(1,3) - cdelt
	  temp = cdelt * ZRow + crval
	  do k=1,N
	    Data(k) = temp
	  enddo
c
c  Otherwise read it from a file.
c
	else
	  call GetPlane(lIn(Index),Runs,nRuns,0,0,nsize(1),nsize(2),
     *						Data,N,npixel)
	  if(N.ne.npixel) call bug('f','Something is screwy')
	endif
	end
