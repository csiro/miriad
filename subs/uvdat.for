c************************************************************************
c
c  This is a collection of routines to simplify access to uv data-sets.
c  They can do a number of commonly performed initialisation steps (uvselect,
c  linetype processing, planet processing, etc), as well as gain correction
c  and polarisation processing to each visibility record. Several input
c  files can be processed, in a sequential manner.
c
c  History:
c    rjs  13nov89 Original version.
c    rjs  14feb90 Added a few things to the inquiry routine.
c    rjs  23mar90 Added selfcal gains scaling ability. Replaced call to
c		  keya with keyf, for input filenames. Added all the gains
c		  routines.
c    rjs  26mar90 Added some stuff to the uvdatget routine.
c    rjs  27mar90 Fixed bug in making t2 valid, in Gains routines.
c    pjt  27jun90 Increased lenght of strings for filenames
c    rjs  16oct90 Check for cross-correlation data when opening a new
c		  data file.
c    rjs  31jan91 Added preliminary polarisation handling.
c    rjs  19mar91 Better default linetype. uvDatGta('pols'..) call.
c    rjs  18may91 Recognise upper and lower case 'stokes' values, as suggested by
c		  bpw.
c    rjs  14jun91 Fixed linear polarization sign convention.
c    rjs  28jun91 Added polarisation leakage correction.
c    rjs   1jul91 Added uvDatGti('nchan',...)
c    rjs   9jul91 Corrected bug when calibrating data with only one feed.
c    rjs  25jul91 Stokes parameters do not get thrown away if leakage
c		  correction is being performed.
c    mjs  04aug91 Replaced MAXANTS to use MAXANT (from maxdim.h)
c    rjs  27aug91 Some verbosity, in uvDatOpn, to appease nebk.
c    rjs   4sep91 Fixed bugs when performing BOTH leakage correction and
c		  Stokes conversion simultaneously. Bug crept in on 9Jul I
c		  believe. Also general tidying in this area.
c    rjs   4sep91 Fiddled interpolation formula somewhat.
c    rjs  11oct91 Fixed major bug in determining the gain solution.
c		  Probably crept in during July.
c    rjs  22nov91 Does cross and auto processing by uvselect calls.
c    rjs  13dec91 PolII is worked out as a sum of the present visibs.
c    rjs  16jan92 Fixed "bug" which resulted in truncation of some error
c		  messages.
c    nebk 26feb92 Added object='nfiles' to uvgetdti
c    nebk 06mar92 Fix bug in uvPolGet where DOAVER pointer was i 
c                 instead of ipol.  Caused multi-channel data to get
c                 wrongly calibrated
c    rjs  12mar92 Perform logarithmic, rather than linear, interpolation
c		  of gain solutions.
c    rjs  21jul92 Include bandpass calibration inside uvdat.
c    rjs   3aug92 Extracted all the gain handling code.
c    rjs  21oct92 Change to a message only.
c    rjs  23nov92 Better handling of input line type specification.
c    rjs   1dec92 Change "wideband" to "wide".
c    rjs   2apr93 Reorder some statements into standard order.
c    rjs  20jul93 Add uvDatGti('visno'...)
c    rjs  21jul93 Add uvDatGtr('variance'...). Correct bug with
c		  stokes=ii.
c    rjs  23aug93 New routine uvDatRew to appease nebk.
c    rjs  13dec93 Sign of V(linear) and sign of U(circular) fudge.
c    nebk 29mar94 Don't reissue calibration messages after uvdatrew
c    rjs  21jul94 Give message about planet rotation position angle.
c    rjs  26jul94 More accurate equation used in polarisation leakage
c		  correction.
c
c  User-Callable Routines:
c    uvDatInp(key,flags)
c    uvDatRew
c    uvDatOpn(tno)
c    uvDatCls()
c    uvDatRd(preamble,data,flags,n,nread)
c    uvDatWRd(data,flags,n,nread)
c    uvDatGti(object,ival)
c    uvDatGtr(object,rval)
c    uvDatGta(object,aval)
c    uvDatSet(object,ival)
c    uvDatPrb(object,dval)
c
c  Bugs and Shortcomings:
c    uvDatWRd does not return processed polarisations.
c    Only circular and linear polarisation.
c    There is a inconsistency between the leakage correction of circular
c     polarisations and the conversion to Stokes. Leakage correction is
c     NOT correct.
c************************************************************************
c* uvDatInp -- Get command line uv data parameters.
c& rjs
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvDatInp(key,flags)
c
	implicit none
	character key*(*),flags*(*)
c
c  This gets command line inputs and store them away in common. It remembers
c  which uvset calls we need to make. In particular, it gets the following
c  keywords:
c    'vis'	Input visibility files (actually the keyword is the "key"
c		argument).
c    'select'	UV selection specification (optional).
c    'stokes'	Stokes/polarisation parameters desired (optional).
c    'line'	Data linetype specification (optional).
c    'ref'	Reference linetype specification (optional).
c
c  Input:
c    key	The keyword associated with the input visibility file.
c		Generally this will be 'vis'.
c    flags	This is a character string determining the processing steps
c		that need to be performed. It consists of:
c		 'r'	Get reference linetype specification (keyword 'ref').
c		 's'	Get Stokes/polarisations (keyword 'stokes').
c		 'd'	Perform input selection (keyword 'select').
c		 'l'	Get data linetype specification (keyword 'line').
c		 'p'	Apply planet rotation and scaling.
c		 'w'	Return u and v in wavelengths.
c		 '1'	Default number of channels is 1.
c		 'x'	Data must be cross-correlation data.
c		 'a'	Data must be auto-correlation data.
c		 'b'	Input must be a single file.
c		 'c'	Apply gain/phase and delay corrections.
c		 'e'	Apply polarisation leakage corrections.
c		 'f'	Apply bandpass corrections.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
	integer offset,length,i,n,nout
	character In*128
	logical dostokes,dosingle
c
c  Externals.
c
	integer len1
	logical SelProbe
c
c  Line types.
c
	integer NTYPES
	parameter(NTYPES=3)
	character types(NTYPES)*8
	data types/'channel ','velocity','wide    '/
c
c  Initialise miscellaneous parameters.
c
	doplanet = index(flags,'p').gt.0
	dowave   = index(flags,'w').gt.0
	doref    = index(flags,'r').gt.0
	dodata   = index(flags,'l').gt.0
	docal	 = index(flags,'c').gt.0
	dopass   = index(flags,'f').gt.0
	dostokes = index(flags,'s').gt.0
	auto	 = index(flags,'a').gt.0
	cross	 = index(flags,'x').gt.0
	dosingle = index(flags,'b').gt.0
	dosels   = index(flags,'d').gt.0
	doleak	 = index(flags,'e').gt.0
	if(auto.and.cross)
     *	  call bug('f','Data cannot be both auto and cross correlation')
	plinit   = .false.
	plmaj = 0
	plmin = 0
	plangle = 0
	pnt = 0
	tno = 0
	nPolF = 0
c
c  Get the input file names.
c
	nIn = 0
	offset = 0
	call keyf(key,In,' ')
	dowhile(In.ne.' ')
	  nIn = nIn + 1
	  if(nIn.gt.maxIn) call bug('f','Too many input files')
	  length = len1(in)
	  if(offset+length.gt.len(Inbuf))
     *	    call bug('f','Input name buffer overflow')
	  k1(nIn) = offset + 1
	  k2(nIn) = offset + length
	  Inbuf(k1(nIn):k2(nIn)) = In
	  offset = offset + length
	  call keyf(key,in,' ')
	enddo
	if(nIn.eq.0) call bug('f','No input files were given.')
	if(nIn.gt.1.and.dosingle)
     *		     call bug('f','Only one input vis file is allowed')
c
c  Determine the input linetype.
c
	if(dodata)then
	  nchan = 0
	  if(index(flags,'1').gt.0) nchan = 1
	  call keymatch('line',NTYPES,types,1,line,nout)
	  call keyi('line',nchan,nchan)
	  call keyr('line',lstart,1.)
	  call keyr('line',lwidth,1.)
	  call keyr('line',lstep,lwidth)
	else
	  line = ' '
	  nchan = 0
	  lstart = 1.
	  lwidth = 1.
	  lstep  = lwidth
	endif
c
c  Determine the reference line, if required.
c
	if(doref)then
	  call keymatch('ref',NTYPES,types,1,ref,nout)
	  call keyr('ref',rstart,1.)
	  call keyr('ref',rwidth,1.)
	  doref = ref.ne.' '
	endif
c
c  Get the Stokes/polarisation parameters, if required.
c
	if(dostokes)call uvPolInp(maxPol,nPol,Pols)
c
c  Get the selection parameters, and determine whether polarization
c  selection was used.
c
	SelPol = .false.
	SelPol1 = .false.
	if(dosels)then
	  call SelInput('select',sels,maxsels)
	  SelPol = SelProbe(sels,'polarization?',0.d0)
	  if((dostokes.or.doleak).and.SelPol)then
	    i = PolMin
	    n = 0
	    dowhile(i.le.PolMax.and.n.lt.2)
	      if(SelProbe(sels,'polarization',dble(i))) n = n + 1
	      i = i + 1
	    enddo
	    SelPol1 = n.eq.1
	  endif
	endif
	end
c************************************************************************
	subroutine uvPolInp(maxPol,nPol,Pols)
c
	implicit none
	integer maxPol,nPol,Pols(maxPol)
c
c  Get the Stokes/polarisations that the user requires.
c
c  Input:
c    maxPol	The max number of polarisations that the user can request.
c  Output:
c    nPol	The number of polarisations requested.
c    Pols	The desired polarisations.
c------------------------------------------------------------------------
	character type*2
c
c  Externals.
c
	logical keyprsnt
	integer PolsP2C
c
	nPol = 0
	call keya('stokes',type,' ')
	dowhile(type.ne.' '.and.nPol.lt.maxPol)
	  nPol = nPol + 1
	  if(type.eq.'ii'.or.type.eq.'II')then
	    Pols(nPol) = 0
	  else
	    Pols(nPol) = PolsP2C(type)
	  endif
	  call keya('stokes',type,' ')
	enddo
	if(keyprsnt('stokes'))
     *	  call bug('f','Too many Stokes parameters to process')
	end
c************************************************************************
c* uvDatOpn -- Open uv data from a multi-file set.
c& rjs
c: uv-i/o,uv-data,uv-selection
c+
	logical function uvDatOpn(tIn)
c
	implicit none
	integer tIn
c
c  This opens the next UV data file to be processed and performs the
c  necessary initialisation.
c
c  Output:
c    tIn	The handle of the uv data file just opened.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
	integer length
	logical update,present,shortcut,willpass,first
	character obstype*16,type*1,umsg*64
c
c  Externals.
c
	logical hdprsnt
        save first
        data first /.true./
c
	if(tno.ne.0)
     *	  call bug('f','UV data file already open, in UVDatOpn')
        if(first) dunrew=.false.
        first = .false.
	pnt = pnt + 1
	if(pnt.le.nIn)then
	  call uvopen(tno,InBuf(k1(pnt):k2(pnt)),'old')
	  if(dosels)call SelApply(tno,sels,.true.)
c
c  Linetype, etc, setup.
c
	  if(line.eq.' ')then
	    call uvprobvr(tno,'corr',type,length,update)
	    if(type.eq.'j'.or.type.eq.'r'.or.type.eq.'c')then
	      line = 'channel'
	    else
	      line = 'wide'
	    endif
	  endif
c
	  if(dodata)
     * 	    call uvset(tno,'data',line,nchan,lstart,lwidth,lstep)
	  if(doref)
     *	    call uvset(tno,'reference',ref,1,rstart,rwidth,rwidth)
	  if(dowave)
     *	    call uvset(tno,'coord','wavelength',0,0.,0.,0.)
c
c  Check that its auto or cross correlation data.
c
	  if(auto.or.cross)then
	    call rdhda(tno,'obstype',obstype,'crosscorrelation')
	    if(obstype(1:4).ne.'auto'.and.auto)then
	      call uvselect(tno,'and',0.d0,0.d0,.true.)
	      call uvselect(tno,'auto',0.d0,0.d0,.true.)
	    else if(obstype(1:5).ne.'cross'.and.cross)then
	      call uvselect(tno,'and',0.d0,0.d0,.true.)
	      call uvselect(tno,'auto',0.d0,0.d0,.false.)
	    endif
	  endif
c
c  Take care of the planet parameters, particularly the case if the
c  parameters are missing.
c
	  if(doplanet)then
	    call uvprobvr(tno,'plmaj',type,length,update)
	    present = type.eq.'r'
	    call uvprobvr(tno,'plmin',type,length,update)
	    present = present.and.type.eq.'r'
	    call uvprobvr(tno,'plangle',type,length,update)
	    present = present.and.type.eq.'r'
	    if(plinit.and..not.present)then
	      umsg = 'Planet parameters missing from '//
     *				InBuf(k1(pnt):k2(pnt))
	      call bug('w',umsg)
	    else if(present)then
	      call uvset(tno,'planet',' ',0,plmaj,plmin,plangle)
	    else
	      doplanet = .false.
	    endif
	  endif
c
c  Setup the gains routines, if necessary.
c
	  WillCal = docal.and.hdprsnt(tno,'gains')
	  willpass = dopass.and.(hdprsnt(tno,'bandpass').or.
     *	  	hdprsnt(tno,'cgains').or.hdprsnt(tno,'wgains'))
	  if(willpass.and.line.eq.'velocity')then
	    umsg = 'Cannot apply bandpass correction, for '//
     *	      'velocity linetype, to '//InBuf(k1(pnt):k2(pnt))
	    call bug('w',umsg)
	    willpass = .false.
	  endif
	  if(WillCal.or.willpass)call uvGnIni(tno,WillCal,willpass)
c
c  Try to determine the number of polarisations in the file (npol parameter),
c  We conclude that the file has only one polarisation if the "npol"
c  variable is missing. Also we can determine "npol" if it has been overridden.
c
	  nPolF = 1
	  if(.not.SelPol1)then
	    call uvprobvr(tno,'npol',type,length,update)
	    if(type.eq.'i')call rdhdi(tno,'npol',nPolF,0)
	  endif
	  if(nPolF.gt.1.and.SelPol)nPolF = 0
c
c  To correct for polarisation leakage, we must have 4 polarisations
c  present in the file, the user/caller must want to, and we must have the
c  leakage terms in the first place! Also we do not perform polarisation
c  correction to PolII, so turn polarization correction off if this is
c  the only polarisation that we are interested in.
c
	  WillLeak = doleak.and.hdprsnt(tno,'leakage')
	  if(nPol.eq.1) WillLeak = WillLeak.and.Pols(1).ne.PolII
	  if(WillLeak.and.nPolF.lt.4.and.nPolF.ne.0)
     *	    call bug('f','Required polarizations missing from the file')
	  if(WillLeak) call uvLkIni()
c
c  Check if we need to perform polarisation/Stokes processing.
c  We use the shortcut of
c  allowing the uvselection software to do the work if only one
c  polarisation is required, and either the file has only 1 polarisation,
c  or that the polarisation that we want is a "raw" one (its code is less or
c  equal to 0).  We cannot take the shortcut if polarisation leakage
c  correction is to be performed.
c
	  shortcut = (nPol.eq.1.and.Pols(1).le.0).or.nPolF.eq.1
	  shortcut = shortcut.and..not.WillLeak
	  WillPol = nPol.gt.0.or.WillLeak
	  PolCpy  = nPol.eq.0.and.WillPol
c
c  Use the short cut!
c
	  if(WillPol.and.shortcut)then
	    if(nPol.gt.1)then
	      umsg = 'Needed polarizations missing from '//
     *				InBuf(k1(pnt):k2(pnt))
	      call bug('f',umsg)
	    endif
	    call uvselect(tno,'and',0.d0,0.d0,.true.)
	    if(Pols(1).eq.PolII)then
	      call uvselect(tno,'polarization',
     *				dble(PolI),0.d0,.true.)
	      call uvselect(tno,'polarization',
     *				dble(PolRR),0.d0,.true.)
	      call uvselect(tno,'polarization',
     *				dble(PolLL),0.d0,.true.)
	      call uvselect(tno,'polarization',
     *				dble(PolXX),0.d0,.true.)
	      call uvselect(tno,'polarization',
     *				dble(PolYY),0.d0,.true.)
	    else
	      call uvselect(tno,'polarization',
     *				dble(Pols(1)),0.d0,.true.)
	    endif
	    WillPol = .false.
	  endif
c
c  If there is to be polarisation handling, make sure that the user
c  has not used polarization selection.
c
	  if(WillPol.and.SelPol) call bug('f',
     *	     'Cannot use pol. selection when doing pol. processing')
c
c  Give output messages.
c
	  if(willpass.and..not.dunrew)then
	    umsg = 'Applying bandpass corrections to '//
     *	      InBuf(k1(pnt):k2(pnt))
	    call output(umsg)
	  endif
	  if(WillCal.and..not.dunrew) then
	    umsg = 'Applying gain corrections to '//
     *	      InBuf(k1(pnt):k2(pnt))
	    call output(umsg)
	  endif
	  if(WillLeak.and..not.dunrew)then
	    umsg = 'Applying polarization leakage corrections to '//
     *	      InBuf(k1(pnt):k2(pnt))
	    call output(umsg)
	  endif
	  WillCal = WillCal.or.willpass
c
c  All done. Return with the bacon.
c
	  uvDatOpn = .true.
	  tIn = tno
	  iPol = 0
	else
	  uvDatOpn = .false.
	endif
	end
c************************************************************************
c* uvDatRd -- Read uv data from a multi-file set.
c& rjs mchw
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvDatRd(preamble,data,flags,n,nread)
c
	implicit none
	integer n,nread
	double precision preamble(4)
	complex data(n)
	logical flags(n)
c
c  This opens the uv data files, performs the various uvset and uvselect
c  calls, and the uvclose calls, as well (of course) as the uvread call.
c
c  Input:
c    n		The size of the data and flags arrays.
c  Outputs:
c    preamble	The uvread preamble.
c    data	The uvread correlation data.
c    flags	The uvread flags data.
c    data	The correlation data.
c    nread	The number of channels read.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
	character umsg*64
c
c  Get the data.
c
	if(WillPol)then
	  call uvPolGet(preamble,data,flags,n,nread)
	else
	  call uvread(tno,preamble,data,flags,n,nread)
	  if(WillCal.and.nread.gt.0)call uvGnFac(preamble(3),
     *	    real(preamble(4)),0,.false.,data,flags,nread)
	endif
c
c  Perform planet initialisation if necessary.
c
	if(.not.plinit.and.doplanet)then
	  call uvrdvrr(tno,'plmaj',plmaj,0.)
	  call uvrdvrr(tno,'plmin',plmin,0.)
	  call uvrdvrr(tno,'plangle',plangle,0.)
	  doplanet = plmaj.gt.0.and.plmin.gt.0
	  if(doplanet)then
	    write(umsg,'(a,f7.1,a)')
     *	      'Referencing planet position angle to',plangle,' degrees'
	    call output(umsg)
	  endif
	  plinit = .true.
	endif
c
	end
c************************************************************************
c* uvDatRew -- Start the uvDat routines from scratch.
c& rjs
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvDatRew()
c
	implicit none
c
c  This reinitialises the uvDat routines, and allows the caller to reaccess
c  all the files.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
	if(tno.ne.0)call bug('f',
     *		'Uv files still open, when uvDatRew called')
	pnt = 0
        dunrew = .true.
	end
c************************************************************************
c* uvDatCls -- Close a uv data file from a multi-file set.
c& rjs mchw
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvDatCls()
c
	implicit none
c
c  This closes a uv data file from a muli-file set.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
c
	if(tno.eq.0)call bug('f','No uvdata file open, in uvDatCls')
	if(WillCal)call uvGnFin
	call uvclose(tno)
	tno = 0
	nPolF = 0
	end
c************************************************************************
	subroutine uvPolGet(preamble,data,flags,n,nread)
c
	double precision preamble(4)
	integer n,nread
	complex data(n)
	logical flags(n)
c
c  Get data of a particular polarisation.
c  Input:
c    n
c  Output:
c    preamble
c    nread
c    data
c    flags
c
c  The desired polarisation, iPol, is formed as a linear combination of the
c  measured polarisations (i.e the polarisations available in the file).
c
c	do i=1,Snread
c	  temp = 0
c	  do j=1,ncoeff(iPol)
c	    temp = temp + Coeffs(j,iPol) * Sdata(i,indices(j,iPol))
c	  enddo
c	  data(i) = temp
c	enddo
c
c  The coefficients include all effects (e.g. ideal polarisation conversion,
c  leakage terms, gain terms). These terms are calculated by the uvPolIni
c  routine.
c------------------------------------------------------------------------
	include 'uvdat.h'
c
	integer i,j,k,nc
	complex coeff
	integer count(maxchan)
c
c  Externals.
c
	logical uvPolIni
c
c  Get more polarisation info, if necessary.
c
	if(nPol.eq.0)then
	  dowhile(.not.uvPolIni())
	  enddo
	  iPol = 1
	else
	  iPol = mod(iPol, nPol) + 1
	  if(iPol.eq.1) then
	    dowhile(.not.uvPolIni())
	    enddo
	  endif
	endif
c
c  Fill in the outputs.
c
	if(n.lt.Snread)
     *	  call bug('f','Data array too small in UvDatRd')
	nread = Snread
	if(nread.eq.0) then
	  iPol = 0
	  return
	endif
	preamble(1) = Spreambl(1)
	preamble(2) = Spreambl(2)
	preamble(3) = Spreambl(3)
	preamble(4) = Spreambl(4)
c
c  Use linear combinations of the
c  measured polarisations to get the desired one. Several polarisations
c  can be used to determine the desired one. Handle the first coefficient
c  as a separate case, and also handle the case of a coefficient of 1
c  as a special case.
c
	nc = ncoeff(iPol)
c
c  Bad data.
c
	if(nc.eq.0)then
	  do i=1,nread
	    data(i) = (0.,0.)
	    flags(i) = .false.
	  enddo
c
c  Averaged data. The output correlation is an average of the input
c  correlations.
c
	else if(doaver(ipol))then
	  do i=1,nread
	    count(i) = 0
	    data(i) = 0
	  enddo
	  do j=1,nc
	    coeff = coeffs(j,iPol)
	    k = indices(j,iPol)
	    do i=1,nread
	      if(Sflags(i,k))then
	        data(i) = data(i) + coeff * Sdata(i,k)
	        count(i) = count(i) + 1
	      endif
	    enddo
	  enddo
	  do i=1,nread
	    flags(i) = count(i).gt.0
	    if(count(i).gt.1) data(i) = data(i) / count(i)
	  enddo
c
c  Good data. For a good output correlation, all the input correlations
c  must be good.
c
	else
	  k = indices(1,iPol)
	  coeff = coeffs(1,iPol)
	  if(real(coeff).eq.1.and.aimag(coeff).eq.0)then
	    do i=1,nread
	      data(i) = SData(i,k)
	      flags(i) = Sflags(i,k)
	    enddo
	  else
	    do i=1,nread
	      data(i) = coeff * Sdata(i,k)
	      flags(i) = Sflags(i,k)
	    enddo
	  endif
c
	  do j=2,nc
	    coeff = coeffs(j,iPol)
	    k = indices(j,iPol)
	    do i=1,nread
	      data(i) = data(i) + coeff * Sdata(i,k)
	      flags(i) = flags(i) .and. Sflags(i,k)
	    enddo
	  enddo
	endif
c
	end
c************************************************************************
	logical function uvPolIni()
c
	implicit none
c
c  This reads in all the needed data for the polarisation conversion step.
c  It also calculates the coefficients needed to convert to the desired
c  polarisation type.
c------------------------------------------------------------------------
	include 'uvdat.h'
	integer ntmp,indx(PolMin:PolMax),P,nP,i,j,k,type(maxPol)
	logical NoChi,circ2,circx,lin2,linx,doLkCorr
	real Cos2Chi,Sin2Chi
	logical raw,caled(maxPol)
c
c  Initialise the indx array.
c
	do i=PolMin,PolMax
	  indx(i) = 0
	enddo
c
c  Read the first data record.
c
	call uvread(tno,Spreambl,SData(1,1),Sflags(1,1),maxchan,Snread)
	uvPolIni = Snread.eq.0
	if(uvPolIni)return
c
	call uvrdvri(tno,'npol',nP,1)
	if(nP.lt.1.or.nP.gt.maxPol) call bug('f',
     *	  'Invalid number of polarisations for me to handle')
	call uvrdvri(tno,'pol',P,0)
	if(P.ge.PolMin.and.P.le.PolMax) indx(P) = 1
	if(PolCpy)then
	  Pols(1) = P
	  nPol = nP
	endif
	caled(1) = .false.
c
c  Read the remaining data records.
c
	do j=2,nP
	  call uvread(tno,Spreambl,Sdata(1,j),Sflags(1,j),maxchan,ntmp)
	  if(ntmp.ne.Snread)call bug('f',
     *	    'Number of channels of data changed in uvPolIni')
	  call uvrdvri(tno,'pol',P,PolMin-1)
	  if(P.ge.PolMin.and.P.le.PolMax) indx(P) = j
	  if(PolCpy)Pols(j) = P
	  caled(j) = .false.
	enddo
c
c  See which polarisations we have.
c
	circ2 = indx(PolRR).ne.0.and.indx(PolLL).ne.0
	circx = indx(PolRL).ne.0.and.indx(PolLR).ne.0
	lin2  =	indx(PolXX).ne.0.and.indx(PolYY).ne.0
	linx  = indx(PolXY).ne.0.and.indx(PolYX).ne.0
	raw = (circ2.and.circx).or.(lin2.and.linx)
c
c  We have the data records for this instant. Now we have to determine
c  how we are to form the desired polarisations from those actually
c  present.
c
	NoChi = .true.
	do i=1,nPol
	  doLkCorr = WillLeak
	  ncoeff(i) = 0
	  doaver(i) = .false.
c
c  The case of the polarisation being present in the data. Do polarisation
c  leakage correction if requested, and if the correlation is a raw
c  polarisation.
c
	  if(indx(Pols(i)).ne.0)then
	    doLkCorr = doLkCorr.and.Pols(i).lt.0
	    ncoeff(i) = 1
	    coeffs(1,i) = 1
	    type(1) = Pols(i)
c
c  The case of Stokes I, when the source is assumed unpolarised.
c  Here we use Stokes I, if it is available, otherwise we use an
c  average of RR,LL,XX and YY. Never do polarisation leakage correction.
c
	  else if(Pols(i).eq.PolII)then
	    if(indx(PolI).ne.0)then
	      ncoeff(i) = 1
	      type(1) = PolI
	      coeffs(1,i) = 1
	    else
	      if(indx(PolRR).ne.0)then
		ncoeff(i) = ncoeff(i) + 1
		coeffs(ncoeff(i),i) = 1
		type(ncoeff(i)) = PolRR
	      endif
	      if(indx(PolLL).ne.0)then
		ncoeff(i) = ncoeff(i) + 1
		coeffs(ncoeff(i),i) = 1
		type(ncoeff(i)) = PolLL
	      endif
	      if(indx(PolXX).ne.0)then
		ncoeff(i) = ncoeff(i) + 1
		coeffs(ncoeff(i),i) = 1
		type(ncoeff(i)) = PolXX
	      endif
	      if(indx(PolYY).ne.0)then
		ncoeff(i) = ncoeff(i) + 1
		coeffs(ncoeff(i),i) = 1
		type(ncoeff(i)) = PolYY
	      endif
	    endif
	    doLkCorr = .false.
	    doaver(i) = ncoeff(i).gt.1
c
c  Form Stokes I from raw polarisation.
c
	  else if(Pols(i).eq.PolI)then
	    if(circ2)then
	      ncoeff(i) = 2
	      coeffs(1,i) = (0.5,0.0)
	      coeffs(2,i) = (0.5,0.0)
	      type(1) = PolRR
	      type(2) = PolLL
	    else if(lin2)then
	      ncoeff(i) = 2
	      coeffs(1,i) = (0.5,0.0)
	      coeffs(2,i) = (0.5,0.0)
	      type(1) = PolXX
	      type(2) = PolYY
	    endif
c
c  Form Stokes Q from raw polarisations.
c
	  else if(Pols(i).eq.PolQ)then
	    if(circx)then
	      ncoeff(i) = 2
	      coeffs(1,i) = (0.5,0.0)
	      coeffs(2,i) = (0.5,0.0)
	      type(1) = PolRL
	      type(2) = PolLR
	    else if(lin2.and.linx)then
	      if(NoChi)call uvPolChi(tno,NoChi,Cos2Chi,Sin2Chi)
	      ncoeff(i) = 4
	      coeffs(1,i) = cmplx( 0.5*Cos2Chi,0.0)
	      coeffs(2,i) = cmplx(-0.5*Cos2Chi,0.0)
	      coeffs(3,i) = cmplx(-0.5*Sin2Chi,0.0)
	      coeffs(4,i) = cmplx(-0.5*Sin2Chi,0.0)
	      type(1) = PolXX
	      type(2) = PolYY
	      type(3) = PolXY
	      type(4) = PolYX
	    endif
c
c  Form Stokes U from raw polarisations.
c
	  else if(Pols(i).eq.PolU)then
	    if(circx)then
	      ncoeff(i) = 2
	      coeffs(1,i) = (0.0,-0.5)
	      coeffs(2,i) = (0.0, 0.5)
	      type(1) = PolLR
	      type(2) = PolRL
	    else if(lin2.and.linx)then
	      if(NoChi)call uvPolChi(tno,NoChi,Cos2Chi,Sin2Chi)
	      ncoeff(i) = 4
	      coeffs(1,i) = cmplx( 0.5*Sin2Chi,0.0)
	      coeffs(2,i) = cmplx(-0.5*Sin2Chi,0.0)
	      coeffs(3,i) = cmplx( 0.5*Cos2Chi,0.0)
	      coeffs(4,i) = cmplx( 0.5*Cos2Chi,0.0)
	      type(1) = PolXX
	      type(2) = PolYY
	      type(3) = PolXY
	      type(4) = PolYX
	    endif
c
c  Form Stokes V from raw polarizations.
c
	  else if(Pols(i).eq.PolV)then
	    if(circ2)then
	      ncoeff(i) = 2
	      coeffs(1,i) = ( 0.5,0.0)
	      coeffs(2,i) = (-0.5,0.0)
	      type(1) = PolRR
	      type(2) = PolLL
	    else if(linx)then
	      ncoeff(i) = 2
	      coeffs(1,i) = (0.0, 0.5)
	      coeffs(2,i) = (0.0,-0.5)
	      type(1) = PolXY
	      type(2) = PolYX
	    endif
	  endif
c
c  If polarisation leakage correction should be performed, but we do not
c  have all the raw polarisations required, then indicate that we cannot
c  do the conversion.
c
	  if(doLkCorr.and..not.raw) ncoeff(i) = 0
c
c  If ncoeff(i) is zero, this means that we cannot find a way to convert
c  from one to another. In this case, just give up on this time slot.
c
	  if(ncoeff(i).eq.0) return
c
c  The leakage correction and gain correction routines may find that they
c  cannot do the work. In this case, ncoeff(i) is set to zero.
c
c  Correct there coefficients for polarisation leakage.
c
	  if(doLkCorr) call uvLkCorr(Spreambl(4),maxPol,ncoeff(i),type,
     *	      coeffs(1,i),Leaks,nLeaks)
c
c  Calculate the sum of the weights of the coefficients -- to that
c  we can work out the correct variance later on.
c
	  SumWts(i) = 0
	  do j=1,ncoeff(i)
	    SumWts(i) = SumWts(i) + real(coeffs(j,i))**2
     *				  + aimag(coeffs(j,i))**2
	  enddo
	  if(doaver(i)) SumWts(i) = SumWts(i) / (ncoeff(i)**2)
c
c  Correct the data that we need to correct.
c
	  if(WillCal)then
	    do j=1,ncoeff(i)
	      k = indx(type(j))
	      indices(j,i) = k
	      if(.not.caled(k))call uvGnFac(Spreambl(3),
     *		real(Spreambl(4)),type(j),.false.,Sdata(1,k),
     *		Sflags(1,k),Snread)
	      caled(k) = .true.
	    enddo
c
c  Else determine the indices of the data that corresponds.
c
	  else
	    do j=1,ncoeff(i)
	      indices(j,i) = indx(type(j))
	    enddo
	  endif
	enddo
c
	uvPolIni = .true.
c
	end
c************************************************************************
	subroutine uvPolChi(tno,NoChi,Cos2Chi,Sin2Chi)
c
	implicit none
	integer tno
	logical NoChi
	real Cos2Chi,Sin2Chi
c
c  This determines the Cosine and Sine of the parallactic angle. It is
c  used to rotate polarisation data.
c
c  Input:
c    tno	The handle of the uv file.
c  Output:
c    NoChi	Set to false.
c    Cos2Chi,Sin2Chi Set to cos(2*Chi) and sin(2*Chi), respectively.
c------------------------------------------------------------------------
	real Chi
c
	NoChi = .false.
	call uvgetvrr(tno,'chi',chi,1)
	Cos2Chi = cos(2*Chi)
	Sin2Chi = sin(2*Chi)
	end
c************************************************************************
c* uvDatWRd -- Read wideband correlator data.
c& rjs
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvDatWRd(data,flags,n,nread)
c
	implicit none
	integer n,nread
	complex data(n)
	logical flags(n)
c
c  This reads wideband data, and optionally applies the gain factor. It
c  ignores end-of-file (uvDatRd should handle this).
c
c  Input:
c    n		The size of the data and flags arrays.
c  Outputs:
c    data	The uvread correlation data.
c    flags	The uvread flags data.
c    data	The correlation data.
c    nread	The number of channels read.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
	double precision time
	real baseline
c
c  Handle the case of no data.
c
	if(tno.eq.0)then
	  nread = 0
c
c  Get the data, and apply the gains if needed.
c
	else
	  if(WillPol) call bug('f',
     *		'Cannot perform polarisation processing, in uvDatWRd')
	  call uvwread(tno,data,flags,n,nread)
	  if(nread.ne.0.and.WillCal)then
	    call uvgetvrr(tno,'baseline',baseline,1)
	    call uvgetvrd(tno,'time',time,1)
	    call uvGnFac(time,baseline,0,.true.,data,flags,nread)
	  endif
	endif
c
	end
c************************************************************************
c* uvDatGti -- Get integer information about the uvDat routines.
c& rjs
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvDatGti(object,ival)
c
	implicit none
	character object*(*)
	integer ival(*)
c
c  This returns miscellaneous information about what is going on inside
c  the UVDAT routines.
c
c  Input:
c    object	This is a string describing the information to return.
c		Possible values are:
c		 'npol'	   Number of simultaneous polarisations being returned
c			   by the uvDatRd routine. Zero indicates that this
c			   could not be determined.
c		 'pols'	   Returns the polarizations that the uvDatRd
c			   routine returns. This is an array of "npol" values.
c			   If the types could not be determined, values of
c			   zero are returned.
c		 'pol'	   The last Stokes parameter, returned by uvDatRd.
c			   This may vary as each new visibility is read.
c		 'number'  The file number currently being processed.
c		 'nchan'   Number of channels.
c                'nfiles'  Number of files input
c		 'visno'   Visibility number.
c  Output:
c    ival	Integer valued output.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
	double precision visno
	integer i
c
	if(object.eq.'pol')then
	  if(tno.eq.0)
     *	    call bug('f','No file open, in UvDatGt(pol)')
	  if(nPol.gt.0) then
	    ival(1) = Pols(max(iPol,1))
	    if(ival(1).eq.PolII) ival(1) = PolI
	  else
	    call uvrdvri(tno,'pol',ival,PolI)
	  endif
c
c  Determine the number of simultaneous polarisations. This is tricky!!
c  If the user has specified it, using the "stokes" keyword, use this.
c  Otherwise if the number in the file is known, use this.
c  Otherwise get the number from the file. If it is greater than 1, and
c    polarisation selection is being used, then it is useless.
c  If we cannot determine it, return 0.
c
	else if(object.eq.'npol')then
	  if(npol.ne.0)then
	    ival(1) = nPol
	  else if(nPolF.ne.0)then
	    ival(1) = nPolF
	  else if(tno.ne.0)then
	    call uvrdvri(tno,'npol',ival,1)
	    if(SelPol.and.ival(1).gt.1) ival(1) = 0
	  else
	    ival(1) = 0
	  endif
c
c  Determine the visibility number.
c
	else if(object.eq.'visno')then
	  call uvinfo(tno,'visno',visno)
	  ival(1) = nint(visno)
	  if(WillPol)ival(1) = ival(1) - nPol + iPol
c
c  Determine all the polarisations that the uvdat routines return.
c
	else if(object.eq.'pols')then
	  if(npol.eq.0)then
	    ival(1) = 0
	  else
	    do i=1,npol
	      ival(i) = pols(i)
	      if(ival(i).eq.PolII) ival(i) = PolI
	    enddo
	  endif
c
c  Number of channels.
c
	else if(object.eq.'nchan')then
	  ival(1) = nchan
c
c  Determine the number of the file currently being processed.
c
	else if(object.eq.'number')then
	  ival(1) = pnt
	else if(object.eq.'nfiles')then
          ival(1) = nin
        else
	  call bug('f','Unrecognised object in uvDatGti')
	endif
	end
c************************************************************************
c* uvDatGtr -- Get real information about the uvDat routines.
c& rjs
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvDatGtr(object,rval)
c
	implicit none
	character object*(*)
	real rval
c
c  This returns miscellaneous information about what is going on inside
c  the UVDAT routines.
c
c  Input:
c    object	This is a string describing the information to return.
c		Possible values are:
c		 'variance'Returns variance of the data.
c		 'lstart'  Returns the linetype start value, in "rval".
c		 'lwidth'  Returns the linetype width value, in "rval".
c		 'lstep'   Returns the linetype step  value, in "rval".
c  Output:
c    rval	Real valued output.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
	double precision variance
c
	if(object.eq.'variance')then
	  call uvinfo(tno,'variance',variance)
	  rval = variance
	  if(WillPol)rval = SumWts(iPol) * rval
	else if(object.eq.'lstart')then
	  if(dodata)then
	    rval = lstart
	  else
	    rval = 1
	  endif
	else if(object.eq.'lwidth')then
	  if(dodata)then
	    rval = lwidth
	  else
	    rval = 1
	  endif
	else if(object.eq.'lstep')then
	  if(dodata)then
	    rval = lstep
	  else
	    rval = 1
	  endif
	else
	  call bug('f','Unrecognised object in uvDatGtr')
	endif
	end
c************************************************************************
c* uvDatGta -- Get character information about the uvDat routines.
c& rjs
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvDatGta(object,aval)
c
	implicit none
	character object*(*),aval*(*)
c
c  This returns miscellaneous information about what is going on inside
c  the UVDAT routines.
c
c  Input:
c    object	This is a string describing the information to return.
c		Possible values are:
c		 'name'	   The name of the file currently being processed.
c		 'ltype'   Returns linetype in "aval".
c  Output:
c    aval	Character string output.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
c
	if(object.eq.'name')then
	  if(pnt.lt.1.or.pnt.gt.nIn)
     *	    call bug('f','No file open, in UvDatGt(name)')
	  aval = InBuf(k1(pnt):k2(pnt))
	else if(object.eq.'ltype')then
	  aval = line
	else
	  call bug('f','Unrecognised object in uvDatGta')
	endif
	end
c************************************************************************
c* uvDatSet -- Set some of UvDat's behaviour.
c& rjs
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvDatSet(object,value)
c
	implicit none
	character object*(*)
	integer value
c
c  Set some of the behaviour of the UvDat routines.
c
c  Input:
c    object	This determines what is set.
c    value	The value to set it to.
c  These have the following uses:
c
c    Object:	Value:
c    'stokes'	A code giving the Stokes or polarisation parameter that
c		is to be returned by the uvDatRd routine. Several calls
c		specifying this can be made, in which case several
c		Stokes/polarisations will be returned. Valid codes for
c		polarisations are:
c		I 1, Q 2, U 3, V 4, RR -1, LL -2, RL -3, LR -4,
c		XX -5, YY -6, XY -7, YX -8, II 0.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
c
	if(object.eq.'stokes')then
	  npol = npol + 1
	  if(npol.gt.maxpol)
     *	    call bug('f','Too many polarisations, in uvDatSet')
	  if(value.lt.PolMin.or.value.gt.PolMax)
     *	    call bug('f','Invalid polarisation, in uvDatSet')
	  pols(npol) = value
	else
	  call bug('f','Invalid object in uvDatSet')
	endif
	end
c************************************************************************
c* uvDatPrb -- Determine what data have been selected in the uvdat routines.
c& rjs
c: uv-i/o,uv-data,uv-selection
c+
	logical function uvDatPrb(object,value)
c
	implicit none
	character object*(*)
	double precision value
c
c  This probes the data selection criteria. It simply calls the appropriate
c  "Select" routine which does this.
c
c  Input:
c    object	The name of the object to check. It is the corresponding
c		item to the SelProbe routine's "object" arguement.
c		routine.
c    value	The value to check whether it has been selected.
c  Output:
c    uvDatPrb	This returns the value .true. if the data could possibly be
c		selected. It does not guarantee that such data might exist
c		in any particular data file. It also has the limitation that
c		information is not present to convert "uvrange" and "uvnrange"
c		calls into each other. These should be treated with caution.
c--
c------------------------------------------------------------------------
	include 'uvdat.h'
c
c  Externals.
c
	logical SelProbe
c
	uvDatPrb = SelProbe(sels,object,value)
	end
c************************************************************************
	subroutine uvLkIni()
c
	implicit none
c
c  This reads the polarization leakage table.
c
c------------------------------------------------------------------------
	integer iostat,litem
	include 'uvdat.h'
c
c  Externals.
c
	integer hsize
c
c  Open the gains file.
c
	call haccess(tno,litem,'leakage','read',iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'accessing leakage table')
c
c  Determine the number of leakage parameters, as a consistency
c  check.
c
	nLeaks = (hsize(litem)-8)/16
	if(nLeaks.lt.1)call bug('f','Leakage table appears bad')
c
c  Read in the leakages and finish up.
c
	call hreadr(litem,Leaks,8,16*nLeaks,iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'reading leakage table')
	call hdaccess(litem,iostat)
	if(iostat.ne.0)call UvGnBug(iostat,'closing leakage table')
c
	end
c************************************************************************
	subroutine uvLkCorr(baseline,maxPol,ncoeff,type,coeffs,
     *	  Leaks,nLeaks)
c
	implicit none
	integer maxPol,ncoeff,type(maxPol),nLeaks
	complex coeffs(maxPol)
	double precision baseline
	complex Leaks(2,nLeaks)
c
c  The input represents the coefficients that relate the measured
c  polarisations to the desired. We now have to correct these
c  coefficients for the effects of polarisation leakage/cross-talk.
c
c  Input:
c    maxPol	Max number of coefficients.
c    nLeaks	Number of leakage parameters.
c    Leaks	The polarisation leakage parameters.
c    baseline	Baseline number
c  Input/Output:
c    ncoeff	Number of coefficients.
c    type	The polarisation type corresponding to each coefficient.
c    coeffs	The value of the coefficient.
c------------------------------------------------------------------------
	integer i1,i2,n,i,j
	complex G(4),t
	integer indx(4,4),cf1(4),cf2(4),off
	data indx/1,4,3,2, 2,3,4,1, 3,2,1,4, 4,1,2,3/
	data cf1 /1,2,1,2/
	data cf2 /1,2,2,1/
c
	n = ncoeff
	ncoeff = 0
c
	call basant(baseline,i1,i2)
	if(i1.lt.1.or.i1.gt.nLeaks.or.i2.lt.1.or.i2.gt.nLeaks)return
c
	off = 0
	if(type(1).le.-5)off = -4
c
	j = off - type(1)
	G(indx(1,j)) =  coeffs(1)
	G(indx(2,j)) = -coeffs(1) *       Leaks(cf1(j),i1)
	G(indx(3,j)) = -coeffs(1) * conjg(Leaks(cf2(j),i2))
	G(indx(4,j)) =  coeffs(1) * Leaks(cf1(j),i1) * 
     *			      conjg(Leaks(cf2(j),i2))
c
	do i=2,n
	  j = off - type(i)
	  G(indx(1,j)) = G(indx(1,j))
     *	     + coeffs(i)
	  G(indx(2,j)) = G(indx(2,j))
     *	     - coeffs(i) *	 Leaks(cf1(j),i1)
	  G(indx(3,j)) = G(indx(3,j))
     *	     - coeffs(i) * conjg(Leaks(cf2(j),i2))
	  G(indx(4,j)) = G(indx(4,j))
     *	     + coeffs(i) * Leaks(cf1(j),i1) * conjg(Leaks(cf2(j),i2))
	enddo
c
	t =  1. / ((1 - Leaks(1,i1)*Leaks(2,i1)) *
     *	      conjg(1 - Leaks(1,i2)*Leaks(2,i2)) )
	ncoeff = 4
	do i=1,4
	  coeffs(i) = t*G(i)
	  type(i) = off - i
	enddo
c	
	end
