c************************************************************************
c
c  The Select routines are used to parse and apply uv data selection.
c
c  History:
c    rjs   6oct89 Original version.
c    rjs  13mar90 Parsed times in the standard format. The SelProbe routine.
c		  Better accuracy for times.
c    rjs/bpw 6apr90 Changes to accomodate new amplitude range handling
c		  by uvio.c.
c    rjs  16apr90 A fudge to force correct rounding on the Sun. This avoids
c		  the need to specify the -fstore switch when compiling
c		  with the f68881 floating point processor.
c    rjs  17oct90 Added Polarisation and "on" selection.
c    rjs   6feb91 Added "query" mode to SelProbe. select=time() can give
c		  just one time.
c    rjs   6mar91 Improved an error message.
c    rjs  11apr91 Changed atod to atodf.
c    rjs  13apr91 Added shadowing selection.
c    rjs  18may91 Corrected declaration of "type" to be char*12, as found
c		  by bpw. Recognise upper and lower case polarisation mnemonics.
c    rjs  24jul91 Corrected lack of checks in SelProbe for DAYTIME
c		  specification.
c    rjs  25jul91 Improved the SelProbe routine somewhat.
c    mjs  04aug91 Replaced hardcoded MAXANT(S) by including maxdim.h
c    rjs  05sep91 DAYTIME values can cross day boundaries.
c    rjs  22nov91 Added select=auto.
c    rjs  25mar92 Added frequency selection.
c    rjs  ??????? Source selection.
c    rjs   2nov92 Documentation changes only.
c    rjs  23sep93 improve misleading error messages.
c    rjs  22jul94 Added ra and dec selection.
c    rjs   4sep94 Remove char*(*) from subroutine call.
c
c  Routines are:
c    subroutine SelInput(key,sels,maxsels)
c    subroutine SelApply(tno,sels,select)
c    logical function SelProbe(sels,object,value)
c
c  The user can select based on the subcommands:
c    time(t1,t2)	Time given in ??. If one value is given, zero is
c			assumed for t1.
c    amplitude(a1,a2)	The range of values of amplitudes. If only one
c			number is given, a2 is assumed to be infinity.
c    uv(uv1,uv2)	Range in uv, in kilowavelengths. If only one
c			number is given, uv1 is assumed to be 0.
c    uvn(uv1,uv2)	Range in uv, in nanosecs. If only one
c			number is given, uv1 is assumed to be 0.
c    visibility(n1,n2)	Range of visibilities, ranging from 1 to the
c			number of visibilities. If one value is given, then
c			n1 and n2 are assumed the same.
c    window(w1,w2,...)	Spectra windows to select. Used only for velocity
c			line type.
c    antennae(a1,a2..)(b1,b2..) All baselines made from a1*b1,a1*b2,.., a2*b1,
c			a2*b2,...an*b1,an*b2,...an*bn are selected. If only
c			one set of values are given, then all baselines
c			associated with those antennae are selected.
c    pointing(p1,p2)	Data with rms pointing (in arcsec) in range p1 to
c			p2 is selected. If only one number is given, p1 is
c			assumed to be 0.
c    or			"OR" some selections together.
c    dra(p1,p2)		Data with "dra" parameter (in arcsec) between two
c			limits. Both limits must be given.
c    ddec(p1,p2)	Data with "ddec" parameter (in arcsec) between two
c			limits. Both limits must be given.
c    increment(n)	Every nth visibility is selected.
c    on			Those records when the variable "on" == 1.
c    polarization(x)	Select records of a particular polarisation,
c			where "x" is one of:
c			 i,q,u,v,rr,ll,rl,lr,xx,yy,xy,yx
c    shadow(x)		Select records that are shadowed by dishes of diameter
c			"x" meters.
c    auto		Select autocorrelation data.
c    freq(lo,hi)	Frequency selection, based on sky frequency of the
c			first channel.
c    source(src1,src2...) Select by source.
c    ra(hh:mm:ss,hh:mm:ss) Select by RA.
c    dec(dd:mm:ss,dd:mm:ss) Select by DEC.
c
c  The input command would look something like:
c    select=time(t1,t2),uv(uv1,uv2),...
c
c  Each subcommand can be prefixed with a + or -, to indicate to select or
c  to discard, respectively. If the + or - is absent, + is assumed.
c
c  The users input is broken down into an array "sels". The first entry
c  in "sels" is the number of subcommands contained in it. Each subcommand
c  is encoded as a variable number of reals. The first is a "type"
c  indicator, the second is a count, "n", of the data that follows.
c  Type can be positive or negative (for select or discard), indicating
c  the thing (time,visibility,window, etc) of interest. The data associated
c  with each command in some way encodes the users selection. Usually it
c  consists of a hogh and low value to select.
c
c************************************************************************
c* SelProbe -- Check if a particular uv data has been selected.
c& rjs
c: uv-selection,uv-i/o
c+
	logical function SelProbe(sels,object,value)
c
	implicit none
	character object*(*)
	real sels(*)
	double precision value
c
c  This routine checks whether particular uv data have been selected by
c  the user. It returns .true. if so, or .false. otherwise.
c
c  Inputs:
c    sels	The intermidate form of the uv selection, passed back
c		by SelInput.
c    object	The type of value to check. Possible values are:
c		  Object:		Units of Value:
c		  'time'		Julian day.
c		  'antennae'		Baseline number = 256*ant1 + ant2.
c					One of ant1 or ant2 can be zero.
c		  'uvrange'		Wavelengths.
c		  'uvnrange'		Nanoseconds.
c		  'visibility'		Visibility number (1 relative).
c		  'dra'			Radians.
c		  'ddec'		Radians.
c		  'pointing'		Arcseconds.
c		  'amplitude'		Same as correlation data.
c		  'window'		Window Number.
c		  'on'			On switch.
c		  'polarization'	Polarization type.
c		  'shadow'		Shadowing.
c		  'frequency'		Frequency selection.
c		  'source'		Select by source.
c		  'ra'			Select by RA.
c		  'dec'			Select by DEC.
c		Note that this does not support all objects to uvselect.
c		The object name may have a suffix of '?' (e.g. 'window?')
c		in which case the "value" argument is ignored, and SelProbe
c		checks if any selection bassed on this object has been
c		performed.
c		The name must be given in full (no abbreviations and case
c		is significant).
c    value	The value to check whether it has been selected.
c  Output:
c    SelProbe	This returns the value .true. if the data could possibly be
c		selected. It does not guarantee that such data might exist
c		in any particular data file. It also has the limitation that
c		information is not present to convert "uvrange" and "uvnrange"
c		calls into each other. These should be treated with caution.
c--
c------------------------------------------------------------------------
	real val1,val2
	integer ant1,ant2,seltype1,seltype2,t1,t2,nsel,length,i,j
	integer offset,n
	logical first,query,match
	double precision lval,hval
c
c  Externals.
c
	integer len1
c
	include 'select.h'
c
c  Determine the object type.
c
	seltype1 = 0
	length = len1(object)
	if(length.gt.0)then
	  query = object(length:length).eq.'?'
	  if(query) length = length - 1
	endif
	if(length.le.len(types(1)).and.length.gt.0)then
	  do i=1,ntypes
	    if(object(1:length).eq.types(i))seltype1 = i
	  enddo
	endif
	if(seltype1.eq.0)
     *	  call SelBug(object,'Unrecognised object, in SelProbe')
	if((seltype1.eq.OR.or.seltype1.eq.INC).and..not.query)
     *	  call SelBug(object,'Illegal object, in SelProbe')
c
c  Some selections are manifested in two forms in the SELS arrays,
c  and it is possible to convert between the two. Seltype2 is the
c  type of the alternate form.
c
	if(seltype1.eq.TIME)then
	  seltype2 = DAYTIME
	else
	  seltype2 = seltype1
	endif
c
	nsel = nint(sels(1))
c
c  If its a query as to whether the particular object is used in the
c  selection, check through for that object.
c
	if(query)then
	  offset = 3
	  SelProbe = .true.
	  do i=1,nsel
	    t1 = abs(nint(sels(offset+ITYPE)))
	    if(t1.eq.Seltype1.or.t1.eq.Seltype2)return
	    offset = offset + nint(sels(offset+NSIZE))
	  enddo
	  SelProbe = .false.
	  return
	endif
c
c  Convert to the units used in this routine.
c	     Calling Units  Units in Sels Array
c	     -------------  -------------------
c  ANTS	     baseline no.   2 antenna nos (ant1,ant2)
c  TIME	     Julian day	    Offset Julian day (val1) or day-fraction (val2).
c
	if(seltype1.eq.ANTS)then
	  t1 = nint(value)
	  t2 = t1/256
	  t1 = t1 - 256*t2
	  ant1 = min(t1,t2)
	  ant2 = max(t1,t2)
	else if(Seltype1.eq.TIME)then
	  val1 = value - sels(2)
	  val2 = value - nint(value-1.d0) - 0.5
	else
	  val1 = value
	endif
c
c  Run around in circles seeing whats what.
c
	first = .true.
	SelProbe = .true.
c
c  Handle antennae.
c
	offset = 3
	do i=1,nsel
	  t1 = abs(nint(sels(offset+ITYPE)))
	  if(t1.eq.OR)then
	    if(SelProbe) return
	    first = .true.
	  else if(t1.eq.seltype1.or.t1.eq.seltype2)then
	    if(t1.eq.DAYTIME)then
	      match = sels(offset+LOVAL).le.val2.and.
     *		      val2.le.sels(offset+HIVAL)
	    else if(t1.eq.AUTO.or.t1.eq.ON)then
	      match = .true.
	    else if(t1.eq.WINDOW.or.t1.eq.POL)then
	      match = .false.
	      n = nint(sels(offset+NSIZE)) - 2
	      do j=1,n
		match = match.or.nint(sels(offset+j+1)-val1).eq.0
	      enddo
	    else if(t1.eq.ANTS)then
	      t1 = nint(min(sels(offset+LOVAL),sels(offset+HIVAL)))
	      t2 = nint(max(sels(offset+LOVAL),sels(offset+HIVAL)))
	      match = (t1.eq.0.and.t2.eq.0).or.
     *		 (t1.eq.0.and.(t2.eq.ant1.or.t2.eq.ant2)).or.
     *		 (ant1.eq.0.and.(ant2.eq.t1.or.ant2.eq.t2)).or.
     *		 (ant1.eq.t1.and.ant2.eq.t2)
	    else if(t1.eq.RA.or.t1.eq.DEC)then
	      lval = dble(sels(offset+LOVAL)) + dble(sels(offset+MDVAL))
	      hval = dble(sels(offset+HIVAL)) + dble(sels(offset+MDVAL))
	      match = lval.le.val1.and.val1.le.hval
	    else
	      match = sels(offset+LOVAL).le.val1.and.
     *		      val1.le.sels(offset+HIVAL)
	    endif
	    if(match)then
	      SelProbe = sels(offset+ITYPE).gt.0
	    else if(first)then
	      SelProbe = sels(offset+ITYPE).lt.0
	    endif
	    first = .false.
	  endif
	  offset = offset + nint(sels(offset+NSIZE))
	enddo
	end
c************************************************************************
c* SelInput -- Get the users uv selection specification.
c& rjs
c: uv-selection,uv-i/o
c+
	subroutine SelInput(key,sels,maxsels)
c
	implicit none
	character key*(*)
	integer maxsels
	real sels(maxsels)
c
c  This gets the specification of the selected uv data from the key
c  routines.
c
c  Inputs:
c    key	Keyword to be used with the key routine. Usually this is
c		"select".
c    maxsels	The size of the "sels" array. 500 elements is
c		more than adequate. This would be a complicated selection
c		for an array with many antennae.
c
c  Output:
c    sels	Intermediate form of the selection.
c
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer MAXVALS
	parameter(MAXVALS=32)
	integer offset,sgn,k1,k2,n,n1,n2,i,j,i1,i2,seltype,length,nsels
	double precision ant1(MAXANT),ant2(MAXANT)
	double precision vals(MAXVALS)
	real time0
	character spec*80,type*12
	logical more
c
c  Externals.
c
	logical keyprsnt
	integer len1
	include 'select.h'
c
	offset = 3
	time0 = 0
	nsels = 0
	more = keyprsnt(key)
	if(more) call keya(key,spec,' ')
	dowhile(more)
	  k1 = 1
	  k2 = len1(spec)
	  if(offset+2.gt.maxsels)
     *		call SelBug(spec,'Selection expression too complex')
c
c  Determine if its an inclusion or exclusion command.
c
	  sgn = +1
	  if(spec(1:1).eq.'-') sgn = -1
	  if(spec(1:1).eq.'+'.or.spec(1:1).eq.'-') k1 = k1 + 1
c
c  Determine the subcommand type.
c
	  call GetTok(spec,k1,k2,type,length)
	  seltype = 0
	  if(length.le.len(types(1)).and.length.gt.0)then
	    do i=1,ntypes
	      if(type(1:length).eq.types(i)(1:length))then
		if(seltype.ne.0) call SelBug(spec,
     *		  'Ambiguous selection command')
	        seltype = i
	      endif
	    enddo
	  endif
c
c  Process each subcommand.
c  Handle fairly normal loval/hival type selection.
c
	  if(   seltype.eq.VISNO.or.seltype.eq.INC.or.
     *		seltype.eq.UV.or.seltype.eq.POINT.or.
     *		seltype.eq.AMP.or.seltype.eq.UVN.or.
     *		seltype.eq.DRA.or.seltype.eq.DDEC.or.
     *		seltype.eq.SHADOW.or.seltype.eq.FREQ)then
	    call SelDcde(spec,k1,k2,vals,n,2,'real')
c
c  Expand to two parameters, using some default mechanism.
c
	    if(n.eq.1)then
	      vals(2) = vals(1)
	      if(seltype.eq.DRA.or.seltype.eq.DDEC)then
		vals(2) = vals(2) + 1
		vals(1) = vals(1) - 1
	      else if(seltype.eq.FREQ)then
		vals(2) = 1.01 * vals(2)
		vals(1) = 0.99 * vals(1)
	      else if(seltype.ne.VISNO.and.seltype.ne.INC)then
		vals(1) = 0
		if(seltype.eq.AMP) sgn = -sgn
	      endif
	    else if(seltype.eq.SHADOW.or.seltype.eq.INC)then
	      call SelBug(spec,'Too many parameter values')
	    endif
c
c  Perform unit conversion.
c
	    if(seltype.eq.DRA.or.seltype.eq.DDEC)then
	      vals(1) = vals(1) * pi/180/3600
	      vals(2) = vals(2) * pi/180/3600
	    else if(seltype.eq.UV)then
	      vals(1) = 1000 * vals(1)
	      vals(2) = 1000 * vals(2)
	    endif
c
c  Store away the bacon.
c
	    if(offset+4.gt.MAXSELS)
     *		call Selbug(spec,'Selection too complex')
	    sels(offset+LOVAL) = vals(1)
	    sels(offset+HIVAL) = vals(2)
	    sels(offset+ITYPE) = sgn*seltype
	    sels(offset+NSIZE) = 4
	    offset = offset + 4
	    nsels = nsels + 1
c
c  Handle RA and DEC selection.
c
	  else if(seltype.eq.RA.or.seltype.eq.DEC)then
	    if(seltype.eq.RA)call SelDcde(spec,k1,k2,vals,n,2,'ra')
	    if(seltype.eq.DEC)call SelDcde(spec,k1,k2,vals,n,2,'dec')
c
	    if(n.eq.1)then
	      vals(2) = vals(1) + pi/180/3600
	      vals(1) = vals(1) - pi/180/3600
	    endif
c
	    call SelFudge(sels(offset+MDVAL),0.5*(vals(1)+vals(2)))
	    call SelFudge(sels(offset+LOVAL),vals(1)-sels(offset+MDVAL))
	    call SelFudge(sels(offset+HIVAL),vals(2)-sels(offset+MDVAL))
	    if(offset+5.gt.MAXSELS)
     *		call Selbug(spec,'Selection too complex')
	    sels(offset+ITYPE) = sgn*seltype
	    sels(offset+NSIZE) = 5
	    offset = offset + 5
	    nsels = nsels + 1
c
c  Handle  "SOURCE" selection.
c
	  else if(seltype.eq.SOURCE)then
	    if(k1+2.gt.k2)
     *		call SelBug(spec,'Bad source name list')
	    if(spec(k1:k1).ne.'('.or.spec(k2:k2).ne.')')
     *		call SelBug(spec,'Bad source name list')
	    length = 0
	    do j=k1+1,k2
	      if(spec(j:j).eq.','.or.j.eq.k2)then
		if(length.eq.0)
     *		  call SelBug(spec,'Zero length source name')
		sels(offset+ITYPE) = sgn*seltype
		sels(offset+NSIZE) = length + 2
		offset = offset + length + 2
		nsels = nsels + 1
		length = 0
	      else
		length = length + 1
		if(offset+length+1.gt.maxsels)
     *		  call SelBug(spec,'Selection expression too complex')
		sels(offset+length+1) = ichar(spec(j:j))
	      endif
	    enddo
c
c  Handle "OR", "ON", or "AUTO" selection. No parameters associated with
c  these.
c
	  else if(seltype.eq.ON.or.seltype.eq.AUTO.or.seltype.eq.OR)then
	    sels(offset+ITYPE) = sgn*seltype
	    sels(offset+NSIZE) = 2
	    offset = offset + 2
	    nsels = nsels + 1
c
c  Handle time selection. Times can be given in either a "day" or
c  "absolute" form. The absolute time gives the full time specification
c  (in the form yymmmdd:hh:mm:ss.s -- the hh:mm:ss.s can be truncated)
c  The day form gives hh:mm:ss.s (or some truncation). Either 1 or 2 times
c  can be given.
c
c  If two absolute times are given -- all is OK.
c  If one absolute time is given, then this time, plus 1 day is also matched.
c  If two day times are given, then all data (regardless of the day) that fall
c  within the given times are matched.
c
	  else if(seltype.eq.TIME)then
	    call SelDcde(spec,k1,k2,vals,n,2,'date')
	    if(n.eq.1)then
	      if(vals(1).le.1)
     *		call SelBug(spec,'Two day times must be given')
	      vals(2) = vals(1) + 1
	    endif
	    if(min(vals(1),vals(2)).lt.1.and.max(vals(1),vals(2)).gt.1)
     *	       call SelBug(spec,'You cannot mix day and absolute times')
	    if(vals(1).gt.1)then
	      if(time0.eq.0) call SelFudge(time0,vals(1))
	      sels(offset+ITYPE) = sgn*TIME
	      sels(offset+LOVAL) = vals(1) - time0
	      sels(offset+HIVAL) = vals(2) - time0
	    else
	      sels(offset+ITYPE) = sgn*DAYTIME
	      sels(offset+LOVAL) = vals(1)
	      if(vals(1).gt.vals(2))then
		sels(offset+HIVAL) = 1
		sels(offset+NSIZE) = 4
		offset = offset + 4
		nsels = nsels + 1
		if(offset+3.gt.maxsels)
     *			call SelBug(Spec,'Buffer overflow')
		sels(offset+ITYPE) = sgn*DAYTIME
		sels(offset+LOVAL) = 0
	      endif
	      sels(offset+HIVAL) = vals(2)
	    endif
	    sels(offset+NSIZE) = 4
	    offset = offset + 4
	    nsels = nsels + 1
c
c  Handle antennae.
c
	  else if(seltype.eq.ANTS)then
	    call SelDcde(spec,k1,k2,ant1,n1,MAXANT,'real')
	    if(k1.gt.k2)then
	      n2 = 1
	      ant2(1) = 0
	    else
	      call SelDcde(spec,k1,k2,ant2,n2,MAXANT,'real')
	    endif
c
	    do i1=1,n1
	      do i2=1,n2
		if(offset+3.gt.maxsels)
     *		    call SelBug(spec,'Buffer overflow')
		sels(offset+ITYPE) = sgn*seltype
		sels(offset+LOVAL) = nint(ant1(i1))
		sels(offset+HIVAL) = nint(ant2(i2))
		sels(offset+NSIZE) = 4
		offset = offset + 4
		nsels = nsels + 1
	      enddo
	    enddo
c
c  Handle windows and polarisation.
c
	  else if(seltype.eq.WINDOW.or.seltype.eq.POL)then
	    if(seltype.eq.WINDOW)
     *    		call SelDcde(spec,k1,k2,vals,n,MAXVALS,'real')
	    if(seltype.eq.POL)
     *		call SelDcde(spec,k1,k2,vals,n,MAXVALS,'pol')
	    if(offset+n+1.gt.maxsels)
     *		call SelBug(spec,'Buffer overflow')
	    sels(offset+ITYPE) = sgn*seltype
	    sels(offset+NSIZE) = n + 2
	    do i=1,n
	      sels(offset+1+i) = nint(vals(i))
	    enddo
	    offset = offset + nint(sels(offset+NSIZE))
	    nsels = nsels + 1
c
c  Unrecognised sort of selection.
c
	  else
	    call SelBug(spec,'Unrecognised selection command')
	  endif
c
	  more = keyprsnt(key)
	  if(more) call keya(key,spec,' ')
	enddo
c
	sels(1) = nsels
	sels(2) = time0
c
	end
c************************************************************************
	subroutine SelFudge(rval,dval)
c
	implicit none
	real rval
	double precision dval
c
c  This routine is called to ensure the correct rounding of the rval
c  variable, in the conversion from double to single precision. This
c  prevents an optimisation on Sun-3 computers equiped with f68881
c  floating point hardware, which stuffs up the rounding that the Select
c  routines rely on. We could have forced correct rounding on the Sun
c  by specifying -fstore, but this unnecessarily inhibits optimisation
c  in general.
c
c  Input:
c    dval	The input double precision value.
c  Output:
c    rval	The output, real and rounded value.
c------------------------------------------------------------------------
	rval = dval
	end
c************************************************************************
	subroutine SelBug(spec,message)
c
	implicit none
	character message*(*),spec*(*)
c
c  This generates an error message when a selection command appears
c  no good. This never returns.
c
c  Input:
c    spec	The subregion command.
c    message	The error message.
c
c------------------------------------------------------------------------
	character line*80
	integer l
c
c  Externals.
c
	integer len1
c
	l = len1(spec)
	line = message//': '//spec(1:l)
	l = min(len(line),l + 2 + len(message))
	call bug('f',line(1:l))
	end
c************************************************************************
	subroutine SelDcde(spec,k1,k2,vals,n,nmax,format)
c
	implicit none
	character spec*(*)
	integer k1,k2,nmax,n
	double precision vals(nmax)
	character format*(*)
c
c  Decode some reals or times, that are enclosed within brackets.
c  Format: real		'(1.,2.,3.,4.)'
c  	   date		'(90mar02.5,90mar02:12:30:12.5)'
c	   pol		'(rr,ll)'
c	   ra		'(19:34,19:35)'
c	   dec		'(-63.8,-63.9)'
c
c  Input:
c    spec	The string containing the piece to be decoded.
c    nmax	The maximum number of reals to be returned.
c    format	'real' - real values.
c		'date' - Times in Miriad format.
c		'pol' - A polarization code (rr,ll, etc).
c		'ra' - Right ascension (hours).
c		'dec' - Declination (degrees).
c  In/Out:
c    k1,k2	This indicates the substring of spec left to process.
c  Output:
c    vals	Array containing the reals found.
c    n		The number of reals found.
c
c------------------------------------------------------------------------
	integer k0
	logical more,ok
	character umsg*64
c
c  Externals.
c
	integer PolsP2C
c
	if(spec(k1:k1).ne.'(')
     *	  call SelBug(spec,'Bad selection subcommand')
	k1 = k1 + 1
	k0 = k1
	n = 0
	more = .true.
	dowhile(k1.le.k2.and.more)
	  if(spec(k1:k1).eq.','.or.spec(k1:k1).eq.')')then
	    more = spec(k1:k1).eq.','
	    if(k1.le.k0)call SelBug(spec,'Bad selection subcommand')
	    n = n + 1
	    if(n.gt.nmax)
     *	    call SelBug(spec,'Too many values in selection subcommand')
	    ok = .true.
	    if(format.eq.'real')then
	      call atodf(spec(k0:k1-1),vals(n),ok)
	    else if(format.eq.'date')then
	      call dayjul(spec(k0:k1-1),vals(n))
	    else if(format.eq.'pol')then
	      if(k1-k0.ne.1.and.k1-k0.ne.2)
     *		call SelBug(spec,'Bad polarization type')
	      vals(n) = PolsP2C(spec(k0:k1-1))
	    else if(format.eq.'ra')then
	      call decangle(spec(k0:k1-1),vals(n),'hms',ok)
	    else if(format.eq.'dec')then
	      call decangle(spec(k0:k1-1),vals(n),'dms',ok)
	    else
	      umsg = 'Unrecognised format in SelDcde: '//format
	      call bug('f',umsg)
	    endif
	    if(.not.ok)call SelBug(spec,'Error decoding a value')
	    k0 = k1 + 1
	  endif
	  if(more) k1 = k1 + 1
	enddo
c
c  Do some more checks.
c
	if(k1.gt.k2)call SelBug(spec,'Bad selection subcommand')
	if(spec(k1:k1).ne.')')call SelBug(spec,
     *				     'Bad selection subcommand')
	k1 = k1 + 1
	if(n.eq.0)
     *	  call SelBug(spec,'Bad parameters in selection command')
c
	end
c************************************************************************
c* SelApply -- Call the appropriate uv routine to set the uv selection.
c& rjs
c: uv-selection,uv-i/o
c+
	subroutine SelApply(tno,sels,select)
c
	implicit none
	integer tno
	real sels(*)
	logical select
c
c  This calls the uvselect routine, to indicate what data is to be
c  selected, etc.
c
c  Inputs:
c    tno	The handle of the uv data file.
c    sels	The intermediate form of the selection.
c    select	If true, the data is selected, otherwise the data is
c		discarded.
c------------------------------------------------------------------------
	integer i,j,nsels,n,type,offset
	double precision lovalue,hivalue,time0
	logical flag
	character string*64
	include 'select.h'
c
	nsels = nint(sels(1))
	time0 = sels(2)
	offset = 3
c
	do i=1,nsels
	  type = abs(nint(sels(offset+ITYPE)))
	  flag = select.eqv.(sels(offset+ITYPE).gt.0)
	  n = nint(sels(offset+NSIZE)) - 2
c
c Things with 0 parameters
c
	  if(n.eq.0)then
	    call uvselect(tno,types(type),0.0d0,0.0d0,flag)
c
c  Windows and polarisation.
c
	  else if(type.eq.POL.or.type.eq.WINDOW)then
	    do j=1,n
	      call uvselect(tno,types(type),dble(sels(offset+j+1)),
     *		0.0d0,flag)
	    enddo
c
c  Source names.
c
	  else if(type.eq.SOURCE)then
	    if(n.gt.len(string))
     *		call bug('f','String buffer overflow, in SelApply')
	    do j=1,n
	      string(j:j) = char(nint(sels(offset+j+1)))
	    enddo
	    call uvsela(tno,types(type),string(1:n),flag)
c
c  Right ascension and declination.
c
	  else if(type.eq.RA.or.type.eq.DEC)then
	    lovalue = dble(sels(offset+LOVAL))+dble(sels(offset+MDVAL))
	    hivalue = dble(sels(offset+HIVAL))+dble(sels(offset+MDVAL))

	    call uvselect(tno,types(type),lovalue,hivalue,flag)
c
c  Things with two values.
c
	  else if(n.eq.2)then
	    lovalue = sels(offset+LOVAL)
	    hivalue = sels(offset+HIVAL)
	    if(type.eq.TIME)then
	      lovalue = lovalue + time0
	      hivalue = hivalue + time0
	    else if(type.eq.DAYTIME)then
	      type = TIME
	    endif
	    call uvselect(tno,types(type),lovalue,hivalue,flag)
	  else
	    call bug('f','Internal bug, in SelApply')
	  endif
c
	  offset = offset + n + 2
	enddo
	end
