c************************************************************************
c  The key routines provide keyword-oriented access to the command line.
c
c  History:
c    rjs   Dark-ages  Original version.
c    bs    ???88      Converted it to use iargc and getarg. Added -f flag.
c    rjs   8sep89     Improved documentation.
c    nebk  10sep89    Added mkeyr.  I think rjs will not like it (Too right!).
c    rjs   19oct89    Major rewrite to handle @ files.
c    rjs   15nov89    Added keyf routine, and did the rework needed to support
c		      this. Added mkeyf. Modified mkeyr.
c    pjt   26mar90    Added mkeya. like mkeyr (again, bobs will not like this)
c    pjt   10apr90    some more verbose bug calls.
c    rjs   23apr90    Made pjt's last changes into standard FORTRAN (so the
c		      Cray will accept it).
c    pjt   10may90    Make it remember the programname in keyini (se key.h)
c                     for bug calls - introduced progname
c    rjs   22oct90    Check for buffer overflow in keyini.
c    pjt   21jan91    Added mkeyi, variable index is now idx, exp is expd
c    pjt    5mar91    atod -> atodf
c    pjt   18mar91    increased arg buffer-length in keyini 80->256
c                     (working on -h flag; bug verbosities)
c    pjt   25jun91    allow .TRUE. and .FALSE. as logicals too (keyl)
c    nebk  14oct91    Provide more space in PBUF for contents of @files
c                     in KEYREAD
c    pjt    1feb92    Added -k flag to display keywords
c    pjt   13jul92    Provide more space in keyini: 256 char / argument
c    rjs    2sep92    Correctly handle blanks at the end of line in @ files.
c		      Add -? flag.
c    nebk  23nov92    Add mkeyd.  rjs spits dummy.
c    rjs   04dec92    Rewrite keyi, so that mchw's new hex/octal conversion
c		      routine is used.
c    rjs   19sep95    Extra checks.
c    rjs   19feb97    More robust to spaces in .def files.
c    rjs   25jul97    Better reading of .def and @ files.
c    rjs   16dec97    Added keyinic and keyputc routines. Some tidying.
c    rjs   30aug99    Increase a buffer.
c************************************************************************
c* KeyIni -- Initialise the `key' routines.
c& pjt
c: user-input, command-line
c+
	SUBROUTINE keyini
c
	implicit none
c
c  Keyini obtains the command line, and performs some initial parsing
c  of it, breaking it up into its keyword=value pairs.
c  It also remembers the name of the program, which is currently only
c  used by the bug routines.
c
c--
c  Keyini marches successively through the argument list, calling keyput
c  on each argument.  If an argument is '-f' then the next argument is
c  taken as a parameter file, and keyput is called on each line of the
c  file.
c
c  Variables
c	arglen		The length of the argument character string
c	argnum		The number of the argument
c	arg		The argument buffer
c
c------------------------------------------------------------------------
	integer status,lun,arglen,argnum,narg
	character arg*1024,argv0*32
c
c  Externals.
c
	integer iargc,len1
c
c  Initialise the common variables.
c
	call keyinic
c
c  Get the programname
c
        call getarg(0,argv0)
c
c  Loop through the arguments
c
	narg = iargc()
	argnum = 1
	do while( argnum .le. narg )
	  call getarg(argnum, arg)
	  argnum = argnum + 1
c
c  If '-f' then read the parameters from a file
c
	  if( arg .eq. '-f' ) then
	    if(argnum .gt. narg) call bug('f',
     *          'KeyIni: No parameter file given for -f option')
	    call getarg(argnum, arg)
	    argnum = argnum + 1
	    call txtopen(lun,arg,'old',status)
	    if(status.ne.0) call bug('f','KeyIni: ' //
     *	        'Failed to open parameter file ' // arg(1:len1(arg)))
	    call keylget(lun,arg,arglen)
	    do while(arglen.gt.0)
	      call keyput(arglen,arg)
	      call keylget(lun,arg,arglen)
	    enddo
	    call txtclose(lun)
c
c  If -? give help.
c
	  else if(arg.eq.'-?')then
	    call command('mirhelp '//argv0)
	    stop
c
c  Other flags are not understood yet
c
          else if(arg(1:1) .eq. '-') then
            call bug('w','Flag '//arg(1:len1(arg))//' not understood')
c
c  Otherwise the argument is a parameter
c
	  else
	    arglen = len1(arg)
	    if(arglen.eq.len(arg)) then
              call output('Read: '//arg)
              call bug('f',
     *		'KeyIni: Input parameter too long for buffer')
            endif
	    call keyput(arglen,arg)
	  endif
	enddo
c
	end
c************************************************************************
	subroutine keyinic
c
	implicit none
	include 'key.h'
c
c  Initialise the common variables.
c
	nkeys = 0
	end
c************************************************************************
	subroutine keyputc(string)
c
	implicit none
	character string*(*)
c
c  Add a keyword to the buffer.
c------------------------------------------------------------------------
	integer len1
	call keyput(len1(string),string)
	end
c************************************************************************
	subroutine keyput(arglen,arg)
c
	implicit none
	integer arglen
	character arg*(*)
c
c  Read in the input parameters from an input text file. Save the
c  parameters as counted strings in the PBUF buffer.
c
c------------------------------------------------------------------------
	integer next,lvalue,lkey,buflen
	character key*8
	include 'key.h'
c
c  Externals.
c
	logical keyprsnt
c
c  Get the keyword
c
	next = 1
	call gettok(arg,next,arglen,key,lkey)
	if(lkey.le.0.or.lkey.gt.len(key))then
	  call bug('w','Keyword name has zero length or too long')
	  return
	endif
c
c  See if we already have this keyword
c
	if( keyprsnt(key) ) then
	  call bug('w','Multiple occurences of keyword '''//
     *		key(1:lkey)//''' ignored')
	  return
	endif
c
	call spanchar(arg,next,arglen,' ')
	call spanchar(arg,next,arglen,'=')
	call spanchar(arg,next,arglen,' ')
	lvalue = arglen - next + 1
	if(lvalue.le.0)then
	  call bug('w','Zero length value for keyword '//key)
	  return
	endif
c
c  Save this parameter value.
c
	if(nkeys.eq.0)then
	  buflen = 0
	else
	  buflen = k2(nkeys)
	endif
	if(nkeys.eq.maxkeys.or.buflen+lvalue.gt.maxlen)
     *	  call bug('f','KeyIni: Parameter buffer overflow')
	nkeys = nkeys + 1
	keys(nkeys) = key(1:lkey)
	k1(nkeys) = buflen + 1
	k2(nkeys) = buflen + lvalue
	lu(nkeys) = 0
	expanded(nkeys) = .false.
	pbuf(buflen+1:buflen+lvalue) = arg(next:arglen)
	end
c************************************************************************
	subroutine keyget(key,flag,value,length)
c
	implicit none
	character key*(*),value*(*),flag*1
	integer length
c
c  Locate a keyword within a parameter buffer.
c
c------------------------------------------------------------------------
	include 'key.h'
	integer lkey,idx,i,lun,iostat
	logical more,expd
	character line*64
c
c  See if we can find the keyword.
c
	lkey = min(len(keys(1)),len(key))
	more = .true.
	dowhile(more)
	  idx = 0
	  i = nkeys
	  dowhile(idx.eq.0.and.i.gt.0)
	    if(keys(i).eq.key(1:lkey)) idx = i
	    i = i - 1
	  enddo
c
c  Return if nothing was found.
c
	  if(idx.eq.0)then
	    length = 0
	    more = .false.
c
c  If something was found, and get the parameter.
c
	  else
	    call getfield(pbuf,k1(idx),k2(idx),value,length)
	    expd = expanded(idx)
c
c  Trim back the buffer, if this keyword was exhausted.
c
	    if(k1(idx).gt.k2(idx))then
	      lun = lu(idx)
	      nkeys = nkeys - 1
	      do i=idx,nkeys
		expanded(i) = expanded(i+1)
		lu(i) = lu(i+1)
		k1(i) = k1(i+1)
		k2(i) = k2(i+1)
		keys(i) = keys(i+1)
	      enddo
c
c  Read in more stuff, if there is more to go.
c
	      if(lun.ne.0)call keyread(lun,key)
	    endif
c
c  If the value starts with a @ character, open the given file name
c
	    if(length.gt.1.and.value(1:1).eq.'@')then
	      call txtopen(lun,value(2:length),'old',iostat)
	      if(iostat.ne.0) then
		line = 'KeyGet: Error opening @ file '//value(2:length)
		call bug('w',line)
		call bugno('f',iostat)
              endif 
	      call keyread(lun,key)
	      more = .true.
c
c  If the flag says that we are to do wildcard expansion, then do it.
c
	    else if(flag.eq.'*'.and..not.expd)then
	      call keyexp(key,value(1:length))
	      more = .true.
	    else
	      more = .false.
	    endif
	  endif
	enddo
	end
c************************************************************************
	subroutine keyexp(key,value)
c
	implicit none
	character key*(*),value*(*)
c
c  Expand wildcards.
c
c------------------------------------------------------------------------
	integer buflen,i1,i2,length
	character line*64
	include 'key.h'
c
	integer dexpand
c
	if(nkeys.eq.0)then
	  buflen = 0
	else
	  buflen = k2(nkeys)
	endif
c
	i1 = buflen + 1
	i2 = len(pbuf)
	length = dexpand(value,pbuf(i1:i2))
	if(length.le.0)then
	  line = 'KeyExp: Error doing wildcard expansion of '//value
	  call bug('w',line)
	else
	  if(nkeys.eq.maxkeys) call bug('f','KeyExp: Too many keys')
	  nkeys = nkeys + 1
	  keys(nkeys) = key
	  expanded(nkeys) = .true.
	  k1(nkeys) = buflen + 1
	  k2(nkeys) = buflen + length
	  lu(nkeys) = 0
	endif
	end
c************************************************************************
	subroutine keyread(lun,key)
c
	implicit none
	integer lun
	character key*(*)
c
c  Read in another line, from an @ file.
c
c------------------------------------------------------------------------
	integer buflen,i1,i2,length
	include 'key.h'
c
	if(nkeys.eq.0)then
	  buflen = 0
	else
	  buflen = k2(nkeys)
	endif
c
	i1 = buflen + 1
	i2 = len(pbuf)
	call keylget(lun,pbuf(i1:i2),length)
	if(length.eq.0)then
	  call txtclose(lun)
	else
	  if(i1+length-1.gt.i2) call bug('f','KeyRead: Line too long')
	  if(nkeys.eq.maxkeys) call bug('f','KeyRead: Too many keys')
	  nkeys = nkeys + 1
	  keys(nkeys) = key
	  expanded(nkeys) = .true.
	  k1(nkeys) = buflen + 1
	  k2(nkeys) = buflen + length
	  lu(nkeys) = lun
	endif
c
	end
c************************************************************************
	subroutine keylget(lun,line,length)
c
	integer lun,length
	character line*(*)
c
c  Get a line, handling comments and trimming off extra commas, etc.
c------------------------------------------------------------------------
	integer iostat,i,l
	logical within,more
	character c*1
c
	length = 0
	iostat = 0
	dowhile(length.eq.0.and.iostat.eq.0)
	  call txtread(lun,line,length,iostat)
	  if(iostat.eq.0)then
	    i = 0
	    l = 0
	    more = .true.
	    within = .false.
	    dowhile(i.lt.length.and.more)
	      i = i + 1
	      if(within)then
		within = line(i:i).ne.c
		l = i
	      else if(line(i:i).eq.'"'.or.line(i:i).eq.'''')then
		within = .true.
		c = line(i:i)
		l = i
	      else if(line(i:i).eq.'#')then
		more = .false.
	      else if(line(i:i).gt.' '.and.line(i:i).ne.',')then
		l = i
	      endif
	    enddo
	    length = l
	    if(within)call bug('f','Unbalanced quotes on line')
	  else if(iostat.eq.-1)then
	    length = 0
	  else
	    call bug('w','Error reading @ or .def file')
	    call bugno('f',iostat)
	  endif
	enddo
c
	end
c************************************************************************
c* KeyPrsnt -- Determine if a keyword is present on the command line.
c& pjt
c: user-input,command-line
c+
	logical function keyprsnt(key)
c
	implicit none
	character key*(*)
c
c  Determine if a parameter is still present.
c
c  Input:
c    key	The keyword to check for.
c
c  Output:
c    keyprsnt	Indicates whether the keyword is present.
c
c--
c------------------------------------------------------------------------
	integer lkey,i,idx
	include 'key.h'
c
c  See if we can find the keyword.
c
	lkey = min(len(keys(1)),len(key))
	idx = 0
	i = nkeys
	dowhile(idx.eq.0.and.i.gt.0)
	  if(keys(i).eq.key(1:lkey)) idx = i
	  i = i - 1
	enddo
	keyprsnt = idx.gt.0
	end
c************************************************************************
c* KeyFin -- Finish access to the 'key' routines.
c& pjt
c: user-input, command-line
c+
	subroutine keyfin
c
	implicit none
c
c  A call to KeyFin indicates that all parameters (that the program wants)
c  have been retrieved from the command line. KeyFin makes sure all
c  command line parameters have been read.
c--
c------------------------------------------------------------------------
	integer i,lkey
	include 'key.h'
c
c  Externals.
c
	integer len1
        character*80 umsg
c
	do i=1,nkeys
	  lkey = len1(keys(i))
          umsg = 'KeyFin: Parameter '//keys(i)(1:lkey)//
     *	         ' not used or not exhausted'
	  call bug('w', umsg )
	enddo
	end
c************************************************************************
c* Keyf -- Retrieve a filename string (with wildcards) from the command line.
c& pjt
c: user-input,command-line
c+
	subroutine keyf(key,value,default)
c
	implicit none
	character key*(*)
	character value*(*),default*(*)
c
c  Retrieve a character string from the command line. If the keyword is
c  not found, the default is returned.
c
c  Input:
c    key	The name of the keyword to return.
c    default	The default value to return, if the keyword is not present
c		on the command line.
c  Output:
c    value	The returned value.
c--
c------------------------------------------------------------------------
	integer lvalue
c
c  Get the value.
c
	call keyget(key,'*',value,lvalue)
	if(lvalue.eq.0) value = default
	end
c************************************************************************
c* Keya -- Retrieve a character string from the command line.
c& pjt
c: user-input,command-line
c+
	subroutine keya(key,value,default)
c
	implicit none
	character key*(*)
	character value*(*),default*(*)
c
c  Retrieve a character string from the command line. If the keyword is
c  not found, the default is returned.
c
c  Input:
c    key	The name of the keyword to return.
c    default	The default value to return, if the keyword is not present
c		on the command line.
c  Output:
c    value	The returned value.
c--
c------------------------------------------------------------------------
	integer lvalue
c
c  Get the value.
c
	call keyget(key,' ',value,lvalue)
	if(lvalue.eq.0) value = default
	end
c************************************************************************
c* Keyd -- Retrieve a double precision from the command line.
c& pjt
c: user-input,command-line
c+
	subroutine keyd(key,value,default)
c
	implicit none
	character key*(*)
	double precision value,default
c
c  Retrieve a double precision value from the command line. If the keyword is
c  not found, the default is returned.
c
c  Input:
c    key	The name of the keyword to return.
c    default	The default value to return, if the keyword is not present
c		on the command line.
c  Output:
c    value	The returned value.
c--
c------------------------------------------------------------------------
	double precision dval
	character cvalue*48
	integer length
	logical ok
c
c  Get the value.
c
	call keyget(key,' ',cvalue,length)
c
c  Decode it if found.
c
	if(length.gt.0)then
	  call atodf(cvalue(1:length),dval,ok)
	  if(ok)then
	    value = dval
	  else
	    cvalue = 'KeyD: Conversion error decoding parameter '//key
	    call bug('f',cvalue)
	  endif
	else
	  value = default
	endif
	end
c************************************************************************
c* Keyr -- Retrieve a real value from the command line.
c& pjt
c: user-input,command-line
c+
	subroutine keyr(key,value,default)
c
	implicit none
	character key*(*)
	real value,default
c
c  Retrieve a real value from the command line. If the keyword is
c  not found, the default is returned.
c
c  Input:
c    key	The name of the keyword to return.
c    default	The default value to return, if the keyword is not present
c		on the command line.
c  Output:
c    value	The returned value.
c--
c------------------------------------------------------------------------
	double precision temp
	call keyd(key,temp,dble(default))
	value = real(temp)
	end
c************************************************************************
c* Keyi -- Retrieve an integer from the command line.
c& pjt
c: user-input,command-line
c+
	subroutine keyi(key,value,default)
c
	implicit none
	character key*(*)
	integer value,default
c
c  Retrieve an integer value from the command line. If the keyword is
c  not found, the default is returned.
c  The integer can be input as a hex, octal or decimal number using a
c  prefix 0x, %x or h for hex, o or %o for octal, and +, - or nothing
c  for decimal. Either case is ok.
c
c  Input:
c    key	The name of the keyword to return.
c    default	The default value to return, if the keyword is not present
c		on the command line.
c  Output:
c    value	The returned value.
c--
c------------------------------------------------------------------------
	integer ival
	double precision dval
	character cvalue*48
	integer length
	logical ok
c
c  Get the value.
c
	call keyget(key,' ',cvalue,length)
c
c  Decode it if found.
c
	if(length.gt.0)then
	  call atoif(cvalue(1:length),ival,ok)
	  if(ok)then
	    value = ival
	  else
	    call atodf(cvalue(1:length),dval,ok)
	    if(ok)then
	      value = nint(dval)
	    else
	      cvalue = 'KeyI: Conversion error decoding parameter '//key
	      call bug('f',cvalue)
	    endif
	  endif
	else
	  value = default
	endif
	end
c***********************************************************************
c* keyl -- Retrieve a logical value from the command line
c& pjt
c: user-input,command-line
c+
	SUBROUTINE keyl(key,val,def)
c
	IMPLICIT NONE
	CHARACTER key*(*)
	LOGICAL   val, def
c
c Retrieve a logical value from the command line. If the keyword is
c not found, the default is returned.
c It detects, case insensitive, words starting with 'y', 't' and '1'
c with .TRUE. and words starting with 'n', 'f' and '0' with .FALSE.
c .TRUE. and .FALSE. itself are also detected, all with minimum match.
c
c    Input:
c        key      The name of the keyword to return.
c	 def      The default value to return, if the keyword is not 
c                 present on the commandline.
c      Output:
c        val      The returned value.
c
c--
	CHARACTER junk*7, tmp*1
	
	IF (def) THEN
	    tmp = 'y'
        ELSE
            tmp = 'n'
        ENDIF

	CALL keya(key,junk,tmp)
	CALL ucase(junk)
        IF     (junk(1:1).EQ.'T' .OR. 
     -          junk(1:1).EQ.'Y' .OR. 
     -          junk(1:1).EQ.'1' .OR.
     -		junk(1:2).EQ.'.T' ) THEN
            val = .TRUE.
        ELSEIF (junk(1:1).EQ.'F' .OR. 
     -          junk(1:1).EQ.'N' .OR. 
     -          junk(1:1).EQ.'0' .OR.
     - 		junk(1:2).EQ.'.F') THEN
            val = .FALSE.
        ELSE
            CALL bug('w','KeyL: invalid value for a logical')
        ENDIF
        END
c************************************************************************
c* mkeyf -- Retrieve multiple filenames.
c& pjt
c: user-input,command-line
c+
	subroutine mkeyf(key,value,nmax,n)
c
	implicit none
	integer nmax,n
	character key*(*),value(nmax)*(*)
c
c  Return a number of filenames. Wildcard expansion of the names is
c  performed.
c
c  Input:
c    key	The name of the keyword.
c    nmax	The maximum number of names to return.
c  Output:
c    value	The actual filenames.
c    n		The number of filenames returned.
c------------------------------------------------------------------------
	logical keyprsnt
c
	n = 0
	call keyf(key,value(1),' ')
	dowhile(value(n+1).ne.' '.and.n.lt.nmax)
	  n = n + 1
	  if(n.lt.nmax)call keyf(key,value(n+1),' ')
	enddo
	if(n.eq.nmax.and.keyprsnt(key))
     *	  call bug('f','MKeyF: Buffer overflow')
	end
c************************************************************************
c* MKeyr -- Retrieve multiple real values from the command line.
c& pjt
c: user-input,command-line
c+
	subroutine mkeyr(key,value,nmax,n)
c
	implicit none
        integer nmax, n
	character key*(*)
	real value(nmax)
c
c  Retrieve multiple real values from the command line. If the keyword is
c  not found, then zero values are returned. 
c
c  Input:
c    key	The name of the keyword to return.
c    nmax       The maximum number of values to return
c  Output:
c    n          The number of values returned.
c    value	The returned values
c--
c------------------------------------------------------------------------
	logical more
c
	logical keyprsnt
c
	n = 0
	more = keyprsnt(key)
	dowhile(more.and.n.lt.nmax)
	  n = n + 1
	  call keyr(key,value(n),0.)
	  more = keyprsnt(key)
	enddo
	if(more) call bug('f','MKeyR: Buffer overflow')
        end
c************************************************************************
c* MKeyd -- Retrieve multiple double values from the command line.
c& pjt
c: user-input,command-line
c+
	subroutine mkeyd(key,value,nmax,n)
c
	implicit none
        integer nmax, n
	character key*(*)
	double precision value(nmax)
c
c  Retrieve multiple real values from the command line. If the keyword is
c  not found, then zero values are returned. 
c
c  Input:
c    key	The name of the keyword to return.
c    nmax       The maximum number of values to return
c  Output:
c    n          The number of values returned.
c    value	The returned values
c--
c------------------------------------------------------------------------
	logical more
c
	logical keyprsnt
c
	n = 0
	more = keyprsnt(key)
	dowhile(more.and.n.lt.nmax)
	  n = n + 1
	  call keyd(key,value(n),0.d0)
	  more = keyprsnt(key)
	enddo
	if(more) call bug('f','MKeyD: Buffer overflow')
        end
c************************************************************************
c* MKeyi -- Retrieve multiple integer values from the command line.
c& pjt
c: user-input,command-line
c+
	subroutine mkeyi(key,value,nmax,n)
c
	implicit none
        integer nmax, n
	character key*(*)
	integer value(nmax)
c
c  Retrieve multiple integer values from the command line. If the keyword is
c  not found, then zero values are returned. 
c
c  Input:
c    key	The name of the keyword to return.
c    nmax       The maximum number of values to return
c  Output:
c    n          The number of values returned.
c    value	The returned values
c--
c------------------------------------------------------------------------
	logical more
c
	logical keyprsnt
c
	n = 0
	more = keyprsnt(key)
	dowhile(more.and.n.lt.nmax)
	  n = n + 1
	  call keyi(key,value(n),0)
	  more = keyprsnt(key)
	enddo
	if(more) call bug('f','MKeyI: Buffer overflow')
        end
c************************************************************************
c* MKeya -- Retrieve multiple character values from the command line.
c& pjt
c: user-input,command-line
c+
	subroutine mkeya(key,value,nmax,n)
c
	implicit none
        integer nmax, n
	character key*(*)
	character value(nmax)*(*)
c
c  Retrieve multiple character values from the command line. If the keyword is
c  not found, then empty strings are returned. 
c
c  Input:
c    key	The name of the keyword to return.
c    nmax       The maximum number of values to return
c  Output:
c    n          The number of values returned.
c    value	The returned values
c--
c------------------------------------------------------------------------
	logical more, keyprsnt
c
	n = 0
	more = keyprsnt(key)
	dowhile(more.and.n.lt.nmax)
	  n = n + 1
	  call keya(key,value(n),' ')
	  more = keyprsnt(key)
	enddo
	if(more) call bug('f','MKeyA: Buffer overflow')
        end
