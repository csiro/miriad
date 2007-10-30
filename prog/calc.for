c************************************************************************
	program calc
	implicit none
c
c= calc - A calculator program.
c& rjs
c: tools
c+
c	CALC is a calculator program. An expression is given in FORTRAN
c	like syntax, and the result printed out.
c
c	Usage:
c	  calc [-ir] [-f xxx] expression
c
c	  -i             Print the result as an integer.
c	  -f xxx         Print the result using FORTRAN format xxx.
c	  -r             Treat the input as a single integer, and
c	                 print it out in various radix formats.
c	                 The input integer can be in a radix format.
c			 A %x introduces a hex number, %o an octal and
c	                 %b a binary.
c	  -c             Treat the input as a single constant, and print out
c	                 information on it.
c	  expression     A FORTRAN-like expression (except if the -r
c	                 flag is given).
c	                 NOTE: An asterisk and brackets are special
c	                 characters to some UNIX shells. These characters
c	                 may need to be escaped or quoted when giving the
c	                 expression.
c--
c  History:
c    rjs  ??????? Original version.
c    rjs  13sep95 Added -f flag.
c    rjs  12aug97 Support constants.
c------------------------------------------------------------------------
	include 'calc.h'
c
	integer dim,scalar
	parameter(dim=128,scalar=2)
	integer buf(dim),index,type,rin
	real rbuf(dim)
	character in*80,out*80,line*128,dec*40,oct*40,hex*40
	character format*32,form*32,strval*32,def*32,unit*8
	double precision value
	integer lin,lout,lline,ldec,loct,lhex,narg,lform,i
	logical doradix,doint,doconst
c
c  Externals.
c
	integer len1,iargc
	character itoaf*12
	external paction,vaction
c
c  Get the command line and extract the first token.
c
	narg = iargc()
	i = 0
	doradix = .false.
	doint = .false.
	doconst = .false.
	format = ' '
	out = ' '
	nconst = 0
c
c  Perform a radix conversion operation.
c
	dowhile(i.lt.narg)
	  i = i + 1
	  call getarg(i,in)
	  if(in.eq.'-i')then
	    doint = .true.
	  else if(in.eq.'-r')then
	    doradix = .true.
	  else if(in.eq.'-c')then
	    doconst = .true.
	  else if(in.eq.'-f')then
	    i = i + 1
	    if(i.le.narg)call getarg(i,format)
	  else
	    if(out.ne.' ')call bug('f','Only one expression allowed')
	    out = in
	  endif
	enddo
	lout = len1(out)
	if(lout.eq.0)call bug('f','An expression must be given')
	call lcase(out(1:lout))
c
c  Fill in output format.
c
	if(format.eq.' '.and..not.doint)then
	  if(doconst)then
	    format = '1pg20.13'
	  else
	    format = '1pg14.7'
	  endif
	endif
	lform = len1(format)
	form = ' '
	if(lform.gt.0)form = '('//format(1:lform)//')'
c
c  Perform a radix conversion operation.
c
	if(doradix)then
	  if(out(1:1).eq.'%')then
	    if(lout.le.2)call bug('f','Bad radix syntax')
	    rin = 0
	    if(out(2:2).eq.'x'.or.out(2:2).eq.'X')rin = 16
	    if(out(2:2).eq.'d'.or.out(2:2).eq.'D')rin = 10
	    if(out(2:2).eq.'o'.or.out(2:2).eq.'O')rin = 8
	    if(out(2:2).eq.'b'.or.out(2:2).eq.'B')rin = 2
	    if(rin.eq.0)call bug('f','Unrecognised radix')
	    lin = lout - 2
	    in(1:lin) = out(3:lout)
	  else
	    rin = 10
	    lin = lout
	    in(1:lin) = out(1:lout)
	  endif
	  call radix(in,lin,rin,dec,ldec,10)
	  call radix(in,lin,rin,hex,lhex,16)
	  call radix(in,lin,rin,oct,loct, 8)
	  line = '  Result = '//dec(1:ldec)//'  Hex = '//hex(1:lhex)//
     *		 '  Octal = '//oct(1:loct)
c
	else if(doconst)then
	  call cinit
	  call cget(out(1:lout),value,unit,def)
	  write(strval,form)value
	  if(def.ne.' ')then
	    line = out(1:lout)//': '//strval(1:len1(strval))//
     *				' '//unit(1:len1(unit))//
     *				' ('//def(1:len1(def))//')'
	  else
	    line = out(1:lout)//': '//strval(1:len1(strval))//
     *				' '//unit(1:len1(unit))
	  endif
c
c  Perform a real expression.
c
	
	else
	  type = 0
	  call ariComp(out(1:lout),paction,type,Buf,dim,RBuf,dim)
	  if(type.eq.scalar)then
	    call ariExec(vaction,1,Buf,dim,RBuf,dim,Index)
	    if(doint)then
	      if(form.eq.' ')then
	        line = itoaf(nint(RBuf(Index)))
	      else
	        write(line,form)nint(RBuf(Index))
	      endif
	    else
	      write(line,form)RBuf(Index)
	    endif
	  else
	    call bug('f','Syntax error in the arithmetic expression')
	  endif
	endif
c
c  Output the result then exit.
c
	lline = len1(line)
	call output(line(1:lline))
	end
c************************************************************************
	subroutine Vaction
	call bug('f','I should never get here')
	end
c************************************************************************
	subroutine Paction(symbol,type,indx,value)
c
	character symbol*(*)
	integer type,indx
	real value
c------------------------------------------------------------------------
	include 'calc.h'
	double precision val
	character unit*8,def*16
c
	if(nconst.eq.0)call cinit
	type = 1
	indx = 0
	call cget(symbol,val,unit,def)
	value = val
c
	end
c************************************************************************
	subroutine radix(in,lin,rin,out,lout,rout)
c
	implicit none
	character in*(*),out*(*)
	integer lin,lout,rin,rout
c
c  Convert the character representation of an integer in one radix
c  representation to another radix representation. This works "digit
c  at a time", so that the input number can larger than the host
c  machine can take.
c
c  Input:
c    in		Input character string.
c    lin	No. chars in "in".
c    rin	Radix of input.
c    rout	Radix of output.
c
c  Output:
c    out	Output character string.
c    lout	Output length.
c
c------------------------------------------------------------------------
	integer maxdigit
	parameter(maxdigit=64)
	integer carry(maxdigit),din(maxdigit),dout(maxdigit)
	integer digit,n1,n2,n1new,sum,n,m
	logical more
	character digits*36
	data digits/'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
c
c  Convert input to normal form.
c
	do n=1,lin
	  m = ichar(in(n:n))
	  if(m.ge.ichar('0').and.m.le.ichar('9'))then
	    m = m - ichar('0')
	  else if(m.ge.ichar('a').and.m.le.ichar('z'))then
	    m = m - ichar('a') + 10
	  else if(m.ge.ichar('A').and.m.le.ichar('Z'))then
	    m = m - ichar('A') + 10
	  else
	    call bug('f','Bad digit')
	  endif
	  din(lin-n+1) = m
	enddo
c
c  Initialise.
c
	more = .true.
	do n=2,lin
	  carry(n) = 0
	enddo
	carry(1) = 1
	digit = 0
	sum = 0
	m = 0
	n1 = 1
	n2 = lin
c
c  Do it now.
c
	dowhile(sum.gt.0.or.n1.le.n2)
	  n1new = n2 + 1
	  digit = 0
	  do n=n1,n2
	    digit = rin * digit + carry(n)
	    carry(n) = digit / rout
	    digit = mod(digit,rout)
	    sum = sum + din(n) * digit
	    if(carry(n).gt.0)n1new = min(n1new,n)
	  enddo
	  m = m + 1
	  dout(m) = mod(sum,rout)
	  sum = sum/rout
	  n1 = n1new
	enddo
c
c  Delete any leading zeros.
c
	lout = m
	more = .true.
	dowhile(lout.gt.0.and.more)
	  more = dout(lout).eq.0
	  if(more)lout = lout - 1
	enddo
c
c  Now copy to the output.
c
	do n=1,lout
	  m = dout(lout-n+1) + 1
	  out(n:n) = digits(m:m)
	enddo
	end
c************************************************************************
	subroutine Cadd(name,value,unit,def)
c
	implicit none
	character name*(*),unit*(*),def*(*)
	double precision value
c
c------------------------------------------------------------------------
	include 'calc.h'
c
	nconst = nconst + 1
	if(nconst.gt.MAXCONST)call bug('f','Too many constants')
	names(nconst)  = name
	values(nconst) = value
	units(nconst)  = unit
	defs(nconst)   = def
c
	end
c************************************************************************
	subroutine Cget(name,value,unit,def)
c
	implicit none
	character name*(*),unit*(*),def*(*)
	double precision value
c
c------------------------------------------------------------------------
	include 'calc.h'
	integer i
	character line*64
c
	do i=1,nconst
	  if(name.eq.names(i))then
	    value = values(i)
	    unit  = units(i)
	    def   = defs(i)
	    return
	  endif
	enddo
c
	line = 'Constant '//name//' not found'
	call bug('f',line)
	end
c************************************************************************
	subroutine cinit
c
	implicit none
	include 'mirconst.h'
	double precision AU
	parameter(AU=149.597870D9)
c
	call cadd('pi',DPI,' ',' ')
	call cadd('c', DCMKS,'m/s','Speed of light')
	call cadd('k',DKMKS, 'J/K','Boltzmans constant')
	call cadd('h',DHMKS, 'J s','Plancks constant')
	call cadd('r',8.314d0,'J/(mol K)','Universal gas constant')
	call cadd('au',AU,'m','Astronomical unit')
	call cadd('parsec',180*3600/DPI*AU,'m','Parsec')
	call cadd('ly',DCMKS*86400*365.25,'m','Light-year')
	end
