c************************************************************************
c  History:
c    rjs Dark_ages - Created.
c    rjs  7jun89   - Added handling of logical and relational operators.
c    rjs  8aug89   - Improved comments.
c    pjt  4dec89   - bug calls report routine name
c    pjt  4mar91   - changed atod to atodf (new strings.for)
c    pjt 30oct91   - added doc to ariInq
C************************************************************************
c  ARI consists of a collection of routines to perform parsing and
c  evaluation of expressions. The input is a character string, in a
c  FORTRAN-like syntax, which is broken down
c  ("compiled") by ariComp. The result is a buffer of tokens in
c  reverse polish order. This is then fed to ariExec, which evaluates
c  the expression. These routines call two user-supplied "action"
c  routines, PACTION and VACTION. These names are not hard coded, the
c  user passes the subroutine names as arguments of ariComp and ariExec.
c  These routines return information about or values of "symbols" in the
c  input expression. These "symbols" can represent constants, scalars
c  or vectors. "Constants" and "scalar" differ in that the values of constants
c  are returned by PACTION to ariComp, whereas the value of scalars are
c  determined by ariExec via VACTION. Which one you use depends on convenience.
c  Templates for PACTION and VACTION follow. Also see the preamble comments
c  in ariComp and ariExec.
c
c  Error Handling and Bugs:
c
c    All syntax errors in the input expression are detected by ariComp.
c    In this case, it returns with type=0
c
c    ariExec does not detect problems like 1/0, sqrt(negative number), etc.
c    If you try to do these, ariExec will crash.
c
c    a**b**c is evaluated as (a**b)**c, rather than the standard a**(b**c)
c
c    max and min functions take two arguments only (the standard says
c    they can take any number of arguments greater than 1).
c
c    ariComp and ariExec use internal arrays to hold intermediate orderings
c    of tokens, etc. The needed size of these arrays is related to the
c    depth of bracketing, the number of symbols, etc.  Array overflows are
c    checked for, and the routine BUG called if this happens. If this occurs,
c    increase the parameter DIM. In practise this should not happen.
c
c    ariComp and ariExec use two arrays, BUF and RBUF, for the majority
c    of buffering. The length of BUF is roughly the number of tokens in the
c    input expression. 64 elements is probably enough. RBUF should be several
c    times the length of the vector size, though again this varies with the
c    depth of bracketing, etc. If either BUF or RBUF overflow, BUG is called.
c
c    BUG is also called if certain internal consistency checks fail.
c--
c************************************************************************
c	subroutine pAction(symbol,type,index,value)
c
c	implicit none
c	character symbol*(*)
c	integer type,index
c	real value
c
c  The input expression can consist "symbols" (variables).
c  When a symbol is found, ARICOMP calls the PACTION routine to find
c  out a few things about it. In particular, the symbol can represent
c  a) A vector. The value of vectors are read by the VACTION routine
c     called by ARIEXEC.
c  b) A scalar. Again the value is read by VACTION, called by ARIEXEC.
c  c) A (named) constant. Here the value is passed by directly by PACTION.
c
c  PACTION is called once only for each symbol.
c
c  Inputs:
c    Symbol	The symbol name.
c  Outputs:
c    type	Contains the number Error, Constant, Scalar or Vector.
c    index	Used if TYPE.EQ.SCALAR .OR. TYPE.EQ.VECTOR: This is
c		some integer (not too big) which VACTION will use to
c		identify the symbol. Typically it could be an index into
c		a table used by paction/vaction, a logical unit number, etc.
c    value	Used if TYPE.EQ.CONSTANT: The value of the constant.
c
c------------------------------------------------------------------------
c	integer error,constant,scalar,vector
c	parameter(error=0,constant=1,scalar=2,vector=3)
c
c	end
c************************************************************************
c	subroutine Vaction(Index,Tok,Data,N)
c
c	implicit none
c	integer Index,Type,N
c	real Data(*)
c
c  This routine is called by ariExec each time it wants the value of
c  a scalar or vector. It may be called multiple times for the one
c  scalar/vector.
c
c  VACTION may be called multiple times for a given symbol.
c
c  Inputs:
c    Index	Integer index given to ariComp by PACTION, when the symbol
c		was first found.
c    Type	Type passed back to PACTION when symbol was found.
c    N		The length of the vector, passed to ariExec.
c  Output:
c    Data	Where to copy the value of the scalar or vector.
c
c------------------------------------------------------------------------
c
c	end
c************************************************************************
c* AriExec -- Evalute an expression, previously parsed by AriComp.
c& pjt
c: mathematics
c+
	subroutine ariExec(vaction,N,Buf,BufLen,RBuf,RBufLen,Index)
c
	implicit none
	integer N,BufLen,RBufLen,Index
	integer Buf(BufLen)
	real RBuf(RBufLen)
	external vaction
c
c  AriExec evaluates a FORTRAN-like expression, which has been parsed
c  by AriComp.
c
c  AriExec takes the reverse polish list of tokens in Buf, and proceeds
c  to evaluate the expression. VACTION is called to get the values of
c  any scalars or vectors. The result of the expression is returned to
c  the caller starting at RBUF(INDEX).
c
c  Inputs:
c    VACTION	Action routine called to get the vaslue of scalars or vectors.
c    N		Vector length. Any value can be used here if there is
c		no vector processing.
c    BUF	List of tokens, determined by ariComp.
c    BUFLEN	Total length of BUF. Its not really used.
c    RBUFLEN	Total length of RBUF.
c
c  Input/Output/Scratch:
c    RBUF	RBUF contains some constants at the head of the
c		array, used by AriExec. These are unchanged on exit.
c		The result of the evaluation is returned to the caller
c		starting at RBUF(INDEX). Otherwise RBUF is used to hold
c		intermediate results.
c
c  Output:
c    INDEX	Index into RBUF of the result of the expression.
c
c--
c------------------------------------------------------------------------
	integer dim,ModVal,constant,scalar,vector
	parameter(dim=64,ModVal=64,constant=1,scalar=2,vector=3)
c
	integer Stack(dim),StackPt,i,k,NInst,Tok,RBufPt,Op1,Op2,Operands
	integer Nd
c
	RBufPt = Buf(3)
	NInst = Buf(4)
	StackPt = 0
	do i=5,NInst
c
c  Get the token, and determine the indices in RBUF for the start of the
c  data for both 1-operand and 2-operand operators.
c
	  Tok = mod(Buf(i),ModVal)
	  if(StackPt.ge.1)then
	    if(Stack(StackPt).eq.0)then
	      Op1 = RBufPt
	      Nd = 1
	    else
	      Op1 = RBufPt - N + 1
	      Nd = N
	    endif
	  endif
	  if(StackPt.ge.2)then
	    k = Stack(StackPt) + Stack(StackPt-1)
	    if(k.eq.0)then
	      Op2 = RBufPt - 1
	    else if(k.eq.1)then
	      Op2 = RBufPt - N
	    else
	      Op2 = RBufPt - N - N + 1
	    endif
	  endif
c
c  Case to the right code.
c
	  goto( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,110,120,130,140,
     *	   150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,
     *	   300,310,320,330,340,350,360,370,380,390,400),Tok
c
	  call bug('f','ariExec: Should never get here')
c
c  Push a constant or a scalar.
c
  10	  continue
  20	  if(StackPt.ge.dim)call bug('f','ariExec: STACK overflow')
	  StackPt = StackPt + 1
	  Stack(StackPt) = 0
	  if(RBufPt.ge.RBufLen)call bug('f','ariExec: RBUF overflow')
	  Index = Buf(i)/ModVal
	  RBufPt = RBufPt + 1
	  if(Tok.eq.Constant)then
	    RBuf(RBufPt) = RBuf(Index)
	  else
	    call vaction(Index,Tok,RBuf(RBufPt),N)
	  endif
	  Operands = 0
	  goto 1000
c
c  Push a vector.
c
  30	  if(StackPt.ge.dim)call bug('f','ariExec: STACK overflow')
	  StackPt = StackPt + 1
	  Stack(StackPt) = 1
	  if(RBufPt+N.gt.RBufLen)call bug('f','ariExec: RBUF overflow')
	  Index = Buf(i)/ModVal
	  call vaction(Index,Tok,RBuf(RBufPt+1),N)
	  RBufPt = RBufPt + N
	  Operands = 0
	  goto 1000
c
c  Multiplication.
c
  40	  call ariMult(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Exponentiation.
c
  50	  call ariExpo(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Addition.
c
  60	  call ariAdd(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Subtraction.
c
  70	  call ariSub(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Unary minus.
c
  80	  call ariUMin(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  And.
c
  90	  call ariAnd(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Eq.
c
  100	  call ariEq(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Eqv.
c
  110	  call ariEqv(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Ge.
c
  120	  call ariGe(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Gt.
c
  130	  call ariGt(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Le.
c
  140	  call ariLe(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Lt.
c
  150	  call ariLt(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Ne.
c
  160	  call ariNe(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Neqv.
c
  170	  call ariNeqv(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Not.
c
  180	  call ariNot(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Or.
c
  190	  call ariOr(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Division.
c
  200	  call ariDiv(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Absolute value.
c
  210	  call ariAbs(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Arc cosine.
c
  220	  call ariACos(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Truncation (aint).
c
  230	  call ariAInt(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Nearest integer (anint).
c
  240	  call ariANint(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Arc sine.
c
 250	  call ariASin(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Arc tangent.
c
 260	  call ariATan(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Arc tangent, two arguments (atan2).
c
 270	  call ariATan2(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Cosine
c
 280	  call ariCos(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Positive difference (dim). Who uses this I do not know, but its here
c  because I want to say that it does standard FORTRAN expression.
c
 290	  call ariDim(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Exponential (exp).
c
 300	  call ariExp(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Log base e.
c
 310	  call ariLog(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Log base 10.
c
 320	  call ariLog10(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Max value.
c
 330	  call ariMax(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Min value.
c
 340	  call ariMin(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Modulus function.
c
 350	  call ariMod(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Sign function.
c
 360	  call ariSign(Stack(StackPt-1),Stack(StackPt),RBuf(Op2),N)
	  Operands = 2
	  goto 1000
c
c  Sine function.
c
 370	  call ariSin(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Square root.
c
 380	  call ariSqrt(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Step function.
c
 390	  call ariStep(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Tangent.
c
 400	  call ariTan(RBuf(Op1),Nd)
	  Operands = 1
	  goto 1000
c
c  Adjust RBufPt if we had two arguments to this function.
c
 1000	  continue
	  if(Operands.eq.2)then
	    k = Stack(StackPt) + Stack(StackPt-1)
	    if(k.eq.2)then
	      RBufPt = RBufPt - N
	    else
	      RBufPt = RBufPt - 1
	      Stack(StackPt-1) = k
	    endif
	    StackPt = StackPt - 1
	  endif
	enddo
c
c  Return a pointer to the data.
c
	if(StackPt.ne.1)
     -		call bug('f','ariExec: I am confused -- internal bug')
	Index = Buf(3) + 1
	end
c************************************************************************
c* AriInq -- Return information from Ari buffers
c& pjt
c: mathematics
c+
	subroutine ariInq(Buf,RBuf,BSize,RBSize)
c
	implicit none
	integer Buf(*)
	real RBuf(*)
	integer BSize,RBSize
c
c  This returns information about the number of locations that are
c  currently used in Buf and RBuf.
c
c  Inputs:
c	Buf	List of tokens, as returned by AriComp
c	RBuf	Constants, as returned by AriComp
c  Outputs:
c	BSize	Number of tokens held in BUF, including the 4 headers.
c	RBSize  Number of constants held at start of RBUF.
c------------------------------------------------------------------------
	BSize = Buf(4)
	RBSize = Buf(3)
	end
c************************************************************************
c* AriComp -- Parse a Fortran-like expression.
c& pjt
c: mathematics
c+
	subroutine ariComp(exp,paction,type,Buf,Buflen,RBuf,RBufLen)
c
	implicit none
	character exp*(*)
	integer type,BufLen,RBuflen
	integer Buf(Buflen)
	real    RBuf(RBufLen)
	external paction
c
c  AriComp parses a FORTRAN-like expression, breaking it up into
c  a sequence of reverse Polish tokens. The Fortran expression (given
c  as a character string) can contain all normal FORTRAN numeric operators
c  and functions, as well as constants and variables.
c
c  AriComp calls a user written action routine, to determine information
c  about "variables".
c
c  See AriExec to see how to evaluate the expression after it has been
c  parsed.
c
c  Inputs:
c    exp	The expression to parse. This should be a FORTRAN-like
c		real expression.
c    paction	Routine called to determine what each variable represents.
c    buflen	Length of integer buffer.
c    rbuflen	Length of real buffer.
c
c  Outputs:
c    type	Indicates status, and whether a scalar or vector expression
c		was parsed. This can take a value of error, scalar or
c		vector.
c    buf	Sequence of tokens in reverse polish. The first four locations
c		of buf are special, however (used by ariExec, caller
c		will not be interested):
c		  Buf(1)  Reserved.
c		  Buf(2)  Reserved.
c		  Buf(3)  Number of constants held at start of RBUF.
c		  Buf(4)  Number of tokens held in BUF, including these 4.
c    rbuf	Constants.
c
c  This routine returns with a parsing error is found. However it aborts
c  on buffer overflows. It has some internal buffers, which hopefully are
c  big enough, but BUF and RBUF can also be too small.
c
c--
c------------------------------------------------------------------------
	integer vector,constant,scalar,error
	parameter(error=0,constant=1,scalar=2,vector=3)
	integer Func,Operator,Numeric,Symbol,LBracket,Comma,RBracket
	parameter(Func=-7,Operator=-6,Numeric=-5,Symbol=-4)
	parameter(LBracket=-3,Comma=-2,RBracket=-1)
	integer ModVal,NTok,dim

	parameter(ModVal=64,NTok=40,dim=64)
c
	character TokNam(NTok)*8
	integer Prec(NTok),Args(NTok)
	integer Expect(dim),Stack(dim),SymTab(4,dim)
	integer RBufPt,BufPt,StackPt,ExpectPt,SymPt
	integer p1,p2,N1,N2,Index,Tok,i
	logical Ok,LExp,doVec
c
c  Externals
c
	integer len1, ariFind
c
c  This is a table of all the tokens that can be executed. Note
c  that $CONSTAN, $SCALAR and $VECTOR are hard coded to be the first
c  three entries of the table.
c
c  THE TABLE SHOULD BE IN ALPHABETIC (ASCII) ORDER, AND FUNCTION NAMES MUST
c  BE LOWER CASE. 
c
c  PREC gives the relative precedence (order only is importrant). ARGS
c  gives the number of inputs to the operation (all operations have
c  one output).
c  Extra functions should be reasonably easy to add
c  (modify this table, modify the case statement in ariExec, and add a
c  routine to evaluate the new function).
c
	data (TokNam(i),Prec(i),Args(i),i=1,NTok)/
     *   '$CONSTAN',16, 0,    '$SCALAR ',16, 0,    '$VECTOR ',16, 0,
     *	 '*       ', 9, 2,    '**      ',10, 2,    '+       ', 7, 2,
     *	 '-       ', 7, 2,    '--      ', 8, 1,    '.and.   ', 4, 2,
     *   '.eq.    ', 6, 2,    '.eqv.   ', 2, 2,    '.ge.    ', 6, 2,
     *   '.gt.    ', 6, 2,    '.le.    ', 6, 2,    '.lt.    ', 6, 2,
     *   '.ne.    ', 6, 2,    '.neqv.  ', 2, 2,    '.not.   ', 5, 1,
     *   '.or.    ', 3, 2,    '/       ', 9, 2,    'abs     ',11, 1,
     *	 'acos    ',11, 1,    'aint    ',11, 1,    'anint   ',11, 1,
     *	 'asin    ',11, 1,    'atan    ',11, 1,    'atan2   ',11, 2,
     *	 'cos     ',11, 1,    'dim     ',11, 2,    'exp     ',11, 1,
     *	 'log     ',11, 1,    'log10   ',11, 1,    'max     ',11, 2,
     *	 'min     ',11, 2,    'mod     ',11, 2,    'sign    ',11, 2,
     *	 'sin     ',11, 1,    'sqrt    ',11, 1,    'step    ',11, 1,
     *	 'tan     ',11, 1/
c
c  Initialise, then loop.
c
	doVec = .false.
	SymPt = 0
	StackPt = 0
	ExpectPt = 0
	RBufPt = 0
	BufPt = 4
	N1 = 1
	N2 = len1(Exp)
	Ok = .true.
	LExp = .true.
c
c  Push a left bracket onto the stack, then loop.
c
	call ariPutok(LBracket,LBracket,
     *		Stack,StackPt,dim,Buf,BufPt,BufLen)
c
	do while(Ok.and.N1.le.N2)
	  call ariGetok(Exp,N1,N2,p1,p2,Tok)
c
c  Expecting part of an expression.
c
	  if(LExp)then
	    LExp = Tok.ne.Numeric.and.Tok.ne.Symbol
c
c  Function calls.
c
	    if(Tok.eq.Func)then
	      Tok = ariFind(Exp(p1:p2),TokNam,NTok)
	      if(Tok.gt.0)then
		if(ExpectPt+Args(Tok).gt.dim)
     *		   call bug('f','ariComp: EXPECT buffer overflow')
		call ariPutok(Tok,Prec(Tok),
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
		call ariPutok(LBracket,LBracket,
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
		ExpectPt = ExpectPt + 1
		Expect(ExpectPt) = RBracket
		do i=1,Args(Tok)-1
		  ExpectPt = ExpectPt + 1
		  Expect(ExpectPt) = Comma
		enddo
	      else
		Ok = .false.
	      endif
c
c  Numeric value.
c
	    else if(Tok.eq.Numeric)then
	      call ariNum(Exp(p1:p2),Tok,Index,
     *				RBuf,RBufPt,RBufLen)
	      if(Tok.ne.0)then
	        call ariPutok(Tok+ModVal*Index,Prec(Tok),
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
	      else
		Ok = .false.
	      endif
c
c  Symbol.
c
	    else if(Tok.eq.Symbol)then
	      call ariSymbl(Exp,p1,p2,paction,Tok,Index,
     *		SymTab,SymPt,dim,Rbuf,RBufPt,RBufLen)
	        doVec = doVec .or. Tok.eq.vector
	      if(Tok.gt.0)then
		call ariPutok(Tok+ModVal*Index,Prec(Tok),
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
	      else
		Ok = .false.
	      endif
c
c  Left bracket.
c
	    else if(Tok.eq.LBracket)then
	      if(ExpectPt.eq.dim)
     *		call bug('f','ariComp: EXPECT buffer overflow')
	      ExpectPt = ExpectPt + 1
	      Expect(ExpectPt) = RBracket
	      call ariPutok(LBracket,LBracket,
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
c
c  Unary operators. Ignore +, handle - and .not..
c
	    else if(Tok.eq.Operator)then
	      if(Exp(p1:p2).eq.'+')then
		continue
	      else if(Exp(p1:p2).eq.'-')then
	        Tok = ariFind('--',TokNam,NTok)
		call ariPutok(Tok,Prec(Tok),
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
	      else if(Exp(p1:p2).eq.'.not.')then
	        Tok = ariFind('.not.',TokNam,NTok)
		call ariPutok(Tok,Prec(Tok),
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
	      else
		Ok = .false.
	      endif
	    else
	      Ok = .false.
	    endif
c
c  Expecting a binary operator (+,-,*,/, etc), a comma or right bracket.
c
	  else
	    LExp = Tok.ne.RBracket
c
c  An operator.
c
	    if(Tok.eq.Operator)then
	      Tok = ariFind(Exp(p1:p2),TokNam,NTok)
	      if(Tok.gt.0)then
	        call ariPutok(Tok,Prec(Tok),
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
	      else
		OK = .false.
	      endif
c
c  A comma or right bracket.
c
	    else if(Tok.eq.Comma.or.Tok.eq.RBracket)then
	      if(ExpectPt.gt.0.and.Expect(ExpectPt).eq.Tok)then
		ExpectPt = ExpectPt - 1
		call ariPutok(Tok,Tok,
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
	      else
		Ok = .false.
	      endif
	    else
	      Ok = .false.
	    endif
	  endif
	enddo
c
c  Finish converting to reverse polish by pushing a right bracket.
c
	Ok = Ok.and.ExpectPt.eq.0.and..not.LExp
	if(Ok)then
	  call ariPutok(RBracket,RBracket,
     *			Stack,StackPt,dim,Buf,BufPt,BufLen)
	  if(StackPt.ne.0)
     *		call bug('f','ariComp: I am confused -- Internal bug.')
	endif
c
c  Finish up.
c
	type = scalar
	if(doVec)type = vector
	if(.not.Ok)type = 0
	Buf(3) = RBufPt
	Buf(4) = BufPt
	end
c************************************************************************
	subroutine ariGetok(Exp,N1,N2,p1,p2,Tok)
c
	implicit none
	character Exp*(*)
	integer N1,N2,p1,p2,Tok
c
c  Find the next token of characters in a FORTRAN-like expression.
c  Inputs:
c    Exp	The character string containing the expression.
c    N2		The last character of interest in Exp.
c  Input/Output:
c    N1		The first character of interrest in Exp. Updated to
c		point one beyond the last char parsed.
c  Output:
c    p1,p2	Indices to the first and last character of the token.
c    Tok	The token type.
c
c------------------------------------------------------------------------
	integer Func,Operator,Numeric,Symbol,LBracket,Comma,RBracket
	parameter(Func=-7,Operator=-6,Numeric=-5,Symbol=-4)
	parameter(LBracket=-3,Comma=-2,RBracket=-1)
	character c*1
	integer digits,OldN1
	logical More,letter
c
c  Externals.
c
	logical AriChkLg
c
	c = Exp(N1:N1)
	letter = .false.
	if(N1.lt.N2)
     *     letter = (Exp(N1+1:N1+1).ge.'a'.and.Exp(N1+1:N1+1).le.'z')
     *		.or.(Exp(N1+1:N1+1).ge.'A'.and.Exp(N1+1:N1+1).le.'Z')
	p1 = N1
c
c  Check for brackets and commas.
c
	if(c.eq.'(')then
	  Tok = LBracket
	  N1 = N1 + 1
	else if(c.eq.')')then
	  Tok = RBracket
	  N1 = N1 + 1
	else if(c.eq.',')then
	  Tok = Comma
	  N1 = N1 + 1
c
c  Check for a string of +, -, * or /. The caller will decide if the
c  combination is valid. This scheme avoids problems with unary minuses
c  and pluses. Also makes the handling of exponentiation clean.
c
	else if(c.eq.'+'.or.c.eq.'-'.or.c.eq.'/'.or.c.eq.'*')then
	  More = .true.
	  do while(N1.le.N2.and.More)
	    c = Exp(N1:N1)
	    More = c.eq.'+'.or.c.eq.'-'.or.c.eq.'/'.or.c.eq.'*'
	    if(More)N1 = N1 + 1
	  enddo
	  p2 = N1 - 1
	  Tok = Operator
c
c  A relational or logical operator.
c
	else if(c.eq.'.'.and.letter)then
	  More = .true.
	  dowhile(N1.lt.N2.and.More)
	    N1 = N1 + 1
	    c = Exp(N1:N1)
	    More = c.ne.'.'
	  enddo
	  p2 = N1
	  N1 = N1 + 1
	  Tok = Operator
c
c  Check for a symbol or function call. Just skip everything which cannot
c  be the end of a symbol.
c
	else if((c.ge.'a'.and.c.le.'z').or.(c.ge.'A'.and.c.le.'Z'))then
	  More = .true.
	  dowhile(N1.le.N2.and.More)
	    c = Exp(N1:N1)
	    if(c.eq.'.') then
	      More = .not.ariChkLg(Exp,N1,N2)
	    else
	      More = (c.ne.'('.and.c.ne.')'.and.c.ne.','.and.c.ne.'*'
     *			      .and.c.ne.'+'.and.c.ne.'-'.and.c.ne.'/')
	    endif
	    if(More)N1 = N1 + 1
	  enddo
	  p2 = N1 - 1
	  if(c.eq.'(')then
	    Tok = Func
	    N1 = N1 + 1
	  else
	    Tok = Symbol
	  endif
c
c  Symbol enclosed within angular brackets (i.e. < and > ).
c  This acts as an "escape sequence" of sorts, to get any name in.
c
	else if(c.eq.'<')then
	  p1 = N1 + 1
	  More = .true.
	  do while(N1.le.N2.and.More)
	    More = Exp(N1:N1).ne.'>'
	    if(More)N1 = N1 + 1
	  enddo
	  if(N1.gt.N2.or.p1.ge.N1)then
	    Tok = 0
	  else
	    Tok = Symbol
	    p2 = N1 - 1
	    N1 = N1 + 1
	  endif
c
c  Parse a numeric value. Firstly skip over the mantissa.
c
	else
	  Tok = numeric
	  OldN1 = N1
	  call ariSkpNm(Exp,N1,N2)
	  digits = N1 - OldN1
	  if(N1.le.N2)then
	    if(Exp(N1:N1).eq.'.'.and..not.ariChkLg(Exp,N1,N2))then
	      N1 = N1 + 1
	      OldN1 = N1
	      call ariSkpNm(Exp,N1,N2)
	      digits = digits + N1 - OldN1
	    endif
	  endif
c
c  Make sure the mantissa had some digits, then look at the exponent.
c
	  if(digits.eq.0)then
	    Tok = 0
	  else if(N1.le.N2)then
	    c = Exp(N1:N1)
	    if(c.eq.'e'.or.c.eq.'E'.or.c.eq.'d'.or.c.eq.'D')then
	      N1 = N1 + 1
	      if(N1.le.N2)then
		if(Exp(N1:N1).eq.'+'.or.Exp(N1:N1).eq.'-')N1 = N1 + 1
	      endif
	      if(N1.le.N2)then
		OldN1 = N1
		call ariSkpNm(Exp,N1,N2)
		if(N1.eq.OldN1)Tok = 0
	      else
		Tok = 0
	      endif
	    endif
	  endif
	  p2 = N1 - 1
c
	endif
	end
c************************************************************************
	logical function ariChkLg(Exp,N1,N2)
c
	implicit none
	integer N1,N2
	character Exp*(*)
c
c  Check if the next bit of stuff in the expression is a logical or
c  relational operator.
c
c  Inputs:
c    Exp	The expression.
c    N1,N2	Delimit the part of the expression that we are
c		interested in.
c
c  Output:
c   ariChkLg	True if the following bit of the expression could be
c		a logical or relational operator.
c
c------------------------------------------------------------------------
	integer ntok
	parameter(ntok=11)
	logical match
	integer i,length
	integer lens(ntok)
	character toks(ntok)*6
c
	data lens/  5,       4,       5,       4,       4,       4,
     *		    4,       4,       6,       5,       4/
	data toks/'.and. ','.eq.  ','.eqv. ','.ge.  ','.gt.  ','.le.  ',
     *		  '.lt.  ','.ne.  ','.neqv.','.not. ','.or.  '/
c
	match = .false.
	length = N2 - N1 + 1
	if(length.ge.4)then
	  i = 0
	  dowhile(.not.match.and.i.lt.ntok)
	    i = i + 1
	    if(length.ge.lens(i))
     *	      match = Exp(N1:N1+lens(i)-1).eq.toks(i)
	  enddo
	endif
	ariChkLg = match
	end
c************************************************************************
	subroutine ariSkpNm(Exp,N1,N2)
c
	implicit none
	character Exp*(*)
	integer N1,N2
c
c  Skip over a string of digits.
c
c------------------------------------------------------------------------
	logical More
c
	More = .true.
	do while(N1.le.N2.and.More)
	  More = Exp(N1:N1).ge.'0'.and.Exp(N1:N1).le.'9'
	  if(More)N1 = N1 + 1
	enddo
	end
c************************************************************************
	subroutine ariNum(Num,Tok,Index,Rbuf,RBufPt,RBufLen)
c
	implicit none
	character Num*(*)
	integer Tok,Index,RBufPt,RBufLen
	real RBuf(RBufLen)
c
c  Convert a ascii string into a real, and add it to the constant list
c  in RBUF. Return Index and type.
c
c------------------------------------------------------------------------
	integer vector,constant,scalar
	parameter(constant=1,scalar=2,vector=3)
	double precision d
	logical Ok
c
	call atodf(Num,d,Ok)
	if(Ok)then
	  Tok = constant
	  if(RBufPt.eq.RBufLen)call bug('f','ariNum: RBUF overflow')
	  RBufPt = RBufPt + 1
	  Index = RBufPt
	  RBuf(Index) = d
	else
	  Tok = 0
	endif
	end
c************************************************************************
	subroutine ariSymbl(Exp,p1,p2,paction,Tok,Index,
     *		SymTab,SymPt,SymLen,Rbuf,RBufPt,RBufLen)
c
	implicit none
	character Exp*(*)
	integer p1,p2,Tok,Index,SymPt,SymLen,RBufPt,RBufLen
	integer SymTab(4,SymLen)
	real RBuf(RBufLen)
	external paction
c
c  Look for a symbol in the list of symbols that we already have. If found,
c  return its type and index. If not found, call paction to determine its
c  type and index, and add the new symbol to the table.
c
c------------------------------------------------------------------------
	integer vector,constant,scalar
	parameter(constant=1,scalar=2,vector=3)
	logical Found
	integer i,q1,q2
	real Value
c
	i = 0
	Found = .false.
	do while(i.lt.SymPt.and..not.Found)
	  i = i + 1
	  q1 = SymTab(1,i)
	  q2 = SymTab(2,i)
	  Found = Exp(p1:p2).eq.Exp(q1:q2)
	enddo
c
	if(.not.Found)then
	  call paction(Exp(p1:p2),Tok,Index,Value)
	  if(Tok.eq.constant)then
	    if(RBufPt.ge.RBufLen)call bug('f','ariSymbl: RBUF overflow')
	    RBufPt = RBufPt + 1
	    RBuf(RBufPt) = Value
	    Index = RBufPt
	  endif
	  if(SymPt.ge.SymLen)call bug('f','ariSymbl: SYMTAB overflow')
	  SymPt = SymPt + 1
	  SymTab(1,SymPt) = p1
	  SymTab(2,SymPt) = p2
	  SymTab(3,SymPt) = Tok
	  SymTab(4,SymPt) = Index
	else
	  Tok   = SymTab(3,i)
	  Index = SymTab(4,i)
	endif
	end
c************************************************************************
	integer function ariFind(Token,TokNam,NTok)
c
	implicit none
	integer NTok
	character Token*(*),TokNam(NTok)*(*)
c
c  Find this token in a list of tokens. The list of tokens is assumed to
c  be in alphabetic order. A binary search is used. 0 is returned when the
c  token is not found. Otherwise the index of the token is returned.
c
c------------------------------------------------------------------------
	integer k1,k2,k
	character string*8
c
c  The token cannot be too long.
c
	if(len(Token).gt.len(String))then
	  ariFind = 0
c
c  Search for it.
c
	else
	  k1 = 1
	  k2 = NTok
	  String = Token
	  call lcase(String)
	  do while(k1.lt.k2)
	    k = (k1+k2)/2
	    if(String.eq.TokNam(k))then
	      k1 = k
	      k2 = k
	    else if(String.lt.TokNam(k))then
	      k2 = k-1
	    else
	      k1 = k+1
	    endif
	  enddo
c
c  Return it.
c
	  if(TokNam(k1).ne.String)then
	    ariFind = 0
	  else
	    ariFind = k1
	  endif
	endif
	end
c************************************************************************
	subroutine ariPutok(Token,Prec,
     *		Stack,StackPt,StackLen,Buf,BufPt,BufLen)
c
	implicit none
	integer StackLen,BufLen,StackPt,BufPt,Token,Prec
	integer Stack(StackLen),Buf(BufLen)
c
c  Add a token to the output list, converting it to reverse polish on the
c  way.  STACK is used as an intermediate array in converting to reverse
c  Polish.
c
c  If the input token is of equal or lower precedence to the top
c  stack element, pop the stack element and append it to the output
c  Do this until a stack token appears that has a lower precedence
c  than the input token. An exception is left bracket, which is always
c  pushed straight onto the stack.
c
c  After this, a right bracket causes the corresponding left bracket
c  to be popped of the stack, and a comma is simply discarded.
c
c  The odd elements of STACK are the tokens, whereas the following element
c  (even) is the corresponding precedence.
c------------------------------------------------------------------------
	integer LBracket,Comma,RBracket
	parameter(LBracket=-3,Comma=-2,RBracket=-1)
c
c  Check if it is possible that either the stack or output buffer will
c  overflow. This is a conservative check (i.e. there will be cases where
c  it could have worked, which this claims did not).
c
	if(StackPt+2.gt.StackLen.or.StackPt/2+BufPt.gt.BufLen)
     *		call bug('f','ariPutok: Array overflow')
c
c  Pop the stack until a low precedence token appears on the top.
c
	if(Token.ne.LBracket)then
	  do while(StackPt.gt.0.and.Prec.le.Stack(StackPt))
	    StackPt = StackPt - 1
	    BufPt = BufPt + 1
	    Buf(BufPt) = Stack(StackPt)
	    StackPt = StackPt - 1
	  enddo
	endif
c
c  If the token is a right bracket or a comma, then the top of the
c  stack must be a left bracket. On right bracket, discard both right
c  and left brackets. On comma, discard the comma only. Otherwise push
c  the token onto the stack.
c
	if(Token.eq.RBracket)then
	  StackPt = StackPt - 2
	  if(StackPt.lt.0)call bug('f','ariPutok: Internal bug')
	else if(Token.ne.Comma)then
	  StackPt = StackPt + 1
	  Stack(StackPt) = Token
	  StackPt = StackPt + 1
	  Stack(StackPt) = Prec
	endif
c
	end
c************************************************************************
	subroutine ariMult(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c  Multiply either a vector with a vector, a vector and scalar, scalar and
c  vector of scalar with scalar.
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = data(1) * data(2)
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  data(i) = data(i) * temp
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = data(i+1) * temp
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = data(i) * data(i+n)
	enddo
	return
c
	end
c************************************************************************
	subroutine ariExpo(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c  Exponentiation. Make a special case where the index is an integer.
c
c------------------------------------------------------------------------
	integer i,k
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	k = int(data(2))
	if(real(k).eq.data(2))then
	  data(1) = data(1) ** k
	else
	  data(1) = data(1) ** data(2)
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	k = int(temp)
	if(real(k).eq.temp)then
	  do i=1,n
	    data(i) = data(i) ** k
	  enddo
	else
	  do i=1,n
	    data(i) = data(i) ** temp
	  enddo
	endif
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = temp ** data(i+1)
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = data(i) ** data(i+n)
	enddo
	return
c
	end
c************************************************************************
	subroutine ariAdd(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c  Addition.
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = data(1) + data(2)
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  data(i) = data(i) + temp
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = temp + data(i+1)
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = data(i) + data(i+n)
	enddo
	return
c
	end
c************************************************************************
	subroutine ariSub(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c Subtract.
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = data(1) - data(2)
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  data(i) = data(i) - temp
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = temp - data(i+1)
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = data(i) - data(i+n)
	enddo
	return
c
	end
c************************************************************************
	subroutine ariDiv(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c  Divide.
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = data(1) / data(2)
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = 1./data(n+1)
	do i=1,n
	  data(i) = temp * data(i)
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = temp / data(i+1)
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = data(i) / data(i+n)
	enddo
	return
c
	end
c************************************************************************
	subroutine ariAtan2(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = atan2( data(1), data(2) )
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  data(i) = atan2( data(i), temp )
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = atan2( temp, data(i+1) )
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = atan2( data(i), data(i+n) )
	enddo
	return
c
	end
c************************************************************************
	subroutine ariDim(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = dim( data(1), data(2) )
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  data(i) = dim( data(i), temp )
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = dim( temp, data(i+1) )
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = dim( data(i), data(i+n) )
	enddo
	return
c
	end
c************************************************************************
	subroutine ariMax(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = max( data(1), data(2) )
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  data(i) = max( data(i), temp )
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = max( temp, data(i+1) )
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = max( data(i), data(i+n) )
	enddo
	return
c
	end
c************************************************************************
	subroutine ariMin(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = min( data(1), data(2) )
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  data(i) = min( data(i), temp )
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = min( temp, data(i+1) )
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = min( data(i), data(i+n) )
	enddo
	return
c
	end
c************************************************************************
	subroutine ariMod(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = Mod( data(1), data(2) )
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  data(i) = Mod( data(i), temp )
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = Mod( temp, data(i+1) )
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = Mod( data(i), data(i+n) )
	enddo
	return
c
	end
c************************************************************************
	subroutine ariSign(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	data(1) = Sign( data(1), data(2) )
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  data(i) = Sign( data(i), temp )
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  data(i) = Sign( temp, data(i+1) )
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  data(i) = Sign( data(i), data(i+n) )
	enddo
	return
c
	end
c************************************************************************
	subroutine ariUMin(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c  Unary minus.
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = - Data(i)
	enddo
	end
c************************************************************************
	subroutine ariAbs(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = abs( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariAcos(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Acos( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariAint(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Aint( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariAnint(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Anint( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariAsin(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Asin( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariAtan(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Atan( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariCos(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Cos( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariExp(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Exp( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariLog(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Log( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariLog10(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Log10( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariSin(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Sin( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariSqrt(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Sqrt( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariTan(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  Data(i) = Tan( Data(i) )
	enddo
	end
c************************************************************************
	subroutine ariStep(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c  The infamous step function.
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  if(Data(i).gt.0) then
	    Data(i) = 1
	  else
	    Data(i) = 0
	  endif
	enddo
	end
c************************************************************************
	subroutine ariNot(Data,N)
c
	implicit none
	integer N
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	do i=1,n
	  if(Data(i).gt.0) then
	    Data(i) = 0
	  else
	    Data(i) = 1
	  endif
	enddo
	end
c************************************************************************
	subroutine ariGt(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).gt.data(2))then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  if(data(i).gt.temp)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  if(temp.gt.data(i+1))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).gt.data(i+n))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
c************************************************************************
	subroutine ariGe(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).ge.data(2))then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  if(data(i).ge.temp)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  if(temp.ge.data(i+1))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).ge.data(i+n))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
c************************************************************************
	subroutine ariLt(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).lt.data(2))then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  if(data(i).lt.temp)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  if(temp.lt.data(i+1))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).lt.data(i+n))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
c************************************************************************
	subroutine ariLe(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).le.data(2))then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  if(data(i).le.temp)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  if(temp.le.data(i+1))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).le.data(i+n))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
c************************************************************************
	subroutine ariEq(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).eq.data(2))then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  if(data(i).eq.temp)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  if(temp.eq.data(i+1))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).eq.data(i+n))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
c************************************************************************
	subroutine ariNe(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).ne.data(2))then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  if(data(i).ne.temp)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  if(temp.ne.data(i+1))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).ne.data(i+n))then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
c************************************************************************
	subroutine ariAnd(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).gt.0.and.data(2).gt.0)then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	if(temp.gt.0)then
	  do i=1,n
	    if(data(i).gt.0) then
	      data(i) = 1
	    else
	      data(i) = 0
	    endif
	  enddo
	else
	  do i=1,n
	    data(i) = 0
	  enddo
	endif
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	if(temp.gt.0)then
	  do i=1,n
	    if(data(i+1).gt.0) then
	      data(i) = 1
	    else
	      data(i) = 0
	    endif
	  enddo
	else
	  do i=1,n
	    data(i) = 0
	  enddo
	endif
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).gt.0.and.data(i+n).gt.0)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
c************************************************************************
	subroutine ariOr(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).gt.0.or.data(2).gt.0)then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	if(temp.gt.0)then
	  do i=1,n
	    data(i) = 1
	  enddo
	else
	  do i=1,n
	    if(data(i).gt.0)then
	      data(i) = 1
	    else
	      data(i) = 0
	    endif
	  enddo
	endif
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	if(temp.gt.0)then
	  do i=1,n
	    data(i) = 1
	  enddo
	else
	  do i=1,n
	    if(data(i+1).gt.0)then
	      data(i) = 1
	    else
	      data(i) = 0
	    endif
	  enddo
	endif
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).gt.0.or.data(i+n).gt.0)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
c************************************************************************
	subroutine ariEqv(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).gt.0.eqv.data(2).gt.0)then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  if(data(i).gt.0.eqv.temp.gt.0)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  if(temp.gt.0.eqv.data(i+1).gt.0)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).gt.0.eqv.data(i+n).gt.0)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
c************************************************************************
	subroutine ariNeqv(type1,type2,Data,n)
c
	implicit none
	integer type1,type2,n
	real Data(*)
c
c------------------------------------------------------------------------
	integer i
	real temp
c
	goto(10,20,30,40),type1+2*type2+1
c
c Both args are scalars.
c
  10	if(data(1).gt.0.neqv.data(2).gt.0)then
	  data(1) = 1
	else
	  data(1) = 0
	endif
	return
c
c  First arg is a vector, second a scalar.
c
  20	temp = data(n+1)
	do i=1,n
	  if(data(i).gt.0.neqv.temp.gt.0)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  First arg is a scalar, second is a vector.
c
  30	temp = data(1)
	do i=1,n
	  if(temp.gt.0.neqv.data(i+1).gt.0)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
c  Both args are vectors.
c
  40	continue
c# ivdep
	do i=1,n
	  if(data(i).gt.0.neqv.data(i+n).gt.0)then
	    data(i) = 1
	  else
	    data(i) = 0
	  endif
	enddo
	return
c
	end
