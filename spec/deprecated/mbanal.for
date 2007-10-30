c************************************************************************
	program mbanal
	implicit none
c
c= mbanal -- Analyse multibeam data
c& rjs
c: uv-analysis
c+
c	MBANAL is a task to manipulate sets of visibility spectra.
c	One or two spectra sets are read from a visibility dataset,
c	and then these spectra sets can be manipulated interactively
c	with a set of simple commands. These commands manipulate
c	"spectra sets" or individual baselines. A spectra set is 
c	the collection of baselines that are in some way similar.
c	For example, the set
c	of baselines of a particular source observed during some
c	particular time interval would be an example of a spectra set.
c	new spectra sets (or overwrite old ones). Alternately a
c	spectra set could be an observation of a reference blank
c	piece of sky. 
c
c	Spectra sets are named, much like variables. Two spectra
c	set names can be defined at the Miriad command line:
c	'ref' and 'src'. When MBANAL starts up, it loads data into
c	these from a given visibility dataset. These spectra sets
c	can then be manipulated with MBANAL's simple commands.
c
c	Commands include:
c	  Command    Args
c	  -------    ----
c	  add        out  in1 in2
c	  subtract   out  in1 in2
c	  multliply  out  in1 in2
c	  divide     out  in1 in2
c	  conjugate  out  [in]
c	  sqrt       out  [in]
c	  plot       in   [amp|phase|real|imag] [PGPLOT device]
c	  exit
c
c	Commands can take either spectra sets of the spectra
c	of an individual baseline. An individual baseline is
c	given in the form:
c	  name[m,n]
c	where 'name' is the name of the spectra set and m and n
c	are indices of the elements involved. For example
c	  ref[1,13]
c	is the spectra of beams 1 correlated with 13 of the
c	'ref' spectra set.
c
c	For example
c	  mbanal vis=multi.uv ref=time(10:00,10:10)
c	will start mbanal, and define the visibility set
c	'ref'. Subsequent interactive commands could be:
c
c	  plot ref amp
c	will plots the amplitude of the ref datasets.
c
c	  plot ref[1,13] phase
c	will plot the phase of baseline 1-13 of ref.
c
c	  add sum ref src
c	will add the ref and src spectra sets, and produce a
c	new spectra set called 'sum'.
c	
c@ vis
c	The name of the input visibility data-set. No default.
c@ ref
c	This selects the data to be used as a spectra set called
c	'ref'. The default is not to select anything. See the help
c	on "select" for more information.
c@ src
c	This selects the data to be used as a spectra set called
c	'src'. The default is not to select anything. See the help
c	on "select" for more information.
c@ line
c	Standard line-type specification. See the help on "line"
c	for more information.
c@ device
c	Plotting device. Default is /xs. The plotting device can
c	be overridden on the MBANAL prompt.
c--
c------------------------------------------------------------------------
	integer MAXSELS
	character version*(*)
	parameter(version='Mbanal: version 1.0 03-Feb-97')
	parameter(MAXSELS=256)
	character vis*64,device*64,ltype*16,string*128
	character p1*32,p2*32,p3*32,p4*32,token*8
	integer nchan,tIn,k1,k2,length,l
	logical dosrc,doref
	real src(MAXSELS),ref(MAXSELS),lstart,lwidth,lstep
c
	integer len1
	logical keyprsnt
	external add,sub,mul,div,realp,imagp,ampp,phasep,conjgp,sqrtp
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input vis must be given')
	dosrc = keyprsnt('src')
	call SelInput('src',src,MAXSELS)
	doref = keyprsnt('ref')
	call SelInput('ref',ref,MAXSELS)
	call keyline(ltype,nchan,lstart,lwidth,lstep)
	call keya('device',device,'/xs')
	call keyfin
c
	call SlotIni
c
c  Open the vis dataset.
c
	call uvopen(tIn,vis,'old')
	if(ltype.ne.' ')call uvset(tIn,'data',ltype,nchan,
     *					lstart,lwidth,lstep)
c
c  Load the "src" and "ref" signals.
c
	if(dosrc)then
	  call output('Loading the src signal')
	  call uvLoad(tIn,'src',src)
	endif
	if(doref)then
	  call output('Loading the ref signal')
	  call uvLoad(tIn,'ref',ref)
	endif
	call uvclose(tIn)
c
c  Now go into the interactive loop.
c
 100	call prompt(string,length,'MBANAL> ')
	call lcase(string)
	k2 = min(length,len(string))
	if(k2.gt.0)k2 = len1(string(1:k2))
	k1 = 1
	token = ' '
	p1 = ' '
	p2 = ' '
	p3 = ' '
	p4 = ' '
	call getfield(string,k1,k2,token,length)
	call getfield(string,k1,k2,p1,length)
	call getfield(string,k1,k2,p2,length)
	call getfield(string,k1,k2,p3,length)
	call getfield(string,k1,k2,p4,length)
	l = len1(token)
	if(token.eq.' ')then
	  continue
	else if(index('conjugate',token(1:l)).eq.1)then
	  if(p2.eq.' ')p2 = p1
	  call doOne(p1,p2,Conjgp)
	else if(index('sqrt',token(1:l)).eq.1)then
	  if(p2.eq.' ')p2 = p1
	  call doOne(p1,p2,Sqrtp)
	else if(index('exit',token(1:l)).eq.1)then
	  call exit
	else if(index('add',token(1:l)).eq.1)then
	  call doTwo(p1,p2,p3,Add)
	else if(index('subtract',token(1:l)).eq.1)then
	  call doTwo(p1,p2,p3,Sub)
	else if(index('multiply',token(1:l)).eq.1)then
	  call doTwo(p1,p2,p3,Mul)
	else if(index('divide',token(1:l)).eq.1)then
	  call doTwo(p1,p2,p3,Div)
	else if(index('plot',token(1:l)).eq.1)then
	  if(p2.eq.' ')p2 = 'amp'
	  l = len1(p2)
	  if(p3.eq.' ')p3 = device
	  if(index('real',p2(1:l)).ne.0)then
	    call plotit(p1,p3,realp)
	  else if(index('imag',p2(1:l)).ne.0)then
	    call plotit(p1,p3,imagp)
	  else if(index('amplitude',p2(1:l)).ne.0)then
	    call plotit(p1,p3,ampp)
	  else if(index('phase',p2(1:l)).ne.0)then
	    call plotit(p1,p3,phasep)
	  endif
	else if(index('calibrate',token).eq.1)then
	  call bug('w','Calibrate not implemented')
	else
	  call bug('w','Unrecognised command')
	endif
	goto 100
	end
c************************************************************************
	subroutine plotit(p,device,riap)
c
	implicit none
	character p*(*),device*(*)
	real riap
	external riap
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer id,idx,istat,i1,i2,j,i
	double precision bl
	character string*64
	logical ok
c
	integer pgbeg
c
	call deco(p,.true.,id,idx,ok)
	if(.not.ok)return
c
	if(idx.ne.0)then
	  istat = pgbeg(0,device,1,1)
	else
	  istat = pgbeg(0,device,3,2)
	  call pgsch(1.4)
	endif
	if(istat.ne.1)then
	  call bug('w','Error opening PGPLOT device')
	  return
	endif
	if(idx.ne.0)then
	  if(slotav(idx).gt.0)then
	    call plot1(idx,p,riap)
	  else
	    call bug('w','No good data in this baseline')
	  endif
	else
	  do i2=1,MAXANT
	    do i1=1,i2
	      bl = 256*i1+i2
	      call SlotGet(id,bl,idx)
	      if(slotav(idx).gt.0)then
	        write(string,'(a,a,i2,a,i2,a)')p,'[',i1,',',i2,']'
		j = 0
	        do i=1,len(string)
		  if(string(i:i).ne.' ')then
		    j = j + 1
		    string(j:j) = string(i:i)
		  endif
		enddo
		string(j+1:) = ' '
		call plot1(idx,string,riap)
	      endif
	    enddo
	  enddo
	endif
	call pgend
c
	end
c************************************************************************
	subroutine plot1(idx,p,riap)
c
	implicit none
	integer idx
	character p*(*)
	real riap
	external riap
c------------------------------------------------------------------------
	include 'mbanal.h'
	real x(MAXCHAN),y(MAXCHAN)
	integer ic,i
	real t,xmin,xmax,ymin,ymax,xlo,xhi,ylo,yhi
c
	integer len1
c
	ic = slotic(idx)
	t = 1/real(slotav(idx))
	do i=1,nchan
	  x(i) = i
	  y(i) = riap(t*corr(i,ic))
	enddo
	xmax = x(1)
	xmin = xmax
	ymax = y(1)
	ymin = ymax
	do i=1,nchan
	  xmax = max(xmax,x(i))
	  xmin = min(xmin,x(i))
	  ymax = max(ymax,y(i))
	  ymin = min(ymin,y(i))
	enddo
	call pgrnge(xmin,xmax,xlo,xhi)
	call pgrnge(ymin,ymax,ylo,yhi)
	if(ylo.eq.yhi)then
	  ylo = -1
	  yhi = +1
	endif
	call pgpage
	call pgvstd
	call pgswin(xlo,xhi,ylo,yhi)
	call pgbox('BCNST',0.,0,'BCNST',0.,0)
	call pglab('Channel',' ',p(1:len1(p)))
	call pgline(nchan,x,y)
	end
c************************************************************************
	subroutine doOne(p0,p1,op)
c
	implicit none
	character p0*(*),p1*(*)
	external op
c------------------------------------------------------------------------
	include 'mbanal.h'
	logical ok
	integer id0,id1,idx0,idx1,i1,i2
	double precision bl
c
	call deco(p0,.false.,id0,idx0,ok)
	if(.not.ok)return
	call deco(p1,.true.,id1,idx1,ok)
	if(.not.ok)return
	if((idx0.eq.0).neqv.(idx1.eq.0))then
	  call bug('w','Cannot mix baseline/entire ops')
	  return
	endif
c
	if(idx0.eq.0)then
	  do i2=1,MAXANT
	  do i1=1,i2
	    bl = 256*i1 + i2
	    call SlotGet(id0,bl,idx0)
	    call SlotGet(id1,bl,idx1)
	    if(slotav(idx1).eq.0)then
	      slotav(id0) = 0
	    else
	      call Op(idx0,idx1)
	    endif
	  enddo
	  enddo
	else
	  if(slotav(idx1).eq.0)then
	    slotav(idx0) = 0
	  else
	    call Op(idx0,idx1)
	  endif
	endif
	end
c************************************************************************
	subroutine Conjgp(idx0,idx1)
c
	implicit none
	integer idx0,idx1
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i,ic0,ic1
	if(slotic(idx0).eq.0)call Slotit(slotic(idx0))
c
	ic0 = slotic(idx0)
	ic1 = slotic(idx1)
c
	slotav(idx0) = slotav(idx1)
	do i=1,nchan
	  corr(i,ic0) = conjg(corr(i,ic1))
	enddo
c
	end
c************************************************************************
	subroutine Sqrtp(idx0,idx1)
c
	implicit none
	integer idx0,idx1
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i,ic0,ic1
	real a
	if(slotic(idx0).eq.0)call Slotit(slotic(idx0))
c
	ic0 = slotic(idx0)
	ic1 = slotic(idx1)
c
	a = slotav(idx1)
	slotav(idx0) = slotav(idx1)
	do i=1,nchan
	  corr(i,ic0) = a*sqrt(corr(i,ic1)/a)
	enddo
c
	end
c************************************************************************
	subroutine doTwo(p0,p1,p2,op)
c
	implicit none
	character p0*(*),p1*(*),p2*(*)
	external op
c------------------------------------------------------------------------
	include 'mbanal.h'
	logical ok1,ok2,ok3
	integer id0,id1,id2,idx0,idx1,idx2,i1,i2
	double precision bl
	call deco(p0,.false.,id0,idx0,ok1)
	call deco(p1,.true.,id1,idx1,ok2)
	call deco(p2,.true.,id2,idx2,ok3)
c
	if(.not.(ok1.and.ok2.and.ok3))then
	  return
	else if(((idx0.eq.0).neqv.(idx1.eq.0)).or.
     *	        ((idx1.eq.0).neqv.(idx2.eq.0)))then
     	  call bug('w','Cannot mix baseline/entire ops')
	  return
	endif
c
	if(idx0.eq.0)then
	  do i2=1,MAXANT
	    do i1=1,i2
	      bl = 256*i1 + i2
	      call SlotGet(id1,bl,idx1)
	      call SlotGet(id2,bl,idx2)
	      call SlotGet(id0,bl,idx0)
	      if(slotav(idx1).eq.0.or.slotav(idx2).eq.0)then
	        slotav(id0) = 0
	      else
	        call Op(idx0,idx1,idx2)
	      endif
	    enddo
	  enddo
	else
	  if(slotav(idx1).eq.0.or.slotav(idx2).eq.0)then
	    slotav(idx0) = 0
	  else
	    call Op(idx0,idx1,idx2)
	  endif
	endif
c
	end
c************************************************************************
	subroutine Add(idx0,idx1,idx2)
c
	implicit none
	integer idx0,idx1,idx2
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i,ic0,ic1,ic2,t
	real a,b
	if(slotic(idx0).eq.0)call Slotit(slotic(idx0))
c
	ic0 = slotic(idx0)
	ic1 = slotic(idx1)
	ic2 = slotic(idx2)
c
	t = min(slotav(idx1),slotav(idx2))
	a = real(t)/slotav(idx1)
	b = real(t)/slotav(idx2)
	slotav(idx0) = t
	do i=1,nchan
	  corr(i,ic0) =
     *	    a*corr(i,ic1) + b*corr(i,ic2)
	enddo
c
	end
c************************************************************************
	subroutine Sub(idx0,idx1,idx2)
c
	implicit none
	integer idx0,idx1,idx2
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i,ic0,ic1,ic2,t
	real a,b
	if(slotic(idx0).eq.0)call Slotit(slotic(idx0))
c
	ic0 = slotic(idx0)
	ic1 = slotic(idx1)
	ic2 = slotic(idx2)
c
	t = min(slotav(idx1),slotav(idx2))
	a = real(t)/slotav(idx1)
	b = real(t)/slotav(idx2)
	slotav(idx0) = t
	do i=1,nchan
	  corr(i,ic0) =
     *	    a*corr(i,ic1) - b*corr(i,ic2)
	enddo
c
	end
c************************************************************************
	subroutine Mul(idx0,idx1,idx2)
c
	implicit none
	integer idx0,idx1,idx2
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i,ic0,ic1,ic2,t
	real a,b
	if(slotic(idx0).eq.0)call Slotit(slotic(idx0))
c
	ic0 = slotic(idx0)
	ic1 = slotic(idx1)
	ic2 = slotic(idx2)
c
	t = min(slotav(idx1),slotav(idx2))
	a = real(t)/slotav(idx1)
	b = 1/slotav(idx2)
	slotav(idx0) = t
	do i=1,nchan
	  corr(i,ic0) =
     *	    a*corr(i,ic1) * b*corr(i,ic2)
	enddo
c
	end
c************************************************************************
	subroutine Div(idx0,idx1,idx2)
c
	implicit none
	integer idx0,idx1,idx2
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i,ic0,ic1,ic2,t
	real a,b
	if(slotic(idx0).eq.0)call Slotit(slotic(idx0))
c
	ic0 = slotic(idx0)
	ic1 = slotic(idx1)
	ic2 = slotic(idx2)
c
	t = min(slotav(idx1),slotav(idx2))
	a = real(t)/slotav(idx1)
	b = 1/slotav(idx2)
	slotav(idx0) = t
	do i=1,nchan
	  corr(i,ic0) =
     *	    a*corr(i,ic1) / ( b*corr(i,ic2) )
	enddo
c
	end
c************************************************************************
	subroutine deco(p,exist,id,idx,ok)
c
	implicit none
	logical ok,exist
	character p*(*)
	integer id,idx
c------------------------------------------------------------------------
	integer i,i1,i2,ant1,ant2,temp
	double precision bl
c
	integer len1
	logical SlotExst
c
	ok = .true.
	id = 0
	idx = 0
	i1 = index(p,'[')
	i2 = len1(p)
	if(i1.gt.1.and.p(i2:i2).eq.']')then
	  if(exist.and..not.SlotExst(p(1:i1-1)))then
	    call bug('w','Variable does not exist')
	    ok = .false.
	    return
	  endif
	  call SlotVar(p(1:i1-1),id)
	  i1 = i1 + 1
	  i2 = i2 - 1
	  temp = 0
	  do i=i1,i2
	    if(p(i:i).eq.',')then
	      ant1 = temp
	      temp = 0
	    else
	      temp = ichar(p(i:i)) - ichar('0') + 10*temp
	    endif
	  enddo
	  ant2 = temp
	  i1 = min(ant1,ant2)
	  i2 = max(ant1,ant2)
	  bl = 256*i1 + i2
	  call SlotGet(id,bl,idx)
	else if(index(p,'[').ne.0.or.index(p,']').ne.0)then
	  call bug('w','Invalid name/index')
	  ok = .false.
	else
	  if(exist.and..not.SlotExst(p))then
	    call bug('w','Variable does not exist')
	    ok = .false.
	  endif
	  call SlotVar(p,id)
	  idx = 0
	endif
	end
c************************************************************************
	subroutine uvLoad(tno,name,sels)
c
	implicit none
	integer tno
	character name*(*)
	real sels(*)
c
c------------------------------------------------------------------------
	include 'mbanal.h'
	double precision preamble(4)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	integer id,i,ic,idx,nspec,nread
c
	character itoaf*8
c
	call uvrewind(tno)
	call uvselect(tno,'clear',0.d0,0.d0,.true.)
	call SelApply(tno,sels,.true.)
c
c  Allocate a slot.
c
	call SlotVar(name,id)
c
	nspec = 0
	call uvread(tno,preamble,data,flags,MAXCHAN,nchan)
	nread = nchan
	dowhile(nread.eq.nchan)
	  call SlotGet(id,preamble(4),idx)
	  if(slotic(idx).eq.0)call Slotit(slotic(idx))
	  ic = slotic(idx)
	  nspec = nspec + 1
	  if(slotav(idx).eq.0)then
	    slotav(idx) = 1
	    do i=1,nchan
	      corr(i,ic) = data(i)
	    enddo
	  else
	    slotav(idx) = slotav(idx) + 1
	    do i=1,nchan
	      corr(i,ic) = corr(i,ic) + data(i)
	    enddo
	  endif
	  call uvread(tno,preamble,data,flags,MAXCHAN,nread)
	enddo
	call output('Spectra loaded: '//itoaf(nspec))
	if(nread.ne.0)call bug('w','Number of channels changed!')
	end
c************************************************************************
	subroutine Slotit(ic)
c
	implicit none
	integer ic
	include 'mbanal.h'
	nic = nic + 1
	if(nic.gt.MAXIC)call bug('f','Ran out of slots')
	ic = nic
	end
c************************************************************************
	subroutine SlotGet(id,bl,idx)
c
	implicit none
	integer id,idx
	double precision bl
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i1,i2
	call Basant(bl,i1,i2)
	idx = ((i2-1)*i2)/2 + i1
	if(Slotidx(idx,id).ne.0)then
	  idx = Slotidx(idx,id)
	else
	  nslot = nslot + 1
	  if(nslot.gt.MAXSLOT)call bug('f','Too many baselines')
	  slotav(nslot) = 0
	  slotic(nslot) = 0
	  Slotidx(idx,id) = nslot
	  idx = nslot
	endif
	end
c************************************************************************
	logical function SlotExst(name)
c
	implicit none
	character name*(*)
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i
	SlotExst = .false.
	do i=1,nvar
	  SlotExst = SlotExst.or.name.eq.var(i)
	enddo
c
	end
c************************************************************************
	subroutine SlotVar(name,id)
c
	implicit none
	character name*(*)
	integer id
c
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i
c
	do i=1,nvar
	  id = i
	  if(name.eq.var(i))return
	enddo
	if(nvar.eq.MAXVAR)call bug('f','Too many variables')
	nvar = nvar + 1
	var(nvar) = name
	id = nvar
	do i=1,MAXBASE
	  slotidx(i,id) = 0
	enddo
	end
c************************************************************************
	subroutine SlotIni
c
	implicit none
	include 'mbanal.h'
	nvar = 0
	nslot = 0
	nic = 0

	end
c************************************************************************
	real function realp(x)
	implicit none
	complex x
	realp = real(x)
	end
	real function imagp(x)
	implicit none
	complex x
	imagp = aimag(x)
	end
	real function ampp(x)
	implicit none
	complex x
	ampp = abs(x)
	end
	real function phasep(x)
	implicit none
	complex x
	phasep = 180/3.14159265*atan2(aimag(x),real(x))
	end
