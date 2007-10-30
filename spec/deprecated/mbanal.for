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
c	Commands include:
c	  Command    Args
c	  -------    ----
c	  load       var  select
c	  save       var  dataset
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
c@ vis
c	Name of the input visibility dataset. No default.
c@ line
c	Standard line-type specification. See the help on "line"
c	for more information.
c@ script
c	Text file of commands to execute. The default is to
c	interactively prompt at the terminal.
c@ device
c	Plotting device. Default is /xs. The plotting device can
c	be overridden on the MBANAL prompt.
c--
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Mbanal: version 1.0 18-Dec-97')
	character vis*64,device*64,ltype*16,string*128,type*1
	character p1*32,p2*256,p3*32,p4*32,token*8,script*64
	integer nchan,tIn,k1,k2,length,lt,lp1,lp2,lp3,lp4
	logical update
	real lstart,lwidth,lstep
c
	integer len1
	external add,sub,mul,div,realp,imagp,ampp,phasep,conjgp,sqrtp
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input vis must be given')
	call keyline(ltype,nchan,lstart,lwidth,lstep)
	call keya('device',device,'/xs')
	call keya('script',script,' ')
	call keyfin
c
	call SlotIni
c
c  Open the vis dataset.
c
	call uvopen(tIn,vis,'old')
	if(ltype.ne.' ')then
	  call uvset(tIn,'data',ltype,nchan,lstart,lwidth,lstep)
	else
	  call uvprobvr(tIn,'corr',type,length,update)
	  if(type.eq.'j'.or.type.eq.'r'.or.type.eq.'c')then
	    ltype = 'channel'
	  else
	    ltype = 'wide'
	  endif
	endif
	call varInit(tIn,ltype)
c
c  Now go into the interactive loop.
c
	if(script.ne.' ')call tinOpen(script,' ')
 100	if(script.ne.' ')then
	  call tinLine(string,length)
	  if(length.eq.0)then
	    string(1:4) = 'exit'
	    length = 4
	  endif
	else
	  call prompt(string,length,'MBANAL> ')
	endif
	k2 = min(length,len(string))
	if(k2.gt.0)k2 = len1(string(1:k2))
	if(k2.gt.0)call lcase(string(1:k2))
	k1 = 1
	token = ' '
	p1 = ' '
	p2 = ' '
	p3 = ' '
	p4 = ' '
	call getfield(string,k1,k2,token,lt)
	call getfield(string,k1,k2,p1,lp1)
	call getfield(string,k1,k2,p2,lp2)
	call getfield(string,k1,k2,p3,lp3)
	call getfield(string,k1,k2,p4,lp4)
	if(token.eq.' ')then
	  continue
	else if(index('load',token(1:lt)).eq.1)then
	  call uvLoad(tIn,p1(1:lp1),p2(1:lp2))
	else if(index('save',token(1:lt)).eq.1)then
	  call uvsave(tIn,p1(1:lp1),p2(1:lp2),ltype)
	else if(index('conjugate',token(1:lt)).eq.1)then
	  if(p2.eq.' ')p2 = p1
	  call doOne(p1,p2,Conjgp)
	else if(index('sqrt',token(1:lt)).eq.1)then
	  if(p2.eq.' ')p2 = p1
	  call doOne(p1,p2,Sqrtp)
	else if(index('exit',token(1:lt)).eq.1.or.
     *		index('quit',token(1:lt)).eq.1)then
	  call uvclose(tIn)
	  if(script.ne.' ')call tinClose
	  call exit
	else if(index('add',token(1:lt)).eq.1)then
	  call doTwo(p1,p2,p3,Add)
	else if(index('subtract',token(1:lt)).eq.1)then
	  call doTwo(p1,p2,p3,Sub)
	else if(index('multiply',token(1:lt)).eq.1)then
	  call doTwo(p1,p2,p3,Mul)
	else if(index('divide',token(1:lt)).eq.1)then
	  call doTwo(p1,p2,p3,Div)
	else if(index('plot',token(1:lt)).eq.1)then
	  if(p2.eq.' ')then
	    p2 = 'amp'
	    lp2 = 3
	  endif
	  if(p3.eq.' ')then
	    p3 = device
	    lp3 = len1(device)
	  endif
	  if(index('real',p2(1:lp2)).ne.0)then
	    call plotit(p1,p3,realp)
	  else if(index('imag',p2(1:lp2)).ne.0)then
	    call plotit(p1,p3,imagp)
	  else if(index('amplitude',p2(1:lp2)).ne.0)then
	    call plotit(p1,p3,ampp)
	  else if(index('phase',p2(1:lp2)).ne.0)then
	    call plotit(p1,p3,phasep)
	  endif
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
	subroutine uvSave(tIn,name,dataset,ltype)
c
	implicit none
	integer tIn
	character name*(*),dataset*(*),ltype*(*)
c------------------------------------------------------------------------
	include 'mbanal.h'
	logical ok
	integer id0,idx0,i1,i2,tOut
	double precision bl
	double precision preamble(4)
c
	call deco(name,.true.,id0,idx0,ok)
	if(.not.ok)return
c
	call uvopen(tOut,dataset,'new')
	call varOnit(tIn,tOut,ltype)
c
	call uvrewind(tIn)
	call uvNext(tIn)
	preamble(1) = 0
	preamble(2) = 0
	call uvrdvrd(tIn,'time',preamble(3),0.d0)
	call varCopy(tIn,tOut)
c
	if(idx0.eq.0)then
	  do i2=1,MAXANT
	  do i1=1,i2
	    bl = 256*i1 + i2
	    preamble(4) = bl
	    call SlotGet(id0,bl,idx0)
	    if(slotav(idx0).ne.0)
     *	      call blSave(idx0,tOut,preamble)
	  enddo
	  enddo
	else
	  call bug('f','Cannot save a single spectrum')
	endif
c
	call uvclose(tOut)
	end
c************************************************************************
	subroutine blSave(idx,tOut,preamble)
c
	implicit none
	integer idx,tOut
	double precision preamble(4)
c------------------------------------------------------------------------
	include 'mbanal.h'
	integer i,ic
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
c
	ic = slotic(idx)
c
	do i=1,nchan
	  data(i) = corr(i,ic)/slotav(idx)
	  flags(i) = .true.
	enddo
c
	call uvwrite(tOut,preamble,data,flags,nchan)
c
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
	subroutine uvLoad(tno,name,select)
c
	implicit none
	integer tno
	character name*(*),select*(*)
c
c------------------------------------------------------------------------
	integer MAXSELS
	parameter(MAXSELS=256)
	include 'mbanal.h'
	
	double precision preamble(4)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	character line*512
	integer id,i,ic,idx,nspec,nread
	real sels(MAXSELS)
c
c  Externals.
c
	character itoaf*8
	integer len1
c
	call keyinic
	line = 'select='//select
	call keyputc(line(1:len1(line)))
	call selInput('select',sels,MAXSELS)
	call keyfin
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
