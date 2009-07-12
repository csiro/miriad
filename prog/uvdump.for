************************************************************************
	program uvdump
	implicit none
c
c= UVDUMP - Generate an output text file of information from a visibility dataset.
c& rjs
c: uv analysis
c+
c	UVDUMP generates a text file containing information from a visibility
c	dataset. This information will usually be used as input to other
c	analysis outside Miriad.
c
c	For each data record selected, an line is produced in the output
c	text file. Each line can consist of several values.
c@ vis
c	The input UV dataset name. No default.
c@ vars
c	The name of the values to be written. Possible names are the names
c	of visibility variables (with no additional processing), or some
c	of the following special names given below. Note these names cannot
c	be abbreviated.
c	  real      Real part of a correlation.
c	  imag      Imaginary part of a correlation.
c	  amp       Amplitude of a correlation.
c	  phase     Phase of a correlation, in degrees.
c	  flag	    Data flag. This is 1 for good data and zero for bad data.
c	  variance  Theoretical variance of the real/imag parts.
c	  blno      Baseline number in the form xx-yy (character string).
c	  ant1      First antenna of the baseline.
c	  ant2      Second antenna of the baseline.
c	  polid     Polarization identifier (character string).
c	  dtime     Offset time, as a fraction of a day, relative to the date of
c	            first sample.
c	  tod	    Time-of-day, as a day fraction.
c	  freq      Frequency of each channel, in GHz.
c	  uu        U coordinate of the first channel after any frequency averaging
c	            is performed. The units are in wavelengths.
c	  vv        V coordinate. See "uu" above.
c	  ww        W coordinate. See "uu" above.
c	Several names can be given, separated by commas. 
c@ line
c	This selects which channels to include. See the help on `line' for
c	more information. The default is to include all data.
c@ select
c	This selects the data to be processed, using the standard uvselect
c	format. Default is all data.
c@ stokes
c	Normal stokes keyword. The default is to list the unconverted
c	polarisations.
c@ log
c	The list output file name. The default is the terminal.
c@ options
c	This controls what is listed, and the verbosity. Several can be
c	given, separated by commas. Minimum match is used. Possible values
c	are:
c	  flagged   Process flagged data only. By default only unflagged data
c	            are processed.
c	  all       Process all data. By default only unflagged data are processed.
c	  nofqav    Do not average the channels of each spectral to produce an average
c	            value. Note averaging affects the output for values:
c	            real,imag,amp,phase,flag,freq,variance,uu,vv,ww. Depending on
c	            the "flagged" and "all" options, either just the good, bad or all
c	            channels are used in the averaging.
c	  unwrap    Attempt to unwrap the phase (assumes consecutive data has the
c	            same wrap).
c	  space     By default values are separated by a comma. This option causes the
c	            values to be separated by a space instead.
c	The following control application of calibration corrections.
c	  nocal     Do not apply antenna gain calibration.
c	  nopol     Do not apply polarization leakage correction.
c	  nopass    Do not apply bandpass response correction.
c--
c
c  History:
c    rjs 26may09   Original version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	integer MAXVARS
	parameter(version='Uvdump: version 1.0 26-May-09')
	parameter(MAXVARS=16)
c
	character logf*50,uvflags*16,vars(MAXVARS)*8,line*256
	complex data(MAXCHAN),d
	double precision preamble(5),toff,freq(MAXCHAN),f
	real poff
	logical flags(MAXCHAN)
	logical docal,dopol,dopass,doall,doflag,fqav,space,unwrap,first
	integer nvars,lVis,nchan,length,i,n,ngood,nrec
c
c  Externals.
c
	logical uvDatOpn,keyprsnt
	character itoaf*10
c
c  Read the inputs.
c
	call output(version)
 	call keyini
 	call keya('log',logf,' ')
c
	nvars = 0
	dowhile(keyprsnt('vars'))
	  nvars = nvars + 1
	  if(nvars.gt.MAXVARS)call bug('f','Too many variables for me!')
	  call keya('vars',vars(nvars),' ')
	enddo
	if(nvars.eq.0)call bug('f','Values to print must be given')
c
c  Determine the uvDat flags. If no data are being handled, then turn off
c  all calibration.
c
	call GetOpt(docal,dopol,dopass,doall,doflag,fqav,unwrap,space)
	uvflags = 'sdlwb3'
	if(docal) uvflags(7:7) = 'c'
	if(dopol) uvflags(8:8) = 'e'
	if(dopass)uvflags(9:9) = 'f'
	call uvDatInp('vis',uvflags)
	call keyfin
c
c  Open the output text file.
c
 	call LogOpen(logf,' ')
c
c  Open the data file, apply selection, do linetype initialisation and
c  determine the variables of interest.
c
	if(.not.uvDatOpn(lVis))call bug('f','Failed to open a vis file')
	call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	poff = 0
	first = .true.
	nrec = 0
c
	dowhile(nchan.gt.0)
	  n = 0
	  call uvinfo(lVis,'sfreq',freq)
c
c  Frequency average and select based on flagging.
c
	  if(fqav)then
	    d = (0.,0.)
	    f = 0.d0
	    ngood = 0
	    do i=1,nchan
	      if(doall.or.(flags(i).neqv.doflag))then
		d = d + data(i)
		f = f + freq(i)
		if(flags(i))ngood = ngood + 1
		n = n + 1
	      endif
	    enddo
	    if(n.gt.0)then
	      data(1) = d/n
	      f = f/n
	      preamble(1) = preamble(1)/freq(1)*f
	      preamble(2) = preamble(2)/freq(1)*f
	      preamble(3) = preamble(3)/freq(1)*f
	      freq(1) = f
	      nchan = 1
	      flags(1) = 2*ngood.ge.n
	    endif
	  else if(doall)then
	    n = 1
	  else
	    ngood = 0
	    do i=1,nchan
	      if(flags(i))ngood = ngood + 1
	    enddo
	    if(	(     doflag.and.ngood.lt.nchan).or.
     *		(.not.doflag.and.ngood.gt.0))then
	      n = 1
	    else
	      n = 0
	    endif
	  endif
c
c  If there are appropriate data (after averaging), generate the output.
c
	  if(n.gt.0)then
	    if(first)toff = int(preamble(4)-0.5d0) + 0.5d0
	    first = .false.
	    length = 0
	    do i=1,nvars
	      call addval(lVis,line,length,vars(i),preamble,
     *		data,flags,freq,nchan,toff,poff,unwrap,space,n)
	    enddo
	    length = length - 1
	    if(length.gt.0)then
	      call logwrit(line(1:length))
	      nrec = nrec + 1
	    endif
	  endif
c
c  Loop.
c
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	enddo
	if(nrec.eq.0)call bug('f','No appropriate data found')
	call output('Records generated: '//itoaf(nrec))
c
c  Close up shop.
c
	call LogClose
	call uvDatCls
	end
c************************************************************************
	subroutine addval(lVis,line,length,var,preamble,data,flags,freq,
     *			nchan,toff,poff,unwrap,space,n)
c
	implicit none
	character line*(*),var*(*)
	integer length,nchan,lVis,n
	double precision preamble(5),freq(nchan),toff
	real poff
	complex data(nchan)
	logical flags(nchan),unwrap,space
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
c
	real temp,rbuf(MAXCHAN)
	double precision dbuf(MAXCHAN)
	character sstring*16,lstring*64,type*1
	integer ant1,ant2,l,itemp,ibuf(MAXCHAN),i
	logical updated
c
c  Externals.
c
	character polsC2P*4,itoaf*10,stcat*16
c
	if(var.eq.'real')then
	  if(nchan.gt.MAXCHAN)call bug('f','Too many channels for me')
	  do i=1,nchan
	    rbuf(i) = real(data(i))
	  enddo
	  call addreal(line,length,space,rbuf,nchan)
	else if(var.eq.'imag')then
	  if(nchan.gt.MAXCHAN)call bug('f','Too many channels for me')
	  do i=1,nchan
	    rbuf(i) = aimag(data(i))
	  enddo
	  call addreal(line,length,space,rbuf,nchan)
	else if(var.eq.'amp')then
	  if(nchan.gt.MAXCHAN)call bug('f','Too many channels for me')
	  do i=1,nchan
	    rbuf(i) = abs(data(i))
	  enddo
	  call addreal(line,length,space,rbuf,nchan)
	else if(var.eq.'phase')then
	  if(nchan.gt.MAXCHAN)call bug('f','Too many channels for me')
	  do i=1,nchan
	    if(abs(real(data(i)))+abs(aimag(data(i))).le.0)then
	      rbuf(i) = 0.0
	    else
	      rbuf(i) = 180.0/PI * atan2(aimag(data(i)),real(data(i)))
	      if(unwrap)rbuf(i) = 
     *			rbuf(i) - 360*nint((rbuf(i)-poff)/360.0)
	      poff = rbuf(i)
	    endif
	  enddo
	  call addreal(line,length,space,rbuf,nchan)
	else if(var.eq.'flag')then
	  if(nchan.gt.MAXCHAN)call bug('f','Too many channels for me')
	  do i=1,nchan
	    ibuf(i) = 0
	    if(flags(i))ibuf(i) = 1
	  enddo
	  call addint(line,length,space,ibuf,nchan)
	else if(var.eq.'freq')then
	  call adddble(line,length,space,freq,nchan)
	else if(var.eq.'variance')then
	  call uvDatGtr('variance',temp)
	  temp = temp / n
	  call addreal(line,length,space,temp,1)
	else if(var.eq.'blno')then
	  call basant(preamble(5),ant1,ant2)
	  sstring = stcat(stcat(itoaf(ant1),'-'),itoaf(ant2))
	  call addchar(line,length,space,sstring)
	else if(var.eq.'ant1')then
	  call basant(preamble(5),ant1,ant2)
	  call addint(line,length,space,ant1,1)
	else if(var.eq.'ant2')then
	  call basant(preamble(5),ant1,ant2)
	  call addint(line,length,space,ant2,1)
	else if(var.eq.'pol')then
	  call uvdatgti('pol',itemp)
	  call addint(line,length,space,itemp,1)
	else if(var.eq.'polid')then
	  call uvdatgti('pol',itemp)
	  call addchar(line,length,space,polsC2P(itemp))
	else if(var.eq.'dtime')then
	  temp = preamble(4) - toff
	  call addreal(line,length,space,temp,1)
	else if(var.eq.'tod')then
	  temp = preamble(4) - int(preamble(4) - 0.5d0) - 0.5d0
	  call addreal(line,length,space,temp,1)
	else if(var.eq.'uu')then
	  call addreal(line,length,space,real(preamble(1)),1)
	else if(var.eq.'vv')then
	  call addreal(line,length,space,real(preamble(2)),1)
	else if(var.eq.'ww')then
	  call addreal(line,length,space,real(preamble(3)),1)
	else
	  call uvprobvr(lVis,var,type,l,updated)
	  if(type.eq.' ')then
	    sstring = var
	    call bug('f','Unrecognised variable: '//sstring)
	  else if(type.eq.'i')then
	    if(l.gt.MAXCHAN)call bug('f','Too many values for me')
	    call uvgetvri(lVis,var,ibuf,l)
	    call addint(line,length,space,ibuf,l)
	  else if(type.eq.'r')then
	    if(l.gt.MAXCHAN)call bug('f','Too many values for me')
	    call uvgetvrr(lVis,var,rbuf,l)
	    call addreal(line,length,space,rbuf,l)
	  else if(type.eq.'d')then
	    if(l.gt.MAXCHAN)call bug('f','Too many values for me')
	    call uvgetvrd(lVis,var,dbuf,l)
	    call adddble(line,length,space,dbuf,l)
	  else if(type.eq.'a')then
	    if(l.gt.len(lstring))call bug('f','Too many values for me')
	    call uvgetvra(lVis,var,lstring)
	    call addchar(line,length,space,lstring)
	  else
	    sstring = var
	    call bug('f','Unrecognised data type for value: '//sstring)
	  endif
	endif
c
	end
c************************************************************************
	subroutine addreal(line,length,space,buf,n)
c
	implicit none
	integer length,n
	character line*(*)
	real buf(n)
	logical space
c
c------------------------------------------------------------------------
	character string*16
	integer i
c
	do i=1,n
	  write(string,'(1pe15.8)')buf(i)
	  call addchar(line,length,space,string)
	enddo
c
	end
c************************************************************************
	subroutine addint(line,length,space,buf,n)
c
	implicit none
	integer length,n
	character line*(*)
	integer buf(n)
	logical space
c
c------------------------------------------------------------------------
	integer i
c
c  Externals.
c
	character itoaf*10
c
	do i=1,n
	  call addchar(line,length,space,itoaf(buf(i)))
	enddo
c
	end
c************************************************************************
	subroutine adddble(line,length,space,buf,n)
c
	implicit none
	integer length,n
	character line*(*)
	double precision buf(n)
	logical space
c
c------------------------------------------------------------------------
	character string*20
	integer i
c
	do i=1,n
	  write(string,'(1pe20.13)')buf(i)
	  call addchar(line,length,space,string)
	enddo
c
	end
c************************************************************************
	subroutine addchar(line,length,space,string)
c
	implicit none
	integer length
	character line*(*),string*(*)
	logical space
c
c------------------------------------------------------------------------
	integer i1,i2,ldash
	logical more
c
c  Externals.
c
	integer len1
c
	i2 = len1(string)
	more = i2.gt.0
	i1 = 0
	dowhile(i1.lt.i2.and.more)
	  i1 = i1 + 1
	  more = string(i1:i1).eq.' '
	enddo
	if(i2.eq.0.or.i1.gt.i2)call bug('f','Null value')
	ldash = length + (i2-i1+1) + 1
	if(ldash.gt.len(line))call bug('f','Line overflow')
	line(length+1:ldash-1) = string(i1:i2)
	if(space)then
	  line(ldash:ldash) = ' '
	else
	  line(ldash:ldash) = ','
	endif
	length = ldash
c
	end
c************************************************************************
	subroutine GetOpt(docal,dopol,dopass,doall,doflag,fqav,unwrap,
     *	  space)
c
	implicit none
	logical docal,dopol,dopass,doall,doflag,fqav,unwrap,space
c
c  Determine which of the options is to be done.
c
c  Outputs:
c    docal,dopol,dopass	   Calibration switches.
c    doall,doflag
c    fqav
c    unwrap
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=8)
	character opts(nopts)*8
	logical present(nopts)
c
c                  12345678   12345678   12345678   12345678
	data opts/'nocal   ','nopol   ','nopass  ','all     ',
     *		  'flagged ','nofqav  ','unwrap  ','space   '/
c
	call options('options',opts,present,nopts)
c
	docal = .not.present(1)
	dopol = .not.present(2)
	dopass= .not.present(3)
	doall  = present(4)
	doflag = present(5)
	if(doall.and.doflag)call bug('f',
     *	  'Cannot use options "all" and "flagged" together')
	fqav   = .not.present(6)
	unwrap = present(7)
	space  = present(8)
	end
