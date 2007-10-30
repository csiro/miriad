c************************************************************************
	program lagflg
	implicit none
c
c= lagflg - Flag visibility data in the lag domain.
c& rjs
c: calibration
c+
c	LAGFLG is a Miriad task to flag visibility data in the lag domain.
c	As visibility flags are correlation-based (not lag based), this task
c	has to produce a new outpput file.
c
c	Ideally, the input visibility file would be before Hanning smoothing
c	(if any), any calibration has been applied, and certainly before any
c	subsetting of the visibility spectrum has been performed.
c
c	This requires that the number of channels is 2**n+1, for some value of
c	n.
c@ vis
c	Input visibility file to be flagged. No default.
c@ out
c	Output visibility file, after flagging. No default.
c@ lags
c	A list of pairs, giving the lags to be flagged. Each pair gives
c	a range of lag values. These are indices, with the zero lag having
c	the value 0. This is the same lag numbering system used by uvspec
c	when displaying lags. No default - at least one range pair must be
c	given.
c@ select
c	Normal visibility selection. See the help on "select" for more
c	information. All data in the input is copied to the output, but
c	only those data selected using the "select" keyword will go through
c	the lag flagging process. Currently only "polarization", "antenna"
c	and "time" selection is supported in this task. This should be
c	sufficient to select a correlator product that is bad over a
c	particular period.
c--
c
c  History:
c    17-Mar-97 rjs  Original version.
c    14-feb-01 rjs  Tidy up.
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	integer MAXSELS,MAXLAGS
	parameter(version='Lagflg: version 1.0 21-Feb-01')
	parameter(MAXSELS=200,MAXLAGS=32)
c
	integer tIn,tOut,n,off,nvis,i,j,pol,npol,nchan
	character vis*64,out*64
	real rdata(2*MAXCHAN-2),sels(MAXSELS)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	integer lags(2,MAXLAGS),nlags
	double precision preamble(5)
c
c  Externals.
c
	integer nextpow2
	logical selProbe
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input must be given')
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','Output must be given')
	call selinput('select',sels,MAXSELS)
	call mkeyi('lags',lags,2*MAXLAGS,nlags)
	if(mod(nlags,2).ne.0)
     *	  call bug('f','Odd number of values given for "lags"')
	if(nlags.eq.0)call bug('f','No values were given for "lags"')
	nlags = nlags / 2
	call keyfin
c
	call uvopen(tIn,vis,'old')
	call uvset(tIn,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varInit(tIn,'channel')
	call uvopen(tOut,out,'new')
	call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varOnit(tIn,tOut,'channel')
c
	nvis = 0
	call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	dowhile(nchan.gt.0)
	  nvis = nvis + 1
	  off = nchan
	  n = 2*nchan-2
	  if(nextpow2(n).ne.n)call bug('f',
     *	     'Number of correlator channels must be 2**n+1')
	  call uvrdvri(tIn,'pol',pol,0)
	  call uvrdvri(tIn,'npol',npol,0)
c
	  if(selProbe(sels,'time',preamble(4)).and.
     *	     selProbe(sels,'antennae',preamble(5)).and.
     *	     selProbe(sels,'polarization',dble(pol)))then
	    call fftcr(data,rdata,-1,n)
	    call shifty(rdata,n)
	    do j=1,nlags
	      if(lags(1,j)+off.lt.1.or.lags(2,j)+off.gt.n)
     *		call bug('f','Invalid lag range')
	      do i=lags(1,j),lags(2,j)
	        rdata(i+off) = 0
	      enddo
	    enddo
	    call shifty(rdata,n)
	    call fftrc(rdata,data,+1,n)
	    do i=1,nchan
	      data(i) = data(i) / n
	    enddo
	  endif
	  call varCopy(tIn,tOut)
	  if(pol.ne.0)call uvputvri(tOut,'pol',pol,1)
	  if(npol.ne.0)call uvputvri(tOut,'npol',npol,1)
	  call uvwrite(tOut,preamble,data,flags,nchan)
	  call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
c  Copy history and calibration to the output.
c
	call hdCopy(tIn,tOut,'history')
	call hisOpen(tOut,'append')
	call hisWrite(tOut,'LAGFLG: Miriad '//version)
	call hisInput(tOut,'LAGFLG')
	call hisClose(tOut)
c
	call calcopy(tIn,tOut)
c
	call uvclose(tIn)
	call uvclose(tOut)
c
	end
c************************************************************************
	subroutine calcopy(tIn,tOut)
c
	implicit none
	integer tIn,tOut
c
c  Copy across any calibration tables.
c
c------------------------------------------------------------------------
	integer i
        integer NTABLE
        parameter(NTABLE=12)
        character tables(NTABLE)*8
        data tables/'interval','nsols   ','ngains  ','nfeeds  ',
     *   'ntau    ','gains   ','freq0   ','leakage ','bandpass',
     *   'freqs   ','nspect0 ','nchan0  '/
c
	do i=1,NTABLE
	  call hdcopy(tIn,tOut,tables(i))
	enddo
	end
c************************************************************************
	subroutine shifty(rdata,n)
c
	implicit none
	integer n
	real rdata(n)
c------------------------------------------------------------------------
	integer i
	real t
c
	do i=1,n/2
	  t = rdata(i)
	  rdata(i) = rdata(i+n/2)
	  rdata(i+n/2) = t
	enddo
	end
