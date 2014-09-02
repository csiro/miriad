c************************************************************************
	program uvRMS
	implicit none
c
c= uvrms - Measure the RMS value of a visibility dataset.
c& rjs
c: uv analysis
c+
c	UVRMS determines the RMS value of a Miriad visibility data.
c@ vis
c	The names of the input uv data sets. No default.
c@ stokes
c	The Stokes/polarisation visibilities to select. The default is to
c	not perform any polarisation conversion, but to apply polarisation
c	calibration (see below).
c@ select
c	The normal uv selection commands. The default is to select everything.
c@ line
c	The normal uv linetype in the form:
c	  line,nchan,start,width,step
c	The default is all channels.
c@ options
c	Extra processing options.
c	   nocal       Do not apply the gains table.
c	   nopass      Do not apply bandpass corrections. It is unwise
c	               to turn off bandpass correction, as the continuum
c	               estimation process will be confused by a bandpass
c	               which is not flat.
c	   nopol       Do not apply polarization corrections.
c--
c  History:
c    rjs  24jan07 Original version.
c    mhw  12jun14 Cope with >2^30 visibilities
c  Bugs:
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='UvRms: version 1.0 24-Jan-07')
c
	character uvflags*16
	logical flags(MAXCHAN)
	complex data(MAXCHAN)
	double precision preamble(4)
	double precision SumV2,SumS2,n
	integer lIn,nchan,i
	real var,expect,rms
c
c  Externals.
c
	logical uvDatOpn
	character streal*16
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags)
	call uvDatInp('vis',uvflags)
	call keyfin
c
	SumV2 = 0
	SumS2 = 0
	n = 0
c
c  Open the inputs and the outputs.
c
	dowhile(uvDatOpn(lIn))
	  call uvdatRd(preamble,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
	    call uvdatGtr('variance',var)
	    do i=1,nchan
	      if(flags(i))then
		SumV2 = SumV2 + real(data(i))**2 + aimag(data(i))**2
		SumS2 = SumS2 + 2*var
		n = n + 2
	      endif
	    enddo
	    call uvdatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
	  call uvdatCls
	enddo
c
	if(n.eq.0)call bug('f','No good data found')
	rms = sqrt(SumV2/n)
	expect = sqrt(SumS2/n)
c
	call output('Measured RMS: '//streal(rms,'(1pg10.3)'))
	call output('Expected RMS: '//streal(expect,'(1pg10.3)'))
c
	end
c************************************************************************
	subroutine GetOpt(uvflags)
c
	implicit none
	character uvflags*(*)
c
c  Determine extra processing options.
c
c  Output:
c    uvflags
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	logical present(NOPTS),docal,dopol,dopass
	character opts(NOPTS)*9
c
	data opts/'nocal    ','nopol    ','nopass   '/
c
	call options('options',opts,present,NOPTS)
	docal  = .not.present(1)
	dopol  = .not.present(2)
	dopass = .not.present(3)
c
c  Determine the flags to pass to the uvDat routines.
c    d - Data selection.
c    l - Linetype processing.
c    s - Stokes processing.
c    c - Apply gain table.
c    e - Apply leakage correction.
c    f - Apply bandpass correction.
c
	uvflags = 'dls'
	if(docal) uvflags(5:5) = 'c'
	if(dopol) uvflags(6:6) = 'e'
	if(dopass)uvflags(7:7) = 'f'
c
	end
