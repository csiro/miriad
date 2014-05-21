c**********************************************************************c
	program uvstat
	implicit none
c= UVSTAT - plot uvdata statistics.
c& mchw
c: uv analysis, checking
c+
c	UVSTAT is a Miriad program to plot statistics of a visibility
c	dataset. The default is to read through the selected data and plot
c	the rms of each record.
c@ vis
c	The input visibility file. No default.
c@ select
c	This selects which visibilities to be used. Default is
c	all visibilities. See the help on "select" for more information.
c@ line
c	Normal visibility linetype specification. See the help on "line"
c	for more information. The linetype is in the format
c	  line,nchan,start,width,step
c	where "line" can be `channel', `wide' or `velocity'.
c	The default is all spectral channels.
c@ device
c	PGPLOT device. No default.
c@ axis
c	This gives the x and y axes to plot.
c	The xaxis to plot can be "recnum", "time" or "uvdist".
c	Default is "recnum".  The yaxis to plot can be "rms", "trms"
c	(the theoretical rms) or "tsys". The default is "rms".
c@ options
c	Extra processing options. Several can be given, separated by
c	commas. Minimum match applies. Possible options are:
c	  nocal   Do not apply antenna gain calibration. The default is to
c	          apply these.
c	  nopol   Do not apply polarisation leakage correction. The default
c	          is to apply any leakage corrections present.
c	  nopass  Do no apply bandpass calibration. The default is to apply
c	          bandpass calibration if possible.
c--
c
c  History:
c    mchw 29oct91  Initial version.
c    rjs  31oct91  Check that the plot buffer is not overflowed.
c    mjs  13mar93  pgplot subr names have less than 7 chars.
c    rjs  12nov93  Call logclose.
c    rjs  10oct97  Significant tidy up.
c----------------------------------------------------------------------c
	include 'maxdim.h'
	integer MAXPNTS
	parameter(MAXPNTS=100000)
	character*(*) version
	parameter(version='UVSTAT: version 1.0 10-Oct-97')
	double precision preamble(4),time0
	integer lIn,nchan,nvis
	character device*64,xaxis*64,yaxis*64
	character date*18,line*80,uvflags*16
	complex data(maxchan)
	logical flags(maxchan),ok
	real xx(MAXPNTS),yy(MAXPNTS)
	real xlo,xhi,ylo,yhi,xmin,xmax,ymin,ymax,x,y
	double precision dtemp
c
c  Externals
c
	integer ismin,ismax,pgbeg
	logical uvdatOpn
c
c  Get the parameters given by the user.
c
	call output(version)
	call keyini
	call GetOpt(uvflags)
	call uvdatinp('vis',uvflags)
	call keya ('line',line,'channel')
	call keya ('device',device,' ')
	if(device.eq.' ')call bug('f','No plot device given')
	call GetAxis(xaxis,yaxis)
	call keyfin
c
c  Open an old visibility file, and apply selection criteria.
c
	if(.not.uvDatOpn(lIn))call bug('f','Error opening input')
c
c  Read the first record.
c
	call uvDatRd(preamble,data,flags,maxchan,nchan)
	if(nchan.le.0) call bug('f','No data found in the input.')
	time0 = nint(preamble(3)) - 0.5d0
	call JulDay(time0,'H',date)
c
c  Read through the selected data.
c
	nvis = 0
	do while(nchan.gt.0)
c
c  Fill in the xaxis
c
	  if(xaxis.eq.'time')then
	    x = 86400*(preamble(3)-time0)
	  else if(xaxis.eq.'uvdist')then
	    x = sqrt(preamble(1)*preamble(1)+preamble(2)*preamble(2))
	  else
	    call uvinfo(lIn,'visno',dtemp)
	    x = dtemp
	  endif
c
c  Fill in the yaxis
c
	  ok = .true.
	  if(yaxis.eq.'tsys')then
	    call uvrdvrr(lIn,'systemp',y,0.)
	  else if(yaxis.eq.'trms')then
	    call uvinfo(lIn,'variance',dtemp)
	    y = sqrt(real(dtemp))
	  else
	    call uvrms(data, flags, nchan, y, ok)
	  endif
c
	  if(ok)then
	    nvis = nvis + 1
	    if(nvis.gt.MAXPNTS)call bug('f','Too many points to plot')
	    xx(nvis) = x
	    yy(nvis) = y
	  endif
c
	  call uvDatRd(preamble,data,flags,maxchan,nchan)
	enddo
	call uvDatCls
c
c  Plot the statistics.
c
	xmin = xx(ismin(nvis,xx,1))
	xmax = xx(ismax(nvis,xx,1))
	ymin = yy(ismin(nvis,yy,1))
	ymax = yy(ismax(nvis,yy,1))
	call pgrnge(xmin,xmax,xlo,xhi)
	call pgrnge(ymin,ymax,ylo,yhi)
c
	if(pgbeg(0,device,1,1).ne.1)
     *	  call bug('f','Error opening PGPLOT device')
	call pgscf(2)
	call pgpage
	call pgvstd
	call pgswin(xlo,xhi,ylo,yhi)
	if(xaxis.eq.'time')then
	  call pgtbox('BCNSTHZO',0.,0,'BCNST',0.,0)
	else
	  call pgbox('BCNST',0.,0.,'BCNST',0.,0.)
	endif
	if(nvis.lt.100)then
	  call pgpt(nvis,xx,yy,17)
	else
	  call pgpt(nvis,xx,yy,1)
	endif
	call pglab(xaxis,yaxis,' ')
	call pgend
c
	write(line,'(a,a,i6)') date,' number of records= ',nvis
	call output(line)
c
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine uvrms(data, flags, nchan, rms, ok)
	implicit none
	integer nchan
	real rms
	complex data(nchan)
	logical flags(nchan),ok
c
c  Input:
c    nchan	Number of channels
c    flags	Flags
c    data	Data.
c  Output:
c    rms
c    ok
c-----------------------------------------------------------------------
	integer i
	real sumx,sumy,sumsqx,sumsqy,count,x,y
c
	sumx = 0.
	sumy = 0.
	sumsqx = 0.
	sumsqy = 0.
	count = 0.
c
	do i = 1,nchan
	  if(flags(i))then
	    x = real(data(i))
	    y = aimag(data(i))
	    sumx = sumx + x
	    sumy = sumy + y
	    sumsqx = sumsqx + x*x
	    sumsqy = sumsqy + y*y
	    count = count + 1.
	  endif
	enddo
c
	if(count.gt.1)then
	  rms = sqrt( (sumsqx+sumsqy)/count
     *		 - (sumx*sumx+sumy*sumy)/(count*count) )
	  ok = .true.
	else
	  ok = .false.
	endif
c
	end
c************************************************************************
	subroutine GetOpt(uvflags)
c
	implicit none
	character uvflags*(*)
c
c  Get the processing options.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	character opts(NOPTS)*8
	logical present(NOPTS),nocal,nopol,nopass
c
	data opts/'nocal   ','nopol   ','nopass  '/
c
	call options('options',opts,present,NOPTS)
	nocal = present(1)
	nopol = present(2)
	nopass= present(3)
	uvflags = 'sldpbw'
	if(.not.nocal) uvflags(7:7) = 'c'
	if(.not.nopol) uvflags(8:8) = 'e'
	if(.not.nopass)uvflags(9:9) = 'f'
	end
c************************************************************************
        subroutine GetAxis(xaxis,yaxis)
c
        implicit none
        character xaxis*(*),yaxis*(*)
c
c  Determine the X and Y axis to plot.
c
c  Output: 
c    xaxis
c    yaxis
c------------------------------------------------------------------------
        integer NX,NY
        parameter(NX=3,NY=3)
c
        integer n
        character xaxes(NX)*9,yaxes(NY)*9
        data xaxes/'recnum   ','uvdist   ','time     '/
        data yaxes/'rms      ','tsys     ','trms     '/
c    
        call keymatch('axis',NX,xaxes,1,xaxis,n)
        if(n.eq.0)xaxis = xaxes(1)
        call keymatch('axis',NY,yaxes,1,yaxis,n)
        if(n.eq.0)yaxis = yaxes(1)
        end

