c**********************************************************************c
	program uvstat
	implicit none
c= UVSTAT - plot uvdata statistics.
c& mchw
c: uv analysis, checking
c+
c	UVSTAT is a Miriad program to check the uvdata.
c	The default is to read through the selected uvdata and plot
c	the rms across the line for each record.
c@ vis
c	The input visibility file. No default.
c@ select
c	This selects which visibilities to be used. Default is
c	all visibilities. See the Users Guide for information about
c	how to specify uv data selection.
c@ line
c	Linetype of the data in the format line,nchan,start,width,step
c	"line" can be `channel', `wide' or `velocity'.
c	Default is channel,0,1,1,1, which returns actual number of spectral
c	channels. Use line=wide to check number of wideband correlations.
c	If the linetype requested is not present it will NOT be listed.
c@ device
c	PGPLOT device. Default is to prompt the user.
c@ xaxis
c	The xaxis to plot can be 'visno' 'time' or uvdist'. Default=visno.
c@ yaxis
c	The yaxis to plot can be 'rms' 'tsys'. Default=rms.
c@ log
c	The output log file. Default is the terminal.
c--
c
c  History:
c    mchw 29oct91  Initial version.
c    rjs  31oct91  Check that the plot buffer is not overflowed.
c    mjs  13mar93  pgplot subr names have less than 7 chars.
c    rjs  12nov93  Call logclose.
c
c  Optional extra ?
c	The yaxis to plot can be 'rms' 'linfit' or 'tsys'. Default=rms.
c----------------------------------------------------------------------c
	include 'maxdim.h'
	integer MAXPNT
	parameter(MAXPNT=10000)
	character*(*) version
	parameter(version='UVSTAT: version 1.0 29-Oct-91')
	integer maxsels
	parameter(maxsels=512)
	real sels(maxsels)
	double precision preamble(4),time0
	integer lIn,nchan,nread,nvis,length
	real start,width,step
	character vis*64,log*64,device*64,xaxis*64,yaxis*64
	character date*18,line*80
	complex data(maxchan)
	logical flags(maxchan)
	real xx(MAXPNT),yy(MAXPNT),xmin,xmax,ymin,ymax,rms,tsys
c
c  Externals
c
	integer len1,ismin,ismax
c
c  Get the parameters given by the user.
c
	call output(version)
	call keyini
	call keya ('vis',vis,' ')
	call keya ('line',line,'channel')
	call keyi ('line',nchan,0)
	call keyr ('line',start,1.) 
	call keyr ('line',width,1.)
	call keyr ('line',step,1.)
	call SelInput ('select',sels,maxsels)
	call keya ('device',device,'?')
	call keya ('xaxis',xaxis,'recnum')
	call keya ('yaxis',yaxis,'rms')
	call keya ('log',log,' ')
	call keyfin
c
c  Check that the inputs are reasonable.
c
	if (vis.eq.' ') call bug ('f', 'Input name must be given')
	if (nchan.gt.0) nchan = min (nchan,maxchan)
	if (nchan.lt.0) call bug ('f', 'Bad number of channels')
	if (width.le.0.0) call bug ('f','Negative width is useless')
	if (step.eq.0.0) call bug ('f', 'Step=0.0 is useless') 
	if (xaxis.ne.'time'.and.xaxis.ne.'uvdist'
     *			.and.xaxis.ne.'recnum') xaxis = 'recnum'
	if (yaxis.ne.'rms'.and.yaxis.ne.'tsys') yaxis = 'rms'
c    *			.and.yaxis.ne.'linfit') yaxis = 'rms'
c
c  Open an old visibility file, and apply selection criteria.
c
	call uvopen (lIn,vis,'old')
	call uvset(lIn,'data',line,nchan,start,width,step)
	call uvset (lIn,'coord','nanosec',0, 0.0, 0.0, 0.0)
	call uvset (lIn,'planet', ' ', 0, 0.0, 0.0, 0.0)
	call SelApply(lIn,sels,.true.)
c
c  Open log file and write title.
c
	call LogOpen(log,'q')
	call LogWrit(version)
	line = 'File: '//vis
	call LogWrit(line(1:len1(line)))
	call LogWrit(' ')
c
c  Miscelaneous initialization.
c
	nvis = 0
c
c  Read the first record.
c
	call uvread (lIn, preamble, data, flags, maxchan, nread)
	if(nread.le.0) call bug('f','No data found in the input.')
	time0 = preamble(3)
	call JulDay(preamble(3),'H',date)
c
c  Read through the selected data.
c
	do while(nread.gt.0.and.nvis.lt.MAXPNT)
	  nvis = nvis + 1
c
c  Fill in the xaxis
c
	  if(xaxis.eq.'time')then
	    xx(nvis) = preamble(3)-time0
	  else if(xaxis.eq.'uvdist')then
	    xx(nvis) = 
     *		sqrt(preamble(1)*preamble(1)+preamble(2)*preamble(2))
	  else
	    xx(nvis) = nvis
	  endif
c
c  Fill in the yaxis
c
	  if(yaxis.eq.'tsys')then
	    call uvrdvrr(lIn,'systemp',tsys,0.)
	    yy(nvis) = tsys
c	  else if(yaxis.eq.'linfit')then
c	    call uvfit(data, flags, nread, maxchan, rms)
c	    yy(nvis) = rms
	  else
	    call uvrms(data, flags, nread, maxchan, rms)
	    yy(nvis) = rms
	  endif
c
	  call uvread(lIn, preamble, data, flags, maxchan, nread)
	enddo
	if(nread.gt.0)call bug('w',
     *	  'Too much data -- some data discarded')
c
c  Plot the statistics.
c
	xmin = xx(ismin(nvis,xx,1))
	xmax = xx(ismax(nvis,xx,1))
	ymin = yy(ismin(nvis,yy,1))
	ymax = yy(ismax(nvis,yy,1))
	call pgbeg(0,device,1,1)
	call pgsvp(.1,.9,.1,.7)
	call pgswin(xmin,xmax,ymin,ymax)
	call pgbox('bcnst',0.,0.,'bcnst',0.,0.)
	call pghline(nvis,xx,yy,2.)
	call pglab(xaxis,yaxis,vis)
	call pgend
c
c  Write summary.
c
	write(line,'(a,a,i6)') date,' number of records= ',nvis
	    length = 18 + 20 + 6
	    call LogWrit(line(1:length))
	call LogClose
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine uvrms(data, flags, nread, maxchan, rms)
	implicit none
	integer nread,maxchan
	real rms
	complex data(maxchan)
	logical flags(maxchan)
c
c  Input:
c    nread	Number of channels
c    maxchan	Maximum number of channels
c    flags	Flags
c    data	Data.
c  Output:
c    rms
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
	do i = 1,nread
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
	else
	  rms = 0.
	endif
c
	end
