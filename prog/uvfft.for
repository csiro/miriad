c********1*********2*********3*********4*********5*********6*********7*c
	program uvfft
	implicit none
c
c= UVFFT - Fourier transform a sequence of selected uv-data.
c& mchw
c: uv analysis
c+
c	UVFFT takes a 1-D Fourier transform of a uv-data sequence. 
c	If the sequence is a time series, then the FFT is a fringe
c	rate spectrum. If the sequence steps the delay center, then
c	the FFT is a bandpass.
c@ vis
c	The input UV dataset name. No default.
c@ select
c	This selects the data to be processed, using the standard uvselect
c	format. Default is all data. Selecting more than one baseline
c	does not make much sense.
c@ line
c	Standard linetype of data to be tranformed, in the form:
c	  type,nchan,start,width,step
c	where type can be `channel'  `wide' or `velocity'.
c	The default is wide,2,1,1,1. The maximum number of channels
c	which can be processed is 8.
c@ size   
c	The length of the sequence for the Fourier transform.
c	uv-data points in excess of the size of the transform are omitted.
c	Must be a power of 2. The default is 128.
c@ log
c	The list output file name. The default is the terminal.
c@ device
c	standard PGPLOT device, e.g. /xw
c@ delay
c	Step size for stepped delay center. FFT is bandpass function.
c	Default=0  Delay center not stepped. FFT is fringe frequency spectrum.
c& options
c	processing options:
c	  test - substitute test functions for data into Fourier transform.
c--
c  History:
c   18nov93 mchw - original version adopted from onedfft.for
c   07mar94 mchw - improved doc and more checks.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	parameter(version='UVFFT: version 1.0 07-Mar-94')
	integer MAXSELS,MAXSIZE,MAXIN
	parameter(MAXSELS=1024,MAXSIZE=1024,MAXIN=33)
	real sels(maxsels)
	real start,step,width
	character linetype*20,vis*50,log*50,date*18,line*200,device*20
	character options*20
	complex data(maxchan),in(MAXSIZE,MAXIN),out(MAXSIZE,MAXIN)
	real amp(MAXIN),phase(MAXIN)
	logical flags(maxchan)
	integer unit,numchan,num,i,j,size,delay,nchan
	double precision uin,vin,timein,basein
	common/preamb/uin,vin,timein,basein
c
c  Read the inputs.
c
	call output(version)
 	call keyini
	call keyf('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input file must be given')
	call SelInput('select',sels,maxsels)
	call keya('line',linetype,'wide')
	call keyi('line',numchan,2)
	if(numchan.gt.MAXIN)
     *		call bug('f','Maximum number of channels exceeded')
	call keyr('line',start,1.)
	call keyr('line',width,1.)
	call keyr('line',step,width)
	call keyi('size',size,128)
	call keyr('delay',delay,0.)
 	call keya('log',log,' ')
 	call keya('device',device,' ')
 	call keya('options',options,' ')
	call keyfin
c
c  Initialize the data.
c
	do i=1,numchan
	  do j=1,size
	    in(j,i) = (0.,0.)
	  enddo
	enddo
c
c  Open the output text file.
c
 	call LogOpen(log,' ')
c
c  Open the data file, apply selection, do linetype initialisation and
c  determine the variables of interest.
c
	call uvopen(unit,vis,'old')
	call SelApply(unit,sels,.true.)
	  if(linetype.ne.' ')
     *	    call uvset(unit,'data',linetype,numchan,start,width,step)
	  call uvset(unit,'coord','wavelength',0,0.,0.,0.)
c
c  Read through the file, storing the selected data.
c
	num=0
	call uvread(unit,uin,data,flags,maxchan,nchan)
	call JulDay(dble(timein+0.5),'H',date)
	call LogWrit('Data values for '//date(1:7))
	dowhile (nchan.gt.0 .and. num.lt.size)
	  num = num + 1
	  do i = 1,numchan
	    in(num,i) = data(i)
	  enddo
c
c  Loop the loop.
c
	  call uvread(unit,uin,data,flags,maxchan,nchan)
	enddo
c
c  Notify the user.
c
	if(num.eq.0)call bug('f','No data selected')
	write(line,'(i5,a)') num,' points selected'
	call LogWrit(line)
c
c  options=test
c
	if(options.eq.'test')then
	  do i=1,numchan
	    do j=1,size
	      in(j,i) = (0.,0.)
	    enddo
	    do j=size/2+1-2**i,size/2+1+2**i
	      in(j,i) = (1.,0.)
	    enddo
	  enddo
	endif
c
c  Fourier transform the data.
c
	do i=1,numchan
	  call fftcc(in(1,i),out(1,i),-1,size)
	enddo
c
c  List the output.
c
	do j = 1,size
	  do i=1,numchan
	    call amphase(out(j,i),amp(i),phase(i))
	  enddo
	  write(line,'(i5,x,16f10.3)') j,(amp(i),phase(i),i=1,8)
	  call LogWrit(line)
	enddo
c
c  Plot the results.
c
	if(device.ne.' ')
     *		 call plotit(device,size,numchan,out,MAXSIZE,MAXIN)
c
c  Close up shop.
c
	call LogClose
	call uvclose(unit)
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine plotit(device,size,numchan,out,MAXSIZE,MAXIN)
	character*(*) device
	integer size,numchan,maxsize,maxin
	complex out(MAXSIZE,MAXIN)
c
c  Input:
c    device	PGPLOT device
c    size	number of columns to be plotted.
c    nrow	number of rows to plot.
c    out	Complex FFT array
c----------------------------------------------------------------------c
	real amp(1024),phase(1024)
	real yh, maxamp
	character*3 ans
	integer i,j,ip,ismax
	character*5 xflag
c	   
c  Start pgplot
c
	call pgbeg(0,device,1,1)
c
c  Device dependant setup. Adjust plot size and line width for device
c
	call pgqinf('HARDCOPY',ans,ip)
	if (ans.eq.'YES') then
	  call pgslw(3)
	  call pgscf(2)
	else
	  call pgslw(1)
	  call pgscf(1)
	end if
c
c  Draw the plots.
c
	yh=.9/numchan
	do i=1,numchan
	  do j=1,size
	    call amphase(out(j,i),amp(j),phase(j))
	  enddo
	  maxamp=amp(ismax(size,amp,1))
	  call pgsvp(0.1,.9,.98-i*yh,.98-i*yh+yh)
	  call pgswin(0.,float(size),0.,maxamp)
	  call pgmove(1.,amp(1))
	  do j=2,size
	    call pgdraw(float(j),amp(j))
	  end do
c		  
c  Phase plots are scaled to -180/180 and drawn as dots.
c
	  call pgswin(0.,float(size),-180.,180.)
	  do j=1,size
	    call pgpt(1,float(j),phase(j),1)
	  end do
	  xflag = 'BSCT'
	  if(i.eq.numchan) xflag(5:5)='N'
	  call pgbox(xflag,0.,0,'BSCTN',0.,0)
	  if(i.eq.1) call pglab(' ',' ','Fourier Transform of uv-data')
	  if(i.eq.numchan) call pglab('sample',' ',' ')  
	end do
	call pgend
	end
c********1*********2*********3*********4*********5*********6*********7*c
