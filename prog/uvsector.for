c************************************************************************
	program uvsector
c
c= uvsector - Flag a sector of UV data.
c& rjs
c: calibration
c+
c	UVSECTOR flags visibility data with a specific sector.
c	The sector can be specified either by position angle, or as
c	an hour angle (East-West array assumed)	or by way of a stripe
c	direction in an image. The latter form
c	will be useful when a stripe is seen in an image (implying
c	bad visibility data), and you wish to flag out the visibility
c	data that could have produced this stripe.
c@ vis
c	The visibility file to flag. Several files can be given.
c	Wildcard expansion is supported. The default is no file.
c@ select
c	Extra selection criteria. In addition to being in the appropriate
c	sector, the data must satisfy the selection criteria before
c	it is flagged.
c@ angle
c	This defines the angle of the axis of the sector, if the sector is
c	given by a position angle or hour angle (for an EW array). It
c	consists of two values, the first being either the string "uvangle"
c	or "hangle" (for position angle and hour angle, respectively). The
c	second gives the angle in degrees or hours (for position angle
c	and hour angle respectively). Position angle is defined as degrees
c	clockwise from the v axis (which is consistent with the uvplt
c	definition).
c@ in
c	If the sectors position angle is given by a stripe direction,
c	this gives the name of the image to associate with the stripe.
c@ region
c	A long, thin, region of the image given above, running in the
c	direction of a stripe. This parameter must be given if you
c	are defining an hour angle by a stripe direction. Normally
c	you will generate this region using CGCURS's region option,
c	and generate a region along the ridge of a stripe.
c@ width
c	The full width of the sector, in degrees. This must be in the
c	range (0,180). The default is 5 degrees.
c--
c  History
c    rjs  26mar93 Original version.
c    rjs   1dec94 Tell about the offending LST.
c---------------------------------------------------------------------------
	include 'mirconst.h'
	character version*(*)
	integer MAXSELS,MAXBOX,MAXIN
	parameter(version='UvSector: version 1.0 26-Mar-93')
	parameter(MAXSELS=1024,MAXBOX=1024,MAXIN=32)
c
	real sels(MAXSELS)
	integer boxes(MAXBOX)
	real uvpa,width
	integer nin,nout,i,tIn
	character vis(MAXIN)*64,in*64,mode*8
	logical ImPrsnt,RegPrsnt,AnPrsnt
c
c  Externals.
c
	logical keyprsnt
c
c  Ways of defining the angle.
c
	integer NANGLE
	parameter(NANGLE=2)
	character angles(NANGLE)*8
	data angles/'uvangle ','hangle  '/
c
c  Get the task parameters.
c
	call output(version)
	call keyini
	call mkeyf('vis',vis,MAXIN,nin)
	call SelInput('select',sels,MAXSELS)
	call keyr('width',width,5.0)
	if(width.le.0.or.width.ge.180)
     *	  call bug('f','The width must be in the range (0,180)')
c
	ImPrsnt  = keyprsnt('in')
	RegPrsnt = keyprsnt('region')
	AnPrsnt  = keyprsnt('angle')
	if(AnPrsnt)then
	  if(ImPrsnt.or.RegPrsnt)
     *	    call bug('f','Either ANGLE or IN and REGION must be set')
	  call keymatch('angle',NANGLE,angles,1,mode,nout)
	  if(nout.eq.0)call bug('f','Missing angle??')
	  call keyr('angle',uvpa,0.)
	else if(ImPrsnt.and.RegPrsnt)then
	  call keyf('in',in,' ')
	  call BoxInput('region',in,boxes,MAXBOX)
	  mode = 'image'
	else
	  call bug('f','Insufficient info to determine sector angle')
	endif
	call keyfin
c
c  Get an angle as an angle (measured counter-clockwise from u axis)
c  in the u-v plane. Convert the angle and width to radians.
c
	if(mode.eq.'image')then
	  call Getuvpa(in,boxes,MAXBOX,uvpa)
	else if(mode.eq.'uvangle')then
	  uvpa = pi/2 - pi/180*uvpa
	else if(mode.eq.'hangle')then
	  uvpa = -pi/12*uvpa
	else
	  call bug('f','Unrecognised angle??')
	endif
	width = pi/180 * width
c
c  Loop over the visibility data.
c
	do i=1,nin
	  call output('Processing '//vis(i))
	  call uvopen(tIn,vis(i),'old')
c
	  call hisopen(tIn,'append')
	  call hiswrite(tIn,'UVSECTOR: Miriad '//version)
	  call hisinput(tIn,'UVSECTOR')
	  call hisclose(tIn)
c
	  call SelApply(tIn,sels,.true.)
	  call Process(tIn,uvpa,width)
	  call uvclose(tIn)
	enddo
c
	end
c************************************************************************
	subroutine process(tIn,uvpa,width)
c
	implicit none
	integer tIn
	real uvpa,width
c
c  Do the actual flagging.
c
c  Input:
c    tIn
c    uvpa
c    width
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	double precision preamble(4)
	real cs,sn,a,uu,vv
	integer i,flagged,nchan
c
c  Externals.
c
	character itoaf*8
c
c  Get cosines and sines and things.
c
	cs = cos(uvpa)
	sn = sin(uvpa)
	a = tan(width/2)
c
	flagged = 0
	call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	dowhile(nchan.gt.0)
c
c  Rotate the (u,v) coordinates to the axis of the sector.
c
	  uu =  cs*preamble(1) + sn*preamble(2)
	  vv = -sn*preamble(1) + cs*preamble(2)
	  if(abs(vv).lt.abs(a*uu))then
	    do i=1,nchan
	      if(flags(i)) flagged = flagged + 1
	      flags(i) = .false.
	    enddo
	    call uvflgwr(tIn,flags)
	  endif
	  call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	enddo
	call output('Correlations flagged: '//itoaf(flagged))
	end
c************************************************************************
	subroutine Getuvpa(in,boxes,maxbox,uvpa)
c
	implicit none
	character in*(*)
	integer maxbox,boxes(maxbox)
	real uvpa
c
c  Determine a stripe direction, given an image an a region running
c  along the direction of the stripe.
c
c  Input:
c    in		The image of interest.
c    boxes	The boxes spec which destribe the region of interest.
c    maxbox	Dimension of the boxes array.
c  Output:
c    uvpa	The position angle of the sector to be deleted.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mirconst.h'
	integer MAXRUN
	parameter(MAXRUN=3*MAXDIM)
	integer run(3,MAXRUN),nrun,nsize(MAXNAX),tIn
	integer i,j,n,x,y,xmin,xmax,ymin,ymax
	integer ira,idec
	real SumX,SumY,SumXX,SumYY,SumXY,pnts
	double precision cdelt1,cdelt2,crval1
	character line*80,axisname*16
	real r,rx,ry,rxy,theta
c
c  Externals
c
	character hangle*13
c
c  Open the input file, and set up the boxes routine.
c
	call xyopen(tIn,in,'old',MAXNAX,nsize)
c
c  Determine the RA and DEC increments.
c
	ira = 0
	axisname = 'ra'
	call fndaxnum(tIn,'lon',axisname,ira)
	idec = 0
	axisname = 'dec'
	call fndaxnum(tIn,'lat',axisname,idec)
	if(ira.le.0.or.idec.le.0)
     *	  call bug('f','RA or DEC axis is missing')
	if(max(ira,idec).gt.2)
     *	  call bug('f','Input needs to be in XY order')
	call rdhdd(tIn,'crval1',crval1,0.d0)
	call rdhdd(tIn,'cdelt1',cdelt1,1.d0)
	call rdhdd(tIn,'cdelt2',cdelt2,1.d0)
c
c  Get the box spec.
c
	call BoxMask(tIn,boxes,maxbox)
	call BoxSet(boxes,MAXNAX,nsize,' ')
	do i=1,MAXNAX
	  nsize(i) = 1
	enddo
	call BoxRuns(MAXNAX-2,nsize,' ',boxes,Run,MAXRUN,nrun,
     *	  xmin,xmax,ymin,ymax)
	call xyclose(tIn)
c
c  Loop over all the selected pixels.
c
	SumX = 0
	SumY = 0
	SumXX = 0
	SumYY = 0
	SumXY = 0
	pnts = 0
	do j=1,nrun
	  y = Run(1,j)
	  x = Run(2,j) - 1
	  n = Run(3,j) - x
	  do i=1,n
	    x = x + 1
	    SumX = SumX + x
	    SumY = SumY + y
	    SumXY = SumXY + x*y
	    SumXX = SumXX + x*x
	    SumYY = SumYY + y*y
	    pnts = pnts + 1
	  enddo
	enddo
c
c  Determine the best fit slope and the correlation coefficient.
c
	if(pnts.le.1)
     *	  call bug('f','Insufficient data points in the region')
	ry = SumYY - SumY*SumY/pnts
	rx = SumXX - SumX*SumX/pnts
	rxy= SumXY - SumX*SumY/pnts
	if(rx.ne.0.and.ry.ne.0)then
	  r = abs(rxy) / sqrt(rx*ry)
	  if(r.lt.0.5)call bug('f','The region is not narrow enough')
	  if(r.lt.0.9)call bug('w','The region is not very narrow')
	endif
c
	uvpa = atan2(cdelt2*rxy, cdelt1*rx)
c
c  If RA and DEC were reverse, fiddle the uvpa
c
	if(idec.eq.1)uvpa = pi/2 - uvpa
c
c  Go to the Fourier plane, by rotating by 90 degrees.
c
	uvpa = uvpa + pi/2
c
c  Get the hour angle in the range [-6,6].
c
	uvpa = mod(uvpa,2*pi)
	if(uvpa.gt.pi)    uvpa = uvpa - 2*pi
	if(uvpa.lt.-pi)   uvpa = uvpa + 2*pi
	if(uvpa.gt.pi/2)  uvpa = uvpa - pi
	if(uvpa.lt.-pi/2) uvpa = uvpa + pi
c
c  Tell the user whats what.
c
	theta = pi/2 - uvpa
	if(theta.gt.pi/2) theta = theta - pi
	write(line,'(a,f6.1,a)')
     *	  'Region inclination corresponds to a uvangle of',
     *	  180/pi*theta,' degrees'
	call output(line)
	theta = -uvpa
	line = 'For an east-west array, this is an hour angle of '//
     *		hangle(dble(theta))
	call output(line)
	theta = theta - crval1
	theta = mod(theta,2*pi)
	if(theta.lt.0)theta = theta + 2*pi
	line = '                                      and LST of '//
     *		hangle(dble(theta))
	call output(line)
c
	end
