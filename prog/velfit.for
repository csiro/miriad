c**********************************************************************c
	program velfit
	implicit none
c
c= VELFIT - Fit a theoretical velocity pattern to isovelocity image.
c& mchw
c: image analysis
c+
c	VELFIT fits a theoretical velocity pattern to a MIRIAD
c	isovelocity image, weighted by the intensity image and a
c	geometric factor specified by model input paramaters.
c	The default option is to fit a rotation curve to an isovelocity
c	image of a rotating disk. The rotation curve and rms for the
c	fit are printed out and can be used to find the best fit to the
c	other parameters. For more details see paper by Warner, Wright
c	and Baldwin, 1973, MNRAS, 163,163.
c@ in
c       The input image names, separated by commas. The first image is the
c	(x,y) intensity distribution integrated over the z-axis. No default.
c       e.g. velocity-integrated image. The second image is an (x,y) model
c	for the z-values, e.g. a mean velocity image. No default.
c@ region
c	Region of image to be used in the fit. See documentation on region
c	for details. Only the bounding box is supported.
c	The default region is the entire image.
c@ center
c	The center of the annuli in arcsec from the center pixel, measured
c	in the directions of RA and DEC.
c@ pa
c	Position angle of ellipse major axis in degrees. Default is 0 (north).
c@ incline
c	Inclination angle in degrees. Default=0. (face on)
c@ radius
c	Inner and outer radii and step size along major axis in arcsecs.
c	The default is the whole image in steps equal to the pixel size.
c@ vsys
c	Center value z-axis. E.g. systemic velocity for rotation curve.
c@ log
c	The output log file. The default is the terminal.
c@ options
c	None yet. Reserved for alternative models.
c--
c  History:
c    30sep92  mchw  New task for Miriad.
c----------------------------------------------------------------------c
	include 'maxdim.h'
	character*(*) label,version
	parameter(version='version 1.0 30-Sep-92')
	double precision pi,rts
	parameter(pi=3.141592654,rts=3600.d0*180.d0/pi)
	parameter(label='Fit a rotation curve to elliptical annuli')
	integer maxnax,maxboxes,maxruns,naxis
	parameter(maxnax=3,maxboxes=2048,naxis=3)
	parameter(maxruns=3*maxdim)
	integer boxes(maxboxes)
	integer nsize(maxnax),size(maxnax),blc(maxnax),trc(maxnax)
	integer irmin,irmax,nmap,map,i,j,ir,lin(2)
	real crpix(maxnax),cdelt(maxnax),crval(maxnax),crpix1,cdelt1
	real center(2),pa,incline,rmin,rmax,rstep,vsys,crval1,vr,tmp
	real cospa,sinpa,cosi,sini,cost,wt,x,y,xt,yt,r,ave,rms,fsum
	real pixe(maxdim),amp(maxdim),flux(maxdim),vel(maxdim)
	real vsum(maxdim),vsqu(maxdim),wsum(maxdim),vrot(maxdim)
        logical mask(maxdim)
	character*80 in(2),log,line
	character cin*1,ctype*9
c
c  Externals.
c
	integer len1
	character*1 itoaf
c
c Get inputs.
c
	call output( 'VELFIT: '//version )
	call keyini
	call mkeyf('in',in,2,nmap)
	call BoxInput('region',in,boxes,maxboxes)
	call keyr('center',center(1),0.)
	call keyr('center',center(2),0.)
	call keyr('pa',pa,0.)
	call keyr('incline',incline,0.)
	call keyr('radius',rmin,0.)
	call keyr('radius',rmax,0.)
	call keyr('radius',rstep,0.)
	call keyr('vsys',vsys,0.)
	call keya('log',log,' ')
	call keyfin
c
c  Check inputs.
c
	if(nmap.lt.2) call bug('f','Must have two input maps')
c
c  Get center and pixel sizes from first image.
c
	call xyopen(lin(1),in(1),'old',naxis,size)
	if(size(1).gt.maxdim.or.size(2).gt.maxdim)
     *			call bug('f','Image too big for MAXDIM')
	do i=1,2
	  cin = itoaf(i)
	  call rdhda(lIn(1),'ctype'//cin,ctype,' ')
	  if(ctype(1:2).ne.'RA'.and.ctype(1:3).ne.'DEC')
     *	    call bug('w','Axes 1 and 2 are not RA or DEC')
	  call rdhdr(lin(1),'crpix'//cin,crpix(i),0.)
	  if(crpix(i).eq.0)then
	    crpix(i) = nsize(i)/2+1
	    call bug('w','Center pixel missing - assume naxis/2+1')
	  endif
	  call rdhdr(lin(1),'cdelt'//cin,cdelt(i),1.0)
	  if(cdelt(i).eq.0)call bug('f','Pixel size missing')
	  call rdhdr(lin(1),'crval'//cin,crval(i),0.)
	enddo
c
c  Open the second input map and check axis match.
c
	map = 2
	  call xyopen(lin(map),in(map),'old',naxis,nsize)
	  if(nsize(1).ne.size(1).or.nsize(2).ne.size(2))
     *	  call bug('f','Each map must have same xy dimensions')
	  do i=1,2
	    cin = itoaf(i)
	    call rdhdr(lin(map),'cdelt'//cin,cdelt1,1.0)
	    if(cdelt1.ne.cdelt(i)) call bug('w','cdelt not the same')
	    call rdhdr(lin(map),'crpix'//cin,crpix1,0.)
	    call rdhdr(lin(map),'crval'//cin,crval1,0.)
	    if((crval1+(1-crpix1)*cdelt1).ne.
     *		(crval(i)+(1-crpix(i))*cdelt(i)))
     *		   call bug('w','reference positions not the same')
	  enddo
c
c  Set up the region of interest.
c
	call BoxMask(lin(1),boxes,maxboxes)
	call BoxSet(boxes,maxnax,nsize,'s')
	call BoxInfo(boxes,maxnax,blc,trc)
c
c  Open the output text file and write title.
c
	call LogOpen(log,'q')
	call LogWrit(' ***** '//label//' *****')
	call LogWrit(' Intensity Image = '//In(1)(1:len1(in(1))))
	call LogWrit(' Velocity  Image = '//In(2)(1:len1(in(2))))
	write(line,'(a,2f7.1,a,f5.0,a,f4.0,a,f8.0)')
     *	  '  center: ',center,'  Ellipse pa: ',pa,
     *		'  inclination: ',incline,' Vsys: ',vsys
	call LogWrit(line(1:len1(line)))
c
c  Convert the inputs to more useful numbers, and defaults.
c
	cdelt(1) = cdelt(1)*rts
	cdelt(2) = cdelt(2)*rts
	if(rmin.eq.0.) rmin = abs(0.1*cdelt(1))
	if(rmax.eq.0.) rmax = abs(nsize(1)*cdelt(1))
	if(rstep.eq.0.) rstep = abs(cdelt(1))
	cospa = cos(pa*pi/180.)
	sinpa = sin(pa*pi/180.)
	sini = sin(incline*pi/180.)
	cosi = cos(incline*pi/180.)
c
c  Initialize integrals for each axis.
c
	do ir = 1,maxdim
	  pixe(ir) = 0.
	  flux(ir) = 0.
	  vsum(ir) = 0.
	  vsqu(ir) = 0.
	  wsum(ir) = 0.
	enddo
	irmin = rmax/rstep
	irmax = 0
c
c  Integrate in elliptical annuli.
c
	do j = blc(2),trc(2)
	  call xyread(lin(1),j,amp)
	  call xyread(lin(2),j,vel)
	  call xyflgrd(lin(1),j,mask)
	  y = (j-crpix(2))*cdelt(2) - center(2)
	  do i = blc(1),trc(1)
	    x = (i-crpix(1))*cdelt(1) - center(1)
	    yt =  x*sinpa + y*cospa
	    xt = (x*cospa - y*sinpa)/cosi
	    r  = sqrt(xt*xt+yt*yt)
	    if(r.ge.rmin .and. r.le.rmax .and. mask(i)) then
	      ir = r/rstep+1.5
	      cost = yt/r
	      vr = (vel(i)-vsys)/cost/sini
	      wt = amp(i)*abs(cost)
	      pixe(ir) = pixe(ir) + 1.
	      flux(ir) = flux(ir) + amp(i)
	      vsum(ir) = vsum(ir) + wt*vr
	      vsqu(ir) = vsqu(ir) + wt*vr*vr
	      wsum(ir) = wsum(ir) + wt
	      irmin = min(ir,irmin)
	      irmax = max(ir,irmax)
	    endif
	  enddo
	enddo
c
c  Find the rotation curve.
c
	do ir = irmin,irmax
	  if(wsum(ir).ne.0.) then
	    vrot(ir) = vsum(ir)/wsum(ir)
	  else
	    vrot(ir) = 0.
	  endif
	enddo
c
c  Now find the rms residuals from the fitted rotation curve.
c
	do j = blc(2),trc(2)
	  call xyread(lin(1),j,amp)
	  call xyread(lin(2),j,vel)
	  call xyflgrd(lin(1),j,mask)
	  y = (j-crpix(2))*cdelt(2) - center(2)
	  do i = blc(1),trc(1)
	    x = (i-crpix(1))*cdelt(1) - center(1)
	    yt =  x*sinpa + y*cospa
	    xt = (x*cospa - y*sinpa)/cosi
	    r  = sqrt(xt*xt+yt*yt)
	    if(r.ge.rmin .and. r.le.rmax .and. mask(i)) then
	      ir = r/rstep+1.5
	      cost = yt/r
	      vr = vel(i)-vsys-vrot(ir)*cost*sini
	      wt = amp(i)*abs(cost)
	      vsum(ir) = vsum(ir) + wt*vr
	      vsqu(ir) = vsqu(ir) + wt*vr*vr
	      wsum(ir) = wsum(ir) + wt
	    endif
	  enddo
	enddo
c
c  Write out the results.
c
	  call LogWrit(' ')
	  write(line,'(a,a,a,a,a,a)') '  Radius(") ',' # Pixels   ',
     *	     '  intensity ','     fit    ','     rms    ','  total rms '
	  call LogWrit(line(1:72))
c
c  Find averages for each annulus.
c
	fsum = 0.
	do ir = irmin,irmax
	  r = (ir-1)*rstep
	  if(wsum(ir).ne.0.) then
	    tmp = flux(ir)/pixe(ir)
	    ave = vsum(ir)/wsum(ir)
	    rms = sqrt(vsqu(ir)/wsum(ir)-ave*ave)
	  else
	    tmp = 0.
	    rms = 0.
	  endif
	  fsum = fsum + rms*rms
	  write(line,'(6f12.4)')
     *		 r,pixe(ir),tmp,vrot(ir),rms,sqrt(fsum/(ir-irmin+1.))
	  call LogWrit(line(1:72))
	enddo
c
c  All done.
c
	call xyclose(lIn(1))
	call xyclose(lIn(2))
	call LogClose
	end
c********1*********2*********3*********4*********5*********6*********7*c
