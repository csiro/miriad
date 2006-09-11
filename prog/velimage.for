	program velimage
	implicit none
c
c= VELIMAGE - Make (x,y,z) image from an (x,y) image and model z-values.
c& mchw
c: image analysis
c+
c       VELIMAGE is a MIRIAD task to make an (x,y,z) image from an (x,y)
c	image and a model for the z-values and dispersion. The z-values,
c	for example the mean velocity, are input as an (x,y) image, whilst
c	the dispersion can be input as either an (x,y) image or a fixed
c	value. The output image is formed as:-
c
c	   out(x,y,z) = in1(x,y) * exp(-(z-in2(x,y))**2/(2*sigma**2)))
c
c	VELIMAGE can be used to compare models with the data. For example
c	one can generate a model image corresponding to a rotating galaxy,
c	convolve it with the actual beam and subtract the model from the
c	data to determine if there is residual emission which deviates from
c	the model rotation curve.
c@ in
c       The input image names, separated by commas. The first image is the
c	(x,y) intensity distribution integrated over the z-axis. No default.
c       e.g. velocity-integrated image. The second image is an (x,y) model
c	for the z-values, e.g. a mean velocity image. No default.
c       A 3rd input image gives the z-dispersion at each point. e.g. a
c	velocity dispersion image. If the 3rd image is not specified then
c	fixed dispersion, sigma must be specified. 
c@ region
c	Region of image to be used. The default region is the entire image.
c	See documentation on region for details. All pixels within the
c	bounding box are used; pixel masking is not used.
c@ sigma
c	Fixed value for z-dispersion if not specified by 3rd input image.
c@ nchan
c	Number of channels for z-axis of output image. Default=1.
c@ start
c	Starting value for z-axis of output image. No default.
c@ step
c	Interval for z-axis of output image. No default.
c@ out
c       The output (x,y,z) image. No default.
c@ options
c	Options. Minimum match is active.
c	  relax  ignore axis descriptor mismatches
c	         (e.g. pixel increments etc).  Use with care.
c--
c  History:
c    23sep92 mchw  New task.
c-----------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)	
        parameter(version='VELIMAGE: version 30-Nov-92')
	integer maxboxes,maxruns,naxis
	parameter(maxboxes=2048,maxruns=3*maxdim,naxis=4)
	integer boxes(maxboxes)
	character*80 in(3),out
	integer i,j,k,nchan,map,nmap,lout,lin(3)
	integer nsize(naxis),size(naxis),blc(naxis),trc(naxis)
	real amp(maxdim),val(maxdim),sig(maxdim),buf(maxdim)
	real cdelt(naxis),crval(naxis),crpix(naxis)
	real cdelt1,crval1,crpix1,v,sigma,start,step
	logical relax
	character*1 caxis,wflag
c
c  Externals.
c
	character*1 itoaf
c
c  Get the input parameters.
c
        call output(version)
	call keyini
	call mkeyf('in',in,3,nmap)
	call keyr('sigma',sigma,0.)
	call keyi('nchan',nchan,1)
	call keyr('start',start,0.)
	call keyr('step',step,0.)
	call keya('out',out,' ')
        call getopt(relax)
	call keyfin
c
c  Check the inputs.
c
	if(nmap.lt.2) call bug('f','Must have at least two input maps')
	if(out.eq.' ') call bug('f','output image file not given')
	if(nmap.eq.2.and.sigma.eq.0.) call bug('f',
     *	    'Either z-dispersion image or sigma must be specified')
	if(start.eq.0.) call bug('f','No start given for z-axis')
	if(step.eq.0.) call bug('f','No step given for z-axis')
        if(relax) then
          wflag = 'w'
          call bug ('i','Axis descriptor mismatches will be tolerated')
	else
	  wflag = 'f'
        endif
c
c  Open the input maps and check sizes, crpix and cdelt.
c
	call xyopen(lin(1),in(1),'old',naxis,size)
	if(size(1).gt.maxdim.or.size(2).gt.maxdim)
     *			call bug('f','Image too big for MAXDIM')
	do i=1,naxis
	  caxis = itoaf(i)
	  call rdhdr(lin(1),'cdelt'//caxis,cdelt(i),1.0)
	  call rdhdr(lin(1),'crpix'//caxis,crpix(i),0.)
	  call rdhdr(lin(1),'crval'//caxis,crval(i),0.)
	enddo
c
	map = 2
	do while(map.le.nmap)
	  call xyopen(lin(map),in(map),'old',naxis,nsize)
	  if(nsize(1).ne.size(1).or.nsize(2).ne.size(2))
     *	  call bug('f','Each map must have same xy dimensions')
	  do i=1,naxis
	    caxis = itoaf(i)
	    call rdhdr(lin(map),'cdelt'//caxis,cdelt1,1.0)
	    if(cdelt1.ne.cdelt(i)) call bug(wflag,'cdelt not the same')
	    call rdhdr(lin(map),'crpix'//caxis,crpix1,0.)
	    call rdhdr(lin(map),'crval'//caxis,crval1,0.)
	    if((crval1+(1-crpix1)*cdelt1).ne.
     *		(crval(i)+(1-crpix(i))*cdelt(i)))
     *		   call bug(wflag,'reference positions not the same')
	  enddo
	  map = map + 1
	enddo
c
c  Set up the region of interest.
c
	call BoxSet(boxes,naxis,nsize,'s')
	call BoxInfo(boxes,naxis,blc,trc)
c
c  Open the output image and write it's header.
c
	nsize(1) = trc(1)-blc(1)+1
	nsize(2) = trc(2)-blc(2)+1
	nsize(3) = nchan
	call xyopen(lout,out,'new',3,nsize)
	call headcopy(lin,lout,0,2,blc,trc)
	call wrhda(lout,'ctype3','VELO-LSR')
	call wrhda(lout,'bunit','KM/S')
	call wrhdr(lout,'crpix3',1.)
	call wrhdr(lout,'crval3',start)
	call wrhdr(lout,'cdelt3',step)
c
c  Generate the output image.
c
	do k=1,nchan
	  v = start+(k-1)*step
	  call xysetpl(lout,1,k)
	  do j = blc(2),trc(2)
	    call xyread(lin(1),j,amp)
	    call xyread(lin(2),j,val)
	    if(nmap.eq.3) call xyread(lin(3),j,sig)
	    do i = blc(1),trc(1)
	      if(nmap.eq.3) sigma=sig(i)
	      if(abs(v-val(i)).lt.5.*sigma)then
		buf(i) = amp(i) * exp( -(v-val(i))**2 / (2.*sigma**2) )
	      else
		buf(i) = 0.
	      endif
	    enddo
	    call xywrite(lout,j-blc(2)+1,buf(blc(1)))
	  enddo
	enddo
c
c  Update history and close files.
c
	call Hisopen(lout,'append')
	call HisWrite(lout,version)
	call HisInput(lout,'VELIMAGE')
	call HisClose(lout)
	call xyclose(lout)
	do i=1,nmap
	  call xyclose(lin(i))
	enddo

	end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine getopt(relax)
      implicit none
      logical relax
c
c     Decode options array into named variables.
c
c   Output:
c     relax     If true issue warnings about mismatched axis
c               descriptors between images instead of fatal error
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 1)
      character ops(maxopt)*5
      logical present(maxopt)
      data ops /'relax'/
c
      call options ('options', ops, present, maxopt)
      relax = present(1)
      end
c********1*********2*********3*********4*********5*********6*********7*c
