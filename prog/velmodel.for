c**********************************************************************c
	program velmodel
	implicit none
c
c= VELMODEL - Make a model image for a theoretical velocity pattern.
c& mchw
c: image analysis
c+
c	VELMODEL generates a MIRIAD image for a theoretical velocity
c	pattern specified by model input paramaters. The default option
c	is to make an image of the line-of-sight velocity corresponding
c	to a rotating disk model:
c	  velocity = vsys +
c		(vrot*cos(theta-pa)+vrad*sin(theta-pa))*sin(incline)
c	The output image can be combined with an intensity
c	image to make a 3D model using the task VELIMAGE.
c@ in
c	Input image name used as template for the output image.
c@ region
c	Region of image to be used. The default region is the entire image.
c	See documentation of region for details. All pixels within the
c	bounding box are used; pixel masking is ignored.
c@ center
c	The rotation center in arcsec from the center pixel, measured
c	in the directions of RA and DEC. Default=0,0.
c@ pa
c	Position angle of the major axis in degrees. Default is 0. (north).
c@ incline
c	Inclination angle of the disk in degrees. Default is 0. (face on).
c@ radius
c	Sample points along major axis in arcsecs. Need not be evenly spaced.
c	At least one point must be given. 
c@ vrot
c	Rotation curve in the plane of the disk specified by a power law:
c		rotation velocity = vrot(1)*(r/radius(1))**vrot(2)
c	At least one point must be given. For options=sample, vrot gives the
c	rotation velocity sampled at the radius points above, and is
c	interpolated between the sample points, with zero velocity at zero
c	radius, and not extrapolated beyond the last point. Units are km/s.
c@ vrad
c	Radial velocity in plane of disk specified by a power law:
c		radial velocity = vrad(1)*(r/radius(1))**vrad(2)
c	Default=0,0. If only one value given then vrad = vrad(1)
c	Units are km/s.
c@ vsys
c	Systemic velocity of the disk. Units are km/s. Default=0.
c	For options=rms, vsys is a constant rms along the line of sight.
c@ z
c	Thickness of disk, and power law: z' = z(1)*(r/radius(1))**z(2)
c	Default=0,0. If only one value given then z' = z(1)
c	The units of z(1) are the same as arcsec along major axis of disk.
c@ out
c	The output image. No default.
c@ options
c	rms	The output image is the rms calculated from vsys and
c		the rms velocity along the line of sight, computed by
c		integrating through the disk thickness given by z.
c	sample	The rotation velocity is specified by values vrot
c		sampled at the radius values. Default is power law.
c--
c  History:
c    mchw  19sep92  New task.
c    nebk  26nov92  Add btype; change units of btype from VELO_LSR 
c                   to KM/S.  Replace "print" statements with "write"
c    mchw  17apr94  Added power laws and fixed a bug at zero radius.
c    mchw  21apr94  Added options=rms.
c    mchw  27apr94  Cleaned up user inputs. Added options=sample.
c    mchw  06may94  Use quadratic sum of vsys and integral for rms.
c----------------------------------------------------------------------c
	include 'maxdim.h'
	character*(*) version
	parameter (version='Version 1.0 06-May-94')
	double precision pi,rts
	parameter(pi=3.141592654,rts=3600.d0*180.d0/pi)
	integer maxnax,maxboxes,maxruns,naxis
	parameter(maxnax=3,maxboxes=2048)
	parameter(maxruns=3*maxdim)
	integer boxes(maxboxes)
	integer i,j,ir,lin,lout,nsize(maxnax),blc(maxnax),trc(maxnax)
	integer nrad,nrot,nr,nstep
	real crpix(maxnax),cdelt(maxnax),vr(maxdim),radius(maxdim)
	real center(2),pa,incline,rstep,vrot(maxdim),vrad(2),vz,r,r1,r2
	real v1,v2,vsys,z0,zexp,vel
	real buf(maxdim),cospa,sinpa,cosi,sini,x,y,xt,yt
	character in*64,out*64,cin*1,ctype*9
	logical dorms,dosamp
c
c  Externals.
c
	character*1 itoaf
	logical keyprsnt
c
c Get inputs.
c
	call output( 'VELMODEL: '//version)
	call keyini
	call keya('in',in,' ')
	call BoxInput('region',in,boxes,maxboxes)
	call keyr('center',center(1),0.)
	call keyr('center',center(2),0.)
	call keyr('pa',pa,0.)
	call keyr('incline',incline,0.)
	nrad = 0
	do while(keyprsnt('radius'))
	  nrad = nrad+1
	call keyr('radius',radius(nrad),0.)
	enddo
	nrot = 0 
	do while(keyprsnt('vrot'))
	  nrot = nrot+1
	  call keyr('vrot',vrot(nrot),0.)
	enddo
	call keyr('vrad',vrad(1),0.)
	call keyr('vrad',vrad(2),0.)
	call keyr('vz',vz,0.)
	call keyr('vsys',vsys,0.)
	call keyr('z',z0,0.)
	call keyr('z',zexp,0.)
	call keya('out',out,' ')
	call getopt(dorms,dosamp)
	call keyfin
c
c  Check inputs.
c
	if(in .eq. ' ') call bug('f','No input image specified.')
	if(out .eq. ' ') call bug('f','No output image specified.')
	if(.not.dosamp .and. nrad.gt.2)then
	  call bug('w','Using power law for vrot')
	else if(nrot.ne.nrad.and.dosamp)then
	  write(*,*) 'nrad=',nrad
	  write(*,*) 'nrot=',nrot
	  call bug('f','Rotation curve sample points mismatch')
	endif
	call xyopen(lin,in,'old',maxnax,nsize)
	call rdhdi(lin,'naxis',naxis,0)
	naxis = min(naxis,maxnax)
	if(nsize(1).gt.maxdim)call bug('f','Input file too big for me')
c
c  Set up the region of interest.
c
	call BoxSet(boxes,maxnax,nsize,'s')
	call BoxInfo(boxes,maxnax,blc,trc)
c
c  Get center and pixel size from image.
c
	do i=1,naxis
	  cin = itoaf(i)
	  call rdhda(lIn,'ctype'//cin,ctype,' ')
	  call rdhdr(lIn,'crpix'//cin,crpix(i),0.)
	  call rdhdr(lIn,'cdelt'//cin,cdelt(i),0.)
	  if(i.le.2)then
	    if(ctype(1:2).ne.'RA'.and.ctype(1:3).ne.'DEC')
     *	    call bug('w','Axes 1 and 2 are not RA or DEC')
	    if(crpix(i).eq.0)then
	      crpix(i) = nsize(i)/2+1
	      call bug('w','Center pixel missing - assume naxis/2+1')
	    endif
	    if(cdelt(i).eq.0)call bug('f','Pixel size missing')
	  endif
	enddo
c
c  Open the output image and write it's header.
c
	nsize(1) = trc(1)-blc(1)+1
	nsize(2) = trc(2)-blc(2)+1
	call xyopen(lout,out,'new',2,nsize)
	call headcopy(lin,lout,0,2,blc,trc)
	call wrhda(lout,'bunit','KM/S')
        call wrbtype(lout,'velocity')
c
c  Convert the inputs to more useful numbers, and defaults.
c
	cdelt(1) = cdelt(1)*rts
	cdelt(2) = cdelt(2)*rts
	cospa = cos(pa*pi/180.)
	sinpa = sin(pa*pi/180.)
	cosi = cos(incline*pi/180.)
	sini = sin(incline*pi/180.)
	rstep = abs(cdelt(1))/3.
c
c  Interpolate the rotation curve if not power law.
c
	if(dosamp)then
	  nstep = radius(nrad)/rstep
	  nr = 1
	  r1 = 0.
  	  r2 = radius(1)
	  v1 = 0.
	  v2 = vrot(1)
	  do ir=1,nstep
	    r = ir*rstep
	    do while(r.gt.r2.and.r.lt.radius(nrad))
	      nr = nr+1
	      r1 = radius(nr-1)
	      r2 = radius(nr)
	      v1 = vrot(nr-1)
	      v2 = vrot(nr)
	    enddo
	    vr(ir) = v1 + (r-r1)/(r2-r1)*(v2-v1)
	  enddo
	endif
c
c  Generate the velocity model.
c
	do j = blc(2),trc(2)
	  y = (j-crpix(2))*cdelt(2) - center(2)
	  do i = blc(1),trc(1)
	    x  = (i-crpix(1))*cdelt(1) - center(1)
	    yt =  x*sinpa + y*cospa
	    xt = -(x*cospa - y*sinpa)/cosi
	    if(dorms)then
	      call velrms(xt,yt,vrot,vrad,dosamp,vr,rstep,nstep,vel,
      +				radius,z0,zexp,sini)
	      buf(i) = sqrt(vsys**2 + (vel*sini)**2)
	    else
	      call veloc(xt,yt,vrot,vrad,dosamp,vr,rstep,nstep,vel,
      +				radius)
	      buf(i) = vsys + vel*sini
	    endif
	  enddo
	  call xywrite(lout,j-blc(2)+1,buf(blc(1)))
	enddo
c
c  Update history and close files.
c
	call Hisopen(lout,'append')
	call HisWrite(lout,'VELMODEL: '//version)
	call HisInput(lout,'VELMODEL')
	call HisClose(lout)
	call xyclose(lin)
	call xyclose(lout)
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine veloc(xt,yt,vrot,vrad,dosamp,vr,rstep,nstep,vel,
      +				radius)
	implicit none
	integer nstep
	real xt,yt,vrot(1),vrad(2),vr(nstep),rstep,vel,radius(1)
	logical dosamp
c
c compute line of sight velocity.
c
	real r,cost,sint,vrotn,vradial
	integer ir

	r = sqrt(xt*xt+yt*yt)
	if(r.ne.0.)then
	  cost = yt/r
	  sint = xt/r
	  if(dosamp)then
	    ir = nint(r/rstep)
	    if(ir.gt.0.and.ir.lt.nstep)then
	      vrotn = vr(ir)
	    else
	      vrotn = 0.
	    endif
	  else
	    vrotn = vrot(1)*(r/radius(1))**vrot(2)
	  endif
	  if(vrad(2).ne.0)then
	    vradial = vrad(1)*(r/radius(1))**vrad(2)
	  else 
	    vradial = vrad(1)
	  endif
	  vel = vrotn*cost+vradial*sint
	else
	  vel = 0.
	endif
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine velrms(xt,yt,vrot,vrad,dosamp,vr,rstep,nstep,vel,
      +				radius,z0,zexp,sini)
c
c compute rms velocity along line of sight.
c
	implicit none
	integer nstep
	real xt,yt,vrot(1),vrad(2),vr(nstep),rstep,vel,radius(1)
	real z0,zexp,sini
	logical dosamp
c----------------------------------------------------------------------
	real x,y,z,dz,r,rms,pts

	dz = -rstep
	rms = 0.
	pts = 0.
	do while(dz.le.rstep)
	  x = xt
	  y = yt
	  z = 0.
	  r = sqrt(x*x+y*y)
	  do while(abs(z).lt.z0*(r/radius(1))**zexp)
	    call veloc(x,y,vrot,vrad,dosamp,vr,rstep,nstep,vel,
      +				radius)
	    rms = rms + vel*vel
	    pts = pts + 1.
	    z = z + dz
	    x = x + dz*sini
	    r = sqrt(x*x+y*y)
	  enddo
	  dz = dz + 2*rstep
	enddo
	if(pts.ne.0.)then
	  vel = sqrt(rms)/pts
	else
	  vel = 0.
	endif
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine getopt(dorms,dosamp)
	implicit none
	logical dorms,dosamp
c
c     Decode options array into named variables.
c
c   Output:
c     dorms     Compute rms velocity from integral through disk.
c     dosamp	The rotation velocity is specified by values vrot
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 2)
      character ops(maxopt)*6
      logical present(maxopt)
      data ops /'rms','sample'/
c
      call options('options', ops, present, maxopt)
      dorms = present(1)
      dosamp = present(2)
      end
c********1*********2*********3*********4*********5*********6*********7*c
