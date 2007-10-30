c************************************************************************
	program velsw
	implicit none
c= velsw -- Change the `velocity' axis of an image.
c& rjs
c: map manipulation
c+
c	VELSW is a Miriad task which changes the units on the `velocity'
c	axis. As velocity and frequency are related by a Doppler shift
c	formula, it is possible to switch an axis between being labelled
c	in frequency or velocity. VELSW can switch the axis description
c	between frequency (labelled FREQ-) and velocity, using either
c	the `radio convention' (labelled VELO-) or `optical convention'
c	(labelled FELO-) for the formula relating velocity 
c	and frequency.
c
c	Assuming that the data were measured with equal frequency
c	increments, some approximation is involved assigning a velocity
c	axis increment for the optical convention. In this case,
c	the increment stored is correct at the reference pixel.
c@ in
c	The name of the input image data set. No default.
c@ axis
c	This determines what the labelling on the `velocity' axis will
c	be changed to. Possible values are "frequency", "optical" (for
c	velocity using the optical convention) or "radio" (for velocity
c	using the radio convention). The default is "frequency" if the
c	`velocity' axis is given as a velocity, and "radio" otherwise.
c--
c  History:
c    rjs  31aug93 Original version.
c    nebk 07oct93 Doc change
c    nebk 01jul94 Replace guts by stripped out subroutine spaxsw
c
c  Bugs:
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	character version*(*)
	parameter(version='VelSw: version 1.0 01-Jul-94')
c
	character in*64,aname*1,num*2,ctype*16
	integer nr,nout,nsize(MAXNAX),lIn
	double precision crval,cdelt
c
	integer nswitch
	parameter(nswitch=3)
	character switches(nswitch)*9,switch*9
c
c  Externals.
c
	character itoaf*2
c
	data switches/'frequency','optical  ','radio    '/
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('in',in,' ')
	call keymatch('axis',nswitch,switches,1,switch,nout)
	call keyfin
c
c  Check the inputs.
c
	if(in.eq.' ')call bug('f','An input must be given')
c
c  Open the input, and find the velocity axis.
c
	call xyopen(lIn,in,'old',MAXNAX,nsize)
	aname = ' '
	nr = 0
	call fndaxnum(lIn,'freq',aname,nr)
	if(nr.eq.0)call bug('f','Could not find velocity axis')
	num = itoaf(nr)
c
c  Get the values that we need.
c
	call rdhda(lIn,'ctype'//num,ctype,' ')
	call rdhdd(lIn,'cdelt'//num,cdelt,0.d0)
	call rdhdd(lIn,'crval'//num,crval,0.d0)
c
c  Perform the transformation
c
	call spaxsw(lIn,switch,ctype,cdelt,crval)
c
c  Write out the new information.
c
	call wrhda(lIn,'ctype'//num,ctype)
	call wrhdd(lIn,'cdelt'//num,cdelt)
	call wrhdd(lIn,'crval'//num,crval)
c
c  Write out some history.
c
	call hisopen(lIn,'append')
	call hiswrite(lIn,'VELSW: Miriad '//version)
	call hisinput(lIn,'VELSW')
	call hisclose(lIn)
c
c  All said and done. Close up.
c
	call xyclose(lIn)
c	
	end
