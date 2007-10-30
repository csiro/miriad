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
c	using the radio convention). There is no default.
c
c	Optionally you can give a second value to define the  rest frame.
c	This can be "barycentre" (solar system barycentre), "lsr" (Local
c	Standard of Rest) or "observatory" (topocentric). The default is not
c	to change this.
c--
c  History:
c    rjs  31aug93 Original version.
c    nebk 07oct93 Doc change
c    nebk 01jul94 Replace guts by stripped out subroutine spaxsw
c    nebk 20aug94 Scrap SPAXSW for new cocvt routines
c    rjs  07jul97 Allow changing of rest frame, and changed call sequence
c		  to covelset.
c
c  Bugs:
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	character version*(*)
	parameter(version='VelSw: 07-Jul-97')
c
	character in*64
	integer nsize(MAXNAX),lIn
c
	integer nswitch,nframe
	parameter(nswitch=3,nframe=3)
	character switches(nswitch)*12,switch*9,vtypes(nswitch)*4
	character frames(nframe)*12,frame*12,ftypes(nframe)*3
	character ctype*8
	integer ns,nf,is,if
c
c  Externals.
c
	integer binsrcha
c
	data switches/'frequency   ','optical     ','radio       '/
	data vtypes  /'FREQ',        'FELO',        'VELO'        /
	data frames  /'barycentre  ','lsr         ','observatory '/
	data ftypes  /'HEL',         'LSR',         'OBS'         /
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('in',in,' ')
	call keymatch('axis',nswitch,switches,1,switch,ns)
	if(ns.eq.0)call bug('f','An axis type for the '//
     *					'output must be given')
	call keymatch('axis',nframe, frames,1,frame,nf)
	call keyfin
c
c  Check the inputs.
c
	if(in.eq.' ')call bug('f','An input must be given')
c
c  Open the input, and find the velocity axis.
c
	call xyopen(lIn,in,'old',MAXNAX,nsize)
c
	is = binsrcha(switch,switches,nswitch)
	if(nf.eq.1)then
	  if = binsrcha(frame,frames,nframe)
	  ctype = vtypes(is)//'-'//ftypes(if)
	else
	  ctype = vtypes(is)
	endif
c
c  Perform the transformation
c
        call coinit(lIn)
        call covelset(lIn,ctype)
        call cowrite(lIn,lIn)
        call cofin(lIn)
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
