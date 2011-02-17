        program velsw

c= velsw -- Change the 'velocity' axis of an image.
c& rjs
c: map manipulation
c+
c       VELSW is a Miriad task that changes the units on the spectral
c       axis.  As velocity and frequency are related by a Doppler shift
c       formula, it is possible to switch an axis between being labelled
c       in frequency or velocity.  VELSW can switch the axis description
c       between frequency (labelled FREQ-) and velocity, using either
c       the 'radio convention' (labelled VELO-) or 'optical convention'
c       (labelled FELO-) for the formula relating velocity and
c       frequency.
c
c       Assuming that the data were measured with equal frequency
c       increments, some approximation is involved assigning a velocity
c       axis increment for the optical convention.  In this case, the
c       increment stored is correct at the reference pixel.
c
c@ in
c       Name of the input image data set.  No default.
c
c@ axis
c       This determines what the labelling on the 'velocity' axis will
c       be changed to.  Possible values are "frequency", "optical" (for
c       velocity using the optical convention), or "radio" (for velocity
c       using the radio convention).  No default.
c
c       Optionally you can give a second value to define the rest frame.
c       This can be "barycentre" (solar system barycentre), "lsr" (Local
c       Standard of Rest) or "observatory" (topocentric).  The default
c       is not to change this.
c
c$Id: velsw.for,v 1.2 2011/02/17 03:16:52 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   NFRAME, NSWITCH
      parameter (NFRAME=3, NSWITCH=3)

      integer   if, is, lIn, nf, ns, nsize(MAXNAX)
      character ctype*8, frame*12, frames(NFRAME)*12, ftypes(NFRAME)*3,
     *          in*64, switch*9, switches(NSWITCH)*12, version*72,
     *          vtypes(NSWITCH)*4

      external  binsrcha, versan
      integer   binsrcha
      character versan*72

      data switches/'frequency   ','optical     ','radio       '/
      data vtypes  /'FREQ',        'FELO',        'VELO'        /
      data frames  /'barycentre  ','lsr         ','observatory '/
      data ftypes  /'HEL',         'LSR',         'OBS'         /
c-----------------------------------------------------------------------
      version = versan('velsw',
     *                 '$Revision: 1.2 $',
     *                 '$Date: 2011/02/17 03:16:52 $')

c     Get input parameters.
      call keyini
      call keya('in',in,' ')
      call keymatch('axis',NSWITCH,switches,1,switch,ns)
      if (ns.eq.0) call bug('f',
     *  'An axis type for the output must be given')
      call keymatch('axis',NFRAME, frames,1,frame,nf)
      call keyfin

c     Check inputs.
      if (in.eq.' ') call bug('f','An input must be given')

c     Open the input, and find the velocity axis.
      call xyopen(lIn,in,'old',MAXNAX,nsize)

      is = binsrcha(switch,switches,NSWITCH)
      if (nf.eq.1) then
        if = binsrcha(frame,frames,NFRAME)
        ctype = vtypes(is)//'-'//ftypes(if)
      else
        ctype = vtypes(is)
      endif

c     Perform the transformation
      call coinit(lIn)
      call covelset(lIn,ctype)
      call cowrite(lIn,lIn)
      call cofin(lIn)

c     Write out some history.
      call hisopen(lIn,'append')
      call hiswrite(lIn,'VELSW: Miriad '//version)
      call hisinput(lIn,'VELSW')
      call hisclose(lIn)

c     All said and done. Close up.
      call xyclose(lIn)

      end
