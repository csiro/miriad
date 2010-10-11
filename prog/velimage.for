      program velimage

c= VELIMAGE - Make (x,y,z) image from an (x,y) image and model z-values.
c& mchw
c: image analysis
c+
c       VELIMAGE is a MIRIAD task to make an (x,y,z) image from an (x,y)
c       image and a model for the z-values and dispersion.  The
c       z-values, for example the mean velocity, are input as an (x,y)
c       image, whilst the dispersion can be input as either an (x,y)
c       image or a fixed value.  The output image is formed as
c
c          out(x,y,z) = in1(x,y) * exp(-(z-in2(x,y))**2/(2*sigma**2)))
c
c       VELIMAGE can be used to compare models with the data.  For
c       example one can generate a model image corresponding to a
c       rotating galaxy, convolve it with the actual beam and subtract
c       the model from the data to determine if there is residual
c       emission which deviates from the model rotation curve.
c@ in
c       The input image names, separated by commas.  The first image is
c       the (x,y) intensity distribution integrated over the z-axis,
c       e.g. velocity-integrated image.  The second image is an (x,y)
c       model for the z-values, e.g. a mean velocity image.  No default
c       for either.
c       A 3rd input image gives the z-dispersion at each point. e.g. a
c       velocity dispersion image.  If the 3rd image is not specified
c       then fixed dispersion, sigma must be specified.
c@ region
c       Region of image to be used.  See documentation on region for
c       details.  All pixels within the bounding box are used; pixel
c       masking is not used.  Default is the entire image.
c@ sigma
c       Fixed value for z-dispersion if not specified by 3rd input
c       image.
c@ nchan
c       Number of channels for z-axis of output image. Default=1.
c@ start
c       Starting value for z-axis of output image. No default.
c@ step
c       Interval for z-axis of output image. No default.
c@ out
c       The output (x,y,z) image. No default.
c@ options
c       Options. Minimum match is active.
c         relax  ignore axis descriptor mismatches
c                (e.g. pixel increments etc).  Use with care.
c--
c  History:
c    23sep92 mchw  New task.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      character version*(*)
      parameter (version='VELIMAGE: version 30-Nov-92')

      integer MAXBOXES,MAXRUNS,NAXIS
      parameter (MAXBOXES=2048,MAXRUNS=3*maxdim,NAXIS=4)

      integer boxes(MAXBOXES)
      character*80 in(3),out
      integer i,j,k,nchan,map,nmap,lout,lIn(3)
      integer nsize(NAXIS),size(NAXIS),blc(NAXIS),trc(NAXIS)
      real amp(maxdim),val(maxdim),sig(maxdim),buf(maxdim)
      real cdelt(NAXIS),crval(NAXIS),crpix(NAXIS)
      real cdelt1,crval1,crpix1,v,sigma,start,step
      logical relax
      character*1 caxis,wflag

c     Externals.
      character*1 itoaf
c-----------------------------------------------------------------------
c     Get the input parameters.
      call output(version)
      call keyini
      call mkeyf('in',in,3,nmap)
      call keyr('sigma',sigma,0.0)
      call keyi('nchan',nchan,1)
      call keyr('start',start,0.0)
      call keyr('step',step,0.0)
      call keya('out',out,' ')
      call getopt(relax)
      call keyfin

c     Check the inputs.
      if (nmap.lt.2) call bug('f','Must have at least two input maps')
      if (out.eq.' ') call bug('f','output image file not given')
      if (nmap.eq.2 .and. sigma.eq.0.0) call bug('f',
     *    'Either z-dispersion image or sigma must be specified')
      if (start.eq.0.0) call bug('f','No start given for z-axis')
      if (step.eq.0.0) call bug('f','No step given for z-axis')
      if (relax) then
        wflag = 'w'
        call bug('i','Axis descriptor mismatches will be tolerated')
      else
        wflag = 'f'
      endif

c     Open the input maps and check sizes, crpix and cdelt.
      call xyopen(lIn(1),in(1),'old',NAXIS,size)
      if (size(1).gt.maxdim .or. size(2).gt.maxdim)
     *                call bug('f','Image too big for MAXDIM')
      do i = 1, NAXIS
        caxis = itoaf(i)
        call rdhdr(lIn(1),'cdelt'//caxis,cdelt(i),1.0)
        call rdhdr(lIn(1),'crpix'//caxis,crpix(i),0.0)
        call rdhdr(lIn(1),'crval'//caxis,crval(i),0.0)
      enddo

      map = 2
      do while (map.le.nmap)
        call xyopen(lIn(map),in(map),'old',NAXIS,nsize)
        if (nsize(1).ne.size(1) .or. nsize(2).ne.size(2))
     *  call bug('f','Each map must have same xy dimensions')
        do i = 1, NAXIS
          caxis = itoaf(i)
          call rdhdr(lIn(map),'cdelt'//caxis,cdelt1,1.0)
          if (cdelt1.ne.cdelt(i)) call bug(wflag,'cdelt not the same')
          call rdhdr(lIn(map),'crpix'//caxis,crpix1,0.0)
          call rdhdr(lIn(map),'crval'//caxis,crval1,0.0)
          if ((crval1+(1-crpix1)*cdelt1).ne.
     *        (crval(i)+(1-crpix(i))*cdelt(i)))
     *           call bug(wflag,'reference positions not the same')
        enddo
        map = map + 1
      enddo

c     Set up the region of interest.
      call boxSet(boxes,NAXIS,nsize,'s')
      call boxInfo(boxes,NAXIS,blc,trc)

c     Open the output image and write it's header.
      nsize(1) = trc(1)-blc(1)+1
      nsize(2) = trc(2)-blc(2)+1
      nsize(3) = nchan
      call xyopen(lout,out,'new',3,nsize)
      call headcopy(lIn,lout,0,2,blc,trc)
      call wrhda(lout,'ctype3','VELO-LSR')
      call wrhda(lout,'bunit','KM/S')
      call wrhdr(lout,'crpix3',1.0)
      call wrhdr(lout,'crval3',start)
      call wrhdr(lout,'cdelt3',step)

c     Generate the output image.
      do k = 1, nchan
        v = start+(k-1)*step
        call xysetpl(lout,1,k)
        do j = blc(2), trc(2)
          call xyread(lIn(1),j,amp)
          call xyread(lIn(2),j,val)
          if (nmap.eq.3) call xyread(lIn(3),j,sig)
          do i = blc(1), trc(1)
            if (nmap.eq.3) sigma=sig(i)
            if (abs(v-val(i)).lt.5.0*sigma) then
              buf(i) = amp(i) * exp(-(v-val(i))**2 / (2.0*sigma**2))
            else
              buf(i) = 0.0
            endif
          enddo
          call xywrite(lout,j-blc(2)+1,buf(blc(1)))
        enddo
      enddo

c     Update history and close files.
      call hisopen(lout,'append')
      call hisWrite(lout,version)
      call hisInput(lout,'VELIMAGE')
      call hisClose(lout)
      call xyclose(lout)
      do i = 1, nmap
        call xyclose(lIn(i))
      enddo

      end

c***********************************************************************

      subroutine getopt(relax)

      logical relax
c-----------------------------------------------------------------------
c  Decode options array into named variables.
c
c  Output:
c     relax     If true issue warnings about mismatched axis
c               descriptors between images instead of fatal error.
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 1)

      character ops(maxopt)*5
      logical present(maxopt)
      data ops /'relax'/
c-----------------------------------------------------------------------
      call options ('options', ops, present, maxopt)
      relax = present(1)

      end
