      program imcomb

c= imcomb - Combine images
c& rjs
c: map combination
c+
c       IMCOMB is a MIRIAD task that combines several images into one.
c       The images are weighted in the region of overlap to minimise
c       the rms noise.
c@ in
c       Name of the input image datasets. Several can be given.
c       Wildcards are supported. At least one must be given.
c@ out
c       The name of the output dataset.
c@ rms
c       The rms noise levels of each of the input images.  This is used
c       in weighting the images when combining them (and thus minimising
c       the noise level in any overlap region).  Only the relative
c       magnitude of the noise values are important.
c
c       The images are weighted as 1/rms**2 -- so if you wish the images
c       to have speific weights, compute them accordingly. For example
c       if you wish to weight two images by the ratios 4 to 1, use
c         rms = 0.5,1
c       where 0.5 is 1/sqrt(4)
c
c       The default is to use the theoretical rms of the input images
c       found in the image header.  If this is missing, then the last
c       valid rms noise level found is used.  If no values are given,
c       and the first dataset does not contain the rms value in the
c       header, equal weights are used for all images.
c@ options
c       Extra processing options.  Several values can be given,
c       separated by a comma.  Minimum match of names is used.
c       Possible values are:
c         nonormalise  Do not renormalise the output.  Normally the
c                      output is normalised to account for overlap
c                      regions.
c         relax        Do not check that the coordinate systems are
c                      consistent.
c         fqaver       If 3rd axis (assumed to be frequency) is a
c                      single plane, average the input images in
c                      frequency, instead of producing a cube
c                      containing all planes.
c
c$Id: imcomb.for,v 1.10 2013/06/20 05:09:32 wie017 Exp $
c--
c  History:
c    rjs  29nov94 Original version.
c    rjs  12jan95 COrrect alignment code on axis 3. Write mask file.
c    rjs   3aug95 Mask file was not correctly set for options=nonorm.
c    rjs  15mar97 Handle processing multiple more than MAXOPEN files.
c    rjs  02jul97 cellscal change.
c    rjs  21jul97 Handle image alignment better. Fix bug related to
c                 incorrect flagging checks.
c    rjs  23jul97 Added pbtype.
c    rjs  13nov98 Increase MAXIN.
c    rjs  03oct00 Output can be arbitrarily large.
c    rjs  28jun02 Added options=relax, better error message.
c    rjs  17may06 Changes to make it more robust with images that span
c                 360 degrees.
c    rjs  31may06 Changes to use new calling sequence of pcvt.
c    mhw  29aug12 Add fqaver option
c    mhw  20jun13 Fix fqaver option - failed for > MAXOPEN files
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      integer MAXIN, MAXOPEN
      parameter (MAXIN=1024, MAXOPEN=6)

      logical equal, interp, mosaic, nonorm, relax, fqaver
      integer i, k, maxpix, minpix, nOpen, nOut(MAXNAX), naxis, nin,
     *        nrms, nsize(3,MAXIN), off(3), pData, pFlags, pWts, tOut,
     *        tno(MAXIN)
      real    blctrc(6,MAXIN), rms(MAXIN), rms0
      double precision f(MAXIN)
      character in(MAXIN)*64, line*80, out*64, version*80

      character stcat*80, versan*80
      external  stcat, versan
c-----------------------------------------------------------------------
      version = versan ('imcomb',
     *                  '$Revision: 1.10 $',
     *                  '$Date: 2013/06/20 05:09:32 $')
c
c  Get the inputs.
c
      call keyini
      call mkeyf('in',in,MAXIN,nin)
      call keya('out',out,' ')
      call mkeyr('rms',rms,MAXIN,nrms)
      call GetOpt(mosaic,nonorm,relax,fqaver)
      call keyfin
c
c  Check the inputs.
c
      if (nin.le.0) call bug('f','Input images must be given')
      if (out.eq.' ') call bug('f','An output image must be given')
c
c  Open the files, determine the size of the output. Determine the grid
c  system from the first map.
c
      if (nIn.le.maxOpen) then
        nOpen = nIn
      else
        nOpen = maxOpen - 1
      endif

      equal  = .false.
      do i = 1, nIn
        call xyopen(tno(i),In(i),'old',3,nsize(1,i))
        call coInit(tno(i))
        if (max(nsize(1,i),nsize(2,i)).gt.maxdim)
     *    call bug('f','Input map is too big')

        if (i.eq.1) then
          call ThingIni(nsize(1,i),blctrc(1,i))
          call rdhdi(tno(i),'naxis',naxis,3)
          naxis = min(naxis,MAXNAX)
        else
          call ThingChk(tno(1),tno(i),nsize(1,i),relax,fqaver,
     *      interp,blctrc(1,i))
          if (interp) then
            line = stcat('Geometry of '//in(1),
     *                   ' differs from '//in(i))
            call bug('w',line)
            call bug('f','Cannot interpolate')
          endif
        endif
c
c  Check the rms value.
c
        if (equal) then
          rms(i) = 1
        else if (nrms.lt.i) then
          call rdhdr(tno(i),'rms',rms0,0.0)
          if (rms0.gt.0) then
            rms(i) = rms0
          else if (i.eq.1) then
            rms(i) = 1
            equal = .true.
          else
            rms(i) = rms(i-1)
          endif
        endif
        if (rms(i).le.0) call bug('f','Invalid rms value')
        if (fqaver) then
          call rdhdd(tno(i),'crval3',f(i),0.d0)
        endif
        if (i.gt.nOpen) then
          call coFin(tno(i))
          call xyclose(tno(i))
        endif
      enddo
c
c  Determine the size of the output.
c
      do k = 1, 3
        minpix = nint(blctrc(k,1))
        maxpix = nint(blctrc(k+3,1))
        do i = 2, nIn
          minpix = min(minpix,nint(blctrc(k,  i)))
          maxpix = max(maxpix,nint(blctrc(k+3,i)))
        enddo
        nOut(k) = maxpix - minpix + 1
        off(k) = 1 - minpix
        do i = 1, nIn
          blctrc(k,i) = blctrc(k,i) + off(k)
          blctrc(k+3,i) = blctrc(k+3,i) + off(k)
        enddo
      enddo

      do k = 4, naxis
        nout(k) = 1
      enddo
        
c
c  Create the output.
c
      call xyopen(tOut,out,'new',naxis,nOut)
      call hdout(tno,rms,nIn,tOut,off,f,fqaver.and.nOut(3).eq.1,version)
c
c  Allocate arrays.
c
      call memAlloc(pData,nOut(1)*nOut(2),'r')
      call memAlloc(pWts,nout(1)*nOut(2),'r')
      call memAlloc(pFlags,nout(1),'l')
c
c  Process it.
c
      do k = 1, nOut(3)
        call CombIni(memr(pData),memr(pWts),nOut(1),nOut(2))
        do i = 1, nIn
          if (i.gt.nOpen) call xyopen(tno(i),In(i),'old',3,nsize(1,i))
          call Combo(k,tno(i),blctrc(1,i),nsize(1,i),1/rms(i)**2,
     *      memr(pData),memr(pWts),nOut(1),nOut(2))
          if (i.gt.nOpen) call xyclose(tno(i))
        enddo
        call CombFin(k,tOut,nonorm,memr(pData),memr(pWts),nOut(1),
     *    nOut(2),meml(pFlags))
      enddo
c
c  Free arrays.
c
      call memFree(pData,nOut(1)*nOut(2),'r')
      call memFree(pWts, nout(1)*nOut(2),'r')
      call memFree(pFlags,nout(1),'l')
c
c  Close up.
c
      do i = 1, nOpen
        call xyclose(tno(i))
      enddo
      call xyclose(tOut)
      end
c***********************************************************************
      subroutine GetOpt(mosaic,nonorm,relax,fqaver)

      logical mosaic,nonorm,relax,fqaver
c
c  Determine processing options.
c
c  Output:
c    mosaic
c    nonorm
c    relax
c-----------------------------------------------------------------------
      integer NOPTS
      parameter (NOPTS=4)
      logical present(NOPTS)
      character opts(NOPTS)*12
      data opts/'mosaic      ','nonormalise ','relax       ',
     *          'fqaver      '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)

      mosaic = present(1)
      nonorm = present(2)
      relax  = present(3)
      fqaver = present(4)

      end
c***********************************************************************
      subroutine hdout(tin,rms,nIn,tout,off,f,fqaver,version)

      integer nIn,tin(nIn),tout
      real rms(nIn)
      integer off(3)
      logical fqaver
      double precision f(nIn)
      character version*(*)

c  Make up the header of the output file.
c
c  Input:
c    tin        The handles of the input files, first is to be used as a
c               template.
c    rms        The image rms to use (from header or user)
c    nIn        The number of input files
c    tout       The handle of the output file
c    off        The ref pixel offset
c    fqaver     Flag to indicate we're averaging 2D images in frequency
c    version
c-----------------------------------------------------------------------
      integer   i
      double precision crpix
      character line*80, num*2

      character itoaf*2
      external  itoaf
c-----------------------------------------------------------------------
      double precision fout
      real wt
      logical ok
      
      ok=.true.
      
c     Start with a verbatim copy of the header.
      call headcp(tIn, tOut, 0, 0, 0, 0)

c     Update changed header items.
      do i = 1, 3
        num = itoaf(i)
        call rdhdd(tIn(1), 'crpix'//num, crpix, 0d0)
        crpix = crpix + off(i)
        call wrhdd(tOut, 'crpix'//num, crpix)
      enddo

c  Fill in average value of 3rd axis for 2d images
      if (fqaver) then
        fout=0
        wt=0
        do i=1, nIn
          if (f(i).gt.0) then
            fout = fout + log(f(i))/rms(i)**2
            wt = wt + 1/rms(i)**2
          else
            ok = .false.
          endif
        enddo
c  If we found strange freq values, it's probably a velocity axis       
        if (ok) then
          fout = exp(fout/wt)
          call wrhdd(tOut,'crval3',fout)
        endif
      endif

c     Create history.
      call hisopen(tout,'append')
      line = 'IMCOMB: Miriad '//version
      call hiswrite(tout,line)
      call hisinput(tout,'IMCOMB')
      call hisclose(tout)

      end
c***********************************************************************
      subroutine ThingIni(nsize,blctrc)

      integer   nsize(3)
      real      blctrc(6)
c-----------------------------------------------------------------------
      blctrc(1) = 1.0
      blctrc(2) = 1.0
      blctrc(3) = 1.0
      blctrc(4) = nsize(1)
      blctrc(5) = nsize(2)
      blctrc(6) = nsize(3)

      end
c***********************************************************************
      subroutine ThingChk(tIn,tOut,nsize,relax,fqaver,interp,blctrc)

      integer tIn, tOut, nsize(3)
      logical relax, fqaver, interp
      real    blctrc(6)
c-----------------------------------------------------------------------
      logical   valid
      integer   i
      double precision in(3), out(3)
c-----------------------------------------------------------------------
      call pcvtinit(tIn,tOut)

      if (relax) then
        in(1) = dble(nsize(1)/2 + 1)
        in(2) = dble(nsize(2)/2 + 1)
        in(3) = dble(nsize(3)/2 + 1)
        call pcvt(in, out, 3, valid)
        if (.not.valid) call bug('f',
     *    'Invalid coordinates prevented aligning images')

        blctrc(1) = in(1) - out(1) + 1d0
        blctrc(2) = in(2) - out(2) + 1d0
        blctrc(3) = in(3) - out(3) + 1d0
        blctrc(4) = blctrc(1) + nsize(1) - 1
        blctrc(5) = blctrc(2) + nsize(2) - 1
        blctrc(6) = blctrc(3) + nsize(3) - 1

      else
        in(1) = 1d0
        in(2) = 1d0
        in(3) = 1d0
        call pcvt(in, out, 3, valid)
        if (.not.valid) call bug('f',
     *    'Invalid coordinates prevented aligning images')

        blctrc(1) = 2d0 - out(1)
        blctrc(2) = 2d0 - out(2)
        blctrc(3) = 2d0 - out(3)
        in(1) = nsize(1)
        in(2) = nsize(2)
        in(3) = nsize(3)
        call pcvt(in, out, 3, valid)
        if (.not.valid) call bug('f',
     *    'Invalid coordinates prevented aligning images')

        blctrc(4) = 2*nsize(1) - out(1)
        blctrc(5) = 2*nsize(2) - out(2)
        blctrc(6) = 2*nsize(3) - out(3)
      endif

      interp = .false.
      if (fqaver.and.nsize(3).eq.1) then
        blctrc(3)=1d0
        blctrc(6)=1d0
      endif
      do i = 1, 3
        interp = interp .or.
     *     nint(blctrc(i+3))-nint(blctrc(i))+1.ne.nsize(i) .or.
     *     abs(nint(blctrc(i))-blctrc(i)).gt.0.05 .or.
     *     abs(nint(blctrc(i+3))-blctrc(i+3)).gt.0.05
      enddo

      end
c***********************************************************************
      subroutine CombIni(Data,Wts,nx,ny)

      integer nx,ny
      real Data(nx*ny),Wts(nx*ny)
c-----------------------------------------------------------------------
c Zero the arrays.
      integer i
c-----------------------------------------------------------------------
      do i = 1, nx*ny
        Data(i) = 0
        Wts(i) = 0
      enddo

      end
c***********************************************************************
      subroutine Combo(k,tIn,blctrc,nsize,Wt,Data,Wts,nx,ny)

      integer k,tIn,nsize(3),nx,ny
      real Data(nx,ny),Wts(nx,ny),blctrc(6),Wt
c
c  Add the contribution of this image.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real Line(MAXDIM)
      logical flags(MAXDIM)
      integer ioff,joff,koff,jlo,jhi,ilo,ihi,i,j
c-----------------------------------------------------------------------
      if (nsize(1).gt.MAXDIM) call bug('f','Image too big for me')

      ioff = 1 - nint(blctrc(1))
      joff = 1 - nint(blctrc(2))
      koff = 1 - nint(blctrc(3))

      if (k+koff.lt.1 .or. k+koff.gt.nsize(3)) return
      jlo = max(1,1-joff)
      jhi = min(ny,nsize(2)-joff)
      ilo = max(1,1-ioff)
      ihi = min(nx,nsize(1)-ioff)

      if (k+koff.gt.1) call xysetpl(tIn,1,k+koff)
      do j = jlo, jhi
        call xyread(tIn,j+joff,line)
        call xyflgrd(tIn,j+joff,flags)
        do i = ilo, ihi
          if (flags(i+ioff)) then
            Data(i,j) = Data(i,j) + Wt*Line(i+ioff)
            Wts(i,j)  = Wts(i,j)  + Wt
          endif
        enddo
      enddo

      end
c***********************************************************************
      subroutine CombFin(k,tOut,nonorm,Data,Wts,nx,ny,flags)

      integer k,tOut,nx,ny
      logical nonorm
      real Data(nx,ny),Wts(nx,ny)
      logical flags(nx)
c
c  Normalise and write out the images.
c-----------------------------------------------------------------------
      integer i,j
c-----------------------------------------------------------------------
      if (k.gt.1) call xysetpl(tOut,1,k)

      do j = 1, ny
        if (.not.nonorm) then
          do i = 1, nx
            if (Wts(i,j).gt.0.0) Data(i,j) = Data(i,j) / Wts(i,j)
            flags(i) = Wts(i,j).gt.0.0
          enddo
        else
          do i = 1, nx
            flags(i) = Wts(i,j).gt.0.0
          enddo
        endif
        call xywrite(tOut,j,Data(1,j))
        call xyflgwr(tOut,j,flags)
      enddo

      end
