      program imcat

c= imcat - Concatenate several images into one.
c& mchw
c: map combination
c+
c       IMCAT is a MIRIAD task to concatenate several images together
c       along an axis (generally the frequency or velocity
c       dimension).
c@ in
c       The input images.  Several file names can be entered, separated
c       by commas.  No default.
c@ out
c       The output image.  No default.
c@ axis
c       The axis to concatentate together.  The default is the 3rd axis
c       (i.e. axis=3).  You cannot concatenate together on the first
c       axis.
c@ options
c       Task enrichment options.  Minimum match is used.  Currently
c       there is only a single option:
c         relax  This instructs IMCAT to ignore axis descriptor
c                mismatches (e.g. pixel increments etc).  Use this with
c                care.
c
c$Id: imcat.for,v 1.8 2013/08/30 01:49:21 wie017 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      integer    MAXMAP
      parameter (MAXMAP=300)

      logical   domask, first, ok, relax, warned, warned1
      integer   axis, axLen(MAXNAX), axLen1(MAXNAX), i, lIn, lOut, map,
     *          naxis1, nmap, outPlane, plane
      real      dmax, dmin
      double precision cdelt, cdelt1(MAXNAX), crpix, crpix1(MAXNAX),
     *          crval, crval1(MAXNAX), x, x1
      character caxis*1, in(MAXMAP)*80, out*80, version*72, wflag*1

      logical   hdprsnt
      character itoaf*1, versan*72
      external  hdprsnt, itoaf, versan
c-----------------------------------------------------------------------
      version = versan('imcat',
     *                 '$Revision: 1.8 $',
     *                 '$Date: 2013/08/30 01:49:21 $')

c     Get the input parameters.
      call keyini
      call mkeyf('in',in,MAXMAP,nmap)
      if (nmap.le.1) call bug('f','Must have more than one input map')
      call keya('out',Out,' ')
      if (Out.eq.' ')
     *  call bug('f','You must give an output file')
      call keyi('axis',axis,3)
      if (axis.lt.2 .or. axis.gt.MAXNAX)
     *  call bug('f','Invalid value for axis keyword')
      call decopt(relax)
      call keyfin

c     Warn about relax option.
      wflag = 'f'
      if (relax) then
        wflag = 'w'
        call bug('i', 'Axis descriptor mismatches will be tolerated')
      endif

c     Open the input maps and check sizes, crpix and cdelt.
      call xyopen(lIn,in(1),'old',axis,axLen1)
      call rdhdi(lIn,'naxis',naxis1,axis)
      naxis1 = max(min(naxis1,MAXNAX),axis)
      if (axLen1(1).gt.MAXDIM) call bug('f','Image too big for me')
      do i = axis+1, naxis1
        axLen1(i) = 1
      enddo
      domask = hdprsnt(lIn,'mask')

      do i = 1, naxis1
        caxis = itoaf(i)
        call rdhdd(lIn, 'crpix'//caxis, crpix1(i), 0d0)
        call rdhdd(lIn, 'cdelt'//caxis, cdelt1(i), 1d0)
        call rdhdd(lIn, 'crval'//caxis, crval1(i), 0d0)
      enddo

      call xyclose(lIn)
      x1 = crval1(axis) +
     *       (dble(axLen1(axis)+1) - crpix1(axis))*cdelt1(axis)

      warned  = .false.
      warned1 = .false.
      do map = 2, nmap
        call xyopen(lIn,in(map),'old',axis,axLen)
        if (axLen(1).gt.MAXDIM) call bug('f','Image too big for me')
        do i = axis+1, naxis1
          axLen(i) = 1
        enddo
        domask = domask .or. hdprsnt(lIn,'mask')

        do i = 1, naxis1
          caxis = itoaf(i)
          call rdhdd(lIn, 'cdelt'//caxis, cdelt, 1d0)
          call descmp(cdelt, cdelt1(i), ok)
          if (.not.ok) then
            if (.not.warned) call bug(wflag,
     *        'cdelt values differ on axis '//caxis)
            warned1 = .true.
          endif

          call rdhdd(lIn, 'crpix'//caxis, crpix, 0d0)
          call rdhdd(lIn, 'crval'//caxis, crval, 0d0)
          if (i.ne.axis) then
            if (axLen(i).ne.axLen1(i)) call bug('f',
     *        'The images do not have compatible dimensions')

            call descmp(crpix, crpix1(i), ok)
            if (.not.ok) then
              if (.not.warned) call bug(wflag,
     *          'crpix values differ on axis '//caxis)
              warned1 = .true.
            endif

            call descmp(crval, crval1(i), ok)
            if (.not.ok) then
              if (.not.warned) call bug(wflag,
     *          'crval values differ on axis '//caxis)
              warned1 = .true.
            endif
          else
c           For celestial axes this test is only approximate, in normal
c           circumstances cdelt and crval should not differ.
            x  = crval + (1d0 - crpix)*cdelt
            ok = abs(x-x1).le.0.01*abs(cdelt)
            if (.not.ok) then
              if (.not.warned) call bug(wflag,
     *          'Images are not contiguous on axis '//caxis)
              warned1 = .true.
            endif
            x1 = x1 + dble(axLen(axis))*cdelt1(axis)
          endif
        enddo

        warned = warned .or. warned1
        call xyclose(lIn)
        axLen1(axis) = axLen1(axis) + axLen(axis)
      enddo

c     Open the output and make its header from the first input image.
      call xyopen(lIn,In(1),'old',axis,axLen)
      call xyopen(lOut,Out,'new',naxis1,axLen1)
      call headcp(lIn, lOut, 0, 0, 0, 0)

c     Update history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'IMCAT: Miriad ' // version)
      call hisinput(lOut, 'IMCAT')
      call hisclose(lOut)

c     Copy the maps into the output, plane by plane, row by row.
c     Find new max/min.
      first = .true.
      outPlane = 0
      do map = 1, nmap
        if (map.gt.1) call xyopen(lIn,in(map),'old',axis,axLen)

        do plane = 1, axLen(axis)
          outPlane = outPlane + 1
          call DatCpy(lIn,plane,lOut,outPlane,axLen,axis,domask,
     *                dmin,dmax,first)
        enddo
        call xyclose(lIn)
      enddo

c     Update header info and close output file.
      call wrhdr(lOut,'datamin',dmin)
      call wrhdr(lOut,'datamax',dmax)
      call xyclose(lOut)

      end

c***********************************************************************

      subroutine DatCpy(lIn,inPlane,lOut,outPlane,axLen,axis,domask,
     *                                        dmin,dmax,first)

      integer axis,inPlane,outPlane,axLen(axis-1),lIn,lOut
      logical first,domask
      real dmin,dmax
c-----------------------------------------------------------------------
c  Input:
c    axis
c    domask
c    inPlane
c    outPlane
c    axLen
c  Input/Output:
c    first      True if this is the very first call.
c    dmin,dmax  Min and max data value.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      logical   done, mask(MAXDIM)
      integer   i, inp(MAXNAX), outp(MAXNAX), row
      real      Data(MAXDIM)
c-----------------------------------------------------------------------
      do i = 1, axis-1
        inp(i) = 1
        outp(i) = 1
      enddo
      inp(axis)  = inPlane
      outp(axis) = outPlane

      if (axis.ge.3) then
        done = .false.
        do while (.not.done)
          call xysetpl(lIn,axis-2, Inp(3))
          call xysetpl(lOut,axis-2,Outp(3))
          do row = 1, axLen(2)
            call xyread(lIn,row,Data)
            if (domask) call xyflgrd(lIn,row,mask)
            do i = 1, axLen(1)
              if (first) then
                dmin=data(i)
                dmax=data(i)
                first = .false.
              else
                dmin=min(dmin,data(i))
                dmax=max(dmax,data(i))
              endif
            enddo
            call xywrite(lOut,row,Data)
            if (domask) call xyflgwr(lOut,row,mask)
          enddo
          if (axis.gt.3) then
            call planeinc(axis-3,axLen(3),inp(3),done)
            call planeinc(axis-3,axLen(3),outp(3),done)
          else
            done = .true.
          endif
        enddo
      else
        call xyread(lIn,1,Data)
        if (domask) call xyflgrd(lIn,1,mask)
        do i = 1, axLen(1)
          if (first) then
            dmin=data(i)
            dmax=data(i)
            first = .false.
          else
            dmin=min(dmin,data(i))
            dmax=max(dmax,data(i))
          endif
        enddo
        call xywrite(lOut,Outp(2),Data)
        if (domask) call xyflgwr(lOut,Outp(2),mask)
      endif

      end

c***********************************************************************

      subroutine planeinc(n,axLen,plane,done)

      logical done
      integer n,axLen(n),plane(n)
c-----------------------------------------------------------------------
      integer k
c-----------------------------------------------------------------------
      done = .true.
      k = 1
      do while (done .and. k.le.n)
        done = plane(k).ge.axLen(k)
        if (done) then
          plane(k) = 1
        else
          plane(k) = plane(k) + 1
        endif
        k = k + 1
      enddo

      end

c***********************************************************************

      subroutine decopt (relax)

      logical relax
c-----------------------------------------------------------------------
c  Decode options array into named variables.
c
c  Output:
c     relax     If true issue warnings about mismatched axis
c               descriptors between images instead of fatal error
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 1)

      character opshuns(maxopt)*5
      logical present(maxopt)
      data opshuns /'relax'/
c-----------------------------------------------------------------------
      call options('options', opshuns, present, maxopt)

      relax = present(1)

      end

c***********************************************************************

      subroutine descmp (r1, r2, ok)

      double precision r1, r2
      logical ok
c-----------------------------------------------------------------------
c  Check axis descriptors agree allowing for roundoff.
c
c  Output
c     ok     True if axis descriptors agree.
c-----------------------------------------------------------------------
      double precision dmax
c-----------------------------------------------------------------------
      dmax = max(abs(r1),abs(r2))
      ok = .not.(abs(r1-r2).gt.dmax*1d-6 .or. r1*r2.lt.0d0)

      end
