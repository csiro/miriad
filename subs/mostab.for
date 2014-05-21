c***********************************************************************
c  A set of routines to handle multi-pointing vis data sets, mosaic
c  tables, and to mosaic images.
c
c   subroutine MosCini
c   subroutine MosChk
c   subroutine MosCDone
c
c   subrouitne MosGeom
c   subroutine MosSize
c   subroutine MosGFin
c
c   subroutine MosInit
c   subroutine MosSet
c   subroutine MosGet
c   subroutine MosGetn
c   subroutine MosSetn
c   subroutine MosSave
c   subroutine MosLoad
c   subroutine MosPrint
c
c   subroutine MosMIni
c   subroutine Mosaicer
c   subroutine MosMFin
c
c   subroutine MosPnt
c   subroutine MosVal
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c $Id: mostab.for,v 1.11 2014/05/21 04:33:13 wie017 Exp $
c***********************************************************************

      subroutine MosCIni
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'

      integer   i

      external  prime
      integer   prime
c-----------------------------------------------------------------------
      npnt = 0
      nxy  = 0
      doinit = .true.
      HashSize = prime(MAXHASH)

      do i = 1, HashSize
        Hash(i) = 0
      enddo

      end

c***********************************************************************

      subroutine MosCDone(lIn)

      integer lIn
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
      doinit = .true.

      end

c***********************************************************************

      subroutine MosChk(lIn,i)

      integer lIn,i
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
      include 'mirconst.h'

      double precision radec1(2),dra1,ddec1
      character tel1*16,sctype*16

c     Externals.
      integer MosLoc
      logical MosSolar,uvVarUpd
c-----------------------------------------------------------------------
c  Set the reference position, and determine if we are to operate in
c  a mode where appreciable proper motion is anticipated.
c
      if (npnt.eq.0) then
        otf=.false.
        solar = MosSolar(lIn)
        call uvrdvrd(lIn,'ra',radec0(1),0d0)
        call uvrdvrd(lIn,'dec',radec0(2),0d0)
        call coRaDec(coRef,'SIN',radec0(1),radec0(2))
      endif
c
c  Initialise the handle to track changes in the primary beam.
c
      if (doinit) then
        call uvVarIni(lIn,vPntUpd)
        if (.not.solar) call uvVarSet(vPntUpd,'ra')
        if (.not.solar) call uvVarSet(vPntUpd,'dec')
        call uvVarSet(vPntUpd,'telescop')
        call uvVarSet(vPntUpd,'pbfwhm')
        call uvVarSet(vPntUpd,'pbtype')
        call uvVarSet(vPntUpd,'dra')
        call uvVarSet(vPntUpd,'ddec')
        call uvVarSet(vPntUpd,'pntra')
        call uvVarSet(vPntUpd,'pntdec')
        call uvVarSet(vPntUpd,'sctype')
      endif
c
c  Process a change in primary beam model.
c
      if (doinit .or. uvVarUpd(vPntUpd)) then
c
c  Get the RA,DEC and primary beam type for this record.
c
        call uvrdvrd(lIn,'dra',dra1,0d0)
        call uvrdvrd(lIn,'ddec',ddec1,0d0)
        if (solar) then
          radec1(1) = radec0(1)
          radec1(2) = radec0(2)
        else
          call uvrdvrd(lIn,'ra',radec1(1),0d0)
          call uvrdvrd(lIn,'dec',radec1(2),0d0)
        endif
        if (abs(dra1)+abs(ddec1).gt.0) then
          radec1(1) = radec1(1) + dra1 / cos(radec1(2))
          radec1(2) = radec1(2) + ddec1
        endif

        call pbRead(lIn,tel1)

        pntno = MosLoc(tel1,radec1)

c  Check for OTF mosaicing
        call uvrdvra(lIn,'sctype',sctype,' ')
        if (sctype.eq.'otfmos') then
          otf=.true.
          call uvrdvrd(lIn,'pntra',radec2(1,pntno),0d0)
          call uvrdvrd(lIn,'pntdec',radec2(2,pntno),0d0)
        else
          radec2(1,pntno)=0d0
          radec2(2,pntno)=0d0
        endif
      endif
      i = pntno
      doinit = .false.

      end

c***********************************************************************

      integer function MosLoc(tel1,radec1)

      character tel1*(*)
      double precision radec1(2)
c-----------------------------------------------------------------------
c  Locate a particular pointing in my table.
c
c  Tolerances are 1 arcsec in pointing and in primary beam size.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
      include 'mirconst.h'

      double precision tol
      parameter (tol=DPI/180d0/3600d0)

      logical   found
      integer   indx, pnt
      double precision ll1, lm(2), mm1

      external MosHash
      integer  MosHash
c-----------------------------------------------------------------------
c  Locate this pointing in our list of previously encountered pointings.
c
      found = .false.
      call coCvt(coRef,'aw/aw',radec1,'ap/ap',lm)
      ll1 = lm(1)
      mm1 = lm(2)
      if (npnt.gt.0) then
        if (abs(ll1-llmm(1,pntno)).lt.tol .and.
     *     abs(mm1-llmm(2,pntno)).lt.tol .and.
     *     tel1.eq.telescop(pntno)) then
          found = .true.
          pnt = pntno
        else
          indx = MosHash(ll1,mm1,HashSize)
          do while (.not.found .and. Hash(indx).ne.0)
            pnt = Hash(indx)
            if (abs(ll1-llmm(1,pnt)).lt.tol .and.
     *         abs(mm1-llmm(2,pnt)).lt.tol .and.
     *         tel1.eq.telescop(pnt)) then
              found = .true.
            else
              indx = indx + 1
              if (indx.gt.HashSize) indx = 1
            endif
          enddo
        endif
      endif
c
c  This pointing was not found. Add it to our list.
c
      if (.not.found) then
        npnt = npnt + 1
        pnt = npnt
        if (npnt.gt.MAXPNT) call bug('f','Pointing table overflow')
        telescop(npnt) = tel1
        llmm(1,npnt) = ll1
        llmm(2,npnt) = mm1
        radec(1,npnt) = radec1(1)
        radec(2,npnt) = radec1(2)
        indx = MosHash(ll1,mm1,HashSize)
        do while (Hash(indx).ne.0)
          indx = indx + 1
          if (indx.gt.HashSize) indx = 1
        enddo
        Hash(indx) = npnt
      endif

      MosLoc = pnt

      end

c***********************************************************************

      integer function MosHash(ll,mm,HashSize)

      double precision ll,mm
      integer HashSize
c-----------------------------------------------------------------------
c  Return a number in the range [1,HashSize] which is uniquely
c  associated with this pointing position.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision TOL
      parameter (TOL = 5d0*DAS2R)

      integer i,j,n,indx
c-----------------------------------------------------------------------
      i = nint(ll/TOL)
      j = nint(mm/TOL)
      n = mod(2*max(abs(i),abs(j)) - 1,HashSize)

      if (n.lt.0) then
        indx = 0
      else
        indx = n*n + n + i + j + 2
        if (i.gt.j) indx = indx + 2*n + 2
        indx = mod(indx,HashSize)
        if (indx.lt.0) indx = indx + HashSize
      endif
      MosHash = indx + 1

      end

c***********************************************************************

      subroutine mosChar(ra1,dec1,npnt1)

      double precision ra1,dec1
      integer npnt1
c-----------------------------------------------------------------------
c  Get the reference location, number of pointings and the projection
c  geometry.
c
c  Output:
c    ra1,dec1   Reference pointing.
c    npnt1      Number of pointings.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
      include 'mirconst.h'

      integer   i, i0
      double precision l0, m0
c-----------------------------------------------------------------------
c     Determine the average (l,m).
      if (.not.solar) then
        l0 = 0d0
        m0 = 0d0
        do i = 1, npnt
          l0 = l0 + llmm(1,i)
          m0 = m0 + llmm(2,i)
        enddo
        l0 = l0 / npnt
        m0 = m0 / npnt

c       Determine the pointing that is closest to this one.
        i0 = 1
        do i = 2, npnt
          if (abs(llmm(1,i) -l0)+abs(llmm(2,i) -m0).lt.
     *        abs(llmm(1,i0)-l0)+abs(llmm(2,i0)-m0)) i0 = i
        enddo

c       Convert from direction cosines to ra,dec.
        call coCvt(coRef,'ap/ap',llmm(1,i0),'aw/aw',radec0)
      endif

c     Return with all the goodies.
      ra1   = radec0(1)
      dec1  = radec0(2)
      npnt1 = npnt
      call coFin(coRef)

      end

c***********************************************************************

      logical function MosSolar(lIn)

      integer lIn
c-----------------------------------------------------------------------
c  Determine whether this is a solar system object, and so if the "solar
c  system" intepretation ra/dec changes should be used.
c-----------------------------------------------------------------------
      integer i
      real plmaj,plmin
      character source*32
c
c  A table of solar system objects. NOTE: The entries must be in
c  alphabetic order and lower case.
c
      integer NSOLAR
      parameter (NSOLAR=11)
      character solar(NSOLAR)*8

c     Externals.
      integer binsrcha

      data solar/'earth   ','jupiter ','mars    ','mercury ',
     *'moon    ','neptune ','pluto   ','saturn  ','sun     ',
     *'uranus  ','venus   '/
c-----------------------------------------------------------------------
c
c  Look for the source name in the list of solar system objects.
c
      call uvrdvra(lIn,'source',source,' ')
      call lcase(source)
      i = binsrcha(source,solar,NSOLAR)
      if (i.ne.0) then
        MosSolar = .true.
c
c  If it was not found in the list of known solar system objects,
c  see if it has plmaj and plmin variables. If so, its probably
c  a solar system object.
c
      else
        call uvrdvrr(lIn,'plmaj',plmaj,0.0)
        call uvrdvrr(lIn,'plmin',plmin,0.0)
        MosSolar = abs(plmaj)+abs(plmin).gt.0
      endif

      end

c***********************************************************************

      subroutine MosGinit(coObj,nx,ny,nchan,mnx,mny)

      integer coObj,nx,ny,nchan,mnx,mny
c-----------------------------------------------------------------------
c  Do geometry, shift and size calculations for a mosaiced imaging
c  sequence.
c
c  Input:
c    nx,ny      Size of the image.
c    nchan      Number of channels.
c    coObj      Coordinate object. On output, the reference pixel is
c               set.
c  Output:
c    mnx,mny    Mosaiced image size.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      include 'mostab.h'

      integer i
      double precision crpix1,crpix2,wcoeff(3)
      character ctype*16
c-----------------------------------------------------------------------
c  Get the cell increment -- for use later on.
c
      call coAxGet(coObj,1,ctype,crpix1,radec0(1),cdelt1)
      call coAxGet(coObj,2,ctype,crpix2,radec0(2),cdelt2)
c
c  Do goemetry correction calculations.
c
      do i = 1, npnt
        call coGeom(coObj,'aw/aw',radec(1,i),
     *                ucoeff(1,i),vcoeff(1,i),wcoeff)
      enddo
c
c  Allocate the arrays to determine shifts.
c
      nxy = nchan*npnt
      call MemAllop(pX,nxy,'r')
      call MemAllop(pY,nxy,'r')
c
c  Do the shift calculations.
c
      call MosShift(coObj,npnt,nchan,memr(pX),memr(pY))
c
c  Get the size of the output image.
c
      nx2 = (nx-1)/2
      ny2 = (ny-1)/2
      call MosSizer(nx2,ny2,memr(pX),memr(pY),npnt,nchan,mnx,mny,
     *                                        crpix1,crpix2)
c
c  Corrrect the coordinate object for this change in reference
c  pixel.
c
      call coSetd(coObj,'crpix1',crpix1)
      call coSetd(coObj,'crpix2',crpix2)
c
c  Initialise the RMS arrays.
c
      do i = 1, npnt
        Rms2(i) = 0
        SumWt(i) = 0
      enddo

      end

c***********************************************************************

      subroutine MosShift(coObj,NPNT1,NCHAN,x,y)

      integer   coObj, NPNT1, NCHAN
      real      x(NCHAN,NPNT1), y(NCHAN,NPNT1)
c-----------------------------------------------------------------------
c  Determine the fractional pixel shifts and the resulting
c  alignment between the different pointings.
c
c  Input:
c    coObj      Coordinate object handle.
c    NPNT1      Number of pointings.
c    NCHAN      Number of frequency channels.
c  Output:
c    x,y        Pixel location, relative to the reference pixel,
c               of the resulting pointing.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'

      integer   i, j
      double precision x1(3), x2(3)
c-----------------------------------------------------------------------
      if (npnt.ne.NPNT1)
     *  call bug('f','Inconsistent number of pointings')

      do i = 1, NCHAN
        call coCvt1(coObj,3,'ap',dble(i),'aw',x1(3))

        do j = 1, npnt
          x1(1) = radec(1,j)
          x1(2) = radec(2,j)
          call coCvt(coObj,'aw/aw/aw',x1,'op/op/op',x2)
          x(i,j) = x2(1)
          y(i,j) = x2(2)
        enddo
      enddo

      end

c***********************************************************************

      subroutine MosSizer(nx2,ny2,x,y,npnt,nchan,mnx,mny,crpix1,crpix2)

      integer nx2,ny2,npnt,nchan,mnx,mny
      real x(nchan,npnt),y(nchan,npnt)
      double precision crpix1,crpix2
c-----------------------------------------------------------------------
c  Determine the size of the output image.
c
c  Input:
c    nx2,ny2    Half sizes of the base images.
c    nchan      Number of channels.
c    npnt       Number of pointins.
c  Input/Output:
c    x,y        These give the pixel coordinates of the centre of each
c               pointing.  On input, this is relative to a reference
c               pixel of 0.  On output, it it relative to a non-zero
c               reference pixel.
c  Output:
c    crpix1,crpix2 Reference pixels.
c    mnx,mny    Size of mosaiced image.
c-----------------------------------------------------------------------
      integer i,j,imin,imax,jmin,jmax
      real xmin,xmax,ymin,ymax
c-----------------------------------------------------------------------
      xmin = x(1,1)
      xmax = xmin
      ymin = y(1,1)
      ymax = ymin

      do j = 1, npnt
        do i = 1, nchan
          xmin = min(xmin,x(i,j))
          xmax = max(xmax,x(i,j))
          ymin = min(ymin,y(i,j))
          ymax = max(ymax,y(i,j))
        enddo
      enddo

      imin = nint(xmin - nx2 + 0.5)
      imax = nint(xmax + nx2 - 0.5)
      jmin = nint(ymin - ny2 + 0.5)
      jmax = nint(ymax + ny2 - 0.5)
      mnx = imax - imin + 1
      mny = jmax - jmin + 1
      crpix1 = 1 - imin
      crpix2 = 1 - jmin

      do j = 1, npnt
        do i = 1, nchan
          x(i,j) = x(i,j) + crpix1
          y(i,j) = y(i,j) + crpix2
        enddo
      enddo

      end

c***********************************************************************

      subroutine MosGeom(size,n,nchan,npol,Vis,Wts)

      integer size,n,nchan,npol
      complex Vis(size,n)
      real Wts(n)
c-----------------------------------------------------------------------
c  Perform geometry corrections, shifts and calculation of the rms noise
c  for a set of visibilities.
c
c  Input:
c    size       Record size
c    n          Number of records.
c    nchan      Number of channels.
c    npol       Number of polarisations.
c    Wts        Weights to be used -- used to determine rms noise.
c  Input/Output:
c    Vis        Visibility data.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
c     Call the routine that does the real work.
      call MosGeom1(size,n,nchan,npol,npnt,Vis,Wts,ucoeff,vcoeff,
     *  memr(pX),memr(pY),cdelt1,cdelt2,Rms2,SumWt)

      end

c***********************************************************************

      subroutine MosGeom1(size,n,nchan,npol,npnt,Vis,Wts,
     *                ucoeff,vcoeff,x,y,cdelt1,cdelt2,Rms2,SumWt)

      integer size,n,nchan,npol,npnt
      complex Vis(size,n)
      double precision ucoeff(3,npnt),vcoeff(3,npnt)
      real x(nchan,npnt),y(nchan,npnt),RMS2(npnt),SumWt(npnt),Wts(n)
      double precision cdelt1,cdelt2
c-----------------------------------------------------------------------
c  Apply all geometry and shift corrections for a mosaiced observation.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'
      integer InUV,InWPnt,InRmsFq,InData
      parameter (InUV=1,InWPnt=2,InRmsFq=3,InData=5)

      complex fac(MAXCHAN)
      real uu,vv,ww,ud,vd,theta,sigma2
      integer i,j,i0,k,pnt
c-----------------------------------------------------------------------
c  Consistency check.
c
      if (nchan.gt.MAXCHAN)
     *        call bug('f','Too many channels for me, in MosGeom1')
      if (InData-1+nchan*npol.ne.size)
     *        call bug('f','Inconsistent, in MosGeom1')
c
c  Loop the loop.
c
      do k = 1, n
        uu = real (Vis(InUV,k))
        vv = aimag(Vis(InUV,k))
        ww = real (Vis(InWPnt,k))
        pnt = nint(aimag(Vis(InWPnt,k)))
        sigma2 = real(Vis(InRmsFq,k))
c
c  Accumulate rms noise information.
c
        Rms2(pnt) = Rms2(pnt) + Wts(k)*Wts(k)*sigma2
        SumWt(pnt) = SumWt(pnt) + Wts(k)
c
c  Do geometry corrections.
c
        ud = uu*ucoeff(1,pnt) + vv*ucoeff(2,pnt) + ww*ucoeff(3,pnt)
        vd = uu*vcoeff(1,pnt) + vv*vcoeff(2,pnt) + ww*vcoeff(3,pnt)
        Vis(InUV,k) = cmplx(ud,vd)
        ud = ud * cdelt1
        vd = vd * cdelt2
c
c  Do fractional pixel shift.
c
        do i = 1, nchan
          theta = -TWOPI * (ud*(anint(x(i,pnt)) - x(i,pnt)) +
     *                      vd*(anint(y(i,pnt)) - y(i,pnt)))
          fac(i) = cmplx(cos(theta),sin(theta))
        enddo

        i0 = InData
        do j = 1, npol
          do i = 1, nchan
            Vis(i0,k) = Vis(i0,k) * fac(i)
            i0 = i0 + 1
          enddo
        enddo
      enddo

      end

c***********************************************************************

      subroutine MosLoad(tno,npnt1)

      integer tno,npnt1
c-----------------------------------------------------------------------
c  Read in a mosaic table from a dataset.
c
c  Input:
c    tno        Handle of the input dataset.
c  Output:
c    npnt1      The number of pointings.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
      integer i,item,iostat,offset,ival(2),size,n,iax,blocksize
      double precision crpix
      real rval(2)

c     Externals.
      integer hsize
      logical hdprsnt
      character itoaf*2
c-----------------------------------------------------------------------
c
c  Open the pointing table.
c
      if (hdprsnt(tno,'mostable')) then
        call haccess(tno,item,'mostable','read',iostat)
        if (iostat.ne.0) then
          call bug('w','Error opening input mosaic table')
          call bugno('f',iostat)
        endif
c
c  Read the main body of the pointing table.
c
        size = hsize(item)
        call hreadi(item,ival,0,8,iostat)
c
c  Check the table version number (i4 starting at offset 4)
c  and check if we have otf data
c
        otf=.false.
        blocksize = 48
        if (ival(2).eq.2) then
          blocksize = blocksize + 16
          otf=.true.
        endif
        offset = 8
        if (mod(size-offset,blocksize).ne.0)
     *    call bug('f','Bad size for mosaic table')
        npnt = (size - offset)/blocksize
        if (npnt.gt.MAXPNT)
     *        call bug('f','Too many pointings, in mosLoad')
        do i = 1, npnt
          if (iostat.eq.0) call hreadi(item,ival,offset,8,iostat)
          nx2 = (ival(1)-1)/2
          ny2 = (ival(2)-1)/2
          offset = offset + 8
          if (iostat.eq.0) call hreadd(item,radec(1,i),offset,16,iostat)
          offset = offset + 16
          if (iostat.eq.0)
     *        call hreadb(item,telescop(i),offset,16,iostat)
          offset = offset + 16
          if (iostat.eq.0) call hreadr(item,rval,offset,8,iostat)
          Rms2(i) = rval(1)
          offset = offset + 8
          if (otf) then
            if (iostat.eq.0)
     *        call hreadd(item,radec2(1,i),offset,16,iostat)
            offset = offset + 16
          endif
        enddo
c
c  Finish up. Check for errors and then close the dataset.
c
        if (iostat.ne.0) then
          call bug('w','Error reading from mosaic table')
          call bugno('f',iostat)
        endif
        call hdaccess(item,iostat)
        if (iostat.ne.0) then
          call bug('w','Error closing mosaic table')
          call bugno('f',iostat)
        endif
c
c  Handle the case of no mosaicing table -- treat it as if its
c  just a single pointing.
c
      else
        npnt = 1
        call coInit(tno)
        call coFindAx(tno,'longitude',iax)
        if (iax.eq.0) call bug('f','Failed to find RA axis')
        call coCvt1(tno,iax,'op',0d0,'aw',radec(1,1))
        call coCvt1(tno,iax,'op',0d0,'ap',crpix)
        call rdhdi(tno,'naxis'//itoaf(iax),n,0)
        nx2 = max(crpix-1,n-crpix) + 1

        call coFindAx(tno,'latitude',iax)
        if (iax.eq.0) call bug('f','Failed to find DEC axis')
        call coCvt1(tno,iax,'op',0d0,'aw',radec(2,1))
        call coCvt1(tno,iax,'op',0d0,'ap',crpix)
        call rdhdi(tno,'naxis'//itoaf(iax),n,0)
        ny2 = max(crpix-1,n-crpix) + 1

        call rdhdr(tno,'rms',rms2(1),0.0)
        if (rms2(1).le.0) rms2(1) = 1
        call pbRead(tno,telescop(1))

        call coFin(tno)
      endif

      npnt1 = npnt

      end

c***********************************************************************

      subroutine MosPrint
c-----------------------------------------------------------------------
c  Write -- to standard output -- the contents of the mosaic table.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'

      integer   i
      character decs*14, line*80, ras*12

      external  itoaf, hangle, rangle
      character itoaf*8, hangle*12, rangle*14
c-----------------------------------------------------------------------
      call output('    Number of pointing centers: '//itoaf(npnt))
      call output(' ')
      call output('      Sub-Image       Pointing Center      '//
     *        'Primary Beam      Field')
      call output('         Size         RA           DEC     '//
     *        'Type               RMS')
      call output('     ----------- ------------------------  '//
     *        '------------    --------')
      do i = 1, npnt
        ras  = hangle(radec(1,i))
        decs = rangle(radec(2,i))

        write(line,10) i,2*nx2+1,2*ny2+1,ras,decs,telescop(i),rms2(i)
 10     format(i4,1x,i5,1x,i5,1x,3a,1pe8.2)
        call output(line)

        if (otf) then
          ras  = hangle(radec2(1,i))
          decs = rangle(radec2(2,i))
          write(line,20) ras,decs
 20       format(17x,2a)
          call output(line)
        endif
      enddo

      end

c***********************************************************************

      subroutine mosInit(nx,ny)

      integer nx, ny
c-----------------------------------------------------------------------
c  Initialise a mosaic table.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
      npnt = 0
      nx2  = (nx-1)/2
      ny2  = (ny-1)/2

      end

c***********************************************************************

      subroutine mosGet(i,ra1,dec1,rms1,pbtype1)

      integer   i
      double precision ra1, dec1
      real      rms1
      character pbtype1*(*)
c-----------------------------------------------------------------------
c  Get information from the table.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
      include 'mirconst.h'
c-----------------------------------------------------------------------
      if (i.lt.1 .or. i.gt.npnt)
     *  call bug('f','Invalid pointing number, in mosGet')

      ra1  = radec(1,i)
      dec1 = radec(2,i)
      rms1 = rms2(i)
      pbtype1 = telescop(i)

      end

c***********************************************************************

      subroutine mosSet(i,ra1,dec1,rms1,pbtype1)

      integer   i
      double precision ra1, dec1
      real      rms1
      character pbtype1*(*)
c-----------------------------------------------------------------------
c  Add an extry to the mosaic table.
c
c  Input:
c    ra1,dec1   Pointing centre ra,dec.
c    rms1       Nominal field rms noise.
c    pbtype1    Primary beam type.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
      npnt = max(npnt,i)
      if (npnt.gt.MAXPNT) call bug('f','Mosaic table overflow')
      radec(1,i) = ra1
      radec(2,i) = dec1
      rms2(i) = rms1
      telescop(i) = pbtype1

      end

c***********************************************************************

      subroutine mosGetn(nx2d,ny2d,npnt1)

      integer nx2d,ny2d,npnt1
c-----------------------------------------------------------------------
c  Return information about the mosaicing setup.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
      nx2d  = nx2
      ny2d  = ny2
      npnt1 = npnt

      end

c***********************************************************************

      subroutine mosSetn(nx2d,ny2d)

      integer nx2d,ny2d
c-----------------------------------------------------------------------
c  Set info about the image size.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
      nx2 = nx2d
      ny2 = ny2d

      end

c***********************************************************************

      subroutine MosSave(tno)

      integer tno
c-----------------------------------------------------------------------
c  Write out a mosaicing table to this dataset.
c
c  Input:
c    tno        Handle of the output dataset.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'

      integer   i, iostat, item, ival(2), offset
      real      rval(2)
c-----------------------------------------------------------------------
c
c  Open the pointing table.
c
      call haccess(tno,item,'mostable','write',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening output mosaic table')
        call bugno('f',iostat)
      endif
c
c  Write the header.
c
      ival(1) = 0
      ival(2) = 0

c  Set table version number to 2 if we're doing OTF mosaics
      if (otf) ival(2) = 2
      offset = 0
      call hwritei(item,ival,offset,8,iostat)
      offset = offset + 8
c
c  Write the main body of the pointing table.
c
      do i = 1, npnt
        ival(1) = 2*nx2 + 1
        ival(2) = 2*ny2 + 1
        if (iostat.eq.0) call hwritei(item,ival,offset,8,iostat)
        offset = offset + 8
        if (iostat.eq.0) call hwrited(item,radec(1,i),offset,16,iostat)
        offset = offset + 16
        if (iostat.eq.0) call hwriteb(item,telescop(i),offset,16,iostat)
        offset = offset + 16
        rval(1) = Rms2(i)
        rval(2) = 0
        if (iostat.eq.0) call hwriter(item,rval,offset,8,iostat)
        offset = offset + 8
        if (otf) then
          if (iostat.eq.0)
     *      call hwrited(item,radec2(1,i),offset,16,iostat)
          offset = offset + 16
        endif
      enddo
c
c  Finish up. Check for errors and then close the dataset.
c
      if (iostat.ne.0) then
        call bug('w','Error writing to mosaic table')
        call bugno('f',iostat)
      endif
      call hdaccess(item,iostat)
      if (iostat.ne.0) then
        call bug('w','Error closing mosaic table')
        call bugno('f',iostat)
      endif

      end

c***********************************************************************

      subroutine MosGFin
c-----------------------------------------------------------------------
c  Tidy up.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
      integer i,ngood
      real Sig
c-----------------------------------------------------------------------
      if (nxy.gt.0) then
        call MemFrep(pX,nxy,'r')
        call MemFrep(pY,nxy,'r')
        nxy = 0
      endif

      ngood = 0
      Sig = 0
      do i = 1, npnt
        if (SumWt(i).gt.0) then
          Rms2(i) = Rms2(i) / (SumWt(i)*SumWt(i))
        else
          Rms2(i) = 0
        endif
        if (Rms2(i).gt.0) then
          ngood = ngood + 1
          Sig = Sig + Rms2(i)
          Rms2(i) = sqrt(Rms2(i))
        endif
      enddo
c
c  If some of the RMS values were zero, fill them in with an average
c  value. If they are all bad, fill them all in with 1!
c
      if (ngood.eq.0) then
        Sig = 1
      else
        Sig = sqrt(Sig/ngood)
      endif

      if (ngood.lt.npnt) then
        do i = 1, npnt
          if (Rms2(i).le.0) Rms2(i) = Sig
        enddo
      endif

      end

c***********************************************************************

      subroutine MosMIni(coObj,chan)

      integer   coObj
      real      chan
c-----------------------------------------------------------------------
c  Initialise ready for a mosaic operation.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'

      integer   i
      double precision x1(3), x2(3), xn(2)
c-----------------------------------------------------------------------
      call coCvt1(coObj,3,'ap',dble(chan),'aw',x1(3))

      do i = 1, npnt
        x1(1) = radec(1,i)
        x1(2) = radec(2,i)
        call coCvt(coObj,'aw/aw/aw',x1,'ap/ap/ap',x2)
        x0(i) = x2(1)
        y0(i) = x2(2)

        xn(1) = radec2(1,i)
        xn(2) = radec2(2,i)
        if (otf .and. (xn(1).ne.0d0 .or. xn(2).ne.0d0)) then
          call pbInitcc(pbObj(i),telescop(i),coObj,'aw/aw',x1,xn,0d0,0.)
        else
          call pbInitc(pbObj(i),telescop(i),coObj,'ap/ap',x2,0d0,0.0)
        endif
      enddo

      end

c***********************************************************************

      subroutine Mosaicer(In,Out,nx,ny,npnt1,mnx,mny,
     *                                Runs,MAXRUNS,nRuns)

      integer nx,ny,npnt1,mnx,mny,MAXRUNS,nRuns,Runs(3,MAXRUNS)
      real In(nx,ny,npnt1),Out(mnx,mny)
c-----------------------------------------------------------------------
c  Mosaic the different fields together.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      include 'mostab.h'

      ptrdiff pWts
c-----------------------------------------------------------------------
      if (npnt1.ne.npnt)
     *  call bug('f','Inconsistency in Mosaic')

      if (npnt.eq.1) then
        call Mosaic1(In,Out,nx,ny,mnx,mny,Runs,MAXRUNS,nRuns)
      else
        call MemAllop(pWts,mnx*mny,'r')
        if (nx2.gt.(nx-1)/2 .or. ny2.gt.(ny-1)/2)
     *    call bug('f','Inconsistency in Mosaicer')
        call Mosaic2(In,Out,memr(pWts),nx,ny,npnt,mnx,mny,Rms2)
        call mosRuns(memr(pWts),mnx,mny,Runs,MAXRUNS,nRuns)
        call memFrep(pWts,mnx*mny,'r')
      endif

      end

c***********************************************************************

      subroutine Mosaic1(In,Out,nx,ny,mnx,mny,Runs,maxruns,nruns)

      integer nx,ny,mnx,mny,MAXRUNS,nRuns,Runs(3,MAXRUNS)
      real In(nx,ny),Out(mnx,mny)
c-----------------------------------------------------------------------
c  DO the linear mosaic on all the fields in the simple case that
c  there is only one field. This consists of simply copying the input
c  to the output.
c-----------------------------------------------------------------------
      integer pbObj,imin,imax,jmin,jmax,ic,jc,ioff,joff,i,j
      real x,y,xext,yext

c     Externals.
      integer mosPb
c-----------------------------------------------------------------------
      pbObj = mosPb(1)
      call mosExt(1,imin,imax,jmin,jmax)
      call pbExtent(pbObj,x,y,xext,yext)
      ic = nx/2 + 1
      jc = ny/2 + 1
      ioff = ic - nint(x)
      joff = jc - nint(y)
      imin = max(imin,1)
      imax = min(imax,mnx)
      jmin = max(jmin,1)
      jmax = min(jmax,mny)

      if (MAXRUNS.lt.jmax-jmin+2)
     *  call bug('f','Runs buffer overflow, in Mosaicer')

      nRuns = 0

      do j = 1, jmin-1
        do i = 1, mnx
          Out(i,j) = 0
        enddo
      enddo

      do j = jmin, jmax
        do i = 1, imin-1
          Out(i,j) = 0
        enddo
        do i = imin, imax
          Out(i,j) = In(i+ioff,j+joff)
        enddo
        do i = imax+1, mnx
          Out(i,j) = 0
        enddo

        nRuns = nRuns + 1
        Runs(1,nRuns) = j
        Runs(2,nRuns) = imin
        Runs(3,nRuns) = imax
      enddo

      do j = jmax+1, mny
        do i = 1, mnx
          Out(i,j) = 0
        enddo
      enddo

      Runs(1,nRuns+1) = 0

      end

c***********************************************************************

      subroutine Mosaic2(In,Out,Wts,nx,ny,npnt,mnx,mny,Rms2)

      integer nx,ny,npnt,mnx,mny
      real In(nx,ny,npnt),Out(mnx,mny),Wts(mnx,mny),Rms2(npnt)
c-----------------------------------------------------------------------
c  Do a linear mosaic of all the fields. Weight is so that the
c  RMS never exceeds the max RMS in the input data.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer i,j,ic,jc,ioff,joff,imin,jmin,imax,jmax,k,pbObj
      real Pb(MAXDIM),Wt3,x,y,xext,yext

c     Externals.
      real pbGet,mosWt3
      integer mosPb
c-----------------------------------------------------------------------
c
c  Check that we have enough space.
c
      if (mnx.gt.MAXDIM) call bug('f','Buffer overflow, in Mosaicer')
c
c  Initialise the output and weights arrays.
c
      do j = 1, mny
        do i = 1, mnx
          Out(i,j) = 0
          Wts(i,j) = 0
        enddo
      enddo

      ic = nx/2 + 1
      jc = ny/2 + 1

      do k = 1, npnt
        Wt3 = mosWt3(k)
        pbObj = mosPb(k)
        call mosExt(k,imin,imax,jmin,jmax)
        call pbExtent(pbObj,x,y,xext,yext)
        ioff = ic - nint(x)
        joff = jc - nint(y)
        imin = max(imin,1)
        imax = min(imax,mnx)
        jmin = max(jmin,1)
        jmax = min(jmax,mny)

        do j = jmin, jmax
          do i = imin, imax
            Pb(i) = pbGet(pbObj,real(i),real(j))
          enddo
          do i = imin, imax
            Out(i,j) = Out(i,j) + Wt3*Pb(i)*In(i+ioff,j+joff,k)
            Wts(i,j) = Wts(i,j) + Wt3*Pb(i)*Pb(i)
          enddo
        enddo
      enddo

      call MosWt(Rms2,npnt,Out,Wts,mnx*mny)

      end

c***********************************************************************

      subroutine mosRuns(Wts,nx,ny,Runs,MAXRUNS,nRuns)

      integer nx,ny,MAXRUNS,nRuns,Runs(3,MAXRUNS)
      real Wts(nx,ny)
c-----------------------------------------------------------------------
c  Make a Runs array to indicate which pixels are good.
c-----------------------------------------------------------------------
      integer i,j,ngood
c-----------------------------------------------------------------------
      nRuns = 0

      do j = 1, ny
        ngood = 0
        do i = 1, nx
          if (Wts(i,j).gt.0) then
            ngood = ngood + 1
          else if (ngood.gt.0) then
            nruns = nruns + 1
            if (nruns.ge.MAXRUNS) call bug('f','Runs buffer overflow')
            Runs(1,nRuns) = j
            Runs(2,nRuns) = i - ngood
            Runs(3,nRuns) = i - 1
            ngood = 0
          endif
        enddo
        if (ngood.gt.0) then
          nruns = nruns + 1
          if (nruns.ge.MAXRUNS) call bug('f','Runs buffer overflow')
          Runs(1,nRuns) = j
          Runs(2,nRuns) = nx - ngood + 1
          Runs(3,nRuns) = nx
        endif
      enddo

      Runs(1,nRuns+1) = 0

      end

c***********************************************************************

      subroutine MosWt(Rms2,npnt,Out,Wts,n)

      integer npnt,n
      real Rms2(npnt),Out(n),Wts(n)
c-----------------------------------------------------------------------
c  Reweight the data according to some scheme.
c-----------------------------------------------------------------------
      real Sigt,scale
      integer i,k
c-----------------------------------------------------------------------
c
c  Determine the maximum RMS.
c
      Sigt = Rms2(1)
      do k = 2, npnt
        Sigt = max(Sigt,Rms2(k))
      enddo
      Sigt = Sigt*Sigt
c
c  Now rescale to correct for the weights.
c
      do i = 1, n
        scale = Sigt*Wts(i)
        if (scale.le.0) then
          scale = 0
        else if (scale.lt.1) then
          scale = sqrt(scale)/Wts(i)
        else
          scale = 1/Wts(i)
        endif
        Out(i) = scale*Out(i)
      enddo

      end

c***********************************************************************

      subroutine MosMFin
c-----------------------------------------------------------------------
c  Tidy up after mosaicing.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
      integer i
c-----------------------------------------------------------------------
      do i = 1, npnt
        call pbFin(pbObj(i))
      enddo

      end

c***********************************************************************

      subroutine MosVal(coObj,in,x,gain,rms)

      integer   coObj
      character in*(*)
      double precision x(*)
      real      gain, rms
c-----------------------------------------------------------------------
c  Determine the gain and rms response at a particular position.
c
c  Input:
c    coObj      Coordinate object.
c    in,x       These are the normal arguments to coCvr, giving the
c               location (in RA,DEC,freq) of interest.
c  Output:
c    gain       The gain response at the position.
c    rms        The rms at the position.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'

      integer   runs(3)
      double precision xref(3)
c-----------------------------------------------------------------------
c     Determine the location of the reference position in pixel coords.
      call coCvt(coObj, in, x, 'ap/ap/ap', xref)

c     Initialise.
      call mosMini(coObj, real(xref(3)))

c     Mosaic, tidy up.
      runs(1) = nint(xref(2))
      runs(2) = nint(xref(1))
      runs(3) = runs(2)
      call mosWtsr(runs, 1, gain, rms, 1)

      if (gain.gt.0.0) then
        gain = 1.0 / gain
        rms  = sqrt(rms*gain)
      endif

c     Tidy up.
      call mosMFin

      end

c***********************************************************************

      subroutine MosPnt(coObj,in,x,beams,psf,NX,NY,NPNT1)

      integer   coObj, NX, NY, NPNT1
      character in*(*)
      double precision x(*)
      real      beams(NX,NY,NPNT1), psf(NX,NY)
c-----------------------------------------------------------------------
c  Determine the true point-spread function of a mosaiced image.
c
c  Input:
c    coObj      Coordinate object.
c    in,x       These are the normal arguments to coCvr, giving the
c               location (in RA,DEC,freq) of interest.
c    beams      The beam patterns for each pointing.
c    NX,NY      The size of the individual beam patterns.
c    NPNT1      The number of pointings.
c  Output:
c    psf        The point-spread function.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'

      ptrdiff   pWts
      double precision xref(3)
c-----------------------------------------------------------------------
c     Determine the location of the reference position in pixel coords.
      call coCvt(coObj, in, x, 'ap/ap/ap', xref)

      call MosMini(coObj, real(xref(3)))

      call MemAllop(pWts, NX*NY, 'r')
      call MosPnt1(NX, NY, NPNT1, nint(xref(1)), nint(xref(2)), beams,
     *             memr(pWts), psf)
      call MemFrep(pWts, NX*NY, 'r')

      call MosMFin

      end

c***********************************************************************

      subroutine MosPnt1(NX,NY,NPNT1,xr,yr,beams,wts,psf)

      integer   NX, NY, NPNT1, xr, yr
      real      beams(NX,NY,NPNT1), wts(NX,NY), psf(NX,NY)
c-----------------------------------------------------------------------
c  Determine the true point-spread function of a mosaiced image.
c
c  Input:
c    NX,NY      The size of the individual beam patterns.
c    NPNT1      The number of pointings.
c    xr,yr      Pixel location where we want to work out the PSF.
c    beams      The beam patterns for each pointing.
c  Scratch:
c    wts        The weight array.
c  Output:
c    psf        The point-spread function.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'

      integer   i, imax, imin, j, jmax, jmin, k, xoff, yoff
      real      pb(MAXDIM), scale, wt3

      external  mosWt3, pbGet
      real      mosWt3, pbGet
c-----------------------------------------------------------------------
c     Check!
      if (npnt.ne.NPNT1)
     *  call bug('f','Inconsistent number of pointings')

c     Initialise the output and weights array.
      do j = 1, NY
        do i = 1, NX
          psf(i,j) = 0.0
          wts(i,j) = 0.0
        enddo
      enddo

      xoff = xr - NX/2 - 1
      yoff = yr - NY/2 - 1

c     Loop over all the pointings.
      do k = 1, npnt
        call mosExt(k,imin,imax,jmin,jmax)
        imin = max(imin-xoff,1)
        imax = min(imax-xoff,NX)
        jmin = max(jmin-yoff,1)
        jmax = min(jmax-yoff,NY)
        if (imin.le.imax .and. jmin.le.jmax) then
          wt3 = mosWt3(k)
          scale = wt3 * pbGet(pbObj(k),real(xr),real(yr))
          if (scale.gt.0.0) then
            do j = jmin, jmax
              do i = imin, imax
                Pb(i) = pbGet(pbObj(k),real(i+xoff),real(j+yoff))
              enddo
              do i = imin, imax
                psf(i,j) = psf(i,j) + scale*Pb(i)*beams(i,j,k)
                wts(i,j) = wts(i,j) + wt3*Pb(i)*Pb(i)
              enddo
            enddo
          endif
        endif
      enddo

c     Reweight the data.
      call MosWt(Rms2,npnt,psf,wts,NX*NY)

      end

c***********************************************************************

      real function mosWt3(k)

      integer k
c-----------------------------------------------------------------------
c  Return the field-dependent weight to be used in mosaicing.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
      MosWt3 = 1.0 / (rms2(k)*rms2(k))

      end

c***********************************************************************

      integer function mosPb(k)

      integer k
c-----------------------------------------------------------------------
c  Return the primary beam corresponding to a particular pointing.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
      mosPb = pbObj(k)

      end

c***********************************************************************

      subroutine mosExt(k,imin,imax,jmin,jmax)

      integer k,imin,imax,jmin,jmax
c-----------------------------------------------------------------------
c  Determine the region that this pointing will add to.
c  NOTE: We form "ceil" and "floor" functions from nint(x+0.5) and
c  nint(x-0.5) respectively.
c  Also note that we add then subtract an integer offset to make the arg
c  of the nint function positive, because nint(-0.5) + 1 is not equal to
c  nint(-0.5+1), and we need to be consistent (independent of any offset
c  added to the pixel coordinates).
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
      integer offset
c-----------------------------------------------------------------------
      offset = abs(x0(k)) + nx2 + 1
      imin = nint(x0(k)-nx2+0.5 + offset) - offset
      imax = nint(x0(k)+nx2-0.5 + offset) - offset
      offset = abs(y0(k)) + ny2 + 1
      jmin = nint(y0(k)-ny2+0.5 + offset) - offset
      jmax = nint(y0(k)+ny2-0.5 + offset) - offset

      end

c***********************************************************************

      subroutine mosWts(Wt1,Wt2,nx,ny,xoff,yoff)

      integer nx,ny,xoff,yoff
      real Wt1(nx,ny),Wt2(nx,ny)
c-----------------------------------------------------------------------
c  Determine the weights to apply to data during the mosaicing process.
c  The region-of-interest is a rectangular one.
c  Input:
c    xoff,yoff  Add these to the pbObj coordinates to get the local
c               pixel coordinates.
c-----------------------------------------------------------------------
      integer nx2,ny2,npnt,imin,imax,jmin,jmax,i,j,k,pbObj
      real Wt3,Pb

c     Externals.
      real mosWt3,pbGet
      integer mosPb
c-----------------------------------------------------------------------
c
c  Initialise the weight array.
c
      do j = 1, ny
        do i = 1, nx
          Wt1(i,j) = 0
        enddo
      enddo
c
c  Deternime some things.
c
      call mosGetn(nx2,ny2,npnt)

      do k = 1, npnt
        pbObj = mosPb(k)
        Wt3 = mosWt3(k)
        call mosExt(k,imin,imax,jmin,jmax)
        imin = max(imin+xoff,1)
        imax = min(imax+xoff,nx)
        jmin = max(jmin+yoff,1)
        jmax = min(jmax+yoff,ny)

        do j = jmin, jmax
          do i = imin, imax
            Pb = pbGet(pbObj,real(i-xoff),real(j-yoff))
            Wt1(i,j) = Wt1(i,j) + Wt3 * Pb * Pb
          enddo
        enddo
      enddo
c
c  Normalise.
c
      call mosWtC(Wt1,Wt2,nx*ny)

      end

c***********************************************************************

      subroutine mosWtsR(Runs,nRuns,Wt1,Wt2,npix)

      integer nRuns,Runs(3,nRuns),npix
      real Wt1(npix),Wt2(npix)
c-----------------------------------------------------------------------
c  Determine weights to apply to data during the mosaicing process.
c  The region-of-interest is specified by the much unloved Runs format.
c-----------------------------------------------------------------------
      integer k,i,j,n,jmin,jmax,imin,imax,ilo,ihi,iRuns,nin
      integer nx2,ny2,npnt,pbObj
      real Wt3,Pb

c     Externals.
      real mosWt3,pbGet
      integer mosPb
c-----------------------------------------------------------------------
c
c  Initialise the weight array.
c
      do i = 1, npix
        Wt1(i) = 0
      enddo
c
c  Deternime some things.
c
      call mosGetn(nx2,ny2,npnt)

      do k = 1, npnt
        pbObj = mosPb(k)
        Wt3 = mosWt3(k)
        call mosExt(k,imin,imax,jmin,jmax)

        n = 0
        do iRuns = 1, nRuns
          if (Runs(1,iRuns).ge.jmin .and. Runs(1,iRuns).le.jmax .and.
     *       Runs(3,iRuns).ge.imin .and. Runs(2,iRuns).le.imax) then
            j = Runs(1,iRuns)
            ilo = max(Runs(2,iRuns),imin)
            ihi = min(Runs(3,iRuns),imax)
            nin = n + ilo - Runs(2,iRuns)
            do i = ilo, ihi
              nin = nin + 1
              Pb = pbGet(pbObj,real(i),real(j))
              Wt1(nin) = Wt1(nin) + Wt3 * Pb * Pb
            enddo
          endif
          n = n + Runs(3,iRuns) - Runs(2,iRuns) + 1
        enddo
      enddo
c
c  Normalise.
c
      call mosWtC(Wt1,Wt2,npix)

      end

c***********************************************************************

      subroutine mosWtC(Wt1,Wt2,npix)

      integer npix
      real Wt1(npix),Wt2(npix)
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
      integer i,k
      real scale,Sigt
c-----------------------------------------------------------------------
c
c  Determine the maximum RMS.
c
      Sigt = Rms2(1)
      do k = 2, npnt
        Sigt = max(Sigt,Rms2(k))
      enddo
      Sigt = Sigt*Sigt
c
c  Now compute the weights for the data.
c
      do i = 1, npix
        scale = Sigt*Wt1(i)
        if (scale.le.0) then
          Wt1(i) = 0
          Wt2(i) = 0
        else if (scale.lt.1) then
          scale = sqrt(scale)
          Wt2(i) = scale/Wt1(i)
          Wt1(i) = 1/scale
        else
          Wt2(i) = 1/Wt1(i)
          Wt1(i) = 1
        endif
      enddo

      end

c***********************************************************************

      subroutine mosRaDec(k,ra,dec)

      integer k
      double precision ra,dec
c-----------------------------------------------------------------------
c  Return the RA and DEC of the k'th pointing.
c
c  Input:
c    k          Pointing number
c  Output:
c    ra,dec     RA and DEC (radians) of the pointing.
c
c OBSOLETE: Use mosGet
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
      if (k.lt.1 .or. k.gt.npnt) call bug('f',
     *        'Invalid pointing number if mosRaDec')

      ra  = radec(1,k)
      dec = radec(2,k)

      end

c***********************************************************************

      subroutine mosInfo(nx2d,ny2d,npnt1)

      integer nx2d,ny2d,npnt1
c-----------------------------------------------------------------------
c  Return information about the mosaicing setup.
c  OBSOLETE: Use mosGetn
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mostab.h'
c-----------------------------------------------------------------------
      nx2d  = nx2
      ny2d  = ny2
      npnt1 = npnt

      end
