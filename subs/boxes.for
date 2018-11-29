c***********************************************************************
c* Boxes -- Summary of region of interest routines.
c& mjs
c: region-of-interest
c+
c  This collection of routines allows the user to specify a fairly
c  arbitrary region of interest, and for the programmer to retrieve
c  these specifications in a reasonably convenient form.  There
c  are six programmer callable routines:
c
c       subroutine BoxInput(key,file,boxes,maxboxes)
c       subroutine BoxMask(tno,boxes,maxboxes)
c       subroutine BoxSet(boxes,naxis,nsize,flags)
c       subroutine BoxDef(boxes,naxis,blc,trc)
c       subroutine BoxInfo(boxes,naxis,blc,trc)
c       subroutine BoxBound(boxes,shape,naxis,type,blx,trc)
c       logical function BoxRect(boxes)
c       subroutine BoxRuns(naxis,plane,flags,boxes,
c    *                  runs,maxruns,nruns,xminv,xmaxv,yminv,ymaxv)
c
c  BoxInput     This reads the region of interest from the task
c               parameters.
c  BoxMask      "AND" in a mask from a data file.
c  BoxSet       The programmer calls this to indicate the size of the
c               image of interest.
c  BoxInfo      Returns information about the region currently selected.
c  BoxRect      This returns .true. if the selected region is purely
c               rectangular (i.e. describable by blc and trc).
c  BoxRuns      The programmer calls this to retrieve the region-of-
c               interest selected for a particular plane.
c
c  The BOX routines work by reading the region specified on the command
c  line and breaking it into an intermediate form, stored in the "boxes"
c  array.  This intermediate form consists of a number of subregion
c  specifications which are ANDed and ORed together to produce the
c  output region.  The subregions can consist of IMAGE (a rectangular
c  subregion of a range of planes), BOX (a number of rectangular
c  subregions of a range of planes), POLY (a polygonal region of a range
c  of planes) and MASK (an arbitrary region, specified by a mask file).
c  A positive value (the norm) indicates the subregion is "ORed",
c  whereas a negative value (rare) indicates it is "ANDed".  Each shape
c  is described by XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX which gives the cube
c  which bounds the shape.  In some cases these are unknown, if so they
c  are set to zero.
c
c  Some shapes require extra DATA (the size of which is given by SIZE):
c    BOX  This requires the blc and trc for the corners of the boxes,
c         stored as (xmin,ymin),(xmax,ymax). For N boxes, SIZE=4*N.
c         The boxes are sorted in increasing order of xmin.
c    POLY This also gives the (x1,y1),(x2,y2) coordinate pair of each of
c         the line segments which make the polygon (except horizontal
c          segments are discarded).  For a poly made of N (non-
c          horizontal) line segments, SIZE=4*N.  The line segments are
c          sorted in increasing order of min(x1,x2).
c    MASK Additional info is the handle of the mask file.
c    PERC This requires the percentage of the x and y axis to select,
c         SIZE=2.        
c
c  The routine BoxRuns returns the region of a particular plane that was
c  selected. This is returned in "runs", which is a table of entries of
c  (j,xmin,xmax). One entry indicates that
c  pixels (xmin,j) to (xmax,j) are selected. There may be multiple (or
c  zero) entries for a particular value of j, though all entries are
c  non-overlapping. The table is in increasing j and xmin.
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c  CARMA Customizations:
c
c  2011-Jul-13  pkgw  Add box(m) shorthand for box(-m,-m,m,m)
c
c $Id: boxes.for,v 1.11 2018/11/29 22:50:56 wie017 Exp $
c--
c***********************************************************************

c* BoxInput -- Read command line box specification.
c& mjs
c: region-of-interest
c+
      subroutine BoxInput(key, file, boxes, maxboxes)

      character key*(*),file*(*)
      integer   maxboxes, boxes(maxboxes)
c  ---------------------------------------------------------------------
c  Read the box specification from the command line.
c
c  Input:
c    key        The keyword associated with the parameter, generally
c               "region". If this is a blank string, then the user
c               input routines are not called, and the default region
c               of interest is used.
c    file       Name of the Miriad data-set used to extrac coordinate
c               transformation info (in particular crpix, cdelt, and
c               crval).
c    maxboxes   The size of the boxes array.
c  Output:
c    boxes      An intermediate form of the boxes specification.  This
c               is used by subsequent BOX routines to
c--
c  The variables xytype and ztype give the units that are currently
c  expected for specifying coordinates in x and y, and z. Possible
c  values are
c    xytype: 'abspix', 'relpix' or 'arcsec'.
c    ztype:  'abspix' or 'kms  '.
c  The default is 'abspix'.
c-----------------------------------------------------------------------
      include 'boxes.h'
      include 'maxnax.h'

      integer    NTYPES
      parameter (NTYPES=11)

      logical   coordini, more, dounit
      integer   boxtype, i, ilat, ilng, ispc, k2, length,k1, lu(3), n,
     *          nshape, nsize(MAXNAX), offset, spare, tmp(4)
      character algo*8, spec*4096, type*10, types(NTYPES)*10, units*8,
     *          wtype*16, xytype*6, ztype*6

      external  keyprsnt, len1
      logical   keyprsnt
      integer   len1

      data types(BOX  ) /'boxes     '/
      data types(MASK ) /'mask      '/
      data types(POLY ) /'polygon   '/
      data types(IMAGE) /'images    '/
      data types(ARCSEC)/'arcsec    '/
      data types(KMS  ) /'kms       '/
      data types(RELPIX)/'relpixel  '/
      data types(ABSPIX)/'abspixel  '/
      data types(RELCEN)/'relcenter '/
      data types(QUART) /'quarter   '/
      data types(PERC)  /'percentage'/
c-----------------------------------------------------------------------
      coordini = .false.
      xytype = 'abspix'
      ztype  = 'abspix'
      offset = OFFSET0
      nshape = 0
      if (key.ne.' ') then
        more = keyprsnt(key)
      else
        more = .false.
      endif
      if (more) call keya(key,spec,' ')
      do while (more)
        k1 = 1
        k2 = len1(spec)
        spare = maxboxes - offset - HDR
        if (spare.lt.0)
     *    call BoxBug(spec,'Region too complex')

c       Determine the subcommand type.
        call GetTok(spec,k1,k2,type,length)
        boxtype = 0
        if (length.le.len(types(1)) .and. length.gt.0) then
          do i = 1, NTYPES
            if (type(1:length).eq.types(i)(1:length)) boxtype = i
          enddo
        endif

c       Process unit specification subcommands.
        dounit = .false.
        if (boxtype.eq.ABSPIX) then
          xytype = 'abspix'
          ztype  = 'abspix'
        else if (boxtype.eq.RELPIX) then
          xytype = 'relpix'
          dounit = .true.
        else if (boxtype.eq.RELCEN) then
          xytype = 'relcen'
          dounit = .true.
        else if (boxtype.eq.ARCSEC) then
          xytype = 'arcsec'
          dounit = .true.
        else if (boxtype.eq.KMS) then
          ztype = 'kms'
          dounit = .true.

        else if (boxtype.eq.IMAGE .or. boxtype.eq.QUART) then
c         Process region specification subcommands.
          boxes(offset+XMIN) = 0
          boxes(offset+XMAX) = 0
          boxes(offset+YMIN) = 0
          boxes(offset+YMAX) = 0
          boxes(offset+SIZE) = 0
          call BoxZRnge(spec,k1,k2,boxes(offset+ZMIN),ztype,lu)
        else if (boxtype.eq.PERC) then
          boxes(offset+XMIN) = 0
          boxes(offset+XMAX) = 0
          boxes(offset+YMIN) = 0
          boxes(offset+YMAX) = 0
          boxes(offset+SIZE) = 2
          call BoxInt(spec,k1,k2,boxes(offset+DATA),n,1,2,'abspix',lu)
          if (n.eq.0) boxes(offset+DATA) = 100
          if (n.eq.1) boxes(offset+DATA+1) = boxes(offset+DATA)
          call BoxZRnge(spec,k1,k2,boxes(offset+ZMIN),ztype,lu)       
        else if (boxtype.eq.BOX) then
          boxtype = IMAGE
          call BoxInt(spec,k1,k2,tmp,n,4,4,xytype,lu)
          if (n.ne.4) call BoxBug(spec,'Invalid BOX subcommand')
          boxes(offset+XMIN)  = min(tmp(1),tmp(3))
          boxes(offset+XMAX)  = max(tmp(1),tmp(3))
          boxes(offset+YMIN)  = min(tmp(2),tmp(4))
          boxes(offset+YMAX)  = max(tmp(2),tmp(4))
          boxes(offset+SIZE)  = 0
          call BoxZRnge(spec,k1,k2,boxes(offset+ZMIN),ztype,lu)
        else if (boxtype.eq.POLY) then
          call BoxPoly(spec,k1,k2,boxes(offset+DATA),n,spare,
     *        boxes(offset+XMIN),xytype,lu)
          boxes(offset+SIZE) = n
          call BoxZRnge(spec,k1,k2,boxes(offset+ZMIN),ztype,lu)
        else if (boxtype.eq.MASK) then
          if (spare.lt.3) call BoxBug(spec,'Subregion too complex')
          call BoxMsk(spec,k1,k2,boxes(offset+DATA),
     *         boxes(offset+XMIN))
          boxes(offset+SIZE) = 1
        else
          call BoxBug(spec,'Unrecognised subregion command')
        endif

c       Finish up with this subcommand.
        if (boxtype.eq.ABSPIX) then
          continue

        else if (dounit) then
          if (.not.coordini) then
            coordini = .true.
            if (file.eq.' ') call bug('f',
     *        'Only absolute pixel region specification supported')
            call xyopen(lu,file,'old',MAXNAX,nsize)
            call coInit(lu)
            call rdhdi(lu,'naxis1',lu(2),1)
            call rdhdi(lu,'naxis2',lu(3),1)
            call xyclose(lu)
          endif

          if (boxtype.eq.ARCSEC) then
            call coFindAx(lu, 'longitude',ilng)
            call coFindAx(lu, 'latitude', ilat)
            if (min(ilng,ilat).ne.1 .or. max(ilng,ilat).ne.2)
     *        call BoxBug(spec,'First two axes are not in arcsec')

          else if (boxtype.eq.KMS) then
            ispc = 0
            call coAxType(lu, ispc, 'spectral', wtype, units)
            if (ispc.ne.3) call BoxBug(spec,'No spectral axis present')

            if (units.ne.'km/s') then
              call coSpcSet(lu, 'VRAD', ' ', ispc, algo)
            endif
          endif

        else
          boxes(offset+ITYPE) = boxtype
          offset = offset + boxes(offset+SIZE) + HDR
          nshape = nshape + 1
        endif

c       Check that we have finished this command.
        if (k1.le.k2) call BoxBug(spec,'Unexpected trailing characters')

c       Get the next subcommand.
        more = keyprsnt(key)
        if (more) call keya(key,spec,' ')
      enddo

c     Release the coordinate system if one has been allocated.
      if (coordini) call coFin(lu)

c     Fill in a default object if none was given.
      if (nshape.eq.0) then
        boxes(offset+ITYPE) = IMAGE
        boxes(offset+XMIN)  = 0
        boxes(offset+XMAX)  = 0
        boxes(offset+YMIN)  = 0
        boxes(offset+YMAX)  = 0
        boxes(offset+ZMIN)  = 0
        boxes(offset+ZMAX)  = 0
        boxes(offset+SIZE)  = 0
        nshape = 1
      endif

      Boxes(1) = nshape
      Boxes(NX) = 0
      Boxes(NY) = 0
      Boxes(NZ) = 0

      end

c***********************************************************************

      subroutine BoxZRnge(spec,k1,k2,zrange,ztype,lu)

      character spec*(*),ztype*(*)
      integer k1,k2,zrange(2),lu(3)
c-----------------------------------------------------------------------
c  Return the zrange specification.
c
c  Input:
c    spec       The subregion command.
c    ztype      The units used for the z axis. Either 'abspix' or 'kms'.
c    lu         Information used to convert from km/s to pixels.
c  In/Out:
c    k1,k2      The substring of spec still needing to be processed.
c  Output:
c    zrange     The output range in planes.
c
c-----------------------------------------------------------------------
      integer n
c-----------------------------------------------------------------------
      if (k1.gt.k2) then
        zrange(1) = 1
        zrange(2) = 0
      else
        call BoxInt(spec,k1,k2,zrange,n,1,2,ztype,lu)
        if (n.eq.1) then
          zrange(2) = zrange(1)
        else if (zrange(1).gt.zrange(2)) then
          n = zrange(2)
          zrange(2) = zrange(1)
          zrange(1) = n
        endif
      endif

      end

c***********************************************************************

      subroutine BoxInt(spec,k1,k2,boxes,n,modulo,nmax,type,lu)

      character spec*(*),type*(*)
      integer lu(3)
      integer k1,k2,nmax,n,modulo,boxes(nmax)
c-----------------------------------------------------------------------
c  Decode some numbers, that are enclosed within brackets.
c  It should look something like: '(1,2,3,4)'. The numbers can be
c  in a number of units, as given by "type". This routine then
c  converts them to grid units (absolute pixels).
c
c  Input:
c    spec       The string containing the piece to be decoded.
c    nmax       The maximum number of integers to be returned.
c    modulo     The number of integers returned must be some
c               multiple of "modulo".
c    type       One of 'abspix','relpix','arcsec','kms','relcen'.
c    lu         Information used in converting between physical
c               coordinates and grid units.
c  In/Out:
c    k1,k2      This indicates the substring of spec left to process.
c  Output:
c    boxes      Array containing the integers found.
c    n          The number of integers found.
c
c-----------------------------------------------------------------------
      include 'mirconst.h'
      integer k0,i
      logical more,ok,shhand
      double precision temp,temp2,x1(2),x2(2)
c-----------------------------------------------------------------------
      if (spec(k1:k1).ne.'(')
     *  call BoxBug(spec,'Bad subregion command')
      k1 = k1 + 1
      k0 = k1
      n = 0
      more = .true.
      do while (k1.le.k2 .and. more)
        if (spec(k1:k1).eq.',' .or. spec(k1:k1).eq.')') then
          more = spec(k1:k1).eq.','
          if (k1.le.k0) call BoxBug(spec,'Bad region subcommand')
          n = n + 1
          shhand = (.not.more).and.(n.eq.1).and.
     *         (modulo.eq.4).and.(nmax.eq.4)
          if (n.gt.nmax)
     *    call BoxBug(spec,'Subregion too complex -- buffer overflow')
          call atodf(spec(k0:k1-1),temp,ok)
          if (.not.ok) call BoxBug(spec,'Error decoding a value')
          if (type.eq.'abspix') then
            boxes(n) = nint(temp)
          else if (type.eq.'relpix') then
            if (shhand) then
c            Implicit (-x, -x, +x, +x) box
               call coCvt1(lu,1,'op',-temp,'ap',temp2)
               boxes(1) = nint(temp2)
               call coCvt1(lu,2,'op',-temp,'ap',temp2)
               boxes(2) = nint(temp2)
               call coCvt1(lu,1,'op',temp,'ap',temp2)
               boxes(3) = nint(temp2)
               call coCvt1(lu,2,'op',temp,'ap',temp2)
               boxes(4) = nint(temp2)
               n = 4
            else
               i = mod(n-1,2) + 1
               call coCvt1(lu,i,'op',temp,'ap',temp2)
               boxes(n) = nint(temp2)
            endif
          else if (type.eq.'relcen') then
            if (shhand) then
c            Implicit (-x, -x, +x, +x) box
               boxes(1) = nint(-temp) + lu(2)/2 + 1
               boxes(2) = nint(-temp) + lu(3)/2 + 1
               boxes(3) = nint(temp) + lu(2)/2 + 1
               boxes(4) = nint(temp) + lu(3)/2 + 1
               n = 4
            else
               i = mod(n-1,2) + 1
               boxes(n) = nint(temp) + lu(i+1)/2 + 1
            endif
          else if (type.eq.'arcsec') then
            if (shhand) then
c            Implicit (-x, -x, +x, +x) box
               x1(1) = dpi/180/3600 * (-temp)
               x1(2) = x1(1)
               call coCvt(lu,'ow/ow',x1,'ap/ap',x2)
               boxes(1) = nint(x2(1))
               boxes(2) = nint(x2(2))
               x1(1) = dpi/180/3600 * temp
               x1(2) = x1(1)
               call coCvt(lu,'ow/ow',x1,'ap/ap',x2)
               boxes(3) = nint(x2(1))
               boxes(4) = nint(x2(2))
               n = 4
            else
               i = mod(n-1,2) + 1
               x1(i) = dpi/180/3600 * temp
               if (i.eq.2) then
                  call coCvt(lu,'ow/ow',x1,'ap/ap',x2)
                  boxes(n-1) = nint(x2(1))
                  boxes(n)   = nint(x2(2))
               endif
            endif
          else if (type.eq.'kms') then
            call coCvt1(lu,3,'aw',temp,'ap',temp2)
            boxes(n) = nint(temp2)
          endif
          k0 = k1 + 1
        endif
        if (more) k1 = k1 + 1
      enddo
c
c  Do some more checks.
c
      if (k1.gt.k2) call BoxBug(spec,'Bad subregion command')
      if (spec(k1:k1).ne.')') call BoxBug(spec,'Bad subregion command')
      k1 = k1 + 1
      if (mod(n,modulo).ne.0 .or. n.eq.0)
     *  call BoxBug(spec,'Bad number of indices in subregion command')

      end

c***********************************************************************

      subroutine BoxPoly(spec,k1,k2,verts,n,nmax,xyrange,
     *                                        xytype,lu)

      character spec*(*),xytype*(*)
      integer k1,k2,nmax,verts(2,nmax/2),n,xyrange(4),lu(3)
c-----------------------------------------------------------------------
c  Read in and process a description of a polygon region of interest.
c
c  Inputs:
c    spec       The region specification string.
c    xytype     Coordinate units.
c    lu         Information used in converting between physical and
c               grid coordinates.
c    nmax       The max size of boxes that we can handle.
c  In/Out:
c    k1,k2      Delimit the portion of the string left to process.
c  Output:
c    verts      The poly vertices.
c    n          The number of elements in the poly description.
c    xyrange    The value of xmin,xmax,ymin,ymax (in that order).
c-----------------------------------------------------------------------
      integer k,kd,t
c-----------------------------------------------------------------------
      call BoxInt(spec,k1,k2,verts,n,2,nmax-2,xytype,lu)
      n = n /2
c
c  Eliminate redundant vertices -- i.e. those vertices that are colinear
c  with the neighbouring vertices. Also add a vertex at the end to wrap
c  it around
c
      verts(1,n+1) = verts(1,1)
      verts(2,n+1) = verts(2,1)
      kd = 1
      do k = 2, n
        if ((verts(2,k+1)-verts(2,k))*(verts(1,k)-verts(1,kd)).ne.
     *      (verts(2,k)-verts(2,kd))*(verts(1,k+1)-verts(1,k))) then
          kd = kd + 1
          verts(1,kd) = verts(1,k)
          verts(2,kd) = verts(2,k)
        endif
      enddo

      n = kd
      if (n.lt.3) call BoxBug(spec,'Degenerate polygon in BoxInput')
c
c  Check if the first pixel is colinear. This cannot deal with this, and
c  craps out.
c
      if ((verts(2,2)-verts(2,1))*(verts(1,1)-verts(1,n)).eq.
     *   (verts(2,1)-verts(2,n))*(verts(1,2)-verts(1,1))) then
        verts(1,1) = verts(1,n)
        verts(2,1) = verts(2,n)
        n = n - 1
      endif

      if (n.lt.3) call BoxBug(spec,'Degenerate polygon in BoxInput')

      n = n + 1
      verts(1,n) = verts(1,1)
      verts(2,n) = verts(2,1)
c
c  Check if it is in clockwise order.
c
      t = 0
      do k = 1, n-1
        t = t + verts(1,k)*verts(2,k+1) - verts(2,k)*verts(1,k+1)
      enddo
c
c  If its clockwise, convert it to anti-clockwise.
c
      if (t.lt.0) then
        do k = 2, n/2
          kd = n - k + 1
          t = verts(1,k)
          verts(1,k) = verts(1,kd)
          verts(1,kd) = t
          t = verts(2,k)
          verts(2,k) = verts(2,kd)
          verts(2,kd) = t
        enddo
      endif
c
c  Finally, find the range of the vertices.
c
      xyrange(1) = verts(1,1)
      xyrange(2) = verts(1,1)
      xyrange(3) = verts(2,1)
      xyrange(4) = verts(2,1)
      do k = 2, n-1
        xyrange(1) = min(xyrange(1),verts(1,k))
        xyrange(2) = max(xyrange(2),verts(1,k))
        xyrange(3) = min(xyrange(3),verts(2,k))
        xyrange(4) = max(xyrange(4),verts(2,k))
      enddo
c
c  Return the goodies.
c
      n = n + n

      end

c***********************************************************************

      subroutine BoxSort(boxes,n,xyrange)

      integer n,boxes(n),xyrange(4)
c-----------------------------------------------------------------------
c  The array "boxes" contains n/4 coordinate pairs (x1,y1),(x2,y2).
c  Perhaps reverse these pairs, so that the coordinate with minimum
c  x is first, and then sort all the pairs into order of increasing
c  xmin.
c
c  Input:
c    n          Size of the box array. n/4 is the number of coordinate
c               pairs.
c  In/Out:
c    boxes      Contains the coordinate pairs.
c  Output:
c    xyrange    The minimum and maximum values of x and y in the pairs.
c
c-----------------------------------------------------------------------
      integer i,j,x1,y1,x2,y2
c-----------------------------------------------------------------------
c  Sort the box specifications into increasing order of xmin.  This
c  starts by possibly swapping the two coordinates around.  Then a
c  simple insert-sort is performed.  As n is probably quite small, an
c  insert-sort is quite adequate.
      xyrange(1) = boxes(1)
      xyrange(2) = xyrange(1)
      xyrange(3) = boxes(2)
      xyrange(4) = xyrange(3)

      do j = 1,n,4
        i = j
c
c  Get the coordinate pair. X1 has the smaller value of x.
c
        x1 = min(boxes(i ),boxes(i+2))
        x2 = max(boxes(i ),boxes(i+2))
        y1 = min(boxes(i+1),boxes(i+3))
        y2 = max(boxes(i+1),boxes(i+3))
c
c  Determine the maximum and minimum values of y.
c
        xyrange(1) = min(xyrange(1),x1)
        xyrange(2) = max(xyrange(2),x2)
        xyrange(3) = min(xyrange(3),y1)
        xyrange(4) = max(xyrange(4),y2)
c
c  Move all those coordinate pairs which should come after this pair.
c
        do while (i.gt.1 .and. boxes(i-4).gt.x1)
          boxes(i ) = boxes(i-4)
          boxes(i+1) = boxes(i-3)
          boxes(i+2) = boxes(i-2)
          boxes(i+3) = boxes(i-1)
          i = i - 4
        enddo
c
c  Put them back.
c
        boxes(i ) = x1
        boxes(i+1) = y1
        boxes(i+2) = x2
        boxes(i+3) = y2
      enddo

      end

c***********************************************************************

      subroutine BoxMsk(spec,k1,k2,tno,xyzrange)

      character spec*(*)
      integer k1,k2,tno,xyzrange(6)
c-----------------------------------------------------------------------
c  Open up a mask file, and get information about it.
c
c  Input:
c    spec       The mask-subregion command.
c  In/Out:
c    k1,k2      spec(k1:k2) contains the remainder of the file to be
c               processed.
c  Output:
c    tno        The handle of the mask file.
c    xyzrange   The region of the mask file which is set.
c
c-----------------------------------------------------------------------
      integer nsize(3)

      external  hdprsnt
      logical   hdprsnt
c-----------------------------------------------------------------------
      if (spec(k1:k1).ne.'(' .or. spec(k2:k2).ne.')' .or. k2-k1.le.1)
     *  call BoxBug(spec,'Bad or missing mask file name')
      call xyopen(tno,spec(k1+1:k2-1),'old',3,nsize)
      if (.not.hdprsnt(tno,'mask'))
     *  call BoxBug(spec,'Bad or missing mask file')
      call BoxMskPr(tno,xyzrange)

      k1 = k2 + 1

      end

c***********************************************************************
c* BoxMask -- AND in a mask to the region of interest.
c& mjs
c: region-of-interest
c+
      subroutine BoxMask(tno,boxes,maxboxes)

      integer tno,maxboxes,boxes(maxboxes)
c  ---------------------------------------------------------------------
c  Indicate that a mask is to be applied to the data.
c
c  Input:
c    tno        The handle of the input mask.
c    maxboxes   Size of the boxes array.
c  Input/Output:
c    boxes      The boxes specification.
c-----------------------------------------------------------------------
      include 'boxes.h'
      integer offset,i

      external  hdprsnt
      logical   hdprsnt
c-----------------------------------------------------------------------
      if (boxes(1).le.0) call bug('f','No input region, in BoxMask')
c
c  If there is no mask of the correct type, just ignore it. Otherwise
c  process it.
c
      if (hdprsnt(tno,'mask')) then
        offset = OFFSET0
        do i = 1, boxes(1)
          offset = offset + boxes(offset+SIZE) + HDR
        enddo
        if (offset+HDR+1.gt.maxboxes)
     *    call bug('f','Buffer overflow in BoxMask')
        boxes(1) = boxes(1) + 1
        boxes(offset+ITYPE) = -MASK
        boxes(offset+SIZE) = 1
        boxes(offset+DATA) = tno
        call BoxMskPr(tno,boxes(offset+XMIN))
      endif
      end

c***********************************************************************

      subroutine BoxMskPr(tno,xyzrange)

      integer tno,xyzrange(6)
c-----------------------------------------------------------------------
c  Get information about the masking file that we are about to use.
c
c  Input:
c    tno        The handle of the mask file.
c  Output:
c    xyzrange   The values xmin,xmax,ymin,ymax,zmin,zmax, where the
c               mask file indicates a good pixel.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer n,n1,n2,n3,j,k
      integer mask(maxdim)
      logical found
c-----------------------------------------------------------------------
c
c  Determine the dimensions of the mask.
c
      call rdhdi(tno,'naxis1',n1,1)
      call rdhdi(tno,'naxis2',n2,1)
      call rdhdi(tno,'naxis3',n3,1)
c
c  Now loop through the entire mask, determining what is the blc and trc
c  of the region selected.
c
      xyzrange(1) = n1+1
      xyzrange(2) = 0
      xyzrange(3) = n2+1
      xyzrange(4) = 0
      xyzrange(5) = n3+1
      xyzrange(6) = 0

      found = .false.
      do k = 1, n3
        call xysetpl(tno,1,k)
        do j = 1, n2
          call xymkrd(tno,j,mask,maxdim,n)
          if (n.ne.0) then
            xyzrange(1) = min(xyzrange(1),mask(1))
            xyzrange(2) = max(xyzrange(2),mask(n))
            xyzrange(3) = min(xyzrange(3),j)
            xyzrange(4) = max(xyzrange(4),j)
            xyzrange(5) = min(xyzrange(5),k)
            xyzrange(6) = max(xyzrange(6),k)
            found = .true.
          endif
        enddo
      enddo

      if (.not.found) call bug('f','Image is completely blanked')

      end

c***********************************************************************
c* BoxDef -- Set the default region of interest.
c& mjs
c: region-of-interest
c+
      subroutine BoxDef(boxes,naxis,blc,trc)

      integer naxis,blc(naxis),trc(naxis),boxes(*)
c  ---------------------------------------------------------------------
c  Set the default region of interest.
c
c  Input:
c    naxis      The dimension of blc and trc.
c    blc
c    trc
c  Input/Output:
c    boxes      This contains an intermediate form of the subregion
c               specified by the user. On output, certain defaults are
c               filled in.
c-----------------------------------------------------------------------
      include 'boxes.h'
      integer offset,i,xminv,xmaxv,yminv,ymaxv,zminv,zmaxv
c-----------------------------------------------------------------------
      xminv = blc(1)
      xmaxv = trc(1)
      if (naxis.ge.2) then
        yminv = blc(2)
        ymaxv = trc(2)
      else
        yminv = 0
        ymaxv = 0
      endif

      if (naxis.ge.3) then
        zminv = blc(3)
        zmaxv = trc(3)
      else
        zminv = 0
        zmaxv = 0
      endif

      do i = 4, naxis
        if (blc(i).ne.1 .or. trc(i).ne.1) call bug('f',
     *      'Region of interest routines inadequate!')
      enddo

      offset = OFFSET0
      if (boxes(1).eq.0) then
        boxes(1) = 1
        boxes(offset+ITYPE) = IMAGE
        boxes(offset+SIZE) = 0
        boxes(offset+XMIN) = xminv
        boxes(offset+XMAX) = xmaxv
        boxes(offset+YMIN) = yminv
        boxes(offset+YMAX) = ymaxv
        boxes(offset+ZMIN) = zminv
        boxes(offset+ZMAX) = zmaxv
      else
        do i = 1, boxes(1)
          if (boxes(offset+ITYPE).ne.QUART) then
            if (boxes(offset+XMIN).eq.0) boxes(offset+XMIN) = xminv
            if (boxes(offset+XMAX).eq.0) boxes(offset+XMAX) = xmaxv
            if (boxes(offset+YMIN).eq.0) boxes(offset+YMIN) = yminv
            if (boxes(offset+YMAX).eq.0) boxes(offset+YMAX) = ymaxv
            if (boxes(offset+ZMIN).eq.0) boxes(offset+ZMIN) = zminv
            if (boxes(offset+ZMAX).eq.0) boxes(offset+ZMAX) = zmaxv
          endif
          offset = offset + HDR + boxes(offset+SIZE)
        enddo
      endif

      end

c***********************************************************************
c* BoxSet -- Set default region of interest.
c& mjs
c: region-of-interest
c+
      subroutine BoxSet(boxes,naxis,nsize,flags)

      integer naxis,nsize(naxis),boxes(*)
      character flags*(*)
c  ---------------------------------------------------------------------
c  This checks the box specification given by the user, and applies
c  defaults.
c
c  Input:
c    naxis      The dimension of nsize, minv and maxv.
c    nsize      The dimensions of the data set.
c    flags      Character string giving some extra options.  Flags are:
c                 's'   Give warning if region-of-interest is not a
c                       regular shape.
c  Input/Output:
c    boxes      This contains an intermediate form of the subregion
c               specified by the user. On output, certain defaults are
c               filled in.
c-----------------------------------------------------------------------
      include 'boxes.h'
      integer blc(3),trc(3)
      integer i,offset
      real fx,fy
c-----------------------------------------------------------------------
      if (naxis.lt.1) call bug('f','Bad dimension')
      boxes(NX) = nsize(1)
      if (naxis.lt.2) then
        boxes(NY) = 1
      else
        boxes(NY) = nsize(2)
      endif
      if (naxis.lt.3) then
        boxes(NZ) = 1
      else
        boxes(NZ) = nsize(3)
      endif
      if (naxis.gt.3) then
        do i = 4, naxis
          if (nsize(i).gt.1) call bug('f','Can only handle 3D images')
        enddo
      endif
c
c  If the user did not give any subregion spec, fill in the default box
c  as the region.
c
      offset = OFFSET0
      if (boxes(1).eq.0) then
        boxes(1) = 1
        boxes(offset+ITYPE) = IMAGE
        boxes(offset+SIZE) = 0
        boxes(offset+XMIN) = 1
        boxes(offset+XMAX) = boxes(NX)
        boxes(offset+YMIN) = 1
        boxes(offset+YMAX) = boxes(NY)
        boxes(offset+ZMIN) = 1
        boxes(offset+ZMAX) = boxes(NZ)
      else

        do i = 1, boxes(1)
          if (boxes(offset+ITYPE).eq.QUART) then
            boxes(offset+ITYPE) = IMAGE
            boxes(offset+XMIN) = boxes(NX)/4 + 1
            boxes(offset+XMAX) = max(1,boxes(NX)/4 + boxes(NX)/2)
            boxes(offset+YMIN) = boxes(NY)/4 + 1
            boxes(offset+YMAX) = max(1,boxes(NY)/4 + boxes(NY)/2)
          endif
          if (boxes(offset+ITYPE).eq.PERC) then
            fx = boxes(offset+DATA)/100.0
            fy = boxes(offset+DATA+1)/100.0
            boxes(offset+ITYPE) = IMAGE
            boxes(offset+XMIN) = boxes(NX)*(1-fx)/2 + 1
            boxes(offset+XMAX) = max(1.,boxes(NX)*(1+fx)/2)
            boxes(offset+YMIN) = boxes(NY)*(1-fy)/2 + 1
            boxes(offset+YMAX) = max(1.,boxes(NY)*(1+fy)/2)
          endif

          if (boxes(offset+XMIN).eq.0) boxes(offset+XMIN) = 1
          if (boxes(offset+XMAX).eq.0) boxes(offset+XMAX) = boxes(NX)
          if (boxes(offset+YMIN).eq.0) boxes(offset+YMIN) = 1
          if (boxes(offset+YMAX).eq.0) boxes(offset+YMAX) = boxes(NY)
          if (boxes(offset+ZMIN).eq.0) boxes(offset+ZMIN) = 1
          if (boxes(offset+ZMAX).eq.0) boxes(offset+ZMAX) = boxes(NZ)

          if (boxes(offset+XMIN).lt.1 .or.
     *        boxes(offset+XMAX).gt.boxes(NX))
     *    call bug('f','Subregion extends beyond image on axis 1')
          if (boxes(offset+YMIN).lt.1 .or.
     *        boxes(offset+YMAX).gt.boxes(NY))
     *    call bug('f','Subregion extends beyond image on axis 2')
          if (boxes(offset+ZMIN).lt.1 .or.
     *        boxes(offset+ZMAX).gt.boxes(NZ))
     *    call bug('f','Subregion extends beyond image on axis 3')
          offset = offset + HDR + boxes(offset+SIZE)
        enddo
      endif
c
c  Check if the region of interest needs to be rectangular, and give
c  a warning message if it needs to be, and is not.
c
      if (index(flags,'s').ne.0) then
        if (boxes(1).ne.1 .or. boxes(OFFSET0+ITYPE).ne.IMAGE) then
          call bug('w','Only regular regions-of-interest supported')
          call bug('w','Using bounding box of the selected region')
          call BoxInfo(boxes,3,blc,trc)
          boxes(1) = 1
          boxes(OFFSET0+ITYPE) = IMAGE
          boxes(OFFSET0+SIZE) = 0
          boxes(OFFSET0+XMIN) = blc(1)
          boxes(OFFSET0+XMAX) = trc(1)
          boxes(OFFSET0+YMIN) = blc(2)
          boxes(OFFSET0+YMAX) = trc(2)
          boxes(OFFSET0+ZMIN) = blc(3)
          boxes(OFFSET0+ZMAX) = trc(3)
        endif
      endif
      end

c***********************************************************************
c* BoxInfo -- Determine bounding box of the region of interest.
c& mjs
c: region-of-interest
c+
      subroutine BoxInfo(boxes,naxis,blc,trc)

      integer boxes(*),naxis,blc(naxis),trc(naxis)
c  ---------------------------------------------------------------------
c  This returns the bounding box of the region of interest.
c
c  Input:
c    boxes      This contains an intermediate form of the subregion
c               specified by the user.
c
c  Output:
c    blc,trc    Min and max values of the region selected along each
c               axis.
c
c-----------------------------------------------------------------------
      include 'boxes.h'
      integer xminv,xmaxv,yminv,ymaxv,zminv,zmaxv
      integer i,offset
      logical first
c-----------------------------------------------------------------------
c  Go through the region specifications, finding the min and max values
c  of x,y and z.
c
      offset = OFFSET0
      first = .true.
      do i = 1, boxes(1)
c
c  Handle the first subregion. This must be ORed with whatever.
c
        if (first) then
          if (boxes(offset+ITYPE).gt.0) then
            xminv = boxes(offset+XMIN)
            xmaxv = boxes(offset+XMAX)
            yminv = boxes(offset+YMIN)
            ymaxv = boxes(offset+YMAX)
            zminv = boxes(offset+ZMIN)
            zmaxv = boxes(offset+ZMAX)
            first = .false.
          endif
c
c  Handle other than the first, which is ORed.
c
        else if (boxes(offset+ITYPE).gt.0) then
          xmaxv = max(xmaxv,boxes(offset+XMAX))
          xminv = min(xminv,boxes(offset+XMIN))
          ymaxv = max(ymaxv,boxes(offset+YMAX))
          yminv = min(yminv,boxes(offset+YMIN))
          zmaxv = max(zmaxv,boxes(offset+ZMAX))
          zminv = min(zminv,boxes(offset+ZMIN))
c
c  Handle other than the first, which is ANDed.
c
        else
          xmaxv = min(xmaxv,boxes(offset+XMAX))
          xminv = max(xminv,boxes(offset+XMIN))
          ymaxv = min(ymaxv,boxes(offset+YMAX))
          yminv = max(yminv,boxes(offset+YMIN))
          zmaxv = min(zmaxv,boxes(offset+ZMAX))
          zminv = max(zminv,boxes(offset+ZMIN))
        endif

        offset = offset + HDR + boxes(offset+SIZE)
      enddo

      if (first) call bug('f','No subregion selected')
      if (xmaxv.lt.xminv .or. ymaxv.lt.yminv .or. zmaxv.lt.zminv)
     *          call bug('f','No subregion selected')
c
c  Return the info we have to the caller.
c
      blc(1) = xminv
      trc(1) = xmaxv
      if (naxis.ge.2) then
        blc(2) = yminv
        trc(2) = ymaxv
      endif
      if (naxis.ge.3) then
        blc(3) = zminv
        trc(3) = zmaxv
      endif

      do i = 4, naxis
        blc(i) = 1
        trc(i) = 1
      enddo
      end
c***********************************************************************
c* BoxRect -- Determine if a region-of-interest is rectangular.
c& mjs
c: region-of-interest
c+
      logical function BoxRect(boxes)

      integer boxes(*)
c  ---------------------------------------------------------------------
c  This returns .true. if the region described by the boxes array is
c  purely rectangular (i.e. describable by blc and trc).
c
c  Input:
c    boxes      Integer array describing intermediate form of the region
c               selected.
c  Output:
c    BoxRect    True if the region is rectangular.
c-----------------------------------------------------------------------
      include 'boxes.h'
      integer i,offset
      logical first,rect
c
c  Go through the region specifications, finding the min and max values
c  of x,y and z.
c
      offset = OFFSET0
      first = .true.
      do i = 1, boxes(1)
        if (first) then
          if (boxes(offset+ITYPE).gt.0) then
            first = .false.
            rect = boxes(offset+ITYPE).eq.IMAGE
          endif
        else
          rect = .false.
        endif

        offset = offset + HDR + boxes(offset+SIZE)
      enddo

      BoxRect = rect

      end

c***********************************************************************
c* BoxBound -- Return bounding boxes for each "region" subcommand.
c& mjs
c: region-of-interest
c+
      subroutine BoxBound(boxes,subcmd,naxis,type,mode,blc,trc)

      integer boxes(*),subcmd,naxis
      character type*(*),mode*(*)
      integer blc(naxis),trc(naxis)
c  ---------------------------------------------------------------------
c  Return the bounding box for each region subcommand.
c
c  Input:
c    boxes      The boxes array.
c    subcmd     The subcommand number.
c    naxis      The dimensionality of blc and trc.
c  Output:
c    type       Either 'box','poly' or 'mask'.
c    mode       Either 'or' or 'and'.
c    blc        Blc of the bounding box.
c    trc        Trc of the bounding box.
c-----------------------------------------------------------------------
      include 'boxes.h'
      integer i,offset,btype
c-----------------------------------------------------------------------
c  Handle the case where we are asked for a non-existent subcommand.
      if (subcmd.gt.boxes(1)) then
        type = ' '
      else
c
c  Go through the region specifications to find the required shape.
c  of x,y and z.
c
        offset = OFFSET0
        do i = 1, subcmd-1
          offset = offset + HDR + boxes(offset+SIZE)
        enddo
        btype = boxes(offset+ITYPE)
        if (btype.gt.0) then
          mode = 'or'
        else
          mode = 'and'
        endif
        btype = abs(btype)
        if (btype.eq.IMAGE .or. btype.eq.BOX) then
          type = 'box'
        else if (btype.eq.POLY) then
          type = 'poly'
        else if (btype.eq.MASK) then
          type = 'mask'
        else
          call bug('f','Unrecognised region subcommand, in BoxBound')
        endif
        blc(1) = boxes(offset+XMIN)
        trc(1) = boxes(offset+XMAX)
        if (naxis.ge.2) then
          blc(2) = boxes(offset+YMIN)
          trc(2) = boxes(offset+YMAX)
        endif
        if (naxis.ge.3) then
          blc(3) = boxes(offset+ZMIN)
          trc(3) = boxes(offset+ZMAX)
        endif
        do i = 4, naxis
          blc(i) = 1
          trc(i) = 1
        enddo
      endif

      end

c***********************************************************************
c* BoxCount -- Count the pixels in the region-of-interest in a plane.
c& mjs
c: region-of-interest
c+
      subroutine BoxCount(Runs,nRuns,nPoint)

      integer nRuns,Runs(3,nRuns+1)
      ptrdiff nPoint
c  ---------------------------------------------------------------------
c  Count the number of pixels in the region-of-interest in a given
c  plane.
c
c  Input:
c    runs       Runs specifications, as returned by BoxRuns.
c    nruns      Number of runs.
c  Output:
c    nPoint     Number of pixels in the region of interest.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      nPoint = 0
      do i = 1, nRuns
        nPoint = nPoint + Runs(3,i) - Runs(2,i) + 1
      enddo

      end

c***********************************************************************
c* BoxRuns -- Return region of interest in "runs" form.
c& mjs
c: region-of-interest
c+
      subroutine BoxRuns(naxis,plane,flags,boxes,
     *                runs,maxruns,nruns,xminv,xmaxv,yminv,ymaxv)

      character flags*(*)
      integer naxis,plane(naxis)
      integer boxes(*),maxruns,nruns,runs(3,maxruns)
      integer xminv,xmaxv,yminv,ymaxv
c  ---------------------------------------------------------------------
c  This returns the caller a runs specification for the particular plane
c  that the user is currently interested in.
c
c  Input:
c    naxis      The dimension of "plane".
c    plane      The index, along the third, fourth, etc dimension of the
c               cube to be accessed.
c    flags      A string. Each character indicates an extra step to
c               perform.  Currently there is only one possibility:
c                 'r' Make runs specification relative to xminv,yminv.
c    boxes      Some intermediate form of the boxes specified by the
c               user.
c    maxruns    Max number of runs that this program can cope with.
c
c  Output:
c    runs       Runs specifications.
c    nruns      Number of runs.
c    xminv,yminv)       Minimum regions containing the area selected.
c    xmaxv,ymaxv)
c-----------------------------------------------------------------------
      include 'boxes.h'
      include 'maxdim.h'
      integer WORKSIZE,MAXSHAPE
      parameter (WORKSIZE=MAXDIM,MAXSHAPE=1024)
      integer n1,n2,n3,pnt1,pnt2,pnt3,i,j,k,boxtype
      integer offset,jmin,jmax,nshapes,shapes(MAXSHAPE)
      integer work(WORKSIZE,3)
c-----------------------------------------------------------------------
c  This routine can only handle 3D objects. Check that the higher
c  diemsions are all 1.
c
      if (naxis.gt.1) then
        do i = 2, naxis
          if (plane(i).ne.1) call bug('f','Can only handle 3D images')
        enddo
      endif
c
c  Determine the subcommands that are appropriate to this plane.  If
c  there are no commands, return straight away.
c
      nshapes = 0
      offset = OFFSET0
      do i = 1, boxes(1)
        if (((boxes(offset+ZMIN).le.plane(1) .and.
     *      boxes(offset+ZMAX).ge.plane(1)) .or.
     *      (boxes(offset+ITYPE).lt.0 .and. nshapes.gt.0)) .and.
     *    (boxes(offset+ITYPE).gt.0 .or. nshapes.gt.0)) then
          nshapes = nshapes + 1
          if (nshapes.gt.MAXSHAPE)
     *      call bug('f','Buffer overflow in BoxRuns(nshapes)')
          shapes(nshapes) = offset
          if (boxes(offset+ITYPE).gt.0) then
            if (nshapes.eq.1) then
              jmin = boxes(offset+YMIN)
              jmax = boxes(offset+YMAX)
            else
              jmin = min(jmin,boxes(offset+YMIN))
              jmax = max(jmax,boxes(offset+YMAX))
            endif
          else
            jmin = max(jmin,boxes(offset+YMIN))
            jmax = min(jmax,boxes(offset+YMAX))
          endif
        endif
        offset = offset + HDR + boxes(offset+SIZE)
      enddo

      nruns = 0
      runs(1,1) = 0
      if (nshapes.eq.0) return
c
c  Determine the runs appropriate to this plane.  This gets the runs
c  from each relavant shape, then merges it with the runs from previous
c  shapes.  The previous runs are in one buffer (pointed to by pnt1),
c  the current runs are written in another buffer (pointed to by pnt2),
c  and the merged result is written to a third buffer (pointed to by
c  pnt3).  Pnt1 and pnt2 vary cyclicly.
c
      do j = jmin, jmax
        n1 = 0
        pnt1 = 1
        do k = 1, nshapes
          pnt2 = mod(pnt1,3) + 1
          offset = shapes(k)
          boxtype = boxes(offset+ITYPE)
          if (boxes(offset+YMIN).le.j .and.
     *        boxes(offset+YMAX).ge.j .and.
     *        (n1.gt.0 .or. boxtype.gt.0)) then
            n2 = 0
            if (abs(boxtype).eq.IMAGE) then
              work(1,pnt2) = boxes(offset+XMIN)
              work(2,pnt2) = boxes(offset+XMAX)
              n2 = 2
            else if (abs(boxtype).eq.BOX) then
              call BoxBoxX(work(1,pnt2),WORKSIZE,j,
     *                boxes(offset+SIZE)/4,boxes(offset+DATA),n2)
            else if (abs(boxtype).eq.POLY) then
              call BoxPolyX(work(1,pnt2),WORKSIZE,j,
     *                boxes(offset+SIZE)/2,boxes(offset+DATA),n2)
            else if (abs(boxtype).eq.MASK) then
              call BoxMskX(work(1,pnt2),WORKSIZE,j,
     *                plane(1),boxes(offset+DATA),n2)
            else
              call bug('f','Boxes structure is corrupt in BoxRuns')
            endif
c
c  Merge the specs from multiple shapes together.
c
            if (boxtype.gt.0) then
              if (n1.le.0) then
                pnt1 = pnt2
                n1 = n2
              else
                pnt3 = mod(pnt2,3) + 1
                call BoxOr(n1,work(1,pnt1),n2,work(1,pnt2),
     *                        n3,work(1,pnt3),WORKSIZE)
                n1 = n3
                pnt1 = pnt3
              endif
            else if (n1.le.0 .or. n2.le.0) then
              n1 = 0
            else
              pnt3 = mod(pnt2,3) + 1
              call BoxAnd(n1,work(1,pnt1),n2,work(1,pnt2),
     *                        n3,work(1,pnt3),WORKSIZE)
              n1 = n3
              pnt1 = pnt3
            endif
          endif
        enddo
c
c  Copy the runs to the output buffer.
c
        if (n1.gt.0) then
          if (nruns.eq.0) then
            xminv = work(1,pnt1)
            xmaxv = work(n1,pnt1)
            yminv = j
          else
            xminv = min(xminv,work(1,pnt1))
            xmaxv = max(xmaxv,work(n1,pnt1))
          endif
          ymaxv = j
          if (maxruns-nruns.lt.n1/2+1)
     *        call bug('f','Buffer overflow in BoxRuns(output)')
          do i = 1,n1,2
            nruns = nruns+1
            runs(1,nruns) = j
            runs(2,nruns) = work(i,pnt1)
            runs(3,nruns) = work(i+1,pnt1)
          enddo
        endif
      enddo
c
c  We are in the home stretch now. Convert things to indices relative to
c  (xmin,ymin).
c
      if (index(flags,'r').ne.0) then
        do i = 1, nruns
          runs(1,i) = runs(1,i) - yminv + 1
          runs(2,i) = runs(2,i) - xminv + 1
          runs(3,i) = runs(3,i) - xminv + 1
        enddo
      endif
c
c  Append a trailing "null".
c
      runs(1,nruns+1) = 0

      end

c***********************************************************************

      subroutine BoxAnd(n1,in1,n2,in2,nout,out,maxout)

      integer n1,n2,nout,maxout
      integer in1(n1),in2(n2),out(maxout)
c-----------------------------------------------------------------------
c  "AND" together two runs specifications.
c
c  Input:
c    n1,n2      Sizes of the two input runs.
c    in1,in2    The two input runs.
c    maxout     Size of the output array.
c
c  Output:
c    out        Output buffer to receive the runs.
c    nout       The size of the output runs.
c
c-----------------------------------------------------------------------
      integer i1,i2,io,t1,t2
c-----------------------------------------------------------------------
      i1 = 1
      i2 = 1
      io = 1
c
c  And the two.
c
      do while (i1.lt.n1 .and. i2.lt.n2)
        t1 = max(in1(i1),in2(i2))
        t2 = min(in1(i1+1),in2(i2+1))
        if (t1.le.t2) then
          if (io.gt.maxout)
     *      call bug('f','Buffer overflow in BoxRuns(and)')
          out(io) = t1
          out(io+1) = t2
          io = io + 2
        endif

        if (in1(i1+1).lt.in2(i2+1)) then
          i1 = i1 + 2
        else
          i2 = i2 + 2
        endif
      enddo
c
c  Return the size of the output.
c
      nout = io - 1

      end

c***********************************************************************

      subroutine BoxOr(n1,in1,n2,in2,nout,out,maxout)

      integer n1,n2,nout,maxout
      integer in1(n1),in2(n2),out(maxout)
c-----------------------------------------------------------------------
c  Merge two runs specifications together.
c
c  Input:
c    n1,n2      Sizes of the two input runs.
c    in1,in2    The two input runs.
c    maxout     Size of the output array.
c
c  Output:
c    out        Output buffer to receive the runs.
c    nout       The size of the output runs.
c
c-----------------------------------------------------------------------
      integer i1,i2,io,i,xmax
c-----------------------------------------------------------------------
      i1 = 1
      i2 = 1
      io = 1
      xmax = -1
c
c  Merge the two.
c
      do while (i1.lt.n1 .and. i2.lt.n2)
        if (in1(i1).lt.in2(i2)) then
          if (in1(i1)-xmax.le.1) then
            out(io-1) = max(in1(i1+1),xmax)
          else
            if (io.gt.maxout)
     *        call bug('f','Buffer overflow in BoxRuns(or)')
            out(io) = in1(i1)
            out(io+1) = in1(i1+1)
            io = io + 2
          endif
          i1 = i1 + 2
        else
          if (in2(i2)-xmax.le.1) then
            out(io-1) = max(in2(i2+1),xmax)
          else
            if (io.gt.maxout)
     *        call bug('f','Buffer overflow in BoxRuns(or)')
            out(io) = in2(i2)
            out(io+1) = in2(i2+1)
            io = io + 2
          endif
          i2 = i2 + 2
        endif
        xmax = out(io-1)
      enddo
c
c  Copy whichever one is remaining.
c
      if (i1.lt.n1) then
        do i = i1,n1,2
          if (in1(i)-xmax.le.1) then
            out(io-1) = max(in1(i+1),xmax)
          else
            if (io.gt.maxout)
     *        call bug('f','Buffer overflow in BoxRuns(or)')
            out(io) = in1(i)
            out(io+1) = in1(i+1)
            io = io + 2
          endif
          xmax = out(io-1)
        enddo
      else if (i2.lt.n2) then
        do i = i2,n2,2
          if (in2(i)-xmax.le.1) then
            out(io-1) = max(in2(i+1),xmax)
          else
            if (io.gt.maxout)
     *        call bug('f','Buffer overflow in BoxRuns(or)')
            out(io) = in2(i)
            out(io+1) = in2(i+1)
            io = io + 2
          endif
          xmax = out(io-1)
        enddo
      else
        call bug('f','Algorithmic bug in BoxOr')
      endif
c
c  Return the size of the output.
c
      nout = io - 1

      end

c***********************************************************************

      subroutine BoxMskX(buf,nbuf,y,z,tno,nout)

      integer nbuf,buf(nbuf),y,z,tno,nout
c-----------------------------------------------------------------------
      call xysetpl(tno,1,z)
      call xymkrd(tno,y,buf,nbuf,nout)

      end

c***********************************************************************

      subroutine BoxBoxX(goes,maxgoes,j0,nbox,box,ngoes)

      integer maxgoes,goes(maxgoes),j0,nbox,box(4,nbox),ngoes
c-----------------------------------------------------------------------
c  This breaks up the specification of a number of boxes into runs for
c  row j.
c
c  Input:
c    maxgoes
c    j0
c    box
c    nbox
c  Output:
c    ngoes
c    goes
c-----------------------------------------------------------------------
      integer k,xmax
c-----------------------------------------------------------------------
      xmax = -1
      ngoes = 0
      do k = 1, nbox
        if ((j0-box(2,k))*(box(4,k)-j0).ge.0) then
          if (box(1,k).gt.xmax+1) then
            if (ngoes+2.gt.maxgoes)
     *        call bug('f','Buffer overflow in BoxRuns(polyx)')
            goes(ngoes+1) = box(1,k)
            goes(ngoes+2) = box(3,k)
            ngoes = ngoes + 2
          else
            goes(ngoes) = max(goes(ngoes),box(3,k))
          endif
          xmax = goes(ngoes)
        endif
      enddo

      end

c***********************************************************************

      subroutine BoxPolyX(goes,maxgoes,j0,nverts,verts,ngoes)

      integer maxgoes,goes(maxgoes),j0,nverts,verts(2,nverts),ngoes
c-----------------------------------------------------------------------
c  Calculate the runs which lie within a polygon. It does this by
c  calculating the intersections of a horizontal line with the polygon,
c  then sorting the intersections. There should be an even number of
c  intersections.
c  An added complication is the intersection of the horizontal line with
c  a vertex. Three situations are possibilities: go through a vertex
c  into the interior of the poly, clip a vertex, or
c  go along the edge of the poly. The first case counts as
c  one intersection, the second as two, and the third counts as 0 or 1
c  depending whether we are entering or leaving the selected area.
c
c    /          ---^---            _________
c   /_____        / \             /
c   \            /   \           /
c    \          /     \         /
c
c  Input:
c    nverts     Number of veritces of the polygon.
c    verts      The vertices of the polygon. The vertices are assumes to
c               have no redundancies (i.e. all vertices are distinct),
c               and to trace out a anti-clockwise path.
c    j0         The value of y for which we want to determine the runs
c               inside the polygon.
c    maxgoes    Max number of runs is maxgoes/2.
c  Output:
c    goes       The runs for this value of y.
c    ngoes      The number of runs is ngoes/2.
c-----------------------------------------------------------------------
      integer k,kprev,l,t
      logical more
c-----------------------------------------------------------------------
      ngoes = 0
      kprev = nverts-1
      do k = 1,nverts-1,1
c
c  Case of an intersection with a vertex.
c
        if (verts(2,k).eq.j0) then
          t = (j0-verts(2,kprev))*(j0-verts(2,k+1))
          if (t.gt.0) then
            ngoes = ngoes + 2
            goes(ngoes-1) = verts(1,k)
            goes(ngoes)   = verts(1,k)
          else if (t.lt.0) then
            ngoes = ngoes + 1
            goes(ngoes) = verts(1,k)
          else
            t =   verts(1,kprev)*(verts(2,k)    -verts(2,k+1)  )
     *          + verts(1,k)    *(verts(2,k+1)  -verts(2,kprev))
     *          + verts(1,k+1)  *(verts(2,kprev)-verts(2,k)    )
            if (t.gt.0) then
              ngoes = ngoes + 1
              goes(ngoes) = verts(1,k)
            endif
          endif
c
c  Case of an intersection with the line segment between vertices.
c
        else if ((j0-verts(2,k))*(verts(2,k+1)-j0).gt.0) then
          ngoes = ngoes + 1
          goes(ngoes) =  nint(verts(1,k+1) +
     *        real((j0-verts(2,k+1)) * (verts(1,k)-verts(1,k+1)))
     *                / (verts(2,k)-verts(2,k+1)))
        endif
        kprev = k
      enddo

      if (2*(ngoes/2).ne.ngoes)
     *  call bug('f','Algorithmic failure in BoxRuns(polyx)')
c
c  The list of intersections are not in order.  The number of
c  intersections is also likely to be small (probably only two!).  Sort
c  the intersections, but use an insert-sort, because its probably
c  ordered, and small.
c
      do k = 2, ngoes
        l = k
        t = goes(l)
        more = goes(l-1).gt.t
        do while (more)
          goes(l) = goes(l-1)
          l = l - 1
          more = .false.
          if (l.gt.1) more = goes(l-1).gt.t
        enddo
        goes(l) = t
      enddo
c
c  There are possibly redundancies in the list of runs. Eliminate these.
c
      l = 3
      do k = 3,ngoes,2
        if (goes(k)-goes(l-1).le.1) then
          goes(l-1) = goes(k+1)
        else
          goes(l) = goes(k)
          goes(l+1) = goes(k+1)
          l = l + 2
        endif
      enddo
      ngoes = l-1

      end

c***********************************************************************

      subroutine BoxBug(spec,message)

      character message*(*), spec*(*)
c-----------------------------------------------------------------------
c  Generate an error message and die.
c
c  Input:
c    spec       The subregion command.
c    message    The error message.
c-----------------------------------------------------------------------
      integer   l
      character line*80

      external  len1
      integer   len1
c-----------------------------------------------------------------------
      l = len1(spec)
      line = message//': '//spec(1:l)
      l = min(len(line),l + 2 + len(message))
      call bug('f',line(1:l))

      end
