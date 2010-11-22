      program imsub

c= imsub - Extract subportion of an image
c& mchw
c: map manipulation
c+
c       IMSUB is a MIRIAD task which extracts a subportion of an image.
c@ in
c       The input image. No default.
c@ out
c       The output image. No default.
c@ region
c       The region of interest. The default is the entire input image.
c       See the Users Manual for instructions on how to specify this.
c@ incr
c       Increment to be used along each axis. Default is 1.
c
c$Id: imsub.for,v 1.5 2010/11/22 04:59:30 cal103 Exp $
c--
c  History:
c    rjs Dark-ages Original version.
c    rjs 16jul89   Added incr parameter.
c    nebk 11aug89  Add incr parameter to history.  This is my lot in
c                  life.
c    rjs  14sep89  Fixed nebk's addition to history.  Is this my lot in
c                  life?
c    rjs  24apr90  Copied across lstart,lstep,ltype,lwidth keywords.
c    rjs  30apr90  Changed call sequence to BoxInput.
c    mchw 20jun90  Copied across bmaj,bmin,bpa keywords.
c    mchw 09nov90  Copied across pbfwhm keyword.
c    mjs  25feb91  Changed references of mitoa to mitoaf.
c    rjs   8oct91  Updated history writing somewhat.
c    rjs   7may92  Apply mask after determining region to copy.
c    nebk 26may92  Copy "btype" across
c    pjt   9jul92  Length of filenames to be 80, not 24!!!!
c    rjs  25jun93  Trivial change to a history message.
c    rjs   8nov94  Get incr and blanking masks to work correctly.
c    rjs  17aug95  Copy mostable to output.
c    rjs  21nov95  Use maxnax.h.
c    nebk 10jan96  Change crpix and cdelt to double
c    rjs  02jul97  cellscale change.
c    rjs  18jul97  Correct handling of flags when incr != 1.
c    rjs  23jul97  added pbtype.
c    rjs  25nov98  added llrot.
c    rjs  23jul99  Correct copying of pbtype keyword.
c    rjs  12oct99  Correct copying of mostable.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      integer MAXBOXES
      parameter (MAXBOXES = 2048)

      logical   done, rect
      integer   blc(MAXNAX), boxes(MAXBOXES), i, i0, incr(MAXNAX),
     *          inPlane(MAXNAX), j, j0, lIn, lOut, naxis, nIn(MAXNAX),
     *          nOut(MAXNAX), npnt, nx, ny, one(MAXNAX),
     *          outPlane(MAXNAX), trc(MAXNAX)
      real      mapIn(MAXDIM), mapOut(MAXDIM)
      double precision cdelt, crpix
      character inName*80, keyw*8, outNam*80, version*90

      logical   BoxRect, hdprsnt
      character itoaf*1, versan*80
      external  BoxRect, hdprsnt, itoaf, versan
c-----------------------------------------------------------------------
      version = versan('imsub',
     *                 '$Revision: 1.5 $',
     *                 '$Date: 2010/11/22 04:59:30 $')

c     Get the input parameters.
      call keyini
      call keya('in',inName,' ')
      call keya('out',outNam,' ')

      if (inName.eq.' ' .or. outNam.eq.' ')
     *  call bug('f','You must give an input and output file')
      call BoxInput('region',inName,boxes,MAXBOXES)
      do i = 1, MAXNAX
        call keyi('incr',incr(i),1)
        if (incr(i).le.0) call bug('f','Bad increment')
      enddo
      call keyfin

c     Open the input.
      call xyopen(lIn,inName,'old',MAXNAX,nIn)
      if (nIn(1).gt.MAXDIM)
     *  call bug('f','Image too big for me to handle')
      call rdhdi(lIn,'naxis',naxis,0)
      call BoxSet(boxes,MAXNAX,nIn,' ')

c     Determine portion of image to copy.
      call BoxInfo(boxes,MAXNAX,blc,trc)
      call BoxMask(lIn,boxes,MAXBOXES)
      rect = BoxRect(boxes)
      do i = 1, MAXNAX
        nOut(i) = (trc(i) - blc(i) + incr(i))/incr(i)
      enddo

c     Create the output image.
      call xyopen(lOut,outNam,'new',naxis,nOut)

c     Start with a verbatim copy of the input header.
      call headcp(lIn, lOut, 0, 0, 0, 0)

c     Update changed keywords.
      do i = 1, naxis
        keyw = 'crpix' // itoaf(i)
        call rdhdd(lIn, keyw, crpix, 1d0)
        crpix = (crpix - blc(i))/incr(i) + 1d0
        call wrhdd(lOut,keyw, crpix)

        keyw = 'cdelt' // itoaf(i)
        call rdhdd(lIn, keyw, cdelt, 1d0)
        cdelt = cdelt * incr(i)
        call wrhdd(lOut,keyw, cdelt)
      enddo

      call hdcopy(lIn, lOut, 'rms')

c     Update history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'IMSUB: Miriad ' // version)
      call hisinput(lOut, 'IMSUB')
      call hisclose(lOut)

c     Initialise the plane indices.
      do i = 3, MAXNAX
        one(i-2) = 1
        inPlane(i-2) = blc(i)
        outPlane(i-2) = 1
      enddo

c     Make a copy of the portion being copied.
      done = .false.
      do while (.not.done)
        call xysetpl(lIn,MAXNAX-2,inPlane)
        call xysetpl(lOut,MAXNAX-2,outPlane)
        j0 = blc(2)
        do j = 1, nOut(2)
          call xyread(lIn,j0,mapIn)
          j0 = j0 + incr(2)
          if (incr(1).gt.1) then
            i0 = blc(1)
            do i = 1, nOut(1)
              mapOut(i) = mapIn(i0)
              i0 = i0 + incr(1)
            enddo
            call xywrite(lOut,j,mapOut)
          else
            call xywrite(lOut,j,mapIn(Blc(1)))
          endif
        enddo

        if (.not.rect) then
          call MaskIt(lOut, MAXNAX-2, inPlane, boxes, blc(1), blc(2),
     *                nOut(1), nOut(2), incr(1), incr(2))
        endif

        call planeinc(MAXNAX-2,incr(3),blc(3),trc(3),inPlane,done)
        call planeinc(MAXNAX-2,one,one,nOut(3),outPlane,done)
      enddo

c     Copy the mosaic table if neccessary.
      if (hdprsnt(lIn,'mostable')) then
        if (incr(1).ne.1 .or. incr(2).ne.1) then
          call mosLoad(lIn,npnt)
          call mosGetn(nx,ny,npnt)
          call mosSetn(nx/incr(1),ny/incr(2))
          call mosSave(lOut)
        else
          call hdcopy(lIn,lOut,'mostable')
        endif
      endif

      call xyclose(lIn)
      call xyclose(lOut)

      end

c***********************************************************************

      subroutine MaskIt(tno,ndim,plane,boxes,x0,y0,n1,n2,inc1,inc2)

      integer tno,ndim,plane(ndim),boxes(*),x0,y0,n1,n2,inc1,inc2

c  Get the mask to apply to the output, and write it out.
c
c  Input:
c    tno        Handle of the output file.
c    nsize      Gives the dimension of "plane".
c    plane      The array of indices determining which plane we are
c               processing.
c    boxes      The intermediate form of the region specification.
c    x0,y0      The blc corner of the output.
c    n2         Size of the output along the y axis.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer maxRuns
      parameter (maxRuns = 8*MAXDIM)
      integer xminv,xmaxv,yminv,ymaxv,nRuns,nRuns2,iRuns
      integer x1,x2,y,xprev,yprev,t
      integer Runs(3,maxRuns)
c-----------------------------------------------------------------------
c     Get the information about what in the output is good.
      call BoxRuns(ndim,plane,' ',boxes,
     *        Runs,maxRuns,nRuns,xminv,xmaxv,yminv,ymaxv)

c     If we don't need to decimate, write it straight out.
c     Otherwise work out the new runs array.
      if (inc1.eq.1 .and. inc2.eq.1) then
        call PutRuns(tno,Runs,nRuns,1-x0,1-y0,n1,n2)
      else
        xprev = 0
        yprev = 0
        nRuns2 = 0
        do iRuns = 1, nRuns
          y = Runs(1,iRuns) - y0
          if (y.ge.0 .and. y.le.(n2-1)*inc2 .and. mod(y,inc2).eq.0) then
            x1 = max(Runs(2,iRuns),x0)
            x1 = inc1*((x1 - x0 + inc1 - 1)/inc1)
            x2 = min(Runs(3,iRuns)-x0,(n1-1)*inc1)
            if (x1.le.x2) then
              y = y/inc2 + 1
              t = x1/inc1 + 1
              x2 = t + (x2-x1+1)/inc1
              x1 = t
              if (y.eq.yprev .and. x1.eq.xprev) then
                Runs(3,nRuns2) = x2
              else
                nRuns2 = nRuns2 + 1
                Runs(1,nRuns2) = y
                Runs(2,nRuns2) = x1
                Runs(3,nRuns2) = x2
              endif
              xprev = x2 + 1
              yprev = y
            endif
          endif
        enddo
        call PutRuns(tno,Runs,nRuns2,0,0,n1,n2)
      endif

      end

c***********************************************************************

      subroutine planeinc(n,incr,blc,trc,plane,done)

      integer n,blc(n),trc(n),plane(n),incr(n)
      logical done

c  Move to the next plane.
c
c-----------------------------------------------------------------------
      integer k
c-----------------------------------------------------------------------
      k = 1
      done = .true.

      do while (done .and. k.le.n)
        done = plane(k).ge.trc(k)
        if (done) then
          plane(k) = blc(k)
        else
          plane(k) = plane(k) + incr(k)
        endif
        k = k + 1
      enddo

      end
