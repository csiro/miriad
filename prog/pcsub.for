      program pcsub

c= pcsub - Subtract a plane (optional by polynomial fit) from a cube
c& pjt
c: map combination
c+
c       PCSUB (Polynomial Continuum Subtraction) is a MIRIAD task that
c       does a polynomial least squares fit to selected planes of a cube
c       to make a continuum map that can be subtracted from the input
c       cube.  Alternatively, if a continuum map is already known, this
c       can be subtracted.
c
c       Limitations:
c        1) only designed for 3-D images
c        2) the region input parameter should be specified only
c           with the IMAGE command, e.g.,
c
c           region=image(1,5),image(125,128)
c
c           would average and subtract planes 1:5 and 125:128
c           any specified x-y sub-regions will be ignored
c        3) Blanked pixels in the image are only partially handled.
c           If a plane is completely blank then it is ignored, however,
c           a plane that is partially blanked is not dealt with.
c        4) No fit to a polynomial greater than 10 is permitted.
c
c@ in
c       The input image dataset. No default.
c@ out
c       The output image.  For default see cont= keyword.
c@ order
c       The order of the highest term of the polynomial fit.
c       Default is 0, i.e. a constant.
c@ region
c       Specify the channels to fit with a command of the region of
c       interest format, e.g.
c         region=image(1,5),image(120,128)
c       This would fit and subtract channels 1:5 and 120:128 from the
c       cube.  Note that blanked planes are correctly ignored, but
c       isolated blanked pixels in a plane are not.
c@ cont
c       If "out" is provided, the continuum is generated from the region
c       (channels) selected.  This continuum (cont=) is subtracted from
c       each of the channels of the cube (in=) to yield an output cube
c       (out=).
c       If "out" alone is provided, this is all that occurs.
c       If "cont" is also provided, the continuum map is also written to
c       the file specified, if that file did not exist yet.
c       If "cont" did already exist, it is assumed to be the continuum
c       map, and subtracted from the input cube.
c       If "cont" alone is provided, ONLY the continuum map is written,
c       no output cube is written.
c
c$Id: pcsub.for,v 1.4 2010/11/22 05:31:43 cal103 Exp $
c--
c  History:
c    nebk  2may89 Original version. (AVSUB.FOR)
c    rjs  30may90 Changed call sequence to BoxInput. (AVSUB.FOR)
c    dec  28jun90 Changed to handle greater size cubes, and to output a
c                 continuum map (new keyword - cont).  Altered keya's to
c                 keyf's in the initial key reads.  Increased max
c                 filename sizes. (CSUB.FOR)
c    dec  11jul90 Introduced capability to fit to any degree polynomial,
c                 up to degree 10. (PCSUB.FOR)
c    dec  14aug90 Changed from svdfit subroutine to linpack routines.
c                 Changed method of copy of header to the imframe
c                 routine.
c    pjt  21jan91 conformed to new inline doc format
c    pjt  29apr91 add check if cont= exists, itoa->itoaf
c    pjt  21jul91 make it work when cont= exists
c    pjt  10mar92 consistent MAXRUNS length
c    nebk 25nov92 Copy btype to output
c    mjs  27feb93 use tmpdim.h instead of maxdim.h
c    rjs  08may00 Change incorrect call of keyf to keya.
c-----------------------------------------------------------------------
      include 'tmpdim.h'

      integer MAXBOXES, MAXNAX, MAXRUNS, MAXEXP
      integer MAXPOWER, MPPLUS1
      parameter (MAXBOXES = 1024, MAXRUNS = 3*MAXDIM, MAXNAX = 3)
      parameter (MAXPOWER = 10, MAXEXP = 3, MPPLUS1 = MAXPOWER+1)

      logical   ccreate, more
      integer   a(MAXPOWER+1), b(MPPLUS1+MPPLUS1), blc(MAXNAX),
     *          boxes(MAXBOXES), cnin(MAXNAX), i, iend(MAXCHAN), iostat,
     *          isnext, istart(MAXCHAN), j, k, lcont, lin, lout, naxis,
     *          nin(MAXNAX), nplanes, nruns, nsect, nterms,
     *          planes(MAXCHAN), runs(3,MAXRUNS), trc(MAXNAX), xblc,
     *          xtrc, yblc, ytrc
      real      inplanes, contin(MAXDIM), rline(MAXDIM)
      double precision bm(MAXPOWER+1,MAXPOWER+1), c(MAXPOWER+1),
     *          cm(MAXDIM,MAXPOWER+1), rope(MAXDIM*MAXCHAN), y(MAXDIM)
      character aline*72, cont*128, in*128, out*128, version*72

      character versan*80
      external  versan
c-----------------------------------------------------------------------
      version = versan('pcsub',
     *                 '$Revision: 1.4 $',
     *                 '$Date: 2010/11/22 05:31:43 $')
      call bug('w','Use this program only in EXISTING cont= mode')

c     Get the input parameters.
      call keyini
      call keyf('in', in, ' ')
      call keya('out', out, ' ')
      call keya('cont', cont, ' ')
      if (in.eq.' ') call bug('f',
     *    'You must specify an input dataset (in=)')
      if (out.eq.' ' .and. cont.eq.' ') call bug('f',
     *    'Some form of output image is needed, i.e. out= or cont=')
      call boxinput('region', in, boxes, MAXBOXES)
      call keyi('order', nterms, 0)
      if (nterms.gt.MAXPOWER) call bug('f',
     *    'Order too high; order=')
      nterms = nterms + 1
      call keyfin

c     Open the input image and pass some information to the box routines
c     about the region to work on.
      call xyopen(lin, in, 'old', MAXNAX, nin)
      if (nin(1).gt.MAXDIM .or. nin(2).gt.MAXDIM)
     *     call bug('f','Inpout Image too big')
      call rdhdi(lin, 'naxis', naxis, 0)
      if (naxis.lt.3) call bug('f', 'Image only has 2 dimensions')
      call boxmask(lin, boxes, MAXBOXES)
      call boxset(boxes, MAXNAX, nin, 's')

c     Find region of image which contains all channels to average and
c     then work out which planes to read.
      call boxinfo(boxes, MAXNAX, blc, trc)
      nplanes = 0
      do k = blc(3), trc(3)
        call boxruns(1, k, ' ', boxes, runs, MAXRUNS,
     *                nruns, xblc, xtrc, yblc, ytrc)
        if (nruns.ne.0) then
          nplanes = nplanes + 1
          if (nplanes.gt.MAXCHAN)
     *       call bug('f', 'Too many channels to average')
          planes(nplanes) = k
        endif
      enddo

      if (nterms.gt.1 .and. nplanes.le.nterms) then
        call bug('w','Fit likely poor,')
        call output('since the number of planes chosen as a')
        call output('continuum to be fit is less than or equal to')
        call output('the number of terms in the fit polynomial.')
      endif

c     Create the output image, copy the header keywords from the
c     input image and add the new history.
      if (out.ne.' ') then
        call xyopen(lout, out, 'new', MAXNAX, nin)
        call output('Creating continuum subtracted dataset: '//out)
      else
        lout = 0
        call bug('i','No continuum subtracted dataset created')
      endif

      ccreate = .false.
      if (cont.ne.' ') then
c       Dataset existence test: use hopen????? Is that OK
        call hopen(lcont,cont,'old',iostat)
        ccreate = iostat.ne.0
      else
        lcont = 0
        call output('No continuum map created.')
      endif

c     Do all necessary header and history manipulation.
      if (lout.ne.0) then
        call headcp(lin, lout, 0, 0, 0, 0)

        call hisopen(lout, 'append')
        call hiswrite(lout, 'PCSUB: Miriad ' // version)
        call hisinput(lout, 'PCSUB')
      endif

      if (ccreate) then
c       Continuum created from a polynomial fit.
        call output('Creating continuum map: ' // cont)
        call xyopen(lcont, cont, 'new', 2, nin)

c       Copy header from the input image.
        call headcp(lin, lcont, 0, 0, 0, 0)

        call hisopen(lcont, 'append')
        call hiswrite(lcont, 'PCSUB: Miriad ' // version)
        call hisinput(lcont, 'PCSUB')
      else
c       Continuum supplied by user.
        call output('Reading existing continuum map: ' // cont)
        call xyopen(lcont, cont, 'old', MAXNAX, cnin)

        if (cnin(1).ne.nin(1)) call bug('f','Bad X-size of cont= map')
        if (cnin(2).ne.nin(2)) call bug('f','Bad Y-size of cont= map')
        if (nterms.ne.1) nterms = 1
      endif

      call listcom(nplanes, planes, istart, iend, nsect)
      isnext = 1
      more = .true.
      do while (more)
        call txtplane(nsect, istart, iend, aline, isnext, more)
        if (lout.ne.0) call hiswrite(lout, aline)
        if (ccreate)   call hiswrite(lcont,aline)
      enddo
      if (lout.ne.0) call hisclose(lout)
      if (ccreate)   call hisclose(lcont)

c     Select a line of attack going into the cube and fit it to a
c     polynomial, using in the fit only those planes selected by input
c     as past of the continuum.  Then, subtract the polynomial fit from
c     each point along the line of attack (parallel to the z-axis).
      if (nterms.ne.1) then
        do j = 1, nin(2)
          call polfits(planes, nplanes, nterms, lin, lout, lcont, nin,
     *                 j, MAXDIM, MAXCHAN, MPPLUS1, MAXEXP, bm, c, b, a,
     *                 y, rope, cm, rline, contin)
        enddo
      else
c       In this case where the fit is zero order, create for each pixel
c       the average of its intensity over all the channels selected as
c       part of the continuum to be fit.  Then, subtract this average
c       from each plane of the cube at that pixel and deposit the
c       result as part of the output cube.  ==> This mode is also used
c       when the continuum map already existed. <==
        inplanes = 1.0/REAL(nplanes)
        do j = 1, nin(2)
          if (ccreate) then
            do i = 1, nin(1)
              contin(i) = 0.0
            enddo

            do k = 1, nplanes
              call xysetpl(lin,1,planes(k))
              call xyread(lin,j,rline)
              do i = 1, nin(1)
                contin(i) = contin(i) + rline(i)
              enddo
            enddo

            do i = 1, nin(1)
              contin(i) = contin(i)*inplanes
            enddo
            call xywrite(lcont,j,contin)
          else
            call xyread(lcont,j,contin)
          endif

c         From above:
c         If there is to be a separate continuum output file, whether
c         or not a new cube will be generated, then divide the line of
c         channel pixel summations (one row of pixels analyzed through
c         the selected planes) by the number of channels summed,
c         and add the resulting row of the continuum to the output
c         continuum file, cont.

c         Now: if there is to be an output cube, take the line of the
c         continuum, just made, and subtract it from the corresponding
c         line of the input cube (over all channels) to make a line
c         for the output cube.
c
          if (lout.ne.0) then
            do k = 1, nin(3)
              call xysetpl(lin,1,k)
              call xysetpl(lout,1,k)
              call xyread(lin,j,rline)
              do i = 1, nin(1)
                rline(i) = rline(i) - contin(i)
              enddo
              call xywrite(lout,j,rline)
            enddo
          endif
         enddo
      endif

c     Close up shop.
      call xyclose(lin)
      if (lout.ne.0) call xyclose(lout)
      if (ccreate) call xyclose(lcont)

      end

c***********************************************************************

      subroutine listcom (nplanes, planes, istart, iend, nsect)

      integer nplanes, planes(nplanes), istart(nplanes),
     *  iend(nplanes), nsect
c-----------------------------------------------------------------------
c  Compact a list of planes such as: 1,2,3,4,7,10,12,13,14 to a two
c  lists giving the start and END planes of each section for ease of
c  formatting in history.
c
c    Input:
c      nplanes   size of planes() array
c      planes    list of planes
c    Output:
c      istart    lists of start and END planes for each section
c      iend           e.g. if planes=1,2,3,5,7,8,9
c                        istart=1,5,7
c                        iend  =3,5,9
c      nsect     number of sections, for above example, nsect=3
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      nsect = 1

      do i = 1, nplanes
        if (i.eq.1) then
          istart(nsect) = planes(1)
        else if (planes(i).ne.(planes(i-1)+1)) then
          iend(nsect) = planes(i-1)
          nsect = nsect + 1
          istart(nsect) = planes(i)
        else
          if (i.eq.nplanes) iend(nsect) = planes(i)
        endif
      enddo

      end

c***********************************************************************

      subroutine txtplane (nsect, istart, iend, string, isnext, more)

      integer nsect, istart(nsect), iend(nsect), isnext
      character string*(*)
      logical more
c-----------------------------------------------------------------------
c  Write the compacted plane list into a text string.
c
c   Input:
c     nsect      number of sections in LIST
c     istart     list of start planes for each section
c     iend       list of end planes for each section
c   Output:
c     string     output string
c   Input/Output:
c     isnext     The number of the next section to start the next line
c     more       do it again please if true
c-----------------------------------------------------------------------
      integer strlen, l1, l2, ipt, i
      character*4 ch1, ch2
c-----------------------------------------------------------------------
      string = 'PCSUB: channels fit to polynomial = '
      ipt = 29
      strlen = len(string)
      do i = isnext, nsect
        if (istart(i).eq.iend(i)) then
          call itochar(istart(i), ch1, l1)
          if (ipt+l1.lt.strlen) then
            string(ipt:ipt+l1) = ch1(1:l1)
          else
            more = .TRUE.
            isnext = i
            GOTO 999
          endif
          ipt = ipt + l1 + 2
        else
          call itochar(istart(i), ch1, l1)
          call itochar(iend(i), ch2, l2)
          if (ipt+l1+l2.lt.strlen) then
            string(ipt:ipt+l1+l2) = ch1(1:l1)//':'//ch2(1:l2)
            ipt = ipt + l1 + l2 + 2
          else
            more = .TRUE.
            isnext = i
            GOTO 999
          endif
        endif
      enddo
      more = .FALSE.

 999  RETURN
      end

c***********************************************************************

      subroutine itochar (ival, string, ilen)

      integer ival, ilen
      character*(*) string
c-----------------------------------------------------------------------
c  Encode an integer into a character string.
c
c     Input:
c        ival      i      Integer to encode
c     Output:
c        string    c(*)   String in which integer is placed.
c        ilen      i      Length of encoded part of string
c-----------------------------------------------------------------------
      integer   len1
      character itoaf*20, temp*20
c-----------------------------------------------------------------------
      temp = itoaf(ival)
      ilen = len1(temp)
      string = temp(1:ilen)

      end

c***********************************************************************

      subroutine polfits(planes,nplanes,nterms,lin,lout,lcont,
     *                        nin, j, maxdim, maxchan,
     *                        mpplus1, maxexp, bm, c, b, a, y,
     *                        rope, cm, rline, contin)
c-----------------------------------------------------------------------
c  Fit and subtract a continuum map. Create arrays involved.
c
c       Input:
c         maxdim, maxchan, mpplus1, maxexp -- Maximum values for the
c           sizes of a dimension, of the number of channels, of the
c           order plus one
c         nplanes -- number of planes selected for the continuum fit.
c         planes --- list of planes selected for the continuum fit.
c         c -- list of coefficients for a line number i, going into
c           the cube.
c         cm - two dimensional array accumulating the c arrays for each
c           frontal pixel position on the line of interest.
c         bm - the matrix, created in the outfit routine.
c         nin -- axis dimensions.
c         j -- the line of the cube front that is now being fit
c           for each line starting from a point on j, and heading
c           deep into all the planes of the cube.
c         lin, lout, lcont -- the handles for the in, out, and continuum
c           files.
c         rope -- A long array made of the row j for each plane, where
c           each read of the line on a new plane is placed at the end
c           of the rope consecutively.
c         rline -- The line that is read from a plane of the input cube
c           or written to the output cube.
c         contin - The line that is written to an output continuum map.
c-----------------------------------------------------------------------
      integer maxdim, maxchan, mpplus1, maxexp, nplanes, lin, lout,
     *        lcont, j, planes(maxchan), nterms, nin(3),
     *        b(mpplus1+mpplus1)

      integer a(mpplus1)
      real rline(maxdim), contin(maxdim)
      double precision bm(mpplus1, mpplus1), c(mpplus1), y(maxdim)
      double precision rope(maxdim*maxchan), cm(maxdim, mpplus1)

      integer i, k, m, n, expre
c-----------------------------------------------------------------------
c     Form array (rope) of lines (k,j),(k+1,j),(k+2,j)... placed
c     end to end.
      do k = 1, nplanes
        call xysetpl(lin,1,planes(k))
        call xyread(lin,j,rline)
        do i = 1, nin(1)
          rope((k-1)*nin(1)+i) = rline(i)
        enddo
      enddo

      do i = 1, nin(1)
c       Make array (y) of (k-1)*nin(1)+i positions on rope array,
c       getting a line array parallel to the z axis and nplanes in
c       length.
        do k = 1, nplanes
          y(k) = rope((k-1)*nin(1)+i)
        enddo

c       Create arrays and perform fit for this y(*) going into the
c       cube at position (i,j).
        call outfit(mpplus1, maxdim, maxexp, maxchan, nterms,
     *              nplanes, planes, bm, y, a, b, c)

c       Transfer coefficients of polynomial from fit routine and place
c       them in the 2-D grid of coefficients (Position-on-Rline versus
c       Coefficient-Number.)
        do n = 1, nterms
          cm(i,n) = c(n)
          c(n) = 0.0
        enddo
      enddo

c     Subtract the continuum from the image.
      do k = 1, nin(3)
        call xysetpl(lin,1,k)
        if (lout.ne.0) call xysetpl(lout,1,k)
        if (lcont.ne.0) call xysetpl(lcont,1,k)
        call xyread(lin,j, rline)
        do i = 1, nin(1)
          contin(i) = 0.0
          do n = 1, nterms
            expre = 1
            if (n.gt.maxexp) then
c             Form the x**n term, run by run, by which each specific
c             coefficient is to be multiplied by for given i and
c             Coefficient-Number)
              do m = 1, (n-1)
                expre = expre*k
              enddo
            else
              if (n.gt.1) then
                expre = k**(n-1)
              endif
            endif

c           Subtract each coefficient*x**n term of each i from rline(i).
            rline(i) = rline(i) - cm(i,n)*DBLE(expre)
            if (lcont.ne.0) then
              contin(i) = contin(i) + cm(i,n)*DBLE(expre)
            endif
          enddo
        enddo
        if (lout.ne.0)  call xywrite(lout, j,rline)
        if (lcont.ne.0) call xywrite(lcont,j,contin)
      enddo

      end

c***********************************************************************

      subroutine outfit(mpplus1, maxdim, maxexp, maxchan, nterms,
     *                nplanes, planes, bm, y, a, b, c)

c-----------------------------------------------------------------------
c  Create a matrix (bm) and send it to double precision linpack
c  subroutines.
c-----------------------------------------------------------------------
      integer maxdim, maxexp, maxchan, nterms, nplanes,
     *        planes(maxchan), mpplus1, a(mpplus1)
      double precision bm(mpplus1, mpplus1), y(maxdim), c(mpplus1)

      integer i, j, k, info, b(mpplus1+mpplus1)
c-----------------------------------------------------------------------
c     Reset c and bm (the matrices)
      do i = 1, nterms
        do j = 1, nterms
          bm(i,j) = 0.0
        enddo
        c(i) = 0.0
      enddo

c     Create matrix bm and array c to be sent to the linpack fit
      do k = 1, nplanes
c       Create array of planes**0 ... planes**(2*nterms-1)
c       with which to work to create bm
        b(1) = 1
        do i = 2, (nterms+nterms-1)
          b(i) = b((i-1))*planes(k)
        enddo

c       Create bm and c.
        do i = 1, nterms
          do j = 1, nterms
              bm(i,j) = bm(i,j) + dble(b(i+j-1))
          enddo
          c(i) = c(i) + y(k)*dble(b(i))
        enddo
      enddo

c     Call linpack matrix transposition and fit subroutines.
      call dgefa(bm, mpplus1, nterms, a, info)
      if (info.ne.0) call bug('f', 'Highly bogus division by zero')
      call dgesl(bm, mpplus1, nterms, a, c, 0)

      end
