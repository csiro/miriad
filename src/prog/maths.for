      program maths

c= maths - Mathematical operations on images and image data
c& rjs
c: utility, map combination, map manipulation
c+
c       MATHS is a MIRIAD task that performs arithmetic expressions on a
c       number of images.  The expression to be performed is given in a
c       FORTRAN-like syntax, and can consist of operators, real
c       constants and FORTRAN functions.  Normal FORTRAN precedence
c       applies.
c
c       Operators can be +, -, * and /, and all logical and relational
c       operators (e.g. .and. .or. .not. .gt. .ge. etc,).  In MATHS
c       convention, a positive value is considered TRUE, and a negative
c       or zero value is considered FALSE.
c
c       Functions appear only in the generic, rather than specific
c       forms.  For example use "log10" rather than "alog10", and
c       "max" rather than "amax1".  Integers and double precision
c       constants are converted to reals.  File names take the place of
c       variables, and the expression is evaluated on each pixel of the
c       image.  When there is more than one file name in the input
c       expression, the expression is evaluated at corresponding pixels
c       of the input images.  For example to average image "fred" with
c       image "bill", use:
c
c         exp=(fred+bill)/2
c
c       When a file name starts with a numeric character, or contains a
c       character which might be confused with an operator the file name
c       should be bracketed by angular ( < and > ). For example:
c
c         exp=(<2ndtry>+bill.dat)/2
c         mask=<bmap.fft>.gt.0.5
c
c       Files cannot take the name "x", "y", or "z".  MATHS interprets
c       these as being the 3 independent variables of an image, which
c       vary linearly between the limits set by the XRANGE, YRANGE and
c       ZRANGE parameters. The user chooses the meaning of these units.
c       For example, to create one cycle of a two dimensional sine wave
c       along the x and y coordinate axes use:
c
c         exp=sin(x)*sin(y) xrange=-3.14,3.14 yrange=-3.14,3.14
c
c       In addition to the expression, MATHS also allows the user to
c       specify a "mask expression".  MATHS main expression is only
c       evaluated at pixels where the "mask expression" is TRUE or
c       positive valued.
c
c       MATHS does not check for divide by zero, logs of a negative
c       number or any similar problem.  It will probably crash if this
c       is attempted.  Consequently, when performing potentially
c       dangerous operations, it is best to guard the main expression
c       by masking out dangerous situations.  The mask expression can
c       also be used to prevent the calculation where doing so would be
c       undesirable for other reasons (e.g. where the signal is too weak
c       to get meaningful results).
c
c       For example:
c
c          exp=sqrt(fred) mask=fred.gt.0
c@ exp
c       The expression to be evaluated.
c@ mask
c       The mask expression.  The expression given by "exp" is evaluated
c       only at those pixels where the mask expression is TRUE or
c       positive valued.  Pixels, which fail this test, are marked as
c       blank in the output image.
c@ region
c       The region of interest in the input images.  Full region
c       specifications are supported, however the output map will
c       contain only the bounding box. (see also mask=)
c       Default: full map.
c@ out
c       The name of the output image.
c@ imsize
c       The output image size. This is used only if there is no input
c       images (i.e. the expression consists of a function of "x" and
c       "y" only). No default.
c@ xrange
c       When "x" is present in the input expression, the x variable
c       is varied linearly between the two limits set by XRANGE.  The
c       default is -0.5,0.5.
c@ yrange
c       When "y" is present in the input expression, the y variable
c       is varied linearly between the two limits set by YRANGE.  The
c       default is -0.5,0.5
c@ zrange
c       When "z" is present in the input expression, the z variable
c       is varied linearly between the two limits set br ZRANGE.  The
c       default is 0,1.
c@ options
c       Extra processing options.  Several can be given, separated by
c       commas.  Minimum match is used.
c         grow     Allow inputs to "grow" extra axes, if needed,
c                  through replication.  For example, if the expression
c                  subtracts a single-plane image from a cube,
c                  options=grow allows the operation to proceed by first
c                  growing the image into a cube through replication the
c                  plane.  Normally (i.e. without this option), MATHS
c                  insists that the inputs must be identical in size.
c         unmask   Treat all pixels as if they were valid.
c         limitmem Limit the amount of memory used. This may give
c                  a buffer overflow error for complicated expressions.
c                  Use for simple expressions with very large images.
c
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maths.h'

      integer ERROR, CONSTANT, SCALAR, VECTOR
      parameter (ERROR=0, CONSTANT=1, SCALAR=2, VECTOR=3)
      integer BUFLEN, MAXBOX
      parameter (BUFLEN=256, MAXBOX=2048)

      logical   doExp, doMask, doRuns, unmask, limitmem
      integer   boxes(MAXBOX), i, k, l, lout,
     *          nBuf, nERB, nMRB, nout(MAXNAX),
     *          scratch(3,MAXRUNS), type, xblc,
     *          xtrc, yblc, ytrc
      ptrdiff   pnt, rbuflen, idx, npixels
      integer   expbuf(BUFLEN),maskbuf(BUFLEN)
      real      exprbuf(BUFLEN), maskrbuf(BUFLEN), RBUF(MAXBUF)
      character expr*256, mask*256, outNam*64, template*64, version*72

      common    RBUF

      external  boxrect, hdprsnt, PACTION, VACTION, versan
      logical   boxRect, hdprsnt
      character versan*72
c-----------------------------------------------------------------------
      version = versan('maths')

c     Get input parameters.
      call keyini
      call getopt(grow,unmask,limitmem)
      call keya('exp',Expr,' ')
      call keya('mask',Mask,' ')
      call keya('out',outNam,' ')
      call keyi('imsize',nsize(1),0)
      do i = 2, MAXNAX
        call keyi('imsize',nsize(i),1)
      enddo
      call keyr('xrange',Range(1,1),-0.5)
      call keyr('xrange',Range(2,1), 0.5)
      call keyr('yrange',Range(1,2),-0.5)
      call keyr('yrange',Range(2,2), 0.5)
      call keyr('zrange',Range(1,3), 0.0)
      call keyr('zrange',Range(2,3), 1.0)

      doMask = Mask.ne.' '
      doExp  = Expr.ne.' '
      if (.not.doMask .and. .not.doExp) call bug('f',
     *  'An expression (exp=) or mask (mask=) must be given')
      if (outNam.eq.' ') call bug('f',
     *  'Output file must be given (out=)')

c     Parse the expression and mask.
      Xused = .false.
      Yused = .false.
      Zused = .false.
      nfiles = 0
      if (doExp) then
        call ariComp(expr,PACTION,type,ExpBuf,BUFLEN,ExpRBuf,
     *    BUFLEN)
        if (type.eq.error) call bug('f',
     *       'Error parsing the expression: ' // expr)
        if (type.ne.vector) call bug('f',
     *       'No images in input expression: ' // expr)
        call ariInq(ExpBuf,ExpRBuf,nBuf,nERB)
      endif

      if (doMask) then
        call ariComp(Mask,PACTION,type,MaskBuf,BUFLEN,MaskRBuf,
     *    BUFLEN)
        if (type.eq.error) call bug('f',
     *        'Error parsing the mask expression: ' // mask)
        if (type.ne.vector) call bug('f',
     *        'No images in input mask expression: ' // mask)
        call ariInq(MaskBuf,MaskRBuf,nBuf,nMRB)
      endif

c     Get the region of interest and finish with the key routines.
      template = ' '
      if (nfiles.gt.0) template = names(offset(1)+1:offset(2))
      call boxInput('region',template,boxes,MAXBOX)
      call keyfin

c     If there are no input files, check that we know the image size.
      if (nfiles.eq.0) then
        naxis = 1
        do i = 1, MAXNAX
          if (nsize(i).le.0) call bug('f','Image size is wrong')
          if (nsize(i).gt.1) naxis = i
        enddo
      endif

c     If X, Y or Z were used, make sure that they are not dummy axes.
      if (Xused .and. nsize(1).eq.1)
     *  call bug('f','The x dimension is 1 pixel wide')
      if (Yused .and. nsize(2).le.1)
     *  call bug('f','The y dimension is 1 pixel high')
      if (Zused .and. nsize(3).le.1)
     *  call bug('f','The z dimension is 1 image deep')

c     Bloody box information.
      call boxSet(boxes,naxis,nsize,' ')
      call boxInfo(boxes,naxis,blc,trc)

c     If there are input files, "and" all of their flagging masks into
c     regions where the computation is to take place.
      if (.not.unmask) then
        do i = 1, nfiles
          if (naxes(i).lt.naxes(ref) .and. hdprsnt(lIn(i),'mask'))
     *      call bug('f','Cannot handle masks with options=grow')
          call boxMask(lIn(i),boxes,MAXBOX)
        enddo
      endif
      doRuns = .not.BoxRect(boxes)

      do i = 1, naxis
        nOut(i) = trc(i) - blc(i) + 1
      enddo
      do i = naxis+1, MAXNAX
        nOut(i) = 1
        blc(i) = 1
        trc(i) = 1
      enddo

c     Handle naxis > 4.
      do l = 5, naxis
        if (nsize(l).gt.1) call bug('f','Too many dimensions for me')
        plane(l) = 1
      enddo

c     Allocate memory.
c     For large images this can be excessive, so limit this if requested
c
      if (limitmem) then
        rbuflen = 2_8*nOut(1)*nOut(2) + 1024
      else
        rbuflen = 6_8*nOut(1)*nOut(2)
      endif
      call memallox(pnt,rbuflen,'r')
      idx = pnt

c     Open the output and create the header.
      call xyOpen(lOut,outNam,'new',naxis,nOut)
      if (nfiles.gt.0) then
        call oldHdr(lIn(ref),lOut,naxis,nsize,blc,trc,version)
      else
        call newHdr(lOut,naxis,Range,nsize,blc,version)
      endif

c     To recapitulate:
c       The expression (if doExp) is in ExpBuf,RBuf,
c       The mask expression (if doMask) is in MaskBuf,RBuf,
c       The boxes specification (if doRuns) is in boxes,
      do l = 1, nOut(4)
        Plane(4) = l + blc(4) - 1
        do k = 1, nOut(3)
          Plane(3) = k + blc(3) - 1
          do i = 1, nfiles
            if (naxes(i).gt.2) call xySetPl(lIn(i),naxes(i)-2,Plane(3))
          enddo
          call xySetPl(lOut,1,k)

          call boxRuns(max(naxis-2,1),Plane(3),' ',boxes,
     *      runs,MAXRUNS,nRuns,xblc,xtrc,yblc,ytrc)

          if (doMask) then
            call boxCount(runs,nRuns,npixels)
            if (npixels.gt.0) then
              if (nMRB.gt.0) call moveData(nMRB,MaskRBuf,RBuf(pnt))
              call ariExec(VACTION,npixels,MaskBuf,BUFLEN,RBuf(pnt),
     *                     RBufLen,idx)
              idx = idx + pnt - 1
              call compRuns(RBuf(idx),
     *                          runs,MAXRUNS,nRuns,Scratch,MAXRUNS)
            endif
          endif

          if (doMask .or. doRuns) call putRuns(lOut,runs,nRuns,
     *         1-blc(1),1-blc(2),nOut(1),nOut(2))

          if (doExp) then
            call boxCount(runs,nRuns,npixels)
            if (npixels.gt.0) then
              if (nERB.gt.0) call moveData(nERB,ExpRBuf,RBuf(pnt))
              call ariExec(VACTION,npixels,ExpBuf,BUFLEN,RBuf(pnt),
     *                     RBufLen,idx)
              idx = idx + pnt - 1
            endif

            call putPlane(lOut,runs,nRuns,
     *         1-blc(1),1-blc(2),nOut(1),nOut(2),RBuf(idx),npixels)
          endif
        enddo
      enddo

c     Free allocated memory.
      call memfrex(pnt,rbuflen,'r')

c     Close files.
      do i = 1, nfiles
        call xyClose(lIn(i))
      enddo
      call xyClose(lOut)

      end

c***********************************************************************

      subroutine moveData(n,inMap,outMap)

      integer n
      real    inMap(n),outMap(n)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        outMap(i) = inMap(i)
      enddo

      end

c***********************************************************************

      subroutine compRuns(Mask,runs,maxRuns,nRuns,Scratch,nScratch)

      integer nRuns,maxRuns,nScratch
      integer runs(3,maxRuns),Scratch(3,nScratch)
      real Mask(*)

c  Combine an input runs specification and mask into an output runs
c  specification.  The mask is real-valued.  If positive the
c  corresponding pixel is to be selected, if negative or zero the pixel
c  is to be ignored.  The values in the mask correspond only for pixels
c  selected in the input runs specification.
c
c  Inputs:
c    Mask       The mask.
c    maxRuns    The size of the runs array.
c    nScratch   Size of the scratch buffer.
c
c  Input/Output:
c    runs       Input runs.
c    nRuns      Number of runs in the input.
c
c  Scratch:
c    Scratch    An intermediate working array.
c-----------------------------------------------------------------------
      integer i,k,l,length,nOut
c-----------------------------------------------------------------------
c     Initialise.
      nOut = 0
      k = 1

c     Determine the output runs and place them in the scratch buffer.
      do i = 1, nRuns
        length = runs(3,i) - runs(2,i) + 1
        l = 0
        do while (l.lt.length)
          do while (l.lt.length .and. Mask(k).le.0)
            k = k + 1
            l = l + 1
          enddo
          if (l.lt.length) then
            if (nOut.eq.nScratch)
     *        call bug('f','Scratch buffer overflow (MAXRUNS)')
            nOut = nOut + 1
            Scratch(1,nOut) = runs(1,i)
            Scratch(2,nOut) = runs(2,i) + l
            do while (l.lt.length .and. Mask(k).gt.0)
              k = k + 1
              l = l + 1
            enddo
            Scratch(3,nOut) = runs(2,i) + l - 1
          endif
        enddo
      enddo

c     Copy the output runs (in the scratch buffer) to the runs array.
      if (nOut+1.gt.maxRuns) call bug('f',
     *           'Runs buffer overflow MAXRUNS)')
      do i = 1, nOut
        runs(1,i) = Scratch(1,i)
        runs(2,i) = Scratch(2,i)
        runs(3,i) = Scratch(3,i)
      enddo
      nRuns = nOut
      runs(1,nOut+1) = 0
      runs(2,nOut+1) = 0
      runs(3,nOut+1) = 0

      end

c***********************************************************************

      subroutine oldHdr(lIn,lOut,naxis,n,blc,trc,version)

      integer   lIn, lOut
      integer   naxis, n(naxis), blc(naxis), trc(naxis)
      character version*(*)
c-----------------------------------------------------------------------
c  Make the header of the output file.  This is a carbon copy of the
c  input, except that a history record is added.
c-----------------------------------------------------------------------
      integer   iax, lblc, ltrc
      double precision crpix, def
      character card*80, keyw*8, txtblc*32, txttrc*32

      character itoaf*2
      external  itoaf
c-----------------------------------------------------------------------
c     Copy the header from the input map.
      call headcp(lIn, lOut, 0, 0, 0, 0)

c     Rewrite the reference pixel location.
      do iax = 1, naxis
        keyw = 'crpix'//itoaf(iax)
        if (iax.le.2) then
          def = dble(n(iax)/2 + 1)
        else
          def = 1d0
        endif

        call rdhdd(lIn,  keyw, crpix, def)
        crpix = crpix - dble(blc(iax) - 1)
        call wrhdd(lOut, keyw, crpix)
      enddo

      call hisopen(lOut,'append')
      card = 'MATHS: Miriad ' // version
      call hiswrite(lOut,card)
      call hisinput(lOut,'MATHS')
      call mitoaf(blc,naxis,txtblc,lblc)
      call mitoaf(trc,naxis,txttrc,ltrc)
      card = 'MATHS: Bounding region is Blc=('//txtblc(1:lblc)//
     *                               '),Trc=('//txttrc(1:ltrc)//')'
      call hiswrite(lOut,card)
      call hisclose(lOut)

      end

c***********************************************************************

      subroutine newHdr(lOut,naxis,Range,n,blc,version)

      integer   lOut, naxis, n(naxis), blc(naxis)
      real      Range(2,naxis)
      character version*(*)
c-----------------------------------------------------------------------
c  Make the header of the output file. This is a carbon copy of the
c  input, except that a history record is added.
c-----------------------------------------------------------------------
      integer   iax
      double precision cdelt, crpix, crval
      character card*80, num*1

      character itoaf*1
      external  itoaf
c-----------------------------------------------------------------------
      do iax = 1, naxis
        if (iax.le.2) then
          crpix = dble(n(iax)/2 + 1)
        else
          crpix = 1d0
        endif
        crpix = crpix - dble(blc(iax) - 1)

        num = itoaf(iax)
        cdelt = (Range(2,iax) - Range(1,iax)) / (n(iax)-1)
        crval =  Range(1,iax) + (crpix-1)*cdelt
        call wrhdr(lOut, 'crval'//num, crval)
        call wrhdr(lOut, 'cdelt'//num, cdelt)
        call wrhdr(lOut, 'crpix'//num, crpix)
      enddo

      call hisopen(lOut,'append')
      card = 'MATHS: Miriad '//version
      call hiswrite(lOut,card)
      call hisinput(lOut,'MATHS')
      call hisclose(lOut)

      end

c***********************************************************************

      subroutine PACTION(symbol, dType, idx, value)

      character symbol*(*)
      integer   dType
      integer   idx
      real      value
c-----------------------------------------------------------------------
c The parser's action routine.  Open the file and make sure it's the
c right size.
c
c  Inputs:
c    Symbol     The file name.
c  Outputs:
c    dType      VECTOR type.
c    idx        Index into lIn array.
c    value      Unused.
c-----------------------------------------------------------------------
      include 'maths.h'

      integer    VECTOR
      parameter (VECTOR=3)

      logical   dogrow
      integer   i, nin(MAXNAX)
      character umsg*64

      external  len1
      integer   len1
c-----------------------------------------------------------------------
      if (symbol.eq.'x') then
        Xused = .true.
        idx = XVAL
      else if (symbol.eq.'y') then
        Yused = .true.
        idx = YVAL
      else if (symbol.eq.'z') then
        Zused = .true.
        idx = ZVAL
      else
c       Check if we already have this one open.
        idx = 0
        do i = 1, nfiles
          if (symbol.eq.Names(Offset(i)+1:Offset(i+1))) idx = i
        enddo

c       If it was not found, open the file.
        if (idx.eq.0) then
          if (nfiles.ge.MAXFILES) call bug('f','Too many open files')
          nfiles = nfiles + 1
          call xyOpen(lIn(nfiles),Symbol,'old',MAXNAX,nin)
          naxes(nfiles) = 1

          if (nfiles.eq.1) then
            do i = 1, MAXNAX
              nsize(i) = nin(i)
              if (nin(i).gt.1) naxes(nfiles) = i
            enddo

            call rdhdi(lIn(1),'naxis',naxis,0)
            naxis = min(naxis, MAXNAX)
            Offset(1) = 0
            ref = 1
          else
            do i = 1, MAXNAX
              if (nin(i).gt.1) then
                naxes(nfiles) = i
                dogrow = naxes(ref).lt.i
              else
                dogrow = naxes(ref).ge.i
              endif

              if (nin(i).ne.nsize(i) .and.
     *            .not.(dogrow .and. grow)) then
                umsg = 'Input images/mask have different sizes: ' //
     *            Symbol(1:len1(Symbol))
                call bug('f',umsg)
              endif

              if (nin(i).gt.1 .and. naxes(ref).lt.i) then
                ref = nfiles
                nsize(i) = nin(i)
                call rdhdi(lIn(nfiles),'naxis',naxis,naxes(nfiles))
                naxis = min(naxis, MAXNAX)
              endif
            enddo
          endif

          Offset(nfiles+1) = Offset(nfiles) + len1(symbol)
          if (Offset(nfiles+1).gt.len(Names))
     *        call bug('f','Name buffer overflow, in PACTION')
          Names(Offset(nfiles)+1:Offset(nfiles+1)) = symbol
          idx = nfiles
        endif
      endif

      dType = VECTOR
      value = 0.0

      end

c***********************************************************************

      subroutine VACTION(idx, dType, rData, n)

      integer   idx, dType
      ptrdiff   n
      real      rData(*)
c-----------------------------------------------------------------------
c  This routine is called by ariExec each time it wants a row of a
c  file.  It's not exactly the most complex routine in the world.
c
c  Inputs:
c    idx        Index into lIn array of file descriptors.
c    dType      Should be VECTOR.
c    n          Should equal n1.
c  Output:
c    rData      The row of data.
c-----------------------------------------------------------------------
      include 'maths.h'

      integer    VECTOR
      parameter (VECTOR=3)

      integer   i, j
      ptrdiff   npixel, k
      real      cdelt, crval, temp
c-----------------------------------------------------------------------
      if (dType.ne.VECTOR) call bug('f',
     *  'VACTION called with non-vector type')

c     Fill in data if it corresponds to a value of x, y or z.
      if (idx.eq.XVAL) then
        cdelt = (Range(2,1)-Range(1,1))/real(nsize(1)-1)
        crval = Range(1,1) - cdelt
        k = 1
        do j = 1, nRuns
          do i = runs(2,j), runs(3,j)
            rData(k) = cdelt * i + crval
            k = k + 1
          enddo
        enddo

      else if (idx.eq.YVAL) then
        cdelt = (Range(2,2)-Range(1,2))/real(nsize(2)-1)
        crval = Range(1,2) - cdelt
        k = 1
        do j = 1, nRuns
          temp = cdelt * runs(1,j) + crval
          do i = runs(2,j), runs(3,j)
            rData(k) = temp
            k = k + 1
          enddo
        enddo

      else if (idx.eq.ZVAL) then
        cdelt = (Range(2,3)-Range(1,3))/real(nsize(3)-1)
        crval = Range(1,3) - cdelt
        temp = cdelt * Plane(3) + crval
        do k = 1, n
          rData(k) = temp
        enddo

      else
c       Otherwise read it from a file.
        call getPlane(lIn(idx),runs,nRuns,0,0,nsize(1),nsize(2),
     *    rData,n,npixel)
        if (n.ne.npixel) call bug('f','Something is screwy in VACTION')
      endif

      end

c***********************************************************************

      subroutine getopt(grow,unmask,limitmem)

      logical grow,unmask,limitmem
c
c-----------------------------------------------------------------------
      integer NOPTS
      parameter (NOPTS=3)
      character opts(NOPTS)*8
      logical present(NOPTS)
      data opts/'grow    ','unmask  ','limitmem'/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)

      grow = present(1)
      unmask = present(2)
      limitmem = present(3)

      end
