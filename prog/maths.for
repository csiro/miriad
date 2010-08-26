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
c       images (i.e. the expression consists of a function of ``x'' and
c       ``y'' only). No default.
c@ xrange
c       When ``x'' is present in the input expression, the x variable
c       is varied linearly between the two limits set by XRANGE.  The
c       default is -0.5,0.5.
c@ yrange
c       When ``y'' is present in the input expression, the y variable
c       is varied linearly between the two limits set by YRANGE.  The
c       default is -0.5,0.5
c@ zrange
c       When ``z'' is present in the input expression, the z variable
c       is varied linearly between the two limits set br ZRANGE.  The
c       default is 0,1.
c@ options
c       Extra processing options.  Several can be given, separated by
c       commas.  Minimum match is used.
c         grow    Allow inputs to ``grow'' extra axes, if needed,
c                 through replication.  For example, if the expression
c                 subtracts a single-plane image from a cube,
c                 options=grow allows the operation to proceed by first
c                 growing the image into a cube through replication the
c                 plane.  Normally (i.e. without this option), MATHS
c                 insists that the inputs must be identical in size.
c         unmask  Treat all pixels as if they were valid.
c
c$Id: maths.for,v 1.3 2010/08/26 06:58:23 cal103 Exp $
c--
c
c  History:
c    rjs   nov88 - Adapted from the Werong task, arrith.
c    rjs  9jun89 - Added blanking and masking support.
c    rjs 14sep89 - Check if used dimensions are only 1 pixel long.
c    pjt   ???89 - Increased BufLen parameter.
c    rjs 30apr90 - Changed call sequence to BoxInput.
c    pjt 20may90 - Comments about memory allocation (maxdim.h stuff)
c   mchw 21nov90 - Added ltype description and pbfwhm to header.
c    pjt  4mar91 - itoa->itoaf
c    rjs  8apr91 - Fixed bug related to outputs with few planes than the
c                  inputs.
c   mchw 17may91 - Added keyws 'bmaj','bmin','bpa' to header.
c   rjs  18sep91 - Fixed bug when all the output pixels are blanked.
c   rjs  18mar92 - Better memory allocation scheme.
c   rjs   1may92 - Better header copy scheme. Works for 4D files.
c                  Eliminate umsg.  Standard history.  Changes to
c                  appease flint.
c   nebk 26may92 - Add "btype" to header copy
c   pjt  28jul93 - more descriptive error message in PACTION
c                  increased stringlength for expr etc. (from 64 -> 256)
c   rjs  21sep93 - Fiddles with the order in which things are done so
c                  that there is a template to the Boxinp routine.
c   pjt   8jun94 - clarified region=
c   rjs   3dec94   Copy across mosaic table.
c   rjs  26jan95   Eliminate non-standard string concatentation.
c   rjs  10jan97   Handle 5th axis correctly.
c  nebk  05jun97   Handle 6th and 7th axes correctly in oldhdr !
c   rjs  02jul97   cellscal change.
c   rjs  23jul97   Added pbtype.
c   rjs  12mar98   Allow for more complex expressions.
c   rjs  30nov98   Added options=grow
c   rjs  02dec98   Increased BUFLEN again.
c   rjs  17oct00   Added options=unmask
c   rjs  27may05   Eliminate a redundant statement which was confusing
c                  the optimiser on Solaris.
c-----------------------------------------------------------------------
      include 'maths.h'

      integer ERROR, CONSTANT, SCALAR, VECTOR
      parameter (ERROR=0, CONSTANT=1, SCALAR=2, VECTOR=3)
      integer BUFLEN, MAXBOX
      parameter (BUFLEN=256, MAXBOX=2048)

      logical   doExp, doMask, doRuns, unmask
      integer   boxes(MAXBOX), expbuf(BUFLEN), i, indx, k, l, lout,
     *          maskbuf(BUFLEN), nBuf, nERB, nMRB, nout(MAXNAX),
     *          npixels, pnt, rbuflen, scratch(3,MAXRUNS), type, xblc,
     *          xtrc, yblc, ytrc
      real      exprbuf(BUFLEN), maskrbuf(BUFLEN), RBUF(MAXBUF)
      character expr*256, mask*256, outNam*64, template*64, version*72

      common    RBUF

      logical   BoxRect, hdprsnt
      integer   Fill
      character versan*80
      external  boxrect, fill, hdprsnt, paction, vaction, versan
c-----------------------------------------------------------------------
      version = versan('maths',
     *                 '$Revision: 1.3 $',
     *                 '$Date: 2010/08/26 06:58:23 $')
c
c  Get the input parameters.
c
      call keyini
      call getopt(grow,unmask)
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
c
c  Parse the expression and mask.
c
      Xused = .false.
      Yused = .false.
      Zused = .false.
      nfiles = 0
      if (doExp) then
        call ariComp(expr,paction,type,ExpBuf,BUFLEN,ExpRBuf,BUFLEN)
        if (type.eq.error) call bug('f',
     *       'Error parsing the expression: ' // expr)
        if (type.ne.vector) call bug('f',
     *       'No images in input expression: ' // expr)
        call ariInq(ExpBuf,ExpRBuf,nBuf,nERB)
      endif

      if (doMask) then
        call ariComp(Mask,paction,type,MaskBuf,BUFLEN,MaskRBuf,BUFLEN)
        if (type.eq.error) call bug('f',
     *        'Error parsing the mask expression: ' // mask)
        if (type.ne.vector) call bug('f',
     *        'No images in input mask expression: ' // mask)
        call ariInq(MaskBuf,MaskRBuf,nBuf,nMRB)
      endif
c
c  Get the region of interest, and finish with the key routines.
c
      template = ' '
      if (nfiles.gt.0) template = names(offset(1)+1:offset(2))
      call BoxInput('region',template,boxes,MAXBOX)
      call keyfin
c
c  If there are no input files, check that we know the image size.
c
      if (nfiles.eq.0) then
        naxis = 1
        do i = 1, MAXNAX
          if (nsize(i).le.0) call bug('f','Image size is wrong')
          if (nsize(i).gt.1) naxis = i
        enddo
      endif
c
c  If X, Y or Z were used, make sure that they are not dummy axes.
c
      if (Xused .and. nsize(1).eq.1)
     *  call bug('f','The x dimension is 1 pixel wide')
      if (Yused .and. nsize(2).le.1)
     *  call bug('f','The y dimension is 1 pixel high')
      if (Zused .and. nsize(3).le.1)
     *  call bug('f','The z dimension is 1 image deep')
c
c  Bloody box information.
c
      call BoxSet(boxes,naxis,nsize,' ')
      call BoxInfo(boxes,naxis,blc,trc)
c
c  If there are input files, "and" all there flagging masks into
c  regions where the computation is to take place.
c
      if (.not.unmask) then
        do i = 1, nfiles
          if (naxes(i).lt.naxes(ref) .and. hdprsnt(lIn(i),'mask'))
     *      call bug('f','Cannot handle masks with options=grow')
          call BoxMask(lIn(i),boxes,MAXBOX)
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
c
c  Handle naxis > 4.
c
      do l = 5, naxis
        if (nsize(l).gt.1) call bug('f','Too many dimensions for me')
        plane(l) = 1
      enddo
c
c  Allocate memory.
c
      rbuflen = 6*nOut(1)*nOut(2)
      call memalloc(pnt,rbuflen,'r')
      Indx = pnt
c
c  Open the output and create the header.
c
      call xyopen(lOut,outNam,'new',naxis,nOut)
      if (nfiles.gt.0) then
        call OldHdr(lIn(ref),lOut,naxis,nsize,blc,trc,version)
      else
        call NewHdr(lOut,naxis,Range,nsize,blc,version)
      endif
c
c  Let us recapitulate on what we have.
c  The expression (if doExp) is in ExpBuf,RBuf.
c  The mask expression (if doMask) is in MaskBuf,RBuf.
c  The boxes specification (if doRuns) is in boxes.
c
      do l = 1, nOut(4)
        Plane(4) = l + blc(4) - 1
        do k = 1, nOut(3)
          Plane(3) = k + blc(3) - 1
          do i = 1, nfiles
            if (naxes(i).gt.2) call xysetpl(lIn(i),naxes(i)-2,Plane(3))
          enddo
          call xysetpl(lOut,1,k)

          call BoxRuns(max(naxis-2,1),Plane(3),' ',boxes,
     *          Runs,MAXRUNS,nRuns,xblc,xtrc,yblc,ytrc)

          if (doMask) then
            npixels = Fill(Runs,nRuns)
            if (npixels.gt.0) then
              if (nMRB.gt.0) call MoveData(nMRB,MaskRBuf,RBuf(pnt))
              call ariExec(vaction,npixels,MaskBuf,BUFLEN,RBuf(pnt),
     *                                  RBufLen,Indx)
              Indx = Indx + pnt - 1
              call CompRuns(RBuf(Indx),
     *                          Runs,MAXRUNS,nRuns,Scratch,MAXRUNS)
            endif
          endif
          if (doMask .or. doRuns) call PutRuns(lOut,Runs,nRuns,
     *         1-blc(1),1-blc(2),nOut(1),nOut(2))
          if (doExp) then
            npixels = Fill(Runs,nRuns)
            if (npixels.gt.0) then
              if (nERB.gt.0) call MoveData(nERB,ExpRBuf,RBuf(pnt))
              call ariExec(vaction,npixels,ExpBuf,BUFLEN,RBuf(pnt),
     *                                  RBufLen,Indx)
              Indx = Indx + pnt - 1
            endif
            call PutPlane(lOut,Runs,nRuns,
     *         1-blc(1),1-blc(2),nOut(1),nOut(2),RBuf(Indx),npixels)
          endif
        enddo
      enddo
c
c  Free up the allocated memory.
c
      call memfree(pnt,rbuflen,'r')
c
c  Close all the files.
c
      call xyclose(lOut)
      do i = 1, nfiles
        call xyclose(lIn(i))
      enddo

      end

c***********************************************************************

      subroutine MoveData(n,inMap,outMap)

      integer n
      real    inMap(n),outMap(n)
c
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        outMap(i) = inMap(i)
      enddo

      end

c***********************************************************************

      subroutine CompRuns(Mask,Runs,maxRuns,nRuns,Scratch,nScratch)

      integer nRuns,maxRuns,nScratch
      integer Runs(3,maxRuns),Scratch(3,nScratch)
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
c    Runs       Input runs.
c    Nruns      Number of runs in the input.
c
c  Scratch:
c    Scratch    An intermediate working array.
c-----------------------------------------------------------------------
      integer i,k,l,length,nOut
c-----------------------------------------------------------------------
c
c  Initialise.
c
      nOut = 0
      k = 1
c
c  Determine the output runs, and place them in the scratch buffer.
c
      do i = 1, NRuns
        length = Runs(3,i) - Runs(2,i) + 1
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
            Scratch(1,nOut) = Runs(1,i)
            Scratch(2,nOut) = Runs(2,i) + l
            do while (l.lt.length .and. Mask(k).gt.0)
              k = k + 1
              l = l + 1
            enddo
            Scratch(3,nOut) = Runs(2,i) + l - 1
          endif
        enddo
      enddo
c
c  Copy the output runs (in the scratch buffer) to the runs array.
c
      if (nOut+1.gt.maxRuns) call bug('f',
     *           'Runs buffer overflow MAXRUNS)')
      do i = 1, nOut
        Runs(1,i) = Scratch(1,i)
        Runs(2,i) = Scratch(2,i)
        Runs(3,i) = Scratch(3,i)
      enddo
      nRuns = nOut
      Runs(1,nOut+1) = 0
      Runs(2,nOut+1) = 0
      Runs(3,nOut+1) = 0

      end

c***********************************************************************

      integer function Fill(Runs,nRuns)

      integer nRuns,Runs(3,*)

c  Determine the number of pixels of interest in the current plane.
c
c  Output:
c    Fill       The number of pixels of interest.
c
c-----------------------------------------------------------------------
      integer npixels,i
c-----------------------------------------------------------------------
      npixels = 0
      do i = 1, nRuns
        npixels = npixels + Runs(3,i) - Runs(2,i) + 1
      enddo

      Fill = npixels
      end

c***********************************************************************

      subroutine OldHdr(lIn,lOut,naxis,n,blc,trc,version)

      integer   lIn, lOut
      integer   naxis, n(naxis), blc(naxis), trc(naxis)
      character version*(*)

c  Make the header of the output file.  This is a carbon copy of the
c  input, except that a history record is added.
c
c-----------------------------------------------------------------------
      integer   iax, lblc, ltrc
      double precision crpix, def
      character card*80, keyw*8, txtblc*32, txttrc*32

      character itoaf*2
      external  itoaf
c-----------------------------------------------------------------------
c     Copy the header from the input map.
      call headcopy(lIn, lOut, 0, 0, 0, 0)

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

      subroutine NewHdr(lOut,naxis,Range,n,blc,version)

      integer   lOut, naxis, n(naxis), blc(naxis)
      real      Range(2,naxis)
      character version*(*)

c  Make the header of the output file. This is a carbon copy of the
c  input, except that a history record is added.
c
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

      subroutine PACTION(symbol,type,indx,value)

      character symbol*(*)
      integer type,indx
      real value

c The parsers action routine. Open the file and make sure its the
c  right size.
c
c  Inputs:
c    Symbol     The file name.
c  Outputs:
c    type       Vector.
c    indx       Indx into lIn array.
c    value      Unused.
c
c-----------------------------------------------------------------------
      include 'maths.h'

      integer error,constant,scalar,vector
      parameter (error=0,constant=1,scalar=2,vector=3)
      integer i
      integer nin(MAXNAX)
      character umsg*64
      logical dogrow
c
c  Externals.
c
      integer len1
c-----------------------------------------------------------------------
      if (symbol.eq.'x') then
        Xused = .true.
        Indx = XVAL
      else if (symbol.eq.'y') then
        Yused = .true.
        Indx = YVAL
      else if (symbol.eq.'z') then
        Zused = .true.
        Indx = ZVAL
c
c  Check if we already have this one open.
c
      else
        Indx = 0
        do i = 1, nfiles
          if (symbol.eq.Names(Offset(i)+1:Offset(i+1))) Indx = i
        enddo
c
c  If it was not found, open the file.
c
        if (Indx.eq.0) then
          if (nfiles.ge.MAXFILES) call bug('f','Too many open files')
          nfiles = nfiles + 1
          call xyopen(lIn(nfiles),Symbol,'old',MAXNAX,nin)
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
          Indx = nfiles
        endif
      endif

      type = vector

      end

c***********************************************************************

      subroutine VACTION(Indx,Type,Data,N)

      integer Indx,Type,N
      real Data(*)

c  This routine is called by ariExec each time it wants a row of a
c  file. Its not exactly the most complex routine in the world.
c
c  Inputs:
c    Indx       Indx into lIn array of file descriptors.
c    Type       Will be vector.
c    N          Will equal n1.
c  Output:
c    Data       The row of data.
c
c-----------------------------------------------------------------------
      include 'maths.h'
      integer i,j,k,npixel
      real cdelt,crval,temp
c-----------------------------------------------------------------------
c
c  Fill in Data if it corresponds to a value of x, y or z.
c
      if (Indx.eq.XVAL) then
        cdelt = (Range(2,1)-Range(1,1))/real(nsize(1)-1)
        crval = Range(1,1) - cdelt
        k = 1
        do j = 1, nRuns
          do i = Runs(2,j), Runs(3,j)
            Data(k) = cdelt * i + crval
            k = k + 1
          enddo
        enddo
      else if (Indx.eq.YVAL) then
        cdelt = (Range(2,2)-Range(1,2))/real(nsize(2)-1)
        crval = Range(1,2) - cdelt
        k = 1
        do j = 1, nRuns
          temp = cdelt * Runs(1,j) + crval
          do i = Runs(2,j), Runs(3,j)
            Data(k) = temp
            k = k + 1
          enddo
        enddo
      else if (Indx.eq.ZVAL) then
        cdelt = (Range(2,3)-Range(1,3))/real(nsize(3)-1)
        crval = Range(1,3) - cdelt
        temp = cdelt * Plane(3) + crval
        do k = 1, N
          Data(k) = temp
        enddo
c
c  Otherwise read it from a file.
c
      else
        call GetPlane(lIn(Indx),Runs,nRuns,0,0,nsize(1),nsize(2),
     *                                        Data,N,npixel)
        if (N.ne.npixel) call bug('f','Something is screwy in VACTION')
      endif
      end

c***********************************************************************

      subroutine getopt(grow,unmask)

      logical grow,unmask
c
c-----------------------------------------------------------------------
      integer NOPTS
      parameter (NOPTS=2)
      character opts(NOPTS)*8
      logical present(NOPTS)
      data opts/'grow    ','unmask  '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)

      grow = present(1)
      unmask = present(2)

      end
