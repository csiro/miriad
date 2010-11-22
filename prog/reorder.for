      program reorder

c= REORDER - Interchange the axes of a cube
c& rjs
c: map manipulation
c+
c       REORDER is a MIRIAD task to reorder and reverse axes within an
c       image.
c@ in
c       The input image.  No default.
c@ out
c       The output image.  No default.
c@ mode
c       The new axis ordering in terms of the old axis ordering: i.e.
c       "312" make input axis 3 the first output axis, input axis 1
c       the second output axis, etc.  Use "-" to reverse the pixel
c       order on an axis.  Do not include dummy axes in mode.
c
c$Id: reorder.for,v 1.4 2010/11/22 05:29:46 cal103 Exp $
c--
c
c  History:
c    rjs  ??????? Original version.
c    rjs   4oct89 Deleted unused variable.
c    mchw 20jun90 Added some header keywords.
c    mchw 09nov90 Added pbfwhm to header keywords.
c    pjt  16oct91 conversion to double prec coord sys
c                 made flint complain less  (index->idx, sign->idx)
c    mchw 04nov91 Check for dummy axes. Added HisInput and version.
c    mjs  05nov91 Eliminate 'catenated string in sub call' (for Cray)
c    rjs  30mar92 Uses memalloc/memfree routines.
c    rjs  25jan93 New trnio routines.  Handle masks (appease nebk?
c                 Never?).
c    rjs  03feb93 Fixed a bug dealing with 3rd axis, which effectively
c                 broke it.
c    rjs  02mar93 Break out "Inc" routines.
c    rjs  06sep93 Change ownership. Fix bug which inverted (!) the
c                 masking file.
c    rjs  07feb96 Handle 4th, etc, dimension.
c    rjs  02jul97 cellscal change.
c    rjs  23jul97 Add pbtype.
c    rjs  14jun00 Copy across llrot keyword.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      integer   axMap(MAXNAX), i, lIn, lmode, lOut, lu, n, nAxis,
     *          iAxLen(MAXNAX), oAxLen(MAXNAX), pnt, sgn, size
      real      ref(MAXBUF)
      character in*64, mode*16, out*64, version*72

      integer   len1
      logical   hdprsnt
      character versan*80
      external  hdprsnt, len1, versan

      common ref
c-----------------------------------------------------------------------
      version = versan('reorder',
     *                 '$Revision: 1.4 $',
     *                 '$Date: 2010/11/22 05:29:46 $')

c     Get the input parameters.
      call keyini
      call keya('in',in,' ')
      if (in.eq.' ') call bug('f','Input file name is missing')
      call keya('out',out,' ')
      if (out.eq.' ') call bug('f','Output file name is missing')
      call keya('mode',mode,' ')
      lmode = len1(mode)
      if (lmode.eq.0) call bug('f','The mode must be given')
      call keyfin

c     Parse the mode parameter.
      do i = 1, MAXNAX
        axMap(i) = i
      enddo

      n = 0
      sgn = 1
      do i = 1, lmode
        if (mode(i:i).eq.'-') then
          if (n.gt.MAXNAX) call bug('f','Too many dimensions for me')
          sgn = -1
        else if (mode(i:i).ge.'1' .and. mode(i:i).le.'9') then
          n = n + 1
          axMap(n) = ichar(mode(i:i)) - ichar('0')
          if (axMap(n).gt.MAXNAX)
     *      call bug('f','Too many dimensions for me')
          axMap(n) = sgn*axMap(n)
          sgn = 1
        else
          call bug('f','The mode parameter contains rubbish')
        endif
      enddo

c     Open the input file.
      call xyopen(lIn, in, 'old', MAXNAX, iAxLen)
      call rdhdi(lIn, 'naxis', nAxis, 0)
      nAxis = min(nAxis, MAXNAX)

c     Initialise the transpose routines and output.
      call trnini(lu, nAxis, iAxLen, mode)

      do i = 1, nAxis
        oAxLen(i) = iAxLen(abs(axMap(i)))
      enddo
      call xyopen(lOut, out, 'new', nAxis, oAxLen)
      call mkHead(lIn, lOut, nAxis, iAxLen, axMap, version)

      size = max(iAxLen(1)*iAxLen(2), oAxLen(1)*oAxLen(2))
      call memalloc(pnt,size,'r')

c     Do the real work.
      call PixRead(lu,lIn, ref(pnt),iAxLen(1),iAxLen(2),iAxLen,n)
      call PixWrit(lu,lOut,ref(pnt),oAxLen(1),oAxLen(2),oAxLen,n)
      if (hdprsnt(lIn,'mask')) then
        call MaskRead(lu,lIn, ref(pnt),iAxLen(1),iAxLen(2),iAxLen,n)
        call MaskWrit(lu,lOut,ref(pnt),oAxLen(1),oAxLen(2),oAxLen,n)
      endif

c     Tidy up.
      call memfree(pnt,size,'r')
      call xyclose(lOut)
      call trnfin(lu)
      call xyclose(lIn)

      end

c***********************************************************************

      subroutine PixRead(lu,lIn,Data,n1,n2,size,n)

      integer lu,lIn,n1,n2,n,size(n)
      real Data(n1,n2)
c-----------------------------------------------------------------------
c  Read the data and write it to the trnio routines.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer dims(MAXNAX),j

      logical Inc3More
c-----------------------------------------------------------------------
      call IncIni(n,size,dims)
      do while (Inc3More(n,size,dims))
        if (n.ge.3) call xysetpl(lIn,n-2,dims(3))
        do j = 1, n2
          call xyread(lIn,j,Data(1,j))
        enddo
        call trnwrite(lu,Data)
      enddo

      end

c***********************************************************************

      subroutine PixWrit(lu,lOut,Data,n1,n2,size,n)

      integer lu,lOut,n1,n2,n,size(n)
      real Data(n1,n2)
c-----------------------------------------------------------------------
c  Read the data and write it to the trnio routines.
c-----------------------------------------------------------------------
      include 'maxnax.h'
      integer dims(MAXNAX),j

      logical Inc3More
c-----------------------------------------------------------------------
      call IncIni(n,size,dims)
      do while (Inc3More(n,size,dims))
        call trnread(lu,Data)
        if (n.ge.3) call xysetpl(lOut,n-2,dims(3))
        do j = 1, n2
          call xywrite(lOut,j,Data(1,j))
        enddo
      enddo

      end

c***********************************************************************

      subroutine MaskRead(lu,lIn,Data,n1,n2,size,n)

      integer lu,lIn,n1,n2,n,size(n)
      real Data(n1,n2)
c-----------------------------------------------------------------------
c  Read the data and write it to the trnio routines.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      integer dims(MAXNAX),i,j
      logical flags(MAXDIM)

      logical Inc3More
c-----------------------------------------------------------------------
      call IncIni(n,size,dims)
      do while (Inc3More(n,size,dims))
        if (n.ge.3) call xysetpl(lIn,n-2,dims(3))
        do j = 1, n2
          call xyflgrd(lIn,j,flags)
          do i = 1, n1
            Data(i,j) = 0
            if (flags(i)) Data(i,j) = 1
          enddo
        enddo
        call trnwrite(lu,Data)
      enddo

      end

c***********************************************************************

      subroutine MaskWrit(lu,lOut,Data,n1,n2,size,n)

      integer lu,lOut,n1,n2,n,size(n)
      real Data(n1,n2)
c-----------------------------------------------------------------------
c  Read the data and write it to the trnio routines.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      integer dims(MAXNAX),i,j
      logical flags(MAXDIM)

      logical Inc3More
c-----------------------------------------------------------------------
      call IncIni(n,size,dims)
      do while (Inc3More(n,size,dims))
        call trnread(lu,Data)
        if (n.ge.3) call xysetpl(lOut,n-2,dims(3))
        do j = 1, n2
          do i = 1, n1
            flags(i) = Data(i,j).gt.0.1
          enddo
          call xyflgwr(lOut,j,flags)
        enddo
      enddo

      end

c***********************************************************************

      subroutine mkHead(lIn, lOut, nAxis, iAxLen, axMap, version)

      integer   lIn, lOut, nAxis, iAxLen(nAxis), axMap(nAxis)
      character version*72
c-----------------------------------------------------------------------
c  Write a header for the output file.
c
c  Inputs:
c    lIn,lOut   Handle of input and output files.
c    nAxis      Number of axes (true plus dummy) in input and output.
c    iAxLen     Length of each axis in the input image.
c    axMap      Mapping from input to output axes, negative if reversed.
c    version    Version of task.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   blc(MAXNAX), iax
c-----------------------------------------------------------------------
c     Copy the input header with axis permutation.
      do iax = 1, nAxis
        blc(iax) = 1
      enddo

      call headcp(lIn, lOut, nAxis, axMap, blc, iAxLen)

c     Update history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'REORDER: Miriad ' // version)
      call hisinput(lOut, 'REORDER')
      call hisclose(lOut)

      end
