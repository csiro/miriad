      program velcolor

c= VELCOLOR - Make an RGB image cube to display velocity as color.
c& mchw
c: image analysis
c+
c       VELCOLOR makes a three-plane, red-green-blue, Miriad image cube
c       that can be used to generate a colour display of the third axis
c       of a data cube, typically velocity.  The RGB planes must be
c       superposed in a suitable viewer.
c
c       The algorithm weights the channels so that the apparent
c       intensity of the superposed RGB planes is independent of color.
c       For more details see Heiles & Jenkins (1976), A&A, 46, 33.
c
c@ in
c       The input image.  No default.
c@ region
c       The region of the input image to be used.  The 3rd axis region
c       determines the range from blue to red.  See documentation on
c       region for help of how to specify this.  Only the bounding box
c       is supported.
c@ pivot
c       Center channel of input image for output green image.
c       Default is 0.4*trc+0.6*blc
c@ out
c       The output RGB cube.  No default.
c@ clip
c       Two values.  Exclude pixels with values in the range clip(1) to
c       clip(2).  If only one value is given, then exclude -abs(clip) to
c       +abs(clip).
c
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'tmpdim.h'

      integer   MAXNAX, MAXBOXES
      parameter (MAXNAX=3, MAXBOXES=2048)

      integer   blc(MAXNAX), boxes(MAXBOXES), i, j, lin, lout, naxis,
     *          nsize(MAXNAX), size(MAXNAX), trc(MAXNAX)
      real      bhi, blo, clip(2), pivot
      character in*256, out*256, line*72, version*72

      external  keyprsnt, versan
      logical   keyprsnt
      character versan*72
c-----------------------------------------------------------------------
      version = versan('velcolor')

c     Get inputs.
      call keyini
      call keya('in',in,' ')
      call BoxInput('region',in,boxes,MAXBOXES)
      call keya('out',out,' ')
      call keyr('pivot',pivot,0.0)
      call keyr('clip',clip(1),0.0)
      if (keyprsnt('clip')) then
        call keyr('clip',clip(2),0.0)
      else
        clip(2) = abs(clip(1))
        clip(1) = -clip(2)
      endif
      call keyfin

c     Check inputs.
      if (in .eq.' ') call bug('f','No input specified. (in=)')
      if (out.eq.' ') call bug('f','No output specified. (out=)')
      if (clip(2).lt.clip(1)) call bug('f','clip range out of order')

      call xyopen(lin,in,'old',MAXNAX,nsize)
      call rdhdi(lin,'naxis',naxis,0)
      naxis = min(naxis,MAXNAX)
      if (nsize(1).gt.MAXDIM) call bug('f','Input file too big for me')

c     Determine the min and max image values.
      call ImMinMax(lIn,naxis,nsize,blo,bhi)
      if (blo.eq.bhi) then
        call xyclose(lIn)
        write(line,'(''All pixels are '',1pg10.3)') blo
        call output(line)
        stop
      endif

c     Set up the region of interest.  Warn if not rectangle.
      call BoxSet(boxes,MAXNAX,nsize,'s')
      call BoxInfo(boxes,MAXNAX,blc,trc)

c     Open output image and fill in its header.
      j = 0
      do i = 1, naxis
        if (blc(i).lt.1) blc(i) = 1
        if (trc(i).gt.nsize(i)) trc(i) = nsize(i)
        if (i.ne.3) then
          j = j + 1
          size(j) = trc(i) - blc(i) + 1
        endif
      enddo

      size(3) = 3
      call xyopen(lOut,out,'new',naxis,size)
      call mkHead(lIn,lOut,naxis,blc,trc,pivot,version)

c     Calculate the red-green-blue image planes.
      call velcolor3(lIn,lOut,naxis,blc,trc,clip,pivot)

c     Close files.
      call xyclose(lIn)
      call xyclose(lOut)

      end

c***********************************************************************

      subroutine mkHead(lIn,lOut,naxis,blc,trc,pivot,version)

      integer   lIn, lOut, naxis, blc(naxis), trc(naxis)
      real      pivot
      character version*72
c-----------------------------------------------------------------------
c  Copy keywords to output file.
c
c  Inputs:
c    lIn,lOut   Handle of input and output files.
c    naxis      The number of input axes.
c    blc,trc    The corners of the input image.
c    pivot      Center channel for green image.
c-----------------------------------------------------------------------
      include 'maxnax.h'

      integer   axMap(MAXNAX), iax
      real      cdelt, crpix, crval
c-----------------------------------------------------------------------
c     Copy the input header with sub-imaging.
      do iax = 1, naxis
        axMap(iax) = iax
      enddo

      call headcp(lIn, lOut, naxis, axMap, blc, trc)

c     Fix up the third (colour) axis.
      call rdhdr(lin, 'crpix3', crpix, 1.0)
      call rdhdr(lin, 'cdelt3', cdelt, 1.0)
      call rdhdr(lin, 'crval3', crval, 0.0)

      if (pivot.eq.0.0) pivot=0.4*trc(3)+0.6*blc(3)
      call wrhdr(lout,'crpix3', 2.0)
      call wrhdr(lout,'cdelt3', cdelt*(pivot-blc(3)))
      call wrhdr(lout,'crval3', crval + cdelt*(pivot-crpix))

c     Update history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'VELCOLOR: ' // version)
      call hisinput(lOut, 'VELCOLOR')
      call hisclose(lOut)

      end

c***********************************************************************

      subroutine velcolor3(lIn,lOut,naxis,blc,trc,clip,pivot)

      integer lIn,lOut,naxis,blc(naxis),trc(naxis)
      real   clip(2),pivot
c-----------------------------------------------------------------------
c  Calculate the velcolor for axis 3.
c
c  Inputs:
c    lIn,lOut   Handle of input and output files.
c    naxis      The number of input axes.
c    blc,trc    The corners of the input image.
c    clip       Pixels with values in range clip(1:2) are excluded.
c    pivot      Center channel for green image.
c-----------------------------------------------------------------------
      include 'tmpdim.h'

      logical flags(MAXDIM)
      integer i, j, k, n
      real    col(3), buf(MAXDIM), out(MAXDIM,MAXDIM,3)
c-----------------------------------------------------------------------
c     Initialize the output array.
      do n = 1, 3
        do j = 1, MAXDIM
          do i = i, MAXDIM
            out(i,j,n) = 0.0
          enddo
        enddo
      enddo

c     Loop through the velocity channels and accumulate the colors.
      do k = blc(3), trc(3)
        col(1) = max(0.0,(pivot-k)/(pivot-blc(3)))
        col(3) = max(0.0,(k-pivot)/(trc(3)-pivot))
        col(2) = min(1.0-col(1),1.0-col(3))
        call xysetpl(lIn,1,k)
        do j = blc(2), trc(2)
          call xyread(lIn,j,buf)
          call xyflgrd(lIn,j,flags)
          do i = blc(1), trc(1)
            if (flags(i) .and.
     *          (buf(i).le.clip(1) .or. buf(i).ge.clip(2))) then
              do n = 1, 3
                out(i,j,n) = out(i,j,n) + col(n)*buf(i)
              enddo
            endif
          enddo
        enddo
      enddo

c     Write it out.
      do n = 3, 1, -1
        call xysetpl(lOut,1,n)
        do j = blc(2), trc(2)
          call xywrite(lOut,j-blc(2)+1,out(blc(1),j,n))
        enddo
      enddo

      end
