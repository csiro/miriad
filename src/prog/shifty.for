      program shifty

c= shifty - Align two images.
c& rjs
c: image analysis
c+
c       SHIFTY is a MIRIAD task that shifts one image by an integral
c       number of pixels to align it as well as possible with another
c       image.
c@ in1
c       The first input image is considered to be the master.
c       No default.
c@ in2
c       The second input image is shifted to align it with the master.
c       No default.
c@ out
c       The output image.  No default.
c
c$Id: shifty.for,v 1.6 2021/06/02 04:45:09 wie017 Exp $
c--
c  History:
c    rjs  21nov90 Original version.
c    rjs  11apr91 Changed "mitoa" to "mitoaf".
c    rjs  15dec92 Use memalloc.
c    rjs   1may96 Fix coordinates for Brett.
c    rjs  02jul97 cellscal change.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer   i, lIn1, lIn2, lOut, length, naxis, nsize1(3),
     *          nsize2(3), shift(2)
      character in1*64, in2*64, out*64, text*80, version*72

      character versan*72
      external  versan
c-----------------------------------------------------------------------
      version = versan('shifty',
     *                 '$Revision: 1.6 $',
     *                 '$Date: 2021/06/02 04:45:09 $')

c     Get the input parameters and check them.
      call keyini
      call keya('in1',in1,' ')
      call keya('in2',in2,' ')
      if (in1.eq.' ' .or. in2.eq.' ')
     *  call bug('f','Input image names must be given')
      call keya('out',out,' ')
      if (out.eq.' ')
     *  call bug('f','Output image name must be given')
      call keyfin

c     Open the two inputs and check that they are the same size.
      call xyopen(lIn1,in1,'old',3,nsize1)
      call xyopen(lIn2,in2,'old',3,nsize2)
      do i = 1, 3
        if (i.le.2 .and. nsize1(i).gt.maxdim)
     *    call bug('f','Input image too big to handle')
        if (nsize1(i).ne.nsize2(i))
     *    call bug('f','Input image sizes are not the same')
      enddo

c     Determine the shift.
      call DetShift(lIn1,lIn2,nsize1(1),nsize1(2),nsize1(3),
     *              shift(1),shift(2))

c     Open an output image and apply the shift.
      call rdhdi(lIn2,'naxis',naxis,3)
      naxis = min(naxis,3)
      call xyopen(lOut,out,'new',naxis,nsize1)

      call DatShift(lIn2,lOut,nsize1(1),nsize1(2),nsize1(3),
     *              shift(1),shift(2))

c     Copy the output header verbatim from the master input image.
      call headcp(lIn1, lOut, 0, 0, 0, 0)

c     Update history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'SHIFTY: Miriad ' // version)
      call hisinput(lOut, 'SHIFTY')

      call mitoaf(shift,2,text,length)
      call output('Applying a shift of ('//text(1:length)//')')
      call hiswrite(lOut,'SHIFTY: Shift = ('//text(1:length)//')')
      call hisclose(lOut)

c     All said and done. Close up.
      call xyclose(lIn1)
      call xyclose(lIn2)
      call xyclose(lOut)

      end

c***********************************************************************

      subroutine DetShift(lIn1,lIn2,n1,n2,n3,xshift,yshift)

      integer lIn1,lIn2,n1,n2,n3,xshift,yshift
c-----------------------------------------------------------------------
c  This determines, by a cross-correlation technique, the best shifts
c  to apply to image 2, to register it with image 1.
c
c  Inputs:
c    lIn1,lIn2  The file handles of image 1 and image 2.
c    n1,n2,n3   The dimensions of the cube.
c  Outputs:
c    xshift,yshift The shift to apply to image 2.  The best shift is to
c               change pixel (i,j) in image 2, to pixel
c               (i+xshift,j+yshift).
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      ptrdiff iIn1,iIn2,iOut,iScr1,iScr2, i
      integer j,k

c     Externals.
      integer isamax
c-----------------------------------------------------------------------
c
c  Allocate space in our work buffer.
c
      call memalloc(iIn1,n1*n2,'r')
      call memalloc(iIn2,n1*n2,'r')
      call memalloc(iOut,n1*n2,'r')
      call memalloc(iScr1,(n1/2+1)*n2,'c')
      call memalloc(iScr2,(n1/2+1)*n2,'c')
c
c  Initialise the output work buffer.
c
      do i = iOut, iOut+n1*n2-1
        MemR(i) = 0
      enddo
c
c  Loop over all the planes.
c
      do k = 1, n3
        call ImLoad(lIn1,n1,n2,k,MemR(iIn1))
        call ImLoad(lIn2,n1,n2,k,MemR(iIn2))
        call Xcorr(MemR(iIn1),MemR(iIn2),n1,n2,MemR(iOut),
     *    MemC(iScr1),MemC(iScr2))
      enddo
c
c  Now find the peak in the output, and thats that.
c
      j = isamax(n1*n2,MemR(iOut),1) - 1
      yshift = j / n1
      if (yshift.gt.n2/2) yshift = yshift - n2
      xshift = mod(j,n1)
      if (xshift.gt.n1/2) xshift = xshift - n1
c
c  Free up the allocated memory.
c
      call memfree(iIn1,n1*n2,'r')
      call memfree(iIn2,n1*n2,'r')
      call memfree(iOut,n1*n2,'r')
      call memfree(iScr1,(n1/2+1)*n2,'c')
      call memfree(iScr2,(n1/2+1)*n2,'c')

      end

c***********************************************************************

      subroutine ImLoad(lu,n1,n2,k,Out)

      integer lu,n1,n2,k
      real Out(n1,n2)
c-----------------------------------------------------------------------
c  Read in a plane of an image.
c-----------------------------------------------------------------------
      integer j
c-----------------------------------------------------------------------
      call xysetpl(lu,1,k)
      do j = 1, n2
        call xyread(lu,j,Out(1,j))
      enddo

      end

c***********************************************************************

      subroutine Xcorr(in1,in2,n1,n2,out,scr1,scr2)

      integer n1,n2
      real in1(n1,n2),in2(n1,n2),out(n1,n2)
      complex scr1(n1/2+1,n2),scr2(n1/2+1,n2)
c-----------------------------------------------------------------------
c  This calculates the cross correlation between two images.  The result
c  is added to "out".
c
c  Inputs:
c    in1,in2    The two images to cross-correlate.
c    n1,n2      The dimensions of the two images.
c  Input/Output:
c    out        The resultant cross-correlation is added to this image.
c  Scratch:
c    scr1,scr2  These are used to hold FFTs of the input images.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer i,j
      real RTemp(maxdim)
      complex CTemp1(maxdim),CTemp2(maxdim)
c-----------------------------------------------------------------------
c
c  First pass of the FFT.
c
      do j = 1, n2
        call fftrc(In1(1,j),Scr1(1,j),-1,n1)
      enddo
      do j = 1, n2
        call fftrc(In2(1,j),Scr2(1,j),-1,n1)
      enddo
c
c  Second pass of the FFT.
c
      do i = 1, n1/2+1
        do j = 1, n2
          CTemp1(j) = Scr1(i,j)
        enddo
        call fftcc(CTemp1,CTemp2,-1,n2)
        do j = 1, n2
          Scr1(i,j) = CTemp2(j)
        enddo
      enddo

      do i = 1, n1/2+1
        do j = 1, n2
          CTemp1(j) = Scr2(i,j)
        enddo
        call fftcc(CTemp1,CTemp2,-1,n2)
        do j = 1, n2
          Scr2(i,j) = CTemp2(j)
        enddo
      enddo
c
c  Multiply the two.
c
      do j = 1, n2
        do i = 1, n1/2+1
          Scr1(i,j) = Scr1(i,j)*conjg(Scr2(i,j))
        enddo
      enddo
c
c  Take the inverse FFT of Scr1.
c
      do i = 1, n1/2+1
        do j = 1, n2
          CTemp1(j) = Scr1(i,j)
        enddo
        call fftcc(CTemp1,CTemp2,+1,n2)
        do j = 1, n2
          Scr1(i,j) = CTemp2(j)
        enddo
      enddo
c
c  Last pass.
c
      do j = 1, n2
        call fftcr(Scr1(1,j),RTemp,+1,n1)
        do i = 1, n1
          Out(i,j) = Out(i,j) + RTemp(i)
        enddo
      enddo

      end

c***********************************************************************

      subroutine DatShift(lIn,lOut,n1,n2,n3,xshift,yshift)

      integer lIn,lOut,n1,n2,n3,xshift,yshift
c-----------------------------------------------------------------------
c  This performs a shift on the input cube, and writes it to an output
c  cube.
c
c  Inputs:
c    lIn,lOut   The handles of the files of the input and output cubes.
c    n1,n2,n3   The dimensions of the cubes.
c    xshift,yshift The shifts to apply. Pixel (i,j) in the input cube
c               becomes pixel (i+xshift,j+yshift) in the output cube.
c               The shift is cyclic.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real DatIn(maxdim),DatOut(maxdim)
      integer i,j,jd,k,xshiftd,yshiftd
c-----------------------------------------------------------------------
c
c  Find the negative of the shift.
c
      xshiftd = mod(-xshift,n1)
      if (xshiftd.lt.0) xshiftd = xshiftd + n1
      yshiftd = mod(-yshift,n2)
      if (yshiftd.lt.0) yshiftd = yshiftd + n2
c
c  Loop over the cube. Read a row, shift it, and then write it out.
c
      do k = 1, n3
        call xysetpl(lIn,1,k)
        call xysetpl(lOut,1,k)
        do j = 1, n2
          jd = mod(j + yshiftd - 1,n2) + 1
          call xyread(lIn,jd,DatIn)
          if (xshiftd.eq.0) then
            call xywrite(lOut,j,DatIn)
          else
            do i = 1, n1-xshiftd
              DatOut(i) = DatIn(i+xshiftd)
            enddo
            do i = n1-xshiftd+1, n1
              DatOut(i) = DatIn(i+xshiftd-n1)
            enddo
            call xywrite(lOut,j,DatOut)
          endif
        enddo
      enddo

      end
