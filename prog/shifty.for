c************************************************************************
	program shifty
	implicit none
c
c= shifty - Align two images.
c& rjs
c: image analysis
c+
c	SHIFTY is a MIRIAD task which shifts one image, to that it best aligns
c	with another image. Only integral pixel shifts are performed.
c@ in1
c	The first input image. This is considered the master image. No default
c@ in2
c	The second input image. This is the image that is shifted to make it
c	align better with the master image. No default.
c@ out
c	The output image. No default.
c--
c  History:
c    rjs  21nov90 Original version.
c    rjs  11apr91 Changed "mitoa" to "mitoaf".
c    rjs  15dec92 Use memalloc.
c    rjs   1may96 Fix coordinates for Brett.
c    rjs  02jul97 cellscal change.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='version 1.0 1-May-96')
	include 'maxdim.h'
	character in1*64,in2*64,out*64,text*80
	integer lIn1,lIn2,lOut,nsize1(3),nsize2(3),shift(2),length,i
	integer naxis
c
c  Header keywords.
c
	integer nkeys
	parameter(nkeys=43)
	character keyw(nkeys)*8
	data keyw/   'bunit   ','crota1  ','crota2  ','crota3  ',
     *	  'crota4  ','crota5  ','crval1  ','crval2  ','crval3  ',
     *	  'crval4  ','crval5  ','ctype1  ','ctype2  ','ctype3  ',
     *	  'ctype4  ','ctype5  ','obstime ','epoch   ','history ',
     *	  'instrume','niters  ','object  ','telescop','observer',
     *	  'restfreq','vobs    ','cellscal','obsra   ',
     *	  'obsdec  ','lstart  ','lstep   ','ltype   ','lwidth  ',
     *	  'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','cdelt5  ',
     *	  'crpix1  ','crpix2  ','crpix3  ','crpix4  ','crpix5  '/
c
c  Get the input parameters, and check them.
c
	call output('Shifty '//version)
	call keyini
	call keya('in1',in1,' ')
	call keya('in2',in2,' ')
	if(in1.eq.' '.or.in2.eq.' ')
     *	  call bug('f','Input image names must be given')
	call keya('out',out,' ')
	if(out.eq.' ')
     *	  call bug('f','Output image name must be given')
	call keyfin
c
c  Open the two inputs and check that they are the same size.
c
	call xyopen(lIn1,in1,'old',3,nsize1)
	call xyopen(lIn2,in2,'old',3,nsize2)
	do i=1,3
	  if(i.le.2.and.nsize1(i).gt.maxdim)
     *	    call bug('f','Input image too big to handle')
	  if(nsize1(i).ne.nsize2(i))
     *	    call bug('f','Input image sizes are not the same')
	enddo
c
c  Determine the shift.
c
	call DetShift(lIn1,lIn2,nsize1(1),nsize1(2),nsize1(3),
     *						   shift(1),shift(2))
c
c  Open an output image, and apply the shift.
c
	call rdhdi(lIn2,'naxis',naxis,3)
	naxis = min(naxis,3)
	call xyopen(lOut,out,'new',naxis,nsize1)
c
	call DatShift(lIn2,lOut,nsize1(1),nsize1(2),nsize1(3),
     *						   shift(1),shift(2))
c
c  Make the header of the output. This is but a straight copy of the input.
c
	do i=1,nkeys
	  call hdcopy(lIn1,lOut,keyw(i))
	enddo
	call hisopen(lOut,'append')
        call hiswrite (lOut, 'SHIFTY: Miriad Shifty '//version)
	call hisinput(lOut,'SHIFTY')
	call mitoaf(shift,2,text,length)
	call output('Applying a shift of ('//text(1:length)//')')
	call hiswrite(lOut,'SHIFTY: Shift = ('//text(1:length)//')')
	call hisclose(lOut)
c
c  All said and done. Close up.
c
	call xyclose(lIn1)
	call xyclose(lIn2)
	call xyclose(lOut)
	end
c************************************************************************
	subroutine DetShift(lIn1,lIn2,n1,n2,n3,xshift,yshift)
c
	implicit none
	integer lIn1,lIn2,n1,n2,n3,xshift,yshift
c
c  This determines, by a cross-correlation technique, the best shifts
c  to apply to image 2, to register it with image 1.
c
c  Inputs:
c    lIn1,lIn2	The file handles of image 1 and image 2.
c    n1,n2,n3	The dimensions of the cube.
c  Outputs:
c    xshift,yshift The shift to apply to image 2. The best shift is to
c		change pixel (i,j) in image 2, to pixel (i+xshift,j+yshift).
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer iIn1,iIn2,iOut,iScr1,iScr2,i,k
	real Scratch(maxbuf)
	common Scratch
c
c  Externals.
c
	integer isamax
c
c  Allocate space in our work buffer.
c
	call memalloc(iIn1,n1*n2,'r')
	call memalloc(iIn2,n1*n2,'r')
	call memalloc(iOut,n1*n2,'r')
	call memalloc(iScr1,2*(n1/2+1)*n2,'r')
	call memalloc(iScr2,2*(n1/2+1)*n2,'r')
c
c  Initialise the output work buffer.
c
	do i=iOut,iOut+n1*n2-1
	  Scratch(i) = 0
	enddo
c
c  Loop over all the planes.
c
	do k=1,n3
	  call ImLoad(lIn1,n1,n2,k,Scratch(iIn1))
	  call ImLoad(lIn2,n1,n2,k,Scratch(iIn2))
	  call Xcorr(Scratch(iIn1),Scratch(iIn2),n1,n2,Scratch(iOut),
     *	    Scratch(iScr1),Scratch(iScr2))
	enddo
c
c  Now find the peak in the output, and thats that.
c
	i = isamax(n1*n2,Scratch(iOut),1) - 1
	yshift = i / n1
	if(yshift.gt.n2/2) yshift = yshift - n2
	xshift = mod(i,n1)
	if(xshift.gt.n1/2) xshift = xshift - n1
c
c  Free up the allocated memory.
c
	call memfree(iIn1,n1*n2,'r')
	call memfree(iIn2,n1*n2,'r')
	call memfree(iOut,n1*n2,'r')
	call memfree(iScr1,2*(n1/2+1)*n2,'r')
	call memfree(iScr2,2*(n1/2+1)*n2,'r')
c
	end
c************************************************************************
	subroutine ImLoad(lu,n1,n2,k,Out)
c
	implicit none
	integer lu,n1,n2,k
	real Out(n1,n2)
c
c  Read in a plane of an image.
c
c------------------------------------------------------------------------
	integer j
	call xysetpl(lu,1,k)
	do j=1,n2
	  call xyread(lu,j,Out(1,j))
	enddo
	end
c************************************************************************
	subroutine Xcorr(in1,in2,n1,n2,out,scr1,scr2)
c
	implicit none
	integer n1,n2
	real in1(n1,n2),in2(n1,n2),out(n1,n2)
	complex scr1(n1/2+1,n2),scr2(n1/2+1,n2)
c
c  This calculates the cross correlation between two images. The result is
c  added to "out".
c
c  Inputs:
c    in1,in2	The two images to cross-correlate.
c    n1,n2	The dimensions of the two images.
c  Input/Output:
c    out	The resultant cross-correlation is added to this image.
c  Scratch:
c    scr1,scr2	These are used to hold FFTs of the input images.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j
	real RTemp(maxdim)
	complex CTemp1(maxdim),CTemp2(maxdim)
c
c  First pass of the FFT.
c
	do j=1,n2
	  call fftrc(In1(1,j),Scr1(1,j),-1,n1)
	enddo
	do j=1,n2
	  call fftrc(In2(1,j),Scr2(1,j),-1,n1)
	enddo
c
c  Second pass of the FFT.
c
	do i=1,n1/2+1
	  do j=1,n2
	    CTemp1(j) = Scr1(i,j)
	  enddo
	  call fftcc(CTemp1,CTemp2,-1,n2)
	  do j=1,n2
	    Scr1(i,j) = CTemp2(j)
	  enddo
	enddo
c
	do i=1,n1/2+1
	  do j=1,n2
	    CTemp1(j) = Scr2(i,j)
	  enddo
	  call fftcc(CTemp1,CTemp2,-1,n2)
	  do j=1,n2
	    Scr2(i,j) = CTemp2(j)
	  enddo
	enddo
c
c  Multiply the two.
c
	do j=1,n2
	  do i=1,n1/2+1
	    Scr1(i,j) = Scr1(i,j)*conjg(Scr2(i,j))
	  enddo
	enddo
c
c  Take the inverse FFT of Scr1.
c
	do i=1,n1/2+1
	  do j=1,n2
	    CTemp1(j) = Scr1(i,j)
	  enddo
	  call fftcc(CTemp1,CTemp2,+1,n2)
	  do j=1,n2
	    Scr1(i,j) = CTemp2(j)
	  enddo
	enddo
c
c  Last pass.
c
	do j=1,n2
	  call fftcr(Scr1(1,j),RTemp,+1,n1)
	  do i=1,n1
	    Out(i,j) = Out(i,j) + RTemp(i)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine DatShift(lIn,lOut,n1,n2,n3,xshift,yshift)
c
	implicit none
	integer lIn,lOut,n1,n2,n3,xshift,yshift
c
c  This performs a shift on the input cube, and writes it to an output
c  cube.
c
c  Inputs:
c    lIn,lOut	The handles of the files of the input and output cubes.
c    n1,n2,n3	The dimensions of the cubes.
c    xshift,yshift The shifts to apply. Pixel (i,j) in the input cube
c		becomes pixel (i+xshift,j+yshift) in the output cube.
c		The shift is cyclic.
c------------------------------------------------------------------------
	include 'maxdim.h'
	real DatIn(maxdim),DatOut(maxdim)
	integer i,j,jd,k,xshiftd,yshiftd
c
c  Find the negative of the shift.
c
	xshiftd = mod(-xshift,n1)
	if(xshiftd.lt.0) xshiftd = xshiftd + n1
	yshiftd = mod(-yshift,n2)
	if(yshiftd.lt.0) yshiftd = yshiftd + n2
c
c  Loop over the cube. Read a row, shift it, and then write it out.
c
	do k=1,n3
	  call xysetpl(lIn,1,k)
	  call xysetpl(lOut,1,k)
	  do j=1,n2
	    jd = mod(j + yshiftd - 1,n2) + 1
	    call xyread(lIn,jd,DatIn)
	    if(xshiftd.eq.0)then
	      call xywrite(lOut,j,DatIn)
	    else
	      do i=1,n1-xshiftd
		DatOut(i) = DatIn(i+xshiftd)
	      enddo
	      do i=n1-xshiftd+1,n1
		DatOut(i) = DatIn(i+xshiftd-n1)
	      enddo
	      call xywrite(lOut,j,DatOut)
	    endif
	  enddo
	enddo
c
c  All said and done.
c
	end
