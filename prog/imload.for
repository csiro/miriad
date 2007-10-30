c************************************************************************
	program imload
c
c= IMLOAD - Make a Miriad image from an ascii file.
c& rjs
c: image conversion, analysis.
c+
c	ImLoad reads in an image given as pixel values in an ascii file.
c	You can put as many pixels per line as you like (upto MAXDIM;
c	typically 2048) in the text file so that an image row can
c	be on one line or straddle many lines.  The first row of pixels 
c	read from the text file is the bottom row of the image.
c@ in
c	Text file containing the image pixel values. No default.
c@ imsize
c	Image size. 1 to 3 values nx,ny,nz. Default ny=nx, and nz=1.
c@ out
c	Output image. No default.
c--
c  History:
c    mchw  12feb91  Initial version.
c    jat   12feb91  added my own "getimage" subroutine
c    pjt   16pct91  conversion to double prec coord sys
c                   made flint less verbose
c    mjs   02jul93  remove unused format stmt to elim compiler warning.
c    rjs   22oct93  Adapted from imwrite.
c    nebk  15nov93  DOc change
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='ImLoad: version  15-Nov-93')
	character*64 image*64,out*64
	integer lout,nsize(3)
c
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('in',image,' ')
	call keyi('imsize',nsize(1),0)
	call keyi('imsize',nsize(2),nsize(1))
	call keyi('imsize',nsize(3),1)
	call keya('out',Out,' ')
	call keyfin
c
c  Check the reasonableness of the inputs.
c
	if(nsize(1)*nsize(2)*nsize(3).le.0)
     *		call bug('f','imsize unreasonable')
	if(out.eq.' ') call bug('f','Output image missing')
	if(image.eq.' ')call bug('f','Input image must be given')
c
c  Open the output array
c
	call xyopen(lOut,Out,'new',3,nsize)
c
c  Get image pixel values.
c
	call getpixel(lOut,nsize,image)
c
c  Write the history file.
c
	call hisopen(lOut,'write')
        call hiswrite(lOut,'IMWRITE: Miriad '//version)
	call hisinput(lOut,'IMWRITE')
	call hisclose(lOut)
c
c  Close the files after writing history
c
	call xyclose(lOut)
c
	end
c************************************************************************
	subroutine getpixel(lOut,nsize,in)
c
	implicit none
	integer lOut,nsize(3)
	character in*(*)
c
c  Load the pixel data.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer lu,i,j,k,iostat,ipt,npt
	real data(MAXDIM),buf(MAXDIM)
c
c  Open the text file.
c
	call txtopen(lu,in,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening text file')
	  call bugno('f',iostat)
	endif
c
	ipt = 1
	npt = 0
c
	do k=1,nsize(3)
	  call xysetpl(lOut,1,k)
	  do j=1,nsize(2)
	    do i=1,nsize(1)
	      dowhile(ipt.gt.npt)
		ipt = 1
		call DatGet(lu,buf,MAXDIM,npt)
	      enddo
	      data(i) = buf(ipt)
	      ipt = ipt + 1
	    enddo
	    call xywrite(lOut,j,data)
	  enddo
	enddo
c
	call txtclose(lu,iostat)
c
	end
c************************************************************************
	subroutine DatGet(lu,buf,nmax,npt)
c
	implicit none
	integer lu,nmax,npt
	real buf(nmax)
c
c------------------------------------------------------------------------
	integer iostat,k1,k2,length
	double precision d
	character string*256,token*32
	logical ok
c
c  Externals.
c
	integer len1
c
	call txtread(lu,string,k2,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	if(k2.gt.len(string))call bug('f','Line too long')
c
	npt = 0
	if(k2.gt.0)k2 = len1(string(1:k2))
	k1 = 1
c
	dowhile(k1.le.k2)
          call getfield(string,k1,k2,token,length)
          call atodf(token(1:length),d,ok)
          if(.not.ok)call bug('f','Error decoding line')
	  npt = npt + 1
	  buf(npt) = d
	enddo
c
	end
