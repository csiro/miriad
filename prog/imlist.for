c**********************************************************************c
      program ImList
      implicit none
c
c= IMLIST - List items and pixel values from an image
c& rjs
c: image analysis
c+
c       IMLIST lists a Miriad image. By default it lists the value of
c	pixels. Alternatively, it can list an image's mosaic table, or
c	some statistics about an image.
c@ in
c	Input image name. No default.
c@ options
c	Severval options can be given (separated by commas), and can be
c	abbreviated to uniqueness. Possible options are:
c	  data       List some data.
c	  mosaic     List the mosaic table of an image (if present).
c	  statistics List total flux, min and max and rms for each plane.
c@ region
c	The region of interest. See the help on "region" for more
c	information. This gives the region of pixels to be listed, or
c	the region for which statistics are calculated. Only rectangular
c	regions of interest are supported. The default is the entire image
c	(which is usually too big for listing).
c@ format
c	This gives the FORTRAN format for the listing of pixel values.
c	For example: 1pe11.4, f5.2, 1pg12.5. The default is 1pe10.3
c@ log
c	The output log file. The default is the terminal.
c--
c  nebk   may89  Original version.
c  nebk   jul89  Add ability to read flagging mask.
c  rjs  17oct89  Fixed a portability problem involving an expression with a
c		 character*(*) variable, used as a format string in a
c		 "write" statement.
c  mchw  3may90	 major rewrite to add region of interest, and logfile
c  mchw  4may90	 added options, and history. The default is the map header.
c  mchw  9may90  converted units for bmaj,bmin to arcsec.
c  wh	15may90	 fix Sun-specific bug involving writeit.
c  mchw  6jun90	 Better checks on number of image axes (naxis).
c  mchw  6jun90	 Added statistics option.
c  mchw 20jun90	 Allowed for transposed images. Special case for naxis=2.
c  mchw 26jun90	 Standardized image analysis subroutines.
c  mchw 09nov90	 Added pbfwhm to map header.
c  mchw 19dec90	 Convert xshift,yshift to angles.
c  mjs  16feb91  Delete internal subroutines which are now in the
c                subroutine library (with permission).
c  mjs  25feb91  Changed references of itoa to itoaf.
c  mchw 03apr91	 Some LogWrit's to enhance stopping power. More info'
c		  in options=stat.
c  rjs  10apr91  Handles higher dummy dimensions better.
c  mchw 03jun91	 Increased length of in and out.
c  mchw 11mar92  Change stat format to e11.4 since Sun screwed up g11.4
c		 omit test in nsize(2) to keep Peter happy.
c  rjs  11mar92  Added 's' flag to BoxSet.
c  rjs  07may92  Eliminate call to BoxMask -- not needed.
c  mchw 28oct92  Added datamin, datamax to header.
c  nebk 25nov92  Add btype to header
c  nebk 18nov93  Allow semi-infinite sized regions in data listing
c  rjs  18oct94  Print contents of mosaic tables.
c  pjt  15mar95  fixed declaration order for f2c (linux)
c  mchw 23may96  Convert cordinates to double; use rangleh and hangleh
c  rjs  02apr97  Tidy.
c
c----------------------------------------------------------------------c
	character version*(*)
	parameter(version='Imlist: version 1.0 02-Apr-97')
	include 'maxdim.h'
	integer maxboxes,maxnax
	parameter(maxboxes=2048,maxnax=3)
	integer naxis,boxes(maxboxes),nsize(maxnax)
	integer blc(maxnax),trc(maxnax)
	integer lin,fldsize,npnt
	character in*64,out*64,format*10
	logical more,dodata,dostat,domos
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya ('in', In, ' ')
	if (in.eq.' ') call bug ('f', 'Image name not specified')
	call GetOpt(dodata,dostat,domos)
	call BoxInput('region',In,boxes,maxboxes)
	call keya ('format', format, '1pe10.3')
	call chform (format, fldsize)
	call keya('log',out,' ')
	call keyfin
c
c  Open the output text file.
c
	call LogOpen(out,'q')
c
c  Open input image and check dimensions.
c
	call xyopen(lIn,In,'old',maxnax,nsize)
	if(nsize(1).gt.maxdim)
     *	  call bug('f','Image too big for buffer')
	call rdhdi(lIn,'naxis',naxis,0)
	naxis = min(naxis,maxnax)
c
c  Determine portion of image to list.
c
	call BoxSet(boxes,maxnax,nsize,'s')
	call BoxInfo(boxes,maxnax,blc,trc)
c
c  Title line.
c
	call LogWrite('Listing for image '//In,more)
c
c  List the required options.
c
	if(dodata) call ListData(lIn,naxis,blc,trc,fldsize,format)
	if(dostat) call ListStat(lIn,naxis,blc,trc)
	if(domos)then
	  call mosLoad(lIn,npnt)
	  call mosPrint
	endif
c
c  All done.
c
	call xyclose(lIn)
	call LogClose
	end
c**********************************************************************c
	subroutine GetOpt(dodata,dostat,domos)
c
	implicit none
	logical dodata,dostat,domos
c
c  Determine which of the options is to be done. Default is dodata.
c
c  Outputs:
c    dodata,dostat	Things to be listed.
c----------------------------------------------------------------------c
	integer NOPTS
	parameter(NOPTS=3)
	character opts(NOPTS)*10
	logical present(NOPTS)
	data opts/'data      ','statistics','mosaic    '/
c
	call options('options',opts,present,NOPTS)
	dodata = present(1)
	dostat = present(2)
	domos  = present(3)
	dodata = dodata.or..not.(dostat.or.domos)
	end
c**********************************************************************c
      subroutine chform (format, size)
c
      implicit none
      character format*(*)
      integer size
c
c     Check format that user has specified and return field size.
c     Checks are not exhaustive.
c
c  Inputs:
c    format	format specification
c  Output:
c    size	field size
c----------------------------------------------------------------------c
      integer ilen, len1, dot, i, j, is
      logical more
c
      ilen = len1(format)
      dot = index(format(1:ilen), '.')
      if (dot.eq.0) call bug ('f', 'No ''.'' in format descriptor.')
c
      if (index('0123456789', format(dot-1:dot-1)).eq.0)
     *  call bug ('f', 
     *   'Could not extract field size from format descriptor')
c
c  Find the begining of the format description.
c
      i = dot - 2
      more = .true.
c
      dowhile(more)
        if(index('0123456789', format(i:i)).eq.0)then
          is = i + 1
          more = .false.
        else
          i = i - 1
          if (i.eq.0) call bug ('f', 'Invalid format descriptor')
        endif
      enddo
c
c  Decode the length.
c
      j = 1
      size = 0
      do i = dot-1, is, -1
        size = size + (ichar(format(i:i))-ichar('0'))*j
        j = 10*j
      enddo
c
      end
c********1*********2*********3*********4*********5*********6*********7*c
	Subroutine ListData(lIn,naxis,blc,trc,fldsize,format)
	implicit none
	integer lIn,naxis,blc(naxis),trc(naxis),fldsize
	character format*(*)
c
c   List Image in specified format into LogFile.
c
c  Inputs:
c    lIn	The handle of the Image.
c    naxis	Number of axes of the Image.
c    blc,trc	Corners of region of interest.
c    format	Format specification.
c    fldsize	Field size.
c----------------------------------------------------------------------c
	character ctype1*16,ctype2*16,ctype*16,bunit*16,line*80,num*3
	integer plane,l1,l2,i
	logical more
c
c  Externals.
c
	integer len1
	character itoaf*8
c
c  Title.
c
	call LogWrite(' ',more)
	call rdhda(lIn,'ctype1',ctype1,'unkown')
	l1 = len1(ctype1)
	call rdhda(lIn,'ctype2',ctype2,'unknown')
	l2 = len1(ctype2)
	call rdhda(lIn,'bunit',bunit,'unknown')
	line = 'The axes of the data are: X is '//ctype1(1:l1)//
     *	   ' and Y is '//ctype2(1:l2)//'.'
	call LogWrite(line,more)
	do i=3,naxis
	  if(i.eq.3)then
	    num = '3rd'
	  else
	    num = itoaf(i)
	    num(2:3) = 'th'
	  endif
	  call rdhda(lIn,'ctype'//itoaf(i),ctype,'unknown')
	  call LogWrite('                        '//
     *			num//' is '//ctype,more)
	enddo
	call LogWrite('The pixel units are '//bunit,more)
        call LogWrite(' ',more)
c
	if(naxis.gt.2)then
	  do plane=blc(3),trc(3)
	    call LogWrite('--------------------------------'//
     *			  '--------------------------------',more)
	    call LogWrite('Plane '//itoaf(plane),more)
	    call LogWrite(' ',more)
	    call xysetpl(lIn,1,plane)
	    call list(lIn,blc,trc,fldsize,format)
	  enddo
c
c  Special case for 2 axes.
c
	else if(naxis.eq.2) then
	  call list(lIn,blc,trc,fldsize,format)
	endif
	end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine list(lIn,blc,trc,fldsize,format)
c
      implicit none
      integer lIn,blc(2),trc(2),fldsize
      character format*(*)
c
c     List requested section of image
c
c  Inputs:
c    lIn	The handle of the image.
c    blc,trc	Corners of region of interest.
c    fldsize	Size of format field.
c    format	Format specification.
c----------------------------------------------------------------------c
      include 'maxdim.h'
      real data(maxdim)
      integer i, j, len1, ilen, flen, i1
      character line*1000, form1*30, form2*30, form2f*30
      logical flags(maxdim),more
c
      if((trc(1)-blc(1)+1)*(fldsize+1)+5.gt.len(line))then 
	call bug('f','Region too big to list with this format')
      endif
      call makf1 (fldsize,blc(1),trc(1),form1)
      write (line, form1(1:len1(form1))) (i, i = blc(1),trc(1))
      call LogWrite(line(1:len1(line)),more)
      call LogWrite(' ',more)
c
      call makf2 (format, fldsize, form2, form2f)
      ilen = len1(form2)
      flen = len1(form2f)
      do j = trc(2),blc(2),-1
	if(more)then
	 call xyread (lin, j, data)
         call xyflgrd (lin, j, flags)
         i1 = 1
         do i = blc(1),trc(1)
           if (flags(i)) then
             write(line(i1:i1+fldsize),form2(1:ilen)) data(i)
           else
             write(line(i1:i1+fldsize),form2f(1:flen)) 
           end if
           i1 = i1 + fldsize + 1
         end do
         write(line(i1:i1+4),'(i5)') j
	 i1 = i1 + 4
         call LogWrite(line(1:i1),more)
	endif
      end do
      call LogWrite(' ',more)
      write (line, form1(1:len1(form1))) (i, i = blc(1),trc(1))
      call LogWrite(line(1:len1(line)),more)
      call LogWrite(' ',more)
      end
c************************************************************************
      subroutine makf1(fldsize, is, ie, form)
c
      implicit none
      integer fldsize, is, ie
      character form*(*)
c
c     Write first format character.
c-----------------------------------------------------------------------
      character itoaf*10, str1*10, str2*10, str3*10
      integer siz1, siz2, l1, l2, l3, len1
c
      siz1 = (fldsize - 4) / 2
      siz2 = fldsize - siz1 - 4
      str1 = itoaf(siz1)
      str2 = itoaf(siz2)
      str3 = itoaf(ie-is+1)
      l1 = len1(str1)
      l2 = len1(str2)
      l3 = len1(str3)
c
      write (form, 10) str3(1:l3), str1(1:l1), str2(1:l2)
10    format ('(',a,'(',a,'x,i4,',a,'x,1x))')
c
      end
c************************************************************************
      subroutine makf2(format,fldsize,form,formf)
c
      implicit none
      integer fldsize
      character form*(*), formf*(*), format*(*)
c
c     Write second format characters
c--------------------------------------------------------------------------
      integer l1,i1
c
c  Externals.
c
      integer len1
c
      l1 = len1(format)
      form = '('//format(1:l1)//',1x)'
c
      formf = '('''
      i1 = fldsize/2
      formf(i1+2:i1+4) = '...'
      formf(fldsize+3:) = ''',1x)'
c
      end
c******************************************************************c
	Subroutine ListStat(lIn,naxis,blc,trc)
	implicit none
	integer lIn,naxis,blc(naxis),trc(naxis)
c
c   List Image Statistics and write in standard format into LogFile.
c
c  Inputs:
c    lIn	The handle of the Image.
c    naxis	Number of axes of the Image.
c    blc,trc	Corners of region of interest.
c-------------------------------------------------------------------c
	double precision value
	integer axis,plane,length
	character line*80,header*80,label*13,units*13,ctype*9
	real sum,ave,rms,pmax,pmin,cbof
c
c  Write title lines.
c
	call LogWrit(' ')
	call LogWrit('Image Statistics')
	call Title(lIn,naxis,blc,trc,cbof)
	if(cbof.ne.1)then
	  write(line,'(a,e11.4,a,a)')
     *	  '  Effective beam area: ',cbof, ' pixels',
     *    '  (Used to normalize total flux)'
	  length = 23 + 11 + 7 + 32
	  call LogWrit(line(1:length))
	endif
c
c  List statistics for each plane in each axis in selected region.
c
      if(naxis.ge.3) then
	axis = 3
	do while(axis.le.naxis)
	  call AxisType(lIn,axis,plane,ctype,label,value,units)
	  write(line,'(a,i5,5x,a)')
     *	  'Axis: ',axis,ctype
	  length=6+5+5+10
	  call LogWrit(' ')
	  call LogWrit(line(1:length))
	  write(header,'(a,a,a,a,a,a,a)') ' plane ',label,
     *    ' Total Flux ','  Maximum   ','  Minimum   ','  Average   ',
     *	  '    rms     '
	  call LogWrit(header(1:79))
c
c  Accumulate statistics for each hyperplane.
c
	  plane = blc(axis)
	  do while(plane.le.trc(axis))
	    call xysetpl(lIn,1,plane)
	    call AxisType(lIn,axis,plane,ctype,label,value,units)
	    call stat(lin,naxis,blc,trc,sum,ave,rms,pmax,pmin)
	    write(line,'(i5,x,a,1p5e12.4)')
     *		plane,units,sum/cbof,pmax,pmin,ave,rms
	    call LogWrit(line(1:79))
	    plane = plane + 1
	  enddo
	  axis = axis + 1
	enddo
      else if(naxis.eq.2) then
	write(header,'(a,a,a,a,a)') ' Total Flux  ','   Maximum   ',
     *		    '   Minimum   ','   Average   ','     rms     '
	call LogWrit(header(1:65))
	call stat(lin,naxis,blc,trc,sum,ave,rms,pmax,pmin)
	write(line,'(5(g12.5,x))') sum/cbof,pmax,pmin,ave,rms
	call LogWrit(line(1:60))
      endif
      end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine stat(lin,naxis,blc,trc,sum,ave,rms,pmax,pmin)
c
	implicit none
	integer lIn,naxis,blc(naxis),trc(naxis)
	real sum,ave,rms,pmax,pmin
c
c   List Image Statistics and write in standard format into LogFile.
c
c  Inputs:
c    lIn	The handle of the Image.
c    naxis	Number of axes of the Image.
c    blc,trc	Corners of region of interest.
c  Output:
c    sum	Sum of unflagged pixels within region.
c    ave,rms	Average and rms of unflagged pixels within region.
c    pmax,pmin	Maximum and minimum of unflagged pixels.
c-----------------------------------------------------------------
	include 'maxdim.h'
	real data(maxdim)
	logical flags(maxdim)
	integer i,j,num
	real sumsq
c
c  Initialize statistics for each plane.
c
	    sum = 0.
	    sumsq = 0.
	    num = 0
	    ave = 0.
	    rms = 0.
	    pmax = -1.e12
	    pmin = 1.e12
c
c  Accumulate statistics for unflagged data.
c
	    do j = trc(2),blc(2),-1
	      call xyread (lIn,j,data)
	      call xyflgrd (lIn,j,flags)
	      do i = blc(1),trc(1)
		if(flags(i)) then
		  sum = sum + data(i)
		  sumsq = sumsq + data(i)*data(i)
		  num = num + 1
		  pmax=max(pmax,data(i))
		  pmin=min(pmin,data(i))
		endif
	      end do
	    end do
c
c  Calculate average and rms.
c
	    if(num.ne.0)then
	      ave = sum/num
	      rms = sqrt(sumsq/num - ave*ave)
	    endif
	    end
