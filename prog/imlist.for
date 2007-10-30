c**********************************************************************c
      program ImList
      implicit none
c
c= IMLIST - List items and pixel values from an image
c& mchw
c: image analysis
c+
c       IMLIST lists a Miriad image. It will list the header, statistics,
c       selected regions of the data, the mosaic table (if present) and the
c	history.
c@ in
c	Input image name. No default.
c@ options
c	Severval options can be given (separated by commas), and can be
c	abbreviated to uniqueness. Possible options are:
c	  header     Give a summary of all the items in the image. This is
c	             the default if no other options are given.
c	  data       List some data.
c	  mosaic     List the mosaic table of an image (if present).
c	  history    List the history item.
c	  statistics This lists the total flux, min and max values and
c	             rms for each plane.
c@ region
c	Region of image to be listed. E.g.
c	  % imlist  options=data region=relpix,box(-4,-4,5,5)(1,2)
c	lists the center 10 x 10 pixels of image planes 1 and 2.
c	Unmasked pixels within the bounding box are used.
c@ format
c	Format for output, e.g., 1pe11.4, f5.2, 1pg12.5, default is 1pe10.3
c	The size of region possible to list depends on the format. 10 columns
c	of f5.1 fills an 80 character line.
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
c
c  Bugs:
c    Data format still needs work to prevent format overflow.
c    Doesn't handle pixel blanking outside region of interest.
c----------------------------------------------------------------------c
	character version*(*)
	parameter(version='version 18-Oct-94')
	include 'maxdim.h'
	integer maxboxes,maxnax
	parameter(maxboxes=2048,maxnax=3)
	integer naxis,boxes(maxboxes),nsize(maxnax)
	integer blc(maxnax),trc(maxnax)
	integer lin,fldsize,length,lenin,npnt
	character in*64,out*64,format*10,line*80
	logical more,dohead,dodata,dohist,dostat,domos,eof
c
c  Externals.
c
	integer len1
c
c  Get the input parameters.
c
	call output('ImList: '//version)
	call keyini
	call keya ('in', In, ' ')
	if (in.eq.' ') call bug ('f', 'Image name not specified')
	call GetOpt(dohead,dodata,dohist,dostat,domos)
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
	lenin = len1(In)
	line = ' ***** Listing for Image = '//In(1:lenin)//
     *		' *****'
	length = 27 + lenin + 6
	call LogWrite(line(1:length),more)
	call LogWrite(' ',more)
	call LogWrite('------------------------------'//
     *		      '------------------------------',more)
c
c  List the required options.
c
	if(dohead) call ListHead(lIn)
	if(dodata) call ListData(lIn,naxis,blc,trc,fldsize,format)
	if(dostat) call ListStat(lIn,naxis,blc,trc)
	if(domos)then
	  call mosLoad(lIn,npnt)
	  call mosPrint
	endif
c
c  Copy across the history file, if required.
c
	if(dohist) then
	  call LogWrite(' ',more)
	  call LogWrite(' ***** History of Image = '//In,more)
	  call LogWrite(' ',more)
	  call hisopen(lIn,'read')
	  call hisread(lIn,line,eof)
	  dowhile(.not.eof.and.more)
	    call LogWrite(line,more)
	    call hisread(lIn,line,eof)
	  enddo
	  call hisclose(lIn)
	end if
c
c  All done.
c
	call xyclose(lIn)
	call LogClose
      end
c**********************************************************************c
	subroutine GetOpt(dohead,dodata,dohist,dostat,domos)
c
	implicit none
	logical dohead,dodata,dohist,dostat,domos
c
c  Determine which of the options is to be done. Default is dohead.
c
c  Outputs:
c    dohead,dodata,dohist,dostat	Things to be listed.
c----------------------------------------------------------------------c
	integer NOPTS
	parameter(NOPTS=5)
	character opts(NOPTS)*10
	logical present(NOPTS)
	data opts/'header    ','data      ','history   ',
     *		  'statistics','mosaic    '/
c
	call options('options',opts,present,NOPTS)
	dohead = present(1)
	dodata = present(2)
	dohist = present(3)
	dostat = present(4)
	domos  = present(5)
	dohead = dohead.or..not.(dodata.or.dohist.or.dostat.or.domos)
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
      integer ilen, len1, dot, i, j, is, num
      logical more
c
      ilen = len1(format)
      dot = index(format(1:ilen), '.')
      if (dot.eq.0) call bug ('f', 'No ''.'' in format descriptor.')
c
      if (index('0123456789', format(dot-1:dot-1)).eq.0) then
         call bug ('f', 
     *   'Couldn''t extract field size from format descriptor')
      else
         i = dot - 2
         more = .true.
c
         do while (more)
            if (index('0123456789', format(i:i)).eq.0) then
               is = i + 1
               more = .false.
            else
               i = i - 1
               if (i.eq.0) call bug ('f', 'Invalid format descriptor')
            end if
         end do
      end if
c
      j = 0
      size = 0
      do i = dot-1, is, -1
         read (format(i:i), '(i1)') num
         size = size + num*10**j
         j = j + 1
      end do
c
      end
c******************************************************************c
	Subroutine ListHead(tno)
	implicit none
	integer tno
c
c  Read Image header variables.
c  convert units and write in standard format into LogFile.
c
c  Input:
c    tno	The handle of the Image.
c-------------------------------------------------------------------c
	double precision pi,ckms,rtos,rtoh,rtod
	parameter(pi=3.141592654,ckms=299793.)
	parameter(rtos=3600.d0*180.d0/pi,rtoh=12.d0/pi,rtod=180.d0/pi)
	character descr*10,type*10,line*80,RA*1,DEC*1,axis*1
	character xtype*12
	integer i,n
	real data
	logical more

c  Header keywords.
c
	integer nkeys
	parameter(nkeys=49)
	character keyw(nkeys)*8
	data keyw/   'object  ','telescop','observer','date-obs',
     *	  'restfreq','ltype   ','lstart  ','lwidth  ','lstep   ',
     *	  'naxis   ','naxis1  ','naxis2  ','naxis3  ','naxis4  ',
     *	  'crpix1  ','crpix2  ','crpix3  ','crpix4  ','crpix5  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *	  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *	  'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','cdelt5  ',
     *	  'epoch   ','obsra   ','obsdec  ','vobs    ',
     *	  'bunit   ','niters  ','bmaj    ','bmin    ','bpa     ',
     *	  'xshift  ','yshift  ','pbfwhm  ','datamin ','datamax ',
     *    'btype   '/
c
c  Externals.
c
	character angles*13
	integer len1
c
c  Probe for each item and convert to user units.
c
	call LogWrite(' ',more)
	do i=1,nkeys
	  call hdprobe(tno,keyw(i),descr,xtype,n)
	  type = xtype(1:10)
	  if(n.ne.0) then
	    axis = keyw(i)(6:6)
	    if (keyw(i)(1:5).eq.'ctype'
     *			.and.descr(1:8).eq.'RA---SIN') then
		 RA = axis
	    else if(keyw(i)(1:5).eq.'ctype'
     *			.and.descr(1:8).eq.'DEC--SIN') then
		 DEC = axis
	    endif
	    if (keyw(i)(1:5).eq.'crval'.and.axis.eq.RA) then
	      call rdhdr(tno,'crval'//axis,data,0.)
	      call writeit(keyw(i)//': '//angles(dble(data)*rtoh),23)
	    else if (keyw(i)(1:5).eq.'crval'.and.axis.eq.DEC) then
	      call rdhdr(tno,'crval'//axis,data,0.)
	      call writeit(keyw(i)//': '//angles(dble(data)*rtod),23)
	    else if (keyw(i)(1:5).eq.'cdelt'
     *				.and.(axis.eq.RA.or.axis.eq.DEC)) then
	      call rdhdr(tno,'cdelt'//axis,data,0.)
	      call writeit(keyw(i)//': '//angles(dble(data)*rtod),23)
	    else if (keyw(i).eq.'obsra') then
	      call rdhdr(tno,'obsra',data,0.)
	      call writeit(keyw(i)//': '//angles(dble(data)*rtoh),23)
	    else if (keyw(i).eq.'obsdec') then
	      call rdhdr(tno,'obsdec',data,0.)
	      call writeit(keyw(i)//': '//angles(dble(data)*rtod),23)
	    else if (keyw(i).eq.'bmaj'.or.keyw(i).eq.'bmin'
     *		.or.keyw(i).eq.'xshift'.or.keyw(i).eq.'yshift') then
	      call rdhdr(tno,keyw(i),data,0.)
	      call writeit(keyw(i)//': '//angles(dble(data)*rtod),23)
	    else
	      call writeit(keyw(i)//': '//
     *				descr(1:len1(descr)),10+len1(descr))
	    endif
	  endif
	enddo
c
c  Flush buffer if needed.
c
	line = ' '
	call writeit(line,80)
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
	double precision value
	character line*80
	character*9 ctype,ctype1,ctype2,bunit
	character*13 label,units
	integer axis,plane,length
	logical more
c
c  Title.
c
	call rdhda(lIn,'ctype1',ctype1,' ')
	call rdhda(lIn,'ctype2',ctype2,' ')
	call rdhda(lIn,'bunit',bunit,' ')
	write(line,'(a,a,a,a,a,a)')
     *     'Axes of plots: x= ',ctype1,'  y= ',ctype2,'  bunit= ',bunit
	length = 18 + len(ctype1) + 5 + len(ctype2) + 9 + len(bunit)
	call LogWrite(' ',more)
	call LogWrite(line(1:length),more)
	
c
c  List each plane if 3 or more axes.
c
	if(naxis.ge.3) then
	  axis = 3
	  do while(axis.le.naxis.and.more)
	    plane = blc(axis)
	    do while(plane.le.trc(axis).and.more)
	      call AxisType(lIn,axis,plane,ctype,label,value,units)
	      call LogWrite(' ',more)
	      write(line,'(a,i4,4x,a,a,i4,2x,a,a)')
     *	      '  Axis: ',axis,ctype,'  Plane: ',plane,label,units
	      length = 8+4+4+9+9+4+2+13+2+13
	      call LogWrite(line(1:length),more)
	      call xysetpl(lIn,1,plane)
	      call list (lIn,naxis,blc,trc,fldsize,format)
	      plane = plane + 1
	    enddo
	    axis = axis + 1
	  enddo
c
c  Special case for 2 axes.
c
	else if(naxis.eq.2) then
	  call list (lIn,naxis,blc,trc,fldsize,format)
	endif
	end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine list (lIn,naxis,blc,trc,fldsize,format)
c
      implicit none
      integer lIn,naxis,blc(naxis),trc(naxis),fldsize
      character format*10
c
c     List requested section of image
c
c  Inputs:
c    lIn	The handle of the image.
c    naxis	Number of axes of the image.
c    blc,trc	Corners of region of interest.
c    fldsize	Size of format field.
c    format	Format specification.
c----------------------------------------------------------------------c
      include 'maxdim.h'
      real data(maxdim)
      integer i, j, len1, ilen, flen, inc2, i1
      character line*1000, form1*30, form2*30, form2f*30
      logical flags(maxdim),more
c
      if((trc(1)-blc(1))*fldsize.gt.1000)then 
	call bug('f','Region too big to list with this format')
      endif
      call makf1 (fldsize,blc(1),trc(1),form1)
      call LogWrit(' ')
      write (line, form1(1:len1(form1))) (i, i = blc(1),trc(1))
      call LogWrite(line(1:len1(line)),more)
      call LogWrite(' ',more)
c
      call makf2 (format, form2, form2f, inc2)
      ilen = len1(form2)
      flen = len1(form2f)
      do j = trc(2),blc(2),-1
	if(more)then
	 call xyread (lin, j, data)
         call xyflgrd (lin, j, flags)
         i1 = 1
         do i = blc(1),trc(1)
           if (flags(i)) then
             write(line(i1:), form2(1:ilen)) data(i)
           else
             write(line(i1:), form2f(1:flen)) 
           end if
           i1 = i1 + inc2
         end do
         write(line(i1:), '(i6)') j
         call LogWrit(line(1:len1(line)))
	endif
      end do
      call LogWrit(' ')
      write (line, form1(1:len1(form1))) (i, i = blc(1),trc(1))
      call LogWrit(line(1:len1(line)))
      call LogWrit(' ')
      end
c************************************************************************
      subroutine makf1 (fldsize, is, ie, form)
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
10    format ('(', a, '(', a, 'x, i4, ', a, 'x, 2x))')
c
      end
c************************************************************************
      subroutine makf2 (format, form, formf, inc)
c
      implicit none
      integer inc
      character form*(*), formf*(*), format*(*)
c
c     Write second format characters
c--------------------------------------------------------------------------
      character temp*50,temp2*50
      real x
      integer l1, l2, len1, i1
c
      l1 = len1(format)
      x = 1.12345e-10
      temp2 = '('//format(1:l1)//')'
      write (temp, temp2) x
      l2 = len1(temp)
      inc = l2 + 2
c
      form = '('//format(1:l1)//', 2x)'
c
      formf = ' '
      formf(1:2) = '('''
      i1 = l2/2
      formf(i1+2:i1+4) = '...'
      formf(l2+3:l2+7) = ''',2x)'
c
      end
c************************************************************************
	subroutine writeit(partial,plen)
	implicit none

	integer plen
	character partial*80
c
c  Stuff pieces of line into buffer and print them.
c
c  Input:
c    partial	A piece of a line.
c    plen	Length of partial.
c------------------------------------------------------------------------
	character line*80
	integer i,j,jend
	logical first
	data first /.true./
c
	save line,i,j,first
c
	if (first) then
	  first=.false.
	  i=1
	  j=1
	  line=' '
	end if
c
	if(plen+i.gt.len(line)) then
	  call LogWrit(line)
	  line=' '
	  j=1
	  dowhile (plen-j .gt.len(line))
	    jend=j+79
	    dowhile (partial(jend:jend).ne.' ')
	      jend=jend-1
	    enddo
	    call LogWrit(partial(j:jend))
	    j=jend+1
	  enddo
	  i=1
	end if
	line(i:i+plen-j) = partial(j:plen)
	i = i+plen - 1
	i = (i-1)/25*25 + 26
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
	double precision pi,value
	parameter(pi=3.141592654)
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
c********1*********2*********3*********4*********5*********6*********7*c
