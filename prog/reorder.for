c************************************************************************
	program reorder
	implicit none

c= REORDER - Interchange the axes of a cube
c& rjs
c: map manipulation
c+
c	REORDER is a MIRIAD task to reorder and reverse axes within an image.
c@ in
c	The input image. No default.
c@ out
c	The output image. No default.
c@ mode
c	The new axis ordering in terms of the old axis ordering: i.e.
c	'312' make input axis 3 the first output axis, input axis 1
c	the second output axis, etc. Use a - to reverse the pixel
c	order on an axis. Do not include dummy axes in mode.
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
c    rjs  25jan93 New trnio routines. Handle masks (appease nebk? Never?).
c    rjs  03feb93 Fixed a bug dealing with 3rd axis, which effectively
c		  broke it.
c    rjs  02mar93 Break out "Inc" routines.
c    rjs  06sep93 Change ownership. Fix bug which inverted (!) the masking
c		  file.
c    rjs  07feb96 Handle 4th, etc, dimension.
c    rjs  02jul97 cellscal change.
c    rjs  23jul97 Add pbtype.
c    rjs  14jun00 Copy across llrot keyword.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	character version*(*)
	parameter(version='Reorder: version 1.0 14-Jun-00')
	integer naxis,lu,pnt,n
	integer nIn(MAXNAX),nOut(MAXNAX),sgn(MAXNAX),idx(MAXNAX)
	integer lmode,i,size
	integer lIn,lOut
	character in*64,out*64,mode*16
c
	real ref(MAXBUF)
	common ref
c
c  Externals.
c
	integer len1
	logical hdprsnt
c
        call output(version)
c
c  Get the input parameters.
c
	call keyini
	call keya('in',in,' ')
	if(in.eq.' ')call bug('f','Input file name is missing')
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','Output file name is missing')
	call keya('mode',mode,' ')
	lmode = len1(mode)
	if(lmode.eq.0) call bug('f','The mode must be given')
	call keyfin
c
c  Reduce the mode parameter into something more managable.
c
	do i=1,MAXNAX
	  idx(i) = i
	  sgn(i) = 1
	enddo
c
	n = 0
	do i=1,lmode
	  if(mode(i:i).eq.'-')then
	    if(n.gt.MAXNAX) call bug('f','Too many dimensions for me')
	    sgn(n+1) = -1
	  else if(mode(i:i).ge.'1'.and.mode(i:i).le.'9')then
	    n = n + 1
	    idx(n) = ichar(mode(i:i)) - ichar('0')
	    if(idx(n).gt.MAXNAX)
     *			call bug('f','Too many dimensions for me')
	  else
	    call bug('f','The mode parameter contains rubbish')
	  endif
	enddo
c
c  Open the input file, and check it out.
c
	call xyopen(lIn,in,'old',MAXNAX,nIn)
	call rdhdi(lIn,'naxis',naxis,0)
	naxis = min(naxis,MAXNAX)
c
c  Initialise the transpose routines and output.
c
	call trnini(lu,naxis,nIn,mode)
c
	do i=1,naxis
	  nOut(i) = nIn(idx(i))
	enddo
	call xyopen(lOut,out,'new',naxis,nOut)
	call Header(lIn,lOut,nIn,sgn,idx,naxis,version)
c
	size = max(nIn(1)*nIn(2),nOut(1)*nOut(2))
	call memalloc(pnt,size,'r')
c
c  Do the real work.
c
	call PixRead(lu,lIn,ref(pnt),nIn(1),nIn(2),nIn,n)
	call PixWrit(lu,lOut,ref(pnt),nOut(1),nOut(2),nOut,n)
	if(hdprsnt(lIn,'mask'))then
	  call MaskRead(lu,lIn,ref(pnt),nIn(1),nIn(2),nIn,n)
	  call MaskWrit(lu,lOut,ref(pnt),nOut(1),nOut(2),nOut,n)
	endif
c
c  Tidy up afterwards.
c
	call memfree(pnt,size,'r')
	call xyclose(lOut)
	call trnfin(lu)
	call xyclose(lIn)
c
	end
c************************************************************************
	subroutine PixRead(lu,lIn,Data,n1,n2,size,n)
c
	implicit none 
	integer lu,lIn,n1,n2,n,size(n)
	real Data(n1,n2)
c
c  Read the data, and write it to the trnio routines.
c
c------------------------------------------------------------------------
	include 'maxnax.h'
	integer dims(MAXNAX),j
c
	logical Inc3More
c
	call IncIni(n,size,dims)
	dowhile(Inc3More(n,size,dims))
	  if(n.ge.3)call xysetpl(lIn,n-2,dims(3))
	  do j=1,n2
	    call xyread(lIn,j,Data(1,j))
	  enddo
	  call trnwrite(lu,Data)
	enddo
c
	end
c************************************************************************
	subroutine PixWrit(lu,lOut,Data,n1,n2,size,n)
c
	implicit none
	integer lu,lOut,n1,n2,n,size(n)
	real Data(n1,n2)
c
c  Read the data, and write it to the trnio routines.
c
c------------------------------------------------------------------------
	include 'maxnax.h'
	integer dims(MAXNAX),j
c
	logical Inc3More
c
	call IncIni(n,size,dims)
	dowhile(Inc3More(n,size,dims))
	  call trnread(lu,Data)
	  if(n.ge.3)call xysetpl(lOut,n-2,dims(3))
	  do j=1,n2
	    call xywrite(lOut,j,Data(1,j))
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine MaskRead(lu,lIn,Data,n1,n2,size,n)
c
	implicit none 
	integer lu,lIn,n1,n2,n,size(n)
	real Data(n1,n2)
c
c  Read the data, and write it to the trnio routines.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	integer dims(MAXNAX),i,j
	logical flags(MAXDIM)
c
	logical Inc3More
c
	call IncIni(n,size,dims)
	dowhile(Inc3More(n,size,dims))
	  if(n.ge.3)call xysetpl(lIn,n-2,dims(3))
	  do j=1,n2
	    call xyflgrd(lIn,j,flags)
	    do i=1,n1
	      Data(i,j) = 0
	      if(flags(i)) Data(i,j) = 1
	    enddo
	  enddo
	  call trnwrite(lu,Data)
	enddo
c
	end
c************************************************************************
	subroutine MaskWrit(lu,lOut,Data,n1,n2,size,n)
c
	implicit none
	integer lu,lOut,n1,n2,n,size(n)
	real Data(n1,n2)
c
c  Read the data, and write it to the trnio routines.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	integer dims(MAXNAX),i,j
	logical flags(MAXDIM)
c
	logical Inc3More
c
	call IncIni(n,size,dims)
	dowhile(Inc3More(n,size,dims))
	  call trnread(lu,Data)
	  if(n.ge.3)call xysetpl(lOut,n-2,dims(3))
	  do j=1,n2
	    do i=1,n1
	      flags(i) = Data(i,j).gt.0.1
	    enddo
	    call xyflgwr(lOut,j,flags)
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine Header(lIn,lOut,nIn,sgn,idx,naxis,version)
c
	implicit none
	integer lIn,lOut
	character version*(*)
	integer naxis,nIn(naxis),sgn(naxis),idx(naxis)
c
c  Calculate the header parameters for the output file, and write out
c  the history file.
c
c  Inputs:
c    lIn	Handle of the input file.
c    lOut	Handle of the output file.
c    naxis	Nominal number of dimensions in input and output (this
c		the the number of true plus the number of dummy).
c    nIn	Size of the input image.
c    idx	Mapping from input to output axes.
c    version	Version of task.
c
c------------------------------------------------------------------------
	integer i
	double precision temp
	logical same
	character line*80,numi*1,numo*1
c
c  Header keywords.
c
	integer nkeys
	parameter(nkeys=24)
	character keyw(nkeys)*8
c
c  Externals.
c
	character itoaf*8
c
	data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *    'obstime ','epoch   ','history ','instrume','llrot   ',
     *	  'ltype   ','lstart  ','lwidth  ','lstep   ','pbtype  ',
     *	  'niters  ','object  ','observer','obsra   ','obsdec  ',
     *	  'pbfwhm  ','restfreq','telescop','vobs    ','cellscal'/
c
c  Copy header keywords.
c
	do i=1,nkeys
	  call hdcopy(lIn,lOut,keyw(i))
	enddo
c
c  Copy the coordinate information.
c
	do i=1,naxis
	  numo = itoaf(i)
	  same = idx(i).eq.i.and.sgn(i).eq.1
	  if(same)then
	    call hdcopy(lIn,lOut,'ctype'//numo)
	    call hdcopy(lIn,lOut,'cdelt'//numo)
	    call hdcopy(lIn,lOut,'crval'//numo)
	    call hdcopy(lIn,lOut,'crpix'//numo)
	  else
	    numi = char(idx(i)+ichar('0'))
	    call rdhdd(lIn,'cdelt'//numi,temp,1.0d0)
	    call wrhdd(lOut,'cdelt'//numo,sgn(i)*temp)
	    call rdhdd(lIn,'crval'//numi,temp,0.0d0)
	    call wrhdd(lOut,'crval'//numo,temp)
	    call rdhdd(lIn,'crpix'//numi,temp,dble(nIn(idx(i))/2+1))
	    if(sgn(i).eq.-1) temp = nIn(idx(i)) - temp + 1
	    call wrhdd(lOut,'crpix'//numo,temp)
	    call rdhda(lIn,'ctype'//numi,line,' ')
	    if(line.ne.' ') call wrhda(lOut,'ctype'//numo,line)
	  endif
	enddo
c
	call hisopen(lOut,'append')
	line = 'REORDER: Miriad '//version
	call hiswrite(lOut,line)
	call hisinput(lOut,'REORDER')
	call hisclose(lOut)
c
	end
