c************************************************************************
	program velcolor
	implicit none
c
c= VELCOLOR - Make a red-green-blue image to display velocity as color.
c& mchw
c: image analysis
c+
c	VELCOLOR makes a 3-plane Miriad image to display the 3rd axis of
c	a 3-d Miriad image as color. The third axis can be represented by
c	color by superposing the 3 planes as red, green and blue images.
c	The most obvious use is to display the velocity axis as color.
c	The algorithm weights the channels so that the apparent intensity
c	of the superposed red-green-blue image planes is independent of
c	color. (For more details see Heiles & Jenkins, 1976, A&A 46,33)
c@ in
c	The input image. No default.
c@ region
c	The region of the input image to be used. The 3rd axis region
c	determines the range from blue to red. See documentation on region
c	for help of how to specify this. Only the bounding box is supported.
c@ pivot
c	Center channel of input image for output green image.
c	Default is 0.4*trc+0.6*blc
c@ out
c	The output red-green-blue image. No default.
c@ clip
c	Two values. Exclude pixels with values in the range clip(1) to clip(2).
c	If only one value is given, then exclude -abs(clip) to abs(clip).
c--
c  History:
c    06aug92 mchw  Adapted from RALINT task.
c    16sep92 mchw  Added pivot and cleaned up code. Stubs for other axes.
c    27feb93 mjs   use tmpdim.h instead of maxdim.h
c------------------------------------------------------------------------
	include 'tmpdim.h'
 	character version*(*)
	parameter(version='Version 1.0 16-Sep-92')
	integer maxnax,maxboxes,maxruns,naxis,axis
	parameter(maxnax=3,maxboxes=2048)
	parameter(maxruns=3*maxdim)
	integer boxes(maxboxes)
	integer i,j,lin,lout,nsize(maxnax),blc(maxnax),trc(maxnax)
	integer size(maxnax)
	real blo,bhi,clip(2),pivot
	character in*64,out*64,line*72
c
c  External
c
	logical keyprsnt
c
c Get inputs.
c
	call output('VELCOLOR: '//version)
	call keyini
	call keya('in',in,' ')
	call BoxInput('region',in,boxes,maxboxes)
	call keya('out',out,' ')
	call keyr('pivot',pivot,0.)
	call keyr('clip',clip(1),0.)
	if(keyprsnt('clip'))then
	  call keyr('clip',clip(2),0.)
	else
	  clip(2) = abs(clip(1))
	  clip(1) = -clip(2)
	endif
	call keyfin
	axis = 3
c
c Check inputs.
c
	if(in .eq. ' ') call bug('f','No input specified. (in=)')
	if(out .eq. ' ') call bug('f','No output specified. (out=)')
	if(clip(2).lt.clip(1)) call bug('f','clip range out of order')
	call xyopen(lin,in,'old',maxnax,nsize)
	call rdhdi(lin,'naxis',naxis,0)
	naxis = min(naxis,maxnax)
	if(nsize(1).gt.maxdim)call bug('f','Input file too big for me')
c
c  Determine the min and max image values.
c
	call ImMinMax(lIn,naxis,nsize,blo,bhi)
	if(blo.eq.bhi)then
	  call xyclose(lIn)
	  write(line,'(''All pixels are '',1pg10.3)')blo
	  call output(line)
	  stop
	endif
c
c  Set up the region of interest. Warn if not rectangle.
c
	call BoxSet(boxes,maxnax,nsize,'s')
	call BoxInfo(boxes,maxnax,blc,trc)
c
c  Open output image and fill in its header.
c
	j = 0
	do i = 1,naxis
	  if(blc(i).lt.1) blc(i) = 1
          if(trc(i).gt.nsize(i)) trc(i) = nsize(i)
	  if(i.ne.axis) then
	    j = j + 1
	    size(j) = trc(i) - blc(i) + 1
	  endif
	enddo
	size(3) = 3
	call xyopen(lOut,out,'new',naxis,size)
	call header(lIn,lOut,naxis,blc,trc,axis,pivot)
c
c  Calculate the red-green-blue image planes.
c
	call velcolor3(lIn,lOut,naxis,blc,trc,clip,pivot)
c
c  Update history and close files.
c
	call Hisopen(lOut,'append')
        call HisWrite(lOut,'VELCOLOR: '//version)
	call HisInput(lOut,'VELCOLOR')
	call HisClose(lOut)
	call xyclose(lIn)
	call xyclose(lOut)
	end
c************************************************************************
	subroutine header(lIn,lOut,naxis,blc,trc,axis,pivot)
	implicit none
	integer lin,lOut,naxis,blc(naxis),trc(naxis),axis
	real pivot
c
c  Copy keywords to output file.
c
c  Inputs:
c    lIn,lOut	Handle of input and output files.
c    naxis	The number of input axes.
c    blc,trc	The corners of the input image.
c    pivot	Center channel for green image.
c    axis	The axis for which the color is calculated.
c------------------------------------------------------------------------
	integer nkeys, nckeys,i,j,k
	parameter (nkeys=23, nckeys=4)
	character keyw(nkeys)*9, ckeyw(nckeys)*5, itoaf*1, cin*1, cout*1
	character atemp*9,ctype*10
	real rtemp,cdelt,crval,crpix
	logical hdprsnt
c
c  Be careful that nkeys and nckeys match the number of keywords.
c
	data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *    'date-obs','epoch   ','history ','instrume',
     *	  'ltype   ','lstart  ','lwidth  ','lstep   ','niters  ',
     *	  'object  ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *    'restfreq','telescop','vobs    ','xshift  ','yshift  '/
c
c  Keyword values which must be changed as they are passed from in to out.
c
	data ckeyw/'ctype','cdelt','crval','crota'/
c
c  Copy across unchanged header keywords.
c
	do i = 1,nkeys
	  call hdcopy(lin,lout,keyw(i))
	enddo
c
c  Handle the keywords which must be moved to another axis.
c
	j = 0
	do i = 1,naxis
	  if(i.ne.axis) then
	    j = j + 1
	    cin = itoaf(i)
	    cout = itoaf(j)
	    atemp = ckeyw(1)//cin
	    if(hdprsnt(lin,atemp)) then
	      call rdhda(lin,ckeyw(1)//cin,atemp,' ')
	      call wrhda(lout,ckeyw(1)//cout,atemp)
	    endif
	    do k = 2,nckeys
	      atemp = ckeyw(k)//cin
	      if(hdprsnt(lin,atemp)) then
	        call rdhdr(lin,ckeyw(k)//cin,rtemp,0.0)
	        call wrhdr(lout,ckeyw(k)//cout,rtemp)
	      endif
	    enddo
c
c  Special cases: the crpixes will change if the user uses a subcube.
c
	    if(hdprsnt(lin,'crpix'//cin)) then
	      call rdhdr(lin,'crpix'//cin,rtemp,0.)
	      rtemp = rtemp - real(blc(i)) + 1
	      call wrhdr(lout,'crpix'//cout,rtemp)
	    endif
	  endif
	enddo
c
c  Write out third axis.
c
        cin = itoaf(axis)
	call rdhda(lin,'ctype'//cin, ctype, ' ')
	call rdhdr(lin,'crpix'//cin, crpix, 1.0)
	call rdhdr(lin,'crval'//cin, crval, 0.0)
	call rdhdr(lin,'cdelt'//cin, cdelt, 1.0)

	if(pivot.eq.0.)pivot=0.4*trc(axis)+0.6*blc(axis)
	call wrhda(lout,'ctype3', ctype)
	call wrhdr(lout,'crpix3', 2.)
	call wrhdr(lout,'crval3', crval+cdelt*(pivot-crpix))
	call wrhdr(lout,'cdelt3', cdelt*(pivot-blc(axis)))

	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine velcolor3(lIn,lOut,naxis,blc,trc,clip,pivot)
	implicit none
	integer lIn,lOut,naxis,blc(naxis),trc(naxis)
	real clip(2),pivot
c
c  Calculate the velcolor for axis 3.
c
c  Inputs:
c    lIn,lOut	Handle of input and output files.
c    naxis	The number of input axes.
c    blc,trc	The corners of the input image.
c    clip	Pixels with values in range clip(1:2) are excluded.
c    pivot	Center channel for green image.
c------------------------------------------------------------------------
	include 'tmpdim.h'
	real buf(maxdim),out(maxdim,maxdim,3)
	real col(3)
	integer i,j,k,n
	logical flags(maxdim)
c
c  Initialize the output array.
c
	do n=1,3
	  do j=1,maxdim
	    do i=i,maxdim
	      out(i,j,n) = 0.
	    enddo
	  enddo
	enddo
c
c  Loop through the velocity channels and accumulate the colors.
c
	do k = blc(3),trc(3)
	  col(1) = max(0.,(pivot-k)/(pivot-blc(3)))
	  col(3) = max(0.,(k-pivot)/(trc(3)-pivot))
	  col(2) = min(1.-col(1),1.-col(3))
	  call xysetpl(lIn,1,k)
	  do j = blc(2),trc(2)
	    call xyread(lIn,j,buf)
	    call xyflgrd(lIn,j,flags)
	    do i = blc(1),trc(1)
	      if(flags(i).and.
     *		  (buf(i).le.clip(1).or.buf(i).ge.clip(2)) ) then
		do n =1,3
		  out(i,j,n) = out(i,j,n) + col(n)*buf(i)
		enddo
	      endif
	    enddo
	  enddo
	enddo
c
c  Now write out the color map.
c
	do n = 3,1,-1
	  call xysetpl(lOut,1,n)
	  do j = blc(2),trc(2)
	    call xywrite(lOut,j-blc(2)+1,out(blc(1),j,n))
	  enddo
	enddo
	end
