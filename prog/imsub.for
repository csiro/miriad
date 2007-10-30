c************************************************************************
	program imsub
	implicit none
c
c= imsub - Extract subportion of an image
c& mchw
c: map manipulation
c+
c	IMSUB is a MIRIAD task which extracts a subportion of an image.
c@ in
c	The input image. No default.
c@ out
c	The output image. No default.
c@ region
c	The region of interest. The default is the entire input image.
c	See the Users Manual for instructions on how to specify this.
c@ incr
c	Increment to be used along each axis. Default is 1.
c--
c  History:
c    rjs Dark-ages Original version.
c    rjs 16jul89   Added incr parameter.
c    nebk 11aug89  Add incr parameter to history.  This is my lot in life.
c    rjs  14sep89  Fixed nebk's addition to history. Is this my lot in life?
c    rjs  24apr90  Copied across lstart,lstep,ltype,lwidth keywords.
c    rjs  30apr90  Changed call sequence to BoxInput.
c    mchw 20jun90  Copied across bmaj,bmin,bpa keywords.
c    mchw 09nov90  Copied across pbfwhm keyword.
c    mjs  25feb91  Changed references of mitoa to mitoaf.
c    rjs   8oct91  Updated history writing somewhat.
c    rjs   7may92  Apply mask after determining region to copy.
c    nebk 26may92  Copy "btype" across
c    pjt   9jul92  Length of filenames to be 80, not 24!!!!
c    rjs  25jun93  Trivial change to a history message.
c    rjs   8nov94  Get incr and blanking masks to work correctly.
c    rjs  17aug95  Copy mostable to output.
c    rjs  21nov95  Use maxnax.h.
c    nebk 10jan96  Change crpix and cdelt to double
c    rjs  02jul97  cellscale change.
c    rjs  18jul97  Correct handling of flags when incr != 1.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Imsub: version 1.0 18-Jul-97' )
c
	integer maxboxes
	parameter(maxboxes=2048)
	include 'maxdim.h'
	include 'maxnax.h'
	character in*80,out*80
	integer Inplane(maxnax),Outplane(maxnax),one(maxnax)
	integer blc(maxnax),trc(maxnax),Nin(maxnax),Nout(maxnax),i,j
	integer i0,j0,incr(maxnax)
	integer naxis,boxes(maxboxes)
	integer lIn,lOut
	logical done,rect
	double precision crpix,cdelt
	real Data(maxdim),Data2(maxdim)
c
c  Externals.
c
	logical BoxRect
c
c  Header keywords.
c
	integer nkeys
	parameter(nkeys=35)
	character keyw(nkeys)*8
	data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *	  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *	  'obstime ','epoch   ','history ','instrume','niters  ',
     *	  'object  ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *	  'restfreq','telescop','vobs    ','btype   ','rms     ',
     *	  'ltype   ','lstart  ','lwidth  ','lstep   ','mostable',
     *	  'cellscal'/
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('in',In,' ')
	call keya('out',Out,' ')

	if(In.eq.' '.or.Out.eq.' ')
     *	  call bug('f','You must give an input and output file')
	call BoxInput('region',in,boxes,maxboxes)
	do i=1,maxnax
	  call keyi('incr',incr(i),1)
	  if(incr(i).le.0) call bug('f','Bad increment')
	enddo
	call keyfin
c
c  Open the input.
c
	call xyopen(lIn,In,'old',maxnax,Nin)
	if(Nin(1).gt.maxdim)
     *	  call bug('f','Image too big for me to handle')
	call rdhdi(lIn,'naxis',naxis,0)
	call BoxSet(boxes,maxnax,Nin,' ')
c
c  Determine portion of image to copy.
c
	call BoxInfo(boxes,maxnax,blc,trc)
	call BoxMask(lIn,boxes,maxboxes)
	rect = BoxRect(boxes)
	do i=1,maxnax
	  Nout(i) = (trc(i) - blc(i) + incr(i))/incr(i)
	enddo
c
c  Make the output file, and make its header.
c
	call xyopen(lOut,Out,'new',naxis,Nout)
	do i=1,naxis
	  call rdhdd(lIn, 'cdelt'//char(ichar('0')+i),cdelt,1.0d0)
	  call rdhdd(lIn, 'crpix'//char(ichar('0')+i),crpix,1.0d0)
	  cdelt = cdelt * incr(i)
	  crpix = (crpix - blc(i))/incr(i) + 1
	  call wrhdd(lOut,'cdelt'//char(ichar('0')+i),cdelt)
	  call wrhdd(lOut,'crpix'//char(ichar('0')+i),crpix)
	enddo
	do i=1,nkeys
	  call hdcopy(lIn,lOut,keyw(i))
	enddo
	call hisopen(lOut,'append')
        call hiswrite (lOut, 'IMSUB: Miriad '//version)
	call hisinput(lOut,'IMSUB')
	call hisclose(lOut)
c
c  Initialise the plane indices.
c
	do i=3,maxnax
	  one(i-2) = 1
	  Inplane(i-2) = blc(i)
	  Outplane(i-2) = 1
	enddo
c
c  Make a copy of the portion being copied.
c
	done = .false.
	do while(.not.done)
	  call xysetpl(lIn,maxnax-2,Inplane)
	  call xysetpl(lOut,maxnax-2,Outplane)
	  j0 = blc(2)
	  do j=1,Nout(2)
	    call xyread(lIn,j0,Data)
	    j0 = j0 + incr(2)
	    if(incr(1).gt.1)then
	      i0 = blc(1)
	      do i=1,Nout(1)
		Data2(i) = Data(i0)
		i0 = i0 + incr(1)
	      enddo
	      call xywrite(lOut,j,Data2)
	    else
	      call xywrite(lOut,j,Data(Blc(1)))
	    endif
	  enddo
c
	  if(.not.rect)call MaskIt(lOut,maxnax-2,Inplane,boxes,
     *			blc(1),blc(2),nOut(1),nOut(2),incr(1),incr(2))
	  call planeinc(maxnax-2,incr(3),blc(3),trc(3),Inplane,done)
	  call planeinc(maxnax-2,one,one,Nout(3),Outplane,done)
	enddo
	call xyclose(lIn)
	call xyclose(lOut)
	end
c************************************************************************
	subroutine MaskIt(tno,ndim,plane,boxes,x0,y0,n1,n2,inc1,inc2)
c
	implicit none
	integer tno,ndim,plane(ndim),boxes(*),x0,y0,n1,n2,inc1,inc2
c
c  Get the mask to apply to the output, and write it out.
c
c  Input:
c    tno	Handle of the output file.
c    nsize	Gives the dimension of "plane".
c    plane	The array of indices determining which plane we are
c		processing.
c    boxes	The intermediate form of the region specification.
c    x0,y0	The blc corner of the output.
c    n2		Size of the output along the y axis.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer maxRuns
	parameter(maxRuns=8*maxdim)
	integer xminv,xmaxv,yminv,ymaxv,nRuns,nRuns2,iRuns
	integer x1,x2,y,xprev,yprev,t
	integer Runs(3,maxRuns)
c
c  Get the information about what in the output is good.
c
	call BoxRuns(ndim,plane,' ',boxes,
     *		Runs,maxRuns,nRuns,xminv,xmaxv,yminv,ymaxv)
c
c  If we don't need to decimate, write it straight out.
c  Otherwise work out the new runs array.
c
	if(inc1.eq.1.and.inc2.eq.1)then
	  call PutRuns(tno,Runs,nRuns,1-x0,1-y0,n1,n2)
	else
	  xprev = 0
	  yprev = 0
	  nRuns2 = 0
	  do iRuns=1,nRuns
	    y = Runs(1,iRuns) - y0
	    if(y.ge.0.and.y.le.(n2-1)*inc2.and.mod(y,inc2).eq.0)then
	      x1 = max(Runs(2,iRuns),x0)
	      x1 = inc1*((x1 - x0 + inc1 - 1)/inc1)
	      x2 = min(Runs(3,iRuns)-x0,(n1-1)*inc1)
	      if(x1.le.x2)then
	        y = y/inc2 + 1
	        t = x1/inc1 + 1
		x2 = t + (x2-x1+1)/inc1
		x1 = t
	        if(y.eq.yprev.and.x1.eq.xprev)then
		  Runs(3,nRuns2) = x2
	        else
		  nRuns2 = nRuns2 + 1
		  Runs(1,nRuns2) = y
		  Runs(2,nRuns2) = x1
		  Runs(3,nRuns2) = x2
	        endif
		xprev = x2 + 1
	        yprev = y
	      endif
	    endif
	  enddo
	  call PutRuns(tno,Runs,nRuns2,0,0,n1,n2)
	endif
c
	end
c************************************************************************
	subroutine planeinc(n,incr,blc,trc,plane,done)
c
	implicit none
	integer n,blc(n),trc(n),plane(n),incr(n)
	logical done
c
c  Move to the next plane.
c
c------------------------------------------------------------------------
	integer k
c
	k = 1
	done = .true.
c
	do while(done.and.k.le.n)
	  done = plane(k).ge.trc(k)
	  if(done)then
	    plane(k) = blc(k)
	  else
	    plane(k) = plane(k) + incr(k)
	  endif
	  k = k + 1
	enddo
	end
