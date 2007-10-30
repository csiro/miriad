c************************************************************************
	program imcat
	implicit none
c
c= imcat - Concatenate several images into one.
c& mchw
c: map combination
c+
c       IMCAT is a MIRIAD task to concatenate several images together,
c       along an axis (generally the frequency or velocity
c       dimension).
c@ in
c       The input images. Several file names can be entered, separated
c       by commas. No default.
c@ out
c       The output image. No default.
c@ axis
c	The axis to concatentate together. The default is the 3rd axis
c	(i.e. axis=3). You cannot concatenate together on the first axis.
c@ options
c	Task enrichment options.  Minimum match is used. Currently there
c	is only a single option:
c	  relax  This instructs IMCAT to ignore axis descriptor mismatches
c	         (e.g. pixel increments etc).  Use this with care.
c--
c
c  History:
c    10oct89 mchw  Initial version
c    27oct89 rjs   Renamed it IMCAT.
c    20jun90 mchw  Copied across beam and linetype keywords.
c    04oct90 mchw  Added crpix and cdelt keywords; removed crot
c     		     check that cdelt, crpix and crval are consistent.
c    09nov90 mchw  Added pbfwhm keyword.
c    25feb91 mjs   Changed references of itoa to be itoaf.
c    08mar91 mchw  Changed file input to keyf.
c    05aug91 pjt   Also copy the mask over, and compute new minmax
c                  fixed bug when #maps > MAXMAP, made default cdelt 1.0
c                  Only one input file open at any time
c    03nov91 rjs   Check buffer overflow and more standard history.
c    04nov91 mchw  Restored inputs to history.
c    08nov91 pjt   Increase MAXMAP to appease local maphogs
c    13jul92 nebk  Add OPTIONS=RELAX and btype to keywords
c    19jul94 nebk  Allow for roundoff in axis descriptor comparisons
c    20sep95 rjs   Really allow for roundoff in axis descriptor comparisons.
c		   Increase number of maps.
c    16jan97 rjs   Add "axis" keyword, and get it to work with an arbitrary
c		   number of axis.
c    12jun97 nebk  Copy header items for axes 6 and 7
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	character version*(*)	
        parameter(version='IMCAT: version 12-Jun-97')
	integer MAXMAP
	parameter(MAXMAP=300)
	character in(MAXMAP)*80,out*80
	integer map,nmap,plane,i,axis,lin,lout,naxis
	integer size(MAXNAX),nsize(MAXNAX),Outplane
	real cdelt(MAXNAX),crval(MAXNAX),crpix(MAXNAX)
	real cdelt1,crval1,crpix1,dmin,dmax
	logical first,relax,ok,warned,warned1,domask
	character*1 caxis,wflag
c
c  Externals.
c
	character*1 itoaf
	logical hdprsnt
c
c  Header keywords.
c
	integer nkeys
	parameter(nkeys=49)
	character keyw(nkeys)*8
	data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *	  'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','cdelt5  ',
     *    'cdelt6  ','cdelt7  ',
     *	  'crpix1  ','crpix2  ','crpix3  ','crpix4  ','crpix5  ',
     *    'crpix6  ','crpix7  ',
     *	  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *    'crval6  ','crval7  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *    'ctype6  ','ctype7  ',
     *	  'epoch   ','history ','niters  ','obstime ','object  ',
     *	  'observer','pbfwhm  ','obsra   ','obsdec  ','restfreq',
     *	  'telescop','vobs    ','ltype   ','lstart  ','lwidth  ',
     *	  'lstep   ','btype   '/
c
c  Get the input parameters.
c
        call output(version)
	call keyini
	call mkeyf('in',in,MAXMAP,nmap)
	if(nmap.le.1) call bug('f','Must have more than one input map')
	call keya('out',Out,' ')
	if(Out.eq.' ')
     *	  call bug('f','You must give an output file')
	call keyi('axis',axis,3)
	if(axis.lt.2.or.axis.gt.MAXNAX)
     *	  call bug('f','Invalid value for axis keyword')
        call decopt (relax)
	call keyfin
c
c  Give warning about relax option.
c
        wflag = 'f'
        if (relax) then
          wflag = 'w'
          call bug ('i', 'Axis descriptor mismatches will be tolerated')
        end if
c
c  Open the input maps and check sizes, crpix and cdelt.
c
	call xyopen(lin,in(1),'old',axis,size)
	call rdhdi(lin,'naxis',naxis,axis)
	naxis = max(min(naxis,MAXNAX),axis)
	if(size(1).gt.maxdim)call bug('f','Image too big for me')
	do i=axis+1,naxis
	  size(i) = 1
	enddo
	domask = hdprsnt(lin,'mask')
c
	do i=1,naxis
	  caxis = itoaf(i)
	  call rdhdr(lin,'cdelt'//caxis,cdelt(i),1.0)
	  call rdhdr(lin,'crpix'//caxis,crpix(i),0.)
	  call rdhdr(lin,'crval'//caxis,crval(i),0.)
	enddo
c
	warned = .false.
	warned1 = .false.
	do map=2,nmap
	  call xyopen(lin,in(map),'old',axis,nsize)
	  if(nsize(1).gt.maxdim)call bug('f','Image too big for me')
	  do i=axis+1,naxis
	    nsize(i) = 1
	  enddo
	  domask = domask.or.hdprsnt(lin,'mask')
c
	  do i=1,naxis
	    caxis = itoaf(i)
	    call rdhdr(lin,'cdelt'//caxis,cdelt1,1.0)
            call descmp (cdelt1, cdelt(i), ok)
	    if(.not.ok.and..not.warned) call bug(wflag,
     *		'cdelt values differ on axis '//caxis)
	    warned1 = warned1.or..not.ok
c
	    call rdhdr(lin,'crpix'//caxis,crpix1,0.)
	    call rdhdr(lin,'crval'//caxis,crval1,0.)
	    if(i.ne.axis) then
              call descmp (crval1, crval(i), ok)
   	      if(.not.ok.and..not.warned)call bug(wflag,
     *		'crval values differ on axis '//caxis)
	      warned1 = warned1.or..not.ok
              call descmp (crpix1, crpix(i), ok)
	      if(.not.ok.and..not.warned)call bug(wflag,
     *		'crpix values differ on axis '//caxis)
	      warned1 = warned1.or..not.ok
	      if(nsize(i).ne.size(i))call bug('f',
     *		'The images do not have compatable dimensions')
	    else
	      ok = abs((crval1+(1-crpix1)*cdelt1)-
     *		(crval(i)+(1-crpix(i))*cdelt(i) + size(i)*cdelt(i)))
     *		.le.0.01*abs(cdelt1)
	      if(.not.ok.and..not.warned)call bug(wflag,
     *		'Images are not contiguous on axis '//caxis)
	      warned1 = warned1.or..not.ok
	    endif
	  enddo
	  warned = warned.or.warned1
	  call xyclose(lin)
	  size(axis) = size(axis) + nsize(axis)
	enddo
c
c  Open the output file, and make its header from the first input image.
c
	call xyopen(lin,In(1),'old',axis,nsize)
	call xyopen(lOut,Out,'new',naxis,size)
	do i=1,nkeys
	  call hdcopy(lin,lOut,keyw(i))
	enddo
c
c  Write the history.
c
	call hisopen(lOut,'append')
	call hiswrite(lOut,'IMCAT: Miriad '//version)
	call hisinput(lOut,'IMCAT')
	call hisclose(lOut)
c
c  Copy the maps into the output, plane by plane, row by row.
c  Find new max/min.
c
	first = .true.
	Outplane = 0
	do map = 1,nmap
	  if(map.gt.1) call xyopen(lin,in(map),'old',axis,nsize)
c
	  do plane = 1,nsize(axis)
	    Outplane = Outplane + 1
	    call DatCpy(lin,plane,lout,Outplane,nsize,axis,domask,
     *						dmin,dmax,first)
	  enddo
	  call xyclose(lin)
	enddo
c
c  Update header info' and close output file.
c
	call wrhdr(lout,'datamin',dmin)
	call wrhdr(lout,'datamax',dmax)
	call xyclose(lOut)

	end
c************************************************************************
	subroutine DatCpy(lin,Inplane,lout,Outplane,size,axis,domask,
     *						dmin,dmax,first)
c
	implicit none
	integer axis,Inplane,Outplane,size(axis-1),lin,lout
	logical first,domask
	real dmin,dmax
c
c  Input:
c    axis
c    domask
c    Inplane
c    Outplane
c    size
c  Input/Output:
c    first	True if this is the very first call.
c    dmin,dmax	Min and max data value.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	integer inp(MAXNAX),outp(MAXNAX)
	integer i,row
	logical done
	real Data(MAXDIM)
	logical mask(MAXDIM)
c
	do i=1,axis-1
	  inp(i) = 1
	  outp(i) = 1
	enddo
	inp(axis)  = Inplane
	outp(axis) = Outplane
c
	if(axis.ge.3)then
	  done = .false.
	  dowhile(.not.done)
	    call xysetpl(lin,axis-2, Inp(3))
	    call xysetpl(lOut,axis-2,Outp(3))
	    do row=1,size(2)
	      call xyread(lin,row,Data)
	      if(domask)call xyflgrd(lin,row,mask)
	      do i=1,size(1)
	        if(first)then
		  dmin=data(i)
		  dmax=data(i)
		  first = .false.
	        else
		  dmin=min(dmin,data(i))
		  dmax=max(dmax,data(i))
	        endif
	      enddo
	      call xywrite(lout,row,Data)
	      if(domask)call xyflgwr(lout,row,mask)
	    enddo
	    if(axis.gt.3)then
	      call planeinc(axis-3,size(3),inp(3),done)
	      call planeinc(axis-3,size(3),outp(3),done)
	    else
	      done = .true.
	    endif
	  enddo
	else
	  call xyread(lin,1,Data)
	  if(domask)call xyflgrd(lin,1,mask)
	  do i=1,size(1)
	    if(first)then
	      dmin=data(i)
	      dmax=data(i)
	      first = .false.
	    else
	      dmin=min(dmin,data(i))
	      dmax=max(dmax,data(i))
	    endif
	  enddo
	  call xywrite(lout,Outp(2),Data)
	  if(domask)call xyflgwr(lout,Outp(2),mask)
	endif
c
	end
c************************************************************************
	subroutine planeinc(n,size,plane,done)
c
	implicit none
	logical done
	integer n,size(n),plane(n)
c------------------------------------------------------------------------
	integer k
c
	done = .true.
	k = 1
	dowhile(done.and.k.le.n)
	  done = plane(k).ge.size(k)
	  if(done)then
	    plane(k) = 1
	  else
	    plane(k) = plane(k) + 1
	  endif
	  k = k + 1
	enddo
c
	end
c************************************************************************
      subroutine decopt (relax)
      implicit none
c
      logical relax
c
c     Decode options array into named variables.
c
c   Output:
c     relax     If true issue warnings about mismatched axis
c               descriptors between images instead of fatal error
c
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 1)
c
      character opshuns(maxopt)*5
      logical present(maxopt)
      data opshuns /'relax'/
c
      call options ('options', opshuns, present, maxopt)
c
      relax = present(1)
c
      end
c************************************************************************
      subroutine descmp (r1, r2, ok)
c
      implicit none
      real r1, r2
      logical ok
c
c     Check axis descriptors agree allowing for roundoff
c
c     Output
c       ok     True if axis descriptors agree.
c-----------------------------------------------------------------------
      real dmax
c
      dmax = max(abs(r1),abs(r2))
      ok = .not.(abs(r1-r2).gt.dmax*1e-6 .or. r1*r2.lt.0.0d0)
c
      end 
