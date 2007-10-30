c************************************************************************
	program imcat
	implicit none
c
c= imcat - Concatenate several images to one cube
c& mchw
c: map combination
c+
c       IMCAT is a MIRIAD task to concatenate several images together,
c       along the third dimension (generally the frequency or velocity
c       dimension).
c@ in
c       The input images. Several file names can be entered, separated
c       by commas. No default.
c@ out
c       The output image. No default.
c@ options
c	Task enrichment options.  Minimum match is active.
c	
c	relax    instructs IMCAT to ignore axis descriptor mismatches
c	         (e.g. pixel increments etc).  Use with care.
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
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)	
        parameter(version='IMCAT: version 19-jul-94')
	integer maxmap,naxis
	parameter(maxmap=64,naxis=3)
	character in(maxmap)*80,out*80,file*80
	integer Outplane,map,nmap,row,plane,i
	integer lin,nplane(maxmap),lOut,size(naxis),nsize(naxis)
	real Data(maxdim),cdelt(naxis),crval(naxis),crpix(naxis)
	real cdelt1,crval1,crpix1,dmin,dmax
	logical mask(maxdim),first,relax,ok
	character*1 caxis,wflag
c
c  Externals.
c
	character*1 itoaf
c
c  Header keywords.
c
	integer nkeys
	parameter(nkeys=40)
	character keyw(nkeys)*8
	data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *	  'cdelt1  ','cdelt2  ','cdelt3  ',
     *	  'crpix1  ','crpix2  ','crpix3  ',
     *	  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *	  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *	  'date-obs','epoch   ','history ','instrume','niters  ',
     *	  'object  ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *	  'restfreq','telescop','vobs    ','xshift  ','yshift  ',
     *	  'ltype   ','lstart  ','lwidth  ','lstep   ','btype   '/
c
        call output(version)
c
c  Get the input parameters.
c
	call keyini
	call mkeyf('in',in,maxmap,nmap)
	call keya('out',Out,' ')
        call decopt (relax)
	call keyfin
c
c  Check the inputs.
c
	if(nmap.le.1) call bug('f','Must have more than one input map')
	if(Out.eq.' ')
     *	  call bug('f','You must give an output file')
        wflag = 'f'
        if (relax) then
          wflag = 'w'
          call bug ('i', 'Axis descriptor mismatches will be tolerated')
        end if
c
c  Open the input maps and check sizes, crpix and cdelt.
c
	file = in(1)
	call xyopen(lin,file,'old',naxis,size)
	nplane(1) = size(3)
	do i=1,naxis
	  caxis = itoaf(i)
	  call rdhdr(lin,'cdelt'//caxis,cdelt(i),1.0)
	  call rdhdr(lin,'crpix'//caxis,crpix(i),0.)
	  call rdhdr(lin,'crval'//caxis,crval(i),0.)
	enddo
	if(size(1).gt.maxdim)call bug('f','Image too big for me')
c
	map = 2
	do while(map.le.nmap)
	  if(naxis.gt.3)
     *	  call bug('f','Image has too many axes for me to handle')
	  file = in(map)
	  call xyopen(lin,file,'old',naxis,nsize)
	  if(nsize(1).gt.maxdim)call bug('f','Image too big for me')
	  if(nsize(1).ne.size(1).or.nsize(2).ne.size(2))
     *	  call bug('f','Each map must have same xy dimensions')
	  do i=1,naxis
	    caxis = itoaf(i)
	    call rdhdr(lin,'cdelt'//caxis,cdelt1,1.0)
            call descmp (cdelt1, cdelt(i), ok)
	    if(.not.ok) call bug(wflag,'cdelt not the same on axis '//
     *                           caxis)
c
	    call rdhdr(lin,'crpix'//caxis,crpix1,0.)
	    call rdhdr(lin,'crval'//caxis,crval1,0.)
	    if(i.le.(naxis-1)) then
              call descmp (crval1, crval(i), ok)
   	      if(.not.ok) call bug(wflag,'crval not the same on axis '//
     *                             caxis)
              call descmp (crpix1, crpix(i), ok)
	      if(.not.ok) call bug(wflag,'crpix not the same on axis '//
     *                             caxis)
	    else if(i.eq.naxis) then
	      if((crval1+(1-crpix1)*cdelt1).ne.
     *		(crval(i)+(1-crpix(i))*cdelt(i) + size(i)*cdelt(i)))
     *			 call bug(wflag,'channels are not contiguous')
	    endif
	  enddo
	  call xyclose(lin)
	  nplane(map) = nsize(3)
	  size(3) = size(3) + nsize(3)
	  map = map + 1
	enddo
c
c  Open the output file, and make its header from the first input image.
c
	call xyopen(lin,In(1),'old',naxis,nsize)
	call xyopen(lOut,Out,'new',3,size)
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
	outplane = 1
	do map = 1,nmap
	  if(map.gt.1) call xyopen(lin,in(map),'old',naxis,nsize)
	  do plane = 1,nplane(map)
	    call xysetpl(lin,1,plane)
	    call xysetpl(lOut,1,Outplane)
	    do row=1,size(2)
	      call xyread(lin,row,Data)
	      call xyflgrd(lin,row,mask)
	      do i=1,size(1)
		if(mask(i))then
		  if(first)then
		    dmin=data(i)
		    dmax=data(i)
		    first = .false.
		  else
		    dmin=min(dmin,data(i))
                    dmax=max(dmax,data(i))
		  endif
		endif
	      enddo
	      call xywrite(lout,row,Data)
	      call xyflgwr(lout,row,mask)
	    enddo
	    outplane = outplane + 1
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
c
c
      subroutine decopt (relax)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     relax     If true issue warnings about mismatched axis
c               descriptors between images instead of fatal error
c
c-----------------------------------------------------------------------
      implicit none
c
      logical relax
cc
      integer maxopt
      parameter (maxopt = 1)
c
      character opshuns(maxopt)*5
      logical present(maxopt)
      data opshuns /'relax'/
c-----------------------------------------------------------------------
      call options ('options', opshuns, present, maxopt)
c
      relax = present(1)
c
      end
c
c
      subroutine descmp (r1, r2, ok)
c-----------------------------------------------------------------------
c     Check axis descriptors agree allowing for roundoff
c
c     Output
c       ok     True if axis descriptors agree.
c-----------------------------------------------------------------------
      real r1, r2
      logical ok
cc
      real dmax
c-----------------------------------------------------------------------
      dmax = max(abs(r1),abs(r2))
      ok = .not.(abs(r1-r2).gt.dmax*1e-6 .or. r1*r2.lt.0.0d0)
c
      end 
