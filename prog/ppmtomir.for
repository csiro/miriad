c************************************************************************
	program ppmtomir
	implicit none
c
c= PPMTOMIR -- Convert a ascii PPM file to Miriad format.
c& rjs
c: data transfer
c+
c	PPMTOMIR is a Miriad task which converts a non-raw PPM image.
c	Use "pnmnoraw" to convert to a non-raw PPM. Also use the
c	PBM/PGM/PPM/PNM packages to convert from various formats to
c	non-raw PPM format.
c@ in
c	The input PPM file.
c@ out
c	The output Miriad file.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='PpmtoMir: version 1.0 17-Dec-97')
	character in*64,out*64
	integer nsize(2),i,j,k,val(3),lOut,v,levels(16)
	real data(MAXDIM)
c
c  Externals.
c
	integer tinLen,tinNext
c
	data levels/      0, 917504,1441792,1966080,1973248,1977344,
     *		    1981440,1457152, 932864,  15416,  15448,  15480,
     *		      11384,   7288,    120,2097151/
c
	call output(version)	
	call keyini
	call keya('in',in,' ')
	if(in.eq.' ')call bug('f','Input must be given')
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','Output must be given')
	call keyfin
c
	call tinOpen(in,'s')
	if(tinNext().le.0)call bug('f','Error reading input')
	if(tinNext().le.0)call bug('f','Error reading input')
	call tinGeti(nsize(1),0)
	if(nsize(1).gt.MAXDIM)call bug('f','Image too big')
	call tinGeti(nsize(2),0)
	if(tinNext().le.0)call bug('f','Error reading input')
	if(tinNext().le.0)call bug('f','Error reading input')
	call xyopen(lOut,out,'new',2,nsize)
	do j=nsize(2),1,-1
	  do i=1,nsize(1)
	    call tinGeti(val(1),0)
	    call tinGeti(val(2),0)
	    call tinGeti(val(3),0)
	    v = val(1) + val(2) + val(3)
c	    v = val(1)/2 + 128*(val(2)/2) + 128*128*(val(3)/2)'
c
c	    data(i) = 75.5
c	    do k=1,16
c	      if(v.eq.levels(k)) data(i) = -(29.5 - 7*k)
c	    enddo
	  enddo
	  call xywrite(lOut,j,data)
	enddo
c
	call wrhda(lOut,'ctype1','X-pixels')
	call wrhdr(lOut,'crval1',real(nsize(1)/2+1))
	call wrhdr(lOut,'crpix1',real(nsize(1)/2+1))
	call wrhdr(lOut,'cdelt1',1.0)
	call wrhda(lOut,'ctype2','X-pixels')
	call wrhdr(lOut,'crval2',real(nsize(2)/2+1))
	call wrhdr(lOut,'crpix2',real(nsize(2)/2+1))
	call wrhdr(lOut,'cdelt2',1.0)
c
	call xyclose(lOut)
	call tinClose
	end
