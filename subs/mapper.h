c
c  Gridding related parameters:
c
c    MAXCGF	Maximum size of the convolutional gridding function
c		array.
c    n1,n2	Size of the transform.
c    width	Width of the gridding function in uv-plane cells.
c    ncgf	Size of the cgf array.
c    cgf	The convolutional gridding function.
c    xcorr,ycorr Correction arrays.
c
	include 'maxdim.h'
	integer MAXCGF,MAXT
	parameter(MAXCGF=2048,MAXT=5)
c
	real scale(MAXPNT)
	real cgf(MAXCGF),xcorr(MAXDIM),ycorr(MAXDIM),umax,vmax
	integer tscr
	integer width,ncgf,offcorr,chan1,chan2,npnt,totchan
	integer nchan(MAXT),nx(MAXT),ny(MAXT),nt
	integer n1,n2,nu,nv,u0,v0,nxc,nyc,pad
	logical ginit
        ptrdiff pBuff,nBuff8,nextra8,nvis
	character mode*8
	common/mapcom/pBuff,scale,cgf,xcorr,ycorr,umax,vmax,
     *	  tscr,width,ncgf,offcorr,chan1,chan2,npnt,totchan,
     *	    nchan,nx,ny,nt,n1,n2,nu,nv,u0,v0,nxc,nyc,ginit,
     *      pad,nvis,nBuff8,nextra8
	common/mapcomc/mode
c
	integer num
	common/mapcom2/num
