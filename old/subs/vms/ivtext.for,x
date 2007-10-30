c************************************************************************
	subroutine Ivtext(xc,yc,mag,text)
c
	implicit none
	integer xc,yc,mag
	character text*(*)
c
c  Draw nice text in the IVAS graphics planes.
c
c  Inputs:
c    xc,yc	Centre of where the text is to lie.
c    mag	Magnification of the text.
c    title	The text itself.
c
c------------------------------------------------------------------------
	integer width,height
	parameter(width=12,height=10)
	integer length,xo,yo,index
c
c  Characters are represented by their bit map. These bit maps are loaded
c  into the pattern ram, then displayed at the correct magnification.
c
	include 'Ivtext.h'
c
c  Determine the correct possition.
c
	length = len(text)
	xo = xc - (mag*width*length)/2
	yo = yc - (mag*height)/2
c
c  Start doing it then.
c
	do i=1,length
	  index = ichar(text(i:i))
	  if(index.ge.33.and.index.le.127)then
	    call fivasGPHmove(xo-mag*hoff(index),yo-mag*voff(index),0)
	    call fivasGPHpattern(0,0,16,16,0,0,mag-1,mag-1,
     *		pattn(1,index),16,0)
	    call fivasGPHptn(mag*16,mag*16,0,0,0)
	  else
	    call fivasGPHmove(xo,yo-mag*maxvoff,0)
	    call fivasGPHpattern(0,0,1,1,0,0,0,0,0,1,0)
	    call fivasGPHptn(mag*width,mag*maxv,0,0,0)
	  endif
	  xo = xo + mag*width
	enddo
	end

