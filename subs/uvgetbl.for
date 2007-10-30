c********1*********2*********3*********4*********5*********6*********7*c
c* uvgetbl -- Get the baseline number, and conjugate the data if needed.
c& mchw
c: uv-i/o,uv-data,uv-selection
c+
	subroutine uvgetbl(preambl,data,nread,bl)
c
	implicit none
	integer nread,bl
	double precision preambl(4)
	complex data(nread)
c
c  Determine the baseline number, and conjugate the data if necessary.
c
c  Input:
c    nread	The number of channels.
c  Output:
c    bl		baseline number.
c  Input/Output:
c    preambl	Preamble.
c    data	The correlation data to be averaged.
c--
c
c    History:
c
c    mchw --apr93 Extracted from a task and installed as a genl subr.
c    mjs  22apr93 Trivial doc mod.
c------------------------------------------------------------------------
	integer i,i1,i2
c
	i2 = nint(preambl(4))
	i1 = i2 / 256
	i2 = i2 - 256 * i1
	if(i1.le.i2)then
	  bl = ((i2-2)*(i2-1))/2 + i1
	else
	  bl = ((i1-2)*(i1-1))/2 + i2
	  preambl(1) = -preambl(1)
	  preambl(2) = -preambl(2)
	  do i=1,nread
	    data(i) = conjg(data(i))
	  enddo
	endif
c
	end
