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
c  The baseline number is a number between 1 and N*(N-1)/2 for
c  pure cross correlating pairs.
c  This routine does not allow auto-correlations to occur in the data.
c
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
c    27oct00 mchw Changes to support more antennas.
c    24apr08 pjt  Barf if auto correlations
c------------------------------------------------------------------------
	integer i, ant1, ant2
c
        call basant(preambl(4),ant1,ant2)
	if (ant1.eq.ant2)call bug('f','uvgetbl: cannot handle autocorr')
	if(ant1.le.ant2)then
	  bl = ((ant2-2)*(ant2-1))/2 + ant1
	else
	  bl = ((ant1-2)*(ant1-1))/2 + ant2
	  preambl(1) = -preambl(1)
	  preambl(2) = -preambl(2)
	  do i=1,nread
	    data(i) = conjg(data(i))
	  enddo
	endif
c
	end
