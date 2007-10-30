c************************************************************************
	subroutine IvOfm(lut,beg,fin)
c
	implicit none
	character lut*(*)
	integer beg,fin
c
c  Modify and load a lookup table. The modified Lut is a stretched version
c  of one of the fiexed luts.
c
c  Input:
c    LUT	The lookup table name. Can be either 'Colour' or 'B&W'.
c    beg,fin	Colour of pixval.le.beg is given by ofm(1,?),
c			  pixval.ge.fin		    ofm(256,?)
c		They vary linearly in between.
c
c------------------------------------------------------------------------
	integer myofm(256)
	logical first
	integer i,j,r,r0,r1,Range
	include 'ivas.h'
	include 'Ivofm.h'
	save first
	data first/.true./
c
c  If its the first call, initialise Tonry's table.
c
	if(first)then
	  call jttable(ofm(1,8))
	  first = .false.
	endif
c
c  Determine the tables that we have to fiddle with. Tables 1 thru 3 are
c  red, green and blue tables for a colour setup, whereas table 4 is for
c  a black-white setup.
c
	if(lut.eq.'Colour')then
	  r0 = 1
	  r1 = 3
	else if(lut.eq.'Tonry')then
	  r0 = 8
	  r1 = 10
	else if(lut.eq.'IRAF')then
	  r0 = 5
	  r1 = 7
	else
	  r0 = 4
	  r1 = 4
	endif
c
c  If there is no "fiddle" to do, just load the table as is.
c
	if(beg.eq.0.and.fin.eq.255)then
	  do r=r0,r1
	    call fivasVPofm(ofm(1,r),0,256,PassIn,bank(r))
	  enddo
c
c  Else we have to fiddle it.
c
	else
	  Range = abs(fin - beg) + 1
	  do r=r0,r1
	    do i=1,256
	      j = min( max( 256*(i-1-beg)/Range, 0), 255) + 1
	      myofm(i) = ofm(j,r)
	    enddo
	    call fivasVPofm(myofm,0,256,PassIn,bank(r))
	  enddo
	endif
c
	end
c************************************************************************
      subroutine jttable (table)
c
      implicit none
      integer table(256,3)
c
c     Generate John Tonry's colour table
c
c--------------------------------------------------------------
      real f1, f2, bright, dfrac, frac
      integer cstep(6), itotal, i, j, k, l, iwidth, idist, 
     *l1, l2, ir, ig, ib
      data cstep /24, 32, 8, 8, 32, 24/
c
      itotal = 0
      do i = 1, 6
         itotal = itotal + cstep(i)
      end do
c
      iwidth = 256 / itotal
      idist = 0
      f1 = 0.5
      f2 = 1.0
c
      do j = 1, 6
         do i = 1, cstep(j)
            k = idist * iwidth
            dfrac = real(idist) / real(itotal - 1)
            bright = 255.0 * (dfrac * f2 + (1.0 - dfrac) * f1)
            frac = real(i-1) / cstep(j)
            if (mod(j,2).eq.1) frac = 1.0 - frac
            l1 = nint(frac * bright)
            l2 = nint(bright)
            if (j.eq.1) then
               ir = l1
               ig = 0
               ib = l2
            else if (j.eq.2) then
               ir = 0
               ig = l1
               ib = l2
            else if (j.eq.3) then
               ir = 0
               ig = l2
               ib = l1
            else if (j.eq.4) then
               ir = l1
               ig = l2
               ib = 0
            else if (j.eq.5) then
               ir = l2
               ig = l1
               ib = 0
            else if (j.eq.6) then
               ir = l2 
               ig = l1
               ib = l1
            end if
c
            do l = 1, iwidth
               table(k+l,1) = ir
               table(k+l,2) = ig
               table(k+l,3) = ib
            end do
            idist = idist + 1
         end do
      end do
c
      table(1,1) = 0
      table(1,2) = 0
      table(1,3) = 0
      table(256,1) = 255
      table(256,2) = 255
      table(256,3) = 255
c
      end
