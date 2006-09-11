c********1*********2*********3*********4*********5*********6*********7*c
c* Axistype - Find the axis label and plane value in user friendly units
c& mchw
c: plotting
c+
	subroutine AxisType(lIn,axis,plane,ctype,label,value,units)
c
	implicit none
	integer lIn,axis,plane
	character ctype*9,label*13,units*13
	double precision value
c
c Find the axis label and plane value in user friendly units.
c
c  Inputs:
c    lIn	The handle of the image.
c    axis	The image axis.
c    plane	The image plane along this axis.
c  Output:
c    ctype	The official ctype for the input axis.
c    label	A nice label for this axis.
c    value	The value at the plane along this axis.
c    units	User friendly units for this axis.
c--
c		  26jun90 mchw.
c----------------------------------------------------------------------c
	double precision pi
	parameter(pi=3.14159654)
	integer naxis
	real crval,cdelt,crpix
	character*1 caxis
c
c  Externals.
c
	character*1 itoaf
	character angles*13
c
c  Get ctype and value for input plane.
c
	caxis = itoaf(axis)
	call rdhdi(lIn,'naxis',naxis,1)
	if(axis.le.naxis)then
	  call rdhda(lIn,'ctype'//caxis,ctype,' ')
	  call rdhdr(lIn,'crval'//caxis,crval,0.)
	  call rdhdr(lIn,'crpix'//caxis,crpix,0.)
	  call rdhdr(lIn,'cdelt'//caxis,cdelt,0.)
	  value = crval + (plane-crpix)*cdelt
c
c  Convert to user friendly label and units.
c
	  if(ctype(1:2).eq.'RA')then
	    label = '     RA      '
	    units = angles(value*12.d0/pi)
	  else if(ctype(1:3).eq.'DEC')then
	    label = '     DEC     '
	    units = angles(value*180.d0/pi)
	  else if(ctype(1:4).eq.'VELO') then
	    label = '  Velocity   '
	    write(units,'(g13.3)') value
	  else if(ctype(1:4).eq.'FREQ')then
	    label = '  Frequency  '
	    write(units,'(g13.6)') value
	  else
	    label = ctype
	    write(units,'(g13.6)') value
	  endif
	else
	  value = 0.0d0
	  label = 'no axis '//caxis
	  units = ' '
	end if
	end

