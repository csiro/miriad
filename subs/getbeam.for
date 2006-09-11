c* GetBeam -  Get beam from image header.
c& mchw
c: utilities
c+
	subroutine GetBeam(lIn,naxis,bunit,bmaj,bmin,omega,cbof)
	implicit none
	integer lIn,naxis
	character*10 bunit
	real bmaj,bmin,omega,cbof
c
c  Get beam from image header.
c
c  Inputs:
c    lIn	The handle of the Image.
c    naxis	Number of image axes.
c  Outputs:
c    bunit	The pixel units.
c    bmaj,bmin	Beam major and minor axes [radians].
c    omega	Beam solid angle [steradians]
c    cbof	Beam oversampling factor.
c--
c  History:
c		mchw june 1990
c    29apr91  mchw  Make cbof exact for Gaussian beam.
c    30may91  mchw,rjs Check that both RA and DEC axes present.
c----------------------------------------------------------------------c
	double precision pi
	parameter(pi=3.141592654)
	real cdelt
	integer i,count
	character ctype*9,cin*1,itoaf*1
c
c  Look for RA and DEC axes and get pixel size.
c
	count = 0
	omega = 1.
	do i = 1,naxis
	  cin = itoaf(i)
	  call rdhda(lIn,'ctype'//cin,ctype,' ')
	  if(ctype(1:2).eq.'RA'.or.ctype(1:3).eq.'DEC') then
	    call rdhdr(lIn,'cdelt'//cin,cdelt,0.)
	    omega = omega*cdelt
	    count = count + 1
	  endif
	enddo
c
	call rdhda(lIn,'bunit',bunit,' ')
	call rdhdr(lIn,'bmaj',bmaj,0.)
	call rdhdr(lIn,'bmin',bmin,0.)
	if(count.eq.2.and.bunit.eq.'JY/BEAM'
     *		.and.bmaj*bmin*omega.ne.0.)then
	  cbof = abs(bmaj*bmin/omega)*pi/(4.*log(2.))
	  omega = pi * bmaj * bmin /(4.*log(2.))
	else if(count.eq.2.and.bunit.eq.'JY/PIXEL'.and.omega.ne.0.)then
	  cbof = 1.
	  omega = abs(omega)
	else
	  cbof = 1.
	  omega = 1.
	endif
	end

