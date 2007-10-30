c************************************************************************
	subroutine PcvtInit(coObj1d,coObj2d)
c
	implicit none
	integer coObj1d,coObj2d
c
c  Initialise the coordinate system conversion routines.
c------------------------------------------------------------------------
	include 'pcvt.h'
	double precision dtemp,crval1,crpix1,cdelt1,crval2,crpix2,cdelt2
	double precision epoch1,epoch2
	character ctype1*16,ctype2*16
	integer i,l1,l2
c
c  Externals.
c
	integer len1
	double precision epo2jul
c
	coObj1 = coObj1d
	coObj2 = coObj2d
c
	call coGetd(coObj1,'naxis',dtemp)
	naxis = nint(dtemp)
	call coGetd(coObj2,'naxis',dtemp)
	if(naxis.ne.nint(dtemp))call bug('f','Differing number of axes')
c
	nop = .true.
	dofk45z = .false.
	dofk54z = .false.
	ira = 0
	idec = 0
c
	do i=1,naxis
	  call coAxGet(coObj1,i,ctype1,crpix1,crval1,cdelt1)
	  call coAxGet(coObj2,i,ctype2,crpix2,crval2,cdelt2)
c
c  Velocity and frequency axes.
c
	  if(ctype2(1:4).eq.'VELO'.or.ctype2(1:4).eq.'FELO')then
	    call coVelSet(coObj1,ctype2)
	  else if(ctype2(1:4).eq.'FREQ'.and.
     *		  ctype1(1:4).ne.'FREQ')then
	    call coVelSet(coObj1,'FREQ')
c
c  RA and DEC axes.
c
	  else if((ctype2(1:4).eq.'RA--'.or.ctype2(1:4).eq.'DEC-')
     *	     .and.(ctype1(1:4).eq.ctype2(1:4)))then
	    if(ctype1(1:4).eq.'RA--')then
	      ira = i
	    else
	      idec = i
	    endif
	    call coGetd(coObj1,'epoch',epoch1)
	    call coGetd(coObj2,'epoch',epoch2)
	    if(epoch1.lt.1800)epoch1 = epoch2
	    if(abs(epoch1-epoch2).gt.0.1)then
	      if(abs(epoch1-1950).le.0.1.and.
     *	         abs(epoch2-2000).le.0.1)then
	        dofk45z = .true.
	      else if(abs(epoch1-2000).le.0.1.and.
     *		      abs(epoch2-1950).le.0.1)then
		dofk54z = .true.
	      else
	        call bug('f','Unsupported epoch conversion requested')
	      endif
	    endif
c
c  All other conversions.
c
	  else
	    l1 = index(ctype1,'-')-1
	    if(l1.le.0)l1 = len(ctype1)
	    l2 = index(ctype2,'-')-1
	    if(l2.le.0)l2 = len(ctype2)
	    if(ctype1(1:l1).ne.ctype2(1:l2))then
	      l1 = len1(ctype1)
	      l2 = len1(ctype2)
	      call bug('w','Error converting between axis types '//
     *		ctype1(1:l1)//' and '//ctype2(1:l2))
	      call bug('f','Impossible or unimplemented conversion')
	    endif
	  endif
	enddo
c
c  If we are to do epoch conversion, check that it makes sense.
c
	if(dofk45z.or.dofk54z)then
	  if(ira.eq.0.or.idec.eq.0)
     *	    call bug('f','Missing RA or DEC in epoch conversion')
	  if(dofk45z)call coGetd(coObj1,'obstime',obstime)
	  if(dofk54z)call coGetd(coObj2,'obstime',obstime)
	  if(obstime.eq.0)obstime = epo2jul(1950.d0,'B')
	endif
c
	end
c************************************************************************
	subroutine pcvt(x1,x2,n)
c
	implicit none
	integer n
	double precision x1(n),x2(n)
c
c  Perform a coordinate system conversion.
c------------------------------------------------------------------------
	include 'pcvt.h'
	include 'maxnax.h'
	double precision xa(MAXNAX),ra2000,dec2000,ra1950,dec1950
	double precision dra,ddec
c
	if(n.ne.3)call bug('f','Can only handle converting with n=3')
	call coCvt(coObj1,'ap/ap/ap',x1,'aw/aw/aw',xa)
c
	if(dofk45z)then
	  call fk45z(xa(ira),xa(idec),obstime,ra2000,dec2000)
	  xa(ira) = ra2000
	  xa(idec) = dec2000
	else if(dofk54z)then
	  call fk54z(xa(ira),xa(idec),obstime,ra1950,dec1950,dra,ddec)
	  xa(ira) = ra1950
	  xa(idec) = dec1950
	endif
c
	call coCvt(coObj2,'aw/aw/aw',xa,'ap/ap/ap',x2)
	end
