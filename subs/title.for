c********1*********2*********3*********4*********5*********6*********7*c
c* Title -  Write title in standard format into LogFile.
c& mchw
c: image analysis,log-file
c+
	subroutine Title(lIn,naxis,blc,trc,cbof)
c
	implicit none
	integer lIn,naxis,blc(naxis),trc(naxis)
c
c   Write title in standard format into LogFile.
c
c  Inputs:
c    lIn	The handle of the Image.
c    naxis	Number of axes of the Image.
c    blc,trc	Corners of region of interest.
c  Output:
c    cbof	Beam oversampling factor.
c--
c			mchw june 1990
c  mchw 29oct96  Try harder to get frequency from image header.
c  pjt  21oct03  more space for bunit.... (e.g. JY/BEAM.KM/S)
c                also called logwrite(line,more), not length
c                and fixed precision of PI
c-------------------------------------------------------------------c
	include 'mirconst.h'
	double precision RTS
	parameter(RTS=3600.d0*180.d0/PI)
	integer lblc,ltrc,len1
	character line*80
	character*9 ctype1,ctype2,ctype3
	character*20 txtblc,txttrc,bunit
	real bmaj,bmin,freq,omega,cbof,DperJy
	logical more
c
c  Get beam size etc.
c
	call GetBeam(lIn,naxis,bunit,bmaj,bmin,omega,cbof)
	call rdhdr(lIn,'restfreq',freq,0.)
	if(freq.eq.0.)then
	  call rdhda(lIn,'ctype3',ctype3,' ')
          if(ctype3(1:4).eq.'FREQ')then
	    call rdhdr(lIn,'crval3',freq,0.)
	  endif
	endif
	DperJy=1.
	if(freq.ne.0.) then
	  DperJy= (0.3/freq)**2 /(2.*1.38e3*omega)
	endif
c
	write(line,'(a,a,a,f10.6,a,f15.4)') '  bunit: ',
     *          bunit(1:len1(bunit)),
     *		'  frequency: ',freq,'    K/Jy: ',DperJy
	call LogWrite(line,more)
c
	call rdhda(lIn,'ctype1',ctype1,' ')
	call rdhda(lIn,'ctype2',ctype2,' ')
	write(line,'(a,a,a,a,a,f6.2,a,f6.2,a)')
     *	  '  Axes: ',ctype1,' x ',ctype2,
     *    '   Beam: ',bmaj*RTS,' x ',bmin*RTS,' arcsecs'
	call LogWrite(line,more)
	call mitoaf(blc,3,txtblc,lblc)
	call mitoaf(trc,3,txttrc,ltrc)
	line = '  Bounding region is Blc=('//txtblc(1:lblc)//
     *				       '),Trc=('//txttrc(1:ltrc)//')'
	call LogWrite(line,more)
	end
c********1*********2*********3*********4*********5*********6*********7*c

