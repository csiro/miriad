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
c-------------------------------------------------------------------c
	double precision pi,rts
	parameter(pi=3.141592654,rts=3600.d0*180.d0/pi)
	integer length,lblc,ltrc
	character line*80
	character*9 bunit,ctype1,ctype2,ctype3
	character*20 txtblc,txttrc
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
	write(line,'(a,a,a,f10.6,a,f15.4)') '  bunit: ',bunit,
     *		'  frequency: ',freq,'    K/Jy: ',DperJy
	length=9+10+12+10+10+15
	call LogWrite(line(1:length),more)
c
	call rdhda(lIn,'ctype1',ctype1,' ')
	call rdhda(lIn,'ctype2',ctype2,' ')
	write(line,'(a,a,a,a,a,f6.2,a,f6.2,a)')
     *	  '  Axes: ',ctype1,' x ',ctype2,
     *    '   Beam: ',bmaj*rts,' x ',bmin*rts,' arcsecs'
	length = 8 + len(ctype1) + 3 + len(ctype2) + 9+6+3+6+8
	call LogWrite(line(1:length),more)
	call mitoaf(blc,3,txtblc,lblc)
	call mitoaf(trc,3,txttrc,ltrc)
	line = '  Bounding region is Blc=('//txtblc(1:lblc)//
     *				       '),Trc=('//txttrc(1:ltrc)//')'
	length = 26 + lblc + 7 + ltrc + 1
	call LogWrite(line(1:length),more)
	end
c********1*********2*********3*********4*********5*********6*********7*c

