c***********************************************************************
c* Title -  Write title in standard format into LogFile.
c& mchw
c: image analysis,log-file
c+
      subroutine Title(lIn,naxis,blc,trc,cbof)

      integer lIn,naxis,blc(naxis),trc(naxis)
c  ---------------------------------------------------------------------
c  Write title in standard format into LogFile.
c
c  Inputs:
c    lIn        The handle of the Image.
c    naxis      Number of axes of the Image.
c    blc,trc    Corners of region of interest.
c  Output:
c    cbof       Beam oversampling factor.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision RTS
      parameter (RTS=3600d0*180d0/PI)

      logical   more
      integer   lblc, ltrc
      real      bmaj, bmin, cbof, DperJy, freq, omega
      character bunit*20, ctype1*9, ctype2*9, ctype3*9, line*80,
     *          txtblc*20, txttrc*20

      external  len1
      integer   len1
c-----------------------------------------------------------------------
c     Get beam size etc.
      call GetBeam(lIn,naxis,bunit,bmaj,bmin,omega,cbof)
      call rdhdr(lIn,'restfreq',freq,0.0)
      if (freq.eq.0.0) then
        call rdhda(lIn,'ctype3',ctype3,' ')
        if (ctype3(1:4).eq.'FREQ') then
          call rdhdr(lIn,'crval3',freq,0.0)
        endif
      endif

      DperJy=1.0
      if (freq.ne.0.0) then
        DperJy= (0.3/freq)**2 /(2.0*1.38e3*omega)
      endif

      write(line,'(a,a,a,f10.6,a,f15.4)')
     *      '  bunit: ', bunit(1:len1(bunit)),
     *      '  frequency: ', freq,
     *      '    K/Jy: ', DperJy
      call LogWrite(line,more)

      call rdhda(lIn,'ctype1',ctype1,' ')
      call rdhda(lIn,'ctype2',ctype2,' ')
      write(line,'(a,a,a,a,a,f6.2,a,f6.2,a)')
     *  '  Axes: ',ctype1,' x ',ctype2,
     *  '   Beam: ',bmaj*RTS,' x ',bmin*RTS,' arcsecs'
      call LogWrite(line,more)

      call mitoaf(blc,3,txtblc,lblc)
      call mitoaf(trc,3,txttrc,ltrc)
      line = '  Bounding region is Blc=('//txtblc(1:lblc)//
     *                          '),Trc=('//txttrc(1:ltrc)//')'
      call LogWrite(line,more)

      end
