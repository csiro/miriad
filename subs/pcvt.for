c***********************************************************************
c
c  Convert between pixel coordinates of two coordinate systems.
c
c  History:
c    21jul97 rjs  Stripped out of regrid.
c    22jul97 rjs  Support galactic/equatorial and epoch conversion
c    23jul97 rjs  Correct order of doing epoch/coordinate conversion.
c    31may06 rjs  Adapt to use coCvtv (validate coordinate).
c***********************************************************************
      subroutine PcvtInit(coObj1d,coObj2d)

      integer coObj1d,coObj2d

c  Initialise the coordinate system conversion routines.
c-----------------------------------------------------------------------
      include 'pcvt.h'
      double precision dtemp,crval1,crpix1,cdelt1,crval2,crpix2,cdelt2
      double precision epoch1,epoch2
      character ctype1*16,ctype2*16
      integer i,l1,l2

c     Externals.
      integer len1
      double precision epo2jul
c-----------------------------------------------------------------------
      coObj1 = coObj1d
      coObj2 = coObj2d

      call coGetd(coObj1,'naxis',dtemp)
      naxis = nint(dtemp)
      call coGetd(coObj2,'naxis',dtemp)
      if(naxis.ne.nint(dtemp))call bug('f','Differing number of axes')

      dofk45z = .false.
      dofk54z = .false.
      galeq = 0
      ira = 0
      idec = 0

      do i=1,naxis
        call coAxGet(coObj1,i,ctype1,crpix1,crval1,cdelt1)
        call coAxGet(coObj2,i,ctype2,crpix2,crval2,cdelt2)
c
c  Velocity and frequency axes.
c
        if(ctype2(1:4).eq.'VELO'.or.ctype2(1:4).eq.'FELO')then
          call coVelSet(coObj1,ctype2)
        else if(ctype2(1:4).eq.'FREQ'.and.
     *          ctype1(1:4).ne.'FREQ')then
          call coVelSet(coObj1,'FREQ')
c
c  RA axes.
c
        else if((ctype1(1:4).eq.'RA--'.or.ctype1(1:4).eq.'GLON').and.
     *          (ctype2(1:4).eq.'RA--'.or.ctype2(1:4).eq.'GLON'))then
          ira = i
          if(ctype1(1:4).eq.'RA--'.and.
     *        ctype1(1:4).ne.ctype2(1:4))then
            galeq = -1
          else if(ctype1(1:4).eq.'GLON'.and.
     *        ctype1(1:4).ne.ctype2(1:4))then
            galeq = 1
          endif
c
c  DEC axes.
c
        else if((ctype1(1:4).eq.'DEC-'.or.ctype1(1:4).eq.'GLAT').and.
     *          (ctype2(1:4).eq.'DEC-'.or.ctype2(1:4).eq.'GLAT'))then
          idec = i
          if(ctype1(1:4).eq.'DEC-'.and.
     *        ctype1(1:4).ne.ctype2(1:4))then
            galeq = -1
          else if(ctype1(1:4).eq.'GLAT'.and.
     *        ctype1(1:4).ne.ctype2(1:4))then
            galeq = 1
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
     *        ctype1(1:l1)//' and '//ctype2(1:l2))
            call bug('f','Impossible or unimplemented conversion')
          endif
        endif
      enddo
c
c  Determine whether equinox conversion has to be done.
c
      if(ira.ne.0.and.idec.ne.0)then
        call coGetd(coObj1,'epoch',epoch1)
        if(epoch1.lt.1800)epoch1 = 1950
        call coGetd(coObj2,'epoch',epoch2)
        if(epoch2.lt.1800)epoch2 = 1950

        if(abs(epoch1-epoch2).gt.0.1)then
          if(abs(epoch1-1950).le.0.1.and.
     *       abs(epoch2-2000).le.0.1)then
            dofk45z = .true.
          else if(abs(epoch1-2000).le.0.1.and.
     *       abs(epoch2-1950).le.0.1)then
            dofk54z = .true.
          else
            call bug('f','Unsupported epoch conversion requested')
          endif
        endif
      endif
c
c  If we are to do epoch conversion, check that it makes sense.
c
      if(dofk45z.or.dofk54z)then
        if(ira.eq.0.or.idec.eq.0)
     *    call bug('f','Missing RA or DEC in epoch conversion')
        if(dofk45z)call coGetd(coObj1,'obstime',obstime)
        if(dofk54z)call coGetd(coObj2,'obstime',obstime)
        if(obstime.eq.0)obstime = epo2jul(1950.d0,'B')
      endif

      end
c***********************************************************************
      subroutine pcvt(x1,x2,n,valid)

      integer n
      double precision x1(n),x2(n)
      logical valid

c  Perform a coordinate system conversion.
c-----------------------------------------------------------------------
      include 'pcvt.h'
      include 'maxnax.h'
      double precision xa(MAXNAX),ra2000,dec2000,ra1950,dec1950
      double precision dra,ddec
c-----------------------------------------------------------------------
      if(n.ne.3)call bug('f','Can only handle converting with n=3')
      call coCvtv(coObj1,'ap/ap/ap',x1,'aw/aw/aw',xa,valid)
      if(.not.valid)return

      if(dofk54z)then
        call fk54z(xa(ira),xa(idec),obstime,ra1950,dec1950,dra,ddec)
        xa(ira) = ra1950
        xa(idec) = dec1950
      endif

      if(galeq.lt.0)call dsfetra(xa(ira),xa(idec),.false.,-galeq)
      if(galeq.gt.0)call dsfetra(xa(ira),xa(idec),.true.,  galeq)
      if(dofk45z)then
        call fk45z(xa(ira),xa(idec),obstime,ra2000,dec2000)
        xa(ira) = ra2000
        xa(idec) = dec2000
      endif

      call coCvtv(coObj2,'aw/aw/aw',xa,'ap/ap/ap',x2,valid)

      end
