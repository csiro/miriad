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
      subroutine pCvtInit(coObj1d,coObj2d)

      integer coObj1d,coObj2d

c  Initialise the coordinate system conversion routines.
c-----------------------------------------------------------------------
      include 'pcvt.h'

      integer   i, j, k
      double precision cdelt1, cdelt2, crpix1, crpix2, crval1, crval2,
     *          dtemp, eqnox1, eqnox2
      character ctype1*16, ctype2*16, type1*4, type2*4

c     Externals.
      integer len1
      double precision epo2jul
c-----------------------------------------------------------------------
      coObj1 = coObj1d
      coObj2 = coObj2d

      call coGetD(coObj1,'naxis',dtemp)
      naxis = nint(dtemp)
      call coGetD(coObj2,'naxis',dtemp)
      if (naxis.ne.nint(dtemp)) call bug('f','Differing number of axes')

      ilng  = 0
      ilat  = 0
      galeq = 0
      dofk45z = .false.
      dofk54z = .false.

      do i = 1, naxis
        call coAxGet(coObj1,i,ctype1,crpix1,crval1,cdelt1)
        call coAxGet(coObj2,i,ctype2,crpix2,crval2,cdelt2)

c       Get the coordinate type for non-linear axes.
        type1 = ctype1(1:4)
        type2 = ctype2(1:4)

        if (type2.eq.'VELO' .or. type2.eq.'FELO') then
c         Velocity axes.
          call coVelSet(coObj1,ctype2)

        else if (type2.eq.'FREQ' .and. type1.ne.'FREQ') then
c         Frequency axes.
          call coVelSet(coObj1,'FREQ')

        else if ((type1.eq.'RA--' .or. type1.eq.'GLON') .and.
     *           (type2.eq.'RA--' .or. type2.eq.'GLON')) then
c         RA/GLON axes.
          ilng = i
          if (type1.ne.type2) then
            if (type1.eq.'RA--') then
              galeq = -1
            else if (type1.eq.'GLON') then
              galeq = 1
            endif
          endif

        else if ((type1.eq.'DEC-' .or. type1.eq.'GLAT') .and.
     *           (type2.eq.'DEC-' .or. type2.eq.'GLAT')) then
c         DEC/GLAT axes.
          ilat = i
     *    if (type1.ne.type2) then
            if (type1.eq.'DEC-') then
              galeq = -1
            else if (type1.eq.'GLAT') then
              galeq = 1
            endif
          endif

        else
c         All other conversions.
          j = index(ctype1,'-') - 1
          if (j.le.0) j = len(ctype1)

          k = index(ctype2,'-') - 1
          if (k.le.0) k = len(ctype2)

          if (ctype1(1:j).ne.ctype2(1:k)) then
            j = len1(ctype1)
            k = len1(ctype2)
            call bug('w','Error converting between axis types ' //
     *        ctype1(1:j) // ' and ' // ctype2(1:k))
            call bug('f','Impossible or unimplemented conversion')
          endif
        endif
      enddo

c     Is precession needed?
      if (ilng.ne.0 .and. ilat.ne.0) then
        call coGetD(coObj1,'epoch',eqnox1)
        if (eqnox1.lt.1800d0) eqnox1 = 1950
        call coGetD(coObj2,'epoch',eqnox2)
        if (eqnox2.lt.1800d0) eqnox2 = 1950

        if (abs(eqnox1-eqnox2).gt.0.1d0) then
          if (abs(eqnox1-1950d0).le.0.1d0 .and.
     *        abs(eqnox2-2000d0).le.0.1d0) then
            dofk45z = .true.
          else if (abs(eqnox1-2000d0).le.0.1d0 .and.
     *             abs(eqnox2-1950d0).le.0.1d0) then
            dofk54z = .true.
          else
            call bug('f','Unsupported epoch conversion requested')
          endif
        endif

c       Get the epoch for equatorial conversion.
        if (dofk45z .or. dofk54z) then
          if (dofk45z) call coGetD(coObj1,'obstime',obstime)
          if (dofk54z) call coGetD(coObj2,'obstime',obstime)
          if (obstime.eq.0d0) then
            obstime = epo2jul(1950d0,'B')
          endif
        endif
      endif

      end
c***********************************************************************
      subroutine pCvt(x1,x2,n,valid)

      integer n
      double precision x1(n),x2(n)
      logical valid

c  Perform a coordinate system conversion.
c-----------------------------------------------------------------------
      include 'pcvt.h'
      include 'maxnax.h'

      double precision ddec, dec1950, dec2000, dra, ra1950, ra2000,
     *          xa(MAXNAX)
c-----------------------------------------------------------------------
      if (n.ne.3) call bug('f','Can only handle converting with n=3')

      call coCvtv(coObj1,'ap/ap/ap',x1,'aw/aw/aw',xa,valid)
      if (.not.valid) return

      if (dofk54z) then
        call fk54z(xa(ilng),xa(ilat),obstime,ra1950,dec1950,dra,ddec)
        xa(ilng) = ra1950
        xa(ilat) = dec1950
      endif

      if (galeq.lt.0) then
        call dsfetra(xa(ilng),xa(ilat),.false.,-galeq)
      else if (galeq.gt.0) then
        call dsfetra(xa(ilng),xa(ilat),.true.,  galeq)
      endif

      if (dofk45z) then
        call fk45z(xa(ilng),xa(ilat),obstime,ra2000,dec2000)
        xa(ilng) = ra2000
        xa(ilat) = dec2000
      endif

      call coCvtv(coObj2,'aw/aw/aw',xa,'ap/ap/ap',x2,valid)

      end
