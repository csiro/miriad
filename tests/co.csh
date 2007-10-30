#!/bin/csh -ef

cat <<"EOF" > co_test.for
	program cotest
	implicit none
c
	include 'mirconst.h'
	integer SIN,TAN,NCP,ARC
	parameter(SIN=2,TAN=3,ARC=4,NCP=5)
c
	character proj*8,in*64,ras*13,decs*13
	integer nsize(2),lu,ierr,itype
	double precision x1(3),x2(3),L,M,x,y,ra,dec,t
	double precision crval1,crpix1,cdelt1,crval2,crpix2,cdelt2
	logical verb
c
c  Externals
c
	double precision rabs
	character hangle*16,rangle*16
c
	call keyini
	call keya('in',in,'xy')
	call keya('proj',proj,'ncp')
	call keyd('x1',x,0.d0)
	call keyd('x1',y,0.d0)
	call options('options','verbose',verb,1)
	call keyfin
c
	call xyopen(lu,in,'old',3,nsize)
c
c  Do the AIPS conversion.
c
	call rdhdd(lu,'crval1',crval1,0.d0)
	call rdhdd(lu,'crpix1',crpix1,0.d0)
	call rdhdd(lu,'cdelt1',cdelt1,0.d0)
	call rdhdd(lu,'crval2',crval2,0.d0)
	call rdhdd(lu,'crpix2',crpix2,0.d0)
	call rdhdd(lu,'cdelt2',cdelt2,0.d0)
c
	L = cdelt1 * (x - crpix1)
	M = cdelt2 * (y - crpix2)
c
	if(proj.eq.'ncp')then
	  itype = NCP
	else if(proj.eq.'sin')then
	  itype = SIN
	else if(proj.eq.'tan')then
	  itype = TAN
	else if(proj.eq.'arc')then
	  itype = ARC
	endif
	call newpos(itype,crval1,crval2,L,M,ra,dec,ierr)
	if(ierr.ne.0)call bug('f','NEWPOS failed')
c
	call coinit(lu)
	call coPrjSet(lu,proj)
c
	x1(3) = 1
c
	x1(1) = x
	x1(2) = y
	call coCvt(lu,'ap/ap/ap',x1,'ow/ow/ow',x2)
	call coCvt(lu,'ap/ap/ap',x1,'w/w/ap',x2)
	if(verb)write(*,10)x1(1)-x,x1(2)-y,x2(1)-ra,x2(2)-dec,'  pp->ww'
	t = max(abs(x1(1)-x),abs(x1(2)-y),
     *		rabs(x2(1)-ra),abs(x2(2)-dec))
c
	x1(1) = ra
	x1(2) = dec
	call coCvt(lu,'w/w/ap',x1,'ap/ap/ap',x2)
	if(verb)write(*,10)x2(1)-x,x2(2)-y,x1(1)-ra,x1(2)-dec,'  ww->pp'
	t = max(t,abs(x2(1)-x),abs(x2(2)-y),
     *		  rabs(x1(1)-ra),abs(x1(2)-dec))
c
	x1(1) = x
	x1(2) = dec
	call coCvt(lu,'p/w/p',x1,'w/p/p',x2)
	if(verb)write(*,10)x1(1)-x,x2(2)-y,x2(1)-ra,x1(2)-dec,'  pw->wp'
	t = max(t,abs(x1(1)-x),abs(x2(2)-y),
     *	          rabs(x2(1)-ra),abs(x1(2)-dec))
c
	x1(1) = ra
	x1(2) = y
	call coCvt(lu,'w/p/p',x1,'p/w/p',x2)
	if(verb)write(*,10)x2(1)-x,x1(2)-y,x1(1)-ra,x2(2)-dec,'  wp->pw'
	t = max(t,abs(x2(1)-x),abs(x1(2)-y),
     *		  rabs(x1(1)-ra),abs(x2(2)-dec))
c
	if(verb)call output('----------------')
	ras = hangle(ra)
	decs = rangle(dec)
	write(*,11)x,y,ras,decs,'  '//proj,t
	if(verb)call output('-----------------')
	call cofin(lu)

  10	format(1x,1p4d15.8,a,d15.8)
  11	format(1x,1p2d11.3,1x,a,a,a,d11.4)
	end
c************************************************************************
	double precision function rabs(x)
c
	implicit none
	double precision x
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision t
	t = x
	dowhile(t.gt.DPI)
	  t = t - 2*DPI
	enddo
	dowhile(t.lt.-DPI)
	  t = t + 2*DPI
	enddo
	rabs = abs(t)
	end
      SUBROUTINE DIRCOS (TYPE, RA0, DEC0, RA, DEC, L, M, IERR)
C-----------------------------------------------------------------------
C! determines direction cosines between ref position and test position
C# Coordinates
C   This software is the subject of a User agreement and is confidential
C   in nature. It shall not be sold or otherwise made available or
C   disclosed to third parties.
C-----------------------------------------------------------------------
C   DIRCOS determines the direction cosines (L,M) corresponding to
C   the separation from (RA0,DEC0), the reference pixel, to
C   (RA,DEC).  The direction cosine L is assumed to be positive to the
C   east; M is positive to the north.  Several projective geometries
C   are supported.
C   Inputs:
C      TYPE   I    Type of projection: 2 => SIN, 3 => TAN, 4 => Arc,
C                  5 => WSRT (rel north pole), 6 Global sinusoidal,
C                  7 Mercator, 8 Aitoff
C      RA0    D    Coordinate reference right ascension/longitude
C      DEC0   D    Coordinate reference declination/latitude
C      RA     D    right ascension/longitude of point
C      DEC    D    declination/latitude of point
C   Outputs:
C      L      D    Cosine angle of displacement to east
C      M      D    Cosine angle of displacement to north
C      IERR   I    0 ok, 1 out of range, 2 bad type, 3 undefined
C   ALL ANGLES ARE IN RADIANS.
C-----------------------------------------------------------------------
      INTEGER   IERR, TYPE
      DOUBLE PRECISION    RA0, DEC0, RA, DEC, L, M
C
      DOUBLE PRECISION    DEPS, TWOPI, COSS, SINS, DT, DA, DD, SINT
	character msgtxt*80
      DATA DEPS, TWOPI /1.0D-5, 6.28318530717959D0/
C-----------------------------------------------------------------------
C                                       Use full accuracy
 10   IERR = 2
      IF ((TYPE.LT.2) .OR. (TYPE.GT.9)) GO TO 999
      IERR = 0
      COSS = COS (DEC)
      SINS = SIN (DEC)
      L = SIN(RA-RA0) * COSS
      SINT = SINS * SIN(DEC0) + COSS * COS(DEC0) * COS(RA-RA0)
      GO TO (20, 20, 30, 40, 50, 60, 70, 80, 90), TYPE
C                                       SIN projection
 20   CONTINUE
         IF (SINT.LT.0.0D0) IERR = 1
         M = SINS * COS(DEC0) - COSS * SIN(DEC0) * COS(RA-RA0)
         GO TO 999
C                                       TAN projection
 30   CONTINUE
         IF (SINT.LE.0.0D0) THEN
            IERR = 1
         ELSE
            M = SINS * SIN(DEC0) + COSS * COS(DEC0) * COS(RA-RA0)
            L = L / M
            M = (SINS * COS(DEC0) - COSS * SIN(DEC0) * COS(RA-RA0)) / M
            END IF
         GO TO 999
C                                       Arc projection
 40   CONTINUE
         M = SINS * SIN(DEC0) + COSS * COS(DEC0) * COS(RA-RA0)
         M =  MIN (1.0D0, MAX (-1.0D0, M))
         M = ACOS (M)
         IF (M.NE.0.0D0) THEN
            M = M / SIN(M)
         ELSE
            M = 1.0D0
            END IF
         L = L * M
         M = (SINS * COS(DEC0) - COSS * SIN(DEC0) * COS(RA-RA0)) * M
         GO TO 999
C                                       WSRT projection
 50   CONTINUE
         IF (DEC0.NE.0.0D0) GO TO 55
            WRITE (MSGTXT,1050)
	call output(msgtxt)
            L = 0.0D0
            M = 0.0D0
            GO TO 990
 55      M = (COS(DEC0) - COSS * COS(RA-RA0)) / SIN(DEC0)
         GO TO 999
C                                       Global sinusoidal
 60   CONTINUE
         DT = RA - RA0
         IF (ABS(DT).GT.TWOPI/2.0D0) GO TO 990
         IF (ABS(DEC).GT.TWOPI/4.0D0) GO TO 990
         IF (ABS(DEC0).GT.TWOPI/4.0D0) GO TO 990
         M = DEC - DEC0
         L = DT * COSS
         GO TO 999
C                                       Mercator
 70   CONTINUE
         GO TO 999
C                                       Aitoff
 80   CONTINUE
         GO TO 999
C                                       Stereographic
 90   CONTINUE
         DA = RA - RA0
         IF (ABS(DA).GT.TWOPI/2.0D0) GO TO 990
         IF (ABS(DEC).GT.TWOPI/4.0D0) GO TO 990
         DD = 1.0D0 + SINS * SIN(DEC0) + COSS * COS(DEC0) * COS(DA)
         IF (ABS(DD).LT.DEPS) GO TO 990
         DD = 2.0D0 / DD
         L = L * DD
         M = DD * (SINS * COS(DEC0) - COSS * SIN(DEC0) * COS(DA))
         GO TO 999
C                                       Undefined
 990  IERR = 3
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('DIRCOS: WSRT COORDINATES CANNOT HAVE DEC0 = 0')
      END
      SUBROUTINE DIRRA (TYPE, DX, DEC, RA0, DEC0, ROTA, DY, RA, IERR)
C-----------------------------------------------------------------------
C! finds latitude pixel and longitude given longitude pixel and latitude
C# Coordinates
C   This software is the subject of a User agreement and is confidential
C   in nature. It shall not be sold or otherwise made available or
C   disclosed to third parties.
C-----------------------------------------------------------------------
C   DIRRA will find the longitude and the latitude pixel position of a
C   point given the latitude and the longitude pixel of the point.
C   For use with four projective geometries.
C   ALL ANGLES ARE IN RADIANS.
C   Inputs:
C      TYPE  I      2 => SIN, 3 => TAN, 4 => Arc, 5 => WSRT geometry
C                   6 Global sinusoidal, 7 Mercator, 8 Aitoff
C      DX    D      X pixel value from ref pixel
C      DEC   D      declination/ latitude
C      RA0   D      RA / longitude of ref pixel
C      DEC0  D      DEC / latitude of ref pixel
C      ROTA   D      Rotation of coordinates
C   Output:
C      DY    D      Y pixel position
C      RA    D      right ascension/ longitude
C      IERR  D      0 ok, 1 out of range, 2 bad type, 3 undefined answer
C-----------------------------------------------------------------------
      INTEGER   IERR, TYPE
      DOUBLE PRECISION    DX, DEC, RA0, DEC0, ROTA, DY, RA
C
      INTEGER   NC, NCLIM
      DOUBLE PRECISION    COSR, SINR, DA, DB, DC, COS0, SIN0, COSD,
     *   SIND, TWOPI, DEPS, DT, FG, DFDA, DSTEP
      LOGICAL   DOIT
	character msgtxt*80
      DATA TWOPI /6.28318530717959D0/
      DATA NCLIM, DEPS /1000, 1.0D-5/
C-----------------------------------------------------------------------
      DY = 0.0D0
      RA = 0.0D0
      IERR = 2
      IF ((TYPE.LT.2) .OR. (TYPE.GT.9)) GO TO 999
      IERR = 0
      COSR = COS(ROTA)
      SINR = SIN(ROTA)
      COSD = COS(DEC)
      SIND = SIN(DEC)
      COS0 = COS(DEC0)
      SIN0 = SIN(DEC0)
      NC = 0
      IF (ABS(COSD).LT.DEPS) GO TO 990
      GO TO (20, 20, 30, 40, 50, 60, 70, 80, 90), TYPE
C                                       SIN projection
 20   CONTINUE
         DA = COSR
         DB = SINR * SIN0
         DT = SQRT (DA*DA + DB*DB)
         IF (DT.LT.DEPS) GO TO 990
         DT = (DX - SINR*SIND*COS0) / DT / COSD
         IF (ABS(DT).GT.1.0D0) GO TO 990
         RA = ASIN (DT) +  ATAN2 (DB, DA)  +  RA0
         DY = SIND * COS0 * COSR  -  COSD * (COSR * SIN0 * COS(RA-RA0)
     *      + SIN(RA-RA0) * SINR)
         GO TO 900
C                                       TAN projection
 30   CONTINUE
         DA = COSR
         DB = SINR * SIN0 + DX * COS0
         DT = SQRT (DA*DA + DB*DB)
         IF (DT.LT.DEPS) GO TO 990
         DT = (DX*SIN0 - COS0*SINR) / DT * SIND / COSD
         IF (ABS(DT).GT.1.0D0) GO TO 990
         RA = ASIN (DT)  + ATAN2 (DB, DA) + RA0
         DT = SIND * SIN0 + COSD * COS0 * COS(RA-RA0)
         IF (ABS(DT).LT.DEPS) GO TO 990
         DY = (SIND*COS0*COSR - COSD*SIN0*COS(RA-RA0)*COSR -
     *      COSD*SINR*SIN(RA-RA0)) / DT
         GO TO 900
C                                       Arc projection
 40   CONTINUE
         IF (ABS(COSR).LT.DEPS) GO TO 990
         IF (ABS(COS0).GT.DEPS) GO TO 42
            IF (ABS(SIN0).LT.DEPS) GO TO 990
            DY = ACOS (SIND / SIN0)
            DY = DY*DY - DX*DX
            IF (DY.LT.0.0D0) GO TO 990
            DY = -SQRT (DY)
            DA = SQRT (DX*DX + DY*DY)
            DB = 1.0D0
            IF (DA.NE.0.0D0) DB = SIN(DA) / DA
            GO TO 47
 42      DY = 0.0D0
         NC = 0
 45      DA = SQRT (DX*DX + DY*DY)
            NC = NC + 1
            DC = DY
            DB = 1.0D0
            IF (DA.NE.0.0D0) DB = SIN(DA) / DA
            DA = COS(DA)
            DY = (SIND - SIN0*DA - DX*SINR*COS0*DB) / (COSR * COS0 *
     *         DB)
            DA = ABS (DY - DC)
	write(*,*)dy
            IF ((DA.LT.1.0D-9) .OR. (NC.GT.NCLIM)) GO TO 47
               IF (NC.GT.10) DY = (DY + DC) / 2.0D0
               GO TO 45
 47      DT = DB * (DX*COSR - DY*SINR) / COSD
         IF (ABS(DT).GT.1.0D0) GO TO 990
         RA = RA0 + ASIN (DT)
         GO TO 900
C                                       WSRT projection
 50   CONTINUE
         IF (ABS(SIN0).LT.DEPS) GO TO 990
         DA = COSR
         DB = SINR / SIN0
         DT = (DX - COS0*DB) / SQRT (DA*DA + DB*DB) / COSD
         IF (ABS(DT).GT.1.0D0) GO TO 990
         RA = ASIN (DT) + ATAN2 (DB, DA) + RA0
         DY = (COS0 - COSD*COS(RA-RA0)) * COSR / SIN0  -
     *      COSD * SINR * SIN(RA-RA0)
         GO TO 900
C                                       Global sinusoidal
 60   CONTINUE
         IF (ABS(COSR).LT.DEPS) GO TO 990
         DY = (DEC - DEC0 - DX * SINR) / COSR
         RA = RA0 + (DX * COSR - DY * SINR) / COSD
         IF (ABS(RA-RA0).GT.TWOPI/2.0D0) GO TO 990
         GO TO 900
C                                       Mercator
 70   CONTINUE
c         IF (ABS(COSR).LT.DEPS) GO TO 990
c         DT = TAN (DEC/2.0D0+TWOPI/8.0D0)
c         IF (DT.LT.DEPS) GO TO 990
c         DT = GEOMD2(LOCNUM) * LOG (DT) - GEOMD3(LOCNUM)
c         DA = (DX - DT * SINR) / COSR
c         DY = DT * COSR - DA * SINR
c         RA = RA0 + DA / GEOMD1(LOCNUM)
         GO TO 900
C                                       Aitoff
 80   CONTINUE
         GO TO 900
C                                       Stereographic
 90   CONTINUE
         DA = 2.0D0 * COSD * COSR
         DB = 2.0D0 * COSD * SIN0 * SINR + DX * COSD * COS0
         DT = SQRT (DA*DA + DB*DB)
         IF (DT.LT.DEPS) GO TO 990
         DT = (DX + DX*SIND*SIN0 - 2.0D0*SIND*COS0*SINR) / DT
         IF (ABS(DT).GT.1.0D0) GO TO 990
         RA = ASIN (DT) + ATAN2 (DB, DA)
         DY = 1.0D0 + SIND*SIN0 + COSD*COS0*COS(RA)
         IF (ABS(DY).LE.DEPS) GO TO 990
         DY = 2.0D0 * ((SIND*COS0 - COSD*SIN0*COS(RA)) * COSR -
     *      COSD*SIN(RA) * SINR) / DY
         RA = RA + RA0
         GO TO 900
C                                       RA in range:
 900  IF (RA-RA0.LT.-TWOPI/2.0D0) RA = RA + TWOPI
      IF (RA-RA0.GT.TWOPI/2.0D0) RA = RA - TWOPI
      IF (NC.LE.NCLIM/2) GO TO 999
         WRITE (MSGTXT,1900) NC
         IF (NC.GT.NCLIM) WRITE (MSGTXT,1901) NC
	call output(msgtxt)
         IF (NC.GT.NCLIM) IERR = 3
         GO TO 999
C                                       Undefined
 990  IERR = 3
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('DIRRA: ITERATIONS REQUIRED = ',I5)
 1901 FORMAT ('DIRRA: FAILED TO CONVERGE AFTER',I5,' ITERATIONS')
      END
      SUBROUTINE DIRDEC (TYPE, DY, RA, RA0, DEC0, ROTA, DX, DEC, IERR)
C-----------------------------------------------------------------------
C! finds longitude pixel and latitude given latitude pixel and longitude
C# Coordinates
C   This software is the subject of a User agreement and is confidential
C   in nature. It shall not be sold or otherwise made available or
C   disclosed to third parties.
C-----------------------------------------------------------------------
C   DIRDEC will find the latitude and longitude-like pixel position
C   of a point given the longitude and the latitude-pixel of the point.
C   ALL ANGLES ARE IN RADIANS.
C   Inputs:
C      TYPE  I      2 => SIN, 3 => TAN, 4 => Arc, 5 => WSRT geometry
C                   6 Global sinusoidal, 7 Mercator, 8 Aitoff
C      DY    D      the Y pixel pos rel to ref pixel
C      RA    D      right ascension/longitude
C      RA0   D      RA/longitude of ref pixel
C      DEC0  D      DEC/latitude of ref pixel
C      ROT   D      Rotation of coords +dec into +ra
C   Output:
C      DEC   D      declination/latitude
C      DX    D      RA-like pixel position rel to ref pixel
C      IERR  I      0 ok, 1 out of range, 2 bad type, 3 undefined
C-----------------------------------------------------------------------
      DOUBLE PRECISION    DY, RA, RA0, DEC0, ROTA, DX, DEC
      INTEGER   IERR, TYPE
C
      INTEGER   NC, NCLIM
      DOUBLE PRECISION    COSR, SINR, DA, DB, DC, DD, COS0, SIN0, COSA,
     *   SINA, DEPS, DE, TWOPI, DT, TRA, FG, DFDD, DSTEP
      LOGICAL   DOIT
	character msgtxt*80
      DATA TWOPI, DEPS /6.28318530717959D0, 1.D-5/
      DATA NCLIM /1000/
C-----------------------------------------------------------------------
      DX = 0.0D0
      DEC = 0.0D0
      IERR = 2
      IF ((TYPE.LT.2) .OR. (TYPE.GT.9)) GO TO 999
      IERR = 0
      COSR = COS(ROTA)
      SINR = SIN(ROTA)
      COS0 = COS(DEC0)
      SIN0 = SIN(DEC0)
      COSA = COS(RA-RA0)
      SINA = SIN(RA-RA0)
      NC = 0
      GO TO (20, 20, 30, 40, 50, 60, 70, 80, 90), TYPE
C                                       SIN projection
 20   CONTINUE
         DA = COSR * COS0
         DB = COSR * SIN0 * COSA  +  SINR * SINA
         IF ((ABS(DA).LT.DEPS) .AND. (ABS(DB).LT.DEPS)) GO TO 990
         DT = DY / SQRT(DB*DB + DA*DA)
         IF (ABS(DT).GT.1.0D0) GO TO 990
         DEC = ASIN (DT) + ATAN2 (DB, DA)
         DX = SINR * SIN(DEC) * COS0  -  COS(DEC) *
     *      (SINR * SIN0 * COSA - COSR * SINA)
         GO TO 900
C                                       TAN projection
 30   CONTINUE
         DA = COS0 * COSR - DY * SIN0
         IF (ABS(DA).LT.DEPS) GO TO 990
         DB = (SIN0 * COSA * COSR + SINA * SINR + DY * COS0 * COSA) / DA
         DEC = ATAN (DB)
         DX = (SINA*COSR + DB*COS0*SINR - SIN0*COSA*SINR) /
     *      (DB * SIN0 + COS0 * COSA)
         GO TO 900
C                                       Arc projection
 40   CONTINUE
         IF (ABS(COSR).LT.DEPS) GO TO 990
         DD = ATAN2 (SIN0, COS0 * COSA)
         DC = SQRT (1.D0 - COS0*COS0*SINA*SINA)
         DX = (DY * SINR + COS0 * SINA) / COSR
         NC = 0
 45      DA = SQRT (DX*DX + DY*DY)
            NC = NC + 1
            DE = DX
            DB = 1.0D0
            IF (SIN(DA).NE.0.0D0) DB = DA / SIN(DA)
            DA = COS(DA) / DC
            DEC = DD
            IF (ABS(DA).LT.1.0D0) DEC = DEC + ACOS (DA)
            IF ((DY.LT.0.0D0) .AND. (DEC.GT.DEC0)) DEC = -DEC
     *         + 2.0D0 * DD
            DX = (DY * SINR + DB * COS(DEC) * SINA) / COSR
            IF ((ABS(DX-DE).LT.1.0D-9) .OR. (NC.GT.NCLIM)) GO TO 900
            IF (NC.GT.10) DX = (DX + DE) / 2.0D0
            IF ((NC.GT.25) .AND. ((NC/2)*2.EQ.NC)) DX = (DX+DE) / 2.0D0
            GO TO 45
C                                       WSRT projection
 50   CONTINUE
         IF (ABS(SIN0).LT.DEPS) GO TO 990
         DA = COSA * COSR + SINA * SIN0 * SINR
         IF (ABS(DA).LT.DEPS) GO TO 990
         DT = (COS0 * COSR - DY * SIN0) / DA
         IF (ABS(DT).GT.1.0D0) GO TO 990
         DEC = ACOS (DT)
         IF (DEC0.LT.0.0D0) DEC = -DEC
         DX = COS(DEC) * SINA * COSR + SINR * (COS0 - COS(DEC)*COSA)
     *      / SIN0
         GO TO 900
C                                       Global sinusoidal
 60   CONTINUE
         IF (ABS(COSR).LT.DEPS) GO TO 990
         TRA = RA - RA0
         IF (ABS(TRA).GT.TWOPI/2.0D0) GO TO 990
         NC = 0
         DEC = DEC0 + DY / COSR
         IF (ABS(SINR).LT.DEPS) GO TO 67
         DC = -100.0D0
 65      NC = NC + 1
            DSTEP = 0.05D0
            IF (NC.LT.4) DSTEP = 0.20D0 / NC
            FG = DY + TRA*SINR*COS(DEC) - (DEC-DEC0)*COSR
            DFDD = COSR + TRA*SINR*SIN(DEC)
            DOIT = (ABS(DFDD).GT.DEPS) .OR. ((ABS(DFDD).GT.DABS(FG))
     *         .AND. (DFDD.NE.0.0D0))
            IF ((.NOT.DOIT) .AND. (DC.LT.-50.0D0)) GO TO 990
            IF (.NOT.DOIT) DEC = (DEC + DC) / 2.0D0
            IF (DOIT) DC = DEC
            IF (DOIT) DEC = DEC + FG / DFDD
            IF (DEC-DC.GT.DSTEP) DEC = DC + DSTEP
            IF (DEC-DC.LT.-DSTEP) DEC = DC - DSTEP
            IF (NC.GT.NCLIM) GO TO 990
            IF (ABS(DEC-DC).GT.1.0D-9) GO TO 65
 67      DX = TRA * COS(DEC) * COSR + (DEC - DEC0) * SINR
         IF (ABS(DEC-DEC0).GT.TWOPI/2.0D0) GO TO 990
         IF (ABS(DEC).GT.TWOPI/4.0D0) GO TO 990
         GO TO 900
C                                       Mercator
 70   CONTINUE
         GO TO 900
C                                       Aitoff
 80   CONTINUE
         GO TO 900
C                                       Stereographic
 90   CONTINUE
         DA = 2.0D0 * COS0 * COSR - DY * SIN0
         DB = COSA * (2.0D0 * SIN0 * COSR + DY * COS0) +
     *      2.0D0 * SINA * SINR
         DT = SQRT (DA * DA + DB * DB)
         IF (DT.LT.DEPS) GO TO 990
         DT = DY / DT
         IF (ABS(DT).GT.1.0D0) GO TO 990
         DEC = ATAN2 (DB, DA) + ASIN (DT)
C                                       check
         DC = 1.0D0 + SIN(DEC)*SIN0 + COS(DEC)*COS0*COSA
         FG = -1.0D10
         IF (ABS(DC).GT.DEPS) FG = 2.0D0 * ((SIN(DEC)*COS0 -
     *      COS(DEC)*SIN0*COSA)*COSR - COS(DEC)*SINA*SINR) / DC
         IF (ABS(FG-DY).GT.DEPS) DEC = ATAN2 (DB, DA) - ASIN (DT)
     *      + TWOPI/2.0D0
         DX = 1.0D0 + SIN(DEC) * SIN0 +  COS(DEC) * COS0 * COSA
         IF (ABS(DX).LT.DEPS) GO TO 990
         FG = -1.0D10
         IF (ABS(DC).GT.DEPS) FG = 2.0D0 * ((SIN(DEC)*COS0 -
     *      COS(DEC)*SIN0*COSA)*COSR - COS(DEC)*SINA*SINR) / DX
         IF (ABS(FG-DY).GT.DEPS) GO TO 990
         DX = 2.0D0 * (COS(DEC) * SINA * COSR +
     *      (SIN(DEC)*COS0 - COS(DEC)*SIN0*COSA) * SINR) / DX
         GO TO 900
C                                       Check the loop
 900  IF (NC.LE.NCLIM/2) GO TO 999
         WRITE (MSGTXT,1900) NC
         IF (NC.GT.NCLIM) WRITE (MSGTXT,1901) NC
	call output(msgtxt)
         IF (NC.GT.NCLIM) IERR = 3
         GO TO 999
C                                       Undefined
 990  IERR = 3
C      WRITE (MSGTXT,1990)
C      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('DIRDEC: ITERATIONS REQUIRED = ',I5)
 1901 FORMAT ('DIRDEC: FAILED TO CONVERGE AFTER',I5,' ITERATIONS')
 1990 FORMAT ('DIRDEC: OPERATION UNDEFINED AT THE POLE OR BECAUSE OF',
     *   ' ROTATION')
      END
      SUBROUTINE NEWPOS (TYPE, RA0, DEC0, L, M, RAOUT, DECOUT, IERR)
C-----------------------------------------------------------------------
C! returns astronomical coordinates given direction cosines, projection
C# Coordinates
C   This software is the subject of a User agreement and is confidential
C   in nature. It shall not be sold or otherwise made available or
C   disclosed to third parties.
C-----------------------------------------------------------------------
C   NEWPOS determines the coordinates (RAOUT,DECOUT) corresponding to a
C   displacement (L,M) given by the direction cosines from coordinates
C   (RA0,DEC0).  The direction cosine L is assumed to be positive to
C   the east; M is positive to the north.  The routine works for
C   4 kinds of projective geometries and for Celestial, Ecliptic, or
C   Galactic coordinate systems.
C   This subroutine always uses an accurate computation.
C   Inputs:
C      TYPE  I    2 = SIN projection, 3 = TAN projection, 4 = arc
C                 projection, 5 = projection to north pole oriented
C                 plane (i.e. WSRT)
C      RA0   D    Coordinate reference right ascension (longitude)
C      DEC0  D    Coordinate reference declination (latitude)
C      L     D    Cosine angle of displacement to east
C      M     D    Cosine angle of displacement to north
C   Outputs:
C      RAOUT  D    right ascension or longitude at (L,M)
C      DECOUT D    declination or latitude at (L,M)
C      IERR   I    Error condition: 0 = ok, 1 = L,M crazy, 2 = bad
C                        type,  3 = answer undefined
C   ALL ANGLES IN THIS SUBROUTINE ARE IN RADIANS.
C-----------------------------------------------------------------------
      INTEGER   TYPE, IERR
      DOUBLE PRECISION RA0, DEC0, L, M, RAOUT, DECOUT
C
      DOUBLE PRECISION SINS, COSS, TWOPI, DECT, RAT, DT, DEPS, MG, DA,
     *   DD, DZ, COS0, SIN0
      INTEGER   NCLIM
	character msgtxt*80
      DATA TWOPI, DEPS /6.28318530717959D0, 1.D-5/
      DATA NCLIM /1000/
C-----------------------------------------------------------------------
      IERR = 2
      IF ((TYPE.GE.2) .AND. (TYPE.LE.9)) GO TO 5
         WRITE (MSGTXT,1000) TYPE
         CALL OUTPUT(MSGTXT)
         GO TO 999
 5    IERR = 1
      SINS = L*L + M*M
      IF (SINS.LE.1.0D0) GO TO 10
      IF (TYPE.EQ.9) GO TO 10
      IF ((TYPE.EQ.4) .AND. (SINS.LT.TWOPI*TWOPI/4.0D0)) GO TO 10
      IF ((TYPE.GT.5) .AND. (SINS.LT.TWOPI*TWOPI/2.5D0)) GO TO 10
         WRITE (MSGTXT,1005) L, M
         CALL OUTPUT(MSGTXT)
         GO TO 999
 10   IERR = 3
      DECOUT = 0.D0
      RAOUT = 0.D0
      COS0 = COS(DEC0)
      SIN0 = SIN(DEC0)
C                                       Use accurate solution
      GO TO (20, 20, 30, 40, 50, 60, 70, 80, 90), TYPE
C                                       SIN projection
 20   CONTINUE
         COSS = SQRT (1.0D0 - SINS)
         DT = SIN0 * COSS + COS0 * M
         IF (ABS(DT).GT.1.0D0) GO TO 999
         DECT = ASIN (DT)
         RAT = COS0 * COSS - SIN0 * M
         IF ((RAT.EQ.0.D0) .AND. (L.EQ.0.0D0)) GO TO 999
         RAT = ATAN2 (L, RAT) + RA0
         GO TO 900
C                                       TAN projection
 30   CONTINUE
         DECT = COS0 - M * SIN0
         IF (DECT.EQ.0.0D0) GO TO 999
         RAT = RA0 + ATAN2 (L, DECT)
         DECT = ATAN (COS(RAT-RA0) * (M * COS0 + SIN0) / DECT)
         GO TO 900
C                                       Arc projection
 40   CONTINUE
         SINS = SQRT(SINS)
         COSS = COS (SINS)
         IF (SINS.NE.0.0D0) THEN
            SINS = SIN (SINS) / SINS
         ELSE
            SINS = 1.0D0
            END IF
         DT = M * COS0 * SINS + SIN0 * COSS
         IF (ABS(DT).GT.1.0D0) GO TO 999
         DECT = ASIN (DT)
         DA = COSS - DT * SIN0
         DT = L * SINS * COS0
         IF ((DA.EQ.0.0D0) .AND. (DT.EQ.0.0D0)) GO TO 999
         RAT = RA0 + ATAN2 (DT, DA)
         GO TO 900
C                                       WSRT (North pole projection)
 50   CONTINUE
         DECT = COS0 - M * SIN0
         IF (DECT.EQ.0.0D0) GO TO 999
         RAT = RA0 + ATAN2 (L, DECT)
         DT = COS (RAT-RA0)
         IF (DT.EQ.0.0D0) GO TO 999
         DECT = DECT / DT
         IF (ABS(DECT).GT.1.0D0) GO TO 999
         DECT = ACOS (DECT)
         IF (DEC0.LT.0.0D0) DECT = -DECT
         GO TO 900
C                                       Global sinusoid
 60   CONTINUE
         DECT = DEC0 + M
         IF (ABS(DECT).GT.TWOPI/4.0D0) GO TO 999
         COSS = COS (DECT)
         IF (ABS(L).GT.TWOPI*COSS/2.0D0) GO TO 999
         RAT = RA0
         IF (COSS.GT.DEPS) RAT = RAT + L / COSS
         GO TO 900
C                                       Mercator
 70   CONTINUE
         GO TO 900
C                                       Aitoff
 80   CONTINUE
         GO TO 900
C                                       Stereographic
 90   CONTINUE
         DZ = (4.0D0 - SINS) / (4.0D0 + SINS)
         IF (ABS(DZ).GT.1.0D0) GO TO 999
         DECT = DZ * SIN0 + M * COS0 * (1.0D0+DZ) / 2.0D0
         IF (ABS(DECT).GT.1.0D0) GO TO 999
         DECT = ASIN (DECT)
         RAT = COS(DECT)
         IF (ABS(RAT).LT.DEPS) GO TO 999
         RAT = L * (1.0D0+DZ) / (2.0D0 * RAT)
         IF (ABS(RAT).GT.1.0D0) GO TO 999
         RAT = ASIN (RAT)
         MG = 1.0D0 + SIN(DECT) * SIN0 +
     *      COS(DECT) * COS0 * COS(RAT)
         IF (ABS(MG).LT.DEPS) GO TO 999
         MG = 2.0D0 * (SIN(DECT) * COS0 - COS(DECT) * SIN0
     *      * COS(RAT)) / MG
         IF (ABS(MG-M).GT.DEPS) RAT = TWOPI/2.0D0 - RAT
         RAT = RA0 + RAT
         GO TO 900
C                                       Return: in range RA
 900  RAOUT = RAT
      DECOUT = DECT
      IERR = 0
      IF (RAOUT-RA0.GT.TWOPI/2.0D0) RAOUT = RAOUT - TWOPI
      IF (RAOUT-RA0.LT.-TWOPI/2.0D0) RAOUT = RAOUT + TWOPI
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NEWPOS: ILLEGAL PROJECTION TYPE',I4,' INPUT')
 1005 FORMAT ('NEWPOS: ANGLE IS TOO LARGE. L,M=',2E11.3)
      END
"EOF"

fortran -o co_test co_test.for `mirlibs`
rm -rf cotest.xy cotest.diff
imgen out=cotest.xy

foreach proj (ncp sin tan arc)
  co_test in=cotest.xy x1=-10,23 proj=$proj 	 > cotest.diff
  co_test in=cotest.xy x1=-2310,23 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=-10,-2323 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=10,236 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=-10,-23 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=10,-2123 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=-10,23 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=-1093,-23 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=0,-23 proj=$proj		>> cotest.diff
  co_test in=cotest.xy x1=-1,-223 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=-189,23 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=1010,-23 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=-10,273 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=1000,-23 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=10000,10000 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=-10000,10000 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=10000,-10000 proj=$proj	>> cotest.diff
  co_test in=cotest.xy x1=-10000,-10000 proj=$proj	>> cotest.diff
end
set vals = ( `sed -e "s/D/E/g" cotest.diff|awk 'BEGIN{maxv=0;} {if($6 > maxv)maxv=$6;} END {flag = 0;if(maxv > 1e-10){flag=1}; print maxv,flag;}'` )

set maxv = $vals[1];
set flag = $vals[2];

rm -rf cotest.xy
echo "Maximum difference was $maxv"
echo " "
if($flag == 0) then
  echo "*******************"
  echo "*  cotest passed  *"
  echo "*******************"
  exit 0
else
  echo "*******************"
  echo "*  cotest failed  *"
  echo "*******************"
  echo " "
  echo "See cotest.diff for the details"
  exit 1
endif
