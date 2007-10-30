      PROGRAM cotra
c-----------------------------------------------------------------------
c History:
c     30-may-91   Quick and dirty				     PJT
c     20-jun-91   Correction due to dsfetr/dsfetra doc error         MJS
c     24-sep-93   Implemented two-step using intermediate EQ         PJT
c-----------------------------------------------------------------------
c= cotra - coordinate transformations
c& pjt
c: utility
c+
c  COTRA is a MIRIAD task to transform between astronomical coordinate 
c  systems. All calculations are done in double precision, and the
c  equatorial system is used as a pitch between which an arbitrary 
c  transformation is always done.
c
c  Valid coordinate systems must be one of:
c  equatorial, galactic, ecliptic, super-galactic
c  [see also subroutine (d)sfetra]
c@lon
c  Input longitude.
c  Equatorial needs to be in decimal hours (0..24), the rest is in
c  decimal degrees (0..360).
c  Default: 0.0
c@lat
c  Input latitude (in decimal degrees, -90..90). 
c  Default: 0.0
c@sysin
c  Input coordinate system. Valid systems are listed above.
c  Default: galactic
c@sysout
c  Output coordinate system. Valid systems are listed above.
c  Default: equatorial
c@epoch
c  Epoch of coordinate system. Not implemented yet.
c  Default: 1950.00
c-----------------------------------------------------------------------
c include file
      INCLUDE 'mirconst.h'
c parameters:
      CHARACTER  VERSION*(*)
      PARAMETER (VERSION='Version 1.0 24-sep-93')
      DOUBLE PRECISION D2R
      PARAMETER (D2R=DPI/180.0d0)
c local variables:
      CHARACTER sysname(2)*20, epoch*20, snames(3)*3, mesg*80
      DOUBLE PRECISION dlon, dlat
      LOGICAL inv
      INTEGER i, sys, sysnum(2)
      DATA snames/'GAL','ECL','SGL'/
      
c Announce presence
      CALL output('COTRA: '//VERSION)
c Get parameters from command lines
      CALL keyini
      CALL keyd('lon',dlon,0.0d0)
      CALL keyd('lat',dlat,0.0d0)
      CALL keya('sysin',sysname(1),'galactic')
      CALL keya('sysout',sysname(2),'equatorial')
      CALL keya('epoch',epoch,'1950.0')
      CALL keyfin

      IF (epoch(1:6).NE.'1950.0') CALL bug('i',
     *      'Epochs other than 1950.0 not implemented yet')
      DO i=1,2
        sysnum(i) = -1
        IF (sysname(i)(1:2).EQ.'eq') sysnum(i)=0
        IF (sysname(i)(1:1).EQ.'g')  sysnum(i)=1
        IF (sysname(i)(1:2).EQ.'ec') sysnum(i)=2
        IF (sysname(i)(1:1).EQ.'s')  sysnum(i)=3
      ENDDO
      IF (sysnum(1).LT.0) CALL bug('f','Unknown sysin=')
      IF (sysnum(2).LT.0) CALL bug('f','Unknown sysout=')

      dlon = dlon*D2R
      dlat = dlat*D2R
      IF (sysnum(1).NE.0 .AND. sysnum(2).NE.0) THEN
        CALL dsfetra(dlon,dlat,.TRUE., sysnum(1))
        CALL dsfetra(dlon,dlat,.FALSE.,sysnum(2))
      ELSE IF (sysnum(1).EQ.0) THEN
        mesg = 'Transforming from EQ to '//snames(sysnum(2))
        CALL output(mesg)
        dlon = dlon*15.0d0
        inv = .FALSE.
        sys = sysnum(2)
        CALL dsfetra(dlon,dlat,inv,sys)
      ELSE IF (sysnum(2).EQ.0) THEN
        mesg = 'Transforming from '//snames(sysnum(1))//' to EQ'
        CALL output(mesg)
        inv = .TRUE.
        sys = sysnum(1)
        CALL dsfetra(dlon,dlat,inv,sys)
        dlon = dlon/15.0d0
      ELSE
        CALL bug('f','Impossible logic')
      ENDIF
      dlon = dlon/D2R
      dlat = dlat/D2R
      WRITE(mesg,'(2(F15.10,1X))') dlon,dlat
      CALL output(mesg)

      END
