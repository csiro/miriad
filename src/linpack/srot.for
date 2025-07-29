C***********************************************************************
c*SROT -- Apply a plane rotation.
c:BLAS
c+
      SUBROUTINE  SROT (N,SX,INCX,SY,INCY,C,S)
C
C     APPLIES A PLANE ROTATION.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C--
      REAL SX(1),SY(1),STEMP,C,S
      INTEGER I,INCX,INCY,IX,IY,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C	CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C	  TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
	STEMP = C*SX(IX) + S*SY(IY)
	SY(IY) = C*SY(IY) - S*SX(IX)
	SX(IX) = STEMP
	IX = IX + INCX
	IY = IY + INCY
   10 CONTINUE
      RETURN
C
C	CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 DO 30 I = 1,N
	STEMP = C*SX(I) + S*SY(I)
	SY(I) = C*SY(I) - S*SX(I)
	SX(I) = STEMP
   30 CONTINUE
      RETURN
      END
