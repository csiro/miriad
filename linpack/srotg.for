C***********************************************************************
c*SROTG -- Given's plane rotation.
c:BLAS
c+
      SUBROUTINE SROTG(SA,SB,C,S)
C
C     CONSTRUCT GIVENS PLANE ROTATION.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
C--
      REAL SA,SB,C,S,ROE,SCALE,R,Z
C
      ROE = SB
      IF( ABS(SA) .GT. ABS(SB) ) ROE = SA
      SCALE = ABS(SA) + ABS(SB)
      IF( SCALE .NE. 0.0 ) GO TO 10
	 C = 1.0
	 S = 0.0
	 R = 0.0
	 GO TO 20
   10 R = SCALE*SQRT((SA/SCALE)**2 + (SB/SCALE)**2)
      R = SIGN(1.0,ROE)*R
      C = SA/R
      S = SB/R
   20 Z = 1.0
      IF( ABS(SA) .GT. ABS(SB) ) Z = S
      IF( ABS(SB) .GE. ABS(SA) .AND. C .NE. 0.0 ) Z = 1.0/C
      SA = R
      SB = Z
      RETURN
      END
