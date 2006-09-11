c************************************************************************
c* wpfit -- Weighted least squares polynomial fit.
c& rjs
c: polynomials
c+
      SUBROUTINE WPFIT(ND,NP,X,Y,W,A,RNORM,PHI,PHIX,IERR)
c
      IMPLICIT NONE
      INTEGER ND,NP,IERR
      REAL X(NP),Y(NP),W(NP),A(ND+1),PHI(2,ND+1),PHIX(4,NP),RNORM
C
C
C     Weighted least squares polynomial fit.
C
C  Input:
C    ND		Poly order.
C    NP		Number of points.
C    X,Y	X and Y values.
C    W		Weights.
C  Scratch:
C    PHI	At least of size 2*(nd + 1)
C    PHIX	At least of size 4*NP
C  Output:
C    IERR	0 on success. 1 if there is a dimension problem. 3 is there
C		is a negative weight.
C    RNORM	Weighted error, sqrt(Sum W(i)*(y(i)-poly)**2)
C    A		Poly coefficients.
C		  poly = a(1) + a(2)*x + a(3)*x**2 ...
C
C  Algorithm:
C    The abcissas, X(i) are mapped into the interval [-1,1]. Then the
C    Forsythe procedure is used.
C
C--
C  History:
C    		  Derived from code in the NSWC library, from code written
C		  by A.H. Morris.
C     5nov93 rjs  Adapted to Miriad's needs. Minimal changes.
C------------------------------------------------------------------------
      REAL LAMBDA,C,TEMP,XMIN,XMAX,ZA,ZB,ALPHA
      DOUBLE PRECISION DALPHA,DSUM
      INTEGER NW,LA,LB,LS,M,MP1,K,ND1
C  
C                      ERROR CHECKING
C
      IF (ND.LT.1.OR.NP.LT.2) GO TO 200
      NW=0
      DSUM=0.D0
      DO 13 K=1,NP
      IF (W(K)) 202,13,10
   10 NW=NW+1
      DSUM=DSUM+DBLE(W(K))
      IF (NW.GT.1) GO TO 11
      XMIN=X(K)
      XMAX=X(K)
      GO TO 13
   11 IF (X(K).GE.XMIN) GO TO 12
      XMIN=X(K)
      GO TO 13
   12 IF (X(K).GT.XMAX) XMAX=X(K)
   13 CONTINUE
      IF (ND.GE.NW) GO TO 200
C
C                      INITIALIZATION
C
      IERR=0
      ND1=ND+1
      DO 20 K=1,ND1
      A(K)=0.0
      PHI(1,K)=0.0
   20 PHI(2,K)=0.0
C
C                SET Z=A+B*X WHERE ABS(Z).LE.1
C
      ZB=2.0/(XMAX-XMIN)
      ZA=-XMIN*ZB-1.0
      DO 25 K=1,NP
   25 PHIX(3,K)=ZA+ZB*X(K)
C
C           COMPUTE THE CLOSEST POLYNOMIAL OF DEGREE 0
C
      LAMBDA=DSUM
      PHI(1,1)=1.0/SQRT(LAMBDA)
      DALPHA=0.D0
      DSUM=0.D0
      DO 30 K=1,NP
      PHIX(1,K)=PHI(1,1)
      DALPHA=DALPHA+DBLE(W(K)*PHIX(3,K))
   30 DSUM=DSUM+DBLE(W(K)*Y(K))
      ALPHA=DALPHA/LAMBDA
      A(1)=DSUM/LAMBDA
      DO 31 K=1,NP
   31 PHIX(4,K)=A(1)
C
      LA=2
      LB=1
      DO 90 M=1,ND
      MP1=M+1
C
C          GENERATE LAMBDA(M)*PHI(M) AND EVALUATE IT AT Z
C
      IF (M.NE.1) GO TO 50
      PHI(2,1)=-ALPHA*PHI(1,1)
      PHI(2,2)=PHI(1,1)
      DO 40 K=1,NP
   40 PHIX(2,K)=(PHIX(3,K)-ALPHA)*PHI(1,1)
      GO TO 60
C
   50 C=0.0
      DO 51 K=1,M
      PHI(LA,K)=DBLE(C)-DBLE(ALPHA*PHI(LB,K))-DBLE(LAMBDA*PHI(LA,K))
   51 C=PHI(LB,K)
      PHI(LA,MP1)=C
      DO 52 K=1,NP
   52 PHIX(LA,K)=(PHIX(3,K)-ALPHA)*PHIX(LB,K)-LAMBDA*PHIX(LA,K)
C
C                COMPUTE ALPHA(M) AND LAMBDA(M)
C
   60 DALPHA=0.D0
      DSUM=0.D0
      DO 61 K=1,NP
      C=W(K)*PHIX(LA,K)*PHIX(LA,K)
      DALPHA=DALPHA+DBLE(C*PHIX(3,K))
   61 DSUM=DSUM+DBLE(C)
      LAMBDA=DSUM
      ALPHA=DALPHA/LAMBDA
      LAMBDA=SQRT(LAMBDA)
C
C              GENERATE PHI(M) AND EVALUATE IT AT Z
C
      DO 70 K=1,MP1
   70 PHI(LA,K)=PHI(LA,K)/LAMBDA
      DO 71 K=1,NP
   71 PHIX(LA,K)=PHIX(LA,K)/LAMBDA
C
C        COMPUTE THE CLOSEST POLYNOMIAL OF DEGREE M OR LESS
C                     AND EVALUATE IT AT Z
C
      DSUM=0.D0
      DO 80 K=1,NP
   80 DSUM=DSUM+DBLE(W(K)*(Y(K)-PHIX(4,K))*PHIX(LA,K))
      C=DSUM
      DO 81 K=1,MP1
   81 A(K)=A(K)+C*PHI(LA,K)
      DO 82 K=1,NP
   82 PHIX(4,K)=PHIX(4,K)+C*PHIX(LA,K)
C
      LS=LA
      LA=LB
   90 LB=LS
C
C                        COMPUTE RNORM
C
      DSUM=0.D0
      DO 95 K=1,NP
   95 DSUM=DSUM+DBLE(W(K)*(Y(K)-PHIX(4,K))**2)
      RNORM=SQRT(DSUM)
C
C        CONVERT THE CLOSEST POLYNOMIAL FROM A POLYNOMIAL
C                  IN Z TO A POLYNOMIAL IN X
C
      A(1)=A(1)+ZA*A(2)
      A(2)=ZB*A(2)
      IF (ND.EQ.1) RETURN
      PHI(1,1)=ZA
      PHI(1,2)=ZB
      DO 102 M=2,ND
      MP1=M+1
      C=0.0
      DO 100 K=1,M
      TEMP=PHI(1,K)*ZB
      PHI(1,K)=PHI(1,K)*ZA+C
  100 C=TEMP
      PHI(1,MP1)=C
      DO 101 K=1,M
  101 A(K)=A(K)+A(MP1)*PHI(1,K)
  102 A(MP1)=A(MP1)*PHI(1,MP1)
      RETURN
C
C                        ERROR RETURN
C
  200 IERR=1
      RETURN
  202 IERR=3
      RETURN
      END
