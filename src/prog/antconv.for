      PROGRAM antconv
c 
c-----------------------------------------------------------------------
c= antconv - convert baseline corrections into antenna corrections 
c& pjt
c: calibration, utility
c+
c     ANTCONV converts baseline corrections, as they
c     come from the Hat Creek Observatory (mostly Rick Forster),
c     to antenna based corrections which can be used in uvedit to
c     correct your beautiful data. See also UVEDIT.
c     
c     Example of Correction text file:
c          (Delta BL1-2 X)  (Delta BL1-2 Y) (Delta BL1-2 Z)
c          (Delta BL1-3 X)  (Delta BL1-3 Y) (Delta BL1-3 Z)
c              ....            ....           ....
c          (Delta BL1-n X)  (Delta BL1-n Y) (Delta BL1-n Z)
c
c@ in
c     Name of the input text file containing the baseline corrections for 
c     baselines involving antenna 1 and each other antenna in the array.
c     Note the delta baselines must be 1-n, NOT n-1; so delta baseline
c     values for baseline 3-1 must be negated to be delta baselines for
c     baseline 1-3.
c     The baseline corrections are expected in units of nanoseconds and
c     in an f9.6 format (OR separated by commas).
c     No default.
c@ nants
c     Number of antennas.  Default: 3
c     NOTE: Currently you should not (have to) change this 
c     in our 3-element array. Maximum antennas possible is 9.
c@ refant
c     Reference antenna for new positions. Currently this can ONLY be
c     1, which is the default.
c     Default: refant=1
c--
c  History:
c
c  pjt   ??????? Original version.
c  mjs   17nov91 Declared variable "linelen" as integer.
c  jpm   10jan92 Change list directed read to formatted read
c
c Needs:
c       Measured baseline corrections   db(i,j); i=component
c                                                j=baseline
c currently looks in text file specified by "in" for this information.
c
c Outputs: 
c       Delta antenna positions for antennas 2 through n.
c
c-----------------------------------------------------------------------
c include files:
      INCLUDE 'maxdim.h'
c   
      CHARACTER PVERSION*(*)
      PARAMETER (PVERSION='04-oct-91')
c local variables:
      DOUBLE PRECISION db(3,MAXANT),dant(3,MAXANT)
      INTEGER i, j, nants, refant, iostat, lin, linelen
      CHARACTER infile*80, line*256
c-----------------------------------------------------------------------
      CALL output('ANTCONV: '//PVERSION)
c
      CALL keyini
      CALL keyf('in',infile,' ')
      CALL keyi('nants',nants,3)
      CALL keyi('refant',refant,1)
      IF(refant.NE.1) CALL bug('f',
     *              'REFANT? Didn''t read the doc, did you?')
      IF(infile.EQ.' ') CALL bug('f',
     *          'Need input text filename with delta baselines (in=)')
      CALL keyfin
c-----------------------------------------------------------------------
c later joe!      nbase=(nants*(nants-1))/2
c

c Open input file:
      CALL txtopen(lin,infile,'old',iostat)
      IF(iostat.NE.0) CALL bug('f',
     *              'Could not open input file '//infile)

c read in the baseline corrections for baseline 12, 13, etc
      DO j=1,nants-1
         CALL txtread(lin,line,linelen,iostat)
         CALL iocheck(iostat,1)
c        READ(line,*) db(1,j),db(2,j),db(3,j)
         READ(line,2000) db(1,j),db(2,j),db(3,j)
 2000    FORMAT(3(f9.6))
c write out baseline corrections so user can see that they
c have been parsed correctly
         WRITE(line,2001) db(1,j),db(2,j),db(3,j)
 2001    FORMAT(3(G20.10,1x))
      ENDDO
c
c assume that Antenna 1 is always in the 'true' position
c only worry about baselines with Antenna 1  === FIX THIS LATER ====
c
c compute delta antenna positions from delta baselines
c
c Delta antenna 2 = Delta baseline 1-2
c Delta antenna n = Delta baseline 1-n
c 
      DO i=1,3
         DO j=1,nants-1
            dant(i,j+1) = db(i,j)
         ENDDO
      ENDDO
c 
c print out results to screen for grabbing with mouse
c
      CALL output(' Delta Antenna Positions for UVEDIT')
      Write(line,2010) (j,(dant(i,j),i=1,3),j=2,nants)
 2010 Format('dantpos =',9(i1,',',f7.4,',',f7.4,',',f7.4,','))
      CALL output(line)
      CALL txtclose(lin)

      END
c
      SUBROUTINE iocheck(iostat,mode)
      INTEGER iostat, mode
c
c  mode=1 : read : iostat   -1=EOF    0=OK    else=error
c  mode=2 : write: iostat   0=OK else=error
c
      IF(mode.NE.1.AND.mode.NE.2) CALL bug('i',
     *         'IOCHECK: illegal mode')

      IF(mode.EQ.1.AND.iostat.EQ.-1) THEN
         CALL bug('f','Input file not long enough')
      ELSE IF(iostat.NE.0) THEN
         IF(mode.EQ.1) CALL bug('f','read error')
         IF(mode.EQ.2) CALL bug('f','write error')
      ENDIF

      END
