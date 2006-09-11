c***********************************************************************
c  Interactive masking of image data loaded onto a TV device.
c
c  History:
c    pjt  5jun91    Cloned off from tvflag - imflag/immask
c        28jul91    finished batch mode
c         9jul92    extracted pure batch mode program - immask
c         2feb93    more reporting, consistent history
c        21jun93    formal options= installed
c    rjs 16sep93    Deleted the undocumented and unused log file option.
c    pjt 13may94    handle larger cubes in output format 
c    pjt  8jun94    region= clarification
c    pjt  4apr96    complain if logic= is not valid, and work in lower case
c    vjm  5sep96    attempt to unmangle documentation
c***********************************************************************
c= immask - mask an image dataset
c& pjt
c: image analysis
c+
      PROGRAM immask
c
c   IMMASK is a MIRIAD task which allows you to mask an image
c   dataset, or find out the number of masked pixels in an image.
c
c   Masking is directly done to the ``mask'' item of an image dataset, i.e.
c   the actual image data are not modified, only the mask.
c   A pixel with a TRUE mask value is considered a good pixel.
c
c   To set all pixels in an image to FALSE:
c       immask in=ngc_289_6cm flag=false logic=and
c
c   To mask out the pixels in a region:
c       immask in=ngc_289_20cm region=@cgcurs.region flag=false logic=and
c
c   To undo all masking, delete the mask item from the header:
c       delhd in=ngc_289_6cm/mask
c   
c   To change the masking, you must specify a value for the LOGIC keyword.
c   Otherwise IMMASK merely reports the current numbers of masked and
c   unmasked pixels.
c
c   See also MATHS for other ways to set the image mask.
c
c@ in
c   The name of the input image dataset. No default.
c
c@ region
c   Regions which will be masked with the ``flag'' value (see below).
c   Full region descriptions are supported.
c   Note that missing image planes will be automatically masked 
c   to the opposite value set by the ``flag'' keyword below. 
c   Default: whole image.
c
c@ logic
c   The logic of the masking operation. It can have a value of ``AND'',
c   `OR'' or ``NOT'' which determines how the selected region(s) from the 
c   region= keyword are masked with the existing mask item in the image:
c       OR:     region .OR. mask
c       AND:    region .AND. mask
c       NOT:    if (region) .NOT.mask
c   If no value provided, the program will simply report on the 
c   total number of pixels already flagged good and bad.
c   No default.
c
c@ flag
c   The value of the mask inside the selected regions. Can be ``true''
c   or ``false''. Outside selected region it will be its opposite.
c   A ``true'' value is considerd a good pixel.
c   Default: true.
c
c@ history
c   Specifies whether the history should be updated. This should
c   normally never be set to false, though some scripts prefer
c   to do this manually via ADDHIS, and set this parameter to FALSE.
c   In reporting mode (no ``logic'' operation supplied) the history
c   is not updated.
c   Default: TRUE
c
c@ options
c   Valid options: 
c      datamin      flag all values with `flag' when they equal the
c                   value of the 'datamin' keyword in the image header.
c   Default: none.
c
c--
c-----------------------------------------------------------------------
c  Internal parameters.
      INCLUDE 'maxdim.h'

      CHARACTER  PVERSION*(*)
      INTEGER MAXBOXES, MAXRUNS, MAXNAX

      PARAMETER (PVERSION = 'Version 1.0 4-apr-96')
      PARAMETER (MAXBOXES=4096, MAXRUNS=3*MAXDIM, MAXNAX=3)
c
c  Internal variables.
c
      INTEGER box1(MAXBOXES), run1(3,MAXRUNS+1), nrun1, naxis1(MAXNAX)
      INTEGER lun1, k1, blc(MAXNAX), trc(MAXNAX)
      INTEGER i, j, lmode, nin, nmsk, nout, iz
      LOGICAL mask1(MAXDIM), mask2(MAXDIM), dohist, flag, noflag
      LOGICAL dodatmin
      CHARACTER imfile*132, mesg*132, logic*4, option*20
      REAL datamin, data(MAXDIM)
c
c  Externals.
c
      LOGICAL   keyprsnt
c
c  End declarations.
c-----------------------------------------------------------------------
c  Announce program.
c
      CALL output('IMMASK: '//PVERSION)
c-----------------------------------------------------------------------
c  Use the key routines to get the user input parameters.
c
c  The complication here that we want to aribitrary mask
c  existing and user supplied regions is that a tempfile
c  is needed, which has copies of relevant info on the
c  AXES in the infile..more on that later
c
      CALL keyini
      CALL keyf('in', imfile, ' ')
      CALL assertl(imfile.NE.' ','Image dataset must be given (in=)')
      CALL boxinput('region', imfile, box1, MAXBOXES)
      CALL keya('logic', logic, ' ')
      CALL keyl('flag', flag, .TRUE.)
      noflag = .NOT.flag
      CALL keyl('history',dohist,.TRUE.)
      dodatmin = .FALSE.
      IF (keyprsnt('options')) THEN
        CALL keya('options',option,' ')
        IF (option.EQ.'datamin') THEN
            dodatmin = .TRUE.
        ENDIF
      ENDIF
      CALL keyfin
c
      CALL lcase(logic)
      IF (logic(1:1).EQ.'a') THEN
         lmode=1
         CALL bug('i','logic:  region .AND. mask')
      ELSEIF (logic(1:1).EQ.'o') THEN
         lmode=2
         CALL bug('i','logic:  region .OR. mask')
      ELSEIF (logic(1:1).EQ.'n') THEN
         lmode=3
         CALL bug('i','logic: if (region) .NOT.mask')
      ELSE
	 lmode = 0
         dohist = .FALSE.
         CALL bug('i','Reporting mode')
      ENDIF

      IF (dodatmin) lmode=4

c  Open corresponding image file
         CALL xyopen(lun1,imfile,'old',MAXNAX,naxis1)
         CALL rdhdr(lun1,'datamin',datamin,0.0)
         IF (MAXNAX.NE.3) CALL bug('f','Code can only handle MAXNAX=3')
c  At this stage box1() corresponds to the region= specification
         CALL boxset(box1,MAXNAX,naxis1,' ')
         nin = 0
         nmsk = 0
         nout = 0
c  Loop over the WHOLE cube, not the blc(),trc() subcube. This in order
c  to read and write the full mask item. Note that this will only work
c  for 2/3D datasets.
         DO iz=1,naxis1(3)
            CALL boxruns(MAXNAX-2,iz,' ',box1,run1,MAXRUNS,nrun1,
     *                  blc(1), trc(1), blc(2), trc(2))
            CALL xysetpl(lun1,MAXNAX-2,iz)
            k1=1
            run1(1,nrun1+1) = 0
            DO j=1,naxis1(2)
               CALL xyflgrd(lun1,j,mask1)
               DO i=1,naxis1(1)
                  mask2(i) = noflag
                  IF (mask1(i)) nin = nin + 1
               ENDDO
c              * patch the sections inside the region to 'flag' value
               DOWHILE (k1.LE.nrun1 .AND. j.EQ.run1(1,k1))
                  DO i=run1(2,k1),run1(3,k1)
                    mask2(i) = flag               
                    nmsk = nmsk + 1
                  ENDDO
                  k1=k1+1
               ENDDO
               IF (lmode.eq.1) THEN
                  DO i=1,naxis1(1)
                      mask1(i) = mask1(i).AND.mask2(i)
                  ENDDO
               ELSE IF (lmode.eq.2) THEN
                  DO i=1,naxis1(1)
                     mask1(i) = mask1(i).OR.mask2(i)
                  ENDDO
               ELSE IF (lmode.eq.3) THEN
                  DO i=1,naxis1(1)
                     IF(mask2(i)) mask1(i) = .NOT.mask1(i)
                  ENDDO
               ELSE IF (lmode.EQ.4) THEN
                  CALL xyread(lun1,j,data)
                  DO i=1,naxis1(1)
                     IF(data(i).EQ.datamin) mask1(i) = flag
                  ENDDO
               ENDIF
               DO i=1,naxis1(1)
                  IF (mask1(i)) nout = nout + 1
               ENDDO
               IF (lmode.GT.0) CALL xyflgwr(lun1,j,mask1)
            ENDDO
         ENDDO

         IF (lmode.GT.0) THEN
            WRITE (mesg,'('' Masking: '',I9,1x,A,1x,I9,'' => '',I9)')
     *          nin, logic, nmsk, nout
         ELSE
            WRITE (mesg,'(I9,A,I9,A)') nin, ' out of ', 
     *           naxis1(1)*naxis1(2)*naxis1(3),
     *           ' pixels are masked as good'
         ENDIF
         CALL output(mesg)

         IF (dohist) THEN
            CALL hisopen(lun1,'append')
            CALL hiswrite(lun1,'IMMASK: Miriad ImMask: '//PVERSION)
            CALL hisinput(lun1,'IMMASK')
            CALL hiswrite(lun1,'IMMASK: '//mesg)
            CALL hisclose(lun1)
         ENDIF
         CALL xyclose(lun1)

      END
c
c***********************************************************************
c
        SUBROUTINE planeinc(n,blc,trc,plane,done)
c
        INTEGER n,blc(n),trc(n),plane(n)
        LOGICAL done
c
c  Move to the next plane.
c
c------------------------------------------------------------------------
        INTEGER k
c
        k = 1
        done = .TRUE.
c
        DO WHILE(done.AND.k.LE.n)
          done = plane(k).GE.trc(k)
          IF(done)THEN
            plane(k) = blc(k)
          ELSE
            plane(k) = plane(k) + 1
          ENDIF
          k = k + 1
        ENDDO
        END
c***********************************************************************
