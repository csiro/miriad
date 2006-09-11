c************************************************************************
      PROGRAM imstore
      IMPLICIT NONE
c
c= imstore - Write out image data in an ascii format.
c& pjt
c: map analysis
c+
c	IMSTORE is a Miriad task which dumps or tabulates a region of
c       an image in a format suitable for other programs.
c       Different output modes can be selected.
c@ in
c     The input file name. No default.
c@ region
c     The region of interest of the input. Default is the entire input.
c     Full region specifations are supported.
c     Note: 3D datacubes do not display their 3rd coordinate.
c@ clip
c     Only image pixels above the clip value are written. The default
c     is all data.
c@ scale
c     Scale factor with which the image data is multiplied
c     before output. The default is 1. Note this scaling is done before the
c     clip test (as described above).
c@ mode
c     Output mode. This determines the format of the output. Currently 
c     accepted are: dump, uvgen and nemo. The default is dump.
c
c     mode=dump writes out the values of the selected pixel, only, with
c     there being 8 pixels per line.
c
c     mode=uvgen produces an output suitable for input to uvgen.
c           FLUX,DRA,DDEC,WMAJ,WMIN,WPA,POLN,POLPA
c     where the last 5 parameters are set to 0.0. DRA and DDEC are
c     given in arcsec w.r.t. the reference pixel if the axis
c     type is a recognized astronomical one.
c
c     mode=nemo outputs in the format:
c           DRA, DDEC, FLUX
c@ format
c     Override the default mode-dependant output format. Default: none.
c     Note: The format specification must be a legal fortran
c     format, including the parenthesis; e.g. ``format=(3F20.10)''
c@ units
c     Units of the output axes. Valid options are "absolute" and "relative"
c     (with respect to the reference pixel).
c     If an axis is a known astronomical coordinate system, the units are
c     displayed in arcsecs. The default is "relative".
c@ log
c     The output log file. The default is the terminal.
c--
c  Bugs:
c     * The uvgen and nemo modes should use the proper coordinate
c       conversion routines.
c     * Blanked pixels: These should be ignored unless there is a
c       flag to tell us to include them.
c
c  History:
c      7feb92 pjt   Created for UVGEN, cloned off HISTO
c     10jul92 pjt   Changed default mode to 'nemo' - fixed cdelt1
c                   scaling - added format statement
c     18aug92 pjt   Added units option, to appease Bikram.
c     12mar93 pjt   maxnax now from maxdim.h
c      5apr94 pjt   fixed units bug when 2nd axis is an angle
c      9jun94 pjt   region= clarification + export bug fix 5apr94
c     08may00 rjs   Change incorrect call of keyf to keya
c     19jun00 rjs   Tidy up at add mode=dump.
c------------------------------------------------------------------------
c
      INCLUDE 'mirconst.h'
      INCLUDE 'maxdim.h'
      INCLUDE 'maxnax.h'
      INTEGER MAXBOXES,MAXRUNS,NBUF
      REAL RAD2SEC
      CHARACTER PVERSION*(*)
      PARAMETER(MAXBOXES=2048)
      PARAMETER(MAXRUNS=3*MAXDIM,NBUF=8)
      PARAMETER(RAD2SEC=3600.0*180.0/PI)
      PARAMETER(PVERSION='Version 1.0 19-Jun-00')
c
      CHARACTER file*132,line*256,mode*20,logfile*132, 
     *          ctype1*10, ctype2*10, format*30, units*20
      INTEGER nsize(MAXNAX),plane(MAXNAX)
      INTEGER blc(MAXNAX),trc(MAXNAX)
      INTEGER i,j,k, npoints
      INTEGER naxis,lun,nout
      INTEGER boxes(MAXBOXES),runs(3,MAXRUNS),nruns
      REAL dat(MAXDIM),BUF(NBUF)
      REAL flux, scale, clip, wmaj, wmin, wpa, poln, polpa
      REAL crpix1, crpix2, cdelt1, cdelt2, crval1, crval2, x, y
      LOGICAL done, doclip, douvgen, donemo, dodump, doabs
c
      integer nunits,nmode
      parameter(nunits=2,nmode=3)
      CHARACTER unitss(2)*8,modes(3)*8
c
c  Externals.
c
      LOGICAL keyprsnt, astaxis
c
      data unitss/'relative','absolute'/
      data modes /'dump    ','uvgen   ','nemo    '/
c
      CALL output( 'ImStore: ' // PVERSION)
c
c  Get user inputs
c
      CALL keyini
      CALL keyf('in',file,' ')
      IF(file.EQ.' ')CALL bug('f','Input file must be given (in=)')
      CALL BoxInput('region',file,boxes,maxboxes)
      doclip = keyprsnt('clip')
      IF(doclip) CALL keyr('clip',clip,0.0)
      CALL keyr('scale',scale,1.0)
      CALL keymatch('mode',nmode,modes,1,mode,nout)
      if(nout.eq.0)mode = modes(1)
      CALL keya('format',format,' ')
      CALL keya('log',logfile,' ')
      CALL keymatch('units',nunits,unitss,1,units,nout)
      doabs = units.eq.'absolute'
      CALL keyfin
c
      donemo = .FALSE.
      douvgen = .FALSE.
      dodump  = .FALSE.
      IF(mode.EQ.'dump')then
         IF(format.EQ.' ') format = '(1p8e15.7)'
         CALL output('Output mode=dump; format='//format)
         dodump = .TRUE.
      ELSE IF(mode.EQ.'uvgen') THEN
         IF(format.EQ.' ') format = '(8F10.4)'
         CALL output('Output mode=uvgen; format='//format)
         douvgen = .TRUE.
      ELSE IF(mode.eq.'nemo') THEN
         IF(format.EQ.' ') format = '(3G16.9)'
         CALL output('Output mode=nemo; format='//format)
         donemo = .TRUE.
      ENDIF

      CALL logopen(logfile,' ')
c
c  Open file, and get some header info
c 
      CALL xyopen(lun,file,'old',MAXNAX,nsize)
      CALL rdhdi(lun,'naxis',naxis,0)
      naxis = MIN(naxis,MAXNAX)
      IF(nsize(1).GT.MAXDIM)CALL bug('f','Input file too big for me')
      CALL rdhdr(lun,'crpix1',crpix1,1.0)
      CALL rdhdr(lun,'crpix2',crpix2,1.0)
      CALL rdhdr(lun,'cdelt1',cdelt1,1.0)
      CALL rdhdr(lun,'cdelt2',cdelt2,1.0)
      CALL rdhdr(lun,'crval1',crval1,0.0)
      CALL rdhdr(lun,'crval2',crval2,0.0)
      CALL rdhda(lun,'ctype1',ctype1,' ')
      CALL rdhda(lun,'ctype2',ctype2,' ')
      IF (astaxis(ctype1)) THEN
         cdelt1 = cdelt1 * RAD2SEC
         crval1 = crval1 * RAD2SEC
      ENDIF

      IF (astaxis(ctype2)) THEN
         cdelt2 = cdelt2 * RAD2SEC
         crval2 = crval2 * RAD2SEC
      ENDIF

      IF (.NOT.doabs) THEN
         crval1=0.0
         crval2=0.0
      ENDIF
c
c  Set up the region of interest.
c
      CALL boxmask(lun,boxes,MAXBOXES)
      CALL boxset(boxes,MAXNAX,nsize,' ')
      CALL boxinfo(boxes,MAXNAX,blc,trc)
c
c init loop variables
      wmaj = 0.0
      wmin = 0.0
      wpa = 0.0
      poln = 0.0
      polpa = 0.0
      done = .FALSE.
      DO i=1,MAXNAX
	  plane(i) = blc(i)
      ENDDO
      npoints = 0
      nout = 0
c
c  Loop over the image
c
      DOWHILE(.NOT.done)
	  CALL boxruns(MAXNAX-2,plane(3),' ',boxes,runs,MAXRUNS,nruns,
     *					blc(1),trc(1),blc(2),trc(2))
	  CALL xysetpl(lun,MAXNAX-2,plane(3))
	  j = 0
	  DO k=1,nruns
	    IF(Runs(1,k).NE.j)THEN
	      j = runs(1,k)
              y = (j-crpix2)*cdelt2 + crval2
	      CALL xyread(lun,j,dat)
	    ENDIF
	    DO i=runs(2,k),runs(3,k)
	      flux = dat(i)*scale
              IF(.NOT.doclip .OR. doclip.AND.flux.GT.clip) THEN
                 npoints = npoints + 1
                 x = (i-crpix1)*cdelt1 + crval1
                 IF(douvgen)THEN
                   WRITE(line,format) flux,x,y,wmaj,wmin,
     *                                     wpa,poln,polpa
                   CALL logwrit(line)
                 ELSE IF(donemo) THEN
                   WRITE(line,format) x,y,flux
                   CALL logwrit(line)
		 ELSE IF(dodump) THEN
		   IF(nout.eq.NBUF)THEN
		     WRITE(line,format)buf
		     CALL logwrit(line)
		     nout = 0
		   ENDIF
		   nout = nout + 1
		   buf(nout) = flux
                 ENDIF
              ENDIF
	    ENDDO
	  ENDDO
	  CALL planeinc(MAXNAX-2,blc(3),trc(3),plane(3),done)
      ENDDO
      CALL xyclose(lun)
c
c  Flush the last bit out if needed.
c
      IF(dodump.and.nout.gt.0)then
	write(line,format)(buf(i),i=1,nout)
	CALL logwrit(line)
      ENDIF
c
      CALL logclose
      WRITE(line,'(A,I10,A)') 'Found ',npoints,' points'
      CALL output(line)

      END
c***********************************************************************
      SUBROUTINE planeinc(n,blc,trc,plane,done)
c
	IMPLICIT NONE
	INTEGER n,blc(n),trc(n),plane(n)
	LOGICAL done
c
c  Move to the next plane.
c
c-----------------------------------------------------------------------
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
        LOGICAL FUNCTION astaxis(ctype)
        CHARACTER ctype*(*)
c
        astaxis = .FALSE.
        
        IF(ctype(1:2).EQ.'RA') THEN
            astaxis = .TRUE.
        ELSE IF (ctype(1:3).EQ.'DEC') THEN
            astaxis = .TRUE.
        ELSE IF (ctype(1:3).EQ.'SLON') THEN
            astaxis = .TRUE.
        ELSE IF (ctype(1:3).EQ.'SLAT') THEN
            astaxis = .TRUE.
        ELSE IF (ctype(1:3).EQ.'ELON') THEN
            astaxis = .TRUE.
        ELSE IF (ctype(1:3).EQ.'ELAT') THEN
            astaxis = .TRUE.
        ENDIF

        END
