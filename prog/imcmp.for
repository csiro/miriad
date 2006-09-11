c***********************************************************************
      PROGRAM imcmp
      IMPLICIT NONE
c
c= imcmp - Compare two images
c& pjt
c: map analysis
c+
c	IMCMP is a MIRIAD task to compare two images. The maps must have
c	the same dimensions.
c	The two images are compared on a pixel by pixel basis, within a
c	user defined region.
c@ in1
c	The first input image.	No default
c@ in2
c	The second input image.  No default
c@ region
c	Region to select data to compare. Only pixels where both input
c	maps were unmasked will be compared. Only a rectangular region
c	of interest is supported.
c@ cut
c	Cutoff applied to data. By default not used, all data used.
c@ tol
c	Tolerance when comparing the absolute difference between the two
c	maps on pixel by pixel basis. Default: 0
c@ device
c	PGPLOT Plotting device. Default: no plot.
c@ limx
c	Minimum and maximum for plotting the pixel values of image 1.
c	Default: autoscaling.
c@ limy
c	Minimum and maximum for plotting the pixel values of image 2
c	Default: autoscaling.
c@ log
c	File in which to write the data (as two columns).
c	Default: no logfile.
c--
c
c  History:
c    26sep91 pjt   Model created to have Jan Tauber toy with it...
c   < 9oct91 jt    I toyed and joyed
c    10oct91 pjt   replaced open(11) with miriad txtopen etc.
c		   also added the tol= keyword and bit more documentation
c    11feb93 rjs   Eliminate MAXDIM**2 array.
c    22feb93 pjt   Submitted more recent version (and retrofitted feb11 patch)
c		   implemented cut=; pgbegin as function, (all from apr-92)
c     4mar93 pjt   tmpdim.h until memalloc() done properly
c    13mar93 mjs   pgplot subr names have less than 7 chars.
c     8jun94 pjt   region= clarification
c    14may96 rjs   Fix bugs, implement region selection, better messages,
c		   some tidying. Shoddy job pjt.
c    02nov04 tw    Replace keyf with keya for output log file.
c
c  Various things to do or to think about:
c   - fit portions of data with LSQFIT
c   - plot optional in log-log
c   - plot is simple, could have more control, like minmax,
c     adding a fit
c   - cutoff, i.e. ignoring near zero's or so
c   - region: but in this case perhaps only square areas
c     more control could be done via a program IMFLAG
c     which is under construction (sep-91 PJT)
c   - more check if the two maps are really the same. Now
c     only the dimensions are checked, but really one
c     should also check if crpix, cdelt and crval are
c     consistent.
c-----------------------------------------------------------------------
      INCLUDE 'maxdim.h'
c
      CHARACTER PVERSION*(*)
      INTEGER NAXIS, MAXPTS,  MAXBOXES
      PARAMETER(NAXIS=3, MAXPTS=MAXBUF/2, MAXBOXES=1024)
      PARAMETER(PVERSION='Version 1.0 02-Nov-04')
c
      CHARACTER in1*80,in2*80,line*256,logfile*80
      INTEGER row,plane,i,dcount,llog,iostat,tolcount
      INTEGER lin1,lin2,size1(NAXIS),size2(NAXIS),blc(NAXIS),trc(NAXIS)
      REAL buf1(MAXDIM),buf2(MAXDIM), d1(MAXPTS), d2(MAXPTS),tol,cut
      REAL dmin, dmax,dminh,dmaxh,dminv,dmaxv,limx(2),limy(2),diff
      REAL boxes(MAXBOXES)
      LOGICAL mask1(MAXDIM), mask2(MAXDIM), first, doplot
      LOGICAL dolog,dolimx,dolimy,docut
      CHARACTER device*40
c
c  Externals.
c
      LOGICAL keyprsnt
      INTEGER len1, pgbeg
      CHARACTER itoaf*8
c-----------------------------------------------------------------------
      CALL output( 'IMCMP: '//PVERSION)
c
c  Get the input parameters.
c
      CALL keyini
      CALL keyf('in1',in1,' ')
      CALL keyf('in2',in2,' ')
      CALL boxinput('region',in1,boxes,MAXBOXES)
      CALL keyr('tol',tol,0.0)
      doplot = keyprsnt('device')
      IF(doplot) CALL keya('device',device,' ')
      dolog = keyprsnt('log')
      IF(dolog) CALL keya('log',logfile,' ')
      dolimx = keyprsnt('limx')
      IF(dolimx) THEN
	CALL keyr('limx',limx(1),0.0)
	CALL keyr('limx',limx(2),0.0)
	IF (limx(1).eq.limx(2)) THEN
	  CALL bug('f','Minimum and Maximum limits are the same')
	ENDIF
      ENDIF
      dolimy = keyprsnt('limy')
      IF(dolimy) THEN
	CALL keyr('limy',limy(1),0.0)
	CALL keyr('limy',limy(2),0.0)
	IF (limy(1).eq.limy(2)) THEN
	  CALL bug('f','Minimum and Maximum limits are the same')
	ENDIF
      ENDIF
      docut = keyprsnt('cut')
      IF(docut) CALL keyr('cut',cut,0.0)
      CALL keyfin
c
c  and some checks before going on
c
      IF(in1.EQ.' ' .OR. in2.EQ.' ') THEN
	 CALL bug('f','You must specify two input files (in1=,in2=)')
      ENDIF
c
c  Open the input maps and check if sizes are the same and that
c  they fit in the assigned 1D line buffers
c
      CALL xyopen(lin1,in1,'old',NAXIS,size1)
      CALL xyopen(lin2,in2,'old',NAXIS,size2)
      DO i=1,NAXIS
	 IF (size1(i).NE.size2(i)) THEN
	    CALL bug('f','Sizes of images do not agree')
	 ENDIF
      ENDDO
      CALL assertl(size1(1).LE.MAXDIM,'Image too big for me')
      CALL boxset(boxes,NAXIS,size1,'s')
      CALL boxinfo(boxes,NAXIS,blc,trc)
c
c  copy the maps and masks into the output, plane by plane, row by row
c  NOTE: the region limits should be added here
c
      first = .TRUE.
      dcount = 0
      DO plane = blc(3),trc(3)
	 CALL xysetpl(lin1,1,size1(3))
	 CALL xysetpl(lin2,1,size1(3))
	 DO row=blc(2),trc(2)
	    CALL xyread(lin1,row,buf1)
	    CALL xyflgrd(lin1,row,mask1)
	    CALL xyread(lin2,row,buf2)
	    CALL xyflgrd(lin2,row,mask2)
	    DO i=blc(1),trc(1)
	       IF(mask1(i).AND.mask2(i)) THEN
		  IF(.NOT.docut .OR.
     *		     (docut .AND. buf1(i).GT.cut .AND. buf2(i).GT.cut))
     *		  THEN
		     dcount = dcount + 1
		     IF(dcount.LE.MAXPTS) THEN
			d1(dcount) = buf1(i)
			d2(dcount) = buf2(i)
		     ELSE
			dcount=MAXPTS
		     ENDIF
		     IF (first) THEN
			dmin=buf1(i)
			dmax=buf1(i)
			first = .FALSE.
		     ELSE
			dmin=MIN(dmin,buf1(i))
			dmax=MAX(dmax,buf1(i))
			dmin=MIN(dmin,buf2(i))
			dmax=MAX(dmax,buf2(i))
		     ENDIF
		  ENDIF
	       ENDIF
	    ENDDO
	 ENDDO
      ENDDO
      CALL xyclose(lin1)
      CALL xyclose(lin2)
      IF(dcount.EQ.MAXPTS) CALL bug('w',
     *	    'Too many points in image; only first few taken')
      CALL output('Number of pixels selected: '//itoaf(dcount))
      WRITE(line,'(''Image min and max values are:'',1p2e18.6)')
     *		    dmin,dmax
      CALL output(line)
      tolcount = 0
      DO i=1,dcount
	 diff=d1(i)-d2(i)
	 IF(ABS(diff).GT.tol) tolcount=tolcount+1
      ENDDO
      CALL output('Number of pixles differing by more than tol: '//
     *	itoaf(tolcount))
c
c  Write the data to a file if log option is present
c
      IF (dolog) THEN
	 CALL output('Writing to logfile '//logfile)
	 CALL txtopen(llog,logfile,'new',iostat)
	 IF(iostat.NE.0) THEN
	    CALL bug('i','Error opening logfile')
	    CALL bugno('f',iostat)
	 ENDIF
	 DO i=1,dcount
	    WRITE(line,'(2(E18.10,1X))') d1(i),d2(i)
	    CALL txtwrite(llog,line,len1(line),iostat)
	    IF(iostat.NE.0) THEN
	       CALL bug('i','Error writing to logfile')
	       CALL bugno('f',iostat)
	    ENDIF
	 ENDDO
	 CALL txtclose(llog)
      ENDIF
c
c  Simple plot of the data from in1 and in2
c
      dminv=dmin
      dmaxv=dmax
      dminh=dmin
      dmaxh=dmax
      IF (dolimx) THEN
	  dminh=limx(1)
	  dmaxh=limx(2)
      ENDIF
      IF (dolimy) THEN
	  dminv=limy(1)
	  dmaxv=limy(2)
      ENDIF
      IF (doplot) THEN
	 IF(pgbeg(0,device,1,1).NE.1)THEN
	    CALL pgldev
	    CALL bug('f','Error opening device '//device)
	 ENDIF
	 CALL pgenv(dminh,dmaxh,dminv,dmaxv,0,1)
	 CALL pglab(in1,in2,'IMCMP: comparing two images')
	 CALL pgpt(dcount,d1,d2,1)
	 CALL pgend
      ENDIF
c
      END
