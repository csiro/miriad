      PROGRAM minmax
c
c= minmax - Forced write of the minimum and maximum data header values
c& pjt
c: map analysis
c+
c   MINMAX sets the DATAMIN and DATAMAX items in an image in those rare
c   cases that you need it computed from a selected area. Most commonly
c   when data have been flagged (e.g. with MATHS or IMFLAG).
c   See also PUTHD on how to patch a user supplied value for DATAMIN 
c   and/or DATAMAX.
c   If the min/max was found to be the same, nothing will be done to
c   the dataset.
c@ in
c   The input dataset name. No default.
c@ region
c   The region of interest over which the min and max are to be computed.
c   Full region specifications are supported.
c   See the Users Manual for instructions on how to specify this.
c   Default: the whole dataset
c@ minmax
c   If supplied, this is a fast way to set a new value for the min and
c   max as you want them. There is no checking if the values are
c   correct, or even if ``min < max''. 
c   Default: not used.
c--
c
c  History:
c       18jul91 pjt   Created: quick and dirty
c	26jul91	pjt   changed name from imminmax to minmax (think of that!)
c        4aug91 pjt   also wrote old minmax to history - fixed  'done' bug
c	 4sep92 pjt   fixed reporting bug (mixed up min & max)
c	12mar93 pjt   maxnax.h now
c	 8jun94 pjt   clarified region=
c-----------------------------------------------------------------------
      INCLUDE 'maxdim.h'
      INCLUDE 'maxnax.h'

      CHARACTER PVERSION*(*)
      PARAMETER (PVERSION='Version 1.0 12-mar-93')
      INTEGER MAXBOXES,MAXRUNS
      PARAMETER(MAXBOXES=2048)
      PARAMETER(MAXRUNS=3*MAXDIM)
c
      CHARACTER infile*80,line*80
      INTEGER   nsize(MAXNAX),plane(MAXNAX), blc(MAXNAX),trc(MAXNAX)
      INTEGER   i,j,k, naxis,lun, pcount
      INTEGER   boxes(MAXBOXES),runs(3,MAXRUNS),nruns
      REAL      dat(MAXDIM),rmax,rmin,x,ormin,ormax
      LOGICAL   first, done, keyprsnt
c
c  announce to the world
c
      CALL output('MINMAX: '//PVERSION)
c
c  get user inputs
c
      CALL keyini
      CALL keyf('in',infile,' ')
      IF(infile.EQ.' ')CALL bug('f','No input dataset given (in=)')
      IF (keyprsnt('minmax')) THEN
         CALL keyr('minmax',rmin,0.0)
         IF (.NOT.keyprsnt('minmax')) CALL bug('f',
     *              'You must supply TWO values for minmax=')
         CALL keyr('minmax',rmax,0.0)
         IF (keyprsnt('region')) CALL bug('w',
     *              'New minmax supplied, region= ignored')
         done = .TRUE.
      ELSE
         CALL boxinput('region',infile,boxes,MAXBOXES)
	 done = .FALSE.
      ENDIF
      CALL keyfin

c
c  Always open the dataset first
c
      CALL xyopen(lun,infile,'old',MAXNAX,nsize)

      IF (.NOT.done) THEN
c
c  If data needs to be read, see if buffers large enough
c
        CALL rdhdi(lun,'naxis',naxis,0)
        naxis = MIN(naxis,MAXNAX)
        IF(nsize(1).GT.MAXDIM) CALL bug('f','Input data too big for me')
c
c  Set up the region of interest
c
        CALL boxmask(lun,boxes,MAXBOXES)
        CALL boxset(boxes,MAXNAX,nsize,' ')
        CALL boxinfo(boxes,MAXNAX,blc,trc)
c
c  Initialise.
c
        first = .TRUE.
	pcount = 0
        DO i=1,MAXNAX
          plane(i) = blc(i)
        ENDDO
c
c  Loop over the data, finding min/max 
c
	DOWHILE(.not.done)
	  CALL boxruns(maxnax-2,plane(3),' ',boxes,runs,MAXRUNS,nruns,
     *					blc(1),trc(1),blc(2),trc(2))
	  CALL xysetpl(lun,MAXNAX-2,plane(3))
	  j = 0
	  DO k=1,nruns
	    IF(runs(1,k).NE.j)THEN
	      j = runs(1,k)
	      CALL xyread(lun,j,dat)
	    ENDIF
	    IF(first)THEN
	      rmax = dat(runs(2,k))
	      rmin = rmax
	      first = .FALSE.
	    ENDIF
	    DO i=runs(2,k),runs(3,k)
              pcount = pcount + 1
	      x = dat(i)
	      IF(x.GT.rmax)THEN
	        rmax = x
	      ELSE IF(x.LT.rmin)THEN
	        rmin = x
	      ENDIF
	    ENDDO
	  ENDDO
	  CALL planeinc(MAXNAX-2,blc(3),trc(3),plane(3),done)
        ENDDO
c
c Tell the viewers
c
        WRITE(line,
     *  '(''Processed '',I10,'' pixels:'')') pcount
        CALL output(line)
      ENDIF

c
c Update appropriate header and history item(s) if new min/max found
c

c     // Get old header min/max
      CALL rdhdr(lun,'datamin',ormin,0.0)
      CALL rdhdr(lun,'datamax',ormax,0.0)
      WRITE(line,
     *  '(''Old minimum is '',1PE18.6,'' old maximum is '',1PE18.6)')
     *  ormin, ormax
      CALL output(line)

c     // Act according to change

      IF(rmin.EQ.ormin .AND. rmax.EQ.ormax) THEN
        CALL bug('i','Headers not updated, no change in min/max')
      ELSE
        WRITE(line,
     *  '(''New minimum is '',1PE18.6,'' new maximum is '',1PE18.6)')
     *  rmin, rmax
        CALL output(line)
        CALL wrhdr(lun,'datamin',rmin)
        CALL wrhdr(lun,'datamax',rmax)
        CALL hisopen(lun,'append')
        CALL hiswrite(lun,'MINMAX: Miriad MinMax: '//PVERSION)
        CALL hisinput(lun,'MINMAX')
        WRITE(line,'(''Old min/max: '',1PE18.6,1x,1PE18.6)')ormin,ormax
        CALL hiswrite(lun,'MINMAX: '//line)
        CALL hisclose(lun)
        CALL xyclose(lun)
      ENDIF

      END
c***********************************************************************
      SUBROUTINE copyindx(n,from,to)
      INTEGER n,from(n),to(n)
c-----------------------------------------------------------------------
      INTEGER i
      DO i=1,n
	  to(i) = from(i)
      ENDDO
      END
c***********************************************************************
      SUBROUTINE planeinc(n,blc,trc,plane,done)
c
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
