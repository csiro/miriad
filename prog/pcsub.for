	PROGRAM pcsub
c-----------------------------------------------------------------------
c       A MIRIAD program to fit designated channels from a cube to a
c	a polynomial of desired degree by the least squares method
c	(linpack routines dgefa and dgesl) and to subtract the fit
c	continuum map so formed from the cube.
c
c       Limitations: 
c        1) only designed for 3-D images
c        2) the region input parameter should be specified only
c           with the IMAGE command, e.g.,
c
c           region=image(1,5),image(125,128)
c
c           would average and subtract planes 1:5 and 125:128
c           any specified x-y sub-regions will be ignored
c        3) Blanked pixels in the image are only partially handled.
c           If a plane is completely blank then it is ignored, however,
c           a plane that is partially blanked is not dealt with.
c	 4) No fit to a polynomial greater than 10 is permitted.
c-----------------------------------------------------------------------
c  History:
c    nebk  2may89 Original version. (AVSUB.FOR)
c    rjs  30may90 Changed call sequence to BoxInput. (AVSUB.FOR)
c    dec  28jun90 Changed to handle greater size cubes, and to output a
c		  continuum map (new keyword - cont).  Altered keya's to
c		  keyf's in the initial key reads. Increased max filename
c		  sizes. (CSUB.FOR)
c    dec  11jul90 Introduced capability to fit to any degree polynomial,
c		  up to degree 10. (PCSUB.FOR)
c    dec  14aug90 Changed from svdfit subroutine to linpack routines.
c		  Changed method of copy of header to the imframe routine
c    pjt  21jan91 conformed to new inline doc format
c    pjt  29apr91 add check if cont= exists, itoa->itoaf
c    pjt  21jul91 make it work when cont= exists
c    pjt  10mar92 consistent MAXRUNS length
c    nebk 25nov92 Copy btype to output
c    mjs  27feb93 use tmpdim.h instead of maxdim.h
c
c= pcsub - Subtract a plane (optional by polynomial fit) from a cube
c& pjt
c: map combination
c+
c   PCSUB (Polynomial Continuum Subtraction) is a MIRIAD task which
c   submits selected planes of a cube to a polynomial least squares
c   fit to make a continuum map that can be subtracted from the input 
c   cube. Optionally, if a continuum map is already known, this can
c   be subtracted also.
c@ in
c   The input image dataset. No default.
c@ out
c   The output image. For default see cont= keyword.
c@ order
c   The order of the highest term of the polynomial fit. 
c   Default is 0, i.e. a constant.
c@ region
c   Specify the channels to fit with a command of the region
c   of interest format, e.g. 
c        region=image(1,5),image(120,128)
c   This would fit and subtract channels 1:5 and 120:128 from the
c   cube.  Note that blanked planes are correctly ignored, but
c   isolated blanked pixels in a plane are not.
c@ cont
c   If ``out'' is provided, the continuum is generated from the
c   region (channels) selected.  This continuum (cont=) is subtracted from
c   each of the channels of the cube (in=) to yield an output cube (out=)
c   If ``out'' alone is provided, this is all that occurs.  
c   If ``cont'' is also provided, the continuum map is also written to 
c   the file specified, if that file did not exist yet.
c   If ``cont'' did already exist, it is assumed to be the continuum map,
c   and subtracted from the input cube. 
c   If ``cont'' alone is provided, ONLY the continuum map is written, no
c   output cube is written.
c--
c------------------------------------------------------------------------
c
      IMPLICIT none
      CHARACTER *(*) PVERSION
      PARAMETER (PVERSION='Version 1.0 25-nov-92')
      INCLUDE 'tmpdim.h'
      INTEGER MAXBOXES, MAXNAX, MAXRUNS, MAXEXP
      INTEGER MAXPOWER, MPPLUS1
      PARAMETER (MAXBOXES = 1024, MAXRUNS =3*MAXDIM, MAXNAX = 3)
      PARAMETER (MAXPOWER=10, MAXEXP=3, MPPLUS1=MAXPOWER+1)
c
      REAL inplanes,contin(MAXDIM),rline(MAXDIM)
      DOUBLE PRECISION bm(MAXPOWER+1, MAXPOWER+1), y(MAXDIM),
     *	 rope(MAXDIM*MAXCHAN), c(MAXPOWER+1),
     *	 cm(MAXDIM, MAXPOWER+1)
      INTEGER nruns, runs(3,MAXRUNS), planes(MAXCHAN), nplanes,
     *   xblc, xtrc, yblc, ytrc, boxes(MAXBOXES), blc(MAXNAX), 
     *   trc(MAXNAX), nin(MAXNAX), cnin(MAXNAX), i, j, k, naxis,
     *   lin, lout, lcont, istart(MAXCHAN), iend(MAXCHAN), nsect,
     *   isnext, nin1, nin2, nin3, nterms, b(MPPLUS1+MPPLUS1),
     *	 a(MAXPOWER+1), iostat, len1, n
      CHARACTER in*128, out*128, aline*72, cont*128, mesg*96,
     *	 msg*96, type*16, descr*16, itemname*16
      LOGICAL more, ccreate
c
c Announce:
c
      CALL output ('PCSUB: '//PVERSION)
      CALL bug('w','Use this program only in EXISTING cont= mode')
c
c Get the input parameters.
c
      CALL keyini
      CALL keyf ('in', in, ' ')
      CALL keyf ('out', out, ' ')
      CALL keyf ('cont', cont, ' ')
      IF (in .EQ. ' ') CALL bug ('f',
     *    'You must specify an input dataset (in=)')
      IF (out .EQ. ' ' .AND. cont .EQ. ' ') CALL bug ('f',
     *    'Some form of output image is needed, i.e. out= or cont=')
      CALL boxinput ('region', in, boxes, MAXBOXES)
      CALL keyi ('order', nterms, 0)
      IF (nterms .GT. MAXPOWER) CALL bug('f', 
     *    'Order too high; order=')
      nterms=nterms+1	
      CALL keyfin
c
c  Open the input image and pass some information to the box routines
c  about the region to work on
c
      CALL xyopen (lin, in, 'old', MAXNAX, nin)
      IF (nin(1) .GT. MAXDIM .OR. nin(2) .GT. MAXDIM)
     *     CALL bug ('f','Inpout Image too big')
      CALL rdhdi (lin, 'naxis', naxis, 0)
      IF (naxis .LT. 3) CALL bug ('f', 'Image only has 2 dimensions')
      CALL boxmask (lin, boxes, MAXBOXES)
      CALL boxset (boxes, MAXNAX, nin,'s')
c
c  Find region of image which contains all channels to average
c  and then work out which planes to read
c
      CALL boxinfo (boxes, MAXNAX, blc, trc)
      nplanes = 0
      DO k = blc(3), trc(3)
         CALL boxruns (1, k, ' ', boxes, runs, MAXRUNS,
     *                  nruns, xblc, xtrc, yblc, ytrc)
         IF (nruns .NE. 0) THEN
            nplanes = nplanes + 1
            IF (nplanes .GT. MAXCHAN)
     *         CALL bug ('f', 'Too many channels to average')
            planes(nplanes) = k
         ENDIF
      ENDDO
      IF (nplanes .LE. nterms .AND. nterms .NE. 1) THEN
	 CALL bug('w','Fit likely poor,')
	 CALL output('since the number of planes chosen as a')
	 CALL output('continuum to be fit is less than or equal to')
	 CALL output('the number of terms in the fit polynomial.')
      ENDIF
c
c  Create the output image, copy the header keywords from the 
c  input image and add the new history
c
      nin1=nin(1)
      nin2=nin(2)
      nin3=nin(3)
      cnin(1)=nin1
      cnin(2)=nin2
      cnin(3)=1
      IF (out .NE. ' ') THEN
         CALL xyopen (lout, out, 'new', MAXNAX, nin)
         CALL output('Creating continuum subtracted dataset: '//out)
      ELSE
         lout = 0
         CALL bug('i','No continuum subtracted dataset created')
      ENDIF
      IF (cont .NE. ' ') THEN
         IF (nterms .NE. 1) THEN
	    cnin(3)=nin3
	 ENDIF
c              Dataset existence test: use hopen????? Is that OK
         CALL hopen(lcont,cont,'old',iostat)
         ccreate = (iostat.NE.0)
         IF (ccreate) THEN
            CALL output('Creating continuum map: '//cont)
            CALL xyopen (lcont, cont, 'new', MAXNAX, cnin)
         ELSE
            CALL output('Reading existing continuum map: '//cont)
            CALL hclose(lcont)
            CALL xyopen (lcont, cont, 'old', MAXNAX, cnin)
            IF (nterms.NE.1) nterms = 1
            IF(cnin(1).NE.nin1) CALL bug('f','Bad X-size of cont= map')
            IF(cnin(2).NE.nin2) CALL bug('f','Bad Y-size of cont= map')
         ENDIF
      ELSE
         lcont=0
         CALL bug('i','No continuum map created')
      ENDIF
c
c   Do all necessary header and history manipulation
c
      IF (lout .NE. 0) THEN
	 CALL imhdcopy(lin, lout)
	 CALL hisopen (lout, 'append')
         CALL hiswrite (lout, 'PCSUB (MIRIAD)')
	 aline = 'PCSUB: in = '//in
	 CALL hiswrite (lout, aline)
         aline = 'PCSUB: (output) out = '//out
	 CALL hiswrite (lout, aline)
	 IF (ccreate) THEN
	    aline = 'PCSUB: creating cont = '//cont
	 ELSE
	    aline = 'PCSUB: using cont = '//cont
	 ENDIF
	 CALL hiswrite (lout, aline)
         IF(ccreate) THEN
            WRITE(mesg, '(''PCSUB: order of polynomial = '',I3)')  
     *         nterms-1
            CALL hiswrite (lout, mesg)
         ENDIF
      ENDIF
c
c If continuum is created from a polynomial fit, do some creation
c work on the header and history
c Who knows if this works...
c
      IF (ccreate) THEN
	  CALL imhdcopy(lin, lcont)
	  itemname='naxis   '
	  CALL wrhdi(lcont,itemname,2)
c
c	  Delete all unapplicable header entries
c
	  mesg='crpix3  '
	  CALL hdelete(lcont,mesg,iostat)
	  msg='xx  xxx'
	  IF (naxis .GT. 2) THEN
	    DO i=3,naxis
	      WRITE(mesg, '(''cdelt'', I1)') i
c	      mesg=mesg(1:len1(mesg))//msg(3:4)
	      CALL hdelete(lcont,mesg,iostat)
	      IF (iostat .NE. 0) THEN
	        CALL bug('i','Error deleting item '//
     * 				mesg(1:len1(mesg)))
c	        CALL bugno('f',iostat)
	      ENDIF
	      WRITE(mesg, '(''crota'', I1)') i
c	      mesg=mesg(1:len1(mesg))//msg(3:4)
	      CALL hdelete(lcont,mesg,iostat)
	      IF (iostat .NE. 0) THEN
	        CALL bug('i','Error deleting item '//
     * 				mesg(1:len1(mesg)))
c	        CALL bugno('f',iostat)
	      ENDIF
	      WRITE(mesg, '(''crval'', I1)') i
c	      mesg=mesg(1:len1(mesg))//msg(3:4)
	      CALL hdelete(lcont,mesg,iostat)
	      IF (iostat .NE. 0) THEN
	        CALL bug('i','Error deleting item '//
     * 				mesg(1:len1(mesg)))
c	        CALL bugno('f',iostat)
	      ENDIF
	      WRITE(mesg, '(''ctype'', I1)') i
c	      mesg=mesg(1:len1(mesg))//msg(3:4)
	      CALL hdprobe(lcont,mesg,descr,type,n)
	      IF (type .NE. 'nonexistent') THEN
	        CALL hdelete(lcont,mesg,iostat)
	        IF (iostat .NE. 0) THEN
	          CALL bug('i','Error deleting item '//
     * 				mesg(1:len1(mesg)))
c	          CALL bugno('f',iostat)
	        ENDIF
	      ENDIF
	    ENDDO
	  ENDIF
	  CALL hisopen(lcont, 'append')
	  CALL hiswrite(lcont, 'PCSUB (MIRIAD) Continuum Creation')
	  aline = 'PCSUB: in = '//in
	  CALL hiswrite(lcont, aline)
	  aline = 'PCSUB: out = '//out
	  IF (lout .NE. 0) THEN
		CALL hiswrite(lcont, aline)
	  ENDIF
      ENDIF
c	
      CALL listcom (nplanes, planes, istart, iend, nsect)
      isnext = 1
      more = .true.
      DO while (more)
          CALL txtplane (nsect, istart, iend, aline, isnext, more)
          IF (lout .NE. 0) CALL hiswrite (lout, aline)
	  IF (ccreate) CALL hiswrite (lcont,aline)
      ENDDO
      IF (lout .NE. 0) CALL hisclose (lout)
      IF (ccreate) CALL hisclose (lcont)
c
c	Select a line of attack going into the cube and fit it
c	to a polynomial, using in the fit only those planes
c	selected by input as past of the continuum.  Then,
c	subtract the polynomial fit from each point along the
c	line of attack (parallel to the z-axis.)
c
      IF (nterms .NE. 1) THEN
	  DO j=1,nin2
	    CALL polfits(planes,nplanes,nterms,lin,lout,lcont,
     *				nin1, nin3, j, MAXDIM, MAXCHAN,
     *				MPPLUS1, MAXEXP, bm, c, b, a,
     *				y, rope, cm, rline, contin)
	  ENDDO
      ELSE
c
c	 In this case where the fit is zero order, create for each pixel
c	 the average of its intensity over all the channels selected
c	 as part of the continuum to be fit.  Then, subtract this average
c	 from each plane of the cube at that pixel and deposit the result
c	 as part of the output cube.
c        ==> This mode is also use when the continuum map already existed <==
c
	 inplanes=1.0/REAL(nplanes)
	 DO j=1,nin2
c           
            IF (ccreate) THEN
	       DO i=1,nin1
	          contin(i)=0.0
	       ENDDO
	       DO k=1,nplanes
	          CALL xysetpl(lin,1,planes(k))
	          CALL xyread(lin,j,rline)
                  DO i=1,nin1
	             contin(i)=contin(i)+rline(i)
	          ENDDO
	       ENDDO
	       DO i=1,nin1
	          contin(i)=contin(i)*inplanes
	       ENDDO
	       CALL xywrite(lcont,j,contin)
            ELSE
               CALL xyread(lcont,j,contin)
            ENDIF

c  from above:
c	    If there is to be a separate continuum output file, whether
c	    or not a new cube will be generated, then divide the line of
c	    channel pixel summations (one row of pixels analyzed through
c	    the selected planes) by the number of channels summed,
c	    and add the resulting row of the continuum to the output
c	    continuum file, cont.
c

c           Now, 
c	    If there is to be an output cube, take the line of the
c	    continuum, just made, and subtract it from the corresponding
c	    line of the input cube (over all channels) to make a line for
c	    the output cube.
c
	    IF (lout .NE. 0) THEN
	      DO k=1,nin3
	    	CALL xysetpl(lin,1,k)
	    	CALL xysetpl(lout,1,k)
	    	CALL xyread(lin,j,rline)
	    	DO i=1,nin1
		  rline(i)=rline(i)-contin(i)
	  	ENDDO
		CALL xywrite(lout,j,rline)
	      ENDDO
	    ENDIF
	 ENDDO
      ENDIF
c
c ----------------------------------------------------------------
c
c  Close up the shop
c
      CALL xyclose (lin)
      IF (lout .NE. 0) CALL xyclose (lout)
      IF (ccreate) CALL xyclose (lcont)
      END
c
c----------------------------------------------------------------------
        SUBROUTINE listcom (nplanes, planes, istart, iend, nsect)
c----------------------------------------------------------------------
c       Compact a list of planes such as: 1,2,3,4,7,10,12,13,14 to
c       a two lists giving the start and END planes of each section
c       for ease of formatting in history
c
c    Input:
c      nplanes   size of planes() array
c      planes    list of planes
c    Output:
c      istart    lists of start and END planes for each section
c      iend           e.g. if planes=1,2,3,5,7,8,9
c                        istart=1,5,7
c                        iend  =3,5,9
c      nsect     number of sections, for above example, nsect=3
c-------------------------------------------------------------------
      IMPLICIT none
      INTEGER nplanes, planes(nplanes), istart(nplanes), 
     *  iend(nplanes), nsect
cc
      INTEGER i
c-------------------------------------------------------------------
      nsect = 1
c
      DO i = 1, nplanes
          IF (i .EQ. 1) THEN
            istart(nsect) = planes(1)
          ELSE IF (planes(i).ne.(planes(i-1)+1)) THEN
            iend(nsect) = planes(i-1)
            nsect = nsect + 1
            istart(nsect) = planes(i)
          ELSE 
            IF (i .EQ. nplanes) iend(nsect) = planes(i)
          ENDIF
      ENDDO
c
      END
c
c
      SUBROUTINE txtplane (nsect, istart, iend, string, isnext, more)
c----------------------------------------------------------------------
c       Write the compacted plane list into a text string
c
c   Input:
c     nsect      number of sections in LIST
c     istart     list of start planes for each section
c     iend       list of end planes for each section
c   Output:
c     string     output string
c   Input/Output:
c     isnext     The number of the next section to start the next line
c     more       do it again please if true
c----------------------------------------------------------------------
        IMPLICIT none
c
        INTEGER nsect, istart(nsect), iend(nsect), isnext
        CHARACTER string*(*)
        LOGICAL more
cc
        INTEGER strlen, l1, l2, ipt, i
        CHARACTER*4 ch1, ch2
c------------------------------------------------------------------
        string = 'PCSUB: channels fit to polynomial = '
        ipt = 29
        strlen = len(string)
        DO i = isnext, nsect
          IF (istart(i) .EQ. iend(i)) THEN
            CALL itochar (istart(i), ch1, l1)
            IF (ipt+l1.lt.strlen) THEN
              string(ipt:ipt+l1) = ch1(1:l1)
            ELSE
              more = .TRUE.
              isnext = i
              GOTO 999
            ENDIF
            ipt = ipt + l1 + 2
          ELSE
            CALL itochar (istart(i), ch1, l1)
            CALL itochar (iend(i), ch2, l2)
            IF (ipt+l1+l2.lt.strlen) THEN
              string(ipt:ipt+l1+l2) = ch1(1:l1)//':'//ch2(1:l2)
              ipt = ipt + l1 + l2 + 2
            ELSE
              more = .TRUE.
              isnext = i
              GOTO 999
            ENDIF
          ENDIF
        ENDDO
        more = .FALSE.
c
 999    RETURN
        END
c            
c
        SUBROUTINE itochar (ival, string, ilen)
c-----------------------------------------------------------------------
c       Encode an integer into a character string. 
c
c     Input:
c        ival      i      Integer to encode
c     Output:
c        string    c(*)   String in which integer is placed.
c        ilen      i      Length of encoded part of string
c-----------------------------------------------------------------------
        IMPLICIT none
c
        INTEGER ival, ilen
        CHARACTER*(*) string
cc
        CHARACTER itoaf*20, temp*20
        INTEGER len1
c-----------------------------------------------------------------------
        temp = itoaf(ival)
        ilen = len1(temp)
        string = temp(1:ilen)
c
        END
c
c-----------------------------------------------------------------------
	SUBROUTINE polfits(planes,nplanes,nterms,lin,lout,lcont,
     *				nin1, nin3, j, maxdim, maxchan,
     *				mpplus1, maxexp, bm, c, b, a, y,
     *				rope, cm, rline, contin)
c-----------------------------------------------------------------------
c
c	Fit and subtract a continuum map. Create arrays involved.
c
c	Input:
c	  maxdim, maxchan, mpplus1, maxexp -- Maximum values for the
c	    sizes of a dimension, of the number of channels, of the
c	    order plus one
c	  nplanes -- number of planes selected for the continuum fit.
c	  planes --- list of planes selected for the continuum fit.
c	  c -- list of coefficients for a line number i, going into
c	    the cube.
c	  cm - two dimensional array accumulating the c arrays for each
c	    frontal pixel position on the line of interest.
c	  bm - the matrix, created in the outfit routine.
c	  nin1, nin3 -- dimensions of two of the axes -- x, z.
c	  j -- the line of the cube front that is now being fit
c	    for each line starting from a point on j, and heading
c	    deep into all the planes of the cube.
c	  lin, lout, lcont -- the handles for the in, out, and continuum
c	    files.
c	  rope -- A long array made of the row j for each plane, where
c	    each read of the line on a new plane is placed at the end
c	    of the rope consecutively.
c	  rline -- The line that is read from a plane of the input cube
c	    or written to the output cube.
c	  contin - The line that is written to an output continuum map.
c
c-----------------------------------------------------------------------
c
	INTEGER maxdim, maxchan, mpplus1, maxexp, nplanes, lin, lout,
     *		lcont, j, planes(maxchan), nterms, nin1, nin3,
     *		b(mpplus1+mpplus1)
c
	INTEGER a(mpplus1)
	REAL rline(maxdim), contin(maxdim)
	DOUBLE PRECISION bm(mpplus1, mpplus1), c(mpplus1), y(maxdim)
	DOUBLE PRECISION rope(maxdim*maxchan), cm(maxdim, mpplus1)
c
	INTEGER i, k, m, n, expre
c
c	Form array (rope) of lines (k,j),(k+1,j),(k+2,j)... placed
c		end to end.
	DO k=1,nplanes
	  CALL xysetpl(lin,1,planes(k))
	  CALL xyread(lin,j,rline)
	  DO i=1,nin1
	    rope((k-1)*nin1+i)=rline(i)
	  ENDDO
	ENDDO
c
	DO i=1,nin1
c	  Make array (y) of (k-1)*nin1+i positions on rope array, getting
c	  a line array parallel to the z axis and nplanes in length.
	  DO k=1,nplanes
	    y(k)=rope((k-1)*nin1+i)
	  ENDDO
c	  Create arrays and perform fit for this y(*) going into the cube
c	  at position (i,j)
	  CALL outfit(mpplus1, maxdim, maxexp, maxchan, nterms,
     *			nplanes, planes, bm, y, a, b, c)
c	  Transfer coefficients of polynomial from fit routine and place
c	  them in the 2-D grid of coefficients (Position-on-Rline versus
c	  Coefficient-Number.)
	  DO n=1,nterms
	    cm(i,n)=c(n)
	    c(n)=0.0
	  ENDDO
	ENDDO
c
c	Subtract the continuum from the image.
c
	DO k=1,nin3
	  CALL xysetpl(lin,1,k)
	  IF (lout .NE. 0) CALL xysetpl(lout,1,k)
	  IF (lcont .NE. 0) CALL xysetpl(lcont,1,k)
	  CALL xyread(lin,j, rline)
	  DO i=1,nin1
	    contin(i)=0.0
	    DO n=1,nterms
		expre=1
		IF (n .GT. maxexp) THEN
c		  Form the x**n term, run by run, by which each specific
c		  coefficient is to be multiplied by for given i and
c		  Coefficient-Number)
		  DO m=1,(n-1)
		    expre=expre*k
		  ENDDO
		ELSE
		  IF (n .GT. 1) THEN
		    expre=k**(n-1)
		  ENDIF
		ENDIF
c		Subtract each coefficient*x**n term of each i from
c			rline(i)
		rline(i)=rline(i)-cm(i,n)*DBLE(expre)
		IF (lcont .NE. 0) THEN
		  contin(i)=contin(i)+cm(i,n)*DBLE(expre)
		ENDIF
	    ENDDO
	  ENDDO
	  IF (lout .NE. 0) CALL xywrite(lout,j,rline)
	  IF (lcont .NE. 0) CALL xywrite(lcont,j,contin)
	ENDDO
c
	END
c
c------------------------------------------------------------------------------------
c
	SUBROUTINE outfit(mpplus1, maxdim, maxexp, maxchan, nterms,
     *			nplanes, planes, bm, y, a, b, c)
c
c	Create a matrix (bm) and send it to double precision linpack subroutines
c------------------------------------------------------------------------------------
c
	INTEGER maxdim, maxexp, maxchan, nterms, nplanes,
     *		planes(maxchan), mpplus1, a(mpplus1)
	DOUBLE PRECISION bm(mpplus1, mpplus1), y(maxdim), c(mpplus1)
c
	INTEGER i, j, k, info, b(mpplus1+mpplus1)
c
c	Reset c and bm (the matrices)
c
	DO i=1,nterms
	  DO j=1,nterms
	    bm(i,j)=0.0
	  ENDDO
	  c(i)=0.0
	ENDDO
c
c	Create matrix bm and array c to be sent to the linpack fit
c
	DO k=1,nplanes
c	  Create array of planes**0 ... planes**(2*nterms-1)
c	  with which to work to create bm
	  b(1)=1
	  DO i=2,(nterms+nterms-1)
	    b(i)=b((i-1))*planes(k)
	  ENDDO
c	  Create bm and c
	  DO i=1,nterms
	    DO j=1,nterms
		bm(i,j)=bm(i,j)+DBLE(b(i+j-1))
	    ENDDO
	    c(i)=c(i)+y(k)*DBLE(b(i))
	  ENDDO
	ENDDO
c
c	Call linpack matrix transposition and fit subroutines
c
	CALL dgefa(bm, mpplus1, nterms, a, info)
	IF (info .NE. 0) CALL bug('f', 'Highly bogus division by zero')
	CALL dgesl(bm, mpplus1, nterms, a, c, 0)
c
	END
c
c***********************************************************************   
      SUBROUTINE imhdcopy (lin,lout)
      INTEGER lin, lout
c
c  Copy all official Image Header keywords.
c     lin   input file pointer
c     lout  output file pointer
c
c-----------------------------------------------------------------------
      INTEGER NKEYS
      PARAMETER(NKEYS=48)
      CHARACTER keyw(NKEYS)*8
      INTEGER   i
      DATA keyw/   'bunit   ','crota1  ','crota2  ','crota3  ',
     *  'crota4  ','crota5  ','crval1  ','crval2  ','crval3  ',
     *  'crval4  ','crval5  ','ctype1  ','ctype2  ','ctype3  ',
     *  'ctype4  ','ctype5  ','date-obs','epoch   ','history ',
     *  'instrume','niters  ','object  ','telescop','observer',
     *  'restfreq','vobs    ','xshift  ','yshift  ','obsra   ',
     *  'obsdec  ','cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ',
     *  'cdelt5  ','crpix1  ','crpix2  ','crpix3  ','crpix4  ',
     *  'crpix5  ','ltype   ','lstart  ','lwidth  ','lstep   ',
     *  'bmaj    ','bmin    ','bpa     ','btype   '/


      DO i=1,NKEYS
         CALL hdcopy(lin,lout,keyw(i))
      ENDDO

      END
