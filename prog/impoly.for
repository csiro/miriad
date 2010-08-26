      PROGRAM impoly

c= impoly - Flat field subtraction using a 2D polynomial fit
c& pjt
c: map combination
c+
c       IMPOLY (IMage POLYnomial subtraction) is a MIRIAD task that
c       performs a least squares polynomial fit to a specified region
c       of each plane of a cube and subtracts this fit from the entire
c       plane.
c@ in
c       The input image.  No default.
c@ out
c       The output image.  No default.
c@ order
c       The degree of the highest term of the polynomial fit.  It is not
c       possible to have a different order of the polynomial in X and Y.
c       Default is zero (i.e. fit a constant).
c@ coeffs
c       A logical, specifying whether you want to see fitted polynomial
c       coefficients.  Note: the coefficients are defined w.r.t. the
c       reference pixel, and are in pixel units.  Default is not.
c@ region
c       Full region specifications are supported.  Defaults to the
c       entire image.
c
c$Id: impoly.for,v 1.2 2010/08/26 02:38:39 cal103 Exp $
c--
c
c       maxorder        maximum order allowed for the polynomial fit
c       maxexp          maximum power of an x or y term that will be
c                       multiplied out with successive *'s instead of
c                       one "**"; this value is for efficiency
c       maxboxes        maximum number of boxes for box routines
c       maxnax          maximum number of axes of image cube
c       maxlen          maximum number of coefficients, including cross
c                       terms
c       maxruns         maximimum number of runs across polygonal image
c       iwork           work array for work with matrices in fit
c                       routines
c       nlen            length of array of coefficients, including cross
c                       terms
c       a               array of x**n * y**p terms in correct order
c       sa              array of intensity(i) * a(i) terms
c       m(*,*), sm(*)   matrix arrays for polynormial fit
c       jread           used in deciding if we read in a line from the
c                       cube
c       len1            function to find the length of a string
c
c  HISTORY:
c       dec     10-aug-90       Construction of program (David Coleman)
c       dec     29-aug-90       Added rms calculation for when coeffs=y
c       pjt     11-jun-91       Inline doc - readied for submission
c       pjt     21-feb-92       use hisinput(), still uses imhdcopy() !!
c       pjt     10-mar-92       consistent use of MAXRUNS
c       nebk    25-nov-92       Copy btype to output
c       pjt      8-jun-94       region= clarification
c       rjs     02-jul-97       cellscal change.
c       rjs     10-nov-97       Correct handling of 3rd axis and minor
c                               improvement to the documentation.
c       rjs     08-may-00       Change incorrect call to keyf to keya.
c
c-----------------------------------------------------------------------
      INCLUDE 'maxdim.h'

      INTEGER MAXBOXES, MAXNAX, MAXRUNS, MAXORDER, MAXLEN, MAXEXP
      CHARACTER PVERSION*(*)
      PARAMETER (MAXBOXES=1024, MAXNAX=3, MAXRUNS=3*MAXDIM)
      PARAMETER (MAXORDER=10, MAXEXP=3)
      PARAMETER (MAXLEN=(((MAXORDER+1)*(MAXORDER+2))/2)+MAXORDER+1)
      PARAMETER (PVERSION='Version 25-nov-92')

      INTEGER boxes(MAXBOXES), xblc, xtrc, yblc, ytrc, runs(3,MAXRUNS)
      INTEGER nruns, nin(3), order, nlen, idx, jread, info, runnum
      INTEGER lin, lout, i, j, k, p, n, u, v, r, iwork(MAXLEN)
      INTEGER naxis
      REAL rline(MAXDIM), x, y, rms, nrms, x0, y0
      DOUBLE PRECISION a(MAXLEN), sa(MAXLEN), expxy
      DOUBLE PRECISION m(MAXLEN,MAXLEN), sm(MAXLEN)
      CHARACTER in*80, out*80, mesg*80
      CHARACTER msg*100
      LOGICAL coeffs

      INTEGER  len1
      EXTERNAL len1
c-----------------------------------------------------------------------
c     Get inputs.
      CALL output('IMPOLY: '//PVERSION)

      CALL keyini
      CALL keyf('in', in, ' ')
      IF (in.EQ.' ') CALL bug('f',
     *                'You must specify an input file (in=)')
      CALL keya('out', out, ' ')
      IF (out.EQ.' ') CALL bug('f',
     *                'You must name an output file (out=)')
      CALL keyi('order', order, 0)
      IF (order.GT.MAXORDER) CALL bug('f', 'Order is too large')
      CALL keyl('coeffs', coeffs, .FALSE.)
      CALL boxinput('region', in, boxes, MAXBOXES)
      CALL keyfin

c     Open files, copy relevant header items, and get the ref pixel.
      nlen=((order+1)*(order+2))/2
      CALL xyopen(lin, in, 'old', MAXNAX, nin)
      CALL rdhdi(lin, 'naxis', naxis, 1)
      naxis = min(naxis,MAXNAX)
      CALL xyopen(lout, out, 'new', naxis, nin)
      CALL imhdcopy(lin, lout)
      CALL boxmask(lin, boxes, MAXBOXES)
      CALL boxset(boxes, MAXNAX, nin, ' ')
      CALL rdhdr(lin,'crpix1',x0,1.0)
      CALL rdhdr(lin,'crpix2',y0,1.0)

c     Record the activities in a new history file for the output image.
      CALL hisopen(lout, 'append')
      CALL hiswrite(lout, 'IMPOLY: '//PVERSION)
      CALL hisinput(lout,'IMPOLY')

c     By plane, create and subtract a background radiation polynomial
c     fit.
      DO k = 1, nin(3)
        if (k.gt.1) then
          CALL xysetpl(lin,1,k)
          CALL xysetpl(lout,1,k)
        endif

c       Reset sm and m to zero in order to reuse them for each
c       line segment of relevant data in the region of interest
        DO u = 1, nlen
          sm(u)=0.0
          DO v = 1, nlen
              m(u,v)=0.0
          ENDDO
        ENDDO

c       Find the first line segment of the data
        CALL boxruns(1, k, ' ', boxes, runs, MAXRUNS,
     *        nruns, xblc, xtrc, yblc, ytrc)
        jread=0

c       Go through the first line segment
        DO r = 1, nruns
          j=runs(1,r)
          IF (j.GT.jread) THEN
              CALL xyread(lin,j,rline)
              jread=j
          ENDIF

c         Form matrix, including pixel values and x & y terms, to send
c         to the fitting subroutine
          y=real(j)-y0
          DO i = runs(2,r), runs(3,r)
            x=real(i)-x0

            DO p = 0, order
              DO n = 0, order-p
c               Stay with the correct index for following array orders
c               of powers and terms
                idx=((p+n)*(p+n+1))*0.5+n+1
                a(idx)=1

c               Create the x and y components of a particular term
                IF (p.GT.maxexp) THEN
                  DO u = 1, p
                    a(idx)=a(idx)*x
                  ENDDO
                ELSE
                  a(idx)=x**p
                ENDIF

                IF (n.GT.maxexp) THEN
                  DO u = 1, n
                    a(idx)=a(idx)*y
                  ENDDO
                ELSE
                  a(idx)=a(idx)*y**n
                ENDIF

c               Multiply the pixel intensity with the x and y
c               components of the particular numerical term.
                sa(idx)=a(idx)*rline(i)
              ENDDO
            ENDDO

c           Accumulate m and sm
            DO u = 1, nlen
              DO v = 1, nlen
                m(u,v)=m(u,v)+a(u)*a(v)
              ENDDO
              sm(u)=sm(u)+sa(u)
            ENDDO
          ENDDO
        ENDDO

c       Call the outfit for the usage of and preparations toward
c       the fit subroutines (dgefa, dgesl)
        CALL outfit(MAXLEN, nlen, m, sm, iwork, info)

c       Output determined polynomial coefficients if necessary
        IF (coeffs) THEN
          WRITE(mesg,
     *        '(''Coefficients of polynomial fit w.r.t.:'',2F7.1)')
     *                x0, y0
          CALL output(mesg)
          WRITE(mesg, '(''Plane #'', I3)') k
          CALL output(mesg)
          u=1
          DO p = 0, order
            mesg=' '
            v=u+p
            DO n = u, v
              write(msg, '(G15.8)') sm(n)
              mesg=mesg(1:len1(mesg))//msg(1:len1(msg))
              msg=', '
              mesg=mesg(1:len1(mesg))//msg(1:2)
            ENDDO
            u=v+1
            CALL output(mesg)
          ENDDO
        ENDIF

c       Subtract the background, according to formulae derived from
c       the polynomial fit
        runnum=1
        nrms=0.0
        rms=0.0
        DO j = 1, nin(2)
          CALL xyread(lin,j,rline)
          y=real(j)-y0

          DO i = 1, nin(1)
              x=real(i)-x0
              DO p = 0, order
                DO n = 0, order-p
                  idx=((p+n)*(p+n+1))*0.5+n+1

c                 Create the exponential parts of the particular term,
c                 to be multiplied by the coefficient from the fit
c                 subroutine.
                  expxy=1.0
                  IF (p.GT.maxexp) THEN
                    DO u = 1, p
                      expxy=expxy*x
                    ENDDO
                  ELSE
                    expxy=x**p
                  ENDIF

                  IF (n.GT.maxexp) THEN
                    DO u = 1, n
                      expxy=expxy*y
                    ENDDO
                  ELSE
                    expxy=expxy*y**n
                  ENDIF

c                 Subtract the term, as background, from the image
                  rline(i)=rline(i)-sm(idx)*expxy
                ENDDO
              ENDDO
          ENDDO
          CALL xywrite(lout,j,rline)

c         Calculation of the Root-Mean-Square, if COEFFS=Y
          IF (coeffs) THEN
              DO WHILE (runs(1,runnum).EQ.j)
                DO i = runs(2,runnum), runs(3,runnum)
                  rms=rms+rline(i)*rline(i)
                  nrms=nrms+1.0
                ENDDO
                runnum=runnum+1
              ENDDO
          ENDIF
        ENDDO

        IF (coeffs) THEN
          rms=SQRT(rms/nrms)
          write(mesg, '(''Rms of fit over region,'')')
          CALL output(mesg)
          write(mesg,'(''plane #'',I3,'', is '',G15.8)') k, rms
          CALL output(mesg)
          rms=0
          n=0
c         Shove above two lines, rms=n=0, to beginning of DO K loop for
c         both initialization and reinitialization
        ENDIF
      ENDDO

      CALL xyclose(lin)
      CALL xyclose(lout)

      END

c***********************************************************************

      SUBROUTINE outfit(maxlen,nlen,m,sm,iwork,info)

      INTEGER maxlen, nlen, info, iwork(maxlen)
      DOUBLE PRECISION m(maxlen,maxlen), sm(maxlen)
c-----------------------------------------------------------------------
c     Prepare the matrix and send to the fit subroutine.
      CALL dgefa(m, maxlen, nlen, iwork, info)
      IF (info.NE.0) CALL bug('f', 'Division by zero in DGEFA.')
      CALL dgesl(m, maxlen, nlen, iwork, sm, 0)

      END

c***********************************************************************

      SUBROUTINE imhdcopy (lin,lout)
      INTEGER lin, lout

c  Copy all official Image Header keywords, also the history
c     lin   input file pointer
c     lout  output file pointer
c-----------------------------------------------------------------------
      INTEGER NKEYS
      PARAMETER (NKEYS=47)
      CHARACTER keyw(NKEYS)*8
      INTEGER   i
      DATA keyw/   'bunit   ','crota1  ','crota2  ','crota3  ',
     *  'crota4  ','crota5  ','crval1  ','crval2  ','crval3  ',
     *  'crval4  ','crval5  ','ctype1  ','ctype2  ','ctype3  ',
     *  'ctype4  ','ctype5  ','obstime ','epoch   ','history ',
     *  'instrume','niters  ','object  ','telescop','observer',
     *  'restfreq','vobs    ','cellscal','obsra   ',
     *  'obsdec  ','cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ',
     *  'cdelt5  ','crpix1  ','crpix2  ','crpix3  ','crpix4  ',
     *  'crpix5  ','ltype   ','lstart  ','lwidth  ','lstep   ',
     *  'bmaj    ','bmin    ','bpa     ','btype   '/
c-----------------------------------------------------------------------
      DO i = 1, NKEYS
         CALL hdcopy(lin,lout,keyw(i))
      ENDDO

      END
