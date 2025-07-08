      PROGRAM imsharp
c-----------------------------------------------------------------------
c  History:
c     03aug91 pjt   created out of haste, no region= processing done
c     04aug91 pjt   using HEADCOPY now, readied for submission
c     06mar92 pjt   appeased mjs: got rid of unused line
c     11mar93 pjt   maxnax.h
c-----------------------------------------------------------------------
c
c= imsharp - Sharpen an image
c& pjt
c: map analysis
c+
c     IMSHARP is a MIRIAD task which sharpens an image. No sub region can
c     be selected, the whole image is processed. Masking information
c     is properly processed though: if any of the associated pixels has 
c     been masked out, so will be the corresponding pixel in the output 
c     image.
c     Other methods to sharpen images: use SMOOTH with norm=0.0 and
c     divide the smoothed map by the original map using MATHS (unsharp
c     masking). Also: Shift or rotate an image, and subtract it from
c     the original. Good for (non)axysymmetric features resp.
c@ in
c     The input image. No default.
c@ out
c     The output image. No default.
c     Note: Nasty things happen to 4D+ datasets.
c@ method
c     Select the method to use to sharped an image.
c     Currently only simple 2D laplacian derivatives.
c-----------------------------------------------------------------------
      INCLUDE 'maxdim.h'
      INCLUDE 'maxnax.h'
c
      CHARACTER  PVERSION*(*)
      PARAMETER (PVERSION='Version 1.0 12-mar-93')
c
      CHARACTER file*132, outfile*132, method*20
      INTEGER   nsize(MAXNAX)
      INTEGER   i,j,k, nx, ny, nz, voldim
      INTEGER   naxis,lun,lout
      REAL      buf(MAXDIM,3), obuf(MAXDIM), d1, d2, d3, d4, dmin, dmax
      LOGICAL   flg(MAXDIM,3), oflg(MAXDIM), first
      LOGICAL   keyprsnt
c-----------------------------------------------------------------------
c  Announce
      CALL output( 'IMSHARP: '//PVERSION)
c
c  Get user inputs
c
      CALL keyini
      CALL keyf('in',file,' ')
      CALL keya('out',outfile,' ')
      IF(keyprsnt('method')) THEN
         CALL keya('method',method,' ')
         CALL bug('w','Only laplacian derivatives method implemented')
      ENDIF
      CALL keyfin
c
c  Open files for in and output, check if buffers large enough, and
c  copy all header variables. We're not changing anything here to the
c  header.
c
      IF (file.EQ.' ') CALL bug('f',
     *          'No in= dataset specified')
      IF (outfile.EQ.' ') CALL bug('f',
     *          'No out= dataset specified')
c
      CALL xyopen(lun,file,'old',MAXNAX,nsize)
      IF(nsize(1).GT.MAXDIM)call bug('f','Image too big [MAXDIM]')
      CALL rdhdi(lun,'naxis',naxis,0)
      CALL xyopen(lout,outfile,'new',naxis,nsize)
c
      nx = nsize(1)
      ny = nsize(2)
      nz = nsize(3)

c     // check if poor users dataset was really chopped in size
      IF (naxis.GT.3) THEN
         voldim=1
         DO i=4,naxis
            voldim = voldim*nsize(i)
         ENDDO
         IF (voldim.GT.1) THEN
            CALL bug('w','naxis>3: Can only handle the first cube')
         ELSE IF (voldim.LE.0) THEN
            CALL bug('w','Weird: some naxisI values 0?')
         ENDIF
      ENDIF

c     Copy the header.
      CALL headcp(lun, lout, 0, 0, 0, 0)
c
c  Loop over the image/cube (NOTE: can only handle up to 3D)
c
      first = .TRUE.
      DO k=1,nz
c        // write first line always as bad 
         DO i=1,nx
            obuf(i) = 0.0
            oflg(i) = .FALSE.
         ENDDO
         CALL xywrite(lout,1,obuf)
         CALL xyflgwr(lout,1,oflg)
c        // read line 1 and 2 of this plane
         CALL xyread(lun,1,buf(1,1))
         CALL xyflgrd(lun,1,flg(1,1))
         CALL xyread(lun,2,buf(1,2))
         CALL xyflgrd(lun,2,flg(1,2))
c        // run through all lines, except the last one
         DO j=2,ny-1
            CALL xyread(lun,j+1,buf(1,3))
            CALL xyflgrd(lun,j+1,flg(1,3))
            obuf(1) = 0.0
            obuf(nx) = 0.0
            oflg(1) = .FALSE.
            oflg(nx) = .FALSE.
c           // compute the laplacian
            DO i=2,nx-1
                oflg(i) = flg(i,2) .AND. flg(i-1,2) .AND. flg(i+1,2)
     *                    .AND. flg(i,1) .AND. flg(i,3)
                IF (oflg(i)) THEN
                    d1 = buf(i,2) - buf(i-1,2)
                    d2 = buf(i,2) - buf(i+1,2)
                    d3 = buf(i,2) - buf(i,1)
                    d4 = buf(i,2) - buf(i,3)
                    obuf(i) = SQRT(d1*d1+d2*d2+d3*d3+d4*d4)
                    IF (first) THEN
                        dmin=obuf(i)
                        dmax=obuf(i)
                        first=.FALSE.
                    ELSE
                        dmin=MIN(dmin,obuf(i))
                        dmax=MAX(dmax,obuf(i))
                    ENDIF
                ELSE
                    obuf(i) = 0.0
                ENDIF
            ENDDO
            CALL xywrite(lout,j,obuf)
            CALL xyflgwr(lout,j,oflg)
            CALL mymove(MAXDIM,nx,2,1,buf,flg)
            CALL mymove(MAXDIM,nx,3,2,buf,flg)
         ENDDO
c        // write last line always as bad
         DO i=1,nsize(1)
            obuf(i) = 0.0
            oflg(i) = .FALSE.
         ENDDO
         CALL xywrite(lout,ny,obuf)
         CALL xyflgwr(lout,ny,oflg)
      ENDDO
c
c  Write new min and max
c
      IF(first) CALL bug('f','No data processed')
      CALL wrhdr(lout,'datamin',dmin)
      CALL wrhdr(lout,'datamax',dmax)
c
c  Perform history
c
      CALL hisopen(lout,'append')
      CALL hiswrite(lout,'IMSHARP: '//PVERSION)
      CALL hisinput(lout,'IMSHARP')
      CALL hisclose(lout)
c
c  Close datasets
c
      CALL xyclose(lun)
      CALL xyclose(lout)

      END
c***********************************************************************
c mymove: slide lines down, for the sake of easier programming
c         if this get too expensive, consider using an indexor
      SUBROUTINE mymove(mx,nx,from,to,rbuf,lbuf)
      INTEGER mx, nx, from, to
      REAL    rbuf(mx,3)
      LOGICAL lbuf(mx,3)
c
      INTEGER i
        
      DO i=1,nx
        rbuf(i,to) = rbuf(i,from)
        lbuf(i,to) = lbuf(i,from)
      ENDDO

      END
c***********************************************************************

