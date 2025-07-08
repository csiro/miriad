      PROGRAM immedian
      IMPLICIT NONE
c-----------------------------------------------------------------------
c  History:
c     17jan03 pjt   cloned off IMSHARP, Q&D for Stuart Vogel
c     23jan03 pjt   finalized
c     21feb03 pjt   removed the local (duplicated/old wrong) copy of sortr
c     25jul08 pjt   bigger default size
c      7jul11 pjt   add an nsigma keyword
c
c  TODO:
c     - implement looping over all planes in the cube
c
c-----------------------------------------------------------------------
c
c= immedian - Median filter over an image
c& pjt
c: map analysis
c+
c     IMMEDIAN is a MIRIAD task which performs a median filter over an image. 
c     No subregion can be selected, the whole image is processed. 
c     See also ELLINT for a median filter in elliptical rings. 
c     Masking information is properly processed: if any of the associated pixels has 
c     been masked out, they will not be used in the median filter.
c     Edge pixels are not filtered and passed onto the output image unchanged.
c
c@ in
c     The input image. No default.
c@ out
c     The output image. No default.
c     Note: Nasty things can happen to 4D+ datasets.
c@ size
c     Half the box size. The filtering box will be square and have
c     a size of 2*SIZE+1, and will thus always have an odd number of pixels
c     centered on the pixel to be filtered. Edge pixels are left untouched.
c@ nsigma
c     If set to a non-zero value, instead of a median, all outliers
c     more than nsigma*sigma from the mean are removed in computing
c     the filtered output value
c
c-----------------------------------------------------------------------
c
      INCLUDE 'maxdim.h'
      INCLUDE 'maxnax.h'
c
      CHARACTER  PVERSION*(*)
      PARAMETER (PVERSION='Version 1.0 7-jul-11')
      INTEGER   MAXDIM3
      PARAMETER (MAXDIM3=2048)
      INTEGER   MAXBOX
      PARAMETER (MAXBOX=64)

c
      CHARACTER file*132, outfile*132
      INTEGER   nsize(MAXNAX), axnum(MAXNAX), blc(MAXNAX), trc(MAXNAX)

      INTEGER   i,j,k, i1,j1,k1,nx, ny, nz, voldim
      INTEGER   naxis,lun,lout,size,size2
      REAL      buf(MAXDIM3,MAXDIM3), obuf(MAXDIM3), dmin, dmax
      REAL      dat(MAXBOX*MAXBOX), xdat, nsigma
      LOGICAL   flg(MAXDIM3,MAXDIM3), oflg(MAXDIM3)
c-----------------------------------------------------------------------
c  Announce
      CALL output( 'IMMEDIAN: '//PVERSION)
c
c  Get user inputs
c
      CALL keyini
      CALL keyf('in',file,' ')
      CALL keya('out',outfile,' ')
      CALL keyi('size',size,1)
      size2 = 2*size+1
      CALL keyr('nsigma',nsigma,0.0)
      CALL keyfin
c
c  Open files for in and output, check if buffers large enough, and
c  copy all header variables. We're not changing anything here to the
c  header. A dummy BLC/TRC is needed for HEADCOPY. AXNUM in there is
c  used to chop off the 4th and more dimensions of the data, if present.
c
      IF (file.EQ.' ') CALL bug('f',
     *          'No in= dataset specified')
      IF (outfile.EQ.' ') CALL bug('f',
     *          'No out= dataset specified')
c
      CALL xyopen(lun,file,'old',MAXNAX,nsize)
      IF(nsize(1).GT.MAXDIM3)call bug('f','Image 1 too big [MAXDIM3]')
      IF(nsize(2).GT.MAXDIM3)call bug('f','Image 2 too big [MAXDIM3]')
      CALL rdhdi(lun,'naxis',naxis,0)
      CALL xyopen(lout,outfile,'new',naxis,nsize)
      write(*,*) 'Median filter boxsize will be ',size2,' pixels.'
c
      nx = nsize(1)
      ny = nsize(2)
      nz = nsize(3)
c     // Fill some arrays to get HEADCOPY to do a full copy
c     // AXNUMs are such that >3 axes are chopped
c     // Although TRC is not used, we'll be nice, and make one
      DO i=1,naxis
        IF (i.LE.3) THEN
          axnum(i) = i
        ELSE
          axnum(i) = 0
        ENDIF
        blc(i) = 1
        trc(i) = nsize(i)
      ENDDO
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
c     // Now copy the header using spiffy HEADCOPY
      CALL headcopy(lun,lout,axnum,naxis,blc,trc)
c
c  Loop over the image/cube (NOTE: can only handle up to 3D)
c
      DO k=1,nz
         DO j=1,ny
            CALL xyread(lun,j,buf(1,j))
            CALL xyflgrd(lun,j,flg(1,j))
         ENDDO
         IF (k.EQ.1) THEN
            dmin = buf(1,1)
            dmax = buf(1,1)
         ENDIF
c                                                         1..size: just copy
         DO j=1,size
            CALL xywrite(lout,j,buf(1,j))
            CALL xyflgwr(lout,j,flg(1,j))
            CALL xyminmax(nx,buf(1,j),flg(1,j),dmin,dmax)
         ENDDO
c                                                 size+1 ...nx..size:  filter

         DO j=size+1,ny-size
            DO i=1,size
               obuf(i) = buf(i,j)
               oflg(i) = flg(i,j)
            ENDDO
            DO i=size+1,nx-size
               k1=0
               DO j1=-size,size
                  DO i1=-size,size
                     IF(flg(i+i1,j+j1)) THEN
                        k1=k1+1
                        dat(k1) = buf(i+i1,j+j1)
                     ENDIF
                  ENDDO
               ENDDO
               IF (k1.GT.0) THEN
c                  write(*,*) 'median ',i,j,k1
                  IF (nsigma.GT.0) THEN
                     CALL filter2(k1,dat,xdat,nsigma)
                  ELSE
                     CALL median(k1,dat,xdat)
                  ENDIF
                  obuf(i) = xdat
                  oflg(i) = .TRUE.
               ELSE
c                  write(*,*) 'median ',i,j,'***'
                  obuf(i) = buf(i,j)
                  oflg(i) = .FALSE.
               ENDIF
            ENDDO

            DO i=nx-size+1,nx
               obuf(i) = buf(i,j)
               oflg(i) = flg(i,j)
            ENDDO

            CALL xywrite(lout,j,obuf)
            CALL xyflgwr(lout,j,oflg)
            CALL xyminmax(nx,obuf,oflg,dmin,dmax)
         ENDDO
         DO j=ny-size+1,ny
            CALL xywrite(lout,j,buf(1,j))
            CALL xyflgwr(lout,j,flg(1,j))
            CALL xyminmax(nx,buf(1,j),flg(1,j),dmin,dmax)
         ENDDO
      ENDDO
c
c  Write new min and max
c
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
      SUBROUTINE xyminmax(n,buf,flg,dmin,dmax)
      IMPLICIT NONE
      INTEGER n
      REAL buf(n),dmin,dmax
      LOGICAL flg(n)
c
      INTEGER i

      DO i=1,n
         IF(flg(i)) THEN
            IF(buf(i).LT.dmin) dmin = buf(i)
            IF(buf(i).GT.dmax) dmax = buf(i)
         ENDIF
      ENDDO
      END
c***********************************************************************
c  straight from Zodiac (Stuart Vogel)
      SUBROUTINE median(n,x,xmed)
      IMPLICIT NONE
      INTEGER n
      REAL x(n),xmed
c
      INTEGER n2
c
      CALL sortr(x,n)
      n2=n/2
      IF(2*n2.EQ.n)THEN
        xmed=0.5*(x(n2)+x(n2+1))
      ELSE
        xmed=x(n2+1)
      ENDIF
      END
c***********************************************************************
      SUBROUTINE filter1(n,x,xmean,nsigma)
      IMPLICIT NONE
      INTEGER n
      REAL x(n),xmean,nsigma
c
      INTEGER i
c
      xmean = 0.0
      DO i=1,n
         xmean = xmean + x(i)
      ENDDO
      xmean = xmean/n
      END
c***********************************************************************
      SUBROUTINE filter2(n,x,xmean,nsigma)
      IMPLICIT NONE
      INTEGER n
      REAL x(n),xmean,nsigma
c
      INTEGER ip
c
      xmean = x(n)
      END
