c***********************************************************************
      PROGRAM colden
      IMPLICIT NONE
c
c= colden  - calculates column densities for linear molecules
c& mchw 
c: map analysis
c+
c       COLDEN is a MIRIAD task to calculate column densities. 
c	The maps must have the same dimensions.
c	The two images are compared on a pixel by pixel basis, within a
c	user defined region.
c@ method
c	Three possibilities:(1) Give one optically thin transition and the
c	kinetic temperature (Assumes tau=0). Enter in1, in2, J1, B1, mu,
c       scale1.(2) Give two optically thin transitions with different J.The
c	calculation will assume LTE, calculate col. den. as in (1), and
c	fit to the best kinetic temperature and total column density. Enter
c	in1,J1,b1,scale1, in2,J2,scale2, and mu (b1=b2). (3) Give one optically
c	thick and one less thick transition. Assumes Tex(thick)=Tex(thin)
c	and calculates optical depth of less thick transition. Col. den. ensue.
c	Enter in1,J1,b1,scale1, in2,b2,scale2, and mu (J1=J2).
c       Default=1.
c@ in1
c       The first input image contains the most abundant isotope
c	(e.g. 12CO).   The first plane must contain the integrated intensity
c	of the line (K-km/s) for methods 1 and 2, and the peak temperature
c	of the line (K) for method 3. No default
c@ in2
c	For method 1, give the kinetic temperature in the first plane. 
c       For method 2, this image contains the integrated intensity of the
c	second transition. For method 3,
c       the  image contains the less abundant (thin) 
c	isotope (e.g. 13CO).  It contains at least three planes: the peak 
c	temperature of the line, its center position, and its FWHM width. 
c       Use the output from program Gaufit. No default
c@ region
c	Region to select data to compare from....(not implemented)
c@ out
c	Output image. It consists of 7 planes; the meaning of the first 3
c	depend on the method used. (1) the first two planes are blank, the
c	third is the col. den. of the upper level of the transition.
c	(2) the first is the kinetic temperature, the next two the column
c	densities of the upper levels of each transition. (3) the first is
c	the optical depth of the less abundant isotope, the second is the
c	excitation temperature of the abundant isotope, and the third is
c	the column density of the upper level of the line analyzed.
c	The remaining four planes are:
c	the estimated total column density of the molecule, the column
c	density of H2 inferred, the mass in each pixel, and a last plane
c	in which every unmasked pixel is set to 1.
c@ cut
c	Two values.
c       Cutoff applied to data (i.e. column densities will not be calculate
c       for input parameter values less than cutoff). Default=0.1 K, 0.1 km/s.
c       There also is a cutoff for values of temp > 1000 K, v > 100 km/s.
c	Additionally, values for column densities greater than 1.0e+27 are not
c	written to the output file.
c@ b1  
c	Value of B (in GHz) for the optically thick isotope.Default=57.6 (12CO).
c@ b2  
c	Value of B (in GHz) for the optically thin isotope.Default=55.1 (13CO).
c@ j1
c	the rotational number of the upper level. Default J=1
c@ j2
c	the rotational number of the upper level. Default J=2
c@ mu
c	The dipole moment of the molecule (in Debye). Default mu=0.112 (CO)
c@ scale1
c	A constant that will multiply the peak temperatures in in1.
c	Default=1.0
c@ scale2
c	A constant that will multiply the peak temperatures in in2.
c	Default=1.0
c@ abund
c	The abundance of the less abundant isotope, relative to H2. THis will
c	be used to compute the H2 column density. Default=2.0e-06 (appropriate
c	for 13CO in dark clouds).
c@ dist
c	THe distance of the source (in pc). It will be used to compute the mass
c	in each pixel from the H2 column density. Default: 500 pc.
c@ options
c	taulog: the optical depths are written as logs
c	collog: the column densities are written as logs
c	maslog: the masses are written as logs
c--
c
c  History:
c  28oct91  jt   Created by Jan Tauber.
c  15dec92  nebk Write a blank "btype" to output, change "float
c                to "real"
c
      INCLUDE 'maxdim.h'
      CHARACTER PVERSION*(*)
      INTEGER NAXIS, MAXPTS, MAXNAX, imask,method
      PARAMETER(NAXIS=3, MAXPTS=MAXDIM*MAXDIM/4, MAXNAX=7)
      PARAMETER(PVERSION='Version 1.0 05-feb-92')
c
      CHARACTER in1*80,in2*80,out*80,total*6,masked*6
      CHARACTER opts(3)*6
      LOGICAL optpres(3)
      INTEGER row,i,J,JJ,JN,axnum(MAXNAX),blc(MAXNAX),trc(MAXNAX)
      INTEGER lin1,lin2,size1(NAXIS),size2(NAXIS),size3(NAXIS),lout
      REAL buf1(MAXDIM),buf2(MAXDIM),buf3(MAXDIM),hBkt,hBk,mu,scale1
      REAL obuf1(MAXDIM),obuf2(MAXDIM),obuf3(MAXDIM),tex,tp,dv,B1,B2
      REAL obuf4(MAXDIM),obuf5(MAXDIM),obuf6(MAXDIM),obuf7(MAXDIM)
      REAL pi,h,debye,hd2,ex,exbg,temp,const,cut(2),abund,dist,mconst
      REAL cdelt1,cdelt2,area,scale2,kbol
      LOGICAL mask1(MAXDIM),mask2(MAXDIM),mask3(MAXDIM),omask(MAXDIM)
c
c
c  Externals.
c
      CHARACTER itoaf*6
      LOGICAL keyprsnt
      data opts/'taulog','collog','maslog'/
c-----------------------------------------------------------------------
      CALL output( 'COLDEN: '//PVERSION)
c
c  Get the input parameters.
c
      J=1
      JJ=2
      CALL keyini
      CALL keyi('method',method,1)
      CALL keyf('in1',in1,' ')
      CALL keyf('out',out,' ')
      CALL keyi('j1',J,1)
      CALL keyr('b1',B1,57.6)
      hBkt=0.047995*B1
      CALL keyr('mu',mu,0.112)
        CALL keyf('in2',in2,' ')
        if(method.eq.2) CALL keyi('j2',JJ,2)
	if(method.ne.3) hBk=hBkt
        if(method.eq.3) then
	  CALL keyr('b2',B2,55.1)
          hBk=0.047995*B2
        endif
      IF (keyprsnt('scale1')) THEN
	CALL keyr('scale1',scale1,1.0)
	ELSE
	scale1=1.0
      ENDIF
      IF (keyprsnt('scale2')) THEN
	CALL keyr('scale2',scale2,1.0)
	ELSE
	scale2=1.0
      ENDIF
      cut(1)=0.1
      cut(2)=0.1
      IF (keyprsnt('cut')) THEN
	CALL keyr('cut',cut(1),0.1)
	CALL keyr('cut',cut(2),0.1)
      ENDIF
      abund=2.0e-06
      IF(keyprsnt('abund')) CALL keyr('abund',abund,2.0e-06)
      dist=500.0
      IF(keyprsnt('dist')) CALL keyr('dist',dist,500.0)
      CALL Options('options',opts,optpres,3)
      CALL keyfin
c
c  and some checks before going on
c
      IF(in1.EQ.' ' .OR. in2.EQ.' ') THEN
         CALL bug('f','You must specify two input files (in1=,in2=)')
      ENDIF
      IF(out.EQ.' ') THEN
	 CALL bug('f','You must specify an output file (out= )')
      ENDIF
      IF(J.LE.0 .OR. JJ.LE.0) THEN
	 CALL bug('f','J cannot be .< 0')
      ENDIF
c
c  Open the input maps and check if sizes are the same
c
      CALL xyopen(lin1,in1,'old',NAXIS,size1)
	CALL xyopen(lin2,in2,'old',NAXIS,size2)
        DO i=1,2
         IF (size1(i).NE.size2(i)) THEN
            CALL bug('f','Sizes of images do not agree')
         ENDIF
        ENDDO

c
c  open the output map and copy the header into it
c
      size3(1)=size1(1)
      size3(2)=size1(2)
      size3(3)=7
      call xyopen(lout,out,'new',3,size3)
      axnum(1)=0
      blc(1)=0
      trc(1)=0
      call headcopy(lin1,lout,axnum,3,blc,trc)
      call wrhda(lout,'ctype3','TAU-C.D.')
      call wrhda(lout,'bunit','--------')
      call wrhdr(lout,'cdelt3',1.0)
      call wrhdr(lout,'crpix3',1.0)
      call wrhdr(lout,'crval3',1.0)
      call wrbtype (lout, ' ')
      call rdhdr(lout,'cdelt1',cdelt1,1.0)
      call rdhdr(lout,'cdelt2',cdelt2,1.0)
      area=abs(cdelt1)*abs(cdelt2)*(dist**2.0)
c  copy the maps and masks into the output, plane by plane, row by row
c
      pi=3.141592
      h=6.626e-27
      kbol=1.3806e-16
      debye=1.0e-18
c   This next constant is h/(debye**2)
      hd2=6.626e+09
c  THis next constant is m(H2)*(pc**2)/(Solar mass)
      mconst=1.5892e-20
      const=(hd2/(mu**2.0))*3.0
      const=const*((2.0*real(J)+1.0)/real(J))/(8.0*(pi**3.0))
      imask=0
         CALL xysetpl(lin1,1,1)
         DO row=1,size1(2)
 	    CALL xyread(lin1,row,buf1)
            CALL xyflgrd(lin1,row,mask1)
              CALL xysetpl(lin2,1,1)
 	      CALL xyread(lin2,row,buf2)
              CALL xyflgrd(lin2,row,mask2)
	      if(method.eq.3) then
                CALL xysetpl(lin2,1,3)
	        CALL xyread(lin2,row,buf3)
	        CALL xyflgrd(lin2,row,mask3)
               endif
            DO i=1,size1(1)
	       omask(i)=.true.
               IF((method.eq.3.and.mask1(i).AND.mask2(i).AND.mask3(i))
     %             .or.(method.eq.1.and.mask1(i).and.mask2(i))
     %             .or.(method.eq.2.and.mask1(i).and.mask2(i))) THEN
c ***************** Method 1 *************************************
	       if(method.eq.1) then
		 obuf1(i)=0.
		 obuf2(i)=0.
	        IF(buf1(i).lt.cut(1).or.buf2(i).lt.cut(1).or.
     %	        buf1(i).gt.1000.0.or.
     %          buf2(i).gt.1000.0)GOTO10
c 3k/(16 pi^3  GHz Debye^2)  = 8.3487 10^13
		 obuf3(i)=8.3487e+13*(2.0*real(J)+1.0)*buf1(i)
		 obuf3(i)=scale1*obuf3(i)/(B1*((mu*real(J))**2.0))
		 if(obuf3(i).gt.1.0e+25) goto10
		 tex=buf2(i)
		 JN=J
	       endif
c****************** Method 2 *************************************
	       if(method.eq.2) then
	        IF(buf1(i).lt.cut(1).or.buf2(i).lt.cut(1).or.
     %	        buf1(i).gt.1000.0.or.
     %          buf2(i).gt.1000.0)GOTO10
c         The col den of in1:
		 obuf2(i)=8.3487e+13*(2.0*real(J)+1.0)*buf1(i)
		 obuf2(i)=scale1*obuf2(i)/(B1*((mu*real(J))**2.0))
		 if(obuf2(i).gt.1.0e+25) goto10
c         The col den of in2
		 obuf3(i)=8.3487e+13*(2.0*real(JJ)+1.0)*buf2(i)
		 obuf3(i)=scale2*obuf3(i)/(B1*((mu*real(JJ))**2.0))
		 if(obuf3(i).gt.1.0e+25) goto10
c          The kinetic temperature
		 temp=(((real(J)/real(JJ))**2.0)*(buf2(i)/buf1(i))*
     %                      (scale2/scale1))
		 if(temp.lt.0.00001)goto 10
		 temp=log(temp)
		 temp=hBkt*(real(J)*(real(J)+1.0)-real(JJ)*
     %   		 (real(JJ)+1.0))/temp
		 JN=JJ
		 obuf1(i)=temp
		 if(temp.lt.cut(1).or.temp.lt.0.)goto10
		 tex=obuf1(i)
	       endif
c ***************** Method 3 **************************************
               if(method.eq.3) then
	        IF(buf1(i).lt.cut(1).or.buf2(i).lt.cut(1).or.
     %	        buf3(i).lt.cut(2).or.buf1(i).gt.1000.0.or.
     %          buf2(i).gt.1000.0.or.buf3(i).gt.100.0)GOTO10
                    tex = scale1*buf1(i)
		    ex=tex/(2.0*hBkt*real(J))
		    exbg=1.0/(exp(2.0*hBkt*real(J)/2.7)-1.0)
		    IF(ex.gt.20.0) THEN
		      tex=scale1*buf1(i)
                     ELSE
		      tex=2.0*hBkt*real(J)/log(1.0+1.0/(ex+exbg))
                    ENDIF
		    obuf2(i)=tex
		    tp = scale2*buf2(i)
		    dv = buf3(i)
c		    write(*,*) buf1(i),tex,tp,dv
c
c Calculate the column densities
c
                  ex=exp(2.0*hBk*real(J)/tex)-1.0
		  exbg=exp(2.0*hBk*real(J)/2.7)-1.0
                  temp=2.0*hBk*real(J)*((1.0/ex)-(1.0/exbg))
		  if((tp/temp).gt.0.99995)goto10
		  if(temp.lt.0.00001) goto10
		  if((tp/temp).lt.0.0001) then
		    temp=tp/temp
	           else
		    temp=-1.0*log(1.0-(tp/temp))
	          endif
		  obuf1(i)=temp
		  if(optpres(1))obuf1(i)=log10(temp)
c		  if(isinf(dble(obuf1(i))).eq.1.
c     %		       or.isnan(dble(obuf1(i))).eq.1)goto10
		  if((2.0*real(J)*hBk/tex).lt.1.0e-10)goto10
		  if((2.0*real(J)*hBk/tex).gt.20.0)goto10
                  temp=temp/(exp(2.0*real(J)*hBk/tex)-1.0)
		  temp=temp*const*dv*(1.0e+05)
		  if(temp.gt.1.0e+25)goto10
		  obuf3(i)=temp
		  JN=J
                end if
c******** Now calculate all the other things **************************
		  temp=obuf3(i)
		  if(optpres(2))obuf3(i)=log10(temp)
		  if(optpres(2).and.method.eq.2)
     %		    obuf2(i)=log10(obuf2(i))
		  if((hBk*real(JN)*(real(JN)+1.0)/tex).gt.20.0)goto10
		  temp=temp*exp(hBk*real(JN)*(real(JN)+1.0)/tex)
		  temp=temp*(tex/hBk)
		  temp=temp/(2.0*real(JN)+1.0)
		  if(temp.gt.1.0e+28)goto10
		  obuf4(i)=temp
		  if(optpres(2))obuf4(i)=log10(temp)
		  temp=temp/abund
		  obuf5(i)=temp
		  if(optpres(2))obuf5(i)=log10(temp)
		  temp=temp*area*mconst
		  obuf6(i)=temp
		  if(optpres(3))obuf6(i)=log10(temp)
		  obuf7(i)=1.0
c          write(*,*) obuf1(i),obuf2(i),obuf3(i),obuf4(i),obuf5(i)
		  goto20
10                continue
		      omask(i)=.false.
		      imask=imask+1
		      obuf1(i)=0.
		      obuf2(i)=0.
		      obuf3(i)=0.
		      obuf4(i)=0.0
		      obuf5(i)=0.0
		      obuf6(i)=0.0
		      obuf7(i)=0.0
20                continue
		ELSE
                  omask(i)=.false.
		      imask=imask+1
		      obuf1(i)=0.
		      obuf2(i)=0.
		      obuf3(i)=0.
		      obuf4(i)=0.
		      obuf5(i)=0.
		      obuf6(i)=0.
		      obuf7(i)=0.
               ENDIF
	       CALL xysetpl(lout,1,1)
	       CALL xywrite(lout,row,obuf1)
	       CALL xyflgwr(lout,row,omask)
	       CALL xysetpl(lout,1,2)
	       CALL xywrite(lout,row,obuf2)
	       CALL xyflgwr(lout,row,omask)
	       CALL xysetpl(lout,1,3)
	       CALL xywrite(lout,row,obuf3)
	       CALL xyflgwr(lout,row,omask)
	       CALL xysetpl(lout,1,4)
	       CALL xywrite(lout,row,obuf4)
	       CALL xyflgwr(lout,row,omask)
	       CALL xysetpl(lout,1,5)
	       CALL xywrite(lout,row,obuf5)
	       CALL xyflgwr(lout,row,omask)
	       CALL xysetpl(lout,1,6)
	       CALL xywrite(lout,row,obuf6)
	       CALL xyflgwr(lout,row,omask)
	       CALL xysetpl(lout,1,7)
	       CALL xywrite(lout,row,obuf7)
	       CALL xyflgwr(lout,row,omask)
            ENDDO
         ENDDO
      CALL xyclose(lin1)
      CALL xyclose(lin2)
      total=itoaf(size1(1)*size1(2))
      masked=itoaf(imask)
      CALL output('Total number of pixels: '//total)
      CALL output('Number of masked pixels: '//masked)
c
c  Write the history file
c
	CALL hisopen(lout,'write')
	CALL hiswrite(lout,'COLDEN: Calculate Column Densities')
	CALL hisinput(lout,'colden')
	CALL hisclose(lout)
	CALL xyclose(lout)
      END
c************************************************************************
