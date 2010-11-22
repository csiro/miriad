      program colden

c= colden  - calculates column densities for linear molecules
c& mchw
c: map analysis
c+
c       COLDEN is a MIRIAD task to calculate column densities.  The maps
c       must have the same dimensions.  The two images are compared on a
c       pixel by pixel basis, within a user defined region.
c@ method
c       Three possibilities:
c         (1) Give one optically thin transition and the kinetic
c             temperature (Assumes tau=0).  Enter in1, in2, J1, B1, mu,
c             scale1.
c         (2) Give two optically thin transitions with different J.  The
c             calculation will assume LTE, calculate col. den. as in
c             (1), and fit to the best kinetic temperature and total
c             column density.  Enter in1, J1, b1, scale1, in2, J2,
c             scale2, and mu (b1=b2).
c         (3) Give one optically thick and one less thick transition.
c             Assumes Tex(thick)=Tex(thin) and calculates optical depth
c             of less thick transition.  Col. den. ensue.  Enter in1,
c             J1, b1, scale1, in2, b2, scale2, and mu (J1=J2).
c       Default=1.
c@ in1
c       The first input image contains the most abundant isotope
c       (e.g. 12CO).   The first plane must contain the integrated
c       intensity of the line (K-km/s) for methods 1 and 2, and the peak
c       temperature of the line (K) for method 3.  No default
c@ in2
c       For method 1, give the kinetic temperature in the first plane.
c       For method 2, this image contains the integrated intensity of
c       the second transition.
c       For method 3, the image contains the less abundant (thin)
c       isotope (e.g. 13CO).  It contains at least three planes: the
c       peak temperature of the line, its center position, and its FWHM
c       width.
c       Use the output from program Gaufit.  No default
c@ region
c       Region to select data to compare from....(not implemented)
c@ out
c       Output image.  It consists of 7 planes; the meaning of the first
c       three depend on the method used:
c         (1) the first two planes are blank, the third is the column
c             density of the upper level of the transition,
c         (2) the first is the kinetic temperature, the next two the
c             column densities of the upper levels of each transition,
c         (3) the first is the optical depth of the less abundant
c             isotope, the second is the excitation temperature of the
c             abundant isotope, and the third is the column density of
c             the upper level of the line analyzed.
c       The remaining four planes are:
c          -  the estimated total column density of the molecule,
c          -  the column density of H2 inferred,
c          -  the mass in each pixel, and
c          -  a last plane in which every unmasked pixel is set to 1.
c@ cut
c       Two values.  Cutoff applied to data (i.e. column densities will
c       not be calculate for input parameter values less than cutoff).
c       Default=0.1 K, 0.1 km/s.
c       There is also a cutoff for values of temp > 1000K, v > 100 km/s.
c       Additionally, values for column densities greater than 1.0e+27
c       are not written to the output file.
c@ b1
c       Value of B (in GHz) for the optically thick isotope.
c       Default=57.6 (12CO).
c@ b2
c       Value of B (in GHz) for the optically thin isotope.
c       Default=55.1 (13CO).
c@ j1
c       the rotational number of the upper level. Default J=1
c@ j2
c       the rotational number of the upper level. Default J=2
c@ mu
c       The dipole moment of the molecule (in Debye).
c       Default mu=0.112 (CO)
c@ scale1
c       A constant that will multiply the peak temperatures in in1.
c       Default=1.0
c@ scale2
c       A constant that will multiply the peak temperatures in in2.
c       Default=1.0
c@ abund
c       The abundance of the less abundant isotope, relative to H2.
c       This will be used to compute the H2 column density.
c       Default=2.0e-06 (appropriate for 13CO in dark clouds).
c@ dist
c       The distance of the source (in pc).  It will be used to compute
c       the mass in each pixel from the H2 column density.
c       Default: 500 pc.
c@ options
c       taulog: the optical depths are written as logs
c       collog: the column densities are written as logs
c       maslog: the masses are written as logs
c--
c
c  History:
c  28oct91  jt   Created by Jan Tauber.
c  15dec92  nebk Write a blank "btype" to output, change "float
c                to "real"
c  08may00  rjs  Change incorrect keyf call to keya.
c
      include 'maxdim.h'
      character PVERSION*(*)
      integer NAXIS, MAXPTS, MAXNAX, imask,method
      PARAMETER(NAXIS=3, MAXPTS=MAXDIM*MAXDIM/4, MAXNAX=7)
      PARAMETER(PVERSION='Version 1.0 05-feb-92')
c
      character in1*80,in2*80,out*80,total*6,masked*6
      character opts(3)*6
      logical optpres(3)
      integer row,i,J,JJ,JN,axnum(MAXNAX),blc(MAXNAX),trc(MAXNAX)
      integer lin1,lin2,size1(NAXIS),size2(NAXIS),size3(NAXIS),lout
      real buf1(MAXDIM),buf2(MAXDIM),buf3(MAXDIM),hBkt,hBk,mu,scale1
      real obuf1(MAXDIM),obuf2(MAXDIM),obuf3(MAXDIM),tex,tp,dv,B1,B2
      real obuf4(MAXDIM),obuf5(MAXDIM),obuf6(MAXDIM),obuf7(MAXDIM)
      real pi,h,debye,hd2,ex,exbg,temp,const,cut(2),abund,dist,mconst
      real cdelt1,cdelt2,area,scale2,kbol
      logical mask1(MAXDIM),mask2(MAXDIM),mask3(MAXDIM),omask(MAXDIM)
c
c
c  Externals.
c
      character itoaf*6
      logical keyprsnt
      data opts/'taulog','collog','maslog'/
c-----------------------------------------------------------------------
      call output('COLDEN: '//PVERSION)
c
c  Get the input parameters.
c
      J=1
      JJ=2
      call keyini
      call keyi('method',method,1)
      call keyf('in1',in1,' ')
      call keya('out',out,' ')
      call keyi('j1',J,1)
      call keyr('b1',B1,57.6)
      hBkt=0.047995*B1
      call keyr('mu',mu,0.112)
      call keyf('in2',in2,' ')
      if (method.eq.2) call keyi('j2',JJ,2)
      if (method.ne.3) hBk=hBkt
      if (method.eq.3) then
        call keyr('b2',B2,55.1)
        hBk=0.047995*B2
      endif
      if (keyprsnt('scale1')) then
        call keyr('scale1',scale1,1.0)
        else
        scale1=1.0
      endif
      if (keyprsnt('scale2')) then
        call keyr('scale2',scale2,1.0)
        else
        scale2=1.0
      endif
      cut(1)=0.1
      cut(2)=0.1
      if (keyprsnt('cut')) then
        call keyr('cut',cut(1),0.1)
        call keyr('cut',cut(2),0.1)
      endif
      abund=2e-06
      if (keyprsnt('abund')) call keyr('abund',abund,2e-06)
      dist=500.0
      if (keyprsnt('dist')) call keyr('dist',dist,500.0)
      call Options('options',opts,optpres,3)
      call keyfin
c
c  and some checks before going on
c
      if (in1.eq.' ' .or. in2.eq.' ') then
        call bug('f','You must specify two input files (in1=,in2=)')
      endif
      if (out.eq.' ') then
        call bug('f','You must specify an output file (out=)')
      endif
      if (J.le.0 .or. JJ.le.0) then
        call bug('f','J cannot be .< 0')
      endif
c
c  Open the input maps and check if sizes are the same
c
      call xyopen(lin1,in1,'old',NAXIS,size1)
      call xyopen(lin2,in2,'old',NAXIS,size2)
      do i = 1, 2
        if (size1(i).ne.size2(i)) then
          call bug('f','Sizes of images do not agree')
        endif
      enddo
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
      call headcp(lin1,lout,3,axnum,blc,trc)
      call wrhda(lout,'ctype3','TAU-C.D.')
      call wrhda(lout,'bunit','--------')
      call wrhdr(lout,'cdelt3',1.0)
      call wrhdr(lout,'crpix3',1.0)
      call wrhdr(lout,'crval3',1.0)
      call wrbtype(lout, ' ')
      call rdhdr(lout,'cdelt1',cdelt1,1.0)
      call rdhdr(lout,'cdelt2',cdelt2,1.0)
      area=abs(cdelt1)*abs(cdelt2)*(dist**2.0)
c  copy the maps and masks into the output, plane by plane, row by row
c
      pi=3.141592
      h=6.626e-27
      kbol=1.3806e-16
      debye=1e-18
c   This next constant is h/(debye**2)
      hd2=6.626e+09
c  THis next constant is m(H2)*(pc**2)/(Solar mass)
      mconst=1.5892e-20
      const=(hd2/(mu**2.0))*3.0
      const=const*((2.0*real(J)+1.0)/real(J))/(8.0*(pi**3.0))
      imask=0
      call xysetpl(lin1,1,1)
      do row = 1, size1(2)
        call xyread(lin1,row,buf1)
        call xyflgrd(lin1,row,mask1)
        call xysetpl(lin2,1,1)
        call xyread(lin2,row,buf2)
        call xyflgrd(lin2,row,mask2)
        if (method.eq.3) then
          call xysetpl(lin2,1,3)
          call xyread(lin2,row,buf3)
          call xyflgrd(lin2,row,mask3)
        endif
        do i = 1, size1(1)
          omask(i)=.true.
          if ((method.eq.3 .and. mask1(i) .and. mask2(i) .and. mask3(i))
     *        .or. (method.eq.1 .and. mask1(i) .and. mask2(i))
     *        .or. (method.eq.2 .and. mask1(i) .and. mask2(i))) then
c ***************** Method 1 *************************************
            if (method.eq.1) then
              obuf1(i)=0.0
              obuf2(i)=0.0
              if (buf1(i).lt.cut(1) .or. buf2(i).lt.cut(1) .or.
     *        buf1(i).gt.1000.0 .or.
     *        buf2(i).gt.1000.0) goto 10
c 3k/(16 pi^3  GHz Debye^2)  = 8.3487 10^13
              obuf3(i)=8.3487e+13*(2.0*real(J)+1.0)*buf1(i)
              obuf3(i)=scale1*obuf3(i)/(B1*((mu*real(J))**2.0))
              if (obuf3(i).gt.1e+25) goto 10
              tex=buf2(i)
              JN=J
            endif
c****************** Method 2 *************************************
            if (method.eq.2) then
              if (buf1(i).lt.cut(1) .or. buf2(i).lt.cut(1) .or.
     *            buf1(i).gt.1000.0 .or. buf2(i).gt.1000.0) goto 10
c       The col den of in1:
              obuf2(i)=8.3487e+13*(2.0*real(J)+1.0)*buf1(i)
              obuf2(i)=scale1*obuf2(i)/(B1*((mu*real(J))**2.0))
              if (obuf2(i).gt.1e+25) goto 10
c       The col den of in2
              obuf3(i)=8.3487e+13*(2.0*real(JJ)+1.0)*buf2(i)
              obuf3(i)=scale2*obuf3(i)/(B1*((mu*real(JJ))**2.0))
              if (obuf3(i).gt.1e+25) goto 10
c          The kinetic temperature
              temp=(((real(J)/real(JJ))**2.0)*(buf2(i)/buf1(i))*
     *             (scale2/scale1))
              if (temp.lt.0.00001) goto 10
              temp=log(temp)
              temp=hBkt*(real(J)*(real(J)+1.0)-real(JJ)*
     *                (real(JJ)+1.0))/temp
              JN=JJ
              obuf1(i)=temp
              if (temp.lt.cut(1) .or. temp.lt.0.0) goto 10
              tex=obuf1(i)
            endif
c ***************** Method 3 **************************************
            if (method.eq.3) then
              if (buf1(i).lt.cut(1) .or. buf2(i).lt.cut(1) .or.
     *            buf3(i).lt.cut(2) .or. buf1(i).gt.1000.0 .or.
     *            buf2(i).gt.1000.0 .or. buf3(i).gt.100.0) goto 10
              tex = scale1*buf1(i)
              ex=tex/(2.0*hBkt*real(J))
              exbg=1.0/(exp(2.0*hBkt*real(J)/2.7)-1.0)
              if (ex.gt.20.0) then
                tex=scale1*buf1(i)
              else
                tex=2.0*hBkt*real(J)/log(1.0+1.0/(ex+exbg))
              endif
              obuf2(i)=tex
              tp = scale2*buf2(i)
              dv = buf3(i)
c             write(*,*) buf1(i),tex,tp,dv
c
c Calculate the column densities
c
              ex=exp(2.0*hBk*real(J)/tex)-1.0
              exbg=exp(2.0*hBk*real(J)/2.7)-1.0
              temp=2.0*hBk*real(J)*((1.0/ex)-(1.0/exbg))
              if ((tp/temp).gt.0.99995) goto 10
              if (temp.lt.0.00001) goto 10
              if ((tp/temp).lt.0.0001) then
                temp=tp/temp
              else
                temp=-1.0*log(1.0-(tp/temp))
              endif
              obuf1(i)=temp
              if (optpres(1)) obuf1(i)=log10(temp)
c             if(isinf(dble(obuf1(i))).eq.1.
c     %               or.isnan(dble(obuf1(i))).eq.1)goto10
              if ((2.0*real(J)*hBk/tex).lt.1e-10) goto 10
              if ((2.0*real(J)*hBk/tex).gt.20.0) goto 10
              temp=temp/(exp(2.0*real(J)*hBk/tex)-1.0)
              temp=temp*const*dv*(1e+05)
              if (temp.gt.1e+25) goto 10
              obuf3(i)=temp
              JN=J
            endif
c******** Now calculate all the other things **************************
            temp=obuf3(i)
            if (optpres(2)) obuf3(i)=log10(temp)
            if (optpres(2) .and. method.eq.2)
     *        obuf2(i)=log10(obuf2(i))
            if ((hBk*real(JN)*(real(JN)+1.0)/tex).gt.20.0) goto 10
            temp=temp*exp(hBk*real(JN)*(real(JN)+1.0)/tex)
            temp=temp*(tex/hBk)
            temp=temp/(2.0*real(JN)+1.0)
            if (temp.gt.1e+28) goto 10
            obuf4(i)=temp
            if (optpres(2)) obuf4(i)=log10(temp)
            temp=temp/abund
            obuf5(i)=temp
            if (optpres(2)) obuf5(i)=log10(temp)
            temp=temp*area*mconst
            obuf6(i)=temp
            if (optpres(3)) obuf6(i)=log10(temp)
            obuf7(i)=1.0
c          write(*,*) obuf1(i),obuf2(i),obuf3(i),obuf4(i),obuf5(i)
            goto 20
10          continue
            omask(i)=.false.
            imask=imask+1
            obuf1(i)=0.0
            obuf2(i)=0.0
            obuf3(i)=0.0
            obuf4(i)=0.0
            obuf5(i)=0.0
            obuf6(i)=0.0
            obuf7(i)=0.0
20          continue
          else
            omask(i)=.false.
            imask=imask+1
            obuf1(i)=0.0
            obuf2(i)=0.0
            obuf3(i)=0.0
            obuf4(i)=0.0
            obuf5(i)=0.0
            obuf6(i)=0.0
            obuf7(i)=0.0
          endif

          call xysetpl(lout,1,1)
          call xywrite(lout,row,obuf1)
          call xyflgwr(lout,row,omask)
          call xysetpl(lout,1,2)
          call xywrite(lout,row,obuf2)
          call xyflgwr(lout,row,omask)
          call xysetpl(lout,1,3)
          call xywrite(lout,row,obuf3)
          call xyflgwr(lout,row,omask)
          call xysetpl(lout,1,4)
          call xywrite(lout,row,obuf4)
          call xyflgwr(lout,row,omask)
          call xysetpl(lout,1,5)
          call xywrite(lout,row,obuf5)
          call xyflgwr(lout,row,omask)
          call xysetpl(lout,1,6)
          call xywrite(lout,row,obuf6)
          call xyflgwr(lout,row,omask)
          call xysetpl(lout,1,7)
          call xywrite(lout,row,obuf7)
          call xyflgwr(lout,row,omask)
        enddo
      enddo

      call xyclose(lin1)
      call xyclose(lin2)
      total=itoaf(size1(1)*size1(2))
      masked=itoaf(imask)
      call output('Total number of pixels: '//total)
      call output('Number of masked pixels: '//masked)
c
c  Write the history file
c
      call hisopen(lout,'write')
      call hiswrite(lout,'COLDEN: Calculate Column Densities')
      call hisinput(lout,'colden')
      call hisclose(lout)
      call xyclose(lout)

      end
