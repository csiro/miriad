      subroutine atJones(rad,psi,freq,Jo,pb)

      real rad, psi, Jo(2,2), pb
      double precision freq

c  Compute the Jones matrix for the ATCA antennas for a particular
c  frequency and position in the primary beam.  Also compute the primary
c  beam response.
c
c  Accuracy of the Jones matrices appears to degrade significantly beyond
c  a radius of approximately 2*HWHM of the primary beam.
c
c  In general, Jones matrices are complex-valued.  The coherence matrix
c  is given by the product of J * transpose(conjugate(J)), i.e.
c
c    XX = J(1,1)*conjg(J(1,1)) + J(1,2)*conjg(J(1,2))
c    YY = J(2,1)*conjg(J(2,1)) + J(2,2)*conjg(J(2,2))
c    XY = J(1,1)*conjg(J(2,1)) + J(1,2)*conjg(J(2,2))
c    YX = J(2,1)*conjg(J(1,1)) + J(2,2)*conjg(J(1,2))
c       = conjg(XY)
c
c  The Jones matrix for the ATCA antennas is real-valued at all
c  frequencies and is treated as such for efficiency.  The coherence
c  matrix then reduces to
c
c    XX = J(1,1)*J(1,1) + J(1,2)*J(1,2)
c    YY = J(2,1)*J(2,1) + J(2,2)*J(2,2)
c    XY = J(1,1)*J(2,1) + J(1,2)*J(2,2)
c    YX = XY
c
c  Inputs:
c    rad  The distance of the point from the field centre, in radians.
c    psi  Position angle of the point, in radians.
c    freq Observing frequency, in GHz.
c
c  Output:
c    Jo   The 2x2 Jones matrix which is real-valued for the ATCA.
c    pb   The mean total intensity primary beam response at this point.
c
c $Id: atjones.for,v 1.5 2021/06/02 03:12:53 wie017 Exp $
c-----------------------------------------------------------------------
      include 'mirconst.h'

c     alpha2 = 4*log(2).
      real alpha2
      parameter (alpha2 = 2.772589)

      integer i
      real    aC(2,5), aL(2,5), aS(2,7), aX(2,7), px, py, rdist, x(7)

      data aL/ 1.3523E+00,   0.0000E+00,
     *        -8.9658E-02,   4.1000E+00,
     *        -1.2942E-02,   6.4604E-03,
     *         1.5156E-02,  -1.0285E-02,
     *        -1.5113E-03,   5.0859E-03/

      data aS/ 1.3992E+00,   0.0000E+00,
     *         6.6962E-02,   9.2800E-01,
     *        -8.1047E-02,   4.6582E-02,
     *         5.5058E-02,  -4.5728E-02,
     *         4.2927E-02,  -1.5807E-02,
     *         5.2665E-02,  -3.8708E-02,
     *        -1.8535E-02,   1.3006E-02/

      data aC/ 1.3804E+00,   0.0000E+00,
     *        -6.0461E-03,   8.2200E-01,
     *        -3.9537E-02,   2.5856E-02,
     *         3.9076E-02,  -2.5159E-02,
     *        -2.6902E-03,  -4.2609E-03/

      data aX/ 1.4175E+00,   0.0000E+00,
     *         3.0893E-02,   1.1840E+00,
     *        -1.0202E-01,   6.1286E-02,
     *         7.9883E-02,  -4.8667E-02,
     *         3.5436E-03,   3.1695E-02,
     *         2.9788E-02,  -1.7744E-02,
     *        -3.3598E-02,   1.7741E-02/
c-----------------------------------------------------------------------
      if (freq.gt.1d0 .and. freq.lt.2d0) then
c       L-band (20cm) response.
        rdist =  real(rad / (1.384/freq * (34.61/60.0)*D2R))
        x(1) = exp(-alpha2*(rdist/aL(1,1))**2)
        x(2) = aL(1,2)*sin(PI_2*rdist/aL(2,2))**2
        pb = x(1)*x(1) + 0.5*x(2)*x(2)
        do i = 3, 5
          x(i) = (aL(2,i)*rdist + aL(1,i))*rdist
          pb = pb + 0.5*x(i)*x(i)
        enddo

        px =  psi
        py = -psi - PI_2
        Jo(1,1) = x(1) + x(2)*cos(2.0*px) + x(3)*cos(px) + x(4)*sin(px)
        Jo(2,2) = x(1) + x(2)*cos(2.0*py) + x(3)*cos(py) + x(4)*sin(py)
        py = -py
        Jo(1,2) =  x(5)*sin(2.0*px)
        Jo(2,1) = -x(5)*sin(2.0*py)

      else if (freq.gt.2d0 .and. freq.lt.3d0) then
c       S-band (13cm) response.
        rdist =  real(rad / (2.368/freq * (20.9882/60.0)*D2R))
        x(1) = exp(-alpha2*(rdist/aS(1,1))**2)
        x(2) = aS(1,2)*sin(PI_2*rdist/aS(2,2))**2
        pb = x(1)*x(1) + 0.5*x(2)*x(2)
        do i=3,7
          x(i) = (aS(2,i)*rdist + aS(1,i))*rdist
          pb = pb + 0.5*x(i)*x(i)
        enddo

        px =  psi
        py = -psi - PI_2
        Jo(1,1) = x(1) + x(2)*cos(2.0*px) + x(3)*cos(px) + x(4)*sin(px)
        Jo(2,2) = x(1) + x(2)*cos(2.0*py) + x(3)*cos(py) + x(4)*sin(py)
        py = -py
        Jo(1,2) = real(cmplx(x(5),x(6))*sin(px) +
     *    cmplx(x(7),0.0)*cos(px))
        Jo(2,1) = real(-(cmplx(x(5),x(6))*sin(py) +
     *    cmplx(x(7),0.0)*cos(py)))

      else if (freq.gt.4d0 .and. freq.lt.6d0) then
c       C-band (6cm) response.
        rdist =  real(rad / (4.800/freq * (10.06250/60.0)*D2R))
        x(1) = exp(-alpha2*(rdist/aC(1,1))**2)
        x(2) = aC(1,2)*sin(PI_2*rdist/aC(2,2))**2
        pb = x(1)*x(1) + 0.5*x(2)*x(2)
        do i = 3, 5
          x(i) = (aC(2,i)*rdist + aC(1,i))*rdist
          pb = pb + 0.5*x(i)*x(i)
        enddo

        px =  psi
        py = -psi - PI_2
        Jo(1,1) = x(1) + x(2)*cos(2.0*px) + x(3)*cos(px) + x(4)*sin(px)
        Jo(2,2) = x(1) + x(2)*cos(2.0*py) + x(3)*cos(py) + x(4)*sin(py)
        py = -py
        Jo(1,2) =  x(5)*sin(2.0*px)
        Jo(2,1) = -x(5)*sin(2.0*py)

      else if (freq.gt.8d0 .and. freq.lt.9d0) then
c       X-band (3cm) response.
        rdist =  real(rad / (8.640/freq * (5.86/60.0)*D2R))
        x(1) = exp(-alpha2*(rdist/aX(1,1))**2)
        x(2) = aX(1,2)*sin(PI_2*rdist/aX(2,2))**2
        pb = x(1)*x(1) + 0.5*x(2)*x(2)
        do i = 3, 7
          x(i) = (aX(2,i)*rdist + aX(1,i))*rdist
          pb = pb + 0.5*x(i)*x(i)
        enddo

        px =  psi
        py = -psi - PI_2
        Jo(1,1) = x(1) + x(2)*cos(2.0*px) + x(3)*cos(px) + x(4)*sin(px)
        Jo(2,2) = x(1) + x(2)*cos(2.0*py) + x(3)*cos(py) + x(4)*sin(py)
        py = -py
        Jo(1,2) = real(cmplx(x(5),0.0)*sin(2.0*px) +
     *    cmplx(x(6),x(7))*sin(px))
        Jo(2,1) = real(cmplx(x(5),0.0)*sin(2.0*py) +
     *    cmplx(x(6),x(7))*sin(py))

      else
        call bug('f','Polarimetric response not known at this freq')
      endif

      end

      subroutine atJones2(rad,psi,freq,Jo,pb)

      real rad, psi, pb
      complex Jo(2,2)
      double precision freq

c  Compute the Jones matrix for the ATCA antennas for a particular
c  frequency and position in the primary beam.  Also compute the primary
c  beam response. This version works from a table of beam measurements.
c
c  In general, Jones matrices are complex-valued.  The coherence matrix
c  is given by the product of J * transpose(conjugate(J)), i.e.
c
c    XX = J(1,1)*conjg(J(1,1)) + J(1,2)*conjg(J(1,2))
c    YY = J(2,1)*conjg(J(2,1)) + J(2,2)*conjg(J(2,2))
c    XY = J(1,1)*conjg(J(2,1)) + J(1,2)*conjg(J(2,2))
c    YX = J(2,1)*conjg(J(1,1)) + J(2,2)*conjg(J(1,2))
c       = conjg(XY)
c
      include 'mirconst.h'
      complex J(4,16,14,100)
      real P(14,100)
      integer band,f0,df,nf,nr,nth
      real dr,dth
      common /jtable/ J,P,band,f0,df,nf,nr,nth,dr,dth
c      
      real r,th,f,rmu,tmu,fmu
      integer ir,ith,ifr,i,l,m,typ,ir2,ith2,ifr2
      logical ok
      complex a(4,4,4),b(4,4)
      integer xr(4),xt(4),xf(4),kr,kt,kf
c
      complex trilin,bicubicInterpolate,tricubicInterpolate     
c
      r=rad/dr*180/PI
      th=psi*180/PI
      if (th.lt.0) th=th+360
      if (th.ge.360) th=th-360
      th=th/dth
      f=real(max(0.d0,(freq*1000-f0)/df))
      typ=2
c
c     0'th order: return nearest value
c      
      if (typ.eq.0) then
        ok=.true.
        ith=int(th+0.5)+1
        if (ith.gt.nth) ith=1
        tmu=int(th)-th
        ir=int(r+0.5)+1
        ok=ok.and.(ir.le.nr)
        ifr=int(f+0.5)+1
        ok=ok.and.(ifr.ge.1.and.ifr.le.nf)
        if (ok) then
          Jo(1,1)=J(1,ith,ir,ifr)
          Jo(1,2)=J(2,ith,ir,ifr)
          Jo(2,1)=J(3,ith,ir,ifr)
          Jo(2,2)=J(4,ith,ir,ifr)
          pb=P(ir,ifr)
        else
          Jo(1,1)=0
          Jo(1,2)=0
          Jo(2,1)=0
          Jo(2,2)=0 
        endif
      else if (typ.ge.1) then
c     1st order: linear interpolation
        ith=int(th)
        tmu=th-ith
        ith=ith+1
        if (ith.gt.nth) ith=1
        ith2=ith+1
        if (ith2.gt.nth) ith2=1
        ir=int(r)
        rmu=r-ir
        ir=min(nr,ir+1)
        ir2=min(nr,ir+1)
        ifr=int(f)
        fmu=f-ifr
        ifr=min(nf,ifr+1)
        ifr2=min(nf,ifr+1)
        do i=1,4
          l=(i+1)/2
          m=i
          if (m.gt.2) m=m-2
          if (typ.eq.1) then
c         Use trilinear interpolation  
            Jo(l,m)=trilin(J(i,ith,ir,ifr),J(i,ith,ir,ifr2),
     *                     J(i,ith,ir2,ifr),J(i,ith,ir2,ifr2),
     *                     J(i,ith2,ir,ifr),J(i,ith2,ir,ifr2),
     *                     J(i,ith2,ir2,ifr),J(i,ith2,ir2,ifr2),
     *                     tmu,rmu,fmu)
          else if (typ.eq.2) then
c         Use tricubic interpolation
c         Boundary conditions: for r<0 reflect, for theta periodic,
c         for others constant (for now, could use extrapolation) 
            xt(1)=ith-1
            if (xt(1).eq.0) xt(1)=nth
            xt(2)=ith
            xt(3)=ith2
            xt(4)=ith2+1
            if (xt(4).gt.nth) xt(4)=1
            xr(1)=ir-1
            if (xr(1).eq.0) xr(1)=2
            xr(2)=ir
            xr(3)=ir2
            xr(4)=min(ir2+1,nr)
            xf(1)=max(ifr-1,1)
            xf(2)=ifr
            xf(3)=ifr2
            xf(4)=min(ifr2+1,nf)
            do kf=1,4
              do kr=1,4
                do kt=1,4
                  a(kt,kr,kf)= J(i,xt(kt),xr(kr),xf(kf))
                enddo
              enddo
            enddo         
            Jo(l,m)=tricubicInterpolate(a,fmu,rmu,tmu)
          endif
        enddo
        if (typ.eq.1) then
          pb = P(ir,ifr)*(1-rmu)*(1-fmu)+P(ir,ifr2)*(1-rmu)*fmu+
     *       P(ir2,ifr)*rmu*(1-fmu)+P(ir2,ifr2)*rmu*fmu
        else if (typ.eq.2) then
          do kf=1,4
            do kr=1,4
              b(kr,kf)=P(xr(kr),xf(kf))
            enddo
          enddo
          pb = real(bicubicInterpolate(b,fmu,rmu))
        endif
      endif
      end
      
      subroutine atJLoad(freq)
      double precision freq
c      
      integer f,ir,it,i,pol
      character*80 fname,pname,mircat
      character*160 stcat
      real def,rval,ival
c
      complex J(4,16,14,100)
      real P(14,100)
      integer band,f0,df,nf,nr,nth
      real dr,dth
      common /jtable/ J,P,band,f0,df,nf,nr,nth,dr,dth
      
      def=0.0
      if (freq.gt.1.d0.and.freq.lt.3.5d0) then
        band=1
      else
        band=0
      endif
      if (band.eq.0) call bug('f','Unsupported frequency')
      if (band.eq.1) then
        nf=14
        nr=14
        nth=16
        dr=0.05
        dth=360.0/nth
        f0=1268
        df=128
c        write(*,*) 'ATJLoad: using ',nf,' freqs, f0=',f0,', df=',df
        call getenv('MIRCAT',mircat)
        fname='/Jones1268.txt'
        pname='/VoltBeam1268.txt'
        do i=1,nf
          f=f0+(i-1)*df
          write(fname(7:10),'(I4)') f
          call tinOpen(stcat(mircat,fname),'s')
          do it=1,nth
            J(1,it,1,i)=1
            J(2,it,1,i)=0
            J(3,it,1,i)=0
            J(4,it,1,i)=1
          enddo
          do ir=2,nr
            do it=1,nth
              do pol=1,4
                call tinGetr(rval,def)
                call tinGetr(ival,def)
                J(pol,it,ir,i)=complex(rval,ival)
              enddo
            enddo
          enddo
          call tinClose
          write(pname(10:13),'(I4)') f
          call tinOpen(stcat(mircat,pname),'s')
          do ir=1,nr
            call tinGetr(P(ir,i),def)
          enddo
          call tinClose
        enddo
      endif
      end
        
      
      complex function trilin(v000,v001,v010,v011,v100,v101,v110,v111,
     *                        x,y,z)
      complex v000,v001,v010,v011,v100,v101,v110,v111
      real x,y,z 
c     Do trilinear interpolation (after Paul Bourke)
c     The v000 to v111 are the corners of the cube we are interpolating
c     x,y,z are the position within the unit cube (0<=x,y,z<=1)   
      trilin=v000*(1 - x)*(1 - y)*(1 - z) +
     *       v100*     x *(1 - y)*(1 - z) + 
     *       v010*(1 - x)*     y *(1 - z) + 
     *       v001*(1 - x)*(1 - y)*     z  +
     *       v101*     x *(1 - y)*     z  + 
     *       v011*(1 - x)*     y *     z  + 
     *       v110*     x *     y *(1 - z) + 
     *       v111*     x *     y *     z
      end
      
      complex function cubicInterpolate(p, x)
      complex p(4)
      real x
      cubicInterpolate = p(2) + 0.5 * x*(p(3) - p(1) + 
     *  x*(2.0*p(1) - 5.0*p(2) + 4.0*p(3) - p(4) + 
     *  x*(3.0*(p(2) - p(3)) + p(4) - p(1))))
      end


      complex function bicubicInterpolate(p,x,y)
      complex p(4,4)
      real x,y
      complex arr(4)
      complex cubicInterpolate
      arr(1) = cubicInterpolate(p(1,1), y)
      arr(2) = cubicInterpolate(p(1,2), y)
      arr(3) = cubicInterpolate(p(1,3), y)
      arr(4) = cubicInterpolate(p(1,4), y)
      bicubicInterpolate=cubicInterpolate(arr, x)
      end


      complex function tricubicInterpolate(p,x,y,z)
      complex p(4,4,4)
      real x,y,z
      complex arr(4)
      complex cubicInterpolate,bicubicInterpolate
      arr(1) = bicubicInterpolate(p(1,1,1), y, z)
      arr(2) = bicubicInterpolate(p(1,1,2), y, z)
      arr(3) = bicubicInterpolate(p(1,1,3), y, z)
      arr(4) = bicubicInterpolate(p(1,1,4), y, z)
      tricubicInterpolate=cubicInterpolate(arr, x)
      end
