c***********************************************************************

      subroutine modpcomp(uvw,nchan,sfreq,vis)

      integer nchan
      double precision uvw(3),sfreq(nchan)
      complex vis(nchan)
c-----------------------------------------------------------------------
      include 'modp.h'
      include 'mirconst.h'

      integer i,j
      real theta,tflux,epsi
      complex ctemp
c-----------------------------------------------------------------------
c     Determine the decorrelation factor (bandwidth and time smearing).
      call modpDeco(uvw,sfreq,ucoeff,vcoeff,wcoeff,lmn,factor,nsrc,
     *  inttime,wts,nwts,cosd,sind)

      do j = 1, nchan
        ctemp = 0
        if (freq0.gt.0) then
          epsi = log(real(sfreq(j)/freq0))
        else
          epsi = 0
        endif
        do i = 1, nsrc
          tflux = (flux(1,i) + epsi*flux(2,i)) * factor(i)
          theta = 2*DPI*sfreq(j)*(uvw(1)* lmn(1,i) +
     *                            uvw(2)* lmn(2,i) +
     *                            uvw(3)*(lmn(3,i) - 1))/sfreq(1)
          ctemp = ctemp + tflux*cmplx(cos(theta),sin(theta))
        enddo
        vis(j) = vis(j) + ctemp
      enddo

      end

c***********************************************************************

      subroutine modpDeco(uvw,freq0,ucoeff,vcoeff,wcoeff,lmn,
     *  factor,nsrc,inttime,wts,nwts,cosd,sind)

      integer nsrc,nwts
      real inttime,factor(nsrc),wts(nwts)
      double precision uvw(3),lmn(3,nsrc),freq0
      double precision ucoeff(3),vcoeff(3),wcoeff(3),cosd,sind
c-----------------------------------------------------------------------
      include 'mirconst.h'

c     Rotation rate of the Earth in radians/sec.
      real omegae
      parameter (omegae=2*PI/86400*366.25/365.25)

      double precision ll,mm,nn,uu,vv,ww
      real theta,bsmear,tsmear
      integer i,j

      external  dsinc
      double precision dsinc
c-----------------------------------------------------------------------
c     Convert to coordinates at the delay centre.
      uu = ucoeff(1)*uvw(1) + ucoeff(2)*uvw(2) + ucoeff(3)*uvw(3)
      vv = vcoeff(1)*uvw(1) + vcoeff(2)*uvw(2) + vcoeff(3)*uvw(3)
      ww = wcoeff(1)*uvw(1) + wcoeff(2)*uvw(2) + wcoeff(3)*uvw(3)

      do j = 1, nsrc
        ll = ucoeff(1)*lmn(1,j)+ucoeff(2)*lmn(2,j)+ucoeff(3)*lmn(3,j)
        mm = vcoeff(1)*lmn(1,j)+vcoeff(2)*lmn(2,j)+vcoeff(3)*lmn(3,j)
        nn = wcoeff(1)*lmn(1,j)+wcoeff(2)*lmn(2,j)+wcoeff(3)*lmn(3,j)

c       Bandwidth smearing factor.
        theta = 0.256/freq0*(ll*uu+mm*vv+(nn-1)*ww)
        bsmear = 0
        do i = 1, nwts
          bsmear = bsmear + wts(i)*(dsinc(0.5d0*(theta+i-1)) +
     *                               dsinc(0.5d0*(theta-i+1)))
        enddo

c       Time smearing factor. This assumes an east-west array.
        theta = (-ll*vv/sind+mm*uu*sind-(nn-1)*uu*cosd)*omegae*inttime
        tsmear = dsinc(dble(theta))

c       Overall factor.
        factor(j) = bsmear*tsmear
      enddo

      end

c***********************************************************************

      subroutine modpini(tvis,tmod)

      integer tmod,tvis
c-----------------------------------------------------------------------
c  Load the point source file form a image dataset.
c-----------------------------------------------------------------------
      include 'modp.h'

      logical ok
      integer tcmp,iostat,k1,k2,i,length,iax,coObj
      double precision dtemp,radel,decdel,x1(2),radec(2)
      character fluxst*32,line*132

      external  dsinc, len1
      integer   len1
      double precision dsinc
c-----------------------------------------------------------------------
c     Determine the reference frequency.
      call coInit(tmod)
      call coFindAx(tmod,'frequency',iax)
      if (iax.gt.0) then
        call coVelSet(tmod,'frequency')
        call coCvt1(tmod,iax,'op',0d0,'aw',freq0)
      else
        freq0 = 0
      endif
      call coFin(tmod)

c     Get a handle coordinate handle for the visibility dataset.
      call coInit(tvis)
      x1(1) = 0
      x1(2) = 0
      call coCvt(tvis,'op/op',x1,'aw/aw',radec)

c     Determine characteristics about the delay tracking centre.
      call uvrdvrd(tvis,'delra',radel,radec(1))
      call uvrdvrd(tvis,'deldec',decdel,radec(2))
      call uvrdvrr(tvis,'inttime',inttime,10.0)

      cosd = cos(decdel)
      sind = sin(decdel)

c     Compute rotation matrices to convert (u,v,w) and (l,m,n) to
c     be relative to the delay tracking centre.
      call coRaDec(coObj,'NCP',radel,decdel)
      call coGeom(coObj,'aw/aw',radec,ucoeff,vcoeff,wcoeff)
      call coFin(coObj)

c     Get the lag weights. This assumes the 33 channel/128 MHz system,
c     with options=reweight used during ATLOD.
      call lagwt(wts,64,0.04)
      nwts = 32
      do i = 2, 32
        wts(i) = 2*wts(i)*(1.0-real(i-1)/32.0)
      enddo

c     Normalise the lag weights.
      dtemp = 0
      do i = 1, nwts
        dtemp = dtemp + wts(i)*2*dsinc(0.5d0*(i-1))
      enddo
      do i = 1, nwts
        wts(i) = wts(i) / dtemp
      enddo

c     Get a handle for the input dataset.
      call haccess(tmod,tcmp,'sources','read',iostat)
      if (iostat.ne.0) call bugno('f',iostat)

c     Loop over all the sources.
      nsrc = 0
      call hreada(tcmp,line,iostat)
      do while (iostat.eq.0)
        k1 = 1
        k2 = len1(line)
        if (k2.gt.0 .and. line(1:1).ne.'#') then
          nsrc = nsrc + 1
          call modprd(tvis,line,k1,k2,lmn(1,nsrc))
          do i = 1, 2
            call getfield(line,k1,k2,fluxst,length)
            if (length.gt.0) then
              call atodf(fluxst(1:length),dtemp,ok)
              if (.not.ok) call bug('f',
     *                'Error decoding number in sources table')
              flux(i,nsrc) = dtemp
            else
              flux(i,nsrc) = 0
            endif
          enddo
        endif

        call hreada(tcmp,line,iostat)
      enddo

      if (iostat.ne.-1) then
        call bug('w','Error while reading source table')
        call bugno('f',iostat)
      endif
      call hdaccess(tcmp,iostat)

      call cofin(tvis)

      end

c***********************************************************************

      subroutine modprd(tvis,line,k1,k2,lmn)

      integer k1,k2,tvis
      character line*(*)
      double precision lmn(3)
c-----------------------------------------------------------------------
      logical   ok
      integer   declen, ralen
      double precision x(2)
      character decst*32, rast*32
c-----------------------------------------------------------------------
      call getfield(line,k1,k2,rast,ralen)
      call getfield(line,k1,k2,decst,declen)
      if (ralen.le.0 .or. declen.le.0) call bug('f',
     *        'Error in the format of RA/DEC in source table')
      call decangle(rast,x(1),'hms',ok)
      if (ok) call decangle(decst,x(2),'dms',ok)
      if (.not.ok) call bug('f',
     *        'Error in decoding RA/DEC in source table')
      call colmn(tvis,'aw/aw',x,lmn)

      end
