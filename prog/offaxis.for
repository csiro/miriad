      program  offaxis

c= offaxis -- Remove (or simulate) ATCA off-axis polarisation effects
c& rjs
c: uv analysis
c+
c       OFFAXIS is a Miriad task that attempts to remove off-axis
c       instrumental polarisation from ATCA observations. As input,
c       OFFAXIS takes a visibility dataset and a model of the source
c       total intensity (Stokes I). Given this, it computes the expected
c       off-axis effects (given its internal models of the effects),
c       subtracts this from the data, and produces an output (nominally)
c       corrected dataset.
c
c@ vis
c       Input visibility dataset. No default.
c@ select
c       Normal visibility selection parameter. The default is to
c       select all data. See the help on "select" for more information.
c@ line
c       Normal line parameter with the normal defaults. See the help on
c       "line" for more information.
c@ stokes
c       Stokes/polarisations to be processed.  The default is
c       XX,YY,XY,YX.  The only other legitimate value is I.
c@ model
c       Input model of the total intensity image.  This should be the
c       apparent total intensity (i.e. not corrected for primary beam
c       attenuation) in Jy/pixel.  No default.
c@ out
c       The name of the output, corrected, visibility dataset.
c       No default.
c@ clip
c       Pixels in the model less (in absolute value) than the clip level
c       are treated as if they are 0. The default is 0.
c@ options
c       Task enrichment parameters. Several can be given, separated by
c       commas. Only the minimum number of characters to guarantee
c       uniqueness are needed. Possible options are:
c         replace   Normally OFFAXIS subtracts the computed off-axis
c                   response from the data. This option causes the
c                   visibility to be replaced with the model.
c         nocal     Do not apply any antenna gain calibration.
c                   The default is to apply these if they are available.
c         nopol     Do not apply any polarization leakage correction.
c                   The default is to apply these if they are available.
c         nopass    Do not apply bandpass calibration. The default is to
c                   apply these calibrations if they are available.
c
c$Id: offaxis.for,v 1.6 2018/12/04 04:02:11 wie017 Exp $
c--
c  History:
c    rjs  02may96 Original version.
c    rjs  12dec96 Improve checks on reasonableness and error messages.
c    rjs  07apr97 Include improved 13-cm fits, and make it work at
c                 L-band as well.
c    rjs  03may00 Now supports C and X band.
c    rjs  25jul00 Simple support for stokes keyword.
c    rjs  11sep00 Correct bug in above.
c    dpr  20mar01 Doc change only
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'

      integer   MAXPOL, PolI, PolXX, PolYY, PolXY, PolYX
      parameter(MAXPOL=4)
      parameter(PolI=1, PolXX=-5, PolYY=-6, PolXY=-7, PolYX=-8)

      logical   flags(MAXCHAN,4), first, replace

      integer   i, j, nCmp, nchan, npol, nsize(2), nx, ny, pFlux, pPsi,
     *          pRad, pll, pmm, pols(MAXPOL), tMod, tOut, tVis
      real      chi, clip
      double precision preamble(5), sfreq(MAXCHAN)
      complex   sim(MAXCHAN,4), uvdata(MAXCHAN,4)
      character ltype*16, model*64, out*64, uvflags*16, version*72

c     Externals.
      logical   uvDatOpn
      character itoaf*8, versan*72
c-----------------------------------------------------------------------
      version = versan('offaxis',
     :                 '$Revision: 1.6 $',
     :                 '$Date: 2018/12/04 04:02:11 $')
c
c  Get and check the inputs.
c
      call keyini
      call GetOpt(replace,uvflags)
      call uvDatInp('vis',uvflags)
      call keya('model',model,' ')
      call keya('out',out,' ')
      call keyr('clip',clip,0.0)
      call keyfin

      if(clip.lt.0.0)call bug('w','Clip level set to 0')
      clip = max(clip,0.0)
      if(model.eq.' ')call bug('f','An input model must be given')
      if(out.eq.' ')call bug('f','An output dataset must be given')
c
c  We must process all the 4 polarisation parameters.
c
      call uvDatGti('npol',npol)
      if(npol.eq.0)then
        call uvDatSet('stokes',PolXX)
        call uvDatSet('stokes',PolYY)
        call uvDatSet('stokes',PolXY)
        call uvDatSet('stokes',PolYX)
        npol = 4
      else
        call uvDatGti('pols',pols)
        if(npol.eq.4)then
          if(pols(1).ne.PolXX.or.pols(2).ne.PolYY.or.
     *       pols(3).ne.PolXY.or.pols(4).ne.PolYX)
     *         call bug('f','Invalid polarizations selected')
        else if(npol.eq.1)then
          if(pols(1).ne.PolI)
     *         call bug('f','Invalid polarizations selected')
        else
          call bug('f','Invalid number of polarizations to process')
        endif
      endif
c
c  Open the model, get the components.
c
      call xyopen(tMod,model,'old',2,nsize)
      nx = nsize(1)
      ny = nsize(2)

      call memAlloc(pFlux,nx*ny,'r')
      call memAlloc(pll,nx*ny,'r')
      call memAlloc(pmm,nx*ny,'r')
      call GetMod(tMod,nx,ny,clip,nx*ny,
     *  nCmp,memr(pFlux),memr(pll),memr(pmm))
      call output('Number of components found: '//itoaf(nCmp))

      call xyclose(tMod)
c
c  Compute the radius and position angle of each component.
c
      call memAlloc(pRad,nCmp,'r')
      call memAlloc(pPsi,nCmp,'r')
      call ToPolar(nCmp,memr(pll),memr(pmm),memr(pRad),memr(pPsi))
c
c  Now we want to process the visibility data.
c
      first = .true.
      dowhile(uvDatOpn(tVis))
        call uvDatGta('ltype',ltype)
        call VarInit(tVis,ltype)
        if(first)then
          first = .false.
          call uvopen(tOut,out,'new')
          call uvset(tOut,'preamble','uvw/time/baseline',
     *               0,0.d0,0.d0,0.d0)
          call hdcopy(tVis,tOut,'history')
          call hisOpen(tOut,'append')
          call hisWrite(tOut,'OFFAXIS: Miriad '//version)
          call hisInput(tOut,'OFFAXIS')
          call hisClose(tOut)
          call uvputvri(tOut,'npol',npol,1)
          call wrhdi(tOut,'npol',npol)
        endif
        call VarOnit(tVis,tOut,ltype)

        call uvDatRd(preamble,uvdata(1,1),flags(1,1),MAXCHAN,nchan)
        if(nchan.eq.0)call bug('f','No appropriate data were found')
        dowhile(nchan.gt.0)
          do j=2,npol
            call uvDatRd(preamble,uvdata(1,j),flags(1,j),MAXCHAN,nchan)
          enddo
          call uvinfo(tVis,'sfreq',sfreq)
c
c  Generate the simulated stuff.
c
          call uvgetvrr(tVis,'chi',chi,1)
          call Compute(sfreq,preamble,sim,nchan,MAXCHAN,chi,
     *     memr(pFlux),memr(pll),memr(pmm),memr(pRad),memr(pPsi),nCmp)
c
c  Difference or replace it now.
c
          if (npol.eq.1) then
            do i=1,nchan
              sim(i,1) = 0.5*(sim(i,1) + sim(i,2))
            enddo
          endif

          do j=1,npol
            if (replace) then
              do i=1,nchan
                uvdata(i,j) = sim(i,j)
              enddo
            else
              do i=1,nchan
                uvdata(i,j) = uvdata(i,j) - sim(i,j)
              enddo
            endif
          enddo
c
c  Finish up and go back for more.
c
          call VarCopy(tVis,tOut)
          do i=1,4
            call uvputvri(tOut,'pol',-4-i,1)
            call uvwrite(tOut,preamble,uvdata(1,i),flags(1,i),nchan)
          enddo
          call uvDatRd(preamble,uvdata,flags,MAXCHAN,nchan)
        enddo
        call uvDatCls
      enddo
c
c  All said and done.
c
      call memFree(pRad,nCmp,'r')
      call memFree(pPsi,nCmp,'r')
      call memFree(pFlux,nx*ny,'r')
      call memFree(pll,nx*ny,'r')
      call memFree(pmm,nx*ny,'r')
      call uvclose(tOut)
      end
c***********************************************************************
      subroutine Compute(sfreq,uv,sim,nchan,maxchan,chi,
     *                                flux,ll,mm,Rad,Psi,nCmp)

      integer nchan,maxchan,nCmp
      complex sim(maxchan,4)
      double precision sfreq(nchan),uv(2)
      real flux(nCmp),ll(nCmp),mm(nCmp),Rad(nCmp),Psi(nCmp),chi

c  Compute the expected response for a particular component.
c
c  Inputs:
c    sfreq      Sky frequency of each channel, in GHz.
c    uv         UV coordinates, in nanosec.
c    nchan      Number of channels.
c    maxchan    Dimension of the sim array.
c    flux       Flux density of each component.
c    ll,mm      Cartesian coordinate of each component.
c    rad,psi    Polar coordinate of the component (both radians).
c    chi        Parallactic angle (radians).
c  Output:
c    sim        Expected response to the given field.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   i, j
      real      cxx, cxy, cyx, cyy, Jo(2,2), omega, pb, theta
      complex   vis, XX, XY, YX, YY
c-----------------------------------------------------------------------
      do j = 1, nchan
        XX = (0.0,0.0)
        YY = (0.0,0.0)
        XY = (0.0,0.0)
        YX = (0.0,0.0)

        omega = TWOPI * sfreq(j)
        do i = 1, nCmp
          call atJones(rad(i),psi(i)-chi,sfreq(j),Jo,pb)

c         Coherence matrix elements.
          cxx = Jo(1,1)*Jo(1,1) + Jo(1,2)*Jo(1,2)
          cxy = Jo(1,1)*Jo(2,1) + Jo(1,2)*Jo(2,2)
          cyx = cxy
          cyy = Jo(2,1)*Jo(2,1) + Jo(2,2)*Jo(2,2)

c         Model Stokes I visibility.
          theta = omega * (uv(1)*ll(i) + uv(2)*mm(i))
          vis = (flux(i) / pb) * cmplx(cos(theta),sin(theta))

          XX = XX + vis * (cxx - pb)
          XY = XY + vis *  cxy
          YX = YX + vis *  cyx
          YY = YY + vis * (cyy - pb)
        enddo

        sim(j,1) = XX
        sim(j,2) = YY
        sim(j,3) = XY
        sim(j,4) = YX
      enddo

      end
c***********************************************************************
      subroutine GetMod(tMod,nx,ny,clip,maxCmp,nCmp,flux,ll,mm)

      integer tMod,nx,ny,maxCmp,nCmp
      real clip,flux(maxCmp),ll(maxCmp),mm(maxCmp)

c  Get components above a particular clip level.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical   flags(MAXDIM), valid
      integer   i, iax, ih, iostat, j, mosize
      real      map(MAXDIM)
      double precision cdelt1, cdelt2, crpix1, crpix2, equ(2), pix(2), t
      character bunit*16, text*80

c     Externals.
      logical hdprsnt
      integer hsize
c-----------------------------------------------------------------------
      if (nx.gt.MAXDIM) call bug('f','Image too big for me')

      call rdhda(tMod,'bunit',bunit,'JY/PIXEL')
      call lcase(bunit)
      if (index(bunit,'/pixel').eq.0) then
        call bug('w','Input model is not in units of Jy/pixel')
        call bug('w',' ... this may be VERY unwise')
      endif

      call coInit(tMod)
      call coFindAx(tMod,'stokes',iax)
      if (iax.ne.0) then
        call coCvt1(tMod,iax,'ap',1d0,'aw',t)
        if (nint(t).ne.1) then
          call bug('w','Input model is not a Stokes-I one')
          call bug('w',' ... this operation may make no sense')
        endif
      endif

c     Find the pointing centre.
      if (hdprsnt(tMod,'mostable')) then
c       Read the pointing centre from the mosaic table.
        call output('Reading pointing centre from mosaic table.')

        call haccess(tMod, ih, 'mostable', 'read', iostat)
        if (iostat.ne.0) call bugno('f', iostat)

        mosize = hsize(ih)
        if (mosize.ne.56) then
          if (mosize.gt.56) then
            call bug('f','Mosaic table contains multiple entries.')
          else
            call bug('f','Mosaic table appears to be corrupted.')
          endif
        endif

        call hreadd(ih, equ(1), 16, 8, iostat)
        if (iostat.ne.0) call bugno('f', iostat)
        call hreadd(ih, equ(2), 24, 8, iostat)
        if (iostat.ne.0) call bugno('f', iostat)
        call hdaccess(ih, iostat)
        if (iostat.ne.0) call bugno('f', iostat)

        call coCvtv(tMod, 'aw/aw', equ, 'ap/ap', pix, valid)
        crpix1 = pix(1)
        crpix2 = pix(2)
      else
c       Regular synthesis image.
        call output('Assuming pointing centre at the reference pixel.')
        call rdhdd(tMod, 'crpix1', crpix1, dble(nx/2+1))
        call rdhdd(tMod, 'crpix2', crpix2, dble(ny/2+1))
      endif

      write (text, 10) crpix1, crpix2
 10   format ('Pointing centre at pixel',f9.2,',',f9.2,'.')
      call output(text)

      call rdhdd(tMod, 'cdelt1', cdelt1, 0d0)
      call rdhdd(tMod, 'cdelt2', cdelt2, 0d0)
      if (cdelt1.eq.0d0 .or. cdelt2.eq.0d0)
     *  call bug('f','Pixel increments missing from the dataset')

c     Load the model map.
      nCmp = 0
      do j = 1, ny
        call xyread(tMod,j,map)
        call xyflgrd(tMod,j,flags)
        do i = 1, nx
          if (flags(i) .and. abs(map(i)).gt.clip. and.
     *      (nint(i-crpix1).ne.0 .or. nint(j-crpix2).ne.0)) then
            nCmp = nCmp + 1
            if (nCmp.gt.maxCmp) call bug('f','Too many components')
            flux(nCmp) = map(i)
            ll(nCmp) = cdelt1*(i-crpix1)
            mm(nCmp) = cdelt2*(j-crpix2)
          endif
        enddo
      enddo

      end
c***********************************************************************
      subroutine ToPolar(nCmp,ll,mm,Rad,Psi)

      integer nCmp
      real ll(nCmp),mm(nCmp),Rad(nCmp),Psi(nCmp)
c
c  Convert the (l,m) coordinates to the polar form used to determine
c  the Jones matrix.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i=1,nCmp
        Rad(i) = sqrt(ll(i)*ll(i) + mm(i)*mm(i))
        Psi(i) = atan2(ll(i),mm(i))
      enddo

      end
c***********************************************************************
      subroutine GetOpt(replace,uvflags)

      character uvflags*(*)
      logical replace

c  Get processing options.
c-----------------------------------------------------------------------
      integer NOPTS
      parameter(NOPTS=4)
      character opts(NOPTS)*8
      logical present(NOPTS)

      data opts/'replace ','nocal   ','nopol   ','nopass  '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)
      replace = present(1)

      uvflags = 'dl3s'
      if(.not.present(2))uvflags(7:7) = 'c'
      if(.not.present(3))uvflags(5:5) = 'e'
      if(.not.present(4))uvflags(6:6) = 'f'

      end
