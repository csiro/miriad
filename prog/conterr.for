      program conterr

c= conterr - Continuum subtraction error estimate, when using UVLIN.
c& rjs
c: uv analysis
c+
c       Given an image of the continuum in a spectral line experiment,
c       CONTERR estimates the residual continuum artifacts that will
c       be present after continuum subtraction using UVLIN. The results
c       are intended to be indicative, rather than rigorously accurate.
c       The peak value and location of the errors are approximately
c       correct, but the actual error pattern will differ significantly
c       from that presented here.
c
c       This task essentially just evaluates the expressions given in
c       Table 1 of "An analysis of visibility-based continuum
c       subtraction" by Bob Sault.  CONTSEN makes plots of noise
c       amplification factor of tasks such as UVLIN, AVMATHS, and
c       CONTSUB.
c@ in
c       An estimate of the continuum.  It should be either a dirty image
c       or a deconvolved/restored image.  No default.
c@ order
c       The order of the polynomial fit in UVLIN, either one or two
c       numbers can be given.  If two, then CONTSEN determines the error
c       for order(1) to order(2) inclusive.  The default is to determine
c       the noise sensitivity for first order only.
c@ bw
c       Total spectral bandwidth, in GHz. Default is 0.008 (i.e. 8 MHz).
c@ fwhm
c       The image resolution, given as the FWHM of the corresponding
c       gaussian, in arcsec.  This is the same as the FWHM that RESTOR
c       would determine.  The default is to get the information from
c       the input image.  Two values can be given, being the FWHM in
c       x and y respectively (i.e. the position angle is assumed to
c       be along one of these axes).  If only one value is given, the
c       gaussian is assumed to be circularly symmetric.
c
c       Note that high order fits are very sensitive to this parmeter.
c@ out
c       An output image containing the error images. The default is
c       no output image.
c@ options
c       Extra processing options. Several can be given, separated by
c       commas. Minimum match is used. Possible values are:
c         shift  Determine an optimum shift of the phase centre to
c                be applied before the error is evaluated. This
c                shift will minimise the error in some sense.
c
c$Id: conterr.for,v 1.5 2013/08/30 01:49:21 wie017 Exp $
c--
c  History:
c    rjs  28oct93 Original version.
c    rjs  10jan94 Doc changes only.
c    rjs  02jul97 cellscal change.
c  Bugs:
c-----------------------------------------------------------------------
      integer MAXORDER
      parameter (MAXORDER = 9)

      include 'mirconst.h'
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      logical   doout,doshift, higher
      integer   i, ierr, ifax, iorder, lIn, lOut, naxis, nsize(MAXNAX),
     *          nx, ny, order(2), pCont, pErr
      real      dnu, maxerr, nu, off1, off2, thetax, thetay, x, y
      double precision cdelt1, cdelt2, crpix1, crpix2, f0, finc
      character aorder*3, hline*80, in*64, line*80, out*64, version*72

      logical   hdprsnt
      character versan*72
      external  hdprsnt, versan
c-----------------------------------------------------------------------
      version = versan('conterr',
     *                 '$Revision: 1.5 $',
     *                 '$Date: 2013/08/30 01:49:21 $')
c
c  Get the input parameters.
c
      call keyini
      call keya('in',in,' ')
      call keya('out',out,' ')
      call keyi('order',order(1),1)
      call keyi('order',order(2),order(1))
      call keyr('bw',dnu,0.008)
      call keyr('fwhm',thetax,0.0)
      call keyr('fwhm',thetay,thetax)
      call GetOpt(doshift)
      call keyfin
c
c  Check the inputs.
c
      if (in.eq.' ') call bug('f','An input must be given')
      if (order(1).lt.0 .or. order(1).gt.order(2) .or.
     *   order(2).gt.MAXORDER)
     *   call bug('f','Invalid order given')
      doout = out.ne.' '
      if (dnu.lt.0) call bug('f','Invalid bandwidth')
c
c  Open the input and get info on it.
c
      call xyopen(lIn,in,'old',MAXNAX,nsize)

      nx = nsize(1)
      ny = nsize(2)
      call rdhdd(lIn,'crpix1',crpix1,0d0)
      call rdhdd(lIn,'crpix2',crpix2,0d0)
      call rdhdd(lIn,'cdelt1',cdelt1,0d0)
      call rdhdd(lIn,'cdelt2',cdelt2,0d0)
      cdelt1 = 3600*180/pi * cdelt1
      cdelt2 = 3600*180/pi * cdelt2
      if (hdprsnt(lIn,'mask')) call bug('f',
     *  'Cannot handle blanked pixels')
c
c  Check for higher dimensions than 2.
c
      call rdhdi(lIn,'naxis',naxis,2)
      naxis = min(naxis,MAXNAX)
      higher = .false.
      do i = 3, naxis
        higher = nsize(i).gt.1
        nsize(i) = 1
      enddo
      if (higher) call bug('w',
     *  'Ignoring higher order dimensions in input')
c
c  Determine the frequency.
c
      ifax = 0
      call GetFreq(lIn,1.0,ifax,f0,finc,ierr)
      if (ierr.ne.0) call bug('f',
     *    'Failed to determine frequency of input')
      if (ifax.le.2) call bug('f',
     *    'First two dimensions must be spatial')
      nu = f0
c
c  If no beam parameters were given, try to get these from the
c  header.
c
      if (thetax.le.0 .or. thetay.le.0) call GetFWHM(lIn,thetax,thetay)
      thetax = abs(thetax/cdelt1)
      thetay = abs(thetay/cdelt2)
c
c  Create the output.
c
      if (doout) then
        nsize(3) = order(2) - order(1) + 1
        naxis = max(naxis,3)
        call xyopen(lOut,out,'new',naxis,nsize)
        call MkHed(lIn,lOut,order(1))
        call hisopen(lOut,'append')
        call hiswrite(lOut,'CONTERR: Miriad '//version)
        call hisinput(lOut,'CONTERR')
      endif
c
c  Allocate memory and read in the input.
c
      call Memalloc(pCont,nx*ny,'r')
      call Memalloc(pErr,nx*ny,'r')
      call RdIn(lIn,memR(pCont),nx,ny)
c
c  Clip the data.
c
      call ClipIt(memR(pCont),nx,ny)
c
c  Loop over the orders.
c
      do iorder = order(1), order(2)
        if (iorder.eq.1) then
          aorder='1st'
        else if (iorder.eq.2) then
          aorder='2nd'
        else if (iorder.eq.3) then
          aorder='3rd'
        else
          write(aorder,'(i1,a)') iorder,'th'
        endif
c
c  Determine the optimum shift.
c
        if (doshift) then
          call Shifty(memR(pCont),nx,ny,iorder,x,y)
          off1 = cdelt1*(x-crpix1)
          off2 = cdelt2*(y-crpix2)
        else
          x = crpix1
          y = crpix2
        endif
c
c  Determine the error.
c
        call GenErr(memR(pCont),memR(pErr),nx,ny,iorder,
     *                        x,y,dnu,nu,thetax,thetay,maxerr)
c
c  Report to the user.
c
        if (doshift) then
          write(line,'(a,a,a,1pe11.3,a,0pf7.2,a,f7.2)')
     *      'Maximum error for ',aorder,
     *      ' order fit:',maxerr,', Offset=',off1,',',off2
        else
          write(line,'(a,a,a,1pe11.3)')
     *      'Maximum error for ',aorder,
     *      ' order fit:',maxerr
        endif
        call output(line)
          write(line,'(a,f7.2,a,f7.2,a)')
        hline = 'CONTERR: '//line
        if (doout) call hiswrite(lOut,hline)
c
c  Write out the result.
c
        if (doout) then
          call xysetpl(lOut,1,iorder-order(1)+1)
          call WrOut(lOut,memR(pErr),nx,ny)
        endif
      enddo
c
c  All said and done. Close up shop.
c
      call xyclose(lIn)
      call MemFree(pCont,nx*ny,'r')
      call MemFree(pErr,nx*ny,'r')
      if (doout) then
        call hisclose(lOut)
        call xyclose(lOut)
      endif

      end
c***********************************************************************
      subroutine GetFWHM(lIn,thetax,thetay)

      integer lIn
      real thetax,thetay

c  Determine the beam parameters from the input. As a position along the
c  X or Y axis is required, make a best stab at this, and complain if
c  it is too bad.
c
c  Input:
c    lIn           Handle of the input.
c  Output:
c    thetax,thetay Resolution along X and Y axes, in arcsec.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      real bmaj,bmin,bpa
      character line*64
c-----------------------------------------------------------------------
      call rdhdr(lIn,'bmaj',bmaj,0.0)
      call rdhdr(lIn,'bmin',bmin,0.0)
      call rdhdr(lIn,'bmpa',bpa,0.0)
      if (bmaj*bmin.le.0) call bug('f','No FWHM parameters were found')

      bmaj = 180*3600/pi * bmaj
      bmin = 180*3600/pi * bmin

      if (abs(bpa).lt.45) then
        thetay = bmaj
        thetax = bmin
      else
        thetay = bmin
        thetax = bmaj
      endif

      write(line,'(a,f7.1,f7.1)')'Using FWHM parameters of',
     *                                        thetax,thetay
      call output(line)

      end
c***********************************************************************
      subroutine GenErr(Cont,Err,nx,ny,order,
     *                        x,y,dnu,nu,thetax,thetay,maxerr)

      integer nx,ny,order
      real Cont(nx,ny),Err(nx,ny),x,y,dnu,nu,thetax,thetay,maxerr

c  Generate the error for a fit of order "order".
c
c  Input:
c    nx,ny
c    Cont
c    order
c    dnu        The total bandwidth.
c    nu         The centre frequency.
c    thetax,thetay Beam FHWM in pixels, in X and Y directions.
c  Output:
c    Err
c    maxerr
c-----------------------------------------------------------------------
      include 'mirconst.h'
      integer i,j,k,n
      real fac,dist
c-----------------------------------------------------------------------
c
c  Determine the scale factor.
c
      if (mod(order,2).eq.0) then
        fac = pi**2/(2*order+6)
      else
        fac = pi/(order+2)
      endif

      do k = 1, order
        fac = pi/(2*k+1) * fac
      enddo

      n = order + 1
      fac = fac * (0.5*dnu/nu)**n
c
c  Determine the error
c
      maxerr = 0
      do j = 1, ny
        do i = 1, nx
          if (Cont(i,j).gt.0) then
            dist = sqrt(((i-x)/thetax)**2+((j-y)/thetay)**2)
            Err(i,j) = fac*Cont(i,j)*dist**n
            maxerr = max(maxerr,Err(i,j))
          endif
        enddo
      enddo

      end
c***********************************************************************
      subroutine GetOpt(doshift)

      logical doshift
c-----------------------------------------------------------------------
      integer NOPT
      parameter (NOPT=1)
      logical present(NOPT)
      character opts(NOPT)*8
      data opts/'shift   '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPT)

      doshift = present(1)
      end
c***********************************************************************
      subroutine WrOut(lOut,Data,nx,ny)

      integer lOut,nx,ny
      real Data(nx,ny)
c-----------------------------------------------------------------------
      integer j
c-----------------------------------------------------------------------
      do j = 1, ny
        call xywrite(lOut,j,Data(1,j))
      enddo

      end
c***********************************************************************
      subroutine RdIn(lIn,Data,nx,ny)

      integer lIn,nx,ny
      real Data(nx,ny)
c-----------------------------------------------------------------------
      integer j
c-----------------------------------------------------------------------
      do j = 1, ny
        call xyread(lIn,j,Data(1,j))
      enddo

      end
c***********************************************************************
      subroutine MkHed(lIn,lOut,order)

      integer lIn, lOut, order

c  Make the header for the output file.
c-----------------------------------------------------------------------
      call headcp(lIn, lOut, 0, 0, 0, 0)

      call hdcopy(lIn,lOut,'rms')

      call wrhdd(lOut, 'crpix3', 1d0)
      call wrhdd(lOut, 'crval3', dble(order))
      call wrhdd(lOut, 'cdelt3', 1d0)
      call wrhda(lOut, 'ctype3', 'FIT-ORDER')

      end
c***********************************************************************
      subroutine ClipIt(Cont,nx,ny)

      integer nx,ny
      real Cont(nx,ny)

c  Determine the rms of the image.
c
c-----------------------------------------------------------------------
      real rms,clip
      integer i,j
c-----------------------------------------------------------------------
      rms = 0
      do j = 1, ny
        do i = 1, ny
          rms = rms + Cont(i,j)*Cont(i,j)
        enddo
      enddo

      rms = sqrt(rms/(nx*ny))
      clip = 3*rms

      do j = 1, ny
        do i = 1, nx
          Cont(i,j) = abs(Cont(i,j))
          if (Cont(i,j).lt.Clip) Cont(i,j) = 0
        enddo
      enddo

      end
c***********************************************************************
      subroutine Shifty(Cont,nx,ny,order,x,y)

      integer nx,ny,order
      real Cont(nx,ny),x,y

c  Determine the optimum shift to perform to the image.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer MAXORDER
      parameter (MAXORDER=9)

      integer i,j,k,s,t
      double precision sumx(0:2*MAXORDER+1),sumy(0:2*MAXORDER+1),tx,ty
      real wt(MAXDIM),fac
      integer binom(0:2*MAXORDER+1),nmax,offset
      complex roots(2*MAXORDER+1)
c-----------------------------------------------------------------------
      if (order.lt.0 .or. order.gt.MAXORDER)
     *  call bug('f','Invalid order in shifty')
c
c  Generate the appropriate binomial coefficients.
c
      binom(0) = 1
      binom(1) = 1
      do k = 2, 2*order+1
        t = binom(0)
        do i = 1, k-1
          s = binom(i)
          binom(i) = binom(i) + t
          t = s
        enddo
        binom(k) = t
      enddo
c
c  Now sum it.
c
      nmax = max(nx,ny)
      offset = nmax/2
      if (nmax.gt.MAXDIM) call bug('f','N too big, in shifty')
      do i = 1, nmax
        Wt(i) = 1
      enddo

      fac = 1
      do k = 0, 2*order+1
        tx = 0
        ty = 0
        do j = 1, ny
          do i = 1, nx
            tx = tx + Cont(i,j) * Cont(i,j) * Wt(i)
            ty = ty + Cont(i,j) * Cont(i,j) * Wt(j)
          enddo
        enddo

        Sumx(k) = fac * binom(k) * tx
        Sumy(k) = fac * binom(k) * ty
        fac = -fac


        do i = 1, nmax
          Wt(i) = (i-offset) * Wt(i)
        enddo
      enddo
c
c  Find the root.
c
      if (Sumx(0).eq.0) call bug('f','Image looks very constant')
      call Rooted(2*order+1,Sumx,roots,x)
      call Rooted(2*order+1,Sumy,roots,y)

      x = x + offset
      y = y + offset

      end
c***********************************************************************
      subroutine Rooted(order,Coeff,roots,x)

      integer order
      real x
      double precision Coeff(0:order)
      complex roots(order)

c  Determine the roots of a poly. Only one of the roots should be
c  real valued. Return this in x.
c-----------------------------------------------------------------------
      integer ifail,j,nz
c-----------------------------------------------------------------------
      ifail = 0
      nz = order
      call dpolyzr(coeff,nz,roots,ifail)

      if (ifail.ne.0) call bug('f','Poly solver failed')

      nz = 0
      do j = 1, order
        if (aimag(roots(j)).eq.0) then
          x = roots(j)
          nz = nz + 1
        endif
      enddo

      if (nz.le.0) call bug('f','Poly solver really failed')
      if (nz.gt.1) call bug('f','Poly solver is confused')

      end
