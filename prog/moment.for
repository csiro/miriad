c***********************************************************************
        program Moment
c
c= MOMENT -  Calculate moments of a Miriad image.
c& mchw
c: image analysis
c+
c       MOMENT calculates the nth moment of a Miriad image.  The
c       spectral axis, which is determined automatically, must be on
c       axis 1, 2, or 3.
c@ in
c       The input image.  No default.
c@ region
c       Region of the input image to be used, only the bounding box is
c       supported.  See documentation for region on how to specify this.
c@ out
c       The output image.  No default.
c@ mom
c       -3      Velocity at peak intensity using a three-point quadratic
c               fit around the peak (km/s).
c       -2      Peak intensity using a three-point quadratic fit (units
c               same as individual channels).
c       -1      Average intensity (units same as individual channels).
c        0      0th moment = sum(I) (integrated intensity, I x km/s).
c        1      1st moment = sum(I*v)/sum(I), (velocity centroid, km/s),
c                            where v is the velocity.
c        2      2nd moment = sqrt(sum(I*(v-M1)**2)/sum(I)), (velocity
c                            dispersion, km/s), where M1 is the first
c                            moment.  If the line profile is Gaussian,
c                            this produces a map of Gaussian sigma, and
c                            FWHM = 2*sqrt(2*ln(2))*sigma = 2.355*sigma.
c
c               The moments are calculated independently for each pixel
c               using spectral channels with intensity satisfying the
c               specified clip range.
c
c               For FREQ axes the radio velocity is computed from the
c               line rest frequency recorded in the header.  For VELO
c               and FELO axes the axis scale is used directly.
c@ clip
c       Two values.  For mom >= -1, exclude spectral channels with
c       intensity in the range clip(1) to clip(2) inclusive.  If only
c       one value is given, then exclude -abs(clip) to +abs(clip).
c       Default = 0.
c@ rngmsk
c       For mom > 0, mask pixels when the 1st moment lies outside the
c       range of the spectral axis.  This can happen because sum(I)
c       can be arbitrarily small in emission-free channels but with
c       sum(I*v) non-vanishing.  Default: false.
c@ pkmask
c       Two values.  For mom = -3, 1, and 2 (velocities) mask pixels
c       for which the peak intensity lies within the range pkmask(1) to
c       pkmask(2) exclusive.  If only one value is given, then mask
c       pixels with peak intensity less than pkmask.  Default = 0.
c--
c  History:
c    26may89 Robert Loushin  original version - v-axis of vxy image
c                  only.
c    25jun90 mchw  Major update and rework to allow for other image
c                  axes.
c                       Fixed a bug for 2nd and higher moments.
c    13nov90 mchw  Added suport for xyv images, and pixel blanking.
c                       Made 2nd moment work and removed higher moments.
c    08feb91 mchw  Two values for clip range, and version in history.
c    25feb91 mjs   Changed references of itoa to itoaf.
c    01aug91 rjs   Use boxSet(...'s')
c    05aug91 alr/pjt Changed clip from include to exclude range.
c                       Write out dummy 3rd. axis.
c    06aug91 mchw       Merge in above changes.
c    11jul92 pjt   Fixed rather serious flagging bug when axis=1
c    26nov92 nebk  Add btype
c     4jan93 pjt   Fixed rather serious flagging bug when axis=3
c     5feb93 rjs   Use memalloc.
c     5apr93 pjt   Since rjs is walkabouting, I had to fix that amazing
c                  bug: indexor's (i0,j0) in moment3 wrong if blc,trc
c                  used
c    04jan96 nebk  Write header descriptors as double precision
c    26nov96 rjs   Increase length of input and output file names.
c    07mar97 rjs   Improve a message.
c    30jun97 rjs   Change units of 0th moment image to be jy/beam.km/s
c    23jul97 rjs   Added pbtype.
c    27feb98 mwp   Added mom=-2 for peak temperature
c    13jul00 rjs   Copy across llrot keyword.
c    12feb01 pjt   Mask pixels if their velocity is out of range
c    15feb01 dpr   Truncate rangemask key to rngmsk - doh!
c    16feb01 pjt   Added mom=-3 for velocity of peak fit to poly=2
c    18jan02 pjt   Turned rngmask typo into rngmsk (duh)
c     4mar02 pjt   documented FWHM/sigma, fixed units of mom=2 map
c
c $Id: moment.for,v 1.8 2010/04/15 05:54:47 cal103 Exp $
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'
      include 'mem.h'

      integer maxnax,maxboxes,maxruns
      parameter(maxnax=3,maxboxes=2048)
      parameter(maxruns=3*MAXDIM)

      logical rngmsk
      integer axis, blc(maxnax), boxes(maxboxes), i, j, lIn, lOut,
     :        mom, n1, n2, n3, naxes, naxis(maxnax), nchan,
     :        oaxis(maxnax), pMasks, pSpecs, trc(maxnax)
      real    blo, bhi, cdelt, clip(2), crpix, crval, pkmask(2), offset,
     :        restfreq, scl, tmp, vrange(2)
      character cin*1, ctype*9, in*64, text*72, out*64, version*80

c     Externals.
      logical hdprsnt, keyprsnt
      character itoaf*1, versan*80
c-----------------------------------------------------------------------
      version = versan ('moment',
     :                  '$Revision: 1.8 $',
     :                  '$Date: 2010/04/15 05:54:47 $')

c     Get inputs.
      call keyini
      call keya('in',in,' ')
      call BoxInput('region',in,boxes,maxboxes)
      call keya('out',out,' ')
      call keyi('mom',mom,0)
      call keyr('clip',clip(1),0.0)
      if (keyprsnt('clip'))then
        call keyr('clip',clip(2),0.0)
      else
        clip(2) = abs(clip(1))
        clip(1) = -clip(2)
      endif
      call keyl('rngmsk',rngmsk,.FALSE.)
      call keyr('pkmask',pkmask(2),-1e9)
      if (keyprsnt('pkmask'))then
        call keyr('pkmask',pkmask(1),-1e9)
      else
        pkmask(1) = -1e9
      endif
      call keyfin

c     Check inputs.
      if (in.eq.' ')  call bug('f','No input specified. (in=)')
      if (out.eq.' ') call bug('f','No output specified. (out=)')

      if (mom.lt.-3 .or. mom.gt.2) then
        call bug('f','moment must be between -3 and 2')
      endif

      if (clip(2).lt.clip(1)) then
        tmp = clip(1)
        clip(1) = clip(2)
        clip(2) = tmp
        call bug('w', 'clip limits swapped.')
      endif

      if (pkmask(2).lt.pkmask(1)) then
        tmp = pkmask(1)
        pkmask(1) = pkmask(2)
        pkmask(2) = tmp
        call bug('w', 'pkmask limits swapped.')
      endif


c     Open input cube and get parameters.
      call xyopen(lIn,in,'old',maxnax,naxis)
      call rdhdi(lIn,'naxis',naxes,0)
      naxes = min(naxes,maxnax)
      if (naxis(1).gt.MAXDIM)call bug('f','Input file too big for me')

c     Determine the min and max value.
      call ImMinMax(lIn,naxes,naxis,blo,bhi)
      if (blo.eq.bhi)then
        call xyclose(lIn)
        write (text, 10) blo
 10     format ('All pixels are ',1pg10.3)
        call bug('f', text)
      endif

c     Set up the region of interest.
      call BoxSet(boxes,maxnax,naxis,'s')
      call BoxInfo(boxes,maxnax,blc,trc)

c     Locate the spectral axis.
      axis = 0
      do i = 1, naxes
        cin = itoaf(i)
        call rdhda(lIn, 'ctype'//cin, ctype, ' ')
        if (ctype(:4).eq.'VELO' .or.
     :      ctype(:4).eq.'FELO' .or.
     :      ctype(:4).eq.'FREQ') then
          axis = i
          goto 20
        endif
      enddo

 20   if (axis.lt.1) then
        call bug('f', 'Spectral axis not found.')
      else if (axis.gt.3) then
        call output('Reorder axes to take moment of axis 1, 2, or 3.')
        call bug('f', 'axis not implemented.')
      endif

c     Calculate offset and scale to convert from channels to km/s.
      if (hdprsnt(lIn, 'crpix'//cin) .and.
     :    hdprsnt(lIn, 'crval'//cin)) then
        call rdhdr(lIn,'crpix'//cin, crpix, 0.0)
        call rdhdr(lIn,'crval'//cin, crval, 0.0)
      else
        call bug('f', 'crpix and/or crval not in header.')
      endif

      call rdhdr(lIn, 'cdelt'//cin, cdelt, 0.0)
      if (cdelt.eq.0.0) call bug('f','cdelt is 0 or not present.')

c     Compute velocity parameters.
      if (ctype(1:4).eq.'FREQ') then
        call rdhdr(lIn, 'restfreq', restfreq, 0.0)
        if (restfreq.eq.0.0) then
          call bug('f','restfreq not present in header.')
        endif

        offset = crpix - (crval - restfreq) / cdelt
        scl = -CMKS * (cdelt / restfreq) * 1e-3
      else
        offset = crpix - crval/cdelt
        scl = cdelt
      endif

c     Transform to pixel coordinates of output axis.
      offset = offset - blc(axis) + 1

c     Report the velocity range.
      nchan = trc(axis) - blc(axis) + 1
      vrange(1) = (    1 - offset)*scl
      vrange(2) = (nchan - offset)*scl

      if (vrange(2).lt.vrange(1)) then
        tmp = vrange(1)
        vrange(1) = vrange(2)
        vrange(2) = tmp
      endif

      write (text, 30) vrange(1), vrange(2), scl
 30   format ('Velocity range (km/s):',f10.2,' to',f10.2,' by',f8.2)
      call output (text)

      if (mom.lt.1 .or. .not.rngmsk) then
c       Don't enforce velocity range for 2nd and 3rd moment.
        vrange(1) = 0.0
        vrange(2) = 0.0
      endif

c     Open output image and write its header.
      j = 0
      do i = 1, naxes
        if (blc(i).lt.1) blc(i) = 1
        if (trc(i).gt.naxis(i)) trc(i) = naxis(i)
        if (i.ne.axis) then
          j = j + 1
          oaxis(j) = trc(i) - blc(i) + 1
        endif
      enddo
      oaxis(naxes) = 1
      call xyopen(lOut,out,'new',naxes,oaxis)
      call header(lIn,lOut,naxes,blc,trc,mom,axis)

c     Compute the moment.
      if (axis.eq.1) then
        call ax1mom(lIn,lOut,naxes,blc,trc,mom,offset,scl,clip,vrange,
     :    pkmask)

      else if (axis.eq.2) then
        n1 = trc(1) - blc(1) + 1
        n2 = trc(2) - blc(2) + 1
        call memalloc(pSpecs,n1*n2,'r')
        call memalloc(pMasks,n1*n2,'l')
        call ax2mom(lIn,lOut,naxes,blc,trc,mom,offset,scl,clip,vrange,
     :    pkmask,n1,n2,memr(pSpecs),meml(pMasks))
        call memfree(pSpecs,n1*n2,'r')
        call memfree(pMasks,n1*n2,'l')

      else if (axis.eq.3) then
        n1 = trc(1) - blc(1) + 1
        n3 = trc(3) - blc(3) + 1
        call memalloc(pSpecs,n1*n3,'r')
        call memalloc(pMasks,n1*n3,'l')
        call ax3mom(lIn,lOut,naxes,blc,trc,mom,offset,scl,clip,vrange,
     :    pkmask,n1,n3,memr(pSpecs),meml(pMasks))
        call memfree(pSpecs,n1*n3,'r')
        call memfree(pMasks,n1*n3,'l')
      endif

c     Update history and close files.
      call Hisopen(lOut,'append')
      call HisWrite(lOut,'MOMENT: '//version)
      call HisInput(lOut,'MOMENT')
      call HisClose(lOut)
      call xyclose(lIn)
      call xyclose(lOut)

      end

c=======================================================================

      subroutine header(lIn,lOut,naxes,blc,trc,mom,axis)
      integer lIn,lOut,naxes,blc(naxes),trc(naxes),mom,axis
c
c  Copy keywords to output file.
c
c  Inputs:
c    lIn,lOut   Handle of input and output files.
c    naxes      The number of input axes.
c    blc,trc    The corners of the input image.
c    mom        The moment to be calculated.
c    axis       The axis for which the moment is calculated.
c-----------------------------------------------------------------------
      character atemp*16,ctype*10,cin*2,cout*2
      double precision rtemp,cdelt,crval,crpix,idx
      integer i,j,k,l

c     Externals.
      character itoaf*2
      integer len1
      logical hdprsnt

c     nkeys and nckeys must match the number of keywords.
      integer nkeys,nckeys
      parameter (nkeys=23, nckeys=4)
      character keyw(nkeys)*8, ckeyw(nckeys)*5

      data keyw/   'bmaj    ','bmin    ','bpa     ',
     :    'obstime ','epoch   ','history ','llrot   ',
     :    'ltype   ','lstart  ','lstep   ','lwidth  ','pbfwhm  ',
     :    'instrume','niters  ','object  ','telescop','pbtype  ',
     :    'restfreq','vobs    ','observer','obsra   ',
     :    'obsdec  ','mostable'/

c     Keyvalues that must be changed as they are passed from in to out.
      data ckeyw/'ctype','cdelt','crval','crota'/
c-----------------------------------------------------------------------
c     Copy unchanged header keywords.
      do i = 1,nkeys
        call hdcopy(lIn,lOut,keyw(i))
      enddo

c     Handle keywords that must be moved to another axis.
      j = 0
      do i = 1, naxes
        if (i.ne.axis) then
          j = j + 1
          cin = itoaf(i)
          cout = itoaf(j)
          atemp = ckeyw(1)//cin
          call rdhda(lIn,ckeyw(1)//cin,atemp,' ')
          if (atemp.ne.' ')call wrhda(lOut,ckeyw(1)//cout,atemp)
          do k = 2,nckeys
            atemp = ckeyw(k)//cin
            if (hdprsnt(lIn,atemp)) then
              call rdhdd(lIn,ckeyw(k)//cin,rtemp,0.0d0)
              call wrhdd(lOut,ckeyw(k)//cout,rtemp)
            endif
          enddo

c         Special cases: the crpix change for a subcube.
          if (hdprsnt(lIn,'crpix'//cin)) then
            call rdhdd(lIn,'crpix'//cin,rtemp,0.0d0)
            rtemp = rtemp - dble(blc(i)) + 1
            call wrhdd(lOut,'crpix'//cout,rtemp)
          endif
        endif
      enddo

      if (mom.eq.-3) then
        call wrhda(lOut,'bunit','km/s')
        call wrbtype(lOut,'velocity')
      else if (mom.le.-1) then
        call hdcopy(lIn,lOut,'bunit')
      else if (mom.eq.0) then
        call rdhda(lIn,'bunit',atemp,' ')
        l = len1(atemp)
        if (l.gt.0) then
          atemp(l+1:) = '.km/s'
          call wrhda(lOut,'bunit',atemp)
        endif
      else
        call wrhda(lOut,'bunit','km/s')
        if (mom.eq.1) then
          call wrbtype(lOut,'velocity')
        else
          call wrbtype(lOut,'velocity_dispersion')
        endif
      endif

c     Record additional information about the ``third'' dummy axis.
c     Also the third dimension is 1 (naxis3=1), the axes are labeled
c     as much as possible from the input cube.
      cin = itoaf(axis)
      idx = (blc(axis)+trc(axis))/2.0
      call rdhdd(lIn,'crpix'//cin, crpix, 1d0)
      call rdhdd(lIn,'cdelt'//cin, cdelt, 1d0)
      call rdhdd(lIn,'crval'//cin, crval, 0d0)
      call rdhda(lIn,'ctype'//cin, ctype, ' ')
      crval = crval + (idx-crpix)*cdelt
      cdelt = cdelt * (trc(axis)-blc(axis)+1)
      call wrhdd(lOut,'crpix3', 1d0)
      call wrhdd(lOut,'cdelt3', cdelt)
      call wrhdd(lOut,'crval3', crval)
      call wrhda(lOut,'ctype3', ctype)
      end

c=======================================================================

      subroutine ax1mom(lIn,lOut,naxes,blc,trc,mom,offset,scl,clip,
     :  vrange,pkmask)

      integer lIn, lOut, naxes, blc(naxes), trc(naxes), mom
      real    offset, scl, clip(2), vrange(2), pkmask(2)
c
c  Moment calculation for axis 1.
c
c  Inputs:
c    lIn,lOut   Handle of input and output files.
c    naxes      The number of input axes.
c    blc,trc    The corners of the input image.
c    mom        The moment to be calculated. Default = 0.
c    offset     Offset and...
c    scl        ...scale factor to convert from channels to km/s.
c    clip       Exclude channels with intensity in range clip(1) to
c               clip(2).
c    vrange     For mom > 0, flag pixels if 1st moment is outside
c               velocity range vrange(1) to vrange(2).
c    pkmask     For mom = -3, 1, and 2, mask pixels for which the peak
c               intensity lies within the specified range.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical inmask(MAXDIM), outmsk(MAXDIM)
      integer j, j0, k, k0, n1, n2, n3
      real    inbuf(MAXDIM), outbuf(MAXDIM)
c-----------------------------------------------------------------------
      n1 = trc(1) - blc(1) + 1
      n2 = trc(2) - blc(2) + 1
      n3 = trc(3) - blc(3) + 1

      k0 = blc(3)
      do k = 1, n3
        call xysetpl(lIn,1,k0)

        j0 = blc(2)
        do j = 1, n2
          call xyread(lIn,j0,inbuf)
          call xyflgrd(lIn,j0,inmask)

          call momcalc(mom, offset, scl, clip, vrange, pkmask, n1,
     :      inbuf(blc(1)), inmask(blc(1)), outbuf(j), outmsk(j))

          j0 = j0 + 1
        enddo

c       Write out this row of the moment map.
        call xywrite(lOut, k, outbuf)
        call xyflgwr(lOut, k, outmsk)

        k0 = k0 + 1
      enddo

      end

c=======================================================================

      subroutine ax2mom(lIn,lOut,naxes,blc,trc,mom,offset,scl,clip,
     :  vrange,pkmask,n1,n2,specs,masks)

      integer lIn, lOut, naxes, blc(naxes), trc(naxes), mom
      real    offset, scl, clip(2), vrange(2), pkmask(2)
      integer n1, n2
      real    specs(n2,n1)
      logical masks(n2,n1)
c
c  Moment calculation for axis 2.
c
c  Inputs:
c    lIn,lOut   Handle of input and output files.
c    naxes      The number of input axes.
c    blc,trc    The corners of the input image.
c    mom        The moment to be calculated.
c    offset     Offset and...
c    scl        ...scale factor to convert from channels to km/s.
c    clip       Exclude channels with intensity in range clip(1) to
c               clip(2).
c    vrange     For mom > 0, flag pixels if 1st moment is outside
c               velocity range vrange(1) to vrange(2).
c    pkmask     For mom = -3, 1, and 2, mask pixels for which the peak
c               intensity lies within the specified range.
c  Scratch:
c    specs      Storage for spectra.
c    masks      Storage for flags.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical inmask(MAXDIM), outmsk(MAXDIM)
      integer i, i0, j, j0, k, k0, n3
      real    inbuf(MAXDIM), outbuf(MAXDIM)
c-----------------------------------------------------------------------
c     Check consistency.
      if (n1.ne.trc(1)-blc(1)+1 .or. n2.ne.trc(2)-blc(2)+1) then
        call bug('f', 'Dimension inconsistency in AX2MOM')
      endif
      n3 = trc(3) - blc(3) + 1

      k0 = blc(3)
      do k = 1, n3
        call xysetpl(lIn,1,k0)

        j0 = blc(2)
        do j = 1, n2
          call xyread(lIn,j0,inbuf)
          call xyflgrd(lIn,j0,inmask)

          i0 = blc(1)
          do i = 1, n1
            specs(j,i) = inbuf(i0)
            masks(j,i) = inmask(i0)
            i0 = i0 + 1
          enddo

          j0 = j0 + 1
        enddo

        do i = 1, n1
          call momcalc(mom, offset, scl, clip, vrange, pkmask, n2,
     :      specs(1,i), masks(1,i), outbuf(i), outmsk(i))
        enddo

        call xywrite(lOut, k, outbuf)
        call xyflgwr(lOut, k, outmsk)

        k0 = k0 + 1
      enddo

      end

c=======================================================================

      subroutine ax3mom(lIn,lOut,naxes,blc,trc,mom,offset,scl,clip,
     :  vrange,pkmask,n1,n3,specs,masks)

      integer lIn, lOut, naxes, blc(naxes), trc(naxes), mom
      real    offset, scl, clip(2), vrange(2), pkmask(2)
      integer n1, n3
      real    specs(n3,n1)
      logical masks(n3,n1)
c
c  Moment calculation for axis 3.
c
c  Inputs:
c    lIn,lOut   Handle of input and output files.
c    naxes      The number of input axes.
c    blc,trc    The corners of the input image.
c    mom        The moment to be calculated.
c    offset     Offset and...
c    scl        ...scale factor to convert from channels to km/s.
c    clip       Exclude channels with intensity in range clip(1) to
c               clip(2).
c    vrange     For mom > 0, flag pixels if 1st moment is outside
c               velocity range vrange(1) to vrange(2).
c    pkmask     For mom = -3, 1, and 2, mask pixels for which the peak
c               intensity lies within the specified range.
c  Scratch:
c    specs      Storage for spectra.
c    masks      Storage for flags.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical inmask(MAXDIM), outmsk(MAXDIM)
      integer i, i0, j, j0, k, k0, n2
      real    inbuf(MAXDIM), outbuf(MAXDIM)
c-----------------------------------------------------------------------
c     Check consistency.
      if (n1.ne.trc(1)-blc(1)+1 .or. n3.ne.trc(3)-blc(3)+1) then
        call bug('f', 'Dimension inconsistency in AX3MOM')
      endif
      n2 = trc(2) - blc(2) + 1

      j0 = blc(2)
      do j = 1, n2
        k0 = blc(3)
        do k = 1, n3
          call xysetpl(lIn,1,k0)
          call xyread(lIn,j0,inbuf)
          call xyflgrd(lIn,j0,inmask)

          i0 = blc(1)
          do i = 1, n1
            specs(k,i) = inbuf(i0)
            masks(k,i) = inmask(i0)
            i0 = i0 + 1
          enddo

          k0 = k0 + 1
        enddo

        do i = 1, n1
          call momcalc(mom, offset, scl, clip, vrange, pkmask, n3,
     :      specs(1,i), masks(1,i), outbuf(i), outmsk(i))
        enddo

        call xywrite(lOut, j, outbuf)
        call xyflgwr(lOut, j, outmsk)

        j0 = j0 + 1
      enddo

      end

c=======================================================================

      subroutine momcalc(mom, offset, scl, clip, vrange, pkmask, nchan,
     :  spec, mask, moment, flag)

c  Calculate the required moment for a single spectrum.
c
c  Inputs:
c    mom        The moment to be calculated.
c    offset     Offset and...
c    scl        ...scale factor to convert from channels to km/s.
c    clip       Exclude channels with intensity in range clip(1) to
c               clip(2).
c    vrange     For mom > 0, flag pixels if 1st moment is outside
c               velocity range vrange(1) to vrange(2).
c    pkmask     For mom = -3, 1, and 2, mask pixels for which the peak
c               intensity lies within the specified range.
c    nchan      Number of channels in spectrum and mask.
c    spec       Spectrum.
c    mask       Input mask, false if data is rejected.
c  Outputs:
c    moment     The required moment.
c    flag       False if moment failed range tests.
c
c-----------------------------------------------------------------------
      integer nchan
      logical dovflg, flag, mask(nchan)
      integer i, ipeak, mom
      real    a, b, clip(2), pkmask(2), mom1, mom2sq, moment, offset,
     :        peak, scl, spec(nchan), sum0, sum1, sum2, vel, vrange(2),
     :        xpeak
c-----------------------------------------------------------------------
c     Accumulate data for this spectrum.
      sum0 = 0.0
      sum1 = 0.0
      sum2 = 0.0

      ipeak = 1
      do i = 1, nchan
        if (mask(i)) then
          sum0 = sum0 + spec(i)

          if (spec(i).gt.spec(ipeak)) then
            ipeak = i
          endif

          if (mom.ge.1) then
            if (spec(i).lt.clip(1) .or. clip(2).lt.spec(i)) then
              vel = (i - offset)*scl
              sum1 = sum1 + spec(i)*vel
              sum2 = sum2 + spec(i)*vel*vel
            endif
          endif
        endif
      enddo


c     Do quadratic fit to peak position if needed.
      dovflg = mom.eq.-3 .or. mom.ge.1
      if (mom.le.-2 .or. dovflg) then
        xpeak = 0.0
        if (ipeak.gt.1 .and. ipeak.lt.nchan) then
          a = 0.5*(spec(ipeak+1) + spec(ipeak-1)) - spec(ipeak)
          b = 0.5*(spec(ipeak+1) - spec(ipeak-1))
          if (a.ne.0.0) xpeak = -b / (2.0*a)
        endif

c       Peak intensity used for flagging.
        peak = spec(ipeak) + xpeak*b/2.0
      endif


c     Calculate the required moment.
      moment = 0.0
      flag   = .false.
      if (mom.le.-2) then
        if (mom.eq.-2) then
c         Peak intensity.
          moment = peak
          flag = .true.
        else if (mom.eq.-3) then
c         Velocity of peak.
          moment = ((ipeak + xpeak) - offset)*scl
          flag   = .true.
          vel    = moment
        endif

      else if (mom.eq.-1) then
c       Average line intensity.
        moment = sum0 / nchan
        flag = .true.

      else if (mom.eq.0) then
c       Integrated line intensity.
        moment = sum0 * abs(scl)
        flag = .true.

      else if (sum0.ne.0.0) then
c       Velocity centroid or dispersion.
        mom1 = sum1 / sum0
        vel  = mom1

        if (mom.eq.1) then
          moment = mom1
          flag   = .true.
        else
          mom2sq = sum2/sum0 - mom1*mom1
          if (mom2sq.gt.0.0) then
            moment = sqrt(mom2sq)
            flag   = .true.
          endif
        endif
      endif


c     Do velocity flagging?
      if (dovflg .and. flag) then
c       Flagging based on expected range.
        if (vrange(1).lt.vrange(2)) then
          if (vel.lt.vrange(1) .or. vrange(2).lt.vel) then
            flag = .false.
          endif
        endif

c       Flagging based on peak line strength.
        if (pkmask(1).lt.peak .and. peak.lt.pkmask(2)) then
          flag = .false.
        endif
      endif

      end
