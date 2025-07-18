      program zeeeta

c= zeeeta - Compute Zeeman parameter eta
c& bpw
c: profile analysis
c+
c       ZEEETA is a MIRIAD task to compute the Zeeman signal-to-noise
c       parameter eta;
c
c       eta  =  (dI/dnu)_rms / sigma
c            =  sqrt[SUM (dI/dnu)**2 / N] / sigma
c
c       where sigma is the standard deviation of the I or V image, and N
c       is the number of channels in the spectrum.
c
c@ in
c       The input Stokes I image in vxy order.  No default.
c@ out
c       The output eta image, if blc,trc is not specified.
c@ sigma
c       r.m.s. noise of signal free I spectrum.
c@ chan
c       The channel range.  Default is all channels.
c@ blc
c       Specify the blc of the spatial window in pixels.  If unset, the
c       whole image is done, with no spatial summing, and an output eta
c       image made.  If blc and trc are set, then eta is computed for
c       this window, and the results output to the terminal.
c@ trc
c       Specify the trc of the spatial window in pixels
c@ der
c       1 or 2 for one or two sided derivative.  If you would like to
c       see a plot of the last derivative spectrum, include a 'p' as
c       well, e.g. 2p or 1p.
c@ aveop
c       'a' to average spectra before computing eta.  Only if blc,trc
c       set.
c
c--
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical   dutput, flags(MAXDIM)
      integer   axnum, bchan, blc(2), echan, i, iend, ioff, ist, j,
     *          jend, jst, k, kend, kst, lIn, lOut, naxis, nxy, nxyz,
     *          nz, siz(3), trc(2)
      real      buf(MAXDIM), bufav(MAXDIM), chan(MAXDIM), der(MAXDIM),
     *          dfac, eata(MAXDIM), sigma, sumsq
      character ader*2, aveop*1, in*24, out*24, version*72

      external  versan
      character versan*72
c-----------------------------------------------------------------------
      version = versan('zeeeta')

c     Get user inputs.
      call keyini
      call keya('in', in, ' ')
      if (in.eq.' ') call bug('f', 'Input image not given')
      call keya('out', out, ' ')
      call keyr('sigma', sigma, 0.0)
      call keyi('chan', bchan, 1)
      call keyi('chan', echan, 0)
      call keyi('blc', blc(1), 0)
      call keyi('blc', blc(2), 0)
      call keyi('trc', trc(1), 0)
      call keyi('trc', trc(2), 0)
      call keya('der', ader, ' ')
      call keya('aveop', aveop, 's')
      call keyfin

c     Open the input cube and do basic checks.
      call xyopen(lIn, in, 'old', 3, siz)
      if (siz(1).gt.MAXDIM) call bug('f','Image too big')
      call rdhdi(lIn, 'naxis', naxis, 0)
      if (naxis.lt.3) call bug('f', 'Image has less than 3 axes')

      call coInit(lIn)
      call coFindAx(lIn, 'spectral', axnum)
      call coFin(lIn)
      if (axnum.ne.1) call bug('f', 'Input image not in vxy order')

c     Check some more inputs.
      if (sigma.le.0.0) call bug('f', 'sigma must be positive')
      if (bchan.le.0) bchan = 1
      if (echan.le.0 .or. echan.gt.siz(1)) echan = siz(1)
      if (blc(1).lt.0 .or. blc(1).gt.siz(2) .or. trc(1).lt.0 .or.
     *    trc(1).gt.siz(3) .or. blc(2).lt.0 .or.
     *    blc(2).gt.siz(2) .or. trc(2).lt.0 .or.
     *    trc(2).gt.siz(3) .or. trc(1).lt.blc(1) .or.
     *    trc(2).lt.blc(2)) call bug('f','Invalid window')

      if (index(ader,'1').eq.0 .and. index(ader,'2').eq.0)
     *    call bug('f', 'Derivative type must be 1 or 2')

c     Open output image if required and set loop indices.
      if (blc(1).eq.0 .and. blc(2).eq.0 .and.
     *    trc(1).eq.0 .and. trc(2).eq.0) then
        if (out.eq.' ') call bug('f', 'Output image not given')
        dutput = .true.
        call xyopen(lOut, out, 'new', 2, siz(2))
        jst = 1
        jend = siz(2)
        kst = 1
        kend = siz(3)
      else
        dutput = .false.
        jst = blc(1)
        jend = trc(1)
        kst = blc(2)
        kend = trc(2)
      endif

      ist = max(bchan, 2)
      if (index(ader,'1').ne.0) then
        iend = echan
        ioff = 0
        dfac = 1.0
      else
        iend = min(siz(1)-1, echan)
        ioff = 1
        dfac = 0.5
      endif

c     Add header and history to output file.
      if (dutput) then
        call headcp(lIn, lOut, 0, 0, 0, 0)

        call hisopen(lOut, 'append')
        call hiswrite(lOut, 'ZEEETA: MIRIAD' // version)
        call hisinput(lOut, 'ZEEETA')
        call hisclose(lOut)

c       Fill output row with zeros where derivative will not be
c       evaluated and set flagging mask.
        do i = 1, ist - 1
          eata(i) = 0.0
          flags(i) = .false.
        enddo

        if (iend.lt.siz(1)) then
          do i = iend + 1, siz(1)
            eata(i) = 0.0
            flags(i) = .false.
          enddo
        endif

        do i = ist, iend
          flags(i) = .true.
        enddo
      endif

c     Loop over image. Do average and summing options separately.
      if (.not.dutput .and. aveop.eq.'a') then
c       Initialize average spectrum.
        do i = 1, siz(1)
          bufav(i) = 0.0
        enddo

c       Make average spectrum.
        nxy = (jend - jst + 1) * (kend - kst + 1)
        do k = kst, kend
          call xysetpl(lIn, 1, k)
          do j = jst, jend
            call xyread(lIn, j, buf)
            do i = 1, siz(1)
              bufav(i) = bufav(i) + buf(i)/nxy
            enddo
          enddo
        enddo

c       Compute eta.
        sumsq = 0.0
        do i = ist, iend
          chan(i) = i
          der(i) = dfac * (bufav(i+ioff) - bufav(i-1))
          sumsq = sumsq + der(i)**2
        enddo

        nz = (iend - ist + 1)
        eata(1) = sqrt(sumsq/nz) / sigma

      else
        sumsq = 0.0
        do k = kst, kend
          call xysetpl(lIn, 1, k)
          do j = jst, jend
            call xyread(lIn, j, buf)

c           Compute derivative, sum of squares, and eta.
            if (dutput) sumsq = 0.0
            do i = ist, iend
              chan(i) = i
              der(i) = dfac * (buf(i+ioff) - buf(i-1))
              sumsq = sumsq + der(i)**2
            enddo
            if (dutput) eata(j) = sqrt(sumsq/(iend-ist+1)) / sigma
          enddo

          if (dutput) then
            call xywrite(lOut, k, eata)
            call xyflgwr(lOut, k, flags)
          endif
        enddo

        nxyz = (iend - ist + 1) * (jend - jst + 1) * (kend - kst + 1)
        eata(1) = sqrt(sumsq/nxyz) / sigma
      endif

c     Close up and report answer if necessary.
      call xyclose(lIn)
      if (.not.dutput) then
        write(*, 30) bchan, echan, blc(1), blc(2), trc(1), trc(2),
     *                sigma, ader, aveop, eata(1)
30      format(/,'  Channel range = ',i4,' to ',i4,
     *          /,' spatial window = ',i4,',',i4,' to ',i4,',',i4
     *          /,'          sigma = ',1pe12.4,
     *          /,'     derivative = ',a,
     *          /,' averaging mode = ',a,
     *          /,'            eta = ',1pe12.4,/)
      else
        call xyclose(lOut)
      endif

      end
