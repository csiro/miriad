c************************************************************************
      program zeeeta
      implicit none
c
c  A MIRIAD program to calculate the Zeeman signal-to-noise
c  parameter eta;
c
c      eta  =  (dI/dnu)_rms / sigma
c           =  sqrt[SUM (dI/dnu)**2 / N] / sigma
c
c  where sigma is the standard deviation of the I or V image.
c  and N is the number of channels in the spectrum.
c
c= zeeeta - Compute Zeeman parameter eta
c& bpw
c: profile analysis
c+
c	ZEEETA is a MIRIAD task to compute the Zeeman S/N parameter 
c	eta = (dI/dnu)_rms / sigma
c@ in
c	The input Stokes I image in vxy order. No default.
c@ out
c	The output eta image, if blc,trc is not specified
c@ sigma
c	r.m.s. noise of signal free I spectrum
c@ chan
c	The channel range. Default is all channels.
c@ blc
c	Specify the blc of the spatial window in pixels.  If unset,
c	the whole image is done, with no spatial summing, and
c	an output eta image made.  If blc and trc are set, then
c	eta is computed for this window, and the results output to
c	the terminal.
c@ trc
c	Specify the trc of the spatial window in pixels
c@ der
c	1 or 2 for one or two sided derivative.  If you would
c	like to see a plot of the last derivative spectrum, include
c	a `p' as well.  E.g., 2p or 1p
c@ aveop
c	'a' to average spectra before computing eta. Only if blc,trc set
c--
c
c  History:
c    rl     jul89  Initial version.
c    nebk   jul89  Add channel range, xy, and derivative 
c                  options.  Add header and history
c                  information to output image.
c    nebk   aug89  Add blc,trc and remove xy option and last derivative plot
c		   option.
c           aug89  Add averaging option
c    rjs  27oct89  Removed plotting option, because it uses non-standard
c		   routines. Renamed it Zeeeta.
c               
c------------------------------------------------------------------------
      include 'maxdim.h'
      real sigma, buf(maxdim), bufav(maxdim), eata(maxdim), der(maxdim),
     *chan(maxdim), sumsq, dfac
      integer i, j, k, lunin, lunout, siz(3), ioff, bchan, echan, ist,
     *iend, jst, jend, kst, kend, naxis, blc(2), trc(2), nxy, nxyz, nz
      character in*24, out*24, aline*72, ctype*10, ader*2, aveop*1
      logical dutput, flags(maxdim)
c
      integer nkeys
      parameter (nkeys = 41)
      character keyw(nkeys)*8
c
      data keyw/   'bunit   ','cdelt1  ','cdelt2  ','cdelt3  ',
     *  'cdelt4  ','cdelt5  ','crota1  ','crota2  ','crota3  ',
     *  'crota4  ','crota5  ','crpix1  ','crpix2  ','crpix3  ',
     *  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *  'date-obs','epoch   ','history ','instrume','niters  ',
     *  'object  ','restfreq','telescop','vobs    ','obsra   ',
     *  'obsdec  ','observer','xshift  ','yshift  ','bmaj    ',
     *  'bmin    ','bpa     '/
c--------------------------------------------------------------------
c
c Get user inputs
c
      call output( 'Zeeta: version 1.0 27-oct-89' )
      call keyini
      call keya ('in', in, ' ')
      if (in.eq.' ') call bug ('f', 'Input image not given')
      call keya ('out', out, ' ')
      call keyr ('sigma', sigma, 0.0)
      call keyi ('chan', bchan, 1)
      call keyi ('chan', echan, 0)
      call keyi ('blc', blc(1), 0)
      call keyi ('blc', blc(2), 0)
      call keyi ('trc', trc(1), 0)
      call keyi ('trc', trc(2), 0)
      call keya ('der', ader, ' ')
      call keya ('aveop', aveop, 's')
      call keyfin
c
c Open input file
c
      call xyopen (lunin, in, 'old', 3, siz)
      if (siz(1).gt.maxdim) call bug ('f','Image too big')
      call rdhdi (lunin, 'naxis', naxis, 0)
      if (naxis.lt.3) call bug ('f', 'Image only has 2 dimensions')
      call rdhda (lunin, 'ctype1', ctype, ' ')
      if (ctype(1:4).ne.'FREQ' .and. ctype(1:4).ne.'VELO' 
     *    .and. ctype(1:4).ne.'FELO') call bug ('f', 
     *    'Input image not in correct order')
c
c Check some more inputs
c
      if (sigma.le.0.0) call bug ('f', 'sigma must be positive')
      if (bchan.le.0) bchan = 1
      if (echan.le.0 .or. echan.gt.siz(1)) echan = siz(1)
      if (blc(1).lt.0 .or. blc(1).gt.siz(2) .or. trc(1).lt.0 .or.
     *    trc(1).gt.siz(3) .or. blc(2).lt.0 .or.
     *    blc(2).gt.siz(2) .or. trc(2).lt.0 .or.
     *    trc(2).gt.siz(3) .or. trc(1).lt.blc(1) .or.
     *    trc(2).lt.blc(2)) call bug ('f','Invalid window')
c
      if (index(ader,'1').eq.0 .and. index(ader,'2').eq.0) 
     *    call bug ('f', 'Derivative type must be 1 or 2')
c
c Open output image if required and set loop indices
c
      if (blc(1).eq.0 .and. blc(2).eq.0 .and. trc(1).eq.0 .and.
     *    trc(2).eq.0) then
        if (out.eq.' ') call bug ('f', 'Output image not given')
        dutput = .true.
        call xyopen (lunout, out, 'new', 2, siz(2))
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
      end if
c
      ist = max(bchan, 2)
      if (index(ader,'1').ne.0) then
        iend = echan
        ioff = 0
        dfac = 1.0
      else
        iend = min(siz(1)-1, echan) 
        ioff = 1
        dfac = 0.5
      end if
c
c Add header and history to output file
c
      if (dutput) then
        do i = 1, nkeys
	  call hdcopy (lunin, lunout, keyw(i))
	end do
	call hisopen (lunout, 'append')
        call hiswrite (lunout, 'ZEEETA (MIRIAD)')
	aline = 'ZEEETA: in = '//in
	call hiswrite (lunout, aline)
        aline = 'ZEEETA out = '//out
	call hiswrite (lunout, aline)
        write (aline, 10) sigma
10      format ('ZEEETA: sigma = ', 1pe12.4)
	call hiswrite (lunout, aline)
        write (aline, 20) bchan, echan
20      format ('ZEEETA: channel range = ', i4, ' to ', i4)
	call hiswrite (lunout, aline)
        if (ioff.eq.0) then
          aline = 'ZEEETA: one sided derivative selected'
        else
          aline = 'ZEEETA: two sided derivative selected'
        end if
	call hiswrite (lunout, aline)
	call hisclose (lunout)
c
c Fill output row with zeros where derivative will not be evaluated
c and set flagging mask
c
        do i = 1, ist - 1
          eata(i) = 0.0 
          flags(i) = .false.
        end do
        if (iend.lt.siz(1)) then
          do i = iend + 1, siz(1)
            eata(i) = 0.0
            flags(i) = .false.
          end do
        end if
        do i = ist, iend
          flags(i) = .true.
        end do
      end if
c
c Loop over image. Do average and summing options separately
c
      if (.not.dutput .and. aveop.eq.'a') then
c
c Initialize average spectrum
c
        do i = 1, siz(1)
          bufav(i) = 0.0
        end do
c
c Make average spectrum
c
        nxy = (jend - jst + 1) * (kend - kst + 1)
        do k = kst, kend
           call xysetpl (lunin, 1, k)
           do j = jst, jend
              call xyread (lunin, j, buf)
              do i = 1, siz(1)
                bufav(i) = bufav(i) + buf(i)/nxy
              end do
           end do
         end do
c
c Compute eta
c
         sumsq = 0.0
         do i = ist, iend
            chan(i) = i
            der(i) = dfac * (bufav(i+ioff) - bufav(i-1))
            sumsq = sumsq + der(i)**2
         end do
         nz = (iend - ist + 1) 
         eata(1) = sqrt(sumsq/nz) / sigma
      else
        sumsq = 0.0
        do k = kst, kend
           call xysetpl (lunin, 1, k)
           do j = jst, jend
              call xyread (lunin, j, buf)
c
c Compute derivative, sum of squares, and eta
c
              if (dutput) sumsq = 0.0
              do i = ist, iend
                 chan(i) = i
                 der(i) = dfac * (buf(i+ioff) - buf(i-1))
                 sumsq = sumsq + der(i)**2
              end do
              if (dutput) eata(j) = sqrt(sumsq/(iend-ist+1)) / sigma
           end do
           if (dutput) then
             call xywrite (lunout, k, eata)
             call xyflgwr (lunout, k, flags) 
           end if
        end do
        nxyz = (iend - ist + 1) * (jend - jst + 1) * (kend - kst + 1) 
        eata(1) = sqrt(sumsq/nxyz) / sigma
      end if
c
c Plot last spectrum and derivative  if required
c
c      if (index(ader,'p').ne.0 .or. index(ader,'P').ne.0) then
c        call xycurve (iend-ist+1, chan(ist), buf(ist), -1)
c        call xycurve (iend-ist+1, chan(ist), der(ist), -2)
c        call xydraw ('channel', 'derivative', ' ', .false.,
c     *                .false., 0, .false.)
c        call xyend
c      end if
c
c Close up and report answer if necessary
c
      call xyclose (lunin)
      if (.not.dutput) then
        write (*, 30) bchan, echan, blc(1), blc(2), trc(1), trc(2), 
     *                sigma, ader, aveop, eata(1)
30      format (/, '  Channel range = ', i4, ' to ', i4,
     *          /, ' spatial window = ', i4, ',', i4, ' to ', i4, ',',i4
     *          /, '          sigma = ', 1pe12.4, 
     *          /, '     derivative = ', a,
     *          /, ' averaging mode = ', a, 
     *          /, '            eta = ', 1pe12.4, /)
      else
        call xyclose (lunout)
      end if
c
      end
