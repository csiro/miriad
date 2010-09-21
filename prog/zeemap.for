      program zeemap

c= zeemap - Map the magnetic field from I and V cubes
c& nebk
c: profile analysis
c+
c       ZEEMAP tries to find the line-of-sight magnetic field splitting
c       a transition by measuring the shift in the line owing to the
c       Zeeman effect.
c
c@ iin
c       Stokes I spectral cube with channel on the first axis.
c@ vin
c       Stokes V spectral cube with channel on the first axis.
c@ b
c       Output file name for line-of-sight magnetic field.
c@ d
c       Output file name for the derivative of I/2 spectrum (op=div
c       only).
c@ be
c       Output file name for the error in B (op=div only).
c@ g
c       Output file name for amount of residual I found in V (op=fit
c       only).
c@ blc
c       Bottom left corner of box to be fit.
c@ trc
c       Top right corner of box to be fit.
c@ freq
c       Frequency of line in GHz, used to determine amount of splitting
c       per Gauss.
c@ op
c       Operation code that controls how the line shift is determined -
c         div: by dividing V by the derivative of I/2, or
c         fit: by fitting of the derivative of I and I to V.
c@ cutoff
c       For op="div", the division will not be performed unless the
c       derivative of I/2 is greater than cutoff.
c       For op="fit", the fit will not be performed unless one or more
c       channels in the I spectrum is larger in absolute value than
c       cutoff.
c@ vrms
c       RMS deviation in V spectrum in a signal free area.  This can
c       be obtained using histo.  Used for calculating error in B
c       for op=div.
c@ mode
c       This determines the algorithms used, if operation="fit". It
c       can consist of several flags:
c        l  Include leakage term,
c        2  Use 2 sided derivative approximation,
c        m  Use maximum likelihood technique,
c        x  Perform extra checks for better solutions, when
c           using the maximum likelihood technique.
c       Default mode=2m.
c@y corlen
c       Correlation length.  Selected window must be divisible by this
c       number, which is the number of pixels over which you think the
c       magnetic field might be constant.  Default is 1, try maybe 2
c       or 4.
c
c$Id: zeemap.for,v 1.3 2010/09/21 06:58:03 cal103 Exp $
c--
c
c  History:
c  Robert Loushin 28-Feb-1989
c  Neil Killeen   10-May-1989 Try to improve modularity and readability
c                             Changed only the superstructure, not the
c                             engine room
c  Robert Loushin 23-May-1989 Improve fitting algorithm using Bob
c                             Sault's ZED subroutine.  Remove C option,
c                             since a constant offset is not orthogonal
c                             to the I spectrum.
c  Bob Sault  29-May-1989     Deleted unused variables.  Moved DATA
c                             statement.
c  Neil Killeen 01-Jun-1989   Rewrite fitting option to add CORLEN.
c                             This runs about 5% slower than old code
c                             for the CORLEN=1 case.  Also add blanking
c                             mask code.
c  R. Loushin 15-Jun-1989     Fixed setscale so it gets the sign right.
c  Bob Sault  23-Jun-1989     Replaced setscale with ZedScale.
c                             Corrected call sequence to Zed.  Changed
c                             "freq" to a real.  Eliminated the F file
c                             and the fit output.
c  mjs        28-feb-1991     Calls to itoa now call itoaf.
c  nebk       01-jul-1994     Check I and V axis types
c  rjs        02-jul-1997     cellscal change
c
c-----------------------------------------------------------------------
      include 'tmpdim.h'

      real      blank
      integer   maxcor
      parameter (blank = 0.0, maxcor = 4)

      real cutoff, vrms, freq
      integer blc(3), trc(3), lunI, lunV, lunB, lunG, lunD,
     *lunBE, corlen
      character I*24, V*24, B*24, G*24, D*24, BE*24, op*3, mode*3

      real ibuf(maxdim*maxcor), vbuf(maxdim*maxcor), di(maxdim),
     *Bbuf(maxdim,maxcor), Bebuf(maxdim,maxcor), Gbuf(maxdim,maxcor),
     *iplanes(maxdim,maxdim,maxcor), vplanes(maxdim,maxdim,maxcor),
     *Gebuf(maxdim,maxcor)
      real s, scale, imin, imax, bfield, ebfield, beta, ebeta
      integer fsiz(3), siz(3), Isiz(3), Vsiz(3), Dsiz(3), n,
     *  naxis, j, k, l, jj, kk, ll, corlen2, n1
      character aline*80, ctypei*8, ctypev*8, itoaf*1
      logical noline, converge, flags(maxdim,maxcor)
c-----------------------------------------------------------------------
      call output('Zeemap: version 1.0 01-jul-94')
c
c Get inputs from user
c
      call getinp (maxdim, I, V, B, G, D, BE, blc, trc, freq, op,
     *             cutoff, vrms, mode, corlen)
c
c Open input files
c
      call xyopen (lunI, I, 'old', 3, Isiz)
      call xyopen (lunV, V, 'old', 3, Vsiz)
      do j = 1, 3
        if (Isiz(j).ne.Vsiz(j))
     *     call bug('f','Input files not same size.')

        call rdhda (lunI, 'ctype'//itoaf(j), ctypei, ' ')
        call rdhda (lunV, 'ctype'//itoaf(j), ctypev, ' ')
c
c Check axis types, as zedscale checks for correct axis
c type, but only on one image (lunI in this case)
c
        if (ctypei.ne.ctypev)
     *  call bug ('f', 'I and V images have different axis types')
      enddo
      cutoff = abs(cutoff)
c
c Compute scale to convert from a channel increment to a magnetic field
c strength (noline=.false.) or a frequency increment (noline=.true.).
c
      call ZedScale (lunI, freq, scale, noline)
c
c Check operation, output image names, windows and compute image sizes
c
      if (op.eq.' ') then
         call bug('w','No op specified.  Will do least squares fit.')
         op = 'fit'
      else if (op.ne.'div' .and. op.ne.'fit') then
         call bug('f','Specified op not available; pick div or fit.')
      endif
      call checkout (op, B, G, D, BE, vrms, mode)
      call checkwin (op, isiz, blc, trc)
      call sizes (op, blc, trc, siz, fsiz, dsiz, naxis)
      call corcheck (maxcor, corlen, fsiz)
c
c Create the output files and write their headers.
c
      call mkOut (op, lunI, lunB, lunG, lunD, lunBE, B, G, D, BE,
     *            naxis, siz, dsiz, blc, noline)
c
c Add history to output files.
c
      call history (lunB, lunG, lunD, lunBE, I, V, B, G, D, BE,
     *              blc, trc, op, cutoff, freq, vrms, mode, corlen)
c
c Tell user what's about to happen and initialize variables
c
      call telluser (op, cutoff, blc, trc, mode, corlen)
      call initvar (maxdim, maxcor, s, ibuf, vbuf, di, Bbuf,
     *              Bebuf, Gbuf, Gebuf, blank)
c
c Find B field by dividing Stokes V by the derivative of the I/2
c spectrum.  Output B and/or the derivative of I/2 if desired.
c
      if (op.eq.'div') then
        do j = blc(3), trc(3)
          call xysetpl(lunI,1,j)
          call xysetpl(lunV,1,j)
          if (B.ne.' ') call xysetpl(lunB,1,j-blc(3)+1)
          if (D.ne.' ') call xysetpl(lunD,1,j-blc(3)+1)
          if (BE.ne.' ') call xysetpl(lunBE,1,j-blc(3)+1)

          do k = blc(2), trc(2)
            call xyread(lunI,k,ibuf)
            call xyread(lunV,k,Vbuf)
            do l = blc(1)+1, trc(1)-1
              di(l-blc(1)) = (ibuf(l+1)-ibuf(l-1))*0.25
            enddo

            do l = 1, trc(1)-blc(1)-1
              if (abs(di(l)).ge.cutoff) then
                Bbuf(l,1)  = scale*Vbuf(l+blc(1))/di(l)
                Bebuf(l,1) = abs(scale*vrms/di(l))
              else
                Bbuf(l,1)  = blank
                Bebuf(l,1) = blank
              endif
            enddo

            if (B.ne.' ') call xywrite(lunB,k-blc(2)+1,Bbuf(1,1))
            if (D.ne.' ') call xywrite(lunD,k-blc(2)+1,di)
            if (BE.ne.' ') call xywrite(lunBE,k-blc(2)+1,Bebuf(1,1))
          enddo
        enddo
      else
c
c Do maximum likelihood fit
c
        n1 = trc(1) - blc(1) + 1
        corlen2 = corlen * corlen
c
c Loop over all planes in image, stepping by the correlation length
c
        do l = blc(3), trc(3), corlen
c         if (mod(l,10).eq.0 .or. l.eq.1) then
           write (aline, '(a, i4)') 'Beginning plane ', l
           call output (aline)
c         end if
c
c Read corlen planes of I and V into memory. Each plane consists of the
c full length spectrum (first axis) and the subwindow in the second axis
c specified by blc(2) and trc(2).  They are zero padded in the second
c dimension according to blc(2)
c
          do ll = 1, corlen
            call xysetpl (lunI, 1, l+ll-1)
            call xysetpl (lunV, 1, l+ll-1)
            do kk = blc(2), trc(2)
              call xyread (lunI, kk, iplanes(1,kk,ll))
              call xyread (lunV, kk, vplanes(1,kk,ll))
            enddo
          enddo
c
c Now step up second dimension processing corlen**2 spectra at a time
c
          do k = blc(2), trc(2), corlen
c
c Join corlen**2 spectra together by selecting from the relevant second
c and third dimensions.  They are in the order spectrum(k,l) where l is
c the outside loop (third dimension of cube) and k is the inside loop
c (second dimension of cube)
c
            imin = iplanes(blc(1),k,1)
            imax = imin
            n = 1
            do ll = 1, 1 + corlen - 1
              do kk = k, k + corlen - 1
                do jj = blc(1), trc(1)
                  ibuf(n) = iplanes(jj,kk,ll)
                  vbuf(n) = vplanes(jj,kk,ll)
                  imin = min(imin,ibuf(n))
                  imax = max(imax,ibuf(n))
                  n = n + 1
                enddo
              enddo
            enddo
c
c Now we have all the needed spectra joined up, so pass them to ZED.
c The output arrays are all filled starting at the blc of the input
c window, so they may be zero padded at the beginning.  K is the index
c of the first pixel in the second dimension (i.e. the first spatial
c dimension) for the current group of CORLEN**2 spectra.
c
            if (abs(imin).gt.cutoff .or. abs(imax).gt.cutoff) then
              call zed (mode, ibuf, vbuf, n1, corlen2, Bbuf(k,1),
     *                  Gbuf(k,1), Bebuf(k,1), Gebuf(k,1), s, converge)
c
c From the fit we have extracted alpha, beta, and their errors, as well
c as a spectrum (npts*corlen**2 long) which is the true stokes I
c spectrum.  V = alpha*dI/d(nu) + beta*I.
c
              if (converge) then
c
c Now apply fiddle factors to get the absolute truth and convert to
c magnetic field.
c
                bfield  = 2.0 * scale * Bbuf(k,1)
                ebfield = 2.0 * abs(scale * Bebuf(k,1))
                beta = Gbuf(k,1)
                ebeta = Gebuf(k,1)
c
c We must duplicate the results corlen**2 times to fill in the
c output images with identical numbers
c
                call arrfill (maxdim, maxcor, corlen, k, Bbuf, Bebuf,
     *                        Gbuf, Gebuf, flags, bfield, ebfield,
     *                        beta, ebeta, .true.)
              else
c
c No convergence, fill output arrays with blanks, and also turn on the
c blanking switch for later when we write the flag mask
c
                call arrfill (maxdim, maxcor, corlen, k, Bbuf, Bebuf,
     *                        Gbuf, Gebuf, flags, blank, blank,
     *                        blank, blank, .false.)
              endif
            else
c
c Below cutoff, blank results
c
                call arrfill (maxdim, maxcor, corlen, k, Bbuf, Bebuf,
     *                        Gbuf, Gebuf, flags, blank, blank,
     *                        blank, blank, .false.)
            endif
          enddo
c
c Now, for the output 2-D images, we have ready corlen rows, each of
c length trc(2)-blc(2)+1 pixels.   The second dimension of the arrays
c always runs from 1 to corlen.  L is the first of corlen planes read
c from the cube to fill the arrays ready for output at the moment.
c
          if (B.ne.' ') then
            call xysetpl (lunB, 1, 1)
            do ll = 1, corlen
              call xywrite (lunB, l-blc(3)+ll, Bbuf(blc(2),ll))
              call xyflgwr (lunB, l-blc(3)+ll, flags)
            enddo
            call xysetpl (lunB, 1, 2)
            do ll = 1, corlen
              call xywrite (lunB, l-blc(3)+ll, Bebuf(blc(2),ll))
              call xyflgwr (lunB, l-blc(3)+ll, flags)
            enddo
          endif
          if (G.ne.' ') then
            call xysetpl (lunG, 1, 1)
            do ll = 1, corlen
              call xywrite (lunG, l-blc(3)+ll, Gbuf(blc(2),ll))
              call xyflgwr (lunG, l-blc(3)+ll, flags)
            enddo
            call xysetpl (lunG, 1, 2)
            do ll = 1, corlen
              call xywrite (lunG, l-blc(3)+ll, Gebuf(blc(2),ll))
              call xyflgwr (lunG, l-blc(3)+ll, flags)
            enddo
          endif
        enddo
      endif
c
c Close up
c
      call xyclose (lunI)
      call xyclose (lunV)
      if (B.ne.' ')  call xyclose (lunB)
      if (G.ne.' ')  call xyclose (lunG)
      if (D.ne.' ')  call xyclose (lunD)
      if (BE.ne.' ') call xyclose (lunBE)

      end

c***********************************************************************

      subroutine getinp (maxdim, I, V, B, G, D, BE, blc, trc,
     *                   freq, op, cutoff, vrms, mode, corlen)

      real cutoff, vrms, freq
      integer maxdim, blc(3), trc(3), corlen
      character*(*) I, V, B, G, D, BE, op, mode
c-----------------------------------------------------------------------
c  Get user supplied inputs
c-----------------------------------------------------------------------
      integer j
c-----------------------------------------------------------------------
      call keyini
      call keya('iin',I,' ')
      call keya('vin',V,' ')
      call keya('b',B,' ')
      call keya('g',G,' ')
      call keya('d',D,' ')
      call keya('be',BE,' ')
      call keya('mode',mode,'2m')
      do j = 1, 3
        call keyi('blc',blc(j),1)
        call keyi('trc',trc(j),maxdim)
      enddo
      call keyr('freq',freq,0.0)
      call keya('op',op,' ')
      call keyr('cutoff',cutoff,0.0)
      call keyr('vrms',vrms,0.0)
      call keyi('corlen',corlen,1)
      call keyfin

      end

c***********************************************************************

      subroutine checkout (op, B, G, D, BE, vrms, mode)
      character*(*) op, B, G, D, BE, mode
      real vrms
c-----------------------------------------------------------------------
c  Check that output images are set as required for each op
c-----------------------------------------------------------------------
      if (op.eq.'div') then
        if (G.ne.' ') then
          call bug('w','Only allow B and D output images for op=div.')
          G = ' '
        endif
        if (BE.ne.' ' .and. vrms.eq.0.0) then
          call bug ('w', 'BE image output requires VRMS also')
          BE = ' '
        endif
        if (B.eq.' ' .and. D.eq.' ') call bug('f',
     *     'Must specify output file B and/or D for op=div.')
      else
        if (D.ne.' ') then
          call bug('w','D output image not allowed for op=fit')
          D = ' '
        endif
        if (BE.ne.' ') then
          call bug('w','BE output image not allowed for op=fit')
          BE = ' '
        endif
        if (vrms.ne.0.0) then
          call bug ('w', 'VRMS not used for op=fit')
          vrms = 0.0
        endif
        if (B.eq.' ' .and. G.eq.' ') then
          call bug ('f', 'No output image specified')
        endif
        if (G.ne.' ' .and. index(mode,'l').eq.0)
     *     call bug('f','G requested, but mode has no leakage term.')
      endif

      end

c***********************************************************************

      subroutine checkwin (op, isiz, blc, trc)

      character op*(*)
      integer isiz(3), blc(3), trc(3)
c-----------------------------------------------------------------------
c  Check specified window is sensible and sufficient to do analysis
c-----------------------------------------------------------------------
      integer j
c-----------------------------------------------------------------------
      do j = 1, 3
        blc(j) = max(blc(j), 1)
        trc(j) = min(trc(j), isiz(j))
        call btswap (blc(j), trc(j))
      enddo

      if ((trc(1)-blc(1).lt.7 .and. op.eq.'fit') .or.
     *    (trc(1)-blc(1).lt.2 .and. op.eq.'div'))
     *   call bug ('f', 'Not enough channels to work on')

      end

c***********************************************************************

      subroutine btswap (blc, trc)

      integer blc, trc
c-----------------------------------------------------------------------
c  Ensure trc bigger than blc.
c-----------------------------------------------------------------------
      integer itemp
c-----------------------------------------------------------------------
      if (blc.gt.trc) then
        itemp = blc
        blc = trc
        trc = itemp
      endif

      end

c***********************************************************************

      subroutine sizes (op, blc, trc, siz, fsiz, dsiz, naxis)

      character op*(*)
      integer blc(3), trc(3), siz(3), fsiz(3), dsiz(3), naxis
c-----------------------------------------------------------------------
c  Work out the sizes of the fitted area and the output images.
c-----------------------------------------------------------------------
      integer j
c-----------------------------------------------------------------------
      do j = 1, 3
        fsiz(j) = trc(j) - blc(j) + 1
      enddo

      if (op.eq.'div') then
        siz(1) = fsiz(1) - 2
        siz(2) = fsiz(2)
        siz(3) = fsiz(3)
      else
        siz(1) = fsiz(2)
        siz(2) = fsiz(3)
        siz(3) = 2
      endif

      Dsiz(1) = fsiz(1) - 2
      Dsiz(2) = fsiz(2)
      Dsiz(3) = fsiz(3)
      naxis = 3

      end

c***********************************************************************

      subroutine corcheck (maxcor, corlen, fsiz)
      integer maxcor, corlen, fsiz(*)
c-----------------------------------------------------------------------
c  Check validity of correlation length.
c-----------------------------------------------------------------------
      if (corlen.le.1) corlen = 1
      if (corlen.gt.maxcor) call bug ('f',
     *  'Correlation length too big')
      if (mod(fsiz(2),corlen).ne.0) call bug ('f',
     *  'Fitted region in second dimension not divisible by corlen')
      if (mod(fsiz(3),corlen).ne.0) call bug ('f',
     *  'Fitted region in third dimension not divisible by corlen')

      end

c***********************************************************************

      subroutine mkOut(op, lunI, lunB, lunG, lunD, lunBE, B, G, D, BE,
     *                 naxis, siz, dsiz, blc, noline)

      logical   noline
      integer   lunI, lunB, lunG, lunD, lunBE, naxis, siz(3), dsiz(3),
     *          blc(3)
      character op*(*), B*(*), G*(*), D*(*), BE*(*)
c-----------------------------------------------------------------------
c  Open the output files, copy across unchanged header keywords and
c  write new ones to the output files.
c-----------------------------------------------------------------------
      integer   axmap(3), nblc(3)
      character bunit*5
c-----------------------------------------------------------------------
c     "Brightness" units in the B and B(error) maps.
      if (noline) then
        bunit = 'HERTZ'
      else
        bunit = 'GAUSS'
      endif

c     Create the output files and copy header keywords from the I file.
      if (op.eq.'div') then
        axmap(1) = 1
        axmap(2) = 2
        axmap(3) = 3

        nblc(1) = blc(1) + 1
        nblc(2) = blc(2)
        nblc(3) = blc(3)

        if (B.ne.' ') then
          call xyopen (lunB, B, 'new', naxis, siz)
          call headcopy(lunI, lunB, axMap, 3, nblc, 0)
          call wrhda (lunB,  'bunit', bunit)
        endif

        if (D.ne.' ') then
          call xyopen (lunD, D, 'new', naxis, siz)
          call headcopy(lunI, lunD, axMap, 3, nblc, 0)
          call wrhda (lunD, 'bunit', 'JY/BEAM')
        endif

        if (BE.ne.' ') then
          call xyopen (lunBE, BE, 'new', naxis, siz)
          call headcopy(lunI, lunBE, axMap, 3, nblc, 0)
          call wrhda (lunBE, 'bunit', bunit)
        endif

      else if (op.eq.'fit') then
        axmap(1) = 2
        axmap(2) = 3
        axmap(3) = 0

        nblc(1) = blc(2)
        nblc(2) = blc(3)
        nblc(3) = 1

        if (B.ne.' ') then
          call xyopen (lunB, B, 'new', naxis, siz)
          call headcopy(lunI, lunB, axMap, 3, nblc, 0)
          call wrhda (lunB,  'bunit', bunit)
        endif

        if (G.ne.' ') then
          call xyopen (lunG, G, 'new', naxis, siz)
          call headcopy(lunI, lunG, axMap, 3, nblc, 0)
          call wrhda (lunG, 'bunit', ' ')
        endif
      endif

      end

c***********************************************************************

      subroutine history (lunB, lunG, lunD, lunBE, I, V, B, G,
     *                    D, BE, blc, trc, op, cutoff, freq, vrms,
     *                    mode, corlen)

      integer lunB, lunG, lunD, lunBE, blc(3), trc(3), corlen
      character*(*) I, V, B, G, D, BE, op, mode
      real cutoff, vrms, freq
c-----------------------------------------------------------------------
c  The history from the I image was copied as one of the header
c  keywords.  Add new history to all output files here.
c-----------------------------------------------------------------------
      character string*80
c-----------------------------------------------------------------------
      if (B.ne.' ') then
        call newhis (lunB, I, V, B, G, D, BE, blc, trc, op, cutoff,
     *               freq, mode, corlen)
        write (string,100) vrms
100     format ('ZEEMAP: vrms=',1pe14.6)
        if (op.eq.'div') call hiswrite(lunB,string)
        call hisclose(lunB)
      endif

      if (G.ne.' ') then
        call newhis (lunG, I, V, B, G, D, BE, blc, trc, op, cutoff,
     *               freq, mode, corlen)
        call hisclose(lunG)
      endif

      if (D.ne.' ') then
        call newhis (lunD, I, V, B, G, D, BE, blc, trc, op, cutoff,
     *               freq, mode, corlen)
        call hisclose(lunD)
      endif

      if (BE.ne.' ') then
        call newhis (lunBE, I, V, B, G, D, BE, blc, trc, op, cutoff,
     *               freq, mode, corlen)
        write (string,100) vrms
        if (op.eq.'div') call hiswrite (lunBE,string)
        call hisclose(lunBE)
      endif

      end

c***********************************************************************

      subroutine telluser (op, cutoff, blc, trc, mode, corlen)

      real cutoff
      integer blc(3), trc(3), corlen
      character op*3, mode*(*)
c-----------------------------------------------------------------------
c  Inform user as to what is about to happen so he/she/it can see if it
c  is what is supposed to happen.
c-----------------------------------------------------------------------
      character string*80
      character*80 umsg
c-----------------------------------------------------------------------
      if (op.eq.'div') then
        call output('Divide the V spectrum by the derivative of')
        call output('the I/2 spectrum, when the former is greater')
        write(string,100) cutoff
100     format ('than ',1pe14.6,' Jy/beam/channel')
        call output(string)

        write(string,200) blc(1)+1, trc(1)-1
200     format ('Channel range = ', i4, ' to ', i4)
        call output(string)
        write(string,300) blc(2),trc(2)
300     format ('      x-range = ', i4, ' to ', i4)
        call output(string)
        write(string,400) blc(3),trc(3)
400     format ('      y-range = ', i4, ' to ', i4)
        call output(string)
      else
        umsg = 'Do a least squares fit of the V spectrum by the ' //
     *         'derivative of the I spectrum'
        call output(umsg)
        umsg = 'and the I spectrum itself at each pixel where ' //
     *         'the I spectrum'
        call output(umsg)
        write(string,100) cutoff
        call output(string)
        write(string,200) blc(1)+1,trc(1)-1
        call output(string)
        write(string,300) blc(2),trc(2)
        call output(string)
        write(string,400) blc(3),trc(3)
        call output(string)
        if (index(mode,'m').ne.0) then
          call output ('Iterative least-squares used')
        else
          umsg = 'Warning, more biased non-iterative' //
     *           ' least squares used'
          call output (umsg)
        endif
        if (index(mode,'2').ne.0) then
          call output ('Two sided derivative used')
        else
          call output ('Warning, one sided derivative used')
        endif
        if (index(mode,'l').ne.0)
     *    call output ('Leakage term included')
        write (string,500) corlen
500     format ('Correlation length = ', i3)
        call output (string)
      endif

      end

c***********************************************************************

      subroutine newhis (lun, I, V, B, G, D, BE, blc, trc,
     *                   op, cutoff, freq, mode, corlen)

      integer lun, blc(3), trc(3), corlen
      character*(*) I, V, B, G, D, BE, op, mode
      real cutoff, freq
c-----------------------------------------------------------------------
c  Write new history to file associated with LUN.
c-----------------------------------------------------------------------
      integer len1
      character string*72
c-----------------------------------------------------------------------
      call hisopen(lun,'append')

      call hiswrite (lun, 'ZEEMAP: (MIRIAD)')

      write(string,100) I(1:len1(I)), V(1:len1(V))
100   format('ZEEMAP: Input files: I=',a, ' V=',a)
      call hiswrite(lun,string)

      if (B.ne.' ') then
        write(string,200) B(1:len1(B))
200     format('ZEEMAP: Output files: B=',a)
        call hiswrite(lun,string)
      endif

      if (G.ne.' ') then
        write(string,250) G(1:len1(G))
250     format('ZEEMAP:               G=',a)
        call hiswrite(lun,string)
      endif

      if (D.ne.' ') then
        write(string,350) D(1:Len1(D))
350     format('ZEEMAP:               D=',a)
        call hiswrite(lun,string)
      endif

      if (BE.ne.' ') then
        write(string,450) BE(1:len1(BE))
450     format('ZEEMAP:               BE=',a)
        call hiswrite(lun,string)
      endif

      write(string,500) blc(1),blc(2),blc(3)
500   format('ZEEMAP: First channel=',i4,'  blc=',i5,i6)
      call hiswrite(lun,string)

      write(string,600) trc(1),trc(2),trc(3)
600   format('ZEEMAP: Last channel=',i4,'  trc=',i5,i6)
      call hiswrite(lun,string)

      write(string,700) op,cutoff,freq
700   format('ZEEMAP: Op=',a3,', cutoff=',1pe14.6,', freq=',f6.3)
      call hiswrite(lun,string)

      if (op.eq.'fit') then
        write(string,800) mode
800     format('ZEEMAP: mode =',a3)
        call hiswrite(lun,string)

        write(string,900) corlen
900     format('ZEEMAP: correlation length = ', i3)
        call hiswrite(lun,string)
      endif

      end

c***********************************************************************

      subroutine initvar (maxdim, maxcor, s, ibuf, vbuf, di,
     *                    Bbuf, Bebuf, Gbuf, Gebuf, blank)

      integer maxdim, maxcor
      real ibuf(maxdim*maxcor), Vbuf(maxdim*maxcor), di(maxdim),
     *Bbuf(maxdim*maxcor), Bebuf(maxdim*maxcor), Gbuf(maxdim*maxcor),
     *Gebuf(maxdim*maxcor), s, blank
c-----------------------------------------------------------------------
c  Initialize arrays and variables.
c-----------------------------------------------------------------------
      integer j
c-----------------------------------------------------------------------
      s = 0.0
      do j = 1, maxdim*maxcor
        ibuf(j)  = blank
        vbuf(j)  = 0.0
        Bbuf(j)  = blank
        Bebuf(j) = blank
        Gbuf(j)  = blank
        Gebuf(j) = blank
      enddo

      do j = 1, maxdim
        di(j) = blank
      enddo

      end

c***********************************************************************

      subroutine arrfill (maxdim, maxcor, corlen, k, Bbuf, Bebuf,
     *                    Gbuf, Gebuf, flags, bfield, ebfield,
     *                    beta, ebeta, flagval)

      integer maxdim, maxcor, k, corlen
      real Bbuf(maxdim,maxcor), Bebuf(maxdim,maxcor), bfield, ebfield,
     *Gbuf(maxdim,maxcor), Gebuf(maxdim,maxcor), beta, ebeta
      logical flags(maxdim, maxcor), flagval
c-----------------------------------------------------------------------
c  Subroutine used for op=fit to fill some arrays with the results or
c  perhaps with blanks.
c-----------------------------------------------------------------------
      integer kk, ll
c-----------------------------------------------------------------------
      do ll = 1, 1 + corlen - 1
        do kk = k, k + corlen - 1
          Bbuf(kk,ll)  = bfield
          Bebuf(kk,ll) = ebfield
          Gbuf(kk,ll)  = beta
          Gebuf(kk,ll) = ebeta
          flags(kk,ll) = flagval
        enddo
      enddo

      end
