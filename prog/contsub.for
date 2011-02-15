      program contsub

c= CONTSUB - Subtract continuum from a datacube
c& bpw
c: map manipulation
c+
c       CONTSUB is used to subtract the continuum from a datacube, which
c       can be arbitrarily oriented. Several subtraction algorithms are
c       possible, selected using the mode keyword. The maximum length of
c       the frequency axis is 8192 pixels.
c
c< in
c
c< region
c       Anything outside the region is untouched (only rectangular boxes
c       are recognized by contsub).
c
c< out
c       For modes poly, mean and avgs either this keyword or cont= must
c       be specified, for mode subtr it must always be given.  This
c       output dataset will contain only those channels from the input
c       dataset selected by the 'region' keyword.
c
c@ cont
c       An optional output dataset which will be one plane deep and
c       contain the continuum map.  For mode=
c         poly: the interpolated map at the channel halfway between the
c               first and last line channel selected.
c         mean: the averaged image that was subtracted from the channel
c               maps.
c         avgs: the average of the two average images that are
c               calculated (see description of mode keyword).
c         subtr: an input set which will be subtracted from each plane
c               of the input dataset.
c       For modes poly, mean and avgs either this keyword or out= must
c       be specified, while for mode subtr it must always be given.
c
c@ contchan
c       Select channels that supposedly contain continuum signal only. A
c       selection is a list of ranges. A range is specified as (z1,z2),
c       meaning channels z1 through z2, or as (z3) for a single channel.
c       Ranges are separated by commas. E.g.: (z1,z2),(z3),(z4,z5).
c       For options 'poly' (or 'mean'), the fit (or average), is done
c       using all selected continuum channels. For option 'avgs', the
c       selection of continuum channels must consist of two ranges.
c       The 'contchan' keyword always refers to frequency channels,
c       independent of whether the frequency axis is the 'z'-axis, the
c       'x'-axis or any other axis.
c       (Not used for mode='subtr').
c
c@ mode
c       Selects the continuum-determination algorithm.  In each case the
c       operation is done separately for each image pixel.  May be
c       abbreviated.  The default is 'poly,1'.
c
c       'poly,#' - a polynomial fit of order # is made through the
c       selected continuum channels.  The interpolated continuum
c       intensity is subtracted from each separate channels.  The
c       maximum order is 8.
c
c       'mean' - the average intensity of the selected continuum
c       channels is calculated and the average is subtracted from each
c       channel (equivalent to 'poly,0').
c
c       'avgs' - an average of the first range and of the second range
c       of channels is calculated and a linear interpolation between the
c       two averages is subtracted from each channel.
c
c       'subtr' - at each position, subtract the value in the dataset
c       given by cont= from the profile at the same position in the
c       input dataset.
c
c@ options
c       Select which output to write with the cont keyword.  By default
c       the continuum is written.  If options=coeff,# one of the
c       coefficients of the polynomial fit is written instead. '#' gives
c       the order for which the coefficient is written (e.g. if
c       cont=a+bx: 0='a', 1='b', etc).
c
c@ verbose
c       verbose=true makes contsub print out some info every now and
c       then.
c
c@ velaxis
c       For datacubes where the 'VELO' axis-identification does not
c       occur in the header, this can be used to indicate which
c       direction is the velocity (channel) axis.  The value given must
c       be one of 'x', 'y', 'z', 'a', 'b', .... If velaxis is not given
c       and the 'VELO' axis is not present, contsub assumes 'z'.
c
c$Id: contsub.for,v 1.3 2011/02/15 04:32:59 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c The main program first gets all inputs and then calls the workhorse.
c The inputs are:
c unitinp:     handle of the input dataset
c unitout:     handle of the output (continuum-subtracted) dataset
c unitcon:     handle of the dataset that will contain the continuum
c nprofiles:   number of spectra in cube
c nchan:       number of channels in spectrum
c algorithm:   string containing name of algorithm
c contchan:    list of channel numbers to use to determine continuum
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer   contchan(0:MAXCHAN), nchan, nprofiles, opts(2),
     *          unitcon, unitinp, unitout
      character algorithm*10, version*72

      external  versan
      character versan*72
c-----------------------------------------------------------------------
      version = versan('contsub',
     *                 '$Revision: 1.3 $',
     *                 '$Date: 2011/02/15 04:32:59 $')

      call inputs(unitinp, unitout, unitcon, nprofiles, nchan,
     *            algorithm, opts, contchan)
      call work(unitinp, unitout, unitcon, nprofiles, nchan,
     *            algorithm, opts, contchan)
      call finish(unitinp, unitout, unitcon, version,
     *            algorithm, contchan)

      end

c***********************************************************************

      subroutine inputs(unitinp, unitout, unitcon, nprofiles, nchan,
     *                  algorithm,opts, contchan)

      integer   unitinp, unitout, unitcon, nprofiles, nchan
      character algorithm*(*)
      integer   opts(*), contchan(0:*)
c-----------------------------------------------------------------------
c Inputs reads all keyword values and transforms them to variables
c usable in work.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      integer   MAXBOXES
      parameter (MAXBOXES = 1024)

c If MAXTERMS changes, MAXTERMS in polyfit must also change and
c data algopts must be adapted
      integer   NALG, MAXTERMS, NOPT, NOUT, NOPTO
      parameter (NALG  = 4, MAXTERMS = 9, NOUT = 1)
      parameter (NOPT  =  NALG + MAXTERMS)
      parameter (NOPTO =  NOUT + MAXTERMS)

      logical   optprsnt(NOPT), verbose
      integer   axlen(MAXNAX), axnum(MAXNAX), blc(MAXNAX),
     *          boxes(MAXBOXES), i, inpblc(MAXNAX), inptrc(MAXNAX),
     *          maxranges, n, naxis, naxis1, nterms, oaxlen(MAXNAX),
     *          trc(MAXNAX), velaxnr, viraxlen(MAXNAX),
     *          vircubesize(MAXNAX)
      character algopts(NOPT)*10, con*1024, inp*1024, itoaf*1, out*1024,
     *          outopts(NOPTO)*10, type*5, velaxis*1

      data algopts /'poly', 'mean', 'avgs', 'subtr',
     *               '0','1','2','3','4','5','6','7','8'/
      data optprsnt /NOPT*.FALSE./
      data outopts /'coeff', '0','1','2','3','4','5','6','7','8'/
c-----------------------------------------------------------------------
c Initialize keyword routines.
      call keyini

c Figure out which continuum-determination algorithm to use, and for
c algorithm 'poly' also what the fit order must be.
      call options('mode', algopts, optprsnt, NOPT)
      algorithm = ' '
      do i = 1, NALG
        if (optprsnt(i)) call assertl(algorithm.eq.' ',
     *                   'Only one algorithm should be selected')
        if (optprsnt(i)) algorithm = algopts(i)(1:4)
      enddo
      if (algorithm.eq.' ') algorithm = algopts(1)(1:4)

      n=0
      nterms=0
      do i = 1, MAXTERMS
        if (optprsnt(i+NALG)) n=n+1
        if (optprsnt(i+NALG)) nterms = i
      enddo

      if (algorithm(1:4).eq.'poly') then
        if (n.gt.1)
     *    call bug('f', 'Only one polynomial order may be given')
        if (nterms.eq.0) then
          call bug('w', 'Polynomial of order 1 assumed')
          nterms = 2
        endif
        if (nterms.gt.3) call bug('w',
     *      'Think again about fitting a polynomial with order>2')
        algorithm = 'poly' // itoaf(nterms)
        if (nterms.eq.1) algorithm = 'mean'
      else
        if (nterms.ne.0) call bug('w', 'Polynomial order ignored')
      endif

c opts(1): Check the output option.
      do i = 1, NOPTO
        optprsnt(i) = .FALSE.
      enddo
c           Careful: this routine cannot know how many elements of opts
c           to initialize; right now only 1 needed
      opts(1) = 0
      call options('options', outopts, optprsnt, NOPTO)
      if (optprsnt(1)) then
        n=0
        do i = 1, MAXTERMS
          if (optprsnt(i+1)) n=n+1
          if (optprsnt(i+1)) opts(1) = i-1
        enddo
        call assertl(n.eq.1,
     *             'Only one value may be given after options=coeff')
        call assertl(opts(1).lt.nterms,
     *             'Coefficient chosen to write must be < poly order')
        if (n.eq.0) opts(1) = -1
      endif

c opts(2): Check if verbose=true
      call keyl('verbose', verbose, .FALSE.)
      if (verbose) then
        opts(2) = 1
      else
        opts(2) = 0
      endif

c Read names of input, output and continuum dataset.
      call keyf('in',   inp, ' ')
      call keya('out',  out, ' ')
      call keya('cont', con, ' ')
      call assertl(inp.ne.' ', 'You must specify an input file')
      call assertl(out.ne.' ' .or. con.ne.' ',
     *                         'You must specify either out= or cont=')
      if (algorithm(1:4).eq.'subt') call assertl(con.ne.' ',
     *'To subtract an existing continuum set you must give its name')


c Open and get dimension of input dataset: naxis.
      naxis = MAXNAX
      call xyzopen(unitinp, inp, 'old', naxis, axlen)
      call assertl(naxis.gt.0,
     *     'You hit the limit: cannot subtract profile in 0-dim cube')


c Find out which axis is velocity. First read keyword, then call veldir
c which reads the header and sets the value of velaxis if not given in
c keyword.
      call keya('velaxis', type, 'freq')
      velaxis = 'z'
      call fndaxnum(unitinp, type, velaxis, velaxnr)

c Obtain the list of channels to use as continuum. The number of
c continuum channels is stored in contchan(0). For algorithm 'avgs'
c the input must consist of two ranges.
      if (algorithm(1:4).eq.'poly') maxranges = MAXCHAN
      if (algorithm(1:4).eq.'mean') maxranges = MAXCHAN
      if (algorithm(1:4).eq.'avgs') maxranges = 2
      if (algorithm(1:4).ne.'subt') then
        contchan(0) = axlen(velaxnr)
        call getchans(maxranges, contchan)
        if (contchan(0).le.nterms-1) then
          call bug('w','Fit likely to be poor, because number of')
          call bug('w','continuum channels is <= order of polynomial')
        endif
      endif


c Get an input region from the user
      call boxinput('region', inp, boxes, MAXBOXES)
      call boxset(boxes, naxis, axlen, ' ')
      call boxinfo(boxes, naxis, inpblc, inptrc)

c Set up xyzio routines for input dataset
      call xyzsetup(unitinp, velaxis, inpblc,inptrc,
     *               viraxlen,vircubesize)
c Figure out number of profiles to do and their length
      nprofiles = vircubesize(naxis) / vircubesize(1)
      nchan     = viraxlen(1)


c Open output dataset, if required. Then copy header.
      if (out.ne.' ') then
        do i = 1, naxis
          axnum(i)  = i
          oaxlen(i) = inptrc(i) - inpblc(i) + 1
          blc(i)    = 1
          trc(i)    = oaxlen(i)
        enddo
        call xyzopen(unitout, out, 'new', naxis, oaxlen)
        call headcp(unitinp, unitout, naxis, axnum, inpblc, inptrc)
        call xyzsetup(unitout,velaxis, blc,trc, viraxlen,vircubesize)
      else
        unitout = 0
      endif


c Open continuum dataset, if required. Then copy header, but change
c the axes: the velocity axis will be missing from this dataset. This
c is done using the array axnum, which contains the relation between
c the input and output axes. The axis lenghts of the input dataset
c on corresponding continuum set axes are stored in axlen, for use
c by mode subtr.
      if (con.ne.' ') then
        naxis1 = naxis - 1
        if (velaxnr-1.ge.1) then
          do i = 1, velaxnr - 1
            axnum(i)  = i
            oaxlen(i) = inptrc(i) - inpblc(i) + 1
            blc(i)    = 1
            trc(i)    = oaxlen(i)
          enddo
        endif
        if (naxis1.ge.velaxnr) then
          do i = velaxnr, naxis1
            axnum(i)  = i+1
            oaxlen(i) = inptrc(i+1) - inpblc(i+1) + 1
            axlen(i)  = axlen(i+1)
            blc(i)    = 1
            trc(i)    = oaxlen(i)
          enddo
        endif

        if (algorithm(1:4).ne.'subt') then
c Open new continuum set, with naxis1 axes of length oaxlen
          call xyzopen(unitcon, con, 'new', naxis1, oaxlen)
          call headcp(unitinp,unitcon,naxis1,axnum,inpblc,inptrc)
        else
c Open old continuum set and check if it has naxis1 axes of length axlen
c (axlen of input set, oaxlen is a temp var here)
          naxis = MAXNAX
          call xyzopen(unitcon, con, 'old', naxis, oaxlen)
          call assertl(naxis.eq.naxis1,
     *    'Dim of continuum set not one less than dim of input set')
          do i = 1, naxis1
            call assertl(axlen(i).eq.oaxlen(i),
     *      'Axis length of continuum set incompatible with input')
          enddo
        endif
        call xyzsetup(unitcon, ' ', blc, trc, viraxlen, vircubesize)
      else
        unitcon = 0
      endif


c Close keyword routines.
      call keyfin

      end

c***********************************************************************

      subroutine getchans(maxranges, contchan)

      integer   maxranges, contchan(0:*)
c-----------------------------------------------------------------------
c Getchan decodes the contchan keyword. It makes sure the syntax is
c right, using the routine boxint from boxes.for. Also, it takes care
c of some limits. Further, for algorithm 'avgs', the number of ranges
c must be 2.
c-----------------------------------------------------------------------
      integer   ch(2), coords(3), i, k1, k2, len1, n, nch, nchan,
     *          nranges
      character ccinput*80

      data coords /3*0/
c-----------------------------------------------------------------------
c nranges counts number of ranges; nchan is total number of continuum
c channels.
      nranges = 0
      nchan   = 0

c decode current string until keyword is exhausted.
      call keya('contchan', ccinput, ' ')
      call assertl(ccinput.ne.' ','You must specify continuum channels')
      do while (ccinput.ne.' ')
        nranges = nranges + 1
        call assertl(nranges.le.maxranges,
     *               'Too many ranges specified for contchan')
c boxint decodes a string looking like '(1,2)'. In this case 'abspix'
c and coords are dummy arguments.
        k1 = 1
        k2 = len1(ccinput)
        call boxint(ccinput,k1,k2,ch,nch,1,2,'abspix',coords)
        call assertl(nch.eq.1 .or. nch.eq.2,
     *               'Contchan keyword does not contain a range')
c if string was '(1)', set upper end of range too for simplicity below.
        if (nch.eq.1) ch(2) = ch(1)
        nch = ch(2) - ch(1) + 1
        call assertl(ch(1).le.ch(2),
     *       'Start channel # must be lower than end channel #')
        call assertl(nchan+nch.le.contchan(0),
     *       'Too many continuum channels specified')
c put continuum channels in array contchan
c for algorithm 'avgs', second range is flagged by taking negative
c channel number. Is later decoded.
        i = 1
        if (maxranges.eq.2 .and. nranges.eq.2) i = -1
        do n = 1, nch
          contchan(nchan+n) = i*(ch(1) + n-1)
        enddo
        nchan = nchan + nch
        call keya('contchan', ccinput, ' ')
      enddo
      if (maxranges.eq.2 .and. nranges.ne.2)
     *  call bug('f','Two channel ranges are required for mode avgs')
      contchan(0) = nchan

      end

c***********************************************************************

      subroutine work(unitinp, unitout, unitcon, nprofiles, nchan,
     *                algorithm, options, contchan)

      integer   unitinp, unitout, unitcon, nprofiles, nchan
      character algorithm*(*)
      integer   options(*), contchan(0:*)
c-----------------------------------------------------------------------
c Work loops over all profiles, reads each profile, applies the
c continuum-determination algorithm, and writes the results.
c-----------------------------------------------------------------------
      integer   MAXDIM
      parameter (MAXDIM = 8192)

      logical   cmask, mask(MAXDIM), ok
      integer   nterms, profilenr
      real      continuum, data(MAXDIM), linedata(MAXDIM)
      character string*80
c-----------------------------------------------------------------------
      if (algorithm(1:4).eq.'poly') then
         call atoif(algorithm(5:), nterms, ok)
      endif

      do profilenr = 1, nprofiles
        if (options(2).ne.0 .and. mod(profilenr,2500).eq.0) then
          write(string,'(''Done with '',i6,'' profiles ['',i2,''%]'')')
     *          profilenr, 100*profilenr/nprofiles
          call output(string)
        endif
        call xyzprfrd(unitinp, profilenr, data, mask, nchan)
        if (algorithm(1:4).eq.'poly') then
          call polyfit(contchan, nchan, nterms, options,
     *                 data, linedata, mask, continuum, cmask)
        else if (algorithm(1:4).eq.'mean') then
          call submean(contchan, nchan,
     *                 data, linedata, mask, continuum, cmask)
        else if (algorithm(1:4).eq.'avgs') then
          call subavgs(contchan, nchan,
     *                 data, linedata, mask, continuum, cmask)
        else if (algorithm(1:4).eq.'subt') then
          call subtrac(unitcon, profilenr, nchan,
     *                 data, linedata, mask, continuum, cmask)
        endif

c       if( mod(profilenr,1000).eq.0 ) print*,profilenr,continuum
        if (unitout.ne.0)
     *    call xyzprfwr(unitout, profilenr, linedata,  mask, nchan)
        if (unitcon.ne.0 .and. algorithm(1:4).ne.'subt')
     *    call xyzpixwr(unitcon, profilenr, continuum, cmask)
      enddo

      end

c***********************************************************************

      subroutine polyfit(contchan, nchan, nterms, outopt,
     *                    data, linedata, mask, continuum, cmask)

      integer   contchan(0:*), nchan, nterms, outopt(*)
      real      data(*), linedata(*), continuum
      logical   mask(*), cmask
c-----------------------------------------------------------------------
c This routine uses linpack to fit a polynomial through the data in
c the continuumchannels. For each profile a matrix-equation is
c constructed. Then linpack is used. Finally the fitvalue is subtracted
c from all the channel data and the fitvalue in the center is used as
c continuum.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer   MAXTERMS, NELEMENT
      parameter (MAXTERMS = 9, NELEMENT = MAXTERMS*MAXTERMS)

      integer   i, j, k, info, pivot(MAXTERMS)
      real      cont, xtothen
      double precision matrix (MAXTERMS, MAXTERMS), rhs(MAXTERMS),
     *          weight (2*MAXTERMS, MAXCHAN)
c-----------------------------------------------------------------------
c First check if not all continuum channel values are "undefined"
      info = 0
      k = 1
      do while (info.eq.0 .and. k.le.contchan(0))
        if (mask(contchan(k))) info = 1
        k = k + 1
      enddo

      if (info.ne.0) then
c create array of plane**0 ... plane**(2*nterms-1) with which to
c work to create matrix.
        do k = 1, contchan(0)
          if (mask(contchan(k))) then
            weight(1,k) = 1
            do i = 2, 2*nterms-1
              weight(i,k) = weight(i-1,k) * contchan(k)
            enddo
          endif
        enddo
        do i = 1, nterms
          do j = 1, nterms
            matrix(i,j) = 0.0
            do k = 1, contchan(0)
              if (mask(contchan(k)))
     *            matrix(i,j) = matrix(i,j) + weight(i+j-1,k)
            enddo
          enddo
        enddo
c Get right-hand side of matrix equation
        do i = 1, nterms
          rhs(i) = 0.0
          do k = 1, contchan(0)
            if (mask(contchan(k)))
     *         rhs(i) = rhs(i) + dble(data(contchan(k))) * weight(i,k)
          enddo
        enddo

c Call linpack routines to transpose and fit.
        call dgefa(matrix, MAXTERMS, nterms, pivot, info)
        if (info.ne.0) call bug('f','Highly bogus division by zero')
        call dgesl(matrix, MAXTERMS, nterms, pivot, rhs, 0)

c Subtract the continuum from the spectrum.
        do k = 1, nchan
          cont = 0.0
          xtothen = 1
          do i = 1, nterms
            cont = cont + rhs(i) * xtothen
            xtothen = xtothen * k
          enddo
          linedata(k) = data(k) - cont
          if (k.eq.nchan/2) then
           if (outopt(1).eq.-1) continuum = cont
           if (outopt(1).ge.0) continuum = rhs(outopt(1)+1)
          endif
        enddo
        cmask = .TRUE.

      else
        continuum = 0.0
        cmask     = .FALSE.
        do k = 1, nchan
          linedata(k) = data(k)
          mask(k)     = .FALSE.
        enddo
      endif

      end

c***********************************************************************

      subroutine submean(contchan, nchan,
     *                    data, linedata, mask, continuum, cmask)

      integer          contchan(0:*), nchan
      real             data(*), linedata(*), continuum
      logical          mask(*), cmask
c-----------------------------------------------------------------------
c Submean is the simplest algorithm: the mean value of all continuum
c channel data is found and subtracted from the input data.
c-----------------------------------------------------------------------
      integer          k, n
c-----------------------------------------------------------------------
      n         = 0
      continuum = 0.0
      do k = 1, contchan(0)
        if (mask(contchan(k))) then
          continuum = continuum + data(contchan(k))
          n = n + 1
        endif
      enddo

      if (n.ne.0) then
        continuum = continuum / n
        cmask     = .TRUE.
        do k = 1, nchan
          linedata(k) = data(k) - continuum
        enddo
      else
        continuum = 0.0
        cmask     = .FALSE.
        do k = 1, nchan
          linedata(k) = data(k)
          mask(k)     = .FALSE.
        enddo
      endif

      end

c***********************************************************************

      subroutine subavgs(contchan, nchan,
     *                    data, linedata, mask, continuum, cmask)

      integer   contchan(0:*), nchan
      real      data(*), linedata(*), continuum
      logical   mask(*), cmask
c-----------------------------------------------------------------------
c Subavgs averages the channels in range 1 and range 2 (identified by
c whether contchan is positive or negative) and puts a line through the
c middle of the two ranges and the two averages. This 'fit' is then
c subtracted from the input data.
c-----------------------------------------------------------------------
      integer   k, mid1, mid2, n1, n2
      real      cont1, cont2, slope
c-----------------------------------------------------------------------
c Initialize.
      cont1 = 0.0
      cont2 = 0.0
      mid1  = 0
      mid2  = 0
      n1    = 0
      n2    = 0
c Find the two averages.
      do k = 1, contchan(0)
        if (mask(contchan(k))) then
          if (contchan(k).gt.0) then
            cont1 = cont1 + data(contchan(k))
            mid1  = mid1 + contchan(k)
            n1    = n1 + 1
          else
            cont2 = cont2 + data(-contchan(k))
            mid2  = mid2 - contchan(k)
            n2    = n2 + 1
          endif
        endif
      enddo

      if (n1.ne.0 .and. n2.ne.0) then
        cont1     = cont1 / n1
        mid1      = mid1  / n1
        cont2     = cont2 / n2
        mid2      = mid2  / n2
        slope     = (cont2 - cont1) / (mid2 - mid1)
        continuum = (cont1 + cont2) / 2
        cmask     = .TRUE.
        do k = 1, nchan
          linedata(k) = data(k) - (slope*(k-mid1)+cont1)
        enddo
      else if (n1.ne.0 .and. n2.eq.0) then
        continuum = cont1 / n1
        cmask     = .TRUE.
        do k = 1, nchan
          linedata(k) = data(k) - continuum
        enddo
      else if (n1.eq.0 .and. n2.ne.0) then
        continuum = cont2 / n2
        cmask     = .TRUE.
        do k = 1, nchan
          linedata(k) = data(k) - continuum
        enddo
      else if (n1.eq.0 .and. n2.eq.0) then
        continuum = 0.0
        cmask     = .FALSE.
        do k = 1, nchan
          linedata(k) = data(k)
          mask(k)     = .FALSE.
        enddo
      endif

      end

c***********************************************************************

      subroutine subtrac(unitcon, profilenr, nchan,
     *                    data, linedata, mask, continuum, cmask)

      integer   unitcon, profilenr, nchan
      real      data(*), linedata(*), continuum
      logical   mask(*), cmask
c-----------------------------------------------------------------------
c Subtrac find the continuum from the input continuum dataset and
c subtracts it.
c-----------------------------------------------------------------------
      integer   k
c-----------------------------------------------------------------------
      call xyzpixrd(unitcon, profilenr, continuum, cmask)
      if (.not.cmask) continuum = 0
      do k = 1, nchan
        linedata(k) = data(k) - continuum
        mask(k)     = mask(k) .and. cmask
      enddo

      end

c***********************************************************************
      subroutine finish(unitinp, unitout, unitcon, version,
     *                   algorithm, contchan)

      integer   unitinp, unitout, unitcon
      character version*(*), algorithm*(*)
      integer   contchan(*)
c-----------------------------------------------------------------------
c Finish up
c-----------------------------------------------------------------------
      character line*80
c-----------------------------------------------------------------------
      call xyzclose(unitinp)

      if (unitout.ne.0) then
        call hisopen(unitout, 'append')
        line = 'CONTSUB: ' // version
        call hiswrite(unitout, line )
        call hisinput(unitout, 'contsub')
        call hisclose(unitout)
        call xyzclose(unitout)
      endif

      if (unitcon.ne.0) then
        call hisopen(unitcon, 'append')
        line = 'CONTSUB: ' // version
        call hiswrite(unitcon, line )
        call hisinput(unitcon, 'contsub')
        call hisclose(unitcon)
        call xyzclose(unitcon)
      endif

      end
