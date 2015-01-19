      program imstat

c= IMSTAT - calculate and plot map statistics
c& bpw
c: map analysis
c+
c       IMSTAT calculates statistics for images.  These are the sum,
c       mean, rms, maximum and minimum value of a region.  Statistics
c       can be found for profiles or planes, specified using the axes
c       keyword.
c
c       The data can be converted to Kelvin, by using 'options=tb' and
c       the beam keyword.
c
c       Output can be written to the terminal, a log file, or a plot.
c       The options keyword gives control over the plot.
c
c       The plot header can be suppressed by using options=noheader.
c       An alternative title can be put on the plot by options=title.
c       A useful combination is 'options=noh,ti,title', to get only the
c       string 'title', instead of the full header.
c
c< in
c
c< region
c
c@ axes
c       One axis (for profiles) or two axes (for planes) for which
c       statistics are to be calculated.  E.g. 'axes=spectral' computes
c       statistics of each spectrum in the selected region - whether
c       expressed as frequency, velocity, etc.  
c       'axes=longitude,latitude' would compute statistics for each
c       celestial image plane in the dataset (regardless of
c       orientation).
c
c       axes is case insensitive, and may be specified explicitly by
c       coordinate type (ctype), as listed by PRTHD.  In this it is not
c       necessary to specify the algorithm code, e.g. 'RA' matches both
c       'RA---NCP' and 'RA---SIN', while 'VOPT' matches 'VOPT-F2W'.
c
c       Axes may also be specified via their order in the image as
c       'x', 'y', 'z', 'a', 'b', 'c', and 'd' for axes 1 to 7.
c
c       The following generic values are also understood: 'longitude'
c       ('lng' or 'long'), 'latitude' (or 'lat'), and 'spectral' ('spc'
c       or 'spec').
c
c       The default is 'x,y' if the input is multi-dimensional,
c       otherwise 'x'.
c
c@ plot
c       Selects the statistic to be plotted.  Minimal matching is
c       applied.  The default is 'rms'.
c
c         'sum'         Plot the sum
c         'mean'        Plot the mean
c         'rms'         Plot the rms
c         'maximum'     Plot the maximum
c         'minimum'     Plot the minimum
c
c@ cutoff
c       Data values below the cutoff are not used for the calculation
c       of statistics.  Give one real value.  This may be followed by
c       the string ',abs' to get a cutoff in the absolute value of the
c       datavalues, or ',lower' to exclude all values above the cutoff.
c       Default is no cutoff.
c
c< device
c
c@ options
c       Options controlling the characteristics of the plot (minimal
c       matching is done):
c
c        'tb'          Convert the units to brightness temperature,
c                      using the input for the beam keyword
c        'hanning,#'   Hanning smooth the data first over # pixels (must
c                      be an odd number)
c        'boxcar,#'    Boxcar smooth the data first over # pixels
c        'deriv,#'     Take the derivative after smoothing.  If #=1 a
c                      one-sided derivative is taken, for #=2 its
c                      two-sided.  Useful for Zeeman work.
c
c        'noheader'    Do not write the header information, just the
c                      numbers, producing an ASCII file for a plotting
c                      program
c        'nolist'      Do not write the statistics to the screen/logfile
c        'eformat'     Always use format 'e' instead of 'g' to write
c                      results
c        'guaranteespaces' Make sure there is always a space between
c                      columns (at the cost of precision)
c
c        'xmin,#'      Give lower x-value on axis
c        'xmax,#'      Give upper x-value on axis
c        'ymin,#'      Give lower y-value on axis
c        'ymax,#'      Give upper y-value on axis
c                      (for these four options the default is
c                       autoscaling)
c        'title,#1,#2,#3' Put the string #1 at x-position #2 and
c                      y-position #3, with positions measured in units
c                      of the coordinates on the axes.  If 'title' is
c                      the last option, the title is put in the upper
c                      left hand corner.
c        'style,#'     This selects the plot style.
c                      #=connect means connect the datapoints
c                      #=step means make one-bin wide connected
c                      horizontal line segments
c                      #=histo means bins are drawn as a horizontal line
c                      surrounded by two vertical lines
c
c@ beam
c       If options=tb is used, imstat calculates the sum divided by the
c       sum of the beam to get the flux in the selected area, if the
c       units of the input data are 'per beam'.  This is then converted
c       to Kelvin by dividing by 2k/lambda^2 * omega, where omega is
c       found from the beam keyword.
c
c       If the name of a dataset is given for 'beam', imstat assumes it
c       contains a beampattern and sums the data in a region of the same
c       size as the input region.  Else, it assumes that 'beam' gives
c       the major and minor axes of the beam in arcsec and it calculates
c       the sum for a Gaussian beam of that size.
c
c       If 'beam' is omitted, but 'options=tb' was selected, the beam is
c       found from the header (items bmaj and bmin).  If neither is
c       present, no conversion is done.
c
c@ log
c       If specified, output is written to the file given by log=
c       instead of to the terminal.
c
c$Id: imstat.for,v 1.9 2015/01/19 02:17:45 wie017 Exp $
c--
c  IMSPEC - plots spectra from image data
c& bpw
c: map analysis
c+
c       IMSPEC plots spectra.  The flux, primary-beam-corrected-flux,
c       mean or sum of an area can be plotted.  Data can be averaged or
c       summed in ra-dec, ra-vel or dec-vel (etc) planes, to obtain
c       profiles along the vel, dec or ra axes, respectively.  See the
c       description of the axes keyword.
c
c       To get fluxes the sum of the beam in an area of the same size as
c       the input region is calculated, using the beam keyword.
c       The data can be converted to Kelvin, by using 'options=tb' and
c       the beam keyword.
c
c       Output can be written to the terminal, a log file, or a plot.
c       The options keyword gives control over the plot.
c       To write the spectrum to an ASCII file use options=list,noheader
c       and log=logfile.
c
c       The plotheader can be suppressed by using options=noheader.
c       An alternative title can be put on the plot by options=title.
c       A useful combination is 'options=noh,ti,title', to get only the
c       string 'title', instead of the full header.
c
c< in
c
c< region
c       imspec only recognizes rectangular boxes.  It will use the mask
c       associated with the input image.
c
c       The region specifications in the following examples are relative
c       to the reference pixel and assume an (RA,Dec,spectral) cube:
c
c       - Plot a spectrum with channel in the range [10:40] averaged
c         over RA and Dec in the range RA:[-31,32] and Dec[-31,32]:
c           region=relpix,box(-31,-31,32,32)(10,40)
c           axes=ra,dec
c
c       - Plot a profile in RA in the range RA:[-31,32], at Dec=0
c         for spectral channel 10:
c           region=relpix,box(-31,0,32,0)(10)
c           axes=dec,spectral
c
c       - Plot a set of profiles in RA in the range RA:[-31,32], for
c         each Dec in the range Dec:[-31,32], for spectral channel 10: 
c           region=relpix,box(-31,-31,32,32)(10)
c           axes=spectral
c
c       - Plot a profile in Dec in the range dec:[-31,32] with RA
c         averaged from RA:[-10,10], for spectral channel 10:
c           region=relpix,box(-10,-31,10,32)(10)
c           axes=ra,spectral
c
c@ axes
c       One or two axes along which data are averaged to obtain a data
c       point on the profile.  Combined with the region keyword, it can
c       be used to get a profile as function of any coordinate.  E.g.
c       'axes=spectral' averages each spectrum in the selected region -
c       whether expressed as frequency, velocity, etc.
c       'axes=longitude,latitude' averages each celestial image plane
c       in the dataset (regardless of orientation).
c
c       axes is case insensitive, and may be specified explicitly by
c       coordinate type (ctype), as listed by PRTHD.  In this it is not
c       necessary to specify the algorithm code, e.g. 'RA' matches both
c       'RA---NCP' and 'RA---SIN', while 'VOPT' matches 'VOPT-F2W'.
c
c       Axes may also be specified via their order in the image as
c       'x', 'y', 'z', 'a', 'b', 'c', and 'd' for axes 1 to 7.
c
c       The following generic values are also understood: 'longitude'
c       ('lng' or 'long'), 'latitude' (or 'lat'), and 'spectral' ('spc'
c       or 'spec').
c
c       The default is 'x,y' if the input is multi-dimensional,
c       otherwise 'x'.
c
c@ plot
c       This selects what will be plotted as function of e.g. velocity.
c       To convert data to fluxes the input of the beam keyword is used.
c       Minimal matching is applied.  The default is 'flux'.
c
c        'mean'        Plot the mean
c        'sum'         Plot the sum
c        'flux'        Plot the flux
c        'pbcflux'     Plot the primary-beam-corrected flux
c                      (not yet implemented)
c
c@ cutoff
c       Data values below the cutoff are not used for the calculation
c       of statistics.  Give one real value.  This may be followed by
c       the string ',abs' to get a cutoff in the absolute value of the
c       datavalues, or ',lower' to exclude all values above the cutoff.
c       Default is no cutoff.
c
c< device
c
c@ options
c       The options control the characteristics of the plot.
c       Possible options are (minimal matching is done):
c
c        'tb'          Convert the units of mean or sum to brightness
c                      temperature, using the input for the beam keyword
c        'hanning,#'   Hanning smooth the data first over # pixels (must
c                      be an odd number)
c        'boxcar,#'    Boxcar smooth the data first over # pixels
c        'deriv,#'     Take the derivative after smoothing.  If #=1 a
c                      one-sided derivative is taken, for #=2 it's
c                      two-sided.  Useful for Zeeman work.
c
c        'noheader'    Do not write the header information, just the
c                      numbers, producing an ASCII file for a plotting
c                      program
c        'list'        Write the spectrum to the screen/logfile
c        'eformat'     Always use format 'e' instead of 'g' to write
c                      results
c        'guaranteespaces' Make sure there is always a space between
c                      columns (at the cost of precision)
c
c        'xmin,#'      Give lower x-value on axis
c        'xmax,#'      Give upper x-value on axis
c        'ymin,#'      Give lower y-value on axis
c        'ymax,#'      Give upper y-value on axis
c                      (for these four options the default is
c                       autoscaling)
c        'title,#1,#2,#3' Put the string #1 at x-position #2 and
c                      y-position #3, with positions measured in units
c                      of the coordinates on the axes.  If 'title' is
c                      the last option, the title is put in the upper
c                      left hand corner.
c        'style,#'     This selects the plot style.
c                      #=connect means connect the datapoints
c                      #=step means make one-bin wide connected
c                      horizontal line segments
c                      #=histo means bins are drawn as a horizontal line
c                      surrounded by two vertical lines
c
c@ beam
c       If plot=flux is used, imspec calculates the sum divided by the
c       sum of the beam to get the flux in the selected area, if the
c       units of the input data are 'per beam'.
c       If the name of a dataset is given, it assumes this is a beam
c       pattern and sums the data in a region of the same size as the
c       input region.
c       Else, it assumes that 'beam' gives the major and minor axes of
c       the beam in arcsec and it calculates the sum of a gaussian beam
c       of that size.
c       If 'beam' is omitted, but 'flux' was selected, the beam is found
c       from the header (items bmaj and bmin).  If neither is present,
c       the sum is assumed to be 1.
c
c@ log
c       If specified, output is written to the file given by log=
c       instead of to the terminal.
c--
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c
c-----------------------------------------------------------------------
      include 'imstat.h'

      integer   boxes(MAXBOXES), corners(4), counts(0:MAXNAX+2), dim,
     *          lIn, naxis
      real      cut(2)
      double precision beaminfo(4)
      character axlabel(MAXNAX)*32, device*80, version*72

      external  versan
      character versan*72
c-----------------------------------------------------------------------
      version = versan('imstat',
     *                 '$Revision: 1.9 $',
     *                 '$Date: 2015/01/19 02:17:45 $')

      call inputs(lIn,naxis,dim,corners,boxes,cut,counts,beaminfo,
     *  axlabel,device,MAXBOXES)
      cin = lIn
      call stats(lIn,naxis,dim,corners,boxes,cut,counts,beaminfo,
     *  axlabel,device)
      call xyzclose(lIn)
      call coFin(lIn)
      call logclose

      end

c***********************************************************************

      subroutine inputs(lIn,naxis,dim,corners,boxes,cut,counts,beaminfo,
     *  axlabel,device,MAXBOXES)

      integer   lIn, naxis, dim, corners(*), boxes(*)
      real      cut(*)
      integer   counts(0:*)
      double precision beaminfo(*)
      character axlabel(*)*(*), device*(*)
      integer   MAXBOXES
c-----------------------------------------------------------------------
c  Get all the inputs.
c  First get the input file, and open it
c  Then the region in this file to handle
c  optinp decodes the keywords plot and options
c  cutinp gets the possible cutoff value
c  axinp decodes the keyword axes and returns the subcube specification
c        of the averaging-axes and the nice labels along the axes
c  then initialize the input dataset.  This must be done here because
c  beamsum opens and reads another dataset, and before reading a dataset
c          all xyzsetups must have been done
c          beamsum decodes the beam keyword and returns sumap and Jy->K
c          factor
c  outdev gets the plotdevice, and opens the logfile
c
c  After all inputs are read, labset makes the final nice labels to put
c  along the plot header puts out some information about the dataset and
c  units.
c-----------------------------------------------------------------------
      include            'maxdim.h'
      include            'maxnax.h'

      integer            i, dimen

      character*1024     file
      integer            axlen(MAXNAX)
      integer            blc(MAXNAX), trc(MAXNAX)
      integer            viraxlen(MAXNAX)
      ptrdiff            vircsz(MAXNAX)
      character*(MAXNAX) subcube
c-----------------------------------------------------------------------
      call keyini

      call keyf('in', file, ' ')
      naxis = MAXNAX
      call xyzopen(lIn, file, 'old', naxis, axlen)

      call boxinput('region', file, boxes, MAXBOXES)
      call boxset(boxes, naxis, axlen, ' ')
      call boxinfo(boxes, naxis, blc, trc)
      corners(1) = blc(1)
      corners(2) = trc(1)
      corners(3) = blc(2)
      corners(4) = trc(2)

      call optinp

      call cutinp(cut)

      call axinp(lIn, naxis, dim, subcube, axlabel)

      dimen = abs(dim)
      call xyzsetup(lIn, subcube(:dimen), blc,trc, viraxlen,vircsz)
      if (dim.gt.0) counts(0)       = vircsz(dimen)
      if (dim.eq.-2) counts(0)       = viraxlen(1)
      if (dim.eq.-2) counts(naxis+2) = viraxlen(2)

      do i = dimen, naxis
        counts(i-dimen+1) = vircsz(i) / vircsz(dimen)
      enddo
      counts(naxis-dimen+2) = counts(naxis-dimen+1)

      call beamsum(lIn, blc, trc, beaminfo)

      call outdev(device)

      call keyfin

      call labset(axlabel, lIn, file, blc, trc, naxis)

      call header(lIn, file, dimen, beaminfo)

      end

c***********************************************************************

      subroutine cutinp(cut)

      real          cut(*)
c-----------------------------------------------------------------------
c  Get the cutoff value to apply.
c  cut is an array of 2 elements, the first being the cutoff, the second
c  a flag indicating if a cutoff was requested and whether or not this
c  was an absolute value cutoff.
c-----------------------------------------------------------------------
      logical    keyprsnt
      character  string*10
c-----------------------------------------------------------------------
      if (keyprsnt('cutoff')) then
        call keyr('cutoff', cut(1),      0.0)
        call keya('cutoff', string, 'noabs')
        if (string.ne.'abs') cut(2) = 1.0
        if (string.eq.'abs') cut(2) = 2.0
        if (string.eq.'lower') cut(2) = 3.0
      else
        cut(2) = 0.0
      endif

      end

c***********************************************************************

      subroutine optinp

c-----------------------------------------------------------------------
c  Decode the plot and options keywords.
c  The global variables plotvar, plotrnge and plotpar are filled here.
c  plotvar contains flags to indicate if (and sometimes which) options
c  were selected.
c  plotrnge gives the range of x and/or y values to plot, and also a
c  flag to indicate if a range was selected
c  Of plotpar just one element is filled here, the one containing an
c  optional user-given title.
c-----------------------------------------------------------------------
      include       'imstat.h'

      character*20  option
      logical       match
      integer       i, count
      real          v
      integer       matchnr
      integer       len1
      character*80  line
c-----------------------------------------------------------------------
c     Set default values for options.
c     defplt is 'rms' for IMSTAT and 'flux' for IMSPEC
      plotvar(SEL) = matchnr(defplt, plotopts)

c     Default is to write a header.
      plotvar(HEAD) =  1

c     Default is to write out the spectrum for IMSTAT, not for IMSPEC.
      if (NAME.eq.'IMSTAT') plotvar(LIST) =  1
      if (NAME.eq.'IMSPEC') plotvar(LIST) =  0

c     Default is 'g' format.
      plotvar(EFMT) = 0

c     Default is to give as much precision as possible.
      plotvar(GSPAC) = 0

c     Default plot style is to connect the points.
      plotvar(STYLE) = matchnr('connect', styles)

c     Default is not to convert the data units.
      plotvar(DUNIT) = ORIG

c     Default is not to smooth.
      plotvar(DOSMOOTH) =  0

c     Default is not to take the derivative.
      plotvar(DERIV) =  0

c     Default is no extra title.
      plotpar(TITLE) = ' '

c     Default is no range selection.
      plotrnge(FLXL) = 0.0
      plotrnge(FLXU) = 0.0
      plotrnge(FLYL) = 0.0
      plotrnge(FLYU) = 0.0

c     Loop over the plot keyword, to test if input is only one value.
c     If so, plotvar(SEL) is set to the selected plot index
      call keya('plot', option, ' ')
      count = 0
      do while (option.ne.' ')
        call lcase(option)
        if (match(option, plotopts, i)) then
          line = 'Only one of '//plotopts//' can be given at one time'
          call assertl(count.eq.0, line)
          plotvar(SEL) = i
          count = count + 1
        else
          line = 'Illegal option ' // option(:len1(option))
          call bug('f', line)
        endif
        call keya('plot', option, ' ')
      enddo

c     Spectrum units are Jansky if plot=flux was selected.
      if (plotvar(SEL).eq.matchnr('flux',   plotopts) .or.
     *    plotvar(SEL).eq.matchnr('pbcflux',plotopts))
     *    plotvar(DUNIT) = JANSKY

c     Decode the options keyword.  First match the option against the
c     full list.  If present decode the option, else give a warning.
c     For options with arguments, these are read and tested.
      call keya('options', option, ' ')
      do while (option.ne.' ')
        call lcase(option)

        if (match(option, commonop, i)) then
          if (i.eq.0) then
          else if (i.eq.matchnr('noheader',commonop)) then
            plotvar(HEAD) = 0
          else if (i.eq.matchnr('nolist',commonop)) then
            plotvar(LIST) = 0
          else if (i.eq.matchnr('list',commonop)) then
            plotvar(LIST) = 1
          else if (i.eq.matchnr('eformat',commonop)) then
            plotvar(EFMT) = 1
          else if (i.eq.matchnr('guaranteespaces',commonop)) then
            plotvar(GSPAC) = 1
          else if (i.eq.matchnr('style',commonop)) then
            call keya('options', option, ' ')
            call assertl(match(option,styles,i), 'Illegal style')
            plotvar(STYLE) = i
          else if (i.eq.matchnr('tb',commonop)) then
            if (plotvar(SEL).eq.matchnr('flux',   plotopts) .or.
     *          plotvar(SEL).eq.matchnr('pbcflux',plotopts))
     *        call bug('f',
     *          'Option TB cannot be combined with flux or pbcflux')
            plotvar(DUNIT) = KELVIN
          else if (i.eq.matchnr('hanning',commonop)) then
            call keyi('options', i, 3)
            call assertl(i.lt.15, 'Hanning width too large')
            call assertl((i/2)*2.ne.i, 'Width must be odd number')
            plotvar(DOSMOOTH) = HANNING
            plotvar(SMOWID)   = i
          else if (i.eq.matchnr('boxcar',commonop)) then
            call keyi('options', i, 1)
            call assertl(i.lt.14, 'Boxcar width too large')
            plotvar(DOSMOOTH) = BOXCAR
            plotvar(SMOWID)   = i
          else if (i.eq.matchnr('deriv',commonop)) then
            call keyi('options', i, 0)
            call assertl(i.eq.1 .or. i.eq.2,
     *                   'Argument to deriv can only be 1 or 2')
            plotvar(DERIV) = i
          else if (i.eq.matchnr('title',commonop)) then
            call keya('options', plotpar(TITLE),   ' ')
            call keyr('options', plotrnge(XTITLE), MAGICVAL)
            call keyr('options', plotrnge(YTITLE), MAGICVAL)
          else
            call keyr('options', v, 0.0)
            if (  i.eq.matchnr('xmin',commonop)) then
              plotrnge(FLXL) = 1.0
              plotrnge(XLOW) = v
            else if (i.eq.matchnr('xmax',commonop)) then
              plotrnge(FLXU) = 1.0
              plotrnge(XUPP) = v
            else if (i.eq.matchnr('ymin',commonop)) then
              plotrnge(FLYL) = 1.0
              plotrnge(YLOW) = v
            else if (i.eq.matchnr('ymax',commonop)) then
              plotrnge(FLYU) = 1.0
              plotrnge(YUPP) = v
            endif
          endif

        else
          line = 'Illegal or ambiguous option ' //
     *           option(:len1(option)) // ' ignored'
          call bug('w', line)

        endif
        call keya('options', option, ' ')
      enddo

      end

c***********************************************************************

      subroutine axinp(lIn, naxis, dim, subcube, axlabel)

      integer   lIn, naxis, dim
      character subcube*(*), axlabel(*)*(*)
c-----------------------------------------------------------------------
c  Decode the axes keyword.
c-----------------------------------------------------------------------
      include 'imstat.h'

      integer    NAXTYP
      parameter (NAXTYP = 12)

      integer   axind(MAXNAX), axlen(MAXNAX), axnum(MAXNAX), i, indek,
     *          j, len1, n
      character axC*7, axis*32, axtyp(NAXTYP)*4, axlab(NAXTYP)*32

      external  itoaf, keyprsnt, match
      logical   keyprsnt, match
      character itoaf*1

      data axC   /'xyzabcd' /
      data axtyp /'RA',   'DEC',  'GLON', 'GLAT', 'ELON', 'ELAT',
     *            'FREQ', 'WAVE', 'VRAD', 'VOPT', 'ZOPT', 'VELO'/
      data axlab /'Right ascension',          'Declination',
     *            'Galactic longitude (deg)', 'Galactic latitude (deg)',
     *            'Ecliptic longitude (deg)', 'Ecliptic latitude (deg)',
     *            'Frequency (GHz)',          'Wavelength (m)',
     *            'Radio velocity (km/s)',    'Optical velocity (km/s)',
     *            'Redshift',            'Relativistic velocity (km/s)'/
c-----------------------------------------------------------------------
      call coInit(lIn)

c     Decode the axes keyword.
      if (.not.keyprsnt('axes')) then
c       Use the first two axes whatever they are.
        dim = min(2, naxis)
        axnum(1) = 1
        axnum(2) = 2

      else
        dim = 0
c       Locate the selected axes.
        call keya('axes', axis, ' ')
        do while (axis.ne.' ')
          if (axis.eq.' ') goto 10

          dim = dim + 1
          call assertl(dim.le.2, 'A maximum of two axes may be given')

          if (len1(axis).eq.1) then
            axnum(dim) = index(axC,axis(:1))
          else
            call coFindAx(lIn, axis, axnum(dim))

            if (axnum(dim).eq.0) then
c             Try harder.
              call ucase(axis)
              if (axis.eq.'LNG' .or. axis.eq.'LONG') then
                axis = 'longitude'
                call coFindAx(lIn, axis, axnum(dim))
              else if (axis.eq.'LAT') then
                axis = 'latitude'
                call coFindAx(lIn, axis, axnum(dim))
              else if (axis.eq.'SPC' .or. axis.eq.'SPEC') then
                axis = 'spectral'
                call coFindAx(lIn, axis, axnum(dim))
              endif
            endif
          endif

          call assertl(axnum(dim).ne.0,
     *      'No '//axis(:len1(axis))//' axis found')

          call keya('axes', axis, ' ')
        enddo
      endif

 10   call assertl(dim.ne.0, 'No valid axes specified')

c     Sort the axes found to ensure most efficient reading of image.
      call hsorti(dim, axnum, axind)

c     Set subcube variable to the selected axes list for XYZSETUP.
      subcube = ' '
      do n = 1, dim
        subcube(n:n) = axC(axnum(axind(n)):axnum(axind(n)))
      enddo

c     Read naxis and ctype for each axis.
      do n = 1, dim
c       Reorder axes to match the list given by the axes keyword, i.e.
c       the axes over which to average.  The rest will be the other
c       axes, with the lowest numbered first.
        i = axind(n)

        call rdhdi(lIn, 'naxis'//itoaf(axnum(i)), axlen(n), 0)
        call rdhda(lIn, 'ctype'//itoaf(axnum(i)), ctype(n), 'Unknown')
      enddo

      if (dim.lt.naxis) then
        n = dim
        do i = 1, naxis
          if (index(subcube, axC(i:i)).eq.0) then
            n = n + 1
            call rdhda(lIn, 'ctype'//itoaf(i), ctype(n), ' ')
            cindex(n-dim) = i
           endif
        enddo
      endif

c     Create the plot's axis labels, the first being the first non-
c     averaged image axis (no label is needed for those averaged).
      do i = 1, naxis-dim
c       Start by setting the label to the root of the corresponding
c       ctype variable.
        j = dim + i
        axlabel(i) = ctype(j)(:indek(ctype(j),'-')-1)

c       Try to convert to a standard label.
        do j = 1, NAXTYP
          if (axlabel(i).eq.axtyp(j)) then
            axlabel(i) = axlab(j)
            goto 20
          endif
        enddo

 20     continue
      enddo

c     Is it necessary to read a plane profile by profile?
      if (dim.eq.2  .and.  axlen(1)*axlen(2).gt.MAXBUF/2) then
c       Yes, flag this by negating dim.
        dim = -dim
      endif

      end

c***********************************************************************

      subroutine beamsum(lIn, blc, trc, beaminfo)

      integer   lIn, blc(*), trc(*)
      double precision beaminfo(*)
c-----------------------------------------------------------------------
      include          'imstat.h'

      logical          keyprsnt, beamprs, toKelvin
c-----------------------------------------------------------------------
      if (plotvar(DUNIT).eq.ORIG) then
        beamprs = keyprsnt('beam')
        if (NAME.eq.'IMSTAT' .and. beamprs) call bug('w',
     *    'The ignored beam keyword only makes sense with option tb')
        return
      endif

      toKelvin = plotvar(DUNIT).eq.KELVIN
      call sumbeam(lIn, blc, trc, beaminfo, toKelvin)
      if (plotvar(DUNIT).eq.KELVIN .and. .not.toKelvin)
     *  plotvar(DUNIT) = ORIG

      end

c***********************************************************************

      subroutine sumbeam(lIn, blc, trc, beaminfo, toKelvin)

      integer   lIn, blc(*), trc(*)
      double precision beaminfo(*)
      logical   toKelvin
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'

      integer    SUMBM,   KPERJY,   BEAMX,   BEAMY
      parameter (SUMBM=1, KPERJY=2, BEAMX=3, BEAMY=4)

      logical   beamprs, keyprsnt, mask, ok
      integer   axlen(MAXNAX), axlenx, axleny, bmblc(MAXNAX), bmctr(2),
     *          bmsiz(2), bmtrc(MAXNAX), i, iostat, j, latAx, lngAx,
     *          naxis, tbm, viraxlen(MAXNAX)
      ptrdiff   vircsz(MAXNAX),pix
      real      value
      double precision frln2, grid(2), ratio(2), restfreq
      character axC*7, plane*2, string*80, units*10

      external  itoaf
      character itoaf*1

      data axC /'xyzabcd'/
c-----------------------------------------------------------------------
c     First do some tests, i.e. if the dataunits are 'per beam'.  If
c     not, the data cannot be converted in this way (except if they are
c     'per pixel', in which case the conversion factor is simply 1.
c     If Kelvin were requested for the plot, the data units must also be
c     'per beam', and also the wavelength must be known.

      ok = .true.
      beaminfo(SUMBM) = 1d0
      call rdhda(lIn, 'bunit', units, 'Unknown')
      call ucase(units)
      if (index(units,'BEAM').eq.0) then
        ok = .false.
        if (index(units,'PIXEL').ne.0) ok = .true.
        if (.not.ok) call bug('w','Units not /beam, sumbeam=1 assumed')
      endif

      if (toKelvin) then
        if (index(units,'BEAM').eq.0) then
          call bug('w','Cannot calculate conversion from flux to TB')
          toKelvin = .false.
          ok = .false.
        endif

        call rdhdd(lIn, 'restfreq', restfreq, 0d0)
        if (restfreq.eq.0d0) then
          call bug('w','Restfreq unknown, cannot calculate '//
     *                 'conversion from flux to TB')
          toKelvin = .false.
          ok = .false.
        endif
      endif
      if (.not.ok) return

c     Identify the longitude (RA) and latitude (Dec) axes.
      call coFindAx(lIn, 'longitude', lngAx)
      call coFindAx(lIn, 'latitude',  latAx)
      call assertl(lngAx.gt.0, 'Longitude axis not found')
      call assertl(latAx.gt.0, 'Latitude axis not found')
      plane(1:1) = axC(lngAx:lngAx)
      plane(2:2) = axC(latAx:latAx)

c     Set the beamarea of those equal to the map area to be
c     integrated.
      bmsiz(1) = trc(lngAx) - blc(lngAx) + 1
      bmsiz(2) = trc(latAx) - blc(latAx) + 1
      call rdhdi(lIn, 'naxis'//itoaf(lngAx), axlenx, 0)
      call rdhdi(lIn, 'naxis'//itoaf(latAx), axleny, 0)
      if ((bmsiz(1).ne.axlenx .and. mod(bmsiz(1),2).eq.0) .or.
     *    (bmsiz(2).ne.axleny .and. mod(bmsiz(2),2).eq.0)) then
        string = 'The region has an even number of pixels in '
        if (mod(bmsiz(1),2).eq.0) string(44:) = 'x'
        if (mod(bmsiz(2),2).eq.0) string(44:) = 'y'
        if (mod(bmsiz(1),2).eq.0 .and.
     *      mod(bmsiz(2),2).eq.0) string(44:) = 'x and y'
        call bug('w',string)
        call bug('w','thus the beam can not be symmetrically summed')
        call bug('w','which means the sum is badly defined')
        call bug('f','try again, after changing the region keyword')
      endif

c     Read the grid spacings.
      call rdhdd(lIn, 'cdelt'//itoaf(lngAx), grid(1), 0d0)
      call rdhdd(lIn, 'cdelt'//itoaf(latAx), grid(2), 0d0)

c     Check if 'beam' is present.  If so, read it, assume it gives a
c     dataset and try to open it.  If that works iostat will be 0 and a
c     summation must be done.  Else the beam keyword gives the beamsize,
c     and a second number must be read.  If 'beam' was not present, the
c     items bmaj and bmin are found from the header, unless they are not
c     present, in which case an error message is issued.
      beamprs = keyprsnt('beam')
      if (beamprs) then
        call keya('beam', string, ' ')
        call hopen(tbm, string, 'old', iostat)
        if (iostat.eq.0) call hclose(tbm)
      else
        iostat = 1
      endif

      if (iostat.eq.0) then
c       Open beam dataset.
        naxis = MAXNAX
        call xyzopen(tbm, string, 'old', naxis, axlen)

c       Get crpix, and assume this is the beam center.
        call rdhdi(tbm, 'crpix'//itoaf(lngAx), bmctr(1), 0)
        call rdhdi(tbm, 'crpix'//itoaf(latAx), bmctr(2), 0)

c       Set beamset region.
        do i = 1, MAXNAX
          bmblc(i) = 1
          bmtrc(i) = 1
        enddo

        if (bmsiz(1).gt.axlen(lngAx) .or.
     *      bmsiz(2).gt.axlen(latAx)) then
          call bug('w','Beam dataset is smaller than input dataset')
          call bug('f','therefore imspec cannot sum over enough area')
        else if (bmsiz(1).eq.axlenx .and. bmsiz(2).eq.axleny) then
          bmtrc(lngAx) = axlenx
          bmtrc(latAx) = axleny
        else
          bmblc(lngAx) = bmctr(1) - bmsiz(1)/2
          bmtrc(lngAx) = bmctr(1) + bmsiz(1)/2
          bmblc(latAx) = bmctr(2) - bmsiz(2)/2
          bmtrc(latAx) = bmctr(2) + bmsiz(2)/2
        endif
        call xyzsetup(tbm, plane, bmblc, bmtrc, viraxlen, vircsz)

c       Integrate beam.
        beaminfo(SUMBM) = 0d0
        do pix = 1, vircsz(2)
          call xyzpixrd(tbm, pix, value, mask)
          if (mask) beaminfo(SUMBM) = beaminfo(SUMBM) + dble(value)
        enddo

        call xyzclose(tbm)
        beaminfo(BEAMX) = 0d0
        beaminfo(BEAMY) = 0d0

      else
c       Get beam from keyword or from header, in radians.
        call assertl(grid(1).ne.0d0 .and. grid(2).ne.0d0,
     *    'Grid spacing not present in header, cannot sum beam')

        if (beamprs) then
          call matodf(string, beaminfo(BEAMX), 1, ok)
          call assertl(ok, 'Error decoding beam keyword')
          call keyd('beam', beaminfo(BEAMY), beaminfo(BEAMX))
          beaminfo(BEAMX) = beaminfo(BEAMX) * DAS2R
          beaminfo(BEAMY) = beaminfo(BEAMY) * DAS2R
        else
          call rdhdd(lIn, 'bmaj', beaminfo(BEAMX), 0d0)
          call rdhdd(lIn, 'bmin', beaminfo(BEAMY), 0d0)
        endif

c       Relate beam to grid spacing.
        ratio(1) = beaminfo(BEAMX) / abs(grid(1))
        ratio(2) = beaminfo(BEAMY) / abs(grid(2))
        if (ratio(1).eq.0d0 .or. ratio(2).eq.0d0) then
          call bug('w',
     *      'No beam given and no beam in header: sumbeam=1 assumed')
          beaminfo(SUMBM) = 1d0
        else
c         If area large enough integral is simple formula, else
c         calculate it.  For conversion to flux only sum in equivalent
c         region, for conversion to brightness temperature sum full beam
c         always.
          frln2 = 4d0 * log(2d0)
          if (bmsiz(1).gt.8.0*ratio(1) .and.
     *        bmsiz(2).gt.8.0*ratio(2) .or. toKelvin) then
            beaminfo(SUMBM) = (DPI/frln2) * ratio(1)*ratio(2)
          else
            beaminfo(SUMBM) = 0d0
            do i = -bmsiz(1)/2, bmsiz(1)/2
              do j = -bmsiz(2)/2, bmsiz(2)/2
                beaminfo(SUMBM) = beaminfo(SUMBM) +
     *            exp(-frln2*((i/ratio(1))**2+(j/ratio(2))**2))
              enddo
            enddo
          endif
        endif
      endif

c     Conversion from Jansky to Kelvin from S = 2k/lamda^2 * omega * T.
      call rdhdd(lIn, 'restfreq', restfreq, 0d0)
      if (restfreq.ne.0d0) beaminfo(KPERJY) = 1d-26 /
     *       ((2d0*DKMKS /  ((DCMKS/(restfreq*1d9))**2)) *
     *       (beaminfo(SUMBM) * abs(grid(1)*grid(2))))

c     Convert beam to arcseconds.
      beaminfo(BEAMX) = beaminfo(BEAMX) * DR2AS
      beaminfo(BEAMY) = beaminfo(BEAMY) * DR2AS

      end

c***********************************************************************

      subroutine outdev(device)

      character  device*(*)
c-----------------------------------------------------------------------
c  Read the device and log keyword.  Possibly open the logfile.
c  Also: if no plotdevice was given and the listing was suppressed too,
c  make a listing anyway.
c-----------------------------------------------------------------------
      include 'imstat.h'

      character*1024 logfile
c-----------------------------------------------------------------------
      call keya('device', device,  ' ')
      if (device.eq.' ' .and. plotvar(LIST).eq.0) plotvar(LIST) = 1
      call keya('log',    logfile, ' ')
      call logopen(logfile, ' ')

      end

c***********************************************************************

      subroutine labset(axlabel,lIn,file,blc,trc,naxis)

      character axlabel(*)*(*)
      integer   lIn
      character file*(*)
      integer   blc(*), trc(*), naxis
c-----------------------------------------------------------------------
c  Make nice labels and header information, including units and so,
c  ready to be plotted.
c  All this information is stored in the global variable plotpar.
c-----------------------------------------------------------------------
      include 'imstat.h'

      integer   j, k
      real      rdata
      character bln*40, rtoaf*10, string*40, substr*40, tln*40,
     *          units*20, ylab*40

      external  len1
      integer   len1
c-----------------------------------------------------------------------
c     Get the data units.
      call rdhda(lIn, 'bunit', units, 'Unknown')
      if (plotvar(DUNIT).eq.KELVIN) units = 'K'
      string = units
      call lcase(string)

c     Set the x-axis label.
      plotpar(XLABP) = axlabel(1)

c     Set the y-axis label.
      ylab = substr(plotopts, plotvar(SEL))
      if (plotvar(DERIV).gt.0) then
c       Derivative was taken, make it into dy/dx (without units).
        call ucase(ylab(1:1))
        plotpar(YLABP) = 'd' // ylab(:len1(ylab)) // '/d' // axlabel(1)
      else
c       Was the y-axis converted to flux?
        if (index(ylab,'flux').ne.0 .and. index(string,'jy').ne.0) then
c         Remove the /beam or /pixel from the units.
          k = index(string,'beam')
          if (k.ne.0) then
            units = units(:k-1)
            k = len1(units)
            if (units(k:k).eq.'/') units(k:k) = ' '
          endif

          k = index(string,'pixel')
          if (k.ne.0) then
            units = units(:k-1)
            k = len1(units)
            if (units(k:k).eq.'/') units(k:k) = ' '
          endif
        endif

        call ucase(ylab(1:1))
        write(plotpar(YLABP),10) ylab(:len1(ylab)), units(:len1(units))
 10     format(a,' (',a,')')
      endif

c     Construct a header containing the object name, file name, and
c     frequency.
      call rdhda(lIn, 'object',   string, ' ')
      call rdhdr(lIn, 'restfreq', rdata,  0.0)
      if (string.ne.' ') plotpar(INFOP) = 'Source: ' // string
      if (rdata.ne.0.0) then
        k = len1(plotpar(INFOP))
        if (k.eq.0) then
          plotpar(INFOP) = rtoaf(rdata,1,6)
        else
          plotpar(INFOP)(k+1:) = '; ' // rtoaf(rdata,1,6)
        endif

        k = len1(plotpar(INFOP))
        plotpar(INFOP)(k+1:) = ' GHz'
      endif

      if (file.ne.' ') then
        k = len1(plotpar(INFOP))
        if (k.eq.0) then
          plotpar(INFOP) = 'File: ' // file
        else
          plotpar(INFOP)(k+1:) = '; File: ' // file
        endif
      endif

c     Encode the selected box into a nice-looking string.
      call mitoaf(blc, naxis, bln, j)
      call mitoaf(trc, naxis, tln, k)
      string = 'blc=(' // bln(:j) // '), trc=(' // tln(:k) // ')'
      plotpar(BOXP) = 'Bounding box: ' // string

      end

c***********************************************************************

      subroutine header(lIn, file, dim, beaminfo)

      integer          lIn
      character*(*)    file
      integer          dim
      double precision beaminfo(*)
c-----------------------------------------------------------------------
c  Print out some information about the dataset and its units.  Also the
c  beam and sumap if these are used.
c  The informational lines start in column 'offset'.  In a number of
c  cases the number after a piece of text starts in column 'align'.
c-----------------------------------------------------------------------
      include          'imstat.h'

      integer          offset, align
      parameter        (offset=7, align=27)

      integer          i, len1, n(2), nfigd
      character*80     line
      character*40     units
      character*80     rtfmt
      character*9      typ, cubetype
      external rtfmt
c-----------------------------------------------------------------------
      if (plotvar(HEAD).eq.0) return

      line = '***** '//idstr//' of image '//file(:len1(file))//' *****'
      call ucase(line(offset:offset))
      call logwrit(line)

      line = ' '

      line(offset:) = plotpar(BOXP)
      call logwrit(line)

      call rdhda(lIn, 'bunit', units, 'Unknown')
      line(offset:) = 'Unit of datavalues:'
      line(align:)  = units
      call logwrit(line)

      if (plotvar(DUNIT).eq.JANSKY) units =
     *  plotpar(YLABP)(index(plotpar(YLABP),'[')+1 :
     *                 index(plotpar(YLABP),']')-1)
      if (plotvar(DUNIT).eq.KELVIN) units = 'K'
      line(offset:) = 'Unit of ' // idstr // ':'
      line(align:)  = units
      call logwrit(line)

      if (beaminfo(BEAMX).gt.0d0 .and.
     *   (plotvar(DUNIT).eq.JANSKY .or. plotvar(DUNIT).eq.KELVIN)) then
        n(1) = nfigd(beaminfo(BEAMX)) + 3
        n(2) = nfigd(beaminfo(BEAMY)) + 3
        line(offset:) = 'Beam size:'
        write(line(align:), rtfmt('f<>.2,''" x '',f<>.2,''"''', n,2))
     *    beaminfo(BEAMX), beaminfo(BEAMY)
        call logwrit(line)
      endif

      if (plotvar(DUNIT).eq.JANSKY) then
        n(1) = nfigd(beaminfo(SUMBM)) + 4
        write(line(offset:), rtfmt(
     *    '''Sum of beam in equivalent region: '',f<>.3', n, 1))
     *    beaminfo(SUMBM)
        call logwrit(line)
      endif

      if (plotvar(DUNIT).eq.KELVIN) then
        n(1) = nfigd(beaminfo(KPERJY)) + 3
        write(line(offset:), rtfmt(
     *    '''Conversion of Jy/beam to K: '',f<>.2,'' K/(Jy/beam)''',
     *    n,1)) beaminfo(KPERJY)
        call logwrit(line)
      endif

      typ = cubetype(dim)
      line(offset:) = 'Axes of ' // typ(:len1(typ)) // 's :'
      do i  = 1, dim
        line(len1(line)+2 :) = ctype(i)
        line(len1(line)+1 :) = ','
      enddo
      call logwrit(line(:len1(line)-1))

      end

c***********************************************************************

      subroutine stats(lIn, naxis, dim, corners, boxes, cut, counts,
     *  beaminfo, axlabel, device)

      integer   lIn, naxis, dim, corners(*), boxes(*)
      real      cut(*)
      integer   counts(0:*)
      double precision beaminfo(*)
      character axlabel(*)*(*), device*(*)
c-----------------------------------------------------------------------
c  Loop over all selected data and calculate the statistics.
c  These are found for different 'levels'.  level 1 corresponds to the
c  statistics of the selected averaging-axes.  level 2 combines these
c  statistics for a subcube with one higher dimension, etc.
c-----------------------------------------------------------------------
      include          'imstat.h'
      integer          MAXRUNS
      parameter (MAXRUNS=3*MAXDIM)

      ptrdiff          pix
      integer          subcube, i
      integer          iloop, nloop
      integer          coo(MAXNAX)
      integer          level, nlevels
      integer          pgbeg
      logical          doplot

      real             data(MAXBUF/2)
      logical          mask(MAXBUF/2)

      integer          runs(3,MAXRUNS), nruns

      logical          inbox, init(MAXNAX)
      double precision v
      integer          npoints(MAXNAX),crners(4)
      double precision maxval(MAXNAX), minval(MAXNAX)
      double precision sum(MAXNAX), sumpbc(MAXNAX), sumsq(MAXNAX)
c-----------------------------------------------------------------------
      nlevels = naxis - abs(dim) + 1
      do i = 1, MAXNAX
        coo(i) = 1
      enddo

      doplot = device.ne.' '
      if (doplot) then
c       Open the plot device.
        if (pgbeg(0,device,1,1).ne.1) then
          call pgldev
          call bug('f', 'Error opening plot device')
        endif
      endif

c     Loop over all subcubes for which statistics are to be calculated.
      do subcube = 1, counts(nlevels)
        pix = subcube
        call xyzs2c(lIn, pix, coo)

        if (abs(dim).eq.2) then
          call boxruns(naxis,coo,'r',boxes,runs,MAXRUNS,nruns,
     *                 crners(1),crners(2),crners(3),crners(4))
          do i = 1, nruns
            runs(1,i) = runs(1,i) + crners(3) - corners(3)
            runs(2,i) = runs(2,i) + crners(1) - corners(1)
            runs(3,i) = runs(3,i) + crners(1) - corners(1)
          enddo
        endif

c       If init(i)=.true., the statistics for this level must be
c       reinitialized.
        init(1) = .true.
        npoints(1) = 0

c       Read a profile or a plane unless dim was -2, in which case a
c       plane is read profile by profile.
        if (dim.gt.0) nloop = 1
        if (dim.eq.-2) nloop = counts(naxis+2)
        if (dim.eq.1) call xyzprfrd(lIn,subcube,data,mask,counts(0))
        if (dim.eq.2) call xyzplnrd(lIn,subcube,data,mask,counts(0))
        do iloop = 1, nloop
          if (dim.eq.-2) call xyzprfrd(lIn,(subcube-1)*nloop+iloop,
     *                                 data,mask,counts(0)*nloop)

          do i = 1, counts(0)
c           If datapoint falls within limits as defined by cutoff and
c           masking, use it.
            if (inbox(dim,i.eq.1 .and. iloop.eq.1,
     *        data(i),mask(i),runs,corners,cut)) then

c             Convert to Kelvin if requested.
              if (plotvar(DUNIT).eq.KELVIN)
     *          data(i) = data(i) * real(beaminfo(KPERJY))
              if (init(1)) then
                npoints(1) = 0
                maxval(1)  = data(i)
                minval(1)  = data(i)
                sum(1)     = 0d0
                sumpbc(1)  = 0d0
                sumsq(1)   = 0d0
                init(1)    = .false.
              endif
              npoints(1) = npoints(1) + 1
              v          = dble(data(i))
              maxval(1)  = max(maxval(1), v)
              minval(1)  = min(minval(1), v)
              sum(1)     = sum(1)    + v
c             sumpbc(1)  = sumpbc(1) + v*pbccorr(i,coo(1))
              sumsq(1)   = sumsq(1)  + v*v
            endif
          enddo
        enddo

c       After looping over all pixels of a selected subcube, add the
c       results to the statistics of all higher levels.  These are
c       reinitialized at appropriate times, namely if all subcubes from
c       one level lower have been handled.
        if (nlevels.ge.2) then
          do level = 2, nlevels
            init(level) = mod(subcube-1, counts(level)).eq.0
            if (.not.init(1)) then
              if (init(level)) then
                npoints(level) = 0
                maxval(level)  = maxval(1)
                minval(level)  = minval(1)
                sum(level)     = 0d0
                sumpbc(level)  = 0d0
                sumsq(level)   = 0d0
                init(level)    = .false.
              endif
              npoints(level) = npoints(level) + npoints(1)
              maxval(level)  = max(maxval(level), maxval(1))
              minval(level)  = min(minval(level), minval(1))
              sum(level)     = sum(level)    + sum(1)
              sumpbc(level)  = sumpbc(level) + sumpbc(1)
              sumsq(level)   = sumsq(level)  + sumsq(1)
            endif
          enddo
        endif

c       After treating each selected subcube, write out the results.
        call results(subcube, lIn, naxis,abs(dim), counts, axlabel,
     *    doplot, npoints, maxval, minval, sum, sumpbc, beaminfo(SUMBM),
     *    sumsq)
      enddo

      if (doplot) call pgend

      end

c***********************************************************************

      subroutine results(subcube, lIn, naxis, dim, counts, axlabel,
     *  doplot, npoints, maxval, minval, sum, sumpbc, sumap, sumsq)

      integer   subcube, lIn, naxis, dim, counts(0:*)
      character axlabel(*)*(*)
      logical   doplot
      integer   npoints(*)
      double precision maxval(*), minval(*), sum(*), sumpbc(*), sumap,
     *          sumsq(*)
c-----------------------------------------------------------------------
c  Routine controlling when results are written, also doing a few
c  on-the-spot conversions.
c-----------------------------------------------------------------------
      include 'imstat.h'

      logical   dotail
      integer   coo(MAXNAX), level, nlevels
      ptrdiff pix
      double precision coords(MAXNAX)
      character cvalues(MAXNAX)*12
c-----------------------------------------------------------------------
      nlevels = naxis - dim + 1

c     Loop over all levels.
      do level = 1, nlevels
c       If all data for a level were treated, save the results.
        if (mod(subcube,counts(level)).eq.0) then
c         Check if output must be flushed (at last subcube of level 1,
c         and for each higher-level subcube.
          dotail = mod(subcube, counts(2)).eq.0

c         Convert the subcube number to pixels numbers (coo) and then to
c         real coordinates (coords) and string-encoded coordinates
c         (cvalues).
          if (level.lt.nlevels) then
            pix = subcube
            call xyzs2c(lIn, pix, coo)
            call getcoo(axlabel, nlevels, coo, coords, cvalues)
          endif

c         Add an entry to the table.
          if (dotail .or. level.eq.1) then
            call tabentry(coo(level),coords(level),cvalues(level),
     *        npoints(level),maxval(level),minval(level),sum(level),
     *        sumpbc(level),sumap,sumsq(level))
          endif

c         Make output for a level if all subcubes were done and added to
c         list.
          if (dotail) then
c           If a header must be written, write it.
            if (plotvar(HEAD).eq.1 .and. plotvar(LIST).eq.1) then
              call tabhead(naxis,dim,nlevels,level,coo,axlabel(level))
            endif

c           Treat the arrays for hanning/derivative and print them.
c           Make the plot if all subcubes of a level were done.
            call statout(doplot,axlabel,cvalues,dim,level,nlevels)
          endif
        endif
      enddo

      end

c***********************************************************************

      subroutine getcoo(axlabel, nlevels, coo, coords, cvalues)

      character axlabel(*)*(*)
      integer   nlevels, coo(*)
      double precision coords(*)
      character cvalues(*)*(*)
c-----------------------------------------------------------------------
c  Convert pixel coordinates to world coordinates and encode as strings.
c-----------------------------------------------------------------------
      include 'imstat.h'

      integer   i, j
      character radec*24

      external  len1
      integer   len1
c-----------------------------------------------------------------------
      do i = 1, nlevels - 1
c       (May not be correct for celestial axes.)
        call coCvt1(cin,cindex(i),'ap',dble(coo(i)),'aw',coords(i))

        cvalues(i) = ' '
        if (axlabel(i).eq.'Right ascension') then
          coords(i) = coords(i)*DR2D
          call deghms(coords(i), 0d0, radec)
          j = len1(radec(1:12))
          cvalues(i)(13-j:12) = radec(1:j)
          coords(i) = coords(i) * 3600d0 / 15d0
        else if (axlabel(i).eq.'Declination') then
          coords(i)  = coords(i)*DR2D
          call deghms(0d0, coords(i), radec)
          j = len1(radec(13:24))
          cvalues(i)(13-j:12) = radec(13:12+j)
          coords(i) = coords(i) * 3600d0
        else
c         Celestial axis?
          if (index(axlabel(i),'(deg)').ne.0) then
            coords(i) = coords(i)*R2D
          endif

          write(cvalues(i), '(f10.1,2x)') coords(i)
        endif
      enddo

      end

c***********************************************************************

      subroutine tabhead(naxis,dim,nlevels,level,coo,axlabel)

      integer   naxis, dim, nlevels, level, coo(*)
      character axlabel*(*)
c-----------------------------------------------------------------------
c  Write a table header, giving the coordinate value of all higher axes,
c  and a nice string for the x-axis.
c-----------------------------------------------------------------------
      include 'imstat.h'

      integer   i, n(4)
      character cubetype*9, label*32, line*80, typ*9

      external  itoaf, len1, rtfmt
      integer   len1
      character itoaf*2, rtfmt*256
c-----------------------------------------------------------------------
      call logwrit(' ')

c     Abbreviate the label to 12 chars.
      label = axlabel(:32)
      if (label.eq.'Right ascension') then
        label = 'RA'
      else if (label.eq.'Relativistic') then
        label = 'Reltvstc vel'
      else
        i = index(label,'Ecliptic ')
        if (i.ne.0) label(i:) = 'E' // label(9:)

        i = index(label,'Galactic ')
        if (i.ne.0) label(i:) = 'G' // label(9:)

        i = index(label,' longitude')
        if (i.ne.0) label(i:) = 'LON'

        i = index(label,' latitude')
        if (i.ne.0) label(i:) = 'LAT'

        i = index(label,'velocity')
        if (i.ne.0) label(i:) = 'vel'

        i = index(label,'(')
        if (i.ne.0) label(i:) = ' '
      endif

      if (level.lt.nlevels) then
        do i = naxis, dim+level, -1
          line = 'Axis'
          line(len1(line)+2:) = itoaf(i)
          line(len1(line)+2:) = '(' // ctype(i)
          line(len1(line)+1:) = ')'
          if (i.gt.dim+level) line(len1(line)+1:) = ':'
          if (i.gt.dim+level) line(20:) = itoaf(coo(i-dim))
          call logwrit(line)
        enddo

        typ  = cubetype(min(dim+level-1,4))
        n(1) = max(1, (9-len1(typ))/2)
        n(2) =   9 - n(1)
        n(3) = max(1, (12-len1(label)+1)/2)
        n(4) =  12 - n(3)

      else if (level.eq.nlevels) then
        typ  = 'Total'
        label = ' '
        n(1) =  1
        n(2) =  8
        n(3) = 11
        n(4) =  1
      endif

      if (plotvar(DUNIT).eq.ORIG .or. plotvar(DUNIT).eq.KELVIN) then
        write(line,rtfmt('<>x,a<>,<>x,a<>,''   Sum      Mean      ' //
     *    'rms     Maximum   Minimum    Npoints''',n,4)) typ, label
        if (NAME.eq.'IMSPEC') line(index(line,'rms') :) = 'Npoints'
      else
        write(line,rtfmt('<>x,a<>,<>x,a<>,''   Flux     PBC Flux  ' //
     *    'Npoints''',n,4)) typ, label
      endif

      call logwrit(line)

      end

c***********************************************************************

      subroutine tabentry(coo, coord, cvalue, npoints, maxval, minval,
     *  sum, sumpbc, sumap, sumsq)

      integer   coo
      double precision coord
      character cvalue*(*)
      integer   npoints
      double precision maxval, minval, sum, sumpbc, sumap, sumsq
c-----------------------------------------------------------------------
c  First convert from the raw statistics to the useful statistics.
c  Then add an entry to the arrays later to be used for plotting and
c  printing.
c-----------------------------------------------------------------------
      include 'imstat.h'

      logical   ok
      double precision calcrms, rms, stats(NSTATS)
c-----------------------------------------------------------------------
c     Convert from raw to useful statistics.
      rms = calcrms(sum, sumsq, npoints, ok)
      if (plotvar(DUNIT).eq.ORIG .or. plotvar(DUNIT).eq.KELVIN) then
        stats(1) = sum
        if (npoints.ne.0) stats(2) = sum / npoints
        if (npoints.eq.0) stats(2) = 0.0
        stats(3) = rms
        stats(4) = maxval
        stats(5) = minval
        stats(6) = npoints
      else
        stats(1) = sum    / sumap
        stats(2) = sumpbc / sumap
        stats(3) = npoints
      endif

c     Add datapoints to plotarrays.
      if (.not.ok) coord = MAGICVAL
      call statsav(coo, coord, cvalue, stats)

      end

c***********************************************************************

      subroutine statout(doplot,axlabel,cvalues,dim,level,nlevels)

      logical   doplot
      character axlabel(*)*(*), cvalues(*)*(*)
      integer   dim, level, nlevels
c-----------------------------------------------------------------------
      include 'imstat.h'

      logical   first
      integer   coo, i, matchnr, n, plt
      real      iarr(MAXCHAN), statarr(NSTATS,MAXCHAN), xarr(MAXCHAN),
     *          yarr(MAXCHAN)
      double precision coord, stats(*)
      character carr(MAXCHAN)*12, cvalue*(*)

      save first, iarr, xarr, carr, statarr, n
      data first / .true. /
c-----------------------------------------------------------------------
      call assertl(n.ne.0, 'All datapoints are masked')

c     Do possible smoothing and derivative-taking on the data,
      plt = matchnr('sum',plotopts)
      do i = 1, n
        yarr(i) = statarr(plt,i)
      enddo

      call smooth(yarr, n, plotvar(DOSMOOTH), plotvar(SMOWID))
      call differ(xarr, yarr, n, plotvar(DERIV))
      do i = 1, n
        statarr(plt,i) = yarr(i)
      enddo

      plt = matchnr('mean',plotopts)
      do i = 1, n
        yarr(i) = statarr(plt,i)
      enddo

      call smooth(yarr, n, plotvar(DOSMOOTH), plotvar(SMOWID))
      call differ(xarr, yarr, n, plotvar(DERIV))
      do i = 1, n
        statarr(plt,i) = yarr(i)
      enddo

c     Write listed output.
      if (plotvar(HEAD).eq.1 .or. level.eq.1)
     *  call wrnums(iarr,xarr,carr, statarr, n, dim,level,nlevels)

c     Copy array to be plotted to array yarr and make plot.
      if (plotvar(SEL).eq.matchnr('flux',   plotopts)) plt=1
      if (plotvar(SEL).eq.matchnr('pbcflux',plotopts)) plt=2
      if (plotvar(SEL).eq.matchnr('sum',    plotopts)) plt=1
      if (plotvar(SEL).eq.matchnr('mean',   plotopts)) plt=2
      if (plotvar(SEL).eq.matchnr('rms',    plotopts)) plt=3
      if (plotvar(SEL).eq.matchnr('maximum',plotopts)) plt=4
      if (plotvar(SEL).eq.matchnr('minimum',plotopts)) plt=5
      do i = 1, n
        yarr(i) = statarr(plt,i)
      enddo
      if (doplot .and. level.eq.1)
     *  call plotstat(iarr, xarr, yarr, n, axlabel,cvalues,nlevels)

      first = .true.

      return


      entry statsav(coo, coord, cvalue, stats)
      if (first) n = 0
      first = .false.
      n = n + 1
      iarr(n) = coo
      xarr(n) = coord
      carr(n) = cvalue
      do plt = 1, NSTATS
        statarr(plt,n) = stats(plt)
      enddo

      end

c***********************************************************************

      subroutine wrnums(iarr,xarr,carr,statarr,n,dim,level,nlevels)

      include          'imstat.h'
      real             iarr(*), xarr(*)
      character*(*)    carr(*)
      real             statarr(NSTATS,*)
      integer          n
      integer          dim, level, nlevels
c-----------------------------------------------------------------------
      integer          nstat
      character*85     line, temp
      character*20     fmt
      character*9      typ, cubetype
      character*5      itoaf
      integer          i, j, len1
c-----------------------------------------------------------------------
      nstat = 3
      if (NAME.eq.'IMSTAT') nstat = 6

      do i = 1, n
        if (xarr(i).ne.MAGICVAL) then
c         Construct the output line for the typed list.
          line = ' '
          if (level.lt.nlevels)
     *      write(line, '(i6,1x, a)') nint(iarr(i)), carr(i)

          if (plotvar(EFMT).eq.1) then
            if (plotvar(GSPAC).eq.1) then
              write(fmt,10) nstat-1
 10           format('(',i1,'(1pe10.3,1x),i8)')
            else
              write(fmt,15) nstat-1
 15           format('(',i1,'(1pe10.3),i8)')
	    endif
          else if (plotvar(GSPAC).eq.1) then
            write(fmt,20) nstat-1
 20         format('(',i1,'(1pg10.2),i8)')
          else
            write(fmt,30) nstat-1
 30         format('(',i1,'(1pg10.3),i8)')
          endif

          write(temp, fmt=fmt) (statarr(j,i),j=1,nstat-1),
     *      nint(statarr(nstat,i))

c         The following is workaround HP compiler bug
          line(len(typ)+13:) = temp
        else
          typ  =  cubetype(min(dim+level-1,4))
          line = 'All points masked for ' // typ(:len1(typ)) // ' ' //
     *           itoaf(nint(iarr(i)))
        endif

        if (plotvar(LIST).eq.1) call logwrit(line)
      enddo

      end

c***********************************************************************

      subroutine plotstat(iarr,xarr,yarr,n,axlabel,cvalues,nlevels)

      real      iarr(*), xarr(*), yarr(*)
      integer   n
      character axlabel(*)*(*), cvalues(*)*(*)
      integer   nlevels
c-----------------------------------------------------------------------
c  Execute the plot:
c  open the plotfile, and make the axes (with the lower x-axis
c  in appropriate units and the upper in channels).  Plot the points
c  with the selected plotstyle and then add the header/title/ids
c-----------------------------------------------------------------------
      include          'imstat.h'

      real             imin, imax, xmax, xmin, ymax, ymin
      character*40     pginfo
      integer          i
c-----------------------------------------------------------------------
c     Find min and max for plot.
      call mnmx(iarr,xarr,yarr,n, imin,imax, xmin,xmax, ymin,ymax)

      call pgpage
      call pgvstd
      call pgqinf('hardcopy',  pginfo, i)
      i = index('NY', pginfo(:1))
      call pgscf(i)

      if (imin.ne.imax .and. plotvar(HEAD).eq.1) then
        call pgswin(imin, imax, ymin, ymax)
        call pgbox('CMST', 0.0, 0, ' ', 0.0, 0)
      endif

      call pgswin(xmin, xmax, ymin, ymax)
      if (.not.(imin.ne.imax .and. plotvar(HEAD).eq.1)) then
        call pgbox('CST',  0.0, 0, ' ', 0.0, 0)
      endif

      if (axlabel(1).eq.'Right ascension') then
        call pgtbox('BNSTHYZ', 0.0, 0, 'BCNST', 0.0, 0)
      else if (axlabel(1).eq.'Declination') then
        call pgtbox('BNSTDYZ', 0.0, 0, 'BCNST', 0.0, 0)
      else
        call pgtbox('BNST',    0.0, 0, 'BCNST', 0.0, 0)
      endif

      call pgpts(xarr, yarr, n, plotvar(STYLE), ymin)
      call pgident
      if (plotvar(HEAD).eq.1) then
        do i = 3, nlevels
          call pgmtxt('T', -(i-2)-YOFF, XOFF, LEFT,  axlabel(i-1))
          call pgmtxt('T', -(i-2)-YOFF, COFF, RIGHT, cvalues(i-1))
        enddo
      endif
      call output(' ')

      end

c***********************************************************************

      subroutine mnmx(iarr,xarr,yarr,n, imin,imax,xmin,xmax,ymin,ymax)

      real          iarr(*), xarr(*), yarr(*)
      integer       n
      real          imin, imax, xmin, xmax, ymin, ymax
c-----------------------------------------------------------------------
c  Find the plotrange, extending the datarange 8% (EXTEND %) at both
c  sides and substituting user-given values if these were given.
c-----------------------------------------------------------------------
      include       'imstat.h'

      integer       indmin, indmax
      integer       ismin, ismax
      real          EXTEND
      parameter     (EXTEND = 0.08)
      character*40  warning1, warning2
      data          warning1 / 'Wanted minimum above maximum found' /
      data          warning2 / 'Wanted maximum below minumum found' /
c-----------------------------------------------------------------------
      indmin = ismin(n, xarr, 1)
      indmax = ismax(n, xarr, 1)
      if (plotrnge(XTITLE).eq.MAGICVAL) plotrnge(XTITLE) = xarr(indmin)
      imin = iarr(indmin) - EXTEND * (iarr(indmax) - iarr(indmin))
      imax = iarr(indmax) + EXTEND * (iarr(indmax) - iarr(indmin))
      xmin = xarr(indmin) - EXTEND * (xarr(indmax) - xarr(indmin))
      xmax = xarr(indmax) + EXTEND * (xarr(indmax) - xarr(indmin))
      call assertl(xmin.ne.xmax, 'X-range of plot is 0')
      indmin = ismin(n, yarr, 1)
      indmax = ismax(n, yarr, 1)
      if (plotrnge(YTITLE).eq.MAGICVAL) plotrnge(YTITLE) = yarr(indmax)
      ymin = yarr(indmin) - EXTEND * (yarr(indmax) - yarr(indmin))
      ymax = yarr(indmax) + EXTEND * (yarr(indmax) - yarr(indmin))
      call assertl(ymin.ne.ymax, 'Y-range of plot is 0')

      if (plotrnge(FLXL).eq.1 .or. plotrnge(FLXU).eq.1) imin = 0.0
      if (plotrnge(FLXL).eq.1 .or. plotrnge(FLXU).eq.1) imax = 0.0
      if (plotrnge(FLXL).eq.1) then
        call assertl(plotrnge(XLOW).lt.xmax, warning1)
        xmin = plotrnge(XLOW)
      endif
      if (plotrnge(FLXU).eq.1) then
        call assertl(plotrnge(XUPP).gt.xmin, warning2)
        xmax = plotrnge(XUPP)
      endif
      if (plotrnge(FLYL).eq.1) then
        call assertl(plotrnge(YLOW).lt.ymax, warning1)
        ymin = plotrnge(YLOW)
      endif
      if (plotrnge(FLYU).eq.1) then
        call assertl(plotrnge(YUPP).gt.ymin, warning2)
        ymax = plotrnge(YUPP)
      endif

      end

c***********************************************************************

      subroutine smooth(yarr, n, mode, width)
      real             yarr(*)
      integer          n, mode, width
c-----------------------------------------------------------------------
c  Hanning or boxcar smooth the data.
c-----------------------------------------------------------------------
      integer          MAXWIDTH
      parameter        (MAXWIDTH = 7)
      real             coeffs(MAXWIDTH*2+1), work(MAXWIDTH*2+1)
      integer          HANNING, BOXCAR
      parameter        (HANNING=1, BOXCAR=2)
c-----------------------------------------------------------------------
      if (  mode.eq.HANNING) then
        call hcoeffs(width, coeffs)
        call hannsm(width, coeffs, n, yarr, work)
      else if (mode.eq.BOXCAR) then
        call bcoeffs(width, coeffs)
        call boxcarsm(width, coeffs, n, yarr, work)
      endif

      end

c***********************************************************************

      subroutine differ(xarr, yarr, n, mode)
      real          xarr(*), yarr(*)
      integer       n, mode
c-----------------------------------------------------------------------
c  Take a one-sided or two-sided derivative.
c-----------------------------------------------------------------------
      integer       i
c-----------------------------------------------------------------------
      if (  mode.eq.1) then
        do i = 2, n
          yarr(i-1) = (yarr(i)-yarr(i-1))/(xarr(i)-xarr(i-1))
        enddo
      else if (mode.eq.2) then
        do i = 2, n-1
          yarr(i-1) = (yarr(i+1)-yarr(i-1))/(xarr(i+1)-xarr(i-1))
        enddo
        yarr(n-1) = yarr(n-2)
      endif
      if (mode.gt.0) then
        do i = n, 2, -1
          yarr(i) = yarr(i-1)
        enddo
        yarr(1) = yarr(2)
      endif

      end

c***********************************************************************

      character*(*) function cubetype(arg)
      integer arg
c-----------------------------------------------------------------------
c  Return a normal name to identify the type of the cube.
c  Do it via this function to avoid having a data statement in the
c  include file.
c-----------------------------------------------------------------------
      character*9 types(4)
      data types / 'profile', 'plane', 'cube', 'hypercube' /
c-----------------------------------------------------------------------
      cubetype = types(arg)

      end

c***********************************************************************

      logical function inbox(dim, init, data, mask, runs,corners, cut)

      integer dim
      real    data
      logical mask, init
      integer runs(3,*)
      integer corners(*)
      real    cut(*)
c-----------------------------------------------------------------------
c  Test if data are within unmasked, above the cut and inside the region
c-----------------------------------------------------------------------
      integer runpnt, xlen, x,y
      save    runpnt, xlen, x,y
      logical unmasked
c-----------------------------------------------------------------------
      if (init) then
        runpnt = 1
        x      = 0
        y      = 1
        xlen   = corners(2) - corners(1) + 1
      endif

      if (cut(2).eq.0.0) then
        unmasked = mask
      else if (cut(2).eq.1.0) then
        unmasked = mask .and. data.ge.cut(1)
      else if (cut(2).eq.2.0) then
        unmasked = mask .and. abs(data).ge.cut(1)
      else if (cut(2).eq.3.0) then
        unmasked = mask .and. abs(data).le.cut(1)
      endif
c     if (mask) print*,'    mask'

      if (abs(dim).eq.2) then
        x = x + 1
        if (x.gt.xlen) then
          x = 1
          y = y + 1
        endif
        if (runs(1,runpnt).eq.y) then
          if (unmasked) then
            inbox = runs(2,runpnt).le.x .and. x.le.runs(3,runpnt)
          else
            inbox = .false.
          endif
          if (x.eq.runs(3,runpnt)) runpnt = runpnt + 1
        else
          inbox = .false.
        endif
      else
        inbox = unmasked
      endif

      end

c***********************************************************************

      double precision function calcrms(sum, sumsq, npoints, ok)

      double precision sum, sumsq
      integer          npoints
      logical          ok
c-----------------------------------------------------------------------
c  Calculate the rms from the sum and sum-squared.
c-----------------------------------------------------------------------
      double precision rms
c-----------------------------------------------------------------------
      if (npoints.ge.2) then
        rms = (sumsq - sum**2/dble(npoints)) / dble(npoints-1)
        if (rms.ge.0d0) then
          rms = sqrt(rms)
          ok = .true.
        else
          call bug('w', 'Rms^2 is negative!! Square root not taken')
c         ok = .false.
        endif
      else if (npoints.eq.1) then
        rms = 0d0
        ok = .true.
      else if (npoints.eq.0) then
        ok = .false.
      endif

      calcrms = rms

      end

c***********************************************************************

      subroutine pgpts(xarr, yarr, n, plotstyl, ymin)
      real       xarr(*), yarr(*)
      integer    n, plotstyl
      real       ymin
c-----------------------------------------------------------------------
c  Plot points in three possible different styles.
c-----------------------------------------------------------------------
      if (plotstyl.eq.1) call pgline(n,xarr,yarr)
      if (plotstyl.eq.2) call pgbin(n,xarr,yarr,.true.)
      if (plotstyl.eq.3) call pgcbin(n,xarr,yarr,.true.,ymin)

      end

c***********************************************************************

      subroutine pgcbin(n, xarr, yarr, center, ymin)

      integer      n
      real         xarr(*), yarr(*)
      logical      center
      real         ymin
c-----------------------------------------------------------------------
c  Connect points and make bars down to the x-axis.
c-----------------------------------------------------------------------
      real         xpts(4), ypts(4)
      real         dx, xoff, x
      integer      i
c-----------------------------------------------------------------------
      do i = 1, n
        if (i.lt.n) then
          dx = xarr(i+1) - xarr(i)
        else
          dx = xarr(n) - xarr(n-1)
        endif

        if (center) then
          xoff = dx / 2.0
        else
          xoff = 0.0
        endif

        x       = xarr(i) - xoff
        xpts(1) = x
        ypts(1) = ymin
        xpts(2) = x
        ypts(2) = yarr(i)
        xpts(3) = x + dx
        ypts(3) = yarr(i)
        xpts(4) = x + dx
        ypts(4) = ymin
        call pgline(4, xpts, ypts)
      enddo

      end

c***********************************************************************

      subroutine pgident

c-----------------------------------------------------------------------
c  Write a title above the plot.
c-----------------------------------------------------------------------
      include 'imstat.h'

      integer   i
      character ident*48, pginfo*40
c-----------------------------------------------------------------------
      call pgsch(1.0)
      call pglab(plotpar(XLABP), plotpar(YLABP), ' ')

      if (plotpar(TITLE).ne.' ') then
        call pgtext(plotrnge(XTITLE),plotrnge(YTITLE),plotpar(TITLE))
      endif

      if (plotvar(HEAD).eq.1) then
        call pgsch(SC)
        call pgqinf('now', pginfo, i)
        ident = NAME // ' ' // pginfo
        call pgmtxt('T', BASE+2+YOFF, XOFF, LEFT, ident)
        call pgmtxt('T', BASE+1+YOFF, XOFF, LEFT, plotpar(INFOP))
        call pgmtxt('T', BASE  +YOFF, XOFF, LEFT, plotpar(BOXP))
      endif

      end
