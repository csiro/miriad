c= imstat - calculate and plot map statistics
c& bpw
c: map analysis
c+
c  Imstat calculates statistics for images. These are the sum, mean,
c  rms, maximum and minimum value of a region. Statistics can be found
c  for profiles or planes, specified using the axes-keyword.
c  The data can be converted to Kelvin, by using 'options=tb' and the
c  beam keyword.
c  Output can be written to the terminal, a log file, or a plot. The
c  options keyword gives control over the plot.
c  The plotheader can be suppressed by using options=noheader. An
c  alternative title can be put on the plot by options=title. A useful
c  combination is 'options=noh,ti,title', to get only the string 'title',
c  instead of the full header.
c
c< in
c< region
c
c@ plot
c  This selects which statistic will be plotted as function of e.g.
c  velocity. Minimal matching is applied. The default is 'rms'.
c
c   'sum'         Plot the sum
c   'mean'        Plot the mean
c   'rms'         Plot the rms
c   'maximum'     Plot the maximum
c   'minimum'     Plot the minimum
c
c@ options
c  The options control the characteristics of the plot.
c  Possible options are (minimal matching is done):
c
c   'tb'          Convert the units to brightness temperature, using
c                 the input for the beam keyword
c
c   'noheader'    Do not write the header information, just the numbers,
c                 producing an ASCII file for a plotting program
c   'nolist'      Do not write the statistics to the screen/logfile
c   'eformat'     Always use format 'e' instead of 'g' to write results
c
c   'xmin,#'      Give lower x-value on axis
c   'xmax,#'      Give upper x-value on axis
c   'ymin,#'      Give lower y-value on axis
c   'ymax,#'      Give upper y-value on axis
c                 (for these four options the default is autoscaling)
c   'title,#1,#2,#3' Put the string #1 at x-position #2 and y-position #3,
c                 with positions measured in units of the coordinates
c                 on the axes. If 'title' is the last option, the title
c                 is put in the upper left hand corner.
c   'style,#'     This selects the plot style.
c                 #=connect means connect the datapoints
c                 #=step means make one-bin wide connected horizontal
c                 line segments
c                 #=histo means bins are drawn as a horizontal line
c                 surrounded by two vertical lines
c
c@ cutoff
c  All datavalues below the cutoff are not used for the calculation of
c  statistics. Give one real value, which may be followed by the string
c  ',abs' to get a cutoff in the absolute value of the datavalues.
c  Default is no cutoff.
c
c@ beam
c  If options=tb is used, imstat calculates the sum divided by the sum
c  of the beam to get the flux in the selected area, if the units of the
c  input data are 'per beam'. This is then converted to Kelvin by
c  dividing by 2k/lambda^2 * omega, where omega is found from the beam
c  keyword.
c  If the name of a dataset is given for 'beam', imstat assumes it
c  contains a beampattern and sums the data in a region of the same
c  size as the input region.
c  Else, it assumes that 'beam' gives the major and minor axes of the
c  beam in arcsec and it calculates the sum for a gaussian beam of that
c  size.
c  If 'beam' is omitted, but 'options=tb' was selected, the beam is
c  found from the header (items bmaj and bmin). If neither is present,
c  no conversion is done.
c
c@ axes
c  This keyword gives the axis (for profiles) or axes (for planes) along
c  which statistics are calculated. E.g. 'axes=ra,dec' means that
c  statistics are found for ra-dec planes (irrespective of their
c  orientation in the dataset). 'axes=vel' gives statistics of a velocity
c  profile at each ra and dec position in the selected region.
c  The default is to calculate statistics for each ra,dec plane as
c  function of velocity.
c  Possible values for are: 'rascension', 'declination', 'longitude',
c  'latitude', 'glongitude', 'glatitude', 'velocity', 'frequency',
c  'channel', 'stokes', 'x', 'y', 'z', 'a', 'b'. Upper case and
c  capitalized versions and the string 'R.A.' are also recognized.
c  Minimal matching is applied. One or two axes may be given.
c
c< device
c@ log
c  If specified, output is written to the file given by log= instead
c  of to the terminal.
c--
c
c   History:
c
c    20jul91  bpw  Original version
c    29jul91  bpw  Corrected bug for 2-d datasets
c    06aug91  bpw  Corrected channel scale, added flux calculation,
c                  included imspect, created imstahis.h
c    19sep91  bpw  Minor overhaul to make splitting easier and to
c                  finish off some leftover things
c    24sep91  bpw  Split imstahis.for, more fine-tuning
c    27sep91  bpw  Add boxcar smooth, got rid of #ifdefs
c    28sep91  bpw  Add deriv options
c    08oct91  bpw  Got rid of many (but not all!) flint warnings
c    09oct91  bpw  Corrected three bugs in calculation of sumbeam
c    13oct91  bpw  Corrected calculation of ra and dec 
c    13jan92  bpw  Now still smarter when summing beam (if whole map summed)
c    27mar92  bpw  Changed assert into assertl
c    07apr92  bpw  Read huge planes profile by profile
c    27apr92  bpw  Change positioning of histogram
c    13jul92  bpw  Changed output logic, so that hanning smoothed profiles
c                  are properly printed
c    09sep92  bpw  Fixed bug in counts(2) calculation for naxis=2 datasets
c    15dec92  bpw  Adapt for changed fndaxnum
c     2mar92  bpw  Fixed output for case everything is masked
c    13mar93  mjs  pgplot subr names have less than 7 chars
c    24mar93  rjs  Halve the size of arrays using MAXBUF.
c    21may93  mjs  Call pgask(.FALSE.) to override any pgplot default
c    03jun93   jm  Removed pgask(.FALSE.) call and also added printout
c                  of valid devices if input device name is incorrect
c    15jun93  bpw  Did paging with pgcurs to get back preferred situation before
c                  pgplot changes, where clicking left resulted in paging
c    16jun93  bpw  Recorrected pgcurs because of some problems.
c    14jul93  bpw  Fix error in output if planes fully masked.
c    30sep93   jm  Corrected the calling sequence of pgplot routines.
c    26nov93  bpw  Fix to avoid array bounds violation pointed out by rjs.
c                  Also work arounds HP compiler bug as indicated by rjs
c    17jun94  bpw  Finally add the real region keyword (i.e. boxruns),
c                  because Doug needed it
c    25oct94  bpw  introduce option 'eformat'
c    22nov94  bpw  fixed non-plotting: forgot to change some values of plotvar
c                  parameters (equivalent to a C 'enum').
c    28jun95  mhw  change printing formats: guarantee 1 space between fields
c                  in eformat mode and add some decimal places to the freq
c    10jan96  rjs  Make MAXRUNS depend on MAXDIM. Also eliminate dfloat.
c    08oct96  rjs  Fix call to inbox. Use Fortran-5 functions. Propogate
c	           MAXBOXES.
c    29nov96  rjs  Change crpix from integer to double, and include mhw's
c		   formatting changes.
c------------------------------------------------------------------------

c Main program of imstat and imspec. Puts out the identification, where
c the variable NAME from the include file contains the program name.
c Then gets all inputs and does the work before closing up.
c
c Variables:
c   tinp       pointer to input dataset
c   naxis      number of axes of dataset
c   dim        dimension of subcube in which data are averaged
c   cut        1st el: cutoff value
c              2nd el: flag to indicate if cut was requested
c                      0-> no cut, 1-> cut, 2-> cut in absolute value
c   counts     counts number of pixels/profiles/planes to handle
c              el 0: # pixels
c              el 1: 1
c              el 2: loop size, i.e. #profiles if dim=1, #planes if dim=2 etc
c              el naxis+2: # profiles in a plane, if plane read profile by pr.
c                 (clumsy, since introduced much later as a quick fix)
c   beaminfo   el 1: sum of beam
c              el 2: conversion factor from Jansky to Kelvin
c              el 3: beamsize in x in arcsec
c              el 4: beamsize in y in arcsec
c   axlabel    el 1: label to put along x-axis
c              el 2-naxis: names of higher axes
c              el naxis+1: units of x-axis
c   device     plot device, no plot if device=' '
c
c A number of variables and flags are made into global variables by using
c the include file.

      program imstaspc

      character*21     version
      parameter        ( version = 'version 2.2 28-Nov-96' )
      character*29     string

      include          'imstat.h'

      integer          tinp
      integer          naxis, dim
      integer          corners(4), boxes(MAXBOXES)
      real             cut(2)
      integer          counts(0:MAXNAX+2)
      double precision beaminfo(4)
      character*12     axlabel(MAXNAX+1)
      character*80     device

      string = NAME // ': ' // version
      call output( string )
      call inputs( tinp,naxis,dim,corners,boxes,cut,counts,
     *             beaminfo,axlabel,device, MAXBOXES )
      call stats(  tinp,naxis,dim,corners,boxes,cut,counts,
     *             beaminfo,axlabel,device )
      call xyzclose( tinp )
      call logclose

      end


************************************************************************

c Get all the inputs.
c First get the input file, and open it
c Then the region in this file to handle
c optinp decodes the keywords plot and options
c cutinp gets the possible cutoff value
c axinp decodes the keyword axes and returns the subcube specification of
c       the averaging-axes and the nice labels along the axes
c then initialize the input dataset. This must be done here because
c beamsum opens and reads another dataset, and before reading a dataset
c         all xyzsetups must have been done
c         beamsum decodes the beam keyword and returns sumap and Jy->K factor
c outdev gets the plotdevice, and opens the logfile
c
c After all input are read labset makes the final nice labels to put along
c the plot
c header puts out some information about the dataset and units

      subroutine inputs( tinp,naxis,dim, corners, boxes, cut, counts,
     *                   beaminfo, axlabel, device, MAXBOXES )

      integer            tinp
      integer            naxis, dim, MAXBOXES
      integer            corners(*), boxes(*)
      real               cut(*)
      integer            counts(0:*)
      double precision   beaminfo(*)
      character*(*)      axlabel(*)
      character*(*)      device
      include            'maxdim.h'
      include            'maxnax.h'

      integer            i, dimen

      character*1024     file
      integer            axlen( MAXNAX )
      integer            blc(MAXNAX), trc(MAXNAX)
      integer            viraxlen( MAXNAX ), vircsz( MAXNAX )
      character*(MAXNAX) subcube

      call keyini

      call keyf( 'in', file, ' ' )
      naxis = MAXNAX
      call xyzopen( tinp, file, 'old', naxis, axlen )

      call boxinput( 'region', file, boxes, MAXBOXES )
      call boxset(   boxes, naxis, axlen, ' ' )
      call boxinfo(  boxes, naxis, blc, trc )
      corners(1) = blc(1)
      corners(2) = trc(1)
      corners(3) = blc(2)
      corners(4) = trc(2)
                                                  
      call optinp

      call cutinp( cut )

      call axinp( tinp,naxis, dim,subcube,axlabel )

      dimen = abs( dim )
      call xyzsetup( tinp, subcube(:dimen), blc,trc, viraxlen,vircsz )
      if( dim.gt. 0 ) counts(0)       = vircsz(dimen)
      if( dim.eq.-2 ) counts(0)       = viraxlen(1)
      if( dim.eq.-2 ) counts(naxis+2) = viraxlen(2)
      do i = dimen, naxis
         counts(i-dimen+1) = vircsz(i) / vircsz(dimen)
      enddo
      counts(naxis-dimen+2) = counts(naxis-dimen+1)

      call beamsum( tinp, blc, trc, beaminfo )

      call outdev( device )

      call keyfin

      call labset( axlabel, tinp, file, blc, trc, naxis )

      call header( tinp, file, dimen, beaminfo )

      return
      end


************************************************************************

c Get the cutoff value to apply.
c cut is an array of 2 elements, the first being the cutoff, the second
c a flag indicating if a cutoff was requested and whether or not this
c was an absolute value cutoff.

      subroutine cutinp( cut )

      real          cut(*)

      character*10  string
      logical       keyprsnt

      if( keyprsnt( 'cutoff' ) ) then
         call keyr( 'cutoff', cut(1),      0. )
         call keya( 'cutoff', string, 'noabs' )
         if( string.ne.'abs') cut(2) = 1.
         if( string.eq.'abs') cut(2) = 2.
      else
         cut(2) = 0.
      endif

      return
      end

 
***********************************************************************

c Decode the plot and options keywords.
c The global variables plotvar, plotrnge and plotpar are filled here.
c plotvar contains flags to indicate if (and sometimes which) options
c were selected.
c plotrnge gives the range of x and/or y values to plot, and also a
c flag to indicate if a range was selected
c Of plotpar just one element is filled here, the one containing an
c optional user-given title.

      subroutine optinp

      include       'imstat.h'

      character*20  option
      logical       match
      integer       i, count
      real          v
      integer       matchnr
      integer       len1
      character*80  line

c Set default values for options
c defplt is 'rms' for IMSTAT and 'flux' for IMSPEC
      plotvar( SEL)       = matchnr( defplt, plotopts )
c the default is to write a header
      plotvar( HEAD )     =  1
c the default is to write out the spectrum for IMSTAT, not for IMSPEC
      if( NAME.eq.'IMSTAT' ) plotvar( LIST ) =  1
      if( NAME.eq.'IMSPEC' ) plotvar( LIST ) =  0
c the default is 'g' format
      plotvar( EFMT )     = 0
c the default plot style is to connect the points
      plotvar( STYLE )    = matchnr( 'connect', styles )
c the default is not to convert the data units
      plotvar( DUNIT )    = ORIG
c the default is not to smooth
      plotvar( DOSMOOTH ) =  0
c the default is not to take the derivative
      plotvar( DERIV )    =  0
c the default is no extra title
      plotpar( TITLE )    = ' '
c the default is no range selection 
      plotrnge( FLXL )    = 0.
      plotrnge( FLXU )    = 0.
      plotrnge( FLYL )    = 0.
      plotrnge( FLYU )    = 0.


c Loop over the plot keyword, to test if input is only one value
c If so, plotvar(SEL) is set to the selected plot index
      call keya( 'plot', option, ' ' )
      count = 0
      do while( option.ne. ' ' )
         call lcase( option )
         if( match( option, plotopts, i )  ) then
            line = 'Only one of '//plotopts//' can be given at one time'
            call assertl( count.eq.0, line )
            plotvar(SEL) = i
            count = count + 1
         else
            line = 'Illegal option ' // option(:len1(option))
            call bug( 'f', line )
         endif
         call keya( 'plot', option, ' ' )
      enddo
c spectrum units are Jansky if plot=flux was selected.
      if( plotvar(SEL) .eq. matchnr('flux',   plotopts) .or.
     *    plotvar(SEL) .eq. matchnr('pbcflux',plotopts)      )
     *    plotvar(DUNIT) = JANSKY


c Loop over the options keyword. First match the option against the full
c list. If present decode the option, else give a warning. For options
c with arguments, these are read and tested.
      call keya( 'options', option, ' ' )
      do while( option.ne. ' ' )
         call lcase( option )

         if( match( option, commonop, i )  ) then
            if( i.eq.0 ) then
            elseif( i.eq.matchnr('noheader',commonop) ) then
               plotvar(HEAD) = 0
            elseif( i.eq.matchnr('nolist',commonop) ) then
               plotvar(LIST) = 0
            elseif( i.eq.matchnr('list',commonop) ) then
               plotvar(LIST) = 1
            elseif( i.eq.matchnr('eformat',commonop) ) then
               plotvar(EFMT) = 1
            elseif( i.eq.matchnr('style',commonop) ) then
               call keya( 'options', option, ' ' )
               call assertl( match(option,styles,i), 'Illegal style' )
               plotvar(STYLE) = i
            elseif( i.eq.matchnr('tb',commonop) ) then
               if( plotvar(SEL) .eq. matchnr('flux',   plotopts) .or.
     *             plotvar(SEL) .eq. matchnr('pbcflux',plotopts)      )
     *             call bug( 'f', 
     *             'Option TB cannot be combined with flux or pbcflux' )
               plotvar(DUNIT) = KELVIN
            elseif( i.eq.matchnr('hanning',commonop) ) then
               call keyi( 'options', i, 3 )
               call assertl( i.lt.15, 'Hanning width too large' )
               call assertl( (i/2)*2.ne.i, 'Width must be odd number' )
               plotvar(DOSMOOTH) = HANNING
               plotvar(SMOWID)   = i
            elseif( i.eq.matchnr('boxcar',commonop) ) then
               call keyi( 'options', i, 1 )
               call assertl( i.lt.14, 'Boxcar width too large' )
               plotvar(DOSMOOTH) = BOXCAR
               plotvar(SMOWID)   = i
            elseif( i.eq.matchnr('deriv',commonop) ) then
               call keyi( 'options', i, 0 )
               call assertl( i.eq.1 .or. i.eq.2,
     *                       'Argument to deriv can only be 1 or 2' )
               plotvar(DERIV) = i
            elseif( i.eq.matchnr('title',commonop) ) then
               call keya( 'options', plotpar(TITLE),   ' ' )
               call keyr( 'options', plotrnge(XTITLE), MAGICVAL )
               call keyr( 'options', plotrnge(YTITLE), MAGICVAL) 
            else
               call keyr( 'options', v, 0. )
               if(     i.eq.matchnr('xmin',commonop) ) then
                  plotrnge(FLXL) = 1.
                  plotrnge(XLOW) = v
               elseif( i.eq.matchnr('xmax',commonop) ) then
                  plotrnge(FLXU) = 1.
                  plotrnge(XUPP) = v
               elseif( i.eq.matchnr('ymin',commonop) ) then
                  plotrnge(FLYL) = 1.
                  plotrnge(YLOW) = v
               elseif( i.eq.matchnr('ymax',commonop) ) then
                  plotrnge(FLYU) = 1.
                  plotrnge(YUPP) = v
               endif
            endif

         else
            line = 'Illegal or ambiguous option ' //
     *             option(:len1(option)) // ' ignored'
            call bug( 'w', line )
            
         endif
         call keya( 'options', option, ' ' )
      enddo

      return
      end


************************************************************************

c Here the axes keyword is decoded. 

      subroutine axinp( tinp, naxis,dim,subcube, axlabel )

      integer          tinp
      integer          naxis, dim
      character*(*)    subcube
      character*(*)    axlabel(*)
      include          'imstat.h'


      integer          i, j, n
      logical          match
      integer          indek
      integer          len1

      logical          keyprsnt
      integer          NAXOPT
      parameter        ( NAXOPT = 17 )
      logical          present( 3*NAXOPT )
      character*12     axopt(3*NAXOPT), axunit(NAXOPT), axtype(NAXOPT)
      character*512    axopts
      integer          axnum(MAXNAX), axind(MAXNAX), axlen(MAXNAX)

      character*50     line
      character*1      axisname
      character*8      keyw
      character*9      temp
      character*(MAXNAX) axnames

      data             axnames / 'xyzabcd' /
      data             axopt /
     *                 'R.A.',       'Declination', 'Longitude',
     *                 'Latitude',   'Glongitude',  'Glatitude',
     *                 'Velocity',   'Frequency',   'Channel',
     *                 'Stokes',     'x', 'y', 'z', 'a', 'b', 'c', 'd',
     *                 'rascension', 'declination', 'longitude',
     *                 'latitude',   'glongitude',  'glatitude',
     *                 'velocity',   'frequency',   'channel',
     *                 'stokes',     '1', '2', '3', '4', '5', '6', '7',
     *                 'RAscension', 'DECLINATION', 'LONGITUDE',
     *                 'LATITUDE',   'GLONGITUDE',  'GLATITUDE',
     *                 'VELOCITY',   'FREQUENCY',   'CHANNEL',
     *                 'STOKES',     'X', 'Y', 'Z', 'A', 'B', 'C', 'D' /
      data             axunit /
     *                 '[]','[]','[deg]','[deg]','[deg]','[deg]',
     *                 '[km/s]','[GHz]','[]','[]', 7*'[]' /
      data             axtype /
     *                 'Lon',  'Lat',  'Lon',  'Lat',  'Lon',  'Lat',
     *                 'Freq', 'Freq', 'Freq', 'Stokes',
     *                 'x', 'y', 'z', 'a', 'b', 'c', 'd' /
      axopts = ' '
      do i = 1, 3*NAXOPT
         axopts(len1(axopts)+1:) = axopt(i)(:len1(axopt(i))) // ','
      enddo
      axopts(len1(axopts):) = ' '
      do i = 1, 3*NAXOPT
         present(i) = .false.
      enddo

c Read the axes keyword and put result into array present.
c Default for axes is to take first two axes
      if ( keyprsnt('axes') ) then
         call options( 'axes', axopt, present, 3*NAXOPT )
      else
         present(1) = .true.
         present(2) = .true.
      endif

c Find the axisnumbers corresponding to the selected axes
      dim = 0
      do i = 1, 3*NAXOPT
         if( present(i) ) then
            j   = mod( i-1, NAXOPT ) + 1
            dim = dim + 1
            axisname = ' '
            call fndaxnum( tinp, axtype(j), axisname, axnum(dim) )
            line = 'No '//axtype(j)(:len1(axtype(j)))//' axis found'
            call assertl( axisname.ne.' ', line )
         endif
      enddo
c Sort the axes found, to insure most efficient reading of image.
      call assertl( dim.le.2, 'A maximum of 2 axes may be given' )
      call hsorti( dim, axnum, axind )
c Set subcube variable for xyzsetup to the selected axes list.
      do n = 1, dim
         subcube(n:n) = axnames( axnum(axind(n)):axnum(axind(n)) )
      enddo

c Now read the naxis, ctype, crval, crpix and cdelt for all axes. But
c do something with the order: el. 1 to el. dim will correspond to the
c list given by the axes keyword, i.e. the axes over which to average.
c The rest will be the other axes, with the lowest numbered first.
      do n = 1, dim
         i = axind(n)
         call rdhda( tinp, keyw('ctype',axnum(i)), ctype(n), 'Unknown' )
         call rdhdi( tinp, keyw('naxis',axnum(i)), axlen(n), 0         )
      enddo
      if( dim+1 .le. naxis ) then
         n = dim
         do i = 1, naxis
            if( index( subcube, axnames(i:i) ) .eq. 0 ) then
               n = n + 1
               if( i.le.2 ) ctype(n) = 'Position'
               if( i.eq.3 ) ctype(n) = 'Channel'
               if( i.ge.4 ) ctype(n) = 'Unknown'
c              Following is workaround HP compiler bug
               temp = ctype(n)
               call rdhda( tinp, keyw('ctype',i), ctype(n), temp      )
               call rdhdd( tinp, keyw('crval',i), crval(n-dim), 1.d0  )
               call rdhdd( tinp, keyw('crpix',i), crpix(n-dim), 1.d0  )
               call rdhdd( tinp, keyw('cdelt',i), cdelt(n-dim), 0.d0  )
            endif
         enddo
      endif

c Fill the array axlabel. The first element is the first non-averaging
c axis. No label is set corresponding to the averaging axes.
c axlabel is first set equal to first part of the corresponding ctype
c variable. This string is then searched-for in the list of axisnames
c axopts (which contains lots of possibilities with different spellings
c and character cases) and an index is returned. Next this is converted
c to a standard label.
c The units of the first axis (the one which will be the x-axis on the
c plot) are saved in axlabel(naxis+1).
      do i = 1, naxis-dim
         axlabel(i) = ctype(dim+i)( : indek(ctype(dim+i),'-')-1 )
         if( match( axlabel(i)(:len1(axlabel(i))), axopts, n ) ) then
            j = mod( n-1, NAXOPT ) + 1
            axlabel(i) = axopt(j)
            if( i.eq.1 ) axlabel(naxis+1) = axunit(j)
         endif
      enddo

c Check if it is necessary to read a plane profile by profile.
c If so, set dim to a negative number to indicate this further down the
c line.
      if( dim.eq.2  .and.  axlen(1)*axlen(2) .gt. MAXBUF/2 ) dim = -dim

      return
      end


      character*8 function keyw( key, i )
      character*(*) key
      integer     i
      integer     len1
      character*1 itoaf
      keyw = key( :len1(key) ) // itoaf(i)
      return
      end


************************************************************************


      subroutine beamsum( tinp, blc, trc, beaminfo )

      integer          tinp
      integer          blc(*), trc(*)
      double precision beaminfo(*)
      include          'imstat.h'

      logical          keyprsnt, beamprs, toKelvin

      if( plotvar(DUNIT).eq.ORIG ) then
         beamprs = keyprsnt('beam')
         if( NAME.eq.'IMSTAT' .and. beamprs ) call bug( 'w',
     *   'The ignored beam keyword only makes sense with option tb' )
         return
      endif

      toKelvin = plotvar(DUNIT).eq.KELVIN
      call sumbeam( tinp, blc, trc, beaminfo, toKelvin )
      if( plotvar(DUNIT).eq.KELVIN .and. .not.toKelvin )
     *plotvar(DUNIT) = ORIG

      return
      end




      subroutine sumbeam( tinp, blc, trc, beaminfo, toKelvin )

      integer          tinp
      integer          blc(*), trc(*)
      double precision beaminfo(*)
      logical          toKelvin
      include          'mirconst.h'

      integer          SUMBM, KPERJY, BEAMX, BEAMY
      parameter        ( SUMBM=1, KPERJY=2, BEAMX=3, BEAMY=4 )

      character*10     units
      logical          ok
      logical          keyprsnt, beamprs
      integer          tbm, iostat

      integer          i, j
      character*2      plane
      integer          axnumx, axnumy
      character*8      keyw
      double precision grid(2), ratio(2), frln2
      double precision restfreq
      character*80     string

      include          'maxdim.h'
      include          'maxnax.h'
      integer          naxis, axlen(MAXNAX)
      integer          axlenx, axleny
      integer          bmblc(MAXNAX), bmtrc(MAXNAX), bmsiz(2), bmctr(2)
      integer          viraxlen(MAXNAX), vircsz(MAXNAX)
      real             value
      logical          mask

      double precision radtosec
      parameter        ( radtosec = 3600.d0 * 180.d0 / dpi )


c First make some tests, i.e. if the dataunits are 'per beam'. If not, the
c data cannot be converted in this way (except if they are 'per pixel',
c in which case the conversion factor is simply 1.
c If Kelvin were requested for the plot, the data units must also be
c 'per beam', and also the wavelength must be known.

      ok = .true.
      beaminfo(SUMBM) = 1.d0
      call rdhda( tinp, 'bunit', units, 'Unknown' )
      call ucase( units )
      if( index(units,'BEAM').eq.0 ) then
         ok = .false.
         if( index(units,'PIXEL').ne.0 ) ok = .true.
         if(.not.ok) call bug('w','Units not /beam, sumbeam=1 assumed')
      endif
      if( toKelvin ) then
         if( index(units,'BEAM').eq.0 ) then
            call bug('w','Cannot calculate conversion from flux to TB')
            toKelvin = .false.
            ok = .false.
         endif
         call rdhdd( tinp, 'restfreq', restfreq, 0.d0 )
         if( restfreq.eq.0.d0 ) then
            call bug( 'w',
     * 'Restfreq unknown, cannot calculate conversion from flux to TB' )
            toKelvin = .false.
            ok = .false.
         endif
      endif
      if( .not.ok ) return


c Determine which axes are the longitude (RA) and latitude (Dec) axes
c and set the beamarea of those equal to the map area to be integrated.
c Also read the gridspacings.
      plane(1:2) = 'xy'
      call fndaxnum( tinp, 'Lon', plane(1:1), axnumx )
      call fndaxnum( tinp, 'Lat', plane(2:2), axnumy )
      bmsiz(1) = trc(axnumx) - blc(axnumx) + 1
      bmsiz(2) = trc(axnumy) - blc(axnumy) + 1
      call rdhdi( tinp, keyw('naxis',axnumx), axlenx, 0 )
      call rdhdi( tinp, keyw('naxis',axnumy), axleny, 0 )
      if( ( bmsiz(1).ne.axlenx .and. mod(bmsiz(1),2).eq.0 ) .or.
     *    ( bmsiz(2).ne.axleny .and. mod(bmsiz(2),2).eq.0 )     ) then
         string = 'The region has an even number of pixels in '
         if( mod(bmsiz(1),2).eq.0 ) string(44:) = 'x'
         if( mod(bmsiz(2),2).eq.0 ) string(44:) = 'y'
         if( mod(bmsiz(1),2).eq.0 .and. mod(bmsiz(2),2).eq.0 )
     *                              string(44:) = 'x and y'
         call bug('w',string)
         call bug('w','thus the beam can not be symmetrically summed')
         call bug('w','which means the sum is badly defined')
         call bug('f','try again, after changing the region keyword')
      endif
      call rdhdd( tinp, keyw('cdelt',axnumx), grid(1), 0.d0 )
      call rdhdd( tinp, keyw('cdelt',axnumy), grid(2), 0.d0 )


c Check if 'beam' is present. If so, read it, assume it gives a dataset
c and try to open it. If that works iostat will be 0 and a summation
c must be done. Else the beam keyword gives the beamsize, and a second
c number must be read. If 'beam' was not present, the items bmaj and
c bmin are found from the header, unless they are not present, in which
c case an error message is issued.
      beamprs = keyprsnt('beam')
      if( beamprs ) then
         call keya( 'beam', string, ' ' )
         call hopen( tbm, string, 'old', iostat )
         if( iostat.eq.0 ) call hclose( tbm )
      else
         iostat = 1
      endif
      if( iostat.eq.0 ) then
c Open beam dataset
         naxis = MAXNAX
         call xyzopen( tbm, string, 'old', naxis, axlen )
c Get crpix, and assume this is the beam center
         call rdhdi( tbm, keyw('crpix',axnumx), bmctr(1), 0 )
         call rdhdi( tbm, keyw('crpix',axnumy), bmctr(2), 0 )
c Set beamset region
         do i = 1, MAXNAX
            bmblc(i) = 1
            bmtrc(i) = 1
         enddo
         if( bmsiz(1).gt.axlen(axnumx) .or.
     *       bmsiz(2).gt.axlen(axnumy)      ) then
            call bug('w','Beam dataset is smaller than input dataset')
            call bug('f','therefore imspec cannot sum over enough area')
         elseif( bmsiz(1).eq.axlenx .and. bmsiz(2).eq.axleny ) then
            bmtrc(axnumx) = axlenx
            bmtrc(axnumy) = axleny
         else
            bmblc(axnumx) = bmctr(1) - bmsiz(1)/2
            bmtrc(axnumx) = bmctr(1) + bmsiz(1)/2
            bmblc(axnumy) = bmctr(2) - bmsiz(2)/2
            bmtrc(axnumy) = bmctr(2) + bmsiz(2)/2
         endif
         call xyzsetup( tbm, plane, bmblc, bmtrc, viraxlen, vircsz )
c Integrate beam
         beaminfo(SUMBM) = 0.d0
         do i = 1, vircsz(2)
            call xyzpixrd( tbm, i, value, mask )
            if( mask ) beaminfo(SUMBM) = beaminfo(SUMBM) + dble(value)
         enddo         
         call xyzclose( tbm )
         beaminfo(BEAMX) = 0.d0
         beaminfo(BEAMY) = 0.d0
      else
c Get beam from keyword or from header, in radians
         call assertl( grid(1).ne.0.d0 .and. grid(2).ne.0.d0,
     *        'Gridspacing not present in header, cannot sum beam' )
         if( beamprs ) then
            call matodf( string, beaminfo(BEAMX), 1, ok )
            call assertl( ok, 'Error decoding beam keyword' )
            call keyd( 'beam', beaminfo(BEAMY), beaminfo(BEAMX) )
            beaminfo(BEAMX) = beaminfo(BEAMX) / radtosec
            beaminfo(BEAMY) = beaminfo(BEAMY) / radtosec
         else
            call rdhdd( tinp, 'bmaj', beaminfo(BEAMX), 0.d0 )
            call rdhdd( tinp, 'bmin', beaminfo(BEAMY), 0.d0 )
         endif
c Relate beam to gridspacing
         ratio(1) = beaminfo(BEAMX) / abs( grid(1) )
         ratio(2) = beaminfo(BEAMY) / abs( grid(2) )
         if( ratio(1).eq.0.d0 .or. ratio(2).eq.0.d0 ) then
            call bug( 'w',
     *      'No beam given and no beam in header: sumbeam=1 assumed' )
            beaminfo(SUMBM) = 1.d0
         else
c If area large enough integral is simple formula, else calculate it.
c For conversion to flux only sum in equivalent region, for conversion
c to brightness temperature sum full beam always.
            frln2 = 4.d0 * log(2.)
            if( bmsiz(1).gt.8.*ratio(1) .and. bmsiz(2).gt.8.*ratio(2)
     *          .or. toKelvin )
     *      then
               beaminfo(SUMBM) = (dpi/frln2) * ratio(1)*ratio(2)
            else
               beaminfo(SUMBM) = 0.d0
               do i = -bmsiz(1)/2, bmsiz(1)/2
                  do j = -bmsiz(2)/2, bmsiz(2)/2
                     beaminfo(SUMBM) = beaminfo(SUMBM) +
     *               exp( -frln2*( (i/ratio(1))**2+(j/ratio(2))**2 ) )
                  enddo
               enddo
            endif
         endif
      endif

c Find the conversion from Jansky to Kelvin from S = 2k/lamda^2 * omega * T.
      call rdhdd( tinp, 'restfreq', restfreq, 0.d0 )
      if( restfreq.ne.0.d0 ) beaminfo(KPERJY) = 1.d-26 /
     *       ( (  2.d0*DKMKS /  ((DCMKS/(restfreq*1.d9))**2)  ) *
     *         ( beaminfo(SUMBM) * abs(grid(1)*grid(2))       )    )

c Convert beam to arcseconds
      beaminfo(BEAMX) = beaminfo(BEAMX) * radtosec
      beaminfo(BEAMY) = beaminfo(BEAMY) * radtosec

      return
      end


************************************************************************

c Read the device and log keyword. Possibly open the logfile.
c Also: if no plotdevice was given and the listing was suppressed too,
c make a listing anyway.

      subroutine outdev( device )

      character*(*)  device
      include        'imstat.h'

      character*1024 logfile

      call keya( 'device', device,  ' ' )
      if( device.eq.' ' .and. plotvar(LIST).eq.0 ) plotvar(LIST) = 1
      call keya( 'log',    logfile, ' ' )
      call logopen( logfile, ' ' )

      return
      end
      

***********************************************************************

c Make nice labels and header information, including units and so, ready
c to be plotted.
c All this information is stored in the global variable plotpar.

      subroutine labset( axlabel,tinp,file,blc,trc,naxis )

      character*(*) axlabel(*)
      integer       tinp
      character*(*) file
      integer       blc(*), trc(*), naxis
      include       'imstat.h'

      integer       len1
      character*40  string
      character*20  units
      character*40  ylab, substr
      real          rdata
      character*10  rtoaf
      integer       i, j
      character*40  bln, tln

c Get the data units
      call rdhda( tinp, 'bunit', units, 'Unknown' )
      if( plotvar(DUNIT).eq.KELVIN ) units = 'K'
      string = units
      call lcase( string )

c Set the x-axis label, by adding units if possible
      plotpar(XLABP) = axlabel(1)
      if( axlabel(naxis+1).ne.'[]' )
     *   plotpar(XLABP) = plotpar(XLABP)(:len1(plotpar(XLABP)))
     *                    // ' ' // axlabel(naxis+1)

c Set the y-axis label.
c If the y-axis was converted to flux, remove the /beam or /pixel from
c the units.
c If the derivative is to be taken, make it into dy/dx.
      ylab = substr( plotopts, plotvar(SEL) )
      if( index(ylab,'flux').ne.0 .and. index(string,'jy').ne.0 ) then
         if( index(string,'beam').ne.0 ) then
            units = units( : index(string,'beam')-1 )
            i = len1(units)
            if( units(i:i).eq.'/' ) units(i:i) = ' '
         endif
         if( index(string,'pixel').ne.0 ) then
            units = units( : index(string,'pixel')-1 )
            i = len1(units)
            if( units(i:i).eq.'/' ) units(i:i) = ' '
         endif
      endif
      call ucase( ylab(1:1) )
      if( plotvar(DERIV).gt.0 ) then
         ylab  = 'd' // ylab(:len1(ylab)) // '/d' // axlabel(1)
         units = units(:len1(units)) // '/(' //
     *           axlabel(naxis+1)(2:len1(axlabel(naxis+1))-1) // ')'
      endif
      write( plotpar(YLABP), '( a, '' ['', a, '']'' )' )
     *       ylab(:len1(ylab)), units(:len1(units))


c Construct a header containing the object name, the file name and
c the frequency.
      call rdhda( tinp, 'object',   string, ' ' )
      call rdhdr( tinp, 'restfreq', rdata,   0. )
      if( string.ne.' ' ) plotpar(INFOP) = 'Source: ' // string
      if( rdata.ne.0. ) then
         if( len1(plotpar(INFOP)).eq.0 ) then
         plotpar(INFOP)                         =      rtoaf(rdata,1,6)
         else
         plotpar(INFOP)(len1(plotpar(INFOP))+1:)='; '//rtoaf(rdata,1,6)
         endif
         plotpar(INFOP)(len1(plotpar(INFOP))+1:)=' GHz'
      endif
      if( file.ne.' ' ) then
         if( len1(plotpar(INFOP)).eq.0 ) then
         plotpar(INFOP)                          =   'File: ' // file
         else
         plotpar(INFOP)(len1(plotpar(INFOP))+1:) = '; File: ' // file
         endif
      endif

c Encode the selected box into a nice-looking string.
      call mitoaf( blc, naxis, bln, i )
      call mitoaf( trc, naxis, tln, j )
      string = 'blc=(' // bln(:i) // '), trc=(' // tln(:j) // ')'
      plotpar(BOXP) = 'Bounding box: ' // string

      return
      end



************************************************************************


c Print out some information about the dataset and its units. Also the
c beam and sumap if these are used.
c The informational lines start in column 'offset'. In a number of cases
c the number after a piece of text starts in column 'align'.

      subroutine header( tinp, file, dim, beaminfo )

      integer          tinp
      character*(*)    file
      integer          dim
      double precision beaminfo(*)
      include          'imstat.h'

      integer          offset, align
      parameter        ( offset=7, align=27 )

      integer          i, len1, n(2), nfigd
      character*80     line
      character*40     units
      character*80     rtfmt
      character*9      typ, cubetype
      external rtfmt

      if( plotvar(HEAD).eq.0 ) return

      line = '***** '//idstr//' of image '//file(:len1(file))//' *****'
      call ucase( line(offset:offset) )
      call logwrit( line )

      line = ' '

      line(offset:) = plotpar(BOXP)
      call logwrit( line )

      call rdhda( tinp, 'bunit', units, 'Unknown' )
      line(offset:) = 'Unit of datavalues:'
      line(align:)  = units
      call logwrit( line )

      if( plotvar(DUNIT).eq.JANSKY ) units =
     *            plotpar(YLABP)( index(plotpar(YLABP),'[')+1 :
     *                            index(plotpar(YLABP),']')-1   )
      if( plotvar(DUNIT).eq.KELVIN ) units = 'K'
      line(offset:) = 'Unit of ' // idstr // ':'
      line(align:)  = units
      call logwrit( line )

      if( beaminfo(BEAMX).gt.0.d0 .and.
     *    (plotvar(DUNIT).eq.JANSKY.or.plotvar(DUNIT).eq.KELVIN) ) then
         n(1) = nfigd( beaminfo(BEAMX) ) + 3
         n(2) = nfigd( beaminfo(BEAMY) ) + 3
         line(offset:) = 'Beam size:'
         write( line(align:), rtfmt( 'f<>.2,''" x '',f<>.2,''"''', n,2))
     *   beaminfo(BEAMX), beaminfo(BEAMY)
         call logwrit( line )
      endif

      if( plotvar(DUNIT).eq.JANSKY ) then
         n(1) = nfigd( beaminfo(SUMBM) ) + 4
         write( line(offset:), rtfmt(
     *   ' ''Sum of beam in equivalent region: '',f<>.3 ', n, 1 ) )
     *   beaminfo(SUMBM)
         call logwrit( line )
      endif
      if( plotvar(DUNIT).eq.KELVIN ) then
         n(1) = nfigd( beaminfo(KPERJY) ) + 3
         write( line(offset:), rtfmt(
     *   '''Conversion of Jy/beam to K: '',f<>.2,'' K/(Jy/beam)''',n,1))
     *   beaminfo(KPERJY)
         call logwrit( line )
      endif

      typ = cubetype(dim)
      line(offset:) = 'Axes of ' // typ(:len1(typ)) // 's :'
      do i  = 1, dim
         line( len1(line)+2 : ) = ctype(i)
         line( len1(line)+1 : ) = ','
      enddo
      call logwrit( line(:len1(line)-1) )

      return
      end


************************************************************************
************************************************************************
************************************************************************

c Loop over all selected data and calculate the statistics.
c These are found for different 'levels'. level 1 corresponds to the
c statistics of the selected averaging-axes. level 2 combines these
c statistics for a subcube with one higher dimension, etc.

      subroutine stats( tinp,naxis,dim, corners, boxes, cut, counts,
     *                  beaminfo, axlabel, device )

      integer          tinp
      integer          naxis, dim
      integer          corners(*), boxes(*)
      real             cut(*)
      integer          counts(0:*)
      double precision beaminfo(*)
      character*(*)    axlabel(*)
      character*(*)    device
      include          'imstat.h'
      integer          MAXRUNS
      parameter(MAXRUNS=3*MAXDIM)
c
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
      integer          npoints(MAXNAX)
      double precision maxval(MAXNAX), minval(MAXNAX)
      double precision sum(MAXNAX), sumpbc(MAXNAX), sumsq(MAXNAX)


      nlevels = naxis - abs(dim) + 1
      do i = 1, MAXNAX
         coo(i) = 1
      enddo

      doplot = device .ne. ' '
c Open the plot device
      if( doplot ) then
         if( pgbeg( 0,device,1,1 ) .ne. 1 ) then
            call pgldev
            call bug( 'f', 'Error opening plot device' )
         endif
      endif

c loop over all subcubes for which statistics are to be calculated.
      do subcube = 1, counts(nlevels)

         call xyzs2c( tinp, subcube, coo )

         if( abs(dim).eq.2 )
     *     call boxruns(  naxis,coo,'r',boxes,runs,MAXRUNS,nruns,
     *                    corners(1),corners(2),corners(3),corners(4) )

c if init(i)=.true., the statistics for this level must be reinitialized.
         init(1) = .true.
         npoints(1) = 0

c Read a profile or a plane
c Unless dim was -2, in which case a plane is read profile by profile
         if(dim.gt. 0) nloop = 1
         if(dim.eq.-2) nloop = counts(naxis+2)
         if(dim.eq. 1) call xyzprfrd( tinp,subcube,data,mask,counts(0) )
         if(dim.eq. 2) call xyzplnrd( tinp,subcube,data,mask,counts(0) )
         do iloop = 1, nloop
         if(dim.eq.-2) call xyzprfrd( tinp,(subcube-1)*nloop+iloop,
     *                                data,mask,counts(0)*nloop )

         do i = 1, counts(0)
c if datapoint falls within limits as defined by cutoff and masking, use it
c          print*,i,data(i),mask(i)
           if( inbox(dim,i.eq.1.and.iloop.eq.1,
     *		data(i),mask(i),runs,corners,cut) ) then
c              print*,'    used'
c convert to Kelvin if requested.
               if( plotvar(DUNIT).eq.KELVIN )
     *            data(i) = data(i) * real(beaminfo(KPERJY))
               if( init(1)  ) then
                  npoints(1) = 0
                  maxval(1)  = data(i)
                  minval(1)  = data(i)
                  sum(1)     = 0.d0
                  sumpbc(1)  = 0.d0
                  sumsq(1)   = 0.d0
                  init(1)    = .false.
               endif
               npoints(1) = npoints(1) + 1
               v          = dble( data(i) )
               maxval(1)  = max( maxval(1), v )
               minval(1)  = min( minval(1), v )
               sum(1)     = sum(1)    + v
c              sumpbc(1)  = sumpbc(1) + v*pbccorr(i,coo(1))
               sumsq(1)   = sumsq(1)  + v*v
            endif
         enddo
         enddo
c after looping over all pixels of a selected subcube, add the results
c to the statistics of all higher levels. These are reinitialized at
c appropriate times, namely if all subcubes from one level lower have
c been handled.
         if( nlevels.ge.2 ) then
         do level = 2, nlevels
            init(level) = mod( subcube-1, counts(level) ) .eq. 0
            if( .not.init(1) ) then
               if( init(level) ) then
                   npoints(level) = 0
                   maxval(level)  = maxval(1)
                   minval(level)  = minval(1)
                   sum(level)     = 0.d0
                   sumpbc(level)  = 0.d0
                   sumsq(level)   = 0.d0
                   init(level)    = .false.
                endif
                npoints(level) = npoints(level) + npoints(1)
                maxval(level)  = max( maxval(level), maxval(1) )
                minval(level)  = min( minval(level), minval(1) )
                sum(level)     = sum(level)    + sum(1)
                sumpbc(level)  = sumpbc(level) + sumpbc(1)
                sumsq(level)   = sumsq(level)  + sumsq(1)
            endif
         enddo
         endif

c After treating each selected subcube, write out the results.
         call results( subcube, tinp, naxis,abs(dim), counts,
     *                 axlabel, doplot,
     *                 npoints,maxval,minval,
     *                 sum,sumpbc,beaminfo(SUMBM),sumsq )
      enddo
      if(doplot) call pgend

      return
      end


************************************************************************

c Routine controlling when results are written, also doing a few
c on-the-spot conversions.

      subroutine results( subcube, tinp, naxis,dim, counts,
     *                    axlabel, doplot,
     *                    npoints,maxval,minval,sum,sumpbc,sumap,sumsq )

      integer          subcube
      integer          tinp, naxis, dim
      integer          counts(0:*)
      character*(*)    axlabel(*)
      logical          doplot
      integer          npoints(*)
      double precision maxval(*), minval(*)
      double precision sum(*), sumpbc(*), sumap, sumsq(*)
      include          'imstat.h'


      integer          nlevels, level
      logical          dotail
      integer          coo(     MAXNAX )
      double precision coords(  MAXNAX )
      character*12     cvalues( MAXNAX )


      nlevels = naxis - dim + 1

c Loop over all levels
      do level = 1, nlevels

c If all data for a level were treated, save the results
         if( mod(subcube,counts(level)) .eq. 0 ) then

c Check if output must be flushed (at last subcube of level 1, and for
c each higher-level subcube
            dotail = mod( subcube, counts(2) ) .eq. 0

c Convert the subcube number to pixels numbers (coo) and then to real
c coordinates (coords) and string-encoded coordinates (cvalues).
            if( level.lt.nlevels ) then
               call xyzs2c( tinp, subcube, coo )
               call getcoo( axlabel, nlevels, coo, coords, cvalues )
            endif

c Add an entry to the table
            if( dotail .or. level.eq.1 )
     *      call tabentry( coo(level),coords(level),cvalues(level),
     *                     npoints(level),maxval(level),minval(level),
     *                     sum(level),sumpbc(level),sumap,sumsq(level))

c Make output for a level if all subcubes were done and added to list.
            if( dotail ) then
c If a header must be written, write it.
               if( plotvar(HEAD).eq.1 .and. plotvar(LIST).eq.1 )
     *             call tabhead(
     *                  naxis,dim,nlevels,level,coo,axlabel(level) )
c Treat the arrays for hanning/derivative and print them.
c Make the plot if all subcubes of a level were done.
               call statout( doplot, axlabel, cvalues,
     *                       dim, level, nlevels )
            endif
         endif

      enddo

      return
      end


************************************************************************

c Convert pixel coordinates (coo) to real coordinates (coords) and
c string-encoded coordinates (cvalues).
c For ra and dec axes special conversions are done.

      subroutine getcoo( axlabel, nlevels, coo, coords, cvalues )

      character*(*)    axlabel(*)
      integer          nlevels
      integer          coo(*)
      double precision coords(*)
      character*(*)    cvalues(*)
      include          'imstat.h'

      integer          len1
      integer          i, j
      character*24     radec

      do i = 1, nlevels - 1
         coords(i)  = crval(i) + (coo(i)-crpix(i))*cdelt(i)
         cvalues(i) = ' '
         if(     axlabel(i)(:4).eq.'R.A.' ) then
             coords(i)  = 180./dpi * coords(i)
             call deghms( coords(i), 0.d0, radec )
             j = len1( radec(1:12) )
             cvalues(i)(13-j:12) = radec(1:j)
             coords(i) = coords(i) * 3600. / 15.
         elseif( axlabel(i)(:4).eq.'Decl' ) then
             coords(i)  = 180./dpi * coords(i)
             call deghms( 0.d0, coords(i), radec )
             j = len1( radec(13:24 ) )
             cvalues(i)(13-j:12) = radec(13:12+j)
             coords(i) = coords(i) * 3600.
         else
             write( cvalues(i), '( f10.1, 2x )' ) coords(i)
         endif
      enddo

      return
      end


************************************************************************

c Write a table header, giving the coordinate value of all higher axes,
c and a nice string for the x-axis.

      subroutine tabhead( naxis,dim,nlevels,level, coo,axlabel )

      integer            naxis, dim, nlevels, level
      integer            coo(*)
      character*(*)      axlabel
      include            'imstat.h'

      integer            i, len1
      character*80       line
      character*2        itoaf
      character*9        typ, cubetype
      character*256      rtfmt
      integer            n(4), nn
      external rtfmt

      call logwrit( ' ' )

      if( level.lt.nlevels ) then

         do i = naxis, dim+level, -1
            line                  = 'Axis '
            line( len1(line)+2: ) = itoaf(i)
            line( len1(line)+2: ) = '(' // ctype(i)
            line( len1(line)+1: ) = ')'
            if(i.gt.dim+level) line(len1(line)+1:) = ':'
            if(i.gt.dim+level) line(20:)           = itoaf(coo(i-dim))
            call logwrit( line )
         enddo

         typ  = cubetype( min(dim+level-1,4) )
         n(1) = ( len( typ ) - len1( typ )  ) / 2
         n(2) = len( typ ) - n(1)
         n(3) = ( len( axlabel ) - len1( axlabel ) + 1 ) / 2
         n(4) = len( axlabel ) - n(3)

      elseif ( level.eq.nlevels ) then

         typ  = 'Total'
         axlabel = ' '
         n(1) = 1
         n(2) = len( typ ) - 1
         n(3) = len( axlabel ) - 1
         n(4) = 1

      endif

      if( plotvar(DUNIT).eq.ORIG .or. plotvar(DUNIT).eq.KELVIN ) then
         write( line, rtfmt( '<>x,a<>, <>x,a<>, 
     *                        ''   Sum      Mean    '',
     *                        ''  rms     Maximum   Minimum  '',
     *                        ''  Npoints''', n, nn )
     *        ) typ, axlabel
         if( NAME.eq.'IMSPEC' ) line( index(line,'rms') : ) = 'Npoints'
      else
         write( line, rtfmt( '<>x,a<>, <>x,a<>,
     *                        ''   Flux     PBC Flux'',
     *                        ''  Npoints''', n, nn )
     *        ) typ, axlabel
      endif
      call logwrit( line )

      return
      end


************************************************************************

c First convert from the raw statistics to the useful statistics.
c Then add an entry to the arrays later to be used for plotting and
c printing.

      subroutine tabentry( coo, coord, cvalue,
     *                    npoints,maxval,minval,sum,sumpbc,sumap,sumsq)

      integer          coo
      double precision coord
      character*(*)    cvalue
      integer          npoints
      double precision maxval, minval, sum, sumpbc, sumap, sumsq
      include          'imstat.h'

      double precision rms, calcrms, stats(NSTATS)
      logical          ok

c Convert from raw to useful statistics
      rms = calcrms( sum, sumsq, npoints, ok )
      if( plotvar(DUNIT).eq.ORIG .or. plotvar(DUNIT).eq.KELVIN ) then
         stats(1) = sum
         if(npoints.ne.0) stats(2) = sum / npoints
         if(npoints.eq.0) stats(2) = 0.
         stats(3) = rms
         stats(4) = maxval
         stats(5) = minval
         stats(6) = npoints
      else
         stats(1) = sum    / sumap
         stats(2) = sumpbc / sumap
         stats(3) = npoints
      endif

c Add datapoints to plotarrays
      if( .not.ok ) coord = MAGICVAL
      call statsav( coo, coord, cvalue, stats )

      return
      end


************************************************************************

      subroutine statout( doplot, axlabel,cvalues, dim,level,nlevels )

      logical          doplot
      character*(*)    axlabel(*), cvalues(*)
      integer          dim, level, nlevels
      include          'imstat.h'

      integer          coo
      double precision coord
      character*(*)    cvalue
      double precision stats(*)

      real             iarr(MAXCHAN), xarr(MAXCHAN)
      character*12     carr(MAXCHAN)
      real             statarr(NSTATS,MAXCHAN), yarr(MAXCHAN)
      integer          n

      integer          i, plt, matchnr
      logical          first
      save             first, iarr, xarr, carr, statarr, n
      data             first / .true. /

      call assertl( n.ne.0, 'All datapoints are masked' )

c Do possible smoothing and derivative-taking on the data,
      plt = matchnr('sum',plotopts)
      do i = 1, n
         yarr(i) = statarr(plt,i)
      enddo
      call smooth( yarr, n, plotvar(DOSMOOTH), plotvar(SMOWID) )
      call differ( xarr, yarr, n, plotvar(DERIV) )
      do i = 1, n
         statarr(plt,i) = yarr(i)
      enddo
      plt = matchnr('mean',plotopts)
      do i = 1, n
         yarr(i) = statarr(plt,i)
      enddo
      call smooth( yarr, n, plotvar(DOSMOOTH), plotvar(SMOWID) )
      call differ( xarr, yarr, n, plotvar(DERIV) )
      do i = 1, n
         statarr(plt,i) = yarr(i)
      enddo

c Write listed output
      call wrnums( iarr,xarr,carr, statarr, n, dim,level,nlevels )

c Copy array to be plotted to array yarr and make plot
      if( plotvar(SEL).eq.matchnr('flux',   plotopts) ) plt=1
      if( plotvar(SEL).eq.matchnr('pbcflux',plotopts) ) plt=2
      if( plotvar(SEL).eq.matchnr('sum',    plotopts) ) plt=1
      if( plotvar(SEL).eq.matchnr('mean',   plotopts) ) plt=2
      if( plotvar(SEL).eq.matchnr('rms',    plotopts) ) plt=3
      if( plotvar(SEL).eq.matchnr('maximum',plotopts) ) plt=4
      if( plotvar(SEL).eq.matchnr('minimum',plotopts) ) plt=5
      do i = 1, n
         yarr(i) = statarr(plt,i)
      enddo
      if( doplot .and. level.eq.1 )
     *call plotstat( iarr, xarr, yarr, n, axlabel,cvalues,nlevels )

      first = .true.

      return


      entry statsav( coo, coord, cvalue, stats )
      if( first ) n = 0
      first = .false.
      n = n + 1
      iarr(n) = coo
      xarr(n) = coord
      carr(n) = cvalue
      do plt = 1, NSTATS
         statarr(plt,n) = stats(plt)
      enddo
      return
      end
      


************************************************************************

      subroutine wrnums( iarr, xarr, carr, statarr, n,
     *                   dim, level, nlevels )

      include          'imstat.h'
      real             iarr(*), xarr(*)
      character*(*)    carr(*)
      real             statarr(NSTATS,*)
      integer          n
      integer          dim, level, nlevels

      integer          nstat
      character*80     line, temp
      character*17     fmt
      character*9      typ, cubetype
      character*5      itoaf
      integer          i, j, len1

      nstat = 3
      if( NAME.eq.'IMSTAT' ) nstat = 6

      do i = 1, n
         if( xarr(i).ne.MAGICVAL ) then
c Construct the output line for the typed list
            line = ' '
            if( level.lt.nlevels )
     *          write( line, '( i6,1x, a )' ) nint(iarr(i)), carr(i)
c 13 is really len(axlabel)+1, but axlabel is an unknown variable here
c and it would be messy to transfer just to get the length of it.
            if(plotvar(EFMT).eq.1) then
	      write( fmt, '( ''( '',i1,''(1pe10.3),i8 )'' )' ) nstat-1
            else
              write( fmt, '( ''( '',i1,''(1pg10.3),i8 )'' )' ) nstat-1
            endif
            write( temp, fmt=fmt )
     *             ( statarr(j,i),j=1,nstat-1 ), nint(statarr(nstat,i))
c           The following is workaround HP compiler bug
            line( len(typ)+13 : ) = temp
         else
            typ  =  cubetype( min(dim+level-1,4) )
            line = 'All points masked for '
     *             // typ(:len1(typ)) // ' ' // itoaf(nint(iarr(i)))
         endif
         if( plotvar(LIST).eq.1 ) call logwrit( line )
      enddo

      return
      end


************************************************************************

c Execute the plot:
c open the plotfile, and make the axes (with the lower x-axis
c in appropriate units and the upper in channels). Plot the points
c with the selected plotstyle and then add the header/title/ids

      subroutine plotstat( iarr,xarr,yarr,n, axlabel,cvalues,nlevels )

      real             iarr(*), xarr(*), yarr(*)
      integer          n
      character*(*)    axlabel(*), cvalues(*)
      integer          nlevels
      include          'imstat.h'

      real             imin, imax, xmax, xmin, ymax, ymin
      character*40     pginfo
      integer          i

c find min and max for plot
      call mnmx(   iarr,xarr,yarr,n, imin,imax, xmin,xmax, ymin,ymax )

      call pgpage
      call pgvstd
      call pgqinf( 'hardcopy',  pginfo, i )
      call pgscf(  index( 'NY', pginfo(:1) ) )

      if( imin.ne.imax .and. plotvar(HEAD).eq.1 ) then
         call pgswin( imin, imax, ymin, ymax )
         call pgbox( 'CMST', 0.0, 0, ' ', 0.0, 0 )
      endif
      call pgswin( xmin, xmax, ymin, ymax )
      if( .not.( imin.ne.imax .and. plotvar(HEAD).eq.1 )  )then
         call pgbox( 'CST',  0.0, 0, ' ', 0.0, 0 )
      endif

      if(     axlabel(1)(:4).eq.'R.A.' ) then
      call pgtbox( 'BNSTHYZ', 0.0, 0, 'BCNST', 0.0, 0 )
      elseif( axlabel(1)(:4).eq.'Decl' ) then
      call pgtbox( 'BNSTDYZ', 0.0, 0, 'BCNST', 0.0, 0 )
      else
      call pgtbox( 'BNST',    0.0, 0, 'BCNST', 0.0, 0 )
      endif

      call pgpts( xarr, yarr, n, plotvar(STYLE), ymin )
      call pgident
      if( plotvar(HEAD).eq.1 ) then
         do i = 3, nlevels
            call pgmtxt( 'T', -(i-2)-YOFF, XOFF, LEFT,  axlabel(i-1) )
            call pgmtxt( 'T', -(i-2)-YOFF, COFF, RIGHT, cvalues(i-1) )
         enddo
      endif
      call output(' ')
      return
      end


c Find the plotrange, extending the datarange 8% (EXTEND %) at both sides
c and substituting user-given values if these were given.

      subroutine mnmx( iarr,xarr,yarr,n, imin,imax,xmin,xmax,ymin,ymax )

      real          iarr(*), xarr(*), yarr(*)
      integer       n
      real          imin, imax, xmin, xmax, ymin, ymax
      include       'imstat.h'

      integer       indmin, indmax
      integer       ismin, ismax
      real          EXTEND
      parameter     ( EXTEND = 0.08 )
      character*40  warning1, warning2
      data          warning1 / 'Wanted minimum above maximum found' /
      data          warning2 / 'Wanted maximum below minumum found' /
 
      indmin = ismin( n, xarr, 1 )
      indmax = ismax( n, xarr, 1 )
      if( plotrnge(XTITLE).eq.MAGICVAL ) plotrnge(XTITLE) = xarr(indmin)
      imin = iarr(indmin) - EXTEND * ( iarr(indmax) - iarr(indmin) )
      imax = iarr(indmax) + EXTEND * ( iarr(indmax) - iarr(indmin) )
      xmin = xarr(indmin) - EXTEND * ( xarr(indmax) - xarr(indmin) )
      xmax = xarr(indmax) + EXTEND * ( xarr(indmax) - xarr(indmin) )
      call assertl( xmin.ne.xmax, 'X-range of plot is 0' )
      indmin = ismin( n, yarr, 1 )
      indmax = ismax( n, yarr, 1 )
      if( plotrnge(YTITLE).eq.MAGICVAL ) plotrnge(YTITLE) = yarr(indmax)
      ymin = yarr(indmin) - EXTEND * ( yarr(indmax) - yarr(indmin) )
      ymax = yarr(indmax) + EXTEND * ( yarr(indmax) - yarr(indmin) )
      call assertl( ymin.ne.ymax, 'Y-range of plot is 0' )

      if( plotrnge(FLXL).eq.1 .or. plotrnge(FLXU).eq.1 ) imin = 0.
      if( plotrnge(FLXL).eq.1 .or. plotrnge(FLXU).eq.1 ) imax = 0.
      if( plotrnge(FLXL).eq.1 ) then
         call assertl( plotrnge(XLOW).lt.xmax, warning1 )
         xmin = plotrnge(XLOW)
      endif
      if( plotrnge(FLXU).eq.1 ) then
         call assertl( plotrnge(XUPP).gt.xmin, warning2 )
         xmax = plotrnge(XUPP)
      endif
      if( plotrnge(FLYL).eq.1 ) then
         call assertl( plotrnge(YLOW).lt.ymax, warning1 )
         ymin = plotrnge(YLOW)
      endif
      if( plotrnge(FLYU).eq.1 ) then
         call assertl( plotrnge(YUPP).gt.ymin, warning2 )
         ymax = plotrnge(YUPP)
      endif

      return
      end


************************************************************************
************************************************************************
************************************************************************

c Hanning or boxcar smooth the data

      subroutine smooth( yarr, n, mode, width )
      real             yarr(*)
      integer          n, mode, width
      integer          MAXWIDTH
      parameter        ( MAXWIDTH = 7 )
      real             coeffs( MAXWIDTH*2+1 ), work( MAXWIDTH*2+1 )
      integer          HANNING, BOXCAR
      parameter        ( HANNING=1, BOXCAR=2 )
      if(     mode.eq.HANNING ) then
         call hcoeffs(  width, coeffs )
         call hannsm(   width, coeffs, n, yarr, work )
      elseif( mode.eq.BOXCAR ) then
         call bcoeffs(  width, coeffs )
         call boxcarsm( width, coeffs, n, yarr, work )
      endif
      return
      end


***********************************************************************

c Take a one-sided or two-sided derivative

      subroutine differ( xarr, yarr, n, mode )
      real          xarr(*), yarr(*)
      integer       n, mode
      integer       i
      if(     mode.eq.1 ) then
         do i = 2, n
            yarr(i-1) = ( yarr(i)-yarr(i-1)   )/( xarr(i)-xarr(i-1)   )
         enddo
      elseif( mode.eq.2 ) then      
         do i = 2, n-1
            yarr(i-1) = ( yarr(i+1)-yarr(i-1) )/( xarr(i+1)-xarr(i-1) )
         enddo
         yarr(n-1) = yarr(n-2)
      endif
      if( mode.gt.0 ) then
         do i = n, 2, -1
            yarr(i) = yarr(i-1)
         enddo
         yarr(1) = yarr(2)
      endif
      return
      end

************************************************************************

c Return a normal name to identify the type of the cube.
c Do it via this function to avoid having a data statement in the
c include file.

      character*(*) function cubetype( arg )
      integer arg
      character*9 types(4)
      data types / 'profile', 'plane', 'cube', 'hypercube' /
      cubetype = types(arg)
      return
      end      


************************************************************************

c Test if data are within unmasked, above the cut and inside the region

      logical function inbox( dim, init, data, mask, runs,corners, cut )
      integer dim
      real    data
      logical mask, init
      integer runs(3,*)
      integer corners(*)
      real    cut(*)

      integer runpnt, xlen, x,y
      save    runpnt, xlen, x,y
      logical unmasked

      if( init ) then
         runpnt = 1
         x      = 0
         y      = 1
         xlen   = corners(2) - corners(1) + 1
      endif

      if(      cut(2).eq.0. ) then
         unmasked = mask
      else if( cut(2).eq.1. ) then
         unmasked = mask .and.     data .ge.cut(1)
      else if( cut(2).eq.2. ) then
         unmasked = mask .and. abs(data).ge.cut(1)
      endif
c     if( mask.eq..true. ) print*,'    mask'

      if( abs(dim).eq.2 ) then
         x = x + 1
         if( x.gt.xlen ) then
            x = 1
            y = y + 1
         endif
         if( runs(1,runpnt).eq.y ) then
            if( unmasked ) then
               inbox = runs(2,runpnt).le.x .and. x.le.runs(3,runpnt)
            else
               inbox = .false.
            endif
            if( x.eq.runs(3,runpnt) ) runpnt = runpnt + 1
         else
            inbox = .false.
         endif
      else
         inbox = unmasked
      endif

      return
      end


***********************************************************************

c Calculate the rms from the sum and sumsquared.

      double precision function calcrms( sum, sumsq, npoints, ok )

      double precision sum, sumsq
      integer          npoints
      logical          ok
      double precision rms

      if(     npoints.ge.2 ) then
         rms = ( sumsq - sum**2/dble(npoints) ) / dble(npoints-1) 
         if( rms.ge.0.d0 ) then
            rms = sqrt(rms)
            ok = .true.
         else
            call bug( 'w', 'Rms^2 is negative!! Square root not taken' )
            ok = .false.
         endif
      elseif( npoints.eq.1 ) then
         rms = 0.d0
         ok = .true.
      elseif( npoints.eq.0 ) then
         ok = .false.
      endif
      calcrms = rms
      return
      end


************************************************************************

c Plot the points in three possible different styles

      subroutine pgpts( xarr, yarr, n, plotstyl, ymin )
      real       xarr(*), yarr(*)
      integer    n, plotstyl
      real       ymin
      if(plotstyl.eq.1) call pgline( n,xarr,yarr )
      if(plotstyl.eq.2) call pgbin(  n,xarr,yarr,.true. )
      if(plotstyl.eq.3) call pgcbin( n,xarr,yarr,.true.,ymin )
      return
      end


c Connect points and make bars down to the x-axis

      subroutine pgcbin( n, xarr, yarr, center, ymin )

      integer      n
      real         xarr(*), yarr(*)
      logical      center
      real         ymin

      real         xpts(4), ypts(4)
      real         dx, xoff, x
      integer      i
      do i = 1, n
         if( i.lt.n ) dx = xarr(i+1) - xarr(i)
         if( i.eq.n ) dx = xarr(n) - xarr(n-1)
         if(      center ) xoff = dx / 2.
         if( .not.center ) xoff = 0.
         x       = xarr(i) - xoff
         xpts(1) = x
         ypts(1) = ymin
         xpts(2) = x
         ypts(2) = yarr(i)
         xpts(3) = x + dx
         ypts(3) = yarr(i)
         xpts(4) = x + dx
         ypts(4) = ymin
         call pgline( 4, xpts, ypts )
      enddo
      return
      end


c Write out an identifying message above the plot.

      subroutine pgident
      include          'imstat.h'
      character*40     pginfo, ident
      integer          i
      call pgsch( 1. )
      call pglab( plotpar(XLABP), plotpar(YLABP), ' ' )
      if( plotpar(TITLE).ne.' ' ) then
         call pgtext( plotrnge(XTITLE),plotrnge(YTITLE),plotpar(TITLE) )
      endif
      if( plotvar(HEAD).eq.1 ) then
         call pgsch( SC )
         call pgqinf(  'now', pginfo, i )
         ident = NAME // ' ' // pginfo
         call pgmtxt( 'T', BASE+2+YOFF, XOFF, LEFT, ident          )
         call pgmtxt( 'T', BASE+1+YOFF, XOFF, LEFT, plotpar(INFOP) )
         call pgmtxt( 'T', BASE  +YOFF, XOFF, LEFT, plotpar(BOXP)  )
      endif
      return
      end


