c= imhist - plot a histogram of the data
c& bpw
c: map analysis
c+
c  Imhist makes histograms of image data. The output can be written to
c  the terminal, a log file, or a plot.
c  A gaussian curve with the same mean, rms and integral is drawn in
c  the histogram (this can be turned off using the 'nogauss' option).
c  The plotheader can be suppressed by using options=noheader. An
c  alternative title can be put on the plot by options=title. A useful
c  combination is 'options=noh,t,title', to get only the string 'title',
c  instead of the full header.
c
c< in
c< region
c  For the moment imhist only recognizes rectangular boxes.
c
c@ options
c  These options allow to control the plot. They may be abbreviated to
c  uniqueness. Several options may be combined, in random order.
c  Possible options are (# means: give a number):
c
c   'nbin,#'      Select the number of bins of the histogram, default 25
c   'binsize,#'   Select a fixed size for the bins
c   'cumulative'  Make the histogram cumulative
c   'logarithmic' Make the y-scale of the histogram logarithmic
c   'nogauss'     Do not plot the smooth gaussian curve
c
c   'noheader'    Do not write the header information, just the numbers,
c                 producing an ASCII file for a plotting program
c   'nolist'      Do not write the statistics to the screen/logfile
c
c   'ymax,#'      The plot will be cut off at this y-value, default
c                 is 1.25 times maximum histogram value
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
c  the histogram. Give one real value, which may be followed by the
c  string ',abs' to get a cutoff in the absolute value of the datavalues.
c  Default is no cutoff.
c
c@ xrange
c  This gives two numbers, being the acceptable datavalues to include
c  in the statistics and histogram. 
c  
c  This is, in effect, the opposite of cutoff. 
c
c  Specifying both cutoff and xrange is possible. In this case,
c  the two constraints are 'and-ed' together to determine which
c  data are to be included. The xrange key alone, however, defines the 
c  xrange of the plot axis.
c
c< device
c@ log
c  If specified, output is written to the file given by log= instead
c  of to the terminal.
c--
c
c
c   History:
c
c    20jul91  bpw  Original version
c    29jul91  bpw  Corrected bug for 2-d datasets
c    06aug91  bpw  Corrected channel scale, added flux calculation,
c                  included imspect, created imhist.h
c    19sep91  bpw  Minor overhaul to make splitting easier and to
c                  finish off some leftover things
c    24sep91  bpw  Split off from imstahis.for
c    21jan92  bpw  Add writing position of min and max
c    27mar92  bpw  Changed assert into assertl
c    10sep92  bpw  Noted that coordinates for min and max were switched
c    15jan93  bpw  More decimals in writing results
c    13mar93  mjs  pgplot subr names have less than 7 chars.
c    21may93  mjs  Call pgask(.FALSE.) to override any pgplot default.
c    03jun93   jm  Removed pgask(.FALSE.) call and also added printout
c                  of valid devices if input device name is incorrect.
c    15jun93  bpw  Did paging with pgcurs to get back preferred situation before
c                  pgplot changes, where clicking left resulted in paging
c    30sep93   jm  Corrected pgplot calling sequence.
c    12nov97  rjs  Added 's' flag to boxset call.
c    14dec00  dpr  added xrange
c
c------------------------------------------------------------------------

      program imhist

      character*40     version
      parameter        ( version = 'version 2.1 14-Dec-2000' )

      integer          tinp
      real             cut(2)
      real             xrange(2)
      integer          naxis, npixels
      character*80     device

      call output( 'IMHIST: ' // version )
      call inputs( tinp, cut, xrange, npixels, naxis, device )
      call histo(  tinp, cut, xrange, npixels, naxis, device )
      call xyzclose( tinp )
      call logclose

      end


************************************************************************


      subroutine inputs( tinp, cut, xrange, npixels, naxis, device )
      implicit none

      integer            tinp
      real               cut(2)
      real               xrange(2)
      integer            npixels, naxis
      character*(*)      device
      include            'imhist.h'

      integer            MAXBOXES
      parameter          ( MAXBOXES = 1024 )

      character*1024     file
      integer            axlen( MAXNAX )
      integer            boxes( MAXBOXES )
      integer            blc(MAXNAX), trc(MAXNAX)
      integer            viraxlen( MAXNAX ), vircsz( MAXNAX )

      logical            rangeisset

      call keyini

      call keyf( 'in', file, ' ' )
      naxis = MAXNAX
      call xyzopen( tinp, file, 'old', naxis, axlen )

      call boxinput( 'region', file, boxes, MAXBOXES )
      call boxset(   boxes, naxis, axlen, 's' )
      call boxinfo(  boxes, naxis, blc, trc )

      call optinp

      call cutinp( cut )

c  input xrange, defaulting to 0,0, which implies unset!!
      xrange(1)=0.
      xrange(2)=0.
      call keyr('xrange',xrange(1),0.)
      call keyr('xrange',xrange(2),xrange(1)*-1.)

c  if the xrange is set, set the plot range too
c  this overrides values set using options (which is depricated)
      if ( rangeisset(xrange) ) then
        call setrange(xrange)
      endif

      call xyzsetup( tinp, ' ', blc, trc, viraxlen, vircsz )
      npixels = vircsz(naxis)

      call outdev( device )

      call keyfin

      call labset( tinp, file, blc, trc, naxis )

      call header( tinp, file )

      return
      end


************************************************************************


      subroutine cutinp( cut )

      real          cut(2)

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


      subroutine optinp

      include       'imhist.h'

      character*20  option
      logical       match
      integer       i
      real          v
      integer       matchnr
      integer       len1
      character*80  line

      logical       needval( NHISTP )
      real          histdef( NHISTP )
      data          needval  / .true.,.true.,.false.,.false.,.false. /
      data          histdef  / 25., 1., 0.,0.,0. /
      do i = 1, NHISTP
         histpar(FLAG,i)  = 0.
         histpar(VALUE,i) = histdef(i)
      enddo
      plotvar( SEL)     = -1
      plotvar( HEAD )   =  1
      plotvar( LIST )   =  1
      plotvar( HANWD )  =  1
      plotvar( STYLE )  = -1
      plotpar( TITLE )  = ' '
      plotrnge( FLXL )  = 0.
      plotrnge( FLXU )  = 0.
      plotrnge( FLYL )  = 0.
      plotrnge( FLYU )  = 0.

      call keya( 'options', option, ' ' )
      do while( option.ne. ' ' )
         call lcase( option )

         if( match( option, commonop, i )  ) then
            if(     i.eq.matchnr('style',commonop) ) then
               call keya( 'options', option, ' ' )
               call assertl( match(option,styles,i), 'Illegal style' )
               plotvar(STYLE) = i
            elseif( i.eq.matchnr('noheader',commonop) ) then
               plotvar(HEAD) = 0
            elseif( i.eq.matchnr('nolist',commonop) ) then
               plotvar(LIST) = 0
            elseif( i.eq.matchnr('list',commonop) ) then
               plotvar(LIST) = 1
            elseif( i.eq.matchnr('title',commonop) ) then
               call keya( 'options', plotpar(TITLE),   ' ' )
               call keyr( 'options', plotrnge(XTITLE), MAGICVAL )
               call keyr( 'options', plotrnge(YTITLE), MAGICVAL )
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

         elseif( match( option, plotopts, i )  ) then
            histpar(FLAG,i) = 1.
            if( needval(i) )
     *          call keyr( 'options', histpar(VALUE,i), histdef(i) )
            if( .not.needval(i) ) histpar(VALUE,i) = 1.
            if( i.eq.NBINP ) call assertl(
     *          histpar(VALUE,NBINP).gt.0., 'Bad number of bins' )
            if( i.eq.BINSP ) call assertl(
     *          histpar(VALUE,BINSP).gt.0., 'Bin size must be >0' )

         else
            line = 'Illegal or ambiguous option ' //
     *             option(:len1(option)) // ' ignored'
            call bug( 'w', line )
            
         endif
         call keya( 'options', option, ' ' )
      enddo

      if(plotvar(STYLE).eq.-1) plotvar(STYLE)=matchnr('histo',styles)

      return
      end



 
***********************************************************************
c
c  Set the plotting global pltrange to reflect the values specified
c  by the key xrange
c
c  Dont really know what this does - just copied opint
c
c  dpr 14-12-00
c

      subroutine setrange(xrange)

      implicit none
      real     xrange(*)

      include       'imhist.h'
c  (I know, but I didn't do it)

      plotrnge(FLXL) = 1.
      plotrnge(XLOW) = xrange(1)
      
      plotrnge(FLXU) = 1.
      plotrnge(XUPP) = xrange(2)

      end


************************************************************************


      subroutine outdev( device )

      character*(*)  device
      include        'imhist.h'

      character*1024 logfile

      call keya( 'device', device,  ' ' )
      if( device.eq.' ' .and. plotvar(LIST).eq.0 ) plotvar(LIST) = 1
      call keya( 'log',    logfile, ' ' )
      call logopen( logfile, ' ' )

      return
      end
      

***********************************************************************


      subroutine labset( tinp,file,blc,trc,naxis )

      integer       tinp
      character*(*) file
      integer       blc(*), trc(*), naxis
      include       'imhist.h'

      integer       len1
      character*40  string
      character*20  units
      real          rdata
      character*10  rtoaf
      integer       i, j
      character*40  bln, tln

      integer       NRECOG
      parameter     ( NRECOG = 4 )
      character*10  runits( NRECOG ), labels( NRECOG )
      data          runits  / 'JY', 'HZ', 'KM/S', 'KMS-1' /
      data          labels / 'Flux','Frequency','Velocity','Velocity' /

      call rdhda( tinp, 'bunit', units, 'Unknown' )
      string = units
      call lcase( string )


      i = 1
      do while( i.le.NRECOG .and.
     *   index( string, runits(i)(:len1(runits(i))) ) .eq. 0  )
         i = i + 1
      enddo
      if( i.le.NRECOG ) string = labels(i)
      if( i.gt.NRECOG ) string = 'Map value'
      write( plotpar(XLABP), '( a, '' ['', a, '']'' )' )
     *       string(:len1(string)), units(:len1(units))

      plotpar(YLABP) = ' '
      if( histpar(FLAG,LOGAR).eq.1. ) plotpar(YLABP) = 'Log'
      if( histpar(FLAG,CUMUL).eq.1. )
     *plotpar(YLABP)(len1(plotpar(YLABP))+2:) = 'Cumulative'
      plotpar(YLABP)(len1(plotpar(YLABP))+2:) = 'Counts'

      call rdhda( tinp, 'object',   string, ' ' )
      call rdhdr( tinp, 'restfreq', rdata,   0. )
      if( string.ne.' ' ) plotpar(INFOP) = 'Source: ' // string
      if( rdata.ne.0. ) then
          plotpar(INFOP)(len1(plotpar(INFOP))+1:)='; '//rtoaf(rdata,1,6)
          plotpar(INFOP)(len1(plotpar(INFOP))+1:)=' GHz'
      endif
      plotpar(INFOP)(len1(plotpar(INFOP))+1:) = '; File: ' // file

      call mitoaf( blc, naxis, bln, i )
      call mitoaf( trc, naxis, tln, j )
      string = 'blc=(' // bln(:i) // '), trc=(' // tln(:j) // ')'
      plotpar(BOXP) = 'Bounding box: ' // string

      return
      end



************************************************************************


      subroutine header( tinp, file )

      integer          tinp
      character*(*)    file
      include          'imhist.h'

      integer          len1
      character*80     line
      character*20     units

      if( plotvar(HEAD).eq.0 ) return

      line = '***** Histogram of image '//file(:len1(file))//' *****'
      call logwrit( line )

      line = '      ' // plotpar(BOXP)
      call logwrit( line )

      call rdhda( tinp, 'bunit', units, 'Unknown' )
      line = '      Unit of datavalues: ' // units
      call logwrit( line )

      return
      end


************************************************************************
************************************************************************
************************************************************************


      subroutine histo( tinp, cut, xrange, npixels, naxis, device )

      integer       tinp
      real          cut(2)
      real          xrange(2)
      integer       npixels, naxis
      character*(*) device
      include       'imhist.h'

      logical       doplot
      integer       pgbeg

      integer       binmax

      integer       HLEN
      parameter     ( HLEN = 1000 )
      real          xvals(0:HLEN+1), hist(0:HLEN+1)
      data          hist / 0., HLEN * 0., 0. /

      doplot = device .ne. ' '
      if( doplot ) then
         if( pgbeg( 0,device,1,1 ) .ne. 1 ) then
            call pgldev
            call bug( 'f', 'Error opening plot device' )
         endif
      endif

      call histset( tinp, cut, xrange, npixels, HLEN, binmax )

      call histmake( tinp, binmax, cut, xrange, npixels, xvals, hist )

      call histout( doplot, naxis, binmax, xvals, hist )

      return
      end


************************************************************************


      subroutine histset( tinp, cut, xrange, npixels, HLEN, binmax )

      integer          tinp
      real             cut(2)
      real             xrange(2)
      integer          npixels
      integer          HLEN, binmax
      include          'imhist.h'

      integer          npoints
      double precision sum, sumsq, calcrms
      real             minval, maxval
      logical          ok
      integer          delimi
      real             r, gausetup

      call histvars( tinp,cut,xrange,npixels, npoints,sum,sumsq,
     *  minval,maxval )

      if( histpar(FLAG,BINSP).eq.0. )
     *histpar(VALUE,BINSP) = 
     *    1.001*( plotrnge(XUPP)-plotrnge(XLOW) ) / histpar(VALUE,NBINP)

      histvar(NPTS)  = npoints
      histvar(MEANP) = sum / npoints
      histvar(RMSP)  = calcrms( sum, sumsq, npoints, ok )

      binmax = int(  ( plotrnge(XUPP)-plotrnge(XLOW) )
     *                 / histpar(VALUE,BINSP)            ) + 1
      call assertl( binmax.le.HLEN, 'Too many bins' )
      binmax = delimi( binmax, 1, HLEN )

      r=gausetup( minval, maxval, histpar(VALUE,BINSP),
     *  histvar(NPTS),histvar(MEANP),histvar(RMSP),histpar(FLAG,CUMUL) )

      return
      end


************************************************************************


      subroutine histvars( tinp, cut, xrange, npixels,
     *                     npoints, sum, sumsq, minval, maxval )

      integer          tinp
      real             cut(2)
      real             xrange(2)
      integer          npixels
      integer          npoints
      double precision sum, sumsq
      real             maxval, minval
      include          'imhist.h'

      integer          i
      real             data
      logical          mask
      logical          unmasked, init

      if( cut(2).gt.0. .and. plotrnge(FLXU).eq.1. )
     *    call assertl( cut(1).lt.plotrnge(XUPP),
     *                  'Cutoff is above histogram maximum!' )

      init = .true.
      do i = 1, npixels
         call xyzpixrd( tinp, i, data, mask )
         if( unmasked( data, mask, cut, xrange ) ) then
            if( init ) then
               npoints = 0
               minval  = data
               maxval  = data
               call xyzs2c( tinp, i, posmin )
               call xyzs2c( tinp, i, posmax )
               sum     = 0.d0
               sumsq   = 0.d0
               init    = .false.
            endif
            npoints = npoints + 1
            if( data.lt.minval ) then
               minval = data
               call xyzs2c( tinp, i, posmin )
            endif
            if( data.gt.maxval ) then
               maxval = data
               call xyzs2c( tinp, i, posmax )
            endif
            sum     = sum   + data
            sumsq   = sumsq + data*data
         endif
      enddo
      call assertl( .not.init,   'All datapoints are masked' )
      call assertl( npoints.gt.1,'Histogramming 1 datapoint will fail' )

      histvar(MINV) = minval
      histvar(MAXV) = maxval
      if( plotrnge(FLXL).eq.0. ) plotrnge(XLOW) = minval
      if( plotrnge(FLXU).eq.0. ) plotrnge(XUPP) = maxval
      if( cut(2).ne.0. ) call assertl( cut(1).lt.plotrnge(XUPP),
     *                   'Cutoff is above maximum in region!' )

      return
      end


***********************************************************************


      subroutine histmake( tinp, binmax, cut, xrange, npixels, xvals,
     *   hist )

      integer          tinp
      integer          binmax
      real             cut(2)
      real             xrange(2)
      integer          npixels
      real             xvals(0:*), hist(0:*)
      include          'imhist.h'

      integer          bin, i
      real             data
      logical          mask, unmasked
      real             rbin
      integer          delimi

      do bin = 0, binmax+1
         xvals(bin) = (bin-1)*histpar(VALUE,BINSP)+plotrnge(XLOW)
      enddo
      xvals(0)        = xvals(0)        + histpar(VALUE,BINSP) / 2.
      xvals(binmax+1) = xvals(binmax+1) + histpar(VALUE,BINSP) / 2.

      do i = 1, npixels
         call xyzpixrd( tinp, i, data, mask )
         if( unmasked( data, mask, cut, xrange ) ) then
            rbin = (data-plotrnge(XLOW)) / histpar(VALUE,BINSP)
            if( rbin.ge.0. ) bin = int(rbin)+1
            if( rbin.lt.0. ) bin = int(rbin)
            bin = delimi( bin, 0, binmax+1 )
            hist( bin ) = hist( bin ) + 1.
         endif
      enddo

      call histmedi( hist, binmax )

      if( histpar(FLAG,CUMUL).eq.1. ) then
         do bin = 2, binmax
            hist(bin) = hist(bin) + hist(bin-1)
         enddo
      endif

      if( histpar(FLAG,LOGAR).eq.1. ) then
         do bin = 0, binmax+1
            if( hist(bin).ne.0. ) hist(bin) = alog10( hist(bin) )
         enddo
      endif

      return
      end


***********************************************************************


      subroutine histmedi( hist, binmax )

      real          hist(0:*)
      integer       binmax
      include       'imhist.h'

      real          sum
      integer       bin

      sum = 0.
      bin = 0
      do while( sum.lt.histvar(NPTS)/2. .and. bin.le.binmax )
         sum = sum + hist(bin)
         bin = bin + 1
      enddo
      histvar(MEDIANP) = (bin-1)*histpar(VALUE,BINSP) + plotrnge(XLOW)

      return
      end


***********************************************************************


      subroutine histout( doplot, naxis, n, xarr, yarr )

      logical       doplot
      integer       naxis
      real          xarr(0:*), yarr(0:*)
      integer       n
      include       'imhist.h'

      real          xmin, xmax, ymin, ymax
      integer       ismax
      real          EXTEND
      parameter     ( EXTEND = 0.05 )

      integer       bin, i
      character*80  line
      integer       astlen
      parameter     ( astlen = 50 )
      character*(astlen) asterisk
      do i = 1, astlen
         asterisk(i:i) = '*'
      enddo

      call histinfo( naxis, xarr(1), xarr(n) )

      xmin = xarr(0)   - EXTEND*( xarr(n+1) - xarr(0) )
      xmax = xarr(n+1) + EXTEND*( xarr(n+1) - xarr(0) )
      ymin = -0.1
      ymax = 1.25 * yarr( ismax(n,yarr(1),1) )
      if( plotrnge(FLYU).eq.1. ) ymax = plotrnge(YUPP)

      if( doplot ) then

         call pgenv( xmin, xmax, ymin, ymax, 0, 0 )
         call pgpts( xarr(1), yarr(1), n, plotvar(STYLE), ymin )
         call undovr( xarr(0),   yarr(0),   ymax )
         call undovr( xarr(n+1), yarr(n+1), ymax )
         if( histpar(FLAG,GAUCRV).eq.0. ) call gaucurve
         call pgident
         call pgend
         call output(' ')

      else

         call logwrit( ' ' )
         call logwrit(  '  Bin    Value          Number' )
         write( line, '( ''       Underflow   '', i8 )' ) int(yarr(0))
         call logwrit( line )
         do bin = 1, n
            i = (  yarr(bin) / ymax  ) * astlen
            if( i.ge.1 ) then
               write( line, '( i5, 4x, 1pg10.3, i8, 1x, a )' )
     *                bin, xarr(bin), int(yarr(bin)), asterisk(:i)
            else
               write( line, '( i5, 4x, 1pg10.3, i8 )' )
     *                bin, xarr(bin), int(yarr(bin))
            endif
            call logwrit( line )
         enddo
         write( line, '(''       Overflow    '', i8 )' ) int(yarr(n+1))
         call logwrit( line )

      endif

      return
      end
      

************************************************************************


      subroutine histinfo( naxis, xmin, xmax )

      integer          naxis
      real             xmin, xmax
      include          'imhist.h'

      integer          i, len1
      character*80     line

      call logwrit( ' ' )
      call logwrit( 'Histogram information' )

      line='# of points         Mean            rms            Median'
      i = len1( line ) + 2
                                       line(i:) = 'between'
      if( histvar(MEDIANP) .le. xmin ) line(i:) = 'below'
      if( histvar(MEDIANP) .ge. xmax ) line(i:) = 'above'
      call logwrit( line )

      write( line, '( i11, 3x, 1pe14.7,1x, 1pe14.7 )' )
     *       int(histvar(NPTS)), histvar(MEANP), histvar(RMSP)

      i = len1( line ) + 5
      if(     histvar(MEDIANP) .le. xmin   .or.
     *        histvar(MEDIANP) .ge. xmax )       then
         write( line(i:), '( 5x, 1pe14.7 )' ) histvar(MEDIANP)
         write( line(i:), '( 5x, 1pe14.7 )' ) histvar(MEDIANP) 
      else
         write( line(i:), '( 1pe14.7, '' and '', 1pe14.7 )' )
     *       histvar(MEDIANP) - histpar(VALUE,BINSP), histvar(MEDIANP)
      endif
      call logwrit( line )

      write( line, '( ''Maximum is '',1pe14.7,'' at ('' )' )
     *                histvar(MAXV)
      call mitoaf( posmax, naxis, line(len1(line)+1:), i )
      line(len1(line)+1:) = ') (absolute coordinates)'
      call logwrit( line )

      write( line, '( ''Minimum is '',1pe14.7,'' at ('' )' )
     *                histvar(MINV)
      call mitoaf( posmin, naxis, line(len1(line)+1:), i )
      line(len1(line)+1:) = ') (absolute coordinates)'
      call logwrit( line )

      return
      end


************************************************************************


      subroutine undovr( x, y, ymax )
      real       x, y, ymax
      if( y.ne.0. ) then
         if( y.lt.ymax ) call pgpt( 1, x,      y,       8 )
         if( y.ge.ymax ) call pgpt( 1, x, 0.95*ymax, 2262 )
      endif
      return
      end


************************************************************************


      subroutine gaucurve

      include       'imhist.h'

      real          xmin, xmax
      external      gauss, loggauss, gaussint, loggint
      real          gauss, loggauss, gaussint, loggint

      xmin = plotrnge(XLOW) + histpar(VALUE,BINSP)/2.
      xmax = plotrnge(XUPP) - histpar(VALUE,BINSP)/2.

      if(     histpar(FLAG,CUMUL).eq.0. ) then
         if(     histpar(FLAG,LOGAR).eq.0. ) then
            call pgfunx( gauss,    1000, xmin, xmax, 1 )
         elseif( histpar(FLAG,LOGAR).eq.1. ) then
            call pgfunx( loggauss, 1000, xmin, xmax, 1 )
         endif
      elseif( histpar(FLAG,CUMUL).eq.1. ) then
         if(     histpar(FLAG,LOGAR).eq.0. ) then
            call pgfunx( gaussint, 1000, xmin, xmax, 1 )
         elseif( histpar(FLAG,LOGAR).eq.1. ) then
            call pgfunx( loggint,  1000, xmin, xmax, 1 )
         endif
      endif

      return
      end




      real function gauss( x )
      real       loggauss, gaussint, loggint, gausetup

      real       x

      real       cum
      real       minval, maxval, binsize
      real       npoints, mean, rms

      real       z, t, erf
      include    'mirconst.h'
      real       gausum
      integer    bin, binmin, binmax
      real       norm, x0, sigma
      save       norm, x0, sigma

      gauss = norm * exp( -(x-x0)**2/(2.*sigma**2) )
      return

      entry loggauss( x )
      loggauss = alog10( norm * exp( -(x-x0)**2/(2.*sigma**2) ) )
      return

      entry gaussint( x )
      z = abs( (x-x0)/sqrt(2.)/sigma )
      t = 1. / ( 1.+0.5*z )
      erf = 1. - t*exp( -z*z -
     *          1.26551223 + t*( 1.00002368 + t*( 0.37409196 +
     *      t*( 0.09678418 + t*(-0.18628806 + t*( 0.27886807 +
     *      t*(-1.13520398 + t*( 1.48851587 + t*(-0.82215223 +
     *      t*( 0.17087277
     *      ))))))))))
      gaussint = norm * (  0.5 + 0.5*erf*sign(1.,(x-x0))  )
      return

      entry loggint( x )
      z = abs( (x-x0)/sqrt(2.)/sigma )
      t = 1. / ( 1.+0.5*z )
      erf = 1. - t*exp( -z*z -
     *          1.26551223 + t*( 1.00002368 + t*( 0.37409196 +
     *      t*( 0.09678418 + t*(-0.18628806 + t*( 0.27886807 +
     *      t*(-1.13520398 + t*( 1.48851587 + t*(-0.82215223 +
     *      t*( 0.17087277
     *      ))))))))))
      loggint = alog10(  norm * (  0.5 + 0.5*erf*sign(1.,(x-x0))  ) )
      return



      entry gausetup( minval,maxval,binsize, npoints,mean,rms, cum )
      gausum = 0.
      binmin = int( (minval-mean) / binsize )
      binmax = int( (maxval-mean) / binsize ) + 1
      do bin = binmin, binmax
         gausum = gausum + exp( -(bin*binsize)**2 / (2.*rms**2) )
      enddo
      if( cum.eq.0. ) norm = npoints / gausum
      if( cum.eq.1. ) norm = npoints
      x0    = mean
      sigma = rms

      return
      end


************************************************************************
************************************************************************
************************************************************************

      logical function unmasked( data, mask, cut, xrange )
      real       data
      logical    mask
      real       cut(2)
      real       xrange(2)

      logical            rangeisset

      unmasked=mask

      if( cut(2).gt.0. ) then
        unmasked =
     *       ( ( cut(2).eq.1. .and.     data .ge.cut(1) ) .or.
     *       ( cut(2).eq.2. .and. abs(data).ge.cut(1) )     )
     *       .and. unmasked
      endif
      
      if ( rangeisset(xrange) ) then
        unmasked = 
     *  (xrange(1) .le. data) .and. (xrange(2) .ge. data)
     *  .and. unmasked
      endif

      return
      end


***********************************************************************

      logical function rangeisset( range )
c
c     true if either of the values of range is ne zero
c
      implicit none
      real       range(2)

      rangeisset=( range(1) .ne. 0. ) .or. 
     *  ( range(2) .ne. 0. )

      return
      end
***********************************************************************


      double precision function calcrms( sum, sumsq, npoints, ok )

      double precision sum, sumsq
      integer          npoints
      logical          ok
      double precision rms

      if(     npoints.ge.2 ) then
         rms = ( sumsq - sum**2/dfloat(npoints) ) / dfloat(npoints-1) 
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

      subroutine pgpts( xarr, yarr, n, style, ymin )
      real       xarr(*), yarr(*)
      integer    n, style
      real       ymin
      if( style.eq.1 ) call pgline( n, xarr, yarr )
      if( style.eq.2 ) call pgbin(  n, xarr, yarr, .false. )
      if( style.eq.3 ) call pgcbin( n, xarr, yarr, .false., ymin )
      return
      end


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


      subroutine pgident
      include          'imhist.h'
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
         ident = 'IMHIST ' // pginfo
         call pgmtxt( 'T', BASE+2+YOFF, XOFF, LEFT, ident          )
         call pgmtxt( 'T', BASE+1+YOFF, XOFF, LEFT, plotpar(INFOP) )
         call pgmtxt( 'T', BASE  +YOFF, XOFF, LEFT, plotpar(BOXP)  )
      endif
      return
      end



