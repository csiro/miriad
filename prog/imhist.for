      program imhist

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
c
c$Id: imhist.for,v 1.4 2012/03/13 00:39:48 wie017 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      integer   naxis,  tinp
      ptrdiff   npixels
      real      cut(2), xrange(2)
      character device*80, version*72

      external  versan
      character versan*72
c-----------------------------------------------------------------------
      version = versan('imhist',
     *                 '$Revision: 1.4 $',
     *                 '$Date: 2012/03/13 00:39:48 $')

      call output('IMHIST: ' // version)
      call inputs(tinp, cut, xrange, npixels, naxis, device)
      call histo(tinp, cut, xrange, npixels, naxis, device)
      call xyzclose(tinp)
      call logclose

      end

c***********************************************************************

      subroutine inputs(tinp, cut, xrange, npixels, naxis, device)

      integer            tinp
      real               cut(2)
      real               xrange(2)
      ptrdiff            npixels
      integer            naxis
      character*(*)      device
c-----------------------------------------------------------------------
      include 'imhist.h'

      integer            MAXBOXES
      parameter          (MAXBOXES = 1024)

      character*1024     file
      integer            axlen(MAXNAX)
      integer            boxes(MAXBOXES)
      integer            blc(MAXNAX), trc(MAXNAX)
      integer            viraxlen(MAXNAX)
      ptrdiff            vircsz(MAXNAX)

      logical            rangeisset
c-----------------------------------------------------------------------
      call keyini

      call keyf('in', file, ' ')
      naxis = MAXNAX
      call xyzopen(tinp, file, 'old', naxis, axlen)

      call boxinput('region', file, boxes, MAXBOXES)
      call boxset (boxes, naxis, axlen, 's')
      call boxinfo(boxes, naxis, blc, trc)

      call optinp

      call cutinp(cut)

c  input xrange, defaulting to 0,0, which implies unset!!
      xrange(1)=0.0
      xrange(2)=0.0
      call keyr('xrange',xrange(1),0.0)
      call keyr('xrange',xrange(2),xrange(1)*(-1.0))

c  if the xrange is set, set the plot range too
c  this overrides values set using options (which is depricated)
      if (rangeisset(xrange)) then
        call setrange(xrange)
      endif

      call xyzsetup(tinp, ' ', blc, trc, viraxlen, vircsz)
      npixels = vircsz(naxis)

      call outdev(device)
      call keyfin
      call labset(tinp, file, blc, trc, naxis)
      call header(tinp, file)

      end

c***********************************************************************

      subroutine cutinp(cut)

      real          cut(2)
c-----------------------------------------------------------------------
      character*10  string
      logical       keyprsnt
c-----------------------------------------------------------------------
      if (keyprsnt('cutoff')) then
        call keyr('cutoff', cut(1),      0.0)
        call keya('cutoff', string, 'noabs')
        if (string.ne.'abs') cut(2) = 1.0
        if (string.eq.'abs') cut(2) = 2.0
      else
        cut(2) = 0.0
      endif

      end

c***********************************************************************

      subroutine optinp

c-----------------------------------------------------------------------
      include 'imhist.h'

      character*20  option
      logical       match
      integer       i
      real          v
      integer       matchnr
      integer       len1
      character*80  line

      logical       needval(NHISTP)
      real          histdef(NHISTP)
      data          needval  / .true.,.true.,.false.,.false.,.false. /
      data          histdef  / 25.0, 1.0, 0.0,0.0,0.0 /
c-----------------------------------------------------------------------
      do i = 1, NHISTP
        histpar(FLAG,i)  = 0.0
        histpar(VALUE,i) = histdef(i)
      enddo
      plotvar(SEL)   = -1
      plotvar(HEAD)  =  1
      plotvar(LIST)  =  1
      plotvar(HANWD) =  1
      plotvar(STYLE) = -1
      plotpar(TITLE) = ' '
      plotrnge(FLXL) = 0.0
      plotrnge(FLXU) = 0.0
      plotrnge(FLYL) = 0.0
      plotrnge(FLYU) = 0.0

      call keya('options', option, ' ')
      do while (option.ne.' ')
        call lcase(option)

        if (match(option, commonop, i)) then
          if (i.eq.matchnr('style',commonop)) then
            call keya('options', option, ' ')
            call assertl(match(option,styles,i), 'Illegal style')
            plotvar(STYLE) = i
          else if (i.eq.matchnr('noheader',commonop)) then
            plotvar(HEAD) = 0
          else if (i.eq.matchnr('nolist',commonop)) then
            plotvar(LIST) = 0
          else if (i.eq.matchnr('list',commonop)) then
            plotvar(LIST) = 1
          else if (i.eq.matchnr('title',commonop)) then
            call keya('options', plotpar(TITLE),   ' ')
            call keyr('options', plotrnge(XTITLE), MAGICVAL)
            call keyr('options', plotrnge(YTITLE), MAGICVAL)
          else
            call keyr('options', v, 0.0)
            if (i.eq.matchnr('xmin',commonop)) then
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

        else if (match(option, plotopts, i)) then
          histpar(FLAG,i) = 1.0
          if (needval(i))
     *        call keyr('options', histpar(VALUE,i), histdef(i))
          if (.not.needval(i)) histpar(VALUE,i) = 1.0
          if (i.eq.NBINP) call assertl(
     *        histpar(VALUE,NBINP).gt.0.0, 'Bad number of bins')
          if (i.eq.BINSP) call assertl(
     *        histpar(VALUE,BINSP).gt.0.0, 'Bin size must be >0')

        else
          line = 'Illegal or ambiguous option ' //
     *            option(:len1(option)) // ' ignored'
          call bug('w', line)

        endif
        call keya('options', option, ' ')
      enddo

      if (plotvar(STYLE).eq.-1) plotvar(STYLE)=matchnr('histo',styles)

      end

c***********************************************************************
c
c  Set the plotting global pltrange to reflect the values specified
c  by the key xrange
c
c  Dont really know what this does - just copied opint
c
c  dpr 14-12-00

      subroutine setrange(xrange)

      real     xrange(*)
c-----------------------------------------------------------------------

      include 'imhist.h'
c  (I know, but I didn't do it)
c-----------------------------------------------------------------------
      plotrnge(FLXL) = 1.0
      plotrnge(XLOW) = xrange(1)

      plotrnge(FLXU) = 1.0
      plotrnge(XUPP) = xrange(2)

      end

c***********************************************************************

      subroutine outdev(device)

      character*(*)  device
c-----------------------------------------------------------------------
      include  'imhist.h'

      character*1024 logfile
c-----------------------------------------------------------------------
      call keya('device', device,  ' ')
      if (device.eq.' ' .and. plotvar(LIST).eq.0) plotvar(LIST) = 1
      call keya('log',    logfile, ' ')
      call logopen(logfile, ' ')

      end

c***********************************************************************

      subroutine labset(tinp,file,blc,trc,naxis)

      integer       tinp
      character*(*) file
      integer       blc(*), trc(*), naxis
c-----------------------------------------------------------------------
      include 'imhist.h'

      integer       len1
      character*40  string
      character*20  units
      real          rdata
      character*10  rtoaf
      integer       i, j
      character*40  bln, tln

      integer       NRECOG
      parameter     (NRECOG = 4)
      character*10  runits(NRECOG), labels(NRECOG)
      data          runits  / 'JY', 'HZ', 'KM/S', 'KMS-1' /
      data          labels / 'Flux','Frequency','Velocity','Velocity' /
c-----------------------------------------------------------------------
      call rdhda(tinp, 'bunit', units, 'Unknown')
      string = units
      call lcase(string)


      i = 1
      do while (i.le.NRECOG .and.
     *  index(string, runits(i)(:len1(runits(i)))).eq.0)
        i = i + 1
      enddo
      if (i.le.NRECOG) string = labels(i)
      if (i.gt.NRECOG) string = 'Map value'
      write(plotpar(XLABP), '(a, '' ['', a, '']'')')
     *       string(:len1(string)), units(:len1(units))

      plotpar(YLABP) = ' '
      if (histpar(FLAG,LOGAR).eq.1.0) plotpar(YLABP) = 'Log'
      if (histpar(FLAG,CUMUL).eq.1.0)
     *plotpar(YLABP)(len1(plotpar(YLABP))+2:) = 'Cumulative'
      plotpar(YLABP)(len1(plotpar(YLABP))+2:) = 'Counts'

      call rdhda(tinp, 'object',   string, ' ')
      call rdhdr(tinp, 'restfreq', rdata,   0.0)
      if (string.ne.' ') plotpar(INFOP) = 'Source: ' // string
      if (rdata.ne.0.0) then
         plotpar(INFOP)(len1(plotpar(INFOP))+1:)='; '//rtoaf(rdata,1,6)
         plotpar(INFOP)(len1(plotpar(INFOP))+1:)=' GHz'
      endif
      plotpar(INFOP)(len1(plotpar(INFOP))+1:) = '; File: ' // file

      call mitoaf(blc, naxis, bln, i)
      call mitoaf(trc, naxis, tln, j)
      string = 'blc=(' // bln(:i) // '), trc=(' // tln(:j) // ')'
      plotpar(BOXP) = 'Bounding box: ' // string

      end

c***********************************************************************

      subroutine header(tinp, file)

      integer   tinp
      character file*(*)
c-----------------------------------------------------------------------
      include 'imhist.h'

      character line*80, units*20

      external  len1
      integer   len1
c-----------------------------------------------------------------------
      if (plotvar(HEAD).eq.0) return

      line = '***** Histogram of image '//file(:len1(file))//' *****'
      call logwrit(line)

      line = '      ' // plotpar(BOXP)(:72)
      call logwrit(line)

      call rdhda(tinp, 'bunit', units, 'Unknown')
      line = '      Unit of datavalues: ' // units
      call logwrit(line)

      end

c***********************************************************************

      subroutine histo(tinp, cut, xrange, npixels, naxis, device)

      integer       tinp
      real          cut(2)
      real          xrange(2)
      integer       naxis
      ptrdiff       npixels 
      character*(*) device
c-----------------------------------------------------------------------
      include 'imhist.h'

      logical       doplot
      integer       pgbeg

      integer       binmax

      integer       HLEN
      parameter     (HLEN = 1000)
      real          xvals(0:HLEN+1), hist(0:HLEN+1)
      data          hist / 0.0, HLEN * 0.0, 0.0 /
c-----------------------------------------------------------------------
      doplot = device.ne.' '
      if (doplot) then
        if (pgbeg(0,device,1,1).ne.1) then
          call pgldev
          call bug('f', 'Error opening plot device')
        endif
      endif

      call histset(tinp, cut, xrange, npixels, HLEN, binmax)
      call histmake(tinp, binmax, cut, xrange, npixels, xvals, hist)
      call histout(doplot, naxis, binmax, xvals, hist)

      end

c***********************************************************************

      subroutine histset(tinp, cut, xrange, npixels, HLEN, binmax)

      integer          tinp
      real             cut(2)
      real             xrange(2)
      ptrdiff          npixels
      integer          HLEN, binmax
c-----------------------------------------------------------------------
      include 'imhist.h'

      ptrdiff          npoints
      double precision sum, sumsq, calcrms
      real             minval, maxval
      logical          ok
      integer          delimi
      real             r, gausetup
c-----------------------------------------------------------------------
      call histvars(tinp,cut,xrange,npixels, npoints,sum,sumsq,
     *  minval,maxval)

      if (histpar(FLAG,BINSP).eq.0.0)
     *histpar(VALUE,BINSP) =
     *    1.001*(plotrnge(XUPP)-plotrnge(XLOW)) / histpar(VALUE,NBINP)

      histvar(NPTS)  = npoints
      histvar(MEANP) = sum / npoints
      histvar(RMSP)  = calcrms(sum, sumsq, npoints, ok)

      binmax = int((plotrnge(XUPP)-plotrnge(XLOW))
     *                 / histpar(VALUE,BINSP)) + 1
      call assertl(binmax.le.HLEN, 'Too many bins')
      binmax = delimi(binmax, 1, HLEN)

      r=gausetup(minval, maxval, histpar(VALUE,BINSP),
     *  histvar(NPTS),histvar(MEANP),histvar(RMSP),histpar(FLAG,CUMUL))

      end

c***********************************************************************

      subroutine histvars(tinp, cut, xrange, npixels,
     *                     npoints, sum, sumsq, minval, maxval)

      integer          tinp
      real             cut(2)
      real             xrange(2)
      ptrdiff          npixels,npoints
      double precision sum, sumsq
      real             maxval, minval
c-----------------------------------------------------------------------
      include 'imhist.h'

      ptrdiff          i
      real             data
      logical          mask
      logical          unmasked, init
c-----------------------------------------------------------------------
      if (cut(2).gt.0.0 .and. plotrnge(FLXU).eq.1.0)
     *    call assertl(cut(1).lt.plotrnge(XUPP),
     *                  'Cutoff is above histogram maximum!')

      init = .true.
      do i = 1, npixels
        call xyzpixrd(tinp, i, data, mask)
        if (unmasked(data, mask, cut, xrange)) then
          if (init) then
            npoints = 0
            minval  = data
            maxval  = data
            call xyzs2c(tinp, i, posmin)
            call xyzs2c(tinp, i, posmax)
            sum     = 0d0
            sumsq   = 0d0
            init    = .false.
          endif
          npoints = npoints + 1
          if (data.lt.minval) then
            minval = data
            call xyzs2c(tinp, i, posmin)
          endif
          if (data.gt.maxval) then
            maxval = data
            call xyzs2c(tinp, i, posmax)
          endif
          sum     = sum   + data
          sumsq   = sumsq + data*data
        endif
      enddo
      call assertl(.not.init,   'All datapoints are masked')
      call assertl(npoints.gt.1,'Histogramming 1 datapoint will fail')

      histvar(MINV) = minval
      histvar(MAXV) = maxval
      if (plotrnge(FLXL).eq.0.0) plotrnge(XLOW) = minval
      if (plotrnge(FLXU).eq.0.0) plotrnge(XUPP) = maxval
      if (cut(2).ne.0.0) call assertl(cut(1).lt.plotrnge(XUPP),
     *                   'Cutoff is above maximum in region!')

      end

c***********************************************************************

      subroutine histmake(tinp, binmax, cut, xrange, npixels, xvals,
     *   hist)

      integer          tinp
      integer          binmax
      real             cut(2)
      real             xrange(2)
      ptrdiff          npixels
      real             xvals(0:*), hist(0:*)
c-----------------------------------------------------------------------
      include 'imhist.h'

      integer          bin
      ptrdiff          i
      real             data
      logical          mask, unmasked
      real             rbin
      integer          delimi
c-----------------------------------------------------------------------
      do bin = 0, binmax+1
        xvals(bin) = (bin-1)*histpar(VALUE,BINSP)+plotrnge(XLOW)
      enddo
      xvals(0)        = xvals(0)        + histpar(VALUE,BINSP) / 2.0
      xvals(binmax+1) = xvals(binmax+1) + histpar(VALUE,BINSP) / 2.0

      do i = 1, npixels
        call xyzpixrd(tinp, i, data, mask)
        if (unmasked(data, mask, cut, xrange)) then
          rbin = (data-plotrnge(XLOW)) / histpar(VALUE,BINSP)
          if (rbin.ge.0.0) bin = int(rbin)+1
          if (rbin.lt.0.0) bin = int(rbin)
          bin = delimi(bin, 0, binmax+1)
          hist(bin) = hist(bin) + 1.0
        endif
      enddo

      call histmedi(hist, binmax)

      if (histpar(FLAG,CUMUL).eq.1.0) then
        do bin = 2, binmax
          hist(bin) = hist(bin) + hist(bin-1)
        enddo
      endif

      if (histpar(FLAG,LOGAR).eq.1.0) then
        do bin = 0, binmax+1
          if (hist(bin).ne.0.0) hist(bin) = alog10(hist(bin))
        enddo
      endif

      end

c***********************************************************************

      subroutine histmedi(hist, binmax)

      real          hist(0:*)
      integer       binmax
c-----------------------------------------------------------------------
      include 'imhist.h'

      real          sum
      integer       bin
c-----------------------------------------------------------------------
      sum = 0.0
      bin = 0
      do while (sum.lt.histvar(NPTS)/2.0 .and. bin.le.binmax)
        sum = sum + hist(bin)
        bin = bin + 1
      enddo
      histvar(MEDIANP) = (bin-1)*histpar(VALUE,BINSP) + plotrnge(XLOW)

      end

************************************************************************

      subroutine histout(doplot, naxis, n, xarr, yarr)

      logical       doplot
      integer       naxis, n
      real          xarr(0:*), yarr(0:*)
c-----------------------------------------------------------------------
      include 'imhist.h'

      real          xmin, xmax, ymin, ymax
      integer       ismax
      real          EXTEND
      parameter     (EXTEND = 0.05)

      integer       bin, i
      ptrdiff       ny
      character*80  line
      integer       astlen
      parameter     (astlen = 50)
      character*(astlen) asterisk
c-----------------------------------------------------------------------
      do i = 1, astlen
        asterisk(i:i) = '*'
      enddo

      call histinfo(naxis, xarr(1), xarr(n))

      xmin = xarr(0)   - EXTEND*(xarr(n+1) - xarr(0))
      xmax = xarr(n+1) + EXTEND*(xarr(n+1) - xarr(0))
      ymin = -0.1
      ymax = 1.25 * yarr(ismax(n,yarr(1),1))
      if (plotrnge(FLYU).eq.1.0) ymax = plotrnge(YUPP)

      if (doplot) then
        call pgenv(xmin, xmax, ymin, ymax, 0, 0)
        call pgpts(xarr(1), yarr(1), n, plotvar(STYLE), ymin)
        call undovr(xarr(0),   yarr(0),   ymax)
        call undovr(xarr(n+1), yarr(n+1), ymax)
        if (histpar(FLAG,GAUCRV).eq.0.0) call gaucurve
        call pgident
        call pgend
        call output(' ')

      else
        call logwrit(' ')
        call logwrit('  Bin    Value          Number')
        write(line, '(''       Underflow   '', i8)') int(yarr(0))
        call logwrit(line)
        do bin = 1, n
          i = (yarr(bin) / ymax) * astlen
          ny = yarr(bin)
          if (i.ge.1) then
            write(line, '(i5, 4x, 1pg10.3, i8, 1x, a)')
     *            bin, xarr(bin), ny, asterisk(:i)
          else
            write(line, '(i5, 4x, 1pg10.3, i8)')
     *            bin, xarr(bin), ny
          endif
          call logwrit(line)
        enddo
        ny = yarr(n+1)
        write(line, '(''       Overflow    '', i11)') ny
        call logwrit(line)
      endif

      end

c***********************************************************************

      subroutine histinfo(naxis, xmin, xmax)

      integer          naxis
      real             xmin, xmax
c-----------------------------------------------------------------------
      include 'imhist.h'

      integer          i, len1
      ptrdiff          n
      character*80     line
c-----------------------------------------------------------------------
      call logwrit(' ')
      call logwrit('Histogram information')

      line='# of points         Mean            rms            Median'
      i = len1(line) + 2
      line(i:) = 'between'
      if (histvar(MEDIANP).le.xmin) line(i:) = 'below'
      if (histvar(MEDIANP).ge.xmax) line(i:) = 'above'
      call logwrit(line)
      n = histvar(NPTS)

      write(line, '(i11, 3x, 1pe14.7,1x, 1pe14.7)')
     *       n, histvar(MEANP), histvar(RMSP)

      i = len1(line) + 5
      if (histvar(MEDIANP).le.xmin .or.
     *    histvar(MEDIANP).ge.xmax) then
        write(line(i:), '(5x, 1pe14.7)') histvar(MEDIANP)
        write(line(i:), '(5x, 1pe14.7)') histvar(MEDIANP)
      else
        write(line(i:), '(1pe14.7, '' and '', 1pe14.7)')
     *    histvar(MEDIANP) - histpar(VALUE,BINSP), histvar(MEDIANP)
      endif
      call logwrit(line)

      write(line, '(''Maximum is '',1pe14.7,'' at ('')')
     *                histvar(MAXV)
      call mitoaf(posmax, naxis, line(len1(line)+1:), i)
      line(len1(line)+1:) = ') (absolute coordinates)'
      call logwrit(line)

      write(line, '(''Minimum is '',1pe14.7,'' at ('')')
     *                histvar(MINV)
      call mitoaf(posmin, naxis, line(len1(line)+1:), i)
      line(len1(line)+1:) = ') (absolute coordinates)'
      call logwrit(line)

      end

c***********************************************************************

      subroutine undovr(x, y, ymax)

      real       x, y, ymax
c-----------------------------------------------------------------------
      if (y.ne.0.0) then
        if (y.lt.ymax) call pgpt(1, x,      y,       8)
        if (y.ge.ymax) call pgpt(1, x, 0.95*ymax, 2262)
      endif

      end

c***********************************************************************

      subroutine gaucurve

c-----------------------------------------------------------------------
      include 'imhist.h'

      real    xmin, xmax

      external gauss, loggauss, gaussint, loggint
      real     gauss, loggauss, gaussint, loggint
c-----------------------------------------------------------------------
      xmin = plotrnge(XLOW) + histpar(VALUE,BINSP)/2.0
      xmax = plotrnge(XUPP) - histpar(VALUE,BINSP)/2.0

      if (histpar(FLAG,CUMUL).eq.0.0) then
        if (histpar(FLAG,LOGAR).eq.0.0) then
          call pgfunx(gauss,    1000, xmin, xmax, 1)
        else if (histpar(FLAG,LOGAR).eq.1.0) then
          call pgfunx(loggauss, 1000, xmin, xmax, 1)
        endif
      else if (histpar(FLAG,CUMUL).eq.1.0) then
        if (histpar(FLAG,LOGAR).eq.0.0) then
          call pgfunx(gaussint, 1000, xmin, xmax, 1)
        else if (histpar(FLAG,LOGAR).eq.1.0) then
          call pgfunx(loggint,  1000, xmin, xmax, 1)
        endif
      endif

      end

c***********************************************************************

      real function gauss(x)

      real       x
c-----------------------------------------------------------------------
      real       loggauss, gaussint, loggint, gausetup
      real       cum
      real       minval, maxval, binsize
      real       npoints, mean, rms

      real       z, t, erf
      include    'mirconst.h'
      real       gausum
      integer    bin, binmin, binmax
      real       norm, x0, sigma
      save       norm, x0, sigma
c-----------------------------------------------------------------------

      gauss = norm * exp(-(x-x0)**2/(2.0*sigma**2))
      return

      entry loggauss(x)
      loggauss = alog10(norm * exp(-(x-x0)**2/(2.0*sigma**2)))
      return

      entry gaussint(x)
      z = abs((x-x0)/sqrt(2.0)/sigma)
      t = 1.0 / (1.0+0.5*z)
      erf = 1.0 - t*exp(-z*z -
     *          1.26551223 + t*(1.00002368 + t*(0.37409196 +
     *      t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +
     *      t*(-1.13520398 + t*(1.48851587 + t*(-0.82215223 +
     *      t*(0.17087277
     *    ))))))))))
      gaussint = norm * (0.5 + 0.5*erf*sign(1.0,(x-x0)))
      return

      entry loggint(x)
      z = abs((x-x0)/sqrt(2.0)/sigma)
      t = 1.0 / (1.0+0.5*z)
      erf = 1.0 - t*exp(-z*z -
     *          1.26551223 + t*(1.00002368 + t*(0.37409196 +
     *      t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +
     *      t*(-1.13520398 + t*(1.48851587 + t*(-0.82215223 +
     *      t*(0.17087277
     *    ))))))))))
      loggint = alog10(norm * (0.5 + 0.5*erf*sign(1.0,(x-x0))))
      return


      entry gausetup(minval,maxval,binsize, npoints,mean,rms, cum)
      gausum = 0.0
      binmin = int((minval-mean) / binsize)
      binmax = int((maxval-mean) / binsize) + 1
      do bin = binmin, binmax
        gausum = gausum + exp(-(bin*binsize)**2 / (2.0*rms**2))
      enddo
      if (cum.eq.0.0) norm = npoints / gausum
      if (cum.eq.1.0) norm = npoints
      x0    = mean
      sigma = rms

      end

c***********************************************************************

      logical function unmasked(data, mask, cut, xrange)

      real       data
      logical    mask
      real       cut(2)
      real       xrange(2)
c-----------------------------------------------------------------------
      logical            rangeisset
c-----------------------------------------------------------------------
      unmasked=mask

      if (cut(2).gt.0.0) then
        unmasked = ((cut(2).eq.1.0 .and.     data.ge.cut(1))  .or.
     *             (cut(2).eq.2.0 .and. abs(data).ge.cut(1))) .and.
     *              unmasked
      endif

      if (rangeisset(xrange)) then
        unmasked =
     *  (xrange(1).le.data) .and. (xrange(2).ge.data)
     *  .and. unmasked
      endif

      end

c***********************************************************************

      logical function rangeisset(range)

      real       range(2)
c-----------------------------------------------------------------------
c  True if either of the values of range is ne zero.
c-----------------------------------------------------------------------

      rangeisset = (range(1).ne.0.0) .or.  (range(2).ne.0.0)

      end

c***********************************************************************

      double precision function calcrms(sum, sumsq, npoints, ok)

      double precision sum, sumsq
      integer   npoints
      logical   ok
c-----------------------------------------------------------------------
      double precision rms
c-----------------------------------------------------------------------
      if (npoints.ge.2) then
        rms = (sumsq - sum**2/dfloat(npoints)) / dfloat(npoints-1)
        if (rms.ge.0d0) then
          rms = sqrt(rms)
          ok = .true.
        else
          call bug('w', 'Rms^2 is negative!! Square root not taken')
          ok = .false.
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

      subroutine pgpts(xarr, yarr, n, style, ymin)

      real       xarr(*), yarr(*)
      integer    n, style
      real       ymin
c-----------------------------------------------------------------------
      if (style.eq.1) call pgline(n, xarr, yarr)
      if (style.eq.2) call pgbin(n, xarr, yarr, .false.)
      if (style.eq.3) call pgcbin(n, xarr, yarr, .false., ymin)

      end

c***********************************************************************

      subroutine pgcbin(n, xarr, yarr, center, ymin)

      integer      n
      real         xarr(*), yarr(*)
      logical      center
      real         ymin
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

        x = xarr(i) - xoff
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
      include 'imhist.h'

      integer   i
      character pginfo*40
c-----------------------------------------------------------------------
      call pgsch(1.0)
      call pglab(plotpar(XLABP), plotpar(YLABP), ' ')
      if (plotpar(TITLE).ne.' ') then
        call pgtext(plotrnge(XTITLE),plotrnge(YTITLE),plotpar(TITLE))
      endif

      if (plotvar(HEAD).eq.1) then
        call pgsch(SC)
        call pgqinf('now', pginfo, i)
        call pgmtxt('T', BASE+2+YOFF, XOFF, LEFT, 'IMHIST ' // pginfo)
        call pgmtxt('T', BASE+1+YOFF, XOFF, LEFT, plotpar(INFOP))
        call pgmtxt('T', BASE  +YOFF, XOFF, LEFT, plotpar(BOXP))
      endif

      end
