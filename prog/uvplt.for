       program uvplt
c-----------------------------------------------------------------------
c= UVPLT - Make plots from a UV-data base on a PGPLOT device.
c	
c& nebk
c: uv analysis, plotting
c+
c  UVPLT - Plot a variety of quantities from visibility data bases.
c	Options are available to time average data (plot with optional
c	error bars) and to plot different baselines on separate 
c	sub-plots on each page plus many others.
c@ vis
c	The input visibility file(s). Multiple input files and wild card
c	card expansion are supported.    
c	No default
c@ line
c	This is the normal linetype specification. See the help on "line"
c	for more information. The default is all channels.
c@ select
c	This selects which visibilities to be used. Default is all
c	visibilities. See the Users Guide for information about how
c	to specify uv data selection.
c	Default is all data
c@ stokes
c	Select Stokes parameter(s) or polarization(s) from:
c	  xx, yy, xy, yx,  i, q, u, v, 
c	  rr, ll, rl, lr
c	Default is all polarizations or Stokes parameters present
c@ axis
c	Two values (minimum match active), one for each of the x 
c	and y axes chosen from:
c	  time                     [time in DD HH MM SS.S format]
c	  dtime                    [time in decimal days format]
c	  amplitude, real, imag    [natural units; Jy]
c	  phase                    [degrees]
c	  uu, vv                   [u & v in klambda]
c	  uc, vc                   [u,& v, -u & -v in klambda]
c	  uvdistance               [sqrt(u**2+v**2)]
c	  uvangle                  [uv pos'n angle clockwise from v axis]
c	  hangle                   [hour angle in HH MM SS.S]
c	  dhangle                  [hour angle in decimal hours]
c	  parang                   [parallactic angle in degrees]
c         lst                      [local sidereal time in decimal hours]
c         az                       [azimuth in degrees]
c         el                       [elevation in degrees]
c         airmass                  [airmass=1/sin(el)]
c         jyperk                   [system gain, in Jy/K]
c         rms                      [theoretical visibility noise rms, in flux units]
c	NOTE: parang is the true parallactic angle of the source, which can
c	be quite different from the angle between source and antenna feed
c	(Miriad variable chi).
c
c	Defaults are axis=time,amp  (x and y axes).
c@ xrange
c	Plot range in the x-direction 
c	  If axis = uu, vv, or uvdistance [kilo-lambda;   2 values]
c	          unless OPTIONS=NANOSEC;     then   nanoseconds]
c	  If axis = uvangle               [degrees;       2 values]
c	  If axis = time                  [dd,hh,mm,ss.s; 8 values]
c	  If axis = dtime                 [decimal days;  2 values]
c	  If axis = amplitude, real, imag [natural units; 2 values]
c	  If axis = phase                 [degrees;       2 values]
c	  If axis = hangle                [hh,mm,ss.s;    6 values]
c         If axis = dhangle               [decimal hours; 2 values]
c	  If axis = parang                [degrees;       2 values]
c         If axis = az or el              [degrees;       2 values]
c         If axis = airmass               [natural units; 2 values]
c         If axis = jyperk                [Jy/K;          2 values]
c         If axis = rms                   [flux units;    2 values]
c         If axis = lst                   [decimal hours; 2 values]
c
c	Default is to self-scale (see also OPTIONS=XIND).
c@ yrange
c	Plot range in the y-direction as for the x axis.  The 
c	default is to self-scale (see also OPTIONS=YIND).
c@ average
c	The averaging time in minutes (unless OPTIONS=DAYS,HOURS,SECONDS).
c	Averaging is reset at frequency, source, or pointing centre
c	changes.  Individual baselines and polarizations are averaged
c	separately (unless OPTIONS=AVALL).  If you have selected multiple
c	channels and you also ask for time averaging, then all the
c	selected channels are averaged together in the time interval.
c	If you wish to use OPTIONS=AVALL to average everything on
c	the one subplot (e.g. polarizations) but don't want temporal
c	averaging, set AVERAGE to less than one integration.
c	Default is no averaging.
c@ hann
c	Hanning smoothing length (an odd integer < 15).   Is applied
c	after any time averaging and INC selection. Useful for amplitude
c	or phase, say, plotted against time.  Error bars remain unaffected
c	by Hanning smoothing.  Currently, the Hanning smoothing is unaware 
c	of source or frequency changes. Use SELECT if you have boundary 
c	problems. 
c	Default is no smoothing (hann = 1).
c@ inc
c	Plot every INCth point (on each sub-plot) that would normally 
c	have been selected.   Useful if you don't want to average, but 
c	want to cut down on the number of plotted points.  Beware of 
c	increments that divide exactly into the number of baselines 
c	in time ordered data.  
c	Default is 1.
c@ options
c	Task enrichment options. Minimum match is effective. 
c	 nocal   Do not apply the gain corrections
c	 nopol   Do not apply the polarization leakage corrections
c	 nopass  Do not apply the bandpass corrections
c
c	 nofqav  By default, uvplt averages together all channels from
c	         a visibility record before plotting. The nofqav option
c	         disables this, and causes individual channels to be
c	         plotted.  Note that this option is not active when
c		 time averaging is invoked (frequency averaging is always
c 		 done then).
c
c	 nobase  Plot all baselines on the same plot, otherwise
c	         each baseline is plotted on a separate sub-plot.
c
c        notitle Do not write any title on the plot.  Probably most useful 
c                if plotting all baselines on the same plot (using the
c                "nobase" option).
c	 
c	 2pass   Normally uvplt makes assumptions about what it is
c	 	 expecting to find in the data with regards polarizations
c		 and baselines.   Under some conditions, uvplt may
c		 report that it has not allocated sufficient buffer
c		 space.  This option instructs uvplt to make two passes
c		 through the data, the first to accumulate precise
c		 information on the contents of the selected data so
c		 that buffer space is optimally allocated.
c
c	 scalar  Do scalar (average amplitudes or phases) rather than
c	         vector (average real and imaginary) averaging.
c	         This is useful if the visibilities are uncalibrated &
c	         the phase is winding over the averaging interval & you
c	         would like an averaged amplitude. Scalar averaged phase
c	         is not very meaningful in general.
c	 avall   If you are averaging in time, then average all data
c		 selected on each sub-plot together.  E.g. all selected
c		 polarizations, and, if OPTIONS=NOBASE, all baselines
c		 as well.  If you wish to average all the things on
c		 one subplot together but without temporal averaging,
c		 just set the averaging time to less than one integration.
c	 unwrap  When plotting phase, try to unwrap it so that
c	         say, if one point is 179 deg and the next -179,
c		 they will be plotted as 179 and 181 deg.  NOTE:
c		 Unwrapping noise can be VERY misleading.
c
c	 rms     Draw error bars (+/- 1 standard deviation) on the plot if 
c		 averaging is invoked. 
c	 mrms    Draw error bars (+/- 1 standard deviation in the mean)
c		 on the plot if averaging is invoked. 
c	 noerr   The automatically worked out min and max plot limits
c	         will NOT include the ends of the error bars.
c
c	 all     Plot flagged and unflagged visibilties
c	 flagged Plot only flagged visibilities
c		 The default is to plot only unflagged (good) visibilities
c	         ALL overrides  FLAGGED
c
c	 nanosec u and v are plotted in nano-seconds rather than k-lambda
c	 days    The averaging interval is in days rather than minutes
c	 hours   The averaging interval is in hours rather than minutes
c	 seconds The averaging time is in seconds rather than minutes
c
c	 xind    If the x-axis is self-scaled, then unless OPTIONS=NOBASE,
c	         setting XIND will cause each sub-plot to have the x-axis 
c		 self-scaled independently.  The default is that the x-range
c		 used is that which encompasses the ranges from all sub-plots.
c	 yind    The equivalent for the y-axis
c
c	 equal   Plot x and y with equal scales.  Useful only for plots
c	         like AXIS=UU,VV.  Does not mean the plot will necessarily
c	         be square
c	 zero    Plot the x=0 and y=0 lines
c
c	 symbols Each file is plotted with a different plot symbol
c	 nocolour
c	         Each file is plotted with the same colour (white). By
c		 default and when there is only one polarization, each 
c		 file has a separate colour.
c	 dots    If time averaging is invoked, plot the data with dots
c	         rather than filled in circles.  These plot much faster
c		 on hardcopy devices.
c
c	 source  Put the source name rather than the file name in the
c	         plot title
c	 inter   After the plot is drawn, you get a chance to redraw
c	         the plot with a different x- and y-range, and also
c		 on a different device.  In this way you can make a
c		 hard-copy without re-running the program. In this case, 
c	         points outside of the user specified x,y-range are
c	         ARE included in the plot buffer, so that if you redefine
c		 the ranges, those points are available for plotting.
c
c	 log     Write the values and errors (if averaging) that are 
c		 plotted into the log file.  No attempt to separate 
c		 baselines is made, except that automatically obtained 
c		 by not setting OPTIONS=NOBASE
c@ device
c	PGPLOT plot device/type. No default.
c@ nxy
c	Number of plots in the x and y directions for when plotting
c	each baseline separately. Defaults try to choose something
c	sensible.
c@ size
c	PGPLOT character sizes, in units of the default size (i.e., 1)
c	First value is for the labels, the second is for the symbol size
c	Defaults depend upon the number of sub-plots. The second value 
c	defaults to the first.
c@ log
c	The output logfile name. The default is the terminal.
c@ comment
c	A one line comment which is written into the logfile.
c--
c
c  History:
c    nebk 22May89  Original version.
c    nebk 21Sep89  Change PGPAGE calls, replace READ by PROMPT and decoding
c    rjs  18oct89  Changes to new calling sequence for planet scaling/rotation.
c    rjs  23oct89  Changed 'pdev' to 'device'.
c    pjt   2may90  maxdim.h now gets maxants and maxchan
c    mchw 28jun90  Updated to use uvdata selection criteria.
c    mchw 05jul90  Made standard uvheader.
c    rjs   2nov90  Corrected documentation.
c    nebk 18feb91  Substantial changes to add 'time', 'dtime', 'u', 'v',
c		   'uc', 'vc',  'real' and 'imag' plot options.  Add 
c		   Stokes (IQUV) selection and multiple input file ability
c		   by invoking the UVDAT* layer of subroutines.
c    nebk 20feb91  Even more substantial changes to add time averaging.
c                  Move flagging options to OPTIONS
c    nebk 26feb91  Add capability to plot different baselines separately
c    nebk  5mar91  Change itoa to itoaf, atod to atodf.  Add OPTIONS=INTER
c    nebk 24mar91  Rework MAXBASE/MAXBASE2 to avoid conflict, add
c                  OPTIONS=NOERR, and implement averaging of u and v.
c    nebk 28mar91  Add OPTIONS=AUTO, NOCROSS, and LOG
c    nebk 10apr91  Add OPTIONS=ZERO,NANOSECONDS, & EQUAL, XAXIS=UVPA, and
c		   add HANN.  Fix bounds problem when no points to plot.  
c    nebk/mjs
c         11apr91  Fix bug with time range and MKEYR.  Change default
c                  xaxis to 'dtime' instead of 'time'
c    nebk 06may91  Adjust for new pgtime routines and fix bug with getting
c                  users comment.  CHange default axis back to 'time' 
c                  since revised PGTIME routines now have a day field.
c    nebk 21may91  Add OPTIONS=NOCAL, prevent PGPAGE call on last plot,
c                  put Stokes selection on plot and use roman font
c    nebk 31may91  Add OPTIONS=SOURCE
c    nebk 06jun91  Add planet scaling.
c    nebk 11jun91  Fix bug with averaged vector quantities; phase wrong.
c    nebk 17jun91  Fiddle with fonts
c    rjs  27jun91  Changed "docal" to "nocal", add OPTIONS=NOPOL
c    nebk 08aug91  Add DAY field to XRANGE.
c    mchw 28aug91  Test for zero length comment in call to LogWrite
c    nebk 04sep91  Fix logic error when dumping averaged buffers into
c                  the plot buffer - was being done in the wrong place
c    nebk 08sep91  Rearrange logic so that x-value gets tested for user's
c                  plot range after averaging instead of before.  
c                  Add OPTIONS=AVALL,DAYS,HOURS
c    nebk 09sep91  Rewrite some code that could cause integer overflows
c                  with very large averaging times.
c    nebk 04oct91  Plot baselines in increasing order (1-2,1-3 etc)
c                  Add OPTIONS=XIND 
c    nebk 16jan92  CHange OPTION=BASE to OPTION=NOBASE
c    nebk 17feb92  Don't open device when no points.  Add OPTIONS=WRAP.
c		   Fix a bug that was preventing  memory allocation if the
c		   first file in the VIS list had no selected data.
c    nebk 26feb92  Add OPTIONS=SYMBOLS.  This required a lot of rewriting
c    nebk 07mar92  Add OPTIONS=YIND
c    rjs  12mar92  When autoscaling, keep away from rounding problems.
c    nebk 12apr92  Lengthen str2 by 2 for escaping of \ for SUNs in MTITLE
c    nebk 28apr92  Finally implement the same axis types for both X
c                  and Y so both X and Y depend on channel now. Merge
c	           XAXIS and YAXIS into AXIS.  Reformat documentation.
c    nebk 24may92  Use my fabbo KEYMATCH for AXIS to shut Mr S up.
c    rjs  27may92  Absolutely unbelievable mistake. Only Mr K could do it.
c                  Failed to declare a string variable to be long enough.
c    nebk 08jun92  Interchange HatCreek NXY defaults and properly plot
c                  flagged data for OPTIONS=ALL (bug inserted 28apr92)
c    nebk 23jun92  Move final accumlator flush inside file do loop
c                  CHange options=WRAP to UNWRAP.  Finish averaging
c		   when RA or DEC change.
c    nebk 07jul92  Fix problem with last point of file being plotted
c		   as if it belonged to the next file when averaging
c    rjs  17aug92  Replace obsolete uvtrack with soon to be obsolete
c		   uvvarini
c    nebk 25sep92  Remove options AUTO and NOCROSS to SELECT and add
c                  independent averaging of polarizations.  Better NFILES
c		   memory management when DOSYMB is false. 
c    nebk 30sep92  Trap against not all pol'ns turning up in first record.
c    nebk 02oct92  Rearrange sequence of UVDATRD calls
c    nebk 07oct92  Implement standard BASANT subroutine call.
c    nebk 09dec92  Fix bounds bug in baspolid for options=nobase and 
c                  implement more flexible mixed polarization handling
c    nebk 24dec92  When unwrapping phases, don't do any range testing.
c    nebk 19jan93  Increment by 1 length of OPS in INPUTS (NOPASS did this)
c    nebk 09feb93  Use memalloc and overlay blank common with UVDAT
c    nebk 16feb93  Change size (clever rjs algorithm),nx,ny defaults
c    nebk 25feb93  Change to doc. Fiddle with size parameter defaults,
c                  add axis=hangle,dhangle, rework interactive window stuff,
c                  plot polarizations with different colours
c    mjs  15mar93  pgplot names have 6 or less chars.
c    rjs  18mar93  Assume there is at least 1 polarisation in the data.
c    nebk 27apr93  Following suggestion by pjt implement getting of longitude
c                  through obspar as well as uv variable.  Fiddle HA -> +/- pi
c    nebk 18jun93  Add options=colour
c    nebk 17mar94  Add OPTIONS=2PASS  and change to OPTIONS=NOCOLOUR
c    nebk 15jan95  Be less restrictive on use of -u and -v
c    nebk 11mar95  No yellow for hardcopy devices
c    nebk 22spe95  Add axis=parang
c    nebk 03oct95  Fix calculation of hour angle.
c    rjs  01dec95  Improve an error message.
c    nebk 06dec95  Push MAXBASE2 upto 36 for Hat Creek.  Does not affect ATCA
c    nebk 09jan95  Only work out longitude if really needed; some data sets
c		   don't have it.
c    nebk 22may96  Add options=mrms
c    rjs  06jun96  Change frequency behaviour to default to all channels.
c    rjs  30jul96  2pass tries to guess the number of points needed.
c    rjs  14feb97  If the user sets nxy in options=nobase, then honour it.
c    nebk 18jun98  Document nofqav options better
c    rjs  19nov98  Minor correction to computation of LST, and recompute from
c		   scratch the parallactic angle.
c    swa  03sep99  Add options=notitle.
c    rjs  10mar00  Set maxbase2=91.
c    rjs  28mar00  Fix colour indices when there are more than NCOL
c		   inputs.
c    rjs  04may00  Tidy up requirement for lat,long,lst and more.
c    rjs  26sep00  Correct uvangle code.
c    rjs  10jan01  Added az,el,lst to possible axes.
c    rjs  20jan01  Change definition of nbases in uvfish to allow for
c		   possible presence of autocorrelations.
c    rjs  02mar01  Added airmass to possible axes.
c    rjs  04may03  getlst could return the wrong value in some circumstances.
c    rjs  20sep04  Added jyperk and rms.
c    rjs  09may06  Disable planet processing.
c    rjs  07jun06  Neater messages.
c
c To do:
c
c   Vector averaging rms not yet implemented
c   add keyword "bin" 
c   add keyword "model"
c   do better than AVERAGE< integration time + doavall
c   leave baseline mask in place after uvdes rather than redetermine it
c   
c   Notes
c   -----
c 1)   ANtennas
c       maxbase    is the maximum number of baselines corresponding
c                  to maxant.  Both are in MAXDIM.H
c       maxbase2   is the potential number of baselines that can
c                  be plotted singly unless options=2pass
c       pl2dim     is the actual number of baselines to be plotted singly
c                  and the size of the second dimension of BUFFER
c 
c       Because MAXANT is generally 27, MAXBASE is a big number, and
c       slicing up MAXBUF words using this is wasteful if you have far
c       fewer antennas than this.  MAXBASE2 is thus set to something
c       manageable, like 15.  The actual number of baselines allowed
c       to be plotted singly, PL2DIM, is the minimum of MAXBASE,
c       MAXBASE2, or NANTS*(NANTS-1)/2 where NANTS is the number
c       of antennas in the data.  If user gives OPTIONS=2PASS then
c       MAXBASE2 is ignored and they get what they ask for.
c
c 2)   Averaging
c       Each baseline and polarization is averaged separately 
c       regardless of whether they are plotted on the same or
c       different subplots, unless the AVALL option is used 
c       whereupon everything on each sub-plot is averaged together.
c       These might be polarizations or polarizations and baselines
c       (NOBASE).   If everything is on one plot (NOBASE) we
c       have the ability to average up to MAXBASE (rather than
c       PL2DIM) baselines.   In the other case, any baselines over 
c       PL2DIM get ditched.  Note that even if there is no averaging
c       we maintain the polarization identity in the plot buffer
c       (unlike baselines) so that they can be plotted with a different
c       symbol.
c
c 3)   Files
c       There is a parameter called MAXFILE which is the maximum number
c       of files to be read.  It is only used as an easy way to declare
c       NPTS and PLPTS which are small arrays.   BUFFER is chopped up
c       according to either the ACTUAL number of files that are read
c	if different plot symbols for each are wanted, else all 
c       points are put in the same FILE lcoation in BUFFER.  In this
c       way the available space is better used.  MAXFILE is set to
c       be much bigger than any number of files the user is ever
c	likely to input.
c
c
c-----------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      include 'mirconst.h'
c
      integer maxbase2, maxco, maxpol, maxfile
      parameter (maxbase2 = 91, maxco = 7, maxpol = 4, maxfile = 30)
c
      complex data(maxchan)
      double precision freq(maxchan)
      real buffer(maxbuf), coeffs(maxco), work(maxco)
      logical goodf(maxchan)
      integer maxbuf2
      common buffer
c
c These arrays for averaging
c
      real 
     +  xsumr(maxbase,maxpol),   xsumsqr(maxbase,maxpol),
     +  xsumi(maxbase,maxpol),   xsumsqi(maxbase,maxpol), 
     +  xsig(maxbase,maxpol),    ysumr(maxbase,maxpol), 
     +  ysig(maxbase,maxpol),    ysumi(maxbase,maxpol),
     +  ysumsqr(maxbase,maxpol), ysumsqi(maxbase,maxpol),
     +  xave(maxbase,maxpol),    yave(maxbase,maxpol)
      integer nsum(maxbase,maxpol)
c
c Plot extrema
c
      real xxmin(maxbase), xxmax(maxbase), yymin(maxbase), 
     +  yymax(maxbase)
c
c Plotting pointers, dimensions and masks
c
      integer polmsk(-8:4), basmsk(maxbase)
      integer nfiles, npols, nbases, pl1dim, pl2dim, pl3dim, 
     +  pl4dim, maxpnt, xo, yo, elo(2), eho(2), plfidx, pidx, 
     +  plbidx, stbidx
c
c Plot counters
c
      integer npts(maxbase,maxpol,maxfile), order(maxbase),
     +  plpts(maxbase,maxpol,maxfile), a1a2(maxbase,2)
c
      double precision preamble(4), fday, dayav, baseday, day, 
     +  ha, ra, dec, lst, lat, az, el, dtemp
      real size(2), xmin, xmax, ymin, ymax, u, v, uvdist, uvpa, xvalr,
     +  yvalr, paran, jyperk, rms
      integer lin, ivis, nread, dayoff, j,  nx, ny, inc, hann, tunit,
     +  ofile, ifile, jfile, vupd, ip, nkeep, npnts
      character in*64, xaxis*10, yaxis*10, pdev*80, comment*80, 
     +  logf*80, str*2, title*100, ops*9
      logical xrtest, yrtest, more, dodoub, reset, doave, dowave,
     +  dovec(2), dorms(3), doall, doflag, dobase, doperr, dointer,
     +  dolog, dozero, doequal, donano, dosrc, doavall, bwarn(2), 
     +  skip, xgood, ygood, doxind, doyind, dowrap, none, dosymb, 
     +  dodots, false(2), allfull, docol, twopass, dofqav, dotitle
c
c Externals
c
      integer membuf
      logical uvdatopn
      character itoaf*2
c
c Initialize
c
      integer ifac1, ifac2
      parameter (ifac1 = maxpol*maxbase*maxfile, 
     +           ifac2 = maxbase)
      data false, bwarn /.false., .false., .false., .false./
      data none, allfull /.true., .false./
      data ivis, title /0, ' '/
      data plfidx, ifile, ofile /0, 0, 0/
      data npts, plpts, basmsk /ifac1*0, ifac1*0, ifac2*0/
      data polmsk /13*0/
c-----------------------------------------------------------------------
      call output ('UvPlt: version 1.0 09-May-06')
c
c  Get the parameters given by the user and check them for blunders
c
      call inputs (maxco, xaxis, yaxis, xmin, xmax, ymin, ymax, dayav,
     +   tunit, dorms, dovec, doflag, doall, dobase, dointer, doperr,
     +   dolog, dozero, doequal, donano, dosrc, doavall, doxind, 
     +   doyind, dowrap, dosymb, dodots, docol, inc, nx, ny, pdev, 
     +   logf, comment, size, hann, ops, twopass, dofqav, dotitle)
      call chkinp (xaxis, yaxis, xmin, xmax, ymin, ymax, dayav,
     +   dodoub, dowave, doave, dovec, dorms, dointer, doperr,
     +   dowrap, hann, xrtest, yrtest)
c
c  Open the log file and write some messages
c
      call logfop (logf, comment, doave, dovec, doall, doflag)
c
c Read through data set accumulating descriptive information or use
c variables of first integration to guess at what's in the file
c
      maxbuf2 = membuf()
      if (twopass) then
        call uvdes (doflag, doall, dofqav, maxant, maxbase, maxchan,
     +    basmsk, polmsk, data, goodf, nfiles, npols, nbases, npnts,
     +    baseday, dayoff)
        if(dodoub)npnts = 2*npnts
        maxbuf2 = max(2*(npnts+1),maxbuf2)
      else
        call uvfish (nfiles, npols, nbases, baseday, dayoff)
      end if
      if (nbases.eq.0 .or. npols.eq.0) call bug ('f',
     +  'There were no selected baselines or polarizations')
c
c Allocate all the memory we can have.
c
      call memalloc (ip, maxbuf2, 'r')
c
c Chop up the plot buffer according to what we have learned or think
c is in the file.
c
      call chopup (twopass, maxbuf2, maxbase, maxbase2, maxpol, maxfile,
     +   nfiles, npols, nbases, dobase, doavall, dosymb, docol, dorms,
     +   pl1dim, pl2dim, pl3dim, pl4dim, maxpnt, xo, yo, elo, eho)
c
c  Initialize counters  and accumulators
c
      call init (maxbase, maxpol, nsum, xsumr, xsumi, xsumsqr, xsumsqi, 
     +           ysumr, ysumi, ysumsqr, ysumsqi)
c
c Loop over visibility files 
c
      if (twopass) then
        call output (' ')
        call output ('Pass 2: transfer to plot buffers')
      end if
      call output (' ')
      do while (uvdatopn(lin).and..not.allfull.and.plfidx.lt.maxfile)
c
c Track change of variables that cause us to reset the accumulators
c
        call track (lin, vupd)
c
c Set file plot buffer index.  Don't maintain separate files unless 
c plotting them with different symbols or colours
c
        ifile = ifile + 1
        plfidx = 1
        if (dosymb .or. docol) plfidx = ifile
c
c Now loop around processing the visibilities in this file
c
        call uvdatgta ('name', in)
        call logwrite (' ', more)
        str = itoaf (ifile)
	call logwrite ('File # '//str//' = '//in, more)
c
c Read first visibility
c
        call getdat (preamble, data, goodf, maxchan, nread,
     *               dofqav, doflag, doall)
c
c Make plot title when we get some data from a file
c
        if (title.eq.' ' .and. nread.ne.0) 
     +    call mtitle (lin, nread, dosrc, dayav, tunit, nfiles, title)
c
        do while (nread.ne.0 .and. .not.allfull)
c
c Is there some data we want in this visibility ?
c
          call goodat ( nread, goodf, nkeep)
          if (nkeep.eq.0) goto 950
c
          ivis = ivis + 1
          day = preamble(3) + 0.5
c
c Are we at the end of the averaging interval ?
c
          if (doave) then
            call endave (ivis, vupd, dayav, day, baseday, reset)
            if (reset) then
c
c Averaging over; work out averaged quantities and dump to plot buffer
c If we cross a file boundary, and are distinguishing between files, 
c label point as if from previous file; this is fairly arbitrary
c
              jfile = plfidx
              if ((dosymb .or. docol) .and. ifile.ne.ofile .and.
     +            ifile.ne.1) jfile = jfile - 1
              call avdump (dorms, dovec, dobase, dodoub, doavall,
     +           nbases, npols, pl1dim, pl2dim, pl3dim, pl4dim, 
     +           maxpnt, maxbase, maxpol, maxfile, buffer(ip),
     +           npts, xo, yo, elo, eho, xaxis, xrtest, xmin, xmax, 
     +           yaxis, yrtest, ymin, ymax, nsum, xsumr, xsumsqr, xsumi, 
     +           xsumsqi, ysumr, ysumsqr, ysumi, ysumsqi, xave, yave, 
     +           xsig, ysig, plpts, inc, jfile)
c
c Reinitialize accumulators for next averaging period
c
              call init (maxbase, maxpol, nsum, xsumr, xsumi, xsumsqr,
     +           xsumsqi, ysumr, ysumi, ysumsqr, ysumsqi)
            end if
          end if
c
c Assign this visibility baseline plot and statistics numbers and
c a polarization number
c
          call baspolid (doave, doavall, preamble(4), dobase, maxant,
     +       maxbase, pl2dim, pl3dim, bwarn, basmsk, polmsk, nbases,
     +       npols, plbidx, stbidx, pidx, a1a2, skip)
          if (skip) goto 900
c
c Get info from preamble
c
          if (dowave) then
            call getwvl (donano, preamble, u, v, uvdist, uvpa)
            call uvinfo (lin, 'sfreq', freq)
          endif
c
c Fish out the lst if required.
c
	  if (xaxis.eq.'lst'.or.yaxis.eq.'lst')then
	    call getlst(lin,lst)
	  endif
c
c Fish out hour angle if required.
c
          if (xaxis.eq.'hangle' .or. xaxis.eq.'dhangle' .or.
     +        yaxis.eq.'hangle' .or. yaxis.eq.'dhangle') then
	    call uvrdvrd(lin,'ra',dtemp,0.d0)
	    call uvrdvrd(lin,'obsra',ra,dtemp)
	    call getlst(lin,lst)
            ha = (lst - ra) 
            if (ha.gt.dpi) then
              ha = ha - 2.0d0*dpi
            else if (ha.lt.-dpi) then
              ha = ha + 2.0*dpi
            end if
            ha = ha * 12.0d0*3600.0d0/dpi
          endif
c
c  Fish out jyperk or rms if required.
c
	  if(xaxis.eq.'jyperk'.or.yaxis.eq.'jyperk')then
	    call uvDatgtr('jyperk',jyperk)
	  endif
	  if(xaxis.eq.'rms'.or.yaxis.eq.'rms')then
	    call uvDatgtr('variance',rms)
	  endif
c
c  Fish out the parallactic angle,azimuth or elevation
c  if required.
c
	  if(xaxis.eq.'parang'.or.yaxis.eq.'parang'.or.
     +       xaxis.eq.'az'.or.yaxis.eq.'az'.or. 
     +       xaxis.eq.'el'.or.yaxis.eq.'el'.or.
     +	     xaxis.eq.'airmass'.or.yaxis.eq.'airmass')then
	    call uvrdvrd(lin,'ra',dtemp,0.d0)
            call uvrdvrd (lin, 'obsra', ra, dtemp)
	    call uvrdvrd(lin,'dec',dtemp,0.d0)
	    call uvrdvrd(lin, 'obsdec',dec,dtemp)
	    call getlst(lin, lst)
	    call getlat(lin, lat)
	    if(xaxis.eq.'parang'.or.yaxis.eq.'parang')
     +	      call parang(ra,dec,lst,lat,paran)
	    if(xaxis.eq.'az'.or.yaxis.eq.'az'.or.
     +	       xaxis.eq.'el'.or.yaxis.eq.'el'.or.
     +	       xaxis.eq.'airmass'.or.yaxis.eq.'airmass')
     +	      call azel(ra,dec,lst,lat,az,el)
	  endif
c
          fday = day - dayoff
c
c Loop over channels for this visibility, accumulating, or 
c filling the plot buffer
c
          j = 0
          do while (j.lt.nread)
            j = j + 1
            if ( goodf(j)) then
c
c Set x and y values
c
              call setval (xaxis, ha, u, v, uvdist, uvpa, fday,
     +                     paran, lst, az, el, jyperk, rms,
     +			   data(j), j, freq, xvalr, xgood)
              call setval (yaxis, ha, u, v, uvdist, uvpa, fday, 
     +                     paran, lst, az, el, jyperk, rms,
     +                     data(j), j, freq, yvalr, ygood)
              if (xgood .and. ygood) then
                if (doave) then
c
c Accumulate in averaging buffers
c
                  call accum (dovec, xvalr, yvalr, data(j),
     +               xsumr(stbidx,pidx), xsumsqr(stbidx,pidx),
     +               xsumi(stbidx,pidx), xsumsqi(stbidx,pidx),
     +               ysumr(stbidx,pidx), ysumsqr(stbidx,pidx),
     +               ysumi(stbidx,pidx), ysumsqi(stbidx,pidx),
     +               nsum(stbidx,pidx))
                else
c
c Put points into plot buffer
c
                  if (npts(plbidx,pidx,plfidx).lt.maxpnt) then
                    call bufput (false, pl1dim, pl2dim, pl3dim, pl4dim,
     +                 maxbase, maxpol, maxfile, plbidx, pidx, plfidx,
     +                 xrtest, yrtest, xmin, xmax, ymin, ymax, xvalr,
     +                 yvalr, 0.0, 0.0, npts, buffer(ip), xo, yo, elo,
     +                 eho, plpts, inc)
c
c Add -u and -v if requested
c
                    if (npts(plbidx,pidx,plfidx).lt.maxpnt .and.
     +                  dodoub) then
                      if (xaxis.eq.'uc'.or.xaxis.eq.'vc') 
     +                   xvalr = -xvalr
                      if (yaxis.eq.'uc'.or.yaxis.eq.'vc') 
     +                   yvalr = -yvalr
c
                      call bufput (false, pl1dim, pl2dim, pl3dim, 
     +                   pl4dim, maxbase, maxpol, maxfile, plbidx, 
     +                   pidx, plfidx, xrtest, yrtest, xmin, xmax, 
     +                   ymin, ymax, xvalr,yvalr, 0.0, 0.0, npts, 
     +                   buffer(ip), xo, yo, elo, eho, plpts, inc)
                    end if
                  end if
                end if
              end if
            end if
          end do
c
c See if we have filled up ALL of the allocated plot buffer for this file
c and go on to the next file if plotting files with different symbols
c
900       call fullup (maxbase, pl2dim, pl3dim, pl4dim, maxpnt, maxpol,
     +                 npts(1,1,plfidx), ifile, allfull)
c
c Read next visibility
c
950       if (.not.allfull) call getdat (preamble, data, goodf, 
     *        maxchan, nread, dofqav, doflag, doall)
        end do
c
c Issue a message if any (but not all) of the baseline/polarization 
c plot buffers were filled up and close the current file
c
        if (.not.allfull)
     +    call pntful (dobase, pl2dim, pl3dim, pl4dim, maxpnt, maxbase,
     +       maxpol, maxfile, ifile, plfidx, a1a2, npts)
        call uvdatcls
c
c Flush accumulators to plot buffers for last file; do it here
c so can get correct numbers for TELLUSE
c
      if (doave .and. ifile.eq.nfiles .and. .not.allfull)
     +  call avdump (dorms, dovec, dobase, dodoub, doavall, nbases, 
     +     npols, pl1dim, pl2dim, pl3dim, pl4dim, maxpnt, maxbase, 
     +     maxpol, maxfile, buffer(ip), npts, xo, yo, elo, eho,
     +     xaxis, xrtest, xmin, xmax, yaxis, yrtest, ymin, ymax,
     +     nsum, xsumr, xsumsqr, xsumi, xsumsqi, ysumr, ysumsqr, ysumi,
     +     ysumsqi, xave, yave, xsig, ysig, plpts, inc, plfidx)
c
c Tell user some numbers for each file if putting different files into
c separate locations in plot buffer
c
        if (pl4dim.gt.1) then
          call telluse (ivis, plfidx, dobase, maxbase, maxpol,
     +      pl2dim, pl3dim, pl4dim, npts(1,1,plfidx), a1a2, none)
          allfull = .false.
        end if
c
        ofile = ifile
      end do
c
c Tell user number of points plotted if all files with same symbol
c else warn if not all files could be accomodated in storage
c
      if (pl4dim.gt.1) then
        if (nfiles.gt.maxfile) then
          call output (' ')
          call bug ('w', 
     +       'Not all the input files could be read; increase MAXFILE')
        end if
      else
        call telluse (ivis, ifile, dobase, maxbase, maxpol, pl2dim,
     +                pl3dim, pl4dim, npts(1,1,1), a1a2, none)
      end if
c
c  Optionally Hanning smooth data
c
      if (hann.ge.3)
     +   call hannit (hann, coeffs, work, pl1dim, pl2dim, pl3dim,
     +     pl4dim, maxbase, maxpol, maxfile, nbases, npols, npts, 
     +     buffer(ip), yo)
c
c Plot the plots
c
      if (.not.none)
     +   call plotit (dointer, doave, dorms, dobase, dolog, dozero,
     +     doequal, donano, doxind, doyind, doperr, dowrap, dosymb, 
     +     dodots, title, xaxis, yaxis, xmin, xmax, ymin, ymax, xxmin,
     +     xxmax, yymin, yymax, pdev, pl1dim, pl2dim, pl3dim, pl4dim,
     +     maxbase, maxpol, maxfile, nbases, npols, npts, buffer(ip), 
     +     xo, yo, elo, eho, nx, ny, a1a2, order, size, polmsk, 
     +     doavall, docol, dotitle)
c
      call logclose
      call memfree (ip, maxbuf2, 'r')
c
      end
c
c
      subroutine accum  (dovec, xvalr, yvalr, cval, xsumr, xsumsqr,
     +   xsumi, xsumsqi, ysumr, ysumsqr, ysumi, ysumsqi, nsum)
c-----------------------------------------------------------------------
c     Accumulate sums when averaging over a time interval
c
c  Input:
c    dovec        True for vector averaging.  If vector averaging
c                 is requested, then the complex quantity is used.
c                 Otherwise, the scalar value is used; x and y
c    xvalr        Potential real value of the X-axis
c    yvalr        Potential real value of the Y-axis
c    cval         Potential complex value of either the X or Y axis
c  Input/output:
c    x,ysum,sqr   Sum and sum of square of real quantity
c    x,ysum,sqi   Sum and sum of square of imaginary quantity
c    nsum         Number of points accumulated so far this interval
c
c-----------------------------------------------------------------------
      implicit none
c
      logical dovec(2)
      integer nsum
      real xvalr, yvalr, xsumr, ysumr, xsumsqr, ysumsqr, 
     +xsumi, ysumi, xsumsqi, ysumsqi
      complex cval
c-----------------------------------------------------------------------
      nsum = nsum + 1
      call accum2(dovec(1), cval, xvalr, xsumr, xsumi, xsumsqr, xsumsqi)
      call accum2(dovec(2), cval, yvalr, ysumr, ysumi, ysumsqr, ysumsqi)
c
      end    
c
c
      subroutine accum2 (dovec, cval, rval, sumr, sumi, sumsqr, sumsqi)
c-----------------------------------------------------------------------
c     Update averaging sums
c
c  Input:
c    dovec     True for vector averaging, else scalar
c    cval      Complex value
c    rval      Real value
c  Input/Output:
c    sumr      Real sum (vector and scalar)
c    sumi      Imaginary sum (vector)
c    sumsqr    Sum of real squares (vector and imaginary)
c    sumsqi    Imaginary sum of squares (vector)
c
c-----------------------------------------------------------------------
      implicit none
c
      logical dovec
      complex cval
      real rval, sumr, sumi, sumsqr, sumsqi
c-----------------------------------------------------------------------
      if (dovec) then
        sumr = sumr +  real(cval)
        sumi = sumi + aimag(cval)
        sumsqr = sumsqr +  real(cval)**2
        sumsqi = sumsqi + aimag(cval)**2
      else
        sumr = sumr + rval
        sumsqr = sumsqr + rval**2
      end if        
c
      end
c
c
      subroutine arrdec (n, aline, ilen, arr, ok)
c-----------------------------------------------------------------------
c     Decode white space delimitered string into an array
c
c     Input
c       n        Number of numbers to decode from string. 
c       aline    Input string
c       ilen     Length of string with trailing blanks ignored
c     Output
c       arr      Array of numbers
c
c-----------------------------------------------------------------------
      implicit none
c
      character*(*) aline
      integer ilen, n
      real arr(*)
      logical ok
cc
      double precision val
      integer ib, j
c-----------------------------------------------------------------------
      if (ilen.gt.0) then
        ib = 1
        j = 1
        ok = .true.
c
        do while (j.le.n .and. ok)
          call getval (ilen, aline, ib, val, ok)
          arr(j) = val
          j = j + 1
        end do
      else
        ok = .false.
      end if
c
      end
c
c
      subroutine avdump (dorms, dovec, dobase, dodoub, doavall, nbases,
     +   npols, pl1dim, pl2dim, pl3dim, pl4dim, maxpnt, maxbase, 
     +   maxpol, maxfile, buffer, npts, xo, yo, elo, eho, xaxis,
     +   xrtest, xmin, xmax, yaxis, yrtest, ymin, ymax, nsum, xsumr,
     +   xsumsqr, xsumi, xsumsqi, ysumr, ysumsqr, ysumi, ysumsqi, 
     +   xave, yave, xsig, ysig, plpts, inc, plfidx)
c-----------------------------------------------------------------------
c     The end of an averaging interval has been reached.  Work out
c     the averaged qantities and dump them to the plot buffer.
c
c  Input:
c    dorms         True to plot error bars when averaging; x and y
c	 	   dorms(3) = false for standard deviation, true for 
c		   standard deviation of the mean
c    dovec         If true using vector averaging; x and y
c    dobase        If true each baseline on a separate sub-plot
c    dodoub        If true plot -u and -v as well as u and v
c    doavall       Everything on subplot averaged together
c    nbases        Number of baselines encountered in file so far
c    npols         Number of polarizations encountered in fiel so far
c    pl*dim        * = 1-> 4.  DImensions of  BUFFER
c    maxpnt        Maximum number of points allowed per allocated
c                  baseline, polarization and file.
c
c    maxbase       Size of first dimension of statistics arrays
c                  and plot counters
c    maxpol        Size of second dimension of statistics arrays
c
c    maxpol        Size of second dimension of plot counter arrays
c    maxfile       Size of third dimension of plot counter arrays
c
c    x,yo          Offsets to the start locations for X and Y in BUFFER
c    el,ho         Offsets to the start locations for X_lo, X_hi, Y_lo,
c                  and Y_hi errors in BUFFER.  These will have sensible
c                  values only if DORMS is true, so WATCH OUT !
c    x,yaxis       Axis types
c    x,yrtest      True if user specifed x and y plot ranges
c                  and interactive plot mode off.
c    x,ymin,max    x and y plot extrema given by user
c
c    nsum          Number of points accumulated in the current interval 
c                  for each baseline and polarization combination
c    x,ysumr       Sums of real quantities
c    x,ysumsqr     Sums of squares of real quantities
c    x,ysumi       Sums of imaginary quantities
c    x,ysumsqi     Sums of squares of imaginary quantities
c    inc           Plot every INCth point after final data selection
c    plfidx        Number of current file
c  Input/output
c    npts          Number of points in each plot buffer
c    buffer        Plot buffer
c  Input/output (work space):
c    x,yave        Averaged quantities to plot
c    x,ysig        Standard deviation on averaged point
c    plpts         Used in picking out every INCth point from plot arrays
c
c-----------------------------------------------------------------------
      implicit none
c
      logical doavall, dorms(3), dovec(2), dobase, dodoub, yrtest, 
     +  xrtest
      integer pl1dim, pl2dim, pl3dim, pl4dim, maxbase, maxpol,
     +  maxfile, nbases, npols
c
      integer npts(maxbase,maxpol,maxfile), 
     +  plpts(maxbase,maxpol,maxfile), nsum(maxbase,maxpol)
      real buffer(pl1dim,pl2dim,pl3dim,pl4dim),
     +  xsumr(maxbase,maxpol), ysumr(maxbase,maxpol), 
     +  xsumi(maxbase,maxpol), ysumi(maxbase,maxpol),
     +  xsumsqr(maxbase,maxpol), ysumsqr(maxbase,maxpol), 
     +  xsumsqi(maxbase,maxpol), ysumsqi(maxbase,maxpol),
     +  xave(maxbase,maxpol), yave(maxbase,maxpol), 
     +  xsig(maxbase,maxpol), ysig(maxbase,maxpol)
c
      integer xo, yo, elo(2), eho(2), inc, plfidx, maxpnt
      real xmin, xmax, ymin, ymax
      character xaxis*(*), yaxis*(*)
cc
      integer i, j, plbidx, nb, np
c-------------------------------------------------------------------------
c
c Average sums for all the baselines for each polarization
c
      nb = nbases
      if (.not.dobase .and. doavall) nb = 1
c
      np = npols
      if (doavall) np = 1
c
      do i = 1, np
        call avquant(dorms(1), dorms(3), dovec(1), xaxis, nb, nsum(1,i),
     +     xsumr(1,i), xsumsqr(1,i), xsumi(1,i), xsumsqi(1,i), 
     +     xave(1,i), xsig(1,i))
        call avquant(dorms(2), dorms(3), dovec(2), yaxis, nb, nsum(1,i),
     +     ysumr(1,i), ysumsqr(1,i), ysumi(1,i), ysumsqi(1,i), 
     +     yave(1,i), ysig(1,i))
      end do
c
c Now dump averages to plot buffer.  Put all the points on the
c one plot if requested, else separate plots.
c
      do j = 1, np
        do i = 1, nb
          if (nsum(i,j).gt.0) then
            plbidx = i
            if (.not.dobase) plbidx = 1
c
            if (npts(plbidx,j,plfidx).lt.maxpnt) then
              call bufput (dorms, pl1dim, pl2dim, pl3dim, pl4dim, 
     +           maxbase, maxpol, maxfile, plbidx, j, plfidx, 
     +           xrtest, yrtest, xmin, xmax, ymin, ymax, xave(i,j), 
     +           yave(i,j), xsig(i,j), ysig(i,j), npts, buffer,
     +           xo, yo, elo, eho, plpts, inc)
c
c User may want -u and/or -v as well.
c 
              if (npts(plbidx,j,plfidx).lt.maxpnt .and.
     +            dodoub) then
                if (xaxis.eq.'uc' .or. xaxis.eq.'vc') 
     +            xave(i,j) = -xave(i,j)
                if (yaxis.eq.'uc' .or. yaxis.eq.'vc') 
     +            yave(i,j) = -yave(i,j)
c
                call bufput (dorms, pl1dim, pl2dim, pl3dim, pl4dim, 
     +            maxbase, maxpol, maxfile, plbidx, j, plfidx,
     +            xrtest, yrtest, xmin, xmax, ymin, ymax, xave(i,j), 
     +            yave(i,j), xsig(i,j), ysig(i,j), npts, buffer,
     +            xo, yo, elo, eho, plpts, inc)
              end if
            end if
          end if
        end do
      end do
c
      end
c
c
      subroutine avquant (dorms, errmean, dovec, axis, nbasst, nsum, 
     +                    sumr, sumsqr, sumi, sumsqi, ave, sig)
c-----------------------------------------------------------------------
c     Work out the average and rms from the solution interval
c
c  Input:
c    dorms     If true work out standard deviation
c    errmean   If true, work out standard deviation in the mean
c    dovec     If true using vector averaging
c    axis      Type of y axis
c    nbasst    Number of baselines to work out averages for
c    nsum      Number of points accumulated in the current interval for
c              each baseline
c    sumr      Sum of real quantity
c    sumsqr    Sum of squares of real quantity
c    sumi      Sum of imaginary quantity
c    sumsqi    Sum of squares of imaginary quantity
c Output:
c    ave       Averaged value to plot
c    sig       Standard deviation associated with averaged quantity
c
c-----------------------------------------------------------------------
      implicit none
c
      logical dorms, errmean, dovec
      character axis*(*)
      integer nbasst, nsum(nbasst)
      real sumr(nbasst), sumi(nbasst), sumsqr(nbasst), sumsqi(nbasst),
     +  ave(nbasst), sig(nbasst)
cc
      real var
      integer i
c-----------------------------------------------------------------------
      if (dovec) then
c
c Vector averaging
c
        do i = 1, nbasst
c
c Loop over each baseline.  May be no points for some baselines
c in some time intervals
c
          if (nsum(i).gt.0) then
            call setvl2 (axis, cmplx(sumr(i)/nsum(i),sumi(i)/nsum(i)),
     +                   ave(i))
c
c No errors for vector averaging yet
c
            if (dorms) then
              sig(i) = 0.0
            end if
          end if
        end do
      else
c
c Scalar averaging
c
        do i = 1, nbasst
          if (nsum(i).gt.0) then
            ave(i) = sumr(i) / nsum(i)
c
            if (dorms) then
              var = (sumsqr(i)/nsum(i)) - ave(i)**2
              if (var.gt.0.0) then
                sig(i) = sqrt(var)
              else
                sig(i) = 0.0
              end if
              if (errmean) sig(i) = sig(i) / sqrt(real(nsum(i)))
            end if
          end if
        end do
      end if
c
      end
c
c
      subroutine baspolid (doave, doavall, baseln, dobase, maxant, 
     +    maxbase, pl2dim, pl3dim, bwarn, basmsk, polmsk, nbases,
     +    npols, plbidx, stbidx, pidx, a1a2, skip)
c-----------------------------------------------------------------------
c     See if this baseline has already been encountered.  For single
c     baseline plots,  give it a new plot number.  For all baselines
c     together, the plot number is always one.  The averaging statistics
c     are accumulated for different baselines and polarizations
c     separately unless DOAVALL is true, whereupon everything on 
c     each subplot is lumped in together.
c
c  Input:
c    doave    True if averaging
c    doavall  True if averaging everything on the sub-plot together
c    baseln   Baseline number (256*ia1 + ia2)
c    dobase   True to plot each baseline on a separate plot
c    maxant   Maximum number of antennas allowed for accumulation
c             of statistics over the averaging interval
c    maxbase  Maximum number of baselines corresponding to MAXANT
c    pl2dim   Dimension of baseline index in plotting BUFFER
c    pl3dim   Dimension of polarization index in plotting BUFFER
c  Input/output:
c    bwarn    If true, the user has been warned already about trying
c             to plot more than the allowed number of baselines
c    basmsk   Map between baseline and plot index.  Each baseline has
c             a unique index for this array.  The value of the 
c             array for the baseline gives its plot index.  If the
c             array element is zero, this baseline has not yet been 
c             assigned a plot number so better give it one.
c    polmsk   Map between polarization number (-8 -> +4) and polarization
c             number (1 -> maxpol).  The value of the array is the
c             number for that polarization.  Like basmsk
c    nbases   Number of different baselines encountered so far
c    npols    Number of different polarizations encountered so far
c  Output:
c    plbidx   Index into plot buffer for this baseline
c    stbidx   Index into statistics buffers for this baseline
c    pidx     Plot/stats index for this polarization
c    a1a2     The two antennas for each baseline plot number
c    skip     If true, then we ran out of room for more baselines
c
c-----------------------------------------------------------------------
      implicit none
c
      double precision baseln
      logical doave, doavall, dobase, skip, bwarn(2)
      integer maxbase, maxant, nbases, npols, plbidx, stbidx,
     +  pidx, polmsk(-8:4), a1a2(maxbase,2), basmsk(maxant+maxbase), 
     +  pl2dim, pl3dim
cc
      integer idx, ia1, ia2
      character msg*80
c-----------------------------------------------------------------------
      skip = .false.
c
c Find baseline index
c
      call basant (baseln, ia1, ia2)
      idx = ia1 + ia2*(ia2-1)/2
c
      if (basmsk(idx).eq.0) then
c
c New baseline
c
        if (dobase .and. nbases.eq.pl2dim) then
c
c We have run out of space for this baseline.   Tell user.
c
          if (.not.bwarn(1)) then
            call output (' ')
            write (msg, 200) pl2dim
200         format ('Max. no. of baselines can plot singly (', 
     +              i2, ') has been reached')
            call bug ('w', msg)
            call output (' ')
            bwarn(1) = .true.
          end if
          skip = .true.
          return
        else
          nbases = nbases + 1
        end if
c
c Assign new slot in the baseline mask
c
        basmsk(idx) = nbases
c
c When plotting separate baselines, keep track of antennas used for
c each baseline number; used for plot labelling and messages.
c
        if (dobase) then
          a1a2(nbases,1) = ia1
          a1a2(nbases,2) = ia2
        end if
      end if
c
c Now assign indices into plotting and statistics arrays for this baseline
c
      if (dobase) then
c
c Baselines plotted separately
c
        plbidx = basmsk(idx)
c
        if (doave) then
c
c Baselines averaged separately
c
          stbidx = plbidx
        else
c
c Stats index is irrelevant since we are not averaging; it won't
c be used, but set it to something nasty in case it is.  This
c will find it !!
c
          stbidx = -1
        end if
      else
c
c Baselines plotted together
c
        plbidx = 1
        if (doave) then
          if (doavall) then
c
c Baselines averaged together
c
            stbidx = 1
          else
c
c Baselines averaged spearately
c
            stbidx = basmsk(idx)
          end if
        else
c
c No averaging
c
          stbidx = -1
        end if
      end if
c
c Find new polarization
c
      call uvdatgti ('pol', idx)
      if (polmsk(idx).eq.0) then
        if (.not.doavall .and. npols.eq.pl3dim) then
          if (.not.bwarn(2)) then
            call output (' ')
            write (msg, 300) pl3dim
300         format ('Max. no. of polarizations can plot singly (',
     +               i2, ') has been reached.')
            call bug ('w', msg)
            call output (' ')
            bwarn(2) = .true.
          end if
          skip = .true.
          return
        else
          npols = npols + 1
        end if
c
c Assign new slot in the polarization mask
c
        polmsk(idx) = npols
      end if
c
c Assign polarization index.  Same for plotting and statistics arrays
c
      if (doavall) then
c
c Averaging and plotting polarizations together
c
        pidx = 1
      else
c
c Polarizations plotted and averaged separately
c
        pidx = polmsk(idx)
      end if
c
      end 
c
c
      subroutine bufput (dorms, pl1dim, pl2dim, pl3dim, pl4dim, 
     +   maxbase, maxpol, maxfile, plbidx, plpidx, plfidx, xrtest, 
     +   yrtest, xmin, xmax, ymin, ymax, x, y, xsig, ysig, npts,
     +   buffer, xo, yo, elo, eho, plpts, inc)
c-----------------------------------------------------------------------
c     Test the x,y coordinate for being in the user specified range, 
c     if there is one, and put it in the plot buffer if wanted.
c
c  Input:
c    dorms         If true, user wants error bars; x and y
c    maxbase       Size of first dimension of NPTS and PLPTS
c    maxpol        Size of second dimension of NPTS and PLPTS
c    pl*dim        Sizes of 4 BUFFER dimensions
c    plbidx        The BUFFER index into which this baseline goes.
c    plpidx        The BUFFER index into which this polarization goes
c    plfidx        The BUFFER index into which this file goes
c    x,yo          Offsets to the start locations for X and Y in BUFFER
c    el,ho         Offsets to the start locations for X_lo, X_hi, Y_lo
c                  and Y_hi errors in BUFFER.  These will have sensible
c                  values only if DORMS is true, so WATCH OUT !
c    x,yrtest      True if user specified X and Y plot ranges
c    x,ymin,max    x and y plot extrema given by user
c    x,y           x and y values for this datum
c    x,ysig        standard deviation on averaged x and y points
c  Input/output
c    npts          Number of points in each plot buffer
c    buffer        Plot buffer
c    plpts         Used in picking out every INCth point from plot arrays
c    inc           Plot every INCth point after final data selection
c
c-----------------------------------------------------------------------
      implicit none
c
      logical xrtest, yrtest, dorms(2)
      integer pl1dim, pl2dim, pl3dim, pl4dim, maxbase, maxpol, maxfile,
     +  npts(maxbase,maxpol,maxfile), plpts(maxbase,maxpol,maxfile),
     +  xo, yo, elo(2), eho(2), plbidx, plpidx, plfidx, inc
      real xmin, xmax, ymin, ymax, x, y, xsig, ysig, 
     +  buffer(pl1dim,pl2dim,pl3dim,pl4dim)
cc
      integer n
c-----------------------------------------------------------------------
c
c Make sure point in wanted X and Y range
c
      if ( ((xrtest.and.x.ge.xmin.and.x.le.xmax).or..not.xrtest)
     +                           .and.
     +     ((yrtest.and.y.ge.ymin.and.y.le.ymax).or..not.yrtest))
     + then
c 
c Fill plot buffers, picking out every INCth point selected
c
        plpts(plbidx,plpidx,plfidx) = plpts(plbidx,plpidx,plfidx)+1
        if (plpts(plbidx,plpidx,plfidx).eq.inc+1 .or. inc.eq.1) 
     +    plpts(plbidx,plpidx,plfidx) = 1
c
        if (plpts(plbidx,plpidx,plfidx).eq.1) then
          npts(plbidx,plpidx,plfidx) = npts(plbidx,plpidx,plfidx)+1
          n = npts(plbidx,plpidx,plfidx) 
c
          buffer(xo+n,plbidx,plpidx,plfidx) = x
          buffer(yo+n,plbidx,plpidx,plfidx) = y
c
          if (dorms(1)) then
            buffer(elo(1)+n,plbidx,plpidx,plfidx) = x - xsig
            buffer(eho(1)+n,plbidx,plpidx,plfidx) = x + xsig
          end if
          if (dorms(2)) then
            buffer(elo(2)+n,plbidx,plpidx,plfidx) = y - ysig
            buffer(eho(2)+n,plbidx,plpidx,plfidx) = y + ysig
          end if
        end if
      end if
c
      end
c
c
      subroutine chkinp (xaxis, yaxis, xmin, xmax, ymin, ymax, dayav,
     +   dodoub, dowave, doave, dovec, dorms, dointer, doperr, dowrap,
     +   hann, xrtest, yrtest)
c-----------------------------------------------------------------------
c     Check the validity of some of the inputs
c
c   Input:
c     x,yaxis     Axis types
c     x,ymin,max  User specified data extrema for plots
c     dayav       Length of averaging period in days
c     dointer     True for interactive plotting
c   Input/output:
c     dovec       Vector averaging, else scalar averaging; x and y
c                 User just asks for vector averaging, and uvplt tries
c                 to work out if it can do this on both axes or just one.
c     dorms       True to plot error bars when averaging; x and y
c		  dorms(3) = false for standard deviation, true for 
c		  standard deviation of the mean
c     doperr      If true then the automatically determined Y window
c                 includes the ends of the error bars
c     dowrap      Do not unwrap phase
c   Output:
c     dodoub      PLot -u and/or -v on plot
c     dowave      Either X axis or Y axis needs infor from preamble
c     doave       Averaging requested
c     hann        Apply hanning smoothing
c     x,yrtest    If true, then only accumulate in the plot buffer's
c                 X or Y points that are within the user specified ranges
c                 This allows less wastage of space, as when interactive
c                 mode is not selected and there is no plot window 
c                 redefinition, the points outside the given range
c                 are never going to be looked at
c
c-----------------------------------------------------------------------
      implicit none
c
      integer hann
      character*(*) xaxis, yaxis
      double precision dayav
      real xmin, xmax, ymin, ymax
      logical dointer, dodoub, dowave, doave, dovec(2), xrtest, yrtest,
     +  dorms(3), doperr, dowrap
c-----------------------------------------------------------------------
c
c Hanning only useful for certain y-axis settings
c
      if (hann.gt.1 .and. yaxis.ne.'amplitude' .and. yaxis.ne.'phase'
     +    .and. yaxis.ne.'real' .and. yaxis.ne.'imag') then
        hann = 1
        call bug ('w', 'Hanning smoothing not useful for this y-axis')
      end if
c
c Signify may want -u and/or -v as well
c
      dodoub = xaxis.eq.'uc' .or. xaxis.eq.'vc' .or.
     +         yaxis.eq.'uc' .or. yaxis.eq.'vc'
c
c Switch to compute u and v related variables if needed
c
      dowave = .false.
      if (xaxis.eq.'uc' .or. xaxis.eq.'vc' .or. xaxis.eq.'uu' .or.
     +    xaxis.eq.'vv' .or. xaxis.eq.'uvdistance' .or. 
     +    xaxis.eq.'uvangle' .or. xaxis.eq.'hangle' .or. 
     +    xaxis.eq.'dhangle' .or. yaxis.eq.'uc' .or. yaxis.eq.'vc' .or.
     +    yaxis.eq.'uu' .or. yaxis.eq.'vv' .or.
     +    yaxis.eq.'uvdistance' .or. yaxis.eq.'uvangle' .or.
     +    yaxis.eq.'hangle' .or. yaxis.eq.'dhangle') dowave = .true.
c
c Check averaging switches
c
      doave = (dayav.gt.0.0)
      if (.not.doave) then
        dorms(1) = .false.
        dorms(2) = .false.
      end if
c
c Check for sensible axes when asking for errors
c
      if (dorms(1) .and. xaxis.ne.'amplitude'.and. xaxis.ne.'real'
     +    .and. xaxis.ne.'imag' .and. xaxis.ne.'phase') then
        call bug ('w', 'Errors not useful for this x-axis')
        dorms(1) = .false.
      end if
      if (dorms(2) .and. yaxis.ne.'amplitude'.and. yaxis.ne.'real'
     +    .and. yaxis.ne.'imag' .and. yaxis.ne.'phase') then
        call bug ('w', 'Errors not useful for this y-axis')
        dorms(2) = .false.
      end if
c
      if (doave.and.dovec(1) .and. xaxis.ne.'amplitude' .and.
     +    xaxis.ne.'phase' .and.xaxis.ne.'real' .and. 
     +    xaxis.ne.'imag') dovec(1) = .false.
      if (doave.and.dovec(2) .and. yaxis.ne.'amplitude' .and. 
     +    yaxis.ne.'phase' .and.yaxis.ne.'real' .and.
     +    yaxis.ne.'imag') dovec(2) = .false.
c
      if (doave.and..not.dovec(1) .and. xaxis.eq.'phase') call bug ('i',
     +  'Scalar averaging for phase useless if wrap around occurs')
      if (doave.and..not.dovec(2) .and. yaxis.eq.'phase') call bug ('i',
     +  'Scalar averaging for phase useless if wrap around occurs')
c
      if (doave .and. dovec(1) .and. dorms(1)) then
        dorms(1) = .false.
        call bug ('w', 
     +   'x-error bars not yet implimented for vector averaging')
      end if
      if (doave .and. dovec(2) .and. dorms(2)) then
        dorms(2) = .false.
        call bug ('w', 
     +   'y-error bars not yet implimented for vector averaging')
      end if
      if (.not.dorms(1) .and. .not.dorms(2)) doperr = .false.
c
c  Set switches for case when interactive mode not selected so that
c  the plot buffer will not be wasted with points outside of the 
c  plot x,y-ranges.  
c
      xrtest = .false.
      if (.not.dointer .and. (xmin.ne.0.0.or.xmax.ne.0.0)) 
     +   xrtest = .true.
      yrtest = .false.
      if (.not.dointer .and. (ymin.ne.0.0.or.ymax.ne.0.0)) 
     +   yrtest = .true.
c
c Do a fudge here.  Unwrapping is done in PLOTIT long after range
c selection, so that points that might be selected if range selection 
c is applied after unwrapping may have been already rejected.  Just 
c prevent it from ditching points according to range selection if 
c unwrapping. All this means is that a but of buffer space is lost.
c 
      if (.not.dowrap) then
        xrtest = .false.
        yrtest = .false.
      end if
c
      end
c
c
      subroutine chopup (twopass, maxbuf2, maxbase, maxbase2, maxpol,
     +   maxfile, nfiles, npols, nbases, dobase, doavall, dosymb, 
     +   docol, dorms,pl1dim, pl2dim, pl3dim, pl4dim, maxpnt, 
     +   xo, yo, elo, eho)
c-----------------------------------------------------------------------
c     Chop up the allocated memory. Allow for error bars, as well as 
c     individual baseline plots if desired.
c
c  Input:
c    twopass   Two passes through the data
c    maxbuf2   Total amount of available memory
c    maxbase   Number of baselines corresponding to MAXANT
c    maxbase2  Potential maximum number of baselines allowed to plot
c              on separate sub-plots (defined by UVPLT)
c    maxpol    Potential maximum number of polarizations allowed to plot
c              (defined by UVPLT)
c    maxfile   Maximum number of files allowed to plot with different 
c              symbols or colours
c    nfiles    Number of files input by user
c
c    dobase    If true plot each baseline on a separate sub-plot
c    doavall   If true, we want to average everything on each sub-plot
c              together (e.g. polarizations and/or baselines)
c              This can only happens if we are time averaging
c    dosymb    True to plot each file with a different symbol
c    docol     True to plot each file with a different colour if
c              only one polarization
c    dorms     If true want error bars when averaging; x and y
c
c  Input/output
c    npols     Number of polarizations selected by user from input files
c	       (either specified by stokes/select or actual via 2PASS)
c	       or value of 'npols' of first integration if no selections
c	       Zero on output
c    nbases    Number of baselines selected by user from input files
c              (via 2PASS) or number of baselines corresponding to 'nants' 
c	       of first integration
c              Zero on output
c
c  Output:
c               The plot buffer is dimensioned
c                 buffer(pl1dim,pl2dim,pl3dim,pl4dim)
c                        points  basel  pol    files
c    maxpnt     Maximum number of points allowed to plot for each 
c		baseline, polarization and file
c    pl1dim     Dimensions of first index in BUFFER when passed to
c               subroutines.  First dimension contains:
c               X, Y, Xlo, Xhi, Ylo, Yhi (all vectors), where X and Y 
c               are the points to plot, Xlo, Xhi, Ylo and Yhi are the 
c               error bar ends (they are optional).   
c    pl2dim     Dimension of baseline index in BUFFER
c    pl3dim     Dimension of polarization index in BUFFER
c    pl4dim     Dimension of file index in BUFFER
c
c    x,yo       Offsets in the first index of BUFFER for the
c               X and Y vectors
c    elo,eho    Offsets in the first index of BUFFER for the 
c               Xlo, Xhi, Ylo and Yhi vectors  
c
c-----------------------------------------------------------------------
      implicit none
      integer maxbuf2, maxbase, maxbase2, maxpol, maxfile
      integer nfiles, npols, nbases, pl1dim, pl2dim, pl3dim, pl4dim,
     +maxpnt, xo, yo, elo(2), eho(2)
      logical dobase, doavall, dosymb, docol, dorms(2), twopass
cc
      integer nbuf, off
c-----------------------------------------------------------------------
c
c Work out baseline dimension of plot buffer
c
      if (dobase) then
c
c Each baseline plotted separately
c
        if (twopass) then
          pl2dim = min(nbases,maxbase)
        else
          pl2dim = min(nbases,maxbase,maxbase2)
        end if
      else
c
c All baselines on one plot
c
        pl2dim = 1
      end if
c
c Work out polarization dimension of plot buffer
c
      if (doavall) then
c
c All polarizations averaged together
c
        pl3dim = 1
      else
c
c Each polarization plotted separately
c
        pl3dim = min(npols,maxpol)
      end if
c
c Work out file dimension of plot buffer. Cannot have files in
c different colours if plotting more than one polarization
c as the different polarizations get the colours
c
      if (docol .and. pl3dim.gt.1) docol = .false.
      if (dosymb .or. docol) then
c
c Plot each file with different symbol or colour
c
        pl4dim = min(nfiles,maxfile)
      else 
c
c No file discrimination
c
        pl4dim = 1
      end if
c
c Provide space, in the first dimension, for the data (X & Y) and 
c possibly errors (Xlo, Xhi, Ylo, Yhi)
c
      nbuf = 2
      if (dorms(1)) nbuf = nbuf + 2
      if (dorms(2)) nbuf = nbuf + 2
c
c Compute maximum number of points allowed to plot for each
c baseline, polarization and file
c
      maxpnt = maxbuf2 / (nbuf * pl2dim * pl3dim * pl4dim)
      if (maxpnt.lt.1) call bug ('f', 
     +  'Insufficient memory to do anything useful -- select less data')
c
c Dimension of first index of BUFFER when passed to subroutines
c
      pl1dim = nbuf * maxpnt
c
c Offsets for X and Y points in BUFFER
c
      xo = 0
      yo = maxpnt
c
c Offsets for X,Y lo and high errors in buffer.  Careful, do not
c use these pointers when not asking for error bars, as they
c are deliberately set to 0
c
      elo(1) = 0
      elo(2) = 0
      eho(1) = 0
      eho(2) = 0
      off = maxpnt
      if (dorms(1)) then
        off = off + maxpnt
        elo(1) = off
        off = off + maxpnt
        eho(1) = off
      end if
      if (dorms(2)) then
        off = off + maxpnt
        elo(2) = off
        eho(2) = off + maxpnt
      end if     
c
      nbases = 0
      npols = 0
c
      end
c
c
      subroutine endave (ivis, vupd, dayav, day, baseday, reset)
c-----------------------------------------------------------------------
c     Determine if the end of an averaging interval has been reached
c
c  Input:
c    ivis       Visibility counter
c    vupd       Handle of the variables to check for updates
c    dayav      Length of averaging interval in days
c    day        Day of current visibility
c  Input/output:
c    baseday    Day at beginning of current time interval.  Reset
c               when the end of the averaging interval reached
c  Output:
c    reset      True when the end of the averaging interval reached
c
c-----------------------------------------------------------------------
      implicit none
c
      double precision day, baseday, dayav
      logical reset
      integer ivis, vupd
cc
      double precision delday
c
c External
c
      logical uvvarupd, track
c-----------------------------------------------------------------------
      delday = day - baseday
c
c Reset when end of averaging time reached, or when one of the
c tracked variables change, or if time goes backwards
c
      reset = .false.
      track = uvvarupd(vupd)
      if ( delday.lt.0.0 .or. delday.gt.dayav .or. 
     +    (ivis.gt.1 .and. track) ) then
         reset = .true.
         baseday = day
c
         if (delday.lt.0.0) call bug ('w', 
     +      'Data not in time order, accumulators reset')
      end if
c
      end
c
c
      subroutine fixlim (dmin, dmax)
c-----------------------------------------------------------------------
c     Fix up equal limits
c
c     Input/output:
c       dmin,max    Minimum and maximum
c
c-----------------------------------------------------------------------
      implicit none
      real dmin, dmax
c-----------------------------------------------------------------------
      if (dmin.eq.dmax) then
        if (dmin.eq.0.0) then
          dmin = -0.05
          dmax =  0.05
        else
          dmin = dmin - 0.05*dmin
          dmax = dmax + 0.05*dmax
        end if
      end if
c
      end
c
c
      subroutine fullup (maxbase, pl2dim, pl3dim, pl4dim, maxpnt, 
     +                   maxpol, npts, plfidx, allfull)
c-----------------------------------------------------------------------
c     Find out when plot buffer for this file COMPLETELY full
c
c  Input
c    maxpol        Size of second dimension of NPTS
c    maxpnt        Maximum number of points allowed for each
c                  baseline and polarization combination
c    pl*dim        Sizes of last 3 BUFFER dimensions (baseline,
c                  polarization and file)
c    plfidx        The BUFFER index into which this file goes
c    npts          Number of points allocated to this file so far
c                  for each baseline and polarization combination
c  Output
c    allfull       True if no space left for this file
c
c-----------------------------------------------------------------------
      implicit none
c
      integer maxpnt, maxpol, pl2dim, pl3dim, pl4dim, plfidx, maxbase,
     +  npts(maxbase,maxpol)
      logical allfull
cc
      integer i, j
c
      character itoaf*2
c-----------------------------------------------------------------------
c
c  Loop over the allocated plot BUFFER sizes for the baseline and
c  polarization dimensions and see if there is still some room.
c
      allfull = .true.
      do j = 1, pl3dim
        do i = 1, pl2dim
          if (npts(i,j).lt.maxpnt) then
            allfull = .false.
            goto 999
           end if
         end do
      end do
c
      if (pl4dim.gt.1) then
        call bug('w','Plot buffer allocation for file '//
     *	  itoaf(plfidx)//' exhausted; some data lost')
      else
        call bug('w','Plot buffer allocation exhausted; some data lost')
      end if
c
999   end
c
c
      subroutine getdat(preamble,data,flags,maxchan,nread,
     *                  dofqav,doflag,doall)
c-----------------------------------------------------------------------
c  Get a visibility. If needed, perform channel averaging.
c
c  Input:
c    maxchan	Maximum number of channels that can be read.
c    dofqav	If true, average all good channels into a single channel.
c    doflag	If true, treat the flagged data as the desirable ones.
c    doall	If true, ignore the data flags completely.
c
c  Output:
c    preamble	Normal preamble.
c    data	Correlation data.
c    flags	Data flags.
c    nread	Number of output channels (after freqency averaging).
c------------------------------------------------------------------------
      implicit none
      integer maxchan,nread
      double precision preamble(4)
      complex data(maxchan)
      logical flags(maxchan),dofqav,doflag,doall
      integer i,n
      complex sum
c-----------------------------------------------------------------------
      call uvdatrd(preamble,data,flags,maxchan,nread)
c
c  Fudge the flags so the user gets what he or she wants!
c
      if(doall)then
        do i=1,nread
          flags(i) = .true.
        enddo
      else if(doflag)then
        do i=1,nread
          flags(i) = .not.flags(i)
        enddo
      endif
c
c  Average all the channels together, if required.
c
      if(nread.gt.1.and.dofqav)then
        n = 0
        sum = 0
        do i=1,nread
          if(flags(i))then
            sum = sum + data(i)
            n = n + 1
          endif
        enddo
        nread = 1
        flags(1) = n.gt.0
        data(1)  = 0
        if(flags(1))data(1) = sum/n
      endif
c
      end
c
c
      subroutine getdev (devdef, il, pdev)
c-----------------------------------------------------------------------
c     Get plot device from user
c
c  Input:
c     devdef     Default device/type
c  Input/output:
c     pdev       Plot device/type
c     il         Length of PDEV
c
c-----------------------------------------------------------------------
      implicit none
c
      integer il, len1
      character*(*) pdev, devdef
cc
      character str*132
      integer ild
c-----------------------------------------------------------------------
      if (pdev.eq.' ') then
        call output (' ')
        call output (' ')
        ild = len1(devdef)
c
        str = 'Enter plot dev/type (def= '''//devdef(1:ild)//
     +        ''') or ''skip'': '
        call prompt (pdev, il, str(1:len1(str)))
        if (pdev.eq.' ' .or. pdev.eq.'/') then
           pdev = devdef
           il = ild
        end if
        call output (' ')
      else
        il = len1(pdev)
      end if
c
      end
c************************************************************************
      subroutine getlst (lin, lst)
c
      implicit none
      integer lin
      double precision lst
c
c  Get lst of the current data point.
c
c  Input:
c    lin         Handle of file
c  Output:
c    lst         LAST in radians
c-----------------------------------------------------------------------
      double precision time,ra,long,dtemp
      character type*1
      integer length
      logical ok
c
c  Externals.
c
      double precision eqeq
c
      lst = 0.0d0
      call uvprobvr (lin, 'lst', type, length, ok)
      if (type(1:1).eq.' ') then
	call uvrdvrd (lin, 'ra', dtemp, 0.d0)
	call uvrdvrd (lin, 'obsra', ra, dtemp)
	call getlong(lin,long)
	call uvrdvrd(lin,'time',time,0.d0)
        call jullst (time, long, lst)
	lst = lst + eqeq(time)
      else
         call uvrdvrd (lin, 'lst', lst, 0.0d0)
      end if
c
      end
c
c
c
c
      subroutine getlong (lin, long)
c-----------------------------------------------------------------------
c     Get longitude from variable or obspar subroutine
c
c  Input:
c    lin         Handle of file
c  Output:
c    longitude   Longitude in radians
c-----------------------------------------------------------------------
      integer lin
      double precision long
c
      character type*1, telescop*10
      integer length
      logical ok, printed
      save printed
      data printed/.false./
c------------------------------------------------------------------------ 
      long = 0.0d0
      call uvprobvr (lin, 'longitu', type, length, ok)
      if (type(1:1).eq.' ') then
         if(.not.printed)call bug ('w', 
     *		'No longitude variable; trying telescope')
	 printed = .true.
         call uvprobvr (lin, 'telescop', type, length, ok)
         if (type(1:1).eq.' ') then
            call bug ('f', 
     +      'No telescope variable either, can''t work out longitude')
         else
            call uvrdvra (lin, 'telescop', telescop, ' ')
            call obspar (telescop, 'longitude', long, ok)
            if (.not.ok) call bug('f', 
     +          'No valid longitude found for '//telescop)
         end if
      else
         call uvrdvrd (lin, 'longitu', long, 0.0d0)
      end if
c
      end
c
c
      subroutine getlat (lin, lat)
c-----------------------------------------------------------------------
c     Get latitude from variable or obspar subroutine
c
c  Input:
c    lin         Handle of file
c  Output:
c    lat        Latitude in radians
c-----------------------------------------------------------------------
      integer lin
      double precision lat
c
      character type*1, telescop*10
      integer length
      logical ok, printed
      save printed
      data printed/.false./
c------------------------------------------------------------------------ 
      lat = 0.0d0
      call uvprobvr (lin, 'latitud', type, length, ok)
      if (type(1:1).eq.' ') then
         if(.not.printed)call bug ('w', 
     *		'No latitude variable; trying telescope')
	 printed = .true.
         call uvprobvr (lin, 'telescop', type, length, ok)
         if (type(1:1).eq.' ') then
            call bug ('f', 
     +      'No telescope variable either, can''t work out latitude')
         else
            call uvrdvra (lin, 'telescop', telescop, ' ')
            call obspar (telescop, 'latitude', lat, ok)
            if (.not.ok) call bug('f', 
     +          'No valid latitude found for '//telescop)
         end if
      else
         call uvrdvrd (lin, 'latitud', lat, 0.0d0)
      end if
c
      end
c
c
      subroutine getopt (dorms, dovec, doflag, doall, doday, dohour,
     +  dosec, dobase, dointer, doperr, dolog, dozero, doequal,
     +  donano, docal, dopass, dopol, dosrc, doavall, doxind, 
     +  doyind, dowrap, dosymb, dodots, docol, twopass, dofqav,
     +  dotitle)
c-----------------------------------------------------------------------
c     Get user options
c
c   Output:
c     dorms     True if user wants errors plotted when averaging
c     dovec     True if user wants vector averaging, else scalar
c     doflag    Plot only flagged data else plot only unflagged data
c     doall     PLot flagged and unflagged.
c     doday     Averaging time in days if true, else minutes
c     dohour    Averaging time in hours if true, esle minutes
c     dosec     Averaging time in seconds if true, else minutes
c     dobase    True if user wants each baseline on a differnet plot
c     dotitle   If false then don't write plot title
c     dointer   True if user wants to interactively fiddle with the
c               plot after first drawing it.
c     doperr    If true then the automatically determined Y window
c               includes the ends of the error bars
c     dolog     Write plot arrays into the log file
c     dozero    Plot x=0 and y=0
c     doequal   PLot x and y with equal scales
c     donano    Plot u and v in nanoseconds instead of kilo-lambda
c     docal     Apply gains table calibration
c     dopass    Apply bandpass table calibration.
c     dopol     Apply polarisation leakage table correction
c     dosrc     Put source rather than file name in plot title
c     doavall   Average all bvaselines together
c     dox,yind  True for each sub-plot to self-scale the x,y-axis
c     dowrap    True if allowing phases to wrap
c     dosymb    True if plot each file with differnet symbol
c     dodots    Plot averaged data as dots instead of filled circles
c     docol     Plot different files in differnet colours
c     twopass   Make two passes through the data
c     dofqav	Average channels before plotting.
c-----------------------------------------------------------------------
      implicit none
c
      logical dorms(3), dovec(2), doall, doflag, dobase, dointer, 
     +  doperr, dolog, dozero, doequal, donano, docal, dopol, dosrc,
     +  doday, dohour, dosec, doavall, doxind, doyind, dowrap, 
     +  dosymb, dodots, dopass, docol, twopass, dofqav, dotitle
cc
      integer nopt
      parameter (nopt = 29)
c
      character opts(nopt)*8
      logical present(nopt)
      data opts /'rms     ', 'scalar  ', 'flagged ', 'all    ',
     +           'seconds ', 'nobase  ', 'inter   ', 'noerr   ',
     +           'log     ', 'zero    ', 'equal   ', 'nanosec ',
     +           'nocal   ', 'source  ', 'nopol   ', 'days    ', 
     +           'hours   ', 'avall   ', 'xind    ', 'yind    ', 
     +           'unwrap  ', 'symbols ', 'dots    ', 'nopass  ',
     +           'nocolour', '2pass   ', 'mrms    ', 'nofqav  ',
     +           'notitle '/
c-----------------------------------------------------------------------
      call options ('options', opts, present, nopt)
c
      dorms(1) = .false.
      dorms(2) = .false.
      dorms(3) = .false.
      if (present(1) .and. present(27)) then
        call bug ('f', 'Can''t have options=rms,mrms')
      else if (present(1) .or. present(27)) then
        dorms(1) = .true.
        dorms(2) = .true.
        if (present(27)) dorms(3) = .true.
      end if
c
      dovec(1) = .not.present(2)
      dovec(2) = .not.present(2)
      doflag   =      present(3)
      doall    =      present(4)
      dosec    =      present(5)
      dobase   = .not.present(6)
      dointer  =      present(7)
      doperr   = .not.present(8)
      dolog    =      present(9)
      dozero   =      present(10)
      doequal  =      present(11)
      donano   =      present(12)
      docal    = .not.present(13)
      dosrc    =      present(14)
      dopol    = .not.present(15)
      doday    =      present(16)
      dohour   =      present(17)
      doavall  =      present(18)
      doxind   =      present(19)
      doyind   =      present(20)
      dowrap   = .not.present(21)
      dosymb   =      present(22)
      dodots   =      present(23)
      dopass   = .not.present(24)
      docol    = .not.present(25)
      twopass  =      present(26)
      dofqav   = .not.present(28)
      dotitle  = .not.present(29)
c
      end
c
c
      subroutine getrng (keyw, axis, rmin, rmax)
c-----------------------------------------------------------------------
c     Get the axis ranges given by the user
c
c  Input
c    keyw     Keyword to get from user
c    axis     Axis type
c  Output
c    rmin,max Range in appropriate units
c
c-----------------------------------------------------------------------
      implicit none
c-----------------------------------------------------------------------
      character*(*) axis, keyw
      real rmin, rmax
cc
      real trange(8)
      integer il, len1, nt, s
c-----------------------------------------------------------------------
      il = len1(keyw)
      if (axis.eq.'time') then
        call mkeyr (keyw(1:il), trange, 8, nt)
        if (nt.gt.0) then
          if (nt.ne.8) then
            call bug ('f',
     +        'You must specify 8 numbers for the time range')
          else
c
c Convert to seconds
c
            rmin = 24.0*3600.0*trange(1) + 3600.0*trange(2) + 
     +                    60.0*trange(3) + trange(4)
            rmax = 24.0*3600.0*trange(5) + 3600.0*trange(6) + 
     +                    60.0*trange(7) + trange(8)
          end if
        else
          rmin = 0.0
          rmax = 0.0
        end if
      else if (axis.eq.'hangle') then
        call mkeyr (keyw(1:il), trange, 6, nt)
        if (nt.gt.0) then
          if (nt.ne.6) then
            call bug ('f',
     +        'You must specify 6 numbers for the hangle range')
          else
c
c Convert to seconds
c
            s = 1
            if (trange(1).lt.0.0) s = -1
            rmin = 3600.0*abs(trange(1)) + 60.0*trange(2) + trange(3)
            rmin = s * rmin
c 
            s = 1
            if (trange(4).lt.0.0) s = -1
            rmax = 3600.0*abs(trange(4)) + 60.0*trange(5) + trange(6)
            rmax = s * rmax
          end if
        else
          rmin = 0.0
          rmax = 0.0
        end if
      else
        call keyr (keyw(1:il), rmin, 0.0) 
        call keyr (keyw(1:il), rmax, 0.0)
      end if
c
      end
c
c
      subroutine getrng2 (axis, type, rlo, rhi, win, ok)
c-----------------------------------------------------------------------
c     Get the axis ranges given by the user
c
c  Input
c    axis     Axis name; 'x' or 'y'
c    type     Axis type
c    rlo,rhi  Default values
c  Output
c    win      User's values
c    ok       SUccess decoding of inputs
c
c-----------------------------------------------------------------------
      implicit none
c-----------------------------------------------------------------------
      character axis*1, type*(*)
      real rlo, rhi, win(2)
      logical ok
cc
      integer il
      character str*132
c-----------------------------------------------------------------------
      if (type.eq.'time') then
        call prompt (str, il,
     +  'Enter '//axis//'-range (2 x DD HH MM SS.S) (def. with /) :')
        if (str(1:1).eq.'/' .or. str.eq.' ') then
           ok = .true.
           win(1) = rlo
           win(2) = rhi
         else
           call timdec (str(1:il), win(1), win(2), ok)
         end if
      else if (type.eq.'hangle') then
        call prompt (str, il,
     +  'Enter '//axis//'-range (2 x HH MM SS.S) (def. with /) :')
        if (str(1:1).eq.'/' .or. str.eq.' ') then
           ok = .true.
           win(1) = rlo
           win(2) = rhi
         else
           call timdc2 (str(1:il), win(1), win(2), ok)
         end if
      else 
        call prompt (str, il,
     +  'Enter '//axis//'-range (2 reals) (def. with /) :')
        if (str(1:1).eq.'/' .or. str.eq.' ') then
           ok = .true.
           win(1) = rlo
           win(2) = rhi
         else
           call arrdec (2, str, il, win, ok)
         end if
      end if
c
      end
c
c
      subroutine getval (ilen, aline, ib, val, ok)
c------------------------------------------------------------------------
c     Look for the next number in string, where the delimiters
c     are any amount of white space.
c
c  Input:
c    ilen    length of string
c    aline   The string
c  Input/output
c    ib      Location to start at.  Incremented on exit
c Output:
c    val     The number
c    ok      If false, failed to get integer
c
c-----------------------------------------------------------------------
      implicit none
c
      double precision val
      integer ilen, ib
      logical ok
      character aline*(*)
cc
      integer ie
c-----------------------------------------------------------------------
      do while (aline(ib:ib).eq.' ' .and. ib.le.ilen)
        ib = ib + 1
      end do
c
      ie = index (aline(ib:), ' ')
      if (ie.eq.0) then
         ie = ilen
      else
         ie = ie + ib - 2
      end if
c
      if (ib.gt.ie) then
        ok = .false.
      else
        call atodf (aline(ib:ie), val, ok)
      end if
      ib = ie + 2
c
      end
c
c
      subroutine getwin (xaxis, yaxis, xlo, xhi, ylo, yhi)
c-----------------------------------------------------------------------
c     Prompt user for a new plot window
c
c  Input:
c    xaxis              Xaxis type
c    yaxis              Yaixs type
c  Input/output:
c    x1,x2,y1,y2        Previous and new plot window.
c
c-----------------------------------------------------------------------
      implicit none
      real xlo, xhi, ylo, yhi
      character*(*) xaxis, yaxis
cc
      real win(4)
      logical loop, ok
c-----------------------------------------------------------------------
      loop = .true.
      do while (loop) 
        call output (' ')
c
        call shorng ('x', xaxis, xlo, xhi)
        call shorng ('y', yaxis, ylo, yhi)
c
        call output (' ')
        call getrng2 ('x', xaxis, xlo, xhi, win(1), ok)
        if (ok) call getrng2 ('y', yaxis, ylo, yhi, win(3), ok)
c
        if (win(1).eq.win(2) .or. win(3).eq.win(4) .or. .not.ok) then
          call output (' ')
          call output ('Bad window, try again')
        else
          xlo = win(1)
          xhi = win(2)
          ylo = win(3)
          yhi = win(4)
          loop = .false.
        end if
      end do
c
      end
c
c************************************************************************
      subroutine getwvl (donano, preamble, u, v, uvdist, uvpa)
c
      implicit none
      double precision preamble(2)
      real u, v, uvdist, uvpa
      logical donano
c-----------------------------------------------------------------------
c     Get some things from the preamble
c
c  Input:
c    lin          Handle of vis file
c    donano       True for wavelengths in nanoseconds, else kilo-lambda
c    preamble     u and v in raw form (nsec or lambda)
c  Output:
c    u,v          u and v in form selected by user (nsec or klambda)
c    uvdist       sqrt(u**2 + v**2)
c    uvpa         Position angle of u,v clockwise from v axis
c-----------------------------------------------------------------------
cc
      include 'mirconst.h'
c-----------------------------------------------------------------------
      u = preamble(1)
      v = preamble(2)
c
      if (.not.donano) then
        u = preamble(1) / 1000.0
        v = preamble(2) / 1000.0
      end if
c
      uvdist = sqrt(u*u + v*v)
c 
      if (u.ne.0.0 .or. v.ne.0.0) then
        uvpa = 180.0/DPI * atan2(u, v) 
      else
c
c Signal this one no good
c
        uvpa = 999.0
      end if
      end
c************************************************************************
      subroutine goodat ( n, flags, nkeep)
c
c     See if there is any wanted data in this visibility
c
c  Input
c    n         Number of channels
c    flags     Channel flags, true if unflagged
c  Output
c    keep      Number of visibilities to keep.
c-----------------------------------------------------------------------
      implicit none
      integer n, nkeep
      logical flags(n)
cc
      integer i
c-----------------------------------------------------------------------
      nkeep = 0
c
c Plot unflagged data
c
      do i = 1, n
        if (flags(i)) nkeep = nkeep + 1
      end do
c
      end

      subroutine hannit (hann, coeffs, work, pl1dim, pl2dim, pl3dim,
     +   pl4dim, maxbase, maxpol, maxfile, nbases, npols, npts, 
     +   buffer, yo)
c-----------------------------------------------------------------------
c     Hanning smooth arrays
c
c  Input:
c   hann           Hanning length
c   coeffs         Hanning coefficients buffer
c   work           Work array for Hanning smooth
c   npts           Number of points to plot
c   yo             Offsets to BUFFER for Y data
c   nbases         Number of baseline encountered
c   npols          Number of polarizations encountered
c Input/output:
c   buffer         Plot buffer
c
c-----------------------------------------------------------------------
      implicit none
c
      integer pl1dim, pl2dim, pl3dim, pl4dim, maxbase, maxpol,
     +  maxfile, npts(maxbase,maxpol,maxfile), yo, hann,
     +  nbases, npols
      real buffer(pl1dim,pl2dim,pl3dim,pl4dim), coeffs(hann), 
     +  work(hann)
cc
      integer i, j, k, nb, np
c----------------------------------------------------------------------
      call hcoeffs (hann, coeffs)
c
c
c Work out do loop sizes for baselines and polarizations.  It can
c be that we have allocated more plot buffer space (pl*dim) than
c was used.   
c
      nb = min(nbases,pl2dim)
      np = min(npols,pl3dim)
c
      do k = 1, pl4dim
        do j = 1, np
          do i = 1, nb
            if (npts(i,j,k).ne.0) call hannsm (hann, coeffs,
     +        npts(i,j,k), buffer(yo+1,i,j,k), work)
          end do
        end do
      end do
c
      end
c
c
      subroutine init (maxbase, maxpol, nsum, xsumr, xsumi, xsumsqr, 
     +                 xsumsqi, ysumr, ysumi, ysumsqr, ysumsqi)
c-----------------------------------------------------------------------
c     Initialize accumulators
c-----------------------------------------------------------------------
      implicit none
c
      integer maxbase, maxpol, nsum(maxbase,maxpol)
      real xsumr(maxbase,maxpol), ysumr(maxbase,maxpol),
     +  xsumsqr(maxbase,maxpol), ysumsqr(maxbase,maxpol),
     +  xsumi(maxbase,maxpol), ysumi(maxbase,maxpol),
     +  xsumsqi(maxbase,maxpol), ysumsqi(maxbase,maxpol)
cc
      integer i, j
c-----------------------------------------------------------------------
      do j = 1, maxpol
        do i = 1, maxbase
          nsum(i,j) = 0
          xsumr(i,j) = 0.0
          ysumr(i,j) = 0.0
          xsumi(i,j) = 0.0
          ysumi(i,j) = 0.0
          xsumsqr(i,j) = 0.0
          ysumsqr(i,j) = 0.0
          xsumsqi(i,j) = 0.0
          ysumsqi(i,j) = 0.0
        end do
      end do
c
      end
c
c
      subroutine inputs (maxco, xaxis, yaxis, xmin, xmax, ymin, ymax,
     +    dayav, tunit, dorms, dovec, doflag, doall, dobase, dointer,
     +    doperr, dolog, dozero, doequal, donano, dosrc, doavall, 
     +    doxind, doyind, dowrap, dosymb, dodots, docol, inc, nx, ny, 
     +    pdev, logf, comment, size, hann, ops, twopass, dofqav,
     +    dotitle )
c-----------------------------------------------------------------------
c     Get the user's inputs 
c
c  Input:
c    maxco        Maximum allowed size of Hanning smoothing length
c  Output:
c    x,yaxis      Axis types
c    x,ymin,max   User given extrema for plots.  
c    dayav        Averaging interval in days
c    tunit        Converts averaging time in units given by user to days
c    dorms        True to plot error bars when averaging; x and y
c		  dorms(3) = false for standard deviation, true for 
c		  standard deviation of the mean
c    dovec        True for vector averging, else scalar; x and y
c    doflag       Plot flagged visbiltites only else plot unflagged
c    doall        Plot flagged and unflagged.  Overrides DOFLAG
c    dobase       User wants baselines on separaet plots
c    dotitle      If false then don't write plot title
c    dointer      User gets to play interactively with the plot
c    doperr       If true then the automatically determined Y window
c                 includes the ends of the error bars
c    dolog        Write plot arrays into the log file
c    dozero       Plot x=0 and y=0
c    doequal      PLot x and y with equal scales
c    donano       Plot u and v in nanoseconds rather than kilo-lambda
c    dosrc        Put source rather than file name in plot title
c    doavall      Average all baselines together
c    dox,yind     When OPTIONS=NOBASE is not set, DOX,YIND are true for 
c                 each sub-plot to have the x,y-plot range self-scale 
c                 independently.
c    dowrap       When true, allow phases to wrap from 180 to -180.
c                 Otherwise, some simple unwrapping is done.
c    dosymb       True if plot each file with different symbol
c    dodots       Plot averaged data as dots instead of filled circles
c    docol        Plot different filesd in different colours
c    inc          Plot every INCth point after selection
c    nx,ny        Number of plots in x and y directions on page
c    pdev         PGPLOT device.  Blank means prompt user
c    logf         Logfile
c    comment      COmment to put in log file
c    size         PGPLOT character sizes for the labels and symbols
c    hann         Hanning smoothing length
c    twopass      Make two passes through the data
c    dofqav       Average frequency channels before plotting.
c    dotitle      When false don't write plot title
c-----------------------------------------------------------------------
      implicit none
c
      character*(*) xaxis, yaxis, pdev, logf, comment
      double precision dayav
      real xmin, xmax, ymin, ymax, size(2)
      logical dorms(3), dovec(2), doflag, doall, dobase, dointer, 
     +  doperr, dolog, dozero, doequal, donano, docal, dopol, dosrc,
     +  doavall, doxind, doyind, dowrap, dosymb, dodots, dopass, 
     +  docol, twopass, dofqav, dotitle
      integer nx, ny, inc, hann, maxco, ilen, ilen2, tunit
cc
      integer i
      character itoaf*3, str*3, word*50, ops*9, axis(2)*10
      logical doday, dohour, dosec
c
      integer len1
      logical keyprsnt
c
c Types of axes allowed
c
      integer naxmax, nax
      parameter (naxmax = 21)
      character axtyp(naxmax)*10
      data axtyp /  'time      ','dtime     ','uvdistance','uu        ',
     + 'vv        ','uc        ','vc        ','uvangle   ','amplitude ',
     + 'phase     ','real      ','imag      ','hangle    ','dhangle   ',
     + 'parang    ','lst       ','az        ','el        ','airmass   ',
     + 'jyperk    ','rms       '/
c-----------------------------------------------------------------------
      call keyini
c
      call getopt (dorms, dovec, doflag, doall, doday, dohour, dosec,
     +   dobase, dointer, doperr, dolog, dozero, doequal, donano, 
     +   docal, dopass, dopol, dosrc, doavall, doxind, doyind, 
     +   dowrap, dosymb, dodots, docol, twopass, dofqav, dotitle)
c
      ops = 'sdl'
      i = 3
      if (.not.donano) then
        i = i + 1
        ops(i:i) = 'w'
      end if
      if (docal) then
        i = i + 1
        ops(i:i) = 'c'
      endif
      if (dopol) then
        i = i + 1
        ops(i:i) = 'e'
      endif
      if (dopass) then
        i = i + 1
        ops(i:i) = 'f'
      endif
c
c Set UV selection criteria 
c
      call uvdatinp ('vis', ops)
c
      call keymatch ('axis', naxmax, axtyp, 2, axis, nax)
      xaxis = axis(1)
      yaxis = axis(2)
      if (xaxis.eq.' ') xaxis = 'time'
      if (yaxis.eq.' ') yaxis = 'amplitude'
      if (xaxis.eq.yaxis) call bug ('f', 'x and y axes identical')
c
c Get axis ranges
c
      call getrng ('xrange', xaxis, xmin, xmax)
      call getrng ('yrange', yaxis, ymin, ymax)
      if (.not.dobase) then
        doxind = .false.
        doyind = .false.
      end if
      if (xmin.ne.0.0 .or. xmax.ne.0.0) doxind = .false.
      if (ymin.ne.0.0 .or. ymax.ne.0.0) doyind = .false.
c
      call keyd ('average', dayav, -1.0d0)
c
      if (dayav.gt.0.0) then
c
c Convert averaging interval to days
c
        if (doday) then
          tunit = 1
        else if (dohour) then
          tunit = 24
        else if (dosec) then
          tunit = 24 * 3600
        else
          tunit = 24 * 60
        end if
c
        dayav = dayav / tunit
      else
        if (doavall) then
          call bug ('w', 'OPTIONS=AVALL only useful if time averaging')
          doavall = .false.
        end if
      end if
c
      call keyi ('hann', hann, 1)
      if (hann.lt.3) then
        hann = 1
      else if (hann.gt.maxco) then
        str = itoaf(maxco)
        call bug ('f', 'Hanning smoothing length must be <= '//str)
      end if
c
      call keyi ('inc', inc, 1)
      if (inc.le.0) inc = 1
c
      if (doall) doflag = .false.
c
      call keyi ('nxy', nx, 0)
      call keyi ('nxy', ny, nx)
c
c No interactive mode for single baseline plots, and all
c baselines together always get one plot per page only
c
      if (dobase) then
        if (dointer) then
          call bug ('w', 'Interactive mode not allowed for '//
     +              'single baseline plots')
          dointer = .false.
        end if
      else if(nx*ny.eq.0)then
        nx = 1
        ny = 1
      end if
c
      call keya ('device', pdev,' ')
      call keyr ('size', size(1), 0.0)
      call keyr ('size', size(2), size(1))
c
      call keya ('log', logf, ' ')
c
      ilen = 0
      comment = ' '
      do while (keyprsnt('comment'))
        call keya('comment',word,' ')
        ilen2 = len1(word)
        comment(ilen+1:) = word(1:ilen2)//' '
        ilen = ilen + ilen2 + 1
      enddo
c
      call keyfin
c
      end
c
c
      subroutine limstr (dmin, dmax)
c-----------------------------------------------------------------------
c     Stretch limits by 5%
c
c     Input/output:
c       dmin,max    Minimum and maximum
c       
c-----------------------------------------------------------------------
      implicit none
      real dmin, dmax
cc
      real absmax, delta
c-----------------------------------------------------------------------
      delta = 0.05 * (dmax - dmin)
      absmax = max(abs(dmax),abs(dmin))
      if (delta.le.1.0e-5*absmax) delta = 0.01 * absmax
      if (delta.eq.0.0) delta = 1
      dmin = dmin - delta
      dmax = dmax + delta
c
      end
c
c
      subroutine logfop (logf, comment, doave, dovec, doall, doflag)
c-----------------------------------------------------------------------
c     Open the log file and write some useful (?) messages
c
c  Input:
c    logf     Log file name
c    comment  Comment to write in log file
c    doave    True if averaging 
c    dovec    True if vector averging; x and y
c    doall    True if plotting flagged and unflagged points
c    doflag   True if plotting only flagged points
c
c-----------------------------------------------------------------------
      implicit none
c
      character logf*(*), comment*(*)
      logical doave, dovec(2), doall, doflag
cc
      integer len1
      logical more
      character aline*80
c-----------------------------------------------------------------------
      call logopen (logf, ' ')
      if (len1(comment).gt.0)
     +    call logwrite (comment(1:len1(comment)), more)
c
      if (doave) then
        if (dovec(1)) then
           call logwrite ('Applying vector averaging to x-axis', more)
        else
           call logwrite ('Applying scalar averaging to x-axis', more)
        end if
        if (dovec(2)) then
           call logwrite ('Applying vector averaging to y-axis', more)
        else
           call logwrite ('Applying scalar averaging to y-axis', more)
        end if
      end if
c
      if (doall) then
        aline = 'Will plot flagged and unflagged data'
      else if (doflag) then
        aline = 'Will plot flagged data'
      else 
        aline = 'Will plot unflagged data'
      end if
      call logwrite (aline, more)
      call logwrite (' ', more)
c
      end
c
c
      subroutine mtitle (lin, nread, dosrc, dayav, tunit, 
     +                   nfiles, title)
c-----------------------------------------------------------------------
c     Make a title for the plot
c
c   Input
c     lin     Handle for open visibility file
c     nread   Number of channels read from previous visibility
c     dosrc   Put source rather than file name in plot title
c     dayav   Averaging time in days
c     tunit   COnverts between days and units given by user for
c             avearing time
c     nfiles  Number of files to plot
c   Output
c     title   Title string
c
c-----------------------------------------------------------------------
      implicit none
c
      double precision dayav
      integer tunit, nfiles, nread, lin
      character*(*) title
      logical dosrc
cc
      include 'maxdim.h'
      double precision data(maxchan)
      logical more
      real av
      character source*9, str*40, name*30, str2*10, str3*30
      integer len1, il1, il2, il3
c-----------------------------------------------------------------------
      call uvrdvra (lin, 'source', source, ' ')
      call uvinfo (lin, 'sfreq', data)
c
c Write central frequency
c
      call strfd ((data(1)+data(nread))/2.0d0, '(f8.4)', str, il1)
c
c Write source name
c
      if (dosrc) then
        write(title, 100) source(1:len1(source)), str(1:il1)
      else
        if (nfiles.eq.1) then
          call uvdatgta ('name', name)          
          write(title,100) name(1:len1(name)), str(1:il1)
100       format (a, 1x, a,' GHz')
        else 
          write(title,200) str(1:il1)
200       format (a, ' GHz')
        end if
      end if
      il1 = len1(title)
c
c Write averaging time
c
      if (dayav.gt.0.0) then
        if (tunit.eq.1) then
          str2 = '\ud\d'
        else if (tunit.eq.24) then
          str2 = '\uh\d'
        else if (tunit.eq.24*60) then
          str2 = '\um\d'
        else if (tunit.eq.24*60*60) then
          str2 = '\us\d'
        end if
        av = dayav * tunit
c
        call strfr (av, '(f20.2)', str3, il3)
        str = str3(1:il3)//str2
        il2 = len1(str)
        title(il1+2:) = str(1:il2)
      end if
c
      call logwrite (title(1:len1(title)), more)
c
      end
c
c
      subroutine nxyset (nplot, nx, ny)
c-----------------------------------------------------------------------
c     Set default number of sub-plots
c-----------------------------------------------------------------------
      implicit none
      integer nplot, nx, ny
cc
      integer maxsub
      parameter (maxsub = 12)
c
      integer nxx(maxsub), nyy(maxsub), np
      save nxx, nyy
      data nxx /1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4/
      data nyy /1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3/
c-----------------------------------------------------------------------
      np = min(nplot, maxsub)
      if (nx.eq.0) nx = nxx(np)
      if (ny.eq.0) ny = nyy(np)
c
      end
c
c
      subroutine plotit (dointer, doave, dorms, dobase, dolog, dozero,
     +   doequal, donano, doxind, doyind, doperr, dowrap, dosymb, 
     +   dodots, title, xaxis, yaxis, xmin, xmax, ymin, ymax, xxmin,
     +   xxmax, yymin, yymax, pdev, pl1dim, pl2dim, pl3dim, pl4dim, 
     +   maxbase, maxpol, maxfile, nbases, npols, npts, buffer, xo, 
     +   yo, elo, eho, nx, ny, a1a2, order, size, polmsk, 
     +   doavall, docol, dotitle)
c-----------------------------------------------------------------------
c     Draw the plot
c
c  Input:
c   dointer        True if interactive plotting wanted
c   doave          True if time averaging invoked
c   dorms          True if errors wanted on plot; x and y
c   dobase         Plot baselines singly
c   dolog          Write plot ararys into log file
c   dozero         Plot x=0 and y=0 lines
c   doequal        Plot x and y with equal scales
c   donano         u and v in nanoseconds, else k-lambda
c   dox,yind       If true self-scaling is independent on the x,y-axis
c                  for each sub-plot
c   doperr         Include errors in automtaically determined plot limits
c   dowrap         False to unwrap phases
c   dosymb         Plot each file with a differnet symbol
c   dodots         Plot averaged data as dots rather than filled circles
c   docol          Plot differnt files in different colours if one polarization
c   dotitle        False to not write plot title
c   title          Title for plot
c   x,yaxis        X and Y axis types
c   x,ymin,max     User specified plot extrema
c   pdev           Plotting device
c   npts           Number of points to plot
c   buffer         Plot buffer
c   x,yo           Offsets to BUFFER for X and Y
c   el,ho          Offsets to BUFFER for X, Y errors, high and low
c   nx,ny          Number of plots in x and y directiosn on page.
c                  If nx*ny > 1 then each baseline is plotted on
c                  a separate page
c   a1a2           List of baslines encoded into strings
c   size           PGPLOT char sizes for labels and symbols
c   polmsk         Tells what the polarization number is for
c                  certain polarizations.  Thus, polmsk(-5) tells
c                  you the polarization number for YY, if non-zero.
c   doavall        True if averaging everything on the sub-plot
c                  together
c   nbases         Number of baseline encountered in files
c   npols          Number of poalrizations encountered in files
c
c Input/output
c   xx,yymin,max   Work array (automatically determined plot extrema)
c   order          Work array
c-----------------------------------------------------------------------
      implicit none
c
      integer pl1dim, pl2dim, pl3dim, pl4dim, maxbase, maxpol, maxfile,
     +  xo, yo, elo(2), eho(2), a1a2(maxbase,2), order(maxbase),
     +  npts(maxbase,maxpol,maxfile), nx, ny, polmsk(-8:4),
     +  nbases, npols
      real xmin, xmax, ymin, ymax, size(2), xxmin(maxbase), 
     +  xxmax(maxbase), yymin(maxbase), yymax(maxbase),
     +  buffer(pl1dim,pl2dim,pl3dim,pl4dim)
      character title*(*), xaxis*(*), yaxis*(*), pdev*(*), xopt*10,
     +  yopt*10
      logical doave, dorms(2), dobase, dointer, dolog, dozero, doequal,
     +  donano, doxind, doyind, dowrap, doperr, dosymb, dodots, doavall,
     +  docol, dotitle
cc
      integer NCOL
      parameter(NCOL=12)
      real xmnall, xmxall, ymnall, ymxall, xlo, xhi, ylo, yhi
      integer ierr, il1, il2, sym, ip, jf, lp, kp, k, ii,
     +  ipl1, ipl2, npol, i, j, cols1(NCOL), cols2(NCOL), cols(NCOL),
     +  nb, np, ipt, il, ilen, icol
      character xlabel*100, ylabel*100, ans*1, devdef*80, 
     +  str*80, units*10
      character*2 fmt(2), polstr(12)*2, hard*3
      logical new, more, redef, none
c   
      integer pgbeg, len1
      character polsc2p*2
c
      save cols
      data fmt /'i1', 'i2'/
      data cols1 /1, 7, 2, 5, 3, 4, 6, 8, 9,  10, 11, 12/
      data cols2 /1, 2, 5, 3, 4, 6, 8, 9, 10, 11, 12, 13/
c----------------------------------------------------------------------
c
c  Write plot labels; strings must be long enough to accomodate
c  double backslashes caused by ratty for SUNs (\ has to be escaped)
c
      if (donano) then
        units = ' (nsec)'
      else
        units = ' (k\gl)'
      end if
      call setlab ('X', units, xaxis, dozero, xlabel, xopt)
      call setlab ('Y', units, yaxis, dozero, ylabel, yopt)
c
c Set initial plot symbol
c
      sym = 1
      if (doave .and. .not.dodots) sym = 17
      if ( ((xaxis.eq.'uu' .or. xaxis.eq.'uc') .and.
     +      (yaxis.eq.'vv' .or. yaxis.eq.'vc'))   .or.
     +     ((xaxis.eq.'vv' .or. xaxis.eq.'vc') .and.
     +      (yaxis.eq.'uu' .or. yaxis.eq.'uc')) ) sym = 1
      il1 = len1(title)
c
c Work out do loop sizes for baselines and polarizations.  It can
c be that we have allocated more plot buffer space (pl*dim) than
c was used.   
c
      nb = min(nbases,pl2dim)
      np = min(npols,pl3dim)
c
c  Sort baselines into baseline order
c
      if (dobase) then
        call sortbas (maxbase, nb, a1a2, order)
      else 
        order(1) = 1
      end if
c
c  Have a guess at number of plots in x and y directions
c
      if (nx.eq.0 .or. ny.eq.0) call nxyset (nb, nx, ny)
c
c Set default sizes
c
      if(size(1).le.0) size(1) = real(max(nx,ny))**0.4
      if(size(2).le.0) size(2) = size(1)
c
c Work out which polarizations we are going to plot so
c we can give them a different colour on the plot
c
      npol = 0
      do j = 1, 12
        do i = -8, 4
          if (i.ne.0) then
            if (polmsk(i).eq.j) then
              npol = npol + 1
              polstr(npol) = polsc2p(i)
              goto 10
            end if
          end if
        end do
10      continue
      end do
c
c  Initialize extrema from all sub-plots
c
      xmnall =  1.0e32
      xmxall = -1.0e32
      ymnall =  1.0e32
      ymxall = -1.0e32
c
c  Get plot extrema
c
      do ip = 1, pl2dim
c
c  Initialize extrema for each sub-plot
c
        xxmin(ip) =  1.0e32
        xxmax(ip) = -1.0e32
        yymin(ip) =  1.0e32
        yymax(ip) = -1.0e32
c
c  Loop over number of files and polarizations for this sub-plot
c
        do jf = 1, pl4dim
          do kp = 1, np
c
c  Unwrap phases if desired
c
            if (.not.dowrap .and. yaxis.eq.'phase') then
              if (npts(ip,kp,jf).gt.1) then
                call unwrap (npts(ip,kp,jf), buffer(yo+1,ip,kp,jf))
              end if
            end if
c
            if ( (xmin.eq.0.0 .and. xmax.eq.0.0) .or.
     +           (ymin.eq.0.0 .and. ymax.eq.0.0) ) then
c
c  Get x,y auto-limits
c
              do k = 1, npts(ip,kp,jf)
                if (doperr .and. dorms(1)) then
                  xxmin(ip) = min(xxmin(ip),buffer(elo(1)+k,ip,kp,jf),
     +                                      buffer(eho(1)+k,ip,kp,jf))
                  xxmax(ip) = max(xxmax(ip),buffer(elo(1)+k,ip,kp,jf),
     +                                      buffer(eho(1)+k,ip,kp,jf))
                else
                  xxmin(ip) = min(xxmin(ip), buffer(xo+k,ip,kp,jf))
                  xxmax(ip) = max(xxmax(ip), buffer(xo+k,ip,kp,jf))
                end if
c
                if (doperr .and. dorms(2)) then
                  yymin(ip) = min(yymin(ip),buffer(elo(2)+k,ip,kp,jf),
     +                                      buffer(eho(2)+k,ip,kp,jf))
                  yymax(ip) = max(yymax(ip),buffer(elo(2)+k,ip,kp,jf),
     +                                      buffer(eho(2)+k,ip,kp,jf))
                else
                  yymin(ip) = min(yymin(ip), buffer(yo+k,ip,kp,jf))
                  yymax(ip) = max(yymax(ip), buffer(yo+k,ip,kp,jf))
                end if
              end do
            end if
          end do
        end do
c 
c  Update limits from all sub-plots
c
        xmnall = min(xmnall, xxmin(ip))
        xmxall = max(xmxall, xxmax(ip))
        ymnall = min(ymnall, yymin(ip))
        ymxall = max(ymxall, yymax(ip)) 
c
c  Stretch limits for this sub-plot
c
        call limstr (xxmin(ip), xxmax(ip))
        call limstr (yymin(ip), yymax(ip))
c
c  Assign user's limits if desired for this sub-plot
c
        if (xmin.ne.0.0 .or. xmax.ne.0.0) then
          xxmin(ip) = xmin
          xxmax(ip) = xmax
        end if
c
        if (ymin.ne.0.0 .or. ymax.ne.0.0) then
          yymin(ip) = ymin
          yymax(ip) = ymax
        end if
c
c  Fix up bodgy limits
c
        call fixlim (xxmin(ip), xxmax(ip))
        call fixlim (yymin(ip), yymax(ip))
      end do
c
c  Set all encompassing x,y-ranges if desired
c
      call limstr (xmnall, xmxall)
      call limstr (ymnall, ymxall)
      do ip = 1, nb
        if (.not.doxind .and. xmin.eq.0.0 .and. xmax.eq.0.0) then
          xxmin(ip) = xmnall
          xxmax(ip) = xmxall
        end if
c
        if (.not.doyind .and. ymin.eq.0.0 .and. ymax.eq.0.0) then
          yymin(ip) = ymnall
          yymax(ip) = ymxall
        end if
      end do
c
c  Begin plotting loop
c
      devdef = '/xd'
      new = .true.
      redef = .false.
      do while (new)
        ierr = 0
        new = .false.
c
c Prompt for plot device
c
        call getdev (devdef, il2, pdev)     
        if (pdev.ne.'skip') then
c
c  Try to open plot device
c
          ierr = pgbeg (0, pdev(1:il2), nx, ny)
c
          if (ierr.ne.1) then
            call pgldev 
            call bug ('f', 'Error opening plot device')
          else
c
c Set standard viewport
c
            call pgscf (2)
            call pgsch(size(1))
            call pgvstd 
c
c DOn't use yellow for hardcopy
c
            call pgqinf ('hardcopy', hard, ilen)
            if (hard.eq.'YES') then
              do i = 1, NCOL
                cols(i) = cols2(i)
              end do
            else
              do i = 1, NCOL
                cols(i) = cols1(i)
              end do
            end if
c  
c  Loop over number of sub-plots
c 
            do ip = 1, nb
              kp = order(ip)
c
c  See if there is anything to plot for this baseline
c
              none = .true.
              do jf = 1, pl4dim
                do lp = 1, np
                  if (npts(kp,lp,jf).ne.0) none = .false.
                end do
              end do
              if (none) goto 100
c
c  Set plot extrema
c
              if (.not.redef) then
                xlo = xxmin(kp)
                xhi = xxmax(kp)
c                  
                ylo = yymin(kp)
                yhi = yymax(kp)
              end if
c
c  Write title
c
              if (dobase) then
                ipl1 = int(log10(real(a1a2(kp,1)))) + 1
                ipl2 = int(log10(real(a1a2(kp,2)))) + 1
                write (title(il1+3:),
     +            '('//fmt(ipl1)//'''-'''//fmt(ipl2)//')') 
     +            a1a2(kp,1), a1a2(kp,2)
              end if
c
c  Set window on view surface
c
              call pgsch(size(1))
              if (doequal) then
                call pgwnad (xlo, xhi, ylo, yhi)
              else
                call pgswin (xlo, xhi, ylo, yhi)
              end if
c
c  Draw box and label
c
              call pgpage
              call pgtbox (xopt, 0.0, 0, yopt, 0.0, 0)
              call pglab (xlabel, ylabel, ' ')
              if (dotitle) then
                call pltitle (npol, polstr, title, cols, doavall)
              end if
c
c  Plot points and errors
c
              do lp = 1, np
	        icol = mod(lp-1,NCOL)+1
                if (np.ne.1 .and. .not.doavall) call pgsci (cols(icol))
                do jf = 1, pl4dim
	          icol = mod(jf-1,NCOL)+1
                  if (np.eq.1 .and. docol) call pgsci (cols(icol))
                  if (dosymb) then
                    sym = jf
                    if (sym.eq.2) then
                      sym = 21
                    else if (sym.gt.2) then
                      sym = sym - 1
                    end if
                  end if
c              
                  if (npts(kp,lp,jf).ne.0) then
                    call pgsch(size(2))
                    call pgpt (npts(kp,lp,jf),buffer(xo+1,kp,lp,jf), 
     +                            buffer(yo+1,kp,lp,jf), sym)
                    if (dorms(1))
     +                call pgerrx (npts(kp,lp,jf), 
     +                             buffer(elo(1)+1,kp,lp,jf), 
     +                             buffer(eho(1)+1,kp,lp,jf),
     +                             buffer(yo+1,kp,lp,jf),  1.0)
                    if (dorms(2))
     +                call pgerry (npts(kp,lp,jf), 
     +                             buffer(xo+1,kp,lp,jf), 
     +                             buffer(elo(2)+1,kp,lp,jf), 
     +                             buffer(eho(2)+1,kp,lp,jf), 1.0)
c
c  Write log file; save x, y, xer, yerr
c
                    if (dolog) then
                      do ii = 1, npts(kp,lp,jf)
                        str = ' '
                        ipt = 1
                        call strfr (buffer(xo+ii,kp,lp,jf), 
     +                              '(1pe12.5)', str(ipt:), il)
                        ipt = ipt + il + 1
                        call strfr (buffer(yo+ii,kp,lp,jf), 
     +                              '(1pe12.5)', str(ipt:), il)
                        ipt = ipt + il + 1
c
                        if (dorms(1) .or. dorms(2)) then
                          if (dorms(1)) then
                            call strfr (abs(buffer(elo(1)+ii,kp,lp,jf)-
     +                                  buffer(eho(1)+ii,kp,lp,jf))/2.0,
     +                                  '(1pe12.5)', str(ipt:), il)
                            ipt = ipt + il + 1
                          else
                            str(ipt:) = '0.0'
                            ipt = ipt + 4
                          end if
                          if (dorms(2)) then
                            call strfr (abs(buffer(elo(2)+ii,kp,lp,jf)-
     +                                  buffer(eho(2)+ii,kp,lp,jf))/2.0,
     +                                  '(1pe12.5)', str(ipt:), il)
                          else 
                            str(ipt:) = '0.0'
                          end if
                        end if
                        call logwrite (str, more)
                        if (.not.more) goto 999
                      end do
                    end if
                  end if
                end do
              end do
100           continue
              call pgsci (1)
            end do
c
c  Redefine window if plotting interactively.  
c
            if (dointer) then
              call output (' ')
              call output (' ')
              call prompt (ans, il2,
     +             'Would you like to redefine the window (y/n): ')
              if (ans.eq.'y' .or. ans.eq.'Y') then
                call getwin (xaxis, yaxis, xlo, xhi, ylo, yhi)
                devdef = pdev
                pdev = ' '
                redef = .true.
                new = .true.
              end if
            end if
          end if
        end if
      end do
999   call pgend
c
      end
c
c
      subroutine pltitle (npol, polstr, title, cols, doavall)
c-----------------------------------------------------------------------
c     Write the plot title, with polarizations in different
c     colours reflecting the colours they are plotted in.
c
c     Input
c       npol       Number of polarizations read.
c       polstr     What the poalrizaitons are
c       title      The rest of the title
c       cols       COlours
c       doavall    True if averaging everything on plot together
c
c-----------------------------------------------------------------------
      integer npol, cols(*)
      character*2 polstr(*)
      character title*(*)
      logical doavall
cc
      integer i, len1, i1
      real xlen, ylen, xloc, vlen
c-----------------------------------------------------------------------
c
c Find total length that title string will be
c
      vlen = 0.0
      do i = 1, npol
        i1 = len1(polstr(i))
        call pglen (5, polstr(i)(1:i1), xlen, ylen)       
        vlen = vlen + 1.2*xlen
      end do
      i1 = len1(title)
      call pglen (5, title(1:i1), xlen, ylen)
      vlen = vlen + xlen
c
c Start location at which to start writing
c
      xloc = 0.5 - vlen/2.0
c
c Now write multi-colour polarization strings
c
      do i = 1, npol
        i1 = len1(polstr(i))
        if (.not.doavall) call pgsci (cols(i))
        call pgmtxt ('T', 2.0, xloc, 0.0, polstr(i)(1:i1))
c
        call pglen (5, polstr(i)(1:i1), xlen, ylen)       
        xloc = xloc + 1.2*xlen
      end do
      xloc = xloc + 0.8*xlen
c
c Write rest of title
c 
      call pgsci (1)
      call pgmtxt ('T', 2.0, xloc, 0.0, title)
      end
c
c
      subroutine pntful (dobase, pl2dim, pl3dim, pl4dim, maxpnt,
     +   maxbase, maxpol, maxfile, ifile, plfidx, a1a2, npts)
c-----------------------------------------------------------------------
c     Tell user when bits of plot buffers fill up. 
c
c  Input
c    dobase        Separate baseline plots
c    pl*dim        Sizes of last 3 BUFFER dimensions (baseline,
c                  polarization and file)
c    maxpnt        Maximum number of points allowed for each
c                  baseline and polarization combination
c    maxbase       Size of first dimension of NPTS 
c    maxpol        Size of second dimension of NPTS
c    maxfile       Size of third dimension of NPTS
c    ifile         File number 
c    plfidx        The BUFFER index into which this file goes
c    npts          Number of points found so far for each combinaiton
c                  of baseline, polarization and file
c
c-----------------------------------------------------------------------
      implicit none
c
      integer pl2dim, pl3dim, pl4dim, maxfile, maxpnt, maxbase,
     +  maxpol, ifile, npts(maxbase,maxpol,maxfile), a1a2(maxbase,2), 
     +  plfidx
      logical dobase
cc
      character str1*2,aline*80
      integer i, j
      character itoaf*2,stcat*80
c-----------------------------------------------------------------------
c
c Loop over size of BUFFER polarization dimension
c
      do j = 1, pl3dim
c
c Loop over size of BUFFER baseline dimension
c
        do i = 1, pl2dim
          if (npts(i,j,plfidx).eq.maxpnt) then
c
c No more room for this combination
c
            str1 = itoaf(ifile)
c
            if (dobase) then
               if (pl4dim.gt.1) then
                 call bug ('w',
     +            'Buffer for baseline '//itoaf(a1a2(i,1))//'-'//
     +            itoaf(a1a2(i,2))//', pol''n '//itoaf(j)//
     +            ' filled for file # '//itoaf(ifile))
               else
		  aline = stcat(
     *			stcat('Buffer for baseline '//itoaf(a1a2(i,1)),
     *			      '-'//itoaf(a1a2(i,2))),
     *			stcat(', pol''n '//itoaf(j),
     *			      ' filled while reading file # '//
     *				itoaf(ifile)))
		  call bug('w',aline)
               end if
            else
               if (pl4dim.gt.1) then
                 call bug ('w', 'Plot buffer filled for polarization '//
     +                     itoaf(j)//' for file # '//itoaf(ifile))  
               else
                 call bug ('w', 'Plot buffer filled for polarization '//
     +                     itoaf(j)//' while reading file # '
     *			   //itoaf(ifile))  
               end if
            end if
          end if
        end do
      end do
c
      end
c
c
      subroutine setlab (xory, units, axis, dozero, label, opt)
c-----------------------------------------------------------------------
c     Set axis label
c
c  Input:
c    xory     x or y axis
c    units    Units for u and v
c    axis     Axis type
c    dozero   Plot x,y=0
c  Output:
c    label    Label
c    opt      Axis options string for pgplot
c-----------------------------------------------------------------------
      implicit none
      character*(*) axis, label, xory*1, units*10, opt*(*)
      logical dozero
cc
      integer il1, len1
c-----------------------------------------------------------------------
      if (axis.eq.'time') then
        label = 'Time'
      else if (axis.eq.'dtime') then
        label = 'Time (days)'
      else if (axis.eq.'hangle') then
        label = 'Hour Angle'
      else if (axis.eq.'dhangle') then
        label = 'Hour Angle (hours)'
      else if (axis.eq.'parang') then
        label = 'Parallactic Angle (degrees)'
      else if (axis.eq.'lst') then
        label = 'Local Sidereal Time (hours)'
      else if (axis.eq.'az') then
        label = 'Azimuth (degrees)'
      else if (axis.eq.'el') then
        label = 'Elevation (degrees)'
      else if (axis.eq.'jyperk')then
	label = 'System gain [Jy/K]'
      else if (axis.eq.'rms')then
	label = 'Theoretical noise rms [Jy]'
      else if (axis.eq.'airmass')then
	label = 'Airmass [1/sin(el)]'
      else if (axis.eq.'uvdistance') then
        label = '(u\u2\d + v\u2\d)\u1/2\d'//units
      else if (axis.eq.'uu' .or. axis.eq.'uc') then
        label = 'u'//units
      else if (axis.eq.'vv' .or. axis.eq.'vc') then
        label = 'v'//units
      else if (axis.eq.'uvangle') then
        label = 'uv p.a. (degrees)'
      else if (axis.eq.'amplitude') then
        label = 'Amplitude'
      else if (axis.eq.'phase') then
        label = 'Phase (degrees)'
      else if (axis.eq.'real') then
        label = 'Real'
      else if (axis.eq.'imag') then
        label = 'Imaginary'
      else
        call bug ('w', 'Unrecognized '//xory//' axis')
        label = 'unknown'
      end if
c
c  Set axis options
c
      opt = 'BCNST'
      if (axis.eq.'time' .or. axis.eq.'hangle') opt(6:) = 'HZO'
      if (dozero) then
        il1 = len1(opt) + 1
        opt(il1:il1) = 'A'
      end if
c
      end
c
c************************************************************************
      subroutine setval (axis, ha, u, v, uvdist, uvpa, fday, 
     +                   parang, lst, az, el, jyperk, rms,
     +			 data, ichan, freq, val, ok)
c
      implicit none
      complex data
      double precision fday, freq(*), ha, lst, az, el
      real val, u, v, uvdist, uvpa, parang, jyperk, rms
      character axis*(*)
      integer ichan
      logical ok
c
c     Set the value of the desired quantity
c
c  Input:
c    axis     axis type:  u, uc, v, vc, time, dtime, 
c             real, imag, amp, phase
c    ha       Hour angle in seconds
c    u,v      u and v in Klambda
c    uvdist   sqrt(u**2 + v**2) in Klambda
c    uvpa     uv position angle (999 if coudn't evaluate)
c    fday     Fractional day since begining of observation
c    parang   Parallactic Angle
c    lst      LST in radians.
c    az       Azimuth in radians.
c    el       Elevation in radians.
c    jyperk   System gain, in Jy/K.
c    rms      Theoretical visibility noise variance, in flux units.
c    data     Complex visibility
c    ichan    CHannel number
c    freq     Array of frequencies for each channel
c  Output:
c    val      Value 
c    ok       True if value is a valid number to plot
c
c-----------------------------------------------------------------------
      include 'mirconst.h'
      ok = .true.
      if(axis.eq.'uvdistance') then
        val = uvdist * freq(ichan) / freq(1)
      else if (axis.eq.'uu' .or. axis.eq.'uc') then
        val = u * freq(ichan) / freq(1)
      else if (axis.eq.'vv' .or. axis.eq.'vc') then
        val = v * freq(ichan) / freq(1)
      else if (axis.eq.'uvangle') then
        val = uvpa
        if (uvpa.eq.999.0) ok = .false.
      else if (axis.eq.'parang') then
        val = 180./DPI * parang
      else if (axis.eq.'lst') then
	val = 12/PI * lst
      else if (axis.eq.'az') then
        val = 180./PI * az
      else if (axis.eq.'el') then
        val = 180./PI * el
      else if (axis.eq.'airmass') then
	val = 1/sin(el)
      else if (axis.eq.'jyperk') then
	val = jyperk
      else if (axis.eq.'rms') then
	val = sqrt(rms)
      else if (axis.eq.'dtime') then
c
c Fractional days
c
        val = fday
      else if (axis.eq.'time') then
c
c Seconds
c
        val = fday * 24.0 * 3600.0
      else if (axis.eq.'hangle') then
c
c Seconds
c
        val = ha
      else if (axis.eq.'dhangle') then
c
c Fractional hours
c
        val = ha / 3600.0
      else
        call setvl2 (axis, data, val)
      end if
c
      end
c
c
      subroutine setvl2 (axis, data, val)
c-----------------------------------------------------------------------
c     Set value from complex visibility
c
c  Input:
c   axis      axis type: amp, phase, real, imag  only
c   data      visibility
c  Output:
c   val       axis value
c
c-----------------------------------------------------------------------
      implicit none
c
      character*(*) axis
      complex data
      real val, amp, phase
c-----------------------------------------------------------------------
      if (axis.eq.'amplitude') then
        call amphase (data, val, phase)
      else if (axis.eq.'phase') then
        call amphase (data, amp, val)
      else if (axis.eq.'real') then
        val = real(data)
      else if (axis.eq.'imag') then
        val = aimag(data)
      end if
c
      end
c
c
      subroutine shorng (axis, type, rlo, rhi)
c------------------------------------------------------------------------
c     Write current axis range to screen for user's perusal
c
c------------------------------------------------------------------------
      implicit none
c
      character axis*1, type*(*)
      real rlo, rhi
cc
      real rem, tss, tes
      integer tsd, tsh, tsm, ted, teh, tem, il, len1, i
      character ss*1, se*1, aline*132
c------------------------------------------------------------------------
      if (type.eq.'time') then
        rem = rlo / 3600.0 
        tsd = int(rem / 24.0)
        rem = rem - tsd*24.0    
        tsh = int(rem)
        rem = (rem - tsh) * 60.0
        tsm = int(rem)
        tss = (rem - tsm) * 60.0
c
        rem = rhi / 3600.0 
        ted = int(rem / 24.0)
        rem = rem - ted*24.0    
        teh = int(rem)
        rem = (rem - teh) * 60.0
        tem = int(rem)
        tes = (rem - tem) * 60.0
c
        write (aline, 10) axis, tsd, tsh, tsm, tss, ted, teh, tem, tes
10      format ('Current ', a1, '-range is : ', i2, ' ', i2, ' ', i2, 
     +          ' ', f5.2, ' to ', i2, ' ', i2, ' ', i2, ' ', f5.2)
        il = len1(aline)
        do i = 23, il
          if (aline(i:i).eq.' ' .and. aline(i+1:i+1).ne.'t' .and.
     +        aline(i-1:i-1).ne.'o') aline(i:i) = '0'
        end do
      else if (type.eq.'hangle') then
        ss = '+'
        if (rlo.lt.0.0) ss = '-'
        rem = abs(rlo) / 3600.0 
        tsh = int(rem)
        rem = (rem - tsh) * 60.0
        tsm = int(rem)
        tss = (rem - tsm) * 60.0
c
        se = '+'
        if (rhi.lt.0.0) se = '-'
        rem = abs(rhi) / 3600.0 
        teh = int(rem)
        rem = (rem - teh) * 60.0
        tem = int(rem)
        tes = (rem - tem) * 60.0
c
        write (aline, 20) axis, ss, tsh, tsm, tss, se, teh, tem, tes
20      format ('Current ', a1, '-range is : ', a1, i2, ' ', i2, ' ',
     +          f5.2, ' to ', a1 , i2, ' ', i2, ' ', f5.2)
        il = len1(aline)
        do i = 23, il
          if (aline(i:i).eq.' ' .and. aline(i+1:i+1).ne.'t' .and.
     +        aline(i-1:i-1).ne.'o') aline(i:i) = '0'
        end do
      else 
        write (aline, 30) axis, rlo, rhi
30      format ('Current ', a1, '-range is : ', 1pe12.4, 
     +          ' to ', 1pe12.4)
      end if
      call output (aline)
c
      end 
c
c
      subroutine sortbas (maxbase, nplot, a1a2, order)
c-----------------------------------------------------------------------
c     Find pointers to the list of baselines to plot so that
c     they are plotted in increasing baseline order; 1-2, 1-3 etc
c
c   Input
c     maxbase    Size of first dimension of A1A2 and ORDER
c     nplot      Number of baselines to plot
c     a1a2       Antenna numbers for each baseline to plot
c   Output
c     order      Plot the baselines in the order ORDER(1:NPLOT)
c
c-----------------------------------------------------------------------
      integer maxbase, nplot, a1a2(maxbase,2), order(maxbase)
cc
      integer i, j
c-----------------------------------------------------------------------
c
c  Convert to baseline number and sort
c
      do i = 1, nplot
        order(i) = 256*a1a2(i,1) + a1a2(i,2)
      end do
      call sorti (order, nplot)
c
c  Find list giving order to plot in
c
      do i = 1, nplot
        do j = 1, nplot
          if (256*a1a2(j,1)+a1a2(j,2).eq.order(i)) then
             order(i) = j
             goto 100
          end if
        end do
100     continue
      end do
c
      end
c
c
      subroutine telluse (ivis, ifile, dobase, maxbase, maxpol, 
     +                    pl2dim, pl3dim, pl4dim, npts, a1a2, none)
c-----------------------------------------------------------------------
c     Tell the user what happened so far
c
c Input
c   pl2dim   size of baseline dimension of BUFFER
c   pl3dim   size of polarization dimension of BUFFER
c   pl4dim   size of file dimension of BUFFER
c
c-----------------------------------------------------------------------
      implicit none
c
      integer ivis, ifile, maxbase, maxpol, npts(maxbase,maxpol),
     +  a1a2(maxbase,2), pl2dim, pl3dim, pl4dim
      logical dobase, none
cc
      character aline*80
      integer len1, i, nsum, j
      logical more, nunloc
c
c  Externals.
c
	character itoaf*8,stcat*80
c
c-----------------------------------------------------------------------
      if (pl4dim.gt.1) then
	aline = stcat('Read '//itoaf(ivis),
     *		      ' visibilities from file '//itoaf(ifile))
      else
	aline = stcat('Read '//itoaf(ivis),
     *		      ' visibilities from all files')
      end if
      call logwrite (aline(1:len1(aline)), more)
      call logwrite (' ',more)
c
      nunloc = .true.
      if (dobase) then
        do i = 1, pl2dim
          nsum = 0
          do j = 1, pl3dim
            nsum = nsum + npts(i,j)
          end do
          if (nsum.gt.0) then
            nunloc = .false.
	    aline = stcat(
     *		      stcat('Baseline '//itoaf(a1a2(i,1)),
     *			    '-'//itoaf(a1a2(i,2))),
     *		      stcat(', plot '//itoaf(nsum),' points'))
            call logwrite (aline(1:len1(aline)), more)
          end if
        end do
      else 
        nsum = 0
        do j = 1, pl3dim
          nsum = nsum + npts(1,j)
        end do
        if (nsum.gt.0) then
          nunloc = .false.
	  aline = stcat('Plot '//itoaf(nsum),' points')
          call logwrite (aline(1:len1(aline)), more)
        end if
      end if
      call output (' ')
c
      if (pl4dim.gt.1) then
        if (nunloc) call logwrite
     +   ('There are no points to plot in this file; check axis ranges',
     +     more)
      else
        if (nunloc) call logwrite
     +   ('There are no points to plot; check axis ranges', more)
      end if
c
      ivis = 0
      if (.not.nunloc) none = .false.
c     
      end
c
c
      subroutine timdc2 (aline, tlo, thi, ok)
c----------------------------------------------------------------------
c     Decode HH MM S.S  HH MM SS.S string into two floating point
c     numbers in seconds.  Probably won't deal with all stupid
c     formats, but should be good enough.
c
c  Input:
c    aline     String
c  Output:
c    tlo       Start time in seconds
c    thi       End time in seconds
c    ok        If false, decoding failed
c     
c----------------------------------------------------------------------
      implicit none
c
      character*(*) aline
      real tlo, thi
      logical ok
cc
      integer ilen, ib, i, s
      double precision t(6)
c----------------------------------------------------------------------
      ilen = len(aline)
      if (ilen.eq.0) then
         ok = .false.
      else
c
c Extract start and end HH MM SS.S
c
        ib = 1
        i = 1
        ok = .true.
c
        do while (i.le.6 .and. ok)
          call getval (ilen, aline, ib, t(i), ok)
          i = i + 1
        end do
c
c Convert to seconds
c
        s = 1
        if (t(1).lt.0.0d0) s = -1
        tlo = s * (3600.0*abs(t(1)) + 60.0*t(2) + t(3))
        s = 1
        if (t(4).lt.0.0d0) s = -1
        thi = s * (3600.0*abs(t(4)) + 60.0*t(5) + t(6))
      end if
c
      end
c
c
      subroutine timdec (aline, tlo, thi, ok)
c----------------------------------------------------------------------
c     Decode DD HH MM S.S  DD HH MM SS.S string into two floating point
c     numbers in seconds.  Probably won't deal with all stupid
c     formats, but should be good enough.
c
c  Input:
c    aline     String
c  Output:
c    tlo       Start time in seconds
c    thi       End time in seconds
c    ok        If false, decoding failed
c     
c----------------------------------------------------------------------
      implicit none
c
      character*(*) aline
      real tlo, thi
      logical ok
cc
      integer ilen, ib, i
      double precision t(8)
c----------------------------------------------------------------------
      ilen = len(aline)
      if (ilen.eq.0) then
         ok = .false.
      else
c
c Extract start and end DD HH MM SS.S
c
        ib = 1
        i = 1
        ok = .true.
c
        do while (i.le.8 .and. ok)
          call getval (ilen, aline, ib, t(i), ok)
          i = i + 1
        end do
c
c Convert to seconds
c
        tlo = 3600.0*24.0*t(1) + 3600.0*t(2) + 60.0*t(3) + t(4)
        thi = 3600.0*24.0*t(5) + 3600.0*t(6) + 60.0*t(7) + t(8)
      end if
c
      end
c
c
      subroutine track (lin, vupd)
c-------------------------------------------------------------------
c     Track the change of some uv variables.  If they change, then
c     we need to reset the averaging accumulators
c
c  Input:
c    lin     Handle of file
c  Output:
c    vupd    "Variable handle" to track the needed variables.
c
c-------------------------------------------------------------------
      implicit none
c
      integer lin, vupd
c--------------------------------------------------------------------
      call uvvarini (lIn, vupd)
      call uvvarset (vupd, 'source')
      call uvvarset (vupd, 'restfreq')
      call uvvarset (vupd, 'ra')
      call uvvarset (vupd, 'dra')
      call uvvarset (vupd, 'dec')
      call uvvarset (vupd, 'ddec')
c
      end
c
c
      subroutine unwrap (n, phs)
c-----------------------------------------------------------------------
c     Unwrap phases
c
c------------------------------------------------------------------------
      integer n
      real phs(n)
cc
      real theta0
      integer i
c------------------------------------------------------------------------
      theta0 = phs(1)
      do i = 2, n
         phs(i) = phs(i) - 360*nint((phs(i)-theta0)/360.0)
         theta0 = 0.5*(phs(i) + theta0)
      end do
c
      end
c
c
      subroutine uvdes (doflag, doall, dofqav, maxant, maxbase,
     +   maxchan, 
     +   basmsk, polmsk, data, goodf, nfiles, npols, nbases, npnts,
     +   baseday, dayoff)
c-----------------------------------------------------------------------
c     Read through the data once, finding out the number of
c     selected baselines polarizations and files.
c
c  Output:
c    doflag    Plot flagged data
c    doall     Plot all data
c    dofqav    Average all channels into 1.
c    nfiles    Number of files read
c    npols     Number of polarizations encountered
c    nbases    Number of baselines encountered
c    npnts     Number of valid points encountered.
c    baseday   Reference day from first file
c    dayoff    Offset to subtract from day to make fractional days
c-----------------------------------------------------------------------
      implicit none
c
      double precision baseday
      integer maxchan, dayoff
      complex data(maxchan)
      logical goodf(maxchan), doall, doflag, dofqav
      integer nfiles, npols, nbases, polmsk(-8:4), maxant, maxbase,
     +  basmsk(maxbase), npnts
cc
      double precision preamble(4)
      integer lin, nread, ia1, ia2, polidx, basidx, i, nkeep
      character*80 line
c
      logical uvdatopn
c-----------------------------------------------------------------------
      npnts = 0
      npols = 0
      nbases = 0
      call output (' ')
      call output ('Pass 1: determine data characteristics')
c
c Loop over files
c
      call uvdatgti ('nfiles', nfiles)
      do i = 1, nfiles
	if(.not.uvDatOpn(lin))call bug('f','Error opening inputs')
c
c Read first visbility (making variables available)
c
        call getdat (preamble, data, goodf, maxchan, nread,
     *					dofqav, doflag, doall)
c
        if (i.eq.1) then
c
c Get reference day
c
          call uvrdvrd (lin, 'time', baseday, 0.0d0)
          baseday = baseday + 0.5
          dayoff = int(baseday)
        end if
c
        do while (nread.ne.0)
c
c Does this visibility have any data that we want ?
c
          call goodat ( nread, goodf, nkeep)
c
c Find new polarization
c
          if (nkeep.gt.0) then
            call uvdatgti ('pol', polidx)
            if (polidx.lt.-8 .or. polidx.gt.4) call bug ('f',
     +         'Invalid polarization encountered')
            if (polmsk(polidx).eq.0) then
              npols = npols + 1
              polmsk(polidx) = npols
            end if
c
c Find new baseline
c
            call basant (preamble(4), ia1, ia2)
            basidx = ia1 + ia2*(ia2-1)/2
            if (ia1.gt.maxant .or. ia2.gt.maxant) then
              write (line, 150) ia1, ia2, maxant
150           format ('Antenna number too large for baseline ', i2,
     +                '-', i2, ' Need antenna # < ', i2)
              call bug ('f', line)
            end if
c
            if (basmsk(basidx).eq.0) then
              nbases = nbases + 1
              basmsk(basidx) = nbases
            end if
          end if
c
	  npnts = npnts + nkeep
c
c Read another visibility
c
          call getdat (preamble, data, goodf, maxchan, nread,
     *					dofqav, doflag, doall)
        end do
        call uvdatcls
      end do
c
c Reinitialize masks
c
      do i = -8, 4
        polmsk(i) = 0
      end do
      do i = 1, maxant+maxbase
        basmsk(i) = 0
      end do
c
c Reinitialize UVDAT routines
c
      call uvdatrew
c

      end
c
c
      subroutine uvfish (nfiles, npols, nbases, baseday, dayoff)
c-----------------------------------------------------------------------
c     Try and guess at how many polarizations and baselines
c     we should expect to find. 
c
c
c  Output:
c    nfiles    Number of files read
c    npols     Number of polarizations encountered
c    nbases    Number of baselines encountered
c    baseday   Reference day from first file
c    dayoff    Offset to subtract from day to make fractional days
c-----------------------------------------------------------------------
      implicit none
c
      double precision baseday
      integer nfiles, npols, nbases, dayoff
cc
      integer pols(12), pols2(12), lin, i, j, npols2, nants
      logical found
c
      logical uvdatopn, uvdatprb
c-----------------------------------------------------------------------
c
c Extract number of files
c
      call uvdatgti ('nfiles', nfiles)
c
c Now deal with polarizations.  See if the user selected them with
c "select" or "stokes" keyword.   If they select nothing (or 
c everything) then NPOLS will end up at 12.  NO-ONE will EVER ask 
c for 12 polarizations, and so I can assume then that they have asked
c for everything in the file in this case.
c
      npols = 0
      do i = -8, 4
        if (i.ne.0 .and. uvdatprb('polarization', dble(i))) then
          npols = npols + 1
          pols(npols) = i
        end if
      end do
      if (npols.eq.12) then
        do i = 1, 12
          pols(i) = 0
        end do
        npols = 0
      end if
c
c Now fish out what they set with the 'stokes=' selection
c
      call uvdatgti ('npol', npols2)
      call uvdatgti ('pols', pols2)
c
c Now merge the two lists into one
c
      if (npols2.gt.0) then
        do j = 1, npols2
          found = .false.
          do i = 1, npols
             if (pols2(j).eq.pols(i)) found = .true.
          end do
          if (.not.found) then
            npols = npols + 1
            pols(npols) = pols2(j)
          end if
        end do
      end if
c
c If the user didn't select anything, read the first set of
c polarizations from the first file
c
      if(.not.uvdatopn(lin))call bug('f','Error opening input')
      call uvnext (lin)
c
c Get reference day
c
      call uvrdvrd (lin, 'time', baseday, 0.0d0)
      baseday = baseday + 0.5
      dayoff = int(baseday)
c
      if (npols.eq.0) then
        call uvrdvri (lin, 'npol', npols, 1)
        if (npols.eq.0) call bug ('f', 
     +    'There are no polarizations present in the data')
      end if
c
c Now deal with baselines.  We don't do a very good job here.  All
c we can do is find what the NANTS variable is set to.  Note that
c programs such as UVCAT just copy NANTS over, even if you select
c a subset of baseline.  RJS very stubborn (what's new) on this issue.  
c
      call uvrdvri (lin, 'nants', nants, 0)
      nbases = nants * (nants+1) / 2
c
c Close uv file and reinitialize UVDAT routines
c
      call uvdatcls
      call uvdatrew
c
      end
************************************************************************
