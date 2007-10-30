      program cgcurs
c-----------------------------------------------------------------------
c= CGCURS - Read quantities with cursor from images on a PGPLOT device
c& nebk
c: plotting
c+
c	CGCURS displays an image via a contour plot or a grey scale
c	on a PGPLOT device.  The cursor is then used to read image
c	values, or to evaluate image statistics in a polygonal region,
c	or to write a polygonal region definition to a text file.
c	
c	When using cursor options, generally, click the right button
c	(enter X) to exit the function, click the left button (enter A)
c	to add a location, and click the middle button (enter D) to
c	delete a location.
c
c@ in
c	The input image.
c@ type
c	Specifies the image type given in the IN keyword.  Minimum
c	match is supported.  Choose from:
c
c	"contour"  (contour)
c	"grey"     (grey scale)
c
c	Default is "contour"
c@ region
c	Region of interest.  Choose only one spatial region (bounding 
c	box only supported), but as many spectral regions (i.e., 
c	multiple IMAGE specifications) as you like.  If you display a
c	3-D image, the cursor options are activated after each sub-plot 
c	(channel or group of channels; see CHAN below) is drawn.  
c	Default is full image
c@ xybin
c	Upto 4 values.  These give the spatial increment and binning
c	size in pixels for the x and y axes to be applied to the selected
c	region.   If the binning size is not unity, it must equal the 
c	increment.  For example, to bin up the image by 4 pixels in 
c	the x direction and to pick out every third pixel in the y 
c	direction, set XYBIN=4,4,3,1
c	Defaults are 1,XYBIN(1),XYBIN(1),XYBIN(3)
c@ chan
c	2 values. The first is the channel increment, the second is
c	the number of channels to average, for each sub-plot.  Thus
c	CHAN=5,3  would average groups of 3 channels together, starting
c	5 channels apart such as: 1:3, 6:8, 11:13 ...   The channels
c	available are those designated by the REGION keyword.  A new
c	group of channels (sub-plot) is started if there is a
c	discontinuity in the REGION selected channels (such as
c	IMAGE(10,20),IMAGE(22,30).
c
c	Defaults are 1,1
c@ slev
c	2 values.   First value is the type of contour level scale
c	factor.  "p" for percentage and "a" for absolute.   Second
c	value is the level to scale LEVS by.  Thus  SLEV=p,1  would
c	contour levels at LEVS * 1% of the image peak intensity.
c	Similarly, SLEV=a,1.4e-2   would contour levels at LEVS * 1.4E-2
c	Default is no additional scaling of LEVS
c@ levs
c	Levels to contour for first image, are LEVS times SLEV
c	(either percentage of the image peak or absolute).
c	Defaults try to choose something sensible
c@ range
c	3 values. The grey scale range (background to foreground), and
c	transfer function type.  The transfer function type can be one
c	of "lin" (linear), "log" (logarithmic), "heq" (histogram equal-
c	ization), and "sqr" (square root).  See also OPTIONS=FIDDLE which
c	is in addition to the selections here.
c
c	Default is linear between the image minimum and maximum
c	If you wish to just give a transfer function type, set
c	range=0,0,heq   say.
c@ device
c	The PGPLOT plot device, such as plot.plt/ps. No default.
c@ nxy
c	Number of sub-plots in the x and y directions on the page.
c	Defaults choose something sensible
c@ labtyp
c       Two values.  The spatial label type of the x and y axes.
c       Minimum match is active.  Select from:
c       
c	"hms"       the label is in H M S (e.g. for RA)
c	"dms"       the label is in D M S (e.g. for DEC)
c	"arcsec"    the label is in arcsecond offsets
c	"absdeg"    the label is in degrees
c	"reldeg"    the label is in degree offsets
c		    The above assume the  pixel increment is in radians.
c	"abspix"    the label is in pixels
c	"relpix"    the label is in pixel offsets
c	"abskms"    the label is in Km/s
c	"relkms"    the label is in Km/s offsets
c	"absghz"    the label is in GHz
c	"relghz"    the label is in GHz offsets
c	"abslin"    the label is in linear coordinates as defined by 
c		    the header you might call this the natural axis label
c	"rellin"    the label is in offset linear coordinates
c       
c       All offsets are from the reference pixel.  
c	Defaults are "abspix", LABTYP(1) unless LABTYP(1)="hms"
c	whereupon LABTYP(2) defaults to "dms" (for RA and DEC).
c@ options
c	Task enrichment options.  Minimum match is active.
c
c	"fiddle" means enter a routine to allow you to interactively change
c	  the display lookup table.  You can cycle through b&w and colour
c	  displays, as well as alter the transfer function by the cursor 
c	  location, or by selecting predefined transfer functions such as 
c	  histogram equalization, logarithmic, & square root.
c	"wedge" means that if you are drawing a grey scale, also draw
c	  and label a wedge to the right of the plot, showing the map 
c	  of intensity to grey level.  
c
c       "3value"  means label each sub-plot with the appropriate value
c	  of the third axis (e.g. velocity or frequency for an xyv ordered 
c	  cube, position for a vxy ordered cube).
c       "3pixel"  means label each sub-plot with the pixel value of the
c	  the third axis.   Both "3pixel" and "3value" can appear, and both 
c	  will be written on the plot.  They are the average values when
c	  the third axis is binned up with CHAN.  If the third axis is
c	  not velocity or frequency, the units type for "3VALUE" will be 
c	  chosen to be the complement of any like axis in the first 2. 
c	  E.g., the cube is in vxy order and LABTYP=ABSKMS,ARCSEC the units 
c	  for the "3VALUE" label will be arcsec.  If LABTYP=ABSKMS,HMS the 
c	  "3VALUE" label will be DMS (if the third [y] axis is declination).
c
c	"noerase"  Don't erase a snugly fitting rectangle into which the 
c	  "3-axis" value string is written.
c      
c	"unequal" means draw plots with unequal scales in x and y. The
c	  default is that the scales are equal.
c
c	"cursor" means that after drawing each sub-plot, a cursor will
c	  be displayed; striking any key or clicking the relevant mouse 
c	  button (left) causes the location and value of the pixel under 
c	  the cursor to be listed on the terminal.   On terminals, enter 
c	  "x" to exit the cursor.  On workstations, click the relevant button 
c	  (generally the right one).
c	"mark" When in "CURSOR" mode, mark the locations selected. If
c	  OPTIONS=STATS is activated, mark the minimum and maximum pixel 
c	  locations too.
c	"box" When in "CURSOR" mode, rather than listing the value of the
c	  of the pixel under the cursor, list the peak value in a 5x5 pixel 
c	  box centred on the pixel under the cursor.
c	"nearest"  When the cursor is used to select a location, force that
c	  location to be the nearest image pixel, rather than the default 
c	  which allows fractional pixel locations.
c
c	"stats"  means that after drawing each sub-plot, you get the
c	  opportunity to define a polygonal region with the cursor (A to 
c	  add a vertex, D to delete the previous vertex, X to exit; or use 
c	  the three mouse buttons) inside of which image statistics are 
c	  evaluated.
c
c	"region" means use the cursor to define a polygonal region that gets
c	  gets written to a log file as the REGION keyword. The cursor 
c	  behaves as described above for the "stats" option.  You can the 
c	  use this in other programs as "region=@filename"
c	"abspix" means write the region of interest in absolute integer pixels
c	  instead of arcseconds relative to the reference pixel
c
c	"logfile"  When the "cursor" or "stats" are activated, then this
c	  writes the results to log files (cgcurs.curs and cgcurs.stat) as 
c	  well as the screen.
c	"cgspec"  With OPTIONS=CURSOR and LOGFILE, the output log file is
c	  is one with commands appropriate for input to CGSPEC's OLAY keyword.
c	"cgdisp"  With OPTIONS=CURSOR and LOGFILE, the output log file  is
c	  one with commands appropriate for input to CGDISP's OLAY keyword.
c
c         Note that if you specify both CGSPEC and CGDISP then lines 
c	  appropriate to both these programs are written into the log file.  
c	  You can then copy the log file and retain the CGDISP lines in one 
c	  file, and the CGSPEC lines in the other.
c
c@ csize
c	Two values.  Character sizes in units of the PGPLOT default
c	(which is ~ 1/40 of the view surface height) for the plot axis
c	labels and the velocity/channel labels.
c	Defaults choose something sensible.
c--
c
c  History:
c    nebk 17sep91  Original version from pgdisp
c    nebk 20sep91  Stripped subroutines common with pgdisp into
c                  subspg.for
c    nebk 11oct91  Increase maximum size of output region of 
c                  interest line to 400 characters.
c    nebk 14oct91  Carry over recent fixes to boxes.for which need
c                  to be installed here (see curstat & polyruns)
c                  and make fix to reported cursor option pixels
c    nebk/mjs
c         13mar92  Name Change:  pgcurs -> cgcurs
c    nebk 06apr92  Adapted to use memalloc routines
c    nebk 10apr92  Add division of sum by beam area where appopriate
c    nebk 29apr92  Rename *pg subroutines to *cg
c    rjs   1may92  Calls pgcurs rather than prompt. Also standardised
c		   the default plot device (grrr).
c    nebk  5may92  Add options=abspix and make region of interest
c                  default output type arcsec,poly
c    nebk 18may92  Change labtyp keywords to bring them in line with
c                  cgdisp and cgsubs.for.  Abstract limtr to limtrcg
c    nebk 26may92  curpos was not recognizing blanks.  
c    nebk 03jun92  Change call to chnselcg and better redisplaying
c                  behaviour (still not perfect)
c    nebk 28jun92  Add options=log
c    nebk 04jul92  Add options=cgspec,cgdisp,mark and fix small errors
c		   in the coordinate conversions in curpos. Use deghmscg
c		   for more precision in RA and call to conlevcg changed.
c    nebk 10jul92  Go to optcg from options for warnings only.  CHange
c		   to op=unequal and add image plane to region output.
c 		   Remove call to boxmask
c    nebk 17jul92  Keep vertices floating point in cureg; curstat was
c                  reporting min and max off by 1 but did stats ok.
c 		   Cursor/Stats were not coping with log transfer fn.
c		   Add channel to curpos output
c    nebk 16oct92  Add informational message for options=unequal use
c    nebk 28nov92  Add frequency and velocity label types and change
c                  linear -> abslin
c    rjs  24feb93  Rename routine open to opens, to avoid name
c		   conflict on HP machines.
c    mjs  12mar93  Use maxnax.h file instead of setting own value.
c    mjs  13mar93  pgplot subr names have less than 7 chars.
c    nebk 12may93  Replace options=chan,vel by generic 3pix,3vel
c    nebk 29may93  Replace call to chtonvcg by new PG routine pgqcs
c    nebk 23jun93  Remove need for vpaspcg with calls to pgscs and
c		   change for new call to vpadjcg
c    nebk 24aug93  Add info. about 3rd dimension in "cursor" option. 
c                  Add labtyp "absdeg"  and "reldeg".  Improve curpos 
c                  formatting.  Eradicate tr array except in PGPLOT calls.
c                  Add options=noerase. Bring elimrv into line with
c		   modern boxpoly version and bring new polyruns in
c		   from boxes/boxpolyx.for
c    nebk 10sep93  Add options=box
c    rjs  10nov93  Change txtopena to txtopen.
c    rjs  11nov93  's' flag to boxset.
c    nebk 17nov93  Get flux density from Jy/Pixel models in curstat
c    nebk 14dec93  Change call to new taklogcg. Strip limits to limitscg
c    nebk 09jan94  Convert crpix to double precision. options=nearest
c    nebk 29jan94  Add heq and sqr transfer functions to range keyword
c	           Add options=wedge and fiddle. Implement redisplay
c		   erase and strip viewsize to vpsizcg
c    nebk 02mar94  New call and location for SETLABCG
c    nebk 11mar94  Add spatial binning. Return correct image values when 
c                  "sqr" or "heq" transfer function applied
c    nebk 03jun94  Clarify use of region keyword.  
c    nebk 21jun94  Replace OPENS with call to OPIMCG
c    nebk 30jun94  New call to vpadjcg
c    nebk 08aug94  Remove 's' from boxset which naughty robbie included
c                  in 1991. This broke the ability to handle
c                  discontinuous planes (for 3 years !)  
c    nebk 25aug94  Adapt to write out true world coordinates.  Displayed
c                  plot now linearized at centre of displayed region.
c		   Also call new LAB3CG which labels plot with true 
c                  world coordinates for third axis
c    nebk 23dec94  Make sure selected region no bigger than image
c    nebk 05jan95  Use new PGIMAGE in favour of PGGRAY
c To do:
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'
c
      real wedisp, wedwid, tfdisp
      integer maxlev, maxpos, nxdef, nydef, nbins
      parameter (maxlev = 50, maxpos = 50, nxdef = 4, nydef = 4,
     +   wedisp = 1.0, tfdisp = 0.5, wedwid = 0.05, nbins = 128)
c
      integer ipim, ipnim, ipims
c
      double precision cdelt(maxnax), crval(maxnax), crpix(maxnax)
      real levs(maxlev), pixr(2), tr(6), cs(2), pixr2(2), scale(2), 
     +  tfvp(4), wdgvp(4), cumhis(nbins), dmm(2)
      real slev, xmin, xmax, ymin, ymax, vxmin, vymin, vymax, vx, vy,
     +  vxsize, vysize, vxgap, vygap, ydispb, xdispl, groff, blank, 
     +  epoch
c
      integer blc(3), trc(3), size(maxnax), win(maxnax),
     +  grpbeg(maxchan), ngrp(maxchan), srtlev(maxlev), his(nbins)
      integer nx, ny, nlevs, lin, naxis, k, ierr, pgbeg, iostat, ipage,
     +  ilen, ibin(2), jbin(2), kbin(2), krng(2), nlast, ngrps, lstat, 
     +  lreg, lcurs, jj, wedcod
c
      character ctype(maxnax)*9, labtyp(2)*6
      character in*64, pdev*64, xlabel*40, ylabel*40, xopts*20, 
     +  yopts*20, hard*20, xxopts*22, yyopts*22, trfun*3, levtyp*1
c
      logical do3val, do3pix, eqscale, doblnk, cursor, stats, doreg,
     +  mask, smore, rmore, cmore, dogrey, display, doabs, gaps, dolog,
     +  cgspec, cgdisp, mark, doerase, dobox, near, dowedge, dofid,
     +  first
c
      data ipage, scale /0, 0.0, 0.0/
      data dmm /1.0e30, -1.0e30/
c-----------------------------------------------------------------------
      call output ('CgCurs: version 05-Jan-95')
      call output ('Non-linear coordinates are now partially handled')
      call output ('See "help cgcoords" for explanations')
      call output (' ')
c
c Get user inputs
c
      call inputs (maxlev, in, ibin, jbin, kbin, levtyp, slev, levs, 
     +   nlevs, pixr, trfun, pdev, labtyp, do3val, do3pix, eqscale, 
     +   nx, ny, cs, dogrey, cursor, stats, doreg, doabs, dolog, 
     +   cgspec, cgdisp, mark, doerase, dobox, near, dowedge, dofid)
c
c Open image
c
      call opimcg (maxdim, maxnax, in, lin, naxis, size, epoch, mask,
     +             crpix, cdelt, crval, ctype)
c
c Finish key inputs for region of interest now
c
      call region (in, naxis, size, ibin, jbin, kbin, blc, trc,
     +             win, maxchan, grpbeg, ngrp, ngrps)
c
c Try to allocate memory for images.  Need a copy of the image
c if we have histogram equalized it and want to use the
c "cursor" or "stats" option to find out image values.
c
      call memalloc (ipim,  win(1)*win(2), 'r')
      call memalloc (ipnim, win(1)*win(2), 'i')
      if (trfun.eq.'heq' .and. (cursor .or. stats)) then
        call memalloc (ipims, win(1)*win(2), 'r')
      else
c
c If we don't need a copy of this image, we must still have
c a pointer to pass down
c
        call memalloc (ipims, 1, 'r')
      end if
c
c Open log files
c
      if (cursor .and. dolog) then
        call txtopen (lcurs, 'cgcurs.curs', 'append', iostat)
        if (iostat.ne.0) 
     +    call bug ('f', 'Error opening text file "cgcurs.curs"')
        call output (' ')
        call output ('*** Values under cursor output to cgcurs.curs')
        call output (' ')
        if (cgspec) call txtwrite (lcurs, 'IRREGULAR', 9, iostat)
      end if
      if (stats .and. dolog) then
        call txtopen (lstat, 'cgcurs.stat', 'append', iostat)
        if (iostat.ne.0) 
     +    call bug ('f', 'Error opening text file "cgcurs.stat"')
        call output (' ')
        call output ('*** Statistics output to cgcurs.stat')
        call output (' ')
      end if
      if (doreg) then
        call txtopen (lreg, 'cgcurs.region', 'append', iostat)
        if (iostat.ne.0) 
     +    call bug ('f', 'Error opening text file "cgcurs.region"')
        call output (' ')
        call output ('*** Region of interest output to cgcurs.region')
        call output (' ')
      end if
c
c Compute contour levels or check grey scale for log offset
c
      if (dogrey) then
        call grfixcg (pixr, lin, naxis, size, trfun, pixr2,
     +                groff, blank)
      else
        call conlevcg (.false., maxlev, lin, naxis, size, levtyp,
     +                 slev, nlevs, levs, srtlev)
        blank = -99999999.0
      end if
c
c Linearize axis descriptors if non-pixel labels requested
c
      call linco (lin, labtyp, blc, trc, grpbeg, ngrp, ctype,
     +            crval, crpix, cdelt)
c
c Work out array index limits, coordinate transformation array and
c labels.   Also return header items.
c
      call limitscg (labtyp, blc, trc, naxis, epoch, crpix, cdelt, 
     +   crval, ctype, .false., xmin, xmax, ymin, ymax, ibin, jbin,
     +   tr, xlabel, ylabel)
c
c Work out number of plots per page and number of plots
c
      call nxnycg (nxdef, nydef, ngrps, nx, ny, nlast)
      gaps = .false.
      if (ngrps.eq.1 .or. nx*ny.eq.1) gaps = .true.
c
c Work out if wedge outside or inside subplots. Also work out
c if plotting one wedge per subplot or one wedge for all  
c       
      call wedgincg (dowedge, nx, ny, 1, trfun, wedcod)
c
c Work out default character sizes for axis and channel labels
c
      call defchrcg (nx, ny, cs)
c
c Open plot device
c
      ierr = pgbeg (0, pdev, 1, 1)
      if (ierr.ne.1)then
        call pgldev
        call bug ('f', 'Error opening plot device')
      endif
c
c Useless for harcopy device
c
      call pgqinf ('hardcopy', hard, ilen)
      if (hard.eq.'YES')
     +   call bug ('f', 'This program not useful for hard copy devices')
c
c Step to first sub-plot, set font and basic character size
c
      call pgpage
      call pgscf(1)
c       
c Init OFM routines
c       
      if (dofid) call ofmini
c
c Set label displacements from axes and set PGTBOX labelling
c option strings
c
      call setlabcg (labtyp, ymin, ymax, xdispl, ydispb, xopts, yopts)
c
c Work out view port encompassing all sub-plots. Also return 
c the viewport size of sub-plots.
c
      call vpsizcg (.false., dofid, 0, ' ', ' ', 0, ' ', maxlev,
     +   nlevs, srtlev, levs, slev, nx, ny, cs, xdispl, ydispb, 
     +   .false., wedcod, wedwid, wedisp, tfdisp, labtyp, vxmin, 
     +   vymin, vymax, vxgap, vygap, vxsize, vysize, tfvp, wdgvp)
c
c Adjust viewport increments and start locations if equal scales
c requested or if scales provided by user
c
      call vpadjcg ('NO', eqscale, scale, vxmin, vymin, vymax, nx, ny,
     +   blc, trc, naxis, crval, crpix, cdelt, ctype, tfvp, wdgvp, 
     +   vxsize, vysize)
c
c Set viewport location of first sub-plot
c
      vx = vxmin
      vy = vymax - vysize
c
c Loop over number of subplots
c
      do k = 1, ngrps
         if (mod(k,nx*ny).eq.1 .or. nx*ny.eq.1) ipage = ipage + 1
         jj = k - (ipage-1)*nx*ny
         krng(1) = grpbeg(k)
         krng(2) = ngrp(k)
c
c Redraw this sub-plot until user gets bored
c
         cmore = cursor
         smore = stats
         rmore = doreg
         first = .true.
c
c Set viewport and window for current sub-plot
c
         call pgsvp (vx, vx+vxsize, vy, vy+vysize)
         call pgswin (xmin, xmax, ymin, ymax)
c
c Read in image and save it if necessary
c
         call readimcg (.true., mask, blank, lin, ibin, jbin, krng,
     +         blc, trc, .true., memi(ipnim), memr(ipim), doblnk, dmm)
         if (trfun.eq.'heq' .and. (cursor .or. stats)) 
     +     call copyimcg (win(1)*win(2), memr(ipim), memr(ipims))
c
c Apply transfer function
c
         call pgsci (7)
         if (dogrey) then
           if (trfun.ne.'lin') call apptrfcg (pixr2, trfun, groff, 
     +        win(1)*win(2), memi(ipnim), memr(ipim), nbins,
     +        his, cumhis)
c
c Draw wedge outside of redisplay loop if it will be outside subplots 
c and will not get erased
c
           if (wedcod.eq.1 .or. wedcod.eq.2) then
            call pgsch (cs(1))
            call wedgecg (wedcod, wedwid, jj, trfun, groff, nbins, 
     +                    cumhis, wdgvp, pixr2(2), pixr2(1))
           end if
         end if
c
c Loop while user wants to redraw plot
c
         display = .true.
         do while (display)
c
c Draw grey scale
c
           call pgsci (7)
           if (dogrey) then
             call pgimag (memr(ipim), win(1), win(2), 1, win(1), 1,
     +                    win(2), pixr2(1), pixr2(2), tr)
           else 
c
c Draw contours
c
             call conturcg (blank, .false., win(1), win(2), doblnk,
     +                      memr(ipim), nlevs, levs, tr, 0.0)
           end if
c
c Label and draw axes
c
           call pgsch (cs(1))
           if (first) call axlabcg (gaps, nx, ny, ngrps, nlast, k,
     +       xopts, yopts,xdispl, ydispb, labtyp, xlabel, ylabel, 
     +       xxopts, yyopts)
           call boxcg (first, xxopts, yyopts)
c
c Draw wedge inside subplots and overwrite label ticks
c
           if (wedcod.eq.3) then
            call pgsch (cs(1))
            call wedgecg (wedcod, wedwid, jj, trfun, groff, nbins, 
     +                    cumhis, wdgvp, pixr2(2), pixr2(1))
           end if
c
c Modify lookup table
c
           if (dofid) call ofmmod (tfvp, win(1)*win(2), memr(ipim), 
     +                             memi(ipnim), pixr2(1), pixr2(2))
c
c Write velocity or channel label
c
           if (do3val .or. do3pix) then
             call pgsch (cs(2))
             call pgsci (1)
             call lab3cg (lin, doerase, do3val, do3pix, labtyp,
     +                    grpbeg(k), ngrp(k))
           end if
c
c Cursor options
c
           if (cmore) then
c
c Read value and location under cursor
c
             call pgsci (2)
             call curpos (lin, win(1), win(2), memr(ipim), memr(ipims),
     +           memi(ipnim), labtyp, blc, naxis, cdelt, crpix, 
     +           crval, ctype, ibin, jbin, krng, dolog, lcurs, 
     +           cgspec, cgdisp, mark, dobox, near, trfun, groff)
             cmore = .false.
           end if
c
           display = .false.
           if (smore) then
c
c Find image statistics in polygonal region defined by cursor
c
             call  pgsci (3)
             call curstat (lin, blc, win(1), win(2), memr(ipim),
     +          memr(ipims), memi(ipnim), labtyp, naxis, crval, 
     +          cdelt, crpix, ctype, ibin, jbin, doreg, display, 
     +          smore, dolog, mark, near, lstat, trfun, groff)
           end if
c
           if (.not.display .and. rmore) then
c
c Define polygonal region with cursor
c
             call pgsci (8)
             call cureg (lin, blc, ibin, jbin, krng, near, doabs, size, 
     +         naxis, crval, crpix, cdelt, ctype, labtyp, display, lreg)
           end if
c
c Erase subplot
c
           if (display) call erswincg (xmin, xmax, ymin, ymax)
           first = .false.
         end do
c
c Increment sub-plot viewport locations and row counter
c
         call subinccg (k, nx, ny, vxmin, vymax, vxsize, vysize, 
     +                  vxgap, vygap, vx, vy)
c
c Page plot device
c
         if (jj.eq.nx*ny .and. k.lt.ngrps) call pgpage
      end do
c
c Close up
c
      call memfree (ipim,  win(1)*win(2), 'r')
      call memfree (ipnim, win(1)*win(2), 'i')
      if (trfun.eq.'heq' .and. (cursor .or. stats)) then
        call memfree (ipims, win(1)*win(2), 'r')
      else
        call memfree (ipims, 1, 'r')
      end if
c
      call xyclose(lin)
      if (dolog) then
        if (cursor) call txtclose (lcurs)
        if (stats)  call txtclose (lstat)
      end if
      if (doreg) call txtclose (lreg)
      call pgend
c
      end
c
c
      subroutine cgcur (x, y, ans)
      implicit none
      real x, y
      character ans*1
      call pgupdt
      call pgcurs (x, y, ans)
      call ucase (ans)
      call pgupdt
c
      end
c
c
      subroutine compact (str)
c-----------------------------------------------------------------------
c     COmpact string by removing trailing 0's 
c
c-----------------------------------------------------------------------
      implicit none
c
      character*(*) str
cc
      integer il, len1, i, j
      character line*1000
c-----------------------------------------------------------------------
      il = len1(str)
c
      if (str(il:il).eq.'0') str(il:il) = ' '
      do i = il-1, 1, -1
        if (str(i:i).eq.'0' .and. str(i+1:i+1).eq.' ') then
          str(i:i) = ' '
        else if (str(i:i).eq.'.' .and. str(i+1:i+1).eq.' ') then
          str(i:i) = ' '
        end if
      end do
c
      line = ' '
      j = 1
      do i = 1, il
        if (str(i:i).ne.' ') then
          line(j:j) = str(i:i)
          j = j + 1
        end if
      end do
c
      str = ' '
      str = line(1:len1(line))
c
      end
c
c
      subroutine cureg (lin, blc, ibin, jbin, krng, near, doabs, 
     +   size, naxis, crval, crpix, cdelt, ctype, labtyp, redisp, lreg)
c-----------------------------------------------------------------------
c     Define region of interest with cursor and write to log file.
c
c  Input:
c    lin    Handle of image
c    blc    BLC of image
c    i,jbin Spatial pixel binning values
c    krng   Start plane being displayed and number of planes 
c           averaged in display
c    size   ARray of image dimensions
c    naxis  Number of dsimensions in imgae
c    crval  Array of reference value
c    crpix  Array of reference pixels
c    cdelt  Array of pixel increments
c    ctype  Array of axis types
c    labtyp Axis label types
c    lreg   Handle for output text file
c    near   FOrce cursor to nearest pixel
c  Input/output
c    doabs  Output region in absolute pixels instead of offset arcsec
c  Output
c    redisp Have another go after redisplaying the image
c
c-----------------------------------------------------------------------
      implicit none
c
      integer lin, lreg, ibin, jbin, krng(2), naxis, size(naxis), blc(2)
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      character*(*) ctype(naxis), labtyp(2)
      logical redisp, doabs, near
cc
      include 'mirconst.h'
      double precision rtoa
      parameter (rtoa = 3600.0d0*180.0d0/dpi)
c
      integer nvmax, symb
      parameter (nvmax = 100, symb = 17)
c
      double precision vert(2,nvmax), ww(2), pix(3), pixbs(2), 
     +  win(3), wout(3), chan
      real vx(nvmax), vy(nvmax)
      character str1*30, str2*30, str*60, line*500, ans*1, typei(3)*6,
     +  typeo(3)*6
      integer il1, il2, i, ip, il, maxlen, nv, irad(2), iostat, bin(2)
      logical good, more, rads, ok
c
      integer len1, ci
c----------------------------------------------------------------------
      call output (' ')
      call output ('**********************************')
      call output ('Entering region of interest option')
      call output ('**********************************')
      call output (' ')
      call output ('Click left button   (enter A) to mark vertex')
      call output 
     +  ('Click middle button (enter D) to delete previous vertex')
      call output ('Click right button  (enter X) to finish polygon')
      call output (' ')
c
c Do we have an axes in radians, can't output locations
c in arcsecond offsets otherwise.
c
      call axfndcg ('RAD', 1, ctype(1), irad(1))
      call axfndcg ('RAD', 1, ctype(2), irad(2))
      rads = .true.
      if (irad(1)*irad(2).eq.0) rads = .false.
      if (.not.rads) doabs = .true.
      bin(1) = ibin
      bin(2) = jbin
      typei(1) = 'abspix'
      typei(2) = 'abspix'
      typei(3) = 'abspix'
      typeo(1) = 'arcsec'
      typeo(2) = 'arcsec'
      typeo(3) = 'arcsec'
      chan = (real(2*krng(1)+krng(2))-1.0)/2.0
      call initco (lin)
c
c Get vertices with cursor and join up the dots
c
      more = .true.
      do while (more)
        nv = 0
        call pgupdt
c
c Get vertices with cursor
c
        call pgolin (nvmax, nv, vx, vy, symb)
c
        if (nv.gt.nvmax) then
          write (line,10) nv, nvmax
10        format ('Too many (', i4, ') vertices, max. = ', i4)
          call bug ('w', line(1:len1(line)))
        else if (nv.le.1) then
          call bug ('w', 'Not enough vertices for a region')
        else
c
c Convert to nearest pixel if desired
c
          if (near) then
c
c Rub out old points
c
            call pgqci (ci)
            call pgsci (0)
            call pgpt (nv, vx, vy, symb)
            do i = 1, nv
              ww(1) = vx(i)
              ww(2) = vy(i)
              call nearcon (labtyp, naxis, crval, crpix, cdelt,
     +                      ctype, bin, blc, ww, pix, pixbs)
              vx(i) = ww(1)
              vy(i) = ww(2)
            end do
c
c Draw new points
c
            call pgsci (ci)
            call pgpt (nv, vx, vy, symb)
          end if
          call pgsfs (2)
          call pgslw (2)
c
c Join up the dots.  
c
          call pgpoly (nv, vx, vy)
          call pgupdt
          call pgslw (1)
c
c Convert polygon vertices in linear world coordinates to unbinned
c full image pixels 
c
          do i = 1, nv
            call w2pixcg (dble(vx(i)), 1, labtyp(1), naxis, crval, 
     +                    crpix, cdelt, ctype, vert(1,i), ok)
            call w2pixcg (dble(vy(i)), 2, labtyp(2), naxis, crval, 
     +                    crpix, cdelt, ctype, vert(2,i), ok)
          end do
c
c Eliminate redundant vertices
c
          call elimrvd (nvmax, nv, vert) 
c
c Convert unbinned full image pixels to true arcsec offsets if desired 
c
          if (.not.doabs) then
            do i = 1, nv
              win(1) = vert(1,i)
              win(2) = vert(2,i)
              win(3) = chan
              call w2wco (lin, 2, typei, ' ', win, typeo, ' ', wout)
              vert(1,i) = wout(1)
              vert(2,i) = wout(2)
            end do
          end if
c
c Write out region of interest
c
          line = ' '
          maxlen = len(line)
          if (doabs) then
            line = 'abspix,poly('
          else
            line = 'arcsec,poly('
          end if
          ip = len1(line) + 1
c
          good = .true.
          i = 1
          do while (i.le.nv .and. good)
            if (doabs) then
              call strfi (nint(vert(1,i)), '(i4)', str1, il1)
              call strfi (nint(vert(2,i)), '(i4)', str2, il2)
            else
              call strfd (vert(1,i), '(f15.2)', str1, il1)
              call strfd (vert(2,i), '(f15.2)', str2, il2)
            end if
            str = str1(1:il1)//','//str2(1:il2)
            call compact (str)
            il = len1(str) + 1
            str(il:il) = ','
            if (i.eq.nv) str(il:il) = ')'
c
c Write into line if room
c
            if (ip+il.gt.maxlen) then
              call bug ('w', 'Too many vertices for line length')
              good = .false.
            else
              line(ip:) = str(1:il)
              ip = ip + il
            end if
            i = i + 1
          end do
c
c Add image plane
c
          if (size(3).gt.1) then
            call strfi (krng(1), '(i6)', str, il)
            if (krng(2).ne.1) then
              str(il+1:) = ','
              call strfi (krng(1)+krng(2)-1, '(i6)', str(il+2:), il)
              il = len1(str)
            end if
            if (ip+il.gt.maxlen) then
              call bug ('w', 
     +          'Not enough room in line to write image plane')
            else
              line(ip:) = '('//str(1:il)//')'
            end if
          end if
c
c Write into log file
c
          if (good) call txtwrite (lreg, line, len1(line), iostat)
        end if
c
c Have another go
c
        call output (' ')
        call output ('Draw another region with this display:   A')
        call output ('Draw another region after redisplaying:  R')
        call output ('Finish region option:                    X')
        call output (' ')
        call cgcur (vx, vy, ans)
        if (ans.eq.'A') then
          more = .true.
          redisp = .false.
        else if (ans.eq.'R') then
          more = .false.
          redisp = .true.
        else
          more = .false.
          redisp = .false.
        end if
      end do
      call finco (lin)
c
      end
c
c
      subroutine curpos (lin, nx, ny, image, images, nimage, labtyp, 
     +   blc, naxis, cdelt, crpix, crval, ctype, ibin, jbin, krng, 
     +   dolog, lcurs, cgspec, cgdisp, mark, dobox, near, trfun, groff)
c-----------------------------------------------------------------------
c     Return pixel location and value under cursor
c
c  Input:
c     lin     Image handle
c     nx,ny   Size of image
c     image   Image
c     images  A copy of image before the transfer function is applied
c             This will only contain a meaningful image for trfun='heq'
c             because we can't recover the true pixel value from the
c             histogram equalized image
c     blc     blc of window being displayed
c     labtyp  axis label types
c     naxis   Number of axes in image
c     cdelt   Pixel increments
c     crpix   Reference pixel
c     crval   Reference value
c     ctype   Axis types
c     i,jbin  Spatial pixel increment 
c     krng    Start channel and number of channels averaged in
c             current display
c     dolog   Write to log file as well
c     lcurs   Handle of log file
c     cgspec  OUtput log file appropriate to CGSPEC
c     cgdisp  OUtput log file appropriate to CGDISP
c     mark    Mark cursor locaitons
c     dobox   Peak in 5x5 box
c     near    FOrce cursor to nearest pixel
c     trfun   Transfer function type
c     groff   Offset for log offset
c     plst,av STart plane and  number of planes averaged
c
c-----------------------------------------------------------------------
      implicit none
c
      integer nx, ny, nimage(nx,ny), blc(2), lcurs, naxis, 
     +  ibin, jbin, krng(2), lin
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      real image(nx,ny), images(*), groff
      character*(*) labtyp(2), ctype(naxis), trfun
      logical dolog, cgspec, cgdisp, mark, dobox, near
cc
      double precision pix(3), pixbs(2), ww(2)
      real w(2), ival
      integer iostat, len1, iloc, ipl, wl(3), wwl(2), vl(3), k,
     +  bin(2), ib, jb
      character cch*1, line*132, plstr*20, vstr(3)*60, wstr(3)*60, 
     +  wwstr(3)*20, typei(3)*6, typeo(3)*6
      logical ok
c-----------------------------------------------------------------------
      call output (' ')  
      call output ('****************************')
      call output ('Entering cursor value option')
      call output ('****************************')
      call output (' ')
      call output ('Click left button  (enter A) for location')
      call output ('Click right button (enter X) to exit')
      call output (' ')
      call initco (lin)
      typei(1) = 'abspix'
      typei(2) = 'abspix'
      typei(3) = 'abspix'
      pix(3) = (real(2*krng(1)+krng(2))-1.0)/2.0
c
c Format channel value
c
c      if (krng(2).eq.1) then
c        call strfi (krng(1), '(i4)', wstr(3), wl(3))
c      else
c        call strfr (real(pix(3)), '(f7.2)', wstr(3), wl(3))
c      end if
c
c Format channel range for CGDISP log files
c
      call strfi (krng(1), '(i6)', plstr, ipl)
      if (krng(2).ne.1) then
        plstr(ipl+1:) = ' '
        call strfi (krng(1)+krng(2)-1, '(i6)', plstr(ipl+2:), ipl)
        ipl = len1(plstr)
      end if
c
      bin(1) = ibin
      bin(2) = jbin
      cch = ' '
      iloc = 0
      do while (cch.ne.'X')
c
c Read cursor in linear world coordinates and convert to (nearest
c or boxest) binned subimage pixels
c
        call cgcur (w(1), w(2), cch)
        ww(1) = w(1)
        ww(2) = w(2)
        if (near) then
          call nearcon (labtyp, naxis, crval, crpix, cdelt,
     +                  ctype, bin, blc, ww, pix, pixbs)
        else if (dobox) then
          call pkfind (labtyp, naxis, crval, crpix, cdelt, ctype, 
     +      nx, ny, image, nimage, blc, bin, ww, pix, pixbs)
        else
          do k = 1, 2
            call w2pixcg (ww(k), k, labtyp(k), naxis, crval, crpix,
     +                    cdelt, ctype, pix(k), ok)
            pixbs(k) = pix(k)
            call ppconcg (1, blc(k), bin(k), pixbs(k))
          end do
        end if
        w(1) = ww(1)
        w(2) = ww(2)
c
c Keep an integer copy of binned subimage pixel
c
        ib = nint(pixbs(1))
        jb = nint(pixbs(2))
c
        if (ib.lt.1 .or. ib.gt.nx .or. jb.lt.1 .or. jb.gt.ny) then
          call bug ('w', 'Cursor off image')
        else
c
c Mark on plot if desired
c
          if (cch.ne.'X') then
            iloc = iloc + 1
            if (mark) then
              call pgslw (2)
              call pgpt (1, w(1), w(2), 2)
              call pgupdt
              call pgslw (1)
            end if
            call output (' ')
c
            if (dolog) then
c
c Write separator to log file
c
              if (cgspec .or. cgdisp) then
                call txtwrite (lcurs, '#', 1, iostat)
              else
                call txtwrite (lcurs, ' ', 1, iostat)
              end if
            end if
c
c Convert absolute pixel to true world coordinate formatted strings
c with and without units
c
            call setoaco (lin, 'abs', 3, typeo)
            call w2wfco (lin, 3, typei, ' ', pix,  typeo, ' ',
     +                   .true., wstr, wl)
            call w2wfco (lin, 3, typei, ' ', pix, typeo, ' ',
     +                   .false., vstr, vl)
c       
            line = 'World coordinates x,y         : '//
     +              vstr(1)(1:vl(1))//', '//vstr(2)(1:vl(2))
            call output (line)    
c
c Write log files
c
            if (dolog) then
              if (.not.cgspec .and. .not.cgdisp)
     +          call txtwrite (lcurs, line, len1(line), iostat)
c
              if (cgspec) then
                write (line, 5) typeo(1)(1:len1(typeo(1))), 
     +                          typeo(2)(1:len1(typeo(2))),
     +                          wstr(1)(1:wl(1)), wstr(2)(1:wl(2))
5               format (a, 1x, a, 1x, a, 1x, a)
                call txtwrite (lcurs, line, len1(line), iostat)
              end if
c
              if (cgdisp) then
                call pixinc (lin, bin, wwstr, wwl)
c
                write (line, 8) typeo(1)(1:len1(typeo(1))),
     +            typeo(2)(1:len1(typeo(2))), iloc ,
     +            wstr(1)(1:wl(1)), wstr(2)(1:wl(2)),
     +            wwstr(1)(1:wwl(1)), wwstr(2)(1:wwl(2)),
     +            plstr(1:ipl)
8               format ('star ', a, 1x, a, i4, ' no ' a, 1x,
     +                  a, 1x, a, 1x, a, 1x, a)
                call txtwrite (lcurs, line, len1(line), iostat)
              end if
            end if 
c
c Convert absolute pixel to true offset world coordinate formatted strings 
c
            call setoaco (lin, 'off', 3, typeo)
            call w2wfco (lin, 3, typei, ' ', pix,  typeo, ' ',
     +                   .false., wstr, wl)
            line = 'Offset world coordinates x,y  : '//
     +              wstr(1)(1:wl(1))//', '//wstr(2)(1:wl(2))
            call output (line)    
            if (dolog .and. (.not.cgspec .and. .not.cgdisp))
     +          call txtwrite (lcurs, line, len1(line), iostat)
c
c Absolute pixels.  
c
            typeo(1) = 'abspix'
            typeo(2) = 'abspix'
            typeo(3) = 'abspix'
            call w2wfco (lin, 3, typei, ' ', pix, typeo, ' ',
     +                   .true., wstr, wl)
            write (line, 10) wstr(1)(1:wl(1)), wstr(2)(1:wl(2)), 
     +                       wstr(3)(1:wl(3))
10          format ('Image pixel coordinates x,y,z : ', a, ', ', a, 
     +              ', ', a)
            call output (line)
            if (dolog .and. (.not.cgspec .and. .not.cgdisp))
     +          call txtwrite (lcurs, line, len1(line), iostat)
c
c Image intensity; allow for transfer function
c  
            call strfi (nint(pix(1)), '(i4)', wstr(1), wl(1))
            call strfi (nint(pix(2)), '(i4)', wstr(2), wl(2))
            if (nimage(ib,jb).ne.0) then
              ival = image(ib,jb)
              if (trfun.eq.'sqr') then
                ival = ival**2 - groff
              else if (trfun.eq.'log') then
                ival = 10**ival - groff
              else if (trfun.eq.'heq') then
                call heqval (nx, ny, images, ib, jb, ival)
              end if
c
              write (line, 40) ival, wstr(1)(1:wl(1)), 
     +           wstr(2)(1:wl(2)), wstr(3)(1:wl(3))
40            format ('Image intensity               :', 1pe12.4,
     +                ' at pixel (', a, ', ', a, ', ', a, ')')
              call output (line)
              if (dolog .and. (.not.cgspec .and. .not.cgdisp))
     +          call txtwrite (lcurs, line, len1(line), iostat)
            else
              write (line, 50) wstr(1)(1:wl(1)), wstr(2)(1:wl(2)), 
     +                         wstr(3)(1:wl(3))
50            format ('Image intensity(', a, ',', a, ',', a,
     +                ')     : blanked')
              call output (line)
              if (dolog .and. (.not.cgspec .and. .not.cgdisp))
     +          call txtwrite (lcurs, line, len1(line), iostat)
            end if
          end if
        end if
      end do
c
      call finco (lin)
c
      end
c
c
      subroutine curstat (lin, blc, nx, ny, image, images, nimage, 
     +    labtyp, naxis, crval, cdelt, crpix, ctype, ibin, jbin,
     +    doreg, redisp, smore, dolog, mark, near, lstat, trfun, groff)
c-----------------------------------------------------------------------
c     Work out statistics from region marked with cursor.  If the
c     delineated region is invalid, you exit from here, the sub-plot
c     is redrawn, and you get another go.  It has to be redrawn
c     becuase simply erasing the polygon will also erase the underlying
c     image.  You get three goes at drawing a decent polygon
c     before it quits.
c
c  Input:
c    lin    Handle of image
c    blc    Blc of sub-image displayed
c    labtyp axis label types
c    naxis  Number of axes in image
c    cdelt  Pixel increments
c    crpix  Reference pixel
c    crval  Reference value
c    ctype  Axis types
c    nx,ny  Size of displayed sub-image
c    image  Sub-image
c    images Copy of images before any transfer function applied
c           Only meaningful image here if "heq" transfer function
c    nimage Normalization sub-image
c    i,jbin Spatial pixel increment
c    doreg  True if going on to cursor region option next
c    dolog  Write to log file as well
c    mark   Mark min and max
c    near   Force cursor locations to nearest pixel
c    lstat  Handle of output text file
c  Output:
c    redisp Redisplay the image
c    smore  Do more statistics options
c    trfun  Grey scale transfer function type
c    groff  Log offset
c-----------------------------------------------------------------------
      implicit none
c
      integer nx, ny, nimage(nx,ny), blc(2), lin, lstat, naxis,
     +  ibin, jbin
      double precision crval(naxis), cdelt(naxis), crpix(naxis)
      real image(nx,ny), images(*), groff
      logical redisp, doreg, smore, dolog, mark, near
      character*(*) trfun, ctype(naxis), labtyp(2)
cc
      integer symb, nvmax, maxruns
      parameter (symb = 17, nvmax = 100, maxruns = 50)
c
      integer vert(2,nvmax), runs(maxruns), nruns, nv, i, j, k, iostat,
     +  npix, iymin, iymax, kd, t, len1, ci, bin(2)
      double precision cdelt1, cdelt2, x, y, imin, jmin, 
     +  imax, jmax, ww(2), pix(2), pixbs(2)
      real vx(nvmax), vy(nvmax), sum, sumsq, mean, var, rms,
     +  dmin, dmax, bmin, bmaj, barea, ival
      character line*80, ans*1, bunit*8
      logical good, more, ok
c------------------------------------------------------------------------
      call output (' ')
      call output ('**************************')
      call output ('Entering statistics option')
      call output ('**************************')
      call output (' ')
      call output ('Click left button   (enter A) to mark vertex')
      call output 
     +  ('Click middle button (enter D) to delete previous vertex')
      call output ('Click right button  (enter X) to finish polygon')
      call output (' ')
c
c Get beam if present
c
      call rdhdr (lin, 'bmaj', bmaj, 0.0)
      call rdhdr (lin, 'bmin', bmin, 0.0)
      call rdhdd (lin, 'cdelt1', cdelt1, 0.0d0)
      call rdhdd (lin, 'cdelt2', cdelt2, 0.0d0)
      call rdhda (lin, 'bunit', bunit, ' ')
      barea = 1.1331 * bmaj * bmin / abs(cdelt1 * cdelt2)
      bin(1) = ibin
      bin(2) = jbin
c
c Open log file as required
c
      more = .true.
      do while (more)
c
c Get vertices with cursor
c
        nv = 0
        call pgupdt
        call pgolin (nvmax-1, nv, vx, vy, symb)
        call pgupdt 
c
c Go on with enough vertices
c
        if (nv.gt.1) then
c
c Convert to nearest pixel if desired
c
          if (near) then
c
c Rub out old points
c
            call pgqci (ci)
            call pgsci (0)
            call pgpt (nv, vx, vy, symb)
c
c Convert linear world coordinate to linear world coordinate of nearest pixel 
c
            do i = 1, nv
              ww(1) = vx(i)
              ww(2) = vy(i)
              call nearcon (labtyp, naxis, crval, crpix, cdelt,
     +                      ctype, bin, blc, ww, pix, pixbs)
              vx(i) = ww(1)
              vy(i) = ww(2)
            end do
c
c Draw new points
c
            call pgsci (ci)
            call pgpt (nv, vx, vy, symb)
          end if
c
c Join up the vertices of the polygon
c
          call pgsfs (2)
          call pgslw (2)
          call pgpoly (nv, vx, vy)
          call pgupdt
          call pgslw (1)
c
c Loop over vertices
c
          i = 1
          iymin = 1000000
          iymax = 0
          good = .true.
c
          do while (i.le.nv .and. good)
c
c Convert linear world coordinates to integer binned pixels in sub-image
c
            call w2pixcg (dble(vx(i)), 1, labtyp(1), naxis, crval, 
     +                    crpix, cdelt, ctype, pix, ok)
            call ppconcg (1, blc(1), ibin, pix)
            vert(1,i) = nint(pix(1))
c
            call w2pixcg (dble(vy(i)), 2, labtyp(2), naxis, crval, 
     +                    crpix, cdelt, ctype, pix, ok)
            call ppconcg (1, blc(2), jbin, pix)
            vert(2,i) = nint(pix(1))
c
c Update y pixel extrema
c
            iymin = min(iymin,vert(2,i))
            iymax = max(iymax,vert(2,i))
c
            if (vert(1,i).lt.1 .or. vert(1,i).gt.nx .or.
     +          vert(2,i).lt.1 .or. vert(2,i).gt.ny) then
              call bug ('w', 'Polygon off image, try again')
              good = .false.
           else
              good = .true.
           end if
c
            i = i + 1
          end do
c
c Eliminate redundant vertices
c 
          if (good) then
            call elimrvi (nvmax, nv, vert)
            nv = nv + 1
            vert(1,nv) = vert(1,1)
            vert(2,nv) = vert(2,1)
c
c  Check if polygon in clockwise order.
c
            t = 0
            do k = 1, nv-1
              t = t + vert(1,k)*vert(2,k+1) - vert(2,k)*vert(1,k+1)
            end do
c
c  If it's clockwise, convert it to anti-clockwise.
c
            if (t.lt.0) then
              do k = 2, nv/2
                kd = nv - k + 1
                t = vert(1,k)
                vert(1,k) = vert(1,kd)
                vert(1,kd) = t
                t = vert(2,k)
                vert(2,k) = vert(2,kd)
                vert(2,kd) = t
              end do
            end if
c
c Find runs array and accumulate statistics for unblanked
c pixels in each row
c
            sum = 0.0
            sumsq = 0.0
            dmin = 1.0e30
            dmax = -1.0e30
            npix = 0
c
            do j = iymin, iymax
              call polyruns (runs, maxruns, j, nv, vert, nruns)
c
              if (nruns.gt.0) then
                do k = 1, nruns, 2
                  do i = runs(k), runs(k+1)
                    if (nimage(i,j).ne.0) then
c
c Pixel unblanked, find value
c
                      ival = image(i,j)
                      if (trfun.eq.'sqr') then
                        ival = ival**2 - groff
                      else if (trfun.eq.'log') then
                        ival = 10**ival - groff
                      else if (trfun.eq.'heq') then
                        call heqval (nx, ny, images, i, j, ival)
                      end if
c
c Accumulate
c
                      sum = sum + ival
                      sumsq = sumsq + ival**2
                      npix = npix + 1
c
c Note min and max
c
                      if (ival.lt.dmin) then
                        dmin = ival
                        imin = i
                        jmin = j
                      end if
c
                      if (ival.gt.dmax) then
                        dmax = ival
                        imax = i
                        jmax = j
                      end if
                    end if
                  end do
                end do
              end if
            end do
c
c Work out results
c
            if (npix.gt.0) then
              mean = sum / real(npix)
              var = (sumsq/real(npix)) - mean*mean
              if (var.gt.0) then
                rms = sqrt(var)
              else
                rms = 0.0
              end if
c
c Tell user
c
              call output (' ')
              if (dolog) call txtwrite (lstat, ' ', 1, iostat)
              if (barea.gt.0.0 .and. bunit.eq.'JY/BEAM') then
                write (line, 10) sum, sum/barea
10              format ('Sum = ', 1pe12.5, '   Flux density = ',
     +                  1pe12.5, ' Jy')
              else if (bunit.eq.'JY/PIXEL') then
                write (line, 12) sum
12              format ('Flux density  = ', 1pe12.5, ' Jy')
              else
                write (line, 15) sum
15              format ('Sum = ', 1pe12.5)
              end if
              call output (line)
              if (dolog) call txtwrite (lstat, line, len1(line), iostat)
c
              write (line, 17) mean, rms, npix
17            format ('Mean = ', 1pe12.5, '  sigma = ', 1pe12.5,
     +                ' from ', i8, ' valid pixels')
              call output (line)
              if (dolog) call txtwrite (lstat, line, len1(line), iostat)
c
c Give data min and max value locations in unbinned pixels. Centre
c of a binned pixel may be a fractional unbinned pixel, so use
c real unbinned pixels.
c
              call ppconcg (2, blc(1), ibin, imin)
              call ppconcg (2, blc(2), jbin, jmin)
c
              write (line, 20) dmin, imin, jmin
20            format ('Data minimum ', 1pe12.5, ' at pixel i,j = ',
     +                0pf7.2, ',', f7.2)
              call output (line)
              if (dolog) call txtwrite (lstat, line, len1(line), iostat)
c
              call ppconcg (2, blc(1), ibin, imax)
              call ppconcg (2, blc(2), jbin, jmax)
c
              write (line, 30) dmax, imax, jmax
30            format ('Data maximum ', 1pe12.5, ' at pixel i,j = ',
     +                0pf7.2, ',', f7.2)
              call output (line)
              if (dolog) call txtwrite (lstat, line, len1(line), iostat)
c
c Mark location of min and max on plot if desired
c
              if (mark) then
                call pix2wcg (.false., imin, 1, labtyp(1), naxis,
     +                        crval, crpix, cdelt, ctype, x, ok)
                call pix2wcg (.false., jmin, 2, labtyp(2), naxis,
     +                        crval, crpix, cdelt, ctype, y, ok)
                call pgpt (1, real(x), real(y), 2)
c
                call pix2wcg (.false., imax, 1, labtyp(1), naxis,
     +                        crval, crpix, cdelt, ctype, x, ok)
                call pix2wcg (.false., jmax, 2, labtyp(2), naxis,
     +                        crval, crpix, cdelt, ctype, y, ok)
                call pgpt (1, real(x), real(y), 2)
                call pgupdt
              end if
            else
              call bug ('w', 
     +          'There were no valid pixels inside the polygon')
            end if
          end if
        else
          call bug ('w', 
     +              'A polygon with only one vertex is not very useful')
        end if
c
c Have another go
c
        redisp = .false.
        smore = .false.
c
        call output ('  ')
        call output ('Draw another region with this display:   A')
        call output ('Draw another region after redisplaying:  R')
        call output ('Finish statistics option:                X')
        if (doreg) 
     +  call output ('Finish statistics option and redisplay:  C')
c
        call cgcur (vx, vy, ans)
        if (ans.eq.'A') then
          more = .true.
        else if (ans.eq.'R') then
          more = .false.
          redisp = .true.
          smore = .true.
          call output ('Redisplaying')
        else if (ans.eq.'X') then
          more = .false.
        else if (ans.eq.'C') then
          more = .false.
          redisp = .true.
        end if
      end do
c
      end
c
c
      subroutine decopt  (do3val, do3pix, eqscale, cursor, stats, doreg,
     +                    doabs, dolog, cgspec, cgdisp, mark, doerase, 
     +                    dobox, near, dowedge, dofid)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     do3val    True means label sub-plots with value of third axis
c     do3pix    True means label sub-plots with pixel of third axis
c     eqscale   True means plot with x and y scales
c     cursor    True means enter cursor mode at end of plot
c     stats     True means enter cursor staistics mode at the end
c                of each subplot.
c     doreg     True for cursor driven region of interest sepecifcation
c     doabs     True if region of interest in absoluite pixels 
c               insetad of offset arcseconds from reference pixel
c     dolog     Write results to log file as well
c     cgspec    Write log file in format more useful to CGSPEC
c     cgdisp    Write log file in format more useful to CGDISP
c     mark      Mark cursor location
c     doerase   Erase rectangle behind "3-axis" strings
c     dobox     List peak in 5x5 box under cursor
c     near      Force cursor to neasrest pixel in options=cursor
c     dowedge   Draw wedge on grey scale
c     dofid     FIddle lookup table of grey scale
c-----------------------------------------------------------------------
      implicit none
c
      logical do3val, do3pix, eqscale, cursor, stats, doreg, near,
     +  doabs, dolog, cgspec, cgdisp, mark, doerase, dobox, dofid,
     +  dowedge
cc
      integer maxopt
      parameter (maxopt = 16)
c
      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'3value  ', '3pixel  ', 'unequal ', 'stats   ',
     +              'cursor  ', 'region  ', 'abspixel', 'logfile ',
     +              'cgspec  ', 'cgdisp  ', 'mark    ', 'noerase ',
     +              'box     ', 'nearest ', 'wedge   ', 'fiddle  '/
c-----------------------------------------------------------------------
      call optcg ('options', opshuns, present, maxopt)
c
      do3val   =      present(1)
      do3pix   =      present(2)
      eqscale  = .not.present(3)
      stats    =      present(4)
      cursor   =      present(5)
      doreg    =      present(6)
      doabs    =      present(7)
      dolog    =      present(8)
      cgspec   =      present(9)
      cgdisp   =      present(10)
      mark     =      present(11)
      doerase  = .not.present(12)
      dobox    =      present(13)
      near     =      present(14)
      dowedge  =      present(15)
      dofid    =      present(16)
c
      end
        
        
      subroutine elimrvd (nmax, n, v)
c-----------------------------------------------------------------------
c     The list of vertices may have some redundant points in it.
c     Get rid of these.
c
c     This is the real version for CUREG
c
c   Input:
c     nmax    Maximum allowed number of vertices
c   Input/output
c     n       Number of vertices
c     v       Vertices
c
c-----------------------------------------------------------------------
      implicit none
c
      integer nmax, n
      double precision v(2,nmax)
cc
      integer kd, k
c-----------------------------------------------------------------------
      v(1,n+1) = v(1,1)
      v(2,n+1) = v(2,1)
      kd = 1
      do k = 2, n
       if ((v(2,k+1)-v(2,k))*(v(1,k)-v(1,kd)).ne.
     +    (v(2,k)-v(2,kd))*(v(1,k+1)-v(1,k))) then
         kd = kd + 1
         v(1,kd) = v(1,k)
         v(2,kd) = v(2,k)
       end if
      end do
      n = kd
c
      if (n.lt.3) then
        call bug ('w', 'Degenerate polygon in ElimRVr')
      else
c
c  Check if the first pixel is colinear. This cannot deal with this, and
c  craps out.
c
        if((v(2,2)-v(2,1))*(v(1,1)-v(1,n)).eq.
     +     (v(2,1)-v(2,n))*(v(1,2)-v(1,1)))then
          v(1,1) = v(1,n)
          v(2,1) = v(2,n)
          n = n - 1
        end if
c
        if(n.lt.3)call bug ('w', 'Degenerate polygon in ElimRVd')
      end if
c
      end
c
c
      subroutine elimrvi (nmax, n, v)
c-----------------------------------------------------------------------
c     The list of vertices may have some redundant points in it.
c     Get rid of these.
c
c     This is the integer version for CURSTAT
c
c   Input:
c     nmax    Maximum allowed number of vertices
c   Input/output
c     n       Number of vertices
c     v       Vertices
c
c-----------------------------------------------------------------------
      implicit none
c
      integer nmax, n
      integer v(2,nmax)
cc
      integer kd, k
c-----------------------------------------------------------------------
      v(1,n+1) = v(1,1)
      v(2,n+1) = v(2,1)
      kd = 1
      do k = 2, n
       if ((v(2,k+1)-v(2,k))*(v(1,k)-v(1,kd)).ne.
     +    (v(2,k)-v(2,kd))*(v(1,k+1)-v(1,k))) then
         kd = kd + 1
         v(1,kd) = v(1,k)
         v(2,kd) = v(2,k)
       end if
      end do
      n = kd
      if (n.lt.3) then
        call bug ('w', 'Degenerate polygon in ElimRVi')
      else
c
c  Check if the first pixel is colinear. This cannot deal with this, and
c  craps out.
c
        if((v(2,2)-v(2,1))*(v(1,1)-v(1,n)).eq.
     +     (v(2,1)-v(2,n))*(v(1,2)-v(1,1)))then
          v(1,1) = v(1,n)
          v(2,1) = v(2,n)
          n = n - 1
        end if
c
        if(n.lt.3)call bug ('w', 'Degenerate polygon in ElimRVi')
      end if
c
      end
c
c
      subroutine heqval (nx, ny, image, i, j, val)
      implicit none
      integer nx, ny, i, j
      real image(nx,ny), val
      val = image(i,j)
c
      end
c
c
      subroutine inputs (maxlev, in, ibin, jbin, kbin, levtyp, slev,
     +   levs, nlevs, pixr, trfun, pdev, labtyp, do3val, do3pix, 
     +   eqscale, nx, ny, cs, dogrey, cursor, stats, doreg, doabs, 
     +   dolog, cgspec, cgdisp, mark, doerase, dobox, near, 
     +   dowedge, dofid)
c-----------------------------------------------------------------------
c     Get the unfortunate user's long list of inputs
c
c  Input:
c   maxlev     Maximum number of allowed contour levels
c  Output:
c   in         Image name.
c   i,j,kbin   X, y and z pixel increment and average
c   levtyp     Type of contour levels scale factor
c              'p'(ercentage) or 'a'(bsolute)
c   slev       Contour levels scale factors (absolute or percentage)
c   levs       Contour levels.  Will be scaled by SLEV for contouring
c   nlevs      Number of contour levels
c   pixr       Greyscale intensity range
c   trfun      Type of grey scale transfer function: 'log', 'lin', 
c	       'heq', or 'sqr'
c   pdev       PGPLOT plot device/type
c   labtyp     Type of labels for x and y axes
c   do3val     True means label sub-plots with value of third axis
c   do3pix     True means label sub-plots with pixel of third axis
c   eqscale    True means plot with x and y scales
c   nx,ny      Number of sub-plots per page
c   cs         PGPLOT character sizes for the plot axis labels and
c              velocity/channel label,
c   dogrey     True for grey scale, false for contour plot
c   cursor     True to enter cursor mode at end of each sub-plot.
c   stats      True to enter cursor statistics mode at end of
c              each sub-plot
c   doreg      True if define region of interest with cursor.
c   doabs      Region of interest in absolutre pixels instead of
c              offset arcseconds from reference pixel
c   dolog      Write results to log files
c   cgspec     Write log file in a format appropriate to CGSPEC
c   cgdisp     Write log file in a format appropriate to CGDISP
c   mark       Mark cursor locations
c   doerase    Erase rectangle behind "3-axis" value
c   dobox      List peak in 5x5 box under cursor
c   near       FOrce cursor to nearest pixel in options=cursor
c   dofid      FIddle lookup tbale of grey scale
c   dowedge    Draw wedge with grey scale
c-----------------------------------------------------------------------
      implicit none
c
      integer maxlev, nx, ny, nlevs, ibin(2), jbin(2), kbin(2)
      real levs(maxlev), pixr(2), cs(2), slev
      character*(*) labtyp(2), in, pdev, trfun, levtyp
      logical do3val, do3pix, eqscale, cursor, stats, doreg, dogrey,
     +  doabs, dolog, cgspec, mark, cgdisp, doerase, dobox, near,
     +  dowedge, dofid
cc
      integer ntype, nlab, ntype2, nimtype
      parameter (ntype = 13, ntype2 = 2)
      character type(ntype)*6, imtype*7, type2(ntype2)*7
      data type  /'hms   ', 'dms   ', 'abspix', 'relpix', 
     +            'arcsec', 'absghz', 'relghz', 'abskms', 
     +            'relkms', 'abslin', 'rellin', 'absdeg',
     +            'reldeg'/
      data type2 /'contour', 'grey'/
c-----------------------------------------------------------------------
      call keyini
      call keyf ('in', in, ' ')
      if (in.eq.' ') call bug ('f', 'No image specified')
      call keymatch ('type', ntype2, type2, 1, imtype, nimtype)
      if (nimtype.eq.0) imtype = 'contour'
      dogrey = .true.
      if (imtype.eq.'contour') dogrey = .false.
c
      call keyi ('xybin', ibin(1), 1)
      call keyi ('xybin', ibin(2), ibin(1))
      if (ibin(2).ne.1 .and. ibin(2).ne.ibin(1)) call bug ('f',
     +  'Non-unit x spatial averaging must be equal to increment')
      ibin(1) = max(ibin(1), 1)
      ibin(2) = max(ibin(2), 1)
c
      call keyi ('xybin', jbin(1), ibin(1))
      call keyi ('xybin', jbin(2), jbin(1))
      if (jbin(2).ne.1 .and. jbin(2).ne.jbin(1)) call bug ('f',
     +  'Non-unit y spatial averaging must be equal to increment')
      jbin(1) = max(jbin(1), 1)
      jbin(2) = max(jbin(2), 1)
c
      call keyi ('chan', kbin(1), 1) 
      call keyi ('chan', kbin(2), 1) 
      kbin(1) = max(kbin(1), 1)
      kbin(2) = max(kbin(2), 1)
      if (kbin(2).gt.kbin(1)) kbin(2) = kbin(1)
c
      call keya ('slev', levtyp, 'a')
      call keyr ('slev', slev, 0.0)
      call lcase (levtyp)
      if (levtyp.ne.'p' .and. levtyp.ne.'a') call bug ('f',
     +   'Unrecognized contour level scale type; must be "p" or "a"')
c
      call mkeyr ('levs', levs, maxlev, nlevs)
c
      call keyr ('range', pixr(1), 0.0)
      call keyr ('range', pixr(2), 0.0)
      call keya ('range', trfun, 'lin')
      call lcase (trfun)
      if (dogrey) then
        if (trfun.ne.'lin' .and. trfun.ne.'log' .and. trfun.ne.'heq' 
     +      .and. trfun.ne.'sqr') call bug ('f', 
     +    'Unrecognized grey scale transfer function type')
      else
        trfun = ' '
      end if
c
      call keya ('device', pdev, ' ')
      if (pdev.eq.' ') then
        call pgldev
        call bug ('f', 'A PGPLOT device must be given')
      end if
c
      call decopt (do3val, do3pix, eqscale, cursor, stats, doreg, doabs,
     +             dolog, cgspec, cgdisp, mark, doerase, dobox, near, 
     +             dowedge, dofid)
      if (.not.cursor .or. .not.dolog) cgspec = .false.
      if (.not.cursor) dobox = .false.
      if (near .and. dobox) call bug ('f', 
     +  'You can''t have options=near and options=box')
      if (.not.dogrey) then
        dofid = .false.
        dowedge = .false.
      end if
      if (cgdisp .or. cgspec) dolog = .true.
c
      call keymatch ('labtyp', ntype, type, 2, labtyp, nlab)
      if (nlab.eq.0) labtyp(1) = 'abspix'
      if (nlab.le.1) then
        labtyp(2) = labtyp(1)
        if (labtyp(1).eq.'hms') labtyp(2) = 'dms'
      end if
c
      if ( (index(labtyp(1),'lin').ne.0  .and. 
     +      index(labtyp(2),'lin').eq.0) .or.
     +     (index(labtyp(2),'lin').ne.0  .and. 
     +      index(labtyp(1),'lin').eq.0) ) then
        if (eqscale) call bug ('i', 
     +  'You might consider options=unequal with these axis types')
      end if
c
      call keyi ('nxy', nx, 0)
      call keyi ('nxy', ny, nx)
c
      call keyr ('csize', cs(1), 0.0)
      call keyr ('csize', cs(2), 0.0)
c
      end
c
c
      subroutine nearcon (labtyp, naxis, crval, crpix, cdelt, 
     +                    ctype, bin, blc, w, pix, pixbs)
c-----------------------------------------------------------------------
c     Take a world coordinate and find its corresponding binned
c     pixel. Then take the nearest binned pixel, then find
c     the world coordinate of that location and the unbinned
c     full image pixel of that lcoation
c
c  Input
c   near     If true, take nearest pixel
c   bin      Pixel binning factors in x and y
c   blc      BLC of image being displayed
c  Input/output
c   w        World coordinate
c  Output
c   pix      Unbinned full image pixel
c   pixbs    Binned subimage pixel
c
c-----------------------------------------------------------------------
      implicit none
c
      integer naxis, bin(2), blc(2)
      double precision crval(naxis), crpix(naxis), cdelt(naxis), w(2),
     +  pix(2), pixbs(2)
      character*(*) ctype(naxis), labtyp(2)
cc
      integer k
      logical ok
c-----------------------------------------------------------------------
c
c Loop over axes
c
      do k = 1, 2
c
c Convert world coordinate to unbinned full image pixels
c
        call w2pixcg (w(k), k, labtyp(k), naxis, crval, crpix,
     +                cdelt, ctype, pix(k), ok)
c
c Convert to subimage binned pixels. 
c
        call ppconcg (1, blc(k), bin(k), pix(k))
c
c Take nearest subimage pixel and keep copy
c
        pix(k) = dble(nint(pix(k)))
        pixbs(k) = pix(k)
c
c Convert back to full image unbinned pixels
c
        call ppconcg (2, blc(k), bin(k), pix(k))
c
c Convert back to to world coordinate
c
        call pix2wcg (.false., pix(k), k, labtyp(k), naxis,
     +                crval, crpix, cdelt, ctype, w(k), ok)
      end do
c
      end
c
c
      subroutine pixinc (lin, bin, wwstr, wwl)
c-----------------------------------------------------------------------
c     Find pixel increments for each axis in appropriate units
c     Work them out at the reference pixel of the image (all 
c     axes).  Could in principle work them out at the exact
c     coordinate (all axes) that user has generated, but it doesn't
c     really matter for this applciation (writing the width of
c     an overlay) as its arbitrary anyway
c     
c
c  Input
c   lin     Image handle
c   bin     Spatial binning
c  Output
c   wwstr   Array of formatted pixel increments
c   wwl     Length of strings
c-----------------------------------------------------------------------
      implicit none
c       
      integer lin, bin(2), wwl(2)
      character*(*) wwstr(2)
cc
      double precision w1(2), w2(2), pix1(2), pix2(2), winc(2)
      character*6 typei(2), typeo(2)
      integer i
c-----------------------------------------------------------------------
c
c Work out default offset units for axis
c
      call setoaco (lin, 'off', 2, typeo)
c
c Find increments
c
      do i = 1, 2
       pix1(i) = 0.0d0
       pix2(i) = 1.0d0
       typei(i) = 'relpix'
      end do
c      
      call w2wco (lin, 2, typei, ' ', pix1, typeo, ' ', w1)
      call w2wco (lin, 2, typei, ' ', pix2, typeo, ' ', w2)
      winc(1) = w2(1) - w1(1)
      winc(2) = w2(2) - w1(2)
c
c Format
c
      do i = 1, 2
        call strfd (2*bin(i)*abs(winc(i)), '(1pe13.6)', 
     +              wwstr(i), wwl(i))
      end do
c
      end
c       
c       
      subroutine pkfind (labtyp, naxis, crval, crpix, cdelt, ctype,
     +  nx, ny, image, nimage, blc, bin, w, pix, pixbs)
c-----------------------------------------------------------------------
c     Find peak pixel in a box centred on input pixel location
c
c  Input
c    labtyp   Axis label types
c    naxis    Number of axes
c    c*       Axis descriptors
c    nx,ny    SIze of binned subimage
c    image    Binned subimage
c    nimage   Binned sub-mask-image
c    blc      BLC of full unbinned image
c    bin      Pixel binning
c  Input/output
c    w        World coordinate
c    pix      Full image unbinned pixel
c    pixbs    Binned subimage pixel
c
c-----------------------------------------------------------------------
      implicit none
c
      integer naxis, nx, ny, nimage(nx,ny), bin(2), blc(2)
      real image(nx,ny)
      double precision crval(naxis), crpix(naxis), cdelt(naxis),
     +  pix(2), pixbs(2), w(2)
      character*(*) ctype(naxis), labtyp(2)
cc
      real dmax
      integer k, is, ie, js, je, im, jm, ii, jj
      logical ok
c-----------------------------------------------------------------------
c
c Convert world coordinate to binned subimage pixel
c
      do k = 1, 2
c
c Convert world coordinate to binned subimage pixels
c
        call w2pixcg (w(k), k, labtyp(k), naxis, crval, crpix,
     +                cdelt, ctype, pix(k), ok)
        pixbs(k) = pix(k)
        call ppconcg (1, blc(k), bin(k), pixbs(k))
      end do
c
c Find pixel limits for search
c
      is = max(1,nint(pixbs(1))-2)
      ie = min(nx,nint(pixbs(1))+2)
      js = max(1,nint(pixbs(2))-2)
      je = min(ny,nint(pixbs(2))+2)
c
c Find peak
c
      dmax = -1.0e30
      im = -1
      jm = -1
      do jj = js, je
        do ii = is, ie
          if (nimage(ii,jj).ne.0 .and. image(ii,jj).gt.dmax) then
            im = ii
            jm = jj
            dmax = image(ii,jj)
          end if
        end do
      end do 
c
c If there is something unblanked return it, else stick with where
c we started
c
      if (im.ne.-1 .and. jm.ne.-1) then
        pixbs(1) = im
        pixbs(2) = jm
c
c Convert back to full image unbinned pixels and world coordinate
c
        do k = 1, 2
          pix(k) = pixbs(k)
          call ppconcg (2, blc(k), bin(k), pix(k))
c
c Convert back to to world coordinate
c
          call pix2wcg (.false., pix(k), k, labtyp(k), naxis,
     +                  crval, crpix, cdelt, ctype, w(k), ok)
        end do
      end if
c
      end
c
c

	subroutine polyruns(goes,maxgoes,j0,nverts,verts,ngoes)
c
	implicit none
	integer maxgoes,goes(maxgoes),j0,nverts,verts(2,nverts),ngoes
c
c  Calculate the runs which lie within a polygon. It does this by
c  calculating the intersections of a horizontal line with the polygon,
c  then sorting the intersections. There should be an even number of
c  intersections.
c  An added complication is the intersection of the horizontal line with
c  a vertex. Three situations are possibilities: go through a vertex
c  into the interior of the poly, clip a vertex, or
c  go along the edge of the poly. The first case counts as
c  one intersection, the second as two, and the third counts as 0 or 1
c  depending whether we are entering or leaving the selected area.
c
c    /		---^---		   _________
c   /_____	  / \		  /
c   \		 /   \		 /
c    \		/     \		/
c
c  Input:
c    nverts	Number of veritces of the polygon.
c    verts	The vertices of the polygon. The vertices are assumes to have
c		no redundancies (i.e. all vertices are distinct), and to
c		trace out a anti-clockwise path. 
c    j0		The value of y for which we want to determine the runs inside
c		the polygon.
c    maxgoes	Max number of runs is maxgoes/2.
c  Output:
c    goes	The runs for this value of y.
c    ngoes	The number of runs is ngoes/2.
c
c
c  This is the same as BOXPOLYX in BOXES.FOR.  Itr is an internal
c  subroutine in BOXES so I am not allowed to call it.
c  Hence the RJS style.
c------------------------------------------------------------------------
	integer k,kprev,l,t
	logical more
c
	ngoes = 0
	kprev = nverts-1
	do k=1,nverts-1,1
c
c  Case of an intersection with a vertex.
c
	  if(verts(2,k).eq.j0)then
	    t = (j0-verts(2,kprev))*(j0-verts(2,k+1))
	    if(t.gt.0)then
	      ngoes = ngoes + 2
	      goes(ngoes-1) = verts(1,k)
	      goes(ngoes)   = verts(1,k)
	    else if(t.lt.0)then
	      ngoes = ngoes + 1
	      goes(ngoes) = verts(1,k)
	    else
	      t =   verts(1,kprev)*( verts(2,k)    -verts(2,k+1)   )
     *		  + verts(1,k)    *( verts(2,k+1)  -verts(2,kprev) )
     *		  + verts(1,k+1)  *( verts(2,kprev)-verts(2,k)     )
	      if(t.gt.0)then
	        ngoes = ngoes + 1
		goes(ngoes) = verts(1,k)
	      endif
	    endif
c
c  Case of an intersection with the line segment between vertices.
c
	  else if((j0-verts(2,k))*(verts(2,k+1)-j0).gt.0)then
	    ngoes = ngoes + 1
	    goes(ngoes) =  nint( verts(1,k+1) +
     *		real( (j0-verts(2,k+1)) * (verts(1,k)-verts(1,k+1)))
     *			/ (verts(2,k)-verts(2,k+1)) )
	  endif
	  kprev = k
	enddo
c
	if(2*(ngoes/2).ne.ngoes)
     *	  call bug('f','Algorithmic failure in BoxRuns(polyx)')
c
c  The list of intersections are not in order. The number of intersections
c  is also likely to be small (probably only two!). Sort the intersections,
c  but use an insert-sort, because its probably ordered, and small.
c
	do k=2,ngoes
	  l = k
	  t = goes(l)
	  more = goes(l-1).gt.t
	  dowhile(more)
	    goes(l) = goes(l-1)
	    l = l - 1
	    more = .false.
	    if(l.gt.1)more = goes(l-1).gt.t
	  enddo
	  goes(l) = t
	enddo
c
c  There are possibly redundancies in the list of runs. Eliminate these.
c
	l = 3
	do k=3,ngoes,2
	  if(goes(k)-goes(l-1).le.1)then
	    goes(l-1) = goes(k+1)
	  else
	    goes(l) = goes(k)
	    goes(l+1) = goes(k+1)
	    l = l + 2
	  endif
	enddo
	ngoes = l-1
c
	end
c
c
      subroutine region (in, naxis, size, ibin, jbin, kbin, blc, trc,
     +                   win, maxgrp, grpbeg, ngrp, ngrps)
c----------------------------------------------------------------------
c     Finish key routine inputs for region of interest now.
c
c  Input:
c    in            Image file name
c    naxis         Number of dimensions of image
c    size          Dimensions of image
c    i,j,kbin      Pixel increment and binning in x,yz directions
c    maxgrp        Maximum nuber of groups of channels
c  Output:
c    grgbeg        List of start planes for each group of channels
c                  that are to be avearged together for each sub-plot
c                  A new group is begun at every interruption to the
c                  continuity of the selected channels, or if the
c                  channel increment is reached.
c    ngrp          Number of channels in each group of channel to
c                  be averaged together for each sub-plot.
c    ngrps         Number of groups of channels.
c    blc,trc       3-D Hyper-rectangle surrounding region of interest
c    win           Size of BINNED region of interest for 
c                  first 2 dimensions
c             
c
c----------------------------------------------------------------------
      implicit none
c
      integer naxis, size(naxis), blc(*), trc(*), win(2), maxgrp,
     +  ngrp(maxgrp), grpbeg(maxgrp), ngrps, ibin(2), jbin(2), kbin(2)
      character in*(*)
cc
      include 'maxdim.h'
      integer maxbox, i
      parameter (maxbox = 1024)
c
      integer boxes(maxbox)
c----------------------------------------------------------------------
      call boxinput ('region', in, boxes, maxbox)
      call boxset (boxes, naxis, size, 's')
      call keyfin
c
c Find hyper-rectangle surrounding region of interest
c
      call boxinfo (boxes, 3, blc, trc)
      do i = 1, naxis
        blc(i) = max(1,blc(i))
        trc(i) = min(size(i),trc(i))
      end do
c
c Adjust spatial window to fit an integral number of bins and
c find size of binned window
c
      call winfidcg (size(1), 1, ibin, blc(1), trc(1), win(1))
      call winfidcg (size(2), 2, jbin, blc(2), trc(2), win(2))
      if (win(1).le.1 .or. win(2).le.1) call bug ('f',
     +   'Cannot display just one spatial pixel')
c
c Find list of start channels and number of channels for each group
c of channels selected.
c
      call chnselcg (blc, trc, kbin, maxbox, boxes, maxgrp,
     +               grpbeg, ngrp, ngrps)
c
      end
