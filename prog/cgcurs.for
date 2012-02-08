      program cgcurs
c-----------------------------------------------------------------------
c= CGCURS - Read quantities with cursor from images on a PGPLOT device
c& nebk
c: plotting
c+
c       CGCURS displays an image via a contour plots or a pixel map
c       representation (formerly called a "grey scale") on a PGPLOT
c       device. The cursor is then used to read image values, or to
c       evaluate image statistics in a polygonal region, or to write
c       a polygonal region definition to a text file.
c
c       Manipulation of the device colour lookup table is available
c       when you display with a pixel map representation.
c
c       When using cursor options, generally, click the right button
c       (enter X) to exit the function, click the left button (enter A)
c       to add a location, and click the middle button (enter D) to
c       delete a location.
c
c@ in
c       One or two input images may be specified.  If two, and
c       TYPE=both, then the first is used as the background image, and
c       the second for a contour overlay.  They must match in size.
c@ type
c       Specifies the type of the image(s) in the IN keyword.  Minimum
c       match is supported:
c
c         "pixel": pixel map (formerly "grey" which is still supported),
c       "contour": contour plot,
c          "both": first input image is pixel map, the second is contour
c                  overlay.  If only one input image was specified it is
c                  used for both.
c
c       Default is "pixel" if one input image is specified, else "both".
c@ region
c       Region of interest.  Choose only one spatial region (bounding
c       box only supported), but as many spectral regions (i.e.,
c       multiple IMAGE specifications) as you like.  If you display a
c       3-D image, the cursor options are activated after each sub-plot
c       (channel or group of channels; see CHAN below) is drawn.
c       Default is full image
c@ xybin
c       Up to 4 values.  These give the spatial increment and binning
c       size in pixels for the x and y axes to be applied to the
c       selected region.  If the binning size is not unity, it must
c       equal the increment.  For example, to bin up the image by 4
c       pixels in the x direction and to pick out every third pixel in
c       the y direction, set XYBIN=4,4,3,1.
c       Defaults are 1,XYBIN(1),XYBIN(1),XYBIN(3)
c@ chan
c       2 values. The first is the channel increment, the second is
c       the number of channels to average, for each sub-plot.  Thus
c       CHAN=5,3  would average groups of 3 channels together, starting
c       5 channels apart such as: 1:3, 6:8, 11:13 ...   The channels
c       available are those designated by the REGION keyword.  A new
c       group of channels (sub-plot) is started if there is a
c       discontinuity in the REGION selected channels (such as
c       IMAGE(10,20),IMAGE(22,30).
c
c       Defaults are 1,1
c@ slev
c       2 values.   First value is the type of contour level scale
c       factor.  "p" for percentage and "a" for absolute.   Second
c       value is the level to scale LEVS by.  Thus  SLEV=p,1  would
c       contour levels at LEVS * 1% of the image peak intensity.
c       Similarly, SLEV=a,1.4e-2   would contour levels at LEVS * 1.4E-2
c       Default is no additional scaling of LEVS
c@ levs
c       Levels to contour for first image, are LEVS times SLEV
c       (either percentage of the image peak or absolute).
c       Defaults try to choose something sensible
c@ range
c       3 values. The pixel map range (background to foreground), and
c       transfer function type.  The transfer function type can be one
c       of "lin" (linear), "log" (logarithmic), "heq" (histogram equal-
c       ization), and "sqr" (square root).  See also OPTIONS=FIDDLE
c       which is in addition to the selections here.
c
c       Default is linear between the image minimum and maximum
c       If you wish to just give a transfer function type, set
c       range=0,0,heq   say.
c@ device
c       The PGPLOT plot device, such as plot.plt/ps. No default.
c@ nxy
c       Number of sub-plots in the x and y directions on the page.
c       Defaults choose something sensible
c@ labtyp
c       Two values.  The spatial label type of the x and y axes.
c       Minimum match is active.  Select from:
c
c       "hms"       the label is in H M S (e.g. for RA)
c       "dms"       the label is in D M S (e.g. for DEC)
c       "arcsec"    the label is in arcsecond offsets
c       "arcmin"    the label is in arcminute offsets
c       "absdeg"    the label is in degrees
c       "reldeg"    the label is in degree offsets
c                   The above assume the  pixel increment is in radians.
c       "abspix"    the label is in pixels
c       "relpix"    the label is in pixel offsets
c       "abskms"    the label is in Km/s
c       "relkms"    the label is in Km/s offsets
c       "absghz"    the label is in GHz
c       "relghz"    the label is in GHz offsets
c       "absnat"    the label is in natural coordinates as defined by
c                   the header.
c       "relnat"    the label is in offset natural coordinates
c
c       All offsets are from the reference pixel.
c       Defaults are "abspix", LABTYP(1) unless LABTYP(1)="hms"
c       whereupon LABTYP(2) defaults to "dms" (for RA and DEC).
c@ options
c       Task enrichment options.  Minimum match is active.
c
c       "abspix" means write the region of interest in absolute integer
c         pixels instead of arcseconds relative to the reference pixel.
c       "box" When in "CURSOR" mode, rather than listing the value of
c         the of the pixel under the cursor, list the peak value in a
c         5x5 pixel box centred on the pixel under the cursor.
c       "cgspec"  With OPTIONS=CURSOR and LOGFILE, the output log file
c         is is one with commands appropriate for input to CGSPEC's OLAY
c         keyword.
c       "cgdisp"  With OPTIONS=CURSOR and LOGFILE, the output log file
c         is one with commands appropriate for input to CGDISP's OLAY
c         keyword.
c
c         Note that if you specify both CGSPEC and CGDISP then lines
c         appropriate to both these programs are written into the log
c         file.  You can then copy the log file and retain the CGDISP
c         lines in one file, and the CGSPEC lines in the other.
c       "cursor" means that after drawing each sub-plot, a cursor will
c         be displayed; striking any key or clicking the relevant mouse
c         button (left) causes the location and value of the pixel under
c         the cursor to be listed on the terminal.   On terminals, enter
c         "x" to exit the cursor.  On workstations, click the relevant
c         button (generally the right one).
c       "fiddle" means enter a routine to allow you to interactively
c         change the display lookup table.  You can cycle through b&w
c         and colour displays, as well as alter the transfer function by
c         the cursor location, or by selecting predefined transfer
c         functions such as histogram equalization, logarithmic, and
c         square root.
c       "grid" means draw a coordinate grid on the plot rather than just
c         ticks.
c       "logfile"  When the "cursor" or "stats" are activated, then this
c         writes the results to log files (cgcurs.curs and cgcurs.stat)
c         as well as the screen.
c       "mark" When in "CURSOR" mode, mark the locations selected. If
c         OPTIONS=STATS is activated, mark the minimum and maximum pixel
c         locations too.
c       "nearest"  When the cursor is used to select a location, force
c         that location to be the nearest image pixel, rather than the
c         default which allows fractional pixel locations.
c       "noerase"  Don't erase a snugly fitting rectangle into which the
c         "3-axis" value string is written.
c       "region" means use the cursor to define a polygonal region that
c         gets gets written to a log file as the REGION keyword.  The
c         cursor behaves as described above for the "stats" option.  You
c         can the use this in other programs as "region=@filename"
c       "stats"  means that after drawing each sub-plot, you get the
c         opportunity to define a polygonal region with the cursor (A to
c         add a vertex, D to delete the previous vertex, X to exit; or
c         use the three mouse buttons) inside of which image statistics
c         are evaluated.
c       "trlab" means label the top and right axes as well as the bottom
c         and left ones.  This can be useful when non-linear coordinate
c         variation across the field makes the ticks misaligned
c       "unequal" means draw plots with unequal scales in x and y. The
c         default is that the scales are equal.
c       "wedge" means that if you are drawing a pixel map, also draw
c         and label a wedge to the right of the plot, showing the map
c         of intensity to colour
c       "3value"  means label each sub-plot with the appropriate value
c         of the third axis (e.g. velocity or frequency for an xyv
c         ordered cube, position for a vxy ordered cube).
c       "3pixel"  means label each sub-plot with the pixel value of the
c         the third axis.   Both "3pixel" and "3value" can appear, and
c         both will be written on the plot.  They are the average values
c         when the third axis is binned up with CHAN.  If the third axis
c         is not velocity or frequency, the units type for "3VALUE" will
c         be chosen to be the complement of any like axis in the first
c         two.  E.g., the cube is in vxy order and LABTYP=ABSKMS,ARCSEC
c         the units for the "3VALUE" label will be arcsec.  If
c         LABTYP=ABSKMS,HMS the "3VALUE" label will be DMS (if the third
c         [y] axis is declination).
c@ 3format
c       If you ask for "3value" labelling, this keyword allows you
c       specify the FORTRAN format of the labelling.  I have given
c       up trying to invent a decent algorithm to choose this. Examples
c       are "1pe12.6", or "f5.2" etc   If you leave this blank cgdisp
c       will try something that you probably won't like.
c@ csize
c       Two values.  Character sizes in units of the PGPLOT default
c       (which is ~ 1/40 of the view surface height) for the plot axis
c       labels and the velocity/channel labels.
c       Defaults choose something sensible.
c
c$Id: cgcurs.for,v 1.7 2012/02/08 05:31:52 cal103 Exp $
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      integer   MAXLEV, NXDEF, NYDEF, NBINS
      parameter (MAXLEV = 50, NXDEF = 4, NYDEF = 4, NBINS = 128)

      real      TFDISP, WEDWID
      parameter (TFDISP = 0.5, WEDWID = 0.05)

      integer ipim, ipim2, ipnim, ipims

      real    levs(MAXLEV), pixr(2), tr(6), cs(2), pixr2(2), scale(2),
     *        tfvp(4), wdgvp(4), cumhis(NBINS), dmm(3)
      real slev, xmin, xmax, ymin, ymax, vxmin, vymin, vymax, vx, vy,
     *  vxsize, vysize, vxgap, vygap, ydispb, xdispl, groff, blank

      integer blc(3), trc(3), size(maxnax), size2(maxnax), win(maxnax),
     *        grpbeg(maxchan), ngrp(maxchan), srtlev(MAXLEV), his(NBINS)
      integer ibin(2), ierr, ilen, img1, img2, iostat, ipage, jbin(2),
     *        jj, k, kbin(2), krng(2), labcol, lcurs, lreg, lstat,
     *        naxis, naxis2, ngrps, nlast, nlevs, nx, ny, pgbeg, poscol,
     *        regcol, statcol, wedcod

      character labtyp(2)*6
      character img(2)*64, pdev*64, xlabel*40, ylabel*40,
     *  trfun*3, levtyp*1, result*3, val3form*20, versan*80, version*80

      logical cgdisp, cgspec, cmore, cursor, display, do3pix, do3val,
     *        doabs, doabut, doaxlab, doaylab, doblnk, dobox, docont,
     *        doerase, dofid, dolog, donxlab(2), donylab(2), dopixel,
     *        doreg, dotr, dotwo, dowedge, eqscale, first, gaps, grid,
     *        intdev, mark, near, rmore, smore, stats

      data ipage, scale /0, 0.0, 0.0/
      data dmm /1e30, -1e30, -1.0/
      data gaps, doabut /.false., .false./
c-----------------------------------------------------------------------
      version = versan ('cgcurs',
     *                  '$Revision: 1.7 $',
     *                  '$Date: 2012/02/08 05:31:52 $')

c     Get user inputs.
      call inputs(MAXLEV, img, ibin, jbin, kbin, levtyp, slev, levs,
     *   nlevs, pixr, trfun, pdev, labtyp, do3val, do3pix,
     *   eqscale, nx, ny, cs, dopixel, docont, cursor, stats, doreg,
     *   doabs, dolog, cgspec, cgdisp, mark, doerase, dobox, near,
     *   dowedge, dofid, dotr, grid, val3form)

c     Open image(s).
      call opimcg(maxnax, img(1), img1, size, naxis)
      call initco(img1)

      img2  = img1
      dotwo = .false.
      if (dopixel .and. docont .and.  img(2).ne.img(1)) then
        dotwo = .true.
        call opimcg(maxnax, img(2), img2, size2, naxis2)
        if (naxis2.ne.naxis .or.
     *      size2(1).ne.size(1) .or.
     *      size2(2).ne.size(2)) then
          call bug('f', 'Input images are not the same size.')
        endif
      endif

c     Finish key inputs for region of interest now.
      call region(img(1), naxis, size, ibin, jbin, kbin, blc, trc,
     *            win, ngrps, grpbeg, ngrp)

c     Try to allocate memory for images.
      call memalloc(ipim,  win(1)*win(2), 'r')
      call memalloc(ipnim, win(1)*win(2), 'i')
      if (cursor .or. stats) then
c       Need a copy of the image if we have histogram equalized it and
c       want to use the "cursor" or "stats" option to find out image
c       values.
        call memalloc(ipims, win(1)*win(2), 'r')
      else
c       If we don't need a copy of this image, we must still have
c       a pointer to pass down.
        call memalloc(ipims, 1, 'r')
      endif

      ipim2 = ipim
      if (dotwo) then
c       Memory for the contour overlay.
        call memalloc(ipim2,  win(1)*win(2), 'r')
      endif

c Open log files
c
      if (cursor .and. dolog) then
        call txtopen(lcurs, 'cgcurs.curs', 'append', iostat)
        if (iostat.ne.0)
     *    call bug('f', 'Error opening text file "cgcurs.curs"')
        call output(' ')
        call output('*** Values under cursor output to cgcurs.curs')
        call output(' ')
        if (cgspec) call txtwrite(lcurs, 'IRREGULAR', 9, iostat)
      endif
      if (stats .and. dolog) then
        call txtopen(lstat, 'cgcurs.stat', 'append', iostat)
        if (iostat.ne.0)
     *    call bug('f', 'Error opening text file "cgcurs.stat"')
        call output(' ')
        call output('*** Statistics output to cgcurs.stat')
        call output(' ')
      endif
      if (doreg) then
        call txtopen(lreg, 'cgcurs.region', 'append', iostat)
        if (iostat.ne.0)
     *    call bug('f', 'Error opening text file "cgcurs.region"')
        call output(' ')
        call output('*** Region of interest output to cgcurs.region')
        call output(' ')
      endif

      if (dopixel) then
c       Check pixel map for log offset.
        call grfixcg(pixr, img1, naxis, size, trfun, pixr2,
     *               groff, blank)
      endif

      if (docont) then
c       Compute contour levels.
        call conlevcg(.false., MAXLEV, img2, levtyp, slev, nlevs,
     *                levs, srtlev)
        blank = -99999999.0
      endif
c
c Work out coordinate transformation matrix
c
      call limitscg(blc, ibin, jbin, tr)
c
c Work out number of plots per page and number of plots
c
      call nxnycg(NXDEF, NYDEF, ngrps, nx, ny, nlast)
c
c Work out if wedge outside or inside subplots. Also work out
c if plotting one wedge per subplot or one wedge for all
c
      call wedgincg('NO', dofid, dowedge, nx, ny, 1, trfun, wedcod)
c
c Work out default character sizes for axis and channel labels
c
      call defchrcg(nx, ny, cs)
c
c Open plot device
c
      ierr = pgbeg (0, pdev, 1, 1)
      if (ierr.ne.1) then
        call pgldev
        call bug('f', 'Error opening plot device')
      endif

      call pgqinf('hardcopy', result, ilen)
      intdev = .true.
      if (result(1:ilen).eq.'YES') then
        intdev = .false.
        call bug('w',
     *    'Interactive cursor features not available with this device')
      endif


      call pgpage
      call pgscf(2)
c
c Set line graphics colour indices
c
      call setlgc(labcol, poscol, statcol, regcol)
c
c Init OFM routines
c
      if (dopixel) call ofmini
c
c Set axis labels
c
      call setlabcg(img1, labtyp, .false., xlabel, ylabel)
c
c Set label displacements from axes
c
      call setdspcg(img1, labtyp, blc, trc, xdispl, ydispb)
c
c Work out view port encompassing all sub-plots. Also return
c the viewport size of sub-plots.
c
      call vpsizcg(.false., dofid, 0, ' ', ' ', 0, ' ', MAXLEV,
     *  nlevs, srtlev, levs, slev, nx, ny, cs, xdispl, ydispb,
     *  gaps, doabut, dotr, wedcod, WEDWID, TFDISP, labtyp, vxmin,
     *  vymin, vymax, vxgap, vygap, vxsize, vysize, tfvp, wdgvp)
c
c Adjust viewport increments and start locations if equal scales
c requested or if scales provided by user
c
      call vpadjcg(img1, 'NO', eqscale, scale, vxmin, vymin, vymax,
     *  nx, ny, blc, trc, tfvp, wdgvp, vxsize, vysize)
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
         call pgsvp(vx, vx+vxsize, vy, vy+vysize)
         call pgswin(blc(1)-0.5, trc(1)+0.5, blc(2)-0.5, trc(2)+0.5)
c
c Read in image and save it if necessary
c
         if (dotwo) then
           call readimcg(.true., blank, img2, ibin, jbin, krng,
     *       blc, trc, .true., memi(ipnim), memr(ipim2), doblnk, dmm)
         endif

         call readimcg(.true., blank, img1, ibin, jbin, krng,
     *     blc, trc, .true., memi(ipnim), memr(ipim), doblnk, dmm)

         if (cursor .or. stats) then
           call copyimcg(win(1)*win(2), memr(ipim), memr(ipims))
         endif
c
c Apply transfer function
c
         call pgsci(labcol)
         if (dopixel) then
           if (trfun.ne.'lin') call apptrfcg(pixr, trfun, groff,
     *        win(1)*win(2), memi(ipnim), memr(ipim), NBINS,
     *        his, cumhis)
c
c Draw wedge outside of redisplay loop if it will be outside subplots
c and will not get erased
c
           if (wedcod.eq.1 .or. wedcod.eq.2) then
            call pgsch(cs(1))
            call wedgecg(wedcod, WEDWID, jj, trfun, groff, NBINS,
     *                   cumhis, wdgvp, pixr(1), pixr(2))
           endif
         endif
c
c Loop while user wants to redraw plot
c
         display = .true.
         do while (display)
           call pgsci(labcol)
           if (dopixel) then
c            Render pixel map; set default b&w colour table first.
             if (k.eq.1) call ofmcol(1, pixr2(1), pixr2(2))
             call pgimag(memr(ipim), win(1), win(2), 1, win(1), 1,
     *                   win(2), pixr2(1), pixr2(2), tr)
           endif

           if (docont) then
c            Draw contours.
             call conturcg(.false., blank, .false., win(1), win(2),
     *         doblnk, memr(ipim2), nlevs, levs, tr, 0.0, 0, 0)
           endif
c
c Label and draw axes
c
           call pgsch(cs(1))
c
c Determine if the axes need ascii or numeric labelling
c for this subplot
c
           call dolabcg(gaps, dotr, nx, ny, ngrps, nlast, k,
     *                  labtyp, doaxlab, doaylab, donxlab, donylab)
c
c Write on ascii axis labels
c
           if (first) call aaxlabcg(doaxlab, doaylab, xdispl, ydispb,
     *                              xlabel, ylabel)
c
c Draw frame, write numeric labels, ticks and optional grid
c
           call naxlabcg(img1, first, blc, trc, krng, labtyp,
     *                   donxlab, donylab, .false., grid)
c
c Draw wedge inside subplots and overwrite label ticks
c
           if (wedcod.eq.3) then
            call pgsch(cs(1))
            call wedgecg(wedcod, WEDWID, jj, trfun, groff, NBINS,
     *                   cumhis, wdgvp, pixr(1), pixr(2))
           endif
c
c Modify lookup table
c
           if (dofid) call ofmmod(tfvp, win(1)*win(2), memr(ipim),
     *                            memi(ipnim), pixr2(1), pixr2(2))
c
c Write velocity or channel label
c
           if (do3val .or. do3pix) then
             call pgsch(cs(2))
             call pgsci(1)
             call lab3cg(img1, doerase, do3val, do3pix, labtyp,
     *                   grpbeg(k), ngrp(k), val3form)
           endif
c
c Cursor options
c
           if (cmore .and. intdev) then
c
c Read value and location under cursor
c
             call pgsci(poscol)
             call curpos(img1, win(1), win(2), memr(ipims),
     *         memi(ipnim), blc, ibin, jbin, krng, dolog, lcurs,
     *         cgspec, cgdisp, mark, dobox, near)
             cmore = .false.
           endif

           display = .false.
           if (smore .and. intdev) then
c
c Find image statistics in polygonal region defined by cursor
c
             call pgsci(statcol)
             call curstat(img1, blc, win(1), win(2), memr(ipims),
     *         memi(ipnim), ibin, jbin, krng, doreg, display,
     *         smore, dolog, mark, near, lstat)
           endif

           if (.not.display .and. rmore .and. intdev) then
c
c Define polygonal region with cursor
c
             call pgsci(regcol)
             call cureg(img1, blc, ibin, jbin, krng, near, doabs,
     *         display, lreg)
           endif
c
c Erase subplot
c
           if (display) call erswincg(xmin, xmax, ymin, ymax)
           first = .false.
         enddo
c
c Increment sub-plot viewport locations and row counter
c
         call subinccg(k, nx, ny, vxmin, vymax, vxsize, vysize,
     *                 vxgap, vygap, vx, vy)
c
c Page plot device
c
         if (jj.eq.nx*ny .and. k.lt.ngrps) call pgpage
      enddo
c
c Close up
c
      call memfree(ipim,  win(1)*win(2), 'r')
      call memfree(ipnim, win(1)*win(2), 'i')
      if (trfun.eq.'heq' .and. (cursor .or. stats)) then
        call memfree(ipims, win(1)*win(2), 'r')
      else
        call memfree(ipims, 1, 'r')
      endif

      call finco(img1)
      call xyclose(img1)
      if (dotwo) call xyclose(img2)
      if (dolog) then
        if (cursor) call txtclose(lcurs)
        if (stats)  call txtclose(lstat)
      endif
      if (doreg) call txtclose(lreg)
      call pgend

      end

c***********************************************************************

      subroutine cgcur(x, y, ans)
      real x, y
      character ans*1
      call pgupdt
      call pgcurs(x, y, ans)
      call ucase(ans)
      call pgupdt

      end

c***********************************************************************

      subroutine compact(str, trim0)

      character str*(*)
      logical   trim0
c-----------------------------------------------------------------------
c  COmpact string by removing blanks, trailing 0's, decimal points.
c-----------------------------------------------------------------------
      integer il, len1, i, j
      character line*1000
      logical dot
c-----------------------------------------------------------------------
      il = len1(str)

      dot = .true.
      do i = il,1,-1
        if (dot .and.
     *     ((str(i:i).eq.'0' .and. trim0) .or. str(i:i).eq.'.')) then
          dot = str(i:i).eq.'0'
          str(i:i) = ' '
        else
          dot = str(i:i).eq.' ' .or. str(i:i).eq.','
        endif
      enddo

      j = 0
      do i = 1, il
        if (str(i:i).ne.' ') then
          j = j + 1
          line(j:j) = str(i:i)
        endif
      enddo

      str = line(1:j)

      end

c***********************************************************************

      subroutine cureg(img, blc, ibin, jbin, krng, near, doabs,
     *                  redisp, lreg)

      integer   img, lreg, ibin, jbin, krng(2), blc(2)
      logical   redisp, doabs, near
c-----------------------------------------------------------------------
c  Define region of interest with cursor and write to log file.
c
c  Input:
c    img    Handle of image
c    blc    BLC of image
c    i,jbin Spatial pixel binning values
c    krng   Start plane being displayed and number of planes
c           averaged in display
c    lreg   Handle for output text file
c    near   FOrce cursor to nearest pixel
c  Input/output
c    doabs  Output region in absolute pixels instead of offset arcsec
c  Output
c    redisp Have another go after redisplaying the image
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision rtoa
      parameter (rtoa = 3600d0*180d0/dpi)

      integer nvmax, symb
      parameter (nvmax = 100, symb = 17)

      double precision vert(2,nvmax),  pix(3), pixbs(2),
     *  win(3), wout(3)
      real vx(nvmax), vy(nvmax)
      character str1*30, str2*30, str*60, line*500, ans*1, typei(3)*6,
     *  typeo(3)*6
      integer il1, il2, i, ip, il, maxlen, nv, irad(2), iostat, bin(2),
     *  naxis3, naxis
      logical good, more, rads

      integer len1, ci
c-----------------------------------------------------------------------
      call output(' ')
      call output('**********************************')
      call output('Entering region of interest option')
      call output('**********************************')
      call output(' ')
      call output('Click left button   (enter A) to mark vertex')
      call output
     *  ('Click middle button (enter D) to delete previous vertex')
      call output('Click right button  (enter X) to finish polygon')
      call output(' ')
c
c Do we have an axes in radians, can't output locations
c in arcsecond offsets otherwise.
c
      call axfndco(img, 'RAD', 0, 1, irad(1))
      call axfndco(img, 'RAD', 0, 2, irad(2))
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
      typeo(3) = 'abspix'

      win(3) = (real(2*krng(1)+krng(2))-1.0)/2.0
      call rdhdi(img, 'naxis', naxis, 0)
      naxis = min(3,naxis)
c
c Get vertices with cursor and join up the dots
c
      more = .true.
      do while (more)
        nv = 0
        call pgupdt
c
c Get vertices with cursor; coordinates in absolute pixels
c
        call pgolin(nvmax, nv, vx, vy, symb)

        if (nv.gt.nvmax) then
          write(line,10) nv, nvmax
10        format('Too many (', i4, ') vertices, max. = ', i4)
          call bug('w', line(1:len1(line)))
        else if (nv.le.1) then
          call bug('w', 'Not enough vertices for a region')
        else
c
c Convert to nearest binned pixel, and then convert to
c unbinned full image pixels, if desired
c
          if (near) then
c
c Rub out old points
c
            call pgqci(ci)
            call pgsci(0)
            call pgpt(nv, vx, vy, symb)
            do i = 1, nv
              pix(1) = vx(i)
              pix(2) = vy(i)
              call nearcon(bin, blc, pix, pixbs)
              vx(i) = pix(1)
              vy(i) = pix(2)
            enddo
c
c Draw new points
c
            call pgsci(ci)
            call pgpt(nv, vx, vy, symb)
          endif
          call pgsfs(2)
          call pgslw(2)
c
c Join up the dots.
c
          call pgpoly(nv, vx, vy)
          call pgupdt
          call pgslw(1)
c
c Make integer copy of unbinned full image pixel vertices
c
          do i = 1, nv
            vert(1,i) = vx(i)
            vert(2,i) = vy(i)
          enddo
c
c Eliminate redundant vertices
c
          call elimrvd(nvmax, nv, vert)
c
c Convert unbinned full image pixels to true arcsec offsets if desired
c
          if (.not.doabs) then
            do i = 1, nv
              win(1) = vert(1,i)
              win(2) = vert(2,i)
              call w2wco(img, naxis, typei, ' ', win, typeo, ' ', wout)
              vert(1,i) = wout(1)
              vert(2,i) = wout(2)
            enddo
          endif
c
c Write out region of interest
c
          line = ' '
          maxlen = len(line)
          if (doabs) then
            line = 'abspix,poly('
          else
            line = 'arcsec,poly('
          endif
          ip = len1(line) + 1

          good = .true.
          i = 1
          do while (i.le.nv .and. good)
            if (doabs) then
              call strfi(nint(vert(1,i)), '(i4)', str1, il1)
              call strfi(nint(vert(2,i)), '(i4)', str2, il2)
            else
              call strfd(vert(1,i), '(f15.2)', str1, il1)
              call strfd(vert(2,i), '(f15.2)', str2, il2)
            endif
            str = str1(1:il1)//','//str2(1:il2)
            call compact(str, .not.doabs)
            il = len1(str) + 1
            str(il:il) = ','
            if (i.eq.nv) str(il:il) = ')'
c
c Write into line if room
c
            if (ip+il.gt.maxlen) then
              call bug('w', 'Too many vertices for line length')
              good = .false.
            else
              line(ip:) = str(1:il)
              ip = ip + il
            endif
            i = i + 1
          enddo
c
c Add image plane
c
          call rdhdi(img, 'naxis3', naxis3, 0)
          if (naxis3.gt.1) then
            call strfi(krng(1), '(i6)', str, il)
            if (krng(2).ne.1) then
              str(il+1:) = ','
              call strfi(krng(1)+krng(2)-1, '(i6)', str(il+2:), il)
              il = len1(str)
            endif
            if (ip+il.gt.maxlen) then
              call bug('w',
     *          'Not enough room in line to write image plane')
            else
              line(ip:) = '('//str(1:il)//')'
            endif
          endif
c
c Write into log file
c
          if (good) call txtwrite(lreg, line, len1(line), iostat)
        endif
c
c Have another go
c
        call output(' ')
        call output('Draw another region with this display:   A')
        call output('Draw another region after redisplaying:  R')
        call output('Finish region option:                    X')
        call output(' ')
        call cgcur(vx, vy, ans)
        if (ans.eq.'A') then
          more = .true.
          redisp = .false.
        else if (ans.eq.'R') then
          more = .false.
          redisp = .true.
        else
          more = .false.
          redisp = .false.
        endif
      enddo

      end

c***********************************************************************

      subroutine curpos(img, nx, ny, image, nimage, blc, ibin, jbin,
     *   krng, dolog, lcurs, cgspec, cgdisp, mark, dobox, near)

      integer nx, ny, nimage(nx,ny), blc(2), lcurs, ibin, jbin,
     *  krng(2), img
      real image(nx,ny)
      logical dolog, cgspec, cgdisp, mark, dobox, near
c-----------------------------------------------------------------------
c  Return pixel location and value under cursor
c
c  Input:
c     img     Image handle
c     nx,ny   Size of image
c     image   The image without the transfer function is applied
c     blc     blc of window being displayed
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
c     plst,av STart plane and  number of planes averaged
c
c-----------------------------------------------------------------------
      double precision pix(3), pixbs(2)
      real w(2), ival
      integer iostat, len1, iloc, ipl, wl(3), wwl(2), vl(3), k,
     *  bin(2), ib, jb, naxis
      character cch*1, line*132, plstr*20, vstr(3)*60, wstr(3)*60,
     *  wwstr(3)*20, typei(3)*6, typeo(3)*6
c-----------------------------------------------------------------------
      call output(' ')
      call output('****************************')
      call output('Entering cursor value option')
      call output('****************************')
      call output(' ')
      call output('Click left button  (enter A) for location')
      call output('Click right button (enter X) to exit')
      call output(' ')

      typei(1) = 'abspix'
      typei(2) = 'abspix'
      typei(3) = 'abspix'
      pix(3) = (real(2*krng(1)+krng(2))-1.0)/2.0
      call rdhdi(img, 'naxis', naxis, 0)
      naxis = min(3,naxis)
c
c Format channel range for CGDISP log files
c
      call strfi(krng(1), '(i6)', plstr, ipl)
      if (krng(2).ne.1) then
        plstr(ipl+1:) = ' '
        call strfi(krng(1)+krng(2)-1, '(i6)', plstr(ipl+2:), ipl)
        ipl = len1(plstr)
      endif

      bin(1) = ibin
      bin(2) = jbin
      cch = ' '
      iloc = 0
      do while (cch.ne.'X')
c
c Read cursor; location in absolute image pixels.  Find
c location in binned subimage pixels
c
        call cgcur(w(1), w(2), cch)
        pix(1) = w(1)
        pix(2) = w(2)
        if (near) then
          call nearcon(bin, blc, pix, pixbs)
        else if (dobox) then
          call pkfind(nx, ny, image, nimage, blc, bin, pix, pixbs)
        else
          do k = 1, 2
            pixbs(k) = pix(k)
            call ppconcg(1, blc(k), bin(k), pixbs(k))
          enddo
        endif
        w(1) = pix(1)
        w(2) = pix(2)
c
c Keep an integer copy of binned subimage pixel
c
        ib = nint(pixbs(1))
        jb = nint(pixbs(2))

        if (ib.lt.1 .or. ib.gt.nx .or. jb.lt.1 .or. jb.gt.ny) then
          call bug('w', 'Cursor off image')
        else
c
c Mark on plot if desired
c
          if (cch.ne.'X') then
            iloc = iloc + 1
            if (mark) then
              call pgslw(2)
              call pgpt(1, w(1), w(2), 2)
              call pgupdt
              call pgslw(1)
            endif
            call output(' ')

            if (dolog) then
c
c Write separator to log file
c
              if (cgspec .or. cgdisp) then
                call txtwrite(lcurs, '#', 1, iostat)
              else
                call txtwrite(lcurs, ' ', 1, iostat)
              endif
            endif
c
c Convert absolute pixel to true world coordinate formatted strings
c with and without units
c
            call setoaco(img, 'abs', naxis, 0, typeo)
            call w2wfco(img, naxis, typei, ' ', pix,  typeo, ' ',
     *                  .true., wstr, wl)
            call w2wfco(img, naxis, typei, ' ', pix, typeo, ' ',
     *                  .false., vstr, vl)

            line = 'World coordinates x,y         : '//
     *              vstr(1)(1:vl(1))//', '//vstr(2)(1:vl(2))
            call output(line)
c
c Write log files
c
            if (dolog) then
              if (.not.cgspec .and. .not.cgdisp)
     *          call txtwrite(lcurs, line, len1(line), iostat)

              if (cgspec) then
                write(line, 5) typeo(1)(1:len1(typeo(1))),
     *                          typeo(2)(1:len1(typeo(2))),
     *                          wstr(1)(1:wl(1)), wstr(2)(1:wl(2))
5               format(a, 1x, a, 1x, a, 1x, a)
                call txtwrite(lcurs, line, len1(line), iostat)
              endif

              if (cgdisp) then
                call pixinc(img, bin, wwstr, wwl)

                write(line, 8) typeo(1)(1:len1(typeo(1))),
     *            typeo(2)(1:len1(typeo(2))), iloc ,
     *            wstr(1)(1:wl(1)), wstr(2)(1:wl(2)),
     *            wwstr(1)(1:wwl(1)), wwstr(2)(1:wwl(2)),
     *            plstr(1:ipl)
8               format('star ', a, 1x, a, i4, ' no ', a, 1x,
     *                  a, 1x, a, 1x, a, 1x, a)
                call txtwrite(lcurs, line, len1(line), iostat)
              endif
            endif
c
c Convert absolute pixel to true offset world coordinate formatted
c strings.
c
            call setoaco(img, 'off', naxis, 0, typeo)
            call w2wfco(img, naxis, typei, ' ', pix,  typeo, ' ',
     *                   .false., wstr, wl)
            line = 'Offset world coordinates x,y  : '//
     *              wstr(1)(1:wl(1))//', '//wstr(2)(1:wl(2))
            call output(line)
            if (dolog .and. (.not.cgspec .and. .not.cgdisp))
     *          call txtwrite(lcurs, line, len1(line), iostat)
c
c Absolute pixels.
c
            typeo(1) = 'abspix'
            typeo(2) = 'abspix'
            typeo(3) = 'abspix'
            call w2wfco(img, naxis, typei, ' ', pix, typeo, ' ',
     *                  .true., wstr, wl)
            if (naxis.gt.2) then
              write(line, 10) wstr(1)(1:wl(1)), wstr(2)(1:wl(2)),
     *                         wstr(3)(1:wl(3))
10            format('Image pixel coordinates x,y,z : ', a, ', ', a,
     *                ', ', a)
            else
              write(line, 20) wstr(1)(1:wl(1)), wstr(2)(1:wl(2))
20            format('Image pixel coordinates x,y   : ', a, ', ', a)
            endif
            call output(line)
            if (dolog .and. (.not.cgspec .and. .not.cgdisp))
     *          call txtwrite(lcurs, line, len1(line), iostat)
c
c Image intensity; allow for transfer function taking
c
            call strfi(nint(pix(1)), '(i4)', wstr(1), wl(1))
            call strfi(nint(pix(2)), '(i4)', wstr(2), wl(2))
            if (nimage(ib,jb).ne.0) then
              ival = image(ib,jb)

              if (naxis.gt.2) then
                write(line, 40) ival, wstr(1)(1:wl(1)),
     *             wstr(2)(1:wl(2)), wstr(3)(1:wl(3))
40              format('Image intensity               :', 1pe12.4,
     *                  ' at pixel (', a, ', ', a, ', ', a, ')')
              else
                write(line, 45) ival, wstr(1)(1:wl(1)),
     *             wstr(2)(1:wl(2))
45              format('Image intensity               :', 1pe12.4,
     *                  ' at pixel (', a, ', ', a, ')')
              endif
              call output(line)
              if (dolog .and. (.not.cgspec .and. .not.cgdisp))
     *          call txtwrite(lcurs, line, len1(line), iostat)
            else
              if (naxis.gt.2) then
                write(line, 50) wstr(1)(1:wl(1)),
     *            wstr(2)(1:wl(2)), wstr(3)(1:wl(3))
50              format('Image intensity               : blanked',
     *                  ' at pixel (', a, ', ', a, ', ', a, ')')
              else
                write(line, 55) wstr(1)(1:wl(1)),
     *            wstr(2)(1:wl(2))
55              format('Image intensity               : blanked',
     *                  ' at pixel (', a, ', ', a, ')')
              endif
              call output(line)
              if (dolog .and. (.not.cgspec .and. .not.cgdisp))
     *          call txtwrite(lcurs, line, len1(line), iostat)
            endif
          endif
        endif
      enddo

      end

c***********************************************************************

      subroutine curstat(img, blc, nx, ny, image, nimage, ibin, jbin,
     *    krng, doreg, redisp, smore, dolog, mark, near, lstat)

      integer nx, ny, nimage(nx,ny), blc(2), img, lstat, ibin, jbin,
     *  krng(2)
      real image(nx,ny)
      logical redisp, doreg, smore, dolog, mark, near
c-----------------------------------------------------------------------
c  Work out statistics from region marked with cursor.  If the
c  delineated region is invalid, you exit from here, the sub-plot
c  is redrawn, and you get another go.  It has to be redrawn
c  becuase simply erasing the polygon will also erase the underlying
c  image.  You get three goes at drawing a decent polygon
c  before it quits.
c
c  Input:
c    img    Handle of image
c    blc    Blc of sub-image displayed
c    nx,ny  Size of displayed sub-image
c    image  Sub-image (values without transfer function applied)
c    nimage Normalization sub-image
c    i,jbin Spatial pixel increment
c    krng   Start channel and number of channels averaged in
c           current display
c    doreg  True if going on to cursor region option next
c    dolog  Write to log file as well
c    mark   Mark min and max
c    near   Force cursor locations to nearest pixel
c    lstat  Handle of output text file
c  Output:
c    redisp Redisplay the image
c    smore  Do more statistics options
c-----------------------------------------------------------------------
      integer symb, nvmax, maxruns
      parameter (symb = 17, nvmax = 100, maxruns = 50)

      integer vert(2,nvmax), runs(maxruns), nruns, nv, i, j, k, iostat,
     *  npix, iymin, iymax, kd, t, len1, ci, bin(2), naxis, wl(3)
      double precision cdelt1, cdelt2, imin, jmin, imax, jmax,
     *  pix(3), pixbs(2)
      real vx(nvmax), vy(nvmax), sum, sumsq, mean, var, rms,
     *  dmin, dmax, bmin, bmaj, barea, ival
      character line*80, ans*1, bunit*16, typei(3)*6, typeo(3)*6,
     *  wstr(3)*132, bunit2*16, tmp*16
      logical good, more, perbeam
c-----------------------------------------------------------------------
      call output(' ')
      call output('**************************')
      call output('Entering statistics option')
      call output('**************************')
      call output(' ')
      call output('Click left button   (enter A) to mark vertex')
      call output
     *  ('Click middle button (enter D) to delete previous vertex')
      call output('Click right button  (enter X) to finish polygon')
      call output(' ')
c
c Get beam if present
c
      call rdhdi(img, 'naxis', naxis, 0)
      naxis = min(3,naxis)
      do i = 1, naxis
        typei(i) = 'abspix'
      enddo
      pix(3) = (real(2*krng(1)+krng(2))-1.0)/2.0

      call rdhdr(img, 'bmaj', bmaj, 0.0)
      call rdhdr(img, 'bmin', bmin, 0.0)
      call rdhdd(img, 'cdelt1', cdelt1, 0d0)
      call rdhdd(img, 'cdelt2', cdelt2, 0d0)
      call rdhda(img, 'bunit', bunit, ' ')
      barea = 1.1331 * bmaj * bmin / abs(cdelt1 * cdelt2)
      bin(1) = ibin
      bin(2) = jbin
c
c Open log file as required
c
      more = .true.
      do while (more)
c
c Get vertices with cursor; corrdinates in absolute pixels
c
        nv = 0
        call pgupdt
        call pgolin(nvmax-1, nv, vx, vy, symb)
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
            call pgqci(ci)
            call pgsci(0)
            call pgpt(nv, vx, vy, symb)
c
c Find nearest unbinned pixel
c
            do i = 1, nv
              pix(1) = vx(i)
              pix(2) = vy(i)
              call nearcon(bin, blc, pix, pixbs)
              vx(i) = pix(1)
              vy(i) = pix(2)
            enddo
c
c Draw new points
c
            call pgsci(ci)
            call pgpt(nv, vx, vy, symb)
          endif
c
c Join up the vertices of the polygon
c
          call pgsfs(2)
          call pgslw(2)
          call pgpoly(nv, vx, vy)
          call pgupdt
          call pgslw(1)
c
c Loop over vertices
c
          i = 1
          iymin = 1000000
          iymax = 0
          good = .true.

          do while (i.le.nv .and. good)
c
c Convert unbinnned full image pixels to integer binned pixels
c
            pix(1) = vx(i)
            call ppconcg(1, blc(1), ibin, pix)
            vert(1,i) = nint(pix(1))

            pix(2) = vy(i)
            call ppconcg(1, blc(2), jbin, pix(2))
            vert(2,i) = nint(pix(2))
c
c Update y pixel extrema
c
            iymin = min(iymin,vert(2,i))
            iymax = max(iymax,vert(2,i))

            if (vert(1,i).lt.1 .or. vert(1,i).gt.nx .or.
     *          vert(2,i).lt.1 .or. vert(2,i).gt.ny) then
              call bug('w', 'Polygon off image, try again')
              good = .false.
           else
              good = .true.
           endif

            i = i + 1
          enddo
c
c Eliminate redundant vertices
c
          if (good) then
            call elimrvi(nvmax, nv, vert)
            nv = nv + 1
            vert(1,nv) = vert(1,1)
            vert(2,nv) = vert(2,1)
c
c  Check if polygon in clockwise order.
c
            t = 0
            do k = 1, nv-1
              t = t + vert(1,k)*vert(2,k+1) - vert(2,k)*vert(1,k+1)
            enddo
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
              enddo
            endif
c
c Find runs array and accumulate statistics for unblanked
c pixels in each row
c
            sum = 0.0
            sumsq = 0.0
            dmin = 1e30
            dmax = -1e30
            npix = 0

            do j = iymin, iymax
              call polyruns(runs, maxruns, j, nv, vert, nruns)

              if (nruns.gt.0) then
                do k = 1, nruns, 2
                  do i = runs(k), runs(k+1)
                    if (nimage(i,j).ne.0) then
c
c Pixel unblanked, find value
c
                      ival = image(i,j)
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
                      endif

                      if (ival.gt.dmax) then
                        dmax = ival
                        imax = i
                        jmax = j
                      endif
                    endif
                  enddo
                enddo
              endif
            enddo
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
              endif
c
c Tell user
c
              call output(' ')
              if (dolog) call txtwrite(lstat, ' ', 1, iostat)
              call unitdec(bunit, tmp, bunit2, perbeam)

              if (barea.gt.0.0 .and. perbeam) then
                write(line, 10) sum, sum/barea, bunit2
10              format('Sum = ', 1pe12.5, '   Flux density = ',
     *                  1pe12.5, ' ', a)
              else if (bunit.eq.'JY/PIXEL') then
                write(line, 12) sum
12              format('Flux density  = ', 1pe12.5, ' Jy')
              else
                write(line, 15) sum
15              format('Sum = ', 1pe12.5)
              endif
              call output(line)
              if (dolog) call txtwrite(lstat, line, len1(line), iostat)

              write(line, 16) dmin, dmax, bunit
16            format('Minimum = ', 1pe12.5, '  Maximum = ',
     *                1pe12.5, 1x, a)
              call output(line)
              if (dolog) call txtwrite(lstat, line, len1(line), iostat)

              write(line, 17) mean, rms, npix
17            format('Mean = ', 1pe12.5, '  sigma = ', 1pe12.5,
     *                ' from ', i8, ' valid pixels')
              call output(line)
              if (dolog) call txtwrite(lstat, line, len1(line), iostat)
c
c Give data min and max value locations in offset pixels and
c in absolute world coordinates
c
              call ppconcg(2, blc(1), ibin, imin)
              call ppconcg(2, blc(2), jbin, jmin)
              typeo(1) = 'abspix'
              typeo(2) = 'abspix'
              typeo(3) = 'abspix'
              pix(1) = imin
              pix(2) = jmin
              call w2wfco(img, naxis, typei, ' ', pix, typeo, ' ',
     *                     .false., wstr, wl)
              line = 'Data minimum at '//
     *              wstr(1)(1:wl(1))//', '//wstr(2)(1:wl(2))
              call output(line)
              if (dolog) call txtwrite(lstat, line, len1(line), iostat)

              call ppconcg(2, blc(1), ibin, imax)
              call ppconcg(2, blc(2), jbin, jmax)
              pix(1) = imax
              pix(2) = jmax
              call w2wfco(img, naxis, typei, ' ', pix, typeo, ' ',
     *                     .false., wstr, wl)
              line = 'Data maximum at '//
     *              wstr(1)(1:wl(1))//', '//wstr(2)(1:wl(2))
              call output(line)
              if (dolog) call txtwrite(lstat, line, len1(line), iostat)
c
c Now give location in absolute world coordinate
c
              call setoaco(img, 'abs', naxis, 0, typeo)
              pix(1) = imin
              pix(2) = jmin
              call w2wfco(img, naxis, typei, ' ', pix, typeo, ' ',
     *                     .false., wstr, wl)
              line = 'Data minimum at '//
     *              wstr(1)(1:wl(1))//', '//wstr(2)(1:wl(2))
              call output(line)
              if (dolog) call txtwrite(lstat, line, len1(line), iostat)

              pix(1) = imax
              pix(2) = jmax
              call w2wfco(img, naxis, typei, ' ', pix, typeo, ' ',
     *                     .false., wstr, wl)
              line = 'Data maximum at '//
     *              wstr(1)(1:wl(1))//', '//wstr(2)(1:wl(2))
              call output(line)
              if (dolog) call txtwrite(lstat, line, len1(line), iostat)
c
c Mark location of min and max on plot if desired
c
              if (mark) then
                call pgpt(1, real(imin), real(jmin), 2)
                call pgpt(1, real(imax), real(jmax), 2)
                call pgupdt
              endif
            else
              call bug('w',
     *          'There were no valid pixels inside the polygon')
            endif
          endif
        else
          call bug('w',
     *              'A polygon with only one vertex is not very useful')
        endif
c
c Have another go
c
        redisp = .false.
        smore = .false.

        call output('  ')
        call output('Draw another region with this display:   A')
        call output('Draw another region after redisplaying:  R')
        call output('Finish statistics option:                X')
        if (doreg)
     *  call output('Finish statistics option and redisplay:  C')

        call cgcur(vx, vy, ans)
        if (ans.eq.'A') then
          more = .true.
        else if (ans.eq.'R') then
          more = .false.
          redisp = .true.
          smore = .true.
          call output('Redisplaying')
        else if (ans.eq.'X') then
          more = .false.
        else if (ans.eq.'C') then
          more = .false.
          redisp = .true.
        endif
      enddo

      end

c***********************************************************************

      subroutine decopt(do3val, do3pix, eqscale, cursor, stats, doreg,
     *                    doabs, dolog, cgspec, cgdisp, mark, doerase,
     *                    dobox, near, dowedge, dofid, dotr, grid)

      logical do3val, do3pix, eqscale, cursor, stats, doreg, near,
     *  doabs, dolog, cgspec, cgdisp, mark, doerase, dobox, dofid,
     *  dowedge, dotr, grid
c-----------------------------------------------------------------------
c  Decode options array into named variables.
c
c  Output:
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
c     dowedge   Draw wedge on pixel map
c     dofid     FIddle lookup table of pixel map
c     dotr      Label top right as well
c     grid      Draw coordinate grid
c-----------------------------------------------------------------------
      integer maxopt
      parameter (maxopt = 18)

      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'3value  ', '3pixel  ', 'unequal ', 'stats   ',
     *              'cursor  ', 'region  ', 'abspixel', 'logfile ',
     *              'cgspec  ', 'cgdisp  ', 'mark    ', 'noerase ',
     *              'box     ', 'nearest ', 'wedge   ', 'fiddle  ',
     *              'trlab   ', 'grid    '/
c-----------------------------------------------------------------------
      call optcg('options', opshuns, present, maxopt)

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
      dotr     =      present(17)
      grid     =      present(18)

      end

c***********************************************************************

      subroutine elimrvd(nmax, n, v)

      integer nmax, n
      double precision v(2,nmax)
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
c-----------------------------------------------------------------------
      integer kd, k
c-----------------------------------------------------------------------
      v(1,n+1) = v(1,1)
      v(2,n+1) = v(2,1)
      kd = 1
      do k = 2, n
       if ((v(2,k+1)-v(2,k))*(v(1,k)-v(1,kd)).ne.
     *    (v(2,k)-v(2,kd))*(v(1,k+1)-v(1,k))) then
         kd = kd + 1
         v(1,kd) = v(1,k)
         v(2,kd) = v(2,k)
       endif
      enddo
      n = kd

      if (n.lt.3) then
        call bug('w', 'Degenerate polygon in ElimRVr')
      else
c
c  Check if the first pixel is colinear. This cannot deal with this, and
c  craps out.
c
        if ((v(2,2)-v(2,1))*(v(1,1)-v(1,n)).eq.
     *     (v(2,1)-v(2,n))*(v(1,2)-v(1,1))) then
          v(1,1) = v(1,n)
          v(2,1) = v(2,n)
          n = n - 1
        endif

        if (n.lt.3) call bug('w', 'Degenerate polygon in ElimRVd')
      endif

      end

c***********************************************************************

      subroutine elimrvi(nmax, n, v)

      integer nmax, n, v(2,nmax)
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
c-----------------------------------------------------------------------
      integer kd, k
c-----------------------------------------------------------------------
      v(1,n+1) = v(1,1)
      v(2,n+1) = v(2,1)
      kd = 1
      do k = 2, n
       if ((v(2,k+1)-v(2,k))*(v(1,k)-v(1,kd)).ne.
     *    (v(2,k)-v(2,kd))*(v(1,k+1)-v(1,k))) then
         kd = kd + 1
         v(1,kd) = v(1,k)
         v(2,kd) = v(2,k)
       endif
      enddo
      n = kd
      if (n.lt.3) then
        call bug('w', 'Degenerate polygon in ElimRVi')
      else
c
c  Check if the first pixel is colinear. This cannot deal with this, and
c  craps out.
c
        if ((v(2,2)-v(2,1))*(v(1,1)-v(1,n)).eq.
     *     (v(2,1)-v(2,n))*(v(1,2)-v(1,1))) then
          v(1,1) = v(1,n)
          v(2,1) = v(2,n)
          n = n - 1
        endif

        if (n.lt.3) call bug('w', 'Degenerate polygon in ElimRVi')
      endif

      end

c***********************************************************************

      subroutine inputs(maxlev, img, ibin, jbin, kbin, levtyp, slev,
     *   levs, nlevs, pixr, trfun, pdev, labtyp, do3val, do3pix,
     *   eqscale, nx, ny, cs, dopixel, docont, cursor, stats, doreg,
     *   doabs, dolog, cgspec, cgdisp, mark, doerase, dobox, near,
     *   dowedge, dofid, dotr, grid, val3form)

      integer maxlev, nx, ny, nlevs, ibin(2), jbin(2), kbin(2)
      real levs(maxlev), pixr(2), cs(2), slev
      character*(*) labtyp(2), img(2), pdev, trfun, levtyp, val3form
      logical do3val, do3pix, eqscale, cursor, stats, doreg, dopixel,
     *  docont, doabs, dolog, cgspec, mark, cgdisp, doerase, dobox,
     *  near, dowedge, dofid, grid, dotr, dunw
c-----------------------------------------------------------------------
c  Get the unfortunate user's long list of inputs
c
c  Input:
c   maxlev     Maximum number of allowed contour levels
c  Output:
c   img        Image name(s).
c   i,j,kbin   X, y and z pixel increment and average
c   levtyp     Type of contour levels scale factor
c              'p'(ercentage) or 'a'(bsolute)
c   slev       Contour levels scale factors (absolute or percentage)
c   levs       Contour levels.  Will be scaled by SLEV for contouring
c   nlevs      Number of contour levels
c   pixr       Pixel map intensity range
c   trfun      Type of pixel map transfer function: 'log', 'lin',
c              'heq', or 'sqr'
c   pdev       PGPLOT plot device/type
c   labtyp     Type of labels for x and y axes
c   do3val     True means label sub-plots with value of third axis
c   do3pix     True means label sub-plots with pixel of third axis
c   eqscale    True means plot with x and y scales
c   nx,ny      Number of sub-plots per page
c   cs         PGPLOT character sizes for the plot axis labels and
c              velocity/channel label,
c   dopixel    If true do pixel map.
c   docont     If true do contour overlay
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
c   dofid      FIddle lookup tbale of pixel map
c   dowedge    Draw wedge with pixel map
c   dotr       Label top/right as well
c   grid       Draw coordinate grid
c   val3for    Format for 3value labelling
c-----------------------------------------------------------------------
      integer ntype, nlab, ntype2, nimg, nimtype
      parameter (ntype = 16, ntype2 = 4)
      character type(ntype)*6, imtype*7, type2(ntype2)*7
      data type  /'hms   ', 'dms   ', 'abspix', 'relpix',
     *            'arcsec', 'arcmin', 'absghz', 'relghz',
     *            'abskms', 'relkms', 'absnat', 'relnat',
     *            'absdeg', 'reldeg', 'abslin', 'rellin'/
      data type2 /'contour', 'pixel', 'grey', 'both'/
      data dunw /.false./
c-----------------------------------------------------------------------
      call keyini
      call mkeyf('in', img, 2, nimg)
      if (nimg.eq.0) call bug('f', 'No image(s) specified.')

      call keymatch('type', ntype2, type2, 1, imtype, nimtype)
      dopixel = .false.
      docont  = .false.
      if (nimtype.eq.0) then
        dopixel = .true.
        if (nimg.eq.2) docont = .true.
      else if (imtype.eq.'pixel' .or. imtype.eq.'grey') then
        dopixel = .true.
      else if (imtype.eq.'contour') then
        docont  = .true.
      else if (imtype.eq.'both') then
        dopixel = .true.
        docont = .true.
        if (nimg.eq.1) img(2) = img(1)
      else
        call bug('f', 'Unrecognized image type.')
      endif

      call keyi('xybin', ibin(1), 1)
      call keyi('xybin', ibin(2), ibin(1))
      if (ibin(2).ne.1 .and. ibin(2).ne.ibin(1)) call bug('f',
     *  'Non-unit x spatial averaging must be equal to increment')
      ibin(1) = max(ibin(1), 1)
      ibin(2) = max(ibin(2), 1)

      call keyi('xybin', jbin(1), ibin(1))
      call keyi('xybin', jbin(2), jbin(1))
      if (jbin(2).ne.1 .and. jbin(2).ne.jbin(1)) call bug('f',
     *  'Non-unit y spatial averaging must be equal to increment')
      jbin(1) = max(jbin(1), 1)
      jbin(2) = max(jbin(2), 1)

      call keyi('chan', kbin(1), 1)
      call keyi('chan', kbin(2), 1)
      kbin(1) = max(kbin(1), 1)
      kbin(2) = max(kbin(2), 1)

      call keya('slev', levtyp, 'a')
      call keyr('slev', slev, 0.0)
      call lcase(levtyp)
      if (levtyp.ne.'p' .and. levtyp.ne.'a') call bug('f',
     *   'Unrecognized contour level scale type; must be "p" or "a"')

      call mkeyr('levs', levs, maxlev, nlevs)

      call keyr('range', pixr(1), 0.0)
      call keyr('range', pixr(2), 0.0)
      call keya('range', trfun, 'lin')
      call lcase(trfun)
      if (dopixel) then
        if (trfun.ne.'lin' .and. trfun.ne.'log' .and. trfun.ne.'heq'
     *      .and. trfun.ne.'sqr') call bug('f',
     *    'Unrecognized pixel map transfer function type')
      else
        trfun = ' '
      endif

      call keya('device', pdev, ' ')
      if (pdev.eq.' ') then
        call pgldev
        call bug('f', 'A PGPLOT device must be given')
      endif

      call decopt(do3val, do3pix, eqscale, cursor, stats, doreg, doabs,
     *             dolog, cgspec, cgdisp, mark, doerase, dobox, near,
     *             dowedge, dofid, dotr, grid)
      if (.not.cursor .or. .not.dolog) cgspec = .false.
      if (.not.cursor) dobox = .false.
      if (near .and. dobox) call bug('f',
     *  'You can''t have options=near and options=box')
      if (.not.dopixel) then
        dofid = .false.
        dowedge = .false.
      endif
      if (cgdisp .or. cgspec) dolog = .true.

      call keymatch('labtyp', ntype, type, 2, labtyp, nlab)
      if (nlab.eq.0) labtyp(1) = 'abspix'
      if (nlab.le.1) then
        labtyp(2) = labtyp(1)
        if (labtyp(1).eq.'hms') labtyp(2) = 'dms'
      endif
      if (labtyp(1)(4:6).eq.'lin') then
        labtyp(1)(4:6) = 'nat'
        call bug('w', 'Axis label types abslin and rellin are ')
        call bug('w', 'deprecated in favour of absnat and relnat')
        dunw = .true.
      endif
      if (labtyp(2)(4:6).eq.'lin') then
        labtyp(2)(4:6) = 'nat'
        if (.not.dunw) then
          call bug('w', 'Axis label types abslin and rellin are ')
          call bug('w', 'deprecated in favour of absnat and relnat')
        endif
      endif

      if ((index(labtyp(1),'nat').ne.0  .and.
     *      index(labtyp(2),'nat').eq.0)) then
        if (eqscale) call bug('i',
     *  'You might consider options=unequal with these axis types')
      endif

      call keya('3format', val3form, ' ')

      call keyi('nxy', nx, 0)
      call keyi('nxy', ny, nx)

      call keyr('csize', cs(1), 0.0)
      call keyr('csize', cs(2), 0.0)

      end

c***********************************************************************

      subroutine nearcon(bin, blc, pix, pixbs)

      integer bin(2), blc(2)
      double precision pix(2), pixbs(2)
c-----------------------------------------------------------------------
c     Take an unbinned full image pixel, and find its corresponding
c     binned pixel. Then take the nearest binned pixel, then find
c     the world coordinate of that location and the unbinned
c     full image pixel of that lcoation
c
c  Input
c   bin      Pixel binning factors in x and y
c   blc      BLC of image being displayed
c  Input/output
c   pix      Unbinned full image pixel.  On output its the value
c            equivalent to pixbs
c  Output
c   pixbs    Nearest binned subimage pixel
c-----------------------------------------------------------------------
      integer k
c-----------------------------------------------------------------------
c
c Loop over axes
c
      do k = 1, 2
c
c Convert to binned subimage pixels.
c
        call ppconcg(1, blc(k), bin(k), pix(k))
c
c Take nearest subimage pixel and keep copy
c
        pix(k) = nint(pix(k))
        pixbs(k) = pix(k)
c
c Convert back to full image unbinned pixels
c
        call ppconcg(2, blc(k), bin(k), pix(k))
      enddo

      end

c***********************************************************************

      subroutine pixinc(img, bin, wwstr, wwl)

      integer img, bin(2), wwl(2)
      character*(*) wwstr(2)
c-----------------------------------------------------------------------
c  Find pixel increments for each axis in appropriate units
c  Work them out at the reference pixel of the image (all
c  axes).  Could in principle work them out at the exact
c  coordinate (all axes) that user has generated, but it doesn't
c  really matter for this applciation (writing the width of
c  an overlay) as its arbitrary anyway
c
c  Input
c   img     Image handle
c   bin     Spatial binning
c  Output
c   wwstr   Array of formatted pixel increments
c   wwl     Length of strings
c-----------------------------------------------------------------------
      double precision w1(2), w2(2), pix1(2), pix2(2), winc(2)
      character*6 typei(2), typeo(2)
      integer i
c-----------------------------------------------------------------------
c
c Work out default offset units for axis
c
      call setoaco(img, 'off', 2, 0, typeo)
c
c Find increments
c
      do i = 1, 2
       pix1(i) = 0d0
       pix2(i) = 1d0
       typei(i) = 'relpix'
      enddo

      call w2wco(img, 2, typei, ' ', pix1, typeo, ' ', w1)
      call w2wco(img, 2, typei, ' ', pix2, typeo, ' ', w2)
      winc(1) = w2(1) - w1(1)
      winc(2) = w2(2) - w1(2)
c
c Format
c
      do i = 1, 2
        call strfd(2*bin(i)*abs(winc(i)), '(1pe13.6)',
     *             wwstr(i), wwl(i))
      enddo

      end

c***********************************************************************

      subroutine pkfind(nx, ny, image, nimage, blc, bin, pix, pixbs)

      integer nx, ny, nimage(nx,ny), bin(2), blc(2)
      real image(nx,ny)
      double precision pix(2), pixbs(2)
c-----------------------------------------------------------------------
c  Find peak pixel in a box centred on input pixel location
c
c  Input
c    nx,ny    SIze of binned subimage
c    image    Binned subimage
c    nimage   Binned sub-mask-image
c    blc      BLC of full unbinned image
c    bin      Pixel binning
c  Input/output
c    pix      Full image unbinned pixel
c    pixbs    Binned subimage pixel
c
c-----------------------------------------------------------------------
      real dmax
      integer k, is, ie, js, je, im, jm, ii, jj
c-----------------------------------------------------------------------
c
c Convert unbinned full image pixels to binned subimage pixels
c
      do k = 1, 2
        pixbs(k) = pix(k)
        call ppconcg(1, blc(k), bin(k), pixbs(k))
      enddo
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
      dmax = -1e30
      im = -1
      jm = -1
      do jj = js, je
        do ii = is, ie
          if (nimage(ii,jj).ne.0 .and. image(ii,jj).gt.dmax) then
            im = ii
            jm = jj
            dmax = image(ii,jj)
          endif
        enddo
      enddo
c
c If there is something unblanked return it, else stick with where
c we started
c
      if (im.ne.-1 .and. jm.ne.-1) then
        pixbs(1) = im
        pixbs(2) = jm
c
c Convert back to full image unbinned pixels
c
        do k = 1, 2
          pix(k) = pixbs(k)
          call ppconcg(2, blc(k), bin(k), pix(k))
        enddo
      endif

      end

c***********************************************************************

        subroutine polyruns(goes,maxgoes,j0,nverts,verts,ngoes)

        integer maxgoes,goes(maxgoes),j0,nverts,verts(2,nverts),ngoes
c-----------------------------------------------------------------------
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
c    /          ---^---            _________
c   /_____        / \             /
c   \            /   \           /
c    \          /     \         /
c
c  Input:
c    nverts     Number of veritces of the polygon.
c    verts      The vertices of the polygon. The vertices are assumes to
c               have no redundancies (i.e. all vertices are distinct),
c               and to trace out a anti-clockwise path.
c    j0         The value of y for which we want to determine the runs
c               inside the polygon.
c    maxgoes    Max number of runs is maxgoes/2.
c  Output:
c    goes       The runs for this value of y.
c    ngoes      The number of runs is ngoes/2.
c
c
c  This is the same as BOXPOLYX in BOXES.FOR.  Itr is an internal
c  subroutine in BOXES so I am not allowed to call it.
c  Hence the RJS style.
c-----------------------------------------------------------------------
        integer k,kprev,l,t
        logical more
c-----------------------------------------------------------------------
        ngoes = 0
        kprev = nverts-1
        do k = 1,nverts-1,1
c
c  Case of an intersection with a vertex.
c
          if (verts(2,k).eq.j0) then
            t = (j0-verts(2,kprev))*(j0-verts(2,k+1))
            if (t.gt.0) then
              ngoes = ngoes + 2
              goes(ngoes-1) = verts(1,k)
              goes(ngoes)   = verts(1,k)
            else if (t.lt.0) then
              ngoes = ngoes + 1
              goes(ngoes) = verts(1,k)
            else
              t =   verts(1,kprev)*(verts(2,k)    -verts(2,k+1)  )
     *            + verts(1,k)    *(verts(2,k+1)  -verts(2,kprev))
     *            + verts(1,k+1)  *(verts(2,kprev)-verts(2,k)    )
              if (t.gt.0) then
                ngoes = ngoes + 1
                goes(ngoes) = verts(1,k)
              endif
            endif
c
c  Case of an intersection with the line segment between vertices.
c
          else if ((j0-verts(2,k))*(verts(2,k+1)-j0).gt.0) then
            ngoes = ngoes + 1
            goes(ngoes) =  nint(verts(1,k+1) +
     *          real((j0-verts(2,k+1)) * (verts(1,k)-verts(1,k+1)))
     *                  / (verts(2,k)-verts(2,k+1)))
          endif
          kprev = k
        enddo

        if (2*(ngoes/2).ne.ngoes)
     *    call bug('f','Algorithmic failure in BoxRuns(polyx)')
c
c  The list of intersections are not in order.  The number of
c  intersections is also likely to be small (probably only two!).  Sort
c  the intersections, but use an insert-sort, because its probably
c  ordered, and small.
c
        do k = 2, ngoes
          l = k
          t = goes(l)
          more = goes(l-1).gt.t
          do while (more)
            goes(l) = goes(l-1)
            l = l - 1
            more = .false.
            if (l.gt.1) more = goes(l-1).gt.t
          enddo
          goes(l) = t
        enddo
c
c  There are possibly redundancies in the list of runs. Eliminate these.
c
        l = 3
        do k = 3,ngoes,2
          if (goes(k)-goes(l-1).le.1) then
            goes(l-1) = goes(k+1)
          else
            goes(l) = goes(k)
            goes(l+1) = goes(k+1)
            l = l + 2
          endif
        enddo
        ngoes = l-1

        end

c***********************************************************************

      subroutine region(in, naxis, size, ibin, jbin, kbin, blc, trc,
     *                   win, ngrps, grpbeg, ngrp)

      integer naxis, size(naxis), blc(*), trc(*), win(2), ngrp(*),
     *  grpbeg(*), ngrps, ibin(2), jbin(2), kbin(2)
      character in*(*)
c-----------------------------------------------------------------------
c  Finish key routine inputs for region of interest now.
c
c  Input:
c    in            Image file name
c    naxis         Number of dimensions of image
c    size          Dimensions of image
c    i,j,kbin      Pixel increment and binning in x,yz directions
c  Output:
c    ngrps         Number of groups of channels.
c    grgbeg        List of start planes for each group of channels
c                  that are to be avearged together for each sub-plot
c                  A new group is begun at every interruption to the
c                  continuity of the selected channels, or if the
c                  channel increment is reached.
c    ngrp          Number of channels in each group of channel to
c                  be averaged together for each sub-plot.
c    blc,trc       3-D Hyper-rectangle surrounding region of interest
c    win           Size of BINNED region of interest for
c                  first 2 dimensions
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer maxbox, i
      parameter (maxbox = 1024)

      integer boxes(maxbox)
c-----------------------------------------------------------------------
      call boxinput('region', in, boxes, maxbox)
      call boxset(boxes, naxis, size, 's')
      call keyfin
c
c Find hyper-rectangle surrounding region of interest
c
      call boxinfo(boxes, 3, blc, trc)
      do i = 1, min(3,naxis)
        blc(i) = max(1,blc(i))
        trc(i) = min(size(i),trc(i))
      enddo
c
c Adjust spatial window to fit an integral number of bins and
c find size of binned window
c
      call winfidcg(size(1), 1, ibin, blc(1), trc(1), win(1))
      call winfidcg(size(2), 2, jbin, blc(2), trc(2), win(2))
c
c Find list of start channels and number of channels for each group
c of channels selected.
c
      call chnselcg(blc, trc, kbin, maxbox, boxes, ngrps, grpbeg, ngrp)

      end

c***********************************************************************

      subroutine setlgc(labcol, poscol, statcol, regcol)

      integer labcol, poscol, statcol, regcol
c-----------------------------------------------------------------------
c     Set line graphics colours
c
c  OUtput
c    colour indices to use
c-----------------------------------------------------------------------
      integer bgcol
c-----------------------------------------------------------------------
c
c See if black or white background
c
      call bgcolcg(bgcol)
c
c Labels first
c
      labcol = 7
      if (bgcol.eq.1) then
c
c White background
c
        labcol = 2
      else if (bgcol.eq.0) then
c
c Black background
c
        labcol = 7
      else
        call bug('w', 'Non black/white background colour on device')
        labcol = 7
      endif
c
c Now cursor options
c
      poscol = 3
      statcol = labcol
      regcol = 8

      end

c***********************************************************************

      subroutine unitdec(in, tmp, out, perbeam)

      character*(*) in, out, tmp
c-----------------------------------------------------------------------
      integer idx, idx1, idx2, len1
      logical perbeam
c-----------------------------------------------------------------------
      perbeam = .false.
      out = in

      tmp = in
      call ucase(tmp)
      idx = index(tmp, '/BEAM')
      if (idx.ne.0) then
         idx1 = idx
         idx2 = idx + 4
      else
         idx = index(tmp, '/B')
         if (idx.ne.0) then
            idx1 = idx
            idx2 = idx + 1
         endif
      endif
      if (idx.eq.0) return

      perbeam = .true.
      out(1:) = in(1:idx1-1)
      if (idx2+1.le.len1(in)) then
         out(idx1:) = in(idx2+1:)
      endif

      end
