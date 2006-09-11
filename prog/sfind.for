      program sfind
c-----------------------------------------------------------------------
c= SFIND - Interactively find sources in images on a PGPLOT device
c& nebk
c: plotting
c+
c       SFIND displays an image via a contour plot or a pixel map
c       representation on a PGPLOT device. The user is then provided
c       with the opportunity to interactively flag sources as real or
c       not (indicated by a Y or N flag in a log file).
c
c       Source positions are calculated by an algorithm which searches for
c       pixels brighter than the surrounding 24 pixels and then
c       bi-parabolically fitting positions and flux densities.
c       Once a source such as this is detected, SFIND checks to see whether
c       it is brighter than the user set multiple of the background rms.
c       If so, a 2D elliptical gaussian fit is performed (using the same
c       routine as IMFIT) and the source parameters are displayed on the
c       terminal (and written to a log file after user input to determine
c       a flag, Y or N, to attach). The source parameters are (in order):
c
c                 Quantity                        Notes
c              --------------                  -----------
c       Position                       RA and Dec. in standard miriad
c                                      hms,dms format
c       Formal errors in RA and Dec.   (arcsec; treat judicously)
c       Peak flux density              (mJy)
c       Formal error in peak flux      in mJy (generally not a good
c       density                        estimate of the true error)
c       Integrated flux density        (mJy)
c       Major and minor axes and       (arcseconds for axes, degrees for PA)
c       position angle of source       Warning: these are not deconvolved
c                                      from the synthesized beam
c       Local background rms (sigma)   (mJy) can be misleading if source is
c                                      close to other comparitively bright
c                                      sources (ie, they will be included in
c                                      region surrounding source within which
c                                      the background rms is calculated)
c       rms of gaussian fit
c
c
c       Manipulation of the device colour lookup table is available
c       when you display with a pixel map representation.
c
c@ in
c       The input image.
c@ type
c       Specifies the type of the image in the IN keyword. Minimum match 
c       is supported.   Choose from:
c
c       "contour"   (contour plot)
c       "pixel"     (pixel map)
c
c       It is strongly suggested that pixel maps be used for source finding,
c       as contour plots may be deceiving. Default is "pixel"
c
c@ region
c       Region of interest.  Choose only one spatial region (bounding 
c       box only supported), but as many spectral regions (i.e., 
c       multiple IMAGE specifications) as you like.  If you display a
c       3-D image, the cursor options are activated after each sub-plot 
c       (channel or group of channels; see CHAN below) is drawn.  
c       Default is full image
c@ xybin
c       Upto 4 values.  These give the spatial increment and binning
c       size in pixels for the x and y axes to be applied to the selected
c       region.   If the binning size is not unity, it must be equal
c       to the increment.  For example, to bin up the image by 4 pixels in 
c       the x direction and to pick out every third pixel in the y 
c       direction, set XYBIN=4,4,3,1
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
c       ization), and "sqr" (square root).  See also OPTIONS=FIDDLE which
c       is in addition to the selections here.
c
c       Default is linear between the image minimum and maximum
c       If you wish to just give a transfer function type, set
c       range=0,0,heq   say.
c@ cutoff
c       Flux density below which possible sources are ignored. No default.
c@ rmsbox
c       Size (in pixels) of a box around each source within which the
c       background rms is calculated. Default is 20 pixels.
c@ xrms
c       Multiple of the background rms value above which a source must be
c       before the user is given the choice of verifying it. No default.
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
c            The above assume the  pixel increment is in radians.
c       "abspix"    the label is in pixels
c       "relpix"    the label is in pixel offsets
c       "abskms"    the label is in Km/s
c       "relkms"    the label is in Km/s offsets
c       "absghz"    the label is in GHz
c       "relghz"    the label is in GHz offsets
c       "absnat"    the label is in linear coordinates as defined by 
c                   the header you might call this the natural axis label
c       "relnat"    the label is in offset natural coordinates
c       
c       All offsets are from the reference pixel.  
c       Defaults are "abspix", LABTYP(1) unless LABTYP(1)="hms"
c       whereupon LABTYP(2) defaults to "dms" (for RA and DEC).
c@ options
c       Task enrichment options.  Minimum match is active.
c
c       "fiddle" means enter a routine to allow you to interactively change
c         the display lookup table.  You can cycle through b&w and colour
c         displays, as well as alter the transfer function by the cursor 
c         location, or by selecting predefined transfer functions such as 
c         histogram equalization, logarithmic, & square root.
c       "wedge" means that if you are drawing a pixel map, also draw
c         and label a wedge to the right of the plot, showing the map 
c         of intensity to colour
c
c       "3value"  means label each sub-plot with the appropriate value
c         of the third axis (e.g. velocity or frequency for an xyv ordered 
c         cube, position for a vxy ordered cube).
c       "3pixel"  means label each sub-plot with the pixel value of the
c         the third axis.   Both "3pixel" and "3value" can appear, and both 
c         will be written on the plot.  They are the average values when
c         the third axis is binned up with CHAN.  If the third axis is
c         not velocity or frequency, the units type for "3VALUE" will be 
c         chosen to be the complement of any like axis in the first 2. 
c        E.g., the cube is in vxy order and LABTYP=ABSKMS,ARCSEC the units 
c        for the "3VALUE" label will be arcsec.  If LABTYP=ABSKMS,HMS the 
c        "3VALUE" label will be DMS (if the third [y] axis is declination).
c
c       "grid" means draw a coordinate grid on the plot rather than just ticks
c
c       "noerase"  Don't erase a snugly fitting rectangle into which the 
c        "3-axis" value string is written.
c      
c       "unequal" means draw plots with unequal scales in x and y. The
c        default is that the scales are equal.
c
c       "mark" When source has been found, and user has agreed that it is
c         real, mark it with a cross.
c
c       "nofit" Prevents the program from fitting elliptical gaussians to each
c         source. The data given on each source will be that from a
c         bi-parabolic fit, as per the earlier version of sfind. Note that
c         flux densities from this fit are bi-parabolically fitted *peak*
c         flux densities, and the positions are to the peak flux density
c         position (which will always be within 1 pixel of the brightest
c         pixel in the source). This option is useful for providing a starting
c         point for groups of sources which the gaussian fitting procedure 
c         hasn't taken a liking to.
c
c       "asciiart" During the interactive section of the program, an ascii
c         picture of each source is displayed, showing which pixels have
c         been used in the gaussian fitting procedure. The brightest pixel
c         in the source is symbolised by a "O", the rest by asterisks.
c         This option is ignored if "nofit" is being used.
c
c       "auto" The interactive section of the program is bypassed, and
c         all detected sources are flagged as real. The image is not
c         displayed.
c
c       "negative" The map is inverted before source detection and fitting,
c         ie, positive pixels become negative and vice versa. This is
c         to enable detection of negative sources without recourse to MATHS.
c         This feature may be used for detecting sources in polarisation
c         maps.
c
c@ csize
c       Two values.  Character sizes in units of the PGPLOT default
c       (which is ~ 1/40 of the view surface height) for the plot axis
c       labels and the velocity/channel labels.
c       Defaults choose something sensible.
c
c  Known Bugs:
c       The output is designed to print source fluxes in FORTRAN format
c       f8.3 and f9.3 for peak and integrated flux densities respectively.
c       This means that if your source's peak flux is > 9999.999 mJy, (ie
c       10 Jy) or its integrated flux is > 99999.999 mJy (ie, 100 Jy),
c       then it will not be displayed properly. People detecting very bright
c       sources - you have been warned!
c
c       The gaussian fitting procedure can at times be temperamental. If the
c       source lies in a noisy region of the map, or close to another bright
c       source, or is simply of a morphology poorly suited to being fit by
c       gaussians, firstly the source may not be detected at all, and if it
c       is, the quoted errors on position and flux density can be extremely
c       high (often displayed in the output as a row of asterisks due to the
c       vagaries of FORTRAN).
c       In many of these cases, the given values of flux density and position
c       are still quite reasonable, (perhaps with errors an order of magnitude
c       larger than would otherwise be typical), but user discretion is
c       advised. No responsibility is taken by the programmer(s) for loss
c       of life or property caused by taking the results of this program
c       under these conditions too seriously, or by frustration generated
c       by the use of this program under any conditions.
c       Additionally, for unresolved sources, the "integrated" flux
c       density quoted may be less than the peak flux density. (This occurs if
c       the fitted size of the source, proportional to bmaj x bmin, is a
c       smaller gaussian volume than that of the beam.) In this situation it
c       is suggested that the peak flux density be used.
c
c     Suggestions for believing in a source or not:
c       If a source is close to being indistinguishable by eye from the
c       background there are a few rules of thumb to help determine whether
c       the gaussian fit is telling the truth about a source, or whether the
c       source is even real.
c       1) If the pixels used in the fit are widely scattered (as opposed to
c          comprising a nice contiguous group) the fit will probably not be
c          very good and/or will not be a good description of the source.
c       2) Check the fwhms and the position angle, and compare it to the
c          pixels used in the fit. (Remember these values are in arcsec for
c          the fwhm and degrees for the pa, while the ascii picture is in
c          pixels). If these obviously do not agree, then the fit was poor
c          and the source is probably not real.
c       3) Check the rms of the background. If this is high then firstly
c          the fit may not be good (as per 1), and secondly the source is in
c          a noisy area and should be treated with caution anyway.
c--
c
c  History:
c    amh  28sep95  Major hacks to turn into SFIND from CGCURS, using source
c                  finding algorithm from AIPS task SMFND (by Taisheng Ye).
c    amh  23jan96  Added routine from imfit to do elliptical gaussian source
c                  fitting of identified sources.
c    amh  24jan96  Added options nofit and asciiart, for avoiding gaussian
c                  fits and sending ascii images of sources to screen.
c    nebk 12feb96  Massaged to fix subroutine call mismatches, remove
c                  unused variables, remove hangovers from cgcurs that
c                  were not needed, gave unvalued variables a value,
c                  remove unused subroutines.  Made no default for "cutoff"
c                  and xrms. Put some protection into BASECAL for bad 
c                  rms values. Change structure of code in subroutine SEARCH
c                  deal with blanks in BASECAL. 
c    amh  05mar96  added iterative section to subroutine FITTING, whereby
c                  sources with fwhm comparable to area used for selecting
c                  pixels are re-calculated with a larger area. Also deal
c                  with blanks in SEARCH and LOADDAT (blanks now correctly
c                  dealt with throughout). Tidied up subroutine descriptions,
c                  etc.
c    rjs  18mar96  Some FORTRAN standardisation.
c    amh  21apr97  added "auto" option, to give user choice of interactive
c                  flagging of sources or not.
c    amh  28apr97  added "negative" option, to give user ability to detect
c                  negative sources without having to seperately use MATHS.
c    amh  14may97  fixed bug concerning accidental "exit" button press. If this
c                  was accidental, user previously had no way (other than
c                  rerunning program) of accepting source detected when button
c                  was accidentally pressed.
c
c To do:
c
c       Add option to allow list of source positions to be read from file
c       and imfit-type fitting performed at those positions. Also include
c       in this option an option to search for the brightest pixel near the
c       given position to do the fit, or alternatively try to force the fit
c       to occur at the specified position.
c
c       Add option to allow user to "point and click" at sources and have a
c       fit done to that position (or the nearest bright pixel, by some
c       criterion, perhaps the brightest pixel in a 10x10 box centred on the
c       cursor). This could be used as well as or instead of the current
c       method. Think about two seperate options to indicate user's desire
c       of which.
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
      integer ipim, ipnim
c
      real levs(maxlev), pixr(2), tr(6), cs(2), pixr2(2), scale(2), 
     +  tfvp(4), wdgvp(4), cumhis(nbins), dmm(2)
      real slev, vxmin, vymin, vymax, vx, vy, vxsize, vysize, vxgap, 
     +  vygap, ydispb, xdispl, groff, blank, cut, xrms
c
      integer blc(3), trc(3), size(maxnax), win(maxnax),
     +  grpbeg(maxchan), ngrp(maxchan), srtlev(maxlev), his(nbins)
      integer nx, ny, nlevs, lin, naxis, k, ierr, pgbeg, iostat, ipage,
     +  ibin(2), jbin(2), kbin(2), krng(2), nlast, ngrps,
     +  llog, jj, wedcod, labcol, poscol, statcol, regcol, rmsbox
c
      character labtyp(2)*6
      character in*64, pdev*64, xlabel*40, ylabel*40, trfun*3, levtyp*1
c
      logical do3val, do3pix, eqscale, doblnk, dotr, donxlab(2),
     +  donylab(2), dopixel, gaps, doabut, 
     +  doaxlab, doaylab,
     +  mark, doerase, dowedge, dofid,
     +  grid, nofit, asciiart, auto, negative
c
      data ipage, scale /0, 0.0, 0.0/
      data dmm /1.0e30, -1.0e30/
      data gaps, doabut, dotr /.false., .false., .false./
c-----------------------------------------------------------------------
      call output (' ')
      call output ('Sfind: version 1.22, 14-May-97')
      call output (' ')
c
c Get user inputs
c
      call inputs (maxlev, in, ibin, jbin, kbin, levtyp, slev, levs, 
     +   nlevs, pixr, trfun, pdev, labtyp, do3val, do3pix, eqscale, 
     +   nx, ny, cs, dopixel, mark, doerase, dowedge, dofid, grid,
     +   cut, rmsbox, xrms, nofit, asciiart, auto, negative)
c
c Open log files
c
      call txtopen (llog, 'sfind.log', 'append', iostat)
      if (iostat.ne.0) 
     +  call bug ('f', 'Error opening text file "sfind.log"')
      call output (' ')
      call output ('*** Source list output to sfind.log')
      call output (' ')
      call output ('Now opening image...')
      call output (' ')
c
c Open image
c
      call opimcg (maxnax, in, lin, size, naxis)
c
c Finish key inputs for region of interest now
c
      call region (in, naxis, size, ibin, jbin, kbin, blc, trc,
     +             win, ngrps, grpbeg, ngrp)
c
c Try to allocate memory for images.  
c
      call memalloc (ipim,  win(1)*win(2), 'r')
      call memalloc (ipnim, win(1)*win(2), 'i')
c
c Compute contour levels or check pixel map for log offset
c
      if (dopixel) then
        call grfixcg (pixr, lin, naxis, size, trfun, pixr2,
     +                groff, blank)
      else
        call conlevcg (.false., maxlev, lin, levtyp, slev, nlevs,
     +                 levs, srtlev)
        blank = -99999999.0
      end if
c
c Work out coordinate transformation matrix
c
      call limitscg (blc, ibin, jbin, tr)
c
c If the source detection procedure is not to be automated, then perform all
c the image opening, initialization, etc. Otherwise skip all this.
c
      if (.not.auto) then
c
c Work out number of plots per page and number of plots
c
       call nxnycg (nxdef, nydef, ngrps, nx, ny, nlast)
c
c Work out if wedge outside or inside subplots. Also work out
c if plotting one wedge per subplot or one wedge for all  
c       
       call wedgincg ('NO', dofid, dowedge, nx, ny, 1, trfun, wedcod)
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
       call pgpage
       call pgscf(2)
c
c Set line graphics colour indices
c
       call setlgc (labcol, poscol, statcol, regcol)
c       
c Init OFM routines
c       
       if (dopixel) call ofmini
c
c Set axis labels
c
       call setlabcg (lin, labtyp, .false., xlabel, ylabel)
c 
c Set label displacements from axes
c
       call setdspcg (lin, labtyp, blc, trc, xdispl, ydispb)
c
c Work out view port encompassing all sub-plots. Also return 
c the viewport size of sub-plots.
c
       call vpsizcg (.false., dofid, 0, ' ', ' ', 0, ' ', maxlev,
     +   nlevs, srtlev, levs, slev, nx, ny, cs, xdispl, ydispb, 
     +   gaps, doabut, dotr, wedcod, wedwid, tfdisp, labtyp, vxmin,
     +   vymin, vymax, vxgap, vygap, vxsize, vysize, tfvp, wdgvp)
c
c Adjust viewport increments and start locations if equal scales
c requested or if scales provided by user
c
       call vpadjcg (lin, 'NO', eqscale, scale, vxmin, vymin, vymax,
     +   nx, ny, blc, trc, tfvp, wdgvp, vxsize, vysize)
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
c Set viewport and window for current sub-plot
c
         call pgsvp (vx, vx+vxsize, vy, vy+vysize)
         call pgswin (blc(1)-0.5, trc(1)+0.5, blc(2)-0.5, trc(2)+0.5)
c
c Read in image
c
         call readimcg (.true., blank, lin, ibin, jbin, krng, blc,
     +     trc, .true., memi(ipnim), memr(ipim), doblnk, dmm)
c
c Apply transfer function
c
         call pgsci (labcol)
         if (dopixel) then
           if (trfun.ne.'lin') call apptrfcg (pixr, trfun, groff, 
     +        win(1)*win(2), memi(ipnim), memr(ipim), nbins,
     +        his, cumhis)
c
c Draw wedge 
c
           if (wedcod.eq.1 .or. wedcod.eq.2) then
            call pgsch (cs(1))
            call wedgecg (wedcod, wedwid, jj, trfun, groff, nbins,
     +                    cumhis, wdgvp, pixr(1), pixr(2))
           end if
         end if
c
c Draw pixel map; set default b&w colour table first.
c
         call pgsci (labcol)
         if (dopixel) then
           if (k.eq.1) call ofmcol (1, pixr2(1), pixr2(2))
           call pgimag (memr(ipim), win(1), win(2), 1, win(1), 1,
     +                  win(2), pixr2(1), pixr2(2), tr)
         else 
c
c Draw contours
c
           call conturcg (.false., blank, .false., win(1), win(2),
     +                    doblnk, memr(ipim), nlevs, levs, tr, 0.0)
         end if
c
c Determine if the axes need ascii or numeric labelling
c for this subplot
c
         call pgsch (cs(1))
         call dolabcg (gaps, dotr, nx, ny, ngrps, nlast, k,
     +                 labtyp, doaxlab, doaylab, donxlab, donylab)
c
c Write on ascii axis labels
c

         call aaxlabcg (doaxlab, doaylab, xdispl, ydispb,
     +                             xlabel, ylabel)
c
c Draw frame, write numeric labels, ticks and optional grid
c
         call naxlabcg (lin, .true., blc, trc, krng, labtyp,
     +                  donxlab, donylab, .false., grid)
c
c Draw wedge inside subplots and overwrite label ticks
c
         if (wedcod.eq.3) then
           call pgsch (cs(1))
           call wedgecg (wedcod, wedwid, jj, trfun, groff, nbins,
     +                  cumhis, wdgvp, pixr(1), pixr(2))
         end if
c
c Modify lookup table
c
         if (dofid) call ofmmod (tfvp, win(1)*win(2), memr(ipim), 
     +                           memi(ipnim), pixr2(1), pixr2(2))
c
c Write velocity or channel label
c
         if (do3val .or. do3pix) then
           call pgsch (cs(2))
           call pgsci (1)
           call lab3cg (lin, doerase, do3val, do3pix, labtyp,
     +                  grpbeg(k), ngrp(k))
         end if
c
c Interactive graphical source finding routine
c
         call search (lin, win(1), win(2), memr(ipim), memi(ipnim),
     +     blc, ibin, jbin, krng, llog, mark, cut, rmsbox, xrms, 
     +     nofit, asciiart, auto, negative)
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
c
       call xyclose(lin)
       call txtclose(llog)
       call pgend
c
c Now the image display section of code is passed, perform search automatically
c (non-interactively) if required.
c
      else
c
c Loop over groups of channels selected in "region"
c
       do k = 1, ngrps
        krng(1) = grpbeg(k)
        krng(2) = ngrp(k)
c
c Read in image
c
        call readimcg (.true., blank, lin, ibin, jbin, krng, blc,
     +     trc, .true., memi(ipnim), memr(ipim), doblnk, dmm)
c
c The source-detection subroutine.
c
        call search(lin, win(1), win(2), memr(ipim),
     +     memi(ipnim), blc, ibin, jbin, krng, llog, mark, cut,
     +     rmsbox, xrms, nofit, asciiart, auto, negative)
       end do
c
c Close up
c
       call memfree (ipim,  win(1)*win(2), 'r')
       call memfree (ipnim, win(1)*win(2), 'i')
c
       call xyclose(lin)
       call txtclose(llog)
      end if
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
      call pgupdt
      call ucase (ans)
c
      end
c
c
      subroutine search (lin, nx, ny, image, nimage, blc, ibin, jbin,
     +   krng, llog, mark, cut, rmsbox, xrms, nofit, asciiart, auto,
     +   negative)
c-----------------------------------------------------------------------
c  This is the master subroutine for the detecting of sources and the
c interactive decision bit. It detects bright pixels, determines whether they
c are brighter than the surrounding 24 pixels (of which none are permitted to
c be blanked) and if so, performs a bi-parabolic fit to the peak. This is
c then used to determine if the source is above a user set multiple of the
c background rms around the source, and if so an elliptical gaussian fit is
c carryed out. The results are then written to the screen and the cursor
c moved to the position of the object where it waits for the users yay or nay.
c After user input, the source parameters are written, along with a flag (Y
c or N), to the log file, sfind.log
c
c  Input:
c     lin       Image handle
c     nx,ny     Size of image
c     image     Image
c     nimage    Normalization image (0 for blanked pixels)
c     blc       blc of window being displayed
c     i,jbin    Spatial pixel increment 
c     krng      Start plane and number of planes averaged together
c               to make the current displayed plane
c     llog      Handle of log file
c     mark      True to mark cursor locations
c     cut       flux limit below which sources are ignored
c     rmsbox    size of box (in pixels) within which background rms is
c               first calculated
c     xrms      multiple of background rms above which source must be before
c               user is given option of saying yay or nay
c     nofit     True means don't do gaussian fitting for each source
c     asciiart  display ascii representations of each source during
c               interactive source selection
c     auto      skip all the interactive bits of source flagging.
c     negative  inverts image: positive pixels become negative and vice versa
c
c-----------------------------------------------------------------------
      implicit none
c
      integer nx, ny, blc(2), llog, ibin, jbin, lin, nimage(nx,ny),
     +  krng(2)
      real image(nx,ny)
      logical mark, nofit, asciiart, auto, negative
cc
      double precision wa(2), posns(2)
      real ww(2), wsave(2), cut, xrms, peak, base0
      real xpos, ypos, pval, sigma, rms, mult
      real pkfl, intfl, amaj, amin, posa
      real xposerr, yposerr, pkflerr
      real bvol, bmaj, bmin, bpa, bvolp, bmajp
      integer iostat, len1, iloc, bin(2), l, m, radeclen(2), rmsbox
      integer sources, ysources, k, i, j
      character cch*1, line*160, typei(2)*6, radec(2)*80, typeo(2)*6
      character line2*160, line3*160, line4*160
      logical ok, nobeam, fitok
c-----------------------------------------------------------------------
      call output (' ')  
      if (.not.auto) then
       call output ('************************************')
       call output ('Beginning Interactive Source Finding')
       call output ('************************************')
       call output (' ')
       call output ('Click left button   (enter A) to flag source as Y')
       call output
     +  ('Click middle button (enter D) to flag source as N')
       call output ('Click right button  (enter X) to quit')
       call output (' ')
      else
       call output ('****************************************')
       call output ('Beginning Non-Interactive Source Finding')
       call output ('****************************************')
       call output (' ')
       call output ('Please be patient...')
      end if
c
c Initialize
c
      call initco (lin)
      bin(1) = ibin
      bin(2) = jbin
      cch = ' '
      iloc = 0
      sources = 0
      ysources = 0
c
c Invert image if options=negative selected
c
      if (negative) then
       do l = 1,nx
        do m = 1,ny
         image(l,m) = -image(l,m)
        end do
       end do
      end if
c
c Write header for output to screen, unless "auto" option selected.
c
      if (nofit) then
        write(line2,20) 'RA','DEC','flux','x','y','rms',
     +                  'flux/rms'
20      format(5x,a,11x,a,4x,a,7x,a,8x,a,7x,a,2x,a)
        if (.not.auto) call output(line2)
        line3 = '                         mJy      pixels   '//
     +          'pixels    mJy'
        if (.not.auto) call output (line3)
        line4 = '------------------------------------------'//
     +          '-------------------------------------'
        if (.not.auto) call output (line4)
       else
        write(line2,30) 'RA','DEC','err(RA)','err(DEC)','pk-flux',
     +                  'err','flux','bmaj','bmin','pa',
     +                  'rms(bg)','rms(fit)'
30      format(5x,a,9x,a,5x,a,1x,a,1x,a,3x,a,6x,
     +         a,3x,a,3x,a,3x,a,1x,a,1x,a)
        if (.not.auto) call output(line2)       
        line3 = '                        arcsec   arcsec   '//
     +          'mJy      mJy       mJy  arcsec arcsec '//
     +          'deg  mJy     mJy'
        if (.not.auto) call output (line3)
        line4 = '------------------------------------------'//
     +          '-------------------------------------'//
     +          '----------------'
        if (.not.auto) call output (line4)
      end if
c
c Write output header for log file
c
      call txtwrite (llog, line2, len1(line2), iostat)
      call txtwrite (llog, line3, len1(line3), iostat)
      call txtwrite (llog, line4, len1(line4), iostat)
c
c Set the plane appropriate to the displayed image.  Since
c this may be a range of planes averaged together, take the 
c integer nearest the average plane
c
      k = nint(real(krng(1) + krng(1) + krng(2) - 1)/2.0)
c
c Determine beam parameters, including determining if the beam doesn't
c exist.
c
      call BeamPar (lIn, k, bvol, bvolp, bmaj, bmin, bpa, 
     +              bmajp, nobeam)
c
c Loop over all pixels, searching for bright pixels and moving the cursor
c to that position.
c
      do l = 3,nx-3
       do m = 3,ny-3
        pval = image(l,m)
c
c Ignore pixels less than cut and check to see if pixel brighter than
c surrounding 24 pixels, making sure none are blanked pixels.
c
        if (pval.lt.cut) goto 60
        do i = l-2,l+2
          do j = m-2,m+2
            if( ((i.ne.l .or. j.ne.m) .and. nimage(i,j).gt.0 .and.
     +          pval.le.image(i,j)) .or. nimage(i,j).eq.0) goto 60
          end do
        end do
c
c Bi-parabolic fit, base-level and background rms calculation and subtraction
c of base-level from peak
c
        call peakfit(l, m, xpos, ypos, nx, ny, peak, image)
        call basecal(nx, ny, l, m, base0, sigma, rmsbox, image,
     +               nimage, 1.5*bmajp, ok)
        if (.not.ok) goto 60
c 
c Ignore source if less than xrms times background rms
c
        peak = peak - base0
        if (peak.lt.xrms*sigma) goto 60
        mult = peak/sigma
c
c Convert binned, subimage pixels to unbinned full image pixels
c
        wa(1) = dble(xpos)
        wa(2) = dble(ypos)
        call ppconcg(2, blc(1), bin(1), wa(1))
        call ppconcg(2, blc(2), bin(2), wa(2))
c
c Optionally calculate integrated flux density for the source,
c and Gaussian major & minor axes and position angle.
c
        if (.not.nofit) then
          fitok = .true.
          call fitting(lIn, krng, sigma, nx, ny, image, rms, xpos,
     +     xposerr, ypos, yposerr, pkfl, pkflerr, intfl, amaj,
     +     amin, posa, posns, blc, bin, l, m, asciiart, bvol, bvolp,
     +     bmajp, nobeam, nimage, fitok)
          if (.not.fitok) goto 60
c
          typei(1) = 'hms'
          typei(2) = 'dms'
        else
          posns(1) = wa(1)
          posns(2) = wa(2)
          typei(1) = 'abspix'
          typei(2) = 'abspix'
        end if
        typeo(1) = 'hms'
        typeo(2) = 'dms'
c
c Convert location (peak or fitted) to formatted coordinate string
c
        call w2wfco (lin, 2, typei, ' ', posns,  typeo, ' ',
     +               .true., radec, radeclen)
c
c Write output line to screen, but only if "auto" option not selected
c
        if (nofit) then
          if (negative) then
           write(line,40) -peak*1000., xpos, ypos,
     +                   -sigma*1000., mult
          else
           write(line,40) peak*1000., xpos, ypos,
     +                   sigma*1000., mult
          end if
40        format(f8.3,2x,f7.2,2x,f7.2,2x,f7.3,2x,f5.1)
        else
          if (negative) then
           write(line,50) xposerr,yposerr,-pkfl,pkflerr,-intfl,
     +        amaj,amin,posa,-sigma*1000.,-rms*1000.
          else
           write(line,50) xposerr,yposerr,pkfl,pkflerr,intfl,
     +        amaj,amin,posa,sigma*1000.,rms*1000.
          end if
50        format(1x,f6.3,3x,f5.2,2x,f8.3,1x,f6.3,1x,f9.3,1x,
     +           3(f5.1,1x),f6.3,2x,f6.3)
        end if
        line = radec(1)(1:radeclen(1))//' '//
     +         radec(2)(1:radeclen(2))//' '//line
        if (.not.auto) call output(line)
c
c increment number of sources detected
c
        sources = sources + 1
c
c Interactive bit: move cursor to position and wait for button,
c read cursor to get yay or nay or exit from user. This bit is skipped
c entirely if "auto" is selected.
c
        if (.not.auto) then
         wsave(1) = wa(1)
         wsave(2) = wa(2)
         cch = ' '
         do while (cch.ne.'A' .and. cch.ne.'D')
          ww(1) = wsave(1)
          ww(2) = wsave(2)
          call cgcur (ww(1), ww(2), cch)
c
c Action depending upon user's button press
c
55        if (cch.eq.'A') then
            iloc = iloc + 1
            ysources = ysources + 1
c
c Mark on plot if desired
c
            if (mark) then
              call pgslw (3)
              call pgsci (2)
              call pgpt (1, wsave(1), wsave(2), 2)
              call pgupdt
              call pgslw (1)
            end if
c
            line(len1(line)+1:len1(line)+6) = '     Y'
            call txtwrite (llog, line, len1(line), iostat)
          else if (cch.eq.'D') then
            if (mark) then
              call pgslw (3)
              call pgsci (5)
              call pgpt (1, wsave(1), wsave(2), 4)
              call pgupdt
              call pgslw (1)
            end if
c
            line(len1(line)+1:len1(line)+6) = '     N'
            call txtwrite (llog, line, len1(line), iostat)
c
c When user presses quit-type command, check to see that's really what
c they wanted to do, and if so do it. If not, accept the different button
c as the intended command.
c
          else if (cch.eq.'X') then
            call output('Are you sure you want to quit here?')
            call output('(press again to confirm, or other key '//
     +                    'for corresponding action)')
            call cgcur (ww(1), ww(2), cch)
            if (cch.eq.'X') then
             goto 70
            else
             goto 55
            end if
          else
            call output ('  Commands are: A (yes), D (no), X (exit).')
          end if
         end do
        else
c
c The "auto" procedure treats every source as if the user flagged it good
c (pressed button "A").
c
         iloc = iloc + 1
         ysources = ysources + 1
         line(len1(line)+1:len1(line)+6) = '     Y'
         call txtwrite (llog, line, len1(line), iostat)
        end if
60      continue
       end do
      end do
c
70    call output (' ')
      write (line,80) sources
80    format('Total number of sources detected:',i5)
      call output(line)
      write (line,90) ysources
90    format('Number of sources confirmed:',i5)
      call output(line)
      call output(' ')
c
      call finco (lin)
c
      end
c
c
      subroutine peakfit (l, m, x, y, nx, ny, peak, image)
c-----------------------------------------------------------------------
c    Perform a bi-parabolic fit to the position l,m and return fitted
c    coordinates x,y and flux density, peak. Parabolas are fitted along
c    both the x and y axes, and the resulting position is the x,y coords
c    of the parabola's peak (always within one pixel of the brightest pixel).
c    The resulting peak flux density is simply the flux density the brightest
c    pixel would have had if it had been centered at the position x,y.
c     
c     If            z = a x^2 + b x + c
c     Then      z_max = b*b/a
c     And    x(z_max) = -b/2a
c
c  Input:
c    l,m   Bright pixel position
c    image Image matrix with pixel values
c    nx,ny dimensions of image
c  Output:
c    x,y   fitted pixel position
c    peak  fitted flux density
c-----------------------------------------------------------------------
      integer l,m,nx,ny
      real x,y
      real z,a,b,t,peak,image(nx,ny)
c-----------------------------------------------------------------------
      x = l
      y = m
      z = 0.
      t = image(l,m)
c
      b = image(l+1,m) - image(l-1,m)
      a = image(l+1,m) - 2*t + image(l-1,m)
      if ((abs(b).gt.abs(a)).or.(a.ge.0.)) goto 2010
      x = x - 0.5*b/a
      z = b*b/a
2010  continue
c
      b = image(l,m+1) - image(l,m-1)
      a = image(l,m+1) - 2*t + image(l,m-1)
      if ((abs(b).gt.abs(a)).or.(a.ge.0.)) goto 2015
c
      y = y - 0.5*b/a
      z = z + b*b/a
2015  peak = t - 0.125*z      
c
      end
c
c
      subroutine basecal (nx, ny, l, m, base0, sigma, rmsbox, image,
     +                    nimage, boxsize, ok)
c-----------------------------------------------------------------------
c    Iteratively calculate the base level around the found source using
c    all pixels falling within a square of side length rmsbox pixels
c    centred on the peak, but excluding those falling within a circle
c    with radius of boxsize.
c     
c  Input:
c    nx,ny   Size of image array
c    l,m     Bright pixel position
c    image   Image matrix with pixel values
c    nimage  Normalization image
c    rmsbox  Side length of box within which the background rms is
c            calculated
c  Output:
c    base0   The base-level
c    sigma   The background rms
c    ok      If true, was able to determine base and sigma
c-----------------------------------------------------------------------
      integer ii,jj,lmn,lmx,mmn,mmx,nx,ny,kk,l,m,rmsbox, half
      real base0, base1, base2, basen, sigma, rx, rr
      real image(nx,ny), boxsize
      integer nimage(nx,ny)
      logical ok
c-----------------------------------------------------------------------
      half= rmsbox/2
      lmn = max(1,l-half)
      lmx = min(nx,l+half)
      mmn = max(1,m-half)
      mmx = min(ny,m+half)
      ok = .true.
c
c First estimate of sigma from all pixels
c
      base0 = 0.
      base1 = 0.0
      base2 = 0.0
      basen = 0
      do jj = mmn, mmx
        do ii = lmn, lmx
          if (nimage(ii,jj).gt.0) then
            basen = basen + 1.
            base1 = base1 + image(ii,jj)
            base2 = base2 + image(ii,jj)**2
          end if
        end do
      end do
      if (basen.gt.0) then
        base0 = base1/basen
        if (base2/basen-base0**2.gt.0.0) then
          sigma = sqrt(base2/basen - base0**2)
        else
          ok =.false.
        end if
      else
        ok = .false.
      end if
      if (.not.ok) return
c
c Now iterate 3 times from this starting point
c
      do kk = 1,3
        base1 = 0.
        base2 = 0.
        basen = 0.
        do ii = lmn, lmx
          rx = (ii - l)**2
          do jj = mmn, mmx
            rr = rx + (jj-m)**2
            if (nimage(ii,jj).gt.0 .and. rr.ge.(boxsize**2) .and.
     +          abs(image(ii,jj)-base0).le.3*sigma) then
              basen = basen + 1.
              base1 = base1 + image(ii,jj)
              base2 = base2 + image(ii,jj)**2
            end if
          end do
        end do
c
c Mean and sigma
c
        base0 = base1/basen
        if (base2/basen-base0**2.gt.0.0) then
          sigma = sqrt(base2/basen - base0**2)
        else
          ok =.false.
          return
        end if
      end do
c
c If on last iteration, had less than 10 pixels,
c set base level to zero and set ropy sigma
c
      if (basen.lt.10.0) then
        base0 = 0.
        sigma = sqrt(base2/basen)
      end if
c
      end
c
c
      subroutine decopt  (do3val, do3pix, eqscale, mark, doerase, 
     +                    dowedge, dofid, grid, nofit, asciiart, auto,
     +                    negative)
c----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     do3val    True means label sub-plots with value of third axis
c     do3pix    True means label sub-plots with pixel of third axis
c     eqscale   True means plot with x and y scales
c     mark      Mark cursor location
c     doerase   Erase rectangle behind "3-axis" strings
c     dowedge   Draw wedge on pixel map
c     dofid     FIddle lookup table of pixel map
c     grid      Draw coordinate grid
c     nofit     True means don't do gaussian fitting for each source
c     asciiart  display ascii representations of each source during
c               interactive source selection
c     auto      True means skip all interactive bits, including displaying
c               image
c     negative  inverts image: positive pixels become negative and vice versa
c-----------------------------------------------------------------------
      implicit none
c
      logical do3val, do3pix, eqscale, mark, doerase,
     + dofid, dowedge, grid, nofit, asciiart, auto, negative
cc
      integer maxopt
      parameter (maxopt = 12)
c
      character opshuns(maxopt)*8
      logical present(maxopt)
      data opshuns /'3value  ', '3pixel  ', 'unequal ',
     +              'mark    ', 'noerase ', 'wedge   ',
     +              'fiddle  ', 'grid    ', 'nofit   ',
     +              'asciiart', 'auto    ', 'negative'/
c-----------------------------------------------------------------------
      call optcg ('options', opshuns, present, maxopt)
c
      do3val   =      present(1)
      do3pix   =      present(2)
      eqscale  = .not.present(3)
      mark     =      present(4)
      doerase  = .not.present(5)
      dowedge  =      present(6)
      dofid    =      present(7)
      grid     =      present(8)
      nofit    =      present(9)
      asciiart =      present(10)
      auto     =      present(11)
      negative =      present(12)
c
c circumvent possible irritating bug of ascii pictures being printed out
c in auto mode by overriding asciiart parameter.
c
      if (auto.and.asciiart) then
       asciiart = .FALSE.
       call output('Auto mode: Asciiart option being overridden.')
      end if
c
      end
c
c
      subroutine inputs (maxlev, in, ibin, jbin, kbin, levtyp, slev,
     +   levs, nlevs, pixr, trfun, pdev, labtyp, do3val, do3pix, 
     +   eqscale, nx, ny, cs, dopixel, mark, doerase, dowedge, dofid,
     +   grid, cut, rmsbox, xrms, nofit, asciiart, auto, negative)
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
c   pixr       Pixel map intensity range
c   trfun      Type of pixel map transfer function: 'log', 'lin', 
c             'heq', or 'sqr'
c   pdev       PGPLOT plot device/type
c   labtyp     Type of labels for x and y axes
c   do3val     True means label sub-plots with value of third axis
c   do3pix     True means label sub-plots with pixel of third axis
c   eqscale    True means plot with x and y scales
c   nx,ny      Number of sub-plots per page
c   cs         PGPLOT character sizes for the plot axis labels and
c              velocity/channel label,
c   dopixel    True for pixel map, false for contour plot
c   mark       Mark cursor locations
c   doerase    Erase rectangle behind "3-axis" value
c   dofid      FIddle lookup tbale of pixel map
c   dowedge    Draw wedge with pixel map
c   grid       Draw coordinate grid
c   cut        flux limit below which sources are ignored
c   rmsbox     size of box (in pixels) within which background rms is
c              calculated
c   xrms       multiple of background rms above which source must be before
c              user is given option of saying yay or nay
c   nofit      True means don't do gaussian fitting for each source
c   asciiart   display ascii representations of each source during
c              interactive source selection
c   auto       true means skip all interactive sections of program, including
c              image display
c   negative   inverts image: positive pixels become negative and vice versa
c-----------------------------------------------------------------------
      implicit none
c
      integer maxlev, nx, ny, nlevs, ibin(2), jbin(2), kbin(2), rmsbox
      real levs(maxlev), pixr(2), cs(2), slev, cut, xrms
      character*(*) labtyp(2), in, pdev, trfun, levtyp
      logical do3val, do3pix, eqscale, dopixel, mark,
     + doerase, dowedge, dofid, grid, nofit, asciiart, auto, negative
cc
      integer ntype, nlab, ntype2, nimtype
      parameter (ntype = 14, ntype2 = 3)
      character type(ntype)*6, imtype*7, type2(ntype2)*7
      data type  /'hms   ', 'dms   ', 'abspix', 'relpix', 
     +            'arcsec', 'arcmin', 'absghz', 'relghz', 
     +            'abskms', 'relkms', 'absnat', 'relnat', 
     +            'absdeg', 'reldeg'/
      data type2 /'contour', 'pixel', 'grey'/
c-----------------------------------------------------------------------
      call keyini
      call keyf ('in', in, ' ')
      if (in.eq.' ') call bug ('f', 'No image specified')
      call keymatch ('type', ntype2, type2, 1, imtype, nimtype)
      if (nimtype.eq.0) imtype = 'pixel'
      dopixel = .true.
      if (imtype.eq.'contour') dopixel = .false.
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
      if (dopixel) then
        if (trfun.ne.'lin' .and. trfun.ne.'log' .and. trfun.ne.'heq' 
     +      .and. trfun.ne.'sqr') call bug ('f', 
     +    'Unrecognized pixel map transfer function type')
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
      call decopt (do3val, do3pix, eqscale,
     +             mark, doerase, dowedge, dofid, grid,
     +             nofit, asciiart, auto, negative)
      if (.not.dopixel) then
        dofid = .false.
        dowedge = .false.
      end if
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
      call keyr('cutoff',cut,0.0)
      if (cut.lt.0.0)
     +  call bug('f','You must give a non-negative value for cutoff')
      call keyi('rmsbox',rmsbox,20)
      if (rmsbox.lt.20) then
        rmsbox = 20
        call bug('w','rmsbox must be at least 20. Setting it to 20 now')
      end if
      call keyr('xrms',xrms,0.0)
      if (xrms.le.0.0)
     +   call bug('f','Invalid value for keyword "xrms"')
c
      end
c
c       
      subroutine setlgc (labcol, poscol, statcol, regcol)
c-----------------------------------------------------------------------
c     Set line graphics colours
c
c  OUtput
c    colour indices to use
c-----------------------------------------------------------------------
      implicit none
      integer labcol, poscol, statcol, regcol
cc
      integer bgcol
c-----------------------------------------------------------------------
c
c See if black or white background
c
      call bgcolcg (bgcol)
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
        call bug ('w', 'Non black/white background colour on device')
        labcol = 7
      end if
c
c Now cursor options
c
      poscol = 3
      statcol = labcol
      regcol = 8
c
      end
c
c
      subroutine region (in, naxis, size, ibin, jbin, kbin, blc, trc,
     +                   win, ngrps, grpbeg, ngrp)
c----------------------------------------------------------------------
c     Finish key routine inputs for region of interest now.
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
c
c----------------------------------------------------------------------
      implicit none
c
      integer naxis, size(naxis), blc(*), trc(*), win(2), ngrp(*), 
     +  grpbeg(*), ngrps, ibin(2), jbin(2), kbin(2)
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
      do i = 1, min(3,naxis)
        blc(i) = max(1,blc(i))
        trc(i) = min(size(i),trc(i))
      end do
c
c Adjust spatial window to fit an integral number of bins and
c find size of binned window
c
      call winfidcg (size(1), 1, ibin, blc(1), trc(1), win(1))
      call winfidcg (size(2), 2, jbin, blc(2), trc(2), win(2))
c
c Find list of start channels and number of channels for each group
c of channels selected.
c
      call chnselcg (blc, trc, kbin, maxbox, boxes, ngrps, grpbeg, ngrp)
c
      end
c
c
      subroutine fitting (lIn, krng, sigma, nx, ny, image, rms, xpos, 
     +    xposerr, ypos, yposerr, pkfl, pkflerr, intfl, amaj,
     +    amin, posa, posns, blc, bin, lx, my, asciiart, bvol, bvolp,
     +    bmajp, nobeam, nimage, fitok)
c-----------------------------------------------------------------------
c  Do elliptical Gaussian fit to source. First the pixels to be used in
c  the fitting are selected on the basis that they are monotonically
c  decreasing away from the "central" bright pixel, then the gaussian
c  fit is done as per the task "imfit". If the size of the region within
c  which pixels were selected for the fit is smaller than 4 times the fwhm
c  of the major axis of the source, the procedure is iterated with a larger
c  region. If this procedure diverges, (detected by the increasing region
c  becoming larger than the image size), the source is ignored.
c  Once the source size and parameters are finally accepted, the background
c  rms is recalculated (more accurately now the source size is known) again,
c  and used for the output.
c
c  Once the fit is done the source parameters are recorded and passed back
c  to subroutine "search" for it to output as appropriate.
c
c  Input
c    nx,ny    Size of image
c    image    Image
c    nimage   Normalised Image (for masking info)
c    sigma    the background rms around the source. Used to clip pixels. It is
c             recalculated after the source fitting, and is an output as well.
c    nobeam   logical used to determine default beam size if not detected in
c             image header.
c    bmajp    pixel size of major axis of beam. Used for setting size of box
c             within which to detect pixels for gaussian fitting.
c    blc, bin image params used for switching between full image and binned
c             subimage pixels.
c    lx, my   integer pixel coord of brightest pixel in object. Used in
c             pixel selection routine.
c    asciiart logical. True if user wants ascii pictures of the objects
c             displayed.
c    bvol,    beam volume in world and pixel coords, resp.
c    bvolp
c
c  Output
c    posns            The object position in world coords.
c    xpos, xposerr    x-position and error of object.
c    ypos, yposerr    y-position and error of object.
c    pkfl, pkflerr    peak flux density (and error) for object.
c    intfl            integrated flux density of object.
c    amaj, amin, posa axes (arcsec) and position angle (degrees) of object.
c    rms              rms of gaussian fit to selected pixels.
c    fitok            logical. False if the fit failed for any reason.
c
c-----------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer MAXVAR
      parameter(MAXVAR=20)
c
      logical dofit, asciiart, nobeam, fitok, ok
      integer ifail1,ifail2,lIn, i, blc(2), bin(2), maxline, boxsz4
      integer k,m,nvar,lx,my, nx,ny, krng(2), nimage(nx,ny), fiterr
      real image(nx,ny), dumm
      real clip,xvar(MAXVAR),covar(MAXVAR*MAXVAR),rms
      real bvol,bvolp, xpos, ypos, pkfl, intfl
      real sigma, amaj, amin, posa, bmajp, boxsize
      real xposerr, yposerr, pkflerr
      double precision posns(2)
      character ascpic(1000)*80
c
c  Externals.
c
      external FUNCTION
c-----------------------------------------------------------------------
c
c Initialise parameters
c
      dumm = 0.
      nsrc = 1
      do i = 1,nsrc
        vflux(i) = .true.
        vl0(i) = .true.
        vm0(i) = .true.
        vfwhm1(i) = .true.
        vfwhm2(i) = .true.
        vpa(i) = .true.
        fwhm1(i) = 1.
        fwhm2(i) = 1.
        l0(i) = 0.
        m0(i) = 0.
      end do
      clip = sigma
      fitok = .true.
c
c Set the plane appropriate to the displayed image.  Since
c this may be a range of planes averaged together, take the 
c integer nearest the average plane
c
      k = nint(real(krng(1) + krng(1) + krng(2) - 1)/2.0)
c
c  Set the size of the region within which to select pixels to be used in
c  the gaussian fitting procedure.
c
      if (nobeam) then
       boxsize = 5.
      else
       boxsize = bmajp
      end if
c
c  Load the data. Ie, choose the pixels in the region of the source to
c  be used in the fitting procedure, and encode them in a format ready
c  to do so.
c
1200  call LoadDat (clip,m,nx,ny,xpos,ypos,blc,bin,
     +  image,boxsize,lx,my,nimage,asciiart,ascpic,maxline,fitok)
      if (.not.fitok) return
      if (m.eq.0) then
       fitok = .false.
       return
      end if
c
c  Convert the coordinates to pixels, and fill in defaults if necessary.
c
      call CoordFid(lIn,k,.true.)
      call GetEst
c
c  Pack the variables into an array.
c
      call PackVar(xvar,nvar,MAXVAR)
      dofit = nvar.gt.0
      if(.not.dofit) then
        fitok = .false.
        return
      end if
      if(nvar.ge.m) then
        fitok = .false.
        return
      end if
c
c  Do the fitting process.
c
      fiterr = 0
      call lsqfit(FUNCTION,m,nvar,xvar,covar,rms,ifail1,ifail2)
      call UPackVar(xvar,nvar)
      if(ifail2.eq.0)call UpackCov(covar,nvar)
      if(ifail1.ne.0) fiterr = 1
      if(ifail2.ne.ifail1) fiterr = 2
c
c  Check to see whether the fitted size of the image is comparable to the
c  initial box size used in selecting pixels, and if so iterate the
c  procedure choosing a larger box size.
c
      if (boxsize.lt.max(abs(fwhm1(1)),abs(fwhm2(1)))) then
c
c Checks for divergence - eg, noise being fitted by ever larger
c gaussians. Stops and aborts fit if object fwhm is larger than one quarter the
c smallest image dimension, or if the fitted fwhm has increased to more than
c 60 pixels. (At sizes > 60 pixels, the next iteration takes quite a while,
c and leaves a big wait for nothing if the object is then discarded on the
c next iteration.) This (moderately) arbitrary number is, however, an
c unsatisfactory way of dealing with the problem, and a better one would
c be appreciated.
c
       if (max(abs(fwhm1(1)),abs(fwhm2(1))).lt.min(nx,ny,240)/4) then
        boxsize = max(abs(fwhm1(1)),abs(fwhm2(1)))
        goto 1200
       else
        fitok = .false.
        return
       end if
      end if
c
c use basecal to calculate updated value for the background rms, using
c newly found size of object to determine central region to exclude and
c overall region to include (boxsize and 4*boxsize, respectively)
c (Note that here, we are no longer interested in base0 and it has been
c replaced by a dummy var. "dumm".)
c
      boxsz4 = 4*boxsize
      call basecal(nx,ny,lx,my,dumm,sigma,boxsz4,image,nimage,
     +             boxsize,ok)
c
c if the new value was unable to be calculated - too few pixels, etc,
c put back its original value, which has been stored as the variable clip.
c
      if (.not.ok) sigma = clip
c
c print out ascii art if required
c
      if (asciiart) then
       do i = maxline,1,-1
        if ((ascpic(i).ne.' ').or.(i.eq.1).or.(i.eq.maxline))
     +      call output(ascpic(i))
       end do
      end if
c
c Print out warnings if there were problems with the fits
c
      if (fiterr.gt.0) then
       if (fiterr.eq.1) then
        call bug('w','Fitting failed to converge.')
       else
        call bug('w','Failed to determine covariance matrix.')
       end if
       call bug('w','Source may still be real, but parameters may'
     +          //' be incorrect.')
      end if
c
c  Convert to astronomical units, and report - ie, finalise results.
c
      call CoordFid(lIn,k,.false.)
      call Report (bvol, bvolp, xposerr, yposerr, 
     +   pkfl, pkflerr, intfl, amaj, amin, posa, posns, lIn)
c
      end
c
c
      subroutine CoordFid(lIn,k,topix)
c------------------------------------------------------------------------
c
c  Convert coordinates between world and pixel coordinates.
c
c  Input:
c    lIn      Handle of the coordinate system.
c    k
c    topix
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer lIn,k
      logical topix
      double precision in(3),out(3)
      double precision crpix(2),crval(2),cdelt(2)
      character ctype(2)*16
      real bmaj,bmin,bpa,dx,dy
      integer i
c------------------------------------------------------------------------
      do i=1,nsrc
c
c  Convert the position.
c
          in(1) = l0(i)
          in(2) = m0(i)
          in(3) = k
          if(topix)then
            call coCvt(lIn,'ow/ow/ap',in,'ap/ap/ap',out)
          else
            call coCvt(lIn,'ap/ap/ap',in,'ow/ow/ap',out)
          endif
          l0(i) = out(1)
          m0(i) = out(2)
c
c  Convert the gaussian parameters.
c
          if(topix)then
            call coGauCvt(lIn,'ow/ow/ap',in,
     +              'w',fwhm1(i),fwhm2(i),pa(i),'p',bmaj,bmin,bpa)
          else
            call coGauCvt(lIn,'ap/ap/ap',in,
     +              'p',fwhm1(i),fwhm2(i),pa(i),'w',bmaj,bmin,bpa)
          endif
c
c  Convert the uncertainties.
c
          sfwhm1(i) = bmaj / fwhm1(i) * sfwhm1(i)
          sfwhm2(i) = bmin / fwhm2(i) * sfwhm2(i)
          if(spa(i)+sl0(i)+sm0(i).gt.0)then
            if(topix)then
            call coLin(lIn,'ow/ow/ap',in,2,ctype,crpix,crval,cdelt)
            dx = 1/abs(cdelt(1))
            dy = 1/abs(cdelt(2))
            else
            call coLin(lIn,'ap/ap/ap',in,2,ctype,crpix,crval,cdelt)
            dx = abs(cdelt(1))
            dy = abs(cdelt(2))
            endif
            sl0(i) = sl0(i) * dx
            sm0(i) = sm0(i) * dy
            spa(i) = spa(i) / ( dy/dx*cos(pa(i))**2 + 
     +                          dx/dy*sin(pa(i))**2 )
          endif
c
          fwhm1(i) = bmaj
          fwhm2(i) = bmin
          pa(i)    = bpa
      enddo
c
      end
c
c
      subroutine GetEst
c------------------------------------------------------------------------
c  Generate an initial estimate for a single component model.
c
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      include 'mirconst.h'
c
      integer i
      double precision P,XP,YP,XYP,XXP,YYP,SP
      real t,fac
c------------------------------------------------------------------------
      SP = 0
      P = 0
      XP = 0
      YP = 0
      XYP = 0
      XXP = 0
      YYP = 0
c
      do i=1,ndata
        SP  = SP + data(i)
        t = abs(data(i))
        P   = P   + t
        XP  = XP  + t*x(i)
        YP  = YP  + t*y(i)
        XYP = XYP + t*x(i)*y(i)
        XXP = XXP + t*x(i)*x(i)
        YYP = YYP + t*y(i)*y(i)
      enddo
c
      fac = 4*log(2.)
      XP  = XP / P
      YP  = YP / P
      XYP = XYP / P - XP*YP
      XXP = XXP / P - XP*XP
      YYP = YYP / P - YP*YP
      l0(1) = XP
      m0(1) = YP
      fwhm1(1) = sqrt(fac*(XXP + YYP +
     +            sqrt( (XXP-YYP)**2 + 4*(XYP)**2 )))
      fwhm2(1) = sqrt(fac*(XXP + YYP -
     +            sqrt( (XXP-YYP)**2 + 4*(XYP)**2 )))
      pa(1) = 0.5*atan2(2*XYP,YYP-XXP)
      flux(1) = sign(fac * P / ( pi * fwhm1(1) * fwhm2(1) ),SP)
c
      end
c
c
      subroutine LoadDat(clip,m,nx,ny,xpos,ypos,blc,bin,image,boxsize,
     +                     lx,my,nimage,asciiart,ascpic,maxline,fitok)
c-----------------------------------------------------------------------
c  Load the relevant data for this plane. The relevant data are those pixels
c  to which to fit the elliptical gaussians, and they are selected by ...
c
c  Input:
c    clip      Clip level.
c  Output:
c    m            Number of points loaded.
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer m,nx,ny,lmn,lmx,mmn,mmx,nimage(nx,ny)
      integer i,xt,yt,ll,mm, blc(2),bin(2), lx,my, maxline
      real clip,xpos,ypos,image(nx,ny),boxsize
      double precision wa(2)
      logical incirc,fainter,asciiart,fitok
      character ascpic(1000)*80
c-----------------------------------------------------------------------
c
c calculate extremes of box around object, with size 4*bmaj (major axis of
c beam) in pixels (or larger if a later iteration).
c
      lmn = max(1,nint(xpos-2*boxsize))
      lmx = min(nx,nint(xpos+2*boxsize))
      mmn = max(1,nint(ypos-2*boxsize))
      mmx = min(ny,nint(ypos+2*boxsize))
c
c set number of lines to be used in ascii pic including blank
c line at start and end
c
      maxline = mmx - mmn + 3
      if (maxline.gt.1000) then
        call bug('w','Big object skipped - more than 1000 pixels'
     +           //'in one dimension')
        fitok = .false.
        return
      end if
      do i = 1,maxline
       ascpic(i) = ' '
      end do
c
c Choose only those pixels within a *circle* of radius 2*boxsize which are
c also fainter than all the pixels between themselves and the brightest
c pixel and which aren't blanked.
c Store pixel values of object and surrounding region in linear array, data,
c and fill out the x,y, coordinate.
c
      i = 0
      xt = 0
      yt = 0
      do mm = mmn,mmx
       do ll = lmx,lmn,-1
        incirc = ((ll-xpos)**2 + (mm-ypos)**2).lt.(4*boxsize**2)
        call slopey(ll,mm,lx,my,nx,ny,image,fainter)
        if ((image(ll,mm).gt.clip).and.incirc.and.fainter.and.
     +      (nimage(ll,mm).gt.0)) then
         if ((ll.eq.lx).and.(mm.eq.my)) then
          ascpic(mm-mmn+2) = 'O'//ascpic(mm-mmn+2)
         else
          ascpic(mm-mmn+2) = '*'//ascpic(mm-mmn+2)
         end if
         i = i + 1
         data(i) = image(ll,mm)
c
c convert ll,mm from binned subimage pixels to full image pixels
c
         wa(1) = dble(ll)
         wa(2) = dble(mm)
         call ppconcg(2, blc(1), bin(1), wa(1))
         call ppconcg(2, blc(2), bin(2), wa(2))
         x(i) = real(wa(1))
         y(i) = real(wa(2))
         xt = xt + x(i)
         yt = yt + y(i)
        else
         ascpic(mm-mmn+2) = ' '//ascpic(mm-mmn+2)
        end if
       end do
      end do
      ndata = i
      m = ndata
      xoff = xt / real(ndata)
      yoff = yt / real(ndata)
c
c convert xoff,yoff from binned subimage pixels to full image pixels
c
      wa(1) = dble(xoff)
      wa(2) = dble(yoff)
      call ppconcg(2, blc(1), bin(1), wa(1))
      call ppconcg(2, blc(2), bin(2), wa(2))
      xoff = real(wa(1))
      yoff = real(wa(2))
c
      end
c
c
      subroutine slopey(xx,yy,xc,yc,nx,ny,image,slpdwn)
c-----------------------------------------------------------------------------
c this subroutine checks that a given pixel is less than all the pixels
c between it and the central (brightest) pixel.
c
c-----------------------------------------------------------------------------
      integer xx,yy,xc,yc,nx,ny,closerx,closery
      integer furtherx,furthery,pp,qq,stepl,stepm
      real image(nx,ny)
      logical slpdwn
c-----------------------------------------------------------------------------
      if (xx.gt.xc) then
       closerx = xx-1
       furtherx = xx+1
       stepl = -1
      end if
      if (xx.lt.xc) then
       closerx = xx+1
       furtherx = xx-1
       stepl = 1
      end if
      if (xx.eq.xc) then
       closerx = xx
       furtherx = xx
       stepl = 0
      end if
      if (yy.gt.yc) then
       closery = yy-1
       furthery = yy+1
       stepm = -1
      end if
      if (yy.lt.yc) then
       closery = yy+1
       furthery = yy-1
       stepm = 1
      end if
      if (yy.eq.yc) then
       closery = yy
       furthery = yy
       stepm = 0
      end if
c
c Checking to see if the closer/further pixels lie outside the bounds of the
c image, and fixing them if so.
c
      if (closerx.gt.nx)  closerx = nx
      if (closery.gt.ny)  closery = ny
      if (closerx.lt.1)   closerx = 1
      if (closery.lt.1)   closery = 1
      if (furtherx.gt.nx) furtherx = nx
      if (furthery.gt.ny) furthery = ny
      if (furtherx.lt.1)  furtherx = 1
      if (furthery.lt.1)  furthery = 1
      slpdwn = (image(xx,yy).le.image(closerx,closery)).and.
     +          (image(xx,yy).ge.image(furtherx,furthery))
      if ((yy.ne.yc).and.(xx.ne.xc).and.slpdwn) then
       do qq = closery,yc,stepm
        do pp = closerx,xc,stepl
         slpdwn = slpdwn.and.(image(xx,yy).le.image(pp,qq))
        end do
       end do
      end if
c
      end
c
c
      subroutine PackVar(var,nvar,MAXVAR)
c-----------------------------------------------------------------------
c
c  Store all the things that we need to vary.
c
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer nvar,MAXVAR
      real var(MAXVAR)
      integer i,j,ncurr
      real tmp(6)
c-----------------------------------------------------------------------
      nvar = 0
      do i=1,nsrc
        ncurr = 0
        if(vflux(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = flux(i)
        endif
        if(vl0(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = l0(i) - xoff
        endif
        if(vm0(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = m0(i) - yoff
        endif
        if(vfwhm1(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = fwhm1(i)
        endif
        if(vfwhm2(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = fwhm2(i)
        endif
        if(vpa(i))then
          ncurr = ncurr + 1
          tmp(ncurr) = pa(i)
        endif
c
c  Copy the estimates to the variables.
c
        if(nvar+ncurr.gt.MAXVAR)
     +        call bug('f','Too many free parameters')
        do j=1,ncurr
          nvar = nvar + 1
          var(nvar) = tmp(j)
        enddo
c
      enddo
c
      end
c
c
      subroutine UpackCov(covar,nvar)
c------------------------------------------------------------------------
c
c  Unpack the covariance matrix.
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer nvar
      real covar(nvar,nvar)
      integer i,n
c------------------------------------------------------------------------
      n = 0
      do i=1,nsrc
        if(vflux(i))then
          n = n + 1
          sflux(i) = covar(n,n)
        endif
        if(vl0(i))then
          n = n + 1
          sl0(i) = covar(n,n)
        endif
        if(vm0(i))then
          n = n + 1
          sm0(i) = covar(n,n)
        endif
        if(vfwhm1(i))then
          n = n + 1
          sfwhm1(i) = covar(n,n)
        endif
        if(vfwhm2(i))then
          n = n + 1
          sfwhm2(i) = covar(n,n)
        endif
        if(vpa(i))then
          n = n + 1
          spa(i) = covar(n,n)
        endif
      enddo
c
      if(n.ne.nvar)
     +        call bug('f','Inconsistency in UPackCov')
c
      end
c
c
      subroutine UPackVar(var,nvar)
c------------------------------------------------------------------------
c
c  Unpack all the things that we need to vary.
c
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer nvar
      real var(nvar)
      integer i,n
c------------------------------------------------------------------------
      n = 0
      do i=1,nsrc
        if(vflux(i))then
          n = n + 1
          flux(i) = var(n)
        endif
        if(vl0(i))then
          n = n + 1
          l0(i) = var(n) + xoff
        endif
        if(vm0(i))then
          n = n + 1
          m0(i) = var(n) + yoff
        endif
        if(vfwhm1(i))then
          n = n + 1
          fwhm1(i) = var(n)
        endif
        if(vfwhm2(i))then
          n = n + 1
          fwhm2(i) = var(n)
        endif
        if(vpa(i))then
          n = n + 1
          pa(i) = var(n)
        endif
      enddo
c
      if(n.ne.nvar)
     +        call bug('f','Inconsistency in UPackVar')
c
      end
c
c
      subroutine FUNCTION(m,nvar,var,fvec,iflag)
c------------------------------------------------------------------------
c
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer m,nvar,iflag
      real var(nvar)
      real fvec(m)
      integer i
c------------------------------------------------------------------------
      if(m.ne.ndata)call bug('f','Inconsistency in FUNCTION')
c
c  Unpack the things that we are solving for.
c
      call UpackVar(var,nvar)
c
c  Evaluate the model.
c
      call Eval(x,y,fvec,m)
c
c  Compute the residuals now.
c
      do i=1,m
        fvec(i) = Data(i) - fvec(i)
      enddo
c
      end      
c
c
      subroutine Eval(x0,y0,Model,n)
c-------------------------------------------------------------------------
c
c  Evaluate the current model at some pixels.
c
c  Input:
c    n            Number of points.
c    x0,y0      Pixel coordinates at which to evaluate the model.
c  Output:
c    model      The evaluated model.
c------------------------------------------------------------------------
      implicit none
      include 'sfind.h'
      integer n,x0(n),y0(n)
      real Model(n)
      integer i,j
      real cospa,sinpa,t,xx,yy,xp,yp,xscal,yscal
c-------------------------------------------------------------------------
c  Set the model to zero initially.
c
      do i=1,n
        model(i) = 0
      enddo
c
c  Loop over the various model types.
c
      do j=1,nsrc
c
c  Gaussian component.
c
          cospa = cos(pa(j))
          sinpa = sin(pa(j))
          xscal = 4*log(2.)/(fwhm2(j)*fwhm2(j))
          yscal = 4*log(2.)/(fwhm1(j)*fwhm1(j))
          do i=1,n
            xx = x0(i) - l0(j)
            yy = y0(i) - m0(j)
            yp =  yy*cospa + xx*sinpa
            xp = -yy*sinpa + xx*cospa
            t = xscal*(xp*xp) + yscal*(yp*yp)
            if(t.lt.70)model(i) = model(i) + flux(j) * exp(-t)
          enddo
      enddo
c
      end
c
c
      subroutine Report(bvol,bvolp,xposerr,yposerr,pkfl,pkflerr,
     +                    intfl,amaj,amin,posa,posns,lIn)
c-----------------------------------------------------------------------------
c  Report on the source component solution.
c
c  Input:
c    lIn            handle of image
c  Output:
c    posns          x and y positions in abs. world coords (radians)
c    xposerr        xposition error.
c    yposerr        yposition error.
c    pkfl, pkflerr  peak flux density of source, and its error.
c    intfl          integrated flux density of the source
c    amaj, amin     major and minor fwhm of source (arcsec)
c    posa           position angle of source (degrees E of N)
c------------------------------------------------------------------------
      implicit none
      include 'mirconst.h'
      include 'sfind.h'
c
      integer lin
      real bvol,bvolp
      real f1,f2,p,sf1,sf2,sp,tflux,sfac
        real xposerr, yposerr, pkfl, pkflerr, intfl
        real amaj, amin, posa
        double precision posns(2),newpos(2)
c-----------------------------------------------------------------------------
      if(bvolp.gt.0)then
        sfac = sqrt(bvolp)
      else
        sfac = 1
      endif
c
        pkfl = 1000.*flux(1)
        pkflerr = 1000.*sfac*sflux(1)
      if(bvol.gt.0.)then
        tflux = flux(1) * pi/4 * abs(fwhm1(1)) * abs(fwhm2(1))
        tflux = tflux / log(2.0)
        tflux = tflux / bvol
          intfl = 1000.*tflux
        else
          intfl = 0.
      endif
      posns(1) = l0(1)
        posns(2) = m0(1)
        call coCvt(lIn,'ow/ow',posns,'aw/aw',newpos)
        posns(1) = newpos(1)
        posns(2) = newpos(2)
        xposerr =3600*180/pi*sfac*sl0(1)
        yposerr =3600*180/pi*sfac*sm0(1)
      call GauFid(fwhm1(1),fwhm2(1),sfac*sfwhm1(1),
     +            sfac*sfwhm2(1),pa(1),sfac*spa(1),f1,f2,sf1,sf2,p,sp)
        amaj = f1
        amin = f2
        posa = p
c
      end
c
c
      subroutine GauFid(fwhm1,fwhm2,sfwhm1,sfwhm2,pa,spa,f1,f2,
     +                                          sf1,sf2,p,sp)
c-----------------------------------------------------------------------------
c
c  Convert the gaussian parameters to arcsec.
c------------------------------------------------------------------------
      implicit none
      include 'mirconst.h'
      real fwhm1,fwhm2,pa,f1,f2,p,sfwhm1,sfwhm2,spa,sf1,sf2,sp
      real t
c----------------------------------------------------------------------------
      f1 = 3600*180/pi * abs(fwhm1)
      f2 = 3600*180/pi * abs(fwhm2)
      sf1 = 3600*180/pi * sfwhm1
      sf2 = 3600*180/pi * sfwhm2
      p = 180./pi * pa
      sp = 180./pi * spa
      if(f1.lt.f2)then
        t = f1
        f1 = f2
        f2 = t
        t = sf1
        sf1 = sf2
        sf2 = t
        p = p + 90
      endif
      p = mod(p,180.)
      if(p.lt.-90) p = p + 180
      if(p.gt. 90) p = p - 180
      end
c
c
      subroutine BeamPar(lIn,k,bvol,bvolp,bmaj,bmin,bpa,bmajp,nobeam)
c-----------------------------------------------------------------------
c  Get things dealing with units.
c
c  Input:
c    lIn      Handle of the input dataset.
c    k            Plane of interest.
c  Output:
c    bvol      Beam volume, in radians**2. Set to zero if this cannot
c            be determined.
c    bvolp      Beam volume in pixels.
c    bmaj,bmin,bpa Beam major, minor axes and position angle.
c------------------------------------------------------------------------
      implicit none
      integer lIn,k
      real bvol,bvolp,bmaj,bmin,bpa
cc
      include 'mirconst.h'
      character bunit*16,ctype(2)*16
      real bmajp,bminp,bpap
      double precision crpix(2),crval(2),cdelt(2),x(3)
      logical nobeam
c-----------------------------------------------------------------------
      nobeam = .false.
c
c  Convert the beam to radians for the current plane.
c
      call rdhdr(lIn,'bmaj',bmaj,0.)
      call rdhdr(lIn,'bmin',bmin,0.)
      call rdhdr(lIn,'bpa',bpa,0.)
      bpa = pi/180 * bpa
        if ((bmaj.eq.0.).or.(bmin.eq.0.)) then
         call bug('w','Beam not detected in map -')
         call bug('w','Using arbitrary value of 5 pixels.')
         nobeam = .true.
         bvol = 0.
         bvolp = 0.
         goto 1111
        end if
c
      if(bmaj*bmin.gt.0)then
        x(1) = 0
        x(2) = 0
        x(3) = 0
        call coGauCvt(lIn,'op/op/op',x,'w',bmaj,bmin,bpa,
     +                               'p',bmajp,bminp,bpap)
        x(3) = k
        call coGauCvt(lIn,'op/op/ap',x,'p',bmajp,bminp,bpap,
     +                               'w',bmaj,bmin,bpa)
      endif
c
c  Determine the beam value, in radians**2
c
      call rdhda(lIn,'bunit',bunit,' ')
      call ucase(bunit)
      if(index(bunit,'/PIXEL').ne.0)then
        x(1) = 0
        x(2) = 0
        x(3) = k
        call coLin(lIn,'op/op/ap',x,2,ctype,crpix,crval,cdelt)
        bvol = abs(cdelt(1)*cdelt(2))
        bvolp = 1
      else if(index(bunit,'/BEAM').ne.0.and.bmaj*bmin.gt.0)then
        bvol = pi/4/log(2.0)*bmaj*bmin
        bvolp = pi/4/log(2.0)*bmajp*bminp
      else
        bvol = 0
        bvolp = 0
      endif
c
1111    continue
      end

