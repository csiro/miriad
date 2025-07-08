c***********************************************************************
c       velplot.h - include file for velplot program.
c
c $Id: velplot.h,v 1.2 2010/10/22 03:45:47 cal103 Exp $
c-----------------------------------------------------------------------
c  Maximum array dimensions and size.
c  MAXDIM is used to read the image and for 1-dimensional arrays.
      include 'maxdim.h'

c  box in absolute pixels. (is,ib) (ie,it) can be reset by cursor.
      integer      is,ie,ib,it,midx,midy
      common /box/ is,ie,ib,it,midx,midy
c-----------------------------------------------------------------------

      integer   bblc(2), blc(3), brpix(2), btrc(2), niters, trc(3)
      real      amax, amin, arms, bmaj, bmin, bpa, cbof, crval(2),
     *          delv, dperjy, epoch, posend, pospa, posx, posy,
     *          restfreq, vel, velend, xy

      common /vlhead/ amax, amin, arms, bblc, blc, btrc, brpix, bmaj,
     *          bmin, bpa, cbof, crval, delv, dperjy, epoch, niters,
     *          posend, pospa, posx, posy, restfreq, trc, vel, velend,
     *          xy

c  amin, amax, arms - min, max, rms for current plot.
c  bmaj, bmin, bpa (beam) [radians]
c  blc, trc - region of interest in in absolute pixels.
c  bblc, ttrc - box in absolute pixels, can be reset by cursor.
c  brpix - reference pixel in box coordinates.
c  crval1, crval2
c  epoch
c  niters - clean iterations.
c  posx, posy [arcsec] pospa [deg] (position wrt center and pa of l-v
c    plot)
c  posend - Length in arcsecs of cut in l-v plot
c  restfreq, [GHz]
c  vel, delv - lsr velocity and width of current map [km/s]
c  velend - Last velocity in l-v plot.  (First is in vel)
c  xy (map pixel),  [arcsecs]
c-----------------------------------------------------------------------

      integer   conlabel, nlevels
      real      bg, cutoff, fg, levels(10), src
      character abscoord, alabel, apint, bunit*9, cneg, ctype(3)*9,
     *          defgray, device*80, file*80, gray, lgaufit,
     *          lgauplot, maptype*9, object*9, percent, pspec, units,
     *          write

      common /vlargs/ src,levels,nlevels,conlabel,fg,bg,cutoff
      common /vlchar/ abscoord, alabel, apint, bunit, cneg, ctype,
     *          defgray, device, file, gray, lgaufit, lgauplot,
     *          maptype, object, percent, pspec, units, write

c  abscoord     Absolute coordinate labels.     [Y or N]
c  alabel       Plot header                     [Y or N]
c  apint        Integer Plot                    [Y or N]
c  bunit        units from image.
c  cneg         Negative contours               [Y or N]
c  conlabel     Label interval for contours
c  ctype        types of coordinate axes. (same as FITS keywords).
c  cutoff       Cutoff level in moment maps.
c  file         filename of image.
c  levels       Contour levels
c  lgaufit      Gaussian fit to spectra         [Y or N]
c  maptype      maptype                 [X-Y, POS-VEL or SPECTRA]
c  nlevels      Number of levels
c  object       source name from image.
c  percent      Percentage contour levels       [Y or N]
c  src          Plot device (0=screen 1=lp 2=vers 3=imagen)
c  units        units for displayed values.     [J or K]
c  write        Write out map to a file         [Y or N]
c-----------------------------------------------------------------------

