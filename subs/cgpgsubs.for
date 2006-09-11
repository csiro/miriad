c**********************************************************************
c     A collection of subroutines shared by the programs CGDISP,
c     CGSPEC, CGCURS, and CGSLICE. All these subroutines call PGPLOT. 
c
c  annboxcg :  Annotate information from one box image
c  annconcg :  Annotate information from one contour image
c  anndefcg :  Define size of character for annotation
c  anngrscg :  Annotate informatiobn from one grey scale image
c  anninicg :  Initialize plot annotation and write reference value
c  annspccg :  Annotate information from all spectrum images
c  annveccg :  Annotate information from one pair of vector images
c  annwincg :  Annotate plot with window and channel info 
c  axlabcg  :  Label axes
c  boxcg    :  Draw axes with optional numeric labels
c  confmtcg :  Format contour levels
c  conturcg :  Draw contour plot
c  erswincg :  Erase window
c  lab3cg   :  Label sub-plot with value and/or pixel of third axis
c  setlabcg :  Set axis label displacements 
c  strerscg :  Erase rectangle on plot and write string into it
c  strfmtcg :  Format a number into a string with PGNUMB
c  vpadjcg  :  Adjust viewport if equal scales requested
c  vpsizcg  :  Set encompassing viewport and subplot increments
c  wedgecg  :  High level routine to draw wedges (calls WEDGCG)
c  wedgcg   :  Draw grey scale wedge in specified viewport
c  yhtwcg   :  Find y-height of one character in world coordinates
c
c  History:
c     nebk   20sep91    Created from PGDISP/PGCURS
c     nebk   08nov91    Use local blc,trc in call to BOXRUNS in
c                       CHNSELPG, because they may get modified
c                       if blanked pixels exist
c     nebk   28apr92    "g" format seems to behave capriciously
c                        Try to do something better in VCLABPG
c                        Renamed subroutines to *cg from *pg 
c                        as pgdisp etc -> cgdisp etc
c     nebk   12may92     Return actual scales in VPADJCG
c     nebk   14may92     Add  LIMTRCG. Add a couple more
c                        parameters to HEDINFCG
c     nebk   18may92     Add AXFNDCG
c     nebk   04jul92     Don't modify variable (PLAV) in READIMCG. Add
c                        OTOPIXCG, SETTRCG, CONLINCG, STRERSCG, DEGHSMCG,
c			 ANN*CG, CHKDESCG, CHKDIMCG,  add argument 
c			 MIRROR to CONLEVCG 
c     nebk   08jul82     Add OPTCG and INIT/NORM to READIMCG call. FIx 
c                        bug in CHNSELCG causing groups to be redundantly
c                        specified under some circumstances
c     nebk   14jul92     Add POSOFF to OTOPIXCG. Type CDELT and CRVAL
c			 as DOUBLE PRECISION
c     nebk   07aug92     Try to instill some more modularity into all 
c                        coordinate conversions with PIX2WCG and
c                        W2PIXCG, removing SETTRCG along the way.
c     nebk   22oct92     Add units to velocity and frequency axes 
c			 in LIMTRCG.  SETLABCG was not correctly 
c                        setting the dms,hms PGTBOX strings.
c     nebk   28nov92     Add 'abs/relkms' and 'abs/relghz' label types
c                        Change "linear" to "abslin"
c     nebk   02dec92     Add pix2wfcg, sunitcg
c     nebk   24dec92     SPlit off from cgsubs.for
c     nebk   27feb93     Appease Mr T and reformat call sequence
c                        variable code
c     mjs    15mar93     pgplot names have 6 or less chars.
c     nebk   21apr93     vclabcg -> lab3cg and make it generic so that
c                        whatever is on the third axis gets labelled
c     nebk   27may93     Add YHTQCG and use it in a few places
c     nebk   29may93     Replace CHTONVCG by new PGQCS
c     nebk   02jun93     Replace VSSIZECG by new PGQVSZ
c     nebk   02jun93     Move ANNDEFCG, VPADJCG, VPASPCG here from
c			 CGSUBS.FOR as they now call PGPLOT
c     nebk   16jun93     Remove VPASPCG.  Its functions can now be done
c                        with PGQCS
c     nebk   23jun93     Rework VPADJCG because rellin/abslin for RA 
c   			 axes now gives radians of polar rotation
c                        Correctly deal with user given scales
c     nebk   25aug93     Add "absdeg" and "reldeg" axis types.
c			 Scrap DEGHMSCG in favour of new DANGLEH
c                        Add DOERASE to STRERSCG and LAB3CG
c     nebk   13nov93     Minor change to AXLABCG with options strings
c                        Better units for Amax in ANNVECCG
c     nebk   14dec93     Variety of formatting changes in ANN* routines
c     nebk   09jan94     Convert CRPIX to double precision
c     nebk   17jan94     Add fiddle viewport and grey wedge to VPADJCG
c     nebk   27jan94     Add WEDGCG, ERSWINCG and BOXCG
c     nebk   02mar94     Justification->1.0 in PGMTXT call in AXLABCG
c                        Imported SETLABCG from cgsubs.for as it now
c                        calls PGQCS to be cleverer about displacements
c     nebk   08mar94     Move WEDGECG here from CGSUBS.FOR. Variables
c                        PLINC&PLAV -> KBIN(1:2)
c     nebk   17mar94     Get rid of horrid hoop jumping in STRERSCG
c			 with new PGSTBG routine
c     nebk   15jun94     Include spatial binning info in ANNWINCG
c                        CHange LAB3CG positioning algorithm slightly
c                        Add justification to STRERSCG
c     nebk   28jun94     Add ANNBOXCG, STRFMTCG, CONFMTCG and remove
c                        CONLINCG.  Add box type images to VPSIZCG
c     nebk   12jul94     Mess about with scale algorithm in VPADJCG
c     nebk   02aug94     LAB3CG was getting value wrong when channels
c                        averaged together
c**********************************************************************
c
c* annboxCG -- Annotate plot with information from a box image 
c& nebk
c: plotting
c+
      subroutine annboxcg (lb, bin, bfac, yinc, xpos, ypos)
c
      implicit none
      integer lb
      real bfac(5), yinc, xpos, ypos
      character*(*) bin
c
c  Annotate plot with box image information
c
c  Input:
c    lb      Handle for box images
c    bin     Box image
c    bfac    (1)   Maximum value if pixel in region of first subplot
c            (2-3) Scale factors, in x and y, to convert pixel value
c                  into box width in world coordinates
c            (4-5) Scale factors, in x and y, giving box widths per mm 
c                  E.g. if pixel is 50 rad/m/m, then these scale factors
c                  say you have bfac(4) and bfac(5) rad/m/m per mm
c    yinc    World increment between text lines
c    xpos    World x coordinate for text lines
c  Input/output
c    ypos    World y coordinate for next text line
c--
c-----------------------------------------------------------------------
      character src*50, str1*132, str2*132, str3*132, str4*132,
     +  units*20, btype*30
      integer len1, iu, i1, i2, i3, i4
c-----------------------------------------------------------------------
      call rdhda (lb, 'object', src, ' ')
      if (src.ne.' ') then
         str2 = ' ('//src(1:len1(src))//')'
         i2 = len1(str2)
      else 
         str2 = ' '
         i2 = 1
      end if
      str1 = ' Box image: '//bin(1:len1(bin))//str2(1:i2)
      i1 = len1(str1)
c
      call rdhda (lb, 'bunit', units, ' ')
      call rdbtype (lb, btype, ' ')
      if (units.eq.' ') then
        if (btype.eq.'fractional_polarization') then
          units = 'ratio'
        else if (btype.eq.'depolarization_ratio') then
          units = 'ratio'
        else if (btype.eq.'polarized_intensity') then
          units = 'Jy/beam'
        else if (btype.eq.'rotation_measure') then
          units = 'rad m\u-2\d'
        end if
      end if
      iu = max(1,len1(units))
c
c Format scale factors and peak value
c
      call strfmtcg (bfac(4), 4, str2, i2)
      call strfmtcg (bfac(5), 4, str3, i3)
      call strfmtcg (bfac(1), 4, str4, i4)
c
      str1(i1+1:) = ' |B\dmax\u|='//str4(1:i4)//' '//units(1:iu)//
     +              ', x,y-scale='//str2(1:i2)//','//str3(1:i3)//' '//
     +               units(1:iu)//'/mm'
      i1 = len1(str1)
c
      call pgtext (xpos, ypos, str1(1:i1))
      ypos = ypos - yinc
c
      end
c
c* annconCG -- Annotate plot with information from a contour image
c& nebk
c: plotting
c+
      subroutine annconcg (lc, cin, slev, nlevs, levs, srtlev, yinc, 
     +                     xpos, ypos)
c
      implicit none
      integer nlevs, lc, srtlev(nlevs)
      real slev, xpos, ypos, yinc, levs(nlevs)
      character*(*) cin
c
c  Annotate plot with contour image information
c
c  Input:
c    lc      Handle for contour image
c    cin       Contour image name
c    slev      Scale factor that levels are scaled by
c    nlevs     Number of levels
c    levs      Contour levels
c    srtlev    Array  giving levels in increasing order
c    yinc      Y increment between bases of successive lines of text
c              in normalized device coordinates
c    xpos      X location for text
c  Input/output:
c    ypos      Y location for text.  On output, is the location for
c              the next line.
c--
c-----------------------------------------------------------------------
      character str1*132, str2*132, str3*132, src*20
      integer i1, i2, i3, len1, nlines
c-----------------------------------------------------------------------
c
c Scale factors
c
      write (str1, 100) slev
100   format ('Contours scaled by ', 1pe12.4)
      i1 = len1(str1)
c
c Source name
c
      call rdhda (lc, 'object', src, ' ')
      if (src.ne.' ') then
        str2 = ' Contour image: '//cin(1:len1(cin))//' ('//
     +         src(1:len1(src))//')'
      else
        str2 = ' Contour image: '//cin(1:len1(cin))
      end if
      i2 = len1(str2)
      if (slev.ne.1.0) then
        str3 = str2(1:i2)//'   '//str1(1:i1)
      else
        str3 = str2
      end if
      i3 = len1(str3)
      call pgtext (xpos, ypos, str3(1:i3))
      ypos = ypos - yinc
c
c Write out contour levels
c
      call confmtcg (xpos, ypos, yinc, nlevs, srtlev, levs, slev, 
     +               .true., nlines)
c
      end

c
c* anndefCG --Empirical definition of full annotation character size
c& nebk
c: plotting
c+
      subroutine anndefcg (cs, yinc, ygap)
c
      implicit none
      real cs, yinc, ygap
c
c  Empirical definition of size of normalized y viewsurface per 
c  character height for when doing full plot annotation. 
c  One character is defined to be 0.14 inches tall.
c
c  Output:
c    cs    The PGPLOT character size requred to make the required
c          character height
c    yinc  The distance between the bottoms of successive text 
c          lines in units of one character height
c    ygap  Gap between x label and annotaiton text in annotation
c          (CS) character heights
c--
c----------------------------------------------------------------------
      real xht, yht
c----------------------------------------------------------------------
c
c Find height of one character in mm for text written
c vertically and horizontally
c
      call pgsch (1.0)
      call pgqcs (2, xht, yht)
c
c Compute number of character heights in 3mm 
c
      cs = 3.0 / yht
c
c Set separation between text lines in character heights
c
      yinc = 1.2
c
c Gap between bottom of x-axis label and start of full annotation
c in unuts of annotation character height
c
      ygap = 0.75
c
      end
c
c* anngrsCG -- Annotate plot with information from a grey scale image
c& nebk
c: plotting
c+
      subroutine anngrscg (lg, gin, npixr, pixr, trfun, 
     +                     yinc, xpos, ypos)
c
      implicit none
      integer lg, npixr
      real pixr(2), yinc, xpos, ypos
      character*(*) gin, trfun
c
c  Annotate plot with grey scale image information
c
c  Input:
c    lg      Handle for grey scale image
c    gin     Grey scale image
c    npixr   Number of greys scale range groups given by user
c    pixr    Grey scale intensity range
c    trfun   Transfer function type
c    yinc    World increment between text lines
c    xpos    World x coordinate for text lines
c  Input/output
c    ypos    World y coordinate for next text line
c--
c-----------------------------------------------------------------------
      character src1*50, str1*132, str2*132, str3*132, 
     +  str4*132, units*9
      integer len1, i1, i2, i3, i4
c-----------------------------------------------------------------------
      call rdhda (lg, 'object', src1, ' ')
      if (src1.ne.' ') then
        str2 = ' Grey scale image: '//gin(1:len1(gin))//' ('//
     +          src1(1:len1(src1))//')'
      else
        str2 = ' Grey scale image: '//gin(1:len1(gin))
      end if
      i2 = len1(str2)
c
      if (npixr.eq.1) then
        write (str1, 100) pixr(1), pixr(2)
100     format ('Range = ', 1pe10.3, ' to ', 1pe10.3)
        i1 = len1(str1)
c
        str3 = str2(1:i2)//'    '//str1(1:i1)
        i3 = len1(str3)
c
        call rdhda (lg, 'bunit', units, ' ')
        if (units.ne.' ') then
          str4 = str3(1:i3)//' '//units(1:len1(units))//
     +           ' ('//trfun//')'
        else
          str4 = str3(1:i3)//' ('//trfun//')'
        end if
        i4 = len1(str4)
        call pgtext (xpos, ypos, str4(1:i4))
      else
        str3 = str2(1:i2)//'    '//'Various ranges'
        i3 = len1(str3)
        call pgtext (xpos, ypos, str3(1:i3))
      end if
      ypos = ypos - yinc
c
      end
c
c* anniniCG -- Init. plot annotation and write reference values to plot
c& nebk
c: plotting
c+
      subroutine anninicg (no3, naxis, crpix, crval, cdelt, ctype, 
     +                     vymin, pcs, ydispb, labtyp, xpos, ypos, yinc)
c
      implicit none
      integer naxis
      real xpos, ypos, yinc, pcs, ydispb, vymin
      double precision crval(naxis), cdelt(naxis), crpix(naxis)
      character*(*) ctype(naxis), labtyp(2)
      logical no3
c
c  Do some set up chores for the full plot annotation and
c  write the reference values to the plot.  The window is
c  redefined to be the same as the available part of the
c  view-surface in normalized device coords (0 -> 1) to make
c  life easier.
c    
c  Input
c   no3      DOn't write ref pix for third axis
c   naxis    NUmber of axes
c   c*       Axis descriptors
c   vymin    y viewsurface normalized device coordinate
c            at which the lowest sub-plot x-axis is drawn
c   pcs      PGPLOT character size parameters for plot labelling 
c            (not the annotation)
c   ydispb   Displacement of x-axis label in character heights
c   labtyp   Axis label types
c Output
c   x,ypos   World coordinate of next line of text to be written
c   yinc     World increment between lines of text
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxnax.h'
      double precision rd
      parameter (rd = 180.0/dpi)
c
      real xht, yht, xhta, yhta, acs, ychinc, yoff, ygap
      character str1*132, str2*132, abstyp*6, gentyp(maxnax)*4, 
     +  refstr*60
      integer len1, maxis, ip, il1, i
c-----------------------------------------------------------------------
c
c Define viewport to space left at bottom of viewsurface and define
c the window to something easy to use here.  Define character size
c for annotation
c
      call pgsvp (0.0, 1.0, 0.0, vymin)
      call pgswin (0.0, 1.0, 0.0, vymin)
      call anndefcg (acs, ychinc, ygap)
c
c Find size of one character in n.d.c. for axis labels and annotation
c
      call pgsch (pcs)
      call pgqcs (0, xht, yht)
      call pgsch (acs)
      call pgqcs (0, xhta, yhta)
c
c Find start of annotation, allowing for x-label and a bit of space
c between label and annotation
c
      if (labtyp(1).eq.'none') then
        yoff = (1.0 + ygap)*yhta
      else
        yoff = ydispb*yht + (1.0 + ygap)*yhta
      end if
c
c Increment between annotation lines in world coordinates (recall
c n.d.c. = world coordinates with the above viewport deifnitions)
c
      yinc = ychinc * yhta
c
      xpos = 0.0
      ypos = vymin - yoff
c
c Format reference pixels of each axis.
c
      str1 = ' '
      maxis = min(3,naxis)
      if (no3) maxis = min(2,naxis)
      ip = 2
      do i = 1, maxis
        call axtypcg (1, ctype(i), gentyp(i))
        il1 = len1(gentyp(i))
c
c Bit of a mess for UU or VV as their generic descriptor is UV
c
        if (gentyp(i)(1:il1).eq.'UV') then
          if (index(ctype(i),'UU').ne.0 .or. 
     +        index(ctype(i),'uu').ne.0) then
            str1(ip:) = 'UU,'
          else 
            str1(ip:) = 'VV,'
          end if
        else
          write (str1(ip:),'(a)') gentyp(i)(1:il1)//','
        end if
        ip = len1(str1) + 2
      end do
c
      ip = len1(str1)
      str1(ip:) = ' = '
      ip = ip + 3
      do i = 1, maxis
        call axabscg (1, gentyp(i), abstyp)
        call pix2wfcg (abstyp, i, crpix(i), naxis, crval, crpix,
     +                 cdelt, ctype, .false., refstr, il1)
c
        write (str1(ip:),'(a)') refstr(1:il1)//','
        ip = len1(str1) + 2
      end do     
c
      ip = len1(str1)
      write (str1(ip:), '(a)') ' at pixel ('
      ip = len1(str1) + 1
      do i = 1, maxis
        call strfd (crpix(i), '(f7.2)', str2, il1)
        str1(ip:) = str2(1:il1)//','
        ip = ip + il1 + 2
      end do
      ip = len1(str1)
      str1(ip:ip) = ')'
c
      call pgtext (xpos, ypos, str1(1:ip))
c
c Increment location
c
      ypos = ypos - yinc
c
      end
c
c* annspcCG -- Annotate plot with information from spectrum images
c& nebk
c: plotting
c+
      subroutine annspccg (nspec, spin, iscale, yinc, xpos, ypos)
c
      implicit none
      integer nspec
      real yinc, xpos, ypos, iscale(nspec)
      character*(*) spin(nspec)
c
c     Annotate plot with spectrum image information
c
c  Input:
c    nspec   NUmber of spectrum images
c    spin    Image names
c    iscale  Scale factor for each image
c    yinc    World increment between text lines
c    xpos    World x coordinate for text lines
c  Input/output
c    ypos    World y coordinate for next text line
c--
c-----------------------------------------------------------------------
      real xpos2, xlen, ylen
      character str1*132, str2*20, rtoaf*20
      integer len1, i1, i2, i
c-----------------------------------------------------------------------
c
c Write spectrum image names; there will be at least one
c Write them in the same colour they were plotted
c
      xpos2 = xpos
      call pgsci (7)
      call pgtext (xpos2, ypos, ' Spectrum images :')
      call pglen (4, ' Spectrum images :AA', xlen, ylen)
      xpos2 = xpos2 + xlen
c
      do i = 1, nspec
        str2 = rtoaf (iscale(i), 0, 3)
        i2 = len1(str2)
        str1 = spin(i)(1:len1(spin(i)))//' ('//str2(1:i2)//'),'
        i1 = len1(str1)
        if (i.eq.nspec) str1(i1:i1) = ' '
c
        call pgsci (i+1)
        call pgtext (xpos2, ypos, str1(1:i1))
c
        call pglen (4, str1(1:i1)//'AA', xlen, ylen)
        xpos2 = xpos2 + xlen
      end do
      ypos = ypos - yinc
      call pgsci (7)
c
      end
c
c* annvecCG -- Annotate plot with information from a vector image pair
c& nebk
c: plotting
c+
      subroutine annveccg (lv, vin, vfac, yinc, xpos, ypos)
c
      implicit none
      integer lv(2)
      real vfac(2), yinc, xpos, ypos
      character*(*) vin(2)
c
c  Annotate plot with vector image information
c
c  Input:
c    lv      Handles for vector images
c    vin     Vector image 
c    vfac    Maximum vector amplitude and scale in pixel units
c	     per mm (e.g. jy/beam per mm, or ratio per mm)
c    yinc    World increment between text lines
c    xpos    World x coordinate for text lines
c  Input/output
c    ypos    World y coordinate for next text line
c--
c-----------------------------------------------------------------------
      character src1*50, src2*50, str1*132, str2*132, str3*132, 
     +  units*20, btype*30
      integer len1, i1, i2, i3, iu
c-----------------------------------------------------------------------
      call rdhda (lv(1), 'object', src1, ' ')
      if (src1.ne.' ') then
         str3 = ' ('//src1(1:len1(src1))//')'
         i3 = len1(str3)
      else 
         str3 = ' '
         i3 = 1
      end if
c
      call rdhda (lv(2), 'object', src2, ' ')
      if (src2.ne.' ') then
         str2 = ' ('//src2(1:len1(src2))//')'
         i2 = len1(str2)
      else 
         str2 = ' '
         i2 = 1
      end if
      str1 = ' Vector images: '//vin(1)(1:len1(vin(1)))//
     +         str2(1:i2)//', '//vin(2)(1:len1(vin(2)))//str3(1:i3)
      i1 = len1(str1)
c
      call rdhda (lv(1), 'bunit', units, ' ')
      call rdbtype (lv(1), btype, ' ')
      if (units.eq.' ') then
        if (btype.eq.'fractional_polarization') then
          units = 'ratio'
        else if (btype.eq.'depolarization_ratio') then
          units = 'ratio'
        else if (btype.eq.'polarized_intensity') then
          units = 'Jy/beam'
        else if (btype.eq.'rotation_measure') then
          units = 'rad m\u-2\d'
        end if
      end if
      iu = max(1,len1(units))
c
c Format peak and scale factor
c
      call strfmtcg (vfac(1), 4, str2, i2)
      call strfmtcg (vfac(2), 4, str3, i3)
c
      str1(i1+1:) = ' |A\dmax\u|='//str2(1:i2)//' '//units(1:iu)//
     +              ', scale='//str3(1:i3)//' '//units(1:iu)//'/mm'
      i1 = len1(str1)
c
      call pgtext (xpos, ypos, str1(1:i1))
      ypos = ypos - yinc
c
      end
c
c* annwinCG -- Annotate plot with spatial window
c& nebk
c: plotting
c+
      subroutine annwincg (blc, trc, ibin, jbin, kbin, naxis, size,
     +                     cdelt, ctype, yinc, xpos, ypos)
c
      implicit none
      integer blc(*), trc(*), ibin(2), jbin(2), kbin(2), naxis, 
     +  size(naxis)
      real xpos, ypos, yinc
      double precision cdelt(naxis)
      character*(*) ctype(naxis)
c
c  Annotate plot with spatial window and channel increments
c
c  Input:
c    blc,trc   Window in pixels
c    i,jbin    Spatial inc and bin.
c    kbin      Channel increment and averaging size. If both 0,
c              don't write them out
c    naxis     Number of axes
c    size      Size of axes
c    cdelt     Pixel increments
c    ctype     Axis types
c    xpos      X location for text
c    yinc      Y increment between bases of successive lines of text
c              in normalized device coordinates
c  Input/output:
c    ypos      Y location for text.  On output, is the location for
c              the next line.
c--
c-----------------------------------------------------------------------
      character*4 str1, str2, str3, str4
      character*132 stra, strb, strc, strd, stre, line*200
      character gentyp*4, units*10
      integer i1, i2, i3, i4, ia, ib, ic, id, ie, il, iu
c
      integer len1
c-----------------------------------------------------------------------
c
c Format spatial window
c
      call pgnumb (blc(1), 0, 0, str1, i1)
      call pgnumb (blc(2), 0, 0, str2, i2)
      call pgnumb (trc(1), 0, 0, str3, i3)
      call pgnumb (trc(2), 0, 0, str4, i4)
      stra = ' Spatial region : '//str1(1:i1)//','//str2(1:i2)//' to '//
     +                            str3(1:i3)//','//str4(1:i4)
      ia = len1(stra)
c
c Format spatial binning
c
      if (ibin(1).gt.1 .or. ibin(2).gt.1 .or. jbin(1).gt.1 .or.
     +    jbin(2).gt.1) then
        call pgnumb (ibin(1), 0, 0, str1, i1)
        call pgnumb (ibin(2), 0, 0, str2, i2)
        call pgnumb (jbin(1), 0, 0, str3, i3)
        call pgnumb (jbin(1), 0, 0, str4, i4)
        strb = 'Spatial inc/bin : '//str1(1:i1)//'/'//str2(1:i2)//', '//
     +          str3(1:i3)//'/'//str4(1:i4)
        ib = len1(strb)
      else
        strb = ' '
        ib = 1
      end if
c
c Format spectral binning
c
      if (size(3).gt.1 .and. (kbin(1).gt.0 .and. kbin(2).gt.0)) then      
        call pgnumb (kbin(1), 0, 0, str1, i1)
        call pgnumb (kbin(2), 0, 0, str2, i2)
        strc = ' Spectral inc/bin : '//str1(1:i1)//'/'//str2(1:i2)
        ic = len1(strc)
c
        call strfmtcg (real(abs(kbin(1)*cdelt(3))), 4, str1, i1)
        call strfmtcg (real(abs(kbin(2)*cdelt(3))), 4, str2, i2)
c
        call axtypcg (1, ctype(3), gentyp)
        if (gentyp.eq.'FREQ') then
          units = 'GHz'
        else if (gentyp.eq.'VELO') then
          units = 'Km/s'
        else 
          units = 'none'
        end if
        iu = len1(units)
c  
        if (units.eq.'none') then
          strd = '='//str1(1:i1)//'/'//str2(1:i2)
        else
          strd = '='//str1(1:i1)//'/'//str2(1:i2)//' ('//
     +                units(1:iu)//')'
        end if
        id = len1(strd)
c
        stre = strc(1:ic)//' '//strd(1:id)
        ie = len1(stre)
      else
        stre = ' '
        ie = 1
      end if
c
      line = stra(1:ia)//' '//strb(1:ib)//' '//stre(1:ie)
      il = len1(line)
      call pgtext (xpos, ypos, line(1:il))
      ypos = ypos - yinc
c
      end
c
c* axlabCG -- Label axes
c& nebk
c: plotting
c+
      subroutine axlabcg (gaps, nx, ny, nz, nlast, iplot, xopts, yopts,
     +   xdispl, ydispb, labtyp, xlabel, ylabel, xxopts, yyopts)
c
      implicit none
      real xdispl, ydispb
      integer nx, ny, nz, nlast, iplot
      logical gaps
      character xopts*(*), yopts*(*), xxopts*(*), yyopts*(*),
     +  xlabel*(*), ylabel*(*), labtyp(2)*(*)
c
c  Label axes and prepare options strings for PGTBOX according to whether 
c  the sub-plots abut each other or not
c
c  Input
c    gaps    False means sub-plots abut, else they don't
c    nx,ny   Number of sub-plots in x and y directions on page
c    nz      Total number of sub-plots that will be drawn
c    nlast   Number of sub-plots on the last row of the last page
c    iplot   Number of current sub-plot
c    x,yopts Root option strings for PGTBOX
c    xdispl  Displacement in character heights of y-axis label
c    ydispb  Displacement in character heights of x-axis label
c    labtyp  Axis label types
c    xlabel  X-axis label
c    ylabel  Y-axis label
c  Output
c    xxopts  x-axis options string for PGTBOX
c    yyopts  y-axis options string for PGTBOX
c--
c-----------------------------------------------------------------------
      integer jplot, ix, iy
      integer len1
c-----------------------------------------------------------------------
      jplot = mod(iplot,nx*ny)
      if (jplot.eq.0) jplot = nx*ny
      ix = len1(xopts)
      iy = len1(yopts)
      xxopts = xopts(1:ix)
      yyopts = yopts(1:iy)
c
      if (.not.gaps) then
c
c When sub-plots abut each other, only label along the left most
c and bottom axes
c
        if (labtyp(1).ne.'none' .and. 
     +     (jplot.ge.nx*ny-nx+1 .or. iplot.ge.nz-nlast+1 .or.
     +      iplot+nx.gt.nz)) then
c
c Write x-axis label and prepare options string for numeric labelling
c
          if (mod(jplot,nx).eq.1) then
            xxopts = xopts(1:ix)//'N'
          else
            xxopts = xopts(1:ix)//'NF'
          end if
          call pgmtxt ('B', ydispb, 0.5, 0.5, xlabel)
        end if
c
        if (labtyp(2).ne.'none' .and. 
     +      mod(jplot,nx).eq.1 .or. nx.eq.1) then
c 
c Write y-axis label and prepare options string for numeric labelling
c
          yyopts = yopts(1:iy)//'N'
          call pgmtxt ('L', xdispl, 0.5, 0.5, ylabel)
        end if
      else       
c
c Write x and y-axis labels and prepare option strings for numeric labelling
c
        if (labtyp(1).ne.'none') xxopts = xopts(1:ix)//'N'
        if (labtyp(2).ne.'none') yyopts = yopts(1:iy)//'N'
        call pgmtxt ('B', ydispb, 0.5, 0.5, xlabel)
        call pgmtxt ('L', xdispl, 0.5, 0.5, ylabel)
      end if
c
      end
c
c* boxCG -- Draw axes and optionally numerically label
c& nebk
c: plotting
c+
      subroutine boxcg (donum, xopts, yopts)
c
      implicit none
      logical donum
      character*(*) xopts, yopts
c
c  Draw axes and put ticks on.  Optionally write the numeric labels.
c
c  Input
c    donum     If true, this is the first time we have displayed this
c              image so we label with numbers frame and box.  Otherwise
c              we have erased the display and all we want to do is redraw
c              the frame and ticks.  The numbers will not have gone.
c    x,yopts   PGTBOX labelling options
c--
c-----------------------------------------------------------------------
      character*22 xxopts, yyopts
      integer idx
c-----------------------------------------------------------------------
      xxopts = xopts
      yyopts = yopts
c
      if (.not.donum) then
        idx = index (xopts, 'N')
        if (idx.ne.0) xxopts(idx:idx) = ' '
        idx = index (yopts, 'N')
        if (idx.ne.0) yyopts(idx:idx) = ' '
      end if
c
      call pgtbox (xxopts, 0.0, 0, yyopts, 0.0, 0)
c
      end
c
c* confmtCG -- Format contour levels
c& nebk
c: plotting
c+
      subroutine confmtcg (xpos, ypos, yinc, nlevs, srtlev, levs, slev, 
     +                     write, nlines)
c
      implicit none
      integer nlevs, nlines, srtlev(nlevs)
      real levs(nlevs), slev, xpos, ypos, yinc
      logical write
c
c  Format contour levels and optionally write to viewport
c
c Input:
c   xpos      c location to start writing contours levels 
c             in world coordinates
c   yinc      Increment in w.c. to step down image for each
c             line of contours
c   nlevs     NUmber of contour levels
c   srtlev    Array sorting levels into ascending order
c   levs      Levels
c   slev      Scale factor by which user given levels are scaled
c             to make numbers in levs
c   write     True if actually want to write levels to viewport
c Input/output
c   ypos      y location at which to write levels on viewport in
c             world coordiantes.  On exit is location for next
c             line of text
c Output:
c   nlines    Number of lines needed to write all levels
c
c--
c-----------------------------------------------------------------------
      real xw1, xw2, yw1, yw2, x1, y1, x2, y2, dx
      integer i1, i2, len1, k
      character str1*132, str2*30
c-----------------------------------------------------------------------
      call pgqwin (xw1, xw2, yw1, yw2)      
      dx = abs(xw2 - xw1)
c
      nlines = 0
      str1 = ' Contours : '
      i1 = len1(str1) + 1
      k = 0
c
      do while (k.lt.nlevs)
c
c Format level
c
        k = k + 1
        call strfmtcg (levs(srtlev(k))/slev, 4, str2, i2)
c
        call pglen (4, str1(1:i1), x1, y1)
        call pglen (4, ' '//str2(1:i2)//',', x2, y2)
c
c See if space on current line for this level
c
        if (abs(x2)+abs(x1).le.dx) then
c
c Add level to line
c
          str1(i1+1:) = str2(1:i2)//', '
          i1 = len1(str1) + 1
c
c Write it out if last one
c
          if (k.eq.nlevs) then
            if (write) then
              call pgtext (xpos, ypos, str1(1:i1-2))
              ypos = ypos - yinc
            end if
            nlines = nlines + 1
          end if
        else
c
c No more space on this line, write it out if desired
c
          i1 = len1(str1)
          if (write) then
            call pgtext (xpos, ypos, str1(1:i1))
            ypos = ypos - yinc
          end if
          k = k - 1
c
          str1 = ' '
          i1 = 3
          nlines = nlines + 1
        end if
      end do
c
      end
c
c* conturCG -- Draw contour plot
c& nebk
c: plotting
c+
      subroutine conturcg (blank, solneg, win1, win2, dobl, 
     +                     data, nlevs, levs, tr, sdbreak)
c
      implicit none
      integer win1, win2, nlevs
      real data(win1,win2), levs(*), tr(6), sdbreak, blank
      logical solneg, dobl
c
c  Draw contours
c
c  Input:
c    blank    Vaue used for magic blanks
c    solneg   False => Positive contours solid, negative dashed.
c             True  => Positive contours dashed, negative solid.
c             "Positive" above means values >= SDBREAK
c    win1,2   Window sizes in x and y
c    dobl     True if blanks present in image section to contour
c    data     Image to contour
c    nlevs    Number of contour levels
c    levs     Contour levels
c    tr       Transformation matrix between array inices and
c             world coords
c    sdbreak  Value for distinction between solid and dashed contours
c--
c-----------------------------------------------------------------------
      integer stylehi, stylelo, i
c-----------------------------------------------------------------------
      if (.not.solneg) then
        stylehi = 1
        stylelo = 2
      else
        stylehi = 2
        stylelo = 1
      end if
c
      do i = 1, nlevs
        if (levs(i).ge.sdbreak) then
          call pgsls (stylehi)
        else
          call pgsls (stylelo)
        end if
        if (dobl) then
c
c This PG contouring routine does not do a very good job on dashed
c contours and is slower than PGCONT
c
           call pgconb (data, win1, win2, 1, win1, 1, win2, 
     +                  levs(i), -1, tr, blank)
        else
c
c Run faster contouring routine if no blanks
c
           call pgcont (data, win1, win2, 1, win1, 1, win2, 
     +                  levs(i), -1, tr)
          end if
      end do
      call pgsls (1)
c
      end
c
c* erswinCG -- Erase window from PGPLOT device
c& nebk
c: plotting
c+
      subroutine erswincg (xmin, xmax, ymin, ymax)
c
      implicit none
      real xmin, xmax, ymin, ymax
c
c  Erase the specified window from the PGPLOT device with 
c  rectangle fill
c
c  Input
c    x,ymin,max    Window in world coordinates
c--
c-----------------------------------------------------------------------
      integer ci, fs
c-----------------------------------------------------------------------
      call pgqci (ci)
      call pgqfs (fs)
c
      call pgsci (0)
      call pgsfs (1)
      call pgrect (xmin, xmax, ymin, ymax)
c
      call pgsci (ci)
      call pgsfs (fs)
c
      end
c
c* lab3CG -- Label sub-plot with value and/or pixel of 3rd axis
c& nebk
c: plotting
c+
      subroutine lab3cg (doerase, doval, dopix, crpix, cdelt, crval, 
     +                   ctype, labtyp, ipl, plav)
c
      implicit none
      double precision cdelt(*), crval(*), crpix(*)
      integer ipl, plav
      logical doval, dopix, doerase
      character*(*) ctype(*)
      character*6  labtyp(2)
c
c  Label the plot with the third axis values or pixel or both
c
c  Input
c    doerase    .true. to erase the background behind the string
c    doval      .true. if writing value
c    dopix      .true. if writing pixel
c    crpix      Array of reference pixels
c    cdelt      Array of pixel increments
c    crval      Array of reference values
c    ctype      Array of axis types
c    labtyp     Axis label types
c    ipl        Start plane of image being plotted
c    plav       Number of planes being averaged for current sub-plot
c--
c----------------------------------------------------------------------
      double precision val3, v1, v2, pix
      real mx, my, x1, x2, y1, y2, xb(4), yb(4), dx, dy
      character str1*30, str2*30, str3*60, types(3)*4, ltype*6
      integer i1, is2, ie2, i3
      logical ok
c
      integer len1
      character dangle*30, dangleh*30
c------------------------------------------------------------------------
c
c Prepare pixel label
c
      if (plav.eq.1) then
        call pgnumb (ipl, 0, 0, str1, i1)
        pix = ipl
      else
        pix = (ipl+ipl+plav-1) / 2.0
        call strfmtcg (real(pix), 1, str1, i1)
      end if
c
c Prepare value label.  The units depend upon what type of axis the
c third axis is, and what the units of the complementary axis is
c
      if (doval) then
        call axtypcg (3, ctype, types)
        if (types(3).eq.'VELO' .or. types(3).eq.'FREQ' .or.
     +      types(3).eq.'UV' .or. types(3).eq.'NONE') then
c
c Simple linear axis value label
c
          ltype = 'abslin'
        else if (types(3).eq.'RA' .or. types(3).eq.'LONG') then
c  
c Look for DEC axis amongst first two and find label type to
c set label type for 3rd axis value
c
          if (types(1).eq.'DEC' .or. types(1).eq.'LATI') then
            ltype = 'hms'
            if (labtyp(1).eq.'arcsec' .or. labtyp(1)(4:6).eq.'deg')
     +         ltype = labtyp(1)
          else if (types(2).eq.'DEC' .or. types(2).eq.'LATI') then
            ltype = 'hms'
            if (labtyp(2).eq.'arcsec' .or. labtyp(2)(4:6).eq.'deg')
     +         ltype = labtyp(2)
          end if
        else if (types(3).eq.'DEC' .or. types(3).eq.'LATI') then
c  
c Look for RA axis amongst first two and find label type to
c set label type for 3rd axis value
c
          if (types(1).eq.'RA' .or. types(1).eq.'LONG') then
            ltype = 'dms'
            if (labtyp(1).eq.'arcsec' .or. labtyp(1)(4:6).eq.'deg')
     +         ltype = labtyp(1)
          else if (types(2).eq.'RA' .or. types(2).eq.'LONG') then
            ltype = 'dms'
            if (labtyp(2).eq.'arcsec' .or. labtyp(2)(4:6).eq.'deg')
     +         ltype = labtyp(2)
          end if
        end if
c
c Now compute the value of the third axis in the desired units
c Also convert the increment between sub-plots to these units
c
        call pix2wcg (.false., pix, 3, ltype, 3, crval, crpix, 
     +                cdelt, ctype, val3, ok)
c
        call pix2wcg (.false., dble(plav), 3, ltype, 3, crval, crpix, 
     +                cdelt, ctype, v1, ok)
        call pix2wcg (.false., 0.0d0, 3, ltype, 3, crval, crpix, 
     +                cdelt, ctype, v2, ok)
c
c Format value
c
        if (ltype.eq.'hms') then
c
c  Pix2wcg returns RA in seconds of time; dangleh wants hours
c
          val3 = val3 / (3600.0d0 * 24.0d0)
c
c Unwrap if necessary
c 
          if (val3.lt.0.0d0) val3 = 24.0d0 + val3
          str2 = dangleh(val3)
        else if (ltype.eq.'dms') then
c
c  Pix2wcg returns DEC in seconds of arc; dangle wants degrees
c
          val3 = val3 / 3600.0d0
          str2 = dangle(val3)
        else 
          call strfmtcg (real(val3), 6, str2, ie2)
        end if  
        ie2 = len1(str2)
        is2 = 1
        do while (str2(is2:is2).eq.' ' .and. is2.le.ie2)
          is2 = is2 + 1
        end do
      end if
c
c Concatenate strings
c
      if (doval .and. dopix) then
        str3 = str2(is2:ie2)//', '//str1(1:i1)
      else if (doval) then
        str3 = str2(is2:ie2)
      else if (dopix) then
        str3 = str1(1:i1)
      end if
      i3 = len1(str3)
c
c Work out world coordinate of string BLC; 1 char in & 2 char down
c
      call pgqwin (x1, x2, y1, y2)
      call pgqtxt (0.0, 0.0, 0.0, 0.0, 'X', xb, yb)
      dx = (xb(4) - xb(1))
      call pgqtxt (0.0, 0.0, 0.0, 0.0, str3(1:i3), xb, yb)
      dy = (yb(2) - yb(1))
c       
      mx = x1 + dx
      my = y2 - 2.0*dy
c
c Erase rectangle and write string
c
      call strerscg (doerase, 0.0, str3(1:i3), mx, my)
c
      end
c
c* setlabCG -- Set label options strings and axis displacements
c& nebk
c: plotting
c+
      subroutine setlabcg (labtyp, ymin, ymax, xdispl, ydispb, 
     +                     xopts, yopts)
c
      implicit none
      character labtyp(*)*(*), xopts*(*), yopts*(*)
      real xdispl, ydispb, ymin, ymax
c
c  Set the labelling displacements from the relevant axes, and set 
c  PGTBOX labelling options strings.
c
c  Input
c    labtyp   Label type requested by user
c    ymin,max y axis min and max
c  Output
c    xdispl   Displacement in character heights from left y-axis 
c             for Y label
c    ydispb   Displacement in character heights from bottom x-axis 
c             for X label
c    x,yopts  PGTBOX options string, include 'Z' for time labelling
c--
c-------------------------------------------------------------------------------
      real dely, xch, ych, xl, yl, xd
      character str*60
      integer len1, il
c-----------------------------------------------------------------------
c
c X axis
c
      if (labtyp(1).eq.'hms') then
        ydispb = 3.6
        xopts = 'BCSTHYZO'
      else if (labtyp(1).eq.'dms') then
        ydispb = 3.6
        xopts = 'BCSTDYZO'
      else if (labtyp(1).eq.'none') then
        ydispb = 3.6
        xopts = 'BC'
      else
        ydispb = 3.1
        xopts = 'BCST'
      end if
c
c Y axis.  Have a stab at a correct axis label displacement when using
c HMS or DMS; it depends upon the number of decimal places in the 
c labels and knowing about the PGTBOX algorithm.  Very modular.
c Allow for space between numeric label and axis, and between
c numeric label and axis label.

      dely = abs(ymax - ymin)
      if (dely.le.5*60) then
        if (dely/6.0.lt.0.01) then
          str = '1O05\uh\d05\um\d05\us\d.555O'
        else if (dely/6.0.lt.0.1) then
          str = '1O05\uh\d05\um\d05\us\d.55O'
        else if (dely/6.0.lt.1.0) then
          str = '1O05\uh\d05\um\d05\us\d.5O'
        else
          str = '1O05\uh\d05\um\d05\us\dO'
        end if
      else if (dely.le.5*3600) then
        str = '1O05\uh\d05\um\dO'
      else 
        str = '1O05\uh\dO'
      end if
      il = len1(str)
      if (ymin.lt.0.0 .or. ymax.lt.0.0) then
        str(il+1:il+1) = '-'
        il = len1(str)
      end if
c
c Find the length of this string in mm and convert to
c displacement to left of axis for vertical axis label
c
      call pglen (2, str(1:il), xl, yl)
      call pgqcs (2, xch, ych) 
      xd = xl / xch
c
      if (labtyp(2).eq.'hms') then
        xdispl = xd
        yopts = 'BCSTHYZV'
      else if (labtyp(2).eq.'dms') then
        xdispl = xd
        yopts = 'BCSTDYZV'
      else if (labtyp(2).eq.'none') then
        xdispl = 1.0
        yopts = 'BC'
      else
        xdispl = 2.5
        yopts = 'BCST' 
      end if
c
      end
c
c* strersCG -- Optionally erase a rectangle on the view-port & write a string in it
c& nebk
c: plotting
c+
      subroutine strerscg (doerase, just, string, x, y)
c
      implicit none
      real x, y, just
      character string*(*)
      logical doerase
c
c  Optionally erase a snugly fitting rectangle and write a string to the
c  view-port into it
c
c  Input
c    doerase     Erase rectangle behind string if true.
c    just        Horizontal string justification.  
c                     0.0 -> left just
c                     0.5 -> centred
c                     1.0 -> right just
c    string      String to write
c    x,y         World coordinates of BLC of string
c--
c-----------------------------------------------------------------------
      integer len1, tbg
c-----------------------------------------------------------------------
      call pgqtbg (tbg)
      if (doerase) call pgstbg (0)
      call pgptxt (x, y, 0.0, just, string(1:len1(string)))
      call pgstbg (tbg)
c
      end
c
c* strfmtcg -- Format a number with PGNUMB
c& nebk
c: plotting
c+
      subroutine strfmtcg (xnum, ns, str, is)
c
      implicit none
      real xnum
      integer ns, is
      character*(*) str
c
c  Format a number with a specified number of significant figures with
c  the pgplot routine pgnumb. It chooses automatically decimal or
c  exponential notation.  Pgplot superscripting escape sequences
c  may be embedded in the string in the latter case.
c
c  Input:
c    xnum    The number = mm * 10**pp
c    ns      Number of desired signifcant figures
c  Output:
c    str     Formatted string
c    is      Length of string
c--
c-----------------------------------------------------------------------
      integer mm, pp
c-----------------------------------------------------------------------
      pp = int(log10(abs(xnum))) - ns
      mm = nint(xnum/10.0**pp)
c
      call pgnumb (mm, pp, 0, str, is)
c
      end
c
c* vpadjCG -- Adjust viewport if equal scales requested
c& nebk
c: plotting
c+
      subroutine vpadjcg (hard, eqscale, scale, vxmin, vymin, vymax,
     +   nx, ny, blc, trc, naxis, crval, crpix, cdelt, ctype, tfvp,
     +   wdgvp, vxsize, vysize)
c
      implicit none
      integer nx, ny, blc(*), trc(*), naxis
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      real vxsize, vysize, vxmin, vymin, vymax, scale(2), tfvp(4),
     +  wdgvp(4)
      logical eqscale
      character hard*(*), ctype(naxis)*(*)
c
c  So far everything has been worked out for unequal scales in
c  x and y.  If the user requests equal scales, or gives the scales,
c  we need to make some adjustments to the viewport
c
c  Inputs
c    hard         YES for hardcopy device
c    eqscale      True means equals scale requested, else different 
c    nx,ny        Number of sub-plots in x and y
c    blc,trc      Window in pixels
c    cdelt        Array of pixel increments
c  Input/Output
c    scale        scales in x and y in linear axis units/mm
c                 RA axes are radians on the sky per mm
c    vxmin        Left hand side of encompassing view port
c    vymin,vymax  Bottom and top of encompassing view port
c    tfvp         Transfer function fiddle plot viewport. SHould be all zero
c                 if no fiddling.
c    wdgvp        Wedge viewport.  All zero fo no wedge
c    vxsize       Size of viewport for sub-plots in normalized device 
c    vysize       coordinates
c--
c-----------------------------------------------------------------------
      double precision x1, x2, y1, y2, delx, dely, cosdec, xfac, yfac,
     +  xscale, yscale, xscale0, yscale0
      real vx1, vx2, vy1, vy2, vxmore, vymore, vxsize2, vysize2
      character aline*72, type*6
      logical ok, cdok(2), dofid, dowedge
      integer i
c-----------------------------------------------------------------------
c
c Since each axis type may be different convert window to "abslin"
c coordinates to work out equal scales
c
      type = 'abslin'
      call pix2wcg (.false., dble(blc(1)-0.5), 1, type, naxis, crval, 
     +              crpix, cdelt, ctype, x1, ok)
      call pix2wcg (.false., dble(trc(1)+0.5), 1, type, naxis, crval, 
     +              crpix, cdelt, ctype, x2, ok)
      call pix2wcg (.false., dble(blc(2)-0.5), 2, type, naxis, crval, 
     +              crpix, cdelt, ctype, y1, ok)
      call pix2wcg (.false., dble(trc(2)+0.5), 2, type, naxis, crval,
     +              crpix, cdelt, ctype, y2, ok)
c
c For RA/DEC axes, we want the scale equal on the sky, so we
c must take a cos(DEC) term into account
c
      call cosdeccg (1, naxis, ctype, crval, cosdec, cdok(1))
      delx = abs(x2 - x1)
      if (cdok(1)) delx = delx * cosdec
c
      call cosdeccg (2, naxis, ctype, crval, cosdec, cdok(2))
      dely = abs(y2 - y1)
      if (cdok(2)) dely = dely * cosdec
c
      if (.not.cdok(1) .or. .not.cdok(2)) then
        call bug ('w', 
     +    'VPADJCG: Cannot correctly work out scale for RA axis')
        call bug ('w', 
     +    'VPADJCG: because DEC axis missing; image may be squashed')
      end if
c
c Find width of viewport for each subplot in mm and compute scales
c in world coordinates per mm for image optimally filling the viewport.
c
      call pgsvp (0.0, vxsize, 0.0, vysize)
      call pgqvp (2, vx1, vx2, vy1, vy2)
      xscale0 = delx / (vx2 - vx1)
      yscale0 = dely / (vy2 - vy1)
c
c Now set scales actually used, allowing for user given scales,
c or unequal default scales
c
      if (scale(1).ne.0.0 .or. scale(2).ne.0.0) then
c
c Over-ride user given scales if too small to fit image on viewport
c
        if (scale(1).gt.xscale0) then
          xscale = scale(1) 
        else 
          xscale = xscale0
          if (scale(1).ne.0.0) call bug ('w', 
     +        'VPADJCG: User x-scale too small, will self-scale')
        end if
c
        if (scale(2).gt.yscale0) then
          yscale = scale(2)
        else 
          yscale = yscale0
          if (scale(2).ne.0.0) 
     +    call bug ('w', 
     +              'VPADJCG: User y-scale too small, will self-scale')
        end if
c
c Adjust for equal scales if required
c
        if (eqscale) then
          if (xscale.ne.yscale) call bug ('w', 
     +     'VPADJCG: Use options=unequal to honour different '//
     +     'values for keyword "scale"')
c
          xscale = max(xscale,yscale)
          yscale = xscale
        end if
      else
c
c Using defualt scales; adjust for equal scales if required
c
        if (eqscale) then
          xscale = max(xscale0,yscale0)
          yscale = xscale
        else
          xscale = xscale0
          yscale = yscale0
        end if
      end if
c
c Set factor by which we multiply subplot viewport size to
c allow for scake changes
c
      xfac = xscale0 / xscale
      yfac = yscale0 / yscale
c
c Tell user about scales, regardless of equality
c
      if (hard.eq.'YES') then
         write (aline, 100) xscale, yscale
100      format ('Linear x and y scales per mm = ', 
     +           1pe12.6, ', ', 1pe12.6)
         call output (aline)
      end if
c
c Adjust viewports for equal scales or user given scales
c
      if (eqscale .or. scale(1).ne.0.0 .or. scale(2).ne.0.0) then
c
c Set new sub-plot viewport sizes if required
c    
        vxsize2 = vxsize * xfac
        vysize2 = vysize * yfac
c
c Now because we may have made one or both of the subplot viewport 
c dimensions smaller, adjust the encompassing viewport so that the 
c sub-plots are still symmetrically placed on the viewsurface.  
c
        vxmore = nx * (vxsize - vxsize2)
        vxmin = vxmin + vxmore / 2.0
c
        vymore = ny * (vysize - vysize2)
        vymin = vymin + vymore / 2.0
        vymax = vymax - vymore / 2.0
c
c Set new sub-plot sizes
c
        vxsize = vxsize2
        vysize = vysize2
c
c Make sure we shift the transfer function fiddling plot 
c and wedge viewports too
c
        dofid = .false.
        dowedge = .false.
        do i = 1, 4
          if (tfvp(i).ne.0.0) dofid = .true.
          if (wdgvp(i).ne.0.0) dowedge = .true.
        end do
        if (dofid) then
          tfvp(1) = tfvp(1) - vxmore / 2.0
          tfvp(3) = tfvp(3) - vxmore / 2.0
          tfvp(2) = tfvp(2) + vymore / 2.0
          tfvp(4) = tfvp(4) + vymore / 2.0
        end if
        if (dowedge) then
          wdgvp(1) = wdgvp(1) - vxmore / 2.0
          wdgvp(3) = wdgvp(3) - vxmore / 2.0
          wdgvp(2) = wdgvp(2) + vymore / 2.0
          wdgvp(4) = wdgvp(4) - vymore / 2.0
        end if
      end if
c
c Return actual scales used
c
      scale(1) = xscale
      scale(2) = yscale
c
      end
c
c
c* vpsizCG -- Set encompassing viewport and subplot increment sizes
c& nebk
c: plotting
c+
      subroutine vpsizcg (dofull, dofid, ncon, gin, vin, nspec, bin, 
     +  maxlev, nlevs, srtlev, levs, slev, nx, ny, pcs, xdispl, 
     +  ydispb, gaps, wedcod, wedwid, wedisp, tfdisp, labtyp, vxmin, 
     +  vymin, vymax, vxgap, vygap, vxsize, vysize, tfvp, wdgvp)
c
      implicit none
      integer maxlev, nlevs(*), srtlev(maxlev,*), nx, ny, ncon, 
     +  wedcod, nspec
      real vxmin, vymin, vymax, vxgap, vygap, vxsize, vysize, pcs,
     +  ydispb, xdispl,  wedwid, wedisp, tfvp(4), tfdisp, wdgvp(4),
     +  levs(maxlev,*), slev
      logical dofid, dofull, gaps
      character*(*) gin, vin, bin, labtyp(2)*(*)
c
c   Work out view port that encompasses all sub-plots and allows
c   for all labelling below it.   Assume unequal scales in x and y 
c   here.  If user wants equal scales, adjust later.
c
c   Input
c     dofull      True for full plot annotation (contour levels etc)
c     dofid       True for interactive fiddle
c     ncon        Number of contour images
c     *in         Grey, vector and box type image names
c     nspec       Number of spectrum images
c     maxlevs     Maximum number of cintour levels per image
c     nlevs       Number of contour levels for each image
c     srtlev      Array to sort contours in increasing order
c     levs        Contour levels for each image
c     slev        Scale factor by which user given levels are scaled
c                 resulting in the numbers stored in levs
c     nx,ny       Number of sub-plots in x and y 
c     pcs         PGPLOT character size for plot labels
c     xdispl      Displacement of y-axis label from axis in char hghts
c     ydispb      Displacement of x-axis label from axis in char hghts
c     gaps        If true then don't leave gaps between sub-plots else
c                 leave gaps between sub-plots & label each window
c     wedcod      1 -> one wedge to right of all subplots
c                 2 -> one wedge to right per subplot
c                 3 -> one wedge per subplot inside subplot
c     wedwid      Fraction of full viewport for wedge width (wedcod=1)
c     wedisp      Displacement of wedge from right axis in char heights
c     tfdisp      Displacement of transfer function plot from right 
c                 axis in char heights
c     labtyp      Axis labels
c   Output
c     vxmin       X-min of viewport window in normalized device coords
c     vymin,vymax Y viewport range. Viewport encompasses all sub-plots
c     vx,ygap     Leave a gap between sub-plots in ndc in x and y
c     vx,ysize    Size of viewport of each sub-plot in ndcs in x & y
c     tfvp        Viewport coords in which to draw interactive fiddle plot
c     wdgvp       Viewport for wedge if wedcod = 1.  Other wedge type 
c                 viewports are worked out when the wedge is drawn in 
c---------------------------------------------------------------------------
c
c Fraction of viewsurface to use for interactive fiddle plot
c
      real tfvps
      parameter (tfvps = 0.1)
c
      real xht, yht, xhta, yhta, acs, ychinc, annlines, vxmax, dvwx,
     +  dvtx, dvwd, ygap, asp, dvtfx, dvtfy, dvtd, dvwl, xpos, ypos, 
     +  yinc
      integer nlines, i
      logical dowedge
c---------------------------------------------------------------------------
c
c Work out character height in n.d.c. for plot labels
c
      call pgsch (pcs)
      call pgqcs (0, xht, yht)
c
c Set viewport that encompasses all sub-plots in n.d.c.
c
      vxmin = (xdispl + 1.2)*xht
      vymax = 1.0 - yht
c
c Work out wedge spaces
c
      do i = 1, 4
        wdgvp(i) = 0.0
      end do
      dvwx = 0.0
      dowedge = wedcod.eq.1 .or. wedcod.eq.2
      if (dowedge) then
c
c Width of wedge label area and displacement from right hand 
c edge of subplot in ndc
c
        dvwl = 2.0 * xht
        dvwd  = wedisp * xht
c
c Total width taken up by wedge in ndc
c
        dvwx = wedwid + dvwl + dvwd
      end if
c
c Work out transfer function plot spaces
c
      do i = 1, 4
        tfvp(i) = 0.0
      end do
      dvtx = 0.0
      if (dofid) then
c
c We want the fiddle plot to be square on the screen so 
c find the width and height in ndc accordingly
c
        asp = yht / xht
        if (asp.ge.1.0) then
          dvtfx = tfvps / asp
          dvtfy = tfvps
        else
          dvtfx = tfvps 
          dvtfy = tfvps * asp
        end if
c
c x displacement of plot from edge of viewport
c
        dvtd  = tfdisp * xht
c
        dvtx = dvtfx + dvtd
      end if
c
c Set x trc of image viewport
c
      vxmax = 1.0 - max(dvwx,dvtx) - xht
c
c When doing full annotation need to make space at bottom of plot. Allow 
c for x axis label, gap between it and start of text, lines of text, and 
c space between lines of text.
c
      if (dofull) then
        annlines = 0.0
        if (gin.ne. ' ') annlines = annlines + 1.0
        if (vin.ne.' ')  annlines = annlines + 1.0
        if (bin.ne.' ')  annlines = annlines + 1.0
        if (nspec.gt.0)  annlines = annlines + 1.0
c
c Define annotation character size and set it 
c
        call anndefcg (acs, ychinc, ygap)
        call pgsch (acs)
c
        if (ncon.gt.0) then
c
c Find number of lines for contours; set viewport for x direction
c xpos etc dummies as we won't actually plot anything here
c
          call pgsvp (vxmin, vxmax, 0.0, 1.0)
          call pgswin (0.0, 1.0, 0.0, 1.0)
          xpos = 0.0
          ypos = 0.0
          yinc = 0.0
          do i = 1, ncon 
            call confmtcg (xpos, ypos, yinc, nlevs(i), srtlev(1,i), 
     +                     levs(1,i), slev, .false., nlines)
            annlines = annlines + nlines + 1.0
          end do
        end if
c
c Need lines for reference values and window as well.  Window is written
c last and has dangling letters in its line, so allow extra 0.5 character
c heights for that too.
c
        annlines = annlines + 2.5
c
c Allow some extra space for the x-label annotation and a gap
c between it and the additional annotation.  This is not very
c modular, and these numbers must match those in ANNINICG
c
        call pgqcs (0, xhta, yhta)
        if (labtyp(1).ne.'none') then
          vymin = (ydispb*yht) + ((annlines*ychinc)+ygap)*yhta
        else
          vymin = ((annlines*ychinc)+ygap)*yhta
        end if
      else
        vymin = (ydispb + 0.5) * yht
      end if
c
c Now allow for the transfer function fiddle plot if necessary.  It sits
c below the viewport and to the right.  Any full plot annotation will 
c reuse its space.  Set transfer function plot viewport
c
      if (dofid) then
        vymin = max(vymin,dvtfy+yht)
c
        tfvp(1) = vxmax + dvtd
        tfvp(2) = vymin - dvtfy - 0.75*yht
        tfvp(3) = tfvp(1) + dvtfx
        tfvp(4) = tfvp(2) + dvtfy
      end if
c
c Set wedge viewport
c
      if (dowedge) then
        wdgvp(1) = vxmax + dvwd
        wdgvp(2) = vymin
        wdgvp(3) = wdgvp(1) + wedwid
        wdgvp(4) = vymax
      end if
c
c Work out size of sub-plots and gaps between in n.d.c. For gap allow
c for label displacement plus 2 extra characters worth of space
c
      if (nx.gt.1) then
        if (gaps) then
          vxgap = (1.0 + xdispl + 2.0) * xht
        else
          vxgap = 0.0
        end if
        vxsize = ((vxmax - vxmin) - ((nx - 1) * vxgap)) / nx
      else
        vxgap = 0.0
        vxsize = vxmax - vxmin
      end if
c
      if (ny.gt.1) then
        if (gaps) then
          vygap = (ydispb + 2.0) * yht
        else
          vygap = 0.0
        end if
        vysize = ((vymax - vymin) - ((ny - 1) * vygap)) / ny
      else
        vygap = 0.0
        vysize = vymax - vymin
      end if
c
      end
c
c
c* wedgCG -- Draw grey scale wedge in specified viewport
c& nebk
c: plotting
c+
      subroutine wedgcg (label, trfun, groff, nbins, cumhis, wdgvp, 
     +                   fg, bg)
c
      implicit none
      integer nbins
      real wdgvp(4), fg, bg, groff, cumhis(nbins)
      character trfun*3
      logical label
c
c Draw a vertical grey-scale wedge in the specified viewport
c
c Input
c  label  True means label wedge to right else none
c  trfun  Transfer function type applied to image.  One of 'lin',
c         'log', 'heq' or 'sqr'
c  groff  Offset added to image for log and sqrt transfer functions
c  nbins  Number of bins used in histogram equalization of image
c  cumhis Cumulative histogram for histogram equalization
c         Values for each bin are the intensities assigned to   
c         the image.  Thus if an image pixel ended up in
c         cumhis bin idx, then its new value is cumhis(idx)
c  wdgvp  Viewport to draw wedge in
c  fg     The value which is to appear with shade 1 ("foreground"). 
c         Use the values of FG and BG that were sent to PGGRAY.
c  bg     The value which is to appear with shade 0 ("background").
c         These values should be those appropriate to before any
c         application of transfer functions (log etc) and adding of
c         offsets (GROFF)
c
c
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      real wx1, wx2, wy1, wy2, vx1s, vx2s, vy1s, vy2s, wdginc, tr(6), 
     +  fg2, bg2
      integer i, ipw, nbins2
c
      save tr
      data tr /0.0, 1.0, 0.0, 0.0, 0.0, 1.0/
c-----------------------------------------------------------------------
c
c Allocate memory for wedge
c
      nbins2 = nbins
      if (trfun.ne.'heq') nbins2 = 128
      call memalloc (ipw, nbins2, 'r')
c
c Store the current world and viewport coords and the character height.
c
      call pgqwin (wx1, wx2, wy1, wy2)
      call pgqvp (0, vx1s, vx2s, vy1s, vy2s)
c
c Create a dummy wedge array to be plotted.
c
      if (trfun.eq.'log') then
        fg2 = log10(fg+groff)
        bg2 = log10(bg+groff)
      else if (trfun.eq.'sqr') then
        fg2 = sqrt(fg+groff)
        bg2 = sqrt(bg+groff)
      else if (trfun.eq.'heq') then
        fg2 = cumhis(nbins2)
        bg2 = cumhis(1)
      else
        fg2 = fg
        bg2 = bg
      end if
c
c Generate wedge with correct transfer function applied
c
      if (trfun.eq.'heq') then
c
c Make it from histogram returned by HEQCG
c
        do i = 1, nbins2
          memr(ipw+i-1) = cumhis(i)
        end do
      else
c
c Generate linear wedge
c
        wdginc = (fg-bg) / (nbins2-1)
        do i = 1, nbins2
          memr(ipw+i-1) = bg + (i-1) * wdginc
c
c Apply transfer function
c
          if (trfun.eq.'log') then
            memr(ipw+i-1) = log10(memr(ipw+i-1)+groff)
          else if (trfun.eq.'sqr') then
            memr(ipw+i-1) = sqrt(memr(ipw+i-1)+groff)
          end if
        end do
      end if
c
c Draw the wedge and label
c
      call pgsvp (wdgvp(1), wdgvp(3), wdgvp(2), wdgvp(4))
      call pgswin (0.9, 1.1, 1.0, real(nbins2))
      call pggray (memr(ipw), 1, nbins2, 1, 1, 1, nbins2, 
     +             fg2, bg2, tr)
      call pgswin (0.0, 1.0, bg, fg)
      if (label) then
c
c Label box to right
c
        call pgbox('BC', 0.0, 0, 'BCMST', 0.0, 0)
      else 
c
c No labels.
c
        call pgbox('BC', 0.0, 0, 'BCST', 0.0, 0)
      end if
c
c Restore the original viewport and world coordinates.
c
      call pgsvp (vx1s, vx2s, vy1s, vy2s)
      call pgswin (wx1, wx2, wy1, wy2)
      call pgupdt
c
c Free up memory
c
      call memalloc (ipw, nbins2, 'r')
c
      end
c
c* wedgeCG -- Decide if it is time to draw a wedge and do so if so
c& nebk
c: plotting
c+
      subroutine wedgecg (wedcod, wedwid, jj, trfun, groff, nbins, 
     +                    cumhis, wdgvp, fg, bg)
c
      implicit none
      real groff, cumhis(*), wdgvp(4), fg, bg, wedwid
      integer wedcod, jj, nbins
      character trfun*3
c
c Work out whether the grey scale wedges are to be drawn inside
c or outside the subplots, and whether there will be one or many
c  
c Input
c  wedcod 1 -> one wedge to right of all subplots
c         2 -> one wedge to right per subplot
c         3 -> one wedge per subplot inside subplot
c  wedwid Fraction of subplot viewport for wedge (wedcod=2,3)
c  jj     Number of subplot on this page
c  trfun  Transfer function type applied to image.  
c  groff  Offset added to image for log and sqrt transfer functions
c  nbins  Number of bins used in histogram equalization of image
c  cumhis Cumulative histogram for histogram equalization returned
c         by HEQCG
c  wdgvp  Viewport to draw wedge in (wedcod=1)
c  fg,bg  Grey scale max and min
c         Use the values of FG and BG that were sent to PGGRAY.
c         These values should be those appropriate to before 
c         any application of transfer functions (log etc) and 
c         adding of offsets
c  nx,ny  Number of subplots in x and y directions
c  npixr  NUmber of grey scale "range" groups given by user
c  trfun  Transfer function type of first "range" group
c--
c-----------------------------------------------------------------------
      real vx1, vx2, vy1, vy2, wv(4), xht, yht, wedfrc
c-----------------------------------------------------------------------
      call pgqvp (0, vx1, vx2, vy1, vy2)
      call pgqcs (0, xht, yht)
      wedfrc = wedwid * (vx2 - vx1)
c
      if (wedcod.eq.1) then
        if (jj.eq.1) then
          call wedgcg (.true., trfun, groff, nbins, cumhis, wdgvp, 
     +                 fg, bg)
        end if
      else if (wedcod.eq.2) then
        wv(1) = vx2 + 1.0*xht
        wv(2) = vy1
        wv(3) = wv(1) + wedfrc
        wv(4) = vy2
        call wedgcg (.true., trfun, groff, nbins, cumhis, wv,
     +               fg, bg)
      else
        wv(1) = vx2 - wedfrc
        wv(2) = vy1
        wv(3) = vx2
        wv(4) = vy2
        call wedgcg (.false., trfun, groff, nbins, cumhis, wv,
     +               fg, bg)
      end if
c
      end
c
c* yhtwCG -- Find height of one character in world coordinates
c& nebk
c: plotting
c+
      subroutine yhtwcg (yht)
c
      implicit none
      real yht
c
c  Find the height, in world coordinates, of one character
c  with the current PGPLOT character size active
c
c  Output:
c    yht     Height in world coordinates of one character
c--
c-----------------------------------------------------------------------
      real xch, ych, vpx1, vpx2, vpy1, vpy2, wx1, wx2, wy1, wy2
c-----------------------------------------------------------------------
c
c Find current height of one character in normalized device coordinates
c
      call pgqcs (0, xch, ych)
c
c Find current view-port in ndc
c
      call pgqvp (0, vpx1, vpx2, vpy1, vpy2)
c
c Find current window in world coordinates
c
      call pgqwin (wx1, wx2, wy1, wy2)
c
c Convert height from ndc to world coordinates
c
      yht = abs((wy2-wy1) * ych / (vpy2-vpy1))
c
      end


