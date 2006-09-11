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
c  aaxlabcg :  Write ascii axis labels
c  bgcolcg  :  See if background colour of PGPLOT device is black or white
c  confmtcg :  Format contour levels
c  conturcg :  Draw contour plot
c  drwlincg :  Draw vertical/horizontal line at constant x/y world coordinate
c  drwtikcg :  Draw (nonlinear) ticks/grid
c  erswincg :  Erase window
c  lab3cg   :  Label sub-plot with value and/or pixel of third axis
c  naxlabcg :  Draw frame, write numeric labels, ticks and grid
c  setdspcg :  Set axis label displacements 
c  strerscg :  Erase rectangle on plot and write string into it
c  strfmtcg :  Format a number into a string with PGNUMB
c  vpadjcg  :  Adjust viewport if equal scales requested
c  vpsizcg  :  Set encompassing viewport and subplot increments
c  wedgecg  :  High level routine to draw wedges (calls WEDGCG)
c  wedgcg   :  Draw grey scale wedge in specified viewport
c  yhtwcg   :  Find y-height of one character in world coordinates
c
c                 Temporary fudge until PGQTIK available
c  qtikcg   :  Find out about ticks from PG(T)BOX
c  pgtbx1cg
c  pgtbx2cg
c  pgtbx3cg
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
c     nebk   24aug94     Change LAB3CG to use COSUBS coordinate conversion
c                        routines to label with true world value of third
c                        third axis
c     nebk   23dec94     Strings (str1:4) not long enough in ANNWINCG
c                        Increase length of STR1 in CONFMTCG
c     nebk   05jan95     Replace PGGRAY by new PGIMAG. 
c     nebk   14apr95     Label grey scales as "pixel maps" in ANNGRSCG
c     nebk   11aug95     Add arcmin labels and argument nofirst to AXLABCG
c     nebk   24aug95     Add grid argument to SETLABCG
c     nebk   02sep95     Add BGCOLCG, LABAXCG, DRHORICG, DRVERTCG.
c			 Remove BOXCG.  Add dotr argument to VPSIZCG and
c		         AXLABCG.  Add temporary tick enquiry fuges through 
c		         QTIKCG, PGTBX1-3CG
c     nebk   19oct95     Bias wedges by pixr(1) rather than image min
c                        when log or square root transfer function
c     nebk   22nov95     Fix problem with top/right labelling of multi-panel
c 	                 plots. Remove WEDISP from call sequence of VPSIZCG
c     nebk   23nov95     Add argument CONLAB to CONTURCG to label contours
c                        and new call for CTYPECO
c     nebk   29nov95     Use only W2WCO routines in NAXLABCG and friends,
c                        rather than directly using co.for routines
c     nebk   18dec95     Add argument DOABUT to VPSIZCG. Fix truncated
c                        strings in ANNINICG and ANNWINCG
c     nebk   31jan96     More sig figs for pixel label in LAB3CG
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
     +  units*20, btype*30, rtoaf*20
      integer len1, iu, i1, i2, i3, i4
c-----------------------------------------------------------------------
c
c File and objects
c
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
      str2 = rtoaf(bfac(4),0,4)
      i2 = len1(str2)
      str3 = rtoaf(bfac(5),0,4)
      i3 = len1(str3)
      str4 = rtoaf(bfac(1),0,4)
      i4 = len1(str4)
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
      subroutine annconcg (lc, cin, slev, nlevs, levs, srtlev, dmm,
     +                     yinc, xpos, ypos)
c
      implicit none
      integer nlevs, lc, srtlev(nlevs)
      real slev, xpos, ypos, yinc, levs(nlevs), dmm(2)
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
c    dmm       Data min/max
c    yinc      Y increment between bases of successive lines of text
c              in normalized device coordinates
c    xpos      X location for text
c  Input/output:
c    ypos      Y location for text.  On output, is the location for
c              the next line.
c--
c-----------------------------------------------------------------------
      character str1*132, str2*132, str3*132, src*20, rtoaf*20, units*20
      integer i1, i2, i3, len1, nlines
c-----------------------------------------------------------------------
c
c Source name
c
      call rdhda (lc, 'object', src, ' ')
      if (src.ne.' ') then
        str1 = ' Contour image: '//cin(1:len1(cin))//' ('//
     +         src(1:len1(src))//')'
      else
        str1 = ' Contour image: '//cin(1:len1(cin))
      end if
      i1 = len1(str1)
c
c Write data min and max
c
      str2 = rtoaf(dmm(1),0,4)
      i2 = len1(str2)
      str3 = rtoaf(dmm(2),0,4)
      i3 = len1(str3)
      str1(i1+1:) = '  Min/max='//str2(1:i2)//
     +              '/'//str3(1:i3)
      i1 = len1(str1)
c
c Scale factors
c
      str2 = '  Contours x '//rtoaf(slev,0,4)
      i2 = len1(str2)
      if (slev.ne.1.0) str1(i1+1:) = str2(1:i2)
      i1 = len1(str1)
c
c Tack on units
c
      call rdhda (lc, 'bunit', units, ' ')
      if (units.ne.' ') str1(i1+1:) = ' '//units(1:len1(units))
      i1 = len1(str1)
c
      call pgtext (xpos, ypos, str1(1:i1))
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
c* anngrsCG -- Annotate plot with information from a pixel map image
c& nebk
c: plotting
c+
      subroutine anngrscg (lg, gin, npixr, pixr, trfun, dmm,
     +                     yinc, xpos, ypos)
c
      implicit none
      integer lg, npixr
      real pixr(2), yinc, xpos, ypos, dmm(2)
      character*(*) gin, trfun
c
c  Annotate plot with pixel map image information
c
c  Input:
c    lg      Handle for pixel map image
c    gin     pixel map image
c    npixr   Number of greys scale range groups given by user
c    pixr    pixel map intensity range
c    trfun   Transfer function type
c    yinc    World increment between text lines
c    xpos    World x coordinate for text lines
c    dmm     Min and max displayed for this image
c  Input/output
c    ypos    World y coordinate for next text line
c--
c-----------------------------------------------------------------------
      character src1*50, str1*132, str2*132, str3*132, 
     +  units*9, rtoaf*20
      integer len1, i1, i2, i3
c-----------------------------------------------------------------------
c
c Write image name
c
      call rdhda (lg, 'object', src1, ' ')
      if (src1.ne.' ') then
        str1 = ' Pixel map image: '//gin(1:len1(gin))//' ('//
     +          src1(1:len1(src1))//')'
      else
        str1 = ' Pixel map image: '//gin(1:len1(gin))
      end if
      i1 = len1(str1)
c
c Write data min and max
c
      str2 = rtoaf(dmm(1),0,4)
      str3 = rtoaf(dmm(2),0,4)
      str1(i1+1:) = '  Min/max='//str2(1:len1(str2))//
     +              '/'//str3(1:len1(str3))
      i1 = len1(str1)
c
c Write pixel map ranges
c
      call rdhda (lg, 'bunit', units, ' ')
      if (npixr.eq.1) then
        str2 = rtoaf(pixr(1),0,4)
        i2 = len1(str2)
        str3 = rtoaf(pixr(2),0,4)
        i3 = len1(str3)
c
        str1(i1+1:) = '  Range = '//str2(1:i2)//' to '//str3(1:i3)
        i1 = len1(str1)
c
        if (units.ne.' ') then
          str1(i1+1:) = ' '//units(1:len1(units))//' ('//trfun//')'
        else
          str1(i1+1:) = ' ('//trfun//')'
        end if
      else
        if (units.ne.' ') then
          str1(i1+1:) = ' '//units(1:len1(units))//'  Various ranges'
        else
          str1(i1+1:) = '  Various ranges'
        end if
      end if
      i1 = len1(str1)
c
      call pgtext (xpos, ypos, str1(1:i1))
      ypos = ypos - yinc
c
      end
c
c* anniniCG -- Init. plot annotation and write reference values to plot
c& nebk
c: plotting
c+
      subroutine anninicg (lh, no3, vymin, pcs, ydispb, labtyp, 
     +                     xpos, ypos, yinc)
c
      implicit none
      integer lh
      real xpos, ypos, yinc, pcs, ydispb, vymin
      character*(*) labtyp(2)
      logical no3
c
c  Do some set up chores for the full plot annotation and
c  write the reference values to the plot.  The window is
c  redefined to be the same as the available part of the
c  view-surface in normalized device coords (0 -> 1) to make
c  life easier.
c    
c  Input
c   lh       Image handle
c   no3      DOn't write ref pix for third axis
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
      double precision win(maxnax)
      real xht, yht, xhta, yhta, acs, ychinc, yoff, ygap
      character str1*132, str2*132, gentyp*4, typeo(maxnax)*6, 
     +  typei(maxnax)*6, refstr(maxnax)*30, ctype*9, 
     +  itoaf*1
      integer len1, naxis, maxis, ip, il1, i, ir(maxnax), il
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
      call rdhdi (lh, 'naxis', naxis, 0)
      maxis = min(3,naxis)
      if (no3) maxis = min(2,naxis)
      ip = 2
      do i = 1, maxis
        typei(i) = 'abspix'
        call rdhdd (lh, 'crpix'//itoaf(i), win(i), 0.0d0)
c
        call axtypco (lh, 0, i, gentyp)
        il1 = len1(gentyp)
c
c Bit of a mess for UU or VV as their generic descriptor is UV
c
        if (gentyp(1:il1).eq.'UV') then
          call ctypeco (lh, i, ctype, il)
          if (ctype(1:il).eq.'UU') then
            str1(ip:) = 'UU,'
          else 
            str1(ip:) = 'VV,'
          end if
        else
          write (str1(ip:),'(a)') gentyp(1:il1)//','
        end if
        ip = len1(str1) + 2
      end do
      ip = len1(str1)
      str1(ip:) = ' = '
c
      call initco (lh)
      call setoaco (lh, 'abs', maxis, 0, typeo)
      call w2wfco (lh, maxis, typei, ' ', win, typeo, ' ', .false.,
     +             refstr, ir)
      call finco (lh)
      do i = 1, maxis
        ip = len1(str1) + 2
        write (str1(ip:),'(a)') refstr(i)(1:ir(i))//','
      end do      
c
      ip = len1(str1)
      write (str1(ip:), '(a)') ' at pixel ('
      ip = len1(str1) + 1
      do i = 1, maxis
        call strfd (win(i), '(f7.2)', str2, il1)
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
        str2 = rtoaf (iscale(i),0,4)
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
c
c Image name
c
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
c Units
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
      subroutine annwincg (lh, blc, trc, ibin, jbin, kbin, yinc, 
     +                     xpos, ypos)
c
      implicit none
      integer blc(*), trc(*), ibin(2), jbin(2), kbin(2), lh
      real xpos, ypos, yinc
c
c  Annotate plot with spatial window and channel increments
c
c  Input:
c    lh        Image handle
c    blc,trc   Window in pixels
c    i,jbin    Spatial inc and bin.
c    kbin      Channel increment and averaging size. If both 0,
c              don't write them out
c    xpos      X location for text
c    yinc      Y increment between bases of successive lines of text
c              in normalized device coordinates
c  Input/output:
c    ypos      Y location for text.  On output, is the location for
c              the next line.
c--
c-----------------------------------------------------------------------
      double precision cdelt3
      character*30 str1, str2, str3, str4
      character*132 stra, strb, strc, strd, stre, line*200
      character units*10
      integer i1, i2, i3, i4, ia, ib, ic, id, ie, il, iu, naxis3
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
      call rdhdi (lh, 'naxis3', naxis3, 0)
      if (naxis3.gt.1 .and. (kbin(1).gt.0 .and. kbin(2).gt.0)) then      
        call pgnumb (kbin(1), 0, 0, str1, i1)
        call pgnumb (kbin(2), 0, 0, str2, i2)
        strc = ' Spectral inc/bin : '//str1(1:i1)//'/'//str2(1:i2)
        ic = len1(strc)
c
        call rdhdd (lh, 'cdelt3', cdelt3, 0.0d0)
        call strfmtcg (real(abs(kbin(1)*cdelt3)), 4, str1, i1)
        call strfmtcg (real(abs(kbin(2)*cdelt3)), 4, str2, i2)
c
        call sunitco (lh, 3, 'absnat', units)
        iu = len1(units)
c  
        if (units.eq.' ') then
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
c* aaxlabCG -- Write ascii axis labels
c& nebk
c: plotting
c+
      subroutine aaxlabcg (dox, doy, xdispl, ydispb, xlabel, ylabel)
c
      implicit none
      real xdispl, ydispb
      logical dox, doy
      character xlabel*(*), ylabel*(*)
c
c  Write ascii axis labels
c
c  Input
c    dox,y   True to write x or y labels
c    xdispl  Displacement in character heights of y-axis label
c    ydispb  Displacement in character heights of x-axis label
c    xlabel  X-axis label
c    ylabel  Y-axis label
c--
c-----------------------------------------------------------------------
      if (dox) call pgmtxt ('B', ydispb, 0.5, 0.5, xlabel)
      if (doy) call pgmtxt ('L', xdispl, 0.5, 0.5, ylabel)
c
      end
c
c* bgcolCG -- Find background colour of PGPLOT device
c& nebk
c: plotting
c+
      subroutine bgcolcg (bgcol)
c
      implicit none
      integer bgcol
c
c  Look at the RGB colours of colour index 0 to see whether the
c  device background is black or white
c
c  Output:
c    bgcol      0 -> black
c               1 -> white
c	       -1 -> something else
c--
c-----------------------------------------------------------------------
      real r, g, b
c-----------------------------------------------------------------------
      call pgqcr (0, r, g, b)
      if  (abs(r).lt.0.0001 .and. abs(g).lt.0.0001 .and. 
     +    abs(b).lt.0.0001) then
        bgcol = 0
c        write (*,*) 'background is black'
      else if (1.0-abs(r).lt.0.0001 .and. 1.0-abs(g).lt.0.0001 .and. 
     +         1.0-abs(b).lt.0.0001) then
        bgcol = 1
c        write (*,*) 'background is white'
      else
        bgcol = -1
c        write (*,*) 'background is something funny'
      end if
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
      character str1*1000, str2*30
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
      subroutine conturcg (conlab, blank, solneg, win1, win2, dobl, 
     +                     data, nlevs, levs, tr, sdbreak)
c
      implicit none
      integer win1, win2, nlevs
      real data(win1,win2), levs(*), tr(6), sdbreak, blank
      logical solneg, dobl, conlab
c
c  Draw contours
c
c  Input:
c    conlab   Label contours ?
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
      integer stylehi, stylelo, i, intval, minint, il, ns
      character label*20
c-----------------------------------------------------------------------
c
c Set how often we label contours.  The PGPLOT routine is prett dumb.
c Because contouring is done in quadrants, each quadrant is labelled
c individually.  The size of the quadrants is 256 pixels (see
c pgconx, pgcnxb).  MININT says draw first label after contours
c cross this many cells, and every INTVAL thereafter. 
c
      minint = 20
      intval = 40
      if (conlab) then
c        write (*,*) 'default minint, intval=', minint,intval
c        write (*,*) 'enter minint, intval'
c        read (*,*) minint,intval
        if (dobl) then
          call output ('Contour labelling is not yet implemented')
          call output ('for images containing blanked pixels')
        end if
      end if
c
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
     +                 levs(i), -1, tr, blank)
        else
c
c Run faster contouring routine if no blanks
c
          call pgcont (data, win1, win2, 1, win1, 1, win2, 
     +                 levs(i), -1, tr)
        end if
c
c Label contour value
c
        if (conlab) then
          ns = int(abs(log10(abs(levs(i))))) + 3
          call strfmtcg (real(levs(i)), ns, label, il)
          if (dobl) then
c            call pgcnlb (data, win1, win2, 1, win1, 1, win2,
c     +                 levs(i), tr, blank, label(1:il), 
c     +                 intval, minint)
          else
            call pgconl (data, win1, win2, 1, win1, 1, win2,
     +                 levs(i), tr, label(1:il), intval, minint)
          end if
        end if
      end do
      call pgupdt
      call pgsls (1)
c
      end
c
c
      subroutine drwlincg (lun, axis, type, n, wc, zp, p1, p2,
     +                     xline, yline)
c-----------------------------------------------------------------------
c  Draw a vertical or horizontal line at constant x or y  coordinate
c
c  Input
c    lun   handle for coordinate conversions
c    axis  'x' or 'y' for vertical or horizontal lines
c    type  Coordinate conversion types matching LABTYP
c    n     number of points
c    w     constant world coordinate (x or y depending on axis)
c    zp    z absolute pixel for this plane
c    p1,2  range of orthogonal axis pixels to draw between
c  Scratch
c    x,yline
c          plot buffers
c-----------------------------------------------------------------------
      implicit none
      integer n, lun
      double precision wc, p1, p2, zp
      real xline(n), yline(n)
      character*(*) type(3), axis
cc
      double precision inc, wi(3), po(3)
      integer i, naxis
      character typei(3)*6, typeo(3)*6
c-----------------------------------------------------------------------
      inc = (p2-p1)/real(n-1)
      do i = 1, 3
        typei(i) = type(i)
        typeo(i) = 'abspix'
      end do
      call rdhdi (lun, 'naxis', naxis, 0)
      naxis = min(3,naxis)
c
      if (axis.eq.'x') then
c
c Draw vertical line at constant x
c
        wi(1) = wc
        wi(2) = p1
        typei(2) = 'abspix'
      else if (axis.eq.'y') then
c
c Draw horizontal line at constant y
c
        wi(1) = p1
        wi(2) = wc
        typei(1) = 'abspix'
      else
        call bug ('f', 'DRWLINCG: unrecognized axis')
      end if
      wi(3) = zp
c
      do i = 1, n
        call w2wco (lun, naxis, typei, ' ', wi, typeo, ' ', po)
c
        xline(i) = po(1)
        yline(i) = po(2)
c
        if (axis.eq.'x') then
          wi(2) = wi(2) + inc 
        else
          wi(1) = wi(1) + inc
        end if
      end do
      call pgline (n, xline, yline)
c
      end
c
c
      subroutine drwtikcg (axis, opts, tickd, nsub, ticklp, typeo,
     +                     lun, axmin, axmax, blcd, trcd, zp)
c-----------------------------------------------------------------------
c     Write on the plot ticks or grid.  If a grid is requested,
c     no minor ticks are drawn.  These ticks/grid are correct
c     for a non-linear coordinate system too.
c
c  Input 
c    axis      Indicate which axis we are ticking; 'x' or 'y'
c    opts      Options string
c    tickd     Major tick interval 
c    nsub      Number of subintervals between major ticks
c    ticklp    Length of tick in pixels
c    typeo     Coordinate conversion types matching LABTYP
c    lun       Handle for coordinate conversions
c    axmin,max Axis min and max in world according to LABTYP
c    blc,trcd  Orthogonal axis min and max in absolute pixels
c    zp        Absolute pixel of third axis appropriate to this image
c-----------------------------------------------------------------------
      implicit none
      integer lun, nsub
      character axis*1, opts*(*), typeo(3)*6
      double precision tickd, ticklp, axmin, axmax, blcd, trcd, zp
cc
      integer maxpts
      parameter (maxpts = 100)
      real xline(maxpts), yline(maxpts)
      integer ip, i, lw
      double precision ax1, axx, axxx, tinc
      logical firstt
c-----------------------------------------------------------------------
      call pgbbuf
c
c Save PGPLOT line width
c
      call pgqlw (lw)
c
c Find start tick
c
      ip = int(axmin/abs(tickd))
      if (axmin.ge.0.0 .and. (axmin-ip*tickd).ne.0.0) ip = ip + 1
      ax1 = ip * tickd
c
c Loop over all x ticks
c
      firstt = .true.
      axx = ax1
      do while (axx.le.axmax)
        if (index(opts,'G').ne.0) then
c
c Draw line of constant x or y coordinate (thin lines)
c
          call pgslw (1)
          call drwlincg (lun, axis, typeo, maxpts, axx, zp, blcd,
     +                   trcd, xline, yline)
          call pgupdt
          call pgslw (lw)
        else if (index(opts,'T').ne.0) then
c
c Draw major ticks (top/bottom for x, right/left for y)
c
c          if (axis.eq.'x') then
c            write(*,*) 'axis=',axx
c          endif
          call drwlincg (lun, axis, typeo, 2, axx, zp, blcd,
     +                   blcd+ticklp, xline, yline)
          call drwlincg (lun, axis, typeo, 2, axx, zp, trcd-ticklp,
     +                   trcd, xline, yline)
          call pgupdt
        end if
c
c Draw minor ticks, but only if no grid. Remember the minor ticks
c in the interval between the frame and the first major tick
c
        if (index(opts,'G').eq.0 .and. index(opts,'S').ne.0) then
          if (nsub.gt.1) then
            tinc = (tickd/dble(nsub))
c
            if (firstt) then
              axxx = axx - tickd
              do i = 1, nsub-1
                axxx = axxx + tinc
                call drwlincg (lun, axis, typeo, 2, axxx, zp, blcd,
     +                         blcd+ticklp/2, xline, yline)
                call drwlincg (lun, axis, typeo, 2, axxx, zp, 
     +                         trcd-ticklp/2, trcd, xline, yline)
                call pgupdt
              end do
            end if
c
            axxx = axx
            do i = 1, nsub-1
              axxx = axxx + tinc
              call drwlincg (lun, axis, typeo, 2, axxx, zp, blcd,
     +                       blcd+ticklp/2, xline, yline)
              call drwlincg (lun, axis, typeo, 2, axxx, zp, 
     +                       trcd-ticklp/2, trcd, xline, yline)
              call pgupdt
            end do
          end if
        end if
c
c Increment tick location
c
        axx = axx + tickd
        firstt = .false.
      end do
      call pgebuf
c
      end
c
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
c
c* lab3CG -- Label sub-plot with value and/or pixel of 3rd axis
c& nebk
c: plotting
c+
      subroutine lab3cg (lun, doerase, doval, dopix, labtyp, ipl, plav)
c
      implicit none
      integer ipl, plav, lun
      logical doval, dopix, doerase
      character*6  labtyp(2)
c
c  Label the plot with the third axis values or pixel or both
c
c  Input
c    lun        Handle of image
c    doerase    .true. to erase the background behind the string
c    doval      .true. if writing value
c    dopix      .true. if writing pixel
c    labtyp     Axis label types
c    ipl        Start plane of image being plotted
c    plav       Number of planes being averaged for current sub-plot
c--
c----------------------------------------------------------------------
      double precision pix, val3
      real mx, my, x1, x2, y1, y2, xb(4), yb(4), dx, dy
      character str1*30, str2*30, str3*60, types(3)*4, ltype*6
      integer i1, i3, is2, ie2
c
      include 'maxnax.h'
      integer len1, naxis
      character*30 hangleh, rangle
c------------------------------------------------------------------------
      call rdhdi (lun, 'naxis', naxis, 0)
      if (naxis.lt.3) return
c
c Prepare pixel label
c
      if (plav.eq.1) then
        call pgnumb (ipl, 0, 0, str1, i1)
        pix = ipl
      else
        pix = (ipl+ipl+plav-1) / 2.0
        call strfmtcg (real(pix), 5, str1, i1)
      end if
c
c Prepare value label.  The units depend upon what type of axis the
c third axis is, and what the units of the complementary axis is
c
      call initco (lun)
      if (doval) then
        call axtypco (lun, 3, 0, types)
        if (types(3).eq.'VELO') then
          ltype = 'abskms'
        else if (types(3).eq.'FREQ') then
          ltype = 'absghz'
        else if (types(3).eq.'UV' .or. types(3).eq.'NONE') then
          ltype = 'absnat'
        else if (types(3).eq.'RA' .or. types(3).eq.'LONG') then
c  
c Look for DEC axis amongst first two and find label type to
c set label type for 3rd axis value
c
          if (types(1).eq.'DEC' .or. types(1).eq.'LATI') then
            ltype = 'hms'
            if (labtyp(1).eq.'arcsec' .or. labtyp(1).eq.'arcmin' .or.
     +          labtyp(1)(4:6).eq.'deg') ltype = labtyp(1)
          else if (types(2).eq.'DEC' .or. types(2).eq.'LATI') then
            ltype = 'hms'
            if (labtyp(2).eq.'arcsec' .or. labtyp(2).eq.'arcmin' .or.
     +          labtyp(2)(4:6).eq.'deg') ltype = labtyp(2)
          end if
        else if (types(3).eq.'DEC' .or. types(3).eq.'LATI') then
c  
c Look for RA axis amongst first two and find label type to
c set label type for 3rd axis value
c
          if (types(1).eq.'RA' .or. types(1).eq.'LONG') then
            ltype = 'dms'
            if (labtyp(1).eq.'arcsec' .or. labtyp(1).eq.'arcmin' .or.
     +          labtyp(1)(4:6).eq.'deg') ltype = labtyp(1)
          else if (types(2).eq.'RA' .or. types(2).eq.'LONG') then
            ltype = 'dms'
            if (labtyp(2).eq.'arcsec' .or. labtyp(2).eq.'arcmin' .or.
     +          labtyp(2)(4:6).eq.'deg') ltype = labtyp(2)
          end if
        end if
c
c Now compute the value of the third axis in the desired units
c and format it
c 
        call w2wsco (lun, 3, 'abspix', ' ', pix, ltype, ' ', val3)
        call finco (lun)
c
c Format value
c
        if (ltype.eq.'hms') then
          str2 = hangleh(val3)
        else if (ltype.eq.'dms') then
          str2 = rangle(val3)
        else 
          if (val3.lt.100.0) then
             call strfmtcg (real(val3), 2, str2, ie2)
          else if (val3.lt.1000.0) then
             call strfmtcg (real(val3), 3, str2, ie2)
          else
             call strfmtcg (real(val3), 6, str2, ie2)
          end if
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
c* naxlabCG -- Draw frame, write numeric labels, ticks and grid
c& nebk
c: plotting
c+
      subroutine naxlabcg (lun, donum, blc, trc, krng, labtyp, 
     +                     donx, dony, nofirst, grid)
      implicit none
c
      character*(*) labtyp(2)
      integer lun, blc(3), trc(3)
      logical donum, nofirst, grid, donx(2), dony(2)
c
c  Draw plot frame, labels and ticks/grid.  Draws correct non-linear
c  ticks/grid for appropriate axes
c
c  Input
c     lun       handle for coordinate conversions
c     donum     If true, this is the first time we have displayed this
c               image so we label with numbers frame and box.  Otherwise
c               we have erased the display and all we want to do is redraw
c               the frame and ticks.  The numbers will not have gone.
c     blc,trc   absolute pixels of blc and trc of selected hypercube
c     krng      first plane and number of planes averaged in this image
c     labtyp    axis label types
c     donx      If donax(1) is true write bottom x-axis label
c               If donax(2) is true write top    x-axis label
c     dony      If donay(1) is true write left  y-axis label
c               If donay(2) is true write right y-axis label
c     nofirst   True if first x axis tick should not be labelled
c     grid      Draw on grid instead of just ticks
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
c
      double precision as2r, st2r
      parameter (as2r=dpi/3600.d0/180.d0, st2r=dpi/3600.d0/12.0d0)
c
      double precision wwi(3), wblc(3), wtrc(3), wbrc(3), wtlc(3),
     + tickd(2), xmin, xmax, ymin, ymax, zp, ticklp(2), dp, dw, 
     + blcd(2), trcd(2)
      real tick(2),  tickl(2), wpix(4)
      integer nxsub, nysub, i, j, krng(2), ip, naxis
      character xopt*20, yopt*20, typei(3)*6, typeo(3)*6
      logical zero(2), dotime(2)
c-----------------------------------------------------------------------
c
c Save pixel window
c
      call pgqwin (wpix(1), wpix(2), wpix(3), wpix(4))
c
c Work out if we have a RA=0 crossing axis
c
      call razerocg (lun, blc, trc, zero)
c
c Absolute pixel of third axis appropriate for this image, and
c work out pixel blc and trc of corners of displayed image.
c
      zp = dble(krng(1)) + dble(krng(2)-1)/2.0d0
      blcd(1) = blc(1) - 0.5d0
      blcd(2) = blc(2) - 0.5d0
      trcd(1) = trc(1) + 0.5d0
      trcd(2) = trc(2) + 0.5d0
c
c Convert spatial coordinates of all 4 corners of the current plane
c from absolute pixels to world coordinates given by label type
c
      call initco (lun)
      call rdhdi (lun, 'naxis', naxis, 0)
      naxis = min(3,naxis)
      do i = 1, naxis
        typei(i) = 'abspix'
        if (i.le.2) typeo(i) = labtyp(i)
      end do
      typeo(3) = 'abspix'
c
      wwi(1) = blcd(1)
      wwi(2) = blcd(2)
      wwi(3) = zp
      call w2wco (lun, naxis, typei, ' ', wwi, typeo, ' ', wblc)
      wwi(1) = trcd(1)
      wwi(2) = trcd(2)
      call w2wco (lun, naxis, typei, ' ', wwi, typeo, ' ', wtrc)
c
      wwi(1) = trcd(1)
      wwi(2) = blcd(2)
      call w2wco (lun, naxis, typei, ' ', wwi, typeo, ' ', wbrc)
      wwi(1) = blcd(1)
      wwi(2) = trcd(2)
      call w2wco (lun, naxis, typei, ' ', wwi, typeo, ' ', wtlc)
c
c Add 2pi to one end if we cross RA=0.  RAD axis units are radians.
c
      if (zero(1)) then
        if (wblc(1).lt.wbrc(1)) then
          wblc(1) = wblc(1) + dpi*2
        else 
          wbrc(1) = wbrc(1) + dpi*2
        end if
        if (wtlc(1).lt.wtrc(1)) then
          wtlc(1) = wtlc(1) + dpi*2
        else 
          wtrc(1) = wtrc(1) + dpi*2
        end if
      end if
      if (zero(2)) then
        if (wblc(2).lt.wtlc(2)) then
          wblc(2) = wblc(2) + dpi*2
        else 
          wtlc(2) = wtlc(2) + dpi*2
        end if
        if (wbrc(2).lt.wtrc(2)) then
          wbrc(2) = wbrc(2) + dpi*2
        else 
          wtrc(2) = wtrc(2) + dpi*2
        end if
      end if
c
c Now convert any RA/DEC angular world coordinates (currently in radians)
c to seconds of time or arc if desired for PGTBOX.  W2WCO will already 
c have checked that the LABTYP is compatible with the CTYPE so no 
c need to do it again
c
      do j = 1, 2
        call angconcg (1, labtyp(j), wblc(j))
        call angconcg (1, labtyp(j), wbrc(j))
        call angconcg (1, labtyp(j), wtlc(j))
        call angconcg (1, labtyp(j), wtrc(j))
      end do
c
c Set new PGPLOT window.  We only use this to work out the ticks so it 
c doesn't matter much that it is still a linear axis. But it must be the
c correct part of the frame to match where the labels will be written
c
      call pgswin (real(wblc(1)), real(wbrc(1)), 
     +             real(wblc(2)), real(wtlc(2)))
c
c Set PGPLOT PGTBOX options strings; we only do bottom/left
c numeric labelling here
c
      xopt = 'BC'
      ip = 3
      if (donum .and. donx(1)) then
        xopt(ip:ip) = 'N'
        ip = ip + 1
c
        if (nofirst) then
          xopt(ip:ip) = 'F'
          ip = ip + 1
        end if
c
        if (zero(1)) then
          xopt(ip:ip) = 'X'
          ip = ip + 1
        end if
c
        if (labtyp(1).eq.'hms') then
          xopt(ip:) = 'ZYHO'
        else if (labtyp(1).eq.'dms') then
          xopt(ip:) = 'ZYDO'
         end if 
      end if
c
      yopt = 'BC'
      ip = 3
      if (donum .and. dony(1)) then
        yopt(ip:ip) = 'N'
        ip = ip + 1
c
        if (zero(2)) then
          yopt(ip:ip) = 'X'
          ip = ip + 1
        end if
c
        if (labtyp(2).eq.'hms') then
          yopt(ip:) = 'ZYHV'
        else if (labtyp(2).eq.'dms') then
          yopt(ip:) = 'ZYDV'
         end if 
      end if
c
c Now draw frame and write only bottom/left numeric labels.  
c
      call pgtbox (xopt, 0.0, 0, yopt, 0.0, 0)
c
c Fish out the tick intervals and number of subintervals
c that PGTBOX or PGBOX would be using if they were drawing
c the ticks.  
c
      nxsub = 0
      nysub = 0
      tick(1) = 0.0
      tick(2) = 0.0
      dotime(1) = .false.
      dotime(2) = .false.
      if (labtyp(1).eq.'hms' .or. labtyp(1).eq.'dms') dotime(1) = .true.
      if (labtyp(2).eq.'hms' .or. labtyp(2).eq.'dms') dotime(2) = .true.
c
      call qtikcg (dotime, tick(1), tick(2), nxsub, nysub, 
     +             tickl(1), tickl(2))
      tickd(1) = abs(tick(1))
      tickd(2) = abs(tick(2))
c
c Convert tick length to pixels (lengths are in linearized
c coordinate system so this is ok).
c
      dp = trc(2) + 0.5 - (blc(2) - 0.5)
      dw = wblc(2) - wtlc(2)
      ticklp(1) = abs(tickl(1) * dp / dw)
      dp = trc(1) + 0.5 - (blc(1) - 0.5)
      dw = wbrc(1) - wblc(1)
      ticklp(2) = abs(tickl(2) * dp / dw)
c
c The experienced and bold user may also wish to label the top and
c right axes as well.  So reset the world coordinate window to reflect
c these axes (because the coordinate system may be nonlinear, these can 
c differ) and label away.  Must use the ticking values already found.
c
      xopt = ' '
      yopt = ' '
      ip = 1
      if (donum .and. donx(2)) then
        xopt(ip:ip) = 'M'
        ip = ip + 1
c
        if (nofirst) then
          xopt(ip:ip) = 'F'
          ip = ip + 1
        end if
c
        if (zero(1)) then
          xopt(ip:ip) = 'X'
          ip = ip + 1
        end if
c
        if (labtyp(1).eq.'hms') then
          xopt(ip:) = 'ZYHO'
        else if (labtyp(1).eq.'dms') then
          xopt(ip:) = 'ZYDO'
         end if 
      end if
c
      ip = 1
      if (donum .and. dony(2)) then
        yopt(ip:ip) = 'M'
        ip = ip + 1
c
        if (zero(2)) then
          yopt(ip:ip) = 'X'
          ip = ip +1
        end if
c
        if (labtyp(2).eq.'hms') then
          yopt(ip:) = 'ZYHV'
        else if (labtyp(2).eq.'dms') then
          yopt(ip:) = 'ZYDV'
         end if 
      end if
c
      if (index(xopt,'M').ne.0 .or. index(yopt,'M').ne.0) then
        call pgswin (real(wtlc(1)), real(wtrc(1)), 
     +               real(wbrc(2)), real(wtrc(2)))
        call pgtbox (xopt, tick(1), nxsub, yopt, tick(2), nysub)
      end if
c
c Find minimum and maximum x and y coordinates from positions of corners
c
      xmin = min(wblc(1),wbrc(1),wtlc(1),wtrc(1))
      xmax = max(wblc(1),wbrc(1),wtlc(1),wtrc(1))
      ymin = min(wblc(2),wbrc(2),wtlc(2),wtrc(2))
      ymax = max(wblc(2),wbrc(2),wtlc(2),wtrc(2))
c
c Set window in absolute pixel space, ready for ticking
c
      call pgswin(real(blcd(1)), real(trcd(1)),
     +            real(blcd(2)), real(trcd(2)))
c
c Draw x ticks/grid.  We have to convert RA/DEC values back to radians
c for the conversion routines.
c
      xopt = ' '
      if (labtyp(1).ne.'none') then
        xopt = 'TS'
        ip = 3
        if (grid) then
          xopt(ip:ip) = 'G'
          ip = ip + 1
        end if
      end if
      call angconcg (2, typeo(1), xmin)
      call angconcg (2, typeo(1), xmax)
      call angconcg (2, typeo(1), tickd(1))
      call drwtikcg ('x', xopt, tickd(1), nxsub, ticklp(1), typeo,
     +               lun, xmin, xmax, blcd(2), trcd(2), zp)
c
c Draw y ticks/grid. We have to convert RA/DEC values back to radians
c for the conversion routines.
c
      yopt = ' '
      if (labtyp(2).ne.'none') then
        yopt = 'TS'
        ip = 3
        if (grid) then
          yopt(ip:ip) = 'G'
          ip = ip + 1
        end if
      end if
      call angconcg (2, typeo(2), ymin)
      call angconcg (2, typeo(2), ymax)
      call angconcg (2, typeo(2), tickd(2))
      call drwtikcg ('y', yopt, tickd(2), nysub, ticklp(2), typeo,
     +               lun, ymin, ymax, blcd(1), trcd(1), zp)
c
c Free coordinate object
c
      call finco (lun)
c
c Restore original pixel window
c
      call pgswin (wpix(1), wpix(2), wpix(3), wpix(4))
c
      end
c
c* setdspCG -- Set label axis displacements
c& nebk
c: plotting
c+
      subroutine setdspcg (lh, labtyp, blc, trc, xdispl, ydispb)
c
      implicit none
      integer lh, blc(2), trc(2)
      character labtyp(*)*(*)
      real xdispl, ydispb
c
c  Set the labelling displacements from the relevant axes
c
c  Input
c    lh       Handle of image
c    labtyp   Label type requested by user
c    blc      blc in pixels
c    trc      trc in pixels
c  Output
c    xdispl   Displacement in character heights from left y-axis 
c             for Y label
c    ydispb   Displacement in character heights from bottom x-axis 
c             for X label
c--
c-------------------------------------------------------------------------------
      include 'mirconst.h'
      double precision ymin, ymax, win(2), wout1(2), wout2(2)
      real dely, xch, ych, xl, yl
      character str*60, stypeo*8, typei(2)*6
      integer len1, il
      logical zero(2)
c-----------------------------------------------------------------------
c
c X axis
c
      if (labtyp(1).eq.'hms') then
        ydispb = 3.6
      else if (labtyp(1).eq.'dms') then
        ydispb = 3.6
      else if (labtyp(1).eq.'none') then
        ydispb = 3.6
      else
        ydispb = 3.1
      end if
c
c Y axis.  Have a stab at a correct axis label displacement when using
c HMS or DMS; it depends upon the number of decimal places in the 
c labels and knowing about the PGTBOX algorithm.  Very modular.
c Allow for space between numeric label and axis, and between
c numeric label and axis label.
c
      if (labtyp(2).eq.'hms' .or. labtyp(2).eq.'dms') then
c
c Work out y min and max in radians
c
        stypeo = ' '
        typei(1) = 'abspix'
        typei(2) = 'abspix'
c        
        call initco (lh)
        win(1) = blc(1) - 0.5
        win(2) = blc(2) - 0.5
        call w2wco (lh, 2, typei, ' ', win, labtyp, stypeo, wout1)
        win(2) = trc(2) + 0.5
        call w2wco (lh, 2, typei, ' ', win, labtyp, stypeo, wout2)
        call finco (lh)
c
c Allow for RA axis zero crossing
c
        call razerocg (lh, blc, trc, zero)
        if (zero(2)) then
          if (wout1(2).lt.wout2(2)) then
            wout1(2) = wout1(2) + 2*dpi
          else
            wout2(2) = wout2(2) + 2*dpi
          end if
        end if
c
c Convert to seconds of time/arc
c
        ymin = wout1(2)
        ymax = wout2(2)
        call angconcg (1, labtyp(2), ymin)
        call angconcg (1, labtyp(2), ymax)
c
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
        xdispl = xl / xch
      else
        if (labtyp(2).eq.'none') then
          xdispl = 1.0
        else
          xdispl = 2.5
        end if
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
c  Format a number with a specified number of significant figures 
c  with the PGPLOT routine pgnumb. It chooses automatically decimal
c  or exponential notation.  Pgplot superscripting escape sequences
c  may be embedded in the string in the latter case.
c
c  Input:
c    xnum    The number = mm * 10**pp
c    ns      Number of desired significant figures
c  Output:
c    str     Formatted string
c    is      Length of string
c--
c-----------------------------------------------------------------------
      integer mm, pp
c-----------------------------------------------------------------------
      if (xnum.ne.0.0) then
        pp = log10(abs(xnum)) - ns
        mm = nint(xnum * (10.0 ** (-pp)))
      else
        mm = 0
        pp = 1
      end if
c
      call pgnumb (mm, pp, 0, str, is)
c
      end
c
c* vpadjCG -- Adjust viewport if equal scales requested
c& nebk
c: plotting
c+
      subroutine vpadjcg (lh, hard, eqscale, scale, vxmin, vymin, vymax,
     +   nx, ny, blc, trc, tfvp, wdgvp, vxsize, vysize)
c
      implicit none
      integer lh, nx, ny, blc(*), trc(*)
      real vxsize, vysize, vxmin, vymin, vymax, scale(2), tfvp(4),
     +  wdgvp(4)
      logical eqscale
      character hard*(*)
c
c  So far everything has been worked out for unequal scales in
c  x and y.  If the user requests equal scales, or gives the scales,
c  we need to make some adjustments to the viewport
c
c  Inputs
c    lh           Handle of image
c    hard         YES for hardcopy device
c    eqscale      True means equals scale requested, else different 
c    nx,ny        Number of sub-plots in x and y
c    blc,trc      Window in pixels
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
      double precision delx, dely, xfac, yfac, xscale, yscale, 
     +  xscale0, yscale0, cdelt1, cdelt2
      real vx1, vx2, vy1, vy2, vxmore, vymore, vxsize2, vysize2
      character aline*72
      logical dofid, dowedge
      integer i
c-----------------------------------------------------------------------
c
c Get image pixel increments
c
      call rdhdd (lh, 'cdelt1', cdelt1, 0.0d0)
      call rdhdd (lh, 'cdelt2', cdelt2, 0.0d0)
c
c Find size of window in linear coordinates (allow for 1/2
c of a pixel at either end).
c
      delx = dble(trc(1) - blc(1) + 1) * abs(cdelt1)
      dely = dble(trc(2) - blc(2) + 1) * abs(cdelt2)
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
c Using default scales; adjust for equal scales if required
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
c allow for scale changes
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
     +  ydispb, gaps, doabut, dotr, wedcod, wedwid, tfdisp, labtyp, 
     +  vxmin, vymin, vymax, vxgap, vygap, vxsize, vysize, 
     +  tfvp, wdgvp)
c
      implicit none
      integer maxlev, nlevs(*), srtlev(maxlev,*), nx, ny, ncon, 
     +  wedcod, nspec
      real vxmin, vymin, vymax, vxgap, vygap, vxsize, vysize, pcs,
     +  ydispb, xdispl,  wedwid, tfvp(4), tfdisp, wdgvp(4),
     +  levs(maxlev,*), slev
      logical dofid, dofull, gaps, dotr, doabut
      character*(*) gin, vin, bin, labtyp(2)*(*)
c
c   Work out view port that encompasses all sub-plots and allows
c   for all labelling outside of it.   Assume unequal scales in x 
c   and y here.  If user wants equal scales, adjust later in VPADJCG
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
c     xdispl      Displacement of y-axis char. label from axis in char hghts
c     ydispb      Displacement of x-axis char. label from axis in char hghts
c     gaps        If true then don't leave gaps between sub-plots else
c                 leave gaps between sub-plots & label each window
c     doabut      No white space at all around subplots
c     dotr        Means as well as labelling plot on left and bottom axes,
c		  also label it at the top and right.
c     wedcod      1 -> one wedge to right of all subplots
c                 2 -> one wedge to right per subplot
c                 3 -> one wedge per subplot inside subplot
c     wedwid      Fraction of full viewport for wedge width (wedcod=1)
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
c Allow for any labels on top frame
c
      if (dotr) vymax = 1.0 - 2.5*yht
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
        if (dotr) then
          dvwd = xdispl * xht
        else
          dvwd  = xht
        end if
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
c Set x trc of image viewport. Allow for any labels on right axis
c
      vxmax = 1.0 - max(dvwx,dvtx) - xht
      if (dotr) vxmax = 1.0 - max(dvwx,dvtx,xdispl*xht)
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
c If labelling top and right as well allow for that
c
      if (nx.gt.1) then
        if (gaps) then
          vxgap = (1.0 + xdispl + 2.0) * xht
          if (dotr) vxgap = vxgap + xdispl*xht
        else
          vxgap = xht/3
          if (doabut) vxgap = 0.0
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
          if (dotr) vygap = vygap + yht
        else
          vygap = yht/3
          if (doabut) vygap = 0.0
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
c* wedgCG -- Draw pixel map wedge in specified viewport
c& nebk
c: plotting
c+
      subroutine wedgcg (label, trfun, groff, nbins, cumhis, wdgvp, 
     +                   a1, a2)
c
      implicit none
      integer nbins
      real wdgvp(4), a1, a2, groff, cumhis(nbins)
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
c  a1     The value which is to appear with shade C1
c  a2     The value which is to appear with shade C2
c         Use the values of A1 and A2 that were sent to PGIMAG except
c         that these values should be those appropriate to before any
c         application of transfer functions (sqr, log, heq) and adding of
c         offsets (GROFF)
c
c
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      real wx1, wx2, wy1, wy2, vx1s, vx2s, vy1s, vy2s, wdginc, tr(6), 
     +  b1, b2
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
        b1 = log10(a1-groff)
        b2 = log10(a2-groff)
      else if (trfun.eq.'sqr') then
        b1 = sqrt(a1-groff)
        b2 = sqrt(a2-groff)
      else if (trfun.eq.'heq') then
        b1 = cumhis(1)
        b2 = cumhis(nbins2)
      else
        b1 = a1
        b2 = a2
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
        wdginc = (a2-a1) / (nbins2-1)
        do i = 1, nbins2
          memr(ipw+i-1) = a1 + (i-1) * wdginc
c
c Apply transfer function
c
          if (trfun.eq.'log') then
            memr(ipw+i-1) = log10(memr(ipw+i-1)-groff)
          else if (trfun.eq.'sqr') then
            memr(ipw+i-1) = sqrt(memr(ipw+i-1)-groff)
          end if
        end do
      end if
c
c Draw the wedge and label
c
      call pgsvp (wdgvp(1), wdgvp(3), wdgvp(2), wdgvp(4))
      call pgswin (0.9, 1.1, 1.0, real(nbins2))
      call pgimag (memr(ipw), 1, nbins2, 1, 1, 1, nbins2, 
     +             b1, b2, tr)
      call pgswin (0.0, 1.0, a1, a2)
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
     +                    cumhis, wdgvp, a1, a2)
c
      implicit none
      real groff, cumhis(*), wdgvp(4), a1, a2, wedwid
      integer wedcod, jj, nbins
      character trfun*3
c
c Work out whether the pixel map wedges are to be drawn inside
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
c  a1,a2  pixel map max and min
c         Use the values of A1 and A2 that were sent to PGGRAY.
c         These values should be those appropriate to before 
c         any application of transfer functions (log etc) and 
c         adding of offsets
c  nx,ny  Number of subplots in x and y directions
c  npixr  NUmber of pixel map "range" groups given by user
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
          call wedgcg (.true., trfun, groff, nbins, cumhis, 
     +                 wdgvp, a1, a2)
        end if
      else if (wedcod.eq.2) then
        wv(1) = vx2 + 1.0*xht
        wv(2) = vy1
        wv(3) = wv(1) + wedfrc
        wv(4) = vy2
        call wedgcg (.true., trfun, groff, nbins, cumhis, 
     +               wv, a1, a2)
      else
        wv(1) = vx2 - wedfrc
        wv(2) = vy1
        wv(3) = vx2
        wv(4) = vy2
        call wedgcg (.false., trfun, groff, nbins, cumhis, 
     +               wv, a1, a2)
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
c
c
      subroutine qtikcg (dotime, xtick, ytick, nxsub, nysub,
     *                   xtickl, ytickl)
c-----------------------------------------------------------------------
c This subroutine works out some things about ticks.  It returns
c the values that PGTBOX/PGBOX will use.    It is currently
c a big fudge and replicates algorithms; in future it will
c be replaced by a not-yet-standard pgplot routine called PGQTIK
c
c Inputs
c  dotime True if time labelling wanted for X or Y axes
c Input/output
c  xtick  The x-tick in seconds
c  ytick  The y-tick in seconds
c  nxsub  The number of minor x-tick intervals
c  nysub  The number of minor y-tick intervals
c	  These are unchanged if non-zero on input
c Outputs
c  xtickl The length of x-major ticks
c  ytickl The length of y-major ticks
c
c-----------------------------------------------------------------------
      implicit none
      real xtick, ytick, xtickl, ytickl
      integer nxsub, nysub
      logical dotime(2)
cc
      integer tscalex, tscaley, nsub
      real xmin, xmax, ymin, ymax, vx1, vx2, vy1, vy2, xsp, ysp, 
     +  xlen, ylen
      real pgrnd
c-----------------------------------------------------------------------
c
c Find window in world coordinates
c
      call pgqwin (xmin, xmax, ymin, ymax)
c
c Find view port in absolute device coordinates
c
      call pgqvp (3, vx1, vx2, vy1, vy2)
c
c Find character spacing in x an dy directions.  Fudge factor
c from empirical determination, as pgqcs returns xsp=ysp=ysp
c
      call pgqcs (3, xsp, ysp)
      xsp = 10.0 * ysp / 13.0
      xlen = vx2 - vx1
      ylen = vy2 - vy1
c
c Now work out the tick stuff for the X axis
c
      if (dotime(1)) then
c
c For time labelling code stolen from PGTBX1-3
c
        call pgtbx1cg ('X', .false., .true., xmin, xmax, xtick,
     +                 nxsub, tscalex)
      else
c
c For non-time labelling, code stolen from PGBOX
c
        if (xtick.eq.0.0) then
c
c No tick given
c
          xtick = max(0.05, min(7.0*xsp/xlen, 0.20))*(xmax-xmin)
          xtick = pgrnd(xtick,nsub)
c
c Number of subintervals may be given but tick not
c
          if (nxsub.eq.0) nxsub = nsub
        else
c
c Cheap default for number of subintervals
c
          if (nxsub.eq.0) nxsub = 2
        end if
      end if
      xtickl = xsp*0.6*(ymax-ymin)/ylen
c
c Now work out the tick stuff for the Y axis
c
      if (dotime(2)) then
        call pgtbx1cg ('Y', .false., .false., ymin, ymax, ytick,
     +                 nysub, tscaley)
      else 
        if (ytick.eq.0.0) then
          ytick = max(0.05, min(7.0*xsp/ylen, 0.20))*(ymax-ymin)
          ytick = pgrnd(ytick,nsub)
          if (nysub.eq.0) nysub = nsub
        else
          if (nysub.eq.0) nysub = 2
        end if
      end if
      ytickl = xsp*0.6*(xmax-xmin)/xlen
c
      end
c
c
      subroutine pgtbx1cg (axis, doday, dopara, tmin, tmax, tick, 
     +                     nsub, tscale)
c
c
c Work out what the finest units the time labels will be in and
c return the tick increments if the user does not set them.
c
c This is a support routine for PGTBOX and should not 
c be called by the user.
c
c Input:
c  axis   :  'x' or 'y' for use in determining if labels overwrite
c  tmin   :  Start time in seconds 
c  tmax   :  End   time in seconds
c  doday  :  If True write day field DD HH MMSS
c  dopara :  True if labels parallel to axis
c Input/output:
c  tick   :  Major tick interval in seconds.  If 0.0 on input, will 
c            be set here.
c  nsub   :  Number of minor ticks between major ticks. If 0 on input
c            will be set here.
c Outputs:
c  tscale :  Determines finest unit of labelling 
c            (1 => ss, 60 => mm, 3600 => hh, 3600*24 => dd)
c
c-----------------------------------------------------------------------
      implicit none
      real tmin, tmax, tick
      integer nsub, tscale
      logical doday, dopara
      character axis*1
cc
      integer nlist1, nlist2, nlist3, nlist4, nticmx
      parameter (nlist1 = 19, nlist2 = 10, nlist3 = 6, nlist4 = 8,
     *           nticmx = 8)
c
      real ticks1(nlist1), ticks2(nlist2), ticks3(nlist3), 
     *ticks4(nlist4), tock, tock2, tint, tints, tmins, tmaxs
      integer nsubs1(nlist1), nsubs2(nlist2), nsubs3(nlist3), 
     *nsubs4(nlist4), npl, ntick, itick, strlen
      character str*15
c
      save ticks1, ticks2, ticks3, ticks4
      save nsubs1, nsubs2, nsubs3, nsubs4
c
      data ticks1 /0.001,  0.002,                 0.005,
     *             0.01,   0.02,                  0.05,  
     *             0.1,    0.2,                   0.5,  
     *             1.0,    2.0,   3.0,    4.0,    5.0,
     *             6.0,   10.0,  15.0,   20.0,   30.0/
      data nsubs1 / 4,      4,                     2,    
     *              4,      4,                     2,    
     *              4,      4,                     2,    
     *              4,      4,     3,      4,      5,
     *              3,      2,     3,      2,      3/
c
      data ticks2 /1.0,    2.0,   3.0,    4.0,    5.0,
     *             6.0,   10.0,  15.0,   20.0,   30.0/
      data nsubs2 / 4,      4,     3,      4,      5,
     *              3,      2,     3,      2,      3/
c
      data ticks3 /1.0,    2.0,   3.0,    4.0,    6.0,   12.0/
      data nsubs3 / 4,      4,     3,      4,      3,      2/
c
      data ticks4 /1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0, 9.0/
      data nsubs4 / 4,   4,   3,   4,   5,   3,   4,   3 /
c----------------------------------------------------------------------
      tint = abs(tmax - tmin)
      tick = abs(tick)
c
c If a tick size is provided, use it to determine TSCALE
c
      if (tick.ne.0.0) then
        if (tick.ge.tint) then
          call bug ('w', 'PGTBX1CG: user given tick bigger than time '
     *                 //'interval; will auto-tick')
          tick = 0.0
        else if (tick.lt.0.001) then
          call bug ('w', 'PGTBX1CG: user given tick too '
     *                 //'small (< 1 ms); will auto-tick')
          tick = 0.0
        else
          if (mod(tick, 60.0) .ne. 0.0) then
            tscale = 1
          else if (mod(tick, 3600.0).ne.0.0) then
            tscale = 60  
          else if (.not.doday) then
            tscale = 3600   
          else if (mod(tick,(24.0*3600.0)).ne.0.0) then
            tscale = 3600
          else
            tscale = 24 * 3600
          endif
c 
c Make a simple default for the number of minor ticks and bug out
c 
          if (nsub.eq.0) nsub = 2
          return
        end if
      end if
c
c Work out label units depending on time interval if user 
c wants auto-ticking
c
      if (tint.le.5*60) then
        tscale = 1
      else if (tint.le.5*3600) then
        tscale = 60
      else 
        if (.not.doday) then
          tscale = 3600
        else
          if (tint.le.5*24*3600) then
            tscale = 3600
          else
            tscale = 3600*24
          end if
        end if
      end if
c
ccccc
c Divide interval into ntick major ticks and nsub minor intervals
c the tick choosing algorithm is not very robust, so watch out
c if you fiddle anything. 
ccccc
c
      tints = tint / tscale
      if (tscale.eq.1) then
c
c Time in seconds.  if the time interval is very small, may need to 
c label with up to 3 decimal places.  have less ticks to help prevent
c label overwrite. str is a dummy tick label to assess label 
c overwrite potential
c
        if (dopara) then
          if (tints.le.0.01) then
            ntick = 4
            str = '60.423'
            strlen = 6
          else if (tints.le.0.1) then
            ntick = 5
            str = '60.42'
            strlen = 5
          else if (tints.le.1.0) then
            ntick = 6
            str = '60.4'
            strlen = 4
          else
            ntick = 6
            str = '60s'
            strlen = 3
          end if
        else
          ntick = 6
          str = ' '
          strlen = 1
        end if
        tock = tints / ntick
c
c Select nearest tick to tock from list.
c
        call pgtbx2cg (tock, nlist1, ticks1, nsubs1, tick, nsub, itick)
c
c Check label overwrite and/or too many ticks.
c
        call pgtbx3cg (doday, 0, tscale, tints, nticmx, nlist1, ticks1,
     *               nsubs1, itick, axis, dopara, str(1:strlen),
     *               tick, nsub)
      else if (tscale.eq.60) then
c
c Time in minutes 
c
        ntick = 6
        tock = tints / ntick
c
c Select nearest tick from list
c
        call pgtbx2cg (tock, nlist2, ticks2, nsubs2, tick, nsub, itick)
c
c Check label overwrite and/or too many ticks.
c
        if (dopara) then
          str = '42m'
          strlen = 3
        else
          str = ' '
          strlen = 1
        end if
        call pgtbx3cg (doday, 0, tscale, tints, nticmx, nlist2, ticks2,
     *               nsubs2, itick, axis, dopara, str(1:strlen),
     *               tick, nsub)
      else 
        if (tscale.eq.3600 .and. doday) then
c
c Time in hours with the day field 
c
          ntick = 6
          tock = tints / ntick
c
c Select nearest tick from list
c
          call pgtbx2cg (tock, nlist3, ticks3, nsubs3, tick, 
     +                   nsub, itick)
c
c Check label overwrite and/or too many ticks.
c
          if (dopara) then
            str = '42h'
            strlen = 3
          else
            str = ' '
            strlen = 1
          end if
          call pgtbx3cg (doday, 0, tscale, tints, nticmx, nlist3, 
     *                   ticks3, nsubs3, itick, axis, dopara, 
     *                   str(1:strlen), tick, nsub)
        else
c
c Time in hours with no day field or time in days. have less
c ticks for big numbers or the parallel labels will overwrite.
c
          if (dopara) then
            tmins = abs(tmin) / tscale
            tmaxs = abs(tmax) / tscale            
            call pgnpl (-1, nint(max(tints,tmins,tmaxs)), npl)
            if (npl.le.3) then
              ntick = 6
            else if (npl.eq.4) then
              ntick = 5
            else
              ntick = 4
            end if
            str = '345678912'
            str(npl+1:) = 'd'
            strlen = npl + 1
          else
            str = ' '
            strlen = 1
            ntick = 6
          end if
          tock = tints / ntick
c
c Select nearest tick from list; 1 choose nearest nice integer 
c scaled by the appropriate power of 10
c
          call pgnpl (-1, nint(tock), npl)
          tock2 = tock / 10**(npl-1)
c
          call pgtbx2cg (tock2, nlist4, ticks4, nsubs4, tick, 
     +                   nsub, itick)
          tick = tick * 10**(npl-1)
c
c  check label overwrite and/or too many ticks.
c
          call pgtbx3cg (doday, npl, tscale, tints, nticmx, nlist4, 
     *                 ticks4, nsubs4, itick, axis, dopara,
     *                 str(1:strlen), tick, nsub)
        end if
      end if
c
c  convert tick to seconds
c
      tick = tick * tscale
c
      return
      end
c
c
      subroutine pgtbx2cg (tock, nticks, ticks, nsubs, tick, 
     *                     nsub, itick)
c-----------------------------------------------------------------------
c Find the nearest tick in a list to a given value.
c
c This is a support routine for PGTBOX and should not be called
c by the user.
c
c Input:
c  tock   :  Try to find the nearest tick in the list to TOCK
c  nticks :  Number of ticks in list
c  ticks  :  List of ticks
c  nsubs  :  List of number of minor ticks between ticks to go with TICKS
c Output:
c  tick   :  The selected tick
c  itick  :  The index of the selected tick from the list TICKS
c Input/output
c  nsub   :  Number of minor ticks between major ticks. If 0 on input
c            will be set here.
c
c-----------------------------------------------------------------------
      implicit none
      integer nticks, nsubs(nticks), nsub, itick
      real tock, ticks(nticks), tick
cc
      integer i, nsubd
      real dmin, diff
c----------------------------------------------------------------------
      nsubd = nsub
      dmin = 1.0e30
      do 100 i = 1, nticks
        diff = abs(tock - ticks(i))
        if (diff.lt.dmin) then
          tick = ticks(i)
          if (nsubd.eq.0) nsub = nsubs(i)
          itick = i
c
          dmin = diff
        end if
 100  continue
c
      return
      end
c
c
      subroutine pgtbx3cg (doday, npl, tscale, tints, nticmx, nticks,
     *                   ticks, nsubs, itick, axis, dopara, str,
     *                   tick, nsub)
c-----------------------------------------------------------------------
c Try to see if label overwrite is going to occur with this tick 
c selection, or if there are going to be more than a reasonable
c number of ticks in the displayed time range.  If so, choose, 
c if available, the next tick (bigger separation) up in the list.
c If the overwrite requires that we would need to go up to the bext
c TSCALE, give up.  They will need to choose a smaller character size
c
c This is a support routine for PGTBOX and should not 
c be called by the user.
c
c Input:
c  doday  :  True if day field being used
c  npl    :  Number of characters needed to format TICK on input
c  tscale :  Dictates what the finest units of the labelling are.
c            1 = sec, 60 = min, 3600 = hr, 24*3600 = days
c  tints  :  Absolute time interval in units of TSCALE
c  nticmx :  Max. reasonable number of ticks to allow in the time range
c  nticks :  Number of ticks in list of ticks to choose from
c  ticks  :  List of ticks from which the current tick was chosen
c  nsubs  :  List of number of minor ticks/major tick to choose NSUB from
c  itick  :  Index of chosen tick in list TICKS
c  axis   :  'x' or 'y' axis
c  dopara :  Labels parallel or perpendicualr to axis
c  str    :  A typical formatted string used for checking overwrite
c Input/output:
c  tick   :  Current major tick interval in units of TSCALE. May be 
c            made larger if possible if overwrite likely.
c  nsub   :  Number of minor ticks between major ticks. 
c
c-----------------------------------------------------------------------
      implicit none
      integer tscale, nticmx, nticks, itick, nsub, nsubs(nticks), npl
      real tints, ticks(nticks), tick
      character axis*1, str*(*)
      logical doday, dopara
cc
      integer ntick
      real lens, lenx, leny
c----------------------------------------------------------------------
      call pglen (4, str, lenx, leny)
      lens = lenx
      if ( (dopara .and. axis.eq.'Y') .or.
     *     (.not.dopara .and. axis.eq.'X') ) lens = leny
c
      if (tscale.eq.1 .or. tscale.eq.60 .or.
     *    (tscale.eq.3600 .and. doday)) then
c
c  time in seconds or minutes, or in hours with a day field
c
        ntick = int(tints / tick)
        if ( (itick.lt.nticks)  .and. 
     *       ((dopara .and. (lens/tscale).gt.0.9*tick) .or. 
     *       (ntick.gt.nticmx)) ) then
          if (ticks(itick+1).lt.tints) then
            nsub = nsubs(itick+1)
            tick = ticks(itick+1)
          end if
        end if
      else
c
c Time in hours and no day field or time in days
c
        ntick = int(tints / tick)
        if ( (dopara .and. (lens/tscale).gt.0.9*tick) .or. 
     *       (ntick.gt.nticmx) ) then
          if (itick.lt.nticks) then
            if (ticks(itick+1)*10**(npl-1).lt.tints) then
              nsub = nsubs(itick+1)
              tick = ticks(itick+1) * 10**(npl-1)
            end if
          else
            if (ticks(1)*10**npl.lt.tints) then
              nsub = nsubs(1)
              tick = ticks(1) * 10**npl
            end if
          end if
        end if
      end if
c
      return
      end


