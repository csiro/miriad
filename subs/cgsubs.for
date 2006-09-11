c**********************************************************************
c     A collection of subroutines shared by the programs CGDISP,
c     CGSPEC, CGCURS, CGSLICE, IMPOS, MAXFIT, REGRID and UVSUB. All 
c     new additions should end in "cg" 
c
c
c  apptrfcg :  Apply transfer function to image
c  axabscg  :  Return an "abs" axis label type for given generic axis type
c  axfndcg  :  Find a generic axis type
c  axtypcg  :  Returns generic axis label type of given axis
c  chkdescg :  COmpare a real axis descriptor from two images
c  chkdimcg :  Check image dimensions acceptable
c  chnselcg :  Make list of CHAN and REGION selected channel groups
c  conlevcg :  Compute contour levels
c  copyimcg :  Copy image
c  cosdeccg :  Find cos(DEC) for  RA axis
c  defchrcg :  Give a default char. height for axis & velocity labels
c  grfixcg  :  Fix up a grey scale range (includes log taking)
c  hedinfcg :  Get some header information from image
c  heqcg    :  Histogram equalize an image
c  limitscg :  Work out limits and transformation matrix for both axes
c  limtrcg  :  Work out limits and transformation matrix for one axis
c  maskorcg :  OR mask of mask image with mask of data image
c  matchcg  :  Match string with allowed types
c  nxnycg   :  Work out number of sub-plots per page
c  opimcg   :  Open an image and return axis descriptors
c  optcg    :  Version of BobS options, but without the fatalities
c  otopixcg :  Convert overlay location in specified coords to pixels
c  pix2wcg  :  Convert from image pixels to world coord of requested type
c  pix2wfcg :  Convert from image pixels to world coord of requested type
c              and format in string
c  pixi2wcg :  Find pixel increment in world coordinates
c  ppconcg  :  Convert between unbinned full image pixels and binned
c              subimage pixels
c  readbcg  :  Read blanking mask form mask image
c  readimcg :  Read in image dealing with averaging and blanking
c  setcolcg :  Set a PGPLOT colour for multiple line graphics on 1 plot
c  setdescg :  Set axis descriptors for an image by copying from another
c  strprpcg :  Prepare string by stripping extra white space and
c              delimitering fields by commas
c  subinccg :  Step to next sub-plot
c  sunitcg  :  Set pixel units base upon requested label type
c  wedgincg :  Work out if greys cale wedges inside ro outside subplots
c  windfidcg:  Adjust window size to fit an integral number of bins
c  w2pixcg  :  Convert from world coord of given type to image pixels
c  w2wcg    :  Convert between world coord types
c  w2wfcg   :  Convert between world coord types and format
c
c  History:
c     nebk   20sep91     Created from PGDISP/PGCURS
c     nebk   08nov91     Use local blc,trc in call to BOXRUNS in
c                        CHNSELPG, because they may get modified
c                        if blanked pixels exist
c     nebk   28apr92     "g" format seems to behave capriciously
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
c     nebk   24dec92     Split cgpgsubs.for off.
c     rjs    04jan93     Make def. args in rdhdd double in HEDINFCG
c     nebk   27feb93     Earn brownie points with Mr. T by reformatting
c                        subroutine call sequence variables.
c     nebk   21apr93     Add AXTYPCG
c     nebk   27may93     Add 0.5 pixel to blc,trc in LIMTRCG so that
c                        image edges are at n.5,m.5 not n,m
c     nebk   29may93     Remove CHTONVCG as now there is PGQCS 
c     nebk   02jun93     Move ANNDEFCG, VPASPCG and VPADJCG to 
c                        CGPGSUBS.FOR as they now call PGQVSZ
c     nebk   22jun93     Change PIX2WCG, W2PIXCG, LIMTRCG for RA axes to
c			 return abslin/rellin in rads of polar rotation
c                        Add COSDECCG and use it.
c     nebk   05jul93     MAXDIM-> MAXNAX in W2PIXCG.  Whoops !
c     nebk   25aug93     Remove DEGHMSCG in favour of new DANGLEH
c			 Add "absdeg" and "reldeg" axis label types
c                        Add PIXI2WCG.   Add DOEPOCH to LIMTRCG
c     nebk   15sep93     Format change in OPTCG
c     nebk   14nov93     Add 'O' to x-axis options for PGTBOX in SETLAB
c                        Add labtyp=none
c     nebk   07dec93     Add 'V' to y-axis for new PGTBOX in SETLABCG &
c                        modify slightly blanking in READIMCG & TAKLOGCG
c     nebk   10dec93     Add MASKORCG and READBCG
c     nebk   14dec93     Add AXABSCG and LIMITSCG
c     nebk   03jan94     Add new argument to OMATCHCG and rename MATCHCG
c		         Add SETCOLCG
c     nebk   09jan94     Convert CRPIX -> double precision. 
c			 Add W2WCG, W2WFCG
c     nebk   27jan94     Add square root transfer function to GRFIXCG
c                        Rename TAKLOG to APPTRF and include h.e., log,
c			 and square root transfer functions. Add HEQCG
c     nebk   15feb94     Add WEDGINCG and WEDGECG
c     nebk   02mar94     SETLABCG -> cgpgsubs.for
c     nebk   08mar94     Move WEDGECG to CGPGSUBS.FOR, add WINDFIDCG
c                        Implement spatial binning in READBCG, READIMCG,
c                        LIMITSCG and LIMTRCG. Add COPYIMCG, PPCONCG
c     nebk   09jun94     Recognize UU and VV image axes
c     nebk   21jun94     Add OPIMCG 
c     nebk   12jul94     Fix dimensioning bug in SETDESCG
c     nebk   19jul94     Allow roundoff tolerance in CHKDESCG
c***********************************************************************
c
c* apptrfCG -- Apply transfer function to image
c& nebk
c: plotting
c+
      subroutine apptrfcg (pixr, trfun, groff, size, nimage, image,
     +                     nbins, his, cumhis)
c
      implicit none
      integer nimage(*), size, nbins, his(nbins)
      real groff, image(*), pixr(2), cumhis(*)
      character trfun*3
c
c  Take the log of an image with an offset added
c
c  Input:
c   pixr     Intensity range with bias and logs/sqrt taken if necessary
c   trfun    Transfer function.  "lin", "log", "heq" or "sqr"
c   groff    Bias to make image positive if necessary
c   size     Size of image
c   nimage   Normalization image
c   nbins    Number of bins for histogram equalization
c  Input/output:
c   image    Image.  Log taken on output
c   his      Image histogram for histogram equalization
c   cumhis   Cumulative histogram for histogram equalization
c            Values for each bin are the intensities assigned to 
c            the image.  Thus if an image pixel ended up in 
c            cumhis bin idx, then its new value is cumhis(idx)
c            
c--
c---------------------------------------------------------------------
      integer i
c---------------------------------------------------------------------
      if (trfun.eq.'log') then
        do i = 1, size
          if (nimage(i).ne.0) image(i) = log10(image(i) + groff)
        end do
      else if (trfun.eq.'sqr') then
        do i = 1, size
          if (nimage(i).ne.0) image(i) = sqrt(image(i) + groff)
        end do
      else if (trfun.eq.'heq') then
        call heqcg (pixr, size, nimage, image, nbins, his, cumhis)
      end if
c
      end
c
c
c* axabsCG -- Give generic "abs" axis label type for generic axis type
c& nebk
c: plotting
c+
      subroutine axabscg (naxis, gentyp, abstyp)
c
      implicit none
      integer naxis
      character*(*) gentyp(naxis), abstyp(naxis)
c
c  For a given generic axis type, return a sensible absolute axis label type
c
c  Input
c    naxis   Number of axes
c    gentyp  Array of generic axis type as given by AXTYPCG
c  Output
c    abstyp  Array of absolute axis label types
c           
c             RA           -> hms
c             DEC          -> dms
c             LATI, LONG   -> absdeg
c             VELO         -> abskms
c             FREQ         -> absghz
c             UV           -> abslin
c             otherwise    -> abslin
c--
c-----------------------------------------------------------------
      integer i
      character lgtype*8
c-----------------------------------------------------------------
      do i = 1, naxis
        lgtype = gentyp(i)
        call ucase(lgtype)
c
        if (lgtype.eq.'RA') then
          abstyp(i) = 'hms'
        else if (lgtype.eq.'DEC') then
          abstyp(i) = 'dms'
        else if (lgtype.eq.'LONG' .or. lgtype.eq.'LATI') then
          abstyp(i) = 'absdeg'
        else if (lgtype.eq.'VELO') then
          abstyp(i) = 'abskms'
        else if (lgtype.eq.'FREQ') then
          abstyp(i) = 'absghz'
        else if (lgtype.eq.'UV') then
          abstyp(i) = 'abslin'
        else
          abstyp(i) = 'abslin'
        end if
      end do
c
      end
c
c* axfndCG -- Find a specified generic axis in an image
c& nebk
c: plotting
c+
      subroutine axfndcg (type, naxis, ctype, iax)
c
      implicit none
      integer naxis, iax
      character*(*) type, ctype(naxis)
c
c  Find generic axis type in image.
c
c  Input
c    type   Generic axis type to find in axis string.  The first axis
c	    encountered that has this type is returned.  The type
c           should be one of:
c           
c             RA   ->  RA, LL, ELON, GLON
c             DEC  ->  DEC, MM, ELAT, GLAT
c             LONG ->  ELON, GLON
c             LATI ->  ELAT, GLAT
c             VELO ->  VELO, FELO
c             FREQ ->  FREQ
c             UV   ->  UU, VV
c             RAD  ->  An axis whose increment should be in
c		       radians.  These are RA, DEC, LAT, LONG axes
c                      as described by the LHS above.  
c           Other types are searched for exactly as specified
c    naxis  Number of axes to search
c    ctype  Axis types
c  Output
c    iax    Axis number that matches "type".  0 if not present
c--
c-----------------------------------------------------------------
      integer i
      character ltype*8, lctype*8
c-----------------------------------------------------------------
      ltype = type
      call ucase (ltype)	
      iax = 0
c
      do i = 1, naxis
        lctype = ctype(i)
        call ucase (lctype)
c
        if (ltype.eq.'RA') then
          if (index(lctype,'RA').ne.0 .or.
     +        index(lctype,'LL').ne.0) then
            iax = i
            return
          end if
        else if (ltype.eq.'DEC') then
          if (index(lctype,'DEC').ne.0 .or.
     +        index(lctype,'MM').ne.0) then
            iax = i
            return
          end if
        else if (ltype.eq.'LONG') then
          if (index(lctype,'ELON').ne.0 .or.
     +        index(lctype,'GLON').ne.0) then
            iax = i 
            return
          end if
        else if (ltype.eq.'LATI') then
          if (index(lctype,'ELAT').ne.0 .or.
     +        index(lctype,'GLAT').ne.0) then
            iax = i
            return
          end if
        else if (ltype.eq.'VELO') then
          if (index(lctype,'VELO').ne.0 .or.
     +        index(lctype,'FELO').ne.0) then
            iax = i 
            return
          end if
        else if (ltype.eq.'FREQ') then
          if (index(lctype,'FREQ').ne.0) then
            iax = i
            return
          end if
        else if (ltype.eq.'RAD') then
          if (index(lctype,'RA').ne.0 .or.
     +        index(lctype,'LL').ne.0 .or.
     +        index(lctype,'DEC').ne.0 .or.
     +        index(lctype,'MM').ne.0 .or.
     +        index(lctype,'ELON').ne.0 .or.
     +        index(lctype,'GLON').ne.0 .or.
     +        index(lctype,'ELAT').ne.0 .or.
     +        index(lctype,'GLAT').ne.0) then
            iax = i
            return
          end if
        else if (ltype.eq.'UV') then
          if (index(lctype,'UU').ne.0 .or.
     +        index(lctype,'VV').ne.0) then
            iax = i
            return
          end if
        else
          if (index(ctype(i),type).ne.0) then
            iax = i
            return
          end if
        end if
      end do
c
      end
c
c* axtypCG -- Return generic axis type for specified axes
c& nebk
c: plotting
c+
      subroutine axtypcg (naxis, ctype, type)
c
      implicit none
      integer naxis
      character*(*) ctype(naxis)
      character*4 type(naxis)
c
c  Return a generic axis type for each axis type.   
c
c  Input
c    naxis    Number of axes to consider
c    ctype    Array of axis type descriptors
c  Output
c    type     Array of generic axis types describing each axis
c             The generic names returned are one of 
c                 RA, DEC, LATI, LONG, VELO, FREQ, UV, NONE  where
c
c             RA   means CTYPE was one of   RA, LL
c             DEC  means CTYPE was one of   DEC, MM
c             LONG means CTYPE was one of   ELON, GLON
c             LATI means CTYPE was one of   ELAT, GLAT
c             VELO means CTYPE was one of   VELO, FELO
c             FREQ means CTYPE was one of   FREQ
c             UV   means CTYPE was one of   UU, VV
c             NONE means CTYPE was not recognized
c             
c--
c-----------------------------------------------------------------
      integer i
      character lctype*8
c-----------------------------------------------------------------
      do i = 1, naxis
        lctype = ctype(i)
        call ucase (lctype)
c
        if (index(lctype,'RA').ne.0 .or.
     +      index(lctype,'LL').ne.0) then
          type(i) = 'RA'
        else if
     +     (index(lctype,'DEC').ne.0 .or.
     +      index(lctype,'MM').ne.0) then
          type(i) = 'DEC'
        else if 
     +     (index(lctype,'ELON').ne.0 .or.
     +      index(lctype,'GLON').ne.0) then
          type(i) = 'LONG'
        else if
     +     (index(lctype,'ELAT').ne.0 .or.
     +      index(lctype,'GLAT').ne.0) then
          type(i) = 'LATI'
        else if
     +     (index(lctype,'VELO').ne.0 .or.
     +      index(lctype,'FELO').ne.0) then
          type(i) = 'VELO'
        else if (index(lctype,'FREQ').ne.0) then
          type(i) = 'FREQ'
        else if
     +     (index(lctype,'UU').ne.0 .or.
     +      index(lctype,'VV').ne.0) then
          type(i) = 'UV'
        else
          type(i) = 'NONE'
        end if
      end do
c
      end
c
c* chkdesCG -- Compare a double precision axis descriptor from two images
c& nebk
c: plotting
c+
      subroutine chkdescg (relax, type, iaxis, im1, im2, des1, des2)
c
      implicit none
      character type*(*), im1*(*), im2*(*)
      integer iaxis
      double precision des1, des2
      logical relax
c
c  Compare a double precision axis descriptor from two images
c
c  Input:
c    type    Type of descriptor
c    iaxis   Axis number
c    im1,2   Images
c    des1,2  Descriptors
c--
c-----------------------------------------------------------------------
      double precision desmax
      character line*130
      integer len1
c-----------------------------------------------------------------------
      desmax = max(abs(des1),abs(des2))
      if (abs(des1-des2).gt.desmax*1.0d-6 .or. des1*des2.lt.0.0d0) then
        write (line, 10) type, im1(1:len1(im1)), im2(1:len1(im2)),
     +                   iaxis
10      format ('CHKDESCG: Unequal ', a, ' for images ', a, ' & ', a,
     +          ' on axis ', i1)
        if (relax) then
          call bug ('w', line)
        else
          call bug ('i', 
     +       'CHKDESCG: You might consider, with care, OPTIONS=RELAX')
          call bug ('f', line)
        end if
      end if
c
      end
c
c* chkdimCG -- Check an image's dimensions are OK
c& nebk
c: plotting
c+
      subroutine chkdimcg (maxnax, maxdim, naxis, size, image)
c
      implicit none
      integer maxnax, maxdim, naxis, size
      character image*(*)
c
c  Check that an image's dimensions are acceptable.
c
c  Input:
c    maxnax      Maximum number of dimensions allowed in image
c    maxdim      Maximum size of first dimension allowed
c    naxis       Number of dimensions in contour image
c    size        SIze of first dimension
c    image       Image name
c--
c-----------------------------------------------------------------------
      character msg*80
      integer len1
c-----------------------------------------------------------------------
      if (naxis.gt.maxnax) then
        msg = 'CHKDIMCG: '//image(1:len1(image))//
     +        ' has too many dimensions'
        call bug ('f', msg)
      end if
c
      if (size.gt.maxdim) then
        msg = 'CHKDIMCG: '//image(1:len1(image))//
     +        ' first dimension too large'
        call bug ('f', msg)
      end if
c
      end
c
c* chnselCG -- Make list of CHAN and REGION selected channel groups
c& nebk
c: plotting
c+
      subroutine chnselcg (blc, trc, kbin, maxbox, boxes, maxgrp,
     +                     grpbeg, ngrp, ngrps)
c
      implicit none
      integer maxbox, boxes(maxbox), maxgrp, grpbeg(maxgrp), 
     +ngrp(maxgrp), ngrps, kbin(2), blc(3), trc(3)
c
c  Find the channels designated by the CHAN and REGION specifiations
c  via the RUNS arrays.  
c
c  Input
c    blc,trc    Cube surrounding region of interest
c    kbin       Channel increment and average to step through image
c    maxbox     Maximum number of boxes
c    boxes      Boxes following BOXINPUT,BOXSET,BOXINFO (optional) and
c               BOXMASK
c    maxgrp     Maximum number of allowed groups
c  Output
c    grpbeg     Array of start channels for each group of selected
c               channels.  Each group will be averaged together to
c               make on sub-plot
c    ngrp       Number of channels in each group
c    ngrps      Number of groups
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer maxruns
      parameter (maxruns = 10*maxdim)
c
      integer runs(3,maxruns), nruns, k, j, ipl, xmin, xmax, ymin,
     +ymax, inc, iav
c-----------------------------------------------------------------------
      inc = kbin(1)
      iav = kbin(2)
      if (inc.eq.1) iav = 1
      if (iav.gt.inc) iav = inc
c
      j = 0
      k = 0
      ipl = blc(3)
c
      do while (ipl+j.le.trc(3))
c
c Use runs to see if plane has some unblanked pixels
c
        call boxruns (1, ipl+j, ' ', boxes, runs, maxruns,
     +                nruns, xmin, xmax, ymin, ymax)
c
        if (nruns.gt.0) then
c
c This plane in region
c
          j = j + 1
          if (j.eq.1) then
c
c Assign group start plane
c
            k = k + 1
            if (k.gt.maxgrp) call bug ('f',
     +        'CHNSELCG: You have selected too many groups of channels')
            grpbeg(k) = ipl
c
            if (kbin(2).eq.1) then
              ngrp(k) = 1
              j = 0
              ipl = ipl + kbin(1)
            end if
          else if (j.eq.kbin(2)) then
c
c Reached limit of number of planes to average together.
c
            ngrp(k) = j
            j = 0
            ipl = ipl + kbin(1)
          end if
        else
c
c Don't want this plane, so start a new group and assign
c number of planes in old group.
c
          if (k.ne.0 .and. ngrp(k).eq.0) ngrp(k) = j
          ipl = ipl + j + 1
          j = 0
        end if
      end do
c
      if (k.eq.0) call bug ('f', 
     +  'CHNSELCG: There were no valid pixels in the region')
c
c Finish off last group
c
      if (ngrp(k).eq.0) ngrp(k) = j
      ngrps = k
c
      end
c
c* conlevCG -- Compute contour levels
c& nebk
c: plotting
c+
      subroutine conlevcg (mirror, maxlev, lcin, cnaxis, csize,
     +                     levtyp, slev, nlevs, levs, srtlev)
c
      implicit none
      integer lcin, cnaxis, csize(cnaxis), nlevs, maxlev, srtlev(maxlev)
      real slev, levs(maxlev)
      character*1 levtyp
      logical mirror
c
c  Compute contour levels
c
c  Input:
c    mirror   MUltiply specified contour levsls by -1 and add to list
c    maxlev   Maximum number of levels allowed
c    lcin     Handle of contour image
c    cnaxis   Number of dimensions in contour image
c    csize    Dimensions of contour image
c  Input/output:
c    levtyp   Type of scale factor (percentage or absolute)
c    slev     Contour scale factor
c    nlevs    Number of contour levels
c  Output:
c    levs     Contour levels
c    srtlev   Indexes of array giving levels in increasing order
c--
c-----------------------------------------------------------------------
      integer i, ilev, mlevs
      real cdmin, cdmax, off, inc
c-----------------------------------------------------------------------
      mlevs = nlevs
      if (nlevs.eq.0) then
c
c Set default contours
c
        call imminmax (lcin, cnaxis, csize, cdmin, cdmax)
c
        if (cdmax.gt.0.0 .and. cdmin.lt.0.0) then
           slev = max(abs(cdmax), abs(cdmin)) / 8
c
           nlevs = abs(cdmin) / slev
           ilev = 1
           do i = -nlevs, -1, 1
             levs(ilev) = i * slev
             ilev = ilev + 1
           end do 
c
           nlevs = cdmax / slev
           do i = 1, nlevs, 1
             levs(ilev) = i * slev
             ilev = ilev + 1
           end do          
c 
           nlevs = ilev - 1
           slev = 1.0
           levtyp = 'a'
        else
           off = 0.05 * (cdmax - cdmin)
           nlevs = 10
           inc = ((cdmax-off) - (cdmin+off)) / (nlevs - 1)
           do i = 1, nlevs
              levs(i) = cdmin+off + (i-1)*inc
           end do
c
           slev = 1.0
           levtyp = 'a'
        end if
      else if (levtyp.eq.'p')  then
c
c Set percentage contours
c
        if (slev.eq.0.0) slev = 1.0
        call imminmax (lcin, cnaxis, csize, cdmin, cdmax)
        slev = slev * cdmax / 100.0
      else if (levtyp.eq.'a') then
c
c Absolute contours
c
        if (slev.eq.0.0) slev = 1.0
      end if
c
c Set mirrored contours only for user specified contours
c
      if (mirror .and. mlevs.ne.0) then
        mlevs = nlevs
        do i = 1, mlevs
          if (levs(i).ne.0.0) then
            if (nlevs.lt.maxlev) then
              nlevs = nlevs + 1
              levs(nlevs) = -1.0*levs(i)
            else
              call bug ('w',
     +        'CONLEVCG: Max. no. of contours reached during mirroring')
              goto 100
            end if
          end if
        end do
      end if
c
c Scale levels
c
100   do i = 1, nlevs
        levs(i) = levs(i) * slev
      end do
c 
c Sort in increasing order
c
      call sortidxr (nlevs, levs, srtlev)
c      
      end
c
c
c* copyimCG -- Copy image
c& nebk
c: plotting
c+
      subroutine copyimcg (n, in, copy)
c
      implicit none
      integer n
      real in(n), copy(n)
c 
c  Copy an image for safe keeping
c
c Input
c     n       Size of image
c     image   Image
c Output
c     copy    Copy of image
c-
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, n
        copy(i) = in(i)
      end do
c
      end
c
c* cosdecCG -- Find cos(DEC) for an RA axis
c& nebk
c: plotting
c+
      subroutine cosdeccg (iax, naxis, ctype, crval, cosdec, ok)
c
      implicit none
      integer iax, naxis
      double precision crval(naxis), cosdec
      character ctype(naxis)*(*)
      logical ok
c
c  If the axis of interest is an RA axis, find cos(DEC) if
c  the declination axis exists.
c
c Input:
c   iax      Axis of interest in the image
c   naxis    Number of axes in the image
c   ctype    Axis names
c   crval    Axis reference values
c Output:
c   cosdec   cos(DEC).  WIll be 1.0 if axis of interest is not
c            RA, or if DEC axis can't be found 
c   ok       True if RA asked for and DEC found
c            True if RA not asked for
c            False if RA asked for and DEC absent
c--
c-----------------------------------------------------------------------
      integer ira, idec
c------------------------------------------------------------------------
c
c Find RA axis
c
      call axfndcg ('RA', naxis, ctype, ira)
c
c Is it the one we are interested in
c
      if (iax.eq.ira) then
c
c Find the DEC axis
c
        call axfndcg ('DEC', naxis, ctype, idec)
        if (idec.gt.0) then
          ok = .true.
          cosdec = cos(crval(idec))
        else
          ok = .false.
          cosdec = 1.0
        end if
      else
        ok = .true.
        cosdec = 1.0
      end if
c
      end
c
c* defchrCG -- Give a default char. height for axis & velocity labels
c& nebk
c: plotting
c+
      subroutine defchrcg (nx, ny, cs)
c
      implicit none
      real cs(*)
      integer nx, ny
c
c  Work out default character size for axis labels and for velocity 
c  label.  Add a linear ramp otherwise they come out too big for 
c  single plots per page, and too small for multiple plots per page
c
c  Input:
c    nx,ny     Number of sub-plots in x and y directions
c  Input/output:
c    cs        PGPLOT character sizes for axis labels and velocity label
c--
c-----------------------------------------------------------------------
c
c Axis labels
c
      if (cs(1).le.0.0) then
        cs(1) = 1.2 / max(nx,ny) 
        cs(1) = (0.13*max(nx,ny) + 0.67) * cs(1)
      end if
c
c Velocity/frequency/channel labels
c
      if (cs(2).le.0.0) then
        cs(2) = 1.2 / max(nx,ny)
        cs(2) = (0.13*max(nx,ny) + 0.67) * cs(2)
      end if
c
      end
c
c* grfixCG -- Fix up a grey scale range with optional bias for log taking
c& nebk
c: plotting
c+
      subroutine grfixcg (pixr, lgin, gnaxis, gsize, trfun, 
     +                    pixr2, groff, blankg)
c
      implicit none
      real pixr(2), pixr2(2), groff, blankg
      integer lgin, gnaxis, gsize(*)
      character trfun*(*)
c
c  Make sure the grey scale range is valid, and take logs if
c  desired.  This may require a DC bias to avoid negative 
c  numbers in the image.
c
c  Input:
c    lgin     Handle for image
c    gnaxis   Number of dimesions in image
c    gsize    Size of dimensions
c    trfun    'log', 'lin', 'heq', or 'sqr' transfer functions
c  Input/Output:
c    pixr     User supplied grey scale range.  Defaults filled in
c             on output
c  Output:
c    pixr2    Grey scale range with bias and logs/sqrt taken if necessary
c    groff    DC bias to avoid negatives in image if logs taken
c    blankg   Value to use for blanked pixels
c--
c-----------------------------------------------------------------------
      real gmini, gdmin, gdmax
c-----------------------------------------------------------------------
c
c Set default range to data min to max
c
      if (pixr(1).eq.0.0 .and. pixr(2).eq.0.0) then
        call imminmax (lgin, gnaxis, gsize, pixr(1), pixr(2))
      else if (pixr(1).eq.pixr(2)) then
        call bug ('w', 
     +    'GRFIXCG: Zero grey scale range, reset to image range')
        call imminmax (lgin, gnaxis, gsize, pixr(1), pixr(2))
      end if
c
c Work out offset if log transfer function for grey scale requested
c
      pixr2(1) = pixr(1)
      pixr2(2) = pixr(2)
      if (trfun.eq.'log' .or. trfun.eq.'sqr') then
        groff = 0.0
        call imminmax (lgin, gnaxis, gsize, gdmin, gdmax)
        gmini = min(pixr2(1), pixr2(2), gdmin)
        if (gmini.le.0.0) groff = abs(gmini) + 0.01*(gdmax - gdmin)
        if (trfun.eq.'log') then
          pixr2(1) = log10(pixr2(1)+groff)
          pixr2(2) = log10(pixr2(2)+groff)
        else 
          pixr2(1) = sqrt(pixr2(1)+groff)
          pixr2(2) = sqrt(pixr2(2)+groff)
        end if
      end if
c
c Set blanked pixel value to white.   
c
      blankg = pixr2(1) - (0.01*(pixr2(2)-pixr2(1)))
c
      end
c
c* hedinfCG -- Get some header information from image
c& nebk
c: plotting
c+
      subroutine hedinfcg (lun, naxis, size, epoch, crpix, cdelt,
     +                     crval, ctype, mask)
c
      implicit none
      integer lun, naxis, size(naxis)
      real epoch
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      character*(*) ctype(naxis)
      logical mask
c
c  Get some header keywords from the image associated with LUN
c 
c  Input
c    lun      Handle of image
c    naxis    Number of dimensions in image
c    size     Size of each axis
c  Output
c    epoch    Epoch of image
c    crpix    Array of image reference pixels
c    cdelt    Array of image increments (natural inits; rad)
c    crval    Array of image reference values (natural units)
c    ctype    Array of image axis types
c    mask     True if there is blanking mask
c--
c------------------------------------------------------------------------
      integer i
      character str*1, itoaf*1
      logical hdprsnt
c---------------------------------------------------------------------
      do i = 1, naxis
        str = itoaf(i)
c
        call rdhdd (lun, 'crpix'//str, crpix(i), dble(size(i))/2.0)
        call rdhdd (lun, 'cdelt'//str, cdelt(i), 1.0d0)
        call rdhda (lun, 'ctype'//str, ctype(i), ' ')
        call rdhdd (lun, 'crval'//str, crval(i), 0.0d0)
      end do
      call rdhdr (lun, 'epoch', epoch, 0.0)
      mask = hdprsnt (lun, 'mask')
c
      end 
c
c* heqCG -- Histogram equalize an image
c& nebk
c: plotting
c+
      subroutine heqcg (pixr, n, nimage, image, nbins, his, cumhis)
      implicit none
      integer nbins, n, nimage(n), his(nbins)
      real image(n), pixr(2), cumhis(nbins)
c
c  Apply histogram equalization to an image directly.  128 bins 
c  are used in the histogram.
c
c  Input
c   pixr   Display intensity range with bias added and logs/sqrt 
c          taken if necessary
c   n      Number of pixels
c   nimage Normalization image
c   nbins  Number of bins in histogram
c  Input/output
c   image  Image
c   his    Image histogram
c   cumhis Cumulative histogram.  Values for each bin are
c          the intensities assigned to the image.  Thus
c          if an image pixel ended up in cumhis bin idx, then
c          its new value is cumhis(idx)
c
c--
c-----------------------------------------------------------------------
      integer idx, i
      real fac, bmin, bmax, cum
c-----------------------------------------------------------------------
c
c Initialize histogram
c
      bmin = pixr(1)
      bmax = pixr(2)
      do i = 1, nbins
        his(i) = 0
        cumhis(i) = 0.0
      end do
c
c Generate image histogram
c
      fac = real(nbins-1) / (bmax-bmin)
      do i = 1, n
        if (nimage(i).gt.0.0) then
          idx = max(1,min(nbins,nint((image(i)-bmin)*fac)+1))
          his(idx) = his(idx) + 1
        end if
      end do
c
c Generate cumulative histogram.  
c
      cum = 0.0
      do i = 1, nbins
        cum = cum + his(i) 
        cumhis(i) = cum
      end do
c
c Now discretize the cumulative histogram values as well
c
      fac = real(nbins-1) / real(n)
      do i = 1, nbins
c
c This index converts the actual cumulative histogram
c value to the nearest discrete bin
c
        idx = max(1,min(nbins,nint(cumhis(i)*fac)+1))
c
c Convert this bin back to an intensity and reuse CUMHIS array
c
        cumhis(i) = real(idx)/real(nbins)*(bmax-bmin) + bmin
      end do
c
c Now fix the image pixels (including masked ones)
c
      fac = real(nbins-1) / (bmax-bmin)
      do i = 1, n
c
c Find cumulative histogram index of this pixel
c
        idx = max(1,min(nbins,nint((image(i)-bmin)*fac)+1))
c
c Replace by discretized cumulative histogram intensity
c
        image(i) = cumhis(idx)
      end do
c
      end
c
c* limitsCG -- Work out limits and transformation matrix for both axes
c& nebk
c: plotting
c+
      subroutine limitscg (labtyp, blc, trc, naxis, epoch, crpix, cdelt,
     +   crval, ctype, doepoch, xmin, xmax, ymin, ymax, ibin, jbin, 
     +   tr, xlabel, ylabel)
c
      implicit none
      integer blc(*), trc(*), naxis, ibin, jbin
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      real xmin, xmax, ymin, ymax, tr(6), epoch
      character*(*) labtyp(2), ctype(naxis), xlabel, ylabel
      logical doepoch
c
c   Work out window world coordinate limits and transformation array 
c   depending upon label type.  Return header items to be used
c   for all following positional computations.
c
c     Input
c       labtyp       Label type (p, po, ao, rd, l, lo) for x and y axes
c       blc,trc      Spatial window in unbinned pixels
c       naxis        Number of axes
c       crpix        Array of image reference pixels
c       cdelt        Array of image increments 
c       crval        Array of image reference values 
c       ctype        Array of image axis types
c       epoch        EPoch
c       doepoch      Write epoch into axis labels
c       i,jbin       x and y increments to step through image in
c     Output
c       c,ylabel     Labels for axes
c       xmin,xmax    Spatial window in world coordinates, depends on labtyp
c       ymin,ymax
c       tr           Matrix transforming from array indices to world
c                    coordinates.  Note this accounts for the fact that
c                    only the desired window is read into the data arrays,
c                    so there is a blc offset included in tr.  It also
c		     accounts for any spatial binning.
c
c-----------------------------------------------------------------------
c
c Set transformation matrix; converts from pixels to world coordinates.
c No cross terms in transformation
c
      tr(3) = 0.0
      tr(5) = 0.0
c
c Fill limits and transformation depending on label type
c
      call limtrcg (doepoch, labtyp(1), naxis, 1, crval, crpix, cdelt, 
     +   ctype, blc(1), trc(1), epoch, ibin, xmin, xmax, 
     +   tr(1), tr(2), xlabel)
      call limtrcg (doepoch, labtyp(2), naxis, 2, crval, crpix, cdelt, 
     +   ctype, blc(2), trc(2), epoch, jbin, ymin, ymax, 
     +   tr(4), tr(6), ylabel)
c
      end
c
c* limtrCG -- Work out limits and transformation matrix for one axis
c& nebk
c: plotting
c+
      subroutine limtrcg (doepoch, labtyp, naxis, iax, crval, crpix, 
     +                    cdelt, ctype, blc, trc, epoch, bin, axmin, 
     +                    axmax, tr1, tr2, label)
c
      implicit none
      integer blc, trc, iax, naxis, bin
      double precision cdelt(naxis), crval(naxis), crpix(naxis)
      real axmin, axmax, tr1, tr2, epoch
      character labtyp*(*), label*(*), ctype(naxis)*(*)
      logical doepoch
c
c  Work out limits and transformation for current axis
c
c  Input
c    doepoch      Write epoch into axis labels
c    labtyp       Label type for axis
c    naxis        Number of axes
c    iax          Number of axis we want to label. Generally 1 or 2
c    crpix        Array of image reference pixels
c    cdelt        Array of image increments 
c    crval        Array of image reference values 
c    ctype        Array of image axis types
c    blc,trc      Spatial window in unbinned pixels
c    epoch        Epoch
c    bin          Pixel increment that we are stepping through image with
c  Output
c    axmin,axmax  Spatial window in world coordinates, depends on labtyp
c    tr1,2        Matrix elements for current axis for array transforming 
c                 from array indices to world coordinates.  Note this 
c                 accounts for the fact that only the desired window is 
c                 read into the data arrays,  so there is blc offset 
c                 included in tr.  Thus x = tr1 + tr2*i where
c                 i is 1 at blc. 
c    label        Axis label
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
      double precision r2a, w1
      parameter (r2a = 180.0*3600.0/dpi)
c
      double precision winc, pix
      integer l2, ipos, len1, irad, ifrq, ivel, iuv
      character str*20, estr*5
      logical ok
c----------------------------------------------------------------------------
c
c Set limits depending on label type and check validity
c
      call pix2wcg (.true., dble(blc-0.5), iax, labtyp, naxis, 
     +              crval, crpix, cdelt, ctype, w1, ok)
      axmin = w1
      call pix2wcg (.true., dble(trc+0.5), iax, labtyp, naxis, 
     +              crval, crpix, cdelt, ctype, w1, ok)
      axmax = w1
c
      if (axmin.eq.axmax) then
        if (iax.eq.1) call bug ('f', 
     +    'LIMTRCG: Display limits; XMIN = XMAX -- check REGION')
        if (iax.eq.2) call bug ('f', 
     +    'LIMTRCG: Display limits; YMIN = YMAX -- check REGION')
      end if
c
c Set transformation matrix elements depending on label type
c
      pix = dble(blc) - 1.0d0 - (bin-1)*0.5d0
      call pix2wcg (.true., pix, iax, labtyp, naxis, crval, 
     +              crpix, cdelt, ctype, w1, ok)
      tr1 = w1
c
      call pixi2wcg (.true., iax, labtyp, naxis, crval, crpix, 
     +               cdelt, ctype, winc, ok)
      tr2 = winc*bin
c
c Write epoch string for label
c
      if (doepoch .and. epoch.gt.0.0) then
        write (estr(2:), 100) nint(epoch)
100     format (i4)
        if (estr(2:).eq.'2000') then
          estr(1:1) = 'J'
        else if (estr(2:).eq.'1950') then
          estr(1:1) = 'B'
        end if
      else
        estr = ' '
      end if
c
c Remove ---* and --* from axis type
c
      ipos = index(ctype(iax),'---')
      if (ipos.ne.0) then
        str = ctype(iax)(1:ipos-1)
      else
        ipos = index(ctype(iax),'--')
        if (ipos.ne.0) then
          str = ctype(iax)(1:ipos-1)
        else
          str = ctype(iax)(1:len1(ctype(iax)))
        end if
      end if
      l2 = len1(str)
c
c Set the axis label depending on label type
c
      call axfndcg ('RAD',  1, ctype(iax), irad)
      call axfndcg ('FREQ', 1, ctype(iax), ifrq)
      call axfndcg ('VELO', 1, ctype(iax), ivel)
      call axfndcg ('UV',   1, ctype(iax), iuv)
c
      if (labtyp.eq.'abspix') then
        label = str(1:l2)//' (pixels; '//estr//')'
        if (estr.eq.' ') label = str(1:l2)//' (pixels)'
      else if (labtyp.eq.'relpix') then
        label = str(1:l2)//' offset (pixels; '//estr//')'
        if (estr.eq.' ') label = str(1:l2)//' offset (pixels)'
      else if (labtyp.eq.'arcsec') then
        label = str(1:l2)//' offset (arcsec; '//estr//')'
        if (estr.eq.' ') label = str(1:l2)//' offset (arcsec)'
      else if (labtyp.eq.'absdeg') then
        label = str(1:l2)//' (degrees; '//estr//')'
        if (estr.eq.' ') label = str(1:l2)//' (degrees)'
      else if (labtyp.eq.'reldeg') then
        label = str(1:l2)//' offset (degrees; '//estr//')'
        if (estr.eq.' ') label = str(1:l2)//' offset (degrees)'
      else if (labtyp.eq.'hms') then
        label = str(1:l2)//' ('//estr//')'
        if (estr.eq.' ') label = str(1:l2)
      else if (labtyp.eq.'dms') then
        label = str(1:l2)//' ('//estr//')'
        if (estr.eq.' ') label = str(1:l2)
      else if (labtyp.eq.'absghz') then
        label = str(1:l2)//' (GHz)'
      else if (labtyp.eq.'relghz') then
        label = str(1:l2)//' offset (GHz)'
      else if (labtyp.eq.'abskms') then
        label = str(1:l2)//' (Km s\u-1\d)'
      else if (labtyp.eq.'relkms') then
        label = str(1:l2)//' offset (Km s\u-1\d)'
      else if (labtyp.eq.'rellin') then
        if (irad.eq.1) then
          label = str(1:l2)//' offset (radians)'
        else if (ifrq.eq.1) then
          label = str(1:l2)//' offset (GHz)'
        else if (ivel.eq.1) then
          label = str(1:l2)//' offset (Km s\u-1\d)'
        else if (iuv.eq.1) then
          label = str(1:l2)//' offset (\gl)'
        else
          label = str(1:l2)//' offset '
        end if
      else if (labtyp.eq.'abslin') then
        if (irad.eq.1) then
          label = str(1:l2)//' (radians)'
        else if (ifrq.eq.1) then
          label = str(1:l2)//' (GHz)'
        else if (ivel.eq.1) then
          label = str(1:l2)//' (Km s\u-1\d)'
        else if (iuv.eq.1) then
          label = str(1:l2)//' (\gl)'
        else
          label = str(1:l2)
        end if
      else if (labtyp.eq.'none') then
        label = ' '
      end if
c
      end
c
c* maskorCG -- OR mask image mask with data image mask
c& nebk
c: plotting
c+
      subroutine maskorcg (blank, win, bimage, nimage, image)
c
      implicit none
      integer nimage(*), win(2)
      real image(*), blank
      logical bimage(*)
c
c  OR the mask image mask and the grey/contour/vector image mask
c
c  Input:
c    blank       Value to give blanked pixel
c    win         Size of image
c    bimage      The mask image mask.True is unflagged, false is flagged
c  Input/Output
c    nimage      The normalization image.  0-> blanked
c    image       The image.  New blanks may be set
c--
c------------------------------------------------------------------------
      integer i, imsize
c------------------------------------------------------------------------
      imsize = win(1) * win(2)
      do i = 1, imsize
        if (.not.bimage(i)) then
          nimage(i) = 0
          image(i)  = blank
        end if
      end do
c
      end
c
c
c*matchCG -- Match fields with allowed types and die if no good
c:plotting
c+
      subroutine matchcg (n, field, string, struct, ntype, types)
c
      implicit none
      integer ntype, n
      character*(*) types(ntype), string, field, struct
c
c  Look for string in list of allowed ones.  If not found die with 
c  fatal error.  Expand string for minimum match.  Extra variables
c  can be used to provide error messages.  These messages expect
c  that the string is one field from several fields making up
c  one structure, and that there are several structures being
c  examined.  For example, an overlay file for CGDISP expects
c  many rows, each describing one overlay.  Each row contains
c  several fields, each of which may take on different values.
c
c  Inputs:
c    ntype	The number of possible values that STRING can have
c    type	An array of possible values for STRING
c
c    n          Number of the thing that we are matching (used
c               in  error messages if non zero)
c    field      A string describing what field STRING is (used
c               in error messages if non blank).
c    struct     A string describing the generic name of the structure
c               from which STRING is one field (used in error messages
c               if non blank).
c               Examples are "overlay", "slice"
c  Input/output:
c    string     The string that we are trying to match.  It is
c               expanded from minimum  match on output
c
c  An example of an error message would be:
c
c      STRING is ambiguous for STRUCT # N field FIELD.  Choose from: ...
c
c      Thus, if STRING was "rel", STRUCT was "overlay", N was 14
c      and FIELD was "xotype" then the message would read
c
c
c      "rel" is ambiguous for overlay # 14 field "xotype". Choose from: ...
c
c       Got it ??
c 
c--
c------------------------------------------------------------------------
      integer l, i, iopt, j, il, il2
      integer len1
      character*130 umsg, str*10
c-----------------------------------------------------------------------
      l = len1(string)
c
      iopt = 0
      do i = 1, ntype
        if (string(1:l).eq.types(i)(1:l)) then
          if (iopt.ne.0) then
            umsg = '"'//string(1:l)//'" is ambiguous'
            il = len1(umsg) + 1
c
            if (struct.ne.' ' .and. n.gt.0) then
              umsg(il:) = ' for '//struct(1:len1(struct))
              il = len1(umsg) + 1
c
              call strfi (n, '(i4)', str, il2)
              umsg(il:) = ' (# '//str(1:il2)//')'
              il = len1(umsg) + 1
            end if
c
            if (field.ne.' ') then
              if (struct.eq.' ' .or. n.eq.0) then
                umsg(il:) =  ' for field "'//field(1:len1(field))//
     +                       '".  Choose from'
              else
                umsg(il:) =  ' field "'//field(1:len1(field))//
     +                       '".  Choose from'
              end if
            else
              umsg(il:) =  '. Choose from'
            end if
c
            call output (umsg)
            do j = 1, ntype
              umsg = '   '//types(j)
              call output (umsg)
            end do
            call bug('f', 'MATCHCG:')
          end if
          iopt = i
        endif
      end do
c
c Set expanded string
c
      if (iopt.ne.0) string = types(iopt)
c
c Didn't find nuttin
c
      if(iopt.eq.0) then
        umsg = '"'//string(1:l)//'" is unrecognized'
        il = len1(umsg) + 1
c
        if (struct.ne.' ' .and. n.gt.0) then
          umsg(il:) = ' for '//struct(1:len1(struct))
          il = len1(umsg) + 1
c
          call strfi (n, '(i4)', str, il2)
          umsg(il:) = ' (# '//str(1:il2)//')'
          il = len1(umsg) + 1
        end if
c
        if (field.ne.' ') then
          if (struct.eq.' ' .or. n.eq.0) then
            umsg(il:) =  ' for field "'//field(1:len1(field))//
     +                   '".  Choose from'
          else
            umsg(il:) =  ' field "'//field(1:len1(field))//
     +                   '".  Choose from'
          end if
        else
          umsg(il:) =  '. Choose from'
        end if
c
        call output (umsg)
        do j = 1, ntype
          umsg = '   '//types(j)
          call output (umsg)
        end do
        call bug('f', 'MATCHCG:')
      end if
c
      end
c
c
c* nxnyCG -- Work out number of sub-plots per page
c& nebk
c: plotting
c+
      subroutine nxnycg (nxdef, nydef, nz, nx, ny, nlast)
c
      implicit none
      integer nxdef, nydef, nx, ny, nz, nlast
c
c  Work out number of plots in the x and y directions and the
c  total number of plots
c
c  Inputs
c    nx,ydef   Default x and y numbers of sub-plots per page
c    nz        Total number of sub-plots
c  Outputs
c    nx,ny     Number of sub-plots in x and y directions per page
c    nlast     Number of sub-plots on the last row of the last page
c--
c--------------------------------------------------------------------------
      if (nx.le.0 .or. ny.le.0) then
        if (nz.lt.nxdef*nydef) then
          nx = 1
          ny = 1
          do while (nz.gt.nx*ny) 
            if (nx.eq.ny) then
              nx = nx + 1
            else
              ny = ny + 1
            end if
          end do
        else
          nx = nxdef
          ny = nydef
        end if
      end if
c
      nlast = mod(nz,nx)
      if (nlast.eq.0) nlast = nx
c
      end
c
c*OpImCG -- Open an image and return axis descriptors
c:plotting
c+
      subroutine opimcg (maxdim, maxnax, in, lin, naxis, size, epoch,
     +                   mask, crpix, cdelt, crval, ctype)
c
      implicit none
c
      integer maxdim, maxnax, lin, size(maxnax), naxis
      double precision crval(maxnax), cdelt(maxnax),
     +  crpix(maxnax)
      real epoch(*)
      character*(*) in, ctype(maxnax)
      logical mask
c
c     Open an image and return some header descriptors 
c
c   Input:
c     maxdim     Maximum allowed size of first dimension of image
c     maxnax     Maximum number of allowed dimenions for image
c   Output:
c     lin        Handle for image
c     size       Size of each dimension of image 
c     naxis      Number of dimensions of image
c     epoch      Epoch of image
c     mask       True if blanking mask present for image
c     crpix      Reference pixels
c     cdelt      Increments
c     crval      Reference values
c     ctype      Axis types
c--
c-----------------------------------------------------------------------
      integer len1
c-----------------------------------------------------------------------
      call xyopen (lin, in, 'old', maxnax, size)
      call rdhdi (lin, 'naxis', naxis, 0)
      if (naxis.eq.0) call bug ('f', in(1:len1(in))//
     +    ' has zero dimensions !!')
      call hedinfcg (lin, naxis, size, epoch, crpix, cdelt,
     +               crval, ctype, mask)
      call chkdimcg (maxnax, maxdim, naxis, size, in)
c
      end
c
c
c*OptCG -- Get command line options but only warn if unrecognized.
c:plotting
c+
      subroutine optcg (key, opts, present, nopt)
c
      implicit none
      character key*(*)
      integer nopt
      character opts(nopt)*(*)
      logical present(nopt)
c
c  Get options from the command line, and return to the caller those
c  options that are present. 
c
c  Unrecognized options generate a warning only, unlike the standard 
c  Miriad subroutine that does this which issues a fatal error.
c
c  Inputs:
c    key	The task keyword to use.
c    opts	An array of possible option values. These should be in lower
c		case.
c    nopt	The number of possible options.
c  Output:
c    present	This indicates whether the option was present.
c--
c------------------------------------------------------------------------
      character string*16
      integer l,i,iopt
c
c  Externals.
c
      integer len1
      character*80 umsg
c-----------------------------------------------------------------------
c
c  Initialise the options to indicate that none are present.
c
      do i = 1, nopt
        present(i) = .false.
      end do
c
c  Search the task parameters.
c
      call keya (key, string, ' ')
      do while (string.ne.' ')
        l = len1(string)
        call lcase (string(1:l))
        umsg = 'OPTCG: Unrecognised option "'//string(1:l)//'"'
        if (l.gt.len(opts(1))) call bug ('f', umsg)
        iopt = 0
        do i = 1, nopt
          if (string(1:l).eq.opts(i)(1:l)) then
            umsg = 'OPTCG: Ambiguous option "'//string(1:l)//'"'
            if (iopt.ne.0) call bug ('f', umsg)
            iopt = i
          end if
        end do
c
        if (iopt.eq.0) then
          umsg = 'OPTCG: Unrecognised option "'//string(1:l)//'"'
          call bug ('w', umsg)
        else
          present(iopt) = .true.
        end if
        call keya (key, string, ' ')
      end do
c
      end
c
c* otopixCG -- Convert overlay location in given coordinates to pixels
c& nebk
c: plotting
c+
      subroutine otopixcg (otype, iax, dsign, naxis, crval, crpix,
     +                     cdelt, ctype, nums, posoff, opos, nnum)
c
      implicit none
      integer nnum, dsign, iax, naxis
      double precision nums(*), cdelt(naxis), crval(naxis), 
     +  crpix(naxis), opos, posoff
      character ctype(naxis)*(*), otype*(*)
c
c Convert overlay location from OTYPE units to pixels
c
c Input
c   otype     Overlay type
c   iax       The axis number of interest
c   dsign     SIgn for dms axes. +/-1  
c   c*        Axis descriptors
c   nums      The array of numbers read from the overlay file
c             starting with the first one to use here
c   posoff    Offset to be added to the locations decoded from
c	      the text file and held in NUMS.  These are in the same
c             units as OTYPE so no conversion is done.  Is ignored
c             for RA and DEC because I am too lazy.
c Output
c   opos      Output location in pixels
c   nnum      The number of numbers used from the NUMS array for
c             this overlay location.  3 for HMS and DMS else 1
c--
c-----------------------------------------------------------------------
      double precision world
      logical ok
c-----------------------------------------------------------------------
      if (otype.eq.'hms') then
        world = nums(1)*3600.0d0 + nums(2)*60.0d0 + nums(3)
        nnum = 3
      else if (otype.eq.'dms') then
        if (dsign.eq.-1) then
          world = -(abs(nums(1))*3600.d0 + nums(2)*60.d0 + nums(3))
        else
          world = nums(1)*3600.d0 + nums(2)*60.d0 + nums(3)
        end if
        nnum = 3
      else
        world = nums(1) + posoff
        nnum = 1
      end if
      call w2pixcg (world, iax, otype, naxis, crval, crpix, 
     +              cdelt, ctype, opos, ok)
c
      end
c
c* pix2wcg -- Convert from image pixel to world coordinate of given type
c& nebk
c: plotting
c+
      subroutine pix2wcg (domsg, pixel, iax, labtyp, naxis, crval, 
     +                    crpix, cdelt, ctype, world, ok)
c
      implicit none
      integer naxis, iax
      double precision cdelt(naxis), crval(naxis), crpix(naxis),
     +  world, pixel
      character labtyp*(*), ctype(naxis)*(*)
      logical ok, domsg
c
c  Convert from image pixels to world coordinates, depending on
c  requested labtyp.
c
c  Input:
c    domsg   If true, give a message warning when requested labtyp does 
c            not match the axis type in ctype.
c    pixel   Image pixel value
c    iax     This is the axis number in which we are interested. Should
c            be 1, 2 or 3.
c    labtyp  Requested type of world coordinate.   Should be one
c            of   abspix, relpix, arcsec, hms, dms, absghz, relghz,
c                 abskms, relkms, abslin, rellin, none. Note that a 
c	     request for a linear (abs or rel) axis conversion for an
c	     RA axis will return the RA in radians of polar rotation. 
c	     That is, the increment will be divided by cos(DEC)
c	     For labtyp=hms and labtyp=dms the world coordinate is in 
c	     seconds of time and seconds of arc.
c    naxis   Number of axes in image
c    crval   Array of image reference values
c    crpix   Array of image reference pixels
c    cdelt   Array of image pixel increments
c    ctype   Array of image axis types
c  Output:
c    world   Output world coordinate.  
c    ok      If false, then the requested labtyp is inconsistent with
c            the axis type.  WOrld will have been computed as if
c            the axis type was as expected.
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxdim.h'
      double precision r2a, r2d
      integer max2
c
      parameter (r2a = 180.0d0*3600.0d0/dpi, max2 = 11 * maxdim,
     +           r2d = 180.0d0/dpi)
c
      double precision rainc, cosdec
      integer idec, ira, ifrq, ivel, len1, irad
      character msg*80
      logical warn(11,maxdim), cdok
      save warn
      data warn /max2*.true./
c----------------------------------------------------------------------------
      if (iax.lt.1 .or. iax.gt.naxis) 
     +  call bug ('f', 'PIX2WCG: Invalid axis number')
c
      call axfndcg ('RA',   naxis, ctype, ira)
      call axfndcg ('DEC',  naxis, ctype, idec)
      call axfndcg ('FREQ', naxis, ctype, ifrq)
      call axfndcg ('VELO', naxis, ctype, ivel)
      call axfndcg ('RAD', 1, ctype(iax), irad)
      call cosdeccg (iax, naxis, ctype, crval, cosdec, cdok)
c
c Set world coordinate depending on type
c
      ok = .true.
      if (labtyp.eq.'abspix' .or. labtyp.eq.'none') then
c
c Absolute pixels ('none' masquerades as 'abspix')
c
        world = pixel
      else if (labtyp.eq.'relpix') then
c 
c Relative pixels
c
        world = pixel - crpix(iax)
      else if (labtyp.eq.'arcsec') then
c
c Relative arcseconds
c
        if (irad.eq.0) then
          write (msg, 100) iax
100       format ('PIX2WCG: Axis ',i1,' does not appear to have pixel ',
     +            'increments in radians but')
          if (domsg) call bug ('w', msg)
          if (domsg) call bug ('w', 
     +      'PIX2WCG: conversion to "arcsec" requested. Continue '//
     +      'assuming axis in radians')
          warn(1,iax) = .false.
          ok = .false.
        end if
c
        world = (pixel - crpix(iax)) * cdelt(iax) * r2a
      else if (labtyp.eq.'hms') then
c
c HH MM SS.S
c
        if (iax.ne.ira .and. warn(2,iax)) then
          write (msg, 200) iax
200       format ('PIX2WCG: Axis ', i1, ' is not RA but conversion ',
     +            'to "hms"')
          if (domsg) call bug ('w', msg)
          if (domsg) call bug ('w', 
     +      'PIX2WCG: requested.  Continue assuming axis in radians')
          warn(2,iax) = .false.
          ok = .false.
        end if
c
        if (idec.eq.0) call bug ('f', 
     +    'PIX2WCG: No DEC axis found; cannot convert RA to "hms"')
c
c Work out in radians
c
        world = ((pixel-crpix(iax))*cdelt(iax)/cosdec)+crval(iax)
c
c Convert to seconds of time
c
        world = world * r2a / 15.0
      else if (labtyp.eq.'dms') then
c
c DD MM SS.S
c
        if (iax.ne.idec .and. warn(3,iax)) then
          write (msg, 300) iax
300       format ('PIX2WCG: Axis ', i1, ' is not DEC but conversion ',
     +            'to "dms"')
          if (domsg) call bug ('w', msg)
          if (domsg) call bug ('w', 
     +      'PIX2WCG: requested.  Continue assuming axis in radians')
          warn(3,iax) = .false.
          ok = .false.
        end if
c
c Work out in radians
c
        world = ((pixel-crpix(iax))*cdelt(iax)) + crval(iax)
c
c Convert to seconds
c
        world = world * r2a 
      else if (labtyp.eq.'absghz') then
c
c Absolute frequency
c
        if (iax.ne.ifrq .and. warn(4,iax)) then
          write (msg, 400) iax
400       format ('PIX2WCG: Axis ', i1, 
     +            ' is not FREQ but conversion to "absghz"')
          if (domsg) call bug ('w', msg)
          if (domsg) call bug ('w', 
     +    'PIX2WCG: requested. Continue assuming axis in GHz')
          warn(4,iax) = .false.
          ok = .false.
        end if
c
        world = (pixel - crpix(iax)) * cdelt(iax) + crval(iax)
      else if (labtyp.eq.'relghz') then
c
c Relative frequency
c
        if (iax.ne.ifrq .and. warn(5,iax)) then
          write (msg, 500) iax
500       format ('PIX2WCG: Axis ', i1, 
     +            ' is not FREQ but conversion to "relghz"')
          if (domsg) call bug ('w', msg)
          if (domsg) call bug ('w', 
     +    'PIX2WCG: requested. Continue assuming axis in GHz')
          warn(5,iax) = .false.
          ok = .false.
        end if
c
        world = (pixel - crpix(iax)) * cdelt(iax) 
      else if (labtyp.eq.'abskms') then
c
c Absolute velocity
c
        if (iax.ne.ivel .and. warn(6,iax)) then
          write (msg, 600) iax
600       format ('PIX2WCG: Axis ', i1, 
     +            ' is not VELO but conversion to "abskms"')
          if (domsg) call bug ('w', msg)
          if (domsg) call bug ('w', 
     +     'PIX2WCG: requested. Continue assuming axis in Km/s')
          warn(6,iax) = .false.
          ok = .false.
        end if
c
        world = (pixel - crpix(iax)) * cdelt(iax) + crval(iax)
      else if (labtyp.eq.'relkms') then
c
c Relative velocity
c 
        if (iax.ne.ivel .and. warn(7,iax)) then
          write (msg, 700) iax
700       format ('PIX2WCG: Axis ', i1, 
     +            ' is not VELO but conversion to "relkms"')
          if (domsg) call bug ('w', msg)
          if (domsg) call bug ('w', 
     +     'PIX2WCG: requested. Continue assuming axis in Km/s')
          warn(7,iax) = .false.
          ok = .false.
        end if
        world = (pixel - crpix(iax)) * cdelt(iax) 
      else if (labtyp.eq.'abslin') then
c
c Absolute linear coordinate.  For RA convert to radians of
c polar rotation.
c
        if (iax.eq.ira .and. idec.eq.0) then
          if (warn(8,iax)) then
            write (msg, 800) iax
800         format ('PIX2WCG: "abslin" requested for RA axis ', i1, 
     +              ' but no DEC axis')
            call output (msg)
            msg = 'PIX2WCG: in image so coordinate is radians on sky'
            call output (msg)
            warn(8,iax) = .false.
          end if
        end if
c
        rainc = cdelt(iax) / cosdec
        world = (pixel - crpix(iax)) * rainc + crval(iax)
      else if (labtyp.eq.'rellin') then
c
c Relative linear coordinate.  For RA convert to radians of
c polar rotation.
c
        if (iax.eq.ira .and. idec.eq.0 .and. warn(9,iax)) then
          write (msg, 900) iax
900       format ('PIX2WCG: "rellin" requested for RA axis ', i1, 
     +            ' but no DEC axis')
          call output (msg)
          msg = 'PIX2WCG: in image so coordinate is radians on sky'
          call output (msg)
          warn(9,iax) = .false.
        end if
        rainc = cdelt(iax) / cosdec
        world = (pixel - crpix(iax)) * rainc 
      else if (labtyp.eq.'reldeg') then
c
c Relative degrees
c
        if (irad.eq.0) then
          write (msg, 910) iax
910       format ('PIX2WCG: Axis ',i1,' does not appear to have pixel ',
     +            'increments in radians but')
          if (domsg) call bug ('w', msg)
          if (domsg) call bug ('w', 
     +       'PIX2WCG: conversion to "arcsec" requested. Continue '//
     +       'assuming axis in radians')
          warn(10,iax) = .false.
          ok = .false.
        end if
c
        world = (pixel - crpix(iax)) * cdelt(iax) * r2d
      else if (labtyp.eq.'absdeg') then
c
c Absolute degrees
c
        if (irad.eq.0) then
          write (msg, 920) iax
920       format ('PIX2WCG: Axis ',i1,' does not appear to have pixel ',
     +            'increments in radians but')
          if (domsg) call bug ('w', msg)
          if (domsg) call bug ('w', 
     +       'PIX2WCG: conversion to "arcsec" requested. Continue '//
     +       'assuming axis in radians')
          warn(11,iax) = .false.
          ok = .false.
        end if
c
        world = (pixel - crpix(iax)) * cdelt(iax)  + crval(iax)
        world = world * r2d
      else
        msg = 'PIX2WCG: '//labtyp(1:len1(labtyp))//
     +        ' is an unrecognized world coordinate type'
        call bug ('f', msg)
      end if
c
      end
c
c* pix2wfcg -- Convert from image pixel to world coordinate and format
c& nebk
c: plotting
c+
      subroutine pix2wfcg (labtyp, iax, pixel, naxis, crval, crpix, 
     +                     cdelt, ctype, nounit, str, ilen)
c
      implicit none
      integer iax, naxis, ilen
      double precision crval(naxis), cdelt(naxis), crpix(naxis), pixel
      character str*(*), labtyp*(*), ctype(*)*(*)
      logical nounit
c
c  Format the specified pixel value in a string according to the
c  specified label type.   If the label type is inconsistent with
c  the axis (e.g. asking for "hms" for a non-RA-like axis),
c  an "abslin" axis type format will result.
c
c Input/output:
c   labtyp Axis label type. May be changed on output to "abslin"
c          if labtyp is incosnistent with ctype
c   iax    Axis of pixel
c   pixel  Absolute pixel value
c   naxis  Number of axes
c   crval  Array of reference values
c   crpix  Array of reference pixels
c   cdelt  Array of pixel increments
c   ctype  Array of axis types
c   nounit If true don't append units to string
c Output:
c   str    Formatted string
c   ilen   Length of str
c--
c-----------------------------------------------------------------------
      double precision world
      character msg*80, labtyp2*6, unit*20, str1*30
      logical ok
      integer len1, il
c
      character*30 dangle, dangleh
c-----------------------------------------------------------------------
c
c Convert absolute pixel to label type coordinate
c
      labtyp2 = labtyp
      call pix2wcg (.false., pixel, iax, labtyp2, naxis, crval, crpix,
     +              cdelt, ctype, world, ok)
c
c Axis type did not match request, give linear instead
c
      if (.not.ok) then
        write (msg, 100) iax
100     format ('PIX2WFCG: type of axis ', i1, ' and requested label')
        call bug ('w', msg)
        call bug ('w', 'PIX2WFCG: do not match.  Use "abslin" instead')
c
        labtyp2 = 'abslin'
        call pix2wcg (.false., pixel, iax, labtyp2, naxis, crval, crpix,
     +                cdelt, ctype, world, ok)
      end if
c
c Format value
c
      if (labtyp2.eq.'abspix' .or. labtyp2.eq.'relpix' .or.
     +    labtyp2.eq.'none') then
        call strfd (world, '(f8.2)', str1, il)
      else if (labtyp2.eq.'abskms' .or. labtyp2.eq.'relkms') then
        call strfd (world, '(1pe12.5)', str1, il)
      else if (labtyp2.eq.'absghz' .or. labtyp2.eq.'relghz') then
        call strfd (world, '(1pe11.5)', str1, il)
      else if (labtyp2.eq.'absdeg' .or. labtyp2.eq.'reldeg') then
        call strfd (world, '(f8.3)', str1, il)
      else if (labtyp2.eq.'arcsec' .or. 
     +         labtyp2.eq.'abslin' .or. labtyp2.eq.'rellin') then
        call strfd (world, '(1pe15.8)', str1, il)
      else if (labtyp2.eq.'hms') then
c
c  Pix2wcg returns RA in seconds of time; dangleh wants hours
c
        world = world / 3600.0d0
c
c Unwrap if necessary
c
        if (world.lt.0.0d0) world = 24.0d0 + world
        str1 = dangleh(world)
        il = len1(str1)
      else if (labtyp2.eq.'dms') then
c
c  Pix2wcg returns DEC in seconds of arc; dangle wants degrees
c
        world = world / 3600.0d0
        str1 = dangle(world)
        il = len1(str1)
      else
        msg = 'PIX2WFCG: '//labtyp2(1:len1(labtyp2))//
     +        ' is an unrecognized world coordinate type'
        call bug ('f', msg)
      end if
c
c Set coordinate units
c
      if (nounit) then
        str = str1(1:il)
      else
        call sunitcg (ctype(iax), labtyp2, unit)
        str = str1(1:il)//' '//unit
      end if
      ilen = len1(str)
c
      end
c
c* pixi2wcg -- Convert pixel increment to the requested world coordinate
c& nebk
c: plotting
c+
      subroutine pixi2wcg (domsg, iax, labtyp, naxis, crval, crpix,
     +                     cdelt, ctype, winc, ok)
c
      implicit none
      integer naxis, iax
      double precision cdelt(naxis), crval(naxis), crpix(naxis), winc
      character labtyp*(*), ctype(naxis)*(*)
      logical ok, domsg
c
c  Convert from image pixel increment to a world coordinate increment,
c  depending on requested type.
c
c  Input:
c    domsg   If true, give a message warning when requested labtyp does 
c            not match the axis type in ctype.
c    iax     This is the axis number in which we are interested. Should
c            be 1, 2 or 3.
c    labtyp  Requested type of world coordinate.   Should be one
c            of   abspix, relpix, arcsec, hms, dms, absghz, relghz,
c                 abskms, relkms, abslin, rellin, none.   Note that a 
c	     request for a linear (abs or rel) axis conversion for an 
c	     RA axis will return the RA in radians of polar rotation. 
c	     That is, the increment will be divided by cos(DEC)
c    naxis   Number of axes in image
c    crval   Array of image reference values
c    crpix   Array of image reference pixels
c    cdelt   Array of image pixel increments
c    ctype   Array of image axis types
c  Output:
c    winc    The pixel increment in the requested world coordinate type
c            For labtyp=hms and labtyp=dms the world coordinate is in 
c            seconds of time and seconds of arc
c    ok      If false, then the requested type is inconsistent with
c            the axis type.  WOrld will have been computed as if
c            the axis type was as expected.
c--
c-----------------------------------------------------------------------
      double precision w1, w2
c----------------------------------------------------------------------------
      call pix2wcg (domsg, 1.0d0, iax, labtyp, naxis, crval,
     +              crpix, cdelt, ctype, w1, ok)
      call pix2wcg (domsg, 2.0d0, iax, labtyp, naxis, crval,
     +              crpix, cdelt, ctype, w2, ok)
      winc = w2 - w1 
c
      end
c
c
c* ppconCG -- Convert unbinned full image pixels to binned subimage pixels
c& nebk
c: plotting
c+
c
      subroutine ppconcg (id, blc, bin, p)
c
      implicit none
      integer id, blc, bin
      double precision p
c    
c  Convert pixel values from a full image unbinned pixel to
c  a subimage binned pixel, and vice versa
c
c  Input
c   id        Direction of convsersion
c                1 -> p      -> pb-sub
c                2 -> pb-sub -> p
c   blc       BLC (in full image unbinned pixels) at which subimage begins
c   bin       Pixel increment with which we are stepping through image
c  Input/output
c   p         Pixel with bin=1 and blc=1 OR pixel appropriate to BIN
c             and BLC
c-----------------------------------------------------------------------
      if (id.eq.1) then
c
c Convert to subimage pixels
c
        p = p - blc + 1
c
c Convert to binned subimage pixel
c
        p = (p-0.5d0)/dble(bin) + 0.5d0
      else if (id.eq.2) then
c
c Convert to unbinned subimage pixel
c
        p = dble(bin)*(p-0.5d0) + 0.5d0
c
c Convert to full image unbinned pixel
c
        p = p + blc - 1
      end if
c
      end
c
c* readbCG -- Read in mask image mask
c& nebk
c: plotting
c+
c
      subroutine readbcg (init, lun, ibin, jbin, krng, blc, trc, 
     +                    bimage, blanks)
c
      implicit none
      logical bimage(*)
      integer lun, blc(*), trc(*), ibin(2), jbin(2), krng(2)
      logical blanks, init
c
c  Read in the blanking mask from the specified window from the image
c  When reading more than one plane, the mask image pixel is considered
c  blanked if any of the planes are blanked at that pixel.  When 
c  spatially binning images, a binned pixel is considered blanked if
c  any of the input pixels were blanked.
c
c  Input:
c    init        If true initialize BINMAGE to all good first
c    lun         Handle of image
c    ibin        Increment and average for i direction
c    jbin        Increment and average for j direction
c    krng        First pixel in k direction to read and number of
c                pixels to average
c    blc,trc     Window to read
c  Input/Output
c    blanks      True if there are blanked pixels in bimage
c  Output
c    bimage      Masking image.  True means a good pixel (unflagged) and
c                false means bad (flagged pixel).  Will be bad if any
c                pixel in spectral range is bad for each spatial pixel
c
c--
c------------------------------------------------------------------------
      include 'maxdim.h'
      integer i, j, k, ii, jj, pi, po, kst, kav, kend, io, jo,
     +  nii, nji, nio, njo, no
      logical good(maxdim)
c------------------------------------------------------------------------
c  
c Find size of unbinned and binned image 
c 
      nii = trc(1) - blc(1) + 1
      nji = trc(2) - blc(2) + 1
      if (ibin(2).ne.1) then
        nio = nii / ibin(1) 
      else
        nio = (nii-1)/ibin(1) + 1
      end if
      if (jbin(2).ne.1) then
        njo = nji / jbin(1) 
      else
        njo = (nji-1)/jbin(1) + 1
      end if
c
c Initialize
c
      no = nio * njo
      if (init) then
        do i = 1, no
          bimage(i) = .true.
        end do
        blanks = .false.
      end if
      do i = 1, maxdim
        good(i) = .true.
      end do
c
c Read in plane(s)
c
      kst = krng(1)
      kav = krng(2)
      kend = min(trc(3),kst+kav-1)
c
      do k = kst, kend
        call xysetpl (lun, 1, k)
c
c Step through rows
c
        jo = 1
        do j = 1, nji, jbin(1)
          call xyflgrd (lun, j, good)
c
c Accumulate desired rows
c
          do jj = j, j+jbin(2)-1
            call xyflgrd (lun, jj+blc(2)-1, good)
c
c Step through row
c
            io = 1
            do i = 1, nii, ibin(1)
c 
c Accumulate desired pixels
c
              do ii = i, i+ibin(2)-1
c           
c Input row and output image pointers
c
                pi = ii + blc(1) - 1
                po = (jo-1)*nio + io
c           
c If any pixel in the binned region is bad, set the binned pixel to bad
c
                if (.not.good(pi)) then
                  bimage(po) = .false.
                  blanks = .true.
                end if
              end do
              io = io + 1
            end do
          end do
          jo = jo + 1
        end do
      end do
c
      end
c
c
c* readimCG -- Read in image dealing with averaging and blanking
c& nebk
c: plotting
c+
      subroutine readimcg (init, mask, blank, lun, ibin, jbin, krng, 
     +                     blc, trc, norm, nimage, image, blanks)
c
      implicit none
      real blank, image(*)
      integer nimage(*), lun, ibin(2), jbin(2), krng(2), blc(3),
     +  trc(3)
      logical blanks, mask, init, norm
c
c  Read in the specified window from the image and apply spatial
c  and spectral binning as desired
c
c  Input:
c    init        True to initialize output array and normalization
c                image first
c    mask        True if blanking mask present
c    blank       Value to use for magic blanking
c    lun         Handle of image
c    ibin        Increment and binning for i direction
c    jbin        Increment and binning for j direction
c    krng        First pixel in k direction to read and number of
c                pixels to average
c    blc,trc     Input window, in unbinned pixels, to read and bin
c    norm        If true, normalize the summed image before
c                exiting.  It is up to you to renitialize at	
c                the appropriate time with INIT on the next call
c  Output
c    nimage      Normalization image; it is the number of pixels
c                that were averaged together at each output pixel 
c                location.  Will be zero for blanked pixels
c    image       Output image (binned, normalized)
c    blanks      True if blanks in output image
c
c--
c------------------------------------------------------------------------
      include 'maxdim.h'
      real row(maxdim)
      logical good(maxdim)
      integer i, j, k, ii, jj, pi, po, kst, kav, kend, io, jo,
     +  nii, nji, nio, njo, no
c------------------------------------------------------------------------
c
c Find size of unbinned and binned images
c
      nii = trc(1) - blc(1) + 1 
      nji = trc(2) - blc(2) + 1
      if (ibin(2).ne.1) then
        nio = nii / ibin(1) 
      else
        nio = (nii-1)/ibin(1) + 1
      end if
      if (jbin(2).ne.1) then
        njo = nji / jbin(1) 
      else
        njo = (nji-1)/jbin(1) + 1
      end if
c
c Initialize
c
      no = nio * njo
      if (init) then
        do i = 1, no
          image(i) = 0.0
          nimage(i) = 0
        end do
        blanks = .false.
      end if
      do i = 1, maxdim
        good(i) = .true.
      end do
c
c Loop over planes
c
      kst = krng(1)
      kav = krng(2)
      kend = min(trc(3),kst+kav-1)
c
      do k = kst, kend
        call xysetpl (lun, 1, k)
c
c Step through rows
c
        jo = 1
        do j = 1, nji, jbin(1)
c
c Accumulate desired rows
c
          do jj = j, j+jbin(2)-1
            call xyread (lun, jj+blc(2)-1, row)
            if (mask) call xyflgrd (lun, jj+blc(2)-1, good)
c
c Step through row
c
            io = 1
            if (ibin(2).eq.1) then
c
c Faster route if no binning
c
              do i = 1, nii, ibin(1)
c
c Input row and output image pointers
c
                pi = i + blc(1) - 1
                po = (jo-1)*nio + io
c
                if (good(pi)) then
                  nimage(po) = nimage(po) + 1
                  image(po) = image(po) + row(pi)
                end if
                io = io + 1
              end do
            else
              do i = 1, nii, ibin(1)
c
c Accumulate desired pixels
c
                do ii = i, i+ibin(2)-1
c
c Input row and output image pointers
c
                  pi = ii + blc(1) - 1
                  po = (jo-1)*nio + io
c
                  if (good(pi)) then
                    nimage(po) = nimage(po) + 1
                    image(po) = image(po) + row(pi)
                  end if
                end do
                io = io + 1
              end do
            end if
          end do
          jo = jo + 1
        end do
      end do
c
c Normalize and blank 
c
      do i = 1, no
        if (nimage(i).ne.0) then
          if (norm) image(i) = image(i) / real(nimage(i))
        else
          blanks = .true.
          image(i) = blank
        end if
      end do
c
      end
c
c* setcolCG --  Set multiple line graphics PGPLOT colours
c& nebk
c: plotting
c+
      subroutine setcolcg (i, icol)
c
      implicit none
      integer i, icol
c
c  Return a PGPLOT colour index given a graph number, where you plan
c  to put many graphs with different colours on the one plot.  The
c  colours are chosen so that similar colours are not consecutive
c
c Input:
c   i      Graph number in the range 1 -> NGRAPH, where NGRAPH is the number
c          of graphs that will be drawn on the one plot
c Output:
c   icol   The colour index to set with PGSCI (ICOL)
c
c--
c-----------------------------------------------------------------------
      integer maxcol 
      parameter (maxcol = 13)
      integer lcols(maxcol), ip
c
      save lcols
      data lcols /2, 7, 5, 3, 1, 6, 8, 12, 4, 10, 11, 9, 13/
c------------------------------------------------------------------------
      ip = mod(i,maxcol)
      if (ip.eq.0) ip = maxcol
      icol = lcols(ip)
c
      end
c
c* setdesCG --  Set axis descriptors fro an image by copying from another
c& nebk
c: plotting
c+
      subroutine setdescg (naxis1, size1, crval1, cdelt1, crpix1, 
     +   ctype1, epoch1, naxis2, size2, crval2, cdelt2, crpix2, 
     +   ctype2, epoch2)
c
      implicit none
      integer naxis1, size1(naxis1), naxis2, size2(naxis1)
      double precision crval1(naxis1), crval2(naxis1),
     +  cdelt1(naxis1), cdelt2(naxis1), crpix1(naxis1), 
     +  crpix2(naxis1)
      real epoch1, epoch2
      character*(*) ctype1(naxis1), ctype2(naxis1)
c
c  Copy axis descriptors from one set to another
c
c--
c-----------------------------------------------------------------------
      integer i
c------------------------------------------------------------------------
      naxis2 = naxis1
      do i = 1, naxis1
        size2(i) = size1(i)
        crval2(i) = crval1(i)
        cdelt2(i) = cdelt1(i)
        crpix2(i) = crpix1(i)
        ctype2(i) = ctype1(i)
      end do
      epoch2 = epoch1
c
      end
c
c* strprpCG -- Prepare string; strip extra white space & delimiter with commas
c& nebk
c: plotting
c+
      subroutine strprpcg (maxloc, aline, comloc, nfield, lena)
c
      implicit none
      character*(*) aline
      integer nfield, maxloc, comloc(maxloc), lena
c
c     Take a string with a number of mixed ascii/numeric fields in it
c     and prepare it for use by stripping out extra white space and
c     replacing the space delimiters by commas (matod needs this).
c
c     Input:
c       maxloc  Maximum number of fields allowed in string
c     Input/output:
c       aline   String
c     Output
c       comloc  Locations along string of comma delimiters for
c               each field.  comloc(1) is the comma between the
c               first and second fields etc
c       nfield  Number of fields in string
c       lena    Length of output string after massaging
c--
c---------------------------------------------------------------------
      integer i, j, lenb, idx
      character bline*132
c
      integer len1
c--------------------------------------------------------------------
c
c Strip leading white space
c
      idx = 1
      do while (aline(idx:idx).eq.' ')
        idx = idx + 1
      end do
      bline = aline(idx:)
      aline = ' '
      aline = bline
c
c Strip additional white space out. Catch cases where commas 
c already the separator too
c
      bline = ' '
      lena = len1(aline)
      bline(1:1) = aline(1:1)
      j = 2
      do i = 2, lena
        if ((aline(i:i).eq.' ' .and. aline(i-1:i-1).eq.' ') .or.
     +      (aline(i:i).eq.' ' .and. aline(i-1:i-1).eq.',')) then
          continue
        else
          bline(j:j) = aline(i:i)
          j = j + 1
        end if
      end do
c
c Replace spaces and colons (which may come from RA or DEC formatted
c strings) by commas (for matodf) and count how many fields there are
c
      lenb = len1(bline)
      nfield = 0
      do i = 1, lenb
        if (bline(i:i).eq.' ' .or. bline(i:i).eq.':' .or.
     +      bline(i:i).eq.',') then
          bline(i:i) = ','
          nfield = nfield  + 1
          if (nfield.gt.maxloc) call bug ('f',
     +      'STRPRPCG: Too many fields for internal storage')
          comloc(nfield) = i
        end if
      end do
      nfield = nfield + 1
      if (bline(lenb:lenb).eq.',') then
        nfield = nfield - 1
        lenb = lenb - 1
      end if
      aline = bline
      lena = lenb
c
      end 
c
c* subincCG -- Step to next sub-plot
c& nebk
c: plotting
c+
      subroutine subinccg (iplot, nx, ny, vxmin, vymax, vxsize, vysize, 
     +                     vxgap, vygap, vx, vy)
c
      implicit none
      real vxmin, vymax, vxsize, vysize, vxgap, vygap, vx, vy
      integer iplot, nx, ny
c
c  Increment view port locations ready for next sub-plot
c 
c  Input
c    iplot    Current sub-plot number
c    nx,ny    Number of sub-plots in x and y directions on view-surface
c    vxmin    minimum x location of encompassing viewport (ndc)
c    vymax    maximum y location of encompassing viewport (ndc)
c    vx,ysize Size of sub-plots on view-surface (ndc)
c    vx,ygap  Gap between sub-plots on the view-surface (ndc)
c  Input/output
c    vx,vy    Location of blc of next sub-plot on view-surface
c--
c-----------------------------------------------------------------------
      if (mod(iplot,nx*ny).eq.0) then
        vx = vxmin
        vy = vymax - vysize
      else if (mod(iplot,nx).eq.0) then
        vx = vxmin
        vy = vy - vygap - vysize
      else
        vx = vx + vxgap + vxsize
      end if
c
      end
c
c
c* sunitCG -- Set axis units given axis type
c& nebk
c: plotting
c+
      subroutine sunitcg (ctype, labtyp, units)
c
      implicit none
      character*(*) labtyp, units, ctype
c
c  Set the units of a pixel based upon the requested labelling
c  type and the axis type.  Used for ascii not graphical output
c  so no PGPLOT escape sequences.
c
c  Inputs:
c    ctype  Axis header CTYPE value
c    labtyp Axis type
c  Output:
c    units  Axis units
c--
c-----------------------------------------------------------------------
      integer ifrq, ivel, irad, iuv
c-----------------------------------------------------------------------
      if (labtyp.eq.'hms' .or. labtyp.eq.'dms') then
        units = ' '
      else if (labtyp.eq.'arcsec') then
        units = 'arcsec'
      else if (labtyp.eq.'absdeg') then
        units = 'degrees'
      else if (labtyp.eq.'reldeg') then
        units = 'offset degrees'
      else if (labtyp.eq.'abspix' .or. labtyp.eq.'none') then
        units = 'pixels'
      else if (labtyp.eq.'relpix') then
        units = 'offset pixels'
      else if (labtyp.eq.'absghz') then
        units = 'GHz'
      else if (labtyp.eq.'relghz') then
        units = 'offset GHz'
      else if (labtyp.eq.'abskms') then
        units = 'Km/s'
      else if (labtyp.eq.'relkms') then
        units = 'offset Km/s'
      else 
        call axfndcg ('RAD',  1, ctype, irad)
        call axfndcg ('FREQ', 1, ctype, ifrq)
        call axfndcg ('VELO', 1, ctype, ivel)
        call axfndcg ('UV',   1, ctype, iuv)
c
        if (labtyp.eq.'abslin') then
          if (irad.eq.1) then
            units = 'radians'
          else if (ifrq.eq.1) then
            units = 'GHz'
          else if (ivel.eq.1) then
            units = 'Km/s'
          else if (iuv.eq.1) then
            units = 'wavelengths'
          else
            units = ' '
          end if
        else if (labtyp.eq.'rellin') then
          if (irad.eq.1) then
            units = 'offset radians'
          else if (ifrq.eq.1) then
            units = 'offset GHz'
          else if (ivel.eq.1) then
            units = 'offset Km/s'
          else if (iuv.eq.1) then
            units = 'offset wavelengths'
          else
            units = 'offset'
          end if
        end if
      end if
c
      end
c
c* wedginCG -- See if grey scale wedges are inside or outside subplots
c& nebk
c: plotting
c+
      subroutine wedgincg (dowedge, nx, ny, npixr, trfun, wedcod)
c
      implicit none
      logical dowedge
      integer nx, ny, npixr, wedcod
      character trfun*3
c
c Work out whether the grey scale wedges are to be drawn inside
c or outside the subplots, and whether there will be one or many
c  
c Input
c  dowedge   True if user requests wedge
c  nx,ny     Number of subplots in x and y directions
c  npixr     NUmber of grey scale "range" groups given by user
c  trfun     Transfer function type of first "range" group
c Output
c wedcod     1 -> one wedge to right of all subplots
c            2 -> one wedge to right per subplot
c            3 -> one wedge per subplot inside subplot
c--
c-----------------------------------------------------------------------
      if (dowedge) then
        if (nx*ny.eq.1 .or. (npixr.eq.1 .and. trfun.ne.'heq')) then
          wedcod = 1
        else if (ny.gt.1.and.nx.eq.1 .and. ((npixr.eq.1 .and. 
     +           trfun.eq.'heq') .or. npixr.gt.1)) then
          wedcod = 2
        else 
          wedcod = 3
        end if
      else    
        wedcod = 0
      end if
c
      end
c
c* winfidcg - adjust window size to fit integral number of bins
c& nebk
c: plotting
c+
      subroutine winfidcg (size, axis, bin, blc, trc, win)
      implicit none
      integer axis, bin(2), blc, trc, size, win
c
c     Adjust the size of the window so that the bin width fits
c     an integer number of times 
c
c     Input:
c        size    SIze of total available image
c        axis    Axis number 
c        bin     Pixel increment and binning width across image
c     Input/output
c        blc,trc Window in pixels, adjusted if necessary to fit
c		 an integral number of bins
c     Output
c        win     Size of binned window
c
c--
c-----------------------------------------------------------------------
      integer lo, hi, rem, size2, bin2
      logical new, fail
      character aline*80
c-----------------------------------------------------------------------
c
c Don't fiddle width if no binning, READIMCG and READBCG will cope
c
      if (bin(2).eq.1) then
        win = ((trc-blc+1)-1)/bin(1) + 1
        return
      end if
c
c If the binning width is not unity, the increment must already
c hae been set to the same number
c
      bin2 = bin(2)
      lo = blc
      hi = trc
      new = .false.
      fail = .false.
      size2 = hi - lo + 1
      rem = mod(size2,bin2)
c
c If no adjustement needed, bug out now
c
      if (rem.eq.0) then
        win = (trc-blc+1) / bin2
        return
      end if
c
c Adjust window to fit integral number of bins.  Decrement BLC by 1
c and increment TRC by 1 until ok.
c
      do while (rem.ne.0 .and. .not.fail)
        if (blc.eq.1 .and. trc.eq.size) fail = .true.
c
        if (.not.fail) then
          blc = max(blc-1,1)
          size2 = trc - blc + 1
          rem = mod(size2,bin2)
c
          if (rem.ne.0) trc = min(trc+1,size)
          size2 = trc - blc + 1
          rem = mod(size2,bin2)
c
          new = .true.
        end if
      end do
c
      if (fail) then
c
c We failed by making the window smaller, try making it bigger
c
        size2 = hi - lo + 1
        rem = mod(size2,bin2)
        new = .false.
        fail = .false.
        do while (rem.ne.0 .and. .not.fail)
          if (blc+bin2.gt.trc) fail = .true.
c
          if (.not.fail) then
            blc = blc + 1
            size2 = trc - blc + 1
            rem = mod(size2,bin2)
c
            if (rem.ne.0) trc = trc - 1
            size2 = trc - blc + 1
            rem = mod(size2,bin2)
c
            new = .true.
          end if
        end do
      end if
c
c Tell user what happened
c
      if (fail) then
        call bug ('f', 'Can''t adjust spatial window to contain'//
     +            ' an integral number of bins')
      else if (new) then
        write (aline, 100) axis, lo, hi, blc, trc, bin(2)
100     format ('Adjusted axis ', i1, ' window from ', i4, ',', i4,
     +          ' to ', i4, ',', i4, ' to fit bin width ',i4)
        call output (aline)
      end if
c
c Size of binned window
c
      win = size2 / bin2
c
      end
c
c* w2pixCG -- Convert world coordinate of given type to image pixels
c& nebk
c: plotting
c+
      subroutine w2pixcg (world, iax, labtyp, naxis, crval, crpix, 
     +                    cdelt, ctype, pixel, ok)
c
      implicit none
      integer naxis, iax
      double precision cdelt(naxis), crval(naxis), crpix(naxis),
     +  world, pixel
      character ctype(naxis)*(*), labtyp*(*)
      logical ok
c
c  Convert world coordinate of given type to image pixels
c
c  Input:
c    world   World coordinate.  
c    iax     This is the axis number in which we are interested. Should
c            be 1, 2 or 3.
c    labtyp  Given type of world coordinate.   Should be one
c            of   abspix, relpix, arcsec, hms, dms, absghz, relghz,
c                 abskms, relkms, abslin, rellin, none.  For RA axes
c            linear coordinates are assumed to be in radians of
c            polar rotation.  For labtyp=hms and labtyp=dms the 
c            world coordinate is in seconds of time and seconds of arc
c    naxis   Number of axes in image
c    crval   Array of image reference values
c    crpix   Array of image reference pixels
c    cdelt   Array of image pixel increments
c    ctype   Array of image axis types
c  Output:
c    pixel   Image pixel
c    ok      If false, the requested type conflicted with the actual
c            axis type.  The pixel was calculated as if there was 
c            no conflict.
c--
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxnax.h'
      double precision a2r, d2r
      integer max2
c
      parameter (a2r = dpi / (180.0d0 * 3600.0d0), max2 = 9 * maxnax,
     +           d2r = dpi / 180.0d0)
c
      double precision rad, delra, rainc, cosdec
      integer ira, idec, ifrq, ivel, len1, irad
      logical warn(9,maxnax), cdok
      character msg*80
      save warn
      data warn /max2*.true./
c-----------------------------------------------------------------------
      if (iax.lt.1 .or. iax.gt.naxis) 
     +  call bug ('f', 'W2PIXCG: Invalid axis number')
c
      call axfndcg ('RA',   naxis, ctype, ira)
      call axfndcg ('DEC',  naxis, ctype, idec)
      call axfndcg ('FREQ', naxis, ctype, ifrq)
      call axfndcg ('VELO', naxis, ctype, ivel)
      call axfndcg ('RAD', 1, ctype(iax), irad)
      call cosdeccg (iax, naxis, ctype, crval, cosdec, cdok)
c
c Convert to pixels from world coordinate depending upon type
c 
      ok = .true.
      if (labtyp.eq.'abspix' .or. labtyp.eq.'none') then
c
c Absolute pixels ('none' masquerading as 'abspix')
c
        pixel = world
      else if (labtyp.eq.'relpix') then
c
c Relative pixels
c
        pixel = crpix(iax) + world
      else if (labtyp.eq.'arcsec') then
c
c Relative arcseconds
c
        if (irad.eq.0) then
          write (msg, 100) iax
100       format ('W2PIXCG: Axis ', i1, ' is not RA,DEC,LAT,LON but ',
     +            'conversion from "arcsec"')
          call bug ('w', msg)
          call bug ('w', 
     +      'W2PIXCG: requested.  Continue assuming axis in radians')
          warn(1,iax) = .false.
          ok = .false.
        end if
c
        pixel = world * a2r / cdelt(iax) + crpix(iax)
      else if (labtyp.eq.'hms') then
c
c HH MM SS.S
c
        if (iax.ne.ira .and. warn(2,iax)) then
          write (msg, 200) iax
200       format ('W2PIXCG: Axis ', i1, ' is not RA but conversion ',
     +            'from "hms"')
          call bug ('w', msg)
          call bug ('w', 
     +      'W2PIXCG: requested.  Continue assuming axis in radians')
          warn(2,iax) = .false.
          ok = .false.
        end if
c
        if (idec.eq.0) call bug ('f', 
     +    'W2PIXCG: No DEC axis found; cannot convert from "hms"')
c
c Convert world coordinate in seconds of time to radians
c
        rad = 15.d0 * world * a2r
c
c Take smallest distance between reference value and location
c
        delra = rad - crval(iax)
        if (abs(delra).gt.dpi) then
          if (delra.gt.0.0) then
            delra = delra - 2.0*dpi
          else
            delra = delra + 2.0*dpi
          end if
        end if
c
c Finally convert to pixels
c
        pixel = delra*cosdec/cdelt(iax) + crpix(iax)
      else if (labtyp.eq.'dms') then
c
c DD MM SS.S
c
        if (iax.ne.idec .and. warn(3,iax)) then
          write (msg, 300) iax
300       format ('W2PIXCG: Axis ', i1, ' is not DEC but conversion ',
     +            'from "dms"')
          call bug ('w', msg)
          call bug ('w', 
     +      'W2PIXCG: requested.  Continue assuming axis in radians')
          warn(3,iax) = .false.
          ok = .false.
        end if
c
        rad = world * a2r
        pixel = (rad-crval(iax))/cdelt(iax) + crpix(iax)
      else if (labtyp.eq.'absghz') then
c
c Absolute frequency
c
        if (iax.ne.ifrq .and. warn(4,iax)) then
          write (msg, 400) iax
400       format ('W2PIXCG: Axis ', i1, 
     +            ' is not FREQ but conversion from "absghz"')
          call bug ('w', msg)
          call bug ('w', 
     +    'W2PIXCG: requested. Continue assuming axis in GHz')
          warn(4,iax) = .false.
          ok = .false.
        end if
        pixel = (world - crval(iax)) / cdelt(iax) + crpix(iax)
      else if (labtyp.eq.'relghz') then
c
c Relative frequency
c
        if (iax.ne.ifrq .and. warn(5,iax)) then
          write (msg, 500) iax
500       format ('W2PIXCG: Axis ', i1, 
     +            ' is not FREQ but conversion from "relghz"')
          call bug ('w', msg)
          call bug ('w', 
     +    'W2PIXCG: requested. Continue assuming axis in GHz')
          warn(5,iax) = .false.
          ok = .false.
        end if
        pixel = world / cdelt(iax) + crpix(iax)
      else if (labtyp.eq.'abskms') then
c
c Absolute velocity
c
        if (iax.ne.ivel .and. warn(6,iax)) then
          write (msg, 600) iax
600       format ('W2PIXCG: Axis ', i1, 
     +            ' is not VELO but conversion from "abskms"')
          call bug ('w', msg)
          call bug ('w', 
     +      'W2PIXCG: requested. Continue assuming axis in Km/s')
          warn(6,iax) = .false.
          ok = .false.
        end if
        pixel = (world - crval(iax)) / cdelt(iax) + crpix(iax)
      else if (labtyp.eq.'relkms') then
c
c Relative velocity
c
        if (iax.ne.ivel .and. warn(7,iax)) then
          write (msg, 700) iax
700       format ('W2PIXCG: Axis ', i1, 
     +            ' is not VELO but conversion from "relkms"')
          call bug ('w', msg)
          call bug ('w', 'W2PIXCG: "relkms" requested. '//
     +                   'Continue assuming axis in Km/s')
          warn(7,iax) = .false.
          ok = .false.
        end if
        pixel = world / cdelt(iax) + crpix(iax)
      else if (labtyp.eq.'abslin') then
c
c Absolute linear coordinate.  For RA convert from radians of
c polar rotation.
c
        if (iax.eq.ira .and. idec.eq.0) then
          write (msg, 800) iax
800       format ('PIX2WCG: conversion for RA axis ', i1, 
     +            ' from "abslin" to pixels requested')
          call output (msg)
          msg = 'PIX2WCG: but no DEC axis in image.  Cannot convert'
          call output (msg)
          call bug ('f', ' ')
        end if
c
        rainc = cdelt(iax) / cosdec
        pixel = (world - crval(iax)) / rainc + crpix(iax)
      else if (labtyp.eq.'rellin') then
c
c Relative linear coordinate.  For RA convert from radians of
c polar rotation.
c
        if (iax.eq.ira .and. idec.eq.0) then
          write (msg, 900) iax
900       format ('PIX2WCG: conversion for RA axis ', i1, 
     +            ' from "rellin" to pixels requested')
          call output (msg)
          msg = 'PIX2WCG: but no DEC axis in image.  Cannot convert'
          call output (msg)
          call bug ('f', ' ')
        end if
c
        rainc = cdelt(iax) / cosdec
        pixel = world / rainc + crpix(iax)
      else if (labtyp.eq.'reldeg') then
c
c Relative degrees
c
        if (irad.eq.0) then
          write (msg, 910) iax
910       format ('W2PIXCG: Axis ', i1, ' is not RA,DEC,LON,LAT but ',
     +            'conversion from "reldeg"')
          call bug ('w', msg)
          call bug ('w', 
     +      'W2PIXCG: requested.  Continue assuming axis in radians')
          warn(8,iax) = .false.
          ok = .false.
        end if
c
        pixel = world * d2r / cdelt(iax) + crpix(iax)
      else if (labtyp.eq.'absdeg') then
c
c Absolute degrees 
c
        if (irad.eq.0) then
          write (msg, 920) iax
920       format ('W2PIXCG: Axis ', i1, ' is not RA,DEC,LON,LAT but ',
     +            'conversion from "absdeg"')
          call bug ('w', msg)
          call bug ('w', 
     +      'W2PIXCG: requested.  Continue assuming axis in radians')
          warn(9,iax) = .false.
          ok = .false.
        end if
c
        pixel = (world*d2r - crval(iax)) / cdelt(iax) + crpix(iax)
      else
        msg = 'W2PIXCG: '//labtyp(1:len1(labtyp))//
     +        ' is an unrecognized world coordinate type'
        call bug ('f', msg)
      end if
c
      end
c
c
c* w2wcg -- Convert from world coordinate to world coordinate 
c& nebk
c: plotting
c+
      subroutine w2wcg (domsg, win, iax, typein, typeout, naxis, crval,
     +                  crpix, cdelt, ctype, wout, ok)
c
      implicit none
      integer naxis, iax
      double precision cdelt(naxis), crval(naxis), crpix(naxis),
     +  win, wout
      character typein*(*), typeout*(*), ctype(naxis)*(*)
      logical ok, domsg
c
c  Convert from image world coordinate in one type to another.
c
c  Input:
c    domsg   If true, give a message warning when requested labtyp does 
c            not match the axis type in ctype.
c    win     Input world coordinate.  For typein=hms and typein=dms the 
c            world coordinate is in seconds of time and seconds of arc. For
c	     RA axes linear coordinates are assumed to be in radians of polar
c            rotation.That is, the increment will be divided by cos(DEC)
c    typein  Input world coordinate type.   Should be one
c            of   abspix, relpix, arcsec, hms, dms, absghz, relghz,
c                 abskms, relkms, abslin, rellin, none. 
c    typeout Output world coordinate type
c    iax     This is the axis number in which we are interested. Should
c            be 1, 2 or 3.
c    naxis   Number of axes in image
c    crval   Array of image reference values
c    crpix   Array of image reference pixels
c    cdelt   Array of image pixel increments
c    ctype   Array of image axis types
c  Output:
c    wout    Output world coordinate.  For labtyp=hms and labtyp=dms the 
c            world coordinate is in seconds of time and seconds of arc
c            ready for PGTBOX. A request for a linear (abs or rel) axis 
c	     conversion for an RA axis will return the RA in radians of
c	     polar rotation.  
c    ok      If false, then the requested labtyp is inconsistent with
c            the axis type.  WOrld will have been computed as if
c            the axis type was as expected.
c--
c-----------------------------------------------------------------------
      double precision pixel
c-----------------------------------------------------------------------
      if (typein.eq.'abspix') then
        call pix2wcg (domsg, win, iax, typein, naxis, crval, 
     +                crpix, cdelt, ctype, wout, ok)
      else 
        call w2pixcg (win, iax, typein, naxis, crval, crpix, 
     +                cdelt, ctype, pixel, ok)
        call pix2wcg (domsg, pixel, iax, typeout, naxis, crval, 
     +                crpix, cdelt, ctype, wout, ok)
      end if
c
      end
c
c
c* w2wfcg -- Convert from world coordinate to world coordinate and format
c& nebk
c: plotting
c+
      subroutine w2wfcg (win, iax, typein, typeout, naxis, crval, 
     +                   crpix, cdelt, ctype, nounit, str, ilen)
c
      implicit none
      integer naxis, iax, ilen
      double precision cdelt(naxis), crval(naxis), crpix(naxis), win
      character typein*(*), typeout*(*), ctype(naxis)*(*), str*(*)
      logical nounit
c
c  Convert from image world coordinate in one type to another.
c
c  Input:
c    win     Input world coordinate.  For typein=hms and typein=dms the 
c            world coordinate is in seconds of time and seconds of arc. For
c	     RA axes linear coordinates are assumed to be in radians of polar
c            rotation.That is, the increment will be divided by cos(DEC)
c    typein  Input world coordinate type.   Should be one
c            of   abspix, relpix, arcsec, hms, dms, absghz, relghz,
c                 abskms, relkms, abslin, rellin, none. 
c    typeout Output world coordinate type
c    iax     This is the axis number in which we are interested. Should
c            be 1, 2 or 3.
c    naxis   Number of axes in image
c    crval   Array of image reference values
c    crpix   Array of image reference pixels
c    cdelt   Array of image pixel increments
c    ctype   Array of image axis types
c    nounit  If true don't append units to string
c  Output:
c    str     Output world coordinate in formatted string.
c    ilen    Length of string
c--
c-----------------------------------------------------------------------
      double precision pixel
      logical ok
c-----------------------------------------------------------------------
      call w2pixcg (win, iax, typein, naxis, crval, crpix, 
     +              cdelt, ctype, pixel, ok)
      call pix2wfcg (typeout, iax, pixel, naxis, crval, crpix, 
     +               cdelt, ctype, nounit, str, ilen)
c
      end
