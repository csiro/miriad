c************************************************************************
c Some routines to manipulate the ofm (output function map or colour
c lookup table) of a PGPLOT device.  It is expected that the image will 
c have been displayed with PGIMAG (not PGGRAY or PGPIXL). To use PGIMAG
c you should first load a lookup table with PGSCR and then call PGIMAG.
c However, these routines do not know about it, and will overwrite
c it with their own as soon as you go into the layer that loads colour
c tables.
c 
c These routines work by first defining a BASIC 256 level ofm selected
c from a variety of types.   This is then spline fit and resampled for 
c the number of levels appropriate to the PGPLOT device you are displaying
c on to produce the ACTIVE ofm. Some ofm types can be generated 
c algorithmically and these do not go through the spline fitting stage;
c the ACTIVE table is computed directly.  You can then can fiddle the 
c ofm interactively by manipulating the transfer function. A copy of the
c unfiddled ACTIVE ofm is kept in the SAVE ofm so that it can be reset 
c when necessary.
c
c
c Routines are as follows.  Those with an * are designed for the 
c user to call.  You should always call OFMINI to begin with.
c
c *  ofmini      Initialize ofm routines
c *  ofmmod      Interactively modify the ofm
c *  ofmcol      Apply a specified colour ofm
c *  ofmcmp      Apply complement of current OFM if b&w
c
c    ofmapp      Apply the ACTIVE ofm to the PGPLOT device
c    ofmevl      Evaluate BASIC ofm spline coefficients and 
c                tabulate an ACTIVE  ofm for the PGPLOT device
c    ofmfit      Spline fit the BASIC ofm
c    ofmheq      Apply histogram equalization transfer function
c    ofml1m      Issue instruction message on return to layer 1 
c    ofmlin      Apply linear transfer function of different slopes 
c                and offset as given by cursor position
c    ofmlnf      Apply linear transfer function of slope 1 offset 0
c    ofmlog      Apply logarithmic transfer function
c    ofmrep      Repplicate ofm by factor of 2
c    ofmrev      Reverse ACTIVE and SAVE ofms
c    ofmrsf      Restore last fiddle to ACTIVE table
c    ofmsel      Select with cursor an ofm type (b&w etc) and apply it
c    ofmsqr      Apply square root transfer function
c    ofmtabw     Tabulate absolute B&W ACTIVE and SAVE tables
c    ofmtba      Tabulate an ACTIVE ofm for the PGPLOT device
c    ofmtbb      Tabulate a BASIC ofm
c    ofmtbw      Tabulate B&W ACTIVE and SAVE tables
c    ofmtcc      Tabulate RDE colour contours ACTIVE and SAVE tables
c    ofmtfe      Erase the transfer function plot
c    ofmtff      Use the cursor to fiddle the ACTIVE ofm transfer 
c                function and apply it
c    ofmtfp      Plot the transfer function
c    ofmuin      Get input from cursor if possible else terminal 
c
c
c
c  *** Call tree for OFMMOD ***
c  
c     ofmuin
c     ofmsel - ofmuin
c            - ofmtba - ofmtbw
c                     - ofmtcc
c                     - ofmtbb
c                     - ofmfit
c                     - ofmevl
c            - ofmrsf
c            - ofmrev
c            - ofmapp
c            - ofmrep
c            - ofml1m
c
c     ofmtff - ofmuin
c            - ofmlin
c            - ofmheq
c            - ofmlog
c            - ofmsqr
c            - ofmlnf
c            - ofmapp
c            - ofmtfp - ofmtfe 
c            - ofml1f
c
c  *** Call tree for OFMCOL ***
c  
c     ofmtba - ofmtbw
c            - ofmtcc
c            - ofmtbb
c            - ofmfit
c            - ofmevl
c     ofmrev
c     ofmapp
c
c
c
c Note that the colour postscript drivers do not have a lookup table 
c concept.  However, if the colour indices are set before the postscript 
c file is written then they will be reproduced.  Thus, you can call
c OFMMOD *before* calling PGIMAG.  It is not necessary to pass an all
c zero transfer function viewport.  The software will detect that for
c hard copy devices it should not draw the transfer function plot. You
c will not have access to the linear fiddler, but you will have access
c to the different colour tables and the built in transfer functions.
c
c
c History:
c   nebk 17jan94  Enlightenment brings creation
c   nebk 25feb94  Add fixed zero colour contours & hardcopy device
c                 capability
c   nebk 03aug94  Add replication capability (ofmrep)
c   nebk 05jan95  Replace PGQCOL with new PGQCIR and make some other minor
c                 changes for the use of new PGIMAG instead of PGGRAY
c                 Remove OFMRAP as no longer needed with PGIMAG
c   nebk 10feb95  Add OFMCOL and OFMINQ
c   nebk 10apr95  Add OFMTABW
c   nebk 14apr95  Add OFMCMP. Remove OFMINQ
c***********************************************************************
c
      subroutine ofmapp 
c-----------------------------------------------------------------------
c     Apply the current ACTIVE ofm
c
c   Input in common
c     na      Number of levels
c     ofma    ACTIVE ofm
c     ci1     First colour index to use
c   Output in common
c     ofmdun  OFM in ACTIVE table has been applied to device
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      integer i
c-----------------------------------------------------------------------
      do i = 1, na
        call pgscr (ci1+i-1, ofma(i,1), ofma(i,2), ofma(i,3))
      end do
      call pgupdt
c
c Signify ACTIVE table applied to device
c
      ofmdun = .true.
c
      end
c
c
c* OfmCol -- Apply a colour OFM specified by the user
c& nebk
c: PGPLOT,display
c+
      subroutine ofmcol (jofm, imin, imax)
c
      implicit none
      real imin, imax
      integer jofm
c
c  Apply a colour lookup table as offered by the user
c
c  Input 
c    imin,max   Image min and max that PGIMAG was called with. Only
c		 used for iofm=5
c    jofm    Type of ofm
c               1 => B&W
c               2 => RJS rainbow 
c               3 => Tody linear pseudo colour
c               4 => RDE floating zero colour contours
c               5 => RDE fixed zero colour contours
c               6 => RGB
c               7 => Background
c               8 => Heat
c	        9 => Absolute b&w
c            If negative, then reverse
c  Output in common
c    iofm    Type of ofm
c-
c-----------------------------------------------------------------------
      include 'ofm.h'
c-----------------------------------------------------------------------
c
c Do we have enough colour indices to play with ?
c
      if (na.eq.0) return
c
c Is it possible to generate the fixed zero colour contour ofm ?
c
      if (imin.lt.0.0 .and. imax.gt.0.0 .and.
     +    abs(imax).ge.abs(imin)) dofcc = .true.
      if (.not.dofcc .and. jofm.eq.5) then
        call bug ('w', 
     +    'Cannot generate fixed zero colour contours for this image')
        iofm = 1
      else
        iofm = abs(jofm)
      end if
c
c Generate the table, reverse it if needed, and apply
c
      call ofmtba (imin, imax)
      if (jofm.lt.0) call ofmrev
      call ofmapp
c
      end
c
c
c
c
c* OfmCmp -- Apply complement of b&w OFMs
c& nebk
c: PGPLOT,display
c+
      subroutine ofmcmp
      implicit none
c
c     Apply the b&w complement of the current OFM.   I.e. take 
c     one minus the current values of the OFMs.   Only active for
c     b&w tables
c
c  Input in common
c    na      Number of levels in ACTIVE ofm
c  Input/output in common
c    ofma    ACTIVE ofm
c    ofms    SAVE ofm = ACTIVE ofm at this point
c--
c-----------------------------------------------------------------------
      include 'ofm.h'
      integer i 
c-----------------------------------------------------------------------
c
c This only makes sense for b&w lookup tables so don't do anything
c for other types
c
      if (iofm.eq.1 .or. iofm.eq.9) then
        do i = 1, na
          ofms(i,1) = 1.0 - ofms(i,1)
          ofms(i,2) = 1.0 - ofms(i,2)
          ofms(i,3) = 1.0 - ofms(i,3)
          ofma(i,1) = 1.0 - ofma(i,1)
          ofma(i,2) = 1.0 - ofma(i,2)
          ofma(i,3) = 1.0 - ofma(i,3)
        end do
        call ofmapp
      end if
c
      end
c
c
      subroutine ofmevl 
c-----------------------------------------------------------------------
c     Evaluate the spline coefficients fit to the BASIC ofm and
c     generate the new ACTIVE ofm
c
c  Input in common
c    na      Number of levels in ACTIVE ofm
c    a,b,c   Spline coefficients
c    x1d,y1d Fitting arrays
c  Output in common
c    ofma    New ACTIVE ofm
c    ofms    New SAVE ofm = ACTIVE ofm at this point

c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      double precision x1d(maxlev), y1d(maxlev,3), a(maxlev,3),
     +  b(maxlev,3), c(maxlev,3), xo(maxlev)
      integer i
      double precision seval
c
      common /splco/ x1d, y1d, a, b, c
c-----------------------------------------------------------------------
c
c Generate abcissa array at which to sample the spline fit
c
      do i = 1, na
        xo(i) = i
      end do
c
c Loop over the number of levels for the new ACTIVE table
c
      do i = 1, na
c
c Evaluate the ACTIVE table for RGB. Ensure result is in the range [0,1]
c
        ofma(i,1) = min(1.0d0,max(0.0d0,
     +              seval(maxlev,xo(i),x1d,y1d(1,1),
     +                    a(1,1),b(1,1),c(1,1))))
        ofma(i,2) = min(1.0d0,max(0.0d0,
     +              seval(maxlev,xo(i),x1d,y1d(1,2),
     +                    a(1,2),b(1,2),c(1,2))))
        ofma(i,3) = min(1.0d0,max(0.0d0,
     +              seval(maxlev,xo(i),x1d,y1d(1,3),
     +                    a(1,3),b(1,3),c(1,3))))
c
c Store SAVE ofm before it gets fiddled
c
        ofms(i,1) = ofma(i,1)
        ofms(i,2) = ofma(i,2)
        ofms(i,3) = ofma(i,3)
      end do
c
      end
c
c
      subroutine ofmfit
c-----------------------------------------------------------------------
c     Fit the BASIC 256 level ofm with a spline 
c
c  Input in common
c   ofmb     BASIC ofm of 256 values
c   na       Number of levels on current PGPLOT device
c  Output in common
c   a,b,c    Spline coefficients
c   x1d,y1d  Fitting arrays
c
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      double precision x1d(maxlev), y1d(maxlev,3), a(maxlev,3),
     +  b(maxlev,3), c(maxlev,3)
      integer i
c
      common /splco/ x1d, y1d, a, b, c
c-----------------------------------------------------------------------
c
c Fill RGB arrays for fitting, scale the x-values so they 
c range from 0 -> na
c
      do i = 1, maxlev
        x1d(i) = real(na)*real(i)/real(maxlev)
c
        y1d(i,1) = ofmb(i,1)
        y1d(i,2) = ofmb(i,2)
        y1d(i,3) = ofmb(i,3)
      end do
c
c Compute spline coefficients
c
      do i = 1, 3
        call spline (maxlev, x1d, y1d(1,i), a(1,i), b(1,i), c(1,i))
      end do
c
      end
c
c     
      subroutine ofmheq (npix, image, mask, imin, imax)
c-----------------------------------------------------------------------
c     Generate new ACTIVE ofm from histogram equalization
c
c  Input
c    npix    Number of pixels in image
c    image   Image
c    mask    Mask
c    imin    Image minimum given to PGIMAG
c    imax    Image maximum given to PGIMAG
c  Input in common
c    na      Number of levels in ACTIVE ofm
c  Output in common
c    yt      Y values of transfer function for plot
c    ofma    ACTIVE ofm
c    fidun   Signal fiddle has been done
c    tflab   Transfer function plot titles
c
c-----------------------------------------------------------------------
      implicit none
      integer npix, mask(npix)
      real image(npix), imin, imax
cc
      include 'ofm.h'
      real fac, cum
      integer i, his(maxlev), cumhis(maxlev), idx
      save cumhis
c-----------------------------------------------------------------------
      call output ('Applying histogram equalization transfer function')
      tflab = 'h.e.'
c
c Initialize histograms and plots if have not already done it
c
      if (.not.hedun) then
        do i = 1, na
          his(i) = 0
          cumhis(i) = 0
          yt(i) = 0.0
        end do
c
c Generate image histogram
c
        fac = real(na-1) / (imax-imin)
        do i = 1, npix
          if (mask(i).gt.0) then
            idx = max(1,min(na,nint((image(i)-imin)*fac)+1))
            his(idx) = his(idx) + 1
          end if
        end do
c
c Generate cumulative histogram.  This histogram is discretized into 
c NA levels in both x and y
c
        cum = 0.0
        do i = 1, na
          cum = cum + his(i) 
          cumhis(i) = cum
        end do
      end if
c
c Now discretize the cumulative histogram and set the ofm
c
      fac = real(na-1) / real(npix)
      do i = 1, na
c
c This index converts the actual cumulative histogram
c value to the nearest discrete bin
c
        idx = max(1,min(na,nint(cumhis(i)*fac)+1))
c
c Set active ofm and fiddle index array
c
        ofma(i,1) = ofms(idx,1)
        ofma(i,2) = ofms(idx,2)
        ofma(i,3) = ofms(idx,3)
        fid(i) = idx
c
c Set transfer function plot array
c
        yt(i) = real(idx-1)/real(na-1)
      end do
      fidun = .true.
      hedun = .true.
c
      end
c
c* OfmIni -- Initialize OFM routines
c& nebk
c: PGPLOT,display
c+
      subroutine ofmini
c
      implicit none
c
c  Initialize ofm routines.
c
c All output in common
c   na    Number of available colour indices.  If 0, then
c         ofmini has decided it cannot function on this device
c         (too many or too few colour indices)
c--
      include 'ofm.h'
c
c Set state switches
c
      fidun = .false.
      ofmdun = .false.
      dofcc = .false.
      nocurs = .false.
c
c Get available colour indices on this device.  
c
      call pgqcir (ci1, ci2)
      na = ci2 - ci1 + 1
c      write (*,*) 'ci1,ci2=',ci1,ci2
      if (na.gt.maxlev) then
        call bug ('w',
     +   'OFMINI: Too many colours on this device for internal storage')
        na = 0
      else if (na.lt.3) then
        call bug ('w', 'OFMINI: Not enough colours on this device')
        na = 0
      end if
c
      end
c
c
      subroutine ofml1m
c-----------------------------------------------------------------------
c     Print message decribing layer 1 on exiting from layer 2
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
c-----------------------------------------------------------------------

      call output ('*******   Continue lookup table '//
     +             'modification *******')
      call output (' ')
      if (nocurs) then
        call output ('To select basic lookup table enter A')
        call output ('To modify transfer function  enter D')
        call output ('To exit                      enter X')
      else
        call output ('To select basic lookup table click '//
     +               'left   button (enter A)')
        call output ('To modify transfer function  click '//
     +               'middle button (enter D)')
        call output ('To exit                      click '//
     +               'right  button (enter X)')
      end if
      call output (' ')
c
      end
c
c
      subroutine ofmlin (x, y, domsg)
c-----------------------------------------------------------------------
c     Apply a linear transformation to the transfer function and modify
c     the ACTIVE ofm.  The cursor y location defines the slope of the 
c     line between 0 (cursor at top of viewport) and infinity (cursor at
c     bottom of viewport).  The cursor x location defines the offset of 
c     the line.  When the offset reaches (0,0) it switches between an 
c     x offset and a y offset so as to avoid infinities
c
c  Input
c    x,y     Cursor location in (-1,0.999) -> (1.0,0.001) window
c    domsg   Tell user what to do
c  Input in comon
c    na      Number of levels in ACTIVE table
c    ofms    SAVE ofm
c  Output in common
c    yt      Y values for transfer function plot
c    ofma    ACTIVE ofm
c    fid     Fiddle index array
c    fidun   Signal fiddle has been done
c    tflab   Current transfer function plot title
c-----------------------------------------------------------------------
      implicit none
      real x, y
      logical domsg
cc
      include 'ofm.h'
      real m, b, xv
      integer i, ci
c-----------------------------------------------------------------------
      if (domsg) then
        call output ('Modify linear transfer function ')
        if (iofm.ne.5) then
          call output ('       Y location controls slope')
          call output ('       X location controls offsets')
        else 
          call output ('  Y location controls slope')
          call output 
     +      ('  X/Y offsets forced to keep blue/green boundary at 0')
        end if
        call output (' ')
      end if
      tflab = 'lin'
c
c Fiddle the ofm. First find the slope of the transfer function
c The slope ranges between infinity and zero
c
      if (y.le.0.5) then
        m = 2.0 * y
      else
        m = 0.5 / (1.0-y)
      end if
c
c For fixed zero contour colours, we force the intercepts
c so that the boundary between blue and green is kept at 0.0
c
      if (iofm.eq.5) then
        if (m.gt.1.0) then
          x = real(ci0) / real(na) * (1.0 - 1.0/m)
        else
          x = -real(ci0) / real(na) * (1.0 - m)
        end if  
      end if
c  
c Now deal with the intercept. The user can change the slope of the 
c transfer function and the intercept.  The x intercept is changed 
c until it reaches 0, whereupon the intercept changed becomes the y 
c intercept. This avoids problems with infinities (thanks mhw).
c
      if (x.ge.0.0) then
c
c Here we use an x intercept so the equation of the transfer
c function is y = m*(x-xo)
c
        do i = 1, na
c
c Compute output value and save.
c
          xv = real(i-1)/real(na-1) - x
          yt(i) = min(1.0,max(0.0,m*xv))
          ci = 1 + nint((na-1)*yt(i))
          fid(i) = ci
c
c Set table
c 
          ofma(i,1) = ofms(ci,1)
          ofma(i,2) = ofms(ci,2)
          ofma(i,3) = ofms(ci,3)
        end do
      else
c
c Here we use a y intercept so the equation of the transfer
c function is y = m*x + b
c
        b = -x
        do i = 1, na
c
c Compute output value
c
          xv = real(i-1)/real(na-1)
          yt(i) = min(1.0,max(0.0,m*xv+b))
          ci = 1 + nint((na-1)*yt(i))
          fid(i) = ci
c
c Set new ACTIVE table from SAVE table
c
          ofma(i,1) = ofms(ci,1)
          ofma(i,2) = ofms(ci,2)
          ofma(i,3) = ofms(ci,3)
        end do
      end if
      fidun = .true.
c
      end
c
c
      subroutine ofmlnf
c-----------------------------------------------------------------------
c     Distribute the ofm levels linearly with slope 1 and offset 0
c
c  Input in common
c    na      Number of levels in ACTIVE table
c    ofms    SAVE ofm
c  Output in common
c    yt      Y values for transfer function plot
c    ofma    ACTIVE ofm
c    fidun   Signal fiddle has been done
c    fid     Fiddle index array
c    tflab   Current transfer function plot title
c
c-----------------------------------------------------------------------
      implicit none
cc
      include 'ofm.h'
      integer i
c-----------------------------------------------------------------------
      call output ('Applying linear transfer function')
      tflab = 'lin'
c
      do i = 1, na
c
c Set ACTIVE ofm
c
        ofma(i,1) = ofms(i,1)
        ofma(i,2) = ofms(i,2)
        ofma(i,3) = ofms(i,3)
c
c Set fiddle index array and transfer function plot
c
        fid(i) = i
        yt(i) = real(i-1)/real(na-1)
      end do
      fidun = .true.
c
      call ofmapp
c
      end
c
c
      subroutine ofmlog (imin, imax)
c-----------------------------------------------------------------------
c     Distribute the ofm levels logarithmically and set the
c     ACTIVE ofm
c
c  Input
c    imin,imax
c            Min and max intensities used in the call to PGIMAG
c  Input in common
c    na      Number of levels in ACTIVE table
c    ofms    SAVE ofm
c  Output in common
c    yt      Y values for transfer function plot
c    ofma    ACTIVE ofm
c    fidun   Signal fiddle has been done
c    fid     Fiddle index array
c    tflab   Current transfer function plot title
c
c-----------------------------------------------------------------------
      implicit none
      real imin, imax
cc
      include 'ofm.h'
      real ds, ls, imin2, imax2, ioff, lsmin, lds
      integer i, idx
c-----------------------------------------------------------------------
      call output ('Applying logarithmic transfer function')
      tflab = 'log'
c
c Add offset if necessary to make intensities all positive
c
      if (imin.le.0.0 .or. imax.le.0.0) then
        ioff = abs(min(imax,imin)) + abs(imax-imin)/100.0
        imin2 = imin + ioff
        imax2 = imax + ioff
      else
        imin2 = imin
        imax2 = imax
      end if
c
c Find linear intensity increment
c
      ds = (imax2 - imin2) / (na - 1)
c
c Find logarithmic intensity increment and minium value
c
      lsmin = log10(imin2)
      lds = (log10(imax2) - log10(imin2)) / (na - 1)
c
c Loop over levels
c
      do i = 1, na
c
c Log intensity in range log(imin2) to log(imax2)
c
        ls = log10((i-1)*ds + imin2)
c
c Find log bin that this linear intensity falls into
c
	idx = nint((ls-lsmin)/lds) + 1
c
c Set ACTIVE ofm
c
        ofma(i,1) = ofms(idx,1)
        ofma(i,2) = ofms(idx,2)
        ofma(i,3) = ofms(idx,3)
c
c Set fiddle index array and transfer function plot
c
        fid(i) = idx
        yt(i) = real(idx-1)/real(na-1)
      end do
      fidun = .true.
c
      end
c
c
c* OfmMod -- Interactively modify a PGPLOT ofm
c& nebk
c: PGPLOT,display
c+
      subroutine ofmmod (tfvpu, nu, image, mask, imin, imax)
c
      implicit none
      integer nu, mask(*)
      real tfvpu(4), image(*), imin, imax
c
c  Interactively modify the colour ofm on a PGPLOT device.  There are 
c  options to cycle through different colour ofms, and also to fiddle
c  the transfer function with predefined transfer functions, as well
c  as a linear fiddler
c
c  Input 
c    tfvpu     Viewport (in normalized device coordinates) to be used
c              to display the transfer function during ofm manipulation.  
c              TFVP(1)=xmin, TFVP(2)=ymin, TFVP(3)=xmax and TFVP(4)=ymax.    
c              Try and make it postage stamp sized somewhere out of the 
c              way. All zero means don't draw it.
c    nu        Number of pixels in displayed image
c    image     Displayed image.  This is used only for the histogram
c              equalization mode.  If you call ofmmod according to
c	             "call ofmmod (tfvp, 1, 0.0, 0, 0.0, 0.0)"  
c              h.e. mode will not be activated
c    mask      Image mask.  0 for blanked pixels otherwise good.  Is
c              used only for histogram equalization such that blanked
c	       pixels do not contribute to histograms. See above also.
c    imin,max  Min and max used in call to PGIMAG.  Must be set for
c              histogram equalization and logarithmic transfer
c              functions to work
c
c Input/output in common
c    fidun     .false. says no fiddle done to transfer function yet
c    ofmdun    .false. says no ofm has been applied to this device
c Output in common
c    dofcc     True if possible to generate fixed zero colour contours
c    hedun     Says no h.e. done yet. Always reset by OFMMOD
c    tfvp      Transfer function plot viewport
c    nocurs    True if device does not have a cursor
c-
c-----------------------------------------------------------------------
      include 'ofm.h'
      integer npix, il, i
      real x, y, x1, x2, y1, y2, xi, yi
      character ch*1, ans*3
      logical hard
c-----------------------------------------------------------------------
c
c Do we have enough colour indices to play with
c
      if (na.eq.0) return
c
c Does this device have a cursor ?
c
      call pgqinf ('CURSOR', ans, il)
      if (ans.eq.'NO') nocurs = .true.
c
c Is this a hardcopy device ?
c
      hard = .false.
      call pgqinf ('HARDCOPY', ans, il)
      if (ans.eq.'YES') hard = .true.
c
c Is it possible to generate the fixed zero colour contour ofm ?
c
      if (imin.lt.0.0 .and. imax.gt.0.0 .and. 
     +    abs(imax).gt.abs(imin)) dofcc = .true.
c
c If no ofm has been applied, apply the greyscale ofm.  With /ps
c devices, the ofm must be loaded before calling PGIMAG. WIth
c interactive devices, the ofm cam be loaded after PGIMAG.
c Also initialize fiddle array.
c
      if (.not.ofmdun) then
        iofm = 1
        call ofmtba (0.0, 0.0)
        call ofmapp
c
        do i = 1, na
          fid(i) = i
        end do
      end if
c
c Initialize other parameters.  Always reinit histogram equalization
c in case we are calling OFMMOD with a different image. If image
c min and max=0 signal to h.e. routine that we can't do it.
c
      npix = nu
      if (imin.eq.0.0 .and. imax.eq.0.0) npix = 0
      hedun = .false.
c
c Generate transfer plot abcissa
c
      do i = 1, na
        xt(i) = real(i-1) / real(na-1)
      end do
c
c Copy of transfer function plot viewport in common
c
      do i = 1, 4
        tfvp(i) = tfvpu(i)
      end do
      tfpic = .not.(tfvp(1).eq.0.0 .and. tfvp(2).eq.0.0 .and.
     +              tfvp(3).eq.0.0 .and. tfvp(4).eq.0.0)
      if (hard) tfpic = .false.
c
c Save current window and position cursor in middle
c
      call pgqwin (x1, x2, y1, y2)
      xi = (x1 + x2) / 2.0
      yi = (y1 + y2) / 2.0
c
c Enter the outer fiddle cursor loop
c
      call output (' ')
      call output 
     +('**********    Begin lookup table modification   **********')
      call output (' ')
      if (nocurs) then
        call output ('To select basic lookup table enter A')
        call output ('To modify transfer function  enter D')
        call output ('To exit                      enter X')
      else
        call output
     +  ('To select basic lookup table click left   button (enter A)')
        call output
     +  ('To modify transfer function  click middle button (enter D)')
        call output 
     +  ('To exit                      click right  button (enter X)')
      end if
      call output (' ')
c
      ch = ' '
      x = xi
      y = yi
      do while (ch.ne.'x')
c
c Get the user's selection
c
        call ofmuin (x, y, ch)
c
        if (ch.eq.'a') then
c
c Select different ofms
c
          call ofmsel (imin, imax)
        else if (ch.eq.'d') then
c
c Modify transfer function
c
          call ofmtff (npix, image, mask, imin, imax)
          x = xi
          y = yi
        else
c
c Exit
c
          ch = 'x'
          call ofmtfe
          call output (' ')
          call output ('Finish lookup table  modification')
          call output (' ')
        end if
      end do
c
      end
c
c
      subroutine ofmrep
c-----------------------------------------------------------------------
c     Replicate the OFM by a factor of 2
c                
c  Input in common
c     savact  Scratch array
c     savsav  Scratch array
c
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
cc
      integer i, j, ip
c-----------------------------------------------------------------------
      call output ('Replicating lookup table') 
c
c Save original ACTIVE and SAVE ofms
c
      do i = 1, na
        savact(i,1) = ofma(i,1)
        savact(i,2) = ofma(i,2)
        savact(i,3) = ofma(i,3)
c
        savsav(i,1) = ofms(i,1)
        savsav(i,2) = ofms(i,2)
        savsav(i,3) = ofms(i,3)
      end do
c
c Replicate. Very cheap algorithm.  Goes wrong if
c you replicate too much
c
      ip = 0
      do j = 1, 2
        do i = 1, na, 2
          if (ip.lt.na) then
            ip = ip + 1
            ofma(ip,1) = savact(i,1)
            ofma(ip,2) = savact(i,2)
            ofma(ip,3) = savact(i,3)
            ofms(ip,1) = savsav(i,1)
            ofms(ip,2) = savsav(i,2)
            ofms(ip,3) = savsav(i,3)
          end if
        end do
      end do
c
      end
c
c
      subroutine ofmrev 
c-----------------------------------------------------------------------
c     Reverse the ACTIVE ofm and replace the SAVE ofm by the
c     unfiddled reversed ofm
c
c  Input in common
c    na    Number of levels in ACTIVE/SAVE ofms
c    ofma  ACTIVE ofm
c    ofms  SAVE ofm
c    savact  Scratch array
c    savsav  Scratch array
c  Output in common
c    ofma  Reversed ACTIVE ofm
c    ofms  Reversed SAVE ofm
c
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
cc
      integer i
c-----------------------------------------------------------------------
c      call output ('Reversing lookup table') 
c
c Save original ACTIVE and SAVE ofms
c
      do i = 1, na
        savact(i,1) = ofma(i,1)
        savact(i,2) = ofma(i,2)
        savact(i,3) = ofma(i,3)
c
        savsav(i,1) = ofms(i,1)
        savsav(i,2) = ofms(i,2)
        savsav(i,3) = ofms(i,3)
      end do
c
c Reverse them
c
      do i = 1, na
        ofma(i,1) = savact(na-i+1,1)
        ofma(i,2) = savact(na-i+1,2)
        ofma(i,3) = savact(na-i+1,3)
c
        ofms(i,1) = savsav(na-i+1,1)
        ofms(i,2) = savsav(na-i+1,2)
        ofms(i,3) = savsav(na-i+1,3)
      end do
c
      end
c
c
      subroutine ofmrsf
c-----------------------------------------------------------------------
c     Restore the last fiddle to the ACTIVE ofm 
c
c  Input in common
c    na    Number of levels in ACTIVE/SAVE ofms
c    ofma  ACTIVE ofm
c    fidun True if fiddle done
c    fid   Fiddle index array
c    ofms  SAVE ofm
c  Output in common
c    ofma  Fiddled ACTIVE ofm
c
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
cc     
      integer i
c-----------------------------------------------------------------------
      if (fidun) then
        do i = 1, na
          ofma(i,1) = ofms(fid(i),1)
          ofma(i,2) = ofms(fid(i),2)
          ofma(i,3) = ofms(fid(i),3)
        end do
      end if
c
      end
c
c
      subroutine ofmsel (imin, imax)
c-----------------------------------------------------------------------
c     Basic ofm selection routine with cursor.  Last fiddle is applied
c
c  Input
c    imin,imax  Image minimum and maximum with which PGIMAG was called
c               Only needed for iofm=5
c  Input in common
c    dofcc   True if possible to generate fixed zero colour contours
c    iofm    Type of ofm  
c               1 => B&W
c               2 => RJS rainbow
c               3 => Tody linear pseudo colour
c               4 => RDE floating zero colour contours
c               5 => RDE fixed zero colour contours
c               6 => RGB
c               7 => Background
c               8 => Heat
c	        9 => Absolute b&w
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      real imin, imax
cc
      real x, y
      character ch*1
c-----------------------------------------------------------------------
c
c Tell user what's available
c
      call output (' ')
      call output ('**********  Begin lookup table '
     +           //'selection **********')
      call output (' ')
      if (nocurs) then
        call output ('To select next lookup table enter A')
        call output ('To reverse     lookup table enter D')
        call output ('To replicate   lookup table enter R')
        call output ('To exit to upper layer      enter X')
      else
        call output ('To select next lookup table click '
     +             //'left   button (enter A)')
        call output ('To reverse     lookup table click '
     +             //'middle button (enter D)')
        call output ('To replicate   lookup table       '
     +             //'               enter R')
        call output ('To exit to upper layer      click '
     +             //'right  button (enter X)')
      end if
      call output (' ')
c
      ch = ' '
      do while (ch.ne.'x')
        call ofmuin (x, y, ch)
c               
        if (ch.eq.'a') then
c
c Step to next ofm and tell user
c
          iofm = iofm + 1   
          if (iofm.eq.5 .and. .not.dofcc) iofm = iofm + 1
          if (iofm.gt.9) iofm = 1
c
          if (iofm.eq.1) then
            call output ('Loading black and white')
          else if (iofm.eq.2) then
            call output ('Loading spectrum colours')   
          else if (iofm.eq.3) then
            call output ('Loading linear colours')
          else if (iofm.eq.4) then
            call output ('Loading colour contours')
          else if (iofm.eq.5) then
            call output ('Loading fixed zero colour contours')
          else if (iofm.eq.6) then
            call output ('Loading RGB colours')
          else if (iofm.eq.7) then
            call output ('Loading background colours')
          else if (iofm.eq.8) then
            call output ('Loading heat colours')
          else if (iofm.eq.9) then
            call output ('Loading absolute black and white')
          end if
c
c Tabulate the new ACTIVE and SAVE ofms
c     
          call ofmtba (imin, imax)
c
c Restore last fiddle to ACTIVE ofm except for iofm=5
c    
          if (iofm.ne.5) call ofmrsf
c
c Apply ACTIVE ofm
c    
          call ofmapp
        else if (ch.eq.'d') then
c    
c Reverse ofm and apply it
c    
          call ofmrev
          call ofmapp
        else if (ch.eq.'r') then
c
c Replicate ofm and apply it
c
          call ofmrep 
          call ofmapp
        else
      
          call output ('Finish lookup table selection')
          call output (' ')
          call output (' ')
c
c Remind users of what they can do
c
          call ofml1m
          ch = 'x'
        end if
      end do
c
      end
c
c
      subroutine ofmsqr (imin, imax)
c-----------------------------------------------------------------------
c     Distribute the ofm levels according to a square root and set
c     the ACTIVE ofm
c
c  Input
c    imin,imax
c            Min and max intensities used in the call to PGIMAG
c  Input in common
c    na      Number of levels in ACTIVE table
c    ofms    SAVE ofm
c  Output in common
c    yt      Y values for transfer function plot
c    ofma    ACTIVE ofm
c    fidun   Signal fiddle has been done
c    fid     Fiddle index array
c    tflab   Current transfer function plot title
c
c-----------------------------------------------------------------------
      implicit none
      real imin, imax
cc
      include 'ofm.h'
      real ds, ls, imin2, imax2, ioff, lsmin, lds
      integer i, idx
c-----------------------------------------------------------------------
      call output ('Applying square root transfer function')
      tflab = 'sqrt'
c
c Add offset if necessary to make intensities all positive
c
      if (imin.le.0.0 .or. imax.le.0.0) then
        ioff = abs(min(imax,imin)) + abs(imax-imin)/100.0
        imin2 = imin + ioff
        imax2 = imax + ioff
      else
        imin2 = imin
        imax2 = imax
      end if
c
c Find linear intensity increment
c
      ds = (imax2 - imin2) / (na - 1)
c
c Find square root intensity increment and minium value
c
      lsmin = sqrt(imin2)
      lds = (sqrt(imax2) - sqrt(imin2)) / (na - 1)
c
c Loop over levels
c
      do i = 1, na
c
c Sqrt intensity in range sqrt(imin2) to sqrt(imax2)
c
        ls = sqrt((i-1)*ds + imin2)
c
c Find square root bin that this linear intensity falls into
c
	idx = nint((ls-lsmin)/lds) + 1
c
c Set ACTIVE ofm
c
        ofma(i,1) = ofms(idx,1)
        ofma(i,2) = ofms(idx,2)
        ofma(i,3) = ofms(idx,3)
c
c Set fiddle index array and transfer function plot
c
        fid(i) = idx
        yt(i) = real(idx-1)/real(na-1)
      end do
      fidun = .true.
c
      end
c
c
      subroutine ofmtabw (imin, imax)
c-----------------------------------------------------------------------
c     Generate absolute black and white SAVE and BASIC tables
c     Here absolute deviations from 0 are given a b&w ramp
c
c  Input
c    imin    Displayed min and max
c    imax
c  Input in common
c    na      Number of levels in ACTIVE ofm
c  Output in common
c    ofma    New ACTIVE ofm
c    ofms    New SAVE ofm = ACTIVE ofm at this point
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      real imin, imax
cc
      real dc, col, m
      integer i, nl, iz
c-----------------------------------------------------------------------
c
c For a linear transfer function, find the colour index (in the
c range 1 -> na) that is equivalent to zero intensity
c
      m = (imax - imin) / real(na - 1)
      iz = nint(1.0 - imin/m)
c
c Now each segment 1->iz and iz->na has a b&w ramp, the slope
c of which is such that the maximum absolute value of the
c displayed image has colour [R,G,B]=1.0 
c 
      nl = max(na-iz,iz-1)
c      write (*,*) 'iz=', iz
      dc = 1.0 / real(nl)
c        
c Set ACTIVE and SAVE table indices from the zero intensity colour 
c index to the index for intensity IMIN
c
      if (iz.lt.1 .or. iz.gt.na) call bug ('f', 'Error in ofmtabw')
      if (iz.ge.1) then
        col = 0.0
        do i = iz, 1, -1
          col = min(1.0,max(0.0,col))
          ofms(i,1) = col
          ofms(i,2) = col
          ofms(i,3) = col
          ofma(i,1) = col
          ofma(i,2) = col
          ofma(i,3) = col
c
          col = col+dc
        end do
c        write (*,*) 'ci range=', iz, 1
c        write (*,*) 'col range=', 0.0, col-dc
      end if
c
c Set ACTIVE and SAVE table indices from the zero intensity colour 
c index to the index for intensity IMAX

c
      if (iz+1.le.na) then
        col = dc
        do i = iz+1, na, 1
          col = min(1.0,max(0.0,col))
          ofms(i,1) = col
          ofms(i,2) = col
          ofms(i,3) = col
          ofma(i,1) = col
          ofma(i,2) = col
          ofma(i,3) = col
c
          col = col+dc
        end do
c        write (*,*) 'ci range=', iz+1, na
c        write (*,*) 'col range=', dc, col-dc
      end if
c
      end 
c
c
      subroutine ofmtba (imin, imax)
c-----------------------------------------------------------------------
c     Tabulate the ACTIVE and SAVE ofms for the specified ofm type
c
c  Input
c    imin,max   Image min and max that PGIMAG was called with. Only
c		used for iofm=5
c  Input in common
c    dofcc   True if possible to generate fized zero colour contours
c    iofm    Type of ofm
c               1 => B&W
c               2 => RJS rainbow 
c               3 => Tody linear pseudo colour
c               4 => RDE floating zero colour contours
c               5 => RDE fixed zero colour contours
c               6 => RGB
c               7 => Background
c               8 => Heat
c	        9 => Absolute b&w
c
c  Output in common
c    na      Number of levels in ACTIVE table
c    ci1     First available colour index
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      real imin, imax
c-----------------------------------------------------------------------
      if (iofm.lt.1 .or .iofm.gt.9) then
        iofm = 1
        call bug ('w', 'Unrecognized lookup table, setting b&w')
      end if
c
      if (iofm.eq.1 .or. iofm.eq.4 .or. iofm.eq.5 .or. iofm.eq.9) then
c
c Set ACTIVE and SAVE tables that are generated algorithmically
c
        if (iofm.eq.1) then
c
c Black and white
c
          call ofmtbw
        else if (iofm.eq.4) then
c
c Floating zero colour contours
c
          call ofmtcc (0.0, 0.0)
        else if (iofm.eq.5) then
c
c Fixed zero colour contours if possible
c
          if (dofcc) call ofmtcc (imin, imax)
        else if (iofm.eq.9) then
c
c Absolute b&w; if no zero crossing, just generate normal b&w table
c
          if (imin*imax.gt.0.0) then
            call ofmtbw
          else
            call ofmtabw (imin, imax)
          end if
        end if
      else
c
c Set BASIC 256 level ofm
c
        call ofmtbb 
c
c Spline fit it
c
        call ofmfit
c
c Evaluate the new ACTIVE table from the spline coefficients
c
        call ofmevl
      end if
c
      end
c
c
      subroutine ofmtbb 
c-----------------------------------------------------------------------
c     Set the desired type of BASIC 256 level colour ofm ready for
c     spline fitting.
c
c  Input in common
c    iofm    Type of ofm
c               1 => B&W (not done here)
c               2 => RJS rainbow 
c               3 => Tody linear pseudo colour
c               4 => RDE floating zero colour contours (not done here)
c               5 => RDE fixed zero colour contours (not done here)
c               6 => RGB
c               7 => Background
c               8 => Heat
c	        9 => Absolute b&w (not done here)
c
c  Output in common
c    ofmb    BASIC ofm for Red [ofm(i,1)], Green [ofm(i,2)]
c            and Blue [ofm(i,3)]  The 256 values range between 0 and 1
c
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      real ofm(maxlev,18)
      integer itab, i, j, k
      save ofm
c
c    ofm	Colour table data.
c
c		  ofm(*,1) - red for rjs colour ofm.
c		  ofm(*,2) - green for rjs colour ofm.
c		  ofm(*,3) - blue for rjs colour ofm.
c
c		  ofm(*,4) - red for Tody's table.
c		  ofm(*,5) - green for Tody's table.
c		  ofm(*,6) - blue for Tody's table.
c
c                 ofm(*,7) - red for rgb
c                 ofm(*,8) - green for rgb
c                 ofm(*,9) - blue for  rgb
c
c                 ofm(*,10) - red for background
c                 ofm(*,11) - green for background
c                 ofm(*,12) - blue for background
c
c                 ofm(*,13) - red for heat
c                 ofm(*,14) - green for heat
c                 ofm(*,15) - blue for heat
c
c
c
c Bob Sault rainbow colours 
c RED table
c
      data (ofm(i,1),i=1,129) /129*0/
      data (ofm(i,1),i=130,189) /
     + 0.211765, 0.250980, 0.278431, 0.298039, 0.317647, 0.337255, 
     + 0.349020, 0.364706, 0.376471, 0.388235, 0.396078, 0.407843, 
     + 0.419608, 0.427451, 0.435294, 0.447059, 0.454902, 0.462745, 
     + 0.466667, 0.474510, 0.482353, 0.490196, 0.498039, 0.505882, 
     + 0.513726, 0.517647, 0.525490, 0.533333, 0.541176, 0.549020,
     + 0.552941, 0.560784, 0.564706, 0.568627, 0.576471, 0.584314,
     + 0.588235, 0.592157, 0.600000, 0.607843, 0.611765, 0.615686,
     + 0.619608, 0.627451, 0.635294, 0.639216, 0.643137, 0.650980,
     + 0.654902, 0.658824, 0.666667, 0.670588, 0.674510, 0.682353,
     + 0.686275, 0.690196, 0.698039, 0.701961, 0.701961, 0.709804/
      data (ofm(i,1),i=190,256) /
     + 0.713726, 0.717647, 0.725490, 0.729412, 0.733333, 0.737255,
     + 0.741176, 0.749020, 0.752941, 0.756863, 0.760784, 0.764706,
     + 0.772549, 0.776471, 0.776471, 0.784314, 0.788235, 0.796078,
     + 0.796078, 0.803922, 0.807843, 0.811765, 0.815686, 0.819608,
     + 0.827451, 0.827451, 0.835294, 0.839216, 0.839216, 0.847059,
     + 0.850980, 0.854902, 0.858824, 0.862745, 0.866667, 0.874510,
     + 0.874510, 0.878431, 0.886275, 0.886275, 0.894118, 0.894118,
     + 0.901961, 0.905882, 0.909804, 0.913725, 0.917647, 0.921569,
     + 0.925490, 0.929412, 0.937255, 0.937255, 0.945098, 0.945098,
     + 0.952941, 0.952941, 0.960784, 0.960784, 0.968627, 0.968627,
     + 0.976471, 0.976471, 0.984314, 0.984314, 0.992157, 0.992157, 
     + 1.00000/
c
c Bob Sault rainbow colours 
c GREEN table
c
      data (ofm(i,2),i=1,100) /
     + 0.00000E+00, 1.56863E-02, 2.74510E-02, 3.92157E-02, 5.09804E-02, 
     + 5.88235E-02, 7.05882E-02, 7.84314E-02, 8.62745E-02, 9.41176E-02, 
     + 1.01961E-01, 0.109804, 0.117647, 0.125490, 0.133333, 0.141176, 
     + 0.149020, 0.152941, 0.160784, 0.168627, 0.172549, 0.180392, 
     + 0.188235, 0.192157, 0.200000, 0.207843, 0.211765, 0.219608, 
     + 0.223529, 0.231373, 0.239216, 0.243137, 0.250980, 0.254902, 
     + 0.258824, 0.266667, 0.270588, 0.278431, 0.282353, 0.290196,
     + 0.294118, 0.298039, 0.305882, 0.309804, 0.317647, 0.321569,
     + 0.329412, 0.333333, 0.337255, 0.345098, 0.349020, 0.352941,
     + 0.360784, 0.364706, 0.368627, 0.376471, 0.380392, 0.384314,
     + 0.392157, 0.396078, 0.400000, 0.403922, 0.411765, 0.415686,
     + 0.419608, 0.427451, 0.427451, 0.435294, 0.439216, 0.443137,
     + 0.450980, 0.454902, 0.458824, 0.462745, 0.470588, 0.470588,
     + 0.478431, 0.482353, 0.486275, 0.490196, 0.498039, 0.501961,
     + 0.505882, 0.509804, 0.517647, 0.521569, 0.525490, 0.529412,
     + 0.533333, 0.537255, 0.545098, 0.549020, 0.552941, 0.556863,
     + 0.560784, 0.564706, 0.572549, 0.572549, 0.580392, 0.584314/
      data (ofm(i,2),i=101,160) /
     + 0.588235, 0.592157, 0.596078, 0.600000, 0.607843, 0.607843,
     + 0.615686, 0.619608, 0.623529, 0.627451, 0.631373, 0.635294,
     + 0.639216, 0.647059, 0.647059, 0.654902, 0.654902, 0.662745,
     + 0.666667, 0.670588, 0.674510, 0.678431, 0.682353, 0.686275,
     + 0.690196, 0.694118, 0.698039, 0.701961, 0.705882, 0.705882,
     + 0.709804, 0.709804, 0.713726, 0.709804, 0.713726, 0.713726,
     + 0.717647, 0.717647, 0.717647, 0.717647, 0.721569, 0.721569, 
     + 0.725490, 0.721569, 0.725490, 0.725490, 0.725490, 0.729412, 
     + 0.725490, 0.729412, 0.729412, 0.729412, 0.729412, 0.729412, 
     + 0.733333, 0.733333, 0.733333, 0.733333, 0.733333, 0.733333/
      data (ofm(i,2),i=161,256) /
     + 0.733333, 0.733333, 0.737255, 0.733333, 0.737255, 0.737255,
     + 0.733333, 0.737255, 0.737255, 0.737255, 0.737255, 0.737255,
     + 0.737255, 0.737255, 0.737255, 0.737255, 0.733333, 0.737255,
     + 0.733333, 0.733333, 0.733333, 0.733333, 0.733333, 0.733333,
     + 0.729412, 0.733333, 0.733333, 0.729412, 0.729412, 0.725490,
     + 0.729412, 0.729412, 0.725490, 0.725490, 0.721569, 0.721569,
     + 0.721569, 0.721569, 0.721569, 0.717647, 0.717647, 0.713726,
     + 0.713726, 0.713726, 0.709804, 0.709804, 0.705882, 0.705882,
     + 0.701961, 0.701961, 0.698039, 0.698039, 0.690196, 0.690196,
     + 0.686275, 0.686275, 0.682353, 0.678431, 0.674510, 0.674510,
     + 0.670588, 0.666667, 0.662745, 0.662745, 0.654902, 0.654902,
     + 0.647059, 0.647059, 0.639216, 0.635294, 0.631373, 0.623529,
     + 0.619608, 0.611765, 0.607843, 0.600000, 0.592157, 0.588235,
     + 0.580392, 0.576471, 0.564706, 0.556863, 0.549020, 0.537255,
     + 0.525490, 0.517647, 0.501961, 0.490196, 0.474510, 0.458824,
     + 0.435294, 0.415686, 0.388235, 0.349020, 0.294118, 0.000000/
c
c Bob Sault rainbow colours 
c BLUE table
c
      data (ofm(i,3),i=1,60) /
     + 0.0, 5.88235E-02, 8.62745E-02, 0.105882, 0.121569, 0.137255, 
     + 0.149020, 0.160784, 0.172549, 0.180392, 0.192157, 0.200000, 
     + 0.211765, 0.215686, 0.227451, 0.231373, 0.239216, 0.247059, 
     + 0.254902, 0.258824, 0.266667, 0.274510, 0.278431, 0.282353, 
     + 0.290196, 0.294118, 0.298039, 0.305882, 0.309804, 0.313726, 
     + 0.317647, 0.321569, 0.325490, 0.329412, 0.337255, 0.341176, 
     + 0.345098, 0.349020, 0.349020, 0.356863, 0.356863, 0.360784, 
     + 0.364706, 0.368627, 0.372549, 0.376471, 0.376471, 0.380392, 
     + 0.384314, 0.388235, 0.388235, 0.392157, 0.392157, 0.396078, 
     + 0.400000, 0.400000, 0.403922, 0.407843, 0.407843, 0.407843/
      data (ofm(i,3),i=61,128) /
     + 0.411765, 0.411765, 0.415686, 0.415686, 0.419608, 0.419608, 
     + 0.419608, 0.423529, 0.423529, 0.427451, 0.427451, 0.427451, 
     + 0.427451, 0.431373, 0.431373, 0.431373, 0.431373, 0.435294, 
     + 0.435294, 0.435294, 0.435294, 0.435294, 0.435294, 0.435294, 
     + 0.435294, 0.435294, 0.435294, 0.435294, 0.435294, 0.435294, 
     + 0.435294, 0.435294, 0.435294, 0.431373, 0.431373, 0.431373, 
     + 0.427451, 0.427451, 0.427451, 0.427451, 0.423529, 0.419608, 
     + 0.419608, 0.415686, 0.415686, 0.411765, 0.407843, 0.403922, 
     + 0.400000, 0.396078, 0.396078, 0.392157, 0.384314, 0.380392, 
     + 0.376471, 0.368627, 0.364706, 0.356863, 0.349020, 0.341176, 
     + 0.329412, 0.317647, 0.305882, 0.290196, 0.270588, 0.247059, 
     + 0.207843, 0.000000/
      data (ofm(i,3),i=129,256) /128*0.0/
c
c Doug Tody's "linear pseudo-colour"
c RED table.
c
      data (ofm(i,4),i=1,127)/127*0.0/
      data (ofm(i,4),i=128,168)/	      
     + 0.023529, 0.047058, 0.070588, 0.094118, 0.117647, 0.141176, 
     + 0.164706, 0.188235, 0.211765, 0.235294, 0.258824, 0.282353, 
     + 0.305882, 0.333333, 0.356863, 0.380392, 0.403922, 0.427451, 
     + 0.450980, 0.474510, 0.498039, 0.521569, 0.545098, 0.568627, 
     + 0.592157, 0.615686, 0.639216, 0.666667, 0.690196, 0.713726, 
     + 0.737255, 0.760784, 0.784314, 0.807843, 0.831373, 0.854902, 
     + 0.878431, 0.901961, 0.925490, 0.949020, 0.972549/
      data (ofm(i,4),i=169,256)/88*1.0/
c
c Doug Tody's "linear pseudo-colour"
c GREEN table.
c
      data (ofm(i,5),i=  1,40)/40*0.0/
      data (ofm(i,5),i=41,88)/
     + 0.000000, 0.000000, 0.000000, 0.023529, 0.047059, 0.070588, 
     + 0.094118, 0.117647, 0.141176, 0.164706, 0.188235, 0.211765, 
     + 0.235294, 0.258824, 0.282353, 0.305882, 0.333333, 0.356863, 
     + 0.380392, 0.403922, 0.427451, 0.450980, 0.474510, 0.498039, 
     + 0.521569, 0.545098, 0.568627, 0.592157, 0.615686, 0.639216, 
     + 0.666667, 0.690196, 0.713726, 0.737255, 0.760784, 0.784314, 
     + 0.807843, 0.831373, 0.854902, 0.878431, 0.901961, 0.925490, 
     + 0.949020, 0.972549, 1.000000, 1.000000, 1.000000, 1.000000/
      data (ofm(i,5),i=89,168)/80*1.0/
      data (ofm(i,5),i=169,256)/
     + 1.000000, 0.972549, 0.949020, 0.925490, 0.901961, 0.878431, 
     + 0.854902, 0.831373, 0.807843, 0.784314, 0.760784, 0.737255, 
     + 0.713726, 0.690196, 0.666667, 0.639216, 0.615686, 0.592157, 
     + 0.568627, 0.545098, 0.521569, 0.498039, 0.474510, 0.450980, 
     + 0.427451, 0.403922, 0.380392, 0.356863, 0.333333, 0.305882, 
     + 0.282353, 0.258824, 0.235294, 0.211765, 0.188235, 0.164706, 
     + 0.141176, 0.117647, 0.094118, 0.070588, 0.047059, 0.023529, 
     + 0.000000, 0.023529, 0.047059, 0.070588, 0.094118, 0.117647, 
     + 0.141176, 0.164706, 0.188235, 0.211765, 0.235294, 0.258824, 
     + 0.282353, 0.305882, 0.333333, 0.356863, 0.380392, 0.403922, 
     + 0.427451, 0.450980, 0.474510, 0.498039, 0.521569, 0.545098,
     + 0.568627, 0.592157, 0.615686, 0.639216, 0.666667, 0.690196, 
     + 0.713726, 0.737255, 0.760784, 0.784314, 0.807843, 0.831373, 
     + 0.854902, 0.878431, 0.901961, 0.925490, 0.949020, 0.972549, 
     + 1.00000, 1.00000, 1.00000, 1.00000/
c
c Doug Tody's "linear pseudo-colour"
c BLUE table.
c
      data (ofm(i,6),i=  1,48)/
     + 0.000000, 0.023529, 0.047059, 0.070588, 0.094118, 0.117647, 
     + 0.141176, 0.164706, 0.188235, 0.211765, 0.235294, 0.258824, 
     + 0.282353, 0.305882, 0.333333, 0.356863, 0.380392, 0.403922, 
     + 0.427451, 0.450980, 0.474510, 0.498039, 0.521569, 0.545098, 
     + 0.568627, 0.592157, 0.615686, 0.639216, 0.666667, 0.690196, 
     + 0.713726, 0.737255, 0.760784, 0.784314, 0.807843, 0.831373, 
     + 0.854902, 0.878431, 0.901961, 0.925490, 0.949020, 0.972549, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000/
      data (ofm(i,6),i=49,88)/40*1.0/
      data (ofm(i,6),i=89,128)/
     + 0.901961, 0.878431, 0.854902, 0.831373, 0.807843, 0.784314, 
     + 0.760784, 0.737255, 0.713726, 0.690196, 0.666667, 0.639216, 
     + 0.615686, 0.592157, 0.568627, 0.545098, 0.521569, 0.498039, 
     + 0.474510, 0.450980, 0.427451, 0.403922, 0.380392, 0.356863, 
     + 0.333333, 0.305882, 0.282353, 0.258824, 0.235294, 0.211765, 
     + 0.188235, 0.164706, 0.141176, 0.117647, 0.094118, 0.0705988, 
     + 0.047059, 0.023529, 0.000000, 0.000000/
      data (ofm(i,6),i=129,208)/80*0.0/
      data (ofm(i,6),i=209,256)/
     + 0.000000, 0.000000, 0.000000, 0.023529, 0.047059, 0.070588, 
     + 0.094118, 0.117647, 0.141176, 0.164706, 0.188235, 0.211765, 
     + 0.235294, 0.258824, 0.282353, 0.305882, 0.333333, 0.356863, 
     + 0.380392, 0.403922, 0.427451, 0.450980, 0.474510, 0.498039, 
     + 0.521569, 0.545098, 0.568627, 0.592157, 0.615686, 0.639216, 
     + 0.666667, 0.690196, 0.713726, 0.737255, 0.760784, 0.784314, 
     + 0.807843, 0.831373, 0.854902, 0.878431, 0.901961, 0.925490,
     + 0.949020, 0.972549, 1.000000, 1.000000, 1.000000, 1.000000/
c
c GIPSY RGB colour
c RED table
c
      data (ofm(i,7),i=1,90) /
     + 0.000000, 0.011760, 0.027450, 0.043140, 0.058820, 0.074510, 
     + 0.086270, 0.101960, 0.117650, 0.133330, 0.149020, 0.160780, 
     + 0.176470, 0.192160, 0.207840, 0.223530, 0.235290, 0.250980, 
     + 0.266670, 0.282350, 0.298040, 0.309800, 0.325490, 0.341180,
     + 0.356860, 0.372550, 0.384310, 0.400000, 0.415690, 0.431370,
     + 0.447060, 0.458820, 0.474510, 0.490200, 0.505880, 0.521570,
     + 0.537250, 0.549020, 0.564710, 0.580390, 0.596080, 0.611760,
     + 0.623530, 0.639220, 0.654900, 0.670590, 0.686270, 0.698040,
     + 0.713730, 0.729410, 0.745100, 0.760780, 0.772550, 0.788240,
     + 0.803920, 0.819610, 0.835290, 0.847060, 0.862750, 0.878430,
     + 0.894120, 0.909800, 0.921570, 0.937250, 0.952940, 0.968630,
     + 0.984310, 1.000000, 0.984310, 0.968630, 0.952940, 0.937250, 
     + 0.921570, 0.901960, 0.886270, 0.870590, 0.854900, 0.839220, 
     + 0.823530, 0.803920, 0.788240, 0.772550, 0.756860, 0.741180, 
     + 0.721570, 0.705880, 0.690200, 0.674510, 0.658820, 0.643140/
      data (ofm(i,7),i=91,128) /
     + 0.623530, 0.607840, 0.592160, 0.576470, 0.560780, 0.541180, 
     + 0.525490, 0.509800, 0.494120, 0.478430, 0.462750, 0.443140, 
     + 0.427450, 0.411760, 0.396080, 0.380390, 0.360780, 0.345100, 
     + 0.329410, 0.313730, 0.298040, 0.282350, 0.262750, 0.247060, 
     + 0.231370, 0.215690, 0.200000, 0.180390, 0.164710, 0.149020, 
     + 0.133330, 0.117650, 0.101960, 0.082350, 0.066670, 0.050980,
     + 0.035290, 0.019610/
      data (ofm(i,7),i=129,256) /128*0.0/
c
c GIPSY RGB colour
c GREEN table
c
      data (ofm(i,8),i=1,65) /65*0.0/
      data (ofm(i,8),i=66,155) /
     + 0.011760, 0.027450, 0.043140, 0.058820, 0.074510, 0.090200, 
     + 0.105880, 0.121570, 0.137250, 0.152940, 0.168630, 0.184310, 
     + 0.200000, 0.215690, 0.231370, 0.247060, 0.262750, 0.278430, 
     + 0.294120, 0.309800, 0.325490, 0.341180, 0.356860, 0.372550, 
     + 0.388240, 0.403920, 0.419610, 0.435290, 0.450980, 0.466670, 
     + 0.482350, 0.498040, 0.513730, 0.529410, 0.545100, 0.560780, 
     + 0.576470, 0.592160, 0.607840, 0.623530, 0.639220, 0.654900, 
     + 0.670590, 0.686270, 0.701960, 0.717650, 0.733330, 0.749020, 
     + 0.764710, 0.780390, 0.796080, 0.811760, 0.827450, 0.843140, 
     + 0.858820, 0.874510, 0.890200, 0.905880, 0.921570, 0.937250, 
     + 0.952940, 0.968630, 0.984310, 1.000000, 0.984310, 0.968630,
     + 0.952940, 0.937250, 0.921570, 0.905880, 0.890200, 0.874510,
     + 0.858820, 0.843140, 0.827450, 0.811760, 0.796080, 0.780390,
     + 0.764710, 0.749020, 0.733330, 0.717650, 0.701960, 0.686270,
     + 0.666670, 0.650980, 0.635290, 0.619610, 0.603920, 0.588240/
      data (ofm(i,8),i=156,191) /
     + 0.572550, 0.556860, 0.541180, 0.525490, 0.509800, 0.494120,
     + 0.478430, 0.462750, 0.447060, 0.431370, 0.415690, 0.400000,
     + 0.384310, 0.368630, 0.352940, 0.333330, 0.317650, 0.301960,
     + 0.286270, 0.270590, 0.254900, 0.239220, 0.223530, 0.207840,
     + 0.192160, 0.176470, 0.160780, 0.145100, 0.129410, 0.113730,
     + 0.098040, 0.082350, 0.066670, 0.050980, 0.035290, 0.019610/
      data (ofm(i,8),i=192,256) /65*0.0/
c
c GIPSY RGB colour
c BLUE table
c
      data (ofm(i,9),i=1,127) /127*0.0/
      data (ofm(i,9),i=128,217) /
     + 0.011760, 0.027450, 0.043140, 0.058820, 0.074510, 0.090200, 
     + 0.105880, 0.117650, 0.133330, 0.149020, 0.164710, 0.180390, 
     + 0.196080, 0.211760, 0.223530, 0.239220, 0.254900, 0.270590, 
     + 0.286270, 0.301960, 0.317650, 0.333330, 0.345100, 0.360780, 
     + 0.376470, 0.392160, 0.407840, 0.423530, 0.439220, 0.450980, 
     + 0.466670, 0.482350, 0.498040, 0.513730, 0.529410, 0.545100,  
     + 0.556860, 0.572550, 0.588240, 0.603920, 0.619610, 0.635290, 
     + 0.650980, 0.666670, 0.678430, 0.694120, 0.709800, 0.725490, 
     + 0.741180, 0.756860, 0.772550, 0.784310, 0.800000, 0.815690, 
     + 0.831370, 0.847060, 0.862750, 0.878430, 0.890200, 0.905880, 
     + 0.921570, 0.937250, 0.952940, 0.968630, 0.984310, 1.000000, 
     + 0.984310, 0.968630, 0.952940, 0.937250, 0.921570, 0.905880, 
     + 0.890200, 0.874510, 0.858820, 0.843140, 0.827450, 0.811760, 
     + 0.796080, 0.780390, 0.764710, 0.749020, 0.733330, 0.717650, 
     + 0.701960, 0.686270, 0.666670, 0.650980, 0.635290, 0.619610/
      data (ofm(i,9),i=218,256) /
     + 0.603920, 0.588240, 0.572550, 0.556860, 0.541180, 0.525490, 
     + 0.509800, 0.494120, 0.478430, 0.462750, 0.447060, 0.431370, 
     + 0.415690, 0.400000, 0.384310, 0.368630, 0.352940, 0.333330, 
     + 0.317650, 0.301960, 0.286270, 0.270590, 0.254900, 0.239220, 
     + 0.223530, 0.207840, 0.192160, 0.176470, 0.160780, 0.145100, 
     + 0.129410, 0.113730, 0.098040, 0.082350, 0.066670, 0.050980, 
     + 0.035290, 0.019610, 0.000000/
c
c GIPSY background colour 
c RED table
c      
      data (ofm(i,10),i=1,90) /
     + 0.000000, 0.015870, 0.031740, 0.047610, 0.063480, 0.079350,
     + 0.095220, 0.111090, 0.126960, 0.142830, 0.158700, 0.174570,
     + 0.190440, 0.206310, 0.222180, 0.238050, 0.253920, 0.269790,
     + 0.285660, 0.301530, 0.317400, 0.333270, 0.349140, 0.365010,
     + 0.380880, 0.396750, 0.412620, 0.428490, 0.444360, 0.460230,
     + 0.476100, 0.491970, 0.507840, 0.523710, 0.539580, 0.555450,
     + 0.571320, 0.587190, 0.603060, 0.618930, 0.634800, 0.650670,
     + 0.666540, 0.682410, 0.698280, 0.714150, 0.730020, 0.745890,
     + 0.761760, 0.777630, 0.793500, 0.809370, 0.825240, 0.841110,
     + 0.856980, 0.872850, 0.888720, 0.904590, 0.920460, 0.936330,
     + 0.952200, 0.968070, 0.983940, 0.999810, 0.000000, 0.000000, 
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000/
      data (ofm(i,10),i=91,180) /
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
     + 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000,
     + 0.000000, 0.000000, 0.000000, 0.015870, 0.031740, 0.047610,
     + 0.063480, 0.079350, 0.095220, 0.111090, 0.126960, 0.142830,
     + 0.158700, 0.174570, 0.190440, 0.206310, 0.222180, 0.238050,
     + 0.253920, 0.269790, 0.285660, 0.301530, 0.317400, 0.333270,
     + 0.349140, 0.365010, 0.380880, 0.396750, 0.412620, 0.428490,
     + 0.444360, 0.460230, 0.476100, 0.491970, 0.507840, 0.523710,
     + 0.539580, 0.555450, 0.571320, 0.587190, 0.603060, 0.618930,
     + 0.634800, 0.650670, 0.666540, 0.682410, 0.698280, 0.714150,
     + 0.730020, 0.745890, 0.761760, 0.777630, 0.793500, 0.809370/
      data (ofm(i,10),i=181,256) /
     + 0.825240, 0.841110, 0.856980, 0.872850, 0.888720, 0.904590,
     + 0.920460, 0.936330, 0.952200, 0.968070, 0.983940, 0.999810,
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000,
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000,
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000,
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000,
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000,
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000/
c
c GIPSY background colour 
c GREEN table
c      
      data (ofm(i,11),i=1,90) /
     + 0.000000, 0.015870, 0.031740, 0.047610, 0.063480, 0.079350, 
     + 0.095220, 0.111090, 0.126960, 0.142830, 0.158700, 0.174570, 
     + 0.190440, 0.206310, 0.222180, 0.238050, 0.253920, 0.269790, 
     + 0.285660, 0.301530, 0.317400, 0.333270, 0.349140, 0.365010, 
     + 0.380880, 0.396750, 0.412620, 0.428490, 0.444360, 0.460230, 
     + 0.476100, 0.491970, 0.507840, 0.523710, 0.539580, 0.555450, 
     + 0.571320, 0.587190, 0.603060, 0.618930, 0.634800, 0.650670, 
     + 0.666540, 0.682410, 0.698280, 0.714150, 0.730020, 0.745890, 
     + 0.761760, 0.777630, 0.793500, 0.809370, 0.825240, 0.841110, 
     + 0.856980, 0.872850, 0.888720, 0.904590, 0.920460, 0.936330, 
     + 0.952200, 0.968070, 0.983940, 0.999810, 0.000000, 0.015870,
     + 0.031740, 0.047610, 0.063480, 0.079350, 0.095220, 0.111090, 
     + 0.126960, 0.142830, 0.158700, 0.174570, 0.190440, 0.206310, 
     + 0.222180, 0.238050, 0.253920, 0.269790, 0.285660, 0.301530, 
     + 0.317400, 0.333270, 0.349140, 0.365010, 0.380880, 0.396750/
      data (ofm(i,11),i=91,180) /
     + 0.412620, 0.428490, 0.444360, 0.460230, 0.476100, 0.491970, 
     + 0.507840, 0.523710, 0.539580, 0.555450, 0.571320, 0.587190, 
     + 0.603060, 0.618930, 0.634800, 0.650670, 0.666540, 0.682410, 
     + 0.698280, 0.714150, 0.730020, 0.745890, 0.761760, 0.777630, 
     + 0.793500, 0.809370, 0.825240, 0.841110, 0.856980, 0.872850, 
     + 0.888720, 0.904590, 0.920460, 0.936330, 0.952200, 0.968070, 
     + 0.983940, 0.999810, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000,
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000/
       data (ofm(i,11),i=181,256) /
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 
     + 0.999810, 0.983940, 0.968070, 0.952200, 0.936330, 0.920460, 
     + 0.904590, 0.888720, 0.872850, 0.856980, 0.841110, 0.825240, 
     + 0.809370, 0.793500, 0.777630, 0.761760, 0.745890, 0.730020, 
     + 0.714150, 0.698280, 0.682410, 0.666540, 0.650670, 0.634800, 
     + 0.618930, 0.603060, 0.587190, 0.571320, 0.555450, 0.539580, 
     + 0.523710, 0.507840, 0.491970, 0.476100, 0.460230, 0.444360, 
     + 0.428490, 0.412620, 0.396750, 0.380880, 0.365010, 0.349140, 
     + 0.333270, 0.317400, 0.301530, 0.285660, 0.269790, 0.253920, 
     + 0.238050, 0.222180, 0.206310, 0.190440, 0.174570, 0.158700, 
     + 0.142830, 0.126960, 0.111090, 0.095220, 0.079350, 0.063480,
     + 0.047610, 0.031740, 0.015870, 0.000000/
c
c GIPSY background colour 
c BLUE table
c      
      data (ofm(i,12),i=1,90) /
     + 0.000000, 0.015870, 0.031740, 0.047610, 0.063480, 0.079350,
     + 0.095220, 0.111090, 0.126960, 0.142830, 0.158700, 0.174570,
     + 0.190440, 0.206310, 0.222180, 0.238050, 0.253920, 0.269790,
     + 0.285660, 0.301530, 0.317400, 0.333270, 0.349140, 0.365010,
     + 0.380880, 0.396750, 0.412620, 0.428490, 0.444360, 0.460230,
     + 0.476100, 0.491970, 0.507840, 0.523710, 0.539580, 0.555450,
     + 0.571320, 0.587190, 0.603060, 0.618930, 0.634800, 0.650670,
     + 0.666540, 0.682410, 0.698280, 0.714150, 0.730020, 0.745890,
     + 0.761760, 0.777630, 0.793500, 0.809370, 0.825240, 0.841110,
     + 0.856980, 0.872850, 0.888720, 0.904590, 0.920460, 0.936330,
     + 0.952200, 0.968070, 0.983940, 0.999810, 0.999810, 0.983940,
     + 0.968070, 0.952200, 0.936330, 0.920460, 0.904590, 0.888720,
     + 0.872850, 0.856980, 0.841110, 0.825240, 0.809370, 0.793500,
     + 0.777630, 0.761760, 0.745890, 0.730020, 0.714150, 0.698280,
     + 0.682410, 0.666540, 0.650670, 0.634800, 0.618930, 0.603060/
      data (ofm(i,12),i=91,127) /
     + 0.587190, 0.571320, 0.555450, 0.539580, 0.523710, 0.507840, 
     + 0.491970, 0.476100, 0.460230, 0.444360, 0.428490, 0.412620, 
     + 0.396750, 0.380880, 0.365010, 0.349140, 0.333270, 0.317400, 
     + 0.301530, 0.285660, 0.269790, 0.253920, 0.238050, 0.222180, 
     + 0.206310, 0.190440, 0.174570, 0.158700, 0.142830, 0.126960, 
     + 0.111090, 0.095220, 0.079350, 0.063480, 0.047610, 0.031740, 
     + 0.015870/
      data (ofm(i,12),i=128,256) /129*0.0/
c
c GIPSY heat colour 
c RED table
c
      data (ofm(i,13),i=1,85) /
     + 0.000000, 0.011760, 0.023530, 0.035290, 0.047060, 0.058820, 
     + 0.070590, 0.082350, 0.094120, 0.105880, 0.117650, 0.129410, 
     + 0.141180, 0.152940, 0.164710, 0.176470, 0.188240, 0.200000, 
     + 0.211760, 0.223530, 0.235290, 0.247060, 0.258820, 0.270590, 
     + 0.282350, 0.294120, 0.305880, 0.317650, 0.329410, 0.341180,
     + 0.352940, 0.364710, 0.376470, 0.388240, 0.400000, 0.411760,
     + 0.423530, 0.435290, 0.447060, 0.458820, 0.470590, 0.482350,
     + 0.494120, 0.505880, 0.517650, 0.529410, 0.541180, 0.552940,
     + 0.564710, 0.576470, 0.588240, 0.600000, 0.611760, 0.623530,
     + 0.635290, 0.647060, 0.658820, 0.670590, 0.682350, 0.694120,
     + 0.705880, 0.717650, 0.729410, 0.741180, 0.752940, 0.764710,
     + 0.776470, 0.788240, 0.800000, 0.811760, 0.823530, 0.835290,
     + 0.847060, 0.858820, 0.870590, 0.882350, 0.894120, 0.905880,
     + 0.917650, 0.929410, 0.941180, 0.952940, 0.964710, 0.976470,
     + 0.988240/
      data (ofm(i,13),i=86,256) /171*1.0/
c
c GIPSY heat colour
c GREEN table
c
      data (ofm(i,14),i=1,90) /
     + 0.000000, 0.003920, 0.007840, 0.011760, 0.015690, 0.019610, 
     + 0.023530, 0.027450, 0.031370, 0.035290, 0.039220, 0.043140, 
     + 0.047060, 0.050980, 0.054900, 0.058820, 0.062750, 0.066670, 
     + 0.070590, 0.074510, 0.078430, 0.082350, 0.086270, 0.090200, 
     + 0.094120, 0.098040, 0.101960, 0.105880, 0.109800, 0.113730, 
     + 0.117650, 0.121570, 0.125490, 0.129410, 0.133330, 0.137250, 
     + 0.141180, 0.145100, 0.149020, 0.152940, 0.156860, 0.160780, 
     + 0.164710, 0.168630, 0.172550, 0.176470, 0.180390, 0.184310, 
     + 0.188240, 0.192160, 0.196080, 0.200000, 0.203920, 0.207840, 
     + 0.211760, 0.215690, 0.219610, 0.223530, 0.227450, 0.231370, 
     + 0.235290, 0.239220, 0.243140, 0.247060, 0.250980, 0.254900, 
     + 0.258820, 0.262750, 0.266670, 0.270590, 0.274510, 0.278430, 
     + 0.282350, 0.286270, 0.290200, 0.294120, 0.298040, 0.301960, 
     + 0.305880, 0.309800, 0.313730, 0.317650, 0.321570, 0.325490, 
     + 0.329410, 0.333330, 0.337250, 0.341180, 0.345100, 0.349020/
      data (ofm(i,14),i=91,180) /
     + 0.352940, 0.356860, 0.360780, 0.364710, 0.368630, 0.372550,
     + 0.376470, 0.380390, 0.384310, 0.388240, 0.392160, 0.396080,
     + 0.400000, 0.403920, 0.407840, 0.411760, 0.415690, 0.419610, 
     + 0.423530, 0.427450, 0.431370, 0.435290, 0.439220, 0.443140, 
     + 0.447060, 0.450980, 0.454900, 0.458820, 0.462750, 0.466670, 
     + 0.470590, 0.474510, 0.478430, 0.482350, 0.486270, 0.490200, 
     + 0.494120, 0.498040, 0.501960, 0.505880, 0.509800, 0.513730, 
     + 0.517650, 0.521570, 0.525490, 0.529410, 0.533330, 0.537250, 
     + 0.541180, 0.545100, 0.549020, 0.552940, 0.556860, 0.560780, 
     + 0.564710, 0.568630, 0.572550, 0.576470, 0.580390, 0.584310, 
     + 0.588240, 0.592160, 0.596080, 0.600000, 0.603920, 0.607840, 
     + 0.611760, 0.615690, 0.619610, 0.623530, 0.627450, 0.631370, 
     + 0.635290, 0.639220, 0.643140, 0.647060, 0.650980, 0.654900, 
     + 0.658820, 0.662750, 0.666670, 0.670590, 0.674510, 0.678430, 
     + 0.682350, 0.686270, 0.690200, 0.694120, 0.698040, 0.701960/
      data (ofm(i,14),i=181,256) /
     + 0.705880, 0.709800, 0.713730, 0.717650, 0.721570, 0.725490, 
     + 0.729410, 0.733330, 0.737250, 0.741180, 0.745100, 0.749020, 
     + 0.752940, 0.756860, 0.760780, 0.764710, 0.768630, 0.772550, 
     + 0.776470, 0.780390, 0.784310, 0.788240, 0.792160, 0.796080, 
     + 0.800000, 0.803920, 0.807840, 0.811760, 0.815690, 0.819610, 
     + 0.823530, 0.827450, 0.831370, 0.835290, 0.839220, 0.843140, 
     + 0.847060, 0.850980, 0.854900, 0.858820, 0.862750, 0.866670, 
     + 0.870590, 0.874510, 0.878430, 0.882350, 0.886270, 0.890200, 
     + 0.894120, 0.898040, 0.901960, 0.905880, 0.909800, 0.913730, 
     + 0.917650, 0.921570, 0.925490, 0.929410, 0.933330, 0.937250, 
     + 0.941180, 0.945100, 0.949020, 0.952940, 0.956860, 0.960780, 
     + 0.964710, 0.968630, 0.972550, 0.976470, 0.980390, 0.984310,
     + 0.988240, 0.992160, 0.996080, 1.00000/
c
c GIPSY heat colour
c BLUE table
c
      data (ofm(i,15),i=1,166) /166*0.0/
      data (ofm(i,15),i=167,256) /
     + 0.011760, 0.023530, 0.035290, 0.047060, 0.058820, 0.070590,
     + 0.082350, 0.094120, 0.105880, 0.117650, 0.129410, 0.141180,
     + 0.152940, 0.164710, 0.176470, 0.188240, 0.200000, 0.211760,
     + 0.223530, 0.235290, 0.247060, 0.258820, 0.270590, 0.282350,
     + 0.294120, 0.305880, 0.317650, 0.329410, 0.341180, 0.352940,
     + 0.364710, 0.376470, 0.388240, 0.400000, 0.411760, 0.423530,
     + 0.435290, 0.447060, 0.458820, 0.470590, 0.482350, 0.494120,
     + 0.505880, 0.517650, 0.529410, 0.541180, 0.552940, 0.564710,
     + 0.576470, 0.588240, 0.600000, 0.611760, 0.623530, 0.635290,
     + 0.647060, 0.658820, 0.670590, 0.682350, 0.694120, 0.705880,
     + 0.717650, 0.729410, 0.741180, 0.752940, 0.764710, 0.776470,
     + 0.788240, 0.800000, 0.811760, 0.823530, 0.835290, 0.847060,
     + 0.858820, 0.870590, 0.882350, 0.894120, 0.905880, 0.917650,
     + 0.929410, 0.941180, 0.952940, 0.964710, 0.976470, 0.988240,
     + 1.000000, 1.000000, 1.000000, 1.000000, 1.000000, 1.000000/
c-----------------------------------------------------------------------
c
c Select table of interest
c
      if (iofm.eq.2) then
        itab = 1
      else if (iofm.eq.3) then
        itab = 4
      else if (iofm.eq.6) then
        itab = 7
      else if (iofm.eq.7) then
        itab = 10
      else if (iofm.eq.8) then
        itab = 13
      else        
        call bug ('f', 'Unrecognized lookup table type')
      end if
c
c Copy the ofms for fitting 
c
      k = 1
      do j = itab, itab+2
        do i = 1, 256
          ofmb(i,k) = ofm(i,j) 
        end do
        k = k + 1
      end do
c
      end
c
c
      subroutine ofmtbw
c-----------------------------------------------------------------------
c     Generate black and white SAVE and BASIC tables
c
c  Input in common
c    na      Number of levels in ACTIVE ofm
c  Output in common
c    ofma    New ACTIVE ofm
c    ofms    New SAVE ofm = ACTIVE ofm at this point
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
cc
      integer i
      real col
c-----------------------------------------------------------------------
      do i = 1, na
        col = real(i-1) / real(na-1)
        ofms(i,1) = col
        ofms(i,2) = col
        ofms(i,3) = col
        ofma(i,1) = col
        ofma(i,2) = col
        ofma(i,3) = col
      end do
c
      end
c
c
      subroutine ofmtcc (imin, imax)
c-----------------------------------------------------------------------
c     Generate Ron Ekers' colour contours SAVE and BASIC tables.  This
c     lookup table is designed to show quickly whether you have signal
c     or not by colour symmetry.   The boundary between blue and green
c     is kept fixed at image intensity 0.0.  For pure noise, there would
c     be equal numbers of blueish and greenish colours.  Red and white
c     indicate signal is the levels are adjusted correctly. Note that 
c     the linear fiddler refuses to allow the blue/green boundary to move.
c
c     The above is implemented if imin < 0 and imax > 0 and 
c     abs(imax).gt.abs(imin), the normal case for which it is meant.
c     Otherwise, the colour contours are uniformly distributed
c     and the linear fiddler will move the blue/green boundary.
c
c  Input
c    imin,max  Image min and max that was used in call to PGIMAG  
c              If given, fix blue/green boundary at zero.
c  Input in common
c    na        Number of levels in ACTIVE ofm
c    dofcc     True if possible to generate fixed zero colour contours
c  Output in common
c    ofma      New ACTIVE ofm
c    ofms      New SAVE ofm = ACTIVE ofm at this point
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      real imin, imax
cc
      integer i1, i2, i, dn, diff, ntot, nlevs(10)
      real f
c-----------------------------------------------------------------------
c
c Generate colours with blue/green boundary at zero if possible.
c Otherwise evenly distribute the colours.
c
      if (imin.ne.0.0 .and. imax.ne.0.0) then
c
c Work out how many levels per colour for first 8 colours
c
        ntot = 0
        f = abs(imin) / (abs(imax) + abs(imin))
        dn = int(f*real(na)/4.0)
        diff = nint(f*real(na)) - 4*dn
        do i = 1, 4
          nlevs(i) = dn 
          nlevs(i+4) = dn 
        end do
        ntot = 8 * dn
        do i = 1, diff
          nlevs(i) = nlevs(i) + 1
          nlevs(9-i) = nlevs(9-i) + 1
          ntot = ntot + 2
        end do
c
c Split the last two colours with whatever is left. 
c
        dn = (na - ntot)/2
        nlevs(9) = dn
        ntot = ntot + dn
        nlevs(10) = na - ntot
        ntot = ntot + nlevs(10)
      else
c
c Work out how many levels per colour
c
        dn = int(real(na)/10.0)
        ntot = dn*10
c
c Distribute any extra ones over all the colours
c
        do i = 1, 10
          nlevs(i) = dn
        end do
c
        diff = na - ntot
        do i = 1, diff
          nlevs(i) = nlevs(i) + 1
          ntot = ntot + 1
        end do
      end if
c
c Black
c
      i1 = 1
      i2 = i1 + nlevs(1) - 1
c      write (*,*) 'i1,i2=',i1,i2
      do i = i1, i2
        ofms(i,1) = 0.19608
        ofms(i,2) = 0.19608
        ofms(i,3) = 0.19608
        ofma(i,1) = 0.19608
        ofma(i,2) = 0.19608
        ofma(i,3) = 0.19608
      end do
c
c Purple
c
      i1 = i2 + 1
      i2 = i1 + nlevs(2) - 1
c      write (*,*) 'i1,i2=',i1,i2
      do i = i1, i2
        ofms(i,1) = 0.47451 
        ofms(i,2) = 0.0
        ofms(i,3) = 0.60784
        ofma(i,1) = 0.47451 
        ofma(i,2) = 0.0
        ofma(i,3) = 0.60784
      end do
c
c Dark blue
c
      i1 = i2 + 1
      i2 = i1 + nlevs(3) - 1
c      write (*,*) 'i1,i2=',i1,i2
      do i = i1, i2
        ofms(i,1) = 0.0
        ofms(i,2) = 0.0
        ofms(i,3) = 0.78431
        ofma(i,1) = 0.0
        ofma(i,2) = 0.0
        ofma(i,3) = 0.78431
      end do
c
c Light blue
c
      i1 = i2 + 1
      i2 = i1 + nlevs(4) - 1
c      write (*,*) 'i1,i2=',i1,i2
      do i = i1, i2
        ofms(i,1) = 0.37255
        ofms(i,2) = 0.65490
        ofms(i,3) = 0.92549
        ofma(i,1) = 0.37255
        ofma(i,2) = 0.65490
        ofma(i,3) = 0.92549
      end do
c
c This is the colour index at the boundary between blue and green.
c It is needed by the linear fiddler if we are keeping the zero
c intensity fixed at this colour.
c
      ci0 = i - 1
c
c Light green
c
      i1 = i2 + 1
      i2 = i1 + nlevs(5) - 1
c      write (*,*) 'i1,i2=',i1,i2
      do i = i1, i2
        ofms(i,1) = 0.0
        ofms(i,2) = 0.96471 
        ofms(i,3) = 0.0
        ofma(i,1) = 0.0
        ofma(i,2) = 0.96471
        ofma(i,3) = 0.0
      end do
c
c Dark green
c
      i1 = i2 + 1
      i2 = i1 + nlevs(6) - 1
c      write (*,*) 'i1,i2=',i1,i2
      do i = i1, i2
        ofms(i,1) = 0.0
        ofms(i,2) = 0.56863
        ofms(i,3) = 0.0
        ofma(i,1) = 0.0
        ofma(i,2) = 0.56863
        ofma(i,3) = 0.0
      end do
c
c Yellow
c
      i1 = i2 + 1
      i2 = i1 + nlevs(7) - 1
c      write (*,*) 'i1,i2=',i1,i2
      do i = i1, i2
        ofms(i,1) = 1.0
        ofms(i,2) = 1.0
        ofms(i,3) = 0.0
        ofma(i,1) = 1.0
        ofma(i,2) = 1.0
        ofma(i,3) = 0.0
      end do
c
c Orange
c
      i1 = i2 + 1
      i2 = i1 + nlevs(8) - 1
c      write (*,*) 'i1,i2=',i1,i2
      do i = i1, i2
        ofms(i,1) = 1.0
        ofms(i,2) = 0.69412
        ofms(i,3) = 0.0
        ofma(i,1) = 1.0
        ofma(i,2) = 0.69412
        ofma(i,3) = 0.0
      end do
c
c Red
c
      if (i2.lt.na) then
        i1 = i2 + 1
        i2 = min(na, i1+nlevs(9)-1)
c        write (*,*) 'i1,i2=',i1,i2
        do i = i1, i2
          ofms(i,1) = 1.0
          ofms(i,2) = 0.0
          ofms(i,3) = 0.0
          ofma(i,1) = 1.0
          ofma(i,2) = 0.0
          ofma(i,3) = 0.0
        end do
      end if
c
c White
c
      if (i2.lt.na) then
        i1 = i2 + 1
        i2 = min(na, i1+nlevs(10)-1)
c        write (*,*) 'i1,i2=',i1,i2
        do i = i1, i2
          ofms(i,1) = 1.0
          ofms(i,2) = 1.0
          ofms(i,3) = 1.0
          ofma(i,1) = 1.0
          ofma(i,2) = 1.0
          ofma(i,3) = 1.0
        end do
      end if
c
      end
c
c
      subroutine ofmtfe
c-----------------------------------------------------------------------
c     Erase transfer function plot
c
c Input in common
c   fidun True if a fiddle has been done which means the
c         transfer fucntion plot will have been drawn
c   tfpic True if we can draw the transfer function plot
c   tfvp  Viewport for transfer function plot
c   tfcs  Character height for transfer function labels
c
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      integer ci, fs
      real x1, x2, y1, y2, vx1, vx2, vy1, vy2, cs, xht, yht, dx, dy
c-----------------------------------------------------------------------
      if (.not.tfpic .or. .not.fidun) return
c
      call pgqvp (0, vx1, vx2, vy1, vy2)
      call pgqwin (x1, x2, y1, y2)
      call pgqcs (0, xht, yht)
      call pgqci (ci)
      call pgqch (cs)
      call pgqfs (fs)
c
c Set rectangle erase viewport and window
c
      dx = abs(tfvp(3)-tfvp(1))/50.0
      dy = abs(tfvp(4)-tfvp(2))/50.0
      call pgsvp (tfvp(1)-dx, tfvp(3)+dx, tfvp(2)-dy, tfvp(4)+dy)
      call pgswin (0.0, 1.0, 0.0, 1.0)
c
c Erase 
c
      call pgsci (0)
      call pgsfs (1)
      call pgrect (0.0, 1.0, 0.0, 1.0)
c
c Restore viewport
c
      call pgsvp (vx1, vx2, vy1, vy2)
      call pgswin (x1, x2, y1, y2)
      call pgsci (ci)
      call pgsch (cs)
      call pgsfs (fs)
c
      end
c
c
      subroutine ofmtff (npix, image, mask, imin, imax)
c-----------------------------------------------------------------------
c     Modify the transfer function with the cursor. Either linear,
c     histogram equalization or logarithmic transfer functions
c     are available
c
c Input
c    npix      Number of pixels in displayed image
c    image     Displayed image.  This is used only for the histogram
c              equalization mode.  If you pass n=1, image=0 viz
c              call ofmtff (1, 0, 0.0, 0.0, tfvp)  h.e. will not available
c    mask      Image mask. 0 -> blanked.
c    imin,max  Min and max used in call to PGIMAG.  Must be set for
c              histogram equalization or logarithmic transfer
c              functions to work
c
c-----------------------------------------------------------------------
      implicit none
      integer npix, mask(*)
      real image(*), imin, imax
cc
      include 'ofm.h'
      integer icyc
      real x, y, vx1, vx2, vy1, vy2, x1, x2, y1, y2
      character ch*1
      logical domsg, doapp
c-----------------------------------------------------------------------
c
c Tell user what's available
c
      call output (' ')
      call output ('**************   Begin transfer function '//
     +             'modification *************')
      call output (' ')
      if (nocurs) then
        call output ('To select next predefined transfer function '
     +             //'enter A')
        call output ('To exit to upper layer                      '
     +             //'enter X')
      else
        call output ('To select next predefined transfer function '
     +             //'click left   button (A)')
        call output ('To modify a linear transfer function        '
     +             //'click middle button (D)')
        call output ('To exit to upper layer                      '
     +             //'click right  button (X)')
      end if
      call output (' ')
c
c Get old viewport, window and character size
c
      call pgqwin (x1, x2, y1, y2)
      call pgqvp (0, vx1, vx2, vy1, vy2) 
c 
c Set easy viewport and window.  Use y extrema of 0.0001 
c and 0.9999 to avoid problems with divide by zero
c
      call pgsvp (0.0, 1.0, 0.0, 1.0)
      call pgswin (-1.0, 1.0, 0.9999, 0.0001)
c
c Navigate around fiddle loop
c
      x = 0.0
      y = 0.5
      ch = ' '
      icyc = 1
      domsg = .true.
      do while (ch.ne.'x')
c
c Get user input
c
        call ofmuin (x, y, ch)
        x = min(1.0,max(-1.0,x))
        y = min(0.9999,max(0.0001,y))
        doapp = .false.
c
        if (.not.nocurs .and. ch.eq.'d') then
c
c Linear transfer function fiddler
c
          call ofmlin (x, y, domsg)
          domsg = .false.
          doapp = .true.
        else if (ch.eq.'a') then
c
c Cycle through specific transfer function types
c
          if (icyc.gt.4) icyc = 1
          if (icyc.eq.1) then
c
c Linear, no fiddle
c
            call ofmlnf
            icyc = icyc + 1
            domsg = .true.
            doapp = .true.
          else if (icyc.eq.2) then
c
c Square root 
c
            if (iofm.eq.5) then
              call output ('Square root transfer function '//
     +                     'unavailable for fixed 0 colour contours')
            else
              if (imin.ne.imax) then
                call ofmsqr (imin, imax)
                domsg = .true.
                doapp = .true.
              else
                if (imin.eq.0.0) then
                  call bug ('w', 'Square root transfer function '//
     +                      'unavailable without display min/max')
                else
                  call bug ('w', 'Square root transfer function '//
     +                      'unavailable with INVALID min/max')
                end if 
              end if
            end if
            icyc = icyc + 1
          else if (icyc.eq.3) then
c
c Logarithmic
c
            if (iofm.eq.5) then
              call output ('Logarithmic transfer function '//
     +                     'unavailable for fixed 0 colour contours')
            else
              if (imin.ne.imax) then
                call ofmlog (imin, imax)
                domsg = .true.
                doapp = .true.
              else
                if (imin.eq.0.0) then
                  call bug ('w', 'Log transfer function unavailable '//
     +                      'without display min/max')
                else
                  call bug ('w', 'Log transfer function unavailable '//
     +                      'with INVALID min/max')
                end if 
              end if
            end if
            icyc = icyc + 1
          else if (icyc.eq.4) then
c
c Histogram equalization
c
              if (iofm.eq.5) then
                call output ('Histogram equalization unavailable '//
     +                       'for fixed 0 colour contours')
              else
                if (npix.gt.1) then
                  call ofmheq (npix, image, mask, imin, imax)
                  domsg = .true.
                  doapp = .true.
                else
                  call bug ('w', 'Histogram equalization unavailable '//
     +                      'without image & display min/max')
                end if
             end if
            icyc = icyc + 1
          end if
        else
c
c Exit
c
          ch = 'x'
          call output ('Finish transfer function modification')
          call output (' ')
          call output (' ')
c
c Remind user of what they can do
c
          call ofml1m
        end if
c
c Apply new ACTIVE ofm and plot transfer function
c
        if (ch.ne.'x' .and. doapp) then
          call ofmapp
          call ofmtfp
        end if          
      end do
c
c Restore viewport
c
      call pgswin (x1, x2, y1, y2)
      call pgsvp (vx1, vx2, vy1, vy2)
c
      end    
c
c
      subroutine ofmtfp 
c-----------------------------------------------------------------------
c     Draw a plot of the transfer function
c
c  Input in common
c    na      Number of levels in ofm
c    tfpic   True if we can draw the transfer function plot
c    tfvp    Viewport to draw plot in (normalized device coords)
c    xt,yt   Transfer function
c    yh      Histogram and cumulative histogrms from histogram
c            equalization routine
c    tfcs    Character height for transfer function labels
c-----------------------------------------------------------------------
      implicit none
cc
      include 'ofm.h'
      integer ci
      real x1, x2, y1, y2, vx1, vx2, vy1, vy2, cs, xht, yht
c-----------------------------------------------------------------------
      if (.not.tfpic) return
c
c Get old viewport and window
c
      call pgqwin (x1, x2, y1, y2)
      call pgqvp (0, vx1, vx2, vy1, vy2)
      call pgqch (cs)
      call pgqci (ci)
c
c Set character height to be 0.17 the size of the plot
c
      call pgsch (1.0)
      call pgqcs (0, xht, yht)
      tfcs = 0.17*min(tfvp(3)-tfvp(1),tfvp(4)-tfvp(2))/max(xht,yht)
      call pgsch (tfcs)
      call pgqcs (0, xht, yht)
c
c Erase old transfer function
c
      call ofmtfe 
c
c Draw box 
c
      call pgsci (2)
      call pgsvp (tfvp(1)+xht, tfvp(3), tfvp(2)+yht, tfvp(4)-yht)
      call pgswin (0.0, 1.0, 0.0, 1.0)
      call pgbox ('BC', 0.0, 0, 'BC', 0.0, 0)
c
c Label
c
      call pgmtxt ('B', 0.8, 0.5, 0.5, 'in')
      call pgmtxt ('L', 0.3, 0.5, 0.5, 'out') 
      call pgmtxt ('T', 0.3, 0.5, 0.5, tflab)  
c
c Draw transfer function
c
      call pgline (na, xt, yt)
      call pgupdt
c
c Restore viewport again
c
      call pgsvp (vx1, vx2, vy1, vy2)
      call pgswin (x1, x2, y1, y2)
      call pgsch (cs)
      call pgsci (ci)
c
      end
c
c
      subroutine ofmuin (x, y, cch)
c-----------------------------------------------------------------------
c     If the device has a cursor, get the input from it.  Otherwise,
c     just read the input character from the terminal.
c
c Output
c  cch     Character input from user
c
c-----------------------------------------------------------------------
      implicit none
      include 'ofm.h'
      real x, y
      character cch*1
c-----------------------------------------------------------------------
      if (nocurs) then
        read (*,'(a)') cch
      else
        call pgcurs (x, y, cch)
      end if
      call lcase (cch)
c
      end
