      program velplot

c= VELPLOT - Interactively plot spectra and slices from Miriad Images.
c& mchw
c: image analysis
c+
c       VELPLOT is an interactive task to analyse spectra, position-
c       velocity slices, and integrated velocity maps from Miriad
c       Images.  There are three basic display options:
c
c        - Velocity-averaged (x,y) images over selected velocity
c               intervals.  Also makes velocity and velocity dispersion
c               images.
c        - Spectra at selected (x,y) positions.  The data can be
c               convolved in x, y or velocity.  Gaussian fitting
c               routine.
c        - Position-velocity maps along user selected position angles,
c               or plots of intensity versus position for selected
c               velocity channels.  The data can be convolved in the
c               (x,y) plane.
c
c       (x,y) positions are (HA,DEC) in arcsec relative to map center.
c       Position angles are measured from the north towards the east.
c
c       The output from each option can be plotted, printed, or written
c       out as Miriad images of velocity-averaged maps, position-
c       velocity maps, or ascii spectra respectively.  Gaussian fits are
c       written into the log.
c
c       The task keeps user-defined lists of velocity-averaged maps,
c       position-velocity cuts, and spectra positions.  These lists can
c       be created and edited within the task, and can be passed between
c       the three displays.  For example, a list of spectra or position-
c       velocity cuts can be created with the cursor whilst displaying a
c       velocity-averaged map.  The lists are used to display the
c       spectra and position-velocity cuts, and can be written out and
c       read in as ascii files.
c
c       An options menu provides a choice of display style, contour
c       levels, and units.  Further interactive help is available within
c       the task.
c
c       For an interactive X windows interface, see VELPLOTC
c       (limited distribution)
c@ in
c       Input image name. No default.
c@ device
c       The PGPLOT plotting device.
c@ region
c       Region of image to be plotted, e.g.
c         region=relpix,box(-30,-30,20,90)(16,57)
c       reads in 51 x 121 pixels of image planes 16 to 57.  See help on
c       'region' for more details.  The default is the whole image.  The
c       maximum array size is set by MAXDIM in 'velplot.h' If the array
c       is larger than memory, then VELPLOT may run slowly.
c@ log
c       The output log file. The default filename is velplot.log
c       Results from image analysis are written into the log file.
c
c$Id: velplot.for,v 1.9 2010/11/22 05:09:47 cal103 Exp $
c--
c  History:
c       developed from Ralint's Velplot (Fuller, Vogel, Wright, 1970's)
c    Nov 1988 mchw Adjustable array dimensions.
c    aug 1989      Added subroutines to read and write Miriad files.
c    25may90 mchw  Major rebuild using Miriad user interface.
c                  Removed Ralint subroutines, mapio etc.
c    15jan91 mchw  Added in-code doc and removed a couple of bugs.
c    23jul91 dm/dw modified VMS version for UNIX
c    25jul91 mchw  removed pi from velplot.h to avoid MONGO conflict.
c    31jul91 djw   PGPLOT version
c    02aug91 mchw  Ported back to Vax and minor bugs cleaned up.
c    14aug91 ms    Patches for cray.
c    02oct91 djw   Fixed Pgplot transformation arrays (1 pixel).
c    07oct91 djw   Modified cursor options to draw specs and cuts.
c    25oct91 mchw  More patches for cray.
c    28oct91 djw   Grayscale
c    31oct91 mchw  Changed grid of spectra to match plot on paper.
c                       Changed ifdef to cft, and improved user i/o.
c                       Added width to range of maps. More cleanup.
c    10apr92 mjs   In sub velmap, "label" now char*30 to elim string
c                       truncation in assignment.
c    13apr92 mchw  Changed position-velocity plots to increment in
c                       direction of pa and fixed bug for unequal axes.
c    28may92 mchw  Convert frequency axis on input image to velocity.
c    29may92 mchw  Changed to absolute cutoff level in moment maps.
c                  Added integral and rms in cursor defined box.
c    04jun92 mchw  Read and write ascii files of spectra & pos-vel cuts.
c    09jun92 mchw  Added Gaussian Fits to spectra using nllsqu.
c    10jun92 djw   Added Multiple Gaussian Fits to spectra.
c    10jun92 mchw  Save rms estimates. Write Gaussian fits into log.
c    12jun92 mchw  Estimate fit from moments. Less things to type in.
c    15jun92 jat   Add position-intensity cuts to PosVel.
c    16jun92 mchw  Write more info' into log. Improved documentation.
c    07jul92 mjs   Remove doubly-declared variables (Convex coughs).
c    13jul92 mchw  Fix new ?? bug in Intensity plot input in PosVel.
c    19aug92 mchw  Fix bug in integration box. Normalize chisq in fit.
c    04sep92 mchw  Spectra options: omits plots, rectangular grids.
c    16sep92 mchw  Better image header for position-velocity maps.
c    23sep92 mchw  Add option to get velocity of peak channel.
c    30sep92 mchw  Add history to output image files.
c    05nov92 djw   Add option to write ascii file of gaussian fits.
c    27feb93 mjs   use tmpdim.h instead of maxdim.h
c    13mar93 mjs   pgplot subr names have less than 7 chars.
c    02jul93 mjs   Elim unused labeled stmts to elim compiler warnings;
c                  elim branches to labels from outside the block.
c    08jul93 mjs   Merge 2 versions with minor differences.
c    21sep93 mchw  Added nint to integer expressions using pixel size.
c    25oct93 mchw  Added nint in subroutine readmap.
c    01feb95 jm/mchw Elliminate some questions. call pgask(.FALSE.)
c    02feb95 jm    De-FLINT'd.
c    16jun95 mchw  Change default: no Gaus fit. Trap typo. pgask(.TRUE.)
c                       swap P and V cursor options.
c    30jan96 mchw  Merge AT version with 16jun95 update.
c    09feb96 mchw  Address some of complaints in AT Users Guide.
c    09nov96 mchw  Allow for FREQ axis.
c    16may97 mchw  Increase array dimensions to MAXDIM.
c    23may97 mchw  Cosmetics. More information for users.
c    10jun97 rjs   Use memalloc to reduce memory usage.
c    04sep97 mchw  Upgrading. Report absolute positions with cursor.
c    10sep97 mchw  Fix new bug at exactly 45.0 position angle.
c    08jul98 mchw  Improve code in spectra and Gaussian fits.
c    16jul98 mchw  More robust interactive input; Elliminate ifdef's.
c    05mar99 mchw  added logarithmic contour levels.
c    05apr00 mchw  specify RA and DEC cuts with a range and increment.
c    29aug00 mchw  Label RA, DEC on spectra and position-velocity plots.
c    19sep00 pjt   increased filenames
c    27jan01 pjt   fixed bug due to above
c    22mar02 mchw  better logarithmic contour levels.
c    26jun02 mchw  fixed bug due to longer filenames. (cf. 19sep00)
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mem.h'

      integer    MAXBOXES
      parameter (MAXBOXES=128)

      integer boxes(MAXBOXES),nsize(3)
      character ans*1,line*80,logfil*80
      integer lIn,nx,ny,nc,length
      real vlsr(MAXDIM)
      integer ary,v

c     Set default plotting parameters.
      data units/'J'/,alabel/'Y'/,percent/'Y'/
      data apint/'N'/,abscoord/'N'/,write/'N'/
      data cneg/'Y'/,pspec/'Y'/
      data gray/'N'/,defgray/'Y'/
      data lgaufit/'N'/,lgauplot/'Y'/
      data levels/15,30,45,60,75,90,0,0,0,0/,nlevels/6/

      character versan*80, version*80
c-----------------------------------------------------------------------
      version = versan('velplot',
     *                 '$Revision: 1.9 $',
     *                 '$Date: 2010/11/22 05:09:47 $')
c
c  Get the input parameters.
c
      call keyini
      call keya('in',file,' ')
      if (file.eq.' ') call bug('f','Image Name missing')
      call BoxInput('region',file,boxes,MAXBOXES)
      call keya('device',device,'?')
      call keya('log',logfil,'velplot.log')
      call keyfin
c
c  read Miriad array and check dimensions requested.
c
      call xyopen(lIn,file,'old',3,nsize)
      call BoxMask(lIn,boxes,MAXBOXES)
      call BoxSet(boxes,3,nsize,' ')
      call BoxInfo(boxes,3,blc,trc)
      nx = trc(1) - blc(1) + 1
      ny = trc(2) - blc(2) + 1
      nc = trc(3) - blc(3) + 1
      call output('file: '//file)
      write(line, *) 'Array dimensions are: nx,ny,nc = ',nx,ny,nc
      call output(line)
      write(line, *) 'Array size is: nx*ny*nc = ',nx*ny*nc
      call output(line)
      write(line, *) 'Maximum array dimension is ', MAXDIM
      call output(line)
      call memAlloc(ary,nx*ny*nc,'r')
      call memAlloc(v,max(nx*ny,max(nx,ny)*nc),'r')
      if (nx.gt.MAXDIM .or. ny.gt.MAXDIM .or. nc.gt.MAXDIM)
     *  call bug('w','Dimension too big for some buffers')
      call output(' ')
      call readmap(lIn,memr(ary),vlsr,nx,ny,nc)
c
c  Tell user how to exit from xwindow.
c
      if (index(device,'/x').ne.0) then
        call output('Use RIGHT mouse button to exit from xwindow')
      endif
c
c set default parameters
c
      bblc(1) = 1
      bblc(2) = 1
      btrc(1) = nx
      btrc(2) = ny
      cutoff = -9998.0
c
c  Start the output log file.
c
      call LogOpen(logfil,'q')
      call LogWrit('VELPLOT '//version)
      call velohead(nx,ny,nc)
c
c  Prompt for interactive options:
c
      do while (.true.)
        call output(' ')
        call output('MAIN MENU:')
        call output('Comment  - Write comment into log')
        call output('Integral - Integrated flux and statistics')
        call output('List     - List header and velocity information')
        call output('Options  - Select plot parameters')
        call output('Pos-Vel  - Plot versus position & velocity')
        call output('Spectra  - Plot spectra')
        call output('Vel-map  - Plot integrated velocity maps')
        call output('File     - Write spectra & position-velocity file')
        call output('Read     - Read spectra & position-velocity file')
        call output('Write    - Write Miriad Image to disk file')
        call output('Exit     - Exit from program')
        call output(' ')

        call prompt(ans,length,'Selection (type 1st character) :')
        call ucase(ans)
        if (ans.eq.'C') then
          call comment
        else if (ans.eq.'F') then
          call wrary
        else if (ans.eq.'I') then
          call integral(memr(ary),vlsr,nx,ny,nc)
        else if (ans.eq.'L') then
          call velohead(nx,ny,nc)
        else if (ans.eq.'O') then
          call plOpts
        else if (ans.eq.'P') then
          call posVel(memr(ary),vlsr,nx,ny,nc,memr(v))
        else if (ans.eq.'R') then
          call rdary
        else if (ans.eq.'S') then
          call spectra(memr(ary),vlsr,nx,ny,nc)
        else if (ans.eq.'V') then
          call velmap(lIn, nx, ny, nc, memr(ary), vlsr, memr(v))
        else if (ans.eq.'W') then
          vel=vlsr(1)
          delv=vlsr(2)-vlsr(1)
          call writemap(lIn, nx, ny, nc, memr(ary))
        else if (ans.eq.'E') then
          call memFree(v,max(nx*ny,max(nx,ny)*nc),'r')
          call memFree(ary,nx*ny*nc,'r')
          call LogClose
          stop
        endif
      enddo

      end

c***********************************************************************

      subroutine comment

c-----------------------------------------------------------------------
c  Enter comment into log
c-----------------------------------------------------------------------
      character*80 text
      integer ktext
c-----------------------------------------------------------------------
      call prompt(text,ktext,'Enter comment >')

      do while (text.ne.' ')
        call LogWrit('comment >'//text(1:ktext))
        call prompt(text,ktext,'      comment >')
      enddo

      end

c***********************************************************************

      subroutine conv(ary,nx,ny,nc,x0,y0,ncon,con,conary,wt)

      integer nx,ny,nc,ncon
      real ary(nx,ny,nc), con(ncon,ncon,4,4), conary(nc)
      real x0,y0
c-----------------------------------------------------------------------
c       performs 2-dimensional convolution of ARY by Gaussian
c       centered at (X0,Y0); returns convolution in CONARY.
c       changed to variable size array Oct 1987 MCHW
c       Idiot proof loop for cray. 25oct91 mchw.
c-----------------------------------------------------------------------
      integer ix,iy,noff,ioff,joff,mid,ic,jc
      integer i,j,k
      real wt,gaus
c-----------------------------------------------------------------------
c
c  find nearest sample point
c
      ix = nint(x0)
      iy = nint(y0)
c
c  find offset to nearest 1/noff and move to lower left sample point ---
c  ioff = 1 is zero offset
c
      noff = 4
      ioff = nint(noff*(x0-ix))
      if (ioff.lt.0) then
        ix = ix-1
        ioff = ioff + noff
      endif
      ioff = ioff + 1

      joff = nint(noff*(y0-iy))
      if (joff.lt.0) then
        iy = iy-1
        joff = joff + noff
      endif
      joff = joff + 1
c
c  Initialize convolved output array.
c
      wt = 0.0
      do 10 k = 1, nc
10      conary(k) = 0.0
c
c  Convolve array by convolution function.
c
      mid = ncon/2 + 1
      do 40 ic = 1, ncon
        i = ix + ic - mid
        if (i.lt.1 .or. i.gt.nx) go to 40
        do 30 jc = 1, ncon
          j = iy + jc - mid
          if (j.lt.1 .or. j.gt.ny) go to 30
          gaus = con(ic, jc, ioff, joff)
          wt = wt + gaus
          do 20 k = 1, nc
            conary(k) = conary(k) + gaus * ary(i,j,k)
20        continue
30      continue
40    continue
c
c  renormalize
c
      if (wt.ne.0.0) then
        do k = 1, nc
          conary(k) = conary(k) / wt
        enddo
      endif

      end

c***********************************************************************

      subroutine convinit(cmaj,cmin,cpa,xy,ncon,con)

      integer ncon
      real cmaj,cmin,cpa,xy
      real con(ncon,ncon,4,4)
c-----------------------------------------------------------------------
c  Set up 2-dimensional convolution function for CONV
c       variable size array Oct 1987 MCHW
c-----------------------------------------------------------------------
      include 'mirconst.h'

      real cma,cmi,sinpa,cospa,off
      real xp,yp,sum,gaus,x,y
      integer i,j,noff,ioff,joff,mid
c-----------------------------------------------------------------------
c  Set minimum convolving beam for interpolating data ---
c --- save original inputs; convert to pixels  ---
c     sigma = fwhm/1.665, exp(- (0.5* xy/0.6*xy)**2) = 0.5
      cma = max(cmaj/xy/1.665, 0.6)
      cmi = max(cmin/xy/1.665, 0.6)

c     Position angle north tho' east.
      sinpa = -sin(cpa*D2R)
      cospa =  cos(cpa*D2R)

c     Center to nearest 1/off of a pixel.
      mid  = NCON/2 + 1
      off  = 4.0
      noff = off
c
c  Build convolution function
c
      do 20 ioff = 1, noff
        xp = real(ioff - 1)/off
        do 20 joff = 1, noff
          yp = real(joff - 1)/off
          sum = 0.0
          do 10 i = 1, ncon
            x = real(i-mid) - xp
            do 10 j = 1, ncon
              y = real(j-mid) - yp
              gaus = ((-x*sinpa+y*cospa)/cma)**2
     *                         + ((x*cospa+y*sinpa)/cmi)**2
              if (gaus.lt.4.6) then
                con(i,j,ioff,joff) = exp(-gaus)
              else
                con(i,j,ioff,joff) = 0.0
              endif
10            sum = sum + con(i,j,ioff,joff)
              do 15 i = 1, ncon
                do 15 j = 1, ncon
15              if (sum.ne.0.0)
     *            con(i,j,ioff,joff) = con(i,j,ioff,joff)/sum
20    continue

      end

c***********************************************************************

      subroutine convsize(cmaj,cmin,cpa,xy,ncon)

      real cmaj,cmin,cpa,xy
      integer ncon
c-----------------------------------------------------------------------
c  Determine size of convolution array for VELO
c               mchw Oct 1987
c  Inputs:
c    xy                 Pixel size
c  Outputs:
c    cmaj,cmin,cpa      Convolving beam and position angle.
c    ncon               Size of convolution array.
c-----------------------------------------------------------------------
      character*80 line
      integer length
      double precision dval(3)
      logical ok
c-----------------------------------------------------------------------
c
c  Initialize convolving beam.
c
      cmaj = 0.0
      cmin = 0.0
      cpa  = 0.0

10    call prompt(line,length,
     *  '>Enter convolving beam (major("), minor("), pa(deg): ')
      if (length.eq.0) goto 20
        call matodf(line,dval,3,ok)
        if (ok) then
          cmaj = dval(1)
          cmin = dval(2)
          cpa  = dval(3)
        else
          goto 10
        endif
20    ncon = 2*cmaj/xy + 1
      ncon = min(99,(max(ncon,3)))
      call output('Gaussian falls to 6% at edge of array')
      write(line, 105) ncon, ncon*xy
      call output(line)
105   format(' size of convolution array:',i5,' pixels,',f8.3,
     *       ' arcsecs')

      end

c***********************************************************************

      subroutine cursor(ary,nx,ny,key)

      integer nx,ny
      real ary(nx,ny)
      character key*1
c-----------------------------------------------------------------------
c  Cursor options
c
c  Inputs:
c    ary(nx,ny) The current array,
c  Outputs:
c    key        cursor option
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mirconst.h'

      integer   sym
      real      flux, pat, sx(2), sy(2), xpts(2), xx, xx1, ypts(2), yy,
     *          yy1
      character key2, msg*80, ras*13, decs*13

c     External.
      character  hangleh*13, rangleh*13

      integer   nspec, ngauss(49)
      real      xc(49), yc(49), gauss(49,3,10), gausserr(49,3,10)
      common /spectrae/ xc, yc, nspec, ngauss, gauss, gausserr

      integer   ncut
      real      xcut(128), ycut(128), pa(128)
      common /cuts/ xcut, ycut, pa, ncut
c-----------------------------------------------------------------------
10    call output('>CURSOR OPTIONS: Type L to list options: ')
      xx = 0.0
      yy = 0.0
      call pgcurs(xx,yy,key)
      call ucase(key)

c     List options.
      if (key.eq.'L') then
        call output(' Cursor options:')
        call output(' A - absolute coordinates')
        call output(' B - blc for integral')
        call output(' C - change contour interval')
        call output(' D - clear spec/pos-vel stacks')
        call output(' E - exit from plot')
        call output(' X - exit from plot')
        call output(' G - grayscale')
        call output(' H - header label')
        call output(' I - integral and rms in box')
        call output(' J - Jy contours')
        call output(' K - Kelvin contours')
        call output(' N - negative contours')
        call output(' P - cursor position and value')
        call output(' Q - plot spec/pos-vel positions')
        call output(' R - replot maps')
        call output(' S - spectra position')
        call output(' T - trc for integral')
        call output(' V - define pos-vel cut')
        call output(' W - write out this map')
        call output('(x,y) positions are in (HA,DEC) directions.')
        call output('(Position angle is measured from N through E)')

      else if (key.eq.'A') then
c       Absolute coordinate labels.
        abscoord='N'
        call output('Sorry...option not yet implemented.')
      else if (key.eq.'B') then
c       Define box and find integral and rms.
        bblc(1) = nint(xx/xy) + brpix(1)
        bblc(2) = nint(yy/xy) + brpix(2)
        write(msg, *) 'blc=(', bblc(1)-brpix(1), ',',
     *                         bblc(2)-brpix(2), ')'
        call output(msg)
      else if (key.eq.'C') then
c       Change  contour interval.
        call setCont
      else if (key.eq.'D') then
c       Clear spectrum pos-vel stacks.
        call output('-cleared spec/pos-vel stacks')
        nspec=0
        ncut=0
      else if (key.eq.'E' .or. key.eq.'X') then
c       Exit from plot.
        key = 'E'
        return
      else if (key.eq.'G') then
c       Change grayscale.
        if (gray.eq.'Y') then
          gray='N'
          call output('-will not include grayscale')
        else
          gray='Y'
          call output('-will include grayscale')
          call setGray
        endif
      else if (key.eq.'H') then
c       Annotate plot.
        if (alabel.eq.'Y') then
          alabel='N'
          call output('-will not annotate plot')
        else
          alabel='Y'
          call output('-will annotate plot')
        endif
      else if (key.eq.'I') then
        call integrate(ary,nx,ny)
      else if (key.EQ.'J' .and. units.eq.'K') THEN
c       Change plot units.
        units='J'
        call output('-units will be Janskeys')
      else if (key.EQ.'K' .and. units.eq.'J') THEN
        units='K'
        call output('-units will be Kelvin')
      else if (key.eq.'N') then
c       Plot negative contours.
        if (cneg.eq.'Y') then
          cneg='N'
          call output('-will not plot negative contours')
        else
          cneg='Y'
          call output('-will plot negative contours')
        endif
      else if (key.eq.'P') then
c       Cursor position and value.
        flux = ary(nint(xx)+brpix(1),nint(yy)+brpix(2))
        write(msg, *) 'x=',xx, '  y=',yy, '  value=',flux
        call output(msg)
c       RA and dec.
        ras  = hangleh(dble(crval(1)-xx*AS2R/cos(crval(2))))
        decs = rangleh(dble(crval(2)+yy*AS2R))
        write(msg,'(a,a)') ras, decs
        call output(msg)
      else if (key.eq.'Q') then
c       Plot spectra positions.
        if (pspec.eq.'Y') then
          pspec='N'
          call output('-will not indicate spec/pos-vel positions')
        else
          pspec='Y'
          call output('-will indicate spec/pos-vel positions')
        endif
      else if (key.eq.'R') then
c       Replot on display.
        call pgend
        return
      else if (key.eq.'S') then
c       Get position for spectra.
        if (nspec.ge.49) then
          call output('maximum 49 spectra')
          go to 10
        endif
        nspec=nspec+1
        xc(nspec)=xx
        yc(nspec)=yy
        write(msg, 113) nspec,xx,yy
113     format(' spectra(',i2,') x=',f8.3,'  y=',f8.3)
        call output(msg)
        sx(1)=xc(nspec)
        sy(1)=yc(nspec)
        sym=nspec+64
        call pgpt(1,sx,sy,sym)

c       RA and Dec.
        ras  = hangleh(dble(crval(1)-xx*AS2R/cos(crval(2))))
        decs = rangleh(dble(crval(2)+yy*AS2R))
        write(msg,'(a,a)') ras, decs
        call output(msg)
      else if (key.eq.'T') then
        btrc(1) = nint(xx/xy) + brpix(1)
        btrc(2) = nint(yy/xy) + brpix(2)
        write(msg, *) 'trc=(', btrc(1)-brpix(1), ',',
     *                         btrc(2)-brpix(2), ')'
        call output(msg)
      else if (key.eq.'V') then
c       Get pos-vel cuts.
        if (ncut.ge.25) then
          call output('maximum 25 pos-vel cuts')
          go to 10
        endif

        ncut=ncut+1
        call output('>position-velocity cut:')
        call output('  define position angle with two points')
        call output('  move cursor and strike any key')
        xx1=xx
        yy1=yy
        call pgcurs(xx1,yy1,key2)
        pa(ncut)=atan((xx-xx1)/(yy1-yy))*R2D
        if (pa(ncut).lt.0) pa(ncut)=pa(ncut)+180.0
        xcut(ncut)=0.5*(xx1+xx)
        ycut(ncut)=0.5*(yy1+yy)
        write(msg, 114) ncut,xcut(ncut),ycut(ncut),pa(ncut)
114     format(' cut(',i2,') x=',f8.3,' y=',f8.3,' pa=',f8.3)
        call output(msg)
        pat=-tan((90.0-pa(ncut))*D2R)
        xpts(1)=-1e4
        ypts(1)=-1e4*pat+(ycut(ncut)-xcut(ncut)*pat)
        xpts(2)=1e4
        ypts(2)=1e4*pat+(ycut(ncut)-xcut(ncut)*pat)
        call pgline(2,xpts,ypts)
        sx(1)=xcut(ncut)
        sy(1)=ycut(ncut)
        sym=ncut+96
        call pgpt(1,sx,sy,sym)
      else if (key.eq.'W') THEN
c       Write out map.
        if (write.eq.'N') then
          write='Y'
          call output('-will write MIRIAD Image to disk')
          return
        else
          write='N'
          call output('-will not write MIRIAD Image to disk')
        endif
      else
        write(msg, *) 'Unknown key chosen: ' // key
        call output(msg)
        go to 10
      endif
      go to 10

      end

c***********************************************************************

      subroutine gaufit(nc,vlsr,t,spec,rms)

      real rms
c-----------------------------------------------------------------------
c Subroutine fits multiple gaussians to spectrum using numerical
c recipes routines FGAUSS and MRQMIN. Up to 30 (3x10) free parameters.
c See Numerical Recipes pp. 521-528 for details.
c
c User specified parameters:
c  i. spectrum rms (used to aid convergence and for error estimates)
c  ii. number of gaussians to fit (currently restricted to 10)
c  iii. initial guesses for gaussian heights, velocities, fwhms
c
c N.B. Performance is closely related to initial guesses!
c
c Inputs:
c  nc      number of channels
c  vlsr    velocity array
c  t       amplitudes
c  spec    spectrum index
c  rms     current rms estimate
c Ouputs
c  ngauss  number of simultaneously fitted  gaussians
c  gauss   gaussian parameters
c           gauss(1,i) amplitude of gaussian i
c           gauss(2,i) center velocity of gaussian i
c           gauss(3,i) sigma of gaussian i
c  gausserr error estimates for gaussian parameters
c-----------------------------------------------------------------------
      include 'velplot.h'

      integer    NMA
      parameter (NMA=30)

      logical   ok
      integer   i, itst, j, k, k1, k2, length, lista(NMA), ma, mfit, nc,
     *          spec, tlen
      real      a(NMA), alamda, alpha(NMA,NMA), amp, chisq,
     *          covar(NMA,NMA), mom1, mom2, ochisq, rmsEst,
     *          sig(MAXDIM), t, value, vlsr
      double precision dval
      character ans*80, line*80, string*80

      integer   nspec, ngauss(49)
      real      xc(49), yc(49), gauss(49,3,10), gausserr(49,3,10)
      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr

      external fgauss
c-----------------------------------------------------------------------
c ask user to continue or not
      call output(' ')
      call prompt(ans, length, '>Continue with gaussian fit ([Y]/N): ')
      call ucase(ans)
      if (ans(1:1).eq.'N') return
c get number of simultaneous gaussians to fit
      ngauss(spec)=1
      call promptf(value,'f2.0',
     *  '>Enter number of gaussians to fit',real(ngauss(spec)))
      if (value.lt.1.0) return
      ngauss(spec)=value
c ma is the number of free parameters to be fit
      ma=ngauss(spec)*3
c mfit must be fit if some parameters held fixed; for the future
      mfit=ngauss(spec)*3
c get initial guesses for amp,center,width,rmsEst
c
      call moment1(nc,vlsr,t,rms,amp,mom1,mom2)
      do i = 1, ngauss(spec)
        write(line,107) i,amp,mom1,mom2*2.0*sqrt(log(2.0))

107     format('>Gaussian no. ',i1,', Enter amp, vel, fwhm [',3f8.3,
     *         '] :')
        call prompt(ans,length,line)
        gauss(spec,1,i)=amp
        gauss(spec,2,i)=mom1
        gauss(spec,3,i)=mom2
        if (length.ne.0) then
          k1 = 1
          k2 = length
          k  = 0
          do while (k1.le.k2 .and. k.lt.3)
            call getfield(ans, k1, k2, string, tlen)
            call atodf(string,dval,ok)
            if (ok) then
              k = k+1
              gauss(spec,k,i) = dval
            else
              call bug('w', 'bad input in gaufit')
              return
            endif
          enddo
          gauss(spec,3,i)=gauss(spec,3,i)/(2.0*sqrt(log(2.0)))
        endif
      enddo

      call promptf(rmsEst, 'f10.4', '>Enter rms estimate: ', rms)
      rms = rmsEst

      do 13 i = 1, mfit
        lista(i)=i
13    continue

      j=1
      do 14 i = 1, ngauss(spec)
        a(j) = gauss(spec,1,i)
        a(j+1)=gauss(spec,2,i)
        a(j+2)=gauss(spec,3,i)
        j = j + 3
14    continue

      do i = 1, nc
        sig(i)=rmsEst
      enddo
c
c first call to mrqmin uses initial guess and sets alamda to 0.001
c
      alamda=-1.0

      call mrqmin(vlsr,t,sig,nc,a,ma,lista,mfit,covar,alpha,
     *            nma,chisq,fgauss,alamda)
c
c repeat calls to mrqmin until delta chisq is less than 0.01
c (note that chisq is not divided by ndata in mrqcof)
c
      k=1
      itst=0
1     WRITE(*,'(/1X,A,I2,T18,A,F10.4,T43,A,E9.2)') 'Iteration #',K,
     *        'Chi-squared:',chisq/nc, 'alamda:',alamda
      k=k+1
      ochisq=chisq
      call mrqmin(vlsr,t,sig,nc,a,ma,lista,mfit,covar,alpha,
     *            nma,chisq,fgauss,alamda)

      if (chisq.gt.ochisq) then
        itst=0
      else if (abs(ochisq-chisq).lt.0.01) then
        itst=itst+1
      endif

      if (itst.lt.2) then
        goto 1
      endif
c
c make one last call to mrqmin to get the covariance matrix
c
      alamda=0.0
      call mrqmin(vlsr,t,sig,nc,a,ma,lista,mfit,covar,alpha,
     *            nma,chisq,fgauss,alamda)

      j = 1
      do 24 i = 1, ngauss(spec)
        gauss(spec,1,i) = a(j)
        gauss(spec,2,i)=a(j+1)
        gauss(spec,3,i)=a(j+2)
        j = j + 3
24    continue
c
c print results and write them into the log
c
      call LogWrit(' ')
      write(line,'(a,f7.2,a,f7.2,a,a,f10.4,a,f10.4)')
     *  '(', xc(spec), ',', yc(spec), ')', ' rmsEst=', rmsEst,
     *  '  chisq=', chisq/nc
      call LogWrit(line)
      j=1
      do i = 1, ngauss(spec)
        write(line,'(a,a,a,a)')
     *    ' Gaussian ',' amplitude',' velocity ','   fwhm   '
        call output(line)
        call LogWrit(line)
        write(line,'(i10,3f10.4)') i,gauss(spec,1,i),gauss(spec,2,i),
     *    gauss(spec,3,i)*1.665
        call output(line)
        call LogWrit(line)

c       Error estimates.
        gausserr(spec,1,i)=sqrt(covar(j,j))
        gausserr(spec,2,i)=sqrt(covar(j+1,j+1))
        gausserr(spec,3,i)=sqrt(covar(j+2,j+2))
        j=j+3
        write(line,157)
157     format('  Estimated errors:')
        call output(line)
        call LogWrit(line)
        write(line,'(i10,3f10.4)') i, gausserr(spec,1,i),
     *                                gausserr(spec,2,i),
     *                                gausserr(spec,3,i)*1.665
        call output(line)
        call LogWrit(line)
      enddo

      end

c***********************************************************************

      SUBROUTINE FGAUSS(X,A,Y,DYDA,NA)

      integer na
      real a,x,y,dyda
      real ex,fac,arg
      DIMENSION A(NA),DYDA(NA)
      integer i
c-----------------------------------------------------------------------
      Y=0.0
      DO 11 I=1,NA-1,3
        ARG=(X-A(I+1))/A(I+2)
        EX=EXP(-ARG**2)
        FAC=A(I)*EX*2.0*ARG
        Y=Y+A(I)*EX
        DYDA(I)=EX
        DYDA(I+1)=FAC/A(I+2)
        DYDA(I+2)=FAC*ARG/A(I+2)
11    CONTINUE
      RETURN

      END

c***********************************************************************

      SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)

      integer n,np,m,mp,nmax,ipiv,indxr,indxc,ll
      PARAMETER (NMAX=50)
      real a,b
      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      integer i,j,k,l,irow,icol
      real big,dum,pivinv
c-----------------------------------------------------------------------
      DO 11 J=1,N
        IPIV(J)=0
11    CONTINUE
      DO 22 I=1,N
        BIG=0.0
        DO 13 J=1,N
          IF(IPIV(J).NE.1) THEN
            DO 12 K=1,N
              IF (IPIV(K).EQ.0) THEN
                IF (ABS(A(J,K)).GE.BIG) THEN
                  BIG=ABS(A(J,K))
                  IROW=J
                  ICOL=K
                ENDIF
              ELSE IF (IPIV(K).GT.1) THEN
                call bug('f', 'Singular matrix.')
              ENDIF
12          CONTINUE
          ENDIF
13      CONTINUE
        IPIV(ICOL)=IPIV(ICOL)+1
        IF (IROW.NE.ICOL) THEN
          DO 14 L=1,N
            DUM=A(IROW,L)
            A(IROW,L)=A(ICOL,L)
            A(ICOL,L)=DUM
14        CONTINUE
          DO 15 L=1,M
            DUM=B(IROW,L)
            B(IROW,L)=B(ICOL,L)
            B(ICOL,L)=DUM
15        CONTINUE
        ENDIF
        INDXR(I)=IROW
        INDXC(I)=ICOL
        IF (A(ICOL,ICOL).EQ.0.0) call bug('f', 'Singular matrix.')
        PIVINV=1.0/A(ICOL,ICOL)
        A(ICOL,ICOL)=1.0
        DO 16 L=1,N
          A(ICOL,L)=A(ICOL,L)*PIVINV
16      CONTINUE
        DO 17 L=1,M
          B(ICOL,L)=B(ICOL,L)*PIVINV
17      CONTINUE
        DO 21 LL=1,N
          IF(LL.NE.ICOL) THEN
            DUM=A(LL,ICOL)
            A(LL,ICOL)=0.0
            DO 18 L=1,N
              A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
18          CONTINUE
            DO 19 L=1,M
              B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
19          CONTINUE
          ENDIF
21      CONTINUE
22    CONTINUE
      DO 24 L=N,1,-1
        IF(INDXR(L).NE.INDXC(L)) THEN
          DO 23 K=1,N
            DUM=A(K,INDXR(L))
            A(K,INDXR(L))=A(K,INDXC(L))
            A(K,INDXC(L))=DUM
23        CONTINUE
        ENDIF
24    CONTINUE
      RETURN

      END

c***********************************************************************

      SUBROUTINE MRQMIN(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,
     *  COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)

c-----------------------------------------------------------------------
c Levenberg-Marquant method, attempting to reduce the value of chisq
c of a fit between a set of NDATA points X(I),Y(I) with individual
c standard deviations, SIG(I), and a nonlinear function dependent on MA
c coeff A.
c The array LISTA numbers the parameters A such that the first MFIT
c elements correspond to values actually being adjusted; the remaining
c MA-MFIT parameters are held fixed at their input values. The routine
c returns current best fit values for the MA fit paramters A, and CHISQ.
c COVAR(NCA,NCA) and ALPHA(NCA,NCA) with physical dinemsion NCA >= MFIT
c are used as working space during most iterations. Supply a subroutine
c FUNCS(X,A,YFIT,DYDA,MA) that evaluates the fitting function YFIT and
c its derivatives DYDA with respect to the fitting parameters A at X.
c On the first call provide an inital guess for the parameters A, and
c set ALAMDA<0 for initialization (which then sets ALAMDA=0.001).  If a
c step succeeds CHISQ becomes smaller and ALAMDA decreases by a factor
c of 10.  If a step fails ALAMDA grows by a factor of 10.  You must call
c this routine repeatedly until convergence is achieved.
c Then make one final call with ALAMDA=0 so that COVAR(I,J) returns
c the covariance matrix and ALPHA(I,J) he curvature matrix.
c
c     MMAX changed from 20 to 30, djw.
c-----------------------------------------------------------------------
      integer mmax
      PARAMETER (MMAX=30)
      integer ndata,lista,ma,mfit,nca
      real x,y,sig,a,covar,alpha,atry,beta,da
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),A(MA),LISTA(MA),
     *  COVAR(NCA,NCA),ALPHA(NCA,NCA),ATRY(MMAX),BETA(MMAX),DA(MMAX)
      real chisq,ochisq,alamda
      integer j,k,kk,ihit

      external funcs
c-----------------------------------------------------------------------
      IF(ALAMDA.LT.0.0) THEN
        KK=MFIT+1
        DO 12 J=1,MA
          IHIT=0
          DO 11 K=1,MFIT
            IF(LISTA(K).EQ.J) IHIT=IHIT+1
11        CONTINUE
          IF (IHIT.EQ.0) THEN
            LISTA(KK)=J
            KK=KK+1
          ELSE IF (IHIT.GT.1) THEN
            call bug('f', 'Improper permutation in LISTA')
          ENDIF
12      CONTINUE
        IF (KK.NE.(MA+1)) call bug('f', 'Improper permutation in LISTA')
        ALAMDA=0.001
        CALL MRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NCA,CHISQ,
     *              FUNCS)
        OCHISQ=CHISQ
        DO 13 J=1,MA
          ATRY(J)=A(J)
13      CONTINUE
      ENDIF
      DO 15 J=1,MFIT
        DO 14 K=1,MFIT
          COVAR(J,K)=ALPHA(J,K)
14      CONTINUE
        COVAR(J,J)=ALPHA(J,J)*(1.0+ALAMDA)
        DA(J)=BETA(J)
15    CONTINUE
      CALL GAUSSJ(COVAR,MFIT,NCA,DA,1,1)
      IF(ALAMDA.EQ.0.0) THEN
        CALL COVSRT(COVAR,NCA,MA,LISTA,MFIT)
        RETURN
      ENDIF
      DO 16 J=1,MFIT
        ATRY(LISTA(J))=A(LISTA(J))+DA(J)
16    CONTINUE
      CALL MRQCOF(X,Y,SIG,NDATA,ATRY,MA,LISTA,MFIT,COVAR,DA,NCA,CHISQ,
     *            FUNCS)
      IF(CHISQ.LT.OCHISQ) THEN
        ALAMDA=0.1*ALAMDA
        OCHISQ=CHISQ
        DO 18 J=1,MFIT
          DO 17 K=1,MFIT
            ALPHA(J,K)=COVAR(J,K)
17        CONTINUE
          BETA(J)=DA(J)
          A(LISTA(J))=ATRY(LISTA(J))
18      CONTINUE
      ELSE
        ALAMDA=10.0*ALAMDA
        CHISQ=OCHISQ
      ENDIF
      RETURN

      END

c***********************************************************************

      SUBROUTINE MRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NALP,
     *  CHISQ,FUNCS)

      integer mmax
      PARAMETER (MMAX=30)
      integer ndata,ma,lista,mfit,nalp
      real x,y,sig,alpha,beta,dyda,a,chisq
      external funcs
      integer i,j,k
      real sig2i,dy,ymod,wt
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),ALPHA(NALP,NALP),BETA(MA),
     *  DYDA(MMAX),LISTA(MFIT),A(MA)
c-----------------------------------------------------------------------
      DO 12 J=1,MFIT
        DO 11 K=1,J
          ALPHA(J,K)=0.0
11      CONTINUE
        BETA(J)=0.0
12    CONTINUE
      CHISQ=0.0
      DO 15 I=1,NDATA
        CALL FUNCS(X(I),A,YMOD,DYDA,MA)
        SIG2I=1.0/(SIG(I)*SIG(I))
        DY=Y(I)-YMOD
        DO 14 J=1,MFIT
          WT=DYDA(LISTA(J))*SIG2I
          DO 13 K=1,J
            ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(LISTA(K))
13        CONTINUE
          BETA(J)=BETA(J)+DY*WT
14      CONTINUE
        CHISQ=CHISQ+DY*DY*SIG2I
15    CONTINUE
      DO 17 J=2,MFIT
        DO 16 K=1,J-1
          ALPHA(K,J)=ALPHA(J,K)
16      CONTINUE
17    CONTINUE
      RETURN

      END

c***********************************************************************

      SUBROUTINE COVSRT(COVAR,NCVM,MA,LISTA,MFIT)

      integer ncvm,ma,lista,mfit,i,j
      real covar,swap
      DIMENSION COVAR(NCVM,NCVM),LISTA(MFIT)
c-----------------------------------------------------------------------
      DO 12 J=1,MA-1
        DO 11 I=J+1,MA
          COVAR(I,J)=0.0
11      CONTINUE
12    CONTINUE

      DO 14 I=1,MFIT-1
        DO 13 J=I+1,MFIT
          IF(LISTA(J).GT.LISTA(I)) THEN
            COVAR(LISTA(J),LISTA(I))=COVAR(I,J)
          ELSE
            COVAR(LISTA(I),LISTA(J))=COVAR(I,J)
          ENDIF
13      CONTINUE
14    CONTINUE

      SWAP=COVAR(1,1)
      DO 15 J=1,MA
        COVAR(1,J)=COVAR(J,J)
        COVAR(J,J)=0.0
15    CONTINUE

      COVAR(LISTA(1),LISTA(1))=SWAP
      DO 16 J=2,MFIT
        COVAR(LISTA(J),LISTA(J))=COVAR(1,J)
16    CONTINUE

      DO 18 J=2,MA
        DO 17 I=1,J-1
          COVAR(I,J)=COVAR(J,I)
17      CONTINUE
18    CONTINUE
      RETURN

      END

c***********************************************************************

      subroutine gauplot(nc,vlsr,t,spec)

      integer   nc, spec
      real      t(nc), vlsr(nc)
c-----------------------------------------------------------------------
c Inputs:
c  nc      number of channels
c  vlsr    velocity array
c  t       spectrum
c  spec    spectrum index
c  ngauss  number of simultaneously fitted  gaussians
c  gauss   gaussian parameters
c           gauss(1,i) amplitude of gaussian i
c           gauss(2,i) center velocity of gaussian i
c           gauss(3,i) sigma of gaussian i
c-----------------------------------------------------------------------
      include 'velplot.h'

      integer   i, j, ngauss(49), nspec
      real      arg1, dv, gauss(49,3,10), gausserr(49,3,10), term,
     *          tmod(MAXDIM), tmod2(5*MAXDIM), tres(MAXDIM),
     *          vlsr2(5*MAXDIM), xc(49), yc(49)

      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c-----------------------------------------------------------------------
c
c make sure there are gaussian fits to compute
c
      if (ngauss(spec).eq.0) return
c form gaussian model spectrum
      do i = 1, nc
        tmod(i) = 0.0
        do j = 1, ngauss(spec)
          arg1=(vlsr(i)-gauss(spec,2,j))/gauss(spec,3,j)
          if (abs(arg1).gt.4.0) then
            term=0.0
          else
            term=gauss(spec,1,j)*exp(-arg1*arg1)
          endif
          tmod(i) = tmod(i) + term
        enddo
      enddo
c
c form residual spectrum
      do i = 1, nc
        tres(i)=t(i)-tmod(i)
      enddo
c
c form new gaussian model spectrum with factor of 5 oversampling
c
      dv=(vlsr(2)-vlsr(1))/5.0
      do i = 1, nc*5
        vlsr2(i)=vlsr(1)+(real(i)*dv)
        tmod2(i) = 0.0
        do j = 1, ngauss(spec)
          arg1=(vlsr2(i)-gauss(spec,2,j))/gauss(spec,3,j)
          if (abs(arg1).gt.4.0) then
            term=0.0
          else
            term=gauss(spec,1,j)*exp(-arg1*arg1)
          endif
          tmod2(i) = tmod2(i) + term
        enddo
      enddo

c plot model fit and residual spectrum
      call pgsls(4)
      call pgline(nc*5,vlsr2,tmod2)
c      call pgsls(2)
c      call pgHline(nc,vlsr,tres,2.)
      call pgsls(1)
      return

      end

c***********************************************************************

      subroutine getList(imaps,vmin,vmax,vlsr,nc)

      integer imaps,nc
      real    vmin(*),vmax(*),vlsr(nc)
c-----------------------------------------------------------------------
c       Get list of velocity intervals.
c
c  Inputs:
c    vlsr(nc)   Array of velocities.
c    nc         Number of channels in image.
c  Outputs:
c    imaps      Number of velocity intervals.
c    vmin,vmax  Array of velocity intervals.
c-----------------------------------------------------------------------
      real velmin,velmax,swap
      integer i,k,length,k1,k2,tlen
      character line*80,string*80
      double precision dval
      logical ok
c-----------------------------------------------------------------------
      goto 9
20    continue
      call output(' ')
      call output('Try again; map number must be in list,' //
     *            'or increment list by one')
      call listMaps(imaps,vmin,vmax,vlsr,nc)
9     call output(' ')
      call output('---- Enter list of velocity intervals ---')
      call output('>Type -n to delete map n')
      call output('      -99 to delete all')
      call output('       L to list')
      call output('      <cr> to use the current list')
10    call prompt(line,length,
     *  '>Enter map number and velocity interval (N,Vmin,Vmax): ')
      if (length.ne.0) then
        k1 = 1
        k2 = length
        k  = 0
        do while (k1.le.k2 .and. k.lt.3)
          call getfield(line, k1, k2, string, tlen)
          call atodf(string,dval,ok)
          if (ok) then
            k = k+1
            if (k.eq.1) i = dval
            if (k.eq.2) velmin = dval
            if (k.eq.3) velmax = dval
          else
            goto 20
          endif
        enddo
      else
        return
      endif

c  end of list
      if (i.eq.0) goto 50
c  swap
      if (velmin.gt.velmax) then
        swap = velmin
        velmin = velmax
        velmax = swap
      endif
c  add or change map
      if (i.ge.1 .and. i.le.min(imaps+1,25)) then
        vmin(i) = velmin
        vmax(i) = velmax
        imaps = max(i,imaps)
c  delete map
      else if (-i.ge.1 .and. -i.le.imaps) then
        k = -i
        do i = k, imaps-1
          vmin(i) = vmin(i+1)
          vmax(i) = vmax(i+1)
        enddo
        imaps = imaps-1
c  start new list
      else if (i.le.-99) then
        call output('All maps deleted from list')
        imaps = 0
      else
        goto 20
      endif
      goto 10
50    return

      end

c***********************************************************************

      subroutine getMom(nmom)

      integer nmom
c-----------------------------------------------------------------------
c  Get moment to be plotted for velomap
c
c  Outputs:
c    nmom       Moment of map to be computed.
c-----------------------------------------------------------------------
      include 'velplot.h'

      real value
c-----------------------------------------------------------------------
 10   call output(' ')
      call output(' Map computed: ')
      call output('   0: straight average [default]')
      call output('   1: velocity centroid from 1st moment')
      call output('  -1: velocity of peak channel')
      call output('   2: velocity width or variance from 2nd moment')
      call output('   3: degree of skewness or asymmetry')
      call output('   4: degree of excess, or "peakiness"')
      call output('>Enter moment to compute (-1 to 4; default [0]: ')
      read(*, '(I5)', err=10) nmom
      if ((nmom.lt.-1) .or. (nmom.gt.4)) then
        call output('moment must be integer in range (-1 to 4)')
        goto 10
      endif

      call promptf(value, 'f12.5', '>Enter blanking cutoff level: ',
     *             cutoff)
      cutoff = value

      if (nmom.ne.0) then
        units = ' '
        call setCont
      endif

      end

c***********************************************************************

      subroutine getRange(imaps,vmin,vmax,vlsr,nc)

      integer imaps,nc
      real vmin(*),vmax(*),vlsr(nc)
c-----------------------------------------------------------------------
c  Get range of velocities to plot.
c
c  Inputs:
c    vlsr(nc)   Array of velocities.
c    nc         Number of channels in image.
c  Outputs:
c    imaps      Number of velocity intervals.
c    vmin,vmax  Array of velocity intervals.
c-----------------------------------------------------------------------
      real vfirst,vlast,vwidth,vinc,vstart,vend
c-----------------------------------------------------------------------
      vfirst=0.0
      vlast=0.0
      vinc=0.0
      vstart=0.0
      vend=0.0
      vwidth=0.0

      call output(' ')
      call output(
     *    '>Enter first and last velocity, width and increment: ')
      read(*,*) vfirst,vlast,vwidth,vinc
c
c  Find min and maximum velocity.
c
      vstart=min(vlast,vfirst)
      vend=max(vlast,vfirst)
      if (vinc.eq.0.0) vinc = vlsr(2)-vlsr(1)
      if (vstart.eq.0.0 .and. vend.eq.0.0) then
        vstart = vlsr(1)
        vend = vlsr(nc)
      endif
c
c  Give VINC correct sign to go from VSTART to VEND
c
      vinc=sign(vinc,(vend-vstart))
      vmin(1) = vstart - abs(vwidth)/2.0
      vmax(1) = vstart + abs(vwidth)/2.0
      imaps=1
      if (vinc.gt.0.0) then
        do while (vmax(imaps).lt.max(vstart, vend))
          imaps=imaps+1
          vmin(imaps)=vmin(imaps-1)+vinc
          vmax(imaps)=vmax(imaps-1)+vinc
        enddo
      else
        do while (vmin(imaps).gt.min(vstart, vend))
          imaps=imaps+1
          vmin(imaps)=vmin(imaps-1)+vinc
          vmax(imaps)=vmax(imaps-1)+vinc
        enddo
      endif

      end

c***********************************************************************

      subroutine header(ipr)

      integer ipr
c-----------------------------------------------------------------------
c  convert units and write out map header if ipr .ne. 0.
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mirconst.h'

      real CKMS
      parameter (CKMS = CMKS*1e-3)

      real freqs
      real omega,pixel
      character line*120
      character*13  ras, decs

c     External.
      integer len1
      character hangleh*13, rangleh*13
c-----------------------------------------------------------------------
c
c --- convert units for maps ----
c
      ras  = hangleh(dble(crval(1)))
      decs = rangleh(dble(crval(2)))
      cbof = 1.0
      if (bmaj*bmin.ne.0.0) then
        pixel = xy*AS2R
        if (bmaj.gt.pixel) then
          omega = PI * bmaj*bmin /(4.0*log(2.0))
          cbof  = omega / (pixel*pixel)
        else
          omega = pixel*pixel
        endif
      endif
      dperjy = 1.0
      freqs = restfreq*(1.0-vel/CKMS)
      if (freqs.ne.0.0) then
        dperjy = (0.3/freqs)**2 / (2.0*1.38e3*omega)
      endif
c
c --- write out map header information ---
c
      if (ipr.eq.0) then
         return
      else if (ipr.eq.5) then
        write(line,100) object,ras,decs,
     *    ' cell:',xy,' vel:',vel,' delv:',delv
        call output(line(1:len1(line)))
        write(line,101) ctype,posx,posy,pospa,restfreq,bunit
        call output(line(1:len1(line)))
        write(line,102) bmaj*R2AS, bmin*R2AS, bpa, niters,dperjy,cbof
        call output(line(1:len1(line)))
      else if (ipr.eq.6) then
        write(line,100) object,ras,decs,
     *    ' cell:',xy,' vel:',vel,' delv:',delv
        call LogWrit(line(1:len1(line)))
        write(line,101) ctype,posx,posy,pospa,restfreq,bunit
        call LogWrit(line(1:len1(line)))
        write(line,102) bmaj*R2AS, bmin*R2AS, bpa, niters,dperjy,cbof
        call LogWrit(line(1:len1(line)))
      endif

100   format(a,1x,a,1x,a,a,f8.3,a,f8.2,a,f8.2)
101   format(a,a,a,3f6.2,' freq:',f9.5,' unit:',a)
102   format('beam:',3f6.1,' niters:',i7,' K/Jy:',f9.2,' cbof:',f7.2)

      end

c***********************************************************************

      subroutine intannot(nchan,ichan,vlsr,nc)

      integer nchan,ichan(10),nc
      real vlsr(nc),yloc
c-----------------------------------------------------------------------
c  Annotate plots.
c-----------------------------------------------------------------------
      include 'velplot.h'
      character line*80
      integer i,j
c-----------------------------------------------------------------------
c
c  Set pg viewport to right side.
c
      call pgswin(0.0,1.0,0.0,1.0)

      write(line,'(''Velocities (km/s): '')')
      call pgtext(0.0,0.30,line)
c  levels
      j=0
      yloc=26.0
      do while (j.lt.nchan .and. yloc.gt.2.0)
        write(line,'(2(1pg10.3,x))')
     *    (vlsr(ichan(j+i)),i=1,min(2,nchan-j))
        call pgtext(0.0,yloc/100,line)
        j=j+2
        yloc=yloc-4.0
      enddo

      write(line,'(''Line Styles:  (1) ____'')')
      call pgtext(0.0,0.14,line)
      write(line,'(''  (2) _ _ _   (3) . _ . _ .'')')
      call pgtext(0.0,0.10,line)
      write(line,'(''  (4) . . . . .   (5) _ . . . _'')')
      call pgtext(0.0,0.06,line)
c
c  Don't forget to Restore original plot window.
c
      end

c***********************************************************************

      subroutine integral(ary,vlsr,nx,ny,nc)

      integer nx,ny,nc
      real    ary(nx,ny,nc),vlsr(nc)
c-----------------------------------------------------------------------
c  List integral and statistics within box.
c
c  Inputs:
c    ary        The image.
c    nx,ny,nc   Dimensions of image.
c    vlsr       Array of velocities.
c-----------------------------------------------------------------------
      include 'velplot.h'

      integer   imax, imin, ipr, jmax, jmin, k, num
      real      ave, tmax, tmin
      character ans, line*80
c-----------------------------------------------------------------------
      call header(0)
      write(line,'(a,4i6,a)') 'Integral and rms in box (',
     *  bblc(1)-brpix(1), bblc(2)-brpix(2),
     *  btrc(1)-brpix(1), btrc(2)-brpix(2), ')'
      call output(line)
      write(line,'(a,a,a,a,a,a,a)') ' plane  ','  Velocity  ',
     *  ' Total Flux ','  Maximum   ','  Minimum   ','  Average   ',
     *  '    rms     '
      call output(line)
      do k = 1, nc
        call maxmap(ary(1,1,k),nx,ny,bblc(1),btrc(1),bblc(2),btrc(2),
     *                tmax,imax,jmax,tmin,imin,jmin,ave,arms,num)
        write(line,'(1x,i4,1x,6(1x,f11.4))')
     *        k,vlsr(k),ave*num/cbof,tmax,tmin,ave,arms
        call output(line)
      enddo
c
c  write into log file.
c
c       call prompt(line,k,'>Type <cr> to continue')
      call prompt(ans,ipr,'Write into log ? (Y/[N]) :')
      call ucase(ans)
      if (ans.eq.'Y') then
        call header(0)
        write(line,'(a,4i6,a)') 'Integral and rms in box (',
     *    bblc(1)-brpix(1),bblc(2)-brpix(2),
     *    btrc(1)-brpix(1),btrc(2)-brpix(2),')'
        call LogWrit(line)
        write(line,'(a,a,a,a,a,a,a)') ' plane  ','  Velocity  ',
     *  ' Total Flux ','  Maximum   ','  Minimum   ','  Average   ',
     *  '    rms     '
        call LogWrit(line)
        do k = 1, nc
          call maxmap(ary(1,1,k),nx,ny,bblc(1),btrc(1),bblc(2),btrc(2),
     *                tmax,imax,jmax,tmin,imin,jmin,ave,arms,num)
          write(line,'(1x,i4,1x,6(1x,f11.4))')
     *        k,vlsr(k),ave*num/cbof,tmax,tmin,ave,arms
          call LogWrit(line)
        enddo
      endif

      end

c***********************************************************************

      subroutine integrate(ary,nx,ny)

      integer nx,ny
      real ary(nx,ny)
c-----------------------------------------------------------------------
c  Find integral,max,min,rms in box.
c
c  Inputs:
c    ary        The array.
c    nx,ny      Dimensions of array.
c-----------------------------------------------------------------------
      include 'velplot.h'
      character line*80
      real tmax,tmin,ave
      integer imin,jmin,imax,jmax,num
c-----------------------------------------------------------------------
      write(line,'(a,4i6,a)') 'Integral and rms in box (',
     *  bblc(1)-brpix(1), bblc(2)-brpix(2),
     *  btrc(1)-brpix(1), btrc(2)-brpix(2), ')'
      call output(line)
      call LogWrit(line)
      write(line,'(a,a,a,a,a,a)') '  Velocity  ',
     *  ' Total Flux ','  Maximum   ','  Minimum   ','  Average   ',
     *  '    rms     '
      call output(line)
      call LogWrit(line)
      call maxmap(ary,nx,ny,bblc(1),btrc(1),bblc(2),btrc(2),
     *                tmax,imax,jmax,tmin,imin,jmin,ave,arms,num)
      call header(0)
      write(line,'(6(x,f11.4),a)')
     *        vel, ave*num/cbof, tmax, tmin, ave, arms, ' Jy'
      call output(line)
      call LogWrit(line)
      write(line,'(24x,4(x,f11.4),a)')
     *        tmax*dperjy, tmin*dperjy, ave*dperjy, arms*dperjy, ' K'
      call output(line)
      call LogWrit(line)

      end

c***********************************************************************

      subroutine listMaps(imaps,vmin,vmax,vlsr,nc)

      integer imaps,nc
      real vmin(*),vmax(*),vlsr(nc)
c-----------------------------------------------------------------------
c  List the maps available and current selection
c
c  Inputs:
c    nc         Number of channels in image.
c    vlsr(nc)   Array of values.
c  Outputs:
c    imaps      Number of intervals.
c    vmin,vmax  Min and max values.
c-----------------------------------------------------------------------
      integer   i
      character msg*80
c-----------------------------------------------------------------------
c
c --- maps available ---
c
      call output(' ')
      write(msg,'(a,f12.3,a,f12.3)') ' First channel:',
     *                vlsr(1), '   Last channel:',vlsr(nc)
      call output(msg)
      write(msg,'(a,f12.3)') ' Increment :',vlsr(2)-vlsr(1)
      call output(msg)
      call output(' ')
c
c --- current selection of maps ---
c
      if (imaps.ne.0) then
        call output(' --- current selection of maps ---')
        do i = 1, imaps
          write(msg, 20) i, vmin(i), vmax(i)
 20       format(' map(',i2,') velocity range: (',f12.3,' to',f12.3,')')
          call output(msg)
        enddo
      else
        call output(' --- no current selection of maps ---')
      endif

      end

c***********************************************************************

      subroutine maxmap(ary,nx,ny,iblc,itrc,jblc,jtrc,
     *        amax,imax,jmax,amin,imin,jmin,ave,rms,num)

      integer nx,ny,iblc,itrc,jblc,jtrc,imax,jmax,imin,jmin,num
      real ary(nx,ny),amax,amin,ave,rms
c-----------------------------------------------------------------------
c  Find max, min and rms in specified region of real array.
c                       MCHW Jan 1986
c    pixel blanking: ignore values .lt. -99999.  wh feb 87
c
c  Inputs:
c    ary        array of values.
c    nx,ny      dimensions of ary
c    iblc,...   Region to search.
c
c  Outputs:
c    amax,amin  maximum and minimum in specified region.
c    imax,jmax  position of maximum.
c    imin,jmin  position of minimum.
c    ave,rms    average and rms in specified region.
c    num        number of pixels greater than -99999
c-----------------------------------------------------------------------
      integer i,j
      real a,sum,sumsq
c-----------------------------------------------------------------------
      amax = -1e9
      amin = 1e9
      sum = 0.0
      sumsq = 0.0
      num = 0
      do j = jblc, jtrc
        do i = iblc, itrc
          a = ary(i,j)
          if (abs(a+99999.0).gt.0.5) then
            sum = sum + a
            sumsq = sumsq + a*a
            num = num + 1
            if (a.gt.amax) then
              amax = a
              imax = i
              jmax = j
            else if (a.lt.amin) then
              amin = a
              imin = i
              jmin = j
            endif
          endif
        enddo
      enddo

      if (num.gt.0) then
        ave = sum/num
        rms = sqrt(sumsq/num) - ave*ave
      endif

      end

c***********************************************************************

      subroutine moment1(nc,vlsr,buf,clip,amp,mom1,mom2)

      integer nc
      real vlsr(nc),buf(nc),amp,mom1,mom2,clip
c-----------------------------------------------------------------------
c  Calculate moments.
c
c  Input:
c    nc         number of channels
c    vlsr       velocities
c    buf        amplitudes
c    clip       cutoff level in moment calculation
c  Output:
c    amp        maximum amplitude
c    mom1       1st moment
c    mom2       2nd moment
c-----------------------------------------------------------------------
      integer i
      real mom0
c-----------------------------------------------------------------------
      amp = 0.0
      mom0 = 0.0
      mom1 = 0.0
      mom2 = 0.0
c
c  Find the maximum and accumulate the moments.
c
      do i = 1, nc
        if (buf(i).gt.clip) then
          if (buf(i).gt.amp) amp=buf(i)
          mom0 = mom0 + buf(i)
          mom1 = mom1 + buf(i)*vlsr(i)
          mom2 = mom2 + buf(i)*vlsr(i)*vlsr(i)
        endif
      enddo
c
c  Normalize and scale the moments.
c
      if (mom0.ne.0.0) then
        mom1 =mom1/mom0
        mom2 = mom2/mom0 - mom1*mom1
        if (mom2.gt.0) then
          mom2 = sqrt(mom2)
        else
          mom2 = 0.0
        endif
      endif

      end

c***********************************************************************

      subroutine plAnnot(cf,cmaj,cmin,cpa)

      real cf,cmaj,cmin,cpa
c-----------------------------------------------------------------------
c  Annotate plots.
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mirconst.h'

      real yloc
      character line*80
      character*13  ras, decs
      integer i,j,len1
      real scale,absmax

c     External.
      character*13  hangleh, rangleh
c-----------------------------------------------------------------------
c
c  Set pg viewport to right side.
c
      call pgswin(0.0,1.0,0.0,1.0)
c  object
      call pgtext(0.0,0.95,object)
c  ra and dec
      ras  = hangleh(dble(crval(1)))
      decs = rangleh(dble(crval(2)))
      write(line,'(a,a)') ras, decs
      call pgtext(0.0,0.9,line)
c  epoch
      write(line,'(a,f5.0)') 'epoch: ', epoch
      call pgtext(0.0,0.86,line)
c  pixel
      write(line,'(f9.2,''  x'',f9.2)') xy, xy
      call pgtext(0.0,0.82,line)
c  freq
      write(line,'(''RestFreq: '',F10.5,'' GHz'')') restfreq
      call pgtext(0.0,0.66,line)
c  vel
      write(line,'(''Velocity: '',F10.3,'' km/s'')') vel
      call pgtext(0.0,0.62,line)
c  delv
      write(line,'(''Width:    '',F10.3,'' km/s'')') abs(delv)
      call pgtext(0.0,0.58,line)
c  file
      write(line,'(''filename:'',1x,A)') file(1:len1(file))
      call pgtext(0.0,0.54,line)
c  beam
      if (bmaj.gt.0.0) then
        write(line,'(''Beam'',1x,f6.2,1x,f6.2,1x,f6.1)')
     *        bmaj*R2AS,bmin*R2AS,bpa
        call pgtext(0.0,0.50,line)
      endif
c  convolving beam
      if (cmaj.gt.0.0) then
        write(line,'(''Conv'',1x,f6.2,1x,f6.2,1x,f6.1)')
     *        cmaj,cmin,cpa
        call pgtext(0.0,0.46,line)
      endif
c  bunit
      write(line,'(''Map Unit:'',1x,a)') bunit
      call pgtext(0.0,0.40,line)
c  dperjy
      write(line,'(''K/Jy ='',1pg10.3)') dperjy
      call pgtext(0.0,0.36,line)
c
c  convert units for caption using cf.
c
      if (maptype.eq.'SPECTRA') then
       goto 202
      endif
c  max
      write(line,'(''Maximum:'',1pg10.3,1X,A)') amax*cf,units
      call pgtext(0.0,0.30,line)
c  min
      write(line,'(''Minimum:'',1pg10.3,1x,a)') amin*cf,units
      call pgtext(0.0,0.26,line)
c  rms
      write(line,'(''Rms:    '',1pg10.3,1x,a)') arms*cf,units
      call pgtext(0.0,0.22,line)
c  contours
      scale=1.0
      if (percent.eq.'Y') then
        absmax=abs(amax)
        if (cneg.eq.'Y') absmax=(max(abs(amax),abs(amin)))
        scale=absmax*cf/100.0
      endif
      write(line,'(''Contour Levels:'',1x,A)') units
      call pgtext(0.0,0.18,line)
c  levels
      j=0
      yloc=14.0
      do while (j.lt.nlevels .and. yloc.gt.2.0)
        write(line,'(3(1pg10.3,x))')
     *    (levels(j+i)*scale,i=1,min(3,nlevels-j))
        call pgtext(0.0,yloc/100,line)
        j=j+3
        yloc=yloc-4.0
      enddo
c
c  Don't forget to Restore original plot window.
c
202   continue

      end

c***********************************************************************

      subroutine plLimits(iopt,nx,ny,xmin,xmax,ymin,ymax)

      integer iopt,nx,ny
      real xmin,xmax,ymin,ymax
c-----------------------------------------------------------------------
c  Find the plot limits.
c
c  Inputs:
c    iopt=0 for relative coords; iopt=1 for absolute coords
c    nx,ny      Size of array.
c    xy,crval1,crval2 come from the map header
c  Outputs:
c    xmin,xmax,ymin,ymax        plot limits
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mirconst.h'

      real      decsec, rasec
      character msg*80
c-----------------------------------------------------------------------
      if (iopt.eq.0) then
c       Relative coords.
        xmin = -xy*(1 -brpix(1))
        xmax = -xy*(nx-brpix(1))
        ymin =  xy*(1 -brpix(2))
        ymax =  xy*(ny-brpix(2))

      else if (iopt.eq.1) then
c       Absolute coords.
        rasec  = crval(1) * R2AS/15.0
        decsec = crval(2) * R2AS
        xmin = rasec  - xy*(1 -brpix(1))/cos(crval(2))/15.0
        xmax = rasec  - xy*(nx-brpix(1))/cos(crval(2))/15.0
        ymin = decsec + xy*(1 -brpix(2))
        ymax = decsec + xy*(ny-brpix(2))
        write(msg, *) cos(crval(2)),cos(crval(2))/15.0,rasec,decsec
        call output(msg)
      endif

      end

c***********************************************************************

      subroutine plOpts
c-----------------------------------------------------------------------
c  Set plotting parameters
c-----------------------------------------------------------------------
      include 'velplot.h'

      integer   i, length
      character ans*80, line*80, tline*9
c-----------------------------------------------------------------------
c
c display current defaults
c
90    call output(' ')
      call output('   OPTIONS MENU')

c***********************************************************************
       call output('Select alternative options [1/2] for the following')
      call output(' ')
c units
      write(line,'(''Units for display [J/K].....'',1x,A)') units
      call output(line)
c negative contours
      write(line,'(''Negative contours [Y/N].....'',1x,A)') cneg
      call output(line)
c labels
      write(line,'(''Plot labels [Y/N]...........'',1x,A)') alabel
      call output(line)
c Write map to file
      write(line,'(''Write map to file [Y/N].....'',1x,A)') write
      call output(line)
c Absolute coords
      write(line,'(''Absolute coordinates [Y/N]..'',1x,A)') abscoord
      call output(line)
c Integer plot
      write(line,'(''Integer plot [Y/N]..........'',1x,A)') apint
      call output(line)
c Spectra Positions
      write(line,'(''Spectra positions [Y/N].....'',1x,A)') pspec
      call output(line)
c Gaussian Fits
      write(line,'(''Fit Gaussians [Y/N].........'',1x,A)') lgaufit
      call output(line)
c Plot Gaussian Fits
      write(line,'(''Overlay Gauss Fits [Y/N]....'',1x,A)') lgauplot
      call output(line)
c Gray Scale
      write(line,'(''Gray Scale [Y/N]............'',1x,A)') gray
      call output(line)
c Exit
      call output('Exit from options menu')
c Contour levels
      if (percent.eq.'Y') then
        tline='percent'
      else
        tline='absolute'
      endif
      write(line,'(''Contour levels: '',1x,a)') tline
      call output(line)
      do i = 1, nlevels
        write(line, 109) i,levels(i)
        call output(line)
      enddo
109   format(' level(',i2,') = ',f9.3)
c
c ask for input
c
199   call output(' ')
      call prompt(ans,length,
     *  'Select option (type 1st character, <cr> to list) :')
      call ucase(ans)
      if (ans(1:1).eq.'U') then
        if (units.eq.'J') then
          units='K'
          call output('-plot units are now Kelvin')
        else if (units.eq.'K') then
            units='J'
            call output('-plot units are now Janskys')
        else if (units.ne.'J' .and. units.ne.'K') then
            call output('-unrecognized map units')
        endif
      else if (ans(1:1).eq.'N') then
        if (cneg.eq.'Y') then
          cneg='N'
          call output('-negative contours will not be displayed')
        else
          cneg='Y'
          call output('-negative contours will be displayed')
        endif
      else if (ans(1:1).eq.'P') then
        if (alabel.eq.'Y') then
          alabel='N'
          call output('-plot will not be annotated')
        else
          alabel='Y'
          call output('-plot will be annotated')
        endif
      else if (ans(1:1).eq.'W') then
        if (write.eq.'Y') then
          write='N'
          call output('-will not write MIRIAD image to disk')
        else
          write='Y'
          call output('-will write MIRIAD image to disk')
        endif
      else if (ans(1:1).eq.'A') then
        if (abscoord.eq.'Y') then
          abscoord='N'
        else
          abscoord='N'
        endif
        call output('Sorry...option not yet implemented.')
      else if (ans(1:1).eq.'I') then
        if (apint.eq.'Y') then
          apint='N'
          call output('-plot will not be integer plot')
        else
          apint='Y'
          call output('-plot will be integer plot')
        endif
      else if (ans(1:1).eq.'S') then
        if (pspec.eq.'Y') then
          pspec='N'
          call output('-plot will not indicate spectra positions')
        else
          pspec='Y'
          call output('-plot will indicate spectra positions')
        endif
      else if (ans(1:1).eq.'F') then
        if (lgaufit.eq.'Y') then
          lgaufit='N'
          call output('-will not fit gaussians to spectra')
        else
          lgaufit='Y'
          call output('-will fit gaussians to spectra')
        endif
      else if (ans(1:1).eq.'O') then
        if (lgauplot.eq.'Y') then
          lgauplot='N'
          call output('-will not overlay gaussian fits to spectra')
        else
          lgauplot='Y'
          call output('-will overlay gaussian fits to spectra')
        endif
      else if (ans(1:1).eq.'G') then
        if (gray.eq.'Y') then
          gray='N'
          call output('-plot will not include gray scale')
        else
          gray='Y'
          call output('-plot will include gray scale')
          call setGray
        endif
      else if (ans(1:1).eq.'C') then
        call setCont
      else if (ans(1:1).eq.' ') then
        goto 90
      else if (ans(1:1).eq.'E') then
        goto 200
      endif
      goto 199
200   continue

      end

c***********************************************************************

      subroutine plotcon(ary,nx,ny,cf,tr)

      integer nx,ny
      real ary(nx,ny),cf,tr(6)
c-----------------------------------------------------------------------
c  Contour plots.
c  Gray scale.
c                       MCHW Dec 1985
c       cf is the conversion factor from map units - mchw 7Feb86
c-----------------------------------------------------------------------
      include 'velplot.h'
      integer i,imin,imax,jmin,jmax,num,loop,nloop
      real clevels(10),ave,scale,absmax
      real levmax,levmin
      character line*80

c     External.
      integer ismax,ismin
c-----------------------------------------------------------------------
c
c  Set scale for contour levels. (amin,amax,arms are used in plAnnot)
c
      call maxmap(ary,nx,ny,1,nx,1,ny,amax,imax,jmax,amin,imin,jmin,ave,
     *            arms,num)
      if (cf.eq.0.0) cf=1.0
      scale=1.0/cf
      if (percent.eq.'Y') then
        absmax=abs(amax)
        if (cneg.eq.'Y') absmax=(max(abs(amax),abs(amin)))
        scale=absmax/100.0
      endif
c
c  Plot grayscale if required
c
      if (gray.eq.'Y') then
        if (defgray.eq.'Y') then
          fg=amax
          bg=amin
        endif
        call pggray(ary,nx,ny,1,nx,1,ny,fg,bg,tr)
      endif
c
c  Set linetype and plot contours
c
      call pgsls(1)
      nloop=1
      if (cneg.eq.'Y') nloop=2

      do loop = 1, nloop
        do i = 1, nlevels
          clevels(i) = levels(i)*scale
        enddo
        write(line,'(a,f9.3,a,f9.3)')
     *                'minimum: ', amin, ' maximum: ', amax
        call output(line)
        levmax = clevels(ismax(nlevels,clevels,1))
        levmin = clevels(ismin(nlevels,clevels,1))
        write(line,'(a,f9.3,a,f9.3)')
     *                '   contours from ', levmin, ' to ', levmax
        call output(line)
        call pgcont(ary,nx,ny,1,nx,1,ny,clevels,nlevels,tr)
        if (cneg.eq.'Y') then
          call pgsls(2)
          scale=-scale
        endif
      enddo
      call pgsls(1)

      end

c***********************************************************************

      subroutine plotint(ary,nx,ny)

      integer nx,ny
      real ary(nx,ny)
c-----------------------------------------------------------------------
c  Integer plot.
c
c  Inputs:
c    ary        The array to print.
c    nx,ny      Dimensions of array.
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mirconst.h'

      integer line(40)
      character text*80
      real tmax,tmin,ave,scale
      integer imin,jmin,imax,jmax,num,i,j,iend
c-----------------------------------------------------------------------
c
c  Title.
c
      call output('Image: '//file)
      call output('source: '//object//'  map units: '//bunit)
      write(text,'('' summary for box '',4i5)')
     *  1-brpix(1), nx-brpix(1), 1-brpix(2), ny-brpix(2)
      call output(text)
c
c  Find max,min,rms in selected region.
c
      call maxmap(ary,nx,ny,1,nx,1,ny,tmax,imax,jmax,tmin,imin,jmin,ave,
     *            arms,num)
      call header(0)
      write(text,'(''Maximum='',1pg10.3,''at '',2i5)')
     *        tmax, imax-brpix(1), jmax-brpix(2)
      call output(text)
      write(text,'(''Minimum='',1pg10.3,''at '',2i5)')
     *        tmin, imin-brpix(1), jmin-brpix(2)
      call output(text)
      write(text,'(''Average='',1pg10.3,''  rms='',1pg10.3)') ave,arms
      call output(text)
      write(text, *) 'Beam ',bmaj*R2AS,bmin*R2AS,bpa, '  cbof=',cbof
      call output(text)
      if (cbof.ne.0.0) then
        write(text, *) 'Total flux in box =',ave*num/cbof
        call output(text)
        write(text, *) 'Brightness max, min and rms are',
     *         tmax*dperjy,tmin*dperjy,arms*dperjy
        call output(text)
      endif
c
c  Convert plot to integers.
c
      scale = 100.0/max(abs(tmax),abs(tmin))
      write(text, *) 'units are: ',1.0/scale,' ',bunit
      call output(text)
      iend = min(nx,24)
      write(text, 103) -brpix(2),(i-brpix(1),i=1,iend)
      call output(text)
      do j = ny,1,-1
        do i = 1, iend
          if (ary(i,j).gt.-99999.0) then
            line(i)=scale*ary(i,j)
          else
            line(i)=0
          endif
        enddo
        write(text, 103) j-brpix(2),(line(i),i=1,iend)
        call output(text)
      enddo
      write(text, 103) -brpix(2),(i-brpix(1),i=1,iend)
      call output(text)
103   format(1x,i4,1x,24i3)

      end

c***********************************************************************

      subroutine posVel(ary,vlsr,nx,ny,nc,v)

      integer nx,ny,nc
      real ary(nx,ny,nc),vlsr(nc),v(*)
c-----------------------------------------------------------------------
c  Make position-velocity images and plots.
c
c  Inputs:
c    ary        The image.
c    nx,ny,nc   Dimensions of image.
c    vlsr       Array of velocities.
c-----------------------------------------------------------------------
      include 'velplot.h'
      integer np,k,l,ncon
      real xin,yin,pain,cmaj,cmin,cpa,cf
      real xstart,xend,vstart,vend,xmin,xmax,pstart,pend,pinc,pos
c  convolution array maximum size
      real con(99,99,4,4)
      real tr(6),ymax,ymin,xval(MAXDIM),xcoord(MAXDIM),xdelta
      character label*80,ans*1,olddev*80
      character*9 lab1,lab2,lab3
      character*1 lab4
      character*80 xlabel,ylabel,line
      integer i,windx,windy,length,lwidth
      integer ii,jj,jjj,nchan,ichan(10),ifix
      double precision dval(4)
      logical ok

      integer ncut
      real xcut(128),ycut(128),pa(128)
      common /cuts/ xcut,ycut,pa,ncut
c-----------------------------------------------------------------------
c
c  Introduction.
c
      call output(
     *'Plot intensity versus position & velocity along selected cuts')
      call prompt(ans,length,'>Type H for help, <cr> to continue :')
      call ucase(ans)
      if (ans.eq.'H') then
        call output(' ')
        call output(
     *   'Plot position-velocity maps, or intensity versus position.')
        call output('Cuts are selected with the cursor on an x-y map')
        call output('or can be entered as a list which can be edited')
        call output('RA-velocity or DEC-velocity maps can be plotted')
        call output('Position-velocity maps can also be printed, or')
        call output('written out as images for further processing.')
        call output('(x,y) positions are in (HA,DEC) directions.')
        call output('(Position angle is measured from N through E)')
        call output(' ')
      endif
c
c  List current selection of cuts.
c
      goto 9
20    call output(' ')
      call output('Cut number must edit or extend list by one')
9     if (ncut.ne.0) then
        call output('--- current selection of cuts ---')
        do i = 1, ncut
          write(line, 109) i,xcut(i),ycut(i),pa(i)
          call output(line)
        enddo
      else
        call output('--- no current selection of cuts ---')
      endif
109   format(' map(',i2,')  x= ',f9.3,' y= ',f9.3,' pa= ',f8.3)
c
c  Specify List of cuts, or RA-vel or DEC-velocity.
c
      call output(' ')
      call prompt(ans,length,
     *  '>List of cuts, RA-vel or DEC-vel ? ([L]/X/Y): ')
      call ucase(ans)
      if (ans.eq.' ') ans='L'
c
c  Enter list of position-velocity cuts across image.
c
      if (ans.eq.'L') then
        call output(' ')
        call output('--- Enter list of pos-vel cuts to plot ---')
        call output(
     *    'angles measured from north to the east (0 to 180).')
        call output('>Type -n to delete cut n')
        call output('      -99 to delete all')
        call output('      L to list')
        call output('     <cr> to use the current list.')
10      call prompt(line,length,
     *    '>Enter cut number, position and angle (n,x,y,pa): ')
        if (length.eq.0) goto 50
        call matodf(line,dval,4,ok)
        if (ok) then
          i    = dval(1)
          xin  = dval(2)
          yin  = dval(3)
          pain = dval(4)
        else
          goto 20
        endif
c
c  End of list. Make plots.
c
        if (i.eq.0) goto 50
c  Add or change map
        if (i.ge.1 .and. i.le.min(ncut+1,25)) then
          xcut(i) = xin
          ycut(i) = yin
          pa(i) = pain
          ncut = max(i,ncut)
c  Delete map
        else if (-i.ge.1 .and. -i.le.ncut) then
          k = -i
          do i = k, ncut-1
            xcut(i) = xcut(i+1)
            ycut(i) = ycut(i+1)
            pa(i) = pa(i+1)
          enddo
          ncut = ncut-1
c  Start new list
        else if (i.le.-99) then
          call output('All cuts deleted from list')
          ncut = 0
        else
          call output(' ')
          call output('Cut number must edit or extend list by one')
          goto 10
        endif
        goto 10
c
c  RA-velocity maps.
c
      else if (ans.eq.'X') then
        call prompt(line,length,
     *    '>Enter starting DEC offset, ending DEC offset, interval:')
        if (length.eq.0) goto 50
        call matodf(line,dval,3,ok)
        if (ok) then
          pstart = dval(1)
          pend   = dval(2)
          pinc   = dval(3)
          i = 1
          pos = pstart
          do while (i.le.128 .and. pos.le.pend)
            ycut(i) = pos
            xcut(i) = 0.0
            pa(i) = 90.0
            pos = pos + pinc
            i = i + 1
          enddo
          ncut = i - 1
        endif
c
c  DEC-velocity maps.
c
      else if (ans.eq.'Y') then
        call prompt(line,length,
     *    '>Enter starting X offset, ending X offset, interval:')
        if (length.eq.0) goto 50
        call matodf(line,dval,3,ok)
        if (ok) then
          pstart = dval(1)
          pend   = dval(2)
          pinc   = dval(3)
          i = 1
          pos = pstart
          do while (i.le.128 .and. pos.le.pend)
            xcut(i) = pos
            ycut(i) = 0.0
            pa(i) = 0.001
            pos = pos + pinc
            i = i + 1
          enddo
          ncut = i - 1
        endif
      else
        goto 9
      endif
50    if (ncut.eq.0) then
        call output('--- no current selection of cuts ---')
        return
      endif
c
c  Set up convolution function.
c
      call convsize(cmaj,cmin,cpa,xy,ncon)
      call convinit(cmaj,cmin,cpa,xy,ncon,con)
c
c  Check whether it is contour or intensity plots
c
      nchan=0
      call prompt(line,length,'Contour or Intensity plots ? [C]/I: ')
      if (length.eq.0) goto 399
      call ucase(line)
      if (line.eq.'I') then
398     continue
        call prompt(line,length,'How many channels ? (5 max): ')
        nchan=1
        if (length.eq.0) goto 399
        read (line(1:length),101) nchan
101     format(i10.0)
        if (nchan.gt.nc) then
          call output('Cannot be more than there are in the image')
          goto 398
        endif
        if (nchan.gt.5) then
          call output('Up to 5 channels allowed')
          goto 398
        endif
        call prompt(line,length,'Enter the channel numbers: ')
        read(line(1:length), '(5i10)') (ichan(i),i=1,nchan)
c         do l=1,nchan
c           read(substr(line,l),101) ichan(l)
c         enddo
        call prompt(line,length,'Fix the scales ?  Y/[N]: ')
        call ucase(line)
        ifix=0
        if (line.eq.'Y') then
          ifix=1
          call prompt(line,length,'Enter xmin,xmax,ymin,ymax: ')
          read (line(1:length),'(4f20.0)') xmin,xmax,ymin,ymax
        endif
      endif
399   continue
c
c  Set maptype to position-velocity; save plot device from command line.
c
      maptype = 'POS-VEL'
      xlabel='velocity (km/s)'
      ylabel='position (arcsec)'
      olddev=device
      lwidth = 1
c
c  Set up the plot windows.
c
60    if (write.ne.'Y' .and. apint.ne.'Y') then
        call plPanels(ncut,windx,windy)
        call pgbeg(0,device,windx,windy)
c         call pgask(.FALSE.)
        call pgslw(lwidth)
      endif
c
c  Calculate the conversion factor for map units.
c
      call header(0)
      cf = 1.0
      if (units.eq.'K') cf = dperjy

c
c  Convolve array into position-velocity maps, output in V.
c
      do l = 1, ncut
        call pgpage
        if (windx*windy.eq.1 .and. alabel.eq.'Y') then
          call pgsvp(0.1,0.7,0.1,0.9)
        else
          call pgsvp(0.1,0.9,0.1,0.9)
        endif
        call veloline(ary,nx,ny,nc,xcut(l),ycut(l),pa(l),ncon,con,
     *    v,np,xstart,xend)
        vstart = vlsr(1)
        vend = vlsr(nc)
        if (nchan.eq.0) then
          call output('--- position velocity map ---')
          else
          call output('--- position intensity plot ---')
        endif
        write(line, 109) l,xcut(l),ycut(l),pa(l)
        call output(line)
c
c  Set up parameters for plot labels.
c
        write(lab1,104) -xcut(l)
        write(lab2,104) ycut(l)
        write(lab3,104) pa(l)
        lab4=char(l+96)
        if (pspec.eq.'Y') then
          label=lab4//' ('//lab1//','//lab2//')   PA='//lab3
        else
          label=' ('//lab1//','//lab2//')   PA='//lab3
        endif
104     format(f8.2)
c
c  Write out map.
c
        if (write.eq.'Y') then
          call writeposvel(nc,np,1,v,vstart,xstart,pa(l),1.0,1.0,
     *                (vend-vstart)/(nc-1),(xend-xstart)/(np-1))
c
c  Integer plot.
c
        else if (apint.eq.'Y') then
          call plotint(v,nc,np)
c
c  Plot contours.
c
        else
          if (nchan.eq.0) then
            call pgswin(vstart,vend,xstart,xend)
c  Set up transformation array for contour routine.
            tr(2)=(vend-vstart)/(nc-1)
            tr(1)=vstart-tr(2)
            tr(3)=0.0
            tr(5)=0.0
            tr(6)=(xend-xstart)/(np-1)
            tr(4)=xstart-tr(6)
c  Plot contours.
            call plotcon(v,nc,np,cf,tr)
            call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
            call pglab(xlabel,ylabel,label)
          else
            if (cf.eq.0.0) cf=1.0
            if (ifix.eq.0) then
              ymax=-1e+6
              ymin=1e+6
              do ii = 1, nchan
                do jj = 1, np
                  jjj=(jj-1)*nc+ichan(ii)
                  if ((v(jjj)/cf).gt.ymax) ymax=v(jjj)/cf
                  if ((v(jjj)/cf).lt.ymin) ymin=v(jjj)/cf
                enddo
              enddo
              ymin = ymin - 0.05 * (ymax - ymin)
              ymax = ymax + 0.05 * (ymax - ymin)
              call pgswin(xstart,xend,ymin,ymax)
            else
              call pgswin(xmin,xmax,ymin,ymax)
            endif

            call pgbox('BCNST',0.0,0,'BCNST',0.0,0)

            if (units.eq.'K') then
              xlabel='Kelvin'
            else if (units.eq.'J') then
              xlabel='Janskys'
            else
              xlabel='Map units'
            endif

            call pglab(ylabel,xlabel,label)
            xdelta=(xstart-xend)/real(np-1)
            if (xstart.lt.xend) xdelta=abs(xdelta)
            if (xstart.gt.xend) xdelta=-1.0*abs(xdelta)
            do ii = 1, nchan
              do jj = 1, np
                jjj=(jj-1)*nc+ichan(ii)
                xval(jj)=v(jjj)/cf
                xcoord(jj)=xstart+real(jj-1)*xdelta
              enddo
              call pgsls(ii)
              call pgline(np,xcoord,xval)
            enddo
            call pgsls(1)
            maptype='SPECTRA'
          endif

c         Now annotate plot (had to wait for plotcon to get values).
          if (windx*windy.eq.1 .and. alabel.eq.'Y') then
            call pgsvp(0.72,0.9,0.1,0.9)
            call plAnnot(cf,cmaj,cmin,cpa)
            if (nchan.ne.0) call intannot(nchan,ichan,vlsr,nc)
            call pgsvp(0.1,0.7,0.1,0.9)
          endif

c         Write ASCII file of channel intensities along cut.
          if (nchan.ne.0) then
            call writeint(v,nc,np,xstart,xend,cf,nchan,ichan,
     *                    xcut(l),ycut(l),pa(l),vlsr)
          endif
        endif
      enddo
c
c  Replotting options.
c
      if (apint.ne.'Y' .and. write.ne.'Y') then
        if (alabel.eq.'Y') call pgiden
        call pgqinf('HARDCOPY',ans,length)
        call pgend
        if (ans.ne.'YES') then
          call output(' ')
          call prompt(ans,length,'Another PGPLOT device? (Y/[N])')
          call ucase(ans)
          if (ans(1:1).eq.'Y') then
            call prompt(device,length,'>Enter new PGPLOT device: ')
            call lcase(device)
            call prompt(ans,length,'>Enter line width [1]: ')
            if (length.ne.0) then
              read(ans,'(i1)') lwidth
            endif
            goto 60
          endif
        endif
      endif
c
c  Restore original plot device.
c
      device = olddev

      end

c***********************************************************************

      subroutine plPanels(imaps,windx,windy)

      integer imaps,windx,windy
c-----------------------------------------------------------------------
c       Return number of plotting windows in x and y directions
c       (windx,windy) imaps is the number of maps to be plotted
c-----------------------------------------------------------------------
      character msg*80, line*80
      integer length
      double precision dval(2)
      logical ok
c-----------------------------------------------------------------------
      if (imaps.gt.16) then
        windx=5
        windy=5
      else if (imaps.gt.9 .and. imaps.le.16) then
        windx=4
        windy=4
      else if (imaps.gt.4 .and. imaps.le.9) then
        windx=3
        windy=3
      else if (imaps.gt.1 .and. imaps.le.4) then
        windx=2
        windy=2
      else if (imaps.eq.1) then
        windx=1
        windy=1
      endif
c
c  See if these defaults are ok with the user
c
      call output(' ')
      write(msg,'(a,i2,a,i2,a)')
     * '>Enter number of windows in x and y: [',windx,',',windy,']:'
      call prompt(line, length, msg)
      if (length.ne.0) then
        call matodf(line,dval,2,ok)
        if (ok) then
          if (dval(1).ne.0d0) windx=dval(1)
          if (dval(2).ne.0d0) windy=dval(2)
        endif
      endif

      end

c***********************************************************************

      subroutine rdary
c-----------------------------------------------------------------------
c  Read file of spectra & position-velocity cuts.
c-----------------------------------------------------------------------
      integer lu,iostat,len1,n,length
      character*80 file,line

      integer ncut
      real xcut(128),ycut(128),pa(128)
      common /cuts/ xcut,ycut,pa,ncut

      integer nspec,ngauss(49)
      real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c-----------------------------------------------------------------------
c
c  Open input file.
c
      call prompt(file,length,
     *    'Input file for spectra & position-velocity cuts: ')
      call txtopen(lu,file,'old',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening ascii file')
        return
      endif

      do n = 1, 2
        call txtread(lu,line,length,iostat)
        if (iostat.ne.0) then
          call bug('w','Error reading ascii file')
          return
        endif
        call output(line)
      enddo
c
c  Read spectra positions.
c
      call txtread(lu,line,length,iostat)
      call output(line)
      read(line,'(i4)') nspec
      if (nspec.ne.0) then
        do n = 1, nspec
          call txtread(lu,line,length,iostat)
          call output(line)
          read(line,'(2f20.4)') xc(n),yc(n)
c zero number of gaussians in fit to spectrum
          ngauss(n)=0
        enddo
      endif
c
c  Read position-velocity cuts.
c
      call txtread(lu,line,length,iostat)
      call output(line)
      read(line,'(i4)') ncut
      if (ncut.ne.0) then
        do n = 1, ncut
          call txtread(lu,line,length,iostat)
          call output(line)
          read(line,'(3f20.4)') xcut(n),ycut(n),pa(n)
        enddo
      endif
c
c  Close input file and tell user.
c
      call txtclose(lu)
      write(line,'(i3,a,i3,a,a)') nspec,' spectra and ',ncut,
     *  ' position-velocity cuts from file: ',file(1:len1(file))
      call output(line(1:len1(line)))

      end

c***********************************************************************

      subroutine readmap(lIn,ary,vlsr,nx,ny,nc)

      integer lIn,nx,ny,nc
      real ary(1),vlsr(1)
c-----------------------------------------------------------------------
c  Read portion of Miriad Image into array.
c
c  Inputs:
c    lIn        Handle of input Image.
c
c  Outputs:
c    ary(nx,ny,nc)
c               spectral line image
c    vlsr(nc)   velocity array
c    nx ny nc   dimensions of images
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mirconst.h'

      double precision CKMS
      parameter (CKMS = DCMKS*1d-3)

      integer   i, ipt, j, k
      real      cdelt1, cdelt2, cdelt3, crpix1, crpix2,
     *          crpix3, crval3, row(MAXDIM)
c-----------------------------------------------------------------------
      call rdhdr(lIn, 'crpix1', crpix1, real(nx/2+1))
      call rdhdr(lIn, 'crpix2', crpix2, real(ny/2+1))
      call rdhdr(lIn, 'crpix3', crpix3, 1.0)
      call rdhdr(lIn, 'cdelt1', cdelt1, 0.0)
      call rdhdr(lIn, 'cdelt2', cdelt2, 0.0)
      call rdhdr(lIn, 'cdelt3', cdelt3, 1.0)
      call rdhdr(lIn, 'crval1', crval(1), 0.0)
      call rdhdr(lIn, 'crval2', crval(2), 0.0)
      call rdhdr(lIn, 'crval3', crval3,   1.0)
      call rdhda(lIn, 'ctype1', ctype(1), ' ')
      call rdhda(lIn, 'ctype2', ctype(2), ' ')
      call rdhda(lIn, 'ctype3', ctype(3), ' ')

      if (cdelt1.eq.0.0 .or. cdelt2.eq.0.0)
     *  call bug('f', 'Coordinate increment missing (cdelt).')
      if (abs(cdelt1).ne.abs(cdelt2)) 
     *  call bug('f', 'Unequal coordinate increments (cdelt).')

      xy = abs(cdelt1)
      if ((ctype(1)(1:2).eq.'RA'   .and. ctype(2)(1:3).eq.'DEC') .or.
     *    (ctype(1)(1:4).eq.'GLON' .and. ctype(2)(1:4).eq.'GLAT')) then
        xy = xy*R2AS
      endif

c     Reference pixel in box coordinates.
      brpix(1) = nint(crpix1) - blc(1) + 1
      brpix(2) = nint(crpix2) - blc(2) + 1

      call rdhda(lIn, 'bunit', bunit, ' ')
      call rdhdr(lIn, 'bmaj',  bmaj,  0.0)
      call rdhdr(lIn, 'bmin',  bmin,  0.0)
      call rdhdr(lIn, 'bpa',   bpa,   0.0)
      call rdhdr(lIn, 'epoch', epoch, 0.0)
      call rdhdi(lIn, 'niters', niters, 0)
      call rdhda(lIn, 'object', object, ' ')
      call rdhdr(lIn, 'restfreq', restfreq, 0.0)

      if (ctype(3)(1:4).eq.'FREQ' .and. restfreq.ne.0.0) then
        call output('Convert frequency axis to velocity')
        cdelt3 = (cdelt3/restfreq)*CKMS
        crval3 = (crval3/restfreq)*CKMS
      endif

      vel  = crval3 + (blc(3)-crpix3)*cdelt3
      delv = cdelt3
      do i = 1, nc
        vlsr(i) = crval3 + (blc(3)-crpix3 +i-1)*cdelt3
      enddo


      call output('Reading data cube...')
      ipt = 1
      do k = blc(3), trc(3)
        call xysetpl(lIn,1,k)
        do j = blc(2), trc(2)
          call xyread(lIn,j,row)
          do i = blc(1), trc(1)
            ary(ipt) = row(i)
            ipt = ipt + 1
          enddo
        enddo
      enddo

      end

c***********************************************************************

      subroutine setCont

c-----------------------------------------------------------------------
c  Set contour levels and label interval
c-----------------------------------------------------------------------
      include 'velplot.h'
      character*80 ans,line
      real cmin,cmax,cint
      integer l,i
c-----------------------------------------------------------------------
      call output(' ')
      call prompt(percent,l,
     * '>Enter contours as % (Y), Absolute, Log, [previous levels]: ')
      if (l.eq.0) return
      call ucase(percent)
      if (percent.eq.'L') then
        write(line,'(a)') '>Enter lowest level:'
        call output(line)
        read(5,*) levels(1)
        nlevels=10
        do i = 2, nlevels
          levels(i)=1.584893*levels(i-1)
        enddo
      else
        call prompt(ans,l,'Enter contour List or Range (L/[R]): ')
        call ucase(ans)
        if (ans.eq.'L') then
          write(line, '(a)') '>Enter levels (max 10, end=-9999.0):'
          call output(line)
          do i = 1, 10
            levels(i)=0.0
          enddo
          do i = 1, 10
           write(line,'(a,i2,a)') '>Enter: level(',i,')='
           call output(line)
           read(5,*) levels(i)
           if (levels(i).eq.-9999.0 .or. i.eq.10) then
             nlevels=i-1
             goto 129
           endif
          enddo
        else
          write(line, '(a)') '>Enter min,max,interval: '
          call output(line)
          read(5,*) cmin,cmax,cint
          nlevels =1
          levels(1)=cmin
          do while (levels(nlevels).lt.cmax .and. nlevels.lt.10)
            nlevels = nlevels + 1
            levels(nlevels) = levels(nlevels-1) + cint
          enddo
        endif
      endif

129   end

c***********************************************************************

      subroutine setGray
c-----------------------------------------------------------------------
c  Set grayscale levels
c-----------------------------------------------------------------------
      include 'velplot.h'
      character msg*80
      integer l
c-----------------------------------------------------------------------
c
c Grayscale default to map min/max or
c enter values for bg and fg
c
      call output(' ')
      call prompt(defgray,l,
     *  '>Use map min/max for grayscale (Y/[N]): ')
      call ucase(defgray)
      if (defgray.eq.'Y') return
      call output(' pgplot grayscale shade is a number in the ')
      call output(' range 0 to 1 obtained by linear interpolation')
      call output(' between the background and foreground level,')
      call output('e.g. shade=[A(i,j)-bg]/[fg-bg] ')
      call output(' ')
100   write(msg, '(a)') '>Enter grayscale background (bg) level:'
      call output(msg)
      read(5,110,err=100) bg
      call output(' ')
200   write(msg, '(a)') '>Enter grayscale foreground (fg) level:'
      call output(msg)
      read(5,110,err=200) fg
110   format(f20.0)

      end

c***********************************************************************

      subroutine spectra(ary,vlsr,nx,ny,nc)

      integer nx,ny,nc
      real ary(nx,ny,nc),vlsr(nc)
c-----------------------------------------------------------------------
c  Plot spectra at selected positions in image.
c
c  Inputs:
c    ary        The image.
c    nx,ny,nc   Dimensions of image.
c    vlsr       Array of velocities.
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mirconst.h'

      real t(MAXDIM),tk(MAXDIM,49)
      character*80 outfile
      integer maxspec,ix,iy,lu,iostat
      real ymin,ymax,cf,cmaj,cmin,cpa,xtlc,ytlc
      real x0,y0,vmin,vmax,x,y,wt,pa,step,sum,sumc
      integer windx,windy,ncon,ns,nsmooth
      integer spec,c,i,j,k,ig,nrows,ncols,length,ichannel,lwidth
      character text*310
      character*80 olddev,line
      character*1 ans,fix,smooth,sym,noplot
      character*28 label,xlabel,ylabel
      character*8 xchar,ychar
      double precision dval(5)
      logical ok
c  convolution array maximum size
      real con(99,99,4,4)

      integer nspec,ngauss(49)
      real xc(49),yc(49)
      real gauss(49,3,10),gausserr(49,3,10)
      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr

      data maxspec/49/
c-----------------------------------------------------------------------
c
c  Introduction.
c
      call output(' ')
      call output('Plot spectra at selected positions.')
      call output('(x,y) positions are in (HA,DEC) directions.')
      call prompt(ans,length,
     *  '>Type H for Help, E to Exit, <cr> to continue: ')
      call ucase(ans)
      if (ans.eq.'H') then
        call output(' ')
        call output(
     *    'Positions are selected with the cursor on an x-y map')
        call output('or can be entered as a GRID or a LIST here.')
        call output(
     *    'Spectra can also be convolved in space and velocity')
        call output(
     *    'and written as ascii files for further processing.')
        call output(' ')
      endif
      if (ans.eq.'E') then
        call output('No spectra selected')
        return
      endif
c
c  List current selection of spectra.
c
      goto 9
11    call output(' ')
      call output(
     *  'Try again; spectrum number must edit or extend list')
9     if (nspec.ne.0) then
        call output(' ')
        call output('--- current selection of spectra ---')
        do i = 1, nspec
          write(line, 109) i,xc(i),yc(i)
          call output(line)
        enddo
      else
        call output('--- no current selection of spectra ---')
      endif
109   format(' spectra(',i2,') (x,y) arcsecs: (',f9.3,',',f9.3,')')
c
c  Select List or grid of positions for spectra.
c
      call output(' ')
      call output('>Enter G to enter a grid of positions,')
      call prompt(ans,length,
     *  '      <cr> to edit or enter a list of positions: ')
      call ucase(ans)
c
c  Enter grid of positions for spectra.
c
      if (ans.eq.'G') then
        call output(' ')
        call output(
     *        '>Enter tlc (x,y), and PA of grid: ')
        read(5,*) xtlc,ytlc,pa
        call output('  (Grid PA is measured from N through E)')
        call output(
     *      '>Enter interval(arcsec), nrows(along PA) and ncols:')
        read(5,*) step,nrows,ncols
        pa = pa*D2R
        nspec=min(nrows*ncols,maxspec)
        do ig = 1, nspec
          xc(ig) = xtlc + real(mod(ig-1,ncols))*step*cos(pa)
     *              + real(((ig-1)/ncols))*step*sin(pa)
          yc(ig) = ytlc + real(mod(ig-1,ncols))*step*sin(pa)
     *              - real(((ig-1)/ncols))*step*cos(pa)
        enddo
        call output('--- current selection of spectra ---')
        do i = 1, nspec
          write(line, 109) i,xc(i),yc(i)
          call output(line)
        enddo
        call output(' ')
        call prompt(ans,length,'selection OK? ([Y]/N): ')
        call ucase(ans)
        if (ans.eq.'N') goto 9
        goto 20
      endif
c
c  Enter list of positions for spectra.
c
      call output(' ')
      call output('---- Enter list of spectra ----')
      call output('>Type -n to delete spectra n')
      call output('      -99 to delete all')
      call output('       L to list')
      call output('      <cr> to use the current list')
10    call prompt(line,length,
     *      '>Enter spectrum number and position (n,X,Y): ')
      if (length.eq.0) goto 20
      call matodf(line,dval,3,ok)
      if (ok) then
        i = dval(1)
        x = dval(2)
        y = dval(3)
      else
        goto 11
      endif
      if (i.eq.0) goto 20
c  end of list; plot spectra
      if (i.ge.1 .and. i.le.min(nspec+1,maxspec)) then
c  add or change point
        xc(i) = x
        yc(i) = y
        nspec = max(i,nspec)
      else if (-i.ge.1 .and. -i.le.nspec) then
c  delete point
        k = -i
        do i = k, nspec-1
          xc(i) = xc(i+1)
          yc(i) = yc(i+1)
          ngauss(i)=ngauss(i+1)
          do j = 1, ngauss(i+1)
            gauss(i,1,j)=gauss(i+1,1,j)
            gauss(i,2,j)=gauss(i+1,2,j)
            gauss(i,3,j)=gauss(i+1,3,j)
            gausserr(i,1,j)=gausserr(i+1,1,j)
            gausserr(i,2,j)=gausserr(i+1,2,j)
            gausserr(i,3,j)=gausserr(i+1,3,j)
          enddo
        enddo
        nspec = nspec-1
      else if (i.le.-99) then
c  start new list
        nspec = 0
      else
        call output(' ')
        call output(
     *      'Try again; spectrum number must edit or extend list')

        goto 9
      endif
      goto 10

20    if (nspec.eq.0) then
        call output(' ')
        call output('--- no spectra selected ---')
        return
      endif
c
c  Write info' into log if Gaussian fitting.
c
      if (lgaufit.eq.'Y') then
        call output(' ')
        call output('GAUSSIAN FITS TO SPECTRA')
        call LogWrit(' ')
        call LogWrit('GAUSSIAN FITS TO SPECTRA')
      endif
c
c  Set up convolution function.
c
      call convsize(cmaj,cmin,cpa,xy,ncon)
      call convinit(cmaj,cmin,cpa,xy,ncon,con)
      write(line,'(a,3f10.4)') '  original beam ', bmaj*R2AS, bmin*R2AS,
     *  bpa
      call output(line)
      if (lgaufit.eq.'Y') call LogWrit(line)
      write(line,'(a,3f10.4)') 'convolving beam ', cmaj, cmin, cpa
      call output(line)
      if (lgaufit.eq.'Y') call LogWrit(line)
      write(line,'(a)') 'Units of intensity using the original beam'
      call output(line)
      if (lgaufit.eq.'Y') call LogWrit(line)
c
c  Inquire about smoothing.
c
      call prompt(smooth,length,
     *  '>Smooth spectra (H=Hanning, B=Boxcar, [None]): ')
      call ucase(smooth)
      if (smooth.eq.'B') then
        call output(
     *    '>Enter number of channels to average (Integer): ')
        read (5,*) nsmooth
        if (nsmooth.lt.1) nsmooth = 1
      endif
c
c  Omit plots and Gaussian fits ?
c
      call prompt(noplot,length,
     *  '>Omit plots and Gaussian fits ? (Y/[N]): ')
      call ucase(noplot)
      if (noplot.eq.'Y') goto 60
c
c  Inquire about fixed scale spectra.
c
      call prompt(fix,length,
     *  '>Fix scale for spectra ? (Y/[N]): ')
      call ucase(fix)
      if (fix.eq.'Y') then
        call output('>Enter Vmin,Vmax,Ymin,Ymax in selected units: ')
        read(*,*) Vmin,Vmax,Ymin,Ymax
      endif
c
c  Set maptype and save plot device type from comand line.
c
      maptype = 'SPECTRA'
      olddev=device
      lwidth = 1
c
c  Initialize plot device.
c
      call plPanels(nspec,windx,windy)
50    call pgbeg(0,device,windx,windy)
c       call pgask(.FALSE.)
      call pgslw(lwidth)
c
c  Calculate the conversion factor for map units.
c
60    call header(0)
      cf = 1.0
      if (units.eq.'K') cf = dperjy
c
c  Loop through list of spectra.
c
      do spec = 1, nspec
c
c  Check for plot annotation.
c
        if (noplot.ne.'Y') then
          call pgpage
          if (windx*windy.eq.1 .and. alabel.eq.'Y') then
            call pgsvp(0.1,0.7,0.1,0.9)
          else
            call pgsvp(0.1,0.9,0.1,0.9)
          endif
        endif
c
c  Find position of spectra in the array.
c
        x0 = xc(spec)/xy + brpix(1)
        y0 = yc(spec)/xy + brpix(2)
        ix = nint(x0)
        iy = nint(y0)
        if (ix.lt.1 .or. ix.gt.nx .or. iy.lt.1 .or. iy.gt.ny) then
          call output('requested spectra is outside array')
          if (noplot.ne.'Y') call pgend
          goto 30
        endif
c
c  Convolve array at each position; return convolved spectra in t
c
        call conv(ary,nx,ny,nc,x0,y0,ncon,con,t,wt)
c
c  Boxcar smoothing if requested.
c
        if (smooth.eq.'B') then
          do ichannel = 1,nc,nsmooth
            sum = 0.0
            sumc = 0.0
            do c = ichannel,min(ichannel+nsmooth-1,nc)
              sum = sum + t(c)
              sumc = sumc + 1.0
            enddo
            do c = ichannel,min(ichannel+nsmooth-1,nc)
              t(c) = sum / sumc
            enddo
          enddo
c
c  Hanning smooth spectrum if requested. Note that the end channels
c     are not affected and the number of pixels stays the same.
c
        else if (smooth.eq.'H') then
          do c = 2, nc-1
            t(c) = (t(c) + (t(c-1)+t(c+1))/2.0)/2.0
          enddo
        endif
c
c  Save convolved spectra and write to array for plotting.
c
        if (fix.ne.'Y') then
          vmin = vlsr(1)
          vmax = vlsr(1)
          ymin = t(1)*cf
          ymax = t(1)*cf
        endif

        do k = 1, nc
          t(k) = t(k) * cf
          tk(k,spec) = t(k)
          if (fix.ne.'Y') then
            ymin = min(ymin,t(k))
            ymax = max(ymax,t(k))
            vmin = min(vmin,vlsr(k))
            vmax = max(vmax,vlsr(k))
          endif
        enddo
        if (noplot.eq.'Y') goto 70
c
c  Set max and min for plot.
c
        if (fix.ne.'Y') then
          vmin = vmin - 0.05 * (vmax - vmin)
          vmax = vmax + 0.05 * (vmax - vmin)
          ymin = ymin - 0.05 * (ymax - ymin)
          ymax = ymax + 0.05 * (ymax - ymin)
        endif
c
c  Plot spectra.
c
        call pgswin(vmin,vmax,ymin,ymax)
        call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
        call pgHline(nc,vlsr,t,2.0)
c
c  Label spectra with offset coords.
c
        write(xchar,116) -xc(spec)
        write(ychar,116) yc(spec)
        write(sym,115) char(spec+64)
115     format(a)
116     format(f8.2)
c
c  include symbol with spectrum position or not
c
        if (pspec.eq.'Y') then
          label = sym//' ('//xchar(1:index(xchar,'.')+2)//','
     *        //ychar(1:index(ychar,'.')+2)//')'
        else
          label = ' ('//xchar(1:index(xchar,'.')+2)//','
     *        //ychar(1:index(ychar,'.')+2)//')'
        endif

        xlabel='Velocity (km/s)'
        if (units.eq.'K') then
          ylabel='K'
        else if (units.eq.'J') then
          ylabel='JY'
        else
          ylabel='Map units'
        endif
        if (alabel.eq.'Y') call pglab(xlabel,ylabel,label)

        if (windx*windy.eq.1 .and. alabel.eq.'Y') then
          call pgsvp(0.72,0.9,0.1,0.9)
          call plAnnot(cf,cmaj,cmin,cpa)
          call pgsvp(0.1,0.7,0.1,0.9)
          call pgswin(vmin,vmax,ymin,ymax)
        endif
c
c If not harcopy device, then call routine to fit gaussians
c Otherwise, call routine to plot gaussians
        call pgqinf('HARDCOPY',ans,length)
        if (ans(1:1).eq.'N' .and. lgaufit.eq.'Y') then
          call gaufit(nc,vlsr,t,spec,arms)
        endif
        if (lgauplot.eq.'Y') then
          call gauplot(nc,vlsr,t,spec)
        endif
c
c  If this is the last window close the plot
c
        if (spec.eq.nspec) then
          if (alabel.eq.'Y') call pgiden
          call pgqinf('HARDCOPY',ans,length)
          call pgend
          if (ans(1:1).ne.'Y') then
            call output(' ')
            call prompt(ans,length,'Another PGPLOT device? (Y/[N])')
            call ucase(ans)
            if (ans(1:1).eq.'Y') then
              call prompt(device,length,'>Enter new PGPLOT device: ')
              call lcase(device)
              call prompt(ans,length,'>Enter line width [1]: ')
              if (length.ne.0) then
                read(ans,'(i1)') lwidth
              endif
              goto 50
            endif
          endif
        endif

30      continue
      enddo
c
c  Restore original plot device.
c
      device=olddev
c
c  Write out convolved spectra to ASCII file.
c
70    line='>Enter filename for spectra (<cr> to continue): '
      call prompt(outfile,length,line)
      if (length.gt.0) then
        ns = min(nspec,30)
        if (nspec.gt.30) then
          call output('only first 30 spectra written to '//outfile)
        endif
        call TxtOpen(lu,outfile,'new',iostat)
        if (iostat.eq.0) write(text,'(a,a)')
     *    'File: ',outfile
        length=6 + len(file)
        call TxtWrite(lu,text,length,iostat)
        if (iostat.eq.0) write(text,'(a,a)')
     *    'Image File: ',file
        length=12 + len(file)
        call TxtWrite(lu,text,length,iostat)
        if (iostat.eq.0) write(text,'(a,a,4x,f11.6,a)')
     *    'Spectra for ',object,restfreq,' GHz'
        length=12 + len(object) + 15 + 4
        call TxtWrite(lu,text,length,iostat)
        if (iostat.eq.0) write(text,'(a,2f12.3,a,f6.1,a)')
     *  'Convolving beam: ',cmaj,cmin,' arcsecs, pa: ',cpa,' degrees'
        length = 17 + 12 + 12 + 14 + 6 + 8
        call TxtWrite(lu,text,length,iostat)
        if (iostat.eq.0) write(text,'(a)') 'Positions (x,y) arcsecs'
        call TxtWrite(lu,text,23,iostat)
        k=0
        do while (iostat.eq.0 .and. k.lt.ns)
          write(text,'(4(a,2f7.2,a))')
     *      ('(',xc(i+k),yc(i+k),') ',i=1,min(4,ns-k))
          length = min(4,ns-k)*(1+2*7+2)
          call TxtWrite(lu,text,length,iostat)
          k=k+4
        enddo
        do while (length.gt.0 .and. iostat.eq.0)
          call prompt(text,length,'Enter comment: ')
          call TxtWrite(lu,text,length,iostat)
        enddo
        k=1
        do while (iostat.eq.0 .and. k.le.nc)
          write(text,'(31(1x,f9.3))') vlsr(k),(tk(k,i),i=1,ns)
          call TxtWrite(lu,text,10+10*ns,iostat)
          k=k+1
        enddo
        call TxtClose(lu)
      endif
c
c  Write out gaussian fits to ASCII file.
c
      if (ngauss(1).gt.0) then
        line='>Enter filename for gaussian fits (<cr> to continue):'
        call prompt(outfile,length,line)
        if (length.gt.0) call wrGauss(outfile,nc,vlsr,cmaj,cmin,cpa)
      endif

      end

c***********************************************************************

      subroutine velaver(vmin,vmax,vlsr,ary,nx,ny,nc,
     *                    nmom,v,nmaps,vmean,vwidth,
     *                    vmom1,vmom2,vmom3,vmom4)

      integer nx,ny,nc,nmom,nmaps
      real vmin,vmax,vlsr(nc),ary(nx,ny,nc),v(nx*ny)
      real vmom1(nx*ny),vmom2(nx*ny),vmom3(nx*ny),vmom4(nx*ny)
      real vmean,vwidth
c-----------------------------------------------------------------------
c       Average velocity maps in the range vmin to vmax.
c
c  Inputs:
c    vmin,vmax          Velocity range to be averaged.
c    vlsr(nc)           Array of velocities.
c    ary(nx,ny,nc)      Array to be averaged.
c    nx,ny,nc           Dimensions of array.
c    nmom               The moment to be computed.
c  Outputs:
c    v(nx*ny)           Averaged map.
c    nmaps              Number of maps in the average.
c    vmean,vwidth       The mean velocity and width of the averaged map.
c-----------------------------------------------------------------------
      include 'velplot.h'
      character msg*80
      real anorm,vcent,vcent2,sigma,sigma2,v2origin,v3origin,v3
      real wt,wtmom1,wtmom2,wtmom3,wtmom4,vsum
      integer i,j,k,m,noff,kmax
c-----------------------------------------------------------------------
c
c  Velocity of peak channel.
c
      if (nmom.eq.-1) then
        do j = 1, ny
          do i = 1, nx
            amax = cutoff
            do k = 1, nc
              if (vlsr(k).ge.vmin .and. vlsr(k).le.vmax .and.
     *            ary(i,j,k).gt.amax) then
                kmax = k
                amax = ary(i,j,k)
              endif
            enddo
            if (amax.gt.cutoff) then
              v(nx*(j-1)+i) = vlsr(kmax)
            else
              v(nx*(j-1)+i) = -99999.0
            endif
          enddo
        enddo
        nmaps=1
        return
      endif
c
c  Initialize arrays.
c
      nmaps = 0
      anorm = 0.0
      vmean = 0.0
      vsum = 0.0
      vwidth = 0.0
      do i = 1, nx*ny
        v(i)=0.0
        if (nmom.ge.1) vmom1(i) = 0.0
        if (nmom.ge.2) vmom2(i) = 0.0
        if (nmom.ge.3) vmom3(i) = 0.0
        if (nmom.ge.4) vmom4(i) = 0.0
      enddo
c
c  Get weights for each velocity moment map.
c
      do k = 1, nc
        delv = vlsr(2) - vlsr(1)
        call veloint(vmin,vmax,vlsr(k),delv,wt)
        if (wt.ne.0) then
          nmaps = nmaps + 1
          anorm = anorm + wt
          vsum = vsum + wt*vlsr(k)
          if (nmom.ge.1) wtmom1 = wt*vlsr(k)
          if (nmom.ge.2) wtmom2 = wtmom1*vlsr(k)
          if (nmom.ge.3) wtmom3 = wtmom2*vlsr(k)
          if (nmom.eq.4) wtmom4 = wtmom3*vlsr(k)
c
c  Compute velocity moment maps.
c
          do j = 1, ny
            noff = nx*(j-1)
            do i = 1, nx
              if (ary(i,j,k).ge.cutoff) then
                m = i+noff
                v(m) = wt*ary(i,j,k) + v(m)
                if (nmom.ge.1) vmom1(m)=wtmom1*ary(i,j,k)+vmom1(m)
                if (nmom.ge.2) vmom2(m)=wtmom2*ary(i,j,k)+vmom2(m)
                if (nmom.ge.3) vmom3(m)=wtmom3*ary(i,j,k)+vmom3(m)
                if (nmom.eq.4) vmom4(m)=wtmom4*ary(i,j,k)+vmom4(m)
              endif
            enddo
          enddo
        endif
      enddo
c
c  Normalize for straight average (0-th moment).
c
      if (nmom.eq.0) then
        if (anorm.ne.0.0) then
          vmean = vsum/anorm
          vwidth = anorm*(vlsr(2)-vlsr(1))
          do i = 1, nx*ny
            v(i) = v(i) / anorm
          enddo
        endif
        write(msg, '(f7.2,a,2f10.3,a)') anorm,
     *    ' averaged into velocity window',vmin,vmax,' km/s'
        call output(msg)
        return
      endif
c
c  Normalize by 0-th moment map, and for higher moments, form
c  moments with respect to the mean, using the relations given in
c  Abramowitz and Stegun, Handbook of Mathematical Functions, p. 928.
c
      do i = 1, nx*ny
        if (v(i).ge.cutoff) then
c         1st moment (centroid).
          vcent = vmom1(i)/v(i)

          if (nmom.eq.1) v(i) = vcent
          if (nmom.ge.2) then
c           Needed for higher moments.
            vcent2 = vcent*vcent

c           2nd mom. w.r.t. origin.
            v2origin = vmom2(i)/v(i)

c           2nd mom w.r.t. mean (variance).
            sigma2 = abs(v2origin - vcent2)

c           Std. dev., or "line width".
            sigma = sqrt(sigma2)
          endif

          if (nmom.eq.2) v(i) = sigma

c         Coeff. of skewness (cf. Abramowitz and Stegun).
          if (nmom.ge.3) then
c           3rd mom. w.r.t. origin.
            v3origin = vmom3(i)/v(i)

c           3rd mom. w.r.t. mean.
            v3 = v3origin - 3.0*sigma2*vcent - vcent*vcent2
          endif

          if (nmom.eq.3) v(i) = v3/sigma2/sigma

c         Coeff. of excess, or kurtosis.
          if (nmom.eq.4)
     *      v(i) = ((vmom4(i)/v(i))-4.0*v3*vcent-6.0*sigma2*vcent2
     *               -vcent2*vcent2)/sigma2/sigma2 - 3.0
        else
c         Magic value for blanked pixel.
          v(i) = -99999.0
        endif
      enddo

      end

c***********************************************************************

      subroutine velmap(lIn, nx, ny, nc, ary, vlsr, v)

      integer   lIn, nx, ny, nc
      real      ary(nx,ny,nc), vlsr(nc), v(nx*ny)
c-----------------------------------------------------------------------
c       average velocity maps in window in velocity
c
c  Inputs:
c    ary        The image.
c    nx,ny,nc   Dimensions of image.
c    vlsr       Array of velocities.
c    v          velocity-averaged map.
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mirconst.h'
      include 'mem.h'

      integer vmom1,vmom2,vmom3,vmom4
      real vmax(128),vmin(128),xmin,xmax,ymin,ymax,vmean,vwidth
      integer imaps,nmaps,i,j,k,nmom,length
      integer windx,windy,sym,lwidth
      real cf,tr(6)
      character*1 key
      character*9 c1,c2
      character*30 label
      character*80 ans,xlabel,ylabel,olddev
      real sx(1),sy(1)
      real pat
      real xpts(2),ypts(2)
      real cmaj,cmin,cpa

      integer ncut
      real xcut(128),ycut(128),pa(128)
      common /cuts/ xcut,ycut,pa,ncut

      integer nspec,ngauss(49)
      real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr

      data imaps/0/
c-----------------------------------------------------------------------
      nmaps = 0
      do i = 1, nx*ny
        v(i)=0.0
      enddo

c     Introduction.
      call prompt(ans,length,'>Enter H for Help, <cr> to continue: ')
      call ucase(ans)
      if (ans(1:1).eq.'H') then
        call output(' ')
        call output('Plot x-y maps in selected velocity intervals.')
        call output('Velocity intervals can be entered as a list,')
        call output('or specified as a range of velocities at equal')
        call output('intervals. The list can be edited. Maps can be')
        call output('printed or written out for further processing.')
        call output('Single plots allow further cursor options.')
      endif

c     Get map type to be plotted.
      call getMom(nmom)

c     List velocities available and current selection of maps.
      call listMaps(imaps,vmin,vmax,vlsr,nc)

c     Get velocities to be plotted.
      call output(' ')
      call prompt(ans,length,
     *  '>Enter List or Range of velocity intervals to plot ([L]/R): ')

      call ucase(ans)
      call output(' ')

c     Enter List of velocity intervals or enter Range of velocities.
      if (ans(1:1).ne.'R') then
        call getList(imaps,vmin,vmax,vlsr,nc)
      else
        call getRange(imaps,vmin,vmax,vlsr,nc)
      endif

      if (imaps.eq.0) then
        call output(' ')
        call output('--- no velocity intervals selected ---')
        return
      endif

c     Save plot device type from command line.
      maptype = 'X-Y'
      olddev = device
      lwidth = 1

c     Loop through list of maps. Reset image vel and width.
      key='L'

60    vel  = vlsr(1)
      delv = vlsr(2) - vlsr(1)
      if (write.ne.'Y' .and. apint.ne.'Y') then
        call plPanels(imaps,windx,windy)
        call pgbeg(0,device,windx,windy)
c         call pgask(.FALSE.)
        call pgslw(lwidth)
      endif

c     Calculate the conversion factor plotted units.
      call header(0)
      cf = 1.0
      if (units.eq.'K') cf=dperjy

c     Average the sets of maps  - nmaps is the number of maps in the
c     average returned from velaver.
      do k = 1, imaps
        call output(' ')
        call output('---- Integrated velocity map ----')
        vmom1 = 1
        vmom2 = 1
        vmom3 = 1
        vmom4 = 1
        if (nmom.ge.1) call memAlloc(vmom1,nx*ny,'r')
        if (nmom.ge.2) call memAlloc(vmom2,nx*ny,'r')
        if (nmom.ge.3) call memAlloc(vmom3,nx*ny,'r')
        if (nmom.ge.4) call memAlloc(vmom4,nx*ny,'r')
        call velaver(vmin(k),vmax(k),vlsr,ary,nx,ny,nc,
     *                    nmom,v,nmaps,vmean,vwidth,
     *        memr(vmom1),memr(vmom2),memr(vmom3),memr(vmom4))
        if (nmom.ge.1) call memFree(vmom1,nx*ny,'r')
        if (nmom.ge.2) call memFree(vmom2,nx*ny,'r')
        if (nmom.ge.3) call memFree(vmom3,nx*ny,'r')
        if (nmom.ge.4) call memFree(vmom4,nx*ny,'r')

c       Put mean velocity and width into map header for this plot.
        vel  = vmean
        delv = vwidth

c       Plot or write out maps.
        if (nmaps.gt.0) then
          if (write.eq.'Y') then
            call writemap(lIn, nx, ny, 1, v)
          else if (apint.eq.'Y') then
            call plotint(v,nx,ny)
          else
c           Plot maps.
            call pgpage
            if (windx*windy.eq.1 .and. alabel.eq.'Y') then
              call pgsvp(0.1,0.7,0.1,0.9)
            else
              call pgsvp(0.1,0.9,0.1,0.9)
            endif

c           Plot the box.
c           if (abscoord.eq.'Y') then
c             Absolute coords are tricky since we want to keep the
c             equal pixel sizes in x and y.
c             call plLimits(0,nx,ny,xmin,xmax,ymin,ymax)
c             call pgwnad(xmin,xmax,ymin,ymax)
c             call pgtbox('BCNSTZ',0.0,0,'BCNSTZ',0.0,0)
c             xlabel='Right Ascension (1950)'
c             ylabel='Declination (1950)'
c             call plLimits(0,nx,ny,xmin,xmax,ymin,ymax)
c             call pgwnad(xmin,xmax,ymin,ymax)
c           else
c             Relative coords (only).
              call plLimits(0,nx,ny,xmin,xmax,ymin,ymax)
              call pgwnad(xmin,xmax,ymin,ymax)
              call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
              xlabel='Relative R.A. (arcsec)'
              ylabel='Relative Dec. (arcsec)'
c           endif

c           Set up transformation array for contour routine
c           PGPLOT instructions not intuitive.
            tr(2)=(xmax-xmin)/(nx-1)
            tr(1)=xmin-tr(2)
            tr(3)=0.0
            tr(5)=0.0
            tr(6)=(ymax-ymin)/(ny-1)
            tr(4)=ymin-tr(6)

c           Plot contours and greyscale (if required).
            call plotcon(v,nx,ny,cf,tr)

c           Plot labels.
            write(c1,130) vmin(k)
            write(c2,130) vmax(k)
130         format(f9.3)
            label = '                         '
            label = c1//' - '//c2
            call pglab(xlabel,ylabel,label)

c           Now plot the annotation (had to wait for plotcon to get the
c           info) and have to set window back.
            if (windx*windy.eq.1 .and. alabel.eq.'Y') then
              call pgsvp(0.72,0.9,0.1,0.9)
              call plAnnot(cf,cmaj,cmin,cpa)
              call pgsvp(0.1,0.7,0.1,0.9)
              call plLimits(0,nx,ny,xmin,xmax,ymin,ymax)
              call pgwnad(xmin,xmax,ymin,ymax)
            endif

c           Redraw box.
            call pgbox('BCNST',0.0,0,'BCNST',0.0,0)

c           Transpose RA axis for cursor and overlay.
            call pgwnad(-xmin,-xmax,ymin,ymax)

c           Plot spectra positions, if any.
            if (pspec.eq.'Y' .and. nspec.gt.0) then
              do j = 1, nspec
                sx(1)=xc(j)
                sy(1)=yc(j)
                sym=j+64
                call pgpt(1,sx,sy,sym)
              enddo
            endif

c           Plot pos-vel cuts, if any.
            if (pspec.eq.'Y' .and. ncut.gt.0) then
              do j = 1, ncut
                pat=-tan((90.0-pa(j))*D2R)
                xpts(1)=-1e4
                ypts(1)=-1e4*pat+(ycut(j)-xcut(j)*pat)
                xpts(2)=1e4
                ypts(2)=1e4*pat+(ycut(j)-xcut(j)*pat)
                call pgline(2,xpts,ypts)
                sx(1)=xcut(j)
                sy(1)=ycut(j)
                sym=j+96
                call pgpt(1,sx,sy,sym)
              enddo
            endif
            call pgwnad(xmin,xmax,ymin,ymax)

c           If one map and not hardcopy then call for cursor.
            call pgqinf('HARDCOPY',ans,length)
            if (ans.ne.'YES' .and. windx*windy.eq.1) then
              call pgwnad(-xmin,-xmax,ymin,ymax)
              call cursor(v,nx,ny,key)
              call pgwnad(xmin,xmax,ymin,ymax)
              if (key.eq.'E') goto 66
              goto 60
            endif
          endif
66      endif
      enddo

c     Finished plotting maps; replotting options.
      if (write.ne.'Y' .and. apint.ne.'Y') then
        if (alabel.eq.'Y') call pgiden
        call pgqinf('HARDCOPY',ans,length)
        call pgend
        if (ans.ne.'YES') then
          call output(' ')
          call prompt(ans,length,'Another PGPLOT device? (Y/[N])')
          call ucase(ans)
          if (ans(1:1).eq.'Y') then
            call prompt(device,length,'>Enter new PGPLOT device: ')
            call lcase(device)
            call prompt(ans,length,'>Enter line width [1]: ')
            if (length.ne.0) then
              read(ans,'(i1)') lwidth
            endif
            goto 60
          endif
        endif
      endif

c     Restore original plot device, and image vel and width.
      device = olddev
      vel  = vlsr(1)
      delv = vlsr(2) - vlsr(1)

      end

c***********************************************************************

      subroutine velohead(nx,ny,nc)
      integer nx,ny,nc
c-----------------------------------------------------------------------
c  Header and velocity information
c                    mchw 9 nov 1987
c-----------------------------------------------------------------------
      include 'velplot.h'

      character msg*80
c-----------------------------------------------------------------------
      call output(' ')
      call output('Header and velocity information')
      call output('Image file: '//file)
      call header(5)
      write(msg, *) ' map pixels (L,R,B,T)=',1-brpix(1),nx-brpix(1),
     *                                       1-brpix(2),ny-brpix(2)
      call output(msg)
      write(msg, *) ' Array dimensions are: nx,ny,nc=',nx,ny,nc
      call output(msg)
      call LogWrit('Image File: '//file)
      call header(6)

      end

c***********************************************************************

      subroutine veloint(vmin,vmax,vel,velint,wt)

      real vmin,vmax,vel,velint,wt
c-----------------------------------------------------------------------
c  Find weight wt of point with velocity vel in interval (vmin,vmax)
c  Inputs:
c    vmin,vmax,velint   Velocity range and sample interval
c    vel                Requested velocity.
c  Outputs:
c    wt                 Weight for velocity vel.
c-----------------------------------------------------------------------
      real vchn,vbot,vtop,vlo,vhi
c-----------------------------------------------------------------------
      vchn = abs(velint)
c -- velocity range included in interpolation
      vbot = vmin - vchn
      vtop = vmax + vchn
c -- allow 5% capture range without interpolation
      vlo = vmin - 0.05*vchn
      vhi = vmax + 0.05*vchn

      if (vel.ge.vlo .and. vel.le.vhi) then
        wt = 1.0
      else if (vel.gt.vhi .and. vel.lt.vtop) then
        wt = (vtop-vel)/vchn
      else if (vel.gt.vbot .and. vel.lt.vlo) then
        wt = (vel-vbot)/vchn
      else
        wt = 0.0
      endif

      end

c***********************************************************************

      subroutine veloline(ary,nx,ny,nc,xc,yc,pa,ncon,con,
     *                                         v,np,xstart,xend)

      integer nx,ny,nc,ncon,np
      real ary(nx,ny,nc)
      real xc,yc,pa,xstart,xend
      real v(nx*ny)
      real con(ncon,ncon,4,4)
c-----------------------------------------------------------------------
c  Routine that does all the work for velocity-position maps
c  The algorithm in this routine only works for 0<pa<180
c
c  Inputs:
c    ary(nx,ny,nc)      The array to be convolved.
c    xc,yc,pa           The center and pa of the positon velocity plot
c    ncon               Size of convolution array
c    con                The convolution array
c
c  Outputs:
c    v                  The convolved position-velocity array
c    np                 number of positions in position-velocity array
c    xstart,xend        Limits for plot
c-----------------------------------------------------------------------
      include 'velplot.h'
      include 'mirconst.h'

      integer ix,iy,n,k,m
      real xinc,yinc,x0,y0,step,x1,y1,c,xl,yl,xr,yr
      real wt,fi,fj
      real ms,conary(MAXDIM)
      character msg*80
c-----------------------------------------------------------------------
      ix = nint(xc/xy) + brpix(1)
      iy = nint(yc/xy) + brpix(2)
      if (ix.lt.1 .or. ix.gt.nx .or. iy.lt.1 .or. iy.gt.ny) then
        call output('pos-vel requested center is outside array')
        return
      endif

c     Set variables for labelling box (for single map).
      posx = xc
      posy = yc
      pospa = pa

c     Find the points where this line goes outside the box.
      if (pa.eq.45.0) pa=44.999
      if (pa.eq.90.0) then
        x1 = (bblc(1) - brpix(1))*xy
        y1 = yc
        np = nx
        step = xy
        x0 = (nx - brpix(1))*xy
        y0 = yc
      else if (pa.eq.0.0) then
        x1 = xc
        y1 = (ny - brpix(2))*xy
        np = ny
        step = xy
        x0 = xc
        y0 = (1 - brpix(2))*xy
      else
c       General case y = ms*x + c; yl is the intercept on the left edge,
c       and xl is the intercept on the bottom.
        ms = -1.0/tan(pa*D2R)
        c = yc - (ms*xc)
        yl = (bblc(1)-brpix(1))*xy*ms + c
        xl = ((1-brpix(2))*xy - c)/ms

c       Test to see when line leaves box and set x1,y1 to this point.
        x1 = (bblc(1)-brpix(1))*xy
        y1 = (1-brpix(2))*xy
        if (yl.gt.(1-brpix(2))*xy .and. yl.le.(ny-brpix(2))*xy) then
c         Line intercepts left side of box.
          y1 = yl
        else if (yl.lt.(1-brpix(2))*xy) then
c         Line intercepts bottom of box.
          x1 = xl
        else if (yl.gt.(ny-brpix(2))*xy) then
c         Line intercepts top of box.
          x1 = ((ny-brpix(2))*xy - c)/ms
          y1 = (ny-brpix(2))*xy
        endif

c       x1,y1 is the end of the line in arcsec.  Now find the start of
c       the line so that the length and step size can be found.  xr and
c       yr are the intercepts on the right and bottom.
        xr = ((ny-brpix(2))*xy - c)/ms
        yr = (nx-brpix(1))*xy*ms + c

c       Test to see when line enters box and set x0,y0 to this point.
        x0 = (nx-brpix(1))*xy
        y0 = (ny-brpix(2))*xy
        if (yr.gt.(1-brpix(2))*xy .and. yr.le.(ny-brpix(2))*xy) then
c         Line intercepts right side of box.
          y0 = yr
        else if (yr.lt.(1-brpix(2))*xy) then
c         Line intercepts bottom of box.
          x0 = ((1-brpix(2))*xy - c)/ms
          y0 = (1-brpix(2))*xy
        else if (yr.gt.(ny-brpix(2))*xy) then
c         Line intercepts top of box.
          x0 = xr
        endif

c       Set step size.
        np = max(nx,ny)
        step = sqrt((x0-x1)**2.0 + (y0-y1)**2.0)/(np-1)
      endif

      write(msg, 101) x0,y0
101   format(' Line enters box at (x,y) ',f8.2,' ',f8.2,' arcsec')
      call output(msg)
      write(msg, 102) x1,y1
102   format(' Line leaves box at (x,y) ',f8.2,' ',f8.2,' arcsec')
      call output(msg)

c     Set up limits for plot label.
      xstart = -1.0*sqrt((x0-xc)**2.0 + (y0-yc)**2.0)
      xend = sqrt((x1-xc)**2.0 + (y1-yc)**2.0)

c     Step along line.
      xinc = -sin(pa*D2R)
      yinc =  cos(pa*D2R)
      x0 = x0 - xinc * step
      y0 = y0 - yinc * step
      do n = 1, np
        x0 = x0 + xinc * step
        y0 = y0 + yinc * step

c       Convert arcsec to pixel coordinates.
        fi = (x0/xy) + brpix(1)
        fj = (y0/xy) + brpix(2)

c       Is the point within the array?
        if ((fi-1).ge.-1e-4 .and. (fi-nx).le.1e-4 .and.
     *      (fj-1).ge.-1e-4 .and. (fj-ny).le.1e-4) then
c         Convolve array; spectrum is in conary.
          call conv(ary,nx,ny,nc,fi,fj,ncon,con,conary,wt)
          do k = 1, nc
            m = k + (n-1)*nc
            v(m) = conary(k)
          enddo
        else
          do k = 1, nc
            m = k + (n-1)*nc
            v(m) = 0.0
          enddo
        endif
      enddo

      end

c***********************************************************************

      subroutine wrary

c-----------------------------------------------------------------------
c  Write file of spectra & position-velocity cuts.
c-----------------------------------------------------------------------
      integer lu,iostat,len1,n,length
      character*80 file,line

      integer ncut
      real xcut(128),ycut(128),pa(128)
      common /cuts/ xcut,ycut,pa,ncut

      integer nspec,ngauss(49)
      real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c-----------------------------------------------------------------------
      if (nspec.eq.0 .and. ncut.eq.0) then
        call bug('w', 'No spectra or position-velocity cuts in lists.')
        return
      endif
c
c  Open output file.
c
      call prompt(file,length,
     *    'File for spectra & position-velocity cuts: ')
      call txtopen(lu,file,'new',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening ascii file')
        return
      endif

      call txtwrite(lu,file,len1(file),iostat)
      write(line,'(a)')
     *        'File for spectra & position-velocity cuts.'
      call txtwrite(lu,line,len1(line),iostat)
c
c  Write spectra positions.
c
      write(line,'(i4,a)')
     *  nspec, '  spectra positions (HA,DEC) arcsec w.r.t. crpix'
      call txtwrite(lu,line,len1(line),iostat)
      if (nspec.ne.0) then
        do n = 1, nspec
          write(line,'(2f20.4)') xc(n),yc(n)
          call txtwrite(lu,line,40,iostat)
        enddo
      endif
c
c  Write position-velocity cuts.
c
      write(line,'(i4,a)')
     *  ncut, '  position-velocity cuts HA,DEC (") & pa (degrees)'
      call txtwrite(lu,line,len1(line),iostat)
      if (ncut.ne.0) then
        do n = 1, ncut
          write(line,'(3f20.4)') xcut(n),ycut(n),pa(n)
          call txtwrite(lu,line,60,iostat)
        enddo
      endif
c
c  Close output file and tell user.
c
      call txtclose(lu)
      write(line,'(i3,a,i3,a,a)') nspec,' spectra and ',ncut,
     *  ' position-velocity cuts written to ',file(1:len1(file))
      call output(line(1:len1(line)))

      end

c***********************************************************************

      subroutine wrGauss(outfile,nc,vlsr,cmaj,cmin,cpa)

c-----------------------------------------------------------------------
c  Write Gaussian fits to spectra into an ascii file.
c
c  Inputs:
c    outfile    filename for ascii fits
c    nc         number of channels
c    vlsr       velocity array
c    ngauss     number of simultaneously fitted gaussians
c    gauss      gaussian parameters:
c                 gauss(i,1) amplitude of gaussian i
c                 gauss(i,2) center velocity of gaussian i
c                 gauss(i,3) sigma of gaussian i
c-----------------------------------------------------------------------
      include 'velplot.h'

      integer   MAXMOD
      parameter (MAXMOD = 30)

      integer   i, iostat, j, k, length, lu, nc, ns
      real      arg1, cmaj, cmin, cpa, dv, term, tmod(MAXDIM,MAXMOD),
     *          tmod2(5*MAXDIM,MAXMOD), vlsr(nc), vlsr2(5*MAXDIM)
      character outfile*80, text*310

      integer   nspec, ngauss(49)
      real      xc(49), yc(49), gauss(49,3,10), gausserr(49,3,10)
      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c-----------------------------------------------------------------------
c     Loop through spectra to form gaussian models.
      ns = min(nspec,MAXMOD)
      do k = 1, ns
c       Loop through channels.
        do i = 1, nc
          tmod(i,k) = 0.0
          do j = 1, ngauss(k)
            arg1=(vlsr(i)-gauss(k,2,j))/gauss(k,3,j)
            if (abs(arg1).gt.4.0) then
              term=0.0
            else
              term=gauss(k,1,j)*exp(-arg1*arg1)
            endif
            tmod(i,k) = tmod(i,k) + term
          enddo
        enddo

c       Form new gaussian model spectrum with factor of 5 oversampling.
        dv=(vlsr(2)-vlsr(1))/5.0
        do i = 1, nc*5
          vlsr2(i)=vlsr(1)+(real(i)*dv)
          tmod2(i,k) = 0.0
          do j = 1, ngauss(k)
            arg1=(vlsr2(i)-gauss(k,2,j))/gauss(k,3,j)
            if (abs(arg1).gt.4.0) then
              term=0.0
            else
              term=gauss(k,1,j)*exp(-arg1*arg1)
            endif
            tmod2(i,k) = tmod2(i,k) + term
          enddo
        enddo
      enddo

c     Write out gaussian fits to ASCII file.
      if (nspec.gt.MAXMOD) then
        write(text,10) MAXMOD, outfile
 10     format('only first',i3,' fits written to ',a)
      endif
      call TxtOpen(lu,outfile,'new',iostat)
        call TxtOpen(lu,outfile,'new',iostat)
        if (iostat.eq.0) write(text,'(a,a)')
     *    'File: ',outfile
        length=6 + len(file)
        call TxtWrite(lu,text,length,iostat)
        if (iostat.eq.0) write(text,'(a,a)')
     *    'Image File: ',file
        length=12 + len(file)
        call TxtWrite(lu,text,length,iostat)
      if (iostat.eq.0) write(text,'(a,a,4x,f11.6,a)')
     *  'Gau fit for ',object,restfreq,' GHz'
      length=12 + len(object) + 15 + 4
      call TxtWrite(lu,text,length,iostat)
      if (iostat.eq.0) write(text,'(a,2f12.3,a,f6.1,a)')
     *'Convolving beam: ',cmaj,cmin,' arcsecs, pa: ',cpa,' degrees'
      length = 17 + 12 + 12 + 14 + 6 + 8
      call TxtWrite(lu,text,length,iostat)
      if (iostat.eq.0) write(text,'(a)') 'Positions (x,y) arcsecs'
      call TxtWrite(lu,text,23,iostat)
      k=0
      do while (iostat.eq.0 .and. k.lt.ns)
        write(text,'(4(a,2f7.2,a))')
     *    ('(',xc(i+k),yc(i+k),') ',i=1,min(4,ns-k))
        length = min(4,ns-k)*(1+2*7+2)
        call TxtWrite(lu,text,length,iostat)
        k=k+4
      enddo
      do while (length.gt.0 .and. iostat.eq.0)
        call prompt(text,length,'Enter comment: ')
        call TxtWrite(lu,text,length,iostat)
      enddo
      k=1
      do while ((iostat.eq.0) .and. (k.le.(5*nc)))
        write(text,'(31(1x,f9.3))') vlsr2(k),(tmod2(k,i),i=1,ns)
        call TxtWrite(lu,text,10+10*ns,iostat)
        k=k+1
      enddo
      call TxtClose(lu)

      end

c***********************************************************************

      subroutine writeint(v,nc,np,xstart,xend,cf,nchan,ichan,
     *     x0,y0,pa,vlsr)

      integer nc,np,nchan,ichan(10),ii,jj,jjj
      integer lu,length,iostat
      real v(nc*np),cf,xstart,xend,xdelta,x0,y0,pa,vlsr(nc)
c-----------------------------------------------------------------------
c  Integer plot.
c
c  Inputs:
c    v          The array to print.
c    nc*np      Dimensions of array.
c-----------------------------------------------------------------------
      include 'velplot.h'

      real      xcoord, xval(MAXDIM)
      character line*80, outfile*80, text*120
c-----------------------------------------------------------------------
c     Write out channel values to ASCII file.
      line='>Enter name for ASCII file (<cr> to continue): '
      call prompt(outfile,length,line)
      if (length.gt.0) then
        call TxtOpen(lu,outfile,'new',iostat)
        if (iostat.eq.0) then
c         Title.
          write(text,'(''Image: '',a)') file
          length=7 + len(file)
          if (iostat.eq.0) call TxtWrite(lu,text,length,iostat)

          write(text, '(''source: '',a,''  map units: '',a)')
     *      object,bunit
          length= 8 + len(object) + 13 + len(bunit)
          if (iostat.eq.0) call TxtWrite(lu,text,length,iostat)

          write(text, '(''Map values have been divided by: '',f9.3)') cf
          length=33 + 9
          if (iostat.eq.0) call TxtWrite(lu,text,length,iostat)

          write(text, '(''Cut centered at: '',
     *      f9.3,'','',f9.3,''  PA: '',f9.3)') x0,y0,pa
          length = 17 + 9 + 1 + 9 + 7 + 9
          if (iostat.eq.0) call TxtWrite(lu,text,length,iostat)

          write(text, '(''Channels listed are (km/s): '')')
          length=29
          if (iostat.eq.0) call TxtWrite(lu,text,length,iostat)

          write(text, '(2x,5(1x,f9.3))') (vlsr(ichan(ii)),ii=1,nchan)
          length=52
          if (iostat.eq.0) call TxtWrite(lu,text,length,iostat)

c         text = 'The columns are: offset from center (arcsec), and ' //
c    *           'channel values.'
c         length=64
c         if (iostat.eq.0) call TxtWrite(lu,text,length,iostat)

          if (cf.eq.0.0) cf=1.0
          xdelta=(xstart-xend)/real(np-1)
          if (xstart.lt.xend) xdelta=abs(xdelta)
          if (xstart.gt.xend) xdelta=-1.0*abs(xdelta)
          do jj = 1, np
            do ii = 1, nchan
              jjj=(jj-1)*nc+ichan(ii)
              xval(ii)=v(jjj)/cf
            enddo
            xcoord=xstart+real(jj-1)*xdelta
            write(text,'(1x,6(f9.3,1x))') xcoord,(xval(ii),ii=1,nchan)
            length=61
            if (iostat.eq.0) call TxtWrite(lu,text,length,iostat)
          enddo
          call TxtClose(lu)
        endif
      endif

      end

c***********************************************************************

      subroutine writemap(lIn, nx, ny, nc, ary)

      integer   lIn, nx, ny, nc
      real      ary(*)
c-----------------------------------------------------------------------
c  Write out Miriad Image.
c
c  Inputs:
c    nx ny nc           dimensions of image
c    ary(nx,ny,nc)      spectral line image
c-----------------------------------------------------------------------
      include 'velplot.h'

      integer   ipt, j, k, length, lOut, nsize(3)
      character filename*80, text*80
c-----------------------------------------------------------------------
c     Open output file and write header from values in common.
      call prompt(filename,length,'Enter output name: ')
      if (length.le.0) return
      nsize(1) = nx
      nsize(2) = ny
      nsize(3) = nc
      call xyopen(lOut, filename, 'new', 3, nsize)

c     Copy the header verbatim then apply updates.
      call headcp(lIn, lOut, 3, 0, blc, 0)
      call wrhdd(lOut, 'crpix1', dble(brpix(1)))
      call wrhdd(lOut, 'crpix2', dble(brpix(2)))
      call wrhdd(lOut, 'crpix3', 1d0)
      call wrhdd(lOut, 'cdelt3', dble(delv))
      call wrhdd(lOut, 'crval3', dble(vel))

c     Write it out.
      call output('writing output Image to file: '//filename)
      ipt = 1
      do k = 1, nc
        call xysetpl(lOut,1,k)
        do j = 1, ny
          call xywrite(lOut,j,ary(ipt))
          ipt = ipt + nx
        enddo
      enddo

c     Write the history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'VELPLOT:')
      call hisinput(lOut, 'VELPLOT')

      call hiswrite(lOut, 'VELPLOT: Integrated-velocity map')

      write(text,'(a,f12.3,a)') 'VELPLOT: Integrated over ',delv,' km/s'
      call hiswrite(lOut,text)

      call hisclose(lOut)

      call xyclose(lOut)

      end

c***********************************************************************

      subroutine writeposvel(nx, ny, nc, ary, crval1, crval2, crval3,
     *  crpix1, crpix2, cdelt1, cdelt2)

      integer   nx, ny, nc
      real      ary(*), crval1, crval2, crval3, crpix1, crpix2, cdelt1,
     *          cdelt2
c-----------------------------------------------------------------------
c  Write out position-velocity image.
c
c  Inputs:
c    ary(nx,ny,nc)      spectral line image
c    nx ny nc           dimensions of image
c    crval1,2,3
c    crpix1,2
c    cdelt1,2
c-----------------------------------------------------------------------
      include 'velplot.h'

      integer   ipt, j, k, length, lOut, nsize(3)
      character filename*80, text*80
c-----------------------------------------------------------------------
c     Open output file and write header from values in common.
      call prompt(filename,length,'Enter output name: ')
      if (length.le.0) return
      nsize(1) = nx
      nsize(2) = ny
      nsize(3) = nc
      call xyopen(lOut,filename,'new',3,nsize)

      call wrhda(lOut, 'ctype1', ctype(3))
      call wrhda(lOut, 'ctype2', 'POSITION')
      call wrhda(lOut, 'ctype3', 'ANGLE')
      call wrhdr(lOut, 'crval1', crval1)
      call wrhdr(lOut, 'crval2', crval2)
      call wrhdr(lOut, 'crval3', crval3)
      call wrhdr(lOut, 'crpix1', crpix1)
      call wrhdr(lOut, 'crpix2', crpix2)
      call wrhdr(lOut, 'crpix3', 1.0)
      call wrhdr(lOut, 'cdelt1', cdelt1)
      call wrhdr(lOut, 'cdelt2', cdelt2)
      call wrhdr(lOut, 'cdelt3', delv)

      call wrhda(lOut, 'bunit', bunit)
      call wrhdr(lOut, 'bmaj',  bmaj)
      call wrhdr(lOut, 'bmin',  bmin)
      call wrhdr(lOut, 'bpa',   bpa)
      call wrhdr(lOut, 'epoch', epoch)
      call wrhdi(lOut, 'niters', niters)
      call wrhda(lOut, 'object', object)
      call wrhdr(lOut, 'restfreq', restfreq)

c     Write it.
      call output('writing output Image to file: '//filename)
      ipt = 1
      do k = 1, nc
        call xysetpl(lOut,1,k)
        do j = 1, ny
          call xywrite(lOut,j,ary(ipt))
          ipt = ipt + nx
        enddo
      enddo

c     Write the history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'VELPLOT:')
      call hisinput(lOut, 'VELPLOT')

      call hiswrite(lOut, 'VELPLOT: Position-velocity cut')

      write(text,10) posx, posy, pospa
 10   format('VELPLOT: Center (x,y)=(',f8.3,',',f8.3,') arcsec',
     *       '  Position-angle=',f5.1,' degrees')
      call hiswrite(lOut, text)

      call hisclose(lOut)

      call xyclose(lOut)

      end
