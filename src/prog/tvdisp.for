c************************************************************************
      program tvdisp
      implicit none
c
c= tvdisp - Display an image on a TV device
c& jm
c: visual display
c+
c	TVDISP is a MIRIAD task to display an image on a TV device.
c	TVDISP does not clear the screen before it initializes the TV;
c	see TVINIT for this function.
c< in
c
c< region
c
c	NOTE: This region only permits simple bounding boxes.
c	The default is to display the entire image.
c
c@ range
c	The minimum and maximum range used in scaling. The default is
c	to use the minimum and maximum values of the image. 
c
c@ tvchan
c	The TV channel to display the image on.  This can be 1 to 3 on
c	the IVAS; 1 to 2 on the MXAS or XMTV servers.  The default is 1. 
c
c@ incr
c	Only every INCR'th pixel is displayed.  This can take from 1
c	to up to 3 values, giving the increment along the x, y, and z
c	axis, respectively. The default is chosen such that the selected
c	region of each image will fit on the screen. 
c
c@ tvcorn
c	This gives the coordinate where the lower left corner of the
c	image is displayed.  The coordinate system typically runs from
c	1 to 1024 in x and y, the origin being in the lower left corner.
c	The default is to center the image on the screen. 
c
c< server
c
c	There is no default for the server. 
c
c@ options
c	These indicate extra operations that may be performed. One or
c	more can be given.  Possible values are: 
c
c	  fiddle  Allow interactive modification of colour tables, zoom,
c	          etc.  NOTE: The Fiddle option currently only works
c	          with the IVAS servers!
c	  zoom    Zoom in on the displayed images.  NOTE: Zoom attempts
c	          to display the bounding box of all of the displayed
c	          images.  If more than one channel was loaded, then
c	          the zoomed display will be of the first channel.
c	  movie   Display the sequence of images as a movie.
c	          NOTE: Movie will attempt to zoom onto one image and
c	          then center the display step by step through each
c	          following image (and then return back to the first
c	          image to begin again).  The rate at which the images
c	          will change is a function of the cursor's X position.
c	          The further to the left of the display that the cursor
c	          is located, the faster the rate of change; the further
c	          right, the slower.  NOTE: some displays require a
c	          mouse button press to identify the cursor position.
c	          Functions to control the direction and style of motion
c	          are available if the panel is present.  If not, a
c	          subset of functions will be available to the four
c	          functions A - D (may also be keys F3 - F6).
c--
c
c
c
c  History:
c   rjs       89 - Initial version
c   nebk 29apr89 - Movies can span across TV channels.
c   rjs   7jun89 - Work around for devices without cursors.
c   rjs   7nov89 - added 's' flag to BoxSet.
c   rjs  30apr90 - Changed call sequence to BoxInput.
c   pjt   2may90 - maxchan -> mxchan because of new maxdim.h
c   rjs   4mar91 - Added zoom option. Some tidying.
c    jm  04jul92 - Added call to tvscale.
c    jm  18jul92 - Added lots of documentation and fixed a MAXPL bug.
c                  Added an extra call to TvCursor inside movie to make
c                  sure that the buttons are reset prior to beginning the
c                  movie.  Also tried to clean up the code a bit.
c   rjs  23jul92 - Fixed up documentation.
c   rjs  21sep93 - Display blanked pixels as the background colour.
c   mjs  30sep93 - call delay -> call delayf
c   rjs  13jan93 - was not getting the mask correct when incr != 1.
c    jm  01jun94 - Substantial modification of movie subroutine to allow
c                  the control panel to be used to execute the multiple
c                  functions that have also been added.  Also added a
c                  subset of commands to the four button functions (A-D)
c                  in case the panel is not present.  This also included
c                  changes to the main routine to open and close the
c                  panel as needed.  Also corrected the documentation to
c                  clarify that only a simple bounding box is acceptable
c                  as an input region.
c   rjs  20dec95 - Tidy up movie subroutine.
c   rjs  05dec96 - Improve delay logical in movie sequence.
c------------------------------------------------------------------------
      include 'maxdim.h'
      integer MAXPL, MAXBOXES
      parameter(MAXPL=256, MAXBOXES=2048)
c
      character in*132, server*132, msg*132
      integer lIn
      integer i,j,k,kl,i0,j0,k0,jx0,jy0,jx,jy,nx,ny,nz,nxp,nyp,kk,nxy
      integer incx,incy,incz,widthx,widthy
      integer achan,channel,temp,imx,imy
      integer xpix, ypix, zpix, levels, mxchan
      integer blcx, blcy, blcz, trcx, trcy, trcz, xdim, ydim, zdim
      integer nsize(3),blc(3),trc(3)
      integer idat(MAXDIM)
      integer xcoord(MAXPL), ycoord(MAXPL), chans(MAXPL)
      integer boxes(MAXBOXES)
      real minpix, maxpix, bscale, bzero
      real rdat(MAXDIM)
      logical default,dolocal,domovie,dozoom
      logical flags(MAXDIM)
c
c  Externals.
c
      integer Len1
      integer nextpow2
      logical keyprsnt

      call output('Tvdisp: Version 2.0 01-Jun-94')
c
c  Get the parameters.
c
      call keyini
      call keya('in', In, ' ')
      call BoxInput('region',in,boxes,MAXBOXES)
      default = .not.keyprsnt('range')
      call keyr('range', minpix, 0.0)
      call keyr('range', maxpix, 0.0)
      call keyi('tvchan', channel, 1)
      call keyi('incr', incx, 0)
      call keyi('incr', incy, incx)
      call keyi('incr', incz, 0)
      call keyi('tvcorn', jx0, 0)
      call keyi('tvcorn', jy0, 0)
      call keya('server', server, ' ')
      call GetOpt(domovie,dolocal,dozoom)
      call keyfin
c
      if(server.eq.' ')
     *  call bug('f','A TV device/server name must be provided.')
c
c  Open the input file.
c
      call xyopen (lIn, In, 'old', 3, nsize)
      if(nsize(1).gt.MAXDIM)
     *  call bug('f','Image too large for internal buffer.')
      xdim = nsize(1)
      ydim = nsize(2)
      zdim = nsize(3)
c
c  Determine the region of interest.
c
      call BoxSet(boxes,3,nsize,'s')
      call BoxInfo(boxes,3,blc,trc)
      blcx = blc(1)
      blcy = blc(2)
      blcz = blc(3)
      trcx = trc(1)
      trcy = trc(2)
      trcz = trc(3)
c
c  Open the connection to the display server, get device
c  characteristics, and determine the active channel.
c
      call tvopen(server)
      call tvchar(xpix, ypix, mxchan,levels)
      achan = min(max(channel,1), mxchan)
c
c  Set up defaults when neither window nor increments set
c
      call fillin(blcx,trcx,incx,xpix,xdim,nx)
      call fillin(blcy,trcy,incy,ypix,ydim,ny)
      if((nx.lt.2).or.(ny.lt.2))
     *  call bug('f','No image to display')
      nx = 2 * (nx / 2)
c
c  Pad the x and y dimensions to the next power of 2 larger.
c
      nxp = nextpow2(nx)
      nxp = min(nxp,xpix)
      nyp = nextpow2(ny)
      nyp = min(nyp,ypix)
c
c  Find the number of images that we can fit into the memory of the beast.
c  Determine the z blc,trc etc.
c  The z dimension is treated as the total number of images that
c  can be fitted into all available TV channels, rather than
c  just the number of TV channels.
c
      zpix = mxchan * (xpix/nxp) * (ypix/nyp)
      zpix = min(MAXPL, zpix)
      call fillin(blcz,trcz,incz,zpix,zdim,nz)
c
c  Determine the number to put across and the number to put down the screen.
c
      call imageper(nz,xpix/nxp,ypix/nyp,imx,imy)
c
      widthx = nxp*(imx-1) + nx
      if (jx0.le.0 .or. jx0+widthx-1.gt.xpix) then
        jx0 = (xpix - widthx) / 2
      else
        jx0 = jx0 - 1
      end if
      jx0 = 2 * (jx0 / 2)
c
      widthy = nyp*(imy-1) + ny
      if (jy0.le.0 .or. jy0+widthy-1.gt.ypix) then
        jy0 = (ypix - widthy) / 2
      else
        jy0 = jy0 - 1
      endif
c
      if (default)
     *  call ImMinMax(lIn, 3, nsize, minpix, maxpix)
      if(maxpix.eq.minpix)
     *  call bug('f','Min and max of pixel values are the same')
      levels = levels - 1
      bscale = levels / (maxpix - minpix)
      bzero  = minpix
      call tvscale(bzero, bscale)
c
      msg = 'Displaying image: ' // In(1:Len1(In))
      call Output(msg)
      call rangemsg(msg, 'X', blcx, trcx, incx)
      call Output(msg)
      call rangemsg(msg, 'Y', blcy, trcy, incy)
      call Output(msg)
      call rangemsg(msg, 'Z', blcz, trcz, incz)
      call Output(msg)
c
c  Loop through the image displaying it.  kk counts the number
c  of images displayed on the current active TV channel.  k
c  counts the total number of images to load.
c
      widthx = widthx - nx
      widthy = widthy - ny
      nxy = imx * imy
      call tvchan(achan)
      k0 = blcz
      kk = 1
      do k=1,nz
        call xysetpl(lIn,1,k0)
        j0 = blcy
        kl = (kk-1)/imx
        if(2*(kl/2).eq.kl)then
          jx = jx0 + (mod(kk-1,imx) * nxp)
        else
          jx = jx0 + widthx - (mod(kk-1,imx) * nxp)
        endif
        jy = jy0 + widthy - (kl * nyp)
        xcoord(k) = jx
        ycoord(k) = jy
        chans(k) = achan
        do j=1,ny
          call xyread(lIn, j0, rdat)
          call xyflgrd(lIn, j0, flags)
          i0 = blcx
          do i = 1, nx
            if (flags(i0)) then
              temp = bscale * (rdat(i0) - bzero)
              idat(i) = min(max(0,temp), levels)
            else
              idat(i) = 0
            endif
            i0 = i0 + incx
          enddo
          call tvline(jx, jy, achan, idat, nx)
          jy = jy + 1
          j0 = j0 + incy
        enddo
        k0 = k0 + incz
c
        if((mod(k,nxy).eq.0) .and. (k.ne.nz)) then
c
c  We have filled up one complete TV channel and still want to see more
c  images; so turn on the next channel and reset some counters.
c
          kk = 1
          achan = achan + 1
          if(achan.gt.mxchan) achan = 1
          call tvchan(achan)
        else
          kk = kk + 1
        endif
      enddo
c
c  Perform requested options, if any.
c
      if(dolocal) call tvlocal
      if(nz.gt.1.and.domovie) then
        call movie(xcoord,ycoord,chans,nx,ny,nz,xpix,ypix,server)
      else if(domovie) then
        call bug('w','Movie option ignored for one image.')
      endif
      if(dozoom) then
        call tvchan(min(max(channel,1), mxchan))
        call zoomit(xcoord,ycoord,nx,ny,nz)
      endif
c
c  Close down now.
c
      call tvclose
      call xyclose(lIn)
      end
c************************************************************************
      subroutine GetOpt(domovie,dolocal,dozoom)
c
      implicit none
      logical domovie,dolocal,dozoom
c
c  Determine processing options.
c
c  Output:
c    domovie      True if to run a movie at the end.
c    dozoom      True if to leave the screen zoomed.
c    dolocal      True if to enter "local" mode.
c------------------------------------------------------------------------
      integer NOPTS
      parameter(NOPTS=3)
c
      logical present(NOPTS)
      character opts(NOPTS)*8
c
      data opts /'fiddle  ', 'movie   ', 'zoom    '/
c
      call options('options',opts,present,NOPTS)
      dolocal = present(1)
      domovie = present(2)
      dozoom  = present(3)
c
      return
      end
c************************************************************************
      subroutine rangemsg(msg, axis, blc, trc, inc)
      implicit none
      character msg*(*), axis*1
      integer blc, trc, inc
c
c  Formats a message describing the range of the image loaded.
c
c  Inputs:
c    blc      Lower left corner of axis.
c    trc      Upper right corner of axis.
c    inc      Incremental step.
c  Outputs:
c    msg      String with formatted message.
c
c------------------------------------------------------------------------
c
      character bstr*8, tstr*8, istr*8
      integer blen, tlen, ilen
c
c  Externals.
c
      character itoaf*8
      integer Len1
c
      bstr = itoaf(blc)
      blen = Len1(bstr)
      tstr = itoaf(trc)
      tlen = Len1(tstr)
      istr = itoaf(inc)
      ilen = Len1(istr)
c
      write(msg, 10) axis, bstr(1:blen), tstr(1:tlen), istr(1:ilen)
   10   format(A, ' Range: ', A, '-', A, ' every ', A, ' pixel.')
c
      return
      end
c************************************************************************
      subroutine imageper(nim,imxmax,imymax,imx,imy)
c
      implicit none
      integer nim,imxmax,imymax,imx,imy
c
c  Determine the number of images to place along the x and y directions.
c  Choose the numbers so that the display comes out roughly square.
c
c  Input:
c    nim      Number of images.
c    imxmax      Max number of images in x.
c    imymax      Max number of images in y.
c
c  Output:
c    imx      Images along x.
c    imy      Images down y.
c
c------------------------------------------------------------------------
      if(nim.ge.imxmax*imymax)then
        imx = imxmax
        imy = imymax
      else
        imx = 1
        imy = 1
        dowhile(imy*imx.lt.nim)
          imx = min(imx+1,imxmax)
          imy = min(imy+1,imymax)
        enddo
      endif
      return
      end
c************************************************************************
      subroutine fillin(blc,trc,inc,pix,dims,n)
c
      implicit none
      integer blc,trc,inc,pix,dims,n
c
c  Fill in the blc, trc and inc for current dimension
c
c  Note that blc and trc will never be 0 for the way TVDISP is
c  currently written, so the first two IF sections will never
c  be accessd.
c
c  Input:
c    dims      Size of image
c    pix      Size of TV
c  Input/output:
c    blc,trc  Image window
c    inc      Image increment
c  Output:
c    n        Number of pixels to load on TV from image
c
c------------------------------------------------------------------------
      integer c,pix2
c
c  If no blc,trc or inc, adjust inc to fix whole image on screen.
c
      if(blc.eq.0 .and. trc.eq.0 .and. inc.eq.0)then
        blc = 1
        trc = dims
        inc = (trc - blc) / pix + 1
c
c  If only a inc, adjust blc and trc to fit something on screen.
c
      else if(blc.eq.0 .and. trc.eq.0 .and. inc.gt.0)then
        c = dims / 2
        pix2 = pix / 2
        blc = max(1, c - (pix2 * inc) + 1)
        trc = min(dims, c + (pix2 * inc))
c
c  If no inc (but blc and trc), set inc to fix something on screen.
c
      else if(blc.ne.0 .and. trc.ne.0 .and. inc.eq.0) then
        inc = (trc - blc) / pix + 1
      endif
c
c  Make the values valid.
c
      if(blc.lt.1.or.blc.gt.dims) blc = 1
      if(trc.lt.1.or.trc.gt.dims) trc = dims
      inc = max(1,inc)
      if(blc.gt.trc) call bug('f','Invalid BLC or TRC')
c
      n = (trc-blc)/inc + 1
      if(n.gt.pix) call bug('f','Image too big to fit on the screen.')
c
      return
      end
c************************************************************************
      subroutine zoomit(xcoord,ycoord,nx,ny,n)
c
      implicit none
      integer n,nx,ny
      integer xcoord(n),ycoord(n)
c
c  Determine the portion of the screen that is currently covered by the
c  images, and zoom in on it.
c
c  Input:
c    n            Number of images displayed.
c    nx,ny      Size of each image, in pixels.
c    xcoord,ycoord The BLC coordinate of the first pixel of each image.
c
c------------------------------------------------------------------------
      integer i,xmin,xmax,ymin,ymax
c
      xmin = xcoord(1)
      xmax = xmin + nx - 1
      ymin = ycoord(1)
      ymax = ymin + ny - 1
      do i=2,n
        xmin = min(xmin,xcoord(i))
        xmax = max(xmax,xcoord(i)+nx-1)
        ymin = min(ymin,ycoord(i))
        ymax = max(ymax,ycoord(i)+ny-1)
      enddo
c
      call TvView(xmin,ymin,xmax,ymax)
c
      return
      end
c************************************************************************
      subroutine movie(xcoord,ycoord,chans,nx,ny,n,xmax,ymax,server)
c
      implicit none
      integer n,nx,ny,xmax,ymax
      integer xcoord(n),ycoord(n),chans(n)
      character server*(*)
c
c  A mosaic of images have been loaded onto the TV screen.
c  This routine will run the sequence as a movie.
c
c  Inputs:
c    n            Number of images in the mosaic.
c    nx,ny      Size of each image.
c    xcoord,ycoord Bottom left coordinate of each image.
c    chans      TV channel for each image
c    xmax       Screen width.
c
c------------------------------------------------------------------------
      integer change, val1, val2
      integer blcx, blcy, trcx, trcy, nxsize, nysize
      integer k, x, y, button, achan, z
      real scale, t1, t2
      logical more, runit, step, right, loop, ctrl, restart
c
c  Externals.
c
      character itoaf*3
c
      call CtrlOpen(server, ctrl)
c
      call Output('Control the rate of speed of the movie')
      call Output('by moving the X position of the cursor.')
      call Output('The left edge of the display will run')
      call Output('the movie faster; the right edge, slower.')
c
      if(ctrl) then
        call CtrlDef('RunLeft',   'button',   '<< Run',  1)
        call CtrlDef('RunRight',  'button',   'Run >>',  1)
        call CtrlDef('StepLeft',  'button',   '<- Step', 1)
        call CtrlDef('StepRite',  'button',   'Step ->', 1)
        call CtrlDef('Reverse',   'button',   'Loop/Osc',1)
        call CtrlDef('Stop',      'button',   'Stop',    1)
	call CtrlDef('Restart',   'button',   'ReZoom',  1)
        call CtrlDef('Exit',      'button',   'Exit',    1)
        call CtrlDef('Plane',     'status', 'Plane:  1', 1)
        call CtrlView
        call CtrlClr
      else
        call Output('Button A runs the movie to the left;')
        call Output('Button B stops the movie;')
        call Output('Button C runs the movie to the right;')
        call Output('Button D exits.')
        call Output('Buttons A - D may also be keys F3 - F6.')
      endif
c
      restart = .TRUE.
      more  = .TRUE.
      right = .TRUE.
      loop  = .TRUE.
      runit = .TRUE.
      step  = .FALSE.
c
c  Get the current window size.  If one image is smaller or larger
c  than the current window, then zoom the image up to the appropriate
c  window size.
c
c
      nxsize = (xmax / 2) - 1 - (nx / 2)
      nysize = (ymax / 2) - 1 + (ny / 2)
      k = 0
      x = 0
      y = 0
      achan = chans(1)
      call tvchan(achan)
      call tvcursor(x,y,button)
      button = 3
c
      call output('Looping around the displayed channels')
      dowhile(more)
	if(restart)then
	  blcx = 0
	  blcy = 0
	  trcx = 0
	  trcy = 0
	  call TvWind(blcx, blcy, trcx, trcy)
	  t1 = min( (trcx - blcx)/real(nx), (trcy - blcy)/real(ny) )
	  t2 = min(xmax/real(nx), ymax/real(ny))
	  z = max(1,nint(min(t1,t2)))
	  call TvZoom(z, xmax/2, ymax/2)
	  scale = 5.0 / (trcx - blcx)
	  restart = .false.
	endif
        step = .FALSE.
        if (ctrl) then
          call CtrlChck('Restart', change, val1, val2)
	  restart = change.gt.0
          call CtrlChck('Reverse', change, val1, val2)
          if (change.gt.0) then
	    loop = .not.loop
            if(loop)then
	      call output('Looping around the displayed channels')
	    else
	      call output('Oscillating between start and end')
	    endif
	  endif
          call CtrlChck('RunLeft', change, val1, val2)
          if (change.gt.0) then
            runit = .TRUE.
            right = .FALSE.
          endif
          call CtrlChck('RunRight', change, val1, val2)
          if (change.gt.0) then
            runit = .TRUE.
            right = .TRUE.
          endif
          call CtrlChck('StepLeft', change, val1, val2)
          if (change.gt.0) then
            runit = .FALSE.
            step = .TRUE.
            right = .FALSE.
          endif
          call CtrlChck('StepRite', change, val1, val2)
          if (change.gt.0) then
            runit = .FALSE.
            step = .TRUE.
            right = .TRUE.
          endif
          call CtrlChck('Stop', change, val1, val2)
          if (change.gt.0) then
            runit = .FALSE.
          endif
          call CtrlChck('Exit', change, val1, val2)
          if (change.gt.0) then
            runit = .FALSE.
            more = .FALSE.
          endif
        else
c
c  No panel present; function subset available through button presses.
c
          if(button.eq.1) then
            runit = .TRUE.
            right = .FALSE.
          else if(button.eq.2) then
            runit = .FALSE.
          else if(button.eq.3) then
            runit = .TRUE.
            right = .TRUE.
          else if(button.eq.4) then
            runit = .FALSE.
            more = .FALSE.
          endif
        endif
c
c  Does a move exist?  If so, increment the image counter and scroll.
c
        if (step.or.runit) then
          if (right) then
            k = k + 1
            if (k.gt.n.and..not.loop) then
              right = .FALSE.
              k = n - 1
            else if (k .gt. n) then
              k = 1
            endif
          else
            k = k - 1
            if (k.lt.1.and..not.loop)then
              right = .TRUE.
              k = 2
            else if (k .lt. 1) then
              k = n
            endif
          endif
c
          if(chans(k).ne.achan)then
            achan = chans(k)
            call tvchan(achan)
          endif
c
          if (ctrl)call CtrlSeta('Plane', 'Plane: '//itoaf(k))
c
          trcx = nxsize - xcoord(k)
          trcy = nysize + ycoord(k)
          call TvScrl(trcx, trcy)
        endif
c
        call tvcursor(x, y, button)
        x = x - blcx + 1
        if (x .lt. 1) x = 1
	if(runit)then
	  call delayf(scale*x)
	else if(.not.restart)then
	  call delayf(0.1)
	endif
      enddo
c
      if (ctrl) then
        call CtrlClr
        call CtrlFin
      endif
c
      end
