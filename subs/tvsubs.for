c***********************************************************************
c
c  History:
c     jm    23mar90  Original code.
c     jm    16apr90  Modified code and cleaned up.
c     jm    07may90  Changed call to include declared x-size of array.
c     jm    17jun90  Cleaned up code a bit.
c     jm    27aug90  Broke up TvLine writes into MAXDIM chunks so
c                    that images larger than the internal array size
c                    of MAXDIM can be loaded.
c     jm    16nov90  Corrected redeclaration of maxchan problem.
c     jm    04jul92  Included call to tvscale subroutine in display.
c
c***********************************************************************
c* ImScale -- Autoscale a map.
c& jm
c: image-data,utilities
c+
      subroutine imscale(map, Mx, Nx, Ny, pmin, pmax)
c
      implicit none
      integer Mx, Nx, Ny
      real map(Mx, Ny), pmin, pmax
c
c  Finds the minimum and maximum of the real array map(x,y).
c
c  Input:
c    map(Nx,Ny)   Input real array of values.
c    Mx           The maximum (or declared) X dimension of the array.
c    Nx, Ny       X/Y working dimensions of the map array.
c
c  Output:
c    pmin, pmax   The minimum (pmin) and maximum (pmax) of the array.
c
c--
c-----------------------------------------------------------------------
      integer x, y
c
      pmin = map(1, 1)
      pmax = pmin
      do 20 y = 1, Ny
        do 10 x = 1, Nx
          if (map(x, y) .lt. pmin) pmin = map(x, y)
          if (map(x, y) .gt. pmax) pmax = map(x, y)
   10   continue
   20 continue
      return
      end
c
c***********************************************************************
c* Display -- Load a map onto a TV device with user specified ranges.
c& jm
c: image-data,tv,display
c+
      subroutine display(map, Mx, Nx, Ny, chan, x0, y0, pmin, pmax)
c
      implicit none
      integer Mx, Nx, Ny, chan, x0, y0
      real map(Mx, Ny), pmin, pmax
c
c  Displays an array of real numbers on the TV device scaled to the
c  limits of the device.  An internal integer array is used to hold
c  the scaled data and quietly accounts for very large array
c  dimensions.
c
c  Input:
c    map(Nx,Ny)   Input real array of values.
c    Mx           The maximum (or declared) X dimension of the array.
c    Nx, Ny       X/Y working dimensions of the map array.
c    chan         The TV channel to load the image.
c    x0, y0       The lower left corner to start loading the image.
c    pmin         The minimum value of the array.
c    pmax         The maximum value of the array.
c
c  Output:
c    none
c
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer data(MAXDIM)
c
      integer jx, jy, ichan
      integer m, x, y, Nitem, ival
      integer Nfirst, Nlast, Nsection
      integer xmax, ymax, mostchan, nlev
      real bzero, bscale
c
      if (pmin .eq. pmax) return
      call TvChar(xmax, ymax, mostchan, nlev)
      if ((chan .lt. 1) .or. (chan .gt. mostchan)) return
c
      ichan = chan
      nlev = nlev - 1
      bzero = pmin
      bscale = nlev / (pmax - pmin)
      Nsection = (Nx - 1) / MAXDIM
      jy = y0
c
      call TvScale(bzero, bscale)
      do 30 y = 1, Ny
        jx = x0
        Nfirst = 1
        Nlast = MAXDIM
        do 20 m = 0, Nsection
          if (m .eq. Nsection) Nlast = Nx
          Nitem = 0
          do 10 x = Nfirst, Nlast
            ival = (map(x, y) - bzero) * bscale
            if (ival .lt. 0) ival = 0
            if (ival .gt. nlev) ival = nlev
            Nitem = Nitem + 1
            data(Nitem) = ival
   10     continue
          call TvLine(jx, jy, ichan, data, Nitem)
          jx = jx + MAXDIM
          Nfirst = Nlast + 1
          Nlast = Nlast + MAXDIM
   20   continue
        jy = jy + 1
   30 continue
      call TvFlush
      return
      end
c
c***********************************************************************
c* Wedge -- Load a wedge onto a TV device with user specified ranges.
c& jm
c: image-data,tv,display
c+
      subroutine wedge(map, Nxy, Dir, chan, x0, y0, pmin, pmax, width)
c
      implicit none
      integer Nxy, Dir, chan, x0, y0, width
      real map(Nxy), pmin, pmax
c
c  Displays a wedge of real numbers on the TV device scaled to the
c  limits of the device.  If Dir is 1 and Nxy is too large (MAXDIM),
c  the wedge is truncated (to MAXDIM).
c
c  Input:
c    map(Nxy)     Input real array of values.
c    Nxy          The dimension of the wedge array.
c    Dir          The direction to stretch the wedge.  If ``Dir'' is 0,
c                 then the array is displayed along the x direction
c                 and repeated ``width'' pixels upward; if ``Dir'' is 1,
c                 then the array is displayed along the y direction
c                 and repeated ``width'' pixels to the right.  Any
c                 other value of Dir is treated as if Dir was set to 0.
c    chan         The TV channel to load the wedge.
c    x0, y0       The lower left corner to start loading the wedge.
c    pmin         The minimum value of the array.
c    pmax         The maximum value of the array.
c    width        The number of pixels to repeat the wedge in
c                 direction ``Dir'' on the display.
c
c  Output:
c    none
c
c--
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer data(MAXDIM)
c
      integer jx, jy, ichan
      integer m, x, y, ival, Nitem
      integer Nfirst, Nlast, Nsection
      integer xmax, ymax, mostchan, nlev
      real bzero, bscale
c
      if (pmin .eq. pmax) return
      call TvChar(xmax, ymax, mostchan, nlev)
      if ((chan .lt. 1) .or. (chan .gt. mostchan)) return
c
      ichan = chan
      nlev = nlev - 1
      jx = x0
      jy = y0
      bzero = pmin
      bscale = nlev / (pmax - pmin)
c
      if (Dir .eq. 1) then
        Nlast = min(width, MAXDIM)
        do 20 y = 1, Nxy
          ival = (map(y) - bzero) * bscale
          if (ival .lt. 0) ival = 0
          if (ival .gt. nlev) ival = nlev
          do 10 x = 1, Nlast
            data(x) = ival
   10     continue
          call TvLine(jx, jy, ichan, data, Nlast)
          jy = jy + 1
   20   continue
      else
        jx = x0
        Nsection = (Nxy - 1) / MAXDIM
        Nfirst = 1
        Nlast = MAXDIM
        do 70 m = 0, Nsection
          jy = y0
          if (m .eq. Nsection) Nlast = Nxy
          Nitem = 0
          do 50 x = Nfirst, Nlast
            ival = (map(x) - bzero) * bscale
            if (ival .lt. 0) ival = 0
            if (ival .gt. nlev) ival = nlev
            Nitem = Nitem + 1
            data(Nitem) = ival
   50     continue
          do 60 y = 1, width
            call TvLine(jx, jy, ichan, data, Nitem)
            jy = jy + 1
   60     continue
          jx = jx + MAXDIM
          Nfirst = Nlast + 1
          Nlast = Nlast + MAXDIM
   70   continue
      endif
      call TvFlush
      return
      end
