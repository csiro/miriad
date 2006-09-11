      program tvcycle
      implicit none
c
c= tvcycle - Cycle through TV channels to compare different images.
c  
c& nebk
c: visual display
c+
c	TVCYCLE is a MIRIAD task to cycle through TV channels thus
c	allowing different images to be compared.   Use button D
c	to exit.  Use button C for local fiddle mode.
c@ tvchan
c	List of channels to cycle around.
c@ delay
c	Delay in seconds between channel changes. Default is 0.5 s
c	This can be altered with the x-location of the cursor.
c	The cursor at the left and right hand edges give 0*delay
c	and  2*delay respectively.
c@ server
c	The TV device where the image is to be displayed. No default. 
c@ options
c	reverse   Default is to loop around the channels. For example
c	           1-2-3-1-2-3   This options does 1-2-3-2-1-2-3
c--
c
c  History:
c   nebk 27apr92 Initial version
c   nebk 23oct92 Tell user delay size
c   mjs  30sep93 call delay -> call delayf
c
c------------------------------------------------------------------------
      integer nmax
      parameter (nmax = 10)
c
      integer chan(nmax), nchan, i, ichan, ix, iy, ibutt, xpix, ypix,
     +mxchan, levels, j
      character server*32, line*80
      real delay1, delay2
      logical reverse
c------------------------------------------------------------------------
      call output('TvCycle: version 23-Oct-92')
c
c  Get the inputs
c
      call keyini
      call mkeyi ('tvchan', chan, nmax, nchan)
      if (nchan.lt.2) call bug ('f', 'Not enough channels given')
      call keyr ('delay', delay1, 0.5)
      if (delay1.lt.0.0) call bug ('f', 'Negative delay !')
      call keya ('server', server, ' ')
      if (server.eq.' ') call bug ('f', 'No server given')
      call getopt (reverse)
      call keyfin
c
c  Open TV
c
      call tvopen (server)
      call tvchar (xpix, ypix, mxchan,levels)
      do i = 1, nchan
        if (chan(i).gt.mxchan .or. chan(i).lt.1) then
          write (line, 100) i, chan(i)
100       format ('tvchan(', i2, ') = ', i4, ' is invalid')
          call bug ('f', line)
        end if
      end do
c
c  Loop until interrupted by user
c
      delay2 = delay1
      ibutt = 0 
      i = 1
      j = 1
      do while (ibutt.ne.4)
c
c  Turn on the correct channel
c
        ichan = chan(i)
        call tvchan (ichan)
        call tvflush
c
        if (i.eq.nchan) then
          i = i - 1
          if (.not.reverse) i = 1
        else
          i = i + 1
        end if
c
c  Delay and then read cursor status. Tell user delay size
c  every 4th time
c
        if (j.eq.1) then
          write (line,200) delay2
200       format ('Current time delay = ', f8.4, ' seconds')
          call output (line)
        end if
        j = j + 1
        if (j.eq.4) j = 1
c 
        call delayf (delay2)
        call tvcursor (ix, iy, ibutt)
c
c  Do local TV fiddle.  WIll do nothing for XAS
c
        if (ibutt.eq.3) call tvlocal
c
c  Set new delay
c
        if (ix.lt.1 .or. ix.gt.xpix) ix = xpix / 2
        delay2 = 2.0 * (real(ix) / real(xpix)) * delay1
      end do
c
c  Close up
c
      call tvclose
c
      end
c
c
      subroutine getopt (reverse)
c---------------------------------------------------------------------
c  Determine processing options.
c
c  Output:
c    reverse    True to do reverse loop
c---------------------------------------------------------------------
      implicit none
      logical reverse
c
      integer nopts
      parameter(nopts=1)
      logical present(nopts)
      character opts(nopts)*8
      data opts/'reverse'/
c---------------------------------------------------------------------
      call options('options',opts,present,nopts)
      reverse = present(1)
c
      end
