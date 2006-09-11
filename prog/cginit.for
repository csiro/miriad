      program cginit
c-----------------------------------------------------------------------
c= CGINIT - Initialize a PGPLOT device. 
c& nebk
c: plotting
c+
c	CGINIT clears the display of the specified PGPLOT device.
c
c@ device
c	The PGPLOT plot device.   No default.
c--
c
c  History:
c    nebk 10Aug91  Create.
c         02Sep91  Rename from GRCLEAR
c    nebk/mjs
c         13mar92  Rename:  PGINIT -> CGINIT
c    mjs  13mar93  pgplot subr names have less than 7 chars.
c-----------------------------------------------------------------------
      implicit none
c
      character dev*32, hard*3
      integer ierr, pgbeg, ilen
c-----------------------------------------------------------------------
      call keyini
      call keya ('device', dev, ' ')
      call keyfin
      if (dev.eq.' ') call bug ('f', 'You must specify a device')
c
      ierr = pgbeg (0, dev, 1, 1)
      if (ierr.ne.1) call bug ('f', 'Error opening plot device') 
c
      call pgqinf ('hardcopy', hard, ilen)
      if (hard.eq.'YES') then
        call bug ('w', 'You are clearing a hardcopy device !!')
      end if
c
      call pgask(.false.)
      call pgpage
      call pgend
c
      end 
