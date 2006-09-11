c************************************************************************
       program tvinit
       implicit none

c= tvinit - Initialize a TV device
c& rjs
c: visual display
c+
c	TVINIT is a MIRIAD/Werong task which initializes a TV device. These
c	initializations clear the screen, set pan, zoom and colour lookup
c	table to their defaults.
c@ server
c	The TV device. No default. See the Users Manual for information
c	on how to specify this.
c--
c
c      nebk 02Feb88   Old and dusty version
c      nebk 02Sep91   Vastly improved by fixing spelling mistake in
c                     help file.
c------------------------------------------------------------------------
      character server*32
      call output( 'Tvinit: version 02-sep-91' )
      call keyini
      call keya('server',server,' ')
      call keyfin
c
      call tvopen(server)
      call tvreset
      call tvclose
c
      end
