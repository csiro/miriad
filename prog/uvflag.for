      program uvflag

c= UVFLAG - Flags or unflags uv data
c& bpw
c: calibration, uv analysis
c+
c	UVFLAG is used to change flags corresponding to visibility data.
c	Using the keywords select, line and edge one selects a portion of a
c	uv-data file. For all selected correlations the flags are then set to
c	the value specified with the keyword flagval. If flagval equals
c	'flag', the uv data are flagged as bad; for flagval equal to 'unflag',
c	the uv data are flagged as good.
c	The user can control the amount of output produced using the options
c	keyword. Only records that fulfill all selection criteria as
c	specified by select, line and edge are shown and counted, not the
c	complete datafile.
c	Because the physical writing of the flags is done using a buffering
c	approach, the flags are only actually changed in the datafile if
c	one lets uvflag finish normally. I.e., if the 'q' option is used to
c	stop printing, the flags may or may not have been changed.
c	UVFLAG can also be used to inspect the uv-data. If option 'noapply'
c	is used, everything works as it would do normally, except that the
c	flags are not actually changed in the datafile. This is particularly
c	useful in combination with option 'full'.
c< vis
c	Multiple input datasets are allowed.
c< select
c< line
c	Uvflag supports only types 'channel and wide'. If line is
c	unspecified, both the 'channel data' and the 'wideband data'
c	(if present) will be flagged. Also, since averaging of data
c	is an undefined operation when setting flags, width is
c	forced to be equal to 1.
c@ edge
c	This keyword allows uvflag to work on the edges of spectral
c	windows. Three numbers may be given, n, m and c. Flags will
c	be changed only for the first n and last m channels of each
c	spectral window and for the central c channels.
c	If one value is given, the number of selected channels at
c	the start and end of the window is assumed to be equal.
c	If two values are given the first gives the number of
c	selected channels at the start, the second the number at
c	the end. In this case either one may also be 0.
c	The third value gives the number of channels to delete from
c	the center of the band. The default is 0.  The center of a
c	window is defined as 'startchannel+nchannels/2'.
c	A negative value is interpreted as meaning that the beginning,
c	ending, or middle 1/8th of the band must be deleted.
c	Use of the edge keyword forces the step parameter of the
c	line keyword to be equal to 1.
c	Edge cannot be combined with linetype 'wide'. If edge is
c	used, the linetype defaults to channel,0,1,1,1, i.e. all
c	channels.
c	This also works for multiple input visibility files, even
c	if these have different correlator modes.
c	[0,0,0]
c@ flagval
c	Either 'flag' or 'unflag', which tells whether the flags for
c	the correlations selected by 'select', 'line' and 'edge'
c	have to be set or unset. May be abbreviated to 'f' or 'u'.
c	Exactly one of the options 'flag' or 'unflag' must be
c	present.
c@ options
c	One or more of
c	  'noapply',
c	  'none', 'brief', 'indicative', 'full', 'noquery',
c	  'hms', 'decimal'.
c
c	These options can be abbreviated to uniqueness. The default
c	is 'brief,hms', except when a logfile is given (see keyword
c	log), then it becomes 'indicative,hms'.
c
c	'noapply' will go through the whole process without
c	actually changing the flags. Useful for checking what will
c	happen or for inspecting the flags.
c	No history comments are written.
c
c	'none', 'brief', 'indicative' and 'full' control the
c	amount of information returned to the user.
c	  - 'brief' gives an overview when UVFLAG finishes.
c	  - 'indicative' lists the number of good, bad and changed
c	     flags for each selected uv-record.
c	  - 'full' lists the data, the old flag and the new flag for
c	     each channel separately and also an overview of each record.
c	  - If more than 1 verbosity level is given, the lowest is taken.
c	  - Option 'noquery' will turn off the feature that printing
c	    is halted every 22 lines.
c
c	For verbosity levels 'indicative' and 'full' the format
c	of the time that is written is determined by 'hms' (hours,
c	minutes and seconds) or 'decimal' (decimal parts of a day).
c@ log
c	Name of a file where reported information is written.
c	If empty this implies the terminal screen, else the
c	named file. Giving a filename also sets option 'indicative'.
c	[terminal]
c--
c***********************************************************************
c
c  History:
c	    nebk    23may89 Original version.
c	    bpw/rjs  6apr90 COMPLETELY rewritten version.
c	    bpw     25may90 Divided options= into flagval= and options=
c	    bpw     17jul90 Fixed ratty and some cray peculiarities to
c			    get it working.
c	    bpw     15aug90 Added writing of history information.
c	    bpw     17dec90 Add keyword shadow and edge plus did some
c			    rewriting because of this.
c	    bpw     20mar91 Reshuffling so that edge refers to window
c			    edges, not band edges; include multiple
c			    input files.
c	    bpw     27mar91 Fixed handling of wideband flags. rjs wrote
c			    uvwflgwr to make this possible.
c	    bpw      5apr91 Repaired amplitude selection.
c	    bpw      8apr91 Allowed infinity of input files.
c	    bpw     17apr91 Got rid of shadow keyword, now included in
c			    uvselect.
c	    bpw     22apr91 Fixed "closing of logfile with multiple
c			    input datasets" bug.
c	    bpw     09may91 Checked select=shadow and did one output
c			    beautification.
c	    bpw     22may91 Set MAXANT to 28 in antusage and included
c			    some tests.
c	    rjs     19jun91 Corrected copying history string
c	    mjs     04aug91 Replaced local MAXANT with maxdim.h value
c	    bpw     21oct91 Add count of total number of records
c			    Added some arrays to avoid write(..) fn(.)
c	    bpw     27mar91 Changed assert into assertl
c	    rjs     17jun93 Change calls to rtfmt, to avoid Sun compiler bug
c	    bpw     14dec93 Realized that uvwread does not take uvset into
c			    account, and that therefore line=wide did not
c			    flag properly if nwchan!=0
c	    rjs     23dec93 Minimum match of line parameter.
c	    rjs     10oct94 Eliminate spurious extra call to uvflgwr.
c	    rjs     16aug96 Eliminate MAXWIDE definition. Change NSELS,
c			    standardise some FORTRAN.
c	    rjs     09dec97 Make antennas used message more robust.
c	    rjs     11mar98 Some FORTRAN standardisation, to appease g77.
c	    rjs     30aug99 Increase outline to 256 chars.
c	    tw	    26aug04 allow fractional edge flagging using edge<0
c           rjs     26nov05 Change to avoid compiler bug.
c************************************************************************
c uvflag works as follows:
c It reads the name of the first visibility file.
c Then all other keywords are asked-for and decoded.
c Next it treat the input file. I.e. first the selection criteria are
c transferred to uvio, then the to-be-used channelnumbers are set, then
c the flagging is done.
c Then it asks for the next visibility file and does the whole process
c again until the list is exhausted.

      character*(*) version
      parameter ( version = 'uvflag: version 2.6 26-aug-04')

      character*64     vis

      integer	       NSELS
      parameter        ( NSELS = 25000 )
      real	       sels(NSELS)

      integer	       line(7)
      character*16     type
      include	       'maxdim.h'
      logical	       usech(MAXCHAN)

      logical	       flagval

      logical	       apply
      character*10     ropt
      character*1      tformat

      data	       usech / MAXCHAN * .true. /

      call output( version )
      call keyini
      call keyf( 'vis', vis, ' ' )
      call inputs( vis, sels,nsels, line,type,
     *			flagval, apply,ropt,tformat )
      do while( vis .ne. ' ' )
	 call scanvis( vis, sels,nsels, line,type,usech,
     *			    flagval, apply,ropt,tformat )
	 call keyf( 'vis', vis, ' ' )
      enddo
      call keyfin
      call logclose
      stop
      end


c***********************************************************************
c As its name says: input reads all inputs. It also transforms them to
c some codes used by the program and the uvio routines.
c vis, line and type are used for making the output human-understandable.
c sels is a coded selection, transported to caller.
c flagval gives the value to which the flags must be set, FALSE for
c	  option 'flag', TRUE for option 'unflag'.
c apply is true by default and false if the 'noapply' option was given.
c ropt gives the verbosity level.
c tformat indicates the time format: 'H' or 'D'.

      subroutine inputs( vis, sels,nsels, line,type,
     *			      flagval, apply,ropt,tformat )

      character*(*)    vis
      real	       sels(*)
      integer	       nsels
      integer	       line(*)
      character*(*)    type
      logical	       flagval
      logical	       apply
      character*(*)    ropt
      character*(*)    tformat

      integer	       unit
      integer	       nchan
      real	       start, width, step
      integer	       chwin(32)
      character*128    logfile
      character*6      flagopts(2)
      integer	       NOPT
      parameter        ( NOPT = 8 )
      character*10     opts(NOPT)
      logical	       optprsnt(NOPT)
      integer	       i, nout
      character lines(3)*8

      data	       opts /
     *		       'noapply',
     *		       'none', 'brief', 'indicative', 'full', 'noquery',
     *		       'hms', 'decimal' /
      data	       flagopts / 'flag', 'unflag' /
      data	       chwin / 32*-1 /
      data	       lines /'channel ', 'wide    ', 'both    '/

c Open first dataset and position it to read other info.
      call uvinit( unit, vis, .false. )

c Get user-defined selection criteria and transfer to uvio
      call selinput( 'select', sels, nsels )

c Get linetype and transfer it to uvio.c
c Linetype velocity is not allowed because it is not possible to
c flag averaged data.
c Width is forced to be 1, because any other value would allow
c meaningless flagging.
      call keymatch('line', 3, lines, 1, type, nout)
      if( nout.eq.0 ) type = 'both'
      if( type.ne.'both' )
     *then
	 call keyi( 'line', nchan, 0 )
	 call keyr( 'line', start, 1. )
	 call keyr( 'line', width, 1. )
	 call keyr( 'line', step, width )
	 call assertl( start.gt.0., 'Channel numbers <0 do not exist'  )
	 call assertl( width.eq.1., 'Width must be 1 for UVFLAG' )
	 call assertl( step .gt.0., 'Step between channels must be >0' )
      else
	 nchan = 0
	 start = 1.
	 width = 1.
	 step  = 1.
      endif
      line(1) = nchan
      line(2) = int(start)
      line(3) = int(width)
      line(4) = int(step)

c Read window information to get default for line(7).
      call uvrdvri( unit, 'nschan', chwin, chwin )
c See if selected channels must be limited to edge/center channels.
      call keyi( 'edge', line(5), 0	  )
      call keyi( 'edge', line(6), line(5) )
      call keyi( 'edge', line(7), 0	  )
      if( line(5).lt.0 ) line(5) = chwin(1)/8
      if( line(6).lt.0 ) line(6) = chwin(1)/8
      if( line(7).lt.0 ) line(7) = chwin(1)/8
      if( line(5).ne.0 .or. line(6).ne.0 .or. line(7).ne.0 )
     *then
	 call uvrdvri( unit, 'nchan', nchan, -1 )
	 call assertl( nchan .ne. -1,
     *	      'No channel data in visibility file' )
	 call assertl( type.ne.'wide',
     *	      'edge= is invalid if linetype wide was selected' )
	 call assertl( line(3).eq.1,
     *	      'To use edge=, the step parameter of line= must be 1' )
	 call assertl( line(5).lt.(chwin(1)/2+1) .and.
     *		       line(6).lt.(chwin(1)/2+1) .and.
     *		       line(7).lt. chwin(1),
     *		       'Edge selection would drop all channels' )
	 type = 'channel'
      endif

c Only one of the flagging options (flag, unflag) may be present.
c If not, the program quits.
      call options( 'flagval', flagopts, optprsnt, 2 )
      call assertl( .not.( optprsnt(1) .and. optprsnt(2) ),
     *		    'Flag and unflag can not be combined' )
      call assertl( optprsnt(1) .or. optprsnt(2),
     *		    'Flagval must be flag or unflag' )
      if( optprsnt(1) ) flagval = .false.
      if( optprsnt(2) ) flagval = .true.

c Interpret options keyword.
c First parse the input line and return whether options are or are not
c present.
      call options ( 'options', opts, optprsnt, NOPT )

c By default apply flagging. Only if 'noapply' present don't do it
      apply = .not. optprsnt(1)

c Ask the log keyword. The logfile will be opened a little later.
      call keya( 'log', logfile, ' ' )

c The lowest verbosity level is chosen if more than one is given.
c If a logfile was entered the default value of ropt is set to
c 'indicative'.
			   ropt = 'brief'
      if( logfile.ne.' ' ) ropt = 'indicative'
      do i = 5, 2, -1
	 if( optprsnt(i) ) ropt = opts(i)
      enddo

c Open logfile with flag determined by presence/absence of 'noquery'
c option.
      if(      optprsnt(6) ) call logopen( logfile, ' ' )
      if( .not.optprsnt(6) ) call logopen( logfile, 'q' )

c Select whether times are written in hours, minutes and seconds or
c decimal parts of a day.
			tformat = 'H'
      if( optprsnt(7) ) tformat = 'H'
      if( optprsnt(8) ) tformat = 'D'

c A special case that implies nothing useful can be done: quit.
      call assertl( apply .or. ropt.ne.'none',
     *	'No apply and no reporting means do nothing; UVFLAG quits' )

c Close dataset, which was opened to get some inputs.
      call uvclose( unit )

      return
      end


c***********************************************************************
c uvinit reads the first record of the vis file, so that other
c information can be read from it.
c Also count the number of records if docount is true

      subroutine uvinit( unit, vis, docount )

      integer	       unit
      character*(*)    vis
      logical	       docount

      double precision preamble(4)
      include	       'maxdim.h'
      complex	       data(MAXCHAN)
      logical	       flags(MAXCHAN)
      integer	       nchan
      integer	       count

      call uvopen( unit, vis, 'old' )
      call uvread( unit, preamble, data, flags, MAXCHAN, nchan )
      if( docount ) then
	 count = 0
	 do while( nchan.ne.0 )
	    count = count + 1
	    call uvread( unit, preamble, data, flags, MAXCHAN, nchan )
	 enddo
	 call uvrewind( unit )
	 call rectot( count )
      endif

      return
      end


c***********************************************************************
c SCANVIS controls the scanning of the dataset and whether flags must be
c set for each particular record. It uses work to set the flags and write
c them to the file. This is done first for linetype channel, next for
c linetype wide, or both if the line keyword was defaulted.
c vis is the name of the visibility dataset.
c sels,nsels contains the selection criteria for uvreading.
c line and type contain the nchan,start,width,step part of the linetype.
c usech is a mask for the selected channels.
c flagval is the value to which the flags must be set.
c apply determines whether the changed flags are physically written.
c ropt gives the verbosity level.
c tformat is transfered to report.

      subroutine scanvis( vis, sels,nsels, line,type,usech,
     *			       flagval, apply,ropt,tformat )

      character*(*)    vis
      integer	       nsels
      real	       sels(nsels)
      integer	       line(*)
      character*(*)    type
      logical	       usech(*)
      logical	       flagval
      logical	       apply
      character*(*)    ropt
      character*(*)    tformat

      integer	       unit
      double precision preamble(4)
      include	       'maxdim.h'
      integer	       mxchan
      complex	       data(MAXCHAN)
      logical	       oldflags(MAXCHAN), newflags(MAXCHAN)
      integer	       nchan, nwchan
      logical	       have
      mxchan = MAXCHAN

      call showinp( vis, line,type, flagval, apply, ropt )
      call setup(   vis, sels, line,type, usech, unit )

      nchan = -1
      do while( nchan.ne.0 )
	 if( type .eq. 'both' )
     *	 then
	    call uvset(  unit, 'data', 'channel', 0, 1.,1.,1. )
	    call uvread( unit, preamble, data, oldflags, mxchan, nchan )
	    if( have(unit,'nchan') .and. nchan.ne.0 )then
	      call work(   unit, preamble, 'channel',
     *			 flagval, ropt,tformat,line,
     *			 data,oldflags,newflags, usech, nchan )
	      if( apply ) call uvflgwr( unit,newflags )
	    endif

	    if( have(unit,'nwide') .and. nchan.ne.0 ) then
	      call uvwread(unit, data, oldflags, mxchan, nwchan )
	      if(nwchan.gt.0)then
		call work(   unit, preamble, 'wide',
     *			 flagval, ropt,tformat,line,
     *			 data,oldflags,newflags, usech, nwchan )
		if( apply ) call uvwflgwr( unit,newflags )
	      endif
	    endif

	 else

	    call uvset( unit, 'data', type, line(1), real(line(2)),
     *				      real(line(3)), real(line(4)) )
	    call uvread( unit, preamble, data, oldflags, mxchan, nchan )
	    if(  nchan.ne.0.  .and.
     *		 ( (type.eq.'channel' .and. have(unit,'nchan')) .or.
     *		   (type.eq.'wide'    .and. have(unit,'nwide'))      )
     *								)then
	      call work(   unit, preamble, type,
     *			 flagval, ropt,tformat,line,
     *			 data,oldflags,newflags, usech, nchan )
	      if( apply ) call uvflgwr( unit,newflags )
	    endif
	 endif
	 if( nchan.ne.0 ) call reccount(1)
      enddo

      call overview( unit, vis, type, flagval,apply,ropt )

      call uvclose( unit )

      end


c***********************************************************************
c reccount counts the number of records read

      subroutine reccount( i )
      integer	       i, opt
      integer	       countsel, counttot
      save	       countsel, counttot
      data	       countsel, counttot / 0, 0 /
      countsel = countsel + i
      if( i.le.0 ) countsel = 0
      return
      entry rectot( i )
      counttot = i
      return
      entry nrecords( i, opt )
      if( opt.eq.1 ) i = countsel
      if( opt.eq.2 ) i = counttot
      return
      end


c************************************************************************
c setup sets/resets the uv-selection and channels to be used for each
c new input datafile. It also opens it.

      subroutine setup( vis, sels, line,type, usech, unit )

      character*(*)    vis
      real	       sels(*)
      integer	       line(*)
      character*(*)    type
      logical	       usech(*)
      integer	       unit

      integer	       nchan, nwins, stwin(32), chwin(32)
      integer	       chan, chnr, win, boxnri, stawin, midwin, endwin
      integer	       i, inittot

c Open visibility file, count the number of records, and position it
      call uvinit( unit, vis, .true. )

c If needed, build masking array of edge channels.
      if( type.eq.'channel' .and.
     *	  ( line(5).ne.0 .or. line(6).ne.0 .or. line(7).ne.0 ) )
     *then
c     Read window info.
	 call uvgetvri( unit, 'nspect', nwins,1     )
	 call uvgetvri( unit, 'ischan', stwin,nwins )
	 call uvgetvri( unit, 'nschan', chwin,nwins )
	 if( line(1).eq.0 ) call uvgetvri( unit, 'nchan', nchan,1 )
	 if( line(1).ne.0 ) nchan = line(1)
	 do chan = 1, nchan
	    chnr   = chan + line(2)-1
	    win    = boxnri( chnr, stwin, nwins )
	    stawin = stwin(win)
	    midwin = stwin(win) + chwin(win)/2
	    endwin = stwin(win) + chwin(win) - 1
	    usech( chan ) = .false.
	    if( chnr-stawin .lt. line(5) ) usech(chan) = .true.
	    if( endwin-chnr .lt. line(6) ) usech(chan) = .true.
	    if( line(7).gt.0 .and. chnr.le.midwin .and.
     *	       midwin-chnr .le.   line(7)   /2 ) usech(chan) = .true.
	    if( line(7).gt.0 .and. chnr.gt.midwin .and.
     *	       chnr-midwin .le.  (line(7)-1)/2 ) usech(chan) = .true.
	 enddo
      endif

c Reposition to beginning
      call uvrewind( unit )
c Reset selection criteria.
      call uvselect( unit, 'clear', 0.d0,0.d0,.true. )
c Apply selection criteria.
      call selapply( unit, sels, .TRUE. )
      call uvset( unit, 'selection', 'amplitude',  0, 0.,0.,0. )
      call uvset( unit, 'coord',     'wavelength', 0, 0.,0.,0. )

c Reset record count.
      call reccount(-1)
      i=inittot(0)

      return
      end


c***********************************************************************
c have indicates the presence/absence of channel/wideband data.

      logical function have( unit, var )
      integer	    unit
      character*(*) var
      character*1   type
      integer	    length
      logical	    update
      call uvprobvr( unit, var, type, length, update )
      have = type .ne. ' '
      return
      end


c***********************************************************************
c Do the work with the selected channels:
c Change the flag array,
c Write flags to dataset,
c Do statistics and reporting.
      subroutine work( unit, preamble, type,
     *		       flagval, ropt,tformat,line,
     *		       data,oldflags,newflags, usech, nchan )

      integer	       unit
      double precision preamble(*)
      character*(*)    type
      logical	       flagval
      character*(*)    ropt, tformat
      integer	       line(*)
      complex	       data(*)
      logical	       oldflags(*), newflags(*)
      logical	       usech(*)
      integer	       nchan
c------------------------------------------------------------------------
      integer itemp
c
c  Externals.
c
      integer counting
c
      call flgset( unit, flagval, data,oldflags,newflags, usech, nchan )
      itemp = counting( type, oldflags,newflags, nchan )
      call report( ropt, unit,preamble,tformat,line,type,
     *		   data,oldflags,newflags, usech, nchan )
      end
c************************************************************************
c Loop through all channels in the record and set the new flags.
c Depending on the value of amprange(1) a check will be made whether the
c data are in or out of range.

      subroutine flgset( unit, flagval, data,oldflags,newflags,
     *			 usech, nchan )

      integer	       unit
      logical	       flagval
      complex	       data(*)
      logical	       oldflags(*), newflags(*)
      logical	       usech(*)
      integer	       nchan

      double precision amp2
      integer	       i

      double precision amprange(3)
      integer	       ampflag
      double precision amplo2, amphi2
      save	       ampflag, amplo2, amphi2

c get amplitude info.
      amprange(2) = 0.d0
      amprange(3) = 0.d0
      call uvinfo( unit, 'amprange', amprange )
      ampflag = nint(amprange(1))
      amplo2  = amprange(2) ** 2
      amphi2  = amprange(3) ** 2

c keep old flags before setting some or all of the new flags.
      do i = 1, nchan
	 newflags(i) = oldflags(i)
      enddo

c amprange(1)= 0: no amplitude selection requested by 'select' keyword
c then uvinfo returned the default value of 0.
c so: set all flags in the record
      if(     ampflag.eq.0 )
     *then
	 do i = 1, nchan
	    if( usech(i) ) newflags(i) = flagval
	 enddo
c amprange(1) = 1: positive amplitude selection specified. Only when
c data is inside range are the flags changed.
      elseif( ampflag.eq.1 )
     *then
	 do i = 1, nchan
	    if( usech(i) )
     *	    then
	       amp2 = real(data(i))**2 + aimag(data(i))**2
	       if( amplo2.le.amp2 .and. amp2.le.amphi2 )
     *	       newflags(i) = flagval
	    endif
	 enddo
c amprange(1) = -1: negative amplitude selection specified. Only when
c data is not inside range are the flags changed.
      elseif( ampflag.eq.-1 )
     *then
	 do i = 1, nchan
	    if( usech(i) )
     *	    then
	       amp2 = real(data(i))**2 + aimag(data(i))**2
	       if( amp2.lt.amplo2 .or. amp2.gt.amphi2 )
     *	       newflags(i) = flagval
	    endif
	 enddo
      endif
      return

      end


c***********************************************************************
c Accumulates a few interesting counts.
c counts(1,j) = number of originally good flags
c counts(2,j) = number of originally bad flags
c counts(3,j) = number of currently good flags
c counts(4,j) = number of currently bad flags
c counts(5,j) = number of flags changed from good to bad
c counts(6,j) = number of flags changed from bad to good
c j=1 for channel linetype, j=2 for wide linetype

      integer function counting( type, oldflags, newflags, nchan )
      integer tcount, totcount, inittot

      character*(*)    type
      logical	       oldflags(*), newflags(*)
      integer	       nchan
      integer	       nr, lt

      integer	       i, j, NCOUNTS
      parameter        ( NCOUNTS = 6 )
      integer	       counts(NCOUNTS,2), totcnts(NCOUNTS,2)
      save	       counts, totcnts
      data	       totcnts / NCOUNTS*0, NCOUNTS*0 /

      if( type.eq.'channel' ) j = 1
      if( type.eq.'wide'    ) j = 2
c Loop through flag arrays to get counts
      do i = 1, NCOUNTS
	counts(i,j) = 0
      enddo
      do i = 1, nchan
	 if(	  oldflags(i) ) counts(1,j) = counts(1,j) + 1
	 if( .not.oldflags(i) ) counts(2,j) = counts(2,j) + 1
	 if(	  newflags(i) ) counts(3,j) = counts(3,j) + 1
	 if( .not.newflags(i) ) counts(4,j) = counts(4,j) + 1
	 if(	  oldflags(i) .and. .not.newflags(i) )
     *				counts(5,j) = counts(5,j) + 1
	 if( .not.oldflags(i) .and.	 newflags(i) )
     *				counts(6,j) = counts(6,j) + 1
      enddo
      do i = 1, NCOUNTS
	totcnts(i,j) = totcnts(i,j) + counts(i,j)
      enddo
c
      counting = 0
c
      return

      entry tcount(nr,type)
      if( type.eq.'channel' ) j = 1
      if( type.eq.'wide'    ) j = 2
      tcount = counts(nr,j)
      return
      entry totcount(nr,lt)
      totcount = totcnts(nr,lt)
      return
      entry inittot(nr)
      do j = 1, 2
	 do i = 1, NCOUNTS
	    totcnts(i,j) = 0
	 enddo
      enddo
      inittot=0
      return
      end


c***********************************************************************
c Type an overview of keyword values

      subroutine showinp( vis, line,type, flagval,apply,ropt )

      character*(*) vis
      integer	    line(*)
      character*(*) type
      logical	    flagval
      logical	    apply
      character*(*) ropt

      logical	    equals
      character*80  outline
      character*132 fmt, rtfmt
      integer	    len1
      integer	    vals(2), nfigi

      if( ropt.eq.'none' )return

      call logwrit( ' ' )
      if(      flagval ) outline = 'Set flags to indicate good ' //
     *			 'data in visibility file ' // vis(:len1(vis))
      if( .not.flagval ) outline = 'Set flags to indicate bad ' //
     *			 'data in visibility file ' // vis(:len1(vis))
      call logwrit ( outline )

      if(.not.apply) call logwrit('CHANGES WILL NOT REALLY BE APPLIED')

      if( equals( type, 'channel,wide' ) )
     *then
	 if( line(1).eq.0 ) write( outline,
     *	     '( ''Linetype '',a, ''; select all channels'' )' )
     *	     type(:len1(type))
	 if( line(1).eq.1 ) write( outline,
     *	     '( ''Linetype '',a, ''; select channel '',i4     )' )
     *	     type(:len1(type)), int(line(2))
	 if( line(1).gt.1 ) write( outline,
     *	     '( ''Linetype '',a, ''; select '',i4,'' channels; '','//
     *		'''start '',i4,'', width '',i4,'', step '',i4  )' )
     *	     type(:len1(type)), line(1), line(2), line(3), line(4)
      elseif( equals( type, 'both' ) )
     *then
	 write( outline,
     *	   '(''All line channels and all wideband channels selected'')')
      endif
      call logwrit( outline )

      if( line(5).ne.0 .or. line(6).ne.0 ) then
	 vals(1) = nfigi( line(5) )
	 vals(2) = nfigi( line(6) )
	 fmt = rtfmt(
     *	 ' ''Set flags for the first '',i<>,'' and the last '',i<>,'//
     *	   ''' channels of each window'' ', vals,2 )
	 write( outline, fmt) line(5), line(6)
	 call logwrit( outline )
      endif
      if( line(7).ne.0 ) then
	 vals(1) = nfigi( line(7) )
	 fmt = rtfmt(
     *	 ' ''Set flags for the central '',i<>,'' channels'' ',
     *	   vals, 1 )
	 write( outline, fmt ) line(7)
	 call logwrit( outline )
      endif

      return
      end


c************************************************************************
c report writes out what happened. There are four levels of verbosity:
c 'none':	write nothing.
c 'brief':	do all counting of good, bad and changed flags but write
c		result only at end.
c 'indicative': for each visibility record write out some info on the
c		record and what happened to the flags.
c 'full':	write data, old and new flags for each channel in each
c		record. Produces enormous amounts of output unless
c		selection criteria are restrictive. Then useful to check
c		the data.

      subroutine report( ropt, unit,preamble, tformat, line,type,
     *			 data, oldflags, newflags, usech, nchan )

      character*(*)    ropt
      integer	       unit
      double precision preamble(*)
      character*(*)    tformat
      integer	       line(*)
      character*(*)    type
      complex	       data(*)
      logical	       oldflags(*), newflags(*)
      logical	       usech(*)
      integer	       nchan

c No reporting: return right away.
      if( ropt.eq.'none' ) return

c Construct report, output depending on report mode
      if( ropt.eq.'full' .or. ropt.eq.'indicative' )
     *	  call wrsumm( unit, preamble, type, ropt, tformat )

      if( ropt.eq.'full' )
     *	  call wrdata( data,oldflags,newflags, usech,nchan,line,type )

c Save which antennae were used
      call antusage( preamble(4) )

      return
      end


c***********************************************************************
c Indicative and full: for each record report uv coord, time, ants,
c number of good, bad and changed flags.
      subroutine wrsumm( unit, preamble, type, ropt, tformat )

      integer	       unit
      double precision preamble(*)
      character*(*)    type
      character*(*)    ropt, tformat

      double precision visno
      double precision u, v
      character*18     caltime
      integer	       ant1, ant2
      logical	       wrhead
      save	       wrhead
      character*79     outline
      integer	       tcount, counts(4), i
      data	       wrhead / .TRUE. /

c Get identification number of this record
      call uvinfo( unit, 'visno', visno )
c Decode preamble; change units of u and v to kilowavelengths; time
c coded as yymmmdd:hh:mm:ss; extract antennae from baselinenumber
      u = preamble(1) / 1000.
      v = preamble(2) / 1000.
      call julday( preamble(3), tformat, caltime )
      call basant( preamble(4), ant1, ant2 )

      if( wrhead )
     *then
	 call logwrit( ' ' )
	 outline = 'rec# lt	  u,v		  Time	     ' //
     *		   ' Ants  Originally  Currently'
	 call logwrit( outline )
	 if( tformat.eq.'H' ) outline =
     *		   '	    kilo wavelengths yymmmdd:hh:mm:ss ' //
     *		   '	  #good #bad #good #bad'
	 if( tformat.eq.'D' ) outline =
     *		   '	    kilo wavelengths yymmmdd.dd       ' //
     *		   '	  #good #bad #good #bad'
	 call logwrit( outline )
	 wrhead = ropt.eq.'full'
      endif
      do i = 1, 4
	 counts(i) = tcount(i,type)
      enddo
      write( outline, '('//
     *	     'i4,1x,	  a1,1x,     f8.4,1x, f8.4,1x, a,1x,'//
     *	     'i2,1x, i2,2x,'//
     *	     'i4,1x,		   i4,2x,    i4,1x,    i4	    )' )
     *	     int(visno), type(1:1), real(u), real(v), caltime(1:16),
     *	     ant1,  ant2, counts
      call logwrit( outline )

      return
      end


c***********************************************************************
c Full report: report for each channel the datavalue, the old and the
c new flag.
      subroutine wrdata( data,oldflags,newflags, usech,nchan,line,type )

      complex	       data(*)
      logical	       oldflags(*), newflags(*)
      logical	       usech(*)
      integer	       nchan, line(*)
      character*(*)    type

      character*25     head
      integer	       hdlen, len1
      integer	       n, off
      character*79     outline

      integer chan, i
      real    arg
      complex z
      real    radian

      data	       head / 'Chan Amplitude Phase Flag' /

      chan(i) = int( line(2) + (i-1)*line(4) )
      arg(z)  = atan2( aimag(z), real(z) ) * radian
      radian  = 180. / acos(-1.)

      call logwrit( ' ' )
      write( outline, '( ''Linetype: '', a )' ) type(:len1(type))
      call logwrit ( outline )
      write( outline, '( a,1x,a,1x,a )' ) head, head, head
      call logwrit ( outline )
      outline = ' '
      hdlen = len1( head ) + 1
      off   = -1
      do n = 1, nchan
	 if( usech(n) )
     *	 then
	    off = mod( off+1, 3 )
	    if( off.eq.0 ) outline = ' '
	    write( outline( off*hdlen+1 : off*hdlen+hdlen-1 ),
     *		   '( i4,1x,   f8.2,1x,       f6.1,1x,'//
     *		      'l1,''->'',   l1 )' )
     *		      chan(n), abs(data(n)), arg(data(n)),
     *		      oldflags(n), newflags(n)
	  endif
	  if( off.eq.2 ) call logwrit( outline )
      enddo
      if( off.ne.2 ) call logwrit( outline )
      return
      end

c***********************************************************************
c Keep track of which antennas were used.
c Entry antuse produces an output line with the result.

      subroutine antusage( antcode )

      implicit none
      double precision antcode

      integer	       ant1, ant2
      include	       'maxdim.h'
      logical	       antused ( MAXANT )
      save	       antused
      integer	       i, n
      character        line*64
      character        outline*(*)
c
c  Externals.
c
      integer	       len1
      character        itoaf*4

      data	       antused / MAXANT*.FALSE. /

      call basant( antcode, ant1, ant2 )
      if( ant1.lt.1 .or. ant1.gt.MAXANT ) then
	  write( line, '( ''Refers to antenna '',i2 )' ) ant1
	  call bug( 'w', line )
      else
	  antused( ant1 ) = .TRUE.
      endif
      if( ant2.lt.1 .or. ant2.gt.MAXANT ) then
	  write( line, '( ''Refers to antenna '',i2 )' ) ant2
	  call bug( 'w', line )
      else
	  antused( ant2 ) = .TRUE.
      endif
      return

      entry antuse( outline )
      outline = 'Antennas used:'
      i = len1( outline ) + 1
      do n = 1, MAXANT
	if( antused(n) )then
	  outline(i+1:i+2) = itoaf(n)
	  i = len1(outline(1:i+2)) + 1
	  outline(i:i) = ','
	endif
      enddo
      outline(i:i) = ' '
      end


c***********************************************************************
c Type an overview and update history to finish off

      subroutine overview( unit,vis, type, flagval,apply,ropt )

      integer	    unit
      character*(*) vis
      character*(*) type
      logical	    flagval
      logical	    apply
      character*(*) ropt

      character     ltype*16
      integer	    lt, lt1, lt2
      integer	    reccount, treccnt
      integer	    totcount, totcnt(6), i, l
      character     outline*256
c
c  Externals.
c
      character     itoaf*8
      integer	    len1

      if( ropt.eq.'none' ) return

      call lhwr( 'open', unit, apply )

      outline = 'Overview of flagging on visibility file ' //
     *		 vis(:len1(vis))
      call lhwr( outline, unit, apply )

      if( .not.apply ) call logwrit( 'CHANGES WERE NOT REALLY APPLIED' )

      if(      flagval ) outline =
     *	  'Changed flags set to TRUE (data flagged as good)'
      if( .not.flagval ) outline =
     *	  'Changed flags set to FALSE (data flagged as bad)'
      call lhwr( outline, unit, apply )

      call nrecords( reccount, 1 )
      call nrecords( treccnt,  2 )
      outline = 'Total number of records selected: '//itoaf(reccount)
      l = len1(outline)
      outline(l+1:) = '; out of '//itoaf(treccnt)
      l = len1(outline)
      outline(l+1:) = ' records'
      call lhwr( outline, unit, apply )

      call antuse( outline )
      call lhwr( outline, unit, apply )

      if( type.eq.'both'    ) lt1 = 1
      if( type.eq.'both'    ) lt2 = 2
      if( type.eq.'channel' ) lt1 = 1
      if( type.eq.'channel' ) lt2 = 1
      if( type.eq.'wide'    ) lt1 = 2
      if( type.eq.'wide'    ) lt2 = 2
      call lhwr('Counts of correlations within selected channels',
     *	  unit,apply)

      do lt = lt1, lt2

	 if( lt.eq.1 ) ltype = 'channel'
	 if( lt.eq.2 ) ltype = 'wide'
	 write( outline, '( a8,''  Originally  Currently'')') ltype
	 call lhwr( outline, unit, apply )

	 do i = 1, 6
	    totcnt(i) = totcount(i,lt)
	 enddo
	 write( outline, '( ''Good:  '', 3x, i10, 1x, i10 )' )
     *	       totcnt(1), totcnt(3)
	 if( .not.flagval ) write( outline( len1(outline)+1 : ), '('//
     *	       '4x, ''Changed to bad: '', i10 )' ) totcnt(5)
	 call lhwr( outline, unit, apply )

	 write( outline, '( ''Bad:   '', 3x, i10, 1x, i10 )' )
     *		totcnt(2), totcnt(4)
	 if(	  flagval ) write( outline( len1(outline)+1 : ), '('//
     *		'4x, ''Changed to good: '',i10 )' ) totcnt(6)
	 call lhwr( outline, unit, apply )

       enddo
c	  write( outline, '(
c     *        ''Number of good channels originally: '',i17 )' )
c     * 	 totcnt(1)
c	  call lhwr( outline, unit, apply )
c	  write( outline, '(
c     *        ''Number of good channels currently:  '',i17 )' )
c     * 	 totcnt(3)
c	  call lhwr( outline, unit, apply )
c	  write( outline, '(
c     *        ''Number of bad	channels originally: '',i17 )' )
c     * 	 totcnt(2)
c	  call lhwr( outline, unit, apply )
c	  write( outline, '(
c     *        ''Number of bad	channels currently:  '',i17 )' )
c     * 	 totcnt(4)
c	  call lhwr( outline, unit, apply )
c	  if( .not.flagval ) write( outline, '(
c     *        ''Number of channels changed from good to bad: '',i8 )' )
c     * 	 totcnt(5)
c	  if(	   flagval ) write( outline, '(
c     *        ''Number of channels changed from bad to good: '',i8 )' )
c     * 	 totcnt(6)
c	  call lhwr( outline, unit, apply )
      call lhwr( 'close', unit, apply )

      return
      end


      subroutine lhwr( outline, unit, apply )
      character*(*) outline
      integer	    unit
      logical	    apply
      character*80  outline2
      if( outline .eq. 'open' ) then
	 call logwrit( ' ' )
	 if( apply ) call hisopen(  unit, 'append' )
	 if( apply ) call hisinput( unit, 'uvflag' )
      elseif( outline .eq. 'close' ) then
	 if( apply ) call hisclose( unit )
      else
	 call logwrit( outline )
	 outline2 = 'UVFLAG: ' // outline
	 if( apply ) call hiswrite( unit, outline2 )
      endif
      return
      end

