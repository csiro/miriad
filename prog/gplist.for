c************************************************************************
	program gplist
	implicit none
c
c= GpList -- List gains table and optionally overwrite calibration
c& smw
c: calibration
c+
c     GpList is a Miriad task to list the amplitude gains in a gains table
c     and (optionally) replace the amplitude information in a gains
c     table with constants of the users choosing, and/or simultaneously 
c	setting the phase corrections to zero.
c
c     WARNING: if you modify the gains, the resulting amplitude gains will be 
c              constant with time! The original gains table is overwritten.
c
c     The motivation for this routine arose from datasets in which the
c     calibrator is weak so that conventional amplitude calibration was not
c     very successful. This was compounded by antenna 3 being blown about 
c     by the wind. The source, on the other hand, was strong but time
c     variable, hence not susceptible to self-cal. From observations of
c     strong planets before and after it was observed that the amplitude
c     scale was reasonably stable, and certainly more stable than implied
c     by the phase-calibrator data, so that only phase calibration really needs
c     to be applied to the source. This program allows the amplitude scale
c     to be forced to the desired values (in this case, obtained from the
c     planet observations) without changing the phase calibration.
c       
c@ vis
c     The input visibility file, containing the gain file to list/massage
c@ options
c	  amp      List the amplitude gains for 10 antennas: default option
c	           The mean, median and rms (about the mean) are reported.
c	  phase    List the phase corrections for 10 antennas.
c	  complex  List complex gains for current 10 ants only (2 lines per soln)
c	  all      List all complex gains (one line per antenna per solution
c	           for all antennas; lots of output, better than
c	           options=complex if you want to grep one one antenna.)
c	  replace  Replace the amplitude gains with the list supplied 
c	           Unless OPTIONS=FORCE is also set, only antennas with 
c                  non-zero values in the list are affected
c	           so if jyperk is not set, nothing happens. Phases are
c	           preserved unless options=zerophas is also specified
c	  force    if set, then all values in jyperk are enforced when
c                  doing a replace, even if they (or the initial gains)
c                  are zero
c	  limit    impose an upper limit on the amplitude gains using the
c	           list specified in jyperk
c	  multiply Multiply existing sqrt(Jy/K) values in a gains table by
c	           the list supplied in the jyperk variable. Only antennas
c	           corresponding to nonzero jyperk elements are changed.
c	           No effect on phases.
c	  zerophas Zero all phase corrections (no antenna selection method)
c	  clip     Set to zero all gains outside range jyperk(1),jyperk(2)
c                  Useful for pseudo-flagging of bad data, e.g.,
c                  gplist vis=dummy options=clip jyperk=0.5,2.0
c                  effectively flags data with gains outside 0.5-2 (default 
c                  range). However, data are not really flagged. The next
c                  option is an alternative "flagging" option.
c	  sigclip  Set to zero all gains more than jyperk(1)*rms away from 
c	           median on each antenna
c       
c	  Use options=replace,zerophas with suitable jyperk list to 
c	  both set amp scale and zero phases (the two steps are 
c	  carried out sequentially with the amplitudes being set first)
c@ jyperk 
c     Array of 12 numbers (1 per antenna) giving the Jy-per-K values.
c     Array elements default to zero so you don't have to give 12 numbers.
c     Action will only be taken for antennas corresponding to nonzero 
c     elements of jyperk.
c     Used for options=replace or options=multiply. For options=replace,
c     if any of the numbers are zero then the amp gains in the pre-existing
c     table will not be changed, so you can change the gains on a single
c     antennna without changing the others by setting all the other
c     values to zero. However, be aware that your one bad antenna will
c     have affected the solutions for the other antennas as well.
c     For options=multiply, jyperk supplies a list of multiplication
c     factors (one per antenna) which will be used to multiply the 
c     sqrt(Jy/K) amplitude gains in the existing table. 
c  
c--
c  History:
c    smw     23feb95 Original version: cloned from Bob's gpaver, 
c                    complete with occasional vulgarity
c    smw     25feb95 Added 'complex' and 'zerophas' options
c    smw     25may95 Added 'multiply' option
c    smw     07sep95 Converted to 12 antennas
c    smw     01jan96 Added phase option, deleted redundant solarfix option
c    smw     17feb96 Compiled at Hat Creek and prettied up some things
c    rjs     15may96 Trivial FORTRAN standardisation.
c    smw     17dec96 Added 'limit' option 
c    smw     04nov97 Upgraded hardcoded output (necessary to fit
c                    everything into 80 character lines) to 10 antennas 
c                    with real antenna 2
c    smw     27may99 Added median output at Kartik's suggestion
c    smw     15jul99 Added rms output at Kartik's request
c    smw     06aug99 Changed output format at Kartik's request
c    smw     16aug99 Added "clip" option 
c    smw     19aug99 Added "sigclip" option 
c    smw     30aug99 Added "force" option 
c    pjt     27jul00 Fixed bug in options=phase for 10th ant
c    pjt      4aug00 smw generously allowed me to fix the write-history
c   		     'bug' when nothing was modified
c    smw     21nov03 Modified "force" option to enforce any value
c
c  Bugs and Shortcomings:
c    Like gpaver, gplist is hardwired for 12 antennas!
c    This will have to be changed when expansion occurs
c-----------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='GpList: version 2.0b 22-nov-03')
	logical dovec,docomp,dophas,doall,dozero,domult,hexists,doamp
      logical dolimit,doclip,dosigclip,doforce,dohist
	real jyperk(12) 
	character vis*80,msg*80
	integer ngains,nfeeds,ntau,nants,iostat,njyperk
	integer tVis
      data jyperk /0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call mkeyr('jyperk',jyperk,12,njyperk)
	call GetOpt(doamp,dovec,docomp,dophas,doall,dozero,domult,
     *            dolimit,doclip,dosigclip,doforce)
	call keyfin
	if(vis.eq.' ')call bug('f','An input file must be given')
c
c  Open the visibility file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
	call hopen(tVis,vis,'old',iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error opening '//vis)

      IF (.not.hexists(tVis,'gains')) then
       CALL output(' ')
       msg='There are no gains present in this dataset!!!'
       CALL output(msg)
       msg='To create a dummy gains table, run gperror!!!'
       CALL output(msg)
       CALL output(' ')
       call bug('f','Aborting now.')
      ENDIF

      IF (dovec.and.domult) then
       CALL output(' ')
       msg='options=replace,multiply should not be done together !!!'
       CALL output(msg)
       msg='You must make separate runs for each (both use jyperk)!!!'
       CALL output(msg)
       CALL output(' ')
       call bug('f','Aborting now.')
      ENDIF

c
c
c  Determine the number of feeds in the gain table.
c
	call rdhdi(tVis,'ngains',ngains,0)
	call rdhdi(tVis,'nfeeds',nfeeds,1)
	call rdhdi(tVis,'ntau',  ntau,  0)
	if(nfeeds.le.0.or.nfeeds.gt.2.or.mod(ngains,nfeeds+ntau).ne.0
     *	  .or.ntau.gt.1.or.ntau.lt.0)
     *	  call bug('f','Bad number of gains or feeds in '//vis)
	nants = ngains / (nfeeds + ntau)
      write(6,*) "Found gain entries for ",nants," antennas."
c
c  List/Replace the gains now.
c
	call ReplGain(tVis,dohist,
     *        doamp,dovec,docomp,dophas,doall,dozero,domult,dolimit,
     *        doclip,dosigclip,doforce,nfeeds,ntau,nants,jyperk)
c
c  Write out some history now.
c
      if(dohist) then
	call hisopen(tVis,'append')
	call hiswrite(tVis,'GPLIST: Miriad '//version)
	call hisinput(tVis,'GPLIST')
	call hisclose(tVis)
      endif
c
c  Close up everything.
c
	call hclose(tVis)	
	end
c************************************************************************
       subroutine GetOpt(doamp,dovec,docomp,dophas,doall,dozero,
     &                   domult,dolimit,doclip,dosigclip,doforce)
c
	implicit none
	logical doamp,dovec,docomp,dophas,doall,dozero,domult,
     &        dolimit,doclip,dosigclip,doforce
c
c  Get "Task Enrichment Parameters".
c
c  Output:
c    dovec	Replace amplitude gains
c    docomp List complex gains for 6 current ants
c    dozero Zero phase corrections
c    doall  full list of complex gains
c    dophas List phase gains
c    domult Multiply amplitude gains
c    dolimit Impose upper limit on gains
c    doclip Set amp gain to zero if outside absolute "normal" range
c    dosigclip Set amp gain to zero if outside relative "normal" range
c    doforce Force use of zeroes in jyperk array.
c------------------------------------------------------------------------
	integer nopts
	parameter(nopts=11)
	logical present(nopts)
	character opts(nopts)*8
      data opts
     &/'amp     ','complex ','replace ','zerophas','all     ',
     & 'phase   ','multiply','limit   ','clip    ','sigclip ',
     & 'force   '/
c
	call options('options',opts,present,nopts)
      docomp = present(2)
	dovec = present(3)
      dozero = present(4)
      doall = present(5)
      dophas = present(6)
      domult = present(7)
      dolimit = present(8)
      doclip = present(9)
      dosigclip = present(10)
      doforce = present(11)
      doamp = present(1).or.present(10).or.(.not.
     &(docomp.or.dovec.or.dophas.or.doall.or.dozero.or.domult.
     &        or.dolimit.or.doclip))
c
	end
c************************************************************************
	subroutine AverBug(iostat,message)
c
	implicit none
	integer iostat
	character message*(*)
c
c  Give an error message, and bugger off.
c------------------------------------------------------------------------
	call bug('w',message)
	call bugno('f',iostat)
	end
c***********************************************************************
      subroutine ReplGain(tVis,dohist,doamp,dovec,docomp,dophas,doall,
     *              dozero,domult,dolimit,doclip,dosigclip,doforce,
     *              nfeeds,ntau,nants,jyperk)
c
      implicit none
      logical doamp,dovec,docomp,dophas,doall,dozero,domult,dolimit,
     *        doclip,dosigclip,doforce,dohist
      integer nfeeds,ntau,nants,tVis,j,jant(12),k,jind(3600)
      real jyperk(12),dbcor(12),MeanGain(12),radtodeg,GainArr(12,3600),
     *     MednGain(12), MedArr(3600), GainRms(12), mingain, maxgain
      logical doMed
      data doMed /.false./
c
c  Read and write the gains, and list gains and replace amplitudes
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXSOLS,MAXGAINS
	parameter(MAXSOLS=10000,MAXGAINS=3*MAXSOLS*MAXANT)
	complex Gains(MAXGAINS)
	double precision time(MAXSOLS)
	integer nsols,offset,pnt,i,tGains,iostat,ngains
      character line*128,ctime*8,msg*80
c
c  Externals.
c
	integer hsize
c
      data dbcor 
     *     /1.87,0.00,1.88,2.25,2.00,2.75,1.70,0.00,0.00,0.0,0.0,0.0/
c
c  Open the gains table and read them all in.
c
	dohist = .FALSE.
	call haccess(tVis,tGains,'gains','read',iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error opening the gains')
	nsols = (hsize(tGains)-8)/(8*nants*(nfeeds+ntau)+8)
	if(nsols.gt.MAXSOLS.or.nsols*nants*(nfeeds+ntau).gt.MAXGAINS)
     *	  call bug('f','Gain table too big for me to handle')
c
	ngains = nants*(nfeeds+ntau)
	offset = 8
	pnt = 1
	do i=1,nsols
	  call hreadd(tGains,time(i),offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0)call hreadr(tGains,Gains(pnt),offset,8*ngains,
     *								 iostat)
	  pnt = pnt + ngains
	  offset = offset + 8*ngains
	  if(iostat.ne.0)call Averbug(iostat,'Error reading gain table')
	enddo
c
c  Close up.
c
	call hdaccess(tGains,iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error closing gain table')
c  
c  Now list the values read
c

      if (docomp) then
         call output('The complex gains listed in the table are:')
         write(msg(1:37),94) '  Time     Ants 1/6     Ants 2/7     '
         write(msg(38:76),94) 'Ants 3/8     Ants 4/9     Ants 5/10    '
         call output(msg)
         do i=1,nsols
            call JulDay(time(i),'H',line(1:18))
            ctime = line(9:16)
            write(msg,95) ctime,Gains((i-1)*nants+1),
     *                          Gains((i-1)*nants+2),
     *                          Gains((i-1)*nants+3),
     *                          Gains((i-1)*nants+4),
     *                          Gains((i-1)*nants+5)
            call output(msg)
            write(msg,95) '   ',Gains((i-1)*nants+6),
     *                          Gains((i-1)*nants+7),
     *                          Gains((i-1)*nants+8),
     *                          Gains((i-1)*nants+9),
     *                          Gains((i-1)*nants+10)
            call output(msg)
         enddo
      else if (doall) then
         call output('The complex gains listed in the table are:')
         do i=1,nsols
            call JulDay(time(i),'H',line(1:18))
            ctime = line(9:16)
         write(msg,96) ctime,'Ant  ',1,'   gain = ',Gains((i-1)*nants+1)
            call output(msg)
            do j=2,nants
               write(msg,97) 'Ant  ',j,'   gain = ',Gains((i-1)*nants+j)
               call output(msg)
            enddo
         enddo
      else if (dophas) then
         call output('The phase gain values listed in the table are:')
         write(msg(1:35),94) '  Time     Ant 1 Ant 2 Ant 3 Ant 4 '
         write(msg(36:71),94) 'Ant 5 Ant 6 Ant 7 Ant 8 Ant 9 Ant 10'
         call output(msg)
         radtodeg=180.0/3.14159
         do i=1,nsols
            call JulDay(time(i),'H',line(1:18))
            ctime = line(9:16)
            k=(i-1)*nants
            write(msg,198) ctime,
     *        int(radtodeg*atan2(AImag(Gains(k+1)),Real(Gains(k+1)))),
     *        int(radtodeg*atan2(AImag(Gains(k+2)),Real(Gains(k+2)))),
     *        int(radtodeg*atan2(AImag(Gains(k+3)),Real(Gains(k+3)))),
     *        int(radtodeg*atan2(AImag(Gains(k+4)),Real(Gains(k+4)))),
     *        int(radtodeg*atan2(AImag(Gains(k+5)),Real(Gains(k+5)))),
     *        int(radtodeg*atan2(AImag(Gains(k+6)),Real(Gains(k+6)))),
     *        int(radtodeg*atan2(AImag(Gains(k+7)),Real(Gains(k+7)))),
     *        int(radtodeg*atan2(AImag(Gains(k+8)),Real(Gains(k+8)))),
     *        int(radtodeg*atan2(AImag(Gains(k+9)),Real(Gains(k+9)))),
     *        int(radtodeg*atan2(AImag(Gains(k+10)),Real(Gains(k+10))))
            call output(msg)
         enddo
      else if (doamp) then
         do j=1,nants
            MeanGain(j)=0.0
            GainRms(j)=0.0
            jant(j)=0
         enddo
      call output('The amplitude gain values listed in the table are:')
         write(msg(1:35),94) '  Time     Ant 1 Ant 2 Ant 3 Ant 4 '
         write(msg(36:71),94) 'Ant 5 Ant 6 Ant 7 Ant 8 Ant 9 Ant 10'
         call output(msg)
         do i=1,nsols
            call JulDay(time(i),'H',line(1:18))
            ctime = line(9:16)
            write(msg,199) ctime,abs(Gains((i-1)*nants+1)),
     *                  abs(Gains((i-1)*nants+2)),
     *                  abs(Gains((i-1)*nants+3)),
     *                  abs(Gains((i-1)*nants+4)),
     *                  abs(Gains((i-1)*nants+5)),
     *                  abs(Gains((i-1)*nants+6)),
     *                  abs(Gains((i-1)*nants+7)),
     *                  abs(Gains((i-1)*nants+8)),
     *                  abs(Gains((i-1)*nants+9)),
     *                  abs(Gains((i-1)*nants+10))
            call output(msg)
            do j=1,nants
               if (abs(Gains((i-1)*nants+j)).gt.0.0) then
                  MeanGain(j)=MeanGain(j)+abs(Gains((i-1)*nants+j))
                  GainRms(j)=GainRms(j)+abs(Gains((i-1)*nants+j))**2
                  jant(j)=jant(j)+1
                  GainArr(j,jant(j))=abs(Gains((i-1)*nants+j))
               endif
            enddo
         enddo
      do j=1,nants
         if (jant(j).gt.0) MeanGain(j)=MeanGain(j)/jant(j)
         if (jant(j).gt.2) then
            doMed = .true.
            do k=1,jant(j)
               MedArr(k)=GainArr(j,k)
            enddo
            call sortidxr( jant(j), MedArr, jind)
            MednGain(j)=MedArr(jind(int(jant(j)/2)))
            GainRms(j)=
     * sqrt((GainRms(j)-jant(j)*MeanGain(j)*MeanGain(j))/(jant(j)-1))
         else
            GainRms(j)=0.0
         endif
      enddo
      write(msg,197) '------------------------------------',
     &               '------------------------------------'
      call output(msg)
      write(msg,199) 'Means:  ',MeanGain(1),MeanGain(2),MeanGain(3),
     *                          MeanGain(4),MeanGain(5),MeanGain(6),
     *              MeanGain(7),MeanGain(8),MeanGain(9),MeanGain(10)
      call output(msg)
      if (doMed) then
        write(msg,199) 'Medians:',MednGain(1),MednGain(2),MednGain(3),
     *                            MednGain(4),MednGain(5),MednGain(6),
     *                MednGain(7),MednGain(8),MednGain(9),MednGain(10)
        call output(msg)
        write(msg,199) 'Rms:    ',GainRms(1),GainRms(2),GainRms(3),
     *                            GainRms(4),GainRms(5),GainRms(6),
     *                GainRms(7),GainRms(8),GainRms(9),GainRms(10)
        call output(msg)
      endif
      write(msg,197) '------------------------------------',
     &               '------------------------------------'
      call output(msg)
      endif
c
c  Do the replacement of current amp corrections with specified list
c
      if (dovec) then
         dohist = .TRUE.
         if (doforce) then
         msg='Replacing amplitude gains with (all values enforced):'
         call output(msg)
         else
         msg='Replacing amplitude gains with (0.0 means no change):'
         call output(msg)
         end if
         write(msg,99) '        ',jyperk(1),jyperk(2),jyperk(3),
     *     jyperk(4),jyperk(5),jyperk(6),jyperk(7),jyperk(8),
     *     jyperk(9),jyperk(10)
         call output(msg)
         do i=1,nsols
            do j=1,nants
        if (Gains((i-1)*nants+j).ne.cmplx(0.0,0.0)) then
               Gains((i-1)*nants+j)=
     *       jyperk(j)*Gains((i-1)*nants+j)/abs(Gains((i-1)*nants+j))
          else if (doforce) then
               Gains((i-1)*nants+j)=cmplx(jyperk(j),0.0)
          end if
            enddo
         enddo
      endif
c
c  Zero all phases
c
      if (dozero) then
         dohist = .TRUE.
         msg='Zeroing all phases: use options=complex to check.'
         call output(msg)
         do i=1,nsols
            do j=1,nants
               if (Gains((i-1)*nants+j).ne.cmplx(0.0,0.0))
     *        Gains((i-1)*nants+j)=cmplx(abs(Gains((i-1)*nants+j)),0.0)
            enddo
         enddo
      endif
c
c  Multiply amplitudes by arbitrary numbers supplied in jyperk
c
      if (domult) then
         dohist = .TRUE.
         msg='Multiplying sqrt(Jy/K) by (1 per antenna):'
         call output(msg)
         write(msg,99) 'sqrt(Jy/K) x ',jyperk(1),jyperk(2),jyperk(3),
     *     jyperk(4),jyperk(5),jyperk(6),jyperk(7),jyperk(8),
     *     jyperk(9),jyperk(10)
         call output(msg)
         do i=1,nsols
            do j=1,nants
        if (Gains((i-1)*nants+j).ne.cmplx(0.0,0.0).and.jyperk(j).ne.0.0) 
     *   Gains((i-1)*nants+j)=jyperk(j)*Gains((i-1)*nants+j)
            enddo
         enddo
      endif
c
c  Impose upper limit on amp gains using numbers supplied in jyperk
c
      if (dolimit) then
         dohist = .TRUE.
         msg='Imposing upper limits on gains of:'
         call output(msg)
         write(msg,99) ' ',jyperk(1),jyperk(2),jyperk(3),
     *     jyperk(4),jyperk(5),jyperk(6),jyperk(7),jyperk(8),
     *     jyperk(9),jyperk(10)
         call output(msg)
         do i=1,nsols
            do j=1,nants
        if (Gains((i-1)*nants+j).ne.cmplx(0.0,0.0).and.jyperk(j).ne.0.0.
     *       and.abs(Gains((i-1)*nants+j)).gt.jyperk(j)) 
     *   Gains((i-1)*nants+j)=
     *      jyperk(j)*Gains((i-1)*nants+j)/abs(Gains((i-1)*nants+j))
            enddo
         enddo
      endif
c
c  "Clip": set gains to zero if outside "normal" range
c
      if (doclip) then
         dohist = .TRUE.
         k = 0
         if (jyperk(1).eq.0.0) jyperk(1)=0.5
         if (jyperk(2).eq.0.0) jyperk(2)=2.0
         write(msg,93) 'Clipping gains outside range:',jyperk(1),
     *                 jyperk(2)
         call output(msg)
         do i=1,nsols
            do j=1,nants
        if (Gains((i-1)*nants+j).ne.cmplx(0.0,0.0).and.
     *       ((abs(Gains((i-1)*nants+j)).lt.jyperk(1)).or.
     *         abs(Gains((i-1)*nants+j)).gt.jyperk(2))) then
                  Gains((i-1)*nants+j)=cmplx(0.0,0.0)
                  k = k + 1
               end if
            enddo
         enddo
         write(msg,92) 'Clipped ',k,' antenna-interval pairs.'
         call output(msg)
      endif
c
c  "SigClip": set gains to zero if outside relative "normal" range
c             doamp must have already run to get Median and Rms
c
      if (dosigclip) then
         dohist = .TRUE.
         k = 0
         if (jyperk(1).eq.0.0) jyperk(1)=3.0
         write(msg,91) 'Clipping gains outside range:',jyperk(1),
     *                  ' sigma'
         call output(msg)
         do j=1,nants
            mingain = MednGain(j)-jyperk(1)*GainRms(j)
            maxgain = MednGain(j)+jyperk(1)*GainRms(j)
            do i=1,nsols
        if (Gains((i-1)*nants+j).ne.cmplx(0.0,0.0).and.
     *     (GainRms(j).ne.0.0).and.
     *       ((abs(Gains((i-1)*nants+j)).lt.mingain).or.
     *         abs(Gains((i-1)*nants+j)).gt.maxgain)) then
                  Gains((i-1)*nants+j)=cmplx(0.0,0.0)
                  k = k + 1
               end if
            enddo
         enddo
         write(msg,92) 'Clipped ',k,' antenna-interval pairs.'
         call output(msg)
      endif


      if (.NOT.dohist) return
c
c  Now write out the new gain solutions.
c
	call wrhdi(tVis,'nsols',nsols)
	call haccess(tVis,tGains,'gains','write',iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error reopening gain table')
c
	call hwritei(tGains,0,0,4,iostat)
	if(iostat.ne.0)
     *	  call AverBug(iostat,'Error writing gain table preamble')
c
	offset = 8
	pnt = 1
	do i=1,nsols
	  call hwrited(tGains,time(i),offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0)call hwriter(tGains,Gains(pnt),offset,8*ngains,
     *								 iostat)
	  pnt = pnt + ngains
	  offset = offset + 8*ngains
	  if(iostat.ne.0)call Averbug(iostat,'Error writing gain table')
	enddo
c
	call hdaccess(tGains,iostat)
	if(iostat.ne.0)call AverBug(iostat,'Error reclosing gain table')
c
199   format(a8,2x,10f6.3)
198   format(a8,2x,10i6)
197   format(a36,a36)
99    format(a8,2x,10(f5.2,1x))
97    format(10x,a,i2,a,f9.3,f9.3)
96    format(a8,2x,a,i2,a,f9.3,f9.3)
95    format(a8,1x,5(f5.2,1x,f5.2,2x))
94    format(a)
93    format(a30,1x,f5.2,1x,f5.2)
92    format(a9,1x,i4,a25)
91    format(a30,1x,f5.2,1x,a6)
	end
c************************************************************************
