      program uvacflag
c--------------------------------------------------------------------------
c     UVACFLAG will flag all cross correlations corresponding to flagged
c     auto correlations.  
c
c= uvacflag - Flag crosscorrelations when auto correlations are flagged.
c
c& mhw
c: calibration
c+
c     UVACFLAG will flag all cross correlations corresponding to flagged
c     auto correlations.  This can be useful, for example,
c     if you run TVFLAG, and flag data based upon the auto correlations.
c     You then use UVACFLAG and flag all other 
c     correlations at the same time when the auto correlations 
c     are flagged. 
c
c@ vis
c	Input visibility file. No default.
c@ options
c	Task enrichment options.  Minimum match is active.
c
c	full    Instructs UVACFLAG to tell you about every record
c	        it flags.  The default is a summary at the end.
c--
c
c     mhw  19nov07 Original version, adapted uvpflag into uvacflag
c---------------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      integer maxpol,maxbin,maxrec,maxtime,maxfreq,ntime,nfreq
      integer match_time,match_freq
      parameter (maxpol = 4, maxbin=32, maxrec=10000000,
     +           maxtime=10000, maxfreq=100)
      character version*(*)
      parameter (version='UvACFlag: Version 19-Nov-07')
c
      complex data(maxchan)
      double precision pream(4), freq,df, timelist(maxtime),
     * freqlist(maxfreq,2)
      integer nchanlist(maxfreq)
      real scratch(maxchan)
      integer lin, lins, npol, i, nchan, nscr, irec,
     +  offset, ant1, ant2, nant, nbin, nif, lastit, it
      integer ibin,iif,ipol,iant1,iant2,b,r,pol,bin,ifno
      character in*40, ctime*18
      logical flags(maxchan), some, brief
      integer flagoff(2,maxrec), offauto(MAXANT,maxpol,MAXWIN,maxbin)
      integer recnum(MAXBASE,maxpol,MAXWIN,maxbin),polidx(4)
      character  aline*100
      integer ntot,ngood,ngood2,ngood3,nflag
c
c-----------------------------------------------------------------------
      call output(version)
      
      ntot=0
      ngood=0
      nflag=0

c
c Get inputs
c
      call keyini
c
      call keya ('vis', in, ' ')
      if (in.eq.' ') call bug ('f', 'Visibility file name not given')
      call getopt (brief)
      call keyfin
c
c Open files
c
      call uvopen (lin, in, 'old')
      call scropen (lins)
c
c Loop over visibilities in time order, write a copy of the autocorr flags
c to a scratch file and remember the file offsets. Also keep track of the
c record number for each cross correlation.
c After each time slot, figure out the corresponding autocorr flag offsets
c for each cross correlation record. On a second pass through the data
c flag the cross correlations using the info collected.
c
      call uvset(lin, 'preamble', 'time/baseline/pol',0,0.0,0.0,0.0)
      call uvread (lin, pream, data, flags, maxchan, nchan)
      call countit(flags,nchan,ntot,ngood)
      irec = 1
      flagoff(1,irec)=-1
      flagoff(2,irec)=-1
      if (nchan.eq.0) call bug ('f', 'No data in this file')
      offset = 0
      nscr = 0
      lastit = 0
      nbin=maxbin
      nif=MAXWIN
      nant=MAXANT
      npol=maxpol
      ntime=0
      nfreq=0
c
      do while (nchan.gt.0)
c
c Extract info from first visibility in the group of polarizations
c
        it = match_time(pream(1),.true.,timelist,ntime,maxtime)
c
c New time slot encountered
c        
        if (it.ne.lastit) then
          lastit=it
          do ibin = 1,nbin
            do iif = 1, nif
              do ipol = 1, npol
                do iant1 = 1, nant-1
                  do iant2 = iant1+1, nant
                    b=iant2*(iant2+1)/2+iant1-iant2
                    r=recnum(b,ipol,iif,ibin)
                    if (r.gt.0) then
                      flagoff(1,r)=offauto(iant1,ipol,iif,ibin)
                      flagoff(2,r)=offauto(iant2,ipol,iif,ibin)
                      if (.not.brief.and.
     +                    (flagoff(1,r).ge.0.or.flagoff(2,r).ge.0)) then
   10                   format('Flagging record ',i9,' baseline ',
     +                       i3,'-',i3)                
                        write(aline,10) r,iant1,iant2
                        call output(aline)
                      endif
                    endif
                  enddo
                enddo
                do iant1 = 1, nant
                  offauto(iant1,ipol,iif,ibin)=-1
                enddo
                do b=1,MAXBASE
                  recnum(b,ipol,iif,ibin)=-1
                enddo
              enddo
            enddo
          enddo
          nbin=0
          nif=0
          nant=0
          npol=0
          do i=1,4
            polidx(i)=0
          enddo
        endif
        
        ant1=pream(2)/256
        ant2=mod(pream(2),256)
        pol= pream(3)
        ipol=0
        do i=1,npol
          if (pol.eq.polidx(i)) ipol=i
        enddo
        if (ipol.eq.0) then
          npol=npol+1
          polidx(npol)=pol
          ipol=npol
        endif
        call uvrdvri(lin,'bin',bin,1)
        call uvrdvrd(lin,'sfreq',freq,0.0)
        call uvrdvrd(lin,'sdf',df,0.0)
        ifno = match_freq(freq,df,nchan,.true.,freqlist,nchanlist,
     *                    nfreq,maxfreq)
        nbin = max(nbin,bin)
        nif = max(nif,ifno)
        nant = max(nant,ant1)
        nant = max(nant,ant2)
        
c
c  keep copy of auto correlation flags 
c        
          if (ant1.eq.ant2) then

            call JulDay(pream(1),'H',ctime)
c
c Are any of the channels actually bad ; .false. = -1 = yes.
c Convert from logical to real for scratch file.
c
            some = .false.
            do i = 1, nchan
              scratch(i) = 1.0
              if (.not.flags(i)) then
                some = .true.
                scratch(i) = -1.0
              end if
            enddo
c
c Write out scratch record if there were some flags found
c
            if (some) then
              call scrwrite (lins, scratch, offset, nchan)
              offauto(ant1,ipol,ifno,bin)=offset
              offset = offset + nchan
              nscr = nscr + 1
c              write(aline,60) ' flags for ',ant1,ipol,ifno,bin,irec
c 60           format(a,4i3,i6)
c              call output(aline)
            end if
          else
          
c
c Save record number of cross correlations
c
            if (ant2.gt.ant1) then
              b=ant2*(ant2+1)/2+ant1-ant2
            else 
              b=ant1*(ant1+1)/2+ant2-ant1
            endif
            recnum(b,ipol,ifno,bin)=irec
          endif
c
c Get another visibility
c
        call uvread (lin, pream, data, flags, maxchan, nchan)
        call countit(flags,nchan,ntot,ngood)
        irec = irec + 1
        flagoff(1,irec)=-1
        flagoff(2,irec)=-1
        if (irec.gt.maxrec) call bug('f','Too many records in file')
      end do
      if (nscr.eq.0) call bug ('f', 
     +  'There were no flagged records in the auto correlations')
c
c
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
c
c Now read through data again, this time flagging  
c cross correlations when there are flags stored in 
c the scratch file
c
      write(aline,70) ' Found ',nscr,
     + ' autocorrelation records with flags'
  70  format(a,i6,a)
      call output(aline)
      call uvrewind (lin)      
      call uvread (lin, pream, data, flags, maxchan, nchan)
      ngood2=0
      call countit(flags,nchan,i,ngood2)
      irec=1
      nflag=0
      
      do while (nchan.gt.0)
        if (flagoff(1,irec).ge.0.or.flagoff(2,irec).ge.0) then
          if (flagoff(1,irec).ge.0) then
            call scrread(lins,scratch,flagoff(1,irec),nchan)
            do i=1,nchan
              if (scratch(i).lt.0) flags(i)=.false.
            enddo
          endif
          if (flagoff(2,irec).ge.0) then
            call scrread(lins,scratch,flagoff(2,irec),nchan)
            do i=1,nchan
              if (scratch(i).lt.0) flags(i)=.false.
            enddo
          endif
c          write(aline,80) 'writing flags for rec ',irec
c 80       format(a,i6)
c          call output(aline)
          ngood3=0
          call countit(flags,nchan,i,ngood3)
          nflag=nflag+ngood2-ngood3
          call uvflgwr(lin,flags)
        endif
        call uvread (lin, pream, data, flags, maxchan, nchan)
        ngood2=0
        call countit(flags,nchan,i,ngood2)
        irec=irec+1
      enddo 
      
c
c Flagging summary
c 
      call output(' Overview of flagging on visibility file '//in)     
      call output(' Correlations: Total   Good      Bad')
      write(aline,'(a,i9,i9,i9)')' Before:    ',
     *      ntot,ngood,ntot-ngood
      call output(aline)
      write(aline,'(a,i9,i9,i9)')' After:     ',
     *      ntot,ngood-nflag,ntot-ngood+nflag
      call output(aline)
c
c Write the history.
c
      call hisopen (lin, 'append')
      call hiswrite (lin, 'UVACFLAG: Miriad '//version)
      call hisinput (lin, 'UVACFLAG')
      call hisclose (lin)
c
c Close up the files.
c
 400  continue
      call uvclose (lin)
      call scrclose (lins)
c
      end
c
c************************************************************************
	subroutine countit(flags,n,ntot,ngood)
c
	implicit none
	integer n,ntot,ngood
	logical flags(n)
c------------------------------------------------------------------------
	integer i
c
	ntot = ntot + n
	do i=1,n
	  if(flags(i))ngood = ngood + 1
	enddo
c
	end

      integer function match_time(time,new,list,ntime,maxtime)
c-----------------------------------------------------------------------
c     Match the input time with times already seen
c
c     Input
c        time   The time to check
c        new    Add to list if not in the list already
c       list    List of times seen so far
c       ntime   Number of times in list
c       maxtime Max number of times in list
c     Returned
c        match_time index into list, or -1 if not found and new=false
c-----------------------------------------------------------------------
      implicit none
c
      double precision time
      logical new
      integer ntime,maxtime
      double precision list(maxtime)
c
      integer i
      
      match_time=-1
      i=1
      do while(match_time.lt.0.and.i.le.ntime)
         if (abs(time-list(i)).lt.1.0/86400.) then
            match_time=i
         endif
         i=i+1
      enddo
      if (match_time.lt.0.and.new) then
        ntime=ntime+1
        if (ntime.gt.maxtime) call bug('f','Too many timeslots')
        list(ntime)=time
        match_time=ntime
      endif
      return
      end
c
      integer function match_freq(freq,df,nchan,new,freqlist,nchanlist,
     *                            nfreq,maxfreq)
c-----------------------------------------------------------------------
c     Match the input freq setup with setups already seen
c
c     Input
c        freq   The freq to check
c        df     The channel spacing to check
c        nchan  The number of channels to check
c        new    Add to list if not in the list already
c       freqlist    List of freq and df seen so far
c       nchanlist   List of number of channels seen so far
c       nfreq   Number of freq setups in list
c       maxfreq Max number of freq setups in list
c     Returned
c        match_freq index into list, or -1 if not found and new=false
c-----------------------------------------------------------------------
      implicit none
c
      integer nchan,nfreq,maxfreq,nchanlist(maxfreq)
      double precision freq,df,freqlist(maxfreq,2)
      logical new

c
      integer i
      
      match_freq=-1
      i=1
      do while(match_freq.lt.0.and.i.le.nfreq)
         if (abs(freq-freqlist(i,1)).lt.1.0e-3.and.
     *       abs(df-freqlist(i,2)).lt.1.0e-5.and.
     *       nchan.eq.nchanlist(i)) then
            match_freq=i
         endif
         i=i+1
      enddo
      if (match_freq.lt.0.and.new) then
        nfreq=nfreq+1
        if (nfreq.gt.maxfreq) call bug('f','Too many frequency setups')
        freqlist(nfreq,1)=freq
        freqlist(nfreq,2)=df
        nchanlist(nfreq)=nchan
        match_freq=nfreq
      endif
      return
      end
c
c
c
      subroutine getopt (brief)
c-----------------------------------------------------------------------
c     Get user options
c
c   Output:
c      brief  SHort summary instead of all the juice
c-----------------------------------------------------------------------
      implicit none
c
      logical brief
cc
      integer nopt
      parameter (nopt = 1)
c
      character opts(nopt)*7
      logical present(nopt)
      data opts /'full'/
c-----------------------------------------------------------------------
      call options ('options', opts, present, nopt)
      brief = .not.present(1)

c
      end
