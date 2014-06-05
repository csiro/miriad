c***********************************************************************
c  A set of routines, called by uvdat.for, which apply calibration
c  corrections to the data.
c
c  History:
c    04aug92 rjs  Derived from some code in uvdat, plus lots of new
c                 code.
c    28aug92 rjs  Cosmetic error message change.
c    22oct92 rjs  Another cosmetic error message change.
c     2apr93 rjs  Corrected a bug checking whether the gains were good.
c    25jun93 rjs  Changed gain interpolation method to make any errors
c                 introduced to be closing ones.
c     5aug93 rjs  Changed definition of "attenuation" parameter.
c    19jul94 rjs  Set bad gains to 0 in uvgnpsma (previously the gain
c                 flag was marked as bad, but the gain was not
c                 initialised.
c    31oct95 rjs  Fix horror of a bug when averaging channel gains
c                 together.
c    13nov95 rjs  Fix possible non-closing gains in uvgnFac.
c    16nov95 rjs  Linearly interpolate when the bandpass gains are
c                 sampled more coarsely than the data.
c    29mar96 rjs  Some tidying of the cgains routines!!
c     7may96 rjs  Improved goodness measure in uvgnpsma.
c    23sep96 rjs  Mark memory as deallocated after deallocating!
c    26jun97 rjs  Correct channel numbering when there are multiple
c                 windows and bandpass averaging taking place.
c    10dec97 rjs  Check gain table size is correct.
c    24feb97 rjs  Make "bandpass calibration" work for wide-only files.
c    01jan05 rjs  Double precision baselines and use basant.
c    08jan07 rjs  Use MAXWIN more rigorously.
c    13feb07 pjt  Fix sign of swidth for LSB wide bands
c    08aug07 rjs  Correct bug in averaging wide channels.
c    27aug09 mhw  Handle multiple bandpass solution intervals
c    31oct11 mhw  Use ptrdiff for memory allocation
c    24feb11 mhw  Handle freq bins in gains and leakage
c    22jun12 pjt  Fixed some ptrdiff
c
c $Id: uvgn.for,v 1.19 2014/06/05 03:49:47 wie017 Exp $
c***********************************************************************

      subroutine uvGnIni(tno1,dogains1,dopass1)

      integer tno1
      logical dogains1,dopass1
c-----------------------------------------------------------------------
c  Initialise the "gains" routines and ready it to start reading data.
c-----------------------------------------------------------------------
      include 'uvgn.h'

c     Externals.
      logical hdprsnt
c-----------------------------------------------------------------------
      tno = tno1
c
c  Work out what to do.
c
      dogains = dogains1
      dopass = dopass1 .and. hdprsnt(tno,'bandpass')
      if (dopass1 .and. .not.dopass) then
        docgains = hdprsnt(tno,'cgains')
        dowgains = hdprsnt(tno,'wgains')
      endif
c
c  Initialise everything we are interested in.
c
      if (dogains .or. dopass) then
        call rdhdi(tno,'ngains',ngains,0)
        call rdhdi(tno,'nfeeds',nfeeds,1)
        call rdhdi(tno,'ntau',  ntau,  0)
        nants = ngains / (ntau + nfeeds)
        dotau = dogains .and. ntau.gt.0
        if (ngains.le.0) call bug('f',
     *    'Number of gains is missing or bad, in uvGnIni')
        if (nfeeds.le.0 .or. nfeeds.gt.MAXFEEDS) call bug('f',
     *    'Number of feeds is bad')
        if (ntau.lt.0 .or. ntau.gt.1) call bug('f',
     *    'Number of delay terms is bad')
        if (nants*(ntau + nfeeds).ne.ngains) call bug('f',
     *    'Bad number of gains or feeds')
        if (nants.gt.MAXANT) call bug('f',
     *    'Too many antennae for me to handle, in uvGnIni')
      else
        nants = MAXANT
        ngains = MAXANT
        nfeeds = 1
        ntau = 0
        dotau = .false.
      endif
c
c  Initialise the gains stuff.
c
      if (dogains) call uvGnGnIn
      if (docgains) call uvGnInic
      if (dowgains) call uvGnIniw
      if (dopass .or. dotau) call uvGnPsIn

      end

c***********************************************************************

      subroutine uvGnGnIn
c-----------------------------------------------------------------------
      include 'uvgn.h'
      integer iostat
      integer i,j,k,off

c     Externals.
      integer hsize
c-----------------------------------------------------------------------
      call rdhdi(tno,'nsols', nsols,0)
      call rdhdd(tno,'interval',dtime,0d0)
      if (nsols.le.0) call bug('f',
     *    'Number of gain solutions is missing or bad, in uvGnIni')
      if (dtime.le.0) call bug('f',
     *    'The time interval increment is missing or bad, in uvGnIni')

      call haccess(tno,gitem,'gains','read',iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'accessing gains table')
c
c  Check that its the right size.
c
      if (hsize(gitem).ne.8+(ngains+1)*8*nsols)
     *        call bug('f','Gain table size is incorrect')

      t1 = 1
      t2 = min(2,nsols)
c
c  Read in the gain solutions
c

      off = 8
      do i=1,nsols
        call hreadd(gitem,timetab(i),off,8,iostat)
        call hreadr(gitem,Gains(1,i,0),off+8,8*ngains,iostat)
        if (iostat.ne.0) call uvGnBug(iostat,'reading gains')
        do j = 1, ngains
          gFlag(j,i,0) = abs(real(Gains(j,i,0)))+
     *                    abs(aimag(Gains(j,i,0))).ne.0
        enddo
        off = off + 8 + ngains * 8
      enddo
      call hdaccess(gitem,iostat)
      
c
c  Read in the binned gain solutions
c
      nfbin = 0
      call haccess(tno,gitem,'gainsf','read',iostat)
      if (iostat.ne.0) return
      call hreadi(gitem,nfbin,4,4,iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'reading gainsf header')
      off = 8
      do k=1,nfbin
        do i=1,nsols
          off = off + 8
          call hreadr(gitem,Gains(1,i,k),off,8*ngains,iostat)
          do j = 1, ngains
            gFlag(j,i,k) = abs(real(Gains(j,i,k)))+
     *                      abs(aimag(Gains(j,i,k))).ne.0
          enddo
          off = off + ngains * 8
        enddo
        call hreadd(gitem,freq(k),off,8,iostat)
        if (iostat.ne.0) call UvGnBug(iostat,'reading gainsf')
        off = off + 8
      enddo       
      call hdaccess(gitem,iostat)

      end

c***********************************************************************

      subroutine uvGnRead(tno,G,time,freq,ngains,nfeeds,ntau,nsols,
     *  nfbin,maxgains,maxtimes,maxfbin) 
      integer tno,ngains,nfeeds,ntau,nsols,nfbin,maxgains,maxtimes
      integer maxfbin
      complex G(maxgains)
      double precision time(maxtimes),freq(maxfbin)
c-----------------------------------------------------------------------
c      Read a gain table into memory.
c      Read both the standard gains and the gainsf table if present.
c      Intended as a standalone routine to read gain tables.
c   Input:
c    tno      handle of visibility file
c    maxgains maximum number of gains to be read
c    maxtimes maximum number of solution intervals
c    maxfbin  maximum number of freq bins
c  Output:
c    G        the gains, both continuum and freq binned gains 
c             are returned
c    time     the time for each solution
c    freq     the frequency for each bin
c    ngains   the number of gains per solution
c    nfeeds   the number of feeds
c    ntau     ntau=1 means we have delay solutions (nfbin should be 0)
c    nsols    the number of solutions
c    nfbin    the number of freq bins
c-----------------------------------------------------------------------
      integer iostat
      integer i,j,k,off,item

c     Externals.
      integer hsize
c-----------------------------------------------------------------------
      call rdhdi(tno,'ngains', ngains,0)
      call rdhdi(tno,'nfeeds', nfeeds,0)
      call rdhdi(tno,'ntau', ntau,0)
      call rdhdi(tno,'nsols', nsols,0)
      if (nsols.le.0) call bug('f',
     *    'Number of gain solutions is missing or bad, in uvGnRead')
      if (nsols.gt.maxtimes) call bug('f',
     *    'Too many solutions for supplied buffer, in uvGnRead')
      if (ngains*nsols.gt.maxgains) call bug('f',
      *   'Too many gains for supplied buffer, in uvGnRead')

      call haccess(tno,item,'gains','read',iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'accessing gains table')
c
c  Check that it's the right size.
c
      if (hsize(item).ne.8+(ngains+1)*8*nsols)
     *        call bug('f','Gain table size is incorrect')

c
c  Read in the gain solutions
c

      off = 8
      k=1
      do i=1,nsols
        call hreadd(item,time(i),off,8,iostat)
        call hreadr(item,G(k),off+8,8*ngains,iostat)
        if (iostat.ne.0) call uvGnBug(iostat,'reading gains')
        off = off + 8 + ngains * 8
        k=k+ngains
      enddo
      call hdaccess(item,iostat)
      
c
c  Read in the binned gain solutions
c
      nfbin = 0
      call haccess(tno,item,'gainsf','read',iostat)
      if (iostat.ne.0) return
      call hreadi(item,nfbin,4,4,iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'reading gainsf header')
      if (nfbin.gt.maxfbin) call bug('f',
     *   'Too many freq bins for supplied buffer, in uvGnRead')
      if (ngains*nsols*(nfbin+1).gt.maxgains) call bug('f',
     *   'Too many gains for supplied buffer, in uvGnRead.')
      off = 8
      do j=1,nfbin
        do i=1,nsols
          off = off + 8
          call hreadr(item,G(k),off,8*ngains,iostat)
          off = off + ngains * 8
          k=k+ngains
        enddo
        call hreadd(item,freq(j),off,8,iostat)
        if (iostat.ne.0) call UvGnBug(iostat,'reading gainsf')
        off = off + 8
      enddo       
      call hdaccess(item,iostat)

      end

c***********************************************************************

      subroutine uvGnWrit(tno,G,time,freq,ngains,nsols,
     *  nfbin,maxgains,maxtimes,maxfbin,replace) 
      integer tno,ngains,nsols,nfbin,maxgains,maxtimes
      integer maxfbin
      complex G(maxgains)
      logical replace
      double precision time(maxtimes),freq(maxfbin)
c-----------------------------------------------------------------------
c      Read a gain table into memory.
c      Read both the standard gains and the gainsf table if present.
c      Intended as a standalone routine to read gain tables.
c
c-----------------------------------------------------------------------
      integer iostat
      integer i,j,k,off,item
      character*6 mode
c-----------------------------------------------------------------------
      mode='append'
      if (replace) mode='write'
      call haccess(tno,item,'gains',mode,iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'accessing gains table')
c
c  Write the gain solutions
c
      off = 8
      k=1
      do i=1,nsols
        call hwrited(item,time(i),off,8,iostat)
        call hwriter(item,G(k),off+8,8*ngains,iostat)
        if (iostat.ne.0) call uvGnBug(iostat,'writing gains')
        off = off + 8 + ngains * 8
        k=k+ngains
      enddo
      call hdaccess(item,iostat)
      
c
c  Write the binned gain solutions
c
      if (nfbin.gt.1) then
        call haccess(tno,item,'gainsf',mode,iostat)
        if (iostat.ne.0) call UvGnBug(iostat,'writing gainsf header')
        call hwritei(item,nfbin,4,4,iostat)
        if (iostat.ne.0) call UvGnBug(iostat,'writing gainsf header')
        off = 8
        do j=1,nfbin
          do i=1,nsols
            call hwrited(item,time(i),off,8,iostat)
            off = off + 8
            call hwriter(item,G(k),off,8*ngains,iostat)
            off = off + ngains * 8
            k=k+ngains
          enddo
          call hwrited(item,freq(j),off,8,iostat)
          if (iostat.ne.0) call UvGnBug(iostat,'writing gainsf')
          off = off + 8
        enddo       
        call hdaccess(item,iostat)
      endif
      end

c***********************************************************************

      subroutine uvGnPsIn
c-----------------------------------------------------------------------
c  Initialise ready to perform bandpass and delay correction on the
c  correlation data.
c-----------------------------------------------------------------------
      include 'uvgn.h'
c-----------------------------------------------------------------------
c
c  Set the dynamic memory pointers to NULL.
c
      nTab    = 0
      nDat(1) = 0
      nDat(2) = 0
      nFreq(1)= 0
      nFreq(2)= 0

      aver = .false.
      call rdhdi(tno,'nbpsols', nbpsols,0)
      if (nbpsols.gt.MAXSOLN) call bug('f',
     *   'Too many bandpass solutions for me too handle, in uvGn')
c
c  If we are to do bandpass correction, read the bandpass table.
c
      if (dopass) call uvGnPsLd(tno,MAXSPECT,nbpsols,nfeeds*nants,nchan,
     *  nspect,sfreq,sdf,nschan,pTab,nTab,bptimes,maxsoln)
c
c  Get the reference frequency, in case we are doing the delay
c  correction.
c
      call rdhdd(tno,'freq0',freq0,0d0)

      first = .true.

      end

c***********************************************************************

      subroutine uvGnFin()
c-----------------------------------------------------------------------
c  Close up the bandpass and delay routines. This does not do much!
c-----------------------------------------------------------------------
      include 'uvgn.h'
c-----------------------------------------------------------------------
c
c
c  Free up all the memory that may have been allocated for the antenna
c  based bandpass calibration.
c
      if (nTab.ne.0) then
        call MemFrep(pTab, nTab, 'c')
        nTab = 0
      endif
      if (nDat(1).ne.0) then
        call MemFrep(pDat(1),nDat(1),'c')
        call MemFrep(pFlags(1),nDat(1),'l')
        nDat(1) = 0
      endif
      if (nDat(2).ne.0) then
        call MemFrep(pDat(2),nDat(2),'c')
        call MemFrep(pFlags(2),nDat(2),'l')
        nDat(2) = 0
      endif
      if (nFreq(1).ne.0) then
        call MemFrep(pFreq(1),nFreq(1),'d')
        nFreq(1) = 0
      endif
      if (nFreq(2).ne.0) then
        call MemFrep(pFreq(2),nFreq(2),'d')
        nFreq(2) = 0
      endif
c
c  Free up all the memory that may have been allocated for the baseline
c  based bandpass calibration.
c
      if (docgains) call MemFrep(pCgains,ncgains*ncbase,'c')
      if (dowgains) call MemFrep(pWgains,nwgains*nwbase,'c')
c
c  Clear out everything.
c
      dopass = .false.
      dogains = .false.
      docgains = .false.
      dowgains = .false.
      dotau = .false.

      end

c***********************************************************************

      subroutine uvGnFac(time,baseline,pol,dowide,data,flags,nread,
     *  grms,chnfreq,updated)

      integer nread
      complex data(nread)
      logical flags(nread),dowide,updated
      double precision time,baseline,chnfreq(nread)
      real grms
      integer pol
c-----------------------------------------------------------------------
c  Determine the gain factor for a particular visibility.
c
c  Input:
c    pol        Polarization. If 0, and the info is needed, this is
c               determined from the data file
c    time       Julian date of the data.
c    baseline   Baseline number.
c    dowide     True if the data was retrieved using uvwread (rather
c               than uvread).
c  Input/Output:
c    data       The correlation data. On input this is uncalibrated. On
c               output, it is gain/bandpass calibrated.
c    flags      Data flags.  If the antenna gains were bad for some
c               reason, the data are flagged as bad.
c  Output:
c    grms       The rms gain.
c-----------------------------------------------------------------------
      include 'uvgn.h'
      logical t1good,t2good,flag(0:MAXFBIN)
      integer i,i1,i2,ant1,ant2,s,n,p,gpant,k
      real mag,epsi
      complex tau1,tau2,taua1,taub1,taua2,taub2,tau
      complex ga1,gb1,ga2,gb2,g,gain(0:MAXFBIN)
      integer f1(4),f2(4)
      save f1,f2
      data f1/0,1,0,1/
      data f2/0,1,1,0/
c-----------------------------------------------------------------------
c
c  Assume that we fail!
c
      grms = 1
      do k=0,nfbin
        flag(k) = .false.
      enddo
c
c  Determine the polarisation type index.
c
      p = 1
      if (nfeeds.gt.1) then
        p = pol
        if (p.eq.0) call uvrdvri(tno,'pol',p,0)
        if (p.ge.0) goto 100
        if (p.le.-5) then
          p = -4 - p
        else
          p = - p
        endif
        if (p.gt.4) goto 100
      endif
c
c  Determine the gain indices based on antenna numbers.
c
      call basant(baseline,ant1,ant2)
      if (ant1.lt.1 .or. ant1.gt.nants .or.
     *    ant2.lt.1 .or. ant2.gt.nants) goto 100
c
c  If we are applying the gains, get the solution.
c  Check if (t1,t2) bounds the solution. If not, find t1,t2 which do.
c  This uses a binary step through the gains array.
c
      tau = 0
      if (dogains) then
        if ((time-timetab(t1))*(time-timetab(t2)).gt.0) then
          n = 1
          do while (timetab(t2).lt.time .and. t2.lt.nsols)
            t1 = t2
            t2 = min(t2+n,nsols)
            n = n + n
          enddo
          do while (timetab(t1).gt.time .and. t1.gt.1)
            t2 = t1
            t1 = max(t1-n,1)
            n = n + n
          enddo
c
c  Check for the case that we have fallen off the end of the gains
c  table.
c
          if (time.gt.timetab(t2)) then
            t1 = t2
            t2 = nsols + 1
            timetab(t2) = time + 1e6*dtime
          else if (time.lt.timetab(t1).or.
     *             (t1.eq.t2.and.time.eq.timetab(t1))) then
            t2 = t1
            t1 = 0
            timetab(t1) = time - 1e6*dtime
          endif
c
c  We have solution intervals which bound "time", but they may not
c  be adjacent solutions. Home in on an adjacent solution pair. We use
c  a binary bisection technique.
c
          do while (t1+1.ne.t2)
            s = (t1+t2)/2
            if (timetab(s).gt.time) then
              t2 = s
            else
              t1 = s
            endif
          enddo
        endif
c
c  Determine the indices of the gains.
c
        gpant = nfeeds + ntau
        i1 = gpant*(ant1-1) + 1
        i2 = gpant*(ant2-1) + 1
        
        do k=0,nfbin
c
c  Determine the gains for each antenna.
c
          flag(k) = .true.
          t1good = abs(time-timetab(t1)).lt.dtime
          t2good = abs(time-timetab(t2)).lt.dtime

          if (    t1good .and. gflag(i1+f1(p),t1,k)) then
            ga1 = gains(i1+f1(p),t1,k)
          else if (t2good .and. gflag(i1+f1(p),t2,k)) then
            ga1 = gains(i1+f1(p),t2,k)
          else
            flag(k) = .false.
          endif

          if (    t2good .and. gflag(i1+f1(p),t2,k)) then
            ga2 = gains(i1+f1(p),t2,k)
          else if (t1good .and. gflag(i1+f1(p),t1,k)) then
            ga2 = gains(i1+f1(p),t1,k)
          else
            flag(k) = .false.
          endif

          if (    t1good .and. gflag(i2+f2(p),t1,k)) then
            gb1 = gains(i2+f2(p),t1,k)
          else if (t2good .and. gflag(i2+f2(p),t2,k)) then
            gb1 = gains(i2+f2(p),t2,k)
          else
            flag(k) = .false.
          endif
          if (    t2good .and. gflag(i2+f2(p),t2,k)) then
            gb2 = gains(i2+f2(p),t2,k)
          else if (t1good .and. gflag(i2+f2(p),t1,k)) then
            gb2 = gains(i2+f2(p),t1,k)
          else
            flag(k) = .false.
          endif

          if (ntau.eq.1 .and. flag(0) .and. k.eq.0) then
            if (    t1good .and. gflag(i1+f1(p),t1,0)) then
              taua1 = gains(i1+nfeeds,t1,0)
            else if (t2good .and. gflag(i1+f1(p),t2,0)) then
              taua1 = gains(i1+nfeeds,t2,0)
            endif

            if (    t2good .and. gflag(i1+f1(p),t2,0)) then
              taua2 = gains(i1+nfeeds,t2,0)
            else if (t1good .and. gflag(i1+f1(p),t1,0)) then
              taua2 = gains(i1+nfeeds,t1,0)
            endif

            if (    t1good .and. gflag(i2+f2(p),t1,0)) then
              taub1 = gains(i2+nfeeds,t1,0)
            else if (t2good .and. gflag(i2+f2(p),t2,0)) then
              taub1 = gains(i2+nfeeds,t2,0)
            endif
            if (    t2good .and. gflag(i2+f2(p),t2,0)) then
              taub2 = gains(i2+nfeeds,t2,0)
            else if (t1good .and. gflag(i2+f2(p),t1,0)) then
              taub2 = gains(i2+nfeeds,t1,0)
            endif
          endif
c
c  If all is good, interpolate the gains to the current time interval.
c
          if (flag(k)) then
            epsi = 0
            if (t1.ne.t2) 
    *         epsi = (timetab(t2)-time)/(timetab(t2)-timetab(t1))

            g = ga1/ga2
            mag = abs(g)
            gain(k) = ga2 * (1 + (mag-1)*epsi) * (g/mag) ** epsi

            g = gb1/gb2
            mag = abs(g)
            gain(k) = gain(k) *
     *           conjg(gb2 * (1 + (mag-1)*epsi) * (g/mag) ** epsi)

            if (ntau.eq.1.and.k.eq.0) then
              tau1 = taua1 + conjg(taub1)
              tau2 = taua2 + conjg(taub2)
              tau = tau2 - epsi * (tau2 - tau1)
            endif
          endif
        enddo
      else
        do k=0,nfbin
          flag(k) = .true.
        enddo
      endif
      
      if (updated) then
        call uvGnInt(chnfreq,nread)
      endif
c
c  Apply the gain to the data.
c
  100 continue
      if (nfbin.eq.0.and..not.flag(0)) then 
        do i = 1, nread
          flags(i) = .false.
        enddo
      else
        if (dogains) then
          if (nfbin.eq.0) then
            do i = 1, nread
              data(i) = gain(0) * data(i)
            enddo
            grms = abs(gain(0))
          else
            do i = 1, nread
              if (flag(b(1,i)).and.flag(b(2,i))) then
                g = gain(b(1,i))/gain(b(2,i))
                mag = abs(g)
                data(i) = data(i)*gain(b(2,i))*(1+(mag-1)*fac(i))*
     *            (g/mag)**fac(i)
              else
                flags(i) = .false.
              endif
            enddo 
          endif
        endif
        if (dopass .or. dotau)
     *    call uvGnPsAp(dowide,time,ant1,ant2,p,tau,data,flags,nread)
        if (docgains .or. dowgains)
     *    call uvGnCWAp(dowide,ant1,ant2,data,flags,nread)
      endif

      end

c************************************************************************
	subroutine uvGnInt(chnfreq,n)
c
	implicit none
        integer n
        double precision chnfreq(n)
c
c  Interpolate from bins to channels.
c------------------------------------------------------------------------
	include 'uvgn.h'
	integer chan,i
        double precision d1,d2,d
c************************************************************************
        do chan=1,n
          b(1,chan) = 1
          d1 = abs(chnfreq(chan)-freq(1))
          do i=2,nfbin
            d = abs(chnfreq(chan)-freq(i))
            if (d.lt.d1) then
              d1 = d
              b(1,chan) = i
            endif
          enddo
          b(2,chan) = 0
          d2 = abs(chnfreq(chan))
          do i=1,nfbin
            d = abs(chnfreq(chan)-freq(i))
            if (i.ne.b(1,chan).and.d.lt.d2) then
              d2 = d
              b(2,chan) = i
            endif
          enddo
          if (b(2,chan).eq.0) b(2,chan)=b(1,chan)
          if (b(1,chan).eq.b(2,chan)) then
            fac(chan) = 0
          else
            fac(chan) = (freq(b(2,chan))-chnfreq(chan))/
     *                  (freq(b(2,chan))-freq(b(1,chan)))
          endif
        enddo
        end
c***********************************************************************

      subroutine uvGnCWAp(dowide,ant1,ant2,data,flags,nread)

      integer ant1,ant2,nread
      logical dowide,flags(nread)
      complex data(nread)
c-----------------------------------------------------------------------
c  Apply either CGains or WGains as is appropriate.
c
c  Input:
c    dowide
c    ant1,ant2
c    nread
c  Input/Output:
c    data
c    flags
c-----------------------------------------------------------------------
      include 'uvgn.h'
      include 'mem.h'
      integer LINE,WIDE,VELO
      parameter (LINE=1,WIDE=2,VELO=3)
      integer TYPE,COUNT,START,WIDTH,STEP
      parameter (TYPE=1,COUNT=2,START=3,WIDTH=4,STEP=5)
      integer linetype,i,bl,i0
      ptrdiff j
      logical willcg,ok
      double precision dat(6)
c-----------------------------------------------------------------------
c
c  Determine whether its cgains or wgains that we are to apply.
c
      if (dowide) then
        willcg = .false.
        i0 = 1
      else
        call uvinfo(tno,'line',dat)
        linetype  = nint(dat(TYPE))
        willcg = linetype.eq.LINE
        if (linetype.eq.VELO) call bug('f',
     *    'Cannot apply bandpass correction with velocity linetype')
        if (nint(dat(STEP)).ne.1 .and. nint(dat(WIDTH)).ne.1)
     *    call bug('f','Linetype width and step must be 1')
        i0 = nint(dat(START))
      endif
c
c  Determine the baseline number.
c
      bl = ((ant2-1)*ant2)/2 + ant1
c
c  Get the appropriate table.
c
      if (willcg .and. docgains) then
        j = pCgains + ncgains*(bl-1) + (i0-1)
        ok = bl.le.ncbase .and. ncgains.ge.nread+i0-1
      else if (.not.willcg .and. dowgains) then
        j = pWgains + nwgains*(bl-1) + (i0-1)
        ok = bl.le.nwbase .and. nwgains.ge.nread+i0-1
      else
        ok = .false.
      endif
c
c Check it all makes sense.
c
      if (.not.ok) then
        do i = 1, nread
          flags(i) = .false.
        enddo
      else
        do i = 1, nread
          data(i) = data(i) * memC(j)
          j = j + 1
        enddo
      endif

      end

c***********************************************************************

      subroutine uvGnBug(iostat,text)

      integer iostat
      character text*(*)
c-----------------------------------------------------------------------
c  Produce two error messages, then abort.  The first message is formed
c  from the text passed in, the second from the i/o status indicator.
c
c  Input:
c    iostat     I/O status indicator, returned by an unsuccessful i/o
c               operation.
c    text       Some text saying what was being done when the i/o error
c               occurred.
c-----------------------------------------------------------------------
      character umsg*64
c-----------------------------------------------------------------------
      umsg = 'I/O error when '//text//', in Gains routines.'
      call bug('w',umsg)
      call bugno('f',iostat)

      end

c***********************************************************************

      subroutine uvGnGet(gitem,solno,Gains,Flags,nsols,ngains)

      integer solno,nsols,ngains,gitem
      complex Gains(ngains)
      logical Flags(ngains)
c-----------------------------------------------------------------------
c  This reads in the gains for a particular time instant. If the gains
c  are beyond the edge of the table, then zero gains are returned.
c
c  Input:
c    gitem
c    solno      Row number of read.
c    nsols
c    ngains     Total number of gains in each record.
c  Output:
c    Gains
c    Flags      Set to true or false, depending whether the gains are
c               good.
c-----------------------------------------------------------------------
      integer offset,iostat,i
c-----------------------------------------------------------------------
      if (solno.lt.1 .or. solno.gt.nsols) then
        do i = 1, ngains
          Flags(i) = .false.
        enddo
      else
        offset = 8*(ngains+1)*(solno-1) + 16
        call hreadr(gitem,Gains,offset,8*ngains,iostat)
        if (iostat.ne.0) call uvGnBug(iostat,'reading gains entry')
        do i = 1, ngains
          Flags(i) = abs(real(Gains(i)))+abs(aimag(Gains(i))).ne.0
        enddo
      endif

      end

c***********************************************************************

      subroutine uvGnPsAp(dowide,time,ant1,ant2,p,tau,data,flags,
     *                    nread)

      logical dowide
      double precision time
      integer ant1,ant2,p,nread
      complex tau,data(nread)
      logical flags(nread)
c-----------------------------------------------------------------------
c  Apply bandpass and delay corrections.
c
c  Input:
c    dowide     Do "wide" channels rather than the standard line.
c    time       Julian date of the data
c    ant1,ant2  Two antenna numbers.
c    p          Polarisation code, in range 1 to 4.
c    tau        Delay/attenuation term. data = data * exp(tau*(f-f0))
c    nread      Number of data channels.
c  Input/Output:
c    data       The correlation data. Corrected on output.
c    flags      The data flags.
c-----------------------------------------------------------------------
      include 'uvgn.h'
      include 'mem.h'

      logical upd
      integer table,i
      real    factor

      logical  uvvarupd
      external uvvarupd
c-----------------------------------------------------------------------
c     Determine which table to use and whether the data has been updated
c     recently.  Need to update every time if interpolating between
c     bandpass solutions.
      upd = nbpsols.gt.1
      if (first) then
        call uvGnPs1t(tno,vwide,vline)
        bpsolno = 0
        upd = .true.
      endif

      first = .false.
      if (dowide) then
        table = 2
        upd = upd .or. uvvarupd(vwide)
      else
        table = 1
        upd = upd .or. uvvarupd(vline)
      endif

c     Find the correct time slot, simple linear search since we expect
c     no more than a dozen bp solutions.
      if (nbpsols.gt.1) then
        if (time.lt.bptimes(bpsolno+1) .or.
     *      time.ge.bptimes(bpsolno+2)) then
          upd = .true.
          i = 0
          bpsolno = -1
          do while (i.le.nbpsols .and. bpsolno.lt.0)
            if (time.ge.bptimes(i+1) .and. time.lt.bptimes(i+2)) then
              bpsolno = i
            endif
            i = i + 1
          enddo

          if (bpsolno.eq.-1) call bug('f','uvGn: corrupted bp table?')
        endif
      endif

c     Compute the interpolation factor for the time direction.
      if (bpsolno.eq.0) then
        bpsolno = 1
        factor  = 0.0
      else if (bpsolno.eq.nbpsols) then
        factor  = 0.0
      else
        factor  = (time-bptimes(bpsolno+1)) /
     *              (bptimes(bpsolno+2)-bptimes(bpsolno+1))
      endif


      if (upd) call uvGnPsRd(tno,dowide,nread,nchan,nfeeds,nants,
     *  aver,nspect,sfreq,sdf,nschan,pTab,nbpsols,bpsolno,factor,
     *  pFlags(table),pDat(table),nDat(table),pFreq(table),
     *  nFreq(table),dotau,dopass)

c     Having the up-to-date gains and frequencies, apply them.
      if (dopass) call uvGnPsPB(ant1,ant2,p,nfeeds,nants,
     *  memC(pDat(table)),memL(pFlags(table)),data,flags,nread)

      if (dotau) call uvGnPsDl(tau,data,memD(pFreq(table)),freq0,nread)

      end

c***********************************************************************

      subroutine uvGnPs1t(tno,vwide,vline)

      integer tno,vwide,vline
c-----------------------------------------------------------------------
      integer WIDE,TYPE
      parameter (WIDE=2,TYPE=1)
      double precision data(6)
c-----------------------------------------------------------------------
c
c  Initialise the handle to check for a change in the wide channels.
c
      call uvvarini(tno,vwide)
      call uvvarset(vwide,'wfreq')
      call uvvarset(vwide,'wwidth')

      call uvvarini(tno,vline)
      call uvinfo(tno,'line',data)
      if (nint(data(TYPE)).eq.WIDE) then
        call uvvarset(vline,'wfreq')
        call uvvarset(vline,'wwidth')
      else
        call uvvarset(vline,'sfreq')
        call uvvarset(vline,'sdf')
        call uvvarset(vline,'nschan')
      endif

      end

c***********************************************************************

      subroutine uvGnPsPB(ant1,ant2,p,nfeeds,nants,
     *                        Gains,GFlags,data,flags,nread)

      integer ant1,ant2,p,nfeeds,nants,nread
      complex Gains(nread,nfeeds*nants),data(nread)
      logical Gflags(nread,nfeeds*nants),flags(nread)
c-----------------------------------------------------------------------
c  Apply the bandpass corrections to the data.
c
c  Input:
c    ant1,ant2  The antenna numbers of the data.
c    p          Polarisation number in range 1 - 4.
c    nfeeds     Number of feed gains.
c    nants      Number of antennae.
c    nread      Number of channels.
c    Gains      The bandpass gains.
c    Gflags     Bandpass flags.
c  Input/Output:
c    data       Correlation data.
c    flags      Flags for the correlation data.
c-----------------------------------------------------------------------
      integer i,i1,i2
      integer f1(4),f2(4)
      save f1,f2
      data f1/1,2,1,2/
      data f2/1,2,2,1/
c-----------------------------------------------------------------------
      i1 = nfeeds*(ant1-1) + f1(p)
      i2 = nfeeds*(ant2-1) + f2(p)

      do i = 1, nread
        data(i)  = data(i)*Gains(i,i1)*conjg(Gains(i,i2))
        flags(i) = flags(i) .and. Gflags(i,i1) .and. Gflags(i,i2)
      enddo

      end

c***********************************************************************

      subroutine uvGnPsDl(tau,data,freq,freq0,nread)

      integer nread
      double precision freq(nread),freq0
      complex tau,data(nread)
c-----------------------------------------------------------------------
c  Apply delay correction to the data.
c
c  Input:
c    nread      Number of channels.
c    tau        The complex "delay" correction.
c    freq       Channel frequency.
c    freq0      Reference frequency.
c  Input/Output:
c    data       The correlation data.
c-----------------------------------------------------------------------
      integer i
      real atten,theta,s,t
c-----------------------------------------------------------------------
      atten = real(tau)
      theta = aimag(tau)

      if (theta.ne.0) then
        if (atten.ne.0) then
          do i = 1, nread
            s = (real(freq(i)/freq0))**atten
            t = theta*(freq(i)-freq0)
            data(i) = data(i) * s * cmplx(cos(t),sin(t))
          enddo
        else
          do i = 1, nread
            t = theta*(freq(i)-freq0)
            data(i) = data(i) * cmplx(cos(t),sin(t))
          enddo
        endif
      else if (atten.ne.0) then
        do i = 1, nread
          s = (real(freq(i)/freq0))**atten
          data(i) = s * data(i)
        enddo
      endif

      end

c***********************************************************************

      subroutine uvGnPsLd(tno,maxspect,nbpsols,ngains,nchan,
     *  nspect,sfreq,sdf,nschan,pGains,Size,bptimes,maxsoln)

      integer tno,maxspect,nbpsols,ngains,nchan,nspect,maxsoln
      integer nschan(maxspect),Size
      ptrdiff pGains
      double precision sfreq(maxspect),sdf(maxspect)
      double precision bptimes(maxsoln)
c-----------------------------------------------------------------------
c  Load in the bandpass gains.
c
c  Input:
c    tno
c    nbpsols   Number of bandpass solution intervals (0: old format)
c    ngains    Number of antennas*number of feeds
c    maxspect
c  Output:
c    nchan      Total number of channels.
c    nspect     spectral info
c    sfreq
c    sdf
c    nschan
c    pGains     All the bandpass gains
c    Size       Size of pGains allocation
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      integer item,iostat,i,off,offg,n
      double precision freqs(2)

c     Externals.
      integer hsize
c-----------------------------------------------------------------------
c     Get the dimensionality numbers.
      n=max(1,nbpsols)
      call rdhdi(tno,'nchan0',nchan,0)
      call rdhdi(tno,'nspect0',nspect,0)
      if (ngains.le.0 .or. nchan.le.0 .or. nspect.le.0) call bug('f',
     *  'Invalid value for nchan, ngains or nspect, in uvDat')
      Size = ngains*nchan*n
      call MemAllop(pGains,Size,'c')
      if (nspect.gt.maxspect) call bug('f',
     *  'Too many spectral windows for me to handle, in uvDat')

c     Load the frequency table.
      call haccess(tno,item,'freqs','read',iostat)
      if (iostat.ne.0) call uvGnBug(iostat,'accessing freqs table')
      off = 8
      do i = 1, nspect
        call hreadi(item,nschan(i),off,4,iostat)
        if (iostat.ne.0) call uvGnBug(iostat,'reading freqs table')
        off = off + 8
        call hreadd(item,freqs,off,16,iostat)
        if (iostat.ne.0) call uvGnBug(iostat,'reading freqs table')
        off = off + 16
        sfreq(i) = freqs(1)
        sdf(i)   = freqs(2)
      enddo

      call hdaccess(item,iostat)
      if (iostat.ne.0) call uvGnBug(iostat,'closing freqs table')

c     Read in the gains themselves now.
      call haccess(tno,item,'bandpass','read',iostat)
      if (iostat.ne.0) call uvGnBug(iostat,'accessing bandpass table')
      if (hsize(item).ne.8+n*ngains*nchan*8+nbpsols*8) then
        call bug('f', 'Bandpass table size is incorrect. ' //
     *    'This happens when the inputs for mfcal and gpcal are ' //
     *    'inconsistent')
      endif

      bptimes(1) = -1e20
      bptimes(nbpsols+2) = 1e20
      off=8
      offg=0
      do i = 1, n
        call hreadr(item,memC(pGains+offg),off,8*ngains*nchan,iostat)
        off=off+8*ngains*nchan
        offg=offg+ngains*nchan
        if (nbpsols.gt.0) then
          call hreadd(item,bptimes(i+1),off,8,iostat)
          off=off+8
        endif
        if (iostat.ne.0) call uvGnBug(iostat,'reading bandpass table')
      enddo
      call hdaccess(item,iostat)
      if (iostat.ne.0) call uvGnBug(iostat,'closing bandpass table')
      nbpsols=n

      end

c***********************************************************************

      subroutine uvGnPsRd(tno,dowide,nread,nchan,nfeeds,nants,aver,
     *  nspect,sfreq,sdf,nschan,pTab,nbpsols,bpsolno,factor,pFlags,
     *  pDat,nDat,pFreq,nFreq,dotau,dopass)

      integer tno,nread,nchan,nfeeds,nants,nspect,nschan(nspect)
      integer nbpsols
      double precision sfreq(nspect),sdf(nspect)
      real factor
      logical dowide,dotau,dopass,aver
      ptrdiff pTab,pFlags,pDat,pFreq
      integer nDat,nFreq,bpsolno
c-----------------------------------------------------------------------
c  Match and compute the passband gains.
c
c  Input:
c    tno
c    nread      Number of correlations read.
c    nchan      Total number of channels.
c    nfeeds
c    nants
c    nspect
c    nschan
c    sfreq
c    sdf
c    dowide
c    pTab
c  Input/Output:
c    pDat,nDat
c    pFlags
c    pFreq,nFreq
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      integer MAXSPECT
      parameter (MAXSPECT=MAXWIN)
      integer nspect0,nschan0(MAXSPECT)
      double precision sfreq0(MAXSPECT),sdf0(MAXSPECT)
      double precision swidth0(MAXSPECT)
      logical doaver
c-----------------------------------------------------------------------
c     Load the description of the data line.
      call uvGnPsGt(tno,dowide,nread,MAXSPECT,doaver,
     *        nspect0,sfreq0,sdf0,swidth0,nschan0)
      if (doaver .and. dopass .and. .not.aver) then
        aver = .true.
        call bug('w',
     *  'Performing linetype averaging before applying bandpass!!')
        call bug('w',
     *  ' ... this may be very unwise')
      endif

c     Generate the gains?
      if (dopass) then
c       Check if we have enough space.
        if (nDat.lt.nfeeds*nants*nread) then
          if (nDat.gt.0) then
            call MemFrep(pDat,nDat,'c')
            call MemFrep(pFlags,nDat,'l')
          endif
          nDat = nfeeds*nants*nread
          call MemAllop(pDat,nDat,'c')
          call MemAllop(pFlags,nDat,'l')
        endif

        call uvGnPsMa(nbpsols,nfeeds*nants,nchan,nspect,sfreq,sdf,
     *    nschan,memC(pTab),bpsolno,factor,nread,nspect0,sfreq0,sdf0,
     *    swidth0,nschan0,memC(pDat),memL(pFlags))
      endif

c     Generate the frequency table?
      if (dotau) then
c       Check if we have enough space.
        if (nFreq.lt.nread) then
          if (nFreq.gt.0) call MemFrep(pFreq,nFreq,'d')
          nFreq = nread
          call MemAllop(pFreq,nFreq,'d')
        endif

        call uvGnPsFq(nread,nspect0,sfreq0,sdf0,nschan0,memD(pFreq))
      endif

      end

c***********************************************************************

      subroutine uvGnPsFq(nchan,nspect,sfreq,sdf,nschan,freq)

      integer nchan,nspect,nschan(nspect)
      double precision sfreq(nspect),sdf(nspect),freq(nchan)
c-----------------------------------------------------------------------
c  Determine the offset frequency for each channel of data.
c
c  Input:
c    nchan
c    nspect
c    sfreq
c    sdf
c    nschan
c  Output:
c    freq
c-----------------------------------------------------------------------
      integer i,j,k
c-----------------------------------------------------------------------
      k = 1
      do j = 1, nspect
        do i = 1, nschan(j)
          freq(k) = sfreq(j) + sdf(j)*(i-1)
          k = k + 1
        enddo
      enddo

      end

c***********************************************************************

      subroutine uvGnPsGt(tno,dowide,nread,mspect,doaver,
     *  nspect,sfreq,sdf,swidth,nschan)

      integer tno,mspect,nspect,nschan(mspect),nread
      logical dowide,doaver
      double precision sfreq(mspect),sdf(mspect),swidth(mspect)
c-----------------------------------------------------------------------
c  Determine the frequency setup of the data.
c
c  Input:
c    tno        Handle of the input visibility file.
c    mspect     Max number of windows that can be set.
c    dowide     Ignore standard linetype, and process WIDE channels.
c    nread      Number of channels returned by uvread or uvwread.
c  Output:
c    doaver     Set to true if averaging is being performed.
c    nspect     Number of windows in the output.
c    sfreq      Centre freq of the first channel of each output window.
c    sdf        Frequency increment between each output channel.
c    swidth     Bandwidth of each output channel.
c    nschan     Number of channels in each window.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer LINE,WIDE,VELO
      parameter (LINE=1,WIDE=2,VELO=3)
      integer TYPE,COUNT,START,WIDTH,STEP
      parameter (TYPE=1,COUNT=2,START=3,WIDTH=4,STEP=5)
      integer lstart,lwidth,lstep,ltype,nchan
      double precision data(6)

      integer MSPECT1
      parameter (MSPECT1=MAXWIN)
      double precision sfreq0(MSPECT1),sdf0(MSPECT1)
      real wfreq(MSPECT1),wwidth(MSPECT1)
      integer nschan0(MSPECT1),nspect0,maxspect
      integer ispect,nwide,i,i0,j
c-----------------------------------------------------------------------
      maxspect = min(mspect,MSPECT1)
c
c  Get the linetype description.
c
      if (dowide) then
        ltype = WIDE
        nchan = nread
        lstart = 1
        lwidth = 1
        lstep = lwidth
      else
        call uvinfo(tno,'line',data)
        ltype  = nint(data(TYPE))
        nchan  = nint(data(COUNT))
        lstart = nint(data(START))
        lwidth = nint(data(WIDTH))
        lstep  = nint(data(STEP))
      endif
      if (nread.ne.nchan) call bug('f',
     *  'Inconsistent number of channels, in uvGn(pass-get)')
      doaver = lwidth.gt.1
c
c  Handle the case of channel data.
c
      if (ltype.eq.LINE) then
        call uvrdvri(tno,'nspect',nspect0,1)
        if (nspect0.le.0 .or. nspect0.gt.maxspect) call bug('f',
     *    'Bad value for variable nspect, in uvGn(pass-get)')

        call uvgetvri(tno,'nschan',nschan0,nspect0)
        call uvgetvrd(tno,'sdf',sdf0,nspect0)
        call uvgetvrd(tno,'sfreq',sfreq0,nspect0)
c
c  Generate the window description parameters for the output.
c
        ispect = 1
        nspect = 0
        do while (nchan.gt.0)
          do while (lstart.gt.nschan0(ispect))
            lstart = lstart - nschan0(ispect)
            ispect = ispect + 1
          enddo
          nspect = nspect + 1
          sfreq(nspect) = sfreq0(ispect) +
     *        (lstart-1)*sdf0(ispect) + 0.5*(lwidth-1)*sdf0(ispect)
          nschan(nspect) = min((nschan0(ispect)-lstart)/lstep+1,nchan)
          swidth(nspect) = lwidth*abs(sdf0(ispect))
          sdf(nspect)    = lstep *sdf0(ispect)
          nchan = nchan - nschan(nspect)
          lstart = lstart + lstep*nschan(nspect)
        enddo
c
c  Handle "wide" data.
c
      else if (ltype.eq.WIDE) then
        call uvrdvri(tno,'nwide',nwide,0)
        if (nchan.gt.maxspect)
     *    call bug('f','nchan.gt.maxspect, in uvGn(pass-get)')
        if (nwide.lt.0 .or. nwide.gt.maxspect)
     *    call bug('f','nwide.gt.maxspect, in uvGn(pass-get)')
        call uvgetvrr(tno,'wfreq',wfreq,nwide)
        call uvgetvrr(tno,'wwidth',wwidth,nwide)
        nspect = nchan
        do j = 1, nchan
          i0 = lstart + (j-1)*lstep
          sfreq(j) = 0
          swidth(j) = 0
          do i = 1, lwidth
            sfreq(j) = sfreq(j) + wfreq(i0)*abs(wwidth(i0))
            swidth(j) = swidth(j) + wwidth(i0)
            i0 = i0 + 1
          enddo
          sfreq(j) = sfreq(j) / swidth(j)
          sdf(j) = swidth(j)
          nschan(j) = 1
          swidth(j) = abs(swidth(j))
        enddo
c
c  Something else, which I cannot handle.
c
      else
        call bug('f','Unsupported linetype, in uvGn(pass-get)')
      endif

      end

c***********************************************************************

      subroutine uvGnPsMa(nbpsols,ngains,nchan,nspect,sfreq,sdf,
     *  nschan,Tab,bpsolno,factor,nread,nspect0,sfreq0,sdf0,swidth0,
     *  nschan0,Dat,flags)

      integer nbpsols,nchan,nread,nspect,nspect0,ngains,bpsolno
      integer nschan(nspect),nschan0(nspect0)
      double precision sfreq(nspect),sdf(nspect)
      real factor
      double precision sfreq0(nspect0),sdf0(nspect0),swidth0(nspect0)
      complex Tab(nchan,ngains,nbpsols),Dat(nread,ngains)
      logical flags(nread,ngains)
c-----------------------------------------------------------------------
c  Match up the data with the measured bandpass functions.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer MAXSPECT
      parameter (MAXSPECT=MAXWIN)
      integer i,j,k,l,ibeg,iend,n,win(MAXSPECT),ischan(MAXSPECT),off
      integer i0,ib
      double precision startt,endt,startd,endd,width
      real chan,inc,hwidth,goodness,good(MAXSPECT),epsi,fac
      logical nearest
      complex r11,r12,r21,r22
c-----------------------------------------------------------------------
      ib=max(bpsolno,1)
      r21=0
      r22=0
      ischan(1) = 1
      do j = 2, nspect
        ischan(j) = ischan(j-1) + nschan(j-1)
      enddo

      do j = 1, nspect0
        if (sdf0(j).gt.0) then
          startd = sfreq0(j) - 0.5*swidth0(j)
          endd   = sfreq0(j) + sdf0(j) * (nschan0(j) - 1)
     *                                         + 0.5*swidth0(j)
        else
          endd   = sfreq0(j) + 0.5*swidth0(j)
          startd = sfreq0(j) + sdf0(j) * (nschan0(j) - 1)
     *                                         - 0.5*swidth0(j)
        endif
        good(j) = 0
        win(j) = 0

        do i = 1, nspect
          if (sdf(i).gt.0) then
            startt = sfreq(i) - 0.5*sdf(i)
            endt   = sfreq(i) + sdf(i) * (nschan(i) - 0.5)
          else
            endt   = sfreq(i) - 0.5*sdf(i)
            startt = sfreq(i) + sdf(i) * (nschan(i) - 0.5)
          endif

c         Is there an overlap in the frequency range of the calibrations
c         tables and the data?
          width = min(endt,endd) - max(startt,startd)
          if (width.gt.0) then
            hwidth = abs(swidth0(j) / sdf(i))
            goodness = 2 * width / (endd - startd)
            if (hwidth.gt.0.8) goodness = goodness + 1
            if (hwidth.lt.1.2) goodness = goodness + 1
            if (goodness.gt.good(j)) then
              good(j) = goodness
              win(j) = i
            endif
          endif
        enddo
      enddo

c     We have now found the best matching bandpass for each spectral
c     window.  Now work out the actual bandpass values.
      do k = 1, ngains
        off = 1
        do j = 1, nspect0
          i0 = win(j)

c         The case of no matching gains.
          if (i0.eq.0) then
            do i = 1, nschan0(j)
              flags(off,k) = .false.
              off = off + 1
            enddo

          else
            chan = (sfreq0(j) - sfreq(i0)) / sdf(i0)
            inc  = sdf0(j) / sdf(i0)
            hwidth = 0.5*abs(swidth0(j) / sdf(i0))
            if (hwidth.lt.0.8) then
c             One corresponding gain in the table for each channel of
c             data.
              do i = 1, nschan0(j)
                l = nint(chan-0.5)
                nearest = l.lt.0 .or. l.ge.nschan(i0)-1
                l = l + ischan(i0)
                r11=tab(l,k,ib)
                r12=tab(l+1,k,ib)
                if (.not.nearest) nearest =
     *              abs(real(r11))+ abs(aimag(r11)).eq.0 .or.
     *              abs(real(r12))+ abs(aimag(r12)).eq.0
                if (nearest) then
                  l = nint(chan)
                  flags(off,k) = l.ge.0 .and. l.lt.nschan(i0)
                  l = l + ischan(i0)
                  r11=tab(l,k,ib)
                  r12=tab(l+1,k,ib)
                  if (ib.lt.nbpsols) then
                    r21=tab(l,k,ib+1)
                    r22=tab(l+1,k,ib+1)
                  endif
                  if (flags(off,k)) flags(off,k) =
     *              real(r11).ne.0 .or. aimag(r11).ne.0
                  if (flags(off,k)) then
                    fac=factor
                    if (abs(r21).eq.0) fac=0
                    dat(off,k) = (1-fac)*r11+fac*r21
                  else
                    dat(off,k) = 0
                  endif
                else
                  if (ib.lt.nbpsols) then
                    r21=tab(l,k,ib+1)
                    r22=tab(l+1,k,ib+1)
                  endif
                  fac=factor
                  if (abs(r21).eq.0 .or. abs(r22).eq.0) fac=0
                  epsi = chan + ischan(i0) - l
                  dat(off,k) = ((1-epsi)*r11 + epsi*r12)*(1-fac)+
     *                         ((1-epsi)*r21 + epsi*r22)*fac
                  flags(off,k) = .true.
                endif
                off = off + 1
                chan = chan + inc
              enddo

            else
c             Many corresponding gains in the table for each channel of
c             data.  Average the gains together.
              do i = 1, nschan0(j)
                ibeg = max(1,nint(chan-hwidth)+ischan(i0))
                iend = min(nint(chan+hwidth)+ischan(i0),
     *                                ischan(i0)+nschan(i0)-1)
                n = 0
                dat(off,k) = 0
                do l = ibeg, iend
                  r11=tab(l,k,ib)
                  if (ib.lt.nbpsols) r21=tab(l,k,ib+1)
                  if (real(r11).ne.0 .or. aimag(r11).ne.0) then
                    fac=factor
                    if (abs(r21).eq.0) fac=0
                    dat(off,k) = dat(off,k) + (1-fac)*r11+fac*r21
                    n = n + 1
                  endif
                enddo
                flags(off,k) = n.gt.0
                if (n.gt.1) dat(off,k) = dat(off,k) / n
                off = off + 1
                chan = chan + inc
              enddo
            endif
          endif
        enddo
      enddo

      end

c***********************************************************************

      subroutine uvGnInic()
c-----------------------------------------------------------------------
c  This reads the channel gains into common from the cgains item.
c-----------------------------------------------------------------------
      integer iostat,item
      include 'uvgn.h'
      include 'mem.h'
c-----------------------------------------------------------------------
c
c  Get the number of channel gains.
c
      call rdhdi(tno,'ncgains',ncgains,0)
      if (ncgains.le.0 .or. ncgains.gt.MAXCHAN) call bug('f',
     *  'Number of gains is missing or bad, in uvGnInic')
      call rdhdi(tno,'ncbase',ncbase,0)
      if (ncbase.le.0 .or. ncbase.gt.MAXBASE) call bug('f',
     *  'Number of baselines is missing or bad, in uvGnInic')
c
c  Allocate memory.
c
      call MemAllop(pCgains,ncgains*ncbase,'c')
c
c  Open the cgains item.
c
      call haccess(tno,item,'cgains','read',iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'accessing cgains table')
c
c  Read in channel gains and finish up..
c
      call hreadr(item,memC(pCgains),0,8*ncgains*ncbase,iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'reading cgains table')
      call hdaccess(item,iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'closing cgains table')

      end

c***********************************************************************

      subroutine uvGnIniw()
c-----------------------------------------------------------------------
c  This reads the wideband gains into common from the wgains item.
c-----------------------------------------------------------------------
      integer iostat,item
      include 'uvgn.h'
      include 'mem.h'

c-----------------------------------------------------------------------
c
c  Get the number of wideband gains.
c
      call rdhdi(tno,'nwgains',nwgains,0)
      if (nwgains.le.0 .or. nwgains.gt.MAXCHAN) call bug('f',
     *  'Number of gains is missing or bad, in uvGnIniw')
      call rdhdi(tno,'nwbase',nwbase,0)
      if (nwbase.le.0 .or. nwbase.gt.MAXBASE) call bug('f',
     *  'Number of baselines is missing or bad, in uvGnIniw')
c
c  Allocate memory.
c
      call MemAllop(pWgains,nwgains*nwbase,'c')
c
c  Open the wgains item.
c
      call haccess(tno,item,'wgains','read',iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'accessing wgains table')
c
c  Read in channel gains and finish up..
c
      call hreadr(item,memC(pWgains),0,8*nwgains*nwbase,iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'reading wgains table')
      call hdaccess(item,iostat)
      if (iostat.ne.0) call UvGnBug(iostat,'closing wgains table')

      end
