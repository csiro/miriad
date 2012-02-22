c***********************************************************************
        program atrecal
        implicit none
c
c= atrecal - Copy and recalibrate a CABB uv dataset.
c& rjs
c: uv analysis
c+
c       atrecal copies a uv dataset and recomputes the systemp and
c       xyphase variables from the autocorrelations.
c       Useful for CABB data with RFI affected Tsys or when splitting  
c       wide bands. Invert uses the systemp variable for the data 
c       weights when options=systemp is used.
c@ vis
c       The name of the input uv data sets. Several can be given (wild
c       cards are supported). No default.
c@ select
c       The normal uv selection commands. The default is copy everything
c@ line
c       The normal uv linetype in the form:
c         line,nchan,start,width,step
c       The default is all channels (or all wide channels if there are
c       no spectral channels). The output will consist of only spectral
c       or wideband data (but not both).
c@ options
c       This gives extra processing options. Several options can be 
c       given, each separated by commas. They may be abbreviated to the 
c       minimum needed to avoid ambiguity. Possible options are:
c          nocal       Do not apply the gains file. By default, atrecal
c                      applies the gains file in copying the data.
c          nopass      Do not apply bandpass corrections. By default,
c                      atrecal corrects for the bandpass shape if the
c                      required information is available.
c          nopol       Do not apply polarization corrections. By default
c                      atrecal corrects for polarization cross-talk.
c          relax       Normally atrecal discards a correlation record if
c                      all the correlations are bad. This option causes
c                      atrecal to retain all records.
c@ out
c       The name of the output uv data set. No default.
c$Id: atrecal.for,v 1.2 2012/02/22 04:07:30 wie017 Exp $
c--
c  Recalibrate CABB data
c  CABB data has auto correlations with noise cal OFF in bin 1 and ON in
c  bin 2. The online system calculates gtp=(on+off)/2 and sdo=on-off
c  averaged over the window set with tvchan. An acal calibration
c  calculates calJy, the size of the calibration noise source (sdo) in Jy.
c  Subsequent data is calibrated from counts to Jy using V=C*CalJy/sdo.
c  Tsys is calculated over the same tvchan window: gtp/sdo*CalJy/JyperK
c
c  Atlod with option notsys undoes this calibration and produces counts
c  divided by 10^6.
c  Calibrating this data in the normal way appears to work ok, and
c  produces images that differ very little. 
c
c  Using raw counts as the visibilities means the amp level changes when
c  you change attenuators on line -  hopefully this will be done only
c  when on a calibrator, so the gain change can be calibrated out.
c
c  This task assumes the data has been flux calibrated and recalculates
c  Tsys using the non-flagged part of the autocorrelation
c  spectrum. The tsys formula used is just: Vis(Jy)/10 (nominal Jy/K).
c  It also recalculates the xyphases from the crosscorrelations.
c
c  History:
c    mhw  08jul10 Original version, based on uvaver.
c    mhw  22feb12 Cleanup, remove unwanted options
c
c------------------------------------------------------------------------
        include 'maxdim.h'
        character version*80
        character uvflags*12,ltype*16,out*64
        integer npol,Snpol,tIn,tOut,vupd,nread,nrec,i,nbin
        real inttime
        logical dotaver,doflush,buffered,PolVary,first
        logical relax,ok,donenpol
        double precision preamble(5),Tmin,Tmax,Tprev,interval
        complex data(MAXCHAN)
        logical flags(MAXCHAN)
        integer ischan(MAXWIN),nspect,nants
c
c  Externals.
c
        logical uvDatOpn,uvVarUpd
        character versan*80
c
c  Get the input parameters.
c
        version=versan('atrecal',
     :                 '$Revision: 1.2 $',
     :                 '$Date: 2012/02/22 04:07:30 $')   
        call keyini
        call GetOpt(uvflags,relax)
        call uvDatInp('vis',uvflags)
        call keya('out',out,' ')
        call keyfin
c
c  Check the input parameters.
c
        if(out.eq.' ')call bug('f','Output file must be specified')
c
c  Various initialisation.
c
        interval = 1.0/(24.*3600.)
        npol = 0
        Snpol = 0
        first = .true.
        PolVary = .false.
        doflush = .false.
        buffered = .false.
        donenpol = .false.
        dotaver = .true.
        call BufIni
        nrec = 0
c
c  Open the input and the output files.
c
        dowhile(uvDatOpn(tIn))
          call uvDatGta('ltype',ltype)
          call VarInit(tIn,ltype)
          call uvVarIni(tIn,vupd)
          call uvVarSet(vupd,'dra')
          call uvVarSet(vupd,'ddec')
          call uvVarSet(vupd,'source')
          call uvVarSet(vupd,'on')
c
c Special processing the first time around.
c
          if(first)then
            call uvopen(tOut,out,'new')
            call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
            call hdcopy(tIn,tOut,'history')
            call hisopen(tOut,'append')
            call hiswrite(tOut,'atrecal: Miriad '//version)
            call hisinput(tOut,'atrecal')
            call hisclose(tOut)
            first = .false.
          endif
          call VarOnit(tIn,tOut,ltype)
c
c  Loop over the data.
c
          call uvDatRd(preamble,data,flags,maxchan,nread)
          nbin = 1
          if(dotaver)then
            call uvrdvri(tIn,'nbin',nbin,1)
            if(nbin.gt.1)then
              call bug('w',
     *        'Time averaging or pol''n selection of bin-mode data')
              call bug('w',
     *        'This will average all bins together')
            endif
          endif
          Tprev = preamble(4)
          Tmin = Tprev
          Tmax = Tmin
          dowhile(nread.gt.0)
c
c  Count the number of records read.
c
            nrec = nrec + 1
c
c  Do we want to keep this record.
c
            ok = relax.or.donenpol
            if(.not.ok)then
              do i=1,nread
                ok = ok.or.flags(i)
              enddo
            endif
c
c  Determine if we need to flush out the averaged data.
c
            doflush = ok.and.dotaver
            if(doflush)then
              doflush = uvVarUpd(vupd)
              doflush = (doflush.or.preamble(4)-Tmin.gt.interval.or.
     *                              Tmax-preamble(4).gt.interval)
     *                  .and.buffered
            endif
c
c  Flush out the accumulated data -- the case of time averaging.
c
            if(doflush)then
              call BufFlush(tOut,npol,nspect,nants)
              PolVary = PolVary.or.npol.eq.0.or.
     *          (Snpol.ne.npol.and.Snpol.gt.0)
              Snpol = npol
              Tmin = preamble(4)
              Tmax = Tmin
              buffered = .false.

            endif
c
c  Accumulate more data, if we are time averaging.
c
            if(dotaver.and.ok)then
              call uvrdvrr(tIn,'inttime',inttime,10.)
              call uvgetvri(tIn,'nspect',nspect,1)
              call uvgetvri(tIn,'ischan',ischan,nspect)
              call uvgetvri(tIn,'nants',nants,1)
              call BufAcc(preamble,inttime,data,flags,nread,ischan,
     *                    nspect)
              buffered = .true.
              call VarCopy(tIn,tOut)
              if(nbin.gt.1)call uvputvri(tOut,'nbin',1,1)
            endif
c
c  Keep on going. Read in another record.
c
            if(ok)then
              Tprev = preamble(4)
              Tmin = min(Tmin,Tprev)
              Tmax = max(Tmax,Tprev)
            endif
            call uvDatRd(preamble,data,flags,maxchan,nread)
          enddo
c
c  Flush out anything remaining.
c
          if(buffered)then
            call BufFlush(tOut,npol,nspect,nants)
            PolVary = PolVary.or.npol.le.0.or.
     *        (Snpol.ne.npol.and.Snpol.gt.0)
            Snpol = npol
            buffered = .false.
          endif
          call uvDatCls
        enddo
c
c  Write things to the header which describe the data as a whole.
c
        if(first)call bug('f','Error opening input')
        if(nrec.eq.0)call bug('f','No data found')
        if(.not.PolVary)call wrhdi(tOut,'npol',Snpol)
c
c  Update the history and close up files.
c
        call uvclose(tOut)
        end
c***********************************************************************
        subroutine GetOpt(uvflags,relax)
c
        implicit none
        logical relax
        character uvflags*(*)
c
c  Determine the flags to pass to the uvdat routines.
c
c  Output:
c    uvflags    Flags to pass to the uvdat routines.
c    relax      Do not discard bad records.
c-----------------------------------------------------------------------
        integer nopts
        parameter(nopts=4)
        character opts(nopts)*9
        integer l
        logical present(nopts),docal,dopol,dopass
        data opts/'nocal    ','nopol    ','nopass   ',
     *            'relax    '/
c
        call options('options',opts,present,nopts)
        docal = .not.present(1)
        dopol = .not.present(2)
        dopass= .not.present(3)
        relax  = present(4)
c
c Set up calibration flags
c
        uvflags = 'dslr3'
        l = 5
        if(docal)then
          l = l + 1
          uvflags(l:l) = 'c'
        endif
        if(dopass)then
          l = l + 1
          uvflags(l:l) = 'f'
        endif
        if(dopol)then
          l = l + 1
          uvflags(l:l) = 'e'
        endif
        end
c***********************************************************************
        subroutine BufIni
        implicit none
c
c  Initialise the routines which do the buffering and averaging of
c  the visibility data.
c  All the buffering/averaging is performed in arrays stored in a
c  common block.
c
c-----------------------------------------------------------------------
        include 'atrecal.h'
        free = 1
        mbase = 0
        nauto = 0
        end
c***********************************************************************
        subroutine BufFlush(tOut,npol,nif,nants)
c
        implicit none
        integer tOut,npol,nif,nants
c
c  This writes out the averaged data. The accumulated data is in common.
c  This starts by dividing the accumulated data by "N", and then writes
c  it out.
c
c  Inputs:
c    tOut       The handle of the output file.
c  Output:
c    npol       The number of polarisations in the output. If this
c               varies, a zero is returned.
c-----------------------------------------------------------------------
        include 'atrecal.h'
        complex data(MAXCHAN)
        real inttime
        double precision preambl(5),time(MAXBASE)
        logical flags(MAXCHAN)
        integer i,j,jd,k,ngood,nbp,p,idx1(MAXBASE),idx2(MAXBASE)
        logical PolVary
c
c  Determine the number of good baselines, and sort them so we have an
c  index of increasing time.
c
        ngood = 0
        do j=1,mbase
          if(cnt(j).gt.0)then
            ngood = ngood + 1
            time(ngood) = preamble(4,j) / cnt(j)
            idx2(ngood) = j
          endif
        enddo
        if(ngood.le.0)return
        call sortidxd(ngood,time,idx1)
c
c  Write out the new Tsys and xyphase values
c
        if (nauto.eq.0) call bug('f','No autocorrelations found')
        call Sct(tOut,'systemp',xtsys,ytsys,  
     *           ATIF,ATANT,1,nif,nants)
        call Sco(tOut,'xyphase', xyphase,
     *           ATIF,ATANT,1,nif,nants)
        call Sco(tOut,'xtsys', xtsys,
     *           ATIF,ATANT,1,nif,nants)
        call Sco(tOut,'ytsys', ytsys,
     *           ATIF,ATANT,1,nif,nants)
c
c  Now loop through the good baselines, writing them out.
c
        nbp = 0
        npol = 0
        do jd=1,ngood
          j = idx2(idx1(jd))
          if(npols(j).ne.npol)then
            call uvputvri(tOut,'npol',npols(j),1)
            PolVary = npol.gt.0
            npol = npols(j)
          endif
          preambl(1) = preamble(1,j) / cnt(j)
          preambl(2) = preamble(2,j) / cnt(j)
          preambl(3) = preamble(3,j) / cnt(j)
          preambl(4) = preamble(4,j) / cnt(j)
          preambl(5) = preamble(5,j) / cnt(j)
          inttime    = preamble(6,j) / npols(j)
          call uvputvrr(tOut,'inttime',inttime,1)
c
c  Average the data in each polarisation. If there is only one scan in
c  the average, not bother to average it.
c
          do i=1,npol
            p = pnt(i,j) - 1
            call uvputvri(tOut,'pol',pols(i,j),1)
            nbp = nbp + 1
c
c  Loop over the channels.
c
            do k=1,nchan(i,j)
              flags(k) = count(k+p).gt.0
              if(.not.flags(k))then
                data(k) = 0
              else
                data(k) = buf(k+p) / count(k+p)
              endif
            enddo
            call uvwrite(tOut,preambl,data,flags,nchan(i,j))
          enddo
        enddo
c
c  Reset the counters.
c
        free = 1
        mbase = 0

c  If the number of polarisations varied, zero npol.
c
        if(PolVary) npol = 0
        end
c***********************************************************************
        subroutine BufAcc(preambl,inttime,data,flags,nread,ischan,nwin)
c
        implicit none
        integer nread,nwin,ischan(nwin)
        double precision preambl(5)
        real inttime
        complex data(nread)
        logical flags(nread)
c
c  This accumulates the visibility data. The accumulated data is left
c  in common.
c
c  Input/Output:
c    preambl    Preamble. Destroyed on output.
c    data       The correlation data to be averaged. Destroyed on output
c    flags      The data flags.
c    nread      The number of channels.
c-----------------------------------------------------------------------
        include 'atrecal.h'
        integer i,i1,i2,p,bl,pol,j,n,iend
        integer PolXX,PolYY,PolXY,PolYX
        complex t
        parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
c
c  Determine the baseline number, and conjugate the data if necessary.
c
        call BasAnt(preambl(5),i1,i2)
        bl = ((i2-1)*i2)/2 + i1
        if(bl.gt.MAXBASE)
     *    call bug('f','Too many baselines for me to handle, in BUFACC')
c
c  Zero up to, and including, this baseline.
c
        do i=mbase+1,bl
          cnt(i) = 0
        enddo
        mbase = max(mbase,bl)
c
c  Add in this visibility.
c
        if(cnt(bl).eq.0)then
          cnt(bl) = inttime
          npols(bl) = 0
          preamble(1,bl) = inttime * preambl(1)
          preamble(2,bl) = inttime * preambl(2)
          preamble(3,bl) = inttime * preambl(3)
          preamble(4,bl) = inttime * preambl(4)
          preamble(5,bl) = inttime * preambl(5)
          preamble(6,bl) = inttime
        else
          cnt(bl) = cnt(bl) + inttime
          preamble(1,bl) = preamble(1,bl) + inttime * preambl(1)
          preamble(2,bl) = preamble(2,bl) + inttime * preambl(2)
          preamble(3,bl) = preamble(3,bl) + inttime * preambl(3)
          preamble(4,bl) = preamble(4,bl) + inttime * preambl(4)
          preamble(5,bl) = preamble(5,bl) + inttime * preambl(5)
          preamble(6,bl) = preamble(6,bl) + inttime
        endif
c
c  Determine the polarisation.
c
        call uvDatGti('pol',pol)
        p = 0
        do i=1,npols(bl)
          if(pols(i,bl).eq.pol) p = i
        enddo
                
c
c  A new baseline. Set up the description of it.
c
        if(p.eq.0)then
          npols(bl) = npols(bl) + 1
          p = npols(bl)
          if(p.gt.MAXPOL) call bug('f',
     *      'Too many polarizations, in BufAcc')
          pols(p,bl) = pol
          nchan(p,bl) = nread
          pnt(p,bl) = free
          free = free + nread
          if(free.gt.MAXAVER)call bug('f',
     *      'Too much data to accumulate, in BufAcc')
c
c  Copy across the new data.
c
          p = pnt(p,bl) - 1
          do i=1,nread
            if(flags(i))then
              buf(i+p) = inttime * data(i)
              bufr(i+p) = inttime * abs(data(i))
              count(i+p) = inttime
            else
              buf(i+p) = (0.0,0.0)
              bufr(i+p) = 0.0
              count(i+p) = 0
            endif
          enddo
c
c  Else accumulate new data for old baseline.
c
        else
          nread = min(nread,nchan(p,bl))
          nchan(p,bl) = nread
          p = pnt(p,bl) - 1
          do i=1,nread
            if(flags(i))then
              buf(i+p) = buf(i+p) + inttime * data(i)
              bufr(i+p) = bufr(i+p) + inttime * abs(data(i))
              count(i+p) = count(i+p) + inttime
            endif
          enddo
        endif
c
c  Process auto correlations
c
        if (i1.eq.i2) then
          if (pol.eq.PolXX.or.pol.eq.PolYY.or.pol.eq.PolYX) then
            do i=1,nwin
              t=0
              if (i.eq.nwin) then
                iend=nread
              else 
                iend=ischan(i+1)-1
              endif
              n=0
              do j=ischan(i),iend
                if(flags(j)) then
                  t=t+data(j)
                  n=n+1
                endif  
              enddo
              if (n.gt.0) then
                nauto = nauto + 1
                if (pol.eq.PolXX) then
                  xtsys(i,i1)=real(t)/10/n
                else if (pol.eq.PolYY) then
                  ytsys(i,i1)=real(t)/10/n
                else
                  xyphase(i,i1)=atan2(aimag(t),real(t))
                endif
              endif
            enddo
          endif
        endif
c
        end
c***********************************************************************
        subroutine Sct(tno,var,xtsys,ytsys,ATIF,ATANT,if1,if2,nants)
c
        integer tno,ATIF,ATANT,if1,if2,nants
        character var*(*)
        real xtsys(ATIF,ATANT),ytsys(ATIF,ATANT)
c
c  Write out the SYSTEMP variable, which we fudge to be the geometric
c  mean of the xtsys and ytsys variables.
c-----------------------------------------------------------------------
        integer ant,if,cnt
        real buf(ATIF*ATANT)
c
        cnt = 0
        do if=if1,if2
          do ant=1,nants
            cnt = cnt + 1
            buf(cnt) = sqrt(xtsys(if,ant)*ytsys(if,ant))
          enddo
        enddo
c
        call uvputvrr(tno,var,buf,cnt)
c
        end

c***********************************************************************
        subroutine Sco(tno,var,dat,ATIF,ATANT,if1,if2,nants)
c
        integer ATIF,ATANT,if1,if2,nants,tno
        character var*(*)
        real dat(ATIF,ATANT),buf(ATIF*ATANT)
c
c  Write out a syscal variable.
c-----------------------------------------------------------------------
        integer ant,if,cnt
c
        cnt = 0
        do if=if1,if2
          do ant=1,nants
            cnt = cnt + 1
            buf(cnt) = dat(if,ant)
          enddo
        enddo
c
        call uvputvrr(tno,var,buf,cnt)
c
        end
