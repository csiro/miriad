c********1*********2*********3*********4*********5*********6*********7*c
        program gpedit
        implicit none
c
c= Gpedit -- Edit the gain table.
c& mchw
c: calibration
c+
c       Gpedit is a MIRIAD task which modifies calibration tables.
c@ vis
c       The input visibility file, containing the gain file to modify.
c       No default.
c@ select
c       Normal uv data selection commands. See the help on "select" for
c       more information. Currently only antenna, time, and amplitude
c       selection is supported. The amplitude selection applies to the
c       gains, not the uv-data, and can be used to flag or replace bad
c       gain amplitudes. The default is to select everything.
c@ feeds
c       The polarisation feeds affected (e.g. R, L, X or Y). Default is
c       all feeds.
c@ gain
c       This gives the complex-valued gain used in the `multiply' and
c       `replace' options (see below). It is given as an amplitude
c       and phase (in degrees). For example gain=2,90 produces a gain
c       with a amplitude of 2 and phase of 90 degrees.
c       The first two values specify the continuum solution, if there is
c       a gain table with n binned solutions, another n*2 values can be
c       specified. The default is 1,0 for all gains.
c@ options
c       This gives extra processing options. Several values can be given,
c       separated by commas. Option values can be abbreviated to
c       uniqueness.
c
c       The following options operate on the gains:
c         replace   The existing gains are replaced by the value
c                   given by the `gain' keyword. NOTE: If no other
c                   options are given, the replace option is performed.
c         multiply  The existing gains are multiplied by the gain
c                   given by the `gain' keyword.
c         flag      The existing gains are flagged as bad.
c         amplitude The phases of the existing gains are set to 0.
c         phase     The amplitudes of the existing gains are set to 1.
c         scale     The phase of the gains is multiplied by the factor
c                   given by the `gain' keyword.
c         dup       Convert a single-polarization gain table into a dual
c                   polarization table.
c         invert    The existing gains are inverted (1/gain, phase
c                   negated) to allow undoing calibration that has been
c                   applied to the uvdata with uvaver.
c
c       The following option operates on the polarization leakages:
c         reflect   The existing leakages are made to possess a
c                   symmetry: For each antenna, we make
c                   DX = -conjg(DY).
c         zmean     Subtract an offset, so that the mean leakage is 0.
c
c       The following option operates on the bandpass table:
c         extend    Extend the bandpass table to cover the full range of
c                   channels in the data. Use gain=0 to extend the
c                   range by replicating the edge value and gain=1 to use
c                   linear extrapolation of the edge values.
c         hanning   Hanning smooth the bandpass. (Miriad actually uses
c                   binomial smoothing when it claims to use hanning)
c         boxcar    Boxcar smooth the bandpass (i.e., running average)
c@ width
c       The smoothing width for hanning or boxcar smoothing of the
c       bandpass. Width will be increased to next odd number if even.
c       Default and minimum value is 3. Maximum is 33 for hanning, 
c       and 257 for boxcar smoothing.
c       An optional second width parameter can be used to specify blocks
c       of channels to smooth, with no smoothing across block boundaries.
c       Default 0, is no blocks.      
c
c       Example:
c         gpedit vis=cyga gain=14.4,90 select=ant(1,2) feeds=X
c         gpedit vis=cyga options=boxcar width=15
c--
c  History:
c    rjs    4sep91 gpbreak. gpedit code copied from gpbreak.
c    mchw  23apr96 keyword driven task for the squeamish. Instead of BEE.
c    rjs   25feb97 Tidy up and extend the possibilities.
c    rjs   26feb97 Fix feeds reading.
c    rjs   24jun97 Add the reflect option.
c    rjs   01aug97 Added options=zmean.
c    mchw  18nov98 Added options=scale.
c    rjs   01dec98 Added options=dup.
c    tw    16aug03 Allow gain amplitude selection
c    rjs   02jan05 Correct gain selection.
c    mhw   01mar11 Added options=invert
c    mhw   08sep11 Handle freq binned gains
c    mhw   19mar15 Gain argument now takes binned gains too
c    mhw   06oct15 Add bandpass extension, smoothing
c    mhw   26feb18 Add 'askap' smoothing 
c-----------------------------------------------------------------------
        include 'maxdim.h'
        include 'mem.h'
        include 'mirconst.h'
        integer MAXFEED,MAXSELS
        character version*72
        parameter(MAXFEED=2,MAXSELS=300)
c
        character vis*64
        logical domult,dorep,doflag,doamp,dophas,dorefl,dozm,doscal
        logical dogain,doleak,dup,doinv,doext,dohan,dobox,doband
        integer iostat,tVis,itLeak,nants,nfeeds,nsols,ntau,i
        integer numfeed,feeds(MAXFEED),nleaks,nfbin,ngains,maxgains
        integer numamph,n,width,blocksize
        complex gain(0:MAXFBIN),Leaks(2,MAXANT)
        real sels(MAXSELS),amph(MAXFBIN+1),phi
        logical mask(2*MAXANT)
        integer pGains,pTimes
        double precision freq(MAXFBIN)
c
c  Externals.
c
        integer hsize
        logical hdprsnt
        character versan*72
        external MultOp,RepOp,FlagOp,AmpOp,PhasOp,ScalOp,InvOp
c
c  Get the input parameters.
c
        version = versan('gpedit',
     *                   '$Revision: 1.13 $',
     *                   '$Date: 2018/02/25 22:59:59 $')
        call keyini
        call keya('vis',vis,' ')
        if(vis.eq.' ')call bug('f','No input vis data-set given')
        call SelInput('select',sels,MAXSELS)
        call mkeyr('gain',amph,2*(MAXFBIN+1),numamph)
        call mkeyfd('feeds',feeds,MAXFEED,numfeed)
        call GetOpt(dorep,domult,doflag,doamp,dophas,dorefl,dozm,
     *    doscal,dup,doinv,doext,dohan,dobox)
        dogain = dorep.or.domult.or.doflag.or.doamp.or.
     *                           dophas.or.doscal.or.dup.or.doinv
        doleak = dorefl.or.dozm
        doband = (doext.or.dohan.or.dobox)
        call keyi('width',width,3)
        width=min(257,(max(3,width)/2)*2+1)
        if (dohan) width=min(33,width)
        call keyi('width',blocksize,0)
        call keyfin
c
c  Open the input file. Use the hio routines, as all we want to get
c  at is items for which the uvio routines have no access anyway.
c
        call hopen(tVis,vis,'old',iostat)
        if(iostat.ne.0)call EditBug(iostat,'Error opening '//vis)
        if(dogain.and..not.hdprsnt(tVis,'gains'))
     *    call bug('f','The dataset has no gain table')
        if(doleak.and..not.hdprsnt(tVis,'leakage'))
     *    call bug('f','The dataset has no leakage table')
        if(doext.and..not.hdprsnt(tVis,'bandpass'))
     *    call bug('f','The dataset has no bandpass table')
c
c  Determine the number of things in the gain table.
c
        if(dogain)then
          call rdhdi(tVis,'ntau',ntau,0)
          if(ntau.ne.0)call bug('f',
     *    'GPEDIT cannot cope with a gain table with delays')
          call rdhdi(tVis,'ngains',nants,0)
          call rdhdi(tVis,'nfeeds',nfeeds,1)
          if(nfeeds.le.0.or.nfeeds.gt.2.or.nants.lt.nfeeds.or.
     *      mod(nants,nfeeds).ne.0)
     *      call bug('f','Bad number of gains or feeds in '//vis)
          nants = nants / nfeeds
          if(nfeeds.eq.2.and.dup)call bug('w',
     *      'Gain table is already a dual polarization table')
          dup = dup.and.nfeeds.eq.1
          call rdhdi(tVis,'nsols',nsols,0)
c
c  See if we have enough space.
c
          if(nants.gt.MAXANT)
     *      call bug('f','Too many antennae for me to cope with')
          if(nsols.le.0)
     *      call bug('f','Bad number of solutions')
          maxgains = nsols*nants*2*(maxfbin+1)
          call memAlloc(pGains,maxgains,'c')
          call memAlloc(pTimes,nsols,'d')
c
c  Check the given feed numbers, and set the default feed numbers
c  if needed.
c
          if(numfeed.gt.0)then
            do i=1,numfeed
              if(feeds(i).gt.nfeeds)then
                call bug('f','Invalid feeds specified.')
                call bug('f','Gain table has only one feed')
              endif
            enddo
          else
            do i=1,nfeeds
              feeds(i) = i
            enddo
            numfeed = nfeeds
          endif
c
c  Set up the breakpoint mask.
c
          call SetMask(nfeeds,nants,mask,feeds,numfeed,sels)
c
c  Open the gains file. Mode=='append' so that we can overwrite it.
c
c         call haccess(tVis,itGain,'gains','append',iostat)
c         if(iostat.ne.0)call EditBug(iostat,'Error accessing gains')
c
c  Read the gains.
c
          call uvGnRead(tVis,memc(pGains),memd(pTimes),freq,
     *    ngains,nfeeds,ntau,nsols,nfbin,maxgains,nsols,maxfbin)
c         call GainRd(itGain,nsols,nants,nfeeds,
c     *                         memd(pTimes),memc(pGains))
c
c  Edit the gains.
c
          n=min(numamph/2,nfbin+1)
          do i=0,nfbin
            gain(i)=1.0
          enddo
          do i=1,n
            phi=amph(i*2)
            gain(i-1)=amph(i*2-1)*
     *        cmplx(cos(phi*pi/180.),sin(phi*pi/180.))
          enddo
          if(dorep) call GainEdt(nsols,nants*nfeeds,nfbin,
     *      memd(pTimes),memc(pGains),mask,sels,gain,RepOp)
          if(domult)call GainEdt(nsols,nants*nfeeds,nfbin,
     *      memd(pTimes),memc(pGains),mask,sels,gain,MultOp)
          if(doflag)call GainEdt(nsols,nants*nfeeds,nfbin,
     *      memd(pTimes),memc(pGains),mask,sels,gain,FlagOp)
          if(doamp )call GainEdt(nsols,nants*nfeeds,nfbin,
     *      memd(pTimes),memc(pGains),mask,sels,gain,AmpOp)
          if(dophas)call GainEdt(nsols,nants*nfeeds,nfbin,
     *      memd(pTimes),memc(pGains),mask,sels,gain,PhasOp)
          if(doscal)call GainEdt(nsols,nants*nfeeds,nfbin,
     *      memd(pTimes),memc(pGains),mask,sels,gain,ScalOp)
          if(doinv) call GainEdt(nsols,nants*nfeeds,nfbin,
     *      memd(pTimes),memc(pGains),mask,sels,gain,InvOp)
c
c  Duplicate gains if needed
c
          if (dup) call GainDup(memc(pGains),ngains,maxgains)

c
c  Write out the gains.
c
          call uvGnWrit(tVis,memc(pGains),memd(pTimes),freq,ngains,
     *      nsols,nfbin,maxgains,nsols,maxfbin,.false.)
c         call GainWr(itGain,dup,nsols,nants,nfeeds,
c     *                         memd(pTimes),memc(pGains))
          call memFree(pTimes,nsols,'d')
          call memFree(pGains,nfeeds*nants*nsols,'c')
c         call hdaccess(itGain,iostat)
          if(dup)then
            call wrhdi(tVis,'nfeeds',2)
            call wrhdi(tVis,'ngains',2*nants)
          endif
        endif
c
c  Process the leakages if needed.
c
        if(doleak)then
          call haccess(tVis,itLeak,'leakage','append',iostat)
          if(iostat.ne.0)call EditBug(iostat,'Error accessing leakages')
          nLeaks = (hsize(itLeak)-8)/16
          if(nLeaks.lt.1)call bug('f','Leakage table appears bad')
          call hreadr(itLeak,Leaks,8,16*nLeaks,iostat)
          if(iostat.ne.0)call EditBug(iostat,
     *                          'Error reading leakage table')
          if(dorefl.or.dozm)call LeakIt(Leaks,nLeaks,sels,dorefl,dozm)
          call hwriter(itLeak,Leaks,8,16*nLeaks,iostat)
          call hdaccess(itLeak,iostat)
        endif
c
c  Process the bandpass if needed
c
        if(doband) then
c         We need some access to uv variables
          call hclose(tVis)
          call uvopen(tVis,vis,'old')
          call PassEdit(tVis,doext,amph(1).gt.0,dohan,dobox,width,
     *      blocksize)
          call uvclose(tVis)
          call hopen(tVis,vis,'old',iostat)
          if(iostat.ne.0)call EditBug(iostat,'Error opening '//vis)
        endif
c
c  Write outsome history now.
c
        call hisopen(tVis,'append')
        call hiswrite(tVis,'GPEDIT: Miriad '//version)
        call hisinput(tVis,'GPEDIT')
        call hisclose(tVis)
c
c  Close up everything.
c
        call hclose(tVis)
        end
c************************************************************************
        subroutine PassEdit(tVis,doext,dolin,dohan,dobox,width,
     *    blocksize)
c
        implicit none
        integer tVis,width,blocksize
        logical doext,dolin,dohan,dobox
c
c  Extend the bandpass to cover the full spectrum
c------------------------------------------------------------------------
        include 'maxdim.h'
        include 'mem.h'
        integer MAXSOLN
        parameter (MAXSOLN=1024)
        integer nchan0,nspect0,nschan0(1),Size0
        integer nchan1,nspect1,nschan1(1),Size1
        integer ngains,nfeeds,ntau,nants,nbpsols,nlow,nhigh,n,i
        integer item,off,offg,iostat
        ptrdiff pGains0,pGains1
        double precision sfreq0(1),sdf0(1),freqs(2)
        double precision sfreq1(1),sdf1(1),bptimes(MAXSOLN)
c external
        integer hsize,uvscan
c
        call rdhdi(tVis,'nchan0',nchan0,0)
        call rdhdi(tVis,'nspect0',nspect0,0)
        if(nchan0.lt.1.or.nspect0.lt.1)
     *    call bug('f','Bandpass table appears bad')
        if (nspect0.gt.1) call bug('f','gpedit extend can only '//
     *    'handle a single spectrum')
        call rdhdi(tVis,'ngains',ngains,0)
        call rdhdi(tVis,'nfeeds',nfeeds,1)
        call rdhdi(tVis,'ntau',  ntau,  0)
        nants = ngains / (ntau + nfeeds)
        call rdhdi(tVis,'nbpsols', nbpsols,0)
        n=max(1,nbpsols)
        if (n+2.gt.MAXSOLN) call bug('f','Too many bandpass solutions')
        if (ngains.le.0 ) call bug('f','Invalid value for ngains')
        Size0 = ngains*nchan0*n
        call MemAllop(pGains0,Size0,'c')

c     Load the frequency table.
        if (doext) then
          call haccess(tVis,item,'freqs','read',iostat)
          if (iostat.ne.0) call bug('f','Error accessing freqs table')
          off = 8
          do i = 1, nspect0
            call hreadi(item,nschan0(i),off,4,iostat)
            if (iostat.ne.0) call bug('f','Error reading freqs table')
            off = off + 8
            call hreadd(item,freqs,off,16,iostat)
            if (iostat.ne.0) call bug('f','Error reading freqs table')
            off = off + 16
            sfreq0(i) = freqs(1)
            sdf0(i)   = freqs(2)
          enddo

          call hdaccess(item,iostat)
          if (iostat.ne.0) call bug('f','Error closing freqs table')
        endif
c     Read in the gains themselves now.
        call haccess(tVis,item,'bandpass','read',iostat)
        if (iostat.ne.0) call bug('f','Error accessing bandpass table')
        if (hsize(item).ne.8+n*ngains*nchan0*8+nbpsols*8) then
          call bug('f', 'Bandpass table size is incorrect.')
        endif

        bptimes(1) = -1e20
        bptimes(nbpsols+2) = 1e20
        off=8
        offg=0
        do i = 1, n
          call hreadr(item,memC(pGains0+offg),off,8*ngains*nchan0,
     *      iostat)
          off=off+8*ngains*nchan0
          offg=offg+ngains*nchan0
          if (nbpsols.gt.0) then
            call hreadd(item,bptimes(i+1),off,8,iostat)
            off=off+8
          endif
          if (iostat.ne.0) call bug('f','Error reading bandpass table')
        enddo
        call hdaccess(item,iostat)
        if (iostat.ne.0) call bug('f','Error closing bandpass table')
        nbpsols=n

c     Get the spectral layout of the data
        iostat=uvscan(tVis,'corr')
        call uvrdvri(tVis,'nchan',nchan1,1)
        call uvrdvri(tVis,'nspect',nspect1,1)
        if (nspect1.gt.1) call bug('f',
     *   '>1 spectrum found in data, please use uvsplit first')
        call uvgetvri(tVis,'nschan',nschan1,nspect1)
        call uvgetvrd(tVis,'sdf',sdf1,nspect1)
        call uvgetvrd(tVis,'sfreq',sfreq1,nspect1)

c     Figure out how much to extend the bandpass by
        nlow=0
        nhigh=0
        if (doext) then
          if (sdf1(1)*sdf0(1).gt.0) then
            nlow=nint((sfreq0(1)-sfreq1(1))/sdf0(1))
            nhigh=nint((sfreq1(1)+sdf1(1)*(nchan1-1)-
     *               (sfreq0(1)+sdf0(1)*(nchan0-1)))/sdf0(1))
            if (nchan0+nlow+nhigh.ne.nchan1)
     *    call bug('w','bandpass spectrum size differs from data')
          else
            call bug('f','Bandpass and spectrum incompatible')
          endif
        endif
        Size1 = ngains*(nchan0+nlow+nhigh)*n
        call MemAllop(pGains1,Size1,'c')
        if (doext) call PassExt(memC(pGains0),memC(pGains1),nchan0,
     *      ngains,n,nlow,nhigh,dolin)
        if (dohan.or.dobox) call PassSm(memC(pGains0),memC(pGains1),
     *      nchan0,ngains,n,dohan,dobox,width,blocksize)

        call haccess(tVis,item,'bandpass','append',iostat)
        if (iostat.ne.0) call bug('f','Error writing bandpass table')
        nchan1=nchan0+nlow+nhigh
        off=8
        offg=0
        do i = 1, n
          call hwriter(item,memC(pGains1+offg),off,8*ngains*nchan1,
     *      iostat)
          off=off+8*ngains*nchan1
          offg=offg+ngains*nchan1
          if (nbpsols.gt.0) then
            call hwrited(item,bptimes(i+1),off,8,iostat)
            off=off+8
          endif
          if (iostat.ne.0) call bug('f','Error writing bandpass table')
        enddo
        call hdaccess(item,iostat)
        call MemFrep(pGains1,Size1,'c')
        call MemFrep(pGains0,Size0,'c')

c
c  Update the frequency table
c
        if (doext) then
          call haccess(tVis,item,'freqs','write',iostat)
          if (iostat.ne.0) call bug('f','Error writing frequency table')
          call hwritei(item,0,0,4,iostat)
          off = 8
          do i=1,nspect1
            call hwritei(item,nschan1(i),off,4,iostat)
            off = off + 8
            if(iostat.ne.0)then
              call bug('f','Error writing nschan to freq table')
            endif
            freqs(1) = sfreq0(i)-nlow*sdf0(i)
            freqs(2) = sdf0(i)
            call hwrited(item,freqs,off,2*8,iostat)
            off = off + 2*8
            if(iostat.ne.0)then
              call bug('f','Error writing freqs to freq table')
            endif
          enddo
          call hdaccess(item,iostat)
          if(iostat.ne.0)call bug('f','Error closing freq table')
          call wrhdi(tVis,'nchan0',nchan1)
        endif

c
        end
c************************************************************************
        subroutine PassExt(Gains0,Gains1,nchan0,ngains,ntimes,
     *     nlow,nhigh,dolin)
c
        implicit none
        integer ngains,nchan0,ntimes,nlow,nhigh
        complex Gains0(nchan0,ngains,ntimes)
        complex Gains1(nchan0+nlow+nhigh,ngains,ntimes)
        logical dolin
c
c  Copy bandpass gains across and extend to cover full spectrum
c------------------------------------------------------------------------
        integer itime,ig,ich

        do itime=1,ntimes
          do ig=1,ngains
            do ich=1,nlow
              if (dolin) then
                Gains1(ich,ig,itime)=Gains0(1,ig,itime)-(nlow-ich+1)*
     *           (Gains0(2,ig,itime)-Gains0(1,ig,itime))
              else
                Gains1(ich,ig,itime)=Gains0(1,ig,itime)
              endif
            enddo
            do ich=1,nchan0
              Gains1(ich+nlow,ig,itime)=Gains0(ich,ig,itime)
            enddo
            do ich=1,nhigh
              if (dolin) then
                Gains1(ich+nlow+nchan0,ig,itime)=
     *            Gains0(nchan0,ig,itime)+ich*
     *            (Gains0(nchan0,ig,itime)-Gains0(nchan0-1,ig,itime))
              else
                Gains1(ich+nlow+nchan0,ig,itime)=Gains0(nchan0,ig,itime)
              endif
            enddo
          enddo
        enddo
      end

c************************************************************************
          subroutine PassSm(Gains0,Gains1,nchan0,ngains,ntimes,
       *     dohan,dobox,width,blocksize)
c
          implicit none
          integer ngains,nchan0,ntimes,width,blocksize
          complex Gains0(nchan0,ngains,ntimes)
          complex Gains1(nchan0,ngains,ntimes)
          logical dohan,dobox
c
c  Smooth bandpass gains with hanning or boxcar averaging
c------------------------------------------------------------------------
          include 'maxdim.h'
          integer itime,ig,ich,k
          integer MAXWIDTH
          PARAMETER (MAXWIDTH=257)
          real work(MAXWIDTH),coeffs(MAXWIDTH),re(MAXCHAN),im(MAXCHAN)

          if (width.gt.MAXWIDTH)
     *       call bug('f','Smoothing window width too large')
          if (blocksize.gt.0) then
            if(width.gt.blocksize)
     *        call bug('f','Smoothing window larger than block size')
            if ((nchan0/blocksize)*blocksize.ne.nchan0)
     *        call bug('f','# Channels not a multiple of block size')
          endif
          if (dohan) call hcoeffs(width,coeffs)
          if (dobox) call bcoeffs(width,coeffs)
          do itime=1,ntimes
            do ig=1,ngains
              if (blocksize.gt.1) then
                do ich=0,nchan0-1,blocksize
                  do k=1,blocksize
                    re(k)=real(Gains0(ich+k,ig,itime))
                    im(k)=imag(Gains0(ich+k,ig,itime))
                  enddo
                  if (dohan) call hannsm(width,coeffs,blocksize,re,work)
                  if (dohan) call hannsm(width,coeffs,blocksize,im,work)
                  if (dobox)
     *              call boxcarsm(width,coeffs,blocksize,re,work)
                  if (dobox)
     *              call boxcarsm(width,coeffs,blocksize,im,work)
                  do k=1,blocksize
                    Gains1(ich+k,ig,itime)=Complex(re(k),im(k))
                  enddo
                enddo
              else
                do ich=1,nchan0
                  re(ich)=real(Gains0(ich,ig,itime))
                  im(ich)=imag(Gains0(ich,ig,itime))
                enddo
                if (dohan) call hannsm(width,coeffs,nchan0,re,work)
                if (dohan) call hannsm(width,coeffs,nchan0,im,work)
                if (dobox) call boxcarsm(width,coeffs,nchan0,re,work)
                if (dobox) call boxcarsm(width,coeffs,nchan0,im,work)
                do ich=1,nchan0
                  Gains1(ich,ig,itime)=Complex(re(ich),im(ich))
                enddo
              endif
            enddo
          enddo
        end



c************************************************************************
        subroutine Leakit(Leaks,nants,sels,dorefl,dozm)
c
        implicit none
        integer nants
        real sels(*)
        complex Leaks(2,nants)
        logical dorefl,dozm
c
c  Make the leakages have that funny symmetry.
c------------------------------------------------------------------------
        integer i,n
        complex D
c
c  Externals.
c
        logical SelProbe
c
        if(dorefl)then
          do i=1,nants
            if(SelProbe(sels,'antennae',dble(257*i)))then
              D = 0.5*(Leaks(1,i) - conjg(Leaks(2,i)))
              Leaks(1,i) = D
              Leaks(2,i) = -conjg(D)
            endif
          enddo
        endif
c
        if(dozm)then
          D = 0
          n = 0
          do i=1,nants
            if(SelProbe(sels,'antennae',dble(257*i)))then
              D = D + Leaks(1,i) - conjg(Leaks(2,i))
              n = n + 2
            endif
          enddo
          D = D / n
          do i=1,nants
            if(SelProbe(sels,'antennae',dble(257*i)))then
              Leaks(1,i) = Leaks(1,i) - D
              Leaks(2,i) = Leaks(2,i) + conjg(D)
            endif
          enddo
        endif
c
        end
c********1*********2*********3*********4*********5*********6*********7*c
        subroutine SetMask(nfeeds,nants,mask,feeds,numfeed,sels)
c
        implicit none
        integer nfeeds,nants,numfeed
        integer feeds(numfeed)
        real sels(*)
        logical mask(nfeeds,nants)
c
c  Set up the breakpoint mask.
c
c  Input:
c    nfeeds
c    nants
c    feeds
c    numfeed
c    sels
c  Output:
c    mask
c-----------------------------------------------------------------------
        integer i,i0,j
c
c  Externals.
c
        logical SelProbe
c
        do j=1,nants
          do i=1,nfeeds
            mask(i,j) = .false.
          enddo
          if(SelProbe(sels,'antennae',dble(257*j)))then
            do i0=1,numfeed
              i = feeds(i0)
              mask(i,j) = .true.
            enddo
          endif
        enddo
c
        end
c********1*********2*********3*********4*********5*********6*********7*c
        subroutine GainRd(itGain,nsols,nants,nfeeds,times,Gains)
c
        implicit none
        integer itGain,nsols,nants,nfeeds
        complex Gains(nfeeds*nants,nsols)
        double precision times(nsols)
c
c  Read the gains from the gains table.
c
c  Input:
c    itGain     The item handle of the gains table.
c    nsols      Number of solutions.
c    nants      Number of antennae
c    nfeeds     Number of feeds.
c  Output:
c    times      The read times.
c    gains      The gains.
c-----------------------------------------------------------------------
        integer offset,iostat,k
c
        offset = 8
        do k=1,nsols
          call hreadd(itGain,times(k),offset,8,iostat)
          if(iostat.ne.0)call EditBug(iostat,'Error reading gain time')
          offset = offset + 8
          call hreadr(itGain,Gains(1,k),offset,8*nfeeds*nants,iostat)
          if(iostat.ne.0)call EditBug(iostat,'Error reading gains')
          offset = offset + 8*nfeeds*nants
        enddo
        end
c********1*********2*********3*********4*********5*********6*********7*c
        subroutine GainEdt(nsols,nants,nfbin,times,Gains,mask,sels,
     *                                                  gain,oper)
c
        implicit none
        integer nsols,nants,nfbin
        double precision times(nsols)
        complex Gains(nants,nsols,0:nfbin),gain(0:nfbin)
        real sels(*)
        logical mask(nants)
        external oper
c
c  Edit the gains.
c
c  Input:
c    nsols      number of solutions.
c    nants      Number of antennae times the number of feeds.
c    nfbin      Number of frequency bins (0=continuum only)
c    mask       Antenna/feed mask.
c    sels       UV selection array.
c    gain       Complex gains to be used.
c    oper       The routine to perform the operation.
c  Input/Output:
c    gains      The gains.
c-----------------------------------------------------------------------
        integer i,j,k
c
c  Externals.
c
        logical SelProbe
c
        do k=0,nfbin
          do j=1,nsols
            if(SelProbe(sels,'time',times(j)))then
              do i=1,nants
                if (mask(i)) then
                   if (SelProbe(sels,'amplitude',
     *                  dble(abs(Gains(i,j,k))))) then
                      call oper(Gains(i,j,k),gain(k))
                   endif
                endif
              enddo
            endif
          enddo
        enddo
c
        end
c************************************************************************
c
c  These are the service routines to perform the desired operation.
c
        subroutine MultOp(Gain,fac)
c
        implicit none
        complex Gain,fac
c
        Gain = Gain * fac
        end
        subroutine RepOp(Gain,fac)
c
        implicit none
        complex Gain,fac
c
        Gain = fac
        end
        subroutine FlagOp(Gain,fac)
c
        implicit none
        complex Gain,fac
c
        Gain = 0
        end
        subroutine AmpOp(Gain,fac)
c
        implicit none
        complex Gain,fac
c
        Gain = abs(Gain)
        end
        subroutine PhasOp(Gain,fac)
c
        implicit none
        complex Gain,fac
c
        real t
        t = abs(Gain)
        if(t.gt.0)Gain = Gain / t
        end
c
        subroutine ScalOp(Gain,fac)
c
        implicit none
        complex Gain,fac
c
        complex expi
        real phase
        Gain = abs(Gain)*expi(real(fac)*phase(Gain))
        end
c
        subroutine InvOp(Gain,fac)
c
        implicit none
        complex Gain,fac
c
        real t
        t = abs(Gain)
        if (t.gt.0) Gain = 1 / Gain
        end

c********1*********2*********3*********4*********5*********6*********7*c
        subroutine GainDup(Gains,ngains,maxgains)
c
        implicit none
        integer ngains,maxgains
        complex Gains(maxgains)
c
c  Duplicate the gains to turn a single feed table into a dual feed one
c
c  Input/Output:
c    Gains      The single feed gains
c    ngains     Number gains
c    maxgains   Max number of gains
c-----------------------------------------------------------------------
        include 'maxdim.h'
c
        integer i
c
        if(2*ngains.gt.maxgains)call bug('f',
     *     'Too many gains in GainDup')
        do i=ngains,1,-1
          gains(i*2-1)=gains(i)
          gains(i*2)=gains(i)
        enddo
        ngains=ngains*2
        end

c********1*********2*********3*********4*********5*********6*********7*c
        subroutine GainWr(itGain,dup,nsols,nants,nfeeds,times,Gains)
c
        implicit none
        integer itGain,nsols,nants,nfeeds
        complex Gains(nfeeds*nants,nsols)
        double precision times(nsols)
        logical dup
c
c  Write the gains from the gains table.
c
c  Input:
c    itGain     The item handle of the gains table.
c    nsols      Number of solutions.
c    nants      Number of antennae
c    nfeeds     Number of feeds.
c    times      The read times.
c    gains      The gains.
c    dup        If true, convert a single into a dual polarization table.
c-----------------------------------------------------------------------
        include 'maxdim.h'
c
        complex G(MAXANT)
        integer offset,iostat,i,j,k
c
        if(nants.gt.MAXANT)call bug('f','Too many gains in GainWr')
        offset = 8
        do k=1,nsols
          call hwrited(itGain,times(k),offset,8,iostat)
          if(iostat.ne.0)call EditBug(iostat,'Error writing gain time')
          offset = offset + 8
          if(dup)then
            j = 1
            do i=1,nants
              G(j) = Gains(i,k)
              G(j+1) = G(j)
              j = j + 2
            enddo
            call hwriter(itGain,G,offset,8*2*nants,iostat)
            offset = offset + 8*2*nants
          else
            call hwriter(itGain,Gains(1,k),offset,8*nfeeds*nants,
     *                                                      iostat)
            offset = offset + 8*nfeeds*nants
          endif
          if(iostat.ne.0)call EditBug(iostat,'Error writing gains')
        enddo
        end
c********1*********2*********3*********4*********5*********6*********7*c
        subroutine mkeyfd(keyw,feeds,maxfeeds,nfeeds)
c
        implicit none
        character keyw*(*)
        integer maxfeeds,nfeeds
        integer feeds(maxfeeds)
c
c  This gets feed codes from the user (i.e. 'r','l','x' or 'y'), and
c  converts them to 1 (for 'r' and 'x') or 2 (for 'l' and 'y').
c
c  Input:
c    keyw       Keyword to use in calling keya.
c    maxfeeds   Max number of feeds to return.
c  Output:
c    feeds      The given feeds.
c    nfeeds     The number of feeds retrieved.
c-----------------------------------------------------------------------
        character string*4
        logical more
        integer i
        integer nallfds
        parameter(nallfds=8)
        character allfds(nallfds)
        integer codes(nallfds)
c
c  Externals.
c
        integer binsrcha
        logical keyprsnt
c
c  Data statements.
c
        data allfds/'L','R','X','Y','l','r','x','y'/
        data codes / 2 , 1 , 1 , 2 , 2 , 1 , 1 , 2 /
c
        nfeeds = 0
        more = keyprsnt(keyw)
        call keya(keyw,string,' ')
        dowhile(nfeeds.lt.maxfeeds.and.more)
          i = binsrcha(string,allfds,nallfds)
          if(i.eq.0)call bug('f','Unrecognised feed mnemonic: '//string)
          nfeeds = nfeeds + 1
          feeds(nfeeds) = codes(i)
          call keya(keyw,string,' ')
          more = keyprsnt(keyw)
        enddo
c
        if(more)call bug('f','Too many feeds given')
        end
c********1*********2*********3*********4*********5*********6*********7*c
        subroutine EditBug(iostat,message)
c
        implicit none
        integer iostat
        character message*(*)
c
c  Give an error message, and bugger off.
c-----------------------------------------------------------------------
        call bug('w',message)
        call bugno('f',iostat)
        end
c********1*********2*********3*********4*********5*********6*********7*c
        subroutine GetOpt(dorep,domult,doflag,doamp,dophas,dorefl,dozm,
     *          doscal,dup,doinv,doext,dohan,dobox)
c
        implicit none
        logical dorep,domult,doflag,doamp,dophas,dorefl,dozm,doscal
        logical dup, doinv, doext, dohan, dobox
c
c  Get the various processing options.
c
c-----------------------------------------------------------------------
        integer NOPTS
        parameter(NOPTS=13)
        character opts(NOPTS)*10
        logical present(NOPTS)
        data opts/'replace  ','multiply ','flag     ',
     *            'amplitude','phase    ','reflect  ',
     *            'zmean    ','scale    ','dup      ',
     *            'invert   ','extend   ','hanning  ',
     *            'boxcar   '/
c
        call options('options',opts,present,NOPTS)
c
        domult = present(2)
        dorep  = present(1)
        doflag = present(3)
        doamp  = present(4)
        dophas = present(5)
        dorefl = present(6)
        dozm   = present(7)
        doscal = present(8)
        dup    = present(9)
        doinv  = present(10)
        doext  = present(11)
        dohan  = present(12)
        dobox  = present(13)
        if(.not.(domult.or.dorep.or.doflag.or.doamp.or.dophas.or.
     *    dorefl.or.dozm.or.doscal.or.dup.or.doinv.or.doext.or.
     *    dohan.or.dobox))
     *    dorep = .true.
        if (doext.and.dohan.or.doext.and.dobox.or.dohan.and.dobox)
     *    call bug('f','Please specify a single option out of '//
     *      'extend, hanning and boxcar')
c
        end
