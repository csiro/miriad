c************************************************************************
	program gpcopy
	implicit none
c
c= GpCopy -- Copy or merge gains, bandpass and polarization correction.
c& rjs
c: calibration
c+
c	GpCopy is a MIRIAD task which copies or merges calibration corrections
c	(antenna gains, polarization leakages, frequency table, bandpass item)
c	from one data-set to another.
c@ vis
c	The name of the input data-set. This will normally be a visibility
c	data-set. No default.
c@ out
c	The name of the output data-set. This is NOT created by GPCOPY, but
c	rather the relevant items are copied to this data-set.
c@ mode
c	This determines how the calibration tables, etc, are ``copied''
c	to the output. The default is to ``copy''. Possible values are:
c	  create   Create the output, and copy the calibration tables to it.
c	  copy     Copy the calibration tables to the output, overwriting
c	           and previously existing calibration tables. This is the
c	           default.
c	  apply    Apply the input calibration tables to the output calibration
c	           tables. This is the sort of operation that you might do
c	           if you have two sets of calibration tables that you wish
c	           to "multiply" together (implemented for gains and bandpass
c	           only).
c	  merge    Merge the two calibration tables together. This is the
c	           sort of operation that you will want to perform if you
c	           have two sets of calibration tables which are disjoint in
c	           time (e.g. you used two secondary calibrators). The
c	           merge opertation will weave the two sets of tables together.
c	           (implemented for gain tables only).
c@ options
c	This gives extra processing options, which are used to suppress
c	the copying of certain items. Several options can be given,
c	separated by commands. Minimum match is used:
c	  nopol    Do not copy the items dealing with polarization
c	           calibration.
c	  nocal    Do not copy the items dealing with antenna gain
c	           calibration.
c	  nopass   Do not copy the items dealing with bandpass
c	           calibration (this includes the cgains and wgains tables).
c         relax    Relax the interpolation interval limits to 0.5 days. 
c                  Use this when copying gain tables created by selfcal
c                  or gpscal to other sources. 
c                  With mode=apply: simply adjust the existing output gains,
c                  rather than doing the default merge & multiply of solutions.
c                  
c--
c  History:
c    rjs  16jul91 Original version.
c    rjs  21jul91 Copied xyphases item as well.
c    nebk 25aug91 Inform user
c    nebk 16jan92 Mention XYPHASES item in help.
c    nebk 06apr92 Add options=noxy
c    rjs  04aug92 The xyphases array is now redundant. Included bandpass
c		  copying.
c    rjs  05nov92 Attempt to copy only those items that are present.
c    rjs  22mar93 Added the merging capability.
c    rjs  30mar93 Generalise the "merge" capability... at least start to.
c    rjs  24nov93 mode=create also copies the history file.
c    rjs  17jan93 Copy cgains and wgains.
c    rjs  24nov94 Implement merging of gain tables.
c    rjs   3dec94 Implement applying of gain tables.
c    mchw 04jan95 Doc change only.
c    rjs  17aug95 More messages.
c    rjs  10dec97 Change some fatal messages to warnings only, to
c		  prevent tables getting corrupted. Add check for
c		  apparently corrupt gain table.
c    rjs  19sep04 Copy across sensitivity model.
c    rjs  23jan07 Correct some logical errors and also copy "leakage2" table.
c    mhw  02sep09 Handle multiple bandpass solution intervals
c    mhw  11aug11 Copy gainsf and leakagef tables
c    mhw  13may13 Handle gainsf in 'merge' and 'apply' mode
c    mhw  12jun13 Fix apply mode, add relax option
c    mhw  20nov13 Change the way the relax option works
c    mhw  10feb15 Fix apply/relax mode
c  Bugs:
c    None?
c------------------------------------------------------------------------
	character version*72
	logical dopol,docal,dopass,docopy,relax
	integer iostat,tIn,tOut
	character vis*256,out*256,mode*8,line*64
	double precision interval
c
c  Externals.
c
	logical hdprsnt
        character*72 versan
c
	version = versan('gpcopy',
     *                   '$Revision: 1.13 $',
     *                   '$Date: 2015/10/29 01:31:39 $')
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input data-set must be given')
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','Output data-set must be given')
	call GetOpt(dopol,docal,dopass,relax)
	call GetMode(mode)
	docopy = mode.eq.'create'.or.mode.eq.'copy'
	call keyfin
c
c  Open the input and the output.
c
	call hopen(tIn,vis,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening input '//vis)
	  call bugno('f',iostat)
	endif
	if(mode.eq.'create')then
	  call hopen(tOut,out,'new',iostat)
	  call hdcopy(tIn,tOut,'history')
	else
	  call hopen(tOut,out,'old',iostat)
	endif
	if(iostat.ne.0)then
	  call bug('w','Error opening output '//out)
	  call bugno('f',iostat)
	endif
c
c  Copy across the relevant items.
c
	if(dopol) dopol = hdprsnt(tIn,'leakage')
	if(dopol)then
	  dopol = .not.docopy.and.hdprsnt(tOut,'leakage')
	  if(mode.eq.'merge'.and.dopol)then
	    call bug('w','Merging of polarization table unimplemented')
	  else if(mode.eq.'apply'.and.dopol)then
	    call bug('w','Applying of polarization table unimplemented')
	  else
	    call output('Copying leakage table')
	    call hdcopy(tIn,tOut,'leakage')
	    call hdcopy(tIn,tOut,'leakage2')
	    call hdcopy(tIn,tOut,'leakagef')
	  endif
        end if
c
	if(docal) docal = hdprsnt(tIn,'gains')
	if(docal)then
	  docal = .not.docopy.and.hdprsnt(tOut,'gains')
	  if(mode.eq.'merge'.and.docal)then
	    call output('Merging gain table')
	    call GnMerge(tIn,tOut,relax)
	  else if(mode.eq.'apply'.and.docal)then
	    call output('Applying gain table')
	    call GnApply(tIn,tOut,relax)
	  else
	    call output('Copying gain table')
	    if(hdprsnt(tIn,'interval'))then
	      call rdhdd(tIn,'interval',interval,0.d0)
              if (relax) interval=max(interval,0.5d0)
	      write(line,'(a,f7.2)')
     *		'Interpolation tolerance set to (minutes):',
     *		24*60*interval
	      call output(line)
	    endif
	    call hdcopy(tIn,tOut,'interval')
	    call hdcopy(tIn,tOut,'nsols')
	    call hdcopy(tIn,tOut,'ngains')
	    call hdcopy(tIn,tOut,'nfeeds')
	    call hdcopy(tIn,tOut,'ntau')
	    call hdcopy(tIn,tOut,'gains')
	    call hdcopy(tIn,tOut,'gainsf')
            call hdcopy(tIn,tOut,'nfbin')
	    call hdcopy(tIn,tOut,'freq0')
	    call hdcopy(tIn,tOut,'senmodel')
	  endif
	endif
c
	if(dopass)then
	  if(hdprsnt(tIn,'bandpass'))then
	    dopass = .not.docopy.and.hdprsnt(tOut,'bandpass')
	    if(mode.eq.'merge'.and.dopass)then
	      call bug('w','Merging bandpasses is not implemented')
	    else if(mode.eq.'apply'.and.dopass)then
	      call output('Applying input bandpass table to output')
	      call BpApply(tIn,tOut)
	    else
	      call output('Copying bandpass table')
	      call hdcopy(tIn,tOut,'ngains')
	      call hdcopy(tIn,tOut,'nfeeds')
	      call hdcopy(tIn,tOut,'ntau')
	      call hdcopy(tIn,tOut,'bandpass')
	      call hdcopy(tIn,tOut,'freqs')
	      call hdcopy(tIn,tOut,'nspect0')
	      call hdcopy(tIn,tOut,'nchan0')
              if (hdprsnt(tIn,'nbpsols'))
     *          call hdcopy(tIn,tOut,'nbpsols')
	    endif
	  else
	    if(hdprsnt(tIn,'cgains'))then
	      dopass = .not.docopy.and.hdprsnt(tOut,'cgains')
	      if(mode.eq.'merge'.and.dopass)then
		call bug('w','Merging cgains is not implemented')
	      else if(mode.eq.'apply'.and.dopass)then
		call bug('w','Applying cgains is not implemented')
	      else
		call output('Copying cgains table')
		call hdcopy(tIn,tOut,'cgains')
		call hdcopy(tIn,tOut,'ncbase')
		call hdcopy(tIn,tOut,'ncgains')
	      endif
	    endif
	    if(hdprsnt(tIn,'wgains'))then
	      dopass = .not.docopy.and.hdprsnt(tOut,'wgains')
	      if(mode.eq.'merge'.and.dopass)then
		call bug('w','Merging wgains is not implemented')
	      else if(mode.eq.'apply'.and.dopass)then
		call bug('w','Applying wgains is not implemented')
	      else
		call output('Copying wgains table')
		call hdcopy(tIn,tOut,'wgains')
		call hdcopy(tIn,tOut,'nwbase')
		call hdcopy(tIn,tOut,'nwgains')
	      endif
	    endif
	  endif
	endif
c
c  Write some history.
c
	call hisopen(tOut,'append')
	call hiswrite(tOut,'GPCOPY: Miriad '//version)
	call hisinput(tOut,'GPCOPY')
	call hisclose(tOut)
c
c  Close up now.
c
	call hclose(tIn)
	call hclose(tOut)
	end
c************************************************************************
	subroutine GetMode(mode)
c
	implicit none
	character mode*(*)
c
c  Get the mode that this is supposed to work in.
c
c  Output:
c    mode
c------------------------------------------------------------------------
	integer NMODES
	parameter(NMODES=4)
	integer nout
	character modes(NMODES)*8
	data modes/'create  ','copy    ','apply   ','merge   '/
c
	call keymatch('mode',NMODES,modes,1,mode,nout)
	if(nout.eq.0) mode = 'copy'
	end
c************************************************************************
	subroutine GetOpt(dopol,docal,dopass,relax)
c
	implicit none
	logical dopol,docal,dopass,relax
c
c  Get extra processing options.
c
c  Output:
c    dopol	If true, copy polarization tables.
c    docal	If true, copy gain tables.
c    dopass	If true, copy bandpass tables.
c    relax      If true, relax interpolation limits
c------------------------------------------------------------------------
	integer nopt
	parameter(nopt=4)
	logical present(nopt)
	character opts(nopt)*8
c
	data opts/'nopol   ','nocal   ','nopass  ','relax   '/
c
	call options('options',opts,present,nopt)
	dopol = .not.present(1)
	docal = .not.present(2)
	dopass  = .not.present(3)
        relax = present(4)
	if(.not.docal.and..not.dopol.and..not.dopass)
     *    call bug('f','No work to be performed')
	end
c************************************************************************
	subroutine BpApply(tIn,tOut)
c
	implicit none
	integer tIn,tOut
c
c  Merge the banpass tables from two data-sets. The frequencies in
c  the output frequency table will not change -- just the associated
c  gains will be updated.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXPASS
	parameter(MAXPASS=MAXWIN*MAXCHAN)
c
	complex Gains1(MAXPASS),Gains2(MAXPASS)
	double precision freq1(2),freq2(2)
	integer nants,nfeeds,nants2,nfeeds2,ntau,nspect,nspect2
	integer item1,item2,nchan,off,nschan1,nschan2,iostat,i,j,k
        integer nbpsols1,nbpsols2
c
c  Determine the number of antennas and feeds.
c
	call rdhdi(tIn,'ngains',nants,0)
	call rdhdi(tIn,'nfeeds',nfeeds,1)
	call rdhdi(tIn,'ntau',ntau,0)
	nants = nants / (nfeeds + ntau)
c
	call rdhdi(tOut,'ngains',nants2,0)
	call rdhdi(tOut,'nfeeds',nfeeds2,1)
	call rdhdi(tOut,'ntau',ntau,0)
	nants2 = nants2 / (nfeeds2 + ntau)
c
	if(nants.ne.nants2.or.nfeeds.ne.nfeeds2)
     *	  call bug('f','Incompatible number of antennas or feeds')
c
c  Compare the two frequency tables.
c
	call rdhdi(tIn,'nspect0',nspect,0)
	call rdhdi(tOut,'nspect0',nspect2,0)
	if(nspect.ne.nspect2)
     *	  call bug('f','Incompatible number of spectral windows')
	if(nspect.gt.MAXWIN)call bug('f','Too many spectral windows')
c
	call haccess(tIn,item1,'freqs','read',iostat)
	if(iostat.eq.0)call haccess(tOut,item2,'freqs','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error accessing the bandpass frequency table')
	  call bugno('f',iostat)
	endif
c
	nchan = 0
	off = 8
	do i=1,nspect
	  call hreadi(item1,nschan1,off,4,iostat)
	  if(iostat.eq.0)call hreadi(item2,nschan2,off,4,iostat)
	  off = off + 8
	  if(iostat.eq.0)call hreadd(item1,freq1,off,2*8,iostat)
	  if(iostat.eq.0)call hreadd(item2,freq2,off,2*8,iostat)
	  off = off + 2*8
	  if(iostat.ne.0)then
	    call bug('w','Error reading bandpass frequency table')
	    call bugno('f',iostat)
	  endif
	  if(nschan1.ne.nschan2.or.
     *	     abs(freq1(1)-freq2(1)).gt.0.01*abs(freq1(2)).or.
     *	     abs(freq1(2)-freq2(2)).gt.0.01*abs(freq1(2)))
     *	     call bug('f','Frequency setup is not the same')
	  nchan = nchan + nschan1
	enddo
	call hdaccess(item1,iostat)
	if(iostat.eq.0)call hdaccess(item2,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
        
        call rdhdi(tIn,'nbpsols',nbpsols1,0)
        call rdhdi(tOut,'nbpsols',nbpsols2,0)
        if (nbpsols1.ne.nbpsols2)
     *    call bug('f', 'Number of bandpass solution intervals differs')
c
c  The bandpass tables for the two sets are identical in all respects.
c  Apply the input bandpass to the output bandpass.
c
	call haccess(tIn,item1,'bandpass','read',iostat)
	if(iostat.eq.0)
     *	  call haccess(tOut,item2,'bandpass','append',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error accessing the bandpass table')
	  call bugno('f',iostat)
	endif
c
	off = 8
        do k=1,max(1,nbpsols1)
	  do j=1,nants*nfeeds
	    call hreadr(item1,Gains1,off,8*nchan,iostat)
	    if(iostat.eq.0)call hreadr(item2,Gains2,off,8*nchan,iostat)
	    if(iostat.ne.0)then
	      call bug('w','Error reading bandpass table')
	      call bugno('f',iostat)
	    endif
	    do i=1,nchan
	      Gains2(i) = Gains1(i) * Gains2(i)
	    enddo
	    call hwriter(item2,Gains2,off,8*nchan,iostat)
	    if(iostat.ne.0)then
	      call bug('w','Error writing bandpass table')
	      call bugno('f',iostat)
	    endif
	    off = off + 8*nchan
	  enddo
          if (nbpsols1.gt.0) then
            off = off + 8
          endif
        enddo
c
c  Close up shop
c
	call hdaccess(item1,iostat)
	if(iostat.eq.0)call hdaccess(item2,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	end
c************************************************************************
	subroutine GnMerge(tIn,tOut,relax)
c
	implicit none
	integer tIn,tOut
        logical relax
c
c  Merge two gain tables together. The tables must be the same
c  in terms of antennas, feeds and ntau. They must also not
c  overlap in time.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
c
	integer ngains,nfeeds,ntau,nsols1,nsols2,nfbin,maxtimes,maxgains
	double precision int1,int2,freq(maxfbin)
	integer pGain1,pGain2,pGainO,pTim1,pTim2,pTimO
c
c  Get info about the two gain tables.
c
	call GnCheck(tIn,tOut,nsols1,nsols2,ngains,nfeeds,ntau,
     *	  int1,int2,nfbin)
c
c  Allocate memory.
c
        maxtimes = nsols1+nsols2
        maxgains = maxtimes*ngains*(nfbin+1)
	call memAlloc(pGain1,maxgains,'c')
	call memAlloc(pTim1,maxtimes,'d')
	call memAlloc(pGain2,maxgains,'c')
	call memAlloc(pTim2,maxtimes,'d')
	call memAlloc(pGainO,maxgains,'c')
	call memAlloc(pTimO,maxtimes,'d')
c
c  Load the two tables.
c
        call uvGnRead(tIn,memc(pGain1),memd(pTim1),freq,ngains,nfeeds,
     *    ntau,nsols1,nfbin,maxgains,maxtimes,maxfbin)    
        call uvGnRead(tOut,memc(pGain2),memd(pTim2),freq,ngains,nfeeds,
     *    ntau,nsols2,nfbin,maxgains,maxtimes,maxfbin)    
c
c  Now merge them.
c
	call GnMerge1(ngains,nfbin,
     *    memc(pGain1),memd(pTim1),nsols1,
     *	  memc(pGain2),memd(pTim2),nsols2, 
     *    memc(pGainO),memd(pTimO),maxtimes)
c
        call uvGnWrit(tOut,memc(pGainO),memd(pTimO),freq,ngains,
     *    maxtimes,nfbin,maxgains,maxtimes,maxfbin,.true.)
c
c  Release allocated memory.
c
	call memFree(pGain1,maxgains,'c')
	call memFree(pTim1,maxtimes,'d')
	call memFree(pGain2,maxgains,'c')
	call memFree(pTim2,maxtimes,'d')
	call memFree(pGainO,maxgains,'c')
	call memFree(pTimO,maxtimes,'d')
c
c  Make the interval the larger of the individual intervals.
c
        int1 = max(int1, int2)
        if (relax) int1 = max(int1, 0.5d0)
	if(int1.gt.int2)call wrhdd(tOut,'interval',int1)
c
c  Assume that freq0 (if present) is the same for both,
c
	continue
c
c  Set the new number of solution intervals.
c
	call wrhdi(tOut,'nsols',nsols1+nsols2)
c
	end
c************************************************************************
	subroutine GnApply(tIn,tOut,relax)
c
	implicit none
	integer tIn,tOut
        logical relax
c
c  Apply one gains table to a second.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
c
	integer ngains,nfeeds,ntau,nsols1,nsols2,nsols,nfbin,maxtimes
        integer maxgains
	double precision int1,int2,freq(maxfbin)
	integer pGain1,pGain2,pGain,pTim1,pTim2,pTim
c
c  Get info about the two gain tables.
c
	call GnCheck(tIn,tOut,nsols1,nsols2,ngains,nfeeds,ntau,
     *	  int1,int2,nfbin)
c
c  Allocate memory.
c
        maxtimes = nsols1*3+nsols2*3+2
        maxgains = maxtimes*ngains*(nfbin+1)
	call memAlloc(pGain1,maxgains,'c')
	call memAlloc(pTim1,maxtimes,'d')
	call memAlloc(pGain2,maxgains,'c')
	call memAlloc(pTim2,maxtimes,'d')
	call memAlloc(pGain,maxgains,'c')
	call memAlloc(pTim,maxtimes,'d')
c
c  Load the two tables.
c
        call uvGnRead(tIn,memc(pGain1),memd(pTim1),freq,ngains,nfeeds,
     *    ntau,nsols1,nfbin,maxgains,maxtimes,maxfbin)    
        call uvGnRead(tOut,memc(pGain2),memd(pTim2),freq,ngains,nfeeds,
     *    ntau,nsols2,nfbin,maxgains,maxtimes,maxfbin)    
c
c  Now apply them.
c
        if (relax) then
          int1 = max(0.5d0,int1)
          call GnApply1(ngains,nfeeds,ntau,nfbin,
     *	  memc(pGain1),memd(pTim1),nsols1,int1,
     *	  memc(pGain2),memd(pTim2),nsols2,
     *	  memc(pGain),memd(pTim),nsols)
        else
          call GnApply2(ngains,nfeeds,ntau,nfbin,maxtimes,maxgains,
     *	  memc(pGain1),memd(pTim1),nsols1,int1,
     *	  memc(pGain2),memd(pTim2),nsols2,int2,
     *	  memc(pGain),memd(pTim),nsols)
        endif
c
        if (nsols.gt.0) then
          call uvGnWrit(tOut,memc(pGain),memd(pTim),freq,ngains,
     *    nsols,nfbin,maxgains,maxtimes,maxfbin,.true.)
c
c  Make the interval the larger of the individual intervals.
c
	  call wrhdd(tOut,'interval',max(int1,int2))
c
c  Set the new number of solution intervals.
c
	  call wrhdi(tOut,'nsols',nsols)
        else
          call bug('w','Unable to apply gain table')
        endif
c  
c  Release allocated memory.
c
	call memFree(pGain1,maxgains,'c')
	call memFree(pTim1,maxtimes,'d')
	call memFree(pGain2,maxgains,'c')
	call memFree(pTim2,maxtimes,'d')
	call memFree(pGain,maxgains,'c')
	call memFree(pTim,maxtimes,'d')
c
	end
c************************************************************************
	subroutine GnCheck(tIn,tOut,nsols1,nsols2,ngains,nfeeds,ntau,
     *	  int1,int2, nfbin)
c
	implicit none
	integer tIn,tOut,nsols1,nsols2,ngains,nfeeds,ntau,nfbin
	double precision int1,int2
c
c  Get the description of the gain table.
c------------------------------------------------------------------------
	integer nants1,nants2,ntau2,nfeeds2,ngains2,nfbin1
c
c  Determine the number of solution intervals.
c
	call rdhdi(tIn,'nsols',nsols1,0)
	call rdhdi(tOut,'nsols',nsols2,0)
	if(nsols1.le.0.or.nsols2.le.0)
     *    call bug('f','Could not determine number of gain intervals')
c
c  Get the number of antennas and feeds and determine if tau is present.
c  Check that they are consistent.
c
	call rdhdi(tIn,'ngains',ngains,0)
	call rdhdi(tIn,'nfeeds',nfeeds,1)
	call rdhdi(tIn,'ntau',  ntau,  0)
	nants1 = ngains / (ntau + nfeeds)
c
	call rdhdi(tOut,'ngains',ngains2,0)
	call rdhdi(tOut,'nfeeds',nfeeds2,1)
	call rdhdi(tOut,'ntau',  ntau2,  0)
	nants2 = ngains2 / (ntau2 + nfeeds2)
c
	if(nants1.ne.nants2)
     *	  call bug('f','The no. antennas in the two gain tables differ')
	if(nfeeds.ne.nfeeds2)
     *	  call bug('f','The no. feeds in the two gain tables differ')
	if(ntau.ne.ntau2)
     *	  call bug('f','The gain tables do not both have delay values')
c
	call rdhdd(tIn, 'interval',int1,0.d0)
	call rdhdd(tOut,'interval',int2,0.d0)
c        
        call rdhdi(tIn,'nfbin',nfbin,0)
        call rdhdi(tOut,'nfbin',nfbin1,0)
        if (nfbin.ne.nfbin1) call bug('f',
     *   'The number of frequency bins in the two gain tables differs')
c
	end
c************************************************************************
	subroutine GnMerge1(ngains,nfbin,Gains1,Times1,nsols1,
     *	                    Gains2,Times2,nsols2,GainsO,TimesO,nsols)
c
	implicit none
	integer ngains,nfbin,nsols1,nsols2,nsols
	double precision Times1(nsols1),Times2(nsols2),TimesO(nsols)
	complex Gains1(ngains,nsols1,0:nfbin)
        complex Gains2(ngains,nsols2,0:nfbin)
        complex GainsO(ngains,nsols, 0:nfbin)
c
c  Merge and write two gain tables.
c------------------------------------------------------------------------
	integer i1,i2,i,j,k
	logical do1,do2
	i1 = 0
	i2 = 0
        i = 0
	dowhile(i1.lt.nsols1.or.i2.lt.nsols2)
c
c  Determine whether we want to write from table 1 or 2.
c
	  do1 = i1.lt.nsols1
	  do2 = i2.lt.nsols2
	  if(do1.and.do2)then
	    do1 = Times1(i1+1).lt.Times2(i2+1)
	    do2 = .not.do1
	  endif
c
c  Copy the appropriate gains.
c
	  if(do1)then
	    i1 = i1 + 1
            i = i + 1
	    TimesO(i) = Times1(i1)
            do k=0,nfbin
              do j=1,ngains
                GainsO(j,i,k) = Gains1(j,i1,k)
              enddo
            enddo
	  endif
	  if(do2)then
	    i2 = i2 + 1
            i = i + 1
            TimesO(i) = Times2(i2)
            do k=0,nfbin
              do j=1,ngains
                GainsO(j,i,k) = Gains2(j,i2,k)
              enddo
            enddo
	  endif
	enddo
        if (i.ne.nsols) call bug('f','Inconsistent number of solutions')
c
	end
c************************************************************************
	subroutine GnApply1(ngains,nfeeds,ntau,nfbin,
     *	  Gains1,Times1,nsols1,int1,
     *    Gains2,Times2,nsols2,
     *    Gains,Times,nsols)
c
	implicit none
	integer ngains,nfeeds,ntau,nfbin,nsols1,nsols2
        integer nsols
	double precision Times1(nsols1),Times2(nsols2),Times(nsols1)
        double precision int1
	complex Gains1(ngains,nsols1,0:nfbin)
        complex Gains2(ngains,nsols2,0:nfbin)
        complex Gains(ngains,nsols2,0:nfbin)
c
c  Apply gain table 1 to table 2 in 'relaxed' mode.
c  This just evaluates table1 gains at the times of table2 gains and
c  multiplies the gains.
c  Input:
c    nearly everything
c  Output:
c    nsols	Number of gain solutions.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer NMAX
	double precision tol
	parameter(NMAX=3*MAXANT)
	parameter(tol=0.5d0/(24.d0*3600.d0))
	integer i,j,i1,j1,indx1,k,n
	double precision t,ti1,tj1
        logical zero
c	
c  Check.
c
	if(ngains.gt.NMAX)call bug('f','Too many gains for me!')
c
c  Cleanup table1 - remove slots with all zero gains
c
        i=1
        n=nsols1
        do while (i.le.n)
          zero=.true.
          j=1
          do while (zero.and.j.le.ngains)
            zero=abs(gains1(j,i,0)).eq.0.
            j=j+1
          enddo
          if (zero) then
            n=n-1
            do i1=i,n
              times1(i1)=times1(i1+1)
              do k=0,nfbin
                do j=1,ngains
                  gains1(j,i1,k)=gains1(j,i1+1,k)
                enddo
              enddo
            enddo
          else
            i=i+1
          endif
        enddo
        nsols1=n
c
        indx1 = 1
	do i=1,nsols2
          t = Times2(i)
	  Times(i) = t
	  call GnGet(t,nsols1,Times1,int1,indx1,i1,j1,ti1,tj1)
          do k=0,nfbin
            do j=1,ngains
              Gains(j,i,k) = Gains2(j,i,k)
            enddo
	    call GnInterp1(Gains(1,i,k),t,ngains,nfeeds,ntau,
     *                     ti1,Gains1(1,i1,k),tj1,Gains1(1,j1,k))
          enddo
	enddo

	nsols = nsols2
c
	end
c************************************************************************
	subroutine GnApply2(ngains,nfeeds,ntau,nfbin,maxsols,maxgains,
     *	  Gains1,Times1,nsols1,int1,
     *    Gains2,Times2,nsols2,int2,
     *    Gains,Times,nsols)
c
	implicit none
	integer ngains,nfeeds,ntau,nfbin,maxsols,maxgains,nsols1,nsols2
        integer nsols
	double precision Times1(nsols1),Times2(nsols2),Times(maxsols)
        double precision int1,int2
	complex Gains1(ngains,nsols1,0:nfbin)
        complex Gains2(ngains,nsols2,0:nfbin)
        complex Gains(maxgains)
c
c  Merge and apply two gain tables.
c  Input:
c    nearly everything
c  Output:
c    nsols	Number of gain solutions.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer NMAX
	double precision tol
	parameter(NMAX=3*MAXANT)
	parameter(tol=0.5d0/(24.d0*3600.d0))
	integer slot,offset,i,i1,i2,j1,j2,indx1,indx2,n,k
	double precision t,ta,tb,ti1,ti2,tj1,tj2,tend
	logical pre,post
c	
c  Check.
c
	if(ngains.gt.NMAX)call bug('f','Too many gains for me!')
c
	offset = 0
        n=nfbin
        if (n.le.1) n=0
        do k=0,n
          slot = 0
c
	  indx1 = 1
	  indx2 = 1
	  t = max(times1(1)-int1,times2(1)-int2) - tol
	  tend = min(times1(nsols1)+int1,times2(nsols2)+int2)
c
	  dowhile(t.le.tend)
	    call GnGet(t,nsols1,times1,int1,indx1,i1,j1,ti1,tj1)
	    call GnGet(t,nsols2,times2,int2,indx2,i2,j2,ti2,tj2)
	    ta = max(ti1,ti2)
	    tb = min(tj1,tj2)
c
c  Do we have a good gain somewhere?
c
	    if((i1.ne.0.or.j1.ne.0).and.(i2.ne.0.or.j2.ne.0))then
c
c  Determine if we need to write blanked records before and
c  after.
c
	      if(i1.eq.0.and.ti1.lt.ti2)i1 = j1
	      if(j1.eq.0.and.tj1.gt.tj2)j1 = i1
	      if(i2.eq.0.and.ti2.lt.ti1)i2 = j2
	      if(j2.eq.0.and.tj2.gt.tj1)j2 = i2
c
	      pre  = i1.eq.0.or.i2.eq.0
	      post = j1.eq.0.or.j2.eq.0
c
	      if(i1.eq.0) i1 = j1
	      if(j1.eq.0) j1 = i1
	      if(i2.eq.0) i2 = j2
	      if(j2.eq.0) j2 = i2
c
	      if(pre) then
                if (offset+ngains.gt.maxgains) 
     *            call bug('f','Buffer overflow in GnApply1')
                slot=slot+1
	        Times(slot)=ta-tol
                do i=1,ngains
                  Gains(offset+i)=0
                enddo
                offset=offset+ngains
              endif
c
c  Interpolate the gain at the start of the interval.
c
              if (offset+ngains.gt.maxgains) 
     *          call bug('f','Buffer overflow in GnApply1')
              slot = slot + 1
              Times(slot) = ta
	      call GnInterp(Gains(offset+1),ta,ngains,nfeeds,ntau,
     *                      ti1,Gains1(1,i1,k),tj1,Gains1(1,j1,k),
     *                      ti2,Gains2(1,i2,k),tj2,Gains2(1,j2,k))
              offset = offset + ngains
c
c  Interpolate a gain at the end of the interval, if needed.
c
	      if(post)then
                if (offset+2*ngains.gt.maxgains) 
     *            call bug('f','Buffer overflow in GnApply1')
                slot = slot + 1
                Times(slot) = tb
	        call GnInterp(Gains(offset+1),tb,ngains,nfeeds,ntau,
     *                        ti1,Gains1(1,i1,k),tj1,Gains1(1,j1,k),
     *                        ti2,Gains2(1,i2,k),tj2,Gains2(1,j2,k))
                offset = offset + ngains

                slot=slot+1
	        Times(slot)=tb+tol
                do i=1,ngains
                  Gains(offset+i)=0
                enddo
                offset=offset+ngains
              endif
	    endif
	    t = tb
	  enddo
        enddo
c
	nsols = slot
c
	end
c************************************************************************
	subroutine GnGet(t,nsols,times,interval,indx,i,j,ti,tj)
c
	implicit none
	integer nsols,indx,i,j
	double precision interval,t,ti,tj,times(nsols)
c
c  Get the two gain solutions which bound this time.
c------------------------------------------------------------------------
	double precision tol
	parameter(tol=0.5/(24.d0*3600.d0))
c
c  Handle the situation that we are beyond the edge of the gain table.
c
	if(t.lt.times(1)-interval)then
	  i = 0
	  j = 0
	  ti = t
	  tj = times(1) - interval
	else if(t.lt.times(1))then
	  i = 0
	  ti = times(1) - interval
	  j = 1
	  tj = times(1)
	  indx = 1
	else if(t.ge.times(nsols)+interval)then
	  i = 0
	  j = 0
	  ti = times(nsols) + interval
	  tj = times(nsols) + 10*(t-times(nsols))
	  indx = nsols + 1
	else if(t.ge.times(nsols))then
	  i = nsols
	  ti = times(nsols)
	  j = 0
	  tj = times(nsols) + interval + tol
	  indx = nsols
c
c  The case that we are in the middle of the gain table.
c
	else
	  dowhile(t.ge.times(indx+1))
	    indx = indx + 1
	  enddo
c
	  if(abs(t-times(indx)).lt.interval+tol)then
	    ti = times(indx)
	    i = indx
	  else
	    ti = times(indx) + interval
	    i = 0
	  endif
	  if(abs(t-times(indx+1)).le.interval+tol)then
	    tj = times(indx+1)
	    j = indx + 1
	  else
	    tj = times(indx+1) - interval
	    j = 0
	  endif
	endif
c
	end
c************************************************************************
	subroutine GnInterp(GainsO,T,ngains,nfeeds,ntau,
     *	  ti1,Gains1i,tj1,Gains1j,ti2,Gains2i,tj2,Gains2j)
c
	implicit none
	integer ngains,nfeeds,ntau
	double precision T,ti1,tj1,ti2,tj2
	complex Gains1i(ngains),Gains1j(ngains)
	complex Gains2i(ngains),Gains2j(ngains),GainsO(ngains)
c
c  Interpolate between two gain solutions.
c------------------------------------------------------------------------
	integer i
	complex g1,g2
	real epsi1,epsi2,mag
	logical dotau,bad1i,bad1j,bad2i,bad2j
c
	epsi1 = (tj1-t)/(tj1-ti1)
	epsi2 = (tj2-t)/(tj2-ti2)
c
	do i=1,ngains
	  dotau = ntau.gt.0.and.mod(i-nfeeds,nfeeds+ntau).eq.1
	  if(dotau)then
	    GainsO(i) = Gains1j(i) - epsi1*(Gains1j(i) - Gains1i(i)) +
     *		       Gains2j(i) - epsi2*(Gains2j(i) - Gains2i(i))
	  else
	    bad1j = abs(real(Gains1j(i)))+abs(aimag(Gains1j(i))).eq.0
	    bad1i = abs(real(Gains1i(i)))+abs(aimag(Gains1i(i))).eq.0
	    bad2j = abs(real(Gains2j(i)))+abs(aimag(Gains2j(i))).eq.0
	    bad2i = abs(real(Gains2i(i)))+abs(aimag(Gains2i(i))).eq.0
	    if((bad1j.and.bad1i).or.(bad2j.and.bad2i))then
	      GainsO(i) = 0
	    else
	      if(bad1j)then
		g1 = Gains1i(i)
	      else if(bad1i)then
		g1 = Gains1j(i)
	      else
		g1 = Gains1i(i)/Gains1j(i)
		mag = abs(g1)
		g1 = Gains1j(i) * (1 + (mag-1)*epsi1) * (g1/mag)**epsi1
	      endif
	      if(bad2j)then
		g2 = Gains2i(i)
	      else if(bad2i)then
		g2 = Gains2j(i)
	      else
		g2 = Gains2i(i)/Gains2j(i)
		mag = abs(g2)
		g2 = Gains2j(i) * (1 + (mag-1)*epsi2) * (g2/mag)**epsi2
	      endif
	      GainsO(i) = g1*g2
	    endif
	  endif
	enddo
c
	end
c************************************************************************
	subroutine GnInterp1(Gains,t,ngains,nfeeds,ntau,
     *	                     ti1,Gains1i,tj1,Gains1j)
c
	implicit none
	integer ngains,nfeeds,ntau
	double precision t,ti1,tj1
	complex Gains(ngains)
	complex Gains1i(ngains),Gains1j(ngains)
c
c  Interpolate first gain to second gain
c------------------------------------------------------------------------
	integer i
	complex g,g1
	real epsi,mag
	logical dotau,bad,bad1i,bad1j
c
	epsi = (tj1-t)/(tj1-ti1)
c
	do i=1,ngains
	  dotau = ntau.gt.0.and.mod(i-nfeeds,nfeeds+ntau).eq.1
	  if(dotau)then
	    Gains(i) = Gains(i) +
     *		       Gains1j(i) - epsi*(Gains1j(i) - Gains1i(i))
	  else
	    bad = abs(real(Gains(i)))+abs(aimag(Gains(i))).eq.0
	    bad1j = abs(real(Gains1j(i)))+abs(aimag(Gains1j(i))).eq.0
	    bad1i = abs(real(Gains1i(i)))+abs(aimag(Gains1i(i))).eq.0
	    if(bad.or.(bad1i.and.bad1j))then
	      Gains(i) = 0
	    else
              g = Gains(i)
	      if(bad1j)then
		g1 = Gains1i(i)
	      else if(bad1i)then
		g1 = Gains1j(i)
	      else
		g1 = Gains1i(i)/Gains1j(i)
		mag = abs(g1)
		g1 = Gains1j(i) * (1 + (mag-1)*epsi) * (g1/mag)**epsi
	      endif
	      Gains(i) = g*g1
	    endif
	  endif
	enddo
c
	end
c************************************************************************
