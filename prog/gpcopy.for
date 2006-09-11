c************************************************************************
	program gpcopy
	implicit none
c
c= GpCopy -- Copy or merge gains, bandpass and polarization correctsion.
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
c	The name of the output data-set. This is NOT created by GPCAL, but
c	rather the relevant items are copied to this data-set.
c@ mode
c	This determines how the calibration tables, etc, are ``copied''
c	to the output. The default is to ``copy''. Possible values are:
c	  create   Create the output, and copy the calibration tables to it.
c	  copy     Copy the calibration tables to the output, overwriting
c	           and previously existing calibration tables. This is the
c	           default.
c	  apply    Apply the input calibration tables to the output calibration
c	           tables (not yet implemented).
c	  merge    Merge the two calibration tables together. The calibration
c	           tables will usually not overlap in time or frequency (not
c		   yet implemented)
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
c
c  Bugs:
c    None?
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='GpCopy: version 17-Jan-94')
	logical dopol,docal,dopass,docopy
	integer iostat,tIn,tOut
	character vis*64,out*64,mode*8
c
c  Externals.
c
	logical hdprsnt
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input data-set must be given')
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','Output data-set must be given')
	call GetOpt(dopol,docal,dopass)
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
	  if(mode.eq.'merge'.and.dopass)then
	    call bug('f','Merging of polarization table unimplemented')
	  else if(mode.eq.'apply'.and.dopass)then
	    call bug('f','Applying of polarization table unimplemented')
	  else
	    call output('Copying leakage table')
	    call hdcopy(tIn,tOut,'leakage')
	  endif
        end if
c
	if(docal) docal = hdprsnt(tIn,'gains')
	if(docal)then
	  docal = .not.docopy.and.hdprsnt(tOut,'gains')
	  if(mode.eq.'merge'.and.docal)then
	    call bug('f','Merging of gain tables is not implemented')
	  else if(mode.eq.'apply'.and.docal)then
	    call bug('f','Applying of gain tables is not implemented')
	  else
	    call output('Copying gain table')
	    call hdcopy(tIn,tOut,'interval')
	    call hdcopy(tIn,tOut,'nsols')
	    call hdcopy(tIn,tOut,'ngains')
	    call hdcopy(tIn,tOut,'nfeeds')
	    call hdcopy(tIn,tOut,'ntau')
	    call hdcopy(tIn,tOut,'gains')
	    call hdcopy(tIn,tOut,'freq0')
	  endif
	endif
c
	if(dopass)then
	  if(hdprsnt(tIn,'bandpass'))then
	    dopass = .not.docopy.and.hdprsnt(tOut,'bandpass')
	    if(mode.eq.'merge'.and.dopass)then
	      call bug('f','Merging bandpasses is not implemented')
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
	    endif
	  else
	    if(hdprsnt(tIn,'cgains'))then
	      dopass = .not.docopy.and.hdprsnt(tOut,'cgains')
	      if(mode.eq.'merge'.and.dopass)then
		call bug('f','Merging cgains is not implemented')
	      else if(mode.eq.'apply'.and.dopass)then
		call bug('f','Applying cgains is not implemented')
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
		call bug('f','Merging wgains is not implemented')
	      else if(mode.eq.'apply'.and.dopass)then
		call bug('f','Applying wgains is not implemented')
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
c  Get the mode that this this is supposed to work in.
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
	subroutine GetOpt(dopol,docal,dopass)
c
	implicit none
	logical dopol,docal,dopass
c
c  Get extra processing options.
c
c  Output:
c    dopol	If true, copy polarization tables.
c    docal	If true, copy gain tables.
c    dopass	If true, copy bandpass tables.
c------------------------------------------------------------------------
	integer nopt
	parameter(nopt=3)
	logical present(nopt)
	character opts(nopt)*8
c
	data opts/'nopol   ','nocal   ','nopass   '/
c
	call options('options',opts,present,nopt)
	dopol = .not.present(1)
	docal = .not.present(2)
	dopass  = .not.present(3)
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
	integer MAXSPECT,MAXPASS
	parameter(MAXSPECT=16,MAXPASS=MAXSPECT*MAXCHAN)
c
	complex Gains1(MAXPASS),Gains2(MAXPASS)
	double precision freq1(2),freq2(2)
	integer nants,nfeeds,nants2,nfeeds2,ntau,nspect,nspect2
	integer item1,item2,nchan,off,nschan1,nschan2,iostat,i,j
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
	if(nspect.gt.maxspect)call bug('f','Too many spectral windows')
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
c
c  Close up shop
c
	call hdaccess(item1,iostat)
	if(iostat.eq.0)call hdaccess(item2,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	end
