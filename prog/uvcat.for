c************************************************************************
	program uvcat
	implicit none
c
c
c  History:
c    rjs  Dark-ages Original version.
c    rjs  27mar90   Major overhaul. Added uv selection, window selection,
c		    self-cal applying. Copied more variables.
c    rjs  11apr90   Better handling of wide data. nowide and nochan
c		    options.
c    pjt   2may90   maxdim.h now defines maxchan
c   mchw  16jul90   Fixed a bug counting channels in windit.
c   mchw  27jul90   Copied across all the Miriad variables.
c    rjs  16oct90   Checks that the data are consistently cross or
c		    autocorrelation data. Incorporated changes from
c		    mchw and bpw.
c    rjs  18oct90   Added the "on" variable to the list to be copied
c		    across.
c    rjs   1nov90   Added the ability to copy only those data that
c		    have some data that are not flagged.
c    mchw  9nov90   Merged rjs versions with 18oct and 1 nov changes.
c		    Added uv-variables "evector" "pbfwhm" "npol".
c    rjs  24jan91   Copied variables atten,comment,institut. Discarded
c		    variable obstype (now an item).
c    rjs  25jan91   Merged rjs and mchw versions.
c    rjs   5feb91   Major rework to include Stokes handling.
c    rjs  20feb91   Fixed bug which incorrectly "overrode" npol in some cases.
c    mchw 07mar91   Added passband option for planet observations.
c    mchw 19mar91   Added hanning option.
c    mchw 28mar91   Changes to handle processing options after window selection.
c    rjs  19apr91   Repaired options=nochan, which I apparently broke on
c		    20feb91.
c    rjs  20may91   Added xyphase to the list of variables to copy.
c    rjs  28jun91   Handle the flags for polarisation leakage correction.
c    mjs  04aug91   Replaced local maxants with maxdim.h value MAXANT
c    mchw 08oct91   Continuum subtraction option using linear fit to spectra.
c    mchw 29oct91   Split off special processing options into uvcal task.
c    rjs  31oct91   Deleted some redundant code.
c    rjs  13dec91   Deleted obstype processing (now done in uvio).
c    rjs  15jun92   Some use of "var" routines.
c    rjs  17aug92   New call sequence to "var" routines.
c    rjs   1oct92   Fiddles with copying of systemp, to handle datasets
c		    which do not have this variable.
c    mchw 10apr94   Put MAXWIN into maxdim.h
c    rjs  27feb95   Fix options=unflagged for files with multiple
c		    polarisations.
c    rjs  12oct95   Copy xyphase variable.
c    rjs  13jun96   Fix window selection.
c    rjs  23sep99   Fix window selection again!!
c    gmx  08apr04   Added xtsys and ytsys to the variables to be handled
c                   when removing channels.
c    jwr  16jun04   Fixed bug in calling uvVarUpd that relied on McCarthy
c                   evaluation of logical expressions (ifort does not!)
c    dnf  30jun06   increased size of output file name to 256 characters
c    mhw  02nov11   add l to uvflags to make sure line params get initialized
c    pjt  11may12   deal with bfmask(nspect) and bfmask(1)
c    pjt   6jun12   more checks when to write out in windupd(), although this
c                   takes care of bugzilla 1049, the root cause is still a
c                   mystery
c    mhw  07jun12   Handle ifchain correctly when selecting windows
c    mhw  07aug12   Merge atca and carma version (ifchain and bfmask)
c    mhw  15oct12   Make sure sfreq appears in output, even if uvprobvr
c                   claims it never updates
c
c  Bugs:
c
c= uvcat - Catenate and copy uv datasets; Apply gains file, Select windows.
c& rjs mchw
c: uv analysis
c+
c	UVCAT is a MIRIAD task which copies and catenates multiple MIRIAD
c	uv data sets. By default, UVCAT applies the gains file in copying the
c	data. The spectral windows copied to the output file can be selected.
c@ vis
c	The names of the input uv data sets. Multiple names can be given,
c	separated by commas. At least one name must be given.
c@ select
c	The normal uv selection commands. One unusual aspect of this is that
c	the "window" subcommand can be used to select which windows are
c	copied to the output file (normally the "window" only has an
c	effect for velocity line type). The default is to copy everything.
c@ stokes
c	If a value is given, uvcat converts the input into the required
c	polarizations before writing to the output. Default is to copy
c	across the polarizations present in the input files.
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreivated to the minimum
c	needed to avoid ambiguity. Possible options are:
c	   'nocal'       Do not apply the gains file. By default, UVCAT
c	                 applies the gains file in copying the data.
c	   'nopass'      Do not apply bandpass corrections. By default, UVCAT
c	                 applies bandpass corrections if possible.
c	   'nopol'       Do not apply polarization correction. By
c	                 default UVCAT corrects polarizations, if possible.
c	   'nowide'      Do not copy across wide-band channels.
c	   'nochannel'   Do not copy across spectral channels.
c	   'unflagged'   Copy only those records where there are some
c	                 unflagged visibilites.
c@ out
c	The name of the output uv data set. No default.
c--
c------------------------------------------------------------------------
        include 'maxdim.h'
	character version*(*)
	parameter(version='UvCat: version 15-oct-2012')
c
	integer nchan,vhand,lIn,lOut,i,j,nspect,nPol,Pol,SnPol,SPol
	integer nschan(MAXWIN),ischan(MAXWIN),ioff,nwdata,length
	integer lflags
	double precision preamble(4)
	complex data(maxchan),wdata(maxchan)
	logical flags(maxchan),wflags(maxchan)
	logical nocal,nopol,window,wins(MAXWIN)
	logical first,init,new,more,dopol,PolVary,donenpol
	logical nowide,nochan,dochan,dowide,docopy,doall,updated
	logical nopass
	character out*256,type*1,uvflags*8
c
c  Externals.
c
        logical uvVarUpd,uvDatPrb,uvDatOpn
c
	call output(version)
	call keyini
	call GetOpt(nocal,nopol,nopass,nowide,nochan,doall)
	lflags = 3
	uvflags(1:3) = 'dsl'
	if(.not.nocal)then
	  lflags = lflags + 1
	  uvflags(lflags:lflags) = 'c'
	endif
	if(.not.nopol)then
	  lflags = lflags + 1
	  uvflags(lflags:lflags) = 'e'
	endif
	if(.not.nopass)then
	  lflags = lflags + 1
	  uvflags(lflags:lflags) = 'f'
	endif
	call uvDatInp('vis',uvflags(1:lflags))
	call keya('out',out,' ')
	call keyfin
c
c  Check user inputs.
c
	if(out.eq.' ') call bug('f','Output file name is missing')
c
c  Open the output.
c
	call uvopen(lOut,out,'new')
c
c  Determine which windows have possibly been selected.
c
	do j=1,MAXWIN
	  wins(j) = uvDatPrb('window',dble(j))
	enddo
c
c  Other initialisation.
c
	window = .false.
	first = .true.
	init = .false.
	new = .true.
	SnPol = 0
	SPol = 0
	PolVary = .false.
c
c  Loop the loop. Open a file, process it, copy it, etc.
c
	more = uvDatOpn(lIn)
	dowhile(more)
	  if(new)then
	    call SetUp(lIn,nochan,nowide,dochan,dowide,dopol,vhand)
	    if(dowide.and..not.dochan)
     *	      call uvset(lOut,'data','wide',0,1.,1.,1.)
	    npol = 0
	    donenpol = .false.
	    new = .false.
	  endif
c
c  Copy the history the first time, and set the form of the output
c  correlations the first time we are to copy some.
c
	  if(first)then
	    call hdcopy(lIn,lOut,'history')
	    first = .false.
	  endif
	  if(.not.init.and.dochan)then
	    call uvprobvr(lIn,'corr',type,length,updated)
	    call uvset(lOut,'corr',type,0,0.,0.,0.)
	    init = .true.
	  endif
c
c  Read in the data.
c
	  call uvDatRd(preamble,data,flags,maxchan,nchan)
c
c  Case of end-of-file. Close the old file, and open the new.
c
	  if(nchan.eq.0)then
	    call uvDatCls
	    more = uvDatOpn(lIn)
	    new = .true.
c
c  Case of still more data. Copy across any variables that we want,
c  eliminate undesired spectra, write out the data.
c
	  else
	    if(npol.eq.0)then
	      call uvDatGti('npol',npol)
	      if(npol.le.0)call bug('f',
     *		'Could not determine number of polarizations present')
	      donenpol = .false.
	    endif
c
c  Update the window parameters if needed.
c
	    if(dochan) then
	      if (uvVarUpd(vhand)) call WindUpd(lIn,lOut,
     *			  MAXWIN,wins,nspect,nschan,ischan,window)
	    endif
c
c  Move the data around, if we are eliminating spectra.
c
	    ioff = 1
	    if(window)
     *	      call WindIt(data,flags,nspect,nschan,ischan,ioff,nchan)
c
c  Check if this data is wanted.
c
	    docopy = doall.or.donenpol
	    if(.not.docopy)then
	      do i=ioff,ioff+nchan-1
	        docopy = docopy .or. flags(i)
	      enddo
	    endif
c
c  Copy the variables we are interested in.
c
	    if(docopy)then
	      if(.not.donenpol)then
	        if(nPol.ne.SnPol)then
		  call uvputvri(lOut,'npol',nPol,1)
		  PolVary = SnPol.ne.0
		  SnPol = nPol
	        endif
		donenpol = .true.
	      endif
	      call uvDatGti('pol',Pol)
	      if(Pol.ne.SPol)then
		call uvputvri(lOut,'pol',Pol,1)
		SPol = Pol
	      endif
	      call VarCopy(lIn,lOut)
	      if(dowide.and.dochan)then
	        call uvDatWRd(wdata,wflags,maxchan,nwdata)
	        call uvwwrite(lOut,wdata,wflags,nwdata)
	      endif
	      call uvwrite(lOut,preamble,data(ioff),flags(ioff),nchan)
	    endif
	    npol = npol - 1
	  endif
	enddo
c
c  Write out the "npol" parameter, if it did not vary.
c
	if(.not.PolVary) call wrhdi(lOut,'npol',Snpol)
c
c  Finish up the history, and close up shop.
c
        call hisopen(lOut,'append')
        call hiswrite(lOut,'UVCAT: Miriad '//version)
	call hisinput(lOut,'UVCAT')
        call hisclose (lOut)
	call uvclose(lOut)
	end
c************************************************************************
	subroutine SetUp(lIn,nochan,nowide,dochan,dowide,dopol,vhand)
c
	implicit none
	logical dopol,nochan,nowide,dochan,dowide
	integer lIn,vhand
c
c  Input:
c    lIn	Handle of the uv dataset.
c    nochan	True if the user does not want "corr" data.
c    nowide	True if the user does not want "wcorr" data.
c  Output:
c    dochan	Copy "corr" data across.
c    dowide	Copy "wcorr" data across.
c    dopol	True if the file contains polarisation information.
c    vhand	Handle pointing to all the windowing variables.
c------------------------------------------------------------------------
	character type*1
	integer length,j
	logical updated
c
	integer nwin
	parameter(nwin=11)
	character windpar(nwin)*8
c
	data windpar/ 'ischan  ','nschan  ','nspect  ','restfreq',
     *	   'sdf     ','sfreq   ','systemp ','xtsys   ','ytsys   ',
     *     'bfmask  ','ifchain '/
c
c  Check if "wcorr" and "corr" are present, and determine which ones we
c  want to write out.
c
	call uvprobvr(lIn,'wcorr',type,length,updated)
	dowide = type.eq.'c'.and..not.nowide
	call uvprobvr(lIn,'corr',type,length,updated)
	dochan = (type.eq.'r'.or.type.eq.'j'.or.type.eq.'c')
     *		 .and..not.nochan
c
	if(.not.dochan.and..not.dowide)
     *	  call bug('f','No corr or wcorr data to copy')
c
c  Mark the variables according to what we need.
c
	call VarInit(lIn,' ')
c
	if(dochan)then
	  call uvvarini(lIn,vhand)
	  do j=1,nwin
	    call uvvarset(vhand,windpar(j))
	  enddo
	endif
c
	if(dowide)then
	  call VarWInit(lIn)
	  if(.not.dochan)call uvset(lIn,'data','wide',0,1.,1.,1.)
	endif
c
	call uvprobvr(lIn,'npol',type,length,updated)
	dopol = type.eq.'i'
c
c  Disable window-based selection, as that is done manually.
c
	call uvset(lIn,'selection','window',0,0.,0.,0.)
c
	end
c************************************************************************
	subroutine WindUpd(lIn,lOut,nwins,wins,nspectd,nschand,ischand,
     *							        window)
c
	implicit none
	integer nwins,nspectd,nschand(nwins),ischand(nwins),lIn,lOut
	logical wins(nwins),window
c
c  This updates uv variables that are affected if we remove channels.
c  These variables are:
c    nspect
c    nschan
c    ischan
c    sdf
c    sfreq
c    restfreq
c    systemp
c    (ifchain)
c    xtsys
c    ytsys
c    bfmask
c
c  It also returns a description used by a later routine to extract the
c  useful channels.
c
c  Input:
c    lIn	Handle of the input uv data file.
c    lOut	Handle of the output uv data file.
c    nwins	Size of the arrays.
c    wins	If wins(i) is true, window "i" is selected.
c  Output:
c    nspectd	Number of windows selected.
c    nschand	Number of channels in each window.
c    ischand	The offset of the first channel in each window.
c    window	This is false if all windows are selected.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer nschan(MAXWIN),ischan(MAXWIN),bfmask(MAXWIN),nants
	integer length,nspect,offset,nout,i,j,nsystemp,nxyph
	integer nxtsys,nytsys
	double precision sdf(MAXWIN),sfreq(MAXWIN),restfreq(MAXWIN)
	real systemp(MAXANT*MAXWIN),xyphase(MAXANT*MAXWIN)
        integer ifchain(MAXWIN)
	real xtsys(MAXANT*MAXWIN),ytsys(MAXANT*MAXWIN)
	character type*1
	logical unspect,unschan,uischan,usdf,usfreq,urest,usyst
	logical uxtsys,uytsys,ubfmask
	logical uxyph,uifch
	logical first,bfirst
c
	save first,bfirst
	data first/.TRUE./,bfirst/.TRUE./
c
c  Get the dimensioning info.
c
	call uvgetvri(lIn,'nants',nants,1)
	call uvprobvr(lIn,'nspect',type,length,unspect)
	call uvgetvri(lIn,'nspect',nspect,1)
	if(nspect.le.0)
     *	  call bug('f','Bad value for uv variable nspect')
	if(nspect.gt.nwins)
     *	  call bug('f','There are too many windows for me to handle')
c
c  Get all the goodies, noting whether they are being updated.
c
	call uvprobvr(lIn,'nschan',type,length,unschan)
	call uvgetvri(lIn,'nschan',nschan,nspect)
	call uvprobvr(lIn,'ischan',type,length,uischan)
	call uvgetvri(lIn,'ischan',ischan,nspect)
	call uvprobvr(lIn,'sdf',type,length,usdf)
	call uvgetvrd(lIn,'sdf',sdf,nspect)
	call uvprobvr(lIn,'sfreq',type,length,usfreq)
	call uvgetvrd(lIn,'sfreq',sfreq,nspect)
	call uvprobvr(lIn,'restfreq',type,length,urest)
	call uvgetvrd(lIn,'restfreq',restfreq,nspect)
        call uvprobvr(lIn,'ifchain',type,length,uifch)
        uifch = (uifch.or.first).and.type.eq.'i'.and.length.eq.nspect
        if (uifch) call uvgetvri(lIn,'ifchain',ifchain,nspect)
	call uvprobvr(lIn,'bfmask',type,length,ubfmask)
c
c  bfmask if a new CARMA variable (for general use). But
c  it is allowed to contain 1 element, but is forced to
c  be nspect, as is required by most downstream software
c  This construction here will fix that issue.
c
	if (ubfmask) then
	   if (length.eq.nspect) then
	      call uvgetvri(lIn,'bfmask',bfmask,nspect)
	   else if (length.eq.1) then
	      if (bfirst) then
		 call bug('i','Fixing up bfmask(nspect)')
		 bfirst = .FALSE.
	      endif
	      call uvgetvri(lIn,'bfmask',bfmask,1)
	      do i=2,nspect
		 bfmask(i) = bfmask(1)
	      enddo
	   else
	      call bug('f','bfmask is not nspect or 1 in size')
	   endif
	endif
c
c       System Temperature
c
	call uvprobvr(lIn,'systemp',type,nsystemp,usyst)
	usyst = usyst.and.type.eq.'r'.and.nsystemp.le.MAXANT*MAXWIN.and.
     *				nsystemp.gt.0
	if(usyst)call uvgetvrr(lIn,'systemp',systemp,nsystemp)
c
c       x-feed system temperature
c
	call uvprobvr(lIn,'xtsys',type,nxtsys,uxtsys)
	uxtsys = uxtsys.and.type.eq.'r'.and.nxtsys.le.MAXANT*MAXWIN.and.
     *				nxtsys.gt.0
	if(uxtsys)call uvgetvrr(lIn,'xtsys',xtsys,nxtsys)

c
c       y-feed system temperature
c
	call uvprobvr(lIn,'ytsys',type,nytsys,uytsys)
	uytsys = uytsys.and.type.eq.'r'.and.nytsys.le.MAXANT*MAXWIN.and.
     *				nytsys.gt.0
	if(uytsys)call uvgetvrr(lIn,'ytsys',ytsys,nytsys)

	call uvprobvr(lIn,'xyphase',type,nxyph,uxyph)
	uxyph = uxyph.and.type.eq.'r'.and.nxyph.le.MAXANT*MAXWIN.and.
     *				nxyph.gt.0
	if(uxyph)call uvgetvrr(lIn,'xyphase',xyphase,nxyph)
c
c  Trim them down to size.
c
	nout = 0
	offset = 1
	do i=1,nspect
	  if(wins(i))then
	    nout = nout + 1
	    nschand(nout) = nschan(i)
	    ischand(nout) = ischan(i)	    
	    nschan(nout) = nschan(i)
	    ischan(nout) = offset
	    offset = offset + nschan(i)
	    sdf(nout) = sdf(i)
	    sfreq(nout) = sfreq(i)
	    restfreq(nout) = restfreq(i)
            if (uifch) ifchain(nout) = ifchain(i)
	    bfmask(nout) = bfmask(i)
c
	    if(usyst.and.nsystemp.ge.nspect*nants)then
	      do j=1,nants
	        systemp((nout-1)*nants+j) = systemp((i-1)*nants+j)
	      enddo
	    endif

	    if(uxtsys.and.nxtsys.ge.nspect*nants)then
	      do j=1,nants
	        xtsys((nout-1)*nants+j) = xtsys((i-1)*nants+j)
	      enddo
	    endif

	    if(uytsys.and.nytsys.ge.nspect*nants)then
	      do j=1,nants
	        ytsys((nout-1)*nants+j) = ytsys((i-1)*nants+j)
	      enddo
	    endif
c
	    if(uxyph.and.nxyph.ge.nspect*nants)then
	      do j=1,nants
	        xyphase((nout-1)*nants+j) = xyphase((i-1)*nants+j)
	      enddo
	    endif
	  endif
	enddo
c
c  Write all the goodies out.
c
	if(nout.eq.0) call bug('f','No windows were selected')
	call uvputvri(lOut,'nspect',nout,1)
	call uvputvri(lOut,'nschan',nschan,nout)
	call uvputvri(lOut,'ischan',ischan,nout)
	if(usdf.or.first) call uvputvrd(lOut,'sdf',sdf,nout)
	if(usfreq.or.first) call uvputvrd(lOut,'sfreq',sfreq,nout)
	if(urest.or.first) call uvputvrd(lOut,'restfreq',restfreq,nout)
        if(uifch) call uvputvri(lOut,'ifchain',ifchain,nout)
	if(ubfmask) call uvputvri(lOut,'bfmask',bfmask,nout)
	if(nsystemp.ge.nspect*nants)nsystemp = nout*nants
	if(usyst)call uvputvrr(lOut,'systemp',systemp,nsystemp)
	if(nxtsys.ge.nspect*nants) nxtsys = nout*nants
	if(uxtsys) call uvputvrr(lOut,'xtsys',xtsys,nxtsys)
	if(nytsys.ge.nspect*nants) nytsys = nout*nants
	if(uytsys) call uvputvrr(lOut,'ytsys',ytsys,nytsys)
c-check?
	if(uxyph)call uvputvrr(lOut,'xyphase',xyphase,nxyph)
c
c  Determine the output parameters.
c
	window = nout.ne.nspect
	if(window)then
	  nspectd = 1
	  do i=2,nout
	    if(ischand(nspectd)+nschand(nspectd).eq.ischand(i))then
	      nschand(nspectd) = nschand(nspectd) + nschand(i)
	    else
	      nspectd = nspectd + 1
	      nschand(nspectd) = nschand(i)
	      ischand(nspectd) = ischand(i)
	    endif
	  enddo
	endif
        first = .false.
c
	end
c************************************************************************
	subroutine WindIt(data,flags,nspect,nschan,ischan,ioff,nchan)
c
	implicit none
	complex data(*)
	logical flags(*)
	integer nchan,nspect,ioff,nschan(nspect),ischan(nspect)
c
c  This extracts the data and the flags, for the desired windows, for a
c  particular visibility data record.
c
c  Input:
c    nspect	Number of windows selected.
c    nschan	Number of channels in each window.
c    ischan	The offset of the first channel in each window.
c  Input/Output:
c    data
c    flags
c  Output:
c    ioff	Offset of first channel in output data.
c    nchan	Number of output channels.
c------------------------------------------------------------------------
	integer i,j,l1,l2
c
c  Move the data around.
c
	ioff = ischan(1)
	nchan = nschan(1)
	do i=2,nspect
	  l1 = ischan(i)
	  l2 = ischan(i) + nschan(i) - 1
	  do j=l1,l2
	    data(ioff+nchan) = data(j)
	    flags(ioff+nchan) = flags(j)
	    nchan = nchan + 1
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(nocal,nopol,nopass,nowide,nochan,doall)
c
	implicit none
	logical nocal,nopol,nopass,nowide,nochan,doall
c
c  Determine extra processing options.
c
c  Output:
c    nocal	If true, do not apply selfcal corrections.
c    nopol	If true, do not apply polarisation corrections.
c    nopass	If true, do not apply bandpass corrections.
c    nowide	True if wide channels are not to be copied across.
c    nochan	True if spectral channels are not to be copied across.
c    doall	True if all data (not just unflagged) is to be copied.
c------------------------------------------------------------------------
	integer nopt
	parameter(nopt=6)
	character opts(nopt)*9
	logical present(nopt)
	data opts/'nocal    ','nowide   ','nochannel','unflagged',
     *		  'nopol    ','nopass   '/
	call options('options',opts,present,nopt)
	nocal = present(1)
	nowide = present(2)
	nochan = present(3)
	doall = .not.present(4)
	nopol  = present(5)
	nopass = present(6)
	end
