c************************************************************************
	program uvsplit
c
	implicit none
c
c= uvsplit - Break a uv data-set into single source, single-band files.
c& rjs
c: uv analysis
c+
c	UVSPLIT breaks an input data-set into a number of output
c	data-sets, each of which contains a single ``type'' of data.
c	Generally this means single source and single frequency
c	band, though this is controlled by some options.
c
c	UVSPLIT chooses its own names for the output data-sets. These
c	are in the form
c	    sourcename.frequency
c	Where ``frequency'' is the center frequency of the band, in MHz,
c	rounded to the nearest MHz.
c@ vis
c	The name if an input uv data-set. No default.
c@ select
c	The normal uv selection commands. The default is copy everything.
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreivated to the minimum
c	needed to avoid ambiguity.
c	  nocopy    By default, uvsplit copies any calibration tables
c	            present in the input to the output. The nocopy
c	            option suppresses this.
c	  mosaic    Typically this option is used when splitting datasets
c	            observed in mosaic mode. UVSPLIT attempts to place all
c	            pointing centres of the source of interest into the one
c	            output dataset. This requires that the field names of
c	            the different pointings be composed of two parts,
c	            separated by an underscore, viz
c	                  a_b
c	            Where "a" is common to all field names (typically
c	            a source) and "b" is a field-specific name (typically
c	            a field number). For example field 123 of a mosaic
c	            experiment of the LMC might be called "lmc_123"
c	  clobber   If a dataset exists with the same name as one that
c	            uvsplit would create, then delete that dataset before
c	            creating uvsplit's output.
c	The following three options determine which data-set characteristics
c	result in UVSPLIT generating different output data-sets.
c	  nosource  Do not produce new data-sets based on source name. That
c	            is each output data-set can contain multiple sources.
c	            The default is for each output to contain only a single
c	            source.
c	  nofreq    Do not produce new output data-sets based on a switch
c	            in frequency. That is each output data-set can contain
c	            frequency switches.
c	  nowindow  Do not generate a separate output data-set for each
c	            spectral window. The default is to create a new
c	            output for each spectral window.
c@ maxwidth
c        The maximum bandwidth (in GHz) for each output frequency band.
c        Default is no subdivision of input bands. The maxwidth limit
c        is only applied when splitting by frequency.
c--
c  History:
c    rjs  13oct93 Original version.
c    rjs  29aug94 W-axis change.
c    rjs   6sep94 Use MAXWIN in maxdim.h. Better treatment of xyphase.
c    rjs  25jan95 Added options=mosaic.
c    rjs  21feb95 Get select=win to work.
c    rjs  22sep95 Re-added support for pulsar binning.
c    rjs  05oct95 Handle xyphase and systemp being temporarily
c		  missing.
c    rjs  29may96 Added nbin to uvvariables.
c    rjs  05aug96 Increased maxfiles.
c    rjs  23jul97 Added pbtype.
c    rjs  16aug04 Added various variables to the list to be copied across.
c    rjs  19sep04 Copy across sensitivity model and more variables.
c    rjs  28jan05 Added clobber option.
c    mhw  19may08 Added maxwidth parameter
c    mhw  29sep09 Fix freq axis mislabeling bug
c    mhw  14oct09 Separate out identical freqs on different IFs
c  Bugs:
c   the full xtsys and ytsys variables are passed to split files,
c   but for the systemp variable only the appropriate data (if) is copied
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXSELS
	parameter(MAXSELS=256)
	character version*(*)
	parameter(version='UvSplit: version 1.0 19-May-08')
c
	character vis*64,dtype*1
	integer tvis
	real sels(MAXSELS),maxwidth
	integer length,i
	logical dosource,dofreq,dowin,updated,dowide,docomp,docopy
	logical mosaic,clobber
	logical more,first,winsel,selwins(MAXWIN)
c
c  Externals.
c
	logical SelProbe
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(dosource,dofreq,dowin,docopy,mosaic,clobber)
	call keyf('vis',vis,' ')
        call keyr('maxwidth',maxwidth,0.0)
	call SelInput('select',sels,MAXSELS)
	call keyfin
c
	if(vis.eq.' ')call bug('f','A visibility input must be given')
	if(.not.(dosource.or.dofreq.or.dowin))
     *	  call bug('f','The output would contain all the input!')
c
c  This program cannot tolerate polarisation, visibility or increment
c  selection (for obscure reasons to do with polarisation counting).
c
	if(SelProbe(sels,'increment?',0.d0))
     *	  call bug('f','UvSplit does not support select=inc')
	if(SelProbe(sels,'visibility?',0.d0))
     *	  call bug('f','UvSplit does not support select=vis')
	if(SelProbe(sels,'polarization?',0.d0))
     *	  call bug('f','UvSplit does not support select=pol')
	winsel = SelProbe(sels,'window?',0.d0)
	if(winsel.and..not.dowin)call bug('f',
     *	  'UvSplit does not support select=win with options=nowin')
c
c  Determine the selected windows.
c
	do i=1,MAXWIN
	  if(winsel)then
	    selwins(i) = SelProbe(sels,'window',dble(i))
	  else
	    selwins(i) = .true.
	  endif
	enddo
c
c  Loop: read the input file as many times as needed, creating MAXOPEN output
c  files at a time
c
	first = .true.
	more = .true.
	dowhile(more)
c
c  Open the input, and determine some things about it.
c
	  call uvopen(tVis,vis,'old')
	  call uvset(tVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	  call uvset(tVis,'selection','window',0,0.,0.,0.)
	  call SelApply(tVis,sels,.true.)
	  if(first)then
	    call uvprobvr(tVis,'corr',dtype,length,updated)
	    dowide = dtype.eq.' '
	    docomp = dtype.eq.'j'
	    call FileSup(tVis,dowide,docomp,dowin,
     *				selwins,MAXWIN,version)
	  endif
c
c  Read through the file.
c
	  call Process(tVis,dosource,dofreq,dowin,dowide,mosaic,
     *      maxwidth,clobber)
c
	  first = .false.
	  call FileFin(docopy,more)
	  call uvclose(tVis)
	enddo
c
	end
c************************************************************************
	subroutine Process(tVis,dosource,dofreq,dowin,dowide,mosaic,
     *						maxwidth,clobber)
c
	implicit none
	integer tVis
	logical dosource,dofreq,dowin,dowide,mosaic,clobber
        real maxwidth
c
c  Do a pass through the data file.
c
c  Input:
c    tVis
c    dosource
c    dofreq
c    dowin
c    dowide
c    mosaic
c    maxwidth
c    clobber
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer MAXINDX
	parameter(MAXINDX=99)
c
	complex data(MAXCHAN)
	logical flags(MAXCHAN),skip
	double precision preamble(5)
	integer nchan,nindx,vCheck,indx(MAXINDX),nschan(MAXINDX)
	integer onchan(MAXINDX),oschan(MAXINDX),i,offset
        
c
c  Externals.
c
	logical uvVarUpd
c
c  Create a handle to track those things that cause us to have to
c  re-check the indices.
c
	call HanGen(tVis,vCheck,dosource,dofreq,dowin,dowide)
c
c  Loop the loop.
c
	skip = .true.
	call uvread(tVis,preamble,data,flags,MAXCHAN,nchan)
	dowhile(nchan.gt.0)
c
c  Update the indices if necessary.
c
	  if(uvVarUpd(vCheck))then
	    call GetIndx(tVis,dosource,dofreq,dowin,dowide,mosaic,
     *	      maxwidth,clobber,indx,nschan,onchan,oschan,
     *        nIndx,MAXINDX)
	    skip = .true.
	    do i=1,nIndx
	      if(indx(i).ne.0)skip = .false.
	    enddo
	  endif
c
c  Now write out the data.
c
	  if(.not.skip)then
	    offset = 1
	    do i=1,nindx
	      if(indx(i).gt.0)call FileDat(indx(i),
     *		preamble,data(offset),flags(offset),onchan(i),oschan(i))
	      offset = offset + onchan(i)
	    enddo
	    if(offset.ne.nchan+1)
     *		call bug('f','Consistency check failed')
	  endif
c
c  Loop the loop.
c
	  call uvread(tVis,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	end
c************************************************************************
	subroutine GetIndx(tVis,dosource,dofreq,dowin,dowide,mosaic,
     *	  maxwidth,clobber,Indx,nschan,onchan,oschan,nIndx,MAXINDX)
c
	implicit none
	integer tVis,nIndx,MAXINDX,nschan(MAXINDX),Indx(MAXINDX)
        integer onchan(MAXINDX),oschan(MAXINDX)
	logical dosource,dofreq,dowin,dowide,mosaic,clobber
        real maxwidth
c  Inputs:
c       tVis - the handle to the visibility file
c       dosource - split by source
c       dofreq   - split by freq
c       dowin    - split by spectral window
c       dowide   - split by wide band
c       mosaic   - don't split mosaic fields if splitting by source
c       maxwidth - max bandwidth of the output files - further freq split
c       clobber  - destroy existing files with same names
c   Outputs:
c       Indx     - index to translate from output name to file
c       nschan   - number of channels in each input spectral window
c       nIndx    - number of entries in indx table
c       onchan   - number of channels in each output file
c       oschan   - starting channel in input window for each output file
c
c  Determine the current indices of interest.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	character base*32,source*32,c*1
	integer maxi,n,nchan,ichan,nwide,length,lenb,i,ii,nsub,j,nindx1
	double precision sdf(MAXWIN),sfreq(MAXWIN)
	real wfreq(MAXWIN)
	logical discard,duplicate
c
c  Externals.
c
	character itoaf*8,stcat*16
	integer len1
c
c  Initialise.
c
	maxi = min(MAXINDX,MAXWIN)
c
c  Generate the base name.
c
	length = 0
	if(dosource)then
	  call uvrdvra(tVis,'source',source,' ')
	  length = min(len1(source),len(base))
	  lenb = 0
	  discard = .false.
	  do i=1,length
	    c = source(i:i)
	    if(discard.or.(c.eq.'_'.and.mosaic))then
	      discard = .true.
	    else if((c.ge.'a'.and.c.le.'z').or.
     *	       (c.ge.'A'.and.c.le.'Z').or.
     *	       (c.ge.'0'.and.c.le.'9').or.
     *	       c.eq.'-'.or.c.eq.'+'.or.c.eq.'_'.or.c.eq.'.')then
	      lenb = lenb + 1
	      base(lenb:lenb) = c
	    endif
	  enddo
	  length = lenb
	endif
c
	if(length.eq.0)then
	  base = 'uvsplit'
	  length = 7
	endif
c
c  The split is based on frequency.
c
c  Wide band case.
c
	if(dofreq)then
	  if(dowide)then
	    call uvrdvri(tVis,'nwide',nwide,1)
	    if(.not.dowin)then
	      call uvrdvrr(tVis,'wfreq',wfreq,0.)
	      nindx = 1
	    else
	      nindx = nwide
	      if(nindx.gt.maxi)call bug('f','Too many wide channels')
	      call uvgetvrr(tVis,'wfreq',wfreq,nindx)
	    endif
	    do i=1,nindx
	      n = nint(1000*wfreq(i))
	      call FileIndx(base(1:length)//'.'//itoaf(n),i,indx(i),
     *		clobber)
	      nschan(i) = 1
	    enddo
	    if(nindx.eq.1)nschan(1) = nwide
c
c  Channel Case.
c
	  else
	    call uvrdvri(tVis,'nchan',nchan,1)
	    if(.not.dowin)then
	      call uvrdvrd(tVis,'sfreq',sfreq,0.d0)
	      call uvrdvrd(tVis,'sdf',sdf,0.d0)
	      call uvrdvri(tVis,'nschan',nschan,1)
	      nindx = 1
	    else
	      call uvrdvri(tVis,'nspect',nindx,1)
	      if(nindx.gt.maxi)call bug('f','Too many spec windows')
	      call uvgetvrd(tVis,'sdf',sdf,nindx)
	      call uvgetvrd(tVis,'sfreq',sfreq,nindx)
	      call uvgetvri(tVis,'nschan',nschan,nindx)
	    endif
c            
c Check for data with IFs at identical frequencies         
c
            duplicate=.false.
            do i=1,nindx-1
              do j=i+1,nindx
                if (sfreq(i).eq.sfreq(j)) duplicate=.true.
              enddo
            enddo
                  
            nindx1=nindx
            ii=0
c
c Optional subdivision of spectra based on maxwidth
c onschan = # output channels for each output spectrum
c chan = channel offset in input spectrum for current output spectrum
c
	    do i=1,nindx
              nsub=1
              if (maxwidth.gt.0.0.and.
     *            abs(sdf(i)*nschan(i)).gt.maxwidth) then
                nsub = max(1,nint(abs(sdf(i)*(nschan(i)+0.1)/maxwidth)))
              endif
              nindx1=nindx1+nsub-1
	      if(nindx1.gt.MAXINDX)
     *          call bug('f','Too many output windows')
              oschan(ii+1)=0
              do j=1,nsub
                ii=ii+1
                ichan = nschan(i)*(2*j-1)/2/nsub
                onchan(ii) = nschan(i)/nsub
                if (j.gt.1) oschan(ii) = oschan(ii-1)+onchan(ii-1)
                n = nint(1000*(sfreq(i) +  sdf(i) * ichan))
                if (duplicate) then
                  call FileIndx(base(1:length)//'.'//
     *              stcat(itoaf(n),'.'//itoaf(i)),i,indx(ii),clobber)
                else
                  call FileIndx(base(1:length)//'.'//itoaf(n),i,
     *              indx(ii),clobber)
                endif                
              enddo
              onchan(ii)=nschan(i)-(nsub-1)*(nschan(i)/nsub)
	    enddo
            nindx=nindx1
	    if(nindx.eq.1)nschan(1) = nchan
	  endif
c
c  The split is based on IF number.
c
	else if(dowin)then
	  if(dowide)then
	    call uvrdvri(tVis,'nwide',nindx,1)
	    if(nindx.gt.maxi)call bug('f','Too many windows')
	    do i=1,nindx
	      call FileIndx(base(1:length)//'.'//itoaf(i),i,indx(i),
     *		clobber)
	      nschan(i) = 1
	    enddo	      
	  else
	    call uvrdvri(tVis,'nspect',nindx,1)
	    if(nindx.gt.maxi)call bug('f','Too many windows')
	    call uvgetvri(tVis,'nschan',nschan,nindx)
	    do i=1,nindx
	      call FileIndx(base(1:length)//'.'//itoaf(i),i,indx(i),
     *		clobber)
	    enddo
	  endif
c
c  The split is based purely on the source name.
c
	else
	  if(dowide)then
	    call uvrdvri(tVis,'nwide',nchan,1)
	  else
	    call uvrdvri(tVis,'nchan',nchan,1)
	  endif
	  call FileIndx(base(1:length),0,indx,clobber)
	  nindx = 1
	  nschan(1) = nchan
	endif
c
	end
c************************************************************************
	subroutine HanGen(tVis,vCheck,dosource,dofreq,dowin,dowide)
c
	implicit none
	integer tVis,vCheck
	logical dosource,dofreq,dowin,dowide
c
c  Determine which variables we have to track changes.
c
c------------------------------------------------------------------------
	call uvVarIni(tVis,vCheck)
	if(dosource)call uvVarSet(vCheck,'source')
	if(dofreq)then
	  if(dowide)then
	    call uvVarSet(vCheck,'wfreq')
	  else
	    call uvVarSet(vCheck,'sfreq')
	    call uvVarSet(vCheck,'sdf')
	    call uvVarSet(vCheck,'nschan')
	  endif
	else if(dowin)then
	  if(dowide)then
	    call uvVarSet(vCheck,'nwide')
	  else
	    call uvVarSet(vCheck,'nspect')
	  endif
	endif
c
	end
c************************************************************************
	subroutine GetOpt(dosource,dofreq,dowin,docopy,mosaic,clobber)
c
	implicit none
	logical dosource,dofreq,dowin,docopy,mosaic,clobber
c
c  Determine extra processing options.
c
c  Output:
c    dosource
c    dofreq
c    dowin
c    docopy
c    mosaic
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=6)
	logical present(NOPTS)
	character opts(NOPTS)*8
	data opts/'nosource','nofreq  ','nowindow','nocopy  ',
     *		  'mosaic  ','clobber '/
c
	call options('options',opts,present,NOPTS)
	dosource = .not.present(1)
	dofreq   = .not.present(2)
	dowin    = .not.present(3)
	docopy   = .not.present(4)
	mosaic   =      present(5)
	clobber  =      present(6)
c
	end
c************************************************************************
	subroutine FileSup(tVis,tdowide,tdocomp,tdoif,
     *						wins1,nwins1,tvers)
c
	implicit none
	integer tVis,nwins1
	logical tdowide,tdocomp,tdoif,wins1(nwins1)
	character tvers*(*)
c
c  Initialise the File* routines.
c------------------------------------------------------------------------
	include 'uvsplit.h'
	integer i
c
	version = 'UVSPLIT: Miriad '//tvers
c
c  Copy the record of selected windows.
c
	nwins = nwins1
	if(nwins.gt.MAXWIN)
     *	  call bug('f','Window selection buffer overflow')
	do i=1,nwins1
	  wins(i) = wins1(i)
	enddo
c
	lVis = tVis
	call rdhdi(lVis,'npol',npol,0)
	dowide = tdowide
	docomp = tdocomp
	doif = tdoif
c
	nfiles = 0
	nopen = 0
	end
c************************************************************************
	subroutine FileDat(tindx,preamble,data,flags,nchan,offset)
c
	implicit none
	integer tindx,nchan,offset
	double precision preamble(5)
	complex data(nchan)
	logical flags(nchan)
c
c  Write data out to a file.
c
c  Input:
c    tindx	Index into the file table.
c    preamble,data,flags,nchan Normal uvwrite arguments.
c
c------------------------------------------------------------------------
	include 'uvsplit.h'
c
c  Externals.
c
	logical uvVarUpd
c
c  Return straight away if there is nothing to do.
c
	if(tIndx.eq.0)return
c
c  Copy variables that have changed to the output.
c
	call uvvarcpy(vCopy(tIndx),lOut(tIndx))
c
c  Generate the appropriate frequency descriptors, if needed.
c
	if(doif)then
	  if(uvVarUpd(vCheck(tIndx)))then
	    if(dowide)then
	      call FileWpec(lVis,lOut(tIndx),nchan,ifno(tIndx))
	    else
	      call FileCpec(lVis,lOut(tIndx),nchan,ifno(tIndx),offset)
	    endif
	  endif
	endif
c
c  Finally write the data.
c
	call uvwrite(lOut(tIndx),preamble,data,flags,nchan)
	end
c************************************************************************
	subroutine FileWpec(lVis,lOut,nchan,ifno)
c
	implicit none
	integer lVis,lOut,nchan,ifno
c
c  Pick out the wideband description of this wideband channel.
c
c  Input:
c    lVis
c    lOut
c    nchan
c    ifno
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer nwide
	real wfreq(MAXWIN),wwidth(MAXWIN)
c
	if(nchan.ne.1)call bug('f','Inconsistency!!')
c
	call uvrdvri(lVis,'nwide',nwide,1)
	if(ifno.gt.nwide)call bug('f','Something is screwy')
	call uvgetvrr(lVis,'wfreq',wfreq,nwide)
	call uvgetvrr(lVis,'wwidth',wwidth,nwide)
c
	call uvputvrr(lOut,'wfreq',wfreq(ifno),1)
	call uvputvrr(lOut,'wwidth',wwidth(ifno),1)
c
c  Now the wide-band system temperature.
c
	call UpdVar(lVis,lOut,ifno,nwide,'wsystemp')
c
	end
c************************************************************************
	subroutine FileCpec(lVis,lOut,nchan,ifno,offset)
c
	implicit none
	integer lVis,lOut,nchan,ifno,offset
c
c  Pick out the spectral description of this spectral window.
c
c  Input:
c    lVis
c    lOut
c    nchan
c    ifno
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer nspect
	double precision sdf(MAXWIN),sfreq(MAXWIN)
	double precision restfreq(MAXWIN)
c
	call uvrdvri(lVis,'nspect',nspect,1)
	if(ifno.gt.nspect)call bug('f','Something is screwy')
	call uvgetvrd(lVis,'sdf',sdf,nspect)
	call uvgetvrd(lVis,'sfreq',sfreq,nspect)
	call uvgetvrd(lVis,'restfreq',restfreq,nspect)
c
	call uvputvri(lOut,'nspect',1,1)
	call uvputvri(lOut,'nschan',nchan,1)
	call uvputvri(lOut,'ischan',1,1)
	call uvputvrd(lOut,'sdf',sdf(ifno),1)
	call uvputvrd(lOut,'sfreq',sfreq(ifno)+offset*sdf(ifno),1)
	call uvputvrd(lOut,'restfreq',restfreq(ifno),1)
c
c  Update the system temperature and the XY phase.
c
	call UpdVar(lVis,lOut,ifno,nspect,'systemp')
	call UpdVar(lVis,lOut,ifno,nspect,'xyphase')
c
	end
c************************************************************************
	subroutine UpdVar(lVis,lOut,ifno,nspect,var)
c
	implicit none
	integer lVis,lOut,ifno,nspect
	character var*(*)
c
c  Update a variable which is of dimension (nants,nspect).
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer n,nants,off
	real buf(MAXANT*MAXWIN)
	logical upd
	character type*1
c
	call uvprobvr(lVis,var,type,n,upd)
	upd = type.eq.'r'.and.n.le.MAXANT*MAXWIN.and.n.gt.0
	if(upd)then
	  call uvgetvrr(lVis,var,buf,n)
	  call uvrdvri(lVis,'nants',nants,0)
	  if(n.lt.nants*nspect)then
	    n = min(n,nants)
	    off = 1
	  else
	    n = nants
	    off = (ifno-1)*nants + 1
	  endif
          
	  call uvputvrr(lOut,var,buf(off),n)
	endif
c
	end
c************************************************************************
	subroutine FileIndx(name,tifno,tindx,clobber)
c
	implicit none
	character name*(*)
	integer tifno,tindx
	logical clobber
c
c  Get the index corresponding to a particular file.
c------------------------------------------------------------------------
	include 'uvsplit.h'
	integer n
	logical more
	character line*64
c
c  Externals.
c
	integer binsrcha
c
c  Do we already have this file.
c
	tindx = 0
	if(.not.wins(tifno))return
	if(nfiles.gt.0)tindx = binsrcha(name,out,nfiles)
	if(tindx.gt.0)tindx = indx(tindx)
c
c  We do not. Create a new slot for it.
c
	if(tindx.eq.0)then
	  if(nfiles.eq.MAXFILES)call bug('f','Name buffer overflow')
c
c  Merge the name into our ordered list of known names.
c
	  n = nfiles
	  more = .true.
	  dowhile(n.gt.0.and.more)
	    more = Out(n).gt.name
	    if(more)then
	      Out(n+1) = Out(n)
	      Indx(n+1) = Indx(n)
	      n = n - 1
	    endif
	  enddo
c
	  Out(n+1) = name
	  Indx(n+1) = nfiles + 1
c
c  Fix up the description of this file.
c
	  nfiles = nfiles + 1
	  tIndx = nfiles
	  done(tIndx) = .false.
	  lOut(tIndx) = 0
	endif
c
c  Remember the current IF slot of this data.
c
	ifno(tIndx) = tifno
c
c  If we have space open, open this file up. Otherwise just check that
c  the file does not already exist.
c
	if(lOut(tindx).eq.0.and..not.done(tindx))then
	  if(nopen.lt.MAXOPEN)then
	    call FileOpen(lVis,lOut(tIndx),clobber,
     *		vCheck(tIndx),vCopy(tIndx),
     *		name,dowide,docomp,doif,npol,version)
	    nopen = nopen + 1
	  else if(.not.clobber)then
	    line = 'File already exists: '//name
	    call assertf(name,.false.,line)
	  endif
	endif
c
	if(lOut(tIndx).eq.0)tIndx = 0
c
	end
c************************************************************************
	subroutine FileOpen(lVis,lOut,clobber,vCheck,vCopy,
     *			name,dowide,docomp,doif,npol,version)
c
	implicit none
	integer lVis,lOut,vCheck,vCopy,npol
	character name*(*),version*(*)
	logical dowide,docomp,doif,clobber
c
c  Open and initialise an output file.
c
c------------------------------------------------------------------------
	integer i,vTemp,tno,iostat
	character line*64
c
	integer NCOPY,NSCHECK,NWCHECK
	parameter(NCOPY=98,NSCHECK=8,NWCHECK=3)
	character copy(NCOPY)*8,scheck(NSCHECK)*8,wcheck(NWCHECK)*8
        data copy/    'airtemp ','antaz   ','antdiam ','antel   ',
     *     'antpos  ','atten   ','axismax ','axisrms ','bin     ',
     *     'cable   ','calcode ','chi     ','chi2    ','corbit  ',
     *     'corbw   ','corfin  ','cormode ','coropt  ','cortaper',
     *     'ddec    ','dec     ','delay   ','delay0  ','deldec  ',
     *     'delra   ','dewpoint','dra     ','epoch   ','evector ',
     *     'focus   ','freq    ','freqif  ','ifchain ','inttime ',
     *     'ivalued ','jyperk  ','jyperka ','latitud ','longitu ',
     *     'lo1     ','lo2     ','lst     ','mount   ','name    ',
     *     'nants   ','nbin    ','ntemp   ','ntpower ','obsdec  ',
     *     'observer','obsline ','obsra   ','on      ','operator',
     *     'pbfwhm  ','pbtype  ','phaselo1','phaselo2','phasem1 ',
     *     'plangle ','plmaj   ','plmin   ','pltb    ','pntdec  ',
     *     'pntra   ','precipmm','pressmb ','project ','ra      ',
     *     'rain    ','refpnt  ','relhumid','rmspath ','sctype  ',
     *     'smonrms ','source  ','tau230  ','telescop','temp    ',
     *     'themt   ','tif2    ','tpower  ','tsis    ','ut      ',
     *     'veldop  ','veltype ','version ','vsource ','wind    ',
     *     'winddir ','windmph ','xtsys   ','ytsys   ','xsampler',
     *     'ysampler','xyamp   ',
     *     'npol    ','pol     '/
c
	data SCheck/  'nspect  ','restfreq','ischan  ','nschan  ',
     *     'sfreq   ','sdf     ','systemp ','xyphase '/
        data WCheck/  'wfreq   ','wwidth  ','wsystemp'/
c
c  Open the file, and set the correlation type.
c
	if(clobber)then
	  call hopen(tno,name,'old',iostat)
	  if(iostat.eq.0)then
	    line = 'Clobbering '//name
	    call output(line)
	    call hrm(tno)
	  endif
	endif
	line = 'Creating '//name
	call output(line)
	call uvopen(lOut,name,'new')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	if(dowide)then
	  call uvset(lOut,'data','wide',0,0.,0.,0.)
	else
	  if(docomp)then
	    call uvset(lOut,'corr','j',0,0.,0.,0.)
	  else
	    call uvset(lOut,'corr','r',0,0.,0.,0.)
	  endif
	endif
c
c  Write the number of polarisations.
c
	if(npol.gt.0)call wrhdi(lOut,'npol',npol)
c
c  Make the output history.
c
	call hdcopy(lVis,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,version)
	call hisinput(lOut,'UVSPLIT')
	call hisclose(lOut)
c
c  Determine the things to be checked and the things to be copied.
c
	call uvVarIni(lVis,vCopy)
	do i=1,NCOPY
	  call uvVarSet(vCopy,Copy(i))
	enddo
c
c  Determine whether we have to check ot copy the variables that describe
c  the frequency setup.
c
	if(doif)then
	  call uvVarIni(lVis,vCheck)
	  vTemp = vCheck
	else
	  vCheck = 0
	  vTemp = vCopy
	endif
c
c  Remember the variables to check or copy.
c
	if(dowide)then
	  do i=1,NWCHECK
	    call uvVarSet(vTemp,WCheck(i))
	  enddo
	else
	  do i=1,NSCHECK
	    call uvVarSet(vTemp,SCheck(i))
	  enddo
	endif
c
	end
c************************************************************************
	subroutine FileFin(docopy,more)
c
	implicit none
	logical more,docopy
c
c  Close up any open files, and see if there are any more to be done.
c
c  Input:
c    docopy	Copy the calibration tables.
c  Output:
c    more	Is there more to be done?
c
c------------------------------------------------------------------------
	integer i,j
	include 'uvsplit.h'
c
	integer NTABLE
	parameter(NTABLE=13)
	character tables(NTABLE)*8
	data tables/'interval','nsols   ','ngains  ','nfeeds  ',
     *	 'ntau    ','gains   ','freq0   ','leakage ','bandpass',
     *	 'freqs   ','nspect0 ','nchan0  ','senmodel'/
c
	more = .false.
	do i=1,nfiles
	  if(lOut(i).ne.0)then
	    if(docopy)then
	      do j=1,NTABLE
		call hdcopy(lVis,lOut(i),tables(j))
	      enddo
	    endif
	    call uvclose(lOut(i))
	    lOut(i) = 0
	    done(i) = .true.
	    nopen = nopen - 1
	  else
	    more = more.or..not.done(i)
	  endif
	enddo
c
	if(nopen.ne.0)call bug('f','Consistency check failed')
c
	end

