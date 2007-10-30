c************************************************************************
      program uvaflag
c
c= uvaflag - Use flags in one visibility dataset to flag another.
c& rjs
c: calibration
c+
c	UVAFLAG is a MIRIAD task which flags correlations in one 
c	visibility dataset based on the flag status of matching
c	correlations in a template dataset.
c
c	The template dataset should be a subset of the visibility dataset
c	being flagged, and the order of records in the two datasets should
c	be the same. Normally correlations in the template and visibility
c	datasets are matched by comparing time, polarisation, baseline and
c	frequency. However options `nopol' and `nofreq' can turn off the
c	matching of polarisation and frequency.
c@ vis
c	The input visibility file to be flagged. No default.
c@ tvis
c	The template input visibility file. The default is the same
c	as the `vis' dataset. This default makes no sense without the `nopol'
c	or `nofreq' options. Several files can be given. Wildcards
c	are supported.
c@ select
c	Normal visibility selection, which is applied to the template
c	dataset. See the help on `select' for more information.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	Minimum match is supported. Possible values are:
c	  nopol  The polarisation of the records in the template are
c	         ignored. If any polarisation in the template is
c	         flagged, then all polarisations in the input are flagged.
c	         This can be useful when applying flagging based on one
c	         polarisation type (e.g. stokes V) to the other polarisations.
c	  nofreq The frequency of the correlations in the template are
c	         ignored. If any channel in the template is flagged, then
c	         all channels in the input are flagged. This can be useful
c	         when applying flagging based on a `channel-0' dataset.
c	  noapply Do not apply the flagging, just report the statistics
c	         about what would be flagged.
c--
c  History:
c     nebk 25may89 Original program
c     pjt   2may90 included maxchan through maxdim.h
c     bpw  28jan91 include standard keyword vis
c     rjs   8mar93 Standardise history writing.
c     rjs   4oct97 Go back to the drawing board and rewrite program.
c     rjs   7nov97 Handle multiple templates.
c     rjs  22oct98 noapply option.
c---------------------------------------------------------------------------
	implicit none
	include 'maxdim.h'
	integer MAXSELS,MAXFILES
	character version*(*)
	parameter(version='Uvaflag: version 1.0 22-Oct-98')
	parameter(MAXSELS=512,MAXFILES=64)
c
	complex data(maxchan)
	double precision ttbp(3),vtbp(3)
	real sels(MAXSELS)
	integer lVis,lTmp,vVis,vTmp,ntot,ngood,nflag,i,npol,nt,nv,offset
	integer nfiles,k
	character in1(MAXFILES)*64,in2*64,line*64
	logical vflags(MAXCHAN),tflags(MAXCHAN),nofreq,nopol,match,doapp
c
c  Externals.
c
	logical selProbe
c
c Get inputs
c
	call output(version)
	call keyini
	call keya('vis', in2, ' ')
	if(in2.eq.' ')call bug('f','Visibility file name not given')
	call mkeyf('tvis',in1,MAXFILES,nfiles)
	if(nfiles.eq.0)then
	  in1(1) = in2
	  nfiles = 1
	endif
	call selInput('select',sels,MAXSELS)
	call getopt(nofreq,nopol,doapp)
	if(nfiles.eq.1.and.in1(1).eq.in2.and.
     *	  .not.nofreq.and..not.nopol)
     *	  call bug('f','Requested operation makes no sense')
	call keyfin
c
c Open files
c
	do k=1,nfiles
	  call uvopen(lVis,in2,'old')
	  call uvset(lVis,'preamble','time/baseline/pol',0,0.,0.,0.)
	  call uvopen(lTmp,in1(k),'old')
	  call uvset(lTmp,'preamble','time/baseline/pol',0,0.,0.,0.)
	  call selApply(lTmp,sels,.true.)
	  if(nopol.and.selProbe(sels,'polarization?',0.d0))call bug('f',
     *	    'Polarisation selection cannot be used with options=nopol')
	  call offIni(lVis,vVis,lTmp,vTmp,offset)
c
	  ntot  = 0
	  ngood = 0
	  nflag = 0
c
c Loop over visibilities and set flags
c
	  call getrec(lTmp,nopol,nofreq,ttbp,tflags,MAXCHAN,nt)
	  dowhile(nt.gt.0)
	    call uvread(lVis,vtbp,data,vflags,MAXCHAN,nv)
	    match = .false.
	    if(nv.eq.0)then
	      call bug('w','Unexpected end of visibility dataset')
	    else if(abs(vtbp(1)-ttbp(1)).lt.1./86400.0.and.
     *	       nint(vtbp(2)-ttbp(2)).eq.0.and.
     *	       nopol)then
	      match = .true.
	      if(.not.nofreq)
     *		call offGet(lVis,vVis,nv,lTmp,vTmp,nt,offset)
	      call flagit(tflags,nt,vflags,nv,offset,
     *				nofreq,ntot,ngood,nflag)
	      if(doapp)call uvflgwr(lVis,vflags)
	      call uvrdvri(lVis,'npol',npol,1)
	      do i=2,npol
	        call uvread(lVis,vtbp,data,vflags,MAXCHAN,nv)
	        call flagit(tflags,nt,vflags,nv,offset,
     *				nofreq,ntot,ngood,nflag)
	        if(doapp)call uvflgwr(lVis,vflags)
	      enddo
	    else if(abs(vtbp(1)-ttbp(1)).lt.1./86400.0.and.
     *		nint(vtbp(2)-ttbp(2)).eq.0.and.
     *		nint(vtbp(3)-ttbp(3)).eq.0)then
	      match = .true.
	      if(.not.nofreq)
     *		call offGet(lVis,vVis,nv,lTmp,vTmp,nt,offset)
	      call flagit(tflags,nt,vflags,nv,offset,
     *				nofreq,ntot,ngood,nflag)
	      if(doapp)call uvflgwr(lVis,vflags)
	    else
	      call countit(vflags,nv,ntot,ngood)
	    endif
c
c  Go back for more.
c
	    if(nv.eq.0)then
	      nt = 0
	    else if(match)then
	      call getrec(lTmp,nopol,nofreq,ttbp,tflags,MAXCHAN,nt)
	    endif
	  enddo
c
c  Finish counting the flags in the main visibility file.
c
	  dowhile(nv.gt.0)
	    call uvread(lVis,vtbp,data,vflags,MAXCHAN,nv)
	    call countit(vflags,nv,ntot,ngood)
	  enddo
	  call uvclose(lTmp)
	  if(k.eq.nfiles)then
	    call hisopen(lVis,'append')
	    call hiswrite(lVis,'UVAFLAG: Miriad '//version)
	    call hisinput(lVis,'UVAFLAG')
	    call hisclose (lVis)
	  endif
	  call uvclose(lVis)
c
c  Give a summary about the flagging performed.
c
	  if(nfiles.gt.1)call output('After processing '//in1(k))
	  call output(' Correlations: Total   Good      Bad')
	  write(line,'(a,i8,i8,i8)')' Before:    ',ntot,ngood,ntot-ngood
	  call output(line)
	  write(line,'(a,i8,i8,i8)')' After:     ',
     *				   ntot,ngood-nflag,ntot-ngood+nflag
	  call output(line)
c
	enddo
c
	end
c************************************************************************
	subroutine flagit(tflags,nt,vflags,nv,offset,
     *					nofreq,ntot,ngood,nflag)
c
	implicit none
	integer nt,nv,ntot,ngood,nflag,offset
	logical tflags(nt),vflags(nv),nofreq
c------------------------------------------------------------------------
	integer i
c
	if(nofreq)then
	  if(tflags(1))then
	    do i=1,nv
	      if(vflags(i))ngood = ngood + 1
	    enddo
	  else
	    do i=1,nv
	      if(vflags(i))then
		nflag = nflag + 1
		ngood = ngood + 1
		vflags(i) = .false.
	      endif
	    enddo
	  endif
c
	else
	  do i=1,nv
	    if(vflags(i))then
	      ngood = ngood + 1
	      if(i.ge.offset+1.and.i.le.offset+nt)then
		if(.not.tflags(i-offset))then
		  nflag = nflag + 1
		  vflags(i) = .false.
		endif
	      endif
	    endif
	  enddo
	endif
c
	ntot = ntot + nv
	end
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
c************************************************************************
	subroutine GetOpt(nofreq,nopol,doapp)
c
	implicit none
	logical nofreq,nopol,doapp
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'nofreq  ','nopol   ','noapply '/
c
	call options('options',opts,present,NOPTS)
	nofreq =      present(1)
	nopol  =      present(2)
	doapp  = .not.present(3)
c
	end
c************************************************************************
	subroutine getrec(lTmp,nopol,nofreq,ttbp,tflags,mchan,nt)
c
	implicit none
	integer lTmp,mchan,nt
	logical nopol,nofreq,tflags(mchan)
	double precision ttbp(3)
c
c  Get another record from the input.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	logical flags(MAXCHAN),flags2(MAXCHAN),f
	integer nchan,i,j,npol
	complex data(MAXCHAN)
c
	call uvread(lTmp,ttbp,data,flags,MAXCHAN,nchan)
	if(nopol)then
	  call uvrdvri(lTmp,'npol',npol,1)
	  do j=2,npol
	    call uvread(lTmp,ttbp,data,flags2,MAXCHAN,nchan)
	    do i=1,nchan
	      flags(i) = flags(i).and.flags2(i)
	    enddo
	  enddo
	endif
c
	if(nofreq)then
	  f = flags(1)
	  do i=2,nchan
	    f = f.and.flags(i)
	  enddo
	  tflags(1) = f
	  nt = 1
	else
	  if(nchan.gt.mchan)call bug('f','Too many flags for me')
	  do i=1,nchan
	    tflags(i) = flags(i)
	  enddo
	  nt = nchan
	endif
c
	end
c************************************************************************
	subroutine offGet(lVis,vVis,nv,lTmp,vTmp,nt,offset)
c
	implicit none
	integer vVis,vTmp,lTmp,lVis,offset,nt,nv
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer CHANNEL,WIDE
	parameter(CHANNEL=1,WIDE=2)
	double precision tsdf(MAXWIN),vsdf(MAXWIN)
	double precision tsfreq(MAXWIN),vsfreq(MAXWIN)
	integer tnschan(MAXWIN),vnschan(MAXWIN)
	real twfreq(MAXWIN),vwfreq(MAXWIN)
	real twwidth(MAXWIN),vwwidth(MAXWIN)
	double precision tline(6),vline(6)
	integer tnspect,vnspect,tnwide,vnwide,i,n,o
	real t,tol
	logical updated,more
	character type*1
c
c  Externals.
c
	logical uvVarUpd
c
	if(uvVarUpd(vVis).or.uvVarUpd(vTmp))then
	  call uvinfo(lVis,'line',vline)
	  call uvinfo(lTmp,'line',tline)
	  if(nint(tline(1)).ne.nint(vline(1)))
     *	    call bug('f','Line types of the two datasets differ')
c
c  Handle channel linetypes.
c
	  if(nint(tline(1)).eq.CHANNEL)then
	    call uvprobvr(lVis,'sfreq',type,vnspect,updated)
	    call uvprobvr(lTmp,'sfreq',type,tnspect,updated)
	    if(max(vnspect,tnspect).gt.MAXWIN)
     *	      call bug('f','Too many windows for me')
	    call uvgetvrd(lVis,'sdf',vsdf,vnspect)
	    call uvgetvrd(lVis,'sfreq',vsfreq,vnspect)
	    call uvgetvri(lVis,'nschan',vnschan,vnspect)
	    call uvgetvrd(lTmp,'sdf',tsdf,tnspect)
	    call uvgetvrd(lTmp,'sfreq',tsfreq,tnspect)
	    call uvgetvri(lTmp,'nschan',tnschan,tnspect)
	    more = .true.
	    i = 0
	    n = 0
	    dowhile(more.and.i.lt.vnspect)
	      i = i + 1
	      t = ( tsfreq(1) - vsfreq(i) ) / vsdf(i)
	      o = nint(t)
	      more = abs(vsdf(i)-tsdf(1)).gt.
     *		0.05*min(abs(vsdf(i)),abs(tsdf(1)))	.or.
     *		abs(o-t).gt.0.05			.or.
     *		o.lt.0					.or.
     *		o.ge.vnschan(i)
	      offset = o + n
	      n = n + vnschan(i)
	    enddo
	    if(o+tnschan(1).lt.vnschan(i).and.tnspect.gt.1)
     *	      call bug('f','Channel subsetting to complex for me')
	  else
	    call uvprobvr(lVis,'wwidth',type,vnwide,updated)
	    call uvprobvr(lTmp,'wwidth',type,tnwide,updated)
	    if(max(tnwide,vnwide).gt.MAXWIN)
     *		call bug('f','Too many wide channels for me')
	    call uvgetvrr(lVis,'wwidth',vwwidth,vnwide)
	    call uvgetvrr(lVis,'wfreq', vwfreq, vnwide)
	    call uvgetvrr(lTmp,'wwidth',twwidth,tnwide)
	    call uvgetvrr(lTmp,'wfreq', twfreq, tnwide)
	    more = .true.
	    i = 0
	    dowhile(more.and.i.lt.vnwide)
	      i = i + 1
	      tol = 0.05*min(abs(vwwidth(i)),abs(twwidth(1)))
	      more = abs(vwwidth(i)-twwidth(1)).gt.tol	.or.
     *		abs(vwfreq(i)-twfreq(1)).gt.tol
	      offset = i - 1
	    enddo
	  endif
	  if(more.or.offset+nt.gt.nv)call bug('f',
     *		'Failed to match channels between datasets')
	endif
c
	end
c************************************************************************
	subroutine offIni(lVis,vVis,lTmp,vTmp,offset)
c
	implicit none
	integer vVis,vTmp,lVis,lTmp,offset
c
c  Initialise the routine to determine the range of channels.
c------------------------------------------------------------------------
	offset = 0
	call uvVarIni(lVis,vVis)
	call uvVarSet(vVis,'nspect')
	call uvVarSet(vVis,'sfreq')
	call uvVarSet(vVis,'sdf')
	call uvVarSet(vVis,'nschan')
	call uvVarSet(vVis,'wfreq')
	call uvVarSet(vVis,'wwidth')
	call uvVarIni(lTmp,vTmp)
	call uvVarIni(lVis,vVis)
	call uvVarSet(vTmp,'nspect')
	call uvVarSet(vTmp,'sfreq')
	call uvVarSet(vTmp,'sdf')
	call uvVarSet(vTmp,'nschan')
	call uvVarSet(vTmp,'wfreq')
	call uvVarSet(vTmp,'wwidth')
	end
	
