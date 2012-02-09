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
c	being flagged, and the order of records in the two datasets 
c	should be the same. Normally correlations in the template and 
c	visibility datasets are matched by comparing time, polarisation, 
c	baseline, bin and frequency. However options `nopol',`nofreq' 
c	and 'nobin' can turn off the matching of polarisation, frequency
c       and bin  number.
c@ vis
c	The input visibility file to be flagged. No default.
c@ tvis
c	The template input visibility file. The default is the same
c	as the `vis' dataset. This default makes no sense without the 
c	`nopol' or `nofreq' options. Several files can be given. 
c	Wildcards are supported.
c@ select
c	Normal visibility selection, which is applied to the template
c	dataset. See the help on "select" for more information.
c@ options
c	Extra processing options. Several can be given, separated by 
c       commas.
c	Minimum match is supported. Possible values are:
c	  nopol  The polarisation of the records in the template are
c	         ignored. If any polarisation in the template is
c	         flagged, then all polarisations in the input are flagged
c	         This can be useful when applying flagging based on one
c	         polarisation type (e.g. stokes V) to the other 
c                polarisations.
c	  nofreq The frequency of the correlations in the template are
c	         ignored. If any channel in the template is flagged, then
c	         all channels in the input are flagged. This can be 
c                useful when applying flagging based on a `channel-0'
c	         dataset.
c         nobin  The bin numbers of the correlations in the template are
c                ignored. If any bin in the template is flagged, then
c                all bins in the inputs are flagged. This can be useful
c                when applying flagging based on a bin averaged dataset.
c	  noapply Do not apply the flagging, just report the statistics
c	         about what would be flagged.
c$Id: uvaflag.for,v 1.3 2012/02/09 02:52:48 wie017 Exp $
c--
c  History:
c     nebk 25may89 Original program
c     pjt   2may90 included maxchan through maxdim.h
c     bpw  28jan91 include standard keyword vis
c     rjs   8mar93 Standardise history writing.
c     rjs   4oct97 Go back to the drawing board and rewrite program.
c     rjs   7nov97 Handle multiple templates.
c     rjs  22oct98 noapply option.
c     rjs  27apr09 Fix spurious warning when using options=nofreq. Change
c		   print output message format.
c     mhw  03feb12 Add nobin option
c------------------------------------------------------------------------
	implicit none
	include 'maxdim.h'
        include 'mem.h'
	integer MAXSELS,MAXFILES,MAXPOL
	character version*(*)
	parameter(version='Uvaflag: $Revision: 1.3 $, '//
     *             '$Date: 2012/02/09 02:52:48 $')
	parameter(MAXSELS=512,MAXFILES=64,MAXPOL=4)
c
	complex data(maxchan)
	double precision ttbp(4,MAXPOL,MAXBASE),vtbp(4)
	real sels(MAXSELS)
	integer lVis,lTmp,vVis,vTmp,ntot,ngood,nflag,i,npol,nt,nv,offset
	integer nfiles,k,ib,ip,nbl,npol1,off
	character in1(MAXFILES)*64,in2*64,line*64
	logical vflags(MAXCHAN)
        logical nofreq,nopol,nobin,doapp,match,eof,skip
        ptrdiff pFlag
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
	call getopt(nofreq,nopol,nobin,doapp)
	if(nfiles.eq.1.and.in1(1).eq.in2.and.
     *	  .not.nofreq.and..not.nopol.and..not.nobin)
     *	  call bug('f','Requested operation makes no sense')
	call keyfin
c
c Open files
c
	do k=1,nfiles
	  call uvopen(lVis,in2,'old')
	  call uvset(lVis,'preamble','time/baseline/pol/bin',0,0.,0.,0.)
	  call uvopen(lTmp,in1(k),'old')
	  call uvset(lTmp,'preamble','time/baseline/pol/bin',0,0.,0.,0.)
	  call selApply(lTmp,sels,.true.)
	  if(nopol.and.selProbe(sels,'polarization?',0.d0))call bug('f',
     *	    'Polarisation selection cannot be used with options=nopol')
	  call offIni(lVis,vVis,lTmp,vTmp,offset)
c
	  ntot  = 0
	  ngood = 0
	  nflag = 0
          skip = .false.
c
c Loop over visibilities and set flags
c
	  call getrec(lTmp,nopol,nofreq,ttbp,pFlag,MAXPOL,
     *      MAXBASE,nbl,npol,nt,eof)
          nv = 1
	  do while (nv.gt.0)
	    match = .false.
            if(.not.skip) call uvread(lVis,vtbp,data,vflags,MAXCHAN,nv)
            skip = .false.
 	    if(nv.eq.0.and..not.eof)then
	      call bug('w','Unexpected end of visibility dataset')
	    else if(abs(vtbp(1)-ttbp(1,1,1)).lt.1./86400.0.and.
     *              (nobin.or.nint(vtbp(4)-ttbp(4,1,1)).eq.0)) then
c
c matching time + bin found
c
              ib=1
              ip=1
              off=0
              match=.false.
              do while (.not.match.and.ib.le.nbl)
                if (nint(vtbp(2)-ttbp(2,ip,ib)).eq.0..and.
     *                (nopol.or.nint(vtbp(3)-ttbp(3,ip,ib)).eq.0)) then
c                      
c matching baseline + pol found
c
                  match = .true.
	          if(.not.nofreq)
     *		    call offGet(lVis,vVis,nv,lTmp,vTmp,nt,offset)
	          call flagit(memL(pFlag+off),nt,vflags,nv,offset,
     *		              nofreq,ntot,ngood,nflag)
	          if(doapp)call uvflgwr(lVis,vflags)
                  if (nopol) then
	            call uvrdvri(lVis,'npol',npol1,1)
                    do i=2,npol1
	              call uvread(lVis,vtbp,data,vflags,MAXCHAN,nv)
	              call flagit(memL(pFlag+off),nt,vflags,nv,
     *	                          offset,nofreq,ntot,ngood,nflag)
	              if(doapp)call uvflgwr(lVis,vflags)
	            enddo
                  endif
                else
                  ip=ip+1
                  if (ip.gt.npol) then
                    ip=1
                    ib=ib+1
                  endif
                  off = off + nt
                endif
              enddo
            else
c             
c Time or bin doesn't match, read next template
c             
              skip=.false.
              do while
     *         (.not.eof.and.((vtbp(1)-ttbp(1,1,1)).gt.1./86400.
     *          .or.(.not.nobin.and.nint(vtbp(4)-ttbp(4,1,1)).ne.0)))
                call getrec(lTmp,nopol,nofreq,ttbp,pFlag,MAXPOL,
     *                      MAXBASE,nbl,npol,nt,eof)
                skip = .true.
              enddo
	    endif
	    if (.not.match.and..not.skip) 
     *        call countit(vflags,nv,ntot,ngood)
c
c  Go back for more.
c
	  enddo
c
	  call uvclose(lTmp)
	  if(k.eq.nfiles.and.doapp)then
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
	  call output(' Correlations: Total      Good         Bad')
	  write(line,'(a,i11,i11,i11)')' Before:    ',
     *				   ntot,ngood,ntot-ngood
	  call output(line)
	  write(line,'(a,i11,i11,i11)')' After:     ',
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
	subroutine GetOpt(nofreq,nopol,nobin,doapp)
c
	implicit none
	logical nofreq,nopol,nobin,doapp
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=4)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'nofreq  ','nopol   ','nobin   ','noapply '/
c
	call options('options',opts,present,NOPTS)
	nofreq =      present(1)
	nopol  =      present(2)
        nobin  =      present(3)
	doapp  = .not.present(4)
c
	end
c************************************************************************
	subroutine getrec(lTmp,nopol,nofreq,ttbp,pFlag,mpol,
     *                    mbase,nbase,npol,nt,eof)
c
	implicit none
	integer lTmp,mbase,mpol,nbase,npol,nt
        ptrdiff pFlag
	logical nopol,nofreq,eof
	double precision ttbp(4,mpol,mbase)
c
c  Get another record from the input.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
        include 'mem.h'
        integer MAXPOL
        PARAMETER (MAXPOL=4)
	logical flags(MAXCHAN,MAXPOL),flags2(MAXCHAN),sflags(MAXCHAN)
        logical f,init,more
	integer nchan,i,nchan1,ib,ip,snchan,nFlags,off,nant
	complex data(MAXCHAN)
        double precision savetbp(4)
        common /last/ savetbp, sflags, snchan, init, nFlags
        data init/.true./, nFlags/0/
c
        ib = 1
        more = .true.
        ip = 1
        off = 0
	if (init) then
          savetbp(1)=0.d0
          call uvread(lTmp,ttbp(1,ip,ib),data,flags,MAXCHAN,nchan)
          call uvrdvri(lTmp,'nants',nant,1)
          call uvrdvri(lTmp,'npol',npol,1)
          i = nFlags
          nFlags = (nant*(nant+1))/2
          if (.not.nofreq) nFlags = nFlags * nchan
          if (.not.nopol) nFlags = nFlags * npol
          if (i.lt.nFlags) then
            if (i.ne.0) call memFrep(pFlag,i,'l')
            call memAllop(pFlag,nFlags,'l')
          else
            nFlags=i
          endif
          init = .false.
        else
          nchan = snchan
          do i=1,4
            ttbp(i,ip,ib) = savetbp(i)
          enddo
          do i=1,nchan
            flags(i,1) = sflags(i)
          enddo
        endif
        do while (more)
	  call uvrdvri(lTmp,'npol',npol,1)
	  do ip=2,npol
	    call uvread(lTmp,ttbp(1,ip,ib),data,flags2,MAXCHAN,nchan)
	    do i=1,nchan
	      if (nopol) then 
                flags(i,1) = flags(i,1).and.flags2(i)
              else 
                flags(i,ip) = flags2(i)
              endif
	    enddo
	  enddo
          if (nopol) npol = 1
          do ip=1,npol
	    if(nchan.eq.0)then
	      nt = 0
	    else if(nofreq)then
	      f = flags(1,ip)
	      do i=2,nchan
	        f = f.and.flags(i,ip)
	      enddo
	      memL(pFlag+off) = f
              off = off + 1
	      nt = 1
	    else
	      do i=1,nchan
	        memL(pFlag+off) = flags(i,ip)  
                off = off + 1                 
	      enddo
	      nt = nchan
	    endif
          enddo
          ib = ib+1
          ip = 1
          call uvread(lTmp,ttbp(1,ip,ib),data,flags,MAXCHAN,nchan1)
          more = nchan1.gt.0.and.nchan1.eq.nchan.and.
     *      abs(ttbp(1,1,ib)-ttbp(1,1,ib-1)).lt.1./86400.and.
     *      nint(ttbp(4,1,ib)-ttbp(4,ib-1,1)).eq.0
          if (off.gt.nFlags) 
     *      call bug('f','Internal flag storage exceeded')
        enddo
        eof = nchan1.le.0
        if (eof) then
          init=.true.
        else
          do i=1,4
            savetbp(i) = ttbp(i,1,ib)
          enddo
          do i=1,nchan1
            sflags(i) = flags(i,ip)
          enddo
          snchan = nchan1
        endif
        nbase = ib -1
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
	
