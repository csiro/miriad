c************************************************************************
c
c  Routines to simplify copying a uv file.
c
c  VarInit -- Initialise an input file.
c  VarOnit -- Initialise the output file.
c  VarWInit -- Initialise to copy wideband data frequency description
c	       (goes with uvwread and uvwwrite routines).
c  VarCopy -- Copy frequency setup variables to the output file.
c  VarAvAll -- Indicate that all the channels are being averaged into one.
c
c  History:
c    rjs  15jun92 Derived from routines in uvcat/uvaver/uvmodel.
c    rjs  26jun92 Add latitud,longitu.
c    rjs  10jul92 Better channel linetype output. Copy antdiam variable.
c    rjs  21jul92 Eliminated a redundant variable. Better wfreq.
c    rjs   7aug92 Use new uvvar routines (rather than uvcopyvr)
c    mchw 18feb93 Added delay0 to uvvariables.
c    rjs  22feb93 Doc comment changes.
c    rjs  24feb93 Bug in copying systemp in VarChan. Correct some error
c		  nessages.
c    rjs  24aug93 Added VarAvAll.
c    mjs  06sep93 Doc only mod (changed c= to c*).
c    rjs  07sep93 Merge mjs and rjs versions.
c    mchw 10mar94 Added antaz,antel to uvvariables.
c    rjs   6sep94 XYphase fiddles. Use MAXWIN.
c    rjs   9sep94 Support felocity linetype.
c    mchw  5jan95 Added pressmb to uv-variables.
c    rjs  13jan95 Added "bin" to uv-variables.
c    mchw 15mar96 Added delay to uvvariables.
c    rjs  29may96 Added "nbin" to uvvariables.
c    mchw 06aug96 Dimension wideband variables MAXWIDE.
c    rjs  11oct96 Added delra,deldec,pntra,pntdec.
c    rjs  23jul97 Added pbtype,xtsys,ytsys,xsampler,ysampler,xyamp
c    pjt  12oct98 Added cable,jyperka,obsline,project,themt,tsis,tif2
c                 (they are currently all BIMA specific)
c    rjs  16oct98 Reduce the number of continuation lines in above
c		  change to bring it in line with FORTRAN standard.
c
c************************************************************************
c*VarInit -- Initialise the copy routines.
c:uv-data
c& rjs
c+
	subroutine VarInit(tIn,linetype)
c
	implicit none
	character linetype*(*)
	integer tIn
c
c  The VarCpIni routine marks (using uvtrack) a number of variables to be
c  copied across by the uvcopyvr routine.
c  It also marks frequency setup and system temp variables, to note
c  updates to these, so that VarCopy can be used later to update them.
c
c  Typical use would be:
c
c    call VarInit(tIn,linetype)
c    call VarOnit(tIn,tOut,linetype)
c          .
c          .
c          .
c    call uvread(tIn,...)
c	   .
c	   .
c	   .
c    call VarCopy(tIn,tOut)
c    call uvwrite(tOut,...)
c
c  Variables whose characteristics are set by VarOnit are:
c   line=channel or velocity: corr
c   line=wide		    : wcorr
c  Variables NOT set up to be copied by VarInit are:
c    preamble variables:	time, baseline, coord
c    polarisation variables:	npol, pol
c    data variables:		corr, wcorr, nchan, nwide
c  Variables produced by the VarCopy routine:
c   line=channel or velocity: nspect,nschan,ischan,sdf,sfreq,restfreq,systemp
c   line=wide		    : wfreq,wwidth,wsystemp.
c  Variables setup for copying, and copied by VarWInit:
c   wfreq,wwidth,wsystemp
c
c  Input:
c    linetype	Either 'channel', 'wide' or 'velocity'. If this is
c		blank, then uvcopy does not prepare itself to update the
c		frequency setup variables.
c------------------------------------------------------------------------
	integer vhandc,vhandu
	logical avall
	common/VarCom/vhandc,vhandu,avall
c
	integer i
c
	integer nvar,nline,nwide,nvelo
	parameter(nvar=84,nline=8,nwide=3,nvelo=4)
        character var(nvar)*8,line(nline)*8,wide(nwide)*8,velo(nvelo)*8
c
c  Variables to check for a change, for line=channel.
c
	data line/    'nspect  ','restfreq','ischan  ','nschan  ',
     *	   'sfreq   ','sdf     ','systemp ','xyphase '/
c
c  Variables to check for a change, for line=wide.
c
	data wide/    'wfreq   ','wwidth  ','wsystemp'/
c
c  Variables to check for a change, for line=velocity.
c
	data velo/    'restfreq','systemp ','veldop  ','vsource '/
c
c  Variables to copy whenever they change.
c
	data var/     'airtemp ','antaz   ','antdiam ','antel   ',
     *	   'antpos  ','atten   ','axisrms ','bin     ','cable   ',
     *	   'chi     ','corbit  ','corbw   ','corfin  ','cormode ',
     *	   'coropt  ','cortaper','ddec    ','dec     ','deldec  ',
     *	   'delra   ','dewpoint','dra     ','epoch   ','evector ',
     *	   'focus   ','freq    ','freqif  ','inttime ','ivalued ',
     *     'jyperk  ','jyperka ','latitud ','longitu ','lo1     ',
     *	   'lo2     ','lst     ','mount   ','nants   ','nbin    ',
     *	   'ntemp   ','ntpower ','obsdec  ','observer','obsline ',
     *     'obsra   ','on      ','operator','pbfwhm  ','phaselo1',
     *	   'phaselo2','pntdec  ','pntra   ','phasem1 ','plangle ',
     *	   'plmaj   ','plmin   ','pltb    ','precipmm','pressmb ',
     *	   'project ','ra      ','relhumid','source  ','telescop',
     *	   'temp    ','themt   ','tif2    ','tpower  ','tsis    ',
     *	   'ut      ','veldop  ','veltype ','version ','vsource ',
     *	   'winddir ','windmph ','delay   ','delay0  ','xtsys   ',
     *	   'ytsys   ','xsampler','ysampler','xyamp   ','pbtype  '/
c------------------------------------------------------------------------
	avall = .false.
c
c  Copy the variables that have changed.
c
	vhandu = 0
	call uvvarini(tIn,vhandc)
	do i=1,nvar
	  call uvvarset(vhandc,var(i))
	enddo
c
c  Set the variables that we want to watch, to determine changes
c  in the frequency set-up parameters.
c
	if(linetype.ne.' ')call uvvarini(tIn,vhandu)
	if(linetype.eq.' ')then
	  continue
	else if(linetype.eq.'channel')then
	  do i=1,nline
	    call uvvarset(vhandu,line(i))
	  enddo	  
	else if(linetype.eq.'wide')then
	  do i=1,nwide
	    call uvvarset(vhandu,wide(i))
	  enddo
	else if(linetype.eq.'velocity'.or.linetype.eq.'felocity')then
	  do i=1,nvelo
	    call uvvarset(vhandu,velo(i))
	  enddo
	else
	  call bug('f','Unrecognised linetype, in VarInit')
	endif	
	end
c************************************************************************
c*VarOnit -- Initialise the output when copying variables.
c:uv-data
c& rjs
c+
	subroutine VarOnit(tIn,tOut,linetype)
c
	implicit none
	integer tIn,tOut
	character linetype*(*)
c
c  Initialise characteristics about the output data variables.
c
c  Variables whose characteristics are set by VarOnit are:
c   line=channel or velocity: corr
c   line=wide		    : wcorr
c
c  Input:
c    tIn
c    tOut
c    linetype
c------------------------------------------------------------------------
	character type*1
	integer length
	logical updated
c
	if(linetype.ne.'wide')then
	  call uvprobvr(tIn,'corr',type,length,updated)
	  call uvset(tOut,'corr',type,0,0.,0.,0.)
	else
	  call uvset(tOut,'data','wide',0,1.,1.,1.)
	endif
	end
c************************************************************************
c*VarWInit -- Initialise the input to copy wide channel parameters.
c:uv-data
c& rjs
c+
	subroutine VarWInit(tIn)
c
	implicit none
	integer tIn
c
c  Setup the frequency description of the wide variables to be copied.
c
c  Input:
c    tIn	Currently ignored.
c------------------------------------------------------------------------
	integer vhandc,vhandu
	logical avall
	common/VarCom/vhandc,vhandu,avall
c
	integer nwide
	parameter(nwide=3)
	character wide(nwide)*8
	integer i
c
c  Variables to check for a change, for line=wide.
c
	data wide/    'wfreq   ','wwidth  ','wsystemp'/
c
	do i=1,nwide
	  call uvvarset(vhandc,wide(i))
	enddo
	end
c************************************************************************
c*VarCopy -- Write out variables describing an output linetype.
c:uv-data
c& rjs
c+
	subroutine VarCopy(tIn,tOut)
c
	implicit none
	integer tIn,tOut
c
c  Copy across a description of the line.
c
c  Input:
c    tIn	Handle of the input uv file.
c    tOut	Handle of the output uv file.
c
c  UV variables produced are:
c   line=channel or velocity: nspect,nschan,ischan,sdf,sfreq,restfreq,systemp
c   line=wide		    : wfreq,wwidth,wsystemp.
c
c
c------------------------------------------------------------------------
	integer vhandc,vhandu
	logical avall
	common/VarCom/vhandc,vhandu,avall
c
	integer LINE,WIDE,VELO
	parameter(LINE=1,WIDE=2,VELO=3)
	integer TYPE,N,START,WIDTH,STEP,WIN
	parameter(TYPE=1,N=2,START=3,WIDTH=4,STEP=5,WIN=6)
	double precision data(6)
c
c  Externals.
c
	logical uvvarupd
c
c  Copy across the variables that have changed. Also check the frequency
c  description variables. If they have not changed, return straight away.
c
	call uvvarcpy(vhandc,tOut)
	if(vhandu.eq.0)return
	if(.not.uvvarupd(vhandu))return
c
c  Things have changed, so update things.
c
	call uvinfo(tIn,'line',data)
c
	if(data(TYPE).eq.LINE)then
	  call VarChan(tIn,tOut,nint(data(START)),nint(data(WIDTH)),
     *	    nint(data(STEP)),nint(data(N)),avall)
	else if(data(TYPE).eq.WIDE)then
	  call VarWide(tIn,tOut,nint(data(START)),nint(data(WIDTH)),
     *	    nint(data(STEP)),nint(data(N)),avall)
	else if(data(TYPE).eq.VELO)then
	  call VarVelo(tIn,tOut,real(data(START)),
     *	    real(data(STEP)),nint(data(N)),nint(data(WIN)),avall)
	else
	  call bug('f','Unrecognised linetype, in VarCopy')
	endif
	end
c************************************************************************
	subroutine VarWide(tvis,tout,lstart,lwidth,lstep,nchan,avall)
c
	implicit none
	integer tvis,tout,lstart,lwidth,lstep,nchan
	logical avall
c
c  Calculate the frequency setup and systemp variables for wide linetype
c  copying.
c
c  Input:
c    tvis	Handle of the input file.
c    tout	Handle of the output file.
c    lstart	Start wide channel of interest.
c    lwidth	Width to average over.
c    lstep	Step between wide channels.
c    nchan	Number of wide channels of interest.
c    avall
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	logical uwfreq,uwwidth,uwtsys
	integer nwide,nants,length,i,j,i0,j0,k
	character type*1
	real wfreq(MAXWIDE),owfreq(MAXWIDE),wwidth(MAXWIDE)
	real owwidth(MAXWIDE)
	real wtsys(MAXWIDE*MAXANT),owtsys(MAXWIDE*MAXANT)
c
	call uvprobvr(tvis,'wfreq',type,nwide,uwfreq)
	if(type.ne.'r') return
	call uvprobvr(tvis,'wwidth',type,nwide,uwwidth)
	if(type.ne.'r') return
c
c  Update them if necessary.
c
	if(nwide.le.MAXWIDE)then
	  call uvgetvrr(tvis,'wfreq',wfreq,nwide)
	  call uvgetvrr(tvis,'wwidth',wwidth,nwide)
	  do j=1,nchan
	    i0 = lstart + (j-1)*lstep
	    owfreq(j) = 0
	    owwidth(j) = 0
	    do i=1,lwidth
	      owfreq(j) = owfreq(j) + wfreq(i0)*wwidth(i0)
	      owwidth(j) = owwidth(j) + wwidth(i0)
	      i0 = i0 + 1
	    enddo
	    owfreq(j) = owfreq(j) / owwidth(j)
	  enddo
	  if(avall.and.nchan.gt.1)then
	    do i=2,nchan
	      owfreq(1) = owfreq(1) + owfreq(i)
	      owwidth(1) = owwidth(1) + owwidth(i)
	    enddo
	    owfreq(1) = owfreq(1) / nchan
	    call uvputvrr(tout,'wfreq',owfreq,1)
	    call uvputvrr(tout,'wwidth',owwidth,1)
	  else
	    call uvputvrr(tout,'wfreq',owfreq,nchan)
	    call uvputvrr(tout,'wwidth',owwidth,nchan)
	  endif
	endif
c
c  Update the system temperature, if needed.
c
	call uvprobvr(tvis,'wsystemp',type,length,uwtsys)
	if(length.gt.MAXWIDE*MAXANT)call bug('f',
     *	  'Too many wideband channels or antennae, in VarWide')
	nants = length / nwide
	if(type.ne.'r'.or.nants.le.0) return
	call uvgetvrr(tvis,'wsystemp',wtsys,length)
c
	if(avall)then
	  do k=1,nants
	    owtsys(k) = 0
	    do j=1,nchan
	      i0 = ((lstart-1) + (j-1)*lstep) * nants + k
	      do i=1,lwidth
	        owtsys(k) = owtsys(k) + wtsys(i0)
		i0 = i0 + nants
	      enddo
	    enddo
	    owtsys(k) = owtsys(k) / (lwidth*nchan)
	  enddo
	  call uvputvrr(tout,'wsystemp',owtsys,nants)   
	else
	  j0 = 1
	  do j=1,nchan
	    do k=1,nants
	      owtsys(j0) = 0
	      i0 = ((lstart-1) + (j-1)*lstep) * nants + k
	      do i=1,lwidth
	        owtsys(j0) = owtsys(j0) + wtsys(i0)
	        i0 = i0 + nants
	      enddo
	      owtsys(j0) = owtsys(j0) / lwidth
	      j0 = j0 + 1
	    enddo
	  enddo
	  call uvputvrr(tout,'wsystemp',owtsys,nants*nchan)
	endif

	end
c************************************************************************
	subroutine VarChan(tvis,tout,lstart,lwidth,lstep,nchan,avall)
c
	implicit none
	integer tvis,tout,nchan,lstart,lwidth,lstep
	logical avall
c
c  Calculate the frequency setup and systemp variables for channel linetype
c  copying.
c
c  The variables affected by this are -
c    Channel linetype:	
c    nspect		
c    nschan
c    ischan	
c    sdf					  
c    sfreq					
c    restfreq		
c    systemp		
c
c  Inputs:
c    tvis	Handle of the input uv data file.
c    tout	Handle of the output uv data file.
c    newline	True if we must recast the window variables.
c    lstart	First channel used in linetype calculations.
c    lwidth	Width to average over.
c    lstep	Step between channel averages.
c    nchan	Number of channels.
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	real xyphase(MAXANT*MAXWIN),xyphase0(MAXANT*MAXWIN)
	real systemp(MAXANT*MAXWIN),systemp0(MAXANT*MAXWIN)
	double precision rfreq(MAXWIN),sdf(MAXWIN)
	double precision rfreq0(MAXWIN),sdf0(MAXWIN)
	double precision sfreq0(MAXWIN),sfreq(MAXWIN)
	integer ischan0(MAXWIN),nschan0(MAXWIN)
	integer nschan(MAXWIN),trn(MAXWIN)
	integer ispect,ospect,nspect,n,i,j,k,l,nants,start
	integer nsystemp,nxyphase
	character type*1
	logical upd
c
c  Get the various window-related variables from the uvdata.
c
	call uvrdvri(tVis,'nspect',nspect,1)
	if(nspect.le.0)
     *	  call bug('f','Bad value for uv variable nspect in VarChan')
	if(nspect.gt.MAXWIN)
     *	  call bug('f','nspect .gt. MAXWIN, in VarChan')
c
	call uvgetvri(tVis,'nschan',nschan,nspect)
	call uvgetvrd(tVis,'sdf',sdf,nspect)
	call uvgetvrd(tVis,'sfreq',sfreq,nspect)
	call uvgetvrd(tVis,'restfreq',rfreq,nspect)
	call uvrdvri(tVis,'nants',nants,0)
c
c  Generate the window description parameters for the output.
c
	n = nchan
	ispect = 1
	ospect = 0
	start = lstart
	dowhile(n.gt.0)
	  dowhile(start.gt.nschan(ispect))
	    start = start - nschan(ispect)
	    ispect = ispect + 1
	  enddo
	  ospect = ospect + 1
	  sfreq0(ospect) = sfreq(ispect) +
     *		(start-1)*sdf(ispect) + 0.5*(lwidth-1)*sdf(ispect)
	  nschan0(ospect) = min((nschan(ispect)-start)/lstep + 1,n)
	  rfreq0(ospect) = rfreq(ispect)
	  trn(ospect) = ispect
	  if(nschan0(ospect).eq.1)then
	    sdf0(ospect) = lwidth*sdf(ispect)
	  else
	    sdf0(ospect) = lstep*sdf(ispect)
	  endif
	  n = n - nschan0(ospect)
	  start = start + lstep*nschan0(ospect)
	enddo
c
c  Handle the system temperature.
c
	call uvprobvr(tVis,'systemp',type,nsystemp,upd)
	upd = type.eq.'r'.and.
     *	  nsystemp.le.MAXANT*MAXWIN.and.nsystemp.ge.1
	if(upd)then
	  call uvgetvrr(tVis,'systemp',systemp,nsystemp)
	  if(nants.eq.0)then
	    nsystemp = 1
	  else if(nsystemp.lt.nants*nspect)then
	    nsystemp = min(nsystemp,nants)
	    do i=1,nsystemp
	      systemp0(i) = systemp(i)
	    enddo
	  else
	    nsystemp = nants*ospect
	    k = 0
	    do j=1,ospect
	      l = nants*(trn(j)-1)
	      do i=1,nants
		k = k + 1
		systemp0(k) = systemp(i+l)
	      enddo
	    enddo
	  endif
	else
	  nsystemp = 0
	endif
c
c  Handle the xyphase.
c
	call uvprobvr(tVis,'xyphase',type,nxyphase,upd)
	upd = type.eq.'r'.and.nants.gt.0.and.
     *	  nxyphase.le.MAXANT*MAXWIN.and.nxyphase.ge.1
	if(upd)then
	  call uvgetvrr(tVis,'xyphase',xyphase,nxyphase)
	  if(nxyphase.lt.nants*nspect)then
	    nxyphase = min(nxyphase,nants)
	    do i=1,nxyphase
	      xyphase0(i) = xyphase(i)
	    enddo
	  else
	    nxyphase = nants*ospect
	    k = 0
	    do j=1,ospect
	      l = nants*(trn(j)-1)
	      do i=1,nants
		k = k + 1
		xyphase0(k) = xyphase(i+l)
	      enddo
	    enddo
	  endif
	else
	  nxyphase = 0
	endif
c
c  Form an average, if required.
c
	if(avall)then
	  sfreq0(1) = nschan0(1) *
     *		      ( sfreq0(1) + 0.5*(nschan0(1)-1)*sdf(1) ) / nchan
	  sdf0(1) = nschan0(1) * sdf0(1)
	  do i=2,ospect
	    sfreq0(1) = sfreq0(1) + nschan0(i) *
     *		      ( sfreq0(i) + 0.5*(nschan0(i)-1)*sdf(i) ) / nchan
	    sdf0(1) = sdf0(1) + nschan0(i)*sdf0(i)
	  enddo
	  ospect = 1
	  nschan0(1) = 1
	  nsystemp = min(nsystemp,nants)
	  nxyphase = min(nxyphase,nants)
	endif
c
c  Generate the ischan array.
c
	ischan0(1) = 1
	do i=2,ospect
	  ischan0(i) = ischan0(i-1) + nschan0(i-1)
	enddo
c
c  Now write out all the goodies.
c
	call uvputvri(tOut,'nspect',ospect,1)
	call uvputvri(tOut,'nschan',nschan0,ospect)
	call uvputvri(tOut,'ischan',ischan0,ospect)
	call uvputvrd(tOut,'sdf',sdf0,ospect)
	call uvputvrd(tOut,'sfreq',sfreq0,ospect)
	call uvputvrd(tOut,'restfreq',rfreq0,ospect)
c
	if(nsystemp.gt.0)
     *	  call uvputvrr(tOut,'systemp',systemp0,nsystemp)
	if(nxyphase.gt.0)
     *	  call uvputvrr(tOut,'xyphase',xyphase0,nxyphase)
c
	end
c************************************************************************
	subroutine VarVelo(tvis,tout,lstart,lstep,nchan,win,avall)
c
	implicit none
	integer tvis,tout,nchan,win
	real lstart,lstep
	logical avall
c
c  Calculate the frequency setup and systemp variables for velocity linetype
c  copying.
c  This cannot be done strictly correctly, so this is only a fudge.
c
c  Input:
c    tvis	Handle of the input file
c    tout	Handle of the output file.
c    lstart	First velocity to form, in km/sec
c    lstep	Step between velocities, in km/sec
c    nchan	Number of velocity channels.
c    win	First window used to form velocity channels.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
c
	double precision restfreq,dfreq,sdf,sfreq
	real vsource,veldop
c
	integer i
	real systemp(MAXANT*MAXWIN)
	character type*1
	logical usyst
	integer nsystemp,nants,nspect
c
c  Get info from the input file, and compute sfreq and sdf.
c
	call uvfit1(tvis,'restfreq',nchan,restfreq,dfreq)
	call uvrdvrr(tvis,'vsource',vsource,0.)
	call uvrdvrr(tvis,'veldop',veldop,0.)
	sfreq = restfreq*(1.d0-dble(1000*(lstart+veldop-vsource)/cmks))
	sdf = -restfreq*1000*lstep/cmks
	if(avall)then
	  sfreq = sfreq + 0.5 * (nchan-1) * sdf
	  sdf = nchan*sdf
	endif
c
c  Write info to the output file.
c
	call uvputvri(tout,'nspect',1,1)
	call uvputvri(tout,'ischan',1,1)
	if(avall)then
	  call uvputvri(tout,'nschan',1,1)
	else
	  call uvputvri(tout,'nschan',nchan,1)
	endif
	call uvputvrd(tout,'restfreq',restfreq,1)
	call uvputvrr(tout,'vsource',vsource,1)
	call uvputvrr(tout,'veldop',veldop,1)
	call uvputvrd(tout,'sdf',sdf,1)
	call uvputvrd(tout,'sfreq',sfreq,1)
c
c  Handle the system temperature. These are the temperatures of the
c  first window which is used in forming the velocity.
c
	i = win
	call uvprobvr(tVis,'systemp',type,nsystemp,usyst)
	usyst = type.eq.'r'.and.nsystemp.lt.MAXANT*MAXWIN
	if(usyst)then
	  call uvgetvrr(tVis,'systemp',systemp,nsystemp)
	  call uvrdvri(tVis,'nspect',nspect,0)
	  call uvrdvri(tVis,'nants',nants,0)
	  if(nsystemp.eq.nants*nspect)then
	    i = (i-1)*nants + 1
	    nsystemp = nants
	  else
	    i = 1
	    usyst = nsystemp.eq.1.or.nsystemp.eq.nants
	  endif
	endif
c
	if(usyst)call uvputvrr(tOut,'systemp',systemp(i),nsystemp)
c
	end
c************************************************************************
c*VarAvAll -- Indicate that all the output channels are averaged together.
c:uv-data
c& rjs
c+
	subroutine VarAvAll(tvis,doav)
c
	implicit none
	integer tvis
	logical doav
c
c  Control whether the output channels are deemed to be averaged together.
c
c  Input:
c    tvis	Handle of the input data-set.
c--
c------------------------------------------------------------------------
	integer vhandc,vhandu
	logical avall
	common/VarCom/vhandc,vhandu,avall
c
	avall = doav
	end
