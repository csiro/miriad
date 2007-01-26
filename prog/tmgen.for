c************************************************************************
	program tmgen
	implicit none
c
c= tmgen - Simulate a SMA polarimetric observation.
c& rjs
c: uv analysis, map making
c+
c	tmgen forms a visibility dataset which has some of the characteristics
c	of an SMA polarimetric dataset. In particular it can optionally produce
c	time-multiplexed polarization products and two-part, Nasmyth-like leakages.
c
c@ vis
c	The input visibility dataset. No default. NOTE: For a number of the
c	steps performed by this task, the input visibility dataset must 
c	have four simultaneous polarization correlations (e.g. XX, YY, XY, YX
c	or RR, LL, RL, LR).
c@ out
c	The output visibility dataset. No default.
c@ polar
c	Polarization patterns for generating time shared polarization data. 
c	Up to MAXPOLAR=100 strings of the characters R and L, or X and Y, 
c	to represent the polarization of each antenna
c	R, L (right and left circular polarization), X and Y (linear
c	polarization).
c	E.g. for 3 antennas, the polar=LLL,LRR,RRL,RLR cycles
c	through all combinations of LCP and RCP for each baseline every
c	4 integrations. The default is to not subset the data based
c	on polarization.
c@ leakage
c	Simulate polarimetric leakage. This parameter gives the leakage magnitude
c	as a percent. Random leakages with this rms value are generated and
c	applied to the data. 
c@ options
c	Extra processing options.
c	  nasmyth   Add an extra rotation to the polarisations that would result
c	            from SMA-style Nasmyth receiver placement.
c--
c  History:
c    02jan07 rjs  Original version.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	double precision tol
	character version*(*)
	integer MAXPOLAR,MAXPOL
	parameter(version = 'tmgen: version 1.0 02-Jan-07')
	parameter(MAXPOLAR=100,MAXPOL=4)
	parameter(tol=1.d0/86400.d0)
c
	character vis*80,out*80,ltype*16
	integer tIn,tOut,lxp,nchan,ipol,npolar,pold,i1,i2,nc
	integer pol(MAXPOL),npol,nants,i,iXX,iXY,iYX,iYY
	double precision preamble(6),tprev,antel
	complex data(MAXCHAN,MAXPOL),D(2,MAXANT)
	logical flags(MAXCHAN,MAXPOL),dopolar,donasm,doleak,circ
	character polar(MAXPOLAR)*27,xpolar*27,line*80
	real leakrms,chi,chi1,chi2
c
c  Externals.
c
	integer polsP2C,len1
	character itoaf*8
c
c  Get command line arguments.
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	if(vis.eq.' ')
     *	  call bug('f','An input visibility file must be given')
	call keya('out',out,' ')
	if(out.eq.' ')
     *	  call bug('f','An output visibility file must be given')
	call mkeya('polar',polar,MAXPOLAR,npolar)
	dopolar = npolar.gt.0
	call keyr('leakage',leakrms,0.0)
	doleak = leakrms.gt.0
	leakrms = 0.01*leakrms
	call getopt(donasm)
	call keyfin
c
c  Process the files.
c
	call uvopen(tIn,vis,'old')
	call uvset(tIn,'preamble','uvw/time/baseline/pol',0,0.,0.,0.)
	call defline(tIn,ltype)
	call varInit(tIn,ltype)
c
	call uvopen(tOut,out,'new')
	call varOnit(tIn,tOut,ltype)
	call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	if(dopolar)then
	  call uvputvri(tOut,'npol',1,1)
	  call wrhdi(tOut,'npol',1)
	endif
	call hdcopy(tIn,tOut,'history')
	call hisopen(tOut,'append')
	call hiswrite(tOut,'TMGEN: Miriad '//version)
	call hisinput(tOut,'TMGEN')
c
c  Process the data.
c
	nc = 0
	ipol = 0
	tprev = 0
	call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	if(nchan.eq.0)call bug('f','No data found')
c
c  Generate leakages.
c
	if(doleak)then
	  call uvrdvri(tIn,'nants',nants,0)
	  if(nants.le.0.or.nants.gt.MAXANT)call bug('f',
     *	    'Invalid number of antennas within the input dataset')
	  call gaus(D,4*nants)
	  D(1,1) = (0.,0.)
	  do i=1,nants
	    D(1,i) = leakrms * D(1,i)
	    D(2,i) = (0.,0.)
	  enddo
	  call output(' ')
	  call output('Generated leakage terms:')
	  call output('------------------------')
	  call hiswrite(tOut,'TMGEN: Generated leakage terms:')
	  do i=1,nants
	    write(line,'(1x,a,i2,a,f6.3,a,f6.3,a,f6.3,a,f6.3,a)')
     *	      'Ant',i,':Da,Db = (',real(D(1,i)),',',aimag(D(1,i)),'),(',
     *				   real(D(2,i)),',',aimag(D(2,i)),')'
	    call output(line)
	    call hiswrite(tOut,'TMGEN: '//line)
	  enddo
	endif
c
	dowhile(nchan.gt.0)
	  call uvrdvri(tIn,'npol',npol,1)
	  if(npol.gt.MAXPOL)call bug('f','Too many polarisations')
	  pol(1) = nint(preamble(6))
	  do i=2,npol
	    call uvread(tIn,preamble,data(1,i),flags(1,i),MAXCHAN,nchan)
	    pol(i) = nint(preamble(6))
	  enddo
	  call varCopy(tIn,tOut)
	  call basant(preamble(5),i1,i2)
c
c  Do leakage and rotation.
c
	  if(doleak.or.donasm)then
	    call crack(npol,pol,circ,iXX,iYY,iXY,iYX)
	    do i=1,nchan
	      flags(i,1) = flags(i,1).and.flags(i,2).and.
     *			   flags(i,3).and.flags(i,4)
	      flags(i,2) = flags(i,1)
	      flags(i,3) = flags(i,1)
	      flags(i,4) = flags(i,1)
	    enddo
	  endif
c
	  if(donasm)then
	    call uvgetvrr(tIn,'chi',chi1,1)
	    call uvgetvrd(tIn,'antel',antel,1)
	    chi2 = -PI/180.0 * antel
	chi2 = 2*PI*mod(preamble(4),1.d0)
	    call rotate(circ,data,nchan,MAXCHAN,
     *				npol,iXX,iYY,iXY,iYX,chi2)
	    chi = chi1 + chi2
	    call uvputvrr(tOut,'chi',chi,1)
	    call uvputvrr(tOut,'chi2',chi2,1)
	  endif
	  if(doleak)call leak(data,nchan,MAXCHAN,npol,iXX,iYY,iXY,iYX,
     *						D(1,i1),D(1,i2))
c
	  if(dopolar)then
	    if(abs(tprev-preamble(4)).gt.tol)then
	      ipol = mod(ipol,npolar) + 1
	      xpolar = polar(ipol)
	      lxp = len1(xpolar)
	      tprev = preamble(4)
	    endif
	    if(i2.gt.lxp)call bug('f','polar string too short')
	    pold = polsP2C(xpolar(i1:i1)//xpolar(i2:i2))
	  else
	    call uvputvri(tOut,'npol',npol,1)
	  endif
c
	  do i=1,npol
	    if(pol(i).eq.pold.or..not.dopolar)then
	      call uvputvri(tOut,'pol',pol(i),1)
	      call uvwrite(tOut,preamble,data(1,i),flags(1,i),nchan)
	      nc = nc + 1
	    endif
	  enddo
	  call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
c  Check all happened successfully.
c
	call output('Visibility records written: '//itoaf(nc))
	if(nc.eq.0)call bug('f','No data were written')
c
	call hisclose(tOut)
	call uvclose(tOut)
	call uvclose(tIn)
c
	end
c************************************************************************
	subroutine defline(tIn,line)
c
	implicit none
	integer tIn
	character line*(*)
c------------------------------------------------------------------------
	integer length
	character type*1
	logical update
c
	call uvprobvr(tIn,'corr',type,length,update)
	if(type.eq.'j'.or.type.eq.'r'.or.type.eq.'c')then
	  line = 'channel'
	else
	  line = 'wide'
	endif
	end
c************************************************************************
	subroutine rotate(circ,data,nchan,mchan,npol,
     *					iXX,iYY,iXY,iYX,chi)
c
	implicit none
	integer nchan,mchan,npol,iXX,iYY,iXY,iYX
	complex data(mchan,npol)
	logical circ
	real chi
c
c------------------------------------------------------------------------
	real c,s,cc,ss,cs,c2,s2
	complex vaa,vbb,vab,vba
	integer i
c
	c = cos(chi)
	s = sin(chi)
	cc = c*c
	ss = s*s
	cs = c*s
	c2 = cc - ss
	s2 = 2*cs
c
	do i=1,nchan
	  if(circ)then
	    data(i,iXY) = data(i,iXY)*cmplx(c2, s2)
	    data(i,iYX) = data(i,iYX)*cmplx(c2,-s2)
	  else
	    vaa = data(i,iXX)
	    vbb = data(i,iYY)
	    vab = data(i,iXY)
	    vba = data(i,iYX)
	    data(i,iXX) = cc*vaa + cs*(vab+vba) + ss*vbb
	    data(i,iYY) = cc*vbb - cs*(vab+vba) + ss*vaa
	    data(i,iXY) = cs*(vbb-vaa) + cc*vab - ss*vba
	    data(i,iYX) = cs*(vbb-vaa) - ss*vab + cc*vba
	  endif
	enddo
c
	end
c************************************************************************
	subroutine leak(data,nchan,mchan,npol,iXX,iYY,iXY,iYX,D1,D2)
c
	implicit none
	integer nchan,mchan,npol,iXX,iYY,iXY,iYX
	complex data(mchan,npol),D1(2),D2(2)
c
c------------------------------------------------------------------------
	integer A,B
	parameter(A=1,B=2)
	complex vaa,vbb,vab,vba
	integer i
c
	do i=1,nchan
	  vaa = data(i,iXX)
	  vbb = data(i,iYY)
	  vab = data(i,iXY)
	  vba = data(i,iYX)
	  data(i,iXX) = vaa + D1(A)*vba + conjg(D2(A))*vab + 
     *		              D1(A)     * conjg(D2(A))*vbb
	  data(i,iXY) = vab + D1(A)*vbb + conjg(D2(B))*vaa + 
     *		              D1(A)     * conjg(D2(B))*vba
	  data(i,iYX) = vba + D1(B)*vaa + conjg(D2(A))*vbb + 
     *			      D1(B)     * conjg(D2(A))*vab
	  data(i,iYY) = vbb + D1(B)*vab + conjg(D2(B))*vba + 
     *			      D1(B)     * conjg(D2(B))*vaa
	enddo
c
	end
c************************************************************************
	subroutine getopt(donasm)
c
	implicit none
	logical donasm
c
c  Get extra processing options.
c
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
c
	character opts(NOPTS)*8
	logical present(NOPTS)
c
	data opts/'nasmyth '/
c
	call options('options',opts,present,NOPTS)
	donasm = present(1)
	end
c************************************************************************
	subroutine crack(npol,pol,circ,iXX,iYY,iXY,iYX)
c
	implicit none
	integer npol,pol(npol),iXX,iYY,iXY,iYX
	logical circ
c
c  Determine the indices of the polarisations.
c
c  Input:
c    npol
c    pol
c  Output:
c    circ
c    iXX,iYY,iXY,iYX
c------------------------------------------------------------------------
	integer idx(4),poff,p,i
c
	if(npol.ne.4)call bug('f','Invalid number of polarisations')
	poff = -((-pol(1)-1)/4)*4
	if(poff.ne.0.and.poff.ne.-4)call bug('f',
     *		'Data neither linear or circular correlations')
	circ = poff.eq.0
c
	do i=1,npol
	  p = -pol(i) + poff
	  if(p.lt.1.or.p.gt.4)call bug('f','Invalid polarisations')
	  idx(p) = i
	enddo
	iXX = idx(1)
	iYY = idx(2)
	iXY = idx(3)
	iYX = idx(4)
c
	end

