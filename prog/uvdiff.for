c************************************************************************
	program uvdiff
	implicit none
c
c= uvdiff -- Difference two visibility datasets.
c& rjs
c: uv analysis
c+
c	Given two Miriad visibility datasets, UVDIFF matches and optionally
c	subtracts one from the other. In doing this, UVDIFF assumes that the
c	two datasets sample the same source using the same array configuration
c	and observing frequency setup (no checks are made to verify these).
c	UVDIFF matches visibility records based on baseline number,
c	polarisation type and hour angle. If the hour angles in the two
c	datasets do not match to better than 1 second (which would be unlikely),
c	UVDIFF linearly interpolates between two integrations of one
c	dataset to the appropriate hour angle of the other.
c
c	NOTE: UVDIFF does not apply calibration corrections to either
c	of the input datasets. The datasets should be in time order.
c@ vis
c	The two datasets to difference. No default. When differencing, the
c	output is the first minus the second. The first visibility dataset
c	is used as the template for the output. It is the second dataset
c	that is interpolated when needed.
c@ select
c	Standard visibility selection keyword. See the help on "select" for
c	more information. Note that this selection is applied ONLY to the
c	first visibility dataset. The default is to select everything.
c@ out
c	The output dataset. No default.
c@ mode
c	This determines what data is written. Possibilities values are:
c	  difference Write the difference of the two inputs (the first
c	             minus the second). This is the default.
c	  two        Write the data from the second input, interpolated
c	             to the first.
c	  one        Write the first dataset.
c	With the exception of the visibility data itself, all three
c	possibilities will produce identical datasets. This includes
c	identical flagging information.
c@ tol
c	Interpolation tolerance, in minutes. This gives the maximum
c	gap between two integrations to interpolate across. The default is
c	2 minutes.
c--
c  History:
c    04-jun-96 rjs  Preliminary version.
c
c  Bugs/Shortcomings:
c    * Should handle the conjugate symmetry property, and match data over
c      a wider range of HA.
c    * Handle general number of channels, polarisations, baselines.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
c
	character version*(*)
	integer MAXSELS
	parameter(version='Uvdiff: version 1.0 4-Jun-96')
	parameter(MAXSELS=1000)
	character vis1*64,vis2*64,out*64
	complex data(MAXCHAN),mdata(MAXCHAN)
	logical flags(MAXCHAN),mflags(MAXCHAN)
	real tol,sels(MAXSELS)
	double precision preamble(8),lst
	integer tIn,tOut,nchan,pol,i,npol
c
	integer NMODES
	parameter(NMODES=3)
	character modes(NMODES)*12,mode*12
	integer nout
	data modes/'difference  ','one         ','two         '/
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya('vis',vis1,' ')
	call keya('vis',vis2,' ')
	call keya('out',out,' ')
	call keymatch('mode',NMODES,modes,1,mode,nout)
	if(nout.eq.0)mode = modes(1)
	call keyr('tol',tol,2.0)
	call SelInput('select',sels,MAXSELS)
	call keyfin
	if(vis1.eq.' '.or.vis2.eq.' '.or.out.eq.' ')
     *	  call bug('f','An input or output is missing')
	if(tol.le.0)call bug('f','Invalid interpolation tolerance')
	tol = 2*PI*tol/(24.*60.)
c
c  Open the inputs and outputs.
c
	call uvopen(tIn,vis1,'old')
	call uvset(tIn,'preamble','uvw/time/baseline/pol/ra/lst',
     *							0,0.,0.,0.)
	call SelApply(tIn,sels,.true.)
c
	call BInit(vis2)
	call varInit(tIn,'channel')
	call uvopen(tOut,out,'new')
	call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call hdcopy(tIn,tOut,'history')
	call hisopen(tOut,'append')
	call hiswrite(tOut,'UVDIFF: Miriad '//version)
	call hisinput(tOut,'UVDIFF')
	call hisclose(tOut)
	call varOnit(tIn,tOut,'channel')
c
c  Loop over all the data.
c	
	call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	dowhile(nchan.gt.0)
	  call uvrdvri(tIn,'pol',pol,0)
	  call uvrdvrd(tIn,'lst',lst,0.d0)
c
c  Get the model data.
c
	  call BGet(preamble(5),mdata,mflags,nchan,tol)
c
c  Subtract if required.
c
	  if(mode.eq.'one')then
	    do i=1,nchan
	      mdata(i) = data(i)
	      mflags(i) = flags(i).and.mflags(i)
	    enddo
	  else if(mode.eq.'two')then
	    do i=1,nchan
	      mflags(i) = flags(i).and.mflags(i)
	    enddo
	  else
	    do i=1,nchan
	      mdata(i)  = data(i) - mdata(i)
	      mflags(i) = flags(i).and.mflags(i)
	    enddo
	  endif
c
	  call varCopy(tIn,tOut)
	  call uvrdvri(tIn,'npol',npol,0)
	  call uvputvri(tOut,'pol',pol,1)
	  call uvputvri(tOut,'npol',npol,1)
	  call uvwrite(tOut,preamble,mdata,mflags,nchan)
	  call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call uvclose(tIn)
	call uvclose(tOut)
	call BFin
c
	end
c************************************************************************
	subroutine BGet(vars,rdata,rflags,nchan1,tol)
c
	implicit none
	integer nchan1
	real tol
	double precision vars(4)
	complex rdata(nchan1)
	logical rflags(nchan1)
c
c  Generate the interpolated data.
c------------------------------------------------------------------------
	include 'mirconst.h'
	real mtol
	parameter(mtol=2.0*PI/86400.0)
	include 'uvdiff.h'
	integer bl,ipol,i
	real w1,w2
	double precision mha
c
	call Unpack(vars,mha,bl,ipol)
c
c  Go through the dataset until we find the right integrations.
c
	dowhile(min(abs(mha-ha(1)),abs(mha-ha(2))).gt.mtol.and.
     *		mha.gt.max(ha(1),ha(2)).and.
     *		min(nchan(1),nchan(2)).gt.0)
	  call Bload
	enddo
c
c  Cases of straight copy.
c
	if(abs(mha-ha(1)).lt.mtol.and.nchan1.eq.nchan(1))then
	  do i=1,nchan1
	    rdata(i) = Data(i,ipol,bl,1)
	    rflags(i) = Flags(i,ipol,bl,1)
	  enddo
	else if(abs(mha-ha(2)).lt.mtol.and.nchan1.eq.nchan(2))then
	  do i=1,nchan1
	    rdata(i) = Data(i,ipol,bl,2)
	    rflags(i) = Flags(i,ipol,bl,2)
	  enddo
c
c  Linearly interpolate between the two now.
c
	else if(nchan(1).eq.nchan1.and.nchan(2).eq.nchan1.and.
     *	   (mha-ha(1))*(ha(2)-mha).ge.0.and.abs(ha(2)-ha(1)).le.tol)then
	  w1 = 1 - abs((mha-ha(1))/(ha(2)-ha(1)))
	  w2 = 1 - abs((mha-ha(2))/(ha(2)-ha(1)))
	  do i=1,nchan1
	    rdata(i) = w1*Data(i,ipol,bl,1) + w2*Data(i,ipol,bl,2)
	    rflags(i) = Flags(i,ipol,bl,1).and.Flags(i,ipol,bl,2)
	  enddo
c
c  Case of it all failing.
c
	else
	  do i=1,nchan1
	    rflags(i) = .false.
	    rdata(i)  = 0
	  enddo
	endif
c
	end
c************************************************************************
	subroutine unpack(vars,ha,bl,ipol)
c
	implicit none
	double precision vars(4),ha
	integer bl,ipol
c
c  Unpack variables, in the order baseline,pol,ra,lst
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i1,i2
c
	ipol = - nint(vars(2)) - 4
	call Basant(vars(1),i1,i2)
	bl = ((i2-1)*(i2-2))/2 + i1
	ha = mod(vars(4) - vars(3),2.d0*DPI)
	if(ha.gt.DPI)then
	  ha = ha - 2.d0*DPI
	else if(ha.lt.-DPI)then
	  ha = ha + 2*DPI
	endif
c
	end
c************************************************************************
	subroutine BFin
c
	implicit none
c------------------------------------------------------------------------
	include 'uvdiff.h'
	call uvclose(tno)
	end
c************************************************************************
	subroutine BInit(vis)
c
	implicit none
	character vis*(*)
c------------------------------------------------------------------------
	include 'uvdiff.h'
	integer nbin
c
c  Open the dataset to be used as the template.
c
	call uvopen(tno,vis,'old')
	call uvset(tno,'preamble','baseline/pol/ra/lst',0,0.,0.,0.)
	call uvrdvri(tno,'nbin',nbin,1)
	if(nbin.ne.1)call bug('f','Cannot operate on bin-mode data')
c
c  Get ready to do.
c
	nchan(1) = 1
	nchan(2) = 1
	ha(1) = 0
	ha(2) = 1
	call uvread(tno,pspare,cspare,lspare,MAXCHAN,nspare)
	call BLoad
	ha(2) = ha(1) - 1
	call BLoad
	if(min(nchan(1),nchan(2)).eq.0)
     *		call bug('f','No integrations found')
	end
c************************************************************************
	subroutine Bload
c
	implicit none
c
c  Load the next integration.
c------------------------------------------------------------------------
	include 'uvdiff.h'
	integer ipnt,ipol,bl,i,j,k
	double precision mha
c
c  Determine the polarisation and hour angle of the spare record.
c
	ipnt = 1
	if(ha(1).gt.ha(2))ipnt = 2
	nchan(ipnt) = nspare
	if(min(nchan(1),nchan(2)).le.0)return
	call unpack(pspare,mha,bl,ipol)
	ha(ipnt) = mha
c
c  Initialise the output array.
c
	do k=1,MAXBASE
	  do j=1,MAXPOL
	    do i=1,MAXCHAN
	      flags(i,j,k,ipnt) = .false.
	    enddo
	  enddo
	enddo
c
	dowhile(abs(mha-ha(ipnt)).lt.1e-4.and.
     *		nspare.eq.nchan(ipnt))
c
c  Which output slot does the current record fall into.
c
	  do i=1,nspare
	    data(i,ipol,bl,ipnt) = cspare(i)
	    flags(i,ipol,bl,ipnt) = lspare(i)
	  enddo
c
c  Get another record.
c
	  call uvread(tno,pspare,cspare,lspare,MAXCHAN,nspare)
	  if(nspare.gt.0)call unpack(pspare,mha,bl,ipol)
	enddo
c
	if(abs(mha-ha(ipnt)).lt.1e-4.and.nspare.ne.0)
     *	call bug('f','Number of channels changed within an integration')
c
	end

