c************************************************************************
	program mfflat
	implicit none
c
c= mfflat - Flatten the spectral variation of a visibility data-set.
c& rjs
c: calibration
c+
c	MFFLAT is a MIRIAD task which flattens the spectral variation of a
c	visibility data-set. This is used in multi-frequency synthesis,
c	where it is desirable to eliminate the dominant spectral variation
c	from the data (i.e. flatten the spectral variation), and so reduce
c	the spectral artifacts in the mapping and deconvolution process.
c	MFFLAT works by modifying the calibration tables of the input
c	data-sets.
c@ vis
c	Names of the input visibility data-sets. Several can be given.
c	Wildcard expansion is supported. On output, the calibration tables
c	of the data-set have been modified in such a way as to eliminate
c	the dominant spectral variation.
c@ model
c	The input model. This must be MFS image, consisting of an intensity
c	and a scaled-derivative plane. This is analysed to determine the
c	dominant spectral index.
c--
c
c  History:
c    rjs   4aug93  Original version.
c------------------------------------------------------------------------
	character version*(*)
	integer MAXIN
	parameter(version='MfFlat: version 1.0 4-Aug-93')
	parameter(MAXIN=32)
	include 'maxdim.h'
c
	character vis(MAXIN)*64,model*64,string*32
	integer nIn,lVis,i
	real spindx
	double precision freq0
c
c  Externals.
c
	logical hdprsnt
c
c  Get the inputs.
c
	call output(version)
	call keyini
	call mkeyf('vis',vis,MAXIN,nIn)
	call keya('model',model,' ')
	call keyfin
c
c  Check the inputs.
c
	if(model.eq.' ')
     *	  call bug('f','An input model data-set must be given')
	if(nIn.le.0)
     *	  call bug('f','Input visibility data-sets must be given')
c
c  Determine the dominant spectral index.
c
	call DomSpec(model,spindx,freq0)
c
c  Process each of the input visibility data-sets.
c
	do i=1,nIn
	  call uvopen(lVis,vis(i),'old')
c
	  if(hdprsnt(lVis,'gains'))then
	    call WithGain(lVis,spindx,freq0)
	  else
	    call NoGain(lVis,spindx,freq0)
	  endif
c
	  call HisOpen(lVis,'append')
	  call HisWrite(lVis,'MFFLAT: Miriad '//version)
	  call HisInput(lVis,'MFFLAT')
	  write(string,'(f7.2)')spindx
	  call HisWrite(lVis,
     *	    'MFFLAT: Applied a spectral index of'//string)
	  call HisClose(lVis)
c
	  call uvclose(lVis)
	enddo
c
	end
c************************************************************************
	subroutine WithGain(lVis,spindx,freq0)
c
	implicit none
	integer lVis
	real spindx
	double precision freq0
c
c  Fiddle the gains of a data-set that already has a gains table.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
c
	double precision freq
	integer pGains,pTime,lGain
	integer ngains,ntau,nsols,nfeeds,nants,iostat
c
c  Externals.
c
	integer hsize
c
	call haccess(lVis,lGain,'gains','append',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening gains table')
	  call bugno('f',iostat)
	endif
c
	call rdhdi(lVis,'ntau',ntau,0)
	call rdhdi(lVis,'ngains',ngains,0)
	call rdhdi(lVis,'nfeeds',nfeeds,1)
	call rdhdd(lVis,'freq0',freq,freq0)
	nants = ngains/(nfeeds+ntau)
	nsols = hsize(lGain)/(8*(ngains+1))
c
c  Allocate some memory for the gain table.
c
	call MemAlloc(pGains,nsols*(nfeeds+1)*nants,'c')
	call MemAlloc(pTime,nsols,'d')
c
c  Load the gains, adjust them, then write them.
c
	call GainGet(lGain,nsols,nfeeds,nants,ntau,
     *				memC(pGains),memD(pTime))
	call GainAdj(nsols,nfeeds,nants,spindx,freq,freq0,
     *				memC(pGains))
	call GainPut(lGain,nsols,nfeeds,nants,
     *				memC(pGains),memD(pTime))
c
c  Finish up.
c
	call MemFree(pGains,nsols*(nfeeds+1)*nants,'c')
	call MemFree(pTime,nsols,'d')
c
	call wrhdi(lVis,'ntau',1)
	call wrhdi(lVis,'ngains',nants*(nfeeds+1))
	call hdaccess(lGain,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing gains table')
	  call bugno('f',iostat)
	endif
c
	end
c************************************************************************
	subroutine GainGet(lGain,nsols,nfeeds,nants,ntau,
     *					Gains,Time)
c
	implicit none
	integer lGain,nsols,nfeeds,nants,ntau
	complex Gains(nfeeds+1,nants,nsols)
	double precision time(nsols)
c
c  Load the antenna gains table.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer off,iostat,ngains,i,j,j0,k
	complex g(2*MAXANT)
c
	ngains = nants*(nfeeds+ntau)
	off = 8
	do i=1,nsols
	  call hreadd(lGain,time(i),off,8,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error reading gains table')
	    call bugno('f',iostat)
	  endif
	  off = off + 8
	  if(ntau.eq.1)then
	    call hreadr(lGain,Gains(1,1,i),off,8*ngains,iostat)
	  else
	    call hreadr(lGain,G,off,8*ngains,iostat)
	  endif
	  if(iostat.ne.0)then
	    call bug('w','Error writing gains table')
	    call bugno('f',iostat)
	  endif
	  if(ntau.ne.1)then
	    do k=1,nfeeds
	      j0 = k
	      do j=1,nants
	        Gains(k,j,i) = G(j0)
	        Gains(nfeeds+1,j,i) = 0
	        j0 = j0 + nfeeds
	      enddo
	    enddo
	  endif
	  off = off + 8*ngains
	enddo
	end
c************************************************************************
	subroutine GainAdj(nsols,nfeeds,nants,spindx,freq,freq0,Gains)
c
	implicit none
	integer nsols,nfeeds,nants
	complex Gains(nfeeds+1,nants,nsols)
	real spindx
	double precision freq,freq0
c
c  Adjust the antenna gains table for a spectral index.
c  Each "antenna factor" consists of 1 gain for each feed of each antenna.
c				     1 frequency dependent term. The real
c				       part of this is a spectral index
c				       correction factor.
c  To correct the "antenna gain", we adjust the gain so that the overall
c  adjustment is nothing at f=freq0. Because the reference frequency
c  (freq) may differ from freq0, we also scale the gains.
c------------------------------------------------------------------------
	real factor
	integer i,j
c
	factor = (freq/freq0)**(-0.5*spindx)
	do j=1,nsols
	  do i=1,nants
	    Gains(1,i,j) = factor*Gains(1,i,j)
	    if(nfeeds.eq.2)Gains(2,i,j) = factor*Gains(1,i,j)
	    Gains(nfeeds+1,i,j) = Gains(nfeeds+1,i,j) - 0.5*spindx
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine GainPut(lGain,nsols,nfeeds,nants,
     *					Gains,Time)
c
	implicit none
	integer lGain,nsols,nfeeds,nants
	complex Gains(nfeeds+1,nants,nsols)
	double precision Time(nsols)
c
c  Write out the antenna gains table.
c------------------------------------------------------------------------
	integer off,iostat,ngains,i
c
	ngains = nants*(nfeeds+1)
	off = 8
	do i=1,nsols
	  call hwrited(lGain,time(i),off,8,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error writing gains table')
	    call bugno('f',iostat)
	  endif
	  off = off + 8
	  call hwriter(lGain,Gains(1,1,i),off,8*ngains,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error writing gains table')
	    call bugno('f',iostat)
	  endif
	  off = off + 8*ngains
	enddo
c
	end
c************************************************************************
	subroutine NoGain(lVis,spindx,freq0)
c
	implicit none
	integer lVis
	real spindx
	double precision freq0
c
c  Create a dummy gains table for this data-set.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer nants,ngains,lGain,i,iostat
	complex Gains(2,MAXANT)
	double precision time
c
	call uvnext(lVis)
	call uvrdvri(lVis,'nants',nants,0)
	if(nants.le.0)call bug('f','Invalid number of antennas')
	nants = min(nants,MAXANT)
	ngains = nants + nants
	call uvrdvrd(lVis,'time',time,0.d0)
	if(time.le.0)call bug('f','Invalid time')
c
c  Write all the other needed variables.
c
	call wrhdi(lVis,'nfeeds',1)
	call wrhdi(lVis,'ngains',2*nants)
	call wrhdi(lVis,'nsols',1)
	call wrhdd(lVis,'interval',100*365.d0)
	call wrhdi(lVis,'ntau',1)
	call wrhdd(lVis,'freq0',freq0)
c
c  Fill in the gain table.
c
	do i=1,nants
	  Gains(1,i) = (1.0,0.0)
	  Gains(2,i) = cmplx(-spindx/2,0.)
	enddo
c
c  Write the gain table.
c
	call haccess(lVis,lGain,'gains','write',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hwritei(lGain,0,0,4,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hwrited(lGain,time,8,8,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hwriter(lGain,gains,16,8*ngains,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hdaccess(lGain,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	end
c************************************************************************
	subroutine DomSpec(model,spindx,freq0)
c
	implicit none
	character model*(*)
	real spindx
	double precision freq0
c
c  Determine the dominant spectral index of an MFS model.
c
c  Input:
c    model	Name of the model.
c  Output:
c    spindx	The dominant spectral index.
c    freq0	Reference frequency.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer lIn,nsize(3),i,j
	real Sum11,Sum12
	real Inten(MAXDIM),IAlpha(MAXDIM)
	character string*16
c
	call xyopen(lIn,model,'old',3,nsize)
	call rdhda(lIn,'ctype3',string,' ')
	if(nsize(3).ne.2.or.string(1:4).ne.'FREQ')
     *	  call bug('f','Input model does not look like an MFS model')
	if(nsize(1).gt.MAXDIM)
     *	  call bug('f','Input model dimensions are too big')
c
	call rdhdd(lIn,'crval3',freq0,0.d0)
	write(string,'(f7.2)')freq0
	call output('Reference frequency (GHz):'//string)
c
	Sum11 = 0
	Sum12 = 0
c
	do j=1,nsize(2)
	  call xysetpl(lIn,1,1)
	  call xyread(lIn,j,Inten)
	  call xysetpl(lIn,1,2)
	  call xyread(lIn,j,IAlpha)
	  do i=1,nsize(1)
	    Sum11 = Sum11 + Inten(i)*Inten(i)
	    Sum12 = Sum12 + Inten(i)*IAlpha(i)
	  enddo
	enddo
c
	call xyclose(lIn)
c
	spindx = Sum12 / Sum11
	write(string,'(f6.2)')spindx
	call output('Dominant spectral index:'//string)
c
	end
