c************************************************************************
	program plboot
	implicit none
c
c= PlBoot -- Set the flux scale of a visibility dataset given a planet obs.
c& rjs
c: calibration
c+
c	PlBoot is a MIRIAD program which corrects the flux scale in
c	visibility datasets. In doing this, it assumes that the flux
c	scale is out by a constant factor. PlBoot looks for observations
c	of planets, and given its model of the planetary visibility
c	function, it computes the factor needed to correct the dataset's
c	flux scale.
c
c	To fix the flux scale, PlBoot modifies calibration tables attached
c	to each dataset.
c@ vis
c	Input visibility datasets. Several datasets can be given (wildcards
c	are supported). The datasets should include observations of a planet.
c@ select
c	Normal uv-selection parameter. This selects the data in the input
c	datasets to analyse. See the help on ``select'' for more information.
c	plboot will use any data that has a source name which it recognises
c	as a planet. You may wish to select just the shortest spacing, where
c	the planet is strongest.
c@ device
c	PGPLOT device to plot the planet model data as well as the visibility
c	data. The default is to not produce a plot.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	  vector  Use the real part (rather than amplitude) of the data and
c	          model. This option should be used if the visibility data are
c	          phase calibrated
c	  noapply Do not apply the scale factor (just evaluate it).
c         nofqav  When dealing with amplitude data, PLBOOT normally
c                 averaging in frequency first, to avoid noise
c	          biases. The nofqav disables this averaging.
c--
c  History:
c    rjs     04feb01 Original version.
c    rjs     19may03 Average in freq when dealing with amplitudes.
c    rjs     05jun05 Added plotting option.
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='Plboot: version 1.0 05-Jun-05')
	integer MAXVIS,MAXPNTS
	parameter(MAXVIS=32,MAXPNTS=10000)
c
	character vis(MAXVIS)*64,source*32,line*64,device*64
	logical vector,planet,noapply,nofqav
	integer nvis,lVis,vsource,nchan,iplanet,nants,i,iplanetp
	real fac
	double precision SumXX,SumXY,preamble(4),time
	double precision sfreq(MAXCHAN)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
	real xval(MAXPNTS),ydval(MAXPNTS),ymval(MAXPNTS)
	integer npnt
c
c  Externals.
c
	logical uvDatOpn,uvVarUpd,hdPrsnt
	integer plLook
	character streal*16
c
c  Get the user input.
c
	call output(version)
	call keyini
	call uvDatInp('vis','xcefd')
	call uvDatSet('stokes',0)
	call keya('device',device,' ')
	call getopt(vector,noapply,nofqav)
	call keyfin
c
c  Process the data.
c
	iplanetp = 0
	SumXX = 0
	SumXY = 0
	nvis = 0
	npnt = 0
	dowhile(uvDatOpn(lVis))
	  nvis = nvis + 1
	  if(nvis.gt.MAXVIS)call bug('f','Too many inputs for me!')
	  call uvDatGta('name',vis(nvis))
	  call uvVarIni(lVis,vsource)
	  call uvVarSet(vsource,'source')
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  planet = .false.
	  dowhile(nchan.gt.0)
	    if(uvVarUpd(vsource))then
	      call uvrdvra(lVis,'source',source,' ')
	      iplanet = plLook(source)
	      planet = iplanet.ge.1.and.iplanet.le.9.and.iplanet.ne.3
	      if(planet.and.iplanet.ne.iplanetp)
     *		call output('Found data for planet '//source)
	      iplanetp = iplanet
	    endif
	    if(planet)then
	      call uvinfo(lVis,'sfreq',sfreq)
	      call Acc(nofqav,vector,preamble,preamble(3),iplanet,
     *				data,flags,sfreq,nchan,SumXX,SumXY,
     *				MAXPNTS,npnt,xval,ydval,ymval)
	    endif
	    call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
	  call uvDatCls
	enddo
c
	if(SumXX.le.0)call bug('f','No good data found')
	if(SumXY.le.0)call bug('f','Problem seems ill-conditioned')
	fac = SumXX/SumXY
	line = 'Scaling the data by '//streal(fac,'(f13.3)')
	call output(line)
c
c  Generate the plot, if needed.
c
	do i=1,npnt
	  ydval(i) = fac*ydval(i)
	enddo
	if(npnt.gt.0.and.device.ne.' ')
     *	  call plotit(xval,ydval,ymval,npnt,device)
c
c  Now apply the scale factor to all the input datasets.
c
	fac = sqrt(fac)
	if(.not.noapply)then
	  do i=1,nvis
	    call uvopen(lVis,vis(i),'old')
	    if(hdPrsnt(lVis,'gains'))then
	      call gainSca(lVis,fac)
	    else
	      call uvscan(lVis,'baseline')
	      call uvrdvrd(lVis,'time',time,0.d0)
	      call uvrdvri(lVis,'nants',nants,0)
	      call gainWri(lVis,fac,time,nants)
	    endif
	    call hisopen(lVis,'append')
	    call hiswrite(lVis,'PLBOOT: Miriad '//version)
	    call hisinput(lVis,'PLBOOT')
	    call hiswrite(lVis,'PLBOOT: '//line)
	    call hisclose(lVis)
	    call uvclose(lVis)
	  enddo
	endif
c
	end
c************************************************************************
	subroutine plotit(xval,ydval,ymval,npnt,device)
c
	implicit none
	integer npnt
	character device*(*)
	real xval(npnt),ydval(npnt),ymval(npnt)
c
c  Generate a plot.
c
c------------------------------------------------------------------------
	integer i
	real xmin,xmax,ymin,ymax,xlo,xhi,ylo,yhi
c
c  Externals.
c
	integer pgbeg
c
	xmin = xval(1)
	xmax = xmin
	ymin = ydval(1)
	ymax = ymin
c
	do i=1,npnt
	  xmin = min(xmin,xval(i))
	  xmax = max(xmax,xval(i))
	  ymin = min(ymin,ydval(i),ymval(i))
	  ymax = max(ymax,ydval(i),ymval(i))
	enddo
c
       if(pgbeg(0,device,1,1).ne.1)then
          call pgldev
          call bug('f','Error opeing graphics device')
        endif
        call pgscf(2)
        call pgrnge(0.0,xmax,xlo,xhi)
        call pgrnge(ymin,ymax,ylo,yhi)
        call pgpage
        call pgvstd
        call pgswin(xlo,xhi,ylo,yhi)
        call pgbox('BCNST',0.,0,'BCNST',0.,0)
        call pgpt(npnt,xval,ydval,1)
	call pgsci(2)
	call pgpt(npnt,xval,ymval,1)
	call pgsci(1)
        call pglab('Baseline Length (metres)','Flux Density (Jy)',' ')
        call pgend
c
	end
c************************************************************************
	subroutine Acc(nofqav,vector,uv,time,iplanet,
     *		data,flags,sfreq,nchan,SumXX,SumXY,
     *		MAXPNT,npnt,xval,ydval,ymval)
c
	implicit none
	logical vector,nofqav
	integer nchan,iplanet
	complex data(nchan)
	logical flags(nchan)
	double precision uv(2),time,sfreq(nchan),SumXX,SumXY
	integer MAXPNT,npnt
	real xval(MAXPNT),ydval(MAXPNT),ymval(MAXPNT)
c
c  Accumulate info given the planetary data.
c------------------------------------------------------------------------
        include 'mirconst.h'
        integer i,n
        real a,b,cospa,sinpa,pltb,bmaj,bmin,bpa,model
	real avmodel
	complex avdata
        double precision sub(3),dist
c
c  Externals.
c
        real j1xbyx,pltbs
        double precision deltime
c
        call plpar(time+deltime(time,'tdb'),iplanet,sub,
     *                                          dist,bmaj,bmin,bpa)
        pltb = pltbs(iplanet,real(sfreq(1)))
        cospa = cos(bpa)
        sinpa = sin(bpa)
        b = PI * sqrt((bmaj*(uv(1)*cospa-uv(2)*sinpa))**2
     *              + (bmin*(uv(1)*sinpa+uv(2)*cospa))**2)
        a = 2 * pltb * (KMKS*1e18)/(CMKS*CMKS*1e-26)
     *    * 2 * PI/4 * bmaj*bmin
c
	avdata = (0.,0.)
	avmodel = 0
	n = 0
        do i=1,nchan
	  if(flags(i))then
	    model = a*sfreq(i)*sfreq(i)*j1xbyx(real(b*sfreq(i)))
	    avdata = avdata + data(i)
	    avmodel = avmodel + model
	    n = n + 1
	    if(nofqav)then
	      SumXX = SumXX + model*model
	      if(vector)then
	        SumXY = SumXY + model*data(i)
	      else
	        SumXY = SumXY + abs(model*data(i))
	      endif
	    endif
	  endif
        enddo
	if(n.gt.0)then
	  npnt = npnt + 1
	  if(npnt.gt.MAXPNT)call bug('f','Too many points')
	  xval(npnt) = CMKS*1e-9*sqrt(uv(1)*uv(1) + uv(2)*uv(2))
	  ydval(npnt) = abs(avdata/n)
	  ymval(npnt) = abs(avmodel/n)
	  if(.not.nofqav)then
	    SumXX = SumXX + avmodel*avmodel/n
	    SumXY = SumXY + abs(avmodel*avdata/n)
	  endif
	endif
c
        end
c************************************************************************
	subroutine getopt(vector,noapply,nofqav)
c
	implicit none
	logical vector,noapply,nofqav
c
c  Get extra processing parameters.
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	logical present(NOPTS)
	character opts(NOPTS)*8
	data opts/'vector  ','noapply ','nofqav  '/
c
	call options('options',opts,present,NOPTS)
	vector =  present(1)
	noapply = present(2)
	nofqav =  present(3).or.vector
	end
c************************************************************************
	subroutine gainSca(lVis,fac)
c
	implicit none
	integer lVis
	real fac
c
c  Scale the gains table present in the data.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex Gains(3*MAXANT)
	real scale(3*MAXANT)
	integer item,iostat,nfeeds,ntau,nsols,ngains,offset,i,j
c
c  Externals.
c
	integer hsize
c
	call haccess(lVis,item,'gains','append',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening to modify gains item')
	  call bugno('f',iostat)
	endif
	call rdhdi(lVis,'nfeeds',nfeeds,1)
	call rdhdi(lVis,'ntau',ntau,0)
	call rdhdi(lVis,'ngains',ngains,0)
	if(mod(ngains,nfeeds+ntau).ne.0)
     *	  call bug('f','Bad number of gains or feeds in table')
	nsols = hsize(item)
	if(mod(nsols-8,8*ngains+8).ne.0)
     *	  call bug('f','Size of gain table looks wrong')
	nsols = (nsols-8)/(8*ngains+8)
c
	if(ngains.gt.3*MAXANT)call bug('f','Too many gains for me!')
	do i=1,ngains,nfeeds+ntau
	  scale(i) = fac
	  if(nfeeds.eq.2)scale(i+1) = fac
	  if(ntau.eq.1)scale(i+nfeeds) = 1
	enddo
c
c  Now correct the data.
c
	offset = 16
	do i=1,nsols
	  call hreadr(item,gains,offset,8*ngains,iostat)
	  if(iostat.ne.0)call bugno('f',iostat)
	  do j=1,ngains
	    gains(j) = scale(j)*gains(j)
	  enddo
	  call hwriter(item,gains,offset,8*ngains,iostat)
	  if(iostat.ne.0)call bugno('f',iostat)
	  offset = offset + 8 + 8*ngains
	enddo
c
	call hdaccess(item,iostat)
	end
c************************************************************************
	subroutine gainWri(lVis,fac,time,nants)
c
	implicit none
	real fac
	double precision time
	integer nants,lVis
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex gains(MAXANT)
	integer header(2),i,item,iostat
c
	do i=1,nants
	  gains(i) = fac
	enddo
c
c  Create the gains table.
c
	call wrhdd(lVis,'interval',2.d0)
	call wrhdi(lVis,'ngains',nants)
	call wrhdi(lVis,'nsols',1)
	call wrhdi(lVis,'ntau',0)
	call wrhdi(lVis,'nfeeds',1)
	call haccess(lVis,item,'gains','write',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
c
	header(1) = 0
	header(2) = 0
	call hwritei(item,header,0,8,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hwrited(item,time,8,8,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hwriter(item,gains,16,8*nants,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hdaccess(item,iostat)
	end
