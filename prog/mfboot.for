c************************************************************************
	program mfboot
	implicit none
c
c= mfboot -- Set the flux scale of a visibility dataset.
c& rjs
c: calibration
c+
c	MFBOOT is a MIRIAD program that corrects the flux scale in
c	visibility datasets. In doing this, it assumes that the flux
c	density scale is out by a constant scale factor. MFBOOT computes
c	and applies a scale factor to the calibration tables of visibility
c	datasets to account for this.
c
c	To determine this factor, MFBOOT compares the flux
c	density of measured visibilities with models of the expected
c	visibility. The visibilities can correspond to measurements of
c	planets or point sources. For point sources, they can be either
c	standard calibrators with known flux density, or others. In the
c	latter case the user must specify its flux density.
c
c@ vis
c	Input visibility datasets. Several datasets can be given (wildcards
c	are supported).  The scale factor will be applied uniformly to all
c	datasets whether or not they contain the source used to determine it.
c@ line
c	Normal uv data "line" parameter, with the normal defaults. See
c	the help on "line" for more information.
c@ select
c	Normal uv-selection parameter. This selects the data in the input
c	datasets to analyse. The data selected should consist of
c	a single planet or point source. See the help on ``select'' for more
c	information. For planets, you may wish to select just the shortest
c	spacing, where the planet is strongest.
c@ flux
c       Three to five numbers, giving the source flux density, a reference 
c       frequency (in GHz), the source spectral index and optionally two
c       higher order terms. The flux and spectral index are at the reference 
c       frequency. If no values are given, then MFBOOT checks whether the
c       source is one of a set of known sources or a planet, and uses
c       the appropriate flux variation with frequency. 
c       MFBOOT has built-in models for a few calibrators as well as the
c	planets - see calplot and plplt.
c
c	For a point source, this parameter gives the flux density of the
c	calibrator in Janskys. For a planet, this parameter gives the
c	brightness temperature of the planet in Kelvin.
c@ mode
c	MFBOOT can process the data in one of three modes: "triple", "scalar"
c	and "vector". The default is "triple" for point sources and "scalar"
c	for planets.
c
c	In "triple" mode, MFBOOT compares the triple product of the data
c	and model. The advantage of comparing triple products is that the
c	process is robust to phase errors, and the data do not need to be
c	phase calibrated. Because planets can be significantly resolved,
c	triple mode is often not appropriate for them. However if the
c	resolution is modest on the selected baselines, triple mode can
c	be used.
c
c	In "scalar" mode, the amplitude of the data and model are compared.
c	This is also robust to phase errors, but will experience a noise
c	bias when the signal to noise is poor. Planets are often sufficiently
c	strong that noise bias is not an issue.
c
c	In "vector" mode, the real part of the data is compare with the model.
c	The data need to be phase calibrated.
c@ clip
c	For planets, this parameter can be used to discard data for baselines
c	that are significantly resolved. Data for a particular baseline will
c	be discarded when the expected flux density on that baseline is
c	less than the clip factor times the total flux density. The clip
c	parameter takes on values between 0 to 1.0, with the default being 0
c	(ie the default is to accept all data).
c@ device
c	PGPLOT device to plot the model data as well as the visibility
c	data. The default is to not produce a plot. No plot is produced
c	in triple mode.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	  noapply Do not apply the scale factor - just evaluate it.
c         nospec Do not try to determine and correct the spectral index across
c                the band.
c
c$Id: mfboot.for,v 1.16 2021/06/03 07:09:31 wie017 Exp $
c--
c  History:
c    rjs     15jan06 Original version adapted from plboot.
c    rjs     19jan06 Fix call to subroutine with //char*(*) arg.
c    rjs     09may06 Increase size of MAXPNT.
c    rjs     07jul06 Increase size of MAXPNT again.
c    mhw     07sep09 Use central frequency for planet parameters
c    mhw     24mar10 Correct spectral index too
c    mhw     16jun11 Fix triple mode spectral index correction
c    mhw     27jun13 Fix nospec option
c    mhw     24apr14 Allow higher order flux models
c    mhw     05jun14 Avoid creating inconsistent gain tables
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*72
	integer MAXVIS,MAXPNTS
	parameter(MAXVIS=32,MAXPNTS=4000000)
c
	character vis(MAXVIS)*256,source*32,line*64,device*64,mode*8
	character psource*32
	logical noapply,nospec
	integer nvis,lVis,vsource,nchan,iplanet,i,n(2),nants,nfeeds
	real fac,f(2),m(2),s,uflux(5),clip
        real f0,f1,f2,fac1,fac2,alpha
	double precision SumXX(2),SumXY(2),SumF(2),preamble(4),time0
	complex data(MAXCHAN),d(2)
	logical flags(MAXCHAN)
	real xval(MAXPNTS),ydval(MAXPNTS),ymval(MAXPNTS)
	integer npnt,SumN(2)
c
	complex db(2,MAXBASE)
	real sb(MAXBASE),mb(2,MAXBASE),fb(2,MAXBASE)
	integer nb(2,MAXBASE),nbl
c
c  Externals.
c
	logical uvDatOpn,uvVarUpd,hdPrsnt
	integer plLook,len1
	character streal*16, versan*72
        
        version = versan('mfboot',
     *                   '$Revision: 1.16 $',  
     *                   '$Date: 2021/06/03 07:09:31 $')
c
c  Get the user input.
c
	call keyini
	call uvDatInp('vis','xcefdwl')
	call uvDatSet('stokes',0)
	call keya('device',device,' ')
	call keyr('flux',uflux(1),0.0)
	call keyr('flux',uflux(2),0.0)
	call keyr('flux',uflux(3),0.0)
	call keyr('flux',uflux(4),0.0)
	call keyr('flux',uflux(5),0.0)
	call keyr('clip',clip,0.0)
	call getopt(mode,noapply,nospec)
	call keyfin
c
c  Process the data.
c
	psource = ' '
        do i=1,2
  	  SumXX(i) = 0
	  SumXY(i) = 0
          SumF(i) = 0
          SumN(i) = 0
        enddo
	nvis = 0
	npnt = 0
c
	nbl = 0
	time0 = 0
	call intIni(nb,MAXBASE)
	dowhile(uvDatOpn(lVis))
	  nvis = nvis + 1
	  if(nvis.gt.MAXVIS)call bug('f','Too many inputs for me!')
	  call uvDatGta('name',vis(nvis))
	  call uvVarIni(lVis,vsource)
	  call uvVarSet(vsource,'source')
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
c
	    if(abs(preamble(3)-time0).gt.2/86400.0)then
	      if(nbl.gt.0)then
	        call intFlush(db,mb,fb,sb,nb,nbl,SumXX,SumXY,SumF,SumN)
	        call intIni(nb,nbl)
	      endif
	      nbl = 0
	      time0 = preamble(3)
	    endif
c
	    if(uvVarUpd(vsource))then
	      call uvrdvra(lVis,'source',source,' ')
	      iplanet = plLook(source)
	      if(iplanet.lt.1.or.iplanet.gt.9.or.iplanet.eq.3)
     *						     iplanet = 0
	      if(source.ne.psource.and.psource.ne.' ')then
		call bug('w','Data also processed for '//source)
	      else if(psource.eq.' ')then
		call output('Found data for source '//source)
                psource = source
		if(mode.eq.' ')then
		  mode = 'scalar'
		  if(iplanet.eq.0)mode = 'triple'
		endif
	      endif
	    endif
c
	    call avdat(lVis,data,flags,nchan,source,
     *		preamble(3),preamble,iplanet,clip,uflux,d,m,f,s,n)
	    if(mode.eq.'triple')then
	      call intAcc(preamble(4),d,m,f,s,n,MAXBASE,
     *         db,mb,fb,sb,nb,nbl)
	    else
	      call acc(mode.eq.'vector',preamble,d,m,f,s,n,
     *		SumXX,SumXY,SumF,SumN,MAXPNTS,npnt,xval,ydval,ymval)
	    endif
	    call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	  enddo
	  call uvDatCls
	enddo
	if(nbl.gt.0)call intFlush(db,mb,fb,sb,nb,nbl,SumXX,SumXY,
     *                            SumF,SumN)
c
	if(SumXX(1)+SumXX(2).le.0)call bug('f','No good data found')
	if(SumXY(1)+SumXY(2).le.0)
     *   call bug('f','Problem seems ill-conditioned')
	fac = (SumXX(1)+SumXX(2))/(SumXY(1)+SumXY(2))
	if(mode.eq.'triple')fac = fac ** (1.0/3.0)
	line = 'Scaling the data by '//streal(fac,'(f13.3)')
	call output(line)
        if (.not.nospec) then
          alpha = 0
          if (SumXX(1).gt.0.and.SumXX(2).gt.0.and.SumXY(1).gt.0.and.
     *        SumXY(2).gt.0.and.SumN(1).gt.0.and.SumN(2).gt.0) then
            f0 = (sumF(1)+SumF(2))/(SumN(1)+SumN(2))
            f1 = SumF(1)/SumN(1)
            f2 = SumF(2)/SumN(2)
            fac1 = SumXX(1)/SumXY(1)
            fac2 = SumXX(2)/SumXY(2)
            if (mode.eq.'triple') then
              fac1 = fac1 ** (1.0/3.0)
              fac2 = fac2 ** (1.0/3.0)
            endif
            if (f2.eq.f1) 
     *       call bug('f','Error in spectral index calculation')
            alpha = log(fac2/fac1)/log(f2/f1)
            line = 'Adjusting spectral slope by '//
     *              streal(alpha,'(f8.3)')
	    call output(line)
          else
            call bug('w','Cannot determine spectral index')
          endif
        endif
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
	      call uvrdvrd(lVis,'time',time0,0.d0)
	      call uvrdvri(lVis,'nants',nants,0)
	      call rdhdi(lVis,'nfeeds',nfeeds,1)
	      call gainWri(lVis,fac,time0,nants,nfeeds)
	    endif
            if (.not.nospec) then
              if(hdPrsnt(lVis,'bandpass')) then
                call bpSca(lVis,f0,alpha)
	      else
	        call bug('w','Cannot adjust spectral slope for '
     *            //vis(i)(1:len1(vis(i)))//
     *          ' because it has no bandpass table')        
	      endif 
            endif        
	    call hisopen(lVis,'append')
	    call hiswrite(lVis,'MFBOOT: Miriad '//version)
	    call hisinput(lVis,'MFBOOT')
	    call hiswrite(lVis,'MFBOOT: '//line)
	    call hisclose(lVis)
	    call uvclose(lVis)
	  enddo
	endif
c
	end
c************************************************************************
	subroutine intIni(nb,nbl)
c
	implicit none
	integer nbl,nb(2,nbl)
c------------------------------------------------------------------------
	integer i
c
	do i=1,nbl
	  nb(1,i) = 0
	  nb(2,i) = 0
	enddo
	end
c************************************************************************
	subroutine intAcc(bl,d,m,f,s,n,MAXBASE,db,mb,fb,sb,nb,nbl)
c
	implicit none
	double precision bl
	integer MAXBASE,nbl
	complex d(2),db(2,MAXBASE)
	real s,sb(MAXBASE),m(2),mb(2,MAXBASE),f(2),fb(2,MAXBASE)
	integer n(2),nb(2,MAXBASE)
c------------------------------------------------------------------------
	integer i1,i2,i,j
c
	call basant(bl,i1,i2)
	i = ((i2-1)*(i2-2))/2 + i1
	if(i.gt.MAXBASE)call bug('f','Buffer overflow in intAcc')
	nbl = max(nbl,i)
	if(nb(1,i)+nb(2,i).eq.0)then
          do j=1,2
	    db(j,i) = 0
	    mb(j,i) = 0
            fb(j,i) = 0
	    nb(j,i) = 0
          enddo
	  sb(i) = 0
	endif
        do j=1,2
	  db(j,i) = db(j,i) + d(j)
	  mb(j,i) = mb(j,i) + m(j)
          fb(j,i) = fb(j,i) + f(j)
	  nb(j,i) = nb(j,i) + n(j)
        enddo
	sb(i) = sb(i) + s
	end
c************************************************************************
	subroutine intFlush(db,mb,fb,sb,nb,nbl,SumXX,SumXY,SumF,SumN)
c
	implicit none
	integer nbl
	integer nb(2,nbl),SumN(2)
	real mb(2,nbl),sb(nbl),fb(2,nbl)
	complex db(2,nbl)
	double precision SumXX(2),SumXY(2),SumF(2)
c------------------------------------------------------------------------
	integer b12,b23,b13,i1,i2,i3,j
	logical more,ok
	real tm,sigma2
	complex td
c
	i3 = 3
	more = .true.
	dowhile(more)
          do i2=2,i3-1
            do i1=1,i2-1
              b12 = ((i2-1)*(i2-2))/2 + i1
              b13 = ((i3-1)*(i3-2))/2 + i1
              b23 = ((i3-1)*(i3-2))/2 + i2
	      more = b23.le.nbl
	      ok = more
              do j=1,2
  	        if(more)ok = nb(j,b12).gt.0.and.nb(j,b13).gt.0.and.
     *		  	     nb(j,b23).gt.0
	        if(ok)then
		  td =  db(j,b12)*db(j,b23)*conjg(db(j,b13))/
     *		        nb(j,b12)/nb(j,b23)/      nb(j,b13)
		  tm =  mb(j,b12)*mb(j,b23)*      mb(j,b13) /
     *		        nb(j,b12)/nb(j,b23)/      nb(j,b13)
		  sigma2 = 1
	          SumXX(j) = SumXX(j) +     (tm*tm/sigma2)
	          SumXY(j) = SumXY(j) + real(tm*td/sigma2)
                  SumF(j) = SumF(j) + fb(j,b23)+fb(j,b12)+fb(j,b13)
                  SumN(j) = SumN(j) + nb(j,b23)+nb(j,b12)+nb(j,b13)
	        endif
              enddo
	    enddo
	  enddo
	  i3 = i3 + 1
	enddo

	end
c************************************************************************
	subroutine avdat(lVis,data,flags,nchan,source,time,uv,iplanet,
     *						clip,uflux,d,m,f,s,n)
c
	implicit none
	integer lVis,nchan
	complex data(nchan)
	logical flags(nchan)
	character source*(*)
	double precision time,uv(2)
	integer iplanet
	real uflux(5),clip
c
	complex d(2)
	real m(2),f(2),s
	integer n(2)
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'maxdim.h'
	double precision sfreq(MAXCHAN),cfreq
	real model(MAXCHAN),pltbv(MAXCHAN)
	integer i,ierr
	real a,b,cospa,sinpa,bmaj,bmin,bpa,rms2,lfr,alpha
        double precision sub(3),dist
	logical ok
	character line*80
c
c  Externals.
c
        real j1xbyx,pltbs
        double precision deltime
c
	if(nchan.gt.MAXCHAN)call bug('f','Too many channels for me')
	call uvinfo(lVis,'sfreq',sfreq)
c
c  Work out the model of the visibility data.
c
c might work out flux vs freq for model  like in mfcal
c
	ok = .true.
        cfreq = sfreq(nchan/2+1)
        if (uflux(2).gt.0) then
          cfreq = uflux(2)
        endif       
	if(iplanet.ne.0)then
	  if(uflux(1).gt.0)then
            do i=1,nchan
              lfr = log(sfreq(i)/cfreq)
              alpha = uflux(3) + lfr*(uflux(4)+lfr*uflux(5))
              pltbv(i) = uflux(1)*(sfreq(i)/cfreq)**alpha
            enddo
	  else
            do i=1,nchan
	      pltbv(i) = pltbs(iplanet,real(sfreq(i)))
            enddo
	  endif
c
	  call plpar(time+deltime(time,'tdb'),iplanet,sub,
     *                                          dist,bmaj,bmin,bpa)
	  cospa = cos(bpa)
	  sinpa = sin(bpa)
	  b = PI * sqrt((bmaj*(uv(1)*cospa-uv(2)*sinpa))**2
     *              + (bmin*(uv(1)*sinpa+uv(2)*cospa))**2)/cfreq
          a = 2 * (KMKS*1e18)/(CMKS*CMKS*1e-26)
     *          * 2 * PI/4 * bmaj*bmin
	  do i=1,nchan
	    model(i) = a*pltbv(i)*sfreq(i)*sfreq(i)
     *                 *j1xbyx(real(b*sfreq(i)))
	  enddo
	  ok = abs(model(1)).ge.abs(clip*a*sfreq(1)*sfreq(1)*0.5)
	else if(uflux(1).gt.0)then
	  do i=1,nchan
            lfr = log(sfreq(i)/cfreq)
            alpha = uflux(3) + lfr*(uflux(4)+lfr*uflux(5))
	    model(i) = uflux(1)*(sfreq(i)/cfreq)**alpha
	  enddo
	else
	  call calstoke(source,'i',sfreq,model,nchan,ierr)
	  if(ierr.gt.1)then
	    line = 'Source is not recognised: '//source
	    call bug('f',line)
	  endif
	endif
c
        do i=1,2
	  d(i) = 0
	  m(i) = 0
          n(i) = 0
          f(i) = 0
        enddo
	s = 0
	if(ok)then
	  call uvdatGtr('variance',rms2)
	  do i=1,nchan
	    if(flags(i))then
              if (i.le.nchan/2) then
                d(1) = d(1) + data(i)
                m(1) = m(1) + model(i)
                n(1) = n(1) + 1
                f(1) = f(1) + sfreq(i)
              else
                d(2) = d(2) + data(i)
                m(2) = m(2) + model(i)
                n(2) = n(2) + 1
                f(2) = f(2) + sfreq(i)
              endif
	      s = s + rms2
	    endif
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
          call bug('f','Error opening graphics device')
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
        call pglab('Baseline Length (k\gl)','Flux Density (Jy)',' ')
        call pgend
c
	end
c************************************************************************
	subroutine Acc(vector,uv,data,model,f,sigma2,n,SumXX,SumXY,
     *		SumF,SumN,MAXPNT,npnt,xval,ydval,ymval)
c
	implicit none
	logical vector
	complex data(2)
	real model(2),f(2),sigma2
	integer n(2)
	double precision uv(2),SumXX(2),SumXY(2),SumF(2)
	integer MAXPNT,npnt,SumN(2),j
	real xval(MAXPNT),ydval(MAXPNT),ymval(MAXPNT)
c
c  Accumulate info given the planetary data.
c------------------------------------------------------------------------
        include 'mirconst.h'
c
	if(n(1)+n(2).gt.0)then
	  npnt = npnt + 1
	  if(npnt.gt.MAXPNT)call bug('f','Too many points')
	  xval(npnt) = 1e-3*sqrt(uv(1)*uv(1) + uv(2)*uv(2))
	  if(vector)then
	    ydval(npnt) = real((data(1)+data(2))/(n(1)+n(2)))
	    ymval(npnt) = real((model(1)+model(2))/(n(1)+n(2)))
            do j=1,2
	      SumXX(j) = SumXX(j) +     (model(j)*model(j)/sigma2)
	      SumXY(j) = SumXY(j) + real(model(j)*data(j)/sigma2)
            enddo
	  else
	    ydval(npnt) = abs((data(1)+data(2))/(n(1)+n(2)))
	    ymval(npnt) = abs((model(1)+model(2))/(n(1)+n(2)))
            do j=1,2
	      SumXX(j) = SumXX(j) + abs(model(j)*model(j)/sigma2)
	      SumXY(j) = SumXY(j) + abs(model(j)*data(j)/sigma2)
            enddo
	  endif
          do j=1,2
            SumF(j) = SumF(j) + f(j)
            SumN(j) = SumN(j) + n(j)
          enddo
	endif
c
        end
c************************************************************************
	subroutine getopt(mode,noapply,nospec)
c
	implicit none
	logical noapply,nospec
	character mode*(*)
c
c  Get extra processing parameters.
c------------------------------------------------------------------------
	integer NMODES
	parameter(NMODES=3)
	character modes(NMODES)*8
	integer nout
c
	integer NOPTS
	parameter(NOPTS=2)
	logical present(NOPTS)
	character opts(NOPTS)*8
c
	data modes/'triple  ','scalar  ','vector  '/
	data opts /'noapply ','nospec  '/
c
	call options('options',opts,present,NOPTS)
	noapply = present(1)
        nospec = present(2)
c
	call keymatch('mode',NMODES,modes,1,mode,nout)
	if(nout.eq.0)mode = ' '
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
        integer maxgains,maxtimes
        parameter (MAXTIMES=10000,MAXGAINS=3*MAXANT*MAXTIMES*
     *     MAXFBIN)
	complex G(MAXGAINS)
        double precision time(MAXTIMES),freq(0:MAXFBIN)
	integer nfeeds,ntau,nsols,ngains,nfbin,i,j,k,l
        
        call uvGnRead(lVis,G,time,freq,ngains,nfeeds,ntau,nsols,nfbin,
     *    maxgains,maxtimes,maxfbin)
        if (ntau.gt.0.and.nfbin.gt.0) call bug('f','Cannot handle '//
     *    ' combination of gains table with delays and gainsf table')
c
c        scale the gains
c
        l=1
        do k=0,nfbin
          do j=1,nsols
            do i=1,ngains,nfeeds+ntau
              G(l) = G(l) * fac
              if (nfeeds.eq.2) G(l+1)=G(l+1) * fac
              l = l + nfeeds + ntau
            enddo
          enddo
        enddo
            
        call uvGnWrit(lVis,G,time,freq,ngains,nsols,nfbin,maxgains,
     *    maxtimes,maxfbin,.false.)
	end
c************************************************************************
	subroutine bpSca(lVis,f0,alpha)
c
	implicit none
	integer lVis
	real alpha,f0
c
c  Fix the slope of the bandpass table present in the data.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
        integer MAXSOLN
        parameter (MAXSOLN=1024)
        integer nschan(MAXWIN)
        double precision freqs(2),sfreq(MAXWIN),sdf(MAXWIN),f
      
	complex bpgains(2*MAXANT*MAXCHAN)
	real scale(MAXCHAN)
	integer item,iostat,nfeeds,ntau,nbpsols,ngains,i,j,k
        integer ichan,nants,n,nchan,nspect,off,ioff
c
c  External
c
        integer hsize
c
c  Get the dimensionality numbers.
c
	call rdhdi(lVis,'ngains',ngains,0)
	call rdhdi(lVis,'nfeeds',nfeeds,1)
	call rdhdi(lVis,'ntau',  ntau,  0)
	nants = ngains / (ntau + nfeeds)
        ngains = nants * nfeeds
	call rdhdi(lVis,'nbpsols', nbpsols,0)
        n=max(1,nbpsols)
	call rdhdi(lVis,'nchan0',nchan,0)
	call rdhdi(lVis,'nspect0',nspect,0)
	if(ngains.le.0.or.nchan.le.0.or.nspect.le.0) call bug('f',
     *	  'Invalid value for nchan, ngains or nspect, in mfboot')
	if(nspect.gt.MAXWIN)call bug('f',
     *	  'Too many spectral windows for me to handle, in mfboot')
c
c  Load the frequency table.
c
	call haccess(lVis,item,'freqs','read',iostat)
	if(iostat.ne.0)call uvGnBug(iostat,'accessing freqs table')
	off = 8
	do i=1,nspect
	  call hreadi(item,nschan(i),off,4,iostat)
	  if(iostat.ne.0)call uvGnBug(iostat,'reading freqs table')
	  off = off + 8
	  call hreadd(item,freqs,off,16,iostat)
	  if(iostat.ne.0)call uvGnBug(iostat,'reading freqs table')
	  off = off + 16
	  sfreq(i) = freqs(1)
	  sdf(i)   = freqs(2)
	enddo
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)call uvGnBug(iostat,'closing freqs table')
c
c  Read in the bandpass gains and correct them
c 
	call haccess(lVis,item,'bandpass','append',iostat)
	if(iostat.ne.0)call bug('f','Error accessing bandpass table')
        if (hsize(item).ne.8+n*ngains*nchan*8+nbpsols*8) 
     *        call bug('f','Bandpass table size is incorrect. '//
     *           'This happens when the inputs for mfcal and'//
     *           ' gpcal are inconsistent')
        ichan=0
        do i=1,nspect
          do k=1,nschan(i)
            ichan=ichan+1
            if (ichan.gt.MAXCHAN) call bug('f','Too many channels')
            f=sfreq(i)+(k-1)*sdf(i)
            scale(ichan) = sqrt((f/f0)**alpha)
          enddo
        enddo
c
        off=8
        do k=1,n
	  call hreadr(item,bpgains,off,8*ngains*nchan,iostat)
          do j=1,ngains
            ioff=nchan*(j-1)
            do i=1,nchan
              bpgains(i+ioff) = bpgains(i+ioff)*scale(i)
            enddo
          enddo
          call hwriter(item,bpgains,off,8*ngains*nchan,iostat)
          off=off+8*ngains*nchan
          if (nbpsols.gt.0) then
            off=off+8
          endif
          if(iostat.ne.0)call bug('f','Error updating bandpass table')
        enddo
	call hdaccess(item,iostat)
	if(iostat.ne.0)call bug('f','Error closing bandpass table')
c

	end
c************************************************************************
	subroutine gainWri(lVis,fac,time,nants,nfeeds)
c
	implicit none
	real fac
	double precision time
	integer nants,nfeeds,lVis
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	complex gains(MAXANT*2)
	integer header(2),i,item,iostat
c
	do i=1,nants*nfeeds
	  gains(i) = fac
	enddo
c
c  Create the gains table.
c
	call wrhdd(lVis,'interval',2.d0)
	call wrhdi(lVis,'ngains',nants*nfeeds)
	call wrhdi(lVis,'nsols',1)
	call wrhdi(lVis,'ntau',0)
	call wrhdi(lVis,'nfeeds',nfeeds)
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
	call hwriter(item,gains,16,8*nants*nfeeds,iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hdaccess(item,iostat)
	end
