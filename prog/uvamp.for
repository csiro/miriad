c************************************************************************
	program uvamp
	implicit none
c
c= uvamp - Annular averages a uv dataset in bins. Prints and plots results.
c& lgm
c: uv analysis
c+
c	UVAMP reads a uv dataset, bins the data in annuli according to
c	uv distance and prints/plots vector averaged amplitude versus 
c	uvdist. Statistical error bars and the expected amplitude for 
c	zero signal are calculated.
c@ vis
c	The name of the input uv data set. No default.
c@ select
c	The normal uv selection commands. 
c@ line
c	The normal uv linetype in the form:
c	  line,nchan,start,width,step
c	The default is all channels (or all wide channels if there are no
c	spectral channels). The output will consist of only spectral or
c	wideband data (but not both). If spectral averaging is performed
c	(``width'' not equal to 1), then the output will be written as a
c	single spectral window.
c@ ref
c	The normal reference linetype, in the form:
c	  line,start,width
c	The default is no reference line.
c@ stokes
c	If a value is given, uvamp will work with the specified 
c	polarizations. Normally this should be set to stokes I if
c	the data includes polarizations.
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreviated to the minimum
c	needed to avoid ambiguity. Possible options are:
c	   'nocal'       Do not apply the gains file. By default, UVAMP
c	                 applies the gains file in copying the data.
c	   'nopol'       Do not apply polarizatiopn corrections. By default
c	                 UVAMP corrects for polarization cross-talk.
c	   'nopass'      DO not apply bandpass corrections. By default
c	                 UVAMP corrects for the bandpass shape.
c	   'ampscalar'   Applies vector averaging to work out the
c	                 averaged visibility phase, but scalar averaging
c	                 to find the averaged visibility amplitude.
c@ bin
c	Number of bins, width of bins, and units of bin width. Up to 200
c	bins are allowed. The units for bin width may be either nsec or
c	klam. The default unit for bin width is nsecs.
c	Default values: 30,40,nsec
c@ offset
c	RA-Dec offset of desired phase center relative to phase center
c	of original uv dataset in arcseconds. Source of interest should be 
c	at the phase center in the typical use of this program. 
c	Default = 0.0,0.0.
c@ type
c       Calculate the amplitude and standard deviation for each bin based on 
c       the real and imaginary components (total) or just based on the real 
c       component (real). The "real" option is useful if your expected source 
c       visibility is entirely real -- i.e. a point source or circularly 
c       symmetric source at the phase center. 
c       Possible options are "total" and "real". Default: total.
c@ device
c	Plot device name. If not specified, no plot is created.
c@ log  
c	Log file name. Default is no log file.
c--
c  History:
c	lgm 25mar92 Original version started as offshoot of uvaver.
c       mjs 08apr92 Variable name mod so it compiles on Convex.
c       mjs 13mar93 pgplot subr names have less than 7 chars.
c	rjs 26aug94 Better coordinate handling. Fix a few problems.
c       lgm 03mar97 Corrected standard deviation calculation
c       pjt  3may99 proper logopen/close interface; better line= stmts
c	mchw 16may02 format change on output listing.
c  Bugs:
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	real secrad
	parameter(secrad=pi/180./3600.)
c
	integer maxbins
	parameter (maxbins = 200)
c
	character version*(*)
	parameter(version='UvAmp: version 2.0 16-may-02')
	character uvflags*8,line*80,pldev*60,logfile*60
	character bunit*10,type*5
	integer tIn,i,nread,numdat(maxbins),numbins,ibin
	real sdatr2(maxbins),sdati2(maxbins),uuvamp(maxbins)
        real sigmean(maxbins),top(maxbins)
	real uvdist(maxbins),rdat,idat,sigr2,sigi2,binsiz,uvd,sigtot
	real dra,ddec,ratio(maxbins),phaz,bot(maxbins)
	real xzero(2),yzero(2),maxamp,minamp,expect(maxbins)
	logical ampsc,klam
	double precision preamble(4),freq
	double precision chfreq(maxchan)
	complex data(maxchan),sumdat(maxbins)
	logical flags(maxchan)
c
c  Externals.
c
	logical uvDatOpn,more
c
	more   = .true.
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags,ampsc)
	call uvDatInp('vis',uvflags)
	call keyi('bin',numbins,30)
	call keyr('bin',binsiz,40.)
	call keya('bin',bunit,'nsec')
	call keyr('offset',dra,0.0)
	call keyr('offset',ddec,0.0)
        call keya('type',type,'total')
        if(type .eq. 'real') then
          line = ' Using only the real components in calculations'
          call output(line)
        else
          line = ' Using the real and imaginary components ' //
     *           'in calculations '
          call output(line)
        endif
        call output('  ')
	call keya('device',pldev,' ')
	call keya('log',logfile,' ')
	call keyfin
c
c   Check input parameters
c
	if(numbins .lt. 0. .or. binsiz .le. 0.) 
     1		call bug('f','Bins improperly specified')
	if(numbins .gt. maxbins) then
	   write(line,'('' Maximum allow bins = '',i5)') maxbins
	   call bug('f',line)
	endif
	klam = .false.
	if(bunit(1:1) .eq. 'k') then
	   klam = .true.
	endif
	dra  = dra*secrad
	ddec = ddec*secrad
c
c  Initialize bins and summing arrays
c
	do i=1,numbins
	   uvdist(i) = binsiz/2.0 + (i-1)*binsiz
	   sumdat(i) = cmplx(0.0,0.0)
	   numdat(i) = 0
	   sdatr2(i) = 0.0
	   sdati2(i) = 0.0
	enddo
c
c  Open the input uv file and output logfile.
c
	if(.not.uvDatOpn(tIn))call bug('f','Error opening input')
	call LogOpen(logfile,' ')
c
c  Convert dra,ddec from true to grid offsets.
c
	call OffCvt(tIn,dra,ddec)
c
c   Loop over data accumulating points in bins and squared quantities for
c   calculations of formal error in result. The phaz rotates the phase 
c   center of the data to the desired position.
c
	call uvDatRd(preamble,data,flags,maxchan,nread)
	dowhile(nread.gt.0)
	   call uvinfo(tIn,'sfreq',chfreq)
	   uvd  = (preamble(1)*preamble(1) + preamble(2)*
     1				preamble(2))**0.5
           ibin = uvd/binsiz + 1
 	   do i=1,nread
	      freq = chfreq(i)
	      if(klam) ibin = (freq*uvd/1000.0)/binsiz + 1
	      if(flags(i).and.ibin.le.numbins) then
		 phaz = -((preamble(1) * dra) + (preamble(2) * ddec)) *
     1			2.0*pi*freq
		 data(i) = data(i) * cmplx(cos(phaz),sin(phaz))
	         sumdat(ibin) = sumdat(ibin) + data(i)
	         numdat(ibin) = numdat(ibin) + 1
	         sdatr2(ibin) = sdatr2(ibin) + 
     1				real(data(i))*real(data(i))
	         sdati2(ibin) = sdati2(ibin) + 
     1				aimag(data(i))*aimag(data(i))
	      endif
	   enddo
	   call uvDatRd(preamble,data,flags,maxchan,nread)
	enddo
c
c  Write out header stuff for log file
c
	line='                 Output Visibility Amplitudes'
        call LogWrite(line,more)
	line='     uv limits        amplitude   sigma      S/N   ' //
     *       'expect      #pnts   '
	call LogWrite(line,more)
	if(klam) then
	   call LogWrite('       (klam) ',more)
	else
	   call LogWrite('       (nsec) ',more)
	endif
c
c  Now do the arithmetic on accumulated data to calculate mean, error
c  in mean, singal-to-noise, and expectational value for zero signal
c
	do i=1,numbins
	   if(numdat(i) .gt. 2) then
	      rdat  = real(sumdat(i))/numdat(i)
	      idat  = aimag(sumdat(i))/numdat(i)
	      sigr2 = (sdatr2(i) - numdat(i)*rdat*rdat)/(numdat(i)-1)
	      sigi2 = (sdati2(i) - numdat(i)*idat*idat)/(numdat(i)-1)
              if(type .eq. 'real') then
                uuvamp(i) = (rdat*rdat)**0.5
                sigtot = sigr2
              else
	        uuvamp(i) = (rdat*rdat + idat*idat)**0.5
                sigtot = (rdat*rdat/(uuvamp(i)*uuvamp(i)))*sigr2 +
     1                   (idat*idat/(uuvamp(i)*uuvamp(i)))*sigi2
              endif
	      sigmean(i) = (sigtot/(numdat(i)-2))**0.5
	      if(sigmean(i).gt.0)then
		ratio(i) = uuvamp(i)/sigmean(i)
	      else
		ratio(i) = 0
	      endif
              expect(i) = ((pi/2.0)**0.5)*sigmean(i)
	   else
	      uuvamp(i)   = 0.0
	      sigmean(i) = 0.0
	      ratio(i)   = 0.0
	      expect(i)  = 0.0
	   endif
	   write(line,
     0     '(f9.2,1x,f9.2,2x,1pe9.2,1x,e9.2,2x,0pf6.1,2x,1pe9.2,2x,i8)') 
     1          binsiz*(i-1),binsiz*i,uuvamp(i),
     2		sigmean(i),ratio(i),expect(i),numdat(i)
	   call LogWrite(line,more)
	enddo
c
c   Write out a few explanation lines for the above table
c
	call LogWrite(' ',more)
 	line='Sigma = the formal standard deviation in the mean'
	call LogWrite(line,more)
	line='Expect = expectation value for the amp assuming no signal'
	call LogWrite(line,more)
c
c  Prepare error bar data and limits for making plot, if requested.
c
	if(pldev .ne. ' ') then
	   maxamp = uuvamp(1)
	   minamp = uuvamp(1)
	   do i=1,numbins
	      top(i) = uuvamp(i) + sigmean(i)
	      bot(i) = uuvamp(i) - sigmean(i)
	      if(top(i) .gt. maxamp) maxamp = top(i)
	      if(bot(i) .lt. minamp) minamp = bot(i)
	   enddo
	   if(minamp .lt. 0.0) minamp = 1.03*minamp
	   if(minamp .gt. 0.0) minamp = 0.97*minamp
	   maxamp = 1.03*maxamp
	   xzero(1) = 0.0
	   xzero(2) = binsiz*numbins
	   yzero(1) = 0.0
	   yzero(2) = 0.0
c
c  Begin actual plot of binned data, error bars, and expactation values
c
	   call pgbeg(0,pldev,1,1)
	   call pgslw(2)
	   call pgscf(2)
	   call pgenv(xzero(1),xzero(2),minamp,maxamp,0,0)
	   if(klam) then
	      call pglab('UV Distance (klam)','Amplitude',' ')
	   else
	      call pglab('UV Distance (nsec)','Amplitude',' ')
	   endif
	   call pgpt(numbins,uvdist,uuvamp,17)
	   call pgerry(numbins,uvdist,top,bot,1.0)
	   call pgline(2,xzero,yzero)
	   call pgsls(4)
	   call pgbin(numbins,uvdist,expect,.true.)
	   call pgend
	endif
	call LogClose
	call uvDatCls
c
	end
c************************************************************************
        subroutine GetOpt(uvflags, ampsc)
c
        implicit none
        logical ampsc
        character uvflags*(*)
c
c  Determine the flags to pass to the uvdat routines.
c
c  Output:
c    uvflags    Flags to pass to the uvdat routines.
c    ampsc      True for amp-scalar averaging
c------------------------------------------------------------------------
        integer nopts
        parameter(nopts=4)
        character opts(nopts)*9
        integer l
        logical present(nopts),docal,dopol,dopass
        data opts/'nocal    ','nopol    ','ampscalar','nopass   '/
c
        call options('options',opts,present,nopts)
        docal = .not.present(1)
        dopol = .not.present(2)
        ampsc =      present(3)
	dopass= .not.present(4)
        uvflags = 'dslr'
        l = 4
        if(docal)then
          l = l + 1
          uvflags(l:l) = 'c'
        endif
        if(dopol)then
          l = l + 1
          uvflags(l:l) = 'e'
        endif
	if(dopass)then
	  l = l + 1
	  uvflags(l:l) = 'f'
	endif
        end
c************************************************************************
	subroutine OffCvt(tIn,dra,ddec)
c
	implicit none
	integer tIn
	real dra,ddec
c
c  Convert a true offset RA/DEC to a "grid" offset. This avoids the
c  small angle assumption that offsets are equivalent to direction
c  cosines, and the fact that a 2D Fourier transform relationship is
c  used is taken into account.
c
c------------------------------------------------------------------------
	double precision x1(2),x2(2)
c
	call coInit(tIn)
c
	x1(1) = dra
	x1(2) = ddec
	call coCvt(tIn,'ow/ow',x1,'op/op',x2)
	dra = x2(1)
	ddec = x2(2)
c
	call coFin(tIn)
c
	end
