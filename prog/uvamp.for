c************************************************************************
	program uvamp
	implicit none
c
c= uvamp - Annular averages a uv dataset in bins. Prints and plots results.
c& lgm
c: uv analysis
c+
c       UVAMP reads a uv dataset, bins the data in annuli according to
c       uv distance and prints/plots vector averaged amplitude versus 
c       uvdist. Statistical error bars and the expected amplitude for 
c       zero signal are calculated.
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
c       the data includes polarizations.
c@ options
c       This gives extra processing options. Several options can be given,
c       each separated by commas. They may be abbreviated to the minimum
c       needed to avoid ambiguity. Possible options are:
c          'nocal'       Do not apply the gains file. By default, UVAMP
c                        applies the gains file in copying the data.
c          'nopol'       Do not apply polarizatiopn corrections. By default
c                        UVAMP corrects for polarization cross-talk.
c	   'nopass'      DO not apply bandpass corrections. By default
c	                 UVAMP corrects for the bandpass shape.
c          'ampscalar'   Applies vector averaging to work out the
c                        averaged visibility phase, but scalar averaging
c                        to find the averaged visibility amplitude.
c@ bin
c	Number of bins, width of bins, and units of bin width. Up to 200
c       bins are allowed. The units for bin width may be either nsec or
c       klam. The default unit for bin width is nsecs.
c       Default values: 30,40,nsec
c@ offset
c	RA-Dec offset of desired phase center relative to phase center
c	of original uv dataset in arcseconds. Source of interest should be 
c       at the phase center in the typical use of this program. 
c       Default = 0.0,0.0.
c@ device
c	Plot device name. If not specified, no plot is created.
c@ log  
c	Log file name. Default is no log file.
c--
c  History:
c	lgm 25mar92 Original version started as offshoot of uvaver.
c       mjs 08apr92 Variable name mod so it compiles on Convex.
c       mjs 13mar93 pgplot subr names have less than 7 chars.
c
c  Bugs:
c	does not work yet
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer maxbins
	parameter (maxbins = 200)
c
	character version*(*)
	parameter(version='UvAmp: version 1.0 26-Aug-92')
	character uvflags*8,line*80,pldev*60,logfile*60
	character type*1,bunit*10
	integer tIn,i,nread,numdat(maxbins),numbins
	integer ibin,j,length,nspect,nschan(8),ischan(8),ifr
	real sdatr2(maxbins),sdati2(maxbins),uuvamp(maxbins)
        real sigmean(maxbins),top(maxbins)
	real uvdist(maxbins),rdat,idat,sigr2,sigi2,binsiz,uvd
	real dra,ddec,secrad,ratio(maxbins),phaz,bot(maxbins)
	real xzero(2),yzero(2),maxamp,minamp,expect(maxbins)
	logical ampsc,klam
	double precision preamble(4),freq,fstep(8),pi
	double precision fstart(8),chfreq(2048)
	complex data(maxchan),sumdat(maxbins)
	logical flags(maxchan),present
c
c  Externals.
c
	logical uvDatOpn,more
c
	more   = .true.
	secrad = 1.0/206265.0
	pi     = 3.141592654
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
	if(logfile .ne. ' ') call LogOpen(logfile,' ')
c
c  Get the first data record an other info that is needed to shift
c  the phase center if requested.
c
	call uvDatRd(preamble,data,flags,maxchan,nread)
c
        call UvProbvr(tIn, 'nspect', type, length, present)
        if (present) then
 	   call UvGetvri(tIn, 'nspect', nspect, 1)
	else
	   nspect = 1
	endif
	call UvProbvr(tIn,'sfreq',type,length,present)
	if(present) then
	   call UvGetvri(tIn,'nschan', nschan, nspect)
	   call UvGetvri(tIn, 'ischan', ischan, nspect)
	   call UvGetvrd(tIn,'sfreq',fstart,nspect)
	   call UvGetvrd(tIn,'sdf',fstep,nspect)
	   do i=1,nread
	      ifr = nspect
	      do j=1,nspect-1
		 if(i.ge.ischan(j) .and. i.lt.ischan(j+1))
     1			ifr = j
	      enddo
	      chfreq(i) = fstart(ifr) + fstep(ifr)*(i-ischan(ifr))
	   enddo
	else
	   call bug('w','Obs Frequency not found.')
	   call bug('w','Shifting of phase center not possible')
	endif
c
c   Loop over data accumulating points in bins and squared quantities for
c   calculations of formal error in result. The phaz rotates the phase 
c   center of the data to the desired position.
c
	dowhile(nread.gt.0)
	   uvd  = (preamble(1)*preamble(1) + preamble(2)*
     1				preamble(2))**0.5
           ibin = uvd/binsiz + 1
 	   do i=1,nread
	      if(present) then
	         freq = chfreq(i)
		 if(klam) ibin = (freq*uvd/1000.0)/binsiz + 1
	      else
	         freq = 0.0
	      endif
	   if(flags(i)) then
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
	line(1:50)='     uv limits      amplitude   sigma      S/N   '
        line(51:70)='expect      #pnts   '
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
	   if(numdat(i) .gt. 0) then
	      rdat = real(sumdat(i))/numdat(i)
	      idat = aimag(sumdat(i))/numdat(i)
	      uuvamp(i) = (rdat*rdat + idat*idat)**0.5
	      sigr2 = (sdatr2(i) - numdat(i)*rdat*rdat)/(numdat(i)-1)
	      sigi2 = (sdati2(i) - numdat(i)*idat*idat)/(numdat(i)-1)
	      sigmean(i) = ((sigr2 + sigi2)/(numdat(i)-2))**0.5
	      ratio(i)   = uuvamp(i)/sigmean(i)
              expect(i) = ((pi/2.0)**0.5)*sigmean(i)
	   else
	      uuvamp(i)   = 0.0
	      sigmean(i) = 0.0
	      ratio(i)   = 0.0
	      expect(i)  = 0.0
	   endif
	   write(line,
     0     '(f8.1,1x,f8.1,2x,1pe9.2,1x,e9.2,2x,0pf6.1,2x,1pe9.2,2x,i8)') 
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
	stop
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
