c***********************************************************************
	program FitGains
	implicit none
c
c= FITGAINS - List gains for a uv dataset
c& mchw
c: uv analysis
c+
c	FITGAINS lists the gains derived from a self-calibration 
c	for a MIRIAD UV data file. The mean and rms of the gains
c	are calculated.
c@ vis
c	The input UV dataset name. Only the gains item needs to be
c	present in the uv dataset. No default.
c
c@ log
c	The list output file name.  The default is the terminal. The
c	gains are listed versus time, and can be plotted using Mongo.
c
c@ refant
c	The gain of this antenna is set to cmplx(1.,0.). The
c	other antenna gains are relative to the reference antenna.
c	The default is to use the original gains.
c--
c
c  History:
c    mchw  23mar90 Original version.
c    mchw  13Apr90 Added stop flag.
c    mchw   2may90 fixed default, and added comment.
c    pjt    2may90 Used  maxdim.h include file.
c    rjs    3may90 Fixed up the collision between mchw and pjt.
c    mchw  24nov90 Protected sqrt(negative)
c    mchw  12dec90 Removed maxdim.h, using larger maxant.
c			changed logwrite to logwrit.
c    mjs   25feb91 Changed references of itoa to itoaf.
c    mjs   04aug91 Replaced local maxant with maxdim.h MAXANT
c    mchw  31mar93 Fixed format for more than 6 antennae.
c    mjs   02jul93 Commented out unused fmt stmt to elim compiler warn.
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*),vis*64,log*64,line*80
	parameter(version='version 1.0 31-mar-93')
	integer tgains,header(2),nants,nsols,item,offset
	double precision interval,dtime
	integer refant,iostat,i,j,k
	complex gains(maxant),ref
	real SumAmp(maxant),AveAmp(maxant),RmsAmp(maxant),SumPhi(maxant)
	real SumWts(maxant),Amp(maxant),Phi(maxant),RmsPhi(maxant)
	real AvePhi(maxant)
c
c  Externals.
c
	character itoaf*8
c
c  Get the input parameters.
c
	call output('FitGains: '//version)
	call keyini
	call keyf('vis',vis,' ')
	call keya('log',log,' ')
	call keyi('refant',refant,0)
	call keyfin
c
c  Check that the inputs make sense.
c
	if(vis.eq.' ') call bug('f','Input visibility file is missing')
c
c  Open the output log file.
c
	call LogOpen(log,'q')
	call LogWrit('Gains for '//vis)
c
c  Open the uvdata file containing the gain solutions.
c		We do not use uvopen since vartable may not be present.
c 
	call hopen(tgains,vis,'old',iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error opening output gains file '//vis)
	    call bugno('f',iostat)
	  endif
c
c  Read some header information for the gains file.
c
	call rdhdd(tgains,'interval',interval,0.d0)
	call rdhdi(tgains,'ngains',nants,0)
	call rdhdi(tgains,'nsols',nSols,0)
	call output('Number of gains: '//itoaf(nants))
	call output('Number of solution intervals: '//itoaf(nSols))
	if(nants*nSols.eq.0) call bug('f','No gains to fit')
c
c  Look for the gains item
c
	call haccess(tgains,item,'gains','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening output gains item')
	  call bugno('f',iostat)
	endif
	offset = 0
	call hreadi(item,header,offset,8,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error reading gains item')
	  call bugno('f',iostat)
	endif
	offset = 8
c
c  Initialize some statistics.
c
	do j=1,nants
	  SumAmp(j) = 0.
	  RmsAmp(j) = 0.
	  SumPhi(j) = 0.
	  RmsPhi(j) = 0.
	  SumWts(j) = 0.
	enddo
c
c  Read the gains and write out amplitude and phase relative to reference
c  antenna. Accumulate statistics.
c
	if(refant.le.0.or.refant.gt.nants)refant=0
	do k=1,nsols
	  call hreadd(item,dtime,offset,8,iostat)
	  offset = offset + 8
	  if(iostat.eq.0) call hreadr(item,gains,offset,8*nants,
     *								iostat)
	  if(iostat.ne.0)then
	    call bug('w','I/O error while reading gains')
	    call bugno('f',iostat)
	  endif
	  if(refant.ne.0)ref = gains(refant)
	  do j=1,nants
	    if(refant.ne.0.and.abs(gains(refant)).ne.0.)then
	      gains(j) = gains(j)/ref
	    endif
	    call amphase(gains(j),amp(j),phi(j))
	    SumAmp(j) = SumAmp(j) + amp(j)
	    SumPhi(j) = SumPhi(j) + phi(j)
	    RmsAmp(j) = RmsAmp(j) + amp(j)*amp(j)
	    RmsPhi(j) = RmsPhi(j) + phi(j)*phi(j)
	    SumWts(j) = SumWts(j) + 1.
	  enddo
c	  write(line,100) dtime-2444239.5d0,(amp(i),nint(phi(i)),
c     *							i=1,nchan)
c100  	  format(f13.7,' ',6(f8.3,i5))
c	  length = 13 + 1 + nchan*(8+5)
c	  call LogWrit(line(1:length))
	  offset = offset + 8*nants
    	enddo
c
c  Close gains item
c
	call hdaccess(item,iostat)
	if(iostat.ne.0)then
	  call bug('w','Error closing output gains item')
	  call bugno('f',iostat)
	endif
c
c  Write out some statistics for the gains.
c
	do j=1,nants
	  if(SumWts(j).ne.0.)then
	    AveAmp(j) = SumAmp(j)/SumWts(j)
	    AvePhi(j) = SumPhi(j)/SumWts(j)
	    RmsAmp(j) = sqrt(max(RmsAmp(j)/SumWts(j)-AveAmp(j)**2,0.))
	    RmsPhi(j) = sqrt(max(RmsPhi(j)/SumWts(j)-AvePhi(j)**2,0.))
	  endif
	enddo
c
	do j=1,nants,6
	  k = min(j+5,nants)
	  write(line,110) (AveAmp(i),nint(AvePhi(i)),i=j,k)
110  	  format(3x,'Average',3x,6(f7.3,i4))
	  call LogWrit(line)
	  write(line,120) (RmsAmp(i),nint(RmsPhi(i)),i=j,k)
120  	  format(5x,'Rms',5x,6(f7.3,i4))
	  call LogWrit(line)
	enddo
c
	call LogClose
	call hclose(tgains,iostat)
	end
c********1*********2*********3*********4*********5*********6*********7*c
