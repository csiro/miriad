c************************************************************************
	program ZeeStat
	implicit none

c= zeestat - Fits Zeeman parameters to I and V cubes
c& nebk
c: model fitting
c+
c	ZEESTAT is a Miriad task that fits Zeeman parameters to a region of
c	input I and V cubes. Zeestat also analyses the noise correlations
c	to determine a factor used in correcting the sigma estimates.
c@ iin
c	Input I cube in vxy order. Wild card expansion is supported. No default.
c@ vin
c	Input V cube in vxy order. Wild card expansion is supported. No default.
c@ beam
c	The beam of the observation. This is used in determining the sigma
c	correction factor. The default is a delta function. You should
c	give the beam if you want an accurate estimate of sigma.
c	Wild card expansion is supported. 
c@ rho
c	The channel-to-channel correlation coefficient. This is used in
c	determining the sigma correction factor. The default is 0.
c@ freq
c	Frequency (GHz) of splitting. No default.
c@ mode
c	This is a character string that determines the algorithm used in the
c	fitting process. It consists of several flags, which can be:
c	  m Use maximum likelihood technique.
c	  l Include a leakage term in the fitting.
c	  2 Use a two sided derivative estimate.
c	  x Perform extra checks for better solutions when using the maximum
c	    likelihood technique.
c	The default is ' ' i.e., least squares and 1.
c@ aveop
c	If 'a' then the spectra specified by the spatial window are
c	averaged before being passed to the fitting routine.  Otherwise,
c	and by default, all spectra are passed to the fitting routine
c	and "spatial summing" (as defined by SKLZ) is performed.
c@ chan
c	Channel range. Default is all channels.
c@ blc
c	Bottom left corner of spatial region to examine. Default is (1,1).
c@ trc
c	Top right corner of spatial region to examine. Default is all
c	of image.
c@ log
c	This is the name of an output logfile, which shows how the error
c	estimate varies with the alpha parameter.
c@ out
c	Output results to this file as well as screen
c--
c
c  History:
c    rjs   jun89 Original version.
c    rjs 23jun89 Changed call sequences to ZedScale, Zed. Added the ability
c		 to produce a log of alpha versus sigi.
c    rjs 30jun89 Changed call sequence to ZedFunc.
c    rjs  9jul89 Added options dealing with noise correlation, and the
c		 calls to ZeeFudge.
c    nebk 9aug89 Add output of eta, change default mode to ' ', 
c                change blc,trc, to chan and blc,trc
c    nebk 9aug89 Add spectrum averaging option
c    nebk17aug89 Make Sig_B always positive
c    nebk18sep89 Change averaging option to use less arrays
c    nebk22sep89 Remove uneccessary restriction on size of area for averaging
c    nebk12mar91 Change KEYA to KEYF
c    nebk30mar92 Convert to memalloc/memfree
c    nebk07sep93 Add keyword out and replace (*,*) outputs with a call
c		 to OUTPUT
c    nebk01jul94 Check I and V axis types the same
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer nsamp,nBemMax
	parameter(nsamp=100,nBemMax=51*51)
c
        integer ptv, pti
        real data(maxbuf)
        common data
c
	real Row(maxdim),BemData(nBemMax)
	real a,b,siga,sigb,sigi,scale,freq,a0,rho,Fudge
        real eta2, etahat2
	logical noline,convrg
	integer lIIn,lVin,lBeam,lLog
	character mode*8,iin*64,vin*64,logf*64,line*80,beam*64
        character aveop*1,outf*64, ctypei*8, ctypev*8, itoaf*1
	integer blc(3),trc(3),isize(3),vsize(3),bsize(2),n(3)
	integer size,nbx,nby,npix,nspec
	integer i,j,k,i0,v0,b0,length,iostat,delta,ii0,vv0
c
c  Externals.
c
	integer len1
c
        call output('Zeestat: version 01-jul-94')
	call keyini
	call keyf('iin',iin,' ')
	call keyf('vin',vin,' ')
	call keyf('beam',beam,' ')
	call keyr('freq',freq,0.)
	call keya('mode',mode,' ')
        call keya('aveop', aveop, 's')
	call keyr('rho',rho,0.)
	call keyi('chan',blc(1),1)
        call keyi('chan',trc(1),0)
	call keyi('blc',blc(2),1)
	call keyi('blc',blc(3),1)
	call keyi('trc',trc(2),0)
	call keyi('trc',trc(3),0)
	call keya('log',logf,' ')
	call keya('out',outf,' ')
	call keyfin
	if(iin.eq.' '.or.vin.eq.' ')
     *	  call bug('f','Input name is missing')
c
c  Open the input file, and fix up the region specification.
c
	call xyopen(lIIn,iin,'old',3,isize)
	call xyopen(lVIn,vin,'old',3,vsize)
c
	size = 1
	do i=1,3
          call rdhda (lIIn, 'ctype'//itoaf(i), ctypei, ' ')
          call rdhda (lVIn, 'ctype'//itoaf(i), ctypev, ' ')
          if (ctypei.ne.ctypev)
     +      call bug ('f', 'I and V images have different axis types')
c
	  if(isize(i).ne.vsize(i))
     *	    call bug('f','Input files are different sizes')
	  if(trc(i).eq.0) trc(i) = isize(i)
	  if(blc(i).lt.1.or.trc(i).gt.isize(i).or.blc(i).gt.trc(i))
     *	    call bug('f','Invalid blc or trc')
c
	  n(i) = trc(i) - blc(i) + 1
	  size = size * n(i)
	enddo
	if(isize(1).gt.maxdim) call bug('f','First dimension too large')
c
c  Allocate memory for data arrays
c
        if (aveop.eq.'a') then
          call memalloc(pti,n(1),'r')
          call memalloc(ptv,n(1),'r')
        else
          call memalloc(pti,size,'r')
          call memalloc(ptv,size,'r')
        end if
c
c  Now read in the entire region requested by the user.
c
        do i = 1, n(1)
          data(pti+i-1) = 0.0
          data(ptv+i-1) = 0.0
        end do
        npix = n(2) * n(3)
c
	i0 = pti
	v0 = ptv
	do k=blc(3),trc(3)
	  call xysetpl(lIIn,1,k)
	  call xysetpl(lVin,1,k)
	  do j=blc(2),trc(2)
	    call xyread(lIIn,j,Row)
            ii0 = pti
	    do i=blc(1),trc(1)
              if (aveop.eq.'a') then
                data(ii0) = data(ii0) + Row(i)/npix
                ii0 = ii0 + 1
              else
  	        data(i0) = Row(i)
  	        i0 = i0 + 1
              end if
	    enddo
c
	    call xyread(lVIn,j,Row)
            vv0 = ptv
	    do i=blc(1),trc(1)
              if (aveop.eq.'a') then
                data(vv0) = data(vv0) +  Row(i)/npix
                vv0 = vv0 + 1
              else
 	        data(v0) = Row(i)
                v0 = v0 + 1
              end if
	    enddo
	  enddo
	enddo
c
c  Now estimate the spectra and eta
c
        if (index(mode,'2').ne.0) then
           delta = 1
        else
           delta = 0
        end if
        if(aveop.eq.'a') then
          nspec = 1
        else
          nspec = n(2) * n(3)
        end if
        call zed(mode,data(pti),data(ptv),n(1),nspec,a,b,siga,
     *           sigb,sigi,convrg)
        call zedeta(data(pti),n(1),nspec,sigi,delta,eta2,etahat2)
	a = 2*a
	siga = 2*siga
	if(.not.convrg)
     *	  call bug('w','Fitting algorithm failed to converge')
c
c Compute scale to convert from a channel increment to a magnetic 
c field strength (noline=.false.) or a frequency increment (noline=.true.)
c
	call ZedScale(lIIn, freq, scale, noline)
	if(noline)
     *	  call bug('w', 'Spectral line not matched, result in Hertz')
c
c  Output the results.
c
        call logopen(outf,' ')
        if(aveop.eq.'a') then
          line = 'Spectra averaged in spatial region'
          call output (line)
          if (outf.ne.' ') call logwrit (line)
        else
          line = 'Spectra summed in spatial region'
          call output (line)
          if (outf.ne.' ') call logwrit (line)
        end if
c
        write (line,'(a,i8)') 'Number of pixels:      ', n(2)*n(3)
        call output (line)
        if (outf.ne.' ') call logwrit (line)
c
	write (line,'(a,1pe13.6,3x,1pe13.6)') 
     *    'B field strength, sigma: ', scale*a, abs(scale)*siga
        call output (line)
        if (outf.ne.' ') call logwrit (line)
c
	write (line,'(a,1pe13.6,3x,1pe13.6)')
     *   'Channel Splitting, sigma:', a, siga
        call output (line)
        if (outf.ne.' ') call logwrit (line)
c
	write (line,'(a,1pe13.6,3x,1pe13.6)')
     *   'Leakage gain, sigma:     ', b, sigb
        call output (line)
        if (outf.ne.' ') call logwrit (line)
c
	write (line,'(a,1pe13.6)') 
     *   'Rms noise per pixel:     ',sigi
        call output (line)
        if (outf.ne.' ') call logwrit (line)
c
	write (line,'(a,1pe13.6)') 
     *   'Estimated eta:           ',sqrt(eta2)
        call output (line)
        if (outf.ne.' ') call logwrit (line)
c
        write (line,'(a,1pe13.6)') 
     *   'Estimated etahat**2      ',etahat2
        call output (line)
        if (outf.ne.' ') call logwrit (line)
c
c  Determine the sigma fudge factor, if there is information for us to
c  derive it. Note that we only pass that portion of the beam that we can
c  fit.
c
        if (aveop.ne.'a') then
  	  if(abs(rho).gt.0.001.or.
     *      (beam.ne.' '.and.n(2)*n(3).gt.1))then
  	    if(beam.ne.' '.and.n(2)*n(3).gt.1)then
	      call xyopen(lBeam,beam,'old',2,bsize)
	      nbx = sqrt(real((nBemMax*bsize(1))/bsize(2)))
 	      nbx = min(nbx,bsize(1))
	      nby = min(nBemMax / nbx, bsize(2))
	      if(min(nbx,nby).lt.1) call bug('f','Something is wrong')
	      blc(1) = (bsize(1) - nbx)/2 + 1
	      blc(2) = (bsize(2) - nby)/2 + 1
	      trc(1) = blc(1) + nbx - 1
	      trc(2) = blc(2) + nby - 1
	      b0 = 1
	      do j=blc(2),trc(2)
	        call xyread(lBeam,j,Row)
	        do i=blc(1),trc(1)
	          BemData(b0) = Row(i)
	          b0 = b0 + 1
	        enddo
	      enddo
	    else
	      BemData(1) = 1.
	      nbx = 1
	      nby = 1
	    endif
	    call ZedFudge(mode,data(pti),data(ptv),n(1),n(2),n(3),0.5*a,
     *		          b,fudge,rho,BemData,nbx,nby)
            write (line,'(a,1pe13.6)') 
     *         'Sigma Fudge factor:      ',fudge
            call output (line)
            if (outf.ne.' ') call logwrit (line)
c
            write (line,'(a,1pe13.6)') 
     *         'Corrected B field sigma: ', fudge*abs(scale)*siga
            call output (line)
            if (outf.ne.' ') call logwrit (line)
c
            write (line,'(a,1pe13.6)') 
     *        'Corrected Channel sigma: ', fudge*siga
            call output (line)
            if (outf.ne.' ') call logwrit (line)
          end if
	endif
        if (outf.ne.' ') call logclose
c
c  If the "log" parameter was given, then the user wants a log file
c  of how sigi varies with a.
c
	if(logf.ne.' ')then
	  call txtopen(lLog,Logf,'new',iostat)
	  if(iostat.ne.0) call bugno('f',iostat)
	  do i=1,nsamp+1
	    a0 = a - 15*siga + (30./nsamp) * siga * (i-1)
	    call ZedFunc(mode,data(pti),data(ptv),n(1),nspec,
     *		         0.5*a0,b,sigi)
	    write(line,'(1p2e15.7)')a0,sigi
	    length = len1(line)
	    call txtwrite(lLog,line,length,iostat)
	    if(iostat.ne.0)call bugno('f',iostat)
	  enddo
	  call txtclose(lLog,iostat)
	endif
c
	end
