c************************************************************************
	program rpgen
c
c= rpgen - Generate an RPFITS file based on the input parameters
c& rjs
c: data transfer
c+
c	RPGEN is a MIRIAD task which generates a file in RPFITS
c	format. An RPFITS file consists of a number of scans, each with
c       a complete header that describes the data following. RPGEN
c       currently supports writing multiple scans, but all scans will have
c       the same frequency setup. You can concatenate rpfits files to 
c       generate more complex observations.
c
c@ out
c	Name of the output RPFITS file. No default.
c@ source
c       List of source names, default='scp'
c@ radec
c       List of phase centers corresponding to each source. Each center is
c       specified as a pair of numbers (ra and dec), specified in 
c       hh:mm:ss,dd:mm:ss format, or as decimal hours and decimal
c       degrees. The default is 0,-90. 
c@ scan
c       List of scan integration times for each source, in minutes.
c@ time
c       The start time of the observation, followed by the duration in h.
c       This is in the form
c         yymmmdd.ddd,hh.h
c       or
c         yymmmdd:hh:mm:ss.s,hh.h
c       The default is 08JAN01.0,1.0. A function of the start time 
c       is also used as a seed for the random number generator.
c       The sources ares reused, with specified integration time, until
c       the start time + duration is reached.
c@ inttime
c       The correlator integration time in seconds
c@ spwin 
c       Number of spectral windows, followed by a triple of
c       number of channels, frequency in GHz, bandwidth in GHz
c       for each spectral window
c@ stokes
c       List of stokes parameters, default is xx,xy,yx,yy
c@ options
c       cabb - interpret spwin to match CABB output (combine overlaps)
c
c$Id: rpgen.for,v 1.5 2013/08/30 01:49:21 wie017 Exp $
c--
c The program rpgen is roughly based on uvgen but currently
c dummy fills most of the data fields.
c-----------------------------------------------------------------------
	integer MAXSRC,MAXPOL,MAXWIN
	parameter(MAXSRC=32,MAXPOL=4,MAXWIN=18)
c
	character sources(MAXSRC)*24,out*64,version*72
	integer nsrc,nspw,npol,pol(MAXPOL),nchan(MAXWIN)
	integer i,nant,jstat,isrc
	double precision radec(MAXSRC,2),freq(MAXWIN),bw(MAXWIN),
     *    scantime(MAXSRC),def,starttime,duration,inttime,t,tscan
        logical docabb
	integer PolRR,PolLL,PolRL,PolLR,PolXX,PolYY,PolXY,PolYX
	parameter(PolRR=-1,PolLL=-2,PolRL=-3,PolLR=-4)
	parameter(       PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
c
c  Externals.
c
	character versan*72
c-----------------------------------------------------------------------
      version = versan ('rpgen',
     :                  '$Revision: 1.5 $',
     :                  '$Date: 2013/08/30 01:49:21 $')
c
c  Get the input parameters.
c
	call keyini
	call keya('out',out,' ')
	if(out.eq.' ')
     *	  call bug('f','Output name must be given')
        call mkeya('source',sources,MAXSRC,nsrc)
        do i=1,nsrc
          call keyt('radec',radec(i,1),'hms',0.0d0)
          call keyt('radec',radec(i,2),'dms',-90.0d0)
        enddo
        do i=1,nsrc
          call keyd('scan',scantime(i),10.0d0)
          scantime(i)=scantime(i)/1440.d0
        enddo       
        call dayjul('08JAN01.0',def)
        call keyt('time',starttime,'atime',def)
        call keyd('time',duration,1.0d0)
        duration=duration/24.0d0
        call keyd('inttime',inttime,10.0d0)
        inttime=inttime/86400.d0

        call keyi('spwin',nspw,1)
        do i=1,nspw
          call keyi('spwin',nchan(i),1)
          call keyd('spwin',freq(i),100.d0)
          call keyd('spwin',bw(i),0.1d0)
c
c         mysteriously roundoff errors creep into the frequencies
c          
          freq(i)=nint(freq(i)*1d6)/1d6
          bw(i)=nint(bw(i)*1d6)/1d6
        enddo
        call getpol(pol,npol,MAXPOL)
	call getopt(docabb)
c
c       dummy fill all the data for now
c
        nant=6
        
        call RPOpen(out,jstat)
        if (jstat.ne.0) call bug('f','Error opening output file')
        
        t=starttime
        tscan=0.d0
        isrc=1
        do while (t.lt.(starttime+duration))
        
c
c         write the scan header
c         
          if (tscan.eq.0.0d0) then
            jstat=-1
            call RPWrite(jstat,t,sources(isrc),radec(isrc,1),
     *          radec(isrc,2),nspw,nchan,freq,bw,
     *          npol,pol,nant)
          endif
c
c         write data for one integration
c
          jstat=0
          call RPWrite(jstat,t,sources(isrc),radec(isrc,1),
     *        radec(isrc,2),nspw,nchan,freq,bw,npol,pol,nant)      
          
          t=t+inttime
          tscan=tscan+inttime
          if (tscan.gt.scantime(isrc)) then
            isrc=isrc+1
            if (isrc.gt.nsrc) isrc=1
            tscan=0.d0
          endif
            
        enddo
        
        
        call RPClose(jstat)
        
c
        end
c************************************************************************
	subroutine RPClose(jstat)
c
	integer jstat
c------------------------------------------------------------------------
	integer flag,baseln,bin,ifno,srcno
	real ut,u,v,w,weight
	complex vis
c
	jstat = 1
	call rpfitsout(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
	if(jstat.ne.0)call bug('w','Error closing file')
	end
c************************************************************************
	subroutine RPOpen(out,jstat)
c
	character out*(*)
	integer jstat
c
c  Open the RPFITS file.
c------------------------------------------------------------------------
	include 'rpfits.inc'
c
	integer flag,baseln,bin,ifno,srcno
	real ut,u,v,w,weight
	complex vis
c
	file = out
c
	jstat = -3
	an_found = .false.
	call rpfitsout(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
	if(jstat.ne.0) call bug('w','Error opening RPFITS file')
	end
        
c************************************************************************
	subroutine RPWrite(jstat,t,source,raj,decj,
     *        nspw,nchan,frq,bw,npol,pol,numant)
c
	integer jstat,nspw,nchan(nspw),npol,pol(npol),numant
        double precision t,raj,decj,frq(nspw),bw(nspw)
        character*(*) source
c
c  Write header or data
c------------------------------------------------------------------------
	include 'rpfits.inc'
	include 'maxdim.h'
        include 'mirconst.h'
c
        integer maxpol,maxdata
        parameter (maxpol=4,maxdata=MAXCHAN*maxpol)

	integer flag,baseln,bin,ifno,srcno
	real ut,u,v,w,weight,dx
	complex vis(maxdata)
        integer ant,i,j,k,ant1,ant2
        character*2 polcode(-8:-1)/'YX','XY','YY','XX',
     *                             'LR','LR','LL','RR'/
        character*40 config
        real sc_buf(max_sc*max_if*ant_max)
        equivalence (sc_buf(1), sc_cal(1,1,1))
c
        ut=(t-int(t))*86400.d0
        
        write(*,*) jstat, t

	if (jstat.eq.-1) then
c
c  Write header
c
          version='rpgen 5-dec-07'
	  data_format = 2
          object=source
          datobs='2007-12-05'
          rp_observer='MHW'
          instrument='ATCA'
          bunit='Jy'
          n_su=1
          su_num(1)=1
          su_name(1)=source
          srcno=1
          su_ra(1)=raj
          su_dec(1)=decj
          ra=raj
          dec=decj
          coord='J2000'
          intime=10
          nant=numant
          
          do ant=1,nant
              feed_type(1,ant)='X'
              feed_type(2,ant)='Y'
              write(sta(ant),'(''CA0'',I1)') ant
              x(ant)=ant*10.d0
              y(ant)=ant*100.d0
              z(ant)=ant*1000.d0
              ant_num(ant)=ant
          enddo
          
          n_if=nspw
          do i=1,nspw
            if_chain(i)=i
c
c  Assign if_chain to first frq/bw that includes this freq
c
            if (i.gt.2) then
              do j=2,1,-1
                if (frq(i).gt.frq(j)-bw(j)/2.and.
     *              frq(i).lt.frq(j)+bw(j)/2) then
                  if_chain(i)=j
                endif
              enddo
           endif                
            if_simul(i)=1
            if_nfreq(i)=nchan(i)
            if_num(i)=i
            if_sampl(i)=2
            if_nstok(i)=npol
            if_ref(i)=nchan(i)/2+1
            if_bw(i)=bw(i)*1.d9
c            if ((nchan(i)/2)*2.lt.nchan(i)) if_bw(i)=
c     *         if_bw(i)/(nchan(i)-1)*nchan(i)
            if_invert(i)=1
            if_freq(i)=frq(i)*1.d9
            do j=1,npol
              if_cstok(j,i)=polcode(pol(j))
            enddo              
          enddo
          freq = if_freq(1)
          dfreq = if_bw(1)/(2*(nchan(1)/2))
          rfreq = if_freq(1)
          pm_ra=0.d0
          pm_dec=0.d0
          pm_epoch=0.d0
          
          write(card(1),'(A,A,A)') 'OBSTYPE = ','''        ''',
     *       '  / Observation type'
          write(card(2),'(A,F11.9,1x,F12.9,A)') 'PNTCENTR= ',ra,dec,
     *       ' / Pointing centre'
          config = 'full_128_2'
          write(card(3),'(A,A)') 'CORR_CFG=  ',config
          write(card(4),'(A,A)') 'SCANTYPE= ',' '
          write(card(5),'(A,A)') 'COORDTYP= ','J2000'
          write(card(6),'(A,A)') 'LINEMODE= ','F F / T=line F=continuum'
          write(card(7),'(A,A)') 'CACALCNT=      0',' / CACAL counter'
          ncard=7
          
          
	  call rpfitsout(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
        else if (jstat.eq.0) then

c
c  First write the syscal data
c
           write(*,*) ' syscal '
           baseln=-1
           sc_q=16
           sc_ant=numant+1
           sc_if=nspw
           sc_srcno=1
           sc_ut=ut
           k=0
           do i=1,sc_ant
             do j=1,sc_if
               if (i.le.numant) then
                 sc_buf(k+1) =i    !ant
                 sc_buf(k+2) =j    !freq band
                 sc_buf(k+3) =0.02 !xphase
                 sc_buf(k+4) =22.3 !sqrt(10*tsysA)*sign(Tsys enabled)
                 sc_buf(k+5) =22.4 !sqrt(10*tsysB)*sign(Tsys enabled)
                 sc_buf(k+6) =17.3 !samp stats A -
                 sc_buf(k+7) =50.0 !             0
                 sc_buf(k+8) =17.2 !             +
                 sc_buf(k+9) =17.2 !samp stats B -
                 sc_buf(k+10)=50.0 !             0
                 sc_buf(k+11)=17.3 !             +
                 sc_buf(k+12)=0.   !par. angle
                 sc_buf(k+13)=0    ! flag bit 0 offsrc, 1 syncA, 2 syncB
                 sc_buf(k+14)=10.1 !xyamp
                 sc_buf(k+15)=5.1  !max track error
                 sc_buf(k+16)=1.2  !rms tracking error
               else
                 sc_buf(k+1) =0 ! mon data
                 sc_buf(k+2) =25.0    !temperature
                 sc_buf(k+3) =1010.0  !pressure
                 sc_buf(k+4) =33.0    !humidity
                 sc_buf(k+5) =7.1     !windspeed
                 sc_buf(k+6) =12.3    !wind dir
                 sc_buf(k+7) =0       !weather flag
                 sc_buf(k+8) =0.1     !rain gauge
                 sc_buf(k+9) =123.0   !seeing monitor phase
                 sc_buf(k+10)=234.0   !seeing monitor rms phase
                 sc_buf(k+11)=0       !seeing monitor flag
                 sc_buf(k+12)=0
                 sc_buf(k+13)=0
                 sc_buf(k+14)=0
                 sc_buf(k+15)=0
                 sc_buf(k+16)=0
               
               endif
               
               k=k+sc_q
             enddo
           enddo 
	   call rpfitsout(jstat,sc_cal,weight,baseln,ut,u,v,w,
     *	                 sc_ant,sc_if,sc_q,srcno)
           if (jstat.ne.0) call bug('f','Error writing syscal data') 

c
c  Write the visibility data
c
           bin=1
           srcno=1
           write(*,*) ' vis data ',nspw,numant
           do ifno=1,nspw
             do ant1=1,numant
               do ant2=ant1,numant 
                 baseln=256*ant1+ant2
                 k=1
                 dx=nchan(ifno)/10.0
                 do i=1,nchan(ifno)
                   do j=1,npol
                     if (j.le.2) then
                       vis(k)=cmplx(1.0+srcno/100.0,0.0)
                     else
                       vis(k)=cmplx(0.001*j,1.e-7*i)
                     endif
                     if (i.lt.dx)  vis(k)=vis(k)*sin(i/dx*PI_2)
                     if (nchan(ifno)-i.lt.dx) vis(k)=vis(k)*
     *                    sin((nchan(ifno)-i)/dx*PI_2)
c                     !  if (ant1.eq.1.and.ant2.eq.1.and.j.eq.1)
c     *              !      write(*,*) i,abs(vis(k)),nchan(ifno),dx
                     k=k+1
                   enddo
                 enddo
                 weight=1.0
                 u=ant2-ant1
                 v=ant2-ant1
                 w=0
                 flag=0
                 
	         call rpfitsout(jstat,vis,weight,baseln,ut,u,v,w,flag,
     *						bin,ifno,srcno)
                 if (jstat.ne.0) 
     *             call bug('f','Error writing vis data') 
               enddo
             enddo
           enddo 
     
     
               
        endif
	if(jstat.ne.0) call bug('w','Error writing to RPFITS file')
	end
        
c************************************************************************

c Routine getpol, stolen unmodified from uvgen
	subroutine GetPol(pol,npol,maxpol)
c
	implicit none
	integer npol,maxpol
	integer pol(maxpol)
c
c  Get the polarisations that the user wants to form.
c
c  Input:
c    maxpol	Max number of polarisations than this can handle.
c  Output:
c    npol	The number of polarisations that the user wants to form.
c    pol	The polarisations that the user wants to form.
c------------------------------------------------------------------------
	integer i,j,t
	character user*4
c
c  Externals.
c
	integer PolsP2C
	character PolsC2P*2
	logical keyprsnt
c
	call keya('stokes',user,'i')
	npol = 0
	dowhile(user.ne.' '.and.npol.lt.maxpol)
	  npol = npol + 1
	  pol(npol) = PolsP2C(user)
	  call keya('stokes',user,' ')
	enddo
	if(keyprsnt('stokes'))
     *	  call bug('f','Too many stokes/polarizations for me to handle')
c
c  Sort the polarisations into descending order. Bubble sort because who cares
c  with only 4 elements.
c
	do j=1,npol-1
	  do i=1,npol-j
	    if(pol(i).lt.pol(i+1))then
	      t = pol(i)
	      pol(i) = pol(i+1)
	      pol(i+1) = t
	    endif
	  enddo
	enddo
c
c  Squeeze out replications (pretty paranoid eh?).
c
	j = 1
	do i=2,npol
	  if(pol(i).ne.pol(j))then
	    j = j + 1
	    pol(j) = pol(i)
	  endif
	enddo
	npol = j
c
c  Check that the max polarisation is 1.
c
	if(pol(1).gt.1)call bug('f','Unsupported polarisation '//
     *	  PolsC2P(pol(1)))
c
	end
        
c 
c@ options
c       This gives extra processing options. Several options can be given,
c       each separated by commas. They may be abbreviated to the minimum
c       needed to avoid ambiguity. Possible options are:
c          'cabb'       Try to produce output matching that of CABB
        subroutine GetOpt(docabb)
c
        implicit none
        logical docabb
c
c  Determine extra processing options.
c
c  Output:
c    docabb     Try to produce output matching that of CABB
c-----------------------------------------------------------------------
        integer nopt
        parameter(nopt=1)
        character opts(nopt)*9
        logical present(nopt)
        data opts/'cabb     '/
c
        call options('options',opts,present,nopt)
        docabb  = present(1)
c
        end
