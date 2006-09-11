c************************************************************************
	program sigest
	implicit none
c
c= sigest - Estimate the rms level in an image.
c& rjs
c: map analysis
c+
c	SIGEST is a Miriad task which attempts to estimate the rms noise
c	level in an image. It does this by determining statistics of a
c	clipped version of the image data. Based on the statistics of
c	clipped Gaussian noise, the rms is then estimated. This
c	assumes that the image pixels, after clipping, are dominated by noise. 
c
c	SIGEST will work poorly on images containing significant sidelobes.
c@ in
c	The input image dataset. No default.
c@ clip
c	The clip level to use. Pixels are excluded from the calculation
c	if their absolute value is greater than the clip value. Ideally
c	the clip should be about 2 to 3 times the rms noise level.
c	The default uses an initial estimate of the rms to set the clip
c	at 2.5 times the rms.
c@ region
c	The standard image region of interest of the input. See the help
c	on "region" for more information. The default is the entire image.
c--
c
c  History:
c    14apr99 rjs  Initial version.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mirconst.h'
	integer MAXBOXES,MAXRUNS,MAXITER
	character version*(*)
	parameter(MAXBOXES=2048,MAXRUNS=80000,MAXITER=30)
	parameter(version = 'version 14-Apr-1999' )
c
	character file*64,line*64
	integer nsize(MAXNAX),plane(MAXNAX),blc(MAXNAX),trc(MAXNAX)
	integer i,j,k,naxis,tno,n,no,niter
	real clip,fac,ofac,t,data(MAXDIM)
	double precision rms
	logical defclip,done
	integer boxes(MAXBOXES),runs(3,MAXRUNS),nruns
c
c  Externals.
c
	logical keyprsnt
	real errfun
	character itoaf*5
c
c  Open the input file, and make sure that I can handle it.
c
	call output( 'Sigest: '//version )
	call keyini
	call keya('in',file,' ')
	if(file.eq.' ')call bug('f','Input file must be given')
	call BoxInput('region',file,boxes,maxboxes)
	defclip = .not.keyprsnt('clip')
        call keyr('clip',clip,0.0)
	if(clip.lt.0)call bug('f','Invalid clip value')
	call keyfin
c
	call xyopen(tno,file,'old',MAXNAX,nsize)
	call rdhdi(tno,'naxis',naxis,0)
	naxis = min(naxis,MAXNAX)
	if(nsize(1).gt.MAXDIM)call bug('f','Input file too big for me')
c
c  Set up the region of interest.
c
	call BoxMask(tno,boxes,maxboxes)
	call BoxSet(boxes,MAXNAX,nsize,' ')
	call BoxInfo(boxes,MAXNAX,blc,trc)
c
c  If no clip was given, compute the rms of the overall image.
c
	if(defclip)then
	  call output('Computing the clip level')
	  rms = 0
	  n = 0
	  do i=1,MAXNAX
	    plane(i) = blc(i)
	  enddo
	  done = .false.
	  dowhile(.not.done)
	    call BoxRuns(MAXNAX-2,plane(3),' ',boxes,runs,maxruns,nruns,
     *					    blc(1),trc(1),blc(2),trc(2))
	    call xysetpl(tno,MAXNAX-2,plane(3))
	    j = 0
	    do k=1,nruns
	      if(Runs(1,k).ne.j)then
		j = Runs(1,k)
		call xyread(tno,j,data)
	      endif
	      do i=Runs(2,k),Runs(3,k)
		rms = rms + abs(data(i))
		n = n + 1
	      enddo
	    enddo
	    call planeinc(MAXNAX-2,blc(3),trc(3),plane(3),done)
	  enddo
	  if(n.le.0)call bug('f','No valid data found')
	  clip = 2.5*sqrt(0.5*PI)*(rms/n)
	  write(line,'(a,1pg10.3)')'Using a clip level of',clip
	  call output(line)
	endif
c
c  Now lets accumulate again, but this tiume clipping.
c
	rms = 0
	n = 0
	no = 0
	do i=1,MAXNAX
	  plane(i) = blc(i)
	enddo
	done = .false.
	dowhile(.not.done)
	  call BoxRuns(MAXNAX-2,plane(3),' ',boxes,runs,maxruns,nruns,
     *					    blc(1),trc(1),blc(2),trc(2))
	  call xysetpl(tno,MAXNAX-2,plane(3))
	  j = 0
	  do k=1,nruns
	    if(Runs(1,k).ne.j)then
	      j = Runs(1,k)
	      call xyread(tno,j,data)
	    endif
	    do i=Runs(2,k),Runs(3,k)
	      if(abs(data(i)).lt.clip)then
	        rms = rms + data(i)*data(i)
	        n = n + 1
	      else
		no = no + 1
	      endif
	    enddo
	  enddo
	  call planeinc(MAXNAX-2,blc(3),trc(3),plane(3),done)
	enddo
c
	if(n.le.0)call bug('f','No valid data left after clipping')
	rms = sqrt(rms/n)
	call output('Percentage of pixels within clip limit: '//
     *		itoaf(nint(100.0*n/real(n+no))))
	niter = 0
	ofac = -1
	fac = 1
	dowhile(niter.lt.MAXITER.and.abs(fac-ofac).gt.0.01)
	  niter = niter + 1
	  ofac = fac
	  t = clip/(fac*rms)
	
	  fac = sqrt(1.0/(1.0 - 
     *	    sqrt(2.0/PI)*t*exp(-0.5*t*t)/errfun(t/sqrt(2.0))))
	enddo
c
	if(niter.gt.MAXITER)
     *	  call bug('w','Correction algorithm failed to converge')
	write(line,'(a,1pe10.3)')'Estimated rms is',fac*rms
	call output(line)
c
	call rdhdr(tno,'rms',clip,-1.0)
	if(clip.gt.0)then
	  fac = fac*rms/clip
	  write(line,'(a,1pe10.3)')
     *	    'This is the theoretical rms times a factor of',fac
	  call output(line)
	endif
c
	call xyclose(tno)
	end
c************************************************************************
	subroutine planeinc(n,blc,trc,plane,done)
c
	implicit none
	integer n,blc(n),trc(n),plane(n)
	logical done
c
c  Move to the next plane.
c
c------------------------------------------------------------------------
	integer k
c
	k = 1
	done = .true.
c
	do while(done.and.k.le.n)
	  done = plane(k).ge.trc(k)
	  if(done)then
	    plane(k) = blc(k)
	  else
	    plane(k) = plane(k) + 1
	  endif
	  k = k + 1
	enddo
	end
