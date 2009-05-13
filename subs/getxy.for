c************************************************************************
c* GetXY -- Return information on the XY phase of a dataset.
c& rjs
c: utilities
c+
	subroutine GetXY(tno,fac,xyphase,count,mxant,nants)
c
	implicit none
	integer tno,mxant,nants,count(mxant)
	real xyphase(mxant),fac
c
c  This determines the average XY phase, and its standard deviation, of
c  the gains of a visibility data-set. The information is derived
c  from the "gains" item.
c
c  Input:
c    tno	Handle of the input visibility data-set.
c    mxant	Max number of antennae.
c  Output:
c    fac        RMS gain amplitude.
c    xyphase	Mean xyphase, in radians.
c    count	Number of gains used in the average. This can be 0!
c    nants	Number of antenna found. This will be zero of the xyphase
c		could not be determined at all.
c--
c  History:
c    rjs  ???????	Original version
c    rjs  01may09	Fix bug to return RMS gain amplitude. 	
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	integer item,iostat,ngains,nfeeds,ntau,nsols,i,j,k,off,ntot
	real theta,sumsq
	complex gx,gy,gains(3*MAXANT)
c
c  Initialise the counters.
c
	fac = 0
	sumsq = 0
	ntot = 0
	do i=1,mxant
	  count(i) = 0
	  xyphase(i) = 0
	enddo
c
c  Determine the number of gains, antennae, etc.
c
	call rdhdi(tno,'ngains',ngains,0)
	call rdhdi(tno,'nfeeds',nfeeds,1)
	call rdhdi(tno,'ntau',  ntau,  0)
c
	nants = 0
	if(nfeeds.ne.2) return
	call haccess(tno,item,'gains','read',iostat)
	if(iostat.ne.0) return
c
	nants = ngains / (nfeeds + ntau)
	if(nants.le.0) call bug('f','Bad number of antennae gains')
	if(nants.gt.MAXANT)
     *	  call bug('f','Too many antennae to handle')
	call rdhdi(tno,'nsols',nsols,0)
c
c  Read through the gains, compute the XY phase at each antenna, and
c  accumulate it.
c
	do j=1,nsols
	  off = 8*(ngains+1)*(j-1) + 16
	  call hreadr(item,Gains,off,8*ngains,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error while reading the gains table')
	    call bugno('f',iostat)
	  endif
	  k = 1
	  do i=1,min(mxant,nants)
	    gx = gains(k)
	    gy = gains(k+1)
	    if(abs(real(gx))+abs(aimag(gx)).gt.0.and.
     *	       abs(real(gy))+abs(aimag(gy)).gt.0)then
	      sumsq = sumsq + real(gx)**2 + aimag(gx)**2 +
     *			      real(gy)**2 + aimag(gy)**2
	      ntot = ntot + 2
	      gx = gx / gy
	      theta = atan2(aimag(gx),real(gx))
	      if(count(i).gt.0)theta = theta +
     *		  2*pi*nint((xyphase(i)/count(i) - theta)/(2*pi))
	      count(i) = count(i) + 1
	      xyphase(i) = xyphase(i) + theta
	    endif
	    k = k + nfeeds + ntau
	  enddo
	enddo
c
	call hdaccess(item,iostat)
c
c  We have the needed info. Now work out the mean and standard deviation.
c
	do i=1,mxant
	  if(count(i).gt.0)then
	    theta = xyphase(i) / count(i)
	    xyphase(i) = theta
	  endif
	enddo
	if(sumsq.gt.0)fac = sqrt(ntot/sumsq)
c
	end
	
