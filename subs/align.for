c************************************************************************
c  A set of routines to check that two images represent the same
c  projection and to determine the alignment between them.
c
c  History:
c    rjs  12oct95 Derived from routines in CLEAN.
c    rjs  15jul97 Make AlignIni less fussy about the 3rd axis
c		  it there is only one pixel there.
c************************************************************************
	subroutine AlignIni(lModel,lMap,mMap,nMap,oMap,xoff,yoff,zoff)
c
	implicit none
	integer lModel,lMap
	integer mMap,nMap,oMap,xoff,yoff,zoff
c
c  Determine the alignment parameters between the map and the model.
c  This insists that they line up on pixels.
c
c  Input:
c    lModel	Handle of the model file.
c    lMap	Handle of the map file.
c    mMap,nMap,oMap Map dimensions.
c
c  Output:
c    xoff,yoff,zoff These values have to be added to the grid units of
c		the model to convert them to grid units of the map.
c
c------------------------------------------------------------------------
	integer i,offset(3),nsize(3),naxis,n3map,n3mod
	character num*1
	real vM,dM,rM,vE,dE,rE,temp
c
c  Externals.
c
	character itoaf*2
c
	nsize(1) = mMap
	nsize(2) = nMap
	nsize(3) = oMap
c
c  Decide whether we should process 2 or 3 axes.
c
	call rdhdi(lMap,'naxis3',n3map,1)
	call rdhdi(lModel,'naxis3',n3mod,1)
	if(n3map.eq.1.and.n3mod.eq.1)then
	  naxis = 2
	  offset(3) = 0
	else
	  naxis = 3
	endif
c
	do i=1,naxis
	  num = itoaf(i)
	  call rdhdr(lModel,'crval'//num,vM,0.)
	  call rdhdr(lModel,'cdelt'//num,dM,1.)
	  call rdhdr(lModel,'crpix'//num,rM,1.)
	  call rdhdr(lMap,'crval'//num,vE,0.)
	  call rdhdr(lMap,'cdelt'//num,dE,1.)
	  call rdhdr(lMap,'crpix'//num,rE,1.)
	  if(dE.eq.0)
     *	    call bug('f','Increment on axis '//num//' is zero')
	  if(abs(vM-vE).gt.abs(dE))
     *	    call bug('f','Reference pixel values differ')
	  temp = (rM-rE)
	  offset(i) = nint(temp)
	  if(abs(offset(i)-temp).gt.0.05)
     *	    call bug('f','Input images do not align on pixels')
	  if(abs(dM-dE).ge.0.001*abs(dM))
     *	    call bug('f','Input image increments are different')
	  if(offset(i).gt.0.or.offset(i).le.-nsize(i))
     *	    call bug('w','Input images do not overlap well')
	enddo
c
	xoff = offset(1)
	yoff = offset(2)
	zoff = offset(3)
c
	end
c************************************************************************
	subroutine AlignGet(lModel,Runs,nRuns,k,ioff,joff,koff,
     *	  n1,n2,n3,Data,MaxData,nData)
c
	implicit none
	integer lModel,ioff,joff,koff,nRuns,Runs(3,nRuns),k,n1,n2,n3
	integer nData,MaxData
	real Data(MaxData)
c
c  Get the portion of a image that overlaps with the selected region.
c
c  Input:
c  Output:
c    Data	The image data.
c    nData	Number of pixels.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,i1,i2,j,jsave,ipnt,iRuns
	real Buf(MAXDIM)
c
	call BoxCount(Runs,nRuns,nData)
	if(nData.gt.MaxData)call bug('f','Buffer too small in AlignGet')
c
	if(k+koff.lt.1.or.k+koff.gt.n3)then
	  do i=1,nData
	    Data(i) = 0
	  enddo
	  ipnt = nData
	else
	  ipnt = 0
	  jsave = 0
	  call xysetpl(lModel,1,k+koff)
	  do iRuns=1,nRuns
	    j = Runs(1,iRuns) + joff
	    i1 = Runs(2,iRuns) + ioff
	    i2 = Runs(3,iRuns) + ioff
	    if(j.lt.1.or.j.gt.n2.or.i1.gt.n1.or.i2.lt.1)then
	      do i=i1,i2
		ipnt = ipnt + 1
		Data(ipnt) = 0
	      enddo
	    else
	      if(j.ne.jsave)call xyread(lModel,j,Buf)
	      jsave = j
	      do i=i1,0
		ipnt = ipnt + 1
		Data(ipnt) = 0
	      enddo
	      do i=max(i1,1),min(i2,n1)
		ipnt = ipnt + 1
		Data(ipnt) = Buf(i)
	      enddo
	      do i=n1+1,i2
		ipnt = ipnt + 1
		Data(ipnt) = 0
	      enddo
	    endif
	  enddo
	endif
c
	if(ipnt.ne.nData)call bug('f','Algorithmic failure in AlignGet')
	end
