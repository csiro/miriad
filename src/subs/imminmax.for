c************************************************************************
c*ImMinMax -- Return Miriad image minimum and maximum value.
c&rjs
c:image-data
c+
	subroutine ImMinMax(lun,naxis,nsize,rmin,rmax)
c
	implicit none
	integer lun,naxis,nsize(naxis)
	real rmin,rmax
c
c  Determine the min and max values of a Miriad image. This first checks
c  the image header for the information. If it is not present in the header,
c  it calculates it directly from the data, and then stores it in the header.
c
c  Input:
c    lun	The handle of the input image.
c    naxis	The number of axes.
c    nsize	The length of each axis.
c
c  Output:
c    rmin,rmax	The min and max value of the image.
c
c--
c  rjs         89  Initial version
c  nebk 04-may-89  Add message telling user min and max being found
c                  and remove return statement from middle of code
c  rjs  28-jun-89  Added a check that the internal buffer is big enough.
c  rjs  22-feb-93  Include maxnax.h.
c  rjs   5-jan-94  Check whether output items are writable.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
c
	real dat(maxdim)
	integer i,j,k,plane(maxnax),imin,imax
	logical done,first,present
	character mode*8
c
c  Externals.
c
	integer ismin,ismax
	logical hdprsnt
c
c  Check the image header for the min and max value.
c
	present = hdprsnt(lun,'datamin')
	if(present) present = hdprsnt(lun,'datamax')
	if(present)then
	  call rdhdr(lun,'datamin',rmin,0.)
	  call rdhdr(lun,'datamax',rmax,0.)
	else
c
c  The header did not contain the min and max. Calculate it the hard
c  way.
c
	  if(naxis.gt.maxnax)call bug('f','Too many axes')
c
c  Do one dimensional images, to please Bob Loushin.
c
          call output ('Finding image minimum and maximum')
	  if(nsize(1).gt.maxdim)
     *	    call bug('f','First dimension too big, in ImMinMax')
  	  if(naxis.eq.1)then
	    call xyread(lun,1,dat)
  	    imin = ismin(nsize(1),dat,1)
	    imax = ismax(nsize(1),dat,1)
	    rmin = dat(imin)
	    rmax = dat(imax)
c
c  Else do 2 or higher dimensions.
c
	  else
	    do i=1,naxis
	      plane(i) = 1
	    enddo
c
c  Calculate the min and max values.
c
	    first = .true.
	    done = .false.
	    dowhile(.not.done)
	      if(naxis.gt.2)call xysetpl(lun,naxis-2,plane(3))
	      do j=1,nsize(2)
	        call xyread(lun,j,dat)
  	        imin = ismin(nsize(1),dat,1)
	        imax = ismax(nsize(1),dat,1)
	        if(first)then
	          rmin = dat(imin)
	          rmax = dat(imax)
	          first = .false.
	        else
	          rmin = min(rmin,dat(imin))
	          rmax = max(rmax,dat(imax))
	        endif
	      enddo
c
c  Increment the plane to access.
c
	      k = 3
	      done = .true.
c
	      dowhile(done.and.k.le.naxis)
	        done = plane(k).ge.nsize(k)
	        if(done)then
	          plane(k) = 1
	        else
	          plane(k) = plane(k) + 1
	        endif
	        k = k + 1
	      enddo
	    enddo
	  endif
c
c  We have found the image min and max. Write this to the image header.
c
	  call hmode(lun,mode)
	  if(index(mode,'w').ne.0)then
	    call wrhdr(lun,'datamin',rmin)
	    call wrhdr(lun,'datamax',rmax)
	  endif
        endif
c
	end
