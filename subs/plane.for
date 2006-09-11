c************************************************************************
c  History:
c    rjs    nov88 - Created.
c    rjs   7jun89 - Improved PutPlane to not continually rezero a buffer.
c    rjs   8sep89 - Improved efficiency of GetPlane.
c    nebk 10sep89 - Removed a '`)' and added an ENDIF in GetPlane so that 
c                   it would compile.   Efficiency ???
c    rjs   8apr91 - Removed use of blank common. Put checks for image
c		    size exceeding maxdim.
c    rjs  18sep91 - Cosmetic changes to appease bounds checkers.
c************************************************************************
c* GetPlane -- Read portion of a plane, specified by runs format.
c& mjs
c: region-of-interest,runs
c+
	subroutine GetPlane(lu,Run,nRun,xoff,yoff,nx,ny,Out,MaxOut,nOut)
c
	implicit none
	integer lu,nRun,Run(3,nRun+1),MaxOut,nOut,xoff,yoff,nx,ny
	real Out(MaxOut)
c
c  Read in the section of a plane that we are interested in.
c
c  Inputs:
c    lu		The handle of the input file.
c    Run	The runs specifications.
c    nRun	The number of runs.
c    xoff,yoff	Offset to add to the runs before reading.
c    nx,ny	The size of the plane.
c    maxOut	The max number of pixels that can be returned.
c
c  Output:
c    Out	The output array to receive the selected pixels.
c    nOut	The number of pixels in the output array.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer j0,k,i
	real Data(maxdim)
c
	if(nx.gt.maxdim)call bug('f','Image row too large, in GetPlane')
	nOut = 0
	j0 = 0
	do k=1,nRun
	  if(Run(2,k)+xoff.eq.1.and.Run(1,k).ne.Run(1,k+1).and.
     *	    Nout+nx.le.MaxOut)then
	    call xyread(lu,Run(1,k)+yoff,Out(nOut+1))
	    nOut = nOut + Run(3,k) - Run(2,k) + 1
	  else
	    if(Run(1,k).ne.j0)then
	      j0 = Run(1,k)
	      call xyread(lu,j0+yoff,Data)
	    endif
	    if(nOut+Run(3,k)-Run(2,k)+1.gt.MaxOut)
     *	      call bug('f','Selected region too big to handle')
	    do i=Run(2,k)+xoff,Run(3,k)+xoff
	      nOut = nOut + 1
	      Out(nOut) = Data(i)
	    enddo
	  endif
        end do
	end
c************************************************************************
c* PutPlane -- Writes a portion of a plane, specified by runs format.
c& mjs
c: region-of-interest,runs
c+
	subroutine PutPlane(lu,Run,nRun,xoff,yoff,nx,ny,In,nIn)
c
	implicit none
	integer lu,nRun,Run(3,nRun+1),nIn,xoff,yoff,nx,ny
	real In(nIn)
c
c  Write out a subportion of a Miriad image plane. The subportion to
c  write  (given in runs form).
c
c  Input:
c    lu		The handle of the output Miriad file.
c    Run	The runs specifications.
c    nRun	The number of runs.
c    xoff,yoff	Offset to add to the runs before writing.
c    nx,ny	The size of the plane.
c    In		The pixel data.
c    nIn	The total number of pixels in the "In" array.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,k,filled,id
	logical Zeroed
	real Data(maxdim)
c
	if(nx.gt.maxdim)call bug('f','Image row too large, in PutPlane')
	id = 1
	k = 1
	Zeroed = .false.
	do j=1,ny
c
c  Handle case where there is no blanking in this row.
c
	  if(Run(1,k)+yoff.eq.j.and.Run(2,k)+xoff.eq.1.and.
     *	     Run(3,k)+xoff.eq.nx)then
	    call xywrite(lu,j,In(id))
	    id = id + nx
	    k = k + 1
c
c  Handle case where there is blanking in this row.
c
	  else
	    filled = 0
	    dowhile(k.le.nRun.and.Run(1,k)+yoff.eq.j)
	      if(.not.zeroed)then
	        do i=filled+1,Run(2,k)+xoff-1
	          Data(i) = 0
	        enddo
	      endif
	      do i=Run(2,k)+xoff,Run(3,k)+xoff
	        Data(i) = In(id)
	        id = id + 1
	      enddo
	      filled = Run(3,k)+xoff
	      k = k + 1
	    enddo
c
	    if(.not.zeroed)then
	      do i=filled+1,nx
	        Data(i) = 0
	      enddo
	    endif
	    zeroed = filled.eq.0
	    call xywrite(lu,j,Data)
	  endif
c
	enddo
c
	end
c************************************************************************
c* PutRuns -- Writes the blanking file of an image.
c& mjs
c: region-of-interest,runs,blanking
c+
	subroutine PutRuns(lOut,Runs,nRuns,xoff,yoff,nx,ny)
c
	implicit none
	integer lOut,nRuns,xoff,yoff,nx,ny
	integer Runs(3,nRuns+1)
c
c  Write the runs data for the current plane.
c
c  Input:
c    lOut	The handle of the output Miriad file.
c    Run	The runs specifications.
c    nRun	The number of runs.
c    xoff,yoff	Offset to add to the runs before writing.
c    nx,ny	The size of the plane.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer Out(maxdim),j,run,nOut
c
	if(nx.gt.maxdim)call bug('f','Image row too large, in PutRuns')
	run = 1
c
	do j=1,ny
	  nOut = 0
	  dowhile(Runs(1,run)+yoff.eq.j)
	    Out(nOut+1) = Runs(2,run) + xoff
	    Out(nOut+2) = Runs(3,run) + xoff
	    nOut = nOut + 2
	    run = run + 1
	  enddo
	  call xymkwr(lOut,j,Out,nOut)
	enddo
c
	end
