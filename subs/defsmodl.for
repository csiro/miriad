c************************************************************************
	subroutine defsmodl(tno)
c
	implicit none
	integer tno
c
c  Determine the default sensitivity model for this dataset, and write
c  it to the dataset explicitly.
c
c------------------------------------------------------------------------
	character telescop*16,smodel*8
c
	call rdhda(tno,'senmodel',smodel,' ')
c
	if(smodel.eq.' ')then
	  call uvrdvra(tno,'telescop',telescop,' ')
c
	  call lcase(telescop)
	call output(telescop)
	  if(telescop.eq.'atca')then
	    call wrhda(tno,'senmodel','GSV')
	  else
	    call wrhda(tno,'senmodel','VFIX')
	  endif
	endif
	end
