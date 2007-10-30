#!/bin/csh -ef

cat <<"EOF" >tran_test.for
c************************************************************************
	program ttest
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	real array(MAXBUF)
	common array
	integer n(4),pnt,idx(3,6),mult(3),lu,lu2,i,maxi
	character reorder*6,reorder2*6
	data idx/ 1,2,3, 1,3,2, 2,1,3, 2,3,1, 3,1,2, 3,2,1/
c
	call keyini
	call keyi('imsize',n(1),128)
	call keyi('imsize',n(2),n(1)+1)
	call keyi('imsize',n(3),n(2)+1)
	call keyfin
	n(4) = 1
c
	maxi = max(n(1),n(2),n(3))
	call memalloc(pnt,maxi*maxi,'r')
c
	do i=1,6
	  write(reorder,'(i1,a,2i1)')idx(1,i),'-',idx(2,i),idx(3,i)
	  write(reorder2,'(3i1)')idx(1,i),idx(2,i),idx(3,i)
	  write(*,*)'Reorder ',reorder
	  mult(1) = 1
	  mult(2) = n(1)
	  mult(3) = n(1)*n(2)
	  call trnini(lu,4,n,reorder)
	  call trnini(lu2,4,n,reorder2)
	  call Fill(lu,n(1),n(2),n(3),array(pnt))
	  call Fill(lu2,n(1),n(2),n(3),array(pnt))
	  call UnFill(lu,n(idx(1,i)),n(idx(2,i)),n(idx(3,i)),
     *		mult(idx(1,i)),mult(idx(2,i)),mult(idx(3,i)),
     *		.false.,.true.,.false.,array(pnt))
	  write(*,*)'Reorder ',reorder2
	  call UnFill(lu2,n(idx(1,i)),n(idx(2,i)),n(idx(3,i)),
     *		mult(idx(1,i)),mult(idx(2,i)),mult(idx(3,i)),
     *		.false.,.false.,.false.,array(pnt))
	  call trnfin(lu)
	  call trnfin(lu2)
	enddo
	end
c************************************************************************
	subroutine UnFill(lu,n1,n2,n3,m1,m2,m3,r1,r2,r3,array)
c
	implicit none
	integer lu,n1,n2,n3,m1,m2,m3
	logical r1,r2,r3
	real array(n1,n2)
c------------------------------------------------------------------------
	integer i,j,k,val,nerror
c
	nerror = 0
	do k=1,n3
	  call trnread(lu,array)
	  do j=1,n2
	    do i=1,n1
	      if(r1)then
		val = (n1-i)*m1 + 1
	      else
		val = (i-1)*m1 + 1
	      endif
	      if(r2)then
		val = val + (n2-j)*m2
	      else
		val = val + (j-1)*m2
	      endif
	      if(r3)then
		val = val + (n3-k)*m3
	      else
		val = val + (k-1)*m3
	      endif
	      if(array(i,j).ne.val)nerror = nerror + 1
	    enddo
	  enddo
	enddo
c
	if(nerror.gt.0)then
	  write(*,*)'Number of errors:',nerror
	  call bug('f','There were errors detected')
	endif
	end
c************************************************************************
	subroutine Fill(lu,n1,n2,n3,array)
c
	implicit none
	integer lu,n1,n2,n3
	real array(n1,n2)
c------------------------------------------------------------------------
	integer i,j,k
c
	do k=1,n3
	  do j=1,n2
	    do i=1,n1
	      array(i,j) = (k-1)*n1*n2 + (j-1)*n1 + (i-1) + 1
	    enddo
	  enddo
	  call trnwrite(lu,array)
	enddo
	end
"EOF"

fortran -o tran_test tran_test.for `mirlibs`
tran_test
