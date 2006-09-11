c************************************************************************
c  Arg routines -- simulate the iargc and getarg routines on VMS.
c
c  History:
c    rjs  Dark-ages Original version.
c    rjs  22oct90   Fudge to getarg when the caller wants to know the
c		    progname.
c************************************************************************
	integer function iargc()
c
	implicit none
c
c  Return the number of arguments on the command line.
c
c------------------------------------------------------------------------
	include 'args.h'
	if(.not.init)call arginit
	iargc = nargs
	end
c************************************************************************
	subroutine getarg(k,arg)
c
	implicit none
	integer k
	character arg*(*)
c
c  Return the k'th argument from the command line.
c
c  Input:
c    k		The command line argument to return.
c
c  Output:
c    arg	The commandline argument.
c------------------------------------------------------------------------
	include 'args.h'
	if(.not.init)call arginit
	if(k.eq.0)then
	  arg = '(unknown progname)'
	else if(k.lt.1.or.k.gt.nargs)then
	  arg = ' '
	else
	  arg = line(range(1,k):range(2,k))
	endif
	end
c************************************************************************
	subroutine arginit
c
	implicit none
c
c  Initalise the command argument routines.
c
c------------------------------------------------------------------------
	include 'args.h'
	integer*2 length
	logical quoted,skipping,ok
	integer k
c
c  Externals.
c
	logical lib$get_foreign
c
c  Get the command line.
c
	ok = lib$get_foreign(line,,length)
	if(.not.ok)call bugno('w',ok)
	init = .true.
c
c  Determine the range of each argument.
c
	skipping = .true.
	quoted = .false.
	k = 0
	nargs = 0
	dowhile(k.lt.length)
	  k = k + 1
	  if(quoted)then
	    quoted = line(k:k).ne.'"'
	  else
c
	    if(skipping.and.line(k:k).le.' ')then
	      continue
	    else if(skipping)then
	      nargs = nargs + 1
	      if(nargs.gt.MAXARG)call bug('f','Too many args')
	      range(1,nargs) = k
	      skipping = .false.
	    else if(line(k:k).le.' ')then
	      range(2,nargs) = k - 1
	      skipping = .true.
	    endif
c
	    if(line(k:k).ge.'A'.and.line(k:k).le.'Z')then
	      line(k:k) = char(ichar(line(k:k)) - ichar('A')+ichar('a'))
	    else if(line(k:k).eq.'"')then
	      quoted = .true.
	    endif
c
	  endif
	enddo
c
c  Finish up.
c
	if(.not.skipping)range(2,nargs) = length
	if(quoted)call bug('w','Odd number of quotes')
	end
