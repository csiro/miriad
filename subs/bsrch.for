c************************************************************************
c  Binary search routines.
c  History:
c    rjs   8nov89 Adapted from the binary search routine in FITS.
c    rjs  16sep93 Renamed the routines to binsrch, to avoid conflict
c		  with the spice library.
c************************************************************************
c*binsrcha -- Search a list of strings for a particular string.
c:search,binary-search
c+
	integer function binsrcha(key,keys,nkeys)
c
	implicit none
	integer nkeys
	character key*(*),keys(nkeys)*(*)
c
c  Search for a particular string in a list of strings. Return the
c  index of the string. If the string is not found, return 0.
c  A binary search is used.
c
c  Input:
c    key	The string to search for.
c    keys	A list of strings. These are assumed to be in alphabetic
c		order to allow a binary search.
c    nkeys	The number of strings.
c
c  Output:
c    binsrcha	Either the index of the string (if it is found in the
c		list), or zero.
c--
c------------------------------------------------------------------------
	integer j,k,l
c
	k = 1
	l = nkeys
	binsrcha = 0
	do while(k.le.l)
	  j = (k+l)/2
	  if(key.lt.keys(j))then
	    l = j - 1
	  else if(key.gt.keys(j))then
	    k = j + 1
	  else
	    binsrcha = j
	    k = l + 1
	  endif
	enddo
	end
c************************************************************************
c*binsrchi -- Search a list of integers for a particular integer.
c:search,binary-search
c+
	integer function binsrchi(key,keys,nkeys)
c
	implicit none
	integer nkeys
	integer key,keys(nkeys)
c
c  Search for a particular integer in a list of integers. Return the
c  index of the integer. If the integer is not found, return 0.
c  A binary search is used.
c
c  Input:
c    key	The integer to search for.
c    keys	A list of integers. These are assumed to be in alphabetic
c		order to allow a binary search.
c    nkeys	The number of integers.
c
c  Output:
c    binsrchi	Either the index of the integer (if it is found in the
c		list), or zero.
c--
c------------------------------------------------------------------------
	integer j,k,l
c
	k = 1
	l = nkeys
	binsrchi = 0
	do while(k.le.l)
	  j = (k+l)/2
	  if(key.lt.keys(j))then
	    l = j - 1
	  else if(key.gt.keys(j))then
	    k = j + 1
	  else
	    binsrchi = j
	    k = l + 1
	  endif
	enddo
	end
