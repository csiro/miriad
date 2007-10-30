c************************************************************************
c* Prime -- Returns a prime less than or equal to N.
c& jm
c: numbers,mathematics
c+
	integer function prime(n)
c
	implicit none
	integer n
c
c  This returns a prime less than or equal to N.
c
c  Input:
c    n		Number to find a prime close to.
c  Output:
c    prime	The closest prime less than or equal to n.
c--
c  For numbers less that a limit, it finds the prime by a binary search
c  of a table of primes. For numbers above the limit, it divides candidate
c  numbers by primes in the table. This continues until the remainder from
c  a division is 0 (cannot be a prime) or prime**2 exceeds the candidate
c  (i.e. the candidate must be a prime).
c
c  History:
c    rjs   5mar90 Adapted from the Werong routine in ASCAL.
c------------------------------------------------------------------------
	integer nprimes
	parameter(nprimes=200)
	integer i,j,k,result
	integer primes(nprimes)
	logical more
	data (primes(i),i=1,100)/
     *    2,  3,  5,  7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53,
     *	 59, 61, 67, 71, 73, 79, 83, 89, 97,101,103,107,109,113,127,131,
     *	137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,
     *	227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,
     *	313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,
     *	419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,
     *	509,521,523,541/
c
	data (primes(i),i=101,200)/
     *	547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,
     *	643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,
     *	751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,
     *	859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,
     *	 977, 983, 991, 997,1009,1013,1019,1021,1031,1033,1039,1049,
     *	1051,1061,1063,1069,1087,1091,1093,1097,1103,1109,1117,1123,
     *	1129,1151,1153,1163,1171,1181,1187,1193,1201,1213,1217,1223/
c
c  The quick case. Do a binary search.
c
	if(n.le.primes(nprimes))then
	  i = 1
	  j = nprimes
	  do while(i.lt.j)
	    k = (i+j)/2
	    if(n.le.primes(k))then
	      j = k
	    else if(n.ge.primes(k))then
	      i = k + 1
	    endif
	  enddo
	  if(n.ne.primes(i).and.i.gt.1)i = i - 1
	  result = primes(i)
c
c  The harder case. Check odd numbers less than n for prime factors
c
	else if(n.gt.primes(nprimes)*primes(nprimes))then
	  call bug('f','Algorithm to find a prime failed')
	else
	  result = 2*((n-1)/2) + 1
	  more = .true.
	  do while(result.gt.primes(nprimes).and.more)
	    i = 2
	    dowhile(primes(i)*(result/primes(i)).ne.result .and.
     *		     result.gt.primes(i)*primes(i)    )
	      i = i + 1
	    enddo
	    if(result.lt.primes(i)*primes(i))then
	      more = .false.
	    else
	      result = result - 2
	    endif
	  enddo
	endif
c
c  Return the result.
c
	prime = result
	end
