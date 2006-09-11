c************************************************************************
c  History:
c    rjs  Dark-ages Original version.
c    rjs  19feb90   Fixed some comments.
c    rjs   4feb91   Ability to set random number generator seed.
c    rjs   7jan93   Include code for HP-UX machines, and reorganise defines.
c    rjs  xxjan93  Original version.
c    mjs  24feb93  Added some doc lines; no code mods.
c    rjs   7jul93  Include "ran" function here.
c    mjs  09mar94  Mods for sgi-only (mips).
c    rjs  25jan95  Mods for linux.
c    rjs  18aug95  Get it to work on SGI.
c    rjs  18sep95  Did I screw up in the above??I don't understand whats
c		   happened to the code.
c************************************************************************
c
c  Choose which random number style we are to use. We have three choices:
c  vms, cft or unix style. You also have the choice of including the
c  "ran" function.
c
#ifdef sgi
#  define sgi_style
#  define defined
#endif
#ifdef vms
#  define vms_style
#  define defined
#endif
#ifdef hpux
#  define vms_style
#  define inc_ran
#  define defined
#endif
#ifdef f2c
#  define vms_style
#  define inc_ran
#  define defined
#endif
#ifdef mips
#  define vms_style
#  define inc_ran
#  define defined
#endif
#ifdef cft
#  define cft_style
#  define defined
#endif
#ifndef defined
#  define unix_style
#endif
c************************************************************************
c*RandSet -- Set random number generator seed.
c&rjs
c:random-variables,noise
c+
	subroutine RandSet(seed)
c
	implicit none
	integer seed
c
c  This sets the seed of the random number generator. Future sequences
c  of random numbers will be generated from this seed.
c
c  Input:
c    seed	Some "random" integer value, which is the seed to be
c		used.
c--
c------------------------------------------------------------------------`
#ifdef sgi_style
	call srand(seed)
#endif
#ifdef cft_style
	call ranset(seed)
#endif
#ifdef vms_style
	integer iseed
	common/noisecom/iseed
	data iseed/12345/
	iseed = seed
#endif
#ifdef unix_style
	real dummy
	real rand
	dummy = rand(seed)
#endif
	end
c************************************************************************
c*Uniform -- Return uniformly distributed random variables.
c&rjs
c:random-variables,noise
c+
	subroutine uniform(data,n)
c
	implicit none
	integer n
	real data(n)
c
c  Generate uniformly distributed noise, in the range [0,1]. This works
c  on any machine.
c
c  Inputs:
c    n		Number of random numbers to produce.
c
c  Output:
c    data	An array of uniformly distributed random numbers.
c--
c  Externals Called:
c    ran,rand or ranf
c		A random number generator which produces uniform variates
c		in [0,1].
c
c------------------------------------------------------------------------
	integer i
#ifdef sgi_style
	double precision rand
	do i=1,n
	  data(i) = rand()
	enddo
#endif
#ifdef cft_style
	real ranf
	do i=1,n
	  data(i) = ranf()
	enddo
#endif
#ifdef vms_style
	real ran
	integer iseed
	common/noisecom/iseed
	do i=1,n
	  data(i) = ran(iseed)
	enddo
#endif
#ifdef unix_style
	real rand
	do i=1,n
	  data(i) = rand(0)
	enddo
#endif
	end
c************************************************************************
c*Gaus -- Generate gaussianly distributed random variables.
c&rjs
c:random-variables,noise
c+
	subroutine gaus(data,n)
c
	implicit none
	integer n
	real data(n)
c
c  Generate gaussian noise. This works on any machine, but the Gaussian
c  noise it produces is system dependent and not extremely accurate.
c
c  Gaussian noise is produced by adding 12 variates uniformly distributed
c  in [0,1], then subtracting 6. This will have zero
c  mean, and a variance of 1, and by the Central Limit Theorem, will
c  approximate gaussian noise.
c
c  Inputs:
c    n		Number of gaussian numbers to produce.
c
c  Output:
c    data	An array of gaussian noise.
c--
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,j,l,ltot
	real buf(MAXDIM)
c
c  Initialise the output with a uniformly distributed variable.
c
	call uniform(data,n)
c
c  Add another 11 uniformly distributed numbers. Use a strip-mine
c  approach in case buf is too small.
c
	do j=2,12
	  do l=1,n,MAXDIM
	    ltot = min(n-l+1,MAXDIM)
	    call uniform(buf,ltot)
	    do i=1,ltot
	      data(i+l-1) = data(i+l-1) + buf(i)
	    enddo
	  enddo
	enddo
c
c  Subtract off the mean value. The result should be roughly gaussianly
c  distributed with 0 mean and variance of 1.
c
	do i=1,n
	  data(i) = data(i) - 6
	enddo
c
	end
c************************************************************************
#ifdef inc_ran
      real function ran(iseed)
c
      implicit none
      integer iseed

c  Generate a random number in the range 0 to 1.
c
c  See "Numerical Recipes", Press et al, pp196.
c
c  This has the same call sequence as the VAX/VMS function of the same
c  name. The algorith, used, however, is completely different.
c------------------------------------------------------------------------
      real r(97)
      integer ix1,ix2,ix3,j
c
      integer m1,m2,m3,ia1,ia2,ia3,ic1,ic2,ic3
      real rm1,rm2
      parameter (m1=259200,ia1=7141,ic1=54773,rm1=3.8580247e-6)
      parameter (m2=134456,ia2=8121,ic2=28411,rm2=7.4373773e-6)
      parameter (m3=243000,ia3=4561,ic3=51349)
c
      integer seed
      logical first
      save first
      data first/.true./
c
      if (iseed.ne.0.or.first) then
	ix1=mod(ic1+abs(iseed),m1)
	ix1=mod(ia1*ix1+ic1,m1)
	ix2=mod(ix1,m2)
	ix1=mod(ia1*ix1+ic1,m1)
	ix3=mod(ix1,m3)
	do j=1,97
	  ix1=mod(ia1*ix1+ic1,m1)
	  ix2=mod(ia2*ix2+ic2,m2)
	  r(j)=(real(ix1) + real(ix2)*rm2)*rm1
	enddo
	iseed = 0
	first = .false.
      endif
      ix1=mod(ia1*ix1+ic1,m1)
      ix2=mod(ia2*ix2+ic2,m2)
      ix3=mod(ia3*ix3+ic3,m3)
      j=1+(97*ix3)/m3
      if(j.gt.97.or.j.lt.1)call bug('f','Something screwy in RAN')
      ran=r(j)
      r(j)=(real(ix1) + real(ix2)*rm2)*rm1
c
      end
#endif
