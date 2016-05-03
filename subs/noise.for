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
c    rjs  24oct99  Added "save" statement in "ran" function.
c    rjs  04jul00  Use double precision arithmetic in "ran" to avoid some
c		   machine rounding biases.
c    rjs  18oct00  Setting the seedon "vms-style" machines was not working.
c    mc   12mar07  external for ran fixes gfortran problem
c    pjt  20mar07  merged previous MIR4 fortran standards
c    mhw  03may16  improve gaussian noise generator
c
c $Id: noise.for,v 1.4 2016/05/03 01:26:25 wie017 Exp $
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
#ifdef linux
#  define vms_style
#  define inc_ran
#  define defined
#endif
#ifdef unicos
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
	call setseed(seed)
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
	logical first
#ifdef inc_ran
	external ran
#endif
	save first
	common/noisecom/iseed
	data first/.true./
	if(first)then
	  call setseed(0)
	  first = .false.
	endif

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
#ifdef vms_style
c************************************************************************
	subroutine setseed(seed)
c
	implicit none
	integer seed
c
c  Set the seed for the VMS-style random number generator.
c
c------------------------------------------------------------------------
	integer iseed
	logical first
	save first
	common/noisecom/iseed
	data first/.true./
c
	if(seed.ne.0)then
	  iseed = seed
	else if(first)then
	  iseed = 12345
	endif
	first = .false.
c
	end
#endif
c************************************************************************
c*Gaus -- Generate gaussian distributed random variables.
c&rjs
c:random-variables,noise
c+
	subroutine gaus(data,n)
c
	implicit none
	integer n
	real data(n)
c
c  Generate gaussian noise using the Box-Mueller or polar method
c
c  The Box-Muller method uses the technique of inverse transformation
c  to turn two uniformly distributed randoms into two unit normal
c  randoms
c
c  Inputs:
c    n		Number of gaussian numbers to produce.
c
c  Output:
c    data	An array of gaussian noise.
c--
c  Ref:  the Box-Mueller (polar) method 
c         see Knuth, vol. 2, p. 104.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer i,k,nmax
        real v1,v2,s,c
	real buf(MAXDIM)
c
c  Fill buffer with uniform values, refill when needed
c
	nmax=min(MAXDIM,n+1)
        call uniform(buf,nmax)
        k=1
        do i=1,n,2
          s=2
          do while(s.ge.1.or.s.eq.0)
            v1=2*buf(k)-1
            v2=2*buf(k+1)-1
            s=v1*v1+v2*v2
            k=k+2
            if (k+1.gt.nmax) then
              call uniform(buf,nmax)
              k=1
            endif
          enddo
          c=sqrt(-2*log(s)/s)
          data(i)=c*v1
          if (i.lt.n) data(i+1)=c*v2
        enddo

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
      parameter(m1=134456,ia1=8121,ic1=28411)
      parameter(m2=243000,ia2=4561,ic2=51349)
      parameter(m3=259200,ia3=7141,ic3=54773)
c
      logical first
      save first,r,ix1,ix2,ix3
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
	  r(j)=(dble(ix1) + dble(ix2)/dble(m2))/dble(m1)
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
      r(j)=(dble(ix1) + dble(ix2)/dble(m2))/dble(m1)
c
      end
#endif
