c************************************************************************
c
c  A routine containing flux information about a few select calibrators.
c
c  History:
c    nebk xxxxxxx Original version.
c    rjs   4aug92 Adapted from nebk's code in gpcal. Plenty of tabs and
c		  fewer spaces.
c    rjs  29sep93 Added 0823 and 0407.
c    rjs  18jan94 Added new1934
c    rjs   1aug94 Added really new 1934. Change source name matching criteria.
c************************************************************************
c* CalStoke -- Flux characteristics of selected calibrators.
c& nebk, rjs
c: utilities
c+
      subroutine calstoke(source,stokes,freq,flux,nchan,ierr)
c
      implicit none
      character source*(*),stokes*1
      integer ierr,nchan
      double precision freq(nchan)
      real flux(nchan)
c
c     Return IQUV for known calibrator sources.  Spectra generally
c     described by third or fourth order polynomials in either the
c     log(f)-log(S), f-S or log(f)-S domains.
c
c   Input:
c     source	Source name.
c     stokes	One of IQUV.
c     freq	Frequency (GHz)
c     nchan	Number of channels.
c   Output:
c     flux	The source flux for those frequencies for that Stokes
c		parameter.
c     ierr	Status return: 0 = All OK.
c			       1 = OK, but extrapolation performed.
c			       2 = No match found.
c--
c-----------------------------------------------------------------------
      integer nsrc, nnames
      integer loglog,loglin,linlin,linlog
      parameter (nsrc = 8, nnames = 20)
      parameter (loglog=1,loglin=2,linlin=3,linlog=4)
c
      character name*32
      double precision x
      real coeffs(5,nsrc,4), stmp, frange(2,nsrc)
      character names(nnames)*8
      integer srcnum(nnames), isrc, ipol, i, j
      integer coeftype(nsrc,4)
c
      save coeffs, names, srcnum, coeftype
c
c  All lists are in the order:
c       3C286,    Perley/Killeen, 1991
c       3C48,     Baars et al
c       3C147,    Baars et al
c       3C138,    Perley/Killeen, 1991
c       1934-638, Reynolds, 1994
c       0823-500  Bob Duncan's fit to Parkes data.
c       0407-658  ??
c
      data ((coeffs(i,j,1),i=1,5),j=1,nsrc) /
     +	    1.099506E2,  -44.80922,   4.618715,    0.0,       0.0,
     +      2.345,         0.071,    -0.138,       0.0,       0.0,
     +      1.766,         0.447,    -0.184,       0.0,       0.0,
     +    178.2661,     -114.4718,   25.23650,    -1.905347,  0.0,
     +    -30.7667,       26.4908,   -7.0977,      0.605334,  0.0,
     +      8.1685,       -1.3370,    6.4657e-2,   0.0,       0.0,
     +      1.6863,        0.75933,  -0.29088,     0.0,       0.0,
     +    -23.839,        19.569,    -4.8168,      0.35836,   0.0/
c     +	   -1.1472989,	  -9.9004221, 9.5516825,  -2.7583933, 0.2533961/
c
c
c  Q coefficients.
c
      data ((coeffs(i,j,2),i=1,5),j=1,nsrc) /
     +		     2.735732,  -0.923091,    0.073638,    0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +               0.7228561,  0.258980,   -0.09490732,  0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0/
c
c  U coefficients.
c   
      data ((coeffs(i,j,3),i=1,5),j=1,nsrc) /
     +		     6.118902,  -2.05799,     0.163173,    0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +              -1.167723,   0.3653473,  -0.0252807,   0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0,
     +               0.0,        0.0,         0.0,         0.0,0.0/
c
c  V coefficients.
c
      data ((coeffs(i,j,4),i=1,5),j=1,nsrc) /
     +		    0.0, 0.0, 0.0, 0.0,0.0,
     +              0.0, 0.0, 0.0, 0.0,0.0,
     +              0.0, 0.0, 0.0, 0.0,0.0,
     +              0.0, 0.0, 0.0, 0.0,0.0,
     +              0.0, 0.0, 0.0, 0.0,0.0,
     +              0.0, 0.0, 0.0, 0.0,0.0,
     +              0.0, 0.0, 0.0, 0.0,0.0,
     +              0.0, 0.0, 0.0, 0.0,0.0/
c
c Recognized names
c
      data names /'3C286', '1328+307', '1331+305',
     +            '3C48',  '0134+329', '0137+331',
     +            '3C147', '0538+498', '0542+498',
     +            '3C138', '0518+165', '0521+166',
     +            '1934',  '1934-638', '1939-637',
     +            '0823',  '0823-500',
     +            '0407',  '0407-658',
     +		  'OLD1934'/
c
c Source number in coef table
c
      data srcnum /1,1,1, 2,2,2, 3,3,3, 4,4,4, 5,5,5, 6,6, 7,7, 8/
c
c  What sort of fit is the polynomial (logarithmic? linear?).
c
      data coeftype /
     +  loglin,loglog,loglog,loglin,loglog,linlin,loglog,loglog,
     +  loglin,linlin,linlin,loglin,linlin,linlin,linlin,linlin,
     +  loglin,linlin,linlin,loglin,linlin,linlin,linlin,linlin,
     +  linlin,linlin,linlin,linlin,linlin,linlin,linlin,linlin/
c
c Frequency range polynomial fits done over (0.0 means don't know)
c
      data frange /0.3275,  14.9850,
     +               0.0,      0.0,
     +               0.0,      0.0, 
     +             0.3275,  14.985,
     +             0.4080,   8.400,
     +             1.380,    8.640,
     +             0.408,    8.400,
     +             0.4080,   8.400/
c-----------------------------------------------------------------------
      ierr = 2
      name = source
      call ucase (name)
c
c  Try and match source name. Jump out if a match is found.
c
      do  i = 1, nnames
        if(name.eq.names(i))then
          isrc = srcnum(i)
          ierr = 0
          goto 100
        endif
      enddo
c
100   if(ierr.eq.0)then
	ipol = index('iquv',stokes)
	if(ipol.eq.0)call bug('f','Unrecognised polarisation type')
	do i=1,nchan
	  x = freq(i)
          if (frange(1,isrc).ne.0.0 .and. frange(2,isrc).ne.0.0 .and.
     +      (x.lt.frange(1,isrc) .or. x.gt.frange(2,isrc)))
     +	    ierr=1
	  if(coeftype(isrc,ipol).eq.loglog.or.
     +       coeftype(isrc,ipol).eq.loglin) x = log10(1000.*x)
c
	  stmp = coeffs(1,isrc,ipol)  + x*(coeffs(2,isrc,ipol) + 
     +         x*(coeffs(3,isrc,ipol) + x*(coeffs(4,isrc,ipol) +
     +	       x*(coeffs(5,isrc,ipol)				))))
	  if (coeftype(isrc,ipol).eq.linlog.or.
     +        coeftype(isrc,ipol).eq.loglog)then
 	    flux(i) = 10**stmp
          else
            flux(i) = stmp
          endif
        enddo
      endif
c
      end
