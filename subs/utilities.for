c
c History:
c   bpw:   11sep90   created
c   bpw:   30sep91   some refinements in docs
c
c-----------------------------------------------------------------------

c* nelc - return the length of the string
c& bpw
c: utilities,strings
c+
      integer function nelc(string)
      character string*(*)

c Nelc finds the number of characters in a string before a zero byte,
c discarding trailing spaces.
c
c Input:
c   string:   string to determine the length of
c--
      integer   n
      n = len(string)
      do while( n.gt.0.and.
     *      (string(n:n).eq.' '.or.string(n:n).eq.char(0)) )
        n = n - 1
      enddo
      nelc = n
      return
      end


c* nel - return the number used values in an array
c& bpw
c: utilities
c+
c      integer          function neli ( i, ni )
c      integer          function nelr ( r, nr )
c      integer          function neld ( d, nd )
c      integer          i(ni)
c      real             r(nr)
c      double precision d(nd)
c      integer          ni, nr, nd

c Nel returns the index of the last used element of the input array,
c i.e. it gives the used length of the array, which is defined as the
c index of the last non-zero element. The result is 0 if all elements are
c 0 and nx if all elements are non-zero. Nelr works on real arrays, neli
c on integer arrays and neld on double precision arrays.
c
c Input:
c   i/r/d:      input array
c   ni/nr/nd:   number of elements of input array
c--
      integer function neli ( i, ni )
      integer ni, n
      integer i(ni)
      n = ni
      do while( n.gt.0  .and.  i(n).eq.0 )
        n = n - 1
      enddo
      neli = n
      return
      end

      integer function nelr ( r, nr )
      integer nr, n
      real    r(nr)
      n = nr
      do while( n.gt.0  .and.  r(n).eq.0. )
        n = n - 1
      enddo
      nelr = n
      return
      end

      integer function neld ( d, nd )
      integer          nd, n
      double precision d(nd)
      n = nd
      do while( n.gt.0  .and.  d(n).eq.0.d0 )
        n = n - 1
      enddo
      neld = n
      return
      end


c* nfig - find number of digits in a number
c& bpw
c: utilities
c+
c      integer          function nfigi ( ivalue )
c      integer          function nfigr ( rvalue )
c      integer          function nfigd ( dvalue )
c      integer          ivalue
c      real             rvalue
c      double precision dvalue

c nfigi returns the number of digits of an integer number, e.g.
c nfigi(4)=1, nfigi(55)=2, nfigi(-10)=3 etc.
c nfigr returns the number of digits in the integer part of a real
c number. I.e. nfigr(1.)=1, nfigr(1.01)=1, nfigr(555.)=3, nfigr(-10.)=3
c etc.
c nfigd returns the number of digits in the integer part of a double
c precision number. I.e. nfigd(1.d0)=1, nfigd(1.01d0)=1, nfigd(-10.d0)=3
c etc.
c
c Input:
c   value:     value to work on
c--
      integer function nfigi( ivalue )
      integer ivalue
      nfigi = int(  alog10( amax1( 1., abs(float(ivalue))   ))) + 1
      if( ivalue.lt.0. ) nfigi = nfigi + 1
      return
      end

      integer function nfigr( rvalue )
      real rvalue
      nfigr = int(  alog10( amax1( 1., abs(rvalue)   ))) + 1
      if( rvalue.lt.0. ) nfigr = nfigr + 1
      return
      end

      integer function nfigd( dvalue )
      double precision dvalue
      nfigd = int(  dlog10( dmax1( 1.d0, dabs(dvalue)   ))) + 1
      if( dvalue.lt.0.d0 ) nfigd = nfigd + 1
      return
      end


***********************************************************************


c* teken - returns the sign of a real value
c& bpw
c: utilities
c+
c     integer          function tekeni ( i )
c     real             function tekenr ( r )
c     double precision function tekend ( d )
c     integer          i
c     real             r
c     double precision d
c
c tekenr returns +1.   or -1.   depending on the sign of the real x.
c tekeni returns +1    or -1    depending on the sign of the integer i
c tekend returns +1.d0 or -1.d0 depending on the sign of the double
c                               precision variable d
c--
      integer function tekeni ( i )
      integer i
      tekeni = isign ( 1,    i )
      return
      end
      real function tekenr ( r )
      real r
      tekenr =  sign ( 1.0,  r )
      return
      end
      double precision function tekend ( d )
      double precision d
      tekend = dsign ( 1.d0, d )
      return
      end


c* entier - the entier function
c: utilities
c& bpw
c+
c      integer function entier(x)
c      real             x
c
c This returns the entier of x, i.e. the next integer smaller than x.
c
c Input:
c   x: real value to take the entier of
c
c--
      integer function entier(x)
      real x
      if( x.ge.0. ) entier =  int( x+1.e-30)
      if( x.lt.0. ) entier = -int(-x-1.e-30)-1
      return
      end


c* delim - make sure value is between limits
c& bpw
c: utilities
c+
c      integer          function delimi ( ivalue, iminvalue, imaxvalue )
c      real             function delimr ( rvalue, rminvalue, rmaxvalue )
c      double precision function delimd ( dvalue, dminvalue, dmaxvalue )
c      integer          iminvalue, imaxvalue, ivalue
c      real             rminvalue, rmaxvalue, rvalue
c      double precision dminvalue, dmaxvalue, dvalue
c
c This returns minvalue if value<minvalue, maxvalue if value>maxvalue and
c value otherwise.
c
c Input:
c   value:     value to check
c   minvalue:  minimum delimiting value
c   maxvalue:  maximum delimiting value
c--
      integer function delimi  ( ivalue, iminvalue, imaxvalue )
      integer          iminvalue, imaxvalue, ivalue
      delimi =  max ( iminvalue,  min ( imaxvalue, ivalue ) )
      return
      end

      real function delimr ( rvalue, rminvalue, rmaxvalue )
      real             rminvalue, rmaxvalue, rvalue
      delimr = amax1( rminvalue, amin1( rmaxvalue, rvalue ) )
      return
      end

      double precision function delimd ( dvalue, dminvalue, dmaxvalue )
      double precision dminvalue, dmaxvalue, dvalue
      delimd = dmax1( dminvalue, dmin1( dmaxvalue, dvalue ) )
      return
      end


c* boxnr - find in which box the input value lies.
c& bpw
c: utilities
c+
c      integer          function boxnri ( i, iedges, nedges )
c      real             function boxnrr ( r, redges, nedges )
c      double precision function boxnrd ( d, dedges, nedges )
c      integer          i, iedges(nedges)
c      real             r, redges(nedges)
c      double precision d, dedges(nedges)
c      integer          nedges

c The array i/r/dedges defines a set of boxes. Boxnr then returns the
c index of the box in which i/r/d lies. For example, if iedges has the
c values 0, 2, 5, 10, 50 then boxnr becomes 1 if i=1, 2 if i=2, 2 if i=4,
c 4 if i=45 etc. Values on the edges are supposed to lie in the box above
c the value. Values below the first edge are supposed to lie in box 0,
c values above the last edge are in box nedges.
c--
      integer function boxnri ( i, iedges, nedges )
      integer nedges
      integer i, iedges(nedges)
      integer delimi
      integer k
      k = 1
      do while( .not.( i.lt.iedges(min(k,nedges)) .or. k.eq.nedges+1 ) )
        k = k + 1
      enddo
      boxnri = delimi ( k-1, 0, nedges )
      return
      end

      real    function boxnrr ( r, redges, nedges )
      integer nedges
      real    r, redges(nedges)
      integer delimi
      integer k
      k = 1
      do while( .not.( r.lt.redges(min(k,nedges)) .or. k.eq.nedges+1 ) )
        k = k + 1
      enddo
      boxnrr = delimi ( k-1, 0, nedges )
      return
      end

      double precision function boxnrd ( d, dedges, nedges )
      integer          nedges
      double precision d, dedges(nedges)
      integer          delimi
      integer          k
      k = 1
      do while( .not.( d.lt.dedges(min(k,nedges)) .or. k.eq.nedges+1 ) )
        k = k + 1
      enddo
      boxnrd = delimi ( k-1, 0, nedges )
      return
      end


***********************************************************************


c* arctan - arctangent function
c& bpw
c: utilities
c+
      real   function arctan( y, x )
      real   y, x

c This is like the atan2 intrinsic function, except that it gives the
c proper answer if x=0 (pi/2 for y>0 and 3pi/2 for y<0)
c
c Input:
c   x,y:     x and y coordinates
c--
      real   amod
      real   twopi
      if(     x.eq.0. .and. y.ge.0. ) then
        arctan = acos(-1.) / 2.
      elseif( x.eq.0. .and. y.lt.0. ) then
        arctan = acos(-1.) * 3. / 2.
      else
        twopi = 2. * acos(-1.)
        arctan = amod(  atan2(y,x) + twopi, twopi )
      endif
      return
      end
