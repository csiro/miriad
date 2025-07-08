c*  match - check if a string occurs in a list of valid strings
c& bpw
c: strings
c+
      logical function match ( input, valid_strings, nr )
      character*(*) input
      character*(*) valid_strings
      integer       nr

c match does a minimal match of the input string 'input' on the comma-separated
c list of valid strings. It also returns the string number.
c
c On output the value of match indicates whether or not any match was found.
c This implies that the input variable input can not be an explicit string,
c but must be a declared character variable.
c The matching is minimal, that means that if e.g. valid_strings has the value
c 'terminal,printer,file' and input is 'f', a match is produced. The return
c value of nr will be 3.
c 
c Input:
c   input:         the string of which it has to be checked if it matches
c   valid_strings: a comma-separated list of valid input strings
c Output:
c   nr:            the string number of input; 0 if there was no match
c--
      logical       matchdcd
      match = matchdcd( input,valid_strings,nr,.false.,.false.,.false. )
      return
      end


c* matchnr - get the ordinal number of a matching string
c: string
c& bpw
c+
      integer function matchnr( input, valid_strings )
      character*(*) input
      character*(*) valid_strings
c match does a minimal match of the input string 'input' on the
c comma-separated list of valid strings and returns the string number.
c If there is no match the result is 0.
c
c Input
c   input:         the string tp be checked
c                  (maximum length 512 characters)
c   valid_strings: a comma-separated list of valid input strings
c--
      character*512 the_input
      integer       nr
      logical       amatch, matchdcd
      the_input = input
      amatch =
     *matchdcd( the_input, valid_strings, nr, .false.,.false.,.false. )
      matchnr = nr
      return
      end


c* matchdcd - check if a string occurs in a list of valid strings, with extras
c& bpw
c: strings
c+
      logical function matchdcd( input,valid_strings,nr,logsc,cum,abs )

      character*(*) input
      character*(*) valid_strings
      integer       nr
      logical       logsc
      logical       cum
      logical       abs

c matchdcd does a minimal match of the input string 'input' on the
c comma-separated list of valid strings. It also returns the string number.
c Depending on the values of the logical variables logsc, cum and abs, it allows
c the input string to be c preceded by 'log' or '>' or to be surrounded by '|',
c respectively, returning whether this was the case or not in the same logicals.
c If such special options are present, they will be stripped from the input
c string. I.e. if input was "|x|" and abs=true, on exit input will be "x".
c
c On exit the value of matchdcd indicates whether or not any match was found. If
c no match was produced nr is zero on exit.
c
c The matching is minimal, that means that if valid_strings has the value
c 'mass,flux,nh' and input is 'f', a match is produced. The return value of nr
c will be 2. On exit input will be set equal to the full string that was
c matched.
c If the input string is preceded by the string 'exact:' the minimal matching
c feature is turned off and only exact matches produce a return value true.
c
c If on input logsc is true, it will be true on output if input was 'logmass',
c but false if input was 'mass'.
c If on input cum is true, it will be true on output if input was '>flux', but
c false if input was 'flux'.
c If on input abs is true, it will be true on output if input was '|x|', but
c false if input was 'x'.
c These three variables can be combined, i.e. the input can be 'log>|x|'. If
c logsc, cum or abs are false on input, these possibilities are disabled. If all
c three are false, a call to function match may be more useful.
c
c Input:
c   input:         the string of which it has to be checked if it matches
c   valid_strings: a comma-separated list of valid input strings
c Output:
c   nr:            the string number of input; 0 if there was no match
c Input/Output:
c   logsc:         if true on input: input string may be preceded by log
c                  output indicates whether it was or not
c                  if false on input: input string can not be preceded by log
c   cum:           if true on input: input string may be preceded by >
c                  output indicates whether it was or not
c                  if false on input: input string can not be preceded by >
c   abs:           if true on input: input string may be surrounded by |
c                  output indicates whether it was or not
c                  if false on input: input string can not be surrounded by |
c--

      integer        nelc, indek
      logical        exact
      integer        nv,  ni
      integer        i1,  i2
      integer        mi1, mi2
      integer        matches
      integer        count

      if( nelc(input) .eq. 0 ) then
        matchdcd = .false.
        return
      endif

      exact = index(input,'exact:') .eq. 1
      if( exact ) input = input(7:nelc(input)) // ' '

      ni = len( input )
      if( logsc ) logsc      = index( input, 'log' ) .eq. 1
      if( logsc ) input(:ni) = input(4:ni) // '   '
      if( cum   ) cum        = input(1:1) .eq. '>'
      if( cum   ) input(:ni) = input(2:ni) // ' '
      if( abs   ) abs        = input(1:1).eq.'|'.and.
     *                         input(nelc(input):nelc(input)).eq.'|'
      if( abs   ) input(:ni) = input(2:max(2,nelc(input)-1)) // ' '
      ni = nelc ( input )

      if( ni.eq.0 ) then
        call bug('e','match: obligatory default not given in call')
        matchdcd = .false.
        return
      endif

      nv      = nelc ( valid_strings )
      matches = 0
      count   = 0

      i2      = -1
      do while( i2 .ne. nv )
        i1    = i2 + 2
        i2    = i2 + indek ( valid_strings(i1:nv), ',' )
        count = count + 1
        if( .not.exact ) then
          if( index ( valid_strings(i1:i2), input(:ni)  ) .eq. 1 ) then
            matches = matches + 1
            mi1     = i1
            mi2     = i2
            nr      = count
          endif
        endif
        if( valid_strings(i1:i2) .eq. input(:ni) ) then
          matches = 1
          mi1     = i1
          mi2     = i2
          nr      = count
        endif
      enddo

      if( matches .eq. 1 ) then
        input = valid_strings(mi1:mi2)
      else
        nr    = 0
        if( logsc ) logsc = .false.
        if( cum   ) cum   = .false.
        if( abs   ) abs   = .false.
      endif
      matchdcd = matches .eq. 1
      return
      end



c* equals - check if input equals one of a number of possible strings
c& bpw
c: strings
c+
      logical function equals ( input, valid_strings )
      character*(*) input
      character*(*) valid_strings
      character*80  to_match

c Equals checks if the input string occurs in the comma-separated list of
c strings given by valid_strings. This effectively replaces a statement like
c "if(x.eq.'a'.or.x.eq.'b'.or.x.eq.'c'.or.x.eq.'d')" with
c "if(equals(x,'a,b,c,d'))".
c
c Input:
c   input:         the string of which it has to be checked if it matches
c   valid_strings: a comma-separated list of valid input strings
c--

      logical       matchdcd
      integer       nr
      to_match = 'exact:' // input
      equals=matchdcd(to_match,valid_strings,nr,.false.,.false.,.false.)
      return
      end
      


c* substr - returns the n-th substring from the input string
c& bpw
c: strings
c+
      character*(*) function substr ( string, n )
      character*(*) string
      integer       n

c This takes as input a string which consists of a comma-separated list of
c substrings and an integer. It returns with the substring corresponding to
c the n-th position. E.g. substring('a,b,c,d,e',3) equals 'c'.
c
c If n is an illegal number, i.e. 0 or larger than the number of substrings
c in string, an empty string is returned.
c
c Input:
c   string:     comma-separated list of substrings
c   n:          position of requested substring in the list
c--
      integer       i1, i2, count, ns
      integer       nelc, indek
      ns = nelc(string)
      if( n.eq.0 .or. ns.eq.0 ) substr = ' '
      if( n.eq.0 .or. ns.eq.0 ) return
      i2    = -1
      count =  0
      do while( count.ne.n   .and.   i2.lt.ns )
        i1    = i2 + 2
        i2    = i2 + indek ( string(i1:ns), ',' )
        count = count + 1
      enddo
      substr = string(i1:i2)
      if( count.lt.n ) substr = ' '
      return
      end


***********************************************************************


c* isupperf - return true if char is an uppercase letter (A-Z)
c& bpw
c: strings
c+
      logical function isupperf ( char )
      character*1 char

c Isupperf checks if the character char is an uppercase letter (A-Z)
c and returns .true. if so, .false. if otherwise
c
c Input:
c   char:    character to check
c--
      isupperf = index( 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', char ) .ne. 0
      return
      end

c* islowerf - return true if char is a lowercase letter (a-z)
c& bpw
c: strings
c+
      logical function islowerf ( char )
      character*1 char

c Islowerf checks if the character char is a lowercase letter (a-z)
c and returns .true. if so, .false. if otherwise
c
c Input:
c   char:    character to check
c--
      islowerf = index( 'abcdefghijklmnopqrstuvwxyz', char ) .ne. 0
      return
      end

c* isdigitf - return true if char is 0, 1, 2, 3, 4, 5, 6, 7, 8 or 9
c& bpw
c: strings
c+
      logical function isdigitf ( char )
      character*1 char

c isdigitf checks if the character char is a digit (0-9) and returns
c .true. if so, .false. if otherwise
c
c Input:
c   char:    character to check
c--
      isdigitf = index( '0123456789', char ) .ne. 0
      return
      end

c* isalphaf - return true if char is a letter (a-z, A-Z)
c& bpw
c: strings
c+
      logical function isalphaf ( char )
      character*1 char

c Isalphaf checks if the character char is a letter (a-z or A-Z)
c and returns .true. if so, .false. if otherwise
c
c Input:
c   char:    character to check
c--
      logical     isupperf, islowerf
      isalphaf = isupperf(char) .or. islowerf(char)
      return
      end

c* isalnumf - return true if char is an alphanumeric character
c& bpw
c: strings
c+
      logical function isalnumf ( char )
      character*1 char
c
c Isalnumf checks if the character char is an alphanumeric character
c (letter or digit) and returns .true. if so, .false. if otherwise
c
c Input:
c   char:    character to check
c--
      logical     isalphaf, isdigitf
      isalnumf = isalphaf(char) .or. isdigitf(char)
      return
      end

