c* rtfmt - construct a format during run time
c: strings
c& bpw
c+
      character*(*)  function rtfmt( string, vals, nvals )
      character*(*)  string
      integer        vals( * )
      integer        nvals

c Rtfmt takes as input a format string. It replaces characters between
c < and > with a number, which is given by the array vals. The first
c occurence of <...> corresponds to vals(1), etc. This allows to make up
c the format in a write statement during runtime, thereby making it
c possible to print only as many characters as needed and avoiding unused
c empty spaces. To get a '<' character in the output format string,
c stutter it, i.e. use '<<'.
c
c Example 1:
c write(*,rtfmt( '''Date is:'',i<>,''-'',i<>,''-'',i<>', rtf, 3 )) date
c where date is a 3-element array. The 3-element array rtf contains the
c number of digits of each of the three elements (see function nfig for
c how to obtain these numbers). If rtf has e.g. the values 1, 1 and 4,
c the format string that is produced will be:
c '''Date is:'',i1,''-'',i1,''-'',i4'. The output on the c terminal will
c look something like: '1-1-1900' or '1-10-90' or '12-12-1991', i.e.
c there are never any obsolete spaces.
c
c Example 2:
c if(    abs( x-int(x) ) .lt. 1.e-10   ) ndec(2) = 0
c if(    abs( x-int(x) ) .ge. 1.e-10   ) ndec(2) = 2
c ndec(1) = nfigi( int(x) ) + 3
c write( *, rtfmt( '''x='',f<>.<>', ndec, 2 ) ) x
c Now the f-format with which a variable is written depends on the value
c of the variable, so that the output will be 'x=1.02' or 'x=1' or
c 'x=100' or 'x=1000', i.e. no distracting spaces come out.
c
c Example 3:
c nspac = 60 - len1(message)
c write(*,rtfmt('a, <nspac>x, ''>'',$',nspac,1)) message(:len1(message))
c This puts out a message on the screen, with the prompt character >
c always in column 60, no matter how long the string 'message' was.
c
c Input:
c   string:     A format.
c   vals:       Values to replace characters between < and > with
c   nvals:      Number of values
c--
c History:
c   Long ago:   bpw   Created
c   08-nov-91   bpw   add error messages

      integer        i, j, ptr
      integer        nelc, slen
      integer        n, nfigi
      logical        error
      character*1024 outstring
      character*80   errstring
      outstring = ' '
      error = .false.

      i   = 1
      j   = 1
      ptr = 1
      slen = nelc(string)
      do while( i .le. slen )
         if( string(i:i).ne.'<' ) then
            outstring(j:j) = string(i:i)
            j = j + 1
         elseif(  string(i:i).eq.'<' .and. string(i+1:i+1).eq.'<' ) then
            outstring(j:j) = string(i:i)
            j = j + 1
            i = i + 1
         else
            do while( string(i:i) .ne. '>' )
              i = i + 1
            enddo
            n = nfigi(vals(ptr))
            if( vals(ptr).le.0 ) then
               error = .true.
               call bug( 'e',
     *         'Rtfmt told to generate format with negatives or zeroes')
            endif
            if( n.eq.1 ) write( outstring(j:j  ),'( i1 )' ) vals(ptr)
            if( n.eq.2 ) write( outstring(j:j+1),'( i2 )' ) vals(ptr)
            j   = j + n
            ptr = ptr + 1
         endif
         i = i + 1
      enddo
      rtfmt = '(' // outstring(:nelc(outstring)) // ')'
      if( error ) then
         errstring = 'Generated format: ' // outstring(:nelc(outstring))
         call bug( 'e', errstring )
      endif
      return
      end
