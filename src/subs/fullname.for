c* fullname - expand an environment variable in front of a filename
c& bpw
c: files
c+
      character*(*) function fullname( filename )
      character*(*) filename

c This takes a filename consisting of two parts separated by a ':' and
c expands the environment variable indicated by the first half. If no ':'
c occurs, it checks for the occurrence of a '$' and tries to expand the
c environment variable indicated by that (the characters between "$" and
c "/"). If still nothing is found the original input is returned.

c Input:
c   filename:    name of a file containing an environment variable
c--
c
c History:
c   bpw  11sep90  created
c   bpw  26mar91  proper handling of getenv on Cray
c   bpw  07may91  made mgetenv and adapted this one accordingly
c   rjs  23dec92  Change call sequence of mgetenv.
c-----------------------------------------------------------------------
#ifdef vms
c
c Do nothing if VMS
c
      integer       nelc 
      fullname = filename(:nelc(filename))
      return
      end
#else
      character*1024 fname
      character*1024 envnam
      character*1024 name
      character*1024 expnam
      character*80   message
      integer        icolon, idollar, islash, nf
      integer        nelc
      icolon  = index( filename, ':' )
      idollar = index( filename, '$' )
      nf      = nelc(filename)
      if(     icolon.ne.0 ) then
        fname  = filename( : nf )
      elseif( idollar.eq.nf ) then
        call bug('w','Fullname: untranslatable: "$<NULL>"')
        fullname = filename(:nf)
        return
      elseif( idollar.ne.0 ) then
        fname  = filename( idollar+1 : nf )
        nf     = nelc(fname)
        islash = index( fname, '/' )
        if( islash.ne.0 ) then
          fname(islash:islash) = ':'
          icolon = islash
        else
          fname(nf+1  :nf+1  ) = ':'
          icolon = nf+1
          nf     = nf+1
        endif
      else
        fullname = filename(:nf)
        return
      endif
      if( icolon.ne.1  ) envnam = fname( : icolon-1 )
      if( icolon.eq.1  ) envnam = ' '
      if( icolon.lt.nf ) name   = fname( icolon+1 : nf )
      if( icolon.eq.nf ) name   = ' '
      call mgetenv( expnam, envnam )
      if( nelc(expnam).eq.0 ) then
        write( message, '( ''Fullname: '',a,'' not translated'' )' )
     *         envnam(:max(1,nelc(envnam)))
        call bug( 'w', message )
        write( message,'(''Fullname: assuming file '',a,'' is local'')')
     *         name(:max(1,nelc(name)))
        call bug( 'w', message )
        fullname = name(:max(1,nelc(name)))
      else
        if( nelc(name).ne.0 ) expnam = expnam(:nelc(expnam)) // '/'
        if( idollar.gt.1    ) expnam = filename(:idollar-1) //
     *                                 expnam(:nelc(expnam))
        fullname = expnam(:nelc(expnam)) // name(:max(1,nelc(name)))
      endif
      return
      end
#endif

c* remext - remove the extension part of a filename
c& bpw
c: files
c+
      subroutine remext ( filename )
      character*(*)  filename

c This takes the filename and strips all characters after the last '.'.
c If no '.' occurs, the input remains unchanged.
c
c  Input/Output:
c    filename:    name of a file
c--
      integer        rindx, nelc
      character*80   dir, name
      if(     index( filename,']' ) .ne. 0 ) then
        dir      = filename(:index(filename,']')  )
        name     = filename( index(filename,']')+1 : nelc(filename) )
        filename = dir(:nelc(dir)) // name( :rindx(name,'.')-1 ) // ' '
      elseif( index(filename,'/') .ne. 0 ) then
        dir      = filename(:index(filename,'/')  )
        name     = filename( index(filename,'/')+1 : nelc(filename) )
        filename = dir(:nelc(dir)) // name( :rindx(name,'.')-1 ) // ' '
      else
        filename = filename( :rindx(filename,'.')-1 ) // ' '
      endif
      return
      end

c***********************************************************************

c* rindx - return right index of character in string
c: utilities
c& bpw
c+
      integer function rindx( string, char )
      character*(*) string
      character*(*) char
c
c This subroutine returns the index of the first occurence of a
c character from the right, instead of the left as the standard
c index function does.
c
c   Input:
c      string      input string
c      char        character to check
c--
c History
c   bpw  24feb93  Created for sgi
c
c-----------------------------------------------------------------
      integer i, len1
      do i = len1(string), 1, -1
         if( string(i:i).eq.char ) then
            rindx = i
            return
         endif
      enddo
      rindx = 0
      return
      end
