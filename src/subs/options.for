c************************************************************************
c  History:
c    rjs    nov89 Original version.
c    rjs  25jan90 Minor documentation improvement.
c    nebk 18may92 Add KEYMATCH
c    nebk 03jun92 Give user more help in KEYMATCH if they bomb and
c                 restrict number of possible KEYA calls to MAXOUT
c                 so successive KEYMATCH calls with the same KEYWORD 
c                 can be used
c    rjs  02jul92 Insensitive to case. Input must be in lower case.
c    nebk 21feb95 Small format changes to KEYMATCH
c    rjs  27sep95 Improve error messages.
c************************************************************************
c*Options -- Get command line options.
c:user-input
c+
	subroutine Options(key,opts,present,nopt)
c
	implicit none
	character key*(*)
	integer nopt
	character opts(nopt)*(*)
	logical present(nopt)
c
c  Get options from the command line, and return to the caller those
c  options that are present. For example, assume the task keyword is
c  "options", and that possible options are, say, display,movie,fiddle, etc
c  then this examines the command line:
c    task options=fiddle
c  and returns indicating that "fiddle" was present, but "display" and
c  "movie" were absent.
c  This will allow the user to abbreviate options to uniqueness, and will
c  generate an error if there is an ambiguous option.
c
c  Inputs:
c    key	The task keyword to use.
c    opts	An array of possible option values. These should be in lower
c		case.
c    nopt	The number of possible options.
c  Output:
c    present	This indicates whether the option was present.
c--
c------------------------------------------------------------------------
	character string*16
	integer l,i,iopt
c
c  Externals.
c
	integer len1
        character*80 umsg
c
c  Initialise the options to indicate that none are present.
c
	do i=1,nopt
	  present(i) = .false.
	enddo
c
c  Search the task parameters.
c
	call keya(key,string,' ')
	dowhile(string.ne.' ')
	  l = len1(string)
	  call lcase(string(1:l))
          umsg = 'Unrecognised option '//string
	  if(l.gt.len(opts(1)))
     *	    call bug('f',umsg)
	  iopt = 0
	  do i=1,nopt
	    if(string(1:l).eq.opts(i)(1:l))then
              umsg = 'Ambiguous option '//string
	      if(iopt.ne.0)
     *		call bug('f',umsg)
	      iopt = i
	    endif
	  enddo
          umsg = 'Unrecognised option '//string
	  if(iopt.eq.0)
     *	    call bug('f',umsg)
	  present(iopt) = .true.
	  call keya(key,string,' ')
	enddo
	end
c************************************************************************
c*KeyMatch -- Get, with minimum match, command line options.
c:user-input
c+
      subroutine keymatch (key, ntype, types, maxout, out, nout)
      implicit none
c
      integer ntype, nout, maxout
      character*(*) types(ntype), out(maxout), key
c
c     Get a list of inputs for one keyword from the user and expand them
c     for minimum match compared to a list of possible inputs.
c     This will generate an error if there is an ambiguous option.
c
c  Inputs:
c    key	The task keyword to use.
c    type	An array of possible input values. These should be in lower
c               case.
c    ntype	The number of possible input types
c    maxout     Maximum number of output values
c  Output:
c    out        The expanded keyword list.  Will be all blank
c               if no values given
c    nout       The number of output values in out.
c--
c------------------------------------------------------------------------
	character string*16
	integer l, i, iopt, j
	integer len1
        character*130 umsg
c-----------------------------------------------------------------------
c
c  Initialize
c
      do i = 1, maxout
        out(i) = ' '
      end do
      nout = 0
c
c Search input for known type
c
      call keya(key,string,' ')
      dowhile(string.ne.' ' .and. nout.lt.maxout)
        l = len1(string)
        call lcase(string(1:l))
c
        iopt = 0
        do i = 1, ntype
          if(string(1:l).eq.types(i)(1:l))then
            if(iopt.ne.0) then
              umsg = '"'//string(1:len1(string))//
     +               '" is ambiguous for '//
     +               'keyword "'//key//'".  Choose from'
              call output (umsg)
              do j = 1, ntype
                umsg = '   '//types(j)
                call output (umsg)
              end do
              call bug('f', ' ')
            end if
            iopt = i
          endif
        enddo
        umsg = 'Value "'//string(1:l)//'" is not valid for keyword "'//
     +         key//'". Choose from'
        if(iopt.eq.0) then
          call output (umsg)
          do j = 1, ntype
            umsg = '   '//types(j)
            call output (umsg)
          end do
	  umsg = 'Correct keyword: '//key
          call bug('f', umsg)
        end if
c
        nout = nout + 1
        out(nout) = types(iopt)
c
        if (nout.lt.maxout) call keya(key,string,' ')
      enddo
c
      end
