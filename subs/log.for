c************************************************************************
c  LogOpen, LogWrite, LogClose:
c  A series of routines to simplify writting to either a log file or
c  the users terminal.
c
c  History:
c    rjs  16oct89  Original version.
c    pjt  11dec89  Experimental '/printer' version (see also log.h)
c    bpw  25may90  Add 'Q' flag and LogWrit
c    mjs  10mar91  "system" call becomes "ishell" call on Cray
c    rjs  10jan96  Changes to appease g77 on linux.
c    rjs  03feb97  General improvements.
c    pjt  26apr98  check if logopen was ever called
c    pjt   1may98  more standard FORTRAN, using LogNOpen (courtesy RJS)
c    rjs   7may98  Correction to change in logclose.
c************************************************************************
c* LogOpen -- Initialise the log file routines.
c& rjs
c: text-i/o,log-file
c+
	subroutine LogOpen(name,flags)
c
	implicit none
	character name*(*),flags*(*)
c
c  This initialises the log file routines, and opens an output log file.
c
c  Inputs:
c    name	Name of the file to open. If this is blank, then output
c		is directed to the users terminal. If the name is '/printer'
c               at closing (LogClose) the file is sent to printer
c    flags	A parameter giving additional options to the routine. This
c		consists of a character string, with each character signifying
c		some particular option. Possible values are:
c		 ' '    Just write every to output device on every call
c			to logwrite.
c		 'q'	Query. If the output is a terminal, after every
c			22 lines, the user is queried if he/she wants to
c			continue. See also documentation of LogWrite and
c			LogWrit.			
c--
c------------------------------------------------------------------------
	include 'log.h'
	integer iostat,lognopen

	nopen = lognopen(1)
	if (nopen.ne.1) call bug('f','LogOpen: had been opened before')
	nlines = 0
	nopen = 1
        printer = .false.
        query = .false.
	domore = .true.
        if(name.eq.'/printer') then
          printer = .true. 
          call filedel('printer',iostat)
	  call txtopen(lu,'printer','new',iostat)
	  if(iostat.ne.0) call bugno('f',iostat)
	else if(name.ne.' ')then
	  call txtopen(lu,name,'new',iostat)
	  if(iostat.ne.0) call bugno('f',iostat)
	else
	  lu = 0
	  query  = index(flags,'q').ne.0
	endif
	end
c************************************************************************
c* LogWrit -- Write a line to the log file.
c& rjs
c: text-i/o,log-file
c+
	subroutine LogWrit(line)
c
	character line*(*)
c
c  THIS ROUTINE IS DEPRECATED
c
c  This writes a line to the log file or the users terminal.
c  If LogOpen was called with option 'q', LogWrit stops the
c  task if the user specifies 'quit' as answer.
c  See also LogWrite.
c
c  Input:
c    line	Line to write.
c--
c------------------------------------------------------------------------
	include 'log.h'
	logical more
	call logwrite(line,more)
	end
c************************************************************************
c* LogWrite -- Write a line to the log file.
c& rjs
c: text-i/o,log-file
c+
	subroutine LogWrite(line,more)
c
	implicit none
	character line*(*)
	logical more
c
c  This writes a line to the log file or the users terminal.
c  If LogOpen was called with option 'q' then LogWrite will
c  return a .FALSE. value in more when the user specifies 'quit'.
c  The applications program then has to take care of stopping.
c
c  Input:
c    line	Line to write.
c  Output:
c    more	Set to false if the user has had enough.
c--
c------------------------------------------------------------------------
	include 'log.h'
	character ans*1
	integer length,iostat,lognopen
c
	nopen = lognopen(0)
	if(nopen.ne.1) call bug('f','LogWrite: LogOpen never called')
	if(domore)then
	  if(lu.ne.0)then
	    call txtwrite(lu,line,len(line),iostat)
	    if(iostat.ne.0) call bugno('f',iostat)
	  else
	    if(query.and.nlines.eq.maxlines)then
	      call prompt(ans,length,
     *		'Hit RETURN to continue, q to quit: ')
	      nlines = 0
	      domore = length.eq.0.or.(ans.ne.'q'.and.ans.ne.'Q')
	    endif
	    nlines = nlines + 1
	    if(domore) call output(line)
	  endif
	endif
	more = domore
	end
c************************************************************************
c* LogClose -- Finish up with the log file.
c& rjs
c: text-i/o,log-file
c+
	subroutine LogClose
c
	implicit none
c
c  This completes access to the log file or terminal, and closes is it up.
c  In case the logfile was '/printer', it sends that file to a printer
c--
c------------------------------------------------------------------------
	include 'log.h'
c
	character lpr
	integer lognopen
#ifdef vms
	parameter(lpr='print')
#else
	parameter(lpr='lpr')
#endif
	nopen = lognopen(-1)
	if(nopen.ne.0) call bug('f','LogClose: LogOpen never called')
	if(lu.ne.0) call txtclose(lu)
	if(printer)then
	  call output('Queuing output to printer')
	  call command(lpr//' printer')
	endif
	nopen = 0
c
	end
c************************************************************************
c* LogNOpen -- Helper function for logging usage in LogOpen
c& rjs
c: text-i/o,log-file
c+
        integer function lognopen(val)
c
	implicit none
        integer val
c
c   This functions aids the logging usage of LogOpen/LogWrite/LogClose,
c   since variables cannot appear in both a DATA and COMMON.

c--
c------------------------------------------------------------------------
c
        integer nopen
        save nopen
        data nopen/0/
c
        nopen = nopen + val
        lognopen = nopen
        end

