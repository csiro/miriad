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
c************************************************************************
c* LogOpen -- Initialise the log file routines.
c& bpw
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
c		 'p'	Pack. When many consecutive output lines are identical,
c			the pack option replaces them with one output line
c			followed by another line telling how many repetitions.
c			Currently ignored.
c--
c------------------------------------------------------------------------
	include 'log.h'
	integer iostat

	nlines = 0
        printer = .false.
	query  = index(flags,'q').ne.0
        if(name(1:8).eq.'/printer') then
          call output('LogOpen: Printer output selected')
          printer = .true. 
          query = .false.
          call filedel('printer',iostat)
	  call txtopen(lu,'printer','new',iostat)
	  if(iostat.ne.0) call bugno('f',iostat)
	else if(name.ne.' ')then
	  call txtopen(lu,name,'new',iostat)
	  if(iostat.ne.0) call bugno('f',iostat)
	else
	  lu = 0
	endif
	end
c************************************************************************
c* LogWrit -- Write a line to the log file.
c& bpw
c: text-i/o,log-file
c+
	subroutine LogWrit(line)
c
	character line*(*)
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
	if( .not.more ) then
	  call logclose
	  stop
	endif
	return
	end
c************************************************************************
c* LogWrite -- Write a line to the log file.
c& bpw
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
	integer length,iostat
c
	if(lu.ne.0)then
	  call txtwrite(lu,line,len(line),iostat)
	  if(iostat.ne.0) call bugno('f',iostat)
	  more = .true.
	else
	  if(query.and.nlines.eq.maxlines)then
	    call prompt(ans,length,
     *		'Hit RETURN to continue, q to quit: ')
	    nlines = 0
	    more = length.eq.0.or.(ans.ne.'q'.and.ans.ne.'Q')
	  else
	    more = .true.
	  endif
	  nlines = nlines + 1
	  if(more) call output(line)
	endif
	end
c************************************************************************
c* LogClose -- Finish up with the log file.
c& bpw
c: text-i/o,log-file
c+
	subroutine LogClose
c
	implicit none
c
c  This completes access to the log file or terminal, and closes is it up.
c  In case the logfile was '/printer', it sends that file to a printer
c  (yet to be determined how and which one, in a system independand way)
c  (environment variable??)
c
c--
c------------------------------------------------------------------------
	include 'log.h'
#ifdef vms
	integer  iostat
#else
#ifdef cft
	integer  iostat
#else
	integer  iostat, system
        external system
#endif
#endif

	if(lu.ne.0) call txtclose(lu)

	if (printer) then
#ifdef vms
            call output('** The file printer can be send to printer **')
#else
#ifdef cft
            call output('File sent to printer')
            iostat = ishell('lpr printer')
	    if (iostat.ne.0) then
                call bug('w','LogClose: printer command failed')
            endif 
#else
            call output('File sent to printer')
            iostat = system('lpr printer')
	    if (iostat.ne.0) then
                call bug('w','LogClose: printer command failed')
            endif 
#endif
#endif
	endif
	end
