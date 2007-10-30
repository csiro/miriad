c ***********************************************************************
c  A collection of routines to get text data in and out of programs.
c
c  History:
c    rjs Dark-ages Original version.
c    rjs  13feb91  Changed GetLun and FreeLun to LunGet and LunFree to
c		   avoid a name conflict.
c    mjs  28feb91  Added in-code docs
c    rjs  30aug93  Use lib$get_lun and lib$free_lun.
c ***********************************************************************
c* Output - Output a line of text to the user.
c& rjs
c: terminal-i/o
c+
	subroutine output(string)

	implicit none
	character string*(*)

c  Output writes a line of text to the standard output (usually the users
c   terminal).
c
c  Input:
c    line       The line of text to write. Only the significant part
c               of the string 'line(1:len1(line))' is output
c--
	call lib$put_output(string)
	end
c ***********************************************************************
c* prompt - Read input from the user
c& rjs
c: terminal-i/o
c+
	subroutine prompt(string,length,prmpt)

	implicit none
	character string*(*),prmpt*(*)
	integer length

c  Read input from the standard input (usually the users terminal).
c
c  Input:
c    prmpt      Prompt string.
c  Output :
c    string     The string read from the terminal.
c    length     The length of the string read from the terminal.
c--
	integer*2 l2
	logical ok
c
c  Externals.
c
	logical lib$get_input
c
	ok = lib$get_input(string,prmpt,l2)
	if(.not.ok)call exit(ok)
	length = l2
	end
c ***********************************************************************
c* txtopen - Open a text file
c& rjs
c: text-files,text-i/o
c+
	subroutine txtopen(lu,name,status,iostat)

	implicit none
	integer lu,iostat
	character name*(*),status*(*)

c  This opens a text file for either reading or writing.
c
c  Inputs:
c    name       Name of the file to be opened.
c    status     Either 'old', 'new' or 'append'.
c  Output:
c    lu         A handle used to access the file.
c    iostat     Completion code. A zero value indicates success. Other
c               values are system dependent.
c--
c
c  Get a logical unit number. Return with an error if none were available.
c
	iostat = -1
	call lib$get_lun(lu)
	if(lu.lt.0)return
c
c  Open the file.
c
	if(status.eq.'old')then
	  open(unit=lu,file=name,status=status,readonly,
     *	  iostat=iostat,defaultfile='[].;')
	else
	  open(unit=lu,file=name,status=status,carriagecontrol='list',
     *	  iostat=iostat,defaultfile='[].;')
	endif
c
c  Handle errors.
c
	if(iostat.ne.0)then
	  call lib$free_lun(lu)
	  call txtstat(iostat)
	endif
c
	end
c ***********************************************************************
c* txtclose - Close a text file
c& rjs
c: text-files,text-i/o
c+
	subroutine txtclose(lu)

	implicit none
	integer lu

c  TxtClose closes a text file.
c
c  Input:
c    lu         The handle of the text file
c--
	close(unit=lu)
	call lib$free_lun(lu)
	end
c ***********************************************************************
c* txtread - Read a line from a text file
c& rjs
c: text-files,text-i/o
c+
	subroutine txtread(lu,string,length,iostat)

	implicit none
	integer lu,length,iostat
	character string*(*)

c  TxtRead reads a line from a text file.
c
c  Input:
c    lu         The handle of the text file.
c  Output:
c    string     The string read.
c    length     The number of characters read.
c    iostat     I/O completion code. Zero indicates success, -1 indicates
c               end-of-file, other values indicate an error.
c--
	integer l
	l = len(string)
	read(lu,'(q,a)',iostat=iostat)length,
     *				      string(1:max(1,min(l,length)))
	if(iostat.gt.0)call txtstat(iostat)
	end
c ***********************************************************************
c* txtwrite - Write a line to a text file
c& rjs
c: text-files,text-i/o
c+
	subroutine txtwrite(lu,string,length,iostat)

	implicit none
	integer lu,iostat,length
	character string*(*)

c  TxtWrite writes a line to a text file. Any trailing spaces in input
c  string are not written.
c
c  Input:
c    lu         The handle of the text file.
c    string     The string to be written.
c    length     The number of characters to be written.
c  Output:
c    iostat     I/O completion code. Zero indicates success.
c--
	integer l
	l = min(length,len(string))
	if(l.eq.0)then
	  write(lu,'()',iostat=iostat)
	else
	  write(lu,'(a)',iostat=iostat)string(1:l)
	endif
	if(iostat.gt.0)call txtstat(iostat)
	end
c ***********************************************************************
	subroutine txtstat(iostat)
c
	implicit none
	integer iostat
c
C  Routine to return the last i/o error that occurred. The return is a standard
C  VAX status return. First the rms code is gained. If this represents an error,
C  then it is used. If it does not, then the VAX condition value is used.
C
C  This is done as the rms error messages are usually more specific. However
C  not all i/o errors are rms errors, such as opening a file with an
C  inconsistent record length (this causes a FORTRAN error, but not an RMS one).
c
c  Output:
c    iostat	The returned system error code.
C
c--
      integer temp
c
      call errsns(,iostat,,,temp)
      if( iostat.eq.0 .or. 2*(iostat/2).ne.iostat )iostat = temp
      end
