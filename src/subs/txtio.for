c ***********************************************************************
c  History:
c      rjs  longago  Original version.
c      pjt   7may90  output significant part of strings using len1()
c      nebk 29oct90  add txtopena
c      bpw  08nov90  added documentation again.
c      rjs  13feb91  Broke out txtopena; getlun,freelun -> lunget,lunfree
c      bpw  25jul91  Corrected errors in docs.
c      mjs  26jul91  Corrected bug in 25jul91 txtopen code.
c      rjs  30aug93  Significant rewrite to use hio routines.
c ***********************************************************************
c* Output - Output a line of text to the user.
c& rjs
c: terminal-i/o
c+
	subroutine output(line)

	implicit none
	character line*(*)

c  Output writes a line of text to the standard output (usually the users
c   terminal).
c
c  Input:
c    line       The line of text to write. Only the significant part
c               of the string 'line(1:len1(line))' is output
c--
	integer len1
	write(*,'(a)')line(1:len1(line))
	end
c ***********************************************************************
c* prompt - Read input from the user
c& rjs
c: terminal-i/o
c+
	subroutine prompt(string,length,prmpt)

	implicit none
	integer length
	character string*(*),prmpt*(*)

c  Read input from the standard input (usually the users terminal).
c
c  Input:
c    prmpt      Prompt string.
c  Output :
c    string     The string read from the terminal.
c    length     The length of the string read from the terminal.
c--
c
c  Externals.
c
	integer len1
c
c  Do the read/prompt.
c
	write(*,'(a,$)')prmpt
	read(*,'(a)')string
	length = len1(string)
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
c    status     Either 'old','new' or 'append'.
c  Output:
c    lu         A handle used to access the file.
c    iostat     Completion code. A zero value indicates success. Other
c               values are system dependent.
c--
c------------------------------------------------------------------------
	character stat*16
c
	if(status.eq.'old')then
	  stat = 'read'
	else if(status.eq.'new')then
	  stat = 'write'
	else
	  stat = status
	endif
	call haccess(0,lu,name,stat,iostat)
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
c------------------------------------------------------------------------
	integer iostat
	call hdaccess(lu,iostat)
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
c------------------------------------------------------------------------
	integer htell
c
	length = htell(lu)
	call hreada(lu,string,iostat)
	length = htell(lu) - length - 1
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
c------------------------------------------------------------------------
	call hwritea(lu,string(1:length),iostat)
	end
