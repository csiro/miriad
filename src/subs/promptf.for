c********1*********2*********3*********4*********5*********6*********7**
c*promptf - Prompt for real value from standard input.
c: terminal-i/o
c& mchw
c+
	subroutine promptf(value,format,prmpt,default)
	implicit none
	character*(*) format,prmpt
	real value,default
c
c  Prompt for real value from standard input (usually user terminal)
c  in format 'prmpt' (default) :
c  <cr> returns value=default
c
c  Input:
c    format	Format string for default and value. e.g. f10.3
c    prmpt	Prompt string.
c    default	Default value.
c  Output:
c    value	Value read from standard input.
c--
c  History
c    mchw 4-dec-90
c    mjs  19feb93 Elim concat of "unknown" strlen (for the SGI)
c
c-----------------------------------------------------------------------
	integer length
	character*80 question,answer,form,msg1
c
c  External
c
	integer len1
c
	msg1 = format
	write(form,'(a)') '(a,'//msg1(1:len1(msg1))//',a)'
	msg1 = prmpt
	write(question,form(1:len1(form))) msg1(1:len1(msg1)) //
     +        ' (',default,') :'
	call prompt(answer,length,question(1:len1(question)))
	if(length.gt.0)then
	  msg1 = format
	  write(form,'(a)') '('//msg1(1:len1(msg1))//')'
	  read(answer(1:length),form(1:len1(form))) value
	else
	  value = default
	endif
	end
