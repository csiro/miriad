c***********************************************************************
	program varlist
	implicit none
C
C    List all variable names, types, and lengths in a u,v data set 
C
C    user inputs:  dataset   - name of u,v data set 
C                  outfile  - output file for listing; default = logfile
C
c= varlist - List all variables in dataset
c& lgm
c: utility, plotting
c+
c	VARLIST lists all variable names or names, types and length
c	in a data set.
c< vis
c@ log
c	Name of output file
c@ option
c	Option for printout (names or all).  Default is "all."
c--
C  History:
c    lgm  Dark-ages  Original version.
c    pjt  15oct89    Repaired.
c    rjs  24oct89    Fixed portability problem in a format statement.
c    rjs   7nov89    Some standardising and cosmetic changes.
c    lgm  12nov89    Fix so printed variable lengths are non-zero
c    pjt  30jun93    Wow, 2.5 years of bugfree riding, but now added MAXCHAN
c    rjs  16sep93    Call logclose.
c    rjs  27apr95    Distinguish between zero-length and unset 
c  ToDo
c    * fix questionable practice to find all uv vars (at most 300 now)
c      (there is a subroutine for this...)
c-----------------------------------------------------------------------
	integer MAXVAR
	parameter(MAXVAR=300)
	logical eof, more
	character var(MAXVAR)*11,dataset*40,outfile*40,option*8
	character line*80
	integer iostat,tno,item,ivar,iv,jv,nvar,l
	logical update
	character*1 type(MAXVAR)
	character*4 length(MAXVAR)
        character*80 umsg
C
C  call key routines to get user inputs
C
        call output( 'Varlist: version 1.0 16-Sep-93' )
	call keyini
	call keya('vis',dataset,' ')
	call keya('log',outfile,' ')
	call keya('option',option,'all')
	call keyfin
C
C  Open the output log file.
C
	call LogOpen(outfile,' ')
C
C  do the initial opens to read the variable table
C
	call uvopen(tno,dataset,'old')
	call uvnext(tno)
	call haccess(tno,item,'vartable','read',iostat)
C
C  loop through and read the variable names (questionable practice)
C
	eof = .false.
	do ivar=1,MAXVAR
	   call hreada(item,var(ivar),eof)
	   if(eof) go to 105
	   call uvprobvr(tno,var(ivar)(3:10),type(ivar),l,
     *		update)
	   if(l.eq.0.and..not.update)then
	     length(ivar) = '  ??'
	   else
	     write(length(ivar),'(i4)')l
	   endif
	enddo
  105	nvar = ivar-1
C
C  if names only option is choosen,
C  write variable names to output file in 5 column mode
C
        umsg = 'Listing of all variable names in '//dataset
	call LogWrite( umsg ,more)
	call LogWrite(' ',more)
	if(option(1:3) .eq. 'nam') then
		do iv=1,nvar,5
		   jv = min(nvar,iv+4)
		   write(line,'(10x,5(a8,3x))') 
     *				(var(ivar)(3:10),ivar=iv,jv)
		   call LogWrite(line,more)
		enddo
		call LogWrite(' ',more)
		call LogWrite(' ',more)
	else
C
C   otherwise write all info about variables in 3 column mode
C
                call LogWrite('   Format: variable name:type:length',
     *             more )
		call LogWrite(' ',more)
		do iv=1,nvar,3
		   jv = min(nvar,iv+2)
		   write(line,'(5x,3(a8,'':'',a,'':'',a,5x))')
     *		    (var(ivar)(3:10),type(ivar),length(ivar),ivar=iv,jv)
		   call LogWrite(line,more)
		enddo
	endif
C
C  close file access
C
	call logclose
	call hdaccess(item,iostat)
	call uvclose(tno)
	end
