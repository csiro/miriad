	program prthis
c= prthis - List history item of a dataset
c& bpw
c: utility
c+
c	PRTHIS is a MIRIAD task to list the history item of a miriad
c	dataset.
c< in
c@ bcount
c	The first line to list. Default is 1.
c@ log
c	The name of the output listing file. The default is the terminal
c@ task
c	List history only of this list of tasks (max=5). Default is all tasks.
c--
c
c  History:
c    nebk  4may89
c    nebk 28may89  Added multiple TASK parameter and BCOUNT.
c    rjs   6nov89  Minor tidying and standardisation.
c    bjpw 28jan91  Include standard keyword in
c    pjt  25jul91  increased size filename name a bit
c-----------------------------------------------------------------------
      implicit none
c
      integer maxtask
      parameter (maxtask = 5)
c
      integer lin, iostat, tlen, alen, len1, i, ntask, bcount
      character in*80, out*80, task(maxtask)*16, aline*132, ctemp*16
      logical more, eof
c-----------------------------------------------------------------------
      call output( 'Prthis: version 1.0 25-jul-91' )
c
c Get the input parameters.
c
      call keyini
      call keya ('in', in, ' ')
      if (in.eq.' ') call bug ('f', 'Input name is missing')
c
      call keyi ('bcount', bcount, 1)
      if (bcount.le.0) bcount = 1
      call keya ('log', out, ' ')
c
      ntask = 0
      ctemp = 'a'
      do while (ctemp.ne.' ' .and. ntask.le.maxtask)
        call keya ('task', ctemp, ' ')
	if(ctemp.ne.' ')then
	  ntask = ntask + 1
	  call ucase(ctemp)
	  task(ntask) = ctemp
	endif
      end do
c          
      call keyfin
c
c  Open the input file and the history file
c
      call hopen (lin, in, 'old', iostat)
      if (iostat.ne.0) call bug ('f', 'Input file not found')
      call hisopen (lin, 'read')
c
c  Open the output file if required.
c
      call LogOpen(out,'q')
c
c Skip unwanted lines
c
      if (bcount.gt.1) then
        i = 1
        eof = .false.
        do while (i.le.bcount-1 .and. .not.eof)
          call hisread (lin, aline, eof)
          i = i + 1
        end do
      end if
c
c  List the history
c
      more = .true.
      do while (more)
         call hisread (lin, aline, eof)
         alen = len1(aline)
c
         if (eof) then
            more = .false.
         else if (ntask.eq.0) then
	    call LogWrite(aline(1:alen),more)
         else
            do i = 1, ntask
	      tlen = len1(task(i))
              if ( index(aline(1:8+tlen), task(i)(1:tlen)).gt.0 )
     *		    call LogWrite(aline(1:alen),more)
            enddo
         end if
      end do
c
c  Close all files
c
      call hisclose (lin)
      call hclose (lin)
      call LogClose
c
      end
