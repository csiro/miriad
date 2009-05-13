c************************************************************************
	program itemize
c
c= itemize - List information about MIRIAD dataset(s)
c& pjt
c: utility
c+
c	ITEMIZE is a MIRIAD task which dumps a dataset or an item within a
c	dataset. If the input name is an item, then the contents of the
c	item (element for element) are written to the screen. If the input
c	name represents a dataset, then a summary of the items within the
c	dataset are given.
c@ in
c	The name of either a dataset, or an item within a data set or a 
c	wildcard. For example:
c	  % itemize in=dataset
c	or
c	  % itemize in=dataset/item
c	or
c	  % itemize in='*'
c	For example, to show the history information of file ``cm'', use
c	  % itemize in=cm/history
c	When a dataset name is given, itemize summarizes the contents
c	of the entire dataset. When an item name is also given, then
c	itemize dumps the entire contents of the item (in accordance
c	to the index and format keywords).
c@ log
c	The name of the output listing file. The default is the users
c	terminal.
c@ index
c	When dumping an entire item, "index" specifies the range of elements
c	to dump. The default is the entire item. For example, to print out
c	lines 10 through 20 of the history item, use:
c	  % itemize in=cm/history index=10,20
c@ format
c	When dumping an entire item, this gives the FORTRAN format specifier
c	to be used. For example, when dumping a real item, you may set:
c	  format=8e15.7
c	The default varies according to the data type.
c@ options
c	Extra processing options. Several options can be given separated by
c	commas. Minimum match is honoured.
c	  nocompact  Normally itemize does not print blocks of identical lines.
c	             Instead it gives a message indicating the number of repetitions
c	             of a line. The "nocompact" option causes it to print the
c	             repetitions. This is useful if the output is being parsed
c	             by other software.
c--
c History:
c
c  rjs        89  Original version.
c  nebk 5-may-89  change output text file name from OUT to LOG to avoid
c                 conflicts with other tasks which use OUT
c  rjs 27-apr-90  Better message for zero length items.
c  ??? ??-???-??  inline doc - messed with un-necessary umsg's  (not PJT)
c  pjt 26-nov-90  file scanner when multiple files used in 'in=', 
c		  added scan= keyword
c  rjs  4-mar-91  Corrected bug in handling "complex" items.
c  pjt  4-mar-91  also used atoif instead of atoi
c  pjt 10-mar-91  fixed empty strings bug (ANSI requires ' ', and not '')
c		  and increase buffers for MAXFILES
c  pjt  7-mar-92  deleted seemingly redundant code in info, doc repair
c  mchw 29oct92	  If "scan=image" makes an index of image parameters.
c  rjs  19mar93   Delete scanning mode.
c  rjs  15aug94   Honour format if given, even if there is only 1 value.
c  rjs  25jul97   Get rid of announcement header.
c  rjs  01aug97   Support wildcards.
c  rjs  29apr09   Added options=nocompact
c------------------------------------------------------------------------
	implicit none
	integer MAXIN
	parameter(MAXIN=128)
	integer range1,range2,tno,iostat,i,nin,l
	character in(MAXIN)*128,item*16,format*16,outlog*64
	logical more,nocom
c
c  Externals.
c
	integer len1
c
c  Get the input parameters.
c
	call keyini
	call mkeyf('in',in,MAXIN,nin)
	call keya('log',outlog,' ')
	call keyi('index',range1,0)
	call keyi('index',range2,range1)
	call keya('format',format,' ')
	call getopt(nocom)
	call keyfin
c
c  Open the listing file, if one is required.
c
	call logopen(outlog,' ')
c
c  Attempt to open the input, as if it were a data set. If this fails,
c  it must be an item. In this case open the higher level.
c
	do i=1,nin
	item = ' '
	call hopen(tno,in(i),'old',iostat)
	if(iostat.ne.0.and.index(in(i),'/').ne.0)then
	  call GetItem(in(i),item)
	  call hopen(tno,in(i),'old',iostat)
	endif
	if(i.eq.1.and.item.eq.' ')
     *	  call output( 'Itemize: Version 1.0 1-Aug-97' )
	if(nin.gt.1)call logwrite(' ',more)
c
c  List the items.
c
	l = len1(in(i))
	if(iostat.ne.0)then
	  call bug('w','Not a Miriad dataset: '//in(i)(1:l))
	else if(item.eq.' ')then
	  call hclose(tno)
	  if(nin.gt.1)
     *	    call logwrite('Items for dataset: '//in(i)(1:l),more)
	  call ShowAll(in(i))
	else
	  call ShowItem(tno,item,range1,range2,format,.not.nocom)
	  call hclose(tno)
	endif
c
	enddo
c
	call logclose
	end
c************************************************************************
	subroutine getopt(nocom)
c
	implicit none
	logical nocom
c
c  Get extra processing options.
c
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=1)
	logical present(NOPTS)
	character opts(NOPTS)*10
	data opts/'nocompact '/
c
	call options('options',opts,present,NOPTS)
	nocom = present(1)
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine GetItem(in,item)
c
	implicit none
	character in*(*),item*(*)
c
c  Extract the trailing item specification from a data-set name.
c  Remove this from `in', and return it in `item'.
c
c  Input/Output:
c    in		Name of the dataset/item. The return is just the name of
c		the dataset.
c  Output:
c    item	Name of the item.
c
c------------------------------------------------------------------------
	integer k1,k2
	logical more
c
c  Externals.
c
	integer len1
c
	k2 = len1(in)
	k1 = k2
	more = .true.
	dowhile(k1.gt.0.and.more)
	  more = in(k1:k1).ne.'/'
	  if(more) k1 = k1 - 1
	enddo
c
	if(k1.eq.k2) call bug('f','Bad name/item specification')
	item = in(k1+1:k2)
	in(k1:k2) = ' '
c
	end
c************************************************************************
	subroutine ShowAll(in)
c
	implicit none
	character in*(*)
c
c  This summarises all the items found in a particular dataset.
c
c  Input:
c    in		Name of the dataset.
c
c------------------------------------------------------------------------
	integer MAXDEPTH
	parameter(MAXDEPTH=10)
	character blanks*(2*MAXDEPTH),name*132
	character item*16,descr*64,type*16
	integer depth,tno(MAXDEPTH),itno(MAXDEPTH),lname(MAXDEPTH)
	integer iostat,n
        character*80 umsg
c
	blanks = ' '
	depth = 0
	call push(in,name,lname,tno,itno,depth,maxdepth)
c
	dowhile(depth.ge.1)
	  call hreada(itno(depth),item,iostat)
	  if(iostat.eq.-1)then
	    call pop(tno,itno,depth)
	  else if(iostat.ne.0)then
	    call bugno('f',iostat)
	  else if(index(item,'/').ne.0)then
            umsg = blanks(1:2*depth)//item
	    call out(1, umsg )
	    call push(item,name,lname,tno,itno,depth,maxdepth)
	  else
	    call hdprobe(tno(depth),item,descr,type,n)
	    call ItemSum(blanks(1:2*depth),item,descr,type,n)
	  endif
	enddo
c
	end
c************************************************************************
	subroutine push(item,name,lname,tno,itno,depth,maxdepth)
c
	implicit none
	character item*(*),name*(*)
	integer maxdepth,lname(maxdepth),tno(maxdepth),itno(maxdepth)
	integer depth
c
c  Go down into a sub-structure of a dataset.
c
c  Inputs:
c    item	Name of the substructure to open up.
c    maxdepth	The size of thevarious arrays.
c
c  Input/Output:
c    depth	The index of where to put the information.
c    tno	Array holding handles of the substructure.
c    itno	Array holding handles of the directory item of a substructure.
c    lname	The length of the name of a substructure.
c    name	The name of the substructure.
c
c------------------------------------------------------------------------
	integer length,iostat
c
c  Externals.
c
	integer len1
c
	if(depth.ge.maxdepth)call bug('f','Too deeply nested')
	depth = depth + 1
c
c  Copy the name, and record its length.
c
	if(depth.eq.1)then
	  lname(depth) = len1(item)
	  if(lname(depth).gt.len(name))call bug('f','Name too long')
	  name(1:lname(depth)) = item(1:lname(depth))
	else
	  length = len1(item)
	  if(length.eq.0)call bug('f','Zero length name')
	  lname(depth) = lname(depth-1) + length + 1
	  if(lname(depth).gt.len(name))call bug('f','Name too long')
	  name(lname(depth-1)+1:lname(depth)) = '/'//item(1:length)
	endif
	if(name(lname(depth):lname(depth)).eq.'/')
     *				lname(depth) = lname(depth) - 1
c
c  Open the things we want.
c
	call hopen(tno(depth),name(1:lname(depth)),'old',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call haccess(tno(depth),itno(depth),'.','read',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
c
	end
c************************************************************************
	subroutine pop(tno,itno,depth)
c
	implicit none
	integer depth,tno(*),itno(*)
c
c  Close up a tree of items, that we have been processing.
c
c  Input:
c    tno	Array of the handles of datasets.
c    itno	Array of the handles of the directory of a dataset.
c
c  Input/Output:
c    depth	The current dataset we are processing in tno and itno.
c
c------------------------------------------------------------------------
	integer iostat
c
	call hdaccess(itno(depth),iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hclose(tno(depth))
	depth = depth - 1
	end
c************************************************************************
	subroutine ShowItem(tno,item,range1,range2,format,docomp)
c
	implicit none
	integer tno,range1,range2
	character item*(*),format*(*)
	logical docomp
c
c  Print out some information about an item.
c
c  Input:
c    tno	Handle of the input dataset.
c    item	Name of the item that we are interested in.
c    range1,range2 The range of elements to print. if these are zero, then
c		all the elements are printed.
c    format	This gives the FORTRAN format to use in printing out the
c		information.
c    docomp	True if compacting of the output is to be attempted.
c
c------------------------------------------------------------------------
	integer OFFI,OFFJ,OFFR,OFFD,OFFC,SIZEI,SIZEJ,SIZER,SIZED,SIZEC
	parameter(OFFI=4, OFFJ=4, OFFR=4, OFFD=8, OFFC=8)
	parameter(SIZEI=4,SIZEJ=2,SIZER=4,SIZED=8,SIZEC=8)
	integer ntypes,maxNelm
	parameter(ntypes=6,maxNelm=64)
c
	logical more
	character descr*32,type*16,line*132,previous*132
	character types(ntypes)*9,defFmt(ntypes)*10,Fmt*16
	integer defNelm(ntypes),Nelm,count,i,j,itype,itno,errno,n
	integer length
	real datr(maxNelm)
	integer dati(maxNelm)
	double precision datd(maxNelm)
c
	data (types(i),defNelm(i),defFmt(i),i=1,ntypes)/
     *	  'integer  ', 6,'i12       ','integer*2',10,'i8        ',
     *	  'real     ', 6,'(1pg12.4) ','double   ', 6,'(1pd12.4) ',
     *	  'complex  ', 3,'(1p2g12.4)','text     ', 1,'a         '/
c
c  Determine information about the item, determine its type.
c
	call hdprobe(tno,item,descr,type,n)
	if(range2.le.0.or.range2.gt.n)range2 = n
	range1 = min(max(range1,1),range2)
c
	itype = 0
	do i=1,ntypes
	  if(type.eq.types(i)) itype = i
	enddo
c
c  Output a summary.
c
	if((n.le.1.and.format.eq.' ').or.itype.eq.0)then
	  call ItemSum('  ',item,descr,type,n)
c
c  Prepare for a dump of some of the values of the item. First check
c  out the format statement.
c
	else
	  call CrackFmt(format,defFmt(itype),defNelm(itype),Fmt,Nelm)
	  Nelm = min(Nelm,MaxNelm)
c
c  Open the item, ready for reading.
c
	  call haccess(tno,itno,item,'read',errno)
	  if(errno.ne.0)call bugno('f',errno)
c
c  If it is a text file, skip the initial records that the user is not
c  interersted in.
c
	  if(type.eq.'text')then
	    do i=1,range1-1
	      call hreada(itno,line,errno)
	      if(errno.ne.0)call bugno('f',errno)
	    enddo
	  endif
c
c  Do the loop which prints out the data.
c
	  count = 0
	  previous = ' '
	  more = .true.
	  i = range1
	  dowhile(i.le.range2.and.more)
	    length = min(range2-i+1,nelm)
c
	    if(type.eq.'integer')then
	      call hreadi(itno,dati,OFFI+SIZEI*(i-1),SIZEI*length,errno)
	      if(errno.eq.0)write(line,fmt)(dati(j),j=1,length)
	    else if(type.eq.'integer*2')then
	      call hreadj(itno,dati,OFFJ+SIZEJ*(i-1),SIZEJ*length,errno)
	      if(errno.eq.0)write(line,fmt)(dati(j),j=1,length)
	    else if(type.eq.'real')then
	      call hreadr(itno,datr,OFFR+SIZER*(i-1),SIZER*length,errno)
	      if(errno.eq.0)write(line,fmt)(datr(j),j=1,length)
	    else if(type.eq.'complex')then
	      call hreadr(itno,datr,OFFC+SIZEC*(i-1),SIZEC*length,errno)
	      if(errno.eq.0)write(line,fmt)(datr(j),j=1,2*length)
	    else if(type.eq.'double')then
	      call hreadd(itno,datd,OFFD+SIZED*(i-1),SIZED*length,errno)
	      if(errno.eq.0)write(line,fmt)(datd(j),j=1,length)
	    else if(type.eq.'text')then
	      call hreada(itno,line,errno)
	    else
	      call bug('f','I cannot get here without a weird problem')
	    endif
c
c  Output the line now. If it is a duplicate of a previously written
c  line, just remember it.
c
	    more = errno.eq.0
	    if(more)then
	      if(docomp.and.line.eq.previous)then
	        count = count + 1
	      else
	        if(count.gt.0)call out(count,previous)
	        count = 0
	        previous = line
	        call out(1,previous)
	      endif
	    endif
c
	    i = i + length
	  enddo
c
c  Finish up.
c
	  if(count.gt.0)call out(count,previous)
	  if(errno.ne.0.and.errno.ne.-1)call bugno('w',errno)
	  call hdaccess(itno,errno)
	endif
c
	end
c************************************************************************
	subroutine out(count,line)
c
	implicit none
	integer count
	character line*(*)
c
c  Output a line. If the count is non-zero, this indicates that there are
c  a number of duplicates. Output a line indicating this, if so.
c
c  Input:
c    lu		Handle of the output text file.
c    count	The number of copies of the line.
c    line	The line itself.
c
c------------------------------------------------------------------------
	integer length
	character num*8
	logical more
c
c  Externals.
c
	integer len1
	character itoaf*8
        character*80 umsg
c
	if(count.eq.1)then
	  length = len1(line)
	  if(length.eq.0)then
	    call logwrite(' ',more)
	  else
	    call logwrite(line(1:length),more)
	  endif
	else
	  num = itoaf(count)
	  length = len1(num)
          umsg = '   *** '//num(1:length)//
     *		   ' more identical lines ***'
	  call logwrite(umsg,more)
	endif
	end
c************************************************************************
	subroutine CrackFmt(format,defFmt,defNelm,Fmt,Nelm)
c
	implicit none
	character format*(*),Fmt*(*),defFmt*(*)
	integer Nelm,defNelm
c
c  Break a format specification, like `10f13.4' into a integer and the
c  format specification. If part of the format is missing or bad, the
c  default is used.
c
c  Input:
c    format	The format string, as given by the user.
c    defFmt	The default format (excluding count).
c    defNelm	The default count.
c
c  Output:
c    Fmt	The format to be used.
c    Nelm	The count to be used.
c
c------------------------------------------------------------------------
	integer k1,k2
	logical more
c
c  Externals.
c
	integer len1
c
	k1 = 1
	k2 = len1(format)
	Nelm = 0
	more = .true.
	dowhile(k1.le.k2.and.more)
	  more = format(k1:k1).ge.'0'.and.format(k1:k1).le.'9'
	  if(more)then
	    Nelm = 10*Nelm + ichar(format(k1:k1)) - ichar('0')
	    k1 = k1 + 1
	  endif
	enddo
	if(Nelm.eq.0) Nelm = defNelm
c
	if(k1.gt.k2)then
	  write(Fmt,'(''('',i3,a,'')'')')Nelm,defFmt
	else if((format(k1:k1).le.'a'.or.format(k1:k1).gt.'z').and.
     *		 format(k1:k1).ne.'(')then
	  write(Fmt,'(''('',i3,a,'')'')')Nelm,defFmt
	  call bug('w','Unrecognised format string -- default used')
	else
	  write(Fmt,'(''('',i3,a,'')'')')Nelm,format(k1:k2)
	endif
	end
c************************************************************************
	subroutine ItemSum(indent,item,descr,type,n)
c
	implicit none
	character indent*(*),item*(*),descr*(*),type*(*)
	integer n
c
c  Output a summary about an item.
c
c  Input:
c    indent	Something to pad the start of each line with.
c    item	The name of the item.
c    descr	A description of the item, as returned by hdprobe.
c    type	The type of the item, as returned by hdprobe.
c    n		The number of elements in the item.
c
c------------------------------------------------------------------------
	character line*80,num*16,it*8
	integer ltype,lnum
c
c  Externals.
c
	integer len1
	character itoaf*16
c
	it = item
	if(descr.eq.'nonexistent')then
	  line = indent//it//' is non-existent ???'
	else if(n.eq.1)then
	  line = indent//it//' = '//descr
	else
	  ltype = len1(type)
	  num = itoaf(n)
	  lnum = len1(num)
	  line = indent//it//'   ('//type(1:ltype)//
     *				' data, '//num(1:lnum)//' elements)'
	endif
	call out(1,line)
	end
