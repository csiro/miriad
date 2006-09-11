c************************************************************************
	program gethd
	implicit none
c= GetHd -- Print the value of a header item.
c& rjs
c: miscellaneous
c	GetHd simply prints the value of a header item. Its main use will
c	be in scripts, where it can be used to extract information from
c	the image header.
c@ in
c	The name of an item within a dataset. This is given in the form
c	  in=dataset/item
c	There is no default. The dataset may be either a uv or image data-set. 
c@ format
c	For numeric values, this determines the way that the output value
c	is converted and formated. Possible values are:
c	  default   No conversion. This is the default.
c	  time	    The value is assumed to be a Julian day, and is formatted
c	            into Miriad's date format.
c	  pol       The value is assumed to be a polarization tag, and is
c	            formatted as I,Q,U,V,XX,YY, etc.
c	The following assume that the value is in radians.
c	  arcsec    The value is converted to arcsec.
c	  arcmin    The value is converted to arcmin.
c	  degrees   The value is converted to degrees.
c	  hours	    The value is converted to hours.
c	  hms       The value is formatted as hh:mm:ss.
c	  dms	    The value is formatted as dd:mm:ss.
c@ log
c	Output log file. Default is the terminal.
c--
c  History:
c    rjs  07dec95 Original version (supercedes some imhead functionality).
c    rjs  06feb00 Added format keyword.
c------------------------------------------------------------------------
	include 'mirconst.h'
	character in*128,item*32,logf*64,descr*32,type*16,form*8,line*80
	logical more
	integer n,lIn,iostat,ltype,ldescr,k1,k2
	double precision value
c
c  Externals.
c
	character itoaf*8,polsc2p*8,rangleh*32,hangleh*32
	integer len1
c
c  NOTE: we do not give a version line, as this would clutter the output
c  that is presumably being piped or captured.
c
	call keyini
	call keya('in',in,' ')
	call getform(form)
	call keya('log',logf,' ')
	call keyfin
	if(in.eq.' ')call bug('f','Input name is missing')
c
c  Get the item and open the file.
c
	call getitem(in,item)
	call hopen(lin,in,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening the data-set '//in)
	  call bugno('f',iostat)
	endif
c
	call logOpen(Logf,' ')
	if(form.eq.'default')then	
	  call hdprobe(lin,item,descr,type,n)
	  if(n.gt.1)then
	    descr = itoaf(n)
	    ldescr = len1(descr)
	    ltype  = len1(type)
	    descr(ldescr+1:) = ' '//type(1:ltype)//' values'
	  endif
	else
	  call rdhdd(lin,item,value,0.d0)
	  if(form.eq.'arcsec')then
	    write(line,'(1pd13.6)')value * 3600 * 180 / DPI
	  else if(form.eq.'arcmin')then
	    write(line,'(1pd13.6)')value * 60 * 180 / DPI
	  else if(form.eq.'hours')then
	    write(line,'(1pd13.6)')value * 12 / DPI
	  else if(form.eq.'degrees')then
	    write(line,'(1pd13.6)')value * 180 / DPI
	  else if(form.eq.'hms')then
	    line = hangleh(value)
	  else if(form.eq.'dms')then
	    line = rangleh(value)
	  else if(form.eq.'time')then
	    call julday(value,'H',line)
	  else if(form.eq.'pol')then
	    line = polsc2p(nint(value))
	  else
	    call bug('f','Unrecognised format: '//form)
	  endif
	  k1 = 1
	  k2 = len1(line)
	  call getfield(line,k1,k2,descr,n)
	endif
	if(n.gt.0)call logWrite(descr,more)
	call logClose
	call hclose(lin)
	end
c************************************************************************
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
	subroutine getform(form)
c
	implicit none
	character form*(*)
c
c  Get the format that the user wants.
c
c------------------------------------------------------------------------
	integer NFORMS
	parameter(NFORMS=9)
	character forms(NFORMS)*8
	integer nout
c
	data forms/  'default ','arcsec  ','arcmin  ','degrees ',
     *	  'hours   ','hms     ','dms     ','pol     ','time    '/
c
	call keymatch('format',NFORMS,forms,1,form,nout)
	if(nout.eq.0)form = forms(1)
c
	end
