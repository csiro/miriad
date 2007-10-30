c************************************************************************
      PROGRAM puthd
c
c  History:
c    rjs  Dark-ages Original version.
c    rjs  15oct90   Added option to append a description of the variable
c		    to vartable.
c    pjt   4mar91   atod -> atodf
c	   2jul91   improved doc, added some ideas for future backup expansion
c    pjt   9aug91   added history to infile, Joe got nasty
c    pjt   8oct91   small doc changes
c    rjs  11oct91   Small history change.
c    pjt  28oct91   Warning when item type and/or length changes.
c    pjt   7nov91   Allow Alex to use a unit - also fixed
c                   some error checking logic in type vs. rtype (a bug)
c    pjt  12nov91   readability improvement in output message; craytemp
c    pjt  21feb92   fixed ascii/character confusion and treated them the same
c    rjs  11feb93   varcheck was not checking the entire variable name.
c    rjs  21jun93   Really fix the confusion over ascii/character this time.
c		    Varous checks for sensibility and min match.
c    rjs  18aug93   Standardise history comments.
c    rjs  04oct95   Support "units" of time,dms and hms.
c
c= puthd - Change the value of or add a single header item
c& pjt
c: utility
c+
c   PUTHD is a MIRIAD task to add or modify an item in the ``header''
c   of an image or uv dataset. The item CANNOT be an array or any other
c   complex data structure, it must be a single entity. To modify
c   such complex data structures, specialized programs are available.
c@ in
c   The name of an item within a data set. This is given in the
c   form as in the example:
c          puthd in=dataset/item
c@ value
c   The value to be placed in the item. Note only single values can
c   be given, no arrays. An optional second argument can be given to
c   cause conversion of the value before the item is written. Possible
c   values for the units are "time", "arcmin", "arcsec", "hours", "hms"
c   and "dms". Times are given in the standard Miriad form and are
c   converted to Julian dates. An angular unit causes conversion to
c   radians.
c@ type
c   The data type of the argument. Values can be 'integer',
c   'real', 'double' and 'ascii'. The default is determined from the
c   format of the value parameter or from the type of the item if it
c   was already present. Normally you can allow this parameter to default.
c   PUTHD will complain when you change the datatype, but otherwise
c   allow you to do so.
c--
c-----------------------------------------------------------------------
	character PVERSION*(*)
	parameter(PVERSION='Version 1.0 04-Oct-95')
	integer lin,iostat,l,n
	character in*80,item*32,value*64,type*10,descr*32,rtype*16
        character mesg*120, unit*20
	logical ok
	double precision d
c
c  Externals.
c
	integer len1
c-----------------------------------------------------------------------
        call output( 'PUTHD: '//PVERSION )
c
	call keyini
	call keya('in',in,' ')
        call GetVal(value,unit)
	call GetType(type)
	call keyfin
	if(in.eq.' ')call bug('f','Input data-set name is missing')
c
c  Split 'in' into dataset and item, then open the dataset file (in)
c
	call GetItem(in,item)
	call hopen(lin,in,'old',iostat)
	if(iostat.ne.0)then
	   call bug('i','Error opening dataset '//in(1:len1(in)))
	   call bugno('f',iostat)
        endif
c
c  If the units are "time", "hms" or "dms", then set the type
c  accordingly.
c
	if(type.eq.' '.and.
     *	  (unit.eq.'time'.or.unit.eq.'dms'.or.unit.eq.'hms'))
     *	  type = 'double'
c
c  Get info on the item to see if it is present already
c  and check this with what the user has supplied. Catch
c  possible mistakes and warn user 
c
	call hdprobe(lin,item,descr,rtype,n)
	if(rtype.eq.'character')rtype = 'ascii'
	if(type.eq.' ')then
	  if(rtype.eq.'nonexistent')then
	    call DetType(value,type)
	  else
	    type = rtype
	  endif
	endif
c
        if(n.gt.1) then
          write(mesg,
     *        '(''Truncation number of items from '',I6,'' to 1.'')') n
          call bug('w',mesg)
        endif

	if(rtype.eq.'nonexistent') then
          write(mesg,
     *          '(''New item '',a,'' created with datatype '',a)') 
     *          item(1:len1(item)),type(1:len1(type))
          call output(mesg)
        else if(type.ne.rtype) then
          write(mesg,
     *          '(''Changing type of '',a,'' from '',a,'' to '',a)' )
     *        item(1:len1(item)),rtype(1:len1(rtype)),type(1:len1(type))
          call bug('w',mesg)
 	endif
c
	if(type.eq.'integer'.or.type.eq.'real'.or.type.eq.'double')then
	  l = len1(value)
	  if(unit.eq.'time')then
	    call dectime(value(1:l),d,'atime',ok)
	  else if(unit.eq.'hms'.or.unit.eq.'dms')then
	    call decangle(value(1:l),d,unit,ok)
	  else
	    call atodf(value(1:l),d,ok)
 	    if(ok.and.unit.ne.' ')call units(d,unit)
	  endif
	  if(.not.ok)call bug('f','Error decoding numeric value')
	  if(type.eq.'integer')then
	    call wrhdi(lIn,item,nint(d))
	  else if(type.eq.'real')then
	    call wrhdr(lIn,item,real(d))
	  else if(type.eq.'double')then
	    call wrhdd(lIn,item,d)
	  endif
	else if(type.eq.'ascii')then
	  call wrhda(lIn,item,value)
	else
	  call bug('f','Unrecognised variable type '//type)
	endif
c
c  Check if we should add this variable to the "vartable" item.
c
	call hdprobe(lIn,'vartable',descr,rtype,n)
	if(rtype.eq.'text'.and.n.gt.0)call varcheck(lIn,item,type)
c
c  History
c
	call hisopen(lin,'append')
	call hiswrite(lin,'PUTHD: Miriad PutHd: '//PVERSION)
	call hisinput(lin,'PUTHD')
	call hisclose(lin)
c
c  Close up and go home.
c
	call hclose(lin)
	end
c************************************************************************
	subroutine varcheck(lIn,item,type)
c
	implicit none
	integer lIn
	character item*(*),type*(*)
c
c  Check if a particular item is in the vartable, and add it if it does not.
c
c  Inputs:
c    lIn	The handle of the data set.
c    item	The name of the item to check for in "vartable".
c    type	The type of the item (either "real", "integer", "double"
c		or "ascii".
c------------------------------------------------------------------------
	integer itno,iostat,length
	logical more
	character line*64
c
c  Externals.
c
	integer len1
c
	call haccess(lIn,itno,'vartable','read',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening vartable item')
	  call bugno('f',iostat)
	endif

c  Read through vartable, and check each line if it matches this
c  item name.
c
	more = .true.
	call hreada(itno,line,iostat)
	dowhile(iostat.eq.0.and.more)
	  length = len1(line)
	  more = line(3:length).ne.item
	  if(more)call hreada(itno,line,iostat)
	enddo
	if(iostat.ne.0.and.iostat.ne.-1)then
	  call bug('w','Error reading from vartable')
	  call bugno('f',iostat)
	endif
	call hdaccess(itno,iostat)
c
c  If the item name was not found in the vartable, then append it to
c  the end of the vartable.
c
	if(more)then
	  call haccess(lIn,itno,'vartable','append',iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error opening vartable item')
	    call bugno('f',iostat)
	  endif
	  line = type(1:1)//' '//item(1:length)
	  call hwritea(itno,line,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error appending to the vartable item')
	    call bugno('f',iostat)
	  endif
	  call hdaccess(itno,iostat)
	endif
c
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
	subroutine GetType(type)
c
	implicit none
	character type*(*)
c
c  Check the user specified type.
c------------------------------------------------------------------------
	integer nout
	integer nopt
	parameter(nopt=5)
	character opts(nopt)*9
	data opts/'integer  ','real     ','double   ','character',
     *		  'ascii    '/
c
	call keymatch('type',nopt,opts,1,type,nout)
	if(nout.eq.0)then
	  type = ' '
	else if(type.eq.'character')then
	  type = 'ascii'
	endif
	end
c************************************************************************
	subroutine GetVal(value,unit)
c
	implicit none
	character value*(*),unit*(*)
c
c  Check the user specified type.
c------------------------------------------------------------------------
	integer nout
	integer nopt
	parameter(nopt=8)
	character opts(nopt)*10
	data opts/'arcseconds','arcminutes','radians   ',
     *		  'degrees   ','hours     ','dms       ',
     *		  'hms       ','time      '/
c
	call keya('value',value,' ')
	if(value.eq.' ')call bug('f','A value must be given')
	call keymatch('value',nopt,opts,1,unit,nout)
	if(nout.eq.0)unit = ' '
	end
c************************************************************************
	subroutine DetType(value,type)
c
	implicit none
	character value*(*),type*(*)
c
c  Determine the type of a value.
c
c  Input:
c    value	The value.
c
c  Output:
c    type	Either 'integer', 'real', 'double', 'ascii' or 'unknown'.
c
c------------------------------------------------------------------------
	integer l,length
	logical more,numeric
c
	integer len1
c
c  Handle a logical.
c
	type = 'ascii'
	if(value.eq.'T'.or.value.eq.'F')then
	  type = 'logical'
c
c  Check if its a numeric value. If it fails this test, it must be ascii.
c
	else
	  length = len1(value)
	  l = 1
	  if(value(1:1).eq.'+'.or.value(1:1).eq.'-') l = 2
	  numeric = .false.
	  more = .true.
	  type = 'integer'
	  dowhile(l.lt.length.and.more)
	    more = value(l:l).ge.'0'.and.value(l:l).le.'9'
	    if(more) l = l + 1
	    numeric = numeric .or. more
	  enddo
	  if(l.le.length.and.value(l:l).eq.'.') then
	    type = 'real'
	    l = l + 1
	    more = .true.
	  endif
	  dowhile(l.le.length.and.more)
	    more = value(l:l).ge.'0'.and.value(l:l).le.'9'
	    if(more) l = l + 1
	    numeric = numeric .or. more
	  enddo
	  if(l.lt.length.and.numeric.and.
     *	    index('dDeE',value(l:l)).ne.0)then
	    if(value(l:l).eq.'d'.or.value(l:l).eq.'d')then
	      type = 'double'
	    else
	      type = 'real'
	    endif
	    l = l + 1
	    if(l.lt.length.and.
     *		(value(l:l).eq.'+'.or.value(l:l).eq.'-')) l = l + 1
	    more = .true.
	    dowhile(l.le.length.and.more)
	      more = value(l:l).ge.'0'.and.value(l:l).le.'9'
	      if(more) l = l + 1
	    enddo
	  endif
	  if(l.le.length.or..not.numeric) type = 'ascii'
	endif
	end
c***********************************************************************
      SUBROUTINE units(d,unit)
      DOUBLE PRECISION d
      CHARACTER unit*(*)
c
c The following units are understood and converted to:
c
c   arcmin, arcsec, degrees,hours,radians
c
c A minimum match algorithm is use, but a few characters should be
c supplied.
c
c------------------------------------------------------------------------
      include 'mirconst.h'
c
      IF(unit.EQ.'radians'.OR.unit.eq.' ') THEN
	 continue
      ELSE IF(unit.EQ.'arcminutes') THEN
         d = d * DPI / (180d0 * 60d0)
      ELSE IF(unit.EQ.'arcseconds') THEN
         d = d * DPI / (180d0 * 3600d0)
      ELSE IF(unit.EQ.'degrees') THEN
         d = d * DPI / 180d0
      ELSE IF(unit.EQ.'hours') THEN
         d = d * DPI / 12.d0
      ELSE
         call bug('w','Unrecognised units, in UNITS')
      ENDIF

      END
