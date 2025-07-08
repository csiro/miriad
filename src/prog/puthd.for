      program puthd

c= puthd - Change the value of or add a single header item
c& pjt
c: utility
c+
c       PUTHD is a MIRIAD task to add or modify an item in the "header"
c       of an image or uv dataset.  The item CANNOT be an array or any
c       other complex data structure, it must be a single entity.  To
c       modify such complex data structures, specialized programs are
c       available.
c
c@ in
c       The name of an item within a data set.  This is given in the
c       form as in the example:
c          puthd in=dataset/item
c@ value
c       The value to be placed in the item.  Note only single values can
c       be given, no arrays.
c
c       An optional second units argument can be given to convert the
c       value before the item is stored.  Possible units are "degrees",
c       "arcminutes", "arcseconds", "time", "hours", "hms", and "dms"
c       (with mininum match).  Times are given in the standard Miriad
c       form and are stored as Julian dates.  Angular units are
c       converted if necessary and stored in radians.  bpa, being the
c       exception, is stored in degrees, but value is still interpreted
c       as radians unless you specify "degrees" as the unit.
c@ type
c       The data type of the argument.  Values can be 'integer', 'real',
c       'double' and 'ascii'.  The default is determined from the format
c       of the value parameter or from the type of the item if it was
c       already present.  Normally you can allow this parameter to
c       default.
c
c       PUTHD will complain when you change the datatype, but otherwise
c       allow you to do so.
c--
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      logical   ok
      integer   iostat, l, lin, n
      double precision d
      character descr*32, in*80, item*32, mesg*120, rtype*16, type*10,
     *          unit*20, value*64, version*72

      external  len1, versan
      integer   len1
      character versan*72
c-----------------------------------------------------------------------
      version = versan ('puthd',
     *                  '$Revision: 1.4 $',
     *                  '$Date: 2021/03/08 00:37:26 $')

      call keyini
      call keya('in',in,' ')
      call GetVal(value,unit)
      call GetType(type)
      call keyfin
      if (in.eq.' ') call bug('f','Input data-set name is missing')

c     Split 'in' into dataset and item, then open the dataset file (in).
      call GetItem(in,item)
      call hopen(lin,in,'old',iostat)
      if (iostat.ne.0) then
         call bug('i','Error opening dataset '//in(1:len1(in)))
         call bugno('f',iostat)
      endif

c     If the units are "time", "hms" or "dms", then set the type
c     accordingly.
      if (type.eq.' ' .and.
     *  (unit.eq.'time' .or. unit.eq.'dms' .or. unit.eq.'hms'))
     *  type = 'double'

c     Get info on the item to see if it is present already and check
c     this with what the user has supplied.  Catch possible mistakes and
c     warn user.
      call hdprobe(lin,item,descr,rtype,n)
      if (rtype.eq.'character') rtype = 'ascii'
      if (type.eq.' ') then
        if (rtype.eq.'nonexistent') then
          call DetType(value,type)
        else
          type = rtype
        endif
      endif

      if (n.gt.1) then
        write(mesg,
     *      '(''Truncation number of items from '',i6,'' to 1.'')') n
        call bug('w',mesg)
      endif

      if (rtype.eq.'nonexistent') then
        write(mesg,
     *        '(''New item '',a,'' created with datatype '',a)')
     *        item(1:len1(item)),type(1:len1(type))
        call output(mesg)
      else if (type.ne.rtype) then
        write(mesg,
     *        '(''Changing type of '',a,'' from '',a,'' to '',a)')
     *      item(1:len1(item)),rtype(1:len1(rtype)),type(1:len1(type))
        call bug('w',mesg)
      endif

      if (type.eq.'integer' .or.
     *    type.eq.'real'    .or.
     *    type.eq.'double') then
        l = len1(value)
        if (unit.eq.'time') then
          call dectime(value(1:l),d,'atime',ok)
        else if (unit.eq.'hms' .or. unit.eq.'dms') then
          call decangle(value(1:l),d,unit,ok)
        else
          call atodf(value(1:l),d,ok)
          if (ok .and. unit.ne.' ') call units(d,unit)

c          bpa is stored in degrees.  (WHY?)
           if (item.eq.'bpa') d = d * DR2D
        endif

        if (.not.ok) call bug('f','Error decoding numeric value')

        if (type.eq.'integer') then
          call wrhdi(lIn,item,nint(d))
        else if (type.eq.'real') then
          call wrhdr(lIn,item,real(d))
        else if (type.eq.'double') then
          call wrhdd(lIn,item,d)
        endif

      else if (type.eq.'ascii') then
        call wrhda(lIn,item,value)
      else
        call bug('f','Unrecognised variable type '//type)
      endif

c     Check if we should add this variable to the "vartable" item.
      call hdprobe(lIn,'vartable',descr,rtype,n)
      if (rtype.eq.'text' .and. n.gt.0) call varcheck(lIn,item,type)

c     History.
      call hisopen(lin,'append')
      call hiswrite(lin,'PUTHD: Miriad PutHd: ' // version)
      call hisinput(lin,'PUTHD')
      call hisclose(lin)

c     Close up and go home.
      call hclose(lin)
      end

c***********************************************************************

      subroutine varcheck(lIn,item,type)

      integer lIn
      character item*(*),type*(*)
c-----------------------------------------------------------------------
c  Check if a particular item is in the vartable, add it if not.
c
c  Inputs:
c    lIn        The handle of the data set.
c    item       The name of the item to check for in "vartable".
c    type       The type of the item (either 'real', 'integer', 'double'
c               or 'ascii'.
c-----------------------------------------------------------------------
      integer   iostat, itno, length
      logical   more
      character line*64

      external len1
      integer  len1
c-----------------------------------------------------------------------
      call haccess(lIn,itno,'vartable','read',iostat)
      if (iostat.ne.0) then
        call bug('w','Error opening vartable item')
        call bugno('f',iostat)
      endif

c     Look for this item in vartable.
      more = .true.
      call hreada(itno,line,iostat)
      do while (iostat.eq.0 .and. more)
        length = len1(line)
        more = line(3:length).ne.item
        if (more) call hreada(itno,line,iostat)
      enddo

      if (iostat.ne.0 .and. iostat.ne.-1) then
        call bug('w','Error reading from vartable')
        call bugno('f',iostat)
      endif

      call hdaccess(itno,iostat)

c     If the item name was not found in the vartable, then append it to
c     the end of the vartable.
      if (more) then
        call haccess(lIn,itno,'vartable','append',iostat)
        if (iostat.ne.0) then
          call bug('w','Error opening vartable item')
          call bugno('f',iostat)
        endif

        line = type(1:1)//' '//item
        call hwritea(itno,line,iostat)

        if (iostat.ne.0) then
          call bug('w','Error appending to the vartable item')
          call bugno('f',iostat)
        endif

        call hdaccess(itno,iostat)
      endif

      end

c***********************************************************************

      subroutine GetItem(in,item)

      character in*(*), item*(*)
c-----------------------------------------------------------------------
c  Extract the trailing item specification from a data-set name.
c  Remove this from 'in', and return it in 'item'.
c
c  Input/Output:
c    in         Name of the dataset/item.  The return is just the name
c               of the dataset.
c  Output:
c    item       Name of the item.
c-----------------------------------------------------------------------
      integer k1,k2
      logical more

      external len1
      integer  len1
c-----------------------------------------------------------------------
      k2 = len1(in)
      k1 = k2
      more = .true.
      do while (k1.gt.0 .and. more)
        more = in(k1:k1).ne.'/'
        if (more) k1 = k1 - 1
      enddo

      if (k1.eq.k2) call bug('f','Bad name/item specification')
      item = in(k1+1:k2)
      in(k1:k2) = ' '

      end

c***********************************************************************

      subroutine GetType(type)

      character type*(*)
c-----------------------------------------------------------------------
c  Check the user specified type.
c-----------------------------------------------------------------------
      integer    NOPT
      parameter (NOPT=5)

      integer   nout
      character opts(NOPT)*9

      data opts/'integer  ','real     ','double   ','character',
     *          'ascii    '/
c-----------------------------------------------------------------------
      call keymatch('type', NOPT, opts, 1, type, nout)
      if (nout.eq.0) then
        type = ' '
      else if (type.eq.'character') then
        type = 'ascii'
      endif

      end

c***********************************************************************

      subroutine GetVal(value,unit)

      character value*(*),unit*(*)
c-----------------------------------------------------------------------
c  Check the user specified type.
c-----------------------------------------------------------------------
      integer    NOPT
      parameter (NOPT=8)

      integer nout
      character opts(NOPT)*10

      data opts/'arcseconds','arcminutes','radians   ',
     *          'degrees   ','hours     ','dms       ',
     *          'hms       ','time      '/
c-----------------------------------------------------------------------
      call keya('value',value,' ')
      if (value.eq.' ') call bug('f','A value must be given')
      call keymatch('value', NOPT, opts, 1, unit, nout)
      if (nout.eq.0) unit = ' '

      end

c***********************************************************************
      subroutine DetType(value,type)

      character value*(*),type*(*)
c-----------------------------------------------------------------------
c  Determine the type of a value.
c
c  Input:
c    value      The value.
c
c  Output:
c    type       'integer', 'real', 'double', 'ascii', or 'unknown'.
c-----------------------------------------------------------------------
      integer l,length
      logical more,numeric

      integer len1
c-----------------------------------------------------------------------

c     Handle a logical.
      type = 'ascii'
      if (value.eq.'T' .or. value.eq.'F') then
        type = 'logical'

      else
c       Is it a numeric value?
        length = len1(value)
        l = 1
        if (value(1:1).eq.'+' .or. value(1:1).eq.'-') l = 2
        numeric = .false.
        more = .true.
        type = 'integer'

        do while (l.lt.length .and. more)
          more = value(l:l).ge.'0' .and. value(l:l).le.'9'
          if (more) l = l + 1
          numeric = numeric .or. more
        enddo

        if (l.le.length .and. value(l:l).eq.'.') then
          type = 'real'
          l = l + 1
          more = .true.
        endif

        do while (l.le.length .and. more)
          more = value(l:l).ge.'0' .and. value(l:l).le.'9'
          if (more) l = l + 1
          numeric = numeric .or. more
        enddo

        if (l.lt.length .and. numeric .and.
     *    index('dDeE',value(l:l)).ne.0) then
          if (value(l:l).eq.'d' .or. value(l:l).eq.'d') then
            type = 'double'
          else
            type = 'real'
          endif

          l = l + 1
          if (l.lt.length .and.
     *        (value(l:l).eq.'+' .or. value(l:l).eq.'-')) l = l + 1
          more = .true.
          do while (l.le.length .and. more)
            more = value(l:l).ge.'0' .and. value(l:l).le.'9'
            if (more) l = l + 1
          enddo
        endif

c       If all tests failed it must be ascii.
        if (l.le.length .or. .not.numeric) type = 'ascii'
      endif

      end

c***********************************************************************

      subroutine units(d, unit)

      double precision d
      character unit*(*)
c-----------------------------------------------------------------------
c  The following units are understood and converted to:
c
c   arcmin, arcsec, degrees, hours, radians
c
c  A minimum match algorithm is used, but a few characters should be
c  supplied.
c-----------------------------------------------------------------------
      include 'mirconst.h'
c-----------------------------------------------------------------------
      if (unit.eq.'radians' .or. unit.eq.' ') then
        continue
      else if (unit.eq.'arcminutes') then
        d = (d / 60d0) * DD2R
      else if (unit.eq.'arcseconds') then
        d = d * DAS2R
      else if (unit.eq.'degrees') then
        d = d * DD2R
      else if (unit.eq.'hours') then
        d = d * DPI / 12d0
      else
        call bug('w','Unrecognised units, in UNITS')
      endif

      end
