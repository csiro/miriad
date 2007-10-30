c***********************************************************************
	program uvputhd
c
c   Allows user to change the value of all occurances of a given
c   variable in a uv dataset.
c
c
c  History:
c       lgm  7-sep-90  take off from uvcat made for a quick fix
c       lgm  5-feb-91  brought up to standard and submitted 
c       pjt  2-jul-91  added notes on PUTHD in doc file and more witty comments
c                     also uses keyf() now.
c       mjs  7-apr-92  elim unused var -> elim compiler warning.
c       pjt  6-aug-92  fixed read(,*,) to read(,'(a)',) for avarnew (READVAL)
c pjt/sally 31-mar-97  defined MAXVAL and increased from 8 to 16
c       pjt 17-aug-99  added tabular time dependant input, substantial rewrite
c       tw   4-nov-02  table can now give non-ascii array values
c       pjt 16-jan-03  fix array input from varval= keyword
c          
c  unfinished:
c       - array values for tables
c       - ascii uv variables for tables
c
c------ Inline doc (retrieved with doc to a .doc file) --------------72]
c
c= uvputhd - Allows user to alter values of header variables in uv dataset
c& lgm
c: vis,uv,header
c+
c    UVPUTHD allows the user to change the values of uv variables in a
c    uv dataset. All occurances of the variable are changed to the
c    new value. If the variable is an array, all new values must be
c    entered in sequential order. If the user desires to set all members 
c    of an array to a single value, only one value need be entered.
c    Values can also be inserted from a time ordered table.
c    Note: PUTHD must be used to use the uv override principle, but
c    can only be used for single items, i.e. uv variables which are
c    not an array.
c@ vis	
c    Name of the input MIRIAD dataset. Only one dataset is allowed.
c    No default.
c@ hdvar
c    Name of header variable to be changed. Refer to user manual or
c    run VARLIST to see the selection of allowed variable names.
c    No default.
c@ type
c    Type of variable, either integer (i),real (r), double precision (d), 
c    or ascii (a). Unused if variable already exists in the data file.
c    CAVEAT: Tables do not support ascii types yet.
c    No Default.
c@ length
c    Length of array of variable values. Unused if variable already 
c    exists in data file or table is specified, else set to 1.
c@ varval
c    New values of header variable - if the variable is an array
c    all values must be specified or will assume one value for all.
c    If you want values that change with time, you must use the 
c    table= keyword instead.
c    No default.
c@ table
c    Name of the table that lists time (fractional days since time0) in
c    column 1, and the values of the array in columns 2 through LENGTH+1.
c    Values are simply inserted as soon as the time has passed, no 
c    interpolation is performed.
c    CAVEAT: do not use array variables with tables, and type=ascii cannot
c    be used either. 
c    No default.
c@ time0
c    Offset time as applied to the times in column 1 of the table for
c    tabular input. Legal formats as defined in subroutine DAYJUL, 
c    of which the ISO standard "ccyy-mm-dd[Thh[:mm[:ss.s]]]" is to
c    be preferred. 
c    Default: not used, in which case the offset is 0, in which case 
c    fractional days are really Julian Days.
c@ out
c    Name of the output dataset. No default.
c-----------------------------------------------------------------------
	include 'uvputhd.h'
	character VERSION*(*)
	parameter(VERSION='(Version 16-jan-03 pjt)')
	character varval(MAXVAL)*30,hdvar*10,time0*32
        character outfile*80,infile*80,tabfile*80
        character except(20)*10,newtype*1,line*256
	integer nread,inset,outset,nexcept,nwread,newlong,nval
	double precision preamble(4), jd0, jd1
	complex data(MAXCHAN),wdata(MAXCHAN)
	logical flags(MAXCHAN),wflags(MAXCHAN),there,first
	integer len1,ncols,i
	logical keyprsnt
c
	call output('UVPUTHD: '//version)
	call keyini
	call keyf('vis',infile,' ')
	call keya('hdvar',hdvar,' ')
	call keya('type',newtype,' ')
	call keyi('length',newlong,1)
	call mkeya('varval',varval,MAXVAL,nval)
        call keyf('table',tabfile,' ')
        call keya('time0',time0,' ')
	call keya('out',outfile,' ')
	if (keyprsnt('nvals')) call bug('i',
     *       'keyword "nvals" has been deprecated in UVPUTHD')
	call keyfin
c-----------------------------------------------------------------------
	if(infile.eq.' ') call bug('f'
     *                                  ,'Vis must be specified (vis=)')
	if(hdvar.eq.' ') call bug('f',
     *                              'No uv variable specified (hdvar=)')
        if(nval.le.0 .and. tabfile.eq.' ') call bug('f',
     *                'No value given for variable (varval=) or table=')
	if(outfile.eq.' ') call bug('f',
     *                             'No output dataset specified (out=)')
	if (time0 .eq. ' ') then
	   jd0 = 0.0d0
	else
	   call dayjul(time0,jd0)
	endif
        write(*,*) 'DEBUG: TIME0=',time0,' JD0 = ',jd0
c-----------------------------------------------------------------------

c
c  read in ascii table
c
	if (tabfile .ne. ' ') then
	   call rtable(tabfile,jd0,ncols)
	else
	   ncols = nval
	endif
c
c  open input visfile
c
	call uvopen(inset,infile,'old')
	write(line,'('' Reading data from file: '',a)') infile
	call output(line)
c
c  Set tracking on all file variables and Check if user request variable 
c  is in var table
c
	call invars(inset)
 	call hdcheck(inset,hdvar,there)
	if(there) then
	   write(line,'('' Altering value of '',a,'' in data'')')
     *		hdvar(1:len1(hdvar))
	   call output(line)	
	   write(line,'('' Expecting array of '',i2,'' values'')')
     *		length(yourvar)
	   call output(line)	
	   if (length(yourvar).ne.ncols) then
	      if (nval.eq.1 .and. ncols.eq.1) then
		 do i=2,length(yourvar)
		    varval(i) = varval(1)
		 enddo
		 write(line,'('' Replicating '',i2,'' value'')') ncols
		 call output(line)	
	      else
		 write(line,'('' Found '',i2,'' values'')') ncols
		 call output(line)	
		 call bug('f',' Incorrect number of columns in table')
	      endif
	   endif
	else
	   if(newtype(1:1) .eq. ' ') 
     *	      call bug('f',' Type must be specified for a new variable')
	   write(line,'('' Entering Variable '',a,'' in data file'')')
     *		hdvar(1:len1(hdvar))
	   call output(line)
	   if (tabfile .ne. ' ') newlong=ncols
	   write(line,'('' Creating array of '',i2,'' values'')')
     *		newlong
	   call output(line)
	   call addvar(hdvar,newtype,newlong)
	endif
	except(1) = 'corr'
	except(2) = 'wcorr'
	except(3) = 'tscale'
	except(4) = 'coord'
	except(5) = 'time'
	except(6) = 'baseline'
	except(7) = hdvar
	nexcept   = 7
        call trackall(inset,except,nexcept)
c
c   read ascii input of user header variable values and stick them
c   into the appropriate arrays (non-table input)
c
	if (tabfile .eq. ' ') then
	   call readval(hdvar,varval,nval)
	endif
c
c  Read the first record in visfile
c
	first = .true.
	call uvread(inset,preamble,data,flags,maxchan,nread) 
	if(nread.le.0) call bug('f','No data in input vis file')
c
c  Open the output and copy history
c
        call uvopen(outset,outfile,'new')
        call hdcopy(inset,outset,'history') 
	write(line,'('' Writing data out to file: '',a)') outfile
	call output(line)
c
c  Loop the loop. 
c
	jd1 = 0.0d0
	nmods = 0
        DO WHILE (nread.GT.0)

c           write(*,*) 'TimeJD = ',preamble(3),preamble(3)-jd0

c
c   Copy all unchanged variables to output data set
c
	    call uvcopyvr(inset,outset)
c
c  Copy the variable whose value we are changing to outset
c
	    if(tabfile.eq.' ')then
	      if(there) then
                call varcop(inset,outset,there)
	      else
	        if(first) call varcop(inset,outset,there)
	      endif
	    else
	       if (preamble(3).ne.jd1) then
		  jd1 = preamble(3)
c		  WRITE(*,*) 'New time: ',jd1,' = ',jd1-jd0
		  CALL itable(jd1)
		  CALL varins(inset,outset,there)
	       endif
	    endif
c
c  write uv data record out to outset
c
	    call uvwread(inset,wdata,wflags,maxchan,nwread)
	    if (nwread .gt. 0) then
                call uvwwrite(outset,wdata,wflags,nwread)
	    else
	        if(first)call output('No Wideband data')
	    endif
	    call uvwrite(outset,preamble,data,flags,nread)

c
c  read in next uv data record from inset
c
	    first = .false.
            call uvread(inset,preamble,data,flags,maxchan,nread)
        END DO
c
c  Finish up the history, and close up shop.
c
	write(*,*) 'DEBUG: Variable modified ',nmods,' times.'
        call hisopen(outset,'append')
        call hiswrite(outset,'UVPUTHD: Miriad UVPUTHD '//version)
	call hisinput(outset,'UVPUTHD')
        call hisclose (outset)
	call uvclose(outset)
	call uvclose(inset)
	stop
	end
c***********************************************************************
	subroutine invars(inset)
c
c  Retrieves all variable names and types from file for future
c  use. Results are stuffed into common block head.
c
        include 'uvputhd.h'
 
        integer i,inset,item,iostat,nread
        double precision preamble(4)
        complex data(MAXCHAN)
        logical flags(MAXCHAN)
        character vname*11
        logical eof,upd
c 
        CALL uvread(inset,preamble,data,flags,MAXCHAN,nread)
        CALL haccess(inset,item,'vartable','read',iostat)
        DO i=1,MAXVAR
           CALL hreada(item,vname,eof)
           IF(eof) THEN
	      CALL hdaccess(item,iostat)
	      CALL uvrewind(inset)
	      RETURN
	   ENDIF
           IF(vname(3:6).ne.'corr' .and. vname(3:7).ne.'wcorr') then
              CALL uvprobvr(inset,vname(3:10),type(i),length(i),upd)
              hdvars(i) = vname(3:10)
              nhdvars = i
	   ENDIF
	ENDDO
        CALL bug('f',
     *     'invars: Too many UV variables, increase MAXVAR')
        END

c************************************************************************
        SUBROUTINE trackall(inset,except,nexcept)
c
c   Marks all variable in input data set for copying to output
c   data set. Assumes that the dataset is already open and at
c   begining.
c
        include 'uvputhd.h'

        INTEGER ivar,i,inset,item,iostat,nread,nexcept
	DOUBLE PRECISION preamble(4)
	COMPLEX data(MAXCHAN)
	LOGICAL flags(MAXCHAN)
        CHARACTER vname*11,except(20)*10
        LOGICAL eof,track

        CALL uvread(inset,preamble,data,flags,MAXCHAN,nread)
        CALL haccess(inset,item,'vartable','read',iostat)

        DO ivar=1,MAXVAR
           CALL hreada(item,vname,eof)
           IF(eof) THEN
	      CALL hdaccess(item,iostat)
	      CALL uvrewind(inset)
	      RETURN
	   ENDIF
	   track = .true.
	   DO i=1,nexcept
	      if(vname(3:10) .eq. except(i)) track = .false.
   	   ENDDO
           IF(track) THEN
 		CALL uvtrack(inset,vname(3:10),'c')
C	   else
C	   	write(text,'(''Autocopy not set for '',a)') 
C     *                           vname(3:10)
C	      	call output(text)
	   ENDIF
	ENDDO
        CALL bug('f',
     *       'trackall: Too many UV variables, increase MAXVAR')
        END
c***********************************************************************
	subroutine hdcheck(inset,hdvar,there)
c
c    Checks to see of the user input header variable name is in the
c    var table for the requested data set. 
c    returns true in logical there if it is...
c
	character hdvar*(*)
	integer inset,i
	logical there
c
        include 'uvputhd.h'

c
	there =.false.
	do i=1,nhdvars
	   if(hdvar .eq. hdvars(i)) then
	     there = .true.
	     yourvar = i
             call uvtrack(inset,hdvar,'u')
	   endif
	enddo
	end
c***********************************************************************
	subroutine addvar(hdvar,newtype,newlong)
c
c   Add new variable name, type and length to list of known
c   header variables so that it can be written by varcop later.
c
	character*10 hdvar,newtype*1
	integer newlong
c
        include 'uvputhd.h'
c     
	if (nhdvars.EQ.MAXVAR) call bug('f',
     *           'addvar: Too many UV variables, increase MAXVAR')
	nhdvars = nhdvars+1
	hdvars(nhdvars) = hdvar
	type(nhdvars)   = newtype(1:1)
	length(nhdvars) = newlong
	yourvar         = nhdvars
	return
	end
c***********************************************************************
	subroutine varcop(inset,outset,there)
	integer inset,outset
        logical there
c
c   Change the user selected header variable to the new value if
c   it was updated in data record being read
c
	integer nvals
	character vtype*1,vname*10
        logical update
c 
        include 'uvputhd.h'
c
        vname = hdvars(yourvar)
	vtype = type(yourvar)
	nvals = length(yourvar)
	if(there) then
	   call uvprobvr(inset,vname,vtype,nvals,update)
	   if(update) then
	      nmods = nmods + 1
c           write(*,*) 'Updating there ',vtype,nvals
	   if(vtype .eq. 'i') call uvputvri(outset,vname,ivarnew,nvals)
	   if(vtype .eq. 'r') call uvputvrr(outset,vname,rvarnew,nvals)
	   if(vtype .eq. 'd') call uvputvrd(outset,vname,dvarnew,nvals)
	   if(vtype .eq. 'a') call uvputvra(outset,vname,avarnew)
	   endif
	else
c           write(*,*) 'Updating not there ',vtype,nvals
	      nmods = nmods + 1
           if(vtype .eq. 'i') call uvputvri(outset,vname,ivarnew,nvals)
           if(vtype .eq. 'r') call uvputvrr(outset,vname,rvarnew,nvals)
           if(vtype .eq. 'd') call uvputvrd(outset,vname,dvarnew,nvals)
           if(vtype .eq. 'a') call uvputvra(outset,vname,avarnew)
	endif
	return
	end
c***********************************************************************
	SUBROUTINE varins(inset,outset,there)
	INTEGER inset,outset
        LOGICAL there
c
c   Change the user selected header variable to the new value if
c   it was updated in data record being read, this time from the
c   table where the index had been stored previously
c
	INTEGER nvals,i,idxtab
	CHARACTER vtype*1,vname*10
        LOGICAL update
c 
        include 'uvputhd.h'
c
        vname = hdvars(yourvar)
	vtype = type(yourvar)
	nvals = length(yourvar)
c	write(*,*) 'varins: ',tidx,dvarnew(tidx*nvals)
	DO i=1,nvals
	   idxtab=(tidx-1)*nvals+i
	   if(vtype .eq. 'i') ivarnew(i) = dvarnew(idxtab)
	   if(vtype .eq. 'r') rvarnew(i) = dvarnew(idxtab)
	   if(vtype .eq. 'd') dvarnew(i) = dvarnew(idxtab)
	   if(vtype .eq. 'a') call bug('f','No ascii from tables')
	ENDDO

	IF(there) THEN
	   call uvprobvr(inset,vname,vtype,nvals,update)
	   if(update) then
	      nmods = nmods + 1
c           write(*,*) 'Updating there ',vtype,nvals
	   if(vtype .eq. 'i') call uvputvri(outset,vname,ivarnew,nvals)
	   if(vtype .eq. 'r') call uvputvrr(outset,vname,rvarnew,nvals)
	   if(vtype .eq. 'd') call uvputvrd(outset,vname,dvarnew,nvals)
	   if(vtype .eq. 'a') call uvputvra(outset,vname,avarnew)
	   endif
	ELSE
	      nmods = nmods + 1
c           write(*,*) 'Updating not there ',vtype,nvals
           if(vtype .eq. 'i') call uvputvri(outset,vname,ivarnew,nvals)
           if(vtype .eq. 'r') call uvputvrr(outset,vname,rvarnew,nvals)
           if(vtype .eq. 'd') call uvputvrd(outset,vname,dvarnew,nvals)
           if(vtype .eq. 'a') call uvputvra(outset,vname,avarnew)
	ENDIF
	END
c***********************************************************************
        subroutine readval(hdvar,varval,nvals)
c
c   read ascii input of user header variable values and stick them
c   into the appropriate arrays
c
	integer nvals
	character hdvar*(*),varval(nvals)*(*)
c
        include 'uvputhd.h'
	character vtype*1
	integer i,varlen
c 
	varlen   = length(yourvar)
c	write (*,*) 'nvals is ',nvals, ' varlen is ', varlen
	vtype = type(yourvar)
	if(nvals .eq. 1) then
	   if(vtype .eq. 'i')
     *         read(varval(1),*,err=990) ivarnew(1)
           if(vtype .eq. 'r')      
     *         read(varval(1),*,err=990) rvarnew(1)
           if(vtype .eq. 'd')      
     *         read(varval(1),*,err=990) dvarnew(1)	      
           if(vtype .eq. 'a')      
     *         read(varval(1),'(a)',err=990) avarnew
	   if(varlen .gt. 1 .and. vtype .ne. 'a') then
              do 200 i=1,varlen
    	         ivarnew(i) = ivarnew(1)
                 rvarnew(i) = rvarnew(1)
                 dvarnew(i) = dvarnew(1)
  200	      continue
	   endif
	else
           if(vtype .eq. 'a')
     *        call bug('f','Ascii variables of length 1 only')
	   do 300 i=1,nvals
              if(vtype .eq. 'i')
     *           read(varval(i),*,err=990) ivarnew(i)
              if(vtype .eq. 'r')     
     *           read(varval(i),*,err=990) rvarnew(i)
              if(vtype .eq. 'd')
     *           read(varval(i),*,err=990) dvarnew(i)
  300	   continue
	   if(varlen .gt. nvals) then
	      do 400 i=nvals+1,varlen
                 ivarnew(i) = ivarnew(nvals)
                 rvarnew(i) = rvarnew(nvals)
                 dvarnew(i) = dvarnew(nvals)
  400	      continue
	   endif
	endif
	return
  990	call bug('f','Error in reading new variable values')
	return
	end
c***********************************************************************
	SUBROUTINE rtable(fname,jd0,ncols)
	CHARACTER fname*(*)
	DOUBLE PRECISION jd0
	include 'uvputhd.h'
	INTEGER tno,iostat,len1,tlen,i,ncols,MAXCOLS
	PARAMETER (MAXCOLS=50)
	CHARACTER line*256
	DOUBLE PRECISION dtime,dvar(MAXCOLS)

	WRITE(*,*) 'DEBUG: Opening TABLE ',fname(1:len1(fname))
	CALL txtopen(tno,fname,'old',iostat)
	IF (iostat.NE.0) CALL bug('f','Could not open table file')
	tlen=1
	nttable=0
	ncols=MAXCOLS
	DO WHILE(tlen.GT.0 .AND. iostat.EQ.0 .AND. nttable.LT.MAXVAL)
	   CALL txtread(tno,line,tlen,iostat)
	   IF(tlen.GT.0 .AND. iostat.EQ.0)THEN
	      nttable = nttable + 1
	      READ(line,*,err=990) dtime,(dvar(i),i=1,ncols)
 990	      ncols=i-1
c	      WRITE(*,*) 'LINE: ',line(1:len1(line))
c	      WRITE(*,*) 'INPUT: ',dtime,(dvar(i),i=1,ncols)
	      IF (nttable.LE.MAXVAL) THEN
		 atime(nttable) = dtime+jd0
		 do i = 1,ncols
		    dvarnew((nttable-1)*ncols+i) = dvar(i)
		 enddo
	      ENDIF
	   ENDIF
	ENDDO
	WRITE(*,*) 'DEBUG: Read ',nttable,' lines ',ncols,' cols'
	CALL txtclose(tno)
	tidx=0
	RETURN

	END
c***********************************************************************
	SUBROUTINE itable(jd1)
	DOUBLE PRECISION jd1
c
c  Lookup at time 'jd1' into the array atime where (jd0 offsets had been 
c  applied previously)
c
	include 'uvputhd.h'

	LOGICAL done
	INTEGER i

	done = .FALSE.
        i = 0

	DO WHILE(.NOT.done)
	   IF(tidx.GT.0)THEN
	      IF(jd1.GE.atime(tidx+1))THEN
c		 write(*,*) 'Updating to tidx=',tidx+1,' at ',jd1,
c	1	      ' ',dvarnew(tidx+1)
		 i = tidx
		 tidx = 0
	      ELSE IF(jd1.LT.atime(tidx+1)) THEN
		 done = .TRUE.
	      ELSE
		 tidx = 0
	      ENDIF
	   ELSE
	      IF(i.EQ.0) THEN
	         WRITE(*,*)'JD range in table: ',atime(1),atime(nttable)
		 i = 1
              ENDIF
c Allow small amount (86 seconds) of extrapolation back in time
	      IF(atime(1).GT.jd1+0.001) THEN
		 WRITE(*,*) 1,atime(1)
		 CALL bug('f','Table starts too late')
	      ENDIF
c Allow arbitrary amount of extrapolation forward in time
	      IF(atime(nttable).LT.jd1) THEN
		 tidx=nttable
		 RETURN
c                WRITE(*,*) nttable,atime(nttable),' (Change time0=)'
c		 WRITE(*,*) jd1
c		 CALL bug('f','Table ends too early')
	      ENDIF
	      DO WHILE(i.LT.nttable)
		 IF(atime(i+1).GT.jd1) THEN
c		    write(*,*) 'Found new index ',i,' ',dvarnew(i)
		    tidx = i
		    RETURN
		  ENDIF
		  i = i + 1
	      ENDDO
	      
	   ENDIF
	ENDDO

	END
