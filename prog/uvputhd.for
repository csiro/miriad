c***********************************************************************
	program uvputhd
c
c   Allows user to change the value of all occurances of a given
c   variable in a uv dataset.
c
c
c  History:
c      lgm  7-sep-90  take off from uvcat made for a quick fix
c      lgm  5-feb-91  brought up to standard and submitted 
c      pjt  2-jul-91  added notes on PUTHD in doc file and more witty comments
c                     also uses keyf() now.
c      mjs  7-apr-92  elim unused var -> elim compiler warning.
c      pjt  6-aug-92  fixed read(,*,) to read(,'(a)',) for avarnew (READVAL)
c      pjt/sally 31-mar-97  defined MAXVAL and increased from 8 to 16
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
c    Note: PUTHD must be used to use the uv override principle, but
c    can only be used for single items, i.e. uv variables which are
c    not an array.
c@ vis	
c    Name of the input MIRIAD dataset. Only one dataset is allowed.
c    No default.
c@ hdvar
c    Name of header variable to be changed. Refer to user manual or
c    run VARLIST to see the selection of allowed variable names.
c@ type
c    Type of variable, either integer (i),real (r), double precision (d), 
c    or ascii (a). Unused if variable already exists in the data file.
c@ length
c    Length of array of variable values. Unused if variable already 
c    exists in data file.
c@ nvals
c    Number of new values for varibles that are being entered in varval.
c@ varval
c    New values of header variable - if the variable is an array
c    all values must be specified or will assume one value for all.
c@ out
c    Name of the output dataset. No default.
c-----------------------------------------------------------------------
        include 'maxdim.h'
	INTEGER MAXVAL
	PARAMETER (MAXVAL=16)
	character version*(*),infile*50,varval(MAXVAL)*30,hdvar*10
        character outfile*50,except(20)*10,newtype*1,line*80
	parameter(version='(Version 31-mar-97)')
	integer nread,inset,outset,nexcept,nwread,nvals,newlong,nval
	double precision preamble(4)
	complex data(MAXCHAN),wdata(MAXCHAN)
	logical flags(MAXCHAN),wflags(MAXCHAN),there,first
c
	call output('UVPUTHD: '//version)
	call keyini
	call keyf('vis',infile,' ')
	call keya('hdvar',hdvar,' ')
	call keya('type',newtype,' ')
	call keyi('length',newlong,1)
	call keyi('nvals',nvals,1)
	call mkeya('varval',varval,MAXVAL,nval)
	call keya('out',outfile,' ')
	call keyfin
c-----------------------------------------------------------------------
	if(infile.eq.' ') call bug('f'
     *                                  ,'Vis must be specified (vis=)')
	if(hdvar.eq.' ') call bug('f',
     *                              'No uv variable specified (hdvar=)')
	if(nvals .eq. 0) call bug('f',
     *                                       'NVALS = 0 is not allowed')
        if(varval(1).eq.' ') call bug('f',
     *                          'No value given for variable (varval=)')
	if(outfile.eq.' ') call bug('f',
     *                             'No output dataset specified (out=)')
	if (nval.NE.nvals) call bug('f','Wrong number of varval''s')

c-----------------------------------------------------------------------
c
c  open input file
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
     1		   hdvar
	   call output(line)
	else
	   if(newtype(1:1) .eq. ' ') 
     1	      call bug('f',' Type must be specified for a new variable')
	   write(line,'('' Entering Variable '',a,'' in data file'')')
     1				hdvar
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
c   into the appropriate arrays
c
	call readval(hdvar,varval,nvals)
c
c  Read the first record
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
  100 	continue
c
c   Copy all unchanged variables to output data set
c
	call uvcopyvr(inset,outset)
c
c  Copy the variable whose value we are changing to outset
c
	if(there) then
           call varcop(inset,outset,there)
	else
	   if(first) call varcop(inset,outset,there)
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
c
c  if nread .gt. 0 loop to continue reading and writing
c
	if(nread .gt. 0) go to 100
c
c  Finish up the history, and close up shop.
c
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
        include 'maxdim.h'
 
        integer ivar,inset,item,iostat,nread
        double precision preamble(4)
        complex data(MAXCHAN)
        logical flags(MAXCHAN)
        character varname*11
        logical eof,update
c 
        character hdvars(500)*10,type(500)*1,avarnew*20
        real rvarnew(100) 
        double precision dvarnew(100)    
        integer nhdvars,length(500),yourvar,ivarnew(100)
        common /head_c/ hdvars,type,avarnew   
        common /head/ nhdvars,length,yourvar,ivarnew,rvarnew
        common /head_d/ dvarnew
c
 
        call uvread(inset,preamble,data,flags,MAXCHAN,nread)
        call haccess(inset,item,'vartable','read',iostat)
        do 100 ivar=1,500
           call hreada(item,varname,eof)
           if(eof) go to 125
           if(varname(3:6).eq.'corr' .or.
     1               varname(3:7).eq.'wcorr') goto 100
           call uvprobvr(inset,varname(3:10),type(ivar),length(ivar),
     *          update)
           hdvars(ivar) = varname(3:10)
           nhdvars = ivar
  100   continue
  125   continue
        call hdaccess(item,iostat)
        call uvrewind(inset)
        return
        end

c************************************************************************
        subroutine trackall(inset,except,nexcept)
c
c   Marks all variable in input data set for copying to output
c   data set. Assumes that the dataset is already open and at
c   begining.
c
        include 'maxdim.h'

        integer ivar,i,inset,item,iostat,nread,nexcept
	double precision preamble(4)
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
        character varname*11,except(20)*10
        logical eof,track

        call uvread(inset,preamble,data,flags,MAXCHAN,nread)
        call haccess(inset,item,'vartable','read',iostat)

        do 100 ivar=1,500
           call hreada(item,varname,eof)
           if(eof) go to 125
	   track = .true.
	   do 50 i=1,nexcept
	      if(varname(3:10) .eq. except(i)) track = .false.
   50	   continue
           if(track) then
 		call uvtrack(inset,varname(3:10),'c')
C	   else
C	   	write(text,'(''Autocopy not set for '',a)') 
C     *                           varname(3:10)
C	      	call output(text)
	   endif
  100   continue
  125   continue
        call hdaccess(item,iostat)
        call uvrewind(inset)
        return
        end

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
        character hdvars(500)*10,type(500)*1,avarnew*20
        real rvarnew(100)
        double precision dvarnew(100)
        integer nhdvars,length(500),yourvar,ivarnew(100)
        common /head_c/ hdvars,type,avarnew
        common /head/ nhdvars,length,yourvar,ivarnew,rvarnew
        common /head_d/ dvarnew
c
	there =.false.
	do 100 i=1,nhdvars
	   if(hdvar .eq. hdvars(i)) then
	     there = .true.
	     yourvar = i
             call uvtrack(inset,hdvar,'u')
	   endif
  100	continue
	return
	end

	subroutine addvar(hdvar,newtype,newlong)
c
c   Add new variable name, type and length to list of known
c   header variables so that it can be written by varcop later.
c
	character*10 hdvar,newtype*1
	integer newlong
c
        character hdvars(500)*10,type(500)*1,avarnew*20
        real rvarnew(100)
        double precision dvarnew(100)
        integer nhdvars,length(500),yourvar,ivarnew(100)
        common /head_c/ hdvars,type,avarnew
        common /head/ nhdvars,length,yourvar,ivarnew,rvarnew
        common /head_d/ dvarnew
c     
	nhdvars = nhdvars+1
	hdvars(nhdvars) = hdvar
	type(nhdvars)   = newtype(1:1)
	length(nhdvars) = newlong
	yourvar         = nhdvars
	return
	end

	subroutine varcop(inset,outset,there)
c
c   Change the user selected header variable to the new value if
c   it was updated in data record being read
c
	integer inset,outset,nvals
	character vtype*1,vname*10
        logical update,there
c 
        character hdvars(500)*10,type(500)*1,avarnew*20
        real rvarnew(100) 
        double precision dvarnew(100) 
        integer nhdvars,length(500),yourvar,ivarnew(100) 
        common /head_c/ hdvars,type,avarnew
        common /head/ nhdvars,length,yourvar,ivarnew,rvarnew
        common /head_d/ dvarnew
c
        vname = hdvars(yourvar)
	vtype = type(yourvar)
	nvals = length(yourvar)
	if(there) then
	   call uvprobvr(inset,vname,vtype,nvals,update)
	   if(update) then
	   if(vtype .eq. 'i') call uvputvri(outset,vname,ivarnew,nvals)
	   if(vtype .eq. 'r') call uvputvrr(outset,vname,rvarnew,nvals)
	   if(vtype .eq. 'd') call uvputvrd(outset,vname,dvarnew,nvals)
	   if(vtype .eq. 'a') call uvputvra(outset,vname,avarnew)
	   endif
	else
           if(vtype .eq. 'i') call uvputvri(outset,vname,ivarnew,nvals)
           if(vtype .eq. 'r') call uvputvrr(outset,vname,rvarnew,nvals)
           if(vtype .eq. 'd') call uvputvrd(outset,vname,dvarnew,nvals)
           if(vtype .eq. 'a') call uvputvra(outset,vname,avarnew)
	endif
	return
	end

        subroutine readval(hdvar,varval,nvals)
c
c   read ascii input of user header variable values and stick them
c   into the appropriate arrays
c
	character hdvar*(*),varval(8)*30
	character vtype*1
	integer nvals,i,varlen
c
        character hdvars(500)*10,type(500)*1,avarnew*20
        real rvarnew(100)
        double precision dvarnew(100)
        integer nhdvars,length(500),yourvar,ivarnew(100)
        common /head_c/ hdvars,type,avarnew 
        common /head/ nhdvars,length,yourvar,ivarnew,rvarnew
        common /head_d/ dvarnew
c 
	varlen   = length(yourvar)
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

