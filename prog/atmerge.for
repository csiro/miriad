c************************************************************************
	program atmerge
	implicit none
c
c= atmerge - Merge ancillary information with an AT-observed uv file.
c& rjs
c: uv analysis
c+
c	ATMERGE merges certain ancillary information of an AT observation
c	into the corresponding Miriad uv file.
c@ vis
c	The names of the input uv data sets. No default.
c@ tsys
c	Either one of two names, being the text files containing the
c	system temperatures for the various antennas. If one value is
c	given, then the Tsys for the X and Y channels are assumed to be the
c	same. If two values are given, the first is assumed to be X, then
c	second is Y. The default is no tsys file.
c@ xyphase
c	The name of a text file containing the XY phases. The default is
c	no xyphase file. 
c@ chi
c	The name of a text file containing the RPFITS parallactic angle
c	data. This results in the variable ``errchi'', which gives
c	the difference between the Miriad value and the RPFITS value.
c	Default is no chi file.
c@ xyamp
c	The name of the text file containing the XY-amplitude data.
c	This results in the variable ``xyamp'' being written. Default is
c	no xy-amplitude file.
c@ out
c	The name of the output uv data set. No default.
c--
c  History:
c    09aug91 rjs  Original version.
c    19aug91 rjs  Added "xyamp" file.
c    21aug91 rjs  Some tidying.
c     1sep91 rjs  Added Tsys and xyphase files.
c    17dec91 rjs  ^%$^%U#%&$^ nebk changed the format of the text files.
c		  Change this to account for nebk's whims.
c     4ayg92 rjs  Eliminate mention of xyphases item.
c    12oct93 rjs  (*&^(**%( yet again (though I am rather slow to catch up).
c		  A large number of changes to try to tidy it up.
c    13oct93 nebk Correct calculation of center frequency
c    13oct93 rjs  Change check for failure of search.
c
c  Bugs:
c    * This program is a real fudge.
c------------------------------------------------------------------------
	include 'mirconst.h'
        include 'maxdim.h'
	character version*(*)
	parameter(version='ATMerge: version 1.0 12-Oct-93')
c
	integer tIn,tOut,lchi,lamp,ltsysx,ltsysy,lxyph
	integer iostat,i,nchan,sideband,nschan,nspect,pol,npol
	character out*64,vis*64,txtchi*64,ampxy*64
	character tsysx*64,tsysy*64,xyphase*64
	double precision preamble(4),t0,ttol,ftol,time,freq
	double precision sdf,sfreq
	double precision tchi,tamp,ttsysx,txyph
	double precision fchi,famp,ftsysx,fxyph
	real chi,ochi,vals(6),vals2(6)
	complex data(maxchan)
	logical flags(maxchan),dochi,doamp,dotsys1,dotsys2,doxyph
	logical twoif
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call keya('tsys',tsysx,' ')
	call keya('tsys',tsysy,' ')
	call keya('xyphase',xyphase,' ')
	call keya('chi',txtchi,' ')
	call keya('xyamp',ampxy,' ')
	call keya('out',out,' ')
	call keyfin
c
c  Check the inputs and work out what to do.
c
	if(vis.eq.' ') call bug('f','No input given')
	dotsys1 = tsysx.ne.' '
	dotsys2 = dotsys1.and.tsysy.ne.' '
	dotsys1 = dotsys1.and..not.dotsys2
	doxyph  = xyphase.ne.' '
	dochi = txtchi.ne.' '
	doamp = ampxy.ne.' '
	if(.not.dochi.and..not.doamp.and..not.doxyph.and.
     *	   .not.dotsys1.and..not.dotsys2)
     *    call bug('f','No chi or amp file given')
	if(out.eq.' ') call bug('f','Output file name is missing')
c
c  Open the relevant vis data-sets.
c
	call uvopen(tIn,vis,'old')
	call VarInit(tIn,'channel')
c
	call uvopen(tOut,out,'new')
	call VarOnit(tIn,tOut,'channel')
c
c  Copy various items to the output.
c
	call HeadCpy(tIn,tOut)
c
c  Open the text file.
c
	if(dotsys1.or.dotsys2)	call doOpen(ltsysx,tsysx)
	if(dotsys2)	      	call doOpen(ltsysy,tsysy)
	if(doxyph)		call doOpen(lxyph,xyphase)
	if(dochi)		call doOpen(lchi,txtchi)
	if(doamp)		call doOpen(lamp,ampxy)
c
c  Process the data now.
c
	call uvread(tIn,preamble,data,flags,maxchan,nchan)
	t0 = nint(preamble(3)-1) + 0.5
	tchi = -1
	tamp = -1
	ttsysx = -1
	txyph = -1
	ttol = 5.0/(24.0*3600.0)
	twoif = .false.
c
	dowhile(nchan.gt.0)
	  time = preamble(3) - t0
c
c  Check that its a single IF.
c
	  if(.not.twoif)then
	    call uvrdvri(tIn,'nspect',nspect,1)
	    twoif = nspect.gt.1
	    if(twoif)then
	      call bug('w',
     *		'Only the variables for the first IF will be copied')
	    endif
	  endif
c
c  Determine the center frequency.
c
	  call uvrdvrd(tIn,'sfreq',sfreq,0.d0)
	  call uvrdvrd(tIn,'sdf',sdf,0.d0)
	  call uvrdvri(tIn,'nschan',nschan,0)
	  freq = sfreq + sdf*(int((nschan+1)/2)-1)
	  sideband = 1
	  if(sdf.lt.0)sideband = -1
	  ftol = abs(nschan*sdf/4)
c
c  Copy all the important variables across.
c
	  call VarCopy(tIn,tOut)
	  call uvrdvri(tIn,'pol',pol,1)
	  call uvrdvri(tIn,'npol',npol,1)
	  call uvputvri(tOut,'pol',pol,1)
	  call uvputvri(tOut,'npol',npol,1)
c
c  Copy across the xyphases if needed.
c
	  if(doxyph.and.abs(time-txyph).gt.ttol)then
	    call GetVal(lxyph,time,ttol,txyph,
     *			      freq,ftol,fxyph,vals,6)
	    do i=1,6
	      vals(i) = sideband * pi/180 * vals(i) - pi
	    enddo
	    call uvputvrr(tOut,'xyphase',vals,6)
	  endif
c
c  Copy across tsys if needed.
c
	  if(dotsys1.and.abs(time-ttsysx).gt.ttol)then
	    call GetVal(ltsysx,time,ttol,ttsysx,
     *			       freq,ftol,ftsysx,vals,6)
	    call uvputvrr(tOut,'systemp',vals,6)
	  endif
	  if(dotsys2.and.abs(time-ttsysx).gt.ttol)then
	    call GetVal(ltsysx,time,ttol,ttsysx,
     *			       freq,ftol,ftsysx,vals,6)
	    call GetVal(ltsysy,time,ttol,ttsysx,
     *			       freq,ftol,ftsysx,vals2,6)
	    do i=1,6
	      vals(i) = sqrt(abs(vals(i)*vals2(i)))
	    enddo
	    call uvputvrr(tOut,'systemp',vals,6)
	  endif
c
c  Generate the "errchi" variable.
c
	  if(dochi.and.abs(time-tchi).gt.ttol)then
	    call GetVal(lchi,time,ttol,tchi,
     *			     freq,ftol,fchi,vals,6)
	    chi = pi/180 * vals(1) + pi/4
	    call uvrdvrr(tIn,'chi',ochi,chi)
	    ochi = ochi - chi
	    call uvputvrr(tOut,'errchi',ochi,1)
	  endif
c
c  Generate the "xyamp" variable.
c
	  if(doamp.and.abs(time-tamp).gt.ttol)then
	    call GetVal(lamp,time,ttol,tamp,
     *			     freq,ftol,famp,vals,6)
	    call uvputvrr(tOut,'xyamp',vals,6)
	  endif
c
c  Loop the loop.
c
	  call uvwrite(tOut,preamble,data,flags,nchan)
	  call uvread(tIn,preamble,data,flags,maxchan,nchan)
	enddo
c
c  Finish up the history.
c
        call hisopen(tOut,'append')
        call hiswrite(tOut,'ATMERGE: Miriad '//version)
	call hisinput(tOut,'ATMERGE')
        call hisclose (tOut)
c
c  Close shop.
c
	if(dotsys1.or.dotsys2)	call txtclose(ltsysx,iostat)
	if(dotsys2)		call txtclose(ltsysy,iostat)
	if(doxyph)		call txtclose(lxyph,iostat)
	if(dochi)		call txtclose(lchi,iostat)
	if(doamp)		call txtclose(lamp,iostat)
	call uvclose(tOut)
	call uvclose(tIn)
	end
c************************************************************************
	subroutine HeadCpy(tIn,tOut)
c
	implicit none
	integer tIn,tOut
c
c  Copy across items from the input to the output.
c
c  Input:
c    tIn	Handle of the input.
c    tOut	Handle of the output.
c------------------------------------------------------------------------
	integer i
	integer nitems
	parameter(nitems=13)
	character items(nitems)*8
	data items/'history ','interval ','nsols   ','ngains  ',
     *		   'nfeeds  ','gains    ','leakage ','ntau    ',
     *		   'freq0   ','freqs    ','bandpass','nspect0 ',
     *		   'nchan0  '/
c
	do i=1,nitems
	  call hdcopy(tIn,tOut,items(i))
	enddo
	end
c************************************************************************
	subroutine GetVal(lu,time,ttol,tamp,freq,ftol,famp,vals,nvals)
c
	implicit none
	integer lu,nvals
	double precision time,tamp,ttol,freq,famp,ftol
	real vals(nvals)
c
c  Get the corresponding value of chi.
c
c  Input:
c    lu		Logical unit of the text file.
c    time	Time desired.
c    freq	Frequency desired.
c    ttol	Time tolerance
c    ftol	Frequency tolerance.
c    nvals	The number of values to read.
c  Output:
c    tamp	The time corresponding to this measurement.
c    famp	The frequency corresponding to this measurement.
c    vals	The values read.
c------------------------------------------------------------------------
	logical more
	character line*64,mline*256
	double precision vals2(14)
	integer i,length,iostat
c
	if(nvals.ne.6)call bug('f','Number of values read must be 6')
	more = .true.
	dowhile(more)
	  call txtread(lu,mline,length,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error reading from text file')
	    call bugno('f',iostat)
	  endif
	  if(length.eq.0.or.mline(1:1).eq.'#')then
	    continue
	  else
	    call ddecode(mline(1:length),vals2,14)
	    tamp = vals2(1)
	    famp = 0.001 * vals2(2)
	    more = abs(time-tamp).gt.ttol.or.abs(freq-famp).gt.ftol
	    if(more.and.tamp.gt.time+ttol)then
	      write(line,'(a,f9.6)')'Last time read: ',tamp
	      call output(line)
	      write(line,'(a,f9.6)')'Looking for time: ',time
	      call output(line)
	      write(line,'(a,f9.6)')'Looking for freq: ',freq
	      call output(line)
	      call bug('f','Did not find appropriate data')
	    endif
	  endif
	enddo
c
c  Copy every second point to the output.
c
	do i=1,6
	  vals(i) = vals2(2*i+1)
	enddo
c
	end
c************************************************************************
	subroutine ddecode(line,vals,nvals)
c
	implicit none
	integer nvals
	double precision vals(nvals)
	character line*(*)
c
c  Decode a string of doubles.
c------------------------------------------------------------------------
	integer k1,k2,length,i
	character token*32
	logical ok
c
	k1 = 1
	k2 = len(line)
c
	do i=1,nvals
	  call getfield(line,k1,k2,token,length)
	  if(length.le.0)call bug('f','Line too short')
	  call atodf(token(1:length),vals(i),ok)
	  if(.not.ok)call bug('f','Error decoding line')
	enddo
c
	end
c************************************************************************
	subroutine doOpen(lu,name)
c
	implicit none
	integer lu
	character name*(*)
c
c  This opens up the text files that we want to read.
c
c  Input:
c    name	Name of the text file to be opened.
c  Output:
c    lu		Logical unit of the opened text file.
c------------------------------------------------------------------------
	character line*64
	integer iostat
c
	call txtopen(lu,name,'old',iostat)
	if(iostat.ne.0)then
	  line = 'Error opening '//name
	  call bug('w',line)
	  call bugno('w',iostat)
	endif
	end
