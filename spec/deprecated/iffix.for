c************************************************************************
	program iffix
	implicit none
c= iffix -- Correct for wrong IF INVERT in ATCA freq switching data.
c& dpr
c: uv analysis
c+
c
c	When using frequency-switching mode with the old ATCA
c	correlator, it was necessary to set the IF INVERT manually in
c	the .FRQ file. If this was set incorrectly, the data won't be
c	quite what you expect.
c
c       Firstly, the bandpass will be reversed ie. the channel
c       increment will be -ve to what it is normally, and the.
c       frequency of the reference pixel will be at the other 
c       end of the bandpass
c
c       Secondly, the data written to the RPFITS file will be conjugated,
c       so any image will appear rotated wrt reallity.
c
c       IFFIX conjugates the data, and labels the bandpass correctly.
c
c@ vis
c	The name of the input uv data set. No default.
c@ select
c	The normal uv selection commands. The default is to select everything.
c@ options
c	The standard options to turn off calibration:
c	  nocal    Do not apply antenna calibration.
c	  nopol    Do not apply leakage correction.
c	  nopass   Do no apply bandpass calibration.
c
c	Be warned: the calibration tables will NOT be valid after the 
c       running IFFIX
c@ out
c	The name of the output uv data-set. There is no default name.
c--
c  History:
c    11apr01 dpr  Adapted from transifx.
c
c  Bugs:
c------------------------------------------------------------------------
	include 'mirconst.h'
	character version*(*)
	parameter(version='Iffix: version 1.0 17-Apr-01')
c
	character out*64,ltype*16,uvflags*16
	integer lIn,lOut
c
c  Externals.
c
	logical uvDatOpn
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call GetOpt(uvflags)
	call uvDatInp('vis',uvflags)
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','An output must be given')
c
	call keyfin
c
c  Set that we want the raw polarisations.
c
	call uvDatSet('stokes',-5)
	call uvDatSet('stokes',-6)
	call uvDatSet('stokes',-7)
	call uvDatSet('stokes',-8)
c
c  Open the inputs and the outputs.
c
	if(.not.uvDatOpn(lIn))
     *	  call bug('f','Failed to open the input data-set')
	call uvDatGta('ltype',ltype)
	call VarInit(lIn,ltype)
c
	call uvopen(lOut,out,'new')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call hdcopy(lIn,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'IFFIX: Miriad '//version)
	call hisinput(lOut,'IFFIX')
	call hisclose(lOut)
	call VarOnit(lIn,lOut,ltype)
c
c  Do the work.
c
	call Process(lIn,lOut)
c
c  All said and done. Close up shop.
c
	call uvDatCls
	call uvclose(lOut)
	end
c************************************************************************
	subroutine GetOpt(uvflags)
c
	implicit none
	character uvflags*(*)
c
c  Determine extra processing options.
c
c  Output:
c    uvflags
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=3)
	logical present(NOPTS)
	character opts(NOPTS)*8
c
	data opts/'nocal   ','nopol   ','nopass  '/
c
	call options('options',opts,present,NOPTS)
c
c  Determine the flags to pass to the uvDat routines.
c    d - Data selection.
c    l - Linetype processing.
c    b - Input must be a single file.
c    3 - Return w in the preamble.
c    c - Apply gain table.
c    e - Apply leakage correction.
c    f - Apply bandpass correction.
c
	uvflags = 'dlb3'
	if(.not.present(1))uvflags(5:5) = 'c'
	if(.not.present(2))uvflags(6:6) = 'e'
	if(.not.present(3))uvflags(7:7) = 'f'
c
	end
c************************************************************************
	subroutine Process(lIn,lOut)
c
	implicit none
	integer lIn,lOut
c
c  Do all the real work.
c
c  Input:
c       lIn  - Input data handle
c       lOut - Output data handle
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	integer nchan
	double precision preamble(5)
	logical flags(MAXCHAN,4)
	complex data(MAXCHAN,4)
	integer i1,i2,i,j
c
c  Stuff which is common.
c
	call uvputvri(lOut,'npol',4,1)
	call wrhdi(lOut,'npol',4)
c
c  Read the first record.
c
	call uvDatRd(preamble,data(1,1),flags(1,1),MAXCHAN,nchan)
c
c  the default rest frame, if we are doing velocity recomputation
c  and a rest frame has not been given.
c
	dowhile(nchan.gt.0)
	  do i=2,4
	    call uvDatRd(preamble,data(1,i),flags(1,i),MAXCHAN,nchan)
	  enddo
c
c  Conjugate data for this baseline
c
	call basant(preamble(5),i1,i2)
c
	do i=1,nchan
	  do j=1,4
            data(i,j) = conjg(data(i,j))
          enddo
	enddo
c
c  Copy all the variables from the input to the output.
c
	  call VarFix(lIn,lOut)
c
c  All done. Loop the loop.
c
	  do i=1,4
	    call uvputvri(lOut,'pol',-4-i,1)
	    call uvwrite(lOut,preamble,data(1,i),flags(1,i),nchan)
	  enddo
	  call uvDatRd(preamble,data(1,1),flags(1,1),MAXCHAN,nchan)
	enddo
c
	end

c************************************************************************
	subroutine VarFix(tIn,tOut)
c
	implicit none
	integer tIn,tOut
c
c  Fix bandpass variables and copy across a description of the line.
c
c  Input:
c    tIn	Handle of the input uv file.
c    tOut	Handle of the output uv file.
c
c  UV variables produced are:
c      nspect,nschan,ischan,sdf,sfreq,restfreq,systemp
c
c
c------------------------------------------------------------------------
	integer vhandc,vhandu
	logical avall
	common/VarCom/vhandc,vhandu,avall
c
	integer LINE,WIDE,VELO
	parameter(LINE=1,WIDE=2,VELO=3)
	integer TYPE,N,START,WIDTH,STEP,WIN
	parameter(TYPE=1,N=2,START=3,WIDTH=4,STEP=5,WIN=6)
	double precision data(6)
c
c  Externals.
c
	logical uvvarupd
c
c  Copy across the variables that have changed. Also check the frequency
c  description variables. If they have not changed, return straight away.
c
	call uvvarcpy(vhandc,tOut)
	if(vhandu.eq.0)return
	if(.not.uvvarupd(vhandu))return
c
c  Things have changed, so update things.
c
	call uvinfo(tIn,'line',data)
c
	if(data(TYPE).eq.LINE)then
	  call VarChanFix(tIn,tOut)
	else if(data(TYPE).eq.WIDE)then
	  call bug('f','Unable to process wide-band data in VarFix')
	else if(data(TYPE).eq.VELO)then
	  call bug('f','Unable to process velocity data in VarFix')
	else
	  call bug('f','Unrecognised linetype, in VarFix')
	endif
	end
c************************************************************************
	subroutine VarChanFix(tvis,tout)
c
	implicit none
	integer tvis,tout
c
c  Calculate the frequency setup: inverts the bandpass, and 
c  sets the start freq correctly.
c
c  Copies xyphase and systemp
c
c  The variables affected by this are -
c    Channel linetype:	
c    nspect		
c    nschan
c    ischan	
c    sdf					  
c    sfreq					
c    restfreq		
c    systemp		
c
c  Inputs:
c    tvis	Handle of the input uv data file.
c    tout	Handle of the output uv data file.
c------------------------------------------------------------------------
	include 'maxdim.h'
c
	real xyphase(MAXANT*MAXWIN)
	real systemp(MAXANT*MAXWIN)
	double precision rfreq(MAXWIN),sdf(MAXWIN)
	double precision sdf0(MAXWIN)
	double precision sfreq0(MAXWIN),sfreq(MAXWIN)
	integer ischan0(MAXWIN)
	integer nschan(MAXWIN)
	integer ispect,nspect,n,i,nants
	integer nsystemp,nxyphase
	character type*1
	logical sysupd,xyupd
c
c  Get the various window-related variables from the uvdata.
c
	call uvrdvri(tVis,'nspect',nspect,1)
	if(nspect.le.0)
     *	 call bug('f','Bad value for uv variable nspect in VarChanFix')
	if(nspect.gt.MAXWIN)
     *	 call bug('f','nspect .gt. MAXWIN, in VarChanFix')
c
	call uvgetvri(tVis,'nschan',nschan,nspect)
	call uvgetvrd(tVis,'sdf',sdf,nspect)
	call uvgetvrd(tVis,'sfreq',sfreq,nspect)
	call uvgetvrd(tVis,'restfreq',rfreq,nspect)
	call uvrdvri(tVis,'nants',nants,0)
c
c  Generate the window description parameters for the output.
c
	ispect = 1
        do ispect=1,nspect
          do n=1,nschan(ispect)
            sdf0(ispect) = -1* sdf(ispect)
            sfreq0(ispect) = sfreq(ispect) + 
     *        nschan(ispect) * sdf(ispect)
          enddo
        enddo
c
c  Handle the system temperature.
c
	call uvprobvr(tVis,'systemp',type,nsystemp,sysupd)
	sysupd = type.eq.'r'.and.
     *	  nsystemp.le.MAXANT*MAXWIN.and.nsystemp.ge.1
	if(sysupd)then
	  call uvgetvrr(tVis,'systemp',systemp,nsystemp)
	endif
c
c  Handle the xyphase.
c
	call uvprobvr(tVis,'xyphase',type,nxyphase,xyupd)
	xyupd = type.eq.'r'.and.nants.gt.0.and.
     *	  nxyphase.le.MAXANT*MAXWIN.and.nxyphase.ge.1
	if(xyupd)then
	  call uvgetvrr(tVis,'xyphase',xyphase,nxyphase)
        endif
c
c  Generate the ischan array.
c
	ischan0(1) = 1
	do i=2,nspect
	  ischan0(i) = ischan0(i-1) + nschan(i-1)
	enddo
c
c  Now write out all the goodies.
c
	call uvputvri(tOut,'nspect',nspect,1)
	call uvputvri(tOut,'nschan',nschan,nspect)
	call uvputvri(tOut,'ischan',ischan0,nspect)
	call uvputvrd(tOut,'sdf',sdf0,nspect)
	call uvputvrd(tOut,'sfreq',sfreq0,nspect)
	call uvputvrd(tOut,'restfreq',rfreq,nspect)
c
	if(nsystemp.gt.0 .and. sysupd)
     *	  call uvputvrr(tOut,'systemp',systemp,nsystemp)
	if(nxyphase.gt.0 .and. xyupd)
     *	  call uvputvrr(tOut,'xyphase',xyphase,nxyphase)
c
	end

