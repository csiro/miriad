c************************************************************************
	program uvmodel
	implicit none

c= uvmodel - Add, subtract, etc, a model from a uv data set.
c& rjs
c: uv analysis
c+
c	UVMODEL is a MIRIAD task which modifies a visibility dataset by a model.
c	Allowed operations are adding, subtracting, multiplying, dividing and
c	replacing. The model is specified in the image domain, so that its
c	Fourier transform is first computed before application to the
c	visibilities. The model may be either an image (e.g., a CLEAN
c	component image) or a point source.
c
c	An example is as follows. UVMODEL could be used to remove CLEAN
c	components from a visibility data file.  The residual data base could
c	then be examined for anomalous points, which could in turn be clipped.
c	UVMODEL could then be reapplied to add the CLEAN components back into
c	the visibility data base for re-imaging. 
c@ vis
c	Input visibility data file. No default
c@ model
c	Input model cube. The default is a point source model.
c	This will generally be a deconvolved map, formed from the visibility
c	data being modified. It should be made with ``channel'' linetype.
c	The model should have units of JY/PIXEL and be weighted by the primary
c	beam. The task DEMOS can be used to extract primary beam weighted
c	models from a mosaiced image.
c@ select
c	The standard uv selection subcommands. The default is all data.
c@ options
c	This gives extra processing options. Several values can be given
c	(though many values are mutually exclusive), separated by commas.
c	Option values can be abbreviated to uniqueness.
c	Possible options are (no default):
c	  add       Form: out = vis + model
c	  subtract  Form: out = vis - model
c	  multiply  Form: out = vis * model
c	  divide    Form: out = vis / model
c	  replace   Form: out = model
c	  flag      Form: out = vis, but flag data where the difference
c	            between vis and model is greater than "sigma" sigmas.
c	  unflag    Unflag any flagged data in the output.
c
c	  mosaic    This causes UVMODEL to select only those visibilities
c	            whose observing center is within plus or minus three
c	            pixels of the model reference pixel. This is needed
c	            if there are multiple pointings or multiple sources in
c	            the input uv file. By default no observing center
c	            selection is performed.
c	  mfs       This is used if there is a single plane in the input
c	            model, which is assumed to represen t the data at all
c	            frequencies. This should also be used if the model has
c	            been derived using MFCLEAN.
c	The operations add, subtract, multiply, divide, replace and flag are
c	mutually exclusive. The operations flag and unflag are also mutually
c	exclusive.
c
c	The unflag option should be used with caution. Data in the output
c	may still be flagged, if it was not possible to calculate the
c	model.
c@ clip
c	Clip level. Pixels in the model below this level are set to zero.
c	The default is not to perform any clipping.
c@ flux
c	If MODEL is blank, then the flux (Jy) of a point source model should
c	be specified here. Also used as the default flux in the apriori
c	option. The default is 1 (assuming the model parameter is not given).
c@ offset
c	The RA and DEC offsets (arcseconds) of the point source from the
c	observing center. A point source to the north and east has positive
c	offsets. Defaults are zero.
c@ line
c	The visibility linetype to use, in the standard form, viz:
c	  type,nchan,start,width,step
c	Generally if there is an input model, this defaults to the linetype
c	parameters used to construct the map. For a point source or planet
c	model, the default is all channels. If you wish to override these
c	defaults, or if the info is not present in the header, this parameter
c	can be useful.
c@ sigma
c	For options=flag, UVMODEL flags those points in the output that
c	differ by more than "sigma" sigmas. The default is 100.
c@ out
c	Output visibility data file name. No default. The output file will
c	contain only as many channels as there are planes in the model
c	cube. The various uv variables which describe the windows are
c	adjusted accordingly.
c--
c
c  History:
c    rjs  28mar90 Original version.
c    rjs  31mar90 Changed maxchan. apriori option. Concat. with char*(*) bug.
c	 	  Improved handling of flagged data and the unflag option.
c    rjs  12apr90 Added support for "wide" and "velocity" linetypes.
c    rjs  24apr90 Checked that "out" keyword was set.
c    pjt   2may90 maxdim.h now defines maxchan
c    mchw 26may90 Corrected code in main and WindUpd for model image.
c    mchw 19nov90 Added option "imhead" to copy image header to uvvariables.
c    mchw 15dec90 Checked for variable "nants" in uvdata.
c    rjs  11mar91 Copy "chi" variable, and change "npols" to "npol".
c    rjs  25mar91 Added "line" task parameter.
c    mchw 27mar91 Better bookkeeping in WindUpd. Added more variables.
c    rjs   1jul91 Corrected calculation of wwidth in the output file.
c    mjs  04aug91 Replaced local maxants with maxdim.h MAXANT
c    rjs  29aug91 Complete rework of WindUpd.
c    nebk 29aug91 Make user specify an option.
c    rjs   1nov91 Added options=mfs and polarized. Simple polarisation
c		  processing.
c    rjs  29jan92 New call sequence to model.
c    rjs  30mar92 Added option selradec.
c    rjs  26apr92 Better model processing.
c    rjs  15jun92 Use "Var" routines to simplify copying uv files.
c    rjs  23jun92 Doc and message changes only.
c    mchw 04aug92 Corrected sign of vstart written into uvdata.
c    rjs  17aug92 Updated call sequence to var routines, plus other
c		  tidying.
c    mchw 04sep92 Fixed a missing argument in uvVarCpy.
c    rjs  15feb93 Changes to make ra,dec variables double.
c    rjs  29mar93 Fiddles with the sigma.
c    rjs  23dec93 Minimum match of linetype name.
c    rjs  31jan95 Changes to support w-axis.
c    mhw  05jan96 Add zero option to avoid flagging outer uvplane
c    rjs  30sep96 Tidy up and improved polarisation handling.
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='version 1.0 30-Sep-96')
	integer maxsels,nhead,nbuf
	parameter(maxsels=64,nhead=1,nbuf=5*maxchan+nhead)
c
	character vis*64,modl*64,out*64,oper*8,ltype*32,type*1
	character flag1*8,flag2*8
	logical unflag,updated,defline
	logical mfs,selradec,doclip
	real sels(maxsels),offset(2),flux(2),clip,sigma
	real lstart,lstep,lwidth
	integer nsize(3),nchan,nread,nvis,length,i,npol,pol
	integer tvis,tmod,tscr,tout
	double precision preamble(5)
	complex data(maxchan)
	logical flags(maxchan)
	real buffer(nbuf)
c
	logical calcrms,dounflag
	common/uvmodcom/calcrms,dounflag
c
c  Externals.
c
	external header
	logical keyprsnt
c
c  Get the input parameters.
c
	call output('Uvmodel: '//version)
	call keyini
	call keya('vis',vis,' ')
	call SelInput('select',sels,maxsels)
	call GetOpt(oper,unflag,mfs,selradec)
	call keya('model',modl,' ')
	doclip = keyprsnt('clip')
	call keyr('clip',clip,0.)
	call keyr('flux',flux(1),1.)
	flux(2) = 1
	call keyr('sigma',sigma,100.)
	call keyr('offset',offset(1),0.)
	call keyr('offset',offset(2),0.)
	call keyline(ltype,nchan,lstart,lwidth,lstep)
	call keya('out',out,' ')
	call keyfin
c
c  Check the input parameters.
c
	if(vis.eq.' ')
     *	  call bug('f','Input visibility file name must be given')
	if(modl.eq.' '.and.flux(1).eq.0.)
     *	  call bug('f','There was no input model')
	if(out.eq.' ')
     *	  call bug('f','Output visibility file name must be given')
	if(oper.eq.'flag'.and.sigma.le.0)
     *	  call bug('f','Sigma must be positive valued.')
	if(modl.eq.' '.and.abs(offset(1))+abs(offset(2)).eq.0)
     *	  call output('Model is a point source at the observing center')
c
c  Miscellaneous initialisation.
c
	call uvopen(tvis,vis,'old')
c
c  Determine the flags to the ModelIni and Model routines.
c
	flag1 = ' '
	if(selradec) flag1 = 'p'
	flag2 = ' '
	if(mfs)	     flag2(1:1) = 'm'
	if(doclip)   flag2(2:2) = 'l'
c       
	calcrms = oper.eq.'flag'
	dounflag = unflag
c
c  Determine the default linetype from the uvdata if needed.
c
	Defline = ltype.eq.' '
	if(Defline)then
	  call uvprobvr(tvis,'corr',type,length,updated)
	  if(type.eq.'j'.or.type.eq.'r')then
	    ltype = 'channel'
	  else
	    call uvprobvr(tvis,'wcorr',type,length,updated)
	    if(type.ne.'c')call bug('f',
     *		'Visibility file contains neither corr nor wcorr')
	    ltype = 'wide'
	  endif
	  nchan = 0
	  lstart = 1.
	  lstep = 1.
	  lwidth = 1.
	endif
c
c  Do the model calculation for point source, or for model image.
c
	if(Modl.eq.' ')then
	  call SelApply(tvis,sels,.true.)
	  call uvset(tvis,'data',ltype,nchan,lstart,lwidth,lstep)
	  call Model(flag2,tvis,0,offset,flux,tscr,nhead,header,
     *							nchan,nvis)
	else
	  call xyopen(tmod,Modl,'old',3,nsize)
	  if(Defline)then
	    call rdhda(tmod,'ltype',ltype,ltype)
	    call rdhdr(tmod,'lstart',lstart,lstart)
	    call rdhdr(tmod,'lwidth',lwidth,lwidth)
	    call rdhdr(tmod,'lstep',lstep,lstep)
	    if(.not.mfs)nchan = nsize(3)
	  endif
	  call uvset(tvis,'data',ltype,nchan,lstart,lwidth,lstep)
	  call ModelIni(tmod,tvis,sels,flag1)
	  call Model(flag2,tvis,tmod,offset,Clip,tscr,nhead,header,
     *							nchan,nvis)
	endif
c
c  We have computed the model. Reset the input file and ready ourselves for
c  a copy operation.
c
	call uvrewind(tvis)
	call uvset(tvis,'coord','nanosec',0,0.,0.,0.)
	call uvset(tvis,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
	call uvopen(tout,out,'new')
	call uvset(tout,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call VarInit(tvis,ltype)
	call VarOnit(tvis,tout,ltype)
c
c  Perform the copying.
c
	length = 5*nchan + nhead
	do i=1,nvis
	  call uvread(tvis,preamble,data,flags,maxchan,nread)
	  if(nread.ne.nchan) call bug('f',
     *	    'No. channels  unexpectedly changed, when rereading data')
	  call scrread(tscr,buffer,(i-1)*length,length)
	  call process(oper,buffer(1)*sigma,
     *			buffer(nhead+1),data,flags,nchan)
c
c  Copy polarisation info across.
c
	  call uvrdvri(tvis,'npol',npol,0)
	  if(npol.gt.0)then
	    call uvputvri(tout,'npol',npol,1)
	    call uvrdvri(tvis,'pol',pol,1)
	    call uvputvri(tout,'pol',pol,1)
	  endif
c
c  Copy the variables and the data.
c
	  call VarCopy(tvis,tout)
	  call uvwrite(tout,preamble,data,flags,nchan)
	enddo
	call scrclose(tscr)
c
c  Make the history of the output and close up shop.
c
	call hdcopy(tvis,tOut,'history')
	call hisOpen(tOut,'append')
	call hisWrite(tOut,'UVMODEL: Miriad UvModel '//version)
	call hisInput(tOut,'UVMODEL')
	call hisClose(tOut)
	if(Modl.ne.' ') call xyclose(tmod)
	call uvClose(tvis)
	call uvClose(tOut)
	end
c************************************************************************
	subroutine header(tvis,preamble,data,flags,nchan,
     *						accept,Out,nhead)
	implicit none
	integer tvis,nchan,nhead
	complex data(nchan)
	logical flags(nchan),accept
	real Out(nhead)
	double precision preamble(5)
c
c  This is a service routine called by the model subroutines. It is
c  called every time a visibility is read from the data file.
c
c  Input:
c    tvis	Handle of the visibility file.
c    nhead	The value of nhead
c    nchan	The number of channels.
c    preamble	Preamble returned by uvread.
c    data	A complex array of nchan elements, giving the correlation data.
c		Not used.
c    flags	The data flags. Not used.
c  Output:
c   out		The nhead values to save with the data. One value is
c		returned:
c		  out(1) -- rms error.
c   accept	This determines whether the data is accepted or discarded.
c		It is always accepted unless the baseline number looks bad.
c------------------------------------------------------------------------
	integer i
	double precision sigma2
c
	logical calcrms,dounflag
	common/uvmodcom/calcrms,dounflag
c
c  Unflag the data if necessary.
c
	if(dounflag)then
	  do i=1,nchan
	    flags(i) = .true.
	  enddo
	endif
c
c  Determine the rms error, if needed.
c
	if(calcrms)then
	  call uvinfo(tvis,'variance',sigma2)
	  out(1) = sigma2
	  accept = sigma2.gt.0
	else
	  Out(1) = 0.
	  accept = .true.
	endif
	end
c************************************************************************
	subroutine process(oper,rms,buffer,data,flags,nchan)
c
	implicit none
	integer nchan
	character oper*(*)
	real rms,buffer(5,nchan)
	complex data(nchan)
	logical flags(nchan)
c
c  Perform the desired operation on the data.
c
c  Input:
c    oper	Operation to perform.
c    rms	Estimate of the visibility rms (only if oper='flag').
c    nchan	Number of channels.
c    buffer	The visibility, model and flag data, returned by the
c		model subroutine.
c  Input/Output:
c       On input, these are the original values. On output, these are the values
c       after doing whatever operation is called for.
c    data	The data read from the visibility file.
c    flags	The flags read from the visibility file.
c------------------------------------------------------------------------
	integer i
	real temp,rms2
c
c  Do a replace operation.
c
	if(oper.eq.'replace')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    data(i) = cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Add operation.
c
	else if(oper.eq.'add')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    data(i)=data(i)+cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Subtract operation.
c
	else if(oper.eq.'subtract')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    data(i)=data(i)-cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Multiply operation.
c
	else if(oper.eq.'multiply')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    data(i)=data(i)*cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Divide operation.
c
	else if(oper.eq.'divide')then
	  do i=1,nchan
	    flags(i) = buffer(5,i).gt.0
	    if(flags(i))data(i)=data(i)/cmplx(buffer(3,i),buffer(4,i))
	  enddo
c
c  Flag operation.
c
	else if(oper.eq.'flag')then
	  rms2 = 2*rms*rms
	  do i=1,nchan
	    temp = (real (data(i))-buffer(3,i))**2 +
     *		   (aimag(data(i))-buffer(4,i))**2
	    flags(i) = flags(i).and.temp.lt.rms2
	  enddo
	else
	  call bug('f','Unrecognised operation, in Process')
	endif
	end
c************************************************************************
	subroutine GetOpt(oper,unflag,mfs,selradec)
c
	implicit none
	character oper*(*)
	logical unflag,mfs,selradec
c
c  Get the various processing options.
c
c  Output:
c    oper	One of 'add','subtract','multiply','divide','replace','flag'.
c		The default is 'add'.
c    unflag	Determine whether we are to unflag the data.
c    selradec	Input uv file contains multiple pointings or multiple
c		sources.
c------------------------------------------------------------------------
	integer i,j
	integer nopt
	parameter(nopt=9)
	character opts(nopt)*9
	logical present(nopt)
	data opts/    'add      ','divide   ','flag     ','multiply ',
     *	  'replace  ','subtract ','unflag   ','mfs      ','mosaic   '/
	call options('options',opts,present,nopt)
c
	j = 0
	do i=1,6
	  if(present(i))then
	    if(j.ne.0)call bug('f',
     *	        'Options '//opts(j)//' and '//opts(i)//
     *		' are mutually exclusive')
	    j = i
	  endif
	enddo
	if(j.eq.0) call bug ('f', 'You must specify an option')
	oper = opts(j)
c
	unflag = present(7)
	mfs = present(8)
	selradec = present(9)
	if(oper.eq.'flag'.and.unflag)
     *	  call bug('f','You cannot use options=flag,unflag')
	end
