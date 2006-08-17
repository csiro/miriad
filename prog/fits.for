c************************************************************************
	program fits
	implicit none
c= fits - Conversion between MIRIAD and FITS image and uv formats
c& rjs
c: data transfer
c+
c	FITS is a MIRIAD task, which converts image and uv files both from
c	FITS to Miriad format, and from Miriad to FITS format. Note that
c	because there is not a perfect correspondence between all information
c	in a FITS and Miriad file, some information may be lost in the
c	conversion step. This is particularly true for uv files.
c@ in
c	Name of the input file (either a FITS or MIRIAD file name, depending
c	on OP). No default.
c@ op
c	This determines the operation to perform. Possible values are:
c	  "uvin"    Convert FITS uv file to Miriad uv file.
c	  "uvout"   Convert Miriad uv file to FITS uv file.
c	  "xyin"    Convert FITS image file to Miriad image file.
c	  "xyout"   Convert Miriad image file to FITS image file.
c	  "print"   Print out a FITS header.
c	There is no default.
c@ out
c	Name of the output file (either a MIRIAD or FITS file name, depending
c	on OP). If op=print, then this parameter is not required. Otherwise
c	there is no default.
c@ line
c	Line type of the output, when op=uvout. This is of the form:
c
c	  linetype,nchan,start,width,step
c
c	"Linetype" is either "channel", "wide" or "velocity". "Nchan" is
c	the number of channels in the output.
c@ select
c	Normal uv selection, used when op=uvout.
c@ stokes
c	Normal Stokes selection, used when op=uvout
c@ options
c	These options for op=uvout only.
c	  nocal   Do not apply the gains table to the data.
c	  nopol   Do not apply the polarization leakage table to the data
c	  nopass  Do not apply the bandpass table correctsions to the data
c@ altr
c	Velocity information, used when op=uvin. If this is given, it
c	overrides any information that may be present in the FITS header.
c	It can be two values, namely the reference channel, and the
c	velocity at the reference channel (in km/sec).
c--
c
c  Bugs:
c    * Blanked pixels are not handled, in both xyin and xyout. Magic
c      value blanking of visibilities is not supported.
c    * uvin should check that the phase and pointing center are the same.
c      xyin should generate the Miriad obsra and obsdec parameters.
c    * xyin should eliminate dummy Stokes axes in some cases.
c      Percent polarisation not correctly handled.
c    * SHould have a "ccin" option to read AIPS clean component tables and convert
c      to Miriad images.
c  Other problems:
c    In Miriad obsra/obsdec are used to store the apparent phase center (coordinates
c    in the current epoch) of the observation. The FITS standard uses OBSRA/OBSDEC
c    to store the pointing center of the observation (at the epoch given by the
c    EPOCH keyword). Miriad does not support differing phase and pointing
c    centers. FITS does not have a place for coordinates in the current epoch.
c
c  History:
c    rjs         89
c    nebk 05-may-89  Add new FITS history
c    rjs  17-may-89  Improved header variables. uvin makes the output corr
c		     format match the precision of the input. Added the
c		     ability to specify altrpix and altrval for uvin.
c    rjs  18-jul-89  Made default ctype as RA---SIN and DEC--SIN for
c		     naxis=1 or 2, for xyin.
c    rjs  18-oct-89  Changes to accomodate changes to the interface to
c		     get planet scaling/rotation.
c    mchw 24-oct-89  Converted units of bmaj, bmin in xyin and xyout
c    rjs   8-nov-89  Extracted the binary search routine.
c    rjs  13-feb-90  Replaced velocalc with calls to uvfit. Handled some
c		     FITS keywords for uvin somewhat better.
c    rjs  21-feb-90  Did not write some uv variables if the corresponding
c		     FITS keyword is blank!
c    rjs   8-mar-89  Changed call to uvgetvrr to uvrdvrr. Calculated the
c		     number of antenna on uvin.
c    rjs   2-may-90  Changed call to uvsetcor to uvset. Added version id.
c    pjt   3-may-90  maxdim.h now defines maxchan
c    rjs  10-jan-91  Check for TELESCOP keyword for uv files in hdin.
c		     More robust for ascii values containing rubbish.
c		     Improved .doc comments.
c    rjs  22-jan-91  Added op=print option.
c    rjs  31-jan-91  Better Stokes handling, in both uvin and uvout. A
c		     significant rework of these routines. Also got rid of
c		     "umsg".
c    rjs  19-feb-91  Ability for uvin to redetermine parallactic angle.
c		     Write out DATE-OBS for uvout (AIPS pretty well insists
c		     on it!).
c    rjs  25-feb-91  Changed declaration in hdout, to allow keywords to be
c		     longer that 7 characters.
c    rjs   1-mar-91  In uvin, compute parallactic angle if there are 4
c		     polarisations. Added lat/long for VLA. More robust
c		     when FITS weight is zero.
c    rjs   8-mar-91  Bug in PolCheck, determining the max polarisation
c		     to output.
c    rjs   5-apr-91  Fixed bug created by the change on 25feb91, which gave
c		     extra space between the keyword and the equals sign.
c		     Changed itoa to itoaf.
c    rjs   5-apr-91  Fixed AT lat/long bug. Added some AT-specific parameters.
c    rjs   8-apr-91  Added ORIGIN to output FITS files, at pjt's request.
c    rjs  11-apr-91  Changed "atod" into "atodf".
c    rjs  18-apr-91  Increased size of a string buffer.
c    rjs  11-jun-91  Fiddled uvin to add Hat Ck characteristics.
c		     Calculate LST.D
c    rjs  13-jun-91  Changed 45 degrees fiddle of parallactic angle
c		     for the AT.
c    rjs  17-jun-91  Changed 45 degrees fiddle of parallactic angle
c		     for the AT AGAIN!
c    nebk 06-aug-91  Implement UVDAT* routines for option 'uvout'.
c                    Adds keywords STOKES and OPTIONS
c    rjs  12-aug-91  Changed the latitude of the AT. What is the correct
c		     latitude.
c    nebk 16-aug-91  COnvert AT data with circular poln header to its 
c                    true linear from (just a labelling change).  CHange
c                    AT's latitude to geodetic value !!
c    rjs  19-sep-91  Changed Jy/K for the AT.
c    rjs   1-oct-91  Calls JulFDate, rather than internal routine. Calls
c		     obspar, rather than using its own tables.
c    rjs  17-oct-91  Copy image parameters crval,crpix,cdelt, etc in
c		     double precision.
c    rjs  17-oct-91  Changed default number of channels in uvout to all
c		     channels.
c    rjs  18-nov-91  Fiddles with the output header for uvout.
c    mchw 26-nov-91  Check pointing offsets and add to output coordinates.
c    rjs  12-dec-91  Deleted generation of obstype parameter in uvoin (this
c		     is now done inside uvio).
c    rjs  28-feb-92  Better handling of OBSRA and OBSDEC.
c    rjs  11-mar-92  Increased the length of a string buffer.
c    rjs   8-apr-92  Changes to accomodate new version of fitsio.for.
c    rjs   8-may-92  Handle AIPS AN, SU, FQ and CH tables on input.
c    rjs  20-may-92  Fixed multiple bugs in the new sections of code.
c    rjs  21-may-92  Protect against stupid AIPS Stokes values in xyin.
c    rjs  26-may-92  Write btype header parameter.
c    rjs  10-jun-92  Handle multisource file without FQ table.
c    rjs  25-jun-92  More robust at handling duplicate visibilities,
c		     in uvout (to cope with crazy data from jlim). Save
c		     more info for op=uvin.
c    rjs  28-jun-92  Handle restfreq better.
c    rjs  27-jul-92  Default restfreq is 0.
c    rjs  19-aug-92  Fixed bug in conversion of antenna positions from
c		     meters to nanosec.
c    rjs  26-aug-92  Added nopass options.
c    rjs   4-sep-92  Increase a buffer in uvin.
c    rjs   7-sep-92  Use the number of nants from AN table, where it exists.
c    rjs   9-sep-92  Corrected calculation of Miriad-style antenna coordinates
c		     and reversed antenna numbers.
c    rjs  25-sep-92  Estimate the integration time for each visibility in
c		     uvin.
c    rjs  29-sep-92  Relabel RL as LR and visa versa, in uvin and uvout.
c    rjs  24-dec-92  Fudges to get around AIPS FITLD bug.
c    rjs  10-feb-93  Protect against NaN in uvin.
c    rjs  15-feb-93  Variables ra and dec now double.
c    rjs  02-mar-93  Better calculation of frequencies. Handle multiple
c		     configurations. Use maxnax.h, use mirconst.h
c    rjs  18-mar-93  Better handling of extension tables in op=print.
c    rjs  29-mar-93  Increase jyperk.
c    rjs  30-mar-93  Fixed bug in handling of altrpix,altrval, in uvhdin,
c		     apparently introduced on 02-mar-93.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Fits: version 1.1 30-Mar-93')
	character in*64,out*64,op*8,uvdatop*12
	integer iops
	real altrpix,altrval
	logical altr,docal,dopol,dopass
c
c  Externals.
c
	logical keyprsnt
c
c  Get the input parameters.
c
	call output( version )
	call keyini
	call GetOp(op)
	in = ' '
	out = ' '
        if(op.ne.'uvout') call keya('in',in,' ')
	if(op.ne.'print') call keya('out',out,' ')
c
        if(op.eq.'uvin') then
          altr = keyprsnt('altr')
  	  call keyr('altr',altrpix,1.)
	  call keyr('altr',altrval,0.)
        endif
c
c Set UVDAT flags
c
        if(op.eq.'uvout') then
          call getopt (docal, dopol, dopass)
c
          uvdatop = 'sdlb'
          iops = 5
          if(docal) then
            iops = iops + 1
            uvdatop(iops:iops) = 'c'
          endif
          if(dopol) then
            iops = iops + 1
            uvdatop(iops:iops) = 'e'
          endif
	  if(dopass) then
	    iops = iops + 1
	    uvdatop(iops:iops) = 'f'
	  endif
          call uvdatinp('in',uvdatop(1:iops))
        endif
c
	call keyfin
	if(op.ne.'uvout'.and.in.eq.' ')
     *	  call bug('f','Input file name is missing')
	if(op.ne.'print'.and.out.eq.' ')
     *	  call bug('f','Output file name is missing')
c
c  Handle the four cases.
c
	if(op.eq.'uvin')then
	  call uvin(in,out,altr,altrpix,altrval,version)
	else if(op.eq.'uvout')then
	  call uvout(out,version)
	else if(op.eq.'xyin')then
	  call xyin(in,out,version)
	else if(op.eq.'xyout')then
	  call xyout(in,out,version)
	else if(op.eq.'print')then
	  call prthd(in)
	endif
	end
c************************************************************************
	subroutine GetOp(op)
c
	implicit none
	character op*(*)
c
c  Determine the processing option.
c
c  Output:
c    op		The processing option.
c------------------------------------------------------------------------
	integer nout
c
	integer nopts
	parameter(nopts=5)
	character opts(nopts)*5
	data opts/'uvin ','uvout','xyin ','xyout','print'/
c
	call keymatch('op',nopts,opts,1,op,nout)
	if(nout.eq.0)call bug('f','An op must be given')
	end
c************************************************************************
      subroutine getopt (docal, dopol, dopass)
c
      implicit none
      logical docal, dopol, dopass
c
c     Get a couple of the users options from the command line
c
c  Output:
c    docal   Apply gain calibration
c    dopol   Apply polarization calibration
c    dopass  Apply bandpass calibration
c
c------------------------------------------------------------------------
      integer nopt
      parameter (nopt = 3)
      character opts(nopt)*6
      logical present(nopt)
      data opts /'nocal ', 'nopol ','nopass'/
c
      call options ('options', opts, present, nopt)
      docal = .not.present(1)
      dopol = .not.present(2)
      dopass= .not.present(3)
c
      end
c************************************************************************
	subroutine prthd(in)
c
	implicit none
	character in*(*)
c
c  This prints the header of a FITS file.
c
c  Input:
c    in		Name of the FITS file.
c
c------------------------------------------------------------------------
	integer lu
	logical more
	character line*80
c
	call fitopen(lu,in,'old')
	call fitsrch(lu,'SIMPLE',more)
	if(.not.more) call bug('f','Failed to find the SIMPLE keyword')
c
	dowhile(more)
	  call fitcdio(lu,line)
	  dowhile(line(1:3).ne.'END')
	    call output(line)
	    call fitcdio(lu,line)
	  enddo
	  call output('***********************************************')
	  call output('***********************************************')
	  call ftabNxt(lu,' ',more)
	  if(more)call fitsrch(lu,'XTENSION',more)
	enddo
	call fitclose(lu)
	end
c************************************************************************
	subroutine uvin(in,out,altr,altrpix,altrval,version)
c
	implicit none
	character in*(*),out*(*)
	logical altr
	real altrpix,altrval
	character version*(*)
c
c  Read in a UV FITS file. The MIRIAD uv file does not have something
c  corresponding to the FITS weight array. Get around this by assuming
c  that the weights are a measure of the integration time in seconds.
c  This at least leads to something that is proportionally correct.
c
c  Inputs:
c    in		Name of the input uv FITS file.
c    out	Name of the output MIRIAD file.
c    altr	True if the user specified altrpix and altrval.
c    altrpix	The user given value for altrpix.
c    altrval	The user given value for altrval.
c
c------------------------------------------------------------------------
	include 'fits.h'
	integer ALTAZ,EQUATOR,MAXTIME
	parameter(ALTAZ=0,EQUATOR=1,MAXTIME=10240)
	integer lu,tno,nvis,npol,nfreq,i,j,bitpix
	integer ant1,ant2,nants,bl,nconfig,config,srcid,freqid,t1,t2,t3
	integer itemp,offset,P,Pol0,PolInc,Data,mount
	integer nspect,ischan(MAXIF),nschan(MAXIF)
	logical flags(maxchan),dochi,zerowt,dolst,multi,found,anfound
	logical conj
	complex corr(maxchan)
	real visibs(7+12*maxchan)
	double precision preamble(7),T0,Tprev
	double precision latitude,longitud,obsra,obsdec,lst
	real chi,chioff,times(MAXTIME),inttime
	integer ntimes,refbase,litime
	character telescop*32,itime*8
	character random(7)*8
c
c  Externals.
c
	double precision fuvGetT0
	integer len1,PolCvt
	character itoaf*8
c
	data random/'UU    ','VV      ','WW      ','BASELINE',
     *		    'DATE  ','SOURCE  ','FREQSEL '/
c
c  Open the input FITS and output MIRIAD files.
c
	call fuvOpen(lu,in,'old',nvis,npol,nfreq)
	call fitrdhdi(lu,'BITPIX',bitpix,16)
	if(npol*nfreq.gt.4*maxchan) call bug('f','Too many channels')
c
c  Copy parameters to the output file, and do some general fiddling.
c  If the data in the input FITS file is not 16 bit integers, set it so
c  that the output MIRIAD file will contain the correlations in real format.
c
	call uvopen(tno,out,'new')
	call hisopen(tno,'append')
	call hdin(lu,tno,version,.true.)
c
	if(abs(bitpix).gt.16) call uvset(tno,'corr','r',0,0.,0.,0.)
	call uvhdin(lu,tno,altr,altrpix,altrval,Pol0,PolInc)
	call Info(lu,multi,nspect)
c
	if(npol.gt.1)then
	  if(Pol0.ge.1.and.Pol0.le.4)then
	    call output('Data are Stokes correlations')
	  else if(Pol0.le.-1.and.Pol0.ge.-4)then
	    call output('Data are circularly polarized')
	  else if(Pol0.le.-5.and.Pol0.ge.-8)then
	    call output('Data are linearly polarized')
	  else
	    call bug('w','Unrecognised polarization type')
	  endif
	endif
c
	if(multi)then
	  Data = uvFreqID + 1
	  call fuvSetPa(lu,7,random)
	else
	  Data = uvT + 1
	  call fuvSetPa(lu,5,random)
	endif
c
c  Get various telescope characteristics.
c
	call telpar(lu,dolst,telescop,latitude,longitud,chioff,mount)
	if(dolst)then
	  call uvputvrd(tno,'latitud',latitude,1)
	  call uvputvrd(tno,'longitu',longitud,1)
	  call uvputvrr(tno,'evector',chioff,1)
	  call uvputvri(tno,'mount',mount,1)
	endif
c
	dochi = dolst.and.npol.eq.4
	if(dochi.and.mount.eq.EQUATOR)call uvputvrr(tno,'chi',chioff,1)
	dochi = dochi.and.mount.eq.ALTAZ
c
c  Do some processing peculiar to a few telescopes.
c
	if(telescop.eq.'ATCA')then
	  if(Pol0.le.-1.and.Pol0.ge.-4) then
            Pol0 = -5
            call bug('w',
     *	    'Relabelling AT circularly polarised data as linear')
          endif
	  call output('Assuming systemp=50,jyperk=13')
	  call uvputvrr(tno,'systemp',50.0,1)
	  call uvputvrr(tno,'jyperk',13.0,1)
	else if(telescop.eq.'HATCREEK')then
	  call output('Assuming systemp=200,jyperk=200')
	  call uvputvrr(tno,'systemp',200.0,1)
	  call uvputvrr(tno,'jyperk',200.,1)
	endif
c
c  Load antenna, source and frequency information.
c
	call TabLoad(lu,multi,nspect,anfound)
	call ftabLoc(lu,' ',found)
	if(.not.found)call bug('f','Error rewinding to main header')
c
c  Do the descriptions of polarisations and the "IF" axis.
c
	call uvputvri(tno,'nants',0,1)
	call uvputvri(tno,'npol',npol,1)
	call wrhdi(tno,'npol',npol)
	call uvputvri(tno,'nspect',nspect,1)
	do i=1,nspect
	  ischan(i) = (nfreq/nspect)*(i-1) + 1
	  nschan(i) =  nfreq/nspect
	enddo 
	call uvputvri(tno,'ischan',ischan,nspect)
	call uvputvri(tno,'nschan',nschan,nspect)
c
c  Initialise things to work out the integration time.
c
	call uvputvrr(tno,'inttime',10.0,1)
	ntimes = 0
c
c  Copy the data itself. The conversion of units is as follows:
c   Variable		FUVREAD		  UVWRITE
c     U			  sec		  nanosec
c     V		 	  sec		  nanosec
c     Time	  Offset Julian days	Julian days
c
	Tprev = 0
	nconfig = 0
	nants = 0
	zerowt = .false.
	srcid = -1
	freqid = -1
	config = -1
	T0 = fuvGetT0(lu)
c
	call output('Reading the correlation data')
	do i=1,nvis
	  call fuvread(lu,visibs,i,1)
	  preamble(1) = 1.0e9 * visibs(uvU)
	  preamble(2) = 1.0e9 * visibs(uvV)
	  preamble(3) = visibs(uvT) + T0
c
c  If the weight of the visibility is 0, then zero the data (as it may
c  be a NaN!).
c
	  offset = Data
	  do j=1,npol*nfreq
	    if(visibs(offset+2).eq.0)then
	      visibs(offset) = 0
	      visibs(offset+1) = 0
	    endif
	    offset = offset + 3
	  enddo
c
c  Convert baseline number to normal Miriad. Note the different conventions!
c
c  Miriad convention is that bl = 256*ant1 + ant2, where the baseline is ant2 - ant1
c  AIPS                      bl = 256*ant1 + ant2                        ant1 - ant2 !!
c
c  In both cases, ant1 < ant2.
c
	  bl = int(visibs(uvBl) + 0.01)
	  ant2 = bl/256
	  ant1 = mod(bl,256)
	  conj = ant1.gt.ant2
	  if(conj)then
	    itemp = ant1
	    ant1 = ant2
	    ant2 = itemp
	    preamble(1) = -preamble(1)
	    preamble(2) = -preamble(2)
	    offset = Data
	    do j=1,npol*nfreq
	      visibs(offset+1) = -visibs(offset+1)
	      offset = offset + 3
	    enddo
	  endif
	  bl = 256*ant1 + ant2
	  if(i.eq.1) refbase = bl
	  if(refbase.eq.bl.and.ntimes.lt.MAXTIME)then
	    ntimes = ntimes + 1
	    times(ntimes) = visibs(uvT)
	  endif
	  preamble(4) = bl
	  nants = max(nants,ant1,ant2)
c
c  Determine the current srcid, freqid and configuration. Write out any
c  needed table info.
c
	  if(multi)then
	    t1 = nint(visibs(uvSrcId))
	    t2 = nint(visibs(uvFreqId))
	  else
	    t1 = 1
	    t2 = 1
	  endif
	  t3 = nint(100*(visibs(uvBl)-bl)) + 1
	  if(srcid.ne.t1.or.freqid.ne.t2.or.config.ne.t3)then
	    srcid = t1
	    freqid = t2
	    config = t3
	    call TabWrite(tno,srcid,freqid,config,obsra,obsdec)
	    nconfig = max(config,nconfig)
	  endif
c
c  Calculate and store the parallactic angle, if needed.
c
	  if(dolst.and.(visibs(uvT).ne.Tprev.or.i.eq.1))then
	    call jullst(preamble(3),longitud,lst)
	    call uvputvrd(tno,'lst',lst,1)
	    if(dochi)then
	      call parang(obsra,obsdec,lst,latitude,chi)
	      call uvputvrr(tno,'chi',chi+chioff,1)
	    endif
	    Tprev = visibs(uvT)
	  endif
c
c  Store the correlation data.
c
	  do j=1,npol
	    P = Pol0 + (j-1)*PolInc
	    if(conj)P = PolCvt(P)
	    call uvputvri(tno,'pol',P,1)
	    call Extract(nfreq,npol,j,visibs(Data),corr,flags,zerowt)
	    call uvwrite(tno,preamble,corr,flags,nfreq)
	  enddo
	enddo
c
c  Work out the integration time now, and use override mechanism to set
c  it in the data-set.
c
	call GetInt(times,ntimes,inttime)
	call wrhdr(tno,'inttime',inttime)
	itime = itoaf(nint(inttime))
	litime = len1(itime)
	call output('The estimated integration time of a sample is '//
     *	  itime(1:litime)//' seconds')
c
c  Write out the number of antennae.
c
	if(.not.anfound)call wrhdi(tno,'nants',nants)
	call output('Number of antennae: '//itoaf(nants))
	call output('Number of antenna configurations: '//
     *		itoaf(nconfig))
c
	if(zerowt)call bug('w','Some visibilities had zero weight')
c
c  Close up shop.
c
	call fuvclose(lu)
	call hisclose(tno)
	call uvclose(tno)
	end
c************************************************************************
	integer function PolCvt(P)
c
	implicit none
	integer P
c
c  Fiddle the polarisation labelling around. This is because the baseline
c  order convention is different in Miriad and FITS -- which results in
c  polarisation RL in FITS being LR in Miriad, etc. In principle the
c  same fiddling should be done for linear feeds, but they come in
c  pre-fiddled from AIPS (because AIPS ATLOD has screwed up).
c
c  Input:
c    P		Polarisation code.
c------------------------------------------------------------------------
	integer PolRL,PolLR
	parameter(PolRL=-3,PolLR=-4)
	if(P.eq.PolRL)then
	  PolCvt = PolLR
	else if(P.eq.PolLR)then
	  PolCvt = PolRL
	else
	  PolCvt = P
	endif
	end
c************************************************************************
	subroutine GetInt(times,ntimes,inttime)
c
	implicit none
	integer ntimes
	real times(ntimes),inttime
c
c  Make a good guess at the integration time. Given a set of samples of
c  time, this sorts the times, works out time differences, sorts them
c  and uses the minimum positive value as an estimate of the integration
c  time. This needs to be at least one second.
c
c  Input:
c    times	Some samples of the sampling time (in day fractions).
c    ntimes	Number of samples of time.
c  Output:
c    inttime	Some estimate of the integration time (in seconds).
c------------------------------------------------------------------------
	integer i
	logical more
c
c  If there is only one value, assume the integration time is 10 seconds.
c
	inttime = 10
	if(ntimes.eq.1)return
c
c  Sort the times.
c
	call sortr(times,ntimes)
c
c  Work out the delta times, in seconds.
c
	do i=1,ntimes-1
	  times(i) = 24*3600*(times(i+1) - times(i))
	enddo
c
c  Sort the delta times.
c
	call sortr(times,ntimes-1)
c
c  Choose the first time which is greater than 1 sec as the integration
c  time.
c
	i = 1
	more = .true.
	dowhile(more)
	  inttime = times(i)
	  i = i + 1
	  more = inttime.lt.1.and.i.lt.ntimes
	enddo
c
c  If we did not find any valid times, set it at 10 sec.
c
	if(inttime.lt.1) inttime = 10
	end
c************************************************************************
c************************************************************************
	subroutine  TabLoad(lu,multi,nspect,anfound)
c
	implicit none
	integer lu,nspect
	logical multi,anfound
c
c  This loads table information from the FITS files. AIPS tables are
c  assumed.
c  This reads AN, FQ, CH and SU tables.
c
c  Input:
c    lu		Handle of the input FITS file.
c    multi	Expect a multisource/multi-freq file.
c    nspect	Number of IFs.
c  Output:
c    anfound	True if antenna tables were found.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'fits.h'
c
	double precision Jul2000
	parameter(Jul2000=2451544.5d0)
c
	logical badapp,badepo,more,found
	double precision Coord(3,4),T0,rfreq,jday
	character defsrc*16
	real defepoch,diff
	integer nval,i,j,t,nxyz,n
	double precision xyz(3,MAXANT),xc,yc,zc
	double precision r,sint,cost,temp
	character type*1,units*16
c
	double precision raepo(MAXSRC),decepo(MAXSRC)
	double precision raapp(MAXSRC),decapp(MAXSRC)
	double precision dra(MAXSRC),ddec(MAXSRC)
	double precision sfreq(MAXIF*MAXFREQ)
	double precision freqoff(MAXSRC*MAXIF),epoch(MAXSRC)
	double precision freqref(MAXCONFG)
	double precision restfreq(MAXSRC*MAXIF)
	double precision antpos(3*MAXANT,MAXCONFG)
	integer nants(MAXCONFG)
	
	real sdf(MAXIF*MAXFREQ)
	integer nsrc,nif,nfreq,nconfg
	integer srcid(MAXSRC),freqid(MAXFREQ)
	integer sindx(MAXSRC),findx(MAXFREQ)
	logical mosaic
	character source(MAXSRC)*20
	common/Tables/raepo,decepo,raapp,decapp,dra,ddec,sfreq,freqoff,
     *	  restfreq,antpos,freqref,epoch,sdf,
     *	  nsrc,nif,nfreq,nconfg,nants,srcid,freqid,sindx,findx,mosaic
	common/TablesC/source
c
c  Externals.
c
	double precision fuvGetT0
c
c  Get the default values for everything we are interested in.
c
	call fitrdhda(lu,'OBJECT',defsrc,' ')
	call fuvrdhd(lu,Coord)
	call fitrdhdr(lu,'EPOCH',defepoch,2000.0)
	T0 = fuvGetT0(lu)
	call fitrdhdd(lu,'RESTFREQ',rfreq,0.d0)
c
c  Load the antenna table.
c
	nconfg = 0
	call ftabLoc(lu,'AIPS AN',found)
	anfound = found
	dowhile(found)
	  call output('Reading AIPS AN table')
	  nconfg = nconfg + 1
	  call ftabInfo(lu,'STABXYZ',type,units,n,nxyz)
c
	  if(nconfg.gt.MAXCONFG)
     *	    call bug('f','Too many array configurations')
	  if(nxyz.ne.3.or.n.le.0.or.type.ne.'D')
     *	    call bug('f','Something is screwy with the antenna table')
	  if(n.gt.MAXANT)call bug('f','Too many antennae for me')
c
c  Get the coordinates of the antennae.
c
	  nants(nconfg) = n
	  call fitrdhdd(lu,'FREQ',freqref(nconfg),Coord(uvCrval,uvFreq))
	  if(abs(freqref(nconfg)-Coord(uvCrval,uvFreq)).gt.
     *	     0.01*abs(freqref(nconfg)).and.nconfg.eq.1)then
	    call bug('w',
     *	      'Header and antenna table reference frequency differ')
	    call bug('w','Using antenna table reference frequency')
	  endif
	  freqref(nconfg) = 1e-9 * freqref(nconfg)
	  call fitrdhdd(lu,'ARRAYX',xc,0.d0)
	  call fitrdhdd(lu,'ARRAYY',yc,0.d0)
	  call fitrdhdd(lu,'ARRAYZ',zc,0.d0)
	  call ftabGetd(lu,'STABXYZ',xyz)
c
c  Convert to earth-centered coordinates. The AIPS coordinates have X being in the
c  direction from the earth center to the Grennwich meridian, and Z being towards the pole.
c
	  do i=1,n
	    xyz(1,i) = xyz(1,i) + xc
	    xyz(2,i) = xyz(2,i) + yc
	    xyz(3,i) = xyz(3,i) + zc
	  enddo
c
c  Convert them to the Miriad system: y is local East, z is parallel to pole
c  Units are nanosecs.
c
	  r = sqrt(xyz(1,1)*xyz(1,1) + xyz(2,1)*xyz(2,1))
	  cost = xyz(1,1) / r
	  sint = xyz(2,1) / r
	  do i=1,n
	    temp = xyz(1,i)*cost + xyz(2,i)*sint - r
	    antpos(i,nconfg)     = (1d9/DCMKS) * temp
	    temp = -xyz(1,i)*sint + xyz(2,i)*cost
	    antpos(i+n,nconfg)   = (1d9/DCMKS) * temp
	    antpos(i+2*n,nconfg) = (1d9/DCMKS) * (xyz(3,i)-xyz(3,1))
	  enddo
c
c
c  Get the next AN table.
c
	  call ftabNxt(lu,'AIPS AN',found)
	enddo
c
	if(nconfg.eq.0)then
	  call bug('w','No antenna table found')
	  freqref(1) = 1e-9 * Coord(uvCrval,uvFreq)
	endif
	  
c
c  Load the FQ table if its present.
c
	found = .false.
	call ftabLoc(lu,'AIPS FQ',found)
	if(found)then
	  call ftabInfo(lu,'FRQSEL',type,units,nfreq,nval)
	  if(nfreq.gt.1.and..not.multi)
     *	    call bug('f','FQ table present for non-multi-source file')
	  if(nfreq.gt.MAXFREQ)call bug('f','Too many freqs')
	  if(nval.ne.1.or.type.ne.'I')
     *	    call bug('f','Something screwy with FQ table')
	  call fitrdhdi(lu,'NO_IF',nif,nspect)
	  if(nif.gt.MAXIF)call bug('f','Too many IFs')
	  call output('Reading AIPS FQ table')
	  call ftabGeti(lu,'FRQSEL',freqid)
	  if(.not.multi)freqid(1) = 1
	  call ftabGetd(lu,'IF FREQ',sfreq)
	  call ftabGetr(lu,'CH WIDTH',sdf)
	else
c
c  Load a CH table, if its present.
c
	  call ftabLoc(lu,'AIPS CH',found)
	  if(found)then
	    call ftabInfo(lu,'IF NO.',type,units,nif,nval)
	    if(nif.gt.MAXIF)call bug('f','Too many IFs')
	    if(nval.ne.1.or.type.ne.'I')
     *	      call bug('f','Something screwy with CH table')
	    call ftabGeti(lu,'IF NO.',freqid)
c
c  Check that the if table is in the standard order.
c
	    do i=1,nif
	      if(freqid(i).ne.i)
     *		call bug('f','Software bug IFNO.ne.IFNO')
	    enddo
c
	    call output('Reading AIPS CH table')
	    nfreq = 1
	    freqid(1) = 1
	    call ftabGetd(lu,'FREQUENCY OFFSET',sfreq)
	    do i=1,nif
	      sdf(i) = Coord(uvCdelt,uvFreq)
	    enddo
c
c  If neither a CH or FQ table were found, just use the info in the header.
c
	  else
	    if(multi)call bug('w',
     *			'Neither an FQ nor CH table were found')
	    nif = nspect
	    nfreq = 1
	    freqid(1) = 1
	    do i=1,nif
	      sfreq(i) = 0
	      sdf(i)   = Coord(uvCdelt,uvFreq)
	    enddo
	  endif
	endif
c
c  Check things are consistent.
c
	if(nif.ne.nspect)
     *	  call bug('f','Inconsistent number of IFs')
c
c  Convert the frequencies to the form that Miriad wants -- in GHz and
c  relative to channel 1.
c
	temp = 1 - Coord(uvCrpix,uvFreq)
	do i=1,nif*nfreq
	  sfreq(i) = 1e-9 * ( sfreq(i) + temp * sdf(i) )
	  sdf(i)   = 1e-9 * sdf(i)
	enddo
c
c  Sort the freqid table, to make it easier to find things in it.
c
	call Sortie(findx,freqid,nfreq)
c
c  Find and load the SU table. If it was not found, set everything
c  to a default.
c
	found = .false.
	if(multi)call ftabLoc(lu,'AIPS SU',found)
	if(.not.found)then
	  if(multi)call bug('w','AIPS SU table not found')
	  nsrc = 1
	  srcid(1) = 1
	  source(1) = defsrc
	  raepo(1) = Coord(uvCrval,uvRa)
	  raapp(1) = Coord(uvCrval,uvRa)
	  decepo(1) = Coord(uvCrval,uvDec)
	  decapp(1) = Coord(uvCrval,uvDec)
	  epoch(1) = defepoch
	  do i=1,nif
	    freqoff(i) = 0
	    restfreq(i) = rfreq
	  enddo
	else
	  call fitrdhdi(lu,'NO_IF',t,nif)
	  if(t.ne.nif)call bug('f','Number of IFs is inconsistent')
	  call ftabInfo(lu,'ID. NO.',type,units,nsrc,nval)
	  if(nsrc.gt.MAXSRC)call bug('f','Too many sources in SU table')
	  if(nval.ne.1.or.type.ne.'I')
     *	    call bug('f','Something screwy with SU table')
	  call output('Reading AIPS SU table')
	  call ftabGeti(lu,'ID. NO.',srcid)
	  call ftabGeta(lu,'SOURCE',source)
	  call ftabGetd(lu,'RAEPO',raepo)
	  call ftabGetd(lu,'DECEPO',decepo)
	  call ftabGetd(lu,'RAAPP',raapp)
	  call ftabGetd(lu,'DECAPP',decapp)
	  call ftabGetd(lu,'EPOCH',epoch)
	  call ftabGetd(lu,'FREQOFF',freqoff)
	  call ftabGetd(lu,'RESTFREQ',restfreq)
	endif
c
c  Check that everything looks OK.
c
	badapp = .false.
	badepo = .false.
	do i=1,nsrc
	  call lcase(source(i))
	  if(epoch(i).lt.1850.0.or.epoch(i).gt.2150.0)then
	    badepo = .true.
	    epoch(i) = defepoch
	  endif
	  diff = max( abs(raapp(i)-raepo(i)),abs(decapp(i)-decepo(i)) )
	  raepo(i)  = dpi/180 * raepo(i)
	  decepo(i) = dpi/180 * decepo(i)
	  raapp(i)  = dpi/180 * raapp(i)
	  decapp(i) = dpi/180 * decapp(i)
	  if(diff.gt.1.or.3600*diff.lt.1)then
	    badapp = .true.
	    jday = 365.25d0*(epoch(i)-2000) + Jul2000
	    call precess(jday,raepo(i),decepo(i),T0,raapp(i),decapp(i))
	  endif
	enddo
c
	if(badepo)call bug('w',
     *	  'Some epochs looked bad -- they were modified')
	if(badapp.and.multi)call bug('w',
     *	  'Some apparent RA/DECs looked bad -- they were recomputed')
c
c  Scale the source-specific frequencies and the rest frequencies.
c
	do i=1,nif*nsrc
	  freqoff(i) = 1e-9*freqoff(i)
	  restfreq(i) = 1e-9*restfreq(i)
	enddo
c
c  If there are multiple sources with the same name, assume they are
c  part of a mosaicking experiment, and change them to Miriad's dra/ddec
c  way of specifying things.
c
	mosaic = .false.
	do i=1,nsrc
	  dra(i) = 0
	  ddec(i) = 0
	  more = .true.
	  j = i-1
	  dowhile(j.gt.0.and.more)
	    if(source(i).eq.source(j))then
	      dra(i)  = (raepo(i) - raepo(j)) * cos(decepo(i))
	      ddec(i) = decepo(i) - decepo(j)
	      raepo(i) = raepo(j)
	      decepo(i) = decepo(j)
	      more = .false.
	      mosaic = mosaic.or.
     *		       (abs(dra(i))+abs(ddec(i)).gt.0.1/3600*pi/180)
	    endif
	    j = j - 1
	  enddo
	enddo
c
	call Sortie(sindx,srcid,nsrc)
c
	end
c************************************************************************
	subroutine Info(tno,multi,nif)
c
	implicit none
	logical multi
	integer nif,tno
c
c  Determine whether there are srcid/freqid and whether there is an IF axis.
c
c  Input:
c    tno	Handle of the FITS file.
c  Output:
c    multi	True if there is a src-id or freq-id random parameter.
c    nif	Number of IFs along the IF axis.
c------------------------------------------------------------------------
	integer i,naxis,count
	character num*2,type*16
c
c  Externals.
c
	character itoaf*2
c
c  See what the dimension of the IF axis is.
c
	call fitrdhdi(tno,'NAXIS',naxis,0)
	nif = 1
	do i=2,naxis
	  num = itoaf(i)
	  call fitrdhda(tno,'CTYPE'//num,type,' ')
	  if(type.eq.'IF') call fitrdhdi(tno,'NAXIS'//num,nif,1)
	enddo
c
c  Determine whether there is a source-id and freq-id random parameter.
c
	call fitrdhdi(tno,'PCOUNT',naxis,0)
	count = 0
	do i=1,naxis
	  num = itoaf(i)
	  call fitrdhda(tno,'PTYPE'//num,type,' ')
	  if(type.eq.'SOURCE'.or.type.eq.'FREQSEL')count = count + 1
	enddo
	multi = count.eq.2
	if(count.ne.2.and.count.ne.0)
     *	  call bug('f','Not a true multi-source file')
c
	end
c************************************************************************
	subroutine Sortie(indx,id,n)
c
	implicit none
	integer n,indx(n),id(n)
c
c  Sort the ids into increasing order, to make it easier to search for
c  them later.
c
c  Input:
c    n		Number of entries.
c  Input/Output:
c    id		On input, the list of ids. On output, the sorted list of ids.
c  Output:
c    indx	Index rubbish.
c------------------------------------------------------------------------
	integer j,k,tid,tindx
	logical more
c
c  The table is probably already in increasing order. Do a simple
c  sort on it, keeping track of indices, to make sure this is so.
c
	do j=1,n
	  indx(j) = j
	enddo
c
	do j=2,n
	  k = j
	  tindx = indx(j)
	  tid = id(j)
	  more = id(j-1).gt.tid
	  dowhile(more)
	    id(k) = id(k-1)
	    indx(k) = indx(k-1)
	    k = k - 1
	    more = .false.
	    if(k.gt.1) more = id(k-1).gt.tid
	  enddo
	  indx(k) = tindx
	  id(k) = tid
	enddo
c
	end
c************************************************************************
	subroutine TabWrite(tno,sid,fid,config,obsra,obsdec)
c
	implicit none
	integer tno,sid,fid,config
	double precision obsra,obsdec
c
c  This writes out information about the current source and frequency
c  setup.
c
c  Input:
c    tno	Handle of the output Miriad uv file.
c    sid	FITS source id number.
c    fid	FITS frequency id number.
c    config	FITS configuration number.
c  Output:
c    obsra,obsdec Observation RA and DEC.
c------------------------------------------------------------------------
	include 'fits.h'
c
	double precision raepo(MAXSRC),decepo(MAXSRC)
	double precision raapp(MAXSRC),decapp(MAXSRC)
	double precision dra(MAXSRC),ddec(MAXSRC)
	double precision sfreq(MAXIF*MAXFREQ)
	double precision freqoff(MAXSRC*MAXIF),epoch(MAXSRC)
	double precision freqref(MAXCONFG)
	double precision restfreq(MAXSRC*MAXIF)
	double precision antpos(3*MAXANT,MAXCONFG)
	integer nants(MAXCONFG)
	
	real sdf(MAXIF*MAXFREQ)
	integer nsrc,nif,nfreq,nconfg
	integer srcid(MAXSRC),freqid(MAXFREQ)
	integer sindx(MAXSRC),findx(MAXFREQ)
	logical mosaic
	character source(MAXSRC)*20
	common/Tables/raepo,decepo,raapp,decapp,dra,ddec,sfreq,freqoff,
     *	  restfreq,antpos,freqref,epoch,sdf,
     *	  nsrc,nif,nfreq,nconfg,nants,srcid,freqid,sindx,findx,mosaic
	common/TablesC/source
c
	integer i,j,k,l
	double precision sfreq0(MAXIF),sdf0(MAXIF),rfreq0(MAXIF)
c
c  Externals.
c
	integer bsrchi
c
c  Write out the antenna table.
c
	if(config.le.nconfg)then
	  call uvputvri(tno,'nants',nants(config),1)
	  call uvputvrd(tno,'antpos',antpos(1,config),3*nants(config))
	endif
c
c  Locate this source and frequency in the appropriate tables.
c
	i = bsrchi(sid,srcid,nsrc)
	j = bsrchi(fid,freqid,nfreq)
c
c  Write the info relevant to this.
c
	if(i.eq.0.or.j.eq.0)then
	  call bug('f','Invalid source or freq id')
	else
	  i = sindx(i)
	  j = findx(j)
	  call uvputvrd(tno,'obsra',raapp(i),1)
	  call uvputvrd(tno,'obsdec',decapp(i),1)
	  obsra = raapp(i)
	  obsdec = decapp(i)
	  call uvputvrd(tno,'ra',raepo(i),1)
	  call uvputvrd(tno,'dec',decepo(i),1)
	  if(mosaic)then
	    call uvputvrr(tno,'dra',real(dra(i)),1)
	    call uvputvrr(tno,'ddec',real(ddec(i)),1)
	  endif
	  call uvputvrr(tno,'epoch',real(epoch(i)),1)
	  call uvputvra(tno,'source',source(i))
	  i = (i-1)*nif + 1
	  j = (j-1)*nif + 1
	  l = config
	  if(l.gt.nconfg) l = 1
	  do k=1,nif
	    rfreq0(k) = restfreq(i)
	    sfreq0(k) = sfreq(j) + freqoff(i) + freqref(l)
	    sdf0(k) =   sdf(j)
	    i = i + 1
	    j = j + 1
	  enddo
	  call uvputvrd(tno,'sfreq',sfreq0,nif)
	  call uvputvrd(tno,'freq',sfreq0,1)
	  call uvputvrd(tno,'sdf',sdf0,nif)
	  call uvputvrd(tno,'restfreq',rfreq0,nif)
	endif
c
	end
c************************************************************************
	subroutine Extract(nfreq,npol,pol,visibs,corr,flags,zerowt)
c
	implicit none
	integer nfreq,npol,pol
	real visibs(3*nfreq*npol)
	complex corr(nfreq)
	logical flags(nfreq),zerowt
c
c  Extract the input visibilities into an output buffer. Only one polarisation
c  is extracted at a time.
c
c  Input:
c    nfreq	Number of frequency channels in the input.
c    npol	Number of polarisations in the input.
c    pol	Polarisation number to extract.
c    visibs	The input data.
c  Input/Output:
c    zerowt	True if a zero weight was found somewhere.
c  Output:
c    corr	The correlation data of the desired polarisation.
c    flags	Logical array, giving true if the correlation is good.
c------------------------------------------------------------------------
	integer i,j
c
	j = 3*(pol-1) + 1
	do i=1,nfreq
	  corr(i) = cmplx(visibs(j),visibs(j+1))
	  flags(i) = visibs(j+2).gt.0
	  zerowt = zerowt.or.visibs(j+2).eq.0
	  j = j + 3*npol
	enddo
	end
c************************************************************************
	subroutine uvhdin(lu,tno,altr,altrpix,altrval,Pol0,PolInc)
c
	implicit none
	integer lu,tno
	logical altr
	real altrpix,altrval
	integer Pol0,PolInc
c
c  Get the header parameters from a UVFITS file, and write them out to
c  the MIRIAD file.
c
c  Input:
c    lu		Handle of the input FITS file.
c    tno	Handle of the output MIRIAD file.
c    altr	True if the user specified altrpix and altrval.
c    altrpix	The user given value for altrpix.
c    altrval	The user given value for altrval.
c  Output:
c    Pol0	Base polarisation code.
c    PolInc	Increment between polarisations.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'fits.h'
	integer veltype
	double precision f0,f,altval,altdelt,Coord(3,4)
	real veldop,altpix
	character string*32,vtype(6)*8
c
	data vtype/'FELO-LSR','FELO-HEL','FELO-OBS',
     *		   'VELO-LSR','VELO-HEL','VELO-OBS'/
c
c  Determine various of the important parameters.
c
	call fuvRdHd(lu,Coord)
c
	Pol0 = nint( Coord(uvCrval,uvStokes) + 
     *		(1-Coord(uvCrpix,uvStokes))*Coord(uvCdelt,uvStokes) )
	PolInc = nint(Coord(uvCdelt,uvStokes))
c
	call fitrdhdd(lu,'RESTFREQ',f,Coord(uvCrval,uvFreq))
	f0 = Coord(uvCrval,uvFreq) +
     *		(1-Coord(uvCrpix,uvFreq))*Coord(uvCdelt,uvFreq)

	if(altr)then
	  altval = 1e3 * altrval
	  altpix = altrpix
	else
	  call fitrdhdd(lu,'ALTRVAL',altval,cmks*(1-f0)/f)
	  call fitrdhdr(lu,'ALTRPIX',altpix,1.0)
	endif
	altdelt = -cmks*Coord(uvCdelt,uvFreq)/f
	veldop = cmks*(1-f0/f) - (altval + altdelt*(1-altpix))
	veldop = 1.0e-3 * veldop
	call uvputvrr(tno,'veldop',veldop,1)
	call uvputvrr(tno,'vsource',0.,1)
c
	call fitrdhdi(lu,'VELREF',veltype,4)
	if(veltype.gt.256) veltype = veltype - 256 + 3
	if(veltype.lt.1.and.veltype.gt.6) veltype = 4
	call uvputvra(tno,'veltype',vtype(veltype))
c
	call fitrdhda(lu,'TELESCOP',string,' ')
	if(string.ne.' ')call uvputvra(tno,'telescop',string)
	call fitrdhda(lu,'OBSERVER',string,' ')
	if(string.ne.' ')call uvputvra(tno,'observer',string)
c
	end
c************************************************************************
	subroutine telpar(lu,ok,telescop,latitude,longitud,chioff,mount)
c
	implicit none
	integer lu,mount
	character telescop*(*)
	double precision latitude,longitud
	real chioff
	logical ok
c
c  Determine the latitude and longitude of the observatory.  First we
c  look for FITS keywords which might give this. If these are missing,
c  get the observatory name, and see if we know the lat,long of this
c  observatory.
c
c  Input:
c    lu		Handle of the FITS file.
c  Output:
c    ok		Set to true if lat/long successfully found.
c    telescop	Telescope name.
c    latitude)	Observatory latitude and longitude, in radians.
c    longitud)
c    chioff	The position angle of the X feed with respect to the
c		local vertical.
c    mount	Antenna mount type.
c------------------------------------------------------------------------
	double precision dtemp
c
	call fitrdhda(lu,'TELESCOP',telescop,' ')
	if(telescop.eq.' ')
     *	  call fitrdhda(lu,'INSTRUME',telescop,' ')
c
c  Get the info we need from the obspar routine.
c
	ok = telescop.ne.' '
	if(ok)call obspar(telescop,'latitude',latitude,ok)
	if(ok)call obspar(telescop,'longitude',longitud,ok)
	if(ok)call obspar(telescop,'evector',dtemp,ok)
	if(ok)chioff = dtemp
	if(ok)call obspar(telescop,'mount',dtemp,ok)
	if(ok)mount = dtemp
	if(.not.ok)
     *	  call bug('w','Unable to determine telescope lat/long')
	end
c************************************************************************
	subroutine uvout(out,version)
c
	implicit none
	character out*(*),version*(*)
c
c  Write out a UV FITS file.
c
c  Inputs:
c    out	Name of the output uv FITS file.
c    version	Version of this program.
c------------------------------------------------------------------------
	include 'fits.h'
	include 'mirconst.h'
	complex Data(maxchan)
	logical Flags(maxchan)
	real OutData(uvRandom+1+3*maxchan)
c
	integer i,i0
	integer tIn,tScr,tOut
	integer nread,nvis,nVisRef,offset,length,velref,nchan
	integer bl,ant1,ant2,itemp
	real inttime,epoch,dra,ddec,dra0,ddec0
	double precision ra,dec,restfreq,f0,df,preamble(4),Coord(3,4)
	double precision repsi,fepsi,T0
	character string*64,ltype*32,veltype*32,vtype(6)*8
	character source*32,observer*32,telescop*32
	integer pols(PolMin:PolMax),P,badpol,npol,Pol0,PolInc
	logical dowide
c
	integer NPARM
	parameter(NPARM=5)
	character parms(NPARM)*8
c
c  Externals.
c
	character itoaf*8
        logical uvdatopn
	integer PolCvt
c
	data parms/'UU      ','VV      ','WW      ',
     *		   'BASELINE','DATE    '/
	data vtype/'FELO-LSR','FELO-HEL','FELO-OBS',
     *		   'VELO-LSR','VELO-HEL','VELO-OBS'/
c
c  Initialise the array to count the sorts of polarisations that we have.
c
	do i=PolMin,PolMax
	  pols(i) = 0
	enddo
c
c  Open up and initialise the input file. Also get the scratch file.
c
        if (.not.uvdatopn(tin))call bug('f','Error opening input file')
	call scropen(tScr)
c
c  Read through the input, determining the min and max values of all the
c  parameters that we need.
c
	call uvdatrd(preamble,data,flags,maxchan,nread)
	if(nread.eq.0)call bug('f','No data to write out!')
        nchan = nread
c
	nvis = 0
	length = uvRandom + 1 + 3*nchan
	offset = 0
c
c  Get things which define the coordinate system.
c
	call uvrdvrr(tIn,'epoch',epoch,1950.)
	call uvrdvrd(tIn,'ra',ra,0.)
	call uvrdvrd(tIn,'dec',dec,0.)
	call uvrdvrr(tIn,'dra',dra,0.)
	call uvrdvrr(tIn,'ddec',ddec,0.)
	dra0 = dra
	ddec0 = ddec
c
	call uvfit2(tIn,'frequency',nread,df,f0,fepsi)
	if(fepsi.gt.0.1*abs(df))call bug('w',
     *	    'Channel frequencies deviated by > 10% from linearity')
	if(nread.eq.1)call uvfit1(tIn,'bandwidth',nread,df,fepsi)
	f0 = 1e9 * f0
	df = 1e9 * df
c
	call uvdatgta('ltype',ltype)
	dowide = ltype.eq.'wide'.or.nread.eq.1
	if(dowide)then
	  velref = 0
	  restfreq = 0
	else
	  call uvrdvra(tIn,'veltype',veltype,'VELO-LSR')
	  velref = 0
	  do i=1,6
	    if(veltype.eq.vtype(i)) velref = i
	  enddo
	  if(velref.gt.3) velref = velref - 3 + 256
c
	  call uvfit1(tIn,'restfreq',nread,restfreq,repsi)
	  if(repsi.gt.0.001*restfreq) call bug('w',
     *	    'Rest frequencies varied between channels by > 0.1%')
	  restfreq = 1e9 * restfreq
	endif
c
c  Get other book keeping.
c
	call uvrdvra(tIn,'source',source,out)
	call uvrdvra(tIn,'telescop',telescop,' ')
	call uvrdvra(tIn,'observer',observer,' ')
c
c  Set the reference date.
c
	T0 = preamble(3)
	T0 = int(T0 - 0.5d0) + 0.5d0
c
c  Read the data. Check that we are dealing with a single pointing.
c  If the polarisation ocde is OK, do some conversions, and write to
c  a scratch file.
c
	badpol = 0
	dowhile(nread.eq.nchan)
c
	  call uvrdvrr(tIn,'dra',dra,0.)
	  call uvrdvrr(tIn,'ddec',ddec,0.)
	  if(dra.ne.dra0.or.ddec.ne.ddec0)then
	    call output('FITS will handle only one pointing center')
	    call bug('f','Pointing offsets changed in Input file')
	  endif
c
	  call uvdatgti('pol',P)
	  if(P.ge.PolMin.and.P.le.PolMax.and.P.ne.0)then
	    P = PolCvt(P)
	    pols(P) = pols(P) + 1
	    nvis = nvis + 1
	    call uvrdvrr(tIn,'inttime',inttime,1.)
c
c  Convert baseline number to normal AIPS form. Note the different conventions!
c
c  Miriad convention is that bl = 256*ant1 + ant2, where the baseline is ant2 - ant1
c  AIPS                      bl = 256*ant1 + ant2                        ant1 - ant2 !!
c
c  In both cases ant1 is normally less than ant2.
c
	    bl = preamble(4)
	    ant2 = bl / 256
	    ant1 = bl - 256*ant2
	    if(ant1.gt.ant2)then
	      preamble(1) = -preamble(1)
	      preamble(2) = -preamble(2)
	      do i=1,nchan
		data(i) = conjg(data(i))
	      enddo
	      itemp = ant1
	      ant1 = ant2
	      ant2 = itemp
	    endif
c
	    OutData(uvU+1) = 1e-9 * preamble(1)
	    OutData(uvV+1) = 1e-9 * preamble(2)
	    OutData(uvW+1) = 0.
	    OutData(uvT+1) = preamble(3) - T0
	    OutData(uvBl+1) = 256*ant1 + ant2
	    OutData(1) = P
	    i0 = uvData
	    do i=1,nchan
	      OutData(i0+1) = real(Data(i))
	      OutData(i0+2) = aimag(Data(i))
	      if(flags(i))then
	        OutData(i0+3) = inttime
	      else
	        OutData(i0+3) = -inttime
	      endif
	      i0 = i0 + 3
	    enddo
	    call scrwrite(tScr,OutData,offset,length)
	    offset = offset + length
	  else
	    badpol = badpol + 1
	  endif
	  call uvdatrd(preamble,data,flags,maxchan,nread)
	enddo
c
c  Summarise what we read.
c
	if(badpol.gt.0) call bug('w',
     *	  'Visibilities with bad pol codes: '//itoaf(badpol))
	if(nread.gt.0) call bug('f','Bad number of channels')
	if(nvis.le.0) call bug('f','No visibilities found')	
c
c  Determine the polarisations that we are going to output.
c
	call PolCheck(pols,npol,Pol0,PolInc)
	nVisRef = pols(Pol0)
c
c  Create the output FITS file, and write its header.
c
	Coord(uvCrval,uvStokes) = Pol0
	Coord(uvCdelt,uvStokes) = PolInc
	Coord(uvCrpix,uvStokes) = 1
	Coord(uvCrval,uvFreq) = f0
	Coord(uvCdelt,uvFreq) = df
	Coord(uvCrpix,uvFreq) = 1
	Coord(uvCrval,uvRa) = 180./pi * (ra+dra/cos(dec))
	Coord(uvCdelt,uvRa) = 1
	Coord(uvCrpix,uvRa) = 1
	Coord(uvCrval,uvDec) = 180./pi * (dec+ddec)
	Coord(uvCdelt,uvDec) = 1
	Coord(uvCrpix,uvDec) = 1
c
c  Open the FITS file and write out some info. NOTE that a bug in AIPS
c  FITLD requires that the OBJECT keyword be written out as early
c  as possible.
c
	call fuvopen(tOut,out,'new',nVisRef,npol,nchan)
	if(source.ne.' ')call fitwrhda(tOut,'OBJECT',source)
	call fuvSetT0(tOut,T0)
	call fuvSetPa(tOut,NPARM,parms)
	call fuvWrhd(tOut,Coord)
	call fitwrhdd(tOut,'OBSRA',Coord(uvCrval,uvRa))
	call fitwrhdd(tOut,'OBSDEC',Coord(uvCrval,uvDec))
c
c  Copy various things to the output.
c
	call fitwrhdr(tOut,'EPOCH',epoch)
	if(restfreq.ne.0)call fitwrhdd(tOut,'RESTFREQ',restfreq)
	if(telescop.ne.' ')then
	  call fitwrhda(tOut,'TELESCOP',telescop)
	  call fitwrhda(tOut,'INSTRUME',telescop)
	endif
	if(observer.ne.' ')call fitwrhda(tOut,'OBSERVER',observer)
	string = 'Miriad '//version
	call fitwrhda(tOut,'ORIGIN',string)
	if(velref.ne.0) call fitwrhdi(tOut,'VELREF',velref)
c
c  Copy the history.
c
	call CopyHist(tIn,tOut)
c
c  We now have all the data we want in a scratch file. Copy this
c  data to the output FITS file.
c
	call uvoutWr(tScr,tOut,nvis,nVisRef,npol,nchan,Pol0,PolInc)
c
c  Everything is done. Close up shop.
c
	call uvdatcls
	call scrclose(tScr)
	call fuvclose(tOut)
c
	end
c************************************************************************
	subroutine uvoutWr(tScr,tOut,nvis,nVisRef,npol,nchan,Pol0,
     *								PolInc)
c
	integer tScr,tOut,nvis,nVisRef,npol,nchan,Pol0,PolInc
c
c  This reads visibilities back from the scratch file, and forms a
c  visibility record the way FITS likes it. That is the visibility
c  consists of 3*npol*nfreq bits of data. This is unlike Miriad in that
c  the polarisation axis is in with the frequency axis.
c
c  Inputs:
c    tScr	Handle of the scratch file.
c    tOut	Handle of the output FITS file.
c    nvis	Number of visibilities in the scratch file.
c    nVisRef	Number of visibilities that will be written to the output
c		FITS file.
c    npol	The dimension of the Stokes axis in the output file.
c    nchan	The number of frequency channels in the output file.
c    Pol0	The code of the first polarisation.
c    PolInc	The increment between polarisations.
c------------------------------------------------------------------------
c  Records are copied or discarded according to whether the reference
c  polarisation is present or not. If not, the record is discarded. Currently
c  the reference polarisation is always the first polarisation, but this may
c  change in the future.
c
	integer RefPol
	parameter(RefPol=1)
	include 'fits.h'
	integer pols(PolMin:PolMax),discard(PolMin:PolMax),pnt(maxPol)
	integer ncopy,totcopy,l,l2,InPnt,OutPnt,Bl,P,iP,i,j,jd,k,length
	logical copied(maxPol)
	character num*8,num2*8
	real In(uvRandom+1+3*maxchan),Out(uvRandom+3*maxPol*maxchan)
	real Time,wt
c
c  Externals.
c
	character itoaf*8,PolsC2P*2
	integer len1
c
c  Check! These should have been done before, somewhere or other.
c
	if(nchan.gt.maxchan.or.npol.gt.maxpol)
     *	  call bug('f','Too many channels, or too many polarisations')
c
c  Form a table to help sort out which polarisations to keep, and where
c  to put them in the output record. Also initialise an array of counters
c  to determine the number of visibilities that are being discarded.
c
	do i=PolMin,PolMax
	  pols(i) = 0
	  discard(i) = 0
	enddo
c
c  Initialise some tables to help me keep track of things. They allow
c  determination of the polarisation code, from the number 1..nPol, and
c  visa versa.
c
	i = Pol0
	do j=1,npol
	  copied(j) = .false.
	  pnt(j) = i
	  pols(i) = j
	  i = i + PolInc
	enddo
c
	length = uvRandom + 1 + 3*nchan
	ncopy = 0
	totcopy = 0
	Time = 0
	Bl = 0
	jd = 0
	wt = 0
c
	do j=1,nvis
	  call scrread(tScr,In,(j-1)*length,length)
	  P = nint(In(1))
	  iP = pols(P)
c
c  Handle the case of a polarisation that we are not handling, or the
c  case where we have only one polarisation to handle.
c
	  if(iP.eq.0)then
	    discard(P) = discard(P) + 1
	  else if(npol.eq.1)then
	    jd = jd + 1
	    totcopy = totcopy + 1
	    call fuvwrite(tOut,in(2),jd,1)
	  else
c
c  We have a good polarisation, and we are handling more than 1 polarisations.
c  If it does not match with the pervious record, then we have to rid
c  ourselves of the previous record.
c
c  If the "reference" polarisation is not present, discard the previous record.
c  If the "reference" polarisation is present, blnak out any channels which
c  are missing data, and finally write out the previous good record.
c
	    if(nint(In(uvBl+1)).ne.Bl.or.In(uvT+1).ne.Time.or.
     *	       (copied(RefPol).and.copied(iP)))then
	      if(.not.copied(RefPol))then
	        do i=1,npol
	          if(copied(i)) discard(pnt(i)) = discard(pnt(i)) + 1
	        enddo
	      else
		if(ncopy.lt.npol)call ZeroOut(copied,npol,nchan,out,wt)
		jd = jd + 1
		totcopy = totcopy + ncopy
		call fuvwrite(tOut,Out,jd,1)
	      endif
c
c  We have a new record. Ready ourselves for it.
c
	      do i=1,npol
		copied(i) = .false.
	      enddo
	      ncopy = 0
	      Out(uvU) = In(uvU+1)
	      Out(uvV) = In(uvV+1)
	      Out(uvW) = In(uvW+1)
	      Out(uvT) = In(uvT+1)
	      Out(uvBl) = In(uvBl+1)
	      Bl = nint(In(uvBl+1))
	      Time = In(uvT+1)
	      Wt = abs(In(uvData+3))
	    endif
c
c  We have to add visibilities to the currently existing record.
c
	    InPnt = uvData
	    OutPnt = uvRandom + 3*(iP-1)
	    do k=1,nchan
	      out(OutPnt+1) = in(InPnt+1)
	      out(OutPnt+2) = in(InPnt+2)
	      out(OutPnt+3) = in(InPnt+3)
	      InPnt = InPnt + 3
	      OutPnt = OutPnt + 3*npol
	    enddo
	    ncopy = ncopy + 1
	    copied(iP) = .true.
	  endif
	enddo
c
c  We have more or less finished. One record could still be buffered up.
c  Either discard it or write it, depending whether the reference
c  polarisation is present.
c
	  if(.not.copied(RefPol))then
	    do i=1,npol
	      if(copied(i)) discard(pnt(i)) = discard(pnt(i)) + 1
	    enddo
	  else
	    if(ncopy.lt.npol)call ZeroOut(copied,npol,nchan,out,wt)
	    jd = jd + 1
	    totcopy = totcopy + ncopy
	    call fuvwrite(tOut,Out,jd,1)
	  endif
c
c  Generate messages about the number of visibilities copied and not copied.
c
	if(jd.ne.nVisRef)
     *	  call bug('f','Internal inconsistency, in uvout(write)')
	num = itoaf(totcopy)
	l = len1(num)
	num2 = itoaf(nVisRef)
	l2 = len1(num2)
	call output(num(1:l)//' visibilities copied to '//num2(1:l2)//
     *		' output records.')
c
	do i=PolMin,PolMax
	  if(discard(i).ne.0) then
	    num = itoaf(discard(i))
	    l = len1(num)
	    call bug('w','Discarded '//num(1:l)//
     *		' visibilities of type '//PolsC2P(i))
	  endif
	enddo
c
	end
c************************************************************************
	subroutine ZeroOut(copied,npol,nchan,out,wt)
c
	implicit none
	integer npol,nchan
	logical copied(npol)
	real out(5+3*npol*nchan),wt
c
c  This blanks out any polarisations that have not truely been copied.
c  It does this by setting the correlation value to zero, and the weight
c  to indicate bad data.
c
c  Inputs:
c    copied	Logical array indicating if a particular polarisation has
c		been copied.
c    wt		The weight to associate with the blanked out data.
c    npol	Number of polarisations.
c    nchan	Number of channels.
c  Input/Output:
c    out	The visibility record (FITS style). Missing polarisations
c		are blanked out.
c------------------------------------------------------------------------
	integer i,OutPnt,k
c
	do i=1,npol
	  if(.not.copied(i))then
	    OutPnt = 5 + 3*(i-1)
	    do k=1,nchan
	      out(OutPnt+1) = 0
	      out(OutPnt+2) = 0
	      out(OutPnt+3) = -wt
	      OutPnt = OutPnt + 3*npol
	    enddo
	  endif
	enddo
	end
c************************************************************************
	subroutine PolCheck(pols,npol,Pol0,PolInc)
c
	implicit none
	include 'fits.h'
	integer pols(PolMin:PolMax),npol,Pol0,PolInc
c
c  The "pols" array counts the polarisations that were found in the input
c  data. Because FITS can only handle a regular Stokes axis, and because
c  Miriad allows an arbitrary Stokes "dimension", we have to form a regular
c  axis. The rule is to find the commonest polarisation, and then to copy
c  "adjacent" polarisations that are at least 70% as common.
c  When a required polarisation is missing, we give a dummy (but flagged)
c  value.
c
c  Input:
c    pols	The counts of the polarisations encountered in the
c		input file.
c  Output:
c    npol	Number of polarisations to form in the output file.
c    Pol0	The code for the first polarisation.
c    PolInc	The code increment between different polarisations. This
c		can be either +1 or -1, having the same sign as Pol0.
c------------------------------------------------------------------------
	integer imax,i,PolMn,PolMx,thresh,p,length
	logical more
	character line*32
c
c  Externals.
c
	character PolsC2P*2
	integer len1
c
c  Determine which polarisation type is the most common.
c
	imax = PolMin
	do i=PolMin,PolMax
	  if(pols(i).gt.pols(imax).or.
     *	    (pols(i).eq.pols(imax).and.abs(i).lt.abs(imax)) ) imax = i
	enddo
c
c  Around the commonest type, find those types that are at least 70% as
c  common. This spans the polarisations that we will choose.
c
	thresh = nint(0.7*pols(imax))
	PolMn = imax
	more = .true.
	dowhile(PolMn-1.ge.PolMin.and.more)
	  more = pols(PolMn-1).ge.thresh.and.PolMn-1.ne.0
	  if(more) PolMn = PolMn - 1
	enddo
c
	PolMx = imax
	more = .true.
	dowhile(PolMx+1.le.PolMax.and.more)
	  more = pols(PolMx+1).ge.thresh.and.PolMx+1.ne.0
	  if(more) PolMx = PolMx + 1
	enddo
c
c  Fill in the parameters to describe the choosen polarisation.
c
	npol = PolMx - PolMn + 1
	if(npol.gt.maxPol)
     *	  call bug('f','Too many polarisations for me to handle')
	if(PolMx.gt.0)then
	  Pol0 = PolMn
	  PolInc = 1
	else
	  Pol0 = PolMx
	  PolInc = -1
	endif
c
c  Give messages about the polarisations that we are copying
c  and those that we are discarding.
c
	length = 0
	p = Pol0
	do i=1,npol
	  line(length+1:length+2) = PolsC2P(p)
	  length = len1(line(1:length+2)) + 1
	  line(length:length) = ','
	  p = p + PolInc
	enddo
	line(length:length) = '.'
	call output('Polarisations copied: '//line(1:length))
c
	end
c************************************************************************
	subroutine xyin(in,out,version)
c
	implicit none
	character in*(*),out*(*),version*(*)
c
c  Read in an image FITS file.
c
c  Inputs:
c    in		Name of the input image FITS file.
c    out	Name of the output MIRIAD file.
c
c  Internal Variables:
c    lu		Handle of the FITS file.
c    tno	Handle of the MIRIAD file.
c    array	Array to use as buffer.
c    maxdim	Size of array.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	real array(maxdim)
	integer nsize(MAXNAX),axes(MAXNAX),naxis
	integer lu,tno,j
c
c  Externals.
c
	logical Inc3More
c
c  Open the input FITS and output MIRIAD files.
c
	call fxyopen(lu,in,'old',MAXNAX,nsize)
	if(nsize(1).gt.maxdim)
     *	  call bug('f','Image too big to handle')
	call fitrdhdi(lu,'NAXIS',naxis,0)
	if(naxis.le.0)call bug('f','Weird bug')
	naxis = min(naxis,MAXNAX)
	call xyopen(tno,out,'new',naxis,nsize)
	call hisopen(tno,'append')
	call hdin(lu,tno,version,.false.)
c
c  Copy the image data itself.
c
	call IncIni(naxis,nsize,axes)
	dowhile(Inc3More(naxis,nsize,axes))
	  if(naxis.gt.2)then
	    call fxysetpl(lu,naxis-2,axes(3))
	    call xysetpl(tno,naxis-2,axes(3))
	  endif
	  do j=1,nsize(2)
	    call fxyread(lu,j,array)
	    call xywrite(tno,j,array)
	  enddo
	enddo
c
c  Handle the header.
c
	call axisin(lu,tno,naxis)
c
c  Close up shop.
c
	call fxyclose(lu)
	call hisclose(tno)
	call xyclose(tno)
	end
c************************************************************************
	subroutine axisin(lu,tno,naxis)
c
	implicit none
	integer lu,tno,naxis
c
c  This copies (performing any necessary scaling) the BMAJ,BMIN, CDELT, CROTA,
c  CRVAL, CRPIX and CTYPE, RESTFREQ, XSHIFT and YSHIFT keywords
c  to the output image file. This has to be handled separately as the MIRIAD
c  standard units are radians, GHz and km/sec, whereas FITS uses degrees, Hz
c  and m/sec.
c
c  Inputs:
c    lu		Handle of the input FITS image.
c    tno	Handle of the output MIRIAD image.
c    naxis	Number of dimensions of the image.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i,polcode
	character num*2,ctype*32,bunit*32,types(5)*25,btype*32
	real bmaj,bmin
	double precision cdelt,crota,crval,crpix,scale
	double precision restfreq,obsra,obsdec
	real xshift,yshift
	logical differ,ok
c
c  Externals.
c
	character itoaf*2
c                   1234567890123456789012345
	data types/'percent_polarization     ',
     *		   'fractional_polarization  ',
     *		   'position_angle           ',
     *		   'spectral_index           ',
     *		   'optical_depth            '/
c
	call fitrdhda(lu,'BUNIT',bunit,' ')
	btype = ' '
	differ = .false.
	do i=1,naxis
	  ok = .true.
	  num = itoaf(i)
	  call fitrdhda(lu,'CTYPE'//num,ctype,' ')
	  call fitrdhdd(lu,'CDELT'//num,cdelt,1.d0)
	  call fitrdhdd(lu,'CROTA'//num,crota,0.d0)
	  call fitrdhdd(lu,'CRPIX'//num,crpix,1.d0)
	  call fitrdhdd(lu,'CRVAL'//num,crval,0.d0)
c
	  if(ctype.eq.' ')then
	    if(i.eq.1)then
	      ctype = 'RA---SIN'
	    else if(i.eq.2)then
	      ctype = 'DEC--SIN'
	    endif
	  endif
	  if(ctype(1:5).eq.'RA---')then
	    scale = pi/180d0
	    call fitrdhdd(lu,'OBSRA',obsra,crval)
	    differ = differ.or.(abs(obsra-crval).gt.abs(cdelt))
	  else if(ctype(1:5).eq.'DEC--')then
	    scale = pi/180d0
	    call fitrdhdd(lu,'OBSDEC',obsdec,crval) 
	    differ = differ.or.(abs(obsdec-crval).gt.abs(cdelt))
	  else if(ctype(1:5).eq.'VELO-'.or.ctype(1:5).eq.'FELO-')then
	    scale = 1d-3
	  else if(ctype(1:4).eq.'FREQ')then
	    scale = 1d-9
c
c  AIPS writes out miscellaneous information about the axis type
c  on the Stokes axis.
c
	  else if(ctype(1:6).eq.'STOKES')then
	    polcode = nint(crval+(1-crpix)*cdelt)
	    ok = polcode.ge.-8.and.polcode.le.4.and.polcode.ne.0
	    if(polcode.ge.5.and.polcode.le.9)btype=types(polcode-4)
	  else
	    scale = 1d0
	  endif
	  if(ok)then
	    call wrhdd(tno,'cdelt'//num,scale*cdelt)
	    call wrhdd(tno,'crota'//num,crota)
	    call wrhdd(tno,'crpix'//num,crpix)
	    call wrhdd(tno,'crval'//num,scale*crval)
	    if(ctype.ne.' ') call wrhda(tno,'ctype'//num,ctype)
	  endif
	enddo
c
	if(differ)then
	  call bug('w','Phase and pointing center differ')
	  call output('Differing phase and pointing centers'//
     *		' are not supported')
	  call output('Pointing center will be treated as'//
     *		' being the same as the phase center')
	endif
c
	if(bunit.ne.' ')call wrhda(tno,'bunit',bunit)
	if(btype.ne.' ')call wrbtype(tno,btype)
	call fitrdhdd(lu,'RESTFREQ',restfreq,-1.d0)
	if(restfreq.gt.0) call wrhdd(tno,'restfreq',1.0d-9*restfreq)
	call fitrdhdr(lu,'XSHIFT',xshift,0.)
	if(xshift.ne.0) call wrhdr(tno,'xshift',(pi/180)*xshift)
	call fitrdhdr(lu,'YSHIFT',yshift,0.)
	if(yshift.ne.0) call wrhdr(tno,'yshift',(pi/180)*yshift)
	call fitrdhdr(lu,'BMAJ',bmaj,0.)
	if(bmaj.ne.0) call wrhdr(tno,'bmaj',(pi/180)*bmaj)
	call fitrdhdr(lu,'BMIN',bmin,0.)
	if(bmin.ne.0) call wrhdr(tno,'bmin',(pi/180)*bmin)
c
	end
c************************************************************************
	subroutine xyout(in,out,version)
c
	implicit none
	character in*(*),out*(*),version*(*)
c
c  Write out a image FITS file.
c
c  Inputs:
c    in		Name of the input Miriad image file.
c    out	Name of the output FITS file.
c    version	Version of this program.
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	real array(maxdim)
	integer naxis,tno,lu,j,nsize(MAXNAX),axes(MAXNAX)
	character string*64
c
c  Externals.
c
	logical Inc3More
c
c  Open the input MIRIAD file and determine a few things about it.
c
	call xyopen(tno,in,'old',MAXNAX,nsize)
	if(nsize(1).gt.maxdim)
     *	  call bug('f','Image too big to handle')
	call rdhdi(tno,'naxis',naxis,0)
	naxis = min(naxis,MAXNAX)
c
c  Open the output FITS file.
c
	call fxyopen(lu,out,'new',naxis,nsize)
c
c  Handle the output header.
c
	call axisout(lu,tno,naxis)
	call hdout(tno,lu)
	string = 'Miriad '//version
	call fitwrhda(lu,'ORIGIN',string)
c
c  Copy the data.
c
	call IncIni(naxis,nsize,axes)
	dowhile(Inc3More(naxis,nsize,axes))
	  if(naxis.gt.2)then
	    call xysetpl(tno,naxis-2,axes(3))
	    call fxysetpl(lu,naxis-2,axes(3))
	  endif
	  do j=1,nsize(2)
	    call xyread(tno,j,array)
	    call fxywrite(lu,j,array)
	  enddo
	enddo
c
c  All said and done. Go to sleep.
c
	call xyclose(tno)
	call fxyclose(lu)
c
	end
c************************************************************************
	subroutine axisout(lu,tno,naxis)
c
	implicit none
	integer lu,tno,naxis
c
c  This copies (performing any necessary scaling) the BMAJ, BMIN, 
c  CDELT, CROTA, CRVAL, CRPIX and CTYPE keywords
c  from the MIRIAD input to the FITS output image.
c  FITS uses degrees and m/sec. MIRIAD uses radians and km/sec.
c
c  Inputs:
c    lu		Handle of the input FITS image.
c    tno	Handle of the output MIRIAD image.
c    naxis	Number of dimensions of the image.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i
	character num*2,ctype*32
	real cdelt,crota,crpix,scale,bmaj,bmin
	double precision restfreq,crval
	real xshift,yshift
c
c  Externals.
c
	character itoaf*2
c
	do i=1,naxis
	  num = itoaf(i)
	  call rdhda(tno,'ctype'//num,ctype,' ')
	  call rdhdr(tno,'cdelt'//num,cdelt,1.)
	  call rdhdr(tno,'crota'//num,crota,0.)
	  call rdhdr(tno,'crpix'//num,crpix,1.)
	  call rdhdd(tno,'crval'//num,crval,0.d0)
c
c  Determine scale factor.
c
	  if(ctype(1:5).eq.'RA---')then
	    scale = 180./pi
	    call fitwrhdd(lu,'OBSRA',scale*crval)
	  else if(ctype(1:5).eq.'DEC--')then
	    scale = 180./pi
	    call fitwrhdd(lu,'OBSDEC',scale*crval)
	  else if(ctype(1:4).eq.'FREQ')then
	    scale = 1e9
	  else if(ctype(1:5).eq.'VELO-')then
	    scale = 1e3
	  else
	    scale = 1.
	  endif
	  call fitwrhdr(lu,'CDELT'//num,scale*cdelt)
	  call fitwrhdr(lu,'CROTA'//num,crota)
	  call fitwrhdr(lu,'CRPIX'//num,crpix)
	  call fitwrhdd(lu,'CRVAL'//num,scale*crval)
	  if(ctype.ne.' ')call fitwrhda(lu,'CTYPE'//num,ctype)
	enddo
c
c  Write out other coordinates.
c
	call rdhdd(tno,'restfreq',restfreq,-1.d0)
	if(restfreq.gt.0) call fitwrhdd(lu,'RESTFREQ',1.0d9*restfreq)
	call rdhdr(tno,'xshift',xshift,0.)
	if(xshift.ne.0) call fitwrhdr(lu,'XSHIFT',(180/pi)*xshift)
	call rdhdr(tno,'yshift',yshift,0.)
	if(yshift.ne.0) call fitwrhdr(lu,'YSHIFT',(180/pi)*yshift)
	call rdhdr(tno,'bmaj',bmaj,0.)
	if(bmaj.ne.0) call fitwrhdr(lu,'BMAJ',(180/pi)*bmaj)
	call rdhdr(tno,'bmin',bmin,0.)
	if(bmin.ne.0) call fitwrhdr(lu,'BMIN',(180/pi)*bmin)
c
	end
c************************************************************************
	subroutine hdout(tno,lu)
c
	implicit none
	integer lu,tno
c
c  Convert the header of a FITS file into a MIRIAD data items and history
c  files.
c
c  Input:
c    tno	Handle of the input MIRIAD file.
c    lu		Handle of the output FITS file.
c
c------------------------------------------------------------------------
	integer nshort,nlong
	parameter(nshort=6,nlong=9)
	character short(nshort)*5,long(nlong)*8
	integer iostat,item,n,length
	character key*12,line*80,descr*32,type*16
	logical discard
c
c  Externals.
c
	integer len1,bsrcha
c
	data short/'cdelt','crota','crpix','crval','ctype','naxis'/
	data long/'bmaj   ','bmin   ','history ','image   ',
     *     'obsdec  ','obsra   ','restfreq','xshift  ','yshift  '/
c
c  Short items have numbers attached.
c  Open the "special item" which gives the names of all the items in the
c  file.
c
	call haccess(tno,item,'.','read',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hreada(item,key,iostat)
	dowhile(iostat.eq.0)
	  discard = bsrcha(key,long,nlong).ne.0.or.
     *		    bsrcha(key(1:5),short,nshort).ne.0
	  if(.not.discard)then
	    call hdprobe(tno,key,descr,type,n)
	    if(n.eq.1.and.
     *	      (type.eq.'integer*2' .or. type.eq.'integer' .or.
     *	       type.eq.'real'      .or. type.eq.'double'  .or.
     *	       type.eq.'character'))then
	      if(type.eq.'character')then
	        length = max(8,len1(descr))
		line = key(1:8)//'= '''//descr(1:length)//''''
	      else
		line = key(1:8)//'= '//descr
	      endif
	      call ucase(line)
	      call fitcdio(lu,line)
	    endif
	  endif
	  call hreada(item,key,iostat)
	enddo
	call hdaccess(item,iostat)
c
c  Write out the history file as HISTORY comments.
c
	call copyHist(tno,lu)
	end
c************************************************************************
	subroutine CopyHist(tIn,tOut)
c
	integer tin,tout
c
c  Copy out the history comments.
c
c  Input:
c    tIn	The handle of the input MIRIAD file.
c    tOut	The handle of the output FITS file.
c
c------------------------------------------------------------------------
	logical eof
	character card*132,line*80
c
	call hisopen(tIn,'read')
	call hisread(tIn,card,eof)
	dowhile(.not.eof)
	  line = 'HISTORY '//card(1:72)
	  call fitcdio(tOut,line)
	  call hisread(tIn,card,eof)
	enddo
	call hisclose(tIn)
	end
c************************************************************************
	subroutine hdin(lu,tno,version,isuv)
c
	implicit none
	integer lu,tno
	character version*(*)
	logical isuv
c
c  Convert the header of a FITS file into a MIRIAD data items and history
c  files.
c
c  Input:
c    lu		Handle of the input FITS file.
c    tno	Handle of the output MIRIAD file.
c    in         Input FITS file
c    isuv	True if we are processing a UV file.
c    out        Output MIRIAD file
c    stokes     Stokes parameter used in conversion
c
c------------------------------------------------------------------------
	integer nlong,nshort,nextra
	parameter(nlong=28,nshort=9,nextra=6)
	character card*80,key*8,type*8
	logical more,discard,ok,found
	integer i,k1,k2
	double precision value
	logical history(nlong)
	character long(nlong)*8,short(nshort)*5,uvextra(nextra)*8
c
c  Externals.
c
	integer bsrcha,len1
c
c  A table of keywords (common to uv and xy) that are either history
c  comments, or keywords to be discarded. Some of these are handled
c  as special cases elsewhere. Short keywords have numbers attached.
c
	data (uvextra(i),i=1,nextra)/
     *	  'EPOCH   ','OBJECT  ','OBSDEC  ','OBSERVER','OBSRA   ',
     *	  'TELESCOP'/
	data (long(i),history(i),i=1,nlong)/
     *	  '        ', .true.,  'ALTRPIX ', .false., 'ALTRVAL ', .false.,
     *    'BITPIX  ', .false., 'BLOCKED ', .false.,
     *	  'BMAJ    ', .false., 'BMIN    ', .false., 'BSCALE  ', .false.,
     *	  'BUNIT   ', .false.,
     *	  'BZERO   ', .false., 'COMMAND ', .true.,  'COMMENT ', .true.,
     *	  'DATE    ', .false.,
     *	  'DATE-MAP', .false., 'END     ', .false., 'EXTEND  ', .false.,
     *	  'GCOUNT  ', .false., 'GROUPS  ', .false., 'HISTORY ', .true.,
     *	  'OBSDEC  ', .false., 'OBSDEC  ', .false., 'ORIGIN  ', .false.,
     *	  'PCOUNT  ', .false., 'RESTFREQ', .false., 'SIMPLE  ', .false.,
     *	  'VELREF  ', .false., 'XSHIFT  ', .false., 'YSHIFT  ', .false./
	data short/'CDELT','CROTA','CRPIX','CRVAL','CTYPE','NAXIS',
     *	  'PSCAL','PTYPE','PZERO'/
c
c  Search for the 'SIMPLE' keyword, which effectively sets the pointer
c  at the first card in the file.
c
	call fitsrch(lu,'SIMPLE',found)
	if(.not.found)call bug('f','Weird bug')
c
c  Now process the header.
c
	more = .true.
	dowhile(more)
	  call fitcdio(lu,card)
c
c  Check for, and handle, special keywords.
c
	  i = bsrcha(card(1:8),long,nlong)
	  if(i.ne.0)then
	    discard = .true.
	    if(history(i)) call hiscard(tno,card)
	    more = card(1:8).ne.'END'
	  else
	    if(isuv) discard = bsrcha(card(1:8),uvextra,nextra).ne.0
	    if(.not.discard) discard =
     *		bsrcha(card(1:5),short,nshort).ne.0
	  endif
c
c  Process the card. First determine whether it represents an integer,
c  real, logical, double precision or string. Then create the appropriate
c  item.
c
	  if(.not.discard)then
	    call GetValue(card,type,k1,k2)
	    key = card(1:8)
	    call lcase(key)
	    if(type.eq.'ascii')then
	      do i=k1,k2
		if(card(i:i).lt.' ')card(i:i) = ' '
	      enddo
	      if(len1(card(k1:k2)).gt.0)then
		call wrhda(tno,key,card(k1:k2))
	      else
		call bug('w','Zero length ascii keyword: '//key)
		call hiswrite(tno,card)
	      endif
	    else if(type.eq.'unknown'.or.type.eq.'logical')then
	      call bug('w','Discarding keyword '//key)
	      call hiswrite(tno,card)
c
c  Handle a numeric keyword.
c
	    else
	      call atodf(card(k1:k2),value,ok)
	      if(.not.ok)then
		call bug('w','Error decoding keyword '//key)
		call hiswrite(tno,card)
	      else if(type.eq.'integer')then
		call wrhdi(tno,key,nint(value))
	      else if(type.eq.'real')then
		call wrhdr(tno,key,real(value))
	      else if(type.eq.'double')then
		call wrhdd(tno,key,value)
	      endif
	    endif
	  endif
	enddo
c
c  Add new history
c
	card = 'FITS: Miriad '//version
        call hiswrite (tno, card)
	call hisinput (tno, 'FITS')
c
	end
c************************************************************************
	subroutine hiscard(tno,card)
c
	implicit none
	integer tno
	character card*(*)
c
c  Determine the portion of a history card to write out to the history
c  file. This aims at cutting out as much of the extraneous crap that AIPS
c  puts in history comments as possible, to attempt to improve the
c  appearance of the history file.
c
c  Input:
c    tno	Handle of the output file.
c    card	The card.
c
c------------------------------------------------------------------------
	integer k1,k2
c
c  Externals.
c
	integer len1
c
	k1 = 9
	if(card(k1:k1).eq.' ') k1 = 10
	k2 = len1(card)
	if(k2-k1.gt.len('HISTORY'))then
	  if(card(k2-6:k2).eq.'HISTORY') k2 = len1(card(1:k2-7))
	endif
	if(k1.le.k2) call hiswrite(tno,card(k1:k2))
	end
c************************************************************************
	subroutine GetValue(card,type,k1,k2)
c
	implicit none
	character card*(*),type*(*)
	integer k1,k2
c
c  Determine the type of a value and returns the indices which delimit
c  the value.
c
c  Input:
c    card	The FITS card.
c
c  Output:
c    type	Either 'integer', 'real', 'double', 'ascii' or 'unknown'.
c    k1,k2	These delimit the value in CARD.
c
c------------------------------------------------------------------------
	character c*1
	integer l
	logical more
c
	k1 = 9
	k2 = len(card)
	type = 'unknown'
c
c  Skip to the first non-blank character after the equals sign.
c
	call spanchar(card,k1,k2,' ')
	call spanchar(card,k1,k2,'=')
	call spanchar(card,k1,k2,' ')
	if(k1.gt.k2)return
c
c  At this stage, we should have a quote, plus/minus sign, decimal point,
c  or numeric digit.
c
	c = card(k1:k1)
	if(c.eq.'''')then
	  k1 = k1 + 1
	  l = k1
	  call scanchar(card,l,k2,'''')
	  k2 = l - 1
	  dowhile(k2.ge.k1.and.card(k2:k2).eq.' ')
	    k2 = k2 - 1
	  enddo
	  if(k1.le.k2)type = 'ascii'
c
c  Handle a logical.
c
	else if(c.eq.'T'.or.c.eq.'F')then
	  k2 = k1
	  type = 'logical'
c
c  Handle what must be a numeric value.
c
	else
	  l = k1
	  type = 'integer'
	  more = .true.
	  dowhile(l.le.k2.and.more)
	    c = card(l:l)
	    if(c.eq.' '.or.c.eq.'/')then
	      more = .false.
	      k2 = l - 1
	     if(k1.gt.k2) type = 'unknown'
	    else if(c.eq.'.'.or.c.eq.'E'.or.c.eq.'e')then
	      type = 'real'
	    else if(c.eq.'d'.or.c.eq.'D')then
	      type = 'double'
	    elseif(c.ne.'+'.and.c.ne.'-'.and.(c.lt.'0'.or.c.gt.'9'))then
	      type = 'unknown'
	      more = .false.
	    endif
	    l = l + 1
	  enddo
	endif
c
	end
