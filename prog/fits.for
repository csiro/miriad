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
c
c	WARNING: When writing uv FITS files, fits can handle single
c	frequency band, single array configuration only. Minimal
c	checks are made to see that these restrictions are observed!
c
c       References:
c	  For a description of the standard, see
c	    http://fits.gsfc.nasa.gov/fits_home.html
c
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
c@region
c	The region of interest. The default is the entire input image.
c	See the Users Manual for instructions on how to specify this.
c       Used when op=xyout
c@ select
c	Normal uv selection, used when op=uvout.
c@ stokes
c	Normal Stokes selection, used when op=uvout
c@ options
c	These options applies for op=uvin only.
c	  compress Store the data in compressed uv format.
c	  nochi    Assume that the parallactic angle of the
c	           telescope is a constant 0 (or that the data are
c	           from circularly polarised feeds and have already
c	           been corrected for parallactic angle).
c	  lefty    Assume that the FITS antenna table uses a
c	           left-handed coordinate system (rather than the
c	           more normal right-handed system).
c	  varwt    The visibility weight in the FITS file should
c	           be interpretted as the reciprocal of the noise
c	           variance on that visibility.
c
c	These options for op=uvout only.
c	  nocal    Do not apply the gains table to the data.
c	  nopol    Do not apply the polarization leakage table
c	           to the data.
c	  nopass   Do not apply the bandpass table correctsions
c	           to the data.
c
c	These options apply for op=xyin only.
c	  rawdss   Use the conventions for raw Digital Sky Survey FITS
c	           files, and convert (partially!) the header. A raw
c                  DSS FITS file has header items such as PLTSCALE,
c                  XPIXELSZ, YPIXELSZ etc. If you are unsure if your DSS
c                  image is raw or conventional FITS, run:
c                    Task FITS:
c                      in=mydss.fits
c                      op=print
c                  and look for those header items. Note that DSS images
c                  retrieved using SkyView have a conventional fits header,
c                  and do not require options=rawdss.
c	  nod2     Use the conventions of NOD2 FITS files.
c@ velocity
c	Velocity information. This is only used for op=uvin,
c	and is only relevant for line observations. The default is
c	to use the information present in the FITS header. The
c	"velocity" parameter allows this information to be overriden or
c	the velocity system to be changed.
c
c	Within each line visibility data-set, Miriad stores the velocity
c	of the observatory wrt a rest frame. This allows account to be taken
c	of this when determining channel velocities.
c
c	The fits task will determine the observatory velocity either by being
c	given a velocity at a given channel (wrt a rest frame) or by using
c	a model of Earth and solar system motion (accurate to 0.005km/s).
c
c	The "velocity" parameter can be used to specify the velocity of
c	a particular channel. The parameter consists of three values:
c	the velocity system of the reference value, the reference value
c	and the reference channel, viz:
c	   velocity=velsys,refval,refchan
c	Possible values for the velocity system are:
c	  lsr     Velocity is the radio definition wrt the LSR frame.
c	  bary    Velocity is the radio definition wrt the barycenter.
c	  optlsr  Velocity is the optical definition wrt the LSR frame.
c	  optbary Velocity is the optical definition wrt the barycenter.
c	  obs     Velocity wrt the observatory.
c	  
c	The reference value gives the velocity, at the reference channel,
c	in km/s. If the reference value and reference channel are
c	omitted, a model of Earth and solar system motion is used to
c	determine the appropriate information.
c
c	For example:
c	  velocity=lsr,30,1
c	indicates that the first channel has radio LSR velocity of 30 km/s.
c	The observatory velocity, relative to LSR, can then be computed.
c
c	Alternately:
c	  velocity=lsr
c	indicates that fits is to determine the observatory velocity
c	wrt the LSR frame using an appropriate model.
c--
c
c  Bugs:
c    * uvin should check that the phase and pointing center are the same.
c      xyin should generate the Miriad obsra and obsdec parameters.
c    * xyin should eliminate dummy Stokes axes in some cases.
c      Percent polarisation not correctly handled.
c    * SHould have a "ccin" option to read AIPS clean component tables and convert
c      to Miriad images.
c    * uvout could be infintely smarter (handle multiple windows, write
c      FQ tables, etc).
c    * In uvin 'RESTFREQ' from AIPS SU table is ignored. Also
c      POLAA and POLAB are ignored.
c    * A general mechanism to override wrong stuff from the FITS header
c      is needed. Probably need the user to give an auxillary text file.
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
c    rjs  28-jun-93  Use expanded obspar routines proper-like.
c    rjs  06-jul-93  Depend more on the stuff in the AIPS AN table. Handle
c		     pixel blanking. Another bug in calculation of veldop.
c    rjs  08-jul-93  Better velocity computation for uvin.
c    rjs  21-jul-93  Better velocity/freq handling for uvout.
c    rjs  27-jul-93  Handle AIPS FG tables, and more multi-source/freq
c		     tables.
c    rjs  12-aug-93  Long time bug in hdin, which unnecessarily discarded
c		     extra FITS info. Would not have caused anyone a problem
c		     because the stuff was not the important header info.
c    rjs  19-aug-93  Recognise galactic and ecliptic coordinates in xyin.
c		     Fiddle times when reading in uv data. Include
c		     nutation and aberration when determining apparent
c		     coordinates.
c    rjs  16-sep-93  Rename bsrch to binsrch.
c    rjs  22-oct-93  Did not write out the write freqid to the FG table.
c    rjs  25-oct-93  Stokes axis in xyin was not being handled correctly.
c    rjs  28-nov-93  Correct calculation of parallactic angle for
c		     multi-source files.
c    rjs  30-nov-93  Use ftabskip when listing files.
c    rjs   9-dec-93  Really make sure the default velocity system is
c		     VELO-OBS.
c    rjs  13-dec-93  Sign convention of U(circular), V(linear) change.
c    rjs  21-jan-94  Ignore keywords in input FITS files which contain
c		     special characters.
c    rjs  14-mar094  Correct misunderstanding about antenna coordinates
c		     in AIPS AN file (affects VLA FITS files only).
c    rjs   6-apr-94  Check antenna table ref freq for validity, and use
c		     header ref freq if it looks bad (as suggested by
c		     Ray Plante).
c    rjs   5-jun-94  Get BPA from fits header.
c    rjs  19-jul-94  Fiddle WSRT sign convention to the "normal"
c		     convention.
c    rjs  29-jul-94  Better default integration time.
c    rjs  17-aug-94  Label RA--- and DEC-- image axes from EW telescopes
c		     as NCP.
c    rjs  19-aug-94  Don't make character strings upper case, in hdout.
c    rjs  29-aug-94  Handle w axis in uvin.
c    rjs  13-sep-94  Recognise 'VELOCITY' and 'FELOCITY' axes.
c    rjs  23-sep-94  Handle w axis in uvout.
c    rjs  26-sep-95  Somewhat better handling of odd input axes.
c    rjs   7-nov-95  options=dss.
c    rjs  05-dec-95  Negate the cdelt1 increment when options=dss.
c    nebk 12-jan-96  Replace "percent_polarization" by "polarized_intensity"
c		     in subroutine AXISIN (AIPS manuals say percent_pol
c		     but my empirical evidence is contrary.  Recognize 
c		     LL,MM as RA---SIN and DEC--SIN.
c    rjs  07-aug-96  Correct scaling of axis type.
c    rjs  16-aug-96  Added options=nochi.
c    rjs  17-oct-96  Make the visibility weight equal to 1/sigma**2.
c		     Discard OBSRA and BLANK in reading in images.
c    rjs  07-feb-97  Increase max string length.
c    rjs  21-feb-97  Better treatment of missing evector. More messages.
c    rjs  21-mar-97  Write antenna tables for options=uvout.
c    rjs  06-may-97  Support apparent coordinates in SU table.
c    rjs  08-may-97  Write all FITS keywords in standard format.
c    rjs  02-jul-97  Handle cellscal keyword.
c    rjs  07-jul-97  Improve handling of EPOCH/EQUINOX and pointing parameters.
c    rjs  08-jul-97  Fix bug introduced yesterday.
c    rjs  12-jul-97  Added options=nod2.
c    rjs  16-jul-97  Added options=compress.
c    rjs  01-aug-97  Made FITS date string variables longer, to
c		     allow for new FITS standard.
c    rjs  05-aug-97  More robust in interpretation of epoch keyword.
c    rjs  22-aug-97  More robust again. Also treat unrecognised keywords
c		     as history comments.
c    rjs  25-aug-97  Fix up bug I created on Friday.
c    rjs  20-sep-97  Replace julfdate and fdatejul with julday and dayjul.
c    rjs  21-apr-98  Increase max number of antenna configs.
c    rjs  19-aug-98  Added options=lefty and made the uv writer check obspar
c		     for observatory latitude/longitude if it was missing
c		     from the vis dataset.
c    pjt  15-sep-98  Recognise galactic and ecliptic coordinates the right way
c    rjs  25-sep-98  Correct handling of OBSRA and OBSDEC in op=xyin.
c    rjs  27-oct-98  Check in CD keyword for image pixel increment.
c    rjs  20-nov-98  Better handling of image projections and rotation.
c    rjs  25-nov-98  More work on better handling of image projection and rotation.
c    rjs  07-jan-99  Write dates in new FITS format.
c    rjs  26-feb-99  Used new subroutine "fitdate" to be more robust to
c		     corrupted dates.
c    rjs  20-jul-99  uvout writes AIPS SU tables.
c    rjs  30-aug-99  Changed some "PI" to "DPI"
c    rjs  11-nov-99  options=varwt
c    rjs  11-apr-00  In uvout, multisource files were always being generated.
c    rjs  10-may-00  In xyout, increase size of descr buffer.
c    rjs  04-Oct-00  Make xyout work for arbitrarily large images.
c    rjs  10-oct-00  Really do the above this time!
c    dpr  01-nov-00  Change CROTAn to AIPS convention for xyout
c    dpr  27-nov-00  fix stokes convention for xyin
c    dpr  05-apr-01  Add region key for op=xyout
c    dpr  10-may-01  Change dss to rawdss
c    dpr  11-may-01  Check history exists before copying it
c    dpr  26-jun-01  Relax antenna table format restrictions
c    dpr  02-jul-01  Relax AN restrictions properly (I hope!!)
c    rjs  04-oct-01  Get GLS history comment right.
c    nebk 08-jan-02  In AntWrite, set POLAA and POLAB to 45/135 for ATCA
c    rjs  29-dec-03  Be more robust to poorly formed CTYPE labels in 
c		     inputs.
c    rjs  04-jul-05  More robust to some weirdo variants.
c    rjs  19-sep-05  Better defaults for velocity info, and better
c		     velocity formula. Handle AIPS OB tables.
c    rjs  24-sep-05  Increase max number of sources in op=uvout
c    rjs  03-aug-06  In reading in images, llrot was sometimes not
c                    correct. Also handle some rare keywords a little better.
c------------------------------------------------------------------------
	character version*(*)
	parameter(version='Fits: version 1.1 03-Aug-06')
	integer maxboxes
	parameter(maxboxes=2048)
	character in*128,out*128,op*8,uvdatop*12
	integer velsys
	integer boxes(maxboxes)
	real altrpix,altrval
	logical altr,docal,dopol,dopass,dss,dochi,nod2,compress
	logical lefty,varwt
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
        if(op.eq.'uvin')call GetVel(velsys,altr,altrval,altrpix)
	if(op.eq.'xyout') call BoxInput('region',in,boxes,maxboxes)
c
c  Get options.
c
        call getopt(docal,dopol,dopass,dss,nod2,dochi,compress,
     *							lefty,varwt)
        if(op.eq.'uvout') then
          uvdatop = 'sdlb3'
	  if(docal)uvdatop(7:7) = 'c'
	  if(dopol)uvdatop(8:8) = 'e'
	  if(dopass)uvdatop(9:9) = 'f'
          call uvdatinp('in',uvdatop)
        endif
c
	call keyfin
	if(op.ne.'uvout'.and.in.eq.' ')
     *	  call bug('f','Input file name is missing')
	if(op.ne.'print'.and.out.eq.' ')
     *	  call bug('f','Output file name is missing')
c
c  Handle the five cases.
c
	if(op.eq.'uvin')then
	  call uvin(in,out,velsys,altr,altrpix,altrval,dochi,
     *				    compress,lefty,varwt,version)
	else if(op.eq.'uvout')then
	  call uvout(out,version)
	else if(op.eq.'xyin')then
	  call xyin(in,out,version,dss,nod2)
	else if(op.eq.'xyout')then
	  call xyout(in,out,version,boxes)
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
	subroutine GetVel(velsys,altr,altrval,altrpix)
c
	implicit none
	integer velsys
	logical altr
	real altrval,altrpix
c
c  Determine the velocity system.
c
c  Output:
c    velsys	Velocity system desired.
c    altr	True if altrval is set.
c    altrval	Reference velocity.
c    altrpix	Reference channel.
c------------------------------------------------------------------------
	integer OBSRADIO,OBSOPTIC,LSRRADIO,LSROPTIC,HELRADIO,HELOPTIC
	parameter(OBSRADIO=259,OBSOPTIC=3,LSRRADIO=257,LSROPTIC=1)
	parameter(HELRADIO=258,HELOPTIC=2)
c
	integer nout
	character string*8
c
	integer nopts
	parameter(nopts=5)
	integer vals(nopts)
	character opts(nopts)*8
c
c  Externals.
c
	integer binsrcha
	logical keyprsnt
c
c  Data.
c
	data opts/'bary    ','lsr     ','obs     ',
     *			     'optbary ','optlsr  '/
	data vals/ HELRADIO,  LSRRADIO,  OBSRADIO,
     *			      HELOPTIC,  LSROPTIC/
c
	call keymatch('velocity',nopts,opts,1,string,nout)
	if(nout.eq.0)then
	  velsys = 0
	  altr = .false.
	else
	  velsys = vals(binsrcha(string,opts,nopts))
	  altr = keyprsnt('velocity')
	  call keyr('velocity',altrval,0.)
	  call keyr('velocity',altrpix,1.)
	endif
	end
c************************************************************************
      subroutine getopt(docal,dopol,dopass,dss,nod2,dochi,
     *						compress,lefty,varwt)
c
      implicit none
      logical docal,dopol,dopass,dss,dochi,nod2,compress,lefty,varwt
c
c     Get a couple of the users options from the command line
c
c  Output:
c    docal   Apply gain calibration
c    dopol   Apply polarization calibration
c    dopass  Apply bandpass calibration
c    dss     Handle DSS image.
c    nod2    Handle NOD2 image.
c    dochi   Attempt to calculate the parallactic angle.
c    compress Store data in compressed format.
c    lefty   Assume antenna table uses a left-handed system.
c    varwt   Interpret the visibility weight as the reciprocal of the
c	     noise variance.
c------------------------------------------------------------------------
      integer nopt
      parameter (nopt = 10)
      character opts(nopt)*8
      logical present(nopt),olddss
      data opts /'nocal   ','nopol   ','nopass  ','rawdss  ',
     *		 'nod2    ','nochi   ','compress','lefty   ',
     *		 'varwt   ','dss     '/
c
      call options ('options', opts, present, nopt)
      docal    = .not.present(1)
      dopol    = .not.present(2)
      dopass   = .not.present(3)
      dss      =      present(4)
      nod2     =      present(5)
      dochi    = .not.present(6)
      compress =      present(7)
      lefty    =      present(8)
      varwt    =      present(9)
      olddss   =      present(10)
c
      if (olddss) then
	call bug('w','Option DSS is deprecated. Please use RAWDSS')
        dss=.true.
      endif
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
	  call ftabSkip(lu,' ',more)
	  if(more)call fitsrch(lu,'XTENSION',more)
	enddo
	call fitclose(lu)
	end
c************************************************************************
	subroutine uvin(in,out,velsys,altr,altrpix,altrval,dochi,
     *					compress,lefty,varwt,version)
c
	implicit none
	character in*(*),out*(*)
	integer velsys
	logical altr,dochi,compress,lefty,varwt
	real altrpix,altrval
	character version*(*)
c
c  Read in a UV FITS file. 
c
c  Inputs:
c    in		Name of the input uv FITS file.
c    out	Name of the output MIRIAD file.
c    velsys	Velocity system.
c    altr	True if the user specified altrpix and altrval.
c    altrpix	The user given value for altrpix.
c    altrval	The user given value for altrval.
c    dochi	Attempt to calculate the parallactic angle.
c    compress   Store the data in compressed format.
c    lefty      Assume the antenna table uses a left-handed system.
c    varwt	Interpret the visibility weight as the reciprocal of the
c		noise variance.
c------------------------------------------------------------------------
	include 'maxdim.h'
	integer PolXX,PolYY,PolXY,PolYX
	parameter(PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)
c
	integer MAXTIME
	parameter(MAXTIME=10240)
	integer lu,tno,nvis,npol,nfreq,i,j,bitpix,nif
	integer ant1,ant2,nants,bl,nconfig,config,srcid,freqid
	integer itemp,offset,P,Pol0,PolInc
	logical flags(maxchan),zerowt,found,anfound
	logical conj,sfudge
	complex corr(maxchan)
	real fac,swt
	integer nwt
	real visibs(7+12*maxchan)
	double precision preamble(5),T0,uu,vv,ww,time
	real times(MAXTIME),inttime
	integer ntimes,refbase,litime
	integer uvU,uvV,uvW,uvBl,uvT,uvSrcId,uvFreqId,uvData
	character telescop*32,itime*8
	integer antloc(MAXANT)
c
c  Externals.
c
	integer len1,PolCvt
	character itoaf*8
	double precision fuvGetT0
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
	call histin(lu,tno,version)
	if(.not.compress.and.abs(bitpix).gt.16)
     *	  call uvset(tno,'corr','r',0,0.,0.,0.)
c
c  Determine if its a multisource file, and set the random parameters to
c  handle accordingly.
c
	call Indices(lu,uvU,uvV,uvW,UvT,uvBl,uvSrcId,uvFreqid,uvData)
	if(uvW.eq.0)then
	  call uvset(tno,'preamble','uv/time/baseline',0,0.,0.,0.)
	else
	  call uvset(tno,'preamble','uvw/time/baseline',0,0.,0.,0.)
	endif
c
c  Load antenna, source and frequency information. Set frequency information.
c
	call TabLoad(lu,uvSrcId.ne.0,uvFreqId.ne.0,
     *		telescop,anfound,Pol0,PolInc,nif,dochi,lefty,
     *          nants,antloc)
	call TabVeloc(velsys,altr,altrval,altrpix)
c
c  Load any FG tables.
c
	call FgLoad(lu,tno)
c
c  Rewind the header.
c
	call ftabLoc(lu,' ',found)
	if(.not.found)call bug('f','Error rewinding to main header')
c
c  Messages about the polarisations present. Note that much ATCA
c  data comes in being labelled as circulars. Tell the user there is
c  a problem, and relabel it as linears.
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
c  Do some telescope-specific operations.
c
	sfudge = .false.
	if(telescop.eq.'ATCA')then
	  if(Pol0.le.-1.and.Pol0.ge.-4) then
            Pol0 = -5
            call bug('w',
     *	    'Relabelling ATCA circularly polarised data as linear')
          endif
	else if(telescop.eq.'WSRT')then
	  call bug('w','Changing sign convention of XY correlations')
	  call bug('w','for WSRT data')
	  sfudge = .true.
	endif
c
c  Determine the noise scaling factor.
c
	fac = 1
	if(Pol0.gt.0)fac = 2
c
c  Write out the telescope.
c
	if(telescop.ne.' ')call uvputvra(tno,'telescop',telescop)
c
c  Do the descriptions of polarisations and the "IF" axis.
c
	call uvputvri(tno,'nants',0,1)
	call uvputvri(tno,'npol',npol,1)
	call wrhdi(tno,'npol',npol)
c
c  Initialise things to work out the integration time.
c
	if(varwt)then
	  call DoTime(lu,nvis,uvT,uvBl,visibs,
     *				times,MAXTIME,inttime)
	  ntimes = MAXTIME + 1
	else
	  inttime = 10
	  ntimes = 0
	endif
	call uvputvrr(tno,'inttime',inttime,1)
c
c  Copy the data itself. The conversion of units is as follows:
c   Variable		FUVREAD		  UVWRITE
c     U			  sec		  nanosec
c     V		 	  sec		  nanosec
c     W			  sec		  nanosec
c     Time	  Offset Julian days	Julian days
c
	nconfig = 0
	zerowt = .false.
	srcid = 1
	freqid = 1
	T0 = fuvGetT0(lu)
c
	call output('Reading the correlation data')
	do i=1,nvis
	  call fuvread(lu,visibs,i,1)
c
c  Unpack the preamble.
c
	  uu = 1.0e9 * visibs(uvU)
	  vv = 1.0e9 * visibs(uvV)
	  if(uvW.gt.0)then
	    ww = 1.0e9 * visibs(uvW)
	  else
	    ww = 0
	  endif
	  time = visibs(uvT) + T0
	  bl = int(visibs(uvBl) + 0.01)
	  ant1 = bl/256
	  ant2 = mod(bl,256)
	  config = nint(100*(visibs(uvBl)-bl))+1
	  if(uvSrcid.gt.0) srcid  = nint(visibs(uvSrcId))
	  if(uvFreqid.gt.0)freqid = nint(visibs(uvFreqId))
c
c  Convert baseline number to normal Miriad. Note the different conventions!
c
c  Miriad convention is that bl = 256*ant1 + ant2, where the baseline is ant2 - ant1
c  AIPS                      bl = 256*ant1 + ant2                        ant1 - ant2 !!
c
c  In both cases, ant1 < ant2.
c  the variables ant1,ant2 are Miriad antenna numbers.
c
	  conj = ant2.gt.ant1
	  if(conj)then
	    uu = -uu
	    vv = -vv
	    ww = -ww
	    offset = uvData
	    do j=1,npol*nfreq
	      visibs(offset+1) = -visibs(offset+1)
	      offset = offset + 3
	    enddo
	  else
	    itemp = ant1
	    ant1 = ant2
	    ant2 = itemp
	  endif
c  This is allows fits files with no AN table, but
c  for those with an AN table, and antenna numbers which
c  do not start at 1 it keeps the nants from the table
          if (anfound) then
	    ant1=antloc(ant1)
	    ant2=antloc(ant2)
          else 
	    nants = max(nants,ant1,ant2)
	  end if
	  bl = 256*ant1 + ant2
	  nconfig = max(config,nconfig)
c
c  Determine some times at whcih data are observed. Use these later to
c  guestimate the integration time.
c
	  if(i.eq.1) refbase = bl
	  if(refbase.eq.bl.and.ntimes.lt.MAXTIME)then
	    ntimes = ntimes + 1
	    times(ntimes) = visibs(uvT)
	  endif
c
c  Write out the necessary table information.
c
	  call TabWrite(tno,srcid,freqid,config,time)
c
c  Determine Jyperk if varwt is true.
c
	  if(varwt)then
	    nwt = 0
	    swt = 0
	    offset = uvData
	    do j=1,npol*nfreq/nif
	      if(abs(visibs(offset+2)).gt.0)then
		swt = swt + abs(visibs(offset+2))
		nwt = nwt + 1
	      endif
	      offset = offset + 3
	    enddo
	    if(nwt.gt.0)call TabVar(tno,fac*nwt/swt,inttime)
	  endif
c
c  Store the correlation data.
c
	  preamble(1) = uu
	  preamble(2) = vv
	  if(uvW.gt.0)then
	    preamble(3) = ww
	    preamble(4) = time
	    preamble(5) = bl
	  else
	    preamble(3) = time
	    preamble(4) = bl
	  endif
c
	  do j=1,npol
	    P = Pol0 + (j-1)*PolInc
	    if(.not.conj)P = PolCvt(P)
c
c  The following code was once required to work around a labelling
c  bug in NEWSTAR -- I think this bug has now been corrected there.
c
c	if(P.eq.PolYY.or.P.eq.PolXY)then
c	  P = P -1
c	else if(P.eq.PolYX)then
c	  P = PolYY
c	endif
	    call uvputvri(tno,'pol',P,1)
	    call Extract(nfreq,npol,j,visibs(uvData),corr,flags,zerowt)
	    if(sfudge.and.P.eq.PolYX)call Negate(nfreq,corr)
	    call uvwrite(tno,preamble,corr,flags,nfreq)
	  enddo
	enddo
c
c  Work out the integration time now, and use override mechanism to set
c  it in the data-set.
c
	if(.not.varwt)call GetInt(times,ntimes,inttime)
	call wrhdr(tno,'inttime',inttime)
	itime = itoaf(nint(inttime))
	litime = len1(itime)
	call output('The estimated integration time of a sample is '//
     *	  itime(1:litime)//' seconds')
c
c  Write out the number of antennas.
c
	if(.not.anfound)call wrhdi(tno,'nants',nants)
	call output('Number of antennas: '//itoaf(nants))
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
	subroutine DoTime(lu,nvis,uvT,uvBl,visibs,times,
     *						maxtimes,inttime)
c
	implicit none
	integer lu,nvis,uvT,uvBl,maxtimes
	real visibs(*),times(maxtimes),inttime
c
c  Guestimate the integration time of a sample.
c------------------------------------------------------------------------
	integer bl,refbl,i,ntimes
c
	i = 0
	ntimes = 0
	dowhile(ntimes.lt.MAXTIMES.and.i.lt.nvis)
	  i = i + 1	  
	  call fuvread(lu,visibs,i,1)
	  bl = int(visibs(uvBl) + 0.01)
	  if(i.eq.1)refbl = bl
	  if(bl.eq.refbl)then
	    ntimes = ntimes + 1
	    times(ntimes) = visibs(uvT)
	  endif
	enddo
c
	call GetInt(times,ntimes,inttime)
c
	end
c************************************************************************
	subroutine Negate(nfreq,corr)
c
	implicit none
	integer nfreq
	complex corr(nfreq)
c------------------------------------------------------------------------
	integer i
c
	do i=1,nfreq
	  corr(i) = -corr(i)
	enddo
c
	end
c************************************************************************
	subroutine Indices(lu,uvU,uvV,uvW,uvT,uvBl,uvSrcId,
     *							uvFreqid,uvData)
c
	implicit none
	integer lu
	integer uvU,uvV,uvW,uvT,uvBl,uvSrcid,uvFreqid,uvData
c
c  Determine indices for the random parameters and data.
c
c  Input:
c    lu 	Handle of the FITS file.
c  Output:
c    uvU,uvV,uvW,uvT,uvBl,uvSrcid,uvFreqid,uvData
c------------------------------------------------------------------------
	integer i,naxis,nrandom
	character num*2,type*16
	character random(7)*8
c
c  Externals.
c
	character itoaf*2
c
	uvU = 1
	random(1) = 'UU'
	uvV = 2
	random(2) = 'VV'
	uvBl = 3
	random(3) = 'BASELINE'
	uvT = 4
	random(4) = 'DATE'
	nrandom = 4
c
	uvW = 0
	uvSrcid = 0
	uvFreqid = 0
c
c  Determine whether there is a source-id and freq-id random parameter.
c
	call fitrdhdi(lu,'PCOUNT',naxis,0)
	do i=1,naxis
	  num = itoaf(i)
	  call fitrdhda(lu,'PTYPE'//num,type,' ')
	  if((type(1:3).eq.'WW'.or.type(1:3).eq.'WW-').and.uvW.eq.0)then
	    nrandom = nrandom + 1
	    uvW = nrandom
	    random(nrandom) = 'WW'
	  endif
	  if(type.eq.'SOURCE'.and.uvSrcid.eq.0)then
	    nrandom = nrandom + 1
	    uvSrcid = nrandom
	    random(nrandom) = type
	  endif
	  if(type.eq.'FREQSEL'.and.uvFreqid.eq.0)then
	    nrandom = nrandom + 1
	    uvFreqid = nrandom
	    random(nrandom) = type
	  endif
	enddo
c
	uvData = nrandom + 1
	call fuvSetPa(lu,nrandom,random)
c
	end
c************************************************************************
	integer function PolCvt(P)
c
	implicit none
	integer P
c
c  Fiddle the polarisation labelling around, because of the different
c  labelling conventions of Miriad and FITS.
c
c  Input:
c    P		Polarisation code.
c------------------------------------------------------------------------
	integer PolYX,PolXY,PolYY,PolXX,PolLR,PolRL
	integer PolMin,PolMax
	parameter(PolYX=-8,PolXY=-7,PolYY=-6,PolXX=-5,PolLR=-4,PolRL=-3)
	parameter(PolMin=-8,PolMax=-3)
c
	integer pols(PolMin:PolMax)
	data pols/PolXY,PolYX,PolYY,PolXX,PolRL,PolLR/
c
	PolCvt = p
	if(p.ge.PolMin.and.p.le.PolMax)PolCvt = pols(p)
c
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
	real delta
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
c  Find the first delta time greater than 1 sec as the integration time.
c
	inttime = times(ntimes) - times(1)
	do i=1,ntimes-1
	  delta = times(i+1) - times(i)
	  if(delta.gt.1./(24.*3600.).and.delta.lt.inttime)
     *	    inttime = delta
	enddo
	inttime = 24*3600*inttime
c
	end
c************************************************************************
	subroutine FgLoad(lu,tno)
c
	implicit none
	integer lu,tno
c
c  Load any AIPS FG tables.
c------------------------------------------------------------------------
	double precision Time0
	integer ntab,lTab,iostat
	logical more
c
c  Externals.
c
	character itoaf*3
c
c  Get the reference time, and convert it to a Julian date.
c
	call TabRefT0(Time0)
c
c  Look for FG tables.
c
	ntab = 0
	call ftabLoc(lu,'AIPS FG',more)
	dowhile(more)
	  call output('Reading AIPS FG table -- '//
     *				'apply this with task fgflag')
	  ntab = ntab + 1
	  call haccess(tno,lTab,'aipsfg'//itoaf(ntab),'write',iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error accessing output table')
	    call bugno('f',iostat)
	  endif
c
c  Process the AIPS FG table.
c
	  call aips2mir(lu,lTab,Time0)
c
c  Close up this item.
c
	  call hdaccess(lTab,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error accessing output table')
	    call bugno('f',iostat)
	  endif
	  call ftabNxt(lu,'AIPS FG',more)
	enddo
c
	end
c************************************************************************
	subroutine aips2mir(lIn,lTab,Time0)
c
	implicit none
	integer lIn,lTab
	double precision Time0
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
c
	character type*1,units*16
	integer nrows,nval
	integer pSrc,pSub,pAnts,pTime,pIfs,pChans,pFreq
c
	call ftabInfo(lIn,'SOURCE',type,units,nrows,nval)
	if(nrows.le.0.or.nval.ne.1.or.type.ne.'I')then
	  call bug('w','Unrecognised AIPS FG table format')
c
c  Allocate space for the various tables.
c
	else
	  call FgGeti(lIn,pSrc,'SOURCE',1,nrows)
	  call FgGeti(lIn,pSub,'SUBARRAY',1,nrows)
	  call FgGeti(lIn,pFreq,'FREQ ID',1,nrows)
	  call FgGeti(lIn,pAnts,'ANTS',2,nrows)
	  call FgGetr(lIn,pTime,'TIME RANGE',2,nrows)
	  call FgGeti(lIn,pIfs,'IFS',2,nrows)
	  call FgGeti(lIn,pChans,'CHANS',2,nrows)
c
	  call FgWrite(lTab,Time0,
     *	    nrows,MemI(pSrc),MemI(pSub),MemI(pFreq),MemI(pAnts),
     *		  MemR(pTime),MemI(pIfs),MemI(pChans))
c
	  call MemFree(pSrc,nrows,'i')
	  call MemFree(pSub,nrows,'i')
	  call MemFree(pFreq,nrows,'i')
	  call MemFree(pAnts,2*nrows,'i')
	  call MemFree(pTime,2*nrows,'r')
	  call MemFree(pIfs,2*nrows,'i')
	  call MemFree(pChans,2*nrows,'i')
	endif
	end
c************************************************************************
	subroutine FgWrite(lTab,Time0,nrows,
     *	  SrcId,SubArray,FreqId,Ants,Time,Ifs,Chans)
c
	implicit none
	integer lTab,nrows
	double precision Time0
	integer SrcId(nrows),SubArray(nrows),Ants(2,nrows)
	integer Ifs(2,nrows),Chans(2,nrows),FreqId(nrows)
	real Time(2,nrows)
c----------------------------------------------------------------------
	integer i,length,iostat
	character line*132
c
	do i=1,nrows
	  length = 0
	  if(time(1,i).ne.0.or.time(2,i).ne.0)
     *	    call FgTimOut(time(1,i)+time0,time(2,i)+time0,
     *	      line,length)
	  if(srcid(i).gt.0)
     *	    call FgValOut('srcid',SrcId(i),1,line,length)
	  if(freqid(i).gt.0)
     *	    call FgValOut('freqid',FreqId(i),1,line,length)
	  if(subarray(i).gt.0)
     *	    call FgValOut('array',SubArray(i),1,line,length)
	  if(ants(1,i).gt.0.or.ants(2,i).gt.0)
     *	    call FgValOut('ant',ants(1,i),2,line,length)
	  if(chans(1,i).gt.0.or.chans(2,i).gt.0)
     *	    call FgValOut('chan',chans(1,i),2,line,length)
	  if(ifs(1,i).gt.0.or.ifs(2,i).gt.0)
     *	    call FgValOut('ifs',ifs(1,i),2,line,length)
c
	  call hwritea(lTab,line(1:length),iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error writing to the aipsfg table')
	    call bugno('f',iostat)
	  endif
	enddo
	end
c************************************************************************
	subroutine FgValOut(type,vals,nvals,line,length)
c
	implicit none
	character type*(*),line*(*)
	integer nvals,vals(nvals),length
c------------------------------------------------------------------------
	integer k
c
	k = len(type)
	if(length+k+3.gt.len(line))
     *	  call bug('f','Buffer overflow, in FgValOut')
	if(length.gt.0)then
	  line(length+1:length+k+2) = ','//type//'('
	  length = length + k + 2
	else
	  line(length+1:length+k+1) = type//'('
	  length = length + k + 1
	endif
	call mitoaf(vals,nvals,line(length+1:),k)
	length = length + k + 1
	if(length.gt.len(line))
     *	  call bug('f','Buffer overflow, in FgValOut')
	line(length:length) = ')'
	end
c************************************************************************
	subroutine FgTimOut(t1,t2,line,length)
c
	implicit none
	double precision t1,t2
	character line*(*)
	integer length
c------------------------------------------------------------------------
	integer k1,k2
	character time1*32,time2*32
c
c  Externals.
c
	integer len1
c
	call julday(t1,'H',time1)
	k1 = len1(time1)
	call julday(t2,'H',time2)
	k2 = len1(time2)
c
	if(length.ne.0)then
	  if(length.ge.len(line))
     *	    call bug('f','Buffer overflow, in FgTimOut')
	  length = length + 1
	  line(length:length) = ','
	endif
c
	if(length+k1+k2+len('time(,)').gt.len(line))
     *	  call bug('f','Buffer overflow, in FgTimOut')
	line(length+1:length+k1+k2+7) =
     *	  'time('//time1(1:k1)//','//time2(1:k2)//')'
	length = length + k1 + k2 + 7
c
	end
c************************************************************************
	subroutine FgGeti(lIn,pnt,name,nx,ny)
c
	integer pnt,nx,ny,lIn
	character name*(*)
c
c  Check whether something is in the flagging table. If so get if. If not
c  set default values to 0.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
c
	integer nrow,nval,i
	character type*1,units*24
c
	call MemAlloc(pnt,nx*ny,'i')
	call ftabInfo(lIn,name,type,units,nrow,nval)
	if(type.eq.' '.or.nrow*nval.eq.0)then
	  do i=pnt,pnt+nx*ny-1
	    MemI(i) = 0
	  enddo
	else
	  if(nx.ne.nval.or.ny.ne.nrow.or.type.ne.'I')
     *	    call bug('f','FG table has an odd shape')
	  call ftabGeti(lIn,name,0,MemI(pnt))
	endif
	end
c************************************************************************
	subroutine FgGetr(lIn,pnt,name,nx,ny)
c
	integer pnt,nx,ny,lIn
	character name*(*)
c
c  Check whether something is in the flagging table. If so get if. If not
c  set default values to 0.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mem.h'
	integer nrow,nval,i
	character type*1,units*24
c
	call MemAlloc(pnt,nx*ny,'r')
	call ftabInfo(lIn,name,type,units,nrow,nval)
	if(type.eq.' '.or.nrow*nval.eq.0)then
	  do i=pnt,pnt+nx*ny-1
	    MemR(i) = 0
	  enddo
	else
	  if(nx.ne.nval.or.ny.ne.nrow.or.type.ne.'R')
     *	    call bug('f','FG table has an odd shape')
	  call ftabGetr(lIn,name,0,MemR(pnt))
	endif
	end
c************************************************************************
c************************************************************************
	subroutine TabLoad(lu,dosu,dofq,tel,anfound,Pol0,PolInc,nif0,
     *	  dochi,lefty,numants,antloc)
c
	implicit none
	integer lu,Pol0,PolInc,nif0
	logical dosu,dofq,anfound,dochi,lefty
	character tel*(*)
	integer numants
	integer antloc(*)
c
c  Determine some relevant parameters about the FITS file. Attempt to
c  ferrit this information from all nooks and crannies. In general use
c  the procedure of looking at known telescope parameters, the main
c  FITS header, and then any extension tables. As particular information
c  could be found at any level, use the higher level stuff as defaults
c  for the lower level values.
c
c  AIPS tables read include AN, FQ, CH and SU tables. NOTE that CL tables
c  are not read!
c
c  Input:
c    lu		Handle of the input FITS file.
c    dosu,dofq	Expect a multisource/multi-freq file.
c    dochi	Attempt to compute the parallactic angle.
c    lefty	Assume the antenna table uses a left-handed system.
c  Output:
c    tel	Telescope name.
c    anfound	True if antenna tables were found.
c    nif0       Number of IFs
c    Pol0	Code for first polarisation.
c    PolInc	Increment between polarisations.
c    numants    Total number of antennas in AN table
c    antloc     AN table indices for antenna station numbers
c               zero if station number not used.
c------------------------------------------------------------------------
	include 'mirconst.h'
	include 'fits.h'
c
	logical badapp,badepo,more,found,badmnt
	double precision Coord(3,4),rfreq,freq
	double precision veldef
	character defsrc*16,num*2
	real defepoch,diff
	integer nval,i,j,t,nxyz,n,naxis,itemp
	double precision xyz(3,MAXANT),xc,yc,zc,r0,d0
	double precision temp,eporef,vddef
	character type*1,units*16,ctype*8
	integer sta(MAXANT)
c
c  Externals.
c
	character itoaf*2
	double precision fuvGetT0,Epo2jul,Jul2epo
c
c  Set default nants, source and freq ids.
c
	inited = .false.
	numants = 0
c
c  Get some preliminary info from the main header.
c
	call fuvrdhd(lu,Coord)
	call fitrdhda(lu,'TELESCOP',telescop,' ')
	if(telescop.eq.' ')
     *	  call fitrdhda(lu,'INSTRUME',telescop,' ')
	tel = telescop
	call fitrdhda(lu,'OBSERVER',observer,' ')
	call fitrdhda(lu,'OBJECT',defsrc,' ')
	call fitrdhdr(lu,'EPOCH',defepoch,2000.0)
c
c  Determine reference time.
c
	call fitdate(lu,'DATE-OBS',timeref)
	if(timeref.eq.0)timeref = fuvGetT0(lu)
c
c  Get velocity definition information, just in case this is a spectral
c  line observation.
c
	call fitrdhdd(lu,'RESTFREQ',rfreq,0.d0)
	call fitrdhdr(lu,'ALTRPIX',velref,real(Coord(uvCrpix,uvFreq)))
	call fitrdhdi(lu,'VELREF',velsys,257)
	freq = (velref-Coord(uvCrpix,uvFreq))*Coord(uvCdelt,uvFreq)
     *		 + Coord(uvCrval,uvFreq)
	if(rfreq.gt.0.and.freq.gt.0)then
	  if(velsys.lt.256)then
	    vddef = CMKS*(rfreq-freq)/freq
	  else
	    vddef = CMKS*(rfreq-freq)/rfreq
	  endif
	else
	  vddef = 0
	endif
	call fitrdhdd(lu,'ALTRVAL',veldef,vddef)
c
c  See what the dimension of the IF axis according to the header.
c
	call fitrdhdi(lu,'NAXIS',naxis,0)
	nif = 1
	nchan = 1
	do i=2,naxis
	  num = itoaf(i)
	  call fitrdhda(lu,'CTYPE'//num,ctype,' ')
	  if(ctype.eq.'IF') call fitrdhdi(lu,'NAXIS'//num,nif,1)
	  if(ctype.eq.'FREQ')call fitrdhdi(lu,'NAXIS'//num,nchan,1)
	enddo
	if(nif.gt.MAXIF)call bug('f','Too many IFs')
	nif0 = nif
c
c  Polarisation information.
c
	Pol0 = nint( Coord(uvCrval,uvStokes) + 
     *		(1-Coord(uvCrpix,uvStokes))*Coord(uvCdelt,uvStokes) )
	PolInc = nint(Coord(uvCdelt,uvStokes))
c
c  Set default values for reference freq, lat, long, mount, evector.
c  Also determine the only values for systemp and jyperk.
c
	call telpar(telescop,systemp,systok,jyperk,jok,
     *	  llok,lat,long,emok,evec,mount)
	if(.not.emok.and.dochi)call bug('w',
     *	  'Insufficient information to determine parallactic angle')
	emok = emok.and.dochi
	freqref(1) = 1e-9 * Coord(uvCrval,uvFreq)	  
c
c  Load the antenna table.
c
	nconfig = 0
	call ftabLoc(lu,'AIPS AN',found)
	anfound = found
	dowhile(found)
	  call output('Reading AIPS AN table')
	  nconfig = nconfig + 1
	  call ftabInfo(lu,'STABXYZ',type,units,n,nxyz)
c
	  if(nconfig.gt.MAXCONFG)
     *	    call bug('f','Too many array configurations')
	  if(nxyz.ne.3.or.n.le.0.or.type.ne.'D')
     *	    call bug('f','Something is screwy with the antenna table')
	  if(n.gt.MAXANT)call bug('f','Too many antennas for me')
c
c  Set up antloc to handle entries where table row and station
c  number are different
c
	  do i=1,MAXANT
	    antloc(i)=0
	  end do
c
	  call ftabGeti(lu,'NOSTA',0,sta)
c
	  do i=1,n
	    antloc(sta(i))=i
	  enddo
c
	  if(sta(n).ne.n)then
	    call bug('w',
     *	      ' Some antennas were missing from the antenna table ')
	  endif
	  numants = numants + n
	  nants(nconfig) = n
c
c  Get the reference freqeuncy. Note that multiple bugs in AIPS
c  make the reference frequency (either in AN table or header)
c  suspect when subsetting multi-IF data.
c
	  call fitrdhdd(lu,'FREQ',freqref(nconfig),
     *					Coord(uvCrval,uvFreq))
	  if(freqref(nconfig).le.1)then
	    freqref(nconfig) = Coord(uvCrval,uvFreq)
	    call bug('w','Antenna table reference frequency looks bad')
	    call bug('w','Using header reference frequency')
	  elseif(abs(freqref(nconfig)-Coord(uvCrval,uvFreq)).gt.
     *	     0.01*abs(freqref(nconfig)).and.nconfig.eq.1)then
	    call bug('w',
     *	      'Header and antenna table reference frequency differ')
	    call bug('w','Using antenna table reference frequency')
	  endif
	  freqref(nconfig) = 1e-9 * freqref(nconfig)
c
c  Determine times and offset times.
c
	  call TabTime(lu,nconfig,timeref,timeoff(nconfig))
c
c  Determine mount type. Miriad insists all the mounts are the same.
c
	  call ftabGeti(lu,'MNTSTA',0,sta)
	  mount(nconfig) = sta(1)
	  badmnt = .false.
	  do i=1,n
	    badmnt = badmnt.or.sta(i).ne.mount(nconfig)
	  enddo
	  if(badmnt)call bug('w',
     *	    'Mount types differed between antennas')
	  emok = emok.and..not.badmnt
c
c  Get the antenna coordinates.
c  Convert to earth-centered coordinates. The AIPS coordinates have X being in the
c  direction from the earth center to the Grennwich meridian, and Z being towards the pole.
c
	  call fitrdhdd(lu,'ARRAYX',xc,0.d0)
	  call fitrdhdd(lu,'ARRAYY',yc,0.d0)
	  call fitrdhdd(lu,'ARRAYZ',zc,0.d0)
	  call ftabGetd(lu,'STABXYZ',0,xyz)
	  call antproc(lefty,xc,yc,zc,xyz,n,antpos(1,nconfig),
     *		lat(nconfig),long(nconfig))
	  llok = .true.
	  call ftabNxt(lu,'AIPS AN',found)
	enddo
c
c  If no antenna table was found, try for an OB table!
c
	if(.not.anfound)then
	found = .false.
	  call ftabLoc(lu,'AIPS OB',found)
	  anfound = found
	  if(found)then
	    call output('Reading AIPS OB table')
c
	    nconfig = 1
	    call ftabInfo(lu,'ORBXYZ',type,units,n,nxyz)
c
	    if(nxyz.ne.3.or.n.le.0.or.type.ne.'D')
     *	      call bug('f','Something is screwy with the OB table')
	    if(n.gt.MAXANT)call bug('f','Too many antennas for me')
c
	    do i=1,MAXANT
	      antloc(i)=0
	    end do
c
	    call ftabGeti(lu,'ANTENNA_NO',0,sta)
c
	    do i=1,n
	      antloc(sta(i))=i
	    enddo
c
	    if(sta(n).ne.n)call bug('w',
     *	      ' Some antennas were missing from the antenna table ')
	    numants = numants + n
	    nants(nconfig) = n
	    freqref(nconfig) = 1e-9*Coord(uvCrval,uvFreq)
	    timeoff(nconfig) = 0
	    mount(nconfig) = 0
	    emok = .false.
	    badmnt = .true.
c
c  Get the antenna coordinates.
c
	    call fitrdhdd(lu,'ARRAYX',xc,0.d0)
	    call fitrdhdd(lu,'ARRAYY',yc,0.d0)
	    call fitrdhdd(lu,'ARRAYZ',zc,0.d0)
	    call ftabGetd(lu,'ORBXYZ',0,xyz)
	    call antproc(lefty,xc,yc,zc,xyz,n,antpos(1,nconfig),
     *		lat(nconfig),long(nconfig))
	    llok = .true.
	  endif
	endif
c
c  Summarise info about the antenna characteristics.
c
	if(nconfig.eq.0)call bug('w',
     *	    'No antenna table was found')
	if(.not.llok)call bug('w',
     *	    'Telescope latitude/longitude could not be determined')
c
c  Load the FQ table if its present.
c
	found = .false.
	call ftabLoc(lu,'AIPS FQ',found)
	if(found)then
	  call ftabInfo(lu,'FRQSEL',type,units,nfreq,nval)
	  if(nfreq.gt.1.and..not.dofq)
     *	    call bug('f','FQ table present for non-multi-freq file')
	  if(nfreq.gt.MAXFREQ)call bug('f','Too many freqs')
	  if(nval.ne.1.or.type.ne.'I')
     *	    call bug('f','Something screwy with FQ table')
	  call fitrdhdi(lu,'NO_IF',itemp,nif)
	  if(itemp.ne.nif)
     *	    call bug('f','Inconsistent number of IFs')
	  call output('Reading AIPS FQ table')
	  call ftabGeti(lu,'FRQSEL',0,freqids)
	  if(.not.dofq)freqids(1) = 1
	  call ftabGetd(lu,'IF FREQ',0,sfreq)
	  call ftabGetr(lu,'CH WIDTH',0,sdf)
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
	    call ftabGeti(lu,'IF NO.',0,freqids)
c
c  Check that the if table is in the standard order.
c
	    do i=1,nif
	      if(freqids(i).ne.i)
     *		call bug('f','Software bug IFNO.ne.IFNO')
	    enddo
c
	    call output('Reading AIPS CH table')
	    nfreq = 1
	    freqids(1) = 1
	    call ftabGetd(lu,'FREQUENCY OFFSET',0,sfreq)
	    do i=1,nif
	      sdf(i) = Coord(uvCdelt,uvFreq)
	    enddo
c
c  If neither a CH or FQ table were found, just use the info in the header.
c
	  else
	    if(dofq)call bug('w',
     *			'Neither an FQ nor CH table were found')
	    nfreq = 1
	    freqids(1) = 1
	    do i=1,nif
	      sfreq(i) = 0
	      sdf(i)   = Coord(uvCdelt,uvFreq)
	    enddo
	  endif
	endif
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
	call Sortie(findx,freqids,nfreq)
c
c  Find and load the SU table. If it was not found, set everything
c  to a default.
c
	found = .false.
	if(dosu)call ftabLoc(lu,'AIPS SU',found)
	if(.not.found)then
	  if(dosu)call bug('w','AIPS SU table not found')
	  nsrc = 1
	  srcids(1) = 1
	  source(1) = defsrc
	  raepo(1) = Coord(uvCrval,uvRa)
	  raapp(1) = Coord(uvCrval,uvRa)
	  decepo(1) = Coord(uvCrval,uvDec)
	  decapp(1) = Coord(uvCrval,uvDec)
	  epoch(1) = defepoch
	  veldop(1) = veldef
	  do i=1,nif
	    freqoff(i) = 0
	    restfreq(i) = rfreq
	  enddo
	else
	  call VelGetty(lu,velsys)
	  call fitrdhdi(lu,'NO_IF',t,nif)
	  if(t.ne.nif)call bug('f','Number of IFs is inconsistent')
	  call ftabInfo(lu,'ID. NO.',type,units,nsrc,nval)
	  if(nsrc.gt.MAXSRC)call bug('f','Too many sources in SU table')
	  if(nval.ne.1.or.type.ne.'I')
     *	    call bug('f','Something screwy with SU table')
	  call output('Reading AIPS SU table')
	  call ftabGeti(lu,'ID. NO.',0,srcids)
	  call ftabGeta(lu,'SOURCE',0,source)
	  call ftabGetd(lu,'RAEPO',0,raepo)
	  call ftabGetd(lu,'DECEPO',0,decepo)
	  call ftabGetd(lu,'RAAPP',0,raapp)
	  call ftabGetd(lu,'DECAPP',0,decapp)
	  call ftabGetd(lu,'EPOCH',0,epoch)
	  call ftabGetd(lu,'FREQOFF',0,freqoff)
	  call ftabGetd(lu,'LSRVEL',0,restfreq)
	  do i=1,nsrc
	    veldop(i) = restfreq((i-1)*nif+1)
	  enddo
	  call ftabGetd(lu,'RESTFREQ',0,restfreq)
	endif
c
c  Check that everything looks OK.
c
	badapp = .false.
	badepo = .false.
	do i=1,nsrc
	  call lcase(source(i))
	  if(nint(epoch(i)+1).eq.0)then
	    epoch(i) = Jul2Epo(timeref,' ')
	  else if(epoch(i).lt.1850.0.or.epoch(i).gt.2150.0)then
	    badepo = .true.
	    epoch(i) = defepoch
	  endif
	  diff = max( abs(raapp(i)-raepo(i)),abs(decapp(i)-decepo(i)) )
	  raepo(i)  = dpi/180 * raepo(i)
	  decepo(i) = dpi/180 * decepo(i)
	  raapp(i)  = dpi/180 * raapp(i)
	  decapp(i) = dpi/180 * decapp(i)
c
c  If the apparent RA and DEC look bad, recompute them.
c
	  eporef = epo2jul(epoch(i),' ')
	  if((diff.gt.1.and.abs(eporef-timeref).gt.1)
     *		.or.3600*diff.lt.1)then
	    badapp = .true.
	    call Precess(eporef, raepo(i),decepo(i),
     *			 timeref,raapp(i),decapp(i))
	    call Nutate(timeref,raapp(i),decapp(i),r0,d0)
	    call Aberrate(timeref,r0,d0,raapp(i),decapp(i))
	  endif
	enddo
c
	if(badepo)call bug('w',
     *	  'Some epochs looked bad -- they were modified')
	if(badapp.and.dosu)call bug('w',
     *	  'Some apparent RA/DECs looked bad -- they were recomputed')
c
c  Scale the source-specific frequencies, rest frequencies and velocities.
c
	do i=1,nif*nsrc
	  freqoff(i) = 1e-9*freqoff(i)
	  restfreq(i) = 1e-9*restfreq(i)
	enddo
	do i=1,nsrc
	  veldop(i) = 1e-3*veldop(i)
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
	call Sortie(sindx,srcids,nsrc)
c
	end
c************************************************************************
	subroutine antproc(lefty,xc,yc,zc,xyz,n,antpos,lat,long)
c
	implicit none
	integer n
	double precision xc,yc,zc,antpos(n,3),lat,long,xyz(3,n)
	logical lefty
c
c  Fiddle the antenna information.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision r,cost,sint,height,temp
	integer i
c
c  Determine the latitude, longitude and height of the first antenna
c  (which is taken to be the observatory lat,long,height). Handle
c  geocentric and local coordinates.
c
	if(abs(xc)+abs(yc)+abs(zc).eq.0)then
	  call xyz2llh(xyz(1,1),xyz(2,1),xyz(3,1),
     *	      lat,long,height)
	else
	  call xyz2llh(xc,yc,zc,
     *	       lat,long,height)
	endif
c
c  Convert them to the Miriad system: y is local East, z is parallel to pole
c  Units are nanosecs.
c
	if(abs(xc)+abs(yc)+abs(yc).eq.0)then
	  r = sqrt(xyz(1,1)*xyz(1,1) + xyz(2,1)*xyz(2,1))
	  cost = xyz(1,1) / r
	  sint = xyz(2,1) / r
	  do i=1,n
	    temp = xyz(1,i)*cost + xyz(2,i)*sint - r
	    antpos(i,1) = (1d9/DCMKS) * temp
	    temp = -xyz(1,i)*sint + xyz(2,i)*cost
	    antpos(i,2) = (1d9/DCMKS) * temp
	    antpos(i,3) = (1d9/DCMKS) * (xyz(3,i)-xyz(3,1))
	  enddo
	else
	  do i=1,n
	    antpos(i,1) = (1d9/DCMKS) * xyz(1,i)
 	    antpos(i,2) = (1d9/DCMKS) * xyz(2,i)
	    antpos(i,3) = (1d9/DCMKS) * xyz(3,i)
	  enddo
	endif
c
c  If the antenna table uses a left-handed system, convert it to a 
c  right-handed system.
c
	if(lefty)then
	  long = -long
	  do i=1,n
	    antpos(i,2) = -antpos(i,2)
	  enddo
	endif
c
	end
c************************************************************************
	subroutine TabVar(tno,var,inttime)
c
	implicit none
	integer tno
	real var,inttime
c
c  Determine the Jyperk from variance, integration time, channel bandwidth
c  and system temperature.
c
c------------------------------------------------------------------------
	include 'fits.h'
	real temp
c
	temp = 50.
	if(systok)temp = systemp
c
	call uvputvrr(tno,'jyperk',sqrt(abs(2*inttime*dnu*var))/temp,1)
	end
c************************************************************************
	subroutine TabRefT0(t)
c
	implicit none
	double precision t
c
c  Return the reference time.
c------------------------------------------------------------------------
	include 'fits.h'
	t = timeref
	if(nconfig.ge.1)t = t + timeoff(1)
	end
c************************************************************************
	subroutine TabTime(lu,nconfig,jdateobs,timeoff)
c
	implicit none
	integer lu,nconfig
	double precision jdateobs,timeoff
c
c  Determine the time correction to add to the FITS time to convert
c  it to a true UT1 time (as best as we can). This involves both
c  an offset between the time system and UT1, and offsets to remove
c  fudges performed by AIPS DBCON.
c------------------------------------------------------------------------
	character timsys*16,line*80
	double precision jrdate,datutc,ut1utc
	integer ltsys
c
c  Externals.
c
	integer len1
	double precision deltime
c
c  Determine the offset times and the time system.
c
	call fitrdhdd(lu,'DATUTC',datutc,0.d0)
	call fitrdhdd(lu,'UT1UTC',ut1utc,0.d0)
	call fitrdhda(lu,'TIMSYS',timsys,'UTC')
	if(timsys.eq.' ')timsys = 'UTC'
	ltsys = len1(timsys)
c
c  Determine the reference time.
c
	call fitdate(lu,'RDATE',jrdate)
	if(jrdate.eq.0)jrdate = jdateobs
c
c  If there is not DATA to UTC time correction present, check if the
c  time is IAT time. If so, work our the time difference.
c
	if((timsys(1:3).eq.'IAT'.or.timsys(1:3).eq.'TAI').and.
     *	  datutc.eq.0) datutc = 24*3600*deltime(jrdate,'tai')
c
c  Give messages about what we are going.
c
	if(timsys(1:2).ne.'UT'.and.datutc.eq.0)then
	  call bug('w','The time offset '//timsys(1:ltsys)//
     *		'-UTC is claimed to be 0.')
	else if(datutc.ne.0)then
	  write(line,'(a,i3,a,f5.1,a)')
     *	    'Decrementing times for configuration',nconfig,' by',datutc,
     *	    ' seconds ('//timsys(1:ltsys)//'-UTC).'
	  call output(line)
	endif
c
	if(ut1utc.ne.0)then
	  write(line,'(a,i3,a,f6.2,a)')
     *	    'Decrementing times for configration',nconfig,' by',-ut1utc,
     *	    ' seconds (UTC-UT1).'
	  call output(line)
	endif
c
	if(nconfig.gt.1)then
	  write(line,'(a,i3,a,i3,a)')
     *	    'Fiddling times for configuration',nconfig,
     *	    ' to remove AIPS DBCON fudges.'
	  call output(line)
	endif
c
	timeoff = (ut1utc - datutc) / (24*3600) 
     *			- jdateobs + jrdate - 5*(nconfig-1)
c
	end
c************************************************************************
	subroutine TabVeloc(altsys,altr,altrval,altrpix)
c
	implicit none
	integer altsys
	logical altr
	real altrpix,altrval
c------------------------------------------------------------------------
	include 'fits.h'
	integer i,i0
	integer OBSRADIO,OBSOPTIC
	parameter(OBSRADIO=3,OBSOPTIC=259)
c
c  Determine the mode that we are using to compute velocities.
c
	if(altsys.eq.OBSRADIO.or.altsys.eq.OBSOPTIC)then
	  velcomp = .false.
	  velsys = altsys
	else if(altr.and.altsys.ne.0)then
	  velcomp = .false.
	  velsys = altsys
	  velref = altrpix
	  do i=1,nsrc
	    veldop(i) = altrval
	  enddo
	else if(altsys.ne.0)then
	  velcomp = .true.
	  velsys = altsys
	else
	  velcomp = .false.
	endif
c
c  Compute the velocity if it is fixed (e.g. the data have been Doppler
c  tracked or the like).
c
	if(.not.velcomp)then
	  do i=1,nsrc
	    i0 = (i-1)*nif + 1
	    call VelCvt(velsys,velref,restfreq(i),
     *		sfreq(1)+freqoff(i0)+freqref(1),sdf(1),veldop(i))
	  enddo
	endif
c
c  Convert the velocity system to a radio one -- as thats the only thing
c  that Miriad supports.
c
	if(velsys.ge.1.and.velsys.le.256)velsys = velsys + 256
c
	end
c************************************************************************
	subroutine VelGetty(lu,velsys)
c
	implicit none
	integer lu,velsys
c
c  Determine the velocity system.
c
c  Input:
c    lu
c  Output:
c    velsys
c------------------------------------------------------------------------
	character string*16
	call fitrdhda(lu,'VELTYP',string,'OBS')
	if(string.eq.' ')string = 'OBS'
	velsys = 0
	if(string(1:3).eq.'OBS')then
	  velsys = 3
	else if(string(1:3).eq.'HEL')then
	  velsys = 2
	else if(string(1:3).eq.'LSR')then
	  velsys = 1
	endif
	call fitrdhda(lu,'VELDEF',string,'RADIO')
	if(string.eq.' ')string = 'RAD'
	if(string(1:3).eq.'RAD')velsys = velsys + 256
	end
c************************************************************************
	subroutine VelCvt(velsys,velref,restfreq,f,df,veldop)
c
	implicit none
	integer velsys
	real velref,df
	double precision restfreq,f,veldop
c
c  Convert a velocity from being the velocity of a channel to the
c  radial velocity of the observatory.
c
c  Input:
c    velsys	Velocity system (1=LSR, 2=HEL, 3=OBS, plus 256 for radio definition)
c    velref	Velocity (km/s) at reference pixel.
c    restfreq	Rest frequency (GHz).
c    f		Frequency (GHz) at channel 1.
c    df		Channel frequency (GHz) increment.
c  Input/Output:
c    veldop	On input, the velocity (km/s) at the reference pixel.
c		On output, the radial velocity of the rest frame (km/s).
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer OBSRADIO,OBSOPTIC,LSRRADIO,LSROPTIC,HELRADIO,HELOPTIC
	parameter(OBSRADIO=259,OBSOPTIC=3,LSRRADIO=257,LSROPTIC=1)
	parameter(HELRADIO=258,HELOPTIC=2)
	double precision fref
c
c  Determine the frequency at the reference channel.
c
	fref = f + (velref-1)*df
c
c  Given this frequency, determine the velocity that the frequency
c  difference corresponds to.
c
	if(velsys.eq.OBSRADIO.or.velsys.eq.OBSOPTIC.or.
     *					restfreq.eq.0)then
	  veldop = 0
	else if(velsys.eq.HELRADIO.or.velsys.eq.LSRRADIO)then
	  veldop = 0.001*CMKS*(restfreq/fref*(1-veldop/(0.001*CMKS))-1)
	else if(velsys.eq.HELOPTIC.or.velsys.eq.LSROPTIC)then
	  veldop = 0.001*CMKS*(restfreq/fref/(1+veldop/(0.001*CMKS))-1)
	else
	  call bug('f','Unrecognised velocity system, in VelCvt')
	endif
	
	end
c************************************************************************
	subroutine telpar(telescop,systemp,systok,jyperk,jok,
     *		latlong,latitude,longitud,polinfo,chioff,mount)
c
	implicit none
	character telescop*(*)
	integer mount
	double precision latitude,longitud
	real chioff,systemp,jyperk
	logical latlong,polinfo,systok,jok
c
c  Determine default characteristics of the observatory (in case
c  no AIPS AN file exists).
c
c  Input:
c    telescop	Telescope name.
c  Output:
c    systok	True if systemp has been initialised.
c    systemp	Typical system temperature.
c    jok	True if jyperk has been initialised.
c    jyperk	System gain.
c    latlong	True if the latitude and longitude are known.
c    latitude)	Observatory latitude and longitude, in radians.
c    longitud)
c    polinfo	True if mount and chioff have been initialised.
c    chioff	The position angle of the X feed with respect to the
c		local vertical.
c    mount	Antenna mount type.
c------------------------------------------------------------------------
	double precision dtemp
	logical ok
c
c  Externals.
c
	character itoaf*8
c
c  Get the info we need from the obspar routine.
c
	if(telescop.eq.' ')then
	  call bug('w','Telescope name was not present in the'//
     *							' FITS file')
	  call bug('w','Unable to guess typical systemp and jyperk')
	  latlong = .false.
	  polinfo = .false.
	  jok = .false.
	  systok = .false.
	else
c
c  System temperature.
c
	  call obspar(telescop,'systemp',dtemp,systok)
	  if(systok)then
	    call output('Assuming systemp='//itoaf(int(dtemp)))
	    systemp = dtemp
	  else
	    call bug('w','Unable to guess typical system temperature')
	  endif
c
c  System gain.
c
	  call obspar(telescop,'jyperk',dtemp,jok)
	  if(jok)then
	    call output('Assuming jyperk='//itoaf(int(dtemp)))
	    jyperk = dtemp
	  else
	    call bug('w','Unable to guess typical system gain')
	  endif
c
c  Latitude, longitude, evector and mount type.
c
	  call obspar(telescop,'latitude',latitude,latlong)
	  call obspar(telescop,'longitude',longitud,ok)
	  latlong = latlong.and.ok
c
c  Mount and evector.
c
	  call obspar(telescop,'evector',dtemp,ok)
	  if(ok)then
	    chioff = dtemp
	  else
	    chioff = 0
	    call output('Assuming feed angle is 0 degrees')
	  endif
	  call obspar(telescop,'mount',dtemp,polinfo)
	  if(polinfo)mount = nint(dtemp)
	endif
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
	subroutine TabInit(tno)
c
	implicit none
	integer tno
c------------------------------------------------------------------------
	include 'fits.h'
	integer i,nschan(MAXIF),ischan(MAXIF)
	character veltype(3)*8
	data veltype/'VELO-LSR','VELO-HEL','VELO-OBS'/
c
	call uvputvri(tno,'nspect',nif,1)
	do i=1,nif
	  ischan(i) = nchan*(i-1) + 1
	  nschan(i) = nchan
	enddo 
	call uvputvri(tno,'ischan',ischan,nif)
	call uvputvri(tno,'nschan',nschan,nif)
c
	if(systok)call uvputvrr(tno,'systemp',systemp,1)
	if(jok)   call uvputvrr(tno,'jyperk',jyperk,1)
c
c  Velocity information.
c
	call uvputvrr(tno,'vsource',0.0,1)
	i = velsys - 256
	if(i.ge.1.and.i.le.3)call uvputvra(tno,'veltype',veltype(i))
c
c  Telescope and observer.
c
	if(telescop.ne.' ')call uvputvra(tno,'telescop',telescop)
	if(observer.ne.' ')call uvputvra(tno,'observer',observer)
c
c  Initialise common variables.
c
	srcid = -1
	freqid = -1
	config = -1
	Tprev = -1
	inited = .true.
c
	end
c************************************************************************
	subroutine TabWrite(tno,sid,fid,confg,time)
c
	implicit none
	integer tno,sid,fid,confg
	double precision time
c
c  Write information about the current source and frequency
c  setup and observatory characteristics.
c
c  Input:
c    tno	Handle of the output Miriad uv file.
c    sid	FITS source id number.
c    fid	FITS frequency id number.
c    confg	FITS configuration number.
c  Input/Output:
c    time	Observing time. Input is the nominal value. Output is the
c		value corrected for clock differences.
c------------------------------------------------------------------------
	integer LSRRADIO
	parameter(LSRRADIO=257)
	include 'fits.h'
	integer i,j,k
	logical newsrc,newfreq,newconfg,newlst,newchi,newvel,neweq
	real chi,dT
	double precision lst,vel
	double precision sfreq0(MAXIF),sdf0(MAXIF),rfreq0(MAXIF)
c
c  Externals.
c
	integer binsrchi
	double precision eqeq
c
	if(.not.inited)call TabInit(tno)
c
	newsrc = srcid.ne.sid
	if(newsrc)then
	  srcid = sid
	  srcidx = binsrchi(srcid,srcids,nsrc)
	  if(srcidx.eq.0)srcidx = 1
	  srcidx = sindx(srcidx)
	  call uvputvri(tno,'fgsrcid',srcid,1)
	endif
c
	newfreq = freqid.ne.fid
	if(newfreq)then
	  freqid = fid
	  freqidx = binsrchi(freqid,freqids,nfreq)
	  if(freqidx.eq.0)freqidx = 1
	  freqidx = findx(freqidx)
	  call uvputvri(tno,'fgfreqid',freqid,1)
	endif
c
	newconfg = config.ne.confg
	config = confg
	if(config.gt.nconfig)config = 1
c
c  Correct the time
c
	if(config.le.nconfig)time = time + timeoff(config)
c
	dT = time - tprev
	tprev = time
c
c  Write out the antenna table and array latitude/longitude, evector, mount.
c
	if(newconfg)then
	  if(config.le.nconfig)then
	    call uvputvri(tno,'fgarray',config,1)
	    call uvputvri(tno,'nants',nants(config),1)
	    call uvputvrd(tno,'antpos',antpos(1,config),3*nants(config))
	  endif
	  if(llok)then
	    call uvputvrd(tno,'latitud',lat(config),1)
	    call uvputvrd(tno,'longitu',long(config),1)
	  endif
	  if(emok)then
	    call uvputvrr(tno,'evector',evec,1)
	    call uvputvri(tno,'mount',mount(config),1)
	    if(mount(config).eq.EQUATOR)call uvputvrr(tno,'chi',evec,1)
	  endif
	endif
c
c  Write out source information.
c
	if(newsrc)then
	  if(.not.velcomp)
     *	    call uvputvrr(tno,'veldop',real(veldop(srcidx)),1)
	  call uvputvrd(tno,'obsra',raapp(srcidx),1)
	  call uvputvrd(tno,'obsdec',decapp(srcidx),1)
	  call uvputvrd(tno,'ra',raepo(srcidx),1)
	  call uvputvrd(tno,'dec',decepo(srcidx),1)
	  if(mosaic)then
	    call uvputvrr(tno,'dra',real(dra(srcidx)),1)
	    call uvputvrr(tno,'ddec',real(ddec(srcidx)),1)
	  endif
	  call uvputvrr(tno,'epoch',real(epoch(srcidx)),1)
	  call uvputvra(tno,'source',source(srcidx))
	endif
c
c  Write out source/frequency information.
c
	if(newsrc.or.newfreq)then
	  i = (srcidx-1)*nif + 1
	  j = (freqidx-1)*nif + 1
	  do k=1,nif
	    rfreq0(k) = restfreq(i)
	    sfreq0(k) = sfreq(j) + freqoff(i) + freqref(config)
	    sdf0(k) =   sdf(j)
	    i = i + 1
	    j = j + 1
	  enddo
	  call uvputvrd(tno,'sfreq',sfreq0,nif)
	  call uvputvrd(tno,'freq',sfreq0,1)
	  call uvputvrd(tno,'sdf',sdf0,nif)
	  call uvputvrd(tno,'restfreq',rfreq0,nif)
	  dnu = sdf0(1)*1e9
	endif
c
c  Recompute the equation of the equinox every day.
c
	neweq = (abs(dT).gt.1)
	if(neweq) eq = eqeq(time)
c
c  Compute and save the local sideral time. Recompute the LST every sec.
c
	newlst = (abs(dT).gt.1./(24.*3600.).or.newconfg).and.llok
	if(newlst)then
	  call jullst(time,long(config),lst)
	  lst = lst + eq
	  call uvputvrd(tno,'lst',lst,1)
	endif
c
c  Compute and save the parallactic angle. Recompute whenever LST changes.
c
	newchi = (newlst.or.newsrc.or.newconfg).and.llok.and.
     *		 emok.and.(mount(config).eq.ALTAZ)
	if(newchi)then
	  call parang(raapp(srcidx),decapp(srcidx),lst,lat(config),chi)
	  call uvputvrr(tno,'chi',chi+evec,1)
	endif
c
c  Compute and save the radial velocity. Compute a new velocity every
c  minute.
c
	newvel = velcomp .and. ( abs(dT).gt.60./(24.*3600.) .or. newsrc
     *		.or. newconfg )
	if(newvel)then
	  if(.not.llok)then
	    call bug('w','Cannot compute velocities ...')
	    call bug('f','Observatory location is not known')
	  endif
	  call VelRad(velsys.eq.LSRRADIO,
     *	    time,raapp(srcidx),decapp(srcidx),
     *	    epoch(srcidx),raepo(srcidx),decepo(srcidx),lst,
     *	    lat(config),vel)
	  call uvputvrr(tno,'veldop',real(vel),1)
	endif
c
	end
c************************************************************************
	subroutine VelRad(dolsr,time,raapp,decapp,epoch,raepo,decepo,
     *	  lst,lat,vel)
c
	implicit none
	logical dolsr
	double precision time,raapp,decapp,epoch,raepo,decepo
	double precision lst,lat,vel
c
c  Compute the radial velocity of the observatory, in the direction of
c  a source, with respect to either LSR or the barycentre.
c
c  Input:
c    dolsr	If true, compute LSR velocity. Otherwise barycentric.
c    time	Time of interest (Julian date).
c    raapp,decapp Apparent RA and DEC (radians).
c    raepo,decepo RA and DEC at the standard epoch (radians).
c    epoch	The standard epoch (years).
c    lat	Observatory geodetic latitude (radians).
c    lst	Local sideral time (radians).
c  Output:
c    vel	Radial velocity.
c------------------------------------------------------------------------
	double precision lmn2000(3),lmnapp(3),ra2000,dec2000
	double precision velsite(3),posearth(3),velearth(3),velsun(3)
	integer i
c
c  Externals.
c
	double precision Epo2jul
c
c  Compute barycentric velocity.
c
	call sph2lmn(raapp,decapp,lmnapp)
	call vsite(lat,lst,velsite)
	call vearth(time,posearth,velearth)
	vel = 0
	do i=1,3
	  vel = vel - (velsite(i) + velearth(i))*lmnapp(i)
	enddo
c
c  To compute LSR velocity, we need the source position in J2000 coordinates.
c  Precess, if necessary, to the J2000 frame. Vsun returns the Suns velocity
c  in the J2000 frame. Add this contribution to the velocity we already have.
c
	if(dolsr)then
	  if(abs(epoch-2000).gt.0.001)then
	    call precess(Epo2jul(epoch,' '),raepo,decepo,
     *		       Epo2jul(2000.d0,'J'),ra2000,dec2000)
	    call sph2lmn(ra2000,dec2000,lmn2000)
	  else
	    call sph2lmn(raepo,decepo,lmn2000)
	  endif
	  call vsun(velsun)
	  do i=1,3
	    vel = vel + lmn2000(i)*velsun(i)
	  enddo
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
	include 'maxdim.h'
	include 'mirconst.h'
	integer uvCrval,uvCdelt,uvCrpix
	integer uvStokes,uvFreq,uvRa,uvDec
	parameter(uvCrval=1,uvCdelt=2,uvCrpix=3)
	parameter(uvStokes=1,uvFreq=2,uvRa=3,uvDec=4)
c
	integer uvU,uvV,uvW,uvBl,uvT,uvSrc,uvRandom,uvData
	parameter(uvU=1,uvV=2,uvW=3,uvBl=4,uvT=5,uvSrc=6,uvRandom=6)
	parameter(uvData=uvRandom+1)
c
	integer Polmin,Polmax,maxPol,PolRR,PolXX
	parameter(PolMin=-8,PolMax=4,maxPol=4,PolXX=-5,PolRR=-1)
c
	complex Data(maxchan)
	logical Flags(maxchan)
	real OutData(uvRandom+1+3*maxchan)
c
	integer i,i0
	integer tIn,tScr,tOut,vSrc
	integer nread,nvis,nVisRef,offset,length,velref,nchan
	integer nSrc,iSrc
	integer ant1,ant2
	real wt,epoch
	double precision pntra,pntdec,restfreq,preamble(5),Coord(3,4)
	double precision f0,df,v0,dv,repsi,fepsi,vepsi,T0
	character string*64,ltype*32,veltype*32,vtype(6)*8
	character observer*32,telescop*32
	integer pols(PolMin:PolMax),P,badpol,npol,Pol0,PolInc
	integer nants,mount
	character polty*2,type*1
	logical updated
	double precision xyz(3*MAXANT),lat,long,height
c
	integer MAXSRC
	parameter(MAXSRC=2048)
	double precision ras(MAXSRC),decs(MAXSRC)
	double precision aras(MAXSRC),adecs(MAXSRC)
	character sources(MAXSRC)*16
c
	integer MAXPARMS
	parameter(MAXPARMS=6)
	integer nparms
	character parms(MAXPARMS)*8
c
c  Externals.
c
	character itoaf*8
        logical uvdatopn
	logical hdprsnt
c
	data parms/'UU      ','VV      ','WW      ',
     *		   'BASELINE','DATE    ','SOURCE  '/
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
	call uvVarIni(tin,vSrc)
	call uvVarSet(vSrc,'source')
	call uvVarSet(vSrc,'dra')
	call uvVarSet(vSrc,'ddec')
	call scropen(tScr)
c
c  Read through the input, determining the min and max values of all the
c  parameters that we need.
c
	nSrc = 0
	iSrc = 0
	ras(1) = 0
	decs(1) = 0
	sources(1) = ' '
c
	call uvdatrd(preamble,data,flags,maxchan,nread)
	if(nread.eq.0)call bug('f','No data to write out!')
	call SrcUpd(tIn,vSrc,iSrc,ras,decs,aras,adecs,sources,
     *							nSrc,MAXSRC)
	call uvrdvrd(tIn,'pntra',pntra,ras(1))
	call uvrdvrd(tIn,'pntdec',pntdec,decs(1))
        nchan = nread
c
	nvis = 0
	length = uvRandom + 1 + 3*nchan
	offset = 0
c
c  Get things which define the coordinate system.
c
	call uvrdvrr(tIn,'epoch',epoch,1950.)
	call uvfit2(tIn,'sfreq',nread,df,f0,fepsi)
	if(fepsi.gt.0.1*abs(df))call bug('w',
     *	    'Channel frequencies deviated by > 10% from linearity')
	if(nread.eq.1)call uvfit1(tIn,'bandwidth',nread,df,fepsi)
	f0 = 1e9 * f0
	df = 1e9 * df
c
	call uvdatgta('ltype',ltype)
	if(ltype.eq.'wide')then
	  velref = 0
	  restfreq = 0
	else
	  call uvfit1(tIn,'restfreq',nread,restfreq,repsi)
	  if(repsi.gt.0.001*restfreq) call bug('w',
     *	    'Rest frequencies varied between channels by > 0.1%')
	  restfreq = 1e9 * restfreq
	  if(restfreq.gt.0)then
	    call uvrdvra(tIn,'veltype',veltype,'VELO-LSR')
	    call uvfit2(tIn,'velocity',nread,dv,v0,vepsi)
	    v0 = 1000.d0 * v0
	    if(vepsi.gt.0.1*abs(dv))call bug('w',
     *	      'Channel velocities deviate by > 10% from linearity')
	    velref = 0
	    do i=1,6
	      if(veltype.eq.vtype(i)) velref = i
	    enddo
	    if(velref.gt.3) velref = velref - 3 + 256
	  endif
	endif
c
c  Get other book keeping.
c
	call uvrdvra(tIn,'telescop',telescop,' ')
	call uvrdvra(tIn,'observer',observer,' ')
c
c  Set the reference date.
c
	T0 = preamble(4)
	T0 = int(T0 - 0.5d0) + 0.5d0
c
c  Load antenna table information.
c
	call uvprobvr(tIn,'antpos',type,nants,updated)
	if(mod(nants,3).ne.0)
     *	  call bug('f','Antpos variable looks bad')
	nants = nants/3
	if(nants.gt.MAXANT)nants = 0
	if(nants.gt.0)then
	  call uvgetvrd(tIn,'antpos',xyz,3*nants)
	  call getarr(tIn,mount,lat,long,height)
	endif
c
c  Read the data. Check that we are dealing with a single pointing.
c  If the polarisation code is OK, do some conversions, and write to
c  a scratch file.
c
	badpol = 0
	dowhile(nread.eq.nchan)
	  call uvdatgti('pol',P)
	  if(P.ge.PolMin.and.P.le.PolMax.and.P.ne.0)then
	    nvis = nvis + 1
	    call uvdatgtr('variance',wt)
	    if(wt.eq.0)then
	      call uvrdvrr(tIn,'inttime',wt,1.)
	    else
	      wt = 1/wt
	    endif
c
c  Convert baseline number to normal AIPS form. Note the different conventions!
c
c  Miriad convention is that bl = 256*ant1 + ant2, where the baseline is ant2 - ant1
c  AIPS                      bl = 256*ant1 + ant2                        ant1 - ant2 !!
c
c  In both cases ant1 is normally less than ant2.
c
	    call Basant(preamble(5),ant1,ant2)
	    pols(P) = pols(P) + 1
c
	    OutData(uvU+1) = -1e-9 * preamble(1)
	    OutData(uvV+1) = -1e-9 * preamble(2)
	    OutData(uvW+1) = -1e-9 * preamble(3)
	    OutData(uvT+1) = preamble(4) - T0
	    OutData(uvBl+1) = 256*ant1 + ant2
	    OutData(uvSrc+1) = iSrc
	    OutData(1) = P
	    i0 = uvData
	    do i=1,nchan
	      OutData(i0+1) = real(Data(i))
	      OutData(i0+2) = -aimag(Data(i))
	      if(flags(i))then
	        OutData(i0+3) = wt
	      else
	        OutData(i0+3) = -wt
	      endif
	      i0 = i0 + 3
	    enddo
	    call scrwrite(tScr,OutData,offset,length)
	    offset = offset + length
	  else
	    badpol = badpol + 1
	  endif
	  call uvdatrd(preamble,data,flags,maxchan,nread)
	  call SrcUpd(tIn,vSrc,iSrc,ras,decs,aras,adecs,sources,
     *							nSrc,MAXSRC)
	enddo
c
c  Summarise what we read.
c
	if(badpol.gt.0) call bug('w',
     *	  'Visibilities with bad pol codes: '//itoaf(badpol))
	if(nread.gt.0) call bug('f','Bad number of channels')
	if(nvis.le.0)  call bug('f','No visibilities found')	
c
c  Determine the polarisations that we are going to output.
c
	call PolCheck(pols,PolMin,PolMax,npol,Pol0,PolInc)
	if(npol.gt.maxPol)
     *	  call bug('f','Too many polarisations for me to handle')
	if(pol0.le.PolXX)then
	  polty = 'XY'
	else if(pol0.le.PolRR)then
	  polty = 'RL'
	else
	  polty = '  '
	endif
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
	Coord(uvCrval,uvRa) = 180.d0/dpi * ras(1)
	Coord(uvCdelt,uvRa) = 1
	Coord(uvCrpix,uvRa) = 1
	Coord(uvCrval,uvDec) = 180.d0/dpi * decs(1)
	Coord(uvCdelt,uvDec) = 1
	Coord(uvCrpix,uvDec) = 1
c
c  Open the FITS file and write out some info. NOTE that a bug in AIPS
c  FITLD requires that the OBJECT keyword be written out as early
c  as possible.
c
	call fuvopen(tOut,out,'new',nVisRef,npol,nchan)
	call fuvSetT0(tOut,T0)
	nparms = 5
	if(nSrc.gt.1)nparms = 6
	call fuvSetPa(tOut,nparms,parms)
	call fuvWrhd(tOut,Coord)
	if(sources(1).ne.' ')call fitwrhda(tOut,'OBJECT',sources(1))
	call fitwrhdd(tOut,'OBSRA', 180.0d0/DPI*pntra)
	call fitwrhdd(tOut,'OBSDEC',180.0d0/DPI*pntdec)
	call fitwrhdr(tOut,'EPOCH',epoch)
c
c  Spectral line and velocity information.
c
	if(restfreq.gt.0)then
	  call fitwrhdd(tOut,'RESTFREQ',restfreq)
	  if(velref.gt.0)call fitwrhdi(tOut,'VELREF',velref)
	  call fitwrhdr(tOut,'ALTRPIX',1.0)
	  call fitwrhdr(tOut,'ALTRVAL',real(v0))
	endif
c
c  Other miscellaneous information.
c
	if(telescop.ne.' ')then
	  call fitwrhda(tOut,'TELESCOP',telescop)
	  call fitwrhda(tOut,'INSTRUME',telescop)
	endif
	if(observer.ne.' ')call fitwrhda(tOut,'OBSERVER',observer)
	string = 'Miriad '//version
	call fitwrhda(tOut,'ORIGIN',string)
c
c  Copy the history.
c
	if (hdprsnt(tIn,'history ')) then
	  call CopyHist(tIn,tOut,version)
	end if
c
c  We now have all the data we want in a scratch file. Copy this
c  data to the output FITS file.
c
	call uvoutWr(tScr,tOut,nvis,nVisRef,npol,nchan,Pol0,PolInc,
     *	  nSrc.gt.1)
c
c  Write the source file.
c
	if(nSrc.gt.1)call SrcWrite(tOut,nSrc,sources,
     *	  ras,decs,aras,adecs,epoch,restfreq,df,v0)
c
c  Write the antenna file.
c
	if(nants.gt.0)then
	  call output('Writing FITS antenna table')
	  call AntWrite(tOut,t0,f0,telescop,polty,mount,
     *			xyz,nants,lat,long,height)
	else
	  call bug('w','Insufficent information for antenna table')
	endif
c
c  Everything is done. Close up shop.
c
	call uvdatcls
	call scrclose(tScr)
	call fuvclose(tOut)
c
	end
c************************************************************************
	subroutine SrcUpd(tIn,vSrc,iSrc,ras,decs,aras,adecs,
     *						sources,nSrc,MAXSRC)
c
	integer tIn,vSrc,iSrc,nSrc,MAXSRC
	double precision ras(MAXSRC),decs(MAXSRC)
	double precision aras(MAXSRC),adecs(MAXSRC)
	character sources(MAXSRC)*(*)
c
c------------------------------------------------------------------------
	character source*32
	integer i
	double precision ra,dec,dra,ddec
c
c  Externals.
c
	logical uvVarUpd
c
c  Change in source. Get the source name, ra and dec.
c
	if(uvVarUpd(vSrc).or.nSrc.eq.0)then
	  call uvrdvra(tIn,'source',source,' ')
	  if(len(source).gt.len(sources(1)))
     *	    source(len(sources(1))+1:) =  ' '
	  call uvrdvrd(tIn,'ra',ra,0.d0)
	  call uvrdvrd(tIn,'dec',dec,0.d0)
	  call uvrdvrd(tIn,'dra',dra,0.d0)
	  call uvrdvrd(tIn,'ddec',ddec,0.d0)
	  ra = ra + dra/cos(dec)
	  dec = dec + ddec
	  iSrc = 0
	  i = 1
	  dowhile(i.le.nSrc.and.iSrc.eq.0)
	    if(ras(i).eq.ra.and.decs(i).eq.dec.and.
     *	      sources(i).eq.source)iSrc = i
	    i = i + 1
	  enddo
c
	  if(iSrc.eq.0)then
	    nSrc = nSrc + 1
	    iSrc= nSrc
	    if(nSrc.gt.MAXSRC)call bug('f','Source table overflow')
	    sources(iSrc) = source
	    ras(iSrc) = ra
	    decs(iSrc) = dec
	    call uvrdvrd(tIn,'obsra',aras(iSrc),ras(iSrc))
	    call uvrdvrd(tIn,'obsdec',adecs(iSrc),decs(iSrc))
	  endif
	endif
c
	end
c************************************************************************
	subroutine getarr(tIn,mount,lat,long,height)
c
	implicit none
	integer tIn,mount
	double precision lat,long,height
c------------------------------------------------------------------------
	character telescop*32,type*1
	double precision dval
	integer n
	logical updated,ok
c
c  Determine the telescope.
c
	call uvrdvra(tin,'telescop',telescop,' ')
c
c  Determine the mount.
c
	call uvprobvr(tIn,'mount',type,n,updated)
	ok = n.eq.1
	if(ok)then
	  call uvrdvri(tIn,'mount',mount,0)
	else if(telescop.ne.' ')then
	  call obspar(telescop,'mount',dval,ok)
	  if(ok)mount = nint(dval)
	endif
	if(.not.ok)then
	  call bug('w',
     *	    'Antenna mount could not be determined -- assuming alt/az')
	  mount = 0
	endif
c
c  Determine the latitude.
c
	call uvprobvr(tIn,'latitud',type,n,updated)
	ok = n.eq.1
	if(ok)then
	  call uvrdvrd(tIn,'latitud',lat,0.d0)
	else if(telescop.ne.' ')then
	  call obspar(telescop,'latitude',lat,ok)
	endif
	if(.not.ok)then
	  lat = 0.d0
	  call bug('w','Telescope latitude could not be determined')
	endif
c
c  Determine the longitude.
c
	call uvprobvr(tIn,'longitu',type,n,updated)
	ok = n.eq.1
	if(ok)then
	  call uvrdvrd(tIn,'longitu',long,0.d0)
	else if(telescop.ne.' ')then
	  call obspar(telescop,'longitude',long,ok)
	endif
	if(.not.ok)then
	  call bug('w','Telescope longitude could not be determined')
	  long = 0.d0
	endif
c
c  Determine the height.
c
	call uvprobvr(tIn,'height',type,n,updated)
	ok = n.eq.1
	if(ok)then
	  call uvrdvrd(tIn,'height',height,0.d0)
	else if(telescop.ne.' ')then
	  call obspar(telescop,'height',height,ok)
	endif
	if(.not.ok)height = 0.d0
c
	end
c************************************************************************
	subroutine SrcWrite(tOut,nSrc,sources,ras,decs,aras,adecs,epoch,
     *	  restfreq,df,v0)
c
	implicit none
	integer tOut,nSrc
	double precision ras(nSrc),decs(nSrc),aras(nSrc),adecs(nSrc)
	double precision restfreq,df,v0
	character sources(nSrc)*(*)
	real epoch
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i
c
	call ftabdini(tOut,'AIPS SU')
	call ftabdef(tOut,'ID. NO.',  'I',' ',      nSrc,1)
	call ftabdef(tOut,'SOURCE',   'A',' ',      nSrc,
     *						len(sources(1)))
	call ftabdef(tOut,'QUAL',     'I',' ',      nSrc,1)
	call ftabdef(tOut,'CALCODE',  'A',' ',      nSrc,4)
	call ftabdef(tOut,'IFLUX',    'R','JY',     nSrc,1)
	call ftabdef(tOut,'QFLUX',    'R','JY',     nSrc,1)
	call ftabdef(tOut,'UFLUX',    'R','JY',     nSrc,1)
	call ftabdef(tOut,'VFLUX',    'R','JY',     nSrc,1)
	call ftabdef(tOut,'FREQOFF',  'D','HZ',     nSrc,1)
	call ftabdef(tOut,'BANDWIDTH','D','HZ',     nSrc,1)
	call ftabdef(tOut,'RAEPO',    'D','DEGREES',nSrc,1)
	call ftabdef(tOut,'DECEPO',   'D','DEGREES',nSrc,1) 
	call ftabdef(tOut,'EPOCH',    'D','YEARS',  nSrc,1)
	call ftabdef(tOut,'RAAPP',    'D','DEGREES',nSrc,1)
	call ftabdef(tOut,'DECAPP',   'D','DEGREES',nSrc,1)
	call ftabdef(tOut,'LSRVEL',   'D','M/SEC',  nSrc,1)
	call ftabdef(tOut,'RESTFREQ', 'D','HZ',     nSrc,1)
	call ftabdef(tOut,'PMRA',     'D','DEG/DAY',nSrc,1)
	call ftabdef(tOut,'PMDEC',    'D','DEG/DAY',nSrc,1)
	call ftabdfin(tOut)
c
	call fitwrhdi(tOut,'NO_IF',1)
	call fitwrhda(tOut,'VELTYP','OBS')
	call fitwrhda(tOut,'VELDEF','RADIO')
c
	do i=1,nSrc
	  call ftabputi(tOut,'ID. NO.',  i,i)
	  call ftabputa(tOut,'SOURCE',   i,sources(i))
	  call ftabputi(tOut,'QUAL',     i,0)
	  call ftabputa(tOut,'CALCODE',  i,'    ')
	  call ftabputr(tOut,'IFLUX',    i,0.0)
	  call ftabputr(tOut,'QFLUX',    i,0.0)
	  call ftabputr(tOut,'UFLUX',    i,0.0)
	  call ftabputr(tOut,'VFLUX',    i,0.0)
	  call ftabputd(tOut,'FREQOFF',  i,0.d0)
	  call ftabputd(tOut,'BANDWIDTH',i,df)
	  call ftabputd(tOut,'RAEPO',    i,180.d0/DPI*ras(i))
	  call ftabputd(tOut,'DECEPO',   i,180.d0/DPI*decs(i))
	  call ftabputd(tOut,'EPOCH',    i,dble(epoch))
	  call ftabputd(tOut,'RAAPP',    i,180.d0/DPI*aras(i))
	  call ftabputd(tOut,'DECAPP',   i,180.d0/DPI*adecs(i))
	  call ftabputd(tOut,'LSRVEL',   i,v0)
	  call ftabputd(tOut,'RESTFREQ', i,restfreq)
	  call ftabputd(tOut,'PMRA',     i,0.d0)
	  call ftabputd(tOut,'PMDEC',    i,0.d0)
	enddo
c
	end
c************************************************************************
	subroutine AntWrite(tOut,rtime,rfreq,telescop,polty,mount,
     *			xyz,nants,lat,long,height)
c
	implicit none
	integer tOut,nants,mount
	double precision rtime,rfreq
	double precision xyz(nants,3),lat,long,height
	character telescop*(*),polty*(*)
c
c  Write an antenna table into the output.
c------------------------------------------------------------------------
	include 'mirconst.h'
c
	real zero(3)
	character anname*8,rdate*32
	double precision iatutc,gstia0,gstia1,degpdy,xyzd(3)
	integer i
c
c  Externals.
c
	character itoaf*4
	double precision deltime,eqeq
c
	call ftabdini(tOut,'AIPS AN')
	call ftabdef(tOut,'ANNAME', 'A',' ',      nants,len(anname))
	call ftabdef(tOut,'STABXYZ','D','METERS', nants,3)
	call ftabdef(tOut,'ORBPARM','D',' ',      nants,0)
	call ftabdef(tOut,'NOSTA',  'I',' ',      nants,1)
	call ftabdef(tOut,'MNTSTA', 'I',' ',      nants,1)
	call ftabdef(tOut,'STAXOF', 'R','METERS', nants,1)
	call ftabdef(tOut,'POLTYA', 'A',' ',      nants,1)
	call ftabdef(tOut,'POLAA',  'R','DEGREES',nants,1)
	call ftabdef(tOut,'POLCALA','R',' ',      nants,3)
	call ftabdef(tOut,'POLTYB', 'A',' ',      nants,1)
	call ftabdef(tOut,'POLAB',  'R','DEGREES',nants,1)
	call ftabdef(tOut,'POLCALB','R',' ',      nants,3)
	call ftabdfin(tOut)
c
c  Determine various things to do with time.
c
	iatutc = deltime(rtime,'tai')
	call jullst(rtime-iatutc,0.d0,gstia0)
	gstia0 = 180d0/DPI * (gstia0 + eqeq(rtime-iatutc))
	if(gstia0.lt.0)  gstia0 = gstia0 + 360
	if(gstia0.ge.360)gstia0 = gstia0 - 360
	call jullst(rtime-iatutc+1.d0,0.d0,gstia1)
	gstia1 = 180d0/DPI * (gstia1 + eqeq(rtime-iatutc+1.d0))
	if(gstia1.lt.0)  gstia1 = gstia1 + 360
	if(gstia1.ge.360)gstia1 = gstia1 - 360
	degpdy = gstia1 - gstia0 + 360
	if(degpdy.lt.360)degpdy = degpdy + 360
c
c  Fill out information in the antenna table header.
c
	call llh2xyz(lat,long,height,xyzd(1),xyzd(2),xyzd(3))
	call fitwrhdd(tOut,'ARRAYX',xyzd(1))
	call fitwrhdd(tOut,'ARRAYY',xyzd(2))
	call fitwrhdd(tOut,'ARRAYZ',xyzd(3))
	call fitwrhdd(tOut,'GSTIA0',gstia0)
	call fitwrhdd(tOut,'DEGPDY',degpdy)
	call fitwrhdd(tOut,'FREQ',  rfreq)
	call julday(rtime,'T',rdate)
	call fitwrhda(tOut,'RDATE',rdate)
	call fitwrhdd(tOut,'POLARX',0.d0)
	call fitwrhdd(tOut,'POLARY',0.d0)
	call fitwrhdd(tOut,'UT1UTC',0.d0)
	call fitwrhdd(tOut,'DATUTC',0.d0)
	call fitwrhda(tOut,'TIMSYS','UTC')
	call fitwrhda(tOut,'ARRNAM',telescop)
	call fitwrhdi(tOut,'NUMORB',0)
	call fitwrhdi(tOut,'NOPCAL',3)
	call fitwrhdi(tOut,'FREQID',-1)
	call fitwrhdd(tOut,'IATUTC',86400d0*iatutc)
c
c  Zero out the unused fields.
c
	zero(1) = 0
	zero(2) = 0
	zero(3) = 0
	do i=1,nants
	  anname = 'ANT'//itoaf(i)
	  call ftabputa(tOut,'ANNAME', i,anname)
	  xyzd(1) = DCMKS*1d-9*xyz(i,1)
	  xyzd(2) = DCMKS*1d-9*xyz(i,2)
	  xyzd(3) = DCMKS*1d-9*xyz(i,3)
	  call ftabputd(tOut,'STABXYZ',i,xyzd)
	  call ftabputi(tOut,'NOSTA',  i,i)
	  call ftabputi(tOut,'MNTSTA', i,mount)
	  call ftabputr(tOut,'STAXOF', i,0.0)
	  call ftabputa(tOut,'POLTYA', i,polty(1:1))
          if (telescop.eq.'ATCA') then
   	     call ftabputr(tOut,'POLAA',  i, 45.0)
	     call ftabputr(tOut,'POLAB',  i, 135.0)
          else 
   	     call ftabputr(tOut,'POLAA',  i, 0.0)
	     call ftabputr(tOut,'POLAB',  i, 0.0)
          end if
	  call ftabputr(tOut,'POLCALA',i,zero)
	  call ftabputa(tOut,'POLTYB', i,polty(2:2))

	  call ftabputr(tOut,'POLCALB',i,zero)
	enddo
c
	end
c************************************************************************
	subroutine uvoutWr(tScr,tOut,nvis,nVisRef,npol,nchan,Pol0,
     *							PolInc,doSrc)
c
	integer tScr,tOut,nvis,nVisRef,npol,nchan,Pol0,PolInc
	logical doSrc
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
c    doSrc	True if we should write the source number.
c------------------------------------------------------------------------
c  Records are copied or discarded according to whether the reference
c  polarisation is present or not. If not, the record is discarded. Currently
c  the reference polarisation is always the first polarisation, but this may
c  change in the future.
c
	include 'maxdim.h'
	integer maxPol,RefPol,PolMin,PolMax
	parameter(RefPol=1,PolMin=-8,PolMax=4,maxPol=4)
	integer uvU,uvV,uvW,uvBl,uvT,uvSrc,uvRandom,uvData
	parameter(uvU=1,uvV=2,uvW=3,uvBl=4,uvT=5,uvSrc=6,uvRandom=6)
	parameter(uvData=uvRandom+1)
c
	integer pols(PolMin:PolMax),discard(PolMin:PolMax),pnt(maxPol)
	integer ncopy,totcopy,l,l2,InPnt,OutPnt,Bl,P,iP,i,j,jd,k,length
	integer iSrc
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
	iSrc = 0
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
	    if(doSrc)then
	      call fuvwrite(tOut,in(2),jd,1)
	    else
	      in(7) = in(6)
	      in(6) = in(5)
	      in(5) = in(4)
	      in(4) = in(3)
	      in(3) = in(2)
	      call fuvwrite(tOut,in(3),jd,1)
	    endif
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
     *	       nint(In(uvSrc+1)).ne.iSrc.or.
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
	      Out(uvSrc) = In(uvSrc+1)
	      Bl = nint(In(uvBl+1))
	      Time = In(uvT+1)
	      iSrc = nint(In(uvSrc+1))
	      Wt = abs(In(uvData+3))
	    endif
c
c  We have to add visibilities to the currently existing record.
c
	    InPnt = uvData
	    OutPnt = uvRandom - 1 + 3*(iP-1)
	    if(doSrc)OutPnt = OutPnt + 1
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
	subroutine PolCheck(pols,PolMin,PolMax,npol,Pol0,PolInc)
c
	implicit none
	integer PolMin,PolMax,pols(PolMin:PolMax),npol,Pol0,PolInc
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
c    PolMin
c    PolMax
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
	subroutine xyin(in,out,version,dss,nod2)
c
	implicit none
	character in*(*),out*(*),version*(*)
	logical dss,nod2
c
c  Read in an image FITS file.
c
c  Inputs:
c    in		Name of the input image FITS file.
c    out	Name of the output MIRIAD file.
c    dss	Expect a DSS image. Handle header accordingly!
c    nod2	Expect a NOD2 image.
c
c  Internal Variables:
c    lu		Handle of the FITS file.
c    tno	Handle of the MIRIAD file.
c    array	Array to use as buffer.
c    maxdim	Size of array.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	real array(MAXDIM)
	logical allgood,doflag,flags(MAXDIM)
	integer nsize(MAXNAX),axes(MAXNAX),naxis
	integer lu,tno,i,j,iostat
c
c  Externals.
c
	logical Inc3More,FitBlank
c
c  Open the input FITS and output MIRIAD files.
c
	call fxyopen(lu,in,'old',MAXNAX,nsize)
	doflag = FitBlank(lu,.false.)
	if(nsize(1).gt.maxdim)
     *	  call bug('f','Image too big to handle')
	call fitrdhdi(lu,'NAXIS',naxis,0)
	if(naxis.le.0)call bug('f','Weird bug')
	naxis = min(naxis,MAXNAX)
	call xyopen(tno,out,'new',naxis,nsize)
	call hisopen(tno,'append')
	call histin(lu,tno,version)
c
c  Copy the image data itself.
c
	allgood = .true.
	call IncIni(naxis,nsize,axes)
	dowhile(Inc3More(naxis,nsize,axes))
	  if(naxis.gt.2)then
	    call fxysetpl(lu,naxis-2,axes(3))
	    call xysetpl(tno,naxis-2,axes(3))
	  endif
	  do j=1,nsize(2)
	    call fxyread(lu,j,array)
	    call xywrite(tno,j,array)
c
c  Handle the flags.
c
	    if(doflag)then
	      call fxyflgrd(lu,j,flags)
	      call xyflgwr(tno,j,flags)
	      if(allgood)then
		do i=1,nsize(1)
		  allgood = allgood.and.flags(i)
		enddo
	      endif
	    endif
c
	  enddo
	enddo
c
c  Handle the header.
c
	call axisin(lu,tno,naxis)
	if(dss)call dssfudge(lu,tno)
	if(nod2)call nodfudge(lu,tno)
c
c  Close up shop.
c
	call fxyclose(lu)
	call hisclose(tno)
	call xyclose(tno)
c
c  FUDGE!!! Delete the flagging mask if all the flags were good.
c
	if(doflag)then
	  if(allgood)then
	    call hopen(tno,out,'old',iostat)
	    if(iostat.ne.0)call bugno('f',iostat)
	    call hdelete(tno,'mask',iostat)
	    if(iostat.ne.0)call bugno('f',iostat)
	    call hclose(tno)
	    call output('There were no blanked pixels in the input')
	  else
	    call output(
     *	     'Some pixels were blanked ... A blanking mask was created')
	  endif
	else
	  call output(
     *	     'There was no blanking information in the input')
	endif
c
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
	include 'maxnax.h'
	integer i,polcode,nx,ny,ilong,ilat
	character num*2,bunit*32,types(5)*25,btype*32
	character telescop*16,atemp*16,observer*16,cellscal*16
	character object*32,pbtype*16
	real bmaj,bmin,bpa,epoch,equinox,rms,vobs,pbfwhm
	double precision restfreq,obsra,obsdec,dtemp
	double precision llrot
	double precision cdelt(MAXNAX),crval(MAXNAX),crpix(MAXNAX)
	character ctype(MAXNAX)*8
c
c  Externals.
c
	character itoaf*2
	double precision epo2jul
c
c                   1234567890123456789012345
	data types/'polarized_intensity      ',
     *		   'fractional_polarization  ',
     *		   'position_angle           ',
     *		   'spectral_index           ',
     *		   'optical_depth            '/
c
c  Attempt to determine observer and telescope type.
c
	call fitrdhda(lu,'USER',atemp,' ')
	call fitrdhda(lu,'OBSERVER',observer,atemp)
	if(observer.ne.' ')call wrhda(tno,'observer',observer)
	call fitrdhda(lu,'INSTRUME',atemp,' ')
	call fitrdhda(lu,'TELESCOP',telescop,atemp)
	if(telescop.ne.' ')call wrhda(tno,'telescop',telescop)
	call fitrdhda(lu,'OBJECT',object,' ')
	if(object.ne.' ')call wrhda(tno,'object',object)
c
	call fitrdhda(lu,'BUNIT',bunit,' ')
	call fitrdhda(lu,'BTYPE',btype,' ')
c
	call fitrdhdr(lu,'rms',rms,0.)
	if(rms.gt.0)call wrhdr(tno,'rms',rms)
	call fitrdhdr(lu,'pbfwhm',pbfwhm,0.)
	atemp = telescop
	if(pbfwhm.ne.0)call pbEncode(atemp,'gaus',PI/180/3600*pbfwhm)
	call fitrdhda(lu,'pbtype',pbtype,atemp)
c
c  Get the axis coordinate information.
c
	call cord(lu,naxis,ctype,crval,crpix,cdelt,llrot,ilong,ilat)
c
c  Handle the OBSRA and OBSDEC keywords.
c
	if(ilong.ne.0.and.ilat.ne.0)then
	  if(ctype(ilong)(1:5).eq.'RA---'.and.
     *	     ctype(ilat )(1:5).eq.'DEC--')then
	    call fitrdhdd(lu,'OBSRA' ,obsra, crval(ilong))
	    call fitrdhdd(lu,'OBSDEC',obsdec,crval(ilat))
	    if(abs(obsra-crval(ilong)).gt.abs(cdelt(ilong)).or.
     *	       abs(obsdec-crval(ilat)).gt.abs(cdelt(ilat)))then
	      call fitrdhdi(lu,'NAXIS'//itoaf(ilong),nx,0)
	      call fitrdhdi(lu,'NAXIS'//itoaf(ilat), ny,0)
              call output('Phase and pointing centers differ ...'//
     *					' saving pointing information')
	      call mosInit(nx,ny)
	      if(rms.le.0)rms = 1
	      call mosSet(1,DPI/180d0*obsra,DPI/180d0*obsdec,rms,pbtype)
	      call mosSave(tno)
	    else if(pbtype.ne.telescop)then
	      call wrhda(tno,'pbtype',pbtype)
	    endif
	  endif
	endif
c
c  Perform unit conversion of coordinate system and write them out.
c
	do i=1,naxis
	  num = itoaf(i)
	  if(ctype(i)(1:5).eq.'RA---'.or.ctype(i)(1:5).eq.'DEC--'.or.
     *	     ctype(i)(1:5).eq.'GLON-'.or.ctype(i)(1:5).eq.'GLAT-'.or.
     *	     ctype(i)(1:5).eq.'ELON-'.or.ctype(i)(1:5).eq.'ELAT-'.or.
     *	     ctype(i).eq.'ANGLE')then
	    crval(i) = DPI/180.d0 * crval(i)
	    cdelt(i) = DPI/180.d0 * cdelt(i)
          else if(ctype(i)(1:4).eq.'VELO'.or.
     *		  ctype(i)(1:4).eq.'FELO')then
	    crval(i) = 1d-3 * crval(i)
	    cdelt(i) = 1d-3 * cdelt(i)
          else if(ctype(i)(1:4).eq.'FREQ')then
	    crval(i) = 1d-9 * crval(i)
	    cdelt(i) = 1d-9 * cdelt(i)
	  else if(ctype(i).eq.'STOKES')then
	    polcode = nint(crval(i)+(1-crpix(i))*cdelt(i))
c  dpr 27-nov-00 ->
	    if(polcode.lt.-8.or.polcode.gt.4.or.polcode.eq.0)then
c  <- dpr 27-nov-00 
	      ctype(i) = 'ASTOKES'
	      if(polcode.ge.5.and.polcode.le.9)btype=types(polcode-4)
	    endif
	  endif
c
	  if(ctype(i).ne.' ')call wrhda(tno,'ctype'//num,ctype(i))
	  call wrhdd(tno,'crval'//num,crval(i))
	  call wrhdd(tno,'cdelt'//num,cdelt(i))
	  call wrhdd(tno,'crpix'//num,crpix(i))
	enddo
c
	if(llrot.ne.0)then
	  call wrhdd(tno,'llrot',llrot)
	  call bug('w','This image has a sky rotation')
	  call bug('w','Sky rotations are poorly supported by Miriad')
	endif
c
c  Determine dates.
c
	call geteqep(lu,'EPOCH',epoch)
	call geteqep(lu,'EQUINOX',equinox)
	if(epoch.lt.1800)epoch = equinox
	if(equinox.lt.1800)equinox = epoch
	if(equinox.gt.1800)call wrhdr(tno,'epoch',equinox)
c
	call fitdate(lu,'DATE-OBS',dtemp)
	if(dtemp.le.0.and.epoch.gt.1800)dtemp = Epo2Jul(dble(epoch),' ')
	if(dtemp.gt.0)call wrhdd(tno,'obstime',dtemp)
c
	if(bunit.ne.' ')call wrhda(tno,'bunit',bunit)
	if(btype.ne.' ')call wrbtype(tno,btype)
	call fitrdhdr(lu,'VOBS',vobs,0.)
	if(vobs.ne.0)call wrhdr(tno,'vobs',vobs)
	call fitrdhdd(lu,'RESTFREQ',restfreq,-1.d0)
	if(restfreq.gt.0) call wrhdd(tno,'restfreq',1.0d-9*restfreq)
	call fitrdhda(lu,'CELLSCAL',cellscal,'CONSTANT')
	if(cellscal.ne.' ')call wrhda(tno,'cellscal',cellscal)
	call fitrdhdr(lu,'BMAJ',bmaj,0.)
	if(bmaj.ne.0) call wrhdr(tno,'bmaj',(pi/180)*bmaj)
	call fitrdhdr(lu,'BMIN',bmin,0.)
	if(bmin.ne.0) call wrhdr(tno,'bmin',(pi/180)*bmin)
	call fitrdhdr(lu,'BPA',bpa,0.)
	if(bmaj*bmin.ne.0)call wrhdr(tno,'bpa',bpa)
c
	end
c************************************************************************
	subroutine cord(lu,naxis,ctype,crval,crpix,cdelt,llrot,
     *							ilong,ilat)
c
	implicit none
	integer lu,naxis,ilong,ilat
	character ctype(naxis)*(*)
	double precision crval(naxis),crpix(naxis),cdelt(naxis),llrot
c
c  Load in FITS coordinate system and fiddle it into a Miriad coordinate
c  system.
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i,j,k,l,l1,l2
	double precision dtemp,scale
	logical more
	character num*2
	character templat*8,ctemp*12,ctemp1*12
c
c  Externals.
c
	character itoaf*2,pc*8,cd1*8,cd2*8
	integer len1
c
	data templat/'-----CAR'/
c
	ilong = 0
	ilat = 0
	llrot = 0
c
	do i=1,naxis
	  num = itoaf(i)
	  call fitrdhda(lu,'CTYPE'//num,ctemp1,' ')
	  ctemp = ' '
	  j = 0
	  do k=1,len(ctemp1)
	    if(ctemp1(k:k).ne.' ')then
	      j = j + 1
	      ctemp(j:j) = ctemp1(k:k)
	    endif
	  enddo
	  call ucase(ctemp)
c
	  if(ctemp.eq.' '.and.i.eq.1)ctemp = 'RA---SIN'
	  if(ctemp.eq.' '.and.i.eq.2)ctemp = 'DEC--SIN'
	  if(ctemp.eq.'VLSR')then
	    ctemp = 'VELO-LSR'
	    scale = 1000.0
	  else
	    scale = 1.0
	  endif
c
	  l1 = index(ctemp,'-') - 1
	  l2 = len1(ctemp)
	  if(l1.le.0)then
	    ctype(i) = ctemp
	  else
	    ctype(i) = ctemp(1:l1)//'------'
	    l1 = l2
	    more = .true.
	    dowhile(l1.gt.1.and.more)
	      more = ctemp(l1-1:l1-1).ne.'-'
	      if(more)l1 = l1 - 1
	    enddo
	    ctype(i)(6:8) = ctemp(l1:l2)
	  endif
	  call fitrdhdd(lu,'CRPIX'//num,crpix(i),1.0d0)
	  call fitrdhdd(lu,'CRVAL'//num,crval(i),0.0d0)
	  
c
	  call fitrdhdd(lu,cd1(i,i),cdelt(i),1.d0)
	  call fitrdhdd(lu,cd2(i,i),dtemp,cdelt(i))
	  call fitrdhdd(lu,'CDELT'//num,cdelt(i),dtemp)
	  cdelt(i) = scale*cdelt(i)
	  crval(i) = scale*crval(i)
c
c
	  if(ctype(i).eq.'RA'  .or.ctype(i)(1:5).eq.'RA---'.or.
     *	     ctype(i).eq.'GLON'.or.ctype(i)(1:5).eq.'GLON-'.or.
     *	     ctype(i).eq.'ELON'.or.ctype(i)(1:5).eq.'ELON-')then
	    ilong = i
	  else if(ctype(i).eq.'DEC' .or.ctype(i)(1:5).eq.'DEC--'.or.
     *		  ctype(i).eq.'GLAT'.or.ctype(i)(1:5).eq.'GLAT-'.or.
     *		  ctype(i).eq.'ELAT'.or.ctype(i)(1:5).eq.'ELAT-')then
	    ilat = i
	  endif
	enddo
c
c  Check for skew/rotations that cannot be handled.
c
	do j=1,naxis
	  do i=1,naxis
	    if(i.ne.j.and..not.(
     *		(i.eq.ilong.and.j.eq.ilat).or.
     *		(j.eq.ilong.and.i.eq.ilat)))then
	      call fitrdhdd(lu,pc(i,j),dtemp,0.d0)
	      if(dtemp.ne.0)call bug('w',
     *		'Cannot handle non-zero FITS skewness parameter '//
     *		pc(i,j))
	      call fitrdhdd(lu,cd1(i,j),dtemp,0.d0)
	      if(dtemp.ne.0)call bug('w',
     *		'Cannot handle non-zero FITS skewness parameter '//
     *		cd1(i,j))
	      call fitrdhdd(lu,cd2(i,j),dtemp,0.d0)
	      if(dtemp.ne.0)call bug('w',
     *		'Cannot handle non-zero FITS skewness parameter '//
     *		cd2(i,j))
	    endif
	  enddo
	  if(j.ne.ilat.and.j.ne.ilong)then
	    call fitrdhdd(lu,'CROTA'//itoaf(j),dtemp,0.d0)
	    if(dtemp.ne.0)call bug('w',
     *		'Cannot handle non-zero FITS rotation parameter '//
     *		itoaf(j))
	  endif
	enddo
c
c  Determine the rotation.
c
	if(ilat.ne.0.and.ilong.ne.0)then
	  call cdget(lu,ilong,ilat,cdelt(ilong),cdelt(ilat),llrot)
c
c  Convert projectionless latitude/longitude into -CAR projection type.
c
	  if(ctype(ilong)(6:).eq.' '.and.ctype(ilat)(6:).eq.' ')then
	    l = len1(ctype(ilong))
	    ctype(ilong)(l+1:) = templat(l+1:)
	    l = len1(ctype(ilat))
	    ctype(ilat)(l+1:) =  templat(l+1:)
	    if(abs(llrot).gt.1e-3)call bug('w',
     *		'Cannot handle rotation with -CAR projection')
	    llrot = 0
	    cdelt(ilong) = cdelt(ilong)*cos(DPI/180.d0*crval(ilat))
	  endif
	endif
c
	end
c************************************************************************
	subroutine cdget(lu,ilong,ilat,cdelt1,cdelt2,llrot)
c
	implicit none
	integer lu,ilong,ilat
	double precision cdelt1,cdelt2,llrot
c
c  Get the longitude/latitude transformation matrix.
c------------------------------------------------------------------------
	include 'mirconst.h'
	double precision crota,pc11,pc12,pc21,pc22,cd11,cd12,cd21,cd22
	double precision dtemp
c
c  Externals.
c
	character itoaf*2,pc*8,cd1*8,cd2*8
c
	call fitrdhdd(lu,'CDELT'//itoaf(ilong),cdelt1,1.d0)
	call fitrdhdd(lu,'CDELT'//itoaf(ilat),cdelt2,1.d0)
	call fitrdhdd(lu,'CROTA'//itoaf(ilat),crota,0.d0)
	crota = DPI/180.d0 * crota
c
	call fitrdhdd(lu,pc(ilong,ilong),pc11,cos(crota))
	if(crota.ne.0)then
	  call fitrdhdd(lu,pc(ilong,ilat), pc12,
     *					-sin(crota)*cdelt2/cdelt1)
	  call fitrdhdd(lu,pc(ilat,ilong), pc21,
     *					 sin(crota)*cdelt1/cdelt2)
	else
	  pc12 = 0
	  pc21 = 0
	endif
	call fitrdhdd(lu,pc(ilat,ilat),  pc22,cos(crota))
c
	call fitrdhdd(lu,cd1(ilong,ilong),dtemp,pc11*cdelt1)
	call fitrdhdd(lu,cd2(ilong,ilong),cd11,dtemp)
c
	call fitrdhdd(lu,cd1(ilong,ilat),dtemp,pc12*cdelt1)
	call fitrdhdd(lu,cd2(ilong,ilat),cd12,dtemp)
c
	call fitrdhdd(lu,cd1(ilat,ilong),dtemp,pc21*cdelt2)
	call fitrdhdd(lu,cd2(ilat,ilong),cd21,dtemp)
c
	call fitrdhdd(lu,cd1(ilat,ilat),dtemp,pc22*cdelt2)
	call fitrdhdd(lu,cd2(ilat,ilat),cd22,dtemp)
c
c  We have the CD parameters. Now convert these to cdelt1,cdelt2,llrot
c
	llrot = atan(cd21/cd11)
	dtemp = atan(-cd12/cd22)
	if(abs(dtemp-llrot).gt.1e-3)then
	  call bug('w',
     *	    'Image plane is skew, which Miriad does not support')
	  call bug('w','Using some mean rotation')
	endif
	llrot = 0.5*(llrot + dtemp)
	cdelt1 = cd11/cos(llrot)
	cdelt2 = cd22/cos(llrot)
c
	end
c************************************************************************
	character*(*) function pc(i,j)
c
	implicit none
	integer i,j
c
c------------------------------------------------------------------------
	character keyw*8
	write(keyw,'(a,i3.3,i3.3)')'PC',i,j
	pc = keyw
	end
c************************************************************************
	character*(*) function cd1(i,j)
c
	implicit none
	integer i,j
c
c------------------------------------------------------------------------
	character keyw*8
	write(keyw,'(a,i3.3,i3.3)')'CD',i,j
	cd1 = keyw
	end
c************************************************************************
	character*(*) function cd2(i,j)
c
	implicit none
	integer i,j
c
c------------------------------------------------------------------------
	character keyw*8
	integer l
c
c  Externals.
c
	integer len1
	character itoaf*2
c
	keyw = 'CD'//itoaf(i)
	l = len1(keyw)
	cd2 = keyw(1:l)//'_'//itoaf(j)
	end

c************************************************************************
	subroutine geteqep(lu,key,value)
c
	implicit none
	integer lu
	character key*(*)
	real value
c------------------------------------------------------------------------
	character card*80,type*8
	integer k1,k2
	logical ok
	double precision dtemp
c
	call fitsrch(lu,key,ok)
	if(ok)then
	  call fitcdio(lu,card)
	  call ucase(card)
	  call getvalue(card,type,k1,k2)
	  ok = k1.le.k2
	endif
	if(ok)then
	  if(card(k1:k1).eq.'B'.or.card(k1:k1).eq.'J')k1 = k1 + 1
	  call atodf(card(k1:k2),dtemp,ok)
	  if(.not.ok)call bug('f','Error decoding EPOCH/EQUINOX')
	  value = dtemp
	else
	  value = 0
	endif
c
	end
c************************************************************************
	subroutine nodfudge(lu,tno)
c
	implicit none
	integer lu,tno
c
c------------------------------------------------------------------------
	integer itemp
	double precision crval1,crval2,cdelt1,cdelt2,crpix1,crpix2
c
	call bug('i','Assuming equinox of coordinates is B1950')
	call bug('i','Assuming TAN projection')
c
	call rdhdi(tno,'naxis1',itemp,0)
	crpix1 = itemp/2 + 1
	call rdhdi(tno,'naxis2',itemp,0)
	crpix2 = itemp/2 + 1
	call rdhdd(tno,'crval1',crval1,0.d0)
	call rdhdd(tno,'cdelt1',cdelt1,1.d0)
	call rdhdd(tno,'crval2',crval2,0.d0)
	call rdhdd(tno,'cdelt2',cdelt2,1.d0)
	crval1 = crval1 + cdelt1*(crpix1-1)
	crval2 = crval2 + cdelt2*(crpix2-1)
	cdelt1 = cdelt1 * cos(crval2)
	call wrhda(tno,'ctype1','RA---TAN')
	call wrhdd(tno,'cdelt1',cdelt1)
	call wrhdd(tno,'crval1',crval1)
	call wrhdd(tno,'crpix1',crpix1)
	call wrhda(tno,'ctype2','DEC--TAN')
	call wrhdd(tno,'crval2',crval2)
	call wrhdd(tno,'crpix2',crpix2)
	call wrhdr(tno,'epoch',1950.0)
c
	end
c************************************************************************
	subroutine dssfudge(lu,tno)
c
	implicit none
	integer lu,tno
c
c  Convert a DSS header into a normal Miriad one.
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	real xpixelsz,ypixelsz,pltscale,objctx,objcty,cnpix1,cnpix2
	double precision crval1,crval2,cdelt1,cdelt2,crpix1,crpix2
	character objctra*16,objctdec*16
c
c  Get information from the header of the DSS file.
c
	call fitrdhdr(lu,'XPIXELSZ',xpixelsz,0.)
	call fitrdhdr(lu,'YPIXELSZ',ypixelsz,0.)
	call fitrdhdr(lu,'PLTSCALE',pltscale,0.)
	call fitrdhdr(lu,'OBJCTX',objctx,0.)
	call fitrdhdr(lu,'OBJCTY',objcty,0.)
	call fitrdhda(lu,'OBJCTRA',objctra,' ')
	call fitrdhda(lu,'OBJCTDEC',objctdec,' ')
	call fitrdhdr(lu,'CNPIX1',cnpix1,0.)
	call fitrdhdr(lu,'CNPIX2',cnpix2,0.)
c
	crpix1 = objctx - cnpix1 + 1
	crpix2 = objcty - cnpix2 + 1
	cdelt1 = -dpi/180.d0/3600.d0 * xpixelsz * pltscale * 1e-3
	cdelt2 =  dpi/180.d0/3600.d0 * ypixelsz * pltscale * 1e-3
	call decval(crval1,objctra)
	crval1 = dpi/12.d0 * crval1
	call decval(crval2,objctdec)
	crval2 = dpi/180.d0 * crval2
c
	call wrhdr(tno,'epoch',2000.0)
	call wrhdd(tno,'crpix1',crpix1)
	call wrhdd(tno,'crpix2',crpix2)
	call wrhdd(tno,'cdelt1',cdelt1)
	call wrhdd(tno,'cdelt2',cdelt2)
	call wrhdd(tno,'crval1',crval1)
	call wrhdd(tno,'crval2',crval2)
	call wrhda(tno,'ctype1','RA---CAR')
	call wrhda(tno,'ctype2','DEC--CAR')
	end
c************************************************************************
	subroutine decval(value,string)
c
	implicit none
	double precision value
	character string*(*)
c
c------------------------------------------------------------------------
	integer k1,k2,length
	character token*16
	double precision v1,v2,v3
	logical neg,ok
c
c  Externals.
c
	integer len1
c
	k1 = 1
	k2 = len1(string)
	call getfield(string,k1,k2,token,length)
	neg = token(1:1).eq.'-'
	ok = (.not.neg.and.length.ge.1).or.(neg.and.length.ge.2)
	if(ok)then
	  if(neg)then
	    call atodf(token(2:length),v1,ok)
	  else
	    call atodf(token(1:length),v1,ok)
	  endif
	endif
	call getfield(string,k1,k2,token,length)
	ok = ok.and.length.ge.1
	if(ok)call atodf(token(1:length),v2,ok)
	call getfield(string,k1,k2,token,length)
	ok = ok.and.length.ge.1
	if(ok)call atodf(token(1:length),v3,ok)
c
c  Check for errors.
c
	if(.not.ok)call bug('f','Error decoding DSS header item')
c
	value = v1 + v2/60.d0 + v3/3600.d0
	if(neg)value = -value
	end
c************************************************************************
	subroutine xyout(in,out,version,boxes)
c
	implicit none
	character in*(*),out*(*),version*(*)
	integer boxes(*)
c
c  Write out a image FITS file.
c
c  Inputs:
c    in		Name of the input Miriad image file.
c    out	Name of the output FITS file.
c    version	Version of this program.
c    boxes      Region of interest specification
c
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'maxnax.h'
	include 'mem.h'
	integer pArray,pFlags
	integer naxis,tno,lu,i,j,j0,nsize(MAXNAX)
	integer blc(maxnax),trc(maxnax),Nout(MAXNAX)
	integer Inplane(maxnax),Outplane(maxnax),one(maxnax)
	character string*64
	logical doflag,done
c
c  Externals.
c
	logical FitBlank,hdprsnt
c
c  Open the input MIRIAD file and determine a few things about it.
c
	call xyopen(tno,in,'old',MAXNAX,nsize)
	if(nsize(1).gt.maxdim)
     *	  call bug('f','Image too big for me to handle')
	call coInit(tno)
	doflag = hdprsnt(tno,'mask')
	call rdhdi(tno,'naxis',naxis,0)
	call BoxSet(boxes,MAXNAX,nsize,' ')
	naxis = min(naxis,MAXNAX)
c
c  Determine portion of image to copy.
c
	call BoxInfo(boxes,MAXNAX,blc,trc)
	do i=1,maxnax
	  Nout(i) = (trc(i) - blc(i) + 1)
	enddo
c
c  Open the output FITS file.
c
	call fxyopen(lu,out,'new',naxis,Nout)
	doflag = FitBlank(lu,doflag)
c
c  Handle the output header.
c
	call axisout(lu,tno,naxis,blc)
	call hdout(tno,lu,version)
	string = 'Miriad '//version
	call fitwrhda(lu,'ORIGIN',string)
c
c  Copy the data.
c
	call memAlloc(pArray,nsize(1),'r')
	if(doflag)call memAlloc(pFlags,nsize(1),'l')
c
c  Initialise the plane indices.
c
	do i=3,MAXNAX
	  one(i-2) = 1
	  Inplane(i-2) = blc(i)
	  Outplane(i-2) = 1
	enddo

c	call IncIni(naxis,nsize,axes)
c	dowhile(Inc3More(naxis,nsize,axes))
	done = .false.
	do while(.not.done)
	  call xysetpl(tno,maxnax-2,Inplane)
	  call fxysetpl(lu,maxnax-2,Outplane)
c
c	  if(naxis.gt.2)then
c	    call xysetpl(tno,naxis-2,axes(3))
c	    call fxysetpl(lu,naxis-2,axes(3))
c	  endif
	  j0 = blc(2)
	  do j=1,Nout(2)
	    call xyread(tno,j0,memr(pArray))
	    call fxywrite(lu,j,memr(pArray + blc(1) - 1))

	    if(doflag)then
	      call xyflgrd(tno,j0,meml(pFlags))
	      call fxyflgwr(lu,j,meml(pFlags + blc(1) - 1))
	    endif
	    j0 = j0 + 1
	  enddo
	  call planeinc(maxnax-2,1,blc(3),trc(3),Inplane,done)
	  call planeinc(maxnax-2,one,one,Nout(3),Outplane,done)
	enddo
	call memFree(pArray,nsize(1),'r')
	if(doflag)call memFree(pFlags,nsize(1),'l')
c
c  All said and done. Go to sleep.
c
	call coFin(tno)
	call xyclose(tno)
	call fxyclose(lu)
c
	end

c************************************************************************
	subroutine planeinc(n,incr,blc,trc,plane,done)
c
	implicit none
	integer n,blc(n),trc(n),plane(n),incr(n)
	logical done
c
c  Move to the next plane.
c
c------------------------------------------------------------------------
	integer k
c
	k = 1
	done = .true.
c
	do while(done.and.k.le.n)
	  done = plane(k).ge.trc(k)
	  if(done)then
	    plane(k) = blc(k)
	  else
	    plane(k) = plane(k) + incr(k)
	  endif
	  k = k + 1
	enddo
	end
c************************************************************************
	subroutine axisout(lu,tno,naxis,blc)
c
	implicit none
	integer lu,tno,naxis,blc(naxis)
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
c    blc        Bottom-left hand corner pixel value for axis
c               used to determine crpix if a subregion was
c               selected
c
c------------------------------------------------------------------------
	include 'mirconst.h'
	integer i,npnt
	character num*2,ctype*32,date*32,card*80
	real bmaj,bmin,rms
	double precision restfreq,crval,cdelt,crota,crpix
	double precision obstime,obsra,obsdec,scale,lat
	character cellscal*16,telescop*16
	logical givegls
c
c  Externals.
c
	character itoaf*2
	logical hdprsnt
c
c  Load the mosaic table.
c
	if(hdprsnt(tno,'mostable'))then
	  call mosLoad(tno,npnt)
	  if(npnt.gt.1)call bug('w',
     *	  'Mosaicing information not stored in output FITS file')
	  call mosGet(1,obsra,obsdec,rms,telescop)
	  call fitwrhdd(lu,'OBSRA',180.d0/DPI * obsra)
	  call fitwrhdd(lu,'OBSDEC',180.d0/DPI * obsdec)
	else
	  call rdhda(tno,'telescop',telescop,' ')
	endif
	if(telescop.ne.' ')call fitwrhda(lu,'TELESCOP',telescop)
c
	givegls = .true.
	do i=1,naxis
	  call coAxGet(tno,i,ctype,crpix,crval,cdelt)
	  num = itoaf(i)
c
c  Determine scale factor.
c
	  if(ctype(1:5).eq.'RA---'.or.
     *	     ctype(1:5).eq.'DEC--'.or.
     *	     ctype(1:5).eq.'GLON-'.or.
     *	     ctype(1:5).eq.'GLAT-'.or.
     *	     ctype(1:5).eq.'ELON-'.or.
     *	     ctype(1:5).eq.'ELAT-')then
	    scale = 180.d0/DPI
c
	    call coGetd(tno,'llrot',crota)
c
c  Fiddle to the FITS convention.
c
	    if(     ctype.eq.'RA---CAR'.or.ctype.eq.'GLON-CAR'.or.
     *	            ctype.eq.'ELON-CAR')then
	      if(crota.ne.0)call bug('w','Rotation of CAR'//
     *		' projection not supported in output FITS file')
	      call striper(ctype)
	      call getlat(tno,lat)
	      cdelt = cdelt/cos(lat)
	      crota = 0
	    else if(ctype.eq.'DEC--CAR'.or.ctype.eq.'GLAT-CAR'.or.
     *		    ctype.eq.'ELAT-CAR')then
	      call striper(ctype)
	      crota = 0
	    else if(ctype(5:8).eq.'-GLS'.and.givegls)then
	      call bug('w',
     *		'The output uses the old convention for GLS projection')
	      card='HISTORY This FITS file uses the old GLS convention'
	      call fitcdio(lu,card)
	      card='HISTORY See AIPS Memo 46 for details'
	      call fitcdio(lu,card)
	      givegls = .false.
	    endif
	    if(crota.ne.0 .and. i.eq.2)
     *         call fitwrhdd(lu,'CROTA'//num,180.0/DPI*crota)
c
	  else if(ctype.eq.'ANGLE')then
	    scale =180.d0/DPI
	  else if(ctype(1:4).eq.'FREQ')then
	    scale = 1d9
	  else if(ctype(1:4).eq.'VELO'.or.ctype(1:4).eq.'FELO')then
	    scale = 1d3
	  else
	    scale = 1.
	  endif
	  call fitwrhdd(lu,'CDELT'//num,scale*cdelt)
	  call fitwrhdd(lu,'CRPIX'//num,crpix - blc(i) + 1)
	  call fitwrhdd(lu,'CRVAL'//num,scale*crval)
	  if(ctype.ne.' ')call fitwrhda(lu,'CTYPE'//num,ctype)
	enddo
c
c  Write out other coordinates.
c
	call coGetd(tno,'obstime',obstime)
	if(obstime.gt.0) then
	  call julday(obstime,'T',date)
	  call fitwrhda(lu,'DATE-OBS',date)
	endif
	call coGetd(tno,'restfreq',restfreq)
	if(restfreq.gt.0) call fitwrhdd(lu,'RESTFREQ',1.0d9*restfreq)
	call coGeta(tno,'cellscal',cellscal)
	if(cellscal.ne.' ')call fitwrhda(lu,'CELLSCAL',cellscal)
c
	call rdhdr(tno,'bmaj',bmaj,0.)
	if(bmaj.ne.0) call fitwrhdr(lu,'BMAJ',(180/pi)*bmaj)
	call rdhdr(tno,'bmin',bmin,0.)
	if(bmin.ne.0) call fitwrhdr(lu,'BMIN',(180/pi)*bmin)
c
	end
c************************************************************************
	subroutine striper(ctype)
c
	implicit none
	character ctype*(*)
c
c------------------------------------------------------------------------
	integer i
	logical more
c
	ctype(5:) = ' '
	i = 4
	more = ctype(i:i).eq.'-'
	dowhile(more)
	  ctype(i:i) = ' '
	  i = i - 1
	  more = i.gt.0
	  if(more)more = ctype(i:i).eq.'-'
	enddo
c
	end
c************************************************************************
	subroutine getlat(tno,lat)
c
	implicit none
	integer tno
	double precision lat
c------------------------------------------------------------------------
	integer iax
c
	character itoaf*2
c
	call coFindAx(tno,'latitude',iax)
	call coGetd(tno,'crval'//itoaf(iax),lat)
	end
c************************************************************************
	subroutine hdout(tno,lu,version)
c
	implicit none
	integer lu,tno
	character version*(*)
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
	parameter(nshort=6,nlong=11)
	character short(nshort)*5,long(nlong)*8
	integer iostat,item,n,ival
	real rval
	double precision dval
	character key*12,line*80,descr*80,type*16,ukey*12
	logical discard
c
c  Externals.
c
	integer len1,binsrcha
	logical hdprsnt
c
	data short/'cdelt','crota','crpix','crval','ctype','naxis'/
	data long/'bmaj    ','bmin    ','cellscal',
     *		  'history ','image   ','llrot   ','mask    ',
     *		  'mostable','obstime ','restfreq','telescop'/
c
c  Short items have numbers attached.
c  Open the "special item" which gives the names of all the items in the
c  file.
c
	call haccess(tno,item,'.','read',iostat)
	if(iostat.ne.0)call bugno('f',iostat)
	call hreada(item,key,iostat)
	dowhile(iostat.eq.0)
	  discard = binsrcha(key,long,nlong).ne.0.or.
     *		    binsrcha(key(1:5),short,nshort).ne.0
	  if(.not.discard)then
	    call hdprobe(tno,key,descr,type,n)
	    if(n.eq.1)then
	      ukey = key
	      call ucase(ukey)
	      if(type.eq.'integer*2'.or.type.eq.'integer')then
		call rdhdi(tno,key,ival,0)
		call fitwrhdi(lu,ukey,ival)
	      else if(type.eq.'real')then
		call rdhdr(tno,key,rval,0.)
		call fitwrhdr(lu,ukey,rval)
	      else if(type.eq.'double')then
		call rdhdd(tno,key,dval,0.d0)
		call fitwrhdd(lu,ukey,dval)
	      else if(type.eq.'character')then
		call fitwrhda(lu,ukey,descr(1:len1(descr)))
	      else
		call ucase(descr)
		line = key(1:8)//'= '//descr
	        call fitcdio(lu,line)
	      endif
	    endif
	  endif
	  call hreada(item,key,iostat)
	enddo
	call hdaccess(item,iostat)
c
c  Write out the history file as HISTORY comments.
c
	if (hdprsnt(tno,'history ')) then
	  call copyHist(tno,lu,version)
	end if
	end
c************************************************************************
	subroutine CopyHist(tIn,tOut,version)
c
	integer tin,tout
	character version*(*)
c
c  Copy out the history comments.
c
c  Input:
c    tIn	The handle of the input MIRIAD file.
c    tOut	The handle of the output FITS file.
c
c------------------------------------------------------------------------
	logical eof
	character card*132,line*80,name*32
	integer narg,i,l1,l2,length,lu,iostat
	logical dofile
	character file*256
	double precision julian
c
c  Externals.
c
	integer iargc,len1
c
	call hisopen(tIn,'read')
	call hisread(tIn,card,eof)
	dowhile(.not.eof)
	  line = 'HISTORY '//card(1:72)
	  call fitcdio(tOut,line)
	  call hisread(tIn,card,eof)
	enddo
	call hisclose(tIn)
c
c  Write some extra FITS history.
c
	narg = iargc()
	name = 'HISTORY FITS:'
	l2 = len(line)
	l1 = len1(name)
	line = name(1:l1)//' Miriad fits: '//version
	call fitcdio(tOut,line)
	call TodayJul(julian)
	call JulDay(julian, 'H', file)
	line = name(1:l1)//' Executed on: '//file(1:len1(file))
	call fitcdio(tOut,line)
	line(l1+1:) = ' Command line inputs follow:'
	call fitcdio(tOut,line)
c
	line(l1+1:l1+4) = ' '
	l1 = l1 + 5
c
	dofile = .false.
	do i=1,narg
	  if(dofile)then
	    call getarg(i,file)
	    call txtopen(lu,file,'old',iostat)
	    if(iostat.ne.0)then
	      call bug('w','Error opening input parameter file')
	      call bugno('w',iostat)
	    else
	      call txtread(lu,line(l1:l2),length,iostat)
	      dowhile(iostat.eq.0)
		length = min(l2,length + l1 - 1)
		call fitcdio(tOut,line(1:length))
		call txtread(lu,line(l1:l2),length,iostat)
	      enddo
	      call txtclose(lu)
	    endif
	    dofile = .false.
	  else
	    call getarg(i,line(l1:l2))
	    if(line(l1:l2).eq.'-f')then
	      dofile = .true.
	    else
	      call fitcdio(tOut,line(1:l2))
	    endif
	  endif
	enddo
c
	line = 
     *	 'HISTORY FITS: NOTE: Use options=varwt if loading into Miriad'
	call fitcdio(tOut,line)
c
	end
c************************************************************************
	subroutine histin(lu,tno,version)
c
	implicit none
	integer lu,tno
	character version*(*)
c
c  Create the history file.
c
c  Input:
c    lu		Handle of the input FITS file.
c    tno	Handle of the output MIRIAD file.
c    version   
c
c------------------------------------------------------------------------
	integer nlong,nshort
	parameter(nlong=50,nshort=9)
	character card*80
	logical more,discard,found,unrec
	integer i
	logical history(nlong)
	character long(nlong)*8,short(nshort)*5
c
c  Externals.
c
	integer binsrcha
c
c  A table of keywords (common to uv and xy) that are either history
c  comments, or keywords to be discarded. Some of these are handled
c  as special cases elsewhere. Short keywords have numbers attached.
c
	data (long(i),history(i),i=1,nlong)/
     *	  '        ', .true.,  'ALTRPIX ', .false., 'ALTRVAL ', .false.,
     *    'BITPIX  ', .false., 'BLANK   ', .false., 'BLOCKED ', .false.,
     *	  'BMAJ    ', .false., 'BMIN    ', .false., 'BPA     ', .false.,
     *	  'BSCALE  ', .false., 'BTYPE   ', .false., 'BUNIT   ', .false.,
     *    'BZERO   ', .false., 'CELLSCAL', .false., 'COMMAND ', .true.,
     *    'COMMENT ', .true.,  'DATAMAX ', .false., 'DATAMIN ', .false., 
     *	  'DATE    ', .false., 'DATE-MAP', .false., 'DATE-OBS', .false.,
     *	  'END     ', .false., 'EPOCH   ', .false., 'EQUINOX ', .false.,
     *	  'EXTEND  ', .false., 'GCOUNT  ', .false., 'GROUPS  ', .false.,
     *	  'HISTORY ', .true.,  'INSTRUME', .false., 'LSTART  ', .false.,
     *	  'LSTEP   ', .false., 'LTYPE   ', .false., 'LWIDTH  ', .false.,
     *	  'NITERS  ', .false., 'OBJECT  ', .false.,
     *	  'OBSDEC  ', .false., 'OBSERVER', .false., 'OBSRA   ', .false.,
     *	  'ORIGIN  ', .false., 'PBFWHM  ', .false., 'PBTYPE  ', .false.,
     *	  'PCOUNT  ', .false., 'RESTFREQ', .false., 'RMS     ', .false.,
     *	  'SIMPLE  ', .false., 'TELESCOP', .false.,
     *	  'VELREF  ', .false., 'VOBS    ', .false., 'XSHIFT  ', .false.,
     *	  'YSHIFT  ', .false./
	data short/'CDELT','CROTA','CRPIX','CRVAL','CTYPE','NAXIS',
     *	  'PSCAL','PTYPE','PZERO'/
c
c  Search for the 'SIMPLE' keyword, which effectively sets the pointer
c  at the first card in the file.
c
	unrec = .false.
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
	  discard = .false.
	  i = binsrcha(card(1:8),long,nlong)
	  if(i.ne.0)then
	    discard = .true.
	    if(history(i)) call hiscard(tno,card)
	    more = card(1:8).ne.'END'
	  else
	    discard = binsrcha(card(1:5),short,nshort).ne.0
	  endif
	  unrec = unrec.or..not.discard
	  if(.not.discard)call hiswrite(tno,card)
	enddo
	if(unrec)call bug('w',
     *	  'Some unrecognised cards were written to the history file')
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
	logical more
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
	more = .true.
	dowhile(k1.le.k2.and.more)
	  more = card(k1:k1).le.' '.or.card(k1:k1).eq.'='
	  if(more) k1 = k1 + 1
	enddo
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
c  This returns a type "unknown" if the keyword looks bad. In particular
c  the first 8 characters must be alphanumeric with possible trailing
c  blanks.
c
c  Input:
c    card	The FITS card.
c
c  Output:
c    type	Either 'integer', 'real', 'double', 'ascii' or 'unknown'.
c    k1,k2	These delimit the value in CARD, for other than 'unknown'
c
c------------------------------------------------------------------------
	character c*1
	integer l,i
	logical more
c
c  Externals.
c
	integer len1
c
	type = 'unknown'
	if(len(card).le.8)return
c
c  Look for the first non-alphanumeric character.
c
	l = len1(card(1:8))
	do i=1,l
	  c = card(i:i)
	  if(.not.((c.ge.'a'.and.c.le.'z').or.
     *		   (c.ge.'A'.and.c.le.'Z').or.
     *		   (c.ge.'0'.and.c.le.'9')))return
	enddo
c
c  Skip to the first non-blank character after the equals sign.
c
	k1 = 9
	k2 = len(card)
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
