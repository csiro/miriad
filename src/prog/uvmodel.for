      program uvmodel

c= uvmodel - Add, subtract, etc, a model from a uv data set.
c& rjs
c: uv analysis
c+
c       UVMODEL is a MIRIAD task that modifies a visibility dataset by
c       combining or replacing it a model.  Allowed operations are
c       adding, subtracting, multiplying, dividing and replacing.  The
c       model is specified in the image domain, so that its Fourier
c       transform is first computed before application to the
c       visibilities.  The model may be either an image (e.g., a CLEAN
c       component image) or a point source.
c
c       As an example, UVMODEL could be used to remove CLEAN components
c       from a visibility data file.  The residual data base could then
c       be examined for anomalous points, which could in turn be
c       clipped.  UVMODEL could then be reapplied to add the CLEAN
c       components back into the visibility data base for re-imaging.
c@ vis
c       Input visibility data file.  No default
c@ model
c       Input model cube.  The default is a point source model.  This
c       will generally be a deconvolved map formed from the visibility
c       data being modified.  It should be made with "channel" linetype.
c       The model should have units of JY/PIXEL and be weighted by the
c       primary beam.  The task DEMOS can be used to extract primary
c       beam weighted models from a mosaiced image.
c@ select
c       The standard uv selection subcommands.  The default is all data.
c@ options
c       This gives extra processing options.  Several values can be
c       given though many are mutually exclusive, separated by commas.
c       Option values can be abbreviated to uniqueness.
c       Possible options are (no default):
c         add       Form: out = vis + model
c         subtract  Form: out = vis - model
c         multiply  Form: out = vis * model
c         divide    Form: out = vis / model
c         replace   Form: out = model
c         flag      Form: out = vis, but flag data where the difference
c                   between vis and model is greater than "sigma"
c                   sigmas.
c         unflag    Unflag any flagged data in the output.
c         mosaic    Select only those visibilities whose observing
c                   centre is within plus or minus three pixels of the
c                   model reference pixel.  This is needed if there are
c                   multiple pointings or multiple sources in the input
c                   uv file.  By default no observing centre selection
c                   is performed.
c         mfs       This is used if there is a single plane in the input
c                   model, which is assumed to represent the data at all
c                   frequencies.  This should also be used if the model
c                   has been derived using MFCLEAN.
c         zero      Use zero as the value of the model if it cannot be
c                   calculated.  This can be used to avoid flagging the
c                   data in the outer parts of the u-v-plane when
c                   subtracting a low resolution model.
c       The operations add, subtract, multiply, divide, replace and flag
c       are mutually exclusive.  The operations flag and unflag are also
c       mutually exclusive.
c
c       The unflag option should be used with caution.  Data in the
c       output may still be flagged, if it was not possible to calculate
c       the model.
c@ clip
c       Clip level.  Pixels in the model below this level are set to
c       zero.  The default is not to perform any clipping.
c@ flux
c       If MODEL is blank, then the flux (Jy) of a point source model
c       should be specified here.  Also used as the default flux in the
c       apriori option.  The default is 1 (assuming the model parameter
c       is not given).  The flux can optionally be followed by i,q,u,v
c       or the other polarisation mnemonics to indicate the polarisation
c       type. The 3rd-6th parameter optionally specify the reference
c       frequency in GHZ and the spectral index and higher order terms 
c       (as given by uvsfit or uvfmeas with options=mfflux).
c@ offset
c       The RA and DEC offsets (arcsec) of the point source from the
c       observing centre.  A point source to the north and east has
c       positive offsets.  Defaults are zero.
c@ line
c       The visibility linetype to use, in the standard form, viz:
c         type,nchan,start,width,step
c       Generally if there is an input model, this defaults to the
c       linetype parameters used to construct the map.  For a point
c       source or planet model, the default is all channels.  This
c       parameter may be used if you wish to override these defaults,
c       or if the relevant information is not present in the header, 
c@ sigma
c       For options=flag, UVMODEL flags those points in the output that
c       differ by more than "sigma" sigmas.  The default is 100.
c@ out
c       Output visibility data file name.  The output file will contain
c       only as many channels as there are planes in the model cube.
c       The various uv variables that describe the windows are adjusted
c       accordingly.  No default. 
c
c--
c
c  History:
c    rjs  28mar90 Original version.
c    rjs  31mar90 Changed maxchan. apriori option.  Concat. with
c                 char*(*) bug.  Improved handling of flagged data and
c                 the unflag option.
c    rjs  12apr90 Added support for "wide" and "velocity" linetypes.
c    rjs  24apr90 Checked that "out" keyword was set.
c    pjt   2may90 maxdim.h now defines maxchan
c    mchw 26may90 Corrected code in main and WindUpd for model image.
c    mchw 19nov90 Added option "imhead" to copy image header to
c                 uvvariables.
c    mchw 15dec90 Checked for variable "nants" in uvdata.
c    rjs  11mar91 Copy "chi" variable, and change "npols" to "npol".
c    rjs  25mar91 Added "line" task parameter.
c    mchw 27mar91 Better bookkeeping in WindUpd. Added more variables.
c    rjs   1jul91 Corrected calculation of wwidth in the output file.
c    mjs  04aug91 Replaced local maxants with maxdim.h MAXANT
c    rjs  29aug91 Complete rework of WindUpd.
c    nebk 29aug91 Make user specify an option.
c    rjs   1nov91 Added options=mfs and polarized.  Simple polarisation
c                 processing.
c    rjs  29jan92 New call sequence to model.
c    rjs  30mar92 Added option selradec.
c    rjs  26apr92 Better model processing.
c    rjs  15jun92 Use "Var" routines to simplify copying uv files.
c    rjs  23jun92 Doc and message changes only.
c    mchw 04aug92 Corrected sign of vstart written into uvdata.
c    rjs  17aug92 Updated call sequence to var routines, plus other
c                 tidying.
c    mchw 04sep92 Fixed a missing argument in uvVarCpy.
c    rjs  15feb93 Changes to make ra,dec variables double.
c    rjs  29mar93 Fiddles with the sigma.
c    rjs  23dec93 Minimum match of linetype name.
c    rjs  31jan95 Changes to support w-axis.
c    mhw  05jan96 Add zero option to avoid flagging outer uvplane
c    rjs  30sep96 Tidy up and improved polarisation handling.
c    rjs  19jun97 Point source models can be different polarisations.
c    rjs  26sep97 Re-add mhw's zero option.
c    rjs  01dec98 More warning messages.
c    rjs  03apr09 Fix long standing bug in "options=flag"
c    mhw  17jan12 Use ptrdiff for scr routines to handle larger files
c    mhw  24jan15 Add spectral parameters to flux keyword
c    jbs  08nov19 Changes to allow for mixing with UV-based models
c    jbs  03dec19 Fixed a bug in determining if the model was image
c                 or uv.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer MAXSELS, NHEAD, NBUF
      parameter (MAXSELS=64, NHEAD=1, NBUF=5*MAXCHAN+NHEAD)

      logical   calcrms, defline, doclip, dounflag, flags(MAXCHAN),
     *          mfs, selradec, unflag, updated, zero
      integer   i, length, nchan, npol, nread, nsize(3), nvis, pol,
     *          pols(-8:4), tMod, tOut, tScr, tVis, tChk, chkStat
      ptrdiff   off
      real      buffer(NBUF), clip, flux(6),
     *          offset(2), sels(MAXSELS), sigma
      double precision preamble(5), lstart, lstep, lwidth
      real      p1,p2,p3
      complex   uvdata(MAXCHAN)
      character flag1*8, flag2*8, ltype*32, modl*256, oper*8, out*256,
     *          poltype*4, type*1, version*72, vis*256

      common /uvmodcom/ calcrms, dounflag

      external  hdprsnt, header, keyprsnt, polsp2c, versan
      logical   hdprsnt, keyprsnt, modisuv
      integer   polsp2c
      character versan*72
c-----------------------------------------------------------------------
      version = versan('uvmodel')

c     Get the input parameters.
      call keyini
      call keya('vis',vis,' ')
      call SelInput('select',sels,MAXSELS)
      call GetOpt(oper,unflag,mfs,selradec,zero)
      call keya('model',modl,' ')
      doclip = keyprsnt('clip')
      call keyr('clip',clip,0.0)
      call keyr('flux',flux(1),1.0)
      call keya('flux',poltype,'i')
      flux(2) = polsp2c(poltype)
      call keyr('flux',flux(3),0.0)
      call keyr('flux',flux(4),0.0)
      call keyr('flux',flux(5),0.0)
      call keyr('flux',flux(6),0.0)
      call keyr('sigma',sigma,100.0)
      call keyr('offset',offset(1),0.0)
      call keyr('offset',offset(2),0.0)
      call keyline(ltype,nchan,lstart,lwidth,lstep)
      call keya('out',out,' ')
      call keyfin

c     Check the input parameters.
      if (vis.eq.' ')
     *  call bug('f','Input visibility file name must be given')
      if (modl.eq.' ' .and. flux(1).eq.0.0)
     *  call bug('f','There was no input model')
      if (out.eq.' ')
     *  call bug('f','Output visibility file name must be given')
      if (oper.eq.'flag' .and. sigma.le.0.0)
     *  call bug('f','Sigma must be positive valued.')
      if (modl.eq.' ' .and. abs(offset(1))+abs(offset(2)).eq.0.0)
     *  call output('Model is a point source at the observing centre')

c     Miscellaneous initialisation.
      call uvopen(tVis,vis,'old')
      if (hdprsnt(tVis,'gains') .or. hdprsnt(tVis,'leakage') .or.
     *   hdprsnt(tVis,'bandpass')) then
        call bug('w','UVMODEL does not apply any calibration tables')
        if (hdprsnt(tVis,'gains'))call bug('w',
     *    'Antenna gain calibration not applied')
        if (hdprsnt(tVis,'leakage'))call bug('w',
     *    'Polarization leakage calibration not applied')
        if (hdprsnt(tVis,'bandpass'))call bug('w',
     *    'Bandpass calibration not applied')
      endif

c     Determine the flags to the ModelIni and Model routines.
      flag1 = ' '
      flag2 = ' '
      if (selradec) flag1 = 'p'
      if (mfs)      flag2(1:1) = 'm'
      if (doclip)   flag2(2:2) = 'l'
      if (zero)     flag2(3:3) = 'z'

      calcrms = oper.eq.'flag'
      dounflag = unflag

c     Determine the default linetype from the uvdata if needed.
      Defline = ltype.eq.' '
      if (Defline) then
        call uvprobvr(tVis,'corr',type,length,updated)
        if (type.eq.'j' .or. type.eq.'r') then
          ltype = 'channel'
        else
          call uvprobvr(tVis,'wcorr',type,length,updated)
          if (type.ne.'c')call bug('f',
     *        'Visibility file contains neither corr nor wcorr')
          ltype = 'wide'
        endif
        nchan  = 0
        lstart = 1.0
        lstep  = 1.0
        lwidth = 1.0
      endif

c     Do the model calculation for point source or model image.
      if (Modl.eq.' ') then
        call SelApply(tVis,sels,.true.)
        call uvset(tVis,'data',ltype,nchan,lstart,lwidth,lstep)
        call Model(flag2,tVis,0,offset,flux,tScr,NHEAD,header,
     *                                                nchan,nvis)
      else
         call hopen(tChk, modl, 'old', chkStat)
         if (hdprsnt(tChk,'image')) then
            call hclose(tChk)
            modisuv = .false.
            call xyopen(tMod,Modl,'old',3,nsize)
            if (Defline) then
c     Convert double to real and back again       
               p1 = lstart
               p2 = lwidth
               p3 = lstep
               call rdhda(tMod,'ltype',ltype,ltype)
               call rdhdr(tMod,'lstart',p1,p1)
               call rdhdr(tMod,'lwidth',p2,p2)
               call rdhdr(tMod,'lstep',p3,p3)
               lstart = p1
               lwidth = p2
               lstep  = p3
               if (.not.mfs)nchan = nsize(3)
            endif
            call uvset(tVis,'data',ltype,nchan,lstart,lwidth,lstep)
         else
            call hclose(tChk)
            modisuv = .true.
            call uvopen(tMod, modl,'old')
            call SelApply(tMod,sels,.true.)
         endif
        call ModelIni(tMod,tVis,sels,flag1)
        call Model(flag2,tVis,tMod,offset,Clip,tScr,NHEAD,header,
     *                                                nchan,nvis)
      endif

c     Reset the input file and set up for a copying.
      call uvrewind(tVis)
      call uvset(tVis,'coord','nanosec',0,0.0,0.0,0.0)
      call uvset(tVis,'preamble','uvw/time/baseline',0,0.0,0.0,0.0)

      call uvopen(tOut,out,'new')
      call uvset(tOut,'preamble','uvw/time/baseline',0,0.0,0.0,0.0)
      call VarInit(tVis,ltype)
      call VarOnit(tVis,tOut,ltype)

      do pol = -8, 4
        pols(pol) = 0
      enddo

c     Perform the copying.
      length = 5*nchan + NHEAD
      do i = 1, nvis
        call uvread(tVis, preamble, uvdata, flags, MAXCHAN, nread)
        if (nread.ne.nchan) call bug('f',
     *    'No. channels  unexpectedly changed, when rereading data')
        off = i-1
        off = off * length
        call scrread(tScr, buffer, off, length)
        call process(oper, buffer(1)*sigma, nchan, buffer(NHEAD+1),
     *               uvdata, flags)

c       Copy polarisation info - npol here may be an upper limit.
        call uvrdvri(tVis,'npol',npol,0)
        if (npol.gt.0) then
          call uvputvri(tOut, 'npol', npol, 1)
          call uvrdvri (tVis, 'pol', pol, 1)
          call uvputvri(tOut, 'pol', pol, 1)
          pols(pol) = 1
        endif

c       Copy the variables and the data.
        call VarCopy(tVis,tOut)
        call uvwrite(tOut,preamble,uvdata,flags,nchan)
      enddo
      call scrclose(tScr)

c     How many polarisations were there (assuming they don't vary)?
      npol = 0
      do pol = -8, 4
        npol = npol + pols(pol)
      enddo
      call wrhdi(tOut, 'npol', npol)

c     Write history.
      call hdcopy(tVis,tOut,'history')
      call hisOpen(tOut,'append')
      call hisWrite(tOut,'UVMODEL: Miriad UvModel '//version)
      call hisInput(tOut,'UVMODEL')
      call hisClose(tOut)

c     Close up shop.
      if (Modl.ne.' ') then
         if (modisuv.eqv..true.) then
            call uvClose(tMod)
         else
            call xyclose(tMod)
         endif
      endif
      call uvClose(tVis)
      call uvClose(tOut)

      end

c***********************************************************************

      subroutine header(tVis, preamble, uvdata, flags, nchan,
     *                  accept, Out, nhead)

      integer tVis,nchan,nhead
      complex uvdata(nchan)
      logical flags(nchan),accept
      real Out(nhead)
      double precision preamble(5)
c-----------------------------------------------------------------------
c  This is a service routine called by the model subroutines.  It is
c  called every time a visibility is read from the uvdata file.
c
c  Input:
c    tVis       Handle of the visibility file.
c    nhead      The value of nhead
c    nchan      The number of channels.
c    preamble   Preamble returned by uvread.
c    uvdata     A complex array of nchan elements giving the correlation
c               data.
c               Not used.
c    flags      The data flags. Not used.
c  Output:
c   out         The nhead values to save with the data.  One value is
c               returned:
c                 out(1) -- rms error.
c   accept      This determines whether the data is accepted or
c               discarded.  It is always accepted unless the baseline
c               number looks bad.
c-----------------------------------------------------------------------
      integer i
      double precision sigma2

      logical calcrms,dounflag
      common/uvmodcom/calcrms,dounflag
c-----------------------------------------------------------------------
c     Unflag the data if necessary.
      if (dounflag) then
        do i = 1, nchan
          flags(i) = .true.
        enddo
      endif

c     Determine the rms error if needed.
      if (calcrms) then
        call uvinfo(tVis,'variance',sigma2)
        out(1) = sqrt(abs(sigma2))
        accept = sigma2.gt.0
      else
        Out(1) = 0.0
        accept = .true.
      endif
      end

c***********************************************************************

      subroutine process(oper,rms,nchan,buffer,uvdata,flags)

      integer   nchan
      character oper*(*)
      real      rms, buffer(5,nchan)
      complex   uvdata(nchan)
      logical   flags(nchan)
c-----------------------------------------------------------------------
c  Perform the desired operation on the data.
c
c  Input:
c    oper       Operation to perform.
c    rms        Estimate of the visibility rms (only if oper='flag').
c    nchan      Number of channels.
c    buffer     The visibility, model and flag data, returned by the
c               model subroutine.
c  Input/Output:
c       On input, these are the original values.  On output, these are
c       the values after doing whatever operation is called for.
c    uvdata     The data read from the visibility file.
c    flags      The flags read from the visibility file.
c-----------------------------------------------------------------------
      integer i
      real    rms2, temp
c-----------------------------------------------------------------------
      if (oper.eq.'replace') then
c       Replace data with model.
        do i = 1, nchan
          flags(i)  = buffer(5,i).gt.0.0
          uvdata(i) = cmplx(buffer(3,i),buffer(4,i))
        enddo

      else if (oper.eq.'add') then
c       Add model to data.
        do i = 1, nchan
          flags(i)  = buffer(5,i).gt.0.0
          uvdata(i) = uvdata(i) + cmplx(buffer(3,i),buffer(4,i))
        enddo

      else if (oper.eq.'subtract') then
c       Subtract model from data.
        do i = 1, nchan
          flags(i)  = buffer(5,i).gt.0.0
          uvdata(i) = uvdata(i) - cmplx(buffer(3,i),buffer(4,i))
        enddo

      else if (oper.eq.'multiply') then
c       Multiply data by model.
        do i = 1, nchan
          flags(i)  = buffer(5,i).gt.0.0
          uvdata(i) = uvdata(i) * cmplx(buffer(3,i),buffer(4,i))
        enddo

      else if (oper.eq.'divide') then
c       Divide data by model.
        do i = 1, nchan
          flags(i) = buffer(5,i).gt.0.0
          if (flags(i)) then
            uvdata(i) = uvdata(i) / cmplx(buffer(3,i),buffer(4,i))
          endif
        enddo

      else if (oper.eq.'flag') then
c       Flag data.
        rms2 = 2.0*rms*rms
        do i = 1, nchan
          temp =  (real(uvdata(i))-buffer(3,i))**2 +
     *           (aimag(uvdata(i))-buffer(4,i))**2
          flags(i) = flags(i) .and. temp.lt.rms2
        enddo

      else
        call bug('f','Unrecognised operation, in Process')
      endif

      end

c***********************************************************************

      subroutine GetOpt(oper,unflag,mfs,selradec,zero)

      character oper*(*)
      logical unflag,mfs,selradec,zero
c-----------------------------------------------------------------------
c  Get the various processing options.
c
c  Output:
c    oper       One of 'add', 'subtract', 'multiply', 'divide',
c               'replace', 'flag'.  The default is 'add'.
c    unflag     Determine whether we are to unflag the data.
c    selradec   Input uv file contains multiple pointings or multiple
c               sources.
c-----------------------------------------------------------------------
      integer i,j
      integer nopt
      parameter (nopt=10)
      character opts(nopt)*9
      logical present(nopt)
      data opts/    'add      ','divide   ','flag     ','multiply ',
     *  'replace  ','subtract ','unflag   ','mfs      ','mosaic   ',
     *  'zero     '/
c-----------------------------------------------------------------------
      call options('options',opts,present,nopt)

      j = 0
      do i = 1, 6
        if (present(i)) then
          if (j.ne.0) call bug('f',
     *        'Options '//opts(j)//' and '//opts(i)//
     *        ' are mutually exclusive')
          j = i
        endif
      enddo
      if (j.eq.0) call bug ('f', 'You must specify an option')
      oper = opts(j)

      unflag   = present(7)
      mfs      = present(8)
      selradec = present(9)
      zero     = present(10)

      if (oper.eq.'flag' .and. unflag)
     *  call bug('f','You cannot use options=flag,unflag')

      end
