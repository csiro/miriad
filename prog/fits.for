************************************************************************
      program fits

c= fits - Conversion between Miriad and FITS image and uv formats
c& rjs
c: data transfer
c+
c       FITS is a Miriad task that converts image and uv files both
c       from FITS to Miriad format, and from Miriad to FITS format.
c       Note that because there is not a perfect correspondence between
c       all information in a FITS and Miriad file, some information may
c       be lost in the conversion step.  This is particularly true for
c       uv files.
c
c
c       WARNING: When writing uv FITS files, fits can handle single
c       frequency band, single array configuration only. Minimal
c       checks are made to see that these restrictions are observed!
c
c       References:
c         For a description of the standard, see
c           http://fits.gsfc.nasa.gov/fits_home.html
c
c@ in
c       Name of the input file (either a FITS or Miriad file name,
c       depending on OP).  No default.
c@ op
c       This determines the operation to perform. Possible values are:
c         "uvin"    Convert FITS uv file to Miriad uv file.
c         "uvout"   Convert Miriad uv file to FITS uv file.
c         "xyin"    Convert FITS image file to Miriad image file.
c         "xyout"   Convert Miriad image file to FITS image file.
c         "print"   Print out a FITS header.
c       There is no default.
c@ out
c       Name of the output file (either a Miriad or FITS file name,
c       depending on OP).  If op=print, then this parameter is not
c       required.  Otherwise there is no default.
c@ line
c       Line type of the output, when op=uvout. This is of the form:
c
c         linetype,nchan,start,width,step
c
c       "Linetype" is either "channel", "wide" or "velocity".  "Nchan"
c       is the number of channels in the output.
c@region
c       The region of interest. The default is the entire input image.
c       See the Users Manual for instructions on how to specify this.
c       Used when op=xyout
c@ select
c       Normal uv selection, used when op=uvout.
c@ stokes
c       Normal Stokes selection, used when op=uvout
c@ options
c       These options applies for op=uvin only.
c         compress Store the data in compressed uv format.
c         nochi    Assume that the parallactic angle of the
c                  telescope is a constant 0 (or that the data are
c                  from circularly polarised feeds and have already
c                  been corrected for parallactic angle).
c         lefty    Assume that the FITS antenna table uses a
c                  left-handed coordinate system (rather than the
c                  more normal right-handed system).
c         varwt    The visibility weight in the FITS file should
c                  be interpretted as the reciprocal of the noise
c                  variance on that visibility.
c         blcal    Apply AIPS baseline-dependent calibration to the
c                  data.
c
c       These options for op=uvout only.
c         nocal    Do not apply the gains table to the data.
c         nopol    Do not apply the polarization leakage table
c                  to the data.
c         nopass   Do not apply the bandpass table correctsions
c                  to the data.
c
c       These options apply for op=xyin only.
c         rawdss   Use the conventions for raw Digital Sky Survey FITS
c                  files, and convert (partially!) the header. A raw
c                  DSS FITS file has header items such as PLTSCALE,
c                  XPIXELSZ, YPIXELSZ etc. If you are unsure if your DSS
c                  image is raw or conventional FITS, run:
c                    Task FITS:
c                      in=mydss.fits
c                      op=print
c                  and look for those header items.  Note that DSS
c                  images retrieved using SkyView have a conventional
c                  FITS header, and do not require options=rawdss.
c         nod2     Use the conventions of NOD2 FITS files.
c@ velocity
c       Velocity information. This is only used for op=uvin,
c       and is only relevant for line observations.  The default is
c       to use the information present in the FITS header.  The
c       "velocity" parameter allows this information to be overriden or
c       the velocity system to be changed.
c
c       Within each line visibility data-set, Miriad stores the velocity
c       of the observatory wrt a rest frame.  This allows account to be
c       taken of this when determining channel velocities.
c
c       The fits task will determine the observatory velocity either by
c       being given a velocity at a given channel (wrt a rest frame) or
c       by using a model of Earth and solar system motion (accurate to
c       5 m/s).
c
c       The "velocity" parameter can be used to specify the velocity of
c       a particular channel.  The parameter consists of three values:
c       the velocity system of the reference value, the reference value
c       and the reference channel, viz:
c          velocity=velsys,refval,refchan
c       Possible values for the velocity system are:
c         lsr     Velocity is the radio definition wrt the LSR frame.
c         bary    Velocity is the radio definition wrt the barycentre.
c         optlsr  Velocity is the optical definition wrt the LSR frame.
c         optbary Velocity is the optical definition wrt the barycentre.
c         obs     Velocity wrt the observatory.
c
c       The reference value gives the velocity, at the reference
c       channel, in km/s.  If the reference value and reference channel
c       are omitted, a model of Earth and solar system motion is used to
c       determine the appropriate information.
c
c       For example:
c         velocity=lsr,30,1
c       indicates that the first channel has radio LSR velocity of
c       30 km/s.  The observatory velocity, relative to LSR, can then
c       be computed.
c
c       Alternately:
c         velocity=lsr
c       indicates that fits is to determine the observatory velocity
c       wrt the LSR frame using an appropriate model.
c
c$Id: fits.for,v 1.29 2014/06/05 01:28:54 sau078 Exp $
c--
c
c  Bugs:
c    * uvin should check that the phase and pointing centre are the
c      same.  xyin should generate the Miriad obsra and obsdec
c      parameters.
c    * xyin should eliminate dummy Stokes axes in some cases.
c      Percent polarisation not correctly handled.
c    * Should have a "ccin" option to read AIPS clean component tables
c      and convert to Miriad images.
c    * uvout could be infintely smarter (handle multiple windows, write
c      FQ tables, etc).
c    * In uvin 'RESTFREQ' from AIPS SU table is ignored. Also
c      POLAA and POLAB are ignored.
c    * A general mechanism to override wrong stuff from the FITS header
c      is needed. Probably need the user to give an auxillary text file.
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c    mhw 26oct12  Initialize map rotation to zero
c    rjs 21mar13  Fix check for buffer overflow in uvin.
c    mhw 01may14  Cope with HPX rotation
c
c-----------------------------------------------------------------------
      integer   MAXBOXES
      parameter (MAXBOXES=2048)

      logical   altr, compress, dobl, docal, dochi, dopass, dopol, dss,
     *          lefty, nod2, varwt
      integer   boxes(MAXBOXES), velsys
      real      altrpix, altrval
      character in*128, op*8, out*128, uvdatop*12, version*72

      external  versan
      character versan*72
c-----------------------------------------------------------------------
      version = versan('fits',
     *                 '$Revision: 1.29 $',
     *                 '$Date: 2014/06/05 01:28:54 $')
c
c  Get the input parameters.
c
      call keyini
      call GetOp(op)
      in = ' '
      out = ' '
      if (op.ne.'uvout') call keya('in',in,' ')
      if (op.ne.'print') call keya('out',out,' ')

      if (op.eq.'uvin') call GetVel(velsys,altr,altrval,altrpix)
      if (op.eq.'xyout') call BoxInput('region',in,boxes,MAXBOXES)
c
c  Get options.
c
      call getopt(docal,dopol,dopass,dss,nod2,dochi,compress,
     *                                        lefty,varwt,dobl)
      if (op.eq.'uvout') then
        uvdatop = 'sdlb3'
        if (docal) uvdatop(7:7) = 'c'
        if (dopol) uvdatop(8:8) = 'e'
        if (dopass) uvdatop(9:9) = 'f'
        call uvdatinp('in',uvdatop)
      endif

      call keyfin
      if (op.ne.'uvout' .and. in.eq.' ')
     *  call bug('f','Input file name is missing')
      if (op.ne.'print' .and. out.eq.' ')
     *  call bug('f','Output file name is missing')
c
c  Handle the five cases.
c
      if (op.eq.'uvin') then
        call uvin(in,out,velsys,altr,altrpix,altrval,dochi,
     *                          compress,lefty,varwt,dobl,version)
      else if (op.eq.'uvout') then
        call uvout(out,version)
      else if (op.eq.'xyin') then
        call xyin(in,out,version,dss,nod2)
      else if (op.eq.'xyout') then
        call xyout(in,out,version,boxes)
      else if (op.eq.'print') then
        call prthd(in)
      endif

      end

c***********************************************************************

      subroutine GetOp(op)

      character op*(*)
c-----------------------------------------------------------------------
c  Determine the processing option.
c
c  Output:
c    op         The processing option.
c-----------------------------------------------------------------------
      integer nopts
      parameter (nopts=5)

      integer nout
      character opts(nopts)*5
      data opts/'uvin ','uvout','xyin ','xyout','print'/
c-----------------------------------------------------------------------
      call keymatch('op',nopts,opts,1,op,nout)
      if (nout.eq.0) call bug('f','An op must be given')

      end

c***********************************************************************

      subroutine GetVel(velsys,altr,altrval,altrpix)

      integer velsys
      logical altr
      real altrval,altrpix
c-----------------------------------------------------------------------
c  Determine the velocity system.
c
c  Output:
c    velsys     Velocity system desired.
c    altr       True if altrval is set.
c    altrval    Reference velocity.
c    altrpix    Reference channel.
c-----------------------------------------------------------------------
      integer OBSRADIO,OBSOPTIC,LSRRADIO,LSROPTIC,HELRADIO,HELOPTIC
      parameter (OBSRADIO=259,OBSOPTIC=3,LSRRADIO=257,LSROPTIC=1)
      parameter (HELRADIO=258,HELOPTIC=2)

      integer nout
      character string*8

      integer nopts
      parameter (nopts=5)
      integer vals(nopts)
      character opts(nopts)*8

c     Externals.
      integer binsrcha
      logical keyprsnt

      data opts/'bary    ','lsr     ','obs     ',
     *                     'optbary ','optlsr  '/
      data vals/ HELRADIO,  LSRRADIO,  OBSRADIO,
     *                      HELOPTIC,  LSROPTIC/
c-----------------------------------------------------------------------
      call keymatch('velocity',nopts,opts,1,string,nout)
      if (nout.eq.0) then
        velsys = 0
        altr = .false.
      else
        velsys = vals(binsrcha(string,opts,nopts))
        altr = keyprsnt('velocity')
        call keyr('velocity',altrval,0.0)
        call keyr('velocity',altrpix,1.0)
      endif

      end

c***********************************************************************

      subroutine getopt(docal,dopol,dopass,dss,nod2,dochi,
     *                                  compress,lefty,varwt,dobl)

      logical docal,dopol,dopass,dss,dochi,nod2,compress,lefty,varwt
      logical dobl
c-----------------------------------------------------------------------
c  Get a couple of the users options from the command line
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
c            noise variance.
c    dobl    Apply AIPS baseline-dependent calibration.
c-----------------------------------------------------------------------
      integer nopt
      parameter (nopt = 11)
      character opts(nopt)*8
      logical present(nopt),olddss
      data opts /'nocal   ','nopol   ','nopass  ','rawdss  ',
     *           'nod2    ','nochi   ','compress','lefty   ',
     *           'varwt   ','dss     ','blcal   '/
c-----------------------------------------------------------------------
      call options('options', opts, present, nopt)
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
      dobl     =      present(11)

      if (olddss) then
        call bug('w','Option DSS is deprecated. Please use RAWDSS')
        dss=.true.
      endif

      end

c***********************************************************************

      subroutine prthd(in)

      character in*(*)
c-----------------------------------------------------------------------
c  This prints the header of a FITS file.
c
c  Input:
c    in         Name of the FITS file.
c-----------------------------------------------------------------------
      integer lu
      logical more
      character line*80
c-----------------------------------------------------------------------
      call fitopen(lu,in,'old')
      call fitsrch(lu,'SIMPLE',more)
      if (.not.more) call bug('f','Failed to find the SIMPLE keyword')

      do while (more)
        call fitcdio(lu,line)
        do while (line(1:3).ne.'END')
          call output(line)
          call fitcdio(lu,line)
        enddo
        call output('***********************************************')
        call output('***********************************************')
        call ftabSkip(lu,' ',more)
        if (more) call fitsrch(lu,'XTENSION',more)
      enddo
      call fitclose(lu)

      end
c***********************************************************************
      subroutine uvin(in,out,velsys,altr,altrpix,altrval,dochi,
     *                        compress,lefty,varwt,dobl,version)
c
      implicit none
      character in*(*),out*(*)
      integer velsys
      logical altr,dochi,compress,lefty,varwt,dobl
      real altrpix,altrval
      character version*(*)
c-----------------------------------------------------------------------
c  Read in a UV FITS file.
c
c  Inputs:
c    in         Name of the input uv FITS file.
c    out        Name of the output Miriad file.
c    velsys     Velocity system.
c    altr       True if the user specified altrpix and altrval.
c    altrpix    The user given value for altrpix.
c    altrval    The user given value for altrval.
c    dochi      Attempt to calculate the parallactic angle.
c    compress   Store the data in compressed format.
c    lefty      Assume the antenna table uses a left-handed system.
c    varwt      Interpret the visibility weight as the reciprocal of the
c               noise variance.
c    dobl       Apply AIPS baseline-dependent calibration.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer PolXX,PolYY,PolXY,PolYX
      parameter (PolXX=-5,PolYY=-6,PolXY=-7,PolYX=-8)

      integer MAXTIME
      parameter (MAXTIME=10240)
      integer lu,tno,nvis,npol,nfreq,i,j,bitpix,nif
      integer ant1,ant2,nants,bl,nconfig,config,srcid,freqid
      integer itemp,offset,P,Pol0,PolInc
      logical flags(maxchan),zerowt,found,anfound
      logical conj,sfudge,blfound
      complex corr(maxchan)
      real fac,swt
      integer nwt
      real visibs(7+12*maxchan)
      double precision preamble(5),T0,uu,vv,ww,time
      real times(MAXTIME),inttime
      integer ntimes,refbase,litime
      integer uvU,uvV,uvW,uvBl,uvT,uvSrcId,uvFreqId,uvData
      character telescop*32,itime*8

c     Externals.
      integer len1,PolCvt
      character itoaf*8
      double precision fuvGetT0,antbas
c
c  Open the input FITS and output Miriad files.
c
      call fuvOpen(lu,in,'old',nvis,npol,nfreq)
      call fitrdhdi(lu,'BITPIX',bitpix,16)
      if(npol.gt.4)call bug('f','Too many polarizations')
      if(nfreq.gt.maxchan) call bug('f','Too many channels')
c
c  Copy parameters to the output file, and do some general fiddling.
c  If the data in the input FITS file is not 16 bit integers, set it so
c  that the output Miriad file will contain the correlations in real
c  format.
c
      call uvopen(tno,out,'new')
      call hisOpen(tno,'append')
      call histIn(lu,tno,version)
      if (.not.compress .and. abs(bitpix).gt.16)
     *  call uvset(tno,'corr','r',0,0.0,0.0,0.0)
c
c  Determine if its a multisource file, and set the random parameters to
c  handle accordingly.
c
      call Indices(lu,uvU,uvV,uvW,UvT,uvBl,uvSrcId,uvFreqid,uvData)
      if (uvW.eq.0) then
        call uvset(tno,'preamble','uv/time/baseline',0,0.0,0.0,0.0)
      else
        call uvset(tno,'preamble','uvw/time/baseline',0,0.0,0.0,0.0)
      endif
c
c  Load antenna, source and frequency information.  Set frequency
c  information.
c
      call TabLoad(lu,uvSrcId.ne.0,uvFreqId.ne.0,
     *        telescop,anfound,Pol0,PolInc,nif,dochi,lefty,
     *        nants)
      call TabVeloc(velsys,altr,altrval,altrpix)
c
c  Load any FG tables.
c
      call FgLoad(lu,tno)
c
c  Load any BL tables.
c
      blfound = .false.
      if (dobl) call BlInit(lu,nif,blfound)
c
c  Give a summary about various tables.
c
      call tabinfo(lu,blfound)
c
c  Rewind the header.
c
      call ftabLoc(lu,' ',found)
      if (.not.found) call bug('f','Error rewinding to main header')
c
c  Messages about the polarisations present. Note that much ATCA
c  data comes in being labelled as circulars. Tell the user there is
c  a problem, and relabel it as linears.
c
      call output(' ')
      call output('Handling the visibility data')
      if (npol.gt.1) then
        if (Pol0.ge.1 .and. Pol0.le.4) then
          call output('  Data are Stokes correlations')
        else if (Pol0.le.-1 .and. Pol0.ge.-4) then
          call output('  Data are circularly polarized')
        else if (Pol0.le.-5 .and. Pol0.ge.-8) then
          call output('  Data are linearly polarized')
        else
          call bug('w','Unrecognised polarization type')
        endif
      endif
c
c  Do some telescope-specific operations.
c
      sfudge = .false.
      if (telescop.eq.'ATCA') then
        if (Pol0.le.-1 .and. Pol0.ge.-4) then
          Pol0 = -5
          call bug('w',
     *    'Relabelling ATCA circularly polarised data as linear')
        endif
      else if (telescop.eq.'WSRT') then
        call bug('w','Changing sign convention of XY correlations')
        call bug('w','for WSRT data')
        sfudge = .true.
      endif
c
c  Determine the noise scaling factor.
c
      fac = 1
      if (Pol0.gt.0) fac = 2
c
c  Write out the telescope.
c
      if (telescop.ne.' ') call uvputvra(tno,'telescop',telescop)
c
c  Do the descriptions of polarisations and the "IF" axis.
c
      call uvputvri(tno,'nants',0,1)
      call uvputvri(tno,'npol',npol,1)
      call wrhdi(tno,'npol',npol)
c
c  Initialise things to work out the integration time.
c
      if (varwt) then
        call DoTime(lu,nvis,uvT,uvBl,visibs,
     *                        times,MAXTIME,inttime)
        ntimes = MAXTIME + 1
      else
        inttime = 10
        ntimes = 0
      endif
      call uvputvrr(tno,'inttime',inttime,1)
c
c  Copy the data itself. The conversion of units is as follows:
c   Variable            FUVREAD           UVWRITE
c     U                   sec             nanosec
c     V                   sec             nanosec
c     W                   sec             nanosec
c     Time        Offset Julian days    Julian days
c
      nconfig = 0
      zerowt = .false.
      srcid = 1
      freqid = 1
      T0 = fuvGetT0(lu)

      call output('  Reading the correlation data')
      do i = 1, nvis
        call fuvread(lu,visibs,i,1)
c
c  Unpack the preamble.
c
        uu = 1e9 * visibs(uvU)
        vv = 1e9 * visibs(uvV)
        if (uvW.gt.0) then
          ww = 1e9 * visibs(uvW)
        else
          ww = 0
        endif
        time = visibs(uvT) + T0
        call fbasant(visibs(uvBl),ant1,ant2,config)
        if (uvSrcid.gt.0) srcid  = nint(visibs(uvSrcId))
        if (uvFreqid.gt.0) freqid = nint(visibs(uvFreqId))
c
c  Apply baseline calibration if required.
c
        if (blfound) call blfix(ant1,ant2,visibs(uvData),npol,nfreq)
c
c  Convert baseline number to normal Miriad.  Note the different
c  conventions
c    Miriad: bl = 256*ant1 + ant2, where the baseline is ant2 - ant1
c      AIPS: bl = 256*ant1 + ant2                        ant1 - ant2!
c
c  In both cases, ant1 < ant2.
c  the variables ant1,ant2 are Miriad antenna numbers.
c
        conj = ant2.gt.ant1
        if (conj) then
          uu = -uu
          vv = -vv
          ww = -ww
          offset = uvData
          do j = 1, npol*nfreq
            visibs(offset+1) = -visibs(offset+1)
            offset = offset + 3
          enddo
        else
          itemp = ant1
          ant1 = ant2
          ant2 = itemp
        endif

        nants = max(nants,ant1,ant2)
        bl = nint(antbas(ant1,ant2))
        nconfig = max(config,nconfig)
c
c  Determine some times at whcih data are observed. Use these later to
c  guestimate the integration time.
c
        if (i.eq.1) refbase = bl
        if (refbase.eq.bl .and. ntimes.lt.MAXTIME) then
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
        if (varwt) then
          nwt = 0
          swt = 0
          offset = uvData
          do j = 1, npol*nfreq/nif
            if (abs(visibs(offset+2)).gt.0) then
              swt = swt + abs(visibs(offset+2))
              nwt = nwt + 1
            endif
            offset = offset + 3
          enddo
          if (nwt.gt.0) call TabVar(tno,fac*nwt/swt,inttime)
        endif
c
c  Store the correlation data.
c
        preamble(1) = uu
        preamble(2) = vv
        if (uvW.gt.0) then
          preamble(3) = ww
          preamble(4) = time
          preamble(5) = bl
        else
          preamble(3) = time
          preamble(4) = bl
        endif

        do j = 1, npol
          P = Pol0 + (j-1)*PolInc
          if (.not.conj) P = PolCvt(P)
c
c  The following code was once required to work around a labelling
c  bug in NEWSTAR -- I think this bug has now been corrected there.
c
c       if(P.eq.PolYY.or.P.eq.PolXY)then
c         P = P -1
c       else if(P.eq.PolYX)then
c         P = PolYY
c       endif
          call uvputvri(tno,'pol',P,1)
          call Extract(nfreq,npol,j,visibs(uvData),corr,flags,zerowt)
          if (sfudge .and. P.eq.PolYX) call Negate(nfreq,corr)
          call uvwrite(tno,preamble,corr,flags,nfreq)
        enddo
      enddo
c
c  Work out the integration time now, and use override mechanism to set
c  it in the data-set.
c
      if (.not.varwt) call GetInt(times,ntimes,inttime)
      call wrhdr(tno,'inttime',inttime)
      itime = itoaf(nint(inttime))
      litime = len1(itime)
      call output('  The estimated integration time of a sample is '//
     *  itime(1:litime)//' seconds')
c
c  Write out the number of antennas.
c
      if (.not.anfound) call wrhdi(tno,'nants',nants)
      call output('  Number of antennas: '//itoaf(nants))
      call output('  Number of antenna configurations: '//
     *        itoaf(nconfig))

      if (zerowt) call bug('w','Some visibilities had zero weight')
c
c  Close up shop.
c
      if (blfound) call BlFin
      call fuvclose(lu)
      call hisclose(tno)
      call uvclose(tno)

      end

c***********************************************************************

      subroutine DoTime(lu,nvis,uvT,uvBl,visibs,times,
     *                                        maxtimes,inttime)

      integer lu,nvis,uvT,uvBl,maxtimes
      real visibs(*),times(maxtimes),inttime
c-----------------------------------------------------------------------
c  Guestimate the integration time of a sample.
c-----------------------------------------------------------------------
      integer bl,refbl,i,ntimes
c-----------------------------------------------------------------------
      i = 0
      ntimes = 0
      do while (ntimes.lt.MAXTIMES .and. i.lt.nvis)
        i = i + 1
        call fuvread(lu,visibs,i,1)
        bl = int(visibs(uvBl) + 0.01)
        if (i.eq.1) refbl = bl
        if (bl.eq.refbl) then
          ntimes = ntimes + 1
          times(ntimes) = visibs(uvT)
        endif
      enddo

      call GetInt(times,ntimes,inttime)

      end

c***********************************************************************

      subroutine Negate(nfreq,corr)

      integer nfreq
      complex corr(nfreq)
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      do i = 1, nfreq
        corr(i) = -corr(i)
      enddo

      end

c***********************************************************************

      subroutine Indices(lu,uvU,uvV,uvW,uvT,uvBl,uvSrcId,
     *                                                uvFreqid,uvData)

      integer lu
      integer uvU,uvV,uvW,uvT,uvBl,uvSrcid,uvFreqid,uvData
c-----------------------------------------------------------------------
c  Determine indices for the random parameters and data.
c
c  Input:
c    lu         Handle of the FITS file.
c  Output:
c    uvU,uvV,uvW,uvT,uvBl,uvSrcid,uvFreqid,uvData
c-----------------------------------------------------------------------
      integer   iax, naxis, nrandom
      character cax*2, ptype*16, random(7)*8

      external  itoaf
      character itoaf*2
c-----------------------------------------------------------------------
      uvU = 1
      random(1) = 'UU'
      uvV = 2
      random(2) = 'VV'
      uvBl = 3
      random(3) = 'BASELINE'
      uvT = 4
      random(4) = 'DATE'
      nrandom = 4

      uvW = 0
      uvSrcid = 0
      uvFreqid = 0

c     Are the source-id and freq-id random parameters present?
      call fitrdhdi(lu, 'PCOUNT', naxis, 0)
      do iax = 1, naxis
        cax = itoaf(iax)
        call fitrdhda(lu, 'PTYPE'//cax, ptype, ' ')

        if (uvW.eq.0 .and.
     *     (ptype(:3).eq.'WW' .or. ptype(:3).eq.'WW-')) then
          nrandom = nrandom + 1
          uvW     = nrandom
          random(nrandom) = 'WW'
        endif

        if (ptype.eq.'SOURCE' .and. uvSrcid.eq.0) then
          nrandom = nrandom + 1
          uvSrcid = nrandom
          random(nrandom) = 'SOURCE'
        endif

        if (ptype.eq.'FREQSEL' .and. uvFreqid.eq.0) then
          nrandom  = nrandom + 1
          uvFreqid = nrandom
          random(nrandom) = 'FREQSEL'
        endif
      enddo

      uvData = nrandom + 1
      call fuvSetPa(lu,nrandom,random)

      end

c***********************************************************************

      integer function PolCvt(P)

      integer P
c-----------------------------------------------------------------------
c  Fiddle the polarisation labelling around, because of the different
c  labelling conventions of Miriad and FITS.
c
c  Input:
c    P          Polarisation code.
c-----------------------------------------------------------------------
      integer PolYX,PolXY,PolYY,PolXX,PolLR,PolRL
      integer PolMin,PolMax
      parameter (PolYX=-8,PolXY=-7,PolYY=-6,PolXX=-5,PolLR=-4,PolRL=-3)
      parameter (PolMin=-8,PolMax=-3)

      integer pols(PolMin:PolMax)
      data pols/PolXY,PolYX,PolYY,PolXX,PolRL,PolLR/
c-----------------------------------------------------------------------
      PolCvt = p
      if (p.ge.PolMin .and. p.le.PolMax) PolCvt = pols(p)

      end

c***********************************************************************

      subroutine GetInt(times,ntimes,inttime)

      integer ntimes
      real times(ntimes),inttime
c-----------------------------------------------------------------------
c  Make a good guess at the integration time. Given a set of samples of
c  time, this sorts the times, works out time differences, sorts them
c  and uses the minimum positive value as an estimate of the integration
c  time. This needs to be at least one second.
c
c  Input:
c    times      Some samples of the sampling time (in day fractions).
c    ntimes     Number of samples of time.
c  Output:
c    inttime    Some estimate of the integration time (in seconds).
c-----------------------------------------------------------------------
      integer i
      real delta
c-----------------------------------------------------------------------
c  If there is only one value, assume the integration time is 10 sec.
c
      inttime = 10
      if (ntimes.eq.1) return
c
c  Sort the times.
c
      call sortr(times,ntimes)
c
c  Find the first delta time greater than 1 sec as the integration time.
c
      inttime = times(ntimes) - times(1)
      do i = 1, ntimes-1
        delta = times(i+1) - times(i)
        if (delta.gt.1.0/(24.0*3600.0) .and. delta.lt.inttime)
     *    inttime = delta
      enddo
      inttime = 24*3600*inttime

      end

c***********************************************************************

      subroutine BlInit(lu,nif1,blfound)

      integer lu,nif1
      logical blfound
c-----------------------------------------------------------------------
c  Load any AIPS BL tables.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      integer nants,nbl,ntab,npol,nif
      integer pBlTab
      common/blcomm/pBlTab,nants,nbl,ntab,npol,nif
c-----------------------------------------------------------------------
c     Look for BL tables.
      ntab = 0
      call ftabLoc(lu,'AIPS BL',blfound)
      if (blfound) then
        call fitrdhdi(lu,'NO_ANT',nants,0)
        call fitrdhdi(lu,'NO_POL',npol,1)
        call fitrdhdi(lu,'NO_IF',nif,1)
        if (nif.ne.nif1) call bug('f',
     *        'Inconsistent number of IF axes in AIPS BL table')
        blfound = npol.ge.1 .and. nants.ge.1 .and. nif.ge.1
        if (blfound) then
          call output(
     *        '  Using baseline-dependent calibration factors')
          nbl = (nants*(nants+1))/2
          ntab = 2*nbl*npol*nif
          call memAlloc(pBlTab,ntab,'c')
          call bllod(lu,npol,nif,nbl,memc(pBlTab))
        else
          call bug('w',
     *        'Ignoring badly formed baseline calibration table')
          blfound = .false.
        endif
      endif

      end

c***********************************************************************

      subroutine bllod(lIn,npol,nif,nbl,BlTab)

      integer lIn,npol,nbl,nif
      complex BlTab(2,npol,nif,nbl)
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real rpm(MAXWIN),ipm(MAXWIN),rpa(MAXWIN),ipa(MAXWIN)
      integer nrows,nval,i,p,ant1,ant2,bl,ifno
      integer fid,sid,config
      logical conj,warn
      character type*1,units*16
      character idx*9,c*1,line*80
      save idx

c     Externals.
      character stcat*80,itoaf*12

      data idx/'123456789'/
c-----------------------------------------------------------------------
      if (nif.gt.MAXWIN) call bug('f',
     *        'Too many IF columns in AIPS BL table')
      call ftabInfo(lIn,'TIME',type,units,nrows,nval)
      if (nrows.le.0 .or. nval.ne.1 .or. type.ne.'R')
     *  call bug('f','Unrecognised AIPS BL table format')
c
c  Initialise the table.
c
      do bl = 1, nbl
        do ifno = 1, nif
          do p = 1, npol
            BlTab(1,p,ifno,bl) = (1.0,0.0)
            BlTab(2,p,ifno,bl) = (0.0,0.0)
          enddo
        enddo
      enddo
c
c  Loop over all the rows in the FITS table.
c
      warn = .false.
      do i = 1, nrows
        call ftabGeti(lIn,'ANTENNA1',i,ant1)
        call ftabGeti(lIn,'ANTENNA2',i,ant2)
        call ftabGeti(lIn,'FREQ ID',i,fid)
        call ftabGeti(lIn,'SOURCE ID',i,sid)
        call ftabGeti(lIn,'SUBARRAY',i,config)
        if (.not.warn .and.
     *     (fid.ne.1 .or. sid.ne.1 .or. config.ne.1)) then
          warn = .true.
          call bug('i','Probable problem in handling AIPS BL table')
          line = stcat('Freq ID='//itoaf(fid),
     *           stcat(', Src ID='//itoaf(sid),
     *                    ', Config='//itoaf(config)))
          call bug('i',line)
          call bug('i','Task does not know how to handle this')
        endif
        conj = ant1.gt.ant2
        if (conj) then
          bl = ant2 + (ant1*(ant1-1))/2
        else
          bl = ant1 + (ant2*(ant2-1))/2
        endif
        if (min(ant1,ant2).ge.1 .and. bl.le.nbl) then
          do p = 1, npol
            c = idx(p:p)
            call ftabGetr(lIn,'REAL M'//c,i,rpm)
            call ftabGetr(lIn,'IMAG M'//c,i,ipm)
            call ftabGetr(lIn,'REAL A'//c,i,rpa)
            call ftabGetr(lIn,'IMAG A'//c,i,ipa)
            do ifno = 1, nif
            if (rpm(ifno).eq.0.0 .and. ipm(ifno).eq.0.0) rpm(ifno) = 1.0
              if (conj) then
                bltab(1,p,ifno,bl) = cmplx(rpm(ifno),-ipm(ifno))
                bltab(2,p,ifno,bl) = cmplx(rpa(ifno),-ipa(ifno))
              else
                bltab(1,p,ifno,bl) = cmplx(rpm(ifno),ipm(ifno))
                bltab(2,p,ifno,bl) = cmplx(rpa(ifno),ipa(ifno))
              endif
            enddo
          enddo
        endif
      enddo

      end

c***********************************************************************

      subroutine blfix(ant1,ant2,Visibs,np,nfreq)

      integer ant1,ant2,np,nfreq
      real visibs(3,np,nfreq)
c-----------------------------------------------------------------------
c  Apply baseline corrections to visibility data.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      integer nants,nbl,ntab,npol,nif
      integer pBlTab
      common/blcomm/pBlTab,nants,nbl,ntab,npol,nif
c-----------------------------------------------------------------------
      if (min(ant1,ant2).lt.1 .or. max(ant1,ant2).gt.nants) return
      call blfix2(ant1,ant2,visibs,np,nfreq/nif,nif,
     *                                memc(pBlTab),npol,nbl)

      end

c***********************************************************************

      subroutine blfix2(ant1,ant2,visibs,np,nchan,nif,BlTab,npol,nbl)

      integer ant1,ant2,np,nchan,npol,nbl,nif
      real visibs(3,np,nchan,nif)
      complex BlTab(2,npol,nif,nbl)
c-----------------------------------------------------------------------
      logical conj
      integer bl,chan,p,ifno
      complex temp
c-----------------------------------------------------------------------
      conj = ant1.gt.ant2
      if (conj) then
        bl = ant2 + (ant1*(ant1-1))/2
      else
        bl = ant1 + (ant2*(ant2-1))/2
      endif

      do ifno = 1, nif
        do chan = 1, nchan
          do p = 1,min(np,npol)
            if (conj) then
              temp = cmplx(visibs(1,p,chan,ifno),
     *                                visibs(2,p,chan,ifno))*
     *               conjg(bltab(1,p,ifno,bl))
     *                                + conjg(bltab(2,p,ifno,bl))
            else
              temp = cmplx(visibs(1,p,chan,ifno),
     *                                visibs(2,p,chan,ifno))*
     *                     bltab(1,p,ifno,bl)
     *                                +       bltab(2,p,ifno,bl)
            endif
            visibs(1,p,chan,ifno) = real(temp)
            visibs(2,p,chan,ifno) = aimag(temp)
          enddo
        enddo
      enddo

      end

c***********************************************************************

      subroutine BlFin

c-----------------------------------------------------------------------
      integer nants,nbl,ntab,npol,nif
      integer pBlTab
      common/blcomm/pBlTab,nants,nbl,ntab,npol,nif
c-----------------------------------------------------------------------
      if (ntab.ne.0) call memFree(pBlTab,ntab,'c')
      ntab = 0

      end

c***********************************************************************

      subroutine FgLoad(lu,tno)

      integer lu,tno
c-----------------------------------------------------------------------
c  Load any AIPS FG tables.
c-----------------------------------------------------------------------
      double precision Time0
      integer ntab,lTab,iostat
      logical more

c     Externals.
      character itoaf*3
c-----------------------------------------------------------------------
c  Get the reference time, and convert it to a Julian date.
c
      call TabRefT0(Time0)
c
c  Look for FG tables.
c
      ntab = 0
      call ftabLoc(lu,'AIPS FG',more)
      do while (more)
        call output('  Saving off-line flagging table: '//
     *                        'Apply this with task fgflag')
        ntab = ntab + 1
        call haccess(tno,lTab,'aipsfg'//itoaf(ntab),'write',iostat)
        if (iostat.ne.0) then
          call bug('w','Error accessing output table')
          call bugno('f',iostat)
        endif
c
c  Process the AIPS FG table.
c
        call fglod2(lu,lTab,Time0)
c
c  Close up this item.
c
        call hdaccess(lTab,iostat)
        if (iostat.ne.0) then
          call bug('w','Error accessing output table')
          call bugno('f',iostat)
        endif
        call ftabNxt(lu,'AIPS FG',more)
      enddo

      end

c***********************************************************************

      subroutine fglod2(lIn,lTab,Time0)

      integer lIn,lTab
      double precision Time0
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'

      character type*1,units*16
      integer nrows,nval
      integer pSrc,pSub,pAnts,pTime,pIfs,pChans,pFreq
c-----------------------------------------------------------------------
      call ftabInfo(lIn,'SOURCE',type,units,nrows,nval)
      if (nrows.le.0 .or. nval.ne.1 .or. type.ne.'I') then
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

        call Fglod3(lTab,Time0,
     *    nrows,MemI(pSrc),MemI(pSub),MemI(pFreq),MemI(pAnts),
     *          MemR(pTime),MemI(pIfs),MemI(pChans))

        call MemFree(pSrc,nrows,'i')
        call MemFree(pSub,nrows,'i')
        call MemFree(pFreq,nrows,'i')
        call MemFree(pAnts,2*nrows,'i')
        call MemFree(pTime,2*nrows,'r')
        call MemFree(pIfs,2*nrows,'i')
        call MemFree(pChans,2*nrows,'i')
      endif

      end

c***********************************************************************

      subroutine fglod3(lTab,Time0,nrows,
     *  SrcId,SubArray,FreqId,Ants,Time,Ifs,Chans)

      integer lTab,nrows
      double precision Time0
      integer SrcId(nrows),SubArray(nrows),Ants(2,nrows)
      integer Ifs(2,nrows),Chans(2,nrows),FreqId(nrows)
      real Time(2,nrows)
c-----------------------------------------------------------------------
      integer i,length,iostat
      character line*132
c-----------------------------------------------------------------------
      do i = 1, nrows
        length = 0
        if (time(1,i).ne.0 .or. time(2,i).ne.0)
     *    call FgTimOut(time(1,i)+time0,time(2,i)+time0,
     *      line,length)
        if (srcid(i).gt.0)
     *    call FgValOut('srcid',SrcId(i),1,line,length)
        if (freqid(i).gt.0)
     *    call FgValOut('freqid',FreqId(i),1,line,length)
        if (subarray(i).gt.0)
     *    call FgValOut('array',SubArray(i),1,line,length)
        if (ants(1,i).gt.0 .or. ants(2,i).gt.0)
     *    call FgValOut('ant',ants(1,i),2,line,length)
        if (chans(1,i).gt.0 .or. chans(2,i).gt.0)
     *    call FgValOut('chan',chans(1,i),2,line,length)
        if (ifs(1,i).gt.0 .or. ifs(2,i).gt.0)
     *    call FgValOut('ifs',ifs(1,i),2,line,length)

        call hwritea(lTab,line(1:length),iostat)
        if (iostat.ne.0) then
          call bug('w','Error writing to the aipsfg table')
          call bugno('f',iostat)
        endif
      enddo

      end

c***********************************************************************

      subroutine FgValOut(type,vals,nvals,line,length)

      character type*(*),line*(*)
      integer nvals,vals(nvals),length
c-----------------------------------------------------------------------
      integer k
c-----------------------------------------------------------------------
      k = len(type)
      if (length+k+3.gt.len(line))
     *  call bug('f','Buffer overflow, in FgValOut')
      if (length.gt.0) then
        line(length+1:length+k+2) = ','//type//'('
        length = length + k + 2
      else
        line(length+1:length+k+1) = type//'('
        length = length + k + 1
      endif
      call mitoaf(vals,nvals,line(length+1:),k)
      length = length + k + 1
      if (length.gt.len(line))
     *  call bug('f','Buffer overflow, in FgValOut')
      line(length:length) = ')'

      end

c***********************************************************************

      subroutine FgTimOut(t1,t2,line,length)

      double precision t1,t2
      character line*(*)
      integer length
c-----------------------------------------------------------------------
      integer k1,k2
      character time1*32,time2*32

c     Externals.
      integer len1
c-----------------------------------------------------------------------
      call julday(t1,'H',time1)
      k1 = len1(time1)
      call julday(t2,'H',time2)
      k2 = len1(time2)

      if (length.ne.0) then
        if (length.ge.len(line))
     *    call bug('f','Buffer overflow, in FgTimOut')
        length = length + 1
        line(length:length) = ','
      endif

      if (length+k1+k2+len('time(,)').gt.len(line))
     *  call bug('f','Buffer overflow, in FgTimOut')
      line(length+1:length+k1+k2+7) =
     *  'time('//time1(1:k1)//','//time2(1:k2)//')'
      length = length + k1 + k2 + 7

      end

c***********************************************************************

      subroutine FgGeti(lIn,pnt,name,nx,ny)

      integer pnt,nx,ny,lIn
      character name*(*)
c-----------------------------------------------------------------------
c  Check whether something is in the flagging table.  If so get it.  If
c  not set default values to 0.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'

      integer nrow,nval,i
      character type*1,units*24
c-----------------------------------------------------------------------
      call MemAlloc(pnt,nx*ny,'i')
      call ftabInfo(lIn,name,type,units,nrow,nval)
      if (type.eq.' ' .or. nrow*nval.eq.0) then
        do i = pnt, pnt+nx*ny-1
          MemI(i) = 0
        enddo
      else
        if (nx.ne.nval .or. ny.ne.nrow .or. type.ne.'I')
     *    call bug('f','AIPS table has an unexpectedly odd shape')
        call ftabGeti(lIn,name,0,MemI(pnt))
      endif

      end

c***********************************************************************

      subroutine FgGetr(lIn,pnt,name,nx,ny)

      integer pnt,nx,ny,lIn
      character name*(*)
c-----------------------------------------------------------------------
c  Check whether something is in the flagging table.  If so get it.  If
c  not set default values to 0.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'
      integer nrow,nval,i
      character type*1,units*24
c-----------------------------------------------------------------------
      call MemAlloc(pnt,nx*ny,'r')
      call ftabInfo(lIn,name,type,units,nrow,nval)
      if (type.eq.' ' .or. nrow*nval.eq.0) then
        do i = pnt, pnt+nx*ny-1
          MemR(i) = 0
        enddo
      else
        if (nx.ne.nval .or. ny.ne.nrow .or. type.ne.'R')
     *    call bug('f','AIPS table has an unexpectly odd shape')
        call ftabGetr(lIn,name,0,MemR(pnt))
      endif

      end

c***********************************************************************

      subroutine TabInfo(lu,dobl)

      integer lu
      logical dobl
c-----------------------------------------------------------------------
c  Give information on the tables in the file.
c
c-----------------------------------------------------------------------
      integer NTAB
      parameter (NTAB=6)
      character tabs(NTAB)*8
      logical found,givecal
      character ename*16

c     Externals.
      integer binsrcha

      data tabs/'AIPS AN ','AIPS CH ','AIPS FG ',
     *          'AIPS FQ ','AIPS OB ','AIPS SU '/
c-----------------------------------------------------------------------
      call ftabloc(lu,' ',found)
      if (.not.found) call bug('f','Something is screwy')
      call ftabNxt(lu,' ',found)
      givecal = .true.
      do while (found)
        call fitrdhda(lu,'EXTNAME',ename,' ')
c
c  Ignore the tables that are handled elsewhere.
c
        if (binsrcha(ename,tabs,NTAB).ne.0) then
          continue
        else if (ename.eq.'AIPS BL') then
          if (.not.dobl) then
            call output('  Baseline calibration table present')
            call output(
     *        '   ... use options=blcal if you wish to apply this')
          endif
        else if (ename.eq.'AIPS CL' .or. ename.eq.'AIPS NX' .or.
     *          ename.eq.'AIPS SN' .or. ename.eq.'AIPS BP') then
          if (givecal) call output('  Ignoring AIPS calibration tables')
          givecal = .false.
        else if (ename.eq.'AIPS OF') then
          call output('  Ignoring AIPS on-line flagging table')
          call output('   ... it is assumed FILLM applied these.')
        else if (ename.eq.'AIPS PO') then
          call output('  Ingoring AIPS planetary ephemeris table')
        else if (ename.eq.'AIPS WX') then
          call output('  Ignoring AIPS weather table')
        else if (ename.eq.'AIPS TY') then
          call output('  Ignoring AIPS system flux cal table values')
          call output('   ... it is assumed FILLM applied these.')
        else if (ename.ne.' ') then
          call output('  Ignoring unrecognised table type: '//ename)
        else
          call output('  Ignoring unknown table type')
        endif
        call ftabNxt(lu,' ',found)
      enddo

      end

c***********************************************************************

      subroutine TabLoad(lu,dosu,dofq,tel,anfound,Pol0,PolInc,nif0,
     *  dochi,lefty,numants)

      integer lu,Pol0,PolInc,nif0
      logical dosu,dofq,anfound,dochi,lefty
      character tel*(*)
      integer numants
c-----------------------------------------------------------------------
c  Determine some relevant parameters about the FITS file. Attempt to
c  ferret this information from all nooks and crannies. In general use
c  the procedure of looking at known telescope parameters, the main
c  FITS header, and then any extension tables. As particular information
c  could be found at any level, use the higher level stuff as defaults
c  for the lower level values.
c
c  AIPS tables read include AN, FQ, CH and SU tables.  NOTE that CL
c  tables are not handled.
c
c  Input:
c    lu         Handle of the input FITS file.
c    dosu,dofq  Expect a multisource/multi-freq file.
c    dochi      Attempt to compute the parallactic angle.
c    lefty      Assume the antenna table uses a left-handed system.
c  Output:
c    tel        Telescope name.
c    anfound    True if antenna tables were found.
c    nif0       Number of IFs
c    Pol0       Code for first polarisation.
c    PolInc     Increment between polarisations.
c    numants    Total number of antennas in AN table
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'fits.h'

      logical   badan, badapp, badepo, badmnt, found, more,docio
      integer   i, itemp, j, n, naxis, nd, nval, nxyz, sta(MAXANT), t
      real      defepoch, diff, rsdf(MAXFREQ*MAXIF)
      double precision coord(3,4), d0, dtemp, eporef, freq, r0, rfreq,
     *          vddef, veldef, xc, xyz(3,MAXANT), yc, zc
      character cax*2, ctype*8, defsrc*16, type*1, units*16
c
c     Externals.
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
      if (telescop.eq.' ')
     *  call fitrdhda(lu,'INSTRUME',telescop,' ')
      tel = telescop
      call fitrdhda(lu,'OBSERVER',observer,' ')
      call fitrdhda(lu,'OBJECT',defsrc,' ')
      call fitrdhdr(lu,'EPOCH',defepoch,2000.0)
c
c  Determine reference time.
c
      call fitdate(lu,'DATE-OBS',timeref)
      if (timeref.eq.0) timeref = fuvGetT0(lu)
c
c  Get velocity definition information, just in case this is a spectral
c  line observation.
c
      call fitrdhdd(lu,'RESTFREQ',rfreq,0d0)
      if (rfreq.eq.0d0) call fitrdhdd(lu,'RESTFRQ',rfreq,0d0)
      call fitrdhdr(lu,'ALTRPIX',velref,real(Coord(uvCrpix,uvFreq)))
      call fitrdhdi(lu,'VELREF',velsys,257)
      freq = (velref-Coord(uvCrpix,uvFreq))*Coord(uvCdelt,uvFreq)
     *         + Coord(uvCrval,uvFreq)
      if (rfreq.gt.0 .and. freq.gt.0) then
        if (velsys.lt.256) then
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
      do i = 2, naxis
        cax = itoaf(i)
        call fitrdhda(lu,'CTYPE'//cax,ctype,' ')
        if (ctype.eq.'IF') call fitrdhdi(lu,'NAXIS'//cax,nif,1)
        if (ctype.eq.'FREQ') call fitrdhdi(lu,'NAXIS'//cax,nchan,1)
      enddo
      if (nif.gt.MAXIF) call bug('f','Too many IFs')
      nif0 = nif
c
c  Polarisation information.
c
      Pol0 = nint(Coord(uvCrval,uvStokes) +
     *        (1-Coord(uvCrpix,uvStokes))*Coord(uvCdelt,uvStokes))
      PolInc = nint(Coord(uvCdelt,uvStokes))

      call output('Determining various defaults ...')
c
c  Set default values for reference freq, lat, long, mount, evector.
c  Also determine the only values for systemp and jyperk.
c
      call telpar(telescop,systemp,systok,jyperk,jok,
     *  llok,lat,long,emok,evec,mount)
      if (.not.emok .and. dochi) call bug('w',
     *  'Insufficient information to determine parallactic angle')
      emok = emok .and. dochi
      freqref(1) = 1d-9 * Coord(uvCrval,uvFreq)
c
c  Load the antenna table.
c
      call output(' ')
      call output('Analysing the extension tables ...')
      docio = telescop.eq.'askap'
      if(docio)call bug('i','Fudging ASKAP antenna table')
      nconfig = 0
      call ftabLoc(lu,'AIPS AN',found)
      anfound = found
      do while (found)
        call output('  Using antenna table information')
        nconfig = nconfig + 1
        call ftabInfo(lu,'STABXYZ',type,units,n,nxyz)

        if (nconfig.gt.MAXCONFG)
     *    call bug('f','Too many array configurations')
        if (nxyz.ne.3 .or. n.le.0 .or. type.ne.'D')
     *    call bug('f','Something is screwy with the antenna table')
        if (n.gt.MAXANT) call bug('f','Too many antennas for me')

        call ftabGeti(lu,'NOSTA',0,sta)

        nd = 0
        do i = 1, n
          if (sta(i).le.0) call bug('f','Invalid antenna numbers')
          nd = max(nd,sta(i))
        enddo
        if (nd.gt.MAXANT) call bug('f','Too many antennas for me')
        if (nd.lt.n) call bug('f','Invalid antenna table')
        numants = max(numants,nd)
        nants(nconfig) = nd
c
c  Get the antenna coordinates.
c  Convert to earth-centred coordinates. The AIPS coordinates have X
c  being in the direction from the earth centre to the Grennwich
c  meridian, and Z being towards the pole.
c
        call fitrdhdd(lu,'ARRAYX',xc,0d0)
        call fitrdhdd(lu,'ARRAYY',yc,0d0)
        call fitrdhdd(lu,'ARRAYZ',zc,0d0)
        call ftabGetd(lu,'STABXYZ',0,xyz)
        call antproc(lefty,docio,xc,yc,zc,xyz,n,sta,nd,
     *	      antpos(1,nconfig),
     *        lat(nconfig),long(nconfig),badan)
        llok = llok .or. .not.badan
c
c  Get the reference freqeuncy. Note that multiple bugs in AIPS
c  make the reference frequency (either in AN table or header)
c  suspect when subsetting multi-IF data.
c
        call fitrdhdd(lu,'FREQ',freqref(nconfig),
     *                                Coord(uvCrval,uvFreq))
        if (freqref(nconfig).le.1) then
          freqref(nconfig) = Coord(uvCrval,uvFreq)
          call bug('w','Antenna table reference frequency looks bad')
          call bug('w','Using header reference frequency')
        else if (abs(freqref(nconfig)-Coord(uvCrval,uvFreq)).gt.
     *     0.01*abs(freqref(nconfig)) .and. nconfig.eq.1) then
          call bug('w',
     *      'Header and antenna table reference frequency differ')
          call bug('w','Using antenna table reference frequency')
        endif
        freqref(nconfig) = 1d-9 * freqref(nconfig)
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
        do i = 1, n
          badmnt = badmnt .or. sta(i).ne.mount(nconfig)
        enddo
        if (badmnt) call bug('w',
     *    'Mount types differed between antennas')
        emok = emok .and. .not.badmnt

        call ftabNxt(lu,'AIPS AN',found)
      enddo
c
c  If no antenna table was found, try for an OB table!
c
      if (.not.anfound) then
        found = .false.
        call ftabLoc(lu,'AIPS OB',found)
        anfound = found
        if (found) then
          call output('Using orbit parameter table information')

          nconfig = 1
          call ftabInfo(lu,'ORBXYZ',type,units,n,nxyz)

          if (nxyz.ne.3 .or. n.le.0 .or. type.ne.'D')
     *      call bug('f','Something is screwy with the OB table')
          if (n.gt.MAXANT) call bug('f','Too many antennas for me')

          call ftabGeti(lu,'ANTENNA_NO',0,sta)

          nd = 0
          do i = 1, n
            if (sta(i).le.0) call bug('f','Invalid antenna numbers')
            nd = max(nd,sta(i))
          enddo
          if (nd.gt.MAXANT) call bug('f','Too many antennas for me')

          if (nd.ne.n) call bug('w',
     *      ' Some antennas were missing from the antenna table ')
          if (nd.lt.n) call bug('f',
     *      'Invalid antenna table')
          numants = max(numants,nd)
          nants(nconfig) = nd
c
c  Get the antenna coordinates.
c
          call fitrdhdd(lu,'ARRAYX',xc,0d0)
          call fitrdhdd(lu,'ARRAYY',yc,0d0)
          call fitrdhdd(lu,'ARRAYZ',zc,0d0)
          call ftabGetd(lu,'ORBXYZ',0,xyz)
          call antproc(lefty,.false.,xc,yc,zc,xyz,n,sta,nd,
     *	      antpos(1,nconfig),
     *        lat(nconfig),long(nconfig),badan)
          llok = llok .or. .not.badan

          freqref(nconfig) = 1d-9*Coord(uvCrval,uvFreq)
          timeoff(nconfig) = 0
          mount(nconfig) = 0
          emok = .false.
          badmnt = .true.

        endif
      endif
c
c  Summarise info about the antenna characteristics.
c
      if (nconfig.eq.0) call bug('w',
     *    'No antenna table was found')
      if (.not.llok) call bug('w',
     *    'Telescope latitude/longitude could not be determined')
c
c  Load the FQ table if its present.
c
      found = .false.
      call ftabLoc(lu,'AIPS FQ',found)
      if (found) then
        call ftabInfo(lu,'FRQSEL',type,units,nfreq,nval)
        if (nfreq.gt.1 .and. .not.dofq)
     *    call bug('f','FQ table present for non-multi-freq file')
        if (nfreq.gt.MAXFREQ) call bug('f','Too many freqs')
        if (nval.ne.1 .or. type.ne.'I')
     *    call bug('f','Something screwy with FQ table')
        call fitrdhdi(lu,'NO_IF',itemp,nif)
        if (itemp.ne.nif)
     *    call bug('f','Inconsistent number of IFs')
        call output('  Using frequency table information')
        call ftabGeti(lu,'FRQSEL',0,freqids)
        if (.not.dofq) freqids(1) = 1
        call ftabGetd(lu,'IF FREQ',0,sfreq)
        call ftabGetr(lu,'CH WIDTH',0,rsdf)
        do i = 1, nif*nfreq
          sdf(i) = rsdf(i)
        enddo
      else
c
c  Load a CH table, if its present.
c
        call ftabLoc(lu,'AIPS CH',found)
        if (found) then
          call ftabInfo(lu,'IF NO.',type,units,nif,nval)
          if (nif.gt.MAXIF) call bug('f','Too many IFs')
          if (nval.ne.1 .or. type.ne.'I')
     *      call bug('f','Something screwy with CH table')
          call ftabGeti(lu,'IF NO.',0,freqids)
c
c  Check that the if table is in the standard order.
c
          do i = 1, nif
            if (freqids(i).ne.i)
     *        call bug('f','Software bug IFNO.ne.IFNO')
          enddo

          call output('  Using channel table information')
          nfreq = 1
          freqids(1) = 1
          call ftabGetd(lu,'FREQUENCY OFFSET',0,sfreq)
          do i = 1, nif
            sdf(i) = Coord(uvCdelt,uvFreq)
          enddo
c
c  If neither a CH or FQ table were found, just use the info in the
c  header.
c
        else
          if (dofq) call bug('w',
     *                'Neither an FQ nor CH table were found')
          nfreq = 1
          freqids(1) = 1
          do i = 1, nif
            sfreq(i) = 0
            sdf(i)   = Coord(uvCdelt,uvFreq)
          enddo
        endif
      endif
c
c  Convert the frequencies to the form that Miriad wants -- in GHz and
c  relative to channel 1.
c
      dtemp = 1 - Coord(uvCrpix,uvFreq)
      do i = 1, nif*nfreq
        sfreq(i) = 1d-9 * (sfreq(i) + dtemp * sdf(i))
        sdf(i)   = 1d-9 * sdf(i)
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
      if (dosu) call ftabLoc(lu,'AIPS SU',found)
      if (.not.found) then
        if (dosu) call bug('w','AIPS SU table not found')
        nsrc = 1
        srcids(1) = 1
        source(1) = defsrc
        raepo(1) = Coord(uvCrval,uvRa)
        raapp(1) = Coord(uvCrval,uvRa)
        decepo(1) = Coord(uvCrval,uvDec)
        decapp(1) = Coord(uvCrval,uvDec)
        epoch(1) = defepoch
        veldop(1) = veldef
        do i = 1, nif
          freqoff(i) = 0
          restfreq(i) = rfreq
        enddo
      else
        call VelGetty(lu,velsys)
        call fitrdhdi(lu,'NO_IF',t,nif)
        if (t.ne.nif) call bug('f','Number of IFs is inconsistent')
        call ftabInfo(lu,'ID. NO.',type,units,nsrc,nval)
        if (nsrc.gt.MAXSRC) call bug('f','Too many sources in SU table')
        if (nval.ne.1 .or. type.ne.'I')
     *    call bug('f','Something screwy with SU table')
        call output('  Using source table information')
        call ftabGeti(lu,'ID. NO.',0,srcids)
        call ftabGeta(lu,'SOURCE',0,source)
        call ftabGetd(lu,'RAEPO',0,raepo)
        call ftabGetd(lu,'DECEPO',0,decepo)
        call ftabGetd(lu,'RAAPP',0,raapp)
        call ftabGetd(lu,'DECAPP',0,decapp)
        call ftabGetd(lu,'EPOCH',0,epoch)
        call ftabGetd(lu,'FREQOFF',0,freqoff)
        call ftabGetd(lu,'LSRVEL',0,restfreq)
        do i = 1, nsrc
          veldop(i) = restfreq((i-1)*nif+1)
        enddo
        call ftabGetd(lu,'RESTFREQ',0,restfreq)
      endif
c
c  Check that everything looks OK.
c
      badapp = .false.
      badepo = .false.
      do i = 1, nsrc
        call lcase(source(i))
        if (nint(epoch(i)+1).eq.0) then
          epoch(i) = Jul2Epo(timeref,' ')
        else if (epoch(i).lt.1850.0 .or. epoch(i).gt.2150.0) then
          badepo = .true.
          epoch(i) = defepoch
        endif
        raepo(i) = mod(raepo(i),360d0)
        if (raepo(i).lt.0) raepo(i) = raepo(i) + 360d0
        raapp(i) = mod(raapp(i),360d0)
        if (raapp(i).lt.0) raapp(i) = raapp(i) + 360d0
        diff = max(abs(raapp(i)-raepo(i)),abs(decapp(i)-decepo(i)))
        raepo(i)  =  raepo(i)*DD2R
        decepo(i) = decepo(i)*DD2R
        raapp(i)  =  raapp(i)*DD2R
        decapp(i) = decapp(i)*DD2R
c
c  If the apparent RA and DEC look bad, recompute them.
c
        eporef = epo2jul(epoch(i),' ')
        if ((diff.gt.1 .and. abs(eporef-timeref).gt.1)
     *        .or. 3600*diff.lt.1) then
          badapp = .true.
          call Precess(eporef, raepo(i),decepo(i),
     *                 timeref,raapp(i),decapp(i))
          call Nutate(timeref,raapp(i),decapp(i),r0,d0)
          call Aberrate(timeref,r0,d0,raapp(i),decapp(i))
        endif
      enddo

      if (badepo) call bug('w',
     *  'Some epochs looked bad -- they were modified')
      if (badapp .and. dosu) call bug('w',
     *  'Some apparent RA/DECs looked bad -- they were recomputed')
c
c  Scale the source-specific frequencies, rest frequencies and
c  velocities.
c
      do i = 1, nif*nsrc
        freqoff(i)  = freqoff(i) *1e-9
        restfreq(i) = restfreq(i)*1e-9
      enddo
      do i = 1, nsrc
        veldop(i) = veldop(i)*1e-3
      enddo
c
c  If there are multiple sources with the same name, assume they are
c  part of a mosaicking experiment, and change them to Miriad's dra/ddec
c  way of specifying things.
c
      mosaic = .false.
      do i = 1, nsrc
        dra(i) = 0
        ddec(i) = 0
        more = .true.
        j = i-1
        do while (j.gt.0 .and. more)
          if (source(i).eq.source(j)) then
            dra(i)  = (raepo(i) - raepo(j)) * cos(decepo(i))
            ddec(i) = decepo(i) - decepo(j)
            raepo(i) = raepo(j)
            decepo(i) = decepo(j)
            more = .false.
            mosaic = mosaic .or. (abs(dra(i))+abs(ddec(i)).gt.0.1*AS2R)
          endif
          j = j - 1
        enddo
      enddo

      call Sortie(sindx,srcids,nsrc)

      end
c***********************************************************************
	subroutine antproc(lefty,docio,xc,yc,zc,xyz,n,sta,nd,antpos,
     *							lat,long,badan)
c
	implicit none
	integer n,nd
	integer sta(n)
	double precision xc,yc,zc,antpos(nd,3),lat,long,xyz(3,n)
	logical lefty,badan,docio
c
c  Fiddle the antenna information.
c
c  Intput:
c    lefty	The antenna table is in a left handed coordinate system.
c    xc,yc,zc	Array centre.
c    xyz	Antenna positions relative to the array centre.
c    n		Number of antennas.
c
c  Output:
c    badan	The antenna coordinates were bad, and the remaining
c		values are unset.
c    lat,long	The latitude and longitude of the observatory.
c    antpos	Antenna positions, in Miriad format.
c-----------------------------------------------------------------------
	include 'mirconst.h'
	double precision r,cost,sint,height,temp,xref,yref,zref
	logical dolocal
	integer i,idx
c
c  Initialise the antenna table to zeros.
c
	do i=1,nd
	  antpos(i,1) = 999999
	  antpos(i,2) = 999999
	  antpos(i,3) = 999999
	enddo
c
c  Determine the latitude, longitude and height of the first antenna
c  (which is taken to be the observatory lat,long,height). Handle
c  geocentric and local coordinates.
c
c  Convert them to the Miriad system: y is local East, z is parallel to
c  pole Units are nanosecs.
c
	badan = .false.
	if(abs(xc)+abs(yc)+abs(yc).eq.0)then
	  call goodxyz(idx,xyz,n)
	  if(idx.ne.0)then
	    xref = xyz(1,idx)
	    yref = xyz(2,idx)
	    zref = xyz(3,idx)
	    dolocal = .false.
	  else
	    badan = .true.
	  endif
	else
	  xref = xc
	  yref = yc
	  zref = zc
	  dolocal = .not.docio
	endif
c
	if(badan)then
	  call bug('w','Bad antenna coordinates ignored')
	  lat = 0
	  long = 0
	  height = 0
	else
	  call xyz2llh(xref,yref,zref,lat,long,height)
	  if(dolocal)then
	    do i=1,n
	      antpos(sta(i),1) = (1d9/DCMKS) * xyz(1,i)
	      antpos(sta(i),2) = (1d9/DCMKS) * xyz(2,i)
	      antpos(sta(i),3) = (1d9/DCMKS) * xyz(3,i)
	    enddo
	  else
	    r = sqrt(xref*xref + yref*yref)
	    cost = xref / r
	    sint = yref / r
	    do i=1,n
	      temp = (xyz(1,i)+xc)*cost + (xyz(2,i)+yc)*sint - r
	      antpos(sta(i),1) = (1d9/DCMKS) * temp
	      temp = -(xyz(1,i)+xc)*sint + (xyz(2,i)+yc)*cost
	      antpos(sta(i),2) = (1d9/DCMKS) * temp
	      antpos(sta(i),3) = (1d9/DCMKS) * (xyz(3,i)+zc-zref)
	    enddo
	  endif
c
c  If the antenna table uses a left-handed system, convert it to a
c  right-handed system.
c
	  if(lefty)then
	    long = -long
	    do i=1,nd
	      if(antpos(i,2).ne.999999)antpos(i,2) = -antpos(i,2)
	    enddo
	  endif
	endif
c
	end
c***********************************************************************

      subroutine goodxyz(idx,xyz,n)

      integer idx,n
      double precision xyz(3,n)
c-----------------------------------------------------------------------
c  Locate the first antenna coordinate that looks valid. If no valid
c  coordinates are found, return 0.
c
c  Input:
c    xyz        Antenna coordinates.
c    n          Number of antennas.
c  Output:
c    idx        Index of first good antenna.
c-----------------------------------------------------------------------
      real r
      integer i
c-----------------------------------------------------------------------
      idx = 0
      i = 0
      do while (idx.eq.0 .and. i.lt.n)
        i = i + 1
        r = xyz(1,i)*xyz(1,i)+xyz(2,i)*xyz(2,i)+xyz(3,i)*xyz(3,i)
        if (r.gt.1e12) idx = i
      enddo

      end

c***********************************************************************

      subroutine TabVar(tno,var,inttime)

      integer tno
      real var,inttime
c-----------------------------------------------------------------------
c  Determine the Jyperk from variance, integration time, channel
c  bandwidth and system temperature.
c-----------------------------------------------------------------------
      include 'fits.h'
      real temp
c-----------------------------------------------------------------------
      temp = 50.0
      if (systok) temp = systemp

      call uvputvrr(tno,'jyperk',sqrt(abs(2*inttime*dnu*var))/temp,1)

      end

c***********************************************************************

      subroutine TabRefT0(t)

      double precision t
c-----------------------------------------------------------------------
c  Return the reference time.
c-----------------------------------------------------------------------
      include 'fits.h'
c-----------------------------------------------------------------------
      t = timeref
      if (nconfig.ge.1) t = t + timeoff(1)

      end

c***********************************************************************

      subroutine TabTime(lu,nconfig,jdateobs,timeoff)

      integer lu,nconfig
      double precision jdateobs,timeoff
c-----------------------------------------------------------------------
c  Determine the time correction to add to the FITS time to convert
c  it to a true UT1 time (as best as we can). This involves both
c  an offset between the time system and UT1, and offsets to remove
c  fudges performed by AIPS DBCON.
c-----------------------------------------------------------------------
      character timsys*16,line*80,s*8
      double precision jrdate,datutc,ut1utc
      integer ltsys,ls

c     Externals.
      integer len1
      character itoaf*8
      double precision deltime
c-----------------------------------------------------------------------
c  Determine the offset times and the time system.
c
      call fitrdhdd(lu,'DATUTC',datutc,0d0)
      call fitrdhdd(lu,'UT1UTC',ut1utc,0d0)
      call fitrdhda(lu,'TIMSYS',timsys,'UTC')
      if (timsys.eq.' ') timsys = 'UTC'
      ltsys = len1(timsys)
c
c  Determine the reference time.
c
      call fitdate(lu,'RDATE',jrdate)
      if (jrdate.eq.0) jrdate = jdateobs
c
c  If there is not DATA to UTC time correction present, check if the
c  time is IAT time. If so, work our the time difference.
c
      if ((timsys(1:3).eq.'IAT' .or. timsys(1:3).eq.'TAI') .and.
     *  datutc.eq.0) datutc = 24*3600*deltime(jrdate,'tai')
c
c  Give messages about what we are going.
c
      if (timsys(1:2).ne.'UT' .and. datutc.eq.0) then
        call bug('w','The time offset '//timsys(1:ltsys)//
     *        '-UTC is claimed to be 0.')
      else if (datutc.ne.0) then
        s = itoaf(nconfig)
        ls = len1(s)
        write(line,'(a,a,a,f5.1,a)')
     *    '  Decrementing times for configuration ',s(1:ls),' by',
     *        datutc,' seconds ('//timsys(1:ltsys)//'-UTC).'
        call output(line)
      endif

      if (ut1utc.ne.0) then
        s = itoaf(nconfig)
        ls = len1(s)
        write(line,'(a,a,a,f6.2,a)')
     *    '  Decrementing times for configration ',s(1:ls),' by',
     *        -ut1utc,' seconds (UTC-UT1).'
        call output(line)
      endif

      if (nconfig.gt.1) then
        write(line,'(a,i3,a,i3,a)')
     *    '  Fiddling times for configuration',nconfig,
     *    ' to remove AIPS DBCON fudges.'
        call output(line)
      endif

      timeoff = (ut1utc - datutc) / (24*3600)
     *                - jdateobs + jrdate - 5*(nconfig-1)

      end

c***********************************************************************

      subroutine TabVeloc(altsys,altr,altrval,altrpix)

      integer altsys
      logical altr
      real altrpix,altrval
c-----------------------------------------------------------------------
      include 'fits.h'
      integer i,i0
      integer OBSRADIO,OBSOPTIC
      parameter (OBSRADIO=3,OBSOPTIC=259)
c-----------------------------------------------------------------------
c  Determine the mode that we are using to compute velocities.
c
      if (altsys.eq.OBSRADIO .or. altsys.eq.OBSOPTIC) then
        velcomp = .false.
        velsys = altsys
      else if (altr .and. altsys.ne.0) then
        velcomp = .false.
        velsys = altsys
        velref = altrpix
        do i = 1, nsrc
          veldop(i) = altrval
        enddo
      else if (altsys.ne.0) then
        velcomp = .true.
        velsys = altsys
      else
        velcomp = .false.
      endif
c
c  Compute the velocity if it is fixed (e.g. the data have been Doppler
c  tracked or the like).
c
      if (.not.velcomp) then
        do i = 1, nsrc
          i0 = (i-1)*nif + 1
          call VelCvt(velsys,velref,restfreq(i),
     *        sfreq(1)+freqoff(i0)+freqref(1),sdf(1),veldop(i))
        enddo
      endif
c
c  Convert the velocity system to a radio one -- as thats the only thing
c  that Miriad supports.
c
      if (velsys.ge.1 .and. velsys.le.256) velsys = velsys + 256

      end

c***********************************************************************

      subroutine VelGetty(lu,velsys)

      integer lu,velsys
c-----------------------------------------------------------------------
c  Determine the velocity system.
c
c  Input:
c    lu
c  Output:
c    velsys
c-----------------------------------------------------------------------
      character string*16
c-----------------------------------------------------------------------
      call fitrdhda(lu,'VELTYP',string,'OBS')
      if (string.eq.' ') string = 'OBS'
      velsys = 0
      if (string(1:3).eq.'OBS') then
        velsys = 3
      else if (string(1:3).eq.'HEL') then
        velsys = 2
      else if (string(1:3).eq.'LSR') then
        velsys = 1
      endif
      call fitrdhda(lu,'VELDEF',string,'RADIO')
      if (string.eq.' ') string = 'RAD'
      if (string(1:3).eq.'RAD') velsys = velsys + 256

      end

c***********************************************************************

      subroutine VelCvt(velsys,velref,restfreq,f,df,veldop)

      integer velsys
      real velref
      double precision restfreq,f,veldop,df
c-----------------------------------------------------------------------
c  Convert a velocity from being the velocity of a channel to the
c  radial velocity of the observatory.
c
c  Input:
c    velsys     Velocity system (1=LSR, 2=HEL, 3=OBS, plus 256 for radio
c               definition)
c    velref     Velocity (km/s) at reference pixel.
c    restfreq   Rest frequency (GHz).
c    f          Frequency (GHz) at channel 1.
c    df         Channel frequency (GHz) increment.
c  Input/Output:
c    veldop     On input, the velocity (km/s) at the reference pixel.
c               On output, the radial velocity of the rest frame (km/s).
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer OBSRADIO,OBSOPTIC,LSRRADIO,LSROPTIC,HELRADIO,HELOPTIC
      parameter (OBSRADIO=259,OBSOPTIC=3,LSRRADIO=257,LSROPTIC=1)
      parameter (HELRADIO=258,HELOPTIC=2)

      double precision fref
c-----------------------------------------------------------------------
c  Determine the frequency at the reference channel.
c
      fref = f + (velref-1)*df
c
c  Given this frequency, determine the velocity that the frequency
c  difference corresponds to.
c
      if (velsys.eq.OBSRADIO .or. velsys.eq.OBSOPTIC .or.
     *                                restfreq.eq.0) then
        veldop = 0
      else if (velsys.eq.HELRADIO .or. velsys.eq.LSRRADIO) then
        veldop = 0.001*CMKS*(restfreq/fref*(1-veldop/(0.001*CMKS))-1)
      else if (velsys.eq.HELOPTIC .or. velsys.eq.LSROPTIC) then
        veldop = 0.001*CMKS*(restfreq/fref/(1+veldop/(0.001*CMKS))-1)
      else
        call bug('f','Unrecognised velocity system, in VelCvt')
      endif

      end

c***********************************************************************

      subroutine telpar(telescop,systemp,systok,jyperk,jok,
     *        latlong,latitude,longitud,polinfo,chioff,mount)

      character telescop*(*)
      integer mount
      double precision latitude,longitud
      real chioff,systemp,jyperk
      logical latlong,polinfo,systok,jok
c-----------------------------------------------------------------------
c  Determine default characteristics of the observatory (in case
c  no AIPS AN file exists).
c
c  Input:
c    telescop   Telescope name.
c  Output:
c    systok     True if systemp has been initialised.
c    systemp    Typical system temperature.
c    jok        True if jyperk has been initialised.
c    jyperk     System gain.
c    latlong    True if the latitude and longitude are known.
c    latitude)  Observatory latitude and longitude, in radians.
c    longitud)
c    polinfo    True if mount and chioff have been initialised.
c    chioff     The position angle of the X feed with respect to the
c               local vertical.
c    mount      Antenna mount type.
c-----------------------------------------------------------------------
      double precision dtemp
      logical ok

c     Externals.
      character itoaf*8
c-----------------------------------------------------------------------
c  Get the info we need from the obspar routine.
c
      if (telescop.eq.' ') then
        call bug('w','Telescope name was not present in the'//
     *                                                ' FITS file')
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
        if (systok) then
          call output('  Assuming systemp='//itoaf(int(dtemp)))
          systemp = dtemp
        else
          call bug('w','Unable to guess typical system temperature')
        endif
c
c  System gain.
c
        call obspar(telescop,'jyperk',dtemp,jok)
        if (jok) then
          call output('  Assuming jyperk='//itoaf(int(dtemp)))
          jyperk = dtemp
        else
          call bug('w','Unable to guess typical system gain')
        endif
c
c  Latitude, longitude, evector and mount type.
c
        call obspar(telescop,'latitude',latitude,latlong)
        call obspar(telescop,'longitude',longitud,ok)
        latlong = latlong .and. ok
c
c  Mount and evector.
c
        call obspar(telescop,'evector',dtemp,ok)
        if (ok) then
          chioff = dtemp
        else
          chioff = 0
          call output('  Assuming feed angle is 0 degrees')
        endif
        call obspar(telescop,'mount',dtemp,polinfo)
        if (polinfo) mount = nint(dtemp)
      endif

      end

c***********************************************************************

      subroutine Sortie(indx,id,n)

      integer n,indx(n),id(n)
c-----------------------------------------------------------------------
c  Sort the ids into increasing order, to make it easier to search for
c  them later.
c
c  Input:
c    n          Number of entries.
c  Input/Output:
c    id         On input, the list of ids.  On output, the sorted list
c               of ids.
c  Output:
c    indx       Index rubbish.
c-----------------------------------------------------------------------
      integer j,k,tid,tindx
      logical more
c-----------------------------------------------------------------------
c  The table is probably already in increasing order. Do a simple
c  sort on it, keeping track of indices, to make sure this is so.
c
      do j = 1, n
        indx(j) = j
      enddo

      do j = 2, n
        k = j
        tindx = indx(j)
        tid = id(j)
        more = id(j-1).gt.tid
        do while (more)
          id(k) = id(k-1)
          indx(k) = indx(k-1)
          k = k - 1
          more = .false.
          if (k.gt.1) more = id(k-1).gt.tid
        enddo
        indx(k) = tindx
        id(k) = tid
      enddo

      end

c***********************************************************************

      subroutine TabInit(tno)

      integer tno
c-----------------------------------------------------------------------
      include 'fits.h'
      integer i,nschan(MAXIF),ischan(MAXIF)
      character veltype(3)*8
      data veltype/'VELO-LSR','VELO-HEL','VELO-OBS'/
c-----------------------------------------------------------------------
      call uvputvri(tno,'nspect',nif,1)
      do i = 1, nif
        ischan(i) = nchan*(i-1) + 1
        nschan(i) = nchan
      enddo
      call uvputvri(tno,'ischan',ischan,nif)
      call uvputvri(tno,'nschan',nschan,nif)

      if (systok) call uvputvrr(tno,'systemp',systemp,1)
      if (jok)   call uvputvrr(tno,'jyperk',jyperk,1)
c
c  Velocity information.
c
      call uvputvrr(tno,'vsource',0.0,1)
      i = velsys - 256
      if (i.ge.1 .and. i.le.3) call uvputvra(tno,'veltype',veltype(i))
c
c  Telescope and observer.
c
      if (telescop.ne.' ') call uvputvra(tno,'telescop',telescop)
      if (observer.ne.' ') call uvputvra(tno,'observer',observer)
c
c  Initialise common variables.
c
      srcid = -1
      freqid = -1
      config = -1
      Tprev = -1
      inited = .true.

      end

c***********************************************************************

      subroutine TabWrite(tno,sid,fid,confg,time)

      integer tno,sid,fid,confg
      double precision time
c-----------------------------------------------------------------------
c  Write information about the current source and frequency
c  setup and observatory characteristics.
c
c  Input:
c    tno        Handle of the output Miriad uv file.
c    sid        FITS source id number.
c    fid        FITS frequency id number.
c    confg      FITS configuration number.
c  Input/Output:
c    time       Observing time.  Input is the nominal value.  Output is
c               the value corrected for clock differences.
c-----------------------------------------------------------------------
      integer LSRRADIO
      parameter (LSRRADIO=257)
      include 'fits.h'
      integer i,j,k
      logical newsrc,newfreq,newconfg,newlst,newchi,newvel,neweq
      logical neweph
      real chi,chi2,dT
      double precision lst,vel,az,el
      double precision sfreq0(MAXIF),sdf0(MAXIF),rfreq0(MAXIF)

c     Externals.
      integer binsrchi
      double precision eqeq
c-----------------------------------------------------------------------
      if (.not.inited) call TabInit(tno)

      newsrc = srcid.ne.sid
      if (newsrc) then
        srcid = sid
        srcidx = binsrchi(srcid,srcids,nsrc)
        if (srcidx.eq.0) srcidx = 1
        srcidx = sindx(srcidx)
        call uvputvri(tno,'fgsrcid',srcid,1)
      endif

      newfreq = freqid.ne.fid
      if (newfreq) then
        freqid = fid
        freqidx = binsrchi(freqid,freqids,nfreq)
        if (freqidx.eq.0) freqidx = 1
        freqidx = findx(freqidx)
        call uvputvri(tno,'fgfreqid',freqid,1)
      endif

      newconfg = config.ne.confg
      config = confg
      if (config.gt.nconfig) config = 1
c
c  Correct the time
c
      if (config.le.nconfig) time = time + timeoff(config)

      dT = time - tprev
      neweph = newsrc .or. newconfg .or. abs(dT).gt.1d0/86400d0
      if (neweph) tprev = time
c
c  Write out the antenna table and array latitude/longitude, evector,
c  mount.
c
      if (newconfg) then
        if (config.le.nconfig) then
          call uvputvri(tno,'fgarray',config,1)
          call uvputvri(tno,'nants',nants(config),1)
          call uvputvrd(tno,'antpos',antpos(1,config),3*nants(config))
        endif
        if (llok) then
          call uvputvrd(tno,'latitud',lat(config),1)
          call uvputvrd(tno,'longitu',long(config),1)
        endif
        if (emok) then
          call uvputvrr(tno,'evector',evec,1)
          call uvputvri(tno,'mount',mount(config),1)
          if (mount(config).eq.EQUATOR) call uvputvrr(tno,'chi',evec,1)
        endif
      endif
c
c  Write out source information.
c
      if (newsrc) then
        if (.not.velcomp)
     *    call uvputvrr(tno,'veldop',real(veldop(srcidx)),1)
        call uvputvrd(tno,'obsra',raapp(srcidx),1)
        call uvputvrd(tno,'obsdec',decapp(srcidx),1)
        call uvputvrd(tno,'ra',raepo(srcidx),1)
        call uvputvrd(tno,'dec',decepo(srcidx),1)
        if (mosaic) then
          call uvputvrr(tno,'dra',real(dra(srcidx)),1)
          call uvputvrr(tno,'ddec',real(ddec(srcidx)),1)
        endif
        call uvputvrr(tno,'epoch',real(epoch(srcidx)),1)
        call uvputvra(tno,'source',source(srcidx))
      endif
c
c  Write out source/frequency information.
c
      if (newsrc .or. newfreq) then
        i = (srcidx-1)*nif + 1
        j = (freqidx-1)*nif + 1
        do k = 1, nif
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
        dnu = sdf0(1)*1d9
      endif
c
c  Recompute the equation of the equinox every day.
c
      neweq = neweph
      if (neweq) eq = eqeq(time)
c
c  Compute and save the local sideral time. Recompute the LST every sec.
c
      newlst = neweph .and. llok
      if (newlst) then
        call jullst(time,long(config),lst)
        lst = lst + eq
        call uvputvrd(tno,'lst',lst,1)
      endif
c
c  Compute and save the parallactic angle.  Recompute whenever LST
c  changes.
c
      newchi = neweph .and. llok .and. emok .and.
     *        (mount(config).eq.ALTAZ .or. mount(config).eq.NASMYTH)
      if (newchi) then
        call parang(raapp(srcidx),decapp(srcidx),lst,lat(config),chi)
        if (mount(config).eq.NASMYTH) then
          call azel(raapp(srcidx),decapp(srcidx),lst,lat(config),
     *                                                         az,el)
          chi2 = -el
          call uvputvrr(tno,'chi2',chi2,1)
        else
          chi2 = 0
        endif
        call uvputvrr(tno,'chi',chi+evec+chi2,1)
      endif
c
c  Compute and save the radial velocity. Compute a new velocity every
c  minute.
c
      newvel = velcomp .and. neweph
      if (newvel) then
        if (.not.llok) then
          call bug('w','Cannot compute velocities ...')
          call bug('f','Observatory location is not known')
        endif
        call VelRad(velsys.eq.LSRRADIO,
     *    time,raapp(srcidx),decapp(srcidx),
     *    epoch(srcidx),raepo(srcidx),decepo(srcidx),lst,
     *    lat(config),vel)
        call uvputvrr(tno,'veldop',real(vel),1)
      endif

      end

c***********************************************************************

      subroutine VelRad(dolsr,time,raapp,decapp,epoch,raepo,decepo,
     *  lst,lat,vel)

      logical dolsr
      double precision time,raapp,decapp,epoch,raepo,decepo
      double precision lst,lat,vel
c-----------------------------------------------------------------------
c  Compute the radial velocity of the observatory, in the direction of
c  a source, with respect to either LSR or the barycentre.
c
c  Input:
c    dolsr      If true, compute LSR velocity. Otherwise barycentric.
c    time       Time of interest (Julian date).
c    raapp,decapp Apparent RA and DEC (radians).
c    raepo,decepo RA and DEC at the standard epoch (radians).
c    epoch      The standard epoch (years).
c    lat        Observatory geodetic latitude (radians).
c    lst        Local sideral time (radians).
c  Output:
c    vel        Radial velocity.
c-----------------------------------------------------------------------
      double precision lmn2000(3),lmnapp(3),ra2000,dec2000
      double precision velsite(3),posearth(3),velearth(3),velsun(3)
      integer i

c     Externals.
      double precision Epo2jul
c-----------------------------------------------------------------------
c  Compute barycentric velocity.
c
      call sph2lmn(raapp,decapp,lmnapp)
      call vsite(lat,lst,velsite)
      call vearth(time,posearth,velearth)
      vel = 0
      do i = 1, 3
        vel = vel - (velsite(i) + velearth(i))*lmnapp(i)
      enddo
c
c  To compute LSR velocity, we need the source position in J2000
c  coordinates.  Precess, if necessary, to the J2000 frame.  Vsun
c  returns the Sun's velocity in the J2000 frame.  Add this contribution
c  to the velocity we already have.
c
      if (dolsr) then
        if (abs(epoch-2000).gt.0.001) then
          call precess(Epo2jul(epoch,' '),raepo,decepo,
     *               Epo2jul(2000d0,'J'),ra2000,dec2000)
          call sph2lmn(ra2000,dec2000,lmn2000)
        else
          call sph2lmn(raepo,decepo,lmn2000)
        endif
        call vsun(velsun)
        do i = 1, 3
          vel = vel + lmn2000(i)*velsun(i)
        enddo
      endif

      end

c***********************************************************************

      subroutine Extract(nfreq,npol,pol,visibs,corr,flags,zerowt)

      integer nfreq,npol,pol
      real visibs(3*nfreq*npol)
      complex corr(nfreq)
      logical flags(nfreq),zerowt
c-----------------------------------------------------------------------
c  Extract the input visibilities into an output buffer.  Only one
c  polarisation is extracted at a time.
c
c  Input:
c    nfreq      Number of frequency channels in the input.
c    npol       Number of polarisations in the input.
c    pol        Polarisation number to extract.
c    visibs     The input data.
c  Input/Output:
c    zerowt     True if a zero weight was found somewhere.
c  Output:
c    corr       The correlation data of the desired polarisation.
c    flags      Logical array, giving true if the correlation is good.
c-----------------------------------------------------------------------
      integer i,j
c-----------------------------------------------------------------------
      j = 3*(pol-1) + 1
      do i = 1, nfreq
        corr(i) = cmplx(visibs(j),visibs(j+1))
        flags(i) = visibs(j+2).gt.0
        zerowt = zerowt .or. visibs(j+2).eq.0
        j = j + 3*npol
      enddo
c
      end
c***********************************************************************
      subroutine uvout(out,version)
c
      character out*(*),version*(*)
c
c  Write out a UV FITS file.
c
c  Inputs:
c    out        Name of the output uv FITS file.
c    version    Version of this program.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      integer uvCrval,uvCdelt,uvCrpix
      integer uvStokes,uvFreq,uvRa,uvDec
      parameter (uvCrval=1,uvCdelt=2,uvCrpix=3)
      parameter (uvStokes=1,uvFreq=2,uvRa=3,uvDec=4)

      integer uvU,uvV,uvW,uvBl,uvT,uvSrc,uvRandom,uvData
      parameter (uvU=1,uvV=2,uvW=3,uvBl=4,uvT=5,uvSrc=6,uvRandom=6)
      parameter (uvData=uvRandom+1)

      integer Polmin,Polmax,maxPol,PolRR,PolXX
      parameter (PolMin=-8,PolMax=4,maxPol=4,PolXX=-5,PolRR=-1)

      complex Data(maxchan)
      logical Flags(maxchan)
      real OutData(uvRandom+1+3*maxchan)

      integer i,i0
      integer tIn,tScr,tOut,vSrc
      integer nread,nvis,nVisRef,length,velref,nchan
      ptrdiff offset
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

      integer MAXSRC
      parameter (MAXSRC=2048)
      double precision ras(MAXSRC),decs(MAXSRC)
      double precision aras(MAXSRC),adecs(MAXSRC)
      character sources(MAXSRC)*16

      integer MAXPARMS
      parameter (MAXPARMS=6)
      integer nparms
      character parms(MAXPARMS)*8

c     Externals.
      character itoaf*8
      logical uvdatopn
      logical hdprsnt
      data parms/'UU      ','VV      ','WW      ',
     *           'BASELINE','DATE    ','SOURCE  '/
      data vtype/'FELO-LSR','FELO-HEL','FELO-OBS',
     *           'VELO-LSR','VELO-HEL','VELO-OBS'/
c-----------------------------------------------------------------------
c  Initialise the array to count the sorts of polarisations that we
c  have.
c
      do i = PolMin, PolMax
        pols(i) = 0
      enddo
c
c  Open and initialise the input file. Also get the scratch file.
c
      if (.not.uvdatopn(tin)) call bug('f','Error opening input file')
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

      call uvdatrd(preamble,data,flags,maxchan,nread)
      if (nread.eq.0) call bug('f','No data to write out!')
      call SrcUpd(tIn,vSrc,iSrc,ras,decs,aras,adecs,sources,
     *                                                nSrc,MAXSRC)
      call uvrdvrd(tIn,'pntra',pntra,ras(1))
      call uvrdvrd(tIn,'pntdec',pntdec,decs(1))
      nchan = nread

      nvis = 0
      length = uvRandom + 1 + 3*nchan
      call scrrecsz(tScr,length)
      offset = 0
c
c  Get things which define the coordinate system.
c
      call uvrdvrr(tIn,'epoch',epoch,1950.0)
      call uvfit2(tIn,'sfreq',nread,df,f0,fepsi)
      if (fepsi.gt.0.1*abs(df)) call bug('w',
     *    'Channel frequencies deviated by > 10% from linearity')
      if (nread.eq.1) call uvfit1(tIn,'bandwidth',nread,df,fepsi)
      f0 = 1e9 * f0
      df = 1e9 * df

      call uvdatgta('ltype',ltype)
      if (ltype.eq.'wide') then
        velref = 0
        restfreq = 0
      else
        call uvfit1(tIn,'restfreq',nread,restfreq,repsi)
        if (repsi.gt.0.001*restfreq) call bug('w',
     *    'Rest frequencies varied between channels by > 0.1%')
        restfreq = 1e9 * restfreq
        if (restfreq.gt.0) then
          call uvrdvra(tIn,'veltype',veltype,'VELO-LSR')
          call uvfit2(tIn,'velocity',nread,dv,v0,vepsi)
          v0 = 1000d0 * v0
          if (vepsi.gt.0.1*abs(dv)) call bug('w',
     *      'Channel velocities deviate by > 10% from linearity')
          velref = 0
          do i = 1, 6
            if (veltype.eq.vtype(i)) velref = i
          enddo
          if (velref.gt.3) velref = velref - 3 + 256
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
      if (mod(nants,3).ne.0)
     *  call bug('f','Antpos variable looks bad')
      nants = nants/3
      if (nants.gt.MAXANT) nants = 0
      if (nants.gt.0) then
        call uvgetvrd(tIn,'antpos',xyz,3*nants)
        call getarr(tIn,mount,lat,long,height)
      endif
c
c  Read the data. Check that we are dealing with a single pointing.
c  If the polarisation code is OK, do some conversions, and write to
c  a scratch file.
c
      badpol = 0
      do while (nread.eq.nchan)
        call uvdatgti('pol',P)
        if (P.ge.PolMin .and. P.le.PolMax .and. P.ne.0) then
          nvis = nvis + 1
          call uvdatgtr('variance',wt)
          if (wt.eq.0) then
            call uvrdvrr(tIn,'inttime',wt,1.0)
          else
            wt = 1/wt
          endif
c
c  Convert baseline number to normal AIPS form.  Note the different
c  conventions
c    Miriad: bl = 256*ant1 + ant2, where the baseline is ant2 - ant1
c      AIPS: bl = 256*ant1 + ant2                        ant1 - ant2!
c
c  In both cases ant1 is normally less than ant2.
c
          call Basant(preamble(5),ant1,ant2)
          pols(P) = pols(P) + 1

          OutData(uvU+1) = -1e-9 * preamble(1)
          OutData(uvV+1) = -1e-9 * preamble(2)
          OutData(uvW+1) = -1e-9 * preamble(3)
          OutData(uvT+1) = preamble(4) - T0
          call fantbas(ant1,ant2,1,OutData(uvBl+1))
          OutData(uvSrc+1) = iSrc
          OutData(1) = P
          i0 = uvData
          do i = 1, nchan
            OutData(i0+1) = real(Data(i))
            OutData(i0+2) = -aimag(Data(i))
            if (flags(i)) then
              OutData(i0+3) = wt
            else
              OutData(i0+3) = -wt
            endif
            i0 = i0 + 3
          enddo
          call scrwrite(tScr,OutData,offset,1)
          offset = offset + 1
        else
          badpol = badpol + 1
        endif
        call uvdatrd(preamble,data,flags,maxchan,nread)
        call SrcUpd(tIn,vSrc,iSrc,ras,decs,aras,adecs,sources,
     *                                                nSrc,MAXSRC)
      enddo
c
c  Summarise what we read.
c
      if (badpol.gt.0) call bug('w',
     *  'Visibilities with bad pol codes: '//itoaf(badpol))
      if (nread.gt.0) call bug('f','Bad number of channels')
      if (nvis.le.0)  call bug('f','No visibilities found')
c
c  Determine the polarisations that we are going to output.
c
      call PolCheck(pols,PolMin,PolMax,npol,Pol0,PolInc)
      if (npol.gt.maxPol)
     *  call bug('f','Too many polarisations for me to handle')
      if (pol0.le.PolXX) then
        polty = 'XY'
      else if (pol0.le.PolRR) then
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
      Coord(uvCrpix,uvStokes) = 1d0
      Coord(uvCrval,uvFreq) = f0
      Coord(uvCdelt,uvFreq) = df
      Coord(uvCrpix,uvFreq) = 1d0
      Coord(uvCrval,uvRa)  = ras(1)*DR2D
      Coord(uvCdelt,uvRa)  = 1d0
      Coord(uvCrpix,uvRa)  = 1d0
      Coord(uvCrval,uvDec) = decs(1)*DR2D
      Coord(uvCdelt,uvDec) = 1d0
      Coord(uvCrpix,uvDec) = 1d0
c
c  Open the FITS file and write out some info. NOTE that a bug in AIPS
c  FITLD requires that the OBJECT keyword be written out as early
c  as possible.
c
      call fuvopen(tOut,out,'new',nVisRef,npol,nchan)
      call fuvSetT0(tOut,T0)
      nparms = 5
      if (nSrc.gt.1) nparms = 6
      call fuvSetPa(tOut,nparms,parms)
      call fuvWrhd(tOut,Coord)
      if (sources(1).ne.' ') call fitwrhda(tOut,'OBJECT',sources(1))
      call fitwrhdd(tOut,'OBSRA', pntra*DR2D)
      call fitwrhdd(tOut,'OBSDEC',pntdec*DR2D)
      call fitwrhdr(tOut,'EPOCH',epoch)
c
c  Spectral line and velocity information.
c
      if (restfreq.gt.0) then
        call fitwrhdd(tOut,'RESTFREQ',restfreq)
        if (velref.gt.0) call fitwrhdi(tOut,'VELREF',velref)
        call fitwrhdr(tOut,'ALTRPIX',1.0)
        call fitwrhdr(tOut,'ALTRVAL',real(v0))
      endif
c
c  Other miscellaneous information.
c
      if (telescop.ne.' ') then
        call fitwrhda(tOut,'TELESCOP',telescop)
        call fitwrhda(tOut,'INSTRUME',telescop)
      endif
      if (observer.ne.' ') call fitwrhda(tOut,'OBSERVER',observer)
      string = 'Miriad '//version
      call fitwrhda(tOut,'ORIGIN',string)
c
c  Copy the history.
c
      if (hdprsnt(tIn,'history ')) then
        call copyHist(tIn,tOut,version)
      endif
c
c  We now have all the data we want in a scratch file. Copy this
c  data to the output FITS file.
c
      call uvoutWr(tScr,tOut,nvis,nVisRef,npol,nchan,Pol0,PolInc,
     *  nSrc.gt.1)
c
c  Write the source file.
c
      if (nSrc.gt.1) call SrcWrite(tOut,nSrc,sources,
     *  ras,decs,aras,adecs,epoch,restfreq,df,v0)
c
c  Write the antenna file.
c
      if (nants.gt.0) then
        call output('Writing FITS antenna table')
        call AntWrite(tOut,t0,f0,telescop,polty,mount,
     *                xyz,nants,lat,long,height)
      else
        call bug('w','Insufficent information for antenna table')
      endif
c
c  Everything is done. Close up shop.
c
      call uvdatcls
      call scrclose(tScr)
      call fuvclose(tOut)

      end

c***********************************************************************

      subroutine SrcUpd(tIn,vSrc,iSrc,ras,decs,aras,adecs,
     *                                        sources,nSrc,MAXSRC)

      integer tIn,vSrc,iSrc,nSrc,MAXSRC
      double precision ras(MAXSRC),decs(MAXSRC)
      double precision aras(MAXSRC),adecs(MAXSRC)
      character sources(MAXSRC)*(*)
c-----------------------------------------------------------------------
      character source*32
      integer i
      double precision ra,dec,dra,ddec

c     Externals.
      logical uvVarUpd
c-----------------------------------------------------------------------
c  Change in source. Get the source name, ra and dec.
c
      if (uvVarUpd(vSrc) .or. nSrc.eq.0) then
        call uvrdvra(tIn,'source',source,' ')
        if (len(source).gt.len(sources(1)))
     *    source(len(sources(1))+1:) =  ' '
        call uvrdvrd(tIn,'ra',ra,0d0)
        call uvrdvrd(tIn,'dec',dec,0d0)
        call uvrdvrd(tIn,'dra',dra,0d0)
        call uvrdvrd(tIn,'ddec',ddec,0d0)
        ra = ra + dra/cos(dec)
        dec = dec + ddec
        iSrc = 0
        i = 1
        do while (i.le.nSrc .and. iSrc.eq.0)
          if (ras(i).eq.ra .and. decs(i).eq.dec .and.
     *      sources(i).eq.source) iSrc = i
          i = i + 1
        enddo

        if (iSrc.eq.0) then
          nSrc = nSrc + 1
          iSrc= nSrc
          if (nSrc.gt.MAXSRC) call bug('f','Source table overflow')
          sources(iSrc) = source
          ras(iSrc) = ra
          decs(iSrc) = dec
          call uvrdvrd(tIn,'obsra',aras(iSrc),ras(iSrc))
          call uvrdvrd(tIn,'obsdec',adecs(iSrc),decs(iSrc))
        endif
      endif

      end

c***********************************************************************

      subroutine getarr(tIn,mount,lat,long,height)

      integer tIn,mount
      double precision lat,long,height
c-----------------------------------------------------------------------
      character telescop*32,type*1
      double precision dval
      integer n
      logical updated,ok
c-----------------------------------------------------------------------
c  Determine the telescope.
c
      call uvrdvra(tin,'telescop',telescop,' ')
c
c  Determine the mount.
c
      call uvprobvr(tIn,'mount',type,n,updated)
      ok = n.eq.1
      if (ok) then
        call uvrdvri(tIn,'mount',mount,0)
      else if (telescop.ne.' ') then
        call obspar(telescop,'mount',dval,ok)
        if (ok) mount = nint(dval)
      endif
      if (.not.ok) then
        call bug('w',
     *    'Antenna mount could not be determined -- assuming alt/az')
        mount = 0
      endif
c
c  Determine the latitude.
c
      call uvprobvr(tIn,'latitud',type,n,updated)
      ok = n.eq.1
      if (ok) then
        call uvrdvrd(tIn,'latitud',lat,0d0)
      else if (telescop.ne.' ') then
        call obspar(telescop,'latitude',lat,ok)
      endif
      if (.not.ok) then
        lat = 0d0
        call bug('w','Telescope latitude could not be determined')
      endif
c
c  Determine the longitude.
c
      call uvprobvr(tIn,'longitu',type,n,updated)
      ok = n.eq.1
      if (ok) then
        call uvrdvrd(tIn,'longitu',long,0d0)
      else if (telescop.ne.' ') then
        call obspar(telescop,'longitude',long,ok)
      endif
      if (.not.ok) then
        call bug('w','Telescope longitude could not be determined')
        long = 0d0
      endif
c
c  Determine the height.
c
      call uvprobvr(tIn,'height',type,n,updated)
      ok = n.eq.1
      if (ok) then
        call uvrdvrd(tIn,'height',height,0d0)
      else if (telescop.ne.' ') then
        call obspar(telescop,'height',height,ok)
      endif
      if (.not.ok) height = 0d0

      end

c***********************************************************************

      subroutine SrcWrite(tOut,nSrc,sources,ras,decs,aras,adecs,epoch,
     *  restfreq,df,v0)

      integer tOut,nSrc
      double precision ras(nSrc),decs(nSrc),aras(nSrc),adecs(nSrc)
      double precision restfreq,df,v0
      character sources(nSrc)*(*)
      real epoch
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer i
c-----------------------------------------------------------------------
      call ftabdini(tOut,'AIPS SU')
      call ftabdef(tOut,'ID. NO.',  'I',' ',      nSrc,1)
      call ftabdef(tOut,'SOURCE',   'A',' ',      nSrc,
     *                                        len(sources(1)))
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

      call fitwrhdi(tOut,'NO_IF',1)
      call fitwrhda(tOut,'VELTYP','OBS')
      call fitwrhda(tOut,'VELDEF','RADIO')

      do i = 1, nSrc
        call ftabputi(tOut,'ID. NO.',  i,i)
        call ftabputa(tOut,'SOURCE',   i,sources(i))
        call ftabputi(tOut,'QUAL',     i,0)
        call ftabputa(tOut,'CALCODE',  i,'    ')
        call ftabputr(tOut,'IFLUX',    i,0.0)
        call ftabputr(tOut,'QFLUX',    i,0.0)
        call ftabputr(tOut,'UFLUX',    i,0.0)
        call ftabputr(tOut,'VFLUX',    i,0.0)
        call ftabputd(tOut,'FREQOFF',  i,0d0)
        call ftabputd(tOut,'BANDWIDTH',i,df)
        call ftabputd(tOut,'RAEPO',    i,ras(i)*DR2D)
        call ftabputd(tOut,'DECEPO',   i,decs(i)*DR2D)
        call ftabputd(tOut,'EPOCH',    i,dble(epoch))
        call ftabputd(tOut,'RAAPP',    i,aras(i)*DR2D)
        call ftabputd(tOut,'DECAPP',   i,adecs(i)*DR2D)
        call ftabputd(tOut,'LSRVEL',   i,v0)
        call ftabputd(tOut,'RESTFREQ', i,restfreq)
        call ftabputd(tOut,'PMRA',     i,0d0)
        call ftabputd(tOut,'PMDEC',    i,0d0)
      enddo

      end

c***********************************************************************

      subroutine AntWrite(tOut,rtime,rfreq,telescop,polty,mount,
     *                xyz,nants,lat,long,height)

      integer tOut,nants,mount
      double precision rtime,rfreq
      double precision xyz(nants,3),lat,long,height
      character telescop*(*),polty*(*)
c-----------------------------------------------------------------------
c  Write an antenna table into the output.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      real zero(3)
      character anname*8,rdate*32
      double precision iatutc,gstia0,gstia1,degpdy,xyzd(3)
      integer i

c     Externals.
      character itoaf*4
      double precision deltime,eqeq
c-----------------------------------------------------------------------
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
      call jullst(rtime-iatutc,0d0,gstia0)
      gstia0 = (gstia0 + eqeq(rtime-iatutc))*DR2D
      if (gstia0.lt.0d0)   gstia0 = gstia0 + 360d0
      if (gstia0.ge.360d0) gstia0 = gstia0 - 360d0
      call jullst(rtime-iatutc+1d0,0d0,gstia1)
      gstia1 = (gstia1 + eqeq(rtime-iatutc+1d0)) * DR2D
      if (gstia1.lt.0d0)   gstia1 = gstia1 + 360d0
      if (gstia1.ge.360d0) gstia1 = gstia1 - 360d0
      degpdy = gstia1 - gstia0 + 360d0
      if (degpdy.lt.360d0) degpdy = degpdy + 360d0
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
      call fitwrhdd(tOut,'POLARX',0d0)
      call fitwrhdd(tOut,'POLARY',0d0)
      call fitwrhdd(tOut,'UT1UTC',0d0)
      call fitwrhdd(tOut,'DATUTC',0d0)
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
      do i = 1, nants
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
        endif
        call ftabputr(tOut,'POLCALA',i,zero)
        call ftabputa(tOut,'POLTYB', i,polty(2:2))

        call ftabputr(tOut,'POLCALB',i,zero)
      enddo

      end

c***********************************************************************

      subroutine uvoutWr(tScr,tOut,nvis,nVisRef,npol,nchan,Pol0,
     *                                                PolInc,doSrc)

      integer tScr,tOut,nvis,nVisRef,npol,nchan,Pol0,PolInc
      logical doSrc
c-----------------------------------------------------------------------
c  This reads visibilities back from the scratch file, and forms a
c  visibility record the way FITS likes it. That is the visibility
c  consists of 3*npol*nfreq bits of data. This is unlike Miriad in that
c  the polarisation axis is in with the frequency axis.
c
c  Inputs:
c    tScr       Handle of the scratch file.
c    tOut       Handle of the output FITS file.
c    nvis       Number of visibilities in the scratch file.
c    nVisRef    Number of visibilities that will be written to the
c               output FITS file.
c    npol       The dimension of the Stokes axis in the output file.
c    nchan      The number of frequency channels in the output file.
c    Pol0       The code of the first polarisation.
c    PolInc     The increment between polarisations.
c    doSrc      True if we should write the source number.
c-----------------------------------------------------------------------
c  Records are copied or discarded according to whether the reference
c  polarisation is present or not.  If not, the record is discarded.
c  Currently the reference polarisation is always the first
c  polarisation, but this may change in the future.
c
      include 'maxdim.h'
      integer maxPol,RefPol,PolMin,PolMax
      parameter (RefPol=1,PolMin=-8,PolMax=4,maxPol=4)
      integer uvU,uvV,uvW,uvBl,uvT,uvSrc,uvRandom,uvData
      parameter (uvU=1,uvV=2,uvW=3,uvBl=4,uvT=5,uvSrc=6,uvRandom=6)
      parameter (uvData=uvRandom+1)

      integer pols(PolMin:PolMax),discard(PolMin:PolMax),pnt(maxPol)
      integer ncopy,totcopy,l,l2,InPnt,OutPnt,Bl,P,iP,i,j,jd,k,length
      integer iSrc
      logical copied(maxPol)
      character num*8,num2*8
      real In(uvRandom+1+3*maxchan),Out(uvRandom+3*maxPol*maxchan)
      real Time,wt
      ptrdiff offset

c     Externals.
      character itoaf*8,PolsC2P*2
      integer len1
c-----------------------------------------------------------------------
c  Check! These should have been done before, somewhere or other.
c
      if (nchan.gt.maxchan .or. npol.gt.maxpol)
     *  call bug('f','Too many channels, or too many polarisations')
c
c  Form a table to help sort out which polarisations to keep, and where
c  to put them in the output record. Also initialise an array of
c  counters to determine the number of visibilities that are being
c  discarded.
c
      do i = PolMin, PolMax
        pols(i) = 0
        discard(i) = 0
      enddo
c
c  Initialise some tables to help me keep track of things. They allow
c  determination of the polarisation code, from the number 1..nPol, and
c  visa versa.
c
      i = Pol0
      do j = 1, npol
        copied(j) = .false.
        pnt(j) = i
        pols(i) = j
        i = i + PolInc
      enddo

      length = uvRandom + 1 + 3*nchan
      ncopy = 0
      totcopy = 0
      Time = 0
      Bl = 0
      iSrc = 0
      jd = 0
      wt = 0

      do j = 1, nvis
        offset = j-1
        call scrread(tScr,In, offset,1)
        P = nint(In(1))
        iP = pols(P)
c
c  Handle the case of a polarisation that we are not handling, or the
c  case where we have only one polarisation to handle.
c
        if (iP.eq.0) then
          discard(P) = discard(P) + 1
        else if (npol.eq.1) then
          jd = jd + 1
          totcopy = totcopy + 1
          if (doSrc) then
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
c  We have a good polarisation, and we are handling more than one
c  polarisation.  If it does not match with the pervious record, then
c  we have to rid ourselves of the previous record.
c
c  If the "reference" polarisation is not present, discard the previous
c  record.  If the "reference" polarisation is present, blank out any
c  channels which are missing data, and finally write out the previous
c  good record.
c
          if (nint(In(uvBl+1)).ne.Bl .or. In(uvT+1).ne.Time .or.
     *       nint(In(uvSrc+1)).ne.iSrc .or.
     *       (copied(RefPol) .and. copied(iP))) then
            if (.not.copied(RefPol)) then
              do i = 1, npol
                if (copied(i)) discard(pnt(i)) = discard(pnt(i)) + 1
              enddo
            else
              if (ncopy.lt.npol) call ZeroOut(copied,npol,nchan,out,wt)
              jd = jd + 1
              totcopy = totcopy + ncopy
              call fuvwrite(tOut,Out,jd,1)
            endif
c
c  We have a new record. Ready ourselves for it.
c
            do i = 1, npol
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
          if (doSrc) OutPnt = OutPnt + 1
          do k = 1, nchan
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
        if (.not.copied(RefPol)) then
          do i = 1, npol
            if (copied(i)) discard(pnt(i)) = discard(pnt(i)) + 1
          enddo
        else
          if (ncopy.lt.npol) call ZeroOut(copied,npol,nchan,out,wt)
          jd = jd + 1
          totcopy = totcopy + ncopy
          call fuvwrite(tOut,Out,jd,1)
        endif
c
c  Generate messages about the number of visibilities copied and not
c  copied.
c
      if (jd.ne.nVisRef)
     *  call bug('f','Internal inconsistency, in uvout(write)')
      num = itoaf(totcopy)
      l = len1(num)
      num2 = itoaf(nVisRef)
      l2 = len1(num2)
      call output(num(1:l)//' visibilities copied to '//num2(1:l2)//
     *        ' output records.')

      do i = PolMin, PolMax
        if (discard(i).ne.0) then
          num = itoaf(discard(i))
          l = len1(num)
          call bug('w','Discarded '//num(1:l)//
     *        ' visibilities of type '//PolsC2P(i))
        endif
      enddo

      end

c***********************************************************************

      subroutine ZeroOut(copied,npol,nchan,out,wt)

      integer npol,nchan
      logical copied(npol)
      real out(5+3*npol*nchan),wt
c-----------------------------------------------------------------------
c  This blanks out any polarisations that have not truely been copied.
c  It does this by setting the correlation value to zero, and the weight
c  to indicate bad data.
c
c  Inputs:
c    copied     Logical array indicating if a particular polarisation
c               has been copied.
c    wt         The weight to associate with the blanked out data.
c    npol       Number of polarisations.
c    nchan      Number of channels.
c  Input/Output:
c    out        The visibility record (FITS style).  Missing
c               polarisations are blanked out.
c-----------------------------------------------------------------------
      integer i,OutPnt,k
c-----------------------------------------------------------------------
      do i = 1, npol
        if (.not.copied(i)) then
          OutPnt = 5 + 3*(i-1)
          do k = 1, nchan
            out(OutPnt+1) = 0
            out(OutPnt+2) = 0
            out(OutPnt+3) = -wt
            OutPnt = OutPnt + 3*npol
          enddo
        endif
      enddo

      end

c***********************************************************************

      subroutine PolCheck(pols,PolMin,PolMax,npol,Pol0,PolInc)

      integer PolMin,PolMax,pols(PolMin:PolMax),npol,Pol0,PolInc
c-----------------------------------------------------------------------
c  The "pols" array counts the polarisations that were found in the
c  input data.  Because FITS can only handle a regular Stokes axis, and
c  because Miriad allows an arbitrary Stokes "dimension", we have to
c  form a regular axis.  The rule is to find the commonest polarisation,
c  and then to copy "adjacent" polarisations that are at least 70% as
c  common.  When a required polarisation is missing, we give a dummy
c  (but flagged) value.
c
c  Input:
c    pols       The counts of the polarisations encountered in the
c               input file.
c    PolMin
c    PolMax
c  Output:
c    npol       Number of polarisations to form in the output file.
c    Pol0       The code for the first polarisation.
c    PolInc     The code increment between different polarisations. This
c               can be either +1 or -1, having the same sign as Pol0.
c-----------------------------------------------------------------------
      integer imax,i,PolMn,PolMx,thresh,p,length
      logical more
      character line*32

c     Externals.
      character PolsC2P*2
      integer len1
c-----------------------------------------------------------------------
c  Determine which polarisation type is the most common.
c
      imax = PolMin
      do i = PolMin, PolMax
        if (pols(i).gt.pols(imax) .or.
     *    (pols(i).eq.pols(imax) .and. abs(i).lt.abs(imax))) imax = i
      enddo
c
c  Around the commonest type, find those types that are at least 70% as
c  common. This spans the polarisations that we will choose.
c
      thresh = nint(0.7*pols(imax))
      PolMn = imax
      more = .true.
      do while (PolMn-1.ge.PolMin .and. more)
        more = pols(PolMn-1).ge.thresh .and. PolMn-1.ne.0
        if (more) PolMn = PolMn - 1
      enddo

      PolMx = imax
      more = .true.
      do while (PolMx+1.le.PolMax .and. more)
        more = pols(PolMx+1).ge.thresh .and. PolMx+1.ne.0
        if (more) PolMx = PolMx + 1
      enddo
c
c  Fill in the parameters to describe the choosen polarisation.
c
      npol = PolMx - PolMn + 1
      if (PolMx.gt.0) then
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
      do i = 1, npol
        line(length+1:length+2) = PolsC2P(p)
        length = len1(line(1:length+2)) + 1
        line(length:length) = ','
        p = p + PolInc
      enddo
      line(length:length) = '.'
      call output('Polarisations copied: '//line(1:length))

      end

c***********************************************************************

      subroutine xyin(in,out,version,dss,nod2)

      character in*(*),out*(*),version*(*)
      logical dss,nod2
c-----------------------------------------------------------------------
c  Read in an image FITS file.
c
c  Inputs:
c    in         Name of the input image FITS file.
c    out        Name of the output Miriad file.
c    dss        Expect a DSS image.  Handle header accordingly!
c    nod2       Expect a NOD2 image.
c
c  Internal Variables:
c    lIn        Handle of the input  FITS file.
c    lOut       Handle of the output Miriad file.
c    array      Array to use as buffer.
c    maxdim     Size of array.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'

      real array(MAXDIM)
      logical allgood,doflag,flags(MAXDIM)
      integer nsize(MAXNAX),axes(MAXNAX),naxis
      integer lIn,lOut,i,j,iostat

      external fitBlank, inc3more
      logical  fitBlank, inc3more
c-----------------------------------------------------------------------
c     Open the input FITS and output Miriad files.
      call fxyopen(lIn,in,'old',MAXNAX,nsize)
      doflag = fitBlank(lIn,.false.)
      if (nsize(1).gt.maxdim)
     *  call bug('f','Image too big to handle')
      call fitrdhdi(lIn,'NAXIS',naxis,0)
      if (naxis.le.0) call bug('f','Weird bug')
      naxis = min(naxis,MAXNAX)
      call xyopen(lOut,out,'new',naxis,nsize)
      call hisOpen(lOut,'append')
      call histIn(lIn,lOut,version)

c     Copy the image data itself.
      allgood = .true.
      call IncIni(naxis,nsize,axes)
      do while (Inc3More(naxis,nsize,axes))
        if (naxis.gt.2) then
          call fxysetpl(lIn,naxis-2,axes(3))
          call xysetpl(lOut,naxis-2,axes(3))
        endif
        do j = 1, nsize(2)
          call fxyread(lIn,j,array)
          call xywrite(lOut,j,array)

c         Handle the flags.
          if (doflag) then
            call fxyflgrd(lIn,j,flags)
            call xyflgwr(lOut,j,flags)
            if (allgood) then
              do i = 1, nsize(1)
                allgood = allgood .and. flags(i)
              enddo
            endif
          endif

        enddo
      enddo

c     Handle the header.
      call kwdIn(lIn,lOut,naxis)
      if (dss)  call dssFudge(lIn,lOut)
      if (nod2) call nodFudge(lIn,lOut)

c     Close up shop.
      call fxyclose(lIn)
      call hisclose(lOut)
      call xyclose(lOut)

c     FUDGE!!! Delete the flagging mask if all the flags were good.
      if (doflag) then
        if (allgood) then
          call hopen(lOut,out,'old',iostat)
          if (iostat.ne.0) call bugno('f',iostat)
          call hdelete(lOut,'mask',iostat)
          if (iostat.ne.0) call bugno('f',iostat)
          call hclose(lOut)
          call output('There were no blanked pixels in the input')
        else
          call output(
     *     'Some pixels were blanked ... A blanking mask was created')
        endif
      else
        call output(
     *     'There was no blanking information in the input')
      endif

      end

c***********************************************************************

      subroutine kwdIn(lIn, lOut, naxis)

      integer   lIn, lOut, naxis
c-----------------------------------------------------------------------
c  Copy (with any necessary scaling) FITS keywords to the Miriad image
c  file.
c
c  Inputs:
c    lIn        Handle of the input FITS image.
c    lOut       Handle of the output Miriad image.
c    naxis      Number of dimensions of the image.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'maxnax.h'

      logical   found
      integer   iax, ilat, ilng, k, m, nx, ny, polcode
      real      bmaj, bmin, bpa, epoch, equinox, pbfwhm, rms, vobs
      double precision cdelt(MAXNAX), crpix(MAXNAX), crval(MAXNAX),
     *          dval, llrot, obsdec, obsra, restfreq
      character atemp*16, btype*24, bunit*32, cax*2, cellscal*16,
     *          ctype(MAXNAX)*8, object*32, observer*16, pbtype*16,
     *          pv*8, specsys*16, telescop*16, btypes(5)*24

      external  epo2jul, itoaf, len1
      integer   len1
      double precision epo2jul
      character itoaf*2

      data btypes/'polarized_intensity     ',
     *            'fractional_polarization ',
     *            'position_angle          ',
     *            'spectral_index          ',
     *            'optical_depth           '/
c-----------------------------------------------------------------------
c     Attempt to determine observer and telescope type.
      call fitrdhda(lIn,'USER',atemp,' ')
      call fitrdhda(lIn,'OBSERVER',observer,atemp)
      if (observer.ne.' ') call wrhda(lOut,'observer',observer)

      call fitrdhda(lIn,'INSTRUME',atemp,' ')
      call fitrdhda(lIn,'TELESCOP',telescop,atemp)
      if (telescop.ne.' ') call wrhda(lOut,'telescop',telescop)

      call fitrdhda(lIn,'OBJECT',object,' ')
      if (object.ne.' ') call wrhda(lOut,'object',object)

      call fitrdhda(lIn,'BUNIT',bunit,' ')
      call fitrdhda(lIn,'BTYPE',btype,' ')

      call fitrdhdr(lIn,'rms',rms,0.0)
      if (rms.gt.0.0) call wrhdr(lOut,'rms',rms)

      call fitrdhdr(lIn,'pbfwhm',pbfwhm,0.0)
      atemp = telescop
      if (pbfwhm.ne.0.0) call pbEncode(atemp,'gaus',pbfwhm*AS2R)
      call fitrdhda(lIn,'pbtype',pbtype,atemp)

c     Get world coordinate information, convert units, and write out.
      call crdLd(lIn,naxis,crpix,cdelt,crval,ctype,llrot,ilng,ilat)

      do iax = 1, naxis
        if (ctype(iax).eq.'STOKES') then
c         Translate the Stokes code.
          polcode = nint(crval(iax)+(1-crpix(iax))*cdelt(iax))
          if (polcode.lt.-8 .or. polcode.eq.0 .or. 4.lt.polcode) then
            ctype(iax) = 'ASTOKES'
            if (polcode.gt.4 .and. polcode.le.9) btype=btypes(polcode-4)
          endif
        endif

        cax = itoaf(iax)
        call wrhdd(lOut, 'crpix'//cax, crpix(iax))
        call wrhdd(lOut, 'cdelt'//cax, cdelt(iax))
        call wrhdd(lOut, 'crval'//cax, crval(iax))
        if (ctype(iax).ne.' ') call wrhda(lOut,'ctype'//cax,ctype(iax))
      enddo

      if (llrot.ne.0d0) then
        call wrhdd(lOut, 'llrot', llrot)
        call bug('w','This image has a sky rotation')
        call bug('w','Sky rotations are poorly supported by Miriad')
      endif

      if (ilng.ne.0 .and. ilat.ne.0) then
c       Handle LONPOLE and LATPOLE.
        call fitsrch(lIn, 'LONPOLE', found)
        if (found) then
          call fitrdhdd(lIn, 'LONPOLE', dval, 0d0)
          call wrhdd(lOut, 'lonpole', dval)
        endif

        call fitsrch(lIn, 'LATPOLE', found)
        if (found) then
          call fitrdhdd(lIn, 'LATPOLE', dval, 0d0)
          call wrhdd(lOut, 'latpole', dval)
        endif

c       phi0, theta0, and xyzero.
        cax = itoaf(ilng)
        pv = 'PV'//cax
        k = len1(pv) + 1

        pv(k:) = '_0'
        call fitsrch(lIn, pv, found)
        if (found) then
          call fitrdhdd(lIn, pv, dval, 0d0)
          if (dval.eq.0d0) then
            call wrhdi(lOut, 'xyzero', 0)
          else
            call wrhdi(lOut, 'xyzero', 1)
          endif
        endif

        pv(k:) = '_1'
        call fitsrch(lIn, pv, found)
        if (found) then
          call fitrdhdd(lIn, pv, dval, 0d0)
          call wrhdd(lOut, 'phi0', dval)
        endif

        pv(k:) = '_2'
        call fitsrch(lIn, pv, found)
        if (found) then
          call fitrdhdd(lIn, pv, dval, 0d0)
          call wrhdd(lOut, 'theta0', dval)
        endif

c       These override LONPOLE and LATPOLE if present.
        pv(k:) = '_3'
        call fitsrch(lIn, pv, found)
        if (found) then
          call fitrdhdd(lIn, pv, dval, 0d0)
          call wrhdd(lOut, 'lonpole', dval)
        endif

        pv(k:) = '_4'
        call fitsrch(lIn, pv, found)
        if (found) then
          call fitrdhdd(lIn, pv, dval, 0d0)
          call wrhdd(lOut, 'latpole', dval)
        endif

c       Projection parameters.
        cax = itoaf(ilat)
        pv = 'PV'//cax
        k = len1(pv) + 1

        do m = 0, 29
          pv(k:) = '_' // itoaf(m)
          call fitsrch(lIn, pv, found)
          if (found) then
            call fitrdhdd(lIn, pv, dval, 0d0)
            call wrhdd(lOut, 'pv'//itoaf(m), dval)
          endif
        enddo


c       Handle the OBSRA and OBSDEC keywords.
        if (ctype(ilng)(:2).eq.'RA' .and.
     *      ctype(ilat)(:3).eq.'DEC') then
          call fitrdhdd(lIn, 'OBSRA' , obsra,  crval(ilng))
          call fitrdhdd(lIn, 'OBSDEC', obsdec, crval(ilat))
          if (abs(obsra -crval(ilng)).gt.abs(cdelt(ilng)) .or.
     *        abs(obsdec-crval(ilat)).gt.abs(cdelt(ilat))) then
            call fitrdhdi(lIn, 'NAXIS'//itoaf(ilng), nx, 0)
            call fitrdhdi(lIn, 'NAXIS'//itoaf(ilat), ny, 0)
            call output('Phase and pointing centres differ ...'//
     *                  ' saving pointing information')
            call mosInit(nx,ny)
            if (rms.le.0) rms = 1
            call mosSet(1,obsra*DD2R,obsdec*DD2R,rms,pbtype)
            call mosSave(lOut)
          else if (pbtype.ne.telescop) then
            call wrhda(lOut,'pbtype',pbtype)
          endif
        endif
      endif

c     Determine dates.
      call geteqep(lIn,'EPOCH',epoch)
      call geteqep(lIn,'EQUINOX',equinox)
      if (epoch  .lt.1800.0) epoch = equinox
      if (equinox.lt.1800.0) equinox = epoch
      if (equinox.gt.1800.0) call wrhdr(lOut,'epoch',equinox)

      call fitdate(lIn,'DATE-OBS',dval)
      if (dval.le.0d0 .and. epoch.gt.1800.0)
     *  dval = Epo2Jul(dble(epoch),' ')
      if (dval.gt.0d0) call wrhdd(lOut,'obstime',dval)

      if (bunit.ne.' ') call wrhda(lOut,'bunit',bunit)
      if (btype.ne.' ') call wrbtype(lOut,btype)

      call fitrdhdr(lIn,'VOBS',vobs,0.0)
      if (vobs.ne.0.0) call wrhdr(lOut,'vobs',vobs)

      call fitrdhdd(lIn,'RESTFREQ',restfreq,0d0)
      if (restfreq.eq.0d0) call fitrdhdd(lIn,'RESTFRQ',restfreq,0d0)
      if (restfreq.gt.0d0) call wrhdd(lOut,'restfreq',restfreq*1d-9)

      call fitrdhda(lIn,'SPECSYS',specsys,' ')
      if (specsys.ne.' ') call wrhda(lOut,'specsys',specsys)

      call fitrdhda(lIn,'CELLSCAL',cellscal,'CONSTANT')
      if (cellscal.ne.' ') call wrhda(lOut,'cellscal',cellscal)

      call fitrdhdr(lIn,'BMAJ',bmaj,0.0)
      if (bmaj.ne.0.0) call wrhdr(lOut,'bmaj',bmaj*D2R)
      call fitrdhdr(lIn,'BMIN',bmin,0.0)
      if (bmin.ne.0.0) call wrhdr(lOut,'bmin',bmin*D2R)
      call fitrdhdr(lIn,'BPA',bpa,0.0)
      if (bmaj*bmin.ne.0.0) call wrhdr(lOut,'bpa',bpa)

      end

c***********************************************************************

      subroutine crdLd(lIn,naxis,crpix,cdelt,crval,ctype,llrot,ilng,
     *                 ilat)

      integer   lIn, naxis, ilng, ilat
      double precision crpix(naxis), cdelt(naxis), crval(naxis), llrot
      character ctype(naxis)*(*)
c-----------------------------------------------------------------------
c  Load FITS WCS keyvalues and fiddle them into a Miriad coordinate
c  system.  CUNIT is not interpreted and we assume that FITS default
c  units are used, namely degrees, Hz, and m/s.  These are converted to
c  Miriad's internal units: radian, GHz, and km/s.
c-----------------------------------------------------------------------
      integer   iax, j, jax, k
      double precision dtemp, scl
      character algo*8, axtype*16, cax*2, cd*2, pc*2, units*8, wtype*16

      external  m0ij, mi_j, itoaf
      character m0ij*8, mi_j*8, itoaf*2

      data cd, pc /'CD', 'PC'/
c-----------------------------------------------------------------------
      ilng = 0
      ilat = 0
      llrot = 0d0

      do iax = 1, naxis
        cax = itoaf(iax)

c       CRPIXj.
        call fitrdhdd(lIn, 'CRPIX'//cax, crpix(iax), 1d0)

c       CDELTi.
        call fitrdhdd(lIn, m0ij(cd,iax,iax), cdelt(iax), 1d0)
        call fitrdhdd(lIn, mi_j(cd,iax,iax), dtemp,  cdelt(iax))
        call fitrdhdd(lIn, 'CDELT'//cax, cdelt(iax), dtemp)

c       CRVALi.
        call fitrdhdd(lIn, 'CRVAL'//cax, crval(iax), 0d0)

c       CTYPEi.
        call fitrdhda(lIn, 'CTYPE'//cax, ctype(iax), ' ')

c       Clean up CTYPEi.
        if (ctype(iax).eq.' ') then
          if (iax.eq.1) then
c           A stab in the dark!
            ctype(iax) = 'RA---SIN'
          else if (iax.eq.2) then
c           And another one!
            ctype(iax) = 'DEC--SIN'
          endif
        else
c         Strip off leading blanks if any.
          if (ctype(iax)(:1).eq.' ') then
            j = 0
            do k = 1, len(ctype(iax))
              if (j.gt.0) then
                j = j + 1
                ctype(iax)(j:j) = ctype(iax)(k:k)
              else if (ctype(iax)(k:k).ne.' ') then
                j = 1
                ctype(iax)(j:j) = ctype(iax)(k:k)
              endif
            enddo
            ctype(iax)(j+1:) = ' '
          endif

          call ucase(ctype(iax))

          if (ctype(iax).eq.'VLSR') then
c           Assumed to be radio velocity wrt LSRK in km/s.
            ctype(iax) = 'VELO-LSR'
            cdelt(iax) = cdelt(iax)*1d3
            crval(iax) = crval(iax)*1d3
          endif
        endif

        call coCtype(ctype(iax), axtype, wtype, algo, units, scl)

c       Locate the celestial axes.
        if (axtype.eq.'longitude') then
          ilng = iax
        else if (axtype.eq.'latitude') then
          ilat = iax
        endif
      enddo

c     Check for skew/rotations that cannot be handled.
      do jax = 1, naxis
        do iax = 1, naxis
          if (iax.ne.jax .and.
     *      .not.((iax.eq.ilng .and. jax.eq.ilat) .or.
     *            (jax.eq.ilng .and. iax.eq.ilat))) then
            call fitrdhdd(lIn, mi_j(pc,iax,jax), dtemp, 0d0)
            if (dtemp.ne.0d0) call bug('w',
     *        'Cannot handle non-zero FITS skewness parameter '//
     *        mi_j(pc,iax,jax))

            call fitrdhdd(lIn, m0ij(pc,iax,jax), dtemp, 0d0)
            if (dtemp.ne.0d0) call bug('w',
     *        'Cannot handle non-zero FITS skewness parameter '//
     *        m0ij(pc,iax,jax))

            call fitrdhdd(lIn, m0ij(cd,iax,jax), dtemp, 0d0)
            if (dtemp.ne.0d0) call bug('w',
     *        'Cannot handle non-zero FITS skewness parameter '//
     *        m0ij(cd,iax,jax))

            call fitrdhdd(lIn, mi_j(cd,iax,jax), dtemp, 0d0)
            if (dtemp.ne.0d0) call bug('w',
     *        'Cannot handle non-zero FITS skewness parameter '//
     *        mi_j(cd,iax,jax))
          endif
        enddo

        if (jax.ne.ilat .and. jax.ne.ilng) then
          call fitrdhdd(lIn, 'CROTA'//itoaf(jax), dtemp, 0d0)
          if (dtemp.ne.0d0) call bug('w',
     *        'Cannot handle non-zero FITS rotation parameter '//
     *        itoaf(jax))
        endif
      enddo

c     Determine the increment and rotation for celestial axes.
      if (ilat.ne.0 .and. ilng.ne.0) then
        call cdget(lIn,ilng,ilat,cdelt(ilng),cdelt(ilat),llrot)
      endif
      
c     Convert to Miriad units (ignoring CUNITi).
      do iax = 1, naxis
        call coCtype(ctype(iax), axtype, wtype, algo, units, scl)
        cdelt(iax) = cdelt(iax) / scl
        crval(iax) = crval(iax) / scl
      enddo
      
      end

c***********************************************************************

      subroutine cdget(lIn,ilng,ilat,cdelt1,cdelt2,llrot)

      integer lIn,ilng,ilat
      double precision cdelt1,cdelt2,llrot
c-----------------------------------------------------------------------
c  Get the longitude/latitude transformation matrix.
c
c  [This is not strictly correct.]
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision cd11, cd12, cd21, cd22, crota1, crota2, dtemp,
     *          pc11, pc12, pc21, pc22
      character cd*2, pc*2

      external  m0ij, mi_j, itoaf
      character m0ij*8, mi_j*8, itoaf*2

      data cd, pc /'CD', 'PC'/
c-----------------------------------------------------------------------
      call fitrdhdd(lIn,'CROTA'//itoaf(ilat),crota2,0d0)
      crota2 = crota2*DD2R

      call fitrdhdd(lIn,m0ij(pc,ilng,ilng),dtemp,cos(crota2))
      call fitrdhdd(lIn,mi_j(pc,ilng,ilng),pc11,dtemp)
      
      call fitrdhdd(lIn,m0ij(pc,ilng,ilat),dtemp,
     *                  -sin(crota2)*cdelt2/cdelt1)
      call fitrdhdd(lIn,mi_j(pc,ilng,ilat),pc12,dtemp)
      
      call fitrdhdd(lIn,m0ij(pc,ilat,ilng),dtemp,
     *                   sin(crota2)*cdelt1/cdelt2)
      call fitrdhdd(lIn,mi_j(pc,ilat,ilng),pc21,dtemp)
      
      call fitrdhdd(lIn,m0ij(pc,ilat,ilat),dtemp,cos(crota2))
      call fitrdhdd(lIn,mi_j(pc,ilat,ilat),pc22,dtemp)

      call fitrdhdd(lIn,m0ij(cd,ilng,ilng),dtemp,pc11*cdelt1)
      call fitrdhdd(lIn,mi_j(cd,ilng,ilng),cd11,dtemp)

      call fitrdhdd(lIn,m0ij(cd,ilng,ilat),dtemp,pc12*cdelt1)
      call fitrdhdd(lIn,mi_j(cd,ilng,ilat),cd12,dtemp)

      call fitrdhdd(lIn,m0ij(cd,ilat,ilng),dtemp,pc21*cdelt2)
      call fitrdhdd(lIn,mi_j(cd,ilat,ilng),cd21,dtemp)

      call fitrdhdd(lIn,m0ij(cd,ilat,ilat),dtemp,pc22*cdelt2)
      call fitrdhdd(lIn,mi_j(cd,ilat,ilat),cd22,dtemp)

c     Convert matrix elements to cdelt1, cdelt2, and llrot.
      crota1 = atan( cd21/cd11)
      crota2 = atan(-cd12/cd22)
      if (abs(crota2-crota1).gt.1d-3) then
        call bug('w',
     *    'Image plane is skew, which Miriad does not support')
        call bug('w','Using some mean rotation')
      endif
      llrot  = (crota1 + crota2)/2d0
      if (abs(llrot).lt.1d-30) llrot=0d0
      cdelt1 = cd11/cos(llrot)
      cdelt2 = cd22/cos(llrot)

      end

c***********************************************************************

      character*(*) function mi_j(m,i,j)

      character m*2
      integer   i, j
c-----------------------------------------------------------------------
      external  itoaf, spaste
      character itoaf*2, spaste*8
c-----------------------------------------------------------------------
      mi_j = spaste(m//itoaf(i), '_', itoaf(j), ' ')

      end

c***********************************************************************

      character*(*) function m0ij(m,i,j)

      character m*2
      integer   i, j
c-----------------------------------------------------------------------
      write(m0ij, '(a2,i3.3,i3.3)') m, i, j

      end

c***********************************************************************

      subroutine geteqep(lIn,key,value)

      integer lIn
      character key*(*)
      real value
c-----------------------------------------------------------------------
      character card*80,type*8
      integer k1,k2
      logical ok
      double precision dtemp
c-----------------------------------------------------------------------
      call fitsrch(lIn,key,ok)
      if (ok) then
        call fitcdio(lIn,card)
        call ucase(card)
        call getValue(card,type,k1,k2)
        ok = k1.le.k2
      endif

      if (ok) then
        if (card(k1:k1).eq.'B' .or. card(k1:k1).eq.'J') k1 = k1 + 1
        call atodf(card(k1:k2),dtemp,ok)
        if (.not.ok) call bug('f','Error decoding EPOCH/EQUINOX')
        value = dtemp
      else
        value = 0
      endif

      end

c***********************************************************************

      subroutine nodFudge(lIn,lOut)

      integer lIn,lOut
c-----------------------------------------------------------------------
      integer   naxis1, naxis2
      double precision cdelt1, cdelt2, crpix1, crpix2, crval1, crval2
c-----------------------------------------------------------------------
      call bug('i','Assuming equinox of coordinates is B1950')
      call bug('i','Assuming TAN projection')

      call rdhdi(lIn, 'naxis1', naxis1, 0)
      call rdhdi(lIn, 'naxis2', naxis2, 0)
      call rdhdd(lIn, 'cdelt1', cdelt1, 1d0)
      call rdhdd(lIn, 'crval1', crval1, 0d0)
      call rdhdd(lIn, 'cdelt2', cdelt2, 1d0)
      call rdhdd(lIn, 'crval2', crval2, 0d0)

      crpix1 = dble(naxis1/2 + 1)
      crpix2 = dble(naxis2/2 + 1)
      crval1 = crval1 + cdelt1*(crpix1-1d0)
      crval2 = crval2 + cdelt2*(crpix2-1d0)
      cdelt1 = cdelt1 * cos(crval2)

      call wrhdd(lOut, 'crpix1', crpix1)
      call wrhdd(lOut, 'cdelt1', cdelt1)
      call wrhdd(lOut, 'crval1', crval1)
      call wrhda(lOut, 'ctype1', 'RA---TAN')

      call wrhdd(lOut, 'crpix2', crpix2)
      call wrhdd(lOut, 'cdelt2', cdelt2)
      call wrhdd(lOut, 'crval2', crval2)
      call wrhda(lOut, 'ctype2', 'DEC--TAN')

      call wrhdr(lOut, 'epoch',  1950.0)

      end

c***********************************************************************

      subroutine dssFudge(lIn,lOut)

      integer lIn,lOut
c-----------------------------------------------------------------------
c  Approximate a DSS header as a normal Miriad one.
c
c  [N.B. this is a crude approximation, the DSS plate solution is closer
c   to a rotated TAN than a straight CAR! - MRC]
c-----------------------------------------------------------------------
      include 'mirconst.h'

      double precision cdelt1, cdelt2, cnpix1, cnpix2, crpix1, crpix2,
     *          crval1, crval2, objctx, objcty, pltscale, xpixelsz,
     *          ypixelsz
      character objctdec*16, objctra*16
c-----------------------------------------------------------------------
c     Get information from the header of the DSS file.
      call fitrdhdd(lIn, 'XPIXELSZ', xpixelsz, 0d0)
      call fitrdhdd(lIn, 'YPIXELSZ', ypixelsz, 0d0)
      call fitrdhdd(lIn, 'PLTSCALE', pltscale, 0d0)
      call fitrdhdd(lIn, 'OBJCTX',   objctx,   0d0)
      call fitrdhdd(lIn, 'OBJCTY',   objcty,   0d0)
      call fitrdhda(lIn, 'OBJCTRA',  objctra,  ' ')
      call fitrdhda(lIn, 'OBJCTDEC', objctdec, ' ')
      call fitrdhdd(lIn, 'CNPIX1',   cnpix1,   0d0)
      call fitrdhdd(lIn, 'CNPIX2',   cnpix2,   0d0)
      call decval(crval1, objctra)
      call decval(crval2, objctdec)

c     Transform it.
      crpix1 = objctx - cnpix1 + 1
      cdelt1 = -xpixelsz * pltscale * 1d-3 * DAS2R
      crval1 = crval1*15d0*DD2R

      crpix2 = objcty - cnpix2 + 1
      cdelt2 =  ypixelsz * pltscale * 1d-3 * DAS2R
      crval2 = crval2*DD2R

c     Write out the results.
      call wrhdd(lOut, 'crpix1', crpix1)
      call wrhdd(lOut, 'cdelt1', cdelt1)
      call wrhdd(lOut, 'crval1', crval1)
      call wrhda(lOut, 'ctype1', 'RA---CAR')

      call wrhdd(lOut, 'crpix2', crpix2)
      call wrhdd(lOut, 'cdelt2', cdelt2)
      call wrhdd(lOut, 'crval2', crval2)
      call wrhda(lOut, 'ctype2', 'DEC--CAR')

      call wrhdd(lOut, 'epoch',  2000d0)

      end

c***********************************************************************

      subroutine decval(value,string)

      double precision value
      character string*(*)
c-----------------------------------------------------------------------
      integer k1,k2,length
      character token*16
      double precision v1,v2,v3
      logical neg,ok

      external  len1
      integer   len1
c-----------------------------------------------------------------------
      k1 = 1
      k2 = len1(string)
      call getfield(string,k1,k2,token,length)
      neg = token(1:1).eq.'-'
      ok = (.not.neg .and. length.ge.1) .or. (neg .and. length.ge.2)
      if (ok) then
        if (neg) then
          call atodf(token(2:length),v1,ok)
        else
          call atodf(token(1:length),v1,ok)
        endif
      endif
      call getfield(string,k1,k2,token,length)
      ok = ok .and. length.ge.1
      if (ok) call atodf(token(1:length),v2,ok)
      call getfield(string,k1,k2,token,length)
      ok = ok .and. length.ge.1
      if (ok) call atodf(token(1:length),v3,ok)

c     Check for errors.
      if (.not.ok) call bug('f','Error decoding DSS header item')

      value = v1 + v2/60d0 + v3/3600d0
      if (neg) value = -value

      end

c***********************************************************************

      subroutine xyout(in,out,version,boxes)

      character in*(*),out*(*),version*(*)
      integer boxes(*)
c-----------------------------------------------------------------------
c  Write out a image FITS file.
c
c  Inputs:
c    in         Name of the input Miriad image file.
c    out        Name of the output FITS file.
c    version    Version of this program.
c    boxes      Region of interest specification
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      logical   doflag, done
      integer   inPlane(maxnax), nOut(MAXNAX), outPlane(maxnax),
     *          blc(maxnax), i, j, j0, lIn, lOut, naxis, nsize(MAXNAX),
     *          one(maxnax), pArray, pFlags, trc(maxnax)
      character string*64

      external  fitBlank, hdprsnt
      logical   fitBlank, hdprsnt
c-----------------------------------------------------------------------
c     Open the input Miriad file and determine a few things about it.
      call xyopen(lIn,in,'old',MAXNAX,nsize)
      if (nsize(1).gt.maxdim)
     *  call bug('f','Image too big for me to handle')
      call coInit(lIn)
      doflag = hdprsnt(lIn,'mask')
      call rdhdi(lIn,'naxis',naxis,0)
      call BoxSet(boxes,MAXNAX,nsize,' ')
      naxis = min(naxis,MAXNAX)

c     Determine portion of image to copy.
      call BoxInfo(boxes,MAXNAX,blc,trc)
      do i = 1, maxnax
        nOut(i) = (trc(i) - blc(i) + 1)
      enddo

c     Open the output FITS file.
      call fxyopen(lOut,out,'new',naxis,nOut)
      doflag = fitBlank(lOut,doflag)

c     Handle the output header.
      call kwdOut(lIn, lOut, naxis, blc, version)
      string = 'Miriad ' // version
      call fitwrhda(lOut, 'ORIGIN', string)

c     Copy the data.
      call memAlloc(pArray,nsize(1),'r')
      if (doflag) call memAlloc(pFlags,nsize(1),'l')

c     Initialise the plane indices.
      do i = 3, MAXNAX
        one(i-2) = 1
        inPlane(i-2) = blc(i)
        outPlane(i-2) = 1
      enddo

      done = .false.
      do while (.not.done)
        call xysetpl(lIn,maxnax-2,inPlane)
        call fxysetpl(lOut,maxnax-2,outPlane)

        j0 = blc(2)
        do j = 1, nOut(2)
          call xyread(lIn,j0,memr(pArray))
          call fxywrite(lOut,j,memr(pArray + blc(1) - 1))

          if (doflag) then
            call xyflgrd(lIn,j0,meml(pFlags))
            call fxyflgwr(lOut,j,meml(pFlags + blc(1) - 1))
          endif
          j0 = j0 + 1
        enddo
        call planeInc(maxnax-2,1,blc(3),trc(3),inPlane,done)
        call planeInc(maxnax-2,one,one,nOut(3),outPlane,done)
      enddo

      call memFree(pArray,nsize(1),'r')
      if (doflag) call memFree(pFlags,nsize(1),'l')

c     All said and done. Go to sleep.
      call coFin(lIn)
      call xyclose(lIn)
      call fxyclose(lOut)

      end

c***********************************************************************

      subroutine planeInc(n,incr,blc,trc,plane,done)

      integer n,blc(n),trc(n),plane(n),incr(n)
      logical done
c-----------------------------------------------------------------------
c  Move to the next plane.
c-----------------------------------------------------------------------
      integer k
c-----------------------------------------------------------------------
      k = 1
      done = .true.

      do while (done .and. k.le.n)
        done = plane(k).ge.trc(k)
        if (done) then
          plane(k) = blc(k)
        else
          plane(k) = plane(k) + incr(k)
        endif
        k = k + 1
      enddo

      end

c***********************************************************************

      subroutine kwdOut(lIn, lOut, naxis, blc, version)

      integer   lIn, lOut, naxis, blc(naxis)
      character version*(*)
c-----------------------------------------------------------------------
c  Copy (with any necessary scaling) keywords from the Miriad header
c  to the output FITS file.  Miriad's internal units, radian, GHz and
c  km/sec, are converted to FITS default units, degrees, Hz and m/sec.
c
c  Inputs:
c    lIn        Handle of the input  Miriad image.
c    lOut       Handle of the output FITS   image.
c    naxis      Number of dimensions of the image.
c    blc        Bottom-left hand corner pixel value for axis used to
c               determine crpix if a subregion was selected.
c-----------------------------------------------------------------------
      include 'mirconst.h'

      integer   NKWRD2, NKWRD5, NKWRD8
      parameter (NKWRD2=1, NKWRD5=6, NKWRD8=20)

      integer   iax, ilat, ilng, iostat, item, ival, m, n, npnt
      real      rms, rval
      double precision cdelt, crota, crpix, crval, dval, obsdec, obsra,
     *          scl
      character algo*3, axtype*16, cax*2, ctype*32, cval*32, descr*80,
     *          keywrd*12, ktype*16, kwrd2(NKWRD2)*2, kwrd5(NKWRD5)*5,
     *          kwrd8(NKWRD8)*8, telescop*16, text*80, ukey*12,
     *          units*8, wtype*16

      external  binsrcha, hdprsnt, itoaf, len1, spaste
      logical   hdprsnt
      integer   binsrcha, len1
      character itoaf*2, spaste*12

c     Keywords individually taken care of by kwdOut.  These lists must
c     be in ALPHABETICAL ORDER.

c     Non-parameterised keywords.
      data kwrd8 /
     *  'bmaj    ', 'bmin    ', 'bpa     ', 'bunit   ', 'cellscal',
     *  'history ', 'image   ', 'latpole ', 'llrot   ', 'lonpole ',
     *  'mask    ', 'mostable', 'obstime ', 'phi0    ', 'restfreq',
     *  'specsys ', 'telescop', 'theta0  ', 'vobs    ', 'xyzero  '/

c     Parameterised keywords of length 5.
      data kwrd5 /'cdelt', 'crota', 'crpix', 'crval', 'ctype', 'naxis'/

c     Parameterised keywords of length 2.
      data kwrd2 /'pv'/
c-----------------------------------------------------------------------
      call rdhda(lIn, 'bunit', cval, ' ')
      if (cval.ne.' ') call fitwrhda(lOut, 'BUNIT', cval)

      call coGetD(lIn, 'obstime', dval)
      if (dval.gt.0d0) then
        call julday(dval, 'T', cval)
        call fitwrhda(lOut, 'DATE-OBS', cval)
      endif

c     Load the mosaic table.
      if (hdprsnt(lIn, 'mostable')) then
        call mosLoad(lIn, npnt)
        if (npnt.gt.1) call bug('w',
     *    'Mosaicing information not stored in output FITS file')
        call mosGet(1, obsra, obsdec, rms, telescop)
        call fitwrhdd(lOut, 'OBSRA',  obsra *DR2D)
        call fitwrhdd(lOut, 'OBSDEC', obsdec*DR2D)
      else
        call rdhda(lIn, 'telescop', telescop, ' ')
      endif
      if (telescop.ne.' ') call fitwrhda(lOut, 'TELESCOP', telescop)

c     Locate the celestial axes.
      call coFindAx(lIn, 'longitude', ilng)
      call coFindAx(lIn, 'latitude',  ilat)

      do iax = 1, naxis
        call coAxGet(lIn, iax, ctype, crpix, crval, cdelt)

c       Determine scale factor.
        call coCtype(ctype, axtype, wtype, algo, units, scl)

        cax = itoaf(iax)
        call fitwrhdd(lOut, 'CRPIX'//cax, crpix-dble(blc(iax)-1))
        call fitwrhdd(lOut, 'CDELT'//cax, scl*cdelt)
        call fitwrhdd(lOut, 'CRVAL'//cax, scl*crval)
        if (ctype.ne.' ') call fitwrhda(lOut, 'CTYPE'//cax, ctype)

        if (iax.eq.ilat) then
          call coGetD(lIn, 'llrot', crota)
          if (crota.ne.0d0) then
            call fitwrhdd(lOut, 'CROTA'//cax, crota*DR2D)
          endif
        endif
      enddo

c     LONPOLE and LATPOLE.
      if (hdprsnt(lIn, 'lonpole')) then
        call coGetD(lIn, 'lonpole', dval)
        call fitwrhdd(lOut, 'LONPOLE', dval)
      endif

      if (hdprsnt(lIn, 'latpole')) then
        call coGetD(lIn, 'latpole', dval)
        call fitwrhdd(lOut, 'LATPOLE', dval)
      endif

c     phi0, theta0, and xyzero.
      if (hdprsnt(lIn, 'xyzero')) then
        call coGetI(lIn, 'xyzero', ival)
        if (ival.ne.0) then
          keywrd = spaste('PV'//itoaf(ilng), '_', '0', ' ')
          call fitwrhdd(lOut, keywrd, dble(ival))
        endif
      endif

      if (hdprsnt(lIn, 'phi0')) then
        call coGetD(lIn, 'phi0', dval)
        keywrd = spaste('PV'//itoaf(ilng), '_', '1', ' ')
        call fitwrhdd(lOut, keywrd, dval)
      endif

      if (hdprsnt(lIn, 'theta0')) then
        call coGetD(lIn, 'theta0', dval)
        keywrd = spaste('PV'//itoaf(ilng), '_', '2', ' ')
        call fitwrhdd(lOut, keywrd, dval)
      endif

c     Projection parameters.
      do m = 0, 29
        keywrd = 'pv' // itoaf(m)
        if (hdprsnt(lIn, keywrd)) then
          call coGetD(lIn, keywrd, dval)
          keywrd = spaste('PV'//itoaf(ilat), '_', itoaf(m), ' ')
          call fitwrhdd(lOut, keywrd, dval)
        endif
      enddo

c     Write out auxiliary keywords.
      call coGetA(lIn, 'cellscal', cval)
      if (cval.ne.' ') call fitwrhda(lOut, 'CELLSCAL', cval)

      call coGetD(lIn, 'restfreq', dval)
      if (dval.gt.0d0) call fitwrhdd(lOut, 'RESTFREQ', dval*1d9)
      call coGetA(lIn, 'specsys', cval)
      if (cval.ne.' ') call fitwrhda(lOut, 'SPECSYS', cval)
      call rdhdr(lIn, 'vobs', rval, 0.0)
      if (rval.ne.0.0) call fitwrhdr(lOut, 'VOBS', rval)

c     N.B. BPA is stored in degrees in the Miriad header!
      call rdhdr(lIn, 'bmaj', rval, 0.0)
      if (rval.ne.0.0) call fitwrhdr(lOut, 'BMAJ', rval*R2D)
      call rdhdr(lIn, 'bmin', rval, 0.0)
      if (rval.ne.0.0) call fitwrhdr(lOut, 'BMIN', rval*R2D)
      call rdhdr(lIn, 'bpa', rval,  0.0)
      if (rval.ne.0.0) call fitwrhdr(lOut, 'BPA',  rval)


c     Write residual Miriad header items.  Open the "special item" which
c     gives the names of all the items in the file.
      call haccess(lIn, item, '.', 'read', iostat)
      if (iostat.ne.0) call bugno('f', iostat)

      call hreada(item, keywrd, iostat)
      do while (iostat.eq.0)
        if (binsrcha(keywrd,    kwrd8,NKWRD8).eq.0 .and.
     *      binsrcha(keywrd(:5),kwrd5,NKWRD5).eq.0 .and.
     *      binsrcha(keywrd(:2),kwrd2,NKWRD2).eq.0) then
          call hdprobe(lIn,keywrd,descr,ktype,n)
          if (n.eq.1) then
            ukey = keywrd
            call ucase(ukey)
            if (ktype.eq.'integer' .or. ktype.eq.'integer*2') then
              call rdhdi(lIn,keywrd,ival,0)
              call fitwrhdi(lOut,ukey,ival)
            else if (ktype.eq.'real') then
              call rdhdr(lIn,keywrd,rval,0.0)
              call fitwrhdr(lOut,ukey,rval)
            else if (ktype.eq.'double') then
              call rdhdd(lIn,keywrd,dval,0d0)
              call fitwrhdd(lOut,ukey,dval)
            else if (ktype.eq.'character') then
              call fitwrhda(lOut,ukey,descr(1:len1(descr)))
            else
              call ucase(descr)
              text = keywrd(1:8)//'= '//descr
              call fitcdio(lOut,text)
            endif
          endif
        endif

        call hreada(item,keywrd,iostat)
      enddo
      call hdaccess(item,iostat)

c     Write the history file as HISTORY comments.
      if (hdprsnt(lIn,'history ')) then
        call copyHist(lIn,lOut,version)
      endif

      end

c***********************************************************************

      subroutine copyHist(lIn,lOut,version)

      integer lIn,lOut
      character version*(*)
c-----------------------------------------------------------------------
c  Copy out the history comments.
c
c  Input:
c    lIn        The handle of the input Miriad file.
c    lOut       The handle of the output FITS file.
c
c-----------------------------------------------------------------------
      logical eof
      character card*132,line*80,name*32
      integer narg,i,l1,l2,length,lu,iostat
      logical dofile
      character file*256
      double precision julian

c     Externals.
      integer iargc,len1
c-----------------------------------------------------------------------
      call hisOpen(lIn,'read')
      call hisRead(lIn,card,eof)
      do while (.not.eof)
        line = 'HISTORY '//card(1:72)
        call fitcdio(lOut,line)
        call hisRead(lIn,card,eof)
      enddo
      call hisclose(lIn)

c     Write some extra FITS history.
      narg = iargc()
      name = 'HISTORY FITS:'
      l2 = len(line)
      l1 = len1(name)
      line = name(1:l1)//' Miriad fits: '//version
      call fitcdio(lOut,line)
      call TodayJul(julian)
      call JulDay(julian, 'H', file)
      line = name(1:l1)//' Executed on: '//file(1:len1(file))
      call fitcdio(lOut,line)
      line(l1+1:) = ' Command line inputs follow:'
      call fitcdio(lOut,line)

      line(l1+1:l1+4) = ' '
      l1 = l1 + 5

      dofile = .false.
      do i = 1, narg
        if (dofile) then
          call getarg(i,file)
          call txtopen(lu,file,'old',iostat)
          if (iostat.ne.0) then
            call bug('w','Error opening input parameter file')
            call bugno('w',iostat)
          else
            call txtread(lu,line(l1:l2),length,iostat)
            do while (iostat.eq.0)
              length = min(l2,length + l1 - 1)
              call fitcdio(lOut,line(1:length))
              call txtread(lu,line(l1:l2),length,iostat)
            enddo
            call txtclose(lu)
          endif
          dofile = .false.
        else
          call getarg(i,line(l1:l2))
          if (line(l1:l2).eq.'-f') then
            dofile = .true.
          else
            call fitcdio(lOut,line(1:l2))
          endif
        endif
      enddo

      line =
     * 'HISTORY FITS: NOTE: Use options=varwt if loading into Miriad'
      call fitcdio(lOut,line)

      end

c***********************************************************************

      subroutine histIn(lIn,lOut,version)

      integer lIn,lOut
      character version*(*)
c-----------------------------------------------------------------------
c  Create the history file.
c
c  Input:
c    lIn        Handle of the input FITS file.
c    lOut       Handle of the output Miriad file.
c    version
c-----------------------------------------------------------------------
      integer    NHIST, NKWRD8, NKWRD7, NKWRD5, NKWRD2
      parameter (NHIST=4, NKWRD8=45, NKWRD7=5, NKWRD5=9, NKWRD2=1)

      logical   found
      integer   i, unrec
      character card*80, histry(NHIST)*8, kwrd2(NKWRD2)*2,
     *          kwrd5(NKWRD5)*5, kwrd7(NKWRD7)*7, kwrd8(NKWRD8)*8

      external  binsrcha, itoaf, len1
      integer   binsrcha, len1
      character itoaf*5

c     These keyword lists must be maintained in alphabetical order.
c     Unrecognised keywords are written to history.
      data histry /
     *  '        ', 'COMMAND ', 'COMMENT ', 'HISTORY '/

c     Non-parameterised keywords.
      data kwrd8 /
     *  'ALTRPIX ', 'ALTRVAL ', 'BITPIX  ', 'BLANK   ', 'BLOCKED ',
     *  'BMAJ    ', 'BMIN    ', 'BPA     ', 'BSCALE  ', 'BTYPE   ',
     *  'BUNIT   ', 'BZERO   ', 'CELLSCAL', 'DATAMAX ', 'DATAMIN ',
     *  'DATE    ', 'DATE-MAP', 'DATE-OBS', 'END     ', 'EPOCH   ',
     *  'EXTEND  ', 'GCOUNT  ', 'GROUPS  ', 'INSTRUME', 'LSTART  ',
     *  'LSTEP   ', 'LTYPE   ', 'LWIDTH  ', 'NITERS  ', 'OBJECT  ',
     *  'OBSDEC  ', 'OBSERVER', 'OBSRA   ', 'ORIGIN  ', 'PBFWHM  ',
     *  'PBTYPE  ', 'PCOUNT  ', 'RESTFREQ', 'RMS     ', 'SIMPLE  ',
     *  'TELESCOP', 'VELREF  ', 'VOBS    ', 'XSHIFT  ', 'YSHIFT  '/

c     Parameterised keywords of base length 7.
      data kwrd7 /
     *  'EQUINOX',  'LATPOLE',  'LONPOLE',  'RESTFRQ',  'SPECSYS'/

c     Parameterised keywords of base length 5.
      data kwrd5 /
     *  'CDELT',    'CROTA',    'CRPIX',    'CRVAL',    'CTYPE',
     *  'NAXIS',    'PSCAL',    'PTYPE',    'PZERO'/

c     Parameterised keywords of base length 2.
      data kwrd2 /
     *  'PV'/
c-----------------------------------------------------------------------
c     Search for the 'SIMPLE' keyword and read the keyrecord.
      call fitsrch(lIn, 'SIMPLE', found)
      if (.not.found) call bug('f','Weird bug')
      call fitcdio(lIn, card)

c     Look at each keyword in turn.
      unrec = 0
      do while (card(:8).ne.'END')
        call fitcdio(lIn, card)

        i = binsrcha(card(1:8), histry, NHIST)
        if (i.ne.0) then
          call hisCard(lOut, card)
        else
          i = binsrcha(card(:8), kwrd8, NKWRD8)

          if (i.eq.0) then
            i = binsrcha(card(:7), kwrd7, NKWRD7)

            if (i.eq.0) then
              i = binsrcha(card(:5), kwrd5, NKWRD5)

              if (i.eq.0) then
                i = binsrcha(card(:2), kwrd2, NKWRD2)

                if (i.eq.0) then
                  unrec = unrec + 1
                  call hiswrite(lOut,card)
                endif
              endif
            endif
          endif
        endif
      enddo

      if (unrec.gt.0) then
        card = itoaf(unrec)
        call bug('w', card(:len1(card)) //
     *    ' unrecognised cards were written to the history file')
      endif

c     Add new history
      card = 'FITS: Miriad ' // version
      call hiswrite(lOut, card)
      call hisinput(lOut, 'FITS')

      end

c***********************************************************************

      subroutine hisCard(lOut,card)

      integer lOut
      character card*(*)
c-----------------------------------------------------------------------
c  Determine the portion of a history card to write out to the history
c  file. This aims at cutting out as much of the extraneous crap that
c  AIPS puts in history comments as possible, to attempt to improve the
c  appearance of the history file.
c
c  Input:
c    lOut       Handle of the output file.
c    card       The card.
c-----------------------------------------------------------------------
      integer k1,k2
      logical more

c     Externals.
      integer len1
c-----------------------------------------------------------------------
      k1 = 9
      if (card(k1:k1).eq.' ') k1 = 10
      k2 = len1(card)
      if (k2-k1.gt.len('HISTORY')) then
        if (card(k2-6:k2).eq.'HISTORY') k2 = len1(card(1:k2-7))
      endif

      more = .true.
      do while (k1.le.k2 .and. more)
        more = card(k1:k1).le.' ' .or. card(k1:k1).eq.'='
        if (more) k1 = k1 + 1
      enddo

      if (k1.le.k2) call hiswrite(lOut,card(k1:k2))

      end

c***********************************************************************

      subroutine getValue(card,type,k1,k2)

      character card*(*),type*(*)
      integer k1,k2
c-----------------------------------------------------------------------
c  Determine the type of a value and returns the indices that delimit
c  the value.
c
c  Returns type "unknown" if the keyword looks bad.  In particular the
c  first 8 characters must be alphanumeric with possible trailing
c  blanks.
c
c  Input:
c    card       The FITS card.
c
c  Output:
c    type       Data type: 'integer', 'real', 'double', 'ascii' or
c               'unknown'.
c    k1,k2      These delimit the value in CARD, for other than
c               'unknown'.
c-----------------------------------------------------------------------
      character c*1
      integer l,i
      logical more

c     Externals.
      integer len1
c-----------------------------------------------------------------------
      type = 'unknown'
      if (len(card).le.8) return

c     Look for the first non-alphanumeric character.
      l = len1(card(1:8))
      do i = 1, l
        c = card(i:i)
        if (.not.((c.ge.'a' .and. c.le.'z') .or.
     *            (c.ge.'A' .and. c.le.'Z') .or.
     *            (c.ge.'0' .and. c.le.'9'))) return
      enddo

c     Skip to the first non-blank character after the equals sign.
      k1 = 9
      k2 = len(card)
      call spanchar(card,k1,k2,' ')
      call spanchar(card,k1,k2,'=')
      call spanchar(card,k1,k2,' ')
      if (k1.gt.k2) return

c     At this stage, we should have a quote, plus/minus sign, decimal
c     point, or numeric digit.
      c = card(k1:k1)
      if (c.eq.'''') then
        k1 = k1 + 1
        l = k1
        call scanchar(card,l,k2,'''')
        k2 = l - 1
        do while (k2.ge.k1 .and. card(k2:k2).eq.' ')
          k2 = k2 - 1
        enddo
        if (k1.le.k2) type = 'ascii'

      else if (c.eq.'T' .or. c.eq.'F') then
c       Handle a logical.
        k2 = k1
        type = 'logical'

      else
c       Handle what must be a numeric value.
        l = k1
        type = 'integer'
        more = .true.
        do while (l.le.k2 .and. more)
          c = card(l:l)
          if (c.eq.' ' .or. c.eq.'/') then
            more = .false.
            k2 = l - 1
            if (k1.gt.k2) type = 'unknown'
          else if (c.eq.'.' .or. c.eq.'E' .or. c.eq.'e') then
            type = 'real'
          else if (c.eq.'d' .or. c.eq.'D') then
            type = 'double'
          else if ((c.lt.'0' .or. c.gt.'9') .and.
     *            c.ne.'+' .and. c.ne.'-') then
            type = 'unknown'
            more = .false.
          endif
          l = l + 1
        enddo
      endif

      end
