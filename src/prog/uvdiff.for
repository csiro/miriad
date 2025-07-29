c***********************************************************************
      program uvdiff
c
c= uvdiff -- Difference two visibility datasets.
c& rjs
c: uv analysis
c+
c       Given two Miriad visibility datasets, UVDIFF matches and
c       optionally subtracts one from the other.  In doing this, UVDIFF
c       assumes that the two datasets sample the same source using the
c       same array configuration and observing frequency setup (no
c       checks are made to verify these).  UVDIFF matches visibility
c       records based on baseline number, polarisation type and hour
c       angle.  If the hour angles in the two datasets do not match to
c       better than 1 second, UVDIFF linearly interpolates between two
c       integrations of one dataset to the appropriate hour angle of the
c       other.
c
c       NOTE: UVDIFF does not apply calibration corrections to either
c       of the input datasets.  The datasets should be in time order.
c@ vis
c       The two datasets to difference.  No default.  When differencing,
c       the output is the first minus the second.  The first visibility
c       dataset is used as the template for the output.  It is the
c       second dataset that is interpolated when needed.
c@ select
c       Standard visibility selection keyword.  See the help on "select"
c       for more information.  Note that this selection is applied ONLY
c       to the first visibility dataset.  The default is to select
c       everything.
c@ out
c       The output dataset.  No default.
c@ mode
c       This determines what data is written.  Possible values are:
c         difference Write the difference of the two inputs (the first
c                    minus the second).  This is the default.
c         two        Write the data from the second input, interpolated
c                    to the first.
c         one        Write the first dataset.
c       With the exception of the visibility data itself, all three
c       possibilities will produce identical datasets.  This includes
c       identical flagging information.
c       Any of these modes can be prefixed with a minus sign.  In this
c       case the output visibilities are negated.
c@ tol
c       Interpolation tolerance, in minutes.  This gives the maximum gap
c       between two integrations to interpolate across.  The default is
c       2 minutes.
c
c--
c  History:
c    04-jun-96 rjs  Preliminary version.
c    20-jun-96 rjs  Bring it up to scratch.
c    14-aug-96 rjs  Added ability to negate the output.
c    09-jul-04 jwr  Renamed Unpack to Unpck to avoid compiler
c                   complaining about unimplemented intrisics
c    24-jan-07 rjs  Default linetype.
c
c  Bugs/Shortcomings:
c    * Should handle the conjugate symmetry property, and match data
c      over a wider range of HA.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      integer MAXSELS, NMODES
      parameter(MAXSELS=1000, NMODES=6)

      logical flags1(MAXCHAN), flags2(MAXCHAN), negate
      integer i, nchan, nout, npol, pol, tIn, tOut
      real    sels(MAXSELS), tol
      double precision lst, preamble(8)
      complex data1(MAXCHAN), data2(MAXCHAN)
      character ctemp*12, ltype*16, mode*12, modes(NMODES)*12, out*64,
     *          version*72, vis1*64, vis2*64

c     Externals.
      logical hdprsnt
      character versan*72

      data modes/'difference  ','one         ','two         ',
     *           '-difference ','-one        ','-two        '/
c-----------------------------------------------------------------------
      version = versan('uvdiff')

c     Get the input parameters.
      call keyini
      call keya('vis',vis1,' ')
      call keya('vis',vis2,' ')
      call keya('out',out,' ')
      call keymatch('mode',NMODES,modes,1,mode,nout)
      if(nout.eq.0)mode = modes(1)
      negate = mode(1:1).eq.'-'
      if(negate)then
        ctemp = mode
        mode  = ctemp(2:)
      endif
      call keyr('tol',tol,2.0)
      call SelInput('select',sels,MAXSELS)
      call keyfin
      if(vis1.eq.' ' .or. vis2.eq.' ' .or. out.eq.' ')
     *  call bug('f','An input or output is missing')
      if(tol.le.0)call bug('f','Invalid interpolation tolerance')
      tol = tol * 2.0*PI/(24.0*60.0)

c     Open the inputs and outputs.
      call uvopen(tIn,vis1,'old')
      if(hdprsnt(tIn,'gains').or.hdprsnt(tIn,'leakage').or.
     *   hdprsnt(tIn,'bandpass'))then
        call bug('w',
     *    'Uvdiff does not apply pre-existing calibration tables')
        if(hdprsnt(tIn,'gains'))
     *    call bug('w','No antenna gain calibration applied')
        if(hdprsnt(tIn,'leakage'))
     *    call bug('w','No polarization calibration applied')
        if(hdprsnt(tIn,'bandpass'))
     *    call bug('w','No bandpass calibration applied')
      endif

      call uvset(tIn,'preamble','uvw/time/baseline/pol/ra/lst',
     *           0,0.d0,0.d0,0.d0)
      call SelApply(tIn,sels,.true.)

      call BInit(vis2)
      call getltype(tIn,ltype)
      call varInit(tIn,ltype)
      call uvopen(tOut,out,'new')
      call uvset(tOut,'preamble','uvw/time/baseline',
     *           0,0.d0,0.d0,0.d0)
      call hdcopy(tIn,tOut,'history')
      call hisopen(tOut,'append')
      call hiswrite(tOut,'UVDIFF: Miriad '//version)
      call hisinput(tOut,'UVDIFF')
      call hisclose(tOut)
      call varOnit(tIn,tOut,ltype)

c     Loop over all the data.
      call uvread(tIn,preamble,data1,flags1,MAXCHAN,nchan)
      dowhile(nchan.gt.0)
        call uvrdvri(tIn,'pol',pol,0)
        call uvrdvrd(tIn,'lst',lst,0.d0)

c       Get data from the second visibility dataset.
        call BGet(preamble(5),data2,flags2,nchan,tol)

c       Subtract if required.
        if(mode.eq.'one')then
          do i=1,nchan
            flags2(i) = flags1(i).and.flags2(i)
          enddo
        else if(mode.eq.'two')then
          do i=1,nchan
            data1(i)  = data2(i)
            flags2(i) = flags1(i).and.flags2(i)
          enddo
        else
          do i=1,nchan
            data1(i)  = data1(i) - data2(i)
            flags2(i) = flags1(i).and.flags2(i)
          enddo
        endif

c       Negate if required.
        if(negate)then
          do i=1,nchan
            data1(i) = -data1(i)
          enddo
        endif

        call varCopy(tIn,tOut)
        call uvrdvri(tIn,'npol',npol,0)
        call uvputvri(tOut,'pol',pol,1)
        call uvputvri(tOut,'npol',npol,1)
        call uvwrite(tOut,preamble,data1,flags2,nchan)
        call uvread(tIn,preamble,data1,flags1,MAXCHAN,nchan)
      enddo

      call uvclose(tIn)
      call uvclose(tOut)
      call BFin

      end
c***********************************************************************
      subroutine getltype(lIn,ltype)

      integer lIn
      character ltype*(*)

c     Determine the default line type.
c-----------------------------------------------------------------------
      logical update
      integer length
      character type*1

      call uvprobvr(lIn,'corr',type,length,update)
      if(type.eq.'j'.or.type.eq.'r'.or.type.eq.'c')then
        ltype = 'channel'
      else
        ltype = 'wide'
      endif

      end
c***********************************************************************
      subroutine BGet(vars1,data2,flags2,nchan1,tol)

      integer nchan1
      real tol
      double precision vars1(4)
      complex data2(nchan1)
      logical flags2(nchan1)

c  Generate the interpolated data.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'uvdiff.h'

      real mtol
      parameter(mtol = 2.0*PI/86400.0)

      logical oki, okj
      integer bl1, i, ipol1
      ptrdiff cidxi, cidxj, fidxi, fidxj
      real    wi, wj
      double precision ha1

      call Unpck(vars1,ha1,bl1,ipol1)

c     Go through the dataset until we find the right integrations.
c     Note that in time-sorted, multi-day datasets, ha2(iha), the hour
c     angle for the older integration, may be greater than that for the
c     newer integration, ha2(jha).
      dowhile(abs(ha1-ha2(iha)).gt.mtol .and.
     *        abs(ha1-ha2(jha)).gt.mtol .and.
     *       ((ha2(iha).le.ha2(jha) .and. ha1.gt.ha2(jha)) .or.
     *        (ha2(iha).gt.ha2(jha) .and. ha1.gt.ha2(jha))) .and.
     *        nchan(1).gt.0 .and.
     *        nchan(2).gt.0)
        call Bload
      enddo

c     Cases of straight copy.
      cidxi = cindices(ipol1,bl1,iha)
      cidxj = cindices(ipol1,bl1,jha)
      fidxi = findices(ipol1,bl1,iha)
      fidxj = findices(ipol1,bl1,jha)
      oki = cidxi.ne.0 .and. fidxi.ne.0 .and. nchan1.eq.nchan(iha)
      okj = cidxj.ne.0 .and. fidxj.ne.0 .and. nchan1.eq.nchan(jha)
      cidxi = cidxi - 1
      cidxj = cidxj - 1
      fidxi = fidxi - 1
      fidxj = fidxj - 1

      if(oki .and. abs(ha1-ha2(iha)).lt.mtol)then
        do i=1,nchan1
          data2(i)  = Memc(cidxi+i)
          flags2(i) = Meml(fidxi+i)
        enddo
      else if(okj .and. abs(ha1-ha2(jha)).lt.mtol)then
        do i=1,nchan1
          data2(i)  = Memc(cidxj+i)
          flags2(i) = Meml(fidxj+i)
        enddo

      else if(oki .and. okj .and.
     *  (ha1-ha2(iha))*(ha2(jha)-ha1).ge.0.0 .and.
     *   abs(ha2(jha)-ha2(iha)).le.tol)then
c       Interpolate linearly between the two now.
        wi = real(1.0 - abs((ha1-ha2(iha))/(ha2(jha)-ha2(iha))))
        wj = real(1.0 - abs((ha1-ha2(jha))/(ha2(jha)-ha2(iha))))
        do i=1,nchan1
          data2(i)  = wi*Memc(cidxi+i) + wj*Memc(cidxj+i)
          flags2(i) = Meml(fidxi+i).and.Meml(fidxj+i)
        enddo

      else
c       Case of it all failing.
        do i=1,nchan1
          data2(i)  = (0.0,0.0)
          flags2(i) = .false.
        enddo
      endif

      end
c***********************************************************************
      subroutine unpck(vars,ha,bl,ipol)

      integer bl,ipol
      double precision ha, vars(4)
c
c  Unpck variables, in the order baseline,pol,ra,lst
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'uvdiff.h'
      integer i1,i2

      ipol = -nint(vars(2)) - 4
      if(ipol.lt.POLMIN .or. ipol.gt.POLMAX)
     *  call bug('f','Invalid polarisation type')
      if(polindx(ipol).eq.0)then
        npols = npols + 1
        if(npols.gt.MAXPOL)call bug('f','Too many polarisations')
        polindx(ipol) = npols
      endif
      ipol = polindx(ipol)

      call Basant(vars(1),i1,i2)
      bl = ((i2-1)*i2)/2 + i1
      ha = mod(vars(4)-vars(3), 2d0*DPI)
      if(ha.gt.DPI)then
        ha = ha - 2.d0*DPI
      else if(ha.lt.-DPI)then
        ha = ha + 2*DPI
      endif

      end
c***********************************************************************
      subroutine BFin
c
c-----------------------------------------------------------------------
      include 'uvdiff.h'
      call uvclose(tno)
      end
c***********************************************************************
      subroutine BInit(vis)

      character vis*(*)
c-----------------------------------------------------------------------
      include 'uvdiff.h'
      integer i

c     Externals.
      logical hdprsnt

c     Open the second dataset.
      call uvopen(tno,vis,'old')
      if(hdprsnt(tno,'gains')   .or.
     *   hdprsnt(tno,'leakage') .or.
     *   hdprsnt(tno,'bandpass'))then
        call bug('w',
     *    'Uvdiff does not apply pre-existing calibration tables')
        if(hdprsnt(tno,'gains'))
     *    call bug('w','No antenna gain calibration applied')
        if(hdprsnt(tno,'leakage'))
     *    call bug('w','No polarization calibration applied')
        if(hdprsnt(tno,'bandpass'))
     *    call bug('w','No bandpass calibration applied')
      endif

      call uvset(tno,'preamble','baseline/pol/ra/lst',0,0.d0,0.d0,0.d0)

      npols = 0
      do i = POLMIN, POLMAX
        polindx(i) = 0
      enddo

      jha = 0
      call BLoad
      call BLoad
      if(nchan(1).eq.0 .or. nchan(2).eq.0)
     *        call bug('f','No integrations found')
      end
c***********************************************************************
      subroutine Bload
c
c  Load the next integration.
c-----------------------------------------------------------------------
      include 'uvdiff.h'

      logical lspare(MAXCHAN)
      integer base(2), i, ibase, ipol, nspare, pol(2)
      ptrdiff cidx, fidx
      double precision ha, pspare(4)
      complex cspare(MAXCHAN)

      save base, pol
      save cspare, lspare, nspare, pspare

      if (jha.eq.0) then
c       Initialize indexes.
        do jha = 1, 2
          do ibase = 1, MAXBASE
            do ipol = 1, MAXPOL
              cindices(ipol,ibase,jha) = 0
              findices(ipol,ibase,jha) = 0
            enddo
          enddo

          pol(jha)  = 0
          base(jha) = 0
        enddo

        jha = 2

c       Load the spare buffer.
        nchan(1) = 1
        nchan(2) = 1
        call uvread(tno,pspare,cspare,lspare,MAXCHAN,nspare)
      end if

c     Switch buffers.
      iha = jha
      jha = 1 + mod(jha,2)

c     Clear the target buffer.
      do ibase=1,base(jha)
        do ipol=1,pol(jha)
          if(cindices(ipol,ibase,jha).gt.0)
     *      call memFree(cindices(ipol,ibase,jha),nchan(jha),'c')
          if(findices(ipol,ibase,jha).gt.0)
     *      call memFree(findices(ipol,ibase,jha),nchan(jha),'l')
          cindices(ipol,ibase,jha) = 0
          findices(ipol,ibase,jha) = 0
        enddo
      enddo
      base(jha) = 0
      pol(jha)  = 0

c     Determine the polarisation and hour angle of the spare record.
      nchan(jha) = nspare
      if(nchan(1).le.0 .or. nchan(2).le.0)return
      call unpck(pspare,ha,ibase,ipol)
      ha2(jha) = ha

      dowhile(abs(ha-ha2(jha)).lt.1e-4 .and. nspare.eq.nchan(jha))
c       Which output slot does the current record fall into?
        if(cindices(ipol,ibase,jha).gt.0.or.
     *     findices(ipol,ibase,jha).gt.0)call bug('f',
     *    'Multiple records for the same baseline within integration')
        call memAlloc(cindices(ipol,ibase,jha),nchan(jha),'c')
        call memAlloc(findices(ipol,ibase,jha),nchan(jha),'l')
        cidx = cindices(ipol,ibase,jha) - 1
        fidx = findices(ipol,ibase,jha) - 1
        do i=1,nspare
          Memc(cidx+i) = cspare(i)
          Meml(fidx+i) = lspare(i)
        enddo

        base(jha) = max(base(jha),ibase)
        pol(jha)  = max(pol(jha), ipol)

c       Get another record.
        call uvread(tno,pspare,cspare,lspare,MAXCHAN,nspare)
        if(nspare.gt.0)call unpck(pspare,ha,ibase,ipol)
      enddo

      if(abs(ha-ha2(jha)).lt.1e-4 .and. nspare.ne.0)
     *  call bug('f','Number of channels changed within an integration')

      end
