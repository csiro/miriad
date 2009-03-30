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
c
c $Id: uvdiff.for,v 1.3 2009/03/30 00:09:46 cal103 Exp $
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      integer MAXSELS
      parameter(MAXSELS=1000)
      character vis1*64,vis2*64,out*64,ltype*16
      complex data(MAXCHAN),mdata(MAXCHAN)
      logical flags(MAXCHAN),mflags(MAXCHAN)
      real tol,sels(MAXSELS)
      double precision preamble(8),lst
      integer tIn,tOut,nchan,pol,i,npol

      integer NMODES
      parameter(NMODES=6)
      character modes(NMODES)*12,mode*12,ctemp*12, version*80
      logical negate
      integer nout

c     Externals.
      logical hdprsnt
      character versan*80

      data modes/'difference  ','one         ','two         ',
     *           '-difference ','-one        ','-two        '/
c-----------------------------------------------------------------------
      version = versan('uvdiff',
     *  '$Id: uvdiff.for,v 1.3 2009/03/30 00:09:46 cal103 Exp $')

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
      if(vis1.eq.' '.or.vis2.eq.' '.or.out.eq.' ')
     *  call bug('f','An input or output is missing')
      if(tol.le.0)call bug('f','Invalid interpolation tolerance')
      tol = 2*PI*tol/(24.*60.)

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
     *           0,0.,0.,0.)
      call SelApply(tIn,sels,.true.)

      call BInit(vis2)
      call getltype(tIn,ltype)
      call varInit(tIn,ltype)
      call uvopen(tOut,out,'new')
      call uvset(tOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
      call hdcopy(tIn,tOut,'history')
      call hisopen(tOut,'append')
      call hiswrite(tOut,'UVDIFF: Miriad '//version)
      call hisinput(tOut,'UVDIFF')
      call hisclose(tOut)
      call varOnit(tIn,tOut,ltype)

c     Loop over all the data.
      call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
      dowhile(nchan.gt.0)
        call uvrdvri(tIn,'pol',pol,0)
        call uvrdvrd(tIn,'lst',lst,0.d0)

c       Get the model data.
        call BGet(preamble(5),mdata,mflags,nchan,tol)

c       Subtract if required.
        if(mode.eq.'one')then
          do i=1,nchan
            mdata(i) = data(i)
            mflags(i) = flags(i).and.mflags(i)
          enddo
        else if(mode.eq.'two')then
          do i=1,nchan
            mflags(i) = flags(i).and.mflags(i)
          enddo
        else
          do i=1,nchan
            mdata(i)  = data(i) - mdata(i)
            mflags(i) = flags(i).and.mflags(i)
          enddo
        endif

c       Negate if required.
        if(negate)then
          do i=1,nchan
            mdata(i) = -mdata(i)
          enddo
        endif

        call varCopy(tIn,tOut)
        call uvrdvri(tIn,'npol',npol,0)
        call uvputvri(tOut,'pol',pol,1)
        call uvputvri(tOut,'npol',npol,1)
        call uvwrite(tOut,preamble,mdata,mflags,nchan)
        call uvread(tIn,preamble,data,flags,MAXCHAN,nchan)
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
      character type*4

      call uvprobvr(lIn,'corr',type,length,update)
      if(type.eq.'j'.or.type.eq.'r'.or.type.eq.'c')then
        ltype = 'channel'
      else
        ltype = 'wide'
      endif

      end
c***********************************************************************
      subroutine BGet(vars,rdata,rflags,nchan1,tol)

      integer nchan1
      real tol
      double precision vars(4)
      complex rdata(nchan1)
      logical rflags(nchan1)

c  Generate the interpolated data.
c-----------------------------------------------------------------------
      include 'mirconst.h'
      real mtol
      parameter(mtol=2.0*PI/86400.0)
      include 'uvdiff.h'
      integer bl,ipol,i,cidx1,cidx2,fidx1,fidx2
      logical ok1,ok2
      real w1,w2
      double precision mha

      call Unpck(vars,mha,bl,ipol)

c     Go through the dataset until we find the right integrations.
      dowhile(min(abs(mha-ha(1)),abs(mha-ha(2))).gt.mtol .and.
     *        mha.gt.max(ha(1),ha(2)) .and.
     *        min(nchan(1),nchan(2)).gt.0)
        call Bload
      enddo

c     Cases of straight copy.
      cidx1 = cindices(ipol,bl,1)
      cidx2 = cindices(ipol,bl,2)
      fidx1 = findices(ipol,bl,1)
      fidx2 = findices(ipol,bl,2)
      ok1 = cidx1.ne.0.and.fidx1.ne.0.and.nchan1.eq.nchan(1)
      ok2 = cidx2.ne.0.and.fidx2.ne.0.and.nchan1.eq.nchan(2)
      cidx1 = cidx1 - 1
      cidx2 = cidx2 - 1
      fidx1 = fidx1 - 1
      fidx2 = fidx2 - 1

      if(ok1.and.abs(mha-ha(1)).lt.mtol)then
        do i=1,nchan1
          rdata(i) = Memc(cidx1+i)
          rflags(i) = Meml(fidx1+i)
        enddo
      else if(ok2.and.abs(mha-ha(2)).lt.mtol)then
        do i=1,nchan1
          rdata(i) = Memc(cidx2+i)
          rflags(i) = Meml(fidx2+i)
        enddo

      else if(ok1.and.ok2.and.
c       Linearly interpolate between the two now.
     *   (mha-ha(1))*(ha(2)-mha).ge.0.and.abs(ha(2)-ha(1)).le.tol)then
        w1 = 1 - abs((mha-ha(1))/(ha(2)-ha(1)))
        w2 = 1 - abs((mha-ha(2))/(ha(2)-ha(1)))
        do i=1,nchan1
          rdata(i) = w1*Memc(cidx1+i) + w2*Memc(cidx2+i)
          rflags(i) = Meml(fidx1+i).and.Meml(fidx2+i)
        enddo

      else
c       Case of it all failing.
        do i=1,nchan1
          rflags(i) = .false.
          rdata(i)  = 0
        enddo
      endif

      end
c***********************************************************************
      subroutine unpck(vars,mha,bl,ipol)

      double precision vars(4),mha
      integer bl,ipol
c
c  Unpck variables, in the order baseline,pol,ra,lst
c-----------------------------------------------------------------------
      include 'mirconst.h'
      include 'uvdiff.h'
      integer i1,i2

      ipol = - nint(vars(2)) - 4
      if(ipol.lt.PolMin.or.ipol.gt.PolMax)
     *  call bug('f','Invalid polarisation type')
      if(polindx(ipol).eq.0)then
        npols = npols + 1
        if(npols.gt.MAXPOL)call bug('f','Too many polarisations')
        polindx(ipol) = npols
      endif
      ipol = polindx(ipol)

      call Basant(vars(1),i1,i2)
      bl = ((i2-1)*i2)/2 + i1
      mha = mod(vars(4) - vars(3),2.d0*DPI)
      if(mha.gt.DPI)then
        mha = mha - 2.d0*DPI
      else if(mha.lt.-DPI)then
        mha = mha + 2*DPI
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
      integer i,j,k

c     Externals.
      logical hdprsnt

c     Open the dataset to be used as the template.
      call uvopen(tno,vis,'old')
      if(hdprsnt(tno,'gains').or.hdprsnt(tno,'leakage').or.
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

      call uvset(tno,'preamble','baseline/pol/ra/lst',0,0.,0.,0.)

c     Get ready to do things.
      do k=1,2
        mbase(k) = 0
        mpol(k) = 0
        do j=1,MAXBASE
          do i=1,MAXPOL
            cindices(i,j,k) = 0
            findices(i,j,k) = 0
          enddo
        enddo
      enddo

      npols = 0
      do i=PolMin,PolMax
        polindx(i) = 0
      enddo

      nchan(1) = 1
      nchan(2) = 1
      ha(1) = 0
      ha(2) = 1
      call uvread(tno,pspare,cspare,lspare,MAXCHAN,nspare)
      call BLoad
      ha(2) = ha(1) - 1
      call BLoad
      if(min(nchan(1),nchan(2)).eq.0)
     *        call bug('f','No integrations found')
      end
c***********************************************************************
      subroutine Bload
c
c  Load the next integration.
c-----------------------------------------------------------------------
      include 'uvdiff.h'
      integer ipnt,ipol,bl,i,j,cidx,fidx
      double precision mha

c     Delete the old contents of this integration.
      ipnt = 1
      if(ha(1).gt.ha(2))ipnt = 2
      do j=1,mbase(ipnt)
        do i=1,mpol(ipnt)
          if(cindices(i,j,ipnt).gt.0)
     *      call memFree(cindices(i,j,ipnt),nchan(ipnt),'c')
          if(findices(i,j,ipnt).gt.0)
     *      call memFree(findices(i,j,ipnt),nchan(ipnt),'l')
          cindices(i,j,ipnt) = 0
          findices(i,j,ipnt) = 0
        enddo
      enddo
      mbase(ipnt) = 0
      mpol(ipnt) = 0

c     Determine the polarisation and hour angle of the spare record.
      nchan(ipnt) = nspare
      if(min(nchan(1),nchan(2)).le.0)return
      call unpck(pspare,mha,bl,ipol)
      ha(ipnt) = mha

      dowhile(abs(mha-ha(ipnt)).lt.1e-4 .and. nspare.eq.nchan(ipnt))
c       Which output slot does the current record fall into?
        if(cindices(ipol,bl,ipnt).gt.0.or.
     *     findices(ipol,bl,ipnt).gt.0)call bug('f',
     *    'Multiple records for the same baseline within integration')
        call memAlloc(cindices(ipol,bl,ipnt),nchan(ipnt),'c')
        call memAlloc(findices(ipol,bl,ipnt),nchan(ipnt),'l')
        cidx = cindices(ipol,bl,ipnt) - 1
        fidx = findices(ipol,bl,ipnt) - 1
        do i=1,nspare
          Memc(cidx+i) = cspare(i)
          Meml(fidx+i) = lspare(i)
        enddo
        mpol(ipnt) = max(mpol(ipnt),ipol)
        mbase(ipnt) = max(mbase(ipnt),bl)

c       Get another record.
        call uvread(tno,pspare,cspare,lspare,MAXCHAN,nspare)
        if(nspare.gt.0)call unpck(pspare,mha,bl,ipol)
      enddo

      if(abs(mha-ha(ipnt)).lt.1e-4.and.nspare.ne.0)
     *  call bug('f','Number of channels changed within an integration')

      end
