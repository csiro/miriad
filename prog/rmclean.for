c************************************************************************
        program rmclean
c
c= RMCLEAN - Run RM-CLEAN on a set of 'dirty' rm-cubes
c& ghh
c: analysis.
c+
c       RMCLEAN reads in dirty Q and U cubes, generates rmtf based on
c       the frequencies given in an ascii file, and cleans the rm
c       spectra following the algorithm given by Brentjens (2007).
c       The output cubes contain the clean model components and the
c       CLEANed rm spectra.
c       The input cubes must be reordered with mode=312, and the
c       output cubes will have the same ordering and thus must be
c       reordered after being written to disk. A script (runrmclean.py)
c       is provided to put everything together into one step.
c@ inq
c       Cube containing dirty Q spectra. No default.
c@ inu
c       Cube containing dirty U spectra. No default.
c@ freq
c       Ascii file containing the observed frequencies. No default.
c       The file should contain one frequency per line, in Hertz.
c       Comment lines aren't allowed.
c@ nmax
c       Maximum number of iterations per pixel. Default 1000.
c@ gain
c       CLEAN loop gain. Default 0.1.
c@ cutoff
c       CLEAN cutoff (in P, without bias correction). Default 0.
c@ fwhm
c       FWHM of restoring RM beam, in rad/m^2. Default 0==auto.
c@ method
c       Method for determining clean component locations. Choices are:
c       'peakp': Search P spectrum for largest value
c       'xcorr': Perform the cross-correlation of the RMTF and
c                the P spectrum at each iteration, and find the
c                location of the largest value in the amplitude
c                of the cross-correlation spectrum.
c       The cross-correlation option is slower, but should be better,
c       especially in low signal-to-noise situations, and is default.
c@ outq
c       Output (CLEANed) Q cube. Default 'Qclean'.
c@ outu
c       Output (CLEANed) U cube. Default 'Uclean'.
c@ modq
c       Model (CLEANed) Q cube. Default 'Qmodel'.
c@ modu
c       Model (CLEANed) U cube. Default 'Umodel'.
c@ resq
c       Residual Q cube. Default 'Qresid'.
c@ resu
c       Residual U cube. Default 'Uresid'.
c@ ni
c       Number of CLEAN iterations per pixel. Default 'niters'.
c@ debug
c       Debug mode? 0=no, 1=yes. Default 0.
c--
c  History:
c    ghh   28nov07  Initial version [1.0]. Based on matlab version.
c    ghh   04dec07  Version 1.1: added cross-correlation method of
c                                searching for clean components.
c    ghh   16apr08  Version 1.2: added proper in-line 'derotation' of
c                                the polarization vectors
c    ghh   10jul08  Version 1.3: added output of residual cubes
c                                and changed to allow cutoff=0, gain=1
c    ghh   15sep08  Version 1.4: minor changes
c    ghh   22sep08  Version 1.5: fixed RM axis scaling behavior
c    ghh   21apr09  Version 1.6: increased allowed size of frequency file
c                                added debug option and some output
c                                added constant MAXFRQ for array sizes
c                                To do: repeat this for MAXPHI
c    ghh   19apr11  Version 1.7: increased allowed size of phi axis
c                                added constant MAXPHI
c                                created rmclean.h
c------------------------------------------------------------------------
      implicit none
      include 'mirconst.h'
      include 'maxdim.h'
      include 'rmclean.h'
      character version*(*)
      parameter(version='RMCLEAN: version 1.7 19-Apr-11')
      character*64 inq*64,inu*64,freq*64
      character*64 outq*64,outu*64,modq*64,modu*64,ni*64
      character*64 resq*64,resu*64
      character*80 line*80
      character*5 method*5
      character*8 axrm*8
      integer nmax,nsize(3),axnum(2),x,y,i,j,k,debug
      integer loutqc,loutuc,loutqm,loutum,loutni,linq,linu
      integer loutqr,loutur
      integer ffile,iostat,llen,nf,whl,mi
      integer nphi,pphi,naxis,xpix,ypix,nval,blc(3)
      real gain,cutoff,fwhm,fvals(MAXFRQ),phi(MAXPHI),lphi(MAXLPHI)
      real dqs(MAXPHI),dus(MAXPHI),cqs(MAXPHI),cus(MAXPHI)
      real mqs(MAXPHI),mus(MAXPHI),cbeam(MAXPHI),lam02
      real rqs(MAXPHI),rus(MAXPHI)
      real nrow(MAXDIM)
      real sphi,dphi,junk,lf
      complex rmtf(MAXLPHI)
c
c
c  Get the input parameters.
c
      call output(version)
      call keyini
      call keya('inq',inq,' ')
      call keya('inu',inu,' ')
      call keya('freq',freq,' ')
      call keyi('nmax',nmax,1000)
      call keyi('debug',debug,0)
      call keyr('gain',gain,0.1)
      call keyr('cutoff',cutoff,0.0)
      call keyr('fwhm',fwhm,0.0)
      call keya('method',method,'xcorr')
      call keya('outq',outq,'Qclean')
      call keya('outu',outu,'Uclean')
      call keya('modq',modq,'Qmodel')
      call keya('modu',modu,'Umodel')
      call keya('resq',resq,'Qresid')
      call keya('resu',resu,'Uresid')
      call keya('ni',ni,'niters')
      call keyfin
c
c  Check the reasonableness of the inputs.
c
      if(inq.eq.' ') call bug('f','Input Q cube must be given')
      if(inu.eq.' ') call bug('f','Input U cube must be given')
      if(freq.eq.' ') call bug('f','Input frequencies must be given')
      if(nmax.le.0) call bug('f','nmax unreasonable')
      if(gain.le.0.0.or.gain.gt.1.0) call bug('f','gain unreasonable')
      if(cutoff.lt.0.0) call bug('f','cutoff unreasonable')
      if(fwhm.lt.0.0) call bug('f','fwhm unreasonable')
      if(method.eq.'xcorr') then
        mi = 1
      elseif(method.eq.'peakp') then
        mi = 2
      else
        call bug('f','Unknown value given for option METHOD')
      endif
      if (debug.ne.0.and.debug.ne.1) call bug('f','Invalid debug mode')
      if(fwhm.eq.0.0) call bug('w','I will determine FWHM for you')
c
c  Report back the important inputs.
c
      write(line,'(SP,I8)') nmax
      call output('NMAX = '//line)
      write(line,'(SP,F8.5)') gain
      call output('GAIN = '//line)
      write(line,'(SP,F12.9)') cutoff
      call output('CUTOFF = '//line)
      if(fwhm.ne.0.0) then
        write(line,'(SP,F9.2)') fwhm
        call output('FWHM = '//line)
      endif
      if(mi.eq.1) then
        call output('Using cross-correlation clean component method')
      else
        call output('Using peak of P spectrum clean component method')
      endif
c
c  Open the input cubes.
c
      if (debug.eq.1) call output('Opening input Q cube')
      call xyopen(linq,inq,'old',3,nsize)
      if (debug.eq.1) call output('Opening input U cube')
      call xyopen(linu,inu,'old',3,nsize)
c
c  Determine the phi axis specified by the dirty Q cube
c  and make another phi axis twice as long, for mkRMTF
c
      call rdhdi(linq,'naxis',naxis,0)
      if(naxis.lt.3) call bug('f','Input cubes must be 3-d')
      call rdhdi(linq,'naxis2',xpix,0)
      if(nsize(2).ne.xpix) call bug('f','RA axis is wrong size')
      call rdhdi(linq,'naxis3',ypix,0)
      if(nsize(3).ne.ypix) call bug('f','DEC axis is wrong size')
      call rdhdi(linq,'naxis1',nphi,0)
      if(nphi.gt.MAXPHI) call bug('f','PHI axis is too long!')
      if(nsize(1).ne.nphi) call bug('f','PHI axis is wrong size')
      call rdhdi(linq,'crpix1',pphi,0)
      call rdhdr(linq,'crval1',sphi,0.0)
      call rdhdr(linq,'cdelt1',dphi,0.0)
      call rdhda(linq,'ctype1',axrm,'abcd')
      if(axrm.eq.'abcd') call bug('f','Unknown RM axis type')
      if(axrm(1:4).eq.'FREQ') then
        call bug('w','Scaling RM axis values by 1e9')
        dphi = dphi * 1E9
        sphi = sphi * 1E9
      endif
      if(dphi.gt.1E9) call bug('f','Unable to parse RM axis values')
      do i = 1, nphi
        phi(i) = (real(i-pphi)*dphi)+sphi
      enddo
      do i = 1, nphi*2+1
        lphi(i) = (real(i-pphi-nphi/2)*dphi)+sphi
      enddo
c
c  Make the rmtf
c
      if (debug.eq.1) call output('Opening freq file')
      call txtopen(ffile,freq,'old',iostat)
      if(iostat.ne.0) call bug('f','Could not read frequency file')
      nf = 0
      do i = 1, MAXFRQ
        call txtread(ffile,line,llen,iostat)
        if(iostat.eq.0) then
          read(line,'(BN,F20.0)') fvals(i)
          nf = nf + 1
        endif
      enddo
      if(iostat.eq.0) call bug('f','Too many frequencies in file!')
      call txtclose(ffile)
      if (debug.eq.1) call output('Frequency file successfully read')
      if(fwhm.eq.0.0) then
        if (debug.eq.1) call output('Making RMSF, auto fwhm method')
        call mkRMTF(nphi,lphi,nf,fvals,rmtf,fwhm,lam02)
        if (debug.eq.1) call output('Made RMSF')
        write (line,'(SP,F9.2)') fwhm
        call output('Using FWHM = '//line)
        whl = 1
      else
        if (debug.eq.1) call output('Making RMSF, known fwhm method')
        call mkRMTF(nphi,lphi,nf,fvals,rmtf,junk,lam02)
        if (debug.eq.1) call output('Made RMSF')
        whl = 0
      endif
      write(line,'(SP,F12.9)') lam02
      call output('Found lambda_0^2 = '//line//'m^2')
c
c  Create the output cubes.
c
      if (debug.eq.1) call output('Making output cubes')
      call xyopen(loutqc,outq,'new',3,nsize)
      call headcopy(linq,loutqc,0,3,0)
      call xyopen(loutuc,outu,'new',3,nsize)
      call headcopy(linq,loutuc,0,3,0)
      call xyopen(loutqm,modq,'new',3,nsize)
      call headcopy(linq,loutqm,0,3,0)
      call xyopen(loutum,modu,'new',3,nsize)
      call headcopy(linq,loutum,0,3,0)
      call xyopen(loutqr,resq,'new',3,nsize)
      call headcopy(linq,loutqr,0,3,0)
      call xyopen(loutur,resu,'new',3,nsize)
      call headcopy(linq,loutur,0,3,0)
c     The image containing niters is different format (2d)
      do i = 1, 2
        nsize(i) = nsize(i+1)
        axnum(i) = i+1
        blc(i) = 1
      enddo
      blc(3) = 1
      call xyopen(loutni,ni,'new',2,nsize)
      call headcopy(linq,loutni,axnum,2,blc)
      if (debug.eq.1) call output('Made output cubes')
c
c  Write information to the cube histories files.
c
      call hisopen(loutqc,'append')
      call hiswrite(loutqc,'RMCLEAN: Miriad '//version)
      call hisinput(loutqc,'RMCLEAN')
      if(whl.eq.1) then
        write(line,'(SP,F9.2)') fwhm
        call hiswrite(loutqc,'RMCLEAN: Autocalculated FWHM: '//line)
      endif
      call hisclose(loutqc)
      call hisopen(loutuc,'append')
      call hiswrite(loutuc,'RMCLEAN: Miriad '//version)
      call hisinput(loutuc,'RMCLEAN')
      if(whl.eq.1) then
        call hiswrite(loutuc,'RMCLEAN: Autocalculated FWHM: '//line)
      endif
      call hisclose(loutuc)
      call hisopen(loutqm,'append')
      call hiswrite(loutqm,'RMCLEAN: Miriad '//version)
      call hisinput(loutqm,'RMCLEAN')
      if(whl.eq.1) then
        call hiswrite(loutqm,'RMCLEAN: Autocalculated FWHM: '//line)
      endif
      call hisclose(loutqm)
      call hisopen(loutum,'append')
      call hiswrite(loutum,'RMCLEAN: Miriad '//version)
      call hisinput(loutum,'RMCLEAN')
      if(whl.eq.1) then
        call hiswrite(loutum,'RMCLEAN: Autocalculated FWHM: '//line)
      endif
      call hisclose(loutum)
      call hisopen(loutqr,'append')
      call hiswrite(loutqr,'RMCLEAN: Miriad '//version)
      call hisinput(loutqr,'RMCLEAN')
      if(whl.eq.1) then
        call hiswrite(loutqr,'RMCLEAN: Autocalculated FWHM: '//line)
      endif
      call hisclose(loutqr)
      call hisopen(loutur,'append')
      call hiswrite(loutur,'RMCLEAN: Miriad '//version)
      call hisinput(loutur,'RMCLEAN')
      if(whl.eq.1) then
        call hiswrite(loutur,'RMCLEAN: Autocalculated FWHM: '//line)
      endif
      call hisclose(loutur)
      call hisopen(loutni,'append')
      call hiswrite(loutni,'RMCLEAN: Miriad '//version)
      call hisinput(loutni,'RMCLEAN')
      if(whl.eq.1) then
        call hiswrite(loutni,'RMCLEAN: Autocalculated FWHM: '//line)
      endif
      call hisclose(loutni)
c
c  Do the rmclean operation for each spectrum individually
c  and place the resulting spectra in their respective sets.
c  At end, place residual spectrum in the appropriate set.
c
      lf = 0.0
      do y = 1, ypix
c       Change planes for each value of y (dec)
        call xysetpl(linq,1,y)
        call xysetpl(linu,1,y)
        call xysetpl(loutqc,1,y)
        call xysetpl(loutuc,1,y)
        call xysetpl(loutqm,1,y)
        call xysetpl(loutum,1,y)
        call xysetpl(loutqr,1,y)
        call xysetpl(loutur,1,y)
        do x = 1, xpix
c         For each x (ra), pull out a spectrum and clean it
          call xyread(linq,x,dqs)
          call xyread(linu,x,dus)
          call doRMCLEAN(nphi,phi,rmtf,nmax,gain,cutoff,fwhm,mi,lam02,
     +                   dqs,dus,cqs,cus,mqs,mus,rqs,rus,nval)
c         Collect the niters for this row
          nrow(x) = nval
c         Write the cleaned and model spectra to their output files
          call xywrite(loutqm,x,mqs)
          call xywrite(loutum,x,mus)
          call xywrite(loutqc,x,cqs)
          call xywrite(loutuc,x,cus)
          call xywrite(loutqr,x,rqs)
          call xywrite(loutur,x,rus)
        enddo
c       Write a row of niters to the output file
        call xywrite(loutni,y,nrow)
c       Give an occasional status report
        if((real(y)/real(ypix)).eq.0.25+lf) then
          lf = lf+0.25
          write(line,'(SP,I4)') int(lf*100)
          call output('Percent done: '//line)
        endif
      enddo
c
c  Close the files - we're done!
c
      call xyclose(linq)
      call xyclose(linu)
      call xyclose(loutqc)
      call xyclose(loutuc)
      call xyclose(loutqm)
      call xyclose(loutum)
      call xyclose(loutqr)
      call xyclose(loutur)
      call xyclose(loutni)
c
      end
c************************************************************************
      subroutine mkRMTF(numphi,phiv,numf,fv,R,width,lam02)
c
      implicit none
      include 'mirconst.h'
      include 'rmclean.h'
      complex R(MAXLPHI),compI,arg
      real lam2(MAXFRQ),lam02,width,minl2,maxl2,phiv(MAXLPHI),fv(MAXFRQ)
      integer numphi,numf,i,j
c
      lam02 = 0.0
      minl2 = 1000.0
      maxl2 = 0.0
      compI = (0.0,1.0)
c     Produce the minimum, maximum, and weighted mean lambda^2
      do i = 1, numf
        lam2(i) = (CMKS/fv(i))**2
        lam02 = lam02 + lam2(i)
        if(lam2(i).lt.minl2) minl2 = lam2(i)
        if(lam2(i).gt.maxl2) maxl2 = lam2(i)
      enddo
      lam02 = lam02/numf
c     Now make the rmtf
      do i = 1, numphi*2+1
        R(i) = complex(0.0,0.0)
        do j = 1, numf
          R(i) = R(i)+exp(-2.0*compI*phiv(i)*(lam2(j)-lam02))
        enddo
        R(i) = R(i)/real(numf)
      enddo
c     Calculate the theoretical resolution (fwhm) of range(lambda^2)
      width = (2.0*sqrt(3.0))/(maxl2-minl2)
c
      end
c************************************************************************
      subroutine doRMCLEAN(numphi,phiv,R,nm,g,cut,width,mi,lam02,
     +                     dq,du,cq,cu,mq,mu,rq,ru,n)
c
      implicit none
      include 'mirconst.h'
      include 'rmclean.h'
      complex R(MAXLPHI),dp(MAXPHI),cp(MAXPHI),modcomp,sR(MAXLPHI)
      complex mcdr,compI
      real phiv(MAXPHI),g,cut,width,absp(MAXPHI),maxabsp,xc(MAXPHI)
      real dq(MAXPHI),du(MAXPHI),cq(MAXPHI),cu(MAXPHI),mq(MAXPHI)
      real mu(MAXPHI),lam02
      real rq(MAXPHI),ru(MAXPHI)
      integer numphi,nm,n,i,j,maxabspi,mi
c
      n = 0
      compI = (0.0,1.0)
c     Do some initializations
      do i = 1, numphi
        dp(i) = complex(dq(i),du(i))
        absp(i) = abs(dp(i))
        mq(i) = 0.0
        mu(i) = 0.0
        cq(i) = 0.0
        cu(i) = 0.0
      enddo
c     Only iterate up to the maximum number of steps given by user
      do j = 1, nm
c       Depending on the method, search in P or in xcorr(P,RMTF)
        if(mi.eq.1) then
c         We are looking at the cross correlation spectrum
          call xcorr(numphi,dp,R,xc)
          call findmax(numphi,xc,maxabsp,maxabspi)
c         Note, we only want the _location_ of the peak from xcorr...
          maxabsp = absp(maxabspi)
        else
c         We are looking at the P spectrum itself (standard method)
          call findmax(numphi,absp,maxabsp,maxabspi)
        endif
        if(maxabspi.eq.0) call bug('f','Error finding Pmax')
c       If it's below the cutoff, exit the loop
        if(maxabsp.lt.cut) goto 10
        n = n+1
c       The clean component is gain*peak(P)
        modcomp = g*dp(maxabspi)
c       The derotated clean component is modcomp*exp(-2i(phi)(lam02))
        mcdr = modcomp*exp(-2.0*compI*phiv(maxabspi)*lam02)
c       Shift the rmtf to the location of the clean component
        call circshift(numphi,R,sR,maxabspi)
        do i = 1, numphi
c         Subtract out the clean component * shifted rmtf
          dp(i) = dp(i)-modcomp*sR((numphi/2)+i)
          absp(i) = abs(dp(i))
c         Add a piece (pre-derotated) to the cleaned spectrum
          cq(i) = cq(i)+(realpart(mcdr)
     +      *exp(-(phiv(i)-phiv(maxabspi))**2/(2.0*((width/2.355)**2))))
          cu(i) = cu(i)+(imagpart(mcdr)
     +      *exp(-(phiv(i)-phiv(maxabspi))**2/(2.0*((width/2.355)**2))))
          if(i.eq.maxabspi) then
c           Store the clean components for later reference
            mq(i) = mq(i) + realpart(modcomp)
            mu(i) = mu(i) + imagpart(modcomp)
          endif
        enddo
      enddo
c     Finally, add the (derotated) residuals into the clean spectrum
10    do i = 1, numphi
        rq(i) = realpart(dp(i))
        ru(i) = imagpart(dp(i))
        dp(i) = dp(i)*exp(-2.0*compI*phiv(i)*lam02)
        cq(i) = cq(i) + realpart(dp(i))
        cu(i) = cu(i) + imagpart(dp(i))
      enddo
c
      end
c***********************************************************************
      subroutine findmax(n,s,m,i)
c
      implicit none
      include 'rmclean.h'
      integer n,i,j
      real s(1024),m
c
      m = 0.0
      i = 0
      do j = 1, n
        if(s(j).gt.m) then
          m=s(j)
          i=j
        endif
      enddo
c
      end
c***********************************************************************
      subroutine circshift(n,r,sr,s)
c
      implicit none
      include 'rmclean.h'
      integer n,i,ni,s
      complex r(MAXLPHI),sr(MAXLPHI)
c
      do i = 1, n*2+1
        ni = i+(s-1)-(n/2)
        if(ni.lt.1) ni = ni + ((n*2)+1)
        if(ni.gt.((n*2)+1)) ni = ni - ((n*2)+1)
        sr(ni) = r(i)
      enddo
c
      end
c***********************************************************************
      subroutine xcorr(n,a,d,x)
c
c     This subroutine computes the (amplitude of the) cross correlation
c     of two vectors a and d. Note that in order to use the fft routines
c     that are built-in to miriad, the input vectors are zero-padded to
c     make them have a length equal to a power of two.
c     Also, the rmtf, assumed to be 2x as long as P, is truncated.
c
      implicit none
      include 'rmclean.h'
      integer n,i,pt,xp
      complex a(MAXPHI),b(MAXPHI),c(MAXPHI),d(MAXLPHI)
      complex aa(MAXPHI),bb(MAXPHI),cc(MAXPHI)
      real x(MAXPHI)
c
c     First truncate the rmtf
      do i = 1, n
        b(i) = d(i+n/2)
      enddo
c     Figure out which power of two to zero-pad with
      pt = int((log10(real(n))/log10(2.0))+0.5)
      if(mod(n,2**pt).ne.0) then
        xp = (2**pt)-n
        do i = 1, xp
          a(n+i) = complex(0.0,0.0)
          b(n+i) = complex(0.0,0.0)
        enddo
      endif
c     Now do the fourier transforms of the inputs
      call fftcc(a,aa,-1,2**pt)
      call fftcc(b,bb,-1,2**pt)
c     and multiply, conjugating one of the fourier transforms
      do i = 1, 2**pt
        cc(i) = aa(i)*conjg(bb(i))
      enddo
c     and fourier transform back
      call fftcc(cc,c,1,2**pt)
c     We only care about the amplitude, and we must 'fftshift'
      do i = 1, n/2
        x(i) = abs(c((2**pt)/2+i+xp/2))
        x(i+n/2) = abs(c(i))
      enddo
      if(i+n/2.ne.n) x(n) = abs(c(n/2+1))
c
      end
