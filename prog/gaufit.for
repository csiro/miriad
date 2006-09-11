c= gaufit - Fits gaussians to profiles
c& bpw
c: image-analysis
c+
c gaufit fits gaussians to a profile and can write the output to
c a miriad dataset, a logfile or the terminal.
c The current version is not very smart about initial estimates: they
c need to be given manually by the user and need to be reasonably close
c already. It is intented to do this right, eventually.
c Also, it has not been possible to rigorously test everything, and the
c algorithm used is not very robust, so divergent fits and so may occur
c every now and then. But in principle you should get good results.
c
c< in
c
c< region
c  Note: the region=mask option is not implemented. The mask of the
c  input dataset is used however. If there is one, the profile value
c  at masked datapoints is set to zero before doing the fit.
c
c@ model
c Optional output dataset, to which theoretical (described by fit)
c profiles can be written.
c
c@ residual
c Optional output dataset, to whicht the difference between the profile
c and the fit can be written
c
c@ params
c Optional output dataset to which the fit parameters can be written.
c For each fitted component six planes are written, one with the
c amplitude (or integral), one with the position and one with the fwhm
c (or dispersion), and three more with the errors. The planes with
c errors come after the planes with all fit results. A final plane
c which contains the rms of the residual is added.
c Unfortunately, because MIRIAD very-deep-down disallows opening an
c existing dataset for writing, it is not possible to add new fits to
c an existing parameter set. So you have to refit the whole cube if
c one profile is not to your liking! Or do some fiddling with adding
c two cubes. It'll be complex anyway you do it.
c
c@ fitaxis
c This determines along which axis profiles are taken. The default is
c the velocity ('vel') axis. Other possible answers are 'x', 'y', 'z',
c 'a', 'ra', 'dec', 'lon', 'lat', 'freq'.
c
c@ ngauss
c Number of gaussian components to fit (maximum 10, default 1).
c
c@ options
c Controls the output. Possible options are:
c   noprint:      do not print the fit results on the terminal
c   supbad:       suppress results for fits outside ranges given by
c                 cutoff, crange and wrange and results for bad fits.
c   wrprof:       write out a file with the data and the fit so that it
c                 at least is possible to use plotting programs to
c                 compare them; a kludge until gaufit itself can plot.
c   integral:     write out integral of gaussian instead of amplitude
c                 (also interpret input for cutoff and estim keywords
c                 as integral)
c   dispersion:   write out dispersion of gaussian instead of fwhm
c                 (also interpret input for wrange and estim keywords
c                 as dispersion)
c   pixels:       write center and width in pixels, not in units along
c                 axis
c                 (also interpret input for cutoff, vrange, wrange and
c                 estim keywords as pixels)
c   average:      first make an average profile of the selected region
c                 and then fit one single gaussian to this profile
c   summed:       first make a summed profile of the selected region
c                 and then fit one single gaussian to this profile
c   fixvelo:      fix the velocities to the initial estimate during fit
c   fixwidth:     fix the width to the initial estimate while fitting
c                 (fixvelo and fixwidth can be combined)
c
c@ cutoff
c Give a cutoff for the amplitude/integral: fits that give as result an
c absolute value of the amplitude/integral below the given cutoff are
c not written out.
c Default: cut off amplitudes below 1 times the rms.
c
c@ crange
c Give a range (in units along the profile) between which the center
c should lie. Fits that result in centers outside this range are not
c written out.
c Default: cut off centers outside profile range.
c
c@ wrange
c Give 1 or 2 values: a lower limit or a range for the width (fwhm or
c dispersion). Fits giving widths outside this range are not written out.
c Default: cut off widths less than 0.5 pixel and larger than the length
c of the profile.
c
c@ estim
c Initial estimates. Give an estimate for the amplitude, velocity and
c fwhm for each component (if options=integral, pixels or dispersion
c is used, give integral instead of amplitude etc.). This is quite
c critical and should already be reasonably close. The same initial
c estimate will be used for all profiles. (This will change in the
c future).
c
c@ rmsest
c Give a value for the rms of the profile. Used by the fitting
c procedure.
c
c@ log
c If the name of a file is given, the results of the fitting are written 
c to this file instead of to the terminal
c--
c@ continue
c Usually a new model, residual or params dataset is opened. By setting
c continue to yes, one can add to an existing output dataset. Only
c fits in the region specified by the region keyword will be overwritten,
c the rest remains as it was.
c
c***********************************************************************

c  History
c
c     bpw 17oct91 Created (with fitting routine from Stefano Casertano,
c                 who used the ones in Numerical Recipes but adapted and
c                 improved them)
c     bpw 21jan92 Make some adaptations to get rid of Numerical Recipes
c                 code as-is.
c     bpw 26feb92 Corrected accidental / ipv * in exponent because
c                 fitparameter is 1/sigma.
c     bpw 27mar92 Changes to suppress flint warnings; Changed assert into
c                 assertl
c     bpw 23apr92 Add options=summed; sometimes take abs(cdelt)
c     bpw  9jul92 Correct shift if options=pixels and not full region used
c                 Add writing of profile
c     bpw 15dec92 Adapt for changed fndaxnumc
c     bpw  2mar93 Adapt for masking in xyzio
c     bpw 14dec93 Add call logclose

c************************************************************************

c The main program first gets all inputs and then calls the workhorse.
c The inputs are:
c units:       dataset handles
c              1: of input dataset
c              2: of model dataset
c              3: of residual dataset
c              4: of parameter dataset
c prfinf:      profile info:
c              1: number of spectra in cube
c              2: number of channels in spectrum
c              3: axisnumber of fit
c              4: pixelnr of first-read pixel
c ngauss:      number of gaussians to fit
c fitlist:     list of parameters to really fit
c gausspar:    results of fitting
c              el 1=integral, el 2=center, el 3=dispersion
c              el 4,5,6 = errors
c              repeats ngauss times
c              last el (6*ngauss+1) = rms
c optlist:     logicals controlling output and fitting procedure
c              el 1=print?
c              el 2=suppress bad fits
c              el 3=logfile?
c              el 4=write profile
c              el 5=integral or amplitude
c              el 6=dispersion or fwhm
c              el 7=pixels or physical?
c              el 8=average?
c              el 9=sum?
c limlist:     ranges for amp/int,ctr,fwhm/disp

      program gaufit

      character*50 version
      parameter    ( version = 'gaufit: version 1.0 2-mar-93' )

      include      'maxdim.h'

      integer      MAXCMP, MAXPAR
      parameter    ( MAXCMP=10, MAXPAR=MAXCMP*3 )

      integer      NOPTS
      parameter    ( NOPTS=11 )
      integer      NOPRT, SUPBAD, LOGF, WRPROF, INTG, DISP, PIXL
      integer      AVER, SUMMED, FIXVELO, FIXWIDTH
      parameter    ( NOPRT=1, SUPBAD=2, LOGF=3, WRPROF=4 )
      parameter    ( INTG=5, DISP=6, PIXL=7 )
      parameter    ( AVER=8, SUMMED=9, FIXVELO=10, FIXWIDTH=11 )

      integer      units(4)
      integer      prfinf(4)
      integer      ngauss
      integer      fitlist(  MAXPAR )
      real         gausspar( 2*MAXPAR+1 )
      logical      optlist(  NOPTS )
      real         limlist(  9 )

      call output(version)
      call inputs(units,prfinf,ngauss,fitlist,optlist,limlist)
      call work(  units,prfinf,ngauss,fitlist,optlist,limlist,gausspar)
      call finish(units,version,optlist(LOGF))
      call keyfin

      end


c************************************************************************

c Inputs reads all keyword values and transforms them to variables
c usable in work.

      subroutine inputs( units,prfinf,ngauss,fitlist,optlist,limlist )

      integer            units(*)
      integer            prfinf(*)
      integer            ngauss
      integer            fitlist(*)
      logical            optlist(*)
      real               limlist(*)

      character*1024     inp, mdl, res, par
      logical            continue
      include            'maxnax.h'
      integer            naxis
      integer            axlen(MAXNAX), blc(MAXNAX), trc(MAXNAX)
      integer            axnum(MAXNAX)
      integer            nprofs, nchan, fitax
      integer            i
      character*(MAXNAX) axnames
      data               axnames / 'xyzabcd' /

c Initialize keyword routines.n
      call keyini

c Read names of datasets.
      call keyf( 'in',       inp, ' ' )
      call keyf( 'model',    mdl, ' ' )
      call keyf( 'residual', res, ' ' )
      call keyf( 'params',   par, ' ' )
      call keyl( 'continue', continue, .false. )
      call assertl( inp.ne.' ', 'You must specify an input file' )

c Open input dataset and get info about it: selected area, fitax
c number of profiles to do and number of pixels in profile
      call inpopen( inp, units(1), naxis,axlen,blc,trc,
     *              nprofs, nchan, fitax )

c Store some parameters in array prfinf, for easier tranfering
      prfinf(1) = nprofs
      prfinf(2) = nchan
      prfinf(3) = fitax
      prfinf(4) = blc(fitax) - 1

c Get gaussian related input: number, parameters to fit, interpretation
c of parameters; and initialize unit conversions
      call inpgauss( ngauss, fitlist,optlist,limlist, units(1),prfinf )

      call assertl( nchan.gt.3*ngauss,
     *'Fitting more parameters than there are datapoints will not work')

c Make list of axes.
      do i = 1, naxis
         axnum(i)  = i
      enddo
c Open model dataset, if required.
      call setopen( mdl,units(2), continue, naxis,axlen,blc,trc,
     *              axnum,axnames(fitax:fitax),units(1) )

c Open residual dataset, if required.
      call setopen( res,units(3), continue, naxis,axlen,blc,trc,
     *              axnum,axnames(fitax:fitax),units(1) )
      
c Open parameter dataset, but change the axes: the fitax will be
c missing and a parameter axis is added as last axis.
      naxis = naxis - 1
      if( naxis.ge.fitax ) then
         do i = fitax, naxis
            axnum(i) = i+1
            axlen(i) = axlen(i+1)
            blc(i)   = blc(i+1)
            trc(i)   = trc(i+1)
         enddo
      endif
      axnum(naxis+1) = 0
      axlen(naxis+1) = 6*ngauss + 1
      blc(naxis+1)   = 1
      trc(naxis+1)   = 6*ngauss + 1
      call setopen( par,units(4), continue, naxis+1,axlen,blc,trc,
     *              axnum,axnames(naxis+1:naxis+1),units(1) )

      return
      end


c***********************************************************************

c Open input dataset and get information on it.

      subroutine inpopen( name, unit, naxis,axlen,blc,trc,
     *                    nprofs, nchan, fitax )

      character*(*) name
      integer       unit
      integer       naxis
      integer       axlen(*), blc(*), trc(*)
      integer       nprofs, nchan, fitax

      include       'maxnax.h'
      integer       MAXBOXES
      parameter     ( MAXBOXES = 1024 )
      character*1   axis
      character*4   type
      integer       boxes(MAXBOXES)
      integer       viraxl(MAXNAX), vircsz(MAXNAX)

c Open and get dimension of input dataset: naxis.
      naxis = MAXNAX
      call xyzopen( unit, name, 'old', naxis, axlen )

c Get an input region from the user
      call boxinput( 'region', name, boxes, MAXBOXES )
      call boxset(   boxes, naxis, axlen, ' ' )
      call boxinfo(  boxes, naxis, blc, trc )

c Find out which axis is to be fit. First read keyword, then call fndaxnum
c which reads the header and sets the value of fitaxis if not given in keyword.
      call keya( 'fitaxis', type, 'freq' )
      if( type(1:2).eq.'ra'  ) type = 'lon'
      if( type(1:3).eq.'dec' ) type = 'lat'
      if( type(1:3).eq.'vel' ) type = 'freq'
      axis = ' '
      call fndaxnum( unit, type, axis, fitax )
      call assertl( axis.ne.' ',
     *              'Specified fit axis not found in dataset' )

c Set up xyzio routines for input dataset
c and figure out number of profiles to do and their length
      call xyzsetup( unit, axis, blc, trc, viraxl, vircsz )
      nprofs = vircsz(naxis) / vircsz(1)
      nchan  = viraxl(1)

      return
      end


c***********************************************************************

c Get parameters controlling type of fit made and interpretation of
c parameters for the user.

      subroutine inpgauss(ngauss,fitlist,optlist,limlist,uniti,prfinf)

      integer      ngauss, fitlist(*)
      logical      optlist(*)
      real         limlist(*)
      integer      uniti, prfinf(*)

      integer      NOPTS
      parameter    ( NOPTS=11 )
      integer      NOPRT, SUPBAD, LOGF, WRPROF, INTG, DISP, PIXL
      integer      AVER, SUMMED, FIXVELO, FIXWIDTH
      parameter    ( NOPRT=1, SUPBAD=2, LOGF=3, WRPROF=4 )
      parameter    ( INTG=5, DISP=6, PIXL=7 )
      parameter    ( AVER=8, SUMMED=9, FIXVELO=10, FIXWIDTH=11 )

      character*64 logfile
      integer      i, j
      character*6  key
      logical      keyprsnt
      character*10 opts(NOPTS)
      logical      optprsnt(NOPTS)
      data         opts / 'noprint', 'supbad', 'LLOOGGFF', 'wrprof',
     *                    'integral', 'dispersion', 'pixels',
     *                    'average', 'summed', 'fixvelo', 'fixwidth' /
      data         optprsnt / NOPTS*.FALSE. /


c Set up conversion pixels<->coordinates
      call setconv( uniti, prfinf )
 
c Get number of gaussian components.
      call keyi( 'ngauss', ngauss, 1 )

c Get options list.
      call options( 'options', opts, optprsnt, NOPTS )

c Tranfer some options to gssfit and gssout
      optlist(NOPRT)  = optprsnt(NOPRT)
      optlist(SUPBAD) = optprsnt(SUPBAD)
      optlist(WRPROF) = optprsnt(WRPROF)
      optlist(INTG)   = optprsnt(INTG)
      optlist(DISP)   = optprsnt(DISP)
      optlist(PIXL)   = optprsnt(PIXL)
      optlist(AVER)   = optprsnt(AVER)
      optlist(SUMMED) = optprsnt(SUMMED)

c Write to a log file?
      call keyf( 'log', logfile, ' ' )
      optlist(LOGF) = logfile.ne.' '
      if( optlist(LOGF) ) call logopen( logfile, ' ' )

c Fill fitlist array with parameters to fit
      if( optprsnt(FIXVELO) .or. optprsnt(FIXWIDTH) ) then
         do i = 1, ngauss
            j = 3*(i-1)
            fitlist(2*i-1) = j+1
            if( optprsnt(FIXVELO)  ) fitlist(2*i) = j+3
            if( optprsnt(FIXWIDTH) ) fitlist(2*i) = j+2
         enddo
         do i = 2*ngauss+1, 3*ngauss
            fitlist(i) = 0
         enddo
      endif
      if( optprsnt(FIXVELO) .and. optprsnt(FIXWIDTH) ) then
         do i = 1, ngauss
            fitlist(i) = 3*(i-1) + 1
         enddo
         do i = ngauss+1, 3*ngauss
            fitlist(i) = 0
         enddo
      endif
      if( .not.optprsnt(FIXVELO) .and. .not.optprsnt(FIXWIDTH) ) then
         do i = 1, 3*ngauss
            fitlist(i) = i
         enddo
      endif

c Get allowed parameter ranges
c limlist(1:3)=lower limits
c limlist(4:6)=upper limits
c limlist(7:9)=flag if limit used
      do i = 1, 3
         if( i.eq.1 ) key = 'cutoff'
         if( i.eq.2 ) key = 'crange'
         if( i.eq.3 ) key = 'wrange'
         limlist(i)   = 1.
         limlist(i+3) = 1.
         if( keyprsnt(key) ) then
            limlist(i+6) = 1.
            call keyr( key, limlist(i),   limlist(i) )
            call keyr( key, limlist(i+3), limlist(i) )
         else
            limlist(i+6) = 0.
         endif
         call assertl( limlist(i+3).ge.limlist(i),
     *                 'Upper cutoff must be above lower cutoff' )
      enddo
c Convert allowed ranges to pixels
      call parconv( limlist, 2, .true., optlist )
      do i = 1, 2
         limlist(3*i) = 1. / limlist(3*i)
      enddo


      return
      end


c***********************************************************************

c Open a dataset. Can be either an old or a new one. If old the number
c of axes must be consistent. Also require that the size is the same
c as the size of the input dataset.

      subroutine setopen( name,unit, continue, naxis,axlen,blc,trc,
     *                    axnum,axis,uniti )

      character*(*) name
      integer       unit
      logical       continue
      integer       naxis
      integer       axlen(*), blc(*), trc(*)
      integer       axnum(*)
      character*1   axis
      integer       uniti

      include       'maxnax.h'
      integer       naxis1
      integer       viraxl(MAXNAX), vircsz(MAXNAX)

c Opening not needed: return
      if( name.eq.' ' ) then
         unit = 0
         return
      endif


      if( .not.continue ) then
c New: just create it, with size of input set, and copy header.
         call xyzopen(  unit, name, 'new', naxis, axlen )
         call headcopy( uniti, unit, 0, naxis, 0,0 )
      else
c Old: open it. And check axes
         naxis1 = MAXNAX
         call xyzopen( unit, name, 'old', naxis1, viraxl )
         call assertl( naxis.eq.naxis1,
     *        'Number of axes of old dataset inconsistent with input' )
         do naxis1 = 1, naxis
            call assertl( viraxl(naxis1).eq.axlen(naxis1),
     *           'Axis length of old dataset inconsistent with input' )
         enddo
      endif
      
      call xyzsetup( unit, axis, blc, trc, viraxl, vircsz )
      return
      end


c************************************************************************
c************************************************************************
c************************************************************************
c************************************************************************
c************************************************************************

c work loops over all profiles, reads each profile, applies the
c gaussfitting algorithm, and writes the results.

      subroutine work( units, prfinf, ngauss,
     *                 fitlist, optlist, limlist, gausspar )

      integer   units(*)
      integer   prfinf(*)
      integer   ngauss
      integer   fitlist(*)
      logical   optlist(*)
      real      limlist(*)
      real      gausspar(*)

      include   'maxdim.h'
      integer   MAXCMP, MAXPAR
      parameter ( MAXCMP=10, MAXPAR=MAXCMP*3 )

      integer   nchan
      integer   profnr
      logical   makefit
      integer   ier, gssfit
      real      data(MAXDIM), model(MAXDIM), residual(MAXDIM)
      logical   mask(MAXDIM)
      logical   pmsk(2*MAXPAR+1)
      nchan = prfinf(2)

      do profnr = 1, prfinf(1)
         call xyzprfrd( units(1), profnr, data, mask, nchan )
         if( makefit( data,mask, profnr,prfinf, ngauss,optlist ) )
     *   then
           call gssest( data, mask,
     *                  units(1), nchan, ngauss, gausspar )
           ier= gssfit( data, mask,
     *                  prfinf, ngauss, fitlist, optlist, limlist,
     *                  gausspar, model, residual )
           call gssout( data, model,
     *                  units(1), profnr, prfinf, ngauss, optlist,
     *                  gausspar, pmsk, ier )
         else
           call zeroes( gausspar, pmsk, model, residual, ngauss, nchan )
         endif
         if( units(2).ne.0 )
     *   call xyzprfwr( units(2), profnr, model,    mask, nchan )
         if( units(3).ne.0 )
     *   call xyzprfwr( units(3), profnr, residual, mask, nchan )
         if( units(4).ne.0 )
     *   call xyzprfwr( units(4), profnr, gausspar, pmsk, 6*ngauss+1 )
      enddo

      return
      end



      logical function makefit(data,mask,profnr,prfinf,ngauss,optlist)

      real         data(*)
      logical      mask(*)
      integer      profnr
      integer      prfinf(*)
      integer      ngauss
      logical      optlist(*)

      include      'maxdim.h'
      integer      NOPRT, SUPBAD, LOGF, WRPROF, INTG, DISP, PIXL
      integer      AVER, SUMMED, FIXVELO, FIXWIDTH
      parameter    ( NOPRT=1, SUPBAD=2, LOGF=3, WRPROF=4 )
      parameter    ( INTG=5, DISP=6, PIXL=7 )
      parameter    ( AVER=8, SUMMED=9, FIXVELO=10, FIXWIDTH=11 )

      integer      nchan, nprofs
      real         acc(MAXDIM)
      save         acc
      integer      i
      integer      nok
      logical      makfit, canfit

      nprofs = prfinf(1)
      nchan  = prfinf(2)
c If average: average all profiles, and fit only last one, else do nothing.
c If summed:  sum all profiles, and fit only last one, else do nothing.
      if( optlist(AVER) .or. optlist(SUMMED) ) then
         if( profnr.eq.1 ) then
            do i = 1, nchan
               acc(i) = 0.
            enddo
         endif
         do i = 1, nchan
            if(mask(i)) acc(i) = acc(i) + data(i)
         enddo
         if( profnr.eq.nprofs ) then
            do i = 1, nchan
               if( optlist(AVER)   ) data(i) = acc(i) / nprofs
               if( optlist(SUMMED) ) data(i) = acc(i)
            enddo
            makfit = .true.
         else
            makfit = .false.
         endif
      else
         makfit = .true.
      endif

      nok = 0
      do i = 1, nchan
         if( mask(i) .and. (data(i).ne.data(1)) ) nok = nok + 1
      enddo
      canfit = nok .gt. 3*ngauss
      if( (optlist(AVER).or.optlist(SUMMED)) .and. makfit )
     *   call assertl( canfit,
     *   'The profile is a constant, no fit can be done' )
      makefit = makfit .and. canfit

      return
      end


c***********************************************************************


c Zero output arrays
      subroutine zeroes( gausspar, pmsk,model,residual,ngauss,nchan )
      real    gausspar(*), model(*), residual(*)
      logical pmsk(*)
      integer ngauss, nchan
      integer i
      do i = 1, 2*ngauss*3+1
         gausspar(i) = 0.
         pmsk(i) = .false.
      enddo
      do i = 1, nchan
         model(i)    = 0.
         residual(i) = 0.
      enddo
      return
      end


c***********************************************************************


c Finish up
      subroutine finish( units, version, logcls )

      integer       units(4)
      character*(*) version
      logical       logcls

      integer       i
      character*80  line

      call xyzclose( units(1) )
      do i = 2, 4
         if( units(i).ne.0 ) then
            call hisopen(  units(i), 'append'  )
            line = 'GAUFIT: ' // version
            call hiswrite( units(i), line )
            call hisinput( units(i), 'gaufit'  )
            call hisclose( units(i) )
            call xyzclose( units(i) )
         endif
      enddo
      if(logcls) call logclose

      return
      end


c************************************************************************


      subroutine gssest( data, mask, unit, nchan, ngauss, gausspar )

      real      data(*)
      logical   mask(*)
      integer   unit
      integer   nchan
      integer   ngauss
      real      gausspar(*)

      integer   i, j
      integer   MAXCMP, MAXPAR
      parameter ( MAXCMP=10, MAXPAR=MAXCMP*3 )
      real      gaussest(MAXPAR)
      logical   first, keyprsnt
      save      first, gaussest
      data      first / .true. /

      if( first ) then
         call assertl( keyprsnt('estim'),
     *        'It is necessary to use the estim keyword' )
         call assertl( keyprsnt('rmsest'),
     *        'It is necessary to use the rmsest keyword' )
         first = .false.
         do i = 1, MAXPAR
            gaussest(i) = 0.
         enddo
         do i = 1, ngauss
            j = 3*(i-1)
            call keyr( 'estim', gaussest(j+1), 1. )
            call keyr( 'estim', gaussest(j+2), 1. )
            call keyr( 'estim', gaussest(j+3), 1. )
         enddo
         call keyr( 'rmsest', gaussest(2*ngauss*3+1), 1. )
      endif
      do i = 1, MAXPAR
         gausspar(i) = gaussest(i)
      enddo

      return
      end


c***********************************************************************

c Subroutine to control the fitting: converts physical coordinates to
c pixels and back. Sets bad fit results to zero. Finds the rms.
      integer function gssfit(
     *                 data, mask,
     *                 prfinf, ngauss, fitlist, optlist, limlist,
     *                 gausspar, model, residual )

      real      data(*)
      logical   mask(*)
      integer   prfinf(*)
      integer   ngauss
      integer   fitlist(*)
      logical   optlist(*)
      real      limlist(*)
      real      gausspar(*)
      real      model(*), residual(*)
    
      include   'maxdim.h'
      integer   MAXCMP, MAXPAR
      parameter ( MAXCMP=10, MAXPAR=MAXCMP*3 )
      integer   MAXITER
      parameter ( MAXITER=30 )
      real      FTOL
      parameter ( FTOL=0.5 )

      integer   ier
      integer   nchan
      integer   i, j, k, npar, nfit
      real      x(MAXCHAN), sig(MAXCHAN), rms
      real      chisq
      external  gaussfun
      logical   first
      save      first, npar, x, sig, nfit
      data      first / .true. /

      nchan = prfinf(2)
c Set masked datapoints to zero
      do i = 1, nchan
         if(.not.mask(i)) data(i) = 0.
      enddo

c Do an initialization on the first pass (init of x and sig is superfluous
c on later passes; calc of nfit fails on later passes, because fitlist is
c changed deep down)
      if( first ) then
c Number of parameters to fit
        npar = 3*ngauss
c Fill x and sig arrays, as drvmrq requires them
        do i = 1, nchan
           x(i)   = float(i)
           sig(i) = gausspar(2*npar+1)
        enddo
c Number of parameters to really fit
        nfit = 0
        do i = 1, npar
          if( fitlist(i).ne.0 ) nfit = nfit + 1
        enddo
        first = .false.
      endif


c Convert from physical to pixel coordinates
      call parconv( gausspar, ngauss, .true., optlist )

c Fit
      call drvmrq( x, data, sig, nchan,
     *             gausspar,npar, fitlist,nfit, FTOL, MAXITER,
     *             chisq, ier, gaussfun )
c Take absolute value, because fitting procedure only cares about sigma^2
      do i = 1, ngauss
         gausspar(3*i) = abs(gausspar(3*i))
      enddo

c Check if fit OK, if so form gaussian model spectrum and residual,
c calculate rms of residual
      if( ier.eq.0 ) then
         rms = 0.0
         do i = 1, nchan
            model(i) = 0.
            do k = 1, ngauss
               j = 3*(k-1)
               model(i) = model(i) + gausspar(j+1) *
     *         exp( -0.5*( (x(i)-gausspar(j+2))*gausspar(j+3) )**2 )
            enddo
            residual(i) = data(i) - model(i)
            rms         = rms + residual(i)*residual(i) 
         enddo
         call cutoffs( gausspar, ngauss, limlist, nchan, ier )
      endif

c Convert back to physical units.
      if( ier.le.0 ) then
         gausspar(2*npar+1) = sqrt( rms / (nchan-npar) )
         call parconv( gausspar, ngauss, .false., optlist )
      else
         do i = 1, 2*npar+1
            gausspar(i) = 0.
         enddo
      endif

      gssfit = ier
      return
      end


c***********************************************************************


c If topix is .true.: convert
c   from   integral or amplitude, center, dispersion or fwhm
c   to     amplitude, center in pixels, 1/dispersion in pixels
c If topix is .false.: convert
c   from   amplitude, center in pixels, 1/dispersion in pixels
c   to     integral or amplitude, center, dispersion or fwhm

      subroutine parconv( gausspar, ngauss, topix, optlist )

      real         gausspar(*)
      integer      ngauss
      logical      topix
      logical      optlist(*)

      integer      NOPRT, SUPBAD, LOGF, WRPROF, INTG, DISP, PIXL
      integer      AVER, SUMMED, FIXVELO, FIXWIDTH
      parameter    ( NOPRT=1, SUPBAD=2, LOGF=3, WRPROF=4 )
      parameter    ( INTG=5, DISP=6, PIXL=7 )
      parameter    ( AVER=8, SUMMED=9, FIXVELO=10, FIXWIDTH=11 )

      include          'mirconst.h'
      integer          i, j, np
      real             sqrt8ln2, scale
      character*8      ctype
      double precision crval, cdelt
      integer          crpix
      integer          xoff
      save             crval, cdelt, crpix, xoff

      real             coord
      logical          pixels
      integer          unit, prfinf(*)
      character*8      keyw

      sqrt8ln2 = sqrt( 8. * alog(2.) )
      np = 3*ngauss
      do i = 1, ngauss
         j = 3*(i-1)
         if( topix ) then
c  ---      Input FWHM -> convert to dispersion
            if( .not.optlist(DISP) ) then
               gausspar(j+3) = gausspar(j+3) / sqrt8ln2
            endif
c           Input integral -> convert to amplitude
            if( optlist(INTG) ) then
               scale = sqrt(twopi)*gausspar(j+3)
               gausspar(j+1) = gausspar(j+1) / scale
            endif
c  ---      Convert units of dispersion and central velocity to pixels
            if( .not.optlist(PIXL) ) then
               gausspar(j+2) = (gausspar(j+2)-crval)/cdelt-xoff+crpix
               gausspar(j+3) = gausspar(j+3) / abs(cdelt)
            else
               gausspar(j+2) = gausspar(j+2) - xoff
            endif
c  ---      Convert to inverse dispersion
            gausspar(j+3) = 1. / gausspar(j+3)
         else
c  ---      Convert inverse to dispersion
            gausspar(j+3)    = 1. / gausspar(j+3)
            gausspar(j+3+np) = gausspar(j+3+np) * gausspar(j+3)**2
c  ---      Convert units of dispersion and velocity to physical
            if( .not.optlist(PIXL) ) then
               gausspar(j+3)    = gausspar(j+3)    * abs(cdelt)
               gausspar(j+3+np) = gausspar(j+3+np) * abs(cdelt)
               gausspar(j+2)    = crval+(gausspar(j+2)+xoff-crpix)*cdelt
               gausspar(j+2+np) = gausspar(j+2+np) * abs(cdelt)
            else
               gausspar(j+2)    = gausspar(j+2) + xoff
            endif
c  ---      Output Integral -> convert to amplitude
            if( optlist(INTG) ) then
               scale = sqrt(twopi)*gausspar(j+3)
               gausspar(j+1)    = gausspar(j+1)    * scale
               gausspar(j+1+np) = gausspar(j+1+np) * scale
            endif
c           Output FWHM -> convert dispersion
            if( .not.optlist(DISP) ) then
               gausspar(j+3)    = gausspar(j+3)    * sqrt8ln2
               gausspar(j+3+np) = gausspar(j+3+np) * sqrt8ln2
            endif
         endif
      enddo
      return

c convert pixel to physical coordinate
      entry cooconv( coord, pixels )
      if( .not.pixels ) then
         coord = crval + (coord+xoff-crpix)*cdelt
      else
         coord = coord + xoff
      endif
      return

      entry setconv( unit, prfinf )

      call rdhdd( unit, keyw('crval',prfinf(3)), crval, 0.d0 )
      call rdhdi( unit, keyw('crpix',prfinf(3)), crpix, 0    )
      call rdhdd( unit, keyw('cdelt',prfinf(3)), cdelt, 0.d0 )
      call rdhda( unit, keyw('ctype',prfinf(3)), ctype, ' '  )
c if options=arcsec, arcmin, deg
      if( ctype(1:2).eq.'RA' .or. ctype(1:3).eq.'DEC' ) then
         crval = crval * 3600.d0 * 180.d0/pi
         cdelt = cdelt * 3600.d0 * 180.d0/pi
      endif
      xoff = prfinf(4)

      return
      end


      character*8 function keyw( key, i )
      character*(*) key
      integer     i
      integer     len1
      character*1 itoaf
      keyw = key( :len1(key) ) // itoaf(i)
      return
      end


c***********************************************************************

c Apply cutoffs
      subroutine cutoffs( gausspar, ngauss, limlist, nchan, ier )

      real    gausspar(*)
      integer ngauss
      real    limlist(*)
      integer nchan
      integer ier
      
      real    lower, upper
      integer i

c Velocity should be inside range; default: within profile.
      lower = limlist(2)
      upper = limlist(5)
      if( limlist(8).eq.0. ) lower = 1.
      if( limlist(8).eq.0. ) upper = float(nchan)
      do i = 2, 3*ngauss, 3
         if( gausspar(i) .lt. lower  .or.
     *       gausspar(i) .gt. upper ) ier = -5
      enddo

c Width should be inside range; default: more than 0.5 pixel and
c less than length dispersion.
      lower = limlist(3)
      upper = limlist(6)
      if( limlist(9).eq.0. ) lower = 0.5
      if( limlist(9).eq.0. ) upper = float(nchan)
      if( limlist(9).eq.1. .and. lower.eq.upper ) upper = float(nchan)
      do i = 3, 3*ngauss, 3
         if( 1./gausspar(i) .lt. lower ) ier = -6
         if( 1./gausspar(i) .gt. upper ) ier = -7
      enddo

c Amplitude should be high enough; default: more than 1 sigma.
      lower = limlist(1)
      if( limlist(7).eq.0. ) lower = gausspar(2*ngauss*3+1)
      do i = 1, 3*ngauss, 3
         if( abs(gausspar(i)) .lt. lower ) ier = -8
      enddo


      return
      end


c***********************************************************************


      subroutine gssout( data, model,
     *                   unit, profnr, prfinf, ngauss, optlist,
     *                   gausspar, pmsk, ier )

      real         data(*)
      real         model(*)
      integer      unit
      integer      profnr, prfinf(*)
      integer      ngauss
      logical      optlist(*)
      real         gausspar(*)
      logical      pmsk(*)
      integer      ier

      integer      NOPRT, SUPBAD, LOGF, WRPROF, INTG, DISP, PIXL
      integer      AVER, SUMMED, FIXVELO, FIXWIDTH
      parameter    ( NOPRT=1, SUPBAD=2, LOGF=3, WRPROF=4 )
      parameter    ( INTG=5, DISP=6, PIXL=7 )
      parameter    ( AVER=8, SUMMED=9, FIXVELO=10, FIXWIDTH=11 )

      integer      i, j, k, n1, n2, npar
      include      'maxnax.h'
      integer      coords(MAXNAX)
      character*40 fmt
      character*99 line
      integer      len1
      logical      first
      integer      crpix1, crpix2
      save         first, crpix1, crpix2
      data         first / .true. /

      if( optlist(NOPRT)                 ) return
      if( optlist(SUPBAD) .and. ier.ne.0 ) return

      if( first ) call header( unit,prfinf(3),optlist,crpix1,crpix2 )
      first = .false.

      npar = 3*ngauss
      do i = 1, ngauss

         j    = 3*(i-1)
         line = ' '

         n1 = 1
         n2 = 2*4
         if( i.eq.1 ) then
            call xyzs2c( unit, profnr, coords )
            coords(1) = coords(1) - crpix1
            coords(2) = coords(2) - crpix2
            if( optlist(AVER)   ) line(1:8) = 'average '
            if( optlist(SUMMED) ) line(1:8) = 'summed  '
            if( .not.(optlist(AVER).or.optlist(SUMMED)) )
     *      write( line(n1:n2), '(i4,i4)' ) coords(1), coords(2)

            if ( optlist(WRPROF) )
     *      call writprof( data,model,prfinf(2),coords,optlist(PIXL) )
         endif

         if( ier.le.0 ) then

            n1 = n2 + 1
            n2 = n1 + 1+1+2
            write( line(n1:n2), '( 1x, i1, 2x )' ) i
            
            n1 = n2 + 1
            n2 = n2 + 3*(11+9)
            fmt = '( 3(f10.4,1x), 3(f8.4,1x) )'
            do k = 1, 2*npar
               if( gausspar(k).ge.1.e6 ) fmt =
     *            '( 3(1pe10.4,1x), 3(1pe8.4,1x) )'
            enddo
            write( line(n1:n2), fmt )
     *         gausspar(j+1),     gausspar(j+2),     gausspar(j+3), 
     *         gausspar(j+1+npar),gausspar(j+2+npar),gausspar(j+3+npar)

            n1 = n2 + 1
            n2 = n2 + 6
            if(i.eq.1) write(line(n1:n2),'(f6.2)')gausspar(2*npar+1)

            call wrtout( line, optlist(LOGF) )

         else

            n1 = n2 + 4
            line(n1:) = 'Fit failed;'
            call errout( line, len1(line)+2, ier )
            call wrtout( line, optlist(LOGF) )

         endif

      enddo

      if( ier.lt.0 ) then
         n1 = 12
         line = ' '
         line(n1:) = 'Bad fit;'
         call errout( line, len1(line)+2, abs(ier) )
         call wrtout( line, optlist(LOGF) )
         do j = 1, 2*npar
            gausspar(j) = 0.
         enddo
      endif

      do i = 1, 2*npar+1
        pmsk(i) = ier.eq.0
      enddo

      end


c***********************************************************************


      subroutine header( unit, fitax, optlist, crpix1, crpix2 )

      integer      unit, fitax
      logical      optlist(*)
      integer      crpix1, crpix2

      integer      NOPRT, SUPBAD, LOGF, WRPROF, INTG, DISP, PIXL
      integer      AVER, SUMMED, FIXVELO, FIXWIDTH
      parameter    ( NOPRT=1, SUPBAD=2, LOGF=3,  WRPROF=4 )
      parameter    ( INTG=5, DISP=6, PIXL=7 )
      parameter    ( AVER=8, SUMMED=9, FIXVELO=10, FIXWIDTH=11 )

      character*99 line1, line2
      integer      len1

      integer      LABLEN
      parameter    ( LABLEN = 11 )
      integer      crpix3, n
      character*8  keyw, key
      character*(LABLEN) dataunit, profunit, lab

      call rdhdi( unit, keyw('crpix',1), crpix1, 0 )
      call rdhdi( unit, keyw('crpix',2), crpix2, 0 )
      call rdhdi( unit, keyw('crpix',3), crpix3, 0 )
      if( fitax.eq.1 ) crpix1 = crpix2
      if( fitax.eq.1 ) crpix2 = crpix3
      if( fitax.eq.2 ) crpix2 = crpix3

      call rdhda( unit, keyw('ctype',fitax), key, ' ' )
      if( optlist(PIXL) ) then
        profunit = 'pixels'
      else
         if( key(1:2).eq.'RA'   ) profunit =  'arcsec'
         if( key(1:3).eq.'DEC'  ) profunit =  'arcsec'
         if( key(1:4).eq.'VELO' ) profunit =  'km/s'
         if( key(1:4).eq.'FREQ' ) profunit =  'GHz'
      endif

      call rdhda( unit, 'bunit', key, ' ' )
      dataunit = key
      if( optlist(INTG) ) dataunit = key(:len1(key)) // ' ' // profunit

      line1 = '   x   y # |'
      line2 = ' '
      n = len1(line1)
      if(      optlist(INTG) ) lab = 'integral'
      if( .not.optlist(INTG) ) lab = 'amplitude'
      line1( n  +  LABLEN-len1(lab)+1        : ) = lab
      line2( n  +  LABLEN-len1(dataunit)+1   : ) = dataunit
      n = n + LABLEN
      lab = 'position'
      line1( n  +  LABLEN-len1(lab)+1        : ) = lab
      line2( n  +  LABLEN-len1(profunit)+1   : ) = profunit
      n = n + LABLEN
      if(      optlist(DISP) ) lab = 'dispersion'
      if( .not.optlist(DISP) ) lab = 'fwhm'
      line1( n  +  LABLEN-len1(lab)+1        : ) = lab
      line2( n  +  LABLEN-len1(profunit)+1   : ) = profunit
      n = n + LABLEN
      line1(n+1:) = '|          errors           | rms'
      call wrtout( line1, optlist(LOGF) )
      call wrtout( line2, optlist(LOGF) )
         
      return
      end

     
c************************************************************************

      subroutine writprof( data, model, nchan, coords, pixels )

      real          data(*)
      real          model(*)
      integer       nchan
      integer       coords(*)
      logical       pixels

      integer       nf(2), nfigi, len1
      character*80  file, rtfmt
      character*80  line
      integer       unit, iostat
      integer       i
      real          x
      
      nf(1) = nfigi( coords(1) )
      nf(2) = nfigi( coords(2) )
      write( file, rtfmt( '''profile_at_'',i<>,''_'',i<>',nf,2 ) )
     *       coords(1), coords(2)
      call txtopen( unit, file, 'new', iostat )
      if ( iostat.ne.0 ) then
         call bug( 'w', 'error opening file to write profile on' )
      else
         do i = 1, nchan
            x = i
            call cooconv( x, pixels )
            write( line, '( f10.4, 1x, 1pg10.4, 1x, 1pg10.4 )' )
     *      x, data(i), model(i)
            call txtwrite( unit, line, len1(line), iostat )
          enddo
       endif
       call txtclose( unit )

       return
       end

c************************************************************************


      subroutine wrtout( line, logf )
      character*(*) line
      logical       logf
      if( .not.logf ) call output(  line )
      if(      logf ) call logwrit( line )
      return
      end

c************************************************************************

      subroutine errout( line, n1, ier )
      character*(*) line
      integer       n1, ier
      character*80  messages(8)
      save          messages
      data          messages /
     *              'DRVMRQ: No convergence after .. iterations',
     *              'SVDCMP: no convergence after 30 iterations',
     *              'DRVMRQ: too many major iterations',
     *              'Gaussian is bad parametrization of profile',
     *              'Center outside range',
     *              'Width insignificant',
     *              'Width larger than profile length',
     *              'Amplitude less than rms'
     *              /
      if( ier.gt.100 ) then
        write( messages(1)(30:31), '( i2 )' ) ier-100
        ier = 1
      endif
      line(n1:) = messages(ier)
      return
      end


c************************************************************************


      subroutine gaussfun( x, pars, y, dydpar, npars, compgrad )
      integer npars
      real    x, pars(npars)
      real    y, dydpar(npars)
      logical compgrad
      integer i
      real    arg, expon, fac
      y = 0.
      do i = 1, npars-1, 3
        arg   =  (x-pars(i+1)) * pars(i+2)
        expon = exp( -0.5*arg**2 )
        y     = y + pars(i)*expon
        if( compgrad ) then
           fac         = pars(i) * expon * arg
           dydpar(i)   = expon
           dydpar(i+1) =  fac * pars(i+2)
           dydpar(i+2) = -fac * (x-pars(i+1))
        endif
      enddo
      return
      end


c------------------------------------------------------------------
c23456789012345678901234567890123456789012345678901234567890123456789012
c
c       DRVMRQ is a driver for MRQMIN that sets up a few needed vectors
c       and calls MRQMIN as needed. MRQMIN and the subroutines it calls
c       are based on the code presented in Numerical Recipes. Some
c       changes made by Stefano Casertano have been included. These
c       changes enhance the efficiency and, especially for DRVMRQ make
c       it more compatible with MIRIAD fitting than would otherwise be
c       the case. Also, instead of Gauss-Jordan, Singular Value Decomposition
c       is used, courtesy SC too.
c
c       Calling arguments:
c
c       X       Vector containing the independent variable (Input)
c       Y       Vector containing the function values (Input)
c       SIG     Vector containing the error values (Input)
c       NDATA   Number of data points
c       XPAR    Vector containing parameters (Input/Output)
c               Input: the starting point
c               Output: the best fit
c       NPAR    The length of XPAR (Input)
c       FITLIST Vector indicating which parameters must really be fit
c               (Input)
c       NTOFIT  The length of FITLIST (Input)
c       FTOL    Tolerance criterion (how small a CHI SQUARED reduction
c               is considered insignificant) (Input)
c               Recommended value 0.5
c       MAXITER Number of major iterations to allow
c       FRET    Value of CHI SQUARED at minimum (Output)
c       IER     Return value indicating if error occurred (Output)
c
c       DRVMRQ drives a modified version of MRQMIN which allows for
c       major and minor iterations.  
c
c       Each call to MRQMIN (except for setup and error calculation) 
c       is a major iteration in which several minor iterations may be taken.
c       A major iteration starts with a trial value of the parameters and
c       with a value of the control parameter ALAMDA (see below). Several
c       steps are taken, the length of which is controlled by ALAMDA, and 
c       ALAMDA is progressively reduced until an improvement is found.
c       This completes the major iteration. At the end of the major 
c       iteration, control is returned to the driver which decides on further
c       action.
c
c       If the CHI SQUARED is reduced sufficiently, a new major iteration is
c       taken. If the CHI SQUARED reduction is less than FTOL, the 
c       improvement is considered insignificant. After 3 insignificant 
c       improvements, the program considers the fit to be achieved and returns
c       the last set of parameters. If MRQMIN is unable to find an 
c       improved step, DRVMRQ will also return with an error message.
c
c       The parameter ALAMDA controls whether a quadratic or a linear step is
c       taken. For small ALAMDA the step is quadratic (good near the minimum).
c       As ALAMDA is increased the step becomes more linear (gradient-type)
c       and its length decreases. For sufficiently large ALAMDA the step
c       should always succeed in obtaining a decrease of CHI SQUARED, unless
c       the function is at a minimum.
c
c       In the Numerical Recipes implementation, ALAMDA is decreases
c       slightly but not reset to a small value after a successful major
c       iteration. This could be responsible for lower performance, since
c       as you approach the minimum a quadratic step may work better.
c       It might be worthwhile to try a larger decrease in ALAMDA after a 
c       successful major iteration.
c
c       There is also the possibility to leave some of the parameters fixed.
c       This is achieved by renumbering the parameters via a permutation
c       vector FITLIST (1,NPARS) such that the first NTOFIT parameters
c       are to be fitted, the remaining NPARS-NTOFIT are left unchanged.

       subroutine DRVMRQ( x,y, sig, ndata, xpar,npars, fitlist,ntofit,
     *                    FTOL, MAXITER, fret, ier, funcs )

       real      x(*), y(*), sig(*)
       integer   ndata, npars, ntofit
       real      xpar(*)
       integer   fitlist(npars)
       real      FTOL, fret
       integer   MAXITER
       integer   ier
       external  funcs

       integer   NPARMAX
       parameter ( NPARMAX = 100 )
       real      covar(NPARMAX,NPARMAX)
       real      ahess(NPARMAX,NPARMAX)
       real      oldxpar(NPARMAX)
       integer   npmax, ipar, ntries, inegl, maxnegl, iter
       real      alamda, chisq, ochisq, dxpar
       logical   loop
       integer   i, j
       character*80 line
       logical   test
       data      test / .false. /
       npmax = NPARMAX

c record the initial point - useful to keep track of progress
c (nonfunctional - can be commented out together with stuff at the end)
       if( test ) then
          do ipar = 1, npars
             oldxpar(ipar) = xpar(ipar)
          enddo
       endif

c Initializing call - if ALAMDA is negative, MRQMIN will set it to be
c positive and small and then return the initial value of CHI SQUARED
       alamda = -1.
       call MRQMIN( x, y, sig, ndata, xpar, npars, 
     *              fitlist, ntofit, covar, ahess, npmax, chisq, 
     *              funcs, alamda, ntries, ier )
       if( test ) then
          write(line,'(''MRQMIN: setup call; chisq, xpar = '',e10.4)')
     *          chisq
          call output(line)
          write(line,'(10f8.5)') ( xpar(i), i = 1, npars )
          call output(line)
       endif

c Keep track of how many negligible improvements have been achieved
       inegl = 0
       maxnegl = 3

c Loop over major iterations
       iter = 0
       loop = .true.
       do while( iter.lt.MAXITER .and. loop )
          iter = iter + 1
          ochisq = chisq
          call MRQMIN( x, y, sig, ndata, xpar, npars, 
     *                 fitlist, ntofit, covar, ahess, npmax, chisq, 
     *                 funcs, alamda, ntries, ier )

          if(test)write(line,'(''New chisq, ier: '',e10.4,i1)')chisq,ier

          if( ier.eq.0 ) then
             if( (ochisq-chisq) .gt. FTOL ) then
                loop  = .true.
                inegl = 0
                if( test ) call bug( 'w',
     *          'DRVMRQ: Chi squared decrease significant' )
             else
                inegl = inegl + 1
                if( inegl .lt. maxnegl ) then
                   loop = .true.
                   if( test ) call bug( 'w',
     *             'DRVMRQ: Insignificant decrease in chisq' )
                else
                   loop = .false.
                   write( line, '( ''DRVMRQ quits at iteration '',i2,
     *                          '': negligible decrease number '',i2)')
     *                          iter, inegl
                   if( test ) call bug( 'w', line )
                endif
             endif
          elseif( ier.eq.1 ) then
             loop = .false.
             write( line, '( ''MRQMIN: gives up after '',i2,'' tries'',
     *                  '' with alamda = '',e10.4 )' ) ntries, alamda
             if( test ) call bug( 'w', line )
             ier = 100 + iter
          elseif( ier.eq.2 ) then
             loop = .false.
          endif

c The next few lines are non-functional - they are just used to
c print out information about the progress of the fit. They can
c all be commented out to the end of the do loop.
          if( test ) then
             dxpar = 0.0
             do ipar = 1, npars
                dxpar = dxpar + (xpar (ipar) - oldxpar(ipar))**2
                oldxpar (ipar) = xpar (ipar)
             enddo
             dxpar = sqrt(dxpar)
             write( line, '( a, 2i4, f24.8, 2d14.4 )' )
     *       'MRQMIN: ', iter, ntries, chisq, dxpar, alamda
             call output(line)
             write( line, '(10f8.5)') ( xpar(j), j=1,npars )
             call output(line)
          endif

       enddo
c MAXITER exceeded?
       if( iter.eq.MAXITER ) ier = 3

c MRQMIN called with zero ALAMDA will return covariance matrix 
       alamda = 0.0
       call MRQMIN( x, y, sig, ndata, xpar, npars, fitlist,
     *              ntofit, covar, ahess, npmax, chisq, funcs, alamda, 
     *              ntries, ier )
       fret = chisq

c Calculate fit errors: and copy to output array
       do i = 1, npars
          xpar(i+npars) = 0.
       enddo
       do i = 1, ntofit
          j = fitlist(i)
          if( covar(j,j).lt.0. ) then
             ier = 4
          else
             xpar(j+npars) = sqrt( covar(j,j) )
          endif
       enddo

       return
       end


c--------------------------------------------------------------------

      SUBROUTINE MRQMIN( X,Y,SIG,NDATA, XPAR,NPARS, FITLIST,NTOFIT,
     *                   COVAR,ALPHA,NPMAX, CHISQ, FUNCS, ALAMDA,
     *                   NTRIES, IER )
c
c     The original Numerical Recipes version of this routine fits a model 
c     function YF of one variable X(I) with parameters XPAR to the data
c     Y(I). The model function has parameters XPAR(1->NPARS) which should
c     be optimized. It is also possible to optimize only a subset of the
c     parameters as listed in the vector FITLIST(1->NTOFIT); so if
c     parameters 1, 2, 4, 7 of a list of 10 have to be optimized, FITLIST
c     would have values 1,2,4,7, NTOFIT 4 and NPARS 10.
c
c     The model function and derivative are computed by a subroutine
c     MRQCOF. The function name FUNCS is passed as argument.
c
c     Use of the subroutine is as follows. On the first call, the control
c     parameter ALAMDA must be set to a negative number. This will 
c     initialize the routine and set ALAMDA to a positive number.
c     Successive calls will attempt to find a new set of parameters by
c     using a combination linear-quadratic step. If ALAMDA is large the
c     step will be linear, if small it will be quadratic.
c     ALAMDA will be progressively increased until the function decreases.
c     Once a decrease is found, MRQMIN updates the vector of parameter
c     values and returns with IER=0 and NTRIES=the number of different
c     values of ALAMDA tried.
c
c     This logic is quite different from the original Numerical Recipes
c     routine. The changes are described below.
c
c     1) The original routine computes the gradient every time a new point
c        is TRIED. This can be quite wasteful if the gradient is expensive
c        to compute, since if the point is not good the gradient is not
c        needed. So we use a logical flag, COMPGRAD, to tell the function
c        routine whether just the function itself is to be computed or
c        the gradient as well.
c
c     2) The original routine gives no information to the calling routine
c        on whether the point has been optimized or not. The only way to
c        find this out is by a check of the CHISQ: if that is unchanged,
c        then the point has not been upgraded. BUT sometimes the change
c        is small enough to underflow. The calling routine does not know
c        about that. Underflow is common in the original version, since 
c        the main loop on alamda can be repeated MANY times.
c        Solution: this version of MRQMIN iterates until an improvement is
c        found or a maximum number of iterations is reached. If the latter, 
c        a new integer argument, IER, is returned with a non-zero value.
c        Another new integer argument, NTRIES, returns the number of 
c        ALAMDA iterations performed.
c
c     3) Singular Value Decomposition is used instead of Gauss-Jordan,
c        and the additional arrays VMAT, WVEC needed by SVD are defined
c        and dimensioned. In the original version this subroutine produced
c        a direct error message if the number of iterations exceeded 30.
c        Now it returns it, and this is detected, and IER is set to 2.
c
      INTEGER    MMAX, ITMAX, ALAMFAC
      PARAMETER  ( MMAX    =100  )
      PARAMETER  ( ITMAX   = 20  )
      PARAMETER  ( ALAMFAC = 10. )

      INTEGER    NDATA, NPARS
      REAL       X(NDATA),Y(NDATA)
      REAL       SIG(NDATA), XPAR(NPARS)
      INTEGER    FITLIST(NPARS), NTOFIT
      INTEGER    NPMAX
      REAL       COVAR(NPMAX,NPMAX), ALPHA(NPMAX,NPMAX)
      REAL       CHISQ
      EXTERNAL   FUNCS
      REAL       ALAMDA
      INTEGER    NTRIES, IER

      REAL       ATRY(MMAX), BETA(MMAX), DXPAR(MMAX)
      REAL       VMAT(MMAX,MMAX), WVEC(MMAX)
      INTEGER    I, J, K, IHIT, IITER, ITS
      REAL       OCHISQ
      LOGICAL    COMPGRAD
      SAVE

c Initializing call. Set ALAMBDA to positive and return initial chi squared
      IF( ALAMDA.LT.0. ) THEN

         I = NTOFIT+1
         DO J = 1, NPARS
            IHIT = 0
            DO K = 1, NTOFIT
               IF( FITLIST(K).EQ.J ) IHIT=IHIT+1
            ENDDO
            IF( IHIT.EQ.0 ) THEN
               FITLIST(I) = J
               I = I+1
            ELSEIF( IHIT.GT.1 ) THEN
               CALL BUG( 'f', 'Improper permutation in FITLIST' )
            ENDIF
         ENDDO

         IF( I.NE.NPARS+1 )
     *       CALL BUG( 'f', 'Improper permutation in FITLIST' )

         ALAMDA = 0.001
         COMPGRAD = .FALSE.
         CALL MRQCOF( X,Y,SIG,NDATA, XPAR,NPARS, FITLIST,NTOFIT,
     *                ALPHA,BETA, NPMAX, CHISQ, FUNCS, COMPGRAD )
         OCHISQ = CHISQ
         RETURN

c Finishing call. Return covariance matrix.
      ELSE IF( ALAMDA.EQ.0.) THEN

         COMPGRAD = .TRUE.
         CALL MRQCOF( X,Y,SIG,NDATA, XPAR,NPARS, FITLIST,NTOFIT,
     *                ALPHA,BETA, NPMAX, CHISQ, FUNCS, COMPGRAD )
         CALL SVDCMP( ALPHA,NTOFIT,NTOFIT, NPMAX,NPMAX, WVEC,VMAT,ITS )
         DO I = 1, NPARS
            DO J = 1, NPARS
               COVAR(I,J) = 0.
               DO K = 1, NPARS
                  IF( WVEC(K).NE.0. )
     *            COVAR(I,J) = COVAR(I,J)+ALPHA(J,K)*VMAT(I,K)/WVEC(K)
               ENDDO
            ENDDO
         ENDDO
         CALL COVSRT( COVAR,NPMAX,NPARS, FITLIST,NTOFIT )
         RETURN

      ENDIF

c Regular step. First compute gradient and `hessian' (remains the same
c throughout this iteration)

      COMPGRAD = .TRUE.
      CALL MRQCOF( X,Y,SIG,NDATA, XPAR,NPARS, FITLIST,NTOFIT,
     *             ALPHA,BETA, NPMAX, CHISQ, FUNCS, COMPGRAD )
      OCHISQ = CHISQ

c Now iterate on ALAMDA until a good step is found
      DO IITER = 1, ITMAX

c Compute augmented matrix
         DO J = 1, NTOFIT
            DO K = 1, NTOFIT
               COVAR(J,K) = ALPHA(J,K)
            ENDDO
            COVAR(J,J) = ALPHA(J,J) * (1.+ALAMDA)
            DXPAR(J)   = BETA(J)
         ENDDO

c Solve for increment DXPAR
         CALL SVDCMP( COVAR, NTOFIT,NTOFIT,NPMAX,NPMAX, WVEC,VMAT,ITS )
         IF( ITS.EQ.30 ) THEN
            IER = 2
            RETURN
         ENDIF
         CALL SVBKSB( COVAR, WVEC,VMAT, NTOFIT,NPARS,
     *                NPMAX,NPMAX, BETA, DXPAR )

c Increment accepted solution (the commented-out line was the original
c one, and is WRONG because it incremented the last try, even if rejected,
c rather than the last ACCEPTED try)
         DO J = 1, NPARS
c           ATRY(FITLIST(J)) = ATRY(FITLIST(J)) + DXPAR(J)
            ATRY(FITLIST(J)) = XPAR(FITLIST(J)) + DXPAR(J)
         ENDDO

         COMPGRAD = .FALSE.
         CALL MRQCOF( X,Y,SIG,NDATA, ATRY,NPARS, FITLIST,NTOFIT,
     *                COVAR, DXPAR, NPMAX, CHISQ, FUNCS, COMPGRAD )

c Try is successful. Reduce ALAMDA (could be reduced more in the
c interest of efficiency) so next step will start closer to quadratic;
c update XPAR and CHISQ; and return
         IF( CHISQ.LT.OCHISQ ) THEN
            ALAMDA = ALAMDA / ALAMFAC
            OCHISQ = CHISQ
            DO J = 1, NTOFIT
c Unnecessary to update gradient and hessian since it will be done 
c at the beginning of next major iteration
c               DO K = 1, NTOFIT
c                 ALPHA(J,K) = COVAR(J,K)
c               ENDDO
c               BETA(J) = DXPAR(J)
c Update parameter vector
               XPAR(FITLIST(J)) = ATRY(FITLIST(J))
            ENDDO
            GO TO 100

c ALAMDA still too small. Must increase ALAMDA (more gradient-like 
c step of smaller length) and try again
         ELSE
            ALAMDA = ALAMDA * ALAMFAC
         ENDIF
      ENDDO

c We end up here if too many tries were needed
      NTRIES = IITER - 1
      IER = 1
      RETURN

c We end up here if successful
  100 CONTINUE
      NTRIES = IITER
      IER = 0
      RETURN

      END

c-------------------------------------------------------------------

c Routine to resort out the matrix so that it conforms to the original
c definition again, if a list of fixed parameters (FITLIST) was given.

      SUBROUTINE COVSRT( COVAR,NPMAX, NPARS,FITLIST,NTOFIT )

      INTEGER NPARS
      INTEGER NPMAX, NTOFIT
      REAL    COVAR(NPMAX,NPMAX)
      INTEGER FITLIST(NTOFIT)

      INTEGER I,J
      REAL    SWAP

      DO J = 1, NPARS-1
         DO I = J+1, NPARS
            COVAR(I,J) = 0.
         ENDDO
      ENDDO
      DO I = 1, NTOFIT-1
         DO J = I+1, NTOFIT
            IF( FITLIST(J).GT.FITLIST(I) ) THEN
               COVAR(FITLIST(J),FITLIST(I)) = COVAR(I,J)
            ELSE
               COVAR(FITLIST(I),FITLIST(J)) = COVAR(I,J)
            ENDIF
         ENDDO
      ENDDO
      SWAP = COVAR(1,1)
      DO J = 1, NPARS
         COVAR(1,J) = COVAR(J,J)
         COVAR(J,J) = 0.
      ENDDO
      COVAR(FITLIST(1),FITLIST(1)) = SWAP
      DO J = 2, NTOFIT
         COVAR(FITLIST(J),FITLIST(J)) = COVAR(1,J)
      ENDDO
      DO J = 2, NPARS
         DO I = 1, J-1
            COVAR(I,J) = COVAR(J,I)
         ENDDO
      ENDDO
      RETURN
      END

c--------------------------------------------------------------------

      SUBROUTINE MRQCOF( X,Y,SIG,NDATA, XPAR,NPARS, FITLIST,NTOFIT,
     *                   ALPHA,BETA, NALP, CHISQ, FUNCS, COMPGRAD )
c
c     This is the original Numerical Recipes MRQCOF, except for the
c     introduction of the logical variable COMPGRAD. If COMPGRAD is .TRUE.,
c     the function and its derivative are computed, and the gradient
c     and Hessian are updated. If not, only the new CHISQ is computed.
c     COMPGRAD must be passed to FUNCS. Use of COMPGRAD makes sense if gradients
c     and derivatives are expensive to compute. Otherwise, all references
c     to COMPGRAD can be deleted and all IF(COMPGRAD) made true.

      INTEGER   NDATA
      REAL      X(NDATA),Y(NDATA),SIG(NDATA)
      INTEGER   NPARS
      REAL      XPAR(NPARS)
      INTEGER   NTOFIT
      INTEGER   FITLIST(NTOFIT)
      INTEGER   NALP
      REAL      ALPHA(NALP,NALP), BETA(NPARS)
      REAL      CHISQ
      EXTERNAL  FUNCS
      LOGICAL   COMPGRAD

      INTEGER   MMAX
      PARAMETER ( MMAX=100)
      REAL      YMOD, DYDXPAR(MMAX)
      INTEGER   I, J, K
      REAL      SIG2I, DY, WT

      DO J = 1, NTOFIT
         DO K = 1, J
            ALPHA(J,K) = 0.
         ENDDO
         BETA(J) = 0.
      ENDDO
      CHISQ = 0.
      DO I = 1, NDATA
         CALL FUNCS( X(I),XPAR, YMOD,DYDXPAR, NPARS, COMPGRAD )
         SIG2I = 1. / (SIG(I)*SIG(I))
         DY    = Y(I) - YMOD
         IF( COMPGRAD ) THEN
            DO J = 1, NTOFIT
               WT = DYDXPAR(FITLIST(J)) * SIG2I
               DO K = 1, J
                  ALPHA(J,K) = ALPHA(J,K) + WT * DYDXPAR(FITLIST(K))
               ENDDO
               BETA(J) = BETA(J) + DY * WT
            ENDDO
         ENDIF
         CHISQ = CHISQ + DY*DY*SIG2I
      ENDDO
      IF( COMPGRAD ) THEN
         DO J = 2, NTOFIT
            DO K = 1, J-1
               ALPHA(K,J) = ALPHA(J,K)
            ENDDO
         ENDDO
      ENDIF
      RETURN
      END

c
c------------------------------------------------------------------------
c
      SUBROUTINE SVDCMP( MATX,M,N, MPMAX,NPMAX, WVEC,VMAT, ITS )

      INTEGER    NMAX
      PARAMETER ( NMAX=101 )
      INTEGER    M,N, MPMAX,NPMAX
      REAL       MATX(MPMAX,NPMAX)
      REAL       WVEC(NPMAX), VMAT(NPMAX,NPMAX)
      INTEGER    ITS

      REAL       RV1(NMAX)
      REAL       C,F,G,H,S,Y,Z,SCALE,ANORM,X
      INTEGER    I,J,K,L,NM

      G     = 0.0
      SCALE = 0.0
      ANORM = 0.0
      DO I = 1, N
         L      = I+1
         RV1(I) = SCALE*G
         G      = 0.0
         S      = 0.0
         SCALE  = 0.0
         IF( I.LE.M ) THEN
            DO K = I, M
               SCALE = SCALE + ABS(MATX(K,I))
            ENDDO
            IF( SCALE.NE.0.0 ) THEN
               DO K = I, M
                  MATX(K,I) = MATX(K,I)/SCALE
                  S         = S + MATX(K,I)*MATX(K,I)
               ENDDO
               F         = MATX(I,I)
               G         = -SIGN( SQRT(S),F )
               H         = F*G-S
               MATX(I,I) = F-G
               IF( I.NE.N ) THEN
                  DO J = L, N
                     S = 0.0
                     DO K = I, M
                        S = S + MATX(K,I)*MATX(K,J)
                     ENDDO
                     F = S/H
                     DO K = I, M
                        MATX(K,J) = MATX(K,J) + F*MATX(K,I)
                     ENDDO
                  ENDDO
               ENDIF
               DO K = I, M
                  MATX(K,I) = SCALE * MATX(K,I)
               ENDDO
            ENDIF
         ENDIF
         WVEC(I) = SCALE * G
         G    = 0.0
         S    = 0.0
         SCALE= 0.0
         IF( (I.LE.M).AND.(I.NE.N) ) THEN
            DO K = L, N
               SCALE = SCALE + ABS(MATX(I,K))
            ENDDO
            IF( SCALE.NE.0.0 ) THEN
               DO K = L, N
                  MATX(I,K) = MATX(I,K) / SCALE
                  S         = S + MATX(I,K)*MATX(I,K)
               ENDDO
               F         = MATX(I,L)
               G         = -SIGN(SQRT(S),F)
               H         = F*G-S
               MATX(I,L) = F-G
               DO K = L, N
                  RV1(K) = MATX(I,K) / H
               ENDDO
               IF( I.NE.M ) THEN
                  DO J = L, M
                     S = 0.0
                     DO K = L, N
                        S = S + MATX(J,K)*MATX(I,K)
                     ENDDO
                     DO K = L, N
                        MATX(J,K) = MATX(J,K) + S*RV1(K)
                     ENDDO
                  ENDDO
               ENDIF
               DO K = L, N
                  MATX(I,K) = SCALE*MATX(I,K)
               ENDDO
            ENDIF
         ENDIF
         ANORM = MAX( ANORM, (ABS(WVEC(I))+ABS(RV1(I))) )
      ENDDO
      DO I = N, 1, -1
         IF( I.LT.N ) THEN
            IF( G.NE.0.0 ) THEN
               DO J = L, N
                  VMAT(J,I) = (MATX(I,J)/MATX(I,L)) / G
               ENDDO
               DO J = L, N
                  S = 0.0
                  DO K = L, N
                     S = S + MATX(I,K)*VMAT(K,J)
                  ENDDO
                  DO K = L, N
                     VMAT(K,J) = VMAT(K,J) + S*VMAT(K,I)
                  ENDDO
               ENDDO
            ENDIF
            DO J = L, N
               VMAT(I,J) = 0.0
               VMAT(J,I) = 0.0
            ENDDO
         ENDIF
         VMAT(I,I) = 1.0
         G = RV1(I)
         L = I
      ENDDO
      DO I = N, 1, -1
         L = I+1
         G = WVEC(I)
         IF( I.LT.N ) THEN
            DO J = L, N
               MATX(I,J) = 0.0
            ENDDO
         ENDIF
         IF( G.NE.0.0 ) THEN
            G = 1.0/G
            IF( I.NE.N ) THEN
               DO J = L, N
                  S = 0.0
                  DO K = L, M
                     S = S + MATX(K,I)*MATX(K,J)
                  ENDDO
                  F = (S/MATX(I,I)) * G
                  DO K = I, M
                     MATX(K,J) = MATX(K,J) + F*MATX(K,I)
                  ENDDO
               ENDDO
            ENDIF
            DO J = I, M
               MATX(J,I) = MATX(J,I)*G
            ENDDO
         ELSE
            DO J = I, M
               MATX(J,I) = 0.0
            ENDDO
         ENDIF
         MATX(I,I) = MATX(I,I) + 1.0
      ENDDO
      DO K = N, 1, -1
         DO ITS = 1, 30
            DO L = K, 1, -1
               NM = L-1
               IF( (ABS(RV1(L))  +ANORM) .EQ. ANORM ) GOTO 2
               IF( (ABS(WVEC(NM))+ANORM) .EQ. ANORM ) GOTO 1
            ENDDO
    1       C = 0.0
            S = 1.0
            DO I = L, K
               F = S*RV1(I)
               IF( (ABS(F)+ANORM) .NE. ANORM ) THEN
                  G       = WVEC(I)
                  H       = SQRT(F*F+G*G)
                  WVEC(I) = H
                  H       = 1.0/H
                  C       = (G*H)
                  S       = -(F*H)
                  DO J = 1, M
                     Y = MATX(J,NM)
                     Z = MATX(J,I)
                     MATX(J,NM) =  (Y*C)+(Z*S)
                     MATX(J,I)  = -(Y*S)+(Z*C)
                  ENDDO
               ENDIF
            ENDDO
    2       Z = WVEC(K)
            IF( L.EQ.K ) THEN
               IF( Z.LT.0.0 ) THEN
                  WVEC(K) = -Z
                  DO J = 1, N
                     VMAT(J,K) = -VMAT(J,K)
                  ENDDO
               ENDIF
               GOTO 3
            ENDIF
            IF( ITS.EQ.30 ) RETURN
            X  = WVEC(L)
            NM = K-1
            Y  = WVEC(NM)
            G  = RV1(NM)
            H  = RV1(K)
            F  = ( (Y-Z)*(Y+Z) + (G-H)*(G+H) ) / (2.0*H*Y)
            G  = SQRT( F*F+1.0 )
            F  = ( (X-Z)*(X+Z) + H*( (Y/(F+SIGN(G,F))) - H)  ) / X
            C  = 1.0
            S  = 1.0
            DO J = L, NM
               I      = J+1
               G      = RV1(I)
               Y      = WVEC(I)
               H      = S*G
               G      = C*G
               Z      = SQRT(F*F+H*H)
               RV1(J) = Z
               C      = F/Z
               S      = H/Z
               F      =  (X*C)+(G*S)
               G      = -(X*S)+(G*C)
               H      = Y*S
               Y      = Y*C
               DO NM = 1, N
                  X          = VMAT(NM,J)
                  Z          = VMAT(NM,I)
                  VMAT(NM,J) =  (X*C)+(Z*S)
                  VMAT(NM,I) = -(X*S)+(Z*C)
               ENDDO
               Z       = SQRT(F*F+H*H)
               WVEC(J) = Z
               IF( Z.NE.0.0 ) THEN
                  Z = 1.0/Z
                  C = F*Z
                  S = H*Z
               ENDIF
               F =  (C*G)+(S*Y)
               X = -(S*G)+(C*Y)
               DO NM = 1, M
                  Y          = MATX(NM,J)
                  Z          = MATX(NM,I)
                  MATX(NM,J) =  (Y*C)+(Z*S)
                  MATX(NM,I) = -(Y*S)+(Z*C)
               ENDDO
            ENDDO
            RV1(L)  = 0.0
            RV1(K)  = F
            WVEC(K) = X
         ENDDO
    3    CONTINUE
      ENDDO
      RETURN
      END

c
c------------------------------------------------------------------------
c
      SUBROUTINE SVBKSB( U,W,V, NTOFIT,N, MP,NP, BETA,DX )

      INTEGER     NMAX
      PARAMETER ( NMAX=101 )

      INTEGER     NTOFIT, N, MP,NP
      REAL        U(MP,NP), W(NP), V(NP,NP)
      REAL        BETA(MP), DX(NP)

      REAL        TMP(NMAX), S
      INTEGER     I, J

      DO J = 1, N
         S = 0.
         IF( W(J).NE.0. ) THEN
            DO I = 1, NTOFIT
               S = S + U(I,J) * BETA(I)
            ENDDO
            S = S / W(J)
         ENDIF
         TMP(J) = S
      ENDDO
      DO J = 1, N
         S = 0.
         DO I = 1, N
            S = S + V(J,I) * TMP(I)
         ENDDO
         DX(J) = S
      ENDDO
      RETURN
      END
