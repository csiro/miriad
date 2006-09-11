c= gaufit - Fits gaussians to profile
c& bpw
c: image-analysis
c+
ccc FIX params keyword
ccc profile_at => profile_at_
ccc say in doc what profile_at contains
ccc
c
cc gaufit fits gaussians to a profile and can write the output to
c a miriad dataset, a logfile or the terminal.
c
c The fitting is done using an adapted version of fitting routines in
c numerical recipes.
c
c Obligatory parameters are:
c a) either or both of 'in=' and 'parinp='
c b) 'rmsest='
c c) and either 'estim=' or 'options=findestim'
c
c The ease of fitting depends strongly on the initial estimates. These
c can be given using the estim= keyword, or they can be automatically
c found (options=findestim). The latter is usually preferable, though
c the former may be necessary in pathological cases.
c
c If multiple gaussians are fit but the fit is bad, another try is
c made with one less gaussian. This is repeated until the fit works.
c A fit is considered bad if the rms of the residual is higher than
c 1.8 times the rms estimate, or if the parameters lie outside the
c range given by cutoff=, crange= and wrange=, or if the fitting
c routine does not converge. If after all possible ways of retrying a
c fit the rms is between 1.8 and 5.4 times the rms estimate, accept
c the fit after all; maybe a low-level component increased the rms, or
c the profile is not perfectly gaussian, but the fit is somewhat
c reasonable.
c
c Automatic initial estimates are made by first finding the velocity
c and amplitude of the peak. An estimated width is found by looking to
c both sides of the peak for the nearest zero of the 2nd derivative and
c at where the half-maximum lies. The most consistent combination gives
c the width estimate. For low S/N profiles the width is found from the
c integral out to the nearest zero. This estimated component is then
c subtracted from the profile and the process is repeated until the
c maximum amplitude is too low, or until the maximum number of gaussians
c has been found.
c
c If parinp= is used gaussian parameters are taken from this dataset
c for all pixels outside the specified region. Inside the region new
c fits are made. All results are written to the params= dataset. The
c number of fitted gaussians does not have to be the same between the
c parinp= and params= datasets.
c This creation of an extra dataset is needed because MIRIAD
c very-deep-down disallows opening an existing dataset for writing.
c So it is not possible to add new fits to an existing parameter set.
c
c If parinp= is present, but in= is not, then no new fits are made, but
c the gaussians in parinp are sorted as specified by the cmpsort=
c keyword. They can also be transformed as specified by
c options=integral,fwhm,dispersion.
c
c< in
c Input dataset with spectra to be fit.
c
c< region
c Note: the region=mask option is not implemented. The mask of the
c input dataset is used however. If there is one, the profile value
c at masked datapoints is set to zero before doing the fit.
c
c@ rmsest
c Give a value for the rms of the profile or a dataset from which it can
c be read. No default. Used by the fitting procedure to determine when
c convergence occurs.
c If a single real value is given, it is used for each profile.
c If the name of a dataset is given, the rms at each pixel is read from
c that dataset; this is particularly useful when fitting a dataset
c created with linmos, for which the rms will vary across the field.
c Create the rms dataset using 'linmos in=list options=sens out=rms'
c followed by 'maths exp=rms*<rmsvalue>'
c The value should be the rms of the profile as found with imstat on
c signal-free regions.
c
c@ estim
c Initial estimates. Give an estimate for the amplitude, velocity and
c fwhm for each component (if options=integral, pixels or dispersion
c is used, give integral instead of amplitude etc.). This is quite
c critical and should already be reasonably close. The same initial
c estimate will be used for all profiles.
c If options=findestim is used, the initial estimates are determined by
c gaufit and estim= is ignored
c
c@ ngauss
c Maximum number of gaussian components to fit (maximum 10, default 1).
c
c@ parinp
c Optional input parameter dataset. All fits outside the specified
c region are read from this dataset. If in= is not present, the fits
c in the selected region are just sorted and selected as specified by
c cmpsort, cutoff, vrange and wrange. The fits outside the region are
c untouched.
c
c@ params
c Optional output dataset to which the fit parameters can be written.
c For each fitted component six planes are written, one with the
c amplitude (or integral), one with the position and one with the fwhm
c (or dispersion), and three more with the errors. The planes with
c errors come after the planes with all fit results. A final plane
c which contains the rms of the residual is added.
c
c@ fitaxis
c This determines along which axis profiles are taken. The default is
c the velocity ('vel') axis. Other possible answers are 'x', 'y', 'z',
c 'a', 'ra', 'dec', 'lon', 'lat', 'freq'.
c
c@ smooth
c First smooth the profile over 'smooth' pixels before fitting
c
c@ options
c Controls the output. Defaults are the opposite of the action specified
c by an option. Possible options are:
c   nofit:        output the initial estimates, don't make fits
c   findestim:    let gaufit determine the initial estimates
c
c   noprint:      do not print the fit results on the terminal
c   supbad:       suppress results for fits outside ranges given by
c                 cutoff, crange and wrange and results for bad fits.
c   estimout:     print initial estimates
c   intermout:    print some intermediate results for multi-component fits
c   abspix:       x, y coordinates on output are relative to lower left,
c                 rather than relative to crpix
c   abscoo:       x, y coordinates on output are absolute coordinates
c
c   wrprof:       write out a file with the data and the fit so that it
c                 at least is possible to use plotting programs to
c                 compare them; a kludge until gaufit itself can plot.
c                 filenames will be 'profile_at_$x_$y' (or given by prof=)
c
c   integral:     write out integral of gaussian instead of amplitude
c   dispersion:   write out dispersion of gaussian instead of fwhm
c   pixel:        write center and width in pixels, not in axis units
c                 (for these three: also interpret input for cutoff,
c                 vrange, wrange and estim keywords as int/disp/pix)
c
c   average:      first make an average profile of the selected region
c                 and then fit one single gaussian to this profile
c   summed:       first make a summed profile of the selected region
c                 and then fit one single gaussian to this profile
c
c   negative      amplitudes may be both positive and negative, instead
c                 if just positive
c   fixvelo:      fix the velocities to the initial estimate during fit
c   fixwidth:     fix the width to the initial estimate while fitting
c                 (fixvelo and fixwidth can be combined)
c
c@ cmpsort
c This parameter specifies how to sort the resulting components
c The following options exist
c   velocity, amplitude, integral, width, vdiff, vrange
c Option 'velocity' and 'fwhm' result in components sorted on increasing
c velocity or width.
c Option 'amplitude' and 'integral' result in components sorted on
c decreasing amplitude or integral.
c If vdiff is used, then a second parameter gives a center velocity;
c components are sorted based on the difference between the fitted
c velocity and this center velocity.
c If vrange is used, the second and third parameter give a velocity
c range. If one component is within this range, it becomes the first.
c If none or more than one is within this range, they are sorted on
c velocity.
c Usually, cmpsort is applied for every pixel of the dataset. This is
c wanted when originally fitting (in= used). It is also generally wanted
c when refitting part of the dataset (in= and parinp= used), especially
c when more gaussians are to be added in selected regions. However, when
c only parinp= is present, the sorting is done only in the selected
c region and everything outside is left alone.
c
c@ model
c Optional output dataset, to which theoretical (described by fit)
c profiles can be written.
c
c@ residual
c Optional output dataset, to which the difference between the profile
c and the fit can be written
c
c@ prof
c Optional filename for use with options=wrprof
c
c@ cutoff
c Give a cutoff for the amplitude/integral. Can be 1, 2 or 3 values, all
c in units of the rms.
c If one value, fits with amplitude (integral if options=integral set)
c below the given cutoff are not written out. The absolute value of the
c amplitude is used if options=negative was set.
c If two values, fits with amplitude/integral in the specified range are
c eliminated.
c If three values, further eliminate fits for which the ratio of amplitude
c to amplitude error is less than specified ratio (default 1).
c Default: cut off amplitudes below 3 times the rms and with amp/err<2.
c
c@ cutval
c When using options=average or summed, only average/sum pixels whose
c intensity is above cutval (default: sum all)
c
c@ crange
c Give a range (in units along profile) between which the center should
c lie. Fits that result in centers outside this range are not written out.
c A third value specifies to not write out fits whose center is uncertain
c by more than that number of channels.
c Default: cut off centers outside profile range and uncertain by more
c than four channels.
c
c@ wrange
c Give 1, 2 or 3 values: a lower limit, and/or a range, and/or a S/N ratio
c for the width (fwhm or dispersion). Fits giving widths below the lower
c limit, outside the range, or with too uncertain widths are not written
c out.
c Default: cut off fwhms less than 1 pixel and larger than the length
c of the profile, and with value/error less than 1.
c
c@ log
c If the name of a file is given, the results of the fitting are written
c to this file instead of to the terminal
c--
c
c***********************************************************************
c
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
c     bpw 15dec92 Adapt for changed fndaxnum
c     bpw  2mar93 Adapt for masking in xyzio
c     bpw 14dec93 Add call logclose
c     bpw 14nov94 Fix erroneous 'Center outside range' for cdelt3<0
c     pjt 15mar95 Add 'external rtfmt' for ANSI f2c
c     bpw 26mar96 Fix format for writing profile with negative numbers
c     bpw 14jul97 Fix format for writing rms
c     bpw 10jan98 Add automatic initial estimates, some changes in handling
c     bpw  4feb98 Made fitting much more robust, add parinp and using old
c                 parameters.
c     bpw 16mar98 Some simplifications and bug fixes
c     bpw  4jun98 Added rmsest='dataset' and make full use of region keyword
c     rjs 20oct98 Changes to avoid floating point underflow in exp() function
c     bpw 26feb99 Make compiler more silent by avoiding warnings
c     bpw  4mar99 Fixed fitting selected channel range, add cutval=
c     bpw 25aug99 Fix bug with masking while finding initial estimates
c     bpw 21oct99 Add individual gaussians to ascii output file
c     bpw 16nov99 Fix bug in optindex calculation
c     bpw 17dec99 Add prof=, fix bug with conversion of limlist
c     rjs 23jan00 Change some subroutine args to real-valued to avoid
c                 compiler complaints
c     rjs 28jan00 Some FORTRAN standardization to get it through a
c                 compiler.
c     bpw 28feb01 Make it work under linux
c     bpw 21may01 Add smooth keyword
c     rjs 18sep05 Corrected type mismatch error.
c
c************************************************************************

c The main program first gets all inputs and then calls the workhorse.
c The inputs are:
c units:       dataset handles
c              1: of input dataset
c              2: of previous dataset
c              3: of model dataset
c              4: of residual dataset
c              5: of parameter dataset
c              6: of rms dataset
c prfinfo:     profile info:
c              1: number of spectra in cube
c              2: number of channels in spectrum
c              3: axis length of parameter dataset
c              5: blc of x-axis region
c              6: blc of y-axis region
c              7: blc of z-axis region
c              8: trc of x-axis region
c              9: trc of y-axis region
c             10: trc of z-axis region
c ngauss:      1: maximum number of gaussians ever to fit
c              2: maximum number of gaussians to estimate
c              3: number of gaussians found by estimator
c              4: actual number of gaussians fit on pixel
c              5: number of gaussians in parinp dataset
c              6: number of gaussians in output dataset
c gausspar:    results of fitting, size 7*MAXCMP+1
c              el 1=integral, el 2=center, el 3=dispersion; 3*MAXCMP times
c              then follow errors, 3*MAXCMP times
c              then error flags, one for each of MAXCMP components
c                   (see cuttofs/errout for values)
c              last el = rms
c limlist:     ranges for amp/int,ctr,fwhm/disp (els 1-12)
c              intensity cutoff(13)
c              smoothing (14)
c              estimate of rms (el 20) (15-19 free for later use)
c              factors for some algorithmic multiplications (els 21-25)
c                   (see inpgauss for description)
c cmpsort:     el 1=what to sort on
c              el 2=center velocity for vdiff, or min. of range
c              el 3=                              max. of range
c              el 4=max # gaussians, used when sorting a range

      program gaufit

      character*50 version
      parameter    ( version = 'gaufit: version 2.1 28-Feb-01' )
      integer      units(   6)
      integer      prfinfo(10)
      integer      MAXRUNS
      parameter    ( MAXRUNS = 1024 )
      integer      runs(3,MAXRUNS)
      integer      ngauss(  6)
      real         limlist(25)
      real         cmpsort( 4)
      character*80 prnm

      call output(version)
      call inputs(units,prfinfo,runs,ngauss,limlist,cmpsort,prnm)
      call work(  units,prfinfo,runs,ngauss,limlist,cmpsort,prnm)
      call finish(units,version)
      call keyfin

      end


c************************************************************************

c Inputs reads all keyword values and transforms them to variables
c usable in work.

      subroutine inputs(units,prfinfo,runs,ngauss,limlist,cmpsort,prnm)

      integer        MAXBOXES, MAXRUNS
      parameter      ( MAXBOXES = 128, MAXRUNS=1024 )
      integer        units(*)
      integer        prfinfo(*)
      integer        runs(3,MAXRUNS)
      integer        ngauss(*)
      real           limlist(*)
      real           cmpsort(*)
      character*80   prnm

      character*512  logfile
      include       'maxnax.h'
      character*512  inp, pin, cin, mdl, res, par, rms
      logical        spectra
      logical        hdprsnt
      integer        naxis, fitax
      real           naxesgp, dummy
      integer        axleni(MAXNAX), blc(MAXNAX), trc(MAXNAX)
      integer        axlenp(MAXNAX)
      integer        boxes(MAXBOXES), nruns, plane(MAXNAX)
      integer        i, dumprf(4)

c Initialize keyword routines
      call keyini

c Read names of datasets
      call keya( 'in',       inp, ' ' )
      call keya( 'model',    mdl, ' ' )
      call keya( 'residual', res, ' ' )
      call keya( 'parinp',   pin, ' ' )
      call keya( 'params',   par, ' ' )
      call assertl( inp.ne.' ' .or. pin.ne.' ',
     *              'You must specify an input file' )

c Output profiles prefix
      call keya( 'prof',     prnm, 'profile_at' )

c Decode options keyword
      call inpopts()

c Write to a log file?
      call keya( 'log', logfile, ' ' )
      if( logfile.ne.' ' ) then
         call opttrue('logf')
         call logopen( logfile, ' ' )
      endif

      do i=1,6
         units(i)=0
      enddo
c Open input dataset (spectra or previous parameters)
c Get info about it: fitax, # of profiles to do, # of pixels in profile
c call to setopen with 'old' and fitax=0 will find fitax
c Call to cooinfo with fitax=0 will read fitax info from crval(naxis+1)
      fitax = 0
      if( inp.ne.' ' ) then
         call setopen( inp,'old',units(1), naxis,axleni, prfinfo,fitax )
         cin = inp
         if( hdprsnt(units(1),'mask') ) call opttrue('inmask')
      else
         call setopen( pin,'old',units(1), naxis,axleni, prfinfo,naxis )
         cin = pin
      endif
      call cooinfo( 1, units(1), fitax, dummy )

      do i=1,MAXNAX
      plane(i)=1
      enddo
      call boxinput( 'region', cin, boxes,MAXBOXES )
      call boxset(   boxes, naxis, axleni, ' ' )
      call boxinfo(  boxes, naxis, blc, trc )
      prfinfo( 5) = blc(1)
      prfinfo( 6) = blc(2)
      prfinfo( 7) = blc(3)
      prfinfo( 8) = trc(1)
      prfinfo( 9) = trc(2)
      prfinfo(10) = trc(3)
      call boxruns(  naxis, plane, 'r', boxes, runs,MAXRUNS,nruns,
     *               blc(1),trc(1),blc(2),trc(2) )
c Bug in boxes: whatever the input for the last 4 args is, it is always
c changed to blc,trc on the first pass. So: since here coordinates relative
c to 1,1 are needed: fix all runs
      do i=1,nruns
      runs(1,i) = runs(1,i) + prfinfo(6) -1
      runs(2,i) = runs(2,i) + prfinfo(5) -1
      runs(3,i) = runs(3,i) + prfinfo(5) -1
      enddo

c Only if input dataset given: open model and residual dataset, if required.
c dumprf(1) is unit to copy header from
      if( inp.ne.' ' ) then
         dumprf(1) = units(1)
         if( mdl.ne.' ' )
     *   call setopen( mdl,'new',units(3), naxis,axleni, dumprf,fitax)
         if( res.ne.' ' )
     *   call setopen( res,'new',units(4), naxis,axleni, dumprf,fitax)
      endif


c Open dataset with previous fits, if wanted
c axlen and naxis will define parameter output set
      if( pin.ne.' ' ) then
         if( inp.eq.' ' ) call xyzclose(units(1))
         if( inp.eq.' ' ) units(1) = 0
         call setopen( pin,'old',units(2), naxis,axlenp, dumprf,naxis)
         call assertl( 6*((axlenp(naxis)-1)/6)+1 .eq. axlenp(naxis),
     *        'Input parameter dataset has wrong number of channels' )
         call assertl(axleni(1).eq.axlenp(1).and.axleni(2).eq.axlenp(2),
     *        'length of x and y axes of in= and parinp= incompatible' )
         prfinfo(3) = axlenp(naxis)
      endif

c Get gaussian related input: number, parameters to fit, interpretation
c of parameters
      spectra = inp.ne.' '
      call inpgauss(spectra,prfinfo,rms,ngauss,limlist,cmpsort)

c Open parameter dataset
c If no parinp given: use inp, but change the axes: the fitax will be missing
c and a parameter axis is added.
c Also added is a dummy last axis description to save the fitax coordinates
c If parinp given, use its axes, except for the last, which can change if
c ngauss increased.
      if( par.ne.' ' ) then
         if( pin.eq.' ' ) then
            do i = 1, naxis
               if( i.lt.fitax ) axlenp(i) = axleni(i)
               if( i.gt.fitax ) axlenp(i) = axleni(i+1)
            enddo 
            dumprf(1) = units(1)
            ngauss(5) = 0
            ngauss(6) = ngauss(1)
         else
            fitax = naxis
            dumprf(1) = units(2)
            ngauss(5) = (axlenp(fitax)-1)/6
            ngauss(6) = max( ngauss(1), ngauss(5) )
         endif
         axlenp(naxis) = 6*ngauss(6) + 1
         prfinfo(3)    = axlenp(naxis)
 
         call setopen(par,'new',units(5), naxis,axlenp,dumprf,naxis)

c create parameter axis on axis naxis
c copy velocity description from axis fitax to axis naxis+1
         if( inp.ne.' ' ) naxesgp = real(naxis)
         if( inp.eq.' ' ) naxesgp = real(naxis) + 100
         call cooinfo( 2, units(5), fitax, naxesgp )
      else
         units(5) = 0
      endif

c Open rms dataset
      if( rms .ne. ' ' ) then
c        fitax=8 selects ' ' as subcube for xyzsetup
         call setopen( rms,'old',units(6), naxis,axlenp, dumprf,8 )
         call assertl( axlenp(3).eq.1, 'rms dataset has >1 channel' )
         call assertl(axleni(1).eq.axlenp(1).and.axleni(2).eq.axlenp(2),
     *        'length of x and y axes of in= and rmsest= incompatible' )
      else
         units(6) = 0
      endif

      return
      end

c***********************************************************************

c Open input dataset and get information on it.
c Or create output dataset.

      subroutine setopen( name,status,unit, naxis,axlen,prfinfo,fitax)

      character*(*) name
      character*(*) status
      integer       unit
      integer       naxis, axlen(*)
      integer       prfinfo(*), fitax

      include       'maxnax.h'
      character*1   axis
      character*4   type
      character*8   axnames
      integer       blc(MAXNAX), trc(MAXNAX), i
      integer       viraxl(MAXNAX), vircsz(MAXNAX)
      save          axis
      data          axnames / 'xyzabcd ' /

c Open and get dimension of input dataset: naxis.
      if( status.eq.'old' ) then
         naxis = MAXNAX
         call xyzopen( unit, name, 'old', naxis, axlen )
      endif
      if( status.eq.'new' ) then
         call xyzopen(  unit, name, 'new', naxis, axlen )
         call headcopy( prfinfo(1), unit, 0, naxis, 0,0 )
      endif

c Find out which axis is to be fit. First read keyword, then call fndaxnum
c which reads the header and sets the value of fitaxis if not given in keyword.
      if( status.eq.'old' .and. fitax.eq.0 ) then
         call keya( 'fitaxis', type, 'freq' )
         if( type(1:2).eq.'ra'  ) type = 'lon'
         if( type(1:3).eq.'dec' ) type = 'lat'
         if( type(1:3).eq.'vel' ) type = 'freq'
         axis = ' '
         call fndaxnum( unit, type, axis, fitax )
         call assertl( axis.ne.' ',
     *                 'Specified fit axis not found in dataset' )
      else
         axis = axnames(fitax:fitax)
      endif


c Set up xyzio routines for input dataset
c and figure out number of profiles to do and their length
      do i = 1, naxis
         blc(i) = 1
         trc(i) = axlen(i)
      enddo

      call xyzsetup( unit, axis, blc, trc, viraxl, vircsz )

c Store some parameters in array prfinfo, for easier transfering
c prfinfo(1)=# profiles, prfinfo(2)=nchan
      prfinfo(1) = vircsz(naxis) / vircsz(1)
      prfinfo(2) = viraxl(1)

      return
      end


c***********************************************************************

c Get parameters controlling type of fit made and interpretation of
c parameters for the user.

      subroutine inpopts()
      integer      NOPTS
      parameter    ( NOPTS = 19 )
      character*10 optns(NOPTS)
      logical      optvals(NOPTS)
      integer      optindex
      logical      opttab(27*26)

      integer      i

      common /OPTCOM/ opttab
      data       optns /
     *           'nofit', 'findestim', 'logf', 'wrprof',
     *           'noprint', 'supbad', 'estimout', 'intermout',
     *           'abspix', 'abscoo',
     *           'average', 'summed', 'negative', 'fixvelo', 'fixwidth',
     *           'inmask', 'integral', 'dispersion', 'pixels' /
c abscoo         abspix       average
c dispersion
c estimout
c findestim      fixvelo        fixwidth
c inmask         integral       intermout
c logf
c negative       nofit          noprint
c pixels
c summed         supbad
c wrprof
c Get options list.
      do i = 1, NOPTS
         optvals(i) = .FALSE.
      enddo
      call options( 'options', optns, optvals, NOPTS )

c Convert to hashtable based on chars 1 and 4
      do i = 1, NOPTS
         opttab(optindex(optns(i))) = optvals(i)
      enddo
      return
      end

c Set an option to true
      subroutine opttrue(opt)
      character*(*) opt
      integer       optindex
      logical       opttab(27*26)
      common /OPTCOM/ opttab
      opttab(optindex(opt)) = .TRUE.
      return
      end

c Ask for the value of an option
      subroutine optlist(opt,optval)
      character*(*) opt
      logical       optval
      integer       optindex
      logical       opttab(27*26)
      common /OPTCOM/ opttab
      optval = opttab(optindex(opt))
      return
      end

c Convert characters to an array index
      integer function optindex(s)
      character*(*) s
      integer       i, j
      character*26  letters
      data          letters / 'abcdefghijklmnopqrstuvwxyz' /
      call lcase(s)
      i = index(letters,s(1:1))
      j = index(letters,s(4:4))
      if( s(1:1).eq.'i' ) j = index(letters,s(5:5))
      optindex = 26*i + j
      return
      end

c***********************************************************************

c Get parameters controlling type of fit made and interpretation of
c parameters for the user.

      subroutine inpgauss(spectra,prfinfo,rms,ngauss,limlist,cmpsort)

      logical       spectra
      integer       prfinfo(*)
      character*(*) rms
      integer       ngauss(*)
      real          limlist(*)
      real          cmpsort(*)

      logical       optval
      logical       keyprsnt

      integer       nchan
      integer       i, iostat
      character*6   key
      real          rmsest
      logical       ok
      character*20  sort
      real          nch
      real          dv
      logical       disper, pixel
      integer       a0,a1,am1
      data          a0/0/, a1/1/, am1/-1/
c     a0,a1,am1 keep flint quiet

      if( spectra ) then

c        Get maximum number of gaussian components from keyword
         nchan = prfinfo(2)
         call keyi( 'ngauss', ngauss(1), 0 )
         call assertl( nchan.gt.3*ngauss(1),
     *             'Not enough datapoints to fit this many parameters' )

c        Ask for expected rms; check if filename given
         call assertl( keyprsnt('rmsest'),
     *                 'It is necessary to use the rmsest keyword' )
         rmsest = -1.
         call keya( 'rmsest', rms, ' ' )
         call hopen( i, rms, 'old', iostat )
         if( iostat.ne.0 ) then
            call atorf( rms, rmsest, ok )
            call assertl(ok,
     *           'rms dataset does not exist, or other error in rms')
            rms = ' '
            limlist(20) = rmsest
         endif

c        Number of channels given by length of fitaxis
         nch = real(nchan)
      else

         rms = ' '
c        Maximum number of gaussians from GPAR axis
         nchan = prfinfo(3)
         ngauss(1) = (nchan-1)/6
         call assertl( 6*ngauss(1)+1.eq.nchan,
     *                 'Input dataset has wrong number of channels' )
c        Number of channels found from crval(naxis+1)
         call cooinfo( 4,a0,a1,nch )

      endif

c get smoothing factor
      call keyr( 'smooth', limlist(14), 1.0 )

c # initial estimates to make is ngauss from keyword
      ngauss(2) = ngauss(1)


c Find out what to sort on
      call keya( 'cmpsort', sort, 'velocity' )
      if( sort(1:2).eq.'ve' ) cmpsort(1) = 2.
      if( sort(1:2).eq.'in' ) cmpsort(1) = 3.
      if( sort(1:2).eq.'am' ) cmpsort(1) = 4.
      if( sort(1:2).eq.'wi' ) cmpsort(1) = 5.
      if( sort(1:2).eq.'vd' ) cmpsort(1) = 6.
      if( sort(1:2).eq.'vd' ) call keyr( 'cmpsort', cmpsort(2), 0. )
      if( sort(1:2).eq.'vr' ) cmpsort(1) = 7.
      if( sort(1:2).eq.'vr' ) call keyr( 'cmpsort', cmpsort(2), 0. )
      if( sort(1:2).eq.'vr' ) call keyr( 'cmpsort', cmpsort(3), 0. )
      call assertl(cmpsort(1).ne.0.,'Error in name of sort parameter')
c     convert center to pixels
      call cooinfo( 8, a1,a0, cmpsort(2) )
      call cooinfo( 8, a1,a0, cmpsort(3) )


c Get allowed parameter ranges
c limlist( 1: 3)=lower limits
c limlist( 4: 6)=upper limits
c limlist( 7: 9)=error cutoff
c limlist(10:12)=flag if limit used

c min/max amplitude set using cutoff keyword or lower=3*rms
      limlist( 1) = -1.E30
      limlist( 4) =  1.E30
      limlist( 7) =  3.
      limlist(10) =  1.
c min/max velocity = channel range
c maximum velocity error is 1/2 fwhm
      limlist( 2)  = 1.
      call cooinfo( 8, am1,a0, limlist(2) )
      limlist( 5)  = nch
      call cooinfo( 8, am1,a0, limlist(5) )
      limlist( 8) = 0.5 * 2.35482004503094930
      limlist(11) = 1.
c min/max width = 0.85 to half # channels
      call cooinfo( 3, a0,a0, dv )
      limlist( 3) =     0.85 * dv
      limlist( 6) = nch / 2. * dv
      limlist( 9) = 1.
      limlist(12) = 1.

c check keyword values
      do i = 1, 3
         if( i.eq.1 ) key = 'cutoff'
         if( i.eq.2 ) key = 'crange'
         if( i.eq.3 ) key = 'wrange'
         if( keyprsnt(key) ) then
            limlist(i+9) = 1.
            call keyr( key, limlist(i),   limlist(i)   )
            call keyr( key, limlist(i+3), limlist(i+3) )
            call keyr( key, limlist(i+6), limlist(i+6) )
            call assertl( limlist(i+3).ge.limlist(i),
     *                    'Upper cutoff must be above lower cutoff' )
         endif
      enddo


c set default cutoff if not given
      if( limlist(10).eq.0. ) then
         limlist( 1) = 3.0
         limlist(10) = 1.
      endif
      call optlist('negative',optval)
      if( optval ) limlist(10) = 2.

c limlist(2,5) corresponds to velocity; sometimes cdelt3<0, and the
c pixel order of lower and upper limit is reversed
c      if( limlist(2) .gt. limlist(5) ) then
c         tmp        = limlist(2)
c         limlist(2) = limlist(5)
c         limlist(5) = tmp
c      endif

c convert allowed velocity and width ranges to pixels
c make sure conversions are done
c extract from 'call parconv( limlist, -2, .true. )'
c don't call, as parconv also changes errors, and those elements have
c higher indices than the length of limlist
      call optlist('dispersion',disper)
      call optlist('pixel',     pixel )
      if(.not.disper) then
         limlist(3) = limlist(3)/2.35482004503094930
         limlist(6) = limlist(6)/2.35482004503094930
      endif
      if(.not.pixel ) then
         call cooinfo( 8, a1,a0, limlist(2) )
         call cooinfo( 8, a1,a0, limlist(5) )
         limlist(3) = limlist(3)/dv
         limlist(6) = limlist(6)/dv
      endif
      limlist(3) = 1/limlist(3)
      limlist(6) = 1/limlist(6)

c limlist(13) gives cutoff in intensity when summing/averaging
      call keyr( 'cutval', limlist(13), -1.E30 )

c limlist(21) gives factor by which amplitude of initial estimate may be
c lower than amplitude cutoff
      limlist(21) = 0.9
c limlist(22) gives factor such that if amplitude of initial estimate is that
c many times higher than rms, the fwhm is found from the second derivative
      limlist(22) = 8.0
c limlist(23) gives factor to multiply initial estimates by if a second try
c is made for them
      limlist(23) = 0.95
c limlist(24) gives factor by which rms may be higher than rmsest after fit
      limlist(24) = 5.0
c limlist(25) gives factor by which rms may be higher than rmsest when that
c             is the only reason that a fit failed
      limlist(25) = 1.8

      return
      end

c************************************************************************
c************************************************************************
c************************************************************************
c************************************************************************
c************************************************************************

c work loops over all profiles, reads each profile, applies the
c gaussfitting algorithm, and writes the results.

      subroutine work( units,prfinfo,runs,ngauss,limlist,cmpsort,prnm )

      integer   MAXRUNS
      parameter ( MAXRUNS = 1024 )
      integer   units(*)
      integer   prfinfo(*)
      integer   runs(3,MAXRUNS)
      integer   ngauss(*)
      real      limlist(*)
      real      cmpsort(*)
      character*80 prnm

      logical   optval1, optval2, optval3
      integer   gssfit, gssset
      include   'maxdim.h'
      integer   MAXCMP, ARRSIZ
      parameter ( MAXCMP=10, ARRSIZ=7*MAXCMP+1 )

      real      gausspar(ARRSIZ)
      real      gaussest(ARRSIZ)

      integer   profnr, runnr
      real      rprofnr, flag
c     integer   xmin,xmax, ymin,ymax
      integer   nchan
      real      data(MAXDIM), rmsest
      logical   mask(MAXDIM)
      real      limlst(25)
      integer   coords(2)
      logical   inbox, somecmp, sumit, varrms
      integer   ier, pass
      integer   i

      nchan  = prfinfo(2)
c     xmin   = prfinfo(5)
c     ymin   = prfinfo(6)
c     xmax   = prfinfo(8)
c     ymax   = prfinfo(9)
      runnr  = 1

c one-step check on summing, instead of two function calls
      call optlist('aver',  optval1)
      call optlist('summed',optval2)
      sumit  = optval1 .or. optval2

c call setrms first time to set limlst; called for each pixel if varrms true
      varrms = units(6).ne.0
      call setrms( 0,profnr,limlist,limlst,rmsest )

c print output header
      call optlist('estimout', optval1)
      call optlist('intermout',optval2)
      call optlist('noprt',    optval3)
      if( optval1 .or. optval2 .or. .not.optval3 ) call header(units(1))

c loop over all profiles
      do profnr = 1, prfinfo(1)

c which position?
         rprofnr = real(profnr)
         call cooinfo( 6, units(1), units(2), rprofnr )
         flag=0.
         call cooinfo( 7, coords(1), coords(2), flag )
c is this inside the selected region?
         inbox = ( coords(2).eq.runs(1,runnr) .and.
     *             coords(1).ge.runs(2,runnr) .and.
     *             coords(1).le.runs(3,runnr)     ) .or. runs(1,1).eq.0

         if( inbox .and. coords(1).eq.runs(3,runnr) ) runnr = runnr+1
c inside region and in= given => read spectrum
c    and unselect channels outside region=image range
         if(      units(1).ne.0 .and. inbox ) then
            call xyzprfrd( units(1), profnr, data,mask, prfinfo(2) )
            do i = 1, prfinfo(7)-1
               mask(i) = .false.
            enddo
            do i = prfinfo(10)+1, nchan
               mask(i) = .false.
            enddo
            if( sumit ) then
            do i = 1, nchan
               mask(i) = data(i) .gt. limlist(13)
            enddo
            endif
c outside region or no in=, but parinp= given => read gaussians
         else if( units(2).ne.0 ) then
            call xyzprfrd( units(2), profnr, data,mask, prfinfo(3) )

c set inbox false if all read-in comps are zero to avoid doing too much
            if( inbox ) then
               somecmp = .false.
               do i = 1, 3*ngauss(5), 3
                  if( data(i).ne.0. ) somecmp = .true.
               enddo
               if( .not.somecmp ) inbox = .false.
            endif
         endif

c rms from dataset or single value?
         if(varrms) call setrms( units(6),profnr,limlist,limlst,rmsest )

c add profiles together if options=summed or average used.
c inbox comes out false, except on very last profile
         if(sumit) call avgsum(data,mask,profnr,prfinfo,inbox)

c smooth profile if wanted
         if( limlist(14) .ne. 1 ) then
            
         endif

c call gssout with type=-1 to initialize
c some fake variables and arrays in call, so that the compiler is not confused
         ier=0
         if(inbox) call gssout( -1,  ier,data,nchan,data )

         pass=1
         do while( pass.le.2 )

c obtain gaussian parameters at this position
c    spectrum read and in region => make initial estimate and fit
c    no spectra to read or out of box, but parinp given => use gaussian
c then print final result at this position
c check if second pass must be made:
c    ier can be >0 if automatic estimates are made and pass=1
c    try up to 2 times to make initial estimates
c    if pass=2, gssest will make different estimates than for pass=1
c    fit failed completely -> if first pass, redo initial estimates
c    ok if failure because parameter outside limits or second pass or sucess

            ier=0
            if(      units(1).ne.0 .and. inbox ) then
               call gssest(data,mask,nchan, gaussest,         ngauss,
     *                                      limlst,  cmpsort,  pass   )
               ier= gssfit(data,mask,nchan, gaussest,gausspar,ngauss,
     *                                      limlst,  cmpsort          )
            else if( units(2).ne.0 ) then
               ier= gssset(data,                     gausspar,ngauss,
     *                                      limlst,  cmpsort, inbox   )
            endif

            if(inbox) call gssout( 3, ier, gausspar,ngauss, cmpsort )

            if(     pass.eq.1 ) then
               pass=3
               call optlist('findestim',optval1)
               if( ier.gt.0 .and. optval1 ) pass=2
            else if( pass.eq.2 ) then
               pass=3
               if( ier.gt.0 ) then
                  call wrtout('Really cannot fit this one',1)
                  ngauss(4)=0
               endif
            endif
         enddo
         call writfile( units, profnr, inbox,cmpsort,
     *                  data,mask,nchan,gaussest,gausspar,ngauss,prnm )
      enddo
      return
      end

c***********************************************************************

      subroutine setrms( units, profnr, limlist, limlst, rmsest )
      integer units, profnr
      real    limlist(*), limlst(*)
      real    rmsest
      logical rmsmsk
      logical first
      save    first
      data    first / .true. /

      if( units.ne.0 ) then
         call xyzpixrd( units, profnr, rmsest, rmsmsk )
      else
         rmsest = limlist(20)
      endif
      if( first ) then
c 1,2,3 = minimum amp/vel/disp
c 4,5,6 = maximum amp/vel/disp
c 7,8,9 = error cutoff amp/vel/disp
c 10,11,12 = flag to say if limit used
c 13    = intensity cutoff
c 14    = smoothing
c 20    = rms est
c 21    = factor by which amplitude of initial estimate may be lower
c         than cutoff
c 22    = gives factor such that if amplitude of initial estimate is that
c         many times higher than rms, the fwhm is found from the second
c         derivative
c 23    = factor to multiply initial estimates by if a second try is made
c 24    = factor by which rms may be higher than rmsest after fit
c 25    = factor by which rms may be higher than rmsest when that is the
c         only reason that a fit failed
         limlst( 1) = limlist( 1) * rmsest
         limlst( 2) = limlist( 2)
         limlst( 3) = limlist( 3)
         limlst( 4) = limlist( 4)
         limlst( 5) = limlist( 5)
         limlst( 6) = limlist( 6)
         limlst( 7) = limlist( 7)
         limlst( 8) = limlist( 8)
         limlst( 9) = limlist( 9)
         limlst(10) = limlist(10)
         limlst(11) = limlist(11)
         limlst(12) = limlist(12)
         limlst(14) = limlist(14)
         limlst(23) = limlist(23)
         first = .false.
      endif
         limlst(20) =               rmsest
         limlst(21) = limlist(21) * rmsest
         limlst(22) = limlist(22) * rmsest
         limlst(24) = limlist(24) * rmsest
         limlst(25) = limlist(25) * rmsest
      return
      end

c***********************************************************************

      subroutine writfile( units, profnr,  inbox,cmpsort,
     *               data,mask,nchan, gaussest,gausspar,ngauss, prnm )
      integer   units(*)
      integer   profnr
      logical   inbox
      real      cmpsort(*)
      real      data(*)
      logical   mask(*)
      integer   nchan
      real      gaussest(*), gausspar(*)
      integer   ngauss(*)
      character*80 prnm

      logical   optval
      integer   cmpind, errind, rmsind
      include   'maxdim.h'

      integer   i, j, jerr, k, ngss
      real      estim(MAXDIM), model(MAXDIM), residual(MAXDIM)
      logical   stil2clc
      stil2clc = .true.

c write profile, model, residual and estimatemodel to an ASCII file
      call optlist('wrprof',optval)
      if( optval .and. inbox ) then
         stil2clc = .false.
         call gaussmod(estim,nchan,gaussest,ngauss(3))
         call gaussmod(model,nchan,gausspar,ngauss(4))
         call optlist('pixel',optval)
         call writprof(  data,model,estim,nchan,optval,
     *                   gausspar,ngauss(4), prnm )
      endif

c write model, residual to output datasets
      if( units(3).ne.0 .or. units(4).ne.0 ) then
         if( inbox ) then
            if( stil2clc )
     *        call gaussmod(model,nchan,gausspar,ngauss(4))
            do i = 1, nchan
               residual(i) = data(i) - model(i)
               mask(i)     = .false.
            enddo
         else
            do i = 1, nchan
               model(i)    = 0.
               residual(i) = 0.
               mask(i)     = .false.
            enddo
         endif
         if( units(3).ne.0 )
     *      call xyzprfwr( units(3),profnr,model,   mask,nchan )
         if( units(4).ne.0 )
     *      call xyzprfwr( units(4),profnr,residual,mask,nchan )
      endif

c write result to parameter file
      if( units(5).ne.0 ) then

c possibilities:
c no components present => zero all
c inside box            => ngauss(4) = # comps from gssfit or gssset
c outside box           => ngauss(5) = # comps read in (0 if no parinp=)
         if(      inbox ) ngss = ngauss(4)
         if( .not.inbox ) ngss = ngauss(5)

c sort components, if fit are being made or (if parinp= only) if pixel was inbox
c cmpsort(4)<0 will insert zero components if sorting so requires
         if( units(1).ne.0 .or. inbox ) then
            cmpsort(4) = -real(ngauss(6))
            call gsssrt(  data, gausspar, ngss, cmpsort )
         else
            cmpsort(4) = 0.
            call setngsa( data, gausspar, ngss )
         endif
         call parconv( data, ngss, .false. )
         do i = ngss+1, ngauss(6)
            call setgssz( data,i )
         enddo

c move errors to proper position in array
         k = 3*ngauss(6)
         do i = 1, ngauss(6)
            j    = cmpind(i)
            jerr = errind(i)
            gausspar(  j  ) = data(j     )
            gausspar(  j+1) = data(j   +1)
            gausspar(  j+2) = data(j   +2)
            gausspar(k+j  ) = data(jerr  )
            gausspar(k+j+1) = data(jerr+1)
            gausspar(k+j+2) = data(jerr+2)
         enddo
c include rms
         if( ngss.eq.0 ) gausspar(rmsind()) = 0.
         gausspar( 6*ngauss(6)+1 ) = gausspar(rmsind())
c set mask
         do i = 1, 6*ngauss(6)+1
            mask(i) = gausspar(i) .ne. 0.
         enddo

c at last, write it
         call xyzprfwr( units(5),profnr,gausspar,mask,6*ngauss(6)+1)

      endif

      return
      end


c***********************************************************************

c If average: average all profiles, and fit only last one, else do nothing.
c If summed:  sum all profiles, and fit only last one, else do nothing.
c inbox is set to false, except for very last profile

      subroutine avgsum(data,mask,profnr,prfinfo,inbox)

      real      data(*)
      logical   mask(*)
      integer   profnr
      integer   prfinfo(*)
      logical   inbox

      include  'maxdim.h'
      logical   optval1, optval2

      real      accum(MAXDIM)
      save      accum
      integer   i
      integer   nchan, nprofs

      nprofs = prfinfo(1)
      nchan  = prfinfo(2)
      if( profnr.eq.1 ) then
         do i = 1, nchan
            accum(i) = 0.
         enddo
      endif
      if( inbox ) then
         do i = 1, nchan
            if(mask(i)) accum(i) = accum(i) + data(i)
         enddo
      endif
      if( profnr.eq.nprofs ) then
         call optlist('aver',  optval1)
         call optlist('summed',optval2)
         do i = 1, nchan
            if( optval1 ) data(i) = accum(i) / nprofs
            if( optval2 ) data(i) = accum(i)
         enddo
         inbox = .true.
      else
         inbox = .false.
      endif

      return
      end

c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************
c************************************************************************

c convert data array to gaussian parameters.
c if also inbox=true, then apply selection criteria and sort
c
c conversion to internal pixel representation is done so that gssout
c can produce proper output

      integer function gssset(
     *        data, gausspar,ngauss,limlist,cmpsort, inbox )

      real      data(*)
      real      gausspar(*)
      integer   ngauss(*)
      real      limlist(*)
      real      cmpsort(*)
      logical   inbox

      integer   ier
      integer   cmpind, erroff, errpar, rmsind
      integer   i, joff
      integer   cutoffs, delgauss
      logical   somecmp

c ngauss(5) = # gaussians in parinp dataset
      somecmp = .false.
      joff    = 3*ngauss(5)
      do i = 1, 3*ngauss(5)
         gausspar(i         ) = data(i     )
         gausspar(i+erroff()) = data(i+joff)
         if( data(i).ne.0. ) somecmp = .true.
      enddo
      gausspar(rmsind()) = data(6*ngauss(5)+1)
c     convert to internal values
      call parconv( gausspar,ngauss(5), .true. )

c if in= given, inbox will be false here: don't do anything, just write it out
c if in= not given, inbox may be true or false. If false: just write it,
c        if true and components present: delete zero components, apply
c                    selection criteria and sort (in writfile)

      ier=0
c if not inbox or all zeroes => no manipulations
      if( inbox .and. somecmp ) then
c        set errpar flag for zero-components
         do i = 1, ngauss(5)
            if( gausspar(cmpind(i)).ne.0. ) gausspar(errpar(i))=1.
            if( gausspar(cmpind(i)).eq.0. ) gausspar(errpar(i))=2.
         enddo
c        delete zero-components
         ngauss(4) = delgauss(gausspar,ngauss(5))
         ngauss(3) = ngauss(4)
         ier=0
         call gssout( 0,ier, gausspar,ngauss, cmpsort )
c        apply cutoffs
         ier = cutoffs( gausspar,ngauss(4), limlist )
         call gssout( 2,ier, gausspar,ngauss, cmpsort )
c        delete components outside limits
         ngauss(4) = delgauss(gausspar,ngauss(4))
      else
c ngauss(4) is used to write it out
         ngauss(4) = ngauss(5)
      endif

      gssset=ier
      return
      end

c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************
c************************************************************************

      subroutine gssest( data,mask,nchan, gaussest,ngauss,
     *                   limlist, cmpsort, pass )

      real      data(*)
      logical   mask(*)
      integer   nchan
      real      gaussest(*)
      integer   ngauss(*)
      real      limlist(*)
      real      cmpsort(*)
      integer   pass

      logical   optval
      integer   rmsind, rdestim, fndestim
      logical   chkprof
      real      getrms
      integer   ier
      integer   MAXCMP, ARRSIZ
      parameter ( MAXCMP=10, ARRSIZ=7*MAXCMP+1 )
      real      gpar(ARRSIZ), cs

c profile fittable?
      if( chkprof(data,mask,nchan,ngauss(1)) ) then

c obtain estimate, either automatically or from keyword
         call optlist('findestim',optval)
         if( optval) then
            ngauss(3) = fndestim( gaussest,ngauss(2),
     *                            data,mask,nchan, limlist, pass )
         else
            ngauss(3) = rdestim( gaussest,ngauss(2) )
         endif
         gaussest(rmsind()) =
     *            getrms(data,mask,nchan,gaussest,ngauss(3),0.)

c sort on amplitude, so that 'fit with one-component less' works best
         cs=cmpsort(1)
         cmpsort(1)=4.
         call gsssrt(  gpar, gaussest, ngauss(3), cmpsort )
         cmpsort(1)=cs
         call setngsa( gaussest, gpar, ngauss(3) )

                                ier=0
         if( ngauss(3) .eq. 0 ) ier=7
      else
         ngauss(3) = 0
         ier = 5
      endif

c print estimates
      call gssout( 0,ier, gaussest,ngauss, cmpsort )

      return
      end


c***********************************************************************

      logical function chkprof(data,mask,nchan,ngauss)

      real    data(*)
      logical mask(*)
      integer nchan
      integer ngauss

      logical optval
      integer nok, i
      chkprof = .true.

c Enough points to make a fit?
      call optlist('inmask',optval)
      if( optval ) then
         nok = 0
         do i = 1, nchan
            if( mask(i) ) then
               if( data(i).ne.data(1) ) nok = nok + 1
            else
               data(i) = 0.
            endif
         enddo
         if( nok .lt. 5*ngauss ) chkprof=.false.
      endif

      return
      end

c************************************************************************

      integer function rdestim( gaussest,ngauss )

      real      gaussest(*)
      integer   ngauss

      logical   keyprsnt
      integer   MAXCMP, ARRSIZ
      parameter ( MAXCMP=10, ARRSIZ=7*MAXCMP+1 )

      real      estim(ARRSIZ)
      logical   first
      integer   i, j, cmpind
      save      first, estim
      data      first / .TRUE. /

      if( first ) then

         first = .false.
         call assertl( keyprsnt('estim'),
     *        'It is necessary to use estim= or options=findestim' )
         do i = 1, ngauss
            j = cmpind(i)
            call keyr( 'estim', estim(j  ), 1. )
            call keyr( 'estim', estim(j+1), 1. )
            call keyr( 'estim', estim(j+2), 1. )
         enddo

         call parconv( estim, -ngauss, .true. )

      endif

      call setngsa( gaussest, estim, ngauss )

      rdestim = ngauss
      return
      end



      integer function fndestim( gaussest,ngauss, data,mask,nchan,
     *                           limlist, pass )

      real      gaussest(*)
      integer   ngauss
      real      data(*)
      logical   mask(*)
      integer   nchan
      real      limlist(*)
      integer   pass

      include  'maxdim.h'
      real      dat(MAXDIM), zdat(MAXDIM)
      integer   ngss, gssnum, mergecmp
      real      amp, vel, wid
      logical   estok, estcomp

      integer   i

      integer   test(6)
      save      test
      data      test / -1, 0,0,0,0,0 /

c     Second try for initial estimates.
      if( pass.eq.2 ) call wrtout('Try different initial estimates',1)

c     test(1)=1: follow the process
c     test(1)=2: also print data before and after
c     test(1)=3: also print d2data
c     test(1)=4: make files test.wip
c     test(2),test(3): channel range selection when printing data
      if( test(1) .eq. -1 ) then
         call keyi( 'test', test(1), 0 )
         call keyi( 'test', test(2), 1 )
         call keyi( 'test', test(3), nchan )
         call keyi( 'test', test(5), 2 )
         if( test(1).eq.4 ) call testprof(test,0,data,nchan)
      endif
      if(test(1).gt.0)
     *write(*,*)'==================================',ngauss

c work with dat array = data - subtracted gaussians
c also use zdat: dat with false component regions set to zero
      do i = 1, nchan
                             dat(i) = data(i)
         if(      mask(i) ) zdat(i) = data(i)
         if( .not.mask(i) ) zdat(i) = 0.
      enddo
      if(test(1).gt.1) call testprof(test,1,zdat,0)

c loop until maximum below cutoff, or max # components found
      ngss  = 0
      estok = .TRUE.
c still more to find? (estok false if current max below limit)
      do while( estok .and. ngss.lt.abs(ngauss) )

c find velocity and amplitude
         estok=estcomp( dat,zdat,nchan,limlist,test,amp,vel,wid )
         if( estok ) then

            if( pass.eq.2 ) then
               amp = limlist(23)*amp
               wid = limlist(23)*wid
            endif

c check if this component closer than sumdisp/2 to a previous one
            gssnum=mergecmp(gaussest,ngss, dat,nchan, amp,vel,wid,test)

c subtract found gaussian from profile
            do i = max(    1,nint(vel-3*wid)),
     *             min(nchan,nint(vel+3*wid))
                dat(i) = dat(i) - amp * exp( -0.5*( (i-vel)/wid )**2 )
               if( mask(i) ) zdat(i) = dat(i)
            enddo

c insert found gaussian in estimates array
            call setgssv( gaussest,gssnum, amp, vel, 1./wid )
            if(test(1).gt.0)
       *    write(*,*)'gauss#',gssnum,amp,vel,wid,wid*2.35482

         endif
         if(test(1).gt.0) write(*,*)'------------------------------'
         if(test(1).gt.1) call testprof(test,2,zdat,gssnum)

      enddo
      call testprof(test,3,zdat,nchan)

      fndestim = ngss
      return
      end


      subroutine testprof(test,mode,data,ival)
      integer test(*)
      integer mode
      real    data(*)
      integer ival

      integer unit, i, cnt, lcnt, nl, gssnum(20)
      save    unit, cnt, gssnum
      
      if( test(1).eq.4 .and. mode.eq.0 ) then
         open(unit=unit,file='test.wip',status='new')
         cnt = 0
         write(unit,'(''define testdata'')')
      else if( mode.eq.1 .or. mode.eq.2 ) then
         if(mode.eq.1) write(*,'(''Original profile'')')
         if(mode.eq.2) write(*,'(''Residual'')')
         do i = test(2), test(3)
            write(*,*)i,data(i)
            if(test(1).eq.4)
     *         write(unit,'(''echo '',i4,1x,1pe10.3)') i,data(i)
         enddo
         cnt = cnt + 1
         gssnum(cnt) = ival
      else if( test(1).eq.4 .and. mode.eq.3 ) then
         write(unit,'(''end'')')
         write(unit,'(''newplot 1'')')
         if( cnt.le. 6 ) write(unit,'(''setpanel 3 2 1'')')
         if( cnt.le.12 ) write(unit,'(''setpanel 4 3 1'')')
         write(unit,'(''setinsidelabel 00 0.05 0.90'')')
         write(unit,'(''rlimits 1 '',i4,'' -0.2 1'')') ival
         lcnt = 2
         nl   = test(3) - test(2)
         do i = 1, cnt
            write(unit,'(''figpanel 0 1 velocity amplitude'')')
            write(unit,'(''insidelabel 0 gauss '',i4)') gssnum(i)
            write(unit,'(''lineplot test.wip '',i4,1x,i4,'' 2 3 '')')
     *             lcnt,lcnt+nl
            lcnt = lcnt + nl + 1
         enddo
         write(unit,'(''cursor'')')
         close(unit)
      endif
      return
      end


c if maximum very high: use zeroes of 2nd deriv to determine dispersion
c else: use estimate of integral
      logical function estcomp(
     *        data,zdat,nchan,limlist,test,amp,vel,wid)

      real      data(*), zdat(*)
      integer   nchan
      real      limlist(*)
      integer   test(*)
      real      amp, vel, wid

      logical   done
      integer   im
      real      maxdat

      done     = .FALSE.
      maxdat   = 1.E30
      estcomp = .FALSE.

c loop until good component found or max below limit
      do while( .not. done )

c        find position of maximum, until it is a possible gaussian peak,
c        rather than noise
         do while( .not.done  .and.  maxdat.gt.limlist(21) )
            call getmax( zdat,nchan, maxdat, im )
            if(test(1).gt.0)
      *        write(*,*)'max @',im,' =',maxdat,limlist(21)
            done=zdat(im-1)/zdat(im).gt..3.and.zdat(im+1)/zdat(im).gt..3
            if( .not.done ) then
               if(test(1).gt.0)write(*,*)'false maximum @',im
               zdat(im) = 0.
            endif
         enddo

         if( maxdat .gt. limlist(21) ) then
c           peak OK: estimate amp and vel
            call estmax(   data,nchan,maxdat,im,  test, amp,vel     )
            call estwidth( data,nchan,limlist,test, amp,vel,wid )
            done = wid.ge.0.85  .and.  wid.le.nchan
            if( done ) then
               estcomp = .true.
            else
               if(test(1).gt.0)
      *              write(*,*)'component too narrow/wide @',im
               zdat(im) = 0.
            endif
         else
c           peak too low: quit loop with estcomp still .false.
            done = .true.
         endif

      enddo

      return
      end


c find position and amplitude of maximum
      subroutine estmax( data,nchan, maxdat,im, test, amp,vel )

      real      data(*)
      integer   nchan
      real      maxdat
      integer   im
      integer   test(*)
      real      amp, vel
      real      alog

      real      x, w, vw, sv
      integer   j

c find distance-weighted position of center within +- 2 pixels of max
c weight from [data/maxdat] x = = exp(-x^2/2s^2) -> x/s = sqrt(-2*x)
c -> weight (1-x/s)   for velocity
c data(j) can be >maxdat if there was an error found for the real maxdat
c (e.g. a profile like 0.7 0.75 0.74 0.78 0.74 will lead to a deriv sign
c change for max=0.78 at x=0, and a too-small width; then a new max is found
c at 0.75).
      vw = 0.
      sv = 0.
      do j = max(1,im-2), min(nchan,im+2)
         x = min(maxdat,data(j)) / maxdat
         if( x .gt. 0.65 ) then
            w  = 1. - sqrt( -2. * alog(x) )
            vw = vw + w *     j
            sv = sv + w
         endif
      enddo
      amp = maxdat
      vel = vw / sv
      if(test(1).gt.0)write(*,*)'amp est=',amp
      if(test(1).gt.0)write(*,*)'vel est=',vel,nint(vel),
     *                                               '(=',vw,'/',sv,')'
      if( vel.lt.4. .or. vel.gt.nchan-4. ) then
         amp=0.
         return
      endif

      return
      end


      subroutine estwidth( data,nchan, limlist,test,amp,vel,wid )

      real      data(*)
      integer   nchan
      real      limlist(*)
      integer   test(*)
      real      amp, vel, wid

      logical   derivat

      integer   i,  im
      logical   done
      real      tekenr

      include   'maxdim.h'
      real      ddata(MAXDIM), d2data(MAXDIM)
      integer   d1, d2, a1, a2, i1, i2
      real      w1, w2, w3, w4

      real      Imin, Iplus
      if( test(1).gt.2 ) write(*,*)'Find width from profile'

      im = nint(vel)
      derivat = amp.ge.limlist(22)

      if( derivat ) then
         ddata(2) = data(2) - data(1)
         do i = 2, nchan-1
             ddata(i) =  data(i+1) -  data(i  )
            d2data(i) = ddata(i  ) - ddata(i-1)
            if(test(1).gt.2.and.i.gt.test(2).and.i.lt.test(3))
     *            write(*,*)i,data(i),ddata(i),d2data(i)
         enddo
      else
         if( test(1).gt.2 ) then
            do i = 2, nchan-1
               if(i.gt.test(2).and.i.lt.test(3)) write(*,*)i,data(i)
            enddo
         endif
      endif

      d1    = -1
      d2    = -1
      a1    = -1
      a2    = -1
      i1    = -1
      i2    = -1
      Imin  =  0.
      Iplus =  0.
      i     =  0
      done = .false.
      do while( i.lt.min(im-1,nchan-im-1) .and. .not.done )
         if( derivat ) then
         if( d1.lt.0 .and. tekenr(d2data(im-i)) .gt. 0.      ) d1=i-1
         if( d2.lt.0 .and. tekenr(d2data(im+i)) .gt. 0.      ) d2=i-1
         if( a1.lt.0 .and.          data(im-i)  .lt. 0.5*amp ) a1=i-1
         if( a2.lt.0 .and.          data(im+i)  .lt. 0.5*amp ) a2=i-1
         done = d1.ge.0 .and. d2.ge.0 .and. a1.ge.0 .and. a2.ge.0
         else
         if( i1.lt.0 .and. tekenr(  data(im-i)) .ne. tekenr(amp))i1=i
         if( i1.lt.0 ) Imin  = Imin  + data(im-i)
         if( i2.lt.0 .and. tekenr(  data(im+i)) .ne. tekenr(amp))i2=i
         if( i2.lt.0 ) Iplus = Iplus + data(im+i)
         done = i1.ge.0 .and. i2.ge.0
         endif
         i = i + 1
      enddo

      if( derivat ) then
c        find fractional pixel width for dispersion; correct for center shift
         w1 = d1+ d2data(im-d1)/(d2data(im-d1)-d2data(im-d1-1)) + vel-im
         w2 = d2+ d2data(im+d2)/(d2data(im+d2)-d2data(im+d2+1)) + im-vel
         w1  = 2.35482*w1
         w2  = 2.35482*w2
c        d1,d2 give fwhm pixel according to change of change of 2nd deriv
c        a1,a2 give hwhm pixel according to reaching max/2
c        all equal        -> average from derivats
c        left  consistent -> left  derivat
c        right consistent -> right derivat
c        a's   consistent -> average from max/2
c        other            -> average of most consistent side
         if(      2*a1.eq.int(w1) .and. 2*a2.eq.int(w2) ) then
            wid = (w1+w2)/2.
         else if( 2*a1.eq.int(w1) ) then
            wid = w1
         else if( 2*a2.eq.int(w2) ) then
            wid = w2
         else
            w3 = a1 + (data(im-a1)-0.5*amp)/(data(im-a1)-data(im-a1-1))
            w4 = a2 + (data(im+a2)-0.5*amp)/(data(im+a2)-data(im+a2+1))
            w3 = (w3+vel-im)*2.
            w4 = (w4+im-vel)*2.
c            if( a1.eq.a2 ) then
                wid = (w3+w4)/2.
c            else if(  abs(w1-w3) .lt. abs(w2-w4)  ) then
c               wid = (w1+w3)/2.
c            else
c               wid = (w2+w4)/2.
c            endif
         endif
         if(test(1).gt.0)
     *   write(*,'('' wid est='',f7.2,
     *             ''  lft d,a'',i2,i2,''-> '',f7.2,f7.2,
     *             ''  rht d,a'',i2,i2,''-> '',f7.2,f7.2  )')
     *             wid,  d1,a1,w1,w3,  d2,a2,w2,w4
         wid = wid / 2.35482
      else
c dispersion found from sigma=fwhm/2.35; fwhm=(2*half-integral)/amp/1.064
         wid = 2. * min(Imin,Iplus) / amp / 1.064 / 2.35482
         if(test(1).gt.0)
     *   write(*,*)'wid est=',wid,wid*2.35482,' I-/+=',Imin,Iplus
      endif

      return
      end

c is component less than sumfwhm/2 = sumdisp*2.35/2 from another one?
c if so: merge the two components, don't add one to ngss
c don't merge more than 3 times for a component # to avoid getting into
c an infinite loop if the profile is difficult in just the right way
c (first add the old component back in)
c return the component number of the resulting gaussian
      integer function mergecmp(
     *        gaussest,ngauss, data,nchan, amp,vel,wid, test )

      real      gaussest(*)
      integer   ngauss
      real      data(*)
      integer   nchan
      real      amp,  vel,  wid
      integer   test(*)

      integer   MAXCMP, ARRSIZ
      parameter ( MAXCMP=10, ARRSIZ=7*MAXCMP+1 )

      integer   nmerge(MAXCMP), mergenum
      save      nmerge
      real      ampO, velO, widO
      integer   i, j

      if( ngauss.eq.0 ) then
         do i = 1, MAXCMP
            nmerge(i)=0
         enddo
      endif

      mergenum = 0
      do j = 1, ngauss
      if( mergenum.eq.0 ) then
         ampO =    gaussest(3*j-2)
         velO =    gaussest(3*j-1)
         widO = 1./gaussest(3*j-0)
         if(test(1).gt.0)write(*,*)abs(vel-velO),'<',(wid+widO)*1.00,'?'
         if( (abs(vel-velO) .lt. ((wid+widO)*1.00) ) .and.
     *       (abs(ampO/amp) .lt. 6.                ) .and.
     *       (nmerge(j)     .lt. 3                 )        ) then
            if(test(1).gt.0)write(*,*)'merge',amp,vel,wid
            if(test(1).gt.0)write(*,*)'     ',ampO,velO,widO
            do i = max(    1,nint(velO-3*widO)),
     *             min(nchan,nint(velO+3*widO))
               data(i) = data(i) + ampO*exp( -0.5*((i-velO)/widO )**2 )
            enddo
            vel = ( ampO*velO + amp*vel ) / ( ampO + amp  )
            wid = widO + abs(vel-velO)/2.35482
            amp = max( ampO, amp )
            mergenum=j
            nmerge(j) = nmerge(j) + 1
         endif
      endif
      enddo

      if( mergenum.eq.0 ) then
         ngauss   = ngauss + 1
         mergecmp = ngauss
      else
         mergecmp = mergenum         
      endif

      return
      end

c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************

c make gaussian fit, unless not asked for or not possible

      integer function gssfit( data,mask,nchan,
     *                         gaussest,gausspar,ngauss,
     *                         limlist,cmpsort )

      real      data(*)
      logical   mask(*)
      integer   nchan
      real      gaussest(*), gausspar(*)
      integer   ngauss(*)
      real      limlist(*)
      real      cmpsort(*)

      logical   optval
      integer   fitloop

      gssfit = 0
      call optlist('nofit',optval)
      if( optval ) then
c     Result = initial estimate
         call setngsa( gausspar, gaussest, ngauss(3) )
         ngauss(4) = ngauss(3)

      else if( ngauss(3).eq.0 ) then
c     No initial estimate present
         ngauss(4) = ngauss(3)

      else
c     Make a fit
      gssfit = fitloop( data,mask, nchan, gausspar,gaussest,ngauss,
     *                  limlist, cmpsort )
      endif

      return
      end


c************************************************************************

      integer function fitloop( data,mask,nchan,
     *                          gausspar,gaussest,ngauss,
     *                          limlist, cmpsort )

      real      data(*)
      logical   mask(*)
      integer   nchan
      real      gaussest(*), gausspar(*)
      integer   ngauss(*)
      real      limlist(*)
      real      cmpsort(*)

      logical   optval1, optval2
      real      getrms
      integer   rmsind
      logical   dofit, tryagain
      integer   dogssfit, cutoffs

      parameter ( MAXFIT = 30 )
      integer   fitnum, nfit(MAXFIT)
      integer   ier(MAXFIT)
      real      rms(MAXFIT)

      nfit(1)   = ngauss(3)
      fitnum    = 0
      dofit     = .true.

      do while( dofit )
c
         fitnum    = fitnum + 1
         ngauss(4) = nfit(fitnum)

c        do the actual fit, ier comes out >= 0
         ier(fitnum)=dogssfit( data,mask,nchan,
     *                         gaussest,gausspar,ngauss, limlist(20) )

c        fit converges, i.e. ier=0
         if( ier(fitnum) .eq. 0 ) then

c           apply cutoffs, if outside range, ier=-12 (only low-amp err) or -13
            ier(fitnum) = cutoffs( gausspar,ngauss(4), limlist )

c           subtract gaussian and find rms; give too-high-rms an ier value
            rms(fitnum)=getrms(data,mask,nchan,gausspar,ngauss(4),0.0)
            rms(MAXFIT)=getrms(data,mask,nchan,gausspar,ngauss(4),0.1)
            gausspar(rmsind()) = rms(fitnum)
            if( rms(fitnum) .gt. limlist(24) ) ier(fitnum)=-8

         endif

c print intermediate result
         call gssout( 2,ier(fitnum),gausspar,ngauss,cmpsort )

c do we have to and can we try again?
         dofit = tryagain( gaussest,gausspar,ngauss,
     *                     fitnum,nfit,ier,rms, limlist )

c redo fit -> have fiddled initial estimates
         if(dofit) then
            gaussest(rmsind())=
     *              getrms(data,mask,nchan,gaussest,ngauss(3),0.)
            call gssout( 1,0,gaussest,ngauss,cmpsort )
         endif

      enddo

c Warning if fit results very different from initial estimate
      call optlist('findestim',optval1)
      call optlist('intermout',optval2)
      if( optval1 .and. optval2 )
     *    call checkres(gausspar,gaussest,ngauss(4))

      fitloop = ier(fitnum)

      return
      end

c***********************************************************************

c dogssfit does the actual fitting, i.e. creates call to drvmrq

      integer function dogssfit(
     *        data,mask,nchan, gaussest,gausspar,ngauss, rmsest )

      real      data(*)
      logical   mask(*)
      integer   nchan
      real      gaussest(*)
      real      gausspar(*)
      integer   ngauss(*)
      real      rmsest

      integer   erroff
      external  gaussfun
      include   'maxdim.h'
      integer   MAXCMP, ARRSIZ
      parameter ( MAXCMP=10, ARRSIZ=7*MAXCMP+1 )

      logical   first
      integer   fitlist(3*MAXCMP), svfitlst(3*MAXCMP)
      real      x(MAXCHAN), sig(MAXCHAN)

      integer   i, k, npar, nfit

      real      fitdata(MAXCHAN)
      integer   nfitchan

      real      FTOL
      real      chisq
      integer   ier
      integer   MAXITER
      parameter ( FTOL=0.5, MAXITER=30 )

      save      first, svfitlst, x, sig
      data      first / .true. /

c Fill x and sig arrays, as drvmrq requires them
c Find and make copy of fitlist, which is changed deep down if not all are fit
      if( first ) then
         call initfit( fitlist,svfitlst,ngauss(1), mask,x,nchan )
         first = .false.
      endif

c Set error array
      do i = 1, nchan
         sig(i) = rmsest
      enddo

c Total number of parameters of gaussians
      npar = 3*ngauss(4)
c Set list of actual parameters to fit
      nfit = 0
      do i = 1, npar
         fitlist(i)  = svfitlst(i)
         if( fitlist(i).ne.0 ) nfit = nfit + 1
      enddo

c Copy initial estimate to fit array
      call setngsa( gausspar, gaussest, ngauss(4) )

c Copy selected channels
      nfitchan = 0
      do i = 1, nchan
         if( mask(i) ) then
            nfitchan = nfitchan + 1
            fitdata(nfitchan) = data(i)
         endif
      enddo

c Fit the gaussian
      call drvmrq( x, fitdata, sig, nfitchan,
     *             gausspar,npar, fitlist,nfit, FTOL, MAXITER,
     *             chisq, ier, gaussfun )

c Change convention for organization of gausspar by moving errors
      k = 3*ngauss(4)
      do i = 1, 3*ngauss(4)
         gausspar(i+erroff()) = gausspar(k+i)
      enddo
c Take absolute value of sigma, because drvmrq only cares about sigma^2
      do i = 1, ngauss(4)
         gausspar(3*i) = abs(gausspar(3*i))
      enddo

      dogssfit = ier
      return
      end



      subroutine initfit( fitlist,svfitlst,ngauss, mask,x,nchan )

      integer    fitlist(*), svfitlst(*)
      integer    ngauss
      logical    mask(*)
      real       x(*)
      integer    nchan
      
      logical    optval1, optval2
      integer    cmpind
      integer    i, j

      call optlist('fixvelo', optval1)
      call optlist('fixwidth',optval2)

c     Fill fitlist array with parameters to fit
      if( optval1 .or. optval2 ) then
         do i = 1, ngauss
                          fitlist(2*i-1) = cmpind(i)
            if( optval1 ) fitlist(2*i  ) = cmpind(i)+2
            if( optval2 ) fitlist(2*i  ) = cmpind(i)+1
         enddo
         do i = 2*ngauss+1, 3*ngauss
            fitlist(i) = 0
         enddo
      endif
      if( optval1 .and. optval2 ) then
         do i = 1, ngauss
            fitlist(i) = cmpind(i)
         enddo
         do i = ngauss+1, 3*ngauss
            fitlist(i) = 0
         enddo
      endif
      if( .not.optval1 .and. .not.optval2 ) then
         do i = 1, 3*ngauss
            fitlist(i) = i
         enddo
      endif

      do i = 1, 3*ngauss
         svfitlst(i) = fitlist(i)
      enddo

      j = 1
      do i = 1, nchan
         if( mask(i) ) then
            x(j) = real(i)
            j = j + 1
         endif
      enddo

      return
      end

c***********************************************************************

c Form gaussian model spectrum and residual, calculate rms of residual

      subroutine gaussmod( model,nchan,gausspar,ngauss )

      real    model(*)
      integer nchan
      real    gausspar(*)
      integer ngauss
      real    expon

      integer cmpind
      integer i, j, k

      do i = 1, nchan
         model(i)    = 0.
      enddo
      do k = 1, ngauss
         j = cmpind(k)
         do i = 1, nchan
            expon = 0.5*( (i-gausspar(j+1))*gausspar(j+2) )**2
            if( expon.lt.40. ) then
               model(i) = model(i) + gausspar(j) * exp(-expon)
            endif
         enddo
      enddo
      return
      end

      real function getrms( data,mask,nchan, gausspar,ngauss, cut )

      real    data(*)
      logical mask(*)
      integer nchan
      real    gausspar(*)
      integer ngauss
      real    cut

      include 'maxdim.h'
      real    model(MAXDIM)
      integer i, j, n
      real    mmax, rms
      if( ngauss.eq.0 ) return

      call gaussmod( model,nchan, gausspar,ngauss )

      call getmax( model,nchan, mmax, j )
      mmax = cut * mmax

      rms = 0.0
      n   = 0
      do i = 1, nchan
         if( mask(i) .and. (cut.eq.0.0  .or. model(i).ge.mmax) ) then
            rms = rms + ( data(i) - model(i) ) ** 2
            n   = n   + 1
         endif
      enddo

      if( n .le. 3*ngauss ) then
         getrms = 1.E30
      else
         getrms = sqrt( rms / (n - 3*ngauss) )
      endif

      return
      end

c***********************************************************************

c Apply cutoffs
c Amplitude should be high enough; default: more than 1 sigma.
c Velocity should be inside range; default: within profile.
c Width should be inside range; default: more than 0.5 pixel and
c less than length dispersion.; fitted 1/width^2 -> abs & reverse lt, gt

      integer function cutoffs( gausspar,ngauss, limlist )

      real      gausspar(*)
      integer   ngauss
      real      limlist(*)

      integer   cmpind, errind, errpar, rmsind

      integer   i, j, k, eflag
      real      a,w
      logical   err09, err10
      err09 = .false.
      err10 = .false.

      do i = 1, ngauss

         j = cmpind(i)
         k = errind(i)
         if( limlist(10).eq.1. ) a =     gausspar(j)
         if( limlist(10).eq.2. ) a = abs(gausspar(j))

                                                 eflag =          1
c        amp/rms<max
         if( gausspar(j  )/gausspar(k  ).lt.limlist(7) ) eflag=eflag*2
c        err(vel) * width > max verror
         if( gausspar(k+1)*gausspar(j+2).gt.limlist(8) ) eflag=eflag*3
         if( gausspar(k+1)*gausspar(j+2).gt.limlist(8) ) eflag=eflag*3
c width variable = 1/width here; call it s1
c        ds = ds1 / s1^2 => s/ds = 1/s1  * s1^2 / ds1 = s1/ds1
c        dwidth/width > max width error
         if( gausspar(k+2)/gausspar(j+2).gt.limlist(9) ) eflag=eflag*5
c        vel outside range
         if( gausspar(j+1)   .lt. limlist(2) .or.
     *       gausspar(j+1)   .gt. limlist(5)   ) eflag = eflag *  7
c        width outside range
         if( gausspar(j+2)   .gt. limlist(3)   ) eflag = eflag * 11
         if( gausspar(j+2)   .lt. limlist(6) .and.
     *       gausspar(j+2)   .ne. 0.           ) eflag = eflag * 13
c        amplitude too high
         if(        a        .gt. limlist(4)   ) eflag = eflag * 17
c        amplitude too low
         if(        a        .lt. limlist(1)   ) then
            if( limlist(10).eq.1. .and. a.ge.0 ) eflag = eflag * 19
            if( limlist(10).eq.1. .and. a.lt.0 ) eflag = eflag * 23
            if( limlist(10).eq.2.              ) eflag = eflag * 19
         endif

c        low amplitude components still ok if width reasonable
c        i.e. integral > 5*cut, but not too wide (w<15)
c        set eflag to -13 and cutoffs to -12; then 
c        a) component gets counted as OK one by tryagain
c        b) component gets accepted near end of tryagain
c        c) errout is called to print message # |-13| = 13
         if( eflag.eq.19 .or. eflag.eq.23 ) then
            err09 = .true.
            w = 1. / gausspar(j+2) * 2.35482
            if( a*w.gt.limlist(1)*5. .and. w.lt.15. ) eflag = -13
         else
            if( eflag.ne.1 ) err10=.true.
         endif

         gausspar(errpar(i)) = eflag
      enddo

c If err09 true: only error is that low-amplitude components occurred
c If err10 true: some other error (also) occurred
                  cutoffs=  0
      if( err09 ) cutoffs=-12
      if( err10 ) cutoffs=-13
      return
      end

c***********************************************************************

c Figure out if the fit was ok. If not, determine if another fit should
c be tried with different initial estimates or a different number of
c components.

      logical function tryagain( gaussest,gausspar,ngauss,
     *                           fitnum,nfit,ier,rms, limlist )

      real      gaussest(*), gausspar(*)
      integer   ngauss(*)
      integer   fitnum, nfit(*), ier(*)
      real      rms(*)
      real      limlist(*)

      logical   interout
      integer   cmpind, errpar

      integer   MAXFIT
      parameter (MAXFIT = 30)
      integer   try, fiddlenm(MAXFIT)
      save      try, fiddlenm

      integer   nOK(MAXFIT)
      integer   i, j, n, fn
      real      minrms
      logical   acclow, dellow
      integer   delgauss
      save      nOK

      real      fac1(8), fac2(8), fac3(8)
c fiddling: first change width to 80,120,60,180%, then change amp
c     orig            *0.9 *1.2 *0.6 *1.8
      data      fac1 / 1.0, 1.0, 1.0, 1.0, 0.8, 1.5, 0.5, 3.0 /
      data      fac2 / 0.0, 0.2, 0.0,-0.2, 0.0, 0.2, 0.0,-0.2  /
      data      fac3 / 0.8, 1.5, 0.5, 3.0, 0.55,1.0, 1.0, 1.0  /
c cumulative:                              0.8  1.2  0.6  1.8
c                           0.2       0.0       0.2       0.0
c                      0.8  1.2  0.6  1.8  1.0

      call optlist('intermout',interout)

      if( fitnum.eq.1 ) try=1
      fiddlenm(fitnum)=0
 
c ier 0 and rms low enough -> done
      if( ier(fitnum) .eq. 0 ) then
          tryagain = .false.

c ier<0 -> fit converged, but some problem occured.
c If this is not the redone fit, try again with 1 less component
      else if( ier(fitnum).lt.0 ) then

c        determine how many OK components there are
         nOK(fitnum) = 0
         do i = 1, ngauss(4)
            if( gausspar(errpar(i)).le.1. ) nOK(fitnum)=nOK(fitnum)+1
         enddo

         if( try. le. 99 ) then
            nfit(fitnum+1) = nfit(fitnum) - 1
            tryagain       = nfit(fitnum+1) .gt. 0
            if( tryagain ) then
               ngauss(3) = ngauss(3) - 1
               if(interout)call wrtout('Fit with one component less',1)
               try=1
            endif
         else
c           come here after redoing best fit that gave ier<0
            tryagain = .false.
         endif

c ier >0 -> no convergence; if this is first or second time, fit again with
c slightly different initial estimates because sometimes the reason is a
c sensitivity to the initial estimates; else fit with one less component
c On third try, make new initial estimates because now the suspicion is
c that there are two components close together which were merged, but not
c nice enough to fit as two, and assuming they are one makes the initial
c estimate bad.
      else if( ier(fitnum) .gt. 0 ) then
         nOK(fitnum) = 0

         if( try.le.8 ) then
            if(interout) call wrtout('Fiddle initial estimates',1)
            fiddlenm(fitnum)=try
            do n = 1, ngauss(2)
               j = cmpind(n)
               gaussest(j  )=gaussest(j  ) * fac1(try)
               gaussest(j+1)=gaussest(j+1) + fac2(try)*gaussest(3*n)
               gaussest(j+2)=gaussest(j+2) / fac3(try)
            enddo

            nfit(fitnum+1) = nfit(fitnum)
            tryagain = .true.
            try=try+1
         else if( try.le.99 ) then
            nfit(fitnum+1) = nfit(fitnum) - 1
            tryagain       = nfit(fitnum+1) .gt. 0
            if( tryagain ) then
               ngauss(3) = ngauss(3) - 1
               if(interout)call wrtout('Fit with one component less',1)
            endif
            try=1
         else
            tryagain=.false.
            if(interout) call wrtout('Redoing best fit failed!',1)
         endif

      endif

      if( .not.tryagain  .and.  ier(fitnum).ne.0 .and. try.lt.100 ) then
c exhausted all options: tryagain was set to false, but ier!=0
c Then find the best fit: the one with the lowest rms that also had ier=-8.
c If no such fit is found, there is one more possibility: the culprit is a
c low-amplitude component near the cutoff. If this is indeed so, and the rms
c is OK for that fit, use it anyway, cutting out the low-amplitude component
c Then ier=-12.
c If fits had ier=-13 (vel,fwhm,amp out of range), it does not work, quit.
c Redo the 'best' bad fit, except if it was the very last one done.
c After redoing this fit, tryagain will be set to .false. above, because
c try will be 100.

c        save fitnum (for initial estimates backcorrection)
         fn = fitnum
c        determine minimum rms for fits where only rms is bad
c        also find fit with highest # components that has just low amp
         i = 0
         j = 0
         minrms = 1.E30
         do n = fitnum, 1, -1
            if( ier(n).eq. -8 .and. rms(n).lt.minrms ) j=n
            if( ier(n).eq. -8 .and. rms(n).lt.minrms ) minrms=rms(n)
            if( ier(n).eq.-12 .and. nOK(n).ne.0      ) i=n
         enddo
         if(      j.ne.0 ) then
c           fit j has lowest rms and ier=-8, redo it
            tryagain = fitnum .ne. j
            fitnum   = j
            nfit(fitnum+1) = nfit(j)
         else if( i.ne.0 ) then
c           fit i has low-amplitude, but sufficient integral component
            tryagain = fitnum .ne. i
            fitnum   = i
            nfit(fitnum+1) = nfit(i)
         else
c           nothing found that works, quit it with ier<0
            tryagain = .false.
         endif

         if(tryagain) then
            if(interout) call wrtout('Redo best fit',1)
c           undo possible changes to initial estimates, since otherwise
c           they could be wrong and the previous best fit may not converge
c           (fn is fitnum as it was on call to tryagain)
            do i=fn, fitnum, -1
               if( fiddlenm(i).ne.0 .and. nfit(i).eq.nfit(fitnum) )then
                  try=fiddlenm(i)
                  do n = 1, ngauss(2)
                     j = cmpind(n)
                     gaussest(j  )=gaussest(j  ) / fac1(try)
                     gaussest(j+2)=gaussest(j+2) * fac3(try)
                     gaussest(j+1)=gaussest(j+1) - fac2(try)
     *                                                    *gaussest(j+2)
                  enddo
               endif
            enddo
            try = 100
            ngauss(3) = nfit(fitnum)
         endif
      endif

c it was determined that we have to try again; check if there is room in the
c ier and rms arrays.
      if( tryagain ) tryagain = fitnum.lt.MAXFIT-1

      if( .not. tryagain ) then
c        relax criteria if rms is the only culprit
         if( ier(fitnum).eq.-8 ) then
            if(      rms(MAXFIT) .lt. limlist(24) ) then
               if( interout )
     *             call wrtout('rms fine where model is significant',1)
               ier(fitnum)=0
            else if( rms(fitnum) .lt. limlist(25) ) then
               if( interout )
     *             call wrtout('Decide rms is acceptable anyway',1)
               ier(fitnum)=0
            endif
         endif
c        if low-amplitude component is the culprit, remove it
c        if low-amplitude component is wide enough, but not too wide, ok it
         if( ier(fitnum).eq.-12 ) then
            acclow = .FALSE.
            dellow = .FALSE.
            do i=1, ngauss(4)
               dellow = dellow .or. gausspar(errpar(i)).eq. 19.
     *                         .or. gausspar(errpar(i)).eq. 23.
               acclow = acclow .or. gausspar(errpar(i)).eq.-13.
            enddo
            ngauss(4) = delgauss(gausspar,ngauss(4))
            if( ngauss(4).gt.0 ) ier(fitnum)=0
            if( acclow .and. interout )
     *      call wrtout('Accept low amplitude cmps with OK integral',1)
            if( dellow .and. interout .and. ier(fitnum).eq.0 )
     *      call wrtout('Delete weak components and accept fit',1)      
         else if( ier(fitnum).eq.-13 ) then
            ngauss(4)=0
         endif
      endif

      return
      end

c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************

      subroutine gssout( mode, ier, gausspar,ngauss,cmpsort )
c mode=-1 -> init (if ier>0 profile fitting is not possible for some reason)
c mode= 0 -> print estimates         (optlist('estimout'))
c mode= 1 -> print fiddled estimates (optlist('estimout'))
c mode= 2 -> print intermediate fits (optlist('intermout'))
c mode= 3 -> print final fit

      integer      mode
      integer      ier
      real         gausspar(*)
      integer      ngauss(*)
      real         cmpsort(*)

      logical      interout, estimout, optval, optval2
      integer      cmpind, errind, erroff, errpar, rmsind, len1
      integer      MAXCMP, ARRSIZ
      parameter    ( MAXCMP=10, ARRSIZ=7*MAXCMP+1 )

      integer      coords(2)
      real         flag
      real         gpar(ARRSIZ)
      integer      i, j, jerr, n1, n2, ngss, cnt1
      character*50 fmt
      character*7  form
      character*1  c
      character*99 line
      logical      first, first1, ok
      integer      iabs
      save         first, first1

      call optlist('noprt',optval)
      if( optval ) return
      call optlist('supbad',optval)
      ok = (optval.and.ier.eq.0) .or. .not.optval
      call optlist('intermout',interout)
      call optlist('estimout', estimout)

      if(     mode.eq.-1 ) then
         ok = ok .and. ier.gt.0
         first=.true.
      elseif( mode.eq.0 ) then
         ok = ok .and. estimout
         first1=.true.
      elseif( mode.eq.1 ) then
         ok = ok .and. estimout
      elseif( mode.eq.2 ) then
         ok = ok .and. interout
         if( ok.and.first1)   call wrtout('  ----Fitting--',0)
         first1=.false.
      elseif( mode.eq.3 ) then
         ok = ok .and. ngauss(4).gt.0
         if(ok.and.interout) call wrtout('  -----Done----',0)
      endif
      if( .not.ok ) return

c include pixel position on line for very first output
      line = ' '
      if( first ) then
         first = .false.
         call optlist('abspix',optval)
         if(     optval) flag=0.
         if(.not.optval) flag=1.
         call cooinfo( 7, coords(1), coords(2), flag )
         call optlist('aver',  optval)
         call optlist('summed',optval2)
         if( .not.(optval.or.optval2) )
     *      write( line(1:8), '(i4,i4)' ) coords(1), coords(2)
      else if( mode.eq.3 ) then
         call optlist('aver',  optval)
         call optlist('summed',optval2)
         if( optval  ) line(1:8) = 'average '
         if( optval2 ) line(1:8) = 'summed  '
      endif

      if( ier.le.0 ) then

c include rms on first line of list of gaussians
         n1 = 2*4+2 + 1+1+1 + 3*(11+9)
         n2 = n1 + 6
         form = 'f7.3'
         if( gausspar(rmsind()).gt.999.999  .or.
     *       gausspar(rmsind()).lt.000.001)       form = '1pe7.1'
         write( fmt, '( ''('',a,'')'' )' ) form
         write( line(n1:n2), fmt ) gausspar(rmsind())

c mode=0/1: ngauss(3) components were estimated
c mode=2/3: ngauss(4) components were fit, unless ier>0, in which
c           case the reason why is written
         if( mode.le.1 ) ngss = ngauss(3)
         if( mode.ge.2 ) ngss = ngauss(4)

c sort gaussians
         cmpsort(4) = real(ngauss(6))
         call gsssrt( gpar, gausspar, ngss, cmpsort )
c convert sorted values to physical coordinates
         call parconv( gpar, ngss, .false. )

c if gssrt determined that a zero component is inserted, reflect that in output
c (can happen when keyword cmpsort=vrange,.,. is given)
         if( cmpsort(4).ge.0 ) cnt1 = 0
         if( cmpsort(4).lt.0 ) cnt1 = 1

         do i = 1, ngss
            form = 'f'
            do j = cmpind(i), cmpind(i)+2
              if(gpar(j).ge.1.e6.or.gpar(j+erroff()).ge.1.e6)form='1pe'
            enddo
            write( fmt, '(   ''(a'',a,a,a,a,a,'')''   )' )
     *             ',i1,1x, 3(',form,'10.4,1x), 3(',form,'8.4,1x)'

            n1 = 2*4 + 2
            n2 = n1  + 1+1+1 + 3*(11+9)
            if( mode.le.1 ) c='e'
            if( mode.ge.2 ) c=' '
            j    = cmpind(i)
            jerr = errind(i)
            write( line(n1:n2), fmt )  c, i+cnt1,
     *         gpar(j   ), gpar(j   +1), gpar(j   +2),
     *         gpar(jerr), gpar(jerr+1), gpar(jerr+2)
            call wrtout( line, 0 )
            line = ' '
         enddo
      endif

c error messages
c if ier=-12/-13, multiple msg for all gaussians, else just one message
      if( ier.ne.0 ) then
         n1 = 2*4 + 2
         if( mode.eq.-1               ) line(n1:) = 'No fit;     '
         if( mode.eq.0                ) line(n1:) = 'Bad estim;  '
         if( mode.eq.1 .or. mode.eq.2 ) line(n1:) = 'Bad fit;    '
         if( mode.eq.3 .and. ier.lt.0 ) line(n1:) = 'Fit not accepted; '
         if( mode.eq.3 .and. ier.gt.0 ) line(n1:) = 'Fit failed; '

         if( ier.gt.-12 ) then
           call errout( line, len1(line)+2, iabs(ier) )
         else
            n1 = len1(line)+2
            do i = 1, ngauss(4)
               write( line(n1:), '( ''cmp '',i1,'': '' )' ) i
               n2 = len1(line)+2
               call errout( line, n2, -nint(gpar(errpar(i))) )
            enddo
         endif
      endif

      return
      end

c************************************************************************

      subroutine checkres( gausspar, gaussest, ngauss )
      real         gausspar(*), gaussest(*)
      integer      ngauss

      integer      cmpind, errpar

      integer      i, j, l, len1
      real         aratio, vdiff, wratio
      character*80 line

      do i = 1, ngauss
         if( gausspar(errpar(i)) .eq. 1. ) then
         j = cmpind(i)
         aratio =  gausspar(j  ) / gaussest(j  )
         vdiff  = (gausspar(j+1) - gaussest(j+1)) * gaussest(j+2)
         wratio =  gausspar(j+2) / gaussest(j+2)
         write( line, '( ''         NOTE: cmp '',i1,'':'' )' ) i
         l = len1(line)+2
         if(aratio.lt.0.4 .or. aratio.gt.2.5) call errout(line,l, 9)
         if(vdiff .gt.1.0                   ) call errout(line,l,10)
         if(wratio.lt.0.5 .or. wratio.gt.2.0) call errout(line,l,11)
         endif
      enddo
      return
      end

c************************************************************************

      subroutine errout( line, n1, ier )
      character*(*) line
      integer       n1, ier

      integer       ierr
      integer       iabs
      integer       i, j
      integer       primes(9)
      character*80  messages(20)
      save          messages
      data          primes / 23, 19, 17, 13, 11, 7, 5, 3, 2 /
      data          messages /
     *              'DRVMRQ: No convergence after .. iterations',
     *              'SVDCMP: no convergence after 30 iterations',
     *              'DRVMRQ: too many major iterations',
     *              'Gaussian is bad parametrization of profile',
     *              'Too many datapoints masked out',
     *              'The profile is a constant, no fit can be done',
     *              'No gaussian found by estimator',
     *              'rms too high',
     *           'fitted amplitude differs significantly from estimate',
     *              'fitted velocity shifted by more than one FWHM',
     *              'fitted width differs significantly from estimate',
     *              'Negative amplitudes not allowed',
     *              'Amplitude below specified minimum',
     *              'Amplitude above specified maximum',
     *              'Width larger than profile length',
     *              'Width insignificant',
     *              'Center outside range',
     *              'Width error too large',
     *              'Velocity error too large',
     *              'Amplitude error too large' /
c ier>100    - DRVMRQ message
c ier=2-7    - msg # 2-7
c ier=-8 (+8 on call) - rms too high
c ier=9,10,11 - amp/vel/width differs
c ier (<0 on call) (ier=-12, -13 or product of errornumbers)
c mod(-ier,[23,19,17,13,11, 7, 5, 3, 2]) gives message #
c           12,13,14,15,16,17,18,19,20

      ierr = ier
      if(      ierr.gt.100 ) then
         write( messages(1)(30:31), '( i2 )' ) ierr-100
         ierr = 1
         line(n1:) = messages(ierr)
         call wrtout(line,0)
      else if( ierr.gt.0 ) then
         line(n1:) = messages(ierr)
         call wrtout(line,0)
      else if( ierr.lt.-1 ) then
         i = iabs(ierr)
         do j = 1, 9
            if( mod(i,primes(j)) .eq. 0 ) then
               line(n1:) = messages(j+11)
               call wrtout(line,0)
               i = i / primes(j)
            endif
         enddo
      endif
      return
      end

c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************

c If topix is .true.: convert
c   from   integral or amplitude, center, dispersion or fwhm
c   to     amplitude, center in pixels, 1/dispersion in pixels
c If topix is .false.: convert
c   from   amplitude, center in pixels, 1/dispersion in pixels
c   to     integral or amplitude, center, dispersion or fwhm
c
c If ngss is <0, ignore the INTG flag, input is always amplitude

      subroutine parconv( gausspar, ngss, topix )

      real      gausspar(*)
      integer   ngss
      logical   topix

      integer   iabs
      logical   disper, integr, pixel
      real      dv
      integer   cmpind, errind
      include  'mirconst.h'

      integer   ngauss
      integer   i, j, jerr
      real      scale
      real      sqrt8ln2
      integer   a0,a1,am1
      data      sqrt8ln2 / 2.35482004503094930 /

c     a0,a1,am1 keep flint quiet
      a0=0
      a1=1
      am1=-1
      call optlist('dispersion',disper)
      call optlist('integral',  integr)
      call optlist('pixel',     pixel )
      ngauss = iabs(ngss)
      do i = 1, ngauss
         j    = cmpind(i)
         jerr = errind(i)
         if( gausspar(j).ne.0. ) then

c CONVERT PHYSICAL TO PIXEL
         if( topix ) then
c  ---      Input FWHM -> convert to dispersion
            if( .not.disper ) then
               gausspar(j   +2) = gausspar(j   +2) / sqrt8ln2
               gausspar(jerr+2) = gausspar(jerr+2) / sqrt8ln2
            endif
c           Input integral -> convert to amplitude
            if( integr .and. ngss.gt.0 ) then
               scale = sqrt(twopi)*gausspar(j+2)
               gausspar(j   ) = gausspar(j   ) / scale
               gausspar(jerr) = gausspar(jerr) / scale
            endif
c  ---      Convert units of dispersion and central velocity to pixels
            if( .not.pixel ) then
               call cooinfo( 3,a0,a0,dv )
               call cooinfo( 8,a1,a0, gausspar(j+1) )
               gausspar(jerr+1) = gausspar(jerr+1) / dv
               gausspar(j   +2) = gausspar(j   +2) / dv
               gausspar(jerr+2) = gausspar(jerr+2) / dv
            endif
c  ---      Convert to inverse dispersion
c           d(1/s) = d(s) / s^2
            gausspar(jerr+2) = gausspar(jerr+2) / gausspar(j+2)**2
            gausspar(j   +2) =               1. / gausspar(j+2)
         else

c CONVERT PIXEL TO PHYSICAL
c  ---      Convert inverse to dispersion
c           d(s) = d(1/s) / (1/s)^2
            gausspar(jerr+2) = gausspar(jerr+2) / gausspar(j+2)**2
            gausspar(j   +2) = 1. / gausspar(j+2)
c  ---      Convert units of dispersion and velocity to physical
            if( .not.pixel ) then
               call cooinfo( 3,a0,a0,dv )
               call cooinfo( 8,am1,a0,gausspar(j+1) )
               gausspar(jerr+1) = gausspar(jerr+1) * abs(dv)
               gausspar(j   +2) = abs(gausspar(j   +2) * dv)
               gausspar(jerr+2) = abs(gausspar(jerr+2) * abs(dv))
            endif
c  ---      Output Integral -> convert to amplitude
            if( integr .and. ngss.gt.0 ) then
               scale = sqrt(twopi)*gausspar(j+2)
               gausspar(j   ) = gausspar(j   ) * scale
               gausspar(jerr) = gausspar(jerr) * scale
            endif
c           Output FWHM -> convert dispersion
            if( .not.disper ) then
               gausspar(j   +2) = gausspar(j   +2) * sqrt8ln2
               gausspar(jerr+2) = gausspar(jerr+2) * sqrt8ln2
            endif
         endif

         endif
      enddo
      return
      end

c***********************************************************************

c sort the gaussians and insert them in proper place

      subroutine gsssrt( gausrt, gauin, ngauss, cmpsort )

      real      gausrt(*)
      real      gauin(*)
      integer   ngauss
      real      cmpsort(*)

      integer   MAXCMP, ARRSIZ
      parameter ( MAXCMP=10, ARRSIZ=7*MAXCMP+1 )

      real      svals(MAXCMP)
      integer   sind(MAXCMP), off, order, i, j
      integer   ngss, iinrange, k
      real      dv
      integer   a0

c 1=none 2=velocity, 3=integral, 4=amplitude, 5=fwhm, 6=vdiff, 7=vrange
      integer   offs(7), orders(7)
      data      offs   / 1, 1,  2,  2,  0,  1,  2 /
      data      orders / 1, 1, -1, -1,  1,  1, -1 /
      if( ngauss.eq.0 ) return

      off   = offs(   int(cmpsort(1)) )
      order = orders( int(cmpsort(1)) )
c sort is before conversion, i.e. on pixels => reverse if dv < 0
      a0=0
      call cooinfo( 3,a0,a0,dv )
      if( off.eq.1 .and. dv.lt.0 ) order=-1

c make array containing the sorting parameter
      do i = 1, ngauss
                                svals(i) = gauin(3*i-off)
         if( cmpsort(1).eq.1. ) svals(i) = real(i)
         if( cmpsort(1).eq.5. ) svals(i) = abs( cmpsort(2) - svals(i) )
      enddo

c find list of sorted indices
      call sortidxr( ngauss, svals, sind )

c copy to output in sorted order (ascending for order=1, descending for order-1)
      do i = 1, ngauss
         if( order.eq. 1 ) j = sind(         i)
         if( order.eq.-1 ) j = sind(ngauss+1-i)
         call setgssa( gausrt,i, gauin,j )
      enddo

      if( cmpsort(1).eq.7. ) then
c Sort components on velocity range: all components within the given range
c come first.
c If the maximum number of components was fitted, then the following
c possibilities exist:
c  1 component   is within range, that is #1, the rest is #2+
c  0 components are within range, sort all on amplitude
c >1 components are within range, sort all on amplitude
c If less than the maximum number of components was fitted, then the following
c possibilities exist:
c  1 component   is within range, that is #1, the rest is #2+
c  0 components are within range, sort all on amplitude, but if cmpsort(4)
c    was negative, insert a zero component (this happens on last call)
c    if cmpsort(4) was positive on input, set it negative to flag that the
c    component numbering has shifted by 1
c >1 components are within range, sort all on amplitude

c ngauss is # components actually fit
c ngss   is maximum # components to fit
         ngss = nint(abs(cmpsort(4)))

         call setngsa( gauin, gausrt, ngauss )
         k=0
         do i = 1, ngauss
            if( cmpsort(2).le.gauin(3*i-1) .and.
     *          cmpsort(3).ge.gauin(3*i-1)       ) then
               k=k+1
               iinrange = i
            endif
            sind(i) = i
         enddo
         if(      k.eq.0 ) then
            if( ngauss.lt.ngss ) then
               if( cmpsort(4).lt.0. ) then
                  ngauss = ngauss+1
                  call setgssz( gauin,ngauss )
                  sind(1) = ngauss
                  do i = 2, ngauss
                     sind(i) = i-1
                  enddo
               else
                  cmpsort(4) = -1
               endif
            endif
         else if( k.eq.1 ) then
            k=2
            do i = 1, ngauss
               if( i .eq. iinrange ) then
                  sind(1) = i
               else
                  sind(k) = i
                  k       = k + 1
               endif
            enddo
         endif
         do i = 1, ngauss
            call setgssa( gausrt,i, gauin,sind(i) )
         enddo
      endif

      return
      end


c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************
c***********************************************************************

c coordinate info and conversions

      subroutine cooinfo( mode, intpar1, intpar2, realpar )
      integer     mode
      integer     intpar1, intpar2
      real        realpar

c cooinfo( 1, units(1), fitax, 0. )      reads info for axis fitax from unit
c cooinfo( 2, units(5), fitax, naxesgp ) creates gpar axis
c cooinfo( 3, 0,0, dv  )                 return deltav
c cooinfo( 4, 0,0, nch )                 return nvel
c cooinfo( 5, type,0,0. )                return code for ctype(v)
c cooinfo( 6, unit1,unit2,profnr )       call xyzs2c for profile, set coo
c cooinfo( 7, coord1,coord2,flag )       return coo
c cooinfo( 8, flag,0,xval )              convert x from pix to phys and vv

      integer     unit, fitax
      integer     naxg
      real        coord

      integer     unit1, unit2
      integer     profnr

      integer     i, j
      character*8 keyw
      include    'maxnax.h'
      logical     nogpinp

      integer     naxis
      double precision crval(MAXNAX), cdelt(MAXNAX), crpix(MAXNAX)
      character*9 ctype(MAXNAX)
      integer     x, y, v, nvel
      integer     coo(MAXNAX)
      save        naxis, crval, cdelt, crpix, ctype, x,y,v,nvel, coo

      if ( mode .eq. 1 ) then
         unit  = intpar1
         fitax = intpar2

         call rdhdi( unit, 'naxis', naxis, 0 )

c        x, y, v tell which axis number is x,y,v axis
c        read # channels of parinp dataset => nvel
c        fitax.ne.0 => input spectra
c        fitax.eq.0 => parinp only => fitax description in naxis+1
         if( fitax .ne. 0 ) then
            v = fitax
            call rdhdi( unit, keyw('naxis',v), nvel,1 )
         else
            v = naxis + 1
            call rdhdd( unit, keyw('crval',v+1), crval(v+1), 1.d0 )
            nvel = nint(crval(v+1))
         endif

         j = 1
         x = 0
         y = 0
         do i = 1, max(naxis,v)
            call rdhdd( unit, keyw('crval',i), crval(i), 1.d0 )
            call rdhdd( unit, keyw('crpix',i), crpix(i), 1.d0 )
            call rdhdd( unit, keyw('cdelt',i), cdelt(i), 1.d0 )
            call rdhda( unit, keyw('ctype',i), ctype(i), ' '  )
            if( i.ne.fitax ) then
               if( x.ne.0 .and. y.eq.0 ) y=j
               if( x.eq.0              ) x=j
               j = j + 1
            endif
         enddo
c        crpix(naxis+1) is not written (why, BobS?)
c        saved it in cdelt(naxis+2) instead
         if( fitax.eq.0 )
     *      call rdhdd( unit, keyw('cdelt',v+1), crpix(v), 1.d0 )

c        if options=arcsec, arcmin, deg
         if( ctype(v)(1:2).eq.'RA' .or. ctype(v)(1:3).eq.'DEC' ) then
            crval(v) = crval(v) * 3600.d0 * 180.d0/acos(-1.)
            cdelt(v) = cdelt(v) * 3600.d0 * 180.d0/acos(-1.)
         endif

      else if ( mode .eq. 2 ) then
         unit  = intpar1
         fitax = intpar2
c        naxg coded two things, to avoid extra input variable in call
         naxg  = nint(realpar)
         nogpinp = naxg .gt. 100
         if( naxg .gt. 100 ) naxg = naxg - 100

c        create description of parameter axis
         call wrhdd( unit, keyw('crval',naxg),  1.d0  )
         call wrhdd( unit, keyw('crpix',naxg),  1.d0  )
         call wrhdd( unit, keyw('cdelt',naxg),  1.d0  )
         call wrhda( unit, keyw('ctype',naxg), 'GPAR' )
c        copy fitaxis parameters to axis naxes+1
c        if noinp, copy fitax description from naxes+1 instead
         if( nogpinp ) fitax=naxg+1
         call wrhdd( unit, keyw('crval',naxg+1), crval(fitax) )
         call wrhdd( unit, keyw('cdelt',naxg+1), cdelt(fitax) )
         call wrhdd( unit, keyw('cdelt',naxg+2), crpix(fitax) )
         call wrhda( unit, keyw('ctype',naxg+1), ctype(fitax) )
c        copy # channels to crval(naxg+2)
         call wrhdi( unit, keyw('crval',naxg+2), nvel         )

      else if( mode .eq. 3 ) then
         realpar = cdelt(v)

      else if( mode .eq. 4 ) then
         realpar = real(nvel)

      else if( mode .eq. 5 ) then
         if( ctype(v)(1:2).eq.'RA'   ) intpar1=1
         if( ctype(v)(1:3).eq.'DEC'  ) intpar1=2
         if( ctype(v)(1:4).eq.'VELO' ) intpar1=3
         if( ctype(v)(1:4).eq.'FREQ' ) intpar1=4

      else if( mode .eq. 6 ) then
         unit1  = intpar1
         unit2  = intpar2
         profnr = nint(realpar)
         if( unit1.ne.0 ) call xyzs2c( unit1, profnr, coo )
         if( unit1.eq.0 ) call xyzs2c( unit2, profnr, coo )

      else if( mode .eq. 7 ) then
         if( realpar.eq.0. ) then
            intpar1 = coo(1)
            intpar2 = coo(2)
         else
            intpar1 = coo(1) - nint(crpix(x))
            intpar2 = coo(2) - nint(crpix(y))
         endif

      else if( mode .eq. 8 ) then
         coord = realpar
c        intpar1>0 => phys2pix
c        intpar1<0 => 2pix2phys
         if( intpar1.gt.0 ) then
            realpar = (coord-crval(v))/cdelt(v) + crpix(v)
         else
            realpar = (coord-crpix(v))*cdelt(v) + crval(v)
         endif
      endif

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

c************************************************************************

      subroutine header( unit )

      integer            unit

      logical            pixel, integr, disper

      character*99       line1, line2
      character*9        key
      integer            len1, n, type
      integer            LABLEN
      parameter          ( LABLEN = 11 )
      character*(LABLEN+1) dataunit, profunit, lab
      integer            a0
      real               r0

      call optlist('dispersion',disper)
      call optlist('integral',  integr)
      call optlist('pixel',     pixel )

      if( pixel ) then
         profunit = 'pixels'
      else
c        next three statements keep flint quiet
         a0=0
         r0=0.
         type=0
         call cooinfo( 5,type,a0,r0 )
c        1=RA, 2=DEC, 3=VELO, 4=FREG
         if( type.eq.1 ) profunit =  'arcsec'
         if( type.eq.2 ) profunit =  'arcsec'
         if( type.eq.3 ) profunit =  'km/s'
         if( type.eq.4 ) profunit =  'GHz'
      endif

      call rdhda( unit, 'bunit', key, ' ' )
      dataunit = key
      if(integr) dataunit = key(:len1(key)) // ' ' // profunit

      line1 = '   x   y # |'
      line2 = ' '
      n = len1(line1)
      if(      integr ) lab = 'integral'
      if( .not.integr ) lab = 'amplitude'
      line1( n  +  LABLEN-len1(lab)+1        : ) = lab
      line2( n  +  LABLEN-len1(dataunit)+1   : ) = dataunit
      n = n + LABLEN
      if(      pixel ) lab = 'position '
      if( .not.pixel ) lab = 'velocity '
      line1( n  +  LABLEN-len1(lab)+1        : ) = lab
      line2( n  +  LABLEN-len1(profunit)+1   : ) = profunit
      n = n + LABLEN
      if(      disper ) lab = 'dispersion  '
      if( .not.disper ) lab = 'fwhm  '
      line1( n  +  LABLEN-len1(lab)+1        : ) = lab
      line2( n  +  LABLEN-len1(profunit)+1   : ) = profunit
      n = n + LABLEN
      line1(n+1:) = '|          errors           | rms'
      call wrtout( line1, 0 )
      call wrtout( line2, 0 )

      return
      end

c************************************************************************
c************************************************************************
c************************************************************************
c************************************************************************
c************************************************************************

      subroutine writprof( data, model, estimate, nchan, pixels,
     *                     gausspar, ngauss, prnm )

      real          data(*)
      real          model(*), estimate(*)
      integer       nchan
      logical       pixels
      real          gausspar(*)
      integer       ngauss
      character*80  prnm

      include      'maxnax.h'
      integer       coords(MAXNAX)
      integer       nf(2), nfigi, len1
      character*100 file, rtfmt
      external rtfmt
      character*150 line
      character*30  frmt
      integer       unit, iostat
      integer       i
      real          x
      integer       j, n, cmpind
      real          cmpmodel(10), expon
      integer       a0,a1,am1
      real          r1
      character*20  nums
      data          nums / '     111115678901234' /

c     next statement to keep flint quiet
      coords(1)=0
      coords(2)=0
      a0=0
      a1=1
      am1=-1
      r1=0.

      call cooinfo( 7, coords(1), coords(2), r1 )
      nf(1) = nfigi( coords(1) )
      nf(2) = nfigi( coords(2) )
      i = len1(prnm)
      file(:i) = prnm(:i)
      write( file(i+1:), rtfmt( 'i<>,''_'',i<>',nf,2 ) )
     *                   coords(1), coords(2)
      call txtopen( unit, file, 'new', iostat )
      if ( iostat.ne.0 ) then
         call bug( 'w', 'error opening file to write profile on' )
      else

         frmt(1:27) = '(i4, f10.4,  4(1x,1pg11.4))'
         if( ngauss.gt.0 ) then
            frmt(13:13)=nums(ngauss:ngauss)
            frmt(14:14)=nums(ngauss+10:ngauss+10)
         endif

         do i = 1, nchan
            if( ngauss.gt.0 ) then
               do n = 1, ngauss
                  j = cmpind(n)
                  expon = 0.5*( (i-gausspar(j+1))*gausspar(j+2) )**2
                  if(expon.lt.40.) cmpmodel(n) = gausspar(j)*exp(-expon)
                  if(expon.ge.40.) cmpmodel(n) = 0.
               enddo
               x=real(i)
               if(     pixels) call cooinfo( 8,a1, a0,x )
               if(.not.pixels) call cooinfo( 8,am1,a0,x )
               write( line, frmt )
     *         i, x, data(i), model(i), data(i)-model(i), estimate(i),
     *         (cmpmodel(n),n=1,ngauss)
            else
               x=real(i)
               if(     pixels) call cooinfo( 8, a1,a0,x )
               if(.not.pixels) call cooinfo( 8,am1,a0,x )
               write( line, frmt )
     *         i, x, data(i), model(i), data(i)-model(i), estimate(i)
            endif
            call txtwrite( unit, line, len1(line), iostat )
          enddo
       endif
       call txtclose( unit )

       return
       end

c************************************************************************

c Finish up
      subroutine finish( units, version )

      integer       units(*)
      character*(*) version

      logical       optval

      integer       i
      character*80  line

      do i = 1, 6
         if( units(i).ne.0 ) then
            if( i.gt.3 ) then
               call hisopen(  units(i), 'append'  )
               line = 'GAUFIT: ' // version
               call hiswrite( units(i), line )
               call hisinput( units(i), 'gaufit'  )
               call hisclose( units(i) )
            endif
            call xyzclose( units(i) )
         endif
      enddo
      call optlist('logf',optval)
      if(optval) call logclose

      return
      end

c***********************************************************************

      subroutine setgssz( gauss,n )
      real      gauss(*)
      integer   n
      integer   j, jerr, cmpind, errind, errpar
      j    = cmpind(n)
      jerr = errind(n)
      gauss(j     ) = 0.
      gauss(j   +1) = 0.
      gauss(j   +2) = 0.
      gauss(jerr  ) = 0.
      gauss(jerr+1) = 0.
      gauss(jerr+2) = 0.
      gauss(errpar(n)) = 0.
      return
      end
      subroutine setngsz( gauss, ngauss )
      real      gauss(*)
      integer   ngauss, i
      do i = 1, ngauss
         call setgssz( gauss, i )
      enddo
      return
      end

      subroutine setgssa( gauss,n, ingauss,m )
      real      gauss(*), ingauss(*)
      integer   n, m
      integer   j, jerr, k, kerr, cmpind, errind, errpar
      j    = cmpind(n)
      jerr = errind(n)
      k    = cmpind(m)
      kerr = errind(m)
      gauss(j     ) = ingauss(k     )
      gauss(j   +1) = ingauss(k   +1)
      gauss(j   +2) = ingauss(k   +2)
      gauss(jerr  ) = ingauss(kerr  )
      gauss(jerr+1) = ingauss(kerr+1)
      gauss(jerr+2) = ingauss(kerr+2)
      gauss(errpar(n)) = ingauss(errpar(m))
      return
      end
      subroutine setngsa( gauss, ingauss, ngauss )
      real      gauss(*), ingauss(*)
      integer   ngauss, i
      do i = 1, ngauss
         call setgssa( gauss,i, ingauss,i )
      enddo
      return
      end

      subroutine setgssv( gauss,n, amp, vel, wid )
      real      gauss(*), amp,vel,wid
      integer   n
      integer   j, jerr, cmpind, errind, errpar
      j    = cmpind(n)
      jerr = errind(n)
      gauss(j     ) = amp
      gauss(j   +1) = vel
      gauss(j   +2) = wid
      gauss(jerr  ) = 0.
      gauss(jerr+1) = 0.
      gauss(jerr+2) = 0.
      gauss(errpar(n)) = 0.
      return
      end

      integer function delgauss( gausspar,ngauss )
      real      gausspar(*)
      integer   ngauss
      integer   i, j
      integer   errpar, rmsind
      j=0
      do i = 1, ngauss
         if( gausspar(errpar(i)).le.1. ) then
            j = j + 1
            call setgssa( gausspar,j, gausspar,i )
         endif
      enddo
      if( j.eq.0 ) gausspar(rmsind())=0.
      delgauss = j
      end

      integer function cmpind(i)
      integer          i
      cmpind =            3*i-2
      return
      end

      integer function errind(i)
      integer i
      integer   MAXCMP
      parameter ( MAXCMP=10 )
      errind = 3*MAXCMP + 3*i-2
      return
      end

      integer function erroff()
      integer   MAXCMP
      parameter ( MAXCMP=10 )
      erroff = 3*MAXCMP
      return
      end

      integer function errpar(i)
      integer i
      integer   MAXCMP
      parameter ( MAXCMP=10 )
      errpar = 6*MAXCMP + i
      return
      end

      integer function rmsind()
      integer   MAXCMP
      parameter ( MAXCMP=10 )
      rmsind = 7*MAXCMP + 1
      return
      end

c************************************************************************

      subroutine wrtout( line, n1 )
      character*(*) line
      integer       n1
      logical       optval
      character*80  line1
                    line1(1:9) = ' '
      if( n1.eq.0 ) line1( 1:) = line
      if( n1.ne.0 ) line1(10:) = line
      call optlist('logf',optval)
      if( optval ) then
         call logwrit( line1 )
      else
         call output(  line1 )
      endif
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
        expon = 0.5*arg**2
        if( expon.gt.40. ) then
           expon = 0.
        else
           expon = exp(-expon)
        endif
        y = y + pars(i)*expon
        if( compgrad ) then
           fac         = pars(i) * expon * arg
           dydpar(i)   = expon
           dydpar(i+1) =  fac * pars(i+2)
           dydpar(i+2) = -fac * (x-pars(i+1))
        endif
      enddo
      return
      end

      subroutine getmax( data,nchan, maxdat, im )
      real    data(*), maxdat
      integer nchan, im
      integer i
      maxdat = data(1)
      im     =  1
      do i = 1, nchan
         if( data(i).gt.maxdat ) then
            maxdat = data(i)
            im     = i
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
          write(line,'(''MRQMIN: setup call; chisq, xpar = '',1pe10.4)')
     *          chisq
          call output(line)
          write(line,'(10(f8.4,1x))') ( xpar(i), i = 1, npars )
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

          if(test)then
          write(line,'(''New chisq, ier: '',1pe10.4,1x,i1)')chisq,ier
          call output(line)
          endif

          if( ier.eq.0 ) then
             if( (ochisq-chisq) .gt. FTOL ) then
                loop  = .true.
                inegl = 0
                if( test ) call output(
     *          'DRVMRQ: Chi squared decrease significant' )
             else
                inegl = inegl + 1
                if( inegl .lt. maxnegl ) then
                   loop = .true.
                   if( test ) call output(
     *             'DRVMRQ: Insignificant decrease in chisq' )
                else
                   loop = .false.
                   if( test ) then
                   write( line, '( ''DRVMRQ quits at iteration '',i2,
     *                          '': negligible decrease number '',i2)')
     *                          iter, inegl
                   call output(line)
                   endif
                endif
             endif
          elseif( ier.eq.1 ) then
             loop = .false.
             if( test ) then
             write( line, '( ''MRQMIN: gives up after '',i2,'' tries'',
     *                  '' with alamda = '',e10.4 )' ) ntries, alamda
             call output(line)
             endif
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
             write( line, '(10(f8.4,1x))') ( xpar(j), j=1,npars )
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


      subroutine prg(g)
      real g(*)
      write(*,'(12(1pe7.1,1x))')
     * g(1),g(2),g(3),g(4),g(5),g(6),
     * g(31),g(32),g(33),g(34),g(35),g(36)
      return
      end
