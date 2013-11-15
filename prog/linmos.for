      program linmos
c
c= linmos - Linear mosaicing of datacubes
c& rjs
c: map combination
c+
c       LINMOS is a MIRIAD task that performs simple linear mosaicing
c       of input cubes to produce a single output cube.  If only one
c       input cube is given, LINMOS essentially does primary beam
c       correction on it.  If the input cubes overlap, LINMOS combines
c       the overlapping regions so as to minimize the rms error.
c
c       To determine the primary beam of the telescope, LINMOS first
c       checks the map header for the presence of the "pbtype" and then
c       "pbfwhm" parameters.  If present, LINMOS assumes the primary
c       beam is the given type.  If these parameters are missing, LINMOS
c       checks if the telescope is one that it knows.  If so, then the
c       known form for the primary beam is used. See task "pbplot" to
c       check LINMOS's primary beam models.
c@ in
c       The names of the input cubes - many may be given.  There is no
c       default.  Inputs should generally be on the same grid system.
c       If not, linear interpolation is performed to regrid using the
c       first image as the template.  LINMOS's ability to do this is
c       inferior to task REGRID.  The intensity units of all the inputs,
c       and the pixel size and alignment of the third dimension are
c       assumed to be the same.
c@ out
c       The name of the output cube.  No default.  The center and pixel
c       size of the first input image is used as the grid system of the
c       output.
c@ rms
c       RMS noise level in each of the input cubes.  If not specified,
c       the value is taken from the 'rms' item in the input image header
c       if that is available, and if not, then the rms of the previous
c       image is used.  If no value could be determined for the first
c       image, a warning is issued and ALL images are given equal weight
c       by assigning an RMS of 1.0.
c@ bw
c       Bandwidth of the image in GHz, default 0. If specified the beam 
c       response will either be averaged across the frequency band before
c       being applied to the image or, if the input images contain a 
c       spectral index plane (created with the mfs option of restor)  
c       the images will be evaluated and corrected across the band.
c       Use this for wide band images to improve the accuracy of the 
c       correction.
c       Note that doing wide band primary beam correction at low
c       frequency will make the effective observing frequency vary
c       significantly across the field. 
c       An optional second parameter can be given to set the number
c       of frequencies to divide the bandwidth into, it defaults to 10.
c
c@ cutoff
c       The cutoff level to use for the primary beam. Normally the 
c       built-in level for each beam model is used. This can be useful
c       e.g., to restrict polarization mosaics to use only the part of  
c       the beam with low instrumental polarization. Defaults to zero 
c       which means use the built-in level.
c@ options
c       Extra processing options.  Several can be given, separated by
c       commas.  Minimum match is supported.  Possible values are:
c         taper        By default, LINMOS fully corrects for primary
c                      beam attenutation.  This can cause excessive
c                      noise amplification at the edge of the mosaiced
c                      field.  The `taper' option aims at achieving
c                      approximately uniform noise across the image.
c                      This prevents full primary beam correction at the
c                      edge of the mosaic.  See Eq. (2) in Sault,
c                      Staveley-Smith and Brouw (1996), A&AS, 120, 375,
c                      or use "options=gains" to see the form of the
c                      tapering.
c         sensitivity  Rather than a mosaiced image, produce an image
c                      giving the RMS noise across the field.  This is
c                      dependent on the RMS specified, either as an
c                      input parameter or else as a header item (see
c                      above).
c         gain         Rather than a mosaiced image, produce an image
c                      giving the effective gain across the field.  If
c                      options=taper is used, this will be a smooth
c                      function.  Otherwise it will be 1 or 0 (blanked).
c         frequency    Rather than a mosaiced image, produce an image 
c                      giving the effective frequency across the field.
c         alpha        If an I*alpha plane is present in the input, 
c                      produce a mosaiced version of this as well.
c                      You can feed the output image to mfspin to
c                      produce the mosaiced spectral index image.
c                      The spectral index mosaic becomes unreliable  
c                      when the fractional bandwidth is > 0.4 (this
c                      is because higher orders are needed to model
c                      the primary beam response with frequency).
c
c$Id: linmos.for,v 1.25 2013/11/15 04:43:54 wie017 Exp $
c--
c
c  History:
c    rjs  27oct89 Original version.
c    rjs   6nov89 Interpolation, better i/o, more error checks.
c    rjs   8feb90 The routine to determine default primary beam size did
c                 the calculation wrong!
c    rjs  18feb90 Added offseting of x0,y0. Corrected determination of
c                 dra,ddec. Corrected bug in GetPB
c    rjs  14apr90 Fixed bug in the interpolation code.
c    rjs  24apr90 Made GetPB into a separate file.
c    rjs  26apr90 Copied linetype keywords to the output.
c    mchw 09nov90 Get pbfwhm from map header and set pbfwhm=0 in output
c                 map.
c    rjs   5nov91 Eliminated maxdim**2 arrays, and standardised history.
c    rjs  20nov91 Replace keya('in'...) with keyf('in'...).
c    mchw 19mar92 Check third axis alignment.
c    rjs  20mar92 Added "save" statement to above code.
c    rjs  12nov92 Significant mods, to use nebk's primary beam routines,
c                 and to partially handle blanked images.
c    nebk 25nov92 Copy btype to output
c    rjs  10dec92 New CALL sequence for GETFREQ and handle zero
c                 frequency.
c    nebk 28jan93 new P.B. interface
c    rjs   4oct93 Increase number of cubes that can be handled.
c    rjs  26oct93 Fixed geometry problem.
c    rjs   6dec93 Improve some messages.
c    mhw  13jan94 Relax alignment requirement on the third axis.
c    rjs  22jul94 Added options=sensivitivy and options=gain. Use some
c                 standard include files.
c    rjs  26jul94 Doc changes only.
c    rjs  17aug94 Change projection code to cartesian.
c    rjs  24oct94 Use new pb routines.
c    rjs  30nov94 Preserve the projection geometry when all the inputs
c                 have the same geometry. Handle single-pointing
c                 mosaic tables.
c    rjs   3dec94 More lenient "exactness" algorithm in thingchk.
c    rjs  30jan95 Taper option. Eliminate "signal" parameter.
c    mhw  13aug95 Clip input if output too big, instead of giving up.
c    rjs  14aug95 Fix to the above.
c    rjs  02jul97 cellscal change.
c    rjs  23jul97 pbtype change.
c    rjs  04aug97 Doc change only.
c    rjs  25aug97 Doc change and an extra error message.
c    mhw  25nov10 Cope with OTF mosaics using extra parameters in
c                 mosaic table
c    mhw  03mar11 Add bandwidth
c    mhw  18sep12 Use correct frequencies when not all the same
c    mhw  23jan13 Handle 2nd plane (mfs I*alpha) in input
c    mhw  03may13 Extension to previous and add options=frequency
c    mhw  14oct13 Add alpha option
c    mhw  15nov13 Add cutoff keyword
c
c  Bugs:
c    * Blanked images are not handled when interpolation is necessary.
c    * Alignment of images with frequency is not correct.
c    * The handling of geometry issues is abysmal.
c
c  Program parameters:
c    MAXIN      Maximum number of input files.
c    MAXOPN     Maximum number of input files to leave open.
c    MAXLEN     Size of buffer to hold all the input file names.
c    TOL        Tolerance. When the grid locations of two pixels differ
c               by less than "tol", they are taken as being the same
c               pixel (i.e. no interpolation done).
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mem.h'

      real      TOL
      integer   MAXIN, MAXLEN, MAXOPN
      parameter (MAXIN=8192, MAXLEN=MAXIN*64, MAXOPN=6, TOL=0.01)

      logical   defrms, dosen, dogain, dofreq, docar, exact, taper, mfs
      logical   cube, doalpha
      integer   axLen(3,MAXIN), i, itemp, k1(MAXIN), k2(MAXIN), length,
     *          lIn(MAXIN), lOut, lScr, lWts, nIn, nOpen, nOut(4),
     *          naxis, offset, nbw
      ptrdiff   pOut, pWts
      real      blctrc(4,MAXIN), extent(4), rms(MAXIN), sigt, xoff,
     *          yoff, bw, wt, cutoff
      double precision f, fout
      character inName*64, inbuf*(MAXLEN), outNam*64, version*72

      integer   len1
      character versan*72
      external  len1, versan
c-----------------------------------------------------------------------
      version = versan ('linmos',
     *                  '$Revision: 1.25 $',
     *                  '$Date: 2013/11/15 04:43:54 $')

c     Get and check inputs.
      call keyini
      nIn = 0
      offset = 0
      mfs = .false.
      cube = .false.
      call keyf('in',inName,' ')
      do while (inName.ne.' ')
        nIn = nIn + 1
        if (nIn.gt.MAXIN) call bug('f','Too many input cubes')
        length = len1(inName)
        if (offset+length.gt.MAXLEN)
     *    call bug('f','Input name buffer overflow')
        k1(nIn) = offset + 1
        k2(nIn) = offset + length
        Inbuf(k1(nIn):k2(nIn)) = inName
        offset = offset + length
        call keyf('in',inName,' ')
      enddo
      if (nIn.eq.0) call bug('f','No input cubes given')
      call keya('out',outNam,' ')
      if (outNam.eq.' ') call bug('f','No output name given')

      do i = 1, nIn
        call keyr('rms',rms(i),0.0)
        if (rms(i).lt.0)
     *      call bug('f','Non-positive rms noise parameter.')
      enddo
      call keyr('bw',bw,0.0)
      call keyi('bw',nbw,10)
      nbw = max(1,nbw)
      if (bw.le.0.0) nbw=1
      if (nbw.eq.1) bw=0.0
      call keyr('cutoff',cutoff,0.0)
      if (cutoff.ge.1.0.or.cutoff.lt.0.0) 
     * call bug('f','Specify a cutoff between 0 and 1') 

c     Get processing options.
      call getOpt(dosen,dogain,dofreq,taper,doalpha)
      call keyfin

      if (doalpha.and.nbw.eq.1)
     * call bug('w','opions=alpha requires bw>0')
      if (nIn.eq.1 .and. taper) call bug('f',
     *  'options=taper reduces to no correction for single pointings')
      if (nIn.eq.1 .and. dofreq .and. bw.eq.0.) 
     * call bug('f','options=frequency needs bw>0 for single pointings')

c     Open the files, determine the size of the output.  Determine the
c     grid system from the first map.
      if (nIn.le.MAXOPN) then
        nOpen = nIn
      else
        nOpen = MAXOPN - 1
      endif

      docar  = .false.
      defrms = .false.
      fout = 0.d0
      wt = 0
      do i = 1, nIn
        call xyopen(lIn(i),InBuf(k1(i):k2(i)),'old',3,axLen(1,i))
        if (max(axLen(1,i),axLen(2,i)).gt.MAXDIM)
     *    call bug('f','Input map is too big')

        call chkHdr(lIn(i), axLen(1,i), exact, blctrc(1,i), extent, f,
     *     cube)

        if (i.eq.1) then
          call rdhdi(lIn(1),'naxis',naxis,3)
          naxis = min(naxis,4)

          if (rms(1).eq.0.0) call rdhdr(lIn(1),'rms',rms(1),0.0)
          defrms = rms(1).le.0.0
          if (defrms) then
            call output('WARNING: Setting RMS to 1.0 for all images.')
            rms(1) = 1.0
          endif
        else
          docar = docar .or. .not.exact

          if (axLen(3,i).ne.axLen(3,1))
     *      call bug('f', 'Different lengths for 3rd axis')

          if (defrms) then
            rms(i) = 1.0
          else
            if (rms(i).le.0) call rdhdr(lIn(i),'rms',rms(i),0.0)
            if (rms(i).le.0) rms(i) = rms(i-1)
          endif
        endif
        wt = wt + 1.0/rms(i)**2
        if (.not.cube) fout = fout + log(f)/rms(i)**2
        if (i.gt.nOpen) call xyclose(lIn(i))
      enddo
      if (.not.cube) fout = exp(fout/wt)

c     Create the output image and make a header for it.
      do i = 1, 4
        itemp = nint(extent(i))
        if (abs(extent(i)-itemp).gt.TOL) then
          if (i.ge.3) itemp = nint(extent(i)+0.49)
          if (i.lt.3) itemp = nint(extent(i)-0.49)
        endif
        extent(i) = itemp
      enddo

      nOut(1) = nint(extent(3) - extent(1)) + 1
      nOut(2) = nint(extent(4) - extent(2)) + 1
      if (max(nOut(1),nOut(2)).gt.MAXDIM) then
        call bug('w','Output image is too large, clipping input')
        if (nOut(1).gt.MAXDIM) then
           extent(1)=extent(1)+(nOut(1)-MAXDIM+1)/2
           extent(3)=extent(3)-(nOut(1)-MAXDIM+1)/2
           nOut(1) = nint(extent(3) - extent(1)) + 1
        endif
        if (nOut(2).gt.MAXDIM) then
           extent(2)=extent(2)+(nOut(2)-MAXDIM+1)/2
           extent(4)=extent(4)-(nOut(2)-MAXDIM+1)/2
           nOut(2) = nint(extent(4) - extent(2)) + 1
        endif
      endif
      nOut(3) = axLen(3,1)
      if (nOut(3).eq.2) then
        if (bw.gt.0.0) then
          mfs = .true.
          call output('Doing mfs type pb correction')
          if (.not.doalpha) nOut(3)=1
        else
          nOut(3)=1
        endif
      else if (nOut(3).gt.2) then
         cube = .true.        
      endif
      if (dosen .or. dogain . or. dofreq) nOut(3) = 1
      nOut(4) = 1

      call xyopen(lOut,outNam,'new',naxis,nout)
      call mkHead(lIn(1),lOut,axLen(1,1),extent,version,docar,dosen,
     *            dogain,dofreq,fout,cube)
      call coInit(lOut)

c     Correct blctrc for the extent of the image.
      xoff = extent(1) - 1
      yoff = extent(2) - 1
      do i = 1, nIn
        blctrc(1,i) = blctrc(1,i) - xoff
        blctrc(2,i) = blctrc(2,i) - yoff
        blctrc(3,i) = blctrc(3,i) - xoff
        blctrc(4,i) = blctrc(4,i) - yoff
      enddo

c     Allocate memory.
      call MemAllop(pOut,nOut(1)*nOut(2),'r')
      call MemAllop(pWts,nOut(1)*nOut(2),'r')

c     Process each of the files.
      call scrOpen(lScr)
      call scrOpen(lWts)
      do i = 1, nIn
        call output('Processing image '//inbuf(k1(i):k2(i)))
        if (i.gt.nOpen) call xyopen(lIn(i),InBuf(k1(i):k2(i)),
     *                             'old',3,axLen(1,i))
        call process(i,lScr,lWts,lIn(i),lOut,memR(pOut),memR(pWts),
     *    axLen(1,i),axLen(2,i),nOut(1),nOut(2),nOut(3),dogain,
     *    dofreq,blctrc(1,i),rms(i),bw,mfs,nbw,cutoff)
        call xyclose(lIn(i))
      enddo

c     Determine the maximum noise to aim at.
      if (taper) then
        sigt = Rms(1)
        do i = 2, nIn
          sigt = max(sigt, Rms(i))
        enddo
      else
        sigt = 0.0
      endif

c     Go through the scratch file one more time, correcting for the
c     change in the weights, and writing out the final data.
      call lastPass(lOut,lScr,lWts,memR(pOut),memR(pWts),sigt,
     *              nOut(1),nOut(2),nOut(3),dosen)

c     Free memory.
      call MemFrep(pOut,nOut(1)*nOut(2),'r')
      call MemFrep(pWts,nOut(1)*nOut(2),'r')

c     Close down.
      call scrClose(lScr)
      call scrClose(lWts)
      call xyclose(lOut)

      end

***************************************************************** getOpt

      subroutine getOpt(dosen,dogain,dofreq,taper,doalpha)

      logical dosen,dogain,dofreq,taper,doalpha
c-----------------------------------------------------------------------
c  Get processing options.
c
c  Output:
c    dosen      True if we are to produce a sensitivity image.
c    dogain     True if we are to produce an image of the effective
c               gain.
c    dofreq     True if we are to produce an image of the effective
c               frequency.
c    taper      True if the output is to be tapered to achieve quasi-
c               uniform noise across the image.
c    doalpha    True if we are to produce a mosaic of both I and I*alpha
c-----------------------------------------------------------------------
      integer NOPTS
      parameter (NOPTS=5)
      logical present(NOPTS)
      character opts(NOPTS)*11
      data opts/'sensitivity','gain       ','taper      ','frequency  ',
     *          'alpha      '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)

      dosen  = present(1)
      dogain = present(2)
      taper  = present(3)
      dofreq = present(4)
      doalpha = present(5)

      if (dosen .and. dogain .or. dosen .and. dofreq .or. 
     *    dogain .and. dofreq) call bug('f',
     *  'Cannot do options=sensitivity,gains or freq simultaneously')
      end

**************************************************************** process

      subroutine process(fileno,lScr,lWts,lIn,lOut,Out,Wts,
     *  nx,ny,n1,n2,n3,dogain,dofreq,blctrc,rms,bw,mfs,nbw,cutoff)

      integer fileno,lScr,lWts,lIn,lOut
      integer nx,ny,n1,n2,n3,nbw
      real Out(n1,n2),Wts(n1,n2)
      real blctrc(4),rms,bw,cutoff
      logical dogain,dofreq,mfs
c-----------------------------------------------------------------------
c  First determine the initial weight to apply to each pixel and
c  accumulate info so that we can determine the normalisation factor
c  later on.  Then successively read each plane, apply the weight, and
c  accumulate the information in the scratch file.
c
c  Inputs:
c    fileno     Input file number.
c    lScr       Handle of the image scratch file.
c    lWts       Handle of the weights scratch file.
c    lIn        Handle of the input file.
c    lOut       Coordinate system of the output dataset.
c    nx,ny      Size of the image cube.
c    n1,n2,n3   Size of the output cube.
c    blctrc     Grid corrdinates, in the output, that the input maps to.
c    rms        Rms noise parameter.
c    dogain     True if we are to compute the gain function rather than
c               the normal mosaic or sensitivity function.
c    dofreq     True if we are to compute the effective frequency
c    bw         Bandwidth, to average the response in frequency
c    mfs        Use mfs I*alpha plane to do wideband pb correction
c    cutoff     Specify beam cutoff level

c    nbw        Number of bandwidth bins to use
c  Scratch:
c    In         Used for the interpolated version of the input.
c    Out        Used for the output.
c    Wts        Accumulation of the weights array.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      real      TOL
      parameter (TOL=0.01)

      logical interp, mask, dootf
      integer i, j, k, pbObj, xhi, xlo, xoff, yhi, ylo, yoff, iax
      integer nf, jf
      real    In(MAXDIM), pBeam(MAXDIM), Sect(4), sigma, xinc, yinc, wgt
      real    b,fac
      double precision x(3),xn(2),pra(2),pdec(2),f,fj,t
      character pbtype*16

      logical  hdprsnt
      external hdprsnt
c-----------------------------------------------------------------------
c     Determine if we have to interpolate.
      interp = .false.
      do i = 1, 4
        interp = interp .or. abs(blctrc(i)-anint(blctrc(i))).gt.TOL
      enddo
      interp = interp .or. nint(blctrc(3)-blctrc(1)+1).ne.nx
      interp = interp .or. nint(blctrc(4)-blctrc(2)+1).ne.ny

c     Determine the width of the guard band, if we have to interpolate.
      xinc = 0
      yinc = 0
      if (interp) then
        xinc = 2*(blctrc(3)-blctrc(1))/(nx-1)
        yinc = 2*(blctrc(4)-blctrc(2))/(ny-1)
      endif
      Sect(1) = max(1.0, blctrc(1) + xinc)
      Sect(2) = max(1.0, blctrc(2) + yinc)
      Sect(3) = min(real(n1),blctrc(3) - xinc)
      Sect(4) = min(real(n2),blctrc(4) - yinc)

c     Having determined the section of the output we can calculate,
c     round it to integer values.
      xlo = nint(Sect(1) + 0.5 - TOL)
      ylo = nint(Sect(2) + 0.5 - TOL)
      xhi = nint(Sect(3) - 0.5 + TOL)
      yhi = nint(Sect(4) - 0.5 + TOL)
      if (xlo.gt.xhi .or. ylo.gt.yhi) return

c     If we are interpolating, initialise the interpolation routine.
      if (interp) then
        Sect(1) = (nx-1)/(blctrc(3)-blctrc(1))*(xlo-blctrc(1)) + 1
        Sect(2) = (ny-1)/(blctrc(4)-blctrc(2))*(ylo-blctrc(2)) + 1
        Sect(3) = (nx-1)/(blctrc(3)-blctrc(1))*(xhi-blctrc(1)) + 1
        Sect(4) = (ny-1)/(blctrc(4)-blctrc(2))*(yhi-blctrc(2)) + 1
        call IntpIni(xhi-xlo+1,yhi-ylo+1,Sect)
      endif

c     Is there a mask file associated with this image?
      mask = hdprsnt(lIn,'mask')
      if (mask .and. interp) call bug('f',
     *  'Blanked pixels cannot be used when interpolating')
c
c     Ready to construct the primary beam object.
      call pntCent(lIn,pbtype,pra,pdec)
      x(1)=pra(1)
      x(2)=pdec(1)
      if (abs(pra(2)).gt.0.or.abs(pdec(2)).gt.0) then
        dootf=.true.
        xn(1)=pra(2)
        xn(2)=pdec(2)
      else
        dootf=.false.
      endif
      call coInit(lIn)
      call coFindAx(lIn,'frequency',iax)
      if (iax.ne.0) then
        call coFreq(lIn,'op',0d0,f)
      else
        f = 0
      endif

c     Loop over all planes.
      do k = 1, n3
        x(3) = k
        call xysetpl(lIn,1,k)
        if (interp) call IntpRIni

c     Handle mfs I*alpha plane
c      do pb calculations for nf freqs across band 
        nf = 1
        b = bw
        fac = 1
        if (mfs.or.(dofreq.and.bw.gt.0)) then
          nf = nbw
          b = 0
        endif

c       Get a plane from the scratch array.
        if (fileno.eq.1) then
          do j = 1, n2
            do i = 1, n1
              Wts(i,j) = 0
              Out(i,j) = 0
            enddo
          enddo
        else
          call getSec(lScr,Out,k,n1,n2,xlo,xhi,ylo,yhi)
          call getSec(lWts,Wts,k,n1,n2,xlo,xhi,ylo,yhi)
        endif

        do jf = 1, nf
          t = (jf-0.5d0)/nf-0.5d0
          fj = f + t*bw
c
          if (f.gt.0) fac = log(fj/f)
          if (dootf) then
            call pbInitcc(pbObj,pbtype,lOut,'aw/aw/ap',x,xn,fj,b)
          else
            call pbInitc(pbObj,pbtype,lOut,'aw/aw/ap',x,fj,b)
          endif
c         Determine the offsets to start reading.
          if (interp) then
            xoff = xlo
            yoff = 1
          else
            xoff = nint(blctrc(1))
            yoff = ylo - nint(blctrc(2)) + 1
          endif

c         Process this plane.
          do j = ylo, yhi
            call getDat(lIn,nx,xoff,yoff,xlo,xhi,j,pbObj,
     *                  In,pBeam,n1,interp,mask,mfs,fac,k,fj)
            yoff = yoff + 1

            do i = xlo, xhi
              if (pBeam(i).lt.cutoff) then
c               The weight is zero.
                go to 10
              endif

              if (dogain) then
c               Gain function.
                In(i) = pBeam(i)
              else if (dofreq) then
                In(i) = fj * pBeam(i)
              endif

c             Apply primary beam normalization.
              In(i) = In(i) / pBeam(i)
              sigma =  rms  / pBeam(i)

c             Weight by inverse variance.
              wgt = 1.0 / (sigma*sigma) / nf

c             Accumulate data.
              Out(i,j) = Out(i,j) + wgt*In(i)
              Wts(i,j) = Wts(i,j) + wgt
 10           continue
            enddo
          enddo
        enddo
c       Save the output.
        if (fileno.eq.1) then
          call putSec(lScr,Out,k,n1,n2,1,n1,1,n2)
          call putSec(lWts,Wts,k,n1,n2,1,n1,1,n2)
        else
          call putSec(lScr,Out,k,n1,n2,xlo,xhi,ylo,yhi)
          call putSec(lWts,Wts,k,n1,n2,xlo,xhi,ylo,yhi)
        endif

c       Release the primary beam object.
        call pbFin(pbObj)
      enddo
      call coFin(lIn)

      end

***************************************************************** getDat

      subroutine getDat(lIn,nx,xoff,yoff,xlo,xhi,j,pbObj,
     *                  In,Pb,n1,interp,mask,mfs,fac,k,fr)

      integer lIn,xoff,yoff,xlo,xhi,n1,j,pbObj,nx,k
      logical interp,mask,mfs
      real In(n1),Pb(n1),fac
      double precision fr
c-----------------------------------------------------------------------
c  Get a row of data (either from xyread or the interpolation routines).
c
c  Input:
c    lIn
c    xoff
c    yoff
c    xlo,xhi
c    n1
c    interp
c    mask
c    pbObj      Primary beam object.
c    mfs        Use I*alpha plane
c    fac        mfs factor
c    k          plane in input cube
c    fr         frequency
c  Output:
c    In         Image data.
c    Pb         Primary beam response (zeroed out where the data are
c               blanked).
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical   flags(MAXDIM)
      integer   i,ixl,ixh
      real      dat(MAXDIM), Pbd(MAXDIM)

      real     PbGet,PbDer
      external pbget, xyread
c-----------------------------------------------------------------------
c     Determine the primary beam.
      do i = xlo, xhi
        Pb(i) = PbGet(pbObj,real(i),real(j))
        if (mfs.and.k.eq.2) Pbd(i) = PbDer(pbObj,real(i),real(j))
      enddo

c     Zero the primary beam where the data are flagged.
      if (mask) then
        call xyflgrd(lIn,yoff,flags)
        do i = xlo, xhi
          if (.not.flags(i-xoff+1)) Pb(i) = 0
        enddo
      endif

c     Get the data.
      if (interp) then
        call IntpRd(lIn,yoff,In(xoff),xyread)
      else if (xoff.lt.1 .or. xoff+nx-1.gt.n1) then
        call xyread(lIn,yoff,Dat)
        do i = xlo, xhi
          In(i) = Dat(i-xoff+1)
        enddo
      else
        call xyread(lIn,yoff,In(xoff))
      endif
      if (mfs) then
        call xysetpl(lIn,1,3-k)
        if (interp) then
          call IntpRd(lIn,yoff,Dat,xyread)
        else 
          call xyread(lIn,yoff,Dat)
        endif
        if (.not.interp.and.(xoff.lt.1 .or. xoff+nx-1.gt.n1)) then
          ixl = xlo
          ixh = xhi
        else
          ixl = xoff
          ixh = xoff+nx-1
        endif
        do i = xlo, xhi
           if (k.eq.1) In(i) = In(i)+Dat(i-xoff+1)*fac
           if (k.eq.2.and.Pb(i).gt.0) In(i) = 
     *         In(i)-(Dat(i-xoff+1)+In(i)*fac)*fr*Pbd(i)/Pb(i)
        enddo
        call xysetpl(lIn,1,k)
      endif

      end

***************************************************************** getSec

      subroutine getSec(lScr,Out,k,n1,n2,xlo,xhi,ylo,yhi)

      integer lScr,k,n1,n2,xlo,xhi,ylo,yhi
      real Out(n1,n2)
c-----------------------------------------------------------------------
c  Read the region of interest from the scratch file.
c
c  Inputs:
c    lScr       Handle of the scratch file.
c    Out        Array containing the data to write.
c    k          Plane number.
c    n1,n2      Dimensions of the Out array.
c    xlo,ylo    Blc of area to write.
c    xhi,yhi    Trc of area to write.
c-----------------------------------------------------------------------
      integer j,length
      ptrdiff offset
c-----------------------------------------------------------------------
c     If the section of the x dimension that we want to right is pretty
c     well the entire x axis, read the whole lot.
      offset = (k-1)*n1
      offset = offset*n2 + (ylo-1)*n1 + (xlo-1)
      if (10*(xhi-xlo+1).ge.8*n1) then
        length = n1*(yhi-ylo-1) + (n1-xlo+1) + xhi
        call scrRead(lScr,Out(xlo,ylo),offset,length)
      else
        length = xhi - xlo + 1
        do j = ylo, yhi
          call scrRead(lScr,Out(xlo,j),offset,length)
          offset = offset + n1
        enddo
      endif

      end

***************************************************************** putSec

      subroutine putSec(lScr,Out,k,n1,n2,xlo,xhi,ylo,yhi)

      integer lScr,k,n1,n2,xlo,xhi,ylo,yhi
      real Out(n1,n2)
c-----------------------------------------------------------------------
c  Write out to the scratch file the region of interest.
c
c  Inputs:
c    lScr       Handle of the scratch file.
c    Out        Array containing the data to write.
c    k          Plane number.
c    n1,n2      Dimensions of the Out array.
c    xlo,ylo    Blc of area to write.
c    xhi,yhi    Trc of area to write.
c-----------------------------------------------------------------------
      integer j,length
      ptrdiff offset
c-----------------------------------------------------------------------
c     Try and block it into one call if that is possible.
      offset = (k-1)*n1
      offset = offset*n2 + (ylo-1)*n1 + (xlo-1)
      if (xlo.eq.1 .and. xhi.eq.n1) then
        length = n1*(yhi-ylo-1) + (n1-xlo+1) + xhi
        call scrWrite(lScr,Out(xlo,ylo),offset,length)
      else
        length = xhi - xlo + 1
        do j = ylo, yhi
          call scrWrite(lScr,Out(xlo,j),offset,length)
          offset = offset + n1
        enddo
      endif

      end

*************************************************************** lastPass

      subroutine lastPass(lOut,lScr,lWts,Out,Wts,sigt,n1,n2,n3,dosen)

      integer lOut,lScr,lWts,n1,n2,n3
      real sigt,Out(n1,n2),Wts(n1,n2)
      logical dosen
c-----------------------------------------------------------------------
c  Read in the data from the scratch file, multiply by the scale factor,
c  and then write out the data.
c
c  Inputs:
c    lOut       Handle of the output image file.
c    lScr       Handle of the input image file.
c    lWts       Handle of the input weights file.
c    sigt       Critical noise sigma.  Zero for no taper.
c    n1,n2,n3   Size of the output cube.
c    dosen      Determine the sensitivity function.
c  Scratch:
c    Wts        Array containing the weights to be applied.
c    Out        Used to store a plane of the output image.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical doflag, doneflag, flags(MAXDIM)
      integer i, j, k
      real    scale
c-----------------------------------------------------------------------
      doflag   = .false.
      doneflag = .false.

c     Loop over all planes.
      do k = 1, n3
c       Get the image and weight planes.
        call getSec(lScr,Out,k,n1,n2,1,n1,1,n2)
        call getSec(lWts,Wts,k,n1,n2,1,n1,1,n2)
        call xysetpl(lOut,1,k)

        do j = 1, n2
          do i = 1, n1
            flags(i) = Wts(i,j).gt.0
            if (.not.flags(i)) then
              Out(i,j) = 0
              doflag = .true.
            else
              if (sigt.eq.0.0) then
c               No taper.
                scale = 1.0
              else
c               Apply taper to suppress noise near the edges.
                scale = min(1.0, sigt*sqrt(Wts(i,j)))
              endif

              if (dosen) then
c               Sensitivity function.
                Out(i,j) = scale / sqrt(Wts(i,j))
              else
c               Mosaic, gain or freq function.
                Out(i,j) = scale * Out(i,j) / Wts(i,j)
              endif
            endif
          enddo

c         Write flags if a bad pixel has been found.
          if (doflag .and. .not.doneflag) then
            call catchUp(lOut,j,n1,n2,k)
            doneflag = .true.
          endif

          if (doflag) call xyflgwr(lOut,j,flags)

c         Write out the data.
          call xywrite(lOut,j,Out(1,j))
        enddo
      enddo

      end

**************************************************************** catchUp

      subroutine catchUp(lOut,j0,n1,n2,n3)

      integer lOut,j0,n1,n2,n3
c-----------------------------------------------------------------------
c  Write out a batch of "good" flags to the image mask file. This is
c  to catch up on the flags that should have been written earlier.
c
c  Input:
c    lOut
c    j0
c    n1
c    n2
c    n3
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer i,j,k
      logical flags(MAXDIM)
c-----------------------------------------------------------------------
c     Initialise the flags array.
      do i = 1, n1
        flags(i) = .true.
      enddo

c     Flag as good all planes before the first bad plane.
      do k = 1, n3-1
        call xysetpl(lOut,1,k)
        do j = 1, n2
          call xyflgwr(lOut,j,flags)
        enddo
      enddo

c     Flag as good all rows before the first bad row.
      call xysetpl(lOut,1,n3)
      do j = 1, j0-1
        call xyflgwr(lOut,j,flags)
      enddo

      end

***************************************************************** mkHead

      subroutine mkHead(lIn, lOut, axLen, extent, version, docar, dosen,
     *  dogain, dofreq, f, cube)

      integer   lIn, lOut, axLen(3)
      real      extent(4)
      character version*72
      logical   dosen, dogain, dofreq, docar, cube
      double precision f
c-----------------------------------------------------------------------
c  Make up the header of the output file.
c
c  Input:
c    lIn        Handle of the input file which is to be used as a
c               template.
c    lOut       Handle of the output file.
c    axLen      Size of the input image.
c    extent     Expanded extent of the output.
c    docar      True if we are to label with RA---CAR, DEC--CAR
c    dosen      True if the sensitivity function is being evaluated.
c    dogain     True if the gain function is being evaluated.
c    dofreq     True if the effective frequency is being evaluated 
c    f          Frequency for output header
c-----------------------------------------------------------------------
      double precision crpix
      character ctype*16
c-----------------------------------------------------------------------
c     Start with a verbatim copy of the input header.
      call headcp(lIn, lOut, 0, 0, 0, 0)

c     Apply sub-imaging.
      call rdhdd(lIn,  'crpix1', crpix, dble(axLen(1)/2+1))
      crpix = crpix - dble(extent(1)-1.0)
      call wrhdd(lOut, 'crpix1', crpix)

      call rdhdd(lIn,  'crpix2', crpix, dble(axLen(2)/2+1))
      crpix = crpix - dble(extent(2)-1.0)
      call wrhdd(lOut, 'crpix2', crpix)
      
c     Set the frequency (average of all the inputs) for mfs image     
      if (.not.cube) call wrhdd(lOut, 'crval3', f)

c     Write the output projection as plate carrée?
      if (docar) then
        call rdhda(lIn,  'ctype1', ctype, 'RA---CAR')
        if (ctype(5:5).eq.'-') ctype(5:) = '-CAR'
        call wrhda(lOut, 'ctype1', ctype)

        call rdhda(lIn,  'ctype2', ctype, 'DEC--CAR')
        if (ctype(5:5).eq.'-') ctype(5:) = '-CAR'
        call wrhda(lOut, 'ctype2', ctype)
      endif

c     Indicate that it's primary-beam corrected.
      call wrhda(lOut, 'pbtype', 'SINGLE')

c     Update history.
      call hisopen (lOut, 'append')
      call hiswrite(lOut, 'LINMOS: Miriad ' // version)
      call hisinput(lOut, 'LINMOS')
      if (dosen) then
        call hiswrite(lOut,
     *    'LINMOS: The image is the rms noise function.')
      else if (dogain) then
        call hiswrite(lOut,
     *    'LINMOS: The image is the gain function.')
      else if (dofreq) then
        call hiswrite(lOut,
     *    'LINMOS: The image is the effective frequency function')
      endif
      call hisclose(lOut)

      end

***************************************************************** chkHdr

      subroutine chkHdr(lIn, axLen, exact, blctrc, extent, f, cube)

      integer   lIn, axLen(3)
      logical   exact, cube
      real      blctrc(4), extent(4)
      double precision f
c-----------------------------------------------------------------------
      logical   doInit, doWarn
      integer   iax, k
      double precision cdelt(3,2), cdelt1(2), crpix(3,2), crval(3,2),
     *          discr, frq(2), lat(2), lng(2), x, y, z
      character cax*1, ctype(3,2)*16, cunit*16

      save cdelt, cdelt1, crpix, crval, ctype, doInit, frq, lat, lng

      character itoaf*1
      external  itoaf

      data doInit /.true./, doWarn/.true./
c-----------------------------------------------------------------------
      if (doInit) then
        k = 1
        call rdhda(lIn, 'cunit3',cunit,' ')
        cube = cube.or.cunit.ne.'GHz'
      else
        k = 2
      endif

c     Read the axis descriptors.
      do iax = 1, 3
        cax = itoaf(iax)
        call rdhdd(lIn, 'crpix'//cax, crpix(iax,k), 0d0)
        call rdhdd(lIn, 'cdelt'//cax, cdelt(iax,k), 0d0)
        call rdhdd(lIn, 'crval'//cax, crval(iax,k), 1d0)
        call rdhda(lIn, 'ctype'//cax, ctype(iax,k), ' ')
      enddo
      

c     Projection-plane coordinates of pixel (1,1,1).
      cdelt1(k) = cdelt(1,k) / cos(crval(2,k))
      x = (1d0 - crpix(1,k))*cdelt1(k)
      y = (1d0 - crpix(2,k))*cdelt(2,k)
      z = (1d0 - crpix(3,k))*cdelt(3,k)

c     Small-field approximation.
      lng(k) = x + crval(1,k)
      lat(k) = y + crval(2,k)
      frq(k) = z + crval(3,k)

      exact = .true.
      if (doInit) then
        blctrc(1) = 1.0
        blctrc(2) = 1.0
        blctrc(3) = real(axLen(1))
        blctrc(4) = real(axLen(2))

        extent(1) = blctrc(1)
        extent(2) = blctrc(2)
        extent(3) = blctrc(3)
        extent(4) = blctrc(4)

        doInit = .false.

      else
c       Are the celestial coordinate systems equivalent to within a
c       shift?
        do iax = 1, 2
          discr = 0.01d0*abs(cdelt(iax,1))
          exact = exact .and.
     *      ctype(iax,k).eq.ctype(iax,1) .and.
     *      abs(cdelt(iax,k)-cdelt(iax,1)).lt.discr .and.
     *      abs(crval(iax,k)-crval(iax,1)).lt.discr
        enddo

        if (exact) then
          blctrc(1) = 1.0 + crpix(1,1) - crpix(1,2)
          blctrc(2) = 1.0 + crpix(2,1) - crpix(2,2)
          blctrc(3) = blctrc(1) + real(axLen(1) - 1)
          blctrc(4) = blctrc(2) + real(axLen(2) - 1)
        else
c         Approximate pixel coordinates of BLC and TRC in the coordinate
c         system of the first image.
          blctrc(1) = 1.0 + (lng(2) - lng(1))/cdelt1(1)
          blctrc(2) = 1.0 + (lat(2) - lat(1))/cdelt(2,1)
          blctrc(3) = blctrc(1) + real(axLen(1)-1)*cdelt1(2) /cdelt1(1)
          blctrc(4) = blctrc(2) + real(axLen(2)-1)*cdelt(2,2)/cdelt(2,1)
        endif

        if (blctrc(3).lt.blctrc(1) .or. blctrc(4).lt.blctrc(2))
     *    call bug('f','Signs of cdelt of the inputs are not identical')

c       Check third axis alignment.
        discr = 0.5d0*abs(cdelt(3,1))
        if (max(abs(frq(2)-frq(1)),abs(cdelt(3,2)-cdelt(3,1))).gt.discr)
     *    then
          if (doWarn) call bug('w', 'Third axis of inputs do not align')
          doWarn=.false.
        endif

c       Update the extent.
        extent(1) = min(blctrc(1), extent(1))
        extent(2) = min(blctrc(2), extent(2))
        extent(3) = max(blctrc(3), extent(3))
        extent(4) = max(blctrc(4), extent(4))
      endif
      f = frq(k)

      end

**************************************************************** pntCent

      subroutine pntCent(lIn,pbtype,pra,pdec)

      integer lIn
      double precision pra(2),pdec(2)
      character pbtype*(*)
c-----------------------------------------------------------------------
c  Determine the pointing centre and the primary beam type.
c
c  Inputs:
c    lIn        Handle of the input image dataset
c  Output:
c    pra,pdec   1:Pointing centre RA and DEC, in radians.
c               2:Pointing centre for next or prev otf mosaic position
c    pbtype     Primary beam type. This will normally just be the
c               name of a telescope (e.g. 'HATCREEK' or 'ATCA'), but it
c               can also be 'GAUS(xxx)', where xxx is a Gaussian primary
c               beam size, with its FWHM given in arcseconds.  For
c               example 'GAUS(120)' is a Gaussian primary beam with
c               FWHM 120 arcsec.
c-----------------------------------------------------------------------
      integer mit,size,iostat,ival(2)
      character string*16

      logical  hdprsnt,otf
      integer  hsize
      external hdprsnt, hsize
c-----------------------------------------------------------------------
c     Zero the otf parameters
      pra(2)=0
      pdec(2)=0
c     Is the mosaic table present?
      if (hdprsnt(lIn, 'mostable')) then
c       Yes, read it.
        call haccess(lIn, mit, 'mostable', 'read', iostat)
        if (iostat.ne.0) call bugno('f',iostat)

c       Check its size.
        size = hsize(mit)

c       Check version
        call hreadi(mit,ival,0,8,iostat)
        otf = ival(2).eq.2
        
        if ((size.ne.56.and..not.otf).or.(size.ne.72.and.otf))
     *    call bug('f','Bad size for mosaic table')

c       Read (RA,Dec).
        call hreadd(mit,pra(1),16,8,iostat)
        if (iostat.eq.0) call hreadd(mit,pdec(1),24,8,iostat)

c       Read the primary beam type.
        if (iostat.eq.0) call hreadb(mit,string,32,16,iostat)
        pbtype = string
        
c       Read the otf parameters
        if (otf) then
          if (iostat.eq.0) call hreadd(mit,pra(2),56,8,iostat)
          if (iostat.eq.0) call hreadd(mit,pdec(2),64,8,iostat)        
        endif
        
        call hdaccess(mit,iostat)
        if (iostat.ne.0) call bugno('f',iostat)

      else
c       No, treat a regular synthesis image.
        call rdhdd(lIn,'crval1',pra(1), 0.d0)
        call rdhdd(lIn,'crval2',pdec(1),0.d0)
        call pbRead(lIn, pbtype)
      endif

c
      end
