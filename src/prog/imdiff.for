      program imdiff

c= IMDIFF - Shift and expand an image to make it match another.
c& rjs
c: image-analysis
c+
c       IMDIFF is a MIRIAD task which finds optimum parameters (in a
c       maximum likelihood sense) for making one image approximate
c       another image.  The parameters are an amplitude scale factor, dc
c       offset, shifts in x and y direction and an expansion.
c       Alternately any of these parameters can be fixed at a given
c       value, and the others allowed to vary.  This task assumes that
c       the rms noise levels in both images are identical (If this is
c       not the case then one of the images should be scaled to make it
c       the case).  Also noise in the two images is assumed to be
c       uncorrelated.  If either of these conditions are violated, the
c       algorithm will not perform optimally when the signal to noise
c       ratio is poor.
c
c       This task is intended to find only small shifts and expansions.
c       It will probably fail if large shifts and expansions are
c       present.  The task SHIFTY can handle large, integral, shifts.
c       This task currently only handles a rectangular region of
c       interest.  It does not handle blanked pixels correctly.
c@ in1
c       The first input image. This is considered to be the "reference"
c       image. No default.
c@ in2
c       The second input image.  This is considered to be the image that
c       must be adjusted to make it look like IN1.  No default.
c@ adjust
c       This gives the name of an output image data-set, being a version
c       of second image after adjustment.  The default is not to output
c       an adjusted image.
c@ resid
c       An output image consisting of the difference between IN1 and the
c       adjusted form of IN2.  It is generally best to look at the
c       residuals, to check that they are noise-like.  The residuals are
c       defined as:
c
c         Resid(x,y) = In1(x,y)
c                       - Amp*In2(Expand*x+Xshift, Expand*y+YShift)
c                       - Offset
c
c       (Here X and Y are relative to IN2's reference pixel).
c       The default is not to create the residuals.
c@ region
c       The region of interest. Currently this must be a rectangular
c       region. The default is the entire image.
c@ guard
c       The task cannot work right to the edge of the image, because the
c       shifts and expansions might mean that there is no corresponding
c       point between images near the image edges.  Consequently a guard
c       band is needed.  GUARD gives the width of this band, in pixels.
c       In the output residuals, this band will be set to zero.
c       Default is 10.
c
c       The following five parameters give the initial estimates for the
c       various parameters that IMDIFF is trying to find.  If the
c       parameters are fixed, then IMDIFF will not change them from
c       their initial settings.
c@ xshift
c       This gives the initial value, in pixels, of the x-shift.
c       This should be only +/- a few pixels. Default is 0.
c@ yshift
c       This gives the initial value, in pixels, of the y-shift.
c       Default is 0.
c@ expand
c       This gives the initial expansion.  Only small expansions can be
c       handled (e.g. approximately in the range 0.95 to 1.05).  This
c       should be adequate for proper motion studies.  Give two values
c       if you are allowing the x- and y-expansions to vary
c       independently, one value otherwise.  Default is 1.
c@ amp
c       This gives the initial amplitude scale factor. Default is 1.
c@ offset
c       This gives the initial dc offset to apply to IN2. The default
c       is 0.
c@ options
c       Task enrichment options.  Minimum match is active.  These
c       determine what to solve for.  Possible values are:
c         noamplitude Do not adjust the amplitude of the second image.
c                     The default is to adjust the amplitude.
c         nooffset    Do not adjust the offset of the second image.
c                     The default is to adjust the offset.
c         noxshift    Do not shift the second image in x.
c                     The default is to optimise the shift in x.
c         noyshift    Do not shift the second image in y.
c                     The default is to optimise the shift in y.
c         noexpand    Do not apply an expansion factor to the second
c                     image.  The default is to apply an expansion
c                     factor in y.
c         expand      Expand the x and y axes independently.  The
c                     default is that the expansions for the two axes
c                     are the same.
c--
c  This finds the minimum mistmatch between two images.  IMDIFF calls
c  POWELL which does the minimisation. POWELL could be substituted
c  with practically any multivariate minimisation routine, which does
c  not require derivatives.  POWELL calls ERREVAL, which in turn calls
c  DIFF and CALCABC.  DIFF calls some routines which interpolate the
c  image to the right size.  Diff then calculates some statistics.
c  ERREVAL calls CALCABC with these statistics to calculate the error
c  parameter.
c
c  The program is complicated by having to do i/o all the time, and by
c  being written in a vectorisable form.  With infinite memory and an
c  ideal compiler, it would be much simpler.
c-----------------------------------------------------------------------
c  History:
c    rjs  Dark-ages Original Werong version.
c    rjs  11dec90   Converted it to a Miriad program.
c    rjs  18dec90   Changed BLC,TRC to REGION keyword.
c    rjs  04dec92   Check whether guard band is exceeded.
c    nebk 07dec92   Convert VARY to OPTIONS, add independent x and y
c                   expansion and output ADJUST image.  Write correct
c                   headers (or any header) in output files.  Remove
c                   some of the scratch file messing about.  Fix bug
c                   with getting reference pixel.  Make it work on
c                   cubes.
c    rjs  09mar93   Add history.  Make it work on N dimensional files;
c                   thanks to nebk for all of above work fixing my poor
c                   effort.
c    rjs  16jul93   Doc changes.
c    rjs  05aug93   Fix various formating botches.  NEBK does a sloppy
c                   job.
c    nebk 19sep94   Fix typo in options=expand in subroutien getopt
c    rjs  07dec99   Fix writing of history for "adjust" image.
c    rjs  18sep05   Fix type of boxes argument.
c    mchw 09feb09   Format change for ATA.
c    mrc  12nov10   headcopy -> headcp
c    mhw  17jan12   Use ptrdiff for scr routines
c
c  Bugs and Shortcomings:
c   * This should really be part of "shifty".
c   * All the pissing about with scratch files could probably be
c     removed.  This is hangover from Werong days.
c
c-----------------------------------------------------------------------
      character version*(*)
      parameter (version='version 20-jul-12')
      include 'maxdim.h'
      include 'maxnax.h'
      include 'imdiff.h'
      integer maxfun,maxboxes
      parameter (maxfun=500,maxboxes=128)
c
      character Infile1*80,InFile2*80,resid*80,adjust*80,aline*80
      integer i,j,k,lu(2),lRes,ifail,ladj,nexp
      integer naxis,nsize(MAXNAX,2),nsizo(MAXNAX),axnum(MAXNAX),naxisd
      integer blc(MAXNAX), trc(MAXNAX),dims(MAXNAX),indx(MAXNAX)
      real z(10),e(10),w(100),a,b,c,sigma,da,db,crpix
      double precision sx,sy,sxx,syy,sxy
      integer n,nvary
      real Buff(maxdim)
      integer Boxes(maxboxes)
      character perr(4)*40,bunit*9
      ptrdiff off
c
c  Externals.
c
      integer packer,unpacker
      character itoaf*1
      logical Inc3More
      external erreval,packer,unpacker
c
      data perr/'Maximum change does not alter function  ',
     *          'Accuracy limited by errors in calcfx    ',
     *          'Max iterations performed within Powell  ',
     *          'Max function values evaluated in Powell '/
c
c  Get the parameters that we have to work with.
c
      call output('Imdiff: '//version)
      call keyini
      call keya('in1', Infile1, ' ')
      call keya('in2', InFile2, ' ')
      call BoxInput('region', Infile1, boxes, maxboxes)
      call keya('adjust', adjust, ' ')
      call keya('resid', resid, ' ')
      call keyr('xshift', xshift,0.0)
      call keyr('yshift', yshift, 0.0)
      call mkeyr('expand', expand, 2, nexp)
      call keyr('amp', amp, 1.0)
      call keyr('offset', offset, 0.0)
      call keyi('guard', Guard, 10)
      call getopt(vary)
      call keyfin
      if (InFile1.eq.' ' .or. InFile2.eq.' ')
     *  call bug('f','Input files not all specified')
c
c Fill in defaults for expand
c
      xyequal = .false.
      if (index(vary,'E').ne.0) xyequal = .true.
      do i = 1, 2
        if (expand(i).eq.0.0) expand(i) = 1.0
      enddo
c
c  Open the template image.
c
      call xyopen(lu(1), InFile1, 'old', MAXNAX, nsize(1,1))
      if (nsize(1,1).gt.maxdim)
     *  call bug('f','Input images too big for me!')
      call rdhdi(lu(1), 'naxis', naxis, 0)
      if (naxis.eq.0) call bug('f', 'Image has no dimensions')
      naxis = min(naxis,MAXNAX)
c
      call BoxSet(boxes, naxis, nsize(1,1), 's')
      call BoxInfo(boxes, naxis, blc, trc)
c
      do i = 1, naxis
        nsizo(i) = trc(i) - blc(i) + 1
        if (nsizo(i).gt.1) naxisd = i
      enddo
      no1 = nsizo(1)
      no2 = nsizo(2)
c
c  Open the file to be compared with the template
c
      call xyopen(lu(2), InFile2, 'old', naxis, nsize(1,2))
      do i = 1, naxis
        if (nsize(i,1).ne.nsize(i,2))
     *    call bug('f','Input images must be the same size')
      enddo
      call rdhdr(lu(2), 'crpix1', xo, real(nsize(1,2)/2+1))
      xo = xo - blc(1) + 1
      call rdhdr(lu(2), 'crpix2', yo, real(nsize(2,2)/2+1))
      yo = yo - blc(2) + 1
c
      call rdhda(lu(2), 'bunit', bunit, ' ')
c
c Open the adjusted and residual output images now and
c copy the header
c
      do i = 1, naxis
        axnum(i) = i
      enddo
      if (adjust.ne.' ') then
        call xyopen(lAdj, Adjust, 'new', naxis, nsizo)
        call headcp(lu(1), ladj, naxis, axnum, blc, trc)
        call hisopen(lAdj, 'append')
        call hiswrite(lAdj, 'IMDIFF: Miriad ImDiff: '//version)
        call hisinput(lAdj, 'IMDIFF')
        call hiswrite(lAdj, 'IMDIFF: This is an adjusted image')
        call hisclose(lAdj)
        do i = 1, naxis
          call rdhdr(lu(1), 'crpix'//itoaf(i), crpix,
     *                real(nsize(i,1)/2+1))
          call wrhdr(ladj, 'crpix'//itoaf(i), crpix-blc(i)+1.0)
        enddo
        call wrhda(ladj,'bunit',bunit)
      endif
      if (resid.ne.' ') then
        call xyopen(lres, resid, 'new', naxis, nsizo)
        call headcp(lu(1), lres, naxis, axnum, blc, trc)
        call hisopen(lres, 'append')
        call hiswrite(lres, 'IMDIFF: Miriad ImDiff: '//version)
        call hisinput(lres, 'IMDIFF')
        call hiswrite(lres, 'IMDIFF: This is a residual image')
        call hisclose(lres)
        do i = 1, naxis
          call rdhdr(lu(1), 'crpix'//itoaf(i), crpix,
     *                real(nsize(i,1)/2+1))
          call wrhdr(lres, 'crpix'//itoaf(i), crpix-blc(i)+1.0)
        enddo
        call wrhda(lres, 'bunit', bunit)
      endif
c
c  Loop over the other dimensions; use estimates from previous plane as
c  the initial estimate.
c
      call scropen(lIn1)
      call scropen(lIn2)
      call IncIni(naxis,nsizo,dims)
      do while (Inc3More(naxis,nsizo,dims))
        if (naxis.gt.2) then
          call IncOff(naxis,dims,blc,indx)
          call mitoaf(indx(3),naxis-2,aline,k)
          if (naxisd.gt.2) then
            call output('-------------------------------------------')
            call output('Begining plane '//aline(1:k))
          endif
          call xysetpl(lu(1),naxis-2,indx(3))
          call xysetpl(lu(2),naxis-2,indx(3))
          if (adjust.ne.' ') call xysetpl(ladj,naxis-2,dims(3))
          if (resid.ne.' ') call xysetpl(lres,naxis-2,dims(3))
        endif
c
c  Copy the subwindows to scratch files
c
        do j = blc(2), trc(2)
          call xyread(lu(1), j, Buff)
          off = (j-blc(2))*no1
          call scrwrite(lIn1, Buff(blc(1)),off,no1)
c
          call xyread(lu(2), j, Buff)
          call scrwrite(lIn2, Buff(blc(1)),off,no1)
        enddo
c
c  Do the minimisation
c
        nvary = packer(z)
        if (nvary.gt.0) then
          do i = 1, nvary
            e(i) = 0.1
          enddo
          call powell(z, e, nvary, c, 5.0, 1, 100, erreval, w,
     *                 maxfun, ifail)
          if (ifail.ne.0) call bug('w', perr(ifail))
          nvary = unpacker(z)
        endif
c
c  Calculate the statistics about the optimum.
c
        call diff(lIn1, lIn2, no1, no2, Guard, n, sx, sy, sxx,
     *             syy, sxy)
        call calcabc(sx, sy, sxx, syy, sxy, n, a, b, c)
        if (c.le.0) then
          sigma = 0
        else
          sigma = sqrt(C/n)
        endif
        da =(Sxx + 2*a*Sxy - 2*a*b*Sx + a**2*Syy
     *         - 2*a**2*b*Sy + N*(a*b)**2) / (1+a**2)**2
        da = sigma/sqrt(da)
        db = sigma/sqrt(real(n))
c
c  Now print out some info on the minimum
c
        write(aline,100)   'Rms Noise Estimate:             ',sigma
100     format(a,1pe13.6)
        call output(aline)
        write(aline,200)   'Minimum located at X-Shift:     ',xshift
200     format(a,f12.3)
        call output(aline)
        write(aline,200)   '                   Y-Shift:     ',yshift
        call output(aline)
        if (xyequal) then
          write(aline,200) '                   Expansion:   ',
     *                      expand(1)
          call output(aline)
        else
          write(aline,200) '                   X-Expansion: ',
     *                      expand(1)
          call output(aline)
          write(aline,200) '                   Y-Expansion: ',
     *                      expand(2)
          call output(aline)
        endif
        write(aline,300)   '                   Amplitude:   ',A,
     *                    ' +/- ',da
300     format(a,1pe13.6,a,1pe13.6)
        call output(aline)
        write(aline,300)   '                   Offset:      ',B,
     *                    ' +/- ',db
        call output(aline)
c
c Generate adjusted image.
c
        if (Adjust.ne.' ')
     *    call GetAdj(lIn2, ladj, no1, no2, Guard, a, b)
c
c Generate residuals
c
        if (Resid.ne.' ')
     *    call GetRes(lIn1, lIn2, lres, no1, no2, Guard, a, b)
      enddo
c
c Close and delete scratch files
c
      call scrclose(lIn1)
      call scrclose(lIn2)
c
c Close all the other files.
c
      if (adjust.ne.' ') call xyclose(ladj)
      if (resid.ne.' ') call xyclose(lres)
      call xyclose(lu(1))
      call xyclose(lu(2))
c
      end
c***********************************************************************
      subroutine calcabc(sx,sy,sxx,syy,sxy,n,a,b,c)
c
      integer n
      real a,b,c
      double precision sx,sy,sxx,syy,sxy
c
c  Calculate optimum gain, offset, and the corresponding error.
c
c-----------------------------------------------------------------------

      include 'imdiff.h'
      real a0,a1,a2
c
      a0 = -(sxy - sx*sy/n)
      a1 = (sy*sy-sx*sx)/n + sxx - syy
      a2 = sxy - sx*sy/n
      a = amp
      if (index(vary,'A').ne.0) a = (-a1+sqrt(a1*a1-4*a0*a2))/(2.0*a2)
      b = offset
      if (index(vary,'O').ne.0) b = (sy - a*sx)/n
      C =     a**2*sxx - 2.0*a*sxy
     *             + 2*a*b*sx + syy
     *             - 2*b*sy + n*b**2
      C = C/(1+a**2)
c
      end
c***********************************************************************
      integer function packer(z)
c
      real z(5)
c
c  Determine which of the parameters are variables, and pack them into
c  the Z array.
c
c-----------------------------------------------------------------------
      include 'imdiff.h'
      integer i
      i = 0
      if (index(vary,'E').ne.0) then
        i = i + 1
        z(i) = 1000.0*(expand(1)-1)
      endif
      if (index(vary,'X').ne.0) then
        i = i + 1
        z(i) = xshift
      endif
      if (index(vary,'Y').ne.0) then
        i = i + 1
        z(i) = yshift
      endif
      if (index(vary,'I').ne.0) then
        i = i + 1
        z(i) = 1000.0*(expand(1)-1)
      endif
      if (index(vary,'J').ne.0) then
        i = i + 1
        z(i) = 1000.0*(expand(2)-1)
      endif
      packer = i
      end
c***********************************************************************
      integer function unpacker(z)
c
      real z(5)
c
c  Determine which of the parameters are variables, and unpack them from
c  the Z array.
c
c-----------------------------------------------------------------------
      include 'imdiff.h'
      integer i
      i = 0
      if (index(vary,'E').ne.0) then
        i = i + 1
        expand(1) = 1 + 0.001*z(i)
      endif
      if (index(vary,'X').ne.0) then
        i = i + 1
        xshift = z(i)
      endif
      if (index(vary,'Y').ne.0) then
        i = i + 1
        yshift = z(i)
      endif
      if (index(vary,'I').ne.0) then
        i = i + 1
        expand(1) = 1 + 0.001*z(i)
      endif
      if (index(vary,'J').ne.0) then
        i = i + 1
        expand(2) = 1 + 0.001*z(i)
      endif
      unpacker = i
      end
c***********************************************************************
      subroutine erreval(nd, z, c)
c
      integer nd
      real z(nd), c
c
c  Calculate the error for a given shift and expansion.
c
c-----------------------------------------------------------------------
      include 'imdiff.h'
      double precision sx, sy, sxx, syy, sxy
      real a, b
      integer n, nvary
c
c  Externals.
c
      integer unpacker
      external unpacker
c
c  Perform the desired expansion/contraction.
c
      nvary = unpacker(z)
      if (nvary.ne.nd) call bug('f','Inconsistent no. of variables')
c
c  Calculate and save info.
c
      call diff(lIn1, lIn2, no1, no2, Guard, n, sx, sy, sxx, syy, sxy)
      call calcabc(sx, sy, sxx, syy, sxy, n, a, b, c)
c
      end
c***********************************************************************
      subroutine diff(lIn1, lIn2, no1, no2, Guard, n, sx, sy, sxx,
     *                 syy, sxy)
c
      integer no1, no2, lIn1, lIn2, Guard
      integer n
      double precision sx, sy, sxx, syy, sxy
c
c  This notionally applies a shift and expand to the image IN1 and
c  compares it with IN2.
c  "Cubic convolution" interpolation is used to regrid the image.
c  To avoid needing to worry about edge conditions, I have ignored a
c  band "guard" pixels wide around the image.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real RBuf(8*maxdim), In1(maxdim), In2(maxdim)
      integer indices(4), IBuf(2*maxdim), nints
      integer i, j, xmin, xmax, ymin, ymax
      ptrdiff off
c
c  Intialise various rubbish.
c
      xmin = 1 + guard
      xmax = no1 - guard
      ymin = 1 + guard
      ymax = no2 - guard
      n = (ymax-ymin+1)*(xmax-xmin+1)
      sx = 0
      sy = 0
      sxx = 0
      syy = 0
      sxy = 0
c
c  Initialise the interpolation.
c
      call IntpInit(xmin, xmax, indices, IBuf, nints, RBuf)
c
c  Get the interpolated row, get the normal row, and calculate some
c  statistics.
c
      do j = ymin, ymax
        call Intp(lIn2, j, xmin, xmax, no1, no2, indices,
     *             IBuf, nints, RBuf, In2)
        off = (j-1)*no1
        call scrread(lIn1, In1, off, no1)
        do i = xmin, xmax
          sx = sx + In2(i)
          sxx = sxx + In2(i)*In2(i)
          sy = sy + In1(i)
          syy = syy + In1(i)*In1(i)
          sxy = sxy + In2(i)*In1(i)
        enddo
      enddo
c
      end
c***********************************************************************
      subroutine GetAdj(lIn2, ladj, no1, no2, Guard, a, b)
c
      integer no1, no2, lIn2, ladj, Guard
      real a, b
c
c  Determine the adjusted image
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real out(maxdim), RBuf(8*maxdim)
      logical flags(maxdim)
      integer IBuf(2*maxdim)
      integer indices(4), nints
      integer i, j, xmin, xmax, ymin, ymax
c
c  Intialise various rubbish.
c
      xmin = 1 + guard
      xmax = no1 - guard
      ymin = 1 + guard
      ymax = no2 - guard
c
c  Initialise the interpolation.
c
      call IntpInit(xmin, xmax, indices, IBuf, nints, RBuf)
c
c  Zero the initial rows.
c
      do i = 1, no1
        out(i) = 0.0
        flags(i) = .false.
      enddo
      do j = 1, ymin-1
        call xywrite(lAdj, j, out)
        call xyflgwr(lAdj, j, flags)
      enddo
c
c  Calculate row "j" of the expanded image.  Also calculate max and min.
c
      do j = ymin, ymax
        call Intp(lin2, j, xmin, xmax, no1, no2,
     *             indices, IBuf, nints, RBuf, out)
c
        do i = xmin, xmax
          out(i) = a*out(i) + b
          flags(i) = .true.
        enddo
c
        call xywrite(lAdj, j, out)
        call xyflgwr(lAdj, j, flags)
      enddo
c
c  Blank out the last few rows.
c
      do i = 1, no1
        out(i) = 0.0
        flags(i) = .false.
      enddo
      do j = ymax+1, no2
        call xywrite(lAdj, j, out)
        call xyflgwr(lAdj, j, flags)
      enddo
c
      end
c***********************************************************************
      subroutine GetRes(lIn1, lIn2, lres, no1, no2, Guard, a, b)
c
      integer no1, no2, lIn1, lIn2, lres, Guard
      real a, b
c
c  Determine the residuals upon interpolation.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real In1(maxdim), RBuf(8*maxdim), out(maxdim)
      logical flags(maxdim)
      integer IBuf(2*maxdim)
      integer indices(4), nints
      integer i, j, xmin, xmax, ymin, ymax
      ptrdiff off
c
c  Intialise various rubbish.
c
      xmin = 1 + guard
      xmax = no1 - guard
      ymin = 1 + guard
      ymax = no2 - guard
c
c  Initialise the interpolation.
c
      call IntpInit(xmin, xmax, indices, IBuf, nints, RBuf)
c
c  Zero the initial rows.
c
      do i = 1, no1
        out(i) = 0.0
        flags(i) = .false.
      enddo
      do j = 1, ymin-1
        call xywrite(lres, j, out)
        call xyflgwr(lres, j, flags)
      enddo
c
c  Calculate row "j" of the expanded image.  Also calculate max and min.
c
      do j = ymin, ymax
        call Intp(lIn2, j, xmin, xmax, no1, no2, indices,
     *             IBuf, nints, RBuf, out)
        off = (j-1)*no1
        call scrread(lIn1, In1, off, no1)
c
        do i = xmin, xmax
          out(i) = In1(i) - (a*out(i) + b)
          flags(i) = .true.
        enddo
c
        call xywrite(lres, j, out)
        call xyflgwr(lres, j, flags)
      enddo
c
c  Blank out the last few rows.
c
      do i = 1, no1
        out(i) = 0.0
        flags(i) = .false.
      enddo
      do j = ymax+1, no2
        call xywrite(lres, j, out)
        call xyflgwr(lres, j, flags)
      enddo
c
      end
c***********************************************************************
      subroutine IntpInit(xmin, xmax, indices, ints, nints, Wx)
c
      integer xmin, xmax
      real Wx(4*xmax)
      integer ints(2,xmax), nints, indices(4)
c
c  Initialise ready for the interpolation inner loop. This routine
c  consumes little of the overall time, so don't worry to much about
c  vectorisation.
c
c-----------------------------------------------------------------------
      real x,fx
      integer i, jx, jprev
c
c  Externals.
c
      real xval
c
c  Initialise indices.
c
      do i = 1, 4
        indices(i) = 0
      enddo
c
      ints(1,1) = int(xval(xmin))
      jprev = ints(1,1) - 1
      nints = 1
c
      do i = xmin, xmax
        x = xval(i)
        jx = int(x)
        if (jx.ne.jprev+1) then
          ints(2,nints) = jprev
          nints = nints + 1
          ints(1,nints) = jx
        endif
        fx = x - jx
        wx(i    ) = ((-0.5*fx+1.0)*fx-0.5)*fx
        wx(i+  xmax) = ((1.5*fx-2.5)*fx   )*fx + 1.0
        wx(i+2*xmax) = ((-1.5*fx+2.0)*fx+0.5)*fx
        wx(i+3*xmax) = ((0.5*fx-0.5)*fx   )*fx
        jprev = jx
      enddo
      ints(2,nints) = jprev
      end
c***********************************************************************
      subroutine Intp(lu, j, xmin, xmax, n1, n2, indices, ints,
     *                 nints, Dat, Out)
c
      integer j, lu, xmin, xmax, n1, n2, indices(4), NInts,
     *  Ints(2,NInts)
      real Dat(*), Out(n1)
c
c  Ready to do interpolation.
c
c-----------------------------------------------------------------------
      integer i, jy, jy1, jy2, jy3, jy4
      real y, fy, wy1, wy2, wy3, wy4
c
c  Externals.
c
      real yval
      integer indx
c
      y = yval(j)
      jy = int(y)
      fy = y - jy
      wy1 = ((-0.5*fy+1.0)*fy-0.5)*fy
      wy2 = ((1.5*fy-2.5)*fy   )*fy + 1.0
      wy3 = ((-1.5*fy+2.0)*fy+0.5)*fy
      wy4 = ((0.5*fy-0.5)*fy   )*fy
c
      i = 4 * xmax
      jy1 = i + indx(lu,jy-1,Dat(i+1),indices,n1,n2,4)
      jy2 = i + indx(lu,jy  ,Dat(i+1),indices,n1,n2,4)
      jy3 = i + indx(lu,jy+1,Dat(i+1),indices,n1,n2,4)
      jy4 = i + indx(lu,jy+2,Dat(i+1),indices,n1,n2,4)
c
      call intp2(xmin,xmax,n1,Out,ints,nints,
     *        Dat(jy1),Dat(jy2),Dat(jy3),Dat(jy4),
     *        Dat(1),Dat(1+xmax),Dat(1+2*xmax),Dat(1+3*xmax),
     *        wy1,wy2,wy3,wy4)
c
      end
c***********************************************************************
      subroutine intp2(xmin,xmax,n1,Out,ints,nints,
     *        z1,z2,z3,z4,wx1,wx2,wx3,wx4,wy1,wy2,wy3,wy4)
c
      integer xmin,xmax,n1,nints,ints(2,nints)
      real Out(n1)
      real z1(n1), z2(n1), z3(n1), z4(n1)
      real wx1(xmax),wx2(xmax),wx3(xmax),wx4(xmax)
      real wy1,    wy2,    wy3,    wy4
c
c  Do the interpolation. I know this looks pretty terrible, but it
c  vectorises, and is more efficient on a non-vectorising machine as
c  well.  For an expansion of f, the inner loop can be expected to
c  be roughly 1/f long. Thus for 0.5%, the inner loop should be
c  about 200.
c
c-----------------------------------------------------------------------
      integer xlo,xhi,i,j,jx
      i = 0
      xhi = xmin - 1
      do while (xhi.lt.xmax)
        i = i + 1
        xlo = xhi + 1
        xhi = xlo + ints(2,i) - ints(1,i)
        j = ints(1,i)
        do jx = xlo, xhi
          Out(jx) =   (z1(j-1)*wy1 + z2(j-1)*wy2
     *                + z3(j-1)*wy3 + z4(j-1)*wy4) * wx1(jx)
     *              + (z1(j )*wy1 + z2(j )*wy2
     *                + z3(j )*wy3 + z4(j )*wy4) * wx2(jx)
     *              + (z1(j+1)*wy1 + z2(j+1)*wy2
     *                + z3(j+1)*wy3 + z4(j+1)*wy4) * wx3(jx)
     *              + (z1(j+2)*wy1 + z2(j+2)*wy2
     *                + z3(j+2)*wy3 + z4(j+2)*wy4) * wx4(jx)
          j = j + 1
        enddo
      enddo
      end
c***********************************************************************
      real function xval(i)
c
      integer i
c
c-----------------------------------------------------------------------
      include 'imdiff.h'
      xval = (i-xo-xshift)/expand(1) + xo
      end
c***********************************************************************
      real function yval(j)
c
      integer j
c
c-----------------------------------------------------------------------
      include 'imdiff.h'
      if (xyequal) then
        yval = (j-yo-yshift)/expand(1) + yo
      else
        yval = (j-yo-yshift)/expand(2) + yo
      endif
c
      end
c***********************************************************************
      integer function indx(lu,j,data,indices,n1,n2,dims)
c
      integer lu,j,dims,indices(dims),n1,n2
      real data(n1,dims)
c
c  Return the index of a row of data. If it is not present, we discard
c  the earliest row, and read the row in from disk.
c
c-----------------------------------------------------------------------
      integer k,l
      ptrdiff off
      k = 1
      l = 1
      do while (l.le.dims .and. indices(l).ne.j)
        if (indices(l).lt.indices(k)) k = l
        l = l + 1
      enddo
      if (l.gt.dims) then
        l = k
        indices(l) = j
        if (j.lt.1 .or. j.gt.n2) then
          call bug('w','Guard band exceeded')
          call bug('f','Problem may be ill-posed or'//
     *      ' guard band needs to be increased')
        endif
        off = (j-1)*n1
        call scrread(lu,Data(1,l),off,n1)
      endif
      indx = (l-1)*n1 + 1
      end
c***********************************************************************
      subroutine getopt(vary)
c
      character vary*(*)
c
c  Get the user options.
c
c-----------------------------------------------------------------------
      integer lfix,lvary
      character dofix*64,dovary*64
c
c  Externals.
c
      integer len1
c
      integer nopt
      parameter (nopt=6)
      character opts(nopt)*11
      logical present(nopt)
      data opts/'noamplitude', 'nooffset', 'noxshift', 'noyshift',
     *        'noexpand   ', 'expand     '/
c
      call options('options',opts,present,nopt)
c
      dofix = ' '
      dovary = ' '
      if (.not.present(1)) then
      vary(1:1) = 'A'
      lvary = len1(dovary)
      dovary(lvary+1:) = ',amplitude'
      else
      lfix = len1(dofix)
      dofix(lfix+1:) = ',amplitude'
      endif
c
      if (.not.present(2)) then
      vary(2:2) = 'O'
      lvary = len1(dovary)
      dovary(lvary+1:) = ',offset'
      else
      lfix = len1(dofix)
      dofix(lfix+1:) = ',offset'
      endif
c
      if (.not.present(3)) then
      vary(3:3) = 'X'
      lvary = len1(dovary)
      dovary(lvary+1:) = ',x-shift'
      else
      lfix = len1(dofix)
      dofix(lfix+1:) = ',x-shift'
      endif
c
      if (.not.present(4)) then
      vary(4:4) = 'Y'
      lvary = len1(dovary)
      dovary(lvary+1:) = ',y-shift'
      else
      lfix = len1(dofix)
      dofix(lfix+1:) = ',y-shift'
      endif
c
      if (.not.present(5)) then
      lvary = len1(dovary)
      if (present(6)) then
        vary(5:6) = 'IJ'
        dovary(lvary+1:) = ',individual-expansion'
      else
        vary(5:5) = 'E'
        dovary(lvary+1:) = ',expansion'
      endif
      else
      lfix = len1(dofix)
      dofix(lfix+1:) = ',expansion'
      if (present(6)) call bug('f',
     *  'You cannot mix options=noexpand,expind')
      endif
c
      if (vary.eq.' ') call bug('f', 'Nothing to vary')
c
      if (dovary.ne.' ') call output('Varied:  '//dovary(2:))
      if (dofix.ne.' ') call output('Fixed:   '//dofix(2:))
      end
