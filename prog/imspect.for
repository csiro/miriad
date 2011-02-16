      program ImSpect

c       IMSPECT makes an average spectrum of a specified region of a
c       Miriad image.  The average spectrum can be plotted and/or
c       written out as an ascii file for further analysis.
c
c= IMSPECT - Make spectrum from a Miriad image.
c& mchw
c: image analysis and display.
c+
c       IMSPECT makes an spectrum of the velocity or frequency axis of a
c       Miriad image.  The spectrum is integrated, or averaged over the
c       region specified for the other image axes.  The output spectrum
c       can be plotted and/or written out as an ascii file for further
c       analysis.
c
c@ in
c       The input image.  vxy and xyv images are acceptable inputs.
c       No default.
c
c@ region
c       Rectangular region of image to be averaged.  E.g.
c         % imspect  region=relpix,box(-4,-4,5,5)(10,50) for an xyv
c       image, makes a spectrum of image planes 10 to 50 integrated
c       over the center 10 x 10 pixels in the x-y plane.  For a vxy
c       image where the x and y size is 128x128, the corresponding
c       region=box(10,61,50,70)(61,70).  Box refers image axes 1 and 2
c       for either vxy or xyv images.  The default is the entire image.
c
c@ xaxis
c       The x-axis can be plotted as 'channel' or the units in the
c       image.  The default which is whatever units are in the header.
c
c@ yaxis
c       If 'average' then the pixels enclosed in the x-y area specified
c       are averaged.  If 'sum' they are summed and normalized if the
c       units are known.  Default is 'average'
c
c@ yrange
c       Y-axis range for plot.  The default is to self-scale.
c
c@ hann
c       Hanning smoothing length (an odd integer < 15).  The default is
c       no smoothing (hann = 1).
c
c@ options
c       List of minimum match task enrichment options.
c       1deriv  Take 1-sided derivative of spectrum before plotting and
c               after Hanning smoothing.  Useful for Zeeman enthusiasts.
c       2deriv  Take 2-sided derivative of spectrum before plotting.
c       curve   Plot the spectrum joining up the dots instead of the
c               default step-plot.
c
c@ device
c       Standard PGPLOT device.  See the help on "device" for more
c       information.
c
c@ log
c       Write spectrum to this ascii file.  Default is no output file.
c
c@ comment
c       A one-line comment which is written into the logfile.
c
c$Id: imspect.for,v 1.2 2011/02/16 04:44:42 cal103 Exp $
c--
c
c  History:
c    Refer to the RCS log, v1.1 includes prior revision information.
c-----------------------------------------------------------------------
      include 'maxdim.h'

      integer   MAXCO, MAXNAX, MAXBOXES
      parameter (MAXCO=15, MAXNAX=3, MAXBOXES=2048)

      logical   curve, deriv1, deriv2, none

      integer   blc(MAXNAX), boxes(MAXBOXES), i, iostat, lIn, lOut,
     *          lblc, ltrc, naxis, nchan, npix(MAXDIM), nsize(MAXNAX),
     *          nsmth, trc(MAXNAX), vaxis
      real      chan(MAXDIM), coeffs(MAXCO), hwork(MAXCO), spec(MAXDIM),
     *          value(MAXDIM), work(MAXDIM), xdmax, xdmin, ydmax, ydmin,
     *          yrange(2)
      double precision restfreq
      character comment*80, date*9, device*64, in*64, line*72, logf*64,
     *          object*9, str*3, title*130, txtblc*20, txttrc*20,
     *          vctype*9, version*72, word*80, xaxis*64, xlabel*64,
     *          yaxis*64, ylabel*64

      external  itoaf, keyprsnt, len1, pgbeg, versan
      logical   keyprsnt
      integer   len1, pgbeg
      character itoaf*3, versan*72
c-----------------------------------------------------------------------
      version = versan('imspect',
     *                 '$Revision: 1.2 $',
     *                 '$Date: 2011/02/16 04:44:42 $')

c     Get inputs.
      call keyini
      call keyf('in',in,' ')
      call BoxInput('region',in,boxes,MAXBOXES)
      call keya('xaxis',xaxis,' ')
      call keya('yaxis',yaxis,'average')
      call keyr('yrange',yrange(1),0.0)
      call keyr('yrange',yrange(2),yrange(1))
      if (yrange(1).eq.yrange(2) .and. yrange(1).ne.0.0)
     *  call bug('f', 'Invalid y-axis plot range')
      call keyi('hann',nsmth,1)
      if (nsmth.gt.MAXCO) then
        str = itoaf(MAXCO)
        call bug('f', 'Hanning smoothing length must be <= '//str)
      endif
      call getopt(deriv1, deriv2, curve)
      call keya('device',device,' ')
      call keya('log',logf,' ')
      comment = ' '
      do while (keyprsnt('comment'))
          call keya('comment',word,' ')
          comment = comment(1:len1(comment))//' '//word(1:len1(word))
      enddo
      call keyfin

c     Check inputs.
      if (in.eq.' ') call bug('f','No input specified.')
      if (device.eq.' ' .and. logf.eq.' ') call bug('f',
     *  'Neither Plot device nor log file specified for output')
      call xyopen(lIn,in,'old',MAXNAX,nsize)
      call rdhdi(lin,'naxis',naxis,0)
      naxis = min(naxis,MAXNAX)
      if (nsize(1).gt.MAXDIM) call bug('f','Input file too big for me')

c     Find v-axis.
      call GetVaxis(lIn,vaxis,vctype)
      if (vaxis.eq.0) call bug('f','Velocity axis was not found')

c     Set up the region of interest.
      call BoxMask(lIn,boxes,MAXBOXES)
      call BoxSet(boxes,MAXNAX,nsize,'s')
      call BoxInfo(boxes,MAXNAX,blc,trc)
      nchan = trc(vaxis)-blc(vaxis)+1

c     Integrate the spectrum over the specified region.
      if (vaxis.eq.1) then
        call vaxis1(lIn,naxis,blc,trc,nchan,chan,spec,npix,none)
      else if (vaxis.eq.3) then
        call vaxis3(lIn,naxis,blc,trc,nchan,chan,spec,npix,none)
      else
        call bug('f','this image orientation is not implemented')
      endif
      if (none) call bug('f','No good pixels in selected region')

c     Optionally Hanning smooth spectrum.
      if (nsmth.ge.3) then
        call hcoeffs(nsmth, coeffs)
        call hannsm(nsmth, coeffs, nchan, spec, hwork)
      endif

c     Make title.
      call rdhda(lIn,'object',object,' ')
      call rdhda(lIn,'date-obs',date,' ')
      call rdhdd(lIn,'restfreq',restfreq,0d0)
      call mitoaf(blc,3,txtblc,lblc)
      call mitoaf(trc,3,txttrc,ltrc)
      write(title,'(a,f10.6,a,a,a,a,i2)') object,restfreq,' GHz ',date,
     *  ' Blc=('//txtblc(1:lblc)//'),Trc=('//txttrc(1:ltrc)//')',
     *  ' hann=',nsmth
      if (deriv1) title = title(1:len1(title))//', D1'
      if (deriv2) title = title(1:len1(title))//', D2'

c     Get plot axes, convert units, and write labels.
      call axes(lIn,vaxis,xaxis,yaxis,nchan,naxis,npix,
     *          xlabel,ylabel,chan,value,spec)

c     Optionally take derivatives.
      if (deriv1 .or. deriv2) call der(deriv1, nchan, spec, work)

c     Open plot device if requested.
      if (device.ne.' ') then
        iostat = pgbeg(0,device,1,1)
        if (iostat.eq.0) call bug('f', 'Error opening plot device')

c       Work out limits
        if (xaxis.eq.'channel') then
          xdmin = chan(1) - 0.05 * (chan(nchan) - chan(1))
          xdmax = chan(nchan) + 0.05 * (chan(nchan) - chan(1))
        else
          xdmin = value(1) - 0.05 * (value(nchan) - value(1))
          xdmax = value(nchan) + 0.05 * (value(nchan) - value(1))
        endif

        if (yrange(1).ne.0.0 .or. yrange(2).ne.0.0) then
          ydmin = yrange(1)
          ydmax = yrange(2)
        else
          ydmin = spec(1)
          ydmax = spec(1)
          do i = 2, nchan
            ydmax = max(ydmax, spec(i))
            ydmin = min(ydmin, spec(i))
          enddo

          ydmax = ydmax + 0.05 * (ydmax - ydmin)
          ydmin = ydmin - 0.05 * (ydmax - ydmin)
        endif

c       Make plots if requested.
        call pgscf(2)
        call pgsch(1.1)
        call pgsls(1)
        call pgenv(xdmin, xdmax, ydmin, ydmax, 0, 0)
        if (xaxis.eq.'channel') then
          if (curve) then
            call pgline(nchan,chan,spec)
          else
            call pgHline(nchan,chan,spec,2.0)
          endif
        else
          if (curve) then
            call pgline(nchan,value,spec)
          else
            call pgHline(nchan,value,spec,2.0)
          endif
        endif
        call pglab(xlabel,ylabel,title)
        call pgend
      endif

c     Write ascii spectrum if desired.
      if (logf.ne.' ') then
        call txtopen(lOut, logf, 'new', iostat)
        if (iostat.ne.0) call bug('f', 'Error opening output file')
        call txtwrite(lOut,title,len1(title),iostat)
        call txtwrite(lOut,'File: '//In,6+len1(In),iostat)
        call txtwrite(lOut,'Spectral axis type = '//vctype,30,iostat)
        call txtwrite(lOut,comment,len1(comment),iostat)
        do i = 1, nchan
          write(line,'(1pe12.5,3x,1pe12.5,3x,1pe12.5)')
     *      chan(i),value(i),spec(i)
          call txtwrite(lOut, line, 45, iostat)
          if (iostat.ne.0) call bug('f', 'Error writing output file')
        enddo
        call txtclose(lOut)
      endif

c     All done.
      call xyclose(lIn)

      end

c***********************************************************************

      subroutine GetVaxis(lIn,vaxis,vctype)

      integer   lIn, vaxis
      character vctype*(*)
c-----------------------------------------------------------------------
c  Find the v-axis.
c
c  Inputs:
c    lIn        The handle of the image.
c  Output:
c    vaxis      The velocity or frequency axis.
c
c                 03jul90 mchw.
c-----------------------------------------------------------------------
      integer   axis, naxis
      character ctype*9

      external  itoaf
      character itoaf*1
c-----------------------------------------------------------------------
c     Look for v-axis
      vaxis = 0
      call rdhdi(lIn,'naxis',naxis,1)
      do axis = 1, naxis
        call rdhda(lIn,'ctype'//itoaf(axis),ctype,' ')
        if (ctype(1:4).eq.'VELO' .or.
     *      ctype(1:4).eq.'FELO' .or.
     *      ctype(1:4).eq.'FREQ') then
          vaxis = axis
          vctype = ctype
        endif
      enddo

      end

c***********************************************************************

      subroutine vaxis1(lIn,naxis,blc,trc,nchan,chan,spec,npix,none)

      integer lIn,naxis,blc(naxis),trc(naxis),nchan,npix(nchan)
      real chan(nchan),spec(nchan)
      logical none
c-----------------------------------------------------------------------
c  Get integrated spectrum for vaxis=1
c
c  Inputs:
c    lIn        The handle of the image.
c    naxis      Number of image axes.
c    blc,trc    Corners of region of interest.
c    nchan      Number of channels.
c  Output:
c    chan       Array of channel numbers.
c    spec       Integrated spectrum.
c    npix       Number of spatial pixels used for each channel
c    none       True if no good pixels selected
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical flags(MAXDIM)
      integer i, indx, j, k
      real    buf(MAXDIM)
c-----------------------------------------------------------------------
      do i = 1, nchan
        spec(i) = 0.0
        chan(i) = i + blc(1) -1
        npix(i) = 0
      enddo
      none = .true.

      do k = blc(3), trc(3)
        call xysetpl(lIn,1,k)
        do j = blc(2), trc(2)
          call xyread(lIn,j,buf)
          call xyflgrd(lIn,j,flags)
          do i = blc(1), trc(1)
            if (flags(i)) then
              indx = i-blc(1)+1
              spec(indx) = spec(indx) + buf(i)
              npix(indx) = npix(indx) + 1
              none = .false.
            endif
          enddo
        enddo
      enddo

      end

c***********************************************************************

      subroutine vaxis3(lIn,naxis,blc,trc,nchan,chan,spec,npix,none)

      integer lIn,naxis,blc(naxis),trc(naxis),nchan,npix(nchan)
      real chan(nchan),spec(nchan)
      logical none
c-----------------------------------------------------------------------
c  get integrated spectrum for vaxis=3
c
c  Inputs:
c    lIn        The handle of the image.
c    naxis      Number of image axes.
c    blc,trc    Corners of region of interest.
c    nchan      Number of channels.
c  Output:
c    chan       Array of channel numbers.
c    spec       Integrated spectrum.
c    npix       Number of spatial pixels used for each channel
c    none       True if no good pixels selected
c-----------------------------------------------------------------------
      include 'maxdim.h'

      logical flags(MAXDIM)
      integer i, indx, j, k
      real    buf(MAXDIM)
c-----------------------------------------------------------------------
      do i = 1, nchan
        spec(i) = 0.0
        npix(i) = 0
      enddo
      none = .true.

      do k = blc(3), trc(3)
        call xysetpl(lIn,1,k)
        indx = k-blc(3)+1
        chan(indx) = k
        do j = blc(2), trc(2)
          call xyread(lIn,j,buf)
          call xyflgrd(lIn,j,flags)
          do i = blc(1), trc(1)
            if (flags(i)) then
              spec(indx) = spec(indx) + buf(i)
              npix(indx) = npix(indx) + 1
              none = .false.
            endif
          enddo
        enddo
      enddo

      end

c***********************************************************************

      subroutine axes(lIn,vaxis,xaxis,yaxis,nchan,naxis,npix,
     *                                xlabel,ylabel,chan,value,spec)

      integer lIn,vaxis,naxis,nchan,npix(nchan)
      character*(*) xaxis,yaxis,xlabel,ylabel
      real chan(nchan),spec(nchan),value(nchan)
c-----------------------------------------------------------------------
c  Get plot axes and write labels.
c  Inputs:
c    lIn        The handle of the image.
c    vaxis      The velocity or frequency axis.
c    naxis      Number of image axes.
c    xaxis      Units for xaxis.  Can be 'channel' or (default) units in
c               image.
c    yaxis      Units for yaxis.  Can be 'average' or 'sum'.  Default is
c               average.
c    npix       Number of good pixels in integrated spectrum for each
c               channel
c    nchan      Number of channels.
c    chan       Array of channel numbers.
c  Output:
c    value      Array of xaxis values.
c    spec       Spectrum (with converted units).
c    xlabel     Label for xaxis.
c    ylabel     Label for yaxis.
c
c    - Could be much fancier by converting internal units 'JY' 'JY/BEAM'
c    etc. to requested units 'JY' 'JY/BEAM' 'K' etc. calling GetBeam
c    to get beam oversampling factor.
c-----------------------------------------------------------------------
      integer   i
      real      bmaj, bmin, cbof, omega
      double precision dtemp
      character bunit*16, ctype*16

      external  itoaf, len1
      integer   len1
      character itoaf*1
c-----------------------------------------------------------------------
c     Get xlabel.
      call rdhda(lIn,'ctype'//itoaf(vaxis),ctype,' ')
      if (xaxis.eq.'channel') then
        xlabel = 'Channel'
      else if (ctype(1:4).eq.'FREQ') then
        xlabel = 'Frequency (GHz)'
      else if (ctype(1:4).eq.'VELO') then
        xlabel = 'Velocity (km/s)'
      else if (ctype(1:4).eq.'FELO') then
        xlabel = 'Felocity (km/s)'
      else
        xlabel = ctype(1:len1(ctype))
      endif

c     Convert xaxis units.
      call coInit(lIn)
      do i = 1, nchan
        call coCvt1(lIn,vaxis,'ap',dble(chan(i)),'aw',dtemp)
        value(i) = dtemp
      enddo
      call coFin(lIn)

c     Get units and beam oversampling factor from image header.
      call GetBeam(lIn,naxis,bunit,bmaj,bmin,omega,cbof)

c     Normalize the spectra and get the yaxis.
      if (yaxis.eq.'average') then
        do i = 1, nchan
          if (npix(i).gt.0) then
             spec(i) = spec(i)/npix(i)
          else
             call bug('f', 'Error normalizing integrated spectrum')
          endif
        enddo
        ylabel = 'Average Intensity ('//bunit(1:len1(bunit))//')'
      else if (bunit.eq.'JY/PIXEL') then
        ylabel = 'Total Intensity (JY)'
      else if (bunit.eq.'JY/BEAM' .and. bmaj*bmin*omega.ne.0) then
        do i = 1, nchan
          spec(i) = spec(i)/cbof
        enddo
        ylabel = 'Total Intensity (JY)'
      else
        ylabel =
     *    'Total Intensity ('//bunit(1:len1(bunit))//' x pixels)'
      endif

      end

c***********************************************************************

      subroutine getopt (deriv1, deriv2, curve)

      logical deriv1, deriv2, curve
c-----------------------------------------------------------------------
c     Decode options array into named variables.
c
c   Output:
c     deriv1     True means take 1-sided derivative of spectrum
c     deriv2     True means take 2-sided derivative of spectrum
c     curve      True means plot a curve rather than step-plot
c-----------------------------------------------------------------------
      integer MAXOPT
      parameter (MAXOPT = 3)

      logical   present(MAXOPT)
      character opshuns(MAXOPT)*6

      data opshuns /'1deriv', '2deriv', 'curve'/
c-----------------------------------------------------------------------
      call options('options', opshuns, present, MAXOPT)

      deriv1 = present(1)
      deriv2 = present(2)
      if (deriv1 .and. deriv2) deriv2 = .false.
      curve = present(3)

      end

c***********************************************************************

      subroutine der(deriv1, nchan, spec, work)

      logical deriv1
      integer nchan
      real spec(nchan), work(nchan)
c-----------------------------------------------------------------------
c  Take derivative of spectrum.
c
c  Inputs:
c    deriv1       True for one sided, false for two sided derivative
c    nchan        Number of channels
c    spec         SPectrum. On output contains derivative
c    work         Work array.
c-----------------------------------------------------------------------
      integer i
c-----------------------------------------------------------------------
      if (deriv1) then
        do i = 2, nchan
          work(i) = spec(i) - spec(i-1)
        enddo
      else
        do i = 2, nchan-1
          work(i) = 0.5 * (spec(i+1) - spec(i-1))
        enddo

c       Fudge end.
        work(nchan) = work(nchan-1)
      endif

c     Fudge beginning.
      work(1) = work(2)

c     Copy.
      do i = 1, nchan
        spec(i) = work(i)
      enddo

      end
