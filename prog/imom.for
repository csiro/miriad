c**********************************************************************c
      program Imom
      implicit none

c
c= IMOM - Compute intensity-weighted moment of an image
c& vjm
c: image analysis
c+
c       IMOM finds the 'centroid' of a Miriad image, and the 'spread'
c       about that centroid, by evaluating the expressions
c           mom1 = SUM{ I(x)*x } / SUM{ I(x) }
c           mom2 = sqrt( SUM{ I(x)*(x-mom1)**2 } / SUM{ I(x) } )
c       along the first two axes of an image.
c
c       Notes:
c       * The first moment should always be in the range 1..naxis(n).
c       * mom2 is equivalent to the standard deviation, not the FWHM,
c         for a gaussian intensity distribution.
c       * mom2 shows the spread along the directions parallel to the
c         *image axes*. So if your object of interest is elongated along 
c         some random angle, the mom2 values returned will be a combination
c         of the major and minor-axis sizes, projected onto the x- and y-axes.
c       * If all pixels in the region are blank, moments of -1.0 are returned
c         and a warning is printed.
c       * In case of a rounding error etc, moments of 0.0 are returned
c         and a warning is printed.
c
c@ in
c	Input image name. No default.
c@ options
c	The following options can be given (minimum match applies):
c         skew 
c           Compute the 'third moment'. Since the formal third moment
c           has the rather useless dimensions of pixel**3, what
c           is actually returned is the dimensionless quantity
c                SUM{ I(x)*(x - mom1)**3 } / SUM{ I(x) } / mom2**3.
c           A negative value means the distribution is skewed towards
c           values less than the first moment; i.e. the pixels at
c           lower pixel number than mom1 are generally brighter than
c           those at higher pixel number. A positive value
c           means the pixels at higher pixel number than the first
c           moment are brighter.
c         clipmean
c           When you use this program to estimate the peak of a source,
c           better accuracy can be had by ignoring low-intensity values.
c           Setting this option forms the moments using only pixels that
c           are brighter than the (local) mean.
c           If a min or max value is specified (see below), the mean is
c           computed using only the values within the selected range.
c         clip1sigma
c           Forms moments using just the pixels brighter than
c           local_mean + local_rms.
c           If a min or max value is specified, the mean and rms are computed
c           using only the values within the selected range.
c@ region
c	The region of interest. See the help on "region" for more
c	information. This gives the region of pixels to calculate the
c       moments within. Only rectangular regions are supported.
c       The default is the entire image.
c       If a cube is given as the input image, the program computes
c       moments for each plane along axis 3 (within the selected range).
c@ min
c       The minimum pixel value for inclusion in the moment calculation.
c       The default is the image minimum.
c@ max
c       The maximum pixel value for inclusion in the moment calculation
c       The default is the image maximum.
c@ log
c	The output log file. The default is the terminal. Output to
c       the terminal is paged.
c--
c  vjm  20aug99  Cannibalise imlist
c  vjm  23aug99  Revise & expand
c  vjm  01oct99  Fix up cube handling and clip limit handling
c  rjs  01oct99  Tivial FORTRAN standardization.
c----------------------------------------------------------------------c
	character version*(*)
	parameter(version='Imom: version 1.0 01-Oct-99')
	include 'maxdim.h'
	integer   maxboxes,maxnax
	parameter(maxboxes=2048,maxnax=3)
	integer   naxis,boxes(maxboxes),nsize(maxnax)
	integer   blc(maxnax),trc(maxnax)
	integer   lin
	character in*64,out*64,line*80
	logical   more,doskew,getmin,getmax,clipmean,clip1sig
        real      rlo,rhi,imlo,imhi
c
c  Externals
c
        logical  keyprsnt
c
c  Get the input parameters.
c
	call output(version)
	call keyini
	call keya ('in', In, ' ')
	if (in.eq.' ') call bug ('f', 'Image name not specified')

	call GetOpt(doskew,clipmean,clip1sig)

	call BoxInput('region',In,boxes,maxboxes)

        getmin  = .not.keyprsnt('min')
        getmax  = .not.keyprsnt('max')
        call keyr('min',rlo,0.0)
        call keyr('max',rhi,0.0)
	call keya('log',out,' ')

	call keyfin

c
c  Open the output text file.
c
	call LogOpen(out,'q')
c
c  Open input image and check dimensions.
c
	call xyopen(lIn,In,'old',maxnax,nsize)
	if (nsize(1).gt.maxbuf)
     *       call bug('f','Image too big for buffer')
	call rdhdi(lIn,'naxis',naxis,0)
	naxis = min(naxis,maxnax)
c
c  Determine portion of image to list.
c
	call BoxSet(boxes,maxnax,nsize,'s')
	call BoxInfo(boxes,maxnax,blc,trc)
c
c  Determine the min and max value, if needed.
c
        call ImMinMax(lIn,naxis,nsize,imlo,imhi)
        if (getmin.or.getmax) then
           if (imlo.eq.imhi) then
              call xyclose(lIn)
              write(line,'(''All pixels are '',1pg10.3)') rlo
              call output(line)
              stop
           endif

c          rlo,rhi are set to user's value if one is given, else 0.0
c          Reset them to image low, high if user gave no value.
           if (getmin) rlo=imlo
           if (getmax) rhi=imhi

           if (rlo.gt.rhi) then
              call xyclose(lIn)
              write(line,
     *            '(''Minimum '',1pg10.3,'' greater than max '',g10.3)')
     *             rlo,rhi
              call bug('f',line)
              stop
           endif
        endif
c
c  Title line.
c
	call LogWrite('Moments for image '//In,more)
c        write(line,'(''Intensity range='',1pg10.3,x,g10.3)') rlo,rhi
        call LogWrit(line)

        if (clip1sig) then
          call LogWrit('Will clip pixels below (RoI mean + 1 sigma)')
        else
          if (clipmean) then
            call LogWrit('Will clip pixels below the RoI mean')
          endif
        endif
c
c  Compute
c
        call ListMom(lIn,naxis,imlo,imhi,blc,trc,rlo,rhi,
     *               getmin,getmax,doskew,clipmean,clip1sig)
c
c  All done.
c
	call xyclose(lIn)
	call LogClose

        stop
	end
c**********************************************************************c
	subroutine GetOpt(doskew,clipmean,clip1sig)
c
	implicit none
	logical doskew
	logical clipmean
	logical clip1sig
c
c  Determine which of the options is to be done. Default is dodata.
c
c  Outputs:
c    doskew	Compute the skew, or not?
c----------------------------------------------------------------------c
	integer NOPTS
	parameter(NOPTS=3)
	character opts(NOPTS)*10
	logical   present(NOPTS)
	data opts/'skew      ','clipmean  ','clip1sigma'/
c
	call options('options',opts,present,NOPTS)
	doskew   = present(1)
	clipmean = present(2)
	clip1sig = present(3)
	end
c******************************************************************c
	Subroutine ListMom(lIn,naxis,imlo,imhi,blc,trc,rlo,rhi,
     *                     getmin,getmax,doskew,clipmean,clip1sig)
	implicit none
        logical getmin,getmax,doskew,clipmean,clip1sig
	integer lIn,naxis,blc(naxis),trc(naxis)
        real    rlo,rhi,imlo,imhi
c
c       List moments and write in standard format into LogFile.
c
c      Inputs:
c        lIn	    The handle of the Image.
c        naxis	    Number of axes of the Image.
c        imlo,imhi  Min, max values in the entire image
c        blc,trc    Corners of region of interest.
c        rlo,rhi    Intensity range to use
c        getmin     Find the minimum in each region
c        getmax     Find the maximum in each region
c                   (getmin or getmax=false means user has set it)
c        doskew     Compute the skew statistic
c        clipmean   Set lower clip limit to region mean
c        clip1sig   Set lower clip limit to region mean+rms
c-------------------------------------------------------------------c
	double precision value
	integer axis,plane,length
	character line*80,label*13,units*13,ctype*9
	real cbof
        real momx(3),momy(3),stats(3)
c
c  Write title lines.
c
	call LogWrit(' ')
	call LogWrit('Image Moments')
	call Title(lIn,naxis,blc,trc,cbof)

c
c  List moments for each plane in each axis in selected region.
c
      if(naxis.ge.3) then
	axis = 3
	do while(axis.le.naxis)
	  call AxisType(lIn,axis,plane,ctype,label,value,units)
	  write(line,'(a,i5,5x,a)')
     *	  'Axis: ',axis,ctype
	  length=6+5+5+10
	  call LogWrit(' ')
	  call LogWrit(line(1:length))
c
c  Do a separate estimate for each hyperplane.
c
	  plane = blc(axis)
	  do while(plane.le.trc(axis))
	    call xysetpl(lIn,1,plane)
	    call AxisType(lIn,axis,plane,ctype,label,value,units)

	    call momit(lin,naxis,imlo,imhi,blc,trc,rlo,rhi,
     *                 getmin,getmax,doskew,clipmean,clip1sig,
     *                 momx,momy,stats)
            write(line,
     *          '(a,x,i4,2x,a10,x,1pe12.5,1p2(x,a,x,e12.5))')
     *          'Plane:',plane,'Npix:',stats(1),
     *                  'good, range',stats(2),'to',stats(3)
            call LogWrit(line(1:80))
            write(line,'(a,x,i4,2x,a10,1p2(x,e12.5),x,a)')
     *            'Plane:',plane,'Centroid:',momx(1),momy(1),'pixels'
            call LogWrit(line(1:80))
            write(line,'(a,x,i4,2x,a10,1p2(x,e12.5),x,a)') 
     *           'Plane:',plane,'Spread:',momx(2),momy(2),'pixels'
	    call LogWrit(line(1:80))
            if (doskew) then
               write(line,'(a,x,i4,2x,a10,1p2(x,e12.5),x,a)') 
     *              'Plane:',plane,'Skew:',momx(3),momy(3),' '
               call LogWrit(line(1:80))
            endif
	    plane = plane + 1
	  enddo
	  axis = axis + 1
	enddo

      else if(naxis.eq.2) then

        call momit(lin,naxis,imlo,imhi,blc,trc,rlo,rhi,
     *             getmin,getmax,doskew,clipmean,clip1sig,
     *             momx,momy,stats)
        write(line,'(x,a10,x,1pe12.5,2x,a,x,1pe12.5,x,a,x,1pe12.5))')
     *            'Npix:',stats(1),
     *            'good, range',stats(2),'to',stats(3)
	call LogWrit(line(1:80))
        write(line,'(x,a10,1p,2(x,e12.5),x,a)') 
     *       'Centroid:',momx(1),momy(1),' pixels'
	call LogWrit(line(1:80))
        write(line,'(x,a10,1p,2(x,e12.5),x,a)')
     *         'Spread:',momx(2),momy(2),' pixels'
	call LogWrit(line(1:80))
        if (doskew) then
           write(line,'(x,a10,1p,2(x,e12.5),x,a)')
     *          'Skew:',momx(3),momy(3),' '
           call LogWrit(line(1:80))
        endif
      endif

      end

c********1*********2*********3*********4*********5*********6*********7*c
      subroutine momit(lin,naxis,imlo,imhi,blc,trc,rlo,rhi,
     *                 getmin,getmax,doskew,clipmean,clip1sig,
     *                 momx,momy,stats)
c     
      implicit none
      integer lIn,naxis,blc(naxis),trc(naxis)
      real    imlo,imhi,rlo,rhi
      real    momx(3),momy(3),stats(3)
      logical doskew,clipmean,clip1sig,getmin,getmax
c
c  Compute Image Moments within a bounding box
c
c  Inputs:
c    lIn	The handle of the Image.
c    naxis	Number of axes of the Image.
c    imhi,imlo  Max,min values in image (from image header)
c    blc,trc	Corners of region of interest.
c    rlo,rhi    Intensity range of interest.
c    getmin     Find the minumum value in the region.
c    getmax     Find the maximum value in the region.
c    doskew     Compute the skew statistic
c    clipmean   Set low end of clip range = mean of subregion
c    clip1sig   Set low end of clip range = mean+rms of subregion
c
c  Output:
c    momx       moments along "x" axis, of unflagged pixels within box.
c    momy       moments along "y" axis, of unflagged pixels within box.
c    stats      (no. pixels, lowest value, highest value) used in the
c               calculation of the moments.
c
c  Note:
c    a moment < 0 means no unflagged pixels
c    a moment = 0 means the sum of intensities (used as weights) = 0,
c                 or there was a rounding error (mom(2)=0).
c
c This doesn't yet handle a user-fixed min or max; it recomputes rlo,rhi.
c-----------------------------------------------------------------
      include 'maxdim.h'
      real    wt(maxbuf)
      logical flags(maxbuf),apply
      integer i,j,num
      real    sum,sumsq,lo,hi,avg,rms,olo,ohi
      real    ri,rj,sumi,sumsqi,sumj,sumsqj,sumrow,sumcbi,sumcbj
      real    epsilon,nrmi,nrmj,val,offset

      epsilon = 1.0e-12
c
c  Sentinel values; these mean "no unflagged pixels"
c
      do i=1,3
         momx(i)  = -1.0
         momy(i)  = -1.0
         stats(i) = -1.0
      end do
c
c  We need the local minimum value to get the moments right -
c  the weights must be +ve, and the minimum weight should be zero,
c  to avoid biasing the result.
c  So get the min,max, mean & rms of all unmasked pixels.
c  The mean and rms are needed for clipping so we get the right
c  answer when finding peaks at low s/n.
c

c     Get statistics for the current region.
      num    = 0
      sum    = 0.0
      sumsq  = 0.0
      avg    = 0.0
      rms    = 0.0
      lo     = imhi
      hi     = imlo

      if (getmin.and.getmax) then
c        no user-specified limits
         do j = trc(2),blc(2),-1
            call xyread (lIn,j,wt)
            call xyflgrd (lIn,j,flags)
            do i = blc(1),trc(1)
               val = wt(i)
               if (flags(i)) then
                  lo    = min(lo,val)
                  hi    = max(hi,val)
                  sum   = sum    + val
                  sumsq = sumsq  + val*val
                  num   = num    + 1
               endif
            end do
         end do
      else
         if ((.not.getmin).and.(.not.getmax)) then
c           user has fixed range; clip pixels outside it
            do j = trc(2),blc(2),-1
               call xyread (lIn,j,wt)
               call xyflgrd (lIn,j,flags)
               do i = blc(1),trc(1)
                  val = wt(i)
                  if (flags(i).and.val.gt.rlo.and.val.lt.rhi) then
                     lo    = min(lo,val)
                     hi    = max(hi,val)
                     sum   = sum    + val
                     sumsq = sumsq  + val*val
                     num   = num    + 1
                  endif
               end do
            end do
         else
            if (.not.getmin) then
c              user has fixed minimum only: clip pixels below rlo
               do j = trc(2),blc(2),-1
                  call xyread (lIn,j,wt)
                  call xyflgrd (lIn,j,flags)
                  do i = blc(1),trc(1)
                     val = wt(i)
                     if (flags(i).and.val.gt.rlo) then
                        lo    = min(lo,val)
                        hi    = max(hi,val)
                        sum   = sum    + val
                        sumsq = sumsq  + val*val
                        num   = num    + 1
                     endif
                  end do
               end do
            else
c              user has fixed maximum; clip pixels above rhi
               do j = trc(2),blc(2),-1
                  call xyread (lIn,j,wt)
                  call xyflgrd (lIn,j,flags)
                  do i = blc(1),trc(1)
                     val = wt(i)
                     if (flags(i).and.val.lt.rhi) then
                        lo    = min(lo,val)
                        hi    = max(hi,val)
                        sum   = sum    + val
                        sumsq = sumsq  + val*val
                        num   = num    + 1
                     endif
                  end do
               end do
            endif
         endif
      endif

c     Don't waste time if no data
      if (num.eq.0) then
         call bug('w','no valid pixels selected')
         return
      endif
      
      avg    = sum/num
      rms    = sumsq/num - avg*avg
      if (rms.gt.0.0) then
         rms = sqrt(rms)
      else
         rms = 0.0
         call bug('w','rms was root of a negative number')
      endif


c
c     set up intensity range of interest
c
      offset = -1.0*lo
      olo = lo + offset
      ohi = hi + offset
      if (clipmean) olo = avg + offset
      if (clip1sig) olo = avg + offset + rms

      apply = (.not.getmin).or.(.not.getmax)
      apply = apply.or.(clipmean.or.clip1sig)

      num    = 0
      sum    = 0.0
      sumi   = 0.0
      sumsqi = 0.0
      sumcbi = 0.0
      sumj   = 0.0
      sumsqj = 0.0
      sumcbj = 0.0

c
c  Keep position values near 1, to minimise overflow errors
c
      nrmi = 2.0/(blc(1)+trc(1))
      nrmj = 2.0/(blc(2)+trc(2))
c     
c  Accumulate moment statistics for unflagged data.
c     
      if ( doskew ) then

         if ( apply ) then
            do j = trc(2),blc(2),-1
               rj   = real(j)*nrmj
               call xyread (lIn,j,wt)
               call xyflgrd (lIn,j,flags)
               sumrow = 0.0
               do i = blc(1),trc(1)
                  if (flags(i)) then
                     val = wt(i) + offset
                     if(val.gt.olo.and.val.le.ohi) then
                        ri      = real(i)*nrmi
                        num     = num    + 1
                        sum     = sum    + val
                        sumrow  = sumrow + val
                        sumi    = sumi   + val*ri
                        sumsqi  = sumsqi + val*ri*ri
                        sumcbi  = sumcbi + val*ri*ri*ri
                     endif
                  endif
               end do
c              since rj constant for each row, we can do this for y-moment:
               sumj   = sumj   + sumrow*rj
               sumsqj = sumsqj + sumrow*rj*rj
               sumcbj = sumcbj + sumrow*rj*rj*rj
            end do
         else
            do j = trc(2),blc(2),-1
               rj   = real(j)*nrmj
               call xyread (lIn,j,wt)
               call xyflgrd (lIn,j,flags)
               sumrow = 0.0
               do i = blc(1),trc(1)
                  if(flags(i)) then
                     ri      = real(i)*nrmi
                     num     = num    + 1
                     val     = wt(i)  + offset
                     sum     = sum    + val
                     sumrow  = sumrow + val
                     sumi    = sumi   + val*ri
                     sumsqi  = sumsqi + val*ri*ri
                     sumcbi  = sumcbi + val*ri*ri*ri
                  endif
               end do
               sumj   = sumj   + sumrow*rj
               sumsqj = sumsqj + sumrow*rj*rj
               sumcbj = sumcbj + sumrow*rj*rj*rj
            end do
         endif

      else

c       only first 2 moments 
         if ( apply ) then
            do j = trc(2),blc(2),-1
               rj   = real(j)*nrmj
               call xyread (lIn,j,wt)
               call xyflgrd (lIn,j,flags)
               sumrow = 0.0
               do i = blc(1),trc(1)
                  if (flags(i)) then
                     val     = wt(i) + offset
                     if(val.gt.olo.and.val.le.ohi) then
                        ri      = real(i)*nrmi
                        num     = num    + 1
                        sum     = sum    + val
                        sumrow  = sumrow + val
                        sumi    = sumi   + val*ri
                        sumsqi  = sumsqi + val*ri*ri
                     endif
                  endif
               end do
               sumj   = sumj   + sumrow*rj
               sumsqj = sumsqj + sumrow*rj*rj
            end do
         else
            do j = trc(2),blc(2),-1
               rj   = real(j)*nrmj
               call xyread (lIn,j,wt)
               call xyflgrd (lIn,j,flags)
               sumrow = 0.0
               do i = blc(1),trc(1)
                  if(flags(i)) then
                     ri      = real(i)*nrmi
                     num     = num    + 1
                     val     = wt(i)  + offset
                     sum     = sum    + val
                     sumrow  = sumrow + val
                     sumi    = sumi   + val*ri
                     sumsqi  = sumsqi + val*ri*ri
                  endif
               end do
               sumj   = sumj   + sumrow*rj
               sumsqj = sumsqj + sumrow*rj*rj
            end do
         endif

      endif

c     
c  Calculate moments:
c
c     mom1 = sum[ x(i)*wt(i) ] / sum[wt(i)]
c     mom2 = sqrt(  sum[ wt(i) * (x(i)-mom1)**2 ] / sum[wt(i)] )
c
c          = sqrt(   sum[          wt(i)*x(i)*x(i)] / sum[wt(i)]
c                  - sum[   2*mom1*wt(i)*x(i)]      / sum[wt(i)]
c                  + sum[mom1*mom1*wt(i)]           / sum[wt(i)] )
c
c     mom3 = cbrt ( sum [ wt(i) * (x(i)-mom1)**3 ] / sum[ wt(i) ] )
c     skew =        sum [ wt(i) * (x(i)-mom1)**3 ] / mom2**3
c
c  Numerical Details:
c    You don't get the right answer for the first or third moments if the
c    data cross zero.
c
c    For the first moment, this can be done after the summation at the cost
c    of summimg the pixel numbers (ri, rj).
c    However for higher moments, sums of rj**3 etc have to be accumulated,
c    so it's cheaper to offset every pixel; only 1 extra addition is needed,
c    once the offset is known.
c

      if (num.eq.0) then
         call bug('w','No valid data in chosen range')
         return
      endif

      stats(1) = real(num)
      stats(2) = olo - offset
      stats(3) = ohi - offset

      if (abs(sum).gt.epsilon) then
         momx(1) = sumi / sum
         momx(2) = (sumsqi - 2.0*momx(1)*sumi)/sum + momx(1)*momx(1)
         if (momx(2) .lt. epsilon) then
            momx(2) = 0.0
            momx(3) = 0.0
            call bug('w','mom2 was root of a negative number')
         else
            momx(2) = sqrt(momx(2))
            if (doskew) then
               momx(3) = sumcbi - 3*sumsqi*momx(1)
     *                          + 3*sumi*(momx(1)**2)
               momx(3) = momx(3)/sum - (momx(1)**3)
               
               momx(3) = momx(3)/(momx(2)**3)
            endif
         endif
         momx(1) = momx(1) / nrmi
         momx(2) = momx(2) / nrmi

         momy(1) = sumj/sum
         momy(2) = (sumsqj - 2*momy(1)*sumj)/sum + momy(1)*momy(1)
         if (momy(2) .lt. epsilon) then
            momy(2) = 0.0
            momy(3) = 0.0
            call bug('w','mom2 was root of a negative number')
         else
            momy(2) = sqrt(momy(2))
            if (doskew) then
               momy(3) = sumcbj - 3*sumsqj*momy(1)
     *                          + 3*sumj*(momy(1)**2)
               momy(3) = momy(3)/sum - (momy(1)**3)

               momy(3) = momy(3)/(momy(2)**3)
            endif
         endif
         momy(1) = momy(1) / nrmj
         momy(2) = momy(2) / nrmj
      endif
         
      end

