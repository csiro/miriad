c *********************************************************************
c History:
c     nebk   dec90 Original version.
c     mjs  20mar91 itoa calls now call itoaf
c     nebk 28mar92 minor changes to accomodate use of memalloc
c                  routines in calling code (zeesim etc)
c
c *********************************************************************
c* binfid - adjust window size and fiddle bin sizes for spacial avg'ing
c& nebk
c: profile analysis
c+
      subroutine binfid (iwin, aveop, size, axis, bin, blc, trc, nbin)
      implicit none
      integer size, axis, bin, blc, trc, nbin, iwin
      character aveop*1

c     Adjust the size of the window so that the bin width fits
c     an integer number of times and fiddle the bin sizes for
c     spatial averaging
c
c     Input:
c        iwin     i    Window number
c        aveop    c   'a' or 's' for averaging or summing
c        size     i    Size of axis in (unbinned) image
c        axis     i    Axis number 
c     Input/output:
c        bin      i    Bin width.  On output, for averaging, equal to
c                      the window size for spatial axes
c        blc,trc  i    Window corners.
c     Output:
c        nbin     i    Number of binned pixels in window
c
c--
cc
      integer lo, hi, rem
      logical new
      character str*1, aline*80, itoaf*1
c-----------------------------------------------------------------------
      nbin = trc - blc + 1
      if (axis.eq.1) then
         if (bin.gt.nbin)
     *     call bug ('f','Channel bin size larger than spectral window')
c
c Allow non-integral numbers of bins on spectral axis and adjust window
c size to binned size
c
         if(mod(nbin,bin).ne.0) then
           nbin = nbin/bin + 1
         else
           nbin = nbin/bin
         end if
      else
         if (aveop.eq.'a') then
c
c Make the bin size equal to the window size for averaging
c
            bin = nbin
            nbin = 1
         else
            str = itoaf(axis)
            if (bin.lt.1) 
     *         call bug ('f', 'Illegal bin width for axis '//str)
c
            lo = blc
            hi = trc
            new = .false.
            rem = mod(nbin,bin)
c
c Adjust window to fit integral number of bins.  Decrement BLC by 1
c and increment TRC by 1 until ok.
c
            do while (rem.ne.0) 
               if (blc.eq.1 .and. trc.eq.size) then
                  write (aline,100) iwin, str
100               format ('Could not adjust window # ', i2, 
     *                    ' to fit bin size on axis ', a)
                  call bug ('f', aline)
               end if
c
               blc = max(blc-1,1)
               nbin = trc - blc + 1
               rem = mod(nbin,bin)
c
               if (rem.ne.0) trc = min(trc+1,size)
               nbin = trc - blc + 1
               rem = mod(nbin,bin)
c
               new = .true.
            end do
c
c Tell user what happened
c
            if (new) then
               write (aline, 200) axis, iwin, lo, hi, blc, trc, bin
200            format ('Adjusted axis ', i1, ' window (#',i2, ') from ',
     +                  i4, ',', i4, ' to ', i4, ',', i4, 
     +                 ' to fit bin width',i3)
               call output (aline)
            end if
c
c Adjust window size to binned size
c
            nbin = nbin / bin
         end if
      end if
c
      end
c
c
c *********************************************************************
c& nebk
c: profile analysis
c+
      subroutine binrd2 (h1, h2, bin, blc, trc, nbin, row, data1, data2)
      implicit none
      integer h1, h2, bin(3), blc(3), trc(3), nbin(3)
      real row(*), data1(*), data2(*)

c     Read one region from two 3-D images, applying binning criterion 
c     as specified.   Output arrays are one dimensional, with 
c     successively binned rows joined end to end.  Designed for
c     images that are in vxy order, so that binned spectra follow
c     each other in output
c
c     Input:
c       h1,h2    i    Handles for input images
c       bin      i    Binning size for three axes (v,x,y).  For the x 
c                     and y axes, these binning sizes must fit an
c                     integral number of times into the window. For
c                     the spectral dimension (first) this is not vital
c       blc,trc  i    Window describing region to read in (v,x,y)
c       nbin     i    Number of binned pixels in window (v,x,y)
c     Input/output:
c       row      r    Scratch array to hold one full row of data 
c       data1,2  r    Output binned data. All locations likely to
c                     be used must be initialized to 0.0
c                     Binned spectra stacked end to end.
c
c--
cc
      real norm
      integer j, k, koff, kbin, joff, jbin, isp, idum, p1, p2
      logical wrt
c-----------------------------------------------------------------------
c
c Spatial normalization factor when accumulating (binned) output
c
      norm = bin(2) * bin(3)
c
c Read in the entire region requested by the user and add binned
c spectra to the correct output array locations to accomplish 
c spatial binning.
c
      isp = 1
      do k = blc(3), trc(3)
        koff = k - blc(3) + 1
        kbin = koff / bin(3)
        if (mod(koff,bin(3)).ne.0) kbin = kbin + 1
c
c Set planes
c
        call xysetpl (h1, 1, k)
        call xysetpl (h2, 1, k)
c
        do j = blc(2), trc(2)
          joff = j - blc(2) + 1
          jbin = joff / bin(2)
          if (mod(joff,bin(2)).ne.0) jbin = jbin + 1
c
c Location of first desired channel from next spectrum in output
c Spatially binned spectra are joined end to end
c
          p1 = (((kbin-1)*nbin(2) + jbin - 1)*nbin(1)) + 1
          p2 = p1
          idum = p1
c
c Bin up first image row spectrally as desired 
c
c           if (j.eq.blc(2).and.k.eq.blc(3)) wrt = .true.
          wrt=.false.
          call xyread (h1, j, row)
          call binup (row, blc(1), trc(1), bin(1), norm, p1,
     *                data1, wrt)
c
c Bin up second image row spectrally as desired 
c
          call xyread (h2, j, row)
          call binup (row, blc(1), trc(1), bin(1), norm, p2,
     *                data2, .false.)
c
c        if(j.eq.trc(2) .and. k.eq.trc(3)) then
c            write(*,'(a,3(I4,1x),I7,2x,I7)') 
c     *            'k,j,Isp,Start,end location=',k,j,isp,idum,p1
c        end if
c
        enddo
        isp = isp+1
      enddo
c
      end
c
c
c *********************************************************************
c* binup - bin up data in array
c& nebk
c: profile analysis
c+
      subroutine binup (data, blc, trc, bin, norm, ipt, binned, wrt)
      implicit none
      integer blc, trc, ipt, bin
      real data(*), binned(*), norm
      logical wrt

c     Bin up data in array
c
c     Input:
c        data     r   Full array of data (starting at location 1)
c        blc,trc  i   Array window to binup
c        bin      i   Number of pixels to bin together
c        norm     r   Additional multiplicative normalization factor
c                     Binned data are normalized by BIN*NORM
c     Input/output:
c        ipt      i   On input, first location in output array to use
c                     On output, last location in output array used
c        binned   r   Output binned spectrum.  Normalization
c                     of each output point is by the product of the
c                     number of binned points and NORM
c                     Deals with non-integral number of bins in array
c 
c                     BINNED must be initialized to 0.0 on first entry
c
c--
cc
      integer count, chnorm, i
c-----------------------------------------------------------------------
      if (bin.eq.1) then
c
c Take fast route for trivial case
c Scalar directive for C-220.  The O2/3 optimizer fails for this
c trivial piece of code and subtly corrupts data
c$dir scalar
         do i = blc, trc
           binned(ipt) = binned(ipt) + data(i)/norm
c           if (binned(ipt).ne.data(i)/norm) then
c             write(*,*)'i,in,out=',i,data(i)/norm,binned(ipt)
c           end if
           ipt = ipt + 1
         end do
         ipt = ipt - 1
      else
c
c Normalize by available number of points. 
c
         chnorm = min(trc-blc+1,bin)
c
         count = 1
         do i = blc, trc
c
c Fill output array
c
            if (wrt) write(*,*) 'I, bin width=', i,chnorm
            binned(ipt) = binned(ipt) + data(i)/(norm*chnorm)
c
c Increment output pointer and reset normalization factor when necessary
c
            if (count.eq.bin) then
               ipt = ipt + 1
               count = 1
               if (trc-i.lt.bin) chnorm = trc - i 
            else
               count = count + 1
            end if
         end do
         if (count.eq.1) ipt = ipt - 1
      end if
c
      end
