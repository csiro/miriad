c************************************************************************
c     Some subroutines to get the primary beam function. 
c
c     PBCHECK tells the caller whether PBINIT is going to require the 
c             frequency or not.  
c     PBINIT  sets up primary beams with a user specified Gaussian, a
c             Gaussian specified by the header, or from its knowledge
c             of telescopes and the telecope name in the header.
c     PBGET   returns the primary beam function at some radial distance.
c
c     PBDER   returns the derivative of the primary beam with frequency
c             at some radial distance
c     PBINFO  returns some miscellaneous information about the primary beam
c
c     These routines can access both images and visibility files.
c
c     Following a call to PBINIT, many calls to PBGET and PBDER may 
c     be made.  Each call to PBINIT overwrites the previous information
c     stored in common.  
c
c
c  The following example shows how to use PBCHECK/PBINIT/PBGET to get the 
c  primary beam correction for a continuum image.
c
c
c
c Do we need the frequency to p.b. correct this image ?
c PBFWHMU is a user given Gaussian p.b.  It may be 0.0
c
c     call pbcheck (pbfwhmu, tin, .true., needf)
c
c     if (needf) then
c
c Find spectral axis
c
c       axnam = ' '
c       call fndaxnum (tin, 'freq', axnam, ifax)
c       if (ifax.ne.3) call bug ('f', 'Spectral axis should be # 3')
c
c Get frequency at pixel 1.0 of spectral axis.
c
c       call getfreq (tin, 1.0, ifax, freq, finc, ierr)
c       if (ierr.gt.1) call bug ('f', 'Could not get frequency')
c     else
c       freq = -1.0d0
c     end if
c
c Set up primary beam for image, no derivative.
c
c     call pbinit (pbfwhmu, freq, tin, .true.)
c
c Loop over image
c
c     do j = 1, ny
c       ysq = ((real(j) - crpix(2))*cdelt(2))**2
c       do i = 1, nx
c
c Get radius squared in radians
c
c         xsq = ((real(i) - crpix(1))*cdelt(1))**2
c         radsq = xsq + ysq
c
c Get primary beam
c  
c         pb = pbget(radsq)
c         if (pb.eq.0.0) then
c            
c Primary beam not defined
c
c            blank_the_output
c          else
c
c Primary beam defined
c
c            correct_the_output
c          end if
c       end do
c     end do
c
c
c
c History:
c
c   10nov92   nebk   Original version.
c   24nov92   nebk   More header comments to appease pjt.
c   07jan93   nebk   Rewrite calling interface again
c   15feb93   nebk   pbfwhm=0 -> single dish, <0 -> unknown
c   25oct93   rjs    Prevent floating underflow in exp function.
c***********************************************************************
c
c*pbinit -- Initialize for Primary Beam determination
c:image-data
c& nebk
c+
      subroutine pbinit (fwhm, freq, tin, image)
c
      implicit none
      integer tin
      double precision freq
      real fwhm
      logical image
c
c     Set up coefficients and cutoffs for primary beam correction.
c     If necessary (see below), PBINIT will look at the header item
c     (or visibility variable) "telescop" to work out which telescope
c     is appropriate.  It has knowledge about HatCreek, the ATCA, and 
c     the VLA. Polynomials are provided for the ATCA and the VLA.  
c     For HatCreek only Gaussians are provided.   Polynomial fits are 
c     cutoff below a certain level, but Gaussians have no cutoff.
c
c     The hierarchy is:  
c       Input variable "fwhm" takes precedence over the header item or
c       visibility variable "pbfwhm" which takes precedence over the
c       header item or visibility variable "telescop".
c
c     The ATCA results come from the ATNF memo. by Wieringa & Kesteven.
c     The VLA polynomial coefficients come from the AIPS task PBCOR.
c     The HatCreek Gaussian FWHM comes from the Bima Users Guide 
c     (Jan89), which claims this is the value corresponding to a 
c     6 meter telescope. See caveats there.
c
c     The primary beam routines may, or may not, need a frequency
c     with which to set the primary beam.   They will need
c     the frequency if
c
c          1)  You are going to call PBDER 
c                         or
c          2)  You did not specify "FWHM" as an input to PBINIT and
c              "pbfwhm" is not an item/variable in the data set but
c              "telescop" is a recognized item/variable in the data set
c  
c     If you need to know whether PBINIT will need the frequency before
c     before you call PBINIT, or not, you can call PBCHECK first.
c     
c
c  Input:
c    fwhm   r    If this is positive, then a Gaussian primary beam
c                of FWHM radians will be set.
c    freq   d    If positive, this is the frequency (if it is needed) at 
c                which to evaluate the primary beam function. 
c    tin    i    Handle of open data set, from which the primary beam
c                and/or frequency may be extracted.
c    image  l    If .true. "tin" is the handle of an image, else its
c                the handle of a visibility.  You should have already
c                read a visibility for this to work (use UVNEXT say).
c          
c  Output in common:
c    pbcoeff dp   The polynomial coefficients
c    pbfac   r    The exponential factor for the Gaussian primary beam
c                 pbfac = -4 * ln(2) / Phi(rad)**2.  This is set even
c                 for POLY beams, so that the programmer can get a rough
c                 primary beam width. 
c    pbcut   r    Below this value, the correction function is
c                 unsafe or inaccurate.
c    pbtype  c    Type of correction; 'GAUSS', 'POLY' or 'SINGLE'
c                 The latter means do no correction
c    pbfreq  dp   Frequency in GHz
c    pbdone  c    Set to 'done' so PBGET knows PBINIT has been called
c--
c
c-----------------------------------------------------------------------
      include 'pb.h'   
      integer natfrq, nvlafrq, nhcfrq
      parameter (natfrq = 4, nvlafrq = 7, nhcfrq = 1)
c

      double precision atc(maxco,4), vlac(maxco)
      real hcfwhm, atcafwhm(natfrq), vlafwhm, atfrq(2,natfrq), 
     +  vlafrq(2,nvlafrq), hcfrq(2,nhcfrq), hdrfwhm
      character tel*20, aline*80, type*1
      integer ic, i, length, itel, len1
      logical hdprsnt, beam
c
      save atfrq, vlafrq, hcfrq, atc, vlac, hcfwhm
c
c Specify Gaussian FWHM (arcsec) * FREQ (GHz)
c
      data hcfwhm /11040.0/
      data atcafwhm /2874.0, 2982.0, 2898.0, 3036.0/
      data vlafwhm /2655.3/
c
c Specify frequency ranges in GHz for which the polynomial coefficients
c and Gaussian FWHM are valid for each telescope. VLA coefficients
c are surely wrong beyond 3-20 cm
c
      data atfrq  /1.15, 1.88, 
     +             2.10, 2.60, 
     +             4.30, 6.70,
     +             7.90, 9.30/
c
      data vlafrq /0.071,  0.075,
     +             0.297,  0.345,
     +             1.240,  1.810,
     +             4.240,  5.110,
     +             7.540,  9.060,
     +            14.240, 15.710,
     +            21.690, 24.510/
c
      data hcfrq  /74.0, 116.0/
c
c Set coefficients for each telescope and frequency range
c
      data atc /1.0, 8.99e-4, 2.15e-6, -2.23e-9, 1.56e-12,
     +          1.0, 1.02e-3, 9.48e-7, -3.68e-10, 4.88e-13,
     +          1.0, 1.08e-3, 1.31e-6, -1.17e-9, 1.07e-12,
     +          1.0, 1.04e-3, 8.36e-7, -4.68e-10, 5.50e-13/
c
      data vlac /0.9920378, 0.9956885e-3, 0.3814573e-5, -0.5311695e-8,
     +           0.3980963e-11/
c-----------------------------------------------------------------------
      pbfreq = freq
      if (fwhm.gt.0.0) then
c
c Use user's value
c
        pbtype = 'GAUSS'
        pbfac = -4.0 * log(2.0) / fwhm**2
        pbcut = 0.0
        do i = 1, maxco
          pbcoeff(i) = 0.0
        end do
      else
c 
c See if header item/variable "pbfwhm" exists and get telescope type
c
        if (image) then
          beam = hdprsnt(tin, 'pbfwhm')
          if (beam) call rdhdr (tin, 'pbfwhm', hdrfwhm, 0.0)
          call rdhda (tin, 'telescop', tel, 'none')
        else 
          call uvprobvr (tin, 'pbfwhm', type, length, beam)
          if (beam) call uvrdvrr (tin, 'pbfwhm', hdrfwhm, 0.0)
          call uvrdvra (tin, 'telescop', tel, 'none')
        end if
        itel = len1(tel)
c      
c Set coefficients or FWHM
c
        if (beam) then
c
c "pbfwhm" item in header, use it. 
c
          if (hdrfwhm.gt.0.0) then
            pbfac = -4.0 * log(2.0) / (atr * hdrfwhm)**2
            pbcut = 0.0
            pbtype = 'GAUSS'
          else if (hdrfwhm.lt.0.0) then
            call bug ('f', 
     +         'Unrecognized value for primary beam "pbfwhm" item')
          else
c  
c Single dish
c
            pbfac = 0.0
            pbcut = 0.0
            pbtype = 'SINGLE'
          end if
c
          do i = 1, maxco
            pbcoeff(i) = 0.0
          end do
        else if (tel.eq.'ATCA') then
          pbtype = 'POLY'
c
c Find and match frequency
c
          ic = 0
          do i = 1, natfrq
            if (pbfreq.ge.atfrq(1,i).and.pbfreq.le.atfrq(2,i)) ic = i
          end do
          if (ic.eq.0) then
            write (aline, 100) pbfreq, 'the ATCA'
100         format ('PBINIT: ', f8.4, 
     +              ' GHz is an invalid frequency for ', a)
            call bug ('f', aline)
          end if
c
c Set coefficients
c
          pbcut = 0.03
          do i = 1, maxco
            pbcoeff(i) = atc(i,ic)
          end do
c
c Set Gaussian factor as well.  Useful with PBINFO to get a 
c rough primary beam FWHM without fitting the polyomial
c
          pbfac = -4.0 * log(2.0) / (atr * atcafwhm(ic) / pbfreq)**2
        else if (tel.eq.'VLA') then
          pbtype = 'POLY'
c
c Find and match frequency
c
          ic = 0
          do i = 1, nvlafrq
            if (pbfreq.ge.vlafrq(1,i).and.pbfreq.le.vlafrq(2,i)) ic = i
          end do
          if (ic.eq.0) then
            write (aline, 100) pbfreq, 'the VLA'
            call bug ('f', aline)
          end if
c
          pbcut = 0.023
          do i = 1, maxco
            pbcoeff(i) = vlac(i)
          end do
c
c Set Gaussian factor as well.  Useful with PBINFO to get a 
c rough primary beam FWHM without fitting the polyomial
c
          pbfac = -4.0 * log(2.0) / (atr * vlafwhm / pbfreq)**2
        else if (tel.eq.'HATCREEK') then
          pbtype = 'GAUSS'
c
c Find and match frequency
c
          ic = 0
          do i = 1, nhcfrq
            if (pbfreq.ge.hcfrq(1,i).and.pbfreq.le.hcfrq(2,i)) ic = i
          end do
          if (ic.eq.0) then
            write (aline, 100) pbfreq, 'Hat Creek'
            call bug ('f', aline)
          end if
c
          pbcut = 0.0
          pbfac = -4.0 * log(2.0) / (atr * hcfwhm / pbfreq)**2
        else if (tel.eq.'none') then
          if (image) then          
            call bug ('f', 
     +        'PBINIT: No header items "pbfwhm and "telescop"')
          else
            call bug ('f', 
     +        'PBINIT: No variables "pbfwhm and "telescop"')
          end if
        else
          if (image) then
            call bug ('f', 'PBINIT: no header item "pbfwhm" & '
     +        //tel(1:itel)//' is an unknown telescope')
          else
            call bug ('f', 'PBINIT: no variable "pbfwhm" & '
     +        //tel(1:itel)//' is an unknown telescope')
          end if
        end if
      end if
c
c Tell PBGET that PBINIT has been called
c
      pbdone = 'done'
c
      end
c
c
c*pbget -- Get value of Primary Beam
c:image-data
c& nebk
c+
      real function pbget (rsq)
c
      implicit none
      real rsq
c
c     Function to return the primary beam. Subroutine PBINIT must
c     be used to set up the coefficients
c
c  Input
c    rsq     r    Square of radial distance (radians) from pointing centre
c
c  Input in common:
c    pbcoeff dp   The polynomial coefficients
c    pbfac   r    The exponential factor for the Gaussian primary beam
c                 pbfac = -4 * ln(2) / Phi(rad)**2
c    pbcut   r    Below this value, the correction function is
c                 unsafe or inaccurate; it will be set to zero.
c    pbtype  c    Type of correction; 'GAUSS', 'POLY' or 'SINGLE'
c    pbfreq  dp   Frequency in GHz needed for POLY beams
c    pbdone  c    Set to 'done' if PBINIT or PBSET has been called
c--
c
c-----------------------------------------------------------------------
      include 'pb.h'
c
      real fac, x, t
c-----------------------------------------------------------------------
      if (pbdone.ne.'done') 
     +  call bug ('f', 'PBGET: you must call PBINIT first')
c
      if (pbtype.eq.'POLY') then
        x = rsq * rtmsq * pbfreq**2
        fac = pbcoeff(1) + x*(pbcoeff(2) + x*(pbcoeff(3) + 
     +                     x*(pbcoeff(4) + x*pbcoeff(5))))
        if (fac.ne.0.0) then
          pbget = 1.0 / fac
        else
          call bug ('f', 'PBGET: infinite primary beam value')
        end if
      else if (pbtype.eq.'GAUSS') then
	t = pbfac * rsq
	if(t.gt.-12)then
          pbget = exp(pbfac * rsq)
	else
	  pbget = 0
	endif
      else if (pbtype.eq.'SINGLE') then
        pbget = 1.0
      else
        call bug ('f', 'PBGET: unrecognized primary beam type')
      end if
      if (pbget.lt.pbcut) pbget = 0.0
c
      end 
c
c
c*pbder -- Get derivative w.r. frequency of Primary Beam
c:image-data
c& nebk
c+
      real function pbder (rsq)
c
      implicit none
      real rsq
c
c     Function to return the derivative of the primary beam function
c     as a function of frequency.   Subroutine PBINIT must have already
c     been used to set up the coefficients
c
c  Input
c    rsq     r    Square of radial distance (radians) from pointing centre
c  Input in common:
c    pbcoeff dp   The polynomial coefficients
c    pbfac   r    The exponential factor for the Gaussian primary beam
c                 pbfac = -4 * ln(2) / Phi(rad)**2
c    pbcut   r    Below this value, the correction function is
c                 unsafe or inaccurate; it will be set to zero.
c    pbtype  c    Type of correction; 'GAUSS', 'POLY' or 'SINGLE'
c    pbdone  c    Set to 'done' if PBINIT or PBSET has been called
c--
c
c-----------------------------------------------------------------------
      include 'pb.h'
c
      real pbget, fac, x
c-----------------------------------------------------------------------
      if (pbdone.ne.'done') 
     +  call bug ('f', 'PBDER: you must call PBINIT first')
      if (pbfreq.le.0.0) call bug ('f','PBDER: Invalid frequency')
c
      if (pbtype.eq.'POLY') then
        x = rsq * rtmsq * pbfreq * pbfreq
        fac = rsq*rtmsq*pbfreq*(2.0*pbcoeff(2) + 
     +        x*(4.0*pbcoeff(3) + x*(6.0*pbcoeff(4) + 
     +        x*8.0*pbcoeff(5))))
        pbder = -fac * pbget(rsq)**2
      else if (pbtype.eq.'GAUSS') then
        pbder = 2.0 * pbfac * rsq * pbget(rsq) / pbfreq
      else if (pbtype.eq.'SINGLE') then
        pbder = 0.0
      else
        call bug ('f', 'PBDER: unrecognized primary beam type')
      end if
      if (pbget(rsq).lt.pbcut) pbder = 0.0
c
      end 
c
c
c*pbinfo -- Get information about primary beam
c:image-data
c& nebk
c+
      subroutine pbinfo (fwhm, coeffs, cut, type)
c
      implicit none
      real fwhm, cut
      double precision coeffs(5)
      character*(*) type
c
c     Return information about the primary beam correction
c
c  Input in common:
c    pbcoeff dp   The polynomial coefficients
c    pbfac   r    The exponential factor for the Gaussian primary beam
c                 pbfac = -4 * ln(2) / Phi(rad)**2
c    pbcut   r    Below this value, the correction function is
c                 unsafe or inaccurate; it will be set to zero.
c    pbtype  c    Type of correction; 'GAUSS', 'POLY' or ' '
c                 ' ' means do no correction.
c    pbfreq  dp   Frequency in GHz
c
c  Output
c    fwhm    r    Gaussian FWHM in radians.  This is set for both GAUSS 
c                 and POLY primary beams.
c    coeffs  dp   = pbcoeff
c    cut     r    = pbcut
c    type    c    = pbtype
c
c--
c
c-----------------------------------------------------------------------
      include 'pb.h'
      integer i
c-----------------------------------------------------------------------
      if (pbdone.ne.'done') call bug ('f',
     +  'PBINFO: You must call PBINIT first')
c
      if (pbtype.eq.'POLY') then
        do i = 1, maxco
          coeffs(i) = pbcoeff(i)
        end do
      else
        do i = 1, maxco
          coeffs(i) = 0.0
        end do
      end if
c
c The FWHM is set for both GAUSS and POLY primary beams.
c This allows you to get an approximate primary beam
c without having to fit the polynomial.
c
      if (pbtype.eq.'GAUSS' .or. pbtype.eq.'POLY') then
        fwhm = 2.0 * sqrt(log(2.0) / abs(pbfac))
      else if (pbtype.eq.'SINGLE') then
        fwhm = 0.0
      else
        call bug ('f', 'PBINFO: unrecognized primary beam type')
      end if
c
      cut = pbcut
      type = pbtype
c
      end
c
c
c*pbcheck -- 
c:image-data
c& nebk
c+
      subroutine pbcheck (fwhm, tin, image, doder, needf)
c
      implicit none
      integer tin
      real fwhm
      logical image, doder, needf
c
c     Find out if the primary beam routines are going to need
c     to be able to work out the frequency.
c
c  Input:
c    fwhm   r    If this is positive, then a Gaussian primary beam
c                of FWHM radians will be set.
c    tin    i    Handle of data set
c    image  l    If .true. "tin" is the handle of an image, else its
c                the handle of a visibility.  You should already have
c                read a visibility for this to work (use UVNEXT say).
c    doder  l    If .true., then you plan to call PBDER
c          
c--
c
c-----------------------------------------------------------------------
      character tel*20, type*1
      integer length, itel, len1
      real hdrfwhm
      logical hdprsnt, beam
c-----------------------------------------------------------------------
      if (doder) then
c
c Always need frequency if calling PBDER
c
        needf = .true.
        return
      end if
c
      if (fwhm.gt.0.0) then
c
c Just set user specified Gaussian
c
        needf = .false.
      else
c
c May need frequency yet
c
        if (tin.le.0) then
          call bug ('f', 'PBCHECK: input file not open')
        else
          if (image) then
            beam = hdprsnt(tin, 'pbfwhm')
            if (beam) call rdhdr (tin, 'pbfwhm', hdrfwhm, 0.0)
            call rdhda (tin, 'telescop', tel, 'none')
          else 
            call uvprobvr (tin, 'pbfwhm', type, length, beam)
            if (beam) call uvrdvrr (tin, 'pbfwhm', hdrfwhm, 0.0)
            call uvrdvra (tin, 'telescop', tel, 'none')
          end if
          itel = len1(tel)
c      
          if (beam) then
c
c Header item/variable specifies Gaussian
c
            needf = .false.
          else
            if (tel.eq.'ATCA' .or. tel.eq.'VLA' .or.
     +          tel.eq.'HATCREEK') then
c
c Internal representation needs frequency
c
              needf = .true.
            else if (tel.eq.'none') then
              if (image) then
                call bug ('f', 
     +            'PBCHECK: No header items "pbfwhm and "telescop"')
              else
                call bug ('f', 
     +            'PBCHECK: No variables "pbfwhm and "telescop"')
              end if
            else
              if (image) then
                call bug ('f', 'PBCHECK: no header item "pbfwhm" & '
     +            //tel(1:itel)//' is an unknown telescope')
               else
                call bug ('f', 'PBCHECK: no variable "pbfwhm" & '
     +            //tel(1:itel)//' is an unknown telescope')
               end if
            end if
          end if
        end if
      end if
c
      end

