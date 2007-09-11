      program uvsub

c= uvsub - Subtract point source models from visibilities
c& nebk
c: uv analysis
c+
c       UVSUB reads a file containing point source models and subtracts
c       them from the visibilities.  The models can be for a subset of
c       the channels, but the output file will contain all the input
c       channels.
c
c@ vis
c       The name of the input uv data sets.  No default.
c@ line
c       Standard Linetype.  This assumes only a single spectral
c       window is processed.
c@ model
c       A text file containing the models.  Each row should contain
c       four columns giving
c
c         VELOCITY    FLUX DENSITY    X OFFSET      Y OFFSET
c
c       The velocity must be in Km/s (radio definition), with the same
c       rest standard as the data-set.  The flux density is in Jy and
c       the offsets in radians are from the phase centre in the
c       convention
c
c             offset = point source location - phase centre
c
c       Lines beginning with a "#" are ignored.
c@ out
c       The name of the output uv data set.  No default.
c--
c  History:
c    05jan94 nebk  Original version.
c    12jan94 nebk  Convert to use velocity line type
c    17aug94 rjs   Fiddle offsets to give better results.
c    14nov95 nebk  Remove dependence on cgsubs.for
c
c $Id: uvsub.for,v 1.2 2007/09/11 01:22:01 cal103 Exp $
c-----------------------------------------------------------------------
      include 'maxdim.h'
      integer MAXMOD
      parameter (MAXMOD = 500)

      logical flags(MAXCHAN)
      integer iostat, lmod, lout, lvis, nchan, nmod, npol, nschan,
     :        ntemp, pol
      real    vel1, vels, velw
      double precision model(4,MAXMOD), preamble(4), sfreq(MAXCHAN),
     :        vel(MAXCHAN)
      complex data(MAXCHAN)
      character ltype*8, modl*80, out*80, versan*80, version*80, vis*80

      character ltypes(2)*8
      data ltypes/'channel ','velocity'/
c-----------------------------------------------------------------------
      version = versan ('uvsub',
     :  '$Id: uvsub.for,v 1.2 2007/09/11 01:22:01 cal103 Exp $')

c     Get the inputs
      call keyini
      call keya ('vis', vis, ' ')
      call keymatch ('line', 2, ltypes, 1, ltype, ntemp)
      if (ntemp.eq.0) ltype = ltypes(1)
      call keyi ('line', nschan, 0)
      call keyr ('line', vel1, 1.0)
      call keyr ('line', velw, 1.0)
      call keyr ('line', vels, velw)
      call keya ('model', modl, ' ')
      call keya ('out', out, ' ')
      call keyfin

      if (vis.eq.' ')  call bug ('f', 'An input must be given')
      if (modl.eq.' ') call bug ('f', 'A model must be given')
      if (out.eq.' ')  call bug ('f', 'An output must be given')

c     Open the model file and read it.
      call txtopen (lmod, modl, 'old', iostat)
      if (iostat.ne.0) call bug ('f', 'Error opening model file')
      call decmod (lmod, MAXMOD, model, nmod)
      call txtclose (lmod)

c     Open the visibility files and prepare to copy the data.
      call uvopen (lvis, vis, 'old')
      call varinit (lvis, 'velocity')
      call uvset (lvis, 'data', 'velocity', nschan, vel1, velw, vels)

      call uvopen (lout, out, 'new')
      call varonit (lvis, lout, 'velocity')

c     Make the output history.
      call hdcopy (lvis, lout, 'history')
      call hisopen (lout, 'append')
      call hiswrite (lout, 'UVSUB: Miriad ' // version)
      call hisinput (lout, 'UVSUB')
      call hisclose (lout)

c     Get the first visibility
      call uvread (lvis, preamble, data, flags, MAXCHAN, nchan)

c     Fudge the offsets.
      call modfudg (lvis, nmod, model)

      do while (nchan.gt.0)
        call uvinfo (lvis, 'sfreq', sfreq)
        call uvinfo (lvis, 'velocity', vel)
        call uvgetvri (lvis, 'npol', npol, 1)
        call uvgetvri (lvis, 'pol', pol, 1)

c       Subtract the model.
        call modsub (nmod, model, nchan, vel, sfreq, preamble, data)

c       Copy variables and data.
        call varcopy (lvis, lout)
        call uvputvri (lout, 'npol', npol, 1)
        call uvputvri (lout, 'pol', pol, 1)
        call uvwrite (lout, preamble, data, flags, nchan)

c       Get the next visibility.
        call uvread (lvis, preamble, data, flags, MAXCHAN, nchan)
      end do

c     Close up shop.
      call uvclose (lvis)
      call uvclose (lout)

      end


      subroutine modfudg (lvis, nmod, model)
c-----------------------------------------------------------------------
c    Convert model offsets from true offsets to pseudo-offsets to
c    correct for geometry.
c
c  Input:
c    lvis    Handle of the input visibility dataset
c    nmod    Number of models
c    model   VELOCITY, FLUX DENSITY, X OFFSET, YOFFSET
c
c-----------------------------------------------------------------------
      integer lvis, nmod
      double precision model(4,nmod)

      integer i
      double precision x(2)
c-----------------------------------------------------------------------
c     Initialise the coordinate routines.
      call coinit(lvis)

      do i=1,nmod
        x(1) = model(3,i)
        x(2) = model(4,i)
        call cocvt(lvis,'ow/ow',x,'op/op',model(3,i))
      enddo

      call cofin(lvis)

      end


      subroutine decmod (lmod, MAXMOD, model, nmod)
c-----------------------------------------------------------------------
c     Read model text file and decode
c
c  Input
c    lmod    Handle of file
c    MAXMOD  Max number of model entries allowed
c  Output
c    model   VELOCITY, FLUX DENSITY, X OFFSET, YOFFSET
c    nmod    Number of models
c
c-----------------------------------------------------------------------
      integer MAXMOD, lmod, nmod
      double precision model(4,MAXMOD)

      integer iostat, ilen, iline
      character aline*100

      integer len1
      character itoaf*2
c-----------------------------------------------------------------------
c     Read and decode models.
      iline = 0
      nmod = 0
      iostat = 0

      do while (iostat.ne.-1)
        aline = ' '
        call txtread (lmod, aline, ilen, iostat)
        if (iostat.eq.0) then
          if (aline(1:1).ne.'#' .and. aline.ne.' ') then

c           Fish out model.
            iline = iline + 1
            if (nmod.eq.MAXMOD) then
              call bug ('w', 'Reducing no. models to max. '//
     :                       'allowed = '//itoaf(MAXMOD))
              iostat = -1
            else
              nmod = nmod + 1
              ilen = len1(aline)
              call posdec2 (nmod, aline(1:ilen), model(1,nmod))
            end if
          end if
        else
          if (iostat.ne.-1) call bug ('f',
     :       'Error reading from input slice positions file')
        end if
      end do

      if (nmod.gt.0) then
        write (aline,100) nmod
100     format ('There were ', i4, ' models')
        call output (aline)
      else
        call bug ('f', 'There were no valid models')
      end if

      end


      subroutine posdec2 (nmod, aline, model)
c-----------------------------------------------------------------------
c     Decode string into model values
c
c     Input:
c       nmod     Model number
c       aline    Input string
c     Output
c       model    VELOCITY FLUX  DELX  DELY
c                VELOCITY in KM/S, DELX and DELY are in radians
c
c-----------------------------------------------------------------------
      integer nmod
      double precision model(4)
      character*(*) aline

      integer slen, lena, ipres, icomm(4)
      logical ok
      character str*4, estr*80

      integer len1
      character itoaf*4
c-----------------------------------------------------------------------
c     Prepare string for matodf.
      str = itoaf(nmod)
      slen = len1(str)
      call strprp (4, aline, icomm, ipres, lena)
      if (ipres.lt.4) then
        estr = 'There are insufficient fields for model # '//
     :          str(1:slen)
        call bug ('f', estr)
      end if

c     Now extract the numbers.
      call matodf (aline(1:lena), model, ipres, ok)
      if (.not.ok .or. ipres.ne.4) then
        estr = 'Error decoding model # '//str(1:slen)
        call bug ('f', estr)
      end if

      end


      subroutine modsub (nmod, model, nchan, vel, sfreq, uv, data)
c-----------------------------------------------------------------------
c     Subtract the model
c
c  Input
c    nmod     Number of models
c    model    MOdel records:  chan, flux (Jy), delx (rad), dely (rad)
c    nchan    Number of channels
c    vel      Velocity of each channel in km/s
c    sfreq    Frequency of each channel in GHz
c    uv       u and v in ns
c  Input/output
c    data     complex visibilities
c
c-----------------------------------------------------------------------
      integer nmod, nchan
      double precision model(4,nmod), vel(nchan), sfreq(nchan), uv(2)
      complex data(nchan)

      include 'mirconst.h'
      include 'maxdim.h'

      double precision arg, delv, v1
      complex modvis
      integer ic, j
      logical first
      save first
      data first /.true./
c-----------------------------------------------------------------------
      delv = vel(2) - vel(1)
      v1 = vel(1)

c     Loop over models.
      do j = 1, nmod
c       Compute channel number.
        ic = nint((model(1,j)-v1)/delv) + 1
        if (ic.lt.1 .or. ic.gt.nchan) call bug ('f',
     :    'Indexing error in MODSUB')
        if (first) write (*,*)
     :     'v_mod, idx, v_dat', model(1,j),ic,vel(ic)

c       Compute model visibility and subtract it.
        arg =  dtwopi*sfreq(ic) * (uv(1)*model(3,j) + uv(2)*model(4,j))
        modvis = model(2,j)*cmplx(cos(arg), sin(arg))
        data(ic) = data(ic) - modvis
      end do

      first = .false.
      end


      subroutine strprp (maxloc, aline, comloc, nfield, lena)
c-----------------------------------------------------------------------
      character*(*) aline
      integer nfield, maxloc, comloc(maxloc), lena
c-----------------------------------------------------------------------
c
c     Take a string with a number of mixed ascii/numeric fields in it
c     and prepare it for use by stripping out extra white space and
c     replacing the space delimiters by commas (matod needs this).
c
c     Input:
c       maxloc  Maximum number of fields allowed in string
c     Input/output:
c       aline   String
c     Output
c       comloc  Locations along string of comma delimiters for
c               each field.  comloc(1) is the comma between the
c               first and second fields etc
c       nfield  Number of fields in string
c       lena    Length of output string after massaging
c--
c-----------------------------------------------------------------------
      integer i, j, lenb, idx
      character bline*132

      integer len1
c-----------------------------------------------------------------------
c     Strip leading white space
      idx = 1
      do while (aline(idx:idx).eq.' ')
        idx = idx + 1
      end do
      bline = aline(idx:)
      aline = ' '
      aline = bline

c     Strip additional white space out. Catch cases where commas
c     already the separator too.
      bline = ' '
      lena = len1(aline)
      bline(1:1) = aline(1:1)
      j = 2
      do i = 2, lena
        if ((aline(i:i).eq.' ' .and. aline(i-1:i-1).eq.' ') .or.
     :      (aline(i:i).eq.' ' .and. aline(i-1:i-1).eq.',')) then
          continue
        else
          bline(j:j) = aline(i:i)
          j = j + 1
        end if
      end do

c     Replace spaces and colons (which may come from RA or DEC formatted
c     strings) by commas (for matodf) and count fields.
      lenb = len1(bline)
      nfield = 0
      do i = 1, lenb
        if (bline(i:i).eq.' ' .or. bline(i:i).eq.':' .or.
     :      bline(i:i).eq.',') then
          bline(i:i) = ','
          nfield = nfield  + 1
          if (nfield.gt.maxloc) call bug ('f',
     :      'STRPRP: Too many fields for internal storage')
          comloc(nfield) = i
        end if
      end do
      nfield = nfield + 1
      if (bline(lenb:lenb).eq.',') then
        nfield = nfield - 1
        lenb = lenb - 1
      end if
      aline = bline
      lena = lenb

      end
