      program uvsub

c= uvsub - Subtract point source models from visibilities
c& nebk
c: uv analysis
c+
c       UVSUB reads a file containing point source models and subtracts
c       them from the visibilities.  Each model applies either to a
c       single spectral channel, or to all channels (continuum mode).
c       The output file contains all of the input channels whether
c       modified or not.
c
c@ vis
c       The name of the input visibility data sets (no default).
c@ line
c       Standard Linetype: 'channel' (default) and 'velocity' are
c       recognized.  Do 'help line' for more information.  uvsub assumes
c       that only a single spectral window is processed.
c@ model
c       The name of a text file containing the models (no default).
c       Each row must contain four columns which depend on the line type
c       specified.  For line = 'channel':
c
c         CHAN No.    FLUX DENSITY    X OFFSET      Y OFFSET
c
c       A channel number of zero is recognized as applying to all
c       channels (i.e. continuum).  Otherwise, it is an error for the
c       channel number to lie outside the range in the data.
c
c       The flux density is in Jy and the offsets, true angular
c       distances in radians, are measured from the phase centre in the
c       sense
c
c         offset = point source location - phase centre
c
c       For line = 'velocity', the channel number is replaced by
c       velocity:
c
c         VELOCITY    FLUX DENSITY    X OFFSET      Y OFFSET
c
c       The velocity must be in km/s, radio definition, i.e.
c
c         v = c (f0 - f) / f0,
c
c       with the same rest standard as the data-set.  The model
c       visibility will be subtracted from the channel closest to this
c       velocity.  Note that a rest frequency (f0) must be set in the
c       input visibility dataset in order to convert between velocity
c       and channel number (i.e. frequency).
c
c       Lines beginning with "#" are ignored.
c@ out
c       The name of the output visibility data set (no default).
c
c--
c  History:
c    05jan94 nebk  Original version.
c    12jan94 nebk  Convert to use velocity line type
c    17aug94 rjs   Fiddle offsets to give better results.
c    14nov95 nebk  Remove dependence on cgsubs.for
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'mirconst.h'

      integer MAXMOD
      parameter (MAXMOD = 500)

      logical first, flags(MAXCHAN)
      integer ichan, ichan1, ichan2, imod, lout, lvis, nchan, nmod,
     :        npol, ntemp, pol
      double precision    start, step, width
      double precision arg, delv, model(4,MAXMOD), preamble(4),
     :        sfreq(MAXCHAN), vel(MAXCHAN), uv(2), x(2)
      complex data(MAXCHAN)
      character ltype*8, modl*80, out*80, versan*72, version*72, vis*80

      character ltypes(2)*8
      data ltypes /'channel ','velocity'/
c-----------------------------------------------------------------------
      version = versan('uvsub')

c     Get the inputs
      call keyini
      call keya ('vis', vis, ' ')

      call keymatch ('line', 2, ltypes, 1, ltype, ntemp)
      if (ntemp.eq.0) ltype = ltypes(1)
      call keyi ('line', nchan, 0)
      if (ltype.eq.'channel') then
        call keyd ('line', start, 1.d0)
        call keyd ('line', width, 1.d0)
      else
        call keyd ('line', start, 0.d0)
        call keyd ('line', width, 0.d0)
      end if
      call keyd ('line', step, width)

      call keya ('model', modl, ' ')
      call keya ('out', out, ' ')
      call keyfin

      if (vis.eq.' ')  call bug ('f', 'An input must be given')
      if (modl.eq.' ') call bug ('f', 'A model must be given')
      if (out.eq.' ')  call bug ('f', 'An output must be given')

c     Read the model file.
      call decmod (modl, MAXMOD, model, nmod)

c     Open the visibility files and prepare to copy the data.
      call uvopen (lvis, vis, 'old')
      call varinit (lvis, ltype)
      call uvset (lvis, 'data', ltype, nchan, start, width, step)

      call uvopen (lout, out, 'new')
      call varonit (lvis, lout, ltype)

c     Make the output history.
      call hdcopy (lvis, lout, 'history')
      call hisopen (lout, 'append')
      call hiswrite (lout, 'UVSUB: Miriad ' // version)
      call hisinput (lout, 'UVSUB')
      call hisclose (lout)

c     Convert true offsets to pseudo-offsets to correct for geometry.
      call coinit(lvis)

      do imod = 1, nmod
        x(1) = model(3,imod)
        x(2) = model(4,imod)
        call cocvt(lvis, 'ow/ow', x, 'op/op', model(3,imod))
      enddo

      call cofin(lvis)

c     Get the first visibility
      call uvread (lvis, preamble, data, flags, MAXCHAN, nchan)


      first = .true.
      do while (nchan.gt.0)
c       Get channel frequencies (GHz).
        call uvinfo (lvis, 'sfreq', sfreq)
        if (ltype.eq.'velocity') then
c         Get channel velocities (km/s).
          call uvinfo (lvis, 'velocity', vel)
          delv = vel(2) - vel(1)
        end if

        uv(1) = preamble(1)
        uv(2) = preamble(2)

c       Loop over models.
        do imod = 1, nmod
          if (ltype.eq.'channel') then
            ichan = nint(model(1,imod))
            if (ichan.eq.0) then
c             Do all channels (continuum).
              ichan1 = 1
              ichan2 = nchan
            else
              ichan1 = ichan
              ichan2 = ichan
            end if
          else
c           Compute channel number for the specified velocity.
            ichan1 = 1 + nint((model(1,imod) - vel(1)) / delv)
            ichan2 = ichan1

            if (first) then
              write (*, *) 'v_mod, idx, v_dat', model(1,imod), ichan1,
     :          vel(ichan1)
            end if
          end if

          if (ichan1.lt.1 .or. ichan1.gt.nchan) then
            call bug ('f', 'Channel number out of range')
          end if

          do ichan = ichan1, ichan2
c           Compute model visibility and subtract it.
            arg = dtwopi*sfreq(ichan) * (uv(1)*model(3,imod) +
     :                                   uv(2)*model(4,imod))
            data(ichan) = data(ichan) -
     :        model(2,imod)*cmplx(cos(arg), sin(arg))
          end do
        end do

        first = .false.

c       Copy variables and data.
        call varcopy (lvis, lout)

        call uvgetvri (lvis, 'npol', npol, 1)
        call uvputvri (lout, 'npol', npol, 1)

        call uvgetvri (lvis, 'pol', pol, 1)
        call uvputvri (lout, 'pol', pol, 1)

        call uvwrite (lout, preamble, data, flags, nchan)

c       Get the next visibility.
        call uvread (lvis, preamble, data, flags, MAXCHAN, nchan)
      end do

c     Close up shop.
      call uvclose (lvis)
      call uvclose (lout)

      end


      subroutine decmod (modl, MAXMOD, model, nmod)
c-----------------------------------------------------------------------
c     Read model text file and decode.
c
c  Input
c    modl    Model file.
c    MAXMOD  Max number of model entries allowed
c  Output
c    model   VELOCITY, FLUX DENSITY, X OFFSET, YOFFSET
c    nmod    Number of models
c
c-----------------------------------------------------------------------
      integer   MAXMOD, nmod
      double precision model(4,MAXMOD)
      character modl*(*)

      logical ok
      integer alen, icomm(4), iostat, iline, ipres, lmod, slen
      character aline*100, str*4

      integer len1
      character itoaf*4
c-----------------------------------------------------------------------
c     Open the model file.
      call txtopen (lmod, modl, 'old', iostat)
      if (iostat.ne.0) call bug ('f', 'Error opening model file')

c     Read and decode models.
      iline = 0
      nmod = 0
      iostat = 0

      do while (iostat.ne.-1)
        aline = ' '
        call txtread (lmod, aline, alen, iostat)
        if (iostat.eq.0) then
          if (aline(1:1).ne.'#' .and. aline.ne.' ') then

c           Fish out model.
            iline = iline + 1
            if (nmod.eq.MAXMOD) then
              call bug ('w', 'Reducing no. models to max. ' //
     :                       'allowed = ' // itoaf(MAXMOD))
              iostat = -1
            else
              nmod = nmod + 1
              str  = itoaf(nmod)
              slen = len1(str)

c             Prepare string for matodf.
              alen = len1(aline)
              call strprp (4, aline, icomm, ipres, alen)
              if (ipres.lt.4) then
                call bug ('f',
     :            'Insufficient fields for model # ' // str(1:slen))
              end if

c             Now extract the numbers.
              call matodf (aline(1:alen), model(1,nmod), ipres, ok)
              if (.not.ok .or. ipres.ne.4) then
                call bug ('f', 'Error decoding model # ' // str(1:slen))
              end if

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

      call txtclose (lmod)

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
