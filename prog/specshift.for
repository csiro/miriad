c= SPECSHIFT - shift velocity profiles
c& tw
c: analysis
c+
c   SPECSHIFT shifts all spectra in a cube according to a 2-D template
c   (a velocity field).  The velocity of the template is made the
c   central pixel of the spectrum.  If the template is blanked or out
c   of range the spectrum is unchanged.  Based on Bart Wakker's HANNING.
c
c@ in
c        The input data cube. vxy and xyv images are acceptable inputs.
c        No default.
c@ region
c        The region of the input cube to modify.  By default the whole
c        cube is processed.
c@ tin
c        The template image, should be 2-D with same RA/DEC axes as the
c        input cube.
c@ refpix
c        The velocity pixel which will be assigned to the template
c        velocity.  Default is the middle pixel of the v-range.
c@ options
c         doblank    Don't wrap profile around; blank if out of range.
c                    By default the edge channels are assumed to be
c                    noise so the profile is wrapped when shifting
c                    beyond edges.
c@ out
c        The output image.
c--
c
c   History:
c
c    tw   26mar06  Created.
c    tw   22jun07  doblank option.
c
c-----------------------------------------------------------------------
       program specshift

       character*80 version
       parameter (version = 'specshift: version 1.0 22jun07')

       include      'maxnax.h'

       integer      tinp, tout, ttpl
       integer      nprofs, nchan
       real         refpix
       logical      doblank

       call output(version)
       call inputs(tinp, tout, ttpl, nprofs, nchan, refpix,doblank)
       call vshift(tinp, tout, ttpl, nprofs, nchan, refpix,doblank)
       call finish(tinp, tout, ttpl, version)

       end

c-----------------------------------------------------------------------

       subroutine inputs(tinp,tout,ttpl,nprofs,nchan,refpix,doblank)

       include      'maxnax.h'

       integer      tinp, tout, ttpl
       integer      nprofs, nchan

       character*1024 inp, out, tpl
       integer      MAXBOXES, NOPTS
       parameter    (MAXBOXES = 1024)
       parameter    (NOPTS = 1)
       integer      boxes(MAXBOXES)

       integer      naxis
       integer      axlen(MAXNAX), taxlen(MAXNAX)
       integer      iblc(MAXNAX),  itrc(MAXNAX)
       integer      viraxlen(MAXNAX), vircsz(MAXNAX)
       integer      i

       character    velaxis, msg*132, opts(NOPTS)*8
       integer      vaxnr, len1
       real         refpix
       data         opts/'doblank '/
       logical      present(NOPTS), doblank

       call keyini

       call keyf('in',  inp, ' ')
       call keyf('tin', tpl, ' ')
       call keya('out', out, ' ')
       call assertl(inp.ne.' ', 'Input cube name is missing')
       call assertl(tpl.ne.' ', 'Template image name is missing')
       call assertl(out.ne.' ', 'Output file name is missing')
       call keyr('refpix',refpix,0.0)
       naxis = MAXNAX
       call xyzopen(tinp, inp, 'old', naxis, axlen)

       call boxinput('region', inp, boxes, MAXBOXES)
       call boxset(  boxes, naxis, axlen, ' ')
       call boxinfo( boxes, naxis, iblc, itrc)

       call options('options',opts,present,NOPTS)
       doblank = present(1)

       call keyfin

c Set up the input and output cube
       velaxis = 'z'
       call fndaxnum(tinp, 'freq', velaxis, vaxnr)

       call xyzsetup(tinp, velaxis, iblc, itrc, viraxlen, vircsz)
       nprofs = vircsz(naxis) / vircsz(1)
       nchan     = viraxlen(1)

       call xyzopen(tout, out, 'new', naxis, axlen)
       call headcp(tinp, tout, naxis, 0, iblc, itrc)
       call xyzsetup(tout, velaxis, iblc, itrc, viraxlen, vircsz)

c Set up the input image
       call xyopen(ttpl, tpl, 'old', maxnax, taxlen)
       call rdhdi(ttpl, 'naxis', naxis, 0)
       if (naxis.gt.2 .and. taxlen(3).gt.1) then
         msg = tpl(1:len1(tpl))//' appears to have >2 axes'
         call bug('w', msg)
         msg = 'Only the first plane of the 3rd axis will be used'
         call bug('w', msg)
       endif

c Check image sizes for consistency
       if (axlen(1).ne.taxlen(1)) then
         msg = 'Unequal dimensions for input images on axis 1'
         call bug('f', msg)
       endif
       if (axlen(2).ne.taxlen(2)) then
         msg = 'Unequal dimensions for input images on axis 2'
         call bug('f', msg)
       endif

       return
       end

c-----------------------------------------------------------------------

       subroutine vshift(tinp,tout,ttpl,nprofs,nchan,refpix,doblank)

       integer       tinp, tout, ttpl
       integer       nprofs, nchan
       real          refpix, vel, fracshift

       include       'maxdim.h'
       include       'maxnax.h'
       real          data(MAXDIM), work(MAXDIM), tdata(MAXDIM)
       logical       mask(MAXDIM), workmsk(MAXDIM), flags(MAXDIM)
       logical       doblank, outrange
       integer       coords(MAXNAX)
       double precision x(MAXDIM), y(MAXDIM), u
       double precision b(MAXDIM), c(MAXDIM), d(MAXDIM)

       integer       i, j, jold, ictrpx, ivelpx, ivaxnr
       double precision velpx, seval
       character     line*80

c Set up cube
       call coInit(tinp)
       call coFindAx(tinp,'spectral',ivaxnr)
c Set up template
       call xysetpl(ttpl, 1, 1)

       if (refpix.gt.0) then
          ictrpx = nint(refpix)
       else
          ictrpx = nint(nchan/2.0)
       endif
       write(line,'(a,i4)') 'Shifting template velocity to pixel',ictrpx
       call output(line)

       do i = 1, nprofs
c Read the profile
          call xyzs2c(tinp, i, coords)
          call xyzread(tinp, coords, data, mask, nchan)
c Read the velocity from the image
          call xyread(ttpl, coords(2), tdata)
          call xyflgrd(ttpl, coords(2), flags)
          if (flags(coords(1))) then
             vel = tdata(coords(1))
             call coCvt1(tinp,ivaxnr,'aw',dble(vel),'ap',velpx)
             ivelpx = int(velpx)
             fracshift = velpx - ivelpx
c            write(40,*) 'vel,velpx,fracshift: ',vel,velpx,fracshift
          else
             ivelpx = 0
          endif
c Make the shift if velocity is in range; otherwise do nothing.
          if (ivelpx.gt.1 .and. ivelpx.lt.nchan) then
             do j = 1, nchan + 1
                outrange=.false.
                jold = j + ivelpx - ictrpx
                do while (jold.lt.1)
                   jold = jold + nchan
                   outrange=.true.
                enddo
                do while (jold.gt.nchan)
                   jold = jold - nchan
                   outrange=.true.
                enddo
                work(j) = data(jold)
                if (doblank .and. outrange) then
                   workmsk(j) = .false.
                else
                   workmsk(j) = mask(jold)
                endif
                x(j) = dble(j)
                y(j) = dble(work(j))
             enddo
             call spline(nchan+1, x, y, b, c, d)
             do j = 1, nchan
                u = x(j) + fracshift
                work(j) = seval(nchan+1, u, x, y, b, c, d)
             enddo
          else
             do j = 1, nchan
                work(j) = data(j)
                workmsk(j) = mask(j)
             enddo
          endif
          call xyzprfwr(tout, i, work, workmsk, nchan)
       enddo

       return
       end

c-----------------------------------------------------------------------

       subroutine finish(tinp, tout, ttpl, version)

       integer       tinp, tout, ttpl
       character*(*) version
       character*80  line

       call hisopen( tout, 'append')
       line = 'SPECSHIFT: ' // version
       call hisinput(tout, 'SPECSHIFT')
       call hisclose(tout)
       call xyzclose(tinp)
       call xyzclose(tout)
       call xyclose(ttpl)

       return
       end
