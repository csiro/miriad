      program imframe

c= IMFRAME - Interchange axes of a cube and decrease/enlarge the frame
c& bpw
c: map manipulation
c+
c  IMFRAME takes any part of an input cube, rotates it over any
c  combination of 90 deg angles and writes the result anywhere in an
c  output cube. This works on any cube whose longest axis is less than
c  262144 pixels long.
c  Note that IMFRAME does not resample images, only whole-pixel
c  transformations are possible. See REGRID to resample/rotate in a more
c  arbitrary way.
c@ in
c  The input image. No default.
c< region
c@ box
c  Some subcommands of the region keyword work only nicely on 3-d cubes.
c  This is too limited for imframe, so an alternative keyword is
c  provided for other cases, which is less fancy than region= but it
c  does the job.  The values to be given are a list of ranges for each
c  coordinate axis, in relative pixel coordinates, in the order
c    xmin,xmax,ymin,ymax,...
c  etc. c  Note that this is different from the standard region order
c  xmin,ymin,xmax,ymax.
c  The default is to take the whole cube. box= and region= are mutually
c  exclusive.
c@ out
c  The output image. No default.
c@ frame
c  The size of the output dataset.  The values to be given are a list of
c  ranges for each coordinate axis in relative pixel coordinates, in the
c  order
c    xmin,xmax,ymin,ymax,...
c  etc. Note that this is different from the standard region order
c  xmin,ymin,xmax,ymax. The defined frame may be bigger or smaller than
c  the input box. The values refer to the axes in the original ordering.
c  If not given, the input box is assumed (from either region= or box=,
c  whichever one was given).
c@ goal
c  The new axis ordering in terms of the old axis ordering: i.e. 'zxy'
c  makes the original 'z'-axis the 'x'-axis of the output. It is only
c  necessary to give the axes that are changed, the other axes are
c  concatenated to the list, i.e. 'z' would do just as well in the
c  example above. The default is not to exchange axes. Precede an
c  axisname with a - to reverse the direction.
c--
c
c  History:
c    bpw  02may91  Created basic version for max 3-d cubes and no
c                  reversal of axes or framing
c    bpw  08may91  Introduced the box alternative keyword
c    bpw  13may91  Now also does reversals with reordering
c    bpw  20may91  Moved some reversal functionality to xyzio.c
c    bpw  22jun91  Changed box and frame from absolute to relative
c                  pixels.
c    bpw  22jun91  Installed
c    bpw  15jan92  Clarified box= and frame= keyword documentation
c    bpw  27mar92  Changed assert into assertl
c    bpw  09dec92  Read crpix as a real, and manipulate box/frame as
c                  real.
c    bpw  28jan93  Corrected real/integer mistake introduce in 9dec
c                  update.
c    bpw  01feb93  crpix should have been double precision
c    rjs  04feb93  Change amax1,amin1 to max,min. amax1 and amin1 do not
c                  take double precision args.
c    bpw  02mar93  Include maxnax.h
c    bpw  03mar93  Include maxdim.h instead of own BUFSIZE parameter
c    rjs  24mar93  Halve the size of the MAXBUF arrays.
c    rjs  31aug98  Fix call to rdhdd and eliminate flint complaints.
c-----------------------------------------------------------------------

      character*80 version
      parameter (version = 'imframe version 1.1 31-Aug-98')

      include   'maxnax.h'
      include   'maxdim.h'
      integer   MAXBUF2
      parameter (MAXBUF2=MAXBUF/2)

      integer   tinp, tout
      integer   naxis
      integer   dims, looplen, buflen
      integer   axnum(MAXNAX)
      integer   inpblc(MAXNAX), inptrc(MAXNAX)
      real      data(MAXBUF2)
      logical   mask(MAXBUF2)
      data      buflen / MAXBUF2 /

c Give the identifying message
      call output(version)
c Get inputs
      call inputs(   tinp, tout, dims, looplen, buflen,
     *                axnum, inpblc, inptrc, naxis)
c Do the real work
      call transpos(tinp, tout, dims, looplen, buflen,
     *                axnum, inpblc, inptrc, naxis, data, mask)
c Finish off
      call finish(tinp, tout, version)

      end

c***********************************************************************
c Get inputs
      subroutine inputs(tinp, tout, dims, looplen, buflen,
     *                   axnum, blc, trc, naxis)

      integer              tinp, tout
      integer              dims, looplen, buflen
      integer              axnum(*)
      integer              blc(*), trc(*)
      integer              naxis

      integer              MAXNAX
      parameter            (MAXNAX = 7)

      character*1024       inp, out

      character*(MAXNAX+1) goal
      integer              invaxnum(MAXNAX)

      integer              axlen(MAXNAX)
      double precision     inpaxlen(MAXNAX), outaxlen(MAXNAX)
      double precision     inpblc(MAXNAX),   inptrc(MAXNAX)
      double precision     outblc(MAXNAX),   outtrc(MAXNAX), outoff
      integer              viraxlen(MAXNAX)
      ptrdiff              vircubsz(MAXNAX)

      integer              MAXBOXES
      parameter            (MAXBOXES = 1024)
      integer              inpboxes(MAXBOXES)
      logical              keyprsnt

      integer              i, j
      character*1          itoaf
      character*8          keyw
      double precision     crpix(MAXNAX)

c Read keywords
      call keyini
      call keya('in',   inp,  ' ')
      call keya('out',  out,  ' ')
      call keya('goal', goal, 'x')
      call assertl(inp.ne.' ', 'Input file name is missing' )
      call assertl(out.ne.' ', 'Output file name is missing')

c Find reversals and check if axes are given only once
      call decdgoal(goal, axnum, invaxnum)

c Open input dataset to get axislengths and then get blc, trc
      naxis = MAXNAX
      call xyzopen( tinp, inp, 'old', naxis, axlen)
      call itof(axlen, inpaxlen, naxis)

      if (keyprsnt('region') .and. keyprsnt('box'))
     *    call bug('f', 'region= and box= are mutually exclusive')
      do i = 1, naxis
         keyw = 'crpix' // itoaf(i)
         call rdhdd(tinp, keyw, crpix(i), 1d0)
      enddo
      if (keyprsnt('region')) then
         call boxinput('region', inp, inpboxes, maxboxes)
         call boxset(  inpboxes, naxis, axlen, ' ')
         call boxinfo( inpboxes, naxis, blc, trc)
         call itof(blc, inpblc, naxis)
         call itof(trc, inptrc, naxis)
      else
         do i = 1, naxis
            call keyd('box', inpblc(i), 1.0-crpix(i))
            call keyd('box', inptrc(i), inpaxlen(i)-crpix(i))
            inpblc(i) = inpblc(i) + crpix(i)
            inptrc(i) = inptrc(i) + crpix(i)
         enddo
      endif

c Get blc and trc and axlen of output, then open
      do i = 1, naxis
         call keyd('frame', outblc(invaxnum(i)), inpblc(i)-crpix(i))
         call keyd('frame', outtrc(invaxnum(i)), inptrc(i)-crpix(i))
      enddo
      do i = 1, naxis
         outaxlen(i) = outtrc(i) - outblc(i) + 1.0
         outblc(i)   = outblc(i) + crpix(abs(axnum(i)))
         outtrc(i)   = outtrc(i) + crpix(abs(axnum(i)))
      enddo
      call ftoi(outaxlen, axlen, naxis)
      call xyzopen(tout, out, 'new', naxis, axlen)

c Copy the header
c Normally the blc and trc for headcp refer to the input cube. For
c imframe the output cube coordinates are needed.
      do i = 1, naxis
         blc(abs(axnum(i))) = nint(outblc(i))
         trc(abs(axnum(i))) = nint(outtrc(i))
      enddo
      call headcp(tinp, tout, naxis, axnum, blc, trc)


c Take input & output blc/trc to define region in output cube to do so
c that xyzsetup gets the proper area (i.e. relative to the output cube).
      do i = 1, naxis
         j = abs(axnum(i))
         outoff = outblc(i)
         inpblc(j) = max(inpblc(j), outblc(i))
         inptrc(j) = min(inptrc(j), outtrc(i))
         outblc(i) = inpblc(j) - outoff + 1.0
         outtrc(i) = inptrc(j) - outoff + 1.0
         call assertl(outtrc(i).ge.outblc(i),
     *                 'Input region lies outside output frame')
      enddo

c Set up subcubes: do planes or profiles for a little efficiency, so
c take first axis or first two axes for input and make it 'x' axis or
c 'xy' plane for output.
      if (int(outaxlen(1)*outaxlen(2)).gt.buflen) dims = 1
      if (int(outaxlen(1)*outaxlen(2)).le.buflen) dims = 2
      i=dims
      if (               axnum(1).lt.0) i = i+1
      if (dims.eq.2 .and. axnum(2).lt.0) i = i+1

      call ftoi(outblc, blc, naxis)
      call ftoi(outtrc, trc, naxis)
      if (dims.eq.1) call xyzsetup(tout,'x', blc,trc,viraxlen,vircubsz)
      if (dims.eq.2) call xyzsetup(tout,'xy',blc,trc,viraxlen,vircubsz)

      call ftoi(inpblc, blc, naxis)
      call ftoi(inptrc, trc, naxis)
      call xyzsetup(tinp, goal(1:i),       blc,trc,viraxlen,vircubsz)

c Number of loops to be done and number of data points returned
      looplen = vircubsz(naxis) / vircubsz(dims)
      buflen  = vircubsz(dims)

      call keyfin

      return
      end


c Routine used to decode goal keyword
      subroutine decdgoal(goal, axnum, invaxnum)

      character*(*)        goal
      integer              axnum(*)
      integer              invaxnum(*)

      integer              MAXNAX
      parameter            (MAXNAX = 7)

      integer              i, j, k, len1
      character*(MAXNAX+1) axnames
      logical              axisuse(MAXNAX)
      logical              reversal
      data                 axnames  / 'xyzabcd-' /
      data                 axisuse  / MAXNAX * .FALSE. /
      data                 reversal / .FALSE. /

      j = 1
      do i = 1, len1(goal)
         k = index(axnames, goal(i:i))
         call assertl(k.ne.0, 'Bad syntax for goal keyword')
         if (goal(i:i).eq.'-')
     *   then
            call assertl(.not.reversal, 'Bad syntax for goal keyword')
            reversal = .TRUE.
         else
            call assertl(.not.axisuse(k), 'Axis given more than once')
            axisuse(k) = .TRUE.
            axnum(j)   = k
            if (reversal) axnum(j) = -axnum(j)
            j = j + 1
            reversal = .FALSE.
         endif
      enddo
      k = len1(goal)+1
      do i = 1, MAXNAX
         if (.not.axisuse(i)) then
            axnum(j)  = i
            goal(k:k) = axnames(i:i)
            k = k + 1
            j = j + 1
         endif
      enddo
      do i = 1, MAXNAX
         invaxnum(abs(axnum(i))) = i
      enddo

      return
      end

c***********************************************************************
c Do the work

      subroutine transpos(tinp, tout, dims, looplen, buflen,
     *                      axnum, inpblc, inptrc, naxis, data, mask)

      integer   tinp, tout
      integer   dims, looplen, buflen
      integer   axnum(*)
      integer   inpblc(*), inptrc(*)
      integer   naxis
      real      data(*)
      logical   mask(*)

      include   'maxnax.h'

      integer   prpl
      integer   i, j, k
      integer   axlist(MAXNAX)
      integer   inpcoo(MAXNAX)
      logical   next

      do i = 1, naxis
         axlist(i) = i
      enddo
      do j = 1, dims
         do i = abs(axnum(j))+1, naxis
            axlist(i) = axlist(i) - 1
         enddo
      enddo
      do i = dims+1, naxis
         k = abs(axnum(i))
         j = axlist(k)
         if (axnum(k).gt.0) inpcoo(j) = inpblc(k)
         if (axnum(k).lt.0) inpcoo(j) = inptrc(k)
      enddo

c Loop over all first-axis output profiles.  Calculate input coordinates
c of corner of plane/profile, read it from input and write it to output.
c In the loop coordinates in the input cube are increased.  Coord
c axnum(2) is increased fastest.  If the trc is reached, coord axnum(3)
c is increased by 1, etc.

      do prpl = 1, looplen

         call xyzread(tinp, inpcoo, data, mask, buflen)
         if (dims.eq.1) call xyzprfwr(tout, prpl, data, mask, buflen)
         if (dims.eq.2) call xyzplnwr(tout, prpl, data, mask, buflen)

         i = dims + 1
         next = prpl.lt.looplen
         do while (i.le.naxis .and. next)
            k = abs(axnum(i))
            j = axlist(k)
            if (axnum(k).gt.0) then
               inpcoo(j) = inpcoo(j) + 1
               next = inpcoo(j).gt.inptrc(k)
               if (next) inpcoo(j) = inpblc(k)
            else
               inpcoo(j) = inpcoo(j) - 1
               next = inpcoo(j).lt.inpblc(k)
               if (next) inpcoo(j) = inptrc(k)
            endif
            i = i + 1
         enddo

      enddo

      return
      end



      subroutine finish(tinp, tout, version)

      integer       tinp, tout
      character*(*) version

      character*80  line

      call hisopen( tout, 'append')
      line = 'IMFRAME: ' // version
      call hiswrite(tout, line    )
      call hisinput(tout, 'IMFRAME')
      call hisclose(tout)
      call xyzclose(tinp)
      call xyzclose(tout)

      return
      end



      subroutine itof(iarr, farr, n)
      integer          iarr(*)
      double precision farr(*)
      integer          n,i
      do i = 1, n
         farr(i) = real(iarr(i))
      enddo
      return
      end
      subroutine ftoi(farr, iarr, n)
      double precision farr(*)
      integer          iarr(*)
      integer          n,i
      do i = 1, n
         iarr(i) = nint(farr(i))
      enddo
      return
      end

