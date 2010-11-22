c= zapchan - replace range of channels with average of adjacent channels.
c& thw
c: image-analysis
c+
c zapchan reads in a datacube and replaces selected "bad" channels with
c the average of the two adjacent channels.  Needless to say, it will
c not work on edge channels or consecutive channels.
c
c@ in
c Name of input datacube.  No default.
c
c@ out
c Name of output datacube to create.  No default.
c
c@ chans
c Integer list of channels to zap.  You cannot zap edge channels or
c consecutive channels.  Maximum 32.  No default.
c
c--
c
c***********************************************************************
c  History
c     thw 27sep99 created
c     pjt 13jul00 readied for miriad release, out of the BimaSong 'bin'
c                 called keyfin and moved error checking down a bit
c***********************************************************************

      program zapchan

      character*50 version
      parameter    (version = 'zapchan: version 13-jul-00')

      include           'maxdim.h'
      include           'maxnax.h'
      include           'mem.h'
      integer           i, count, naxis, inunit, outunit
      integer           BADMAX
      parameter         (BADMAX=32)
      integer           badchan(BADMAX), nchans, k, next, ndata
      integer           axlen(MAXNAX), blc(MAXNAX), trc(MAXNAX)
      integer           virax(MAXNAX), vircsize(MAXNAX)
      character         inp*80, out*80
      integer           dat1, dat2, msk

c Announce
      call output(version)

c Read user inputs
      call keyini
      call keyf('in', inp, ' ')
      call keyf('out', out, ' ')
      call mkeyi('chans', badchan, BADMAX, nchans)
      call keyfin

c Basic error checking
      call assertl(inp.ne.' ', 'You must specify an input [in=]')
      call assertl(out.ne.' ', 'You must specify an output [out=]')
      call assertl(nchans.gt.0, 'You must specify channels [chans=]')

      call sorti(badchan, nchans)

c Open and get dimensions of input dataset
      naxis = MAXNAX
      call xyzopen(inunit, inp, 'old', naxis, axlen)
      call rdhdi(inunit, 'naxis', naxis, 0)

c Open output dataset with same dimensions
      call xyzopen(outunit, out, 'new', naxis, axlen)
      call headcp(inunit, outunit, 0, 0, 0, 0)

c Reject edge or consecutive channels
      do count = 1, nchans
         if (count.eq.1) then
             if (badchan(count).eq.1) then
                 call bug('f', 'First channel cannot be zapped')
             endif
         else
             if (badchan(count).eq.badchan(count-1)+1) then
                call bug('f', 'Consecutive channels cannot be zapped')
             endif
         endif
         if (badchan(count).eq.axlen(3)) then
             call bug('f', 'Last channel cannot be zapped')
         endif
         if (badchan(count).gt.axlen(3) .or. badchan(count).lt.1) then
             call bug('f', 'Invalid channel number specified')
         endif
      enddo

c Allocate memory for data
c Use memr(dat1) to refer to 1st array, memr(dat2) to refer to 2nd, etc.
      call memalloc(dat1, axlen(1)*axlen(2), 'r')
      call memalloc(dat2, axlen(1)*axlen(2), 'r')
      call memalloc(msk, axlen(1)*axlen(2), 'l')

c Set up array dimensions
      do i = 1, naxis
         blc(i) = 1
         trc(i) = axlen(i)
      enddo
      call xyzsetup(inunit,  'xy', blc, trc, virax, vircsize)
      call xyzsetup(outunit, 'xy', blc, trc, virax, vircsize)

c Process the datacube
      next = 1
      do k = blc(3), trc(3)
         call xyzplnrd(inunit, k, memr(dat1), meml(msk), ndata)
         if (next.le.nchans) then
            if (k.lt.(badchan(next)-1)) then
               call xyzplnwr(outunit, k, memr(dat1), meml(msk), ndata)
            else if (k.eq.(badchan(next)-1)) then
               do i = 1, ndata
                 memr(dat2+i-1) = memr(dat1+i-1)
               enddo
               call xyzplnwr(outunit, k, memr(dat1), meml(msk), ndata)
            else if (k.eq.(badchan(next)+1)) then
               do i = 1, ndata
                 memr(dat2+i-1) = (memr(dat1+i-1) + memr(dat2+i-1))/2.0
               enddo
c We replace the mask of the bad channel by the mask of the following channel
               call xyzplnwr(outunit, k-1, memr(dat2), meml(msk), ndata)
               call xyzplnwr(outunit, k, memr(dat1), meml(msk), ndata)
               write(*,*) 'Finished processing plane ', k-1
               next = next + 1
               if (k.eq.(badchan(next)-1)) then
                  do i = 1, ndata
                     memr(dat2+i-1) = memr(dat1+i-1)
                  enddo
               endif
            endif
         else
            call xyzplnwr(outunit, k, memr(dat1), meml(msk), ndata)
         endif ! (next.le.nchans)
      enddo

c Update history file and close.
      call hisopen(outunit,'append')
      call hiswrite(outunit, 'ZAPCHAN: '//version)
      call hisinput(outunit, 'ZAPCHAN')
      call hisclose(outunit)
      call xyzclose(inunit)
      call xyzclose(outunit)

c Free allocated memory
      call memfree(dat1, axlen(1)*axlen(2), 'r')
      call memfree(dat2, axlen(1)*axlen(2), 'r')
      call memfree(msk,  axlen(1)*axlen(2), 'l')

      end


