c= vblank - masks outlier pixels in velocity space
c& thw
c: image-analysis
c+
c vblank takes a cube that's already been masked and masks it further, 
c so that unmasked pixels that are surrounded by only masked pixels 
c along axis 1 will be masked.  The default is to mask if both adjacent 
c pixels are masked; one can increase the window size with keyword 'tol'.
c
c This can be useful to reduce noise in a datacube that has already 
c been masked with a smoothed cube and where the velocity field is 
c assumed to be smooth.  In this case axis 1 would be the velocity axis
c and spikes far from the channels of actual emission are masked.
c
c NOTE: Because flagging is only implemented for planes, the velocity
c axis must be axis 1.  Use REORDER first to make this so.
c	reorder in=file1 out=file2 mode='312'
c 	vblank in=file2
c	reorder in=file2 out=file3 mode='231'
c
c< in
c
c@ tol
c How far (in pixels) the nearest unmasked pixel can be for the given 
c pixel to stay unmasked.  Default is 1.
c
c--
c
c***********************************************************************

c  History
c     thw 04nov98 created
c     pjt 13jul00 readied for miriad release, call keyfin etc.

c************************************************************************

      program vblank
      implicit none

      character*50 version
      parameter    ( version = 'vblank: version 13-jul-00' )

      include	      	'maxdim.h'
      include           'maxnax.h'
      integer           i, naxis, ivaxis, inunit
      integer           axlen(MAXNAX), blc(MAXNAX), trc(MAXNAX)
      character 	mesg*80, inp*80, vaxis*1
      integer 		tol, nflag

c Announce
      call output(version)

c Read inputs
      call keyini
      call keyf( 'in', inp, ' ' )
      call keyi( 'tol', tol, 1 )
      call keyfin
c Some input error checking
      call assertl( inp.ne.' ', 'You must specify an input file' )

c Open and get dimensions of input dataset
      naxis = MAXNAX
      call xyopen( inunit, inp, 'old', naxis, axlen )

c Get an input region from the user (not implemented yet)
c      call boxinput( 'region', name, boxes, MAXBOXES )
c      call boxset(   boxes, naxis, axlen, ' ' )
c      call boxinfo(  boxes, naxis, blc, trc )
      do i = 1, naxis
         blc(i) = 1
         trc(i) = axlen(i)
      enddo

c Find velocity axis.
      call fndaxnum( inunit, 'freq', vaxis, ivaxis )
      call assertl( vaxis.ne.' ',
     *              'Velocity axis not found in dataset' )
      call assertl( ivaxis.eq.1,
     *          'Velocity axis must be axis 1 (use REORDER)' )

c Do the masking.
      call domask( inunit, naxis, blc, trc, tol, nflag )

c Output number of pixels flagged.
      write (mesg,'( i7,'' pixels flagged'' )') nflag
      call output(mesg)

c Update history file and close.
      call hisopen(inunit,'append')
      call hiswrite(inunit,'VBLANK: '//version)
      call hisinput(inunit,'VBLANK')
      call hiswrite(inunit,'VBLANK: '//mesg)
      call hisclose(inunit)
      call xyclose(inunit)

      end


c***********************************************************************

      subroutine domask( unit, naxis, blc, trc, tol, nflag )
      implicit none

      include 'maxdim.h'
      integer unit, naxis, blc(naxis), trc(naxis), tol, nflag
      real buf(maxdim)
      logical flags(maxdim)

      integer i, j, k, i0, i1, iscan, nscan

      nflag = 0
      do k = blc(3),trc(3)
         call xysetpl(unit,1,k)
         do j = blc(2),trc(2)
            call xyread(unit,j,buf)
            call xyflgrd(unit,j,flags)
	    do i = blc(1),trc(1)
	       if (flags(i)) then
		  i0 = max(i-tol,1)
                  i1 = min(i+tol,trc(1))
                  nscan=0
	          do iscan = i0, i1
		     if (flags(iscan)) nscan=nscan+1
                  enddo
                  if (nscan .eq. 1) then
		     flags(i) = .false.
		     nflag = nflag + 1
                  endif
               endif
            enddo
            call xyflgwr(unit,j,flags)
         enddo
      enddo

      return
      end
