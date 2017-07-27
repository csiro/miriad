      program mospsf

c= mospsf - Determine approximate PSF of a mosaic experiment
c& rjs
c: map combination
c+
c       MOSPSF is a MIRIAD task that determines the point spread
c       function for a linear mosaic of dirty images (as produced by
c       INVERT).
c
c       Strictly speaking, the PSF varies with position and frequency.
c       However if the pointing grid is fairly complete and the
c       individual synthesised beam patterns are similar, the PSF is
c       reasonably independent of position.  It is also usually a good
c       approximation that it is independent of frequency.
c@ beam
c       This gives the name of the input beam-cube (as produced by
c       INVERT).  No default.
c@ out
c       The name of the output point-spread function.  It will be the
c       same size as the input.
c@ radec
c       The RA and DEC (either in the form hh:mm:ss,dd:mm:ss or decimal
c       hours and degrees) at which to compute the point-spread
c       function.  The default is the reference RA,DEC of the beam-cube.
c@ freq
c       The frequency (in GHz) at which to compute the point-spread
c       function.  The default is the reference frequency of the beam-
c       cube.  The point-spread function is reasonably independent of
c       frequency for most spectral line observations.
c
c$Id: mospsf.for,v 1.7 2017/07/27 05:46:46 wie017 Exp $
c--
c
c  History:
c    rjs  25oct93 Adapted from LINMOS.
c    nebk 22aug94 Adapt to GETFREQ error status change
c    rjs  26oct94 Complete rewrite to cope with the new INVERT.
c    rjs  24oct95 MOSPSF should not copy bmaj,bmin and bpa.
c    pjt  16aug2014   Deal with larger buffers (use memallox/memfrex)
c
c  Bugs:
c
c  Program parameters:
c    maxIn      Maximum number of input files.
c    maxlen     Size of buffer to hold all the input file names.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mem.h'

      logical  dofreq, doradec
      integer  i, naxis, npnt, nsize(MAXNAX), nx, ny, tIn, tOut
      double precision dec, freq, ra, x(3)
      character beam*64, coin*12, out*64, version*72
      ptrdiff pIn,pOut,np1,np2

      logical   keyprsnt
      character versan*72
      external  keyprsnt, versan
c-----------------------------------------------------------------------
      version = versan('mospsf',
     *                 '$Revision: 1.7 $',
     *                 '$Date: 2017/07/27 05:46:46 $')

c     Get the input parameters.
      call keyini
      call keya('beam',beam,' ')
      if (beam.eq.' ') call bug('f','Input beam must be given')
      call keya('out',out,' ')
      if (out.eq.' ') call bug('f','Output PSF must be given')
      doradec = .not.keyprsnt('radec')
      call keyt('radec',ra,'hms',0d0)
      call keyt('radec',dec,'dms',0d0)
      dofreq = .not.keyprsnt('freq')
      call keyd('freq',freq,0d0)
      call keyfin

c     Open the input and the output.
      call xyopen(tIn,beam,'old',3,nsize)
      call coInit(tIn)
      call mosLoad(tIn,npnt)
      nx = nsize(1)
      ny = nsize(2)
      if (npnt.ne.nsize(3))
     *  call bug('f','Inconsistent number of pointings')

c     Set the position at which to compute the PSF.
      x(1) = ra
      x(2) = dec
      x(3) = freq
      if (doradec .and. dofreq) then
        coin = 'op/op/op'
      else if (doradec) then
        coin = 'op/op/aw'
      else if (dofreq) then
        coin = 'aw/aw/op'
      else
        coin = 'aw/aw/aw'
      endif

c     Allocate memory and load the input PSF data.
      np2 = nx
      np2 = np2*ny
      np1 = np2*npnt
      call memAlloc(pIn,np1,'r')
      call memAlloc(pOut,np2,'r')

c     Read the input data.
      call GetDat(tIn,memr(pIn),nx,ny,npnt)

c     Do the real work.
      call mosPnt(tIn,coin,x,memr(pIn),memr(pOut),nx,ny,npnt)

c     Create the output dataset.
      call rdhdi(tIn,'naxis',naxis,0)
      naxis = min(naxis-1,MAXNAX)
      do i = 3, naxis
        nsize(i) = 1
      enddo
      call xyopen(tOut,out,'new',naxis,nsize)
      call mkHead(tIn,tOut,version)

c     Write the output.
      call PutDat(tOut,memr(pOut),nx,ny)

c     All said and done. Close up.
      call xyclose(tOut)
      call MemFrex(pOut,np2,'r')
      call MemFrex(pIn,np1,'r')
      call xyclose(tIn)

      end

c***********************************************************************

      subroutine mkHead(tIn, tOut, version)

      integer   tIn, tOut
      character version*72
c-----------------------------------------------------------------------
c  Write the header for the output image.
c-----------------------------------------------------------------------
      character itoaf*2
      integer iostat
      external  itoaf
c-----------------------------------------------------------------------
c     Make a verbatim copy of the input header.
      call headcp(tIn, tOut, 0, 0, 0, 0)

c     Delete header parameters that shouldn't have been copied.
      call hdelete(tOut, 'bmaj',iostat)
      call hdelete(tOut, 'bmin',iostat)
      call hdelete(tOut, 'bpa',iostat)

c     Copy the mask (not done by headcp).
      call hdcopy(tIn, tOut, 'mask')

c     Handle the history.
      call hisopen (tOut, 'append')
      call hiswrite(tOut, 'MOSPSF: Miriad ' // version)
      call hisinput(tOut, 'MOSPSF')
      call hisclose(tOut)

      end

c***********************************************************************

      subroutine GetDat(tIn,In,nx,ny,npnt)

      integer tIn,nx,ny,npnt
      real In(nx,ny,npnt)
c-----------------------------------------------------------------------
c  Read the input data.
c-----------------------------------------------------------------------
      integer j,k
c-----------------------------------------------------------------------
      do k = 1, npnt
        call xysetpl(tIn,1,k)
        do j = 1, ny
          call xyread(tIn,j,In(1,j,k))
        enddo
      enddo

      end
c***********************************************************************
      subroutine PutDat(tOut,Out,nx,ny)

      integer tOut,nx,ny
      real Out(nx,ny)
c-----------------------------------------------------------------------
c  Write the output data.
c-----------------------------------------------------------------------
      integer j
c-----------------------------------------------------------------------
      do j = 1, ny
        call xywrite(tOut,j,Out(1,j))
      enddo

      end
