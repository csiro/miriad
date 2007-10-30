c= hanning - hanning smoothing
c& rjs
c: map combination
c+
c Hanning does a hanning smooth on the velocity axis of miriad
c datasets. It figures out which axis this is from the header. If not
c given in the header it assumes that it is the z-axis.
c The width of the hanning smooth is determined by the keyword width.
c If input data is masked, it is set to zero before smoothing is done.
c
c< in
c< region
c  NB the mask(file) option is not applied, set the mask of the input
c  file.
c< out
c@ width
c The number of channels of the hanning smooth. Must be an odd number.
c Default value is 3.
c--
c
c   History:
c
c    bpw  10jul91  Original version
c    bpw  04aug91  Add default to document
c    bpw  27mar91  Changed assert into assertl
c    bpw  15dec92  Adapt for changed fndaxnum
c    bpw   2mar93  Masked data set to zero
c
c------------------------------------------------------------------------
       program hanning

       character*80 version
       parameter ( version = 'hanning: version 1.0 2-mar-93' )

       include      'maxnax.h'

       integer      tinp, tout
       integer      nprofiles, nchan
       integer      width

       call output( version )
       call inputs(  tinp, tout, nprofiles,nchan, width )
       call hsmooth( tinp, tout, nprofiles,nchan, width )
       call finish(  tinp, tout, version )

       end




       subroutine inputs( tinp, tout, nprofiles, nchan, width )

       include      'maxnax.h'

       integer      tinp, tout
       integer      nprofiles, nchan
       integer      width

       character*1024 inp, out
       integer      MAXBOXES
       parameter    ( MAXBOXES = 1024 )
       integer      boxes(MAXBOXES)

       integer      naxis
       integer      axlen(MAXNAX), axnum(MAXNAX)
       integer      iblc(MAXNAX),  itrc(MAXNAX)
       integer      oblc(MAXNAX),  otrc(MAXNAX)
       integer      viraxlen(MAXNAX), vircsz(MAXNAX)
       integer      i

       character    velaxis
       integer      velaxnr

       call keyini

       call keyf( 'in',  inp, ' ' )
       call keyf( 'out', out, ' ' )
       call assertl( inp.ne.' ', 'Input file name is missing' )
       call assertl( out.ne.' ', 'Output file name is missing' )

       naxis = MAXNAX
       call xyzopen( tinp, inp, 'old', naxis, axlen )

       call boxinput( 'region', inp, boxes, MAXBOXES )
       call boxset(   boxes, naxis, axlen, ' ' )
       call boxinfo(  boxes, naxis, iblc, itrc )

       call keyi( 'width', width, 3 )
       call assertl( (width/2)*2.ne.width, 'Width must be odd number' )

       call keyfin

       velaxis = 'z'
       call fndaxnum( tinp, 'freq', velaxis, velaxnr )

       call xyzsetup( tinp, velaxis, iblc, itrc, viraxlen, vircsz )
       nprofiles = vircsz(naxis) / vircsz(1)
       nchan     = viraxlen(1)

       do i = 1, naxis
          axnum(i) = i
          axlen(i) = itrc(i) - iblc(i) + 1
          oblc(i)  = 1
          otrc(i)  = axlen(i)
       enddo
       call xyzopen(  tout, out, 'new', naxis, axlen )
       call headcopy( tinp, tout, axnum, naxis, iblc, itrc )
       call xyzsetup( tout, velaxis, oblc, otrc, viraxlen, vircsz )

       return
       end





       subroutine hsmooth( tinp, tout, nprofiles, nchan, width )

       integer       tinp, tout
       integer       nprofiles, nchan
       integer       width

       include       'maxdim.h'
       real          data(MAXDIM)
       logical       mask(MAXDIM)

       integer       MAXWIDTH
       parameter     ( MAXWIDTH = 7 )
       real          coeffs( MAXWIDTH*2+1 ), work( MAXWIDTH*2+1 )
       integer       i, j

       call hcoeffs( width, coeffs )
       do i = 1, nprofiles
          call xyzprfrd( tinp, i, data, mask, nchan )
          do j = 1, nchan
            if(.not.mask(j)) data(j)=0.
          enddo
          call hannsm( width, coeffs, nchan, data, work )
          call xyzprfwr( tout, i, data, mask, nchan )
       enddo

       return
       end





       subroutine finish( tinp, tout, version )

       integer       tinp, tout
       character*(*) version

       character*80  line
       
       call hisopen(  tout, 'append' )
       line = 'HANNING: ' // version
       call hisinput( tout, 'HANNING' )
       call hisclose( tout )
       call xyzclose( tinp )
       call xyzclose( tout )

       return
       end
