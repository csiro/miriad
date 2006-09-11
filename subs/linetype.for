c* Linetype -- Read standard linetype keyword and transfer information to uvio
c& bpw
c: text-i/o, utilities
c+
c     
      subroutine linetype( unit, line, type )

      integer       unit
      real          line(4)
      character*(*) type
c
c Returns the values of the linetype keyword in an array and also
c does the call to uvset to tell uvio which linetype was selected.
c It sets the default linetype to 'channel,0,1,1,width', i.e. all
c channels (nchan=0), and a step equal to the width.
c Furthermore some checking on the consistency of the input is done.
c
c Input:
c    unit        the logical unit number of the visibility file
c
c Output:
c    type        the linetype ('channel', 'wide' etc)
c    line        condensed form for: nchan,start,width,step.
c--
 
      integer       nchan
      real          start, width, step

      call keya  ( 'line', type, 'channel' )

      call keyi  ( 'line', nchan, 0 )
      line(1) = real(nchan)

      call keyr  ( 'line', start, 1. )
      if( start.le.0. ) call bug('f','Channel numbers <0 do not exist' )

      call keyr  ( 'line', width, 1. )
      if( width.le.0. ) call bug('f','Channel width must be >0')

      call keyr  ( 'line', step, width )
      if( step.le.0. ) call bug('f','Step between channels must be >0')

      call uvset ( unit, 'data', type, nchan, start, width, step )

      line(2) = start
      line(3) = width
      line(4) = step

      return
      end
