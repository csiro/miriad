      program mirmax
        
c= mirmax -- Print Miriad's built in size limits
c& mhw
c+       
c       Simple program to return maximum sizes used on your platform.
c
c$Id: mirmax.for,v 1.2 2013/05/23 02:15:56 wie017 Exp $
c--
c History:
c   mhw  23may13 Original version
c
c------------------------------------------------------------------------
      include 'maxdim.h'
      
      character version*80
      character versan*80, itoaf*10
      external versan, itoaf
c------------------------------------------------------------------------
      version = versan('mirmax',
     *                 '$Revision: 1.2 $',
     *                 '$Date: 2013/05/23 02:15:56 $')
      
      call output('Miriad''s compiled in limits from maxdim.h')
      call output(' Max Buffer size                 = '//itoaf(MAXBUF))
      call output(' Max Image dimemsion             = '//itoaf(MAXDIM)) 
      call output(' Max number of antennas          = '//itoaf(MAXANT))
      call output(' Max number of baselines         = '//itoaf(MAXBASE))
      call output(' Max number of channels          = '//itoaf(MAXCHAN))
      call output(' Max number of spectral windows  = '//itoaf(MAXWIN))
      call output(' Max number of wideband channels = '//itoaf(MAXWIDE))
      call output(' Max number of frequency bins    = '//itoaf(MAXFBIN))
      call output(' ')
      call output(' To increase these limits, edit maxdim.h and ')
      call output(' maxdimc.h and rebuild miriad from source.')
      call output(' Note that there are architecture specific versions')
      call output(' of the maxdim include files.')
       end
