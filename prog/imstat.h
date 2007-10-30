c To change IMSTAT into IMSPEC:
c 1) edit imstat.h:
c    Change which statements are commented out at beginning to:
c        change NAME variable into IMSPEC, change which plotopts are used,
c        change which commonop is used,
c 2) Change name to imspec.h
c 3) edit imstat.for:
c        remove the = in the c= directive for imstat and add it for imspec
c        change include 'imstat.h' to include 'imspec.h'
c 4) Change name to imspec.for

      include      'maxnax.h'
      include      'maxdim.h'
      include      'mirconst.h'
      integer      MAXBOXES
      parameter    ( MAXBOXES = 1024 )

      character*6  NAME

c plot and options for IMSTAT
      parameter    ( NAME = 'IMSTAT' )
      character*28 plotopts
      parameter    ( plotopts = 'mean,sum,rms,maximum,minimum' )
      character    defplt*3, idstr*10
      parameter    ( defplt = 'rms', idstr = 'statistics' )
      character*80 commonop
      parameter    ( commonop =
     *'tb,noheader,nolist,list,eformat,style,title,xmin,xmax,ymin,ymax')

c plot and options for IMSPEC
c      parameter    ( NAME = 'IMSPEC' )
c      character*21 plotopts
c      parameter    ( plotopts = 'mean,sum,flux,pbcflux' )
c      character    defplt*4, idstr*8
c      parameter    ( defplt = 'flux', idstr = 'spectrum' )
c      character*80 commonop
c      parameter    ( commonop =
c     *'tb,noheader,nolist,list,eformat,hanning,boxcar,deriv,style,title,
c     *xmin,xmax,ymin,ymax' )

c common variables for IMSTAT and IMSPEC

      character*20 styles
      parameter    ( styles = 'connect,step,histo' )

      integer      NPLOTV, NPLOTR, NPLOTP
      parameter    ( NPLOTV = 9, NPLOTR = 10, NPLOTP = 5 )
      integer      plotvar(  NPLOTV )
      real         plotrnge( NPLOTR )
      character*80 plotpar(  NPLOTP )

      integer      SEL, HEAD, LIST, EFMT, DUNIT, STYLE
      parameter    ( SEL=1, HEAD=2, LIST=3, EFMT=4, DUNIT=5, STYLE=6 )
      integer      ORIG, JANSKY, KELVIN
      parameter    ( ORIG=1, JANSKY=2, KELVIN=3 )
      integer      DOSMOOTH, SMOWID, DERIV
      parameter    ( DOSMOOTH=6, SMOWID=7, DERIV=8 )
      integer      HANNING, BOXCAR
      parameter    ( HANNING=1, BOXCAR=2 )

      integer      XLOW, XUPP, YLOW, YUPP, FLXL, FLXU, FLYL, FLYU
      parameter    ( XLOW=1,  XUPP=2,  YLOW=3,  YUPP=4 )
      parameter    ( FLXL=5,  FLXU=6,  FLYL=7,  FLYU=8 )
      integer      XTITLE, YTITLE
      parameter    ( XTITLE=9, YTITLE=10 )

      integer      XLABP, YLABP, INFOP, BOXP, TITLE
      parameter    ( XLABP=1, YLABP=2, INFOP=3, BOXP=4, TITLE=5 )

      integer      crpix(MAXNAX)
      double precision crval(MAXNAX), cdelt(MAXNAX)
      character*9  ctype(MAXNAX)

      integer      NSTATS
      parameter    ( NSTATS = 6 )
      common /VAR/ plotvar, plotrnge
      common /CRD/ crval, cdelt, crpix
      common /CHR/ plotpar, ctype

      integer      SUMBM, KPERJY, BEAMX, BEAMY
      parameter    ( SUMBM=1, KPERJY=2, BEAMX=3, BEAMY=4 )

      real         RIGHT, LEFT
      parameter    ( RIGHT=1.0, LEFT=0.0 )
      real         XOFF, BASE, YOFF, COFF, SC
      parameter    ( XOFF=0.02, BASE=2.3, YOFF=0.7, COFF=0.23, SC=0.7 )
      real         MAGICVAL
      parameter    ( MAGICVAL = -1.e30 )
