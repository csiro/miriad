      character*80 plotopts
      parameter    ( plotopts =
     *'nbin,binsize,cumulative,logarithmic,nogauss' )

      character*60 commonop
      parameter    ( commonop =
     *'noheader,nolist,list,style,title,xmin,xmax,ymin,ymax' )

      character*20 styles
      parameter    ( styles   = 'connect,step,histo' )

      integer      NPLOTV, NPLOTR, NPLOTP
      parameter    ( NPLOTV = 6, NPLOTR = 10, NPLOTP = 5 )

      integer      SEL, HEAD, LIST, HANWD, STYLE, DUNIT
      parameter    ( SEL=1, HEAD=2, LIST=3, HANWD=4, STYLE=5, DUNIT=6 )

      integer      XLOW, XUPP, YLOW, YUPP, FLXL, FLXU, FLYL, FLYU
      parameter    ( XLOW=1,  XUPP=2,  YLOW=3,  YUPP=4 )
      parameter    ( FLXL=5,  FLXU=6,  FLYL=7,  FLYU=8 )
      integer      XTITLE, YTITLE
      parameter    ( XTITLE=9, YTITLE=10 )

      integer      XLABP, YLABP, INFOP, BOXP, TITLE
      parameter    ( XLABP=1, YLABP=2, INFOP=3, BOXP=4, TITLE=5 )

      integer      plotvar(  NPLOTV )
      real         plotrnge( NPLOTR )
      character*80 plotpar(  NPLOTP )

      integer      NHISTP
      integer      FLAG, VALUE
      integer      NBINP, BINSP, CUMUL, LOGAR, GAUCRV
      parameter    ( NHISTP = 5 )
      parameter    ( FLAG=1, VALUE=2 )
      parameter    ( NBINP=1, BINSP=2, CUMUL=3, LOGAR=4, GAUCRV=5 )
      real         histpar(2,NHISTP)

      integer      NHVAR
      parameter    ( NHVAR = 6 )
      integer      NPTS, MEANP, RMSP, MEDIANP, MAXV, MINV
      parameter    ( NPTS=1, MEANP=2, RMSP=3, MEDIANP=4, MAXV=5,MINV=6 )
      real         histvar(NHVAR)

      include      'maxnax.h'
      double precision crval(MAXNAX), cdelt(MAXNAX)
      character*9  ctype(MAXNAX)
      integer      posmin(MAXNAX), posmax(MAXNAX)

      common /VAR/ plotvar, plotrnge
      common /CHR/ plotpar, ctype
      common /HST/ histpar, histvar
      common /CRD/ crval, cdelt, posmin, posmax

      real         RIGHT, LEFT
      parameter    ( RIGHT=1.0, LEFT=0.0 )
      real         XOFF, BASE, YOFF, COFF, SC
      parameter    ( XOFF=0.01, BASE=2.3, YOFF=0.7, COFF=0.30, SC=0.7 )
      real         MAGICVAL
      parameter    ( MAGICVAL = -1.e30 )
