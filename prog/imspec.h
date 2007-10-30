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
c      parameter    ( NAME = 'IMSTAT' )
c      character    plotopts*(*)
c      parameter    ( plotopts = 'mean,sum,rms,maximum,minimum' )
c      character    defplt*(*), idstr*(*)
c      parameter    ( defplt = 'rms', idstr = 'statistics' )
c      character    commonop*(*)
c      parameter    ( commonop =
c     *'tb,noheader,nolist,list,eformat,style,title,xmin,xmax,ymin,ymax')

c plot and options for IMSPEC
      parameter    ( NAME = 'IMSPEC' )
      character    plotopts*(*)
      parameter    ( plotopts = 'mean,sum,flux,pbcflux' )
      character    defplt*(*), idstr*(*)
      parameter    ( defplt = 'flux', idstr = 'spectrum' )
      character    commonop*(*)
      parameter    ( commonop =
     *'tb,noheader,nolist,list,eformat,hanning,boxcar,deriv,style,'//
     *'title,xmin,xmax,ymin,ymax' )

c common variables for IMSTAT and IMSPEC

      character    styles*(*)
      parameter    ( styles = 'connect,step,histo' )

      integer      NPLOTV, NPLOTR, NPLOTP
      parameter    ( NPLOTV = 9, NPLOTR = 10, NPLOTP = 5 )
      integer      plotvar(  NPLOTV )
      real         plotrnge( NPLOTR )
      character*80 plotpar(  NPLOTP )

c plotvar(1-9)
      integer      SEL, HEAD, LIST, EFMT, DUNIT, STYLE
      integer      DOSMOOTH, SMOWID, DERIV
      parameter    ( SEL=1, HEAD=2, LIST=3, EFMT=4, DUNIT=5, STYLE=6 )
      parameter    ( DOSMOOTH=7, SMOWID=8, DERIV=9 )

c values of plotvar(DUNIT)
      integer      ORIG, JANSKY, KELVIN
      parameter    ( ORIG=1, JANSKY=2, KELVIN=3 )
c values of plotvar(DOSMOOTH)
      integer      HANNING, BOXCAR
      parameter    ( HANNING=1, BOXCAR=2 )

c plotrng2(1-10)
      integer      XLOW, XUPP, YLOW, YUPP, FLXL, FLXU, FLYL, FLYU
      parameter    ( XLOW=1,  XUPP=2,  YLOW=3,  YUPP=4 )
      parameter    ( FLXL=5,  FLXU=6,  FLYL=7,  FLYU=8 )
      integer      XTITLE, YTITLE
      parameter    ( XTITLE=9, YTITLE=10 )

c plotpar(1-5)
      integer      XLABP, YLABP, INFOP, BOXP, TITLE
      parameter    ( XLABP=1, YLABP=2, INFOP=3, BOXP=4, TITLE=5 )

      double precision crval(MAXNAX), cdelt(MAXNAX), crpix(MAXNAX)
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
