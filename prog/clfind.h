c-----------------------------------------------------------------------
c     clfind.h
c     include file for the clump finding program
c-----------------------------------------------------------------------
c Allow for a 128x128x128 cube
      integer maxbuf,maxdim
      parameter (maxbuf = 128*128*128, maxdim = 400)

c maxlvl = number of contour levels
c maxreg = number of regions/level
c maxpix = number of pixels/level (level>2)
c maxclp = total number of clumps
c maxpix1 = number of pixels in level 1 (dT < T < 2dT)
      integer maxlvl,maxreg,maxpix,maxclp,maxpix1
      parameter (maxlvl = 50, maxreg = 250, maxclp = 250)
      parameter (maxpix = 25000, maxpix1 = 100000)

c #pixels/level
      integer npix(maxlvl)

c #regions/level
      integer nregions(maxlvl)

c merge flag
      logical merge(maxreg)

c tree structure
      logical tree(maxlvl,maxreg,maxclp)

      common /ianalyz/ npix,nregions
      common /lanalyz/ tree,merge

      integer clump(maxclp,4)
      common /clumps/ clump

      integer nlevels,badcl
      common /parms/ nlevels,badcl

c-----------------------------------------------------------------------
c     header.h
c     header variables for file in clumpfind
c-----------------------------------------------------------------------
      integer lin,lout
      common /files/ lin,lout

      integer p1,p3
      real p2
      common /parms/ p1,p2,p3

      integer nx,ny,nz
      real dx,dy,dv
      common /headvarn/ nx,ny,nz,dx,dy,dv
