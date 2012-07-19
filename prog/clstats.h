c-------------------------------------------------------------
c  old name: clstats.h
c  common stat variables
c-------------------------------------------------------------
      include 'maxdim.h'
      integer ncl
      parameter(ncl=1000)


      integer nmin
      real dist,disterr,xfact,xfacterr
      real meanmol,kpjy,rms
      character*80 file
      common /keyinn/ nmin,dist,disterr,xfact,xfacterr,
     *               meanmol,kpjy,rms
      common /keyinc/ file

      real x0,y0,delx,dely,delv
      real beamx,beamy
      common /stuff/ x0,y0,delx,dely,delv,beamx,beamy


      integer xpeak(ncl),ypeak(ncl),vpeak(ncl)
      real xsum(ncl),xsq(ncl)
      real ysum(ncl),ysq(ncl)
      real vsum(ncl),vsq(ncl)
      real tsum(ncl)
      integer npix(ncl),npixarea(ncl)
      integer clump(ncl)
      logical edge(ncl)
      common /morestuff/ xpeak,ypeak,vpeak,xsum,xsq,ysum,ysq,
     *                   vsum,vsq,tsum,npix,npixarea,
     *                   clump,edge
      
c-------------------------------------------------------------
c-------------------------------------------------------------
c  old name: header.h
c  common header variables for clumpstats
c	19-may-98  pjt	removed never used keyw() array
c-------------------------------------------------------------
      integer lin1,lin2,lout
      common /files/ lin1,lin2,lout

      integer start,naxis
      real dt,rmsnoi
      common /keywords/ start,dt,naxis,rmsnoi

      character*8 ctype(4)
      common /cheadvar/ ctype

      integer nsize(4),nx,ny,nv
      real restfreq
      common /irheadvar/ nsize,nx,ny,nv,restfreq

      double precision crpix(4),crval(4),cdelt(4),bmaj,bmin,bpa
      common /headvar/ crpix,crval,cdelt,bmaj,bmin,bpa

      real datamax,datamin
      common /statis/ datamax,datamin

c-------------------------------------------------------------
