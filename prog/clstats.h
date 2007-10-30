c-------------------------------------------------------------
c  clstats.h
c  common stat variables
c-------------------------------------------------------------
      integer maxbuf,maxdim,ncl
      parameter(maxbuf=4194304,maxdim=400,ncl=300)


      integer nmin
      real dist,disterr,xfact,xfacterr
      real meanmol,kpjy,rms
      character*80 file
      common /keyin/ nmin,dist,disterr,xfact,xfacterr,
     *               meanmol,kpjy,rms,file

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
c  header.h
c  common header variables for clumpstats
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

      integer nkeys
      parameter(nkeys=40)
      character keyw(nkeys)*8
      data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *    'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ',
     *    'crpix1  ','crpix2  ','crpix3  ','crpix4  ',
     *    'crval1  ','crval2  ','crval3  ','crval4  ',
     *    'ctype1  ','ctype2  ','ctype3  ','ctype4  ',
     *    'date-obs','epoch   ','history ','instrume','niters  ',
     *    'object  ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *    'restfreq','telescop','vobs    ','xshift  ','yshift  ',
     *    'ltype   ','lstart  ','lwidth  ','lstep   ','btype   '/
c-------------------------------------------------------------
