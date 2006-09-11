      program clfind
      implicit none
c --------------------------------------------------------------------
c
c= clfind - Find clumps by tracing contours (in 3D)
c& pjt
c: utility
c
c+
c  CLFIND is a MIRIAD task which finds the clumps in a 3-d cube,
c  by tracing contours, proceeding to lower levels and following
c  merging of said clumps. For full description/testing/application in
c    Williams, J.P, de Geus, E.J. & Blitz, L., ApJ, 428, 693. (1994)
c  Also see
c    http://www.ifa.hawaii.edu/~jpw/research/clfind/clfind.html
c  for a general description and access to a postscript memo.
c
c
c@ in
c     The input image containing the data cube.  (No default)
c     The output clump assignment file (also a miriad datacube
c     with the same dimensions as the input) has the name "in".cf
c
c@ dt
c     The contour increment (no default)
c
c@ start
c     The starting contour level (default=1)
c
c@ nmin
c     Minimum number of pixels in each clump.
c     Any clumps with fewer pixels are rejected (default=4).
c
c--
c       this used to be a parameter, but - as discussed in the paper -
c       we don't want users to mess with this. (:-)
c@ naxis
c     Maximum number of axes (1,2,3) for which a
c     pixel can be one resolution element off from
c     another and still be considered a neighbour (default=3)
c
c
c  History:
c   DarkAges jpw creation
c   ??/??/92 edg converted to MIRIAD format
c   01/28/93 jpw created subroutine extendclp.for
c                which divides up merged clumps
c                by method of sucessive neighbours
c   02/24/93 jpw Created parameter naxis to replace 
c                previous "Euclidian" distance parameter d1
c   07/19/94 jpw dynamic memory (memalloc, etc)
c   09/20/96 jpw/pjt   Formal miriad version (finally)
c   18-may-98 rjs/pjt  Moved over to a single-source file
c   13-jul-98 pjt linux/g77 cleanup, and fixed CntLevs counting bug
c   09-nov-05 rjs Update link in documentation.
c
c  Note:
c   This program comes with a testsuite dataset, which you should run
c   clfind on after you've made any importants changes, and compare the 
c   output with. We expect this dataset and the regression output to 
c   become available in $MIR/test, whenever this feature has been merged 
c   into the public release of MIRIAD.
c
c  ToDo:
c      -  this code uses some equivalences and other old fortran habits
c         that we are not supposed to use in miriad code, hence this
c         code won't pass FLINT.
c
c --------------------------------------------------------------------
      character version*(*)
      parameter(version='version 1.0 13-jul-98' )
      include 'clfind.h'

      character*40 filein,filecf
      character*80 line1,line2
      character xtension*3
      integer len1
      integer nclump,ncl,nstop
      integer ngy,nmin
      integer nlevs,npx1,npx2
      integer nsize(3)
      real beamx,beamy


c.....dynamic memory allocations
      integer It,Ia,Ipos,Ipos1,Ireg
      integer assign(maxbuf)
      integer pos1(maxbuf/10),pos(maxbuf),reg(maxbuf)
      real data(maxbuf)
      common assign
      equivalence(assign,pos1,pos,reg,data)


      call output('CLFIND '//version)

c.....Get the parameters from the user.
      call keyini
      call keya('in',filein,' ')
      call keyi('start',p1,1)
      call keyr('dt',p2,0.0)
c      call keyi('naxis',p3,3)
      p3 = 3
      call keyi('nmin',nmin,4)
      call keyfin

      badcl=16796

      if(filein.eq.' ')call bug('f','Input file name missing')
      if(p2.eq.0.0)call bug('f','No contour interval specified')
      if(p3.lt.1 .or. p3.gt.3) call bug('f','Bad naxis')

      call output('Data file: '//filein)
      call xyopen(lin,filein,'old',3,nsize)
      nx = nsize(1)
      ny = nsize(2)
      nz = nsize(3)
      if(nx.gt.maxdim) call bug('f','Image too big in x')
      if(ny.gt.maxdim) call bug('f','Image too big in y')
      if(nz.gt.maxdim) call bug('f','Image too big in v')
      if(nx*ny*nz.gt.maxbuf) call bug('f','Image too big')
      call rdhd(lin)

c.....Calculate the beam size in x and y
      if(bmaj .lt. 1.0e-7) then
        call bug('i','Beam size unspecified')
        beamx = 1.0
        beamy = 1.0
      else
        if(bpa .lt. 45.0 .or. bpa .gt. 135.0) then
          beamx = abs(real(bmin/cdelt(1)))
          beamy = real(bmaj/cdelt(2))
        else
            write (*,*) 'beam=', bmaj,bmin,cdelt(1),cdelt(2)
	      beamx = abs(real(bmaj/cdelt(1)))
	      beamy = real(bmin/cdelt(2))
        endif
        print 1000, beamx,beamy
 1000   format('The beam size is: ',f4.2,' by ',f4.2,' pixels.')
c.......But we use beam radius to connect up pixels
        dx=0.5*beamx
        dy=0.5*beamy
      endif
      if(dx.lt.1.0) then
        if(dx.lt.0.9) call bug('w','Map is not fully sampled in x')
        dx=1.0
      endif
      if(dy.lt.1.0) then
        if(dy.lt.0.9) call bug('w','Map is not fully sampled in y')
        dy=1.0
      endif
      print 1001, dx,dy
 1001 format
     * ('Dist to neighbours is dx=',f3.1,', dy=',f3.1,' pix')

c.....Hardwire in velocity resolution = 1 pixel
      dv=1.0

c.....Open the output, and add a header to it.
      xtension='.cf'
      filecf=filein(1:len1(filein))//xtension
      call output('Output clump assignment file: '//filecf)
      call xyopen(lout,filecf,'new',3,nsize)
      call wrhd(version)

c.....space allocation for data and assign arrays
      call memalloc(Ia,nx*ny*nz,'i')
      call memalloc(It,nx*ny*nz,'r')

c.....Start reading the input file
      call rdgrdclf(data(It),assign(Ia))
      call xyclose(lin)

      line1='------------------------------------------'
      call output(line1)
      call output(' CLFIND  ')
      print 1010, p1
      print 1011, p2
      print 1012, p3
 1010 format(' Starting level:                   ',i4)
 1011 format(' Temperature increment:            ',f4.2,' K')
 1012 format(' Neighbourhood definition:         ',i1,' axes')
      call output(line1)
 
c      print 1013, data(1)
c 1013 format(' t(1) = ',f)
      call CntLevs(data(It),nlevs,npx1,npx2)
      nlevels=nlevs

c.....space allocation for coded position arrays
      call memalloc(Ipos1,npx1,'i')
      call memalloc(Ipos,nlevs*npx2,'i')
      call memalloc(Ireg,nlevs*npx2,'i')

      call FillLevs(data(It),nlevs,npx1,npx2,
     *               pos1(Ipos1),pos(Ipos),reg(Ireg))

c.....Connect to find connected regions at each gray level
      call output(' ')
      call findreg(nlevs,npx2,pos(Ipos),reg(Ireg))

c.....Work thru data cube, from highest level to lowest
c.....connecting regions across levels
c.....Unassigned regions are local maxima => new clumps
c.....Extend (old and new) clumps to next level down
c.....and deal with possible merging.
      call output(' ')
c.....Total number of clumps
      nclump=0
      do ngy=nlevels,2,-1
c........Number of clumps at level ngy
         ncl=0
         call findclp(ngy,ncl,nclump,data(It),assign(Ia),
     *                nlevs,npx2,pos(Ipos),reg(Ireg))
         print 1040, ngy+p1-1,ncl
         if (ngy.gt.2)
     *   call xtendclp(ngy,nclump,data(It),assign(Ia),
     *                  nlevs,npx2,pos(Ipos),reg(Ireg))
      enddo
 1040 format(' Level ',i2,':',i5,' new clumps')

      call output(' ')
      call output('Extending to level 1')
      call xtendlow(nclump,assign(Ia),npx1,pos1(Ipos1))

      call output('Testing clumps for badness')
      nstop=0 
c.....clumps must have greater than nmin pixels
      call testbad(nmin,nstop,nclump,assign(Ia))

      line2='=========================================='
      call output(line2)
      call output('Program terminates sucessfully')
      print 1080, nclump-nstop,nstop
      call output(line2)
 1080 format(i5,' clumps found (',i5,' clumps stopped)')

      call printree(nclump,nlevs,data(It))

      call output(line2)

      call output('Writing assignment file')
      call wrgrdclf(assign(Ia))
      call xyclose(lout)

      call memfree(Ia,nx*ny*nz,'i')
      call memfree(It,nx*ny*nz,'r')
      call memfree(Ipos1,npx1,'i')
      call memfree(Ipos,nlevs*npx2,'i')
      call memfree(Ireg,nlevs*npx2,'i')

      end
c ---------------------------------------------------------
      subroutine CntLevs(t,nlevs,npx1,npx2)
      implicit none
c-----------------------------------------------------------------
c counts the number of levels and pixels/level
c (these numbers are used to assign appropriate
c  space in dynamic memory for arrays npos1,npos2,nreg)
c-----------------------------------------------------------------
      include 'clfind.h'

      integer nlevs,npx1,npx2
      integer nps,npx,ngy,ngx
      real t(*)

      nlevs=-999
      do nps=1,nx*ny*nz
c......Determine contour level

c 1007   format(' p1 = ',i)
c 1008   format(' p2 = ',f)
c 1009   format(' t(',i,') = ',f)
c         print 1007,  p1
c         print 1008,  p2
c         print 1009, nps, t(nps)

         ngy=int(t(nps)/p2)-p1+1

c         print 1010, ngy
c 1010   format(' Number of contours:                   ',i)

      if(ngy.gt.maxlvl) then 
        call bug('f','Too many contour levels')
      endif
c     only count when in range, above the minimum (p1)
      if(ngy.gt.0) then

         npix(ngy)=npix(ngy)+1
         npx=npix(ngy)
         if(ngy.gt.1 .and. npx.gt.maxpix)
     *       call bug('f','Too many pixels per level')

c         print 1011, ngx
c 1011   format(' Number of pixels in level 1:                   ',i4)

         if(ngy.eq.1 .and. npx.gt.maxpix1)
     *        call bug('f','Too many pixels in level 1')

         if (ngy.gt.nlevs) nlevs=ngy
      endif

      enddo

      do ngy=nlevs,1,-1
         print 10, ngy+p1-1,npix(ngy)
      enddo
 10   format(' Level ',i2,':',i5,' pixels')

      npx1=npix(1)
      npx2=npix(2)

      return
      end
c-----------------------------------------------------------------
      subroutine xtendclp(ngy,nclump,t,a,n1,n2,npos,nreg)
      implicit none
c--------------------------------------------------------------
c     subroutine of clfind
c     extends previously defined clumps
c     at gray levels >= ngy to ngy-1
c--------------------------------------------------------------
      include 'clfind.h'

      integer i0,j0,k0
      integer i1,j1,k1
      integer i2,j2,k2
      integer ncl,ncl1,ngy0,ngy1
      integer npx,npx1,nps1,nps2,nrgn
      integer ngy,nclump
      integer indx,maxindx,nclmerge
      integer iter,ngyhigh
      integer nmax,naxis
      integer ix,iy,iv
      integer n1,n2
      integer a(*),npos(n1,n2),nreg(n1,n2)
      real sx,sy,sv
      real d,dmax
      real t(*)
      logical done

c.....temporary arrays for sucessive neighbourhood algorithm
      integer tmpos(maxpix),tmpcl(maxpix)

      ix=int(dx-0.001)+1
      iy=int(dy-0.001)+1
      iv=int(dv-0.001)+1

c.....First extend the tree structure of the clumps to level ngy-1
      do nrgn=1,maxreg
       merge(nrgn)=.false.
      enddo

c.....Look at all higher levels from present one
      do ngy1=ngy,nlevels
       do npx1=1,npix(ngy1)
        nps1=npos(ngy1,npx1)
        ncl1=a(nps1)
        if(ncl1.eq.badcl) goto 20

c........Examine neighbourhood of each pixel
         call invindx(i1,j1,k1,nps1)
         do k2=max(k1-iv,1),min(k1+iv,nz)
         do j2=max(j1-iy,1),min(j1+iy,ny)
         do i2=max(i1-ix,1),min(i1+ix,nx)
          sv=real(abs(k1-k2))/dv
          sy=real(abs(j1-j2))/dy
          sx=real(abs(i1-i2))/dx
          if(sv.lt.1.05.and.sy.lt.1.05.and.sx.lt.1.05) then
           naxis=0
           if(sv.gt.0.05) naxis=naxis+1
           if(sy.gt.0.05) naxis=naxis+1
           if(sx.gt.0.05) naxis=naxis+1

c..........Check each "connected" pixel to
c..........see if at next lower gray level
           if(naxis.le.p3) then
            call gtindx(i2,j2,k2,nps2)
            ngy0=int(t(nps2)/p2)-p1+1
            if(ngy0.eq.ngy-1) then

c............Search for and then flag
c............the region this pixel belongs to
             do npx=1,npix(ngy-1)
              if(npos(ngy-1,npx).eq.nps2) then
               nrgn=nreg(ngy-1,npx)
               merge(nrgn)=.true.

c..............Update structure tree ---
c..............Region <nrgn> at level <ngy-1>
c..............is merged with clump <ncl1>
               tree(ngy-1,nrgn,ncl1)=.true.
               goto 10
              endif
             enddo

 10          continue
            endif
           endif

          endif
         enddo
         enddo
         enddo

 20      continue
       enddo
      enddo


c------------------------------------------------------
c  Split up merged regions at level ngy-1:
c  Unmerged regions are wholly assigned to one clump
c  Merged regions are assigned to different clumps
c  based on "sucessive neighbourhoods"
c  Regions which are not merged with any higher levels
c  are left alone (they become a new clump next time)
c------------------------------------------------------
      do 1001 nrgn=1,nregions(ngy-1)
c......Examine all regions for merging
       if(merge(nrgn)) then
        nclmerge=0
c.......Count how many clumps are merged in this region
        do ncl=1,nclump
         if(tree(ngy-1,nrgn,ncl)) then
           nclmerge=nclmerge+1
           ncl1=ncl
         endif
        enddo

c.......Set up temporary arrays
        indx=0
        do npx=1,npix(ngy-1)
         if(nreg(ngy-1,npx).eq.nrgn) then
          indx=indx+1
          tmpos(indx)=npos(ngy-1,npx)
          tmpcl(indx)=0
         endif
        enddo
        maxindx=indx

c.......It's easy if there is just one merger (= ncl1)
c.......---> assign all the region to this clump
        if(nclmerge.eq.1) then
         do indx=1,maxindx
          a(tmpos(indx))=ncl1
          clump(ncl1,4)=clump(ncl1,4)+1
         enddo

        else

c........If there is more than one contributing clump
c........assign pixels in this region based on method
c........of "sucessive neighbourhoods"
         ngyhigh=ngy+1
         iter=0
 25      continue
         done=.true.
         if(iter.gt.10) ngyhigh=999
         iter=iter+1

         do 1002 indx=1,maxindx
          if(tmpcl(indx).eq.0) then
c..........Go thru each pix in this region until all
c..........have been (sucessively) assigned to a clump
           nmax=999
           done=.false.
           call invindx(i1,j1,k1,tmpos(indx))
           do k2=max(k1-iv,1),min(k1+iv,nz)
           do j2=max(j1-iy,1),min(j1+iy,ny)
           do i2=max(i1-ix,1),min(i1+ix,nx)
            sv=real(abs(k1-k2))/dv
            sy=real(abs(j1-j2))/dy
            sx=real(abs(i1-i2))/dx
            if(sx.lt.1.05.and.sy.lt.1.05.and.sv.lt.1.05) then
             naxis=0
             if(sv.gt.0.05) naxis=naxis+1
             if(sy.gt.0.05) naxis=naxis+1
             if(sx.gt.0.05) naxis=naxis+1

             if(naxis.le.p3) then
c.............Require that pixel is a neighbour and lies
c.............between gray levels ngy-1 and ngyhigh (defined
c.............above). In case more than one clump fits
c.............this category assignment goes to clump
c.............with highest peak temperature.

              call gtindx(i2,j2,k2,nps2)
              ngy0=int(t(nps2)/p2)-p1+1
              if(ngy0.ge.ngy-1 .and. ngy0.le.ngyhigh) then
               ncl=a(nps2)
               if(ncl.gt.0.and.ncl.le.nclump.and.naxis.le.nmax) then
                if(naxis.lt.nmax) then
                 nmax=naxis
                 dmax=9999.9
                endif
                i0=clump(ncl,1)
                j0=clump(ncl,2)
                k0=clump(ncl,3)
                d=(real(i0-i1)/dx)**2+
     *            (real(j0-j1)/dy)**2+
     *            (real(k0-k1)/dv)**2
                if(d.lt.dmax) then
                 dmax=d
                 tmpcl(indx)=ncl
                endif
               endif
              endif

             endif

            endif
           enddo
           enddo
           enddo
          endif
 1002    continue

         do indx=1,maxindx
          ncl=tmpcl(indx)
          if(ncl.gt.0) a(tmpos(indx))=ncl
         enddo
         if(.not.done) goto 25

         do indx=1,maxindx
          ncl=tmpcl(indx)
          if(ncl.gt.0) clump(ncl,4)=clump(ncl,4)+1
         enddo

        endif

       endif
 1001 continue

      return
      end
c ---------------------------------------------------------
      subroutine xtendlow(nclump,a,n,npos1)
      implicit none
c-------------------------------------------------------
c     subroutine of clfind
c     After all clumps defined to second from
c     lowest level, extend pixel by pixel to
c     the lowest level (p1, generally set to 1)
c-------------------------------------------------------
      include 'clfind.h'
      integer ncl1,ncl2
      integer naxis,nclump
      integer nps1,nps2,npx1
      integer i1,j1,k1,i2,j2,k2
      integer ix,iy,iv
      integer a(*)
      real sx,sy,sv
      integer badpix,newpix,goodpix
      integer n
      integer npos1(n)

      ix=int(dx-0.001)+1
      iy=int(dy-0.001)+1
      iv=int(dv-0.001)+1

      badpix=0
      goodpix=0
      newpix=0

 10   continue
      goodpix=goodpix+newpix
      newpix=0
c.....Look at first level
      do npx1=1,npix(1)
         nps1=npos1(npx1)
         call invindx(i1,j1,k1,nps1)
c........Look at neighbourhood of each pixel in first level
         do k2=max(k1-iv,1),min(k1+iv,nz)
         do j2=max(j1-iy,1),min(j1+iy,ny)
         do i2=max(i1-ix,1),min(i1+ix,nx)

c...........Go ahead only if neighbourhood pixel is in a clump
            call gtindx(i2,j2,k2,nps2)
            ncl2=a(nps2)
            ncl1=a(nps1)
            if (ncl1.gt.nclump) goto 30
            if (ncl2.eq.0) goto 20
            if (ncl2.gt.nclump) goto 20
            if (ncl2.eq.ncl1) goto 20

            sv=real(abs(k1-k2))/dv
            sy=real(abs(j1-j2))/dy
            sx=real(abs(i1-i2))/dx
            if(sv.gt.1.05) goto 20
            if(sy.gt.1.05) goto 20
            if(sx.gt.1.05) goto 20
            naxis=0
            if(sv.gt.0.05) naxis=naxis+1
            if(sy.gt.0.05) naxis=naxis+1
            if(sx.gt.0.05) naxis=naxis+1
c...........Goto next pixel if this one not in neighbourhood
            if(naxis.gt.p3) goto 20

            if (ncl1.eq.0) then
c..............If lowest level pixel is unassigned then assign it
               newpix=newpix+1
               a(nps1)=ncl2
               clump(ncl2,4)=clump(ncl2,4)+1
            else
c..............If lowest level pixel is already assigned
c..............then it is merged with >1 clump => flag it bad
               newpix=newpix-1
               badpix=badpix+1
               clump(ncl1,4)=clump(ncl1,4)-1
               a(nps1)=badcl
               goto 30
            endif

 20         continue
         enddo
         enddo
         enddo

 30      continue
c........Goto next pixel in lower level
      enddo

c.....Repeat process until there are no new assigned pixels
      if (newpix.gt.0) goto 10

      print 40,goodpix
      print 50,badpix
 40   format(i5,' pixels added')
 50   format(i5,' pixels unused')

      return
      end
c ---------------------------------------------------------
      subroutine FillLevs(t,nlevs,npx1,npx2,npos1,npos,nreg)
      implicit none
c-----------------------------------------------------------------
c fills in the arrays nreg, npos, and npos1 with the
c 1D pixel number of each contour level
c-----------------------------------------------------------------
      include 'clfind.h'

      integer i,j,k
      integer nlevs,npx1,npx2
      integer nps,npx,ngy
      integer npos1(npx1),npos(nlevs,npx2),nreg(nlevs,npx2)
      real t(*)

c.....Initialize npix() which was counted in CountLevs
c.....but we don't want it to be recounted here
      do ngy=1,nlevs
       npix(ngy)=0
      enddo

      do k=1,nz
      do j=1,ny
      do i=1,nx
       call gtindx(i,j,k,nps)
c      do nps=1,nx*ny*nz
c........Determine contour level
         ngy=int(t(nps)/p2)-p1+1

         if(ngy.gt.1) then
            npix(ngy)=npix(ngy)+1
            npx=npix(ngy)

c...........This point is initially unassigned to any region
            nreg(ngy,npx)=-1
            npos(ngy,npx)=nps
         endif

c........Treat level 1 separately from the others
         if(ngy.eq.1) then
            npix(1)=npix(1)+1
            npx=npix(1)
            npos1(npx)=nps
         endif

      enddo
      enddo
      enddo

      return
      end
c-----------------------------------------------------------------
      subroutine findclp(ngy,ncl,nclump,t,a,n1,n2,npos,nreg)
      implicit none
c-----------------------------------------------------------
c     subroutine of clfind
c     finds and assigns new clumps at gray level ngy
c-----------------------------------------------------------
      include 'clfind.h'

      integer ngy,ncl,nclump
      integer nrgn,npx,npx1,nps1
      integer i1,j1,k1,ipeak,jpeak,kpeak
      integer n1,n2
      integer a(*),npos(n1,n2),nreg(n1,n2)
      real t1,tpeak
      real t(*)

      do nrgn=1,nregions(ngy)
c						 unmerged rgn=>new cl
         if (.not.merge(nrgn)) then              
            ncl=ncl+1
            nclump=nclump+1
            tpeak=-999.
            npx=0
            do npx1=1,npix(ngy)
c						 assign entire rgn
               if (nreg(ngy,npx1).eq.nrgn) then  
                  nps1=npos(ngy,npx1)
                  a(nps1)=nclump
c						find #pix and..
                  npx=npx+1                      
                  t1=t(nps1)
                  if (t1.gt.tpeak) then
                     call invindx(i1,j1,k1,nps1)
c						..peak posn of..
                     tpeak=t1                   
c						..new clump
                     ipeak=i1                   
                     jpeak=j1
                     kpeak=k1
                  endif
               endif
            enddo
            clump(nclump,1)=ipeak
            clump(nclump,2)=jpeak
            clump(nclump,3)=kpeak
            clump(nclump,4)=npx
         endif
      enddo

      return
      end
c-----------------------------------------------------------
      subroutine findreg(n1,n2,npos,nreg)
      implicit none
c-------------------------------------------------------
c     subroutine of clfind
c     finds connected regions at each gray level
c-------------------------------------------------------
      include 'clfind.h'

      integer naxis,ngy,nrgn
      integer npx1,npx2,nps2,npx3,nps3
      integer i2,j2,k2,i3,j3,k3
      integer n1,n2
      integer npos(n1,n2),nreg(n1,n2)
      real sx,sy,sv
      logical newpix

c.....Work at each contour level, from top to bottom
      do ngy=nlevels,2,-1
        nrgn=0

c.......Go thru pixels in this level, looking for
c.......those which are unassigned ==> new region
        do npx1=1,npix(ngy)
        if(nreg(ngy,npx1).eq.-1) then

          nrgn=nrgn+1
          nreg(ngy,npx1)=nrgn
          newpix=.true.
          do while (newpix)
           newpix=.false.

c..........Extend region by looking at the neighbours of
c..........all pixels in this region
           do npx2=1,npix(ngy)
           if(nreg(ngy,npx2).eq.nrgn) then
             nps2=npos(ngy,npx2)
             call invindx(i2,j2,k2,nps2)

             do npx3=1,npix(ngy)
             if(nreg(ngy,npx3).eq.-1) then
               nps3=npos(ngy,npx3)
               call invindx(i3,j3,k3,nps3)
               sx=real(abs(i2-i3))/dx
               sy=real(abs(j2-j3))/dy
               sv=real(abs(k2-k3))/dv
               if(sx.lt.1.05 .and.
     *            sy.lt.1.05 .and.
     *            sv.lt.1.05) then
                    naxis=0
                    if(sx.gt.0.05) naxis=naxis+1
                    if(sy.gt.0.05) naxis=naxis+1
                    if(sv.gt.0.05) naxis=naxis+1
                    if(naxis.le.p3) then
                      newpix=.true.
                      nreg(ngy,npx3)=nrgn
                    endif
               endif
             endif
             enddo

           endif
           enddo

          enddo

        endif
        enddo

        nregions(ngy)=nrgn
        print 1030,ngy+p1-1,nrgn
      enddo
 1030 format(' Level ',i2,':',i5,' regions')

      return
      end
c ---------------------------------------------------------
      subroutine gtindx(i,j,k,nps)
      implicit none
c-------------------------------------------------------
c     subroutine of clfind
c     converts 3d position vector to 1d index
c-------------------------------------------------------
      include 'clfind.h'
      integer i,j,k,nps

      nps=nz*(ny*(i-1)+(j-1))+k

      return
      end
c ---------------------------------------------------------
      subroutine invindx(i,j,k,nps)
      implicit none
c-------------------------------------------------------
c     subroutine of clfind
c     converts 1d index back to 3d positional vector
c-------------------------------------------------------
      include 'clfind.h'
      integer i,j,k,nps

      i = 1+int(real(nps-1)/real(nz*ny))
      j = 1+int(real(nps-1)/real(nz))-ny*(i-1)
      k = nps-nz*(ny*(i-1)+(j-1))

      return
      end
c------------------------------------------------------------------------
      subroutine rdgrdclf(t,a)
      implicit none
c---------------------------------------------------
c     subroutine of clfind
c     read in data array and mask
c---------------------------------------------------
      include 'clfind.h'

      integer i,j,k,nps
      integer a(*)
      real t(*)
      real buff(maxdim)
      logical mask(maxdim)  

      do k=1,nz
        call xysetpl(lin,1,k)
        do j=1,ny
          call xyread(lin,j,buff)
          call xyflgrd(lin,j,mask) 
          do i=1,nx
            call gtindx(i,j,k,nps)
            if(mask(i)) then
              t(nps)=buff(i)
            else
              t(nps)=-1000.0
            endif
            a(nps)=0
          enddo
        enddo
      enddo

      return
      end
c---------------------------------------------------
      subroutine rdhd(in)
      implicit none
c-----------------------------------------------------------
c     subroutine of clfind
c     reads the header of file with handle = in
c-----------------------------------------------------------

      include 'clfind.h'

      integer in
      double precision dummy

      call rdhda(in,'ctype1',ctype(1),' ')
      call rdhda(in,'ctype2',ctype(2),' ')
      call rdhda(in,'ctype3',ctype(3),' ')
      call rdhdd(in,'crpix1',dummy,0.0d0)
      crpix(1) = dummy
      call rdhdd(in,'crpix2',dummy,0.0d0)
      crpix(2) = dummy
      call rdhdd(in,'crpix3',dummy,0.0d0)
      crpix(3) = dummy
      call rdhdd(in,'cdelt1',dummy,0.0d0)
      cdelt(1) = dummy
      call rdhdd(in,'cdelt2',dummy,0.0d0)
      cdelt(2) = dummy
      call rdhdd(in,'cdelt3',dummy,0.0d0)
      cdelt(3) = dummy
      call rdhdd(in,'crval1',dummy,0.0d0)
      crval(1) = dummy
      call rdhdd(in,'crval2',dummy,0.0d0)
      crval(2) = dummy
      call rdhdd(in,'crval3',dummy,0.0d0)
      crval(3) = dummy
      call rdhdd(in,'bmaj',dummy,0.0d0)
      bmaj = dummy
      call rdhdd(in,'bmin',dummy,0.0d0)
      bmin = dummy
      call rdhdd(in,'bpa',dummy,0.0d0)
      bpa = dummy

      return
      end
c-----------------------------------------------------------
      subroutine printree(nclump,nmax,t)
      implicit none

c.....Clump deconvolution is done - now print out the
c.....tree structure (which clumps appear at which level
c.....and how they merge together at lower levels)

      include 'clfind.h'

      integer nclump,nmax
      integer i,j,k,n2,n3
      integer nmerge,ngy0,ngy,nps
      real t(*)
      character*80 line

      integer maxmge
      parameter(maxmge=7)
      integer list(5*maxmge)                  


      line='------------------------------------------'
      call output('Tree structure:')
      do ngy=nmax,2,-1
         call output(line)
         print 1090, ngy+p1-1
 1090    format(' Level ',i2)
         do n3=1,nclump
            i=clump(n3,1)
            j=clump(n3,2)
            k=clump(n3,3)
            call gtindx(i,j,k,nps)
            ngy0=int(t(nps)/p2)-p1+1
            if (ngy0.eq.ngy) then
               print 1100, n3,t(nps),clump(n3,4)
 1100 format(' New clump ',i3,'  Tpeak = ',f4.2,'  Npix =',i4)
            endif
         enddo

         do n2=1,nregions(ngy)
            do nmerge=1,maxmge
               list(nmerge)=0
            enddo
            nmerge=0
            do n3=1,nclump
               if (tree(ngy,n2,n3) .and. clump(n3,4).gt.1) then
                  nmerge=nmerge+1
                  if (nmerge.le.5*maxmge) then
                     list(nmerge)=n3
                  else
                     list(5*maxmge)=9999
                  endif
               endif
            enddo
            if (nmerge.gt.1) then
               print 1110, (list(i),i=1,maxmge)
            endif
            if (nmerge.gt.maxmge) then
               print 1120, (list(i),i=maxmge+1,2*maxmge)
            endif
            if (nmerge.gt.2*maxmge) then
               print 1120, (list(i),i=2*maxmge+1,3*maxmge)
            endif
            if (nmerge.gt.3*maxmge) then
               print 1120, (list(i),i=3*maxmge+1,4*maxmge)
            endif
            if (nmerge.gt.4*maxmge) then
               print 1120, (list(i),i=4*maxmge+1,5*maxmge)
            endif
            if (nmerge.gt.5*maxmge) then
               call bug('w',' output buffer Merged Clumps exhausted')
            endif
 1110    format(' Merged clumps:',7(i3,1x))
 1120    format('               ',7(i3,1x))
         enddo

      enddo

      return
      end
      subroutine testbad(nmin,nstop,nclump,a)
      implicit none
c-------------------------------------------------------
c     subroutine of clfind
c     checks for bad clumps and deassigns them
c
c     clumps must have at least nmin pixels
c-------------------------------------------------------
      include 'clfind.h'
      integer nmin,nstop,nclump
      integer ncl,nps
      integer a(*)

      do ncl=1,nclump
         if(clump(ncl,4).lt.nmin) then
           nstop=nstop+1
           do nps=1,nx*ny*nz
              if(a(nps).eq.ncl) a(nps)=0
           enddo
           clump(ncl,4)=0
         endif
      enddo

      return
      end
c ---------------------------------------------------------
      subroutine wrgrdclf(a)
      implicit none
c---------------------------------------------------------
c     subroutine of clfind
c     write out clump assignment array
c---------------------------------------------------------
      include 'clfind.h'

      integer i,j,k,nps
      integer a(*)
      real buff(maxdim)

      do k=1,nz
        call xysetpl(lout,1,k)
        do j=1,ny
          do i=1,nx
            call gtindx(i,j,k,nps)
            buff(i)=real(a(nps))
          enddo
          call xywrite(lout,j,buff)
        enddo
      enddo

      return
      end
c---------------------------------------------------------
      subroutine wrhd(version)
      implicit none
c---------------------------------------------------------------
c     subroutine of clfind
c     writes header for image cube
c---------------------------------------------------------------
      integer nkeys
      parameter(nkeys=12)
      include 'clfind.h'

      character version*(*), keyw(nkeys)*8, lversion*32
      integer i

      data keyw/   'cdelt1  ','cdelt2  ','cdelt3  ',
     *    'crpix1  ','crpix2  ','crpix3  ',
     *    'crval1  ','crval2  ','crval3  ',
     *    'ctype1  ','ctype2  ','ctype3  '/

c.....Copy the old header partially, and write some new parameters
      do i=1,nkeys
        call hdcopy(lin,lout,keyw(i))
      enddo

c.....Update the history.
      call hdcopy(lin,lout,'history')
      call hisopen(lout,'append')
      lversion=version
      call hiswrite(lout,'CLFIND: Miriad '//lversion)
      call hisinput(lout,'CLFIND')
      call hisclose(lout)

      return
      end
c---------------------------------------------------------------
