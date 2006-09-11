      program clstats
      implicit none
c
c= clstats - Reads in clfind assign file and calculates
c     	     dispersions, masses, etc for each clump
c& jpw
c
c
c  The mass is calculated in the following way;
c  we define X such that,
c
c  X * (sum T) * delv = column density of molecular hydrogen
c                       in units of 10^20 cm-2
c
c  which implies that to convert to a mass, multiply by
c
c  10^20 * meanmol * m(H_2) * Area
c
c  --- KEYWORDS ---
c
c@ in
c       file name of data cube
c       (no default)
c
c@ dist
c       the distance to the object in pc
c       (no default)
c
c@ disterr
c       multiplicative error in the distance
c       eg disterr=2 means dist is uncertain by a factor of 2
c       (default=1)
c
c@ X
c       the X-factor to convert between column density of the
c       species observed to the column density of molecular hydrogen
c       --- see above. (Note that this includes any corrections for
c       abundance, excitation temperature, CMBR, antenna efficiency,etc.)
c       (In units of 10^20, default=2.4)
c
c@ Xerr
c       multiplicative error in X -- from uncertainties in
c       abundances, excitation temperature, efficiency, etc.
c       (default=1)
c
c@ meanmol
c       the mean molecular weight of the gas
c       (default=1.38, corresponding to n(He)/n(H)=0.1)
c
c@ Jyperk
c       the number of Jansky's per Kelvin for these observations
c       (default=1)
c
c@ xy
c       "rel" or "abs" for the clump positions - v is always absolute
c       (default=relative)
c
c@ rms
c       optional input to specify the rms error in temperature
c       for each pixel in the map (used to calcuate errors in
c       the masses). Otherwise 0.5*DeltaT, where DeltaT is the
c       clfind contour interval.
c
c@ nmin
c       each clump must contain at least nmin pixels to be
c       written out to output file
c       (default=4)
c--
c  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c   Clumps are flagged E/A/V/R depending on whether they are near or
c   on and edge, is they have a high aspect ratio (> 2), if they are
c   unresolved in velocity, or if they are unresolved in size.
c  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  History:
c  24jan94  jpw  spruced up numerous versions (newhcstatco,stat13co, etc)
c  18may94  jpw  parameters dimx/y/v allow different dimensons in x,y,v
c  01jun94  jpw  parameter ncl is now the number of real clumps, and not
c                (necessarily) the last clump number.
c  20jul94  jpw  converted to use dynamic memory
c  19feb97  mwp  bug fixes , dv versus sv in dostats.for
c-------------------------------------------------------------------------
      character version*(*)
      parameter(version='version 1.4 19-feb-97' )
c      include 'header.h'
      include 'clstats.h'

      integer lenline,imax,ncmax
      logical pos
      character line*80,positions*3
      character*80 head1,head2

c.....dynamic memory setup
      integer It,Ic,Im
      integer iData(maxbuf)
      real rData(maxbuf)
      logical lData(maxbuf)
      common iData
      equivalence(iData,rData,lData)
	logical keyprsnt

      call output('CLSTATS '//version)

c.....Get the parameters from the user.
      call keyini
      call keya('in',file,' ')
      call keyr('dist',dist,0.0)
      call keyr('disterr',disterr,1.0)
	if(keyprsnt('x')) then
      		call keyr('x',xfact,2.4)
	else
      		call keyr('X',xfact,2.4)
	endif
	if(keyprsnt('xerr')) then
	      call keyr('xerr',xfacterr,1.0)
	else
	      call keyr('Xerr',xfacterr,1.0)
	endif
      call keyr('meanmol',meanmol,1.38)
      call keyr('jyperk',kpjy,1.0)
	kpjy=1./kpjy
      call keya('xy',positions,'rel')
      call keyr('rms',rms,0.0)
      call keyi('nmin',nmin,4)
      call keyfin

      if(file.eq.' ')call bug('f','Input file name missing')
      if(dist.eq.0.0)call bug('f','Source distance must be given')

      if(positions.eq.'abs') pos=.true.

c.....Open data cube
      call output('Data cube: '//file)
      call xyopen(lin1,file,'old',3,nsize)
      nx = nsize(1)
      if(nx.gt.maxdim)call bug('f','Image is too big in axis 1')
      ny = nsize(2)
      if(ny.gt.maxdim)call bug('f','Image is too big in axis 2')
      nv = nsize(3)
      if(nv.gt.maxdim)call bug('f','Image is too big in axis 3')
      if(nx*ny*nv.gt.maxbuf) call bug('f','Image too big')
      call rdhd(lin1)

      call memalloc(It,nx*ny*nv,'r')
      call memalloc(Ic,nx*ny*nv,'i')
      call memalloc(Im,nx*ny*nv,'l')

      call printhead(pos)

      line='-------------------------------------------------'
      call output(line(1:lenline(line))//line(1:lenline(line)))

      call readata(idata(Ic),rdata(It),ldata(Im),imax,ncmax)

      head1='Cl#   Xpeak    Ypeak   Vpeak   <t>     t0   FWHMx'
      head2=' FWHMy R(pc) FWHMv  Mlte   Err     Mgrav    N'
      call output(head1(1:lenline(head1))//head2(1:lenline(head2)))
      call output(line(1:lenline(line))//line(1:lenline(line)))

      call dostats(pos,idata(Ic),rdata(It),ldata(Im),imax,ncmax)

      call output(line(1:lenline(line))//line(1:lenline(line)))

      call memfree(It,nx*ny*nv,'r')
      call memfree(Ic,nx*ny*nv,'i')
      call memfree(Im,nx*ny*nv,'l')

      end
c------------------------------------------------------------------
      subroutine dostats(pos,c,t,m,imax,ncmax)
      implicit none
c---------------------------------------------------------------------
c  calculate the statistics for each clump, in turn
c---------------------------------------------------------------------
c      include 'header.h'
      include 'clstats.h'

      integer i,j,k,nc,imax,ncmax
      integer i1,j1,k1,i2,j2,k2
      integer nclump,nmin,npixels,lenline
      real pi,rad2asec
      real sx2,sx,sy2,sy,sv2,sv,dv
      real x1,y1,v1,xp,yp,vp,tp,tave,ts
      real mlte,masserr,mgrav,mvir,tsumerr
      real radius,beamr
      real aspect
      logical pos
      character flag*4

      integer c(nx,ny,nv)
      real t(nx,ny,nv)
      logical m(nx,ny,nv)

      pi=3.1415926536
      rad2asec=180.0*3600.0/pi

c.....go through the cube checking which clumps border the
c.....edges (as defined by the mask)
      do k=1,nv
      do j=1,ny
      do i=1,nx
        nc=c(i,j,k)

        if(t(i,j,k).gt.2*dt .and. nc.gt.0) then
          do k1=k-1,k+1
           k2=k1
           if(k1.lt.1) k2=1
           if(k1.gt.nv) k2=nv
            do j1=j-1,j+1
             j2=j1
             if(j1.lt.1) j2=1
             if(j1.gt.ny) j2=ny
              do i1=i-1,i+1
               i2=i1
               if(i1.lt.1) i2=1
               if(i1.gt.nx) i2=nx
               if(.not.m(i2,j2,k2)) edge(nc)=.true.
              enddo
            enddo
          enddo
        endif

      enddo
      enddo
      enddo

c.....clumps with Sx/Sy > aspect are flagged
      aspect=2.0

c.....go thru clump numbers 1 to maximum
c.....and find it's coded index
      do nclump=1,ncmax
        nc=0
        do i=1,imax
          if(clump(i).eq.nclump) nc=i
        enddo
        if(nc.eq.0) goto 20

        npixels=npix(nc)
        if (npixels.lt.nmin) goto 20
        ts=tsum(nc)
        if (ts.le.0.0) goto 20

        flag=''
        if(edge(nc)) flag='E'

c.......Relative peak positions in arcseconds
        xp=(real(xpeak(nc))-crpix(1))*delx
        yp=(real(ypeak(nc))-crpix(2))*dely

c.......Absolute peak velocity in km/s
        vp=real(crval(3))+(real(vpeak(nc))-crpix(3))*delv

c.......Peak temperature
        tp=t(xpeak(nc),ypeak(nc),vpeak(nc))

c.......(temperature weighted) mean clump postion
        x1=real(xsum(nc))/ts
        y1=real(ysum(nc))/ts
        v1=real(vsum(nc))/ts

c.......dispersions
        sx2=xsq(nc)/ts - x1**2
c.......Allow for roundoff errors, etc. if sx2 is very close to 0
        if(sx2.gt.0.0) then
          sx=sqrt(sx2)
        else
          sx=0.0
        endif
        sx=sx*abs(delx)

        sy2=ysq(nc)/ts - y1**2
        if(sy2.gt.0.0) then
          sy=sqrt(sy2)
        else
          sy=0.0
        endif
        sy=sy*dely

c.......Correct these sigma's for the beam size
        if(sx .gt. 1.25*(beamx/2.355)) then
          sx = sqrt(sx**2 - (beamx/2.355)**2)
        else
          sx = 0.5*beamx/2.355
        endif

        if(sy .gt. 1.25*(beamy/2.355)) then
          sy = sqrt(sy**2 - (beamy/2.355)**2)
        else
          sy = 0.5*beamy/2.355
        endif

        if (sx.gt.aspect*sy .or. sy.gt.aspect*sx)
     *      flag=flag(1:lenline(flag))//'A'

        sv2=vsq(nc)/ts - v1**2
        if(sv2.gt.0.0) then
c... sv is the 1 d velocity dispersion in pixels
          sv=sqrt(sv2)
        else
          sv=0.0
        endif
c...... delv is the velocity resolution (channel width) in km/s
c...... dv is the v(FWHM) in km/s
        dv=2.355*sv*delv
        if (dv .lt. delv) then
          flag=flag(1:lenline(flag))//'V'
          dv = delv
c... sv is now the 1 d velocity dispersion in km/s
c          sv=dv/2.355
        endif

        radius = sqrt(real(npixarea(nc))*abs(delx)*dely/pi)
        beamr = sqrt((beamx*beamy)*
     *          (2.0*log(tp/(dt*real(start)))))/2.355
	
        if (radius .gt. 1.1*beamr) then
          radius = sqrt(radius**2 - beamr**2)
        else
          flag=flag(1:lenline(flag))//'R'
          radius = 0.5*beamr
        endif
c.......change radius to pc
        radius=radius*dist/rad2asec

c.......Until now everything has been done in units of JY/BEAM.
c.......The summation, however, has been done in pixels. So we
c.......need to convert the temperatures to K and we need to
c.......correct for the effective beam area (beam area in pixels).
        tp = tp*kpjy
        tave = ts*kpjy/real(npixels)

        mlte=1.60*meanmol*xfact*ts*kpjy*(dist**2)*delv*
     *       abs(delx)*dely/(rad2asec*rad2asec)
        tsumerr = 1.0+sqrt(real(npixels))*rms/ts
        masserr=disterr**2 * xfacterr * tsumerr
c.... coefficient here is for 1d vel dispersion sx so 
c.... divide again by 8*ln(2)
        mvir=698.4*radius*(dv**2)/5.54518
        mgrav=0.5*mvir

c.......make dispersions into FWHM, and convert sizes to pc
        sx=2.355*sx*dist/rad2asec
        sy=2.355*sy*dist/rad2asec
c...        sv=2.355*sv

c.......if beam size is smaller than 30" write output in arcseconds
c.......else write output in arcminutes or degrees depending on
c.......whether relative or absolute positions are requested
        if(sqrt(abs(beamx*beamy)).lt.30.0) then
           print 201,
     *           nclump,xp,yp,vp,tave,
     *           tp,sx,sy,radius,dv,
     *           mlte,masserr,mgrav,npixels,flag
        else
          if(pos) then
            xp=(x0+xp)/3600.0
            if(xp.lt.0.0) xp=xp+360.0
            print 201,
     *           nclump,xp,(y0+yp)/3600.,vp,tave,
     *           tp,sx,sy,radius,dv,
     *           mlte,masserr,mgrav,npixels,flag
          else
            print 201,
     *           nclump,xp/60.,yp/60.,vp,tave,
     *           tp,sx,sy,radius,dv,
     *           mlte,masserr,mgrav,npixels,flag
          endif
        endif
 20     continue
      enddo

 200    format(i3,2x,f6.1,3x,f5.1,2x,f7.2,x,f6.2,x,f6.2,
     *         x,f5.2,x,f5.2,x,f5.2,x,f5.2,x,
     *         f7.2,2x,f4.2,2x,f8.2,x,i4,x,a4)
 201    format(i3,x,f10.3,x,f10.3,x,f7.2,x,f6.2,x,f6.2,
     *         x,f7.2,x,f7.2,x,f5.2,x,f5.2,x,
     *         f10.2,2x,f5.3,2x,f10.2,x,i4,x,a4)

      return
      end
c---------------------------------------------------------------------
	integer function lenline(string)
c
	implicit none
	character string*(*)
c
c  This determines the unblanked length of a character string.
c
c  Input:
c    string		The character string that we are interested in.
c  Output:
c    lenline	The unpadded length of the character string.
c--
c------------------------------------------------------------------------
	integer l
	logical more
c
	l = len(string)
	more = .true.
	do while(l.gt.0.and.more)
	  if(string(l:l).le.' '.or.string(l:l).gt.'~')then
	    l = l - 1
	  else
	    more = .false.
	  endif
	enddo
c
	lenline = l
	end
c------------------------------------------------------------------------
      subroutine printhead(pos)
      implicit none
c-----------------------------------------------------------------------
c  print out the header; distance, beam size, etc
c-----------------------------------------------------------------------
c      include 'header.h'
      include 'clstats.h'

      integer lenline,nwords
      real rad2asec
      real refx,fluxmin,mass_sens
      character extension*3,words(10)*80
      character*80 filecf,line
      logical eof,pos

      data eof/.false./
      rad2asec=206264.806


c.....In MIRIAD format, angular variables are in radians 
c.....first, convert to arcseconds
      x0=real(crval(1)*rad2asec)
      y0=real(crval(2)*rad2asec)
      delx=real(cdelt(1)*rad2asec)
      dely=real(cdelt(2)*rad2asec)
      delv=real(cdelt(3))
      bmaj=real(bmaj*rad2asec)
      bmin=real(bmin*rad2asec)
      bpa=real(bpa)

      if(bmaj .lt. 0.0001 .or. bmin .lt. 0.0001) 
     *	call bug('f','Beam parameters incorrectly read from header')
      if(abs(bpa) .le. 45.0) then
        beamx = bmin
        beamy = bmaj
      else
        beamx = bmaj
        beamy = bmin
      endif

c.....Open the clump assignment file
      extension=".cf"
      filecf=file(1:lenline(file))//extension
      call xyopen(lin2,filecf,'old',3,nsize)
      call hisopen(lin2,'read')
      do while (.not.eof)
c.......read in the clumpfind parameters: start,deltaT,naxis
c.......start  = starting contour level
c.......deltaT = contour interval
c.......naxis  = connection criterion (generally=3)
        call hisread(lin2,line,eof)
        call sopchr(line,words,nwords)
        if(words(2)(1:2) .eq. 'DT') read(words(3),*) dt
        if(words(2)(1:5) .eq. 'START') read(words(3),*) start
        if(words(2)(1:5) .eq. 'NAXIS') read(words(3),*) naxis
      enddo
c.....defaults --->
      if(start.eq.0) start=1
      if(dt.eq.0)
     *  call bug('f','CLFIND parameters incorrectly read from header')
      if(naxis.eq.0) naxis=3
      call hisclose(lin2)

c.....print out some header information for the stats file
      refx=x0/3600.0
      if(refx.lt.0.0) refx=refx+360.0
      if(sqrt(beamx*beamy).lt.30.0) then

        if(bpa.gt.0.0) then
          print 101,bmaj,bmin,bpa
        else
          print 102,bmaj,bmin
        endif

        call output(
     *       'Positions are RELATIVE and are in arcseconds')
        print 120,refx,y0/3600.0

      else

        if(bpa.gt.0.0) then
          print 103,bmaj/60.0,bmin/60.0,bpa
        else
          print 104,bmaj/60.0,bmin/60.0
        endif

        if(pos.eq..true.) then
          call output(
     *         'Positions are ABSOLUTE and are in degrees')
          call output(' ')
        else
          call output(
     *         'Positions are RELATIVE and are in arcminutes')
          print 120,refx,y0/3600.0
        endif

      endif

      if(disterr.gt.1.0) then
        print 111,nint(dist),disterr
      else
        print 110,nint(dist)
      endif

      print 140,start
      if(rms.gt.0.0) then
         print 142,dt,rms
      else
         rms=0.5*dt
         print 141,dt
      endif
      print 143,naxis

      if(xfacterr.gt.1.0) then
        print 151,xfact,xfacterr
      else
        print 150,xfact
      endif
c.....smallest clump has one pixel in the second
c.....contour level and (nmin-1) at the first
      fluxmin = ((nmin-1)*start+(start+1))*dt*kpjy
      mass_sens=1.60*meanmol*xfact*(dist**2)*fluxmin*delv*
     *          abs(delx)*dely/(rad2asec*rad2asec)
      print 152,mass_sens



 101  format('Beam size =',
     *       f5.2,' by ',f5.2,' (arcsecs) with pa = ',f7.2)
 102  format('Beam size =',f5.2,' by ',f5.2,' (arcsecs)')
 103  format('Beam size =',
     *        f5.2,' by ',f5.2,' (arcmin) with pa = ',f7.2)
 104  format('Beam size =',f5.2,' by ',f5.2,' (arcmin)')
 110  format('Distance to source =',i6,' pc')
 111  format('Distance to source =',i6,' pc (error = ',f4.2,')')
 120  format('Reference position (degrees) = (',f7.3,',',f7.3,')')
 140  format('Starting contour level =',i2)
 141  format('Delta T = ',f5.3)
 142  format('Delta T = ',f5.3,'  (rms = ',f5.3,')')
 143  format('Naxis =',i2)
 150  format('X =',f5.3,'E20')
 151  format('X =',f5.3,'E20  (error = ',f5.3,')')
 152  format('Mass detectability = ',f8.3,' solar masses')


      return
      end
c-----------------------------------------------------------------------
       subroutine rdhd(in)

c       include 'header.h'
       include 'clstats.h'

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
      subroutine readata(c,t,m,imax,ncmax)
      implicit none
c-----------------------------------------------------------------------
c  read in the data and prepare arrays for calculating statistics
c-----------------------------------------------------------------------
      include 'clstats.h'
c      include 'header.h'

      integer nc,imax,nclump,ncmax
      integer i,j,k,i1,lenline
      real t0,tmp(ncl)
      real buff1(maxdim),buff2(maxdim)
      logical mask(maxdim)
      logical newclump
      character line*80

      integer c(nx,ny,nv)
      real t(nx,ny,nv)
      logical m(nx,ny,nv)

      do nc=1,ncl
         clump(nc)=0
         tmp(nc)=-999.9
         tsum(nc)=0.0
         npix(nc)=0
         xsum(nc)=0.0
         xsq(nc)=0.0
         ysum(nc)=0.0
         ysq(nc)=0.0
         vsum(nc)=0.0
         vsq(nc)=0.0
         edge(nc)=.false.
      enddo

c.....nclump = true clump number from clfind assign cube
c.....nc = indexed clump number running from 1 to imax
      imax=0
      ncmax=-1

      do k=1,nv
        call xysetpl(lin1,1,k)
        call xysetpl(lin2,1,k)
        do j=1,ny
          call xyread(lin1,j,buff1)
          call xyflgrd(lin1,j,mask)
          call xyread(lin2,j,buff2)
          do i=1,nx

            m(i,j,k)=mask(i)
            if(mask(i)) then
              t(i,j,k)=buff1(i)
              nclump=nint(buff2(i))

c.............bad pixels in assignment data cube
              if(nclump.lt.16796 .and. nclump.gt.0) then
                if(nclump.gt.ncmax) ncmax=nclump

c...............Code the clump number to go from 1 to ncl
                if (imax.gt.0) then
                  newclump=.true.
	              do i1=1,imax
                    if(clump(i1).eq.nclump) newclump=.false.
                  enddo
                  if(newclump) then
                    imax=imax+1
                    clump(imax)=nclump
                  endif
                else
                  imax=1
                  clump(imax)=nclump
                endif

                if(imax.gt.ncl) then
                  call bug('i','More clumps than I can deal with')
                  write(line,160) nclump
 160              format('Error occured at clump number = ',i3)
                  call bug('i',line(1:lenline(line)))
                  call bug('f','Increase parameter ncl and recompile')
                endif

c...............Get clump index number
                nc=0
                do i1=1,imax
                  if(clump(i1).eq.nclump) nc=i1
                enddo
c...............The code is screwed up somewhere
                if(nc.eq.0) call bug('f','Something bad just happened!')

                t0=t(i,j,k)
                c(i,j,k)=nc

                npix(nc)=npix(nc)+1
                if (t0.gt.tmp(nc)) then
                  tmp(nc)=t0
                  xpeak(nc)=i
                  ypeak(nc)=j
                  vpeak(nc)=k
                endif
                tsum(nc)=tsum(nc)+t0
                xsum(nc)=xsum(nc)+i*t0
                ysum(nc)=ysum(nc)+j*t0
                vsum(nc)=vsum(nc)+k*t0
                xsq(nc)=xsq(nc)+i*i*t0
                ysq(nc)=ysq(nc)+j*j*t0
                vsq(nc)=vsq(nc)+k*k*t0

c.............End bad pixel loop
              endif

c...........End mask loop
            endif

c.........End array loops over i,j,k
          enddo
        enddo
      enddo

c.....find the projected area (i,j) of each clump
      do nc=1,imax
       tmp(nc)=0.0
      enddo

      do i=1,nx
      do j=1,ny

       do k=1,nv
        nc=c(i,j,k)
        if(nc.gt.0 .and. nc.lt.16796) tmp(nc)=1.0
       enddo

       do nc=1,imax
        npixarea(nc) = npixarea(nc) + nint(tmp(nc))
        tmp(nc)=0.0
       enddo

      enddo
      enddo

      return
      end
c-----------------------------------------------------------------------
      SUBROUTINE SOPCHR(STRING,CCELL,NCELLS)
C-----------------------------------------------------------------
C     Interpretes a character string into a set of sub-strings.
C     K. A. Marsh   1980.  Latest revision by LGM, 1985 March 5.
C-----------------------------------------------------------------
      CHARACTER*(*) STRING,CCELL(20)
      INTEGER NCELLS

      CHARACTER*28 UPPER,LOWER
      INTEGER I1(20),I2(20),I,J,L,IMAX,JMAX
      DATA UPPER/'ABCDEFGHIJKLMNOPQRSTUVWXYZ  '/
      DATA LOWER/'abcdefghijklmnopqrstuvwxyz=,'/
C-------------------------------------------------
C     Initialize CCELL to blanks.
      JMAX=LEN(CCELL(1))
      DO 50 I=1,20
      DO 50 J=1,JMAX
   50    CCELL(I)(J:J)=' '
C----------------------------------------------------------------------
C     Convert letters to upper case and purge any = signs.
      IMAX=LEN(STRING)
      J=0
      DO 100 I=1,IMAX
         L=INDEX(LOWER,STRING(I:I))
         IF(L.NE.0) STRING(I:I)=UPPER(L:L)
  100    CONTINUE
C----------------------------------------------------------------------
C Identify strings by the spaces ; ncells = the number of sub-strings
      DO 200 I=1,IMAX
      IF(STRING(I:I).EQ.' ') GO TO 200
      IF(I.EQ.1) GO TO 120
      IF(STRING(I-1:I-1).NE.' ') GO TO 130
  120 J=J+1
      I1(J)=I
  130 IF(STRING(I+1:I+1).NE.' ') GO TO 200
      I2(J)=I
  200 CONTINUE
      NCELLS=J
      IF(NCELLS.EQ.0) RETURN
C-------------------------------------------------------------------------
C     Translate each sub-string into its own location in CHAR.
      DO 300 J=1,NCELLS
         WRITE(CCELL(J),5001) STRING(I1(J):I2(J))
  300    CONTINUE
C-------------------------------------------------------------------------
      RETURN
 5001 FORMAT(A)
C*****************************************************************
C************* END OF SOPCHAR ************************************
C*****************************************************************
      END
