************************************************************************
c
c* Fndaxnum - Find the axis of a certain type
c& bpw
c: utilities
c+
      subroutine fndaxnum( tinp, type, axisname, axisnr )
      
      integer          tinp
      character*(*)    type
      character*1      axisname
      integer          axisnr

c This figures out which axis of a dataset is the 'lon', 'lat', 'freq'
c or 'stokes' axis. The output variables axisname and axisnr will
c contain the name and number of this axis.
c
c 'lon' axes are all axes for which the ctype keyword starts with
c 'ra', 'lon' or 'glon'. On return axisname='x', axisnr=1.
c 'lat' axes are all axes for which the ctype keyword starts with
c 'dec', 'lat' or 'glat'. On return axisname='y', axisnr=2.
c 'freq' axes are all axes for which the ctype keyword starts with
c 'velo', 'felo' or 'freq'. On return axisname='z', axisnr=3.
c 'stokes' axes are all axes for which the ctype keyword starts with
c 'stok'. On return axisname='a', axisnr=4.
c For example, requesting to know which axis is the 'lon' axis for a
c vel-ra-dec gives as result axisname='y' and axisnr=2.
c
c The 'type' input variable can also have one of the values 'x', 'y',
c 'z', 'a' ... . In that case fndaxnum checks whether that axis is
c present in the dataset, and converts it to an axisnr. axisname is set
c equal to type on output.
c
c axisname is both input and output.
c On input it gives the default axisname to return if the axis of the
c specified type is not found. I.e.: if the axis is not found, it is
c not changed. The only allowed inputs are ' ', 'x', 'y', ... ,'d'.
c If the axis-type is found, axisname and axisnr are used to return
c the result.
c
c Input:
c    tinp        Handle of dataset to check
c    type        Axis to check (possible values are 'lon', 'lat',
c                 'freq' and 'stokes')
c    axisname    On input: default axisname if axis not found
c    axisnr      On input: default axisnr if axis not found
c
c Output:
c    axisname    On output: name of the axis found ( 'x', 'y', ... )
c    axisnr      On output: Index of the axis found ( 1, 2, ... )
c--
c
c  History:
c    20jul91  bpw   Installed
c    13apr92  bpw   Changed call to assert into assertl
c    15dec92  bpw   Make axisname input too
c
c------------------------------------------------------------------------
      integer          i, j, k, len1
      character*80     line
      
      character*20     ltype
      logical          match
      character*80     msg
      integer          nr
      character*9      key, ctype
      character*1      itoaf
      character*4      substr, test

      integer          naxis
      integer          axnum
      integer          NTYPES
      parameter        ( NTYPES = 4 )
      character*20     axtypes
      character*15     valid( NTYPES )

      character*7      axisnames
      data             axisnames / 'xyzabcd' /

      data             axtypes   / 'lon,lat,freq,stokes' /
      data             valid     / 'ra,lon,glon',
     *                             'dec,lat,glat',
     *                             'velo,felo,freq',
     *                             'stokes'              /
      
      call rdhdi( tinp, 'naxis', naxis, 0 )
      call assertl( naxis.ne.0, 'No axes in dataset header' )

      ltype = type
      call lcase( ltype )

      i = index( axisnames, ltype(1:1) )
      if( len1(ltype).eq.1  .and.  i.ne.0 ) then

         line = 'fndaxnum: '//ltype(:1)//' axis does not exist in image'
         call assertl( i.le.naxis, line )
         axnum = i

      else

         msg = 'fndaxnum: Cannot search for axistype ' // type
         call assertl( match( ltype,axtypes,nr ), msg )
 
         axnum = 0
         do i = 1, naxis
            key = 'ctype' // itoaf(i)
            call rdhda( tinp, key, ctype, ' ' )
            call lcase( ctype )
            j = 0
            do while( .true. )
               j = j + 1
               test = substr( valid(nr), j )
               if( test.eq.' ' ) goto 2
               k = len1(test)
               if( ctype(:k) .eq. test(:k) ) then
                  axnum = i
                  goto 3
               endif
            enddo
    2       continue
         enddo
    3    continue

      endif

      if( axnum .ne. 0 ) then
         axisname = axisnames( axnum:axnum )
         axisnr   = axnum
      else
         i = index(axisnames,axisname)
         line = 'fndaxnum: illegal default axis name: '//axisname
         call assertl( i.ne.0 .or. len1(axisname).eq.0, line )
         axisnr = i
      endif

      return

      end
