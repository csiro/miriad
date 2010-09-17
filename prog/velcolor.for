      program velcolor

c= VELCOLOR - Make a red-green-blue image to display velocity as color.
c& mchw
c: image analysis
c+
c       VELCOLOR makes a 3-plane Miriad image to display the 3rd axis of
c       a 3-d Miriad image as color.  The third axis can be represented
c       by color by superposing the 3 planes as red, green and blue
c       images.  The most obvious use is to display the velocity axis as
c       color.  The algorithm weights the channels so that the apparent
c       intensity of the superposed red-green-blue image planes is
c       independent of color.  (For more details see Heiles & Jenkins,
c       1976, A&A 46,33)
c@ in
c       The input image. No default.
c@ region
c       The region of the input image to be used.  The 3rd axis region
c       determines the range from blue to red.  See documentation on
c       region for help of how to specify this.  Only the bounding box
c       is supported.
c@ pivot
c       Center channel of input image for output green image.
c       Default is 0.4*trc+0.6*blc
c@ out
c       The output red-green-blue image. No default.
c@ clip
c       Two values.  Exclude pixels with values in the range clip(1) to
c       clip(2).  If only one value is given, then exclude -abs(clip) to
c       abs(clip).
c
c$Id: velcolor.for,v 1.2 2010/09/17 06:56:29 cal103 Exp $
c--
c  History:
c    06aug92 mchw  Adapted from RALINT task.
c    16sep92 mchw  Added pivot and cleaned up code.  Stubs for other
c                  axes.
c    27feb93 mjs   use tmpdim.h instead of maxdim.h
c    02jul97 rjs   cellscal change.
c    23jul97 rjs   added pbtype.
c-----------------------------------------------------------------------
      include 'tmpdim.h'

      character version*(*)
      parameter (version='Version 1.0 16-Sep-92')

      integer   MAXNAX, MAXBOXES, MAXRUNS
      parameter (MAXNAX=3, MAXBOXES=2048, MAXRUNS=3*MAXDIM)

      integer boxes(MAXBOXES), naxis, axis
      integer i,j,lin,lout,nsize(MAXNAX),blc(MAXNAX),trc(MAXNAX)
      integer size(MAXNAX)
      real blo,bhi,clip(2),pivot
      character in*64,out*64,line*72

c     External.
      logical keyprsnt
c-----------------------------------------------------------------------
c
c Get inputs.
c
      call output('VELCOLOR: '//version)
      call keyini
      call keya('in',in,' ')
      call BoxInput('region',in,boxes,MAXBOXES)
      call keya('out',out,' ')
      call keyr('pivot',pivot,0.0)
      call keyr('clip',clip(1),0.0)
      if (keyprsnt('clip')) then
        call keyr('clip',clip(2),0.0)
      else
        clip(2) = abs(clip(1))
        clip(1) = -clip(2)
      endif
      call keyfin
      axis = 3
c
c Check inputs.
c
      if (in.eq.' ') call bug('f','No input specified. (in=)')
      if (out.eq.' ') call bug('f','No output specified. (out=)')
      if (clip(2).lt.clip(1)) call bug('f','clip range out of order')
      call xyopen(lin,in,'old',MAXNAX,nsize)
      call rdhdi(lin,'naxis',naxis,0)
      naxis = min(naxis,MAXNAX)
      if (nsize(1).gt.MAXDIM) call bug('f','Input file too big for me')
c
c  Determine the min and max image values.
c
      call ImMinMax(lIn,naxis,nsize,blo,bhi)
      if (blo.eq.bhi) then
        call xyclose(lIn)
        write(line,'(''All pixels are '',1pg10.3)') blo
        call output(line)
        stop
      endif
c
c  Set up the region of interest. Warn if not rectangle.
c
      call BoxSet(boxes,MAXNAX,nsize,'s')
      call BoxInfo(boxes,MAXNAX,blc,trc)
c
c  Open output image and fill in its header.
c
      j = 0
      do i = 1, naxis
        if (blc(i).lt.1) blc(i) = 1
        if (trc(i).gt.nsize(i)) trc(i) = nsize(i)
        if (i.ne.axis) then
          j = j + 1
          size(j) = trc(i) - blc(i) + 1
        endif
      enddo
      size(3) = 3
      call xyopen(lOut,out,'new',naxis,size)
      call header(lIn,lOut,naxis,blc,trc,axis,pivot)
c
c  Calculate the red-green-blue image planes.
c
      call velcolor3(lIn,lOut,naxis,blc,trc,clip,pivot)
c
c  Update history and close files.
c
      call Hisopen(lOut,'append')
      call HisWrite(lOut,'VELCOLOR: '//version)
      call HisInput(lOut,'VELCOLOR')
      call HisClose(lOut)
      call xyclose(lIn)
      call xyclose(lOut)

      end

c***********************************************************************

      subroutine header(lIn,lOut,naxis,blc,trc,axis,pivot)

      integer lin,lOut,naxis,blc(naxis),trc(naxis),axis
      real    pivot
c-----------------------------------------------------------------------
c  Copy keywords to output file.
c
c  Inputs:
c    lIn,lOut   Handle of input and output files.
c    naxis      The number of input axes.
c    blc,trc    The corners of the input image.
c    pivot      Center channel for green image.
c    axis       The axis for which the color is calculated.
c-----------------------------------------------------------------------
      integer nkeys, nckeys,i,j,k
      parameter (nkeys=23, nckeys=4)
      character keyw(nkeys)*9, ckeyw(nckeys)*5, itoaf*1, cin*1, cout*1
      character atemp*9,ctype*10
      real rtemp,cdelt,crval,crpix
      logical hdprsnt
c
c  Be careful that nkeys and nckeys match the number of keywords.
c
      data keyw/   'bmaj    ','bmin    ','bpa     ','bunit   ',
     *  'obstime ','epoch   ','history ','instrume',
     *  'ltype   ','lstart  ','lwidth  ','lstep   ','niters  ',
     *  'object  ','observer','obsra   ','obsdec  ','pbfwhm  ',
     *  'restfreq','telescop','vobs    ','cellscal','pbtype  '/
c
c  Keyword values that must be changed.
c
      data ckeyw/'ctype','cdelt','crval','crota'/
c-----------------------------------------------------------------------
c
c  Copy across unchanged header keywords.
c
      do i = 1, nkeys
        call hdcopy(lin,lout,keyw(i))
      enddo
c
c  Handle the keywords which must be moved to another axis.
c
      j = 0
      do i = 1, naxis
        if (i.ne.axis) then
          j = j + 1
          cin = itoaf(i)
          cout = itoaf(j)
          atemp = ckeyw(1)//cin
          if (hdprsnt(lin,atemp)) then
            call rdhda(lin,ckeyw(1)//cin,atemp,' ')
            call wrhda(lout,ckeyw(1)//cout,atemp)
          endif
          do k = 2, nckeys
            atemp = ckeyw(k)//cin
            if (hdprsnt(lin,atemp)) then
              call rdhdr(lin,ckeyw(k)//cin,rtemp,0.0)
              call wrhdr(lout,ckeyw(k)//cout,rtemp)
            endif
          enddo
c
c  Special cases: the crpixes will change if the user uses a subcube.
c
          if (hdprsnt(lin,'crpix'//cin)) then
            call rdhdr(lin,'crpix'//cin,rtemp,0.0)
            rtemp = rtemp - real(blc(i)) + 1
            call wrhdr(lout,'crpix'//cout,rtemp)
          endif
        endif
      enddo
c
c  Write out third axis.
c
      cin = itoaf(axis)
      call rdhda(lin,'ctype'//cin, ctype, ' ')
      call rdhdr(lin,'crpix'//cin, crpix, 1.0)
      call rdhdr(lin,'crval'//cin, crval, 0.0)
      call rdhdr(lin,'cdelt'//cin, cdelt, 1.0)

      if (pivot.eq.0.) pivot=0.4*trc(axis)+0.6*blc(axis)
      call wrhda(lout,'ctype3', ctype)
      call wrhdr(lout,'crpix3', 2.0)
      call wrhdr(lout,'crval3', crval+cdelt*(pivot-crpix))
      call wrhdr(lout,'cdelt3', cdelt*(pivot-blc(axis)))

      end

c***********************************************************************

      subroutine velcolor3(lIn,lOut,naxis,blc,trc,clip,pivot)

      integer lIn,lOut,naxis,blc(naxis),trc(naxis)
      real   clip(2),pivot
c-----------------------------------------------------------------------
c  Calculate the velcolor for axis 3.
c
c  Inputs:
c    lIn,lOut   Handle of input and output files.
c    naxis      The number of input axes.
c    blc,trc    The corners of the input image.
c    clip       Pixels with values in range clip(1:2) are excluded.
c    pivot      Center channel for green image.
c-----------------------------------------------------------------------
      include 'tmpdim.h'

      logical flags(MAXDIM)
      integer i, j, k, n
      real    col(3), buf(MAXDIM), out(MAXDIM,MAXDIM,3)
c-----------------------------------------------------------------------
c
c  Initialize the output array.
c
      do n = 1, 3
        do j = 1, MAXDIM
          do i = i, MAXDIM
            out(i,j,n) = 0.0
          enddo
        enddo
      enddo
c
c  Loop through the velocity channels and accumulate the colors.
c
      do k = blc(3), trc(3)
        col(1) = max(0.0,(pivot-k)/(pivot-blc(3)))
        col(3) = max(0.0,(k-pivot)/(trc(3)-pivot))
        col(2) = min(1.0-col(1),1.0-col(3))
        call xysetpl(lIn,1,k)
        do j = blc(2), trc(2)
          call xyread(lIn,j,buf)
          call xyflgrd(lIn,j,flags)
          do i = blc(1), trc(1)
            if (flags(i) .and.
     *          (buf(i).le.clip(1) .or. buf(i).ge.clip(2))) then
              do n = 1, 3
                out(i,j,n) = out(i,j,n) + col(n)*buf(i)
              enddo
            endif
          enddo
        enddo
      enddo
c
c  Now write out the color map.
c
      do n = 3,1,-1
        call xysetpl(lOut,1,n)
        do j = blc(2), trc(2)
          call xywrite(lOut,j-blc(2)+1,out(blc(1),j,n))
        enddo
      enddo

      end
