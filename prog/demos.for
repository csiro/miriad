c***********************************************************************
      program demos

c= demos - Inverse mosaicing operation
c& rjs
c: map manipulation
c+
c       DEMOS (de-mosaic) is a MIRIAD task that takes a model of the sky
c       (that is primary beam corrected), and multiplies by a primary
c       beam response function at a number of different pointing
c       centers.  It produces a different output for each pointing
c       center.  Thus this task performs the inverse of mosaicing.  The
c       input pointing centers and the primary beam size are indirectly
c       specified by a visibility dataset.
c
c       Because the output of DEMOS have primary beams applied, they can
c       be used for comparison with visibility data and uncorrected
c       images.  In particular SELFCAL cannot handle a model that is
c       primary beam corrected, though it can handle a visibility data
c       file containing multiple pointings.  Thus you could use DEMOS to
c       break the model into several models which are not primary beam
c       corrected.
c@ map
c       This is the name of the image to be de-mosaiced.  A primary beam
c       corrected image, single dish image, or one that is partially
c       primary beam corrected (e.g. the output of MOSMEM).  No default.
c@ vis
c       This is one or more input visibility datasets.  The pointing
c       centres and primary beams corresponding to the selected
c       visibilities are used in the de-mosaicing process.
c@ select
c       Normal uv selection. See help on "select".  Generally you will
c       select only those pointings of interest.
c@ out
c       This gives a template name for the output images.  The actual
c       output image names are formed by appending a number
c       corresponding to each pointing center to this output name.  For
c       example, if out=cygnus, then the output images will be called
c       cygnus1, cygnus2, etc.
c@ imsize
c       This gives two values, being the output image size, in x and y.
c       If no value is given, then the outputs will be one primary beam
c       width in size. If one value is given, then this is used for both
c       x and y. Each output size might be smaller than this, to prevent
c       each output from extending beyond the edges of the input image.
c@ options
c       Extra processing options. There is currently only one option.
c         detaper    This indicates that the input image is not fully
c                    primary beam corrected. Such images are formed by
c                    LINMOS with options=taper or by MOSMEM.
c
c$Id: demos.for,v 1.2 2010/08/25 02:17:21 cal103 Exp $
c--
c  History:
c    rjs  25apr90 Original version.
c    rjs  30apr90 Changed it so that the reference pixels of the output
c                 maps is the pointing centre.
c    mchw 09nov90 Added bmaj bmin bpa and pbfwhm to map header.
c    mjs  25feb91 Changed references of itoa to itoaf.
c    pjt  16oct91 Changed to double precision coordinate crval/cdelt/
c                 crpix.
c    nebk 12nov92 Adapt for new primary beam routines and do blanking.
c    nebk 25nov92 Copy btype to output
c    nebk 17dec92 Adapt to new FNDAXNUM
c    nebk 28jan93 New primary beam interface.
c    mchw 12feb93 Convert uvvariables ra and dec to double precision.
c    rjs  26aug94 Rework to use new co routines.
c    rjs  24oct94 Use new pb routines.
c    rjs  30jan95 Write mosaic table with the output image.
c    rjs   3feb95 options=detaper. Better uv handling. Get rid of pbtype
c                 and center keywords.
c    rjs  27feb95 Correct sign error in the sign of an offset.  Allow
c                 multiple input vis datasets.
c    rjs  17may95 More messages.
c    rjs  02jul97 cellscal change.
c    rjs  07jul97 Change coaxdesc to coaxget.
c    rjs  17sep97 Doc change only.
c    rjs  25sep98 Less fussy about freq axis for 1-plane files.
c-----------------------------------------------------------------------
      character version*(*)
      parameter (version='version 25-Sep-98')
      integer MAXSELS,MAXPNT,MAXVIS
      parameter (MAXSELS=256,MAXPNT=2048,MAXVIS=128)
      include 'maxdim.h'
      include 'mirconst.h'

      character map*64,vis(MAXVIS)*64,out*64,name*64
      character pbtype(MAXPNT)*16
      integer imsize(2),nsize(3),npnt,lout,i,tmap,iax,nvis
      logical detaper
      real sels(MAXSELS)
      double precision ra(MAXPNT),dec(MAXPNT)

      integer   len1
      character itoaf*3
      external  itoaf, len1
c-----------------------------------------------------------------------
c
c  Get the input parameters.
c
      call output('Demos: '//version)
      call keyini
      call keya('map',map,' ')
      call mkeyf('vis',vis,MAXVIS,nvis)
      call keya('out',out,' ')
      call keyi('imsize',imsize(1),0)
      call keyi('imsize',imsize(2),imsize(1))
      call SelInput('select',sels,MAXSELS)
      call GetOpt(detaper)
      call keyfin
c
c  Check that the input parameters are reasonable.
c
      if (map.eq.' ')
     *  call bug('f','An input map must be given')
      if (out.eq.' ')
     *  call bug('f','An output template name must be given')
      if (nvis.eq.0)
     *  call bug('f','A visibility dataset must be given')
c
c  Open the input map.
c
      call xyopen(tmap,map,'old',3,nsize)
      call coInit(tmap)
      if (max(nsize(1),nsize(2)).gt.maxdim)
     *  call bug('f','Input map too big for me to handle')
c
c  Some checks for primary beam stuff; convert user value to radians
c
      call coFindAx(tmap,'longitude',iax)
      if (iax.ne.1)call bug('f','RA axis must be the first axis')
      call coFindAx(tmap,'latitude',iax)
      if (iax.ne.2)call bug('f','DEC axis must be the second axis')
      call coFindAx(tmap,'spectral',iax)
      if (iax.gt.0 .and. iax.ne.3 .and. nsize(3).gt.1) call bug('f',
     *  'The spectral axis of this image must be number 3')
c
c  Get the pointing centres, etc, associated with the vis dataset.
c
      call GetPnt(vis,nvis,sels,MAXPNT,npnt,ra,dec,pbtype)
      call output('Number of pointings: '//itoaf(npnt))
c
c  Process each of the pointings.
c
      lout = len1(out)
      do i = 1, npnt
        name = out(1:lout)//itoaf(i)
        call output('Image '//itoaf(i)//' used primary beam type '
     *        //pbtype(i))
        call Process(tmap,pbtype(i),ra(i),dec(i),
     *                        name,nsize,imsize,version,detaper)
      enddo

      call xyclose(tmap)

      end
c***********************************************************************
      subroutine GetPnt(vis,nvis,sels,MAXPNT,npnt,ra,dec,pbtype)

      integer npnt,MAXPNT,nvis
      character vis(nvis)*(*)
      real sels(*)
      double precision ra(MAXPNT),dec(MAXPNT)
      character pbtype(MAXPNT)*(*)

c  Get the pointing centers and primary beam types. The "mos" routines
c  do the real work.
c-----------------------------------------------------------------------
      integer nread,ipnt,length,tvis,ivis
      double precision preamble(4)
      complex data
      real rms
      character type*1
      logical flag,update
c-----------------------------------------------------------------------
c
c  Open the visibility dataset, select the appropriate data, and
c  get it to return just the first channel.
c
      call mosCIni
      do ivis = 1, nvis
        call uvopen(tvis,vis(ivis),'old')
        call SelApply(tvis,sels,.true.)
c
c  Just read the first channel, to avoid unecessary work.
c
        call uvprobvr(tvis,'corr',type,length,update)
        if (type.ne.' ') then
          call uvset(tvis,'data','channel',1,1.,1.,1.)
        else
          call uvset(tvis,'data','wide',   1,1.,1.,1.)
        endif

        npnt = 0
        call uvread(tvis,preamble,data,flag,1,nread)
        do while (nread.ne.0)
          call mosChk(tvis,ipnt)
          npnt = max(npnt,ipnt)
          if (npnt.gt.MAXPNT)
     *        call bug('f','Too many pointings for me')
          call uvread(tvis,preamble,data,flag,1,nread)
        enddo

        call uvclose(tvis)
        call mosCDone(tvis)
      enddo
c
c  Now get all the information from the pointing table.
c
      do ipnt = 1, npnt
        call mosGet(ipnt,ra(ipnt),dec(ipnt),rms,pbtype(ipnt))
      enddo

      end
c***********************************************************************
      subroutine GetOpt(detaper)

      logical detaper

c  Return processing options.
c
c-----------------------------------------------------------------------
      integer NOPTS
      parameter (NOPTS=1)
      logical present(NOPTS)
      character opts(NOPTS)*8
      data opts/'detaper '/
c-----------------------------------------------------------------------
      call options('options',opts,present,NOPTS)
      detaper = present(1)

      end
c***********************************************************************
      subroutine Process(tmap,pbtype,ra,dec,name,insize,outsize,
     *                                            version,detaper)

      character name*(*),version*(*),pbtype*(*)
      integer tmap,insize(3),outsize(2)
      double precision ra,dec
      logical detaper

c  This is the main processing routine in DEMOS. It reads a part of the
c  input map, applies the primary beam, and writes it out. It also
c  processes the history and header bull.
c
c  Inputs:
c    tmap       Handle of the input cube.
c    name       Name of the output cube.
c    insize     Size of the input image.
c    outsize    Max size of the output image.
c    version    A version id, for the history file.
c-----------------------------------------------------------------------
      include 'maxdim.h'
      include 'maxnax.h'
      include 'mirconst.h'
      include 'mem.h'

      integer tout,naxis,pbObj,pScr,pWts,npnt
      integer nsize(MAXNAX),i,k,n1,n2,n3,x1,x2,y1,y2
      real x0,y0,rms
      double precision crpix1,crpix2,xin(3),xout(3)
      character line*64
c
c  Header keywords.
c
      integer nkeys
      parameter (nkeys=39)
      character keyw(nkeys)*8

      data keyw/   'bunit   ','btype   ',
     *  'cdelt1  ','cdelt2  ','cdelt3  ','cdelt4  ','cdelt5  ',
     *                        'crpix3  ','crpix4  ','crpix5  ',
     *  'crval1  ','crval2  ','crval3  ','crval4  ','crval5  ',
     *  'ctype1  ','ctype2  ','ctype3  ','ctype4  ','ctype5  ',
     *  'epoch   ','history ','niters  ','object  ','telescop',
     *  'observer','restfreq','vobs    ','obsra   ','obsdec  ',
     *  'obstime ','lstart  ','lstep   ','ltype   ','lwidth  ',
     *  'bmaj    ','bmin    ','bpa     ','cellscal'/
c-----------------------------------------------------------------------
      n3 = insize(3)
      nsize(3) = n3
c
c  Determine the output map size.
c
      n1 = outsize(1)
      n2 = outsize(2)
      if (n1.le.0 .or. n2.le.0)call defsiz(tmap,pbtype,n1,n2)
c
c  Determine the field extent in pixels.
c
      xin(1) = ra
      xin(2) = dec
      xin(3) = 1
      call coCvt(tmap,'aw/aw/ap',xin,'ap/ap',xout)
      x0 = xout(1)
      y0 = xout(2)

      x1 = nint(x0 - 0.5*n1)
      x2 = x1 + n1 - 1
      x1 = max(x1,1)
      x2 = min(x2,insize(1))

      y1 = nint(y0 - 0.5*n2)
      y2 = y1 + n2 - 1
      y1 = max(y1,1)
      y2 = min(y2,insize(2))
c
c  Check if the output file is of some size!
c
      if (x2.lt.x1 .or. y2.lt.y1) then
        line = 'Unable to form output dataset '//name
        call bug('w',line)
        call bug('w','... pointing does not overlap with input image')
        return
      endif
c
c Open the output file.
c
      call rdhdi(tmap,'naxis',naxis,1)
      naxis = min(naxis,MAXNAX)
      nsize(1) = x2 - x1 + 1
      nsize(2) = y2 - y1 + 1
      nsize(3) = n3
      do i = 4, naxis
        nsize(i) = 1
      enddo
      call xyopen(tout,name,'new',naxis,nsize)
c
c  Process its header.
c
      call coCvt1(tmap,1,'op',0d0,'ap',crpix1)
      call coCvt1(tmap,2,'op',0d0,'ap',crpix2)

      crpix1 = crpix1 - x1 + 1
      crpix2 = crpix2 - y1 + 1

      call wrhdd(tOut,'crpix1',crpix1)
      call wrhdd(tOut,'crpix2',crpix2)

      do i = 1, nkeys
        call hdcopy(tMap,tOut,keyw(i))
      enddo
      call pbWrite(tOut,pbtype)
c
c  Create output mosaic table.
c
      call rdhdr(tMap,'rms',rms,0.)
      if (rms.le.0)rms = 1

      call mosInit(outsize(1),outsize(2))
      call mosSet(1,ra,dec,rms,pbtype)
      call mosSave(tOut)
c
c  Write some history info.
c
      call hisopen(tOut,'append')
      line = 'DEMOS: Miriad DeMos '//version
      call hiswrite(tOut, line)
      call hisinput(tOut,'DEMOS')
      line = 'DEMOS: Primary beam used is '//pbtype
      call hiswrite(tOut,line)
      call hisclose(tOut)
c
c  Prepare to iterate.
c
      call memAlloc(pWts,nsize(1)*nsize(2),'r')
      if (detaper) then
        call memAlloc(pScr,nsize(1)*nsize(2),'r')
        call mosLoad(tMap,npnt)
      endif
c
c  Loop over all planes
c
      do k = 1, n3
        call xysetpl(tmap,1,k)
        call xysetpl(tOut,1,k)
c
c  Initialise the primary beam object.
c
        xin(1) = ra
        xin(2) = dec
        xin(3) = k
        call pbInitc(pbObj,pbtype,tmap,'aw/aw/ap',xin)

        if (detaper) then
          call mosMIni(tmap,real(k))
          call TapWts(pbObj,memr(pWts),memr(pScr),
     *                        x1-1,y1-1,nsize(1),nsize(2))
          call mosMFin
        else
          call PbWts(pbObj,memr(pWts),x1-1,y1-1,nsize(1),nsize(2))
        endif

        call pbFin(pbObj)
c
c  Now write out the data.
c
        call WrOut(tmap,tOut,memr(pWts),x1-1,y1-1,nsize(1),nsize(2))
      enddo
c
c  All said and done.
c
      call memFree(pWts,nsize(1)*nsize(2),'r')
      if (detaper)call memFree(pScr,nsize(1)*nsize(2),'r')
      call xyclose(tOut)

      end
c***********************************************************************
      subroutine TapWts(pbObj,Wts,Scr,xoff,yoff,nx,ny)

      integer pbObj,xoff,yoff,nx,ny
      real Wts(nx,ny),Scr(nx,ny)

c  Determine the primary beam weights.
c
c-----------------------------------------------------------------------
      integer i,j

      real pbGet
      external pbGet
c-----------------------------------------------------------------------
c
c  Get the de-tapering weights.
c
      call mosWts(Wts,Scr,nx,ny,-xoff,-yoff)

      do j = 1, ny
        do i = 1, nx
          Wts(i,j) = pbGet(pbObj,real(i+xoff),real(j+yoff)) * Wts(i,j)
        enddo
      enddo

      end
c***********************************************************************
      subroutine PbWts(pbObj,Wts,xoff,yoff,nx,ny)

      integer pbObj,xoff,yoff,nx,ny
      real Wts(nx,ny)

c  Determine the primary beam weights.
c
c-----------------------------------------------------------------------
      integer i,j

      real pbGet
      external pbGet
c-----------------------------------------------------------------------
      do j = 1, ny
        do i = 1, nx
          Wts(i,j) = pbGet(pbObj,real(i+xoff),real(j+yoff))
        enddo
      enddo

      end
c***********************************************************************
      subroutine WrOut(tIn,tOut,Wts,xoff,yoff,nx,ny)

      integer tIn,tOut,nx,ny,xoff,yoff
      real Wts(nx,ny)

c  Write out the weighted data.
c
c-----------------------------------------------------------------------
      include 'maxdim.h'
      real data(MAXDIM)
      logical flags(MAXDIM)
      integer i,j
c-----------------------------------------------------------------------
      do j = 1, ny
        call xyread(tIn,j+yoff,data)
        call xyflgrd(tIn,j+yoff,flags)
        do i = 1, nx
          data(i+xoff) = wts(i,j) * data(i+xoff)
          flags(i+xoff) = wts(i,j).gt.0 .and. flags(i+xoff)
        enddo
        call xywrite(tOut,j,data(1+xoff))
        call xyflgwr(tOut,j,flags(1+xoff))
      enddo

      end
c***********************************************************************
      subroutine defsiz(tmap,pbtype,n1,n2)

      integer tmap,n1,n2
      character pbtype*(*)

c  Set default size of image to one primary beam
c
c   Input
c    tmap       Handle of the input dataset.
c    pbtype     Primary beam type.
c  Output
c    n1,n2      Size of image
c
c-----------------------------------------------------------------------
      double precision cdelt,crval,crpix
      character ctype*16
      real pbfwhm,cutoff,maxrad
      integer pbObj
c-----------------------------------------------------------------------
      call pbInit(pbObj,pbtype,tmap)
      call pbInfo(pbObj,pbfwhm,cutoff,maxrad)

      call coAxGet(tmap,1,ctype,crpix,crval,cdelt)
      n1 = 2*nint(maxrad/abs(cdelt))
      call coAxGet(tmap,2,ctype,crpix,crval,cdelt)
      n2 = 2*nint(maxrad/abs(cdelt))

      call pbFin(pbObj)

      end
