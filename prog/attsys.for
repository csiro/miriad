c************************************************************************
	program attsys
c
	implicit none
c
c= attsys - Apply or un-apply or redo on-line Tsys values.
c& rjs
c: uv analysis
c+
c	ATTSYS can apply or remove the Tsys weighting from correlation
c	data and can reapply Tsys based on a specified IF.
c
c	NOTE: If you are using two IF bands when observing, ATTSYS cannot
c	be used after you have split or copied the file down to single-IF
c	datasets.
c@ vis
c	The names of the input uv data sets. No default.
c@ out
c	The name of the output uv data set. No default.
c@ tsysif
c       The IF numbers used to provide the Tsys correction values.
c       Multiple values can be specified, one for each spectral window.
c       E.g., 1,1,3,3 will overwrite the 2nd and 4th set of tsys values
c       with the 1st and 3rd. This parameter is only used with the redo
c       option. Default value is 1, which applies the tsys for the 1st
c       IF to all following IFs.
c@ options
c	Extra processing options. Several options can be given,
c	separated by commas. Minimum match is supported. Possible values
c	are:
c	  apply     Apply the Tsys correction to the data. This is the
c	            default.
c	  unapply   Undo the Tsys correction in the data. Note that you
c	            cannot use the "apply" and "unapply" options
c	            simultaneously.
c	  auto      Use the "tcorr" variable to determine whether Tsys
c	            has been applied or not.
c	            NOTE: Information needed for options=auto is lost if
c	            you copy or split a dataset. If you are going to use 
c	            options=auto, you generally have to do it on the file
c	            resulting from atlod.
c         redo      Remove the existing Tsys correction from all IFs and
c                   reapply the Tsys from the IFs specified in tsysif.
c                   This can be used for certain CABB observations where
c                   the zoom bands have no valid Tsys information.
c                   This option cannot be combined with any others.
c $Id: attsys.for,v 1.3 2011/11/25 03:41:59 wie017 Exp $
c--
c  History:
c    17jul00 rjs  Original version.
c    25may02 rjs  Added options=auto
c    20jul11 mhw  Incorporate tsysfix program by jra
c    25nov11 mhw  Make tsysif an array and update tsys variables
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*80
	integer lVis,lOut,vupd,pol,npol,i1,i2,i,j,k
	logical updated,doapply,auto,redo,update
	character vis*64,out*64,type*1
	integer nschan(MAXWIN),nif,nchan,nants,length,tcorr,na
        integer tsysif(MAXWIN),n
	real xtsys(MAXANT*MAXWIN),ytsys(MAXANT*MAXWIN)
        real nxtsys(MAXANT*MAXWIN),nytsys(MAXANT*MAXWIN)
        real systemp(MAXANT*MAXWIN)
	complex data(MAXCHAN)
	logical flags(MAXCHAN),first
	double precision preamble(5)
c
c  Externals.
c
	logical uvvarUpd
        character versan*80
c
	version = versan('attsys',
     *                   '$Revision: 1.3 $',
     *                   '$Date: 2011/11/25 03:41:59 $')
	call keyini
	call keya('vis',vis,' ')
	call keya('out',out,' ')
        call mkeyi('tsysif',tsysif,MAXWIN,n)
        if (n.eq.0) then
          tsysif(1)=1
          n=1
        endif
	call GetOpt(doapply,auto,redo)
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
c
c  Get ready to copy the data.
c
	call uvopen(lVis,vis,'old')
	call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varInit(lVis,'channel')
c
	call uvvarIni(lVis,vupd)
	call uvvarSet(vupd,'xtsys')
	call uvvarSet(vupd,'ytsys')
	call uvvarSet(vupd,'nschan')
c
	call uvopen(lOut,out,'new')
	call varOnit(lVis,lOut,'channel')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
c  Make the output history.
c
	call hdcopy(lVis,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'ATTSYS: Miriad '//version)
	call hisinput(lOut,'ATTSYS')
	call hisclose(lOut)
c
c  Get first record.
c
	call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
c
c  If auto mode has been requested, check that the "tcorr" variable
c  is present.
c
	if(auto)then
	  call uvprobvr(lVis,'tcorr',type,length,updated)
	  if(length.ne.1)call bug('f',
     *		'Required info for options=auto is missing')
	endif
	call uvrdvri(lVis,'nants',na,0)
        first=.true.
c
	dowhile(nchan.gt.0)
          update=.false.
	  call uvrdvri(lVis,'pol',pol,0)
	  call uvrdvri(lVis,'npol',npol,0)
c
	  if(uvvarUpd(vupd))then
	    call uvprobvr(lVis,'nschan',type,length,updated)
	    nif = length
            if (first.and.redo) then
              first=.false.
              n=min(n,nif)
              do i=1,n
                if (tsysif(i).lt.1.or.tsysif(i).gt.nif)
     *	          call bug('f','Invalid tsysif parameter')
              enddo	
              if (n.lt.nif) then
                do i=n+1,nif
                  tsysif(i)=tsysif(n)
                enddo
              endif
            endif
	    if(type.ne.'i'.or.length.le.0.or.length.gt.MAXWIN)
     *	      call bug('f','Invalid nschan parameter')
	    call uvgetvri(lVis,'nschan',nschan,nif)
c
	    call uvprobvr(lVis,'xtsys',type,length,updated)
	    nants = length/nif
	    if(nants*nif.ne.length.or.nants.le.0.or.nants.gt.MAXANT
     *	      .or.type.ne.'r')call bug('f','Invalid tsys parameter')
	    if(na.ne.nants)
     *		call bug('f','Inconsistency in number of IFs')
	    call uvgetvrr(lVis,'xtsys',xtsys,nants*nif)
	    call uvprobvr(lVis,'ytsys',type,length,updated)
	    if(nants*nif.ne.length.or.type.ne.'r')
     *			      call bug('f','Invalid ytsys parameter')
	    call uvgetvrr(lVis,'ytsys',ytsys,nants*nif)
            update=.true.
	  endif
c
	  call basant(preamble(5),i1,i2)
	  if(auto)then
	    if(doapply)then
	      call uvrdvri(lVis,'tcorr',tcorr,0)
	    else
	      call uvrdvri(lVis,'tcorr',tcorr,1)
	    endif
	    if(doapply.eqv.(tcorr.eq.0))call tsysap(data,nchan,nschan,
     *		xtsys,ytsys,nants,nif,doapply,redo,i1,i2,pol,1)
	  else
	    call tsysap(data,nchan,nschan,xtsys,ytsys,nants,nif,
     *			doapply,redo,i1,i2,pol,tsysif)
	  endif
c
	  call varCopy(lVis,lOut)
	  if(npol.gt.0)then
	    call uvputvri(lOut,'npol',npol,1)
	    call uvputvri(lOut,'pol',pol,1)
	  endif
          if (redo.and.update) then
            k=0
            do i=1,nif
              do j=1,nants 
                k=k+1
                nxtsys(k)=xtsys(j+(tsysif(i)-1)*nants)
                nytsys(k)=ytsys(j+(tsysif(i)-1)*nants)
                systemp(k)=sqrt(nxtsys(k)*nytsys(k))
              enddo
            enddo
            call uvputvrr(lOut,'xtsys',nxtsys,nants*nif)
            call uvputvrr(lOut,'ytsys',nytsys,nants*nif)
            call uvputvrr(lOut,'systemp',systemp,nants*nif)      
          endif
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call uvclose(lVis)
	call uvclose(lOut)
	end
c************************************************************************
	subroutine tsysap(data,nchan,nschan,xtsys,ytsys,nants,nif,
     *					doapply,redo,i1,i2,pol,tsysif)
c
	implicit none
	integer nchan,nants,nif,tsysif,nschan(nif),i1,i2,pol
	real xtsys(nants,nif),ytsys(nants,nif)
	logical doapply,redo
	complex data(nchan)
c
c------------------------------------------------------------------------
	integer XX,YY,XY,YX
	parameter(XX=-5,YY=-6,XY=-7,YX=-8)
	integer i,j,k
	real T1T2,new_T1T2
c
	i = 0
	do k=1,nif
	  if(i+nschan(k).gt.nchan)call bug('f','Invalid description')
	  do j=1,nschan(k)
	    i = i + 1
	    if(pol.eq.XX)then
	      T1T2 = xtsys(i1,k)*xtsys(i2,k)
	      new_T1T2 = xtsys(i1,tsysif)*xtsys(i2,tsysif)
	    else if(pol.eq.YY)then
	      T1T2 = ytsys(i1,k)*ytsys(i2,k)
              new_T1T2 = ytsys(i1,tsysif)*ytsys(i2,tsysif)
	    else if(pol.eq.XY)then
              T1T2 = xtsys(i1,k)*ytsys(i2,k)
              new_T1T2 = xtsys(i1,tsysif)*ytsys(i2,tsysif)
	    else if(pol.eq.YX)then
	      T1T2 = ytsys(i1,k)*xtsys(i2,k)
              new_T1T2 = ytsys(i1,tsysif)*xtsys(i2,tsysif)
	    else
	      call bug('f','Invalid polarization code')
	    endif
c
	    if (redo) then
	     data(i) = data(i)*sqrt(new_T1T2/T1T2)         
            else if(doapply)then
	      data(i) = data(i)*sqrt(T1T2)/50.0
	    else
	      data(i) = data(i)*50.0/sqrt(T1T2)
	    endif
	  enddo
	enddo
c
	end
c************************************************************************
	subroutine getopt(doapply,auto,redo)
c
	implicit none
	logical doapply,auto,redo
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=4)
	character opts(NOPTS)*10
	logical present(NOPTS)
c
	data opts/'apply     ','unapply   ','automatic ','redo      '/
c
	call options('options',opts,present,NOPTS)
	if(present(1).and.present(2))call bug('f',
     *	  'Cannot both apply and unapply Tsys correction')
	doapply = .not.present(2)
	auto    = present(3)
        redo = present(4)
        if (present(4).and.(present(1).or.present(2).or.present(3)))
     *    call bug('f','Option redo must be used on its own')
c
	end
