c************************************************************************
	program attsys
c
	implicit none
c
c= attsys - Do various operations with Tsys values.
c& rjs
c: uv analysis
c+
c	ATTSYS can apply or remove the Tsys weighting from correlation
c	data, reapply Tsys based on a specified IF, or scale the
c       the Tsys values (leaving the data unchanged)
c
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
c@ factor
c       Scale factor to apply to the Tsys values recorded in the data.
c       This parameter is ignored except if options=scale.
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
c                   This option cannot be combined with the previous ones.
c         inverse   Apply the inverse correction for redo
c         scale     Scale the existing tsys values by the value given by
c                   the parameter factor (leaves data unchanged)
c         nocal     Do not apply the gains file. 
c         nopass    Do not apply bandpass corrections.
c         nopol     Do not apply polarization corrections. 
c
c $Id: attsys.for,v 1.10 2018/12/04 04:02:11 wie017 Exp $
c--
c  History:
c    17jul00 rjs  Original version.
c    25may02 rjs  Added options=auto
c    20jul11 mhw  Incorporate tsysfix program by jra
c    25nov11 mhw  Make tsysif an array and update tsys variables
c    12jan12 mhw  Fix array indexing and add inverse option
c    23feb12 mhw  Add scale option
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*72
	integer lVis,lOut,vupd,pol,npol,i1,i2,i,j,k
	logical updated,doapply,auto,redo,update,inv,scale
	character vis*64,out*64,type*1,uvflags*12
	integer nschan(MAXWIN),nif,nchan,nants,length,tcorr,na
        integer tsysif(MAXWIN),n,nst
	real xtsys(MAXANT*MAXWIN),ytsys(MAXANT*MAXWIN)
        real nxtsys(MAXANT*MAXWIN),nytsys(MAXANT*MAXWIN)
        real systemp(MAXANT*MAXWIN),factor
	complex data(MAXCHAN)
	logical flags(MAXCHAN),first
	double precision preamble(5)
c
c  Externals.
c
	logical uvvarUpd
        character versan*72
c
	version = versan('attsys',
     *                   '$Revision: 1.10 $',
     *                   '$Date: 2018/12/04 04:02:11 $')
	call keyini
	call GetOpt(uvflags,doapply,auto,redo,inv,scale)
	call uvDatInp('vis',uvflags)
	call keya('out',out,' ')
        call mkeyi('tsysif',tsysif,MAXWIN,n)
        if (n.eq.0) then
          tsysif(1)=1
          n=1
        endif
        call keyr('factor',factor,1.0)
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
c
c  Get ready to copy the data.
c
	call uvDatOpn(lVis)
c	call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varInit(lVis,'channel')
c
	call uvvarIni(lVis,vupd)
	call uvvarSet(vupd,'xtsys')
	call uvvarSet(vupd,'ytsys')
	call uvvarSet(vupd,'systemp')
	call uvvarSet(vupd,'nschan')
c
	call uvopen(lOut,out,'new')
	call varOnit(lVis,lOut,'channel')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.d0,0.d0,0.d0)
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
	call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
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
	  call uvDatGti('pol',pol)
	  call uvDatGti('npol',npol)
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
	    if(na.ne.nants) then
              if (.not.scale) then
                call bug('f','Inconsistency in number of IFs')
              endif
            endif
	    call uvgetvrr(lVis,'xtsys',xtsys,nants*nif)
	    call uvprobvr(lVis,'ytsys',type,length,updated)
	    if(nants*nif.ne.length.or.type.ne.'r')
     *			      call bug('f','Invalid ytsys parameter')
	    call uvgetvrr(lVis,'ytsys',ytsys,nants*nif)
            call uvprobvr(lVis,'systemp',type,nst,updated)
	    call uvgetvrr(lVis,'systemp',systemp,nst)
            
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
     *		xtsys,ytsys,nants,nif,doapply,redo,inv,i1,i2,pol,tsysif)
	  else if (.not.scale) then
	    call tsysap(data,nchan,nschan,xtsys,ytsys,nants,nif,
     *			doapply,redo,inv,i1,i2,pol,tsysif)
	  endif
c
	  call varCopy(lVis,lOut)
	  call uvputvri(lOut,'pol',pol,1)
	  call uvputvri(lOut,'npol',npol,1)
          if ((redo.or.scale).and.update) then
            k=0
            do i=1,nif
              do j=1,na 
                k=k+1
                if (redo) then
                  nxtsys(k)=xtsys(j+(tsysif(i)-1)*na)
                  nytsys(k)=ytsys(j+(tsysif(i)-1)*na)
                  systemp(k)=sqrt(nxtsys(k)*nytsys(k))
                else if (scale) then
                  systemp(k)=systemp(k)*factor
                endif
              enddo
            enddo
            call uvputvrr(lOut,'xtsys',nxtsys,na*nif)
            call uvputvrr(lOut,'ytsys',nytsys,na*nif)
            call uvputvrr(lOut,'systemp',systemp,nst)      
          endif
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvDatRd(preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call uvDatCls()
	call uvclose(lOut)
	end
c************************************************************************
	subroutine tsysap(data,nchan,nschan,xtsys,ytsys,nants,nif,
     *			doapply,redo,inv,i1,i2,pol,tsysif)
c
	implicit none
	integer nchan,nants,nif,tsysif(nif),nschan(nif),i1,i2,pol
	real xtsys(nants,nif),ytsys(nants,nif)
	logical doapply,redo,inv
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
	      new_T1T2 = xtsys(i1,tsysif(k))*xtsys(i2,tsysif(k))
	    else if(pol.eq.YY)then
	      T1T2 = ytsys(i1,k)*ytsys(i2,k)
              new_T1T2 = ytsys(i1,tsysif(k))*ytsys(i2,tsysif(k))
	    else if(pol.eq.XY)then
              T1T2 = xtsys(i1,k)*ytsys(i2,k)
              new_T1T2 = xtsys(i1,tsysif(k))*ytsys(i2,tsysif(k))
	    else if(pol.eq.YX)then
	      T1T2 = ytsys(i1,k)*xtsys(i2,k)
              new_T1T2 = ytsys(i1,tsysif(k))*xtsys(i2,tsysif(k))
	    else
	      call bug('f','Invalid polarization code')
	    endif
c
	    if (redo) then
              if (k.ne.tsysif(k)) then
                if (inv) then
                  data(i) = data(i)/sqrt(new_T1T2/T1T2) 
                else
                  data(i) = data(i)*sqrt(new_T1T2/T1T2)               
                endif  
              endif      
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
	subroutine getopt(uvflags,doapply,auto,redo,inv,scale)
c
	implicit none
        character uvflags*(*)
	logical doapply,auto,redo,inv,scale
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=9)
	character opts(NOPTS)*10
	logical present(NOPTS)
        integer l
c
	data opts/'apply     ','unapply   ','automatic ','redo      ',
     *            'inverse   ','scale     ','nocal     ','nopass    ',
     *            'nopol     '/
c
	call options('options',opts,present,NOPTS)
	if(present(1).and.present(2))call bug('f',
     *	  'Cannot both apply and unapply Tsys correction')
	doapply = .not.present(2)
	auto    = present(3)
        redo = present(4)
        if (present(4).and.(present(1).or.present(2).or.present(3)))
     *    call bug('f',
     *      'Option redo cannot be combined with (un)apply or auto')
        inv = present(5)
        if (present(6)) then
          if (present(1).or.present(2).or.present(3).or.present(4))then
            call bug('f','Option scale cannot be combined with '//
     *       '(un)apply, auto or redo')
          endif
        endif
        scale = present(6)
c
c Set up calibration flags
c
        uvflags = '3'
        l = 1
        if(.not.present(7))then
          l = l + 1
          uvflags(l:l) = 'c'
        endif
        if(.not.present(8))then
         l = l + 1
         uvflags(l:l) = 'f'
        endif
        if(.not.present(9))then
          l = l + 1
          uvflags(l:l) = 'e'
        endif

	end
