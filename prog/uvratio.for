c************************************************************************
	program uvratio
	implicit none
c
c= uvratio - Determines the ratio of the XX to YY (or RR to LL) correlations.
c& rjs
c: uv analysis
c+
c	UVRATIO is a MIRIAD task which writes out a uv file, which is the
c	ratio of the XX to YY correlations of the input	visibility file.
c	It can also find the ratio of RR and LL -- see option=circular.
c@ vis
c	The names of the input uv data sets. No default.
c@ line
c	Standard line-type parameters. Standard default.
c@ select
c	The normal uv selection commands. Polarisation selection should
c	not generally be used. Default is to select all data.
c@ out
c	The name of the output uv data set, which is the ratio of the
c	inputs. No default.
c@ options
c	Program enrichment options. Several can be given, separated
c	by commas. Minimum match is used
c	  "nopol"    Do not apply polarisation corrections.
c	  "nocal"    Do not apply calibration corrections.
c	  "nopass"   Do not apply bandpass corrections.
c	  "circular" Find the ratio for circulars. Default is for
c	             linears.
c--
c  History:
c    rjs  24sep91   Original version.
c    rjs  15jun92   Use var routines.
c    rjs  17aug92   Change in call sequence to var routines.
c  Bugs:
c------------------------------------------------------------------------
        include 'maxdim.h'
	character version*(*)
	parameter(version='UvRatio: version 1.0 17-Aug-92')
	integer PolXX,PolYY,PolRR,PolLL
	parameter(PolXX=-5,PolYY=-6,PolRR=-1,PolLL=-2)
c
	integer nchan(2),lIn,lOut,pols(2),pnt,i
	double precision preamble(4,2)
	complex data(maxchan,2),out(maxchan)
	logical flags(maxchan,2),oflags(maxchan),circ
	character outnam*64,uvflags*8,ltype*16
c
c  Externals.
c
	logical uvDatOpn
c
	call output(version)
	call keyini
	call GetOpt(uvflags,circ)
	call uvDatInp('vis',uvflags)
	call keya('out',outnam,' ')
	call keyfin
	if(outnam.eq.' ') call bug('f','Output file name is missing')
c
c  Select just the intensity polarisations.
c
	if(circ)then
	  call uvDatSet('stokes',PolRR)
	  call uvDatSet('stokes',PolLL)
	else
	  call uvDatSet('stokes',PolXX)
	  call uvDatSet('stokes',PolYY)
	endif
c
c  Open the input and output, and set things up.
c
	call uvopen(lOut,outnam,'new')
	if(.not.uvDatOpn(lIn))call bug('f','No input file')
	call uvDatGta('ltype',ltype)
	call VarInit(lIn,ltype)
	call VarOnit(lIn,lOut,ltype)
c
c  Initalise the second slot to a null value.
c
	pols(2) = 0
	nchan(2) = 0
	do i=1,4
	  preamble(i,2) = 0
	enddo
c
c  Loop the loop. Open a file, process it, copy it, etc.
c
	pnt = 1
	call uvDatRd(preamble(1,pnt),data(1,pnt),flags(1,pnt),maxchan,
     *	  nchan(pnt))
	dowhile(nchan(pnt).gt.0)
	  call uvDatGti('pol',pols(pnt))
c
c  Are the 1st and 2nd thingos compatible. If so, lets go with it.
c
	  if(nchan(1).eq.nchan(2).and.
     *	     abs(preamble(1,1)-preamble(1,2)).lt.0.001.and.
     *	     abs(preamble(2,1)-preamble(2,2)).lt.0.001.and.
     *	     abs(preamble(3,1)-preamble(3,2)).lt.0.1/86400.d0.and.
     *	     abs(preamble(4,1)-preamble(4,2)).lt.0.01.and.
     *	     abs(pols(1)-pols(2)).eq.1)then
	    i = 2
	    if(pols(1).gt.pols(2)) i = 1
	    call Rats(data(1,i),flags(1,i),data(1,3-i),flags(1,3-i),
     *		out,oflags,nchan(1))
	    call VarCopy(lIn,lOut)
	    call uvwrite(lOut,preamble(1,1),out,oflags,nchan(1))
	  endif
c
c  Get another record.
c
	  pnt = 3 - pnt
	  call uvDatRd(preamble(1,pnt),data(1,pnt),flags(1,pnt),maxchan,
     *	    nchan(pnt))
	enddo
c
c  Finish up the history, and close up shop.
c
	call hdcopy(lIn,lOut,'history')
        call hisopen(lOut,'append')
        call hiswrite(lOut,'UVRATIO: Miriad '//version)
	call hisinput(lOut,'UVRATIO')
        call hisclose (lOut)
	call uvclose(lOut)
	call uvDatCls
	end
c************************************************************************
	subroutine Rats(XX,Xflags,YY,Yflags,Out,Oflags,n)
c
	implicit none
	integer n
	logical Xflags(n),Yflags(n),Oflags(n)
	complex XX(n),YY(n),Out(n)
c
c  Determine the ratio of the X to Y gains.
c
c------------------------------------------------------------------------
	integer i
c
	do i=1,n
	  Oflags(i) = Xflags(i).and.Yflags(i).and.
     *	    abs(real(YY(i)))+abs(aimag(YY(i))).gt.0
	  if(Oflags(i))then
	    Out(i) = XX(i) / YY(i)
	  else
	    Out(i) = 0
	  endif
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(uvflags,circ)
c
	implicit none
	character uvflags*(*)
	logical circ
c
c  Determine extra processing options.
c
c  Output:
c    uvflags	Input flags to uvDat.
c    circ	Do circular polarisations (rather than linear).
c------------------------------------------------------------------------
	logical nopol,nocal,nopass
c
	integer nopt
	parameter(nopt=4)
	character opts(nopt)*9
	logical present(nopt)
	data opts/'nocal    ','nopol    ','circular','nopass   '/
	call options('options',opts,present,nopt)
	nocal = present(1)
	nopol = present(2)
	circ  = present(3)
	nopass= present(4)
c
	uvflags = 'dl'
	if(.not.nocal) uvflags(3:3) = 'c'
	if(.not.nopol) uvflags(4:4) = 'e'
	if(.not.nopass)uvflags(5:5) = 'f'
	end
