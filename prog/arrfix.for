c*************************************************************************
	program arrfix
	implicit none
c
c= arrfix - Correct ASKAP antennna table caused by faulty uv FITS file.
c& rjs
c: uv analysis
c+
c	This corrects an ASKAP antenna table caused by a bug in CASA resulting
c	in a faulty FITS antenna table.
c@ vis
c	The names of the input uv data sets. No default.
c@ select
c	Standard visibility data selection. See the help on "select" for
c	more information. The default is to select all data.
c@ out
c	The name of the output uv data set. No default.
c
c--
c  History:
c    10jun14 rjs  Adopted from atfix.
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
	character version*(*)
	integer MAXSELS
	parameter(version='ArrFix: version 1.0 10-Jun-14')
	parameter(MAXSELS=256)
c
	real sels(MAXSELS)
	double precision xyz(3*MAXANT)
	double precision preamble(5)
	integer lVis,lOut,vant,nchan,nant,pol,npol
	character vis*64,out*64
	complex data(MAXCHAN)
	logical flags(MAXCHAN)
c
c  Externals.
c
	logical uvvarUpd,selProbe
c
	call output(version)
	call keyini
	call keya('vis',vis,' ')
	call selInput('select',sels,MAXSELS)
	call keya('out',out,' ')
	call keyfin
c
c  Check the inputs.
c
	if(vis.eq.' ')call bug('f','An input must be given')
	if(out.eq.' ')call bug('f','An output must be given')
c    
c  This program cannot tolerate polarisation, visibility, increment
c  or window selection (for obscure reasons).
c
        if(selProbe(sels,'increment?',0.d0))
     *    call bug('f','ArrFix not support select=inc')
        if(selProbe(sels,'visibility?',0.d0))
     *    call bug('f','ArrFix not support select=vis')
        if(selProbe(sels,'polarization?',0.d0))
     *    call bug('f','ArrFix does not support select=pol')
        if(selProbe(sels,'window?',0.d0))
     *	  call bug('f','ArrFix does not support select=win')
c
c  Get ready to copy the data.
c
	call uvopen(lVis,vis,'old')
	call SelApply(lVis,sels,.true.)
	call uvset(lVis,'preamble','uvw/time/baseline',0,0.,0.,0.)
	call varInit(lVis,'channel')
	call uvvarIni(lVis,vant)
	call uvvarSet(vant,'antpos')
c
c  Open the output, and make its history.
c
	call uvopen(lOut,out,'new')
	call varOnit(lVis,lOut,'channel')
	call uvset(lOut,'preamble','uvw/time/baseline',0,0.,0.,0.)
c
	call hdcopy(lVis,lOut,'history')
	call hisopen(lOut,'append')
	call hiswrite(lOut,'ARRFIX: Miriad '//version)
	call hisinput(lOut,'ARRFIX')
	call hisclose(lOut)
	call calcopy(lVis,lOut)
c
c  Get first record of the final pass.
c
	call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	if(nchan.eq.0)call bug('f','No data found')
	call uvrdvri(lVis,'nants',nant,0)
c
	dowhile(nchan.gt.0)
	  call varCopy(lVis,lOut)
	  call uvrdvri(lVis,'pol',pol,0)
	  call uvrdvri(lVis,'npol',npol,0)
c
c  Do antenna table correction.
c
	  if(uvvarUpd(vant))call fixer(lVis,lOut,xyz,nant)
c
	  if(npol.gt.0)then
	    call uvputvri(lOut,'npol',npol,1)
	    call uvputvri(lOut,'pol',pol,1)
	  endif
	  call uvwrite(lOut,preamble,data,flags,nchan)
	  call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	enddo
c
	call uvclose(lVis)
	call uvclose(lOut)
	end
c************************************************************************
	subroutine fixer(lVis,lOut,xyz,nant)
c
	implicit none
	integer lVis,lOut,nant
	double precision xyz(nant,3)
c
c  Input:
c    lVis
c    lOut
c    nant
c  Scratch:
c    xyz
c------------------------------------------------------------------------
	double precision lat,long,height,xc,yc,zc,r,cost,sint,x,y
	character telescop*16
	logical ok
	integer i
c
	call uvrdvrd(lVis,'latitud',lat,0.d0)
	call uvrdvrd(lVis,'longitu',long,0.d0)   
	call uvrdvrd(lVis,'height',height,0.d0)
	if(height.eq.0.d0)then
	  call uvrdvra(lVis,'telescop',telescop,'askap')
	  call lcase(telescop)
	  call obspar(telescop,'height',height,ok)
	  if(.not.ok)height = 0.d0
	endif
c

	call llh2xyz(lat,long,height,xc,yc,zc)
	write(*,*)xc,yc,zc
	r = sqrt(xc*xc+yc*yc)
	cost = xc/r
	sint = yc/r
c
	call uvgetvrd(lVis,'antpos',xyz,3*nant)
c
	do i=1,nant
	  if(   xyz(i,1).ne.999999.d0.or.
     *		xyz(i,2).ne.999999.d0.or.
     *		xyz(i,3).ne.999999.d0)then
	    x =  (xyz(i,1)+xc)*cost + (xyz(i,2)+yc)*sint - r
	    y = -(xyz(i,1)+xc)*sint + (xyz(i,2)+yc)*cost 
	    xyz(i,1) = x
	    xyz(i,2) = y
	  endif
	enddo
	
	call uvputvrd(lOut,'antpos',xyz,3*nant)
c
	end
c************************************************************************
	subroutine calcopy(lVis,lOut)
c
	implicit none
	integer lVis,lOut
c
c  Copy across gain tables.
c
c------------------------------------------------------------------------
	integer j
c
        integer NTABLE
        parameter(NTABLE=14)
        character tables(NTABLE)*8
        data tables/'interval','nsols   ','ngains  ','nfeeds  ',
     *   'ntau    ','gains   ','freq0   ','leakage ','bandpass',
     *   'freqs   ','nspect0 ','nchan0  ','senmodel','nbpsols '/
c
        do j=1,NTABLE
           call hdcopy(lVis,lOut,tables(j))
        enddo
c
	end

