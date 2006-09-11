c************************************************************************
	program antpol
	implicit none
c
c= ANTPOL - Calculate instrumental polarization from averaged uv-data.
c& mchw
c: uv analysis
c+
c	ANTPOL - Calculate instrumental polarization from averaged uv-data.
c	ANTPOL does not apply the gains, bandpass, or polarization corrections.
c@ vis
c	The input UV dataset name. No default.
c@ select
c	This selects the data to be processed, using the standard uvselect
c	format. Default uses all the data.
c@ line
c	this gives the linetype that is used, in the
c	form:
c	  type,nchan,start,width,step
c	where type can be `channel' (default), `wide' or `velocity'.
c	Default is channel,1,1,1,1
c@ log
c	The list output file name. The default is the terminal.
c--
c
c  History:
c   26jul96 mchw
c------------------------------------------------------------------------
	include 'maxdim.h'
	character version*(*)
	parameter(version='ANTPOL: version  26-JUL-96')
	real rtoh,rtod,pi
	integer maxsels
	parameter(pi=3.141592653589793,rtoh=12/pi,rtod=180/pi)
	parameter(maxsels=1024)
c
	real sels(maxsels)
	real start,step,width
	character linetype*20,vis*80,out*80,line*80
	complex data(MAXCHAN)
	complex RR(MAXCHAN),LL(MAXCHAN),RL(MAXCHAN),LR(MAXCHAN)
	logical flags(MAXCHAN)
	integer tvis,numchan,num,base0,pol,i,ant1,ant2,nchan
	double precision uin,vin,timein,basein
	common/preamb/uin,vin,timein,basein
c
c  Read the inputs.
c
	call output(version)
 	call keyini
	call keyf('vis',vis,' ')
	if(vis.eq.' ')call bug('f','Input file must be given (vis=)')
	call SelInput('select',sels,maxsels)
	call keya('line',linetype,'channel')
	call keyi('line',numchan,1)
	call keyr('line',start,1.)
	call keyr('line',width,1.)
	call keyr('line',step,width)
 	call keya('log',out,' ')
	call keyfin
c
c  Open the output text file.
c
 	call LogOpen(out,' ')
c
c  Open the data file, apply selection, do linetype initialisation and
c  determine the variables of interest.
c
	call uvopen(tvis,vis,'old')
	call SelApply(tvis,sels,.true.)
      	call uvset(tvis,'data',linetype,numchan,start,width,step)
	call uvset(tvis,'coord','wavelength',0,0.,0.,0.)
c
c  Title line
c
	call output(' --------------------------------------------')
	call output(' Polarization listing for '//vis)
	call output(' --------------------------------------------')
c
c  Read through the file, listing what we have to.
c
	num=0
	call uvread(tvis,uin,data,flags,maxchan,numchan)
	dowhile(numchan.gt.0)
	 nchan = numchan
	 base0 = basein
	 call BasAnt(basein,ant1,ant2)
	 dowhile(basein.eq.base0.and.numchan.gt.0)
	  call uvrdvri(tvis,'pol',pol,1)
	  num = num + 1
c	  print *, ant1, ant2, pol, (data(i),i=1,nchan)
	  if(pol.eq.-1)then
	    do i=1,nchan
	      RR(i) = data(i)
	    enddo
	  else if(pol.eq.-2)then
	    do i=1,nchan
	      LL(i) = data(i)
	    enddo
	  else if(pol.eq.-3)then
	    do i=1,nchan
	      RL(i) = data(i)
	    enddo
	  else if(pol.eq.-4)then
	    do i=1,nchan
	      LR(i) = data(i)
	    enddo
	  endif
	  call uvread(tvis,uin,data,flags,maxchan,numchan)
	 enddo
c
c  List the averaged data.
c
	 write(line,'(2i3,a,2f7.3)') ant1, ant2,
     *	 '  LR/(0.5*(RR+LL))', ((2.*LR(i)/(RR(i)+LL(i))),i=1,nchan)
	 call output(line)
	 write(line,'(2i3,a,2f7.3)') ant1, ant2,
     *	 '  RL/(0.5*(RR+LL))', ((2.*RL(i)/(RR(i)+LL(i))),i=1,nchan)
	 call output(line)
c	  print *, ant1, ant2, '   LR/(0.5*(RR+LL))',
c     *			 ((2.*LR(i)/(RR(i)+LL(i))),i=1,nchan)
c	  print *, ant1, ant2, '   RL/(0.5*(RR+LL))',
c     *			 ((2.*RL(i)/(RR(i)+LL(i))),i=1,nchan)
c
	enddo
c
c  Close up shop.
c
	call LogClose
	call uvclose(tvis)
	end
c********1*********2*********3*********4*********5*********6*********7**
