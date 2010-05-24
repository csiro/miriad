c************************************************************************
      program uvzflag
c
c= uvzflag - Flags a visibility dataset where data are zero or fixed.
c& rjs
c: calibration
c+
c	UVZFLAG is a MIRIAD task which flags correlations in a
c	visibility dataset when either the data are identically zero
c	or where the data remain absolutely unchanged between the different
c	channels of a spectral window.
c@ vis
c	The input visibility datasets to be flagged. No default. Several
c	datasets can be given. Wildcards are supported.
c@ select
c	Normal visibility selection, which is applied to the template
c	dataset. See the help on "select" for more information.
c@ options
c	Extra processing options. Several can be given, separated by commas.
c	Minimum match is supported. Possible values are:
c	  zero    By default, uvzflag flags data when the good (unflagged)
c	          corrlations in a spectral window are all identical. The `zero' 
c	          flag causes uvzflag to flag when the data are identically
c	          zero.
c	  noapply Do not apply the flagging, just report the statistics
c	          about what would be flagged.
c
c$Id: uvzflag.for,v 1.1 2010/05/24 16:40:57 sau078 Exp $
c--
c  History:
c     rjs  11may10 Adapted from uvaflag.
c---------------------------------------------------------------------------
	implicit none
	include 'maxdim.h'
	integer MAXSELS,MAXFILES
	parameter(MAXSELS=512,MAXFILES=64)
c
	complex data(maxchan)
	double precision tbp(3)
	real sels(MAXSELS)
	integer lVis,ntot,ngood,nflag,i,id,j,k,ifirst,nchan
	integer nfiles,nspect,nschan(MAXWIN),ifile
	character in(MAXFILES)*64,line*64,type*1,version*64
	logical flags(MAXCHAN),doapp,dozero,identical,changed,manycor
	logical updated
c
c  Externals.
c
	character versan*64
c
c Get inputs
c
	version = versan('uvzflag',
     *                    '$Revision: 1.1 $',
     *			  '$Date: 2010/05/24 16:40:57 $')
	call keyini
	call mkeyf('vis',in,MAXFILES,nfiles)
	if(nfiles.eq.0)call bug('f','An input dataset must be given')
	call selInput('select',sels,MAXSELS)
	call getopt(dozero,doapp)
	call keyfin
c
c Open files
c
	do ifile=1,nfiles
	  call uvopen(lVis,in(ifile),'old')
	  call uvset(lVis,'preamble','time/baseline/pol',0,0.,0.,0.)
	  call selApply(lVis,sels,.true.)
c
	  ntot  = 0
	  ngood = 0
	  nflag = 0
c
c Loop over visibilities and set flags
c
	  call uvread(lVis,tbp,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
	    ntot = ntot + nchan
	    changed = .false.
c
c  Get the window description.
c
	    call uvprobvr(lVis,'nschan',type,nspect,updated)
	    if(type.ne.'i'.and.nspect.le.0)
     *		call bug('f','Invalid value for nschan')
	    if(nspect.gt.MAXWIN)
     *		call bug('f','Too many windows for me!')
	    call uvgetvri(lVis,'nschan',nschan,nspect)
c
	    id = 0
	    do k=1,nspect
	      i = id
	      ifirst = 0
	      identical = .true.
	      manycor = .false.
c	      
	      do j=1,nschan(k)
		i = i + 1
		if(flags(i))then
		  ngood = ngood + 1
		  if(dozero)then
		    if(data(i).eq.(0.,0.))then
		      changed = .true.
		      nflag = nflag + 1
		      flags(i) = .false.
		    endif
		  else if(ifirst.eq.0)then
		    ifirst = i
		  else
		    identical = identical.and.data(i).eq.data(ifirst)
		    manycor = .true.
		  endif
	        endif
	      enddo
c
	      identical = identical.and.manycor
c
	      if(.not.dozero.and.identical)then
	        i = id
	        do j=1,nschan(k)
		  i = i + 1
		  if(flags(i))nflag = nflag + 1
		  changed = changed.or.flags(i)
		  flags(i) = .false.
		enddo
	      endif
c
	      id = id + nschan(k)
	    enddo
c
	    if(doapp.and.changed)call uvflgwr(lVis,flags)
	    call uvread(lVis,tbp,data,flags,MAXCHAN,nchan)
	  enddo
c
c  Write out the history.
c
	  if(doapp.and.nflag.gt.0)then
	    call hisopen(lVis,'append')
	    call hiswrite(lVis,'UVZFLAG: Miriad '//version)
	    call hisinput(lVis,'UVZFLAG')
	    call hisclose (lVis)
	  endif
	  call uvclose(lVis)
c
c  Give a summary about the flagging performed.
c
	  if(nfiles.gt.1)call output('After processing '//in(k))
	  call output(' Correlations: Total      Good         Bad')
	  write(line,'(a,i11,i11,i11)')' Before:    ',
     *				   ntot,ngood,ntot-ngood
	  call output(line)
	  write(line,'(a,i11,i11,i11)')' After:     ',
     *				   ntot,ngood-nflag,ntot-ngood+nflag
	  call output(line)
c
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(dozero,doapp)
c
	implicit none
	logical dozero,doapp
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=2)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'zero    ','noapply '/
c
	call options('options',opts,present,NOPTS)
	dozero =      present(1)
	doapp  = .not.present(2)
c
	end
