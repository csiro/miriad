c************************************************************************
      program uvdflag
c
c= uvdflag - Flags a visibility dataset on uvdistance in klambda.
c& rjs
c: calibration
c+
c	UVZFLAG is a MIRIAD task which flags correlations in a
c	visibility dataset the uvdistance is in a certain range.
c       This task is needed because the standard uv select keyword
c       only selects on the uv coordinates of the first channel.
c@ vis
c	The input visibility datasets to be flagged. No default. Several
c	datasets can be given. Wildcards are supported.
c@ select
c	Normal visibility selection, which is applied to the template
c	dataset. See the help on "select" for more information.
c@ uvrange
c       Two values, giving the range of uv distances in klambda to flag.
c       A single value will flag all correlations with larger uv distance.
c@ options
c	Extra processing options. Several can be given, separated by 
c	commas. Minimum match is supported. Possible values are:
c	  noapply Do not apply the flagging, just report the statistics
c	          about what would be flagged.
c         invert  Invert the selection - flag correlations outside the
c                 specified uvrange
c
c--
c  History:
c     mhw  18feb2014 Adapted from uvzflag.
c---------------------------------------------------------------------------
	implicit none
	include 'maxdim.h'
	integer MAXSELS,MAXFILES
	parameter(MAXSELS=512,MAXFILES=64)
c
	complex data(maxchan)
	double precision preamble(5),sfreq(MAXWIN),sdf(MAXWIN)
	real sels(MAXSELS), uvr(2),uvd,uvdl
	integer lVis,i,j,k,nchan
        double precision ntot,ngood,nflag
	integer nfiles,nspect,nschan(MAXWIN),ifile
	character in(MAXFILES)*64,line*64,type*1,version*64
	logical flags(MAXCHAN),doapp,invert,sel
	logical updated,changed
c
c  Externals.
c
	character versan*64
c
c Get inputs
c
	version = versan('uvzflag')
	call keyini
	call mkeyf('vis',in,MAXFILES,nfiles)
	if(nfiles.eq.0)call bug('f','An input dataset must be given')
	call selInput('select',sels,MAXSELS)
        call keyr('uvrange',uvr(1),0)
        call keyr('uvrange',uvr(2),1e15)
        uvr(1)=uvr(1)*uvr(1)
        uvr(2)=uvr(2)*uvr(2)
	call getopt(doapp,invert)
	call keyfin
c
c Open files
c
	do ifile=1,nfiles
	  call uvopen(lVis,in(ifile),'old')
	  call uvset(lVis,'preamble','uvw/time/baseline',
     *               0,0.d0,0.d0,0.d0)
	  call selApply(lVis,sels,.true.)
c
	  ntot  = 0
	  ngood = 0
	  nflag = 0          
c
c Loop over visibilities and set flags
c
	  call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	  dowhile(nchan.gt.0)
	    ntot = ntot + nchan*1.d0
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
            call uvgetvrd(lVis,'sfreq',sfreq,nspect)
            call uvgetvrd(lVis,'sdf',sdf,nspect)
c
            uvd = preamble(1)*preamble(1)+preamble(2)*preamble(2)
            i=0
	    do k=1,nspect
c	      
	      do j=1,nschan(k)
                uvdl = (uvd/1e6) * (sfreq(k)+(j-1)*sdf(k))**2
		i = i + 1
		if(flags(i))then
		  ngood = ngood + 1
                  sel = uvdl.ge.uvr(1).and.uvdl.le.uvr(2)
		  if((sel.and..not.invert).or.(.not.sel.and.invert))then
		    changed = .true.
		    nflag = nflag + 1
		    flags(i) = .false.
		  endif
	        endif
	      enddo
	    enddo
c
	    if(doapp.and.changed)call uvflgwr(lVis,flags)
	    call uvread(lVis,preamble,data,flags,MAXCHAN,nchan)
	  enddo
c
c  Write out the history.
c
	  if(doapp.and.nflag.gt.0)then
	    call hisopen(lVis,'append')
	    call hiswrite(lVis,'UVDFLAG: Miriad '//version)
	    call hisinput(lVis,'UVDFLAG')
	    call hisclose (lVis)
	  endif
	  call uvclose(lVis)
c
c  Give a summary about the flagging performed.
c
	  if(nfiles.gt.1)call output('After processing '//in(k))
	  call output(' Correlations: Total      Good         Bad')
	  write(line,'(a,f12.0,f12.0,f12.0)')' Before:    ',
     *				   ntot,ngood,ntot-ngood
	  call output(line)
	  write(line,'(a,f12.0,f12.0,f12.0)')' After:     ',
     *				   ntot,ngood-nflag,ntot-ngood+nflag
	  call output(line)
c
	enddo
c
	end
c************************************************************************
	subroutine GetOpt(doapp,invert)
c
	implicit none
	logical doapp,invert
c------------------------------------------------------------------------
	integer NOPTS
	parameter(NOPTS=2)
	character opts(NOPTS)*8
	logical present(NOPTS)
	data opts/'noapply ','invert  '/
c
	call options('options',opts,present,NOPTS)
	doapp  = .not.present(1)
        invert = present(2)
c
	end
