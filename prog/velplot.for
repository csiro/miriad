c**********************************************************************c
	program velplot
	implicit none
c
c= VELPLOT - Interactive task to plot spectra and slices from Miriad Images.
c& mchw
c: image analysis
c+
c	VELPLOT is an interactive task to analyse spectra, position-velocity
c	slices, and integrated velocity maps from Miriad Images.
c	There are three basic display options:
c
c	 - Velocity-averaged x-y images over selected velocity intervals.
c		Also makes velocity and velocity dispersion images.
c	 - Spectra at selected x-y positions. The data can be convolved
c		in the x-y or velocity. Gaussian fitting routine.
c	 - Position-velocity maps along user selected position angles,
c		or plots of intensity versus position for selected velocity
c		channels. The data can be convolved in the x-y plane.
c
c	(x,y) positions are (HA,DEC) in arcsec relative to map center.
c	Position angles are measured from the north towards the east.
c
c	The output from each option can be plotted, printed, or written out
c	as Miriad images of velocity-averaged maps, position-velocity maps,
c	or ascii spectra respectively. Gaussian fits are written into the log.
c
c	The task saves user-defined lists of velocity-averaged maps, position-
c	velocity cuts, and spectra positions. These lists can be created and 
c	edited within the task, and can be passed between the three options. 
c	For example, a list of spectra or position-velocity cuts can be created 
c	with the cursor whilst displaying a velocity-averaged map. These lists
c	are used to display the spectra and position-velocity cuts, and can be
c	written out and read in as ascii files.
c
c	A menu option provides a choice of display style, contour levels,
c	and units. Further interactive help is available within the task.
c@ in
c	Input image name. No default.
c@ device
c	The PGPLOT plotting device. 
c@ region
c	Region of image to be plotted. E.g.
c	  % image region=relpix,box(-30,-30,20,90)(16,57)
c	reads in 50 x 120 pixels of image planes 16 to 57.
c	The default is the whole Image. The current size limit is 128 in
c	any dimension.
c@ log
c	The output log file. The default filename is velplot.log
c	Results from image analysis are written into the log file.
c--
c	reads and writes 3D arrays of channel maps
c	generates: 1) spectra
c		   2) position-velocity plots
c		   3) velocity-averaged x-y contour plots
c			MCHW Jan 1986
c  History:
c	developed form Ralint's Velplot (Fuller Vogel Wright, 1970's)
c    Nov 1988 mchw Adjustable array dimensions.
c    aug 1989	   Added subroutines to	read and write Miriad files.
c    25may90 mchw  Major rebuild using Miriad user interface.
c		   Removed Ralint subroutines, mapio etc.
c    15jan91 mchw  Added in-code doc and removed a couple of bugs.
c    23jul91 dm/dw modified VMS version for UNIX
c    25jul91 mchw  removed pi from velplot.h to avoid MONGO conflict.
c    31jul91 djw   PGPLOT version
c    02aug91 mchw  Ported back to Vax and minor bugs cleaned up.
c    14aug91 ms    Patches for cray.
c    02oct91 djw   Fixed Pgplot transformation arrays (1 pixel).
c    07oct91 djw   Modified cursor options to draw specs and cuts.
c    25oct91 mchw  More patches for cray.
c    28oct91 djw   Grayscale
c    31oct91 mchw  Changed grid of spectra to match plot on paper.
c			Changed ifdef to cft, and improved user i/o.
c			Added width to range of maps. More cleanup.
c    10apr92 mjs   In sub velmap, "label" now char*30 to elim string
c                       truncation in assignment.
c    13apr92 mchw  Changed position-velocity plots to increment in
c			direction of pa and fixed bug for unequal axes.
c    28may92 mchw  Convert frequency axis on input image to velocity.
c    29may92 mchw  Changed to absolute cutoff level in moment maps.
c		   Added integral and rms in cursor defined box.
c    04jun92 mchw  Read and write ascii files of spectra & pos-vel cuts.
c    09jun92 mchw  Added Gaussian Fits to spectra using nllsqu.
c    10jun92 djw   Added Multiple Gaussian Fits to spectra.
c    10jun92 mchw  Save rms estimates. Write Gaussian fits into log.
c    12jun92 mchw  Estimate fit from moments. Less things to type in.
c    15jun92 jat   Add position-intensity cuts to PosVel.
c    16jun92 mchw  Write more info' into log. Improved documentation.
c    07jul92 mjs   Remove doubly-declared variables (Convex coughs).
c    13jul92 mchw  Fix new ?? bug in Intensity plot input in PosVel.
c    19aug92 mchw  Fix bug in integration box. Normalize chisq in fit.
c    04sep92 mchw  Spectra options: omits plots, rectangular grids.
c    16sep92 mchw  Better image header for position-velocity maps.
c    23sep92 mchw  Add option to get velocity of peak channel.
c    30sep92 mchw  Add history to output image files.
c    05nov92 djw   Add option to write ascii file of gaussian fits.
c    27feb93 mjs   use tmpdim.h instead of maxdim.h
c    13mar93 mjs   pgplot subr names have less than 7 chars.
c    02jul93 mjs   Elim unused labeled stmts to elim compiler warnings;
c                  elim branches to labels from outside the block.
c    08jul93 mjs   Merge 2 versions with minor differences.
c    21sep93 mchw  Added nint to integer expressions using pixel size.
c    25oct93 mchw  Added nint in subroutine readmap.
c    01feb95 jm/mchw Elliminate some questions.	call pgask(.FALSE.)
c    02feb95 jm    De-FLINT'd.
c----------------------------------------------------------------------c
	include 'tmpdim.h'
	include 'velplot.h'
	character*(*) version
	parameter(version='(version 3.0 02-FEB-95)')
	integer maxnax,maxboxes
	parameter(maxnax=3,maxboxes=128)
	integer boxes(maxboxes),nsize(maxnax),blc(maxnax),trc(maxnax)
	character*80 ans,line,logfile
	integer lIn,nx,ny,nc,idoc,iostat,length
	real ary(maxbuf),vlsr(maxdim),v(maxdim*maxdim)
c
c  real ary(128*128*64)=1048576 reals = 4 MBytes
c
c set default plotting parameters
c
	units = 'J'
	alabel = 'Y'
	percent = 'Y'
	apint = 'N'
	abscoord = 'N'
	write = 'N'
	cneg = 'Y'
	pspec = 'Y'
	gray = 'N'
	defgray = 'Y'
	lgaufit = 'Y'
	lgauplot = 'Y'
	nlevels = 6
	do nc = 1, nlevels
	  levels(nc) = nc * 15
	enddo
	do nc = nlevels + 1, 10
	  levels(nc) = 0
	enddo
c
c  Get the input parameters.
c
	call output('VELPLOT '//version)
	call keyini
	call keya('in',file,' ')
	if(file.eq.' ') call bug('f','Image Name missing')
	call BoxInput('region',file,boxes,maxboxes)
	call keya('device',device,'?')
	call keya('log',logfile,'velplot.log')
	call keyfin
c
c  read Miriad array and check dimensions requested.
c
	call xyopen(lIn,file,'old',3,nsize)
	call BoxMask(lIn,boxes,maxboxes)
	call BoxSet(boxes,maxnax,nsize,' ')
	call BoxInfo(boxes,maxnax,blc,trc)
	nx = trc(1)-blc(1)+1
	ny = trc(2)-blc(2)+1
	nc = trc(3)-blc(3)+1
	call output('file: '//file)
	write(line, *) 'Array dimensions are: nx,ny,nc=',nx,ny,nc
	call output(line)
	if(nx*ny*nc.gt.maxbuf)
     *		 call bug('f','Image too big for buffer')
	if(nx.gt.128.or.ny.gt.128.or.nc.gt.128)
     *	  call bug('w','Dimension too big for some buffers')
	call readmap(lIn,blc,trc,ary,vlsr,nx,ny,nc)
c
c  Tell user how to exit from xwindow.
c
	if(index(device,'/x').ne.0)then
	  call output('Use RIGHT mouse button to exit from xwindow')
	endif
c
c set default parameters
c
	is = 1
	ie = nx
	ib = 1
	it = ny
	cutoff = -9998.
c
c  Start the output log file.
c
	call LogOpen(logfile,'q')
	call LogWrit('VELPLOT '//version)
	call velohead(ary,vlsr,nx,ny,nc)
c
c  Prompt for interactive options:
c
10	call output(' ')
	call output('      OPTIONS')
	call output('Comment - Write comment into log')
	call output('Help -  Type  explanation of options')
	call output('List -  List header and velocity information')
	call output('Integral - Integrated flux and statistics')
	call output('Menu -  Select plot parameters')
	call output('Pos-Vel - Plot versus position & velocity')
	call output('Spectra - Plot spectra')
	call output('Vel-map - Plot integrated velocity maps')
	call output('Write - Write Miriad Image to disk file')
	call output(
     *	         'RA - Read file of spectra & position-velocity cuts')
	call output(
     *	         'WA - Write file of spectra & position-velocity cuts')
	call output('Exit - Exit from program')
	call output(' ')
	call prompt(ans,length,'Select option (type 1st character) :')
	call ucase(ans)
	if(ans(1:1).eq.'S') then
	  call spectra(ary,vlsr,nx,ny,nc)
	else if(ans(1:1).eq.'C') then
	  call comment
	else if(ans(1:2).eq.'RA') then
	  call rdary
	else if(ans(1:2).eq.'WA') then
	  call wrary
	else if(ans(1:1).eq.'P') then
	  call PosVel(ary,vlsr,nx,ny,nc)
	else if(ans(1:1).eq.'V') then
	  call velmap(ary,vlsr,nx,ny,nc,v)
	else if(ans(1:1).eq.'L') then
	  call velohead(ary,vlsr,nx,ny,nc)
	else if(ans(1:1).eq.'I') then
	  call Integral(ary,vlsr,nx,ny,nc)
	else if(ans(1:1).eq.'W') then
	  vel=vlsr(1)
	  delv=vlsr(2)-vlsr(1)
	  call writemap(ary,nx,ny,nc)
	else if(ans(1:1).eq.'M') then
	  call menu
	else if(ans(1:1).eq.'H') then
	  call txtopen(idoc,'mirdoc:velplot.doc','old',iostat)
	  do while(iostat.eq.0)
	    call txtread(idoc,line,length,iostat)
	    call output(line(1:length))
	  enddo
	else if(ans(1:1).eq.'E') then
	  call LogClose
	  stop
	else
	  goto 10
	endif
	goto 10
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine velohead(ary,vlsr,nx,ny,nc)
	implicit none
	integer nx,ny,nc
	real ary(1), vlsr(nc)
c
c  List Header and velocity information
c		     mchw 9 nov 1987
c----------------------------------------------------------------------c
	include 'velplot.h'
 	character msg*80
c	character ans*1
c	integer i,ipr
c
	call output('List Header and velocity information')
	call output('File : '//file)
	write(msg, *) ' Array dimensions are :nx,ny,nc=',nx,ny,nc
	call output(msg)
	call header(5)
	write (msg, *) ' map pixels (L,R,B,T)=',1-mid,nx-mid,1-midy,
     *    ny-midy
	call output(msg)
c	call output('Channel velocites :')
c	print *, (vlsr(i),i=1,nc) 
c
c	call prompt(ans,ipr,'Write into log ? (Y/[N]) :')
c	call ucase(ans)
c	if(ans.eq.'Y')then
	  call LogWrit('File : '//file)
	  call header(6)
c	endif
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine readmap(lIn,blc,trc,ary,vlsr,nx,ny,nc)
	implicit none
	integer lIn,blc(3),trc(3),nx,ny,nc
	real ary(1),vlsr(1)
c
c  Read portion of Miriad Image into array.
c
c  Inputs:
c    lIn	Handle of input Image.
c    blc,trc	corners of region of interest.
c
c  Outputs:
c	ary(nx,ny,nc)	spectral line image
c	vlsr(nc)	velocity array
c	nx ny nc 	dimensions of images
c
c  	common/head/ contains map header
c 	common/box/ contains data on the map array
c
c-------------------------------------------------------------------c
	include 'tmpdim.h'
	include 'velplot.h'
	double precision ckms
	parameter(ckms=299793.)
	integer i,j,k,ipt
	real cdelt,crval,crpix1,crpix2,crpix,row(maxdim)
	character*20 ctype3
c
	call rdhdr(lIn,'cdelt1',xy,0.)
	if(xy.eq.0)call bug('f','Pixel increment missing')
	call rdhda(lIn,'object',object,' ')
	call rdhda(lIn,'bunit',bunit,' ')
	call rdhda(lIn,'ctype1',ctype(1),' ')
	call rdhda(lIn,'ctype2',ctype(2),' ')
	if(ctype(1)(1:2).eq.'RA'.and.ctype(2)(1:3).eq.'DEC')then
	  xy = abs(xy) * 180./3.141592654 * 3600.
	endif
	call rdhda(lIn,'ctype3',ctype(3),' ')
	call rdhdr(lIn,'epoch',epoch,0.)
	call rdhdr(lIn,'crval1',ras,0.)
	call rdhdr(lIn,'crval2',decs,0.)
	call rdhdr(lIn,'crpix1',crpix1,real(nx/2+1))
	call rdhdr(lIn,'crpix2',crpix2,real(ny/2+1))
	call rdhdr(lIn,'restfreq',restfreq,0.)
	call rdhdr(lIn,'bmaj',bmaj,0.)
	call rdhdr(lIn,'bmin',bmin,0.)
	call rdhdr(lIn,'bpa',bpa,0.)
	call rdhdi(lIn,'niters',niters,0)
	mid = nint(crpix1-blc(1)+1)
	midy = nint(crpix2-blc(2)+1)
	call output('Reading Image')
	ipt = 1
	do k = blc(3),trc(3)
	  call xysetpl(lIn,1,k)
	  do j = blc(2),trc(2)
	    call xyread(lIn,j,row)
	    do i = blc(1),trc(1)
	      ary(ipt) = row(i)
	      ipt = ipt + 1
	    enddo
	  enddo
	enddo
	call rdhdr(lIn,'cdelt3',cdelt,1.)
	call rdhdr(lIn,'crpix3',crpix,1.)
	call rdhdr(lIn,'crval3',crval,1.)
	call rdhda(lIn,'ctype3',ctype3,' ')
	if(ctype3(1:4).eq.'FREQ'.and.restfreq.ne.0.)then
	  call output('Convert frequency axis to velocity')
	  cdelt = cdelt/restfreq*ckms
	  crval = crval/restfreq*ckms
	endif
	vel = crval + (blc(3)-crpix)*cdelt
	delv = cdelt
	do i=1,nc
	  vlsr(i) = crval + (blc(3)-crpix +i-1)*cdelt
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine writemap(ary,nx,ny,nc)
	implicit none
	integer nx,ny,nc
	real ary(1)
c
c  Write out Miriad Image.
c
c  Inputs:
c    ary(nx,ny,nc)	spectral line image
c    nx ny nc	 	dimensions of image
c    common/head/ contains map header
c------------------------------------------------------------------c
	include 'velplot.h'
	character*80 filename,text
	integer lOut,nsize(3),j,k,ipt,length
	real crpix1,crpix2,cdelt
c
c  Open output file and write header from values in common.
c
	call prompt(filename,length,'Enter output name: ')
	if(length.le.0)return
	nsize(1) = nx
	nsize(2) = ny
	nsize(3) = nc
	call xyopen(lOut,filename,'new',3,nsize)
	crpix1 = mid
	crpix2 = midy
	call wrhdr(lOut,'crpix1',crpix1)
	call wrhdr(lOut,'crpix2',crpix2)
	cdelt = 3.141592654/180./3600.*xy
	if(cdelt.ne.0)call wrhdr(lOut,'cdelt1',-cdelt)
	if(cdelt.ne.0)call wrhdr(lOut,'cdelt2',cdelt)
	call wrhda(lOut,'object',object)
	call wrhda(lOut,'bunit',bunit)
	call wrhda(lOut,'ctype1',ctype(1))
	call wrhda(lOut,'ctype2',ctype(2))
	call wrhda(lOut,'ctype3',ctype(3))
	call wrhdr(lOut,'epoch',epoch)
	call wrhdr(lOut,'crval1',ras)
	call wrhdr(lOut,'crval2',decs)
	call wrhdr(lOut,'crpix3',1.)
	call wrhdr(lOut,'crval3',vel)
	call wrhdr(lOut,'cdelt3',delv)
	call wrhdr(lOut,'restfreq',restfreq)
	call wrhdr(lOut,'bmaj',bmaj)
	call wrhdr(lOut,'bmin',bmin)
	call wrhdr(lOut,'bpa',bpa)
	call wrhdi(lOut,'niters',niters)
c
c write out 1 row at a time
c
	call output('writing output Image to file: '//filename)
	ipt = 1
	do k=1,nc
	  call xysetpl(lOut,1,k)
	  do j = 1,ny
	    call xywrite(lOut,j,ary(ipt))
	    ipt = ipt + nx
	  enddo
	enddo
c
c  Write the history.
c
	call hisopen(lOut,'append')
	call hiswrite(lOut,'VELPLOT:')
	call hisinput(lOut,'VELPLOT')
	write (text,'(a)') 'VELPLOT: Integrated-velocity map'
	call hiswrite(lOut,text)
	write (text,'(a,f12.3,a)') 'VELPLOT: Integrated over ',delv,
     *	  ' km/s'
	call hiswrite(lOut,text)
	call hisclose(lOut)
c
	call xyclose(lOut)
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine wrposvel(ary,nx,ny,nc,crval1,crval2,crval3,
     *		crpix1,crpix2,cdelt1,cdelt2)
	implicit none
	integer nx,ny,nc
	real ary(1),crval1,crval2,crval3,crpix1,crpix2,cdelt1,cdelt2
c
c  Write out Miriad Image.
c
c  Inputs:
c    ary(nx,ny,nc)	spectral line image
c    nx ny nc	 	dimensions of image
c    crval1,2,3
c    crpix1,2
c    cdelt1,2
c    common/head/ contains map header
c------------------------------------------------------------------c
	include 'velplot.h'
	character*80 filename,text
	integer lOut,nsize(3),j,k,ipt,length
c
c  Open output file and write header from values in common.
c
	call prompt(filename,length,'Enter output name: ')
	if(length.le.0)return
	nsize(1) = nx
	nsize(2) = ny
	nsize(3) = nc
	call xyopen(lOut,filename,'new',3,nsize)
	call wrhda(lOut,'ctype1',ctype(3))
	call wrhda(lOut,'ctype2','POSITION')
	call wrhda(lOut,'ctype3','ANGLE')
	call wrhdr(lOut,'crval1',crval1)
	call wrhdr(lOut,'crval2',crval2)
	call wrhdr(lOut,'crval3',crval3)
	call wrhdr(lOut,'crpix1',crpix1)
	call wrhdr(lOut,'crpix2',crpix2)
	call wrhdr(lOut,'crpix3',1.)
	call wrhdr(lOut,'cdelt1',cdelt1)
	call wrhdr(lOut,'cdelt2',cdelt2)
	call wrhdr(lOut,'cdelt3',delv)
	call wrhda(lOut,'object',object)
	call wrhda(lOut,'bunit',bunit)
	call wrhdr(lOut,'epoch',epoch)
	call wrhdr(lOut,'restfreq',restfreq)
	call wrhdr(lOut,'bmaj',bmaj)
	call wrhdr(lOut,'bmin',bmin)
	call wrhdr(lOut,'bpa',bpa)
	call wrhdi(lOut,'niters',niters)
c
c write out 1 row at a time
c
	call output('writing output Image to file: '//filename)
	ipt = 1
	do k=1,nc
	  call xysetpl(lOut,1,k)
	  do j = 1,ny
	    call xywrite(lOut,j,ary(ipt))
	    ipt = ipt + nx
	  enddo
	enddo
c
c  Write the history.
c
	call hisopen(lOut,'append')
	call hiswrite(lOut,'VELPLOT:')
	call hisinput(lOut,'VELPLOT')
	write (text,'(a)') 'VELPLOT: Position-velocity cut'
	call hiswrite(lOut,text)
	write (text,'(a,f8.3,a,f8.3,a,a,f5.1,a)')
     *    'VELPLOT: Center (x,y)=(',
     *    posx,',',posy,') arcsec','  Position-angle=',pospa,' degrees'
	call hiswrite(lOut,text)
	call hisclose(lOut)
c
	call xyclose(lOut)
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine menu
	implicit none
c  Set plotting parameters
c----------------------------------------------------------------------c
	include 'velplot.h'
	integer i,length
        character*80 line
        character*9 tline
	character*80 ans
        
c 
c display curent defaults
c
90      call output(' ')
	call output('Default plotting parameters:')
	call output(' ')
c units 
        write(line,'(''Units for display [J/K].....'',x,A)') units 
        call output(line)
c negative contours 
        write(line,'(''Negative contours [Y/N].....'',x,A)') cneg 
        call output(line)
c header
        write(line,'(''Plot header [Y/N]...........'',x,A)') alabel 
        call output(line)
c Write map to file 
        write(line,'(''Write map to file [Y/N].....'',x,A)') write 
        call output(line)
c Absolute coords
        write(line,'(''Absolute coordinates [Y/N]..'',x,A)') abscoord 
        call output(line)
c Integer plot
        write(line,'(''Integer plot [Y/N]..........'',x,A)') apint 
        call output(line)
c Spectra Positions
        write(line,'(''Spectra positions [Y/N].....'',x,A)') pspec 
        call output(line)
c Gaussian Fits
        write(line,'(''Fit Gaussians [Y/N].........'',x,A)') lgaufit 
        call output(line)
c Plot Gaussian Fits
        write(line,'(''Overlay Gauss Fits [Y/N]....'',x,A)') lgauplot 
        call output(line)
c Gray Scale
        write(line,'(''Gray Scale [Y/N]............'',x,A)') gray
        call output(line)
c Exit
        call output('Exit default menu')
c Contour levels
        if(percent.eq.'Y')then 
          tline='percent'
        else
          tline='absolute'
        endif
        write(line,'(''Current contours: '',x,A)') tline
        call output(line)
        do i=1,nlevels
          write(line, 109) i,levels(i)
          call output(line)
        enddo
109     format(' level(',i2,') = ',f9.3)
c
c ask for input 
c
199     call output(' ')
        call prompt(ans,length,
     *  'Select option (type 1st character, <cr> for options) :')
        call ucase(ans)
        if(ans(1:1).eq.'U')then
          if(units.eq.'J')then 
            units='K'
            call output('-plot units are now Kelvin')
          else if(units.eq.'K')then
              units='J'
              call output('-plot units are now Janskeys')
          else if(units.ne.'J' .and. units.ne.'K')then
              call output('-unrecognized map units')
          endif   
        else if(ans(1:1).eq.'N')then
          if(cneg.eq.'Y')then 
            cneg='N'
            call output('-negative contours will not be displayed')
          else
            cneg='Y'
            call output('-negative contours will be displayed')
          endif   
        else if(ans(1:1).eq.'P')then
          if(alabel.eq.'Y')then 
            alabel='N'
            call output('-plot will not be annotated')
          else
            alabel='Y'
            call output('-plot will be annotated')
          endif   
        else if(ans(1:1).eq.'W')then
          if(write.eq.'Y')then 
            write='N'
            call output('-will not write MIRIAD image to disk')
          else
            write='Y'
            call output('-will write MIRIAD image to disk')
          endif   
        else if(ans(1:1).eq.'A')then
          if(abscoord.eq.'Y')then 
            abscoord='N'
          else
            abscoord='N'
          endif   
          call output('Sorry...option not yet implemented.')
        else if(ans(1:1).eq.'I')then
          if(apint.eq.'Y')then 
            apint='N'
            call output('-plot will not be integer plot')
          else
            apint='Y'
            call output('-plot will be integer plot')
          endif   
        else if(ans(1:1).eq.'S')then
          if(pspec.eq.'Y')then 
            pspec='N'
            call output('-plot will not indicate spectra positions') 
          else
            pspec='Y'
            call output('-plot will indicate spectra positions') 
          endif   
        else if(ans(1:1).eq.'F')then
          if(lgaufit.eq.'Y')then 
            lgaufit='N'
            call output('-will not fit gaussians to spectra') 
          else
            lgaufit='Y'
            call output('-will fit gaussians to spectra') 
          endif   
        else if(ans(1:1).eq.'O')then
          if(lgauplot.eq.'Y')then 
            lgauplot='N'
            call output('-will not overlay gaussian fits to spectra') 
          else
            lgauplot='Y'
            call output('-will overlay gaussian fits to spectra') 
          endif   
        else if(ans(1:1).eq.'G')then
          if(gray.eq.'Y')then 
            gray='N'
            call output('-plot will not include gray scale') 
          else
            gray='Y'
            call output('-plot will include gray scale') 
            call SetGray
          endif   
        else if(ans(1:1).eq.'C')then
          call SetCont
        else if(ans(1:1).eq.' ')then
          goto 90
        else if(ans(1:1).eq.'E')then
          goto 200
        endif
        goto 199
200     continue
        end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine header(ipr)
	implicit none
	integer ipr
c  convert units and write out map header if ipr .ne. 0.
c----------------------------------------------------------------------c
	include 'velplot.h'
	real rae(3),dece(3),ckms,rts,hours,degs,deg,freqs
	real omega,pixel,pi
	integer mins,len1
	character*80 line
	common/back/rae,dece
	data  ckms/299793./,pi/3.141592654/
c
c  radians to arcsecs
	rts = 3600.*180./pi
c
c ---- convert RA and DEC units ----
c
	if(ras.lt.0.) ras = ras + 2.*pi
	hours = ras*12./pi
	rae(1)=int(hours)
	mins = 60.*(hours-int(hours))
	rae(2)=mins
	rae(3)=3600.*(hours-int(hours))-60.*mins
c
	degs=decs*180./pi
	dece(1)=int(degs)
	deg=abs(degs)
	mins = 60.*(deg-int(deg))
	dece(2)=mins
	dece(3)=3600.*(deg-int(deg))-60.*mins
c
c --- convert units for maps ----
c
	cbof   = 1.
	dperjy = 1.
	freqs = restfreq*(1.-vel/ckms)
	if(bmaj*bmin*freqs.ne.0.) then
	  pixel = xy/rts
	  if(bmaj.gt.pixel) then
	    omega = pi * bmaj*bmin /(4.*log(2.))
	    cbof  = omega / (pixel*pixel)
	  else
	    omega = pixel*pixel
	  end if
	  dperjy = (0.3/freqs)**2 / (2.*1.38e3*omega)
	end if
c
c --- write out map header information ---
c
  	if(ipr.eq.0)then
	   return
 	else if(ipr.eq.5)then
	  write(line,100) object,rae,dece,xy,vel,delv
	  call output(line(1:len1(line)))
	  write(line,101) ctype,posx,posy,pospa,restfreq,bunit
	  call output(line(1:len1(line)))
	  write(line,102) bmaj*rts, bmin*rts, bpa, niters,dperjy,cbof
	  call output(line(1:len1(line)))
 	else if(ipr.eq.6)then
	  write(line,100) object,rae,dece,xy,vel,delv
	  call LogWrit(line(1:len1(line)))
	  write(line,101) ctype,posx,posy,pospa,restfreq,bunit
	  call LogWrit(line(1:len1(line)))
	  write(line,102) bmaj*rts, bmin*rts, bpa, niters,dperjy,cbof
	  call LogWrit(line(1:len1(line)))
	endif

100	format(a,x,2(f5.0,f3.0,f6.3),' xy:',f8.3,' vel:',f8.2,
     *	' delv:',f8.2)
101  	format(a,a,a,3f6.2,' freq:',f9.5,' unit:',a)
102  	format('beam:',3f6.1,' niters:',i7,' K/Jy:',f9.2,' cbof:',f7.2)

	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine getpanel(imaps,windx,windy)
	implicit none
	integer imaps,windx,windy
c
c	Return number of plotting windows in x and y directions (windx,windy)
c	imaps is the number of maps to be plotted
c----------------------------------------------------------------------c
	character msg*80
	integer uwindx,uwindy
c
	if (imaps .gt. 16) then
	    windx=5
	    windy=5
        else if (imaps .gt. 9 .and. imaps .le. 16 ) then
            windx=4
            windy=4
	else if (imaps .gt. 4 .and. imaps .le. 9 ) then
	    windx=3
	    windy=3
	else if (imaps .gt. 1 .and. imaps .le. 4) then
	    windx=2
	    windy=2
	else if (imaps .eq. 1) then
	    windx=1
	    windy=1
	endif
c
c  See if these defaults are ok with the user
c
	call output(' ')
105	write(msg,'(a,i2,a,i2,a)')
     *   '>Enter number of windows in x and y: [',windx,',',windy,']:' 
	call output(msg)
	read (5,104,err=105) uwindx,uwindy
104     format (i10.0,i10.0)
	if (uwindx.ne.0) windx=uwindx
	if (uwindy.ne.0) windy=uwindy
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine cursor(ary,nx,ny,key)
	implicit none
	integer nx,ny
	real ary(nx,ny)
	character key*1
c
c  Cursor options
c
c  Inputs:
c    ary(nx,ny)	The current array,
c  Outputs:
c    key	cursor option
c----------------------------------------------------------------------c
	include 'velplot.h'
	real xx,yy  
        real xx1,yy1
        character*1 key2
        character*80 msg
        integer sym
        real sx(2),sy(2),flux,pat,xpts(2),ypts(2)
        real pi
c
	integer nspec,ngauss(49)
	real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
	common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c
        integer ncut
	real xcut(128),ycut(128),pa(128)
        common /cuts/ xcut,ycut,pa,ncut
c
        pi=3.141592654

10	call output('>CURSOR OPTIONS: Type L to list options :')
	xx = 0.
	yy = 0.
        call pgcurs(xx,yy,key)
	call ucase(key)
c  Type options
	IF (KEY.EQ.'L') THEN
	  call output(' Cursor options:')
	  call output(' A - absolute coordinates')
	  call output(' C - change contour interval')
          call output(' D - clear spec/pos-vel stacks')
	  call output(' E - exit from plot')
	  call output(' X - exit from plot')
          call output(' G - change grayscale')
          call output(' H - header label')
	  call output(' J - Jy contours')
          call output(' K - Kelvin contours')
          call output(' N - negative contours')
          call output(' P - cursor position')
	  call output(' Q - Plot spec/pos-vel positions')
	  call output(' I - integral and rms in box')
          call output(' B - blc for integral')
	  call output(' R - replot maps')
          call output(' S - Spectra position')
	  call output(' T - trc for integral')
	  call output(' V - define pos-vel cut')
	  call output(' W - Write out this map')
	  call output('(x,y) positions are in (HA,DEC) directions.')
	  call output('(Position angle is measured from N through E)')
c********1*********2*********3*********4*********5*********6*********7**

c  Absolute coordinate labels
	ELSE IF (KEY.EQ.'A') THEN
	  abscoord='N'
          call output('Sorry...option not yet implemented.')
c  Change  contour interval
	ELSE IF (KEY.EQ.'C') THEN
	  call SetCont
c  Exit from plot
	ELSE IF ((KEY.EQ.'E') .OR. (KEY.EQ.'X')) THEN
	  KEY = 'E'
	  RETURN
c  Annotate plot
	ELSE IF (KEY.EQ.'H') THEN
          if(alabel.eq.'Y')then
	    alabel='N'
            call output('-will not annotate plot')
          else
            alabel='Y'
            call output('-will annotate plot')
          endif
c  Plot negative contours
	ELSE IF (KEY.EQ.'N') THEN
          if(cneg.eq.'Y')then
            cneg='N'
            call output('-will not plot negative contours')
          else
	    cneg='Y'
            call output('-will plot negative contours')
          endif
c  Plot specta positions
        ELSE IF(KEY.EQ.'Q') THEN
          if(pspec.eq.'Y')then
            pspec='N'
            call output('-will not indicate spec/pos-vel positions')
          else
            pspec='Y'
            call output('-will indicate spec/pos-vel positions')
          endif
c Change grayscale 
        ELSE IF(KEY.EQ.'G') THEN
          if(gray.eq.'Y')then
            gray='N'
            call output('-will not include grayscale')
          else
            gray='Y'
            call output('-will include grayscale')
            call SetGray
          endif
c  Replot on display
	else if (key.eq.'R') then
          call pgend
	  return
c  Get position for spectra
	else if (key.eq.'S') then
	  if(nspec.ge.49) then
	    call output('maximum 49 spectra')
	    go to 10
	  end if
	  nspec=nspec+1
	  xc(nspec)=xx
	  yc(nspec)=yy
	  write(msg, 113) nspec,xx,yy
	  call output(msg)
          sx(1)=xc(nspec)
          sy(1)=yc(nspec)
          sym=nspec+64
          call pgpt(1,sx,sy,sym)
c  Get pos-vel cuts 
	else if (key.eq.'V') then
	  if(ncut.ge.25) then
	    call output('maximum 25 pos-vel cuts')
	    go to 10
	  end if
	  ncut=ncut+1
          call output('>position-velocity cut:')
          call output('  define position angle with two points')       
          call output('  move cursor and strike any key')
          xx1=xx
          yy1=yy
          call pgcurs(xx1,yy1,key2)
          pa(ncut)=(180*atan((xx-xx1)/(yy1-yy)))/pi 
          if(pa(ncut).lt.0)pa(ncut)=pa(ncut)+180.
          xcut(ncut)=0.5*(xx1+xx)
          ycut(ncut)=0.5*(yy1+yy)
          write(msg, 114) ncut,xcut(ncut),ycut(ncut),pa(ncut)
          call output(msg)
          pat=-tan(pi/2.-pa(ncut)*pi/180.)
          xpts(1)=-1e4
          ypts(1)=-1e4*pat+(ycut(ncut)-xcut(ncut)*pat) 
          xpts(2)=1e4
          ypts(2)=1e4*pat+(ycut(ncut)-xcut(ncut)*pat)
          call pgline(2,xpts,ypts)
          sx(1)=xcut(ncut)
          sy(1)=ycut(ncut)
          sym=ncut+96
          call pgpt(1,sx,sy,sym)
c
c  change plot units
	else if (key.EQ.'J' .and. units.eq.'K') THEN
	  units='J'
          call output('-units will be Janskeys')
	else if (key.EQ.'K' .and. units.eq.'J') THEN
	  units='K'
          call output('-units will be Kelvin')
c  write out map
	else if (key.eq.'W') THEN
          if(write.eq.'N')then
	    write='Y'
            call output('-will write MIRIAD Image to disk')
            return
          else
            write='N'
            call output('-will not write MIRIAD Image to disk')
          endif
c  cursor position
	else if (key.eq.'P') then
	  flux = ary(nint(xx)+mid,nint(yy)+midy)
	  write(msg, *) 'x=',xx, '  y=',yy, '  value=',flux
	  call output(msg)
c clear spectrum pos-vel stacks
        else if (key.eq.'D') then
          call output('-cleared spec/pos-vel stacks')
          nspec=0
          ncut=0
c define box and find integral and rms.
	else if (key.eq.'B') then
	  is = nint(xx/xy) + mid
	  ib = nint(yy/xy) + midy
	  write(msg, *) 'blc=(', is-mid, ',', ib-midy, ')'
	  call output(msg)
	else if (key.eq.'T') then
	  ie = nint(xx/xy) + mid
	  it = nint(yy/xy) + midy
	  write(msg, *) 'trc=(', ie-mid, ',', it-midy, ')'
	  call output(msg)
	else if (key.eq.'I') then
          call integrat(ary,nx,ny)
	else
          write(msg, *) 'Unknown key chosen: ' // key
          call output(msg)
	  go to 10
	end if
  	go to 10
113	format(' spectra(',i2,') x=',f8.3,'  y=',f8.3)
114     format(' cut(',i2,') x='f8.3,' y=',f8.3,' pa=',f8.3)
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine Integral(ary,vlsr,nx,ny,nc)
	implicit none
	integer nx,ny,nc
	real ary(nx,ny,nc),vlsr(nc)
c
c  List Integral and statistics within box.
c
c  Inputs:
c    ary	The image.
c    nx,ny,nc	Dimensions of image.
c    vlsr	Array of velocities.
c-----------------------------------------------------------------------
	include 'velplot.h'
	character*80 line
	real tmax,tmin,ave
	integer imin,jmin,imax,jmax,num,k
c
	call header(0)
        write(line,'(a,4i6,a)') 'Integral and rms in box (',
     *				is-mid,ib-midy,ie-mid,it-midy, ')'
	call output(line)
	write(line,'(a,a,a,a,a,a,a)') ' plane  ','  Velocity  ',
     *    ' Total Flux ','  Maximum   ','  Minimum   ','  Average   ',
     *	  '    rms     '
	call output(line)
	do k=1,nc
	  call maxmap(ary(1,1,k),nx,ny,is,ie,ib,it,
     *			tmax,imax,jmax,tmin,imin,jmin,ave,arms,num)
	  write(line,'(x,i4,x,6(x,f11.4))')
     *		k,vlsr(k),ave*num/cbof,tmax,tmin,ave,arms
	  call output(line)
	enddo
	call prompt(line,k,'>Type <cr> to continue')
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine integrat(ary,nx,ny)
	implicit none
	integer nx,ny
	real ary(nx,ny)
c
c  Find integral,max,min,rms in box.
c
c  Inputs:
c    ary	The array.
c    nx,ny	Dimensions of array.
c----------------------------------------------------------------------c
	include 'velplot.h'
	character line*80
	real tmax,tmin,ave
	integer imin,jmin,imax,jmax,num
c
        write(line,'(a,4i6,a)') 'Integral and rms in box (',
     *				is-mid,ib-midy,ie-mid,it-midy, ')'
	call output(line)
	write(line,'(a,a,a,a,a,a)') '  Velocity  ',
     *    ' Total Flux ','  Maximum   ','  Minimum   ','  Average   ',
     *	  '    rms     '
	call output(line)
	call maxmap(ary,nx,ny,is,ie,ib,it,
     *			tmax,imax,jmax,tmin,imin,jmin,ave,arms,num)
	call header(0)
	write(line,'(6(x,f11.4),a)')
     *		vel, ave*num/cbof, tmax, tmin, ave, arms, ' Jy'
	call output(line)
	write(line,'(24x,4(x,f11.4),a)')
     *		tmax*dperjy, tmin*dperjy, ave*dperjy, arms*dperjy, ' K'
	call output(line)
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine GetMom(nmom)
	implicit none
	integer nmom
c
c  Get moment to be plotted for velomap
c
c  Outputs:
c    nmom	Moment of map to be computed.
c-----------------------------------------------------------------------
	include 'velplot.h'
	real value
c
	goto 10
222	continue
	call output('moment must be integer in range (-1 to 4)')
10	continue
	call output(' ')
	call output(' map computed:')
	call output(' (0 = straight average [default])')
        call output(' (1 = velocity centroid from 1st moment)')
        call output(' (-1 = velocity of peak channel)')
        call output(' (2 = velocity width or variance from 2nd moment)')
        call output(' (3 = degree of skewness or asymmetry )')
        call output(' (4 = degree of excess, or "peakiness" )')
        call output('>Enter moment to compute (-1 to 4; default [0]): ')
	read(*, '(I5)', err=222) nmom
	if((nmom.lt.-1).or.(nmom.gt.4)) goto 222
c
	call promptf(value,'f12.5',
     *    '>Enter blanking cutoff level',cutoff)
	cutoff = value
c
	if(nmom.ne.0) then
	  units = ' '
	  call SetCont
	endif
c
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine SetCont
	implicit none
c
c  Set contour levels and label interval
c----------------------------------------------------------------------c
	include 'velplot.h'
	character*80 ans,line
	real cmin,cmax,cint
	integer l,i
c
	call output(' ')
	call prompt(percent,l,
     *    '>Enter contours as % (Y), Absolute, [use previous levels] :')
	if(l.eq.0)return
	call ucase(percent)
	call prompt(ans,l,'Enter contour List or Range (L/[R]) :')
	call ucase(ans)
	if(ans.eq.'L') then
	  write(line, '(a)') '>Enter levels (max 10, end=-9999.):'
	  call output(line)
          do i=1,10
            levels(i)=0.0
          enddo
          do i=1,10
	   write(line,'(a,i2,a)') '>Enter: level(',i,')='
           call output(line)
           read(5,*) levels(i)
           if(levels(i).eq.-9999. .or. i.eq.10)then
             nlevels=i-1
             goto 129
           endif
          enddo
	else
	  write(line, '(a)') '>Enter min,max,interval :'
	  call output(line)
	  read(5,*) cmin,cmax,cint
	  nlevels =1
	  levels(1)=cmin
	  do while(levels(nlevels).lt.cmax.and.nlevels.lt.10)
	    nlevels = nlevels + 1
	    levels(nlevels) = levels(nlevels-1) + cint
 	  enddo
	endif
129     end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine SetGray
	implicit none
c
c  Set grayscale levels 
c----------------------------------------------------------------------c
	include 'velplot.h'
	character msg*80
        integer l
c
c Grayscale default to map min/max or       
c enter values for bg and fg
c
	call output(' ')
	call prompt(defgray,l,
     *    '>Use map min/max for graysacle (Y/[N]) :')
	call ucase(defgray)
	if(defgray.eq.'Y')return
        call output(' pgplot grayscale shade is a number in the ')
        call output(' range 0 to 1 obtained by linear interpolation')
        call output(' between the background and foreground level,') 
        call output('e.g. shade=[A(i,j)-bg]/[fg-bg] ')
	write(msg, 122)
122	format(/,'>Enter grayscale background (bg) level:')
	call output(msg)
        read(*,*) bg
	write(msg, 123)
123	format(/,'>Enter grayscale foreground (fg) level:')
	call output(msg)
        read(*,*) fg
        end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine GetRange(imaps,vmin,vmax,vlsr,nc)
	implicit none
	integer imaps,nc
	real vmin(1),vmax(1),vlsr(1)
c
c  Get range of velocities to plot.
c
c  Inputs:
c    vlsr(nc)	Array of velocities.
c    nc		Number of channels in image.
c  Outputs:
c    imaps	Number of velocity intervals.
c    vmin,vmax  Array of velocity intervals.
c-----------------------------------------------------------------------
	real vfirst,vlast,vwidth,vinc,vstart,vend
c
	vfirst=0.
	vlast=0.
	vinc=0.
	vstart=0.
	vend=0.
	vwidth=0.
c
	call output(' ')
	call output(
     *      '>Enter first and last velocity, width and increment: ')
	read(*,*) vfirst,vlast,vwidth,vinc
c
c  Find min and maximum velocity.
c
	vstart=min(vlast,vfirst)
	vend=max(vlast,vfirst)
	if(vinc.eq.0.) vinc = vlsr(2)-vlsr(1)
	if(vstart.eq.0. .and. vend.eq.0.) then
	  vstart = vlsr(1)
	  vend = vlsr(nc)
	endif
c
c  Give VINC correct sign to go from VSTART to VEND
c
	vinc=sign(vinc,(vend-vstart))
	vmin(1) = vstart - abs(vwidth)/2.
	vmax(1) = vstart + abs(vwidth)/2.
	imaps=1
	if(vinc.gt.0.) then
	  do while( vmax(imaps).lt.max(vstart,vend) )
	    imaps=imaps+1
	    vmin(imaps)=vmin(imaps-1)+vinc
	    vmax(imaps)=vmax(imaps-1)+vinc
	  end do
	else
	  do while( vmin(imaps) .gt. min(vstart,vend) )
	    imaps=imaps+1
	    vmin(imaps)=vmin(imaps-1)+vinc
	    vmax(imaps)=vmax(imaps-1)+vinc
	  end do
	end if
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine ListMaps(imaps,vmin,vmax,vlsr,nc)
	implicit none
	integer imaps,nc
	real vmin(1),vmax(1),vlsr(1)
c
c  List the maps available and current selection
c
c  Inputs:
c    nc		Number of channels in image.
c    vlsr(nc)	Array of velocities.
c  Outputs:
c    imaps	Number of velocity intervals.
c    vmin,vmax  Array of velocity intervals.
c-----------------------------------------------------------------------
	character msg*80
        integer i
c
c --- maps available ---
c
	call output(' ')
	write(msg,'(a,f12.3,a,f12.3)') ' First velocity channel:',
     *		 	vlsr(1), '   Last velocity channel:',vlsr(nc)
	call output(msg)
	write(msg,'(a,f12.3)') ' Velocity increment :',vlsr(2)-vlsr(1)
	call output(msg)
	call output(' ')
c
c --- current selection of maps ---
c
	if(imaps.ne.0) then
	  call output(' --- current selection of maps ---')
	  do i=1,imaps
	    write(msg, 109) i, vmin(i), vmax(i)
	    call output(msg)
	  end do
109	  format(' map(',i2,') velocity range: (',f12.3,' to',f12.3,')')
	else
	  call output(' --- no current selection of maps ---')
	end if
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine GetList(imaps,vmin,vmax,vlsr,nc)
	implicit none
	integer imaps,nc
	real vmin(1),vmax(1),vlsr(1)
c
c	Get list of velocity intervals.
c
c  Inputs:
c    vlsr(nc)	Array of velocities.
c    nc		Number of channels in image.
c  Outputs:
c    imaps	Number of velocity intervals.
c    vmin,vmax  Array of velocity intervals.
c-----------------------------------------------------------------------
	real velmin,velmax,swap
	integer i,k,length
	character line*80
#ifdef cft
c
c  External
c
	character substr*80
#endif
c
	  goto 9
20	  continue
	  call output(' ')
	  write(line, *) 'Try again; map number must be in list,' //
     *	       'or increment list by one'
	  call output(line)
	  call ListMaps(imaps,vmin,vmax,vlsr,nc)
9	  call output(' ')
	  call output('---- Enter list of velocity intervals ---')
	  call output('>Type -n to delete map n')
          call output('      -99 to delete all')
	  call output('       L to list')
          call output('      <cr> to use the current list')
10	  call prompt(line,length,
     *      '>Enter map number and velocity interval (N,Vmin,Vmax): ')
	  if(length.eq.0) return
#ifdef cft
	  read(substr(line,1),101,err=20) i
	  read(substr(line,2),103) velmin
	  read(substr(line,3),103) velmax
101	  format(i10.0)
103	  format(f20.0)
#else
	  read(line(1:length),102,err=20) i,velmin,velmax
102	  format(i10,2f20.0)
#endif
c
c  end of list; plot maps
	  if(i.eq.0) goto 50
c  swap
	  if(velmin .gt. velmax) then
	    swap = velmin
	    velmin = velmax
	    velmax = swap
	  endif
c  add or change map
	  if (i.ge.1 .and. i.le.min(imaps+1,25) ) then
	    vmin(i) = velmin
	    vmax(i) = velmax
	    imaps = max(i,imaps)
c  delete map
	  else if (-i.ge.1 .and. -i.le.imaps ) then
	    k = -i
	    do i = k,imaps-1
	      vmin(i) = vmin(i+1)
	      vmax(i) = vmax(i+1)
	    end do
	    imaps = imaps-1
c  start new list
	  else if(i.le.-99) then
            call output('All maps deleted from list')
	    imaps = 0
	  else
  	    goto 20
	  endif
	  goto 10
50	  return
	  end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine plotanot(cf,cmaj,cmin,cpa)
	implicit none
	real cf,cmaj,cmin,cpa
c
c  Annotate plots.
c----------------------------------------------------------------------c
	include 'velplot.h'
	real rae(3),dece(3),rts,pi,yloc
	common/back/rae,dece
	character line*80
	integer i,j
	real scale,absmax
	pi = 3.141592654
	rts = 3600.*180./pi
c
c  Set pg viewport to right side.
c
        call pgswin(0.0,1.0,0.0,1.0)

	call header(0)
c  object
        call pgtext(0.,0.95,object)
c  ra and dec
	write(line,'(2i3,f6.2,x,2i3,f5.1)') int(rae(1)),
     *    int(rae(2)),rae(3),int(dece(1)),int(dece(2)),dece(3)
        call pgtext(0.,0.9,line)
c  epoch
	write(line,'   (''('',f5.0,'')'')') epoch
        call pgtext(0.0,0.86,line)
c  pixel 
	write(line,'(f9.2,''  x'',f9.2)') xy, xy
        call pgtext(0.,0.82,line)
c  freq
	write(line,'(''RestFreq: '',F10.5,'' GHz'')') restfreq
        call pgtext(0.0,0.66,line)
c  vel
	write(line,'(''Velocity: '',F10.3,'' km/s'')') vel
        call pgtext(0.0,0.62,line)
c  delv
	write(line,'(''Width:    '',F10.3,'' km/s'')') abs(delv)
        call pgtext(0.0,0.58,line)
c  file
        write(line,'(''filename:'',x,A)') file
        call pgtext(0.0,0.54,line)
c  beam
	if(bmaj.gt.0.) then
	  write(line,'(''Beam'',1x,f6.2,1x,f6.2,1x,f6.1)')
     *		bmaj*rts,bmin*rts,bpa
          call pgtext(0.0,0.50,line)
	end if
c  convolving beam
	if(cmaj.gt.0.) then
	  write(line,'(''Conv'',1x,f6.2,1x,f6.2,1x,f6.1)')
     *		cmaj,cmin,cpa
          call pgtext(0.0,0.46,line)
        endif
c  bunit
        write(line,'(''Map Unit:'',x,A)') bunit
        call pgtext(0.0,0.40,line)
c  dperjy
	write(line,'(''K/Jy ='',1pg10.3)') dperjy
        call pgtext(0.0,0.36,line)
c
c  convert units for caption using cf.
c
        if(maptype.eq.'SPECTRA')then
         goto 202
        endif
c  max
	write(line,'(''Maximum:'',1pg10.3,X,A)') amax*cf,units
        call pgtext(0.0,0.30,line)
c  min
	write(line,'(''Minimum:'',1pg10.3,x,a)') amin*cf,units
        call pgtext(0.0,0.26,line)
c  rms
	write(line,'(''Rms:    '',1pg10.3,x,a)') arms*cf,units
        call pgtext(0.0,0.22,line)
c  contours
	scale=1.
	if(percent.eq.'Y')then
	  absmax=abs(amax)
	  if(cneg.eq.'Y') absmax=(max(abs(amax),abs(amin)))
	  scale=absmax*cf/100.
	endif
        write(line,'(''Contour Levels:'',x,A)') units
        call pgtext(0.0,0.18,line)
c  levels
	j=0
	yloc=14.
	do while(j.lt.nlevels .and. yloc.gt.2.)
	  write(line,'(3(1pg10.3,x))')
     *      (levels(j+i)*scale,i=1,min(3,nlevels-j))
          call pgtext(0.,yloc/100,line)
	  j=j+3
	  yloc=yloc-4.
	enddo
c
c  Don't forget to Restore original plot window.
c
202     continue
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine plotcord( iopt,nx,ny,xmin,xmax,ymin,ymax )
	implicit none
	integer iopt,nx,ny
	real xmin,xmax,ymin,ymax
c
c  Find the plot limits.
c
c  Inputs:
c    iopt=0 for relative coords; iopt=1 for absolute coords
c    nx,ny	Size of array.
c    xy,ras,decs come from the map header
c  Outputs:
c    xmin,xmax,ymin,ymax	plot limits
c----------------------------------------------------------------------c
	include 'velplot.h'
	character msg*80
	real rasec,decsec
c
c  Relative coords.
c
	if(iopt .eq. 0) then
	  xmin = -xy*(1-mid)
	  xmax = -xy*(nx-mid)
	  ymin = xy*(1-midy)
	  ymax = xy*(ny-midy)
c
c  Absolute coords.
c
	else if(iopt .eq. 1) then
	  rasec = ras * 206264.806/15.
	  decsec = decs * 206264.806
	  xmin = rasec - xy*(1-mid)/cos(decs)/15.
	  xmax = rasec - xy*(nx-mid)/cos(decs)/15.
	  ymin = decsec + xy*(1-midy)
	  ymax = decsec + xy*(ny-midy)
          write(msg, *) cos(decs),cos(decs)/15.,rasec,decsec
          call output(msg)
	endif
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine plotcon(ary,nx,ny,cf,tr)
	implicit none
	integer nx,ny
	real ary(nx,ny),cf,tr(6)
c
c  Contour plots.
c  Gray scale.
c			MCHW Dec 1985
c	cf is the conversion factor from map units - mchw 7Feb86
c----------------------------------------------------------------------c
	include 'velplot.h'
	integer i,imin,imax,jmin,jmax,num,loop,nloop
	real clevels(10),ave,scale,absmax
c
c  Set scale for contour levels. (amin,amax,arms are used in plotanot)
c
	call maxmap(ary,nx,ny,1,nx,1,ny,amax,imax,jmax,amin,
     *				imin,jmin,ave,arms,num)
	if(cf.eq.0.)cf=1.
	scale=1./cf
	if(percent.eq.'Y')then
	  absmax=abs(amax)
	  if(cneg.eq.'Y') absmax=(max(abs(amax),abs(amin)))
	  scale=absmax/100.
	endif
c
c  Plot grayscale if required
c
        if(gray.eq.'Y')then
          if(defgray.eq.'Y')then
            fg=amax
            bg=amin
          endif
          call pggray(ary,nx,ny,1,nx,1,ny,fg,bg,tr)
        endif
c
c  Set linetype and plot contours
c
        call pgsls(1)
	nloop=1
	if(cneg.eq.'Y')nloop=2
c
	do loop=1,nloop
	  do i=1,nlevels
	    clevels(i) = levels(i)*scale
	  enddo
	  call pgcont(ary,nx,ny,1,nx,1,ny,clevels,nlevels,tr)
	  if(cneg.eq.'Y') then
            call pgsls(2)
	    scale=-scale
	  endif
	enddo
        call pgsls(1)
c
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine maxmap(ary,nx,ny,is,ie,js,je,
     *		amax,imax,jmax,amin,imin,jmin,ave,rms,num)
	implicit none
	integer nx,ny,is,ie,js,je,imax,jmax,imin,jmin,num
	real ary(nx,ny),amax,amin,ave,rms
c
c  Find max, min and rms in specified region of real array.
c			MCHW Jan 1986
c    pixel blanking: ignore values .lt. -99999.  wh feb 87
c
c  Inputs:
c    ary	array of values.
c    nx,ny	dimensions of ary
c    is,ie,js,je  L,R,B,T of region to search.
c
c  Outputs:
c    max,min	maximum and minimum in specified region.
c    imax,jmax	position of maximum.
c    imin,jmin	position of minimum.
c    ave,rms	average and rms in specified region.
c    num	number of pixels greater than -99999
c----------------------------------------------------------------------c
	integer i,j
	real a,sum,sumsq
c
	amax = -1.e9
	amin = 1.e9
	sum = 0.
	sumsq = 0.
	num = 0
	do j = js,je
	  do i = is,ie
	    a = ary(i,j)
	    if(abs(a+99999.).gt.0.5) then
	      sum = sum + a
	      sumsq = sumsq + a*a
	      num = num + 1
	      if(a.gt.amax) then
		amax = a
		imax = i
		jmax = j
	      else if (a.lt.amin) then
		amin = a
		imin = i
		jmin = j
	      endif
	    endif
	  enddo
	enddo
c
	if(num.gt.0)then
	  ave = sum/num
	  rms = sqrt(sumsq/num) - ave*ave
	endif
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine plotint(ary,nx,ny)
	implicit none
	integer nx,ny
	real ary(nx,ny)
c
c  Integer plot.
c
c  Inputs:
c    ary	The array to print.
c    nx,ny	Dimensions of array.
c----------------------------------------------------------------------c
	include 'velplot.h'
	integer line(40)
	character text*80
	real tmax,tmin,ave,scale,rts
	integer imin,jmin,imax,jmax,num,i,j,iend
	rts=3600.*180./3.141592654
c
c  Title.
c
	call output('Image: '//file)
	call output('source: '//object//'  map units: '//bunit)
	write(text,'('' summary for box '',4i5)')
     *			 1-mid, nx-mid, 1-midy, ny-midy
	call output(text)
c
c  Find max,min,rms in selected region.
c
	call maxmap(ary,nx,ny,1,nx,1,ny,tmax,imax,jmax,tmin,
     *				imin,jmin,ave,arms,num)
	call header(0)
	write(text,'(''Maximum='',1pg10.3,''at '',2i5)')
     *		tmax, imax-mid, jmax-midy
	call output(text)
	write(text,'(''Minimum='',1pg10.3,''at '',2i5)')
     *		tmin, imin-mid, jmin-midy
	call output(text)
	write(text,'(''Average='',1pg10.3,''  rms='',1pg10.3)')ave,arms
	call output(text)
	write(text, *) 'Beam ',bmaj*rts,bmin*rts,bpa, '  cbof=',cbof
	call output(text)
	if(cbof.ne.0.)then
	  write(text, *) 'Total flux in box =',ave*num/cbof
	  call output(text)
	  write(text, *) 'Brightness max, min and rms are',
     *		 tmax*dperjy,tmin*dperjy,arms*dperjy
	  call output(text)
	endif
c
c  Convert plot to integers.
c
	scale = 100./max(abs(tmax),abs(tmin))
	write(text, *) 'units are: ',1./scale,' ',bunit
	call output(text)
	iend = min(nx,24)
	write(text, 103) -midy,(i-mid,i=1,iend)
	call output(text)
	do j=ny,1,-1
	  do i=1,iend
	    if(ary(i,j).gt.-99999.)then
	      line(i)=scale*ary(i,j)
	    else
	      line(i)=0
	    endif
	  enddo
	  write(text, 103) j-midy,(line(i),i=1,iend)
	  call output(text)
	enddo
	write(text, 103) -midy,(i-mid,i=1,iend)
	call output(text)
103	format(x,i4,x,24i3)
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine spectra(ary,vlsr,nx,ny,nc)
	implicit none
	integer nx,ny,nc
	real ary(nx,ny,nc),vlsr(nc)
c
c  Plot spectra at selected positions in image.
c
c  Inputs:
c    ary	The image.
c    nx,ny,nc	Dimensions of image.
c    vlsr	Array of velocities.
c-------------------------------------------------------------------------c
	include 'velplot.h'
        character*10    ichar10
        integer         len10,len10a,i10
	real t(256),tk(256,49)
	character*40 outfile
	integer maxspec,ix,iy,lu,iostat
	real ymin,ymax,cf,cmaj,cmin,cpa,xtlc,ytlc
	real x0,y0,vmin,vmax,x,y,wt,pa,step,sum,sumc
	integer windx,windy,ncon,ns,nsmooth
	integer spec,c,i,j,k,ig,nrows,ncols,length,ichannel,lwidth
	character text*110
	character*80 oldevice,line
	character*1 ans,fix,smooth,sym,noplot
	character*28 label,xlabel,ylabel
	character*8 xchar,ychar
c  convolution array maximum size
	real con(99,99,4,4)
c
	integer nspec,ngauss(49)
	real xc(49),yc(49)
	real gauss(49,3,10),gausserr(49,3,10)
	common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c
	real rts
	parameter(rts=3600.*180./3.141592654)
c
	data maxspec/49/
c
#ifdef cft
c
c  External
c
	character substr*80
#endif
c
c  Introduction.
c
	call output(' ')
	call output('Plot spectra at selected positions.')
	call output('(x,y) positions are in (HA,DEC) directions.')
	call prompt(ans,length,
     *    '>Type H for Help, E to Exit, <cr> to continue :')
	call ucase(ans)
	if(ans.eq.'H')then
	  call output(' ')
	  call output(
     *	    'Positions are selected with the cursor on an x-y map')
	  call output('or can be entered as a GRID or a LIST here.')
	  call output(
     *	    'Spectra can also be convolved in space and velocity')
	  call output(
     *	    'and written as ascii files for further processing.')
	call output(' ')
	end if
	if(ans.eq.'E')then
	  call output('No spectra selected')
	  return
	endif
c
c  List current selection of spectra.
c
	goto 9
21	call output(' ')
	call output('--- no spectra selected ---')
	return
11  	    call output(' ')
  	    call output(
     *		'Try again; spectrum number must edit or extend list')
9	if (nspec.ne.0) then
	  call output(' ')
	  call output('--- current selection of spectra ---')
	  do i=1,nspec
	    write(line, 109) i,xc(i),yc(i)
	    call output(line)
	  end do
	  else
	    call output('--- no current selection of spectra ---')
	  end if
109	format(' spectra(',i2,') (x,y) arcsecs: (',f9.3,',',f9.3,')')
c
c  Select List or grid of positions for spectra.
c
	call output(' ')
	call output('>Enter G to enter a grid of positions,')
	call prompt(ans,length,
     * 	  '      <cr> to edit or enter a list of positions :')
	call ucase(ans)
c
c  Enter grid of positions for spectra.
c
	  if(ans.eq.'G') then
	    call output(' ')
	    call output(
     *            '>Enter tlc (x,y), and PA of grid :')
	    read(5,*) xtlc,ytlc,pa
	    call output('  (Grid PA is measured from N through E)')
	    call output(
     *		'>Enter interval(arcsec), nrows(along PA) and ncols:')
	    read(5,*) step,nrows,ncols
	    pa = pa*3.141592654/180.
	    nspec=min(nrows*ncols,maxspec)
	    do ig=1,nspec
	      xc(ig) = xtlc + real(mod(ig-1,ncols))*step*cos(pa)
     *			+ real(((ig-1)/ncols))*step*sin(pa)
	      yc(ig) = ytlc + real(mod(ig-1,ncols))*step*sin(pa)
     *			- real(((ig-1)/ncols))*step*cos(pa)
	    enddo
	    call output('--- current selection of spectra ---')
	    do i=1,nspec
	      write(line, 109) i,xc(i),yc(i)
	      call output(line)
	    end do
	    call output(' ')
	    call prompt(ans,length,'selection OK? ([Y]/N):')
	    call ucase(ans)
	    if(ans.eq.'N') goto 9
	    goto 20
	  endif
c	  
c  Enter list of positions for spectra.
c
	  call output(' ')
	  call output('---- Enter list of spectra ----')
	  call output('>Type -n to delete spectra n')
          call output('      -99 to delete all')
	  call output('       L to list')
          call output('      <cr> to use the current list')
10	  call prompt(line,length,
     *		'>Enter spectrum number and position (n,X,Y): ')
	  if(length.eq.0) goto 20
#ifdef cft
	  read(substr(line,1),101,err=11) i
	  read(substr(line,2),103) x
	  read(substr(line,3),103) y
101	  format(i10.0)
103	  format(f20.0)
#else
 	  read(line(1:length),102,err=11,end=21) i, x, y
102	  format(i10,2f20.0)
#endif
	  if(i.eq.0) goto 20
c  end of list; plot spectra
	  if(i.ge.1 .and. i.le.min(nspec+1,maxspec)) then
c  add or change point
	    xc(i) = x
	    yc(i) = y
	    nspec = max(i,nspec)
	  else if (-i.ge.1 .and. -i.le.nspec) then
c  delete point
	    k = -i
	    do i=k,nspec-1
	      xc(i) = xc(i+1)
	      yc(i) = yc(i+1)
              ngauss(i)=ngauss(i+1)
              do j=1,ngauss(i+1)
                gauss(i,1,j)=gauss(i+1,1,j)
                gauss(i,2,j)=gauss(i+1,2,j)
                gauss(i,3,j)=gauss(i+1,3,j)
                gausserr(i,1,j)=gausserr(i+1,1,j)
                gausserr(i,2,j)=gausserr(i+1,2,j)
                gausserr(i,3,j)=gausserr(i+1,3,j)
              enddo
	    enddo
	    nspec = nspec-1
	  else if(i.le.-99) then
c  start new list
	    nspec = 0
	  else
  	    call output(' ')
  	    call output(
     *		'Try again; spectrum number must edit or extend list')
c********1*********2*********3*********4*********5*********6*********7**
	    goto 9
	  endif
	  goto 10

20	if(nspec.eq.0) then
  	  call output(' ')
  	  call output('--- no spectra selected ---')
	  return
	endif
c
c  Write info' into log if Gaussian fitting.
c
	if(lgaufit.eq.'Y')then
	  call output(' ')
	  call output('GAUSSIAN FITS TO SPECTRA')
	  call LogWrit(' ')
	  call LogWrit('GAUSSIAN FITS TO SPECTRA')
	endif
c
c  Set up convolution function.
c
	call convsize(cmaj,cmin,cpa,xy,ncon)
	call convinit(cmaj,cmin,cpa,xy,ncon,con)
	write(line,'(a,3f10.4)')
     *		'original   beam ', bmaj*rts, bmin*rts, bpa
	  call output(line)
	  if(lgaufit.eq.'Y') call LogWrit(line)
	write(line,'(a,3f10.4)')
     *		'convolving beam ', cmaj, cmin, cpa
	  call output(line)
	  if(lgaufit.eq.'Y') call LogWrit(line)
	write(line,'(a)')
     *		'Units of intensity using the original beam'
	  call output(line)
	  if(lgaufit.eq.'Y') call LogWrit(line)
c
c  Inquire about smoothing.
c
	call prompt(smooth,length,
     *	  '>Smooth spectra (H=Hanning, B=Boxcar, [None]) :')
	call ucase(smooth)
	if(smooth.eq.'B')then
	  call output(
     *      '>Enter number of channels to average (Integer) :')
	  read (5,*) nsmooth
	  if(nsmooth.lt.1) nsmooth = 1
	end if
c
c  Omit plots and Gaussian fits ?
c
	call prompt(noplot,length,
     *    '>Omit plots and Gaussian fits ? (Y/[N]) :')
	call ucase(noplot)
	if(noplot.eq.'Y') goto 60
c
c  Inquire about fixed scale spectra.
c
	call prompt(fix,length,
     *    '>Fix scale for spectra ? (Y/[N]) :')
	call ucase(fix)
	if(fix.eq.'Y') then
	  call output('>Enter Vmin,Vmax,Ymin,Ymax in selected units :')
	  read(*,*) Vmin,Vmax,Ymin,Ymax
	endif
c  Set maptype and save plot device type from menu.
c
	maptype = 'SPECTRA'
	oldevice=device
	lwidth = 1
c
c  Initialize plot device.
c
        call getpanel(nspec,windx,windy)
50      call pgbeg(0,device,windx,windy)
	call pgask(.FALSE.)
	call pgslw(lwidth)
c
c  Calculate the conversion factor for map units.
c
60	call header(0)
	cf = 1.
	if (units.eq.'K') cf = dperjy
c
c  Loop through list of spectra.
c
	do spec = 1,nspec
c
c  Check for plot annotation.
c
	  if(noplot.ne.'Y')then
	    call pgpage
	    if(windx*windy.eq.1 .and. alabel.eq.'Y')then
	      call pgsvp(0.1,0.7,0.1,0.9)
	    else
	      call pgsvp(0.1,0.9,0.1,0.9)
	    endif
	  endif
c
c  Find position of spectra in the array.
c
	  x0 =  xc(spec)/xy + mid
	  y0 =  yc(spec)/xy + midy
	  ix = nint(x0)
	  iy = nint(y0)
	  if(ix.lt.1 .or. ix.gt.nx .or. iy.lt.1 .or. iy.gt.ny) then
	    call output('requested spectra is outside array')
	    if(noplot.ne.'Y') call pgend
	    goto 30
	  endif
c
c  Convolve array at each position; return convolved spectra in t
c
	  call conv(ary,nx,ny,nc,x0,y0,ncon,con,t,wt)
c
c  Boxcar smoothing if requested.
c
	  if (smooth.eq.'B')then
	    do ichannel = 1,nc,nsmooth
	      sum = 0.
	      sumc = 0.
	      do c = ichannel,min(ichannel+nsmooth-1,nc)
		sum = sum + t(c)
		sumc = sumc + 1.
	      end do
	      do c = ichannel,min(ichannel+nsmooth-1,nc)
		t(c) = sum / sumc
	      end do
	    end do
c
c  Hanning smooth spectrum if requested. Note that the end channels
c     are not affected and the number of pixels stays the same.
c
	  else if(smooth.eq.'H') then
            do c = 2,nc-1
	      t(c) = (t(c) + (t(c-1)+t(c+1))/2.)/2.
 	    enddo
 	  endif
c
c  Save convolved spectra and write to array for plotting.
c
	  if(fix.ne.'Y') then
	    vmin = vlsr(1)
 	    vmax = vlsr(1)
 	    ymin = t(1)*cf
	    ymax = t(1)*cf
 	  end if 

	  do k=1,nc
	    t(k) = t(k) * cf
	    tk(k,spec) = t(k)
	    if (fix.ne.'Y') then
	      ymin = min(ymin,t(k))
	      ymax = max(ymax,t(k))
	      vmin = min(vmin,vlsr(k))
	      vmax = max(vmax,vlsr(k))
	    end if
	  end do
	  if(noplot.eq.'Y') goto 70
c
c  Set max and min for plot.
c
	  if (fix.ne.'Y') then
	    vmin = vmin - 0.05 * (vmax - vmin)
	    vmax = vmax + 0.05 * (vmax - vmin)
	    ymin = ymin - 0.05 * (ymax - ymin)
	    ymax = ymax + 0.05 * (ymax - ymin)
	  end if
c
c  Plot spectra.
c
          call pgswin(vmin,vmax,ymin,ymax)
          call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
          call pgHline(nc,vlsr,t,2.)
c
c  Label spectra with offset coords.
c
          write(xchar,116) xc(spec)
          write(ychar,116) yc(spec)
          write(sym,115) char(spec+64)
115       format(a)
116	  format (f8.2)
c
c  include symbol with spectrum position or not
c
          if(pspec.eq. 'Y') then
            label = sym//' ('//xchar(1:index(xchar,'.')+2)//','
     *		//ychar(1:index(ychar,'.')+2)//')'
          else
            label = ' ('//xchar(1:index(xchar,'.')+2)//','
     *		//ychar(1:index(ychar,'.')+2)//')'
          endif
c
          xlabel='Velocity (km/s)'
          if (units.eq.'K') then
            ylabel='Kelvin'
          else if (units.eq.'J') then
            ylabel='Janskys'
          else
            ylabel='Map units'
          endif
          call pglab(xlabel,ylabel,label)

          if(windx*windy.eq.1 .and. alabel.eq.'Y')then
            call pgsvp(0.72,0.9,0.1,0.9)
            call plotanot(cf,cmaj,cmin,cpa)
            call pgsvp(0.1,0.7,0.1,0.9)
            call pgswin(vmin,vmax,ymin,ymax)
          endif
c
c If not harcopy device, then call routine to fit gaussians
c Otherwise, call routine to plot gaussians
          call pgqinf('HARDCOPY',ans,length)
          if (ans(1:1).eq.'N' .and. lgaufit.eq.'Y')then
            call gaufit(nc,vlsr,t,spec,arms)
          endif 
          if (lgauplot.eq.'Y') then  
            call gauplot(nc,vlsr,t,spec)
          endif
c
c  If this is the last window close the plot 
c
	if (spec.eq.nspec) then
          call pgiden
          call pgqinf('HARDCOPY',ans,length)
          call pgend
          if (ans(1:1).ne.'Y')then
            call output(' ')
	    call prompt(ans,length,'Another PGPLOT device? (Y/[N])')
	    call ucase(ans)
	    if(ans(1:1).eq.'Y')then
              call prompt(device,length,'>Enter new PGPLOT device: ')
              call lcase(device)
	      call prompt(ans,length,'>Enter line width [1]: ')
	      if(length.ne.0)then
		read(ans,'(i1)') lwidth
	      endif
	      goto 50
	    endif
	  endif
	endif
30	enddo
c
c  Restore original plot device.
c
	device=oldevice
c
c  Write out convolved spectra to ASCII file.
c
70	line='>Enter filename for spectra (<cr> to continue):'
	call prompt(outfile,length,line)
	if(length.gt.0) then
	  ns = min(nspec,12)
	  if (nspec .gt. 12) then
	    call output('only first 12 spectra written to '//outfile)
          endif
	  call TxtOpen(lu,outfile,'new',iostat)
	  if(iostat.eq.0) write(text,'(a,a,4x,f11.6,a)')
     *	    'Spectra for ',object,restfreq,' GHz'
	  length=12 + len(object) + 15 + 4
	  call TxtWrite(lu,text,length,iostat)
	  if(iostat.eq.0) write(text,'(a,2f12.3,a,f6.1,a)')
     *	  'Convolving beam: ',cmaj,cmin,' arcsecs, pa: ',cpa,' degrees'
	  length = 17 + 12 + 12 + 14 + 6 + 8
	  call TxtWrite(lu,text,length,iostat)
	  if(iostat.eq.0) write(text,'(a)') 'Positions (x,y) arcsecs'
	  call TxtWrite(lu,text,23,iostat)
	  k=0
	  dowhile(iostat.eq.0 .and. k.lt.ns)
	    write(text,'(4(a,2f7.2,a))')
     *        ('(',xc(i+k),yc(i+k),') ',i=1,min(4,ns-k))
	    length = min(4,ns-k)*(1+2*7+2)
	    call TxtWrite(lu,text,length,iostat)
	    k=k+4
	  enddo
	  dowhile(length.gt.0 .and. iostat.eq.0)
	    call prompt(text,length,'Enter comment :')
	    call TxtWrite(lu,text,length,iostat)
	  enddo
	  k=1
#ifdef vms
	  do while(iostat.eq.0 .and. k.le.nc)
	    write(text,110) vlsr(k),(tk(k,i),i=1,ns)
	    call TxtWrite(lu,text,10+10*ns,iostat)
	    k=k+1
	  enddo
110	  format(x,f9.3,<ns>(1x,f9.3))
#else
c
c  This eliminate the need to use <ns> format, which works for f77 -g
c  but not without, and not on cray.
c
          do while(iostat.eq.0 .and. k.le.nc)
             write(ichar10,'(1x,f9.3)')vlsr(k)
             text(1:10) = ichar10(1:10)
             len10  = 11
             len10a = 20
             do i10=1,ns
                write(ichar10,'(1x,f9.3)')tk(k,i10)
                text(len10:len10a) = ichar10(1:10)
                len10  = len10 + 10
                len10a = len10 + 9
             enddo
            call TxtWrite(lu,text,10+10*ns,iostat)
            k=k+1
          enddo
#endif
	  call TxtClose(lu)
	endif
c
c  Write out gaussian fits to ASCII file.
c
        if(ngauss(1).gt.0)then
	  line='>Enter filename for gaussian fits (<cr> to continue):'
	  call prompt(outfile,length,line)
	  if(length.gt.0) call WrGauss(outfile,nc,vlsr,cmaj,cmin,cpa)
        endif
c
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine velmap(ary,vlsr,nx,ny,nc,v)
	implicit none
	integer nx,ny,nc
	real ary(nx,ny,nc),vlsr(nc),v(nx*ny)
c
c	average velocity maps in window in velocity
c
c  Inputs:
c    ary	The image.
c    nx,ny,nc	Dimensions of image.
c    vlsr	Array of velocities.
c    v		velocity-averaged map.
c----------------------------------------------------------------------c
	include 'velplot.h'
	real vmax(128),vmin(128),xmin,xmax,ymin,ymax,vmean,vwidth
	integer imaps,nmaps,i,j,k,nmom,length
	integer windx,windy,sym,lwidth
        real cf,tr(6)
	character*1 key
	character*9 c1,c2
	character*30 label
        character*80 ans,xlabel,ylabel,oldevice
        real sx(1),sy(1)
        real pat
        real xpts(2),ypts(2)
        real pi
        real cmaj,cmin,cpa
c
        integer ncut
	real xcut(128),ycut(128),pa(128)
        common /cuts/ xcut,ycut,pa,ncut
c
	integer nspec,ngauss(49)
	real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
	common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c
	data imaps/0/
c
        pi=3.141592654
	nmaps = 0
	do i=1,nx*ny
	  v(i)=0.0
	enddo
c
c  Introduction.
c
	call prompt(ans,length,'>Enter H for Help, <cr> to continue: ')
	call ucase(ans)
	if (ans(1:1).eq.'H')then
	  call output(' ')
	  call output('Plot x-y maps in selected velocity intervals.')
	  call output('Velocity intervals can be entered as a list,')
	  call output('or specified as a range of velocities at equal')
	  call output('intervals. The list can be edited. Maps can be')
	  call output('printed or written out for further processing.')
	  call output('Single plots allow further cursor options.')
	endif
c
c  Get map type to be plotted.
c
	call GetMom(nmom)
c
c  List velocities available and current selection of maps.
c
	call ListMaps(imaps,vmin,vmax,vlsr,nc)
c
c  Get velocities to be plotted.
c
	call output(' ')
	call prompt(ans,length,
     *   '>Enter List or Range of velocity intervals to plot ([L]/R): ')
c********1*********2*********3*********4*********5*********6*********7**
	call ucase(ans)
	call output(' ')
c
c  Enter List of velocity intervals or enter Range of velocities.
c
	if (ans(1:1).ne.'R')then
	  call GetList(imaps,vmin,vmax,vlsr,nc)
	else
	  call GetRange(imaps,vmin,vmax,vlsr,nc)
	end if

  	if(imaps.eq.0) then
  	  call output(' ')
  	  call output('--- no velocity intervals selected ---')
	  return
	end if
c
c  Save plot device type from menu.
c
	maptype = 'X-Y'
	oldevice = device
	lwidth = 1
c
c  Loop through list of maps. Reset image vel and width. 
c
        key='L'

60	vel = vlsr(1)
	delv = vlsr(2) - vlsr(1)
	if (write.ne.'Y' .and. apint.ne.'Y') then
          call getpanel(imaps,windx,windy)
	  call pgbeg(0,device,windx,windy)
	  call pgask(.FALSE.)
	  call pgslw(lwidth)
	endif
c
c  Calculate the conversion factor plotted units.
c
	call header(0)
	cf = 1.
	if (units.eq.'K') cf=dperjy
c
c  Average the sets of maps  - nmaps is the number of maps in the 
c	  average returned from velaver.
c
	do k=1,imaps
	  call output(' ')
	  call output('---- Integrated velocity map ----')
	  call velaver(vmin(k),vmax(k),vlsr,ary,nx,ny,nc,
     *	                    nmom,v,nmaps,vmean,vwidth)
c
c  Put mean velocity and width into map header for this plot.
c
	  vel = vmean
	  delv = vwidth
c
c  Plot or write out maps.
c
	  if (nmaps .gt. 0) then
	    if (write.eq.'Y') then 
	      call writemap (v,nx,ny,1)
	    else if (apint.eq.'Y') then
	      call plotint (v,nx,ny)
c
c  Plot maps.
c
	    else
              call pgpage
              if(windx*windy.eq.1 .and. alabel.eq.'Y')then
                call pgsvp(0.1,0.7,0.1,0.9)
              else
                call pgsvp(0.1,0.9,0.1,0.9)
              endif
c
c  Plot the box (with relative coords only)
c
c  Absloute coords are tricky since we want to keep the
c  equal pixel sizes in x and y
c              if(abscoord.eq.'Y')then 
c                call plotcord(0,nx,ny,xmin,xmax,ymin,ymax)
c                call pgwnad(xmin,xmax,ymin,ymax)
c                call pgtbox('BCNSTZ',0.0,0,'BCNSTZ',0.0,0)
c                xlabel='Right Ascension (1950)'
c                ylabel='Declination (1950)'
c                call plotcord(0,nx,ny,xmin,xmax,ymin,ymax)
c                call pgwnad(xmin,xmax,ymin,ymax)
c              else
                call plotcord(0,nx,ny,xmin,xmax,ymin,ymax)
                call pgwnad(xmin,xmax,ymin,ymax)
                call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
                xlabel='Relative R.A. (arcsec)'
                ylabel='Relative Dec. (arcsec)'
c              endif
c
c  Set up transformation array for contour routine
c  PGPLOT instructions not intuitive.    
              tr(2)=(xmax-xmin)/(nx-1)
              tr(1)=xmin-tr(2) 
              tr(3)=0.
              tr(5)=0.
              tr(6)=(ymax-ymin)/(ny-1)
              tr(4)=ymin-tr(6)
c  Plot contours and greyscale (if required)
              call plotcon(v,nx,ny,cf,tr)
c  Plot labels
c
	      write(c1,130) vmin(k)
	      write(c2,130) vmax(k)
130	      format(f9.3)
	      label = '                         '
	      label = c1//' - '//c2
              call pglab(xlabel,ylabel,label) 
c  Now plot the annotation
c  (had to wait for plotcon to get the info)
c  and have to set window back.
              if(windx*windy.eq.1 .and. alabel.eq.'Y')then
                call pgsvp(0.72,0.9,0.1,0.9)
                call plotanot(cf,cmaj,cmin,cpa)
                call pgsvp(0.1,0.7,0.1,0.9)
                call plotcord(0,nx,ny,xmin,xmax,ymin,ymax)
                call pgwnad(xmin,xmax,ymin,ymax)
              endif
c  redraw box
              call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
c
c  Transpose RA axis for cursor and overlay.
              call pgwnad(-xmin,-xmax,ymin,ymax)

c  Plot spectra positions, if any.
              if(pspec.eq.'Y' .and. nspec.gt.0)then
                  do j=1,nspec
                    sx(1)=xc(j)
                    sy(1)=yc(j)
                    sym=j+64
                    call pgpt(1,sx,sy,sym)
                  enddo
               endif
c  Plot pos-vel cuts, if any.
              if(pspec.eq. 'Y' .and. ncut.gt.0)then
                do j=1,ncut
                 pat=-tan(pi/2.-pa(j)*pi/180.)
                 xpts(1)=-1e4
                 ypts(1)=-1e4*pat+(ycut(j)-xcut(j)*pat) 
                 xpts(2)=1e4
                 ypts(2)=1e4*pat+(ycut(j)-xcut(j)*pat)
                 call pgline(2,xpts,ypts)
                 sx(1)=xcut(j)
                 sy(1)=ycut(j)
                 sym=j+96
                 call pgpt(1,sx,sy,sym)
                enddo
              endif
              call pgwnad(xmin,xmax,ymin,ymax)
c  If one map and not hardcopy then call for cursor.
c
              call pgqinf('HARDCOPY',ans,length)
              if(ans.ne.'YES' .and. windx*windy.eq.1)then 
                call pgwnad(-xmin,-xmax,ymin,ymax)
                call cursor(v,nx,ny,key)
                call pgwnad(xmin,xmax,ymin,ymax)
                if(key.eq.'E') goto 66
                goto 60
              endif
	    endif
66        endif
	enddo
c
c  Finished plotting maps.
c	
c  Replotting options.
c
  	if(write.ne.'Y' .and. apint.ne.'Y') then
	  call pgiden
	  call pgqinf('HARDCOPY',ans,length)
	  call pgend
	  if(ans.ne.'YES')then
	    call output(' ')
	    call prompt(ans,length,'Another PGPLOT device? (Y/[N])')
	    call ucase(ans)
	    if(ans(1:1).eq.'Y') then
	      call prompt(device,length,'>Enter new PGPLOT device: ')
	      call lcase(device)
	      call prompt(ans,length,'>Enter line width [1]: ')
	      if(length.ne.0)then
		read(ans,'(i1)') lwidth
	      endif
	      goto 60
	    endif
	  endif
	endif
c
c  Restore original plot device, and image vel and width.
c
  	device = oldevice
	vel = vlsr(1)
	delv = vlsr(2) - vlsr(1)
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine velaver(vmin,vmax,vlsr,ary,nx,ny,nc,
     *	                    nmom,v,nmaps,vmean,vwidth)
	implicit none
	integer nx,ny,nc,nmom,nmaps
	real vmin,vmax,vlsr(nc),ary(nx,ny,nc),v(nx*ny)
	real vmean,vwidth
c
c	Average velocity maps in the range vmin to vmax.
c
c  Inputs:
c    vmin,vmax		Velocity range to be averaged.
c    vlsr(nc)		Array of velocities.
c    ary(nx,ny,nc)	Array to be averaged.
c    nx,ny,nc		Dimensions of array.
c    nmom		The moment to be computed.
c  Outputs:
c    v(nx*ny)		Averaged map.
c    nmaps		Number of maps in the average.
c    vmean,vwidth	The mean velocity and width of the averaged map.
c----------------------------------------------------------------------c
	include 'velplot.h'
	character msg*80
	real vmom1(128*128),vmom2(128*128),vmom3(128*128),vmom4(128*128)
	real anorm,vcent,vcent2,sigma,sigma2,v2origin,v3origin,v3
	real wt,wtmom1,wtmom2,wtmom3,wtmom4,vsum
	integer i,j,k,m,noff,kmax
c
c  Velocity of peak channel.
c
	if(nmom.eq.-1)then
	  do j=1,ny
	    do i=1,nx
	      amax = cutoff
	      do k=1,nc
		if(vlsr(k).ge.vmin.and.vlsr(k).le.vmax
     *					.and.ary(i,j,k).gt.amax)then
		  kmax = k
		  amax = ary(i,j,k)
		endif
	      enddo
	      if(amax.gt.cutoff)then
		v(nx*(j-1)+i) = vlsr(kmax)
	      else
		v(nx*(j-1)+i) = -99999.
	      endif
	    enddo
	  enddo
	  nmaps=1
	  return
	endif
c
c  Initialize arrays.
c
	nmaps = 0
	anorm = 0.
	vmean = 0.
	vsum = 0.
	vwidth = 0.
	do i=1,128*128
	  v(i)=0.0
	  vmom1(i) = 0.
	  vmom2(i) = 0.
	  vmom3(i) = 0.
	  vmom4(i) = 0.
	enddo
c
c  Get weights for each velocity moment map.
c
	do k = 1,nc
	  delv = vlsr(2) - vlsr(1)
	  call veloint(vmin,vmax,vlsr(k),delv,wt)
	  if (wt.ne.0) then
	    nmaps = nmaps + 1
	    anorm = anorm + wt
	    vsum = vsum + wt*vlsr(k)
	    if(nmom.ge.1) wtmom1 = wt*vlsr(k)
	    if(nmom.ge.2) wtmom2 = wtmom1*vlsr(k)
	    if(nmom.ge.3) wtmom3 = wtmom2*vlsr(k)
	    if(nmom.eq.4) wtmom4 = wtmom3*vlsr(k)
c
c  Compute velocity moment maps.
c
	    do j = 1, ny
	      noff = nx*(j-1)
	      do i=1,nx
	        if(ary(i,j,k).ge.cutoff) then
		  m = i+noff
		  v(m) = wt*ary(i,j,k) + v(m)
		  if(nmom.ge.1) vmom1(m)=wtmom1*ary(i,j,k)+vmom1(m)
		  if(nmom.ge.2) vmom2(m)=wtmom2*ary(i,j,k)+vmom2(m)
		  if(nmom.ge.3) vmom3(m)=wtmom3*ary(i,j,k)+vmom3(m)
		  if(nmom.eq.4) vmom4(m)=wtmom4*ary(i,j,k)+vmom4(m)
		endif
	      enddo
	    enddo
	  endif
	enddo
c
c  Normalize for straight average (0-th moment).
c
	if(nmom.eq.0) then
	  if(anorm .ne. 0.) then
	    vmean = vsum/anorm
	    vwidth = anorm*(vlsr(2)-vlsr(1))
	    do i = 1, nx*ny
	      v(i) = v(i) / anorm
	    enddo
	  endif
	  write(msg, '(f7.2,a,2f10.3,a)') anorm,
     *      ' averaged into velocity window',vmin,vmax,' km/s'
	  call output(msg)
	  return	
	endif
c
c  Normalize by 0-th moment map, and for higher moments, form
c  moments with respect to the mean, using the relations given in
c  Abramowitz and Stegun, Handbook of Mathematical Functions, p. 928.
c
	do i=1,nx*ny
	  if(v(i).ge.cutoff) then
	    vcent = vmom1(i)/v(i)		! 1st moment (centroid)
	    if(nmom.eq.1) v(i) = vcent
	    if(nmom.ge.2) then
	      vcent2 = vcent*vcent		! needed for higher moments
	      v2origin = vmom2(i)/v(i)		! 2nd mom. w.r.t. origin
	      sigma2 = abs(v2origin - vcent2)	! 2nd mom w.r.t. mean (variance)
	      sigma = sqrt(sigma2)		! std. dev., or "line width"
	    endif
	    if(nmom.eq.2) v(i) = sigma
c  coeff. of skewness (cf. abramowitz and stegun)
	    if(nmom.ge.3) then			! 3rd mom. w.r.t. origin
		v3origin = vmom3(i)/v(i)
c						! 3rd mom. w.r.t. mean
		v3 = v3origin - 3.*sigma2*vcent - vcent*vcent2
	    endif
	    if(nmom.eq.3) v(i) = v3/sigma2/sigma
c  coeff. of excess, or kurtosis
	    if(nmom.eq.4)
     1        v(i)=((vmom4(i)/v(i))-4.*v3*vcent-6.*sigma2*vcent2
     2             -vcent2*vcent2)/sigma2/sigma2 - 3.
	  else
	    v(i) = -99999.		! magic value for blanked pixel
	  endif
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine veloint(vmin,vmax,vel,velint,wt)
	implicit none
	real vmin,vmax,vel,velint,wt
c
c  Find weight wt of point with velocity vel in interval (vmin,vmax)
c  Inputs:
c    vmin,vmax,velint	Velocity range and sample interval
c    vel		Requested velocity.
c  Outputs:
c    wt			Weight for velocity vel.
c----------------------------------------------------------------------c
	real vchn,vbot,vtop,vlo,vhi
c
	vchn = abs(velint)
c -- velocity range included in interpolation
	vbot = vmin - vchn
	vtop = vmax + vchn
c -- allow 5% capture range without interpolation
	vlo = vmin - 0.05*vchn
	vhi = vmax + 0.05*vchn

	if (vel.ge.vlo .and. vel.le.vhi) then
	    wt = 1.
	else if (vel.gt.vhi .and. vel.lt.vtop) then
	    wt = (vtop-vel)/vchn
	else if (vel.gt.vbot .and. vel.lt.vlo) then
	    wt = (vel-vbot)/vchn
	else
	  wt = 0.
	end if
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine convsize(cmaj,cmin,cpa,xy,ncon)
	implicit none
	real cmaj,cmin,cpa,xy
	integer ncon
c
c  Determine size of convolution array for VELO
c		mchw Oct 1987
c  Inputs:
c    xy			Pixel size
c  Outputs:
c    cmaj,cmin,cpa	Convolving beam and position angle.
c    ncon		Size of convolution array.
c----------------------------------------------------------------------c
	character*80 line
	integer length
#ifdef cft
c
c  External
c
	character substr*80
#endif
c
c  Initialize convolving beam.
c
	cmaj = 0.
	cmin = 0.
	cpa  = 0.

10	call prompt(line,length,
     *	  '>Enter convolving beam (major("), minor("), pa(deg): ')
	if(length.eq.0) goto 20
#ifdef cft
	read(substr(line,1),102,err=10) cmaj
	read(substr(line,2),102) cmin
	read(substr(line,3),102) cpa
102	format(f20.0)
#else
	read(line(1:length),103,err=10,end=20) cmaj,cmin,cpa
103	format(3f20.0)
#endif
c
20	ncon = 2*cmaj/xy + 1
	ncon = min(99,(max(ncon,3)))
	call output('Gaussian falls to 6% at edge of array')
	write(line, 105) ncon, ncon*xy
	call output(line)
105	format(' size of convolution array:',i5,' pixels,',
     *			f8.3,' arcsecs')
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine convinit(cmaj,cmin,cpa,xy,ncon,con)
	implicit none
	integer ncon
	real cmaj,cmin,cpa,xy
	real con(ncon,ncon,4,4)
c
c  Set up 2-dimensional convolution function for CONV
c	variable size array Oct 1987 MCHW
c----------------------------------------------------------------------c
	real pi,cma,cmi,sinpa,cospa,off
	real xp,yp,sum,gaus,x,y
	integer i,j,noff,ioff,joff,mid
	data pi /3.141592654/

c  Set minimum convolving beam for interpolating data ---
c --- save original inputs; convert to pixels  ---
	cma = max(cmaj/XY/1.665, 0.6)	! sigma = fwhm/1.665
	cmi = max(cmin/XY/1.665, 0.6)	! exp(- (0.5* xy/0.6*xy)**2) = 0.5

	sinpa = -sin(cpa*pi/180.)	! position angle north tho' east
	cospa = cos(cpa*pi/180.)

	mid = NCON/2 + 1
	off = 4.		! center to nearest 1/off of a pixel
	noff = off
c
c  Build convolution function
c
	do 20 ioff = 1, noff
	  xp = real(ioff - 1)/off
	do 20 joff = 1, noff
	  yp = real(joff - 1)/off
	  sum = 0.
	  do 10 i = 1, ncon
	    x = real(i-mid) - xp 
	    do 10 j = 1, ncon
	      y = real(j-mid) - yp
	      gaus = ((-x*sinpa+y*cospa)/cma)**2
     *				 + ((x*cospa+y*sinpa)/cmi)**2
	      if ( gaus .lt. 4.6 ) then
		con(i,j,ioff,joff) = exp(-gaus)
	      else
		con(i,j,ioff,joff) = 0.
	      end if
10	      sum = sum + con(i,j,ioff,joff)
	      do 15 i = 1, ncon
	      do 15 j = 1, ncon
15	      if(sum.ne.0.)
     *		  con(i,j,ioff,joff) = con(i,j,ioff,joff)/sum
20	continue
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine conv(ary,nx,ny,nc,x0,y0,ncon,con,conary,wt)
	implicit none
	integer nx,ny,nc,ncon
	real ary(nx,ny,nc), con(ncon,ncon,4,4), conary(nc)
	real x0,y0
c
c	performs 2-dimensional convolution of ARY by Gaussian 
c	centered at (X0,Y0); returns convolution in CONARY.
c	changed to variable size array Oct 1987 MCHW
c	Idiot proof loop for cray. 25oct91 mchw.
c----------------------------------------------------------------------c
	integer ix,iy,noff,ioff,joff,mid,ic,jc
	integer i,j,k
	real wt,gaus
c
c  find nearest sample point
c
	ix = nint(x0)
	iy = nint(y0)
c
c  find offset to nearest 1/noff and move to lower left sample point ---
c  ioff = 1 is zero offset
c
	noff = 4
	ioff = nint(noff*(x0-ix))
	if (ioff .lt. 0) then
	  ix = ix-1
	  ioff = ioff + noff
	end if
	ioff = ioff + 1

	joff = nint(noff*(y0-iy))
	if (joff .lt. 0) then
	  iy = iy-1
	  joff = joff + noff
	end if
	joff = joff + 1
c
c  Initialize convolved output array.
c
	wt = 0.
	do 10 k= 1,nc
10	  conary(k) = 0.
c
c  Convolve array by convolution function.
c
	mid = ncon/2 + 1
	do 40 ic = 1, ncon
	  i = ix + ic - mid
	  if(i .lt. 1 .or. i .gt. nx) go to 40
	    do 30 jc = 1, ncon
	      j = iy + jc - mid
	      if(j .lt. 1 .or. j .gt. ny) go to 30
		gaus = con(ic, jc, ioff, joff)
		wt = wt + gaus
		do 20 k = 1,nc
		  conary(k) = conary(k) + gaus * ary(i,j,k)
20		continue
30		continue
40		continue
c
c  renormalize
c
	if ( wt .ne. 0. ) then
	  do k = 1,nc
	    conary(k) = conary(k) / wt
	  end do
	end if
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine veloline(ary,nx,ny,nc,xc,yc,pa,ncon,con,
     *						 v,np,xstart,xend)
	implicit none
	integer nx,ny,nc,ncon,np
	real ary(nx,ny,nc)
	real xc,yc,pa,xstart,xend
	real v(nx*ny)
	real con(ncon,ncon,4,4)
c
c  Routine that does all the work for velocity-position maps
c  The algorithm in this routine only works for 0<pa<180
c
c  Inputs:
c    ary(nx,ny,nc)	The array to be convolved.
c    xc,xy,pa		The center and pa of the positon velocity plot
c    ncon		Size of	convolution array
c    con		The convolution array
c
c  Outputs:
c    v			The convolved position-velocity array
c    np			number of positions in position-velocity array
c    xstart,xend	Limits for plot
c-----------------------------------------------------------------------
	include 'velplot.h'
	character msg*80
	integer ix,iy,n,k,m
	real xinc,yinc,x0,y0,step,x1,y1,c,xl,yl,xr,yr
	real wt,fi,fj
	real ms,conary(256),pi
	pi = 3.141592654
c
	ix = nint(xc/xy) + mid
	iy = nint(yc/xy) + midy
	if(ix.lt.1 .or. ix.gt.nx .or. iy.lt.1 .or. iy.gt.ny) then
	  call output('pos-vel requested center is outside array')
	  return
	end if
c
c  Set variables for labelling box (for single map)
c
	posx = xc
	posy = yc
	pospa = pa
c
c  Find the points where this line goes outside the box 
c
	if (pa.eq.90.0) then
	  x1 = (is-mid)*xy
	  y1 = yc
	  np = nx
	  step = xy
	  x0 = (nx - mid) * xy
	  y0 = yc
	else if (pa.eq.0.0) then
	  x1 = xc
	  y1 = (ny-midy)*xy
	  np = ny
	  step = xy
	  x0 = xc
	  y0 = (1-midy) * xy
	else
c  General case y = ms*x + c ;  yl is the intercept on the left edge,
c    and xl is the intercept on the bottom.
	  ms = -1./tan(pa*pi/180.)
	  c = yc - (ms*xc)
	  yl = (is-mid)*xy*ms + c
	  xl = ( (1-midy)*xy - c)/ms
c  Test to see when line leaves box and set x1,y1 to this point
	  x1 = (is-mid)*xy
	  y1 = (1-midy)*xy
 	  if(yl.gt.(1-midy)*xy .and. yl.le.(ny-midy)*xy) then 
c  line intercepts left side of box
	    y1 = yl
	  else if(yl.lt.(1-midy)*xy) then
c  line intercepts bottom of box
	    x1 = xl
	  else if (yl.gt.(ny-midy)*xy) then
c  line intercepts top of box
	    x1 = ( (ny-midy)*xy - c )/ms
	    y1 = (ny-midy)*xy
	  end if
c
c  x1,y1 is the end of the line in arcseconds
c  Now find the start of the line so that the length and step size 
c  can be found. xr and yr are the intercepts on the right and bottom. 
c
	  xr = ( (ny-midy)*xy - c)/ms
	  yr = (nx-mid)*xy*ms + c
c  Test to see when line enters box and set x0,y0 to this point
	  x0 = (nx-mid)*xy
	  y0 = (ny-midy)*xy
 	  if (yr.gt.(1-midy)*xy .and. yr.le.(ny-midy)*xy) then 
c  line intercepts right side of box
	    y0 = yr
	  else if (yr.lt.(1-midy)*xy) then
c  line intercepts bottom of box
	    x0 = ( (1-midy)*xy - c )/ms
	    y0 = (1-midy)*xy
	  else if (yr.gt.(ny-midy)*xy) then
c  line intercepts top of box
	    x0 = xr
	  end if
c
c  Set step size.
c
	  np = max(nx,ny)
	  step = sqrt( (x0-x1)**2. + (y0-y1)**2.)/(np-1)
	end if
	write(msg, 101) x0,y0
	call output(msg)
	write(msg, 102) x1,y1
	call output(msg)
101	format (' Line enters box at (x,y) ',f8.2,' ',f8.2,' arcsecs')
102	format (' Line leaves box at (x,y) ',f8.2,' ',f8.2,' arcsecs')
c
c  Set up limits for plot label.
c
	xstart = -1.*sqrt((x0-xc)**2. + (y0-yc)**2.)
	xend = sqrt((x1-xc)**2. + (y1-yc)**2.)
c
c  Step along line
c
	xinc = -sin(pa*pi/180.)
	yinc =  cos(pa*pi/180.)
	x0 = x0 - xinc * step
	y0 = y0 - yinc * step
	do n = 1 ,np
	  x0 = x0 + xinc * step
	  y0 = y0 + yinc * step
c
c  Convert arcsec to pixel numbers
c
	  fi = (x0/xy) + mid
	  fj = (y0/xy) + midy
c
c  check that point is within the array
c
	  if((fi-1).ge.-1.e-4 .and. (fi-nx).le.1.e-4 .and.
     1       (fj-1).ge.-1.e-4 .and. (fj-ny).le.1.e-4) then	
c
c  convolve array; spectrum is in conary
c
	    call conv(ary,nx,ny,nc,fi,fj,ncon,con,conary,wt)
	    do k=1,nc
	      m = k + (n-1)*nc
	      v(m) = conary(k)
	    end do
	  else
	    do k=1,nc
	     m = k + (n-1)*nc
	     v(m) = 0.0
	    end do
	  end if
	end do
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine wrary
	implicit none
c  Write file of spectra & position-velocity cuts.
c----------------------------------------------------------------------c
	integer lu,iostat,len1,n,length
	character*80 file,line
c
        integer ncut
	real xcut(128),ycut(128),pa(128)
        common /cuts/ xcut,ycut,pa,ncut
c
	integer nspec,ngauss(49)
	real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
	common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c
	if(nspec.eq.0.and.ncut.eq.0)then
	  call bug('w',
     *		'No spectra or position-velocity cuts in lists.')
	  return
	endif
c
c  Open output file.
c
	call prompt(file,length,
     *	    'Output file for spectra & position-velocity cuts : ')
	call txtopen(lu,file,'new',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening ascii file')
	  return
	endif
c
	call txtwrite(lu,file,len1(file),iostat)
	write(line,'(a)')
     *		'Output file for spectra & position-velocity cuts.'
	call txtwrite(lu,line,len1(line),iostat)
c
c  Write spectra positions.
c
	write(line,'(i4,a)')
     *    nspec, '  spectra positions (HA,DEC) arcsec w.r.t. crpix'
	call txtwrite(lu,line,len1(line),iostat)
 	if(nspec.ne.0)then
	  do n=1,nspec
	    write(line,'(2f20.4)') xc(n),yc(n)
	    call txtwrite(lu,line,40,iostat)
	  enddo
	endif
c
c  Write position-velocity cuts.
c
	write(line,'(i4,a)')
     *    ncut, '  position-velocity cuts HA,DEC (") & pa (degrees)'
	call txtwrite(lu,line,len1(line),iostat)
 	if(ncut.ne.0)then
 	  do n=1,ncut
	    write(line,'(3f20.4)') xcut(n),ycut(n),pa(n)
	    call txtwrite(lu,line,60,iostat)
	  enddo
	endif
c
c  Close output file and tell user.
c
	call txtclose(lu)
	write(line,'(i3,a,i3,a,a)') nspec,' spectra and ',ncut,
     *    ' position-velocity cuts written to ',file(1:len1(file))
	call output(line(1:len1(line)))
c
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine rdary
	implicit none
c  Read file of spectra & position-velocity cuts.
c----------------------------------------------------------------------c
	integer lu,iostat,len1,n,length
	character*80 file,line
c
        integer ncut
	real xcut(128),ycut(128),pa(128)
        common /cuts/ xcut,ycut,pa,ncut
c
	integer nspec,ngauss(49)
	real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
	common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c
c  Open input file.
c
	call prompt(file,length,
     *	    'Input file for spectra & position-velocity cuts : ')
	call txtopen(lu,file,'old',iostat)
	if(iostat.ne.0)then
	  call bug('w','Error opening ascii file')
	  return
	endif
c
	do n=1,2
	  call txtread(lu,line,length,iostat)
	  if(iostat.ne.0)then
	    call bug('w','Error reading ascii file')
	    return
	  endif
	  call output(line)
	enddo
c
c  Read spectra positions.
c
	call txtread(lu,line,length,iostat)
	call output(line)
	read(line,'(i4)') nspec
 	if(nspec.ne.0)then
	  do n=1,nspec
	    call txtread(lu,line,length,iostat)
	    call output(line)
	    read(line,'(2f20.4)') xc(n),yc(n)
c zero number of gaussians in fit to spectrum 
            ngauss(n)=0
	  enddo
	endif
c
c  Read position-velocity cuts.
c
	call txtread(lu,line,length,iostat)
	call output(line)
	read(line,'(i4)') ncut
 	if(ncut.ne.0)then
 	  do n=1,ncut
	    call txtread(lu,line,length,iostat)
	    call output(line)
	    read(line,'(3f20.4)') xcut(n),ycut(n),pa(n)
	  enddo
	endif
c
c  Close input file and tell user.
c
	call txtclose(lu)
	write(line,'(i3,a,i3,a,a)') nspec,' spectra and ',ncut,
     *    ' position-velocity cuts from file: ',file(1:len1(file))
	call output(line(1:len1(line)))
c
	end
c--------------------------------------------------------------------------
      subroutine gaufit(nc,vlsr,t,spec,rms)
      implicit none
	real rms
c 
c Subroutine fits multiple gaussians to spectrum using numerical
c recipes routines FGAUSS and MRQMIN. Up to 30 (3x10) free parameters.
c See Numerical Recipes pp. 521-528 for details.
c
c User specified parameters:
c  i. spectrum rms (used to aid convergence and for error estimates)
c  ii. number of gaussians to fit (currently restricted to 10)
c  iii. initial guesses for gaussian heights, velocities, fwhms
c
c N.B. Performance is closely related to initial guesses!
c
c Inputs:
c  nc      number of channels
c  vlsr    velocity array
c  t       amplitudes
c  spec    spectrum index
c  rms     current rms estimate
c Ouputs
c  ngauss  number of simultaneously fitted  gaussians 
c  gauss   gaussian parameters
c           gauss(1,i) amplitude of gaussian i
c           gauss(2,i) center velocity of gaussian i
c           gauss(3,i) sigma of gaussian i
c  gausserr error estimates for gaussian parameters
c---------------------------------------------------------------------
      integer nc,spec
      real vlsr,t
c
      integer nspec,ngauss(49)
      real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c
c Gaussian fitting routine adapted from 
c Numerical Recipes driver for MRQMIN
c
      external fgauss
      integer i,j,k,itst

      integer nma,mfit,ma,lista
      parameter(nma=30)
      real sig,a,covar,alpha
      dimension sig(256),a(nma),lista(nma),
     *          covar(nma,nma),alpha(nma,nma)
      real alamda,chisq,ochisq
      integer length
      real value
      real rmsest
      character*80 line,ans
      real p1,p2,p3,amp,mom1,mom2
c
c ask user to continue or not
      call output(' ')
      call prompt(ans,length,
     *    '>Continue with gaussian fit ([Y]/N) :')
      call ucase(ans)
      if(ans(1:1).eq.'N')return
c get number of simultaneous gaussians to fit
      ngauss(spec)=1
      call promptf(value,'f2.0',
     *  '>Enter number of gaussians to fit',real(ngauss(spec)))
      ngauss(spec)=value
c ma is the number of free parameters to be fit
      ma=ngauss(spec)*3
c mfit must be fit if some parameters held fixed; for the future 
      mfit=ngauss(spec)*3
c get initial guesses for amp,center,width,rmsest
c   
	call moment1(nc,vlsr,t,rms,amp,mom1,mom2)
      do i=1,ngauss(spec)
        write(line,107) i,amp,mom1,mom2*2.*sqrt(log(2.))
c********1*********2*********3*********4*********5*********6*********7*c
107   format('>Gaussian no. ',i1,', Enter amp, vel, fwhm [',3f8.3,'] :')
        call prompt(ans,length,line)
	if(length.ne.0)then
          read(ans(1:length),'(3f20.0)') p1,p2,p3
	  gauss(spec,1,i)=p1
	  gauss(spec,2,i)=p2
	  gauss(spec,3,i)=p3/(2.*sqrt(log(2.)))
	else
	  gauss(spec,1,i)=amp
	  gauss(spec,2,i)=mom1
	  gauss(spec,3,i)=mom2
	endif  
      enddo

      call promptf(rmsest,'f10.4',
     *  '>Enter rms estimate: ',rms)
	rms = rmsest

      do 13 i=1,mfit
        lista(i)=i
13    continue

      j=1
      do 14 i=1,ngauss(spec)
	a(j) = gauss(spec,1,i)
        a(j+1)=gauss(spec,2,i)
        a(j+2)=gauss(spec,3,i)
        j = j + 3
14    continue
c
      do i=1,nc
        sig(i)=rmsest
      enddo
c 
c first call to mrqmin uses initial guess and sets alamda to 0.001
c
      alamda=-1.
c
      call mrqmin(vlsr,t,sig,nc,a,ma,lista,mfit,covar,alpha,
     *			nma,chisq,fgauss,alamda)		
c
c repeat calls to mrqmin until delta chisq is less than 0.01
c (note that chisq is not divided by ndata in mrqcof)
c 
      k=1
      itst=0
1	WRITE(*,'(/1X,A,I2,T18,A,F10.4,T43,A,E9.2)') 'Iteration #',K,
     *		'Chi-squared:',chisq/nc, 'alamda:',alamda
      k=k+1
      ochisq=chisq
      call mrqmin(vlsr,t,sig,nc,a,ma,lista,mfit,covar,alpha,
     *			nma,chisq,fgauss,alamda)		
c
      if(chisq.gt.ochisq)then
        itst=0
      else if (abs(ochisq-chisq).lt.0.01)then
        itst=itst+1
      endif
c
      if(itst.lt.2)then
       goto 1
      endif
c
c make one last call to mrqmin to get the covariance matrix
c
      alamda=0.0
      call mrqmin(vlsr,t,sig,nc,a,ma,lista,mfit,covar,alpha,
     *			nma,chisq,fgauss,alamda)		
c
      j = 1
      do 24 i=1,ngauss(spec)
	gauss(spec,1,i) = a(j)
        gauss(spec,2,i)=a(j+1)
        gauss(spec,3,i)=a(j+2)
        j = j + 3
24    continue
c
c print results and write them into the log
c
	call LogWrit(' ')
	write(line,'(a,f7.2,a,f7.2,a,a,f10.4,a,f10.4)')
     *			 '(',xc(spec),',',yc(spec),')',' rms_est=',
     *	  			rmsest,'  chisq=',chisq/nc
        call LogWrit(line)
      j=1
      do i=1,ngauss(spec)
        write(line,'(a,a,a,a)')
     *		' Gaussian ',' amplitude',' velocity ','   fwhm   '
        call output(line)
        call LogWrit(line)
        write(line,'(i10,3f10.4)')i,gauss(spec,1,i),gauss(spec,2,i),
     *          gauss(spec,3,i)*1.665
        call output(line)
        call LogWrit(line)
c
c error estimates
        gausserr(spec,1,i)=sqrt(covar(j,j))
        gausserr(spec,2,i)=sqrt(covar(j+1,j+1))
        gausserr(spec,3,i)=sqrt(covar(j+2,j+2))
        j=j+3
        write(line,157) 
157   format('  Estimated errors:')
        call output(line)
        call LogWrit(line)
        write(line,'(i10,3f10.4)')
     *		i,gausserr(spec,1,i),gausserr(spec,2,i),
     *          			gausserr(spec,3,i)*1.665
        call output(line)
        call LogWrit(line)
      enddo
c
      return
      end
c--------------------------------------------------------------------------
      SUBROUTINE FGAUSS(X,A,Y,DYDA,NA)
      integer na
      real a,x,y,dyda
      real ex,fac,arg
      DIMENSION A(NA),DYDA(NA)
      integer i
      Y=0.
      DO 11 I=1,NA-1,3
        ARG=(X-A(I+1))/A(I+2)
        EX=EXP(-ARG**2)
        FAC=A(I)*EX*2.*ARG
        Y=Y+A(I)*EX
        DYDA(I)=EX
        DYDA(I+1)=FAC/A(I+2)
        DYDA(I+2)=FAC*ARG/A(I+2)
11    CONTINUE
      RETURN
      END
c--------------------------------------------------------------------------
      SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)
      integer n,np,m,mp,nmax,ipiv,indxr,indxc,ll
      PARAMETER (NMAX=50)
      real a,b
      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      integer i,j,k,l,irow,icol
      real big,dum,pivinv
      DO 11 J=1,N
        IPIV(J)=0
11    CONTINUE
      DO 22 I=1,N
        BIG=0.
        DO 13 J=1,N
          IF(IPIV(J).NE.1)THEN
            DO 12 K=1,N
              IF (IPIV(K).EQ.0) THEN
                IF (ABS(A(J,K)).GE.BIG)THEN
                  BIG=ABS(A(J,K))
                  IROW=J
                  ICOL=K
                ENDIF
              ELSE IF (IPIV(K).GT.1) THEN
                call bug('f', 'Singular matrix')
              ENDIF
12          CONTINUE
          ENDIF
13      CONTINUE
        IPIV(ICOL)=IPIV(ICOL)+1
        IF (IROW.NE.ICOL) THEN
          DO 14 L=1,N
            DUM=A(IROW,L)
            A(IROW,L)=A(ICOL,L)
            A(ICOL,L)=DUM
14        CONTINUE
          DO 15 L=1,M
            DUM=B(IROW,L)
            B(IROW,L)=B(ICOL,L)
            B(ICOL,L)=DUM
15        CONTINUE
        ENDIF
        INDXR(I)=IROW
        INDXC(I)=ICOL
        IF (A(ICOL,ICOL).EQ.0.) call bug('f', 'Singular matrix.')
        PIVINV=1./A(ICOL,ICOL)
        A(ICOL,ICOL)=1.
        DO 16 L=1,N
          A(ICOL,L)=A(ICOL,L)*PIVINV
16      CONTINUE
        DO 17 L=1,M
          B(ICOL,L)=B(ICOL,L)*PIVINV
17      CONTINUE
        DO 21 LL=1,N
          IF(LL.NE.ICOL)THEN
            DUM=A(LL,ICOL)
            A(LL,ICOL)=0.
            DO 18 L=1,N
              A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
18          CONTINUE
            DO 19 L=1,M
              B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
19          CONTINUE
          ENDIF
21      CONTINUE
22    CONTINUE
      DO 24 L=N,1,-1
        IF(INDXR(L).NE.INDXC(L))THEN
          DO 23 K=1,N
            DUM=A(K,INDXR(L))
            A(K,INDXR(L))=A(K,INDXC(L))
            A(K,INDXC(L))=DUM
23        CONTINUE
        ENDIF
24    CONTINUE
      RETURN
      END
c--------------------------------------------------------------------------
      SUBROUTINE MRQMIN(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,
     *    COVAR,ALPHA,NCA,CHISQ,FUNCS,ALAMDA)
c
c Levenberg-Marquant method, attempting to reduce the value of chisq
c of a fit between a set of NDATA points X(I),Y(I) with individual
c standard deviations, SIG(I), and a nonlinear function dependent on MA
c coeff A. 
c The array LISTA numbers the parameters A such that the first MFIT
c elements correspond to values actually being adjusted; the remaining
c MA-MFIT parameters are held fixed at their input values. The routine
c returns current best fit values for the MA fit paramters A, and CHISQ.
c COVAR(NCA,NCA) and ALPHA(NCA,NCA) with physical dinemsion NCA >= MFIT
c are used as working space during most iterations. Supply a subroutine
c FUNCS(X,A,YFIT,DYDA,MA) that evaluates the fitting function YFIT and
c its derivatives DYDA with respect to the fitting parameters A at X.
c On the first call provide an inital guess for the parameters A, and
c set ALAMDA<0 for initialization (which then sets ALAMDA=0.001). If a step
c succeeds CHISQ becomes smaller and ALAMDA decreases by a factor of 10.
c If a step fails ALAMDA grows by a factor of 10. You must call this
c routinerepeatedly until convergence is achieved.
c Then make one final call with ALAMDA=0 so that COVAR(I,J) returns
c the covariance matrix and ALPHA(I,J) he curvature matrix.
c
c     MMAX changed from 20 to 30, djw.
c
      integer mmax
      PARAMETER (MMAX=30)
      integer ndata,lista,ma,mfit,nca
      real x,y,sig,a,covar,alpha,atry,beta,da
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),A(MA),LISTA(MA),
     *  COVAR(NCA,NCA),ALPHA(NCA,NCA),ATRY(MMAX),BETA(MMAX),DA(MMAX)
      real chisq,ochisq,alamda
      external funcs
      integer j,k,kk,ihit
      IF(ALAMDA.LT.0.)THEN
        KK=MFIT+1
        DO 12 J=1,MA
          IHIT=0
          DO 11 K=1,MFIT
            IF(LISTA(K).EQ.J)IHIT=IHIT+1
11        CONTINUE
          IF (IHIT.EQ.0) THEN
            LISTA(KK)=J
            KK=KK+1
          ELSE IF (IHIT.GT.1) THEN
            call bug('f', 'Improper permutation in LISTA')
          ENDIF
12      CONTINUE
        IF (KK.NE.(MA+1)) call bug('f', 'Improper permutation in LISTA')
        ALAMDA=0.001
        CALL MRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NCA,CHISQ,
     *    FUNCS)
        OCHISQ=CHISQ
        DO 13 J=1,MA
          ATRY(J)=A(J)
13      CONTINUE
      ENDIF
      DO 15 J=1,MFIT
        DO 14 K=1,MFIT
          COVAR(J,K)=ALPHA(J,K)
14      CONTINUE
        COVAR(J,J)=ALPHA(J,J)*(1.+ALAMDA)
        DA(J)=BETA(J)
15    CONTINUE
      CALL GAUSSJ(COVAR,MFIT,NCA,DA,1,1)
      IF(ALAMDA.EQ.0.)THEN
        CALL COVSRT(COVAR,NCA,MA,LISTA,MFIT)
        RETURN
      ENDIF
      DO 16 J=1,MFIT
        ATRY(LISTA(J))=A(LISTA(J))+DA(J)
16    CONTINUE
      CALL MRQCOF(X,Y,SIG,NDATA,ATRY,MA,LISTA,MFIT,COVAR,DA,NCA,CHISQ,
     *  FUNCS)
      IF(CHISQ.LT.OCHISQ)THEN
        ALAMDA=0.1*ALAMDA
        OCHISQ=CHISQ
        DO 18 J=1,MFIT
          DO 17 K=1,MFIT
            ALPHA(J,K)=COVAR(J,K)
17        CONTINUE
          BETA(J)=DA(J)
          A(LISTA(J))=ATRY(LISTA(J))
18      CONTINUE
      ELSE
        ALAMDA=10.*ALAMDA
        CHISQ=OCHISQ
      ENDIF
      RETURN
      END
c--------------------------------------------------------------------------
      SUBROUTINE MRQCOF(X,Y,SIG,NDATA,A,MA,LISTA,MFIT,ALPHA,BETA,NALP,
     *CHISQ,FUNCS)
      integer mmax
      PARAMETER (MMAX=30)
      integer ndata,ma,lista,mfit,nalp
      real x,y,sig,alpha,beta,dyda,a,chisq
      external funcs
      integer i,j,k
      real sig2i,dy,ymod,wt
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA),ALPHA(NALP,NALP),BETA(MA),
     *    DYDA(MMAX),LISTA(MFIT),A(MA)
      DO 12 J=1,MFIT
        DO 11 K=1,J
          ALPHA(J,K)=0.
11      CONTINUE
        BETA(J)=0.
12    CONTINUE
      CHISQ=0.
      DO 15 I=1,NDATA
        CALL FUNCS(X(I),A,YMOD,DYDA,MA)
        SIG2I=1./(SIG(I)*SIG(I))
        DY=Y(I)-YMOD
        DO 14 J=1,MFIT
          WT=DYDA(LISTA(J))*SIG2I
          DO 13 K=1,J
            ALPHA(J,K)=ALPHA(J,K)+WT*DYDA(LISTA(K))
13        CONTINUE
          BETA(J)=BETA(J)+DY*WT
14      CONTINUE
        CHISQ=CHISQ+DY*DY*SIG2I
15    CONTINUE
      DO 17 J=2,MFIT
        DO 16 K=1,J-1
          ALPHA(K,J)=ALPHA(J,K)
16      CONTINUE
17    CONTINUE
      RETURN
      END
c--------------------------------------------------------------------------
      SUBROUTINE COVSRT(COVAR,NCVM,MA,LISTA,MFIT)
      integer ncvm,ma,lista,mfit,i,j
      real covar,swap
      DIMENSION COVAR(NCVM,NCVM),LISTA(MFIT)
      DO 12 J=1,MA-1
        DO 11 I=J+1,MA
          COVAR(I,J)=0.
11      CONTINUE
12    CONTINUE
      DO 14 I=1,MFIT-1
        DO 13 J=I+1,MFIT
          IF(LISTA(J).GT.LISTA(I)) THEN
            COVAR(LISTA(J),LISTA(I))=COVAR(I,J)
          ELSE
            COVAR(LISTA(I),LISTA(J))=COVAR(I,J)
          ENDIF
13      CONTINUE
14    CONTINUE
      SWAP=COVAR(1,1)
      DO 15 J=1,MA
        COVAR(1,J)=COVAR(J,J)
        COVAR(J,J)=0.
15    CONTINUE
      COVAR(LISTA(1),LISTA(1))=SWAP
      DO 16 J=2,MFIT
        COVAR(LISTA(J),LISTA(J))=COVAR(1,J)
16    CONTINUE
      DO 18 J=2,MA
        DO 17 I=1,J-1
          COVAR(I,J)=COVAR(J,I)
17      CONTINUE
18    CONTINUE
      RETURN
      END

c---------------------------------------------------------------------
      subroutine gauplot(nc,vlsr,t,spec)
      implicit none
c 
c Inputs:
c  nc      number of channels
c  vlsr    velocity array
c  t       spectrum
c  spec    spectrum index
c  ngauss  number of simultaneously fitted  gaussians 
c  gauss   gaussian parameters
c           gauss(1,i) amplitude of gaussian i
c           gauss(2,i) center velocity of gaussian i
c           gauss(3,i) sigma of gaussian i
c---------------------------------------------------------------------
      integer nc,spec
      real vlsr(nc),t(nc)
      real tmod(256),tres(256)
      real vlsr2(1280),tmod2(1280),dv
      integer i,j
      real arg1,term
c
      integer nspec,ngauss(49)
      real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c
c make sure there are gaussian fits to compute
c
      if(ngauss(spec).eq.0)return
c form gaussian model spectrum
      do i=1,nc
        tmod(i) = 0.      
        do j=1,ngauss(spec)
          arg1=(vlsr(i)-gauss(spec,2,j))/gauss(spec,3,j)
          if(abs(arg1).gt.4.)then
            term=0.0
          else
            term=gauss(spec,1,j)*exp(-arg1*arg1) 
          endif
          tmod(i) = tmod(i) + term
        enddo
      enddo
c
c form residual spectrum
      do i=1,nc
        tres(i)=t(i)-tmod(i)
      enddo
c
c form new gaussian model spectrum with factor of 5 oversampling
c
      dv=(vlsr(2)-vlsr(1))/5.
      do i=1,nc*5
        vlsr2(i)=vlsr(1)+(real(i)*dv)
        tmod2(i) = 0.      
        do j=1,ngauss(spec)
          arg1=(vlsr2(i)-gauss(spec,2,j))/gauss(spec,3,j)
          if(abs(arg1).gt.4.)then
            term=0.0
          else
            term=gauss(spec,1,j)*exp(-arg1*arg1) 
          endif
          tmod2(i) = tmod2(i) + term
        enddo
      enddo
c
c plot model fit and residual spectrum
      call pgsls(4)
      call pgline(nc*5,vlsr2,tmod2)
c      call pgsls(2)
c      call pgHline(nc,vlsr,tres,2.)
      call pgsls(1)
      return
      end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine moment1(nc,vlsr,buf,clip,amp,mom1,mom2)
	implicit none
	integer nc
	real vlsr(nc),buf(nc),amp,mom1,mom2,clip
c
c  Calculate moments.
c
c  Input:
c    nc		number of channels
c    vlsr	velocities
c    buf	amplitudes
c    clip	cutoff level in moment calculation
c  Output:
c    amp	maximum amplitude
c    mom1	1st moment
c    mom2	2nd moment
c------------------------------------------------------------------------
	integer i
	real mom0
c
	amp = 0.
	mom0 = 0.0
	mom1 = 0.0
	mom2 = 0.0
c
c  Find the maximum and accumulate the moments.
c
	do i = 1,nc
	  if(buf(i).gt.clip)then
	    if(buf(i).gt.amp)amp=buf(i)
	    mom0 = mom0 + buf(i)
	    mom1 = mom1 + buf(i)*vlsr(i)
	    mom2 = mom2 + buf(i)*vlsr(i)*vlsr(i)
	  endif
	enddo
c
c  Normalize and scale the moments.
c
	if(mom0.ne.0.) then
	  mom1 =mom1/mom0
	  mom2 = mom2/mom0 - mom1*mom1
	  if(mom2.gt.0) then
	    mom2 = sqrt(mom2)
	  else
	    mom2 = 0.
	  endif
	endif

	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine PosVel(ary,vlsr,nx,ny,nc)
	implicit none
	integer nx,ny,nc
	real ary(nx,ny,nc),vlsr(nc)
c
c  Make position-velocity images and plots.
c
c  Inputs:
c    ary	The image.
c    nx,ny,nc	Dimensions of image.
c    vlsr	Array of velocities.
c-----------------------------------------------------------------------
	include 'velplot.h'
	integer np,k,l,ncon
	real xin,yin,pain,cmaj,cmin,cpa,cf
	real xstart,xend,vstart,vend,xmin,xmax
	real v(16384)
c  convolution array maximum size
	real con(99,99,4,4)
        real tr(6),ymax,ymin,xval(128),xcoord(128),xdelta
	character label*80,ans*1,oldevice*80
        character*9 lab1,lab2,lab3
        character*1 lab4
        character*80 xlabel,ylabel,line
	integer i,windx,windy,length,lwidth
	integer ii,jj,jjj,nchan,ichan(10),ifix
c
        integer ncut
	real xcut(128),ycut(128),pa(128)
        common /cuts/ xcut,ycut,pa,ncut
#ifdef cft
c
c  External
c
	character substr*80
#endif
c
c  Introduction.
c
	call output(
     *	'Plot intensity versus position & velocity along selected cuts')
	call prompt(ans,length,'>Type H for help, <cr> to continue :')
	call ucase(ans)
	if(ans.eq.'H')then
	  call output(' ')
	  call output(
     *	   'Plot position-velocity maps, or intensity versus position.')
	  call output('Cuts are selected with the cursor on an x-y map')
	  call output('or can be entered as a list which can be edited')
	  call output('RA-velocity or DEC-velocity maps can be plotted')
	  call output('Position-velocity maps can also be printed, or')
	  call output('written out as images for further processing.')
	  call output('(x,y) positions are in (HA,DEC) directions.')
	  call output('(Position angle is measured from N through E)')
	  call output(' ')
	endif
c
c  List current selection of cuts.
c
	goto 9
20	call output(' ')
	call output('Cut number must edit or extend list by one')
9	if(ncut.ne.0) then
	  call output('--- current selection of cuts ---')
	  do i=1,ncut
	    write(line, 109) i,xcut(i),ycut(i),pa(i)
	    call output(line)
	  end do
	else
	  call output('--- no current selection of cuts ---')
	endif
109	format(' map(',i2,')  x= ',f9.3,' y= ',f9.3,' pa= ',f8.3)
c
c  Specify List of cuts, or RA-vel or DEC-velocity.
c
	call output(' ')
	call prompt(ans,length,
     *    '>List of cuts, RA-vel or DEC-vel ? ([L]/X/Y) :')
	call ucase(ans)
	if(ans.eq.' ') ans='L'
c
c  Enter list of position-velocity cuts across image.
c
	if(ans.eq.'L') then
	  call output(' ')
	  call output('--- Enter list of pos-vel cuts to plot ---')
	  call output(
     *      'angles measured from north to the east (0 to 180).')
	  call output('>Type -n to delete cut n')
          call output('      -99 to delete all')
	  call output('      L to list')
          call output('     <cr> to use the current list.')
10	  call prompt(line,length,
     *      '>Enter cut number, position and angle (n,x,y,pa): ')
	  if(length.eq.0) goto 50
#ifdef cft
	  read(substr(line,1),101,err=20) i
	  read(substr(line,2),103) xin
	  read(substr(line,3),103) yin
	  read(substr(line,4),103) pain
101	  format(i10.0)
103	  format(f20.0)
#else
	  read(line(1:length),102,err=20,end=51) i,xin,yin,pain
102	  format(i10,3f20.0)
#endif
c
c  End of list. Plot maps.
c
	  if(i.eq.0) goto 50
c  Add or change map
	  if (i.ge.1 .and. i.le.min(ncut+1,25) ) then
	    xcut(i) = xin
	    ycut(i) = yin
	    pa(i) = pain
	    ncut = max(i,ncut)
c  Delete map
	  else if (-i.ge.1 .and. -i.le.ncut ) then
	    k = -i
	    do i=k,ncut-1
	      xcut(i) = xcut(i+1)
	      ycut(i) = ycut(i+1)
	      pa(i) = pa(i+1)
	    enddo
	    ncut = ncut-1
c  Start new list
	  else if(i.le.-99) then
            call output('All cuts deleted from list')
	    ncut = 0
	  else
	    call output(' ')
	    call output('Cut number must edit or extend list by one')
	    goto 10
	  endif
	  goto 10
c
c  RA-velocity maps.
c
	else if(ans.eq.'X') then
	  ncut=ny
	  do i=1,ncut
	    xcut(i) = 0.0
	    ycut(i) = (i-mid) * xy
	    pa(i) = 90.0
	  enddo
c
c  DEC-velocity maps.
c
	else if (ans.eq.'Y') then
	  ncut=nx
	  do i=1,ncut
	    xcut(i)=(i-mid) * xy
	    ycut(i)=0.0
	    pa(i)=0.0
	  enddo
	else 
	  goto 9
	endif	    
50	if(ncut.eq.0) then
51	  call output('--- no current selection of cuts ---')
	  return
	endif
c
c  Set up convolution function.
c
	call convsize(cmaj,cmin,cpa,xy,ncon)
	call convinit(cmaj,cmin,cpa,xy,ncon,con)
c  
c  Check whether it is contour or intensity plots
c
	nchan=0
	call prompt(line,length,'Contour or Intensity plots ? [C]/I :')
	if(length.eq.0)goto 399
	call ucase(line)
	if(line.eq.'I') then
398	  continue
	  call prompt(line,length,'How many channels ? (5 max) :')
	  nchan=1
	  if(length.eq.0)goto 399
	  read (line(1:length),101) nchan
101	  format(i10.0)
	  if(nchan.gt.nc)then
	    call output('Cannot be more than there are in the image')
	    goto 398
          end if
	  if(nchan.gt.5)then
	    call output('Up to 5 channels allowed')
	    goto 398
          end if
	  call prompt(line,length,'Enter the channel numbers :')
	  read(line(1:length), '(5i10)') (ichan(i),i=1,nchan)
c	  do l=1,nchan
c	    read(substr(line,l),101) ichan(l)
c	  enddo
	  call prompt(line,length,'Fix the scales ?  Y/[N] :')
	  call ucase(line)
	  ifix=0
	  if(line.eq.'Y')then
	    ifix=1
	    call prompt(line,length,'Enter xmin,xmax,ymin,ymax :')
	    read (line(1:length),'(4f20.0)') xmin,xmax,ymin,ymax
          endif
	end if
399	continue
c
c  Set maptype to position-velocity, and save plot device type from menu.
c
	maptype = 'POS-VEL'
        xlabel='velocity (km/s)'
        ylabel='position (arcsec)'
	oldevice=device
	lwidth = 1
c	
c  Set up the plot windows.
c
60	if(write.ne.'Y' .and. apint.ne.'Y') then
	  call getpanel(ncut,windx,windy)
	  call pgbeg(0,device,windx,windy)
c	  call pgask(.FALSE.)
	  call pgslw(lwidth)
	endif
c
c  Calculate the conversion factor for map units.
c
	call header(0)
	cf = 1.
	if (units.eq.'K') cf = dperjy

c
c  Convolve array into position-velocity maps, output in V.
c
	do l = 1,ncut
          call pgpage
          if(windx*windy.eq.1 .and. alabel.eq.'Y')then
            call pgsvp(0.1,0.7,0.1,0.9)
          else
            call pgsvp(0.1,0.9,0.1,0.9)
          endif
	  call veloline (ary,nx,ny,nc,xcut(l),ycut(l),pa(l),ncon,con,
     1	    v,np,xstart,xend)
	  vstart = vlsr(1)
	  vend = vlsr(nc)
	  if(nchan.eq.0) then
	    call output('--- position velocity map ---')
	    else
	    call output('--- position intensity plot ---')
	  endif
	  write(line, 109) l,xcut(l),ycut(l),pa(l)
	  call output(line)
c
c  Set up parameters for plot labels.
c
          write(lab1,104) xcut(l)
          write(lab2,104) ycut(l)
          write(lab3,104) pa(l)
          lab4=char(l+96)
          if(pspec.eq. 'Y') then
            label=lab4//' (x,y)=('//lab1//','//lab2//')   PA='//lab3
          else
            label=' (x,y)=('//lab1//','//lab2//')   PA='//lab3
          endif
104	  format (f8.2)
c
c  Write out map.
c
	  if(write.eq.'Y') then
	    call wrposvel(v,nc,np,1,vstart,xstart,pa(l),1.,1.,
     *			(vend-vstart)/(nc-1),(xend-xstart)/(np-1))
c
c  Integer plot.
c
	  else if(apint.eq.'Y')then
	    call plotint(v,nc,np)
c
c  Plot contours.
c
	  else
	   if(nchan.eq.0) then
            call pgswin(vstart,vend,xstart,xend)
c  Set up transformation array for contour routine.
            tr(2)=(vend-vstart)/(nc-1)
            tr(1)=vstart-tr(2)
            tr(3)=0.
            tr(5)=0.
            tr(6)=(xend-xstart)/(np-1)
            tr(4)=xstart-tr(6)
c  Plot contours.
            call plotcon(v,nc,np,cf,tr)
            call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
            call pglab(xlabel,ylabel,label)
           else
	    if(cf.eq.0.)cf=1.
	    if(ifix.eq.0) then
	    ymax=-1.0e+6
	    ymin=1.0e+6
	    do ii=1,nchan
	       do jj=1,np
		 jjj=(jj-1)*nc+ichan(ii)
		 if((v(jjj)/cf).gt.ymax)ymax=v(jjj)/cf
		 if((v(jjj)/cf).lt.ymin)ymin=v(jjj)/cf
               enddo
            enddo
	    ymin = ymin - 0.05 * (ymax - ymin)
	    ymax = ymax + 0.05 * (ymax - ymin)
	    call pgswin(xstart,xend,ymin,ymax)
	    else
	      call pgswin(xmin,xmax,ymin,ymax)
            endif
	    call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
          if (units.eq.'K') then
            xlabel='Kelvin'
          else if (units.eq.'J') then
            xlabel='Janskys'
          else
            xlabel='Map units'
          endif
	    call pglab(ylabel,xlabel,label)
	    xdelta=(xstart-xend)/real(np-1)
	    if(xstart.lt.xend)xdelta=abs(xdelta)
	    if(xstart.gt.xend)xdelta=-1.0*abs(xdelta)
	    do ii=1,nchan
	      do jj=1,np
		jjj=(jj-1)*nc+ichan(ii)
		xval(jj)=v(jjj)/cf
		xcoord(jj)=xstart+real(jj-1)*xdelta
              enddo
	      call pgsls(ii)
	      call pgline(np,xcoord,xval)
            enddo
	    call pgsls(1)
	    maptype='SPECTRA'
	  endif
c  Now annotate plot
c  (had to wait for plotcon to get values)
            if(windx*windy.eq.1 .and. alabel.eq.'Y')then
              call pgsvp(0.72,0.9,0.1,0.9)
              call plotanot(cf,cmaj,cmin,cpa)
	      if(nchan.ne.0)call intannot(nchan,ichan,vlsr,nc)
              call pgsvp(0.1,0.7,0.1,0.9)
            endif
c Write ASCII file of channel intensities along cut
	   if(nchan.ne.0)then	
	    call writeint(v,nc,np,xstart,xend,cf,nchan,ichan,
     *        xcut(l),ycut(l),pa(l),vlsr) 
           endif
          endif
	enddo
c	
c  Replotting options.
c
        if(apint.ne.'Y'.and.write.ne.'Y')then
	  call pgiden
	  call pgqinf('HARDCOPY',ans,length)
	  call pgend
	  if(ans.ne.'YES')then
	    call output(' ')
	    call prompt(ans,length,'Another PGPLOT device? (Y/[N])')
	    call ucase(ans)
	    if(ans(1:1).eq.'Y')then 
	      call prompt(device,length,'>Enter new PGPLOT device: ')
	      call lcase(device)
	      call prompt(ans,length,'>Enter line width [1]: ')
	      if(length.ne.0)then
		read(ans,'(i1)') lwidth
	      endif
	      goto 60
	    endif 
	  endif 
	endif
c
c  Restore original plot device.
c
	device = oldevice
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine intannot(nchan,ichan,vlsr,nc)
	implicit none
	integer nchan,ichan(10),nc
	real vlsr(nc),yloc
c
c  Annotate plots.
c----------------------------------------------------------------------c
	include 'velplot.h'
	character line*80
	integer i,j
c
c  Set pg viewport to right side.
c
        call pgswin(0.0,1.0,0.0,1.0)

        write(line,'(''Velocities (km/s): '')') 
        call pgtext(0.0,0.30,line)
c  levels
	j=0
	yloc=26.
	do while(j.lt.nchan .and. yloc.gt.2.)
	  write(line,'(2(1pg10.3,x))')
     *      (vlsr(ichan(j+i)),i=1,min(2,nchan-j))
          call pgtext(0.,yloc/100,line)
	  j=j+2
	  yloc=yloc-4.
	enddo

	write(line,'(''Line Styles:  (1) ____'')')
	call pgtext(0.0,0.14,line)
	write(line,'(''  (2) _ _ _   (3) . _ . _ .'')')
	call pgtext(0.0,0.10,line)
	write(line,'(''  (4) . . . . .   (5) _ . . . _'')')
	call pgtext(0.0,0.06,line)
c
c  Don't forget to Restore original plot window.
c
	end
c********1*********2*********3*********4*********5*********6*********7**
	subroutine writeint(v,nc,np,xstart,xend,cf,nchan,ichan,
     *       x0,y0,pa,vlsr)
	implicit none
	integer nc,np,nchan,ichan(10),ii,jj,jjj
	integer lu,length,iostat
	real v(nc*np),cf,xstart,xend,xdelta,x0,y0,pa,vlsr(nc)
	real xcoord,xval(128)
c
c  Integer plot.
c
c  Inputs:
c    v		The array to print.
c    nc*np	Dimensions of array.
c----------------------------------------------------------------------c
	include 'velplot.h'
	character text*64,line*64
	character*40 outfile
c
c  Write out channel values to ASCII file.
c
        line='>Enter name for ASCII file (<cr> to continue):'
	call prompt(outfile,length,line)
	if(length.gt.0) then
	  call TxtOpen(lu,outfile,'new',iostat)
	  if(iostat.eq.0) then
c
c  Title.
c
	write(text,'(''Image: '',a)')file
	length=7 + len(file)
	if(iostat.eq.0)call TxtWrite(lu,text,length,iostat)
c
	write(text, '(''source: '',a,''  map units: '',a)')object,bunit
	length= 8 + len(object) + 13 + len(bunit)
	if(iostat.eq.0)call TxtWrite(lu,text,length,iostat)
c
	write(text, '(''Map values have been divided by: '',f9.3)')cf
	length=33 + 9
	if(iostat.eq.0)call TxtWrite(lu,text,length,iostat)
c
	write(text,
     *    '(''Cut centered at: '', f9.3, X, f9.3,''  PA : '',f9.3)')
     *    x0,y0,pa
	length = 17 + 9 + 1 + 9 + 7 + 9
	if(iostat.eq.0)call TxtWrite(lu,text,length,iostat)	
c
	write(text, '(''Channels listed are (km/s) : '')')
	length=29
	if(iostat.eq.0)call TxtWrite(lu,text,length,iostat)
c
	write(text, '(2X, 5(1x,f9.3))') (vlsr(ichan(ii)),ii=1,nchan)
	length=52
	if(iostat.eq.0)call TxtWrite(lu,text,length,iostat)
c
	write(text, '(A, A)')
     *    'The columns are: offset from center (arcsec), ',
     *    'and channel values.'
	length=64
	if(iostat.eq.0)call TxtWrite(lu,text,length,iostat)
c
	    if(cf.eq.0.)cf=1.
	    xdelta=(xstart-xend)/real(np-1)
	    if(xstart.lt.xend)xdelta=abs(xdelta)
	    if(xstart.gt.xend)xdelta=-1.0*abs(xdelta)
	    do jj=1,np
	      do ii=1,nchan
		jjj=(jj-1)*nc+ichan(ii)
		xval(ii)=v(jjj)/cf
              enddo
              xcoord=xstart+real(jj-1)*xdelta
	      if(iostat.eq.0)write(text,'(1x,6(f9.3,1x))') 
     *		     xcoord,(xval(ii),ii=1,nchan)
	      length=61
	      if(iostat.eq.0)call TxtWrite(lu,text,length,iostat)
            enddo
        call TxtClose(lu)
	endif
	endif
	end
c********1*********2*********3*********4*********5*********6*********7*c
	subroutine comment
	implicit none
c
c  Enter comment into log
c
	character*80 text
	integer ktext

	call prompt(text,ktext,'Enter comment >')

	do while(text.ne.' ')
	  call LogWrit('comment >'//text(1:ktext))
	  call prompt(text,ktext,'      comment >')
	enddo
	end
c********1*********2*********3*********4*********5*********6*********7*c
      subroutine WrGauss(outfile,nc,vlsr,cmaj,cmin,cpa)
      implicit none
c
c Inputs:
c  outfile	filename for ascii fits
c  nc		number of channels
c  vlsr		velocity array
c  ngauss	number of simultaneously fitted gaussians
c  gauss	gaussian parameters
c		 gauss(i,1) amplitude of gaussian i
c		 gauss(i,2) center velocity of gaussian i
c		 gauss(i,3) sigma of gaussian i
c----------------------------------------------------------------------
      include 'velplot.h'
      integer nc
      real vlsr(nc)
      real tmod(256,12)
      real vlsr2(1280),tmod2(1280,12),dv
      real arg1,term
      character*10    ichar10
      integer         len10,len10a,i10
      character*40 outfile
      integer lu,iostat
      real cmaj,cmin,cpa
      integer i,j,k,length,ns
      character text*110
c
      integer nspec,ngauss(49)
      real xc(49),yc(49),gauss(49,3,10),gausserr(49,3,10)
      common /spectrae/ xc,yc,nspec,ngauss,gauss,gausserr
c
c loop through spectra to form gaussian models, max 12
c
      ns = min(nspec,12)
      do k=1,ns
c
c loop through channels
c
        do i=1,nc
          tmod(i,k) = 0.      
          do j=1,ngauss(k)
            arg1=(vlsr(i)-gauss(k,2,j))/gauss(k,3,j)
            if(abs(arg1).gt.4.)then
              term=0.0
            else
              term=gauss(k,1,j)*exp(-arg1*arg1) 
            endif
            tmod(i,k) = tmod(i,k) + term
          enddo
        enddo
c
c form new gaussian model spectrum with factor of 5 oversampling
c
        dv=(vlsr(2)-vlsr(1))/5.
        do i=1,nc*5
          vlsr2(i)=vlsr(1)+(real(i)*dv)
          tmod2(i,k) = 0.      
          do j=1,ngauss(k)
            arg1=(vlsr2(i)-gauss(k,2,j))/gauss(k,3,j)
            if(abs(arg1).gt.4.)then
              term=0.0
            else
              term=gauss(k,1,j)*exp(-arg1*arg1) 
            endif
            tmod2(i,k) = tmod2(i,k) + term
          enddo
        enddo
c
c end of loop through spectra
c
      enddo
c
c  Write out gaussian fits to ASCII file.
c
	if (nspec .gt. 12) then
	  call output('only first 12 fits written to '//outfile)
        endif
	call TxtOpen(lu,outfile,'new',iostat)
	if(iostat.eq.0) write(text,'(a,a,4x,f11.6,a)')
     *	  'Gau fit for ',object,restfreq,' GHz'
	length=12 + len(object) + 15 + 4
	call TxtWrite(lu,text,length,iostat)
	if(iostat.eq.0) write(text,'(a,2f12.3,a,f6.1,a)')
     *	'Convolving beam: ',cmaj,cmin,' arcsecs, pa: ',cpa,' degrees'
	length = 17 + 12 + 12 + 14 + 6 + 8
	call TxtWrite(lu,text,length,iostat)
	if(iostat.eq.0) write(text,'(a)') 'Positions (x,y) arcsecs'
	call TxtWrite(lu,text,23,iostat)
	k=0
	dowhile(iostat.eq.0 .and. k.lt.ns)
	  write(text,'(4(a,2f7.2,a))')
     *      ('(',xc(i+k),yc(i+k),') ',i=1,min(4,ns-k))
	  length = min(4,ns-k)*(1+2*7+2)
	  call TxtWrite(lu,text,length,iostat)
	  k=k+4
	enddo
	dowhile(length.gt.0 .and. iostat.eq.0)
	  call prompt(text,length,'Enter comment :')
	  call TxtWrite(lu,text,length,iostat)
	enddo
	k=1
#ifdef vms
	do while(iostat.eq.0 .and. k.le.nc)
	  write(text,110) vlsr2(k),(tmod2(k,i),i=1,ns)
	  call TxtWrite(lu,text,10+10*ns,iostat)
	  k=k+1
	enddo
110	format(x,f9.3,<ns>(1x,f9.3))
#else
c
c  This eliminate the need to use <ns> format, which works for f77 -g
c  but not without, and not on cray.
c
c  Note that fits are written with 5x velocity oversampling
c
        do while((iostat.eq.0) .and. (k.le.(5*nc)))
           write(ichar10,'(1x,f9.3)')vlsr2(k)
           text(1:10) = ichar10(1:10)
           len10  = 11
           len10a = 20
           do i10=1,ns
              write(ichar10,'(1x,f9.3)')tmod2(k,i10)
              text(len10:len10a) = ichar10(1:10)
              len10  = len10 + 10
              len10a = len10 + 9
           enddo
          call TxtWrite(lu,text,10+10*ns,iostat)
          k=k+1
        enddo
#endif
	call TxtClose(lu)

c
      return
      end

