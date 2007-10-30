c*********************************************************************c
c	clplot.h
c	include file for clumplot program
c---------------------------------------------------------------------c
	common /box/ is,ie,ib,it,mid,midy
        integer      is,ie,ib,it,mid,midy
c  box in absolute pixels. (is,ib) (ie,it) can be reset by cursor.
c---------------------------------------------------------------------c

	integer maxbuf,maxdim
	parameter(maxbuf=4194304,maxdim=400)

	common/head/ras,decs,epoch,xy,vel,delv,posx,posy,pospa,
     *	     bmaj,bmin,bpa,dperjy,cbof,restfreq,posend,velend,
     *	     amin,amax,arms,niters
	real        ras,decs,epoch,xy,vel,delv,posx,posy,pospa
	real bmaj,bmin,bpa,dperjy,cbof,restfreq,posend,velend
	real amin,amax,arms
	integer		     niters
c
c  ras,decs, (epoch), bmaj,bmin (beam) [radians]
c  xy (map pixel),  [arcsecs]
c  vel,delv - lsr velocity and width of current map [km/s]
c  restfreq, [GHz]
c  posx, posy [arcsec] pospa [deg] (position wrt center and pa of l-v plot) 
c  posend - Length in arcsecs of cut in l-v plot
c  velend - Last velocity in l-v plot.  (First is in vel)
c  amin,amax,arms - min,max,rms for current plot.
c  niters - clean iterations.
c---------------------------------------------------------------------c

	common/image/file,filecf,object,bunit,ctype
	character*9       object,bunit,ctype(3)
	character*40 file,filecf
	common/plotpar/units,cneg,alabel,write,abscoord,apint,percent,
     *			maptype,pspec,gray,defgray,lgaufit,lgauplot 
	character*1    units,cneg,alabel,write,abscoord,apint,percent,
     *                  pspec,gray,defgray,lgaufit,lgauplot 
	character maptype*9,device*64
	common/args/src,levels,nlevels,conlabel,device,
     *          fg,bg,cutoff,clump,nclumps
	real    src,levels(10),fg,bg,cutoff
	integer	nlevels,conlabel,nclumps,clump(20)
c
c  file		filename of image.
c  filecf	filename of clump assignment file = file.cf
c  object	source name from image.
c  bunit	units from image.
c  ctype	types of coordinate axes. (same as FITS keywords).
c  units	units for displayed values.	[J or K]
c  cneg 	Negative contours 		[Y or N]
c  alabel 	Plot header 			[Y or N]
c  write	Write out map to a file		[Y or N]
c  abscoord	Absolute coordinate labels.	[Y or N]
c  apint	Integer Plot			[Y or N]
c  percent	Percentage contour levels	[Y or N]
c  lgaufit      Gaussian fit to spectra         [Y or N]
c  maptype	maptype			[X-Y, POS-VEL or SPECTRA]
c  src		Plot device (0=screen 1=lp 2=vers 3=imagen)
c  levels	Contour levels
c  nlevels	Number of levels
c  clump    List of clumps to plot
c  nclumps  Number of clumps
c  conlabel	Label interval for contours
c  cutoff	Cutoff level in moment maps.
c---------------------------------------------------------------------c
