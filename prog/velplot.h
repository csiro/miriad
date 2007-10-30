c*********************************************************************c
c		velplot.h
c	include file for velplot program
c---------------------------------------------------------------------c
c
c  The maximum array dimensions and size
c  MAXDIM is used to read the image, and for 1-dimensional arrays.
	include 'maxdim.h'
c
c  box in absolute pixels. (is,ib) (ie,it) can be reset by cursor.
        integer      is,ie,ib,it,mid,midy
	common /box/ is,ie,ib,it,mid,midy
c---------------------------------------------------------------------c

	real        ras,decs,epoch,xy,vel,delv,posx,posy,pospa
	real bmaj,bmin,bpa,dperjy,cbof,restfreq,posend,velend
	real amin,amax,arms
	integer		     niters
	common/head/ras,decs,epoch,xy,vel,delv,posx,posy,pospa,
     *	     bmaj,bmin,bpa,dperjy,cbof,restfreq,posend,velend,
     *	     amin,amax,arms,niters
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

	character*9       object,bunit,ctype(3)
	character*40 file
	common/image/file,object,bunit,ctype
c
	character*1    units,cneg,alabel,write,abscoord,apint,percent,
     *                  pspec,gray,defgray,lgaufit,lgauplot 
	character maptype*9
	common/plotpar/units,cneg,alabel,write,abscoord,apint,percent,
     *			maptype,pspec,gray,defgray,lgaufit,lgauplot 
c
	character device*64
	real    src,levels(10),fg,bg,cutoff
	integer	nlevels,conlabel
	common/args/src,levels,nlevels,conlabel,fg,bg,cutoff
	common/chargs/device
c
c  file		filename of image.
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
c  conlabel	Label interval for contours
c  cutoff	Cutoff level in moment maps.
c---------------------------------------------------------------------c

