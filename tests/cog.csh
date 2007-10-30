#!/bin/csh -ef

echo "--------------------------------"
echo " "
echo "Test the coGauCvt routines"
echo " "
echo "--------------------------------"

set f1 = 2
set f2 = 1

cat <<"EOF" >cog_test.for
c************************************************************************
	program cotest
	implicit none
c
	include 'mirconst.h'
c
	character in*64
	integer nsize(2),lu
	real fwhm1,fwhm2,pa,f1,f2,fpa
	double precision x(3)
c
	call keyini
	call keya('in',in,'xy')
	call keyr('spar',fwhm1,1.0)
	call keyr('spar',fwhm2,1.0)
	call keyr('spar',pa,0.0)
	call keyfin
c
	call xyopen(lu,in,'old',3,nsize)
	call coinit(lu)
c
	write(*,*)fwhm1,fwhm2,pa
	fwhm1 = DPI/180.d0/3600.d0 * fwhm1
	fwhm2 = DPI/180.d0/3600.d0 * fwhm2
	pa = DPI/180.d0 * pa
c
	x(1) = 0
	x(2) = 0
	x(3) = 0
	call coGauCvt(lu,'op/op/op',x,'w',fwhm1,fwhm2,pa,
     *				       'p',f1,f2,fpa)
	write(*,*)f1,f2,180/PI*fpa
	call coGauCvt(lu,'op/op/op',x,'p',f1,f2,fpa,
     *				       'w',fwhm1,fwhm2,pa)
	fwhm1 = 3600*180/PI * fwhm1
	fwhm2 = 3600*180/PI * fwhm2
	pa = 180/PI * pa
	write(*,*)fwhm1,fwhm2,pa
	end
"EOF"

fortran -o cog_test cog_test.for `mirlibs`
rm -rf junk
imgen out=junk cell=$f2,$f1
set args = (`cog_test in=junk spar=$f1,$f2`)
set v1 = `calc "max(abs(($f1-$args[1])/$f1),abs(($f2-$args[2])/$f2))"`
set v2 = `calc "max(abs(1-$args[4]),abs(1-$args[5]))"`
set v3 = `calc "max(abs(($f1-$args[7])/$f1),abs(($f2-$args[8])/$f2))"`
set diff = `calc -i "500*max(max($v1,$v2),$v3)"`
if ( $diff != 0 ) then
  echo "The difference was, unexpectedly, not 0"
  exit 1
endif
exit 0
