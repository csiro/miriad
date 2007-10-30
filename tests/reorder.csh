#!/bin/csh -f
#
#  Test scripts for the reorder program

set n1 = 213
set n2 = 131
set n3 = 19

@ n1d = $n1 - 1
@ n2d = $n2 - 1
@ n3d = $n3 - 1

rm -rf junk1 junk2 junk3

maths "exp=x+$n1*y+$n1*$n2*z" imsize=$n1,$n2,$n3 xrange=0,$n1d \
	yrange=0,$n2d zrange=0,$n3d out=junk1

echo "Trying 123 ..."
reorder in=junk1 out=junk2 mode=123
maths exp=junk1-junk2 out=junk3
set line = ( `histo in=junk3|sed -e 1,2d` )
if ( "$line" != "All pixels are 0.00" ) then
  echo "Test 123 failed"
  exit 1
endif
rm -r junk2 junk3

echo "Trying 132 ..."
reorder in=junk1 out=junk2 mode=132
maths "exp=x+$n1*$n2*y+$n1*z-junk2" xrange=0,$n1d \
	yrange=0,$n3d zrange=0,$n2d out=junk3
set line = ( `histo in=junk3|sed -e 1,2d` )
if ( "$line" != "All pixels are 0.00" ) then
  echo "Test 132 failed"
  exit 1
endif
rm -r junk2 junk3

echo "Trying 213 ..."
reorder in=junk1 out=junk2 mode=213
maths "exp=$n1*x+y+$n1*$n2*z-junk2" xrange=0,$n2d \
	yrange=0,$n1d zrange=0,$n3d out=junk3
set line = ( `histo in=junk3|sed -e 1,2d` )
if ( "$line" != "All pixels are 0.00" ) then
  echo "Test 213 failed"
  exit 1
endif
rm -r junk2 junk3

echo "Trying 231 ..."
reorder in=junk1 out=junk2 mode=231
maths "exp=$n1*x+$n1*$n2*y+z-junk2" xrange=0,$n2d \
	yrange=0,$n3d zrange=0,$n1d out=junk3
set line = ( `histo in=junk3|sed -e 1,2d` )
if ( "$line" != "All pixels are 0.00" ) then
  echo "Test 231 failed"
  exit 1
endif
rm -r junk2 junk3

echo "Trying 312 ..."
reorder in=junk1 out=junk2 mode=312
maths "exp=$n1*$n2*x+y+$n1*z-junk2" xrange=0,$n3d \
	yrange=0,$n1d zrange=0,$n2d out=junk3
set line = ( `histo in=junk3|sed -e 1,2d` )
if ( "$line" != "All pixels are 0.00" ) then
  echo "Test 312 failed"
  exit 1
endif
rm -r junk2 junk3

echo "Trying 321 ..."
reorder in=junk1 out=junk2 mode=321
maths "exp=$n1*$n2*x+$n1*y+z-junk2" xrange=0,$n3d \
	yrange=0,$n2d zrange=0,$n1d out=junk3
set line = ( `histo in=junk3|sed -e 1,2d` )
if ( "$line" != "All pixels are 0.00" ) then
  echo "Test 321 failed"
  exit 1
endif
rm -r junk2 junk3

rm -r junk1
