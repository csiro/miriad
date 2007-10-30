#!/bin/csh -ef

echo "--------------------------------"
echo " "
echo "A simple test of the calc tool"
echo " "
echo "--------------------------------"


set i = `calc -i "1+1"`
set j = `calc -i "1e5*(pi-4*atan(1))"`

if ( $i != 2 || $j != 0 ) then
  echo "### Calc failed to give the right answer"
  exit 1
endif
exit 0
