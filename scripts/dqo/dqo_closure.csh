#!/bin/csh -f

set plot = ""
set vis  = ""
foreach arg ($argv)
  if ( "$arg" == "-t" ) then
    echo "File                     Th.Phase   Sigmas"
    echo "----                     --------   ------"
  else if ( "$vis" == "" ) then
    set vis = $arg
  else
    set plot = $arg
  endif
end

if ( "$vis" == "" ) exit

cat << "END-OF-FILE" > junk.awk
NR == 1             { file = $1; }
$1 == "Actual"      { rms = $7; }
$1 == "Theoretical" { trms = $7; }
END	{ printf "%-26s %5.3f    %5.2f\n", file, trms, rms/trms; }
"END-OF-FILE"

if ( "$plot" == "" ) then
  (echo $vis; closure vis=$vis options=notrip interval=8)    | awk -f junk.awk
else
  (echo $vis; closure vis=$vis options=notrip interval=8 device=$plot/ps) | awk -f junk.awk
endif

rm -f junk.awk
