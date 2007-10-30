#!/bin/csh -f

set file = ""
set doexclude = 0

foreach arg ($argv)
  if ( "$arg" == "-x" ) then
    set doexclude = 1
  else if ( "$arg" == "-t" ) then
    echo "File                       Th. RMS      Sigmas  D.R.  Pk Val"
    echo "----                       -------      ------  ----  ------"
  else
    set file = "$arg"
  endif
end

if ( "$file" == "" ) exit
if ( ! -d $file ) then
  echo "### Dataset $file not found"
  exit 1
endif

histo in=$file > junk1.log
if ( $doexclude ) then
  set nx = `gethd in=$file/naxis1`
  set ny = `gethd in=$file/naxis2`
  @ ny2   = $ny / 2
  @ ny2p2 = $ny / 2 + 2
  @ nx4 = $nx / 4
  @ ny4 = $ny / 4
  @ nx34 = 3 * $nx / 4
  @ ny34 = 3 * $ny / 4
  histo in=$file \
    "region=poly(1,1,$nx,1,$nx,$ny,1,$ny,1,$ny2p2,$nx4,$ny2p2,$nx4,$ny34,$nx34,$ny34,$nx34,$ny4,$nx4,$ny4,$nx4,$ny2,1,$ny2)" \
	> junk2.log
else
  cp junk1.log junk2.log
endif

set maxv = `awk '$1 == "Maximum" {print $3}' junk1.log`
set minv = `awk '$1 == "Minimum" {print $3}' junk1.log`
set rms  = `awk '$1 == "Mean"	 {print $4}' junk2.log`
set trms = `gethd in=$file/rms`
set pk = `calc "max($maxv,abs($minv))"`
set dr = `calc -i "anint($pk/$rms)"`
set ratio = `calc -f f8.2 "$rms/$trms"`
set trms = `calc -f 1pe8.2 "$trms"`
set pk   = `calc -f f6.3 "$pk"`

cat << "END-OF-FILE" > junk.awk
{ if( $4 >= 10 )printf "%-26s %8s  %8s  %5d  %6s\n",$1,$2,$3,$4,$5;
  else          printf "%-26s %8s  %8s         %6s\n",$1,$2,$3,$5; }
"END-OF-FILE"

echo "$file $trms $ratio $dr $pk" | awk -f junk.awk
