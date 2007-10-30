#!/bin/csh -f

set file = ""
foreach arg ($argv)
  if ( "$arg" == "-t" ) then
    echo "File                          XX%     YY%     XY%     YX%"
    echo "----                          ---     ---     ---     ---"
  else
    set file = $arg
  endif
end

if ( "$file" == "" ) exit

if ( ! -d $file ) then
  echo "### Input $file is not a Miriad dataset"
  exit
endif

cat << "END-OF-FILE" > junk.awk
$1 == "Filename" { source = $2 }
$1 == "XX"       { xx = $2 }
$1 == "XY"       { xy = $2 }
$1 == "YX"       { yx = $2 }
$1 == "YY"       { yy = $2 }
END		 { printf "%-26s %7s %7s %7s %7s\n", source, xx, yy, xy, yx;}
"END-OF-FILE"

( echo Filename $file ; uvfstats vis=$file ) | awk -f junk.awk
rm -rf junk.awk
