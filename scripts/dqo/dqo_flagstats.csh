#!/bin/csh -f

set file = ""
foreach arg ($argv)
  if ( "$arg" == "-t" ) then
    echo "File             Overall   CA01   CA02   CA03   CA04   CA05   CA06"
    echo "----             -------   ----   ----   ----   ----   ----   ----"
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
$1 == "Overall"  { overall = $2 }
$1 == "1"       { ca01 = $2 }
$1 == "2"       { ca02 = $2 }
$1 == "3"       { ca03 = $2 }
$1 == "4"       { ca04 = $2 }
$1 == "5"       { ca05 = $2 }
$1 == "6"       { ca06 = $2 }
END        { printf "%-17s%7s%7s%7s%7s%7s%7s%7s\n",source,overall,ca01,ca02,ca03,ca04,ca05,ca06;}
"END-OF-FILE"

( echo Filename $file ; uvfstats vis=$file mode=overall; \
			uvfstats vis=$file mode=antenna) | awk -f junk.awk
rm -rf junk.awk
