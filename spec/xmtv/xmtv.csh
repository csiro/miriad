#!/bin/csh -f
# This script will enable you to run the XMtv application without
# having to move the XMtv resource file to your home or defaults directory.

set noglob
#setenv LD_LIBRARY_PATH /usr/local/X11/lib
setenv XAPPLRESDIR $MIRCAT

if ( -x $MIRBIN/xmtv.exe ) then
  exec $MIRBIN/xmtv.exe -xrm "*font: 7x14" ${*:q}
else if ( -x $MIRBIN/xmtv.exe.static ) then
  exec $MIRBIN/xmtv.exe.static -xrm "*font: 7x14" ${*:q}
else if ( -x $MIRBIN/sxmtv.exe ) then
  echo "Unable to find xmtv -- running Simple xmtv (sxmtv) instead"
  exec $MIRBIN/sxmtv.exe ${*:q}
else
  echo "Unable to find xmtv executable"
endif
