#!/bin/csh -f
# This script to run xpanel.

set noglob
#setenv LD_LIBRARY_PATH /usr/local/X11/lib

if ( -x $MIRBIN/xpanel.exe ) then
  exec $MIRBIN/xpanel.exe -xrm "*font: 7x14" -iconic ${*:q}
else if ( -x $MIRBIN/xpanel.exe.static ) then
  exec $MIRBIN/xpanel.exe.static -xrm "*font: 7x14" -iconic ${*:q}
else
  echo "#### Fatal: Could not file xpane executable"
endif
