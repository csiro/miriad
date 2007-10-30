#!/bin/csh -f

# Flagging calibrator: pre calibration flagging of data
# Flags the data using the clip option in tvclip.
# First parameter is uvdata in XX,XY,YX,YY format.

  set vis=$1

# Clip xx yy at 6 sigma with sequence: DIFF CLIP DIFF CLIP EXIT.

  foreach pol (xx yy)
    tvclip vis=$vis server=xmtv@junk taver=5 clip=6 "select=pol($pol)"\
      commands=diff,clip,diff,clip options=notv
  end

#  Clip xy yx at 6 sigma with sequence: CLIP DIFF CLIP EXIT.

  foreach pol (xy yx)
    tvclip vis=$vis server=xmtv@junk taver=5 clip=6 "select=pol($pol)"\
      commands=clip,diff,clip options=notv
  end

# Transfer flags to all polarizations.

  uvpflag vis=$vis polt=xx,xy,yx,yy pols=xx,xy,yx,yy options=or

