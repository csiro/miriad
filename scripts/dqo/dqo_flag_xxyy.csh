#!/bin/csh -f

# Flag the XX, YY data for outliers missed in the Stokes V flagging
# This can be dangerous... Some source structure may look like interference.

# To run this xmtv must be present (unless options=notv is given)

# First parameter is uvdata in XX,XY,YX,YY format.

  set vis=$1

# 6 sigma clip of XX, YY
# this seems to work ok even if there are strong sources at the edge of 
# the field (doesn't clip much on long baselines in that case)

  foreach pol (xx yy)
    tvclip vis=$vis server=xmtv@junk taver=5 clip=6 \
      "select=pol($pol)" commands=clip options=notv
  end
  uvpflag vis=$vis polt=xx,xy,yx,yy pols=xx,xy,yx,yy options=or

