#!/bin/csh -f

# Flag the data by converting to stokes, and clipping on V in tvclip.
# To run this xmtv must be present (unless options=notv is given)
# First parameter is uvdata in XX,XY,YX,YY format

  set vis = $1

# Convert to stokes.

  rm -rf $vis.stokes
  uvaver vis=$vis out=$vis.stokes stokes=i,q,u,v options=relax

# Clip stokes v at 4 sigma.

  tvclip vis=$vis.stokes server=xmtv@junk taver=5 clip=4 \
       "select=pol(v)" commands=clip options=notv

# Transfer flags back.

  uvaflag tvis=$vis.stokes vis=$vis
  rm -r $vis.stokes 
  uvpflag vis=$vis polt=xx,xy,yx,yy pols=xx,xy,yx,yy options=or

