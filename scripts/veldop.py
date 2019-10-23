#!/usr/bin/env python3
# Replace the velocities (veldop variable) in a Miriad uv file
# with astropy generated values for either barycenter or lsrk
# Because the calculation is a bit slow, you can set the update interval to
# something larger than the integration time (linear interpolation)
# Note that this assumes ATCA observations.
# Usage: veldop.py mirfile [bary|lsr] [update-interval(s)]
# Default: lsr, 60s

import sys
from subprocess import call
from astropy.time import Time
from astropy import units as u
from astropy.coordinates import SkyCoord, EarthLocation
import numpy as np
#import matplotlib.pyplot as plt
atca = EarthLocation('149d33m00.5s','-30d18m46.385s',236.87*u.m)

if (len(sys.argv)<2):
    print ("Usage: ",sys.argv[0],' mirfile [bary|lsr] [update-interval(s)]')
    exit(1)
vis = sys.argv[1]
lsr = len(sys.argv)<3 or sys.argv[2]=='lsr'
dt = 60.0
if len(sys.argv)>3: dt = float(sys.argv[3])
interpolate = dt>10
dt/=3600.0
# get values we need into a text file
call(['varplt','vis='+vis,'options=dtime','yaxis=ra','log=ra.log'])
call(['varplt','vis='+vis,'options=dtime','yaxis=dec','log=dec.log'])
call(['varplt','vis='+vis,'options=dtime','yaxis=veldop','log=veldop.log'])

# read in the ra,dec and velocities
ra=np.loadtxt('ra.log')[:,1]
dec=np.loadtxt('dec.log')[:,1]
tvel=np.loadtxt('veldop.log')
t=tvel[:,0]
vel=tvel[:,1]

# read time base from header
with open('veldop.log','r')as f:
    f.readline()
    line=f.readline()
mirdate=line.split()[4]
# Base time is 18JUN13:00:00:00.0
y=int(mirdate[0:2])+2000
m=['JAN','FEB','MAR','APR','MAY','JUN','JUL',
   'AUG','SEP','OCT','NOV','DEC'].index(mirdate[2:5])+1
d=int(mirdate[5:7])
timeIso='%4.4d-%2.2d-%2.2d' %(y,m,d)
mjd0=Time(timeIso).mjd
# LSRK velocity in direction of source
direction = SkyCoord('18h', '30d', frame='fk5', equinox='J1900')
velocity = 20 #*u.km/u.s
uvw_lsr = direction.galactic.cartesian
# in km/s
lightspeed = 299792.458



n=len(ra)
lastra=-1
lastdec=-1
lasttime=-1
veldif=0

meanvel = 0
count = 0

with open('newvel.log','w') as f:
    #f.write('# velocity table for '+vis+'\n')
    for i in range(0,n):
        #source change, new time interval, make sure we get the first
        # and last point on each source to avoid interpolation errors
        if (ra[i]!=lastra or dec[i]!=lastdec or t[i]-lasttime>=dt or
              i==(n-1) or ra[i+1]!=lastra or dec[i+1]!=lastdec):
            sc = SkyCoord(ra[i]*15,dec[i],frame='icrs',unit='deg')
            time=Time(mjd0+t[i]/24,format='mjd',scale='utc')
            newvel = sc.radial_velocity_correction('barycentric',obstime=time,
             location=atca).to(u.km/u.s).value
            lsrvel= sc.galactic.cartesian.dot(uvw_lsr)*velocity
            # astropy radial_velocity_correction method returns lightspeed * z_b, miriad's math would be correct 
            # if it returned lightspeed * ((z_b+1)^2 - 1) / ((z_b+1)^2 + 1)
            # See astropy issue #9269 and ATCASUP-3515
            zb = newvel / lightspeed
            zb_plus1_squared = (zb + 1.) * (zb + 1.)
            newvel = lightspeed * (zb_plus1_squared - 1.) / (zb_plus1_squared + 1.)
            #
            if lsr: 
               newvel+=lsrvel
               print ("LSR velocity component in observatory velocity: ", -lsrvel, ' km/s')
            lastra=ra[i]
            lastdec=dec[i]
            lasttime=t[i]
            veldif=max(veldif,abs(vel[i]+newvel))
            #print ra[i,0]/24,-newvel,vel[i,1],1000*(newvel+vel[i,1])
            f.write("%9.7f  %10.6f \n"%(t[i]/24,-newvel))
            meanvel += -newvel
            count = count + 1
          
print ('Maximum velocity correction: ',veldif,' km/s')
if count > 0:
   meanvel /= float(count)
   print ('Mean observatory velocity: ',meanvel,' km/s')

cmd=['uvputhd','vis='+vis,'out='+vis+'.velcor','hdvar=veldop',
      'table=newvel.log','time0='+timeIso]
if interpolate: cmd.append('options=inter')
call(cmd)
# copy caltables across if any
call(['gpcopy','vis='+vis,'out='+vis+'.velcor'])
