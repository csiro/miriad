#!/usr/bin/env python
# Based on example by Tara
#
# ATNF Synthesis Imaging School 2008
# Tara Murphy

import os

def ra2decimal(rastr):
  r = rastr.split(':')
  ra = (float(r[0]) + float(r[1])/60.0 + float(r[2])/3600.0)*15
  return ra                      


def dec2decimal(decstr):
  d = decstr.split(':')
  if d[0].startswith('-') or float(d[0]) < 0:
    dec = float(d[0]) - float(d[1])/60.0 - float(d[2])/3600.0
  else:
    dec = float(d[0]) + float(d[1])/60.0 + float(d[2])/3600.0
  return dec


def atlod(rpfits,vis,options,ifsel):
    cmd = 'atlod in=%s out=%s options=%s' % (rpfits, vis, options)
    if (len(ifsel)>0):
        cmd = cmd + ' ifsel=%s' % (ifsel)
    print cmd
    for line in os.popen(cmd):
        print line,
    return
    

def maxfit(filename):
    cmd = 'maxfit in=%s' % filename
    (rastr, decstr, cols) = (None, None, None)
    for line in os.popen(cmd):
        cols = line.split()
        if line.startswith('Peak'):
            peak = float(cols[-1])
        elif 'RA' in line:
            rastr = cols[-1]
        elif 'DEC' in line:
            decstr = cols[-1]
    return (rastr, decstr, peak)

    
def uvsplit(vis,maxwidth,sel, opt):
    cmd = 'uvsplit vis=%s maxwidth=%s' % (vis, maxwidth)
    if (len(sel)>0):
        cmd = cmd + ' "select=%s"' % (sel)
    if (len(opt)>0):
        cmd = cmd + ' options=%s' % (opt)
    cols = None
    out = []
    print cmd
    for line in os.popen(cmd):
        print line,
        cols = line.split()
        if line.startswith('Creating'):
            out.append(cols[1])
    return (out)

# assumes CABB version of uvindex with calcode field in output
def uvindex(vis):
    cmd = 'uvindex vis=%s' % vis
    (sources, cal, ra, dec, freqs, nchans, incs) = ([],[],[],[],[],[],[])
    freqlist=False
    sourcelist=False
    print cmd
    for line in os.popen(cmd):
        cols = line.split()
        if freqlist:
            if len(cols)>0:
                nchans.append(cols[0])
                freqs.append(cols[1])
                incs.append(cols[2])
            else:
              freqlist=False
        if line.startswith('  Spectral'):
            freqlist=True
        if sourcelist:
            if len(cols)>0:
                sources.append(cols[0])
                cal.append(cols[1])
                ra.append(cols[2])
                dec.append(cols[3])
            else:
                sourcelist=False
        if line.startswith(' Source'):
            sourcelist=True
    return (sources, cal, ra, dec, freqs, nchans, incs)  
          
def mfcal(vis):
    cmd = 'mfcal vis=%s ' % vis
    print cmd
    for line in os.popen(cmd):
        print line,
    return
    
def gpcal(vis,options):
    cmd = 'gpcal vis=%s "options=%s"' % (vis,options)
    print cmd
    for line in os.popen(cmd):
        print line,
    return

def gpcopy(vis,out):
    cmd = 'gpcopy vis=%s out=%s' % (vis, out)
    print cmd
    for line in os.popen(cmd):
        print line,
    return
    
# incorrect..need to change arguments    
def mfboot(vis,cal):
    cmd = 'mfboot vis=%s "select=source(%s)"' % (vis, cal)
    print cmd
    for line in os.popen(cmd):
        print line,
    return
    
