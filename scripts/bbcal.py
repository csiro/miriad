#!/usr/bin/env python
# bbcal - broad band calibration : calibrate CABB data in bandwidth limited chunks to allow
# for frequency dependent leakage and complex gain variation
from commands import *
import Miriad

help = """
       bbcal broad band calibration : calibrate CABB data in bandwidth limited
       chunks to allow for frequency dependent leakage and complex gain variation
       """
keyval = {
  "rpfits" : " \n RPFITS input file(s) (optional)",
  "vis" : " \n visibility file",
  "pri" : "1934-638\n flux calibrator",
  "sec" : "sec\n secondary (phase) calibrator",
  "bp" : "none\n bandpass calibrator",
  "maxwidth": "0.512\n max bandwidth per file in GHz"
  }
  
Miriad.keyini(keyval,usage=help);

rpfits = Miriad.keya('rpfits')
vis = Miriad.keya('vis')
pri = Miriad.keya('pri')
sec = Miriad.keya('sec')
bp  = Miriad.keya('bp')
if (bp == 'none'): bp = pri
maxwidth = Miriad.keya('maxwidth')

if (len(rpfits)>0):
#atlod
    Miriad.zap(vis)
    atlod(rpfits,vis,"noif,rfiflag","") 

# get some info on the contents of the uv file
(sources,cal,ra,dec,freqs,nchans,incs)=uvindex(vis)

# calculate the bandwidth for each spectral window
nwin = len(nchans)
bw = [float(incs[i])*int(nchans[i]) for i in range(nwin)]

print nwin,bw

# find the 2GHz continuum bands
sel = ''
for i in range(nwin):
    if (bw[i]>2):
        if (i>0): sel=sel+','
        sel+=str(i+1)
    
if (sel==''): sel='1' # process first spectral window if there are no cabb continuum bands
print 'sel=',sel    
    
#split up the continuum bands
uvfile=[]
select='window(%s),source(%s,%s,%s)' % (sel, pri, sec, bp)
options="clobber"
uvfile.extend(uvsplit(vis,maxwidth,select,options))
select='window(%s),-source(%s,%s,%s)' % (sel, pri, sec, bp)
options="clobber,nosource"
uvfile.extend(uvsplit(vis,maxwidth,select,options))
print uvfile
    
#now do the calibration

#calibration
sources = []
splitfreqs =[]
calfiles=[]
for ifile in uvfile:
    sources.append(ifile.split('.'))
    freq = ifile.split('.')[1]
    if freq not in splitfreqs : splitfreqs.append(freq)

if (pri == bp):   # standard cm style calibration
    for freq in splitfreqs:
        prifile = pri+'.'+freq
        if (prifile not in uvfile):
            print '%s not found' % prifile
        else:
            mfcal(prifile)
            gpcal(prifile,'xyvary')
            secfile = sec+'.'+freq
            if (secfile not in uvfile):
                print '%s not found' % secfile
            else:
                gpcopy(prifile,secfile)
                gpcal(secfile,'xyvary,qusolve,nopol')
                mfboot(secfile+','+prifile,pri)                
                srcfile = 'uvsplit.'+freq
                if (srcfile not in uvfile):
                    print '%s not found' % srcfile
                else:
                    gpcopy(secfile,srcfile)
                    calfiles.append(srcfile)
else:   # mm style calibration, e.g., planet for flux, 1921 for bandpass
    for freq in splitfreqs:
        bpfile = bp+'.'+freq
        if (bpfile not in uvfile):
            print '%s not found' % bpfile
        else:
            mfcal(bpfile)
            secfile = isec+'.'+freq
            if (secfile not in uvfile):
                print '%s not found' % secfile
            else:
                gpcopy(bpfile,secfile)
                gpcal(secfile,'xyvary,qusolve')
                prifile = pri+'.'+freq
                if (prifile not in uvfile):
                  print '%s not found' % prifile
                else:
                  gpcopy(bpfile,prifile)
                  mfboot(secfile+','+prifile,pri)
                  srcfile= 'uvsplit.'+freq
                  if (srcfile not in uvfile):
                      print '%s not found' % srcfile
                  else:
                      gpcopy(secfile,srcfile)
                      calfiles.append(srcfile)
          
# all fields now calibrated
print "Calibrated data in ",calfiles

# imaging process to be done..


