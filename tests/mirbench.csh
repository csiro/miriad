#! /bin/csh -ef
#
#  mir.bench: see also http://bima.astro.umd.edu/memo/abstracts.html#81
#
#
set tmp=bench$$
mkdir $tmp
cd $tmp

#			default parameters  (nchan=8, mapsize=512 for a test)
set nchan=32
set mapsize=1024
set cell=0.5
set standalone=0
set clean=0

foreach a ($*)
  set $a
end

#			standalone version has attached bins + libs
if ($standalone) then
   #echo IFREDO: cp -a $MIRBIN/{uvgen,uvcat,invert,clean,restor} .
   #echo IFREDO: cp -a $MIRLIB/libmir.so .
   #echo IFREDO: cp -a /usr/lib/libpgplot.so* .
   set path=(.. $path)
   rehash
   setenv  LD_LIBRARY_PATH ..
endif	

echo "MIRBENCH: (2003-12-05) $tmp : nchan=$nchan mapsize=$mapsize"
echo hostname=`hostname` 
echo benchdir=$tmp
echo system=`uname -a`  
echo date=`date`
echo MIR=$MIR
echo 'Needs:  uvgen uvcat invert clean restor'
echo 'Also from $MIRCAT it needs: bima9_a.ant bima9_b.ant bima9_c.ant.equ'
echo 'cd $MIR;tar cf /tmp/mirbench.tar $MIRBIN/{uvgen,uvcat,invert,clean,restor} $MIRCAT/{bima9_a.ant,bima9_b.ant,bima9_c.ant.equ}'


echo "1,0,0,0,0,0,0,0,0" > point.source
#set corr=0,1,100.0,1000.0 
set corr=$nchan,1,100.0,1000.0
set spectra=1.0,100.0,0.1
set harange=-6,6,0.01
set gnoise=0.1
set pnoise=10,0,0,0
set systemp=75,290,0.15
set tpower=0,0
set jyperk=150
set ant=($MIRCAT/bima9_a.ant $MIRCAT/bima9_b.ant $MIRCAT/bima9_c.ant.equ)

# be careful, MIRIAD uses $TMPDIR, and better make sure this is a local disk
# alternatively, you can set:
# setenv TMPDIR /tmp

time uvgen out=vis1 harange=$harange source=$MIRCAT/point.source \
 corr=$corr spectra=$spectra ant=$ant[1] \
 gnoise=$gnoise pnoise=$pnoise systemp=$systemp tpower=$tpower jyperk=$jyperk \
 > uvgen1.log
time uvgen out=vis2 harange=$harange source=$MIRCAT/point.source \
 corr=$corr spectra=$spectra ant=$ant[2] \
 gnoise=$gnoise pnoise=$pnoise systemp=$systemp tpower=$tpower jyperk=$jyperk \
 > uvgen2.log
time uvgen out=vis3 harange=$harange source=$MIRCAT/point.source \
 corr=$corr spectra=$spectra ant=$ant[3] \
 gnoise=$gnoise pnoise=$pnoise systemp=$systemp tpower=$tpower jyperk=$jyperk \
 > uvgen3.log

time uvcat vis=vis1,vis2,vis3 out=vis > uvcat.log
# echo May need to add 'line=channel,$nchan' to invert for the old versions...
time invert vis=vis map=map1 beam=beam1 imsize=$mapsize cell=$cell > invert.log
time clean map=map1 beam=beam1 out=clean1 > clean.log
time restor model=clean1 beam=beam1 map=map1 out=cmap1 > restor.log
echo The mirstones can now be obtained to divide 5 minutes by the total elapsed time:

if ($clean) rm -rf $tmp.*
