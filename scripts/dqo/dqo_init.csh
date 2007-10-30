#!/bin/csh -fx
#
########################################################################
# Script to execute analysis of intial stages after data quality
# observation.  Loads data from exabyte or disk, plots samplers 
# and XY phases, applies XY phases, and splits files into sources and 
# frequencies
#
# If the first parameter to the script is "flag", the loading and
# sampler plotting stages will be skipped and processing will start
# with flagging the split data.
#
# Sources are: 
#   SSSS = 1934-638 (onaxis), 1934_a (2' offaxis), 1934_b (2deg offaxis)
#          or similar for different source
# Frequencies are:
#   FFFF = 8640, 4800, 2382, 1344
#
#
# Creates postscript files called
#    xsampler.ps
#    ysampler.ps
#    xyphase.ps
#    closure_1934-638.FFFF.ps
#    vis_amp_SSSS.FFFF.ps 
#    spec_SSSS.FFFF.ps
#
# Creates visibility files called
#    SSSS.FFFF
#
########################################################################

# Setup of parameters:

  source dqo.params

  set goto=start
  if ("$1" != "") set goto=$1
  goto $goto
start:

# Examine samplers and XY phases

  foreach i (xsampler ysampler xyphase)
    varplt vis=dq.uv nxy=3,2 yaxis=$i device=$i.ps/ps
    dqo_preview $i.ps
  end

# Apply XY phases of reference antenna.  Should be best one
# so may need to change refant.  Zero applied for all 
# other antennas. 

#  atxy vis=dq.uv out=dq.atxy interval=10 refant=$refant

# Split files by source and frequency. Ditch shadowed data here too.

#  uvsplit "select=-shadow(22)" vis=dq.atxy
  uvsplit "select=-shadow(22)" vis=dq.uv

# Remove XY phase corrected data

#  rm -rf  dq.atxy

# Get the statistics of flagged points before any flagging

  echo "Flagging statistics after ATLOD" > flag.stats
  echo "-------------------------------" >> flag.stats
  dqo_flagstats -t >> flag.stats
  foreach src ($sources)
    foreach frq ($flist)
      dqo_flagstats $src.$frq >> flag.stats
    end
  end

# Flag the onaxis data if requested

flag:
  if ($flag) then
    foreach frq ($flist)
      dqo_flag_cal $cal.$frq > /dev/null
    end
  endif

# Examine closure for onaxis observation

  echo "----------------------------------------------" >> flag.stats
  echo " " >> flag.stats
  echo "Closure phase statistics for the point sources" >> flag.stats
  echi "----------------------------------------------" >> flag.stats
  dqo_closure -t >> flag.stats
  foreach src ($cal $offset1)
    foreach frq ($flist)
      set plot=$src.$frq.closure.ps
      dqo_closure $src.$frq $plot >> flag.stats
      dqo_preview $plot
    end
  end

# Examine visibility amplitudes for each field. Average a few channels together

  foreach src ($sources)
    foreach frq ($flist)
      set plot=$src.$frq.vis_amp.ps
      uvplt vis=$src.$frq stokes=xx,yy line=channel,1,5,3 \
        nxy=4,4 device=$plot/ps
      dqo_preview $plot
    end

# Examine some onaxis spectra 

    foreach frq ($flist)
      set plot=$src.$frq.spec.ps
      uvspec vis=$src.$frq stokes=xx,yy nxy=4,4 \
        interval=1000000 device=$plot/cps
      dqo_preview $plot
    end
  end
