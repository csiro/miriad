#!/bin/csh -fx
#
########################################################################
# Script to execute analysis of intial stages after data quality
# observation.
#
# Creates visibility files called
#    SSSS.FFFF
#
########################################################################

# Setup of parameters:

  source dqo.params

# Determine the total observing time.

  uvindex vis=dq.uv | awk '$1 == "Total" && $2 == "observing" && $3 == "time"' > flag.stats

# Split files by source and frequency. Ditch shadowed data here too.

  foreach file ($cal $offset1 $offset2)
    rm -rf $file.*
  end
  uvsplit "select=-shadow(22)" vis=dq.uv

# Get the statistics of flagged points before any flagging

  echo " "				 >> flag.stats
  echo "Flagging statistics after ATLOD" >> flag.stats
  echo "-------------------------------" >> flag.stats
  dqo_flagstats -t >> flag.stats
  foreach src ($cal $offset1 $offset2)
    foreach frq ($flist)
      dqo_flagstats $src.$frq >> flag.stats
    end
  end

# Flag the onaxis data if requested

  if ($flag) then
    foreach frq ($flist)
      dqo_flag_cal $cal.$frq > /dev/null
    end
  endif

# Examine closure for onaxis observation

  echo "----------------------------------------------" >> flag.stats
  echo " " >> flag.stats
  echo "Closure phase statistics for the point sources" >> flag.stats
  echo "----------------------------------------------" >> flag.stats
  dqo_closure -t >> flag.stats
  foreach src ($cal $offset1)
    foreach frq ($flist)
      dqo_closure $src.$frq junk.ps >> flag.stats
    end
  end
