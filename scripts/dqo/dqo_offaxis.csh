#!/bin/csh -fx
########################################################################
# Script to apply calibration solutions from on-axis
# data to off-axis fields, make images and work out some things
# about them.  It is assumed that you have run
# doit_init and doit_onaxis.
# Not very modular !
#
#  SSSS = 1934_a, 1934_b
#  FFFF = 8640, 4800, 2382, 1344
#
# Creates images
#   SSSS.FFFF-low.bem
#   SSSS.FFFF-low.imap
#   SSSS.FFFF-low.qmap
#   SSSS.FFFF-low.umap
#   SSSS.FFFF-low.vmap
#   SSSS.FFFF-low.cube
#   1934_a.FFFF-low.imod
#   1934_a.FFFF-low.icln
#
#   SSSS.FFFF-full.bem
#   SSSS.FFFF-full.imap
#   SSSS.FFFF-full.qmap
#   SSSS.FFFF-full.umap
#   SSSS.FFFF-full.vmap
#   SSSS.FFFF-full.cube
#   1934_a.FFFF-full.imod
#   1934_a.FFFF-full.icln
#
# Creates postscript files called
#   SSSS.FFFF-full.ps
#
# Creates text files called
#   SSSS.FFFF-full-stats.txt
#
########################################################################

  set roffsets = ( 0 0 0 0 )
  set doffsets = ( 0 0 0 0 )
  source dqo.params

  set targets=($offset1 $offset2)

#  goto plot

# Copy tables and average gain solutions. Then make calibrated datasets

  foreach frq ($flist)
    foreach src ($targets)
      gpcopy vis=$cal.$frq out=$src.$frq
      puthd in=$src.$frq/interval value=5d-2
      gpaver vis=$src.$frq interval=5
      
      rm -rf $src.$frq-cal
      uvaver vis=$src.$frq out=$src.$frq-cal options=relax
    end
  end

# Flag the data on Stokes V and XX/YY if requested

  if ($flag) then
    foreach src ($targets)
      foreach frq ($flist)
        dqo_flag_stokesv $src.$frq-cal > /dev/null
        dqo_flag_xxyy $src.$frq-cal > /dev/null
      end
    end
  endif

# Make images. Natural weighting, all channels, mfs, full resolution
# Put offset1 field in centre of image

  foreach frq ($flist)
    if ("$frq" =~ 8*) then
      set cell=0.3
      set raoffset  = $roffsets[4]
      set decoffset = $doffsets[4]
    else if ("$frq" =~ 4*) then
      set cell=0.7
      set raoffset  = $roffsets[3]
      set decoffset = $doffsets[3]
    else if ("$frq" =~ 2*) then
      set cell=1.3
      set raoffset  = $roffsets[2]
      set decoffset = $doffsets[2]
    else if ("$frq" =~ 1*) then
      set cell=2
      set raoffset  = $roffsets[1]
      set decoffset = $doffsets[1]
    endif

    foreach src ($targets)

      set file=$src.$frq

      if ("$src" == $offset1) then
        set offsetl=$raoffset
        set offsetm=$decoffset
        selfcal vis=$file-cal offset=$offsetl,$offsetm "select=pol(xx,yy)" \
	  line=chan,0 options=mfs,amp interval=0.1
      else if ("$src" == $offset2) then
        set offsetl=0
        set offsetm=0
      endif

      rm -rf $file-full.{imap,qmap,umap,vmap,bem}
      invert vis=$file-cal offset=$offsetl,$offsetm \
        map=$file-full.imap,$file-full.qmap,$file-full.umap,$file-full.vmap \
        beam=$file-full.bem imsize=512 cell=$cell sup=0 stokes=i,q,u,v \
        options=mfs,double
    end
  end

# Deconvolve total intensity for offset1 field
# 500 iterations may need tweaking

  foreach frq ($flist)
    set file=$offset1.$frq
    rm -rf $file-full.imod $file-full.icln
    clean map=$file-full.imap beam=$file-full.bem \
      out=$file-full.imod "region=relcen,box(-250,-250,250,250)" niters=500
    restor map=$file-full.imap beam=$file-full.bem \
      model=$file-full.imod out=$file-full.icln
  end

# Put images into cubes for display

plot:
  foreach frq ($flist)
    set file=$offset1.$frq-full
    rm -rf $file.cube
    imcat in=$file.icln,$file.qmap,$file.umap,$file.vmap \
      options=relax out=$file.cube
    set file=$offset2.$frq-full
    rm -rf $file.cube
    imcat in=$file.imap,$file.qmap,$file.umap,$file.vmap \
      options=relax out=$file.cube

# Make grey scale. Fiddle with levels

    foreach file($offset1.$frq-full $offset2.$frq-full)
      set plot=$file.ps
      cgdisp in=$file.cube type=p labtyp=hms,dms region=quarter \
        device=$plot/ps nxy=2,2 options=full,wedge range=-.003,.003
      dqo_preview $plot
    end
  end

# Add stats to the log file.

  set log = map.stats
  foreach frq ($flist)
    set file = $offset1.$frq
    dqo_mapstats -x $file-full.icln >> $log
    dqo_mapstats    $file-full.qmap >> $log
    dqo_mapstats    $file-full.umap >> $log
    dqo_mapstats    $file-full.vmap >> $log
  end
  foreach frq ($flist)
    set file = $offset2.$frq
    dqo_mapstats    $file-full.imap >> $log
    dqo_mapstats    $file-full.qmap >> $log
    dqo_mapstats    $file-full.umap >> $log
    dqo_mapstats    $file-full.vmap >> $log
  end

# Get the statistics of flagged points after all flagging

  set log = flag.stats
  echo "---------------------------------------" >> $log
  echo " " >> $log
  echo "Post-reduction flagging statistics" >> $log
  echo "----------------------------------" >> $log
  dqo_flagstats -t >> $log
  foreach src ($sources)
    foreach frq ($flist)
      dqo_flagstats $src.$frq-cal >> $log
    end
  end
