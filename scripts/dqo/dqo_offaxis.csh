#!/bin/csh -fx
########################################################################
#
# Script to apply calibration solutions from on-axis
# data to off-axis fields, make images and work out some things
# about them.  It is assumed that you have run
# dqo_init and dqo_onaxis.
# Not very modular !
#
########################################################################

  set array    = 6
  set roffsets = ( 0 0 0 0 )
  set doffsets = ( 0 0 0 0 )
  source dqo.params

  set targets=($offset1 $offset2)

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

  set alength = `echo $array|sed "s/[abcdABCD]//"`
  if ( $alength != 6 ) then
    set select = "select=-ant(6)"
  else
    set select = "select=-ant(7)"
  endif

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
    set cell = `calc "$cell*6/$alength"`

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

      rm -rf $file.{imap,qmap,umap,vmap,bem}
      invert vis=$file-cal offset=$offsetl,$offsetm \
        map=$file.imap,$file.qmap,$file.umap,$file.vmap \
        beam=$file.bem imsize=512 cell=$cell sup=0 stokes=i,q,u,v \
        options=mfs,double "$select"
    end
  end

# Deconvolve total intensity for offset1 field.
# 500 iterations may need tweaking.

  foreach frq ($flist)
    set file=$offset1.$frq
    rm -rf $file.imod $file.icln
    clean map=$file.imap beam=$file.bem \
      out=$file.imod "region=relcen,box(-250,-250,250,250)" niters=500
    restor map=$file.imap beam=$file.bem \
      model=$file.imod out=$file.icln
  end

# Put images into cubes for display.

  foreach frq ($flist)
    foreach src ($cal $offset1 $offset2)
      set file = $src.$frq
      rm -rf $file.cube
      set itype = icln
      if ($src == $offset2 ) set itype = imap
      imcat in=$file.$itype,$file.qmap,$file.umap,$file.vmap \
        options=relax out=$file.cube
      set plot=$file.ps
      cgdisp in=$file.cube type=p labtyp=hms,dms region=quarter \
        device=$plot/ps nxy=2,2 options=full,wedge range=-.003,.003
      rm -rf $file.cube
    end
  end

# Create the map statistics log file.

  set log = map.stats
  echo " " 			> $log
  dqo_mapstats -t		>> $log

  foreach src ($cal $offset1 $offset2)
    foreach frq ($flist)
      set file = $src.$frq
      if ($src != $offset2 ) then
        dqo_mapstats -x $file.icln >> $log
      else
        dqo_mapstats    $file.imap >> $log
      endif
      dqo_mapstats    $file.qmap >> $log
      dqo_mapstats    $file.umap >> $log
      dqo_mapstats    $file.vmap >> $log
    end
    echo " "			 >> $log
  end

# Get the statistics of flagged points after all flagging

  set log = flag.stats
  echo "---------------------------------------" >> $log
  echo " " >> $log
  echo "Post-reduction flagging statistics" >> $log
  echo "----------------------------------" >> $log
  dqo_flagstats -t >> $log
  foreach src ($cal $offset1 $offset2)
    foreach frq ($flist)
      dqo_flagstats $src.$frq-cal >> $log
    end
  end
