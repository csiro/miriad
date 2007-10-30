#!/bin/csh -xf
#
########################################################################
#
# Script to calibrate and image the onaxis observation observed
# at 4 frequencies. It is assumed that the data were loaded with 
# dqo_init and have been split into sources and frequencies.
#
########################################################################

# set parameters

  set array = 6
  source dqo.params

########################################################################
#    
#    CALIBRATION
#
########################################################################

# Delete all calibration tables first

  foreach frq ($flist)
    delhd in=$cal.$frq/leakage >& /dev/null
    delhd in=$cal.$frq/gains   >& /dev/null
    delhd in=$cal.$frq/bandpass>& /dev/null
  end

# Determine calibration.

  foreach frq ($flist)
    mfcal vis=$cal.$frq interval=0.1 refant=$refant
    gpcal vis=$cal.$frq interval=0.1 options=$gpcaloptions refant=$refant
    puthd in=$cal.$frq/interval value=1d-4
  end

# Apply tables and write calibrated data sets

  foreach frq ($flist)
    uvaver vis=$cal.$frq out=$cal.$frq-cal options=relax
  end

# Flag the calibrated data on stokes V if requested

  if ($flag) then 
    foreach frq ($flist)
      dqo_flag_stokesv $cal.$frq-cal > /dev/null
    end
  endif

########################################################################
#
#   HIGH RESOLUTION IMAGING
#
########################################################################

# Make images. Natural weighting, all channels, mfs, full resolution

  set alength = `echo $array|sed "s/[abcdABCD]//"`
  if ( $alength != 6 ) then
    set select = "select=-ant(6)"
  else
    set select = ""
  endif

  foreach frq ($flist)
    if ("$frq" =~ 8*) then
      set cell=0.3
    else if ("$frq" =~ 4*) then
      set cell=0.7
    else if ("$frq" =~ 2*) then
      set cell=1.3
    else if ("$frq" =~ 1*) then
      set cell=2
    endif
    set cell = `calc "$cell*6/$alength"`

    set file=$cal.$frq
    invert vis=$file \
      map=$file.imap,$file.qmap,$file.umap,$file.vmap \
      beam=$file.bem imsize=512 cell=$cell sup=0 stokes=i,q,u,v \
      options=mfs,double $select

# Deconvolve total intensity.  500 iterations may need tweaking

    clean map=$file.imap beam=$file.bem out=$file.imod \
      "region=relpix,box(-250,-250,250,250)" niters=500
    restor map=$file.imap beam=$file.bem model=$file.imod \
      out=$file.icln
  end

