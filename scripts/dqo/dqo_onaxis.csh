#!/bin/csh -xf
#
########################################################################
# Script to calibrate and image the onaxis observation observed
# at 4 frequencies. It is assumed that the data were loaded with 
# doit_init and have been split into sources and frequencies
# Not very modular
#
#  FFFF = 8640,4800,2382,1344
#  Actual source name is set in dqo.params (here 1934-638)
#
# Creates calibration tables in the visibility files named
#    1934-638.FFFF    
# Creates calibrated visibility files called
#    1934-638.FFFF-cal 
#
# Creates images
#   1934-638.FFFF-low.bem
#   1934-638.FFFF-low.imap
#   1934-638.FFFF-low.imod
#   1934-638.FFFF-low.icln
#   1934-638.FFFF-low.qmap
#   1934-638.FFFF-low.umap
#   1934-638.FFFF-low.vmap
#   1934-638.FFFF-low.cube
#
#   1934-638.FFFF-full.bem
#   1934-638.FFFF-full.imap
#   1934-638.FFFF-full.imod
#   1934-638.FFFF-full.icln
#   1934-638.FFFF-full.qmap
#   1934-638.FFFF-full.umap
#   1934-638.FFFF-full.vmap
#   1934-638.FFFF-full.cube
#
# Creates postscript files called
#   bandpass_amp_FFFF.ps
#   bandpass_phase_FFFF.ps
#   gains_amp_FFFF.ps
#   plot_gains_phase_FFFF.ps
#   1934-638-plot_calvis_ri_FFFF.ps
#   1934-638.FFFF-low.ps
#   1934-638.FFFF-full.ps
#
# Creates text files called
#   1934-638-leakages.txt
#   1934-638-uvfluxes.txt
#   1934-638.FFFF-low-headers.txt
#   1934-638.FFFF-full-headers.txt
#   1934-638.FFFF-full-histos.txt
#
# Use cleanup_onaxis to delete all the files that doit_onaxis makes
#
########################################################################

# set parameters

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

# Plot calibration; amplitude and phase

  foreach frq ($flist)
    set plot=bandpass_gain.$frq.ps
    gpplt vis=$cal.$frq nxy=3,2 yaxis=amp,phase options=bandpass device=$plot/ps
    dqo_preview $plot

    set plot=gains.$frq.ps
    gpplt vis=$cal.$frq nxy=3,2 yaxis=amp,phase device=$plot/ps
    dqo_preview $plot
  end

# Print leakages.

  set log=$cal-leakages.txt
  rm -f $log
  foreach frq ($flist)
    gpplt vis=$cal.$frq options=pol log=$log.$frq
    cat $log.$frq >> $log
    rm -f $log.$frq
  end
  if ($hardcopy) lwp -P$printer -m80 $log
  if ($preview) more $log

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

# Plot calibrated data; real versus imaginary. May need
# to change channels averaged together

  foreach frq ($flist)
    set plot=$cal.$frq.calvis_ri.ps
    uvplt vis=$cal.$frq-cal stokes=i,q,u,v axis=real,imag options=nobase,equal \
      device=$plot/cps line=channel,1,1,13
    dqo_preview $plot
  end

# Determine flux densities

  set log=$cal-uvfluxes.txt
  rm -f $log
  foreach frq ($flist)
    uvflux vis=$cal.$frq line=channel,0 stokes=i,q,u,v >> $log
  end
  if ($hardcopy) lwp -P$printer $log
  if ($preview) more $log

########################################################################
#
#   LOW RESOLUTION IMAGING
#
########################################################################

# Make images. Uniform weighting, all channels, mfs, low resolution.
# Synthesised beam FWHM and cell size set to image full primary beam FWHM
# with a 256 square image
 
  foreach frq ($flist)
    if ("$frq" =~ 8*) then
      set cell=1.5
      set fwhm=5
    else if ("$frq" =~ 4*) then
      set cell=2.5
      set fwhm=8
    else if ("$frq" =~ 2*) then
      set cell=5.5
      set fwhm=17
    else if ("$frq" =~ 1*) then
      set cell=8
      set fwhm=24
    endif

    set file=$cal.$frq
    invert vis=$file \
      map=$file-low.imap,$file-low.qmap,$file-low.umap,$file-low.vmap \
      beam=$file-low.bem imsize=256 cell=$cell fwhm=$fwhm stokes=i,q,u,v \
      options=mfs,double

# Deconvolve total intensity.  250 iterations may need tweaking

    clean map=$file-low.imap beam=$file-low.bem out=$file-low.imod \
      "region=relpix,box(-120,-120,120,120)" niters=250
    restor map=$file-low.imap beam=$file-low.bem model=$file-low.imod \
      out=$file-low.icln fwhm=$fwhm
  end

# Print headers and stats and put into a file for each frequency
# Filter out the uninteresting lines.

stats1:
  set log = map.stats
  echo " " > $log
  echo "Image Statistics"
  echo "----------------"
  dqo_mapstats -t >> $log
  foreach frq ($flist)
    set file=$cal.$frq
    dqo_mapstats -x $file-low.icln >> $log
    dqo_mapstats    $file-low.qmap >> $log
    dqo_mapstats    $file-low.umap >> $log
    dqo_mapstats    $file-low.vmap >> $log
  end

# Put images into cubes for display

  foreach frq ($flist)
    set file=$cal.$frq
    imcat in=$file-low.icln,$file-low.qmap,$file-low.umap,$file-low.vmap \
      options=relax out=$file-low.cube
  end

# Make grey scale. FIddle with levels

  foreach frq ($flist)
    set file=$cal.$frq
    set plot=$file-low.ps
    cgdisp in=$file-low.cube type=p labtyp=hms,dms \
      device=$plot/ps nxy=2,2 options=full,wedge range=-.01,.01
    dqo_preview $plot
    rm  -rf $file-low.cube
  end

########################################################################
#
#   HIGH RESOLUTION IMAGING
#
########################################################################

# Make images. Natural weighting, all channels, mfs, full resolution

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

    set file=$cal.$frq
    invert vis=$file \
      map=$file-full.imap,$file-full.qmap,$file-full.umap,$file-full.vmap \
      beam=$file-full.bem imsize=512 cell=$cell sup=0 stokes=i,q,u,v \
      options=mfs,double

# Deconvolve total intensity.  500 iterations may need tweaking

    clean map=$file-full.imap beam=$file-full.bem out=$file-full.imod \
      "region=relpix,box(-250,-250,250,250)" niters=500
    restor map=$file-full.imap beam=$file-full.bem model=$file-full.imod \
      out=$file-full.icln
  end

# Print headers and stats and put into a file for each frequency
# Filter out the uninteresting lines.

  set log = map.stats
  foreach frq ($flist)
    set file = $cal.$frq
    dqo_mapstats -x $file-full.icln >> $log
    dqo_mapstats    $file-full.qmap >> $log
    dqo_mapstats    $file-full.umap >> $log
    dqo_mapstats    $file-full.vmap >> $log
  end

# Put images into cubes for display

  foreach frq ($flist)
    set file=$cal.$frq
    imcat in=$file-full.icln,$file-full.qmap,$file-full.umap,$file-full.vmap \
      options=relax out=$file-full.cube
  end
 
# Make grey scale. Fiddle with levels

grey:
  foreach frq ($flist)
    set file=$cal.$frq
    set plot=$file-full.ps
    cgdisp in=$file-full.cube type=p labtyp=hms,dms region=quarter \
      device=$plot/ps nxy=2,2 options=full,wedge range=-.003,.003
    dqo_preview $plot
    rm -rf $file-full.cube
  end

