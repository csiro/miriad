#!/bin/csh -f

set array = "??"
set roffsets = (0 0 0 0)
set doffsets = (0 0 0 0)
set date = (unknown date)
set refant = "unknown"
set gpcaloptions = "unknown"
source dqo.params

echo "Summary of Data Quality Observations of $date"
echo "-----------------------------------------------"
echo " "
set cdate = (`date "+%e %B %Y"`)
echo "This report was generated on $cdate"
echo " "
echo "Array:                $array"
echo " "
echo "Fields:"
echo "  Calibrator          $cal"
echo "  Offset Calibrator   $offset1"
echo "  Blank Field         $offset2"
echo " "
echo "Frequencies:  $flist[1] $flist[2] $flist[3] $flist[4] MHz"
echo " "
echo "The offset calibrator field was offset in RA,DEC from $cal by"
foreach n (1 2 3 4)
  echo "  $roffsets[$n],$doffsets[$n] arcsec at $flist[$n] MHz"
end
echo " "
echo "Reference antenna: $refant"
echo "Calibration options: $gpcaloptions"
echo " "
echo "See the end of this document for an explanation of terms and the"
echo "filenaming convention."
echo "See http://wwwnar.atnf.csiro.au/www/operations/dqo/dqo.html for"
echo "information on the DQO program."
echo " "
if ( -e notes.txt ) cat notes.txt
echo  ""
echo "========================================================================"
echo " "
echo "Visibility-based statistics"
echo "---------------------------"
echo " "

cat flag.stats

echo  ""
echo "========================================================================"
echo " "
echo "Image-based statistics"
echo "----------------------"
cat map.stats
echo  ""
cat << "END-OF-FILE"
========================================================================

Explanation of Terms:
---------------------

Th.Phase            The theoretical closure phase (in degrees) based on
                    thermal noise arguments (for the 5-minute time
                    averaging used).

Th. RMS             The theoretical RMS in an image based on thermal
                    noise arguments.

Sigmas              The factor by which the relevant measure exceeds
                    either of the above two theoretical levels. Ideally
                    this should be 1.

D.R.                Dynamic range of an image, defined as the ratio of the
                    peak absolute value divided by the RMS. For the CLEANed
                    Stokes-I images of the calibrators, the RMS is evaluated
                    outside the inner quarter of the image. Otherwise
                    the RMS is evaluated over the entire image.

Pk Val              Peak value in an image in Jy.

Explanation of File names:
--------------------------
File names are of the form

FIELD.FREQ           Raw visibility data, giving the field name and observing
                     frequency.
FIELD.FREQ-cal       Calibrated/flagged visibility data.
FIELD.FREQ.TYPE      Image of a portion of the primary beam.
                     TYPE gives the Stokes parameter and
                     whether the image is CLEANed ("cln") or a
                     dirty image ("map").
"END-OF-FILE"
