#!/bin/csh

source dqo.params

echo "Summary of Data Quality Observations of $date"
echo "------------------------------------------------"
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
echo "See the end of this document for an explanation of terms and the"
echo "filenaming convention."
echo " "
echo "------------------------------------------------------------------------"
echo " "

cat flag.stats

echo " "
cat map.stats

cat << "END-OF-FILE"



========================================================================
Explanation of Terms:
---------------------
XX%,YY%,XY%,YY%     Percentage of XX, XY, YX and YY correlations flagged
		    (excluding on-line flagging and shadowing).

Th.Phase	    The theoretical closure phase (in degrees) based on
		    thermal noise arguments (for the 5-minute time
		    averaging used).

Th. RMS 	    The theoretical RMS in an image based on thermal
		    noise arguments.

Sigmas		    The factor by which the relevant meausre exceeds
		    either of the above two theoretical levels. Ideally
		    this should be 1.

D.R.		    Dynamic range of an image, defined as the ratio of the
		    peak absolute value divided by the RMS. For the CLEANed
		    Stokes-I images of the calibrators, the RMS is evaluated
		    outside the inner quarter of the image. Otherwise
		    the RMS is evaluated over the entire image.

Pk Val		    Peak value in an image in Jy.

Explanation of File names:
--------------------------
File names are of the form

FIELD.FREQ	     Raw visibility data, giving the field name and observing
		     frequency.
FIELD.FREQ-cal	     Calibrated/flagged visibility data.
FIELD.FREQ-low.TYPE  Low resolution image, imaging the entire primary
		     beam. TYPE gives the Stokes parameter and
		     whether the image is CLEANed ("cln") or a
		     dirty image ("map").
FIELD.FREQ-full.TYPE Full resolution image, but imaging only a subportion of
		     the primary beam. TYPE is as above.


"END-OF-FILE"
