#!/usr/local/bin/perl

# Perl routines for manipulating Miriad datasets.
#
# To use these in your Perl script, use
#
#  require "$ENV{MIRINC}/miriad.pl"
#
# I am far from a Perl expert, so constributions and
# suggestions on using Perl with Miriad would be appreciated.
#
#				Bob Sault (rsault@atnf.csiro.au)

# Useful constants.

$PI = 3.14159265358979323846;		# Pi.

$CMKS = 299792458.0;			# Speed of light.
$KMKS = 1.380658E-23;			# Boltzmanns constant.
$HMKS = 6.6260755E-34;			# Planck constant.
$HOVERK = 0.04799216;			# Plank/Boltzmann constant.

$AUKM   = 149.597870e6; 		# AU in km.

#************************************************************************
sub keyini{
  foreach $arg (@ARGV){
    my($arg,$value);
    ($var,$value) = ($arg =~ m{(\w+)=(.+)$});
    eval '$'.$var.'="'.$value.'"';
  }
}

#************************************************************************
sub gethd{
  my($var,$fmt,$line);
  ($var,$fmt) = @_;
  $line = "gethd in=$var";
  if("$fmt" ne ""){ $line .= " format=$fmt";}
  $line = `$line`;
  chop $line;
  return $line;
}

#************************************************************************
sub puthd{
  my($var,$value,$fmt,$line);
  ($var,$value,$fmt) = @_;
  $line = "puthd in=$var 'value=\"$value\"";
  if("$fmt" ne ""){$line .= ",$fmt";}
  $line .= "'";
  `$line`;
}
