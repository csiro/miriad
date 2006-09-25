set terminal postscript eps
set output "immerge.ps"
set yrange [-0.05:1.05]
set nozeroaxis
set samples 500
nu = 0.2
fwhm1 = 46./nu/sqrt(4.0*log(2.0))
fwhm2 = 14*fwhm1

f1(lambda) = exp(-(lambda/fwhm1)**2)
f2(lambda) = exp(-(lambda/fwhm2)**2)

set xlabel "Spatial Frequency (wavelengths)"
set ylabel "Amplitude"
plot [0:3500] f1(x) title "Parkes" with lines 2, f2(x)-f1(x) title "Tapered ATCA" with lines 5, f2(x) title "Merged" with lines 1


