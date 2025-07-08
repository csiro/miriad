set(PROG_HEADER_LIST
atfix.h
atlod.h
atmos.h
atrecal.h
atsum.h
atxy.h
blcal.h
blfit.h
calc.h
calred.h.in
clfind.h
clplot.h
clstats.h
fits.h
gpcal.h
gpcomb.h
gpdof.h
gpplt.h
gpscal.h
hermes.h
imdiff.h
imfit.h
imhist.h
imsad.h
imsad2.h
imspec.h
imstat.h
mathd.h
maths.h
mbspect.h
mfcal.h
mfspin.h
psrbl.h
psrfix.h
restor.h
rmclean.h
selfcal.h
sfind.h
tmcvt.h
tvclip.h
tvflag.h
uvaver.h.in
uvdiff.h
uvfit.h
uvgains.h
uvgen.h
uvlist.h
uvpit.h
uvputhd.h
uvsfit.h
uvspec.h.in
uvsplit.h
velplot.h
wbcomm.h
wblod.h
)

# Process each file
foreach(src ${PROG_HEADER_LIST})
    get_filename_component(ext ${src} LAST_EXT)

    if(ext STREQUAL ".in")
        get_filename_component(filename_wle ${src} NAME_WLE)
        message(STATUS "Configuring template ${src} -> ${filename_wle}")
        configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${src}
                       ${CMAKE_CURRENT_BINARY_DIR}/${filename_wle} @ONLY)
    elseif(ext STREQUAL ".h")
        configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${src}
                       ${CMAKE_CURRENT_BINARY_DIR}/${src} COPYONLY)
    endif()
endforeach()

# Add this directory to the include path
include_directories(${CMAKE_CURRENT_BINARY_DIR})