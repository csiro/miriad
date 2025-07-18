# List of source files for prog directory
set(PROG_SOURCES
addhis.for
antconv.for
antpol.for
arrayplt.for
arrfix.for
atfix.for
atlod.for
atmerge.for
atmos.for
atrecal.for
atrtfix.for
atscfix.for
atsum.for
attsys.for
atwvr.for
atxy.for
avmaths.for
blcal.for
blfit.for
blflag.for
boxspec.for
calc.for
calplot.for
calred.for
cclean.for
cgcurs.for
cgdisp.for
cginit.for
cgrego.for
cgslice.for
cgspec.for
clean.for
clfind.for
closure.for
clplot.for
clstats.for
colden.for
conterr.for
contsen.for
contsub.for
convol.for
copyhd.for
cotra.for
delhd.for
demos.for
deproject.for
early.for
ellint.for
ellplt.for
fakeotf.for
fft.for
fgflag.for
fits.for
gaufit.for
gethd.for
gpaver.for
gpboot.for
gpbreak.for
gpcal.for
gpcomb.for
gpcopy.for
gpdof.for
gpedit.for
gperror.for
gplist.for
gpnorm.for
gpplt.for
gpscal.for
gpshift.for
hanning.for
hermes.for
histo.for
im2uv.for
imbin.for
imblr.for
imcat.for
imcfn.for
imcmp.for
imcomb.for
imdiff.for
imfit.for
imframe.for
imgen.for
imheq.for
imhist.for
imhol.for
imlist.for
imload.for
immask.for
immedian.for
immerge.for
imom.for
impol.for
impoly.for
impos.for
imrm.for
imsad.for
imsharp.for
imspec.for
imspect.for
imstat.for
imstore.for
imsub.for
imtab.for
invert.for
itemize.for
lagflg.for
linmos.for
mathd.for
maths.for
maxen.for
maxfit.for
mbspect.for
mfboot.for
mfcal.for
mfclean.for
mfflat.for
mfplan.for
mfspin.for
minmax.for
mirmax.for
moment.for
mopfix.for
moscsdi.for
mosgen.for
moslst.for
mosmem.for
mospsf.for
mossdi.for
mossen.for
mostess.for
obsrms.for
odnh3.for
offaxis.for
offpol.for
opplt.for
paraplot.for
pbplot.for
pcsub.for
pgflag.for
planets.for
plboot.for
plgeom.for
plplt.for
pmosmem.for
pops.for
ppmtomir.for
prthd.for
prthis.for
psrbl.for
psrfix.for
psrplt.for
puthd.for
qvack.for
regrid.for
reorder.for
restor.for
rmclean.for
rpfread.for
rpgen.for
selfcal.for
sfind.for
shifty.for
sigest.for
smooth.for
specshift.for
telepar.for
tmcal.for
tmcvt.for
tmgen.for
tpgains.for
transfix.for
tvall.for
tvclip.for
tvcycle.for
tvdisp.for
tvflag.for
tvinit.for
tvset.for
uvacflag.for
uvaflag.for
uvamp.for
uvaver.for
uvcat.for
uvclip.for
uvdflag.for
uvdiff.for
uvdump.for
uvedit.for
uvfft.for
uvfit.for
uvflag.for
uvflux.for
uvfmeas.for
uvfstats.for
uvgains.for
uvgen.for
uvglue.for
uvimage.for
uvindex.for
uvlin.for
uvlist.for
uvmodel.for
uvpflag.for
uvpit.for
uvplanet.for
uvplt.for
uvputhd.for
uvratio.for
uvredo.for
uvrms.for
uvrot.for
uvsector.for
uvsfit.for
uvspec.for
uvsplit.for
uvstat.for
uvsub.for
uvswap.for
uvtcor.for
uvtplt.for
uvwide.for
uvzflag.for
varlist.for
varmerge.for
varplt.for
vblank.for
velcolor.for
velfit.for
velimage.for
velmodel.for
velplot.for
velsw.for
wblod.for
wbplt.for
xyphase.for
zapchan.for
zeeeta.for
zeefake.for
zeemap.for
zeesim.for
zeestat.for)

message(STATUS "Preprocessing src/prog")
add_custom_target(prog_preprocessing DEPENDS preprocessor_tools wcslib_ext rpfits_ext)

set(DOC_FILES)

set(MIRIAD_TASKS)

# Need to depend on header files explicitly to force rebuild when they change
# Cmake only does this automatically for C/C++
file(GLOB HEADERS "${CMAKE_BINARY_DIR}/inc/*.h" "${CMAKE_BINARY_DIR}/inc/prog/*.h")

foreach(filename ${PROG_SOURCES})
    get_filename_component(progname ${filename} NAME_WE)

    # Preprocess .for -> .f
    set(preprocessed_f ${CMAKE_CURRENT_BINARY_DIR}/${progname}.f)
    add_custom_command(
        OUTPUT ${preprocessed_f}
        COMMAND $<TARGET_FILE:ratty> ${ratty_flags} ${ratty_include_flags}
                ${CMAKE_CURRENT_SOURCE_DIR}/${filename}
                ${preprocessed_f}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${filename} ${HEADERS}
        COMMENT "Preprocessing ${filename}"
    )

    # If program is 'clean', rename CMake target to 'clean_prog'
    if(progname STREQUAL "clean")
        set(target_name "clean_prog")
    else()
        set(target_name ${progname})
    endif()

    list(APPEND MIRIAD_TASKS ${target_name})

    # Build executable from .f
    add_executable(${target_name} ${preprocessed_f})

    # Ensure executable depends on preprocessor tools and preprocessed .f
    add_dependencies(${target_name} prog_preprocessing)

    # Includes and linked libraries
    target_link_libraries(${target_name} PRIVATE
        mir linpack pgplot rpfits wcslib
        ${READLINE_LIB}
        ${X11_LIBRARIES}
        ${PNG_LIB}
    )

    # Set output binary name to original program name
    set_target_properties(${target_name} PROPERTIES OUTPUT_NAME ${progname})

    # Extract documentation using 'doc' tool
    set(doc_output ${CMAKE_BINARY_DIR}/doc/${progname}.doc)
    add_custom_command(
        OUTPUT ${doc_output}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/doc
        COMMAND $<TARGET_FILE:doc> -f ${CMAKE_CURRENT_SOURCE_DIR}/${filename} > ${doc_output}
        DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${filename} doc
        COMMENT "Extracting documentation from ${filename}"
    )
    list(APPEND DOC_FILES ${doc_output})
endforeach()

add_custom_target(prog_docs ALL DEPENDS ${DOC_FILES})

install(TARGETS ${MIRIAD_TASKS} RUNTIME DESTINATION bin)

install(DIRECTORY ${CMAKE_BINARY_DIR}/doc/ DESTINATION doc)
