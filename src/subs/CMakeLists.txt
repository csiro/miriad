# List of source files for subs directory
set(SUBS_SOURCES
align.for
amphase.for
angles.for
antennas.for
ari.for
assert.for
atjones.for
axistype.for
basant.for
bessel.for
boxes.for
bsrch.for
btype.for
bug.c
bugwrap.f2c
callinp.for
calstoke.for
cgpgsubs.for
cgsubs.for
cnvl.for
co.for
convl.for
cosubs.for
ctrl.for
defsmodl.for
deghms.for
dio.c
ephem.for
expun.for
fftsubs.for
fitsio.for
fndaxnum.for
fullname.for
gamma.for
gaupar.for
getbeam.for
getfreq.for
getxy.for
grid.for
hann.for
hdtab.for
headcopy.for
headio.c
hio.c
hio3.f2c
hisinput.for
hsort.for
iface.f2c
imminmax.for
inc.for
interface.c
intpio.for
j1xbyx.for
julday.for
key.for
keyc.c
keyline.for
lagwt.for
linetype.for
log.for
lsearch.for
lsf.for
lspoly.for
lsqfit.for
lsqu.for
lsqu1.for
mapper.for
maskio.c
match.for
math.for
mc.for
medfit.for
median.for
mem.for
mm.f2c
model.for
modp.for
mostab.for
mp.for
nearest.for
nextpow2.for
nllsqu.for
noise.for
nswc.for
numbpg.for
obspar.for
ofm.for
options.for
oscalls.f2c
pack.c
packwrap.f2c
pb.for
pcvt.for
pghline.for
pkfit.for
plane.for
planet.for
plotone.for
plproc.for
pols.for
poly.for
powell.for
prime.for
promptf.for
refract.for
rest.for
rtfmt.for
scrio.c
select.for
sfetra.for
shadowed.for
si.for
sort.for
sortidx.for
spline.for
strf.for
string.for
tabflux.for
tangle.for
tcpio.f2c
tin.for
title.for
trans.for
trnio.for
tv.for
tvsubs.for
txtio.for
utilities.for
uvdat.for
uvfitsubs.for
uvgetbl.for
uvgn.for
uvio.c
uvsubs.for
var.for
velocity.for
versan.for
wpfit.for
wrap.f2c
xyio.c
xyzio.c
xyziowrap.f2c
zed.for
zeebin.for)


message(STATUS "Preprocessing src/subs")
add_custom_target(subs_preprocessing DEPENDS preprocessor_tools wcslib_ext rpfits_ext)

set(SUBS_FORTRAN_SOURCES)
set(SUBS_OTHER_SOURCES)

# Need to depend on header files explicitly to force rebuild when they change
# Cmake only does this automatically for C/C++
file(GLOB HEADERS "${CMAKE_BINARY_DIR}/inc/*.h" "${CMAKE_BINARY_DIR}/inc/subs/*.h")

foreach(filename ${SUBS_SOURCES})
  get_filename_component(progname ${filename} NAME_WE)
  get_filename_component(ext ${filename} EXT)

  if(ext STREQUAL ".for")
    # Preprocess .for -> .f
    set(preprocessed_f ${CMAKE_CURRENT_BINARY_DIR}/${progname}.f)
    add_custom_command(
      OUTPUT ${preprocessed_f}
      COMMAND $<TARGET_FILE:ratty> ${ratty_flags} ${ratty_include_flags}
              ${CMAKE_CURRENT_SOURCE_DIR}/${filename}
              ${preprocessed_f}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${filename} ${HEADERS}
    )
    list(APPEND SUBS_FORTRAN_SOURCES ${preprocessed_f})

  elseif(ext STREQUAL ".f2c")
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${progname}.c
      COMMAND $<TARGET_FILE:intf2c> ${f2c_flags}
              ${CMAKE_CURRENT_SOURCE_DIR}/${filename}
              ${CMAKE_CURRENT_BINARY_DIR}/${progname}.c
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${filename} ${HEADERS}
    )
    list(APPEND SUBS_OTHER_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/${progname}.c)

  else()
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${filename}
      COMMAND ${CMAKE_COMMAND} -E copy
              ${CMAKE_CURRENT_SOURCE_DIR}/${filename}
              ${CMAKE_CURRENT_BINARY_DIR}/${filename}
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${filename}
    )
    list(APPEND SUBS_OTHER_SOURCES ${CMAKE_CURRENT_BINARY_DIR}/${filename})
  endif()
endforeach()

# Build mir object library from compiled Fortran and other objects
add_library(mir_obj OBJECT ${SUBS_FORTRAN_SOURCES} ${SUBS_OTHER_SOURCES})
add_dependencies(mir_obj subs_preprocessing)

target_include_directories(mir_obj PRIVATE
    ${CMAKE_BINARY_DIR}/inc
    ${CMAKE_BINARY_DIR}/inc/subs
)

# Build final mir library from objects
add_library(mir $<TARGET_OBJECTS:mir_obj>)
target_link_libraries(mir PRIVATE ${RPFITS_LIB} ${WCS_LIB} ${READLINE_LIB} ${PGPLOT_LIB} linpack)