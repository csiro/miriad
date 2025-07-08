# List of header files for this directory
set(SUBS_HEADER_LIST
boxes.h
co.h
ctrl.h
fitsio.h
hdtab.h
hio.h
intpio.h
io.h
key.h
log.h
mapper.h
mc.h
model.h
modp.h
mostab.h
mp.h
obspar.h
ofm.h
pb.h
pcvt.h
plproc.h
rest.h
select.h
sysdep.h
tabflux.h
tin.h
trnio.h
tv.h
uvdat.h
uvgn.h)

# Process each file
foreach(src ${SUBS_HEADER_LIST})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/${src}
                ${CMAKE_CURRENT_BINARY_DIR}/${src} COPYONLY)
endforeach()

# Add this directory to the include path
include_directories(${CMAKE_CURRENT_BINARY_DIR})