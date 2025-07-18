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
uvgn.h
version.h.in)

# Prepare for versioning
execute_process(
  COMMAND git describe --tags --dirty
  OUTPUT_VARIABLE GIT_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(
  COMMAND git log -1 --format=%cd --date=short
  OUTPUT_VARIABLE GIT_DATE
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Process each file
foreach(src ${SUBS_HEADER_LIST})
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