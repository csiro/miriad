set(HEADER_LIST
    maxdim.h.in
    maxdimc.h.in
    maxnax.h
    mem.h
    mirconst.h
    miriad.h
    miriad.pl
    tmpdim.h
)

# Process each file
set(INC_OUTPUT_DIR ${CMAKE_BINARY_DIR}/inc)
foreach(src ${HEADER_LIST})
    get_filename_component(ext ${src} LAST_EXT)

    if(ext STREQUAL ".in")
        get_filename_component(filename_wle ${src} NAME_WLE)
        message(STATUS "Configuring template ${src} -> ${filename_wle}")
        configure_file(${src} ${INC_OUTPUT_DIR}/${filename_wle} @ONLY)
    elseif(ext STREQUAL ".h" OR ext STREQUAL ".inc")
        message(STATUS "Copying ${src}")
        configure_file(${src} ${INC_OUTPUT_DIR}/${src} COPYONLY)
    endif()
endforeach()

# Add this directory to the include path
include_directories(${INC_OUTPUT_DIR})