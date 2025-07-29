include(ExternalProject)

# Directory where PGPLOT will be built locally
set(PGPLOT_PREFIX ${CMAKE_BINARY_DIR}/pgplot)
set(PGPLOT_SRC_DIR ${PGPLOT_PREFIX}/src/pgplot_ext)

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  set(PGPLOT_PLATFORM darwin)
else()
  set(PGPLOT_PLATFORM linux)
endif()

ExternalProject_Add(pgplot_ext
  URL https://sites.astro.caltech.edu/~tjp/pgplot/macos/pgplot531.tar.gz
  URL_HASH SHA256=4bcb09a03b19ebfe30e79435a54e641478099e674f4df8a289fdbde3a827f7bf
  PREFIX ${PGPLOT_PREFIX}
  SOURCE_SUBDIR .
  CONFIGURE_COMMAND
    ${CMAKE_COMMAND} -E chdir ${PGPLOT_SRC_DIR}
    ${PGPLOT_SRC_DIR}/makemake . ${PGPLOT_PLATFORM} gfortran_gcc
  BUILD_COMMAND make
  INSTALL_COMMAND ""
  BUILD_IN_SOURCE 1
  PATCH_COMMAND patch -p1 < ${CMAKE_SOURCE_DIR}/patches/pgplot531.patch
  DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

# Define an imported library target for the built PGPLOT static library
add_library(pgplot STATIC IMPORTED GLOBAL)

# Declare where the built static library lives
set_target_properties(pgplot PROPERTIES
  IMPORTED_LOCATION ${PGPLOT_SRC_DIR}/libpgplot.a
  INTERFACE_INCLUDE_DIRECTORIES ${PGPLOT_SRC_DIR}
)

# Ensure the pgplot target depends on the build step

add_dependencies(pgplot pgplot_ext)

# Install grfont.dat and rgb.txt from the PGPLOT source directory into lib
install(FILES
  ${PGPLOT_SRC_DIR}/grfont.dat
  ${PGPLOT_SRC_DIR}/rgb.txt
  DESTINATION lib
)

install(PROGRAMS
  ${PGPLOT_SRC_DIR}/pgxwin_server
  DESTINATION bin
)
