include(ExternalProject)

set(WCSLIB_PREFIX ${CMAKE_BINARY_DIR}/wcslib)
set(WCSLIB_SRC_DIR ${WCSLIB_PREFIX}/src/wcslib_ext)

ExternalProject_Add(wcslib_ext
  URL https://www.atnf.csiro.au/computing/software/wcs/WCS/wcslib-8.4.tar.bz2
  URL_HASH SHA256=960b844426d14a8b53cdeed78258aa9288cded99a7732c0667c64fa6a50126dc
  PREFIX ${WCSLIB_PREFIX}
  CONFIGURE_COMMAND ./configure --prefix=${WCSLIB_PREFIX}
  BUILD_COMMAND make
  INSTALL_COMMAND make install
  BUILD_IN_SOURCE 1
  DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

# Define imported target for the WCSLIB static library
set(WCS_LIB_PATH ${WCSLIB_PREFIX}/lib/libwcs.a NO_DEFAULT_PATH REQUIRED)

add_library(wcslib STATIC IMPORTED GLOBAL)
add_dependencies(wcslib wcslib_ext)
set_target_properties(wcslib PROPERTIES
  IMPORTED_LOCATION ${WCS_LIB_PATH}
)