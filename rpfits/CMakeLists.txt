include(ExternalProject)

set(RPFITS_PREFIX ${CMAKE_BINARY_DIR}/rpfits)
set(RPFITS_SRC_DIR ${RPFITS_PREFIX}/src/rpfits_ext)
# Define platform-specific build directory
string(TOLOWER "${CMAKE_SYSTEM_NAME}" RPFITS_SYSTEM_NAME)
set(RPFITS_ARCH "${CMAKE_SYSTEM_PROCESSOR}")

if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
  execute_process(COMMAND xcrun --show-sdk-path OUTPUT_VARIABLE MACOSX_SDK_PATH OUTPUT_STRIP_TRAILING_WHITESPACE)
  set(RPFITS_ENV
    SDKROOT=${MACOSX_SDK_PATH}
    CFLAGS=-isysroot\ ${MACOSX_SDK_PATH}
  )
else()
  set(RPFITS_ENV "")
endif()

# Adjust for RPFITS naming expectations
if(RPFITS_SYSTEM_NAME STREQUAL "linux" AND RPFITS_ARCH STREQUAL "x86_64")
  set(RPFITS_BUILD_DIR ${RPFITS_SRC_DIR}/linux64)
else()
  set(RPFITS_BUILD_DIR ${RPFITS_SRC_DIR}/${RPFITS_SYSTEM_NAME}_${RPFITS_ARCH})
endif()

ExternalProject_Add(rpfits_ext
  URL ftp://ftp.atnf.csiro.au/pub/software/rpfits/rpfits-2.27.tar.gz
  URL_HASH SHA256=9726153390c250a5995faaad082087ce31e1e6689ebce511ffd81686059eb752
  PREFIX ${RPFITS_PREFIX}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ${CMAKE_COMMAND} -E env ${RPFITS_ENV}
                ${CMAKE_COMMAND} -E chdir ${RPFITS_BUILD_DIR}
                $(MAKE) -j1 CC=${CMAKE_C_COMPILER} FC=${CMAKE_Fortran_COMPILER} MACHTYPE=x
  INSTALL_COMMAND ${CMAKE_COMMAND} -E chdir ${RPFITS_BUILD_DIR} make install PREFIX=${RPFITS_PREFIX} MACHTYPE=x
  INSTALL_DIR ${RPFITS_PREFIX}
  DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

# Imported RPFITS static library
set(RPFITS_LIB_PATH ${RPFITS_PREFIX}/lib/librpfits.a)

add_library(rpfits STATIC IMPORTED GLOBAL)
add_dependencies(rpfits rpfits_ext)
set_target_properties(rpfits PROPERTIES
  IMPORTED_LOCATION ${RPFITS_LIB_PATH}
)