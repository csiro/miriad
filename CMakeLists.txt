cmake_minimum_required(VERSION 3.19)
# Get today's date as YYYY.MM.DD
string(TIMESTAMP MIRIAD_DATE "%Y.%m.%d")

# Set project version dynamically
project(miriad VERSION "${MIRIAD_DATE}" LANGUAGES C Fortran)

set(CMAKE_C_STANDARD 99)
set(CMAKE_Fortran_STANDARD 90)

# Option for large-N configuration
option(LARGEN "Enable large-N Miriad configuration" OFF)
if(LARGEN)
  set(MIRIAD_VARIANT "-LARGEN")
else()
  set(MIRIAD_VARIANT "")
endif()

message(STATUS "Building Miriad version ${PROJECT_VERSION}${MIRIAD_VARIANT}")


# Configuration of Miriad for large N arrays
if(LARGEN)
  set(MAXBUF 1024*1024*128)
  set(MAXANT 256)
  set(MAXCHAN 4096)
  set(MAXAVER 20000000)
  set(MAXWIN 16)
  set(MAXWIDE 18)
  set(MAXSRC 36)
else()
  set(MAXBUF 1024*1024*32)
  set(MAXANT 64)
  set(MAXCHAN 70000)
  set(MAXAVER 2000000)
  set(MAXWIN 48)
  set(MAXWIDE 18)
  set(MAXSRC 1024)
endif()
set(MIRROOT ${CMAKE_INSTALL_PREFIX})

# Preprocessing flags
set (ratty_flags
    -s gfortran
    -p 'integer\(kind=8\)'
)
set (ratty_include_flags
    -I ${CMAKE_BINARY_DIR}/inc
    -I ${CMAKE_BINARY_DIR}/inc/prog
    -I ${CMAKE_BINARY_DIR}/inc/subs
    -I ${CMAKE_BINARY_DIR}/wcslib/include
    -I ${CMAKE_BINARY_DIR}/rpfits/include
)
set (f2c_flags
    -s linux
    -p long
)

# Compiler flags
# add_compile_options($<$<COMPILE_LANGUAGE:C>:-DDEBUG>)
add_compile_options($<$<COMPILE_LANGUAGE:C>:-Wno-deprecated-non-prototype>)
add_compile_options($<$<COMPILE_LANGUAGE:C>:-Wno-format>)
add_compile_options($<$<COMPILE_LANGUAGE:C>:-Wno-sometimes-uninitialized>)
add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-fno-automatic>)
add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-fallow-argument-mismatch>)
add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-w>)

# Dependencies
find_package(X11 REQUIRED)
find_library(PNG_LIB png REQUIRED)
find_library(READLINE_LIB NAMES readline REQUIRED)
#find_library(RPFITS_LIB rpfits REQUIRED)
#find_library(WCS_LIB wcs REQUIRED)

# Subdirectories
add_subdirectory(inc)
add_subdirectory(inc/prog)
add_subdirectory(inc/subs)
add_subdirectory(tools)
add_subdirectory(pgplot)
add_subdirectory(rpfits)
add_subdirectory(wcslib)
add_subdirectory(src)
add_subdirectory(specdoc)
add_subdirectory(scripts)

# Optional documentation build
option(BUILD_GUIDES "Build user and programmer guides" OFF)
if(BUILD_GUIDES)
  add_subdirectory(guides)
endif()

# Install cat directory quietly
set(CMAKE_INSTALL_MESSAGE LAZY)
install(DIRECTORY cat DESTINATION .)

# Install LICENSE & README
install(FILES LICENSE README.md DESTINATION .)

# CPack configuration
include(InstallRequiredSystemLibraries)

set(CPACK_PACKAGE_NAME "Miriad")
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set(CPACK_PACKAGE_CONTACT "Miriad <miriad@atnf.csiro.au>")
set(CPACK_PACKAGE_VENDOR "CSIRO/ATNF")
set(CPACK_GENERATOR "TGZ")
set(CPACK_PACKAGING_INSTALL_PREFIX "/miriad")
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}${MIRIAD_VARIANT}-${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR}")
set(CPACK_SOURCE_IGNORE_FILES "/build/;/.git/;~$;${CPACK_SOURCE_IGNORE_FILES}")

include(CPack)
