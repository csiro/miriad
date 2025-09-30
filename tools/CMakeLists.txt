# Tools executables
add_executable(ratty ratty.c)
add_executable(intf2c intf2c.c)
# Extract docs from sources
add_executable(doc doc.c)
# Command line editing
add_executable(ercmd ercmd.c)
install(TARGETS ratty intf2c doc ercmd DESTINATION bin)


# Preprocessor tools aggregate target
set(preprocessor_tools ratty intf2c doc)
add_custom_target(preprocessor_tools DEPENDS ratty intf2c doc)

# Get Git version
execute_process(
  COMMAND git describe --tags --dirty --always
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE
  ERROR_QUIET
)

if(NOT GIT_VERSION)
  set(GIT_VERSION "unknown")
endif()

# Configure version header
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/version.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/version.h
  @ONLY
)

# Miriad CLI executable
add_executable(miriad miriad.c)
target_include_directories(miriad PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
target_compile_definitions(miriad PRIVATE -DREADLINE)
target_link_libraries(miriad PRIVATE ${READLINE_LIB})

# Format help
add_executable(docfmt docfmt.c)
install(TARGETS miriad docfmt DESTINATION bin)

message(STATUS "Processing tools")
