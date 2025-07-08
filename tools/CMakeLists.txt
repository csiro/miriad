# Tools executables
add_executable(ratty ratty.c)
add_executable(intf2c intf2c.c)
# Extract docs from sources
add_executable(doc doc.c)
install(TARGETS ratty intf2c doc DESTINATION bin)


# Preprocessor tools aggregate target
set(preprocessor_tools ratty intf2c doc)
add_custom_target(preprocessor_tools DEPENDS ratty intf2c doc)


# Miriad CLI executable
add_executable(miriad miriad.c)
target_compile_definitions(miriad PRIVATE -DREADLINE)
target_link_libraries(miriad PRIVATE ${READLINE_LIB})

# Format help
add_executable(docfmt docfmt.c)
install(TARGETS miriad docfmt DESTINATION bin)

message(STATUS "Processing tools")
