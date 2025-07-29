# Set variables
set(INC "${CMAKE_CURRENT_SOURCE_DIR}/../user")
set(DATE_STRING "")
execute_process(COMMAND date "+%d %h %Y" OUTPUT_VARIABLE DATE_STRING OUTPUT_STRIP_TRAILING_WHITESPACE)

# LaTeX build commands
find_program(PDFLATEX latex REQUIRED)
find_program(MAKEINDEX makeindex REQUIRED)
find_program(EPSTOPDF epstopdf REQUIRED)

add_custom_command(
  OUTPUT progguide.pdf
  DEPENDS progguide.tex
  COMMAND ${CMAKE_COMMAND} -E echo "Generating title page"
  COMMAND ${INC}/Buildtitlepage Programmer
  COMMAND ${EPSTOPDF} titlepage.eps titlepage.pdf
  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${INC}/latexdefs.inc ${INC}/image.inc ${INC}/uvvars.inc ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E echo "Running LaTeX pass 1 and 2"
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/progguide.tex > /dev/null
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/progguide.tex > /dev/null
  COMMAND ${MAKEINDEX} progguide
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/progguide.tex > /dev/null
  COMMENT "Building the Miriad Programmer Guide"
)

add_custom_target(progguide ALL
  DEPENDS progguide.pdf
)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/progguide.pdf
        DESTINATION ${CMAKE_INSTALL_PREFIX}/guides)
