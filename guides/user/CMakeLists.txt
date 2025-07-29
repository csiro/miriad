set(DATE_STRING "")
execute_process(COMMAND date "+%d %h %Y" OUTPUT_VARIABLE DATE_STRING OUTPUT_STRIP_TRAILING_WHITESPACE)

file(GLOB MIRFIGS "*.fig")
file(GLOB MIRGS "*.g")
file(GLOB INCS "*.inc")

find_program(MAKEINDEX makeindex REQUIRED)
find_program(PDFLATEX pdflatex REQUIRED)
find_program(GNUPLOT gnuplot REQUIRED)
find_program(FIG2DEV fig2dev REQUIRED)
find_program(EPSTOPDF epstopdf REQUIRED)

# Convert .fig and .g to .pdf
set(PDF_OUTPUTS "")
foreach(fig ${MIRFIGS})
  get_filename_component(name ${fig} NAME_WE)
  list(APPEND PDF_OUTPUTS "${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf")
  add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf"
    DEPENDS ${fig}
    COMMAND ${FIG2DEV} -L pdf -p dummy_arg ${fig} ${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf
    COMMENT "Generating ${name}.pdf from ${fig}"
  )
endforeach()

foreach(gplot ${MIRGS})
  get_filename_component(name ${gplot} NAME_WE)
  list(APPEND PDF_OUTPUTS "${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf")
  add_custom_command(
    OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${name}.pdf"
    DEPENDS ${gplot}
    COMMAND ${GNUPLOT} ${gplot}
    COMMENT "Generating ${name}.pdf from ${gplot}"
  )
endforeach()

add_custom_target(figures ALL DEPENDS ${PDF_OUTPUTS})

# Build userguide.pdf
add_custom_command(
  OUTPUT userguide.pdf
  DEPENDS userguide.tex ${INCS} ${PDF_OUTPUTS}
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/Buildtitlepage User
  COMMAND ${EPSTOPDF} titlepage.eps titlepage.pdf
  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${INCS} ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/colours.pdf ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/userguide.tex > /dev/null
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/userguide.tex > /dev/null
  COMMAND ${MAKEINDEX} userguide
  COMMAND ${PDFLATEX} ${CMAKE_CURRENT_SOURCE_DIR}/userguide.tex > /dev/null
  COMMENT "Building userguide.pdf"
)

add_custom_target(userguide ALL
  DEPENDS userguide.pdf
)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/userguide.pdf
        DESTINATION ${CMAKE_INSTALL_PREFIX}/guides)
