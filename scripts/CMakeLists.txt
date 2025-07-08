# Configure MIRRC files
configure_file(MIRRC.in ${CMAKE_CURRENT_BINARY_DIR}/MIRRC @ONLY)
configure_file(MIRRC.sh.in ${CMAKE_CURRENT_BINARY_DIR}/MIRRC.sh @ONLY)

# Install configured MIRRC files to install prefix base (for build from source)
# Install post_install.sh  & README (for binary install from tar ball)
install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/MIRRC
  ${CMAKE_CURRENT_BINARY_DIR}/MIRRC.sh
  post_install.sh
  README.postinstall.md
  DESTINATION .
)

# Install executable scripts into bin directory
install(PROGRAMS
  mirarch
  mirhelp
  DESTINATION bin
)

install(FILES
  MIRRC.in
  MIRRC.sh.in
  runrmclean.py
  veldop.py
  DESTINATION scripts
)
