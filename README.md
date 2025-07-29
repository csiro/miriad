# Miriad

Miriad is a radio astronomy data reduction package designed for calibration, imaging, and analysis of interferometric observations.

## Features

- Fortran-based scientific core
- Modular tools and tasks
- X11, PS and PNG-based plotting support
- Cross-platform CMake build system
- Compatible with macOS (Apple Silicon) and Linux

## Building from Source

To build Miriad using CMake:

```bash
# For MacOS:
# if you have XQuartz installed (brew install xquartz), which you probably should, you can omit libx11
brew install gcc libpng readline libx11

# For linux:
sudo apt install gfortran libx11-dev libpng-dev libreadline-dev

git clone --branch cmake --depth 1  https://github.com/csiro-internal/miriad.git
cd miriad
# replace <install_path> with the path to your new Miriad installation (e.g., $HOME/miriad or /opt/miriad)
cmake -S . -B build -DCMAKE_INSTALL_PREFIX=<install_path>
cmake --build build --target install
```
You can change the CMAKE_INSTALL_PREFIX to where you would like to install miriad.

A patched version of PGPLOT is built as part of the Miriad build (as well as RPFITS and WCSLIB) because standard packaged versions are either not available or don't provide what Miriad needs.

## Optional: Enable Large N mode
To build with increased buffer sizes and internal limits, used for processing datasets from arrays with more than 64 antennas, pass the LARGEN=ON option to CMake:

```bash
cmake -S . -B build -DCMAKE_INSTALL_PREFIX=<install_path> -DLARGEN=ON
cmake --build build --target install
```
Note that this will reduce a few other internal limits to compensate, so only use this for large N arrays.

## Installation from pre-built binaries

To install Miriad from a tar file with the binary installation, just download the file for your architecture (Linux or MacOS) from the latest Release and extract in your preferred installation location.
Then, from the installation directory, run
```bash
. ./post_install.sh
```
to prepare the MIRRC.sh file and, if needed, allow the binaries to be executed.

## Setting up your environment after installation
```bash
. <install_path>/MIRRC.sh
```

## Documentation
- Documentation for tasks and some keywords is accessible via `mirhelp task` from the command line or just `help task` from the miriad shell
- The User manual and other documentation can be found here: https://www.atnf.csiro.au/computing/software/miriad/ (noting that some information on that page is not up to date)
- Miriad images can be inspected interactively using the CARTA viewer (https://cartavis.org)

## License
Miriad is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Miriad is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

To get a copy of the GNU General Public License, write to the
Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
