#-----------------------------------------------------------------------------
# Process this file with autoconf-2.53 or later to produce a configure script.
#-----------------------------------------------------------------------------
# Author: Mark Calabretta, Australia Telescope National Facility
# $Id: configure.ac,v 1.9 2010/07/07 02:19:22 cal103 Exp $
#-----------------------------------------------------------------------------

AC_INIT([Miriad], [ATNF2.0], [miriad@atnf.csiro.au], [miriad-ATNF2.0])
AC_PREREQ([2.53])
AC_REVISION([$Revision: 1.9 $])

AC_CONFIG_SRCDIR([tools/miriad.c])
AC_CONFIG_AUX_DIR([config])

# Determine the Miriad root directory.
MIRROOT=`pwd`
AC_SUBST([MIRROOT])

# Determine the Miriad architecture name.
MIRARCH=`$MIRROOT/scripts/mirarch`
AC_SUBST([MIRARCH])


# Look for a Fortran compiler.
AC_MSG_NOTICE([

Looking for the Fortran compiler...])

AC_PROG_F77([g77 f77 gfortran ifort xlf frt pgf77 fl32 af77 fort77 f90 xlf90 \
             pgf90 epcf90 f95 fort xlf95 lf95 g95])

if test "x$F77" = x ; then
  AC_MSG_ERROR([
    -------------------------------------------------------
    Fortran compiler not found.
    A Fortran compiler is required to build Miriad.

    ERROR: Miriad configuration failure.
    -------------------------------------------------------], [1])
fi

if test "x$ac_cv_f77_compiler_gnu" = xyes ; then
  # Beware of gfortran!
  if test "x$F77" = xg77 -o "x$F77" = xf77 ; then
    FCSTD="-ff90-intrinsics-hide -Wno-globals"
  fi
fi

AC_SUBST([FCSTD])


# Look for an ANSI C compiler.
AC_MSG_NOTICE([

Looking for an ANSI C compiler...])

AC_PROG_CC
AC_PROG_CC_STDC
AC_C_CONST
AC_TYPE_SIZE_T
if test "x$ac_cv_prog_cc_stdc" = xno -o \
        "x$ac_cv_c_const"      = xno -o \
        "x$ac_cv_type_size_t"  = xno ; then
  AC_MSG_ERROR([
    -------------------------------------------------------
    An ANSI standard C library is required to build Miriad.

    ERROR: Miriad configuration failure.
    -------------------------------------------------------], [1])

fi

# Check for ANSI C headers.
AC_MSG_NOTICE([

Checking ANSI C header files...])

AC_HEADER_STDC
AC_CHECK_HEADERS([errno.h limits.h math.h stdio.h stdlib.h string.h])
if test "x$ac_cv_header_stdc" = xno ; then
  AC_MSG_ERROR([
    -------------------------------------------------------------------
    An ANSI standard C library is required to build Miriad.  One of the
    ANSI C header files it requires is missing or unusable.

    ERROR: Miriad configuration failure.
    -------------------------------------------------------------------], [1])
fi


# Check for other standard header files.
AC_MSG_NOTICE([

Checking for other standard unix header files...])

AC_CHECK_HEADERS([unistd.h])


# Checks for ANSI C library functions (suggested by autoscan).
AC_MSG_NOTICE([

Checking ANSI C library functions...])

AC_CHECK_LIB([m], [floor])
AC_FUNC_CLOSEDIR_VOID
AC_HEADER_MAJOR
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_FUNC_STAT
AC_FUNC_STRTOD
AC_CHECK_FUNCS([bzero floor memset mkdir rmdir strchr strerror])

# Utilities.
AC_MSG_NOTICE([

Checking for miscellaneous utilities...])

AC_PATH_PROG([AR],    [ar])
AC_PATH_PROGS([MANDB], [mandb catman])
AC_PATH_PROG([STRIP], [strip])
AC_PROG_LN_S
AC_PROG_RANLIB
if test "x$MANDB" = x ; then MANDB=: ; fi


# Extra places to look for third-party libraries.
AC_MSG_NOTICE([

Looking for directories where third-party libraries may be found...])

EXTDIRS=

AC_ARG_WITH([pgplot], [AS_HELP_STRING([--with-pgplot=DIR],
            [directory containing pgplot library])], [])
if test "x$with_pgplot" != x ; then
  EXTDIRS="$EXTDIRS $with_pgplot"
fi

AC_ARG_WITH([rpfits], [AS_HELP_STRING([--with-rpfits=DIR],
            [directory containing rpfits library])], [])
if test "x$with_rpfits" != x ; then
  EXTDIRS="$EXTDIRS $with_rpfits"
fi

AC_ARG_WITH([wcslib], [AS_HELP_STRING([--with-wcslib=DIR],
            [directory containing wcslib library])], [])
if test "x$with_wcslib" != x ; then
  EXTDIRS="$EXTDIRS $with_wcslib"
fi

AC_PATH_X
EXTDIRS="$EXTDIRS              \
         $MIRROOT/$MIRARCH/lib \
         /usr/local/pgplot     \
         /usr/local/rpfits     \
         /usr/local/wcslib     \
         /usr/local/gnu/lib    \
         /usr/local/lib        \
         /local/pgplot         \
         /local/gnu/lib        \
         /local/lib            \
         /opt/lib              \
         /opt/local/lib        \
         /sw/lib               \
         /sw/lib/pgplot        \
         /opt/sw/lib           \
         /opt/sw/lib/pgplot    \
         /usr/lib              \
         /lib                  \
         /opt/SUNWspro/lib     \
         /usr/ucblib"

DIRS=
for EXTDIR in $EXTDIRS ; do
  AC_CHECK_FILE([$EXTDIR], [DIRS="$DIRS $EXTDIR"])
done
EXTDIRS=$DIRS


# Set -L options in LDFLAGS.
AC_MSG_NOTICE([

Setting -L flags that may be needed by the linker...])

# Shared library suffix.
AC_MSG_CHECKING([shared library suffix])
SO=so
if test -f /usr/lib/libc.dylib ; then SO=dylib ; fi
AC_MSG_RESULT([$SO])

EXTLIBS=" pgplot png z F77 M77 sunmath g2c m "
for EXTDIR in $EXTDIRS ; do
  DIR=
  for EXTLIB in $EXTLIBS ; do
    AC_CHECK_FILES([$EXTDIR/lib$EXTLIB.$SO $EXTDIR/lib$EXTLIB.a],
                   [DIR=$EXTDIR
                    EXTLIBS=`echo "$EXTLIBS" | sed -e "s/ $EXTLIB / /"`])
  done

  if test "x$DIR" != x -a "x$DIR" != x/usr/lib -a "x$DIR" != x/lib
  then
    LDFLAGS="$LDFLAGS -L$DIR"
  fi
done

LDFLAGS=`echo $LDFLAGS`
AC_MSG_NOTICE([LDFLAGS set to $LDFLAGS])


# Extra platform-specific libraries.
AC_MSG_NOTICE([

Looking for platform-specific libraries...])

XTRALIBS=$LIBS
# Darwin.
#AC_CHECK_LIB([SystemStubs],
#             [_fprintf$LDBLStub], [XTRALIBS="-lSystemStubs $XTRALIBS"],
#             [], [$XTRALIBS])

# Solaris.
AC_CHECK_LIB([nsl],     [gethostent], [XTRALIBS="-lnsl $XTRALIBS"],
             [], [$XTRALIBS])
AC_CHECK_LIB([socket],  [socket],     [XTRALIBS="-lsocket $XTRALIBS"],
             [], [$XTRALIBS])
AC_CHECK_LIB([sunmath], [cosd],       [XTRALIBS="-lsunmath $XTRALIBS"],
             [], [$XTRALIBS])
AC_CHECK_LIB([M77],     [iand_],      [XTRALIBS="-lM77 $XTRALIBS"],
             [], [$XTRALIBS])
AC_CHECK_LIB([F77],     [f77_init],   [XTRALIBS="-lF77 $XTRALIBS"],
             [], [$XTRALIBS])

if test "x$ac_cv_f77_compiler_gnu" = xno ; then
  # Library compiled with a GNU compiler but linked with something else.
  AC_CHECK_LIB([g2c],   [gerror_],   [XTRALIBS="-lg2c $XTRALIBS"],
               [], [$XTRALIBS])
fi

XTRALIBS=`echo $XTRALIBS`
AC_SUBST([XTRALIBS])


# X11 includes and libraries.
if test "x$no_x" = x ; then
  X11INCD=$ac_x_includes
  if test "x$ac_x_libraries" = x ; then
    # E.g. for Gentoo or Redhat which installs X11 libraries in /usr/lib.
    X11LIB="-lX11"
    X11LIBS="-lXaw -lXmu -lXext -lXt -lX11"
  else
    X11LIB="-L$ac_x_libraries -lX11"
    X11LIBS="-L$ac_x_libraries -lXaw -lXmu -lXext -lXt -lX11"
  fi
fi

AC_SUBST([X11INCD])
AC_SUBST([X11LIB])
AC_SUBST([X11LIBS])


# Search for RPFITSLIB, we need the full pathname.
AC_MSG_NOTICE([

Looking for the full pathname of the RPFITS library...])

for EXTDIR in $EXTDIRS ; do
  AC_CHECK_FILES([$EXTDIR/librpfits.$SO $EXTDIR/librpfits.a],
                 [RPFITSLIB="$EXTDIR/librpfits.a" ; break])
done

if test "x$RPFITSLIB" = x ; then
  AC_MSG_WARN([RPFITS library not found.  Some tasks will fail to link.])
else
  AC_MSG_NOTICE([RPFITS appears to be available.])
fi

AC_SUBST([RPFITSLIB])


# Search for WCSLIB, we need the full pathname.
AC_MSG_NOTICE([

Looking for the full pathname of the WCSLIB library...])

for EXTDIR in $EXTDIRS ; do
  AC_CHECK_FILES([$EXTDIR/libwcs.$SO $EXTDIR/libwcs.a],
                 [WCSLIB="$EXTDIR/libwcs.a" ; break])
done

if test "x$WCSLIB" = x ; then
  AC_MSG_WARN([WCSLIB not found.  Many tasks will fail to link.])
else
  AC_MSG_NOTICE([WCSLIB appears to be available.])
fi

AC_SUBST([WCSLIB])


# Search for PGPLOT.  Other libraries may be required depending on what
# graphics drivers were installed with PGPLOT.  We assume only libpng which in
# turn needs zlib.
AC_MSG_NOTICE([

Looking for PGPLOT and related libraries...])

AC_CHECK_LIB([z],       [deflate],   [PGPLOTLIB="-lz $PGPLOTLIB"],
             [], [$PGPLOTLIB $XTRALIBS])
AC_CHECK_LIB([png],     [png_error], [PGPLOTLIB="-lpng $PGPLOTLIB"],
             [], [$PGPLOTLIB $XTRALIBS])
AC_LANG(Fortran 77)
AC_CHECK_LIB([pgplot],  [pgbeg],     [PGPLOTLIB="-lpgplot $PGPLOTLIB"],
             [PGPLOTLIB=], [$PGPLOTLIB $X11LIB $XTRALIBS])

if test "x$PGPLOTLIB" = x ; then
  AC_MSG_WARN([PGPLOT library not found.  Some tasks will fail to link.])
else
  AC_MSG_NOTICE([PGPLOT appears to be available.])
fi

AC_SUBST([PGPLOTLIB])


# Search for the READLINE library, required only by the Miriad shell.  Need
# either termcap or ncurses (which contains termcap functions), not both.  On
# linux systems termcap is symlink'd to ncurses.
AC_MSG_NOTICE([

Looking for READLINE and related libraries...])

DIRS=
for EXTDIR in $EXTDIRS ; do
  AC_CHECK_FILES([$EXTDIR/libtermcap.$SO $EXTDIR/libtermcap.a],
                 [DIRS=$EXTDIR ; break])
done

if test "x$DIRS" = x ; then
  for EXTDIR in $EXTDIRS ; do
    AC_CHECK_FILES([$EXTDIR/libncurses.$SO $EXTDIR/libncurses.a],
                   [DIRS=$EXTDIR ; break])
  done
fi

for EXTDIR in $EXTDIRS ; do
  AC_CHECK_FILES([$EXTDIR/libreadline.$SO $EXTDIR/libreadline.a],
                 [if test "x$DIRS" != "x$EXTDIR" ; then
                    DIRS="$EXTDIR $DIRS"
                  fi
                  break])
done

for DIR in $DIRS
do
  if test "x$DIR" != x/usr/lib -a "x$DIR" != x/lib
  then
    READLINEDIR="$READLINEDIR -L$DIR"
  fi
done

AC_LANG(C)
AC_CHECK_LIB([termcap],   [tgetent], [READLINELIB="-ltermcap"],
             [], [$READLINEDIR])

if test "x$READLINELIB" = x ; then
  AC_CHECK_LIB([ncurses], [tgetent], [READLINELIB="-lncurses"],
               [], [$READLINEDIR])
fi

AC_CHECK_LIB([readline], [readline],
             [READLINELIB="$READLINEDIR -lreadline $READLINELIB"],
             [READLINELIB=], [$READLINEDIR $READLINELIB])

if test "x$READLINELIB" = x ; then
  AC_MSG_WARN([READLINE library not found.
The Miriad shell will be compiled without command-line editing.])
else
  AC_MSG_NOTICE([READLINE appears to be available.])
fi

READLINELIB=`echo $READLINELIB`
AC_SUBST([READLINELIB])


# Finally, do variable substitutions.
AC_MSG_NOTICE([

Constructing MIRRC and $MIRARCH/GNUmakedefs...])

if test -f $MIRARCH/GNUmakedefs ; then
  # Preserve a copy of the old GNUmakedefs.
  rm -f $MIRARCH/GNUmakedefs.bak
  cp $MIRARCH/GNUmakedefs $MIRARCH/GNUmakedefs.bak
  AC_MSG_NOTICE([Preserved the old GNUmakedefs in $MIRARCH.])
fi

AC_CONFIG_FILES([MIRRC:scripts/MIRRC.in MIRRC.sh:scripts/MIRRC.sh.in])
AC_CONFIG_FILES([$MIRARCH/GNUmakedefs:scripts/GNUmakedefs.in])
AC_OUTPUT
