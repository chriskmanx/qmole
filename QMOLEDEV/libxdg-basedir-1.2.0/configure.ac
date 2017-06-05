AC_PREREQ(2.60)
AC_INIT([libxdg-basedir], [1.2.0], mark.nevill@gmail.com)
AM_INIT_AUTOMAKE([-Wall foreign])
# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_LIBTOOL
# Configure Doxygen
DX_HTML_FEATURE(ON)
DX_CHM_FEATURE(OFF)
DX_CHI_FEATURE(OFF)
DX_MAN_FEATURE(OFF)
DX_RTF_FEATURE(OFF)
DX_XML_FEATURE(OFF)
DX_PDF_FEATURE(OFF)
DX_PS_FEATURE(OFF)
DX_INIT_DOXYGEN([libxdg-basedir], [doxygen.cfg], doc)
# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h string.h strings.h memory.h errno.h sys/stat.h])
# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_MODE_T
# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset strcpy strncpy bcopy bzero getenv mkdir strdup])

CC_NOUNDEFINED

# Generated files.
AC_CONFIG_HEADER([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_FILES([Makefile
				include/Makefile
				src/Makefile
				tests/Makefile
				pkgconfig/libxdg-basedir.pc
				pkgconfig/libxdg-basedir-uninstalled.pc])
AC_OUTPUT
