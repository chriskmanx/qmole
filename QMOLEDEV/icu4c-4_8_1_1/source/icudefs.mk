# Make definitions that are shared by the different subprojects of ICU.
#
# Yves Arrouye.
#
# Copyright (C) 2000-2011, International Business Machines Corporation and others.
# All Rights Reserved.

#
# Some of these variables are overridden in the config/mh-* files.
# 
# Please be sure to update config/Makefile.inc.in  if you add something here.
#

# Shell to use

SHELL = /bin/sh

# Standard directories

prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
datarootdir = ${prefix}/share
datadir = ${datarootdir}
libdir = ${exec_prefix}/lib
includedir = ${prefix}/include
mandir = ${datarootdir}/man
sysconfdir = ${prefix}/etc
# controls the include of $(top_builddir)/icucross.mk at bottom of file
cross_compiling = no
cross_buildroot = 

# Package information

PACKAGE_ICU_DESCRIPTION = "International Components for Unicode"
PACKAGE_ICU_URL = "http://icu-project.org"
PACKAGE = icu
VERSION = 4.8.1.1
UNICODE_VERSION = 6.0
SO_TARGET_VERSION = 48.1.1
SO_TARGET_VERSION_MAJOR = 48

# The ICU data external name is usually icudata; the entry point name is
# the version-dependent name (for no particular reason except it was easier
# to change the build this way). When building in common mode, the data
# name is the versioned platform-dependent one. 

ICUDATA_DIR = ${datarootdir}/$(PACKAGE)$(ICULIBSUFFIX)/$(VERSION)

ICUDATA_BASENAME_VERSION = $(ICUPREFIX)dt48
# the entry point is almost like the basename, but has the lib suffix.  
ICUDATA_ENTRY_POINT = $(ICUPREFIX)dt48 
ICUDATA_CHAR = l
ICUDATA_PLATFORM_NAME = $(ICUDATA_BASENAME_VERSION)$(ICUDATA_CHAR)
PKGDATA_LIBSTATICNAME = -L $(STATIC_PREFIX)$(ICUPREFIX)$(DATA_STUBNAME)$(ICULIBSUFFIX)
ifeq ($(strip $(PKGDATA_MODE)),)
PKGDATA_MODE=dll
endif
ifeq ($(PKGDATA_MODE),common)
ICUDATA_NAME = $(ICUDATA_PLATFORM_NAME)
ICUPKGDATA_DIR = $(ICUDATA_DIR)
else
ifeq ($(PKGDATA_MODE),dll)
ICUDATA_NAME = $(ICUDATA_PLATFORM_NAME)
PKGDATA_LIBNAME = -L $(ICUPREFIX)$(DATA_STUBNAME)$(ICULIBSUFFIX)
ICUPKGDATA_DIR = $(libdir)
else
ifeq ($(PKGDATA_MODE),static)
ICUDATA_NAME = $(ICUDATA_PLATFORM_NAME)
PKGDATA_LIBNAME = -L $(ICUPREFIX)$(DATA_STUBNAME)$(ICULIBSUFFIX)
ICUPKGDATA_DIR = $(libdir)
else
ICUDATA_NAME = $(ICUDATA_PLATFORM_NAME)
ICUPKGDATA_DIR = $(ICUDATA_DIR)
endif
endif
endif
# This is needed so that make -j2 doesn't complain when invoking pkgdata's make
PKGDATA_INVOKE_OPTS = MAKEFLAGS=

# These are defined here because mh-cygwin-msvc needs to override these values.
ICUPKGDATA_INSTALL_DIR = $(DESTDIR)$(ICUPKGDATA_DIR)
ICUPKGDATA_INSTALL_LIBDIR = $(DESTDIR)$(libdir)

# If defined to a valid value, pkgdata will generate a data library more quickly
GENCCODE_ASSEMBLY = 

# ICU specific directories

pkgdatadir = $(datadir)/$(PACKAGE)$(ICULIBSUFFIX)/$(VERSION)
pkglibdir = $(libdir)/$(PACKAGE)$(ICULIBSUFFIX)/$(VERSION)
pkgsysconfdir = $(sysconfdir)/$(PACKAGE)$(ICULIBSUFFIX)

# Installation programs

MKINSTALLDIRS = $(SHELL) $(top_srcdir)/mkinstalldirs

INSTALL = /usr/bin/install -c
INSTALL_PROGRAM = ${INSTALL}
INSTALL_DATA = ${INSTALL} -m 644
INSTALL_SCRIPT = ${INSTALL}

# Library suffix (to support different C++ compilers)

ICULIBSUFFIX=

# Compiler and tools

ENABLE_DEBUG = 0
ENABLE_RELEASE = 1
EXEEXT = 
CC = /usr/bin/gcc
CXX = g++
AR = ar
ARFLAGS =  r
RANLIB = ranlib
COMPILE_LINK_ENVVAR = 
UCLN_NO_AUTO_CLEANUP = 1

# Various flags for the tools

# DEFS is for common macro definitions.
# configure prevents user defined DEFS, and configure's DEFS is not needed
# So we ignore the DEFS that comes from configure
# U_ATTRIBUTE_DEPRECATED is defined to hide warnings about deprecated API warnings.
DEFS = -DU_ATTRIBUTE_DEPRECATED=
# CFLAGS is for C only flags
CFLAGS =  -O2 -Wall -ansi -pedantic -Wshadow -Wpointer-arith -Wmissing-prototypes -Wwrite-strings -Wno-long-long $(THREADSCFLAGS)
# CXXFLAGS is for C++ only flags
CXXFLAGS =  -O2 -W -Wall -ansi -pedantic -Wpointer-arith -Wwrite-strings -Wno-long-long $(THREADSCXXFLAGS)
# CPPFLAGS is for C Pre-Processor flags
CPPFLAGS =  $(THREADSCPPFLAGS)
# LIBCFLAGS are the flags for static and shared libraries.
LIBCFLAGS = -fvisibility=hidden
# LIBCXXFLAGS are the flags for static and shared libraries.
LIBCXXFLAGS = -fvisibility=hidden
# DEFAULT_LIBS are the default libraries to link against
DEFAULT_LIBS = -lpthread -lm 
# LIB_M is for linking against the math library
LIB_M = 
# LIB_THREAD is for linking against the threading library
LIB_THREAD = 
# OUTOPT is for creating a specific output name
OUTOPT = -o # The extra space after the argument is needed.
# AR_OUTOPT is for creating a specific output name for static libraries.
AR_OUTOPT =

ENABLE_RPATH = NO
ifeq ($(ENABLE_RPATH),YES)
RPATHLDFLAGS = $(LD_RPATH)$(LD_RPATH_PRE)$(libdir)
endif
LDFLAGS =  $(RPATHLDFLAGS)

# What kind of libraries are we building and linking against?
ENABLE_STATIC = 
ENABLE_SHARED = YES

# Echo w/o newline

#ECHO_N = -n
#ECHO_C = 

# Commands to compile
COMPILE.c=    $(CC) $(CPPFLAGS) $(DEFS) $(CFLAGS) -c
COMPILE.cc=   $(CXX) $(CPPFLAGS) $(DEFS) $(CXXFLAGS) -c

# Commands to link
LINK.c=       $(CC) $(CFLAGS) $(LDFLAGS)
LINK.cc=      $(CXX) $(CXXFLAGS) $(LDFLAGS)

# Commands to make a shared library
SHLIB.c=      $(CC) $(CFLAGS) $(LDFLAGS) -shared $(LD_SOOPTIONS)
SHLIB.cc=     $(CXX) $(CXXFLAGS) $(LDFLAGS) -shared $(LD_SOOPTIONS)

# Environment variable to set a runtime search path
LDLIBRARYPATH_ENVVAR = LD_LIBRARY_PATH

# Versioned target for a shared library.
FINAL_SO_TARGET = $(SO_TARGET).$(SO_TARGET_VERSION)
MIDDLE_SO_TARGET = $(SO_TARGET).$(SO_TARGET_VERSION_MAJOR)
SHARED_OBJECT = $(FINAL_SO_TARGET)

##  How ICU libraries are named...  ex. $(LIBICU)uc$(SO)
# Prefix for the ICU library names
ICUPREFIX = icu
LIBPREFIX = lib
LIBICU = $(LIBPREFIX)$(ICUPREFIX)

## If we can't use the shared libraries, use the static libraries
ifneq ($(ENABLE_SHARED),YES)
STATIC_PREFIX_WHEN_USED = s
else
STATIC_PREFIX_WHEN_USED = 
endif

# Static library prefix and file extension
STATIC_PREFIX = s
LIBSICU = $(LIBPREFIX)$(STATIC_PREFIX)$(ICUPREFIX)
A = a
SOBJ = $(SO)

# Force removal [for make clean]
RMV = rm -rf

# Platform commands to remove or move executable and library targets
# INSTALL-L installs libraries. Override in mh-* file to INSTALL_PROGRAM
#           when the library needs to have executable permissions
INSTALL-S = $(INSTALL_PROGRAM)
INSTALL-L = $(INSTALL_PROGRAM)
#INSTALL-L = $(INSTALL_DATA)

# Location of the libraries before "make install" is used
LIBDIR=$(top_builddir)/lib

# Location of the executables before "make install" is used
BINDIR=$(top_builddir)/bin

# overridden by icucross.mk
TOOLBINDIR=$(BINDIR)
TOOLLIBDIR=$(LIBDIR)

# Current full path directory.
CURR_FULL_DIR=$(shell pwd | sed 's/ /\\ /g')
# Current full path directory for use in source code in a -D compiler option.
CURR_SRCCODE_FULL_DIR=$(shell pwd | sed 's/ /\\ /')

# Name flexibility for the library naming scheme.  Any modifications should
# be made in the mh- file for the specific platform.
DATA_STUBNAME = data
COMMON_STUBNAME = uc
I18N_STUBNAME = i18n
LAYOUT_STUBNAME = le
LAYOUTEX_STUBNAME = lx
IO_STUBNAME = io
TOOLUTIL_STUBNAME = tu
CTESTFW_STUBNAME = test

# Just the libs.
ICULIBS_DT	= -l$(STATIC_PREFIX_WHEN_USED)$(ICUPREFIX)$(DATA_STUBNAME)$(ICULIBSUFFIX)$(SO_TARGET_VERSION_SUFFIX)
ICULIBS_UC	= -l$(STATIC_PREFIX_WHEN_USED)$(ICUPREFIX)$(COMMON_STUBNAME)$(ICULIBSUFFIX)$(SO_TARGET_VERSION_SUFFIX)
ICULIBS_I18N	= -l$(STATIC_PREFIX_WHEN_USED)$(ICUPREFIX)$(I18N_STUBNAME)$(ICULIBSUFFIX)$(SO_TARGET_VERSION_SUFFIX)
ICULIBS_LE	= -l$(STATIC_PREFIX_WHEN_USED)$(ICUPREFIX)$(LAYOUT_STUBNAME)$(ICULIBSUFFIX)$(SO_TARGET_VERSION_SUFFIX)
ICULIBS_LX	= -l$(STATIC_PREFIX_WHEN_USED)$(ICUPREFIX)$(LAYOUTEX_STUBNAME)$(ICULIBSUFFIX)$(SO_TARGET_VERSION_SUFFIX)
ICULIBS_IO	= -l$(STATIC_PREFIX_WHEN_USED)$(ICUPREFIX)$(IO_STUBNAME)$(ICULIBSUFFIX)$(SO_TARGET_VERSION_SUFFIX)
ICULIBS_CTESTFW	= -l$(STATIC_PREFIX_WHEN_USED)$(ICUPREFIX)$(CTESTFW_STUBNAME)$(ICULIBSUFFIX)$(SO_TARGET_VERSION_SUFFIX)
ICULIBS_TOOLUTIL = -l$(STATIC_PREFIX_WHEN_USED)$(ICUPREFIX)$(TOOLUTIL_STUBNAME)$(ICULIBSUFFIX)$(SO_TARGET_VERSION_SUFFIX)
# Link commands to link to ICU libs
LLIBDIR		= -L$(LIBDIR)
LSTUBDIR	= -L$(top_builddir)/stubdata
LCTESTFW	= -L$(top_builddir)/tools/ctestfw

LIBICUDT	= $(LLIBDIR) $(LSTUBDIR) $(ICULIBS_DT)
LIBICUUC	= $(LLIBDIR) $(ICULIBS_UC) $(LSTUBDIR) $(ICULIBS_DT)
LIBICUI18N	= $(LLIBDIR) $(ICULIBS_I18N)
LIBICULE	= $(LLIBDIR) $(ICULIBS_LE)
LIBICULX	= $(LLIBDIR) $(ICULIBS_LX)
LIBCTESTFW	= $(LCTESTFW) $(ICULIBS_CTESTFW)
LIBICUTOOLUTIL	= $(LLIBDIR) $(ICULIBS_TOOLUTIL)
LIBICUIO	= $(LLIBDIR) $(ICULIBS_IO)

# Invoke, set library path for all ICU libraries.
# overridden by icucross.mk
INVOKE = $(LDLIBRARYPATH_ENVVAR)=$(LIBRARY_PATH_PREFIX)$(LIBDIR):$(top_builddir)/stubdata:$(top_builddir)/tools/ctestfw:$$$(LDLIBRARYPATH_ENVVAR) $(LEAK_CHECKER)
# prefer stubdata
PKGDATA_INVOKE = $(LDLIBRARYPATH_ENVVAR)=$(top_builddir)/stubdata:$(top_builddir)/tools/ctestfw:$(LIBRARY_PATH_PREFIX)$(LIBDIR):$$$(LDLIBRARYPATH_ENVVAR) $(LEAK_CHECKER) $(PKGDATA_INVOKE_OPTS)

# Platform-specific setup
include $(top_srcdir)/config/mh-darwin

# When shared libraries are disabled and static libraries are enabled,
# the C++ compiler must be used to link in the libraries for the tools.
ifneq ($(ENABLE_SHARED),YES)
LINK.c = $(LINK.cc)
endif

# some imported things from the cross env
TOOLEXEEXT = $(EXEEXT)
ifneq ($(strip $(cross_buildroot)),)
include $(cross_buildroot)/config/icucross.mk
else
cross_buildroot = $(top_builddir)
endif

# optional include at top
-include $(top_builddir)/icudefs.local
