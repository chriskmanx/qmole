# Component settings
COMPONENT := nsutils
COMPONENT_VERSION := 0.0.1
# Default to a static library
COMPONENT_TYPE ?= lib-static

# Setup the tooling
PREFIX ?= /opt/netsurf
NSSHARED ?= $(PREFIX)/share/netsurf-buildsystem
include $(NSSHARED)/makefiles/Makefile.tools

# Reevaluate when used, as BUILDDIR won't be defined yet
TESTRUNNER = test/runtest.sh $(BUILDDIR) $(EXEEXT)

# Toolchain flags
WARNFLAGS := -Wall -W -Wundef -Wpointer-arith -Wcast-align \
	-Wwrite-strings -Wstrict-prototypes -Wmissing-prototypes \
	-Wmissing-declarations -Wnested-externs
CFLAGS := -I$(CURDIR)/include/ -I$(CURDIR)/src $(WARNFLAGS) $(CFLAGS)
ifneq ($(GCCVER),2)
  CFLAGS := $(CFLAGS) -std=c99
else
  # __inline__ is a GCCism
  CFLAGS := $(CFLAGS) -Dinline="__inline__"
endif
CFLAGS := $(CFLAGS) -D_POSIX_C_SOURCE=200809L

REQUIRED_LIBS := nsutils

# Strictly the requirement for rt is dependant on both the clib and if
# the build is using rt features like clock_gettime() but this check
# will suffice
ifeq ($(HOST),x86_64-linux-gnu)
  REQUIRED_LIBS := $(REQUIRED_LIBS) rt
endif

TESTCFLAGGS := -g -O2
TESTLDFLAGS := -lm -l$(COMPONENT) $(TESTLDFLAGS)

include $(NSBUILD)/Makefile.top

# Extra installation rules
I := /include/nsutils
INSTALL_ITEMS := $(INSTALL_ITEMS) $(I):include/nsutils/errors.h
INSTALL_ITEMS := $(INSTALL_ITEMS) $(I):include/nsutils/base64.h
INSTALL_ITEMS := $(INSTALL_ITEMS) $(I):include/nsutils/time.h
INSTALL_ITEMS := $(INSTALL_ITEMS) /$(LIBDIR)/pkgconfig:lib$(COMPONENT).pc.in
INSTALL_ITEMS := $(INSTALL_ITEMS) /$(LIBDIR):$(OUTPUT)
