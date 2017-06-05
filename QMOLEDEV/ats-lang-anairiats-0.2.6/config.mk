INSTALL := /usr/bin/install -c
PACKAGE_TARNAME := ats-anairiats
PACKAGE_VERSION := 0.2.6

abs_top_srcdir := /home/chris/ats-lang-anairiats-0.2.6
prefix := /usr/local
exec_prefix := ${prefix}
bindir := ${exec_prefix}/bin

CC := /usr/bin/gcc
AR := ar
LN_S := ln -s
INSTALL := /usr/bin/install -c
MKDIR_P := /bin/mkdir -p

CFLAGS := -g -O2 
CPPFLAGS := 
LDFLAGS :=  

ATSHOME := $(abs_top_srcdir)
ATSNEWHOME := $(prefix)/lib/$(PACKAGE_TARNAME)-$(PACKAGE_VERSION)
ATSHOMERELOC := ATS-$(PACKAGE_VERSION)
ATSHOMEQ := "$(ATSHOME)"

HAVE_LIBGMP := yes
HAVE_LIBGLIB20 := 1
HAVE_LIBGTK20 := 1
HAVE_LIBSDL10 := @HAVE_LIBSDL10@
