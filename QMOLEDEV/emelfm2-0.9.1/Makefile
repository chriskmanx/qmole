# $Id: Makefile 3031 2014-01-24 21:07:28Z tpgww $
#
# Copyright (C) 2003-2013 tooar <tooar@emelfm2.net>
#
# This file is part of emelFM2.
# emelFM2 is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3, or (at your option) any later version.
#
# emelFM2 is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# emelFM2; see the file GPL. If not, contact the Free Software Foundation,
# 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

# This is the Makefile of emelFM2. You should not change it!
# Edit the file Makefile.config to change build options.

# include user configuration
include Makefile.config

# internal configuration
DIRS=src src/actions src/build src/command src/command/complete src/config \
     src/dialogs src/filesystem src/utils
LIBS=plugins
OPTLIBS=plugins/optional
ICONS=icons
OBJECTS_DIR=objs
DOCS=docs
PO_DIR=po
API_DIR=docs/api

# deprecated, for uninstalling only
XDG_APPLICATION_DIR=$(PREFIX)/share/application-registry

BUILD_FILE=src/build/build.h
DESKTOP_FILE=docs/desktop_environment/$(TARGET).desktop
SU_FILE=app.$(TARGET).policy

# build info for build.h etc
VERSION=0.9.1
#this is effectively a sub-version, to manage config file upgrades while not changing the version
RELEASE=
#RELEASE=.1
BUILDDATE=$(shell date)
BUILDSTAMP=$(shell date +"%02d %b %04Y")
BUILDINFO=$(shell uname) $(shell uname -r)/$(shell uname -m)
COPYRIGHT=2003-$(shell date +"%04Y"), tooar <tooar@emelfm2.net>
# for logging build-time settings, |-separated strings
BUILD_PARMS:=BIN_DIR=$(BIN_DIR)|CFLAGS=$(CFLAGS)|CONFIGDOC=$(CONFIGDOC)|DEBUG=$(DEBUG)
BUILD_PARMS+=|DOC_DIR=$(DOC_DIR)|DOCS_VERSION=$(DOCS_VERSION)|EDITOR_SPELLCHECK=$(EDITOR_SPELLCHECK)
BUILD_PARMS+=|EXTRA_BINDINGS=$(EXTRA_BINDINGS)|FILES_UTF8ONLY=$(FILES_UTF8ONLY)|HELPDOC=$(HELPDOC)
BUILD_PARMS+=|I18N=$(I18N)|ICON_DIR=$(ICON_DIR)|LIB_DIR=$(LIB_DIR)|LOCALE_DIR=$(LOCALE_DIR)
BUILD_PARMS+=|MAN_DIR=$(MAN_DIR)|NEW_COMMAND=$(NEW_COMMAND)|PANES_HORIZONTAL=$(PANES_HORIZONTAL)
BUILD_PARMS+=|PLUGINS_DIR=$(PLUGINS_DIR)|PREFIX=$(PREFIX)|STRIP=$(STRIP)|USE_GAMIN=$(USE_GAMIN)
BUILD_PARMS+=|USE_INOTIFY=$(USE_INOTIFY)|USE_KQUEUE=$(USE_KQUEUE)|USE_PORTEVENT=$(USE_PORTEVENT)
BUILD_PARMS+=|WITH_ACL=$(WITH_ACL)|WITH_ASSIST=$(WITH_ASSIST)|WITH_CUSTOMMOUSE=$(WITH_CUSTOMMOUSE)
BUILD_PARMS+=|WITH_DEVKIT=$(WITH_DEVKIT)|WITH_GTK2=$(WITH_GTK2)|WITH_GTK3=$(WITH_GTK3)|WITH_HAL=$(WITH_HAL)
BUILD_PARMS+=|WITH_KERNELFAM=$(WITH_KERNELFAM)|WITH_LATEST=$(WITH_LATEST)|WITH_OUTPUTSTYLES=$(WITH_OUTPUTSTYLES)
BUILD_PARMS+=|WITH_POLKIT=$(WITH_POLKIT)|WITH_THUMBLIB=$(WITH_THUMBLIB)|WITH_THUMBS=$(WITH_THUMBS)
BUILD_PARMS+=|WITH_TRACKER=$(WITH_TRACKER)|WITH_TRANSPARENCY=$(WITH_TRANSPARENCY)|WITH_UDISKS=$(WITH_UDISKS)
BUILD_PARMS+=|XDG_DESKTOP_DIR=$(XDG_DESKTOP_DIR)|XDG_INTEGRATION=$(XDG_INTEGRATION)

ifeq ($(DOCS_VERSION), 1)
# cannot use just +=, that inserts a space
TMP:=$(DOC_DIR)-$(VERSION)
DOC_DIR:=$(TMP)
endif

# check for deprecated alternative parameters
ifeq ($(WITH_LATEST), 0)
WITH_LATEST = $(USE_LATEST)
endif
ifeq ($(WITH_UDISKS), 0)
WITH_UDISKS = $(WITH_DEVKIT)
endif

# object directories that have to be created
MKOBJDIRS = $(foreach dir, $(DIRS) $(LIBS) $(OPTLIBS), $(OBJECTS_DIR)/$(dir))

HEADERS = $(foreach dir, $(DIRS), $(wildcard $(dir)/*.h))
SOURCES = $(foreach dir, $(DIRS), $(wildcard $(dir)/*.c))
OBJECTS = $(SOURCES:%.c=$(OBJECTS_DIR)/%.o)
DEP_FILES = $(SOURCES:%.c=$(OBJECTS_DIR)/%.deps)

LIBS_HEADERS = $(foreach dir, $(LIBS), $(wildcard $(dir)/*.h))
LIBS_SOURCES = $(foreach dir, $(LIBS), $(wildcard $(dir)/*.c))
LIBS_OBJECTS = $(LIBS_SOURCES:%.c=$(OBJECTS_DIR)/%.so)
LIBS_DEP_FILES = $(LIBS_SOURCES:%.c=$(OBJECTS_DIR)/%.deps)

# optional plugins (and related icons, below)
PLUGFILEPREFIX=e2p_
ICONFILEPREFIX=plugin_
BASENAME1:=thumbnail
BASENAME2:=track
BASENAME3:=acl
BASENAME9:=vfs

ifeq ($(WITH_THUMBS),0)
# when doing make install, make i18n etc don't want to have to supply optional plugin parameters
WITH_THUMBS:=$(shell test -f $(OBJECTS_DIR)/$(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME1).so && echo "1" || echo "0" 2>&1)
endif
ifeq ($(WITH_THUMBS),1)
THUMBS_HEADERS = $(wildcard $(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME1)*.h)
THUMBS_SOURCES = $(wildcard $(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME1)*.c)
THUMBS_OBJECTS = $(THUMBS_SOURCES:%.c=$(OBJECTS_DIR)/%.so)
ifeq ($(WITH_THUMBLIB),0)
THUMBS_FLAGS =
THUMBS_LIBS =
else
THUMBS_FLAGS = $(shell pkg-config --cflags lib$(BASENAME1))
THUMBS_LIBS = $(shell pkg-config --libs lib$(BASENAME1))
endif
endif

ifeq ($(WITH_TRACKER),0)
WITH_TRACKER:=$(shell test -f $(OBJECTS_DIR)/$(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME2).so && echo "1" || echo "0" 2>&1)
endif
ifeq ($(WITH_TRACKER),1)
TRACKER_HEADERS = $(wildcard $(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME2)*.h)
TRACKER_SOURCES = $(wildcard $(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME2)*.c)
TRACKER_OBJECTS = $(TRACKER_SOURCES:%.c=$(OBJECTS_DIR)/%.so)
TRACKER_FLAGS = #$(shell pkg-config --cflags ??)
TRACKER_LIBS = #$(shell pkg-config --libs ??)
endif

ifeq ($(WITH_ACL),0)
WITH_ACL:=$(shell test -f $(OBJECTS_DIR)/$(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME3).so && echo "1" || echo "0" 2>&1)
endif
ifeq ($(WITH_ACL),1)
ACL_HEADERS = $(wildcard $(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME3)*.h)
ACL_SOURCES = $(wildcard $(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME3)*.c)
ACL_OBJECTS = $(ACL_SOURCES:%.c=$(OBJECTS_DIR)/%.so)
ACL_FLAGS =
ACL_LIBS = -lacl
endif

ifeq ($(WITH_VFS),0)
WITH_VFS:=$(shell test -f $(OBJECTS_DIR)/$(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME9).so && echo "1" || echo "0" 2>&1)
endif
ifeq ($(WITH_VFS),1)
 VFS_HEADERS = $(wildcard $(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME9)*.h)
 VFS_SOURCES = $(wildcard $(OPTLIBS)/$(PLUGFILEPREFIX)$(BASENAME9)*.c)
 VFS_OBJECTS = $(VFS_SOURCES:%.c=$(OBJECTS_DIR)/%.so)
 VFS_FLAGS = $(shell pkg-config --cflags gio-2.0 gio-unix-2.0)
 VFS_LIBS = $(shell pkg-config --libs gio-2.0 gio-unix-2.0)
else
 ifeq ($(WITH_GVFS),1)
  VFS_HEADERS = #FIXME
  VFS_SOURCES = #FIXME
  VFS_OBJECTS = #FIXME
  VFS_FLAGS = $(shell pkg-config --cflags gio-2.0 gio-unix-2.0)
  VFS_LIBS = $(shell pkg-config --libs gio-2.0 gio-unix-2.0)
 endif
endif

LIBS_XHEADERS = $(THUMBS_HEADERS) $(TRACKER_HEADERS) $(ACL_HEADERS) $(VFS_HEADERS)
LIBS_XSOURCES = $(THUMBS_SOURCES) $(TRACKER_SOURCES) $(ACL_SOURCES) $(VFS_SOURCES)
LIBS_XOBJECTS = $(LIBS_XSOURCES:%.c=$(OBJECTS_DIR)/%.so)
LIBS_XDEP_FILES = $(LIBS_XSOURCES:%.c=$(OBJECTS_DIR)/%.deps)

# set up flags
lCFLAGS = $(CFLAGS)
#ifeq ($(WITH_LATEST), 1)
#these are redundant now
#lCFLAGS += -DGTK_DISABLE_DEPRECATED
#lCFLAGS += -DGDK_DISABLE_DEPRECATED
#lCFLAGS += -DG_DISABLE_DEPRECATED
#check for gtk3 limited header-inclusions
#lCFLAGS += -DGTK_DISABLE_SINGLE_INCLUDES
#disable direct access to 'becoming-private' object-data
#lCFLAGS += -DGSEAL_ENABLE
#endif

# set up debugging
ifeq ($(DEBUG), 0)
# this default level omits all warning-message code
DEBUG_LEVEL ?= 2
else
#this is the highest level
DEBUG_LEVEL ?= 5
lCFLAGS += -g -O0
endif

ifeq ($(USE_GAMIN), 1)
#gamin prevails over specific kernel monitoring
WITH_KERNELFAM = 0
USE_INOTIFY = 0
USE_KQUEUE = 0
USE_PORTEVENT = 0
endif

#other conditional defines set in $(BUILD_FILE) - see below

# set up i18n
BIN_XGETTEXT = $(shell which xgettext 2>/dev/null)
ifeq ($(BIN_XGETTEXT), "")
I18N = 0
endif
BIN_MSGFMT = $(shell which msgfmt 2>/dev/null)
ifeq ($(BIN_MSGFMT), "")
I18N = 0
endif
BIN_MSGMERGE = $(shell which msgmerge 2>/dev/null)
ifeq ($(BIN_MSGMERGE), "")
I18N = 0
endif
ifeq ($(I18N), 1)
PO_FILES=$(wildcard $(PO_DIR)/*.po)
MO_FILES=$(PO_FILES:%.po=%.mo)
lCFLAGS += -DENABLE_NLS
endif

# local include directories
LINC = $(foreach dir, $(DIRS), -I$(dir))
#LLIBINC = $(foreach dir, $(LIBS) $(OPTLIBS), -I$(dir))

# some code can be compiler-version-specific
#GCCV=$(shell $(CC) -v 2>&1 | grep '^gcc version ' | sed 's/ (.*//; s/.* //')

# overflow protection with gcc4
#lCFLAGS += -D_FORTIFY_SOURCE
lCFLAGS += -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -D_REENTRANT -I. $(LINC)

ifeq ($(WITH_GTK2), 1)
GTK2 = 1
GTK3 = 0
else
 ifeq ($(WITH_GTK3), 1)
  GTK2 = 0
  GTK3 = 1
 else
  GTK2 = $(shell pkg-config --exists gtk+-2.0 && echo "1" || echo "0")
  ifeq ($(WITH_LATEST), 1)
    GTK3 = $(shell pkg-config --exists gtk+-3.0 && echo "1" || echo "0")
  else
    GTK3 = 0
  endif
 endif
endif

#ifeq ($(WITH_LATEST), 1)
#GLIB3 = $(shell pkg-config --exists glib-3.0 && echo "1" || echo "0")
#else
#GLIB3 = 0
#endif

# may need extra lib for hal, udisks
EXTDBUS = $(shell pkg-config --atleast-version=2.26 glib-2.0 && echo "0" || echo "1")
# may need extra lib for threads
EXTGTHREAD = $(shell pkg-config --atleast-version=2.32 glib-2.0 && echo "0" || echo "1")

ifeq ($(GTK3), 1)
lCFLAGS += $(shell pkg-config --cflags gtk+-3.0 gdk-3.0)
else
lCFLAGS += $(shell pkg-config --cflags gtk+-2.0)
endif
ifeq ($(EXTGTHREAD), 1)
lCFLAGS += $(shell pkg-config --cflags gthread-2.0)
endif
ifeq ($(EDITOR_SPELLCHECK),1)
lCFLAGS += $(shell pkg-config --cflags gtkspell-2.0)
endif
ifeq ($(WITH_UDISKS),1)
 ifeq ($(EXTDBUS), 1)
  lCFLAGS += $(shell pkg-config --cflags dbus-glib-1)
 endif
else
ifeq ($(WITH_HAL),1)
 ifeq ($(EXTDBUS), 1)
  lCFLAGS += $(shell pkg-config --cflags dbus-1 dbus-glib-1 hal hal-storage)
 else
  lCFLAGS += $(shell pkg-config --cflags dbus-1 hal hal-storage)
 endif
endif
endif

lLIBS_CFLAGS = -shared -fPIC -DPIC

#setup linking
ifeq ($(GTK3), 1)
lLIBS = $(shell pkg-config --libs gtk+-3.0 gdk-3.0)
else
lLIBS = $(shell pkg-config --libs gtk+-2.0)
endif
ifeq ($(EXTGTHREAD), 1)
 lLIBS += $(shell pkg-config --libs gthread-2.0 gmodule-2.0)
else
 lLIBS += $(shell pkg-config --libs gmodule-2.0)
endif
# -lrt needed for clock_gettime(), explicit -lm, -ldl needed for some arch-linux distros
lLIBS += -lm -ldl
ifeq ($(USE_GAMIN), 1)
#gamin code is a superset of FAM code, so gamin needs fam as well
lLIBS += -lfam
endif
ifeq ($(EDITOR_SPELLCHECK),1)
lLIBS += $(shell pkg-config --libs gtkspell-2.0)
endif
ifeq ($(WITH_UDISKS),1)
 ifeq ($(EXTDBUS), 1)
  lLIBS += $(shell pkg-config --libs dbus-glib-1)
 endif
else
ifeq ($(WITH_HAL),1)
 ifeq ($(EXTDBUS), 1)
  lLIBS += $(shell pkg-config --libs dbus-1 dbus-glib-1 hal hal-storage)
 else
  lLIBS += $(shell pkg-config --libs dbus-1 hal hal-storage)
 endif
endif
endif
# should not need translation
OPSYS := $(shell uname)
ifeq ($(OPSYS),FreeBSD)
OSREL = $(shell sysctl -n kern.osreldate)
lLIBS += $(shell if test $(OSREL) -lt 500041 ; then echo "-lgnugetopt"; fi)
endif

.PHONY: all plugins install install_plugins uninstall uninstall_plugins doc \
        clean clean_plugins distclean deps i18n install_i18n uninstall_i18n \
        test test2 help

# targets
all: $(OBJECTS_DIR) $(BUILD_FILE) $(TARGET) $(LIBS) $(OPTLIBS) $(DESKTOP_FILE) $(SU_FILE)

plugins: $(OBJECTS_DIR)/$(LIBS) $(LIBS_OBJECTS) $(LIBS_XOBJECTS)

install: all install_plugins
	@echo "installing $(TARGET) to prefix '$(PREFIX)'"
	@install -d -m 755 $(BIN_DIR)
	@install -m 755 $(TARGET) $(BIN_DIR)
	@install -d $(ICON_DIR)
	@for file in `ls $(ICONS)/ |grep -v 'svn\|stock'`; do \
		install -m 644 $(ICONS)/$$file $(ICON_DIR); \
	done
	@install -d $(ICON_DIR)/stock
ifeq ($(WITH_SYSTEM_ICONS),1)
	@for file in `ls $(ICONS)/stock/ |grep -v svn`; do \
		install -m 644 $(ICONS)/stock/$$file $(ICON_DIR)/stock; \
	done
else
	@for file in `ls $(ICONS)/stock/gtk-discard*`; do \
		install -m 644 $$file $(ICON_DIR)/stock; \
	done
endif
	@install -d $(DOC_DIR)
	@for file in `ls $(DOCS)/ |grep -v svn |grep -v desktop_environment |grep -v api |grep -v emelfm2.1`; do \
		install -m 644 $(DOCS)/$$file $(DOC_DIR); \
	done
	@install -d $(MAN_DIR)
	@install -m 644 $(DOCS)/emelfm2.1 $(MAN_DIR)/$(TARGET).1;

	@if [ "`grep "#define E2_XDG" $(BUILD_FILE)`" = "#define E2_XDG" ]; then \
		install -d $(XDG_DESKTOP_DIR); \
		install -m 644 -T $(DOCS)/desktop_environment/$(TARGET).desktop $(XDG_DESKTOP_DIR)/$(TARGET).desktop; \
	fi

# no i18n install unless specific target used

install_plugins: plugins
	@echo "installing plugins to prefix '$(PREFIX)'"
	@install -d $(PLUGINS_DIR)
	@for file in "$(LIBS_OBJECTS) $(LIBS_XOBJECTS)"; do \
		install -m 755 $$file $(PLUGINS_DIR); \
	done
	@install -d $(ICON_DIR)
#FIXME also handle svg icons etc
ifeq ($(WITH_THUMBS),1)
	@install -m 644 $(OPTLIBS)/$(ICONFILEPREFIX)$(BASENAME1)_48.png $(ICON_DIR);
endif
ifeq ($(WITH_TRACKER),1)
	@install -m 644 $(OPTLIBS)/$(ICONFILEPREFIX)$(BASENAME2)_48.png $(ICON_DIR);
endif
ifeq ($(WITH_ACL),1)
	@install -m 644 $(OPTLIBS)/$(ICONFILEPREFIX)$(BASENAME3)_48.png $(ICON_DIR);
endif

uninstall: uninstall_plugins
	@echo "uninstalling $(TARGET) from prefix '$(PREFIX)'"
	@rm -f $(BIN_DIR)/$(TARGET)
	@rm -rf $(DOC_DIR)
#	@echo -e "\nif you like you can also delete the icon directory:\n\t$(ICON_DIR)\n"
	@rm -rf $(ICON_DIR)
	@test -f "$(XDG_DESKTOP_DIR)/$(TARGET).desktop" && rm -f $(XDG_DESKTOP_DIR)/$(TARGET).desktop
	@test -f "$(XDG_APPLICATION_DIR)/$(TARGET).applications" && rm -f $(XDG_APPLICATION_DIR)/$(TARGET).applications
	@rm -f $(MAN_DIR)/$(TARGET).1*;

uninstall_plugins:
	@echo "uninstalling plugins from prefix '$(PREFIX)'"
# 2 removals in case the paths are different, divert any test failure with "|| :"
	@test -d "$(LIB_DIR)/$(TARGET)" && rm -rf "$(LIB_DIR)/$(TARGET)" || :
	@test -d "$(PLUGINS_DIR)" && rm -rf "$(PLUGINS_DIR)" || :

doc:
	@echo -n "generating api documentation in '$(API_DIR)'"
	@rm -rf $(API_DIR)
	@doxygen >/dev/null
	@echo

distclean: clean

clean:
	@echo "cleaning up"
	@rm -f $(TARGET)
	@rm -f $(BUILD_FILE)
	@rm -f $(DESKTOP_FILE)
	@rm -f *.bak
	@rm -f $(PO_DIR)/$(TARGET).pot
	@rm -f $(PO_DIR)/*.mo
	@rm -f $(OPTLIBS)/*.policy
	@rm -rf $(OBJECTS_DIR)
	@rm -rf $(API_DIR)

clean_plugins:
	@echo "cleaning up plugins"
	@rm -rf $(OBJECTS_DIR)/$(LIBS)

$(TARGET): $(OBJECTS)
	@echo "linking binary '$(TARGET)'"
	@$(CC) $(LDFLAGS) $(OBJECTS) -o $(TARGET) -Wl $(lLIBS)
ifeq ($(STRIP), 1)
ifneq ($(DEBUG), 1)
	@echo "stripping binary '$(TARGET)'"
	@strip $(TARGET)
endif
endif

$(OBJECTS): $(OBJECTS_DIR)/%.o: %.c src/emelfm2.h
	@echo "compiling '$*.c'"
	@$(CC) $(lCFLAGS) -c -o $@ $*.c

$(LIBS_OBJECTS): $(OBJECTS_DIR)/%.so: %.c src/emelfm2.h src/e2_plugins.h
	@echo "compiling '$*.c'"
	@$(CC) $(lCFLAGS) $(lLIBS_CFLAGS) $(LDFLAGS) -o $@ $*.c
ifeq ($(STRIP), 1)
ifneq ($(DEBUG), 1)
	@strip -g $@
endif
endif

$(THUMBS_OBJECTS): $(OBJECTS_DIR)/%.so: %.c src/emelfm2.h src/e2_plugins.h
	@echo "compiling '$*.c'"
	@$(CC) $(lCFLAGS) $(lLIBS_CFLAGS) $(THUMBS_FLAGS) $(LDFLAGS) -o $@ $*.c -Wl,--as-needed $(THUMBS_LIBS)
ifeq ($(STRIP), 1)
ifneq ($(DEBUG), 1)
	@strip -g $@
endif
endif

$(TRACKER_OBJECTS): $(OBJECTS_DIR)/%.so: %.c src/emelfm2.h src/e2_plugins.h
	@echo "compiling '$*.c'"
	@$(CC) $(lCFLAGS) $(lLIBS_CFLAGS) $(TRACKER_FLAGS) $(LDFLAGS) -o $@ $*.c -Wl,--as-needed $(TRACKER_LIBS)
ifeq ($(STRIP), 1)
ifneq ($(DEBUG), 1)
	@strip -g $@
endif
endif

$(ACL_OBJECTS): $(OBJECTS_DIR)/%.so: %.c src/emelfm2.h src/e2_plugins.h
	@echo "compiling '$*.c'"
	@$(CC) $(lCFLAGS) $(lLIBS_CFLAGS) $(ACL_FLAGS) $(LDFLAGS) -o $@ $*.c -Wl,--as-needed $(ACL_LIBS)
ifeq ($(STRIP), 1)
ifneq ($(DEBUG), 1)
	@strip -g $@
endif
endif

$(SU_FILE): po/app.emelfm2.policy.in
	@sed s~@PREFIX@~$(PREFIX)~g po/app.emelfm2.policy.in > $(SU_FILE)

$(VFS_OBJECTS): $(OBJECTS_DIR)/%.so: %.c $(OPTLIBS)/e2_vfs_dialog.c src/e2_plugins.h
	@echo "compiling '$*.c'"
	@$(CC) $(lCFLAGS) $(lLIBS_CFLAGS) $(VFS_FLAGS) $(LDFLAGS) -o $@ $*.c -Wl,--as-needed $(VFS_LIBS) 
ifeq ($(STRIP), 1)
ifneq ($(DEBUG), 1)
	@strip -g $@
endif
endif

deps: $(DEP_FILES) #UNUSED $(LIB_DEP_FILES)
$(DEP_FILES): $(OBJECTS_DIR)
$(DEP_FILES): $(BUILD_FILE)
$(DEP_FILES): $(OBJECTS_DIR)/%.deps: %.c
	@echo "generating '$@'"
	@$(CC) $(lCFLAGS) -MM -o $@ $*.c

#$(LIB_DEP_FILES): $(OBJECTS_DIR)
#$(LIB_DEP_FILES): $(BUILD_FILE)
#$(LIB_DEP_FILES): $(OBJECTS_DIR)/%.deps: %.c
#	@echo "generating LIB '$@'"
#	@$(CC) $(lCFLAGS) $(lLIBS_CFLAGS) $(lLIB_LIBS) -MM -o $@ $*.c

$(DEPS_FILE): $(BUILD_FILE)
	@echo "generating dependencies: '$(DEPS_FILE)'"
	@touch $(DEPS_FILE)
	@makedepend -s "# generated dependencies" -f $(DEPS_FILE) -- $(lCFLAGS) -- $(SOURCES)
	@rm -f $(DEPS_FILE).bak

$(MO): $(PO_DIR)/%.mo: $(PO_DIR)/%.po
	@echo "formatting '$*.po'"
	@$(BIN_MSGFMT) $(PO_DIR)/$*.po -o $@

#gettext:
i18n:
	@$(BIN_XGETTEXT) $(foreach dir, $(DIRS) $(LIBS) $(OPTLIBS), -D $(dir)) \
		-p ./$(PO_DIR) --from-code=UTF-8 --no-wrap --omit-header -i -F \
		--copyright-holder="$(COPYRIGHT)" --keyword=_ --keyword=N_ \
		$(foreach file, $(SOURCES) $(HEADERS) $(LIBS_SOURCES) $(LIBS_HEADERS) \
		$(LIBS_XSOURCES) $(LIBS_XHEADERS), $(shell basename $(file)))
	@mv -f $(PO_DIR)/messages.po $(PO_DIR)/$(TARGET).pot
# if we want new messages start with english equivalent instead of empty
# create a po to use for updating
#	@cd $(PO_DIR); msginit --input $(TARGET).pot --output en_US.po --no-translator \
#		--locale=en_US >/dev/null 2>/dev/null
#	@cd $(PO_DIR); for i in `ls *.po` ; do \
#		if [ "$$i" != "en_US.po" ] ; then \
#		echo "updating $$i" ; \
#		$(BIN_MSGMERGE) --update --backup=none $$i en_US.po ; \
#		$(BIN_MSGFMT) $$i -o `echo $$i | sed -e s/.po/.mo/` ; \
#		fi \
#	done
#	@rm $(PO_DIR)/en_US.po
	@cd $(PO_DIR); for i in `ls *.po` ; do \
		echo "updating $$i" ; \
		$(BIN_MSGMERGE) --update --backup=none $$i $(TARGET).pot ; \
		$(BIN_MSGFMT) $$i -o `echo $$i | sed -e s/.po/.mo/` ; \
	done

#i18n:	gettext $(MO_FILES)

install_i18n: i18n
	@echo "installing *.mo files to prefix '$(PREFIX)'"
	@cd po; for i in `ls *.mo` ; do \
		mkdir -p $(LOCALE_DIR)/`echo $$i|sed -e s/.mo//`/LC_MESSAGES;\
		install -m 644 $$i $(LOCALE_DIR)/`echo $$i | sed -e s/.mo//`/LC_MESSAGES/$(TARGET).mo ; \
	done

uninstall_i18n:
	@echo "uninstalling *.mo files from prefix '$(PREFIX)'"
	@cd po; for i in `ls *.po` ; do \
		rm -f $(LOCALE_DIR)/`echo $$i | sed -e s/.po//`/LC_MESSAGES/$(TARGET).mo || test -z "" ; \
	done

test:
	@echo "testing with splint.."
	@splint -preproc -weak -warnposix $(LINC) `pkg-config --cflags 'gtk+-2.0'` -I. $(SOURCES)

test2: $(TARGET)
	@echo "testing with valgrind.."
	@valgrind --num-callers=99 --leak-check=yes --leak-check=full --show-reachable=yes ./$(TARGET) -n

help:
	@echo -e "the following make targets are available:\n \
		\t all\n \
		\t plugins\n \
		\t i18n\n \
		\t install,   install_plugins,   install_i18n\n \
		\t uninstall, uninstall_plugins, uninstall_i18n\n \
		\t clean, distclean, clean_plugins\n \
		\t doc, test, test2\n \
		\t deps"

$(OBJECTS_DIR):
	@echo "creating object directories in '$(OBJECTS_DIR)'"
	@mkdir -p $(MKOBJDIRS)

$(OBJECTS_DIR)/$(LIBS):
	@echo "creating plugin-object directories in '$(OBJECTS_DIR)'"
	@mkdir -p $(OBJECTS_DIR)/$(LIBS)
	@mkdir -p $(OBJECTS_DIR)/$(OPTLIBS)

$(DESKTOP_FILE):
	@sed s~@PREFIX@~$(PREFIX)~g po/emelfm2.desktop.in > $(DESKTOP_FILE)

$(BUILD_FILE):
	@echo "updating build info: '$(BUILD_FILE)'"
	@echo "#ifndef __BUILD_H__" > $(BUILD_FILE)
	@echo "#define __BUILD_H__" >> $(BUILD_FILE)
	@echo "#define PROGNAME \"$(PROGNAME)\"" >> $(BUILD_FILE)
	@echo "#define BINNAME \"$(TARGET)\"" >> $(BUILD_FILE)
	@echo "#define VERSION \"$(VERSION)\"" >> $(BUILD_FILE)
	@echo "#define RELEASE \"$(RELEASE)\"" >> $(BUILD_FILE)
	@echo "#define BUILDSTAMP \"$(BUILDSTAMP)\"" >> $(BUILD_FILE)
	@echo "#define BUILDDATE \"$(BUILDDATE)\"" >> $(BUILD_FILE)
	@echo "#define BUILDINFO \"$(BUILDINFO)\"" >> $(BUILD_FILE)
	@echo "#define COPYRIGHT \"$(COPYRIGHT)\"" >> $(BUILD_FILE)
	@echo "#define BUILDOPTS \"$(BUILD_PARMS)\"" >> $(BUILD_FILE)
	@echo "#define PREFIX \"$(PREFIX)\"" >> $(BUILD_FILE)
	@echo "#define LIB_DIR \"$(LIB_DIR)\"" >> $(BUILD_FILE)
	@echo "#define PLUGINS_DIR \"$(PLUGINS_DIR)\"" >> $(BUILD_FILE)
	@echo "#define DOC_DIR \"$(DOC_DIR)\"" >> $(BUILD_FILE)
	@echo "#define ICON_DIR \"$(ICON_DIR)\"" >> $(BUILD_FILE)
	@echo "#define LOCALE_DIR \"$(LOCALE_DIR)\"" >> $(BUILD_FILE)
	@echo "#define E2_DEBUG_LEVEL $(DEBUG_LEVEL)" >> $(BUILD_FILE)

ifeq ($(WITH_LATEST), 1)
	@echo "#define E2_CURRENTLIBS" >> $(BUILD_FILE)
ifeq ($(WITH_TRANSPARENCY), 1)
	@echo "#define E2_COMPOSIT" >> $(BUILD_FILE)
endif
else
ifeq ($(GTK3), 1)
ifeq ($(GTK2), 0)
	@echo "#define E2_MIN_GTK3" >> $(BUILD_FILE)
ifeq ($(WITH_TRANSPARENCY), 1)
	@echo "#define E2_COMPOSIT" >> $(BUILD_FILE)
endif
endif
endif
endif

ifeq ($(XDG_INTEGRATION), 1)
#this is used only to allow make install without parameters
	@echo "#define E2_XDG" >> $(BUILD_FILE)
endif
ifeq ($(WITH_ASSIST),1)
	@echo "#define E2_ASSISTED" >> $(BUILD_FILE)
endif
ifeq ($(WITH_VFS),1)
	@echo "#define E2_VFS" >> $(BUILD_FILE)
endif
ifeq ($(WITH_UDISKS),1)
	@echo "#define E2_DEVKIT" >> $(BUILD_FILE)
else
ifeq ($(WITH_HAL),1)
	@echo "#define E2_HAL" >> $(BUILD_FILE)
endif
endif
ifeq ($(WITH_POLKIT), 1)
	@echo "#define E2_POLKIT" >> $(BUILD_FILE)
endif
ifeq ($(EDITOR_SPELLCHECK),1)
	@echo "#define E2_SPELLCHECK" >> $(BUILD_FILE)
endif
ifeq ($(WITH_CUSTOMMOUSE),1)
	@echo "#define E2_MOUSECUSTOM" >> $(BUILD_FILE)
endif
ifeq ($(WITH_OUTPUTSTYLES),1)
	@echo "#define E2_OUTPUTSTYLES" >> $(BUILD_FILE)
endif
ifeq ($(EXTRA_BINDINGS),1)
	@echo "#define E2_TRANSIENTBINDINGS" >> $(BUILD_FILE)
endif
ifeq ($(FILES_UTF8ONLY), 1)
	@echo "#define E2_FILES_UTF8ONLY" >> $(BUILD_FILE)
endif
ifeq ($(NEW_COMMAND), 1)
	@echo "#define E2_NEW_COMMAND" >> $(BUILD_FILE)
endif
ifeq ($(WITH_THUMBS), 1)
	@echo "#define E2_THUMBNAILS" >> $(BUILD_FILE)
ifeq ($(WITH_THUMBLIB),1)
	@echo "#define E2_THUMBLIB" >> $(BUILD_FILE)
endif
endif
ifeq ($(WITH_ACL),1)
	@echo "#define E2_ACL" >> $(BUILD_FILE)
endif
ifeq ($(WITH_TRACKER),1)
	@echo "#define E2_TRACKER" >> $(BUILD_FILE)
endif

ifeq ($(DOCS_VERSION), 1)
	@echo "#define E2_VERSIONDOCS" >> $(BUILD_FILE)
endif
	@echo "#define MAIN_HELP \"$(HELPDOC)\"" >> $(BUILD_FILE)
	@echo "#define CFG_HELP \"$(CONFIGDOC)\"" >> $(BUILD_FILE)

ifeq ($(PANES_HORIZONTAL), 1)
	@echo "#define E2_PANES_HORIZONTAL" >> $(BUILD_FILE)
endif

ifeq ($(USE_GAMIN), 1)
	@echo "#define E2_GAMIN" >> $(BUILD_FILE)
else
ifeq ($(USE_INOTIFY), 1)
	@echo "#ifdef __linux__" >> $(BUILD_FILE)
	@echo "#define E2_FAM_INOTIFY" >> $(BUILD_FILE)
	@echo "#endif" >> $(BUILD_FILE)
endif
ifeq ($(USE_KQUEUE), 1)
	@echo "#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(darwin)" >> $(BUILD_FILE)
	@echo "#define E2_FAM_KQUEUE" >> $(BUILD_FILE)
	@echo "#endif" >> $(BUILD_FILE)
endif
ifeq ($(USE_PORTEVENT), 1)
	@echo "#ifdef __solaris__" >> $(BUILD_FILE)
	@echo "#define E2_FAM_PORTEVENT" >> $(BUILD_FILE)
	@echo "#endif" >> $(BUILD_FILE)
endif
endif
ifeq ($(WITH_SYSTEM_ICONS), 1)
#see also gtk 3.10+ check in emelfm2.h
	@echo "#define E2_ADD_STOCKS" >> $(BUILD_FILE)
endif

	@echo "#endif //ndef __BUILD_H__" >> $(BUILD_FILE)

#include dependencies
ifneq ($(MAKECMDGOALS),deps)
ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),gettext)
ifneq ($(MAKECMDGOALS),test)
ifneq ($(MAKECMDGOALS),test2)
ifneq ($(MAKECMDGOALS),help)
ifneq ($(MAKECMDGOALS),doc)
-include $(DEP_FILES)
-include $(LIBS_DEP_FILES)
endif
endif
endif
endif
endif
endif
endif

# DO NOT DELETE
