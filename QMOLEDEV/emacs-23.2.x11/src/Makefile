# Makefile for GNU Emacs.
# Copyright (C) 1985, 1987, 1988, 1993, 1994, 1995, 1999, 2000, 2001, 2002,
#               2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
#               Free Software Foundation, Inc.

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


# Note that this file is edited by msdos/sed1v2.inp for MSDOS.  That
# script may need modifying in sync with changes made here.  Try to
# avoid shell-ism because the DOS build has to use the DOS shell.

# Don't try to replace the cpp processing using autoconf facilities,
# says rms.
# Replacing a particular part of the conditionals to work via autoconf
# is OK.
# Some of the conditionals might be dead now.  Finding them and
# deleting them would be fine.


# Here are the things that we expect ../configure to edit.
# We use $(srcdir) explicitly in dependencies so as not to depend on VPATH.
srcdir=/home/chris/emacs-23.2.x11/src
VPATH=/home/chris/emacs-23.2.x11/src
CC=/usr/bin/gcc
CPP=/usr/bin/gcc -E -no-cpp-precomp
CFLAGS=-g -O2 -Wdeclaration-after-statement -Wno-pointer-sign  
CPPFLAGS=  
LDFLAGS=  
LN_S=ln -s
EXEEXT=
version=23.2
# Substitute an assignment for the MAKE variable, because
# BSD doesn't have it as a default.

# Don't use LIBS.  configure puts stuff in it that either shouldn't be
# linked with Emacs or is duplicated by the cpp stuff below.
# LIBS = -lintl -lncurses -lm -lXext -lXmu -lXt -lSM -lICE -lX11  
LIBOBJS = 

lispsource = ${srcdir}/../lisp/
libsrc = ../lib-src/
etc = ../etc/
oldXMenudir = ../oldXMenu/
lwlibdir = ../lwlib/
lispdir = ../lisp/

# Configuration files for .o files to depend on.
M_FILE = ${srcdir}/m/arm.h
S_FILE = ${srcdir}/s/darwin.h
config_h = config.h $(M_FILE) $(S_FILE)

bootstrap_exe = ../src/bootstrap-emacs${EXEEXT}

OTHER_FILES = 

# ========================== start of cpp stuff =======================
C_SWITCH_SYSTEM=
SHELL=/bin/sh
DEPFLAGS = -MMD -MF deps/$*.d
STARTFILES = pre-crt0.o
ALL_CFLAGS=-Demacs -DHAVE_CONFIG_H $(MYCPPFLAGS) -I. -I${srcdir} -Dtemacs ${CFLAGS_SOUND} ${RSVG_CFLAGS} ${DBUS_CFLAGS} ${GCONF_CFLAGS} ${CFLAGS} ${DEPFLAGS}
ALL_OBJC_CFLAGS=$(ALL_CFLAGS)
.SUFFIXES: .m
.c.o:
	@-test -d deps || mkdir deps
	$(CC) -c $(CPPFLAGS) $(ALL_CFLAGS) $<
.m.o:
	@-test -d deps || mkdir deps
	$(CC) -c $(CPPFLAGS) $(ALL_OBJC_CFLAGS) $<
XMENU_OBJ = xmenu.o
XOBJ= xterm.o xfns.o xselect.o xrdb.o fontset.o xsmfns.o fringe.o image.o xsettings.o xgselect.o
OLDXMENU=${lwlibdir}liblw.a
LIBXMENU= $(OLDXMENU)
LUCID_LIBW= -lXaw
MOTIF_LIBW= -lXm
LIBW=$(LUCID_LIBW)
LIBXTR6 = -lSM -lICE
LIBXT= $(LIBW) -lXmu -lXt $(LIBXTR6) -lXext
X11_LDFLAGS =
LIBX= $(LIBXMENU) $(X11_LDFLAGS) $(LIBXT) -ltiff -ljpeg -lpng -lz -lm -lgif -lXpm -lX11 $(XFT_LIBS)
LIBSOUND=
CFLAGS_SOUND=
RSVG_LIBS=
RSVG_CFLAGS=
LD = $(CC)
TEMACS_LDFLAGS = -prebind -Xlinker -headerpad -Xlinker 690
INTERVALS_H = dispextern.h intervals.h composite.h
GETLOADAVG_LIBS =
FONT_DRIVERS = xfont.o
obj= dispnew.o frame.o scroll.o xdisp.o menu.o $(XMENU_OBJ) window.o charset.o coding.o category.o ccl.o character.o chartab.o cm.o term.o terminal.o xfaces.o $(XOBJ) $(GTK_OBJ) $(DBUS_OBJ) emacs.o keyboard.o macros.o keymap.o sysdep.o buffer.o filelock.o insdel.o marker.o minibuf.o fileio.o dired.o filemode.o cmds.o casetab.o casefiddle.o indent.o search.o regex.o undo.o alloc.o data.o doc.o editfns.o callint.o eval.o floatfns.o fns.o font.o print.o lread.o syntax.o unexmacosx.o bytecode.o process.o callproc.o region-cache.o sound.o atimer.o doprnt.o strftime.o intervals.o textprop.o composite.o md5.o $(MSDOS_OBJ) $(NS_OBJ) $(CYGWIN_OBJ) $(FONT_DRIVERS)
SOME_MACHINE_OBJECTS = dosfns.o msdos.o xterm.o xfns.o xmenu.o xselect.o xrdb.o xsmfns.o fringe.o image.o fontset.o dbusbind.o nsterm.o nsfns.o nsmenu.o nsselect.o nsimage.o nsfont.o w32.o w32console.o w32fns.o w32heap.o w32inevt.o w32menu.o w32proc.o w32reg.o w32select.o w32term.o w32xfns.o $(FONT_DRIVERS)
termcapobj = terminfo.o
widgetobj= widget.o
otherobj= $(termcapobj) lastfile.o $(mallocobj) $(widgetobj) $(LIBOBJS)
lisp= ${lispsource}loaddefs.el ${lispsource}abbrev.elc ${lispsource}buff-menu.elc ${lispsource}button.elc ${lispsource}emacs-lisp/byte-run.elc ${lispsource}composite.elc ${lispsource}cus-face.elc ${lispsource}cus-start.elc ${lispsource}custom.elc ${lispsource}emacs-lisp/backquote.elc ${lispsource}emacs-lisp/lisp-mode.elc ${lispsource}emacs-lisp/lisp.elc ${lispsource}env.elc ${lispsource}faces.elc ${lispsource}files.elc ${lispsource}format.elc ${lispsource}facemenu.elc ${lispsource}mouse.elc ${lispsource}select.elc ${lispsource}scroll-bar.elc ${lispsource}emacs-lisp/float-sup.elc ${lispsource}frame.elc ${lispsource}help.elc ${lispsource}indent.elc ${lispsource}isearch.elc ${lispsource}rfn-eshadow.elc ${lispsource}loadup.el ${lispsource}bindings.elc ${lispsource}emacs-lisp/map-ynp.elc ${lispsource}menu-bar.elc ${lispsource}international/mule.elc ${lispsource}international/mule-conf.elc ${lispsource}international/mule-cmds.elc ${lispsource}international/characters.elc ${lispsource}international/charprop.el ${lispsource}case-table.elc ${lispsource}language/chinese.elc ${lispsource}language/cyrillic.elc ${lispsource}language/indian.elc ${lispsource}language/sinhala.el ${lispsource}language/english.el ${lispsource}language/ethiopic.elc ${lispsource}language/european.elc ${lispsource}language/czech.el ${lispsource}language/slovak.el ${lispsource}language/romanian.el ${lispsource}language/greek.el ${lispsource}language/hebrew.el ${lispsource}language/japanese.el ${lispsource}language/korean.el ${lispsource}language/lao.el ${lispsource}language/cham.el ${lispsource}language/tai-viet.el ${lispsource}language/thai.el ${lispsource}language/tibetan.elc ${lispsource}language/vietnamese.elc ${lispsource}language/misc-lang.el ${lispsource}language/utf-8-lang.el ${lispsource}language/georgian.el ${lispsource}language/khmer.el ${lispsource}language/burmese.el ${lispsource}paths.el ${lispsource}register.elc ${lispsource}replace.elc ${lispsource}simple.elc ${lispsource}minibuffer.elc ${lispsource}startup.elc ${lispsource}subr.elc ${lispsource}term/tty-colors.elc ${lispsource}font-core.elc ${lispsource}emacs-lisp/syntax.elc ${lispsource}font-lock.elc ${lispsource}jit-lock.elc ${lispsource}textmodes/fill.elc ${lispsource}textmodes/page.elc ${lispsource}textmodes/paragraphs.elc ${lispsource}textmodes/text-mode.elc ${lispsource}emacs-lisp/timer.elc ${lispsource}jka-cmpr-hook.elc ${lispsource}vc-hooks.elc ${lispsource}ediff-hook.elc ${lispsource}epa-hook.elc ${lispsource}tooltip.elc ${lispsource}fringe.elc ${lispsource}image.elc ${lispsource}international/fontset.elc ${lispsource}dnd.elc ${lispsource}tool-bar.elc ${lispsource}mwheel.elc ${lispsource}x-dnd.elc ${lispsource}term/common-win.elc ${lispsource}term/x-win.elc ${lispsource}font-setting.elc ${lispsource}widget.elc ${lispsource}window.elc ${lispsource}version.el
shortlisp= ../lisp/loaddefs.el ../lisp/abbrev.elc ../lisp/buff-menu.elc ../lisp/button.elc ../lisp/emacs-lisp/byte-run.elc ../lisp/composite.elc ../lisp/cus-face.elc ../lisp/cus-start.elc ../lisp/custom.elc ../lisp/emacs-lisp/backquote.elc ../lisp/emacs-lisp/lisp-mode.elc ../lisp/emacs-lisp/lisp.elc ../lisp/facemenu.elc ../lisp/faces.elc ../lisp/files.elc ../lisp/emacs-lisp/float-sup.elc ../lisp/format.elc ../lisp/frame.elc ../lisp/help.elc ../lisp/indent.elc ../lisp/isearch.elc ../lisp/rfn-eshadow.elc ../lisp/loadup.el ../lisp/bindings.elc ../lisp/emacs-lisp/map-ynp.elc ../lisp/env.elc ../lisp/international/mule.elc ../lisp/international/mule-conf.elc ../lisp/international/mule-cmds.elc ../lisp/international/characters.elc ../lisp/case-table.elc ../lisp/language/chinese.elc ../lisp/language/cyrillic.elc ../lisp/language/indian.elc ../lisp/language/sinhala.el ../lisp/language/english.el ../lisp/language/ethiopic.elc ../lisp/language/european.elc ../lisp/language/czech.el ../lisp/language/slovak.el ../lisp/language/romanian.el ../lisp/language/greek.el ../lisp/language/hebrew.el ../lisp/language/japanese.el ../lisp/language/korean.el ../lisp/language/lao.el ../lisp/language/cham.el ../lisp/language/tai-viet.el ../lisp/language/thai.el ../lisp/language/tibetan.elc ../lisp/language/vietnamese.elc ../lisp/language/misc-lang.el ../lisp/language/utf-8-lang.el ../lisp/language/georgian.el ../lisp/language/khmer.el ../lisp/language/burmese.el ../lisp/menu-bar.elc ../lisp/paths.el ../lisp/register.elc ../lisp/replace.elc ../lisp/simple.elc ../lisp/minibuffer.elc ../lisp/startup.elc ../lisp/subr.elc ../lisp/term/tty-colors.elc ../lisp/font-core.elc ../lisp/emacs-lisp/syntax.elc ../lisp/font-lock.elc ../lisp/jit-lock.elc ../lisp/textmodes/fill.elc ../lisp/textmodes/page.elc ../lisp/textmodes/paragraphs.elc ../lisp/textmodes/text-mode.elc ../lisp/emacs-lisp/timer.elc ../lisp/vc-hooks.elc ../lisp/jka-cmpr-hook.elc ../lisp/ediff-hook.elc ../lisp/epa-hook.elc ../lisp/widget.elc ../lisp/window.elc ../lisp/version.el
SOME_MACHINE_LISP = ../lisp/mouse.elc ../lisp/select.elc ../lisp/scroll-bar.elc ../lisp/ls-lisp.elc ../lisp/dos-fns.elc ../lisp/w32-fns.elc ../lisp/dos-w32.elc ../lisp/disp-table.elc ../lisp/dos-vars.elc ../lisp/tooltip.elc ../lisp/image.elc ../lisp/fringe.elc ../lisp/dnd.elc ../lisp/mwheel.elc ../lisp/tool-bar.elc ../lisp/x-dnd.elc ../lisp/font-setting.elc ../lisp/international/ccl.elc ../lisp/international/fontset.elc ../lisp/mouse.elc ../lisp/term/common-win.elc ../lisp/term/x-win.elc ../lisp/term/pc-win.elc ../lisp/term/internal.elc ../lisp/term/ns-win.elc ../lisp/term/w32-win.elc ../lisp/emacs-lisp/easymenu.elc
LIBES = $(LOADLIBES) $(LIBS) $(LIBX) $(LIBSOUND) $(RSVG_LIBS) $(DBUS_LIBS) -lncurses $(GETLOADAVG_LIBS) ${GCONF_LIBS} $(GNULIB_VAR) $(GNULIB_VAR)
RUN_TEMACS = `/bin/pwd`/temacs
all: emacs${EXEEXT} $(OTHER_FILES)
emacs${EXEEXT}: temacs${EXEEXT} ${etc}DOC ${lisp}
	LC_ALL=C $(RUN_TEMACS) -batch -l loadup dump
	@: This new Emacs is as functional and more efficient then
	@: bootstrap-emacs, so let us replace it.
	-ln -f emacs${EXEEXT} bootstrap-emacs${EXEEXT}
	-./emacs -q -batch -f list-load-path-shadows
${etc}DOC: ${libsrc}make-docfile${EXEEXT} ${obj} ${lisp} ${SOME_MACHINE_LISP}
	-rm -f ${etc}DOC
	${libsrc}make-docfile -d ${srcdir} ${SOME_MACHINE_OBJECTS} ${obj} > ${etc}DOC
	${libsrc}make-docfile -a ${etc}DOC -d ${srcdir} ${SOME_MACHINE_LISP} ${shortlisp}
${libsrc}make-docfile${EXEEXT}:
	cd ${libsrc}; ${MAKE} ${MFLAGS} make-docfile${EXEEXT}
buildobj.h: Makefile
	echo "#define BUILDOBJ \"${obj} ${otherobj} " "\"" > buildobj.h
temacs${EXEEXT}: $(LOCALCPP) $(STARTFILES) stamp-oldxmenu ${obj} ${otherobj} prefix-args${EXEEXT}
	$(LD) ${STARTFLAGS} ${TEMACS_LDFLAGS} $(LDFLAGS) -o temacs ${STARTFILES} ${obj} ${otherobj} ${LIBES}
prefix-args${EXEEXT}: prefix-args.o $(config_h)
	$(CC) $(LDFLAGS) prefix-args.o -o prefix-args
stamp-oldxmenu: ${OLDXMENU} ../src/$(OLDXMENU)
	touch stamp-oldxmenu
../src/$(OLDXMENU): ${OLDXMENU}
C_SWITCH_MACHINE_1 =
C_SWITCH_SYSTEM_1 =
C_SWITCH_X_SITE_1 =
C_SWITCH_X_MACHINE_1 =
C_SWITCH_X_SYSTEM_1 =
$(OLDXMENU): really-lwlib
really-lwlib:
	cd ${lwlibdir}; ${MAKE} ${MFLAGS} CC='${CC}' CFLAGS='${CFLAGS}' MAKE='${MAKE}' "C_SWITCH_X_SITE=$(C_SWITCH_X_SITE_1)" "C_SWITCH_X_MACHINE=$(C_SWITCH_X_MACHINE_1)" "C_SWITCH_X_SYSTEM=$(C_SWITCH_X_SYSTEM_1)" "C_SWITCH_MACHINE=$(C_SWITCH_MACHINE_1)" "C_SWITCH_SYSTEM=$(C_SWITCH_SYSTEM_1)"
	@true
.PHONY: really-lwlib
../config.status:: epaths.in
	@echo "The file epaths.h needs to be set up from epaths.in."
	@echo "Please run the `configure' script again."
	exit 1
../config.status:: config.in
	@echo "The file config.h needs to be set up from config.in."
	@echo "Please run the `configure' script again."
	exit 1
ecrt0.o: ecrt0.c $(config_h)
	@-test -d deps || mkdir deps
	CRT0_COMPILE ${srcdir}/ecrt0.c
doc.o: buildobj.h
mostlyclean:
	rm -f temacs${EXEEXT} prefix-args${EXEEXT} core *.core \#* *.o libXMenu11.a liblw.a
	rm -f ../etc/DOC
	rm -f bootstrap-emacs${EXEEXT} emacs-${version}${EXEEXT}
	rm -f buildobj.h
clean: mostlyclean
	rm -f emacs-*.*.*${EXEEXT} emacs${EXEEXT}
	-rm -rf deps
bootstrap-clean: clean
	rm -f epaths.h config.h Makefile.c config.stamp stamp-oldxmenu ../etc/DOC-*
	if test -f ./.gdbinit; then mv ./.gdbinit ./.gdbinit.save; if test -f "${srcdir}/.gdbinit"; then rm -f ./.gdbinit.save; else mv ./.gdbinit.save ./.gdbinit; fi; fi
distclean: bootstrap-clean
	rm -f Makefile
maintainer-clean: distclean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."
	rm -f TAGS
versionclean:
	-rm -f emacs${EXEEXT} emacs-*.*.*${EXEEXT} ../etc/DOC*
extraclean: distclean
	-rm -f *~ \#* m/?*~ s/?*~
ctagsfiles1 = [xyzXYZ]*.[hcm]
ctagsfiles2 = [a-wA-W]*.[hcm]
TAGS: $(srcdir)/$(ctagsfiles1) $(srcdir)/$(ctagsfiles2)
	../lib-src/etags --include=TAGS-LISP --include=${lwlibdir}/TAGS --regex='/[ 	]*DEFVAR_[A-Z_ 	(]+"\([^"]+\)"/' $(srcdir)/$(ctagsfiles1) $(srcdir)/$(ctagsfiles2)
frc:
TAGS-LISP: frc
	$(MAKE) -f ${lispdir}Makefile TAGS-LISP ETAGS=../lib-src/etags
$(lwlibdir)TAGS:
	(cd $(lwlibdir); $(MAKE) -f $(lwlibdir)Makefile tags ETAGS=../lib-src/etags)
tags: TAGS TAGS-LISP $(lwlibdir)TAGS
.PHONY: tags
.SUFFIXES: .elc .el
.el.elc:
	@cd ../lisp; $(MAKE) $(MFLAGS) compile-onefile THEFILE=$< EMACS=${bootstrap_exe}
${lisp} ${SOME_MACHINE_LISP}: $(BOOTSTRAPEMACS)
${lispsource}loaddefs.el: $(BOOTSTRAPEMACS)
	cd ../lisp; $(MAKE) $(MFLAGS) autoloads EMACS=${bootstrap_exe}
bootstrap-emacs${EXEEXT}: temacs${EXEEXT}
	cd ../lisp; $(MAKE) $(MFLAGS) update-subdirs
	$(RUN_TEMACS) --batch --load loadup bootstrap
	mv -f emacs${EXEEXT} bootstrap-emacs${EXEEXT}
	@: Compile some files earlier to speed up further compilation.
	cd ../lisp; $(MAKE) $(MFLAGS) compile-first EMACS=${bootstrap_exe}
ALLOBJS=$(STARTFILES) ${obj} ${otherobj} prefix-args.o
-include $(ALLOBJS:%.o=deps/%.d)
