# $Id: Makefile.am 24877 2006-08-18 17:41:19Z mej $

lib_LTLIBRARIES = libEterm.la
bin_PROGRAMS = Eterm

MMX_SRCS = mmx_cmod.S
MMX_OBJS = mmx_cmod.lo

SSE2_SRCS = sse2_cmod.c
SSE2_OBJS = sse2_cmod.lo

libEterm_la_SOURCES = actions.c actions.h buttons.c buttons.h command.c			\
                      command.h draw.c draw.h e.c e.h eterm_debug.h eterm_utmp.h	\
                      events.c events.h feature.h font.c font.h grkelot.c		\
                      grkelot.h icon.h menus.c menus.h misc.c misc.h			\
                      options.c options.h pixmap.c pixmap.h profile.h screen.c		\
                      screen.h script.c script.h scrollbar.c scrollbar.h		\
                      startup.c startup.h system.c system.h term.c term.h		\
                      timer.c timer.h utmp.c windows.c windows.h defaultfont.c		\
                      defaultfont.h libscream.c scream.h screamcfg.h

EXTRA_libEterm_la_SOURCES = $(MMX_SRCS) $(SSE2_SRCS)

libEterm_la_LDFLAGS = -release $(VERSION)
if HAVE_SSE2
libEterm_la_DEPENDENCIES = feature.h $(SSE2_OBJS)
libEterm_la_LIBADD = $(SSE2_OBJS)
else
if HAVE_MMX
libEterm_la_DEPENDENCIES = feature.h $(MMX_OBJS)
libEterm_la_LIBADD = $(MMX_OBJS)
else
libEterm_la_DEPENDENCIES = feature.h
endif
endif

Eterm_SOURCES = main.c
Eterm_DEPENDENCIES = libEterm.la
Eterm_LDFLAGS = -rpath $(libdir):$(pkglibdir)
Eterm_LDADD = libEterm.la 

EXTRA_DIST = gdb.scr mmx_cmod.S sse2_cmod.c

install-exec-hook:
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)
	test ! -z "$(GDB_CMD_FILE)" && $(INSTALL_DATA) $(srcdir)/gdb.scr $(DESTDIR)$(pkgdatadir)/ || :
MAINTAINERCLEANFILES=Makefile.in
