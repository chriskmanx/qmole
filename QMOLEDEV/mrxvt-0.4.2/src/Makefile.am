#
# All portions of code are copyright by their respective author/s.
# Copyright (c) 2002     Alexis <materm@tuxfamily.org>
# Copyright (c) 2004     Jingmin Zhou <jimmyzhou@users.sourceforge.net>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#
# $Id: Makefile.am,v 1.15 2004/11/20 07:20:38 cvs Exp $
#

bin_PROGRAMS = mrxvt
mrxvt_SOURCES = command.c init.c menubar.c strings.c encoding.c \
	misc.c ptytty.c tabbar.c netdisp.c rxvt.c xdefaults.c main.c \
	pixmap.c screen.c scrollbar.c \
	command.h feature.h init.h menubar.h protos.h rxvtgrx.h \
	encoding.h rxvtlib.h rxvt.h \
	left.xpm left_d.xpm right.xpm right_d.xpm \
	close_term.xpm close_term_d.xpm term.xpm term_d.xpm \
	left.xbm right.xbm close_term.xbm term.xbm 

#install-exec-hook:
#	-chown root $(DESTDIR)$(bindir)/mrxvt
#	-chmod go-r $(DESTDIR)$(bindir)/mrxvt
#	-chmod u+s $(DESTDIR)$(bindir)/mrxvt

if USE_JPEG
JPEG_SRC = mjpg.c
else
JPEG_SRC = 
endif

if USE_PNG
PNG_SRC = mpng.c
else
PNG_SRC = 
endif

if PLAIN_SCROLLBAR
SCROLLBAR_PLAIN_SRC = scrollbar-plain.c
else
SCROLLBAR_PLAIN_SRC = 
endif

if XTERM_SCROLLBAR
SCROLLBAR_XTERM_SRC = scrollbar-xterm.c
else
SCROLLBAR_XTERM_SRC = 
endif

if NEXT_SCROLLBAR
SCROLLBAR_NEXT_SRC = scrollbar-next.c
else
SCROLLBAR_NEXT_SRC = 
endif

if RXVT_SCROLLBAR
SCROLLBAR_RXVT_SRC = scrollbar-rxvt.c
else
SCROLLBAR_RXVT_SRC = 
endif

if SGI_SCROLLBAR
SCROLLBAR_SGI_SRC = scrollbar-sgi.c
else
SCROLLBAR_SGI_SRC = 
endif

if UTMP_SUPPORT
UTMP_SRC = logging.c
else
UTMP_SRC =
endif

if GREEK
GREEK_SRC = grkelot.c grkelot.h
else
GREEK_SRC = 
endif

if THAI
THAI_SRC = thai.c
else
THAI_SRC = 
endif

if SESSION_SUPPORT
SESSION_SRC = session.c
else
SESSION_SRC = 
endif


mrxvt_SOURCES += $(SCROLLBAR_PLAIN_SRC) $(SCROLLBAR_XTERM_SRC) $(SCROLLBAR_NEXT_SRC) $(SCROLLBAR_RXVT_SRC) $(SCROLLBAR_SGI_SRC) $(UTMP_SRC) $(GREEK_SRC) $(THAI_SRC) $(JPEG_SRC) $(PNG_SRC) $(SESSION_SRC)
MRXVT_H = ../config.h rxvtlib.h rxvt.h feature.h protos.h
MRXVT_PIXMAPS = left.xpm left_d.xpm right.xpm right_d.xpm \
	close_term.xpm close_term_d.xpm term.xpm term_d.xpm \
	left.xbm right.xbm close_term.xbm term.xbm 
AUTOMAKE_OPTIONS = no-dependencies

# set dependencies
$(bin_PROGRAMS) : 
command.o:			$(MRXVT_H) command.c command.h
encoding.o:			$(MRXVT_H) encoding.c encoding.h
grkelot.o:			$(MRXVT_H) grkelot.c grkelot.h
init.o:				$(MRXVT_H) init.c init.h
mjpg.o:				$(MRXVT_H) mjpg.c
logging.o:			$(MRXVT_H) logging.c
main.o:				$(MRXVT_H) main.c
menubar.o:			$(MRXVT_H) menubar.c menubar.h
misc.o:				$(MRXVT_H) misc.c
netdisp.o:			$(MRXVT_H) netdisp.c
pixmap.o:			$(MRXVT_H) pixmap.c
mpng.o:				$(MRXVT_H) mpng.c
ptytty.o:			$(MRXVT_H) ptytty.c
rxvt.o:				$(MRXVT_H) rxvt.c
screen.o:			$(MRXVT_H) screen.c
scrollbar-next.o:	$(MRXVT_H) scrollbar-next.c
scrollbar-rxvt.o:	$(MRXVT_H) scrollbar-rxvt.c
scrollbar-xterm.o:	$(MRXVT_H) scrollbar-xterm.c
scrollbar-plain.o:	$(MRXVT_H) scrollbar-plain.c
scrollbar-sgi.o:	$(MRXVT_H) scrollbar-sgi.c
scrollbar.o:		$(MRXVT_H) scrollbar.c
strings.o:			$(MRXVT_H) strings.c
session.o:			$(MRXVT_H) session.c
thai.o:				$(MRXVT_H) thai.c
tabbar.o:			$(MRXVT_H) $(MRXVT_PIXMAPS) tabbar.c
xdefaults.o:		$(MRXVT_H) xdefaults.c
