VTEINC=-D_REENTRANT -I/usr/local/include/glib-2.0 -I/usr/local/lib/glib-2.0/include -I/usr/local/include/pango-1.0 -I/usr/local/include/gtk-2.0 -I/usr/local/include/gio-unix-2.0/ -I/usr/local/include/cairo -I/usr/local/lib/gtk-2.0/include -I/usr/local/include/atk-1.0 -I/usr/local/include/gdk-pixbuf-2.0 -I/usr/local/include/pixman-1 -I/usr/local/include/libpng14  
LIBS_VTE=-lgdk-x11-2.0 -lgtk-x11-2.0 -lvte
LIBS_VTE_L=-L/usr/local/lib  
prefix=/usr/local
bindir=$(DESTDIR)/usr/local/bin
mandir=$(DESTDIR)/usr/local/share/man/man1
deskdir=$(DESTDIR)/usr/local/share/applications
#define ICON_DIR "/usr/local/share/pixmaps"
ICON_DIR_INSTALL=$(DESTDIR)/usr/local/share/pixmaps
THEME_DIR=$(DESTDIR)/usr/local/share/icons
GNOME_DEF_APP=$(DESTDIR)/usr/local/share/gnome-control-center/default-apps
PROG=ivte
#ifndef PROGRAM_VERSION
#define PROGRAM_VERSION "0.5.2~pre1"
#endif
CONF_FILE=src/config.h
