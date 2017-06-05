# Copyright (C) 2009 Timothy Richard Musson

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


SUBDIRS = doc po


install-data-hook: update-icon-cache
uninstall-hook: update-icon-cache


AM_CPPFLAGS = -DPACKAGE_DATA_DIR=\"$(datadir)\"           \
              -DPACKAGE_LOCALE_DIR=\"$(datadir)/locale\"  \
              $(PACKAGE_CFLAGS)

bin_PROGRAMS = gxmessage

gxmessage_SOURCES = gxmessage.c

gxmessage_LDADD = $(PACKAGE_LIBS)

icondir       = $(datadir)/icons/hicolor/48x48/apps
icon_DATA     = gxmessage.png

# svgicondir    = $(datadir)/icons/hicolor/scalable/apps
# svgicon_DATA  = gxmessage.svg

EXTRA_DIST    = $(icon_DATA)                  \
                COPYING.icon                  \
                HACKING                       \
                examples/gtkrc                \
                examples/gxaddress            \
                examples/gxdialup             \
                examples/gxdict               \
                examples/gxman                \
                examples/gxview               \
                examples/nautilus-word-count


#
# GTK icon cache
#

gtk_update_icon_cache = gtk-update-icon-cache -f -t $(datadir)/icons/hicolor

update-icon-cache:
	if test -z "$(DESTDIR)"; then \
		echo "Updating Gtk icon cache."; \
		$(gtk_update_icon_cache); \
	else \
		echo "*** Icon cache not updated.  After (un)install, run this:"; \
		echo "***   $(gtk_update_icon_cache)"; \
	fi

