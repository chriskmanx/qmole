#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
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
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

import gtk
import gobject

from zenmapCore.Version import VERSION

class Splash(gtk.Window):
    def __init__(self, image, time=1700):
        gtk.Window.__init__(self, gtk.WINDOW_POPUP)
        self.set_position(gtk.WIN_POS_CENTER)

        splash_img = gtk.gdk.pixbuf_new_from_file(image)
        pixmap, mask = splash_img.render_pixmap_and_mask()
        width, height = pixmap.get_size()
        del splash_img

        self.set_app_paintable(True)
        self.set_size_request(width, height)
        self.set_resizable(False)
        self.realize()

        self.verbox = gtk.VBox()
        self.version = gtk.Label("%s" % VERSION)

        self.version.set_use_markup(True)
        self.version.set_markup("<span size='16000' weight='heavy'>\
%s</span>" % VERSION)

        self.verbox.pack_start(self.version, True, False)
        self.verbox.set_size_request(-1, 56)
        self.verbox.show_all()

        fixed = gtk.Fixed()
        # These constants are derived from the dimensions of the open space in
        # the splash graphic. We attempt to center the version number.
        fixed.put(self.verbox, width - 75 - self.verbox.size_request()[0] / 2, height - 56)
        self.add(fixed)

        self.hid = self.connect("expose-event", self.set_bg, mask, pixmap)
        self.set_bg(self, None, mask, pixmap)
        self.show_all()

        while gtk.events_pending():
            gtk.main_iteration()
        gobject.timeout_add(time, self.destroy)

    def destroy(self):
        gtk.Window.destroy(self)
        return False

    def set_bg(self, widget, event, mask, pixmap):
        if self.window != None:
            self.window.set_back_pixmap(pixmap, False)
        else:
            gobject.idle_add(self.set_bg, widget, event, mask, pixmap)

if __name__ == "__main__":
    from os.path import join
    s = Splash(join(".", "share", "pixmaps", "splash.png"))
    gtk.main()
