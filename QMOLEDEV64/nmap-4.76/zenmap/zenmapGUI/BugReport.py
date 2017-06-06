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
import webbrowser

from zenmapGUI.higwidgets.higboxes import HIGVBox

from zenmapCore.Name import APP_DISPLAY_NAME, NMAP_DISPLAY_NAME, NMAP_WEB_SITE
from zenmapCore.I18N import _

# For escaping text in marked-up labels.
from xml.sax.saxutils import escape

class BugReport(gtk.Window, object):
    def __init__(self):
        gtk.Window.__init__(self)
        self.set_title(_('How to Report a Bug'))
        self.set_position(gtk.WIN_POS_CENTER_ALWAYS)
        
        self._create_widgets()
        self._pack_widgets()
        self._connect_widgets()

    def _create_widgets(self):
        self.vbox = HIGVBox()
        self.button_box = gtk.HButtonBox()
        
        self.text = gtk.Label()

        self.btn_ok = gtk.Button(stock=gtk.STOCK_OK)

    def _pack_widgets(self):
        self.vbox.set_border_width(6)
        
        self.text.set_line_wrap(True)
        self.text.set_markup("""\
<big><b>How to report a bug</b></big>

Like their author, %(nmap)s and %(app)s aren't perfect. But you can help \
make it better by sending bug reports or even writing patches. If \
%(nmap)s doesn't behave the way you expect, first upgrade to the latest \
version available from <b>%(nmap_web)s</b>. If the problem persists, do \
some research to determine whether it has already been discovered and \
addressed. Try Googling the error message or browsing the nmap-dev \
archives at http://seclists.org/. Read the full manual page as well. If \
nothing comes of this, mail a bug report to \
<b>&lt;nmap-dev@insecure.org&gt;</b>. Please include everything you have \
learned about the problem, as well as what version of Nmap you are \
running and what operating system version it is running on. Problem \
reports and %(nmap)s usage questions sent to nmap-dev@insecure.org are \
far more likely to be answered than those sent to Fyodor directly.

Code patches to fix bugs are even better than bug reports. Basic \
instructions for creating patch files with your changes are available at \
http://nmap.org/data/HACKING. Patches may be sent to nmap-dev \
(recommended) or to Fyodor directly.
""" % {"app": escape(APP_DISPLAY_NAME), "nmap": escape(NMAP_DISPLAY_NAME), "nmap_web": escape(NMAP_WEB_SITE)})
        self.vbox.add(self.text)

        self.button_box.set_layout(gtk.BUTTONBOX_END)
        self.button_box.pack_start(self.btn_ok)
        
        self.vbox._pack_noexpand_nofill(self.button_box)
        self.add(self.vbox)

    def _connect_widgets(self):
        self.btn_ok.connect("clicked", self.close)
        self.connect("delete-event", self.close)

    def close(self, widget=None, event=None):
        self.destroy()

if __name__ == "__main__":
    w = BugReport()
    w.show_all()
    w.connect("delete-event", lambda x, y: gtk.main_quit())
    
    gtk.main()
