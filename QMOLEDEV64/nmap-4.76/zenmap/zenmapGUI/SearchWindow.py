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

from zenmapGUI.SearchGUI import SearchGUI

from zenmapCore.I18N import _
from zenmapCore.UmitConf import is_maemo

from zenmapGUI.higwidgets.higboxes import HIGVBox
from zenmapGUI.higwidgets.higbuttons import HIGButton

BaseSearchWindow = None
hildon = None

if is_maemo():
    import hildon
    class BaseSearchWindow(hildon.Window):
        def __init__(self):
            hildon.Window.__init__(self)

        def _pack_widgets(self):
            pass
else:
    class BaseSearchWindow(gtk.Window):
        def __init__(self):
            gtk.Window.__init__(self)
            self.set_title(_("Search Scans"))
            self.set_position(gtk.WIN_POS_CENTER)

        def _pack_widgets(self):
            self.vbox.set_border_width(4)

class SearchWindow(BaseSearchWindow, object):
    def __init__(self, load_method, append_method):
        BaseSearchWindow.__init__(self)
        
        self.set_default_size(600, 400)

        self.load_method = load_method
        self.append_method = append_method

        self._create_widgets()
        self._pack_widgets()
        self._connect_widgets()

    def _create_widgets(self):
        self.vbox = HIGVBox()
        
        self.bottom_hbox = gtk.HBox()
        self.bottom_label = gtk.Label()
        self.btn_box = gtk.HButtonBox()
        self.btn_open = HIGButton(stock=gtk.STOCK_OPEN)
        self.btn_append = HIGButton("Append", gtk.STOCK_ADD)
        self.btn_close = HIGButton(stock=gtk.STOCK_CLOSE)
        
        self.search_gui = SearchGUI(self)

    def _pack_widgets(self):
        BaseSearchWindow._pack_widgets(self)
        
        self.btn_box.set_layout(gtk.BUTTONBOX_END)
        self.btn_box.set_spacing(4)
        self.btn_box.pack_start(self.btn_close)
        self.btn_box.pack_start(self.btn_append)
        self.btn_box.pack_start(self.btn_open)
        
        self.bottom_label.set_alignment(0.0, 0.5)
        self.bottom_label.set_use_markup(True)
        
        self.bottom_hbox.set_spacing(4)
        self.bottom_hbox.pack_start(self.bottom_label, True)
        self.bottom_hbox.pack_start(self.btn_box, False)
        
        self.vbox.set_spacing(4)
        self.vbox.pack_start(self.search_gui, True, True)
        self.vbox.pack_start(self.bottom_hbox, False)
        
        self.add(self.vbox)

    def _connect_widgets(self):
        # Double click on result, opens it
        self.search_gui.result_view.connect("row-activated", self.open_selected)
        
        self.btn_open.connect("clicked", self.open_selected)
        self.btn_append.connect("clicked", self.append_selected)
        self.btn_close.connect("clicked", self.close)
        self.connect("delete-event", self.close)

    def close(self, widget=None, event=None):
        self.search_gui.close()
        self.destroy()
    
    def set_label_text(self, text):
        self.bottom_label.set_label(text)
    
    def open_selected(self, widget=None, path=None, view_column=None, extra=None):
        # Open selected results
        self.load_method(self.results)

        # Close Search Window
        self.close()
    
    def append_selected(self, widget=None, path=None, view_column=None, extra=None):
        # Append selected results
        self.append_method(self.results)

        # Close Search Window
        self.close()

    def get_results(self):
        # Return list with parsed objects from result list store
        return self.search_gui.selected_results

    results = property(get_results)


if __name__ == "__main__":
    search = SearchWindow(lambda x: gtk.main_quit())
    search.show_all()
    gtk.main()
