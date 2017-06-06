#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
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
import pango

from zenmapGUI.higwidgets.higboxes import HIGHBox, HIGVBox
from zenmapGUI.higwidgets.higbuttons import HIGButton
from zenmapGUI.higwidgets.higscrollers import HIGScrolledWindow
from zenmapCore.I18N import _

def status_data_func(widget, cell_renderer, model, iter):
    entry = model.get_value(iter, 0)
    if entry.running:
        status = "Running"
    elif entry.finished:
        if entry.parsed.unsaved:
            status = "Unsaved"
        else:
            status = ""
    elif entry.failed:
        status = "Failed"
    elif entry.canceled:
        status = "Canceled"
    cell_renderer.set_property("text", status)

def command_data_func(widget, cell_renderer, model, iter):
    entry = model.get_value(iter, 0)
    cell_renderer.set_property("ellipsize", pango.ELLIPSIZE_END)
    cell_renderer.set_property("text", entry.get_command_string())

class ScanScanListPage(HIGVBox):
    """This is the "Scans" scan results tab. It the list of running and finished
    scans contained in the ScansListStore passed to the constructor."""
    def __init__(self, scans_store):
        HIGVBox.__init__(self)

        self.set_spacing(4)

        scans_store.connect("row-changed", self._row_changed)

        self.scans_list = gtk.TreeView(scans_store)
        self.scans_list.get_selection().connect("changed", self._selection_changed)

        status_col = gtk.TreeViewColumn("Status")
        cell = gtk.CellRendererText()
        status_col.pack_start(cell)
        status_col.set_cell_data_func(cell, status_data_func)
        self.scans_list.append_column(status_col)

        command_col = gtk.TreeViewColumn("Command")
        cell = gtk.CellRendererText()
        command_col.pack_start(cell)
        command_col.set_cell_data_func(cell, command_data_func)
        self.scans_list.append_column(command_col)

        scrolled_window = HIGScrolledWindow()
        scrolled_window.set_border_width(0)
        scrolled_window.add(self.scans_list)

        self.pack_start(scrolled_window, True, True)

        hbox = HIGHBox()
        buttonbox = gtk.HButtonBox()
        buttonbox.set_layout(gtk.BUTTONBOX_START)
        buttonbox.set_spacing(4)

        self.append_button = HIGButton(_("Append Scan"), gtk.STOCK_ADD)
        buttonbox.pack_start(self.append_button, False)

        self.remove_button = HIGButton(_("Remove Scan"), gtk.STOCK_REMOVE)
        buttonbox.pack_start(self.remove_button, False)

        self.cancel_button = HIGButton(_("Cancel Scan"), gtk.STOCK_CANCEL)
        buttonbox.pack_start(self.cancel_button, False)

        hbox.pack_start(buttonbox, padding = 4)

        self.pack_start(hbox, False, padding = 4)

        self._update()

    def _row_changed(self, model, path, i):
        self._update()

    def _selection_changed(self, selection):
        self._update()

    def _update(self):
        # Make the Cancel button sensitive or not depending on whether a running
        # scan is selected.
        model, selection = self.scans_list.get_selection().get_selected_rows()
        for path in selection:
            entry = model.get_value(model.get_iter(path), 0)
            if entry.running:
                self.cancel_button.set_sensitive(True)
                break
        else:
            self.cancel_button.set_sensitive(False)

        if len(selection) == 0:
            self.remove_button.set_sensitive(False)
        else:
            self.remove_button.set_sensitive(True)
