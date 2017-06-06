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

from types import StringTypes
from zenmapGUI.higwidgets.higboxes import HIGVBox
from zenmapCore.I18N import _

SCANNING = _("Scanning")
CANCELLED = _("Cancelled")

class ScanHostsView(HIGVBox, object):
    def __init__(self, hosts={}, services={}):
        HIGVBox.__init__(self)
        
        self._create_widgets()
        self._connect_widgets()
        self._pack_widgets()
        self._set_scrolled()
        self._set_host_list(hosts)
        self._set_service_list(services)
        
        self._pack_expand_fill(self.main_vbox)

        # Default mode is host mode
        self.host_mode(self.host_mode_button)

        self.host_view.show_all()
        self.service_view.show_all()
    
    def _create_widgets(self):
        # Mode buttons
        self.host_mode_button = gtk.ToggleButton(_("Hosts"))
        self.service_mode_button = gtk.ToggleButton(_("Services"))
        self.buttons_box = gtk.HBox()

        # Main window vbox
        self.main_vbox = HIGVBox()

        # Host list
        self.host_list = gtk.ListStore(str,str)
        self.host_view = gtk.TreeView(self.host_list)
        self.pic_column = gtk.TreeViewColumn(_('OS'))
        self.host_column = gtk.TreeViewColumn(_('Host'))
        self.os_cell = gtk.CellRendererPixbuf()
        self.host_cell = gtk.CellRendererText()

        # Service list
        self.service_list = gtk.ListStore(str)
        self.service_view = gtk.TreeView(self.service_list)
        self.service_column = gtk.TreeViewColumn(_('Service'))
        self.service_cell = gtk.CellRendererText()
        
        self.scrolled = gtk.ScrolledWindow()

    def _pack_widgets(self):
        self.main_vbox.set_spacing(0)
        self.main_vbox.set_border_width(0)
        self.main_vbox._pack_noexpand_nofill(self.buttons_box)
        self.main_vbox._pack_expand_fill(self.scrolled)

        self.host_mode_button.set_active(True)

        self.buttons_box.set_border_width(5)
        self.buttons_box.pack_start(self.host_mode_button)
        self.buttons_box.pack_start(self.service_mode_button)

    def _connect_widgets(self):
        self.host_mode_button.connect("toggled", self.host_mode)
        self.service_mode_button.connect("toggled", self.service_mode)

    def host_mode(self, widget):
        self._remove_scrolled_child()
        if widget.get_active():
            self.service_mode_button.set_active(False)
            self.scrolled.add(self.host_view)
        else:
            self.service_mode_button.set_active(True)

    def service_mode(self, widget):
        self._remove_scrolled_child()
        if widget.get_active():
            self.host_mode_button.set_active(False)
            self.scrolled.add(self.service_view)
        else:
            self.host_mode_button.set_active(True)

    def _remove_scrolled_child(self):
        try:
            child = self.scrolled.get_child()
            self.scrolled.remove(child)
        except:
            pass
    
    def _set_scrolled(self):
        self.scrolled.set_border_width(5)
        self.scrolled.set_size_request(150, -1)
        self.scrolled.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

    def _set_service_list(self, services):
        self.service_view.set_enable_search(True)
        self.service_view.set_search_column(0)

        selection = self.service_view.get_selection()
        selection.set_mode(gtk.SELECTION_MULTIPLE)
        self.service_view.append_column(self.service_column)
        
        self.service_column.set_resizable(True)
        self.service_column.set_sort_column_id(0)
        self.service_column.set_reorderable(True)
        self.service_column.pack_start(self.service_cell, True)
        self.service_column.set_attributes(self.service_cell, text=0)
        
        self.set_services(services)

    def _set_host_list(self, hosts):
        self.host_view.set_enable_search(True)
        self.host_view.set_search_column(1)
        
        selection = self.host_view.get_selection()
        selection.set_mode(gtk.SELECTION_MULTIPLE)
        
        self.host_view.append_column(self.pic_column)
        self.host_view.append_column(self.host_column)
        
        self.host_column.set_resizable(True)
        self.pic_column.set_resizable(True)
        
        self.host_column.set_sort_column_id(1)
        self.pic_column.set_sort_column_id(1)
        
        self.host_column.set_reorderable(True)
        self.pic_column.set_reorderable(True)
        
        self.pic_column.pack_start(self.os_cell, True)
        self.host_column.pack_start(self.host_cell, True)
        
        self.pic_column.set_attributes(self.os_cell, stock_id=0)
        self.pic_column.set_min_width(35)
        self.host_column.set_attributes(self.host_cell, text=1)
        
        self.set_hosts(hosts)
    
    def clear_host_list(self):
        for i in range(len(self.host_list)):
            iter = self.host_list.get_iter_root()
            del(self.host_list[iter])
    
    def clear_service_list(self):
        for i in range(len(self.service_list)):
            iter = self.service_list.get_iter_root()
            del(self.service_list[iter])

    def set_hosts(self, hosts):
        self.hosts = hosts
        self.clear_host_list()

        for host in hosts:
            self.host_list.append ([hosts[host]['stock'], host])

    def set_services(self, services):
        self.services = services
        self.clear_service_list()

        for service in services:
            self.service_list.append([service])
    
    def add_host(self, host):
        for h in host:
            self.host_list.append([host[h]['stock'], h])

    def add_service(self, service):
        if type(service) == type([]):
            for s in service:
                self.service_list.append([s])
        elif type(service) in StringTypes:
            self.service_list.append([service])
    
    def get_action(self, host):
        try: return self.hosts[host]['action']
        except: return False

if __name__ == "__main__":
    w = gtk.Window()
    h = ScanHostsView()
    w.add(h)
    w.show_all()

    gtk.main()
