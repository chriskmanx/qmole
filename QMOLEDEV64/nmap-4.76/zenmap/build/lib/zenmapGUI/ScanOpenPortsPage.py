#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005,2008 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
# Modified: Jurand Nogiec <jurand@jurand.net>, 2008
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

from zenmapGUI.higwidgets.higboxes import HIGVBox, HIGHBox
from zenmapGUI.higwidgets.higtables import HIGTable

from zenmapCore.UmitLogging import log
from zenmapCore.I18N import _

class ScanOpenPortsPage(gtk.ScrolledWindow):
    def __init__(self):
        gtk.ScrolledWindow.__init__(self)
        self.set_policy(gtk.POLICY_AUTOMATIC,gtk.POLICY_AUTOMATIC)
        
        self.__create_widgets()
        
        self.add_with_viewport(self.host)

    def __create_widgets(self):
        self.host = HostOpenPorts()

class HostOpenPorts(HIGVBox):
    def __init__(self):
        HIGVBox.__init__(self)
        
        self._create_widgets()
        self._set_port_list()
        self._set_host_list()
        self._pack_widgets()

    def _create_widgets(self):
        # Ports view
        self.port_columns = {}
        self.port_list = gtk.ListStore(str, str, int, str, str, str, str)
        self.port_tree = gtk.TreeStore(str, str, int, str, str, str, str)
        
        self.port_view = gtk.TreeView(self.port_list)
        
        self.cell_icon = gtk.CellRendererPixbuf()
        self.cell_port = gtk.CellRendererText()
        
        self.port_columns['hostname'] = gtk.TreeViewColumn(_('Host'))
        self.port_columns['icon'] = gtk.TreeViewColumn('')
        self.port_columns['port_number'] = gtk.TreeViewColumn(_('Port'))
        self.port_columns['protocol'] = gtk.TreeViewColumn(_('Protocol'))
        self.port_columns['state'] = gtk.TreeViewColumn(_('State'))
        self.port_columns['service'] = gtk.TreeViewColumn(_('Service'))
        self.port_columns['version'] = gtk.TreeViewColumn(_('Version'))

        # Host services view
        self.host_columns = {}
        self.host_list = gtk.ListStore(str, str, str, int, str, str, str)
        self.host_tree = gtk.TreeStore(str, str, str, int, str, str, str)
        
        self.host_view = gtk.TreeView(self.host_list)
        
        self.cell_host_icon = gtk.CellRendererPixbuf()
        self.cell_host = gtk.CellRendererText()
        
        self.host_columns['service'] = gtk.TreeViewColumn(_('Service'))
        self.host_columns['hostname'] = gtk.TreeViewColumn(_('Hostname'))
        self.host_columns['icon'] = gtk.TreeViewColumn('')
        self.host_columns['protocol'] = gtk.TreeViewColumn(_('Protocol'))
        self.host_columns['port_number'] = gtk.TreeViewColumn(_('Port'))
        self.host_columns['state'] = gtk.TreeViewColumn(_('State'))
        self.host_columns['version'] = gtk.TreeViewColumn(_('Version'))
        
        self.scroll_ports_hosts = gtk.ScrolledWindow()
        self.expander_sorting = gtk.Expander(_('Display Order / Grouping'))
        self.hbox_sort_group = HIGHBox()
        self.frame_sort = gtk.Frame()
        self.frame_group = gtk.Frame()
        self.table_sort = HIGTable()
        self.table_group = HIGTable()
        self.radio_sort_port = gtk.RadioButton(None, _('Sort by port number'))
        self.radio_sort_service = gtk.RadioButton(self.radio_sort_port,\
                                                  _('Sort by service name'))

    def _set_host_list(self):
        self.host_view.set_enable_search(True)
        self.host_view.set_search_column(2)
        
        selection = self.host_view.get_selection()
        selection.set_mode(gtk.SELECTION_MULTIPLE)

        columns = ["service", "icon", "hostname", "port_number",
                   "protocol", "state", "version"]
        
        for c in columns:
            self.host_view.append_column(self.host_columns[c])
            self.host_columns[c].set_reorderable(True)
            self.host_columns[c].set_resizable(True)

        self.host_columns['service'].connect('clicked', self.set_host_search_cb, 0)
        self.host_columns['icon'].connect('clicked', self.set_host_search_cb, 5)
        self.host_columns['hostname'].connect('clicked', self.set_host_search_cb, 2)
        self.host_columns['port_number'].connect('clicked', self.set_host_search_cb, 3)
        self.host_columns['protocol'].connect('clicked', self.set_host_search_cb, 4)
        self.host_columns['state'].connect('clicked', self.set_host_search_cb, 5)
        self.host_columns['version'].connect('clicked', self.set_host_search_cb, 6)

        self.host_columns['service'].set_sort_column_id(0)
        self.host_columns['icon'].set_min_width(35)
        self.host_columns['icon'].set_sort_column_id(5)
        self.host_columns['hostname'].set_sort_column_id(2)
        self.host_columns['port_number'].set_sort_column_id(3)
        self.host_columns['protocol'].set_sort_column_id(4)
        self.host_columns['state'].set_sort_column_id(5)
        self.host_columns['version'].set_sort_column_id(6)

        self.host_columns['service'].pack_start(self.cell_port, True)
        self.host_columns['icon'].pack_start(self.cell_host_icon, True)
        self.host_columns['hostname'].pack_start(self.cell_port, True)
        self.host_columns['port_number'].pack_start(self.cell_port, True)
        self.host_columns['protocol'].pack_start(self.cell_port, True)
        self.host_columns['version'].pack_start(self.cell_port, True)
        self.host_columns['state'].pack_start(self.cell_port, True)
        
        self.host_columns['service'].set_attributes(self.cell_port, text=0)
        self.host_columns['icon'].set_attributes(self.cell_host_icon, stock_id=1)
        self.host_columns['hostname'].set_attributes(self.cell_port, text=2)
        self.host_columns['port_number'].set_attributes(self.cell_port, text=3)
        self.host_columns['protocol'].set_attributes(self.cell_port, text=4)
        self.host_columns['state'].set_attributes(self.cell_port, text=5)
        self.host_columns['version'].set_attributes(self.cell_port, text=6)
        
        self.host_columns['service'].set_visible(False)
        
        self.scroll_ports_hosts.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
    
    def _set_port_list(self):
        self.port_view.set_enable_search(True)
        self.port_view.set_search_column(3)
        
        selection = self.port_view.get_selection()
        selection.set_mode(gtk.SELECTION_MULTIPLE)
        
        self.port_view.append_column(self.port_columns['hostname'])
        self.port_view.append_column(self.port_columns['icon'])
        self.port_view.append_column(self.port_columns['port_number'])
        self.port_view.append_column(self.port_columns['protocol'])
        self.port_view.append_column(self.port_columns['state'])
        self.port_view.append_column(self.port_columns['service'])
        self.port_view.append_column(self.port_columns['version'])
        
        for k in self.port_columns:
            self.port_columns[k].set_reorderable(True)
            self.port_columns[k].set_resizable(True)


        self.port_columns['icon'].set_min_width(35)

        self.port_columns['hostname'].connect('clicked', self.set_search_cb, 0)
        self.port_columns['icon'].connect('clicked', self.set_search_cb, 4)
        self.port_columns['port_number'].connect('clicked', self.set_search_cb,
                                                 2)
        self.port_columns['protocol'].connect('clicked', self.set_search_cb, 3)
        self.port_columns['state'].connect('clicked', self.set_search_cb, 4)
        self.port_columns['service'].connect('clicked', self.set_search_cb, 5)
        self.port_columns['version'].connect('clicked', self.set_search_cb, 6)
        
        self.port_columns['hostname'].set_sort_column_id(0)
        self.port_columns['icon'].set_sort_column_id(4)
        self.port_columns['port_number'].set_sort_column_id(2)
        self.port_columns['protocol'].set_sort_column_id(3)
        self.port_columns['state'].set_sort_column_id(4)
        self.port_columns['service'].set_sort_column_id(5)
        self.port_columns['version'].set_sort_column_id(6)
        
        self.port_columns['hostname'].pack_start(self.cell_port, True)
        self.port_columns['icon'].pack_start(self.cell_icon, True)
        self.port_columns['port_number'].pack_start(self.cell_port, True)
        self.port_columns['protocol'].pack_start(self.cell_port, True)
        self.port_columns['service'].pack_start(self.cell_port, True)
        self.port_columns['version'].pack_start(self.cell_port, True)
        self.port_columns['state'].pack_start(self.cell_port, True)
        
        self.port_columns['hostname'].set_attributes(self.cell_port, text=0)
        self.port_columns['icon'].set_attributes(self.cell_icon, stock_id=1)
        self.port_columns['port_number'].set_attributes(self.cell_port, text=2)
        self.port_columns['protocol'].set_attributes(self.cell_port, text=3)
        self.port_columns['state'].set_attributes(self.cell_port, text=4)
        self.port_columns['service'].set_attributes(self.cell_port, text=5)
        self.port_columns['version'].set_attributes(self.cell_port, text=6)
        
        self.port_columns['hostname'].set_visible(False)
        
        self.scroll_ports_hosts.set_policy(gtk.POLICY_AUTOMATIC,\
                                          gtk.POLICY_AUTOMATIC)

    def port_mode(self):
        child = self.scroll_ports_hosts.get_child()
        if id(child) != id(self.port_view):
            self.scroll_ports_hosts.remove(child)
            self.scroll_ports_hosts.add(self.port_view)
            self.port_view.show_all()
            self.host_view.hide()

    def host_mode(self):
        child = self.scroll_ports_hosts.get_child()
        if id(child) != id(self.host_view):
            self.scroll_ports_hosts.remove(child)
            self.scroll_ports_hosts.add(self.host_view)
            self.host_view.show_all()
            self.port_view.hide()

    def set_ports(self, ports):
        self.clear_port_list()
        
        for p in ports:
            self.port_list.append(p)

    def set_hosts(self, hosts):
        self.clear_host_list()

        for h in hosts:
            self.host_list.append(h)
    
    def add_port(self, port_info):
        log.debug(">>> Add Port: %s" % port_info)
        self.port_list.append([""] + port_info)

    def add_host(self, host_info):
        log.debug(">>> Add Host: %s" % host_info)
        self.host_list.append([""] + host_info)
    
    def switch_port_to_list_store(self):
        if self.port_view.get_model() != self.port_list:
            self.port_view.set_model(self.port_list)
            self.port_columns['hostname'].set_visible(False)
    
    def switch_port_to_tree_store(self):
        if self.port_view.get_model() != self.port_tree:
            self.port_view.set_model(self.port_tree)
            self.port_columns['hostname'].set_visible(True)
    
    def switch_host_to_list_store(self):
        if self.host_view.get_model() != self.host_list:
            self.host_view.set_model(self.host_list)
            self.host_columns['service'].set_visible(False)
    
    def switch_host_to_tree_store(self):
        if self.host_view.get_model() != self.host_tree:
            self.host_view.set_model(self.host_tree)
            self.host_columns['service'].set_visible(True)

    def set_search_cb(self, widget, column_id):
        self.port_view.set_search_column(column_id)

    def set_host_search_cb(self, widget, column_id):
        self.host_view.set_search_column(column_id)
    
    def _pack_widgets(self):
        self.scroll_ports_hosts.add(self.port_view)
        self._pack_expand_fill(self.scroll_ports_hosts)
        #self._pack_noexpand_nofill(self.expander_sorting)

    def clear_port_list(self):
        for i in range(len(self.port_list)):
            iter = self.port_list.get_iter_root()
            del(self.port_list[iter])
            
    def clear_host_list(self):
        for i in range(len(self.host_list)):
            iter = self.host_list.get_iter_root()
            del(self.host_list[iter])

    def clear_port_tree(self):
        for i in range(len(self.port_tree)):
            iter = self.port_tree.get_iter_root()
            del(self.port_tree[iter])

    def clear_host_tree(self):
        for i in range(len(self.host_tree)):
            iter = self.host_tree.get_iter_root()
            del(self.host_tree[iter])

if __name__ == "__main__":
    w = gtk.Window()
    h = HostOpenPorts()
    w.add(h)
    w.show_all()

    gtk.main()
