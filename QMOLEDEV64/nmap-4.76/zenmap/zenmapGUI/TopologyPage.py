#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Vladimir Mitrovic <snipe714@gmail.com>
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

from zenmapGUI.ScanToolbar import *
from zenmapCore.NetworkInventory import NetworkInventory
from zenmapCore.UmitConf import CommandProfile, ProfileNotFound
from zenmapCore.NmapCommand import NmapCommand
from zenmapCore.NmapParser import NmapParser

from zenmapGUI.higwidgets.higboxes import HIGVBox, HIGHBox
from zenmapGUI.higwidgets.higdialogs import HIGAlertDialog
from zenmapGUI.higwidgets.higbuttons import HIGButton, HIGToggleButton
from zenmapGUI.higwidgets.higwindows import HIGWindow

from radialnet.core.XMLHandler import XMLReader
from radialnet.gui.RadialNet import *
from radialnet.gui.ControlWidget import *
from radialnet.gui.Toolbar import Toolbar
from radialnet.bestwidgets.boxes import *
from radialnet.bestwidgets.windows import *
from radialnet.util.integration import make_graph_from_nmap_parser, make_graph_from_hosts


class TopologyPage(HIGVBox):
    def __init__(self, inventory):
        HIGVBox.__init__(self)
        
        self.set_border_width(6)
        self.set_spacing(4)
        
        self.network_inventory = inventory
        
        self._create_widgets()
        self._pack_widgets()
    
    def _create_widgets(self):
        # The second toolbar: command entry field + buttons
        self.second_toolbar = HIGHBox()
        
        self.rn_hbox = gtk.HBox()
        self.rn_hbox.set_spacing(4)
        self.rn_vbox = gtk.VBox()
        
        # RadialNet's widgets
        self.radialnet = RadialNet(LAYOUT_WEIGHTED)
        self.control = ControlWidget(self.radialnet)
        self.fisheye = ControlFisheye(self.radialnet)
        self.rn_toolbar = Toolbar(self.radialnet,
                               self,
                               self.control,
                               self.fisheye)

    def _pack_widgets(self):
        self.second_toolbar.pack_start(self.rn_toolbar, False)
        
        self.rn_hbox.pack_start(self.radialnet, True, True)
        self.rn_hbox.pack_start(self.control, False)

        self.rn_vbox.pack_start(self.rn_hbox, True, True)
        self.rn_vbox.pack_start(self.fisheye, False)
        
        self.pack_start(self.second_toolbar, False, False)
        self.pack_start(self.rn_vbox, True, True)
    
    def add_scan(self, scan):
        """Parses a given XML file and adds the parsed result to the network inventory."""
        self.network_inventory.add_scan(scan)
        self.update_radialnet()
    
    def update_radialnet(self):
        """Creates a graph from network inventory's host list and displays it."""
        graph = make_graph_from_hosts(self.network_inventory.get_hosts())
        self.radialnet.set_empty()
        self.radialnet.set_graph(graph)
        self.radialnet.show()
