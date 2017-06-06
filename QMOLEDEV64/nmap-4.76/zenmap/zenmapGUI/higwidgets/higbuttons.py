#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
#         Cleber Rodrigues <cleber.gnu@gmail.com>
#
# This library is free software; you can redistribute it and/or modify 
# it under the terms of the GNU Lesser General Public License as published 
# by the Free Software Foundation; either version 2.1 of the License, or 
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but 
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public 
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License 
# along with this library; if not, write to the Free Software Foundation, 
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 

"""
higwidgets/higbuttons.py

   button related classes
"""

__all__ = ['HIGMixButton', 'HIGButton']

import gtk

class HIGMixButton (gtk.HBox):
    def __init__(self, title, stock):
        gtk.HBox.__init__(self, False, 4)
        self.img = gtk.Image()
        self.img.set_from_stock(stock, gtk.ICON_SIZE_BUTTON)
        
        self.lbl = gtk.Label(title)
		
        self.hbox1 = gtk.HBox(False, 2)
        self.hbox1.pack_start(self.img, False, False, 0)
        self.hbox1.pack_start(self.lbl, False, False, 0)
		
        self.align = gtk.Alignment(0.5, 0.5, 0, 0)
        self.pack_start(self.align)
        self.pack_start(self.hbox1)

class HIGButton (gtk.Button):
    def __init__ (self, title="", stock=None):
        if title and stock:
            gtk.Button.__init__(self)
            content = HIGMixButton(title, stock)
            self.add(content)
        elif title and not stock:
            gtk.Button.__init__(self, title)
        elif stock:
            gtk.Button.__init__(self, stock=stock)
        else:
            gtk.Button.__init__(self)

class HIGToggleButton(gtk.ToggleButton):
    def __init__(self, title="", stock=None):
        if title and stock:
            gtk.ToggleButton.__init__(self)
            content = HIGMixButton(title, stock)
            self.add(content)
        elif title and not stock:
            gtk.ToggleButton.__init__(self, title)
        elif stock:
            gtk.ToggleButton.__init__(self, stock)
            self.set_use_stock(True)
        else:
            gtk.ToggleButton.__init__(self)
