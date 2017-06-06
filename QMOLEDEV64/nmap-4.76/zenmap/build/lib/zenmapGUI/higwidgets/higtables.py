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
higwidgets/higlogindialog.py

   a basic login/authentication dialog
"""

__all__ = ['HIGTable']

import gtk

#from higlabels import *
#from higentries import *

class HIGTable(gtk.Table):
    """
    A HIGFied table
    """

    # TODO:
    # - Automatic position packing,
    # - Gereric attach function that detects the widget type
    
    def __init__(self, rows=1, columns=1, homogeneous=False):
        gtk.Table.__init__(self, rows, columns, homogeneous)
        self.set_row_spacings(6)
        self.set_col_spacings(12)
        
        self.rows = rows
        self.columns = columns
		
    def attach_label(self, widget, x0, x, y0, y):
        self.attach(widget, x0, x, y0, y, xoptions=gtk.FILL)
            
    def attach_entry(self, widget, x0, x, y0, y):
        self.attach(widget, x0, x, y0, y, xoptions=gtk.FILL|gtk.EXPAND)
