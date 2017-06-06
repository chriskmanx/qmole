#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Guilherme Polo <ggpolo@gmail.com>
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
higwidgets/higframe.py

    hig frame
"""

__all__ = ['HIGFrame']

import gtk

class HIGFrame(gtk.Frame):
    """
    Frame without border with bold label.
    """
    def __init__(self, label=None):
        gtk.Frame.__init__(self)
        
        self.set_shadow_type(gtk.SHADOW_NONE)
        self._flabel = gtk.Label()
        self._set_label(label)
        self.set_label_widget(self._flabel)

    def _set_label(self, label):
        self._flabel.set_markup("<b>%s</b>" % label)

# Demo
if __name__ == "__main__":
    w = gtk.Window()

    hframe = HIGFrame("Sample HIGFrame")
    aalign = gtk.Alignment(0, 0, 0, 0)
    aalign.set_padding(12, 0, 24, 0)
    abox = gtk.VBox()
    aalign.add(abox)
    hframe.add(aalign)
    w.add(hframe)

    for i in xrange(5):
        abox.pack_start(gtk.Label("Sample %d" % i), False, False, 3)

    w.connect('destroy', lambda d: gtk.main_quit())
    w.show_all()
    
    gtk.main()
