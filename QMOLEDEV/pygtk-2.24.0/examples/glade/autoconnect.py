#!/usr/bin/env python
#
# Small test to demonstrate glade.XML.signal_autoconnect on an instance
#

import pygtk
pygtk.require('2.0')
import gtk, gtk.glade

class SimpleTest:
    def __init__(self):
        xml = gtk.glade.XML('test2.glade')
        xml.signal_autoconnect(self)

    def on_button1_clicked(self, button):
        print 'foo!'

test = SimpleTest()
gtk.main()
