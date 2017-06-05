#!/usr/bin/env python
import sys

import pygtk
pygtk.require('2.0')
import gtk
import gtk.glade

if len(sys.argv) > 1:
    fname = sys.argv[1]
else:
    fname = 'test.glade'

# create widget tree ...
xml = gtk.glade.XML(fname)

def gtk_main_quit(*args):
    gtk.main_quit()

xml.signal_autoconnect(locals())

gtk.main()
