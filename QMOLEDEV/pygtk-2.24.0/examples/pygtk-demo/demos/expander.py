#!/usr/bin/env python
'''Expander

GtkExpander allows to provide additional content that is initially hidden.
This is also known as "disclosure triangle".
'''
# pygtk version: Maik Hertha <maik.hertha@berlin.de>

import pygtk
pygtk.require('2.0')
import gtk

class ExpanderDemo(gtk.Dialog):

    def __init__(self, parent=None):
        gtk.Dialog.__init__(self, self.__class__.__name__, parent,
            0,
            (gtk.STOCK_CLOSE, gtk.RESPONSE_NONE))
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.connect("response", lambda d, r: d.destroy())
        self.set_resizable(False)

        vbox = gtk.VBox(False, 5)
        self.vbox.pack_start(vbox, True, True, 0)
        vbox.set_border_width(5)

        label = gtk.Label()
        label.set_markup("Expander demo. Click on the triangle for details.")
        vbox.pack_start(label, False, False, 0)

        # Create the expander
        expander = gtk.Expander("Details")
        vbox.pack_start(expander, False, False, 0)

        # The Label for the expander
        label = gtk.Label("Details can be shown or hidden.")
        expander.add(label)

        self.show_all()

def main():
    ExpanderDemo()
    gtk.main()

if __name__ == '__main__':
    main()
