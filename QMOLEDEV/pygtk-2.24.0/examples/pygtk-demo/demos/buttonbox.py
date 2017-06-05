#!/usr/bin/env python
'''Button Box

This demo shows various button box configurations available.  It also
uses stock buttons, and use of mnemonics for navigation.'''

import pygtk
pygtk.require('2.0')
import gtk

def create_bbox(horizontal=True, title=None, spacing=0,
        layout=gtk.BUTTONBOX_SPREAD):
    frame = gtk.Frame(title)

    if horizontal:
        bbox = gtk.HButtonBox()
    else:
        bbox = gtk.VButtonBox()

    bbox.set_border_width(5)
    bbox.set_layout(layout)
    bbox.set_spacing(spacing)
    frame.add(bbox)

    button = gtk.Button(stock='gtk-ok')
    bbox.add(button)

    button = gtk.Button(stock='gtk-cancel')
    bbox.add(button)

    button = gtk.Button(stock='gtk-help')
    bbox.add(button)

    return frame

class ButtonBoxDemo(gtk.Window):
    def __init__(self, parent=None):
        # Create the toplevel window
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())

        self.set_title(self.__class__.__name__)
        self.set_border_width(10)

        main_vbox = gtk.VBox()
        self.add(main_vbox)

        frame_horiz = gtk.Frame("Horizontal Button Boxes")
        main_vbox.pack_start(frame_horiz, padding=10)

        vbox = gtk.VBox()
        vbox.set_border_width(10)
        frame_horiz.add(vbox)

        vbox.pack_start(create_bbox(True, "Spread", 40, gtk.BUTTONBOX_SPREAD),
                padding=0)
        vbox.pack_start(create_bbox(True, "Edge", 40, gtk.BUTTONBOX_EDGE),
                padding=5)
        vbox.pack_start(create_bbox(True, "Start", 40, gtk.BUTTONBOX_START),
                padding=5)
        vbox.pack_start(create_bbox(True, "End", 40, gtk.BUTTONBOX_END),
                padding=5)

        frame_vert = gtk.Frame("Vertical Button Boxes")
        main_vbox.pack_start(frame_vert, padding=10)

        hbox = gtk.HBox()
        hbox.set_border_width(10)
        frame_vert.add(hbox)

        hbox.pack_start(create_bbox(False, "Spread", 40, gtk.BUTTONBOX_SPREAD),
                padding=0)
        hbox.pack_start(create_bbox(False, "Edge", 40, gtk.BUTTONBOX_EDGE),
                padding=5)
        hbox.pack_start(create_bbox(False, "Start", 40, gtk.BUTTONBOX_START),
                padding=5)
        hbox.pack_start(create_bbox(False, "End", 40, gtk.BUTTONBOX_END),
                padding=5)

        self.show_all()

def main():
    ButtonBoxDemo()
    gtk.main()

if __name__ == '__main__':
    main()
