#!/usr/bin/env python
"""Paned Widgets

The GtkHPaned and GtkVPaned Widgets divide their content area into two panes
with a divider in between that the user can adjust. A separate child is placed
into each pane.
There are a number of options that can be set for each pane. This test contains
both a horizontal(HPaned) and a vertical(VPaned) widget, and allows you to
adjust the options for each side of each widget."""

import pygtk
pygtk.require('2.0')
import gtk

class PanedWidgetsDemo(gtk.Window):
    def __init__(self, parent=None):
        # Create the toplevel window
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())

        self.set_title(self.__class__.__name__)
        self.set_border_width(0)

        vbox = gtk.VBox(False, 0)
        self.add(vbox)

        vpaned = gtk.VPaned()
        vbox.pack_start(vpaned, True, True)
        vpaned.set_border_width(5)

        hpaned = gtk.HPaned()
        vpaned.add1(hpaned)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_IN)
        frame.set_size_request(60, 60)
        hpaned.add1(frame)

        button = gtk.Button("_Hi there")
        frame.add(button)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_IN)
        frame.set_size_request(80, 60)
        hpaned.add2(frame)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_IN)
        frame.set_size_request(60, 80)
        vpaned.add2(frame)

        # Now create toggle buttons to control sizing

        vbox.pack_start(
            self.__create_pane_options(hpaned, "Horizontal", "Left", "Right"),
            False, False, 0)

        vbox.pack_start(
            self.__create_pane_options(vpaned, "Vertical", "Top", "Bottom"),
            False, False, 0)

        self.show_all()

    def on_resize_toggled(self, tbutton, child):
        paned = child.parent

        if child == paned.get_children()[0]:
            paned.remove(child)
            paned.pack1(child, tbutton.get_active(), 0)
        else:
            paned.remove(child)
            paned.pack2(child, tbutton.get_active(), 0)

    def on_shrink_toggled(self, tbutton, child):
        paned = child.parent

        if child == paned.get_children()[0]:
            paned.remove(child)
            paned.pack1(child, 0, tbutton.get_active())
        else:
            paned.remove(child)
            paned.pack2(child, 0, tbutton.get_active())

    def __create_pane_options(self, paned, frame_label, label1, label2):
        frame = gtk.Frame(frame_label)
        frame.set_border_width(4)

        table = gtk.Table(3, 2, True)
        frame.add(table)

        label = gtk.Label(label1)
        table.attach(label, 0, 1, 0, 1)

        check_button = gtk.CheckButton("_Resize")
        check_button.connect('toggled', self.on_resize_toggled, paned.get_children()[0])
        table.attach(check_button, 0, 1, 1, 2)

        check_button = gtk.CheckButton("_Shrink")
        check_button.set_active(True)
        check_button.connect('toggled', self.on_shrink_toggled, paned.get_children()[0])
        table.attach(check_button, 0, 1, 2, 3)

        label = gtk.Label(label2)
        table.attach(label, 1, 2, 0, 1)

        check_button = gtk.CheckButton("_Resize")
        check_button.set_active(True)
        check_button.connect('toggled', self.on_resize_toggled, paned.get_children()[1])
        table.attach(check_button, 1, 2, 1, 2)

        check_button = gtk.CheckButton("_Shrink")
        check_button.set_active(True)
        check_button.connect('toggled', self.on_shrink_toggled, paned.get_children()[1])
        table.attach(check_button, 1, 2, 2, 3)

        return frame

def main():
    PanedWidgetsDemo()
    gtk.main()

if __name__ == '__main__':
    main()
