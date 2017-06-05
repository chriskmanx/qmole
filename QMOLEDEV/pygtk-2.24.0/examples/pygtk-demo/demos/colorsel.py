#!/usr/bin/env python
"""Color Selector

GtkColorSelection lets the user choose a color. GtkColorSelectionDialog is a
prebuilt dialog containing a GtkColorSelection."""

import pygtk
pygtk.require('2.0')
import gtk

class ColorSelectorDemo(gtk.Window):
    color = gtk.gdk.color_parse("blue")

    def __init__(self, parent=None):
        # Create the toplevel window
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())

        self.set_title(self.__class__.__name__)
        self.set_border_width(8)
        vbox = gtk.VBox()
        vbox.set_border_width(8)
        self.add(vbox)

        # Create the color swatch area
        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_IN)
        vbox.pack_start(frame, True, True, 8)

        self.d_area = gtk.DrawingArea()
        self.d_area.set_size_request(200, 200)
        self.d_area.modify_bg(gtk.STATE_NORMAL, self.color)
        frame.add(self.d_area)

        alignment = gtk.Alignment(1.0, 0.5, 0.0, 0.0)

        button = gtk.Button("_Change the above color")
        alignment.add(button)

        vbox.pack_start(alignment, True, True)

        button.connect('clicked', self.on_change_color_clicked)
        button.set_flags(gtk.CAN_DEFAULT)
        button.grab_default()

        self.show_all()

    def on_change_color_clicked(self, button):

        dialog = gtk.ColorSelectionDialog("Changing color")
        dialog.set_transient_for(self)
        colorsel = dialog.colorsel

        colorsel.set_previous_color(self.color)
        colorsel.set_current_color(self.color)
        colorsel.set_has_palette(True)

        response = dialog.run()

        if response == gtk.RESPONSE_OK:
            self.color = colorsel.get_current_color()
            self.d_area.modify_bg(gtk.STATE_NORMAL, self.color)

        dialog.destroy()
        return True

def main():
    ColorSelectorDemo()
    gtk.main()

if __name__ == '__main__':
    main()
