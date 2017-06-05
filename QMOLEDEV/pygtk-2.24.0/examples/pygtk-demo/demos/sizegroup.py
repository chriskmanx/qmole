#!/usr/bin/env python
"""Size Group

GtkSizeGroup provides a mechanism for grouping a number of widgets together so
they all request the same amount of space. This is typically useful when you
want a column of widgets to have the same size, but you can't use a GtkTable
widget.

Note that size groups only affect the amount of space requested, not the size
that the widgets finally receive. If you want the widgets in a GtkSizeGroup to
actually be the same size, you need to pack them in such a way that they get
the size they request and not more. For example, if you are packing your
widgets into a table, you would not include the GTK_FILL flag."""

import pygtk
pygtk.require('2.0')
import gtk

class SizeGroupDemo(gtk.Dialog):
    def __init__(self, parent=None):
        gtk.Dialog.__init__(self, "Size Groups", parent,
            0,
            (gtk.STOCK_CLOSE,  gtk.RESPONSE_CLOSE))
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.connect('response', lambda d, r: d.destroy())
        self.set_resizable(False)

        vbox = gtk.VBox(False, 5)
        self.vbox.pack_start(vbox, True, True, 0)
        vbox.set_border_width(5)

        self.size_group = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)

        # Create one frame holding color options
        frame = gtk.Frame("Color options")
        vbox.pack_start(frame, True, True, 0)

        table = gtk.Table(2, 2, False)
        table.set_border_width(5)
        table.set_row_spacings(5)
        table.set_col_spacings(10)
        frame.add(table)

        color_options = ("Red", "Green", "Blue")
        self.__add_row(table, 0, "_Foreground", color_options)
        self.__add_row(table, 1, "_Background", color_options)

        # And another frame holding line style options
        frame = gtk.Frame("Line options")
        vbox.pack_start(frame, False, False, 0)

        table = gtk.Table(2, 2, False)
        table.set_border_width(5)
        table.set_row_spacings(5)
        table.set_col_spacings(10)
        frame.add(table)

        dash_options  = ("Solid", "Dashed", "Dotted")
        end_options   = ("Square", "Round", "Arrow")
        self.__add_row(table, 0, "_Dashing", dash_options)
        self.__add_row(table, 1, "_Line ends", end_options)

        # And a check button to turn grouping on and off

        check_button = gtk.CheckButton("_Enable grouping")
        vbox.pack_start(check_button, False, False, 0)
        check_button.set_active(True)
        check_button.connect('toggled', self.on_toggle_grouping)

        self.show_all()

    def __create_option_menu(self, options):

        option_menu = gtk.combo_box_new_text()
        for opt in options:
            option_menu.append_text(opt)

        option_menu.set_active(0)
        return option_menu

    def __add_row(self, table, row, label_text, options):
        label = gtk.Label(label_text)
        label.set_use_underline(True)
        label.set_alignment(0, 1)
        table.attach(label, 0, 1, row, row + 1, gtk.EXPAND | gtk.FILL, 0, 0, 0)

        option_menu = self.__create_option_menu(options)
        label.set_mnemonic_widget(option_menu)
        self.size_group.add_widget(option_menu)
        table.attach(option_menu, 1, 2, row, row + 1, 0, 0, 0, 0)

    def on_toggle_grouping(self, check_button):

        # gtk.SIZE_GROUP_NONE is not generally useful, but is useful
        # here to show the effect of gtk.SIZE_GROUP_HORIZONTAL by
        # contrast.
        if check_button.get_active():
            self.size_group.set_mode(gtk.SIZE_GROUP_HORIZONTAL)
        else:
            self.size_group.set_mode(gtk.SIZE_GROUP_NONE)

def main():
    SizeGroupDemo()
    gtk.main()

if __name__ == '__main__':
    main()
