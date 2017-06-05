#!/usr/bin/env python
"""Dialog and Message Boxes

Dialog widgets are used to pop up a transient window for user feedback."""

import pygtk
pygtk.require('2.0')
import gtk

class DialogAndMessageBoxesDemo(gtk.Window):
    counter = 1
    def __init__(self, parent=None):
        # Create the toplevel window
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())

        self.set_title(self.__class__.__name__)
        self.set_border_width(8)

        frame = gtk.Frame("Dialogs")
        self.add(frame)

        vbox = gtk.VBox(False, 8)
        vbox.set_border_width(8)
        frame.add(vbox)

        # Standard message dialog
        hbox = gtk.HBox(False, 8)
        vbox.pack_start(hbox)
        button = gtk.Button("_Message Dialog")
        button.connect('clicked', self.on_message_dialog_clicked)
        hbox.pack_start(button, False, False, 0)
        vbox.pack_start(gtk.HSeparator(), False, False, 0)

        # Interactive dialog
        hbox = gtk.HBox(False, 8)
        vbox.pack_start(hbox, False, False, 0)
        vbox2 = gtk.VBox()

        button = gtk.Button("_Interactive Dialog")
        button.connect('clicked', self.on_interactive_dialog_clicked)
        hbox.pack_start(vbox2, False, False, 0)
        vbox2.pack_start(button, False, False, 0)

        table = gtk.Table(2, 2)
        table.set_row_spacings(4)
        table.set_col_spacings(4)
        hbox.pack_start(table, False, False, 0)

        label = gtk.Label("Entry _1")
        label.set_use_underline(True)
        table.attach(label, 0, 1, 0, 1)

        self.entry1 = gtk.Entry()
        table.attach(self.entry1, 1, 2, 0, 1)
        label.set_mnemonic_widget(self.entry1)

        label = gtk.Label("Entry _2")
        label.set_use_underline(True)
        table.attach(label, 0, 1, 1, 2)

        self.entry2 = gtk.Entry()
        table.attach(self.entry2, 1, 2, 1, 2)
        label.set_mnemonic_widget(self.entry2)

        self.show_all()

    def on_message_dialog_clicked(self, button):
        dialog = gtk.MessageDialog(self,
                gtk.DIALOG_MODAL | gtk.DIALOG_DESTROY_WITH_PARENT,
                gtk.MESSAGE_INFO, gtk.BUTTONS_OK,
                "This message box has been popped up %d time%s." %
                        (self.counter, self.counter > 1 and 's' or ''))
        dialog.run()
        dialog.destroy()
        self.counter += 1

    def on_interactive_dialog_clicked(self, button):

        dialog = gtk.Dialog("Interactive Dialog", self, 0,
                (gtk.STOCK_OK, gtk.RESPONSE_OK,
                "_Non-stock button", gtk.RESPONSE_CANCEL))

        hbox = gtk.HBox(False, 8)
        hbox.set_border_width(8)
        dialog.vbox.pack_start(hbox, False, False, 0)

        stock = gtk.image_new_from_stock(
                gtk.STOCK_DIALOG_QUESTION,
                gtk.ICON_SIZE_DIALOG)
        hbox.pack_start(stock, False, False, 0)

        table = gtk.Table(2, 2)
        table.set_row_spacings(4)
        table.set_col_spacings(4)
        hbox.pack_start(table, True, True, 0)

        label = gtk.Label("Entry _1")
        label.set_use_underline(True)
        table.attach(label, 0, 1, 0, 1)
        local_entry1 = gtk.Entry()
        local_entry1.set_text(self.entry1.get_text())
        table.attach(local_entry1, 1, 2, 0, 1)
        label.set_mnemonic_widget(local_entry1)

        label = gtk.Label("Entry _2")
        label.set_use_underline(True)
        table.attach(label, 0, 1, 1, 2)
        local_entry2 = gtk.Entry()
        local_entry2.set_text(self.entry2.get_text())
        table.attach(local_entry2, 1, 2, 1, 2)
        label.set_mnemonic_widget(local_entry2)

        dialog.show_all()

        response = dialog.run()

        if response == gtk.RESPONSE_OK:
            self.entry1.set_text(local_entry1.get_text())
            self.entry2.set_text(local_entry2.get_text())

        dialog.destroy()

def main():
    DialogAndMessageBoxesDemo()
    gtk.main()

if __name__ == '__main__':
    main()
