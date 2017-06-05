#!/usr/bin/env python
'''Info Bar

is a widget that can be used to show messages to the user without showing a 
dialog. It is often temporarily shown at the top or bottom of a document.
'''
# pygtk version: John Stowers <john.stowers@gmail.com>

import pygtk
pygtk.require('2.0')
import gtk

class InfoBarDemo(gtk.Window):
    def __init__(self, parent=None):
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.set_title(self.__class__.__name__)
        self.set_border_width(8)

        vb = gtk.VBox()
        self.add(vb)

        bar = gtk.InfoBar()
        vb.pack_start(bar, False, False)
        bar.set_message_type(gtk.MESSAGE_INFO)
        bar.get_content_area().pack_start(
                gtk.Label("This is an info bar with message type GTK_MESSAGE_INFO"),
                False, False)

        bar = gtk.InfoBar()
        vb.pack_start(bar, False, False)
        bar.set_message_type(gtk.MESSAGE_WARNING)
        bar.get_content_area().pack_start(
                gtk.Label("This is an info bar with message type GTK_MESSAGE_WARNING"),
                False, False)

        bar = gtk.InfoBar()
        bar.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
        bar.connect("response", self._on_bar_response)
        vb.pack_start(bar, False, False)
        bar.set_message_type(gtk.MESSAGE_QUESTION)
        bar.get_content_area().pack_start(
                gtk.Label("This is an info bar with message type GTK_MESSAGE_QUESTION"),
                False, False)

        bar = gtk.InfoBar()
        vb.pack_start(bar, False, False)
        bar.set_message_type(gtk.MESSAGE_ERROR)
        bar.get_content_area().pack_start(
                gtk.Label("This is an info bar with message type GTK_MESSAGE_ERROR"),
                False, False)

        bar = gtk.InfoBar()
        vb.pack_start(bar, False, False)
        bar.set_message_type(gtk.MESSAGE_OTHER)
        bar.get_content_area().pack_start(
                gtk.Label("This is an info bar with message type GTK_MESSAGE_OTHER"),
                False, False)

        frame = gtk.Frame("Info bars")
        vb.pack_start(frame, False, False, 8)
        vb2 = gtk.VBox(spacing=8)
        vb2.set_border_width(8)
        frame.add(vb2)
        vb2.pack_start(gtk.Label("An example of different info bars"), False, False)

        self.show_all()

    def _on_bar_response(self, button, response_id):
        dialog = gtk.MessageDialog(
                        self,
                        gtk.DIALOG_MODAL|gtk.DIALOG_DESTROY_WITH_PARENT,
                        gtk.MESSAGE_INFO,
                        gtk.BUTTONS_OK,
                        "You clicked a button on an info bar")
        dialog.format_secondary_text("Your response has id %d" % response_id)
        dialog.run()
        dialog.destroy()


def main():
    InfoBarDemo()
    gtk.main()

if __name__ == '__main__':
    main()
