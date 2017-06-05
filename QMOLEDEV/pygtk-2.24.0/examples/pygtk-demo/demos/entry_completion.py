#!/usr/bin/env python
'''Entry Completion

GtkEntryCompletion provides a mechanism for adding support for
completion in GtkEntry.
'''
# pygtk version: Maik Hertha <maik.hertha@berlin.de>

import pygtk
pygtk.require('2.0')
import gtk

class EntryCompletionDemo(gtk.Dialog):

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
        label.set_markup("Completion demo, try writing <b>total</b> "
            "or <b>gnome</b> for example.")
        vbox.pack_start(label, False, False, 0)

        # Create our entry
        entry = gtk.Entry()
        vbox.pack_start(entry, False, False, 0)

        # Create the completion object
        completion = gtk.EntryCompletion()

        # Assign the completion to the entry
        entry.set_completion(completion)

        # Create a tree model and use it as the completion model
        completion_model = self.__create_completion_model()
        completion.set_model(completion_model)

        # Use model column 0 as the text column
        completion.set_text_column(0)

        self.show_all()

    def __create_completion_model(self):
        ''' Creates a tree model containing the completions.
        '''
        store = gtk.ListStore(str)

        # Append one word
        iter = store.append()
        store.set(iter, 0, "GNOME")

        # Append another word
        iter = store.append()
        store.set(iter, 0, "total")

        # And another word
        iter = store.append()
        store.set(iter, 0, "totally")

        return store

def main():
    EntryCompletionDemo()
    gtk.main()

if __name__ == '__main__':
    main()
