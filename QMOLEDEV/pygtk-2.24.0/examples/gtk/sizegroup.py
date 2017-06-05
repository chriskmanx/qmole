#
# Small example of GtkSizeGroup
#
# Johan Dahlin <johan@gnome.org>, 2005
#

"""Simple example that demonstrates how to use a GtkSizeGroup.

In this case we'll have two labels and two entries.
The labels have different width, but we'd like to have the entries
aligned vertically. We can accomplish this by adding a horizontal
sizegroup to the labels.
"""

import pygtk
pygtk.require('2.0')
import gtk

def create_label(text):
    hbox = gtk.HBox(spacing=6)
    label = gtk.Label(text)
    hbox.pack_start(label)
    entry = gtk.Entry()
    hbox.pack_start(entry)
    return hbox, label

def main():
    win = gtk.Window()
    win.connect('delete-event', gtk.main_quit)
    win.set_border_width(6)
    win.set_title('GtkSizeGroup example')

    vbox = gtk.VBox(spacing=6)
    win.add(vbox)

    sg = gtk.SizeGroup(gtk.SIZE_GROUP_HORIZONTAL)

    hbox, label = create_label('Name:')
    sg.add_widget(label)
    vbox.pack_start(hbox)

    hbox, label = create_label('Address:')
    sg.add_widget(label)
    vbox.pack_start(hbox)

    win.show_all()
    gtk.main()

if __name__ == '__main__':
    main()
