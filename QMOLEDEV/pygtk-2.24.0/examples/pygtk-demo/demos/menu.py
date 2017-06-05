#!/usr/bin/env python
'''Menu

This example demonstrates the use of various menu types in gtk.  It
demonstrates the new submenu navigation and scrolling menu features of
gtk 2.0.'''

import pygtk
pygtk.require('2.0')
import gtk

def create_menu(depth, length=5):
    if depth < 1:
        return None

    menu = gtk.Menu()
    group= None

    for i in range(length):
        menuitem = gtk.RadioMenuItem(group, 'item %2d - %d' % (depth, i))
        group = menuitem
        menu.add(menuitem)
        menuitem.show()
        if depth > 1:
            submenu = create_menu(depth - 1)
            menuitem.set_submenu(submenu)
    return menu

class MenuDemo(gtk.Window):
    def __init__(self, parent=None):
        # Create the toplevel window
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())

        self.set_title(self.__class__.__name__)

        vbox = gtk.VBox()
        self.add(vbox)

        menubar = gtk.MenuBar()
        vbox.pack_start(menubar, expand=False)

        menuitem = gtk.MenuItem('test\nline2')
        menuitem.set_submenu(create_menu(2, 50))
        menubar.add(menuitem)

        menuitem = gtk.MenuItem('foo')
        menuitem.set_submenu(create_menu(2))
        menubar.add(menuitem)

        menuitem = gtk.MenuItem('bar')
        menuitem.set_submenu(create_menu(2))
        menuitem.set_right_justified(True)
        menubar.add(menuitem)

        vbox2 = gtk.VBox(spacing=10)
        vbox2.set_border_width(10)
        vbox.pack_start(vbox2)

        combo_box = gtk.combo_box_new_text()
        combo_box.set_wrap_width(2)
        for i in range(50):
            combo_box.append_text('item - %d' % i)
        combo_box.set_active(0)
        vbox2.pack_start(combo_box)

        separator = gtk.HSeparator()
        vbox.pack_start(separator, expand=False)

        vbox2 = gtk.VBox(spacing=10)
        vbox2.set_border_width(10)
        vbox.pack_start(vbox2, expand=False)

        button = gtk.Button('close')
        button.connect('clicked', lambda button, w=self: w.destroy())
        vbox2.pack_start(button)
        button.set_flags(gtk.CAN_DEFAULT)
        button.grab_default()

        self.show_all()

def main():
    MenuDemo()
    gtk.main()

if __name__ == '__main__':
    main()
