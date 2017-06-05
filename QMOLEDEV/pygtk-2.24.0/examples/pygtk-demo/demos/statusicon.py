#!/usr/bin/env python
'''Status Icon

This is a simple example that shows how to create a status icon that
will appear in the "notification area" in GNOME/KDE, or "system tray"
in Windows.
'''
## Author: Nikos Kouremenos

import pygtk
pygtk.require('2.0')
import gtk


def make_menu(event_button, event_time, icon):
    menu = gtk.Menu()
    item = gtk.MenuItem('hi')
    item.show()
    menu.append(item)
    menu.popup(None, None,
        gtk.status_icon_position_menu, event_button,
        event_time, icon)

def on_right_click(icon, event_button, event_time):
    make_menu(event_button, event_time, icon)

def StatusIconDemo(parent=None):
    icon = gtk.status_icon_new_from_stock(gtk.STOCK_QUIT)
    icon.connect('popup-menu', on_right_click)

if __name__ == '__main__':
    StatusIconDemo()
    gtk.main()
