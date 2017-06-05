#!/usr/bin/env python

""" Simple example of creating a basic window and button.
    Also adds a tooltip. """

import pygtk
pygtk.require('2.0')
import gtk

def hello_cb(widget, main_window):
    """ Callback function that prints a message and destroys the window """
    print "Hello World"
    main_window.destroy()

def destroy_cb(widget, main_window):
    """ Callback function to hide the main window and then terminate. """
    main_window.hide()
    gtk.main_quit()

def main():
    """ Sets up the application
        Forms the widgets and connects callback functions to the signals """

    window = gtk.Window( type=gtk.WINDOW_TOPLEVEL )
    window.set_title("Hello World")
    window.set_default_size(200, 200)
    window.set_border_width(10)
    window.connect("destroy", destroy_cb, window)

    button = gtk.Button(label="Hello World")
    window.add(button)
    button.connect("clicked", hello_cb, window)

    # setup tooltips and associate them with the button
    tt = gtk.Tooltips()
    tt.set_tip(button, 'Prints "Hello World"', None)
    tt.enable()

    # shows the window and any child objects (button in this example)
    window.show_all()
    gtk.main()

# if we're being run normally then call the main function
if __name__ == '__main__':
    main()
