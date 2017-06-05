#!/usr/bin/env python

""" Simple Hello World example similar to the GTK+ Tutorials one """

import pygtk
pygtk.require('2.0')
import gtk

def hello(*args):
    """ Callback function that is attached to the button """
    print "Hello World"
    window.destroy()

def destroy(*args):
    """ Callback function that is activated when the program is destoyed """
    window.hide()
    gtk.main_quit()

# this block creates our main application window
window = gtk.Window(gtk.WINDOW_TOPLEVEL)
window.connect("destroy", destroy)
window.set_border_width(10)

# this block creates our button and places it within the window
button = gtk.Button("Hello World")
# connects the 'hello' function to the clicked signal from the button
button.connect("clicked", hello)
window.add(button)
button.show()

# as the button is within the window this also shows the window
window.show_all()
gtk.main()
