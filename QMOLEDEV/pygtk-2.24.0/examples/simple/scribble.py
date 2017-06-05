#!/usr/bin/env python

#this is a simple translation of the scribble example that comes with GTK+

import pygtk
pygtk.require('2.0')
import gtk

pixmap = None

def configure_event(widget, event):
    global pixmap
    win = widget.window
    width, height = win.get_size()
    pixmap = gtk.gdk.Pixmap(win, width, height)
    pixmap.draw_rectangle(widget.get_style().white_gc, True,
                          0, 0, width, height)
    return True

def expose_event(widget, event):
    x, y, width, height = event.area
    gc = widget.get_style().fg_gc[gtk.STATE_NORMAL]
    widget.window.draw_drawable(gc, pixmap, x, y, x, y, width, height)
    return False

def draw_brush(widget, x, y):
    x, y = int(x), int(y)
    pixmap.draw_rectangle(widget.get_style().black_gc, True,
                          x-5, y-5, 10, 10)
    widget.queue_draw()

def button_press_event(widget, event):
    if event.button == 1 and pixmap != None:
        draw_brush(widget, event.x, event.y)
    return True

def motion_notify_event(widget, event):
    if event.is_hint:
        x, y, state = event.window.get_pointer()
    else:
        x = event.x; y = event.y
        state = event.state
    if state & gtk.gdk.BUTTON1_MASK and pixmap != None:
        draw_brush(widget, x, y)
    return True

def main():
    win = gtk.Window()
    win.set_name("Test Input")
    win.connect("destroy", lambda w: gtk.main_quit())
    win.set_border_width(5)

    vbox = gtk.VBox(spacing=3)
    win.add(vbox)
    vbox.show()

    drawing_area = gtk.DrawingArea()
    drawing_area.set_size_request(200, 200)
    vbox.pack_start(drawing_area)
    drawing_area.show()

    drawing_area.connect("expose_event", expose_event)
    drawing_area.connect("configure_event", configure_event)
    drawing_area.connect("motion_notify_event", motion_notify_event)
    drawing_area.connect("button_press_event", button_press_event)
    drawing_area.set_events(gtk.gdk.EXPOSURE_MASK |
                            gtk.gdk.LEAVE_NOTIFY_MASK |
                            gtk.gdk.BUTTON_PRESS_MASK |
                            gtk.gdk.POINTER_MOTION_MASK |
                            gtk.gdk.POINTER_MOTION_HINT_MASK)

    button = gtk.Button(stock=gtk.STOCK_QUIT)
    vbox.pack_start(button, expand=False, fill=False)
    button.connect("clicked", lambda widget, win=win: win.destroy())
    button.show()
    win.show()
    gtk.main()

if __name__ == '__main__':
    main()
