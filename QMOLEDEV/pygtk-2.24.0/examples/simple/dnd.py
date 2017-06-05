#! /usr/bin/env python

import pygtk
pygtk.require('2.0')
import gobject
import gtk

from dndpixmap import drag_icon_xpm, trashcan_open_xpm, trashcan_closed_xpm


trashcan_open = None
trashcan_closed = None

have_drag = False;
popped_up = False
in_popup = False
popup_timer = 0
popdown_timer = 0
popup_win = None


TARGET_STRING = 0
TARGET_ROOTWIN = 1

target = [
    ('STRING', 0, TARGET_STRING),
    ('text/plain', 0, TARGET_STRING),
    ('application/x-rootwin-drop', 0, TARGET_ROOTWIN)
    ]

def target_drag_leave(w, context, time):
    global trashcan_closed
    global have_drag
    print 'leave'
    have_drag = False
    w.set_from_pixbuf(trashcan_closed)
def target_drag_motion(w, context, x, y, time):
    global trashcan_open
    global have_drag
    if not have_drag:
        have_drag = True
        w.set_from_pixbuf(trashcan_open)
    source_widget = context.get_source_widget()
    print 'motion, source ',
    if source_widget:
        print source_widget.__class__.__name__
    else:
        print 'unknown'
    context.drag_status(context.suggested_action, time)
    return True
def target_drag_drop(w, context, x, y, time):
    global trashcan_closed
    global have_drag
    print 'drop'
    have_drag = False
    w.set_from_pixbuf(trashcan_closed)
    if context.targets:
        w.drag_get_data(context, context.targets[0], time)
        return True
    return False
def target_drag_data_received(w, context, x, y, data, info, time):
    if data.format == 8:
        print 'Received "%s" in trashcan' % data.data
        context.finish(True, False, time)
    else:
        context.finish(False, False, time)
def label_drag_data_received(w, context, x, y, data, info, time):
    if data and data.format == 8:
        print 'Received "%s" in label' % data.data
        context.finish(True, False, time)
    else:
        context.finish(False, False, time)
def source_drag_data_get(w, context, selection_data, info, time):
    if info == TARGET_ROOTWIN:
        print 'I was dropped on the rootwin'
    else:
        selection_data.set(selection_data.target, 8, "I'm Data!")

def popdown_cb():
    global popdown_timer, popped_up
    global popup_win
    popdown_timer = 0
    popup_win.hide()
    popped_up = False
    return False
def popup_motion(w, context, x, y, time):
    global in_popup, popdown_timer
    if not in_popup:
        in_popup = True
        if popdown_timer:
            print 'removed popdown'
            gobject.source_remove(popdown_timer)
            popdown_timer = 0
    return True
def popup_leave(w, context, time):
    global in_popup, popdown_timer
    print 'popup_leave'
    if in_popup:
        in_popup = False
        if not popdown_timer:
            print 'added popdown'
            popdown_timer = gobject.timeout_add(500, popdown_cb)
def popup_cb():
    global popped_up, popup_win
    global popup_timer, popdown_timer
    if not popped_up:
        if not popup_win:
            popup_win = gtk.Window(gtk.WINDOW_POPUP)
            popup_win.set_position(gtk.WIN_POS_MOUSE)
            table = gtk.Table(3,3,False)
            for k in range(9):
                i, j = divmod(k, 3)
                b = gtk.Button("%d,%d" % (i,j))
                table.attach(b, i,i+1,j,j+1)
                b.drag_dest_set(gtk.DEST_DEFAULT_ALL, target,
                        gtk.gdk.ACTION_COPY | gtk.gdk.ACTION_MOVE)
                b.connect('drag_motion', popup_motion)
                b.connect('drag_leave', popup_leave)
            table.show_all()
            popup_win.add(table)
        popup_win.show()
        popped_up = True
    popdown_timer = gobject.timeout_add(500, popdown_cb)
    print 'added popdown'
    popup_timer = 0
    return False
def popsite_motion(w, context, x, y, time):
    global popup_timer
    if not popup_timer:
        popup_timer = gobject.timeout_add(500, popup_cb)
    return True
def popsite_leave(w, context, time):
    global popup_timer
    if popup_timer:
        gobject.source_remove(popup_timer)
        popup_timer = 0
def source_drag_data_delete(w, context, data):
    print 'Delete the data!'
def create_pixmap(w, xpm):
    return gtk.gdk.pixmap_create_from_xpm_d(w.window, None, xpm)
def main():
    global trashcan_open
    global trashcan_closed
    global drag_icon
    win = gtk.Window()
    win.realize()
    win.connect('destroy', lambda w: gtk.main_quit())
    table = gtk.Table(2,2)
    win.add(table)
    drag_icon = gtk.gdk.pixbuf_new_from_xpm_data(drag_icon_xpm)
    trashcan_open = gtk.gdk.pixbuf_new_from_xpm_data(trashcan_open_xpm)
    trashcan_closed = gtk.gdk.pixbuf_new_from_xpm_data(trashcan_closed_xpm)
    label = gtk.Label('Drop on Trashcan!\n')
    label.drag_dest_set(gtk.DEST_DEFAULT_ALL, target[:-1],
                        gtk.gdk.ACTION_COPY | gtk.gdk.ACTION_MOVE)
    label.connect('drag_data_received', label_drag_data_received)
    table.attach(label, 0, 1, 0, 1)

    label = gtk.Label('Popup\n')
    label.drag_dest_set(gtk.DEST_DEFAULT_ALL, target[:-1],
                        gtk.gdk.ACTION_COPY | gtk.gdk.ACTION_MOVE)
    table.attach(label, 1, 2, 1, 2)
    label.connect('drag_motion', popsite_motion)
    label.connect('drag_leave', popsite_leave)

    image = gtk.Image()
    image.set_from_pixbuf(trashcan_closed)
    image.drag_dest_set(0, [], 0)
    table.attach(image, 1, 2, 0, 1)
    image.connect('drag_leave', target_drag_leave)
    image.connect('drag_motion', target_drag_motion)
    image.connect('drag_drop', target_drag_drop)
    image.connect('drag_data_received', target_drag_data_received)

    b = gtk.Button('Drag from Here\n')
    b.drag_source_set(gtk.gdk.BUTTON1_MASK|gtk.gdk.BUTTON3_MASK, target,
                      gtk.gdk.ACTION_COPY|gtk.gdk.ACTION_MOVE)
    b.drag_source_set_icon_pixbuf(drag_icon)
    table.attach(b, 0, 1, 1, 2)
    b.connect('drag_data_get', source_drag_data_get)
    b.connect('drag_data_delete', source_drag_data_delete)
    win.show_all()
main()
gtk.main()
