#!/usr/bin/env python
'''Drag and Drop

This is a test of the drag and drop capabilities of gtk.  It is a
fairly straight forward port of the example distributed with gtk.
'''

import pygtk
pygtk.require('2.0')
import gtk
import gobject

from dndpixmap import drag_icon_xpm, trashcan_open_xpm, trashcan_closed_xpm

TARGET_STRING = 0
TARGET_ROOTWIN = 1

target = [
    ('STRING', 0, TARGET_STRING),
    ('text/plain', 0, TARGET_STRING),
    ('application/x-rootwin-drop', 0, TARGET_ROOTWIN)
]

def create_pixmap(widget, xpm_data):
    return \
        gtk.gdk.pixmap_colormap_create_from_xpm_d(
            None, widget.get_colormap(), None, xpm_data)

class DragAndDropDemo(gtk.Window):
    trashcan_open = None
    trashcan_open_mask = None
    trashcan_closed = None
    trashcan_closed_mask = None
    drag_icon = None
    drag_mask = None
    have_drag = False
    popped_up = False
    in_popup = False
    popup_timer = 0
    popdown_timer = 0
    popup_win = None

    def __init__(self, parent=None):
        gtk.Window.__init__(self)
        try:
            self.set_screen(parent.get_screen())
        except AttributeError:
            self.connect('destroy', lambda *w: gtk.main_quit())
        self.set_title(self.__class__.__name__)

        table = gtk.Table(2,2)
        self.add(table)

        self.drag_icon, self.drag_mask = \
            create_pixmap(self, drag_icon_xpm)
        self.trashcan_open, self.trashcan_open_mask = \
            create_pixmap(self, trashcan_open_xpm)
        self.trashcan_closed, self.trashcan_closed_mask = \
            create_pixmap(self, trashcan_closed_xpm)

        label = gtk.Label('Drop to Trashcan!\n')
        label.drag_dest_set(gtk.DEST_DEFAULT_ALL, target[:-1],
                gtk.gdk.ACTION_COPY | gtk.gdk.ACTION_MOVE)
        label.connect('drag_data_received', self.label_drag_data_received)
        table.attach(label, 0, 1, 0, 1)

        label = gtk.Label('Popup\n')
        label.drag_dest_set(gtk.DEST_DEFAULT_ALL, target[:-1],
                gtk.gdk.ACTION_COPY | gtk.gdk.ACTION_MOVE)
        table.attach(label, 1, 2, 1, 2)
        label.connect('drag_motion', self.popsite_motion)
        label.connect('drag_leave', self.popsite_leave)

        image = gtk.Image()
        image.set_from_pixmap(self.trashcan_closed, self.trashcan_closed_mask)
        image.drag_dest_set(0, [], 0)
        table.attach(image, 1, 2, 0, 1)
        image.connect('drag_leave', self.target_drag_leave)
        image.connect('drag_motion', self.target_drag_motion)
        image.connect('drag_drop', self.target_drag_drop)
        image.connect('drag_data_received', self.target_drag_data_received)

        b = gtk.Button('Drag from Here\n')
        b.drag_source_set(gtk.gdk.BUTTON1_MASK | gtk.gdk.BUTTON3_MASK,
                  target, gtk.gdk.ACTION_COPY | gtk.gdk.ACTION_MOVE)
        b.drag_source_set_icon(self.get_colormap(), self.drag_icon, self.drag_mask)
        table.attach(b, 0, 1, 1, 2)
        b.connect('drag_data_get', self.source_drag_data_get)
        b.connect('drag_data_delete', self.source_drag_data_delete)
        self.show_all()

    def label_drag_data_received(self, w, context, x, y, data, info, time):
        if data and data.format == 8:
            print 'Received "%s" in label' % data.data
            context.finish(True, False, time)
        else:
            context.finish(False, False, time)

    def popsite_motion(self, w, context, x, y, time):
        if not self.popup_timer:
            self.popup_timer = gobject.timeout_add(500, self.popup_cb)
        return True

    def popsite_leave(self, w, context, time):
        if self.popup_timer:
            gobject.source_remove(self.popup_timer)
            self.popup_timer = 0

    def popup_motion(self, w, context, x, y, time):
        print 'popup_motion'
        if not self.in_popup:
            self.in_popup = True
            if self.popdown_timer:
                print 'removed popdown'
                gobject.source_remove(self.popdown_timer)
                self.popdown_timer = 0
        return True

    def popup_leave(self, w, context, time):
        print 'popup_leave'
        if self.in_popup:
            self.in_popup = False
            if not self.popdown_timer:
                print 'added popdown'
                self.popdown_timer = gobject.timeout_add(500, self.popdown_cb)

    def popup_cb(self):
        if not self.popped_up:
            if self.popup_win is None:
                self.popup_win = gtk.Window(gtk.WINDOW_POPUP)
                self.popup_win.set_position(gtk.WIN_POS_MOUSE)
                table = gtk.Table(3, 3)
                for k in range(9):
                    i, j = divmod(k, 3)
                    b = gtk.Button("%d,%d" % (i,j))
                    b.drag_dest_set(gtk.DEST_DEFAULT_ALL, target[:-1],
                        gtk.gdk.ACTION_COPY | gtk.gdk.ACTION_MOVE)
                    b.connect('drag_motion', self.popup_motion)
                    b.connect('drag_leave', self.popup_leave)
                    table.attach(b, i, i+1, j, j+1)
                table.show_all()
                self.popup_win.add(table)
            self.popup_win.present()
            self.popped_up = True
        self.popdown_timer = gobject.timeout_add(500, self.popdown_cb)
        print 'added popdown'
        self.popup_timer = 0
        return False

    def popdown_cb(self):
        print 'popdown'
        #if self.in_popup:
        #    return True
        self.popdown_timer = 0
        self.popup_win.hide()
        self.popped_up = False
        return False

    def target_drag_leave(self, img, context, time):
        print 'leave'
        self.have_drag = False
        img.set_from_pixmap(self.trashcan_closed, self.trashcan_closed_mask)

    def target_drag_motion(self, img, context, x, y, time):
        if self.have_drag is False:
            self.have_drag = True
            img.set_from_pixmap(self.trashcan_open, self.trashcan_open_mask)
        source_widget = context.get_source_widget()
        print 'motion, source ',
        if source_widget:
            print source_widget.__class__.__name__
        else:
            print 'unknown'
        context.drag_status(context.suggested_action, time)
        return True

    def target_drag_drop(self, img, context, x, y, time):
        print 'drop'
        self.have_drag = False
        img.set_from_pixmap(self.trashcan_closed, self.trashcan_closed_mask)
        if context.targets:
            img.drag_get_data(context, context.targets[0], time)
            return True
        return False

    def target_drag_data_received(self, img, context, x, y, data, info, time):
        if data.format == 8:
            print 'Received "%s" in trashcan' % data.data
            context.finish(True, False, time)
        else:
            context.finish(False, False, time)

    def source_drag_data_get(self, btn, context, selection_data, info, time):
        if info == TARGET_ROOTWIN:
            print 'I was dropped on the rootwin'
        else:
            selection_data.set(selection_data.target, 8, "I'm Data!")

    def source_drag_data_delete(self, btn, context, data):
        print 'Delete the data!'

def main():
    DragAndDropDemo()
    gtk.main()

if __name__ == '__main__':
    main()
