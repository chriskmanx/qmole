#
# Reimplementation of gtk.Layout in python
# Example on how to implement a scrollable container in python
#
# Johan Dahlin <johan@gnome.org>, 2006
#
# Requires PyGTK 2.8.0 or later

import pygtk
pygtk.require('2.0')
import gobject
import gtk
from gtk import gdk

class Child:
    widget = None
    x = 0
    y = 0

def set_adjustment_upper(adj, upper, always_emit):
    changed = False
    value_changed = False

    min = max(0.0, upper - adj.page_size)

    if upper != adj.upper:
        adj.upper = upper
        changed = True

    if adj.value > min:
        adj.value = min
        value_changed = True

    if changed or always_emit:
        adj.changed()
    if value_changed:
        adj.value_changed()

def new_adj():
    return gtk.Adjustment(0.0, 0.0, 0.0,
                          0.0, 0.0, 0.0)

class Layout(gtk.Container):
    __gsignals__ = dict(set_scroll_adjustments=
                        (gobject.SIGNAL_RUN_LAST, None,
                         (gtk.Adjustment, gtk.Adjustment)))
    def __init__(self):
        self._children = []
        self._width = 100
        self._height = 100
        self._hadj = None
        self._vadj = None
        self._bin_window = None
        self._hadj_changed_id = -1
        self._vadj_changed_id = -1
        gtk.Container.__init__(self)

        if not self._hadj or not self._vadj:
            self._set_adjustments(self._vadj or new_adj(),
                                  self._hadj or new_adj())

    # Public API

    def put(self, widget, x=0, y=0):
        child = Child()
        child.widget = widget
        child.x = x
        child.y = y
        self._children.append(child)

        if self.flags() & gtk.REALIZED:
            widget.set_parent_window(self._bin_window)

        widget.set_parent(self)

    def set_size(self, width, height):
        if self._width != width:
            self._width = width
        if self._height != height:
            self._height = height
        if self._hadj:
            set_adjustment_upper(self._hadj, self._width, False)
        if self._vadj:
            set_adjustment_upper(self._vadj, self._height, False)

        if self.flags() & gtk.REALIZED:
            self._bin_window.resize(max(width, self.allocation.width),
                                    max(height, self.allocation.height))

    # GtkWidget

    def do_realize(self):
        self.set_flags(gtk.REALIZED)

        self.window = gdk.Window(
            self.get_parent_window(),
            window_type=gdk.WINDOW_CHILD,
            x=self.allocation.x,
            y=self.allocation.y,
            width=self.allocation.width,
            height=self.allocation.height,
            wclass=gdk.INPUT_OUTPUT,
            colormap=self.get_colormap(),
            event_mask=gdk.VISIBILITY_NOTIFY_MASK)
        self.window.set_user_data(self)

        self._bin_window = gdk.Window(
            self.window,
            window_type=gdk.WINDOW_CHILD,
            x=int(-self._hadj.value),
            y=int(-self._vadj.value),
            width=max(self._width, self.allocation.width),
            height=max(self._height, self.allocation.height),
            colormap=self.get_colormap(),
            wclass=gdk.INPUT_OUTPUT,
            event_mask=(self.get_events() | gdk.EXPOSURE_MASK |
                        gdk.SCROLL_MASK))
        self._bin_window.set_user_data(self)

        self.set_style(self.style.attach(self.window))
        self.style.set_background(self.window, gtk.STATE_NORMAL)
        self.style.set_background(self._bin_window, gtk.STATE_NORMAL)

        for child in self._children:
            child.widget.set_parent_window(self._bin_window)
        self.queue_resize()

    def do_unrealize(self):
        self._bin_window.set_user_data(None)
        self._bin_window.destroy()
        self._bin_window = None
        gtk.Container.do_unrealize(self)

    def _do_style_set(self, style):
        gtk.Widget.do_style_set(self, style)

        if self.flags() & gtk.REALIZED:
            self.style.set_background(self._bin_window, gtk.STATE_NORMAL)

    def do_expose_event(self, event):
        if event.window != self._bin_window:
            return False
        gtk.Container.do_expose_event(self, event)
        return False

    def do_map(self):
        self.set_flags(gtk.MAPPED)
        for child in self._children:
            flags = child.widget.flags()
            if flags & gtk.VISIBLE:
                if not (flags & gtk.MAPPED):
                    child.widget.map()
        self._bin_window.show()
        self.window.show()

    def do_size_request(self, req):
        req.width = 0
        req.height = 0
        for child in self._children:
            child.widget.size_request()

    def do_size_allocate(self, allocation):
        self.allocation = allocation
        for child in self._children:
            self._allocate_child(child)

        if self.flags() & gtk.REALIZED:
            self.window.move_resize(*allocation)
            self._bin_window.resize(max(self._width, allocation.width),
                                    max(self._height, allocation.height))

        self._hadj.page_size = allocation.width
        self._hadj.page_increment = allocation.width * 0.9
        self._hadj.lower = 0
        set_adjustment_upper(self._hadj,
                             max(allocation.width, self._width), True)

        self._vadj.page_size = allocation.height
        self._vadj.page_increment = allocation.height * 0.9
        self._vadj.lower = 0
        self._vadj.upper = max(allocation.height, self._height)
        set_adjustment_upper(self._vadj,
                             max(allocation.height, self._height), True)

    def do_set_scroll_adjustments(self, hadj, vadj):
        self._set_adjustments(hadj, vadj)

    # GtkContainer

    def do_forall(self, include_internals, callback, data):
        for child in self._children:
            callback(child.widget, data)

    def do_add(self, widget):
        self.put(widget)

    def do_remove(self, widget):
        child = self._get_child_from_widget(widget)
        self._children.remove(child)
        widget.unparent()

    # Private

    def _set_adjustments(self, hadj, vadj):
        if not hadj and self._hadj:
            hadj = new_adj()

        if not vadj and self._vadj:
            vadj = new_adj()

        if self._hadj and self._hadj != hadj:
            self._hadj.disconnect(self._hadj_changed_id)

        if self._vadj and self._vadj != vadj:
            self._vadj.disconnect(self._vadj_changed_id)

        need_adjust = False

        if self._hadj != hadj:
            self._hadj = hadj
            set_adjustment_upper(hadj, self._width, False)
            self._hadj_changed_id = hadj.connect(
                "value-changed",
                self._adjustment_changed)
            need_adjust = True

        if self._vadj != vadj:
            self._vadj = vadj
            set_adjustment_upper(vadj, self._height, False)
            self._vadj_changed_id = vadj.connect(
                "value-changed",
                self._adjustment_changed)
            need_adjust = True

        if need_adjust and vadj and hadj:
            self._adjustment_changed()

    def _adjustment_changed(self, adj=None):
        if self.flags() & gtk.REALIZED:
            self._bin_window.move(int(-self._hadj.value),
                                  int(-self._vadj.value))
            self._bin_window.process_updates(True)

    def _get_child_from_widget(self, widget):
        for child in self._children:
            if child.widget == widget:
                return child
        else:
            raise AssertionError

    def _allocate_child(self, child):
        allocation = gdk.Rectangle()
        allocation.x = child.x
        allocation.y = child.y
        req = child.widget.get_child_requisition()
        allocation.width = req[0]
        allocation.height = req[1]
        child.widget.size_allocate(allocation)

Layout.set_set_scroll_adjustments_signal('set-scroll-adjustments')

def main():
    window = gtk.Window()
    window.set_size_request(300, 300)
    window.connect('delete-event', gtk.main_quit)

    sw = gtk.ScrolledWindow()
    sw.set_policy(gtk.POLICY_ALWAYS, gtk.POLICY_ALWAYS)
    window.add(sw)

    layout = Layout()
    layout.set_size(1000, 1000)
    sw.add(layout)

    b = gtk.Button('foobar')
    layout.put(b, 100, 100)

    window.show_all()
    gtk.main()

if __name__ == '__main__':
    main()
