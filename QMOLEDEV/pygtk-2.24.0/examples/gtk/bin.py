# A simple gtk.Conatiner subclassing example reimplementing gtk.Bin in python

import pygtk
pygtk.require('2.0')
import gtk

class Bin(gtk.Container):
    __gtype_name__ = 'PyGtkBin'
    def __init__(self):
        gtk.Container.__init__(self)
        self.child = None

    def do_add(self, child):
        child.set_parent(self)
        self.child = child

    def do_remove(self, child):
        widget_was_visible = child.flags() & gtk.VISIBLE
        child.unparent()
        self.child = None

        if widget_was_visible:
            self.queue_resize()

    def do_forall(self, internal, callback, data):
        if self.child:
            callback(self.child, data)

label = gtk.Label()
c = Bin()
c.add(label)
print c.get_children()
c.remove(label)
