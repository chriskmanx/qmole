import pygtk; pygtk.require("2.0")
import gobject

class C(gobject.GObject):
    __gsignals__ = {
        'my_signal': (gobject.SIGNAL_RUN_FIRST, gobject.TYPE_NONE,
                      (gobject.TYPE_INT,))
    }
    def __init__(self):
        self.__gobject_init__() # default constructor using our new GType
    def do_my_signal(self, arg):
        print "C: class closure for `my_signal' called with argument", arg

class D(C):
    def do_my_signal(self, arg):
        print "D: class closure for `my_signal' called.  Chaining up to C"
        C.do_my_signal(self, arg)

def my_signal_handler(object, arg, *extra):
    print "handler for `my_signal' called with argument", arg, \
          "and extra args", extra

inst = C()
inst2 = D()

inst.connect("my_signal", my_signal_handler, 1, 2, 3)
inst.emit("my_signal", 42)
inst2.emit("my_signal", 42)
