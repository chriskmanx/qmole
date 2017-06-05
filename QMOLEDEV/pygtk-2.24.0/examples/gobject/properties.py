import pygtk
pygtk.require('2.0')
import gobject

class MyObject(gobject.GObject):
    __gproperties__ = {
        'foo': (gobject.TYPE_STRING, 'foo property', 'the foo of the object',
                'bar', gobject.PARAM_READWRITE),
        'boolprop': (gobject.TYPE_BOOLEAN, 'bool prop', 'a test boolean prop',
                     0, gobject.PARAM_READABLE),
    }

    def __init__(self):
        self.__gobject_init__()
        self.foo = 'bar'
    def do_set_property(self, pspec, value):
        print '    do_set_property called for %s=%r' % (pspec.name, value)
        if pspec.name == 'foo':
            self.foo = value
        else:
            raise AttributeError, 'unknown property %s' % pspec.name
    def do_get_property(self, pspec):
        print '    do_get_property called for %s' % pspec.name
        if pspec.name == 'foo':
            return self.foo
        elif pspec.name == 'boolprop':
            return 1
        else:
            raise AttributeError, 'unknown property %s' % pspec.name
gobject.type_register(MyObject)

print "MyObject properties: ", gobject.list_properties(MyObject)
obj = MyObject()

val = obj.get_property('foo')
print "obj.get_property('foo') == ", val

obj.set_property('foo', 'spam')
print "obj.set_property('foo', 'spam')"

val = obj.get_property('foo')
print "obj.get_property('foo') == ", val

val = obj.get_property('boolprop')
print "obj.get_property('boolprop') == ", val
