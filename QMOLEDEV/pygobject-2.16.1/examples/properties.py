import gobject

class MyObject(gobject.GObject):

    foo = gobject.property(type=str, default='bar')
    boolprop = gobject.property(type=bool, default=False)

    def __init__(self):
        gobject.GObject.__init__(self)

    @gobject.property
    def readonly(self):
        return 'readonly'

gobject.type_register(MyObject)

print "MyObject properties: ", list(MyObject.props)

obj = MyObject()

print "obj.foo ==", obj.foo

obj.foo = 'spam'
print "obj.foo = spam"

print "obj.foo == ", obj.foo

print "obj.boolprop == ", obj.boolprop

print obj.readonly
obj.readonly = 'does-not-work'
