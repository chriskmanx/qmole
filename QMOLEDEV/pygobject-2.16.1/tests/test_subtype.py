import unittest
import weakref
import gc

import testmodule
from common import gobject, testhelper

# FIXME: do not import gtk
import gtk


class _ClassInittableMetaType(gobject.GObjectMeta):
    def __init__(cls, name, bases, namespace):
        super(_ClassInittableMetaType, cls).__init__(name, bases, namespace)
        cls.__class_init__(namespace)


class ClassInittableObject(object):
    __metaclass__ = _ClassInittableMetaType
    def __class_init__(cls, namespace):
        pass
    __class_init__ = classmethod(__class_init__)


class TestSubType(unittest.TestCase):
    def testSubType(self):
        t = type('testtype', (gobject.GObject, gobject.GInterface), {})
        self.failUnless(issubclass(t, gobject.GObject))
        self.failUnless(issubclass(t, gobject.GInterface))

    def testGObject(self):
        label = gobject.GObject()
        self.assertEqual(label.__grefcount__, 1)
        label = gobject.new(gobject.GObject)
        self.assertEqual(label.__grefcount__, 1)

    def testPythonSubclass(self):
        label = testmodule.PyGObject()
        self.assertEqual(label.__grefcount__, 1)
        self.assertEqual(label.props.label, "hello")
        label = gobject.new(testmodule.PyGObject)
        self.assertEqual(label.__grefcount__, 1)
        self.assertEqual(label.props.label, "hello")

    def testCPyCSubclassing(self):
        obj = testhelper.create_test_type()
        self.assertEqual(obj.__grefcount__, 1)
        refcount = testhelper.test_g_object_new()
        self.assertEqual(refcount, 2)

    def testRegisterArgNotType(self):
        self.assertRaises(TypeError, gobject.type_register, 1)

    def testGObjectNewError(self):
        self.assertRaises(TypeError, gobject.new, gobject.GObject, text='foo')

    def testSubSubType(self):
        Object1 = type('Object1', (gobject.GObject,),
                       {'__gtype_name__': 'Object1'})
        Object2 = type('Object2', (Object1,),
                       {'__gtype_name__': 'Object2'})

        obj = Object2()
        self.failUnless(isinstance(obj, Object2))
        self.assertEqual(obj.__gtype__.name, 'Object2')

        obj = gobject.new(Object2)
        self.failUnless(isinstance(obj, Object2))
        self.assertEqual(obj.__gtype__.name, 'Object2')

    def testUnregisteredSubclass(self):
        class MyButton(gtk.Button):
            def custom_method(self):
                pass
        b = MyButton()
        self.assertEqual(type(b), MyButton)
        box = gtk.EventBox()
        box.add(b)
        del b
        b = box.child
        self.assertEqual(type(b), MyButton)
        try:
            b.custom_method()
        except AttributeError:
            self.fail()

    def testInstDict(self):
        b = gtk.Button()
        box = gtk.EventBox()
        box.add(b)
        b.xyz = "zbr"
        del b
        b = box.child
        self.assert_(hasattr(b, "xyz"))
        try:
            xyz = b.xyz
        except AttributeError:
            self.fail()
        self.assertEqual(xyz, "zbr")

    def testImmediateCollection(self):
        b = gtk.Button()
        bref = weakref.ref(b)
        while gc.collect():
            pass
        del b
        self.assertEqual(gc.collect(), 0)
        self.assertEqual(bref(), None)

    def testGCCollection(self):
        a = gtk.Button()
        b = gtk.Button()
        a.b = b
        b.a = a
        aref = weakref.ref(a)
        bref = weakref.ref(b)
        del a, b
        while gc.collect():
            pass
        self.assertEqual(aref(), None)
        self.assertEqual(bref(), None)

    def testWrapperUnref(self):
        b = gtk.Button()
        bref = weakref.ref(b)
        del b
        self.assertEqual(bref(), None)

    def testGObjectUnref(self):
        b = gtk.Button()
        bref = b.weak_ref()
        self.assert_(bref() is b)
        del b
        self.assertEqual(bref(), None)

    def testGCCollection(self):
        a = gtk.Button()
        b = gtk.Button()
        a.b = b
        b.a = a
        aref = a.weak_ref()
        bref = b.weak_ref()
        del a, b
        while gc.collect():
            pass
        self.assertEqual(aref(), None)
        self.assertEqual(bref(), None)

    def testGhostTwice(self):
        b = gtk.Button()
        bref = b.weak_ref()
        box = gtk.EventBox()
        box.add(b)
        del b
        b = box.child
        del b
        self.assertNotEqual(bref(), None)
        box.destroy()
        del box
        self.assertEqual(bref(), None)

    def testGhostWeakref(self):
        b = gtk.Button()
        bref = b.weak_ref()
        box = gtk.EventBox()
        box.add(b)
        del b
        b = bref()
        b.hide()
        del box
        b.hide()
        del b

    def testWeakRefCallback(self):
        def callback(a, b, c):
            self._wr_args = a, b, c
        self._wr_args = None
        b = gtk.Button()
        bref = b.weak_ref(callback, 1, 2, 3)
        del b
        self.assertEqual(self._wr_args, (1, 2, 3))


    def testCycle(self):

        class _TestCycle(gtk.EventBox):
            def __init__(self):
                gtk.EventBox.__init__(self)
                self.connect('notify', self.cb)

                class DetectDel:
                    def __del__(self):
                        pass
                        #print 'del'

                self.d = DetectDel()

            def cb(self, *args):
                pass

        a = _TestCycle()
        a_d_id = id(a.d)
        a.foo = "hello"
        b = gtk.EventBox()
        b.add(a)
        #print "__dict__1: refcount=%i id=%i" % (sys.getrefcount(a.__dict__), id(a.__dict__))

        del a
        while gc.collect():
            pass
        a = b.child
        #print "__dict__2: refcount=%i id=%i" % (sys.getrefcount(a.__dict__), id(a.__dict__))

        del a
        while gc.collect():
            pass
        a = b.child
        #print "__dict__3: refcount=%i id=%i" % (sys.getrefcount(a.__dict__), id(a.__dict__))

        self.assert_(hasattr(a, 'd'))
        self.assert_(hasattr(a, 'foo'))
        self.assertEqual(a.foo, "hello")
        self.assertEqual(id(a.d), a_d_id)

    def testSimpleDecref(self):
        class CallInDel:
            def __init__(self, callback):
                self.callback = callback

            def __del__(self):
                if callable(self.callback):
                    self.callback()
        disposed_calls = []
        def on_dispose():
            disposed_calls.append(None)
        gobj = gobject.GObject()
        gobj.set_data('tmp', CallInDel(on_dispose))
        del gobj
        assert len(disposed_calls) == 1

    def testDescriptor(self):
        # Test for bug #434659
        class GProperty(object):
            def __set__(self, instance, value):
                pass

        class C(gobject.GObject):
            str = GProperty()

        o = C()
        o.str = 'str'
        o.str = 'str'

    def testDescriptorV2(self):
        """http://bugzilla.gnome.org/show_bug.cgi?id=447271"""
        class Foo(gobject.GObject):
            def set_foo(self, foo):
                self._foo = foo
            def get_foo(self, foo):
                self._foo = foo
            fooprop = property(get_foo, set_foo)

        foo = Foo()
        foo.fooprop = 123

    def testGetDict(self):
        """reported in bug #466082"""
        class Foo(gobject.GObject):
            __gtype_name__ = 'Foo'
        foo = Foo()
        d = foo.__dict__

    def test_gtk_buildable_virtual_method(self):
        """Bug 566571."""

        # Currently the bug is not solved, so skip the test.
        return

        class CustomDialog(gtk.Dialog):
            __gtype_name__ = 'CustomDialog'
            def do_parser_finished(self, build):
                self.built = True

        builder = gtk.Builder()
        builder.add_from_string('<interface><object class="CustomDialog" id="main"/></interface>')
        dialog = builder.get_object('main')

        self.assert_(isinstance(dialog, gtk.Buildable))
        self.assert_(hasattr(dialog, 'built'))
