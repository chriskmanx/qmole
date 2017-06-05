import unittest
import warnings

from gobject import GEnum, GFlags, GObject, GType, PARAM_READWRITE
from common import gobject, atk, gtk, gdk

class PObject(GObject):
    __gproperties__ = {
        'enum': (gtk.WindowType, 'blurb', 'description',
                 gtk.WINDOW_TOPLEVEL, PARAM_READWRITE),
        'enum2': (gtk.WindowType, 'blurb', 'description',
                  int(gtk.WINDOW_TOPLEVEL), PARAM_READWRITE),
        'flags': (gtk.AttachOptions, 'blurb', 'description',
                  gtk.EXPAND, PARAM_READWRITE),
        'flags2': (gtk.AttachOptions, 'blurb', 'description',
                   int(gtk.EXPAND), PARAM_READWRITE),
    }

class EnumTest(unittest.TestCase):
    def testEnums(self):
        self.failUnless(issubclass(GEnum, int))
        self.failUnless(isinstance(atk.LAYER_OVERLAY, atk.Layer))
        self.failUnless(isinstance(atk.LAYER_OVERLAY, int))
        self.failUnless('LAYER_OVERLAY' in repr(atk.LAYER_OVERLAY))
        self.failUnless(int(atk.LAYER_OVERLAY))
        self.assertEquals(atk.LAYER_INVALID, 0)
        self.assertNotEquals(atk.LAYER_INVALID, 1)
        self.assertNotEquals(atk.LAYER_INVALID, -1)
        self.assertNotEquals(atk.LAYER_INVALID, atk.LAYER_BACKGROUND)

    def testComparisionWarning(self):
        warnings.filterwarnings("error", "", Warning, "", 0)
        try:
            self.assertNotEquals(atk.LAYER_INVALID, atk.RELATION_NULL)
        except Warning:
            pass
        else:
            raise AssertionError
        warnings.resetwarnings()

    def testWindowGetState(self):
        win = gtk.Window()
        win.realize()

        state = win.window.get_state()
        self.assertEquals(state, gdk.WINDOW_STATE_WITHDRAWN)
        self.failUnless(isinstance(state, gdk.WindowState))
        self.failUnless('WINDOW_STATE_WITHDRAWN' in repr(state))

    def testProperty(self):
        win = gtk.Window()

        wtype = win.get_property('type')
        self.assertEquals(wtype, gtk.WINDOW_TOPLEVEL)
        self.failUnless(isinstance(wtype, gtk.WindowType))
        self.failUnless('WINDOW_TOPLEVEL' in repr(wtype))

    def testAtkObj(self):
        obj = atk.NoOpObject(GObject())
        self.assertEquals(obj.get_role(), atk.ROLE_INVALID)

    def testGParam(self):
        win = gtk.Window()
        enums = filter(lambda x: GType.is_a(x.value_type, GEnum),
                       gobject.list_properties(win))
        self.failUnless(enums)
        enum = enums[0]
        self.failUnless(hasattr(enum, 'enum_class'))
        self.failUnless(issubclass(enum.enum_class, GEnum))

    def testWeirdEnumValues(self):
        self.assertEquals(int(gdk.NOTHING), -1)
        self.assertEquals(int(gdk.BUTTON_PRESS), 4)

    def testParamSpec(self):
        props = filter(lambda prop: GType.is_a(prop.value_type, GEnum),
                       gobject.list_properties(gtk.Window))
        self.failUnless(len(props)>= 6)
        props = filter(lambda prop: prop.name == 'type', props)
        self.failUnless(props)
        pspec = props[0]
        klass = pspec.enum_class
        self.assertEquals(klass, gtk.WindowType)
        self.failUnless(hasattr(klass, '__enum_values__'))
        self.failUnless(isinstance(klass.__enum_values__, dict))
        self.failUnless(len(klass.__enum_values__) >= 2)
        self.failUnless(isinstance(pspec.default_value, gtk.WindowType))

    def testOutofBounds(self):
        val = gtk.icon_size_register('fake', 24, 24)
        self.failUnless(isinstance(val, gobject.GEnum))
        self.assertEquals(int(val), 7)
        self.failUnless('7' in repr(val))
        self.failUnless('GtkIconSize' in repr(val))

    def testEnumProperty(self):
        default = PObject.props.enum.default_value
        self.failUnless(isinstance(default, gtk.WindowType))
        self.assertEqual(default, gtk.WINDOW_TOPLEVEL)
        default = PObject.props.enum2.default_value
        self.failUnless(isinstance(default, gtk.WindowType))
        self.assertEqual(default, gtk.WINDOW_TOPLEVEL)

class FlagsTest(unittest.TestCase):
    def testFlags(self):
        self.failUnless(issubclass(GFlags, int))
        self.failUnless(isinstance(gdk.BUTTON_PRESS_MASK, gdk.EventMask))
        self.failUnless(isinstance(gdk.BUTTON_PRESS_MASK, int))
        self.assertEquals(gdk.BUTTON_PRESS_MASK, 256)
        self.assertNotEquals(gdk.BUTTON_PRESS_MASK, 0)
        self.assertNotEquals(gdk.BUTTON_PRESS_MASK, -256)
        self.assertNotEquals(gdk.BUTTON_PRESS_MASK, gdk.BUTTON_RELEASE_MASK)

        self.assertEquals(gdk.EventMask.__bases__[0], GFlags)
        self.assertEquals(len(gdk.EventMask.__flags_values__), 22)

    def testComparisionWarning(self):
        warnings.filterwarnings("error", "", Warning, "", 0)
        try:
            self.assertNotEquals(gtk.ACCEL_VISIBLE, gtk.EXPAND)
        except Warning:
            pass
        else:
            raise AssertionError
        warnings.resetwarnings()

    def testFlagOperations(self):
        a = gdk.BUTTON_PRESS_MASK
        self.failUnless(isinstance(a, GFlags))
        self.assertEquals(a.first_value_name, 'GDK_BUTTON_PRESS_MASK')
        self.assertEquals(a.first_value_nick, 'button-press-mask')
        self.assertEquals(a.value_names, ['GDK_BUTTON_PRESS_MASK'],
                          a.value_names)
        self.assertEquals(a.value_nicks, ['button-press-mask'],
                          a.value_names)
        b = gdk.BUTTON_PRESS_MASK | gdk.BUTTON_RELEASE_MASK
        self.failUnless(isinstance(b, GFlags))
        self.assertEquals(b.first_value_name, 'GDK_BUTTON_PRESS_MASK')
        self.assertEquals(b.first_value_nick, 'button-press-mask')
        self.assertEquals(b.value_names, ['GDK_BUTTON_PRESS_MASK',
                                          'GDK_BUTTON_RELEASE_MASK'])
        self.assertEquals(b.value_nicks, ['button-press-mask',
                                          'button-release-mask'])
        c = (gdk.BUTTON_PRESS_MASK |
             gdk.BUTTON_RELEASE_MASK |
             gdk.ENTER_NOTIFY_MASK)
        self.failUnless(isinstance(c, GFlags))
        self.assertEquals(c.first_value_name, 'GDK_BUTTON_PRESS_MASK')
        self.assertEquals(c.first_value_nick, 'button-press-mask')
        self.assertEquals(c.value_names,
                          ['GDK_BUTTON_PRESS_MASK',
                           'GDK_BUTTON_RELEASE_MASK',
                           'GDK_ENTER_NOTIFY_MASK'])
        self.assertEquals(c.value_nicks,
                          ['button-press-mask',
                           'button-release-mask',
                           'enter-notify-mask'])
        self.failUnless(int(a))
        self.assertEquals(int(a), int(gdk.BUTTON_PRESS_MASK))
        self.failUnless(int(b))
        self.assertEquals(int(b), (int(gdk.BUTTON_PRESS_MASK) |
                                   int(gdk.BUTTON_RELEASE_MASK)))
        self.failUnless(int(c))
        self.assertEquals(int(c), (int(gdk.BUTTON_PRESS_MASK) |
                                   int(gdk.BUTTON_RELEASE_MASK) |
                                   int(gdk.ENTER_NOTIFY_MASK)))

    def testUnsupportedOpertionWarning(self):
        warnings.filterwarnings("error", "", Warning, "", 0)
        try:
            value = gdk.BUTTON_PRESS_MASK + gdk.BUTTON_RELEASE_MASK
        except Warning:
            pass
        else:
            raise AssertionError
        warnings.resetwarnings()

    def testParamSpec(self):
        props = filter(lambda x: GType.is_a(x.value_type, GFlags),
                       gtk.Table.list_child_properties())
        self.failUnless(len(props) >= 2)
        pspec = props[0]
        klass = pspec.flags_class
        self.assertEquals(klass, gtk.AttachOptions)
        self.failUnless(hasattr(klass, '__flags_values__'))
        self.failUnless(isinstance(klass.__flags_values__, dict))
        self.failUnless(len(klass.__flags_values__) >= 3)
        self.failUnless(isinstance(pspec.default_value, gtk.AttachOptions))

    def testEnumComparision(self):
        enum = gtk.TREE_VIEW_DROP_BEFORE
        self.assertEquals(enum, 0)
        self.failUnless(not enum == 10)
        self.failUnless(not enum != 0)
        self.assertNotEquals(enum, 10)
        self.failUnless(not enum < 0)
        self.failUnless(enum < 10)
        self.failUnless(not enum > 0)
        self.failUnless(not enum > 10)
        self.failUnless(enum >= 0)
        self.failUnless(not enum >= 10)
        self.failUnless(enum <= 0)
        self.failUnless(enum <= 10)

    def testFlagComparision(self):
        flag = gdk.EXPOSURE_MASK
        self.assertEquals(flag, 2)
        self.failUnless(not flag == 10)
        self.failUnless(not flag != 2)
        self.assertNotEquals(flag, 10)
        self.failUnless(not flag < 2)
        self.failUnless(flag < 10)
        self.failUnless(not flag > 2)
        self.failUnless(not flag > 10)
        self.failUnless(flag >= 2)
        self.failUnless(not flag >= 10)
        self.failUnless(flag <= 2)
        self.failUnless(flag <= 10)

    def testFlagsProperty(self):
        default = PObject.props.flags.default_value
        self.failUnless(isinstance(default, gtk.AttachOptions))
        self.assertEqual(default, gtk.EXPAND)
        default = PObject.props.flags2.default_value
        self.failUnless(isinstance(default, gtk.AttachOptions))
        self.assertEqual(default, gtk.EXPAND)

if __name__ == '__main__':
    unittest.main()
