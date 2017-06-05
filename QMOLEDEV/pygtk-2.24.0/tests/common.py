import os
import sys

import pygtk
pygtk.require('2.0')
import gobject

def importModules(buildDir, srcDir):
    # Be very careful when you change this code, it's
    # fragile and the order is really significant

    # atk, pango
    sys.path.insert(0, buildDir)
    # _gtk, keysyms, glade
    sys.path.insert(0, os.path.join(buildDir, 'gtk'))
    sys.argv.append('--g-fatal-warnings')

    atk = importModule('atk', buildDir)
    pango = importModule('pango', buildDir)
    gtk = importModule('gtk', buildDir, 'gtk')
    gdk = importModule('gtk.gdk', buildDir, '_gdk.la')

    glade = importModule('gtk.glade', buildDir)

    globals().update(locals())

    os.environ['PYGTK_USE_GIL_STATE_API'] = ''
    gobject.threads_init()

def importModule(module, directory, name=None):
    global isDistCheck

    origName = module
    if '.' in module:
        fromlist = '.'.join(module.split('.')[:-1])
    else:
        fromlist = None

    if not name:
        name = module + '.la'

    try:
        obj = __import__(module, {}, {}, fromlist)
    except ImportError:
        raise

    if hasattr(obj, '__file__'):
        location = obj.__file__
    else:
        package = __import__(fromlist)
        location = os.path.join(package.__file__, name)

    current = os.getcwd()
    expected = os.path.abspath(os.path.join(current, location))
    current = os.path.abspath(location)
    if current != expected:
        raise AssertionError('module %s imported from wrong location. Expected %s, got %s' % (
                                 module, expected, current))

    return obj
