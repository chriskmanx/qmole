#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# setup.py - distutils configuration for pygtk


'''Python Bindings for the GTK+ Widget Set.

PyGTK is a set of bindings for the GTK+ widget set. It provides an object
oriented interface that is slightly higher level than the C one. It
automatically does all the type casting and reference counting that you
would have to do normally with the C API. You can find out more on the
official homepage, http://www.pygtk.org/'''


import os
import sys
import glob

from distutils.command.build import build
from distutils.core import setup


# Check for windows platform
if sys.platform != 'win32':
    msg =  '*' * 68 + '\n'
    msg += '* Building PyGTK using distutils is only supported on windows. *\n'
    msg += '* To build PyGTK in a supported way, read the INSTALL file.    *\n'
    msg += '*' * 68
    raise SystemExit(msg)

# Check for python version
MIN_PYTHON_VERSION = (2, 6, 0)

if sys.version_info[:3] < MIN_PYTHON_VERSION:
    raise SystemExit('ERROR: Python %s or higher is required, %s found.' % (
                         '.'.join(map(str,MIN_PYTHON_VERSION)),
                         '.'.join(map(str,sys.version_info[:3]))))

# Check for pygobject (dsextras)
try:
    import pygtk
    pygtk.require('2.0')

    from dsextras import GLOBAL_MACROS, GLOBAL_INC, get_m4_define, getoutput, \
                         have_pkgconfig, pkgc_version_check, pkgc_get_defs_dir, \
                         PkgConfigExtension, Template, TemplateExtension, \
                         BuildExt, InstallLib, InstallData
except ImportError:
    raise SystemExit('ERROR: Could not import dsextras module: '
                     'Make sure you have installed pygobject.')

# Check for pkgconfig
if not have_pkgconfig():
    raise SystemExit('ERROR: Could not find pkg-config: '
                     'Please check your PATH environment variable.')


PYGTK_SUFFIX = '2.0'
PYGTK_SUFFIX_LONG = 'gtk-' + PYGTK_SUFFIX
PYGOBJECT_DEFSDIR = pkgc_get_defs_dir('pygobject-%s' % PYGTK_SUFFIX)

GOBJECT_REQUIRED   = get_m4_define('glib_required_version')
ATK_REQUIRED       = get_m4_define('atk_required_version')
PANGO_REQUIRED     = get_m4_define('pango_required_version')
GTK_REQUIRED       = get_m4_define('gtk_required_version')
LIBGLADE_REQUIRED  = get_m4_define('libglade_required_version')
PYCAIRO_REQUIRED   = get_m4_define('pycairo_required_version')
PYGOBJECT_REQUIRED = get_m4_define('pygobject_required_version')

MAJOR_VERSION = int(get_m4_define('pygtk_major_version'))
MINOR_VERSION = int(get_m4_define('pygtk_minor_version'))
MICRO_VERSION = int(get_m4_define('pygtk_micro_version'))
VERSION       = '%d.%d.%d' % (MAJOR_VERSION, MINOR_VERSION, MICRO_VERSION)

GLOBAL_INC += ['.', 'gtk']
GLOBAL_MACROS += [('PYGTK_MAJOR_VERSION', MAJOR_VERSION),
                  ('PYGTK_MINOR_VERSION', MINOR_VERSION),
                  ('PYGTK_MICRO_VERSION', MICRO_VERSION),
                  ('VERSION', '\\"%s\\"' % VERSION),
                  ('PLATFORM_WIN32', 1),
                  ('HAVE_BIND_TEXTDOMAIN_CODESET', 1)]

DEFS_DIR     = os.path.join('share', 'pygtk', PYGTK_SUFFIX, 'defs')
DEFS_INCLUDE_DIR = os.path.join(DEFS_DIR, 'gtk')
INCLUDE_DIR  = os.path.join('include', 'pygtk-%s' % PYGTK_SUFFIX)
HTML_DIR     = os.path.join('share', 'gtk-doc', 'html', 'pygtk')


class PyGtkInstallLib(InstallLib):
    def run(self):
        # Modify the base installation dir
        install_dir = os.path.join(self.install_dir, PYGTK_SUFFIX_LONG)
        self.set_install_dir(install_dir)

        # Install tests
        self.install_tests()

        InstallLib.run(self)

    def copy_test(self, srcfile, dstfile=None):
        if dstfile is None:
            dstfile = os.path.join(self.test_dir, srcfile)
        else:
            dstfile = os.path.join(self.test_dir, dstfile)

        srcfile = os.path.join('tests', srcfile)

        self.copy_file(srcfile, os.path.abspath(dstfile))
        self.local_outputs.append(dstfile)
        self.local_inputs.append('srcfile')

    def install_tests(self):
        self.test_dir = os.path.join(self.install_dir, 'tests', 'pygtk')
        self.mkpath(self.test_dir)

        self.copy_test('runtests-windows.py', 'runtests.py')
        self.copy_test('common-windows.py', 'common.py')

        for testfile in glob.glob('tests/test*.py'):
            self.copy_test(os.path.basename(testfile))

        for gladefile in glob.glob('tests/*.glade'):
            self.copy_test(os.path.basename(gladefile))


class PyGtkInstallData(InstallData):
    def run(self):
        self.add_template_option('VERSION', VERSION)
        self.prepare()

        # Install templates
        self.install_templates()

        InstallData.run(self)

    def install_templates(self):
        self.install_template('pygtk-%s.pc.in' % PYGTK_SUFFIX,
                              os.path.join(self.install_dir,
                                           'lib', 'pkgconfig'))

class PyGtkBuild(build):
    enable_threading = True

PyGtkBuild.user_options.append(('enable-threading', None,
                                'enable threading support'))

###################################################################
# FOR A NEW RELEASE, YOU USUALLY ONLY NEED TO CHANGE THE FOLLOWING
# These defs are registered with many modules...
gdk_defs = [
    'gtk/gdk-2.24.defs',
    'gtk/gdk-2.22.defs',
    'gtk/gdk-2.20.defs',
    'gtk/gdk-2.18.defs',
    'gtk/gdk-2.16.defs',
    'gtk/gdk-2.14.defs',
    'gtk/gdk-2.12.defs',
    'gtk/gdk-2.10.defs',
    'gtk/gdk-base.defs']
gtk_defs = [
    'gtk/gtk-2.24.defs',
    'gtk/gtk-2.22.defs',
    'gtk/gtk-2.20.defs',
    'gtk/gtk-2.18.defs',
    'gtk/gtk-2.16.defs',
    'gtk/gtk-2.14.defs',
    'gtk/gtk-2.12.defs',
    'gtk/gtk-2.10.defs',
    'gtk/gtk-base.defs']
gtk_types_defs = [
    'gtk/gtk-2.24-types.defs',
    'gtk/gtk-2.22-types.defs',
    'gtk/gtk-2.20-types.defs',
    'gtk/gtk-2.18-types.defs',
    'gtk/gtk-2.16-types.defs',
    'gtk/gtk-2.14-types.defs',
    'gtk/gtk-2.12-types.defs',
    'gtk/gtk-2.10-types.defs',
    'gtk/gtk-base-types.defs']
gtk_extra_defs = [
    'gtk/gtk-extrafuncs.defs']
####################################################################

# Atk
atk = TemplateExtension(name='atk',
                        pkc_name=('pygobject-%s' % PYGTK_SUFFIX, 'atk'),
                        pkc_version=(PYGOBJECT_REQUIRED, ATK_REQUIRED),
                        defs='atk.defs',
                        register=['atk-types.defs'],
                        override='atk.override',
                        sources=['atkmodule.c',
                                 'atk.c'],
                        py_ssize_t_clean=True)
# Pango
pango = TemplateExtension(name='pango',
                          pkc_name=('pygobject-%s' % PYGTK_SUFFIX, 'pango'),
                          pkc_version=(PYGOBJECT_REQUIRED, PANGO_REQUIRED),
                          defs='pango.defs',
                          register=['pango-types.defs'],
                          override='pango.override',
                          sources=['pangomodule.c',
                                   'pango.c'],
                          py_ssize_t_clean=True)
# Pangocairo
pangocairo = TemplateExtension(name='pangocairo',
                               pkc_name=('pycairo', 'pangocairo'),
                               pkc_version=(PYCAIRO_REQUIRED, PANGO_REQUIRED),
                               defs='pangocairo.defs',
                               register=['pango-types.defs'],
                               override='pangocairo.override',
                               sources=['pangocairomodule.c',
                                        'pangocairo.c'],
                               py_ssize_t_clean=True)

# Gdk (template only)
gdk_template = Template('gtk/gdk.override', 'gtk/gdk.c',
                        defs=('gtk/gdk.defs', gdk_defs),
                        prefix='pygdk',
                        register=['atk-types.defs',
                                  'pango-types.defs',
                                  ('gtk/gdk-types.defs', gdk_defs)],
                        py_ssize_t_clean=True)
# Gtk+
gtk_pkc_defs=('gtk/gtk.defs', gtk_defs)
gtk_pkc_register=['%s/gio-types.defs' % PYGOBJECT_DEFSDIR,
                  'atk-types.defs',
                  'pango-types.defs',
                  ('gtk/gdk-types.defs',['gtk/gdk-base-types.defs']),
                  ('gtk/gtk-types.defs', gtk_types_defs)]
libglade_pkc_register=[('gtk/gdk-types.defs',['gtk/gdk-base-types.defs']),
                       ('gtk/gtk-types.defs',gtk_types_defs),
                       'gtk/libglade.defs']

gtk = TemplateExtension(name='gtk',
                        pkc_name=('gtk+-%s' % PYGTK_SUFFIX, 'pycairo'),
                        pkc_version=(GTK_REQUIRED, PYCAIRO_REQUIRED),
                        output='gtk._gtk',
                        defs=gtk_pkc_defs,
                        sources=['gtk/gtkmodule.c',
                                 'gtk/gtkobject-support.c',
                                 'gtk/gtk-types.c',
                                 'gtk/pygtktreemodel.c',
                                 'gtk/pygtkcellrenderer.c',
                                 'gtk/gdk.c',
                                 'gtk/gtk.c'],
                        register=gtk_pkc_register,
                        override='gtk/gtk.override',
                        py_ssize_t_clean=True)
gtk.templates.append(gdk_template)

# Libglade
libglade = TemplateExtension(name='libglade',
                             pkc_name='libglade-%s' % PYGTK_SUFFIX,
                             pkc_version=LIBGLADE_REQUIRED,
                             output='gtk.glade',
                             defs='gtk/libglade.defs',
                             sources=['gtk/libglademodule.c',
                                      'gtk/libglade.c'],
                             register=libglade_pkc_register,
                             override='gtk/libglade.override',
                             py_ssize_t_clean=True)

data_files = []
ext_modules = []
py_modules = []
packages = []

if gtk.can_build():
    if '--disable-numpy' in sys.argv:
        sys.argv.remove('--disable-numpy')
    else:
        try:
            import numpy
            numpy # pyflakes
            GLOBAL_MACROS.append(('HAVE_NUMPY', 1))
            NUMPY_INCLUDE = os.path.join(os.path.dirname(numpy.__file__),
                                         'core', 'include')
            gtk.include_dirs.append(NUMPY_INCLUDE)
        except ImportError:
            print ('* Could not import numpy module, disabling numpy support.')

    ext_modules.append(gtk)
    data_files.append((os.path.join(INCLUDE_DIR, 'pygtk'), ('gtk/pygtk.h',)))
    data_files.append((DEFS_DIR, ['gtk/gdk.defs']))
    data_files.append((DEFS_DIR, ['gtk/gdk-types.defs']))
    data_files.append((DEFS_INCLUDE_DIR, ['gtk/gdk-base-types.defs']))
    data_files.append((DEFS_INCLUDE_DIR, gdk_defs))
    data_files.append((DEFS_DIR, ['gtk/gtk.defs']))
    data_files.append((DEFS_DIR, ['gtk/gtk-types.defs']))
    data_files.append((DEFS_INCLUDE_DIR, gtk_defs))
    data_files.append((DEFS_INCLUDE_DIR, gtk_types_defs))
    data_files.append((DEFS_INCLUDE_DIR, gtk_extra_defs))
    data_files.append((HTML_DIR, glob.glob('docs/html/*.html')))

    py_modules += ['gtk.compat', 'gtk.deprecation', 'gtk.keysyms',
                   'gtk._lazyutils']

    if atk.can_build():
        ext_modules.append(atk)
        data_files.append((DEFS_DIR, ('atk.defs', 'atk-types.defs')))
    else:
        raise SystemExit('ERROR: Nothing to do, atk could not be built and is essential.')

    if pango.can_build():
        ext_modules.append(pango)
        data_files.append((DEFS_DIR, ('pango.defs', 'pango-types.defs')))
        if pangocairo.can_build():
            ext_modules.append(pangocairo)
            data_files.append((DEFS_DIR, ('pangocairo.defs',)))
            GLOBAL_MACROS.append(('HAVE_PYCAIRO', 1))
        else:
            raise SystemExit('ERROR: Nothing to do, pangocairo could not be built and is essential.')
    else:
        raise SystemExit('ERROR: Nothing to do, pango could not be built and is essential.')

    if libglade.can_build():
        ext_modules.append(libglade)
        data_files.append((DEFS_DIR, ('gtk/libglade.defs',)))
else:
    raise SystemExit('ERROR: Nothing to do, gtk could not be built and is essential.')

# Threading support
if '--disable-threading' in sys.argv:
    sys.argv.remove('--disable-threading')
    enable_threading = False
else:
    if '--enable-threading' in sys.argv:
        sys.argv.remove('--enable-threading')
    try:
        import thread
        thread # pyflakes
    except ImportError:
        print ('* Could not import thread module, disabling threading.')
        enable_threading = False
    else:
        enable_threading = True

if enable_threading:
    name = 'gthread-%s' % PYGTK_SUFFIX

    for module in ext_modules:
        raw = getoutput('pkg-config --libs-only-l %s' % name)

        for arg in raw.split():
            if arg.startswith('-l'):
                module.libraries.append(arg[2:])
            else:
                module.extra_link_args.append(arg)

        raw = getoutput('pkg-config --cflags-only-I %s' % name)

        for arg in raw.split():
            if arg.startswith('-I'):
                module.include_dirs.append(arg[2:])
            else:
                module.extra_compile_args.append(arg)
else:
    GLOBAL_MACROS.append(('DISABLE_THREADING', 1))


doclines = __doc__.split('\n')
options = {'bdist_wininst': {'install_script': 'pygtk_postinstall.py',
                             'user_access_control': 'auto'}}

setup(name='pygtk',
      url='http://www.pygtk.org/',
      version=VERSION,
      license='LGPL',
      platforms=['MS Windows'],
      maintainer='James Henstridge',
      maintainer_email='james@daa.com.au',
      description=doclines[0],
      long_description='\n'.join(doclines[2:]),
      provides=['gtk', 'atk', 'pango', 'pangocairo'],
      requires=['pycairo (>=%s)' % PYCAIRO_REQUIRED,
                'pygobject (>=%s)' %PYGOBJECT_REQUIRED],
      py_modules=py_modules,
      packages=packages,
      ext_modules=ext_modules,
      data_files=data_files,
      scripts=['pygtk_postinstall.py'],
      options=options,
      cmdclass={'install_lib': PyGtkInstallLib,
                'install_data': PyGtkInstallData,
                'build_ext': BuildExt,
                'build': PyGtkBuild})
