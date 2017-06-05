#!/usr/bin/env python
#
# setup.py - distutils configuration for pygtk
#
# TODO:
# pygtk.spec(.in)
#
"""Python Bindings for the GTK Widget Set.

PyGTK is a set of bindings for the GTK widget set. It provides an object
oriented interface that is slightly higher level than the C one. It
automatically does all the type casting and reference counting that you
would have to do normally with the C API. You can find out more on the
official homepage, http://www.daa.com.au/~james/pygtk/"""

from distutils.command.build import build
from distutils.core import setup
import glob
import os
import sys

from dsextras import get_m4_define, getoutput, have_pkgconfig, \
     pkgc_version_check, \
     GLOBAL_INC, GLOBAL_MACROS, InstallLib, InstallData, BuildExt, \
     PkgConfigExtension, Template, TemplateExtension


if '--yes-i-know-its-not-supported' in sys.argv:
    sys.argv.remove('--yes-i-know-its-not-supported')
else:
    print '*'*70
    print 'Building PyGTK using distutils is NOT SUPPORTED.'
    print "It's mainly included to be able to easily build win32 installers"
    print "You may continue, but only if you agree to not ask any questions"
    print "To build PyGTK in a supported way, read the INSTALL file"
    print
    print "Build fixes are of course welcome and should be filed in bugzilla"
    print '*'*70
    input = raw_input('Not supported, ok [y/N]? ')
    if not input.startswith('y'):
        raise SystemExit

    if sys.version_info[:3] < (2, 3, 5):
        raise SystemExit, \
              "Python 2.3.5 or higher is required, %d.%d.%d found" % sys.version_info[:3]
    
MAJOR_VERSION = int(get_m4_define('pygtk_major_version'))
MINOR_VERSION = int(get_m4_define('pygtk_minor_version'))
MICRO_VERSION = int(get_m4_define('pygtk_micro_version'))

VERSION = "%d.%d.%d" % (MAJOR_VERSION, MINOR_VERSION, MICRO_VERSION)

GOBJECT_REQUIRED  = get_m4_define('glib_required_version')
ATK_REQUIRED      = get_m4_define('atk_required_version')
PANGO_REQUIRED    = get_m4_define('pango_required_version')
GTK_REQUIRED      = get_m4_define('gtk_required_version')
LIBGLADE_REQUIRED = get_m4_define('libglade_required_version')
PYCAIRO_REQUIRED  = get_m4_define('pycairo_required_version')

PYGTK_SUFFIX = '2.0'
PYGTK_SUFFIX_LONG = 'gtk-' + PYGTK_SUFFIX

GLOBAL_INC += ['.', 'gtk']
GLOBAL_MACROS += [('PYGTK_MAJOR_VERSION', MAJOR_VERSION),
                  ('PYGTK_MINOR_VERSION', MINOR_VERSION),
                  ('PYGTK_MICRO_VERSION', MICRO_VERSION)]

if sys.platform == 'win32':
    GLOBAL_MACROS.append(('VERSION', '"""%s"""' % VERSION))
    GLOBAL_MACROS.append(('PLATFORM_WIN32',1))
    GLOBAL_MACROS.append(('HAVE_BIND_TEXTDOMAIN_CODESET',1))
else:
    GLOBAL_MACROS.append(('VERSION', '"%s"' % VERSION))

DEFS_DIR    = os.path.join('share', 'pygtk', PYGTK_SUFFIX, 'defs')
INCLUDE_DIR = os.path.join('include', 'pygtk-%s' % PYGTK_SUFFIX)
HTML_DIR = os.path.join('share', 'gtk-doc', 'html', 'pygtk')

class PyGtkInstallLib(InstallLib):
    def run(self):

        # Install pygtk.pth, pygtk.py[c] and templates
        self.install_pth()

        # Modify the base installation dir
        install_dir = os.path.join(self.install_dir, PYGTK_SUFFIX_LONG)
        self.set_install_dir(install_dir)

        InstallLib.run(self)

    def install_pth(self):
        """Write the pygtk.pth file"""
        file = os.path.join(self.install_dir, 'pygtk.pth')
        self.mkpath(self.install_dir)
        open(file, 'w').write(PYGTK_SUFFIX_LONG)
        self.local_outputs.append(file)
        self.local_inputs.append('pygtk.pth')

class PyGtkInstallData(InstallData):
    def run(self):
        self.add_template_option('VERSION', VERSION)
        self.prepare()

        # Install templates
        self.install_templates()

        InstallData.run(self)

    def install_templates(self):
        self.install_template('pygtk-2.0.pc.in',
                              os.path.join(self.install_dir,
                                           'lib','pkgconfig'))

class PyGtkBuild(build):
    enable_threading = 1
PyGtkBuild.user_options.append(('enable-threading', None,
                                'enable threading support'))

# Atk
atk = TemplateExtension(name='atk', pkc_name='atk',
                        pkc_version=ATK_REQUIRED,
                        sources=['atkmodule.c', 'atk.c'],
                        register=['atk-types.defs'],
                        override='atk.override',
                        defs='atk.defs',
                        py_ssize_t_clean=True)
# Pango
pango = TemplateExtension(name='pango', pkc_name='pango',
                          pkc_version=PANGO_REQUIRED,
                          sources=['pango.c', 'pangomodule.c'],
                          register=['pango-types.defs'],
                          override='pango.override',
                          defs='pango.defs',
                          py_ssize_t_clean=True)
# Pangocairo
pangocairo = TemplateExtension(name='pangocairo',
                               pkc_name=('pycairo', 'pangocairo'),
                               pkc_version=(PYCAIRO_REQUIRED,
                                            PANGO_REQUIRED),
                               sources=['pangocairo.c', 'pangocairomodule.c'],
                               register=['pango-types.defs'],
                               override='pangocairo.override',
                               defs='pangocairo.defs',
                               py_ssize_t_clean=True)

# Gdk (template only)
gdk_template = Template('gtk/gdk.override', 'gtk/gdk.c',
                        defs=('gtk/gdk.defs',
                              ['gtk/gdk-2.10.defs','gtk/gdk-base.defs']),
                        prefix='pygdk',
                        register=['atk-types.defs',
                                  'pango-types.defs',
                                  ('gtk/gdk-types.defs',
                                   ['gtk/gdk-base-types.defs'])],
                        py_ssize_t_clean=True)
# Gtk+
if pangocairo.can_build():
    gtk_pkc_name=('gtk+-2.0','pycairo')
    gtk_pkc_version=(GTK_REQUIRED,PYCAIRO_REQUIRED)
else:
    gtk_pkc_name='gtk+-2.0'
    gtk_pkc_version=GTK_REQUIRED

if pkgc_version_check('gtk+-2.0', '2.10.0'):
    gtk_pkc_defs=('gtk/gtk.defs',['gtk/gtk-2.10.defs','gtk/gtk-base.defs'])
    gtk_pkc_register=['atk-types.defs',
                      'pango-types.defs',
                      ('gtk/gdk-types.defs',['gtk/gdk-base-types.defs']),
                      ('gtk/gtk-types.defs',['gtk/gtk-base-types.defs',
                                             'gtk/gtk-2.10-types.defs'])]
    libglade_pkc_register=[('gtk/gtk-types.defs',
                            ['gtk/gtk-base-types.defs',
                             'gtk/gtk-2.10-types.defs']),
                           'gtk/libglade.defs']
else:
    gtk_pkc_defs=('gtk/gtk.defs',['gtk/gtk-base.defs'])
    gtk_pkc_register=['atk-types.defs',
                      'pango-types.defs',
                      ('gtk/gdk-types.defs',['gtk/gdk-base-types.defs']),
                      ('gtk/gtk-types.defs',['gtk/gtk-base-types.defs'])]
    libglade_pkc_register=[('gtk/gtk-types.defs',['gtk/gtk-base-types.defs']),
                           'gtk/libglade.defs']

gtk = TemplateExtension(name='gtk', pkc_name=gtk_pkc_name,
                        pkc_version=gtk_pkc_version,
                        output='gtk._gtk',
                        sources=['gtk/gtkmodule.c',
                                 'gtk/gtkobject-support.c',
                                 'gtk/gtk-types.c',
                                 'gtk/pygtktreemodel.c',
                                 'gtk/pygtkcellrenderer.c',
                                 'gtk/gdk.c',
                                 'gtk/gtk.c'],
                        register=gtk_pkc_register,
                        override='gtk/gtk.override',
                        defs=gtk_pkc_defs,
                        py_ssize_t_clean=True)
gtk.templates.append(gdk_template)

# Libglade
libglade = TemplateExtension(name='libglade', pkc_name='libglade-2.0',
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

if not have_pkgconfig():
    print "Error, could not find pkg-config"
    raise SystemExit

if atk.can_build():
    ext_modules.append(atk)
    data_files.append((DEFS_DIR, ('atk.defs', 'atk-types.defs')))
if pango.can_build():
    ext_modules.append(pango)
    data_files.append((DEFS_DIR, ('pango.defs', 'pango-types.defs')))
    if pangocairo.can_build():
        ext_modules.append(pangocairo)
        data_files.append((DEFS_DIR, ('pangocairo.defs',)))
        GLOBAL_MACROS.append(('HAVE_PYCAIRO',1))
if gtk.can_build():
    if '--disable-numpy' in sys.argv:
        sys.argv.remove('--disable-numpy')
    else:
        try:
            import numpy
            numpy # pyflakes
            GLOBAL_MACROS.append(('HAVE_NUMPY', 1))
        except ImportError:
            print ('* numpy module could not be found, '
                   'will build without numpy support.')
    ext_modules.append(gtk)
    data_files.append((os.path.join(INCLUDE_DIR, 'pygtk'), ('gtk/pygtk.h',)))
    data_files.append((DEFS_DIR, ('gtk/gdk.defs', 'gtk/gdk-types.defs',
                                  'gtk/gdk-base.defs',
                                  'gtk/gdk-base-types.defs',
                                  'gtk/gtk.defs', 'gtk/gtk-types.defs',
                                  'gtk/gtk-2.10.defs',
                                  'gtk/gtk-2.10-types.defs',
                                  'gtk/gtk-base.defs',
                                  'gtk/gtk-base-types.defs',
                                  'gtk/gtk-extrafuncs.defs')))
    data_files.append((HTML_DIR, glob.glob('docs/html/*.html')))
    py_modules += ['gtk.compat', 'gtk.deprecation', 'gtk.keysyms',
                   'gtk._lazyutils']

if libglade.can_build():
    ext_modules.append(libglade)
    data_files.append((DEFS_DIR, ('gtk/libglade.defs',)))

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
        print "Warning: Could not import thread module, disabling threading"
        enable_threading = False
    else:
        enable_threading = True

if enable_threading:
    name = 'gthread-2.0'
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


doclines = __doc__.split("\n")

options = {"bdist_wininst": {"install_script": "pygtk_postinstall.py"}}

setup(name="pygtk",
      url='http://www.daa.com.au/~james/pygtk/',
      version=VERSION,
      license='LGPL',
      platforms=['yes'],
      maintainer="James Henstridge",
      maintainer_email="james@daa.com.au",
      description = doclines[0],
      long_description = "\n".join(doclines[2:]),
      py_modules=py_modules,
      packages=packages,
      ext_modules=ext_modules,
      data_files=data_files,
      scripts = ["pygtk_postinstall.py"],
      options=options,
      cmdclass={'install_lib': PyGtkInstallLib,
                'install_data': PyGtkInstallData,
                'build_ext': BuildExt,
                'build': PyGtkBuild})
