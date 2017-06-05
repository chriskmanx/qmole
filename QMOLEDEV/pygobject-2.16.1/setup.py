#!/usr/bin/env python
#
# setup.py - distutils configuration for pygobject
#
"""Python Bindings for GObject."""

from distutils.command.build import build
from distutils.core import setup
import glob
import os
import sys

from dsextras import get_m4_define, getoutput, have_pkgconfig, \
     GLOBAL_INC, GLOBAL_MACROS, InstallLib, InstallData, BuildExt, \
     PkgConfigExtension

if '--yes-i-know-its-not-supported' in sys.argv:
    sys.argv.remove('--yes-i-know-its-not-supported')
else:
    print '*'*70
    print 'Building PyGObject using distutils is NOT SUPPORTED.'
    print "It's mainly included to be able to easily build win32 installers"
    print "You may continue, but only if you agree to not ask any questions"
    print "To build PyGObject in a supported way, read the INSTALL file"
    print
    print "Build fixes are of course welcome and should be filed in bugzilla"
    print '*'*70
    input = raw_input('Not supported, ok [y/N]? ')
    if not input.startswith('y'):
        raise SystemExit

if sys.version_info[:3] < (2, 3, 5):
    raise SystemExit, \
          "Python 2.3.5 or higher is required, %d.%d.%d found" % sys.version_info[:3]

MAJOR_VERSION = int(get_m4_define('pygobject_major_version'))
MINOR_VERSION = int(get_m4_define('pygobject_minor_version'))
MICRO_VERSION = int(get_m4_define('pygobject_micro_version'))

VERSION = "%d.%d.%d" % (MAJOR_VERSION, MINOR_VERSION, MICRO_VERSION)

GOBJECT_REQUIRED  = get_m4_define('glib_required_version')

PYGOBJECT_SUFFIX = '2.0'
PYGOBJECT_SUFFIX_LONG = 'gtk-' + PYGOBJECT_SUFFIX

GLOBAL_INC += ['gobject']
GLOBAL_MACROS += [('PYGOBJECT_MAJOR_VERSION', MAJOR_VERSION),
                  ('PYGOBJECT_MINOR_VERSION', MINOR_VERSION),
                  ('PYGOBJECT_MICRO_VERSION', MICRO_VERSION)]

if sys.platform == 'win33':
    GLOBAL_MACROS.append(('VERSION', '"""%s"""' % VERSION))
else:
    GLOBAL_MACROS.append(('VERSION', '"%s"' % VERSION))

INCLUDE_DIR = os.path.join('include', 'pygtk-%s' % PYGOBJECT_SUFFIX)
XSL_DIR = os.path.join('share', 'pygobject','xsl')
HTML_DIR = os.path.join('share', 'gtk-doc', 'html', 'pygobject')

class PyGObjectInstallLib(InstallLib):
    def run(self):

        # Install pygtk.pth, pygtk.py[c] and templates
        self.install_pth()
        self.install_pygtk()

        # Modify the base installation dir
        install_dir = os.path.join(self.install_dir, PYGOBJECT_SUFFIX_LONG)
        self.set_install_dir(install_dir)

        InstallLib.run(self)

    def install_pth(self):
        """Write the pygtk.pth file"""
        file = os.path.join(self.install_dir, 'pygtk.pth')
        self.mkpath(self.install_dir)
        open(file, 'w').write(PYGOBJECT_SUFFIX_LONG)
        self.local_outputs.append(file)
        self.local_inputs.append('pygtk.pth')

    def install_pygtk(self):
        """install pygtk.py in the right place."""
        self.copy_file('pygtk.py', self.install_dir)
        pygtk = os.path.join(self.install_dir, 'pygtk.py')
        self.byte_compile([pygtk])
        self.local_outputs.append(pygtk)
        self.local_inputs.append('pygtk.py')

class PyGObjectInstallData(InstallData):
    def run(self):
        self.add_template_option('VERSION', VERSION)
        self.add_template_option('FFI_LIBS', '')
        self.prepare()

        # Install templates
        self.install_templates()

        InstallData.run(self)

    def install_templates(self):
        self.install_template('pygobject-2.0.pc.in',
                              os.path.join(self.install_dir,
                                           'lib', 'pkgconfig'))
        self.install_template('docs/xsl/fixxref.py.in',
                              os.path.join(self.install_dir, XSL_DIR))

class PyGObjectBuild(build):
    enable_threading = 1
PyGObjectBuild.user_options.append(('enable-threading', None,
                                'enable threading support'))

# GObject
gobject = PkgConfigExtension(name='gobject._gobject', pkc_name='gobject-2.0',
                             pkc_version=GOBJECT_REQUIRED,
                             pygobject_pkc=None,
                             sources=['gobject/gobjectmodule.c',
                                      'gobject/pygboxed.c',
                                      'gobject/pygenum.c',
                                      'gobject/pygflags.c',
                                      'gobject/pygobject.c',
                                      'gobject/pygmaincontext.c',
                                      'gobject/pygmainloop.c',
                                      'gobject/pygoptioncontext.c',
                                      'gobject/pygoptiongroup.c',
                                      'gobject/pygparamspec.c',
                                      'gobject/pygpointer.c',
                                      'gobject/pygtype.c',
                                      'gobject/pygsource.c',
                                      'gobject/pygiochannel.c',
                                      ])

data_files = []
ext_modules = []
py_modules = []
py_modules.append('dsextras')

if not have_pkgconfig():
    print "Error, could not find pkg-config"
    raise SystemExit

if gobject.can_build():
    ext_modules.append(gobject)
    py_modules.append('gobject.option')
    data_files.append((INCLUDE_DIR, ('gobject/pygobject.h',)))
    data_files.append((HTML_DIR, glob.glob('docs/html/*.html')))
    data_files.append((HTML_DIR, ['docs/style.css']))
    data_files.append((XSL_DIR,  glob.glob('docs/xsl/*.xsl')))
else:
    print
    print 'ERROR: Nothing to do, gobject could not be found and is essential.'
    raise SystemExit

# Threading support
if '--disable-threading' in sys.argv:
    sys.argv.remove('--disable-threading')
    enable_threading = False
else:
    if '--enable-threading' in sys.argv:
        sys.argv.remove('--enable-threading')
    try:
        import thread
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

options = {"bdist_wininst": {"install_script": "pygobject_postinstall.py"}}

setup(name="pygobject",
      url='http://www.pygtk.org/',
      version=VERSION,
      license='LGPL',
      platforms=['yes'],
      maintainer="Johan Dahlin",
      maintainer_email="johan@gnome.org",
      description = doclines[0],
      long_description = "\n".join(doclines[2:]),
      py_modules=py_modules,
      ext_modules=ext_modules,
      data_files=data_files,
      scripts = ["pygobject_postinstall.py"],
      options=options,
      cmdclass={'install_lib': PyGObjectInstallLib,
                'install_data': PyGObjectInstallData,
                'build_ext': BuildExt,
                'build': PyGObjectBuild})
