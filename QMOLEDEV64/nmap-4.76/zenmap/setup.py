#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

import errno
import sys
import os
import os.path
import re

import distutils.sysconfig
from distutils import log
from distutils.core import setup, Command
from distutils.command.install import install

from glob import glob
from stat import *

from zenmapCore.Version import VERSION
from zenmapCore.Name import APP_NAME, APP_DISPLAY_NAME, APP_WEB_SITE, APP_DOWNLOAD_SITE, NMAP_DISPLAY_NAME

# The name of the file used to record the list of installed files, so that the
# uninstall command can remove them.
INSTALLED_FILES_NAME = "INSTALLED_FILES"

# Directories for POSIX operating systems
# These are created after a "install" or "py2exe" command
# These directories are relative to the installation or dist directory
data_dir = os.path.join('share', APP_NAME)
pixmaps_dir = os.path.join(data_dir, 'pixmaps')
locale_dir = os.path.join(data_dir, 'locale')
config_dir = os.path.join(data_dir, 'config')
docs_dir = os.path.join(data_dir, 'docs')
misc_dir = os.path.join(data_dir, 'misc')

# Where to install .desktop files.
desktop_dir = os.path.join('share', 'applications')

def mo_find(result, dirname, fnames):
    files = []
    for f in fnames:
        p = os.path.join(dirname, f)
        if os.path.isfile(p) and f.endswith(".mo"):
            files.append(p)
        
    if files:
        result.append((dirname, files))

################################################################################
# Installation variables

data_files = [ (pixmaps_dir, glob(os.path.join(pixmaps_dir, '*.svg')) +
                             glob(os.path.join(pixmaps_dir, '*.png'))),

               (os.path.join(pixmaps_dir, "radialnet"),
                             glob(os.path.join(pixmaps_dir, "radialnet", '*.svg')) +
                             glob(os.path.join(pixmaps_dir, "radialnet", '*.png'))),

               (config_dir, [os.path.join(config_dir, APP_NAME + '.conf')] +
                            [os.path.join(config_dir, 'scan_profile.usp')] +
                            [os.path.join(config_dir, APP_NAME + '_version')]),

               (misc_dir, glob(os.path.join(misc_dir, '*.xml'))), 

               (docs_dir, [os.path.join(docs_dir, 'help.html')])]

# Add i18n files to data_files list
os.path.walk(locale_dir, mo_find, data_files)

# path_startswith and path_strip_prefix are used to deal with the installation
# root (--root option, also known as DESTDIR).

def path_startswith(path, prefix):
    """Returns True if path starts with prefix. It's a little more intelligent
    than str.startswith because it normalizes the paths to remove multiple
    directory separators and down-up traversals."""
    path = os.path.normpath(path)
    prefix = os.path.normpath(prefix)
    return path.startswith(prefix)

def path_strip_prefix(path, prefix):
    """Return path stripped of its directory prefix if it starts with prefix,
    otherwise return path unmodified. This only works correctly with Unix paths;
    for example it will not replace the drive letter on a Windows path.
    Examples:
    >>> path_strip_prefix('/tmp/destdir/usr/bin', '/tmp/destdir')
    '/usr/bin'
    >>> path_strip_prefix('/tmp/../tmp/destdir/usr/bin', '/tmp///destdir')
    '/usr/bin'
    >>> path_strip_prefix('/etc', '/tmp/destdir')
    '/etc'
    >>> path_strip_prefix('/etc', '/')
    '/etc'
    >>> path_strip_prefix('/etc', '')
    '/etc'
    """
    absolute = os.path.isabs(path)
    path = os.path.normpath(path)
    prefix = os.path.normpath(prefix)
    if path.startswith(prefix) and prefix != os.sep:
        path = path[len(prefix):]
    # Absolute paths must remain absolute and relative paths must remain
    # relative.
    assert os.path.isabs(path) == absolute
    return path

################################################################################
# Distutils subclasses

class my_install(install):
    def run(self):
        install.run(self)

        self.set_perms()
        self.set_modules_path()
        self.fix_paths()
        self.create_uninstaller()
        self.write_installed_files()

    def get_installed_files(self):
        """Return a list of installed files and directories, each prefixed with
        the installation root if given. The list of installed directories
        doesn't come from distutils so it may be incomplete."""
        installed_files = self.get_outputs()
        for package in self.distribution.packages:
            dir = package.replace(".", "/")
            installed_files.append(os.path.join(self.install_lib, dir))
        # Recursively include all the directories in data_dir (share/zenmap).
        # This is mainly for convenience in listing locale directories.
        installed_files.append(os.path.join(self.install_data, data_dir))
        for dirpath, dirs, files in os.walk(os.path.join(self.install_data, data_dir)):
            for dir in dirs:
                installed_files.append(os.path.join(dirpath, dir))
        installed_files.append(os.path.join(self.install_scripts, "uninstall_" + APP_NAME))
        return installed_files

    def create_uninstaller(self):
        uninstaller_filename = os.path.join(self.install_scripts, "uninstall_" + APP_NAME)

        uninstaller = """\
#!/usr/bin/env python
import errno, os, os.path, sys

print 'Uninstall %(name)s %(version)s'

answer = raw_input('Are you sure that you want to uninstall %(name)s %(version)s? (yes/no) ')

if answer != 'yes' and answer != 'y':
    print 'Not uninstalling.'
    sys.exit(0)

""" % {'name':APP_DISPLAY_NAME, 'version':VERSION}

        installed_files = []
        for output in self.get_installed_files():
            if self.root is not None:
                # If we have a root (DESTDIR), we need to strip it off the front
                # of paths so the uninstaller runs on the target host. The path
                # manipulations are tricky, but made easier because the
                # uninstaller only has to run on Unix.
                if not path_startswith(output, self.root):
                    # This should never happen (everything gets installed inside
                    # the root), but if it does, be safe and don't delete
                    # anything.
                    uninstaller += "print '%s was not installed inside the root %s; skipping.'\n" % (output, self.root)
                    continue
                output = path_strip_prefix(output, self.root)
                assert os.path.isabs(output)
            installed_files.append(output)

        uninstaller += """\
INSTALLED_FILES = (
"""
        for file in installed_files:
            uninstaller += "    %s,\n" % repr(file)
        uninstaller += """\
)

# Split the list into lists of files and directories.
files = []
dirs = []
for path in INSTALLED_FILES:
    if os.path.isfile(path) or os.path.islink(path):
        files.append(path)
    elif os.path.isdir(path):
        dirs.append(path)
# Delete the files.
for file in files:
    print "Removing '%s'." % file
    try:
        os.remove(file)
    except OSError, e:
        print >> sys.stderr, '  Error: %s.' % str(e)
# Delete the directories. First reverse-sort the normalized paths by
# length so that child directories are deleted before their parents.
dirs = [os.path.normpath(dir) for dir in dirs]
dirs.sort(key = len, reverse = True)
for dir in dirs:
    try:
        print "Removing the directory '%s'." % dir
        os.rmdir(dir)
    except OSError, e:
        if e.errno == errno.ENOTEMPTY:
            print "Directory '%s' not empty; not removing." % dir
        else:
            print >> sys.stderr, str(e)
"""

        uninstaller_file = open(uninstaller_filename, 'w')
        uninstaller_file.write(uninstaller)
        uninstaller_file.close()

        # Set exec bit for uninstaller
        mode = ((os.stat(uninstaller_filename)[ST_MODE]) | 0555) & 07777
        os.chmod(uninstaller_filename, mode)

    def set_modules_path(self):
        app_file_name = os.path.join(self.install_scripts, APP_NAME)
        # Find where the modules are installed. distutils will put them in
        # self.install_lib, but that path can contain the root (DESTDIR), so we
        # must strip it off if necessary.
        modules = self.install_lib
        if self.root is not None:
            modules = path_strip_prefix(modules, self.root)

        re_sys = re.compile("^import sys$")

        ufile = open(app_file_name, "r")
        ucontent = ufile.readlines()
        ufile.close()

        uline = None
        for line in xrange(len(ucontent)):
            if re_sys.match(ucontent[line]):
                uline = line + 1
                break

        ucontent.insert(uline, "sys.path.append(%s)\n" % repr(modules))

        ufile = open(app_file_name, "w")
        ufile.writelines(ucontent)
        ufile.close()

    def set_perms(self):
        re_bin = re.compile("(bin|\.sh)")
        for output in self.get_installed_files():
            if re_bin.findall(output):
                continue

            if os.path.isdir(output):
                os.chmod(output, S_IRWXU | \
                                 S_IRGRP | \
                                 S_IXGRP | \
                                 S_IROTH | \
                                 S_IXOTH)
            else:
                os.chmod(output, S_IRUSR | \
                                 S_IWUSR | \
                                 S_IRGRP | \
                                 S_IROTH)


    def fix_paths(self):
        """Replace some hardcoded paths to match where files were installed."""
        interesting_paths = {"CONFIG_DIR": os.path.join(self.prefix, config_dir),
                             "DOCS_DIR": os.path.join(self.prefix, docs_dir),
                             "LOCALE_DIR": os.path.join(self.prefix, locale_dir),
                             "MISC_DIR": os.path.join(self.prefix, misc_dir),
                             "PIXMAPS_DIR": os.path.join(self.prefix, pixmaps_dir)}

        # Find and read the Paths.py file.
        pcontent = ""
        paths_file = os.path.join("zenmapCore", "Paths.py")
        installed_files = self.get_outputs()
        for f in installed_files:
            if re.findall("(%s)" % re.escape(paths_file), f):
                paths_file = f
                pf = open(paths_file)
                pcontent = pf.read()
                pf.close()
                break

        # Replace the path definitions.
        for path, replacement in interesting_paths.items():
            pcontent = re.sub("%s\s+=\s+.+" % path,
                              "%s = %s" % (path, repr(replacement)),
                              pcontent)

        # Write the modified file.
        pf = open(paths_file, "w")
        pf.write(pcontent)
        pf.close()

        # Rewrite the zenmap.desktop and zenmap-root.desktop files to point to
        # the installed locations of the su-to-zenmap.sh script and application
        # icon.
        su_filename = os.path.join(self.prefix, data_dir, "su-to-zenmap.sh")
        icon_filename = os.path.join(self.prefix, pixmaps_dir, "zenmap.png")

        desktop_filename = None
        root_desktop_filename = None
        for f in installed_files:
            if re.search("%s$" % re.escape("zenmap-root.desktop"), f):
                root_desktop_filename = f
            elif re.search("%s$" % re.escape("zenmap.desktop"), f):
                desktop_filename = f

        if desktop_filename is not None:
            df = open(desktop_filename, "r")
            dcontent = df.read()
            df.close()
            regex = re.compile("^(Icon *= *).*$", re.MULTILINE)
            dcontent = regex.sub("\\1%s" % icon_filename, dcontent)
            df = open(desktop_filename, "w")
            df.write(dcontent)
            df.close()

        if root_desktop_filename is not None:
            df = open(root_desktop_filename, "r")
            dcontent = df.read()
            df.close()
            regex = re.compile("^((?:Exec|TryExec) *= *).*su-to-zenmap.sh(.*)$", re.MULTILINE)
            dcontent = regex.sub("\\1%s\\2" % su_filename, dcontent)
            regex = re.compile("^(Icon *= *).*$", re.MULTILINE)
            dcontent = regex.sub("\\1%s" % icon_filename, dcontent)
            df = open(root_desktop_filename, "w")
            df.write(dcontent)
            df.close()

    def write_installed_files(self):
        """Write a list of installed files for use by the uninstall command.
        This is similar to what happens with the --record option except that it
        doesn't strip off the installation root, if any. File names containing
        newline characters are not handled."""
        if INSTALLED_FILES_NAME == self.record:
            distutils.log.warn("warning: installation record is overwriting --record file '%s'." % self.record)
        f = open(INSTALLED_FILES_NAME, "w")
        try:
            for output in self.get_installed_files():
                assert "\n" not in output
                print >> f, output
        finally:
            f.close()

class my_uninstall(Command):
    """A distutils command that performs uninstallation. It reads the list of
    installed files written by the install command."""

    command_name = "uninstall"
    description = "uninstall installed files recorded in '%s'" % INSTALLED_FILES_NAME
    user_options = []

    def initialize_options(self):
        pass

    def finalize_options(self):
        pass

    def run(self):
        # Read the list of installed files.
        try:
            f = open(INSTALLED_FILES_NAME, "r")
        except IOError, e:
            if e.errno == errno.ENOENT:
                log.error("Couldn't open the installation record '%s'. Have you installed yet?" % INSTALLED_FILES_NAME)
                return
        installed_files = [file.rstrip("\n") for file in f.readlines()]
        f.close()
        # Delete the installation record too.
        installed_files.append(INSTALLED_FILES_NAME)
        # Split the list into lists of files and directories.
        files = []
        dirs = []
        for path in installed_files:
            if os.path.isfile(path) or os.path.islink(path):
                files.append(path)
            elif os.path.isdir(path):
                dirs.append(path)
        # Delete the files.
        for file in files:
            log.info("Removing '%s'." % file)
            try:
                if not self.dry_run:
                    os.remove(file)
            except OSError, e:
                log.error(str(e))
        # Delete the directories. First reverse-sort the normalized paths by
        # length so that child directories are deleted before their parents.
        dirs = [os.path.normpath(dir) for dir in dirs]
        dirs.sort(key = len, reverse = True)
        for dir in dirs:
            try:
                log.info("Removing the directory '%s'." % dir)
                if not self.dry_run:
                    os.rmdir(dir)
            except OSError, e:
                if e.errno == errno.ENOTEMPTY:
                    log.info("Directory '%s' not empty; not removing." % dir)
                else:
                    log.error(str(e))

# setup can be called in different ways depending on what we're doing. (For
# example py2exe needs special handling.) These arguments are common between all
# the operations.
COMMON_SETUP_ARGS = {
    'name': APP_NAME,
    'license': 'GNU GPL (version 2 or later)',
    'url': APP_WEB_SITE,
    'download_url': APP_DOWNLOAD_SITE,
    'author': 'Adriano Monteiro & Cleber Rodrigues',
    'author_email': 'py.adriano@gmail.com, cleber@globalred.com.br',
    'maintainer': 'Adriano Monteiro',
    'maintainer_email': 'py.adriano@gmail.com',
    'description': "%s frontend and results viewer" % APP_DISPLAY_NAME,
    'long_description': """\
%s is an %s frontend \
that is really useful for advanced users and easy to be used by newbies.""" \
% (APP_DISPLAY_NAME, NMAP_DISPLAY_NAME),
    'version': VERSION,
    'scripts': [APP_NAME],
    'packages': ['zenmapCore', 'zenmapGUI', 'zenmapGUI.higwidgets',
                 'radialnet', 'radialnet.bestwidgets', 'radialnet.core', 'radialnet.gui', 'radialnet.util'],
    'data_files': data_files,
}

# All of the arguments to setup are collected in setup_args.
setup_args = {}
setup_args.update(COMMON_SETUP_ARGS)

if 'py2exe' in sys.argv:
    # Windows- and py2exe-specific args.
    import py2exe

    WINDOWS_SETUP_ARGS = {
        'zipfile': None,
        'name': APP_NAME,
        'windows': [{"script": APP_NAME,
                     "icon_resources": [(1, "install_scripts/windows/nmap-eye.ico")]}],
        'options': {"py2exe": {
            "compressed": 1,
            "optimize":2,
            "packages":"encodings",
            "includes" : "\
pango,\
atk,\
gobject,\
pickle,\
bz2,\
encodings,\
encodings.*,\
cairo,\
pangocairo,\
atk\
"}}
    }

    setup_args.update(WINDOWS_SETUP_ARGS)
elif 'py2app' in sys.argv:
    # Args for Mac OS X and py2app.
    import py2app
    import shutil

    # py2app requires a ".py" suffix.
    extended_app_name = APP_NAME + ".py"
    shutil.copyfile(APP_NAME, extended_app_name)

    MACOSX_SETUP_ARGS = {
        'app': [extended_app_name],
        'options': {"py2app": {
            "packages": ["gobject", "gtk", "cairo"],
            "includes": ["atk", "pango", "pangocairo"],
            "argv_emulation": True,
            "compressed": True,
            "plist": "install_scripts/macosx/Info.plist",
            "iconfile": "install_scripts/macosx/zenmap.icns"
        }}
    }

    setup_args.update(MACOSX_SETUP_ARGS)
else:
    # Default args.
    DEFAULT_SETUP_ARGS = {
        'cmdclass': {'install': my_install, 'uninstall': my_uninstall},
    }
    setup_args.update(DEFAULT_SETUP_ARGS)

    data_files = [
        (desktop_dir, glob('install_scripts/unix/*.desktop')),
        (data_dir, ['install_scripts/unix/su-to-zenmap.sh'])
    ]
    setup_args["data_files"].extend(data_files)

setup(**setup_args)
