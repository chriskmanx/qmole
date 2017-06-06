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

from os.path import join, dirname

import errno
import os
import os.path
import sys
import shutil

from zenmapCore.UmitLogging import log
from zenmapCore.UmitConfigParser import UmitConfigParser
from zenmapCore.BasePaths import base_paths, HOME
from zenmapCore.I18N import _
from zenmapCore.Version import VERSION
from zenmapCore.Name import APP_NAME

# Find out the prefix under which data files (interface definition XML,
# pixmaps, etc.) are stored. This can vary depending on whether we are running
# in an executable package and what type of package it is, which we check using
# the sys.frozen attribute. See
# http://mail.python.org/pipermail/pythonmac-sig/2004-November/012121.html.
def get_prefix():
    frozen = getattr(sys, "frozen", None)
    if frozen == "macosx_app":
        # A py2app .app bundle.
        return os.path.join(dirname(sys.executable), "..", "Resources")
    elif frozen is not None:
        # Assume a py2exe executable.
        return dirname(sys.executable)
    else:
        # Normal script execution. Look in the current directory to allow
        # running from the distribution.
        return os.path.abspath(os.path.dirname(sys.argv[0]))

prefix = get_prefix()

# These lines are overwritten by the installer to hard-code the installed
# locations.
CONFIG_DIR = join(prefix, "share", APP_NAME, "config")
LOCALE_DIR = join(prefix, "share", APP_NAME, "locale")
MISC_DIR = join(prefix, "share", APP_NAME, "misc")
PIXMAPS_DIR = join(prefix, "share", "zenmap", "pixmaps")
DOCS_DIR = join(prefix, "share", APP_NAME, "docs")

def get_extra_executable_search_paths():
    """Return a list of additional executable search paths as a convenience for
    platforms where the default PATH is inadequate."""
    if sys.platform == 'darwin':
        return ["/usr/local/bin"]
    elif sys.platform == 'win32':
        return [dirname(dirname(sys.executable))]
    return []

#######
# Paths
class Paths(object):
    """Paths
    """
    hardcoded = ["config_dir",
                 "locale_dir",
                 "pixmaps_dir",
                 "misc_dir",
                 "docs_dir"]

    config_files_list = ["config_file",
                         "scan_profile",
                         "version"]

    empty_config_files_list = ["target_list",
                               "recent_scans",
                               "db"]

    misc_files_list = ["options",
                       "profile_editor",
                       "wizard"]

    def __init__(self):
        self.user_config_dir = os.path.join(base_paths['user_home'], base_paths['user_config_dir'])
        self.user_config_file = os.path.join(self.user_config_dir, base_paths['user_config_file'])
        self.config_dir = CONFIG_DIR
        self.locale_dir = LOCALE_DIR
        self.pixmaps_dir = PIXMAPS_DIR
        self.misc_dir = MISC_DIR
        self.docs_dir = DOCS_DIR

    def __getattr__(self, name):
        if name in self.hardcoded:
            return self.__dict__[name]

        elif name in self.config_files_list:
            return return_if_exists(join(self.user_config_dir, base_paths[name]))

        elif name in self.empty_config_files_list:
            return return_if_exists(join(self.user_config_dir, base_paths[name]), True)

        elif name in self.misc_files_list:
            return return_if_exists(join(self.misc_dir, base_paths[name]))

        try:
            return self.__dict__[name]
        except:
            raise NameError(name)

    def __setattr__(self, name, value):
        self.__dict__[name] = value

def create_dir(path):
    """Create a directory with os.makedirs without raising an error if the
        directory already exists."""
    try:
        os.makedirs(path)
    except OSError, e:
        if e.errno != errno.EEXIST:
            raise

def create_user_config_dir(user_dir, template_dir):
    """Create a user configuration directory by creating the directory if
    necessary, then copying all the files from the given template directory,
    skipping any that already exist."""
    log.debug(">>> Create user dir at %s" % user_dir)
    create_dir(user_dir)

    for filename in os.listdir(template_dir):
        template_filename = os.path.join(template_dir, filename)
        user_filename = os.path.join(user_dir, filename)
        # Only copy regular files.
        if not os.path.isfile(template_filename):
            continue
        # Don't overwrite existing files.
        if os.path.exists(user_filename):
            log.debug(">>> %s already exists." % user_filename)
            continue
        shutil.copyfile(template_filename, user_filename)
        log.debug(">>> Copy %s to %s." % (template_filename, user_filename))

def return_if_exists(path, create=False):
    path = os.path.abspath(path)
    if os.path.exists(path):
        return path
    elif create:
        f = open(path, "w")
        f.close()
        return path
    raise Exception("File '%s' does not exist or could not be found!" % path)

############
# Singleton!
Path = Paths()

if __name__ == '__main__':
    print ">>> SAVED DIRECTORIES:"
    print ">>> LOCALE DIR:", Path.locale_dir
    print ">>> PIXMAPS DIR:", Path.pixmaps_dir
    print ">>> CONFIG DIR:", Path.config_dir
    print
    print ">>> FILES:"
    print ">>> USER CONFIG FILE:", Path.user_config_file
    print ">>> CONFIG FILE:", Path.user_config_file
    print ">>> TARGET_LIST:", Path.target_list
    print ">>> PROFILE_EDITOR:", Path.profile_editor
    print ">>> WIZARD:", Path.wizard
    print ">>> SCAN_PROFILE:", Path.scan_profile
    print ">>> RECENT_SCANS:", Path.recent_scans
    print ">>> OPTIONS:", Path.options
    print
    print ">>> DB:", Path.db
    print ">>> VERSION:", Path.version
