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

import os
import os.path
import sys

from zenmapCore.Name import APP_NAME

# We can't just use os.path.expanduser(u"~") to get a unicode version of the
# home directory, because os.path.expanduser doesn't properly decode the raw
# byte string from the file system encoding. You get a UnicodeDecodeError on
# systems like Windows where the file system encoding is different from the
# result of sys.getdefaultencoding(). So we call os.path.expanduser with a plain
# byte string and decode it ourselves.
def get_home():
    fs_enc = sys.getfilesystemencoding()
    if fs_enc is None:
        fs_enc = "UTF-8"
    raw_home = os.path.expanduser("~")
    home = raw_home.decode(fs_enc)
    return home

HOME = get_home()

# The base_paths dict in this file gives symbolic names to various files. For
# example, use base_paths.target_list instead of 'target_list.txt'.

base_paths = dict(user_config_file = APP_NAME + '.conf',
                  user_config_dir = '.' + APP_NAME,
                  user_dir = os.path.join(HOME, '.' + APP_NAME),
                  scan_profile = 'scan_profile.usp',
                  profile_editor = 'profile_editor.xml',
                  recent_scans = 'recent_scans.txt',
                  target_list = 'target_list.txt',
                  wizard = 'wizard.xml',
                  options = 'options.xml',
                  user_home = HOME,
                  db = APP_NAME + ".db",
                  version = APP_NAME + "_version")
