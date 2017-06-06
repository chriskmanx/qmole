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

# This program updates the version number in all the places it needs to be
# updated. It takes a single command-line argument, which is the new version
# number. For example:
# python install_scripts/utils/version_update.py X.YY

import os
import sys

VERSION = os.path.join("share", "zenmap", "config", "zenmap_version")
VERSION_PY = os.path.join("zenmapCore", "Version.py")

def update_version(base_dir, version):
    print ">>> Updating %s" % os.path.join(base_dir, VERSION)
    vf = open(os.path.join(base_dir, VERSION), "wb")
    print >> vf, version
    vf.close()
    print ">>> Updating %s" % os.path.join(base_dir, VERSION_PY)
    vf = open(os.path.join(base_dir, VERSION_PY), "w")
    print >> vf, "VERSION = \"%s\"" % version
    vf.close()

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print >> sys.stderr, "Usage: %s <version>" % sys.argv[0]
        sys.exit(1)

    version = sys.argv[1]
    print ">>> Updating version number to \"%s\"" % version
    update_version(".", version)
