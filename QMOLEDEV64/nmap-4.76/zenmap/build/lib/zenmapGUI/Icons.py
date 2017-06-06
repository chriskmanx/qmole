#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
#         Cleber Rodrigues <cleber.gnu@gmail.com>
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

import gtk
import gobject
import re
import sys
import os.path

from zenmapCore.Paths import Path
from zenmapCore.UmitConf import is_maemo
from zenmapCore.UmitLogging import log

icon_names = (
# Operating Systems
    'default',
    'freebsd',
    'irix',
    'linux',
    'macosx',
    'openbsd',
    'redhat',
    'solaris',
    'ubuntu',
    'unknown',
    'win',
# Vulnerability Levels
    'vl_1',
    'vl_2',
    'vl_3',
    'vl_4',
    'vl_5')

pixmap_path = Path.pixmaps_dir
if pixmap_path:
    # This is a generator that returns file names for pixmaps in the order they
    # should be tried.
    def get_pixmap_file_names(icon_name, size):
        yield '%s.svg' % icon_name
        yield '%s_%s.png' % (icon_name, size)

    iconfactory = gtk.IconFactory()
    for icon_name in icon_names:
        for type, size in (('icon', '32'), ('logo', '75')):
            key = '%s_%s' % (icon_name, type)
            # Look for a usable image file.
            for file_name in get_pixmap_file_names(icon_name, size):
                file_path = os.path.join(pixmap_path, file_name)
                try:
                    pixbuf = gtk.gdk.pixbuf_new_from_file(file_path)
                    break
                except gobject.GError:
                    # Try again.
                    pass
            else:
                log.warn('Could not find the icon for %s at any of (%s) in %s' % (icon_name, ', '.join(get_pixmap_file_names(icon_name, size)), pixmap_path))
                continue
            iconset = gtk.IconSet(pixbuf)
            iconfactory.add(key, iconset)
            log.debug('Register %s icon name for file %s' % (key, file_path))
    iconfactory.add_default()

def get_os_icon(host):
    if not host:
        return get_os('','','icon')

    osfamily = host.get_best_osclass()['osfamily']
    osmatch = host.get_best_osmatch()['name']

    return get_os(osfamily, osmatch, 'icon')

def get_os_logo(host):
    if not host:
        return get_os('','','logo')

    osfamily = host.get_best_osclass()['osfamily']
    osmatch = host.get_best_osmatch()['name']

    return get_os(osfamily, osmatch, 'logo')

def get_os(osfamily, osmatch, type):
    if osfamily:
        if osfamily == 'Linux':
            if re.findall("ubuntu", osmatch.lower()):
                # Ubuntu icon
                return 'ubuntu_%s'%type
            elif re.findall("red hat", osmatch.lower()):
                # RedHat icon
                return 'redhat_%s'%type
            else:
                # Generic Linux icon
                return 'linux_%s'%type
        elif osfamily == 'Windows':
            # Windows icon
            return 'win_%s'%type
        elif osfamily == 'OpenBSD':
            # OpenBSD icon
            return 'openbsd_%s'%type
        elif osfamily == 'FreeBSD':
            # FreeBSD icon
            return 'freebsd_%s'%type
        elif osfamily == 'NetBSD':
            # NetBSD icon
            return 'default_%s'%type
        elif osfamily == 'Solaris':
            # Solaris icon
            return 'solaris_%s'%type
        elif osfamily == 'OpenSolaris':
            # OpenSolaris icon
            return 'solaris_%s'%type
        elif osfamily == 'IRIX':
            # Irix icon
            return 'irix_%s'%type
        elif osfamily == 'Mac OS X':
            # Mac OS X icon
            return 'macosx_%s'%type
        elif osfamily == 'Mac OS':
            # Mac OS icon
            return 'macosx_%s'%type
        else:
            # Default OS icon
            return 'default_%s'%type
    else:
        # Unknown OS icon
        return 'unknown_%s'%type

def get_vulnerability_logo(open_ports):
    open_ports = int(open_ports)
    if open_ports < 3:
        return 'vl_1_logo'
    elif open_ports < 5:
        return 'vl_2_logo'
    elif open_ports < 7:
        return 'vl_3_logo'
    elif open_ports < 9:
        return 'vl_4_logo'
    else:
        return 'vl_5_logo'
