#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
#         Cleber Rodrigues <cleber.gnu@gmail.com>
#
# This library is free software; you can redistribute it and/or modify 
# it under the terms of the GNU Lesser General Public License as published 
# by the Free Software Foundation; either version 2.1 of the License, or 
# (at your option) any later version.
#
# This library is distributed in the hope that it will be useful, but 
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public 
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License 
# along with this library; if not, write to the Free Software Foundation, 
# Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 

"""
higwidgets/gtkutils.py

   gtk related functions
"""

__all__ = ['gtk_version_major', 'gtk_version_minor', 'gtk_version_release',
           'gtk_constant_name', 'gobject_register']

import gtk
import gobject

# version information
gtk_version_major, gtk_version_minor, gtk_version_release = gtk.gtk_version
assert gtk_version_major == 2

def gtk_constant_name(group, value):
    """
    Returns the (py)GTK+ name of a constant, given its group name
    """
    group_response = { -1 : 'gtk.RESPONSE_NONE',
                       -2 : 'gtk.RESPONSE_REJECT',
                       -3 : 'gtk.RESPONSE_ACCEPT',
                       -4 : 'gtk.RESPONSE_DELETE_EVENT',
                       -5 : 'gtk.RESPONSE_OK',
                       -6 : 'gtk.RESPONSE_CANCEL',
                       -7 : 'gtk.RESPONSE_CLOSE',
                       -8 : 'gtk.RESPONSE_YES',
                       -9 : 'gtk.RESPONSE_NO',
                       -10 : 'gtk.RESPONSE_APPLY',
                       -11 : 'gtk.RESPONSE_HELP' }

    groups = {'response' : group_response}

    return groups.get(group, {}).get(value, 'Error: constant value not found')


def gobject_register(klass):
    """
    Register a given object by calling gobject.type_register.

    Actually gobject.type_register is only called if the pygtk version in use
    is not 2.8 at least.
    """    
    if gtk_version_minor < 8:
        gobject.type_register(klass)
