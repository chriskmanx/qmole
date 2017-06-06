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
higwidgets/higlogindialog.py

   a basic login/authentication dialog
"""

__all__ = ['HIGLoginDialog']

import gtk

from higdialogs import HIGDialog
from higlabels import HIGEntryLabel
from higtables import HIGTable
from higentries import HIGTextEntry, HIGPasswordEntry

class HIGLoginDialog(HIGDialog):
    """
    A dialog that asks for basic login information (username / password)
    """
    def __init__(self, title='Login',
                 buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                          gtk.STOCK_OK, gtk.RESPONSE_ACCEPT)):
        HIGDialog.__init__(self, title, buttons=buttons)

        self.username_label = HIGEntryLabel("Username:")
        self.username_entry = HIGTextEntry()
        self.password_label = HIGEntryLabel("Password:")
        self.password_entry = HIGPasswordEntry()

        self.username_password_table = HIGTable(2, 2)
        self.username_password_table.attach_label(self.username_label,
                                                  0, 1, 0, 1)
        self.username_password_table.attach_entry(self.username_entry,
                                                  1, 2, 0, 1)
        self.username_password_table.attach_label(self.password_label,
                                                  0, 1, 1, 2)
        self.username_password_table.attach_entry(self.password_entry,
                                                  1, 2, 1, 2)

        self.vbox.pack_start(self.username_password_table, False, False)
        self.set_default_response(gtk.RESPONSE_ACCEPT)

    def run(self):
        self.show_all()
        return HIGDialog.run(self)

if __name__ == '__main__':

    from gtkutils import gtk_constant_name

    # HIGLoginDialog
    d = HIGLoginDialog()
    response_value = d.run()
    print gtk_constant_name('response', response_value)
    d.destroy()
