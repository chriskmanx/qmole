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
higwidgets/higdialogs.py

   dialog related classes
"""

__all__ = ['HIGDialog', 'HIGAlertDialog']

import gtk

from gtkutils import gtk_version_minor

class HIGDialog(gtk.Dialog):
    """
    HIGFied Dialog
    """
    def __init__(self, title='', parent=None, flags=0, buttons=()):
        gtk.Dialog.__init__(self, title, parent, flags, buttons)
        self.set_border_width(5)
        self.vbox.set_border_width(2)
        self.vbox.set_spacing(6)

class HIGAlertDialog(gtk.MessageDialog):
    """
    HIGfied Alert Dialog.

    Implements the sugestions documented on:
    http://developer.gnome.org/projects/gup/hig/2.0/windows-alert.html
    """
    
    def __init__(self, parent=None, flags=0, type=gtk.MESSAGE_INFO,
                 # HIG mandates that every Alert should have an "affirmative
                 # button that dismisses the alert and performs the action
                 # suggested"
                 buttons=gtk.BUTTONS_OK,
                 message_format=None,
                 secondary_text=None):
        
        gtk.MessageDialog.__init__(self, parent, flags, type, buttons)

        self.set_resizable(False)
        
        # HIG mandates that Message Dialogs should have no title:
        # "Alert windows have no titles, as the title would usually
        # unnecessarily duplicate the alert's primary text"
        self.set_title("")
        self.set_markup("<span weight='bold'size='larger'>%s</span>" \
                        % message_format)
        if secondary_text:
            # GTK up to version 2.4 does not have secondary_text
            try:
                self.format_secondary_text(secondary_text)
            except:
                pass


if __name__ == '__main__':

    from higlabels import HIGEntryLabel, HIGDialogLabel

    # HIGDialog
    d = HIGDialog(title='HIGDialog',
                  buttons=(gtk.STOCK_OK, gtk.RESPONSE_ACCEPT))
    dialog_label = HIGDialogLabel('A HIGDialogLabel on a HIGDialog')
    dialog_label.show()
    d.vbox.pack_start(dialog_label)

    entry_label = HIGEntryLabel('A HIGEntryLabel on a HIGDialog')
    entry_label.show()
    d.vbox.pack_start(entry_label)

    d.run()
    d.destroy()

    # HIGAlertDialog
    d = HIGAlertDialog(message_format="You Have and Appointment in 15 minutes",
                       secondary_text="You shouldn't be late this time. "
                       "Oh, and there's a huge traffic jam on your way!")
    d.run()
    d.destroy()
