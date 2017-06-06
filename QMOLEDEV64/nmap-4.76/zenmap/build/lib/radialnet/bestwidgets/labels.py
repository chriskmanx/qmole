# vim: set fileencoding=utf-8 :

# Copyright (C) 2008 Insecure.Com LLC.
#
# Author(s): João Paulo de Souza Medeiros <ignotus21@gmail.com>
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



class BWLabel(gtk.Label):
    """
    """
    def __init__(self, text=''):
        """
        """
        gtk.Label.__init__(self)

        self.set_text(text)
        self.set_justify(gtk.JUSTIFY_LEFT)
        self.set_alignment(0, 0.50)
        self.set_line_wrap(True)



class BWSectionLabel(gtk.Label):
    """
    """
    def __init__(self, text=''):
        """
        """
        gtk.Label.__init__(self)

        self.set_markup('<b>' + text + '</b>')
        self.set_justify(gtk.JUSTIFY_LEFT)
        self.set_alignment(0, 0.50)
        self.set_line_wrap(True)


    def bw_set_text(self, text):
        """
        """
        self.set_markup('<b>' + text + '</b>')
