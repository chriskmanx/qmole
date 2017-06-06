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
from radialnet.bestwidgets.labels import BWSectionLabel


class BWExpander(gtk.Expander):
    """
    """
    def __init__(self, label=''):
        """
        """
        gtk.Expander.__init__(self)

        self.__label = BWSectionLabel(label)
        self.set_label_widget(self.__label)

        self.__alignment = gtk.Alignment(0, 0, 1, 1)
        self.__alignment.set_padding(12, 0, 24, 0)

        self.add(self.__alignment)


    def bw_set_label_text(self, text):
        """
        """
        self.__label.bw_set_text(text)


    def bw_add(self, widget):
        """
        """
        if len(self.__alignment.get_children()) > 0:
            self.__alignment.remove(self.__alignment.get_children()[0])

        self.__alignment.add(widget)


    def bw_no_padding(self):
        """
        """
        self.__alignment.set_padding(0, 0, 0, 0)
