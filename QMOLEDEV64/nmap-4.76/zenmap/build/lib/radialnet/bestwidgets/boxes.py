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



class BWBox(gtk.Box):
    """
    """
    def bw_pack_start_expand_fill(self, widget, padding=0):
        """
        """
        self.pack_start(widget, True, True, padding)


    def bw_pack_start_expand_nofill(self, widget, padding=0):
        """
        """
        self.pack_start(widget, True, False, padding)


    def bw_pack_start_noexpand_nofill(self, widget, padding=0):
        """
        """
        self.pack_start(widget, False, False, padding)


    def bw_pack_end_expand_fill(self, widget, padding=0):
        """
        """
        self.pack_end(widget, True, True, padding)


    def bw_pack_end_expand_nofill(self, widget, padding=0):
        """
        """
        self.pack_end(widget, True, False, padding)


    def bw_pack_end_noexpand_nofill(self, widget, padding=0):
        """
        """
        self.pack_end(widget, False, False, padding)



class BWHBox(gtk.HBox, BWBox):
    """
    """
    def __init__(self, homogeneous=False, spacing=12):
        """
        """
        gtk.HBox.__init__(self, homogeneous, spacing)



class BWVBox(gtk.VBox, BWBox):
    """
    """
    def __init__(self, homogeneous=False, spacing=12):
        """
        """
        gtk.VBox.__init__(self, homogeneous, spacing)



class BWStatusbar(gtk.Statusbar, BWBox):
    """
    """
    def __init__(self, homogeneous=False, spacing=12):
        """
        """
        gtk.HBox.__init__(self, homogeneous, spacing)



class BWTable(gtk.Table, BWBox):
    """
    """
    def __init__(self, rows=1, columns=1, homogeneous=False):
        """
        """
        gtk.Table.__init__(self, rows, columns, homogeneous)
        self.bw_set_spacing(12)

        self.__rows = rows
        self.__columns = columns

        self.__last_point = (0, 0)


    def bw_set_spacing(self, spacing):
        """
        """
        self.set_row_spacings(spacing)
        self.set_col_spacings(spacing)


    def bw_resize(self, rows, columns):
        """
        """
        self.__rows = rows
        self.__columns = columns

        self.resize(rows, columns)


    def bw_attach_next(self,
                       child,
                       xoptions=gtk.EXPAND|gtk.FILL,
                       yoptions=gtk.EXPAND|gtk.FILL,
                       xpadding=0,
                       ypadding=0):
        """
        """
        row, column = self.__last_point

        if row != self.__rows:

            self.attach(child,
                        column,
                        column + 1,
                        row,
                        row + 1,
                        xoptions,
                        yoptions,
                        xpadding,
                        ypadding)

            if column + 1 == self.__columns:

                column = 0
                row += 1

            else:
                column += 1

            self.__last_point = (row, column)



class BWScrolledWindow(gtk.ScrolledWindow):
    """
    """
    def __init__(self):
        """
        """
        gtk.ScrolledWindow.__init__(self)
        self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.set_shadow_type(gtk.SHADOW_NONE)
        self.set_border_width(6)
