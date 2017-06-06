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
import gobject



class BWChangeableComboBoxEntry(gtk.ComboBoxEntry):
    """
    """
    def __init__(self):
        """
        """
        self.__liststore = gtk.ListStore(gobject.TYPE_STRING)

        gtk.ComboBoxEntry.__init__(self, self.__liststore, 0)

        self.connect("changed", self.__changed)
        self.get_child().connect("changed", self.__entry_changed)

        self.__last_active = None


    def __changed(self, widget):
        """
        """
        if self.get_active() != -1:
            self.__last_active = self.get_active()


    def bw_get_lenght(self):
        """
        """
        return len(self.__liststore)


    def __entry_changed(self, widget):
        """
        """
        if len(self.__liststore) > 0 and\
           self.__last_active != None and\
           self.get_active() == -1:

            iter = self.get_model().get_iter((self.__last_active,))
            self.__liststore.set_value(iter, 0, widget.get_text().strip())


    def bw_get_active(self):
        """
        """
        if self.get_active() == -1:
            return self.__last_active

        return self.get_active()



# testing widget
if __name__ == "__main__":

    def button_clicked(widget, combo):
        """
        """
        combo.append_text('New')


    window = gtk.Window()
    window.connect("destroy", lambda w: gtk.main_quit())

    box = gtk.HBox()

    combo = BWChangeableComboBoxEntry()
    combo.append_text('New')
    combo.set_active(0)

    button = gtk.Button('More')
    button.connect("clicked", button_clicked, combo)

    box.pack_start(button, False, False)
    box.pack_start(combo, True, True)

    window.add(box)
    window.show_all()

    gtk.main()
