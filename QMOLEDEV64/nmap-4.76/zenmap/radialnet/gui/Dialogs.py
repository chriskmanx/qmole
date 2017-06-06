# vim: set fileencoding=utf-8 :

# Copyright (C) 2008 Insecure.Com LLC.
#
# Author: João Paulo de Souza Medeiros <ignotus21@gmail.com>
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
import pango

from radialnet.core.Info import INFO
from radialnet.gui.Image import Pixmaps



class AboutDialog(gtk.AboutDialog):
    """
    """
    def __init__(self):
        """
        """
        gtk.AboutDialog.__init__(self)

        self.set_name(INFO['name'])
        self.set_version(INFO['version'])
        self.set_website(INFO['website'])
        self.set_authors(INFO['authors'])
        self.set_license(INFO['license'])
        self.set_copyright(INFO['copyright'])

        self.set_logo(Pixmaps().get_pixbuf('logo'))

        self.connect('response', self.__destroy)


    def __destroy(self, dialog, id):
        """
        """
        self.destroy()
