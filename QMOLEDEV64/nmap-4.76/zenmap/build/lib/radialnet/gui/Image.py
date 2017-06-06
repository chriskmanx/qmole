# vim: set fileencoding=utf-8 :

# Copyright (C) 2007, 2008 Insecure.Com LLC.
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

import os
import sys
import gtk

from zenmapCore.Paths import Path


class Image:
    """
    """
    def __init__(self, path=None):
        """
        """
        self.__path = path
        self.__cache = dict()


    def set_path(self, path):
        """
        """
        self.__path = path


    def get_pixbuf(self, icon, image_type='png'):
        """
        """
        if self.__path == None:
            return False

        if icon + image_type not in self.__cache.keys():

            file = self.get_icon(icon, image_type)
            self.__cache[icon + image_type] = gtk.gdk.pixbuf_new_from_file(file)

        return self.__cache[icon + image_type]


    def get_icon(self, icon, image_type='png'):
        """
        """
        if self.__path == None:
            return False

        return os.path.join(self.__path, icon + "." + image_type)



class Pixmaps(Image):
    """
    """
    def __init__(self):
        """
        """
        Image.__init__(self, os.path.join(Path.pixmaps_dir, "radialnet"))



class Icons(Image):
    """
    """
    def __init__(self):
        """
        """
        Image.__init__(self, os.path.join(Path.pixmaps_dir, "radialnet"))



class Application(Image):
    """
    """
    def __init__(self):
        """
        """
        Image.__init__(self, os.path.join(Path.pixmaps_dir, "radialnet"))
