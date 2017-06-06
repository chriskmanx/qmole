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

import sys


class ArgvHandle:
    """
    """
    def __init__(self, argv):
        """
        """
        self.__argv = argv


    def get_option(self, option):
        """
        """
        if option in self.__argv:

            index = self.__argv.index(option)

            if index + 1 < len(self.__argv):
                return self.__argv[index + 1]

        return None


    def has_option(self, option):
        """
        """
        return option in self.__argv


    def get_last_value(self):
        """
        """
        return self.__argv[-1]



if __name__ == '__main__':

    import sys

    h = ArgvHandle(sys.argv)

    print h.get_last_value()
