#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
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


from logging import Logger, StreamHandler, Formatter
from zenmapCore.Name import APP_DISPLAY_NAME
from zenmapCore.UmitOptionParser import option_parser

LOGLEVEL = option_parser.get_verbose()

class Log(Logger, object):
    def __init__(self, name, level=0):
        Logger.__init__(self, name, level)
        self.formatter = self.format

        handler = StreamHandler()
        handler.setFormatter(self.formatter)
        
        self.addHandler(handler)
        
    def get_formatter(self):
        return self.__formatter

    def set_formatter(self, fmt):
        self.__formatter = Formatter(fmt)


    format = "%(levelname)s - %(asctime)s - %(message)s"
    
    formatter = property(get_formatter, set_formatter, doc="")
    __formatter = Formatter(format)


# Import this!
log = Log(APP_DISPLAY_NAME, LOGLEVEL)

if __name__ == '__main__':
    log.debug("Debug Message")
    log.info("Info Message")
    log.warning("Warning Message")
    log.error("Error Message")
    log.critical("Critical Message")
