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

from os.path import exists
from ConfigParser import ConfigParser, DEFAULTSECT, NoOptionError, NoSectionError
from zenmapCore.UmitLogging import log

class UmitConfigParser(ConfigParser):
    filenames = None
    fp = None
    
    def __init__(self, *args):
        ConfigParser.__init__(self, *args)

    def set(self, section, option, value):
        if not self.has_section(section):
            self.add_section(section)
        
        ConfigParser.set(self, section, option, value)
        self.save_changes()

    def read(self, filename):
        log.debug(">>> Trying to parse: %s" % filename)

        self.filename = ConfigParser.read(self, filename)
        return self.filename

    def readfp(self, fp, filename=None):
        ConfigParser.readfp(self, fp, filename)
        self.fp = fp
        self.filenames = filename

    def save_changes(self):
        if self.filenames:
            filename = None
            if isinstance(self.filenames, basestring):
                filename = self.filenames
            elif isinstance(self.filenames, list):
                if len(self.filenames) == 1:
                    filename = self.filenames[0]
                else:
                    raise ValueError("UmitConfigParser can't handle a list of filenames: %s" % self.filenames)
            else:
                raise ValueError("UmitConfigParser can't handle a filename of type %s: %s" % (type(self.filenames), self.filenames))
            self.write(open(filename, 'w'))
        elif self.fp:
            self.write(self.fp)

    def write(self, fp):
        '''Write alphabetically sorted config files'''
        if self._defaults:
            fp.write("[%s]\n" % DEFAULTSECT)
            
            items = self._defaults.items()
            items.sort()
            
            for (key, value) in items:
                fp.write("%s = %s\n" % (key, str(value).replace('\n', '\n\t')))
            fp.write("\n")

        sects = self._sections.keys()
        sects.sort()
        
        for section in sects:
            fp.write("[%s]\n" % section)
            for (key, value) in self._sections[section].items():
                if key != "__name__":
                    fp.write("%s = %s\n" %
                             (key, str(value).replace('\n', '\n\t')))
            fp.write("\n")

def test_umit_conf_content(filename):
    parser = ConfigParser()
    parser.read(filename)

    # Paths section
    section = "paths"
    assert get_or_false(parser, section, "nmap_command_path")


def get_or_false(parser, section, option):
    try:
        result = parser.get(section, option)
        return result
    except NoOptionError:
        return False
    except NoSectionError:
        return False
