#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2007 Adriano Monteiro Marques.
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

from os import access, R_OK, W_OK
from os.path import dirname
from zenmapCore.Paths import Path

class TargetList(object):
    def __init__(self):
        self.temp_list = []

        try:
            self.target_list_file = Path.target_list
        except:
            self.target_list_file = False

        #import pdb; pdb.set_trace()
        if self.target_list_file and \
            (access(self.target_list_file, R_OK and W_OK) or \
             access(dirname(self.target_list_file), R_OK and W_OK)):
            self.using_file = True

            # Recovering saved targets
            target_file = open(self.target_list_file, "r")
            self.temp_list = [t for t in target_file.read().split(";") \
                                    if t != "" and t != "\n"]
            target_file.close()
        else:
            self.using_file = False

    def save(self):
        if self.using_file:
            target_file = open(self.target_list_file, "w")
            target_file.write(";".join(self.temp_list))
            target_file.close()

    def add_target(self, target):
        if target in self.temp_list:
            return

        self.temp_list.append(target)
        self.save()

    def clean_list(self):
        del self.temp_list
        self.temp_list = []
        self.save()

    def get_target_list(self):
        t = self.temp_list[:]
        t.reverse()
        return t

target_list = TargetList()

if __name__ == "__main__":
    import sys
    from os.path import split
    t = TargetList()
    print ">>> Getting empty list:", t.get_target_list()
    print ">>> Adding target 127.0.0.1:", t.add_target("127.0.0.3")
    print ">>> Getting target list:", t.get_target_list()
    del t
