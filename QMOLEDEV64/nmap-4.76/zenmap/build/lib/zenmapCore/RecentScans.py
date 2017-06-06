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

class RecentScans(object):
    def __init__(self):
        self.temp_list = []

        try:
            self.recents_scans_file = Path.recent_scans
        except:
            self.recents_scans_file = False

        if self.recents_scans_file and \
            (access(self.recents_scans_file, R_OK and W_OK) or \
             access(dirname(self.recents_scans_file), R_OK and W_OK)):
            self.using_file = True

            # Recovering saved targets
            recent_file = open(self.recents_scans_file, "r")
            self.temp_list = [t for t in recent_file.read().split(";") \
                                    if t != "" and t != "\n"]
            recent_file.close()
        else:
            self.using_file = False

    def save(self):
        if self.using_file:
            recent_file = open(self.recents_scans_file, "w")
            recent_file.write(";".join(self.temp_list))
            recent_file.close()

    def add_recent_scan(self, recent_scan):
        if recent_scan in self.temp_list:
            return

        self.temp_list.append(recent_scan)
        self.save()

    def clean_list(self):
        del self.temp_list
        self.temp_list = []
        self.save()

    def get_recent_scans_list(self):
        t = self.temp_list[:]
        t.reverse()
        return t

recent_scans = RecentScans()

if __name__ == "__main__":
    import sys
    from os.path import split
    r = RecentScans()
    print ">>> Getting empty list:", r.get_recent_scans_list()
    print ">>> Adding recent scan bla:", r.add_recent_scan("bla")
    print ">>> Getting recent scan list:", r.get_recent_scans_list()
    del r
