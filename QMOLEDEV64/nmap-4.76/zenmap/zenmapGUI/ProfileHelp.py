#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2008 Insecure.Com LLC.
#
# Author: Jurand Nogiec <jurand@gmail.com> http://www.jurand.net
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

from zenmapCore.UmitLogging import log

class ProfileHelp:
    def __init__(self, currentstate=None):
        self.currentstate = "Default"
        self.labels = {}
        self.descs = {}
        self.examples = {}

    def get_currentstate(self):
        return self.currentstate

    def add_label(self, option_name, text):
        self.labels[option_name] = text
 
    def get_label(self):
        if self.currentstate in self.labels.keys():
            return self.labels[self.currentstate]
        else:
            return "" #blank

    def add_shortdesc(self, option_name, text):
        self.descs[option_name] = text
 
    def get_shortdesc(self):
        if self.currentstate in self.descs.keys():
            return self.descs[self.currentstate]
        else:
            return "" #blank

    def add_example(self, option_name, text):
        self.examples[option_name] = text
 
    def get_example(self):
        if self.currentstate in self.examples.keys():
            return self.examples[self.currentstate]
        else:
            return "" #blank

    def handler(self,whichLabel):
        log.debug("whichLabel: %s" % whichLabel)
        self.currentstate = whichLabel
