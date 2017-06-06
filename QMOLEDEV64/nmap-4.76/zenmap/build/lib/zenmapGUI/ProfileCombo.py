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

import gtk

from zenmapCore.UmitConf import CommandProfile
from zenmapCore.I18N import _

class ProfileCombo(gtk.ComboBoxEntry, object):
    def __init__(self):
        gtk.ComboBoxEntry.__init__(self, gtk.ListStore(str), 0)
        
        self.completion = gtk.EntryCompletion()
        self.child.set_completion(self.completion)
        self.completion.set_model(self.get_model())
        self.completion.set_text_column(0)

        self.update()

    def set_profiles(self, profiles):
        list = self.get_model()
        for i in range(len(list)):
            iter = list.get_iter_root()
            del(list[iter])
        
        for command in profiles:
            list.append([command])

    def update(self):
        profile = CommandProfile()
        profiles = profile.sections()
        profiles.sort()
        del(profile)
        
        self.set_profiles(profiles)

    def get_selected_profile(self):
        return self.child.get_text()

    def set_selected_profile(self, profile):
        self.child.set_text(profile)

    selected_profile = property(get_selected_profile, set_selected_profile)

if __name__ == "__main__":
    w = gtk.Window()
    p = ProfileCombo()
    p.update()
    w.add(p)
    w.show_all()

    gtk.main()
