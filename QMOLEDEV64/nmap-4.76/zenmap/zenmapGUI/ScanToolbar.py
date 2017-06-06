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

from zenmapGUI.higwidgets.higboxes import HIGHBox
from zenmapGUI.higwidgets.higbuttons import HIGButton
from zenmapGUI.higwidgets.higlabels import HIGEntryLabel

from zenmapCore.I18N import _

from zenmapGUI.ProfileCombo import ProfileCombo
from zenmapGUI.TargetCombo import TargetCombo


class ScanCommandToolbar(HIGHBox):
    """This class builds the toolbar devoted to Command entry. It allows you to retrieve and edit the current command entered."""
    def __init__(self):
        """Initialize command toolbar"""
        HIGHBox.__init__(self)

        self.command_label = HIGEntryLabel(_("Command:"))
        self.command_entry = gtk.Entry()
        
        self._pack_noexpand_nofill(self.command_label)
        self._pack_expand_fill(self.command_entry)
        
    def get_command(self):
        """Retrieve command entry"""
        return self.command_entry.get_text()

    def set_command(self, command):
        """Set a command entry"""
        self.command_entry.set_text(command)

    command = property(get_command, set_command)


class ScanToolbar(HIGHBox):
    """
    This function regards the Scanning Toolbar, which includes 
    the Target and Profile editable fields/dropdown boxes, as well as
    the Scan button and assigns events and and actions associated with
    each.
    """
    def __init__(self):
        """Initialize Scan Toolbar, including Events, and packing all 
        of the GUI elements in layout"""
        HIGHBox.__init__(self)

        self._create_target()
        self._create_profile()

        self.scan_button = gtk.Button(_("Scan"))
        #self.scan_button = HIGButton(_("Scan "), gtk.STOCK_MEDIA_PLAY)
        self.cancel_button = gtk.Button(_("Cancel"))
        #self.cancel_button = HIGButton(_("Cancel "), gtk.STOCK_CANCEL)

        self._pack_noexpand_nofill(self.target_label)
        self._pack_expand_fill(self.target_entry)
        
        self._pack_noexpand_nofill(self.profile_label)
        self._pack_expand_fill(self.profile_entry)
        
        self._pack_noexpand_nofill(self.scan_button)
        # Removed until I decide what to do with cancelling aggregated scans.
        # self._pack_noexpand_nofill(self.cancel_button)

        # Skip over the dropdown arrow so you can tab to the profile entry.
        self.target_entry.set_focus_chain((self.target_entry.child,))

        self.target_entry.child.connect('activate',
                        lambda x: self.profile_entry.grab_focus())
        self.profile_entry.child.connect('activate',
                        lambda x: self.scan_button.clicked())
        
    def _create_target(self):
        """Create a target and update the list"""
        self.target_label = HIGEntryLabel(_("Target:"))
        self.target_entry = TargetCombo()
        
        self.update_target_list()

    def _create_profile(self):
        """Create new profile and update list"""
        self.profile_label = HIGEntryLabel(_('Profile:'))
        self.profile_entry = ProfileCombo()
        
        self.update()

    def update_target_list(self):
        self.target_entry.update()
        
    def add_new_target(self, target):
        self.target_entry.add_new_target(target)

    def get_selected_target(self):
        """Return currently selected target"""
        return self.target_entry.selected_target

    def set_selected_target(self, target):
        """Modify currently selected target"""
        self.target_entry.selected_target = target

    def update(self):
        self.profile_entry.update()
    
    def set_profiles(self, profiles):
        """Modify profile"""
        self.profile_entry.set_profiles(profiles)

    def get_selected_profile(self):
        """Return currently selected profile"""
        return self.profile_entry.selected_profile

    def set_selected_profile(self, profile):
        """Modify currently selected profile"""
        self.profile_entry.selected_profile = profile

    selected_profile = property(get_selected_profile, set_selected_profile)
    selected_target = property(get_selected_target, set_selected_target)

if __name__ == "__main__":
    w = gtk.Window()
    box = gtk.VBox()
    w.add(box)

    stool = ScanToolbar()
    sctool = ScanCommandToolbar()

    box.pack_start(stool)
    box.pack_start(sctool)

    w.connect("delete-event", lambda x,y: gtk.main_quit())
    w.show_all()
    gtk.main()
