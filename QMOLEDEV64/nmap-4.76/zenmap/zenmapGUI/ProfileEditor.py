#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005,2008 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
# Modified: Jurand Nogiec <jurand@jurand.net>, 2008
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

from zenmapGUI.higwidgets.higwindows import HIGWindow
from zenmapGUI.higwidgets.higboxes import HIGVBox, HIGHBox, HIGSpacer, hig_box_space_holder
from zenmapGUI.higwidgets.higexpanders import HIGExpander
from zenmapGUI.higwidgets.higlabels import HIGSectionLabel, HIGEntryLabel
from zenmapGUI.higwidgets.higscrollers import HIGScrolledWindow
from zenmapGUI.higwidgets.higtextviewers import HIGTextView
from zenmapGUI.higwidgets.higbuttons import HIGButton
from zenmapGUI.higwidgets.higtables import HIGTable
from zenmapGUI.higwidgets.higdialogs import HIGAlertDialog, HIGDialog

from zenmapGUI.OptionBuilder import *

from zenmapCore.Paths import Path
from zenmapCore.NmapCommand import CommandConstructor
from zenmapCore.UmitConf import Profile, CommandProfile
from zenmapCore.UmitLogging import log
from zenmapCore.I18N import _
###
from zenmapGUI.ProfileHelp import ProfileHelp
###

class ProfileEditor(HIGWindow):
    def __init__(self, profile_name=None, deletable=True, overwrite=False):
        HIGWindow.__init__(self)
        self.connect("delete_event", self.exit)
        self.set_title(_('Profile Editor'))
        self.set_position(gtk.WIN_POS_CENTER)

        self.deletable = deletable
        self.profile_name = profile_name
        self.overwrite = overwrite

        self.__create_widgets()
        self.__pack_widgets()
        
        self.profile = CommandProfile()

        options_used = {}
        if profile_name:
            log.debug("Showing profile %s" % profile_name)
            prof = self.profile.get_profile(profile_name)
            options_used = prof['options']
            
            # Interface settings
            self.profile_name_entry.set_text(profile_name)
            self.profile_description_text.get_buffer().set_text(prof['description'])

        self.constructor = CommandConstructor(options_used)
        ###
        self.profilehelp = ProfileHelp(options_used)
        ### 
        self.options = OptionBuilder(Path.profile_editor, self.constructor, self.update_command, self.profilehelp)
        log.debug("Option groups: %s" % str(self.options.groups))
        log.debug("Option section names: %s" % str(self.options.section_names))
        #log.debug("Option tabs: %s" % str(self.options.tabs))
        
        for tab in self.options.groups:
            self.__create_tab(tab, self.options.section_names[tab], self.options.tabs[tab])
        
        self.update_command()
    
    def update_command(self):
        """Regenerate command with target '<target>' and set the value for the command entry"""
        self.command_entry.set_text(self.constructor.get_command('<target>'))
        ### whenever the command would be updated, thats when we update the help
        self.update_help()

    def update_help(self, text=None):
        helpText = self.help_field.get_buffer()
        if text:
            helpText.set_text(text)
        else:
            tempText = ""
            if self.profilehelp.get_currentstate() == "Default":
                helpText.set_text(" ")
            else:
                tempText += self.profilehelp.get_label()
                tempText += "\n\n"
                tempText += self.profilehelp.get_shortdesc()
                if self.profilehelp.get_example():
                    tempText += "\n\nExample input:\n"
                    tempText += self.profilehelp.get_example()
                helpText.set_text(tempText)

    def update_help_name(self, widget, extra):
        self.update_help(text="Profile name\n\nThis is how the"
        +" profile will be identified in the drop-down combo box in the" 
        +" scan tab.")

    def update_help_desc(self, widget, extra):
        self.update_help(text="Description\n\nThe description is a"
        + " full description of what the scan does, which may be long.")

    def help(self, widget):
        d = HIGAlertDialog(parent=self,
                           message_format=_("Help not implemented"),
                           secondary_text=_("Profile editor help is not implemented yet."))
        d.run()
        d.destroy()
    
    def __create_widgets(self):
    
        ###
        # Vertical box to keep 3 boxes
        self.main_whole_box = HIGVBox()

        self.upper_box = HIGVBox()
        self.middle_box = HIGHBox() 
        self.lower_box = HIGHBox()
    
        #self.main_vbox = HIGVBox()
        self.command_expander = HIGExpander('<b>'+_('Command')+'</b>')
        self.command_expander.set_expanded(True)
        self.command_entry = gtk.Entry()
        self.command_entry.set_editable(False)
        
        self.notebook = gtk.Notebook()
        
        # Profile info page
        self.profile_info_vbox = HIGVBox()
        self.profile_info_label = HIGSectionLabel(_('Profile Information'))
        self.profile_name_label = HIGEntryLabel(_('Profile name'))
        self.profile_name_entry = gtk.Entry()
        self.profile_name_entry.connect('enter-notify-event', self.update_help_name)
        self.profile_description_label = HIGEntryLabel(_('Description'))
        self.profile_description_scroll = HIGScrolledWindow()
        self.profile_description_scroll.set_border_width(0)
        self.profile_description_text = HIGTextView()
        self.profile_description_text.connect('motion-notify-event', self.update_help_desc)
        
        # Buttons
        self.buttons_hbox = HIGHBox()
        
        self.help_button = HIGButton(stock=gtk.STOCK_HELP)
        self.help_button.connect('clicked', self.help)
        
        self.cancel_button = HIGButton(stock=gtk.STOCK_CANCEL)
        self.cancel_button.connect('clicked', self.exit)
        
        self.delete_button = HIGButton(stock=gtk.STOCK_DELETE)
        self.delete_button.connect('clicked', self.delete_profile)

        self.ok_button = HIGButton(stock=gtk.STOCK_OK)
        self.ok_button.connect('clicked', self.save_profile)
        
        ###
        self.help_vbox = HIGVBox()
        self.help_label = HIGSectionLabel(_('Help'))
        self.help_scroll = HIGScrolledWindow()
        self.help_scroll.set_border_width(0)
        self.help_field = HIGTextView()
        self.help_field.set_cursor_visible(False)
        self.help_field.set_left_margin(5)
        self.help_field.set_editable(False)
        self.help_vbox.set_size_request(200,-1)
        ###
    def __pack_widgets(self):

        ###
        self.add(self.main_whole_box)
        
        # Packing command expander to upper box
        self.upper_box._pack_noexpand_nofill(self.command_expander)

        # Packing notebook (left) and help box (right) to middle box
        self.middle_box._pack_expand_fill(self.notebook)
        self.middle_box._pack_expand_fill(self.help_vbox)

        # Packing buttons to lower box
        self.lower_box.pack_end(self.buttons_hbox)

        # Packing the three vertical boxes to the main box
        self.main_whole_box._pack_noexpand_nofill(self.upper_box)
        self.main_whole_box._pack_noexpand_nofill(self.middle_box)
        self.main_whole_box._pack_noexpand_nofill(self.lower_box)
        ###
        
        
        # Packing command_entry on command_expander
        self.command_expander.hbox.pack_start(self.command_entry)
        
        # Packing profile information tab on notebook
        self.notebook.append_page(self.profile_info_vbox, gtk.Label(_('Profile')))
        self.profile_info_vbox.set_border_width(5)
        table = HIGTable()
        self.profile_info_vbox._pack_noexpand_nofill(self.profile_info_label)
        self.profile_info_vbox._pack_expand_fill(HIGSpacer(table))
        
        self.profile_description_scroll.add(self.profile_description_text)
        
        vbox_desc = HIGVBox()
        vbox_desc._pack_noexpand_nofill(self.profile_description_label)
        vbox_desc._pack_expand_fill(hig_box_space_holder())
        
        vbox_ann = HIGVBox()
        vbox_ann._pack_expand_fill(hig_box_space_holder())
        
        table.attach(self.profile_name_label,0,1,0,1,xoptions=0,yoptions=0)
        table.attach(self.profile_name_entry,1,2,0,1,yoptions=0)
        table.attach(vbox_desc,0,1,1,2,xoptions=0)
        table.attach(self.profile_description_scroll,1,2,1,2)
        
        # Packing buttons on button_hbox
        #self.buttons_hbox.pack_start(self.help_button)
        self.buttons_hbox._pack_expand_fill(hig_box_space_holder())
        if self.deletable:
            self.buttons_hbox._pack_noexpand_nofill(self.delete_button)
        self.buttons_hbox._pack_noexpand_nofill(self.cancel_button)
        self.buttons_hbox._pack_noexpand_nofill(self.ok_button)
        
        self.buttons_hbox.set_border_width(5)
        self.buttons_hbox.set_spacing(6)
        
        ###
        self.help_vbox._pack_noexpand_nofill(self.help_label)
        self.help_vbox._pack_expand_fill(self.help_scroll)
        self.help_scroll.add(self.help_field)
        self.help_vbox.set_border_width(1)
        self.help_vbox.set_spacing(1)
        ###

    def __create_tab(self, tab_name, section_name, tab):
        log.debug(">>> Tab name: %s" % tab_name)
        log.debug(">>>Creating profile editor section: %s" % section_name)

        vbox = HIGVBox()
        table = HIGTable()
        table.set_row_spacings(2)
        section = HIGSectionLabel(section_name)
        
        vbox._pack_noexpand_nofill(section)
        vbox._pack_noexpand_nofill(HIGSpacer(table))
        vbox.set_border_width(5)

        tab.fill_table(table, True)
        
        self.notebook.append_page(vbox, gtk.Label(tab_name))
    
    def save_profile(self, widget):
        if self.overwrite:
            self.profile.remove_profile(self.profile_name)
        profile_name = self.profile_name_entry.get_text()
        if profile_name == '':
            alert = HIGAlertDialog(message_format=_('Unnamed profile'),\
                                   secondary_text=_('You must provide a name \
for this profile.'))
            alert.run()
            alert.destroy()
            
            self.profile_name_entry.grab_focus()
            
            return None
        
        command = self.constructor.get_command('%s')
        
        buf = self.profile_description_text.get_buffer()
        description = buf.get_text(buf.get_start_iter(),\
                                      buf.get_end_iter())
        
        self.profile.add_profile(profile_name,\
                                 command=command,\
                                 description=description,\
                                 options=self.constructor.get_options())
        
        self.scan_interface.toolbar.profile_entry.update()
        self.scan_interface.refresh_command(None)
        self.destroy()
    
    def clean_profile_info(self):
        self.profile_name_entry.set_text('')
        self.profile_description_text.get_buffer().set_text('')
    
    def set_scan_interface(self, interface):
        self.scan_interface = interface

    def exit(self, *args):
        self.destroy()
        
    def delete_profile(self, widget=None, extra=None):   
        if self.deletable:
            dialog = HIGDialog(buttons=(gtk.STOCK_OK, gtk.RESPONSE_OK,
                                        gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL))
            alert = HIGEntryLabel('<b>'+_("Deleting Profile")+'</b>')
            text = HIGEntryLabel(_('Your profile is going to be deleted! Click\
 Ok to continue, or Cancel to go back to Profile Editor.'))
            hbox = HIGHBox()
            hbox.set_border_width(5)
            hbox.set_spacing(12)
        
            vbox = HIGVBox()
            vbox.set_border_width(5)
            vbox.set_spacing(12)
            
            image = gtk.Image()
            image.set_from_stock(gtk.STOCK_DIALOG_WARNING, gtk.ICON_SIZE_DIALOG)
           
            vbox.pack_start(alert)
            vbox.pack_start(text)
            hbox.pack_start(image)
            hbox.pack_start(vbox)
            
            dialog.vbox.pack_start(hbox)
            dialog.vbox.show_all()
            
            response = dialog.run()
            dialog.destroy()
            if response == gtk.RESPONSE_CANCEL:
                return True
            self.profile.remove_profile(self.profile_name)

        self.update_profile_entry()
        self.destroy()
    
    def update_profile_entry(self, widget=None, extra=None):
        self.scan_interface.toolbar.profile_entry.update()
        list = self.scan_interface.toolbar.profile_entry.get_model()
        length = len(list)
        if length >0 :
            self.scan_interface.toolbar.profile_entry.set_active(0)


if __name__ == '__main__':
    p = ProfileEditor()
    p.show_all()
    
    gtk.main()
