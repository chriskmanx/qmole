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
import os.path

from zenmapGUI.higwidgets.higwindows import HIGWindow
from zenmapGUI.higwidgets.higboxes import HIGVBox, HIGHBox, hig_box_space_holder
from zenmapGUI.higwidgets.higlabels import HIGEntryLabel
from zenmapGUI.higwidgets.higdialogs import HIGAlertDialog
from zenmapGUI.higwidgets.higtables import HIGTable

from zenmapGUI.OptionBuilder import *
from zenmapGUI.ProfileEditor import *

from zenmapCore.Name import NMAP_DISPLAY_NAME
from zenmapCore.Paths import Path
from zenmapCore.TargetList import target_list
from zenmapCore.UmitConf import Profile, CommandProfile
from zenmapCore.I18N import _

# Don't show any logo.
logo = None

# Uncomment this to bring the logo back.
# pixmaps_dir = Path.pixmaps_dir
# if pixmaps_dir:
#     logo = os.path.join(pixmaps_dir, 'wizard_logo.png')

class Wizard(HIGWindow):
    def __init__(self):
        HIGWindow.__init__(self)
        self.set_size_request(600,450)
        self.set_position(gtk.WIN_POS_CENTER)
        
        self.profile = CommandProfile()
        self.constructor = CommandConstructor()
        self.profilehelp = ProfileHelp()
        self.options = OptionBuilder(Path.wizard, self.constructor, self.update_command, self.profilehelp)
        
        self.target = '<target>'
        
        self.title_markup = "<span size='16500' weight='heavy'>%s</span>"
        self.directions = {'Start':self.start_page(),
                           'Choose':self.choose_page(),
                           'Profile':self.profile_page(),
                           'Finish':self.finish_page(),
                           'LastPage':None}
        
        for i in xrange(len(self.options.groups)):
            step = self.options.groups[i]
            last, next = self.__get_pair(i)
            
            self.directions[step] = self.__create_steps(step,
                                        last,
                                        next,
                                        self.options.section_names[step],
                                        self.options.tabs[step])
        
        self.directions['Command'] = self.command_page()
        
        self.main_vbox = HIGVBox()
        self.main_vbox.set_border_width(5)
        self.main_vbox.set_spacing(12)
        self.add(self.main_vbox)
        
        self.__create_wizard_widgets()
        self.set_title(_("%s command constructor wizard" % NMAP_DISPLAY_NAME))
        
        self.main_vbox._pack_expand_fill(self.directions['Start'])
        self.set_scan_interface(None)
        
        self.update_command()

    def __get_pair(self, pos):
        if pos == 0:
            return 'LastPage', self.options.groups[pos+1]
        elif pos == (self.options.groups.__len__() - 1):
            return self.options.groups[pos-1], 'Finish'
        else:
            return self.options.groups[pos-1], self.options.groups[pos+1]

    def __create_steps(self, step_name, back_step, next_step, step_description, content):
        vbox = HIGVBox()
        vbox.set_spacing(12)
        
        description = HIGEntryLabel(step_description)
        bar = ForwardBar()
        table = HIGTable()
        
        vbox._pack_noexpand_nofill(description)
        vbox._pack_expand_fill(table)
        vbox._pack_noexpand_nofill(bar)

        content.fill_table(table, False)

        bar.cancel.connect('clicked', self.close_wizard)
        bar.help.connect('clicked', self._show_help)
        bar.back.connect('clicked', self.switch_page, step_name, back_step)
        bar.forward.connect('clicked', self.switch_page, step_name, next_step)
        
        return vbox

    def set_scan_interface(self, interface):
        self.scan_interface = interface

    def __create_wizard_widgets(self):
        self.wizard_title = HIGEntryLabel("")
        self.wizard_title.set_line_wrap(False)
        self.wizard_event = gtk.EventBox()
        self.wizard_logo = gtk.Image()
        self.wizard_event.add(self.wizard_logo)
        
        command_hbox = HIGHBox()
        self.command_label = HIGEntryLabel(_("Command"))
        self.command_entry = gtk.Entry()
        
        separator = gtk.HSeparator()
        
        self.wizard_header_hbox = HIGHBox()
        
        self.wizard_header_hbox._pack_expand_fill(self.wizard_title)
        self.wizard_header_hbox._pack_noexpand_nofill(self.wizard_event)
        
        command_hbox._pack_noexpand_nofill(self.command_label)
        command_hbox._pack_expand_fill(self.command_entry)
        
        self.main_vbox._pack_noexpand_nofill(self.wizard_header_hbox)
        self.main_vbox._pack_noexpand_nofill(command_hbox)
        self.main_vbox._pack_noexpand_nofill(separator)
        
        self.wizard_logo.set_from_file(logo)
    
    def update_command(self):
        command = self.constructor.get_command(self.target)
        self.command_entry.set_text(command)
    
    def set_title(self, title):
        HIGWindow.set_title(self, title)
        self.wizard_title.set_label(self.title_markup % title)
    
    def close_wizard(self, widget=None, extra=None):
        self.destroy()
    
    def switch_page(self, widget, current, next):
        self.main_vbox.remove(self.directions[current])
        self.directions[current].hide()
        
        self.main_vbox._pack_expand_fill(self.directions[next])
        self.directions[next].show_all()
    
    def start_page(self):
        start = StartPage()
        start.bar.cancel.connect('clicked', self.close_wizard)
        start.bar.help.connect('clicked', self._show_help)
        start.bar.forward.connect('clicked', self.start_forward)
        
        return start
    
    def start_forward(self, widget):
        if self.directions['Start'].novice_radio.get_active():
            self.main_vbox.remove(self.directions['Start'])
            self.main_vbox._pack_expand_fill(self.directions['Choose'])
            
            self.directions['Start'].hide()
            self.directions['Choose'].show_all()
        else:
            p = ProfileEditor(deletable = False)
            p.set_scan_interface(self.scan_interface)
            p.show_all()
            
            self.close_wizard()
    
    def _show_help(self, widget=None):
        import webbrowser
        webbrowser.open("file://%s" % os.path.join(Path.docs_dir, "help.html"), new=2)
    
    def choose_page(self):
        choose = ChoosePage()
        choose.bar.cancel.connect('clicked', self.close_wizard)
        choose.bar.help.connect('clicked', self._show_help)
        choose.bar.back.connect('clicked', self.switch_page, 'Choose', 'Start')
        choose.bar.forward.connect('clicked', self.choose_forward)
        
        return choose
    
    def choose_forward(self, widget):
        if self.directions['Choose'].command_radio.get_active():
            if self.directions['Choose'].target_entry.get_text() == '':
                alert = HIGAlertDialog(message_format=_('No target selected!'),\
                                   secondary_text=_('You must provide a target \
to be scanned.'))
                alert.run()
                alert.destroy()
            
                self.directions['Choose'].target_entry.grab_focus()
                
                return None
        
        self.main_vbox.remove(self.directions['Choose'])
        self.directions['Choose'].hide()
        if self.directions['Choose'].profile_radio.get_active():
            self.main_vbox._pack_expand_fill(self.directions['Profile'])
            self.directions['Profile'].show_all()
            
            self.directions['LastPage'] = self.directions['Profile']
            self.directions['Profile'].prof = True
            self.target = '<target>'
        else:
            self.main_vbox._pack_expand_fill(self.directions['Command'])
            self.directions['Command'].show_all()
            
            self.directions['LastPage'] = self.directions['Choose']
            self.directions['Profile'].prof = False
            self.target = self.directions['Choose'].target_entry.get_text()
            self.directions['Choose'].add_new_target(self.target)
        
        self.update_command()
    
    def profile_page(self):
        profile = ProfilePage()
        profile.bar.cancel.connect('clicked', self.close_wizard)
        profile.bar.help.connect('clicked', self._show_help)
        profile.bar.back.connect('clicked', self.switch_page,'Profile','Choose')
        profile.bar.forward.connect('clicked', self.profile_forward)
        
        return profile
    
    def profile_forward(self, widget):
        if self.directions['Profile'].profile_entry.get_text() == '':
            alert = HIGAlertDialog(message_format=_('Unnamed profile'),\
                                   secondary_text=_('You must provide a name \
for this profile.'))
            alert.run()
            alert.destroy()
            
            self.directions['Profile'].profile_entry.grab_focus()
            
            return None
        
        self.main_vbox.remove(self.directions['Profile'])
        self.main_vbox._pack_expand_fill(self.directions['Command'])
        self.directions['Profile'].hide()
        self.directions['Command'].show_all()
        self.directions['LastPage'] = self.directions['Profile']
    
    def command_page(self):
        return self.directions[self.options.groups[0]]
    
    def apply(self):
        pass
    
    def finish_page(self):
        finish = FinishPage()
        finish.bar.cancel.connect('clicked', self.close_wizard)
        finish.bar.help.connect('clicked', self._show_help)
        finish.bar.back.connect('clicked', self.finish_back, finish, self.options.groups[-1])
        finish.bar.apply.connect('clicked', self.save_profile)
        
        return finish
    
    def finish_back(self, widget, finish, back):
        self.main_vbox.remove(finish)
        finish.hide()
        
        self.main_vbox._pack_expand_fill(self.directions[back])
        self.directions[back].show_all()
    
    def constructor_page(self):
        pass
    
    def save_profile(self, widget):
        command = self.constructor.get_command('%s')
        
        if self.directions['Choose'].profile_radio.get_active():
            profile_name = self.directions['Profile'].profile_entry.get_text()
            
            buffer = self.directions['Profile'].description_text.get_buffer()
            description = buffer.get_text(buffer.get_start_iter(),\
                                          buffer.get_end_iter())
            
            self.profile.add_profile(profile_name,\
                                     command=command,\
                                     description=description,\
                                     options=self.constructor.get_options())
            
            self.scan_interface.toolbar.profile_entry.update()
        else:
            target = self.directions['Choose'].target_entry.get_text()
            cmd = command % target
            
            self.scan_interface.execute_command(cmd)

            self.scan_interface.toolbar.target_entry.selected_target = self.\
                                directions['Choose'].target_entry.get_text()
            self.scan_interface.command_toolbar.command_entry.command = cmd
            self.scan_interface.command_toolbar.set_command(cmd)
        
        self.close_wizard()

class FinishPage(HIGVBox):
    def __init__(self):
        HIGVBox.__init__(self)
        self.set_spacing(12)
        
        self.description = HIGEntryLabel(_("""The %s command has been generated. \
Click Apply to finish this wizard.""" % NMAP_DISPLAY_NAME))
        spacer = hig_box_space_holder()
        self.bar = ApplyBar()
        
        self._pack_noexpand_nofill(self.description)
        self._pack_expand_fill(spacer)
        self._pack_noexpand_nofill(self.bar)

class ProfilePage(HIGVBox):
    def __init__(self):
        HIGVBox.__init__(self)
        self.set_spacing(12)
        self.prof = False
        
        self.description = HIGEntryLabel(_("""Please enter a name \
and optionally a description for this new profile"""))
        self.profile_label = HIGEntryLabel(_("Profile name"))
        self.description_label = HIGEntryLabel(_("Description"))
        
        self.profile_entry = gtk.Entry()
        self.description_scroll = HIGScrolledWindow()
        self.description_scroll.set_border_width(0)
        self.description_text = HIGTextView()
        
        self.description_scroll.add(self.description_text)
        
        table = HIGTable()
        self.bar = ForwardBar()
        
        self._pack_noexpand_nofill(self.description)
        self._pack_expand_fill(table)
        self._pack_noexpand_nofill(self.bar)
        
        table.attach(self.profile_label,0,1,0,1,xoptions=0,yoptions=0)
        table.attach(self.profile_entry,1,2,0,1,yoptions=0)
        
        table.attach(self.description_label,0,1,1,2,xoptions=0)
        table.attach(self.description_scroll,1,2,1,2)

class StartPage(HIGVBox):
    def __init__(self):
        HIGVBox.__init__(self)
        self.set_spacing(12)
        
        sec_vbox = HIGVBox()
        
        self.description = HIGEntryLabel(_("""You can construct \
powerful %s commands in two distinct ways:""" % NMAP_DISPLAY_NAME))
        self.novice_radio = gtk.RadioButton(None, _('Novice'))
        self.expert_radio = gtk.RadioButton(self.novice_radio, _('Expert (opens the Profile Editor)'))
        self.bar = ForwardBar(back=False)
        
        self._pack_noexpand_nofill(self.description)
        self._pack_expand_fill(sec_vbox)
        self._pack_noexpand_nofill(self.bar)
        
        sec_vbox._pack_noexpand_nofill(self.novice_radio)
        sec_vbox._pack_noexpand_nofill(self.expert_radio)

class ChoosePage(HIGVBox):
    def __init__(self):
        HIGVBox.__init__(self)
        self.set_spacing(12)
        
        table = HIGTable()
        self.hbox = HIGHBox()
        
        self.description = HIGEntryLabel(_("""You wish to create a new profile,\
 or just want to quickly create a command and run it once?"""))
        self.profile_radio = gtk.RadioButton(None, _('Profile'))
        self.command_radio = gtk.RadioButton(self.profile_radio, _('Command'))
        self.command_radio.connect('toggled', self.enable_target)
        self.profile_radio.connect('toggled', self.disable_target)
        
        self.target_label = HIGEntryLabel(_("Target"))
        self.target_entry = gtk.Entry()
        self.set_completion()
        
        self.hbox._pack_noexpand_nofill(hig_box_space_holder())
        self.hbox._pack_noexpand_nofill(self.target_label)
        self.hbox._pack_expand_fill(self.target_entry)
        
        self.bar = ForwardBar()
        
        self._pack_noexpand_nofill(self.description)
        self._pack_expand_fill(table)
        self._pack_noexpand_nofill(self.bar)
        
        table.attach(self.profile_radio,0,1,0,1, yoptions=0)
        table.attach(self.command_radio,0,1,1,2, yoptions=0)
        table.attach(self.hbox,0,1,2,3, yoptions=0)
        
        self.disable_target()
    
    def set_completion(self):
        self.completion = gtk.EntryCompletion()
        self.target_list = gtk.ListStore(str)
        self.completion.set_model(self.target_list)
        self.completion.set_text_column(0)
        
        self.target_entry.set_completion(self.completion)

        for target in target_list.get_target_list()[:15]:
            self.target_list.append([target.replace('\n','')])
    
    def add_new_target(self, target):
        target_list.add_target(target)

    def enable_target(self, widget=None):
        self.hbox.set_sensitive(True)
    
    def disable_target(self, widget=None):
        self.hbox.set_sensitive(False)

class ForwardBar(HIGHBox):
    def __init__(self, help=True, cancel=True, back=True, forward=True):
        HIGHBox.__init__(self)
        self.set_homogeneous(True)
        
        self.help = HIGButton(stock=gtk.STOCK_HELP)
        self.cancel = HIGButton(stock=gtk.STOCK_CANCEL)
        self.back = HIGButton(stock=gtk.STOCK_GO_BACK)
        self.forward = HIGButton(stock=gtk.STOCK_GO_FORWARD)
        
        self._pack_expand_fill(self.help)
        self._pack_expand_fill(hig_box_space_holder())
        self._pack_expand_fill(self.cancel)
        self._pack_expand_fill(self.back)
        self._pack_expand_fill(self.forward)
        
        if not help:
            self.help.set_sensitive(False)
        if not cancel:
            self.cancel.set_sensitive(False)
        if not back:
            self.back.set_sensitive(False)
        if not forward:
            self.forward.set_sensitive(False)

class ApplyBar(HIGHBox):
    def __init__(self, help=True, cancel=True, back=True, apply=True):
        HIGHBox.__init__(self)
        self.set_homogeneous(True)
        
        self.help = HIGButton(stock=gtk.STOCK_HELP)
        self.cancel = HIGButton(stock=gtk.STOCK_CANCEL)
        self.back = HIGButton(stock=gtk.STOCK_GO_BACK)
        self.apply = HIGButton(stock=gtk.STOCK_APPLY)
        
        self._pack_expand_fill(self.help)
        self._pack_expand_fill(hig_box_space_holder())
        self._pack_expand_fill(self.cancel)
        self._pack_expand_fill(self.back)
        self._pack_expand_fill(self.apply)
        
        if not help:
            self.help.set_sensitive(False)
        if not cancel:
            self.cancel.set_sensitive(False)
        if not back:
            self.back.set_sensitive(False)
        if not apply:
            self.apply.set_sensitive(False)

if __name__ == '__main__':
    w = Wizard()
    w.show_all()
    w.connect('delete-event', lambda x,y:gtk.main_quit())
    
    gtk.main()
