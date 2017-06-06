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

import locale
import sys
import gtk
import gtk.gdk
import pango
import re

from threading import Thread

from zenmapCore.I18N import _
from zenmapCore.UmitLogging import log
from zenmapCore.UmitConf import NmapOutputHighlight

from zenmapGUI.NmapOutputProperties import NmapOutputProperties

class NmapOutputViewer (gtk.VBox):
    def __init__ (self, refresh=1, stop=1):
        self.nmap_highlight = NmapOutputHighlight()
        gtk.VBox.__init__ (self)
        
        # Creating widgets
        self.__create_widgets()
        
        # Setting scrolled window
        self.__set_scrolled_window()
        
        # Setting text view
        self.__set_text_view()
        
        # Getting text buffer
        self.text_buffer = self.text_view.get_buffer()
        
        self.refreshing = True
        self.thread = Thread()
        
        # Adding widgets to the VBox
        self.pack_start(self.scrolled, expand = True, fill = True)
        
        # The NmapCommand instance, if any, whose output is shown in this
        # display.
        self.command_execution = None
        self.nmap_previous_output = ''

        self.brazil = True
        
    
    def __create_widgets (self):
        # Creating widgets
        self.scrolled = gtk.ScrolledWindow ()
        self.text_view = gtk.TextView ()
        self.txg_font = gtk.TextTag()
        self.txg_date = gtk.TextTag()
    
    def __set_scrolled_window (self):
        # Seting scrolled window
        self.scrolled.set_border_width (5)
        self.scrolled.add(self.text_view)
        self.scrolled.set_policy (gtk.POLICY_AUTOMATIC,gtk.POLICY_AUTOMATIC)
    
    def __set_text_view(self):
        self.text_view.set_wrap_mode(gtk.WRAP_WORD)
        self.text_view.set_editable(False)
        
        self.txg_output = self.text_view.get_buffer().get_tag_table()
        self.txg_output.add(self.txg_font)
        self.txg_font.set_property("family", "Monospace")
        self.text_view.get_buffer().connect ("changed", self.__text_changed_cb)

    def __text_changed_cb (self, widget):
        buff = self.text_view.get_buffer ()
        buff.apply_tag(self.txg_font, buff.get_start_iter(), buff.get_end_iter())
        self.update_output_colors()
        
    def go_to_host(self, host):
        """Go to host line on nmap output result"""
        buff = self.text_view.get_buffer()
        start_iter, end_iter = buff.get_bounds()

        output = buff.get_text(start_iter, end_iter).split("\n")
        re_host = re.compile("%s\s{0,1}:" % re.escape(host))

        for i in xrange(len(output)):
            if re_host.search(output[i]):
                self.text_view.scroll_to_iter(buff.get_iter_at_line(i), 0, True, 0, 0)
                break
        
    def show_output_properties(self, widget):
        nmap_out_prop = NmapOutputProperties(self.text_view)
        #nmap_out_prop.connect("response", self.update_output_colors)
        
        nmap_out_prop.run()
        
        for prop in nmap_out_prop.property_names:
            widget = nmap_out_prop.property_names[prop][8]

            wid_props = []

            if widget.bold:
                wid_props.append(1)
            else:
                wid_props.append(0)

            if widget.italic:
                wid_props.append(1)
            else:
                wid_props.append(0)

            if widget.underline:
                wid_props.append(1)
            else:
                wid_props.append(0)
 
            wid_props.append("(%s, %s, %s)" % (widget.text_color.red,
                                               widget.text_color.green,
                                               widget.text_color.blue))
            wid_props.append("(%s, %s, %s)" % (widget.highlight_color.red,
                                               widget.highlight_color.green,
                                               widget.highlight_color.blue))

            self.nmap_highlight.__setattr__(widget.property_name, wid_props)
            
        nmap_out_prop.destroy()
        self.nmap_highlight.save_changes()
        self.update_output_colors()

    def update_output_colors(self, dialog=None, response_id=None):
        buff = self.text_view.get_buffer()
        tag_table = buff.get_tag_table()
        start = buff.get_start_iter()
        end = buff.get_end_iter()

        buff.remove_all_tags(start, end)
        buff.apply_tag(self.txg_font, start, end)

        if not self.nmap_highlight.enable:
            return        

        if True:
            properties = ["details",
                          "date",
                          "hostname",
                          "ip",
                          "port_list",
                          "open_port",
                          "closed_port",
                          "filtered_port"]
            
            for pos in xrange(buff.get_line_count()):
                # Grab the current line.
                line_start_iter = buff.get_iter_at_line(pos)
                line_end_iter = line_start_iter.copy()
                if not line_end_iter.ends_line():
                    line_end_iter.forward_to_line_end()
                line = buff.get_text(line_start_iter, line_end_iter)

                if not line:
                    continue
                
                for p in xrange(len(properties)):
                    settings = self.nmap_highlight.__getattribute__(properties[p])
                    match = re.finditer(settings[5], line)

                    # Create tags only if there's a matching for the expression
                    if match:
                        tag = gtk.TextTag()
                        
                        if settings[0]:
                            tag.set_property("weight", pango.WEIGHT_HEAVY)
                        else:
                            tag.set_property("weight", pango.WEIGHT_NORMAL)
                            
                        if settings[1]:
                            tag.set_property("style", pango.STYLE_ITALIC)
                        else:
                            tag.set_property("style", pango.STYLE_NORMAL)
                        
                        if settings[2]:
                            tag.set_property("underline", pango.UNDERLINE_SINGLE)
                        else:
                            tag.set_property("underline", pango.UNDERLINE_NONE)

                        text_color = settings[3]
                        highlight_color = settings[4]

                        tag.set_property("foreground",
gtk.color_selection_palette_to_string([gtk.gdk.Color(*text_color),]))
                        tag.set_property("background",
gtk.color_selection_palette_to_string([gtk.gdk.Color(*highlight_color),]))

                        tag_table.add(tag)
                        tag.set_priority(p)

                    for m in match:
                        try:
                            buff.apply_tag(tag, buff.get_iter_at_line_offset(pos, m.start()),
                                           buff.get_iter_at_line_offset(pos, m.end()))
                        except:
                            pass

                    # Brasil-sil-sil!!!
                    match = re.finditer("Bra[sz]il", line)
                    for m in match:
                        tag1 = gtk.TextTag()
                        tag2 = gtk.TextTag()
                        tag3 = gtk.TextTag()

                        tag_table.add(tag1)
                        tag_table.add(tag2)
                        tag_table.add(tag3)

                        tag1.set_property("foreground", "#EAFF00")
                        tag1.set_property("background", "#21C800")
                        tag1.set_property("weight", pango.WEIGHT_HEAVY)
                    
                        tag2.set_property("foreground", "#0006FF")
                        tag2.set_property("background", "#21C800")
                        tag2.set_property("weight", pango.WEIGHT_HEAVY)
                        
                        tag3.set_property("foreground", "#FFFFFF")
                        tag3.set_property("background", "#21C800")
                        tag3.set_property("weight", pango.WEIGHT_HEAVY)

                        try:
                            buff.apply_tag(tag1, buff.get_iter_at_line_offset(pos, m.start()),
                                           buff.get_iter_at_line_offset(pos, m.end() - 5))

                            buff.apply_tag(tag2, buff.get_iter_at_line_offset(pos, m.start() + 1),
                                           buff.get_iter_at_line_offset(pos, m.end() -4))

                            buff.apply_tag(tag3, buff.get_iter_at_line_offset(pos, m.start() + 2),
                                           buff.get_iter_at_line_offset(pos, m.end() - 3))
                            
                            buff.apply_tag(tag1, buff.get_iter_at_line_offset(pos, m.start() + 3),
                                           buff.get_iter_at_line_offset(pos, m.end() - 2))

                            buff.apply_tag(tag2, buff.get_iter_at_line_offset(pos, m.start() + 4),
                                           buff.get_iter_at_line_offset(pos, m.end() - 1))

                            buff.apply_tag(tag3, buff.get_iter_at_line_offset(pos, m.start() + 5),
                                           buff.get_iter_at_line_offset(pos, m.end()))
                        except:
                            pass
                    else:
                        self._brasil_log()

                    
                    match = re.finditer("BRT", line)
                    for m in match:
                        tag1 = gtk.TextTag()
                        tag2 = gtk.TextTag()
                        tag3 = gtk.TextTag()

                        tag_table.add(tag1)
                        tag_table.add(tag2)
                        tag_table.add(tag3)

                        tag1.set_property("foreground", "#EAFF00")
                        tag1.set_property("background", "#21C800")
                        tag1.set_property("weight", pango.WEIGHT_HEAVY)
                    
                        tag2.set_property("foreground", "#0006FF")
                        tag2.set_property("background", "#21C800")
                        tag2.set_property("weight", pango.WEIGHT_HEAVY)
                        
                        tag3.set_property("foreground", "#FFFFFF")
                        tag3.set_property("background", "#21C800")
                        tag3.set_property("weight", pango.WEIGHT_HEAVY)

                        try:
                            buff.apply_tag(tag1, buff.get_iter_at_line_offset(pos, m.start()),
                                           buff.get_iter_at_line_offset(pos, m.end() - 2))

                            buff.apply_tag(tag2, buff.get_iter_at_line_offset(pos, m.start() + 1),
                                           buff.get_iter_at_line_offset(pos, m.end() -1))

                            buff.apply_tag(tag3, buff.get_iter_at_line_offset(pos, m.start() + 2),
                                           buff.get_iter_at_line_offset(pos, m.end()))
                        except:
                            pass
                    else:
                        self._brasil_log()

    def _brasil_log(self):
        if self.brazil:
            log.info("Isto aqui, o o")
            log.info("E um pouquinho de Brasil, io io")
            log.info("Deste Brasil que canta e e feliz")
            log.info("Feliz, feliz")
            log.info("")
            log.info("E tambem um pouco de uma raca")
            log.info("Que nao tem medo de fumaca ai, ai")
            log.info("E nao se entrega, nao ")
            log.info("")
            log.info('Olha o jeito das "cadera"  que ela sabe dar')
            log.info("Olha o tombo nos quadris que ela sabe dar")
            log.info("Olha o passe de batuque que ela sabe dar")
            log.info("Olha so o remelexo que ela sabe dar")
            log.info("")
            log.info("Morena boa me faz chorar")
            log.info("Poe a sandalia de prata")
            log.info("e vem pro samba sambar")
            
            self.brazil = False
    
    def show_nmap_output(self, output):
        """Show the string (or unicode) output in the output display."""
        self.text_buffer.set_text(output)
    
    def set_command_execution(self, command):
        """Set the live running command whose output is shown by this display.
        The current output is extracted from the command object."""
        self.command_execution = command
        self.nmap_previous_output = None
        self.refresh_output()

    def refresh_output(self, widget = None):
        """Update the output from the latest output of the command associated
        with this view, as set by set_command_execution. It has no effect if no
        command has been set."""
        log.debug("Refresh nmap output")

        if self.command_execution is None:
            return

        new_output = self.command_execution.get_output()

        if self.nmap_previous_output != new_output:
            self.show_nmap_output(new_output)
            self.nmap_previous_output = new_output
    
if __name__ == '__main__':
    w = gtk.Window()
    n = NmapOutputViewer()
    w.add(n)
    w.show_all()
    w.connect('delete-event', lambda x,y,z=None:gtk.main_quit())

    buff = n.text_view.get_buffer()
    buff.set_text(read_file("file_with_encoding_issues.txt"))
    
    gtk.main()
