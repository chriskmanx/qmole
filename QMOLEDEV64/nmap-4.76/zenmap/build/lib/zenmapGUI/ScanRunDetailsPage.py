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
from zenmapGUI.higwidgets.higboxes import HIGVBox, HIGHBox, hig_box_space_holder
from zenmapGUI.higwidgets.higtables import HIGTable
from zenmapGUI.higwidgets.higlabels import HIGEntryLabel

from zenmapCore.I18N import _

class ScanRunDetailsPage(HIGVBox):
    def __init__(self, scan):
        HIGVBox.__init__(self)

        na = _('Not available')
        
        # Command info
        self.command_label = HIGEntryLabel(_('Command:'))
        self.info_command_label = HIGEntryLabel(na)
        
        self.nmap_version_label = HIGEntryLabel(_('Nmap Version:'))
        self.info_nmap_version_label = HIGEntryLabel(na)
        
        self.verbose_label = HIGEntryLabel(_('Verbosity level:'))
        self.info_verbose_label = HIGEntryLabel(na)
        
        self.debug_label = HIGEntryLabel(_('Debug level:'))
        self.info_debug_label = HIGEntryLabel(na)

        self.command_expander = gtk.Expander("<b>"+_("Command Info")+"</b>")
        self.command_expander.set_use_markup(True)
        
        self.command_table = HIGTable()
        self.command_table.set_border_width(5)
        self.command_table.set_row_spacings(6)
        self.command_table.set_col_spacings(6)

        self.command_hbox = HIGHBox()
        self.command_hbox._pack_noexpand_nofill(hig_box_space_holder())
        self.command_hbox._pack_noexpand_nofill(self.command_table)
        
        self.command_table.attach(self.command_label,0,1,0,1)
        self.command_table.attach(self.info_command_label,1,2,0,1)
        
        self.command_table.attach(self.nmap_version_label,0,1,1,2)
        self.command_table.attach(self.info_nmap_version_label,1,2,1,2)
        
        self.command_table.attach(self.verbose_label,0,1,2,3)
        self.command_table.attach(self.info_verbose_label,1,2,2,3)
        
        self.command_table.attach(self.debug_label,0,1,3,4)
        self.command_table.attach(self.info_debug_label,1,2,3,4)
        
        self.command_expander.add(self.command_hbox)
        self._pack_noexpand_nofill(self.command_expander)
        self.command_expander.set_expanded(True)
        
        # General info:
        self.start_label = HIGEntryLabel(_('Started on:'))
        self.info_start_label = HIGEntryLabel(na)
        
        self.finished_label = HIGEntryLabel(_('Finished on:'))
        self.info_finished_label = HIGEntryLabel(na)
        
        self.host_up_label = HIGEntryLabel(_('Hosts up:'))
        self.info_hosts_up_label = HIGEntryLabel(na)
        
        self.host_down_label = HIGEntryLabel(_('Hosts down:'))
        self.info_hosts_down_label = HIGEntryLabel(na)
        
        self.host_scanned_label = HIGEntryLabel(_('Hosts scanned:'))
        self.info_hosts_scanned_label = HIGEntryLabel(na)
        
        self.open_label = HIGEntryLabel(_('Open ports:'))
        self.info_open_label = HIGEntryLabel(na)
        
        self.filtered_label = HIGEntryLabel(_('Filtered ports:'))
        self.info_filtered_label = HIGEntryLabel(na)
        
        self.closed_label = HIGEntryLabel(_('Closed ports:'))
        self.info_closed_label = HIGEntryLabel(na)
        
        self.general_expander = gtk.Expander("<b>"+_("General Info")+"</b>")
        self.general_expander.set_use_markup(True)

        self.general_table = HIGTable()
        self.general_table.set_border_width(5)
        self.general_table.set_row_spacings(6)
        self.general_table.set_col_spacings(6)

        self.general_hbox = HIGHBox()
        self.general_hbox._pack_noexpand_nofill(hig_box_space_holder())
        self.general_hbox._pack_noexpand_nofill(self.general_table)
        
        self.general_table.attach(self.start_label,0,1,0,1)
        self.general_table.attach(self.info_start_label,1,2,0,1)
        
        self.general_table.attach(self.finished_label,0,1,1,2)
        self.general_table.attach(self.info_finished_label,1,2,1,2)
        
        self.general_table.attach(self.host_up_label,0,1,2,3)
        self.general_table.attach(self.info_hosts_up_label,1,2,2,3)
        
        self.general_table.attach(self.host_down_label,0,1,3,4)
        self.general_table.attach(self.info_hosts_down_label,1,2,3,4)
        
        self.general_table.attach(self.host_scanned_label,0,1,4,5)
        self.general_table.attach(self.info_hosts_scanned_label,1,2,4,5)
        
        self.general_table.attach(self.open_label,0,1,5,6)
        self.general_table.attach(self.info_open_label,1,2,5,6)
        
        self.general_table.attach(self.filtered_label,0,1,6,7)
        self.general_table.attach(self.info_filtered_label,1,2,6,7)
        
        self.general_table.attach(self.closed_label,0,1,7,8)
        self.general_table.attach(self.info_closed_label,1,2,7,8)
        
        self.general_expander.add(self.general_hbox)
        self._pack_noexpand_nofill(self.general_expander)
        self.general_expander.set_expanded(True)

        self._set_from_scan(scan)
    
    def _set_from_scan(self, scan):
        """Initialize the display from a parsed scan."""
        # Command info.
        self.info_command_label.set_text(scan.get_nmap_command())
        self.info_nmap_version_label.set_text(scan.get_scanner_version())
        self.info_verbose_label.set_text(scan.get_verbose_level())
        self.info_debug_label.set_text(scan.get_debugging_level())

        # General info.
        self.info_start_label.set_text(scan.get_formated_date())
        self.info_finished_label.set_text(scan.get_formated_finish_date())
        self.info_hosts_up_label.set_text(str(scan.get_hosts_up()))
        self.info_hosts_down_label.set_text(str(scan.get_hosts_down()))
        self.info_hosts_scanned_label.set_text(str(scan.get_hosts_scanned()))
        self.info_open_label.set_text(str(scan.get_open_ports()))
        self.info_filtered_label.set_text(str(scan.get_filtered_ports()))
        self.info_closed_label.set_text(str(scan.get_closed_ports()))

        for scaninfo in scan.get_scaninfo():
            exp = gtk.Expander('<b>%s - %s</b>' % (_('Scan Info'), scaninfo['type'].capitalize()))
            exp.set_use_markup(True)
            
            display = self.make_scaninfo_display(scaninfo)

            exp.add(display)
            self._pack_noexpand_nofill(exp)
             
    def make_scaninfo_display(self, scaninfo):
        """Return a widget displaying a scan's "scaninfo" information: type,
        protocol, number of scanned ports, and list of services."""
        hbox = HIGHBox()
        table = HIGTable()
        table.set_border_width(5)
        table.set_row_spacings(6)
        table.set_col_spacings(6)
        
        table.attach(HIGEntryLabel(_('Scan type:')),0,1,0,1)
        table.attach(HIGEntryLabel(scaninfo['type']),1,2,0,1)
        
        table.attach(HIGEntryLabel(_('Protocol:')),0,1,1,2)
        table.attach(HIGEntryLabel(scaninfo['protocol']),1,2,1,2)
        
        table.attach(HIGEntryLabel(_('# scanned ports:')),0,1,2,3)
        table.attach(HIGEntryLabel(scaninfo['numservices']),1,2,2,3)
        
        table.attach(HIGEntryLabel(_('Services:')),0,1,3,4)
        table.attach(self.make_services_display(scaninfo['services']),1,2,3,4)
        
        hbox._pack_noexpand_nofill(hig_box_space_holder())
        hbox._pack_noexpand_nofill(table)

        return hbox

    def make_services_display(self, services):
        """Return a widget displaying a list of services like
        1-1027,1029-1033,1040,1043,1050,1058-1059,1067-1068,1076,1080"""
        combo = gtk.combo_box_new_text()
        
        for i in services.split(","):
            combo.append_text(i)
        
        return combo

if __name__ == "__main__":
    import sys
    from zenmapCore.NmapParser import NmapParser

    filename = sys.argv[1]
    parsed = NmapParser()
    parsed.parse_file(filename)
    run_details = ScanRunDetailsPage(parsed)
    window = gtk.Window()
    window.add(run_details)
    window.connect("delete-event", lambda *args: gtk.main_quit())
    window.show_all()
    gtk.main()
