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

import gobject
import gtk
import pango
import os
import os.path
import tempfile
import webbrowser
import xml.sax

from zenmapGUI.higwidgets.higdialogs import HIGAlertDialog, HIGDialog
from zenmapGUI.higwidgets.higboxes import HIGVBox, HIGHBox, hig_box_space_holder
from zenmapGUI.higwidgets.higlabels import HIGEntryLabel, HIGSectionLabel
from zenmapGUI.higwidgets.higtables import HIGTable
from zenmapGUI.higwidgets.higbuttons import HIGButton

from zenmapCore.Name import APP_DISPLAY_NAME, NMAP_DISPLAY_NAME
from zenmapCore.Diff import Diff
from zenmapCore.UmitConf import UmitConf, DiffColors
from zenmapCore.NmapParser import NmapParser, HostInfo
from zenmapCore.Paths import Path
from zenmapCore.UmitLogging import log
from zenmapCore.I18N import _

from types import StringTypes
from zenmapGUI.FileChoosers import ResultsFileSingleChooserDialog

# difflib.HtmlDiff, used by zenmapCore.DiffHtml, is only in Python 2.4 or later.
try:
    from zenmapCore.DiffHtml import DiffHtml
    use_html = True
except:
    use_html = False


class ScanChooser(HIGVBox):
    """This class allows the selection of scan results from the list of open
    tabs or from a file. It emits the "changed" signal when the scan selection
    has changed."""

    __gsignals__ = {
        "changed": (gobject.SIGNAL_RUN_FIRST, gobject.TYPE_NONE, ())
    }

    def __init__(self, scan_dict, num=""):
        self.__gobject_init__()
        self.num = num
        self.scan_dict = scan_dict
        
        # Setting HIGVBox
        self.set_border_width(5)
        self.set_spacing(6)
        
        self._create_widgets()
        self._pack_hbox()
        self._attaching_widgets()
        self._set_scrolled()
        self._set_text_view()
        self._set_open_button()
        
        for scan in scan_dict:
            self.list_scan.append([scan])
        
        self.combo_scan.connect('changed', self.show_scan)
        self.combo_scan.connect('changed', lambda x: self.emit('changed'))
        
        self._pack_noexpand_nofill(self.lbl_scan)
        self._pack_expand_fill(self.hbox)
    
    def _create_widgets(self):
        self.lbl_scan = HIGSectionLabel("%s %s"%(_("Scan Result"), str(self.num)))
        self.hbox = HIGHBox()
        self.table = HIGTable()
        self.list_scan = gtk.ListStore(str)
        self.combo_scan = gtk.ComboBoxEntry(self.list_scan, 0)
        self.btn_open_scan = gtk.Button(stock=gtk.STOCK_OPEN)
        self.exp_scan = gtk.Expander(_("Scan Result Visualization"))
        self.scrolled = gtk.ScrolledWindow()
        self.txt_scan_result = gtk.TextView()
        self.txg_tag = gtk.TextTag("scan_style")

    def get_buffer(self):
        return self.txt_scan_result.get_buffer()
    
    def show_scan (self, widget):
        nmap_output = self.get_nmap_output()
        if nmap_output is not None:
            self.txt_scan_result.get_buffer().set_text(nmap_output)

    def normalize_output(self, output):
        return "\n".join(output.split("\\n"))

    def _pack_hbox (self):
        self.hbox._pack_noexpand_nofill(hig_box_space_holder())
        self.hbox._pack_expand_fill(self.table)

    def _attaching_widgets (self):
        self.table.attach(self.combo_scan, 0,1,0,1, yoptions=0)
        self.table.attach(self.btn_open_scan, 1,2,0,1, yoptions=0, xoptions=0)
        self.table.attach(self.exp_scan, 0,2,1,2)
    
    def _set_scrolled(self):
        self.scrolled.set_border_width(5)
        self.scrolled.set_size_request(-1, 160)
        
        # Packing scrolled window into expander
        self.exp_scan.add(self.scrolled)
        
        # Packing text view into scrolled window
        self.scrolled.add_with_viewport(self.txt_scan_result)
        
        # Setting scrolled window
        self.scrolled.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
    
    def _set_text_view (self):
        self.txg_table = self.txt_scan_result.get_buffer().get_tag_table()
        self.txg_table.add(self.txg_tag)
        self.txg_tag.set_property("family", "Monospace")
        
        self.txt_scan_result.set_wrap_mode(gtk.WRAP_WORD)
        self.txt_scan_result.set_editable(False)
        self.txt_scan_result.get_buffer().connect("changed", self._text_changed_cb)

    def _set_open_button (self):
        self.btn_open_scan.connect('clicked', self.open_file)
    
    def open_file (self, widget):
        file_chooser = ResultsFileSingleChooserDialog(_("Select Scan Result"))
        
        response = file_chooser.run()
        file_chosen = file_chooser.get_filename()
        file_chooser.destroy()
        if response == gtk.RESPONSE_OK:
            try:
                parser = NmapParser()
                parser.parse_file(file_chosen)
            except xml.sax.SAXParseException, e:
                alert = HIGAlertDialog(
                    message_format='<b>%s</b>' % _('Error parsing file'),
                    secondary_text=_("The file is not an Nmap XML output file. \
The parsing error that occurred was\n%s" % str(e)))
                alert.run()
                alert.destroy()
                return False
            except Exception, e:
                alert = HIGAlertDialog(
                        message_format='<b>%s</b>' % _('Cannot open selected file'),
                        secondary_text=_("This error occurred while trying to open the file:\n%s" % str(e)))
                alert.run()
                alert.destroy()
                return False

            scan_name = os.path.split(file_chosen)[-1]
            self.add_scan(scan_name, parser)
            
            self.combo_scan.set_active(len(self.list_scan) - 1)

    def add_scan(self, scan_name, parser):
        scan_id = 1
        new_scan_name = scan_name
        while new_scan_name in self.scan_dict.keys():
            new_scan_name = "%s (%s)" % (scan_name, scan_id)
            scan_id += 1
                
        self.list_scan.append([new_scan_name])
        self.scan_dict[new_scan_name] = parser
    
    def _text_changed_cb (self, widget):
        buff = self.txt_scan_result.get_buffer ()
        buff.apply_tag(self.txg_tag, buff.get_start_iter(), buff.get_end_iter())

    def get_parsed_scan(self):
        """Return the currently selected scan's parsed output as an NmapParser
        object, or None if no valid scan is selected."""
        selected_scan = self.combo_scan.child.get_text()
        if selected_scan in self.scan_dict:
            return self.scan_dict[selected_scan]
        # What's typed in the entry doesn't match a registered scan.
        return None

    def get_nmap_output(self):
        """Return the currently selected scan's output as a string, or None if
        no valid scan is selected."""
        parsed = self.parsed_scan
        if parsed is not None:
            return parsed.nmap_output
        return None

    nmap_output = property(get_nmap_output)
    parsed_scan = property(get_parsed_scan)


class DiffWindow(gtk.Window):
    def __init__(self, scans):
        """scans in the format: {"scan_title":parsed_scan}
        """
        gtk.Window.__init__(self)
        self.set_title(_("Compare Results"))
        self.scans = scans

        self.umit_conf = UmitConf()
        self.colors = Colors()
        
        # Diff views
        self.text_view = DiffText(self.colors, self.umit_conf.colored_diff)
        self.compare_view = DiffTree(self.colors)

        self.temp_html_file = None

        self._create_widgets()
        self._pack_widgets()
        self._connect_widgets()


        # Settings
        if self.umit_conf.diff_mode == "text":
            self.text_mode.set_active(True)
        else:
            self.compare_mode.set_active(True)
        self.check_color.set_active(self.umit_conf.colored_diff)

        # Initial Size Request
        self.initial_size = self.size_request()

    def _show_help(self, action):
        webbrowser.open("file://%s" % os.path.join(Path.docs_dir, "help.html"), new=2)

    def _create_widgets(self):
        self.main_vbox = HIGVBox()
        self.hbox_mode = HIGHBox()
        self.hbox_settings = HIGHBox()
        self.hbox_buttons = HIGHBox()
        self.hbox_result = HIGHBox()
        self.btn_open_browser = HIGButton(_("Open in Browser"), stock=gtk.STOCK_EXECUTE)
        self.btn_help = HIGButton(stock=gtk.STOCK_HELP)
        self.btn_close = HIGButton(stock=gtk.STOCK_CLOSE)
        self.check_color = gtk.CheckButton(_("Enable colored diffies"))
        self.btn_legend = HIGButton(_("Color Descriptions"), stock=gtk.STOCK_SELECT_COLOR)
        self.text_mode = gtk.ToggleButton(_("Text Mode"))
        self.compare_mode = gtk.ToggleButton(_("Compare Mode"))
        self.vpaned = gtk.VPaned()
        self.hpaned = gtk.HPaned()
        self.scan_chooser1 = ScanChooser(self.scans, "1")
        self.scan_chooser2 = ScanChooser(self.scans, "2")
        self.scan_buffer1 = self.scan_chooser1.get_buffer()
        self.scan_buffer2 = self.scan_chooser2.get_buffer()

    def _pack_widgets(self):
        self.main_vbox.set_border_width(6)
        
        self.vpaned.pack1(self.hpaned, True, False)
        self.vpaned.pack2(self.hbox_result)
        self.hpaned.pack1(self.scan_chooser1, True, False)
        self.hpaned.pack2(self.scan_chooser2, True, False)

        self.hbox_buttons._pack_expand_fill(self.btn_help)
        self.hbox_buttons._pack_expand_fill(self.btn_legend)
        self.hbox_buttons._pack_expand_fill(self.btn_open_browser)
        self.hbox_buttons._pack_expand_fill(self.btn_close)
        self.hbox_buttons.set_homogeneous(True)

        self.hbox_mode.set_homogeneous(True)
        self.hbox_mode.pack_start(self.text_mode)
        self.hbox_mode.pack_start(self.compare_mode)
        self.hbox_settings._pack_noexpand_nofill(self.hbox_mode)
        self.hbox_settings._pack_expand_fill(self.check_color)

        self.main_vbox._pack_expand_fill(self.vpaned)
        self.main_vbox._pack_noexpand_nofill(self.hbox_settings)
        self.main_vbox._pack_noexpand_nofill(self.hbox_buttons)

        self.add(self.main_vbox)

    def _connect_widgets(self):
        self.connect("delete-event", self.close)
        self.btn_legend.connect("clicked", self.show_legend_window)
        self.btn_help.connect("clicked", self._show_help)
        self.btn_close.connect("clicked", self.close)
        self.btn_open_browser.connect("clicked", self.open_browser)
        self.check_color.connect("toggled", self._set_color)
        self.text_mode.connect("clicked", self._change_to_text)
        self.compare_mode.connect("clicked", self._change_to_compare)
        self.scan_chooser1.exp_scan.connect('activate', self.resize_vpane)
        self.scan_chooser2.exp_scan.connect('activate', self.resize_vpane)
        self.scan_chooser1.connect('changed', self.refresh_diff)
        self.scan_chooser2.connect('changed', self.refresh_diff)

    def open_browser(self, widget):
        text1 = self.scan_chooser1.nmap_output
        text2 = self.scan_chooser2.nmap_output

        if text1 is None or text2 is None:
            alert = HIGAlertDialog(
                    message_format='<b>'+_('Select Scan')+'</b>',
                    secondary_text=_("You must select two different scans to \
generate diff."))
            alert.run()
            alert.destroy()
            return False

        if text1 == '' and text2 == '':
            alert = HIGAlertDialog(
                    message_format='<b>'+_('No Text Output')+'</b>',
                    secondary_text=_("Neither of the scans you selected has \
any text output. (Scans loaded from plain Nmap XML output files do not contain \
text output.) The HTML diff shows only differences between text output, so \
there is nothing to show."))
            alert.run()
            alert.destroy()
            return False
        
        # True tells splitlines to keep line endings.
        text1 = text1.splitlines(True)
        text2 = text2.splitlines(True)

        if self.temp_html_file is not None:
            self.temp_html_file.close()
        # A NamedTemporaryFile is deleted when it is closed.
        self.temp_html_file = tempfile.NamedTemporaryFile(suffix = ".html", prefix = "zenmap-diff-")
        if use_html:
            diff = DiffHtml(text1, text2)
            diff = diff.generate()
            self.temp_html_file.write(''.join(diff))
        else:
            diff = Diff(text1, text2)
            diff = diff.generate ()
            diff.insert(0, '''<pre>(This diff is been shown in pure text \
because you dont have Python 2.4 or higher.)\n''')
            diff.append('</pre>')
            self.temp_html_file.writelines(diff)
        self.temp_html_file.flush()
        webbrowser.open("file://" + self.temp_html_file.name, autoraise=1)

    def show_legend_window(self, widget):
        legend_window = DiffLegendWindow(self.colors)
        legend_window.run()
        legend_window.destroy()
        self.refresh_diff(None)

    def refresh_diff (self, widget):
        """This method is called whenever the diff output might have changed,
        such as when a different scan was selected in one of the choosers."""

        if self.compare_mode.get_active():
            # Graphical comparison mode.
            parsed1 = self.scan_chooser1.parsed_scan
            parsed2 = self.scan_chooser2.parsed_scan

            if parsed1 is not None and parsed2 is not None:
                self.compare_view.make_diff(parsed1, parsed2)
            else:
                self.compare_view.clear_diff_tree()
            self.compare_view.activate_color(self.check_color.get_active())
        else:
            # Text comparison mode.
            text1 = self.scan_chooser1.nmap_output
            text2 = self.scan_chooser2.nmap_output

            if text1 is not None and text2 is not None:
                diff = Diff(text1.split('\n'), text2.split('\n'))
                self.text_view.set_text('\n'.join(diff.generate_without_banner()))
            else:
                self.text_view.clear()
            self.text_view.activate_color(self.check_color.get_active())

    def resize_vpane(self, widget):
        exp1 = not widget.get_expanded()
        if widget == self.scan_chooser1.exp_scan:
            exp2 = self.scan_chooser2.exp_scan.get_expanded()
        else:
            exp2 = self.scan_chooser1.exp_scan.get_expanded()

        if not exp1 and not exp2:
            self.vpaned.compute_position(-1, 0, 500)
            self.size_allocate(gtk.gdk.Rectangle(width=self.initial_size[0],
                                                 height=self.initial_size[1]))
            self.queue_resize()

    def _change_to_text(self, widget):
        if not widget.get_active():
            return

        self.umit_conf.diff_mode = "text"

        children = self.hbox_result.get_children()
        if children:
            self.hbox_result.remove(children[0])
            self.compare_view.hide()
        
        self.hbox_result._pack_expand_fill(self.text_view)
        self.text_view.show_all()
        
        self.compare_mode.set_active(False)
        self.refresh_diff(None)

    def _change_to_compare(self, widget):
        if not widget.get_active():
            return

        self.umit_conf.diff_mode = "compare"

        children = self.hbox_result.get_children()
        if children:
            self.hbox_result.remove(children[0])
            self.text_view.hide()
        
        self.hbox_result._pack_expand_fill(self.compare_view)
        self.compare_view.show_all()
        
        self.text_mode.set_active(False)
        self.refresh_diff(None)

    def _set_color(self, widget):
        activate = widget.get_active()
        self.umit_conf.colored_diff = activate
        self.compare_view.activate_color(activate)
        self.text_view.activate_color(activate)

    def close(self, widget=None, extra=None):
        if self.temp_html_file is not None:
            self.temp_html_file.close()
        self.destroy()


class DiffText(HIGVBox, object):
    def __init__ (self, colors, check_color):
        HIGVBox.__init__(self)
        self.set_border_width(5)
        self.set_spacing(6)

        self.colors = colors
        self.check_color = check_color
        
        self._create_widgets()
        self._pack_hbox()
        self._set_text_view()
        self._set_scrolled()
        
        self._pack_noexpand_nofill(self.lbl_diff)
        self._pack_expand_fill(self.hbox)
    
    def _create_widgets (self):
        self.hbox = HIGHBox ()
        self.lbl_diff = HIGSectionLabel ("<b>%s</b>" % _("Diff Result"))
        self.scrolled = gtk.ScrolledWindow()
        self.txt_diff_result = gtk.TextView()
        self.txg_tag = gtk.TextTag ("diff_style")
        self.txg_added = gtk.TextTag ('added style')
        self.txg_removed = gtk.TextTag ('removed style')

    def _pack_hbox (self):
        self.hbox.set_border_width(5)
        self.hbox._pack_noexpand_nofill (hig_box_space_holder())
        self.hbox._pack_expand_fill(self.scrolled)

    def _set_scrolled (self):
        self.scrolled.set_size_request(-1, 250)
        
        # Packing text view into scrolled window
        self.scrolled.add_with_viewport(self.txt_diff_result)
        
        # Setting scrolled window
        self.scrolled.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

    def set_text(self, text):
        """Set the raw diff text of this diff view."""
        self.txt_diff_result.get_buffer().set_text(text)

    def clear(self):
        """Make this diff view empty."""
        self.set_text("")

    def activate_color(self, activate):
        self.check_color = activate
        self._refresh()
    
    def _set_text_view(self):
        self.txg_table = self.txt_diff_result.get_buffer().get_tag_table()
        self.txg_table.add(self.txg_tag)
        self.txg_table.add(self.txg_added)
        self.txg_table.add(self.txg_removed)
        self.txg_tag.set_property("family", "Monospace")
        self.txg_added.set_property("background-gdk", self.colors.added)
        self.txg_removed.set_property("background-gdk", self.colors.not_present)
        
        self.txt_diff_result.set_wrap_mode(gtk.WRAP_WORD)
        self.txt_diff_result.set_editable(False)
        self.txt_diff_result.get_buffer().connect("changed", self._refresh)
        
    def _refresh(self, *args):
        self.txg_added.set_property("background-gdk", self.colors.added)
        self.txg_removed.set_property("background-gdk", self.colors.not_present)
        
        buff = self.txt_diff_result.get_buffer()

        buff.apply_tag(self.txg_tag, buff.get_start_iter(), buff.get_end_iter())

        if self.check_color:
            positions = self._take_changes(buff)
            
            for i in positions['added']:
                buff.apply_tag(self.txg_added, i[0],i[1])
            
            for i in positions['removed']:
                buff.apply_tag(self.txg_removed, i[0], i[1])
        else:
            buff.remove_tag(self.txg_added, buff.get_start_iter (), buff.get_end_iter())
            buff.remove_tag(self.txg_removed, buff.get_start_iter (), buff.get_end_iter())

    def _take_changes (self, buffer):
        positions = {'added':[], 'removed':[]}

        in_line = 0
        type = ''

        iter = buffer.get_start_iter ()
        last_char = '\n'

        while iter.forward_char():
            char = iter.get_char ()
            offset = iter.get_offset()
            if not in_line:
                pos = []
                if char == '+' and last_char == '\n':
                    pos.append (buffer.get_iter_at_offset(offset))
                    type = 'added'
                    in_line = 1
                elif char == '-' and last_char == '\n':
                    pos.append (buffer.get_iter_at_offset(offset))
                    type = 'removed'
                    in_line = 1
            else:
                if char == '\n':
                    pos.append (buffer.get_iter_at_offset(offset))
                    positions [type].append (pos)
                    in_line = 0
                    type = ''
            last_char = char
        
        return positions


class DiffTree(HIGVBox, object):
    def __init__(self, colors):
        HIGVBox.__init__(self)

        self.colors = colors
        self.set_border_width(5)
        self.set_spacing(6)

        self._create_widgets()
        self._set_diff_view()
        self._pack_widgets()

    def _create_widgets(self):
        self.diff_box = HIGHBox()
        self.diff_title = HIGSectionLabel("Comparison")
        self.diff_scrolled = gtk.ScrolledWindow()
        self.diff_tree = gtk.TreeStore(str, str, str, str, str, str)
        self.diff_view = gtk.TreeView(self.diff_tree)
        self.diff_column1 = gtk.TreeViewColumn("")
        self.diff_column2 = gtk.TreeViewColumn(_("Section"))
        self.diff_column3 = gtk.TreeViewColumn(_("Property"))
        self.diff_column4 = gtk.TreeViewColumn(_("Original value"))
        self.diff_column5 = gtk.TreeViewColumn(_("Current value"))
        self.diff_cell = gtk.CellRendererText()


    def activate_color(self, activate):
        if activate:
            self.diff_column1.set_attributes(self.diff_cell, text=0, background=5)
            self.diff_column2.set_attributes(self.diff_cell, text=1, background=5)
            self.diff_column3.set_attributes(self.diff_cell, text=2, background=5)
            self.diff_column4.set_attributes(self.diff_cell, text=3, background=5)
            self.diff_column5.set_attributes(self.diff_cell, text=4, background=5)
        else:
            self.diff_column1.clear_attributes(self.diff_cell)
            self.diff_column2.clear_attributes(self.diff_cell)
            self.diff_column3.clear_attributes(self.diff_cell)
            self.diff_column4.clear_attributes(self.diff_cell)
            self.diff_column5.clear_attributes(self.diff_cell)
            
            self.diff_column1.set_attributes(self.diff_cell, text=0)
            self.diff_column2.set_attributes(self.diff_cell, text=1)
            self.diff_column3.set_attributes(self.diff_cell, text=2)
            self.diff_column4.set_attributes(self.diff_cell, text=3)
            self.diff_column5.set_attributes(self.diff_cell, text=4)

            self.diff_cell.set_property("background", "white")

    def _set_diff_view(self):
        font_desc = pango.FontDescription("Normal 9")
        self.diff_cell.set_property("font-desc", font_desc)
        
        self.diff_scrolled.set_size_request(-1, 260)
        self.diff_view.set_enable_search(True)
        self.diff_view.set_search_column(2)

        selection = self.diff_view.get_selection()
        selection.set_mode(gtk.SELECTION_MULTIPLE)

        self.diff_view.append_column(self.diff_column1)
        self.diff_view.append_column(self.diff_column2)
        self.diff_view.append_column(self.diff_column3)
        self.diff_view.append_column(self.diff_column4)
        self.diff_view.append_column(self.diff_column5)

        # Diff Column 1
        self.diff_column1.set_reorderable(True)
        self.diff_column1.set_resizable(True)
        self.diff_column1.connect("clicked", self.search_column, 0)
        self.diff_column1.set_sort_column_id(0)
        self.diff_column1.pack_start(self.diff_cell, True)
        self.diff_column1.set_attributes(self.diff_cell, text=0)
        

        # Diff Column 2
        self.diff_column2.set_reorderable(True)
        self.diff_column2.set_resizable(True)
        self.diff_column2.connect("clicked", self.search_column, 1)
        self.diff_column2.set_sort_column_id(1)
        self.diff_column2.pack_start(self.diff_cell, True)
        self.diff_column2.set_attributes(self.diff_cell, text=1)

        # Diff Column 3
        self.diff_column3.set_reorderable(True)
        self.diff_column3.set_resizable(True)
        self.diff_column3.connect("clicked", self.search_column, 2)
        self.diff_column3.set_sort_column_id(2)
        self.diff_column3.pack_start(self.diff_cell, True)
        self.diff_column3.set_attributes(self.diff_cell, text=2)

        # Diff Column 4
        self.diff_column4.set_reorderable(True)
        self.diff_column4.set_resizable(True)
        self.diff_column4.connect("clicked", self.search_column, 3)
        self.diff_column4.set_sort_column_id(3)
        self.diff_column4.pack_start(self.diff_cell, True)
        self.diff_column4.set_attributes(self.diff_cell, text=3)

        # Diff Column 5
        self.diff_column5.set_reorderable(True)
        self.diff_column5.set_resizable(True)
        self.diff_column5.connect("clicked", self.search_column, 4)
        self.diff_column5.set_sort_column_id(4)
        self.diff_column5.pack_start(self.diff_cell, True)
        self.diff_column5.set_attributes(self.diff_cell, text=4)

        self.diff_scrolled.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

    def clear_diff_tree(self):
        for i in range(len(self.diff_tree)):
            iter = self.diff_tree.get_iter_root()
            del(self.diff_tree[iter])

    def change_status(self, iter, status):
        self.diff_tree[iter][0] = status
        self.diff_tree[iter][5] = self.colors.get_hex_color(status)

    def set_parent_status(self, parent, status):
        if status != "" and status.upper() != "U":
            while parent:
                self.change_status(parent, "M")
                parent = self.diff_tree.iter_parent(parent)
        
    def make_diff(self, parsed1, parsed2):
        self.clear_diff_tree()

        section = _("%s Info" % APP_DISPLAY_NAME)
        parent = self.append_parent(None, section, "")
        self.diff_it(parent, "", _("Profile"), parsed1.profile, parsed2.profile)
        self.diff_it(parent, "", _("Profile Name"), parsed1.profile_name,
                     parsed2.profile_name)
        self.diff_it(parent, "", _("Profile Options"),
                     ",".join(parsed1.profile_options.keys()),
                     ",".join(parsed2.profile_options.keys()))
        self.diff_it(parent, "", _("Target"), parsed1.target, parsed2.target)

        section = _("%s Info" % NMAP_DISPLAY_NAME)
        parent = self.append_parent(None, section, "")
        
        self.diff_it(parent, "", _("Debugging"), parsed1.debugging_level,
                     parsed2.debugging_level)
        self.diff_it(parent, "", _("Verbosity"), parsed1.verbose_level,
                     parsed2.verbose_level)
        self.diff_it(parent, "", _("Command"), parsed1.nmap_command,
                     parsed2.nmap_command)
        self.diff_it(parent, "", _("Scanner version"), parsed1.scanner_version,
                     parsed2.scanner_version)

        section = _("Scan Info")
        parent = self.append_parent(None, section, "")
        
        self.diff_it(parent, "", _("Open Ports"), parsed1.open_ports,
                     parsed2.open_ports)
        self.diff_it(parent, "", _("Filtered Ports"), parsed1.filtered_ports,
                     parsed2.filtered_ports)
        self.diff_it(parent, "", _("Closed Ports"), parsed1.closed_ports,
                     parsed2.closed_ports)
        self.diff_it(parent, "", _("Hosts Up"), parsed1.hosts_up,
                     parsed2.hosts_up)
        self.diff_it(parent, "", _("Hosts Down"), parsed1.hosts_down,
                     parsed2.hosts_down)
        self.diff_it(parent, "", _("Hosts Scanned"), parsed1.hosts_scanned,
                     parsed2.hosts_scanned)
        self.diff_it(parent, "", _("Finish date"), parsed1.formated_finish_date,
                     parsed2.formated_finish_date)

        hosts1 = parsed1.hosts[:]
        hosts2 = parsed2.hosts[:]
        while hosts1:
            host = hosts1.pop()
            
            second_host = HostInfo()
            host_state = "N"
            for host2 in hosts2:
                if (host.mac and host.mac == host2.mac) or \
                       (host.ip and host.ip == host2.ip) or \
                       (host.ipv6 and host.ipv6 == host2.ipv6):
                    second_host = host2
                    host_state = ""
                    
                    del(hosts2[hosts2.index(host2)]) # Remove it from the hosts2
                    break

            self.add_host_diff(host_state, host, second_host)

        for host in hosts2:
            self.add_host_diff("A", host, host)

    def add_host_diff(self, host_state, host, host2=None):
            section = _("Host")
            if host.ip:
                section = _("Host %s") % (host.ip["addr"])
            elif host.ipv6:
                section = _("Host %s") % (host.ipv6["addr"])
            elif host.mac:
                section = _("Host %s") % (host.mac["addr"])

            parent = self.append_parent(None, section, host_state)

            self.diff_it(parent, "", _("Comment"), host.comment,
                         host2.comment)
            self.diff_it(parent, "", _("LastBoot"), host.uptime.get("lastboot", ""),
                         host2.uptime.get("lastboot", ""))
            self.diff_it(parent, "", _("OS Match"), host.get_best_osmatch().get("name", ""),
                         host2.get_best_osmatch().get("name", ""))


            host_ports = host.ports[:]
            host2_ports = host2.ports[:]
            for port in xrange(len(host_ports)):
                # Making sure that extraports1 will get a sanity value to be processed
                try:
                    extraports1 = host_ports[port].get("extraports", [])
                except:
                    extraports1 = {}
                else:
                    if len(extraports1) == 0:
                        extraports1 = {}
                    else:
                        extraports1 = extraports1[0]

                # Making sure that extraports2 will get a sanity value to be processed
                try:
                    extraports2 = host2_ports[port].get("extraports", [])
                except:
                    extraports2 = {}
                else:
                    if len(extraports2) == 0:
                        extraports2 = {}
                    else:
                        extraports2 = extraports2[0]

                
                if extraports1 and extraports2:
                    self.add_extraports_diff(parent, "", extraports1, extraports2)
                elif extraports1 and not extraports2:
                    self.add_extraports_diff(parent, "N", extraports1, extraports2)
                elif not extraports1 and extraports2:
                    self.add_extraports_diff(parent, "A", extraports1, extraports2)
                

                section =  _("Ports")
                parent = self.append_parent(parent, section, "")


                # Making sure that ports1 will get a sanity value to be processed
                try:
                    ports1 = host_ports[port].get("port", [])
                except:
                    ports1 = {}
                else:
                    if len(ports1) == 0:
                        ports1 = {}
                    elif len(ports1) == 1:
                        ports1 = ports1[0]

                # Making sure that ports2 will get a sanity value to be processed
                try:
                    ports2 = host2_ports[port].get("port", [])
                except:
                    ports2 = {}
                else:
                    if len(ports2) == 0:
                        ports2 = [{}]
                    elif len(ports2) == 1:
                        ports2 = ports2[0]
                
                if type(ports2)!= type([]):
                    ports2 = [ports2]

                if type(ports1) != type([]):
                    ports1 = [ports1]

                for p1 in ports1:
                    if not p1:
                        continue

                    p2 = [port2 for port2 in ports2 \
                          if port2.get("portid", "a") == p1.get("portid", "b")]
                    
                    if p2: # Removing found port
                        ports2.remove(p2[0])

                    if p1 and p2:
                        self.add_port_diff(parent, "", p1, p2[0])
                    elif p1 and not p2:
                        self.add_port_diff(parent, "N", p1, {})

                for p2 in ports2: # If there is something left...
                    self.add_port_diff(parent, "A", {}, p2)
            

    def add_port_diff(self, port_parent, state, port1, port2):
        if (port1 or port2) and (type(port1) == type({})) and (type(port2) == type({})):
            section = port1.get("portid", False)
            if not section: # If port1 is empty, then, try port2
                section = port2.get("portid", "")
            
            parent = self.append_parent(port_parent, section, state)

            self.diff_it(parent, "",
                         _("State"), port1.get("port_state", ""),
                         port2.get("port_state", ""))
            
            self.diff_it(parent, "",
                         _("Service Name"), port1.get("service_name", ""),
                         port2.get("service_name", ""))
                
            self.diff_it(parent, "",
                         _("Product"), port1.get("service_product", ""),
                         port2.get("service_product", ""))
            
            self.diff_it(parent, "",
                         _("Service Version"), port1.get("service_version", ""),
                         port2.get("service_version", ""))
            
            self.diff_it(parent, "",
                         _("Protocol"), port1.get("protocol", ""),
                         port2.get("protocol", ""))
                
            self.diff_it(parent, "",
                         _("Extra Info"), port1.get("service_extrainfo", ""),
                         port2.get("service_extrainfo", ""))
            
            self.diff_it(parent, "",
                         _("Service Conf"), port1.get("service_conf", ""),
                         port2.get("service_conf", ""))

            # Last parent status modification
            if state.upper() == "A":
                self.change_status(parent, "A")

    def add_extraports_diff(self, host_parent, state, extraports1, extraports2):
        if extraports1 or extraports2:
            section =  _("Extraports")
            parent = self.append_parent(host_parent, section, state)
            self.set_parent_status(parent, state)
            
            self.diff_it(parent, "", _("Count"), extraports1.get("count"),
                         extraports2.get("count"))
            self.diff_it(parent, "", _("State"), extraports1.get("state"),
                         extraports2.get("state"))


    def diff_it(self, parent, section, prop_name, prop1, prop2):
        if prop1 or prop2:
            state = diff_state(prop1, prop2)
            self.set_parent_status(parent, state)
            self.diff_tree.append(parent, [state,
                                           section,
                                           prop_name,
                                           prop1,
                                           prop2,
                                           self.colors.get_hex_color(state)])
            return state

    def append_parent(self, parent, section, state):
        self.set_parent_status(parent, state)
        return self.diff_tree.append(parent, [state, section, "", "", "",
                                              self.colors.get_hex_color(state)])

    def search_column(self, widget, column_id):
        self.diff_view.set_search_column(column_id)

    def _pack_widgets(self):
        self._pack_noexpand_nofill(self.diff_title)
        self._pack_expand_fill(self.diff_box)
        self.diff_box._pack_noexpand_nofill(hig_box_space_holder())
        self.diff_box._pack_expand_fill(self.diff_scrolled)

        self.diff_scrolled.add(self.diff_view)

class DiffLegendWindow(HIGDialog, object):
    def __init__(self, colors):
        # Shows colors and chars legend
        HIGDialog.__init__(self, title=_('Color Descriptions'),
                          buttons=(gtk.STOCK_OK, gtk.RESPONSE_ACCEPT))

        self.colors = colors
        self._create_widgets()
        self._pack_widgets()
        self._connect_widgets()

    def _create_widgets(self):
        self.table = HIGTable()
        
        self.unchanged_button = gtk.ColorButton(self.colors.unchanged)
        self.unchanged_label = gtk.Label(_("Property remained <b>U</b>nchanged"))
        
        self.added_button = gtk.ColorButton(self.colors.added)
        self.added_label = gtk.Label(_("Property was <b>A</b>dded"))
        
        self.modified_button = gtk.ColorButton(self.colors.modified)
        self.modified_label = gtk.Label(_("Property was <b>M</b>odified"))
        
        self.not_present_button = gtk.ColorButton(self.colors.not_present)
        self.not_present_label = gtk.Label(_("Property is <b>N</b>ot present"))

    def _pack_widgets(self):
        self.unchanged_label.set_use_markup(True)
        self.added_label.set_use_markup(True)
        self.modified_label.set_use_markup(True)
        self.not_present_label.set_use_markup(True)

        self.table.attach_label(self.unchanged_button, 0, 1, 0, 1)
        self.table.attach_entry(self.unchanged_label, 1, 2, 0, 1)

        self.table.attach_label(self.added_button, 0, 1, 1, 2)
        self.table.attach_entry(self.added_label, 1, 2, 1, 2)

        self.table.attach_label(self.modified_button, 0, 1, 2, 3)
        self.table.attach_entry(self.modified_label, 1, 2, 2, 3)

        self.table.attach_label(self.not_present_button, 0, 1, 3, 4)
        self.table.attach_entry(self.not_present_label, 1, 2, 3, 4)

        self.vbox.pack_start(self.table)
        self.vbox.show_all()

    def _connect_widgets(self):
        self.unchanged_button.connect("color-set", self.set_color, "unchanged")
        self.added_button.connect("color-set", self.set_color, "added")
        self.modified_button.connect("color-set", self.set_color, "modified")
        self.not_present_button.connect("color-set", self.set_color, "not_present")

    def set_color(self, widget, prop):
        self.colors.__setattr__(prop, widget.get_color())

class Colors(object):
    def __init__(self):
        self.diff_colors = DiffColors()

    def get_unchanged(self):
        return gtk.gdk.Color(*self.diff_colors.unchanged)

    def set_unchanged(self, color):
        if type(color) == type([]) or type(color) in StringTypes:
            self.diff_colors.unchanged = color
        else:
            self.diff_colors.unchanged = [color.red, color.green, color.blue]

    def get_added(self):
        return gtk.gdk.Color(*self.diff_colors.added)

    def set_added(self, color):
        if type(color) == type([]) or type(color) in StringTypes:
            self.diff_colors.added = color
        else:
            self.diff_colors.added = [color.red, color.green, color.blue]

    def get_modified(self):
        return gtk.gdk.Color(*self.diff_colors.modified)

    def set_modified(self, color):
        if type(color) == type([]) or type(color) in StringTypes:
            self.diff_colors.modified = color
        else:
            self.diff_colors.modified = [color.red, color.green, color.blue]

    def get_not_present(self):
        return gtk.gdk.Color(*self.diff_colors.not_present)

    def set_not_present(self, color):
        if type(color) == type([]) or type(color) in StringTypes:
            self.diff_colors.not_present = color
        else:
            self.diff_colors.not_present = [color.red, color.green, color.blue]

    def get_hex_unchanged(self):
        return self._get_hex(self.unchanged)

    def get_hex_added(self):
        return self._get_hex(self.added)

    def get_hex_modified(self):
        return self._get_hex(self.modified)

    def get_hex_not_present(self):
        return self._get_hex(self.not_present)

    def _get_hex(self, color):
        if type(color) == type([]):
            return "#%4s%4s%4s" % (hex(color[0])[2:].zfill(4),
                                   hex(color[1])[2:].zfill(4),
                                   hex(color[2])[2:].zfill(4))
        else:
            return "#%4s%4s%4s" % (hex(color.red)[2:].zfill(4),
                                   hex(color.green)[2:].zfill(4),
                                   hex(color.blue)[2:].zfill(4))

    def get_hex_color(self, state):
        state = state.upper()
        
        if state == "A":
            return self.hex_added
        elif state == "M":
            return self.hex_modified
        elif state == "N":
            return self.hex_not_present
        else:
            return self.hex_unchanged
    
    unchanged = property(get_unchanged, set_unchanged)
    added = property(get_added, set_added)
    modified = property(get_modified, set_modified)
    not_present = property(get_not_present, set_not_present)

    hex_unchanged = property(get_hex_unchanged)
    hex_added = property(get_hex_added)
    hex_modified = property(get_hex_modified)
    hex_not_present = property(get_hex_not_present)


def diff_state(prop1, prop2):
    if prop1 == prop2:
        return "U" # Property remained "Unchanged" at the second scan
    elif prop1 == "" and prop2 != "":
        return "A" # Property "Added" at the second scan
    elif prop1 != "" and prop2 != "":
        return "M" # Property "Modified" at the second scan
    else:
        return "N" # Property "Not present" at the second scan


if __name__ == "__main__":
    from zenmapCore.NmapParser import NmapParser

    parsed1 = NmapParser()
    parsed2 = NmapParser()
    parsed3 = NmapParser()
    parsed4 = NmapParser()

    parsed1.parse_file("test/xml_test1.xml")
    parsed2.parse_file("test/xml_test2.xml")
    parsed3.parse_file("test/xml_test3.xml")
    parsed4.parse_file("test/xml_test4.xml")
    
    dw = DiffWindow({"Parsed 1": parsed1,
                     "Parsed 2": parsed2,
                     "Parsed 3": parsed3,
                     "Parsed 4": parsed4})

    dw.show_all()
    dw.connect("delete-event", lambda x,y: gtk.main_quit())

    gtk.main()
