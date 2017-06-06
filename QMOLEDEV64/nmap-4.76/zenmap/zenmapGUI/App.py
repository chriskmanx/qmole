#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
#         Cleber Rodrigues <cleber.gnu@gmail.com>
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

import imp
import os
import signal
import sys
import ConfigParser

# Cause an exception if PyGTK can't open a display. Normally this just
# produces a warning, but the lack of a display eventually causes a
# segmentation fault. See http://live.gnome.org/PyGTK/WhatsNew210.
import warnings
warnings.filterwarnings("error", module = "gtk")
import gtk
warnings.resetwarnings()

from zenmapGUI.higwidgets.higdialogs import HIGAlertDialog

import zenmapCore.UmitConf
import zenmapCore.Paths
from zenmapCore.UmitConf import is_maemo
from zenmapCore.UmitLogging import log
from zenmapCore.UmitOptionParser import option_parser
from zenmapCore.Name import APP_DISPLAY_NAME, NMAP_DISPLAY_NAME
from zenmapCore.UmitConf import SearchConfig
from zenmapCore.I18N import _
from zenmapCore.UmitDB import UmitDB
from zenmapCore.Paths import Path
from zenmapCore.Name import APP_DISPLAY_NAME

from zenmapGUI.MainWindow import ScanWindow

from zenmapGUI.higwidgets.higdialogs import HIGAlertDialog

# A global list of open scan windows. When the last one is destroyed, we call
# gtk.main_quit.
open_windows = []

def _destroy_callback(window):
    open_windows.remove(window)
    if len(open_windows) == 0:
        gtk.main_quit()
    # Cleaning up data base
    UmitDB().cleanup(SearchConfig().converted_save_time)

def new_window():
    w = ScanWindow()
    w.connect("destroy", _destroy_callback)
    if is_maemo():
        import hildon
        hildon_app = hildon.Program()
        hildon_app.add_window(window)
    open_windows.append(w)
    return w

# Script found at http://www.py2exe.org/index.cgi/HowToDetermineIfRunningFromExe
def main_is_frozen():
    return (hasattr(sys, "frozen") # new py2exe
            or hasattr(sys, "importers") # old py2exe
            or imp.is_frozen("__main__")) # tools/freeze

def is_root():
    return sys.platform == "win32" or os.getuid() == 0 or is_maemo()

def safe_shutdown(signum, stack):
    """Kills any active scans/tabs and shuts down the application."""
    log.debug("\n\n%s\nSAFE SHUTDOWN!\n%s\n" % ("#" * 30, "#" * 30))
    log.debug("SIGNUM: %s" % signum)

    for window in open_windows:
        window.scan_interface.kill_all_scans()

    sys.exit(signum)

def run():
    if os.name == "posix":
        signal.signal(signal.SIGHUP, safe_shutdown)
    signal.signal(signal.SIGTERM, safe_shutdown)
    signal.signal(signal.SIGINT, safe_shutdown)

    try:
        # Create the ~/.zenmap directory by copying from the system-wide
        # template directory.
        zenmapCore.Paths.create_user_config_dir(Path.user_config_dir, Path.config_dir)
    except (IOError, OSError), e:
        error_dialog = HIGAlertDialog(message_format = _("Error creating the per-user configuration directory"),
            secondary_text = _("""\
There was an error creating the directory %s or one of the files in it. \
The directory is created by copying the contents of %s. \
The specific error was\n\
\n\
%s\n\
\n\
%s needs to create this directory to store information such as the list of \
scan profiles. Check for access to the directory and try again.\
""") % (Path.user_config_dir, Path.config_dir, str(e), APP_DISPLAY_NAME))
        error_dialog.run()
        error_dialog.destroy()
        sys.exit(1)

    try:
        # Read the ~/.zenmap/zenmap.conf configuration file.
        zenmapCore.UmitConf.config_parser.read(Path.user_config_file)
    except ConfigParser.ParsingError, e:
        error_dialog = HIGAlertDialog(message_format = _("Error parsing the configuration file"),
            secondary_text = _("""\
There was an error parsing the configuration file %s. \
The specific error was\n\
\n\
%s\n\
\n\
%s can continue without this file but any information in it will be ignored \
until it is repaired.\
""") % (Path.user_config_file, str(e), APP_DISPLAY_NAME))
        error_dialog.run()
        error_dialog.destroy()

    # Display a "you're not root" warning if appropriate.
    if not is_root():
        non_root = NonRootWarning()
        non_root.run()
        non_root.destroy()

    # Load files given as command-line arguments.
    filenames = option_parser.get_open_results()
    if len(filenames) == 0:
        # Open up a blank window.
        window = new_window()
        window.show_all()
    else:
        for filename in filenames:
            window = new_window()
            if os.path.isdir(filename):
                window._load_directory(window.scan_interface, filename)
            else:
                window._load(window.scan_interface, filename)
            window.show_all()

    nmap = option_parser.get_nmap()
    target = option_parser.get_target()
    profile = option_parser.get_profile()

    if nmap:
        # Start running a scan if given by the -n option.
        page = window.get_empty_page()
        page.command_toolbar.command = " ".join(nmap)
        page.start_scan_cb()
    elif target or profile:
        # Set up target and profile according to the -t and -p options.
        page = window.get_empty_page()
        if target:
            page.toolbar.selected_target = target
        if profile:
            page.toolbar.selected_profile = profile
        if target and profile:
            page.start_scan_cb()

    if main_is_frozen():
        # This is needed by py2exe
        gtk.gdk.threads_init()
        gtk.gdk.threads_enter()

    gtk.main()

    if main_is_frozen():
        gtk.gdk.threads_leave()

class NonRootWarning (HIGAlertDialog):
    def __init__(self):
        warning_text = _('''You are trying to run %s with a non-root user!\n
Some %s options need root privileges to work.''' % (APP_DISPLAY_NAME, NMAP_DISPLAY_NAME))
        
        HIGAlertDialog.__init__(self, message_format=_('Non root user'),
                                secondary_text=warning_text)
