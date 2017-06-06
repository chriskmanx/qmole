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

import sys
import gtk

from zenmapCore.I18N import _

RESPONSE_OPEN_DIRECTORY = 1

class AllFilesFileFilter(gtk.FileFilter):
    def __init__(self):
        gtk.FileFilter.__init__(self)

        pattern = "*"
        self.add_pattern(pattern)
        self.set_name(_("All files (%s)") % pattern)
        
class ResultsFileFilter(gtk.FileFilter):
    def __init__(self):
        gtk.FileFilter.__init__(self)

        patterns = ["*.xml", "*.usr"]
        for pattern in patterns:
            self.add_pattern(pattern)
        self.set_name(_("Nmap XML files (%s)") % ", ".join(patterns))

class UnicodeFileChooserDialog(gtk.FileChooserDialog):
    """This is a base class for file choosers. It is designed to ease the
    retrieval of Unicode file names. On most platforms, the file names returned
    are encoded in the encoding given by sys.getfilesystemencoding(). On
    Windows, they are returned in UTF-8, even though using the UTF-8 file name
    results in a file not found error. The get_filename method of this class
    handles the decoding automatically."""
    def get_filename(self):
        filename = gtk.FileChooserDialog.get_filename(self)
        if sys.platform == "win32":
            encoding = "UTF-8"
        else:
            encoding = sys.getfilesystemencoding() or "UTF-8"
        try:
            filename = filename.decode(encoding)
        except:
            pass
        return filename

class AllFilesFileChooserDialog(UnicodeFileChooserDialog):
    def __init__(self, title="", parent=None,
                 action=gtk.FILE_CHOOSER_ACTION_OPEN,
                 buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                          gtk.STOCK_OPEN, gtk.RESPONSE_OK), backend=None):

        gtk.FileChooserDialog.__init__(self, title, parent,
                                       action, buttons)
        self.set_default_response(gtk.RESPONSE_OK)
        self.add_filter(AllFilesFileFilter())

class ResultsFileSingleChooserDialog(UnicodeFileChooserDialog):
    """This results file choose only allows the selection of single files, not
    directories."""
    def __init__(self, title="", parent=None,
                 action=gtk.FILE_CHOOSER_ACTION_OPEN,
                 buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                          gtk.STOCK_OPEN, gtk.RESPONSE_OK), backend=None):

        UnicodeFileChooserDialog.__init__(self, title, parent,
                                       action, buttons)
        self.set_default_response(gtk.RESPONSE_OK)
        for f in (ResultsFileFilter(), AllFilesFileFilter()):
            self.add_filter(f)

class ResultsFileChooserDialog(UnicodeFileChooserDialog):
    def __init__(self, title="", parent=None,
                 action=gtk.FILE_CHOOSER_ACTION_OPEN,
                 buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                          "Open Directory", RESPONSE_OPEN_DIRECTORY,
                          gtk.STOCK_OPEN, gtk.RESPONSE_OK), backend=None):

        UnicodeFileChooserDialog.__init__(self, title, parent,
                                       action, buttons)
        self.set_default_response(gtk.RESPONSE_OK)
        for f in (ResultsFileFilter(), AllFilesFileFilter()):
            self.add_filter(f)

class SaveResultsFileChooserDialog(UnicodeFileChooserDialog):
    def __init__(self, title="", parent=None,
                 action=gtk.FILE_CHOOSER_ACTION_SAVE,
                 buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                          gtk.STOCK_SAVE, gtk.RESPONSE_OK), backend=None):

        UnicodeFileChooserDialog.__init__(self, title, parent, action, buttons)
        self.set_default_response(gtk.RESPONSE_OK)
        for f in (ResultsFileFilter(), AllFilesFileFilter()):
            self.add_filter(f)

class DirectoryChooserDialog(UnicodeFileChooserDialog):
    def __init__(self, title="", parent=None,
                 action=gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER,
                 buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                          gtk.STOCK_OPEN, gtk.RESPONSE_OK), backend=None):

        UnicodeFileChooserDialog.__init__(self, title, parent, action, buttons)
        self.set_default_response(gtk.RESPONSE_OK)

class SaveToDirectoryChooserDialog(UnicodeFileChooserDialog):
    def __init__(self, title="", parent=None,
                 action=gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER,
                 buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                          gtk.STOCK_SAVE, gtk.RESPONSE_OK), backend=None):

        UnicodeFileChooserDialog.__init__(self, title, parent, action, buttons)
        self.set_default_response(gtk.RESPONSE_OK)
