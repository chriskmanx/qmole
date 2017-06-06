#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
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

class ScansListStoreEntry(object):
    """This class is an abstraction for running and completed scans, which are
    otherwise represented by very different classes."""

    # Possible states for the scan to be in.
    UNINITIALIZED, RUNNING, FINISHED, FAILED, CANCELED = range(5)

    def __init__(self):
        self.state = self.UNINITIALIZED
        self.command = None
        self.parsed = None

    def set_running(self, command):
        self.state = self.RUNNING
        self.command = command

    def set_finished(self, parsed):
        self.state = self.FINISHED
        self.parsed = parsed

    def set_failed(self):
        self.state = self.FAILED

    def set_canceled(self):
        self.state = self.CANCELED

    def get_command_string(self):
        if self.running or self.failed or self.canceled:
            return self.command.command
        elif self.finished:
            return self.parsed.get_nmap_command()
        else:
            return None

    running = property(lambda self: self.state == self.RUNNING)
    finished = property(lambda self: self.state == self.FINISHED)
    failed = property(lambda self: self.state == self.FAILED)
    canceled = property(lambda self: self.state == self.CANCELED)

class ScansListStore(gtk.ListStore):
    """This is a specialization of a gtk.ListStore that holds running,
    completed, and failed scans."""
    def __init__(self):
        gtk.ListStore.__init__(self, object)

    def add_running_scan(self, command):
        """Add a running NmapCommand object to the list of scans."""
        entry = ScansListStoreEntry()
        entry.set_running(command)
        return self.append([entry])

    def finish_running_scan(self, command, parsed):
        """Find an existing NmapCommand object and replace it with the given
        parsed representation."""
        i = self._find_running_scan(command)
        if i is not None:
            entry = self.get_value(i, 0)
            entry.set_finished(parsed)
            path = self.get_path(i)
            self.row_changed(path, i)
            return i

    def fail_running_scan(self, command):
        """Mark a running scan as failed."""
        i = self._find_running_scan(command)
        if i is not None:
            entry = self.get_value(i, 0)
            entry.set_failed()
            path = self.get_path(i)
            self.row_changed(path, i)
            return i

    def cancel_running_scan(self, command):
        """Mark a running scan as canceled."""
        i = self._find_running_scan(command)
        if i is not None:
            entry = self.get_value(i, 0)
            entry.set_canceled()
            path = self.get_path(i)
            self.row_changed(path, i)
            return i

    def add_scan(self, parsed):
        """Add a parsed NmapParser object to the list of scans."""
        entry = ScansListStoreEntry()
        entry.set_finished(parsed)
        return self.append([entry])

    def _find_running_scan(self, command):
        """Find the scan entry whose command is command."""
        i = self.get_iter_first()
        while i is not None:
            entry = self.get_value(i, 0)
            if entry.command is command:
                return i
            i = self.iter_next(i)
        return None
