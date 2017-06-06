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

# This file contains the definitions of two main classes:
# NmapCommand represents and runs an Nmap command line. CommandConstructor
# builds a command line string from textual option descriptions.

import codecs
import locale
import sys
import os
import re
import tempfile
import unittest
import gtk

from types import StringTypes
try:
    from subprocess import Popen, PIPE
except ImportError, e:
    raise ImportError(str(e) + ".\n" + _("Python 2.4 or later is required."))

import zenmapCore.Paths
from zenmapCore.Paths import Path
from zenmapCore.NmapOptions import NmapOptions
from zenmapCore.UmitLogging import log
from zenmapCore.I18N import _
from zenmapCore.UmitConf import PathsConfig
from zenmapCore.Name import APP_NAME

# This variable is used in the call to Popen. It determines whether the
# subprocess invocation uses the shell or not. If it is False on Unix, the nmap
# process is started with execve and a list of arguments, which is what we want.
# (Indeed it fails when shell_state = True because it tries to exec
# ['sh', '-c', 'nmap', '-v', ...], which is wrong.) So normally we would want
# shell_state = False. But if shell_state = False on Windows, a big ugly black
# shell window opens whenever a scan is run, at least under py2exe. So we define
# shell_state = True on Windows only. Windows doesn't have exec, so it runs the
# command basically the same way regardless of shell_state.
shell_state = (sys.platform == "win32")

# The path to the nmap executable as used by Popen.
# Find the value from configuation file paths nmap_command_path
# to use for the location of the nmap executable.
nmap_paths = PathsConfig()
nmap_command_path = nmap_paths.nmap_command_path

log.debug(">>> Platform: %s" % sys.platform)
log.debug(">>> Nmap command path: %s" % nmap_command_path)

def split_quoted(s):
    """Like str.split, except that no splits occur inside quoted strings, and
    quoted strings are unquoted."""
    return [x.replace("\"", "") for x in re.findall('((?:"[^"]*"|[^"\s]+)+)', s)]

def wrap_file_in_preferred_encoding(f):
    """Wrap an open file to automatically decode its contents when reading from
    the encoding given by locale.getpreferredencoding, or just return the file
    if that doesn't work.

    The nmap executable will write its output in whatever the system encoding
    is. Nmap's output is usually all ASCII, but time zone it prints can be in a
    different encoding. If it is not decoded correctly it will be displayed as
    garbage characters. This function assists in reading the Nmap output. We
    don't know for sure what the encoding used is, but we take a best guess and
    decode the output into a proper unicode object so that the screen display
    and XML writer interpret it correctly."""

    try:
        preferredencoding = locale.getpreferredencoding()
    except locale.Error:
        # This can happen if the LANG environment variable is set to something
        # weird.
        preferredencoding = None

    if preferredencoding is not None:
        try:
            reader = codecs.getreader(preferredencoding)
            return reader(f, "replace")
        except LookupError:
            # The lookup failed. This can happen if the preferred encoding is
            # unknown ("X-MAC-KOREAN" has been observed). Ignore it and return
            # the unwrapped file.
            log.debug("Unknown encoding \"%s\"." % preferredencoding)

    return f

def escape_nmap_filename(filename):
    """Escape '%' characters so they are not interpreted as strftime format
    specifiers, which are not supported by Zenmap."""
    return filename.replace("%", "%%")

class NmapCommand(object):
    """This class represents an Nmap command line. It is responsible for
    starting, stopping, and returning the results from a command-line scan. A
    command line is represented as a string but it is split into a list of
    arguments for execution."""

    def __init__(self, command):
        """Initialize an Nmap command. This creates temporary files for
        redirecting the various types of output and sets the backing
        command-line string."""
        self.command = command
        self.command_process = None

        self._stdout_file = None

        # Get the command as a list of options
        self.command_list = self._get_sanitized_command_list()
        
        # Go through the list and look for -oX or -oA, because that means the
        # user has specified an XML output file. When we find one, we escape '%'
        # characters to avoid strftime expansion and insert it back into the
        # command. We also escape the arguments to -oG, -oN, and -oS for
        # uniformity although we don't use the file names. If we find a -oX or
        # -oA option, set self.xml_is_temp to False and don't delete the file
        # after we're done. Otherwise, generate a random output file name and
        # delete it when the scan is finished.
        self.xml_is_temp = True
        self.xml_output_filename = None
        i = 0
        while i < len(self.command_list):
            if self.command_list[i] == "-oX":
                self.xml_is_temp = False
                if i == len(self.command_list) - 1:
                    break
                self.xml_output_filename = self.command_list[i + 1]
                escaped_xml_output_filename = escape_nmap_filename(self.xml_output_filename)
                self.command_list[i + 1] = escaped_xml_output_filename
                i += 1
            elif self.command_list[i] == "-oA":
                self.xml_is_temp = False
                if i == len(self.command_list) - 1:
                    break
                xml_output_prefix = self.command_list[i + 1]
                self.xml_output_filename = xml_output_prefix + ".xml"
                escaped_xml_output_prefix = escape_nmap_filename(xml_output_prefix)
                self.command_list[i + 1] = escaped_xml_output_prefix
                i += 1
            elif self.command_list[i] in ("-oG", "-oN", "-oS"):
                if i == len(self.command_list) - 1:
                    break
                escaped_filename = escape_nmap_filename(self.command_list[i + 1])
                self.command_list[i + 1] = escaped_filename
            i += 1

        if self.xml_is_temp:
            self.xml_output_filename = tempfile.mktemp(prefix = APP_NAME + "-", suffix = ".xml")
            escaped_xml_output_filename = escape_nmap_filename(self.xml_output_filename)
            self.command_list.append("-oX")
            self.command_list.append("%s" % escaped_xml_output_filename)

        log.debug(">>> Temporary files:")
        log.debug(">>> XML OUTPUT: %s" % self.xml_output_filename)

    def _get_sanitized_command_list(self):
        """Remove comments from the command, add output options, and return the
        command split up into a list ready for execution."""
        command = self.command

        # Remove comments from command.
        command = re.sub('#.*', '', command)

        # Split back into individual options, honoring double quotes.
        command_list = split_quoted(command)        

        # Replace the executable name with the value of nmap_command_path.
        command_list[0] = nmap_command_path

        return command_list

    def close(self):
        """Close and remove temporary output files used by the command."""
        self._stdout_file.close()
        if self.xml_is_temp:
            os.remove(self.xml_output_filename)

    def kill(self):
        """Kill the nmap subprocess."""
        log.debug(">>> Killing scan process %s" % self.command_process.pid)

        if sys.platform != "win32":
            try:
                from signal import SIGKILL
                os.kill(self.command_process.pid, SIGKILL)
            except:
                pass
        else:
            try:
                # Not sure if this works. Must research a bit more about this
                # subprocess's method to see how it works.
                # In the meantime, this should not raise any exception because
                # we don't care if it killed the process as it never killed it anyway.
                from subprocess import TerminateProcess
                TerminateProcess(self.command_process._handle, 0)
            except:
                pass

    def get_path(self):
        """Return a value for the PATH environment variable that is appropriate
        for the current platform. It will be the PATH from the environment plus
        possibly some platform-specific directories."""
        path_env = os.getenv("PATH")
        if path_env is None:
            search_paths = []
        else:
            search_paths = path_env.split(os.pathsep)
        for path in zenmapCore.Paths.get_extra_executable_search_paths():
            if path not in search_paths:
                search_paths.append(path)
        return os.pathsep.join(search_paths)

    def run_scan(self):
        """Run the command represented by this class."""

        # We don't need a file name for stdout output, just a handle. A
        # TemporaryFile is deleted as soon as it is closed, and in Unix is
        # unlinked immediately after creation so it's not even visible.
        f = tempfile.TemporaryFile(mode = "rb", prefix = APP_NAME + "-stdout-")
        self._stdout_file = wrap_file_in_preferred_encoding(f)

        search_paths = self.get_path()
        env = dict(os.environ)
        env["PATH"] = search_paths
        log.debug("PATH=%s" % env["PATH"])

        log.debug("Running command: %s" % repr(self.command_list))

        self.command_process = Popen(self.command_list, bufsize=1,
                                     stdin=PIPE,
                                     stdout=f.fileno(),
                                     stderr=f.fileno(),
                                     shell=shell_state,
                                     env=env)

    def scan_state(self):
        """Return the current state of a running scan. A return value of True
        means the scan is running and a return value of False means the scan
        subprocess completed successfully. If the subprocess terminated with an
        error an exception is raised. The scan must have been started with
        run_scan before calling this method."""
        if self.command_process == None:
            raise Exception("Scan is not running yet!")

        state = self.command_process.poll()

        if state == None:
            return True # True means that the process is still running
        elif state == 0:
            return False # False means that the process had a successful exit
        else:
            log.warning("An error occurred during the scan execution!")
            log.warning("Command that raised the exception: '%s'" %
                         " ".join(self.command_list))
            log.warning("Scan output:\n%s" % self.get_output())

            raise Exception("An error occurred during the scan execution!\n\n'%s'" % self.get_output())

    def get_output(self):
        """Return the stdout of the nmap subprocess."""
        self._stdout_file.seek(0)
        return self._stdout_file.read()

    def get_xml_output_filename(self):
        """Return the name of the XML (-oX) output file."""
        return self.xml_output_filename

class CommandConstructor:
    """This class builds a string representing an Nmap command line from textual
    option descriptions such as 'Aggressive Options' or 'UDP Scan'
    (corresponding to -A and -sU respectively). The name-to-option mapping is
    done by the NmapOptions class. Options are stored in a dict that maps the
    option name to a tuple containing its arguments and "level." The level is
    the degree of repetition for options like -v that can be given more than
    once."""

    def __init__(self, options = {}):
        """Initialize a command line using the given options. The options are
        given as a dict mapping option names to arguments."""
        self.options = {}
        self.option_profile = NmapOptions(Path.options)
        for k, v in options.items():
            self.add_option(k, v, False)

    def add_option(self, option_name, args=[], level=False):
        """Add an option to the command line. Only one of args and level can be
        defined. If both are defined, level takes precedence and args is
        ignored."""
        self.options[option_name] = (args, level)

    def remove_option(self, option_name):
        """Remove an option from the command line."""
        if option_name in self.options.keys():
            del(self.options[option_name])

    def get_command(self, target):
        """Return the contructed command line as a plain string."""
        splited = ['%s' % nmap_command_path]

        for option_name in self.options:
            option = self.option_profile.get_option(option_name)
            args, level = self.options[option_name]

            if type(args) in StringTypes:
                args = [args]

            if level:
                splited.append((option['option']+' ')*level)
            elif args:
                args = tuple (args)
                splited.append(option['option'] % args[0])
            else:
                splited.append(option['option'])
            
        splited.append(target)
        return ' '.join(splited)

    def get_options(self):
        """Return the options used in the command line, as a dict mapping
        options names to arguments. The level, if any, is discarded."""
        return dict([(k, v[0]) for k, v in self.options.items()])

class SplitQuotedTest(unittest.TestCase):
    """A unittest class that tests the split_quoted function."""

    def test_split(self):
        self.assertEqual(split_quoted(''), [])
        self.assertEqual(split_quoted('a'), ['a'])
        self.assertEqual(split_quoted('a b c'), 'a b c'.split())

    def test_quotes(self):
        self.assertEqual(split_quoted('a "b" c'), ['a', 'b', 'c'])
        self.assertEqual(split_quoted('a "b c"'), ['a', 'b c'])
        self.assertEqual(split_quoted('a "b c""d e"'), ['a', 'b cd e'])
        self.assertEqual(split_quoted('a "b c"z"d e"'), ['a', 'b czd e'])

# Module test code.
if __name__ == '__main__':
    unittest.TextTestRunner().run(unittest.TestLoader().loadTestsFromTestCase(SplitQuotedTest))

    # This is an example of how CommandConstructor works. Nmap options are given
    # textual option descriptions.
    command = CommandConstructor()
    command.add_option('Aggressive')
    command.add_option('Version detection')
    command.add_option('UDP Scan')
    command.add_option('Idle Scan', ['10.0.0.138'])
    command.add_option('UDP Scan')
    command.add_option('ACK scan')
    command.remove_option('Idle Scan')
    print command.get_command('localhost')
