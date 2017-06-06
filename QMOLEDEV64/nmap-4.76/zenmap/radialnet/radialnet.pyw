#!/usr/bin/env python
# vim: set fileencoding=utf-8 :

# Copyright (C) 2007 Insecure.Com LLC.
#
# Author: João Paulo de Souza Medeiros <ignotus21@gmail.com>
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

import os
import sys
import gtk

from core.ArgvHandle import *
from gui.Application import *


USAGE = """\
Description: Show RadialNet application from Nmap XML file. It's better if Nmap
XML input file has traceroute information. If you don't know what is this, see
Nmap documentation. You can pass the Nmap xml file by command line or open it
later.

Usage: radialnet.py [options]

Options:

    -h, --help      Show this help text.
    -f <file>       Use <file> as Nmap XML input.

Suggestions or bug reports can be send to ignotus21_at_gmail_dot_com.\
"""

# check for psyco
try:

    import psyco
    psyco.full()

except ImportError:
    print 'Running without psyco (http://psyco.sourceforge.net/).'



if __name__ == '__main__':

    argvh = ArgvHandle(sys.argv)

    if argvh.has_option('-h') or argvh.has_option('--help'):

        print USAGE
        sys.exit(0)

    application = Application()

    if argvh.has_option('-f'):

        file = argvh.get_option('-f')

        if file == None:

            print USAGE
            sys.exit(0)

        else:
            application.parse_nmap_xml_file(os.path.abspath(file))

    application.start()
