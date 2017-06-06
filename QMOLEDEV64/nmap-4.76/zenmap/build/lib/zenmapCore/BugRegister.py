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

import urllib
import urllib2

SF_BUG_TRACKER_SUBMIT = "http://sourceforge.net/tracker/index.php"

# Base URL data to create a new ticket at SourceForge.
BASE_QUERY_DATA = {
"group_id":          "142490",  # group_id and atid identify umit, I guess.
"atid":              "752647",
"func":              "postadd", # Means create a ticket.
"category_id":       "100",     # None.
"artifact_group_id": "100",     # None.
"assigned_to":       "1897047", # User name xvg (David Fifield).
"submit":            "SUBMIT"
}

class BugRegister(object):
    def get_report_url(self, summary, details):
        """Return a URL that submits a new ticket at the SourceForge bug tracker
        with the given summary and details."""
        query_data = {}
        query_data["summary"] = summary
        query_data["details"] = details
        # Lowest priority.
        query_data["priority"] = "1"

        query_data.update(BASE_QUERY_DATA)

        return SF_BUG_TRACKER_SUBMIT + "?" + urllib.urlencode(query_data)

if __name__ == "__main__":
    bug = BugRegister()
    print bug.get_report_url("Test", "Description.")
