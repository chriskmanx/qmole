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

import os
import os.path
import re
import StringIO

from glob import glob
from types import StringTypes

from zenmapCore.Name import APP_NAME
from zenmapCore.UmitDB import UmitDB
from zenmapCore.NmapParser import NmapParser
from zenmapCore.UmitLogging import log


class SearchResult(object):    
    def __init__(self):
        """This constructor is always called by SearchResult subclasses."""
        pass
    
    def search(self, **kargs):
        """Performs a search on each parsed scan. Since the 'and' operator is
        implicit, the search fails as soon as one of the tests fails. The
        kargs argument is a map having operators as keys and argument lists as
        values."""
        
        for scan_result in self.get_scan_results():
            self.parsed_scan = scan_result
            
            # Test each given operator against the current parsed result
            for operator, args in kargs.iteritems():
                if not self._match_all_args(operator, args):
                    # No match => we discard this scan_result
                    break
            else:
                # All operator-matching functions have returned True, so this scan_result
                # satisfies all conditions
                yield self.parsed_scan

    def _match_all_args(self, operator, args):
        """A helper function that calls the matching function for the given
        operator and each of its arguments."""
        for arg in args:
            if not self.__getattribute__("match_%s" % operator)(arg):
                # No match for this operator
                return False
        else:
            # All arguments for this operator produced a match
            return True
    
    def get_scan_results(self):
        # To be implemented by classes that are going to inherit this one
        pass

    def basic_match(self, keyword, property):
        if keyword == "*" or keyword == "":
            return True
        
        return keyword.lower() in str(self.parsed_scan.__getattribute__(property)).lower()

    def match_keyword(self, keyword):
        log.debug("Match keyword: %s" % keyword)
        
        return self.basic_match(keyword, "nmap_output") or \
               self.match_profile(keyword) or \
               self.match_target(keyword)

    def match_profile(self, profile):
        log.debug("Match profile: %s" % profile)
        log.debug("Comparing: %s == %s ??" % (str(self.parsed_scan.profile_name).lower(),
                                              "*%s*" % profile.lower()))
        if profile == "*" or profile == "" or \
           profile.lower() in str(self.parsed_scan.profile_name).lower():
            return True
        return False
    
    def match_option(self, option):
        log.debug("Match option: %s" % option)
        
        if option == "*" or option == "":
            return True
        
        # NOTE: Option matching treats "_" and "-" the same, just like the optcmp
        #       function in utils.cc . Also, option matching is case-sensitive.
        option = option.replace("_", "-")
        
        # We get to the options by taking out "nmap " and targets from the command line
        targets = self.parsed_scan.get_target()
        cmd = self.parsed_scan.get_nmap_command()
        if targets != "":
            options = cmd[4:cmd.find(targets)]
        else:
            options = cmd[4:]
        
        if "(" in option and ")" in option:
            # The syntax allows matching option arguments as "opt:option_name(value)".
            # Since we've received only the "option_name(value)" part, we need to parse it.
            optname = option[:option.find("(")]
            optval = option[option.find("(")+1:option.find(")")]
            
            return (optname + "=" + optval) in options or \
                   (optname + " " + optval) in options or \
                   (optname + optval) in options
        else:
            return ("-" + option) in options

    def match_date(self, date_arg, operator="date"):
        # The parsed scan's get_date() returns a time.struct_time, so we
        # need to convert it to a date object
        from datetime import date, datetime
        scd = self.parsed_scan.get_date()
        scan_date = date(scd.tm_year, scd.tm_mon, scd.tm_mday)
        
        # Check if we have any fuzzy operators ("~") in our string
        fuzz = 0
        if "~" in date_arg:
            # Count 'em, and strip 'em
            fuzz = date_arg.count("~")
            date_arg = date_arg.replace("~", "")
        
        if re.match("\d\d\d\d-\d\d-\d\d", date_arg) != None:
            year, month, day = date_arg.split("-")
            parsed_date = date(int(year), int(month), int(day))
        elif re.match("[-|\+]\d+", date_arg):
            # We need to convert from the "-n" format (n days ago) to a date object
            # (I found this in some old code, don't ask :) )
            parsed_date = date.fromordinal(date.today().toordinal() + int(date_arg))
        else:
            # Fail silently
            return False
        
        # Now that we have both the scan date and the user date converted to date objects,
        # we need to make a comparison based on the operator (date, after, before).
        if operator == "date":
            return abs((scan_date - parsed_date).days) <= fuzz
        # We ignore fuzziness for after: and before:
        elif operator == "after":
            return (scan_date - parsed_date).days >= 0
        elif operator == "before":
            return (parsed_date - scan_date).days >= 0
    
    def match_after(self, date_arg):
        return self.match_date(date_arg, operator="after")
    
    def match_before(self, date_arg):
        return self.match_date(date_arg, operator="before")
    
    def match_target(self, target):
        log.debug("Match target: %s" % target)
        
        if self.basic_match(target, "target"):
            return True
        else:
            # We search the (rDNS) hostnames list
            for hostname in self.parsed_scan.get_hostnames():
                if target in hostname["hostname"]:
                    return True
            # We search the address list
            addrlist = self.parsed_scan.get_mac() + \
                       self.parsed_scan.get_ipv4() + \
                       self.parsed_scan.get_ipv6()
            for addr in addrlist:
                if target in addr:
                    return True
        return False
    
    def match_os(self, os):
        # If you have lots of big scans in your DB (with a lot of hosts scanned),
        # you're probably better off using the keyword (freetext) search. Keyword
        # search just greps through the nmap output, while this function iterates
        # through all parsed OS-related values for every host in every scan! 
        hosts = self.parsed_scan.get_hosts()
        os = os.lower()
        for host in hosts:
            for osclass in host.get_osclasses():
                for value in osclass.itervalues():
                    if os in value.lower():
                        return True
            for osmatch in host.get_osmatches():
                for value in osmatch.itervalues():
                    if os in value.lower():
                        return True
        
        return False
    
    def match_scanned(self, ports):
        if ports == "":
            return True
        
        # Transform a comma-delimited string containing ports into a list
        ports = filter(lambda not_empty: not_empty, ports.split(","))
        
        # Check if they're parsable, if not return False silently
        for port in ports:
            if re.match("^\d+$", port) == None:
                return False
        
        # Make a list of all scanned ports
        services = []
        for scaninfo in self.parsed_scan.get_scaninfo():
            services = services + scaninfo["services"].split(",")
        
        # These two loops iterate over search ports and over scanned ports. As soon as
        # the search finds a given port among the scanned ports, it breaks from the services
        # loop and continues with the next port in the ports list. If a port isn't
        # found in the services list, the function immediately returns False.
        for port in ports:
            for service in services:
                if "-" in service and \
                   int(port) >= int(service.split("-")[0]) and \
                   int(port) <= int(service.split("-")[1]):
                    # Port range, and our port was inside
                    break
                elif port == service:
                    break
            else:
                return False
        else:
            # The ports loop finished for all ports, which means the search was successful.
            return True
    
    def match_port(self, ports, port_state):
        log.debug("Match port:%s" % ports)
        
        # Transform a comma-delimited string containing ports into a list
        ports = filter(lambda not_empty: not_empty, ports.split(","))
        
        # Check if they're parsable, if not return False silently
        for port in ports:
            if re.match("^\d+$", port) == None:
                return False
        
        # Get all scanned ports that are in a state that matches port_state
        scanned_ports = []
        for p in self.parsed_scan.ports:
            for port_dic in p:
                for portid in port_dic["port"]:
                    if portid["port_state"] == port_state:
                        scanned_ports.append(portid["portid"])
        
        if len(ports) == 0:
            # In this case, a user has given us only the port state, but not the
            # port itself (for example, "fp:"), so we need to return True if
            # this scan has any ports in the given state.
            return (len(scanned_ports) > 0)
        else:
            # Return True only if all given ports are in the desired state
            for port in ports:
                if port not in scanned_ports:
                    return False
            else:
                return True
    
    def match_open(self, port):
        return self.match_port(port, "open")
    
    def match_filtered(self, port):
        return self.match_port(port, "filtered")
    
    def match_closed(self, port):
        return self.match_port(port, "closed")
    
    def match_unfiltered(self, port):
        return self.match_port(port, "unfiltered")
    
    def match_open_filtered(self, port):
        return self.match_port(port, "open|filtered")
    
    def match_closed_filtered(self, port):
        return self.match_port(port, "closed|filtered")
    
    def match_service(self, sversion):
        if sversion == "" or sversion == "*":
            return True
        
        versions = []
        for first in self.parsed_scan.ports:
            for ports in first:
                for port in ports["port"]:
                    if port["service_name"] not in versions:
                        # We concatenate all useful fields and add them to the list
                        version = port["service_name"] + " " + \
                                  port["service_product"] + " " + \
                                  port["service_version"] + " " + \
                                  port["service_extrainfo"]
                        version = version.lower()
                        versions.append(version)
        
        for v in versions:
            if sversion.lower() in v:
                return True
        else:
            return False
    
    def match_in_route(self, host):
        if host == "" or host == "*":
            return True
        
        # Since the parser doesn't parse traceroute output, we need to cheat and look
        # the host up in the Nmap output, in the Traceroute section of the scan.
        nmap_out = self.parsed_scan.get_nmap_output()
        tr_pos = 0
        traceroutes = []        # A scan holds one traceroute section per host
        while tr_pos != -1:
            # Find the beginning and the end of the traceroute section, and append
            # the substring to the traceroutes list
            tr_pos = nmap_out.find("TRACEROUTE", tr_pos+1)
            tr_end_pos = nmap_out.find("\n\n", tr_pos)
            if tr_pos != -1:
                traceroutes.append(nmap_out[tr_pos:tr_end_pos])
        
        for tr in traceroutes:
            if host.lower() in tr.lower():
                return True
        else:
            return False
    
    def match_dir(self, dir):
        # The dir: operator is handled by the SearchParser class, we ignore it here.
        return True


class SearchDB(SearchResult, object):
    def __init__(self):
        SearchResult.__init__(self)
        log.debug(">>> Getting scan results stored in data base")
        self.scan_results = []
        u = UmitDB()

        for scan in u.get_scans():
            log.debug(">>> Retrieving result of scans_id %s" % scan.scans_id)
            log.debug(">>> Nmap xml output: %s" % scan.nmap_xml_output)
            
            try:
                buffer = StringIO.StringIO(scan.nmap_xml_output)
                parsed = NmapParser()
                parsed.parse(buffer)
                buffer.close()
            except Exception, e:
                log.warning(">>> Error loading scan with ID %u from database: %s" % (scan.scans_id, str(e)))
            else:
                self.scan_results.append(parsed)
    
    def get_scan_results(self):
        return self.scan_results

class SearchDir(SearchResult, object):
    def __init__(self, search_directory, file_extensions=["usr"]):
        SearchResult.__init__(self)
        log.debug(">>> SearchDir initialized")
        self.search_directory = search_directory

        if type(file_extensions) in StringTypes:
            self.file_extensions = file_extensions.split(";")
        elif type(file_extensions) == type([]):
            self.file_extensions = file_extensions
        else:
            raise Exception("Wrong file extension format! '%s'" % file_extensions)
        
        log.debug(">>> Getting directory's scan results")
        self.scan_results = []
        files = []
        for ext in self.file_extensions:
            files += glob(os.path.join(self.search_directory, "*.%s" % ext))

        log.debug(">>> Scan results at selected directory: %s" % files)
        for scan_file in files:
            log.debug(">>> Retrieving scan result %s" % scan_file)
            if os.access(scan_file, os.R_OK) and os.path.isfile(scan_file):

                try:
                    parsed = NmapParser()
                    parsed.parse_file(scan_file)
                except:
                    pass
                else:
                    self.scan_results.append(parsed)
        
    def get_scan_results(self):
        return self.scan_results


if __name__ == "__main__":
    s = SearchDir("/home/adriano/umit/test", ["usr", "xml"])
    for result in s.search(\
                             keyword="",
                             #profile="",
                             #option="",
                             #started="1121737119",
                             #finished="1121737192",
                             #target="10.0.0.100-180",
                             #mac=":",
                             #ipv4="10.0.0.150",
                             #ipv6="",
                             #uptime=209980,
                             # lastboot="", MUST BE REMOVED FROM THE UI!
                             #port=["22", "80"],
                             #port_open="",
                             #port_filtered="",
                             #port_closed="",
                             #service="",
                             #osclass="Microsoft | Windows | 95/98/ME | General Purpose",
                             #osmatch="gentoo",
                             #product="Apache"\
                           ):

        print "Ports:", result.hosts[-1].ports

