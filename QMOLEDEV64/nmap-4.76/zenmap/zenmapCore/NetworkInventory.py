#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005 Insecure.Com LLC.
#
# Author: Vladimir Mitrovic <snipe714@gmail.com>
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

class NetworkInventory:
    """This class acts as a container for aggregated scans. It is also
    responsible for opening/saving the aggregation from/to persistent storage."""
    def __init__(self, filename=None):
        # A list of all scans that make up this inventory
        self.scans = []
        
        # A dictionary mapping parsed scans to filenames they were loaded from
        self.filenames = {}
        
        # A dictionary mapping IP addresses into HostInfo objects
        self.hosts = {}
        
        if filename != None:
            self.open_from_file(filename)
    
    def add_scan(self, scan, filename=None):
        """Adds a scan to the list of scans. The object passed as an argument
        should be a parsed nmap result."""
        from time import localtime
        
        for host in scan.get_hosts():
            addr = ""
            if len(host.ipv6) > 0:
                # This is an IPv6 host, so we add the IPv6 address to the map
                addr = host.ipv6["addr"]
            else:
                # IPv4
                addr = host.ip["addr"]
            
            if addr not in self.hosts:
                # Add this host to the hosts dictionary, mapped by IP address
                self.hosts[addr] = host
            else:
                # This host is already present in the host list, so we need to update its
                # info with the info held in the current host object
                old_host = self.hosts[addr]
                # We need to find old_host's scan date
                old_date = localtime(0)
                for old_scan in self.scans:
                    if old_host in old_scan.get_hosts():
                        old_date = old_scan.get_date()
                new_date = scan.get_date()
                self._update_host_info(old_host, host, old_date, new_date)
        
        self.scans.append(scan)
        
        if filename != None:
            basename = os.path.basename(filename)
            
            if basename in self.filenames.values():
                # We need to generate a new filename, since this basename already exists
                base = basename
                ext = "usr"
                try:
                    base, ext = basename.rsplit(".", 1)
                except ValueError:
                    pass
                
                counter = 2
                while basename in self.filenames.values():
                    basename = "%s %s.%s" % (base, counter, ext)
                    counter += 1
            
            self.filenames[scan] = basename
                    
    def remove_scan(self, scan):
        """Removes a scan and any host information it contained from the inventory."""
        # Clear the host dictionary
        self.hosts = {}
        
        # Remove the scan from our scan list
        self.scans.remove(scan)
        
        # Remember the scan list
        scans = self.scans
        
        # Empty it
        self.scans = []
        
        # Delete the filename entry, if any
        if scan in self.filenames:
            del self.filenames[scan]
        
        # For each scan in the remembered list, append it to the scan list and update
        # the host list accordingly
        for scan in scans:
            self.add_scan(scan)
    
    def _update_host_info(self, old_host, new_host, old_date, new_date):
        """This function is called when a host needs to be added to the hosts
        dictionary, but another HostInfo object for that host already exists
        in the dictionary (from a previous scan). In that case, we need to
        update the original HostInfo object so that it holds information from
        both scans."""
        
        # Ports
        for new_port in new_host.ports[0]["port"]:
            # Check if new_port is already present in old_host's ports
            for old_port in old_host.ports[0]["port"]:
                if old_port["portid"] == new_port["portid"]:
                    # We update old_host's port information to reflect the latest known port state
                    if old_date < new_date:
                        index = old_host.ports[0]["port"].index(old_port)
                        old_host.ports[0]["port"][index] = new_port
                    # Finished processing this new_port, we jump to the next one
                    break
            else:
                # This new_port isn't present in old_host, so we simply append it to
                # old_host's port info
                old_host.ports[0]["port"].append(new_port)
        
        # extraports, ipidsequence, state, tcpsequence, tcptssequence, uptime
        if old_date < new_date:
            old_host.extraports = new_host.extraports
            old_host.ipidsequence = new_host.ipidsequence
            old_host.state = new_host.state
            old_host.tcpsequence = new_host.tcpsequence
            old_host.tcptssequence = new_host.tcptssequence
            old_host.uptime = new_host.uptime
        
        # Comment
        if old_host.comment == "":
            old_host.comment = new_host.comment
        elif new_host.comment != "":
            old_host.comment = "%s\n\n%s" % (old_host.comment, new_host.comment)
        
        # Hostnames
        # Replace old_host's hostname with new_host's if old_host has no
        # hostname or new_host's is newer.
        if len(new_host.hostnames) > 0 and \
           (len(old_host.hostnames) == 0 or old_date < new_date):
            old_host.hostnames = new_host.hostnames
        
        # MAC address
        # If there was no MAC address set in old_host, set it to whatever is in new_host.mac.
        # Do the same if both hosts have a MAC address set, but new_host's address is newer.
        if len(old_host.mac) == 0 or (len(old_host.mac) > 0 and len(new_host.mac) > 0 and \
                                      old_date < new_date):
            old_host.mac = new_host.mac
        
        # OS detection fields
        # Replace old_host's OS detection fields with new_host's if old_host has no
        # OS detection info or new_host's info is newer.
        if len(new_host.osclasses) > 0 and (len(old_host.osclasses) == 0 or old_date < new_date):
            old_host.osclasses = new_host.osclasses
            old_host.osmatches = new_host.osmatches
            old_host.ports_used = new_host.ports_used
        
        # Traceroute information
        if len(new_host.trace) > 0 and (len(old_host.trace) == 0 or old_date < new_date):
            old_host.trace = new_host.trace
    
    def get_scans(self):
        return self.scans
    
    def get_hosts(self):
        return self.hosts.values()
    
    def open_from_file(self, path):
        """Loads a scan from the given file."""
        from zenmapCore.NmapParser import NmapParser
        
        parsed = NmapParser(path)
        parsed.parse()
        self.add_scan(parsed, path)
    
    def open_from_dir(self, path):
        """Loads all scans from the given directory into the network inventory."""
        from zenmapCore.NmapParser import NmapParser
        
        for filename in os.listdir(path):
            fullpath = os.path.join(path, filename)
            if os.path.isdir(fullpath):
                continue
            parsed = NmapParser(fullpath)
            parsed.parse()
            self.add_scan(parsed, filename=fullpath)
    
    def save_to_file(self, path, index):
        """Saves the scan with the given list index into a file with a given path."""
        f = open(path, 'w')
        self.get_scans()[index].write_xml(f)
        self.filenames[self.get_scans()[index]] = f
        f.close()
    
    def _generate_filenames(self, path):
        """Generates filenames for all scans that don't already have a filename."""
        # The directory must not contain filenames other than those in the self.filenames dictionary
        for filename in os.listdir(path):
            if os.path.basename(filename) not in self.filenames.values():
                raise Exception("The destination directory contains a file (%s) that's not a part "
                                "of the current inventory. The inventory will not be saved." %
                                os.path.basename(filename))
        
        for scan in self.scans:
            if scan in self.filenames:
                # This scan already has a filename
                continue
            
            date = "%04d%02d%02d%02d%02d" % (scan.date[0], scan.date[1], scan.date[2],
                                             scan.date[3], scan.date[4])
            if scan.profile_name != "":
                filename = "%s on %s" % (scan.profile_name, scan.target)
            else:
                filename = scan.nmap_command
            
            # Prepend the date
            filename = "%s %s" % (date, filename)
            
            # Sanitize the filename
            for char in ["\"", "'", "/", "\\", "?", "*", ":", ";"]:
                if char in filename:
                    filename = filename.replace(char, "_")
            
            # Filename length check
            # (http://en.wikipedia.org/wiki/Filename#Comparison_of_file_name_limitations)
            if len(filename) > 250:
                filename = filename[:250]
            
            # TODO: Filename security checks?
            
            # Try to open the file in append mode. If file.tell() returns a greater-than-zero
            # value, this means that the file already exists and has some data in it, so we
            # choose another filename until we successfully open a zero-length file.
            filename_full = filename + ".usr"
            counter = 2
            while filename_full in self.filenames.values():
                # There's already a scan with this filename, so we generate a new name by appending
                # the counter value before the file extension.
                filename_full = "%s %s.usr" % (filename, str(counter))
                counter += 1
            
            # Add the filename to the list of saved filenames
            self.filenames[scan] = filename_full
    
    def save_to_dir(self, path):
        """Saves all scans in the inventory into a given directory and returns
        a list of (full-path) filenames that were used to save the scans."""
        self._generate_filenames(path)
        
        for scan, filename in self.filenames.iteritems():
            f = open(os.path.join(path, filename), "w")
            scan.write_xml(f)
            f.close()
        
        return self.filenames.values()
    
    def open_from_db(self, id):
        pass
    
    def save_to_db(self):
        # For now, this saves each scan making up the inventory separately in
        # the database.
        from time import time
        from tempfile import mktemp
        from zenmapCore.UmitDB import Scans

        filename = mktemp()

        for parsed in self.get_scans():
            f = open(filename, "w")
            parsed.write_xml(f)
            f.close()

            scan = Scans(scan_name = parsed.scan_name,
                         nmap_xml_output = open(filename).read(),
                         date = time())

if __name__ == "__main__":
    from zenmapCore.NmapParser import NmapParser
    
    scan1 = NmapParser("/home/ndwi/scanz/neobee_1.xml")
    scan1.parse()
    scan2 = NmapParser("/home/ndwi/scanz/scanme_nmap_org.usr")
    scan2.parse()
    
    inventory1 = NetworkInventory()
    inventory1.add_scan(scan1)
    inventory1.add_scan(scan2)
    
    for host in inventory1.get_hosts():
        print "%s" % host.ip["addr"],
        #if len(host.hostnames) > 0:
        #    print "[%s]:" % host.hostnames[0]["hostname"]
        #else:
        #    print ":"
        #for port in host.ports[0]["port"]:
        #    print "  %s: %s" % (port["portid"], port["port_state"])
        #print "  OS matches: %s" % host.osmatches
        #print "  OS classes: %s" % host.osclasses
        #print "  Ports used: %s" % host.ports_used
        #print "  Trace: %s" % host.trace
        #if "hops" in host.trace:
        #    print "         (%d)" % len(host.trace["hops"])
    
    inventory1.remove_scan(scan2)
    print
    for host in inventory1.get_hosts():
        print "%s" % host.ip["addr"],
    
    inventory1.add_scan(scan2)
    print
    for host in inventory1.get_hosts():
        print "%s" % host.ip["addr"],
    
    dir = "/home/ndwi/scanz/top01"
    inventory1.save_to_dir(dir)
    
    inventory2 = NetworkInventory()
    inventory2.open_from_dir(dir)
    
    print
    for host in inventory2.get_hosts():
        print "%s" % host.ip["addr"],
