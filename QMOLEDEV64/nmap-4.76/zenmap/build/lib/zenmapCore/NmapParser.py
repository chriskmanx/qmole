#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2005,2008 Insecure.Com LLC.
#
# Author: Adriano Monteiro Marques <py.adriano@gmail.com>
# Modified: Jurand Nogiec <jurand@jurand.net>, 2008
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

import re
import locale
import os
import os.path
import time
import StringIO

from types import StringTypes
from xml.sax import make_parser
from xml.sax.handler import ContentHandler
from xml.sax.saxutils import XMLGenerator
from xml.sax.xmlreader import AttributesImpl as Attributes

from zenmapCore.I18N import _
from zenmapCore.UmitLogging import log
from zenmapCore.NmapCommand import split_quoted

months = ('',_('January'),
             _('February'),
             _('March'),
             _('April'),
             _('May'),
             _('June'),
             _('July'),
             _('August'),
             _('September'),
             _('October'),
             _('November'),
             _('December'),)

class HostInfo(object):
    def __init__(self):
        self.comment = None;
    
    # tcpsequence is a dict of the form
    # {'index': u'203',
    #  'values': u'3637785D,35B440D1,35E9FC3B,3640DB42,355F5931,3601AE14',
    #  'difficulty': u'Good luck!'}
    def set_tcpsequence(self, sequence):
        self._tcpsequence = sequence
    
    def get_tcpsequence(self):
        if self._tcpsequence:
            return self._tcpsequence
        return {}

    # tcptssequence is a dict of the form
    # {'values': u'71D0483C,71D048A3,71D0490C,71D04973,71D049DB,71D04A45',
    #  'class': u'1000HZ'}
    def set_tcptssequence(self, sequence):
        self._tcptssequence = sequence
    
    def get_tcptssequence(self):
        if self._tcptssequence:
            return self._tcptssequence
        return {}

    # ipidsequence is a dict of the form
    # {'values': u'0,0,0,0,0,0', 'class': u'All zeros'}
    def set_ipidsequence(self, sequence):
        self._ipidsequence = sequence
    
    def get_ipidsequence(self):
        if self._ipidsequence:
            return self._ipidsequence
        return {}

    # osclasses is a list containing dicts of the form
    # {'vendor': u'Linux', 'osfamily': u'Linux', 'type': u'general purpose',
    #  'osgen': u'2.6.X', 'accuracy': u'98'}
    def set_osclasses(self, classes):
        self._osclasses = classes
    
    def get_osclasses(self):
        return self._osclasses
    
    # osmatches is a list of dicts of the form
    # {'name': u'Linux 2.6.24', 'accuracy': u'98'}
    def set_osmatches(self, matches):
        self._osmatches = matches
    
    def get_osmatches(self):
        if self._osmatches:
            return self._osmatches
        return []

    def get_best_osmatch(self):
        """Return the OS match with the highest accuracy. If there is a tie, one
        of the best matches will be returned."""
        if not self._osmatches:
            return {}
        def osmatch_key(osmatch):
            # Sort first by accuracy, then by name so it's deterministic.
            try:
                accuracy = float(osmatch.get("accuracy", ""))
            except ValueError:
                accuracy = 0
            return (accuracy, osmatch.get("name"))
        osmatches = self.osmatches[:]
        osmatches.sort(cmp = lambda a, b: cmp(osmatch_key(a), osmatch_key(b)))
        return osmatches[-1]
        
        
    def get_best_osclass(self):
        """Return the OS class with the highest accuracy. If there is a tie, one
        of the best matches will be returned."""
        if not self._osclasses:
            return {}
        def osclass_key(osclass):
            # Sort first by accuracy, then by name so it's deterministic.
            try:
                accuracy = float(osclass.get("accuracy", ""))
            except ValueError:
                accuracy = 0
            return (accuracy, osclass.get("name"))
        osclasses = self.osclasses[:]
        osclasses.sort(cmp = lambda a, b: cmp(osclass_key(a), osclass_key(b)))
        return osclasses[-1]

    # ports_used is a list like
    # [{'state': u'open', 'portid': u'22', 'proto': u'tcp'},
    #  {'state': u'closed', 'portid': u'25', 'proto': u'tcp'},
    #  {'state': u'closed', 'portid': u'44054', 'proto': u'udp'}]
    # but not all three elements are necessarily present.
    def set_ports_used(self, ports):
        self._ports_used = ports
    
    def get_ports_used(self):
        return self._ports_used

    # uptime is a dict of the form
    # {'seconds': u'1909493', 'lastboot': u'Wed Jul 2 06:48:31 2008'}
    def set_uptime(self, uptime):
        self._uptime = uptime
    
    def get_uptime(self):
        if self._uptime:
            return self._uptime
        
        # Avoid empty dict return
        return {"seconds":"", "lastboot":""}

    # ports is a list of one element, having the form
    # [{'extraports': [{'count': u'1709', 'state': u'filtered'}],
    #   'port': [...]}]
    # where each 'port' entry is a dict like
    # {'port_state': u'open', 'portid': u'22', 'protocol': u'tcp',
    #  'service_conf': u'10', 'service_extrainfo': u'protocol 2.0',
    #  'service_method': u'probed', 'service_name': u'ssh',
    #  'service_product': u'OpenSSH', 'service_version': u'4.3'}
    # Having ports be a list with one element is wrong. It should be a simple
    # variable.
    def set_ports(self, port_list):
        self._ports = port_list
    
    def get_ports(self):
        return self._ports

    # extraports is identical to the 'extraports' entry of 'ports'.
    def set_extraports(self, port_list):
        self._extraports = port_list
    
    def get_extraports(self):
        return self._extraports

    # hostnames is a list containing dicts of the form
    # [{'hostname': u'scanme.nmap.org', 'hostname_type': u'PTR'}]
    def set_hostnames(self, hostname_list):
        self._hostnames = hostname_list
    
    def get_hostnames(self):
        return self._hostnames

    # ip, ipv6, and mac are dicts of the form
    # {'vendor': u'', 'type': u'ipv4', 'addr': u'64.13.134.52'}
    def set_ip(self, addr):
        self._ip = addr

    def get_ip(self):
        return self._ip

    def set_mac(self, addr):
        self._mac = addr

    def get_mac(self):
        return self._mac

    def set_ipv6(self, addr):
        self._ipv6 = addr

    def get_ipv6(self):
        return self._ipv6

    # comment is a string.
    def get_comment(self):
        return self._comment
    
    def set_comment(self, comment):
        self._comment = comment

    # state is a string like u'up' or u'down'.
    def set_state(self, status):
        self._state = status
    
    def get_state(self):
        return self._state

    def get_hostname(self):
        hostname = ''
        try:
            hostname = self._hostnames[0]['hostname'] + ' '
        except:
            pass

        # FIXME: Check if i can return the 'addr' key directly from get_ip, get_ipv6 and get_mac
        if self.ip:
            hostname += self._ip['addr']
        elif self.ipv6:
            hostname += self._ipv6['addr']
        elif self.mac:
            hostname += self._mac['addr']
        else:
            hostname = _('Unknown Host')
        
        return hostname

    def get_open_ports(self):
        ports = self.get_ports()
        open = 0
        
        for i in ports:
            port = i['port']
            for p in port:
                if re.findall('open', p['port_state']):
                    open+=1
        
        return open
    
    def get_filtered_ports(self):
        ports = self.get_ports()
        extraports = self.get_extraports()
        filtered = 0
        
        for i in ports:
            port = i['port']
            for p in port:
                if re.findall('filtered', p['port_state']):
                    filtered+=1
 
        for extra in extraports:
            if extra["state"] == "filtered":
                filtered += int(extra["count"])

        return filtered
    
    def get_closed_ports(self):
        ports = self.get_ports()
        extraports = self.get_extraports()
        closed = 0
        
        for i in ports:
            port = i['port']
            for p in port:
                if re.findall('closed', p['port_state']):
                    closed+=1
        
        for extra in extraports:
            if extra["state"] == "closed":
                closed += int(extra["count"])

        return closed
    
    def get_scanned_ports(self):
        ports = self.get_ports()
        extraports = self.get_extraports()
        scanned = 0
        
        for i in ports:
            port = i['port']
            for p in port:
                scanned+=1
        
        for extra in extraports:
            scanned += int(extra["count"])

        return scanned

    def get_services(self):
        services = []
        for port in self.ports:
            for p in port.get("port", []):
                services.append({"service_name":p.get("service_name", _("unknown")),
                                 "portid":p.get("portid", ""),
                                 "service_version":p.get("service_version",
                                                         _("Unknown version")),
                                 "service_product":p.get("service_product", ""),
                                 "service_extrainfo":p.get("service_extrainfo", ""),
                                 "port_state":p.get("port_state", _("Unknown")),
                                 "protocol":p.get("protocol", "")})
        return services
    
    def get_trace(self):
        return self._trace
    
    def set_trace(self, trace):
        self._trace = trace
    
    def append_trace_hop(self, hop):
        if "hops" in self._trace:
            self._trace["hops"].append(hop)
        else:
            self._trace["hops"] = [hop]
    
    def set_trace_error(self, errorstr):
        self._trace["error"] = errorstr

    # Properties
    tcpsequence = property(get_tcpsequence, set_tcpsequence)
    osclasses = property(get_osclasses, set_osclasses)
    osmatches = property(get_osmatches, set_osmatches)
    ports = property(get_ports, set_ports)
    ports_used = property(get_ports_used, set_ports_used)
    extraports = property(get_extraports, set_extraports)
    uptime = property(get_uptime, set_uptime)
    hostnames = property(get_hostnames, set_hostnames)
    tcptssequence = property(get_tcptssequence, set_tcptssequence)
    ipidsequence = property(get_ipidsequence, set_ipidsequence)
    ip = property(get_ip, set_ip)
    ipv6 = property(get_ipv6, set_ipv6)
    mac = property(get_mac, set_mac)
    state = property(get_state, set_state)
    comment = property(get_comment, set_comment)
    services = property(get_services)
    trace = property(get_trace, set_trace)

    _tcpsequence = {}
    _osclasses = []
    _osmatches = []
    _ports = []
    _ports_used = []
    _extraports = []
    _uptime = {}
    _hostnames = []
    _tcptssequence = {}
    _ipidsequence = {}
    _ip = {}
    _ipv6 = {}
    _mac = {}
    _state = ''
    _comment = ''
    _trace = {}


class ParserBasics(object):
    def __init__ (self):
        # This flag informs us whether the XML output file is temporary (True),
        # or user specified (False). If any of them is user-specified, it
        # doesn't get stripped out of the command string in
        # _verify_output_options().
        self.xml_is_temp = True
        
        self.nmap = {'nmaprun':{},\
                     'scaninfo':[],\
                     'verbose':'',\
                     'debugging':'',\
                     'hosts':[],\
                     'runstats':{}}
    
    def set_xml_is_temp(self, xml_is_temp):
        # This flag is False if a user has specified his own -oX option - in
        # which case we not should remove the -oX option from the command
        # string. A value of True means that we're using a temporary file which
        # should be removed from the command string (see
        # _verify_output_options()).
        self.xml_is_temp = xml_is_temp
    
    def get_profile(self):
        return self.nmap['nmaprun'].get('profile', '')

    def set_profile(self, profile):
        self.nmap['nmaprun']['profile'] = profile
    
    def get_profile_name(self):
        return self.nmap['nmaprun'].get('profile_name', '')

    def set_profile_name(self, name):
        self.nmap['nmaprun']['profile_name'] = name
    
    def get_profile_description(self):
        return self.nmap['nmaprun'].get('description', '')

    def set_profile_description(self, description):
        self.nmap['nmaprun']['description'] = description
    
    def get_profile_options(self):
        return self.nmap['nmaprun'].get('options', {})

    def set_profile_options(self, options):
        self.nmap['nmaprun']['options'] = options
    
    def get_target(self):
        return self.nmap['nmaprun'].get('target', '')

    def set_target(self, target):
        self.nmap['nmaprun']['target'] = target

    def get_nmap_output(self):
        return self.nmap['nmaprun'].get('nmap_output', '')

    def set_nmap_output(self, nmap_output):
        self.nmap['nmaprun']['nmap_output'] = nmap_output
    
    def get_debugging_level (self):
        return self.nmap.get('debugging', '')

    def set_debugging_level(self, level):
        self.nmap['debugging'] = level
    
    def get_verbose_level (self):
        return self.nmap.get('verbose', '')

    def set_verbose_level(self, level):
        self.nmap['verbose'] = level
    
    def get_scaninfo(self):
        return self.nmap.get('scaninfo', '')

    def set_scaninfo(self, info):
        self.nmap['scaninfo'] = info
    
    def get_services_scanned (self):
        if self._services_scanned == None:
            return self._services_scanned
        
        services = []
        for scan in self.nmap.get('scaninfo', []):
            services.append(scan['services'])

        self._services_scanned = ','.join(services)
        return self._services_scanned

    def set_services_scanned (self, services_scanned):
        self._services_scanned = services_scanned

    def get_nmap_command (self):
        return self._verify_output_options(self.nmap['nmaprun'].get('args', ''))

    def set_nmap_command(self, command):
        self.nmap['nmaprun']['args'] = self._verify_output_options(command)

    def get_scan_type (self):
        types = []
        for t in self.nmap.get('scaninfo', []):
            types.append(t['type'])
        return types

    def get_protocol (self):
        protocols = []
        for proto in self.nmap.get('scaninfo', []):
            protocols.append(proto['protocol'])
        return protocols

    def get_num_services (self):
        if self._num_services == None:
            return self._num_services
        
        num = 0
        for n in self.nmap.get('scaninfo', []):
            num += int(n['numservices'])

        self._num_services = num
        return self._num_services

    def set_num_services (self, num_services):
        self._num_services = num_services

    def get_date (self):
        epoch = int(self.nmap['nmaprun'].get('start', '0'))
        return time.localtime (epoch)

    def get_start(self):
        return self.nmap['nmaprun'].get('start', '0')

    def set_start(self, start):
        self.nmap['nmaprun']['start'] = start

    def set_date(self, date):
        if type(date) == type(int):
            self.nmap['nmaprun']['start'] = date
        else:
            raise Exception("Wrong date format. Date should be saved \
in epoch format!")
    
    def get_open_ports(self):
        ports = 0

        for h in self.nmap.get('hosts', []):
            ports += h.get_open_ports()

        return ports

    def get_filtered_ports(self):
        ports = 0

        for h in self.nmap.get('hosts', []):
            ports += h.get_filtered_ports()
        
        return ports

    def get_closed_ports(self):
        ports = 0
        
        for h in self.nmap['hosts']:
            ports += h.get_closed_ports()

        return ports

    def get_formated_date(self):
        date = self.get_date()
        return "%s %s, %s - %s:%s" % (months[date[1]], 
                                      str(date[2]), 
                                      str(date[0]),
                                      str(date[3]).zfill(2), 
                                      str(date[4]).zfill(2))

    def get_scanner (self):
        return self.nmap['nmaprun'].get('scanner', '')

    def set_scanner(self, scanner):
        self.nmap['nmaprun']['scanner'] = scanner
    
    def get_scanner_version (self):
        return self.nmap['nmaprun'].get('version', '')

    def set_scanner_version(self, version):
        self.nmap['nmaprun']['version'] = version

    # IPv4
    def get_ipv4(self):
        addresses = []
        for host in self.nmap.get('hosts', []):
            try:
                addresses.append(host.get_ip().get('addr', ''))
            except:
                pass
        
        return addresses

    # MAC
    def get_mac(self):
        addresses = []
        for host in self.nmap.get('hosts', []):
            try:
                addresses.append(host.get_mac().get('addr', ''))
            except:
                pass
        
        return addresses

    # IPv6
    def get_ipv6(self):
        addresses = []
        for host in self.nmap.get('hosts', []):
            try:
                addresses.append(host.get_ipv6().get('addr', ''))
            except:
                pass

        return addresses

    def get_hostnames (self):
        hostnames = []
        for host in self.nmap.get('hosts', []):
            hostnames += host.get_hostnames()
        return hostnames

    def get_ports(self):
        ports = []
        for port in self.nmap.get('hosts', []):
            ports.append(port.get_ports())
        
        return ports

    def get_hosts(self):
        return self.nmap.get('hosts', None)

    def get_runstats(self):
        return self.nmap.get('runstats', None)

    def set_runstats(self, stats):
        self.nmap['runstats'] = stats
    
    def get_hosts_down(self):
        return int(self.nmap['runstats'].get('hosts_down', '0'))

    def set_hosts_down(self, down):
        self.nmap['runstats']['hosts_down'] = int(down)
    
    def get_hosts_up(self):
        return int(self.nmap['runstats'].get('hosts_up', '0'))

    def set_hosts_up(self, up):
        self.nmap['runstats']['hosts_up'] = int(up)
    
    def get_hosts_scanned(self):
        return int(self.nmap['runstats'].get('hosts_scanned', '0'))

    def set_hosts_scanned(self, scanned):
        self.nmap['runstats']['hosts_scanned'] = int(scanned)
    
    def get_finish_time (self):
        return time.localtime(int(self.nmap['runstats'].get('finished_time',
                                                            '0')))

    def set_finish_time(self, finish):
        self.nmap['runstats']['finished_time'] = int(finish)

    def get_finish_epoc_time(self):
        return int(self.nmap['runstats'].get('finished_time', '0'))

    def set_finish_epoc_time(self, time):
        self.nmap['runstats']['finished_time'] = time

    def get_scan_name(self):
        """Get a human-readable string representing this scan."""
        scan_name = self.nmap.get("scan_name")
        if scan_name:
            return scan_name
        if self.profile_name and self.target:
            return _("%s on %s") % (self.profile_name, self.target)
        return self.get_nmap_command()

    def set_scan_name(self, scan_name):
        self.nmap["scan_name"] = scan_name
    
    def get_formated_finish_date(self):
        date = self.get_finish_time()
        return "%s %s, %s - %s:%s" % (months[date[1]], 
                                      str(date[2]), 
                                      str(date[0]),
                                      str(date[3]).zfill(2), 
                                      str(date[4]).zfill(2))

    def _verify_output_options(self, command):
        """Remove and -oX options from the command stored in the XML file,
        unless they were put there by the user."""
        command_list = split_quoted(command)

        i = 0
        while i < len(command_list):
            if command_list[i] == "-oX" and self.xml_is_temp:
                del command_list[i:i + 2]
            else:
                i += 1

        return " ".join (command_list)

    profile = property(get_profile, set_profile)
    profile_name = property(get_profile_name, set_profile_name)
    profile_description = property(get_profile_description, 
                                   set_profile_description)
    profile_options = property(get_profile_options, set_profile_options)
    target = property(get_target, set_target)
    nmap_output = property(get_nmap_output, set_nmap_output)
    debugging_level = property(get_debugging_level, set_debugging_level)
    verbose_level = property(get_verbose_level, set_verbose_level)
    scaninfo = property(get_scaninfo, set_scaninfo)
    services_scanned = property(get_services_scanned, set_services_scanned)
    nmap_command = property(get_nmap_command, set_nmap_command)
    scan_type = property(get_scan_type)
    protocol = property(get_protocol)
    num_services = property(get_num_services, set_num_services)
    date = property(get_date, set_date)
    open_ports = property(get_open_ports)
    filtered_ports = property(get_filtered_ports)
    closed_ports = property(get_closed_ports)
    formated_date = property(get_formated_date)
    scanner = property(get_scanner, set_scanner)
    scanner_version = property(get_scanner_version, set_scanner_version)
    ipv4 = property(get_ipv4)
    mac = property(get_mac)
    ipv6 = property(get_ipv6)
    hostnames = property(get_hostnames)
    ports = property(get_ports)
    hosts = property(get_hosts)
    runstats = property(get_runstats, set_runstats)
    hosts_down = property(get_hosts_down, set_hosts_down)
    hosts_up = property(get_hosts_up, set_hosts_up)
    hosts_scanned = property(get_hosts_scanned, set_hosts_scanned)
    finish_time = property(get_finish_time, set_finish_time)
    finish_epoc_time = property(get_finish_epoc_time, set_finish_epoc_time)
    formated_finish_date = property(get_formated_finish_date)
    start = property(get_start, set_start)
    scan_name = property(get_scan_name, set_scan_name)

    _num_services = None
    _services_scanned = None


class NmapParserSAX(ParserBasics, ContentHandler):
    def __init__(self):
        ParserBasics.__init__(self)

        self.in_run_stats = False
        self.in_host = False
        self.in_ports = False
        self.in_port = False
        self.in_os = False
        self.in_trace = False
        self.list_extraports = []

        self.filename = None

        self.unsaved = False

    def set_parser(self, parser):
        self.parser = parser

    def parse(self, f):
        """Parse an Nmap XML file from the file-like object f."""
        self.parser.parse(f)

    def parse_file(self, filename):
        """Parse an Nmap XML file from the named file."""
        f = open(filename, "r")
        try:
            self.parse(f)
            self.filename = filename
        finally:
            f.close()

    def _parse_nmaprun(self, attrs):
        run_tag = "nmaprun"
        
        self.nmap[run_tag]["nmap_output"] = attrs.get("nmap_output", "")
        self.nmap[run_tag]["profile"] = attrs.get("profile", "")
        self.nmap[run_tag]["profile_name"] = attrs.get("profile_name", "")
        self.nmap[run_tag]["description"] = attrs.get("description", "")
        # The options are supposed to be a dict mapping option descriptors (like
        # "Ports to scan") to arguments (like "80"). So we turn it into a dict.
        # However, this is completely broken, because the option arguments are
        # not stored in the file. Just fill in None so the file will at least
        # load.
        self.nmap[run_tag]["options"] = dict([(o, None) for o in attrs.get("options", "").split(",")])
        self.nmap[run_tag]["target"] = attrs.get("target", "")
        self.nmap[run_tag]["start"] = attrs.get("start", "")
        self.nmap[run_tag]["args"] = attrs.get("args", "")
        self.nmap[run_tag]["scanner"] = attrs.get("scanner", "")
        self.nmap[run_tag]["version"] = attrs.get("version", "")
        self.nmap[run_tag]["xmloutputversion"] = attrs.get("xmloutputversion", "")

    def _parse_scaninfo(self, attrs):
        dic = {}
        
        dic["type"] = attrs.get("type", "")
        dic["protocol"] = attrs.get("protocol", "")
        dic["numservices"] = attrs.get("numservices", "")
        dic["services"] = attrs.get("services", "")
        
        self.nmap["scaninfo"].append(dic)

    def _parse_verbose(self, attrs):
        self.nmap["verbose"] = attrs.get("level", "")

    def _parse_debugging(self, attrs):
        self.nmap["debugging"] = attrs.get("level", "")

    def _parse_runstats_finished(self, attrs):
        self.nmap["runstats"]["finished_time"] = attrs.get("time", "")

    def _parse_runstats_hosts(self, attrs):
        self.nmap["runstats"]["hosts_up"] = attrs.get("up", "")
        self.nmap["runstats"]["hosts_down"] = attrs.get("down", "")
        self.nmap["runstats"]["hosts_scanned"] = attrs.get("total", "")

    def _parse_host(self, attrs):
        self.host_info = HostInfo()
        self.host_info.comment = attrs.get("comment", "")

    def _parse_host_status(self, attrs):
        self.host_info.set_state(attrs.get("state", ""))

    def _parse_host_address(self, attrs):
        address_attributes = {"type":attrs.get("addrtype", ""),
                              "vendor":attrs.get("vendor", ""),
                              "addr":attrs.get("addr", "")}

        if address_attributes["type"] == "ipv4":
            self.host_info.set_ip(address_attributes)
        elif address_attributes["type"] == "ipv6":
            self.host_info.set_ipv6(address_attributes)
        elif address_attributes["type"] == "mac":
            self.host_info.set_mac(address_attributes)

    def _parse_host_hostname(self, attrs):
        self.list_hostnames.append({"hostname":attrs.get("name", ""),
                                    "hostname_type":attrs.get("type", "")})

    def _parse_host_extraports(self, attrs):
        self.list_extraports.append({"state":attrs.get("state", ""),
                                     "count":attrs.get("count", "")})

    def _parse_host_port(self, attrs):
        self.dic_port = {"protocol":attrs.get("protocol", ""), 
                         "portid":attrs.get("portid", "")}

    def _parse_host_port_state(self, attrs):
        self.dic_port["port_state"] = attrs.get("state", "")

    def _parse_host_port_service(self, attrs):
        self.dic_port["service_name"] = attrs.get("name", "")
        self.dic_port["service_method"] = attrs.get("method", "")
        self.dic_port["service_conf"] = attrs.get("conf", "")
        self.dic_port["service_product"] = attrs.get("product", "")
        self.dic_port["service_version"] = attrs.get("version", "")
        self.dic_port["service_extrainfo"] = attrs.get("extrainfo", "")

    def _parse_host_osmatch(self, attrs):
        self.list_osmatch.append(self._parsing(attrs, ['name', 'accuracy']))

    def _parse_host_portused(self, attrs):
        self.list_portused.append(self._parsing(attrs, 
                                                ['state','proto','portid']))

    def _parse_host_osclass(self, attrs):
        self.list_osclass.append(self._parsing(attrs, ['type',
                                                       'vendor',
                                                       'osfamily',
                                                       'osgen',
                                                       'accuracy']))

    def _parsing(self, attrs, attrs_list):
        # Returns a dict with the attributes of a given tag with the
        # atributes names as keys and their respective values
        dic = {}
        for at in attrs_list:
            dic[at] = attrs.get(at, "")
        return dic

    def _parse_host_uptime(self, attrs):
        self.host_info.set_uptime(self._parsing(attrs, ["seconds", "lastboot"]))


    def _parse_host_tcpsequence(self, attrs):
        self.host_info.set_tcpsequence(self._parsing(attrs, ['index',
                                                             'difficulty',
                                                             'values']))
    
    def _parse_host_tcptssequence(self, attrs):
        self.host_info.set_tcptssequence(self._parsing(attrs, ['class',
                                                               'values']))

    def _parse_host_ipidsequence(self, attrs):
        self.host_info.set_ipidsequence(self._parsing(attrs, ['class',
                                                              'values']))
    
    def _parse_host_trace(self, attrs):
        trace = {}
        for attr in ["proto", "port"]:
            trace[attr] = attrs.get(attr, "")
        self.host_info.set_trace(trace)
    
    def _parse_host_trace_hop(self, attrs):
        hop = self._parsing(attrs, ["ttl", "rtt", "ipaddr", "host"])
        self.host_info.append_trace_hop(hop)
    
    def _parse_host_trace_error(self, attrs):
        self.host_info.set_trace_error(attrs.get("errorstr", ""))
    
    def startElement(self, name, attrs):
        if name == "nmaprun":
            self._parse_nmaprun(attrs)
        elif name == "scaninfo":
            self._parse_scaninfo(attrs)
        elif name == "verbose":
            self._parse_verbose(attrs)
        elif name == "debugging":
            self._parse_debugging(attrs)
        elif name == "runstats":
            self.in_run_stats = True
        elif self.in_run_stats and name == "finished":
            self._parse_runstats_finished(attrs)
        elif self.in_run_stats and name == "hosts":
            self._parse_runstats_hosts(attrs)
        elif name == "host":
            self.in_host = True
            self._parse_host(attrs)
            self.list_ports = []
            self.list_port = []
            self.list_extraports = []
        elif self.in_host and name == "status":
            self._parse_host_status(attrs)
        elif self.in_host and name == "address":
            self._parse_host_address(attrs)
        elif self.in_host and name == "hostnames":
            self.in_hostnames = True
            self.list_hostnames = []
        elif self.in_host and self.in_hostnames and name == "hostname":
            self._parse_host_hostname(attrs)
        elif self.in_host and name == "ports":
            self.in_ports = True
        elif self.in_host and self.in_ports and name == "extraports":
            self._parse_host_extraports(attrs)
        elif self.in_host and self.in_ports and name == "port":
            self.in_port = True
            self._parse_host_port(attrs)
        elif self.in_host and self.in_ports and \
             self.in_port and name == "state":
            self._parse_host_port_state(attrs)
        elif self.in_host and self.in_ports and \
             self.in_port and name == "service":
            self._parse_host_port_service(attrs)
        elif self.in_host and name == "os":
            self.in_os = True
            self.list_portused = []
            self.list_osmatch = []
            self.list_osclass = []
        elif self.in_host and self.in_os and name == "osmatch":
            self._parse_host_osmatch(attrs)
        elif self.in_host and self.in_os and name == "portused":
            self._parse_host_portused(attrs)
        elif self.in_host and self.in_os and name == "osclass":
            self._parse_host_osclass(attrs)
        elif self.in_host and name == "uptime":
            self._parse_host_uptime(attrs)
        elif self.in_host and name == "tcpsequence":
            self._parse_host_tcpsequence(attrs)
        elif self.in_host and name == "tcptssequence":
            self._parse_host_tcptssequence(attrs)
        elif self.in_host and name == "ipidsequence":
            self._parse_host_ipidsequence(attrs)
        elif self.in_host and name == "trace":
            self.in_trace = True
            self._parse_host_trace(attrs)
        elif self.in_host and self.in_trace and name == "hop":
            self._parse_host_trace_hop(attrs)
        elif self.in_host and self.in_trace and name == "error":
            self._parse_host_trace_error(attrs)


    def endElement(self, name):
        if name == "runstats":
            self.in_run_stats = False
        elif name == "host":
            self.in_host = False
            self.list_ports.append({"extraports":self.list_extraports,
                                    "port":self.list_port})
            self.host_info.set_extraports(self.list_extraports)
            self.host_info.set_ports(self.list_ports)
            self.nmap["hosts"].append(self.host_info)
            del(self.list_ports)
        elif self.in_host and name == "hostnames":
            self.in_hostnames = False
            self.host_info.set_hostnames(self.list_hostnames)
        elif self.in_host and name == "ports":
            self.in_ports = False
        elif self.in_host and self.in_ports and name == "port":
            self.in_port = False
            self.list_port.append(self.dic_port)
            del(self.dic_port)
        elif self.in_host and self.in_os and name == "os":
            self.in_os = False
            self.host_info.set_ports_used(self.list_portused)
            self.host_info.set_osmatches(self.list_osmatch)
            self.host_info.set_osclasses(self.list_osclass)

            del(self.list_portused)
            del(self.list_osmatch)
            del(self.list_osclass)
        elif self.in_host and self.in_trace and name == "trace":
            self.in_trace = False

    def write_xml(self, f):
        """Write the XML representation of this object to the file-like object
        f."""
        writer = XMLGenerator(f)
        writer.startDocument()
        self._write_nmaprun(writer)
        self._write_scaninfo(writer)
        self._write_verbose(writer)
        self._write_debugging(writer)
        self._write_hosts(writer)
        self._write_runstats(writer)
        writer.endElement("nmaprun")
        writer.endDocument()

    def get_xml(self):
        """Return a string containing the XML representation of this scan."""
        buffer = StringIO.StringIO()
        self.write_xml(buffer)
        string = buffer.getvalue()
        buffer.close()
        return string

    def write_xml_to_file(self, filename):
        """Write the XML representation of this scan to the file whose name is
        given."""
        fd = open(filename, "wb")
        self.write_xml(fd)
        fd.close()

    def _write_runstats(self, writer):
        ##################
        # Runstats element
        writer.startElement("runstats", Attributes(dict()))

        ## Finished element
        writer.startElement("finished",
                        Attributes(dict(time = str(self.finish_epoc_time))))
        writer.endElement("finished")

        ## Hosts element
        writer.startElement("hosts",
                            Attributes(dict(up = str(self.hosts_up),
                                            down = str(self.hosts_down),
                                            total = str(self.hosts_scanned))))
        writer.endElement("hosts")


        writer.endElement("runstats")
        # End of Runstats element
        #########################

    def _write_hosts(self, writer):
        for host in self.hosts:
            # Start host element
            writer.startElement("host",
                                Attributes(dict(comment=host.comment)))

            # Status element
            writer.startElement("status",
                                Attributes(dict(state=host.state)))
            writer.endElement("status")


            ##################
            # Address elements
            ## IPv4
            if type(host.ip) == type({}):
                writer.startElement("address",
                            Attributes(dict(addr=host.ip.get("addr", ""),
                                        vendor=host.ip.get("vendor", ""),
                                        addrtype=host.ip.get("type", ""))))
                writer.endElement("address")

            ## IPv6
            if type(host.ipv6) == type({}):
                writer.startElement("address",
                            Attributes(dict(addr=host.ipv6.get("addr", ""),
                                        vendor=host.ipv6.get("vendor", ""),
                                        addrtype=host.ipv6.get("type", ""))))
                writer.endElement("address")

            ## MAC
            if type(host.mac) == type({}):
                writer.startElement("address",
                            Attributes(dict(addr=host.mac.get("addr", ""),
                                        vendor=host.mac.get("vendor", ""),
                                        addrtype=host.mac.get("type", ""))))
                writer.endElement("address")
            # End of Address elements
            #########################


            ###################
            # Hostnames element
            writer.startElement("hostnames", Attributes({}))

            for hname in host.hostnames:
                if type(hname) == type({}):
                    writer.startElement("hostname",
                            Attributes(dict(name = hname.get("hostname", ""),
                                        type = hname.get("hostname_type", ""))))
                    
                    writer.endElement("hostname")

            writer.endElement("hostnames")
            # End of Hostnames element
            ##########################


            ###############
            # Ports element
            writer.startElement("ports", Attributes({}))

            for ps in host.ports:
                ## Extraports elements
                for ext in ps["extraports"]:
                    if type(ext) == type({}):
                        writer.startElement("extraports",
                            Attributes(dict(count = ext.get("count", ""),
                                            state = ext.get("state", ""))))
                        writer.endElement("extraports")

                ## Port elements
                for p in ps["port"]:
                    if type(p) == type({}):
                        writer.startElement("port",
                            Attributes(dict(portid = p.get("portid", ""),
                                            protocol = p.get("protocol", ""))))

                        ### Port state
                        writer.startElement("state",
                            Attributes(dict(state=p.get("port_state", ""))))
                        writer.endElement("state")

                        ### Port service info
                        writer.startElement("service",
                            Attributes(dict(conf = p.get("service_conf", ""),
                                    method = p.get("service_method", ""),
                                    name = p.get("service_name", ""),
                                    product = p.get("service_product", ""),
                                    version = p.get("service_version", ""),
                                    extrainfo = p.get("service_extrainfo", "")\
                                )))
                        writer.endElement("service")

                        writer.endElement("port")

            writer.endElement("ports")
            # End of Ports element
            ######################


            ############
            # OS element
            writer.startElement("os", Attributes({}))
            
            ## Ports used elements
            for pu in host.ports_used:
                if type(pu) == type({}):
                    writer.startElement("portused",
                                Attributes(dict(state = pu.get("state", ""),
                                                proto = pu.get("proto", ""),
                                                portid = pu.get("portid", ""))))
                    writer.endElement("portused")

            ## Osclass elements
            for oc in host.osclasses:
                if type(oc) == type({}):
                    writer.startElement("osclass",
                        Attributes(dict(vendor = oc.get("vendor", ""),
                                        osfamily = oc.get("osfamily", ""),
                                        type = oc.get("type", ""),
                                        osgen = oc.get("osgen", ""),
                                        accuracy = oc.get("accuracy", ""))))
                    writer.endElement("osclass")

            ## Osmatch elements
            for om in host.osmatches:
                if type(om) == type({}):
                    writer.startElement("osmatch",
                        Attributes(dict(name = om.get("name", ""),
                                        accuracy = om.get("accuracy", ""))))
                writer.endElement("osmatch")

            writer.endElement("os")
            # End of OS element
            ###################

            # Uptime element
            if type(host.uptime) == type({}):
                writer.startElement("uptime",
                    Attributes(dict(seconds = host.uptime.get("seconds", ""),
                                lastboot = host.uptime.get("lastboot", ""))))
                writer.endElement("uptime")

            #####################
            # Sequences elementes
            ## TCP Sequence element
            # Cannot use dict() here, because of the 'class' attribute.
            if type(host.tcpsequence) == type({}):
                writer.startElement("tcpsequence",
                    Attributes({"index":host.tcpsequence.get("index", ""),
                            "difficulty":host.tcpsequence.get("difficulty", ""),
                            "values":host.tcpsequence.get("values", "")}))
                writer.endElement("tcpsequence")

            ## IP ID Sequence element
            if type(host.ipidsequence) == type({}):
                writer.startElement("ipidsequence",
                    Attributes({"class":host.ipidsequence.get("class", ""),
                                "values":host.ipidsequence.get("values", "")}))
                writer.endElement("ipidsequence")

            ## TCP TS Sequence element
            if type(host.tcptssequence) == type({}):
                writer.startElement("tcptssequence",
                    Attributes({"class":host.tcptssequence.get("class", ""),
                            "values":host.tcptssequence.get("values", "")}))
                writer.endElement("tcptssequence")
            # End of sequences elements
            ###########################
            
            ## Trace element
            if len(host.trace) > 0:
                writer.startElement("trace",
                    Attributes({"proto":host.trace.get("proto", ""),
                                "port":host.trace.get("port", "")}))
                
                if "hops" in host.trace:
                    for hop in host.trace["hops"]:
                        writer.startElement("hop",
                            Attributes({"ttl":hop["ttl"],
                                        "rtt":hop["rtt"],
                                        "ipaddr":hop["ipaddr"],
                                        "host":hop["host"]}))
                        writer.endElement("hop")
                
                if "error" in host.trace:
                    writer.startElement("error",
                        Attributes({"errorstr":host.trace["error"]}))
                    writer.endElement("error")
                
                writer.endElement("trace")
            # End of trace element
            ###########################

            # End host element
            writer.endElement("host")

    def _write_debugging(self, writer):
        writer.startElement("debugging", Attributes(dict(
                                            level=str(self.debugging_level))))
        writer.endElement("debugging")

    def _write_verbose(self, writer):
        writer.startElement("verbose", Attributes(dict(
                                            level=str(self.verbose_level))))
        writer.endElement("verbose")

    def _write_scaninfo(self, writer):
        for scan in self.scaninfo:
            if type(scan) == type({}):
                writer.startElement("scaninfo",
                    Attributes(dict(type = scan.get("type", ""),
                                    protocol = scan.get("protocol", ""),
                                    numservices = scan.get("numservices", ""),
                                    services = scan.get("services", ""))))
                writer.endElement("scaninfo")

    def _write_nmaprun(self, writer):
        writer.startElement("nmaprun",
                Attributes(dict(args = str(self.nmap_command),
                                description = str(self.profile_description),
                                nmap_output = self.nmap_output,
                                # Note that option arguments are discarded. This
                                # is incorrect behavior.
                                options = str(",".join(self.profile_options.keys())),
                                profile = str(self.profile),
                                profile_name = str(self.profile_name),
                                scanner = str(self.scanner),
                                start = str(self.start),
                                startstr = str(self.formated_date),
                                target = str(self.target),
                                version = str(self.scanner_version),
                                scan_name = str(self.scan_name))))

    def set_unsaved(self):
        self.unsaved = True

    def is_unsaved(self):
        return self.unsaved

def nmap_parser_sax():
    parser = make_parser()
    nmap_parser = NmapParserSAX()
    
    parser.setContentHandler(nmap_parser)
    nmap_parser.set_parser(parser)

    return nmap_parser

NmapParser = nmap_parser_sax

if __name__ == '__main__':
    import sys

    file_to_parse = sys.argv[1]
    
    np = NmapParser()
    np.parse_file(file_to_parse)
    
    for host in np.hosts:
        print "%s:" % host.ip["addr"]
        print "  Comment:", repr(host.comment)
        print "  TCP sequence:", repr(host.tcpsequence)
        print "  TCP TS sequence:", repr(host.tcptssequence)
        print "  IP ID sequence:", repr(host.ipidsequence)
        print "  Uptime:", repr(host.uptime)
        print "  OS Match:", repr(host.osmatch)
        print "  Ports:"
        for i in host.ports:
            for p in i['port']:
                print "\t%s" % repr(p)
        print "  Ports used:", repr(host.ports_used)
        print "  OS Class:", repr(host.osclasses)
        print "  Hostnames:", repr(host.hostnames)
        print "  IP:", repr(host.ip)
        print "  IPv6:", repr(host.ipv6)
        print "  MAC:", repr(host.mac)
        print "  State:", repr(host.state)
        print "  Trace:"
        for hop in host.trace["hops"]:
            print "    ", repr(hop)
        print

    host = np.nmap["hosts"][-1]

    print "Comment:", repr(host.comment)
    print "TCP sequence:", repr(host.tcpsequence)
    print "TCP TS sequence:", repr(host.tcptssequence)
    print "IP ID sequence:", repr(host.ipidsequence)
    print "Uptime:", repr(host.uptime)
    print "OS Match:", repr(host.osmatches)
    print "Ports:"
    for i in host.ports:
        for p in i['port']:
            print "\t%s" % repr(p)
    print "Ports used:", repr(host.ports_used)
    print "OS Class:", repr(host.osclasses)
    print "Hostnames:", repr(host.hostnames)
    print "IP:", repr(host.ip)
    print "IPv6:", repr(host.ipv6)
    print "MAC:", repr(host.mac)
    print "State:", repr(host.state)
