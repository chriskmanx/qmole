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

from radialnet.core.Graph import *
from radialnet.gui.RadialNet import NetNode

import re


COLORS = [(0.0, 1.0, 0.0),
          (1.0, 1.0, 0.0),
          (1.0, 0.0, 0.0)]

BASE_RADIUS = 5.5
NONE_RADIUS = 4.5



def calc_vulnerability_level(node, host):
    """
    """
    num_open_ports = host.get_open_ports()

    node.set_info({'number_of_scanned_ports': num_open_ports})

    if num_open_ports < 3:
        node.set_info({'vulnerability_score': 0})

    elif num_open_ports < 7:
        node.set_info({'vulnerability_score': 1})

    else:
        node.set_info({'vulnerability_score': 2})


def set_node_info(node, host):
    """
    """
    node.set_info({"host_reference": host})
    
    # getting vulnerability score
    calc_vulnerability_level(node, host)
    
    radius = BASE_RADIUS + node.get_info("number_of_scanned_ports") / 2
    
    node.set_draw_info({"color":COLORS[node.get_info("vulnerability_score")],
                        "radius":radius})
    
    # getting address and hostnames
    addresses = []
    if len(host.ip) > 0:
        addresses.append(host.ip)
    if len(host.ipv6) > 0:
        addresses.append(host.ipv6)
    if len(host.mac) > 0:
        addresses.append(host.mac)
    
    node.set_info({"addresses": addresses})
    if len(addresses) > 0:
        node.set_info({"ip": addresses[0].get("addr", "")})
    
    if len(host.hostnames) > 0:
        hostnames = []
        for hname in host.hostnames:
            hostname = {}
            hostname["name"] = hname.get("hostname", "")
            hostname["type"] = hname.get("hostname_type", "")
            
            hostnames.append(hostname)

        node.set_info({"hostnames": hostnames})
        node.set_info({"hostname": hostnames[0].get("name", "")})

    # getting uptime
    # if len(host.uptime) > 0 doesn't work here, since these fields are present
    # (but empty) even if there's no uptime information
    if reduce(lambda x,y: x + y, host.uptime.values(), "") != "":
        node.set_info({"uptime": host.uptime})
    else:
        node.set_info({"uptime": None})

    # getting os fingerprint information
    os = None
    
    # osclasses
    if len(host.osclasses) > 0:
        os = {}
        types = ["router", "wap", "switch", "firewall"]
        for type in types:
            if type in host.osclasses[0].get("type", "").lower():
                node.set_info({"device_type": type})
        
        os_classes = []
        for osclass in host.osclasses:
            os_class = {}

            os_class["type"] = osclass.get("type", "")
            os_class["vendor"] = osclass.get("vendor", "")
            #os_class["accuracy"] = int(osclass.get("accuracy", ""))
            os_class["accuracy"] = osclass.get("accuracy", "")
            os_class["os_family"] = osclass.get("osfamily", "")
            os_class["os_gen"] = osclass.get("osgen", "")
            
            os_classes.append(os_class)
        os["classes"] = os_classes
    
    # osmatches
    if len(host.osmatches) > 0 and \
       host.osmatches[0]["accuracy"] != "" and \
       host.osmatches[0]["name"] != "":
        if os == None:
            os = {}
        os["matches"] = host.osmatches
        os["matches"][0]["db_line"] = 0     # not supported
        if os["matches"][0].get("accuracy", "") != "":
            #os["matches"][0]["accuracy"] = int(os["matches"][0]["accuracy"])
            os_class["accuracy"] = osclass.get("accuracy", "")
        else:
            os["matches"][0]["accuracy"] = 0
    
    # ports_used
    if len(host.ports_used) > 0:
        if os == None:
            os = {}
        os_portsused = []
        
        for portused in host.ports_used:
            os_portused = {}
            
            os_portused["state"] = portused.get("state", "")
            os_portused["protocol"] = portused.get("proto", "")
            os_portused["id"] = int(portused.get("portid", "0"))
            
            os_portsused.append(os_portused)
        
        os["used_ports"] = os_portsused
    
    if os != None:
        os["fingerprint"] = ""  # currently unsupported by the NmapParserSAX class
    node.set_info({"os": os})
    
    # getting sequences information
    sequences = {}
    # If all fields are empty, we don't put it into the sequences list
    if reduce(lambda x,y: x + y, host.tcpsequence.values(), "") != "":
        tcp = {}
        if host.tcpsequence.get("index", "") != "":
            tcp["index"] = int(host.tcpsequence["index"])
        else:
            tcp["index"] = 0
        tcp["class"] = ""   # not supported
        tcp["values"] = host.tcpsequence.get("values", "").split(",")
        tcp["difficulty"] = host.tcpsequence.get("difficulty", "")
        sequences["tcp"] = tcp
    if reduce(lambda x,y: x + y, host.ipidsequence.values(), "") != "":
        ip_id = {}
        ip_id["class"] = host.ipidsequence.get("class", "")
        ip_id["values"] = host.ipidsequence.get("values", "").split(",")
        sequences["ip_id"] = ip_id
    if reduce(lambda x,y: x + y, host.tcptssequence.values(), "") != "":
        tcp_ts = {}
        tcp_ts["class"] = host.tcptssequence.get("class", "")
        tcp_ts["values"] = host.tcptssequence.get("values", "").split(",")
        sequences["tcp_ts"] = tcp_ts
    node.set_info({"sequences": sequences})

    # host is host filtered
    if len(host.ports) > 0 and len(host.ports[0].get("extraports", "")) > 0 and \
       host.ports[0]["extraports"][0]["state"] == "filtered":
        node.set_info({"filtered": True})
    else:
        for port in host.ports[0].get("port", []):
            if port["port_state"] == "filtered":
                node.set_info({"filtered": True})
                break

    # getting ports information
    ports = list()
    for host_port in host.ports[0].get("port", []):
        port = dict()
        state = dict()
        service = dict()
        
        port["id"] = int(host_port.get("portid", ""))
        port["protocol"] = host_port.get("protocol", "")
        
        state["state"] = host_port.get("port_state", "")
        state["reason"] = ""        # not supported
        state["reason_ttl"] = ""    # not supported
        state["reason_ip"] = ""     # not supported
        
        service["name"] = host_port.get("service_name", "")
        service["conf"] = host_port.get("service_conf", "")
        service["method"] = host_port.get("service_method", "")
        service["version"] = host_port.get("service_version", "")
        service["product"] = host_port.get("service_product", "")
        service["extrainfo"] = host_port.get("service_extrainfo", "")
        
        port["state"] = state
        port["scripts"] = None      # not supported
        port["service"] = service
        
        ports.append(port)
    
    node.set_info({"ports":ports})

    # extraports
    all_extraports = list()
    for extraport in host.ports[0].get("extraports", []):
        extraports = dict()
        extraports["count"] = int(extraport.get("count", ""))
        extraports["state"] = extraport.get("state", "")
        extraports["reason"] = list()       # not supported
        extraports["all_reason"] = list()   # not supported
        
        all_extraports.append(extraports)
    
    node.set_info({"extraports":all_extraports})

    # getting traceroute information
    if len(host.trace) > 0:
        trace = {}
        hops = []

        for host_hop in host.trace.get("hops", []):
            hop = {}
            hop["ip"] = host_hop.get("ipaddr", "")
            hop["ttl"] = int(host_hop.get("ttl", ""))
            hop["rtt"] = host_hop.get("rtt", "")
            hop["hostname"] = host_hop.get("host", "")
            
            hops.append(hop)
        
        trace["hops"] = hops
        trace["port"] = host.trace.get("port", "")
        trace["protocol"] = host.trace.get("proto", "")

        node.set_info({"trace":trace})


def make_graph_from_nmap_parser(parser):
    """
    """
    hosts = parser.get_root().search_children('host', deep=True)
    graph = Graph()
    nodes = list()
    index = 1

    # setting initial reference host
    nodes.append(NetNode(0))
    node = nodes[-1]

    node.set_info({'ip':'127.0.0.1/8', 'hostname':'localhost'})
    node.set_draw_info({'color':(0,0,0), 'radius':NONE_RADIUS})

    # for each host in hosts just mount the graph
    for host in hosts:

        trace = host.search_children('trace', True, True)

        # if host has traceroute information mount graph
        if trace != None:

            prev_node = nodes[0]

            hops = trace.search_children('hop')
            ttls = [int(hop.get_attr('ttl')) for hop in hops]

            # getting nodes of host by ttl
            for ttl in range(1, max(ttls) + 1):

                if ttl in ttls:

                    hop = trace.query_children('hop', 'ttl', ttl, True)

                    for node in nodes:
                        if hop.get_attr('ipaddr') == node.get_info('ip'):
                            break

                    else:

                        nodes.append(NetNode(index))
                        node = nodes[-1]
                        index += 1

                        node.set_draw_info({'valid':True})
                        node.set_info({'ip':hop.get_attr('ipaddr')})
                        node.set_draw_info({'color':(1,1,1),
                                            'radius':NONE_RADIUS})

                        if hop.get_attr('host') != None:
                            node.set_info({'hostname':hop.get_attr('host')})

                    rtt = hop.get_attr('rtt')

                    if rtt != '--':
                        graph.set_connection(node, prev_node, float(rtt))

                    else:
                        graph.set_connection(node, prev_node)

                else:

                    nodes.append(NetNode(index))
                    node = nodes[-1]
                    index += 1

                    node.set_draw_info({'valid':False})
                    node.set_info({'ip':None, 'hostname':None})
                    node.set_draw_info({'color':(1,1,1), 'radius':NONE_RADIUS})

                    graph.set_connection(node, prev_node)

                prev_node = node

    # for each full scanned host
    for host in hosts:

        ip = host.query_children('address', 'addrtype', 'ipv4', True)

        if ip == None:
            ip = host.query_children('address', 'addrtype', 'ipv6', True)

        for node in nodes:
            if ip.get_attr('addr') == node.get_info('ip'):
                break

        else:

            nodes.append(NetNode(index))
            node = nodes[-1]
            index += 1

            node.set_draw_info({'no_route':True})

            graph.set_connection(node, nodes[0])

        node.set_draw_info({'valid':True})
        node.set_info({'scanned':True})
        set_node_info(node, host)

    graph.set_nodes(nodes)
    graph.set_main_node_by_id(0)

    return graph


def make_graph_from_hosts(hosts):
    #hosts = parser.get_root().search_children('host', deep=True)
    graph = Graph()
    nodes = list()
    index = 1

    # Setting initial reference host
    nodes.append(NetNode(0))
    node = nodes[-1]

    node.set_info({"ip":"127.0.0.1/8", "hostname":"localhost"})
    node.set_draw_info({"color":(0,0,0), "radius":NONE_RADIUS})

    # For each host in hosts just mount the graph
    for host in hosts:
        trace = host.trace
        
        hops = trace.get("hops")
        # If host has traceroute information mount graph
        if hops is not None and len(hops) > 0:
            prev_node = nodes[0]
            hops = trace.get("hops", [])
            ttls = [int(hop["ttl"]) for hop in hops]
            
            # Getting nodes of host by ttl
            for ttl in range(1, max(ttls) + 1):
                if ttl in ttls:
                    # Find a hop by ttl
                    hop = None
                    for h in hops:
                        if ttl == int(h["ttl"]):
                            hop = h
                            break
                    
                    for node in nodes:
                        if hop["ipaddr"] == node.get_info("ip"):
                            break
                    else:
                        nodes.append(NetNode(index))
                        node = nodes[-1]
                        index += 1
                        
                        node.set_draw_info({"valid":True})
                        node.set_info({"ip":hop["ipaddr"]})
                        node.set_draw_info({"color":(1,1,1),
                                            "radius":NONE_RADIUS})
                        
                        if hop["host"] != "":
                            node.set_info({"hostname":hop["host"]})
                    
                    rtt = hop["rtt"]
                    if rtt != "--":
                        graph.set_connection(node, prev_node, float(rtt))
                    else:
                        graph.set_connection(node, prev_node)
                else:
                    nodes.append(NetNode(index))
                    node = nodes[-1]
                    index += 1
                    
                    node.set_draw_info({"valid":False})
                    node.set_info({"ip":None, "hostname":None})
                    node.set_draw_info({"color":(1,1,1), "radius":NONE_RADIUS})
                    
                    graph.set_connection(node, prev_node)
                
                prev_node = node

    # For each fully scanned host
    for host in hosts:
        ip = host.ip
        if len(ip) == 0:
            ip = host.ipv6
        
        for node in nodes:
            if ip["addr"] == node.get_info("ip"):
                break
        else:
            nodes.append(NetNode(index))
            node = nodes[-1]
            index += 1
            
            node.set_draw_info({"no_route":True})
            
            graph.set_connection(node, nodes[0])
        
        node.set_draw_info({"valid":True})
        node.set_info({"scanned":True})
        set_node_info(node, host)
    
    graph.set_nodes(nodes)
    graph.set_main_node_by_id(0)

    return graph
