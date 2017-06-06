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


def is_a_new_connection(connection, conection_set):
    """
    """
    (i, j) = connection

    for edge in conection_set:

        (a, b) = edge.get_nodes()

        if (a == i and b == j) or (a == j and b == i):
            return False

    return True



class Node(object):
    """
    Node class
    """
    def __init__(self, id=None):
        """
        Constructor method of Node class
        @type  : integer
        @param : Node identifier
        """
        self.__id = id
        """Node identifier"""
        self.__information = {}
        """Hash with general information"""


    def get_id(self):
        """
        Get node ID
        @rtype: number
        @return: Node identifier
        """
        return self.__id


    def set_id(self, id):
        """
        Set node ID
        @type  : number
        @param : Node identifier
        """
        self.__id = id


    def get_info(self, info=None):
        """
        Get general information about node
        @type  : string
        @param : Information name
        @rtype: mixed
        @return: The requested information
        """
        if info == None:
            return self.__information

        if self.__information.has_key(info):
            return self.__information[info]
            
        return None


    def set_info(self, info):
        """
        Set general information
        @type  : dict
        @param : General information dictionary
        """
        for key in info:
            self.__information[key] = info[key]



class Edge:
    """
    """
    def __init__(self, nodes):
        """
        """
        self.__weigths = []
        self.__nodes = nodes
        self.__weigths_mean = None


    def get_nodes(self):
        """
        """
        return self.__nodes


    def get_weigths(self):
        """
        """
        return self.__weigths


    def set_weigths(self, weigths):
        """
        """
        self.__weigths = weigths


    def add_weigth(self, weigth):
        """
        """
        self.__weigths.append(weigth)


    def get_weigths_mean(self):
        """
        """
        return self.__weigths_mean


    def calc_weigths_mean(self):
        """
        """
        if len(self.__weigths) > 0:
            self.__weigths_mean = sum(self.__weigths) / len(self.__weigths)

        else:
            self.__weigths_mean = None



class Graph:
    """
    Network Graph class
    """

    def __init__(self):
        """
        Constructor method of Graph class
        @type  : list
        @param : List of nodes
        """
        self.__main_node = None
        self.__nodes = []
        self.__edges = []
        self.__max_edge_mean_value = None
        self.__min_edge_mean_value = None

        self.calc_max_edge_mean_weight()
        self.calc_max_edge_mean_weight()


    def set_nodes(self, nodes):
        """
        """
        self.__nodes = nodes


    def get_nodes(self):
        """
        """
        return self.__nodes


    def get_number_of_nodes(self):
        """
        Get the number of nodes in graph
        @rtype: number
        @return: The number of nodes in the graph
        """
        return len(self.__nodes)


    def set_main_node(self, node):
        """
        Set the main node by ID
        @type  : number
        @param : The node ID
        """
        self.__main_node = node


    def set_main_node_by_id(self, id):
        """
        Set the main node by ID
        @type  : number
        @param : The node ID
        """
        self.__main_node = self.get_node_by_id(id)


    def get_node_by_id(self, id):
        """
        Get one node of graph by your ID
        @type  : number
        @param : The node ID
        @rtype: Node
        @return: The node
        """
        for node in self.__nodes:

            if node.get_id() == id:
                return node

        return None


    def get_main_node(self):
        """
        Get the main node
        @rtype: Node
        @return: The main node
        """
        return self.__main_node


    def get_main_node_id(self):
        """
        Get the main node ID
        @rtype: number
        @return: The main node ID
        """
        return self.__main_node.get_id()


    def set_connection(self, a, b, weigth=None):
        """
        Set node connections
        @type  : list
        @param : List of connections
        """
        connection = (a, b)

        # if is a new connection make it
        if is_a_new_connection(connection, self.__edges):
            self.__edges.append(Edge(connection))

        # then add new weigth value
        if weigth != None:

            edge = self.get_connection(a, b)
            edge.add_weigth(weigth)

            edge.calc_weigths_mean()

            self.calc_min_edge_mean_weight()
            self.calc_max_edge_mean_weight()


    def get_connection(self, a, b):
        """
        """
        for edge in self.__edges:

            if a in edge.get_nodes() and b in edge.get_nodes():
                return edge


    def get_edges(self):
        """
        """
        return self.__edges


    def get_node_connections(self, node):
        """
        """
        connections = []

        for edge in self.__edges:

            (a, b) = edge.get_nodes()

            if a == node:
                connections.append(b)
            if b == node:
                connections.append(a)

        return connections


    def get_max_edge_mean_weight(self):
        """
        """
        return self.__max_edge_mean_value


    def get_min_edge_mean_weight(self):
        """
        """
        return self.__min_edge_mean_value


    def calc_max_edge_mean_weight(self):
        """
        """
        max_value = None

        for edge in self.__edges:

            mean = edge.get_weigths_mean()

            if mean != None:
                if mean > max_value or max_value == None:
                    max_value = mean

        self.__max_edge_mean_value = max_value


    def calc_min_edge_mean_weight(self):
        """
        """
        min_value = None

        for edge in self.__edges:

            mean = edge.get_weigths_mean()

            if mean != None:
                if mean < min_value or min_value == None:
                    min_value = mean

        self.__min_edge_mean_value = min_value

