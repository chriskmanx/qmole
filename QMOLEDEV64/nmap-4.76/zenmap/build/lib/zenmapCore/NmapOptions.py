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

from types import StringTypes
from xml.dom import minidom

class NmapOptions:
    '''Manipulate an xml file with the nmap options, retrieving it's informa-
    tions. This Class doesn't write information inside the xml file yet!
    '''
    def __init__ (self, profile):
        '''__init__ (profile)
        
        Constructor receives xml options file path as argument, and start
        it's parsing.
        '''
        # Starting file's parsing
        self.option_xml = minidom.parse(open (profile))
        
        # Root tag of the xml file.
        self.root_tag = 'nmap_options'
        
        # Here we take every 'groups' and 'options' elements insite root tag
        self.nmap_options = self.__get_nmap_options ()
        
        self.options = self.__turn_into_dict(self.nmap_options)
    
    def get_command_option (self, option, args=[]):
        '''get_command_option (option, args=[])
        
        Get the command option for the given registered 'option'.
        Additional argument 'args' will be merged with the command option
        if it is needed. 'args' can be a list of argument (if command option
        require more than one argument) or a single argument within a string.
        
        Important: Only give an argument if the command option will use it,
        otherwise you'll get an error, trying to merge 'args' with the command
        option
        '''
        if type (args) in StringTypes and args:
            args = [args]
        
        if args:
            args = tuple (args)
            return self.__ga_opt (option, 'option') % args
        else:
            return self.__ga_opt (option, 'option')

    def get_hint (self, option):
        return self.__ga_opt (option, 'hint')
    
    def get_arguments (self, option):
        return self.__get_cutted_list \
               (self.__ga_opt (option, 'arguments'))
    
    def get_need_root (self, option):
        need = self.__ga_opt (option, 'need_root')
        
        if need == "0" or need == "False" or need == "false":
            return False
        else:
            return True

    def get_option (self, option):
        return {'name':option,
                'option':self.get_command_option(option),
                'hint':self.get_hint(option),
                'arguments':self.get_arguments(option),
                'need_root':self.get_need_root(option)}
    
    def get_options_list (self):
        return self.__get_list (self.options)
    
    # Private Methods
    def __ga_opt (self, element_name, attribute_name):
        '''Get option elements attributes'''
        try:
            opt = self.options \
                [element_name.encode('utf8')].getAttribute(attribute_name)
            return opt
        except KeyError:
            raise OptionNotFound (element_name)

    def __get_cutted_list (self, list):
        return [i.strip() for i in list.split(';') if i != '']
    
    def __get_list (self, dict):
        return [element for element in dict.keys()]
    
    def __get_nmap_options (self):
        elements = self.option_xml.getElementsByTagName\
                 (self.root_tag)[0].childNodes
        elements_list = []
        for element in elements:
            try:
                if element.tagName == 'option':
                    elements_list.append(element)
            except: pass
        
        return elements_list
    
    def __turn_into_dict (self, list_of_elements):
        elements_dict = {}
        for element in list_of_elements:
            elements_dict [element.getAttribute ('name')] = element
        
        return elements_dict

# Exceptions
class OptionNotFound (Exception):
    def __init__ (self, option):
        self.option = option
    def __str__ (self):
        return "No option named '"+self.option+"' found!"

if __name__ == '__main__':
    from pprint import pprint
    
    teste = NmapOptions ('options.xml')
    print teste.get_command_option('Min parallel hosts', '5')
    print teste.get_options_list()
